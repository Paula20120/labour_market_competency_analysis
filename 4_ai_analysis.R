# ==============================================================================
# STAGE 3 v4: AI-Driven Competence Mapping vs Semantic KEU Buckets
# Engine: llama.cpp HTTP Server (http://127.0.0.1:8080)
# ==============================================================================

library(tidyverse)
library(stringr)
library(httr2)
library(jsonlite)
library(progressr)
library(digest)
library(furrr)

# ------------------------------------------------------------------------------
# 0. CONFIG
# ------------------------------------------------------------------------------
LLAMA_URL              <- "https://generativelanguage.googleapis.com/v1beta/models"
LLAMA_MODEL            <- "gemini-2.5-flash"          # tani, szybki model AI Studio; mozesz zmienic np. na gemini-2.5-flash-lite
GEMINI_API_KEY         <- Sys.getenv("GEMINI_API_KEY") # ustaw zmienna srodowiskowa, NIE wpisuj klucza w kodzie
TIMEOUT_SEC            <- 90
MAX_RETRIES            <- 5
CACHE_FILE             <- "stage3_cache.rds"
TEXT_CACHE_FILE        <- "text_cache_v4.rds"
SEMANTIC_BUCKETS_FILE  <- "keu_semantic_buckets.csv"

TOP_N_KEU_CANDIDATES        <- 3 #8
BATCH_TEXTS_PER_CALL        <- 2 #4 #3 -- zmniejszone z 4 na 2, bo przy 4 ofertach na raz
# odpowiedz JSON bywala tak dluga, ze model
# ucinal ja w polowie (finishReason = MAX_TOKENS)
MAX_FULLREQ_CHARS_IN_PROMPT <- 2000
SAVE_EVERY                  <- 50

# Przerwa miedzy kazdym wywolaniem AI (sekundy) -- chroni przed rate-limitem/blokada
INTER_CALL_SLEEP_SEC <- 15

# AI Studio (darmowy/niski tier) ma niskie limity zapytan na minute -- praca
# rownolegla (futures) byla najpewniej przyczyna wczesniejszej blokady.
# Domyslnie wylaczona (sequential).
USE_PARALLEL <- FALSE
N_WORKERS    <- 1 #6

if (GEMINI_API_KEY == "") {
  stop("Brak GEMINI_API_KEY. Ustaw zmienna srodowiskowa np.: Sys.setenv(GEMINI_API_KEY = \"twoj_klucz\") lub w .Renviron. Klucz wygenerujesz w Google AI Studio: https://aistudio.google.com/app/apikey")
}

TEST_MODE <- TRUE
TEST_N    <- 3

FINAL_BUCKET_VALUES <- c(
  "AI_ARTIFICIAL_INTELLIGENCE",
  "BIG_DATA_DATABASES",
  "DATA_ANALYSIS_STATS",
  "IT_SERVICES_CLOUD_UX",
  "PROGRAMMING_WEB",
  "MANAGEMENT_INFORMATION_SYSTEMS",
  "PROCESS_OPTIMIZATION",
  "PRODUCTION_LOGISTICS",
  "QUALITY_MANAGEMENT",
  "ERGONOMICS_WORKPLACE",
  "PROJECT_MANAGEMENT",
  "INNOVATION_ENTREPRENEURSHIP",
  "FINANCE_ACCOUNTING",
  "LEGAL_ECONOMY_ESG",
  "E_COMMERCE_MARKETING",
  "LEADERSHIP_DECISION_MAKING",
  "STAKEHOLDER_CLIENT_RELATIONS",
  "HR_RECRUITMENT",
  "SOFT_INTERPERSONAL",
  "OTHER_UNCLASSIFIED"
)

# ------------------------------------------------------------------------------
# 1. Load and validate input files
# ------------------------------------------------------------------------------
required_files <- c("cleaned_data.csv", "pwr_keu_peu_mapping.csv", SEMANTIC_BUCKETS_FILE)
missing_files  <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing input files: ", paste(missing_files, collapse = ", "))
}

market_data      <- read_csv("cleaned_data.csv",        show_col_types = FALSE)
pwr_mapping_raw  <- read_csv("pwr_keu_peu_mapping.csv", show_col_types = FALSE)
semantic_buckets <- read_csv(SEMANTIC_BUCKETS_FILE,     show_col_types = FALSE)

# ------------------------------------------------------------------------------
# 2. Build KEU framework with key phrases
# ------------------------------------------------------------------------------
smart_extract_key_phrases <- function(s) {
  if (is.na(s) || s == "") return("")
  phrases <- str_split(s, ";")[[1]] %>% str_trim()
  phrases <- phrases[phrases != ""]
  academic_noise   <- c(
    "podstawowe pojecia", "wprowadzenie do", "istota i znaczenie",
    "w ujeciu", "wybrane zagadnienia", "ogolna charakterystyka",
    "podstawy", "zasady", "pojecie", "definicja", "rodzaje", "typy"
  )
  academic_pattern <- paste0("\\b(", paste(academic_noise, collapse = "|"), ")\\b")
  is_high_value    <- !str_detect(tolower(phrases), academic_pattern)
  ordered_phrases  <- unique(c(phrases[is_high_value], phrases[!is_high_value]))
  paste(ordered_phrases, collapse = "; ")
}

pwr_framework_clean <- semantic_buckets %>%
  filter(!is.na(KEU_Code), !is.na(KEU_Summary), KEU_Summary != "") %>%
  mutate(KEU_Summary_Smart = map_chr(KEU_Summary, smart_extract_key_phrases)) %>%
  select(KEU_Code, KEU_Type, KEU_Summary = KEU_Summary_Smart) %>%
  distinct(KEU_Code, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 3. Filters and patterns
# ------------------------------------------------------------------------------
LANGUAGE_DRIVING_PATTERN <- regex(
  paste(
    "jezyk (angielski|niemiecki|francuski|hiszpanski|wloski|rosyjski|chinski|obcy)",
    "j\\. angielski", "j\\. niemiecki", "english", "german", "french",
    "prawo jazdy", "kat\\. b", "kategoria b",
    sep = "|"
  ),
  ignore_case = TRUE
)

# Boilerplate job-description phrases sometimes extracted as fake competences.
# "Inne, dorazne zadania", "inne obowiazki wynikajace z..." are structural
# artifacts from Polish job ads -- not actual skills.
NON_COMPETENCE_ARTIFACT_PATTERN <- regex(
  paste(
    "inne.*zadania",
    "dorazne zadania",
    "inne obowiazki",
    "zadania wynikajace z",
    "inne prace zlecone",
    "wykonywanie innych.*polecen",
    "pozostale.*obowiazki",
    sep = "|"
  ),
  ignore_case = TRUE
)

# ------------------------------------------------------------------------------
# 3a. DETERMINISTIC GUARD: named systems / technologies
#
# After AI returns results, any competence that contains a recognised named
# system/acronym is checked: if that name does not appear literally in the
# KEU_Summary of the matched code, the match is force-downgraded to
# competency_gap. Hard R-level override, not a prompt instruction.
# Also sets Final_Bucket when it downgrades.
# FIX: added "Oracle APEX" and "APEX" -- these are specific named platforms
# not present in any KEU_Summary, so any AI match to a KEU must be rejected.
# ------------------------------------------------------------------------------
NAMED_SYSTEM_TERMS <- c(
  "ATS", "SAP", "SAP EWM", "SAP ABAP", "SAP HANA", "WMS", "EWM", "ERP",
  "CRM", "TMS", "MES", "PLM", "CMS", "EDI", "AWS", "Azure", "GCP",
  "Oracle APEX", "APEX",
  "Power BI", "Tableau", "Salesforce", "JIRA", "Confluence", "ServiceNow",
  "VBA Excel", "VBA", "Arena", "Python", "3ds Max", "MS Project", "Excel",
  "Word", "PowerPoint", "HTML", "SQL", "Microsoft Access", "SIZ",
  "WordPress", "BPMN", "OLAP", "Big Data", "Figma", "Canva", "R"
)
NAMED_SYSTEM_TERMS <- NAMED_SYSTEM_TERMS[order(-nchar(NAMED_SYSTEM_TERMS))]

NAMED_SYSTEM_PATTERN <- regex(
  paste0("\\b(", paste(str_replace_all(NAMED_SYSTEM_TERMS, " ", "\\\\s+"), collapse = "|"), ")\\b"),
  ignore_case = FALSE
)

extract_named_systems <- function(competence_name) {
  hits <- str_extract_all(competence_name, NAMED_SYSTEM_PATTERN)[[1]]
  unique(hits)
}

verify_named_system_in_keu <- function(matched_code, named_terms, keu_framework) {
  if (matched_code == "competency_gap" || length(named_terms) == 0) return(TRUE)
  summary_row <- keu_framework$KEU_Summary[keu_framework$KEU_Code == matched_code]
  if (length(summary_row) == 0 || is.na(summary_row[1])) return(FALSE)
  any(map_lgl(named_terms, ~ str_detect(summary_row[1], regex(fixed(.x), ignore_case = TRUE))))
}

enforce_named_system_guard <- function(results_df, keu_framework) {
  results_df %>%
    mutate(
      .named_terms = map(Competence_Name, ~ if (is.na(.x)) character(0) else extract_named_systems(.x)),
      .verified    = map2_lgl(Matched_Code, .named_terms,
                              ~ verify_named_system_in_keu(.x, .y, keu_framework)),
      Matched_Code = if_else(!.verified, "competency_gap", Matched_Code),
      Status       = if_else(Matched_Code == "competency_gap", "Gap", Status),
      Final_Bucket = if_else(!.verified, "MANAGEMENT_INFORMATION_SYSTEMS", Final_Bucket)
    ) %>%
    select(-.named_terms, -.verified)
}

# ------------------------------------------------------------------------------
# 3b. DETERMINISTIC GUARD: domain-C phrases
#
# Domain phrases like "rekrutacja", "supply chain", "category management" that
# are Category C in the prompt but occasionally mapped to a generic KEU by the
# local model. Also sets Final_Bucket when it downgrades.
# ------------------------------------------------------------------------------
DOMAIN_C_STEMS <- c(
  "rekrutacj", "recruitment", "talent acquisition", "employer branding",
  "lancuch dostaw", "supply chain", "category management",
  "zarzadzanie zapasami", "logistyka magazynow", "magazynow", "warehouse"
)

detect_domain_c_stems <- function(competence_name) {
  name_lower <- tolower(competence_name)
  DOMAIN_C_STEMS[map_lgl(DOMAIN_C_STEMS, ~ str_detect(name_lower, fixed(.x)))]
}

verify_domain_c_in_keu <- function(matched_code, stems, keu_framework) {
  if (matched_code == "competency_gap" || length(stems) == 0) return(TRUE)
  summary_row <- keu_framework$KEU_Summary[keu_framework$KEU_Code == matched_code]
  if (length(summary_row) == 0 || is.na(summary_row[1])) return(FALSE)
  summary_lower <- tolower(summary_row[1])
  any(map_lgl(stems, ~ str_detect(summary_lower, fixed(.x))))
}

enforce_domain_c_guard <- function(results_df, keu_framework) {
  results_df %>%
    mutate(
      .domain_stems = map(Competence_Name, ~ if (is.na(.x)) character(0) else detect_domain_c_stems(.x)),
      .verified2    = map2_lgl(Matched_Code, .domain_stems,
                               ~ verify_domain_c_in_keu(.x, .y, keu_framework)),
      Matched_Code  = if_else(!.verified2, "competency_gap", Matched_Code),
      Status        = if_else(Matched_Code == "competency_gap", "Gap", Status),
      Final_Bucket  = if_else(
        !.verified2,
        if_else(
          str_detect(tolower(Competence_Name),
                     "rekrut|recruitment|talent acquisition|employer branding"),
          "HR_RECRUITMENT",
          "PRODUCTION_LOGISTICS"
        ),
        Final_Bucket
      )
    ) %>%
    select(-.domain_stems, -.verified2)
}

# ------------------------------------------------------------------------------
# 3c. DETERMINISTIC GUARD: soft skills -> social competence KEU codes
#
# "Teamwork", "Collaboration", "Communication", "Relationship Building" and
# "Conflict Management" ARE covered by PWr social competence codes (K codes).
# The AI misclassifies them as Gap because Category A in the prompt was too
# broad. This guard runs after AI and after other guards, upgrading matched
# patterns to Standard with the correct K code.
# Only fires if Matched_Code is still competency_gap -- does not overwrite
# any KEU code that AI already matched correctly.
#
# FIX: added "relationship build" -> K1_IZ_K05 (Partycypacja, praca zespolowa)
# FIX: added "conflict manag|internal communication" -> K1_IZ_K04
#      (Rozwiazywanie konfliktow; komunikacja w organizacji -- literal KEU text)
# ------------------------------------------------------------------------------
SOFT_SKILLS_KEU_MAP <- list(
  list(pattern = "teamwork|team work|praca zespolow|wspolpraca zespolow",
       keu     = "K1_IZ_K02",
       bucket  = "SOFT_INTERPERSONAL"),
  list(pattern = "collaboration|collaborate|cross.functional collab",
       keu     = "K1_IZ_K07",
       bucket  = "SOFT_INTERPERSONAL"),
  list(pattern = "relationship build|interpersonal relation|budowanie relacj",
       keu     = "K1_IZ_K05",
       bucket  = "SOFT_INTERPERSONAL"),
  list(pattern = "conflict manag|conflict resol|internal communication|komunikacj.*wewnetrz|rozwiazywanie konflikt",
       keu     = "K1_IZ_K04",
       bucket  = "SOFT_INTERPERSONAL"),
  list(pattern = "communication skill|communication abilit|effective communicat|komunikacj",
       keu     = "K1_IZ_K04",
       bucket  = "SOFT_INTERPERSONAL"),
  list(pattern = "self.manag|working independ|samodzielno",
       keu     = "K1_IZ_K06",
       bucket  = "SOFT_INTERPERSONAL"),
  list(pattern = "innovative|innovation|collaborative environment|kreatywno",
       keu     = "K1_IZ_K06",
       bucket  = "SOFT_INTERPERSONAL")
)

enforce_soft_skills_standard <- function(results_df) {
  results_df %>%
    mutate(
      .soft_match = map_chr(Competence_Name, function(name) {
        if (is.na(name)) return(NA_character_)
        name_lower <- tolower(name)
        for (rule in SOFT_SKILLS_KEU_MAP) {
          if (str_detect(name_lower, rule$pattern)) return(rule$keu)
        }
        NA_character_
      }),
      .soft_bucket = map_chr(Competence_Name, function(name) {
        if (is.na(name)) return(NA_character_)
        name_lower <- tolower(name)
        for (rule in SOFT_SKILLS_KEU_MAP) {
          if (str_detect(name_lower, rule$pattern)) return(rule$bucket)
        }
        NA_character_
      }),
      Matched_Code = if_else(
        Matched_Code == "competency_gap" & !is.na(.soft_match),
        .soft_match, Matched_Code
      ),
      Status = if_else(Matched_Code != "competency_gap", "Standard", Status),
      Final_Bucket = if_else(
        !is.na(.soft_bucket) & (is.na(Final_Bucket) | Final_Bucket == "OTHER_UNCLASSIFIED"),
        .soft_bucket, Final_Bucket
      )
    ) %>%
    select(-.soft_match, -.soft_bucket)
}

# ------------------------------------------------------------------------------
# 3a-bis. EXTRA_TECH_STANDARD_TERMS -- tools confirmed present in PWr curriculum
#
# If any of these appear in Mandatory_tech or Nice_to_have_tech, force
# Status = "Standard" with a verified KEU code. These tools are taught at PWr
# but their English names don't appear literally in Polish KEU_Summary text,
# so the dictionary lookup fails silently.
# ------------------------------------------------------------------------------
EXTRA_TECH_STANDARD_TERMS <- c(
  "R", "CSS", "HTML", "ML", "WordPress", "SQL", "Microsoft Access",
  "Figma", "Canva", "OLAP", "BPMN", "BMPN", "Arena", "VBA", "Tableau",
  "3ds Max", "MS Project", "Ma Project"
)

# KEU codes verified against keu_semantic_buckets.csv:
#   W06: "relacyjna baza danych; strukturalny jezyk zapytan; hurtownia danych"
#   W11: "Zarzadzanie projektami; Metodyki; Narzedzia informatyczne"
#   W12: "Technologie internetowe; Zastosowania IT; Projekt informatyczny"
#   W15: "Analityka; Wizualizacje; Pulpity menedzerskie; Metodyka CRISP"
#   U16: "programowanie VBA; MS Excel; programowanie Python; automatyzacja"
EXTRA_TECH_KEU_MAP <- c(
  "r"                = "K1_IZ_U16",
  "css"              = "K1_IZ_W12",
  "html"             = "K1_IZ_W12",
  "ml"               = "K1_IZ_W15",
  "wordpress"        = "K1_IZ_W12",
  "sql"              = "K1_IZ_W06",
  "microsoft access" = "K1_IZ_W06",
  "figma"            = "K1_IZ_W12",
  "canva"            = "K1_IZ_W12",
  "olap"             = "K1_IZ_W06",
  "bpmn"             = "K1_IZ_U16",
  "bmpn"             = "K1_IZ_U16",
  "arena"            = "K1_IZ_U16",
  "vba"              = "K1_IZ_U16",
  "tableau"          = "K1_IZ_W15",
  "3ds max"          = "K1_IZ_W12",
  "ms project"       = "K1_IZ_W11",
  "ma project"       = "K1_IZ_W11"
)

EXTRA_TECH_BUCKET_MAP <- c(
  "r"                = "PROGRAMMING_WEB",
  "css"              = "PROGRAMMING_WEB",
  "html"             = "PROGRAMMING_WEB",
  "ml"               = "AI_ARTIFICIAL_INTELLIGENCE",
  "wordpress"        = "PROGRAMMING_WEB",
  "sql"              = "BIG_DATA_DATABASES",
  "microsoft access" = "BIG_DATA_DATABASES",
  "figma"            = "IT_SERVICES_CLOUD_UX",
  "canva"            = "E_COMMERCE_MARKETING",
  "olap"             = "BIG_DATA_DATABASES",
  "bpmn"             = "PROCESS_OPTIMIZATION",
  "bmpn"             = "PROCESS_OPTIMIZATION",
  "arena"            = "PROCESS_OPTIMIZATION",
  "vba"              = "PROGRAMMING_WEB",
  "tableau"          = "DATA_ANALYSIS_STATS",
  "3ds max"          = "IT_SERVICES_CLOUD_UX",
  "ms project"       = "PROJECT_MANAGEMENT",
  "ma project"       = "PROJECT_MANAGEMENT", 
  "python"                 = "PROGRAMMING_WEB",          
  "javascript"             = "PROGRAMMING_WEB",
  "oracle"                 = "BIG_DATA_DATABASES",
  "oracle data integrator" = "BIG_DATA_DATABASES",
  "microsoft power bi"     = "DATA_ANALYSIS_STATS",
  "git"                    = "PROGRAMMING_WEB",
  "svn"                    = "PROGRAMMING_WEB",
  "azure"                  = "IT_SERVICES_CLOUD_UX",
  "databricks"             = "BIG_DATA_DATABASES"
)

clean_tech_field <- function(tech_string) {
  if (is.na(tech_string) || tech_string == "" || tech_string == "NA") return(character(0))
  cleaned <- str_replace_all(tech_string, "[\\[\\]\\\"\\']", " ")
  items   <- str_split(cleaned, ",|;|/")[[1]] %>% str_trim()
  items   <- items[nchar(items) > 0]
  items   <- items[!str_detect(items, LANGUAGE_DRIVING_PATTERN)]
  unique(items)
}

# ------------------------------------------------------------------------------
# 4. Call llama.cpp with exponential backoff
# ------------------------------------------------------------------------------
call_llama <- function(system_prompt, user_prompt,
                       url       = LLAMA_URL,
                       model     = LLAMA_MODEL,
                       api_key   = GEMINI_API_KEY,
                       timeout   = TIMEOUT_SEC,
                       max_retry = MAX_RETRIES) {
  
  endpoint <- paste0(url, "/", model, ":generateContent")
  
  body <- list(
    system_instruction = list(
      parts = list(list(text = system_prompt))
    ),
    contents = list(
      list(
        role  = "user",
        parts = list(list(text = user_prompt))
      )
    ),
    generationConfig = list(
      temperature     = 0,
      maxOutputTokens = 32768,
      # Gemini 2.5 domyslnie "mysli" wewnetrznie przed odpowiedzia, a te tokeny
      # liczone sa do maxOutputTokens. thinkingBudget=0 ma to wylaczyc, ale jest to
      # znany, niedoskonale dzialajacy mechanizm (model czasem i tak zuzywa tokeny
      # na "thoughts"), wiec dajemy duzy zapas w maxOutputTokens (model wspiera do 65535).
      thinkingConfig = list(
        thinkingBudget = 0
      )
    )
  )
  
  for (attempt in seq_len(max_retry)) {
    resp <- tryCatch({
      request(endpoint) %>%
        req_headers(
          "Content-Type"   = "application/json",
          "x-goog-api-key" = api_key
        ) %>%
        req_body_json(body) %>%
        req_timeout(timeout) %>%
        req_error(is_error = function(resp) FALSE) %>%  # nie wywalaj bledu, obsluzymy status sami
        req_perform()
    }, error = function(e) {
      message("Blad polaczenia z Gemini (próba ", attempt, "/", max_retry, "): ", conditionMessage(e))
      NULL
    })
    
    if (is.null(resp)) {
      Sys.sleep(2 ^ attempt)
      next
    }
    
    status <- resp_status(resp)
    
    # 429 -- rate limit (AI Studio ma niski limit zapytan/min, szczegolnie na darmowym tierze).
    # 503 -- model przeciazony. To jest glowna ochrona przed ponowna blokada.
    if (status == 429 || status == 503) {
      retry_after <- resp_header(resp, "retry-after")
      wait_sec <- if (!is.null(retry_after) && !is.na(suppressWarnings(as.numeric(retry_after)))) {
        as.numeric(retry_after) + 1
      } else {
        30 * attempt  # narastajacy, dlugi odstep -- 30s, 60s, 90s, ...
      }
      message("Gemini rate-limit/503 (status ", status, "), czekam ", wait_sec, "s przed kolejna proba...")
      Sys.sleep(wait_sec)
      next
    }
    
    # 400/401/403 -- zly klucz, zly format requestu lub brak dostepu -- brak sensu w ponawianiu
    if (status %in% c(400, 401, 403)) {
      err_body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
      err_msg  <- if (!is.null(err_body) && !is.null(err_body$error$message)) err_body$error$message else "(brak szczegolow)"
      stop("Gemini odmowil zadania (status ", status, "): ", err_msg, ". Sprawdz GEMINI_API_KEY i nazwe modelu.")
    }
    
    if (status >= 500) {
      Sys.sleep(2 ^ attempt)
      next
    }
    
    if (status >= 200 && status < 300) {
      content <- tryCatch({
        parsed_body   <- resp_body_json(resp)
        finish_reason <- parsed_body$candidates[[1]]$finishReason
        text_out      <- parsed_body$candidates[[1]]$content$parts[[1]]$text
        
        if (!is.null(finish_reason) && finish_reason == "MAX_TOKENS") {
          # Odpowiedz ucieta w polowie -- prawdopodobnie niekompletny JSON.
          # NIE zwracamy tego jako sukces, bo zostalby zapisany w cache na trwale
          # jako poprawny wynik. content <- NULL sprawi, ze petla nizej ponowi
          # proba (Sys.sleep + next), a NIE wyjdzie z funkcji call_llama.
          message("Gemini finishReason = MAX_TOKENS -- odpowiedz ucieta, traktuje jako blad i ponawiam.")
          NULL
        } else {
          if (!is.null(finish_reason) && finish_reason != "STOP") {
            message("Gemini finishReason = ", finish_reason, " -- mozliwy brak pelnej odpowiedzi tekstowej.")
          }
          text_out
        }
      }, error = function(e) NULL)
      if (!is.null(content)) return(content)
    } else {
      message("Gemini zwrocil status ", status, " (próba ", attempt, "/", max_retry, ")")
    }
    
    Sys.sleep(2 ^ attempt)
  }
  ""
}

# ------------------------------------------------------------------------------
# 5. Parse JSON from AI response
# ------------------------------------------------------------------------------
parse_ai_output <- function(ai_output) {
  if (is.null(ai_output) || nchar(trimws(ai_output)) == 0) return(tibble())
  start   <- str_locate(ai_output, "\\[")[1, "start"]
  end_all <- str_locate_all(ai_output, "\\]")[[1]]
  if (is.na(start) || nrow(end_all) == 0) return(tibble())
  end_pos <- end_all[nrow(end_all), "end"]
  cleaned <- str_sub(ai_output, start, end_pos)
  tryCatch({
    parsed <- fromJSON(cleaned, simplifyVector = TRUE)
    if (is.list(parsed) && !is.data.frame(parsed) && length(parsed) == 1) parsed <- parsed[[1]]
    if (is.list(parsed) && !is.data.frame(parsed)) parsed <- bind_rows(parsed)
    as_tibble(parsed)
  }, error = function(e) tibble())
}

ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  df
}

REQUIRED_RESULT_COLS <- c("Text_Id", "Competence_Name", "Matched_Code", "Status", "Final_Bucket")

sanitize_final_bucket <- function(bucket_vec) {
  if_else(bucket_vec %in% FINAL_BUCKET_VALUES, bucket_vec, "OTHER_UNCLASSIFIED")
}

# ------------------------------------------------------------------------------
# 6. DICTIONARY-BASED TECH MAPPING -- pure R, no AI
# ------------------------------------------------------------------------------
tech_keu_dictionary <- pwr_framework_clean %>%
  mutate(keywords = str_split(tolower(KEU_Summary), ";\\s*")) %>%
  unnest(keywords) %>%
  mutate(keywords = str_trim(keywords)) %>%
  filter(nchar(keywords) > 2) %>%
  select(keyword = keywords, KEU_Code)

ARCHITECTURE_PATTERN_TERMS <- c(
  "ddd", "domain driven design", "cqrs", "hexagonal architecture",
  "event-driven architecture", "event driven architecture", "eda",
  "microservices", "microservice"
)

lookup_tech_in_dict <- function(tech_name, dictionary) {
  tech_lower <- tolower(str_trim(tech_name))
  hit <- dictionary %>% filter(keyword == tech_lower)
  if (nrow(hit) > 0) return(hit$KEU_Code[[1]])
  hit2 <- dictionary %>%
    filter(str_detect(keyword, fixed(tech_lower)) | str_detect(tech_lower, fixed(keyword)))
  if (nrow(hit2) > 0) return(hit2$KEU_Code[[1]])
  if (tech_lower %in% ARCHITECTURE_PATTERN_TERMS) {
    cands <- get_top_keu_candidates(tech_name, top_n = 1)
    if (nrow(cands) > 0) return(cands$KEU_Code[[1]])
  }
  NA_character_
}

map_tech_list_pure_r <- function(tech_list, source_label, dictionary) {
  if (length(tech_list) == 0) return(tibble(
    Competence_Name = character(0),
    Matched_Code    = character(0),
    Status          = character(0),
    Source          = character(0),
    Final_Bucket    = character(0)
  ))
  
  dict_codes <- map_chr(tech_list, ~ lookup_tech_in_dict(.x, dictionary))
  
  extra_codes <- map_chr(tech_list, function(tech) {
    key <- tolower(str_trim(tech))
    hit <- EXTRA_TECH_KEU_MAP[key]
    if (!is.na(hit)) return(hit) else return(NA_character_)
  })
  
  final_codes <- if_else(!is.na(dict_codes), dict_codes,
                         if_else(!is.na(extra_codes), extra_codes, NA_character_))
  
  tech_buckets <- map_chr(tech_list, function(tech) {
    key <- tolower(str_trim(tech))
    hit <- EXTRA_TECH_BUCKET_MAP[key]
    if (!is.na(hit)) return(hit) else return("OTHER_UNCLASSIFIED")
  })
  
  tibble(
    Competence_Name = tech_list,
    Matched_Code    = if_else(!is.na(final_codes), final_codes, "competency_gap"),
    Status          = if_else(!is.na(final_codes), "Standard", "Gap"),
    Source          = source_label,
    Final_Bucket    = tech_buckets
  )
}

# ------------------------------------------------------------------------------
# 7. SEMANTIC PREFILTER -- top N KEU candidates per text
# ------------------------------------------------------------------------------
POLISH_STOPWORDS <- c(
  "i", "w", "z", "na", "do", "sie", "nie", "to", "jest", "oraz", "lub", "dla",
  "ktory", "ktora", "ktore", "jako", "przez", "po", "od", "za", "tym", "tej",
  "tego", "ich", "jej", "go", "byc", "ma", "maja", "moze", "mozna", "sa",
  "wiedza", "umiejetnosc", "umiejetnosci", "znajomosc", "podstawowy", "podstawowa",
  "podstawowe", "ogolny", "ogolna", "ogolne", "student",
  "studenta", "zakresie", "obszar", "obszarze", "kompetencje", "kompetencja"
)

tokenize <- function(s) {
  toks <- s %>% tolower() %>% str_replace_all("[^a-z0-9 ]", " ") %>%
    str_split("\\s+") %>% pluck(1) %>% .[nchar(.) > 2]
  toks[!toks %in% POLISH_STOPWORDS]
}

# EN -> PL bridge: English job postings produce tokens with zero overlap with
# Polish KEU summaries. The bridge injects equivalent Polish tokens before IDF
# scoring so the right KEU codes surface as candidates.
# FIX: added "data warehouse" / "dwh" / "data warehousing" -> "hurtownia",
# "danych", "relacyjna" -- these map to W06 which literally contains
# "hurtownia danych; relacyjna baza danych" in its KEU_Summary.
EN_PL_BRIDGE <- list(
  "scrum"                       = c("metodyki", "zwinne", "agile", "projektami"),
  "agile"                       = c("metodyki", "zwinne", "projektami"),
  "kanban"                      = c("metodyki", "zwinne", "projektami"),
  "sprint"                      = c("metodyki", "zwinne", "projektami"),
  "domain driven design"        = c("architektura", "projektowanie", "systemow", "oprogramowania"),
  "ddd"                         = c("architektura", "projektowanie", "systemow", "oprogramowania"),
  "cqrs"                        = c("architektura", "projektowanie", "systemow", "oprogramowania"),
  "hexagonal architecture"      = c("architektura", "projektowanie", "systemow", "oprogramowania"),
  "event driven architecture"   = c("architektura", "integracja", "systemow", "oprogramowania"),
  "event-driven architecture"   = c("architektura", "integracja", "systemow", "oprogramowania"),
  "microservices"               = c("architektura", "systemow", "integracja"),
  "system integration"          = c("integracja", "systemow"),
  "data warehouse"              = c("hurtownia", "danych", "relacyjna"),
  "data warehousing"            = c("hurtownia", "danych", "relacyjna"),
  "dwh"                         = c("hurtownia", "danych", "relacyjna"),
  "project management"          = c("zarzadzanie", "projektami", "planowanie", "projektu"),
  "business analysis"           = c("analiza", "biznesowa", "analizy", "biznesowe"),
  "stakeholder management"      = c("interesariusze", "komunikacja", "wspolpraca"),
  "client management"           = c("obsluga", "klienta", "orientacja", "kliencie"),
  "customer management"         = c("obsluga", "klienta", "orientacja", "kliencie"),
  "leadership"                  = c("kierowanie", "zespolem", "zarzadzanie"),
  "root cause analysis"         = c("analiza", "przyczyn", "rozwiazywanie", "problemow"),
  "structured problem solving"  = c("rozwiazywanie", "problemow", "analiza"),
  "process improvement"         = c("doskonalenie", "procesow", "optymalizacja", "procesow"),
  "process improvement (analyzing and improving warehouse processes)" =
    c("doskonalenie", "procesow", "optymalizacja"),
  "inventory management"        = c("zapasy", "zapasow"),
  "replenishment"               = c("zapasy", "zapasow", "uzupelnianie")
)

EN_PL_BRIDGE_STEMS <- list(
  "project manag"   = c("zarzadzanie", "projektami", "planowanie", "projektu"),
  "business analys" = c("analiza", "biznesowa", "analizy", "biznesowe"),
  "stakeholder"     = c("interesariusze", "komunikacja", "wspolpraca"),
  "root cause"      = c("analiza", "przyczyn", "rozwiazywanie", "problemow"),
  "problem solv"    = c("rozwiazywanie", "problemow", "analiza"),
  "integrat"        = c("integracja", "systemow"),
  "architect"       = c("architektura", "projektowanie", "systemow"),
  "leadership"      = c("kierowanie", "zespolem", "zarzadzanie"),
  "lead "           = c("kierowanie", "zespolem", "zarzadzanie"),
  "client manag"    = c("obsluga", "klienta", "orientacja", "kliencie"),
  "customer manag"  = c("obsluga", "klienta", "orientacja", "kliencie"),
  "process improv"  = c("doskonalenie", "procesow", "optymalizacja"),
  "inventory"       = c("zapasy", "zapasow"),
  "replenish"       = c("zapasy", "zapasow", "uzupelnianie"),
  "configur"        = c("konfiguracja", "wdrozenie", "systemow"),
  "data warehous"   = c("hurtownia", "danych", "relacyjna")
)

translate_en_signals <- function(text) {
  text_lower <- tolower(text)
  hits <- character(0)
  for (phrase in names(EN_PL_BRIDGE)) {
    if (str_detect(text_lower, fixed(phrase))) hits <- c(hits, EN_PL_BRIDGE[[phrase]])
  }
  for (stem in names(EN_PL_BRIDGE_STEMS)) {
    if (str_detect(text_lower, fixed(stem))) hits <- c(hits, EN_PL_BRIDGE_STEMS[[stem]])
  }
  unique(hits)
}

keu_tokens_list <- pwr_framework_clean$KEU_Summary %>% map(tokenize)
names(keu_tokens_list) <- pwr_framework_clean$KEU_Code

doc_freq          <- table(unlist(map(keu_tokens_list, unique)))
n_docs            <- length(keu_tokens_list)
idf_lookup        <- log(n_docs / (1 + as.numeric(doc_freq)))
names(idf_lookup) <- names(doc_freq)

token_idf <- function(tok) {
  val <- idf_lookup[tok]
  if (is.na(val)) return(1)
  val
}

keu_phrase_count <- pwr_framework_clean$KEU_Summary %>%
  map_int(~ length(str_split(.x, ";\\s*")[[1]]))
names(keu_phrase_count) <- pwr_framework_clean$KEU_Code

phrase_count_median <- median(keu_phrase_count)
phrase_count_p60    <- quantile(keu_phrase_count, 0.60, names = FALSE)
phrase_count_p80    <- quantile(keu_phrase_count, 0.80, names = FALSE)

bucket_width_level <- function(keu_code) {
  n <- keu_phrase_count[[keu_code]]
  if (is.null(n) || is.na(n)) return("narrow")
  if (n > phrase_count_p80) return("super_wide")
  if (n > phrase_count_p60) return("wide")
  "narrow"
}

is_wide_bucket <- keu_phrase_count > phrase_count_p60
names(is_wide_bucket) <- names(keu_phrase_count)

message("KEU phrase count median: ", phrase_count_median,
        " | P60: ", phrase_count_p60, " | P80: ", phrase_count_p80)
message("Super-wide codes (P80+, MIN_SHARED=3): ",
        paste(names(keu_phrase_count)[keu_phrase_count > phrase_count_p80], collapse = ", "))
message("Wide codes (P60-P80, MIN_SHARED=2): ",
        paste(names(keu_phrase_count)[keu_phrase_count > phrase_count_p60 &
                                        keu_phrase_count <= phrase_count_p80], collapse = ", "))

bucket_width_penalty <- function(keu_code) {
  n <- keu_phrase_count[[keu_code]]
  if (is.null(n) || is.na(n)) return(1)
  (phrase_count_median / n) ^ 0.8
}

get_top_keu_candidates <- function(text, top_n = TOP_N_KEU_CANDIDATES) {
  text_tokens <- c(tokenize(text), translate_en_signals(text)) %>% unique()
  if (length(text_tokens) == 0) return(pwr_framework_clean %>% slice(0) %>%
                                         mutate(Width_Level = character(0)))
  scores <- map_dbl(names(keu_tokens_list), function(code) {
    keu_toks <- keu_tokens_list[[code]]
    shared   <- intersect(text_tokens, keu_toks)
    if (length(shared) == 0) return(0)
    level <- bucket_width_level(code)
    min_shared_required <- switch(level, "super_wide" = 3, "wide" = 2, 1)
    if (length(shared) < min_shared_required) return(0)
    raw_score  <- sum(map_dbl(shared, token_idf))
    base_score <- raw_score / sqrt(length(unique(keu_toks)))
    base_score * bucket_width_penalty(code)
  })
  names(scores) <- names(keu_tokens_list)
  ord       <- order(scores, decreasing = TRUE)
  ord       <- ord[scores[ord] > 0]
  top_codes <- names(keu_tokens_list)[ord][seq_len(min(top_n, length(ord)))]
  pwr_framework_clean %>%
    filter(KEU_Code %in% top_codes) %>%
    mutate(Width_Level = map_chr(KEU_Code, bucket_width_level))
}

# ------------------------------------------------------------------------------
# 8. PROMPT
# ------------------------------------------------------------------------------
SUPER_WIDE_CODES <- names(keu_phrase_count)[keu_phrase_count > phrase_count_p80]

create_combined_prompt <- function(texts_df) {
  super_wide_list    <- paste(SUPER_WIDE_CODES, collapse = ", ")
  bucket_values_list <- paste(FINAL_BUCKET_VALUES, collapse = ", ")
  
  blocks <- texts_df %>%
    mutate(block = paste0(
      "### Text_Id: ", Text_Id, "\n",
      "Job title: ", Job_title, "\n",
      "Technologies already covered (skip these): ", already_covered, "\n",
      "KEU candidates (map ONLY to these or competency_gap):\n", candidates_json, "\n",
      "JOB AD TEXT:\n", str_sub(Full_requirements, 1, MAX_FULLREQ_CHARS_IN_PROMPT)
    )) %>%
    pull(block) %>%
    paste(collapse = "\n\n")
  
  list(
    system = "Return only a JSON array, no other text whatsoever.",
    user = paste0(
      "For EACH block '### Text_Id: X' below, do BOTH steps in one pass:\n",
      "1. Extract remaining competences from the job ad text that are NOT already covered.\n",
      "   Include soft skills, methodologies, business processes, management/architectural competences.\n",
      "   Exclude completely: language requirements (e.g. English/German) and driving licences.\n",
      "2. Map each extracted competence to ONE KEU code from this block's candidate list if the competence is conceptually covered by KEU_Summary; otherwise return competency_gap.\n",
      "   Do NOT require literal word overlap. A widely recognised named subcategory of a broader KEU concept counts as a match (e.g. Scrum -> Agile project management; EDA -> event-driven architecture/design).\n\n",
      
      "=== IMPORTANT INTERPRETATION RULES ===\n",
      "1. Ignore wrapper labels before ':' or brackets, e.g. 'Business Process:', 'Domain Knowledge:', 'System Architecture:'. Classify only the substantive content after the label.\n\n",
      
      "2. CATEGORY A / B / C RULES\n\n",
      
      "CATEGORY A -- General attitudes / behavioural competences:\n",
      "Examples: Continuous Improvement and Ownership, Initiative, Adaptability, Can-do attitude, Willingness to learn, Proactive attitude, Teamwork, Collaboration, Communication Skills, Relationship Building, Conflict Management, Internal Communication.\n",
      "DEFAULT ASSUMPTION: this is NOT a gap. competency_gap is the EXCEPTION here, not the default.\n",
      "Before assigning competency_gap to a Category A competence, you MUST actively check every candidate KEU_Summary provided for this Text_Id and confirm that NONE of them conceptually cover it (even loosely -- teamwork, communication, self-development, organisational learning, and social/interpersonal competences are very broadly defined in the syllabus and usually ARE covered).\n",
      "Only return competency_gap for Category A if, after checking all candidates, the syllabus genuinely contains no mention -- direct or conceptual -- of that behavioural/interpersonal competence. If in doubt, map it rather than mark it as a gap.\n\n",
      
      "CATEGORY B -- Specific management / methodology / architecture competences:\n",
      "Map normally if the KEU genuinely covers the concept.\n",
      "- Project Management -> KEU with 'zarzadzanie projektami' or 'planowanie projektu'\n",
      "- Business Analysis -> KEU with 'analizy biznesowe' or 'analiza problemu'\n",
      "- Stakeholder Management -> KEU with 'interesariusz' or 'orientacja na klienta'\n",
      "- Client/Customer Management -> KEU with 'klient' or 'orientacja na klienta'\n",
      "- Leadership / Team Management -> KEU with 'kierowanie ludzmi' or 'zarzadzanie zespolem'\n",
      "- Scrum / Kanban / Sprint -> subcategory of Agile project management\n",
      "- DDD / CQRS / Hexagonal Architecture / EDA / Microservices -> software architecture / system design / integration KEUs\n",
      "- Data Warehouse / DWH / Data Warehousing -> KEU with 'hurtownia danych'\n\n",
      
      "CATEGORY C -- Business-domain / named-system competences:\n",
      "Require explicit domain/system coverage in KEU; otherwise competency_gap.\n",
      "- Recruitment / Talent Acquisition / ATS -> KEU must explicitly cover recruitment\n",
      "- Supply Chain / Category Management / Inventory -> KEU must explicitly cover supply chain\n",
      "- SAP / ERP / ATS / WMS / EWM / CRM / AWS / APEX and other named systems/tools -> must appear explicitly in KEU_Summary; general IT/business coverage is NOT enough\n\n",
      
      "3. SUPER-WIDE CODES: extra caution\n",
      "Codes: ", super_wide_list, " have 44+ unrelated phrases from different domains.\n",
      "- Category A: do NOT assign automatically; map only if KEU_Summary clearly covers the behavioural/social competence\n",
      "- Category B: assign only if KEU_Summary has an exact or clearly direct phrase-level match\n",
      "- For super-wide candidates, require at least 2 matching signals/phrases before assigning them\n\n",
      
      "=== FINAL BUCKET ASSIGNMENT ===\n",
      "For EACH extracted competence assign EXACTLY ONE Final_Bucket from: [", bucket_values_list, "]\n\n",
      "Bucket guidance:\n",
      "- AI_ARTIFICIAL_INTELLIGENCE -> AI, ML, neural networks, LLMs, deep learning\n",
      "- BIG_DATA_DATABASES -> Big Data, SQL/NoSQL, data warehouses, data modelling\n",
      "- DATA_ANALYSIS_STATS -> statistics, analytics, forecasting, econometrics\n",
      "- IT_SERVICES_CLOUD_UX -> cloud, IT architecture, UX/UI, IT security\n",
      "- PROGRAMMING_WEB -> programming, software engineering, web technologies\n",
      "- MANAGEMENT_INFORMATION_SYSTEMS -> MIS/SIZ, ERP, CRM, systems/tool integration\n",
      "- PROCESS_OPTIMIZATION -> process optimisation, BPMN, automation, operational research\n",
      "- PRODUCTION_LOGISTICS -> production, logistics, supply chain, warehousing\n",
      "- QUALITY_MANAGEMENT -> quality, ISO, Six Sigma, auditing\n",
      "- ERGONOMICS_WORKPLACE -> ergonomics, workplace/OHS\n",
      "- PROJECT_MANAGEMENT -> project management, Agile/Scrum, project controlling\n",
      "- INNOVATION_ENTREPRENEURSHIP -> innovation, entrepreneurship\n",
      "- FINANCE_ACCOUNTING -> finance, accounting, taxes\n",
      "- LEGAL_ECONOMY_ESG -> law, IP, economics, ESG/ecology\n",
      "- E_COMMERCE_MARKETING -> e-commerce, digital marketing, market analysis\n",
      "- LEADERSHIP_DECISION_MAKING -> leadership, managerial skills, decision-making\n",
      "- STAKEHOLDER_CLIENT_RELATIONS -> stakeholder/client relations, negotiations\n",
      "- HR_RECRUITMENT -> HR, recruitment, employer branding\n",
      "- SOFT_INTERPERSONAL -> behavioural / interpersonal competences\n",
      "- OTHER_UNCLASSIFIED -> niche items not fitting other buckets\n\n",
      
      blocks, "\n\n",
      "Output format (one flat array for all blocks):\n",
      "[{\"Text_Id\": \"...\", \"Competence_Name\": \"...\", ",
      "\"Matched_Code\": \"KEU code or 'competency_gap'\", ",
      "\"Status\": \"Standard or Gap\", ",
      "\"Final_Bucket\": \"ONE VALUE FROM THE LIST ABOVE\"}]"
    )
  )
}

# ------------------------------------------------------------------------------
# 9. Text cache
# ------------------------------------------------------------------------------
text_cache <- if (file.exists(TEXT_CACHE_FILE)) {
  tryCatch(readRDS(TEXT_CACHE_FILE), error = function(e) list())
} else {
  list()
}

# ------------------------------------------------------------------------------
# 10. Pipeline
# ------------------------------------------------------------------------------
full_batch <- market_data
if (TEST_MODE) full_batch <- full_batch %>% head(TEST_N)
full_batch <- full_batch %>% mutate(Row_Id = row_number())
message("Offers to process: ", nrow(full_batch))

# --- 10a. Tech matching ---
tech_results <- map_dfr(seq_len(nrow(full_batch)), function(i) {
  row       <- full_batch[i, ]
  mand_result <- map_tech_list_pure_r(clean_tech_field(row$Mandatory_tech),     "Tech_Mandatory",    tech_keu_dictionary)
  nice_result <- map_tech_list_pure_r(clean_tech_field(row$Nice_to_have_tech),  "Tech_Nice_to_Have", tech_keu_dictionary)
  bind_rows(mand_result, nice_result) %>% mutate(Row_Id = row$Row_Id)
})

# --- 10b. Text deduplication ---
text_lookup <- full_batch %>%
  filter(!is.na(Full_requirements), nchar(trimws(Full_requirements)) > 0) %>%
  mutate(Text_Hash = map_chr(Full_requirements, ~ digest(.x, algo = "xxhash64"))) %>%
  select(Row_Id, Job_title, Full_requirements, Text_Hash)

unique_texts <- text_lookup %>%
  distinct(Text_Hash, .keep_all = TRUE) %>%
  mutate(Text_Id = paste0("T", row_number()))

already_covered_per_text <- tech_results %>%
  left_join(full_batch %>% select(Row_Id), by = "Row_Id") %>%
  group_by(Row_Id) %>%
  summarise(already_covered = paste(Competence_Name, collapse = ", "), .groups = "drop")

unique_texts <- unique_texts %>%
  left_join(already_covered_per_text, by = "Row_Id") %>%
  mutate(already_covered = if_else(is.na(already_covered), "none", already_covered))

needs_ai   <- unique_texts %>% filter(!(Text_Hash %in% names(text_cache)))
from_cache <- unique_texts %>% filter(Text_Hash %in% names(text_cache))

message("Unique texts: ", nrow(unique_texts),
        " | from cache: ", nrow(from_cache),
        " | to AI: ", nrow(needs_ai))

cached_text_results <- map_dfr(from_cache$Text_Hash, function(h) {
  res <- text_cache[[h]]
  if (is.null(res) || nrow(res) == 0) return(tibble())
  res
})
if (nrow(cached_text_results) > 0) {
  hash_to_id <- from_cache %>% select(Text_Hash, Text_Id)
  cached_text_results <- cached_text_results %>%
    rename(Text_Hash = Text_Id) %>%
    left_join(hash_to_id, by = "Text_Hash") %>%
    select(-Text_Hash)
}

# --- 10c. Semantic prefilter + AI calls ---
new_text_results <- tibble()

if (nrow(needs_ai) > 0) {
  needs_ai <- needs_ai %>%
    mutate(candidates_json = map_chr(Full_requirements, function(t) {
      cands <- get_top_keu_candidates(t)
      toJSON(cands %>% select(KEU_Code, KEU_Summary, Width_Level), auto_unbox = TRUE)
    }))
  
  chunks <- split(needs_ai, ceiling(seq_len(nrow(needs_ai)) / BATCH_TEXTS_PER_CALL))
  
  # ----------------------------------------------------------------------
  # Wywolania do Google AI Studio (Gemini): SEKWENCYJNIE, z odstepem INTER_CALL_SLEEP_SEC
  # miedzy kazda paczka ofert. Praca rownolegla (future_map) bije wiele
  # zapytan naraz i to najpewniej spowodowalo wczesniejsza blokade konta --
  # AI Studio liczy rate-limit (RPM/TPM) per-klucz, niezaleznie od tego z ilu workerow leci.
  #
  # CHECKPOINTING: co SAVE_EVERY paczek wyniki sa dopisywane do text_cache i
  # zapisywane na dysk (saveRDS). Jesli polaczenie padnie w polowie przebiegu,
  # wystarczy ponownie odpalic skrypt -- juz przetworzone teksty sa w cache i
  # "needs_ai" automatycznie je przeskoczy (patrz filtr wyzej:
  # needs_ai <- unique_texts %>% filter(!(Text_Hash %in% names(text_cache)))).
  # To dziala jak proponowane "puszczanie po 100-200 sztuk", ale bez koniecznosci
  # recznego zmieniania zakresu petli -- po restarcie skrypt sam wie, gdzie stanal.
  # ----------------------------------------------------------------------
  if (isTRUE(USE_PARALLEL)) {
    plan(multisession, workers = N_WORKERS)
  } else {
    plan(sequential)
  }
  handlers(global = TRUE)
  handlers("txtprogressbar")
  
  # Liczba paczek miedzy checkpointami: SAVE_EVERY to liczba OFERT, a chunk to
  # BATCH_TEXTS_PER_CALL ofert, wiec przeliczamy na liczbe paczek.
  CHECKPOINT_EVERY_CHUNKS <- max(1, round(SAVE_EVERY / BATCH_TEXTS_PER_CALL))
  
  checkpoint_cache <- function(chunk_outputs_so_far, needs_ai_df, text_cache_env) {
    if (length(chunk_outputs_so_far) == 0) return(invisible(NULL))
    done_results <- bind_rows(map(chunk_outputs_so_far, "parsed"))
    done_status  <- map_dfr(chunk_outputs_so_far, function(co) {
      tibble(Text_Id = co$text_ids, ai_call_succeeded = co$success)
    })
    id_to_hash_partial <- needs_ai_df %>%
      select(Text_Id, Text_Hash) %>%
      inner_join(done_status, by = "Text_Id")
    
    for (i in seq_len(nrow(id_to_hash_partial))) {
      tid     <- id_to_hash_partial$Text_Id[[i]]
      hash    <- id_to_hash_partial$Text_Hash[[i]]
      success <- id_to_hash_partial$ai_call_succeeded[[i]]
      if (!isTRUE(success)) next
      if (hash %in% names(text_cache_env$cache)) next  # juz zapisane w poprzednim checkpoincie
      sub_result <- done_results %>% filter(Text_Id == tid)
      if (nrow(sub_result) == 0) {
        sub_result <- tibble(Text_Id = tid, Competence_Name = NA_character_,
                             Matched_Code = "competency_gap", Status = "Gap",
                             Final_Bucket = NA_character_)
      }
      text_cache_env$cache[[hash]] <- sub_result
    }
    saveRDS(text_cache_env$cache, TEXT_CACHE_FILE)
    invisible(NULL)
  }
  
  # Srodowisko-wrapper, zeby checkpoint_cache mogl modyfikowac text_cache "na zywo"
  # (text_cache jest zdefiniowane wczesniej w skrypcie, w sekcji 9 -- tu tylko go opakowujemy)
  text_cache_env <- new.env()
  text_cache_env$cache <- text_cache
  
  with_progress({
    p <- progressor(steps = length(chunks))
    
    call_one_chunk <- function(chunk, idx, total) {
      prompts <- create_combined_prompt(chunk)
      out     <- call_llama(prompts$system, prompts$user)
      success <- nchar(trimws(out)) > 0
      if (!success) {
        message("WARNING: empty AI response for Text_Id(s): ",
                paste(chunk$Text_Id, collapse = ", "),
                " -- likely context overflow or timeout. NOT caching, will retry.")
      }
      parsed <- ensure_cols(parse_ai_output(out), REQUIRED_RESULT_COLS)
      p()
      # Dajemy serwerowi pelny odstep oddechu miedzy ofertami/paczkami --
      # nie czekamy po ostatniej paczce, nie ma sensu.
      if (idx < total) Sys.sleep(INTER_CALL_SLEEP_SEC)
      list(text_ids = chunk$Text_Id, success = success, parsed = parsed)
    }
    
    if (isTRUE(USE_PARALLEL)) {
      # Uwaga: w trybie rownoleglym odstep INTER_CALL_SLEEP_SEC nie chroni przed
      # rate-limitem tak skutecznie, bo workerzy strzelaja niezaleznie od siebie.
      # Checkpointing co-chunk takze nie dziala tutaj (future_map zwraca wszystko
      # na koniec) -- jesli chcesz checkpointow, zostaw USE_PARALLEL <- FALSE.
      chunk_outputs <- future_map(chunks, function(chunk) {
        prompts <- create_combined_prompt(chunk)
        out     <- call_llama(prompts$system, prompts$user)
        success <- nchar(trimws(out)) > 0
        if (!success) {
          message("WARNING: empty AI response for Text_Id(s): ",
                  paste(chunk$Text_Id, collapse = ", "),
                  " -- likely context overflow or timeout. NOT caching, will retry.")
        }
        parsed <- ensure_cols(parse_ai_output(out), REQUIRED_RESULT_COLS)
        p()
        list(text_ids = chunk$Text_Id, success = success, parsed = parsed)
      }, .options = furrr_options(seed = TRUE))
    } else {
      chunk_outputs <- vector("list", length(chunks))
      for (ci in seq_along(chunks)) {
        chunk_outputs[[ci]] <- call_one_chunk(chunks[[ci]], ci, length(chunks))
        
        # --- CHECKPOINT: co CHECKPOINT_EVERY_CHUNKS paczek zapisujemy postep na dysk ---
        if (ci %% CHECKPOINT_EVERY_CHUNKS == 0 || ci == length(chunks)) {
          checkpoint_cache(chunk_outputs[seq_len(ci)], needs_ai, text_cache_env)
          message("Checkpoint zapisany: ", ci, "/", length(chunks),
                  " paczek (~", ci * BATCH_TEXTS_PER_CALL, " ofert) -- ",
                  "mozesz bezpiecznie przerwac, postep jest na dysku w ", TEXT_CACHE_FILE)
        }
      }
    }
  })
  
  plan(sequential)
  
  # text_cache_env$cache zawiera teraz wszystkie checkpointy -- synchronizujemy
  # z glowna zmienna text_cache, zeby kod nizej (sekcja 10d i dalej) dzialal bez zmian
  text_cache <- text_cache_env$cache
  
  # Ten blok to "zapis finalny" -- dla USE_PARALLEL=FALSE cache juz zostal
  # zapisany checkpointami w trakcie petli (sekcja wyzej), wiec to tylko
  # potwierdzenie/synchronizacja na koniec. Dla USE_PARALLEL=TRUE to jest
  # JEDYNY moment zapisu, bo future_map nie wspiera checkpointow w trakcie.
  new_text_results <- bind_rows(map(chunk_outputs, "parsed"))
  
  success_by_text_id <- map_dfr(chunk_outputs, function(co) {
    tibble(Text_Id = co$text_ids, ai_call_succeeded = co$success)
  })
  
  id_to_hash <- needs_ai %>% select(Text_Id, Text_Hash) %>%
    left_join(success_by_text_id, by = "Text_Id")
  
  n_cached_now <- 0
  for (i in seq_len(nrow(id_to_hash))) {
    tid     <- id_to_hash$Text_Id[[i]]
    hash    <- id_to_hash$Text_Hash[[i]]
    success <- id_to_hash$ai_call_succeeded[[i]]
    if (!isTRUE(success)) next
    sub_result <- new_text_results %>% filter(Text_Id == tid)
    if (nrow(sub_result) == 0) {
      sub_result <- tibble(Text_Id = tid, Competence_Name = NA_character_,
                           Matched_Code = "competency_gap", Status = "Gap",
                           Final_Bucket = NA_character_)
    }
    text_cache[[hash]] <- sub_result
    n_cached_now <- n_cached_now + 1
  }
  saveRDS(text_cache, TEXT_CACHE_FILE)
  message("text_cache saved (", n_cached_now, " new entries; failures not cached).")
}

# --- 10d. Apply guards + sanitise ---
all_text_results <- bind_rows(cached_text_results, new_text_results) %>%
  mutate(Source = "Text_Analysis") %>%
  ensure_cols(REQUIRED_RESULT_COLS) %>%
  mutate(Final_Bucket = sanitize_final_bucket(Final_Bucket)) %>%
  filter(is.na(Competence_Name) | !str_detect(Competence_Name, LANGUAGE_DRIVING_PATTERN)) %>%
  filter(is.na(Competence_Name) | !str_detect(Competence_Name, NON_COMPETENCE_ARTIFACT_PATTERN)) %>%
  enforce_named_system_guard(pwr_framework_clean) %>%
  enforce_domain_c_guard(pwr_framework_clean) %>%
  enforce_soft_skills_standard()

# --- 10e. Map Text_Id -> Row_Id ---
text_id_to_rows <- unique_texts %>%
  left_join(text_lookup %>% select(Row_Id, Text_Hash), by = "Text_Hash",
            suffix = c("", "_dup")) %>%
  select(Text_Id, Row_Id = Row_Id_dup) %>%
  bind_rows(unique_texts %>% select(Text_Id, Row_Id)) %>%
  distinct()

text_results_per_offer <- all_text_results %>%
  filter(!is.na(Competence_Name)) %>%
  left_join(text_id_to_rows, by = "Text_Id", relationship = "many-to-many") %>%
  select(Row_Id, Competence_Name, Matched_Code, Status, Source, Final_Bucket) %>%
  filter(!is.na(Row_Id))

# --- 10f. Tech <-> text deduplication ---
tech_names_per_offer <- tech_results %>%
  filter(!is.na(Competence_Name)) %>%
  group_by(Row_Id) %>%
  summarise(tech_names = list(tolower(Competence_Name)), .groups = "drop")

is_covered_by_tech <- function(comp_name, tech_names_list) {
  if (is.na(comp_name) || length(tech_names_list) == 0) return(FALSE)
  comp_lower <- tolower(comp_name)
  any(map_lgl(tech_names_list, function(tech) {
    str_detect(comp_lower, fixed(tech)) || str_detect(tech, fixed(comp_lower))
  }))
}

text_results_per_offer <- text_results_per_offer %>%
  left_join(tech_names_per_offer, by = "Row_Id") %>%
  mutate(.is_dup = map2_lgl(Competence_Name, tech_names,
                            ~ is_covered_by_tech(.x, if (is.null(.y)) list() else .y))) %>%
  filter(!.is_dup) %>%
  select(-.is_dup, -tech_names)

# ------------------------------------------------------------------------------
# 11. Merge results
# ------------------------------------------------------------------------------
combined_results <- bind_rows(
  tech_results           %>% select(Row_Id, Competence_Name, Matched_Code, Status, Source, Final_Bucket),
  text_results_per_offer %>% select(Row_Id, Competence_Name, Matched_Code, Status, Source, Final_Bucket)
)

master_skill_matrix <- combined_results %>%
  left_join(full_batch %>% select(Row_Id, Job_offer_URL, Job_title, Company_name, Seniority_level),
            by = "Row_Id") %>%
  mutate(
    Matched_Code = if_else(is.na(Matched_Code) | Matched_Code %in% c("", "NA", "competency_gap"),
                           "competency_gap", as.character(Matched_Code)),
    Status       = if_else(Matched_Code == "competency_gap", "Gap", as.character(Status))
  ) %>%
  distinct(Job_offer_URL, Competence_Name, .keep_all = TRUE) %>%
  select(Competence_Name, Matched_Code, Status, Source, Final_Bucket,
         Job_offer_URL, Job_title, Company_name, Seniority_level)

write_excel_csv(master_skill_matrix, "final_competence_matrix.csv")
message("Saved final_competence_matrix.csv -- rows: ", nrow(master_skill_matrix))

# ------------------------------------------------------------------------------
# 12. Market aggregation
# ------------------------------------------------------------------------------
n_total_offers <- n_distinct(full_batch$Row_Id)

competence_with_row <- combined_results %>%
  left_join(full_batch %>% select(Row_Id, Job_offer_URL), by = "Row_Id") %>%
  mutate(
    Matched_Code = if_else(is.na(Matched_Code) | Matched_Code %in% c("", "NA", "competency_gap"),
                           "competency_gap", as.character(Matched_Code)),
    Status       = if_else(Matched_Code == "competency_gap", "Gap", as.character(Status))
  )

# 12a. By competence (text analysis only)
market_demand_by_competence <- competence_with_row %>%
  filter(!is.na(Competence_Name), Source == "Text_Analysis") %>%
  group_by(Competence_Name, Matched_Code, Status, Source, Final_Bucket) %>%
  summarise(N_Offers_Requiring = n_distinct(Row_Id), .groups = "drop") %>%
  mutate(Pct_Of_All_Offers = round(100 * N_Offers_Requiring / n_total_offers, 1)) %>%
  arrange(desc(N_Offers_Requiring))

# 12b. By KEU code (text analysis only)
market_demand_by_keu_code <- competence_with_row %>%
  filter(Matched_Code != "competency_gap", Source == "Text_Analysis") %>%
  group_by(Matched_Code) %>%
  summarise(
    N_Offers_Requiring     = n_distinct(Row_Id),
    N_Distinct_Competences = n_distinct(Competence_Name),
    Example_Competences    = paste(unique(Competence_Name)[seq_len(min(3, n_distinct(Competence_Name)))],
                                   collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(Pct_Of_All_Offers = round(100 * N_Offers_Requiring / n_total_offers, 1)) %>%
  arrange(desc(N_Offers_Requiring))

# 12c. Tech demand -- distinct offers + total mentions, by source and bucket
tech_demand <- tech_results %>%
  filter(!is.na(Competence_Name)) %>%
  group_by(Competence_Name, Source, Final_Bucket) %>%
  summarise(
    N_Offers_Requiring = n_distinct(Row_Id),
    N_Mentions         = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = Source,
    values_from = c(N_Offers_Requiring, N_Mentions),
    values_fill = 0
  ) %>%
  { if (!"N_Offers_Requiring_Tech_Mandatory"    %in% names(.)) mutate(., N_Offers_Requiring_Tech_Mandatory    = 0L) else . } %>%
  { if (!"N_Offers_Requiring_Tech_Nice_to_Have" %in% names(.)) mutate(., N_Offers_Requiring_Tech_Nice_to_Have = 0L) else . } %>%
  { if (!"N_Mentions_Tech_Mandatory"            %in% names(.)) mutate(., N_Mentions_Tech_Mandatory            = 0L) else . } %>%
  { if (!"N_Mentions_Tech_Nice_to_Have"         %in% names(.)) mutate(., N_Mentions_Tech_Nice_to_Have         = 0L) else . } %>%
  rename(
    Tech_Mandatory        = N_Offers_Requiring_Tech_Mandatory,
    Tech_Nice_to_Have     = N_Offers_Requiring_Tech_Nice_to_Have,
    Mentions_Mandatory    = N_Mentions_Tech_Mandatory,
    Mentions_Nice_to_Have = N_Mentions_Tech_Nice_to_Have
  ) %>%
  mutate(
    N_Offers_Total       = Tech_Mandatory + Tech_Nice_to_Have,
    N_Mentions_Total     = Mentions_Mandatory + Mentions_Nice_to_Have,
    Pct_Of_All_Mandatory = round(100 * Tech_Mandatory    / n_total_offers, 1),
    Pct_Of_All_Optional  = round(100 * Tech_Nice_to_Have / n_total_offers, 1)
  ) %>%
  arrange(desc(N_Offers_Total))

# 12d. Bucket summary -- Standard vs Gap, includes both text and tech rows
all_competences_with_bucket <- competence_with_row %>%
  filter(!is.na(Competence_Name), !is.na(Final_Bucket))

bucket_summary <- all_competences_with_bucket %>%
  group_by(Final_Bucket, Status) %>%
  summarise(
    N_Competences      = n_distinct(Competence_Name),
    N_Offers_Requiring = n_distinct(Row_Id),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from   = Status,
    values_from  = c(N_Competences, N_Offers_Requiring),
    values_fill  = 0,
    names_expand = TRUE
  ) %>%
  mutate(
    N_Competences_Standard      = if ("N_Competences_Standard"      %in% names(.)) N_Competences_Standard      else 0L,
    N_Competences_Gap           = if ("N_Competences_Gap"           %in% names(.)) N_Competences_Gap           else 0L,
    N_Offers_Requiring_Standard = if ("N_Offers_Requiring_Standard" %in% names(.)) N_Offers_Requiring_Standard else 0L,
    N_Offers_Requiring_Gap      = if ("N_Offers_Requiring_Gap"      %in% names(.)) N_Offers_Requiring_Gap      else 0L,
    Total_Competences      = N_Competences_Standard + N_Competences_Gap,
    Total_Offers_Requiring = N_Offers_Requiring_Standard + N_Offers_Requiring_Gap,
    Pct_Standard = round(100 * N_Offers_Requiring_Standard / pmax(Total_Offers_Requiring, 1), 1),
    Pct_Gap      = round(100 * N_Offers_Requiring_Gap      / pmax(Total_Offers_Requiring, 1), 1)
  ) %>%
  arrange(desc(Total_Offers_Requiring))

write_excel_csv(market_demand_by_competence, "market_demand_by_competence.csv")
write_excel_csv(market_demand_by_keu_code,   "market_demand_by_keu_code.csv")
write_excel_csv(tech_demand,                 "tech_demand.csv")
write_excel_csv(bucket_summary,              "bucket_summary.csv")

message("Saved market_demand_by_competence.csv (", nrow(market_demand_by_competence), " competences)")
message("Saved market_demand_by_keu_code.csv (", nrow(market_demand_by_keu_code), " KEU codes)")
message("Saved tech_demand.csv (", nrow(tech_demand), " technologies)")
message("Saved bucket_summary.csv (", nrow(bucket_summary), " buckets)")

cat("\n=== TOP 15 MOST REQUIRED COMPETENCES ===\n")
market_demand_by_competence %>% slice_head(n = 15) %>% print(n = 15)

cat("\n=== BUCKET SUMMARY: Standard vs Gap ===\n")
bucket_summary %>% print(n = 20)

cat("\n=== TOP 20 MOST REQUIRED TECHNOLOGIES ===\n")
tech_demand %>% slice_head(n = 20) %>% print(n = 20)

# ------------------------------------------------------------------------------
# 13. Join to syllabus
# ------------------------------------------------------------------------------
if (nrow(master_skill_matrix) > 0) {
  final_analysis_with_courses <- master_skill_matrix %>%
    left_join(pwr_mapping_raw, by = c("Matched_Code" = "KEU_Code"),
              relationship = "many-to-many")
  
  write_excel_csv(final_analysis_with_courses, "final_analysis_with_courses.csv")
  message("Saved final_analysis_with_courses.csv")
  
  View(master_skill_matrix,         title = "Competence Matrix")
  View(final_analysis_with_courses, title = "Report with Courses")
  View(bucket_summary,              title = "Bucket Coverage Summary")
  View(tech_demand,                 title = "Technology Demand")
  
  cat("\n=== COMPETENCE SOURCES ===\n")
  master_skill_matrix %>% count(Source, Status, sort = TRUE) %>% print()
}

# ------------------------------------------------------------------------------
# DIAGNOSTIC -- run after main pipeline to find systematic errors worth fixing
# Only patterns repeated 3+ times across offers are worth a code change.
# Everything below that threshold is statistical noise.
# ------------------------------------------------------------------------------

# 1. Suspicious Standards: Text_Analysis rows matched to a KEU code, but the
#    competence name contains a named system that the guard may have missed.
suspicious_standards <- master_skill_matrix %>%
  filter(Source == "Text_Analysis", Status == "Standard") %>%
  count(Competence_Name, Matched_Code, sort = TRUE) %>%
  filter(n >= 3)

cat("\n=== SUSPICIOUS STANDARDS (3+ offers -- check if named system slipped through) ===\n")
print(suspicious_standards, n = 30)

# 2. High-frequency Gaps in SOFT_INTERPERSONAL: candidates for SOFT_SKILLS_KEU_MAP
soft_gaps_to_review <- master_skill_matrix %>%
  filter(Status == "Gap", Final_Bucket == "SOFT_INTERPERSONAL") %>%
  count(Competence_Name, sort = TRUE) %>%
  filter(n >= 5)

cat("\n=== SOFT GAPS 5+ times (add to SOFT_SKILLS_KEU_MAP?) ===\n")
print(soft_gaps_to_review, n = 20)

# 3. High-frequency Gaps in tech buckets: candidates for EN_PL_BRIDGE or EXTRA_TECH_KEU_MAP
tech_gaps_to_review <- master_skill_matrix %>%
  filter(Status == "Gap",
         Final_Bucket %in% c("PROGRAMMING_WEB", "BIG_DATA_DATABASES",
                             "IT_SERVICES_CLOUD_UX", "AI_ARTIFICIAL_INTELLIGENCE")) %>%
  count(Competence_Name, sort = TRUE) %>%
  filter(n >= 5)

cat("\n=== TECH GAPS 5+ times (add to EN_PL_BRIDGE or EXTRA_TECH_KEU_MAP?) ===\n")
print(tech_gaps_to_review, n = 20)

# 4. Artifact filter check: boilerplate that slipped through NON_COMPETENCE_ARTIFACT_PATTERN
artifacts_to_review <- master_skill_matrix %>%
  filter(str_detect(tolower(Competence_Name),
                    "inne|other duties|other tasks|dorazn|ad.hoc|wynikajace z")) %>%
  count(Competence_Name, sort = TRUE)

cat("\n=== POSSIBLE ARTIFACTS slipping through the filter ===\n")
print(artifacts_to_review, n = 20)