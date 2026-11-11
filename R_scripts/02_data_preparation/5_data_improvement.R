# ==============================================================================
# STAGE 6: TARGETED AI RE-CHECK -- AI / API / MS Office competencies only
#
# Problem: AI/API and Excel/Word/PowerPoint are systematically misclassified
# (advanced AI/API as Standard even though they were not taught in the program;
# Office suite as Gap even though they were). Regex rules (Stage 5) only catch
# variants we already know -- new phrasings still come through incorrectly.
#
# Solution: Do NOT run AI on the entire file (90k+ rows -- too expensive
# and slow). Extract from the whole file only the UNIQUE Competence_Name values
# that actually contain AI/API/Excel/Word/PowerPoint/Office (realistically
# a few dozen to a few hundred values, not 90k), send them to AI in batches
# with a very unambiguous prompt, and JOIN the result back to the full file
# by competency name. Every row whose name was not caught by the filter
# remains completely untouched -- zero risk of breaking the rest of the data.
#
# INPUT:   output file from Stage 5 (full, ~90k rows, already after regex
#          corrections) -- final_competence_matrix.csv
# OUTPUT:  final_competence_matrix_ai_recheck.csv (full file, only
#          relevant rows updated) + change report.
#
# FIXES APPLIED IN THIS VERSION (root cause: model occasionally returns
# non-JSON or malformed JSON for harder/longer competency names, especially
# as the run progresses -- "format drift"):
#   1. parse_ai_output() is more forgiving -- it strips markdown code fences,
#      tries to find a JSON ARRAY first, and falls back to wrapping a single
#      JSON OBJECT into an array if no array brackets are found.
#   2. SYSTEM_PROMPT now explicitly forbids markdown/explanations and tells
#      the model what to return if it cannot comply ("[]"), i.e. hard JSON
#      lock instead of a soft instruction.
#   3. Failed calls get ONE retry with an extra, more forceful reminder
#      appended to the user prompt before giving up.
#   4. On failure (after the retry), the raw model output is logged via
#      message() (truncated) so you can see immediately whether it was a
#      JSON break, an HTTP/timeout issue, or the model "talking" instead of
#      classifying.
# ==============================================================================

library(tidyverse)
library(stringr)
library(httr2)
library(jsonlite)


# ------------------------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------------------------

INPUT_FILE            <- "final_competence_matrix.csv"   # full file, Stage 5 output
SEMANTIC_BUCKETS_FILE <- "keu_semantic_buckets.csv"
OUTPUT_FILE           <- "final_competence_matrix_ai_recheck.csv"
CHANGES_LOG_FILE      <- "ai_recheck_changes_log.csv"

LLAMA_URL       <- "http://127.0.0.1:8080/v1/chat/completions"
LLAMA_MODEL     <- "local-model"
TIMEOUT_SEC     <- 90
MAX_RETRIES     <- 3
TOP_N_KEU_CANDIDATES <- 3   # KEU candidates per competency name (set here, also used below)

# How many leading characters of a failed raw response to print for debugging.
DEBUG_RAW_OUTPUT_CHARS <- 400

FINAL_BUCKET_VALUES <- c(
  "AI_ARTIFICIAL_INTELLIGENCE", "BIG_DATA_DATABASES", "DATA_ANALYSIS_STATS",
  "IT_SERVICES_CLOUD_UX", "PROGRAMMING_WEB", "MANAGEMENT_INFORMATION_SYSTEMS",
  "PROCESS_OPTIMIZATION", "PRODUCTION_LOGISTICS", "QUALITY_MANAGEMENT",
  "ERGONOMICS_WORKPLACE", "PROJECT_MANAGEMENT", "INNOVATION_ENTREPRENEURSHIP",
  "FINANCE_ACCOUNTING", "LEGAL_ECONOMY_ESG", "E_COMMERCE_MARKETING",
  "LEADERSHIP_DECISION_MAKING", "STAKEHOLDER_CLIENT_RELATIONS", "HR_RECRUITMENT",
  "SOFT_INTERPERSONAL", "OTHER_UNCLASSIFIED"
)

if (!file.exists(INPUT_FILE))            stop("Missing file: ", INPUT_FILE)
if (!file.exists(SEMANTIC_BUCKETS_FILE)) stop("Missing file: ", SEMANTIC_BUCKETS_FILE)

df_full <- read_csv(INPUT_FILE, show_col_types = FALSE)
message("Loaded full file: ", nrow(df_full), " rows")

semantic_buckets <- read_csv(SEMANTIC_BUCKETS_FILE, show_col_types = FALSE)

pwr_framework_clean <- semantic_buckets %>%
  filter(!is.na(KEU_Code), !is.na(KEU_Summary), KEU_Summary != "") %>%
  select(KEU_Code, KEU_Summary) %>%
  distinct(KEU_Code, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# TOP-N KEU CANDIDATE SELECTION (identical logic to Stage 4)
# Instead of putting ALL KEU codes in the system prompt (too long for local
# model), we compute TF-IDF similarity per competency name and pass only the
# TOP_N_KEU_CANDIDATES most relevant KEU codes in each user prompt.
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

# EN -> PL bridge so English competency names hit Polish KEU summaries
EN_PL_BRIDGE <- list(
  "excel"        = c("ms excel", "excel", "arkusz"),
  "word"         = c("edytor", "tekstu", "office"),
  "powerpoint"   = c("prezentacje", "office"),
  "office"       = c("pakiet", "biurowy", "office", "ms excel"),
  "ms office"    = c("pakiet", "biurowy", "ms excel"),
  "artificial intelligence" = c("sztuczna", "inteligencja", "ai", "uczenie"),
  "machine learning" = c("uczenie", "maszynowe", "algorytmy"),
  "api"          = c("interfejs", "integracja", "systemow"),
  "llm"          = c("sztuczna", "inteligencja", "generatywna"),
  "generative ai"= c("sztuczna", "inteligencja", "generatywna"),
  "prompt engineering" = c("sztuczna", "inteligencja", "ai"),
  "rest api"     = c("integracja", "systemow", "interfejs")
)

translate_en_signals <- function(text) {
  text_lower <- tolower(text)
  hits <- character(0)
  for (phrase in names(EN_PL_BRIDGE)) {
    if (str_detect(text_lower, fixed(phrase))) hits <- c(hits, EN_PL_BRIDGE[[phrase]])
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

get_top_keu_candidates <- function(text, top_n = TOP_N_KEU_CANDIDATES) {
  text_tokens <- c(tokenize(text), translate_en_signals(text)) %>% unique()
  if (length(text_tokens) == 0) return(pwr_framework_clean %>% slice(0))
  scores <- map_dbl(names(keu_tokens_list), function(code) {
    keu_toks <- keu_tokens_list[[code]]
    shared   <- intersect(text_tokens, keu_toks)
    if (length(shared) == 0) return(0)
    sum(map_dbl(shared, token_idf)) / sqrt(length(unique(keu_toks)))
  })
  names(scores) <- names(keu_tokens_list)
  ord       <- order(scores, decreasing = TRUE)
  ord       <- ord[scores[ord] > 0]
  top_codes <- names(keu_tokens_list)[ord][seq_len(min(top_n, length(ord)))]
  pwr_framework_clean %>% filter(KEU_Code %in% top_codes)
}

# ------------------------------------------------------------------------------
# 1. Filter: which Competence_Name values relate to AI / API / Office suite
#    (word-boundary, case-insensitive -- better to cast a slightly wider net
#    than to miss variants; AI will decide anyway, a false hit only wastes
#    one extra request, it won't corrupt data)
# ------------------------------------------------------------------------------
TARGET_PATTERN <- regex(
  paste(
    "\\bai\\b",
    "\\bapi\\b",
    "artificial intelligence",
    "sztuczn\\w* intelig",
    
    "chatgpt",
    "\\bgpt[- ]?4\\b",
    "\\bgpt\\b",
    "copilot",
    "github copilot",
    "microsoft copilot",
    "gemini",
    "claude",
    
    "\\bllm\\b",
    "large language model",
    "generative ai",
    
    "prompt",
    "prompting",
    "prompt engineering",
    
    "\\bexcel\\b",
    "\\bword\\b",
    "\\bpowerpoint\\b",
    "\\boffice\\b",
    "pakiet biurow",
    "ms office",
    
    sep = "|"
  ),
  ignore_case = TRUE
)

target_rows <- df_full %>%
  filter(!is.na(Competence_Name), str_detect(Competence_Name, TARGET_PATTERN))

message("Rows related to AI/API/Office: ", nrow(target_rows),
        " (out of ", nrow(df_full), " total)")

unique_names <- target_rows %>%
  distinct(Competence_Name) %>%
  pull(Competence_Name)

message("Unique competency names to re-evaluate by AI: ", length(unique_names))

if (length(unique_names) == 0) {
  message("Nothing to check -- no AI/API/Office rows found. Exiting.")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------------------------
# 2. AI call -- identical pattern to Stage 3/4 (httr2 -> local model)
# ------------------------------------------------------------------------------
call_llama <- function(system_prompt, user_prompt,
                       url = LLAMA_URL, model = LLAMA_MODEL,
                       timeout = TIMEOUT_SEC, max_retry = MAX_RETRIES) {
  body <- list(
    model    = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    ),
    temperature = 0,
    max_tokens  = 3072
  )
  for (attempt in seq_len(max_retry)) {
    result <- tryCatch({
      resp <- request(url) %>%
        req_headers("Content-Type" = "application/json") %>%
        req_body_json(body) %>%
        req_timeout(timeout) %>%
        req_perform()
      resp_body_json(resp)$choices[[1]]$message$content
    }, error = function(e) NULL)
    if (!is.null(result)) return(result)
    Sys.sleep(2 ^ (attempt - 1))
  }
  ""
}

# ------------------------------------------------------------------------------
# FIX 1: more forgiving JSON extraction.
# - strips ```json / ``` markdown fences if the model wrapped its output
# - tries to locate a JSON ARRAY ([...]) first (this is what we ask for)
# - if no array brackets are found, falls back to locating a single JSON
#   OBJECT ({...}) and wraps it in an array, since the model sometimes
#   returns one bare object instead of an array of one object
# ------------------------------------------------------------------------------
parse_ai_output <- function(ai_output) {
  if (is.null(ai_output) || nchar(trimws(ai_output)) == 0) return(tibble())
  
  cleaned_input <- ai_output %>%
    str_remove_all("```json") %>%
    str_remove_all("```") %>%
    str_trim()
  
  try_parse <- function(json_text) {
    tryCatch({
      parsed <- fromJSON(json_text, simplifyVector = TRUE)
      if (is.list(parsed) && !is.data.frame(parsed) && length(parsed) == 1) parsed <- parsed[[1]]
      if (is.list(parsed) && !is.data.frame(parsed)) parsed <- bind_rows(parsed)
      as_tibble(parsed)
    }, error = function(e) NULL)
  }
  
  # Attempt 1: JSON array
  start   <- str_locate(cleaned_input, "\\[")[1, "start"]
  end_all <- str_locate_all(cleaned_input, "\\]")[[1]]
  if (!is.na(start) && nrow(end_all) > 0) {
    end_pos <- end_all[nrow(end_all), "end"]
    result  <- try_parse(str_sub(cleaned_input, start, end_pos))
    if (!is.null(result) && nrow(result) > 0) return(result)
  }
  
  # Attempt 2: bare JSON object -- wrap it into an array
  start_obj   <- str_locate(cleaned_input, "\\{")[1, "start"]
  end_obj_all <- str_locate_all(cleaned_input, "\\}")[[1]]
  if (!is.na(start_obj) && nrow(end_obj_all) > 0) {
    end_obj_pos <- end_obj_all[nrow(end_obj_all), "end"]
    obj_text    <- str_sub(cleaned_input, start_obj, end_obj_pos)
    result      <- try_parse(paste0("[", obj_text, "]"))
    if (!is.null(result) && nrow(result) > 0) return(result)
  }
  
  tibble()
}

# ------------------------------------------------------------------------------
# FIX 2: SYSTEM PROMPT -- hard JSON lock instead of a soft instruction.
# Explicitly forbids markdown/explanations and tells the model exactly what
# to return if it cannot comply, instead of leaving that case undefined.
# ------------------------------------------------------------------------------
SYSTEM_PROMPT <- paste0(
  "You are an expert classifying job market competencies against a study programme (PWr, Management/Informatics).\n\n",
  "RULE 1 -- AI / API:\n",
  "Using AI tools (AI, ChatGPT, Copilot, Gemini, Claude, prompting, prompt engineering, generative AI tools, AI-assisted work) = ALWAYS Standard (normally K1_IZ_U16).\n",
  "Only competencies involving implementation, integration or development of AI systems (REST API, API integration, Graph API, OpenAPI, webhooks, AI agents, LangChain, LlamaIndex, RAG, vector databases, embeddings, fine-tuning, MCP, function calling, MLOps, model deployment) = ALWAYS Gap / competency_gap.\n\n",
  "RULE 2 -- MS OFFICE SUITE: Excel, Word, PowerPoint, Outlook, MS Office (any phrasing) = ",
  "ALWAYS Standard, code K1_IZ_U16, bucket MANAGEMENT_INFORMATION_SYSTEMS. ",
  "Exception: 'Office 365 Azure AD administration' is IT admin, not office suite -- evaluate by content.\n\n",
  "Valid Final_Bucket values: ", paste(FINAL_BUCKET_VALUES, collapse = ", "), ".\n\n",
  "=== OUTPUT FORMAT -- STRICT, NON-NEGOTIABLE ===\n",
  "Return ONLY a JSON array with exactly ONE object. Nothing else.\n",
  "The JSON MUST be complete and closed.\n",
  "If you cannot finish, DO NOT start output.\n",
  "DO NOT add any explanation, comment, or reasoning before or after the JSON.\n",
  "DO NOT wrap the JSON in markdown code fences (no ```json, no ```).\n",
  "DO NOT answer in plain prose under any circumstance.\n",
  "If you cannot confidently classify the competency, still return your best guess in the ",
  "exact format below -- never return free text. If you truly cannot produce valid JSON, ",
  "return exactly: []\n\n",
  "Required format:\n",
  "[{\"Competence_Name\":\"...\",",
  "\"Matched_Code\":\"KEU code or competency_gap\",",
  "\"Status\":\"Standard or Gap\",",
  "\"Final_Bucket\":\"...\"}]"
)

# Build user prompt for a SINGLE competency name with its TOP-N KEU candidates
build_user_prompt_single <- function(competence_name, strict_reminder = FALSE) {
  candidates <- get_top_keu_candidates(competence_name, top_n = TOP_N_KEU_CANDIDATES)
  if (nrow(candidates) == 0) {
    candidates_text <- "No candidates found -- use competency_gap."
  } else {
    candidates_text <- candidates %>%
      mutate(line = paste0(KEU_Code, ": ", KEU_Summary)) %>%
      pull(line) %>%
      paste(collapse = "\n")
  }
  prompt <- paste0(
    "Evaluate this competency. Apply RULE 1 and RULE 2 first (they override everything).\n",
    "If neither rule applies, map to one of the KEU candidates below or return competency_gap.\n\n",
    "Competency: \"", competence_name, "\"\n\n",
    "KEU candidates (TOP ", TOP_N_KEU_CANDIDATES, " by relevance):\n", candidates_text
  )
  if (strict_reminder) {
    prompt <- paste0(
      prompt, "\n\n",
      "REMINDER: your previous response could not be parsed as JSON. ",
      "Respond with ONLY the JSON array described in the system prompt -- ",
      "no markdown, no explanation, no extra text of any kind."
    )
  }
  prompt
}

message("Names to evaluate: ", length(unique_names),
        " (one AI call per name, TOP ", TOP_N_KEU_CANDIDATES, " KEU candidates each)")

# ------------------------------------------------------------------------------
# FIX 3 + FIX 4: one retry with a stricter reminder on parse failure, and
# logging of the raw (truncated) response when both attempts fail, so you can
# immediately see whether it was a JSON break, a timeout/HTTP issue, or the
# model "talking" instead of classifying.
# ------------------------------------------------------------------------------
ai_results <- map_dfr(seq_along(unique_names), function(i) {
  name <- unique_names[[i]]
  if (i %% 50 == 1) message("  Processing ", i, "/", length(unique_names), " ...")
  
  out    <- call_llama(SYSTEM_PROMPT, build_user_prompt_single(name))
  parsed <- parse_ai_output(out)
  
  if (nrow(parsed) == 0) {
    message("    Retry (strict reminder) for '", name, "' -- first response unparseable.")
    out_retry <- call_llama(SYSTEM_PROMPT, build_user_prompt_single(name, strict_reminder = TRUE))
    parsed    <- parse_ai_output(out_retry)
    if (nrow(parsed) == 0) {
      raw_preview <- str_sub(if (nchar(out_retry) > 0) out_retry else out, 1, DEBUG_RAW_OUTPUT_CHARS)
      message("    WARNING: empty/unparseable response for '", name, "' -- skipping.")
      message("    RAW OUTPUT (first ", DEBUG_RAW_OUTPUT_CHARS, " chars): ", raw_preview)
      return(tibble())
    }
  }
  
  for (col in c("Competence_Name", "Matched_Code", "Status", "Final_Bucket")) {
    if (!col %in% names(parsed)) parsed[[col]] <- NA_character_
  }
  parsed %>% slice(1) %>%
    mutate(Competence_Name = name) %>%
    select(Competence_Name, Matched_Code, Status, Final_Bucket)
})

message("AI returned evaluations for ", nrow(ai_results), " / ", length(unique_names), " unique names")

# ------------------------------------------------------------------------------
# 3. Sanitise AI responses + prepare lookup for join
# ------------------------------------------------------------------------------
ai_corrections <- ai_results %>%
  filter(!is.na(Competence_Name), Status %in% c("Standard", "Gap")) %>%
  mutate(
    Final_Bucket = if_else(Final_Bucket %in% FINAL_BUCKET_VALUES, Final_Bucket, "OTHER_UNCLASSIFIED"),
    Matched_Code = if_else(Status == "Gap" | is.na(Matched_Code) | Matched_Code == "",
                           "competency_gap", Matched_Code),
    .name_key = tolower(str_trim(Competence_Name))
  ) %>%
  distinct(.name_key, .keep_all = TRUE)

message("Corrections after sanitisation: ", nrow(ai_corrections))

# ------------------------------------------------------------------------------
# 4. JOIN back to the FULL file -- update ONLY rows whose Competence_Name
#    (case-insensitive) matches a name evaluated by AI.
#    All other rows (those not caught by TARGET_PATTERN, and those caught
#    but for which AI returned no result) remain EXACTLY as they were.
# ------------------------------------------------------------------------------
df_full <- df_full %>%
  mutate(.name_key = tolower(str_trim(Competence_Name)),
         .row_id_tmp = row_number())

df_joined <- df_full %>%
  left_join(
    ai_corrections %>% select(.name_key, .new_code = Matched_Code,
                              .new_status = Status, .new_bucket = Final_Bucket),
    by = ".name_key"
  )

df_out <- df_joined %>%
  mutate(
    .orig_status = Status,
    .orig_code   = Matched_Code,
    .changed     = !is.na(.new_status) & (.new_status != Status | .new_code != Matched_Code),
    Matched_Code = if_else(!is.na(.new_code),   .new_code,   Matched_Code),
    Status       = if_else(!is.na(.new_status), .new_status, Status),
    Final_Bucket = if_else(!is.na(.new_bucket), .new_bucket, Final_Bucket)
  )

n_changed <- sum(df_out$.changed, na.rm = TRUE)
message("\n=== ROWS CHANGED BY AI RE-CHECK: ", n_changed, " / ", nrow(df_out), " ===")

changes_log <- df_out %>%
  filter(.changed) %>%
  distinct(Competence_Name, .orig_status, Status, Matched_Code, Final_Bucket) %>%
  select(Competence_Name, Status_Before = .orig_status, Status_After = Status,
         Code_After = Matched_Code, Bucket_After = Final_Bucket)

df_out <- df_out %>% select(-.name_key, -.row_id_tmp, -.changed, -.orig_status, -.orig_code,
                            -.new_code, -.new_status, -.new_bucket)

write_excel_csv(df_out, OUTPUT_FILE)
write_excel_csv(changes_log, CHANGES_LOG_FILE)

message("\nSaved: ", OUTPUT_FILE, " (", nrow(df_out), " rows, full file)")
message("Saved: ", CHANGES_LOG_FILE, " (", nrow(changes_log), " unique changes for review)")

cat("\n=== SAMPLE CHANGES (for quick verification) ===\n")
changes_log %>% slice_head(n = 30) %>% print(n = 30, width = 120)

cat("\n=== CHANGE DISTRIBUTION: from -> to ===\n")
changes_log %>% count(Status_Before, Status_After, sort = TRUE) %>% print()

# ==============================================================================
# 5. FINAL COUNTS -- identical to Stage 4/5, recalculated on data
#    AFTER AI RE-CHECK (df_out). Requires the same auxiliary files as
#    Stage 4/5: cleaned_data.csv, pwr_keu_peu_mapping.csv,
#    keu_semantic_buckets.csv (already loaded above as semantic_buckets).
# ==============================================================================

pwr_mapping_raw <- read_csv("pwr_keu_peu_mapping.csv", show_col_types = FALSE)
market_data_raw <- read_csv("cleaned_data.csv",        show_col_types = FALSE)

# --- Reconstruct tech_results (exact same logic as Stage 3/5) ---
EXTRA_TECH_KEU_MAP_AGG <- c(
  "r" = "K1_IZ_U16", "css" = "K1_IZ_W12", "html" = "K1_IZ_W12",
  "ml" = "K1_IZ_W15", "wordpress" = "K1_IZ_W12", "sql" = "K1_IZ_W06",
  "microsoft access" = "K1_IZ_W06", "figma" = "K1_IZ_W12",
  "canva" = "K1_IZ_W12", "olap" = "K1_IZ_W06", "bpmn" = "K1_IZ_U16",
  "bmpn" = "K1_IZ_U16", "arena" = "K1_IZ_U16", "vba" = "K1_IZ_U16",
  "tableau" = "K1_IZ_W15", "3ds max" = "K1_IZ_W12",
  "ms project" = "K1_IZ_W11", "ma project" = "K1_IZ_W11",
  "python" = "K1_IZ_U16", "javascript" = "K1_IZ_W12",
  "oracle" = "K1_IZ_W06", "oracle data integrator" = "K1_IZ_W06",
  "microsoft power bi" = "K1_IZ_W15", "git" = "K1_IZ_W12",
  "svn" = "K1_IZ_W12", "azure" = "K1_IZ_W12", "databricks" = "K1_IZ_W06"
)

EXTRA_TECH_BUCKET_MAP_AGG <- c(
  "r" = "PROGRAMMING_WEB", "css" = "PROGRAMMING_WEB", "html" = "PROGRAMMING_WEB",
  "ml" = "AI_ARTIFICIAL_INTELLIGENCE", "wordpress" = "PROGRAMMING_WEB",
  "sql" = "BIG_DATA_DATABASES", "microsoft access" = "BIG_DATA_DATABASES",
  "figma" = "IT_SERVICES_CLOUD_UX", "canva" = "E_COMMERCE_MARKETING",
  "olap" = "BIG_DATA_DATABASES", "bpmn" = "PROCESS_OPTIMIZATION",
  "bmpn" = "PROCESS_OPTIMIZATION", "arena" = "PROCESS_OPTIMIZATION",
  "vba" = "PROGRAMMING_WEB", "tableau" = "DATA_ANALYSIS_STATS",
  "3ds max" = "IT_SERVICES_CLOUD_UX", "ms project" = "PROJECT_MANAGEMENT",
  "ma project" = "PROJECT_MANAGEMENT", "python" = "PROGRAMMING_WEB",
  "javascript" = "PROGRAMMING_WEB", "oracle" = "BIG_DATA_DATABASES",
  "oracle data integrator" = "BIG_DATA_DATABASES",
  "microsoft power bi" = "DATA_ANALYSIS_STATS", "git" = "PROGRAMMING_WEB",
  "svn" = "PROGRAMMING_WEB", "azure" = "IT_SERVICES_CLOUD_UX",
  "databricks" = "BIG_DATA_DATABASES"
)

LANGUAGE_DRIVING_PATTERN_AGG <- regex(
  paste(
    "jezyk (angielski|niemiecki|francuski|hiszpanski|wloski|rosyjski|chinski|obcy)",
    "j\\. angielski", "j\\. niemiecki", "english", "german", "french",
    "prawo jazdy", "kat\\. b", "kategoria b",
    sep = "|"
  ),
  ignore_case = TRUE
)

clean_tech_field_agg <- function(tech_string) {
  if (is.na(tech_string) || tech_string == "" || tech_string == "NA") return(character(0))
  cleaned <- str_replace_all(tech_string, "[\\[\\]\\\"\\']", " ")
  items   <- str_split(cleaned, ",|;|/")[[1]] %>% str_trim()
  items   <- items[nchar(items) > 0]
  items   <- items[!str_detect(items, LANGUAGE_DRIVING_PATTERN_AGG)]
  unique(items)
}

smart_extract_key_phrases_agg <- function(s) {
  if (is.na(s) || s == "") return("")
  phrases <- str_split(s, ";")[[1]] %>% str_trim()
  phrases <- phrases[phrases != ""]
  paste(unique(phrases), collapse = "; ")
}

pwr_framework_agg <- semantic_buckets %>%
  filter(!is.na(KEU_Code), !is.na(KEU_Summary), KEU_Summary != "") %>%
  mutate(KEU_Summary = map_chr(KEU_Summary, smart_extract_key_phrases_agg)) %>%
  select(KEU_Code, KEU_Summary) %>%
  distinct(KEU_Code, .keep_all = TRUE)

tech_keu_dict_agg <- pwr_framework_agg %>%
  mutate(keywords = str_split(tolower(KEU_Summary), ";\\s*")) %>%
  unnest(keywords) %>%
  mutate(keywords = str_trim(keywords)) %>%
  filter(nchar(keywords) > 2) %>%
  select(keyword = keywords, KEU_Code)

lookup_tech_agg <- function(tech_name) {
  tech_lower <- tolower(str_trim(tech_name))
  hit <- tech_keu_dict_agg %>% filter(keyword == tech_lower)
  if (nrow(hit) > 0) return(hit$KEU_Code[[1]])
  hit2 <- tech_keu_dict_agg %>%
    filter(str_detect(keyword, fixed(tech_lower)) | str_detect(tech_lower, fixed(keyword)))
  if (nrow(hit2) > 0) return(hit2$KEU_Code[[1]])
  NA_character_
}

map_tech_list_agg <- function(tech_list, source_label) {
  if (length(tech_list) == 0) return(tibble())
  dict_codes  <- map_chr(tech_list, lookup_tech_agg)
  extra_codes <- map_chr(tech_list, ~ {
    key <- tolower(str_trim(.x))
    hit <- EXTRA_TECH_KEU_MAP_AGG[key]
    if (!is.na(hit)) hit else NA_character_
  })
  final_codes <- if_else(!is.na(dict_codes), dict_codes,
                         if_else(!is.na(extra_codes), extra_codes, NA_character_))
  tech_buckets <- map_chr(tech_list, ~ {
    key <- tolower(str_trim(.x))
    hit <- EXTRA_TECH_BUCKET_MAP_AGG[key]
    if (!is.na(hit)) hit else "OTHER_UNCLASSIFIED"
  })
  tibble(
    Competence_Name = tech_list,
    Matched_Code    = if_else(!is.na(final_codes), final_codes, "competency_gap"),
    Status          = if_else(!is.na(final_codes), "Standard", "Gap"),
    Source          = source_label,
    Final_Bucket    = tech_buckets
  )
}

message("\nReconstructing tech_results from cleaned_data.csv...")
full_batch_agg <- market_data_raw %>% mutate(Row_Id = row_number())

tech_results_agg <- map_dfr(seq_len(nrow(full_batch_agg)), function(i) {
  row         <- full_batch_agg[i, ]
  mand_result <- map_tech_list_agg(clean_tech_field_agg(row$Mandatory_tech),    "Tech_Mandatory")
  nice_result <- map_tech_list_agg(clean_tech_field_agg(row$Nice_to_have_tech), "Tech_Nice_to_Have")
  bind_rows(mand_result, nice_result) %>% mutate(Row_Id = row$Row_Id)
})
message("tech_results: ", nrow(tech_results_agg), " rows")

n_total_offers <- n_distinct(full_batch_agg$Row_Id)

# Combine text (after AI recheck) + tech into one competence_with_row object
text_rows_for_agg <- df_out %>%
  select(Competence_Name, Matched_Code, Status, Source, Final_Bucket, Job_offer_URL) %>%
  left_join(
    full_batch_agg %>% select(Row_Id, Job_offer_URL),
    by = "Job_offer_URL"
  ) %>%
  mutate(
    Matched_Code = if_else(is.na(Matched_Code) | Matched_Code %in% c("", "NA", "competency_gap"),
                           "competency_gap", as.character(Matched_Code)),
    Status       = if_else(Matched_Code == "competency_gap", "Gap", as.character(Status))
  )

tech_rows_for_agg <- tech_results_agg %>%
  left_join(full_batch_agg %>% select(Row_Id, Job_offer_URL), by = "Row_Id") %>%
  mutate(
    Matched_Code = if_else(is.na(Matched_Code) | Matched_Code %in% c("", "NA", "competency_gap"),
                           "competency_gap", as.character(Matched_Code)),
    Status       = if_else(Matched_Code == "competency_gap", "Gap", as.character(Status))
  )

competence_with_row_all <- bind_rows(
  text_rows_for_agg,
  tech_rows_for_agg %>% select(names(text_rows_for_agg))
)

# 5a. By competence -- Text_Analysis only
market_demand_by_competence <- competence_with_row_all %>%
  filter(!is.na(Competence_Name), Source == "Text_Analysis") %>%
  group_by(Competence_Name, Matched_Code, Status, Source, Final_Bucket) %>%
  summarise(N_Offers_Requiring = n_distinct(Row_Id), .groups = "drop") %>%
  mutate(Pct_Of_All_Offers = round(100 * N_Offers_Requiring / n_total_offers, 1)) %>%
  arrange(desc(N_Offers_Requiring))

# 5b. By KEU code -- Text_Analysis only
market_demand_by_keu_code <- competence_with_row_all %>%
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

# 5c. Tech demand
tech_demand <- tech_results_agg %>%
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

# 5d. Bucket summary -- Standard vs Gap, text + tech combined
bucket_summary <- competence_with_row_all %>%
  filter(!is.na(Competence_Name), !is.na(Final_Bucket)) %>%
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

# 5e. Join to syllabus
final_analysis_with_courses <- df_out %>%
  left_join(pwr_mapping_raw, by = c("Matched_Code" = "KEU_Code"),
            relationship = "many-to-many")

# --- Save all final files (same names as Stage 4/5, ready to replace) ---
write_excel_csv(market_demand_by_competence, "market_demand_by_competence_final.csv")
write_excel_csv(market_demand_by_keu_code,   "market_demand_by_keu_code_final.csv")
write_excel_csv(tech_demand,                 "tech_demand_final.csv")
write_excel_csv(bucket_summary,              "bucket_summary_final.csv")
write_excel_csv(final_analysis_with_courses, "final_analysis_with_courses_final.csv")

message("\nSaved: market_demand_by_competence_final.csv (", nrow(market_demand_by_competence), " competencies)")
message("Saved: market_demand_by_keu_code_final.csv (", nrow(market_demand_by_keu_code), " KEU codes)")
message("Saved: tech_demand_final.csv (", nrow(tech_demand), " technologies)")
message("Saved: bucket_summary_final.csv (", nrow(bucket_summary), " buckets)")
message("Saved: final_analysis_with_courses_final.csv")

cat("\n=== TOP 15 MOST REQUIRED COMPETENCES (after AI recheck) ===\n")
market_demand_by_competence %>% slice_head(n = 15) %>% print(n = 15)

cat("\n=== BUCKET SUMMARY: Standard vs Gap (after AI recheck) ===\n")
bucket_summary %>% print(n = 20)

cat("\n=== TOP 20 MOST REQUIRED TECHNOLOGIES ===\n")
tech_demand %>% slice_head(n = 20) %>% print(n = 20)

cat("\n=== COMPETENCE SOURCES ===\n")
df_out %>% count(Source, Status, sort = TRUE) %>% print()


View(market_demand_by_competence)
View(market_demand_by_keu_code)
View(tech_demand)
View(bucket_summary)
View(final_analysis_with_courses)
