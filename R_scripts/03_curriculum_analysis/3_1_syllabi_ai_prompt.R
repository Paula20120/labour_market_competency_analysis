library(tidyverse)
library(httr2)
library(jsonlite)
library(stringr)
library(progressr)

# ==============================================================================
# CEL: Zbudować 56 koszyków KEU, każdy z PEŁNĄ treścią ze wszystkich PEU
# przypisanych do tego kodu — bez utraty ŻADNEGO słowa kluczowego (Excel,
# Python, SQL, itp.), ale w czytelnej, krótkiej formie do użycia w prompcie
# Stage 3.
#
# Dlaczego dwa kroki, nie jeden:
#   Poprzednia wersja prosiła AI o kompresję do "max 35 słów" + "usuń
#   powtórzenia semantyczne" w JEDNYM kroku. To jest ryzykowne: model sam
#   decyduje co jest "ważne", a rzadkie, ale kluczowe narzędzie (np. Excel
#   wspomniany raz w jednym PEU z 12) łatwo ginie jako "szczegół" wobec
#   częstszych, ogólniejszych fraz.
#
#   Tutaj: Krok A (czysty R, ZERO AI) wyciąga mechanicznie WSZYSTKIE unikalne
#   słowa/frazy z każdego PEU — to się nigdy nie zgubi, bo nie ma w tym kroku
#   żadnej decyzji "co jest ważne". Krok B (AI) tylko PORZĄDKUJE i USUWA
#   DOSŁOWNE DUPLIKATY z już kompletnej listy, z explicite zakazem
#   skracania/oceniania ważności.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIG
# ------------------------------------------------------------------------------
LLAMA_URL   <- "http://127.0.0.1:8080/v1/chat/completions"
LLAMA_MODEL <- "local-model"
TIMEOUT_SEC <- 120
MAX_RETRIES <- 2

INPUT_FILE  <- "pwr_keu_peu_mapping.csv"
OUTPUT_FILE <- "keu_semantic_buckets.csv"

# ------------------------------------------------------------------------------
# 1. VALIDATION
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) {
  stop("Brak pliku wejściowego: ", INPUT_FILE)
}

pwr_mapping_raw <- read_csv(INPUT_FILE, show_col_types = FALSE)

required_cols <- c("KEU_Code", "KEU_Type", "KEU_Description", "PEU_Description")
missing_cols  <- setdiff(required_cols, names(pwr_mapping_raw))
if (length(missing_cols) > 0) {
  stop("Brakuje wymaganych kolumn: ", paste(missing_cols, collapse = ", "))
}

# ------------------------------------------------------------------------------
# 2. KROK A — MECHANICZNA EKSTRAKCJA (bez AI, bez ryzyka utraty słów)
# ------------------------------------------------------------------------------
# Dla każdego KEU_Code: zbierz WSZYSTKIE unikalne pełne zdania/frazy z PEU
# (nie pojedyncze słowa — pełne sensowne frazy, żeby AI w kroku B miało
# kontekst, a nie wyrwane z kontekstu tokeny).

message("KROK A: Mechaniczna ekstrakcja pełnej treści PEU (zero strat)...")

# Rozbij każdy PEU_Description na frazy po ".", ";" — zachowując sens,
# nie pojedyncze słowa. To pozwala AI w kroku B widzieć frazy typu
# "Posługuje się językiem SQL", nie izolowane słowo "SQL".
split_into_phrases <- function(text) {
  text %>%
    str_split("(?<=[.;])\\s+") %>%
    pluck(1) %>%
    str_trim() %>%
    .[nchar(.) > 3]
}

keu_input <- pwr_mapping_raw %>%
  filter(!is.na(KEU_Code), !is.na(PEU_Description)) %>%
  mutate(PEU_Description = str_squish(PEU_Description)) %>%
  group_by(KEU_Code, KEU_Type, KEU_Description) %>%
  summarise(
    PEU_Count = n_distinct(PEU_Description),
    # Wszystkie unikalne frazy ze WSZYSTKICH PEU tego kodu, połączone "|"
    # To jest KOMPLETNA lista — nic nie jest tu jeszcze obcinane czy oceniane.
    All_Phrases = paste(unique(unlist(map(unique(PEU_Description), split_into_phrases))), collapse = " | "),
    .groups = "drop"
  )

message("  -> ", nrow(keu_input), " unikalnych kodów KEU.")
message("  -> Średnia długość pełnej listy fraz: ",
        round(mean(nchar(keu_input$All_Phrases))), " znaków (PRZED kompresją AI).")

# ------------------------------------------------------------------------------
# 3. KROK B — PROMPT BUILDER (AI tylko porządkuje, nie ocenia ważności)
# ------------------------------------------------------------------------------
# Kluczowa różnica względem poprzedniej wersji: model NIE ma limitu słów,
# NIE ma instrukcji "skompresuj" czy "usuń szczegóły". Ma tylko jedno zadanie:
# usunąć DOSŁOWNE powtórzenia tej samej informacji i połączyć w czytelną
# listę średnikową, zachowując KAŻDĄ unikalną nazwę narzędzia/metody/pojęcia.

create_summary_prompt <- function(keu_code, keu_type, keu_description, all_phrases) {
  system_prompt <- paste(
    "Jesteś ekstraktorem pojęć technicznych.",
    "Otrzymujesz listę pełnych zdań akademickich opisujących jeden koszyk kompetencji.",
    "Twoje zadanie: wydobyć z nich SAME NAZWY kompetencji, narzędzi, technologii i metod —",
    "NIE przepisywać całych zdań.",
    "Przykład transformacji:",
    "  WEJŚCIE: 'Posługuje się strukturalnym językiem zapytań SQL do pozyskiwania danych z relacyjnej bazy danych.'",
    "  WYJŚCIE: 'SQL; zapytania do baz danych; relacyjne bazy danych'",
    "  WEJŚCIE: 'Analizuje dane w arkuszu kalkulacyjnym Excel, tworzy tabele przestawne i wykresy.'",
    "  WYJŚCIE: 'Excel; tabele przestawne; wykresy; analiza danych'",
    "BEZWZGLĘDNY ZAKAZ: nie usuwaj żadnej unikalnej nazwy narzędzia, technologii, metody",
    "czy konkretnego pojęcia (np. Excel, Python, SQL, SAP, Power BI, R, VBA, Lean, Six Sigma)",
    "— każda musi się znaleźć w wyniku, nawet jeśli wspomniana tylko raz.",
    "Połącz wszystkie wydobyte pojęcia w JEDNĄ listę, usuwając PRAWDZIWE duplikaty",
    "(to samo pojęcie nazwane tak samo lub synonimicznie).",
    "Zwróć wyłącznie czysty JSON, bez komentarzy."
  )
  
  user_prompt <- paste0(
    "KEU_Code: ", keu_code, "\n",
    "KEU_Type: ", keu_type, "\n",
    "KEU_Description: ", keu_description, "\n\n",
    "PEŁNE ZDANIA Z OPISÓW PEU (do przetworzenia na pojęcia-klucze):\n",
    all_phrases, "\n\n",
    "ZADANIE:\n",
    "1. Z KAŻDEGO zdania wydobądź konkretne pojęcia: nazwy narzędzi, technologii, metod,\n",
    "   konkretnych umiejętności (np. 'SQL', 'Excel', 'analiza SWOT', 'zarządzanie projektami',\n",
    "   'modelowanie matematyczne', 'badania operacyjne').\n",
    "2. NIE przepisuj całych zdań — tylko nazwy pojęć/narzędzi/metod wydobyte z ich treści.\n",
    "3. Zachowaj WSZYSTKIE unikalne nazwy narzędzi/technologii — priorytet nr 1.\n",
    "4. Usuń duplikaty znaczeniowe (to samo pojęcie wspomniane kilka razy = jedna fraza w wyniku).\n",
    "5. Połącz w listę fraz rozdzielonych średnikami (;). Każda fraza to 1-4 słowa, nie całe zdanie.\n\n",
    "Zwróć JSON w formacie:\n",
    "{\n",
    "  \"KEU_Summary\": \"pojęcie 1; pojęcie 2; pojęcie 3; ...\"\n",
    "}"
  )
  
  list(system = system_prompt, user = user_prompt)
}

# ------------------------------------------------------------------------------
# 4. LLAMA.CPP CALL
# ------------------------------------------------------------------------------
call_llama <- function(system_prompt, user_prompt) {
  body <- list(
    model = LLAMA_MODEL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ),
    temperature = 0,
    response_format = list(type = "json_object")
  )
  
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    result <- tryCatch({
      resp <- request(LLAMA_URL) %>%
        req_headers("Content-Type" = "application/json") %>%
        req_body_json(body) %>%
        req_timeout(TIMEOUT_SEC) %>%
        req_perform()
      resp_body_json(resp)$choices[[1]]$message$content
    }, error = function(e) {
      message(sprintf("Błąd llama.cpp dla próby %d/%d: %s", attempt, MAX_RETRIES, conditionMessage(e)))
      if (attempt <= MAX_RETRIES) NULL else ""
    })
    if (!is.null(result)) return(result)
    Sys.sleep(2)
  }
}

parse_summary_output <- function(ai_output) {
  if (is.null(ai_output) || ai_output == "") return(NA_character_)
  cleaned <- ai_output %>%
    str_remove_all("^```json\\s*") %>%
    str_remove_all("```\\s*$") %>%
    str_squish()
  tryCatch({
    parsed <- fromJSON(cleaned)
    if (!is.null(parsed$KEU_Summary)) str_squish(parsed$KEU_Summary) else NA_character_
  }, error = function(e) {
    message("Błąd parsowania JSON: ", conditionMessage(e))
    NA_character_
  })
}

# ------------------------------------------------------------------------------
# 5. GENERATE SUMMARIES (krok B — AI tylko porządkuje)
# ------------------------------------------------------------------------------
handlers(global = TRUE)
handlers("txtprogressbar")

with_progress({
  p <- progressor(steps = nrow(keu_input))
  
  keu_summaries <- map_dfr(seq_len(nrow(keu_input)), function(i) {
    row <- keu_input[i, ]
    
    prompts <- create_summary_prompt(
      keu_code        = row$KEU_Code,
      keu_type        = row$KEU_Type,
      keu_description = row$KEU_Description,
      all_phrases     = row$All_Phrases
    )
    
    ai_output    <- call_llama(prompts$system, prompts$user)
    summary_text <- parse_summary_output(ai_output)
    
    p()
    
    tibble(
      KEU_Code         = row$KEU_Code,
      KEU_Type         = row$KEU_Type,
      KEU_Description  = row$KEU_Description,
      PEU_Count        = row$PEU_Count,
      KEU_Summary      = summary_text,
      All_Phrases_Raw  = row$All_Phrases   # zachowane do weryfikacji / fallbacku
    )
  })
})

# ------------------------------------------------------------------------------
# 6. WERYFIKACJA — sprawdź czy AI niczego nie zgubiło
# ------------------------------------------------------------------------------
# Wyciągamy "słowa-narzędzia" (capitalized tokens, akronimy) z oryginału
# i ze streszczenia, i sprawdzamy czy każde z oryginału jest też w streszczeniu.
# To NIE jest idealna walidacja, ale złapie najbardziej oczywiste straty
# (np. zgubione "Excel", "SQL", "SAP").

extract_tool_tokens <- function(text) {
  if (is.na(text)) return(character(0))
  str_extract_all(text, "\\b[A-Z][A-Za-z0-9]{1,15}\\b")[[1]] %>% unique()
}

keu_summaries <- keu_summaries %>%
  mutate(
    tools_original   = map(All_Phrases_Raw, extract_tool_tokens),
    tools_summary    = map(KEU_Summary, extract_tool_tokens),
    tools_lost       = map2(tools_original, tools_summary, ~ setdiff(.x, .y)),
    n_tools_lost     = map_int(tools_lost, length),
    tools_lost_list  = map_chr(tools_lost, ~ paste(.x, collapse = ", "))
  )

n_with_loss <- sum(keu_summaries$n_tools_lost > 0)
message("\n--- WERYFIKACJA UTRATY SŁÓW KLUCZOWYCH ---")
message("Kody z potencjalną utratą narzędzi/akronimów: ", n_with_loss, " / ", nrow(keu_summaries))
if (n_with_loss > 0) {
  message("Sprawdź kolumnę 'tools_lost_list' w wyniku — to kandydaci do ręcznej korekty.")
  keu_summaries %>%
    filter(n_tools_lost > 0) %>%
    select(KEU_Code, tools_lost_list) %>%
    print(n = 60)
}

# Sprawdź realną kompresję: jeśli streszczenie jest >70% długości oryginału,
# to znaczy że AI nie skondensowało, tylko przepisało (jak w poprzedniej wersji)
keu_summaries <- keu_summaries %>%
  mutate(compression_pct = round((1 - nchar(KEU_Summary) / nchar(All_Phrases_Raw)) * 100, 1))

message("\n--- WERYFIKACJA KOMPRESJI (czy AI faktycznie skondensowało) ---")
message("Średnia redukcja długości: ", round(mean(keu_summaries$compression_pct, na.rm = TRUE), 1), "%")
weak_compression <- keu_summaries %>% filter(compression_pct < 40)
if (nrow(weak_compression) > 0) {
  message(nrow(weak_compression), " kodów ma słabą kompresję (<40%) — AI prawdopodobnie przepisało zdania",
          " zamiast wydobyć pojęcia. Sprawdź je ręcznie:")
  weak_compression %>% select(KEU_Code, compression_pct) %>% print(n = 60)
}

# Usuń kolumny pomocnicze (listy) przed zapisem do CSV — CSV nie obsługuje list-columns
keu_summaries_export <- keu_summaries %>%
  select(-tools_original, -tools_summary, -tools_lost)

# ------------------------------------------------------------------------------
# 7. SAVE OUTPUT
# ------------------------------------------------------------------------------
write_excel_csv(keu_summaries_export, OUTPUT_FILE)

message("\nGotowe. Zapisano plik: ", OUTPUT_FILE)
message("Kolumna 'KEU_Summary' = finalny opis koszyka do użycia w prompcie Stage 3.")
message("Kolumna 'tools_lost_list' = ostrzeżenie, jeśli AI mogło coś zgubić — sprawdź te wiersze.")

View(keu_summaries_export)
