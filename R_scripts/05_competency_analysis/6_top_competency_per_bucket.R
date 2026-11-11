# ==============================================================================
# STAGE 7: Top 6 kompetencji na bucket z konsolidacją synonimów przez AI
#
# Dla każdego z 20 bucketów:
#   1. Wyciąga top ~25 najczęściej wymaganych kompetencji (unikalne oferty)
#   2. Wysyła do AI z prośbą o scalenie synonimów i wybranie top 6 kanonicznych
#   3. Zwraca tabelę: Bucket | Rank | Canonical_Name | N_Offers | Examples
#
# INPUT:  final_competence_matrix_ai_recheck.csv
# OUTPUT: top6_per_bucket.csv  +  View() w RStudio
#
# Liczba callów AI: max 20 (jeden na bucket) -- kilka minut.
# ==============================================================================

library(tidyverse)
library(stringr)
library(httr2)
library(jsonlite)

# ------------------------------------------------------------------------------
# 0. Config
# ------------------------------------------------------------------------------
INPUT_FILE  <- "final_competence_matrix_ai_recheck.csv"
OUTPUT_FILE <- "top6_per_bucket.csv"

LLAMA_URL   <- "http://127.0.0.1:8080/v1/chat/completions"
LLAMA_MODEL <- "local-model"
TIMEOUT_SEC <- 120
MAX_RETRIES <- 3

TOP_CANDIDATES_PER_BUCKET <- 25  # fallback -- używany tylko jeśli adaptive nie zadziała

# Adaptacyjny próg minimalnej liczby ofert per fraza:
# Duże buckety (>2000 fraz) -- próg 5, żeby nie przepełnić kontekstu
# Małe buckety (<500 fraz)  -- próg 2, żeby AI miało wystarczająco danych
MIN_OFFERS_LARGE_BUCKET <- 5   # dla bucketów z >2000 unikalnych fraz
MIN_OFFERS_SMALL_BUCKET <- 2   # dla bucketów z <=2000 unikalnych fraz
LARGE_BUCKET_THRESHOLD  <- 2000
TOP_N_FINAL               <- 6   # ile kanonicznych zwrócić

# Pomijamy buckety które są tylko technologiami ze scrapeowanych pól --
# te już mamy w tech_demand.csv i nie potrzebujemy AI do ich grupowania.
SKIP_SOURCES <- c("Tech_Mandatory", "Tech_Nice_to_Have")

# ------------------------------------------------------------------------------
# 1. Wczytaj dane
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Brak pliku: ", INPUT_FILE)

df <- read_csv(INPUT_FILE, show_col_types = FALSE)
message("Wczytano: ", nrow(df), " wierszy")

# Tylko Text_Analysis (nie tech fields) i tylko Gap/Standard z nazwą
df_text <- df %>%
  filter(
    !is.na(Competence_Name),
    !is.na(Final_Bucket),
    Source == "Text_Analysis"
  )

message("Wiersze Text_Analysis: ", nrow(df_text))

# Unikalne oferty na kompetencję × bucket
# Normalizacja: małe litery + trim żeby "Analytical Skills" i "Analytical skills"
# nie były liczone jako dwie osobne frazy. Zachowujemy oryginalną formę (pierwszą
# napotkaną) jako etykietę do wyświetlenia.
competence_counts <- df_text %>%
  mutate(
    Competence_Name_norm = Competence_Name %>%
      str_trim() %>%
      str_squish() %>%                               # usuwa podwójne spacje wewnątrz
      tolower() %>%
      str_replace_all("[\\s\\p{Z}\\p{C}]+", " ") # usuwa niewidzialne znaki Unicode
  ) %>%
  group_by(Final_Bucket, Competence_Name_norm, Status) %>%
  summarise(
    N_Offers        = n_distinct(Job_offer_URL),
    Competence_Name = first(Competence_Name),  # zachowaj pierwszą oryginalną formę
    .groups = "drop"
  ) %>%
  select(Final_Bucket, Competence_Name, Competence_Name_norm, Status, N_Offers) %>%
  arrange(Final_Bucket, desc(N_Offers))

# Diagnostyka -- pokaż ile unikalnych (znorm.) fraz ma każdy bucket
message("Unikalne znormalizowane frazy per bucket:")
competence_counts %>%
  count(Final_Bucket, name = "n_unique_phrases") %>%
  arrange(desc(n_unique_phrases)) %>%
  print(n = 25)

buckets_present <- sort(unique(competence_counts$Final_Bucket))
message("Liczba bucketów do przetworzenia: ", length(buckets_present))

# ------------------------------------------------------------------------------
# 2. Funkcje pomocnicze
# ------------------------------------------------------------------------------
call_llama <- function(system_prompt, user_prompt,
                       url       = LLAMA_URL,
                       model     = LLAMA_MODEL,
                       timeout   = TIMEOUT_SEC,
                       max_retry = MAX_RETRIES) {
  body <- list(
    model    = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    ),
    temperature = 0,
    max_tokens  = 4096
  )
  for (attempt in seq_len(max_retry)) {
    result <- tryCatch({
      resp <- request(url) %>%
        req_headers("Content-Type" = "application/json") %>%
        req_body_json(body) %>%
        req_timeout(timeout) %>%
        req_perform()
      content <- resp_body_json(resp)$choices[[1]]$message$content
      if (is.null(content) || nchar(trimws(content)) == 0) stop("Pusta odpowiedz")
      content
    }, error = function(e) {
      message("  BLAD proba ", attempt, "/", max_retry, ": ", conditionMessage(e))
      Sys.sleep(2 ^ (attempt - 1))
      NULL
    })
    if (!is.null(result)) return(result)
  }
  ""
}

parse_top6_response <- function(raw, bucket_name) {
  if (is.null(raw) || nchar(trimws(raw)) == 0) return(tibble())
  
  # Usuń markdown code fences jeśli są
  cleaned <- str_replace_all(raw, "```(?:json)?\\s*|```", "")
  cleaned <- str_trim(cleaned)
  
  # Znajdź JSON array
  start <- str_locate(cleaned, "\\[")[1, "start"]
  ends  <- str_locate_all(cleaned, "\\]")[[1]]
  if (is.na(start) || nrow(ends) == 0) {
    message("  WARN: brak JSON array dla bucketa ", bucket_name)
    message("  RAW (pierwsze 300 znakow): ", str_sub(cleaned, 1, 300))
    return(tibble())
  }
  end_pos <- ends[nrow(ends), "end"]
  json_str <- str_sub(cleaned, start, end_pos)
  
  tryCatch({
    parsed <- fromJSON(json_str, simplifyVector = TRUE)
    if (is.data.frame(parsed)) {
      as_tibble(parsed) %>%
        mutate(Final_Bucket = bucket_name)
    } else if (is.list(parsed)) {
      bind_rows(parsed) %>%
        mutate(Final_Bucket = bucket_name)
    } else {
      tibble()
    }
  }, error = function(e) {
    message("  WARN: blad parsowania JSON dla ", bucket_name, ": ", conditionMessage(e))
    tibble()
  })
}

# ------------------------------------------------------------------------------
# 3. System prompt
# ------------------------------------------------------------------------------
SYSTEM_PROMPT <- "You are a competency analyst. Return ONLY a valid JSON array, no markdown, no explanation, no extra text. If you cannot comply, return []."

make_user_prompt <- function(bucket_name, candidates_df, top_n) {
  candidates_str <- candidates_df %>%
    mutate(line = paste0('  {"name": "', Competence_Name, '", "n_offers": ', N_Offers,
                         ', "status": "', Status, '"}')) %>%
    pull(line) %>%
    paste(collapse = "\n")
  
  paste0(
    'You are analysing job market data for a Polish university curriculum gap study.\n',
    'Bucket: "', bucket_name, '"\n\n',
    'Below are competency names extracted from Polish job ads in this bucket, with how many unique job offers require each.\n',
    'Many names are paraphrases or synonyms of the same underlying competency.\n\n',
    'Your task:\n',
    '1. Group ONLY true synonyms and near-identical paraphrases together (e.g. "Project Management" and "Zarządzanie projektami" are the same; "Risk Management" and "Change Management" are NOT the same).\n',
    '2. Do NOT create umbrella/generic groups. Each canonical name must be specific and actionable, not a vague category. BAD example: "Analytical Skills" (too vague). GOOD example: "Data Analysis and Reporting" or "Financial Analysis".\n',
    '3. For each group, choose the most representative canonical English name -- specific, professional, describing a concrete skill.\n',
    '4. Sum the n_offers across all names in the group to get the group total.\n',
    '5. Return exactly ', top_n, ' groups ranked by total n_offers (highest first).\n',
    '6. For each group also list up to 3 example original names (the "variants" field).\n\n',
    'Return ONLY a JSON array of exactly ', top_n, ' objects with these fields:\n',
    '  rank (integer 1-', top_n, '),\n',
    '  canonical_name (string -- specific, not vague),\n',
    '  n_offers_total (integer -- sum across all grouped names),\n',
    '  variants (array of up to 3 original name strings from the input)\n\n',
    'Candidates:\n',
    candidates_str, '\n\n',
    'JSON array:'
  )
}

# ------------------------------------------------------------------------------
# 4. Główna pętla -- jeden call AI na bucket
# ------------------------------------------------------------------------------
results_list <- list()

for (bucket in buckets_present) {
  message("\n--- Bucket: ", bucket, " ---")
  
  # Adaptacyjny próg -- duże buckety filtrują agresywniej
  n_total_phrases <- competence_counts %>% filter(Final_Bucket == bucket) %>% nrow()
  min_offers_threshold <- if_else(
    n_total_phrases > LARGE_BUCKET_THRESHOLD,
    MIN_OFFERS_LARGE_BUCKET,
    MIN_OFFERS_SMALL_BUCKET
  )
  
  candidates <- competence_counts %>%
    filter(Final_Bucket == bucket, N_Offers >= min_offers_threshold) %>%
    arrange(desc(N_Offers))
  
  n_cands <- nrow(candidates)
  message("  Wszystkich fraz: ", n_total_phrases,
          " | prog min_offers: ", min_offers_threshold,
          " | po filtrze: ", n_cands,
          " | top offer count: ", if (n_cands > 0) max(candidates$N_Offers) else 0,
          " | min offer count: ", if (n_cands > 0) min(candidates$N_Offers) else 0)
  
  if (n_cands == 0) {
    message("  Pomijam -- brak danych")
    next
  }
  
  # Jeśli mniej kandydatów niż TOP_N_FINAL, zwróć bezpośrednio bez AI
  if (n_cands <= TOP_N_FINAL) {
    message("  Za malo kandydatow, zwracam bezposrednio bez AI")
    direct <- candidates %>%
      slice_head(n = TOP_N_FINAL) %>%
      mutate(
        rank           = row_number(),
        canonical_name = Competence_Name,
        n_offers_total = N_Offers,
        variants       = Competence_Name,
        Final_Bucket   = bucket
      ) %>%
      select(rank, canonical_name, n_offers_total, variants, Final_Bucket)
    results_list[[bucket]] <- direct
    next
  }
  
  user_prompt <- make_user_prompt(bucket, candidates, TOP_N_FINAL)
  message("  Wysylam do AI (~", round(nchar(user_prompt) / 4), " tokenow)...")
  
  raw <- call_llama(SYSTEM_PROMPT, user_prompt)
  
  if (nchar(trimws(raw)) == 0) {
    message("  FAIL: pusta odpowiedz -- zapisuje top ", TOP_N_FINAL, " bez konsolidacji")
    fallback <- candidates %>%
      slice_head(n = TOP_N_FINAL) %>%
      mutate(
        rank           = row_number(),
        canonical_name = Competence_Name,
        n_offers_total = N_Offers,
        variants       = Competence_Name,
        Final_Bucket   = bucket
      ) %>%
      select(rank, canonical_name, n_offers_total, variants, Final_Bucket)
    results_list[[bucket]] <- fallback
    next
  }
  
  parsed <- parse_top6_response(raw, bucket)
  
  if (nrow(parsed) == 0) {
    message("  FAIL: nie udalo sie sparsowac -- fallback bez konsolidacji")
    fallback <- candidates %>%
      slice_head(n = TOP_N_FINAL) %>%
      mutate(
        rank           = row_number(),
        canonical_name = Competence_Name,
        n_offers_total = N_Offers,
        variants       = Competence_Name,
        Final_Bucket   = bucket
      ) %>%
      select(rank, canonical_name, n_offers_total, variants, Final_Bucket)
    results_list[[bucket]] <- fallback
    next
  }
  
  # Normalizuj kolumny
  parsed <- parsed %>%
    rename_with(tolower) %>%
    rename_with(~ str_replace_all(.x, "\\s+", "_"))
  
  if (!"rank"           %in% names(parsed)) parsed$rank           <- seq_len(nrow(parsed))
  if (!"canonical_name" %in% names(parsed)) parsed$canonical_name <- NA_character_
  if (!"n_offers_total" %in% names(parsed)) parsed$n_offers_total <- NA_integer_
  if (!"variants"       %in% names(parsed)) parsed$variants       <- NA_character_
  
  # Spłaszcz variants jeśli to lista
  parsed <- parsed %>%
    mutate(
      variants = map_chr(variants, function(v) {
        if (is.null(v) || length(v) == 0) return(NA_character_)
        if (is.list(v)) v <- unlist(v)
        paste(v[seq_len(min(3, length(v)))], collapse = " | ")
      }),
      rank = as.integer(rank),
      n_offers_total = as.integer(n_offers_total)
    ) %>%
    arrange(rank) %>%
    slice_head(n = TOP_N_FINAL)
  
  message("  OK: ", nrow(parsed), " grup zwroconych")
  results_list[[bucket]] <- parsed
}

# ------------------------------------------------------------------------------
# 5. Złóż wyniki i zapisz
# ------------------------------------------------------------------------------
final_results <- bind_rows(results_list, .id = "bucket_id") %>%
  mutate(Final_Bucket = coalesce(final_bucket, bucket_id)) %>%
  select(Final_Bucket, rank, canonical_name, n_offers_total, variants) %>%
  arrange(Final_Bucket, rank)

write_excel_csv(final_results, OUTPUT_FILE)
message("\nZapisano: ", OUTPUT_FILE, " (", nrow(final_results), " wierszy)")

# ------------------------------------------------------------------------------
# 6. Wyświetl podsumowanie
# ------------------------------------------------------------------------------
cat("\n=== TOP 6 KOMPETENCJI NA BUCKET ===\n\n")
for (bucket in buckets_present) {
  cat("--- ", bucket, " ---\n")
  sub <- final_results %>% filter(Final_Bucket == bucket)
  if (nrow(sub) == 0) {
    cat("  (brak danych)\n\n")
    next
  }
  sub %>%
    select(rank, canonical_name, n_offers_total) %>%
    { cat(format(.), "\n") }
  cat("\n")
}

View(final_results, title = "Top 6 kompetencji per bucket")

# Dodatkowy widok: pivot szeroki dla łatwiejszego czytania
wide_view <- final_results %>%
  select(Final_Bucket, rank, canonical_name, n_offers_total) %>%
  mutate(entry = paste0(canonical_name, " (", n_offers_total, ")")) %>%
  select(Final_Bucket, rank, entry) %>%
  pivot_wider(names_from = rank, values_from = entry,
              names_prefix = "Top_") %>%
  arrange(Final_Bucket)

View(wide_view, title = "Top 6 -- widok szeroki")
write_excel_csv(wide_view, "top6_per_bucket_wide.csv")
message("Zapisano: top6_per_bucket_wide.csv")
