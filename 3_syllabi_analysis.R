library(tidyverse)
library(stringr)
library(xml2)

docx_path <- "inzynieria_zarzadzania_ist_zib_25.docx"
tmp_dir   <- tempdir()
unzip(docx_path, files = "word/document.xml", exdir = tmp_dir)

xml   <- read_xml(file.path(tmp_dir, "word", "document.xml"))
ns    <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

# Helper: get clean text from a node (concatenates all <w:t> runs)
node_text <- function(node) {
  xml_find_all(node, ".//w:t", ns) %>%
    xml_text() %>%
    paste(collapse = "") %>%
    str_squish()
}

# All top-level body children (paragraphs <w:p> and tables <w:tbl>)
body     <- xml_find_first(xml, "//w:body", ns)
children <- xml_children(body)
tags     <- xml_name(children)   # "p" or "tbl"

# ── 1. Extract KEU codes ───────────────────────────────────────────────────────
# KEU tables: 4-column tables where col1 has K1_IZ_* codes.
# Col1 = code (or empty continuation), col2 = description, col3/4 = PRK (ignored).

keu_records <- list()

for (i in which(tags == "tbl")) {
  tbl   <- children[[i]]
  rows  <- xml_find_all(tbl, ".//w:tr", ns)
  cells <- lapply(rows, function(r) xml_find_all(r, ".//w:tc", ns))
  
  # Check if this is a KEU table (has K1_IZ_ in first column)
  col1_texts <- sapply(cells, function(r) if (length(r) >= 1) node_text(r[[1]]) else "")
  if (!any(str_detect(col1_texts, "^K1_IZ_[WUK]\\d{2}$"))) next
  
  current_code  <- NULL
  desc_parts    <- character(0)
  
  for (r in seq_along(rows)) {
    if (length(cells[[r]]) < 2) next
    c1 <- node_text(cells[[r]][[1]])
    c2 <- node_text(cells[[r]][[2]])
    
    if (str_detect(c1, "^K1_IZ_[WUK]\\d{2}$")) {
      # Save previous entry
      if (!is.null(current_code)) {
        keu_records[[current_code]] <- paste(desc_parts, collapse = " ") %>% str_squish()
      }
      current_code <- c1
      desc_parts   <- if (nchar(c2) > 0) c2 else character(0)
    } else if (!is.null(current_code) && nchar(c2) > 0) {
      desc_parts <- c(desc_parts, c2)
    }
  }
  # Save last entry
  if (!is.null(current_code)) {
    keu_records[[current_code]] <- paste(desc_parts, collapse = " ") %>% str_squish()
  }
}

keu_df <- tibble(
  KEU_Code        = names(keu_records),
  KEU_Description = unlist(keu_records),
  KEU_Type        = case_when(
    str_detect(names(keu_records), "_W") ~ "Knowledge",
    str_detect(names(keu_records), "_U") ~ "Skills",
    str_detect(names(keu_records), "_K") ~ "Social Competences"
  )
) %>%
  filter(nchar(KEU_Description) > 5) %>%
  distinct(KEU_Code, .keep_all = TRUE)

write_excel_csv(keu_df, "pwr_keu_effects.csv")
message("KEU saved: ", nrow(keu_df), " codes")

# ── 2. Extract PEU tables + subject names ─────────────────────────────────────
# Document structure per syllabus card:
#    <w:p>  "Bazy danych"              <- subject name
#    <w:p>  "Karta przedmiotu"
#    <w:p>  "Informacje podstawowe"
#    <w:tbl> Karta header             <- contains "Kod przedmiotu"
#    ...
#    <w:tbl> PEU table                <- header: "Efekt przedmiotowy | Treść | Efekt kierunkowy"
#            or continuation table   <- first cell is "PEU_W01" etc.

peu_rows     <- list()
current_subj <- NA_character_

for (i in seq_along(children)) {
  tag <- tags[i]
  el  <- children[[i]]
  
  # ── Paragraph: check for subject name or "Karta przedmiotu" ──────────────
  if (tag == "p") {
    txt <- node_text(el)
    if (nchar(txt) == 0) next
    
    # "Karta przedmiotu" signals that the previous non-empty, non-header paragraph
    # was the subject name - I already caught it in the look-back below.
    # Nothing to do here for now.
    next
  }
  
  # ── Table ─────────────────────────────────────────────────────────────────
  if (tag != "tbl") next
  
  rows  <- xml_find_all(el, ".//w:tr", ns)
  cells <- lapply(rows, function(r) xml_find_all(r, ".//w:tc", ns))
  if (length(rows) == 0) next
  
  # Full table text for quick checks
  full_text <- sapply(cells, function(row_cells)
    paste(sapply(row_cells, node_text), collapse = " ")
  ) %>% paste(collapse = " ")
  
  # Detect Karta header table → extract subject from paragraph before it
  if (str_detect(full_text, "Kod przedmiotu")) {
    # Walk back through siblings to find subject name paragraph
    for (back in seq(i - 1, max(1, i - 8), by = -1)) {
      if (tags[back] != "p") next
      txt <- node_text(children[[back]])
      if (nchar(txt) > 2 &&
          !str_detect(txt, "Karta przedmiotu|Informacje podstawowe")) {
        current_subj <- txt
        break
      }
    }
    next
  }
  
  # Detect PEU table: header row, continuation table or social-competence section
  first_row_text <- paste(sapply(cells[[1]], node_text), collapse = " ")
  
  is_peu_header  <- str_detect(first_row_text, "Efekt przedmiotowy")
  
  is_peu_cont    <- length(cells[[1]]) >= 1 &&
    str_detect(node_text(cells[[1]][[1]]), "^PEU_[WUK]\\d+$")
  
  is_peu_section <- str_detect(first_row_text, "Z zakresu kompetencji") &&
    any(sapply(cells, function(r)
      length(r) >= 1 &&
        str_detect(node_text(r[[1]]), "^PEU_[WUK]\\d+$")
    ))
  
  if (!is_peu_header && !is_peu_cont && !is_peu_section) next
  
  # Extract PEU rows from this table
  for (r in seq_along(rows)) {
    if (length(cells[[r]]) < 3) next
    peu_code <- node_text(cells[[r]][[1]])
    peu_desc <- node_text(cells[[r]][[2]])
    keu_cell <- node_text(cells[[r]][[3]])
    
    if (!str_detect(peu_code, "^PEU_[WUK]\\d+$")) next
    
    keu_refs <- str_extract_all(keu_cell, "K1_IZ_[WUK]\\d{2}")[[1]]
    if (length(keu_refs) == 0) keu_refs <- NA_character_
    
    for (keu in keu_refs) {
      peu_rows <- append(peu_rows, list(tibble(
        KEU_Code        = keu,
        Subject         = current_subj,
        PEU_Code        = peu_code,
        PEU_Description = peu_desc
      )))
    }
  }
}

peu_table <- bind_rows(peu_rows)
message("PEU mappings: ", nrow(peu_table), " | Subjects: ", n_distinct(peu_table$Subject))

# ── 3. Join KEU + PEU, CLEAN OUT UNWANTED COURSES and export ──────────────────

final_raw <- peu_table %>%
  left_join(keu_df, by = "KEU_Code") %>%
  select(KEU_Code, KEU_Type, KEU_Description, Subject, PEU_Code, PEU_Description) %>%
  arrange(KEU_Code, Subject, PEU_Code)

# Filtrowanie odcinające języki oraz wychowanie fizyczne przed zapisem bazy ramy
final <- final_raw %>%
  filter(
    # Usuwa przedmioty zawierające w nazwie "Wychowanie fizyczne"
    !str_detect(Subject, "(?i)Wychowanie fizyczne"),
    
    # Usuwa przedmioty zawierające "Język angielski", "Język niemiecki" lub ogólnie "Język obcy"
    !str_detect(Subject, "(?i)Język (angielski|niemiecki|obcy)"),
    
    # Zabezpieczenie na wypadek angielskich nazw w sylabusie (Language / Physical education)
    !str_detect(Subject, "(?i)(Language|Foreign language|Physical education)")
  )

write_excel_csv(final, "pwr_keu_peu_mapping.csv")
message("Done! Saved: pwr_keu_peu_mapping.csv")

# ── 4. Audit ──────────────────────────────────────────────────────────────────

cat("\n=================================================================\n")
cat("                    DATA QUALITY AUDIT                           \n")
cat("=================================================================\n\n")

cat("1. GENERAL METRICS:\n")
cat("   KEU codes extracted:         ", nrow(keu_df),                "(expected: 56)\n")
cat("   PEU-KEU raw rows (with WF):  ", nrow(final_raw),             "\n")
cat("   PEU-KEU clean rows saved:    ", nrow(final),                 " (Usunięto: ", nrow(final_raw) - nrow(final), " wierszy śmieciowych)\n")
cat("   Unique subjects (courses):   ", n_distinct(final$Subject),   "\n")
cat("   Unique KEU codes in mapping:", n_distinct(final$KEU_Code),  "\n\n")

cat("2. KEU TYPE BREAKDOWN:\n")
keu_df %>% count(KEU_Type) %>% print()
cat("\n")

cat("3. DRILL-DOWN — courses covering K1_IZ_W06 (IT / databases):\n")
final %>%
  filter(KEU_Code == "K1_IZ_W06") %>%
  select(Subject, PEU_Code, PEU_Description) %>%
  print()
cat("\n")

cat("4. TOP 5 COURSES BY NUMBER OF PEU ENTRIES:\n")
final %>% count(Subject, sort = TRUE) %>% slice_head(n = 5) %>% print()
cat("\n")

cat("5. KEU CODES WITH NO MATCHING PEU (should be 0):\n")
missing <- anti_join(keu_df, final, by = "KEU_Code")
if (nrow(missing) == 0) {
  cat("   OK — all KEU codes appear in the mapping.\n")
} else {
  cat("   WARNING — missing:\n"); print(missing$KEU_Code)
}
cat("\n")

cat("6. DIRTY DESCRIPTIONS — contain stray codes (should be 0):\n")
dirty <- final %>% filter(str_detect(PEU_Description, "PEU_|K1_IZ_"))
cat("   Dirty rows:", nrow(dirty), "\n\n")

cat("7. RANDOM SAMPLE ROW:\n")
final %>% sample_n(1) %>% glimpse()

cat("\n=================================================================\n")
cat("AUDIT COMPLETE\n")
cat("=================================================================\n")

View(final)
