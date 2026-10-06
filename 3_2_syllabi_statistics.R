# ==============================================================================
# CHAPTER 3.1: Characteristics of competencies in the IZ1 curriculum
# Input:  pwr_keu_peu_mapping.csv   (output of 3_syllabi_analysis.R)
#         pwr_keu_effects.csv       (KEU codes table)
# Output: outputs/3_1_*.png  +  outputs/3_1_summary_stats.csv
# ==============================================================================

library(tidyverse)
library(scales)
library(ggtext)
library(gridExtra)
library(grid)

dir.create("outputs/3_1", recursive = TRUE, showWarnings = FALSE)

# ── 0. Load data ──────────────────────────────────────────────────────────────
mapping <- read_csv("pwr_keu_peu_mapping.csv", show_col_types = FALSE)
keu_df  <- read_csv("pwr_keu_effects.csv",     show_col_types = FALSE)

# ── 1. Core summary statistics ────────────────────────────────────────────────
n_keu_total   <- n_distinct(keu_df$KEU_Code)
n_keu_mapping <- n_distinct(mapping$KEU_Code)
n_peu_total   <- n_distinct(paste(mapping$Subject, mapping$PEU_Code))  # unique per course
n_courses     <- n_distinct(mapping$Subject)
n_peu_rows    <- nrow(mapping)   # rows = PEU-KEU links (many-to-many)

keu_type_counts <- keu_df %>%
  count(KEU_Type, name = "N_KEU") %>%
  mutate(Pct = round(100 * N_KEU / sum(N_KEU), 1))

peu_per_keu <- mapping %>%
  group_by(KEU_Code, KEU_Type) %>%
  summarise(
    N_PEU     = n_distinct(PEU_Code),
    N_Courses = n_distinct(Subject),
    N_Links   = n(),
    .groups   = "drop"
  )

peu_per_course <- mapping %>%
  group_by(Subject) %>%
  summarise(
    N_PEU      = n_distinct(PEU_Code),
    N_KEU_refs = n_distinct(KEU_Code),
    N_Links    = n(),
    .groups    = "drop"
  ) %>%
  arrange(desc(N_PEU))

summary_stats <- tibble(
  Metric  = c(
    "Total KEU codes (programme level)",
    "KEU codes with ≥1 PEU in mapping",
    "Unique PEU entries (per course)",
    "PEU–KEU links (rows in mapping)",
    "Courses (subjects) in mapping",
    "KEU Knowledge (W) codes",
    "KEU Skills (U) codes",
    "KEU Social Competences (K) codes",
    "Mean PEU links per KEU",
    "Max PEU links per KEU",
    "Min PEU links per KEU",
    "Mean KEU refs per course"
  ),
  Value = c(
    n_keu_total,
    n_keu_mapping,
    n_peu_total,
    n_peu_rows,
    n_courses,
    keu_type_counts$N_KEU[keu_type_counts$KEU_Type == "Knowledge"],
    keu_type_counts$N_KEU[keu_type_counts$KEU_Type == "Skills"],
    keu_type_counts$N_KEU[keu_type_counts$KEU_Type == "Social Competences"],
    round(mean(peu_per_keu$N_Links), 1),
    max(peu_per_keu$N_Links),
    min(peu_per_keu$N_Links),
    round(mean(peu_per_course$N_KEU_refs), 1)
  )
)

write_csv(summary_stats, "outputs/3_1/3_1_summary_stats.csv")
print(summary_stats, n = 20)

# ── COLOUR PALETTE ────────────────────────────────────────────────────────────
COL_W <- "#1D3557"   # Knowledge  – dark navy
COL_U <- "#457B9D"   # Skills     – medium blue
COL_K <- "#A8DADC"   # Social     – light teal
TYPE_COLS <- c(Knowledge = COL_W, Skills = COL_U, `Social Competences` = COL_K)

THEME_BASE <- theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle   = element_text(size = 10, color = "grey40", hjust = 0.5),
    axis.title      = element_text(size = 10),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# ── PLOT 1: KEU type distribution (bar) ───────────────────────────────────────
p1 <- keu_type_counts %>%
  mutate(KEU_Type = factor(KEU_Type,
                           levels = c("Knowledge", "Skills", "Social Competences"))) %>%
  ggplot(aes(KEU_Type, N_KEU, fill = KEU_Type)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(N_KEU, "  (", Pct, "%)")),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = TYPE_COLS) +
  scale_y_continuous(limits = c(0, max(keu_type_counts$N_KEU) * 1.18),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = c(
    "Knowledge" = "Knowledge\n(W codes)",
    "Skills"    = "Skills\n(U codes)",
    "Social Competences" = "Social\nCompetences\n(K codes)"
  )) +
  labs(
    title    = "Distribution of KEU codes by domain",
    subtitle = paste0("IZ1 programme - ", n_keu_total, " learning outcomes in total"),
    x = NULL, y = "Number of KEU codes"
  ) +
  THEME_BASE

ggsave("outputs/3_1/fig_3_1_keu_distribution.png", p1,
       width = 7, height = 5, dpi = 200)
print(p1)

# ── PLOT 2: PEU links per KEU — distribution histogram + type colour ──────────
p2 <- peu_per_keu %>%
  mutate(KEU_Type = factor(KEU_Type,
                           levels = c("Knowledge", "Skills", "Social Competences"))) %>%
  ggplot(aes(N_Links, fill = KEU_Type)) +
  geom_histogram(binwidth = 3, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = TYPE_COLS, name = "Domain") +
  scale_x_continuous(breaks = seq(0, max(peu_per_keu$N_Links) + 5, by = 5)) +
  labs(
    title    = "Distribution of PEU-KEU links per learning outcome code",
    subtitle = paste0("Each bar = number of KEU codes with that many linked PEU entries  |  ",
                      "mean = ", round(mean(peu_per_keu$N_Links), 1),
                      "  max = ", max(peu_per_keu$N_Links)),
    x = "Number of PEU–KEU links per KEU code",
    y = "Number of KEU codes"
  ) +
  THEME_BASE

ggsave("outputs/3_1/fig_3_2_peu_per_keu_dist.png", p2,
       width = 8, height = 5, dpi = 200)
print(p2)

# ── PLOT 3: Top KEU codes per domain (Knowledge / Skills / Social) ───────────
# Rationale: "top courses by links" mostly reflects how many PEU a course has,
# not which competencies dominate. This shows the most-referenced KEU codes
# within each domain, which speaks directly to which competencies dominate.
top_keu_per_type <- peu_per_keu %>%
  mutate(KEU_Type = factor(KEU_Type,
                           levels = c("Knowledge", "Skills", "Social Competences"))) %>%
  group_by(KEU_Type) %>%
  slice_max(N_Links, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(KEU_Label = paste0(KEU_Code))

p3 <- top_keu_per_type %>%
  mutate(KEU_Label = fct_reorder(KEU_Label, N_Links)) %>%
  ggplot(aes(N_Links, KEU_Label, fill = KEU_Type)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(N_Links, " PEU links")),
            hjust = -0.05, size = 3) +
  facet_wrap(~KEU_Type, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = TYPE_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(
    title    = "Top 3 most-referenced KEU codes per domain",
    subtitle = "Ranked by number of PEU-KEU links - the competencies most addressed across the curriculum",
    x = "Number of PEU-KEU links", y = NULL
  ) +
  THEME_BASE +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text.y = element_text(size = 9)
  )

ggsave("outputs/3_1/fig_3_3_top_keu_per_domain.png", p3,
       width = 8, height = 6, dpi = 200)
print(p3)

# ── PLOT 4: KEU coverage heatmap, faceted by domain for readability ──────────
# With ~56 KEU codes a single column becomes hard to read, so each domain
# (Knowledge / Skills / Social Competences) gets its own panel.
peu_per_keu_plot <- peu_per_keu %>%
  mutate(
    KEU_Num  = as.integer(str_extract(KEU_Code, "\\d{2}$")),
    KEU_Type = factor(KEU_Type, levels = c("Knowledge", "Skills", "Social Competences"))
  ) %>%
  arrange(KEU_Type, KEU_Num) %>%
  group_by(KEU_Type) %>%
  mutate(KEU_Code_Ord = fct_reorder(KEU_Code, KEU_Num)) %>%
  ungroup()

p4 <- peu_per_keu_plot %>%
  ggplot(aes(x = 1, y = KEU_Code_Ord, fill = N_Links)) +
  geom_tile(color = "white", linewidth = 0.5, width = 0.95) +
  geom_text(aes(label = N_Links), size = 3, color = "white", fontface = "bold") +
  facet_wrap(~KEU_Type, scales = "free_y", ncol = 3) +
  scale_fill_gradient(low = "#A8DADC", high = "#1D3557", name = "PEU links") +
  labs(
    title    = "PEU coverage heatmap: links per KEU code, by domain",
    subtitle = "Darker = more PEU entries linked to that learning outcome",
    x = NULL, y = NULL
  ) +
  THEME_BASE +
  theme(
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 8),
    strip.text   = element_text(face = "bold", size = 11),
    legend.position = "right",
    panel.spacing = unit(1, "lines")
  )

ggsave("outputs/3_1/fig_3_4_keu_heatmap.png", p4,
       width = 11, height = 12, dpi = 200)
print(p4)

# ── PLOT 5: Domain-level summary (Knowledge / Skills / Social) ───────────────
# Aggregates per-domain coverage: how many KEU codes, how many PEU links,
# how many courses touch that domain, and the mean links per KEU code.
# (mapping has KEU_Code but not KEU_Type, so join via keu_df first)
keu_to_type <- keu_df %>% distinct(KEU_Code, KEU_Type)

domain_summary <- mapping %>%
  select(-any_of("KEU_Type")) %>%      # avoid KEU_Type.x/.y collision if mapping already has one
  left_join(keu_to_type, by = "KEU_Code") %>%
  filter(!is.na(KEU_Type)) %>%
  group_by(KEU_Type) %>%
  summarise(
    N_KEU_Codes       = n_distinct(KEU_Code),
    N_PEU_KEU_Links    = n(),
    N_Courses_Involved = n_distinct(Subject),
    Mean_Links_Per_KEU = round(n() / n_distinct(KEU_Code), 1),
    .groups = "drop"
  ) %>%
  mutate(KEU_Type = factor(KEU_Type, levels = c("Knowledge", "Skills", "Social Competences"))) %>%
  arrange(KEU_Type)

write_csv(domain_summary, "outputs/3_1/3_1_domain_summary.csv")
print(domain_summary)

# Render the two summary tables as PNG images for easy inclusion in the thesis
save_table_png <- function(df, file, title, col_widths = NULL) {
  tt <- ttheme_minimal(
    core    = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 10)),
    colhead = list(fg_params = list(fontface = "bold", fontsize = 10, col = "white"),
                   bg_params = list(fill = "#1D3557", col = NA)),
    rowhead = list(fg_params = list(fontface = "plain", fontsize = 10))
  )
  g <- tableGrob(df, rows = NULL, theme = tt)
  title_grob <- textGrob(title, gp = gpar(fontsize = 13, fontface = "bold"),
                         hjust = 0.5, x = 0.5)
  g <- gtable::gtable_add_rows(g, heights = grobHeight(title_grob) + unit(8, "mm"), pos = 0)
  g <- gtable::gtable_add_grob(g, title_grob, t = 1, l = 1, r = ncol(g))
  png(file, width = 8, height = 0.6 + 0.35 * nrow(df), units = "in", res = 200)
  grid.draw(g)
  dev.off()
}

save_table_png(
  summary_stats,
  "outputs/3_1/fig_3_5_summary_table.png",
  "PEU-KEU mapping summary"
)

save_table_png(
  domain_summary %>%
    rename(
      `Domain`                 = KEU_Type,
      `KEU codes`              = N_KEU_Codes,
      `PEU-KEU links`          = N_PEU_KEU_Links,
      `Courses involved`       = N_Courses_Involved,
      `Mean links per KEU`     = Mean_Links_Per_KEU
    ),
  "outputs/3_1/fig_3_6_domain_summary_table.png",
  "PEU-KEU coverage by competency domain"
)

# ── Weakly-covered KEU codes (lowest N_Links) — relevant for gap discussion ───
# These are the learning outcomes least reinforced across the curriculum;
# worth flagging explicitly since they're easy to miss in the heatmap's shading.
LOW_COVERAGE_THRESHOLD <- 2   # KEU codes with <= this many PEU links

keu_low_coverage <- peu_per_keu %>%
  filter(N_Links <= LOW_COVERAGE_THRESHOLD) %>%
  arrange(N_Links, KEU_Code) %>%
  select(KEU_Code, KEU_Type, N_PEU, N_Courses, N_Links)

write_csv(keu_low_coverage, "outputs/3_1/3_1_low_coverage_keu.csv")
cat("\nKEU codes with <=", LOW_COVERAGE_THRESHOLD, "PEU links (", nrow(keu_low_coverage), "codes):\n")
print(keu_low_coverage, n = 30)

if (nrow(keu_low_coverage) > 0) {
  save_table_png(
    keu_low_coverage %>%
      rename(`KEU code` = KEU_Code, `Domain` = KEU_Type, `Unique PEU` = N_PEU,
             `Courses` = N_Courses, `PEU-KEU links` = N_Links),
    "outputs/3_1/fig_3_7_low_coverage_table.png",
    paste0("KEU with weakest curriculum coverage (<= ", LOW_COVERAGE_THRESHOLD, " links)")
  )
}


cat("\n===== CHAPTER 3.1 STATISTICS =====\n")
cat("KEU codes total:              ", n_keu_total, "\n")
cat("  Knowledge (W):              ", keu_type_counts$N_KEU[keu_type_counts$KEU_Type=="Knowledge"], "\n")
cat("  Skills (U):                 ", keu_type_counts$N_KEU[keu_type_counts$KEU_Type=="Skills"], "\n")
cat("  Social Competences (K):     ", keu_type_counts$N_KEU[keu_type_counts$KEU_Type=="Social Competences"], "\n")
cat("Unique PEU entries (per course):", n_peu_total, "\n")
cat("PEU–KEU mapping rows:          ", n_peu_rows, "\n")
cat("Courses in mapping:            ", n_courses, "\n")
cat("Mean PEU links per KEU:        ", round(mean(peu_per_keu$N_Links), 1), "\n")
cat("Max PEU links (single KEU):    ", max(peu_per_keu$N_Links),
    " —> ", peu_per_keu$KEU_Code[which.max(peu_per_keu$N_Links)], "\n")
cat("Min PEU links (single KEU):    ", min(peu_per_keu$N_Links), "\n")
cat("\nTop 5 courses by PEU links:\n")
print(peu_per_course %>% slice_head(n=5) %>% select(Subject, N_PEU, N_KEU_refs, N_Links))
cat("\n6 plots/tables saved to outputs/3_1/\n")
cat("Summary CSVs saved to outputs/3_1/3_1_summary_stats.csv and 3_1_domain_summary.csv\n")

# ── Interactive table viewing (RStudio) ───────────────────────────────────────
# Opens each summary table in the RStudio Viewer/Data tab. Falls back to
# print() if View() is unavailable (e.g. running via Rscript outside RStudio).
if (interactive()) {
  View(summary_stats,     "3.1 Overall summary stats")
  View(domain_summary,    "3.1 Domain-level summary")
  View(keu_low_coverage,  "3.1 Low-coverage KEU codes")
} else {
  print(summary_stats)
  print(domain_summary)
  print(keu_low_coverage)
}