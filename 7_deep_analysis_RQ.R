# ==============================================================================
# STAGE 8: DEEP STATISTICAL & ML ANALYSIS -- RQ1-RQ6
#
# Input files (must be in the same folder as this script):
#   final_competence_matrix_ai_recheck.csv
#   market_demand_by_competence_final.csv
#   bucket_summary_final.csv
#   tech_demand_final.csv
#   top6_per_bucket.csv          <- synonym-consolidated, used for RQ1/RQ2
#   keu_semantic_buckets.csv
#   cleaned_data.csv
# ==============================================================================

library(tidyverse)
library(scales)
library(ineq)
library(tidytext)
library(broom)
library(pROC)
library(cluster)
library(mclust)
library(ggrepel)
library(igraph)
library(ggraph)

theme_set(theme_minimal(base_size = 12))

# ------------------------------------------------------------------------------
# 0A. Working directory -> folder of this script
# ------------------------------------------------------------------------------
if (Sys.getenv("RSTUDIO") == "1") {
  script_dir <- tryCatch(
    dirname(rstudioapi::getSourceEditorContext()$path),
    error = function(e) getwd()
  )
  if (nchar(script_dir) > 0) setwd(script_dir)
}
message("Working directory: ", getwd())

dir.create("outputs/plots",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

if (!dir.exists("outputs/plots"))  stop("Cannot create outputs/plots in: ",  getwd())
if (!dir.exists("outputs/tables")) stop("Cannot create outputs/tables in: ", getwd())
message("outputs/plots  -> ", normalizePath("outputs/plots"))
message("outputs/tables -> ", normalizePath("outputs/tables"))

# ------------------------------------------------------------------------------
# 0B. Display helpers -- every plot and table prints to console AND saves file
# ------------------------------------------------------------------------------
SEP <- paste0("\n", strrep("=", 70), "\n")

save_plot <- function(p, name, w = 10, h = 7) {
  cat(SEP, "PLOT:", name, SEP)
  print(p)
  fpath <- normalizePath(file.path("outputs/plots",  paste0(name, ".png")), mustWork = FALSE)
  tryCatch(ggsave(fpath, plot = p, width = w, height = h, dpi = 200),
           error = function(e) message("WARNING: could not save ", fpath, " -- ", conditionMessage(e)))
  message("Saved: ", fpath)
}

save_table <- function(df, name, n_print = 40) {
  cat(SEP, "TABLE:", name, SEP)
  print(df, n = min(n_print, nrow(df)), width = 130)
  fpath <- normalizePath(file.path("outputs/tables", paste0(name, ".csv")), mustWork = FALSE)
  tryCatch(write_excel_csv(df, fpath),
           error = function(e) message("WARNING: could not save ", fpath, " -- ", conditionMessage(e)))
  message("Saved: ", fpath)
}

# ------------------------------------------------------------------------------
# 0C. Load data
# ------------------------------------------------------------------------------
df          <- read_csv("final_competence_matrix_ai_recheck.csv", show_col_types = FALSE)
mkt_comp    <- read_csv("market_demand_by_competence_final.csv",  show_col_types = FALSE)
bucket_sum  <- read_csv("bucket_summary_final.csv",               show_col_types = FALSE)
tech_demand <- read_csv("tech_demand_final.csv",                  show_col_types = FALSE)
top6        <- read_csv("top6_per_bucket.csv",                    show_col_types = FALSE)

# Pull Seniority_level from cleaned_data if missing
if (!"Seniority_level" %in% names(df) && file.exists("cleaned_data.csv")) {
  cleaned <- read_csv("cleaned_data.csv", show_col_types = FALSE)
  if (all(c("Job_offer_URL", "Seniority_level") %in% names(cleaned)))
    df <- df %>% left_join(cleaned %>% select(Job_offer_URL, Seniority_level), by = "Job_offer_URL")
}

df_text        <- df %>% filter(Source == "Text_Analysis", !is.na(Competence_Name))
n_offers_total <- n_distinct(df$Job_offer_URL)

message("Total unique job offers:   ", n_offers_total)
message("Text_Analysis rows:        ", nrow(df_text))
message("Distinct competency names: ", n_distinct(df_text$Competence_Name))
message("top6_per_bucket rows:      ", nrow(top6))

# Bucket colour palette (consistent across all charts)
BUCKET_COLS <- c(
  "AI_ARTIFICIAL_INTELLIGENCE"    = "#E63946",
  "BIG_DATA_DATABASES"            = "#F4A261",
  "DATA_ANALYSIS_STATS"           = "#E9C46A",
  "IT_SERVICES_CLOUD_UX"          = "#2A9D8F",
  "PROGRAMMING_WEB"               = "#264653",
  "MANAGEMENT_INFORMATION_SYSTEMS"= "#457B9D",
  "PROCESS_OPTIMIZATION"          = "#6A4C93",
  "PRODUCTION_LOGISTICS"          = "#8B5E3C",
  "QUALITY_MANAGEMENT"            = "#B5838D",
  "ERGONOMICS_WORKPLACE"          = "#A8DADC",
  "PROJECT_MANAGEMENT"            = "#1D3557",
  "INNOVATION_ENTREPRENEURSHIP"   = "#F77F00",
  "FINANCE_ACCOUNTING"            = "#4CAF50",
  "LEGAL_ECONOMY_ESG"             = "#795548",
  "E_COMMERCE_MARKETING"          = "#FF6B6B",
  "LEADERSHIP_DECISION_MAKING"    = "#9C27B0",
  "STAKEHOLDER_CLIENT_RELATIONS"  = "#00BCD4",
  "HR_RECRUITMENT"                = "#FF9800",
  "SOFT_INTERPERSONAL"            = "#607D8B",
  "OTHER_UNCLASSIFIED"            = "#9E9E9E"
)

# ==============================================================================
# RQ1 -- Most frequently required competencies (synonym-consolidated via top6)
# ==============================================================================
cat(SEP, "RQ1: Most frequently required competencies", SEP)

# -- 1a. Top 30 consolidated --
rq1_top30 <- top6 %>%
  arrange(desc(n_offers_total)) %>%
  slice_head(n = 30) %>%
  rename(Competence_Name = canonical_name, N_Offers = n_offers_total)

save_table(rq1_top30, "RQ1_top30_consolidated")

p_rq1_top30 <- rq1_top30 %>%
  mutate(Competence_Name = fct_reorder(Competence_Name, N_Offers),
         Pct = round(100 * N_Offers / n_offers_total, 1)) %>%
  ggplot(aes(N_Offers, Competence_Name, fill = Final_Bucket)) +
  geom_col() +
  geom_text(aes(label = paste0(Pct, "%")), hjust = -0.1, size = 3) +
  scale_fill_manual(values = BUCKET_COLS, name = "Bucket") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "RQ1: Top 30 most required competencies (synonym-consolidated)",
       subtitle = paste0("N = ", n_offers_total, " unique job offers  |  % = share of all offers"),
       x = "Number of offers", y = NULL)
save_plot(p_rq1_top30, "RQ1_top30_consolidated", w = 13, h = 9)

# -- 1b. Top 6 per bucket (faceted) -- shows breadth, not just overall leaders --
top6_clean <- top6 %>%
  filter(!is.na(canonical_name), Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  rename(Competence_Name = canonical_name, N_Offers = n_offers_total)

save_table(top6_clean, "RQ1_top6_per_bucket")

p_rq1_facet <- top6_clean %>%
  mutate(Competence_Name = str_trunc(Competence_Name, 40)) %>%
  ggplot(aes(N_Offers,
             reorder_within(Competence_Name, N_Offers, Final_Bucket),
             fill = Final_Bucket)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = N_Offers), hjust = -0.1, size = 2.5) +
  scale_y_reordered() +
  scale_fill_manual(values = BUCKET_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_wrap(~ Final_Bucket, scales = "free", ncol = 4) +
  labs(title = "RQ1 / RQ2: Top 6 competencies per bucket (AI-consolidated synonyms)",
       subtitle = "Each facet = one semantic category  |  bars = number of job offers",
       x = "Number of offers", y = NULL) +
  theme(axis.text.y = element_text(size = 6.5),
        strip.text  = element_text(size = 6.5, face = "bold"))
save_plot(p_rq1_facet, "RQ1_RQ2_top6_per_bucket", w = 18, h = 14)

# -- 1c. Pareto + Gini (demand concentration) --
pareto_df <- mkt_comp %>%
  filter(N_Offers_Requiring > 0) %>%
  arrange(desc(N_Offers_Requiring)) %>%
  mutate(rank      = row_number(),
         cum_pct   = 100 * cumsum(N_Offers_Requiring) / sum(N_Offers_Requiring),
         rank_pct  = 100 * rank / n())

gini_coef             <- ineq::Gini(pareto_df$N_Offers_Requiring)
n_for_80              <- pareto_df %>% filter(cum_pct <= 80) %>% nrow()
pct_for_80            <- round(100 * n_for_80 / nrow(pareto_df), 1)

cat("Gini coefficient of demand concentration:", round(gini_coef, 3), "\n")
cat(pct_for_80, "% of competencies cover 80% of demand (", n_for_80, "/", nrow(pareto_df), ")\n")

top40 <- pareto_df %>% slice_head(n = 40)
sf    <- max(top40$N_Offers_Requiring) / 100

p_pareto <- ggplot(top40, aes(x = reorder(Competence_Name, -N_Offers_Requiring))) +
  geom_col(aes(y = N_Offers_Requiring), fill = "#2E86AB") +
  geom_line(aes(y = cum_pct * sf, group = 1), color = "#E63946", linewidth = 1.1) +
  geom_point(aes(y = cum_pct * sf), color = "#E63946", size = 1.8) +
  geom_hline(yintercept = 80 * sf, linetype = "dashed", color = "#E63946", alpha = 0.5) +
  scale_y_continuous(
    name     = "Number of offers",
    sec.axis = sec_axis(~ . / sf, name = "Cumulative % of total demand",
                        labels = label_percent(scale = 1))
  ) +
  labs(title = "RQ1: Pareto chart -- demand concentration across top 40 competencies",
       subtitle = paste0("Gini = ", round(gini_coef, 3),
                         "  |  ", pct_for_80, "% of competencies = 80% of demand"),
       x = NULL) +
  theme(axis.text.x = element_text(angle = 72, hjust = 1, size = 6.5))
save_plot(p_pareto, "RQ1_pareto_chart", w = 14, h = 7)

# Lorenz curve
lc      <- ineq::Lc(pareto_df$N_Offers_Requiring)
lc_df   <- tibble(p = lc$p, L = lc$L)
p_lorenz <- ggplot(lc_df, aes(p, L)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = p, ymax = L), fill = "#2E86AB", alpha = 0.15) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "RQ1: Lorenz curve of competency demand",
       subtitle = paste0("Gini = ", round(gini_coef, 3),
                         "  (dashed = perfect equality; curve below = concentration)"),
       x = "Cumulative % of competencies (rarest first)",
       y = "Cumulative % of demand")
save_plot(p_lorenz, "RQ1_lorenz_curve")

save_table(pareto_df, "RQ1_pareto_data")

# ==============================================================================
# RQ2 -- Grouping competencies into semantic buckets + ML validation
# ==============================================================================
cat(SEP, "RQ2: Bucket sizes + ML validation", SEP)

# -- 2a. Bucket sizes --
rq2_sizes <- df_text %>%
  filter(!is.na(Final_Bucket)) %>%
  group_by(Final_Bucket) %>%
  summarise(N_Distinct_Competences = n_distinct(Competence_Name),
            N_Offers               = n_distinct(Job_offer_URL),
            N_Rows                 = n(),
            .groups = "drop") %>%
  mutate(Pct_Offers = round(100 * N_Offers / n_offers_total, 1)) %>%
  arrange(desc(N_Offers))
save_table(rq2_sizes, "RQ2_bucket_sizes")

p_rq2_sizes <- rq2_sizes %>%
  filter(Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  mutate(Final_Bucket = fct_reorder(Final_Bucket, N_Offers)) %>%
  ggplot(aes(N_Offers, Final_Bucket, fill = Final_Bucket)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(N_Distinct_Competences, " skills\n", Pct_Offers, "%")),
            hjust = -0.05, size = 3, lineheight = 0.9) +
  scale_fill_manual(values = BUCKET_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "RQ2: Job offers per competency bucket",
       subtitle = "Labels: number of distinct skills in bucket  |  % of all offers",
       x = "Number of offers mentioning ≥1 competency from this bucket", y = NULL)
save_plot(p_rq2_sizes, "RQ2_bucket_sizes_chart", h = 8)

# -- 2b. Standard vs Gap by bucket (stacked bar -- answers RQ4/RQ5 visually too) --
rq2_std_gap <- df_text %>%
  filter(!is.na(Final_Bucket), Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  group_by(Final_Bucket, Status) %>%
  summarise(N_Offers = n_distinct(Job_offer_URL), .groups = "drop") %>%
  group_by(Final_Bucket) %>%
  mutate(Total = sum(N_Offers), Pct = round(100 * N_Offers / Total, 1)) %>%
  ungroup()
save_table(rq2_std_gap, "RQ2_standard_vs_gap_by_bucket")

p_rq2_stdgap <- rq2_std_gap %>%
  mutate(Final_Bucket = fct_reorder(Final_Bucket, Total)) %>%
  ggplot(aes(N_Offers, Final_Bucket, fill = Status)) +
  geom_col(position = "stack") +
  # In-segment % labels get clipped or overlap whenever a segment is narrow
  # (small bucket, or a near-0%/100% split) -- instead, place ONE combined
  # label just outside the end of the full bar, which is always wide enough
  # to read regardless of how small the individual segments are.
  geom_text(
    data = rq2_std_gap %>%
      distinct(Final_Bucket, Total) %>%
      left_join(rq2_std_gap %>% filter(Status == "Standard") %>%
                  select(Final_Bucket, Pct_Standard = Pct),
                by = "Final_Bucket") %>%
      left_join(rq2_std_gap %>% filter(Status == "Gap") %>%
                  select(Final_Bucket, Pct_Gap = Pct),
                by = "Final_Bucket") %>%
      mutate(Pct_Standard = coalesce(Pct_Standard, 0),
             Pct_Gap      = coalesce(Pct_Gap, 0),
             Label        = paste0("Std ", Pct_Standard, "%  /  Gap ", Pct_Gap, "%")),
    aes(x = Total, y = Final_Bucket, label = Label),
    inherit.aes = FALSE, hjust = -0.05, size = 2.8, color = "black"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_manual(values = c(Standard = "#2E86AB", Gap = "#E63946")) +
  labs(title = "RQ2 / RQ4 / RQ5: Standard vs Gap breakdown by bucket",
       subtitle = "How well each knowledge domain is covered by the IZ1 curriculum",
       x = "Number of offer-competency occurrences", y = NULL, fill = NULL) +
  theme(legend.position = "top")
save_plot(p_rq2_stdgap, "RQ2_RQ4_RQ5_standard_gap_by_bucket", h = 8)

# ==============================================================================
# RQ2 -- Co-occurrence Network Validation
# ==============================================================================
# `edges` was referenced below without ever being built, so R fell back to
# the igraph::edges() function instead (hence "cannot coerce class 'function'
# to a data.frame"). Building it here from the same TOP-N co-occurrence logic
# used later in "EXTRA 5" -- but kept self-contained so this block does not
# depend on code that runs further down the script.

has_igraph_rq2 <- requireNamespace("igraph", quietly = TRUE)

if (!has_igraph_rq2) {
  
  message("igraph not installed -- skipping RQ2 network validation.")
  
} else {
  
  library(igraph)
  
  TOP_N_NETWORK_VALIDATION <- 25
  
  top_network_names_rq2 <- mkt_comp %>%
    filter(N_Offers_Requiring > 0) %>%
    arrange(desc(N_Offers_Requiring)) %>%
    slice_head(n = TOP_N_NETWORK_VALIDATION) %>%
    pull(Competence_Name)
  
  cooc_long_rq2 <- df_text %>%
    filter(Competence_Name %in% top_network_names_rq2) %>%
    distinct(Job_offer_URL, Competence_Name)
  
  if (n_distinct(cooc_long_rq2$Job_offer_URL) < 2 ||
      n_distinct(cooc_long_rq2$Competence_Name) < 2) {
    
    message("Not enough distinct offers/competencies to build a co-occurrence ",
            "network for RQ2 -- skipping.")
    
  } else {
    
    incidence_rq2 <- table(cooc_long_rq2$Job_offer_URL, cooc_long_rq2$Competence_Name)
    cooc_mat_rq2  <- t(incidence_rq2) %*% incidence_rq2
    diag(cooc_mat_rq2) <- 0
    
    edges <- as.data.frame(as.table(cooc_mat_rq2)) %>%
      rename(From = Var1, To = Var2, Weight = Freq) %>%
      filter(Weight > 0, as.character(From) < as.character(To)) %>%
      arrange(desc(Weight))
    
    if (nrow(edges) == 0) {
      
      message("No co-occurring competency pairs found -- skipping RQ2 network validation.")
      
    } else {
      
      g <- graph_from_data_frame(edges, directed = FALSE)
      
      network_metrics <- tibble(
        Nodes = gorder(g),
        Edges = gsize(g),
        Density = edge_density(g),
        Avg_Degree = mean(degree(g)),
        Avg_Clustering = transitivity(g, type = "average"),
        Connected_Components = components(g)$no,
        Modularity = modularity(cluster_louvain(g))
      )
      
      save_table(network_metrics, "RQ2_network_validation")
    }
  }
}

# ==============================================================================
# RQ3 -- Lexical baseline vs semantic AI pipeline
# ==============================================================================
cat(SEP, "RQ3: Lexical baseline vs semantic pipeline", SEP)

if (file.exists("keu_semantic_buckets.csv")) {
  keu_text <- read_csv("keu_semantic_buckets.csv", show_col_types = FALSE) %>%
    filter(!is.na(KEU_Summary), KEU_Summary != "") %>%
    pull(KEU_Summary) %>% tolower() %>% paste(collapse = " ||| ")
  
  rq3_eval <- mkt_comp %>%
    distinct(Competence_Name, Final_Bucket) %>%
    left_join(df_text %>% distinct(Competence_Name, Status), by = "Competence_Name") %>%
    filter(!is.na(Status)) %>%
    mutate(lexical = if_else(str_detect(keu_text, fixed(tolower(Competence_Name))),
                             "Standard", "Gap"))
  
  TP <- sum(rq3_eval$lexical == "Standard" & rq3_eval$Status == "Standard")
  FP <- sum(rq3_eval$lexical == "Standard" & rq3_eval$Status == "Gap")
  FN <- sum(rq3_eval$lexical == "Gap"      & rq3_eval$Status == "Standard")
  TN <- sum(rq3_eval$lexical == "Gap"      & rq3_eval$Status == "Gap")
  
  prec <- TP / (TP + FP)
  rec  <- TP / (TP + FN)
  f1   <- 2 * prec * rec / (prec + rec)
  
  cat("Precision:", round(prec,3), "  Recall:", round(rec,3), "  F1:", round(f1,3), "\n")
  cat("Competencies recovered ONLY by semantic matching (FN):", FN, "\n")
  
  mcn <- mcnemar.test(matrix(c(TP,FN,FP,TN), 2), correct = TRUE)
  cat("McNemar p-value:", signif(mcn$p.value, 4), "\n")
  
  # Confusion matrix heatmap
  conf_df <- tibble(
    Predicted  = c("Standard","Standard","Gap","Gap"),
    Actual     = c("Standard","Gap","Standard","Gap"),
    Count      = c(TP, FP, FN, TN),
    Label      = c(paste0("TP\n",TP), paste0("FP\n",FP),
                   paste0("FN\n",FN), paste0("TN\n",TN))
  )
  p_conf <- conf_df %>%
    ggplot(aes(Actual, Predicted, fill = Count)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = Label), size = 8, fontface = "bold", color = "white") +
    scale_fill_gradient(low = "#A8DADC", high = "#1D3557") +
    labs(title = "RQ3: Confusion matrix -- lexical baseline vs AI semantic pipeline",
         subtitle = paste0("Precision=", round(prec,3), "  Recall=", round(rec,3),
                           "  F1=", round(f1,3), "  McNemar p=", signif(mcn$p.value,3)),
         x = "Pipeline (AI) -- ground truth proxy",
         y = "Lexical-only baseline") +
    theme(legend.position = "none")
  save_plot(p_conf, "RQ3_confusion_matrix_heatmap", w = 6, h = 5)
  
  # Side-by-side Standard/Gap proportion comparison
  comp_df <- tibble(
    Method = rep(c("Lexical-only\n(baseline)", "Semantic AI\n(pipeline)"), each = 2),
    Status = rep(c("Standard","Gap"), 2),
    N      = c(sum(rq3_eval$lexical == "Standard"), sum(rq3_eval$lexical == "Gap"),
               sum(rq3_eval$Status  == "Standard"), sum(rq3_eval$Status  == "Gap"))
  )
  p_rq3 <- comp_df %>%
    ggplot(aes(Method, N, fill = Status)) +
    geom_col(position = "fill", width = 0.6) +
    geom_text(aes(label = paste0(round(100*N/sum(N),1),"%")),
              position = position_fill(vjust = 0.5), color = "white", size = 5) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_manual(values = c(Standard = "#2E86AB", Gap = "#E63946")) +
    labs(title = "RQ3: How semantic AI matching recovers competencies missed by dictionary lookup",
         subtitle = paste0(round(100*(1-rec),1),
                           "% of Standard competencies are INVISIBLE to pure keyword matching"),
         y = "Share of competencies", x = NULL, fill = NULL) +
    theme(legend.position = "top")
  save_plot(p_rq3, "RQ3_lexical_vs_semantic", w = 7, h = 6)
  
  rq3_metrics <- tibble(Metric = c("TP","FP","FN","TN","Precision","Recall","F1",
                                   "McNemar_stat","McNemar_p"),
                        Value  = c(TP,FP,FN,TN, round(prec,4),round(rec,4),round(f1,4),
                                   round(unname(mcn$statistic),4), signif(mcn$p.value,4)))
  save_table(rq3_metrics, "RQ3_lexical_vs_semantic_metrics")
  save_table(rq3_eval,    "RQ3_row_level_eval")
} else {
  message("RQ3: keu_semantic_buckets.csv not found -- skipping")
}

# ==============================================================================
# RQ4 -- How well does the IZ1 curriculum cover market requirements?
# ==============================================================================
cat(SEP, "RQ4: Curriculum coverage", SEP)

# -- 4a. Overall Standard / Gap split --
rq4_overall <- df_text %>%
  dplyr::count(Status) %>%
  mutate(Pct = round(100 * n / sum(n), 1))
cat("\nOverall Standard vs Gap:\n")
print(rq4_overall)

p_rq4_overall <- rq4_overall %>%
  ggplot(aes(Status, n, fill = Status)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(Pct, "%\n(n=", scales::comma(n), ")")),
            vjust = -0.3, size = 5) +
  scale_fill_manual(values = c(Standard = "#2E86AB", Gap = "#E63946")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "RQ4: Overall coverage of market requirements by the IZ1 curriculum",
       subtitle = paste0(rq4_overall$Pct[rq4_overall$Status=="Standard"],
                         "% of competency occurrences are covered by IZ1 learning outcomes"),
       y = "Number of competency occurrences (Text_Analysis)", x = NULL) +
  theme(legend.position = "none")
save_plot(p_rq4_overall, "RQ4_overall_coverage", w = 6, h = 6)

# -- 4b. Coverage per KEU code (which learning outcomes are most in demand?) --
rq4_keu <- df_text %>%
  filter(Matched_Code != "competency_gap", !is.na(Matched_Code)) %>%
  group_by(Matched_Code) %>%
  summarise(N_Offers       = n_distinct(Job_offer_URL),
            N_Competences  = n_distinct(Competence_Name),
            Example        = paste(unique(Competence_Name)[1:min(3,.N <- n_distinct(Competence_Name))],
                                   collapse = " | "),
            .groups = "drop") %>%
  mutate(Pct = round(100 * N_Offers / n_offers_total, 1)) %>%
  arrange(desc(N_Offers))
save_table(rq4_keu, "RQ4_coverage_by_keu_code")

p_rq4_keu <- rq4_keu %>%
  slice_head(n = 15) %>%
  mutate(Matched_Code = fct_reorder(Matched_Code, N_Offers)) %>%
  ggplot(aes(N_Offers, Matched_Code)) +
  geom_col(fill = "#2E86AB") +
  geom_text(aes(label = paste0(Pct, "%")), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "RQ4: Which IZ1 learning outcomes are most demanded by the market?",
       subtitle = "Top 15 KEU codes by number of job offers requiring a mapped competency",
       x = "Number of offers", y = "KEU code")
save_plot(p_rq4_keu, "RQ4_coverage_by_keu_code", w = 10, h = 7)

# ==============================================================================
# RQ5 -- Competency gaps: what is missing from the curriculum?
# ==============================================================================
cat(SEP, "RQ5: Competency gaps", SEP)

# -- 5a. Top 25 gaps by number of offers --
rq5_gaps <- mkt_comp %>%
  filter(Status == "Gap") %>%
  arrange(desc(N_Offers_Requiring)) %>%
  slice_head(n = 25)
save_table(rq5_gaps, "RQ5_top25_gaps")

p_rq5_gaps <- rq5_gaps %>%
  mutate(Competence_Name = str_trunc(Competence_Name, 55),
         Competence_Name = fct_reorder(Competence_Name, N_Offers_Requiring)) %>%
  ggplot(aes(N_Offers_Requiring, Competence_Name, fill = Final_Bucket)) +
  geom_col() +
  geom_text(aes(label = paste0(Pct_Of_All_Offers, "%")), hjust = -0.1, size = 3) +
  scale_fill_manual(values = BUCKET_COLS, name = "Bucket") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "RQ5: Top 25 competency gaps -- required by market, absent from IZ1 curriculum",
       subtitle = "% = share of all job offers requiring this competency",
       x = "Number of offers", y = NULL)
save_plot(p_rq5_gaps, "RQ5_top25_gaps", w = 14, h = 10)

# -- 5b. Top 3 gaps per bucket (recommendations table) --
rq5_recs <- mkt_comp %>%
  filter(Status == "Gap") %>%
  group_by(Final_Bucket) %>%
  slice_max(N_Offers_Requiring, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(N_Offers_Requiring)) %>%
  mutate(Recommendation = paste0(
    "Add/strengthen '", Competence_Name, "' -- required in ",
    N_Offers_Requiring, " offers (", Pct_Of_All_Offers, "% of all)"
  ))
save_table(rq5_recs, "RQ5_gap_recommendations_per_bucket")

p_rq5_recs <- rq5_recs %>%
  mutate(label = paste0(str_trunc(Competence_Name, 35), " (", N_Offers_Requiring, ")"),
         label = fct_reorder(label, N_Offers_Requiring),
         Final_Bucket = str_replace_all(Final_Bucket, "_", "\n")) %>%
  ggplot(aes(N_Offers_Requiring, label, fill = Final_Bucket)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Final_Bucket, scales = "free", ncol = 4) +
  labs(title = "RQ5: Top 3 curriculum gaps per bucket -- recommended additions",
       subtitle = "All bars = competencies NOT covered by IZ1, grouped by knowledge domain",
       x = "Number of offers requiring", y = NULL) +
  theme(axis.text.y = element_text(size = 6),
        strip.text  = element_text(size = 5.5, face = "bold"))
save_plot(p_rq5_recs, "RQ5_gap_recommendations_per_bucket", w = 18, h = 14)

# -- 5c. Chi-square: does Gap/Standard depend on the bucket? --
chi_tab <- df_text %>%
  filter(!is.na(Final_Bucket)) %>%
  dplyr::count(Final_Bucket, Status) %>%
  pivot_wider(names_from = Status, values_from = n, values_fill = 0)
chi_m   <- as.matrix(chi_tab %>% select(-Final_Bucket))
rownames(chi_m) <- chi_tab$Final_Bucket
chi_res <- chisq.test(chi_m)
cv      <- sqrt(unname(chi_res$statistic) / (sum(chi_m) * (min(dim(chi_m)) - 1)))
cat("Chi-square (Status ~ Bucket): X2 =", round(unname(chi_res$statistic),2),
    "  p =", signif(chi_res$p.value,4), "  Cramer's V =", round(cv,3), "\n")
save_table(tibble(X2 = round(unname(chi_res$statistic),3), df = chi_res$parameter,
                  p  = signif(chi_res$p.value,5), CramersV = round(cv,4)),
           "RQ5_chi_square")

# ==============================================================================
# RQ6 -- Competencies across seniority levels
# ==============================================================================
cat(SEP, "RQ6: Seniority-level analysis", SEP)

has_seniority <- "Seniority_level" %in% names(df)

if (has_seniority) {
  
  # -- 6a. Distribution of offers by seniority --
  # IMPORTANT: uses `df` (the FULL dataset, all Source types), not `df_text`.
  # `df_text` only contains rows with Source == "Text_Analysis", so any offer
  # whose competencies came entirely from Tech_Mandatory/Tech_Nice_to_Have
  # (or had zero extracted competencies at all) is completely absent from
  # df_text -- even though it has a perfectly valid Seniority_level. Using
  # df_text here silently undercounts total offers; this block is purely
  # about offers, not about competencies, so the full `df` is the correct
  # source. NA is additionally relabelled as "No data" as a safety net in
  # case any offer genuinely lacks a Seniority_level value.
  sen_dist <- df %>%
    distinct(Job_offer_URL, Seniority_level) %>%
    mutate(Seniority_level = if_else(is.na(Seniority_level), "No data", Seniority_level)) %>%
    dplyr::count(Seniority_level, name = "N_Offers") %>%
    mutate(Pct = round(100 * N_Offers / sum(N_Offers), 1)) %>%
    arrange(desc(N_Offers))
  save_table(sen_dist, "RQ6_offers_by_seniority")
  
  p_sen_dist <- sen_dist %>%
    mutate(Seniority_level = fct_reorder(Seniority_level, N_Offers)) %>%
    ggplot(aes(N_Offers, Seniority_level, fill = Seniority_level)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(Pct, "%  (n=", N_Offers, ")")), hjust = -0.05, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "RQ6: Distribution of job offers by seniority level",
         x = "Number of offers", y = NULL)
  save_plot(p_sen_dist, "RQ6_offers_by_seniority", w = 8, h = 5)
  
  # -- 6b. Top 10 competencies per seniority level --
  rq6_top <- df_text %>%
    filter(!is.na(Seniority_level)) %>%
    group_by(Seniority_level, Competence_Name, Final_Bucket) %>%
    summarise(N_Offers = n_distinct(Job_offer_URL), .groups = "drop") %>%
    group_by(Seniority_level) %>%
    slice_max(N_Offers, n = 10, with_ties = FALSE) %>%
    ungroup()
  save_table(rq6_top, "RQ6_top10_per_seniority")
  
  p_rq6_top <- rq6_top %>%
    mutate(Competence_Name = str_trunc(Competence_Name, 38)) %>%
    ggplot(aes(N_Offers,
               reorder_within(Competence_Name, N_Offers, Seniority_level),
               fill = Final_Bucket)) +
    geom_col() +
    scale_y_reordered() +
    scale_fill_manual(values = BUCKET_COLS, name = NULL) +
    facet_wrap(~ Seniority_level, scales = "free", ncol = 3) +
    labs(title = "RQ6: Top 10 competencies per seniority level",
         x = "Number of offers", y = NULL) +
    theme(axis.text.y = element_text(size = 7),
          strip.text  = element_text(face = "bold"),
          legend.text = element_text(size = 7))
  save_plot(p_rq6_top, "RQ6_top10_per_seniority", w = 16, h = 10)
  
  # -- 6c. Gap/Standard by seniority --
  rq6_stdgap <- df_text %>%
    filter(!is.na(Seniority_level)) %>%
    group_by(Seniority_level, Status) %>%
    summarise(N = n_distinct(Job_offer_URL), .groups = "drop") %>%
    group_by(Seniority_level) %>%
    mutate(Pct = round(100 * N / sum(N), 1)) %>% ungroup()
  save_table(rq6_stdgap, "RQ6_gap_by_seniority")
  
  p_rq6_gap <- rq6_stdgap %>%
    ggplot(aes(Seniority_level, N, fill = Status)) +
    geom_col(position = "fill", width = 0.6) +
    geom_text(aes(label = paste0(Pct, "%")),
              position = position_fill(vjust = 0.5), color = "white", size = 4) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_manual(values = c(Standard = "#2E86AB", Gap = "#E63946")) +
    labs(title = "RQ6: Curriculum coverage (Standard vs Gap) by seniority level",
         subtitle = "Higher Gap % at senior levels may indicate advanced skills not taught",
         y = "Share of offers", x = NULL, fill = NULL) +
    theme(legend.position = "top")
  save_plot(p_rq6_gap, "RQ6_gap_by_seniority", w = 9, h = 6)
  
  # -- 6d. Chi-square + logistic regression + ROC --
  chi6_tab <- df_text %>%
    filter(!is.na(Seniority_level), !is.na(Final_Bucket)) %>%
    dplyr::count(Seniority_level, Final_Bucket) %>%
    pivot_wider(names_from = Final_Bucket, values_from = n, values_fill = 0)
  chi6_m  <- as.matrix(chi6_tab %>% select(-Seniority_level))
  rownames(chi6_m) <- chi6_tab$Seniority_level
  chi6    <- suppressWarnings(chisq.test(chi6_m))
  cv6     <- sqrt(unname(chi6$statistic) / (sum(chi6_m) * (min(dim(chi6_m)) - 1)))
  cat("Chi-square (Seniority x Bucket): p =", signif(chi6$p.value,4),
      "  Cramer's V =", round(cv6,3), "\n")
  
  ml_data <- df_text %>%
    filter(!is.na(Seniority_level), !is.na(Final_Bucket)) %>%
    mutate(is_gap = as.integer(Status == "Gap"),
           Seniority_level = as.factor(Seniority_level),
           Final_Bucket    = as.factor(Final_Bucket))
  
  set.seed(42)
  idx   <- sample(nrow(ml_data), 0.7 * nrow(ml_data))
  train <- ml_data[idx, ]; test <- ml_data[-idx, ]
  fit   <- glm(is_gap ~ Seniority_level + Final_Bucket, data = train, family = "binomial")
  test$prob <- predict(fit, newdata = test, type = "response")
  roc_obj   <- roc(test$is_gap, test$prob, quiet = TRUE)
  auc_val   <- as.numeric(auc(roc_obj))
  cat("Logistic regression AUC:", round(auc_val, 3), "\n")
  
  p_roc <- ggroc(roc_obj, color = "#2E86AB", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey50") +
    annotate("text", x = 0.3, y = 0.2,
             label = paste0("AUC = ", round(auc_val, 3)), size = 5, color = "#E63946") +
    labs(title = "RQ6 (ML): ROC -- predicting 'competency gap' from Seniority + Bucket",
         subtitle = "AUC > 0.7 = seniority + bucket carry real predictive signal about gaps")
  save_plot(p_roc, "RQ6_roc_curve", w = 7, h = 6)
  
  save_table(broom::tidy(fit), "RQ6_logit_coefficients")
  save_table(tibble(AUC = round(auc_val,4), Chi2_p = signif(chi6$p.value,5),
                    CramersV = round(cv6,4)), "RQ6_chi_ml_metrics")
  
} else {
  message("RQ6: Seniority_level not available -- skipping")
  auc_val <- NA
}

# ==============================================================================
# TECH ANALYSIS -- mandatory vs nice-to-have + gap flag
# ==============================================================================
cat(SEP, "TECH: Mandatory vs optional technologies + curriculum gap", SEP)

tech_full <- tech_demand %>%
  filter(!is.na(Competence_Name)) %>%
  left_join(
    df_text %>% distinct(Competence_Name, Status, Matched_Code),
    by = "Competence_Name"
  ) %>%
  mutate(
    Is_Gap = if_else(is.na(Status) | Status == "Gap", "Gap", "Standard"),
    Matched_Code = if_else(is.na(Matched_Code), "competency_gap", Matched_Code)
  ) %>%
  arrange(desc(N_Offers_Total))

save_table(tech_full %>% slice_head(n = 40), "TECH_top40_mandatory_optional_gap")

tech_top20 <- tech_full %>% slice_head(n = 20)

p_tech <- tech_top20 %>%
  select(Competence_Name, Tech_Mandatory, Tech_Nice_to_Have, Is_Gap) %>%
  pivot_longer(c(Tech_Mandatory, Tech_Nice_to_Have),
               names_to = "Type", values_to = "N") %>%
  mutate(Type = recode(Type, Tech_Mandatory = "Mandatory", Tech_Nice_to_Have = "Nice-to-have"),
         Competence_Name = fct_reorder(Competence_Name, N, .fun = sum)) %>%
  ggplot(aes(N, Competence_Name, fill = Type, alpha = Is_Gap)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(Mandatory = "#E63946", `Nice-to-have` = "#457B9D")) +
  scale_alpha_manual(values = c(Gap = 1, Standard = 0.45),
                     labels = c(Gap = "GAP (not in curriculum)", Standard = "Covered")) +
  labs(title = "TECH: Top 20 technologies -- Mandatory vs Nice-to-have, curriculum gap flag",
       subtitle = "Solid = curriculum GAP  |  Faded = already covered by IZ1",
       x = "Number of offers", y = NULL, fill = "Requirement", alpha = "Curriculum") +
  theme(legend.position = "top")
save_plot(p_tech, "TECH_mandatory_vs_optional_gap", w = 13, h = 9)

# Gap-only tech summary
tech_gaps <- tech_full %>%
  filter(Is_Gap == "Gap") %>%
  arrange(desc(N_Offers_Total)) %>%
  slice_head(n = 20)
save_table(tech_gaps, "TECH_top20_GAP_technologies")

p_tech_gaps <- tech_gaps %>%
  mutate(Competence_Name = fct_reorder(Competence_Name, N_Offers_Total)) %>%
  ggplot(aes(N_Offers_Total, Competence_Name, fill = Final_Bucket)) +
  geom_col() +
  geom_text(aes(label = paste0("M:", Tech_Mandatory, "  NtH:", Tech_Nice_to_Have)),
            hjust = -0.05, size = 3) +
  scale_fill_manual(values = BUCKET_COLS, name = "Bucket") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "TECH: Top 20 technologies NOT covered by IZ1 curriculum (gap)",
       subtitle = "M = mandatory occurrences  |  NtH = nice-to-have occurrences",
       x = "Total offers mentioning", y = NULL)
save_plot(p_tech_gaps, "TECH_top20_gap_technologies", w = 13, h = 8)

# ==============================================================================
# AI / API SATURATION -- how far has AI wording spread across ALL buckets?
# ==============================================================================
cat(SEP, "AI/API SATURATION across all buckets", SEP)

AI_PAT <- regex(
  paste("\\bai\\b","\\ba\\.i\\.\\b","artificial intelligence","sztuczn\\w* intelig",
        "machine learning","\\bml\\b","deep learning","\\bllm\\b","large language model",
        "generative ai","genai","neural network","chatgpt","\\bgpt\\b","copilot",
        "\\bapi\\b","rest api","graph api","openai","langchain","vector database",
        "embeddings","rag\\b","mlops","prompt engineering",
        sep = "|"),
  ignore_case = TRUE
)

ai_hits <- df_text %>%
  filter(!is.na(Competence_Name), !is.na(Final_Bucket),
         str_detect(Competence_Name, AI_PAT)) %>%
  mutate(matched_term = str_extract(tolower(Competence_Name), AI_PAT))

# Overall headline
n_offers_ai <- n_distinct(ai_hits$Job_offer_URL)
cat("Offers mentioning AI/API wording:", n_offers_ai,
    "(", round(100 * n_offers_ai / n_offers_total, 1), "% of all offers)\n")

# By bucket
ai_by_bucket <- ai_hits %>%
  group_by(Final_Bucket) %>%
  summarise(N_Offers      = n_distinct(Job_offer_URL),
            N_Competences = n_distinct(Competence_Name),
            Top_examples  = paste(unique(Competence_Name)[1:min(4, n_distinct(Competence_Name))],
                                  collapse = " | "),
            .groups = "drop") %>%
  mutate(Pct_Of_All = round(100 * N_Offers / n_offers_total, 1)) %>%
  arrange(desc(N_Offers))
save_table(ai_by_bucket, "AI_saturation_by_bucket")

p_ai_bucket <- ai_by_bucket %>%
  filter(Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  mutate(Final_Bucket = fct_reorder(Final_Bucket, N_Offers)) %>%
  ggplot(aes(N_Offers, Final_Bucket, fill = Final_Bucket)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(Pct_Of_All, "%  (", N_Competences, " skills)")),
            hjust = -0.05, size = 3.2) +
  scale_fill_manual(values = BUCKET_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title = "AI/API wording saturation across all competency buckets",
       subtitle = paste0(round(100 * n_offers_ai / n_offers_total, 1),
                         "% of all job offers mention AI/API-related terms somewhere | % = of all offers"),
       x = "Number of offers with AI/API mention in this bucket", y = NULL)
save_plot(p_ai_bucket, "AI_saturation_by_bucket", w = 13, h = 8)

# Top AI-related terms by frequency
ai_term_freq <- ai_hits %>%
  dplyr::count(matched_term, sort = TRUE) %>%
  slice_head(n = 20)
save_table(ai_term_freq, "AI_top_terms_frequency")

p_ai_terms <- ai_term_freq %>%
  mutate(matched_term = fct_reorder(matched_term, n)) %>%
  ggplot(aes(n, matched_term)) +
  geom_col(fill = "#E63946") +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Most frequent AI/API-related terms in competency names",
       subtitle = "Extracted from all Text_Analysis rows  |  n = occurrences",
       x = "Number of occurrences", y = "Matched term")
save_plot(p_ai_terms, "AI_top_terms_frequency", w = 9, h = 7)

# Top 10 example competency names per AI term
ai_examples <- ai_hits %>%
  group_by(matched_term) %>%
  slice_head(n = 5) %>%
  summarise(Example_competencies = paste(Competence_Name, collapse = " | "),
            N_Offers = n_distinct(Job_offer_URL), .groups = "drop") %>%
  arrange(desc(N_Offers))
save_table(ai_examples, "AI_example_competencies_per_term")

# ==============================================================================
# EXTRA -- additional analyses requested for the thesis (frequency
# distribution, curriculum-vs-market heatmap, coverage score, gap index,
# competency co-occurrence network, manual-validation reliability scaffold).
# Everything above this block is unchanged; these sections only ADD outputs
# and reuse data already loaded (mkt_comp, bucket_sum, df_text, BUCKET_COLS).
# ==============================================================================
cat(SEP, "EXTRA: frequency distribution / heatmap / coverage / gap index / co-occurrence / reliability", SEP)

# ------------------------------------------------------------------------------
# EXTRA 1. Competency frequency distribution
# How many competencies dominate the market vs. appear only sporadically?
# ------------------------------------------------------------------------------
freq_dist <- mkt_comp %>%
  filter(N_Offers_Requiring > 0) %>%
  mutate(Freq_Band = case_when(
    N_Offers_Requiring == 1                              ~ "1",
    N_Offers_Requiring >= 2  & N_Offers_Requiring <= 5    ~ "2-5",
    N_Offers_Requiring >= 6  & N_Offers_Requiring <= 20   ~ "6-20",
    N_Offers_Requiring >= 21 & N_Offers_Requiring <= 100  ~ "21-100",
    TRUE                                                  ~ "100+"
  )) %>%
  mutate(Freq_Band = factor(Freq_Band, levels = c("1", "2-5", "6-20", "21-100", "100+"))) %>%
  dplyr::count(Freq_Band, name = "N_Competences") %>%
  mutate(Pct = round(100 * N_Competences / sum(N_Competences), 1))

save_table(freq_dist, "EXTRA_competency_frequency_distribution")

p_freq_dist <- freq_dist %>%
  ggplot(aes(Freq_Band, N_Competences)) +
  geom_col(fill = "#457B9D") +
  geom_text(aes(label = paste0(N_Competences, "\n(", Pct, "%)")), vjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "How many competencies dominate the market vs. appear only rarely?",
       subtitle = "Distinct competencies grouped by number of offers requiring them",
       x = "Number of offers requiring the competency", y = "Number of distinct competencies")
save_plot(p_freq_dist, "EXTRA_frequency_distribution", w = 8, h = 6)

# ------------------------------------------------------------------------------
# EXTRA 2. Heatmap: curriculum coverage vs labour-market demand, per bucket
# ------------------------------------------------------------------------------
bucket_demand <- df_text %>%
  filter(!is.na(Final_Bucket), Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  group_by(Final_Bucket) %>%
  summarise(N_Offers = n_distinct(Job_offer_URL), .groups = "drop") %>%
  # ntile() is rank-based, so it never fails when several buckets tie on
  # N_Offers -- unlike cut(breaks = quantile(...)), which throws "breaks are
  # not unique" in that case and used to silently abort the rest of the script.
  mutate(Demand_Level = factor(dplyr::ntile(N_Offers, 3),
                               levels = 1:3, labels = c("Low", "Medium", "High")))

bucket_coverage <- bucket_sum %>%
  filter(Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  select(Final_Bucket, Pct_Standard) %>%
  mutate(Coverage_Level = cut(Pct_Standard,
                              breaks = c(-Inf, 33.33, 66.67, Inf),
                              labels = c("Low", "Medium", "High")))

heatmap_df <- bucket_demand %>%
  inner_join(bucket_coverage, by = "Final_Bucket") %>%
  pivot_longer(cols = c(Demand_Level, Coverage_Level),
               names_to = "Dimension", values_to = "Level") %>%
  mutate(
    Dimension = recode(Dimension,
                       Demand_Level   = "Labour market\ndemand",
                       Coverage_Level = "IZ1 curriculum\ncoverage"),
    Level = factor(Level, levels = c("Low", "Medium", "High")),
    Label = if_else(Dimension == "Labour market\ndemand",
                    as.character(N_Offers), paste0(round(Pct_Standard, 0), "%"))
  )

save_table(heatmap_df, "EXTRA_heatmap_curriculum_vs_market_data")

p_heatmap <- heatmap_df %>%
  mutate(Final_Bucket = fct_reorder(Final_Bucket, N_Offers)) %>%
  ggplot(aes(Dimension, Final_Bucket, fill = Level)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Label), size = 3, fontface = "bold") +
  scale_fill_manual(values = c(Low = "#E63946", Medium = "#F4A261", High = "#2A9D8F"),
                    name = "Level") +
  labs(title = "Curriculum coverage vs. labour-market demand, by competency category",
       subtitle = "Left = how strongly the market asks for it  |  Right = how well IZ1 already teaches it",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(face = "bold"))
save_plot(p_heatmap, "EXTRA_heatmap_curriculum_vs_market", w = 8, h = 9)

# ------------------------------------------------------------------------------
# EXTRA 3. Coverage Score per bucket
# Coverage Score = share of market demand in that category already matched
# to a curriculum (KEU) code, i.e. bucket_sum$Pct_Standard, presented as its
# own named indicator rather than a side-effect of the Standard/Gap chart.
# ------------------------------------------------------------------------------
coverage_score <- bucket_sum %>%
  filter(Final_Bucket != "OTHER_UNCLASSIFIED") %>%
  transmute(Final_Bucket,
            Coverage_Score_Pct = round(Pct_Standard, 1),
            N_Offers_Standard  = N_Offers_Requiring_Standard,
            N_Offers_Gap       = N_Offers_Requiring_Gap,
            Total_Offers       = Total_Offers_Requiring) %>%
  arrange(Coverage_Score_Pct)

save_table(coverage_score, "EXTRA_coverage_score_by_bucket")

p_coverage <- coverage_score %>%
  mutate(Final_Bucket = fct_reorder(Final_Bucket, Coverage_Score_Pct)) %>%
  ggplot(aes(Coverage_Score_Pct, Final_Bucket, fill = Coverage_Score_Pct)) +
  geom_col() +
  geom_text(aes(label = paste0(Coverage_Score_Pct, "%")), hjust = -0.1, size = 3.2) +
  scale_fill_gradient(low = "#E63946", high = "#2A9D8F", guide = "none") +
  scale_x_continuous(limits = c(0, 110), expand = c(0, 0)) +
  labs(title = "Coverage Score: share of market demand already taught in IZ1",
       subtitle = "Coverage Score = offers matched to a KEU code / total offers requiring that category",
       x = "Coverage score (%)", y = NULL)
save_plot(p_coverage, "EXTRA_coverage_score_by_bucket", w = 9, h = 8)

# ------------------------------------------------------------------------------
# EXTRA 4. Gap Index per bucket
# Gap Index = scaled market demand (0-100) - Coverage Score (%).
# Positive  -> the market is ahead of the curriculum in that category.
# Negative  -> the curriculum already keeps pace with (or exceeds) demand.
# ------------------------------------------------------------------------------
gap_index_df <- bucket_demand %>%
  select(Final_Bucket, N_Offers) %>%
  mutate(Demand_Scaled = round(100 * N_Offers / max(N_Offers), 1)) %>%
  inner_join(bucket_sum %>% select(Final_Bucket, Pct_Standard), by = "Final_Bucket") %>%
  mutate(Gap_Index = round(Demand_Scaled - Pct_Standard, 1)) %>%
  arrange(desc(Gap_Index))

save_table(gap_index_df, "EXTRA_gap_index_by_bucket")

p_gap_index <- gap_index_df %>%
  mutate(Final_Bucket = fct_reorder(Final_Bucket, Gap_Index)) %>%
  ggplot(aes(Gap_Index, Final_Bucket, fill = Gap_Index > 0)) +
  geom_col() +
  geom_text(aes(label = Gap_Index, hjust = if_else(Gap_Index > 0, -0.1, 1.1)), size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = "#E63946", `FALSE` = "#2A9D8F"),
                    labels = c(`TRUE` = "Market ahead of curriculum",
                               `FALSE` = "Curriculum keeps pace"),
                    name = NULL) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Gap Index: scaled market demand minus curriculum coverage",
       subtitle = "Gap Index = scaled demand (0-100) - Coverage Score (%)  |  positive = curricular gap",
       x = "Gap Index", y = NULL) +
  theme(legend.position = "top")
save_plot(p_gap_index, "EXTRA_gap_index_by_bucket", w = 9, h = 8)

# ------------------------------------------------------------------------------
# EXTRA 5. Co-occurrence network among top competencies
# Which competencies tend to be required together in the same posting?
# Restricted to the TOP_N_NETWORK most frequent competencies to keep the
# incidence matrix tractable across several thousand offers.
# Requires igraph + ggraph; if either is missing, the pair table is still
# saved and only the network plot itself is skipped (with a clear message).
# ------------------------------------------------------------------------------
TOP_N_NETWORK <- 25

top_network_names <- mkt_comp %>%
  filter(N_Offers_Requiring > 0) %>%
  arrange(desc(N_Offers_Requiring)) %>%
  slice_head(n = TOP_N_NETWORK) %>%
  pull(Competence_Name)

cooc_long <- df_text %>%
  filter(Competence_Name %in% top_network_names) %>%
  distinct(Job_offer_URL, Competence_Name)

if (n_distinct(cooc_long$Job_offer_URL) >= 2 && n_distinct(cooc_long$Competence_Name) >= 2) {
  
  incidence <- table(cooc_long$Job_offer_URL, cooc_long$Competence_Name)
  cooc_mat  <- t(incidence) %*% incidence
  diag(cooc_mat) <- 0
  
  cooc_edges <- as.data.frame(as.table(cooc_mat)) %>%
    rename(From = Var1, To = Var2, Weight = Freq) %>%
    filter(Weight > 0, as.character(From) < as.character(To)) %>%
    arrange(desc(Weight))
  
  save_table(cooc_edges %>% slice_head(n = 50), "EXTRA_cooccurrence_top_pairs")
  
  cat("Top 10 co-occurring competency pairs:\n")
  print(cooc_edges %>% slice_head(n = 10))
  
  has_igraph <- requireNamespace("igraph", quietly = TRUE)
  has_ggraph <- requireNamespace("ggraph", quietly = TRUE)
  
  if (has_igraph && has_ggraph) {
    edges_for_graph <- cooc_edges %>% filter(Weight >= quantile(Weight, 0.5))
    g <- igraph::graph_from_data_frame(edges_for_graph, directed = FALSE)
    
    p_network <- ggraph::ggraph(g, layout = "fr") +
      ggraph::geom_edge_link(aes(width = Weight), color = "grey70", alpha = 0.6) +
      ggraph::geom_node_point(size = 6, color = "#2A9D8F") +
      ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      ggraph::scale_edge_width(range = c(0.3, 3), name = "Co-occurrences") +
      labs(title = "Co-occurrence network of top competencies",
           subtitle = paste0("Top ", TOP_N_NETWORK,
                             " competencies by demand  |  edge width = offers mentioning both")) +
      theme_void()
    save_plot(p_network, "EXTRA_cooccurrence_network", w = 11, h = 9)
  } else {
    message("EXTRA: igraph/ggraph not installed -- skipping network plot ",
            "(install.packages(c('igraph','ggraph')) to enable it). ",
            "The co-occurrence pair table was still saved.")
  }
} else {
  message("EXTRA: not enough overlapping data to build a co-occurrence network -- skipping.")
}

# ------------------------------------------------------------------------------
# EXTRA 6. Reliability -- manual-validation scaffold
# First run: draws a random sample of competency classifications and writes
# it to disk with a blank Manual_Agree column for hand-coding.
# Once you fill it in (TRUE/FALSE per row) and save it as
# "manual_validation_completed.csv" in the working directory, re-running this
# script computes and reports the agreement percentage automatically.
# ------------------------------------------------------------------------------
RELIABILITY_SAMPLE_FILE    <- "outputs/tables/RELIABILITY_manual_validation_sample.csv"
RELIABILITY_COMPLETED_FILE <- "manual_validation_completed.csv"

if (!file.exists(RELIABILITY_COMPLETED_FILE)) {
  set.seed(123)
  reliability_sample <- df_text %>%
    filter(!is.na(Competence_Name)) %>%
    distinct(Competence_Name, Matched_Code, Status, Final_Bucket) %>%
    slice_sample(n = min(100, n())) %>%
    mutate(Manual_Agree = NA_character_)  # fill in TRUE / FALSE by hand
  
  write_excel_csv(reliability_sample, RELIABILITY_SAMPLE_FILE)
  message("EXTRA: reliability sample (n=", nrow(reliability_sample), ") saved to ",
          RELIABILITY_SAMPLE_FILE, ". Fill in 'Manual_Agree' (TRUE/FALSE) by hand, save the ",
          "completed file as '", RELIABILITY_COMPLETED_FILE,
          "' in the working directory, and re-run this script to get the agreement statistic.")
} else {
  manual_check <- read_csv(RELIABILITY_COMPLETED_FILE, show_col_types = FALSE) %>%
    filter(!is.na(Manual_Agree))
  
  n_checked <- nrow(manual_check)
  n_agree   <- sum(toupper(as.character(manual_check$Manual_Agree)) == "TRUE")
  pct_agree <- round(100 * n_agree / n_checked, 1)
  
  cat("Manual validation: ", n_agree, "/", n_checked, " (", pct_agree,
      "%) of AI classifications confirmed correct on manual review.\n", sep = "")
  
  reliability_summary <- tibble(Metric = c("N_Checked", "N_Agree", "Pct_Agree"),
                                Value  = c(n_checked, n_agree, pct_agree))
  save_table(reliability_summary, "EXTRA_reliability_validation_summary")
}

# ==============================================================================
# FINAL SUMMARY PRINTED TO CONSOLE
# ==============================================================================
cat(SEP)
cat("FINAL SUMMARY -- ready to quote in thesis\n")
cat(strrep("=", 70), "\n")
cat("Job offers analysed:                        ", n_offers_total, "\n")
cat("Distinct Text_Analysis competency names:    ", n_distinct(df_text$Competence_Name), "\n")
cat("Consolidated canonical competencies (top6): ", n_distinct(top6$canonical_name), "\n")
cat("\nRQ1 -- Pareto:\n")
cat("  Gini coefficient:                         ", round(gini_coef, 3), "\n")
cat("  % competencies covering 80% of demand:    ", pct_for_80, "%\n")
cat("\nRQ2 -- Network validation:\n")
cat("Nodes:                 ", network_metrics$Nodes, "\n")
cat("Edges:                 ", network_metrics$Edges, "\n")
cat("Density:               ", round(network_metrics$Density,4), "\n")
cat("Average degree:        ", round(network_metrics$Avg_Degree,2), "\n")
cat("Average clustering:    ", round(network_metrics$Avg_Clustering,3), "\n")
cat("Communities (Louvain): ", network_metrics$Connected_Components, "\n")
cat("Modularity:            ", round(network_metrics$Modularity,3), "\n")
if (exists("prec")) {
  cat("\nRQ3 -- Lexical vs semantic:\n")
  cat("  Precision:                                ", round(prec, 3), "\n")
  cat("  Recall:                                   ", round(rec, 3), "\n")
  cat("  F1:                                       ", round(f1, 3), "\n")
  cat("  Competencies missed by lexical matching:  ", FN, "\n")
}
cat("\nRQ4 -- Curriculum coverage:\n")
cat("  Standard (covered):                       ",
    rq4_overall$Pct[rq4_overall$Status == "Standard"], "%\n")
cat("  Gap (not covered):                        ",
    rq4_overall$Pct[rq4_overall$Status == "Gap"], "%\n")
cat("\nRQ5 -- Top gap:\n")
cat("  Biggest unmet competency:                 ",
    rq5_gaps$Competence_Name[1], " (", rq5_gaps$N_Offers_Requiring[1], " offers)\n")
if (!is.na(auc_val)) {
  cat("\nRQ6 -- ML:\n")
  cat("  Logistic regression AUC:                  ", round(auc_val, 3), "\n")
}
cat("\nAI/API saturation:\n")
cat("  Offers mentioning AI/API wording:         ",
    n_offers_ai, "(", round(100*n_offers_ai/n_offers_total,1), "% of all offers)\n")
if (exists("coverage_score")) {
  cat("\nEXTRA -- Coverage Score (lowest 3 buckets):\n")
  print(coverage_score %>% slice_head(n = 3))
}
if (exists("gap_index_df")) {
  cat("\nEXTRA -- Gap Index (top 3 widest gaps):\n")
  print(gap_index_df %>% slice_head(n = 3))
}
if (exists("freq_dist")) {1
  cat("\nEXTRA -- Competency frequency distribution:\n")
  print(freq_dist)
}
cat(strrep("=", 70), "\n")
message("\nAll plots  -> outputs/plots/")
message("All tables -> outputs/tables/")



list.files("outputs/plots")