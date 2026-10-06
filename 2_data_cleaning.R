# ==============================================================================
# STAGE 1: Production-Ready Market Data Cleaning & Feature Selection
# Purpose: Focused curriculum gap analysis for University Management Engineering
# Input: data_before_cleaning.csv
# Output: cleaned_market_data.csv
# ==============================================================================

library(tidyverse)
library(stringr)

# Check if input file exists
if (!file.exists("data_before_cleaning.csv")) {
  stop("Input file 'data_before_cleaning.csv' missing!")
}

# Load data (Remove 'head(6)' when running on the entire 12k dataset)
raw_data <- read_csv("data_before_cleaning.csv", show_col_types = FALSE)

print("Starting production-scale data cleaning process...")

# Exchange rates for foreign currency normalization (2026 average references)
exchange_rate_eur <- 4.35
exchange_rate_usd <- 3.95

# Execute clean, targeted feature selection and normalization
cleaned_data <- raw_data %>%
  # Filter out test rows and missing job titles
  filter(!str_detect(tolower(title), "testowa"), !is.na(title)) %>%
  mutate(
    # 1. Clean and normalize salary text structure
    salary_norm = str_replace_all(salary, "([a-zA-Z])([0-9])", "\\1 \\2"),
    salary_norm = str_replace_all(salary_norm, ",", "."),
    salary_norm = str_replace_all(salary_norm, "[—–-]", "-"),
    salary_norm = str_remove_all(salary_norm, "zł|(?i)vat|\\(\\s*\\+\\s*vat\\)|mies\\.|/ mies\\."),
    salary_norm = str_squish(salary_norm),
    
    # Detect currency types
    is_eur = str_detect(tolower(salary_norm), "eur|€"),
    is_usd = str_detect(tolower(salary_norm), "usd|\\$"),
    
    # Detect if the rate is hourly
    is_hourly = str_detect(tolower(salary_norm), "godz|/h|/ hr|hr\\."),
    
    # 2. Extract contract keywords anywhere in the string to route data correctly
    is_b2b_type = str_detect(tolower(salary_norm), "net|b2b"),
    is_uop_type = str_detect(tolower(salary_norm), "brutto|gross|prace|zlecenie|employment|mandate"),
    
    # 3. Robust Layer: Clean all spaces and grab only valid mathematical numbers
    clean_digits_only = str_replace_all(salary_norm, " ", ""),
    extracted_numbers = str_extract_all(clean_digits_only, "[0-9]+(\\.[0-9]+)?"),
    
    # Extract baseline minimum and maximum safely using map_dbl
    extracted_min = map_dbl(extracted_numbers, function(x) if(length(x) >= 1) as.numeric(x[1]) else NA_real_),
    extracted_max = map_dbl(extracted_numbers, function(x) if(length(x) >= 2) as.numeric(x[2]) else NA_real_),
    
    # Apply single-rate fallback (if no range exists, max equals min)
    has_range = str_detect(salary_norm, "-|(?i)\\bdo\\b"),
    extracted_max = ifelse(has_range & !is.na(extracted_max), extracted_max, extracted_min),
    
    # 4. Route extracted values to appropriate contract variables
    uop_min = ifelse(is_uop_type | (!is_b2b_type & !is_uop_type), extracted_min, NA_real_),
    uop_max = ifelse(is_uop_type | (!is_b2b_type & !is_uop_type), extracted_max, NA_real_),
    b2b_min = ifelse(is_b2b_type, extracted_min, NA_real_),
    b2b_max = ifelse(is_b2b_type, extracted_max, NA_real_),
    
    # 5. Operational Math: Convert hourly rates to standard monthly values (168h) FIRST
    uop_min = ifelse(is_hourly & !is.na(uop_min) & uop_min < 1000, uop_min * 168, uop_min),
    uop_max = ifelse(is_hourly & !is.na(uop_max) & uop_max < 1000, uop_max * 168, uop_max),
    b2b_min = ifelse(is_hourly & !is.na(b2b_min) & b2b_min < 1000, b2b_min * 168, b2b_min),
    b2b_max = ifelse(is_hourly & !is.na(b2b_max) & b2b_max < 1000, b2b_max * 168, b2b_max),
    
    # 6. Currency Layer: Convert foreign salary inputs into PLN baseline values SECOND
    uop_min = case_when(is_eur ~ uop_min * exchange_rate_eur, is_usd ~ uop_min * exchange_rate_usd, TRUE ~ uop_min),
    uop_max = case_when(is_eur ~ uop_max * exchange_rate_eur, is_usd ~ uop_max * exchange_rate_usd, TRUE ~ uop_max),
    b2b_min = case_when(is_eur ~ b2b_min * exchange_rate_eur, is_usd ~ b2b_min * exchange_rate_usd, TRUE ~ b2b_min),
    b2b_max = case_when(is_eur ~ b2b_max * exchange_rate_eur, is_usd ~ b2b_max * exchange_rate_usd, TRUE ~ b2b_max),
    
    # 7. Safety Net: Filter out structural anomalies only after all math calculations are complete
    uop_min = ifelse(uop_min < 3500 | uop_min > 150000, NA_real_, uop_min),
    uop_max = ifelse(uop_max < 3500 | uop_max > 150000, NA_real_, uop_max),
    b2b_min = ifelse(b2b_min < 3500 | b2b_min > 150000, NA_real_, b2b_min),
    b2b_max = ifelse(b2b_max < 3500 | b2b_max > 150000, NA_real_, b2b_max),
    
    # 8. Meta Feature: Cascading Seniority Extraction (Robust algorithm for 12k rows)
    seniority = case_when(
      # Condition 1: Executive and Director positions (Highest priority in title)
      str_detect(tolower(title), "director|dyrektor|head of|vp|ceo|cto|cfo|board") ~ "Director / Executive",
      
      # Condition 2: Management and Leadership roles (Operational & team management)
      str_detect(tolower(title), "manager|menedżer|kierownik|kierowniczka|koordynator|leader|lead|lider") ~ "Lead / Management",
      
      # Condition 3: Juniors / Interns / Trainees (Extracted from both fields)
      str_detect(tolower(title), "junior|młodszy|intern|staż|praktyk|assistant|asystent") |
        str_detect(tolower(badges), "junior|młodszy|intern|staż|praktyk|asystent") ~ "Junior",
      
      # Condition 4: Seniors / Experts / Principal Specialists
      str_detect(tolower(title), "senior|starszy|expert|ekspert|główny|principal|lead developer") |
        str_detect(tolower(badges), "senior|starszy|expert|ekspert") ~ "Senior",
      
      # Condition 5: Mid / Regular (Only if explicit evidence exists in title or badges)
      str_detect(tolower(title), "mid|regular|specjalista|specialist|inżynier|engineer|consultant|konsultant") |
        str_detect(tolower(badges), "mid|specialist|specjalista") ~ "Mid",
      
      # Condition 6: Fallback for listings with absolutely zero seniority indicators
      TRUE ~ "Not Specified"
    ),
    # 9. Advanced Text Deduplication and Cleaning (Fixes duplication and preserves acronyms like KSeF)
    full_requirements_fixed = map_chr(full_requirements, function(text) {
      if (is.na(text)) return(NA_character_)
      
      # Split the text into OBOWIĄZKI and WYMAGANIA blocks
      parts <- str_split(text, "WYMAGANIA:")[[1]]
      if (length(parts) < 2) return(text)
      
      obow_part <- str_remove(parts[1], "OBOWIĄZKI:")
      reqs_part <- parts[2]
      
      # Internal helper to detect duplication and keep only the clean, spaced second copy
      extract_clean_half <- function(block) {
        block <- str_squish(block)
        n <- nchar(block)
        if (n < 40) return(block)
        
        # Take the last 20 characters as a unique signature of the text end
        tail_str <- str_sub(block, -20)
        
        # Find where this signature first appears (it marks the end of the duplicated first copy)
        first_pos <- str_locate(block, fixed(tail_str))[1,1]
        
        # If it appears earlier, the pristine second copy starts right after it
        if (!is.na(first_pos) && first_pos < (n - 20)) {
          split_point <- first_pos + 19
          return(str_squish(str_sub(block, split_point + 1)))
        }
        return(block)
      }
      
      paste("OBOWIĄZKI:", extract_clean_half(obow_part), "WYMAGANIA:", extract_clean_half(reqs_part))
    })
  ) %>%
  select(
    Job_title = title,
    Company_name = company,
    Seniority_level = seniority,
    Raw_salary_text = salary,       
    Emp_min_gross = uop_min,
    Emp_max_gross = uop_max,
    Net_B2B_min = b2b_min,
    Net_B2B_max = b2b_max,
    Mandatory_tech = tech_mandatory,
    Nice_to_have_tech = tech_nice_to_have,
    Full_requirements = full_requirements_fixed,
    Job_offer_URL = url
  )

write_excel_csv(cleaned_data, "cleaned_data.csv")
print("Stage 1 completed successfully! Absolute fallback logic applied.")

# ==============================================================================
# VISUAL VERIFICATION REPL
# ==============================================================================
cleaned_data <- read_csv("cleaned_data.csv")
View(cleaned_data)
head(cleaned_data$Full_requirements, 2)

