#---------------------------------FINAL VERSION ---------------------------------
library(tidyverse)
library(rvest)
library(glue)
library(httr)

# --- PAGE CONFIGURATION ---
limit_pages_biznes <- 171
limit_pages_it     <- 29 

my_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

url_biznes <- "https://www.pracuj.pl/praca?cc=5008%2C5011%2C5013%2C5017001%2C5018%2C5034001"
url_it     <- "https://it.pracuj.pl/praca?its=agile%2Cai-ml%2Cbig-data-science%2Cbusiness-analytics%2Cdata-analytics-and-bi%2Cproduct-management%2Cproject-management%2Csystem-analytics"

# Output file - also used for resuming an interrupted scrape
output_file <- "data_before_cleaning.csv"

all_links_biznes <- list()
all_links_it <- list()

message("--- STAGE 1: COLLECTING LINKS AND COMPANY NAMES FROM LISTINGS ---")

# ==========================================
# PART A: SCRAPING FROM BUSINESS CATEGORIES
# ==========================================
message(">> Starting Business & Management categories...")
for (i in 1:limit_pages_biznes) {
  url <- if(i == 1) url_biznes else glue("{url_biznes}&pn={i}")
  message("Processing Business page: ", i, " of ", limit_pages_biznes)
  res <- GET(url, add_headers(`User-Agent` = my_agent))
  
  if(status_code(res) == 200) {
    page <- read_html(res)
    offer_cards <- page %>% html_elements("[data-test='section-offer'], [data-test='default-offer']")
    
    all_links_biznes[[i]] <- map_df(offer_cards, function(card) {
      link <- card %>% html_element("a[data-test='link-offer']") %>% html_attr("href")
      comp <- card %>% html_element("[data-test='text-company-name']") %>% html_text(trim = TRUE)
      if(!is.na(link)) return(tibble(offer_url = link, company_name = comp, source = "Biznes"))
      return(NULL)
    })
  }
  Sys.sleep(runif(1, 1, 1.5))
}

# ==========================================
# PART B: SCRAPING FROM IT CATEGORIES
# ==========================================
message(">> Starting IT & Analytics categories...")
for (i in 1:limit_pages_it) {
  url <- if(i == 1) url_it else glue("{url_it}&pn={i}")
  message("Processing IT page: ", i, " of ", limit_pages_it)
  res <- GET(url, add_headers(`User-Agent` = my_agent))
  
  if(status_code(res) == 200) {
    page <- read_html(res)
    offer_cards <- page %>% html_elements("[data-test='section-offer'], [data-test='default-offer']")
    
    all_links_it[[i]] <- map_df(offer_cards, function(card) {
      link <- card %>% html_element("a[data-test='link-offer']") %>% html_attr("href")
      comp <- card %>% html_element("[data-test='text-company-name']") %>% html_text(trim = TRUE)
      if(!is.na(link)) return(tibble(offer_url = link, company_name = comp, source = "IT"))
      return(NULL)
    })
  }
  Sys.sleep(runif(1, 1, 1.5))
}

# Combine lists and remove duplicates within categories
df_biznes <- bind_rows(all_links_biznes) %>% distinct(offer_url, .keep_all = TRUE) %>% filter(!is.na(offer_url))
df_it     <- bind_rows(all_links_it)     %>% distinct(offer_url, .keep_all = TRUE) %>% filter(!is.na(offer_url))

# Combine into one final list for detailed scraping - ALL offers found on the listing pages above
links_to_scrape <- bind_rows(df_biznes, df_it)

message("\nTotal unique offers found: ", nrow(links_to_scrape),
        " (", nrow(df_biznes), " from Biznes, ", nrow(df_it), " from IT)")

# --- RESUME: skip offers we already scraped in a previous run ---
if (file.exists(output_file)) {
  already_scraped <- read_csv(output_file, show_col_types = FALSE)$url
  n_before <- nrow(links_to_scrape)
  links_to_scrape <- links_to_scrape %>% filter(!offer_url %in% already_scraped)
  message("Found existing ", output_file, " - skipping ", n_before - nrow(links_to_scrape), " offers already scraped.")
}

message("Offers to scrape this run: ", nrow(links_to_scrape))

# -----------------------
# STAGE 2: DETAILED SCRAPING
# -----------------------
offers_data <- list()
message("\n--- STAGE 2: EXTRACTING DETAILED JOB DATA ---")

save_every <- 50          # how often we checkpoint to disk
last_saved <- 0            # index up to which offers_data has been written
consecutive_failures <- 0  # tracks failed requests in a row, in case we get blocked

for(j in seq_len(nrow(links_to_scrape))){
  url  <- links_to_scrape$offer_url[j]
  comp_from_list <- links_to_scrape$company_name[j] 
  source_from_list <- links_to_scrape$source[j]
  
  cat(sprintf("\rOverall Progress: [%d/%d] - %.1f%%", j, nrow(links_to_scrape), (j/nrow(links_to_scrape))*100))
  
  tryCatch({
    res_offer <- GET(url, add_headers(`User-Agent` = my_agent))
    
    if(status_code(res_offer) == 200) {
      page <- read_html(res_offer)
      
      title  <- page %>% html_element("h1") %>% html_text(trim = TRUE)
      salary <- page %>% html_element("div[data-test='section-salary']") %>% html_text(trim = TRUE)
      
      badges_raw <- page %>% 
        html_elements("div[data-test='sections-benefit-list'] li, [data-test='offer-badge-title']") %>% 
        html_text(trim = TRUE) %>% unique() %>% paste(collapse = " | ")
      
      # Retained 1:1 - Scraping hard technology badges for your tech stack
      tech_req <- page %>% html_elements("div[data-test='section-technologies-expected'] li") %>% 
        html_text(trim = TRUE) %>% paste(collapse = ", ")
      
      tech_opt <- page %>% html_elements("div[data-test='section-technologies-optional'] li") %>% 
        html_text(trim = TRUE) %>% paste(collapse = ", ")
      
      # Combined wall of text (Responsibilities + Requirements) for AI competence analysis
      resp_text <- page %>% html_elements("section[data-test='section-responsibilities'] li, section[data-test='section-responsibilities']") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
      reqs_text <- page %>% html_elements("section[data-test='section-requirements'] li, section[data-test='section-requirements']") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
      
      ai_text_payload <- paste("OBOWIĄZKI:", resp_text, "WYMAGANIA:", reqs_text)
      
      # Fallback container in case the employer uses a custom layout
      if(str_length(str_squish(ai_text_payload)) < 30) {
        ai_text_payload <- page %>% html_element("article, div[data-test='section-job-description']") %>% html_text(trim = TRUE)
      }
      
      offers_data[[j]] <- tibble(
        url = url,
        title = title,
        company = comp_from_list,
        source = source_from_list,
        salary = salary,
        badges = badges_raw,
        tech_mandatory = tech_req,
        tech_nice_to_have = tech_opt,
        full_requirements = ai_text_payload
      )
      
      consecutive_failures <- 0
      
    } else {
      message("\nNon-200 status (", status_code(res_offer), ") at URL: ", url)
      consecutive_failures <- consecutive_failures + 1
    }
  }, error = function(e) { 
    message("\nError at URL: ", url, " - ", e$message)
    consecutive_failures <<- consecutive_failures + 1
  })
  
  # Checkpoint: append newly scraped offers to disk so we don't lose progress
  if (j %% save_every == 0 || j == nrow(links_to_scrape)) {
    new_rows <- bind_rows(offers_data[(last_saved + 1):j])
    if (nrow(new_rows) > 0) {
      write_csv(new_rows, output_file, append = file.exists(output_file))
    }
    last_saved <- j
  }
  
  # Stop early if we look blocked instead of grinding through failures for hours
  if (consecutive_failures >= 10) {
    message("\nStopping: 10 failed requests in a row, we might be blocked. Re-run later to resume.")
    break
  }
  
  Sys.sleep(runif(1, 1.5, 2.5)) 
}

final_df <- bind_rows(offers_data)

message("\n\n[SUCCESS] ", nrow(final_df), " offers scraped this run and saved to '", output_file, "' (checkpointed during the run).")


# ---------------------------------------------------------------------
# DUPLICATE CONTROL
# ---------------------------------------------------------------------
url_duplicates <- final_df %>%
  group_by(url) %>%
  filter(n() > 1) %>%
  summarise(count = n())

content_duplicates <- final_df %>%
  group_by(title, company) %>%
  filter(n() > 1) %>%
  arrange(title)

message("Number of identical URLs found: ", nrow(url_duplicates))
message("Number of duplicate job titles from the same company: ", nrow(content_duplicates))

if(nrow(content_duplicates) > 0) {
  View(content_duplicates)
}



# Checking NA
#dane <- read_csv("data_before_cleaning.csv")

# 2. Preview the structure and column types
#glimpse(dane)

# 3. Check for missing values (NA) in each column
# This helps evaluate if your CSS/XPath selectors returned empty fields
#dane %>%
  #summarise(across(everything(), ~ sum(is.na(.)))) %>%
  #pivot_longer(cols = everything(), names_to = "Column", values_to = "NA_Count"))