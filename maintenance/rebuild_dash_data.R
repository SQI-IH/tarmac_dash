# maintenance/rebuild_site_month_csvs.R

# DO NOT SOURCE FROM THE APP.
# This is a standalone script for regenerating historical site-level data from raw exports.

library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(janitor)

source("R/fct_clean_ed_data.R")

input_dir <- "C:/Users/boul24/OneDrive - HealthBC/SQI - LB/Tarmac Triage/ed_data_processor/ed_data"
output_dir <- "C:/Users/boul24/OneDrive - HealthBC/SQI - LB/Tarmac Triage/tarmac_dash/app_data/ed"
backup_dir <- paste0(output_dir, "_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))

# Step 1: Back up existing output files
if (dir.exists(output_dir)) {
  message("📦 Backing up existing files to: ", backup_dir)
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(output_dir, full.names = TRUE), backup_dir)
} else {
  dir.create(output_dir, recursive = TRUE)
}

# Step 2: Load and clean raw data
raw_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
raw_df <- map_dfr(raw_files, ~{
  df <- read_csv(.x, show_col_types = FALSE)
  df <- janitor::clean_names(df)
  if ("ctas_level" %in% names(df)) {
    df <- df %>% mutate(ctas_level = as.character(ctas_level))
  }
  df %>% mutate(across(everything(), as.character))
})

# Rename fallback if site_name is missing but facility_name exists
if (!"site_name" %in% names(raw_df) && "facility_name" %in% names(raw_df)) {
  raw_df <- raw_df %>% rename(site_name = facility_name)
}
print("🧾 Sample column names from cleaned data:")
print(names(raw_df))

# Step 3: Validate key columns
required_cols <- c("ed_arrival_date_time", "site_name")
missing <- setdiff(required_cols, names(raw_df))
if (length(missing) > 0) {
  stop("❌ Missing required columns: ", paste(missing, collapse = ", "))
}



# Step 4: Split by site and write one complete file per site
by_site <- split(raw_df, raw_df$site_name)

for (site in names(by_site)) {
  site_df <- by_site[[site]]
  site_path <- file.path(output_dir, paste0(site, ".csv"))
  
  # Skip file if it already exists
  if (file.exists(site_path)) {
    message("⏭️ Skipping existing file: ", site_path)
    next
  }
  
  clean_site <- clean_ed_data(site_df)
  write_csv(clean_site, site_path)
  message("✅ Wrote full dataset for: ", site_path)
}

message("🎉 Rebuild complete.")
