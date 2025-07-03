# R/fct_write_site_csvs.R

write_site_csvs <- function(df, output_dir = "app_data/ed") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  split(df, df$site_name) %>%
    purrr::iwalk(function(site_df, site_name) {
      outfile <- file.path(output_dir, paste0(site_name, ".csv"))
      readr::write_csv(site_df, outfile)
    })
}
