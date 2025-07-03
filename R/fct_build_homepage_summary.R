#' build_homepage_summary 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# R/fct_build_homepage_summary.R

# R/fct_build_homepage_summary.R

build_homepage_summary <- function(data_dir = "app_data/ed", output_path = "app_data/homepage_summary.csv") {
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  
  all_ed <- purrr::map_dfr(files, function(file) {
    df <- tryCatch(readr::read_csv(file, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(df)) {
      df <- janitor::clean_names(df)
      if ("lha" %in% names(df)) {
        df <- df %>% dplyr::mutate(lha = as.character(lha),
                                   triage_level = as.character(triage_level),
                                   health_care_number = as.character(health_care_number),
                                   ctas_level = as.character(ctas_level),
                                   hsda = as.character(hsda))
      }
      df$facility_name <- tools::file_path_sans_ext(basename(file))
    }
    df
  })
  
  all_ed <- all_ed %>%
    dplyr::mutate(
      arrival_datetime = lubridate::ymd_hms(arrival_datetime),
      arrival_month = format(arrival_datetime, "%Y-%m")
    )
  
  # Summary for homepage: total visits, avg CTAS, most recent arrival, tarmac count (12 months)
  last_12_months <- lubridate::today() %m-% months(12)
  
  dfTarmac <-  all_ed %>%
    dplyr::group_by(facility_name) %>%
    tarmacFilter()
  
  summary <- dfTarmac |>
    dplyr::summarise(
      total_visits = dplyr::n(),
      tarmac_12mo = sum(arrival_datetime >= last_12_months, na.rm = TRUE),
      .groups = "drop"
    )
  
  readr::write_csv(summary, output_path)
  
  # Build all possible combinations
  all_months <- seq(
    from = lubridate::floor_date(Sys.Date() %m-% months(12), "month"),
    to = lubridate::floor_date(Sys.Date(), "month"),
    by = "1 month"
  ) %>% format("%Y-%m")
  
  all_sites <- unique(dfTarmac$facility_name)
  
  # Create full combination grid
  full_grid <- expand.grid(
    facility_name = all_sites,
    arrival_month = all_months,
    stringsAsFactors = FALSE
  )
  
  # Actual counts
  monthly_counts <- dfTarmac %>%
    dplyr::mutate(arrival_month = format(arrival_datetime, "%Y-%m")) %>%
    dplyr::count(facility_name, arrival_month)
  
  # Merge with full grid and fill missing combos with zero
  monthly_counts_complete <- full_grid %>%
    dplyr::left_join(monthly_counts, by = c("facility_name", "arrival_month")) %>%
    dplyr::mutate(
      n = tidyr::replace_na(n, 0),
      date = lubridate::ym(arrival_month)
    ) %>%
    dplyr::filter(date >= last_12_months)
  
  
  
  
  readr::write_csv(monthly_counts_complete, "app_data/monthly_counts_by_site.csv")
  
  return(list(summary = summary, monthly_counts = monthly_counts))
}
