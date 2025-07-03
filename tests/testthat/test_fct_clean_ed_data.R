source("C:/Users/boul24/OneDrive - HealthBC/SQI - LB/Tarmac Triage/tarmac_dash/R/fct_clean_ed_data.R")

library(testthat)
test_that("clean_ed_data drops sensitive fields and hashes account_number", {
  raw_df <- data.frame(
    account_number = c("123", "456", "123"),
    ed_arrival_date_time = c("2025-01-01 12:00:00", "2025-01-02 13:30:00", "2025-01-01 12:00:00"),
    disposition = c("A", "B", "A"),
    diagnosis = c("X", "Y", "X"),
    site_name = c("Kamloops", "Kamloops", "Kamloops")
  )
  
  cleaned <- clean_ed_data(raw_df)
  
  expect_false("account_number" %in% names(cleaned))
  expect_true("account_number_hash" %in% names(cleaned))
  expect_equal(nrow(cleaned), 2)
})
