source("C:/Users/boul24/OneDrive - HealthBC/SQI - LB/Tarmac Triage/tarmac_dash/R/fct_upload_to_supabase.R")

library(testthat)

test_that("upload_to_supabase writes to the database", {
  skip("Requires a working database connection")

  conn <- create_db_connection()
  test_df <- data.frame(
    site_name = "SiteX",
    val = 123
  )

  expect_silent(upload_to_supabase(test_df, conn))
  DBI::dbDisconnect(conn)
})
