source("C:/Users/boul24/OneDrive - HealthBC/SQI - LB/Tarmac Triage/tarmac_dash/R/fct_write_site_csvs.R")

library(testthat)

test_that("write_site_csvs writes one file per site", {
  df <- data.frame(
    facility_name = c("SiteA", "SiteB", "SiteA"),
    val = 1:3
  )

  out_dir <- tempfile()
  write_site_csvs(df, out_dir)

  files <- list.files(out_dir, pattern = "\\.csv$")
  expect_setequal(files, c("SiteA.csv", "SiteB.csv"))
})
