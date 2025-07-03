# R/fct_upload_to_supabase.R

upload_to_supabase <- function(df, conn) {
  DBI::dbWriteTable(conn, "ed_arrivals", df, append = TRUE, row.names = FALSE)
}