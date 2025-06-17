#' Create a database connection to Supabase
#' @return A DBI connection
#' @export
#' @name utils_supabase


create_db_connection <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_PORT")),
    dbname = Sys.getenv("SUPABASE_DB"),
    user = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PW"),
    sslmode = "require"
  )
}


library(DBI)
library(RPostgres)

# read old data into database ---------------------------------------------
loadToSupabase <- function(){
  con <- dbConnect(
  RPostgres::Postgres(),
  host = "db.qxxyrajzayjicecbphow.supabase.co",
  dbname = "postgres",
  user = "postgres",
  password = "$cvh^Xq6%mXaX8@cNzxN",
  port = 5432,
  sslmode = "require"
)

# List all CSV files in the folder
csv_files <- list.files(path = "./app_data/ed", pattern = "\\.csv$", full.names = TRUE)

combined_df <- csv_files |>
  purrr::set_names() |>
  purrr::map_dfr(
    ~ readr::read_csv(.x, col_types = readr::cols(.default = "c")),
    .id = "source_file"
  )

dbWriteTable(con, "ed_arrivals", combined_df, overwrite = TRUE)

# Check it worked
dbListTables(con)
dbReadTable(con, "ed_arrivals") |> head()

# Disconnect when done
dbDisconnect(con)
}


