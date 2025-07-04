#' Load and clean ED data from all sites
#' @export
#' @name mod_tarmac_fct_display

tarmacFilter <- function(df) {
  df %>%
    dplyr::filter(
      stringr::str_detect(stringr::str_to_lower(discharge_comment),
                          stringr::regex("t[ar]{1,2}m[ac]{1,2}", ignore_case = TRUE)),
      !stringr::str_detect(discharge_comment, "Tamara"),
      !stringr::str_detect(discharge_comment, "tramadol"),
      !stringr::str_detect(discharge_comment, "Tramacet")
    )
}


tarmac_boxUI <- function(ns){
  renderUI({
    fluidRow(
      # box(
      #   title = "Value Card",
      #   width = 4,
      #   status = "primary",
      #   solidHeader = TRUE,
      #   collapsible = TRUE,
      #   closable = FALSE,
      #   label = tagList(icon("info-circle"), "Dynamic"),
      valueBoxOutput(ns("tarmacs_per_year"), width = 2)
      # )
    )
  })
  
}

summaryBarPlot <- function(dfT, color_palette = NULL) {
  df <- read.csv("app_data/monthly_counts_by_site.csv")
  # Determine the last 12 months based on your latest data
latest_month <- max(ym(df$arrival_month))
all_months <- seq(latest_month %m-% months(11), latest_month, by = "month")

  dfProf <- profileLoad() 
  dfPlot <- dfProf |>
    select(`Data Code`, City) |>
    right_join(df, by = c("Data Code" = "facility_name")) |>
    # tidyr::pivot_longer(
    #   cols = -c(City, `Data Code`),
    #   names_to = "month",
    #   values_to = "count"
    # ) |>
    mutate(year_month = format(ym(arrival_month), "%b %Y"))

  all_months <- sort(my(unique(dfPlot$year_month)))
  
  # Ensure year_month is treated as a factor ordered by date
  summary_data <- dfPlot %>%
    mutate(year_month = factor(year_month, levels = format(all_months, "%b %Y")))
  
  # If no custom color palette is provided, use a default one
  if (is.null(color_palette)) {
    color_palette <- RColorBrewer::brewer.pal(length(unique(summary_data$City)), "Set3")
  }
  
  city_cols  <-
    c(
      "Castlegar"=  "#1f77b4", # blue
      "Sparwood" =  "#f5c647",
      "Chase" = "#17becf",  # cyan,
      "Keremeos" = "#F15A25"
    )
  
  

  # Plotly stacked bar chart with custom color palette
  summary_data <- summary_data %>%
    filter(City %in% names(city_cols))
  x <- summary_data |> group_by(date) |> summarize(n = sum(n)) 
  
  summary_data |>
    plotly::plot_ly(
      x = ~year_month,
      y = ~n,
      color = ~City,
      colors = city_cols,  # Apply the custom color palette
      type = "bar",
      text = ~paste(year_month, "<br>Community:", City, "<br>Count:", n),
      textposition = "none",            # <- suppress always-visible labels
      hoverinfo = "text"                # <- enable them on hover only
    ) %>%
    plotly::layout(
      barmode = "stack",  # Stack the bars instead of grouping
      xaxis = list(
        title = "",
        tickformat = "%b %Y",  # Month abbreviation and year
        tickangle = -45,       # Optional: Rotate labels for better visibility
        showticks = TRUE,      # Show tick marks
        ticks = "inside"       # Optional: Put the ticks inside the plot area
      ),
      yaxis = list(
        title = "Event Count",
        tickmode = "array",    # Ensure only integer breaks
        tickvals = seq(0, max(x$n), by = 1)  # Integer tick values
      ),
      title = "",
      legend = list(
        title = list(text = ""), # Optionally change legend title
        tracegroupgap = 5 # Optional: Adjust spacing between groups in the legend
      )
    )
  
}

allEd <- function(site = "All Tarmac Sites") {
  dfProf <- profileLoad()
  
  cities <- if (site == "All Tarmac Sites") {
    dfProf %>%
      filter(`Active Tarmac` == "Yes") %>%
      pull(City)
  } else {
    site
  }
  paths <- filenameCreate(dfProf, cities)
  purrr::map_dfr(paths, ~data.table::fread(.x, colClasses = "character")) %>%
    mutate(datetime = parse_ed_time(arrival_datetime),
           date = as.Date(datetime),
           weekdate = weekdate(date),
           Hour = hour(datetime),
           time = format(datetime, "%H:%M:%S"),
           Year = year(date))
  
}

profileLoad <- function(){
  googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/19yf8Ty3si6XHjKRw6I6qyZ3CZl52ZY2q_7Q_dTJAR28/edit?gid=0#gid=0')
}

filenameCreate <- function(dfProf, city, from.google = FALSE){
  z <- dfProf |>
    dplyr::filter(City %in% city) |>
    dplyr::select(`Data Code`) |> unlist()
  if(from.google){
    path <-  paste0(z, '.csv')
  }else{
    path <- paste0('./app_data/ed/', z, '.csv')
  }
  path
}

parse_ed_time <- function(date_vec) {
  # Try known formats in order of likelihood
  parsed <- parse_date_time(
    date_vec,
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "mdY HMS", "mdY HM", "mdY",
               "dmy HMS", "dmy HM", "dmy", "Ymd HMS", "Ymd"),
    exact = FALSE,
    tz = "UTC"
  )
  return(parsed)
}
weekdate <- function(date) {
  lubridate::floor_date(date, unit = "week", week_start = 1)  # 1 = Monday
}


load_tarmac_data <- function(df, dfProf){
  dfP <- profileLoad()
  dfP <- dfP |>
    rename('facility_name' = "Data Code")
  # dfT <- tarmacFilter(df)
  # left_join(dfT, dfP)
  data.table::fread("app_data/monthly_counts_by_site.csv") |>
    left_join(dfP)
  
}