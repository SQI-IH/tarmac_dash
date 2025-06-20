#' Load and clean ED data from all sites
#' @export
#' @name mod_tarmac_fct_display

tarmacFilter<- function(df){
  df |>
    filter(stringr::str_detect(
      stringr::str_to_lower(discharge_comment), # Convert to lowercase for case-insensitive matching
      stringr::regex("t[ar]{1,2}m[ac]{1,2}", ignore_case = TRUE) # Fuzzy regex matching "tarmac" with minor typos
    ))
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
  # Convert the date column to Date type
  data <- dfT %>%
    mutate(
      ed_arrival_date_time = ymd_hms(ed_arrival_date_time),
      year_month = ym(year_mo) # Extract year-month
    )
  
  # Get the range for the last 12 months
  end_date <- floor_date(Sys.Date(), "month")
  start_date <- end_date - months(11)
  
  # Generate a complete year-month sequence
  all_months <- seq.Date(start_date, end_date, by = "month")
  
  # Create a summary table with counts
  summary_data <- data %>%
    dplyr::filter(year_month >= start_date & year_month <= end_date) %>%
    dplyr::group_by(City, year_month) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    tidyr::complete(
      City,
      year_month = all_months,
      fill = list(count = 0)
    )
  
  # Ensure year_month is treated as a factor ordered by date
  summary_data <- summary_data %>%
    mutate(year_month = factor(format(year_month, "%b %Y"), levels = format(all_months, "%b %Y")))
  
  # If no custom color palette is provided, use a default one
  if (is.null(color_palette)) {
    color_palette <- RColorBrewer::brewer.pal(length(unique(summary_data$City)), "Set3")
  }
  
  city_cols  <-
    c(
      "Castlegar"=  "#1f77b4", # blue
      "Sparwood" =  "#f5c647",
      "Chase" = "#17becf"  # cyan
    )
  
  x <- summary_data |> group_by(year_month) |> summarise(n = sum(count))
  # Plotly stacked bar chart with custom color palette
  summary_data %>%
    plotly::plot_ly(
      x = ~year_month,
      y = ~count,
      color = ~City,
      colors = city_cols,  # Apply the custom color palette
      type = "bar",
      text = ~paste(year_month, "<br>Community:", City, "<br>Count:", count),
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
  purrr::map_dfr(paths, data.table::fread) %>%
    mutate(datetime = parse_ed_time(ed_arrival_date_time),
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


load_tarmac_data <- function(df){
  dfP <- profileLoad()
  dfP <- dfP |>
    rename('facility_name' = "Data Code")
  dfT <- tarmacFilter(df)
  left_join(dfT, dfP)
}