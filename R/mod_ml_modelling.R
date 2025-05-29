# Module UI

#' @title mod_ml_modelling_ui and mod_ml_modelling_server
#' @description A shiny module.

mod_ml_modelling_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "ed_statistics",
    shinydashboardPlus::box(
      shiny::selectInput(ns("ctas_level"), "CTAS Level", choices = c(2, 3, 4)),
      shiny::selectInput(ns("facility_name"), "Facility", choices = NULL),
      shiny::sliderInput(ns("hour"), "Hour of Day", min = 0, max = 23, value = 12),
      shiny::selectInput(ns("wday"), "Day of Week", choices = levels(lubridate::wday(Sys.Date(), label = TRUE))),
      shiny::checkboxInput(ns("is_weekend"), "Is Weekend", FALSE),
      shiny::numericInput(ns("arrivals_last_3h"), "Arrivals in Last 3 Hours", value = 5),
      shiny::actionButton(ns("predict_btn"), "Predict")
    ),
    shinydashboardPlus::box(
      shiny::verbatimTextOutput(ns("prediction_result"))
    )
  )
}

# Module Server

mod_ml_modelling_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    model_cache <- shiny::reactiveValues(models = list(), thresholds = list())
    shiny::observe({
      for (ctas in c(2, 3, 4)) {
        model_cache$models[[as.character(ctas)]] <- base::readRDS(glue::glue("models/ctas{ctas}_model.rds"))
        model_cache$thresholds[[as.character(ctas)]] <- readr::read_rds(glue::glue("thresholds/ctas{ctas}_threshold.rds"))
      }
    })

    shiny::observe({
      shiny::updateSelectInput(session, "facility_name", choices = c("Site A", "Site B", "Site C"))
    })

    shiny::observeEvent(input$predict_btn, {
      shiny::req(input$ctas_level, input$facility_name)

      df_new <- tibble::tibble(
        facility_name = input$facility_name,
        hour = input$hour,
        wday = factor(input$wday, levels = levels(lubridate::wday(Sys.Date(), label = TRUE))),
        is_weekend = input$is_weekend,
        arrivals_last_3h = input$arrivals_last_3h
      )

      model <- model_cache$models[[as.character(input$ctas_level)]]
      threshold <- model_cache$thresholds[[as.character(input$ctas_level)]]

      prob <- stats::predict(model, newdata = df_new, type = "prob")[, "yes"]
      prediction <- ifelse(prob > threshold, "Ambulance Arrival Likely", "Unlikely Arrival")
    })
    output$prediction_result <- shiny::renderText({
      paste(
        "Predicted Probability:", round(prob, 3), "\n",
        "Threshold:", threshold, "\n",
        "Prediction:", prediction
      )
    })
  })
}



## copy to body.R
# mod_ml_modelling_ui("ml_modelling_ui_1")

## copy to app_server.R
# callModule(mod_ml_modelling_server, "ml_modelling_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "ml_modelling",icon = icon("user"))
