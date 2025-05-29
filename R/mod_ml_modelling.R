#' @title mod_ml_modelling_ui and mod_ml_modelling_server
#' @description A shiny module.

mod_ml_modelling_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "ml_modelling",
    shinydashboardPlus::box(
      shiny::selectInput(ns("ctas_level"), "CTAS Level", choices = c(2, 3, 4)),
      shiny::selectInput(ns("facility_name"), "Facility", choices = NULL),
      shiny::sliderInput(ns("hour"), "Hour of Day", min = 0, max = 23, value = 12),
      shiny::selectInput(ns("wday"), "Day of Week", 
                         choices = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
      shiny::checkboxInput(ns("is_weekend"), "Is Weekend", FALSE),
      shiny::numericInput(ns("arrivals_last_3h"), "Arrivals in Last 3 Hours", value = 5),
      shiny::actionButton(ns("predict_btn"), "Predict")
    ),
    shinydashboardPlus::box(
      shiny::verbatimTextOutput(ns("prediction_result"))
    )
  )
}

mod_ml_modelling_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Safely load models & thresholds once
    models <- list()
    thresholds <- list()
    
    for (ctas in c(2, 3, 4)) {
      model_path <- glue::glue("models/ctas{ctas}_model.rds")
      thresh_path <- glue::glue("thresholds/ctas{ctas}_threshold.rds")
      
      models[[as.character(ctas)]] <- tryCatch({
        readRDS(model_path)
      }, error = function(e) {
        warning("Failed to load model for CTAS ", ctas, ": ", e$message)
        NULL
      })
      
      thresholds[[as.character(ctas)]] <- tryCatch({
        readr::read_rds(thresh_path)
      }, error = function(e) {
        warning("Failed to load threshold for CTAS ", ctas, ": ", e$message)
        NULL
      })
    }
    
    
    # Update site options
    shiny::observe({
      shiny::updateSelectInput(session, "facility_name", choices = facilities.in.model)
    })
    
    # Prediction event
    shiny::observeEvent(input$predict_btn, {
      shiny::req(input$ctas_level, input$facility_name)
      
      df_new <- tibble::tibble(
        facility_name = input$facility_name,
        hour = input$hour,
        wday = factor(input$wday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
        is_weekend = input$is_weekend,
        arrivals_last_3h = input$arrivals_last_3h
      )
      
      model <- models[[as.character(input$ctas_level)]]
      threshold <- thresholds[[as.character(input$ctas_level)]]
      
      if (is.null(model) || is.null(threshold)) {
        output$prediction_result <- shiny::renderText({
          "Model or threshold not available for this CTAS level."
        })
        return()
      }
      
      prob <- stats::predict(model, newdata = df_new, type = "prob")[, "yes"]
      prediction <- ifelse(prob > threshold, "Ambulance Arrival Likely", "Unlikely Arrival")
      
      output$prediction_result <- shiny::renderText({
        paste0(
          "Predicted Probability: ", round(prob, 3), "\n",
          "Threshold: ", threshold, "\n",
          "Prediction: ", prediction
        )
      })
    })
  })
}


## copy to body.R
# mod_ml_modelling_ui("ml_modelling_ui_1")

## copy to app_server.R
# callModule(mod_ml_modelling_server, "ml_modelling_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "ml_modelling",icon = icon("user"))
