# Module UI

#' @title mod_data_upload_ui and mod_data_upload_server
#' @description A shiny module for uploading, previewing, and processing ED arrival data.

mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "data_upload",
    fluidRow(
      box(
        title = "Upload & Process Data",
        width = 12,
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        checkboxInput(ns("upload_to_db"), "Upload to Supabase?", value = FALSE),
        actionButton(ns("process"), "Process & Ingest Data")
      ),
      box(
        title = "Preview Uploaded Data",
        width = 12,
        DT::dataTableOutput(ns("preview")),
        verbatimTextOutput(ns("status"))
      )
    )
  )
}

# Module Server

mod_data_upload_server <- function(input, output, session, conn = NULL, output_dir = "app_data/ed") {
  ns <- session$ns
  
  raw_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
    raw_data(df)
  })
  
  observeEvent(input$process, {
    req(raw_data())
    
    source("R/fct_clean_ed_data.R")
    source("R/fct_write_site_csvs.R")
    if (isTRUE(input$upload_to_db)) source("R/fct_upload_to_supabase.R")
    
    clean_df <- clean_ed_data(raw_data())
    cleaned_data(clean_df)
    
    write_site_csvs(clean_df, output_dir = output_dir)
    
    if (isTRUE(input$upload_to_db) && !is.null(conn)) {
      upload_to_supabase(clean_df, conn)
    }
    
    output$status <- renderText("✅ Data successfully processed.")
  })
  
  output$preview <- DT::renderDataTable({
    req(raw_data())
    DT::datatable(
      head(raw_data(), 10),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
}

## copy to body.R
# mod_data_upload_ui("data_upload_ui_1")
 
## copy to app_server.R
# callModule(mod_data_upload_server, "data_upload_ui_1")
 
## copy to sidebar.R
# menuItem("displayName",tabName = "data_upload",icon = icon("user"))
 
