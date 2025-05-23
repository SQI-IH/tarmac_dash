#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#' @export
golem_add_external_resources <- function() {
  shiny::addResourcePath("www", app_sys("app/www"))

  shiny::tags$head(
    # Load custom CSS
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/custom_desc.css")
  )
}