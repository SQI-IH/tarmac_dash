sidebar <- function() {
  dashboardSidebar(
    sidebarMenu(
      id = "mainSidebar",
      menuItem("Tarmac Overview", tabName = "tarmac", icon = icon("truck-medical"))
      ,menuItem("IH Closures", tabName = "ih_closures", icon = icon("plug-circle-xmark"))
      # # ,menuItem("ED Site Profiles",tabName = "site_profile",icon = icon("hospital"))
      ,menuItem("ED Statistics", tabName = "ed_statistics", icon = icon("chart-column"))
      ,menuItem("Project Documents", tabName = "project_docs", icon = icon("file"))
      ,menuItem("ED Event Viewer", tabName = "event_analysis", icon = icon("calendar"))
      ,menuItem("ED Prediction Model",tabName = "ml_modelling",icon = icon("chart-diagram"))
    ),
    collapsed = FALSE,
    width = 200
  )
}
