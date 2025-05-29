app_server <- function(input, output, session) {
  mod_tarmac_server("tarmac_ui_1")
  mod_ih_closures_server("ih_closures_ui_1")
  mod_project_docs_server("project_docs_ui_1")
  # callModule(mod_site_profile_server, "site_profile_ui_1")
  mod_ed_statistics_server("ed_statistics_ui_1")
  mod_event_analysis_server("event_analysis_ui_1")
  mod_ml_modelling_server("ml_modelling_ui_1")
}
