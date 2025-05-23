app_server <- function(input, output, session) {
  mod_tarmac_server("tarmac_ui_1")
  # callModule(mod_event_analysis_server, "event_analysis_ui_1")
  # callModule(mod_project_docs_server, "project_docs_ui_1")
  # callModule(mod_outages_server, "outages_ui_1")
  # callModule(mod_site_profile_server, "site_profile_ui_1")
  # callModule(mod_ed_statistics_server, "ed_statistics_ui_1")
}
