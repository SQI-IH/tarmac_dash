# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file


pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE,
         auth0_config_file = system.file("app/_auth0.yml", package = "golem.auth0"),
         gargle_oauth_email = TRUE,
         gargle_oauth_cache = "./app_data/.secrets")
tarmac_dash::run_app() # add parameters here (if any)
