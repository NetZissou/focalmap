app_dev <- function(run_check = FALSE) {

  if (run_check) {
    devtools::check()
    app()
  } else {
    devtools::load_all()
    app()
  }
}
