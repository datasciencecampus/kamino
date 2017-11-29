#' Run the App
#'
#' Run the fishing industries app
#'
#' @author Nathan Eastwood
#'
#' @export
run_fisheries_app <- function() {
  app_dir <- system.file("app", package = "kamino")
  if (app_dir == "") {
    stop(
      paste0(
        "Could not find example directory. Try re-installing ",
        "`kamino`."
      ),
      call. = FALSE
    )
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
