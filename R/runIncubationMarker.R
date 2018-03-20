#' Run the IncubationMarker app
#'
#' This function is another way to run the app
#' @param none there is no parameter
#' @keywords shiny
#' @export
#' @examples
#' runIncubationMarker()

runIncubationMarker<-function(){

  appDir <- system.file("IncubationMarker", package = "IncubationMarker")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `IncubationMarker`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
