#' Where should I put my tables from HOBO?
#'
#' This function returns the directory where you should paste your HOBO files
#' @param none there is no parameter
#' @keywords shiny
#' @export
#' @examples
#' whereHOBO()

whereHOBO<-function(){
  appDir <- system.file("IncubationMarker","hobo", package = "IncubationMarker")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `IncubationMarker`.", call. = FALSE)
  }
  appDir
}
