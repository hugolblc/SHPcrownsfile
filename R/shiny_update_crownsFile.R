#' Lancer l'application Shiny
#'
#' Cette fonction lance l'application Shiny.
#'
#' @export
#' @import shiny
shiny_update_crownsFile <- function() {
   appDir <- system.file("shiny_app", package = "SHPcrownsfile")
   if (appDir == "") {
      stop("Impossible de trouver l'application shiny. Veuillez réinstaller le package.", call. = FALSE)
   }
   shiny::runApp(appDir, display.mode = "normal")
}
