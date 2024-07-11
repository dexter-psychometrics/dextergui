

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  shiny::addResourcePath("shinydexter", system.file("www", package = "dextergui", mustWork = TRUE))
}

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  shiny::addResourcePath("shinydexter", system.file("www", package = "dextergui", mustWork = TRUE))
}