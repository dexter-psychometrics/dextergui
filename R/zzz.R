

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("shinydexter", system.file("www", package = "dextergui", mustWork = TRUE))
}

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("shinydexter", system.file("www", package = "dextergui", mustWork = TRUE))
}