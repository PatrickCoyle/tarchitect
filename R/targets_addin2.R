#' Add-in for building a new target in a pipeline
#'
#' @export
targets_addin2 <- function() {
  targets::tar_load(
    rstudioapi::getActiveDocumentContext()$selection[[1]]$text,
    envir = .GlobalEnv
  )
}
