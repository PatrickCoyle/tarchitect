#' Exit tarchitect function
#'
#' @param exit_list a list object; the output of shiny::runGadget()
#' @param envir
#'
#' @return NULL
#' @autoglobal
tarchitect_exit <- function (exit_list, envir = parent.frame(n = 2)) {
  switch(
    exit_list$load_targets,
    "Selected Inputs" = targets::tar_load(exit_list$targets, envir = envir),
    "Everything" = targets::tar_load_everything(envir = envir)
  )
  if (exit_list$load_globals) {
    targets::tar_load_globals(envir = envir)
  }
  cat(exit_list$plan_text)
  return(NULL)
}
