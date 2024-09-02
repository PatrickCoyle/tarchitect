#' Exit tarchitect function
#'
#' @param input_list a list object; the output of shiny::runGadget()
#' 
#' @return NULL
#' @autoglobal
tarchitect_exit <- function (input_list) {
  if (input_list$tar_load_option == "Load Input Targets") {
    targets::tar_load(input_list$df_in$target, envir = parent.frame(n = 2))
  } else if (input_list$tar_load_option == "Load Input Targets and Global Objects") {
    targets::tar_load(input_list$df_in$target, envir = parent.frame(n = 2))
    targets::tar_load_globals(envir = parent.frame(n = 2))
  } else if (input_list$tar_load_option == "Load Everything") {
    targets::tar_load_everything(envir = parent.frame(n = 2))
  }
  cat(input_list$plan_text)
  return(NULL)
}