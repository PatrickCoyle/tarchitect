#' Title
#'
#' @param new_target
#' @param new_function
#' @param import_script
#'
#' @return
#' @export
#'
#' @examples
get_text_plan <- function (import_script, new_target, new_function) {
  ret1 <- paste(import_script, collapse = ",\n\t")
  ret2 <- paste0(
    new_target, " = ",
    new_function, "(\n\t",
    ret1,
    "\n)"
  )
  return(ret2)
}

#' Title
#'
#' @param parameters
#' @param new_function
#'
#' @return
#' @export
#'
#' @examples
get_text_function <- function (parameters, new_function) {
  ret <- paste0(
    new_function,
    " <- function(\n\t",
    paste(parameters, collapse = ",\n\t"),
    "\n) {\n\t",
    paste(parameters, collapse = "\n\t"),
    "\n\treturn(NULL)\n}"
  )
  return(ret)
}
