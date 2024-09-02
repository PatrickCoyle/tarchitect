#' Helper for building a new target in a pipeline
#'
#' @param new_function a character vector of length 1
#' @param code_folder a character vector of length 1
#' @param text_fn a character vector of length 1; the function text
#' @return a list of file names and file contents
#' @autoglobal
#' @export
write_function_script <- function(
  new_function,
  code_folder = "R",
  text_fn
) { 
  filename_fn <- file.path(code_folder, paste0(new_function, ".R"))
  if (!dir.exists(dirname(filename_fn))) {
    dir.create(dirname(filename_fn))
  }
  readr::write_lines(text_fn, filename_fn)
  source(filename_fn)
  text_fn_out <- new_function |> 
  docthis::doc_this() |> 
  stringr::str_remove("\n#' FUNCTION_TITLE\n#'\n") |> 
  stringr::str_remove(paste0("\n#' @examples\n#' # ADD_EXAMPLES_HERE\n", new_function)) |> 
  c(readr::read_lines(file = filename_fn)) 
  readr::write_lines(
    text_fn_out,
    file = filename_fn
  )
  utils::file.edit(filename_fn)
  return(text_fn_out)
}
