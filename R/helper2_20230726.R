#' Helper for building a new target in a pipeline
#'
#' @importFrom rlang .data
#' @param new_inputs_df a data frame
#' @param new_target a character vector of length 1
#' @param new_function a character vector of length 1
#' @param code_folder a character vector of length 1
#' @return a list of file names and file contents
#' @autoglobal
#' @export
helper2_20230726 <- function(
  new_inputs_df,
  new_target,
  new_function,
  code_folder = "R"
) {
  tmp2 <- list(
    "txt_fn" = paste0(
      new_function,
      " <- function(\n\t",
      paste(new_inputs_df$Input_Name_fixed, collapse = ",\n\t"),
      "\n) {\n\t",
      paste(new_inputs_df$Input_Name_fixed, collapse = "\n\t"),
      "\n\treturn(NULL)\n}"
    ),
    "filename_fn" = file.path(code_folder, paste0(new_function, ".R")),
    "txt_tgt" = paste0(
      new_target,
      " = ",
      new_function,
      "(\n\t",
      paste(new_inputs_df$import_script2, collapse = ",\n\t"),
      "\n)"
    )
  )
  helper_fn_dir <- dirname(tmp2$filename_fn)
  if (!dir.exists(helper_fn_dir)) {
    dir.create(helper_fn_dir)
  }
  readr::write_lines(tmp2$txt_fn, tmp2$filename_fn)
  source(tmp2$filename_fn)
  new_function |> 
    docthis::doc_this() |> 
    stringr::str_remove("\n#' FUNCTION_TITLE\n#'\n") |> 
    stringr::str_remove("\n#' @examples\n#' # ADD_EXAMPLES_HERE\nmake_gg") |> 
    c(readr::read_lines(file = tmp2$filename_fn)) |> 
  readr::write_lines(file = tmp2$filename_fn)
  return(tmp2)
}
