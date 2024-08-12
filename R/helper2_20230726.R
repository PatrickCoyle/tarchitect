#' Helper for building a new target in a pipeline
#'
#' @importFrom rlang .data
#' @param new_inputs_df
#' @param new_target
#' @param new_function
#' @return a list of file names and file contents
#' @autoglobal
#' @export
helper2_20230726 <- function(
  new_inputs_df,
  new_target,
  new_function
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
    "filename_fn" = here::here("R", paste0(new_function, ".R")),
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
  # readr::write_lines(tmp3$txt_tgt, tmp3$filename_tgt)
  # utils::file.edit(tmp3$filename_tgt, tmp3$filename_fn)
  return(tmp2)
}
