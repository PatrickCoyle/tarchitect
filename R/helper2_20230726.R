#' Helper for building a new target in a pipeline
#'
#' @importFrom rlang .data
#' @param my_params a list of parameters provided by the user via targetsAddin()
#' @return a list of file names and file contents
#' @export
helper2_20230726 <- function (my_params) {
  tmp1 <- my_params$new_inputs_df %>%
    dplyr::mutate(
      import_script1 = dplyr::case_when(
        Import_Option == "as is" ~ .data[["Target"]],
        Import_Option %in% c("readr::read_rds",
                               "readr::read_csv",
                               "openxlsx2::read_xlsx",
                               "haven::read_sas") ~ paste0(.data[["Import_Option"]], "(", .data[["Target"]], ")"),
        Import_Option == "readr::read_delim(delim = '|')" ~ paste0("readr::read_delim", "(", .data[["Target"]], ", delim = '|')"),
      ),
      import_script2 = paste0(.data[["Input_Name"]], " = ", .data[["import_script1"]])
    )
  tmp2 <- list(
    "txt_fn" = paste0(
      my_params$new_function,
      " <- function(\n\t",
      paste(tmp1$Target, collapse = ",\n\t"),
      "\n) {\n\t",
      paste(tmp1$Target, collapse = "\n\t"),
      "\n\tbrowser()\n\treturn(NULL)\n}"
    ),
    "filename_fn" = here::here("R", paste0(my_params$new_function, ".R")),
    "txt_tgt" = paste0(
      my_params$new_target,
      " = ",
      my_params$new_function,
      "(\n\t",
      paste(tmp1$import_script2, collapse = ",\n\t"),
      "\n),"
    ),
    "filename_tgt" = here::here(paste0("plan_", my_params$new_target, ".txt"))
  )
  return(tmp2)
}
