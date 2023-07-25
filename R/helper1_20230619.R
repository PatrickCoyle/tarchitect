#' Helper for building a new target in a pipeline
#'
#' @param my_params a list of parameters provided by the user via targetsAddin()
#' @return a list of file names and file contents
#' @export
helper1_20230619 <- function (my_params) {
  return(
    list(
      "txt_fn" = paste0(
        my_params$new_function,
        " <- function(\n\t",
        paste(my_params$bucket_list_groups$include, collapse = ",\n\t"),
        "\n) {\n\t",
        paste(my_params$bucket_list_groups$include, collapse = "\n\t"),
        "\n\tbrowser()\n\treturn(NULL)\n}"
      ),
      "filename_fn" = here::here("R", paste0(my_params$new_function, ".R")),
      "txt_tgt" = paste0(
        my_params$new_target,
        " = ",
        my_params$new_function,
        "(\n\t",
        paste(my_params$bucket_list_groups$include, collapse = ",\n\t"),
        "\n),"
      ),
      "filename_tgt" = here::here(paste0("plan_", my_params$new_target, ".txt"))
    )
  )
}
