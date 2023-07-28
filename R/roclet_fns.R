#' New roxygen tag
#'
#' @param x standard roxygen2 input
#' @return standard roxygen2 output
#' @export
roxy_tag_parse.roxy_tag_memo <- function(x) {
  x$val <- list(
    header = paste0("step", stringr::str_sub(x$raw, end = 1L)),
    message = x$raw
  )
  x
}

#' New roclet
#' @importFrom roxygen2 roclet
#' @return standard roxygen2 output
#' @export
memo_roclet <- function() {
  roxygen2::roclet("memo")
}

#' New roclet process
#'
#' @param x standard roxygen2 input
#' @param blocks standard roxygen2 input
#' @param env standard roxygen2 input
#' @param base_path standard roxygen2 input
#' @importFrom roxygen2 roclet_process
#' @return standard roxygen2 output
#' @export
roclet_process.roclet_memo <- function(x, blocks, env, base_path) {
  results <- list()
  for (block in blocks) {
    tags <- roxygen2::block_get_tags(block, "memo")
    for (tag in tags) {
      msg <- tag$val$message
      results[[tag$val$header]] <- c(results[[tag$val$header]], msg)
    }
  }
  results
}

#' New roclet output
#'
#' @param x standard roxygen2 input
#' @param results standard roxygen2 input
#' @param base_path standard roxygen2 input
#' @param ... additional inputs
#' @importFrom roxygen2 roclet_output
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_equal
#' @importFrom here here
#' @return standard roxygen2 output
#' @export
roclet_output.roclet_memo <- function(x, results, base_path, ...) {
  # Instead of rendering the Quarto file to HTML here, let's just edit the quarto file and give it a unique name
  # This will let us edit all quarto docs as a book using a single YAML file
  tmp1 <- paste0(results$fn_name, ".qmd")
  tmp2 <- readr::read_lines(here::here("_extensions", "document-fn", "template.qmd"))
  yaml_end_line <- tmp2 %>%
    stringr::str_equal("---") %>%
    which() %>%
    max()
  tmp3 <- c(
    tmp2[1:yaml_end_line],
    c(
      "",
      "```{r}",
      "#| echo: false",
      "",
      paste0("these_results <- all_results[['", results$fn_name, "']]"),
      "```"
    ),
    tmp2[(yaml_end_line+1):length(tmp2)]
  )
  tmp3 %>% readr::write_lines(here::here("_extensions", "document-fn", paste0(results$fn_name, ".qmd")))
  # tmp1 <- paste0(results$fn_name, ".html")
  # quarto::quarto_render(
  #   input = here::here("_extensions", "document-fn", "template.qmd"),
  #   execute_params = list("results" = results),
  #   output_file = tmp1
  # )
  # file.rename(
  #   here::here(tmp1),
  #   here::here("_extensions", "document-fn", tmp1)
  # )
  # print(results)
  invisible(NULL)
}

#' Documentation wrapper that uses new roclet
#'
#' @param my_file a roxygenized .R file
#' @importFrom roxygen2 roc_proc_text roclet_output
#' @return a list of roxygen tags from the file. A side effect is that function documentation is produced using Quarto.
#' @export
document_file <- function(my_file) {
  tmp1 <- readr::read_lines(my_file)
  tmp2 <- tmp1 %>% paste(collapse = "\n")
  tmp3 <- tmp1 %>%
    stringr::str_sub(end = 2L) %>%
    stringr::str_equal("#'") %>%
    which() %>%
    max() %>%
    `+`(1)
  tmp4 <- tmp1[tmp3:length(tmp1)]
  tmp5 <- tmp4[1] %>% stringr::str_remove_all(" ")
  results <- c(
    roxygen2::roc_proc_text(memo_roclet(), tmp2),
    list(
      "fn_body" = tmp4,
      "fn_name" = stringr::str_sub(tmp5, end = stringr::str_locate(tmp5, "<-")[,"start"] - 1)
    )
  )
  roxygen2::roclet_output(memo_roclet(), results)
  return(results)
}
