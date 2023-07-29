#' Convert roxygenized custom functions into a Quarto book. Works for projects that are not packages.
#'
#' @param my_extension String indicating which Quarto extension to use
#' @param my_fns Character vector of roxygenized functions to document in a book
#' @importFrom stats setNames
#' @return Renders a Quarto book as a side effect. Returns the directory of the Quarto book.
#' @export
render_book <- function(my_extension, my_fns) {
  tmp0 <- here::here("_extensions", my_extension)
  use_quarto_ext(my_extension)
  for(my_fn in my_fns) {
    document_file(my_file = here::here("R", paste0(my_fn, ".R")))
  }
  yaml_in <- readr::read_lines(here::here(tmp0, "_quarto.yml"))
  tmp1 <- which(stringr::str_equal(yaml_in, "    - index.qmd"))
  yaml_out <- c(
    yaml_in[1:tmp1],
    paste0("    - qmd/", my_fns, ".qmd"),
    yaml_in[(tmp1+1):length(yaml_in)]
  )
  yaml_out %>% readr::write_lines(here::here(tmp0, "_quarto.yml"))
  tmp2 <- stats::setNames(my_fns, my_fns)
  all_results <- tmp2 %>%
    purrr::map(
      ~readr::read_rds(here::here("_extensions", my_extension, "results", paste0(.x, ".rds")))
    )
  quarto::quarto_render(
    input = tmp0,
    execute_params = list("all_results" = all_results)
  )
  return(tmp0)
}
