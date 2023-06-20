#' Add-in for building a new target in a pipeline
#'
#' @return a list of file names and file contents. the app has a side effect of creating file with the specified names and contents.
#' @export
targetsAddin <- function () {

  input_targets <- as.list(targets::tar_manifest()$name)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Build New Target"),
    miniUI::miniContentPanel(
      shiny::textInput("new_target", "New Target Name"),
      shiny::textInput("new_function", "New Function Name"),
      sortable::bucket_list(
        header = "Drag and drop inputs for new function",
        group_name = "bucket_list_groups",
        orientation = "horizontal",
        sortable::add_rank_list(
          text = "Available Inputs",
          labels = input_targets,
          input_id = "exclude"
        ),
        sortable::add_rank_list(
          text = "Selected Inputs",
          labels = NULL,
          input_id = "include"
        )
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      my_output_names <- c("bucket_list_groups", "new_target", "new_function") %>%
        purrr::set_names()
      my_output <- my_output_names %>%
        purrr::map(~input[[.x]])
      formatted <- helper1_20230619(my_output)
      readr::write_lines(formatted$txt_fn, formatted$filename_fn)
      readr::write_lines(formatted$txt_tgt, formatted$filename_tgt)
      utils::file.edit(formatted$filename_tgt)
      utils::file.edit(formatted$filename_fn)
      shiny::stopApp(my_output)
    })
  }

  shiny::runGadget(ui, server)
}
