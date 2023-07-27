#' Add-in for building a new target in a pipeline
#'
#' @importFrom rlang .data
#' @param script Character of length 1, path to the target script file. See \link[targets]{tar_manifest} for details.
#' @return a list of file names and file contents. the app has a side effect of creating file with the specified names and contents.
#' @export
targetsAddin <- function (
    script = targets::tar_config_get("script")
) {
  input_targets <- as.list(targets::tar_manifest(script = script)$name)
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Build New Target"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        "Step 1",
        miniUI::miniContentPanel(
          miniUI::miniButtonBlock(
            shiny::textInput("new_target", "New Target Name"),
            shiny::textInput("new_function", "New Function Name")
          ),
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
      ),
      miniUI::miniTabPanel(
        "Step 2",
        miniUI::miniContentPanel(
          rhandsontable::rHandsontableOutput("tbl1")
        )
      )

    )
  )
  server <- function(input, output, session) {
    rv1 <- shiny::reactiveValues(dat = NULL)
    shiny::observeEvent(input$bucket_list_groups$include, {
      rv1$dat <- tibble::tibble(
        Target = unlist(input$bucket_list_groups$include)
      ) %>%
        dplyr::mutate(
          Input_Name = .data[["Target"]],
          Import_Option = factor("as is",
                                 levels = c("as is",
                                            "readr::read_rds",
                                            "readr::read_csv",
                                            "openxlsx2::read_xlsx",
                                            "readr::read_delim(delim = '|')",
                                            "haven::read_sas"),
                                 ordered = TRUE
          )
        )
    })
    output$tbl1 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(rv1$dat, height = 300)
    })
    shiny::observeEvent(input$done, {
      tmp1 <- rhandsontable::hot_to_r(input$tbl1)
      tmp2 <- c(
        "new_target",
        "new_function"
      ) %>%
        purrr::set_names() %>%
        purrr::map(~input[[.x]]) %>%
        c(list("new_inputs_df" = tmp1))
      tmp3 <- helper2_20230726(tmp2)
      helper_fn_dir <- dirname(tmp3$filename_fn)
      if (!dir.exists(helper_fn_dir)) {
        dir.create(helper_fn_dir)
      }
      readr::write_lines(tmp3$txt_fn, tmp3$filename_fn)
      readr::write_lines(tmp3$txt_tgt, tmp3$filename_tgt)
      utils::file.edit(tmp3$filename_tgt, tmp3$filename_fn)
      shiny::stopApp(tmp3)
    })
  }
  shiny::runGadget(ui, server)
}
