#' Add-in for building a new target in a pipeline
#'
#' @param script Character of length 1, path to the target script file.
#' See \link[targets]{tar_manifest} for details.
#' @return a list of file names and file contents.
#' The app has a side effect of creating a file
#' with the specified names and contents.
#' @autoglobal
#' @export
targets_addin <- function(script = targets::tar_config_get("script")) {
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
        ),
        miniUI::miniContentPanel(
          rhandsontable::rHandsontableOutput("tbl1")
        )
      )
    )
  )
  server <- function(input, output, session) {
    rv1 <- shiny::reactiveValues(dat = dplyr::tibble(c()))
    shiny::observeEvent(input$bucket_list_groups$include, {
      rv1$dat <- dplyr::tibble(
        Input_Name = unlist(input$bucket_list_groups$include),
        Import_Option = "As is"
      )
    })
    output$tbl1 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(rv1$dat, height = 300)
    })
    shiny::observeEvent(input$done, {
      tmp3 <- input$tbl1 |> 
      rhandsontable::hot_to_r() |>
      dplyr::mutate(
        import_script1 = dplyr::case_when(
          Import_Option == "As is" ~ Input_Name,
          TRUE ~ paste0(Import_Option, "(", Input_Name, ")")
        )
      ) |> 
      dplyr::transmute(
        Input_Name_fixed = dplyr::case_when(
          Import_Option == "As is" ~ Input_Name,
          TRUE ~ paste0(Input_Name, "_obj")
        ),
        import_script2 = paste0(Input_Name_fixed, " = ", import_script1)
      ) |> 
      helper2_20230726(
        new_target = input$new_target,
        new_function = input$new_function
      )
      shiny::stopApp(tmp3)
    })
  }
  shiny_out <- shiny::runGadget(ui, server)
  utils::file.edit(shiny_out$filename_fn)
  cat(shiny_out$txt_tgt)
  return(shiny_out$txt_tgt)
}
