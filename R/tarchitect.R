#' Shiny app for building a new target in a pipeline
#'
#' @param script Character of length 1, path to the target script file.
#' See \link[targets]{tar_manifest} for details.
#' @param code_folder a character vector of length 1; the name of a valid directory for R scripts.
#' @return a list of file names and file contents.
#' The app has a side effect of creating a file
#' with the specified names and contents.
#' @autoglobal
#' @export
tarchitect <- function(
  script = targets::tar_config_get("script"),
  code_folder = "R/"
) {
  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    miniUI::gadgetTitleBar("Build New Target"),
    miniUI::miniContentPanel(
      miniUI::miniButtonBlock(
        shiny::textInput("new_target", "New Target Name"),
        shiny::textInput("new_function", "New Function Name"),
        shiny::selectInput(
          "tar_load_option",
          "Load Option",
          choices = c(
            "Load Nothing",
            "Load Input Targets",
            "Load Input Targets and Global Objects",
            "Load Everything"
          )        
        )
      ),
      sortable::bucket_list(
        header = "Drag and drop inputs for new function",
        group_name = "bucket_list_groups",
        orientation = "horizontal",
        sortable::add_rank_list(
          text = "Available Inputs",
          labels = as.list(targets::tar_manifest(script = script)$name),
          input_id = "exclude"
        ),
        sortable::add_rank_list(
          text = "Selected Inputs",
          labels = NULL,
          input_id = "include"
        )
      ),
      shiny::div(
        rhandsontable::rHandsontableOutput("tbl1"),
        style = "
          max-width: fit-content;
          margin-left: auto;
          margin-right: auto;
          margin-bottom: 10px;
        "
      ),
      shiny::verbatimTextOutput("text_fn_out")
    )
  )
  server <- function(input, output, session) {
    hot_df <- shiny::eventReactive(input$bucket_list_groups$include, {
      vec1 <- unlist(input$bucket_list_groups$include)
      to_return <- dplyr::tibble(
        target = vec1,
        parameter = vec1,
        load_spec = vec1
      )
      return(to_return)
    })
    output$tbl1 <- rhandsontable::renderRHandsontable(
      hot_df() |> 
      rhandsontable::rhandsontable(
        rowHeaders = NULL,
        overflow = "hidden"
      ) |> 
      rhandsontable::hot_col("target", readOnly = TRUE)
    )
    df_in <- shiny::reactive(rhandsontable::hot_to_r(input$tbl1))
    text_fn_out <- shiny::reactive(
      paste0(
        input$new_function,
        " <- function(\n\t",
        paste(df_in()$parameter, collapse = ",\n\t"),
        "\n) {\n\t",
        paste(df_in()$parameter, collapse = "\n\t"),
        "\n\treturn(NULL)\n}"
      )
    )
    output$text_fn_out <- shiny::renderText(text_fn_out())
    shiny::observeEvent(input$done, {
      df_in2 <- df_in() |> 
      dplyr::mutate(import_script = paste0(parameter, " = ",  load_spec)) 
      write_function_script(
        new_function = input$new_function,
        code_folder = code_folder,
        text_fn = text_fn_out()
      )
      to_return <- list(
        df_in = df_in2,
        plan_text = paste0(
          input$new_target," = ", 
          input$new_function, "(\n\t",
          paste(
            df_in2$import_script, 
            collapse = ",\n\t"
          ), 
          "\n)"
        ),
        tar_load_option = input$tar_load_option
      )
      shiny::stopApp(to_return)
    })
  }
  to_return <- shiny::runGadget(ui, server)
  on.exit(tarchitect_exit(input_list = to_return))
  return(to_return$plan_text)
}