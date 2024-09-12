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
  avail_tgts <- targets::tar_manifest(script = script)$name |>
    purrr::set_names()
  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    miniUI::gadgetTitleBar("Build New Target"),
    miniUI::miniContentPanel(
      miniUI::miniButtonBlock(
        shiny::textInput("new_target", "New Target Name", "new_target"),
        shiny::textInput("new_function", "New Function Name", "new_function"),
        shiny::selectInput(
          "load_targets",
          "Targets to Load",
          choices = c(
            "None",
            "Selected Inputs",
            "Everything"
          )
        ),
        shiny::checkboxInput(
          "load_globals",
          label = "Load global objects?"
        )
      ),
      sortable::bucket_list(
        header = "Drag and drop inputs for new function",
        group_name = "bucket_list_groups",
        orientation = "horizontal",
        sortable::add_rank_list(
          text = "Available Inputs",
          labels = avail_tgts |>
            purrr::map(~shiny::tags$div(
              shiny::textInput(paste0(.x, ".target"), "Target", .x),
              shiny::textInput(paste0(.x, ".parameter"), "Parameter", .x),
              shiny::textInput(paste0(.x, ".load"), "Input", .x)
            )),
          input_id = "exclude"
        ),
        sortable::add_rank_list(
          text = "Selected Inputs",
          labels = NULL,
          input_id = "include"
        )
      ),
      shiny::verbatimTextOutput("text_fn_out")
    )
  )
  server <- function(input, output, session) {
    shiny::observeEvent(input$bucket_list_groups$include, {
      input$bucket_list_groups$include |>
        purrr::map(~{
          shinyjs::show(paste0(.x, ".parameter"))
          shinyjs::show(paste0(.x, ".load"))
        })
    })
    shiny::observeEvent(input$bucket_list_groups$exclude, {
      input$bucket_list_groups$exclude |>
        purrr::map(~{
          shinyjs::hide(paste0(.x, ".parameter"))
          shinyjs::hide(paste0(.x, ".load"))
        })
    })

    hot_df <- shiny::reactive({
      vec1 <- unlist(input$bucket_list_groups$include)
      to_return <- dplyr::tibble(
        Target = input[[paste0(vec1, ".target")]],
        Parameter = input[[paste0(vec1,".parameter")]],
        Input = input[[paste0(vec1,".load")]],
      )
      return(to_return)
    })
    text_fn_out <- shiny::reactive(
      get_text_function(
        parameters = hot_df()$Parameter,
        new_function = input$new_function
      )
    )
    output$text_fn_out <- shiny::renderText(
      text_fn_out()
    )
    shiny::observeEvent(input$done, {
      if (nchar(input$new_function) == 0) {
        shinyalert::shinyalert("Error", "No function name selected.", type = "error")
      } else if (nchar(input$new_target) == 0) {
        shinyalert::shinyalert("Error", "No new target name selected.", type = "error")
      } else {
        write_function_script(
          new_function = input$new_function,
          code_folder = code_folder,
          text_fn = text_fn_out()
        )
        if (nrow(hot_df()) == 0) {
          targets_to_return <- ""
          plan_text <- get_text_plan(
            import_script = "",
            new_target = input$new_target,
            new_function = input$new_function
          )
        } else {
          targets_to_return <- hot_df() |>
            dplyr::pull(Target)
          plan_text <- hot_df() |>
            dplyr::transmute(import_script = paste0(Parameter, " = ", Input)) |>
            dplyr::pull(import_script) |>
            get_text_plan(
              new_target = input$new_target,
              new_function = input$new_function
            )
        }
        to_return <- list(
          targets = targets_to_return,
          plan_text = plan_text,
          load_targets = input$load_targets,
          load_globals = input$load_globals
        )
        shiny::stopApp(to_return)
      }
    })
  }
  to_return <- shiny::runGadget(ui, server)
  on.exit(tarchitect_exit(exit_list = to_return))
  return(to_return$plan_text)
}
