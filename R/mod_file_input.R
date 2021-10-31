#' file_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_input_ui <- function(id, ...){
  ns <- NS(id)
  tagList(

    # useShinyFeedback(),
    shiny::fileInput(ns("file"),
                     label = "Importar archivo",
                     placeholder = "Archivo no seleccionado",
                     buttonLabel = "Importar", ...)

  )
}

#' file_input Server Functions
#'
#' @noRd
mod_file_input_server <- function(id, import_function,
                                  df_trigger, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$file, {

      shinyFeedback::hideFeedback(inputId = "file")

      value_mess <- catchr::catch_expr(
        do.call(import_function(),
                list(path = input$file$datapath,
                     write_sqlite = TRUE, ...)),
        error = c("collect", "muffle"),
        warning = c("collect", "muffle"),
        message = c("collect", "muffle")
      )

      mess_errs <- lapply(value_mess$error, `[[`, 1)

      mess_warns <- lapply(value_mess$warning, `[[`, 1)

      if (length(mess_errs) == 0) {
        df_trigger()$trigger()
        if (length(mess_warns) == 0) {
          shiny::showNotification("Importaci\u00f3n con \u00e9xito", type = "message")
          shinyFeedback::showFeedbackSuccess(inputId = "file",
                                             text = "Importaci\u00f3n COMPLETA")
        } else {
          shinyFeedback::showFeedbackWarning(inputId = "file",
                                             text = paste0("Importaci\u00f3n INCOMPLETA - ",
                                                           "no se pudieron importar ",
                                                           length(mess_warns), " archivos"))
          lapply(mess_warns,
                 function(x) shiny::showNotification(x, type = "warning",
                                                     duration = 10))
        }
      } else {
        shinyFeedback::showFeedbackDanger(inputId = "file",
                                          text = paste0("ERROR en la Importaci\u00f3n",
                                                        " - ning\u00fan archivo fue importado"))
        lapply(mess_warns,
               function(x) shiny::showNotification(x, type = "warning",
                                                   duration = 10))
      }
    })

  })
}

## To be copied in the UI
# mod_file_input_ui("file_input_ui_1")

## To be copied in the server
# mod_file_input_server("file_input_ui_1")
