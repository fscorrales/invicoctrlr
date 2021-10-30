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

      do.call(import_function(),
              list(path = input$file$datapath,
                   write_sqlite = TRUE, ...))

      df_trigger()$trigger()

      shiny::showNotification("Importación con éxito", type = "message")

      shinyFeedback::showFeedbackSuccess(inputId = "file",
                                         text = "Carga COMPLETA")

    })

    # observeEvent(data(), {
    #
    #   shinyFeedback::hideFeedback(inputId = "file")
    #
    #   # if (ext() != require_extension()) {
    #   #   shinyFeedback::showFeedbackDanger(inputId = "file",
    #   #                      text = paste0("Archivo inválido;",
    #   #                                    "por favor cargar un archivo .",
    #   #                                    require_extension))
    #   # } else {
    #     if (data() == FALSE) {
    #       shinyFeedback::showFeedbackDanger(inputId = "file",
    #                                         text = "Archivo incorrecto, VERIFICAR")
    #     } else {
    #       shinyFeedback::showFeedbackSuccess(inputId = "file",
    #                                          text = "Carga COMPLETA")
    #       df_trigger()$trigger()
    #     }
    #   # }
    #
    # })

  })
}

## To be copied in the UI
# mod_file_input_ui("file_input_ui_1")

## To be copied in the server
# mod_file_input_server("file_input_ui_1")
