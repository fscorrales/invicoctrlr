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
                     # tagList(
                     #   tags$span("Importar archivo"),
                     #   tags$span(icon("info-circle"), id = ns("icon"), style = "color: blue;")
                     # ),
                     placeholder = "Archivo no seleccionado",
                     buttonLabel = "Importar", ...)

  )
}

#' file_input Server Functions
#'
#' @noRd
mod_file_input_server <- function(id, import_function,
                                  require_extension = "csv", ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # shinyBS::addPopover(session, ns("icon"), "Ayuda Importación:",
    #                     ContenidoAyuda, placement = "right")

    user_file <- shiny::reactive(req(input$file))

    ext <- reactive({

      tools::file_ext(user_file()$name)

    })

    data <- reactive({

      df <- do.call(import_function(),
                    list(path = user_file()$datapath,
                         write_sqlite = TRUE, ...))

      # ext <- tools::file_ext(userFile()$name)
      # switch(ext(),
      #        csv = ImportarCSV(user_file()$datapath, ArchivoImportar),
      #        xls = ImportarXLS(user_file()$datapath, ArchivoImportar),
      #        FALSE
      #        # tsv = vroom::vroom(input$file$datapath, delim = "\t"),
      #        # validate("Invalid file; Please upload a .csv or .tsv file")
      # )
    })

    observeEvent(data(), {

      shinyFeedback::hideFeedback(inputId = "file")

      if (ext() != require_extension()) {
        shinyFeedback::showFeedbackDanger(inputId = "file",
                           text = paste0("Archivo inválido;",
                                         "por favor cargar un archivo .",
                                         require_extension))
      } else {
        if (data() == FALSE) {
          shinyFeedback::showFeedbackDanger(inputId = "file",
                                            text = "Archivo incorrecto, VERIFICAR")
        } else {
          shinyFeedback::showFeedbackSuccess(inputId = "file",
                                             text = "Carga COMPLETA")
        }
      }

    })

  })
}

## To be copied in the UI
# mod_file_input_ui("file_input_ui_1")

## To be copied in the server
# mod_file_input_server("file_input_ui_1")
