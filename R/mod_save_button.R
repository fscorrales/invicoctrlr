#' export_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_button_ui <- function(id, label_button = "",
                               title = "Guardar como..." ,
                               ...){
  ns <- NS(id)
  tagList(
    shinyFiles::shinySaveButton(ns("download"), label = label_button,
                                title = title, ...)
  )
}

#' export_button Server Functions
#'
#' @noRd
mod_save_button_server <- function(id, df) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$download, {

      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

      shinyFiles::shinyFileSave(input, "download", roots = volumes, session = session,
                                restrictions = system.file(package = "base"),
                                allowDirCreate = TRUE)

      fileinfo <- shinyFiles::parseSavePath(volumes, input$download)

      if (nrow(fileinfo) > 0) {
        file_ext <- tools::file_ext(fileinfo$datapath)

        if (file_ext == "xlsx") {
          openxlsx::write.xlsx(df(), as.character(fileinfo$datapath))
        }

        if (file_ext == "csv") {
          vroom::vroom_write(df(), as.character(fileinfo$datapath), delim = ",")
        }

      }

    })

    # output$download <- shiny::downloadHandler(
    #   filename = function() {
    #     paste(file_name,"-", Sys.Date(), ".", extension, sep="")
    #   },
    #   content = function(filename) {
    #     if (extension == "xlsx") {
    #       openxlsx::write.xlsx(df(), file)
    #     }
    #   }
    # )

  })
}

## To be copied in the UI
# mod_save_button_ui("export_button_ui_1")

## To be copied in the server
# mod_save_button_server("export_button_ui_1")
