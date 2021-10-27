#' export_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_button_ui <- function(id, label_button = "", title = "Guardar como..." ,
                                 icon = "file-excel", filetype=list(xlsx="xlsx")){
  ns <- NS(id)
  tagList(
    shinyFiles::shinySaveButton(ns("download"), label = label_button,
                                title = title, icon = shiny::icon(icon))
  )
}

#' export_button Server Functions
#'
#' @noRd
mod_save_button_server <- function(id, df) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({

      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

      shinyFileSave(input, "download", roots = volumes, session = session, restrictions = system.file(package = "base"))

      fileinfo <- parseSavePath(volumes, input$download)

      if (nrow(fileinfo) > 0) {
        openxlsx::write.xlsx(df, as.character(fileinfo$datapath))
      }

    })

    # output$download <- shiny::downloadHandler(
    #   filename = function() {
    #     paste(file_name,"-", Sys.Date(), ".", extension, sep="")
    #   },
    #   content = function(file) {
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
