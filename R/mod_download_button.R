#' export_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_button_ui <- function(id, label_button = "",
                                 icon = "download"){
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), label = label_button,
                   icon = shiny::icon(icon),
                   width = "125px", class = "btn-primary")
  )
}

#' export_button Server Functions
#'
#' @noRd
mod_download_button_server <- function(id, df, file_name = "R INVICO",
                                     extension = "xlsx"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste(file_name,"-", Sys.Date(), ".", extension, sep="")
      },
      content = function(file) {
        if (extension == "xlsx") {
          openxlsx::write.xlsx(df(), file)
        }
      }
    )

  })
}

## To be copied in the UI
# mod_download_button_ui("export_button_ui_1")

## To be copied in the server
# mod_download_button_server("export_button_ui_1")
