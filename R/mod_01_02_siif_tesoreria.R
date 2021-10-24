#' 01_a_siif_recursos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_02_siif_tesoreria_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::tabsetPanel(
      id = ns(tab_id$bd_tesoreria),
      type = "pills",
      shiny::tabPanel(
        title = "Tab 1",
        "Content 1"
      ),
      shiny::tabPanel(
        title = "Tab 2",
        "Content 2"
      )
    )

  )
}

#' 01_a_siif_recursos Server Functions
#'
#' @noRd
mod_01_02_siif_tesoreria_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_01_a_siif_recursos_ui("01_a_siif_recursos_ui_1")

## To be copied in the server
# mod_01_a_siif_recursos_server("01_a_siif_recursos_ui_1")
