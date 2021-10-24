#' 01_01_siif_presupuesto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_01_siif_presupuesto_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::tabBox(
      id = ns(tab_id$bd_presupuesto),
      type = "pills",
      width = 12,
      status = "primary",
      collapsible = FALSE,
      shiny::tabPanel(
        title = "Presupuesto con Fuente",
        mod_01_00_base_de_datos_ui(ns("pres_fte"))
      ),
      shiny::tabPanel(
        title = "Presupuesto con Descripcion",
        "Content 2"
      )
    )
  )
}

#' 01_01_siif_presupuesto Server Functions
#'
#' @noRd
mod_01_01_siif_presupuesto_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_01_00_base_de_datos_server("pres_fte", reactive(iris))

  })
}

## To be copied in the UI
# mod_01_01_siif_presupuesto_ui("01_01_siif_presupuesto_ui_1")

## To be copied in the server
# mod_01_01_siif_presupuesto_server("01_01_siif_presupuesto_ui_1")
