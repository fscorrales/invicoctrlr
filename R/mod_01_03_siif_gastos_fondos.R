#' 01_03_siif_gastos_fondos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_03_siif_gastos_fondos_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 01_03_siif_gastos_fondos Server Functions
#'
#' @noRd 
mod_01_03_siif_gastos_fondos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_01_03_siif_gastos_fondos_ui("01_03_siif_gastos_fondos_ui_1")
    
## To be copied in the server
# mod_01_03_siif_gastos_fondos_server("01_03_siif_gastos_fondos_ui_1")
