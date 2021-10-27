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
      type = "tabs",
      status = "olive",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      maximizable = TRUE,
      elevation = 2,
      boxToolSize = "lg",
      dropdownMenu =  bs4Dash::boxDropdown(
        icon = shiny::icon("save"),
        bs4Dash::boxDropdownItem("Link to google", href = "http://www.google.com"),
        bs4Dash::boxDropdownItem("Item with inputId", id = "dropdown_item2"),
        bs4Dash::dropdownDivider(),
        bs4Dash::boxDropdownItem("item 3", href = "#", icon = icon("th"))
      ),
      shiny::tabPanel(
        title = "Presupuesto con Fuente",
        mod_data_table_ui(ns("pres_fte"))
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
    hide_columns <- c(3:6, 8, 10, 16)

    mod_data_table_server("pres_fte", siif_ppto_gtos_fte,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns)
                            ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns)
                            )
                          )

  })
}

## To be copied in the UI
# mod_01_01_siif_presupuesto_ui("01_01_siif_presupuesto_ui_1")

## To be copied in the server
# mod_01_01_siif_presupuesto_server("01_01_siif_presupuesto_ui_1")
