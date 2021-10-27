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
        bs4Dash::boxDropdownItem("Actualizar", icon = shiny::icon("sync")),
        bs4Dash::dropdownDivider(),
        bs4Dash::boxDropdownItem("Exportar xls", icon = shiny::icon("file-excel")),
        bs4Dash::boxDropdownItem("Exportar csv", icon = shiny::icon("file-csv"))
      ),
      shiny::tabPanel(
        title = "Presupuesto con Fuente",
        mod_data_table_ui(ns("pres_fte"))
      ),
      shiny::tabPanel(
        title = "Presupuesto con Descripcion",
        mod_data_table_ui(ns("pres_desc"))
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

    hide_columns_pres_fte <- c(2:5, 7, 9, 15)

    mod_data_table_server("pres_fte", siif_ppto_gtos_fte,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_pres_fte)
                            ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_pres_fte)
                            )
                          )

    hide_columns_pres_desc <- c(2, 4, 5, 6, 8, 10, 11, 13)

    mod_data_table_server("pres_desc", siif_ppto_gtos_desc,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_pres_desc)
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_pres_desc)
                          )
    )

  })
}

## To be copied in the UI
# mod_01_01_siif_presupuesto_ui("01_01_siif_presupuesto_ui_1")

## To be copied in the server
# mod_01_01_siif_presupuesto_server("01_01_siif_presupuesto_ui_1")
