#' 03_00_tabla_dinamica UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_00_tabla_dinamica_ui <- function(id){

  ns <- NS(id)
  tablas_anexas <- c()
  tablas <- c(tablas_anexas,
              sapply(invicodatr::list_tables_sqlite("SIIF"),
                     function(x) paste0("siif_", x), USE.NAMES = FALSE),
              sapply(invicodatr::list_tables_sqlite("SSCC"),
                     function(x) paste0("sscc_", x), USE.NAMES = FALSE),
              sapply(invicodatr::list_tables_sqlite("SGF"),
                     function(x) paste0("sgf_", x), USE.NAMES = FALSE),
              sapply(invicodatr::list_tables_sqlite("SGO"),
                     function(x) paste0("sgo_", x), USE.NAMES = FALSE),
              sapply(invicodatr::list_tables_sqlite("ICARO"),
                     function(x) paste0("icaro_", x), USE.NAMES = FALSE))

  steps_td <- list(
    choose = paste0(
      "Deberá elegir una de las tablas del siguiente listado"
    ),
    custom = paste0(
      "Personalice la tabla en función de la información que ",
      "desea obtener. Puede ubicar los campos en filas o columnas ",
      "(arrastrándolos), filtrar los mismos y elegir una función ",
      "de agregación (suma, cuenta, etc.)"
    ),
    output = paste0(
      "Puede seleccionar distintos tipos de salida (tablas o gráficos)"
    )
  )


  tagList(
    bs4Dash::box(
      id = ns("controller"),
      status = "olive",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      maximizable = TRUE,
      elevation = NULL,
      boxToolSize = "lg",
      # tags$head(tags$style(
      #   type = 'text/css',
      #   paste0("#", ns("td"), " { overflow-x: scroll; }")
      # )),
      rpivotTable::rpivotTableOutput(ns("td")),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = TRUE,
        icon = shiny::icon("cogs"),
        htmltools::h4("Selección de Tabla base", style="text-align: center;"),
        rep_br(),
        htmltools::h5("¿Cómo usar la tabla dinámica?:"),
        htmltools::tags$ol(
          list_to_li(steps_td)
        ),
        rep_br(),
        selectizeInput(ns("tabla"), "Selección de Tabla",
                       choices = tablas, selected = "", multiple = F,
                       options = list(placeholder = "Elegir una opción"))
        )
      )
  )
}

#' 03_00_tabla_dinamica Server Functions
#'
#' @noRd
mod_03_00_tabla_dinamica_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$td = rpivotTable::renderRpivotTable({
      rpivotTable::rpivotTable(siif_ppto_gtos_fte_rf602(),
                  rendererName="Tabla Dinamica R INVICO"
  #                 onRefresh = htmlwidgets::JS(paste0("function(config) {Shiny.onInputChange('", ns("myData"), "',
  #                                           document.getElementById('", ns("table"),"').innerHTML);
  # }"))
                  )
    })

  })
}

## To be copied in the UI
# mod_03_00_tabla_dinamica_ui("03_00_tabla_dinamica_ui_1")

## To be copied in the server
# mod_03_00_tabla_dinamica_server("03_00_tabla_dinamica_ui_1")
