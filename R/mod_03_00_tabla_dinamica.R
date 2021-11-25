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

  # tablas_anexas <- c()
  # r6_siif <- MyConnection$new("siif")
  # r6_sscc <- MyConnection$new("sscc")
  # r6_sgf <- MyConnection$new("sgf")
  #             sapply(r6_siif$tables,
  #                    function(x) paste0("siif_", x), USE.NAMES = FALSE),
  #             sapply(r6_sscc$tables,
  #                    function(x) paste0("sscc_", x), USE.NAMES = FALSE),
  #             sapply(r6_sgf$tables,
  #                    function(x) paste0("sgf_", x), USE.NAMES = FALSE))
  #             # sapply(invicodatr::list_tables_sqlite("SGO"),
  #             #        function(x) paste0("sgo_", x), USE.NAMES = FALSE),
  #             # sapply(invicodatr::list_tables_sqlite("ICARO"),
  #             #        function(x) paste0("icaro_", x), USE.NAMES = FALSE))

  # r6_siif$finalize()
  # r6_sscc$finalize()
  # r6_sgf$finalize()

  steps_td <- list(
    choose = paste0(
      "Deber\u00e1 elegir una de las tablas del siguiente listado"
    ),
    custom = paste0(
      "Personalice la tabla en funci\u00f3n de la informaci\u00f3n que ",
      "desea obtener. Puede ubicar los campos en filas o columnas ",
      "(arrastr\u00e1ndolos), filtrar los mismos y elegir una funci\u00f3n ",
      "de agregaci\u00f3n (suma, cuenta, etc.)"
    ),
    output = paste0(
      "Puede seleccionar distintos tipos de salida (tablas o gr\u00e1ficos)"
    )
  )


  tagList(
    bs4Dash::box(
      id = ns("controller"),
      status = "olive",
      title = "Tabla Din\u00e1mica",
      solidHeader = TRUE,
      width = 12,
      height = "600px",
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
        htmltools::h4("Selecci\u00f3n de Tabla base", style="text-align: center;"),
        rep_br(),
        htmltools::h5("\u00bfC\u00f3mo usar la tabla din\u00e1mica?:"),
        htmltools::tags$ol(
          list_to_li(steps_td)
        ),
        rep_br(),
        selectizeInput(ns("tabla"), "Selecci\u00f3n de Tabla",
                       choices = "", selected = "", multiple = F,
                       options = list(placeholder = "Elegir una opci\u00f3n")),
        shiny::column(12, align = "center",
                      bs4Dash::actionButton(ns("update_td"),
                                            "Insertar Tabla Din\u00e1mica",
                                            status = "primary"))

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

    td <- shiny::reactiveVal()

    observe({

      r6_siif <- MyConnection$new(sql_path("siif"))
      r6_sscc <- MyConnection$new(sql_path("sscc"))
      r6_sgf <- MyConnection$new(sql_path("sgf"))

      tablas <- c(sapply(r6_siif$tables,
                         function(x) paste0("siif_", x), USE.NAMES = FALSE),
                  sapply(r6_sscc$tables,
                         function(x) paste0("sscc_", x), USE.NAMES = FALSE),
                  sapply(r6_sgf$tables,
                         function(x) paste0("sgf_", x), USE.NAMES = FALSE))
                  # sapply(invicodatr::list_tables_sqlite("SGO"),
                  #        function(x) paste0("sgo_", x), USE.NAMES = FALSE),
                  # sapply(invicodatr::list_tables_sqlite("ICARO"),
                  #        function(x) paste0("icaro_", x), USE.NAMES = FALSE))

      shiny::updateSelectizeInput(session, "tabla", choices = tablas)

      r6_siif$finalize()
      r6_sscc$finalize()
      r6_sgf$finalize()

    })

    observeEvent(input$update_td, {

      req(input$tabla)

      td(eval(call(input$tabla)))

      output$td = rpivotTable::renderRpivotTable({
        rpivotTable::rpivotTable(td(),
                                 rendererName="Tabla Dinamica R INVICO"
                                 #                 onRefresh = htmlwidgets::JS(paste0("function(config) {Shiny.onInputChange('", ns("myData"), "',
                                 #                                           document.getElementById('", ns("table"),"').innerHTML);
                                 # }"))
        )
      })

    })



  })
}

## To be copied in the UI
# mod_03_00_tabla_dinamica_ui("03_00_tabla_dinamica_ui_1")

## To be copied in the server
# mod_03_00_tabla_dinamica_server("03_00_tabla_dinamica_ui_1")
