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

  steps_pres_fte <- list(
    ingreso = paste0(
      "Ingrese al ", htmltools::strong("SIIF"),
      " y seleccione el menú ",
      htmltools::strong("Reportes / Generación de Reportes / SUB - SISTEMA DE CONTROL DE GASTOS")
    ),
    reporte = paste0(
      strong("Busque e ingrese"), " al reporte ",
      strong("rf602"), " o el codigo ", strong("38")
    ),
    ejercicio = paste0(
      "Ingrese el ", strong("Ejercicio"),
      " para el cual desea obtener el reporte y seleccione el formato a exportar como ",
      strong("XLS")
    ),
    exportar = paste0(
      "Presione el botón ", strong("Ver Reporte")
    ),
    guardar = paste0(
      strong("Guardar"), " el archivo generado en una ubicación que recuerde"
    ),
    importar = paste0(
      strong("Importar"), " el archivo descargado previamente"
    )
  )

  steps_pres_desc <- steps_pres_fte
  steps_pres_desc$reporte <- paste0(
    strong("Busque e ingrese"), " al reporte ",
    strong("rf610"), " o el codigo ", strong("7")
  )



  tagList(
    bs4Dash::tabBox(
      id = ns("controller"),
      type = "tabs",
      status = "olive",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      maximizable = TRUE,
      elevation = NULL,
      boxToolSize = "lg",
      dropdownMenu =  bs4Dash::boxDropdown(
        icon = shiny::icon("save"),
        bs4Dash::boxDropdownItem(mod_save_button_ui(ns("download_xls"), "Exportar xls",
                                                    icon = shiny::icon("file-excel"),
                                                    filetype=list(xlsx="xlsx"))),
        bs4Dash::boxDropdownItem(mod_save_button_ui(ns("download_csv"), "Exportar csv",
                                                    icon = shiny::icon("file-csv"),
                                                    filetype=list(csv="csv"))),
        bs4Dash::boxDropdownItem(mod_download_button_ui(ns("download_dh"),
                                                        "Download Handler")),
        bs4Dash::boxDropdownItem(bs4Dash::actionButton(ns("bs4Dash_button"), "bs4Dash"))
      ),
      shiny::tabPanel(
        title = "Presupuesto con Fuente",
        value = "pres_fte",
        mod_data_table_ui(ns("pres_fte"))
      ),
      shiny::tabPanel(
        title = "Presupuesto con Descripcion",
        value = "pres_desc",
        mod_data_table_ui(ns("pres_desc"))
      ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("sync-alt"),
        htmltools::h4("Actualizar Base de Datos", style="text-align: center;"),
        rep_br(),
        mod_file_input_ui(ns("update"), multiple = TRUE),
        htmltools::h5("Pasos a seguir para importar:"),
        tabsetPanel(id = ns("switcher"), type = "hidden",
                    tabPanel("pres_fte",
                             htmltools::tags$ol(
                               list_to_li(steps_pres_fte)
                             )
                    ),
                    tabPanel("pres_desc",
                             htmltools::tags$ol(
                               list_to_li(steps_pres_desc)
                             )
                    )
        )
        # htmltools::tags$ol(shiny::htmlOutput(ns("steps")))
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

    df <- shiny::reactiveVal()

    fct <- shiny::reactiveVal()

    ext <- shiny::reactiveVal()


    observeEvent(input$controller, {

      if (input$controller == "pres_fte") {
        data <- siif_ppto_gtos_fte()
        import_function <- invicodatr::rpw_siif_ppto_gtos_fte
        require_extension <- "csv"
      }

      if (input$controller == "pres_desc") {
        data <- siif_ppto_gtos_desc()
        import_function <- invicodatr::rpw_siif_ppto_gtos_desc
        require_extension <- "csv"
      }

      df(data)
      fct(import_function)
      ext(require_extension)

      updateTabsetPanel(inputId = "switcher", selected = input$controller)

    })

    mod_save_button_server("download_xls", df)

    mod_save_button_server("download_csv", df)

    mod_file_input_server("update",
                          import_function = fct,
                          require_extension = ext)

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
