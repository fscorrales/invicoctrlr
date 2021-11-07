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
      "Ingrese al <strong>SIIF</strong> y seleccione el men\u00fa ",
      "<strong>Reportes / Generaci\u00f3n de Reportes / SUB - SISTEMA ",
      "DE CONTROL DE GASTOS</strong>"
    ),
    reporte = paste0(
      "<strong>Busque e ingrese</strong> al reporte ",
      "<strong>rf602</strong> o el c\u00f3digo <strong>38</strong>"
    ),
    ejercicio = paste0(
      "Ingrese el <strong>Ejercicio</strong> para el cual desea obtener ",
      "el reporte y seleccione el formato a exportar como <strong>XLS</strong>"
    ),
    exportar = paste0(
      "Presione el bot\u00f3n <strong>Ver Reporte</strong>"
    ),
    guardar = paste0(
      "<strong>Guardar</strong> el archivo generado en una ubicaci\u00f3n que recuerde"
    ),
    importar = paste0(
      "<strong>Importar</strong> el archivo descargado previamente"
    )
  )

  steps_pres_desc <- steps_pres_fte
  steps_pres_desc$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte <strong>rf610</strong> ",
    "o el c\u00f3digo <strong>7</strong>"
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
                                                    filetype=list(csv="csv")))
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

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      Ans <- switch(input$controller,
                    pres_fte = list(data = siif_ppto_gtos_fte_rf602(),
                                    import_function = invicodatr::rpw_siif_ppto_gtos_fte,
                                    df_trigger = siif_ppto_gtos_fte_trigger),
                    pres_desc = list(data = siif_ppto_gtos_desc_rf610(),
                                     import_function = invicodatr::rpw_siif_ppto_gtos_desc,
                                     df_trigger = siif_ppto_gtos_desc_trigger),
                    stop("Invalid `x` value")
                    )

      rpw_controller$df <- Ans$data
      rpw_controller$fct <- Ans$import_function
      rpw_controller$trigger <- Ans$df_trigger

      updateTabsetPanel(inputId = "switcher", selected = input$controller)

      shinyjs::reset("update-file")
      shinyFeedback::hideFeedback("update-file")

    })

    mod_save_button_server("download_xls", reactive(rpw_controller$df))

    mod_save_button_server("download_csv", reactive(rpw_controller$df))

    mod_file_input_server("update",
                          import_function = reactive(rpw_controller$fct),
                          df_trigger = reactive(rpw_controller$trigger))

    hide_columns_pres_fte <- c(2:5, 7, 9, 15)

    formatr_rf602 <- list(columns = c("credito_original", "credito_vigente",
                                      "comprometido", "ordenado",
                                      "saldo", "pendiente"))

    mod_data_table_server("pres_fte", siif_ppto_gtos_fte_rf602,
                          format_round = formatr_rf602,
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

    formatr_rf610 <- list(columns = c("credito_original", "credito_vigente",
                                      "comprometido", "ordenado",
                                      "saldo"))

    mod_data_table_server("pres_desc", siif_ppto_gtos_desc_rf610,
                          format_round = formatr_rf610,
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
