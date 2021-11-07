#' 01_04_siif_contabilidad UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_04_siif_contabilidad_ui <- function(id){
  ns <- NS(id)

  steps_mayor <- list(
    ingreso = paste0(
      "Ingrese al <strong>SIIF</strong> y seleccione el men\u00fa ",
      "<strong>Reportes / Generaci\u00f3n de Reportes / SUB - SISTEMA ",
      "DE CONTROL DE CONTABILIDAD PATRIMONIAL</strong>"
    ),
    reporte = paste0(
      "<strong>Busque e ingrese</strong> al reporte ",
      "<strong>rcocc31</strong> o el c\u00f3digo <strong>387</strong>"
    ),
    filtro = paste0(
      "Ingrese el <strong>Ejercicio</strong>, el c\u00f3digo de cuenta " ,
      "(<strong>nivel, mayor, subcuenta</strong>) para el cual desea obtener ",
      "el reporte. En cuanto al <strong>rango fecha</strong>, puede seleccionar ",
      "per\u00edodos que superen al a\u00f1o calendario dado que los asientos ",
      "de ajuste se registran por fuera del mismo"
    ),
    exportar = paste0(
      "Seleccione el formato a exportar como <strong>XLS</strong> y ",
      "presione el bot\u00f3n <strong>Ver Reporte</strong>"
    ),
    guardar = paste0(
      "<strong>Guardar</strong> el archivo generado en una ubicaci\u00f3n que recuerde"
    ),
    importar = paste0(
      "<strong>Importar</strong> el/los archivo/s descargado/s previamente. ",
      "Es posible importar m\u00e1s de un archivo a la vez del mismo reporte."
    )
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
        title = "Mayor",
        value = "mayor",
        mod_data_table_ui(ns("mayor"))
      ),
      # shiny::tabPanel(
      #   title = "Presupuesto con Descripcion",
      #   value = "pres_desc",
      #   mod_data_table_ui(ns("pres_desc"))
      # ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("sync-alt"),
        htmltools::h4("Actualizar Base de Datos", style="text-align: center;"),
        rep_br(),
        mod_file_input_ui(ns("update"), multiple = TRUE),
        htmltools::h5("Pasos a seguir para importar:"),
        tabsetPanel(id = ns("switcher"), type = "hidden",
                    tabPanel("mayor",
                             htmltools::tags$ol(
                               list_to_li(steps_mayor)
                             )
                    )
                    # tabPanel("pres_desc",
                    #          htmltools::tags$ol(
                    #            list_to_li(steps_pres_desc)
                    #          )
                    # )
        )
      )
    )
  )
}

#' 01_04_siif_contabilidad Server Functions
#'
#' @noRd
mod_01_04_siif_contabilidad_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      Ans <- switch(input$controller,
                    mayor = list(data = siif_mayor_contable_rcocc31(),
                                 import_function = invicodatr::rpw_siif_mayor_contable,
                                 df_trigger = siif_mayor_contable_trigger),
                    # pres_desc = list(data = siif_ppto_gtos_desc_rf610(),
                    #                  import_function = invicodatr::rpw_siif_ppto_gtos_desc,
                    #                  df_trigger = siif_ppto_gtos_desc_trigger),
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

    hide_columns_mayor <- c(2)

    formatr_rcocc31 <- list(columns = c("debitos", "creditos",
                                        "saldo"))

    mod_data_table_server("mayor", siif_mayor_contable_rcocc31,
                          format_round = formatr_rcocc31,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_mayor)
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_mayor)
                          )
    )

  })
}

## To be copied in the UI
# mod_01_04_siif_contabilidad_ui("01_04_siif_contabilidad_ui_1")

## To be copied in the server
# mod_01_04_siif_contabilidad_server("01_04_siif_contabilidad_ui_1")
