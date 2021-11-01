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

  steps_comp_gtos <- list(
    ingreso = paste0(
      "Ingrese al <strong>SIIF</strong> y seleccione el men\u00fa ",
      "<strong>Reportes / Generaci\u00f3n de Reportes / SUB - SISTEMA ",
      "DE CONTROL DE GASTOS</strong>"
    ),
    reporte = paste0(
      "<strong>Busque e ingrese</strong> al reporte ",
      "<strong>rcg01_uejp</strong> o el c\u00f3digo <strong>839</strong>"
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

  steps_comp_gtos_part <- steps_comp_gtos
  steps_comp_gtos_part$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte <strong>rcg01_par</strong> ",
    "o el c\u00f3digo <strong>2068</strong>"
  )

  steps_comp_gtos_gpo_part <- steps_comp_gtos
  steps_comp_gtos_gpo_part$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte <strong>gto_rpa03g</strong> ",
    "o el c\u00f3digo <strong>1175</strong>"
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
        title = "Comprobantes Gtos",
        value = "comp_gtos",
        mod_data_table_ui(ns("comp_gtos"))
      ),
      shiny::tabPanel(
        title = "Comprobantes Gtos + Partida",
        value = "comp_gtos_part",
        mod_data_table_ui(ns("comp_gtos_part"))
      ),
      shiny::tabPanel(
        title = "Comprobantes Gtos + Gpo Partida",
        value = "comp_gtos_gpo_part",
        mod_data_table_ui(ns("comp_gtos_gpo_part"))
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
                    tabPanel("comp_gtos",
                             htmltools::tags$ol(
                               list_to_li(steps_comp_gtos)
                             )
                    ),
                    tabPanel("comp_gtos_part",
                             htmltools::tags$ol(
                               list_to_li(steps_comp_gtos_part)
                             )
                    ),
                    tabPanel("comp_gtos_gpo_part",
                             htmltools::tags$ol(
                               list_to_li(steps_comp_gtos_gpo_part)
                             )
                    )
        )
      )
    )

  )
}

#' 01_03_siif_gastos_fondos Server Functions
#'
#' @noRd
mod_01_03_siif_gastos_fondos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      Ans <- switch(input$controller,
                    comp_gtos = list(data = siif_comprobantes_gtos_rcg01_uejp(),
                                    import_function = invicodatr::rpw_siif_comprobantes_gtos,
                                    df_trigger = siif_comprobantes_gtos_trigger),
                    comp_gtos_part = list(data = siif_comprobantes_gtos_partida_rcg01_par(),
                                     import_function = invicodatr::rpw_siif_comprobantes_gtos_partida,
                                     df_trigger = siif_comprobantes_gtos_partida_trigger),
                    comp_gtos_gpo_part = list(data = siif_comprobantes_gtos_gpo_partida_gto_rpa03g(),
                                          import_function = invicodatr::rpw_siif_comprobantes_gtos_gpo_partida,
                                          df_trigger = siif_comprobantes_gtos_gpo_partida_trigger),
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

    hide_columns_comp_gtos <- c(14:17) #begins in 0

    mod_data_table_server("comp_gtos", siif_comprobantes_gtos_rcg01_uejp,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_comp_gtos)
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_comp_gtos)
                          )
    )

    hide_columns_comp_gtos_part <- c(14:17) #begins in 0

    mod_data_table_server("comp_gtos_part", siif_comprobantes_gtos_partida_rcg01_par,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_comp_gtos_part )
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_comp_gtos_part )
                          )
    )

    hide_columns_comp_gtos_gpo_part <- c(1) #begins in 0

    mod_data_table_server("comp_gtos_gpo_part", siif_comprobantes_gtos_gpo_partida_gto_rpa03g,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_comp_gtos_gpo_part )
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_comp_gtos_gpo_part )
                          )
    )

  })
}

## To be copied in the UI
# mod_01_03_siif_gastos_fondos_ui("01_03_siif_gastos_fondos_ui_1")

## To be copied in the server
# mod_01_03_siif_gastos_fondos_server("01_03_siif_gastos_fondos_ui_1")
