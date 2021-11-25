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
    filtro = paste0(
      "Ingrese el <strong>Ejercicio</strong> y el <strong>rango fecha</strong> ",
      "(preferentemente a\u00f1o calendario) para el cual desea obtener el reporte. ",
      "La <strong>unidad ejecutora</strong> debe quedar con valor 0"
    ),
    exportar = paste0(
      "Seleccione el formato a exportar como <strong>XLS</strong> y presione ",
      "el bot\u00f3n <strong>Ver Reporte</strong>"
    ),
    guardar = paste0(
      "<strong>Guardar</strong> el archivo generado en una ubicaci\u00f3n que recuerde"
    ),
    importar = paste0(
      "<strong>Importar</strong> el/los archivo/s descargado/s previamente. ",
      "Es posible importar m\u00e1s de un archivo a la vez del mismo reporte."
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
  steps_comp_gtos_gpo_part$filtro <- paste0(
    "Ingrese el <strong>Ejercicio</strong> y el <strong>rango de meses</strong> ",
    "(preferentemente a\u00f1o calendario) para el cual desea obtener el reporte. ",
    "Deber\u00e1 exportar un archivo por cada ",
    "<strong>grupo partida</strong> (1, 2, 3, 4, etc.)"
  )

  steps_deuda <- steps_comp_gtos
  steps_deuda$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte <strong>rdeu012</strong> ",
    "o el c\u00f3digo <strong>267</strong>"
  )
  steps_deuda$filtro <- paste0(
    "Ingrese el valor 0 como <strong>c\u00f3digo fuente</strong>, 01-01-2010 ",
    "como <strong>fecha desde</strong> y dejar en blanco el campo ",
    "<strong>clase del gasto</strong>. Por \u00faltimo, deber\u00e1 ingresar ",
    "la <strong>fecha hasta</strong> de acuerdo a su preferencia teniendo en ",
    "cuenta de elegir el \u00faltimo d\u00eda del mes"
  )

  steps_comp_fdos <- steps_comp_gtos
  steps_comp_fdos$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte <strong>rfondo07tp</strong> ",
    "o el c\u00f3digo <strong>2070</strong>"
  )
  steps_comp_fdos$filtro = paste0(
    "Ingrese el <strong>Ejercicio</strong>, el <strong>rango fecha</strong> ",
    "(preferentemente a\u00f1o calendario) y el <strong>tipo de comprobante",
    "</strong> para el cual desea obtener el reporte"
  )


  tagList(
    bs4Dash::tabBox(
      id = ns("controller"),
      type = "tabs",
      status = "olive",
      solidHeader = TRUE,
      width = 12,
      height = "600px",
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
        title = "Comp. Gtos",
        value = "comp_gtos",
        mod_data_table_ui(ns("comp_gtos"))
      ),
      shiny::tabPanel(
        title = "Comp. Gtos + Partida",
        value = "comp_gtos_part",
        mod_data_table_ui(ns("comp_gtos_part"))
      ),
      shiny::tabPanel(
        title = "Comp. Gtos + Gpo Partida",
        value = "comp_gtos_gpo_part",
        mod_data_table_ui(ns("comp_gtos_gpo_part"))
      ),
      shiny::tabPanel(
        title = "Deuda Flotante",
        value = "deuda",
        mod_data_table_ui(ns("deuda"))
      ),
      shiny::tabPanel(
        title = "Comp. Fdos.",
        value = "comp_fdos",
        mod_data_table_ui(ns("comp_fdos"))
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
                    ),
                    tabPanel("deuda",
                             htmltools::tags$ol(
                               list_to_li(steps_deuda)
                             )
                    ),
                    tabPanel("comp_fdos",
                             htmltools::tags$ol(
                               list_to_li(steps_comp_fdos)
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
                    deuda = list(data = siif_deuda_flotante_rdeu012(),
                                 import_function = invicodatr::rpw_siif_deuda_flotante,
                                 df_trigger = siif_deuda_flotante_trigger),
                    comp_fdos = list(data = siif_resumen_fdos_rfondo07tp(),
                                     import_function = invicodatr::rpw_siif_resumen_fdos,
                                     df_trigger = siif_resumen_fdos_trigger),
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

    formatr_rcg01_uejp <- list(columns = c("monto"))

    mod_data_table_server("comp_gtos", siif_comprobantes_gtos_rcg01_uejp,
                          format_round = formatr_rcg01_uejp,
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

    formatr_rcg01_par <- list(columns = c("monto"))

    mod_data_table_server("comp_gtos_part", siif_comprobantes_gtos_partida_rcg01_par,
                          format_round = formatr_rcg01_par,
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

    formatr_gto_rpa03g <- list(columns = c("monto"))

    mod_data_table_server("comp_gtos_gpo_part", siif_comprobantes_gtos_gpo_partida_gto_rpa03g,
                          format_round = formatr_gto_rpa03g,
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

    hide_columns_deuda <- c(0, 7) #begins in 0

    formatr_rdeu012 <- list(columns = c("monto", "saldo"))

    mod_data_table_server("deuda", siif_deuda_flotante_rdeu012,
                          format_round = formatr_rdeu012,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_deuda )
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_deuda)
                          )
    )

    hide_columns_comp_fdos <- c(7) #begins in 0

    formatr_rfondo07tp <- list(columns = c("ingresos", "egresos", "saldo"))

    mod_data_table_server("comp_fdos", siif_resumen_fdos_rfondo07tp,
                          format_round = formatr_rfondo07tp,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_comp_fdos )
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_comp_fdos)
                          )
    )

  })
}

## To be copied in the UI
# mod_01_03_siif_gastos_fondos_ui("01_03_siif_gastos_fondos_ui_1")

## To be copied in the server
# mod_01_03_siif_gastos_fondos_server("01_03_siif_gastos_fondos_ui_1")
