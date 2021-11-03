#' 01_a_siif_recursos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_02_siif_tesoreria_ui <- function(id){
  ns <- NS(id)

  steps_comp_rec <- list(
    ingreso = paste0(
      "Ingrese al <strong>SIIF</strong> y seleccione el men\u00fa ",
      "<strong>Reportes / Generaci\u00f3n de Reportes / SUB - SISTEMA ",
      "DE CONTROL DE RECURSOS</strong>"
    ),
    reporte = paste0(
      "<strong>Busque e ingrese</strong> al reporte ",
      "<strong>rci02</strong> o el c\u00f3digo <strong>33</strong>"
    ),
    filtro = paste0(
      "Ingrese el <strong>Ejercicio</strong> y el <strong>rango fecha</strong> ",
      "(preferentemente a\u00f1o calendario) para el cual desea obtener el reporte. "
    ),
    exportar = paste0(
      "Seleccione el formato a exportar como <strong>XLS</strong> y ",
      "presione el bot\u00f3n <strong>Ver Reporte</strong>"
    ),
    guardar = paste0(
      "<strong>Guardar</strong> el archivo generado en una ubicaci\u00f3n que recuerde"
    ),
    importar = paste0(
      "<strong>Importar</strong> el archivo descargado previamente"
    )
  )

  steps_pagos <- steps_comp_rec
  steps_pagos$ingreso <- paste0(
    "Ingrese al <strong>SIIF</strong> y seleccione el men\u00fa ",
    "<strong>Reportes / Generaci\u00f3n de Reportes / SUB - SISTEMA ",
    "DE CONTROL DE TESORERIA</strong>"
  )
  steps_pagos$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte ",
    "<strong>rtr03</strong> o el c\u00f3digo <strong>2146</strong>"
  )
  steps_pagos$filtro <- paste0(
    "Ingrese el <strong>rango fecha</strong> (preferentemente a\u00f1o calendario) ",
    "para el cual desea obtener el reporte, ",
    "dejando la <strong>cta. cte.</strong> en blanco"
  )

  steps_ret_cod <- steps_pagos
  steps_ret_cod$reporte <- paste0(
    "<strong>Busque e ingrese</strong> al reporte ",
    "<strong>rao01</strong> o el c\u00f3digo <strong>525</strong>"
  )
  steps_ret_cod$filtro <- paste0(
    "Ingrese el <strong>rango fecha</strong> (preferentemente a\u00f1o calendario) ",
    "para el cual desea obtener el reporte y el <strong>c\u00f3digo de retenci\u00f3n</strong>"
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
        title = "Comprobantes Recursos",
        value = "comp_rec",
        mod_data_table_ui(ns("comp_rec"))
      ),
      shiny::tabPanel(
        title = "Pagos",
        value = "pagos",
        mod_data_table_ui(ns("pagos"))
      ),
      shiny::tabPanel(
        title = "Retenciones x C\u00f3digo",
        value = "ret_cod",
        mod_data_table_ui(ns("ret_cod"))
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
                    tabPanel("comp_rec",
                             htmltools::tags$ol(
                               list_to_li(steps_comp_rec)
                             )
                    ),
                    tabPanel("pagos",
                             htmltools::tags$ol(
                               list_to_li(steps_pagos)
                             )
                    ),
                    tabPanel("ret_cod",
                             htmltools::tags$ol(
                               list_to_li(steps_ret_cod)
                             )
                    )
        )
      )
    )

  )
}

#' 01_a_siif_recursos Server Functions
#'
#' @noRd
mod_01_02_siif_tesoreria_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      Ans <- switch(input$controller,
                    comp_rec = list(data = siif_comprobantes_rec_rci02(),
                                    import_function = invicodatr::rpw_siif_comprobantes_rec(),
                                    df_trigger = siif_comprobantes_rec_trigger),
                    pagos = list(data = siif_pagos_rtr03(),
                                 import_function = invicodatr::rpw_siif_pagos,
                                 df_trigger = siif_pagos_trigger),
                    ret_cod = list(data = siif_retenciones_por_codigo_rao01(),
                                 import_function = invicodatr::rpw_siif_retenciones_por_codigo,
                                 df_trigger = siif_retenciones_por_codigo_trigger),
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

    hide_columns_comp_rec <- c(4:5, 8:10) #begins in 0

    mod_data_table_server("comp_rec", siif_comprobantes_rec_rci02,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_comp_rec)
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_comp_rec)
                          )
    )

    hide_columns_pagos <- c(11) #begins in 0

    mod_data_table_server("pagos", siif_pagos_rtr03,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_pagos)
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_pagos)
                          )
    )

    hide_columns_ret_cod <- c(4) #begins in 0

    mod_data_table_server("ret_cod", siif_retenciones_por_codigo_rao01,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_ret_cod)
                          ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_ret_cod)
                          )
    )

  })
}

## To be copied in the UI
# mod_01_a_siif_recursos_ui("01_a_siif_recursos_ui_1")

## To be copied in the server
# mod_01_a_siif_recursos_server("01_a_siif_recursos_ui_1")
