#' 01_a_siif_recursos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_01_recursos_ui <- function(id){
  ns <- NS(id)

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
      # height = "400px",
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
        title = "Recursos vs Banco SSCC",
        value = "rec_vs_sscc",
        mod_data_table_ui(ns("dt_rec_vs_sscc"))
      ),
      shiny::tabPanel(
        title = "Recursos vs Banco SIIF",
        value = "rec_vs_siif",
        mod_data_table_ui(ns("dt_rec_vs_siif"))
      ),
      shiny::tabPanel(
        title = "Recurso 3% vs Cod. Ret 337",
        value = "rec_vs_invico",
        mod_data_table_ui(ns("dt_rec_vs_invico"))
      ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("filter"),
        tabsetPanel(
          id = ns("switcher"), type = "hidden",
          tabPanel("rec_vs_sscc",
                   mod_02_01_01_rec_vs_sscc_ui(ns("filter_rec_vs_sscc"))
                   ),
          tabPanel("rec_vs_siif",

                    ),
          tabPanel("rec_vs_invico",

                    )
        )
      )
    )

  )
}

#' 01_a_siif_recursos Server Functions
#'
#' @noRd
mod_02_01_recursos_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      Ans <- switch(input$controller,
                    rec_vs_sscc = list(data = siif_comprobantes_rec_rci02(),
                                    import_function = invicodatr::rpw_siif_comprobantes_rec(),
                                    df_trigger = siif_comprobantes_rec_trigger),
                    rec_vs_siif = list(data = siif_pagos_rtr03(),
                                 import_function = invicodatr::rpw_siif_pagos,
                                 df_trigger = siif_pagos_trigger),
                    rec_vs_invico = list(data = siif_retenciones_por_codigo_rao01(),
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


    #Table Recursos SIIF vs SSCC Banco INVICO
    rec_vs_sscc <- mod_02_01_01_rec_vs_sscc_server("filter_rec_vs_sscc")

    formatr_rec_vs_sscc <- list(columns = c("recursos_siif", "depositos_sscc",
                                     "diferencia", "dif_acum"))
    formatp_rec_vs_sscc <- list(columns = "prop_desv")

    mod_data_table_server("dt_rec_vs_sscc", rec_vs_sscc,
                          format_round = formatr_rec_vs_sscc,
                          format_perc = formatp_rec_vs_sscc,
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas')
                          )
    )

    # hide_columns_pagos <- c(11) #begins in 0
    #
    # mod_data_table_server("rec_vs_siif", siif_pagos_rtr03,
    #                       columnDefs = list(
    #                         list(visible=FALSE, targets = hide_columns_pagos)
    #                       ),
    #                       buttons = list(
    #                         list(
    #                           extend = 'collection',
    #                           buttons = c('copy', 'print','csv', 'excel', 'pdf'),
    #                           text = 'Download 100 primeras filas'),
    #                         list(
    #                           extend='colvis',
    #                           text="Mostrar / Ocultar columnas",
    #                           columns = hide_columns_pagos)
    #                       )
    # )
    #
    # hide_columns_ret_cod <- c(4) #begins in 0
    #
    # mod_data_table_server("rec_vs_invico", siif_retenciones_por_codigo_rao01,
    #                       columnDefs = list(
    #                         list(visible=FALSE, targets = hide_columns_ret_cod)
    #                       ),
    #                       buttons = list(
    #                         list(
    #                           extend = 'collection',
    #                           buttons = c('copy', 'print','csv', 'excel', 'pdf'),
    #                           text = 'Download 100 primeras filas'),
    #                         list(
    #                           extend='colvis',
    #                           text="Mostrar / Ocultar columnas",
    #                           columns = hide_columns_ret_cod)
    #                       )
    # )

  })
}

## To be copied in the UI
# mod_02_01_recursos_ui("02_01_recursos_ui_1")

## To be copied in the server
# mod_02_01_recursos_server("02_01_recursos_ui_1")
