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
      height = "600px",
      collapsible = FALSE,
      maximizable = TRUE,
      elevation = NULL,
      footer = tabsetPanel(
        id = ns("switcher_footer"), type = "hidden",
        tabPanel("rec_vs_sscc",
                 htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
                                 "y Resumen Gral. de Mov. SSCC</strong>")
                 ),
        tabPanel("rec_vs_siif",
                 htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
                                 "y Mayor SIIF (rcocc31 - 1112-2-6)</strong>")
                 ),
        tabPanel("rec_vs_invico",
                 htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
                                 "y Mayor SIIF (rcocc31 - 1112-2-6 y 2122-1-2)</strong>")
                 )
        ),
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
        easyClose = FALSE,
        icon = shiny::icon("filter"),
        tabsetPanel(
          id = ns("switcher"), type = "hidden",
          tabPanel("rec_vs_sscc",
                   mod_02_01_01_rec_vs_sscc_ui(ns("filter_rec_vs_sscc"))
                   ),
          tabPanel("rec_vs_siif",
                   mod_02_01_02_rec_vs_siif_ui(ns("filter_rec_vs_siif"))
                    ),
          tabPanel("rec_vs_invico",
                   mod_02_01_03_rec_vs_invico_ui(ns("filter_rec_vs_invico"))
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

      ## DANGER, THIS IS NOT WORKING. TRY ANOTHER WAY
      # Ans <- switch(input$controller,
      #               rec_vs_sscc = list(data = rec_vs_sscc()),
      #               rec_vs_siif = list(data = rec_vs_siif()),
      #               rec_vs_invico = list(data = rec_vs_invico()),
      #               stop("Invalid `x` value")
      # )
      #
      # rpw_controller$df <- Ans$data
      # rpw_controller$fct <- Ans$import_function
      # rpw_controller$trigger <- Ans$df_trigger

      updateTabsetPanel(inputId = "switcher", selected = input$controller)
      updateTabsetPanel(inputId = "switcher_footer", selected = input$controller)

      shinyjs::reset("update-file")
      shinyFeedback::hideFeedback("update-file")

    })

    # mod_save_button_server("download_xls", reactive(rpw_controller$df))
    #
    # mod_save_button_server("download_csv", reactive(rpw_controller$df))


    #Table Recursos SIIF vs SSCC Banco INVICO
    rec_vs_sscc <- mod_02_01_01_rec_vs_sscc_server("filter_rec_vs_sscc")

    shiny::observeEvent(rec_vs_sscc(), {

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

    })




    #Table Recursos SIIF vs Banco SIIF
    rec_vs_siif <- mod_02_01_02_rec_vs_siif_server("filter_rec_vs_siif")

    shiny::observeEvent(rec_vs_siif(), {

      formatr_rec_vs_siif <- list(columns = c("recursos_siif", "debitos_banco_siif",
                                              "diferencia", "dif_acum"))
      formatp_rec_vs_siif <- list(columns = "prop_desv")

      mod_data_table_server("dt_rec_vs_siif", rec_vs_siif,
                            format_round = formatr_rec_vs_siif,
                            format_perc = formatp_rec_vs_siif,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })



    #Table Recursos 337 vs Codigo Retencion 337
    rec_vs_invico <- mod_02_01_03_rec_vs_invico_server("filter_rec_vs_invico")

    shiny::observeEvent(rec_vs_invico(), {

      formatr_rec_vs_invico <- list(columns = c("recursos_siif", "gastos_337_siif",
                                                "pagos_337_siif", "dif_pagado_337",
                                                "dif_ingresado", "dif_acum"))
      formatp_rec_vs_invico <- list(columns = "prop_desv")

      mod_data_table_server("dt_rec_vs_invico", rec_vs_invico,
                            format_round = formatr_rec_vs_invico,
                            format_perc = formatp_rec_vs_invico,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })

  })
}

## To be copied in the UI
# mod_02_01_recursos_ui("02_01_recursos_ui_1")

## To be copied in the server
# mod_02_01_recursos_server("02_01_recursos_ui_1")
