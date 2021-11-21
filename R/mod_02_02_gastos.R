#' mod_02_02_gastos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_gastos_ui <- function(id){
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
        tabPanel("obras",
                 htmltools::HTML("<strong>Fuente: R Icaro, Deuda Flotante SIIF (rdeu012), ",
                                 "Resumen de Rendiciones por Prov. SGF y ",
                                 "Litado Proveedores SGF</strong>")
                 ),
        # tabPanel("sueldo",
        #          htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
        #                          "y Gastos SIIF (rcg01_uejp)</strong>")
        #          ),
        # tabPanel("honorarios",
        #          htmltools::HTML("<strong>Fuente: R Icaro, Gastos SIIF (rcg01_uejp), ",
        #                          "y Gastos por Grupo SIIF (gto_rpa03g)</strong>")
        #          )
        tabPanel("pa6",
                 htmltools::HTML("<strong>Fuente: Gastos SIIF (rcg01_uejp) y ",
                                 "Comprobantes Fondos SIIF (rfondo07tp - PA6)</strong>")
                 ),
        tabPanel("debitos_bancarios",
                 htmltools::HTML("<strong>Fuente: Gastos SIIF (rcg01_uejp), ",
                                 "Gastos por Gpo Partida SIIF (gto_rpa03g) ",
                                 "y Sist. Seg. Ctas. Ctes. INVICO</strong>")
        ),
        tabPanel("fei",
                 htmltools::HTML("<strong>Fuente: Libro Mayor SIIF (rcocc31 - 2113-2-9) y ",
                                 "Rensumen de Rendiciones SGF</strong>")
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
        title = "Obras",
        value = "obras",
        mod_data_table_ui(ns("dt_obras"))
      ),
      # shiny::tabPanel(
      #   title = "Sueldo",
      #   value = "sueldo",
      #   mod_data_table_ui(ns("dt_sueldo"))
      # ),
      # shiny::tabPanel(
      #   title = "Honorarios Factureros",
      #   value = "honorarios",
      #   mod_data_table_ui(ns("dt_honorarios"))
      # ),
      shiny::tabPanel(
        title = "Reg. PA6",
        value = "pa6",
        mod_data_table_ui(ns("dt_pa6"))
      ),
      shiny::tabPanel(
        title = "Debitos Bancarios",
        value = "debitos_bancarios",
        mod_data_table_ui(ns("dt_debitos_bancarios"))
      ),
      shiny::tabPanel(
        title = "FEI",
        value = "fei",
        mod_data_table_ui(ns("dt_fei"))
      ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("filter"),
        tabsetPanel(
          id = ns("switcher"), type = "hidden",
          tabPanel("obras",
                   mod_02_02_01_obras_ui(ns("filter_obras"))
                   ),
          # tabPanel("sueldo",
          #          mod_02_02_02_sueldo(ns("filter_sueldo"))
          #           ),
          # tabPanel("honorarios",
          #          mod_02_02_03_honorarios_ui(ns("filter_honorarios"))
          #           )
          tabPanel("pa6",
                   mod_02_02_04_pa6_ui(ns("filter_pa6"))
                    ),
          tabPanel("debitos_bancarios",
                   mod_02_02_05_debitos_bancarios_ui(ns("filter_debitos_bancarios"))
          ),
          tabPanel("fei",
                   mod_02_02_06_fei_ui(ns("filter_fei"))
          )
        )
      )
    )

  )
}

#' 01_a_siif_recursos Server Functions
#'
#' @noRd
mod_02_02_gastos_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      # Ans <- switch(input$controller,
      #               anual = list(data = anual()),
      #               mensual = list(data = mensual()),
      #               registro = list(data = registro()),
      #               stop("Invalid `x` value")
      # )

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


    # Table Control obras
    obras <- mod_02_02_01_obras_server("filter_obras")

    shiny::observeEvent(obras(), {

      formatr_obras <- list(columns = c("ejecutado_icaro", "bruto_sgf",
                                        "diferencia", "dif_acum"))
      formatp_obras <- list(columns = c("prop_desv"))

      mod_data_table_server("dt_obras", obras,
                            format_round = formatr_obras,
                            format_perc = formatp_obras,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })

    pa6 <- mod_02_02_04_pa6_server("filter_pa6")

    shiny::observeEvent(pa6(), {

      formatr_pa6 <- list(columns = c("monto_reg", "monto_pa6",
                                        "saldo_pa6"))

      mod_data_table_server("dt_pa6", pa6,
                            # container = sketch,
                            format_round = formatr_pa6,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })

    debitos_bancarios <- mod_02_02_05_debitos_bancarios_server("filter_debitos_bancarios")

    shiny::observeEvent(debitos_bancarios(), {

      formatr_debitos_bancarios <- list(columns = c("ejecutado_siif", "debitos_sscc",
                                                    "diferencia", "dif_acum"))

      formatp_debitos_bancarios <- list(columns = c("prop_desv"))

      mod_data_table_server("dt_debitos_bancarios", debitos_bancarios,
                            # container = sketch,
                            format_round = formatr_debitos_bancarios,
                            format_perc = formatp_debitos_bancarios,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })

    fei <- mod_02_02_06_fei_server("filter_fei")

    shiny::observeEvent(fei(), {

      formatr_fei <- list(columns = c("carga_fei", "pago_fei", "dif_fei",
                                      "bruto_sgf", "dif_pago", "dif_acum"))

      formatp_fei <- list(columns = c("prop_desv"))

      mod_data_table_server("dt_fei", fei,
                            # container = sketch,
                            format_round = formatr_fei,
                            format_perc = formatp_fei,
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
# mod_02_02_gastos_ui("mod_02_02_gastos_1")

## To be copied in the server
# mod_02_02_gastos_server("mod_02_02_gastos_1")