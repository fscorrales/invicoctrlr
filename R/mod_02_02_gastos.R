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
                                 "Comprobantes Fondos SIIF (rfondo07tp - PA6))</strong>")
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

      # sketch = htmltools::withTags(table(
      #   class = 'display',
      #   thead(
      #     tr(
      #       th(class = 'dt-center', colspan = 3, 'fuente'),
      #       th(class = 'dt-center', colspan = 3, 'monto'),
      #       th(class = 'dt-center', colspan = 3, 'cta_cte'),
      #       th(class = 'dt-center', colspan = 3, 'cuit'),
      #       th(class = 'dt-center', colspan = 3, 'nro_entrada'),
      #       th(class = 'dt-center', colspan = 3, 'fecha'),
      #       th(class = 'dt-center', colspan = 3, 'partida'),
      #     ),
      #     tr(
      #       lapply(rep(c('siif', 'icaro', "dif"), 7), th)
      #     )
      #   )
      # ))

      formatr_pa6 <- list(columns = c("monto_reg", "monto_pa6",
                                        "saldo_pa6"))

      # formats_registro <- list(columns = c("dif_fuente", "dif_monto", "dif_cta_cte",
      #                                      "dif_cuit", "dif_nro_entrada",
      #                                      "dif_fecha", "dif_partida"),
      #                          color = DT::styleEqual(
      #                            c("\u2713", "\u2718"), c('green', 'red')),
      #                          fontWeight = "bold",
      #                          fontSize = "24px")

      mod_data_table_server("dt_pa6", pa6,
                            # container = sketch,
                            format_round = formatr_pa6,
                            # format_style = formats_pa6,
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
