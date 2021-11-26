#' mod_02_04_icaro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_04_icaro_ui <- function(id){
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
        tabPanel("anual",
                 htmltools::HTML("<strong>Fuente:R Icaro ",
                                 "y Ejecucion presupuesto con Fuente SIIF (rf602)</strong>")
                 ),
        # tabPanel("mensual",
        #          htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
        #                          "y Gastos SIIF (rcg01_uejp)</strong>")
        #          ),
        tabPanel("registro",
                 htmltools::HTML("<strong>Fuente: R Icaro, Gastos SIIF (rcg01_uejp), ",
                                 "y Gastos por Grupo SIIF (gto_rpa03g)</strong>")
                 ),
        tabPanel("pa6",
                 htmltools::HTML("<strong>Fuente: R Icaro, Gastos SIIF (rcg01_uejp), ",
                                 "y Comprobantes Fondos SIIF (rfondo07tp - PA6)</strong>")
        )
        ),
      boxToolSize = "lg",
      # dropdownMenu =  bs4Dash::boxDropdown(
      #   icon = shiny::icon("save"),
      #   bs4Dash::boxDropdownItem(mod_save_button_ui(ns("download_xls"), "Exportar xls",
      #                                               icon = shiny::icon("file-excel"),
      #                                               filetype=list(xlsx="xlsx"))),
      #   bs4Dash::boxDropdownItem(mod_save_button_ui(ns("download_csv"), "Exportar csv",
      #                                               icon = shiny::icon("file-csv"),
      #                                               filetype=list(csv="csv")))
      # ),
      shiny::tabPanel(
        title = "Carga Anual",
        value = "anual",
        mod_data_table_ui(ns("dt_anual"))
      ),
      # shiny::tabPanel(
      #   title = "Carga Mensual",
      #   value = "mensual",
      #   mod_data_table_ui(ns("dt_metodo_2"))
      # ),
      shiny::tabPanel(
        title = "Carga por Registro",
        value = "registro",
        mod_data_table_ui(ns("dt_registro"))
      ),
      shiny::tabPanel(
        title = "Carga PA6",
        value = "pa6",
        mod_data_table_ui(ns("dt_pa6"))
      ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("filter"),
        tabsetPanel(
          id = ns("switcher"), type = "hidden",
          tabPanel("anual",
                   mod_02_04_01_anual_ui(ns("filter_anual"))
                   ),
          # tabPanel("mensual",
          #          mod_02_03_02_metodo_2_ui(ns("filter_mensual"))
          #           ),
          tabPanel("registro",
                   mod_02_04_03_registro_ui(ns("filter_registro"))
                    ),
          tabPanel("pa6",
                   mod_02_04_04_pa6_ui(ns("filter_pa6"))
          )
        )
      )
    )

  )
}

#' 01_a_siif_recursos Server Functions
#'
#' @noRd
mod_02_04_icaro_server <- function(id){
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


    # Table Control Anual ICARO
    anual <- mod_02_04_01_anual_server("filter_anual")

    shiny::observeEvent(anual(), {

      formatr_anual <- list(columns = c("siif", "icaro",
                                        "diferencia"))

      mod_data_table_server("dt_anual", anual,
                            format_round = formatr_anual,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })

    # Table Control Registro Carga ICARO
    registro <- mod_02_04_03_registro_server("filter_registro")

    shiny::observeEvent(registro(), {

      sketch_registro = htmltools::withTags(table(
        class = 'display',
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'fuente'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'monto'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'cta_cte'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'cuit'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'nro_entrada'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'fecha'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'partida'),
          ),
          htmltools::tags$tr(
            lapply(rep(c('siif', 'icaro', "ctrl"), 7), htmltools::tags$th)
          )
        )
      ))

      # formatr_registro <- list(columns = c("siif", "icaro",
      #                                   "diferencia"))

      formats_registro <- list(columns = c("dif_fuente", "dif_monto", "dif_cta_cte",
                                           "dif_cuit", "dif_nro_entrada",
                                           "dif_fecha", "dif_partida"),
                               color = DT::styleEqual(
                                 c("\u2713", "\u2718"), c('green', 'red')),
                               fontWeight = "bold",
                               fontSize = "24px")

      headjs_registro <- "function(thead) {
      $(thead).closest('thead').find('th').eq(0).css('background-color', '#D9E1F2');
      $(thead).closest('thead').find('th').eq(1).css('background-color', '#8EA9DB');
      $(thead).closest('thead').find('th').eq(2).css('background-color', '#D9E1F2');
      $(thead).closest('thead').find('th').eq(3).css('background-color', '#8EA9DB');
      $(thead).closest('thead').find('th').eq(4).css('background-color', '#D9E1F2');
      $(thead).closest('thead').find('th').eq(5).css('background-color', '#8EA9DB');
      $(thead).closest('thead').find('th').eq(6).css('background-color', '#D9E1F2');
      }"

      mod_data_table_server("dt_registro", registro,
                            container = sketch_registro,
                            # format_round = formatr_registro,
                            format_style = formats_registro,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            ),
                            headerCallback = DT::JS(headjs_registro)
      )

    })

    # Table Control PA6 ICARO
    pa6 <- mod_02_04_04_pa6_server("filter_pa6")

    shiny::observeEvent(pa6(), {

      sketch_pa6 = htmltools::withTags(table(
        class = 'display',
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(class = 'dt-center', colspan = 6, 'siif'),
            htmltools::tags$th(class = 'dt-center', rowspan = 2, 'ctrl'),
            htmltools::tags$th(class = 'dt-center', colspan = 6, 'icaro')
          ),
          htmltools::tags$tr(
            htmltools::tags$th(colspan = 1, 'nro_fondo'),
            htmltools::tags$th(colspan = 1, 'nro_reg'),
            htmltools::tags$th(colspan = 1, 'fecha_pa6'),
            htmltools::tags$th(colspan = 1, 'fecha_reg'),
            htmltools::tags$th(colspan = 1, 'monto_pa6'),
            htmltools::tags$th(colspan = 1, 'monto_reg'),
            htmltools::tags$th(colspan = 1, 'nro_pa6'),
            htmltools::tags$th(colspan = 1, 'tipo'),
            htmltools::tags$th(colspan = 1, 'monto_pa6'),
            htmltools::tags$th(colspan = 1, 'nro_reg'),
            htmltools::tags$th(colspan = 1, 'tipo'),
            htmltools::tags$th(colspan = 1, 'monto_reg')
          )
        )
      ))

      headjs_pa6 <- "function(thead) {
      $(thead).closest('thead').find('th').eq(0).css('background-color', '#D9E1F2');
      $(thead).closest('thead').find('th').eq(1).css('background-color', '#8EA9DB');
      $(thead).closest('thead').find('th').eq(2).css('background-color', '#D9E1F2');
      }"

      formatr_pa6 <- list(columns = c("monto_pa6", "saldo_pa6",
                                      "monto_pa6_icaro", "monto_reg_icaro"))

      formats_pa6<- list(columns = c("ctrl"),
                               color = DT::styleEqual(
                                 c("\u2713", "\u2718"), c('green', 'red')),
                               fontWeight = "bold",
                               fontSize = "24px")

      mod_data_table_server("dt_pa6", pa6,
                            container = sketch_pa6,
                            format_round = formatr_pa6,
                            format_style = formats_pa6,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            ),
                            columnDefs = list(list(className = 'dt-center', targets = 6)),
                            headerCallback = DT::JS(headjs_pa6)
      )

    })


    #
    #
    # #Table Remanente Metodo 2
    # mensual <- mod_02_03_02_metodo_2_server("filter_mensual")
    #
    # formatr_mensual <- list(columns = c("recursos", "gastos",
    #                                      "remanente"))
    #
    # mod_data_table_server("dt_mensual", mensual,
    #                       format_round = formatr_mensual,
    #                       buttons = list(
    #                         list(
    #                           extend = 'collection',
    #                           buttons = c('copy', 'print','csv', 'excel', 'pdf'),
    #                           text = 'Download 100 primeras filas')
    #                       )
    # )
    #
    # #Table Recursos 337 vs Codigo Retencion 337
    # registro <- mod_02_03_03_diferencia_server("filter_registro")
    #
    # formatr_registro <- list(columns = c("remanente_1", "remanente_2",
    #                                        "diferencia"))
    #
    # mod_data_table_server("dt_registro", registro,
    #                       format_round = formatr_registro,
    #                       buttons = list(
    #                         list(
    #                           extend = 'collection',
    #                           buttons = c('copy', 'print','csv', 'excel', 'pdf'),
    #                           text = 'Download 100 primeras filas')
    #                       )
    # )

  })
}

## To be copied in the UI
# mod_02_04_icaro_ui("mod_02_04_icaro_1")

## To be copied in the server
# mod_02_04_icaro_server("mod_02_04_icaro_1")
