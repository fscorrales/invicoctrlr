#' mod_02_05_slave UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_05_slave_ui <- function(id){
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
        # tabPanel("mensual",
        #          htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
        #                          "y Gastos SIIF (rcg01_uejp)</strong>")
        #          ),
        tabPanel("honorarios",
                 htmltools::HTML("<strong>Fuente: Slave, Gastos SIIF (rcg01_uejp), ",
                                 "y Gastos por Grupo SIIF (gto_rpa03g)</strong>")
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
      # shiny::tabPanel(
      #   title = "Carga Mensual",
      #   value = "mensual",
      #   mod_data_table_ui(ns("dt_metodo_2"))
      # ),
      shiny::tabPanel(
        title = "Honorarios Factureros",
        value = "honorarios",
        mod_data_table_ui(ns("dt_honorarios"))
      ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("filter"),
        tabsetPanel(
          id = ns("switcher"), type = "hidden",
          # tabPanel("mensual",
          #          mod_02_03_02_metodo_2_ui(ns("filter_mensual"))
          #           ),
          tabPanel("honorarios",
                   mod_02_05_01_honorarios_ui(ns("filter_honorarios"))
                    )
        )
      )
    )

  )
}

#' 02_05_slave Server Functions
#'
#' @noRd
mod_02_05_slave_server <- function(id){
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


    # Table Control Honorarios SLAVE
    honorarios <- mod_02_05_01_honorarios_server("filter_honorarios")

    shiny::observeEvent(honorarios(), {

      sketch_honorarios = htmltools::withTags(table(
        class = 'display',
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'nro_entrada'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'fecha'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'partida'),
            htmltools::tags$th(class = 'dt-center', colspan = 3, 'monto')
          ),
          htmltools::tags$tr(
            lapply(rep(c('siif', 'slave', "ctrl"), 4), htmltools::tags$th)
          )
        )
      ))

      # formatr_registro <- list(columns = c("siif", "icaro",
      #                                   "diferencia"))

      formats_honorarios <- list(columns = c("dif_nro_entrada", "dif_fecha",
                                           "dif_partida", "dif_monto"),
                               color = DT::styleEqual(
                                 c("\u2713", "\u2718"), c('green', 'red')),
                               fontWeight = "bold",
                               fontSize = "24px")

      headjs_honorarios <- "function(thead) {
      $(thead).closest('thead').find('th').eq(0).css('background-color', '#D9E1F2');
      $(thead).closest('thead').find('th').eq(1).css('background-color', '#8EA9DB');
      $(thead).closest('thead').find('th').eq(2).css('background-color', '#D9E1F2');
      $(thead).closest('thead').find('th').eq(3).css('background-color', '#8EA9DB');
      }"

      mod_data_table_server("dt_honorarios", honorarios,
                            container = sketch_honorarios,
                            # format_round = formatr_honorarios,
                            format_style = formats_honorarios,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            ),
                            headerCallback = DT::JS(headjs_honorarios)
      )

    })

  })
}

## To be copied in the UI
# mod_02_05_slave_ui("mod_02_05_slave_1")

## To be copied in the server
# mod_02_05_slave_server("mod_02_05_slave_1")
