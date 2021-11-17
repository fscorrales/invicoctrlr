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

    registro <- mod_02_04_03_registro_server("filter_registro")

    shiny::observeEvent(registro(), {

      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', colspan = 3, 'fuente'),
            th(class = 'dt-center', colspan = 3, 'monto'),
            th(class = 'dt-center', colspan = 3, 'cta_cte'),
            th(class = 'dt-center', colspan = 3, 'cuit'),
            th(class = 'dt-center', colspan = 3, 'nro_entrada'),
            th(class = 'dt-center', colspan = 3, 'fecha'),
            th(class = 'dt-center', colspan = 3, 'partida'),
          ),
          tr(
            lapply(rep(c('siif', 'icaro', "dif"), 7), th)
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

      mod_data_table_server("dt_registro", registro,
                            container = sketch,
                            # format_round = formatr_registro,
                            format_style = formats_registro,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
                            # columnDefs = list(list(className = 'dt-center', targets = "_all"))
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
