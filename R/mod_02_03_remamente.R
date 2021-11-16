#' 02_03_remamente UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_03_remamente_ui <- function(id){
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
        tabPanel("metodo_1",
                 htmltools::HTML("<strong>Fuente:Resumen Gral. de Mov. SSCC ",
                                 "y Deuda Flotante SIIF (rdeu012)</strong>")
                 ),
        tabPanel("metodo_2",
                 htmltools::HTML("<strong>Fuente: Recursos SIIF (rci02) ",
                                 "y Gastos SIIF (rcg01_uejp)</strong>")
                 ),
        tabPanel("diferencia",
                 htmltools::HTML("<strong>Fuente: Resumen Gral. de Mov. SSCC, ",
                                 "Deuda Flotante SIIF (rdeu012), Recursos SIIF (rci02) ",
                                 "y Gastos SIIF (rcg01_uejp)</strong>")
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
        title = "M\u00e9todo I: Saldo Banco - Deuda Flotante",
        value = "metodo_1",
        mod_data_table_ui(ns("dt_metodo_1"))
      ),
      shiny::tabPanel(
        title = "M\u00e9todo II: Recursos - Gastos SIIF",
        value = "metodo_2",
        mod_data_table_ui(ns("dt_metodo_2"))
      ),
      shiny::tabPanel(
        title = "Diferencia: M\u00e9todo I - M\u00e9todo II",
        value = "diferencia",
        mod_data_table_ui(ns("dt_diferencia"))
      ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("filter"),
        tabsetPanel(
          id = ns("switcher"), type = "hidden",
          tabPanel("metodo_1",
                   mod_02_03_01_metodo_1_ui(ns("filter_metodo_1"))
                   ),
          tabPanel("metodo_2",
                   mod_02_03_02_metodo_2_ui(ns("filter_metodo_2"))
                    ),
          tabPanel("diferencia",
                   mod_02_03_03_diferencia_ui(ns("filter_diferencia"))
                    )
        )
      )
    )

  )
}

#' 01_a_siif_recursos Server Functions
#'
#' @noRd
mod_02_03_remamente_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      ## DANGER, THIS IS NOT WORKING. TRY ANOTHER WAY
      # Ans <- switch(input$controller,
      #               metodo_1 = list(data = metodo_1()),
      #               metodo_2 = list(data = metodo_2()),
      #               diferencia = list(data = diferencia()),
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


    #Table Remamente Metodo 1
    metodo_1 <- mod_02_03_01_metodo_1_server("filter_metodo_1")

    shiny::observeEvent(metodo_1(), {

      formatr_metodo_1 <- list(columns = c("saldo_banco", "deuda_flotante",
                                           "remanente"))

      mod_data_table_server("dt_metodo_1", metodo_1,
                            format_round = formatr_metodo_1,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })

    #Table Remanente Metodo 2
    metodo_2 <- mod_02_03_02_metodo_2_server("filter_metodo_2")

    shiny::observeEvent(metodo_2(), {

      formatr_metodo_2 <- list(columns = c("recursos", "gastos",
                                           "remanente"))

      mod_data_table_server("dt_metodo_2", metodo_2,
                            format_round = formatr_metodo_2,
                            buttons = list(
                              list(
                                extend = 'collection',
                                buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                                text = 'Download 100 primeras filas')
                            )
      )

    })


    #Table Diferencia Metodo 1 vs Metodo 2
    diferencia <- mod_02_03_03_diferencia_server("filter_diferencia")

    shiny::observeEvent(diferencia(), {

      formatr_diferencia <- list(columns = c("remanente_1", "remanente_2",
                                             "diferencia"))

      mod_data_table_server("dt_diferencia", diferencia,
                            format_round = formatr_diferencia,
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
# mod_02_03_remamente_ui("02_03_remamente_1")

## To be copied in the server
# mod_02_03_remamente_server("02_03_remamente_1")
