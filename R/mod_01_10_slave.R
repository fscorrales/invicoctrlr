#' 01_10_slave UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_01_10_slave_ui <- function(id){
  ns <- NS(id)

  steps_slave <- list(
    aviso = paste0(
      "Momentamente, hasta que SLAVE evolucione a la siguiente versi\u00f3n, ",
      "deber\u00e1 <strong>convertir la base de datos actual</strong>"
    ),
    importar = paste0(
      "Lo \u00fanico que deber\u00e1 hacer es <strong>ubicar e importar</strong> la ",
      "base de datos de Honorarios SLAVE en excel."
    ),
    convertir = paste0(
      "El proceso de <strong>conversi\u00f3n es autom\u00e1tico</strong>"
    )
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
        title = "Honorarios Prestadores",
        value = "honorarios",
        mod_data_table_ui(ns("honorarios"))
      ),
      # shiny::tabPanel(
      #   title = "Carga",
      #   value = "carga",
      #   mod_data_table_ui(ns("carga"))
      # ),
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        startOpen = FALSE,
        icon = shiny::icon("sync-alt"),
        htmltools::h4("Actualizar Base de Datos", style="text-align: center;"),
        rep_br(),
        mod_file_input_ui(ns("update"), multiple = TRUE),
        htmltools::h5("Pasos a seguir para importar:"),
        htmltools::tags$ol(
          list_to_li(steps_slave)
        )
      )
    )
  )
}

#' 01_10_slave Server Functions
#'
#' @noRd
mod_01_10_slave_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::reset("update-file")
    shinyFeedback::hideFeedback("update-file")

    rpw_controller <- rv()

    observeEvent(input$controller, {

      Ans <- switch(input$controller,
                    honorarios = list(data = slave_honorarios(),
                                    import_function = invicodatr::rpw_slave_honorarios,
                                    df_trigger = slave_trigger),
                    # carga = list(data = icaro_carga(),
                    #                  import_function = invicodatr::transmute_icaro_old_to_new,
                    #                  df_trigger = icaro_trigger),
                    stop("Invalid `x` value")
                    )

      rpw_controller$df <- Ans$data
      rpw_controller$fct <- Ans$import_function
      rpw_controller$trigger <- Ans$df_trigger

      # updateTabsetPanel(inputId = "switcher", selected = input$controller)

      shinyjs::reset("update-file")
      shinyFeedback::hideFeedback("update-file")

    })

    mod_save_button_server("download_xls", reactive(rpw_controller$df))

    mod_save_button_server("download_csv", reactive(rpw_controller$df))

    mod_file_input_server("update",
                          import_function = reactive(rpw_controller$fct),
                          df_trigger = reactive(rpw_controller$trigger))

    hide_columns_honorarios <- c(10:16)

    formatr_honorarios <- list(columns = c("importe_bruto", "importe_neto",
                                           "sellos", "seguro","iibb", "lp",
                                           "otras_ret", "anticipo", "descuento"))

    mod_data_table_server("honorarios", slave_honorarios,
                          format_round = formatr_honorarios,
                          columnDefs = list(
                            list(visible=FALSE, targets = hide_columns_honorarios)
                            ),
                          buttons = list(
                            list(
                              extend = 'collection',
                              buttons = c('copy', 'print','csv', 'excel', 'pdf'),
                              text = 'Download 100 primeras filas'),
                            list(
                              extend='colvis',
                              text="Mostrar / Ocultar columnas",
                              columns = hide_columns_honorarios)
                            )
                          )

    # hide_columns_carga <- c(11:14)
    #
    # formatr_carga <- list(columns = c("importe", "fondo_reparo"))
    # formatp_carga <- list(columns = "avance")
    #
    # mod_data_table_server("carga", icaro_carga,
    #                       format_round = formatr_carga,
    #                       format_perc = formatp_carga,
    #                       columnDefs = list(
    #                         list(visible=FALSE, targets = hide_columns_carga)
    #                       ),
    #                       buttons = list(
    #                         list(
    #                           extend = 'collection',
    #                           buttons = c('copy', 'print','csv', 'excel', 'pdf'),
    #                           text = 'Download 100 primeras filas'),
    #                         list(
    #                           extend='colvis',
    #                           text="Mostrar / Ocultar columnas",
    #                           columns = hide_columns_carga)
    #                       )
    # )

  })
}

## To be copied in the UI
# mod_01_10_slave_ui("mod_01_10_slave")

## To be copied in the server
# mod_01_10_slave_server("mod_01_10_slave")
