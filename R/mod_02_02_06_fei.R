#' 02_02_06_fei UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_06_fei_ui <- function(id){

  ns <- NS(id)

  tagList(
    shiny::column(12, align = "center",
                  bs4Dash::actionButton(ns("update"),
                                        "Actualizar Filtros",
                                        status = "primary")),
    rep_br(),

    shiny::fluidRow(
      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio",
                                 choices = "", selected = "", multiple = TRUE,
                                 options = list(placeholder = "\u00daltimo seleccionado")),
        suppressWarnings(
          shiny::dateRangeInput(ns("fecha"), "Seleccionar Fecha", start = NA,
                                end = NA, format = "dd-mm-yyyy",
                                startview = "month", language = "es", separator = " a ")
        )

        # shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
        #                       choices = "", selected = "", multiple = TRUE,
        #                       options = list(placeholder = "Todo seleccionado"))
      ),
      shiny::column(
        6,
        ## filtro por cuenta??
        shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                  choices = c("ejercicio", "mes", "fecha", "cuit"),
                                  selected = "mes",
                                  inline = FALSE),
        #
        # rep_br(2),
#
#         checkboxInput(ns("mostrar"),
#                       "Mostrar solo PA6 pendiente de regularizaci\u00f3n", value = FALSE)

      )
      )
  )
}

#' 02_02_06_fei Server Functions
#'
#' @noRd
mod_02_02_06_fei_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT * ",
      "FROM resumen_rend_prov) R LEFT JOIN " ,
      "(SELECT cuit, descripcion ",
      "FROM listado_prov) P ",
      "ON R.beneficiario = P.descripcion"
    )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(siif_mayor_contable_rcocc31(),
           sgf_resumen_rend_prov())
    })

    observeEvent(to_listen(), {

      r6_siif <- MyData$new(sql_path("siif"))
      r6_sgf <- MyData$new(sql_path("sgf"))

      r6_siif$
        get_query(paste0("SELECT DISTINCT ejercicio ",
                         "FROM mayor_contable_rcocc31 ",
                         "WHERE cta_contable = '2113-2-9'"))

      r6_sgf$
        get_query(paste0("SELECT DISTINCT ejercicio FROM resumen_rend_prov"))

      choices_rv$ejercicio <- sort(c(r6_sgf$data$ejercicio,
                                     r6_siif$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_siif$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM mayor_contable_rcocc31 ",
               "WHERE cta_contable = '2113-2-9'")
      )

      r6_sgf$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM resumen_rend_prov")
      )

      r6_siif$bind_rows(r6_sgf$data)

      choices_rv$fecha <- c(
        r6_siif$data$max_fecha, r6_siif$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_siif <- MyData$new(sql_path("siif"))
      r6_sgf <- MyData$new(sql_path("sgf"))

      #Setting input default value
      if (is.null(input$ejercicio)) {
        shiny::updateSelectizeInput(session, "ejercicio",
                                    selected = max(as.integer(choices_rv$ejercicio)))
      }

      if (is.null(input$grupo)) {
        shiny::updateCheckboxGroupInput(session, "grupo",
                                        selected = "mes")
      }

      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))

      # cta_cte_vec <- input$cta_cte %||%
      #   unique(choices_rv$cta_cte)

      #Filtering siif_rec
      r6_siif$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, ",
                 "debitos, creditos, auxiliar_1 AS cuit ",
                 "FROM mayor_contable_rcocc31 ",
                 "WHERE cta_contable = '2113-2-9' ",
                 "AND tipo_comprobante <> 'APE' ",
                 "AND ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_siif$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising siif
      r6_siif$
        select(input$grupo %||% "mes", .data$debitos, .data$creditos)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(carga_fei = sum(.data$creditos, na.rm = TRUE),
                  pago_fei = sum(.data$debitos, na.rm = TRUE),
                  dif_fei = .data$carga_fei - .data$pago_fei)

      #Filtering sgf_invico
      r6_sgf$
        get_query(
          paste0("SELECT R.destino, R.ejercicio, R.mes, R.fecha, ",
                 "R.importe_bruto, P.cuit, P.descripcion FROM ", sql_join ," ",
                 "WHERE origen = 'FUNCIONAMIENTO' ",
                 "AND cta_cte = '130832-08' ",
                 "AND ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )$
        filter(stringr::str_detect(.data$destino, "HONORARIOS ESCRIBANOS"))

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_sgf$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising sscc
      r6_sgf$
        select(input$grupo %||% "mes", .data$importe_bruto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(bruto_sgf = sum(.data$importe_bruto, na.rm = TRUE))

      #Joinning and calulating
      r6_siif$
        full_join(r6_sgf$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          dif_pago = .data$pago_fei - .data$bruto_sgf,
          dif_acum = cumsum(.data$dif_pago)
        )

      total_desvio <- sum(abs(r6_siif$data$dif_pago))
      r6_siif$mutate(prop_desv = (abs(.data$dif_pago) / total_desvio))

      return(r6_siif$data)

    })

    # return(table)

  })
}

## To be copied in the UI
# mod_02_02_06_fei_ui("02_02_06_fei_1")

## To be copied in the server
# mod_02_02_06_fei_server("02_02_06_fei_1")
