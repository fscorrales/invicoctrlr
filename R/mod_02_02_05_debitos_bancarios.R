#' 02_02_05_debitos_bancarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_05_debitos_bancarios_ui <- function(id){

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
        ),

        shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                              choices = "", selected = "", multiple = TRUE,
                              options = list(placeholder = "Todo seleccionado"))
      ),
      shiny::column(
        6,
        ## filtro por cuenta??
        shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                  choices = c("ejercicio", "mes", "fecha", "cta_cte"),
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

#' 02_04_04_registro Server Functions
#'
#' @noRd
mod_02_02_05_debitos_bancarios_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT ejercicio, mes, fecha, nro_entrada, partida, monto ",
      "FROM comprobantes_gtos_gpo_partida_gto_rpa03g) G LEFT JOIN " ,
      "(SELECT ejercicio, nro_entrada, cta_cte ",
      "FROM comprobantes_gtos_rcg01_uejp) C ",
      "ON (G.ejercicio = C.ejercicio ",
      "AND G.nro_entrada = C.nro_entrada)"
    )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(siif_comprobantes_gtos_rcg01_uejp(),
           siif_comprobantes_gtos_gpo_partida_gto_rpa03g(),
           sscc_banco_invico())
    })

    observeEvent(to_listen(), {

      r6_siif <- MyData$new(sql_path("siif"))
      r6_sscc <- MyData$new(sql_path("sscc"))

      r6_siif$
        get_query(paste0("SELECT DISTINCT G.ejercicio FROM ", sql_join))

      r6_sscc$
        get_query(paste0("SELECT DISTINCT ejercicio FROM banco_invico"))

      choices_rv$ejercicio <- sort(c(r6_sscc$data$ejercicio,
                                     r6_siif$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_siif$get_query(
        paste0("SELECT MAX(G.fecha) as max_fecha, MIN(G.fecha) as min_fecha ",
               "FROM ", sql_join)
      )

      r6_sscc$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM banco_invico")
      )

      r6_siif$bind_rows(r6_sscc$data)

      choices_rv$fecha <- c(
        r6_siif$data$max_fecha, r6_siif$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")

      r6_siif$data <- map_cta_cte("siif",
                                   paste0("SELECT DISTINCT C.cta_cte FROM ", sql_join),
                                   "siif_gastos_cta_cte")

      r6_sscc$data <-  map_cta_cte("sscc",
                                  paste0("SELECT DISTINCT cta_cte FROM banco_invico"),
                                  "sscc_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_siif$data, r6_sscc$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)


      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_siif <- MyData$new(sql_path("siif"))
      r6_sscc <- MyData$new(sql_path("sscc"))

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

      cta_cte_vec <- input$cta_cte %||%
        unique(choices_rv$cta_cte)

      #Filtering siif_rec
      r6_siif$
        get_query(
          paste0("SELECT G.ejercicio, G.mes, G.fecha, ",
                 "G.monto, C.cta_cte ",
                 "FROM ", sql_join, " ",
                 "WHERE G.partida = '355' ",
                 "AND G.ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_gastos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_siif$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising siif
      r6_siif$
        select(input$grupo %||% "mes", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(ejecutado_siif = sum(.data$monto, na.rm = TRUE))

      #Filtering sscc_banco_invico
      r6_sscc$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, cta_cte, ",
                 "monto FROM banco_invico ",
                 "WHERE codigo_imputacion = 31 ",
                 "AND ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sscc_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          monto = .data$monto * (-1)
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_sscc$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising sscc
      r6_sscc$
        select(input$grupo %||% "mes", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(debitos_sscc = sum(.data$monto, na.rm = TRUE))

      #Joinning and calulating
      r6_siif$
        full_join(r6_sscc$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          diferencia = .data$ejecutado_siif - .data$debitos_sscc,
          dif_acum = cumsum(.data$diferencia)
        )

      total_desvio <- sum(abs(r6_siif$data$diferencia))
      r6_siif$mutate(prop_desv = (abs(.data$diferencia) / total_desvio))

      return(r6_siif$data)

    })

    # return(table)

  })
}

## To be copied in the UI
# mod_02_02_05_debitos_bancarios_ui("02_02_05_debitos_bancarios_1")

## To be copied in the server
# mod_02_02_05_debitos_bancarios_server("02_02_05_debitos_bancarios_1")
