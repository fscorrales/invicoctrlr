#' 02_01_03_rec_vs_invico UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_01_03_rec_vs_invico_ui <- function(id){

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
                                 options = list(placeholder = "Todo seleccionado")),
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
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio",
                                                 "fecha", "mes","cta_cte"),
                                     selected = "mes" , inline = FALSE)
      )

      ),

    shiny::fluidRow(
      shiny::column(
        6, shiny::radioButtons(ns("dep_aju_siif"),
                               "\u00bfDepurar Asientos AJU de la Contabilidad?",
                               choices = c("SI", "NO"), selected = "SI")
             ),
      shiny::column(
        6
        )
      )
  )
}

#' 02_01_03_rec_vs_invico Server Functions
#'
#' @noRd
mod_02_01_03_rec_vs_invico_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT * FROM mayor_contable_rcocc31 " ,
      "WHERE cta_contable = '2122-1-2' ",
      "AND auxiliar_1 = '337' ",
      "AND tipo_comprobante <> 'APE') R LEFT JOIN ",
      "(SELECT * FROM mayor_contable_rcocc31 ",
      "WHERE cta_contable = '1112-2-6' ",
      "AND tipo_comprobante <> 'APE') B ",
      "ON (R.ejercicio = B.ejercicio ",
      "AND R.nro_entrada = B.nro_entrada)"
    )

    # #Initial DBs setting
    # db_rec <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db <- siif_comprobantes_rec_rci02() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$siif_recursos_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE)
    #                   )
    #   return(db)
    # })
    #
    # db_cont <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db_banco <- siif_mayor_contable_rcocc31() %>%
    #     dplyr::filter(.data$cta_contable == "1112-2-6",
    #                   .data$tipo_comprobante != "APE") %>%
    #     dplyr::mutate(cta_cte = map_values(.data$auxiliar_1,
    #                                             from = db_cta_cte$siif_contabilidad_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE),
    #                   mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
    #                                        lubridate::year(.data$fecha), sep = "/")) %>%
    #     dplyr::select(-.data$auxiliar_1, -.data$auxiliar_2, -.data$cta_contable)
    #
    #   db <- siif_mayor_contable_rcocc31() %>%
    #     dplyr::filter(.data$cta_contable == "2122-1-2",
    #                   .data$auxiliar_1 == "337",
    #                   .data$tipo_comprobante != "APE") %>%
    #     dplyr::mutate(mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
    #                                        lubridate::year(.data$fecha), sep = "/")) %>%
    #     dplyr::select(-.data$auxiliar_2, -.data$cta_contable) %>%
    #     dplyr::rename(cod_ret = .data$auxiliar_1) %>%
    #     dplyr::left_join(dplyr::select(db_banco, .data$cta_cte,
    #                                    .data$ejercicio, .data$nro_entrada),
    #                      by = c("nro_entrada", "ejercicio"))
    #
    #   return(db)
    #
    # })

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(siif_comprobantes_rec_rci02(),
           siif_mayor_contable_rcocc31())
    })

    observeEvent(to_listen(), {

      r6_siif_rec<- MyData$new(sql_path("siif"))
      r6_siif_cont <- MyData$new(sql_path("siif"))

      r6_siif_rec$data <- map_cta_cte("siif",
                                      "SELECT DISTINCT cta_cte FROM comprobantes_rec_rci02",
                                      "siif_recursos_cta_cte")

      r6_siif_cont$data <-  map_cta_cte("siif",
                                        paste0("SELECT DISTINCT B.auxiliar_1 AS cta_cte ",
                                               "FROM ", sql_join),
                                        "siif_contabilidad_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_siif_rec$data, r6_siif_cont$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      r6_siif_rec$get_query("SELECT DISTINCT ejercicio FROM comprobantes_rec_rci02")

      r6_siif_cont$get_query(paste0(
        "SELECT DISTINCT ejercicio FROM mayor_contable_rcocc31 ",
        "WHERE cta_contable = '2122-1-2' ",
        "AND auxiliar_1 = '337' ",
        "AND tipo_comprobante <> 'APE'"
      ))

      choices_rv$ejercicio <- sort(unique(c(r6_siif_rec$data$ejercicio,
                                            r6_siif_cont$data$ejercicio)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_siif_rec$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM comprobantes_rec_rci02")
      )

      r6_siif_cont$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM mayor_contable_rcocc31 ",
               "WHERE cta_contable = '2122-1-2' ",
               "AND auxiliar_1 = '337' ",
               "AND tipo_comprobante <> 'APE'"
        )
      )

      r6_siif_rec$bind_rows(r6_siif_cont$data)

      choices_rv$fecha <- c(
        r6_siif_rec$data$max_fecha, r6_siif_rec$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")


      shiny::updateDateRangeInput(session, "fecha",
                                  min = min(choices_rv$fecha),
                                  max = max(choices_rv$fecha))

      r6_siif_rec$finalize()
      r6_siif_cont$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_siif_rec <- MyData$new(sql_path("siif"))
      r6_siif_cont <- MyData$new(sql_path("siif"))

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
      r6_siif_rec$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, cta_cte, ",
                 "monto FROM comprobantes_rec_rci02 ",
                 "WHERE invico = 1 ",
                 "AND ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_recursos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_siif_rec$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising siif
      r6_siif_rec$
        select(input$grupo %||% "mes", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(recursos_siif = sum(.data$monto, na.rm = TRUE))

      #Filtering siif_337
      r6_siif_cont$
        get_query(
          paste0("SELECT R.ejercicio, R.fecha, B.auxiliar_1 AS cta_cte, ",
                 "R.tipo_comprobante, R.creditos, R.debitos FROM ",
                 sql_join, " WHERE R.ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_contabilidad_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01",),
          mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                               lubridate::year(.data$fecha), sep = "/")
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_siif_cont$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      if (input$dep_aju_siif == "SI") {
        r6_siif_cont$filter(
          .data$tipo_comprobante != "AJU"
        )
      }

      #Grouping and summarising 337
      r6_siif_cont$
        select(input$grupo %||% "mes", .data$creditos, .data$debitos)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(gastos_337_siif = sum(.data$creditos, na.rm = TRUE),
                  pagos_337_siif = sum(.data$debitos, na.rm = TRUE))$
        mutate(dif_pagado_337 = .data$gastos_337_siif - .data$pagos_337_siif)

      #Joinning and calulating
      r6_siif_rec$
        full_join(r6_siif_cont$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          dif_ingresado = .data$recursos_siif - .data$pagos_337_siif,
          dif_acum = cumsum(.data$dif_ingresado)
        )

      total_desvio <- sum(abs(r6_siif_rec$data$dif_ingresado))
      r6_siif_rec$mutate(prop_desv = (abs(.data$dif_ingresado) / total_desvio))

      return(r6_siif_rec$data)

      r6_siif_rec$finalize()
      r6_siif_cont$finalize()

    })

    return(table)


  })
}

## To be copied in the UI
# mod_02_01_03_rec_vs_invico_ui("02_01_03_rec_vs_invico_ui_1")

## To be copied in the server
# mod_02_01_03_rec_vs_invico_server("02_01_03_rec_vs_invico_ui_1")
