#' 02_02_02_sueldo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_02_sueldo_ui <- function(id){

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

        shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "mes", "fecha"),
                                     selected = "mes",
                                     inline = FALSE)
      )
      ),

      shiny::fluidRow(
        shiny::column(
          6,
          checkboxInput(ns("rdeu"),
                        "\u00bfAjustar Deuda Flotante?", value = TRUE),

          rep_br(),

          checkboxInput(ns("transf_internas"),
                        "\u00bfDepurar transf. internas SSCC?", value = TRUE)
        ),
        shiny::column(
          6,
          checkboxInput(ns("no_pagado_04"),
                      "\u00bfDepurar conceptos no pagados con 130832-04?", value = TRUE),

          checkboxInput(ns("otros_sscc"),
                      "\u00bfDepurar Cheques Remplazados y Reingresos Vs?", value = TRUE)
        )
      )
  )
}

#' 02_04_02_sueldo Server Functions
#'
#' @noRd
mod_02_02_02_sueldo_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT ejercicio, mes, fecha, nro_entrada, partida, monto ",
      "FROM comprobantes_gtos_gpo_partida_gto_rpa03g) G LEFT JOIN " ,
      "(SELECT ejercicio, nro_entrada, cta_cte, cuit ",
      "FROM comprobantes_gtos_rcg01_uejp) C ",
      "ON (G.ejercicio = C.ejercicio ",
      "AND G.nro_entrada = C.nro_entrada)"
    )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      #Movimientos Grales SSCC por certificados negativos?
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

      choices_rv$ejercicio <- sort(c((r6_siif$data$ejercicio),
                                     (r6_sscc$data$ejercicio)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio)

      r6_siif$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
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

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_siif <- MyData$new(sql_path("siif"))
      r6_siif_aux <- MyData$new(sql_path("siif"))
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

      #Filtering siif_rec
      r6_siif$
        get_query(
          paste0("SELECT G.ejercicio, G.mes, G.fecha, ",
                 "G.partida, G.nro_entrada, G.monto, C.cuit ",
                 "FROM ", sql_join, " ",
                 "WHERE C.cta_cte = '130832004'")
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )

      #Depuramos conceptos no abonados con 130832-04
      if (input$no_pagado_04) {

        #Procedemos a Depurar Gastos no pagados con 130832/04
        # - 310 Haberes Erroneos
        # - 245 Gcias
        # - 384 ART

        #Ajustamos ART
        r6_siif$filter(.data$partida %not_in% c("150", "151"))

        #Ajustamos Gcias (245) y Haberes Erroneos (310)
        r6_siif_aux$
          get_query(
            paste0("SELECT ejercicio, mes, fecha, creditos ",
                   "FROM mayor_contable_rcocc31 ",
                   "WHERE cta_contable = '2122-1-2' ",
                   "AND tipo_comprobante <> 'APE' ",
                   "AND auxiliar_1 in ('245', '310')")
          )$
          mutate(monto = .data$creditos * (-1),
                 cuit = "30632351514",
                 fecha = as.Date(.data$fecha, origin = "1970-01-01")
                 )

        r6_siif$bind_rows(r6_siif_aux$data)
      }

      #Depuramos RDEU
      if (input$rdeu) {
        r6_siif_aux$
          get_query(
            paste0("SELECT fecha_hasta, fecha_aprobado, mes_hasta, ",
                   "nro_entrada, cta_cte, cuit, monto, saldo ",
                   "FROM deuda_flotante_rdeu012")
          )$
          mutate(
            cta_cte = map_values(.data$cta_cte,
                                 from = primary_key_cta_cte()$siif_contabilidad_cta_cte,
                                 to = primary_key_cta_cte()$map_to,
                                 warn_missing = FALSE),
            fecha_aprobado = as.Date(.data$fecha_aprobado, origin = "1970-01-01"),
            fecha_hasta = as.Date(.data$fecha_hasta, origin = "1970-01-01"),
            fecha = ifelse(.data$fecha_aprobado > .data$fecha_hasta,
                           .data$fecha_hasta, .data$fecha_aprobado),
            fecha = as.Date(.data$fecha, origin = "1970-01-01"),
            mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                                 lubridate::year(.data$fecha), sep = "/"),
            ejercicio = as.character(lubridate::year(.data$fecha))
          )

        #Neteamos los comprobantes de gastos no pagados (Deuda Flotante)
        r6_siif$data <- r6_siif_aux$data %>%
          # dplyr::filter(.data$ejercicio == ejercicio_vec) %>%
          dplyr::select(.data$ejercicio, .data$mes, .data$fecha,
                        .data$nro_entrada, .data$saldo) %>%
          dplyr::distinct(.data$nro_entrada, .data$mes, .keep_all = TRUE) %>%
          dplyr::semi_join(dplyr::filter(r6_siif$data),
                           by = c("nro_entrada" = "nro_entrada",
                                  "mes" = "mes")) %>%
          dplyr::mutate(monto = (.data$saldo * (-1)),
                        cuit = "30632351514") %>%
          dplyr::select(-.data$saldo) %>%
          dplyr::bind_rows(r6_siif$data)

        #Ajustamos la Deuda Flotante Pagada

        r6_siif_aux$data <- dplyr::tibble(
          fecha_hasta = unique(r6_siif_aux$data$fecha_hasta),
          lead_fecha_hasta = dplyr::lead(unique(r6_siif_aux$data$fecha_hasta))
        ) %>%
          dplyr::left_join(r6_siif_aux$data, .data, by = "fecha_hasta") %>%
          dplyr::filter(!is.na(.data$lead_fecha_hasta)) %>%
          dplyr::rename(fecha_borrar = .data$fecha_hasta,
                        fecha_hasta = .data$lead_fecha_hasta) %>%
          dplyr::mutate(
            mes_hasta = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha_hasta), 2, pad = "0"),
                                       lubridate::year(.data$fecha_hasta), sep = "/")
          ) %>%
          dplyr::anti_join(r6_siif_aux$data) %>%
          dplyr::mutate(ejercicio_ant = as.character(lubridate::year(.data$fecha_borrar)),
                        ejercicio = as.character(lubridate::year(.data$fecha_hasta))) %>%
          dplyr::select(-.data$fecha_borrar)

        #Incorporamos los comprobantes de gastos pagados en periodos posteriores (Deuda Flotante)
          r6_siif_aux$
            filter(.data$ejercicio == ejercicio_vec)$
            semi_join(dplyr::filter(r6_siif$data),
                            by = c("nro_entrada" = "nro_entrada",
                                   "mes" = "mes"))$
            mutate(monto = .data$saldo,
                   cuit = "30632351514")$
            select(.data$ejercicio, fecha = .data$fecha_hasta,
                   mes = .data$mes_hasta, .data$nro_entrada,
                   .data$cuit, .data$monto)

          r6_siif$
            bind_rows(r6_siif_aux$data)

      }

      #Filtramos por fecha y ejercicio
      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_siif$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      r6_siif$filter(.data$ejercicio %in% ejercicio_vec)

      #Grouping and summarising siif
      r6_siif$
        select(input$grupo %||% "mes", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(ejecutado_siif = sum(.data$monto, na.rm = TRUE))

      #Filtering sscc_banco_invico
      r6_sscc$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, codigo_imputacion, ",
                 "monto, concepto FROM banco_invico ",
                 "WHERE movimiento <> 'DEPOSITO' ",
                 "AND cta_cte = '130832-04' ",
                 "AND ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        filter(!stringr::str_detect(.data$concepto, "GCIAS"))$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          monto = .data$monto * (-1)
        )

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_sscc$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      if (input$transf_internas) {
        r6_sscc$filter(
          .data$codigo_imputacion != 34 &
            .data$codigo_imputacion != 4
        )
      }

      if (input$otros_sscc) {
        r6_sscc$filter(
          .data$codigo_imputacion != 3 &
            .data$codigo_imputacion != 55 &
            .data$codigo_imputacion != 5 &
            .data$codigo_imputacion != 13
        )
      }

      #Grouping and summarising sgf
      r6_sscc$
        select(input$grupo %||% "mes", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(pagado_sscc = sum(.data$monto, na.rm = TRUE))

      #Joinning and calulating
      r6_siif$
        full_join(r6_sscc$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          diferencia = .data$ejecutado_siif - .data$pagado_sscc,
          dif_acum = cumsum(.data$diferencia)
        )

      total_desvio <- sum(abs(r6_siif$data$diferencia))
      r6_siif$mutate(prop_desv = (abs(.data$diferencia) / total_desvio))

      return(r6_siif$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_02_02_sueldo_ui("02_02_02_sueldo_1")

## To be copied in the server
# mod_02_02_02_sueldo_server("02_02_02_sueldo_1")
