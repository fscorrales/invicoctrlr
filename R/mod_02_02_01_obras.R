#' 02_02_01_obras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_01_obras_ui <- function(id){

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

        shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("mes", "fecha", "cta_cte", "cuit"),
                                     selected = "mes",
                                     inline = FALSE)

      )
      ),

    shiny::fluidRow(
      shiny::column(
        6, checkboxInput(ns("rdeu"), "\u00bfAjustar Deuda Flotante?",
                         value = TRUE)
        ),
      shiny::column(
        6,
        checkboxInput(ns("cheq"), "\u00bfAjustar reemplazo cheque?",
                         value = FALSE),
        checkboxInput(ns("cert_neg"), "\u00bfAjustar certificados negativos SSCC?",
                      value = TRUE)
      )
    )

  )
}

#' 02_04_03_registro Server Functions
#'
#' @noRd
mod_02_02_01_obras_server <- function(id){
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
      #Movimientos Grales SSCC por certificados negativos?
      list(icaro_carga(),
           sgf_resumen_rend_prov())
    })

    observeEvent(to_listen(), {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_sgf <- MyData$new(sql_path("sgf"))

      r6_icaro$
        get_query("SELECT DISTINCT fecha FROM carga")$
        mutate(fecha = as.Date(.data$fecha, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      r6_sgf$
        get_query(paste0("SELECT DISTINCT fecha FROM resumen_rend_prov"))$
        mutate(fecha = as.Date(.data$fecha, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      choices_rv$ejercicio <- sort(c(unique(r6_icaro$data$ejercicio),
                                     unique(r6_sgf$data$ejercicio)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio)

      r6_icaro$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM carga")
      )

      r6_sgf$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM resumen_rend_prov")
      )

      r6_icaro$bind_rows(r6_sgf$data)

      choices_rv$fecha <- c(
        r6_icaro$data$max_fecha, r6_icaro$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")

      r6_icaro$data <- map_cta_cte("icaro_new",
                                  "SELECT DISTINCT cta_cte FROM carga",
                                  "icaro_cta_cte")

      r6_sgf$data <-  map_cta_cte("sgf",
                                  paste0("SELECT DISTINCT cta_cte FROM resumen_rend_prov ",
                                         "WHERE cta_cte <> ''"),
                                  "sgf_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_icaro$data, r6_sgf$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_siif <- MyData$new(sql_path("siif"))
      r6_sgf <- MyData$new(sql_path("sgf"))
      r6_slave <- MyData$new(sql_path("slave"))
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

      #Filtering icaro
      r6_icaro$
        get_query(
          paste0("SELECT fecha, cta_cte, cuit, tipo, ",
                 "importe, nro_entrada FROM carga ",
                 "WHERE tipo <> 'REG'")
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$icaro_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                               lubridate::year(.data$fecha), sep = "/"),
          ejercicio = as.character(lubridate::year(.data$fecha))
        )

      #Depurar con RDEU
      if (input$rdeu) {
        r6_siif$
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
            ejercicio = as.character(lubridate::year(.data$fecha)),
            nro_entrada = sprintf("%05d", as.numeric(.data$nro_entrada)),
            nro_entrada = stringr::str_c(.data$nro_entrada,
                                         format(.data$fecha, format="%y"),
                                         sep="/")
          )

        #Neteamos los comprobantes de gastos no pagados (Deuda Flotante)
        r6_icaro$data <- r6_siif$data %>%
          dplyr::filter(.data$ejercicio == ejercicio_vec) %>%
          dplyr::select(.data$nro_entrada, .data$mes, .data$saldo) %>%
          dplyr::distinct(.data$nro_entrada, .data$mes, .keep_all = TRUE) %>%
          dplyr::inner_join(dplyr::filter(r6_icaro$data, .data$tipo != "PA6"),
                           by = c("nro_entrada" = "nro_entrada",
                                  "mes" = "mes")) %>%
          dplyr::mutate(importe = (.data$saldo * (-1)),
                        tipo = "RDEU") %>%
          dplyr::select(-.data$saldo) %>%
          dplyr::bind_rows(r6_icaro$data)

        #Ajustamos la Deuda Flotante Pagada
        r6_siif$data <- dplyr::tibble(
          fecha_hasta = unique(r6_siif$data$fecha_hasta),
          lead_fecha_hasta = dplyr::lead(unique(r6_siif$data$fecha_hasta))
        ) %>%
          dplyr::left_join(r6_siif$data, .data, by = "fecha_hasta") %>%
          dplyr::filter(!is.na(.data$lead_fecha_hasta)) %>%
          dplyr::rename(fecha_borrar = .data$fecha_hasta,
                        fecha_hasta = .data$lead_fecha_hasta) %>%
          dplyr::mutate(
            mes_hasta = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha_hasta), 2, pad = "0"),
                                       lubridate::year(.data$fecha_hasta), sep = "/")
          ) %>%
          dplyr::anti_join(r6_siif$data) %>%
          dplyr::mutate(ejercicio_ant = as.character(lubridate::year(.data$fecha_borrar)),
                        ejercicio = as.character(lubridate::year(.data$fecha_hasta))) %>%
          dplyr::select(-.data$fecha_borrar)

        #Incorporamos los comprobantes de gastos pagados en periodos posteriores (Deuda Flotante)
          r6_siif$
            filter(.data$ejercicio == ejercicio_vec)$
            semi_join(dplyr::filter(r6_icaro$data, .data$tipo != "PA6"),
                            by = c("nro_entrada" = "nro_entrada"))$
            mutate(importe = .data$saldo,
                   tipo = "RDEU")$
            select(.data$ejercicio, fecha = .data$fecha_hasta,
                   mes = .data$mes_hasta, .data$nro_entrada,
                   .data$cuit, .data$cta_cte,
                   .data$tipo, .data$importe)

          r6_icaro$
            bind_rows(r6_siif$data)

      }

      r6_icaro$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$cta_cte %in% cta_cte_vec)$
        select(-.data$ejercicio)

      # print(dplyr::filter(r6_icaro$data, mes == "02/2022" &
      #                       cta_cte == "130832-07" &
      #                       tipo == "RDEU"))

      #Filtramos por fecha y ejercicio
      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_icaro$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising icaro
      r6_icaro$
        select(input$grupo %||% "mes", .data$importe)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(ejecutado_icaro = sum(.data$importe, na.rm = TRUE))

      #Filtering sgf
      r6_slave$
        get_query("SELECT DISTINCT beneficiario FROM honorarios")

      r6_sgf$
        get_query(
          paste0("SELECT P.cuit, R.beneficiario, R.fecha, R.cta_cte, R.importe_bruto, ",
                 "R.origen, R.movimiento FROM ", sql_join, " ",
                 "WHERE R.origen <> 'FUNCIONAMIENTO'")
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sgf_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                               lubridate::year(.data$fecha), sep = "/"),
          ejercicio = as.character(lubridate::year(.data$fecha))
        )$
        filter(.data$ejercicio %in% ejercicio_vec,
               !(.data$origen == "EPAM" & .data$beneficiario %in% r6_slave$data$beneficiario),
               .data$cta_cte %in% cta_cte_vec)$
        select(-.data$ejercicio, -.data$origen, -.data$beneficiario)

      #SISTEMAS eliminó el campo destino de Resumen de Rendiciones Prov
      #por lo que tuve idear el siguiente mecanismo para filtrar los honorarios
      # r6_sgf$
      #   anti_join(
      #     r6_slave$data, by = c("beneficiario")
      # )$
      #   select(-.data$beneficiario)

      #Filtramos por fecha y ejercicio
      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_sgf$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Eliminamos registros duplicados de SGR en cta_cte 106
      sgf_106 <- dplyr::filter(r6_sgf$data, cta_cte == '106') %>%
        dplyr::distinct()
      r6_sgf$
        filter(cta_cte != '106')$
        bind_rows(sgf_106)

      if (input$cheq) {
        r6_sscc$get_query(
          paste0(
            "SELECT movimiento, cta_cte, fecha ",
            "FROM banco_invico ",
            "WHERE ejercicio = ? ",
            "AND codigo_imputacion = 55 ",
            "AND es_cheque = 1"
          ),
          params = list(ejercicio_vec)
        )$mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sscc_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE)
        )$filter(
          .data$cta_cte %in% cta_cte_vec)

        #Filtramos por fecha y ejercicio
        if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
          r6_sscc$filter(
            dplyr::between(.data$fecha,
                           lubridate::ymd(input$fecha[[1]]),
                           lubridate::ymd(input$fecha[[2]]))
          )
        }

        r6_sgf$anti_join(
          r6_sscc$data, by = c("movimiento", "cta_cte")
        )

      }

      if (input$cert_neg) {
        r6_sscc$get_query(
          paste0(
            "SELECT movimiento, cta_cte, mes, fecha, ",
            "monto as importe_bruto FROM banco_invico ",
            "WHERE ejercicio = ? ",
            "AND codigo_imputacion = 18 ",
            "AND es_cheque = 0 ",
            "AND movimiento = 'DEPOSITO'"
          ),
          params = list(ejercicio_vec)
        )$mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sscc_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          cuit = "30632351514",
          importe_bruto = .data$importe_bruto * (-1)
        )$filter(
          .data$cta_cte %in% cta_cte_vec)

        #Filtramos por fecha y ejercicio
        if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
          r6_sscc$filter(
            dplyr::between(.data$fecha,
                           lubridate::ymd(input$fecha[[1]]),
                           lubridate::ymd(input$fecha[[2]]))
          )
        }

        r6_sgf$bind_rows(
          r6_sscc$data
        )

      }

      #Grouping and summarising sgf
      r6_sgf$
        select(input$grupo %||% "mes", .data$importe_bruto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(bruto_sgf = sum(.data$importe_bruto, na.rm = TRUE))

      #Joinning and calulating
      r6_icaro$
        full_join(r6_sgf$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          diferencia = .data$ejecutado_icaro - .data$bruto_sgf,
          dif_acum = cumsum(.data$diferencia)
        )

      total_desvio <- sum(abs(r6_icaro$data$diferencia))
      r6_icaro$mutate(prop_desv = (abs(.data$diferencia) / total_desvio))

      return(r6_icaro$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_02_01_obras_ui("02_02_01_obras_1")

## To be copied in the server
# mod_02_02_01_obras_server("02_02_01_obras_1")
