#' 02_02_07_retenciones UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_07_retenciones_ui <- function(id){

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
                                     choices = c("mes", "fecha", "cta_cte", "cuit", "nro_entrada"),
                                     selected = c("mes", "cta_cte"),
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

#' 02_02_07_retenciones Server Functions
#'
#' @noRd
mod_02_02_07_retenciones_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      #Movimientos Grales SSCC por certificados negativos?
      list(icaro_carga())
    })

    observeEvent(to_listen(), {

      r6_icaro <- MyData$new(sql_path("icaro_new"))

      r6_icaro$
        get_query("SELECT DISTINCT fecha FROM carga")$
        mutate(fecha = as.Date(.data$fecha, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      choices_rv$ejercicio <- sort(unique(r6_icaro$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio)

      r6_icaro$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM carga")
      )

      choices_rv$fecha <- c(
        r6_icaro$data$max_fecha, r6_icaro$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")

      r6_icaro$data <- map_cta_cte("icaro_new",
                                  "SELECT DISTINCT cta_cte FROM carga",
                                  "icaro_cta_cte")

      choices_rv$cta_cte <- sort(unique(r6_icaro$data))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_retenciones <- MyData$new(sql_path("icaro_new"))
      r6_siif <- MyData$new(sql_path("siif"))

      #Setting input default value
      if (is.null(input$ejercicio)) {
        shiny::updateSelectizeInput(session, "ejercicio",
                                    selected = max(as.integer(choices_rv$ejercicio)))
      }

      if (is.null(input$grupo)) {
        shiny::updateCheckboxGroupInput(session, "grupo",
                                        selected = c("mes", "cta_cte"))
      }

      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))

      cta_cte_vec <- input$cta_cte %||%
        unique(choices_rv$cta_cte)


      #Filtering icaro
      r6_icaro$
        get_query(
          paste0("SELECT nro_entrada, fecha, cta_cte, cuit, tipo, ",
                 "importe as bruto, nro_entrada || tipo AS id FROM carga ",
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
                                         sep="/"),
            id = stringr::str_c(.data$nro_entrada, "CYO", sep = "")
          )

        #Neteamos los comprobantes de gastos no pagados (Deuda Flotante)
        r6_icaro$data <- r6_siif$data %>%
          dplyr::filter(.data$ejercicio == ejercicio_vec) %>%
          dplyr::select(.data$nro_entrada, .data$mes, .data$saldo) %>%
          dplyr::distinct(.data$nro_entrada, .data$mes, .keep_all = TRUE) %>%
          dplyr::inner_join(dplyr::filter(r6_icaro$data, .data$tipo != "PA6"),
                           by = c("nro_entrada" = "nro_entrada",
                                  "mes" = "mes")) %>%
          dplyr::mutate(bruto = (.data$saldo * (-1)),
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
            mutate(bruto = .data$saldo,
                   tipo = "RDEU")$
            select(.data$ejercicio, fecha = .data$fecha_hasta,
                   mes = .data$mes_hasta, .data$nro_entrada,
                   .data$cuit, .data$cta_cte,
                   .data$tipo, .data$bruto, .data$id)

          r6_icaro$
            bind_rows(r6_siif$data)

      }

      #FILTRAMOS por EJERCICIO Y CTA_CTE seleccionado
      r6_icaro$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$cta_cte %in% cta_cte_vec)$
        select(-.data$ejercicio)

      #Agregamos las retenciones
      r6_retenciones$
        get_query(
          paste0("SELECT nro_entrada || tipo AS id, importe AS retencion, ",
                 "CASE ",
                 "WHEN (cod_ret = '106' OR cod_ret = '113') THEN 'gcias' ",
                 "WHEN (cod_ret = '102' OR cod_ret = '111') THEN 'sellos' ",
                 "WHEN (cod_ret = '104' OR cod_ret = '112') THEN 'lp' ",
                 "WHEN (cod_ret = '101' OR cod_ret = '110') THEN 'iibb' ",
                 "WHEN (cod_ret = '114' OR cod_ret = '117') THEN 'suss' ",
                 "WHEN (cod_ret = '337') THEN 'invico' ",
                 "ELSE 'Otros' ",
                 "END cod_ret ",
                 "FROM retenciones")
        )
      r6_icaro$
        left_join(r6_retenciones$data, .data, by = "id")

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
        select(input$grupo %||% "mes", .data$bruto,
               .data$cod_ret, .data$retencion)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        pivot_wider(names_from = .data$cod_ret, values_from = .data$retencion,
                    values_fn = sum, values_fill = 0)$
        mutate(neto = .data$bruto - .data$lp - .data$sellos
               - .data$gcias - .data$suss - .data$iibb - .data$invico)$
        summarise_all(sum)$
        select(-.data$`NA`)
        # summarise(bruto = sum(.data$bruto, na.rm = TRUE),
        #           gcias = sum(.data$gcias, na.rm = TRUE))

      return(r6_icaro$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_02_07_retenciones_ui("02_02_07_retenciones_1")

## To be copied in the server
# mod_02_02_07_retenciones_server("02_02_07_retenciones_1")
