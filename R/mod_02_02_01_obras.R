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
                                     inline = FALSE),

        rep_br(2),

        checkboxInput(ns("rdeu"),
                      "\u00bfAjustar Deuda Flotante?", value = TRUE)

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
          unique() %>%
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

      #Grouping and summarising icaro
      r6_icaro$
        select(input$grupo %||% "mes", .data$importe)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(ejecutado_icaro = sum(.data$importe, na.rm = TRUE))

      #Filtering sgf
      r6_sgf$
        get_query(
          paste0("SELECT P.cuit, R.fecha, R.cta_cte, R.importe_bruto, ",
                 "R.origen, R.destino FROM ", sql_join, " ",
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
               !(.data$origen == "EPAM" & stringr::str_detect(.data$destino, "HONORARIOS")),
               .data$cta_cte %in% cta_cte_vec)$
        select(-.data$ejercicio, -.data$destino, -.data$origen)

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
