#' 02_03_03_diferencia UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_03_03_diferencia_ui <- function(id){

  ns <- NS(id)

  tagList(
    shiny::column(12, align = "center",
                  bs4Dash::actionButton(ns("update"),
                                        "Actualizar Filtros",
                                        status = "primary")),

    rep_br(),

    shiny::fluidRow(

      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio Hasta",
                                 choices = "", selected = "", multiple = FALSE),
        suppressWarnings(
          shiny::dateInput(ns("fecha"), "Fecha Hasta", format = "dd-mm-yyyy",
                          startview = "month", language = "es", value = NA)
        ),

        shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                              choices = "", selected = "", multiple = TRUE,
                              options = list(placeholder = "Todo seleccionado"))

        ),

      shiny::column(
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "mes","cta_cte"),
                                     selected = "cta_cte" , inline = FALSE)
      )

      )

  )
}

#' 02_03_03_diferencia Server Functions
#'
#' @noRd
mod_02_03_03_diferencia_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Initial DBs setting
    # db_sscc <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db <- sscc_banco_invico() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$sscc_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE),
    #                   ejercicio = as.character(lubridate::year(.data$fecha))
    #                   )
    #   return(db)
    #
    # })
    #
    # db_rdeu <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db <- siif_deuda_flotante_rdeu012() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$siif_contabilidad_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE),
    #                   fecha = .data$fecha_hasta,
    #                   ejercicio = as.character(lubridate::year(.data$fecha)),
    #                   mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha),
    #                                                         2, pad = "0"),
    #                                        lubridate::year(.data$fecha), sep = "/")
    #     )
    #     # dplyr::select(-fecha_hasta, mes_hasta)
    #
    #   return(db)
    #
    # })
    #
    # db_metodo_2 <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db_rec <- siif_comprobantes_rec_rci02() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$siif_recursos_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE)
    #     )
    #
    #   db_gto <- siif_comprobantes_gtos_rcg01_uejp() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$siif_gastos_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE),
    #                   mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha),
    #                                                         2, pad = "0"),
    #                                        lubridate::year(.data$fecha), sep = "/")
    #     )
    #
    #   db <- db_gto %>%
    #     dplyr::select(.data$ejercicio, .data$fecha, .data$mes, .data$cta_cte,
    #                   gasto = .data$monto) %>%
    #     dplyr::bind_rows(dplyr::select(db_rec, .data$ejercicio, .data$fecha, .data$mes,
    #                                    .data$cta_cte, recurso = .data$monto))
    #
    #   return(db)
    #
    # })

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(sscc_banco_invico(),
           siif_deuda_flotante_rdeu012(),
           siif_comprobantes_rec_rci02(),
           siif_comprobantes_gtos_rcg01_uejp())
    })

    observeEvent(to_listen(), {

      r6_sscc <- MyData$new(sql_path("sscc"))
      r6_siif_rdeu <- MyData$new(sql_path("siif"))
      r6_siif_rec <- MyData$new(sql_path("siif"))
      r6_siif_gto <- MyData$new(sql_path("siif"))

      r6_sscc$data <-  map_cta_cte("sscc",
                                   "SELECT DISTINCT cta_cte FROM banco_invico",
                                   "sscc_cta_cte")

      r6_siif_rdeu$data <- map_cta_cte("siif",
                                  "SELECT DISTINCT cta_cte FROM deuda_flotante_rdeu012",
                                  "siif_contabilidad_cta_cte")

      r6_siif_rec$data <-  map_cta_cte("siif",
                                       "SELECT DISTINCT cta_cte FROM comprobantes_rec_rci02",
                                       "siif_recursos_cta_cte")

      r6_siif_gto$data <- map_cta_cte("siif",
                                      "SELECT DISTINCT cta_cte FROM comprobantes_gtos_rcg01_uejp",
                                      "siif_gastos_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_sscc$data, r6_siif_rdeu$data,
                                          r6_siif_rec$data, r6_siif_gto$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      r6_sscc$get_query("SELECT DISTINCT ejercicio FROM banco_invico")

      r6_siif_rdeu$
        get_query("SELECT DISTINCT fecha_hasta FROM deuda_flotante_rdeu012")$
        mutate(fecha = as.Date(.data$fecha_hasta, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      r6_siif_rec$
        get_query("SELECT DISTINCT ejercicio FROM comprobantes_rec_rci02")

      r6_siif_gto$
        get_query("SELECT DISTINCT ejercicio FROM comprobantes_gtos_rcg01_uejp")

      choices_rv$ejercicio <- sort(unique(c(r6_sscc$data$ejercicio,
                                            r6_siif_rdeu$data$ejercicio,
                                            r6_siif_rec$data$ejercicio,
                                            r6_siif_gto$data$ejercicio)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_sscc$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, ",
               "MIN(fecha) as min_fecha ",
               "FROM banco_invico")
      )

      r6_siif_rdeu$get_query(
        paste0("SELECT MAX(fecha_hasta) as max_fecha, ",
               "MIN(fecha_hasta) as min_fecha ",
               "FROM deuda_flotante_rdeu012")
      )

      r6_siif_rec$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, ",
               "MIN(fecha) as min_fecha ",
               "FROM comprobantes_rec_rci02")
      )

      r6_siif_gto$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, ",
               "MIN(fecha) as min_fecha ",
               "FROM comprobantes_gtos_rcg01_uejp")
      )

      r6_sscc$
        bind_rows(r6_siif_rdeu$data)$
        bind_rows(r6_siif_rec$data)$
        bind_rows(r6_siif_gto$data)


      choices_rv$fecha <- c(
        r6_sscc$data$max_fecha, r6_sscc$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")

      shiny::updateDateInput(session, "fecha",
                             min = min(choices_rv$fecha),
                             max = max(choices_rv$fecha))

      r6_sscc$finalize()
      r6_siif_rdeu$finalize()
      r6_siif_rec$finalize()
      r6_siif_gto$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_sscc <- MyData$new(sql_path("sscc"))
      r6_siif_rdeu <- MyData$new(sql_path("siif"))
      r6_siif_rec <- MyData$new(sql_path("siif"))
      r6_siif_gto <- MyData$new(sql_path("siif"))

      #Setting input default value
      if (is.null(input$ejercicio)) {
        shiny::updateSelectizeInput(session, "ejercicio",
                                    selected = max(as.integer(choices_rv$ejercicio)))
      }

      if (is.null(input$grupo)) {
        shiny::updateCheckboxGroupInput(session, "grupo",
                                        selected = "cta_cte")
      }

      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))

      cta_cte_vec <- input$cta_cte %||%
        unique(choices_rv$cta_cte)

      #Filtering sscc_banco_invico
      r6_sscc$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, cta_cte, ",
                 "monto FROM banco_invico")
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sscc_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      if (length(input$fecha) > 0) {
        r6_sscc$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd("2017/01/01"),
                         lubridate::ymd(input$fecha))
        )
      } else{
        #Only for the initial loop
        if (input$ejercicio == "") {
          ejercicio_max <- max(as.integer(choices_rv$ejercicio))
        } else {
          ejercicio_max <- as.integer(input$ejercicio)
        }

        r6_sscc$filter(
          dplyr::between(.data$ejercicio, 2017, ejercicio_max)
        )

      }

      #Filtering siif_rdeu
      mes_hasta_rdeu <- dplyr::last(sort(r6_sscc$data$fecha))
      mes_hasta_rdeu <- stringr::str_c(
        stringr::str_pad(lubridate::month(mes_hasta_rdeu), 2, pad = "0"),
        lubridate::year(mes_hasta_rdeu), sep = "/"
      )

      r6_siif_rdeu$
        get_query(
          paste0("SELECT mes_hasta, fecha_hasta, cta_cte, ",
                 "saldo FROM deuda_flotante_rdeu012 ",
                 "WHERE mes_hasta = '", mes_hasta_rdeu, "'")
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_recursos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha_hasta, origin = "1970-01-01"),
          ejercicio = as.character(lubridate::year(.data$fecha)),
          mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha),
                                                2, pad = "0"),
                               lubridate::year(.data$fecha), sep = "/")
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      #Grouping and summarising sscc
      r6_sscc$
        select(input$grupo %||% "cta_cte", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "cta_cte"))$
        summarise(saldo_banco = sum(.data$monto, na.rm = TRUE))
      # mutate(saldo_banco = cumsum(sum_banco))

      #Grouping and summarising siif_rdeu
      r6_siif_rdeu$
        select(input$grupo %||% "cta_cte", .data$saldo)$
        group_by(!!! rlang::syms(input$grupo %||% "cta_cte"))$
        summarise(deuda_flotante = sum(.data$saldo, na.rm = TRUE))

      #Joinning and calulating
      r6_sscc$
        full_join(r6_siif_rdeu$data, by = input$grupo %||% "cta_cte")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          remanente_1 = .data$saldo_banco - .data$deuda_flotante
        )$
        select(-.data$saldo_banco, -.data$deuda_flotante)

      #Filtering metodo_2
      #Filtering siif_rec
      r6_siif_rec$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, cta_cte, fuente, ",
                 "monto AS recurso FROM comprobantes_rec_rci02")
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_recursos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )

      #Filtering siif_gto
      r6_siif_gto$
        get_query(
          paste0("SELECT ejercicio, fecha, cta_cte, fuente, ",
                 "monto AS gasto FROM comprobantes_gtos_rcg01_uejp")
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_gastos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha),
                                                2, pad = "0"),
                               lubridate::year(.data$fecha), sep = "/")
        )

      #Joining rec y gto and filtering
      r6_siif_rec$
        bind_rows(r6_siif_gto$data)$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$cta_cte %in% cta_cte_vec)

      if (length(input$fecha) > 0) {
        r6_siif_rec$
          filter(
            .data$ejercicio == lubridate::year(input$fecha),
            max(.data$fecha) == lubridate::ymd(input$fecha)
          )
      }

      #Grouping and summarising sscc
      r6_siif_rec$
        select(input$grupo %||% "cta_cte", .data$recurso, .data$gasto)$
        group_by(!!! rlang::syms(input$grupo %||% "cta_cte"))$
        summarise(recursos = sum(.data$recurso, na.rm = TRUE),
                  gastos = sum(.data$gasto, na.rm = TRUE),
                  remanente_2 = .data$recursos - .data$gastos)$
        mutate_if(is.numeric, replace_NA_0)$
        select(-.data$recursos, -.data$gastos)

      #Joinning both methods
      r6_sscc$
        full_join(r6_siif_rec$data, by = input$grupo %||% "cta_cte")$
        mutate(diferencia = .data$remanente_1 - .data$remanente_2)$
        mutate_if(is.numeric, replace_NA_0)

      return(r6_sscc$data)

      r6_sscc$finalize()
      r6_siif_rdeu$finalize()
      r6_siif_rec$finalize()
      r6_siif_gto$finalize()

      # #Filtering metodo_2
      # metodo_2 <- db_metodo_2() %>%
      #   dplyr::filter(.data$ejercicio %in% (input$ejercicio %||%
      #                                   max(as.integer(ejercicio_var()$ejercicio))),
      #                 .data$cta_cte %in% (input$cta_cte %||%
      #                                 unique(ejercicio_var()$cta_cte))
      #   )
      #
      # if (length(input$fecha) > 0) {
      #   metodo_2 <- metodo_2 %>%
      #     dplyr::filter(
      #       .data$ejercicio == lubridate::year(input$fecha),
      #       max(.data$fecha) == lubridate::ymd(input$fecha)
      #     )
      # }
      #
      # #Grouping and summarising metodo_2
      # metodo_2 <- metodo_2 %>%
      #   dplyr::select(input$grupo %||% "cta_cte", .data$recurso, .data$gasto) %>%
      #   dplyr::group_by(!!! rlang::syms(input$grupo %||% "cta_cte")) %>%
      #   dplyr::summarise(recursos = sum(.data$recurso, na.rm = TRUE),
      #                    gastos = sum(.data$gasto, na.rm = TRUE),
      #                    remanente_2 = .data$recursos - .data$gastos) %>%
      #   # tidyr::replace_na(list(recursos = 0, gastos = 0,
      #   #                        remanente = 0)) %>%
      #   dplyr::select(-.data$recursos, -.data$gastos)
      #
      # #Joinning both methods
      # db <- metodo_1 %>%
      #   dplyr::full_join(metodo_2, by = input$grupo %||% "cta_cte") %>%
      #   replace(., is.na(.), 0) %>%
      #   # tidyr::replace_na(list(remanente_1 = 0, remanente_2 = 0)) %>%
      #   dplyr::mutate(diferencia = .data$remanente_1 - .data$remanente_2)
      #
      # return(db)

    })

    return(table)


  })
}

## To be copied in the UI
# mod_02_03_03_diferencia_ui("02_03_03_diferencia_1")

## To be copied in the server
# mod_02_03_03_diferencia_server("02_03_03_diferencia_1")
