#' 02_02_03_honorarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_03_honorarios_ui <- function(id){

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
                              choices = c("130832-05", "130832-07"),
                              selected = "", multiple = TRUE,
                              options = list(placeholder = "Todo seleccionado"))
      ),
      shiny::column(
        6,
        shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                  choices = c("ejercicio", "mes", "fecha",
                                              "cta_cte", "beneficiario"),
                                  selected = "mes",
                                  inline = FALSE),


      ),

      checkboxInput(ns("embargo"),
                    "Incluir embargos no considerados en Resumen Rend SGF", value = TRUE)


      )
  )
}

#' 02_02_03_honorarios Server Functions
#'
#' @noRd
mod_02_02_03_honorarios_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    # sql_join <- paste0(
    #   "(SELECT * ",
    #   "FROM resumen_rend_prov) R LEFT JOIN " ,
    #   "(SELECT cuit, descripcion ",
    #   "FROM listado_prov) P ",
    #   "ON R.beneficiario = P.descripcion"
    # )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(slave_honorarios(),
           sgf_resumen_rend_prov())
    })

    observeEvent(to_listen(), {

      r6_slave <- MyData$new(sql_path("slave"))
      r6_sgf <- MyData$new(sql_path("sgf"))

      r6_slave$
        get_query(paste0("SELECT DISTINCT ejercicio ",
                         "FROM honorarios"))

      r6_sgf$
        get_query(paste0("SELECT DISTINCT ejercicio ",
                         "FROM resumen_rend_prov"))

      choices_rv$ejercicio <- sort(c(r6_sgf$data$ejercicio,
                                     r6_slave$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_slave$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM honorarios")
      )

      r6_sgf$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM resumen_rend_prov")
      )

      r6_slave$bind_rows(r6_sgf$data)

      choices_rv$fecha <- c(
        r6_slave$data$max_fecha, r6_slave$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_slave <- MyData$new(sql_path("slave"))
      r6_siif <- MyData$new(sql_path("siif"))
      r6_sgf <- MyData$new(sql_path("sgf"))
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
        c("130832-05", "130832-07")

      #Filtering siif_rec
      r6_slave$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, nro_entrada, ",
                 "beneficiario, importe_bruto ",
                 "FROM honorarios ",
                 "WHERE ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )

      r6_siif$get_query(
          paste0("SELECT nro_entrada, fecha, cta_cte ",
                 "FROM comprobantes_gtos_rcg01_uejp")
        )$mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_gastos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          nro_entrada = sprintf("%05d", as.numeric(.data$nro_entrada)),
          nro_entrada = stringr::str_c(.data$nro_entrada,
                                       format(.data$fecha, format="%y"),
                                       sep="/")
        )

      cta_cte_siif <- r6_slave$data %>%
        dplyr::left_join(r6_siif$data, by = "nro_entrada") %>%
        dplyr::select(.data$cta_cte)

      r6_slave$data$cta_cte <- cta_cte_siif$cta_cte

      r6_slave$filter(
        .data$cta_cte %in% cta_cte_vec
      )

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_slave$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Grouping and summarising slave
      r6_slave$
        select(input$grupo %||% "mes", .data$importe_bruto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(bruto_slave = sum(.data$importe_bruto, na.rm = TRUE))

      #Filtering sgf_invico
      r6_sgf$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, destino, ",
                 "importe_bruto, cta_cte, ",
                 "beneficiario FROM resumen_rend_prov ",
                 "WHERE origen <> 'OBRAS' ",
                 "AND cta_cte IN ('130832-05', '130832-07') ",
                 "AND destino IN ('HONORARIOS - FUNCIONAMIENTO', ",
                 "'COMISIONES - FUNCIONAMIENTO', 'HONORARIOS - EPAM') ",
                 "AND ejercicio = ?"),
          params = list(ejercicio_vec)
        )

      #Deposito de embargo por alimientos
      if (input$embargo) {

        r6_sscc$
          get_query(
            paste0("Select ejercicio, mes, fecha, ",
                   "monto as importe_bruto, cta_cte, ",
                   "beneficiario FROM banco_invico ",
                   "WHERE cta_cte = '130832-05' ",
                   "AND codigo_imputacion = 49 ",
                   "AND ejercicio = ?"),
            params = list(ejercicio_vec)
          )$
          mutate(
            destino = "Embargo por Alimento",
            importe_bruto = (.data$importe_bruto * (-1))
          )

        r6_sgf$data <- r6_sgf$data %>%
          dplyr::bind_rows(r6_sscc$data)

      }

      r6_sgf$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )$
        filter(
          .data$cta_cte %in% cta_cte_vec
        )

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
      r6_slave$
        full_join(r6_sgf$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          dif_bruto = .data$bruto_slave - .data$bruto_sgf,
          dif_acum = cumsum(.data$dif_bruto)
        )

      total_desvio <- sum(abs(r6_slave$data$dif_bruto))
      r6_slave$mutate(prop_desv = (abs(.data$dif_bruto) / total_desvio))

      return(r6_slave$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_02_03_honorarios_ui("02_02_03_honorarios_1")

## To be copied in the server
# mod_02_02_03_honorarios_server("02_02_03_honorarios_1")
