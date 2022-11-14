#' 02_02_08_sgf_vs_sscc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_02_08_sgf_vs_sscc_ui <- function(id){

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
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "fecha", "mes",
                                                 "cta_cte", "beneficiario",
                                                 "libramiento"),
                                     selected = "mes" , inline = FALSE)
        )

      ),
    # shiny::fluidRow(
    #   shiny::column(
    #     6, shiny::radioButtons(ns("dep_transf_int"),
    #                            "\u00bfDepurar Tranf. Internas?",
    #                            choices = c("SI", "NO"), selected = "SI")
    #     ),
    #   shiny::column(
    #     6, shiny::radioButtons(ns("dep_pf"),
    #                            "\u00bfDepurar Inversiones en PF?",
    #                            choices = c("SI", "NO"), selected = "SI")
    #     )
    #   ),
    # shiny::fluidRow(
    #   shiny::column(
    #     6, shiny::radioButtons(ns("dep_otros"),
    #                            "\u00bfDepurar Cheques Remplazados y Reingresos Vs?",
    #                            choices = c("SI", "NO"), selected = "SI")
    #          ),
    #   shiny::column(
    #     6, shiny::radioButtons(ns("dep_cert_neg"),
    #                            "\u00bfDepurar Cheques endosados a favor de INVICO (Cert. Neg.)?",
    #                            choices = c("SI", "NO"), selected = "SI")
    #     )
    #   )
  )
}

#' 02_02_08_gto_vs_sscc Server Functions
#'
#' @noRd
mod_02_02_08_sgf_vs_sscc_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(sgf_resumen_rend_prov(),
           sscc_banco_invico())
    })

    observeEvent(to_listen(), {

      r6_sgf <- MyData$new(sql_path("sgf"))
      r6_sscc <- MyData$new(sql_path("sscc"))

      r6_sgf$data <- map_cta_cte("sgf",
                                  "SELECT DISTINCT cta_cte FROM resumen_rend_prov",
                                  "sgf_cta_cte")

      r6_sscc$data <-  map_cta_cte("sscc",
                              "SELECT DISTINCT cta_cte FROM banco_invico",
                              "sscc_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_sgf$data, r6_sscc$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      r6_sgf$
        get_query("SELECT DISTINCT ejercicio FROM resumen_rend_prov")

      choices_rv$ejercicio <- sort(r6_sgf$data$ejercicio,
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_sgf$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM resumen_rend_prov")
      )

      r6_sscc$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM banco_invico")
      )

      r6_sgf$bind_rows(r6_sscc$data)

      choices_rv$fecha <- c(
        r6_sgf$data$max_fecha, r6_sgf$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")


      shiny::updateDateRangeInput(session, "fecha",
                                  min = min(choices_rv$fecha),
                                  max = max(choices_rv$fecha))

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })


    #Generate Table
    table <- shiny::eventReactive(input$update, {

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
        unique(choices_rv$cta_cte)

      #Filtering siif_gtos
      r6_sgf$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, cta_cte, ",
                 "importe_neto, beneficiario, movimiento, ",
                 "libramiento_sgf as libramiento ",
                 "FROM resumen_rend_prov ",
                 "WHERE ejercicio = ?"),
          params = list(ejercicio_vec)
        )$
        mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sgf_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )$
        filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_sgf$filter(
          dplyr::between(.data$fecha,
                         lubridate::ymd(input$fecha[[1]]),
                         lubridate::ymd(input$fecha[[2]]))
        )
      }

      #Eliminamos registros duplicados (Origen EPAM y OBRAS)
      r6_sgf$remove_duplicates()

      #Grouping and summarising sgf
      r6_sgf$
        select(input$grupo %||% "mes", .data$importe_neto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(neto_sgf = sum(.data$importe_neto, na.rm = TRUE))

      #Filtering sscc_banco_invico
      r6_sscc$
        get_query(
          paste0("SELECT ejercicio, mes, fecha, cta_cte, codigo_imputacion, ",
                 "monto, beneficiario, libramiento FROM banco_invico ",
                 "WHERE movimiento <> 'DEPOSITO' ",
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

      # if (input$dep_transf_int == "SI") {
      #   r6_sscc$filter(
      #       .data$codigo_imputacion != 34 &
      #         .data$codigo_imputacion != 4
      #       )
      # }
      #
      # if (input$dep_pf == "SI") {
      #   r6_sscc$filter(
      #       .data$codigo_imputacion != 214 &
      #         .data$codigo_imputacion != 215
      #       )
      # }
      #
      # if (input$dep_otros == "SI") {
      #   r6_sscc$filter(
      #       .data$codigo_imputacion != 3 &
      #         .data$codigo_imputacion != 55 &
      #         .data$codigo_imputacion != 5 &
      #         .data$codigo_imputacion != 13
      #       )
      # }
      #
      # if (input$dep_cert_neg == "SI") {
      #   r6_sscc$filter(
      #     .data$codigo_imputacion != 18
      #   )
      # }

      #Grouping and summarising sscc
      r6_sscc$
        select(input$grupo %||% "mes", .data$monto)$
        group_by(!!! rlang::syms(input$grupo %||% "mes"))$
        summarise(debitos_sscc = sum(.data$monto, na.rm = TRUE))

      #Joinning and calulating
      r6_sgf$
        full_join(r6_sscc$data, by = input$grupo %||% "mes")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          diferencia = .data$neto_sgf - .data$debitos_sscc,
          dif_acum = cumsum(.data$diferencia)
        )

      total_desvio <- sum(abs(r6_sgf$data$diferencia))
      r6_sgf$mutate(prop_desv = (abs(.data$diferencia) / total_desvio))

      return(r6_sgf$data)

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_02_08_gto_vs_sscc_ui("02_02_08_gto_vs_sscc_ui_1")

## To be copied in the server
# mod_02_02_08_gto_vs_sscc_server("02_02_08_gto_vs_sscc_ui_1")
