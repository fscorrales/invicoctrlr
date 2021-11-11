#' 02_01_01_rec_vs_sscc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_01_01_rec_vs_sscc_ui <- function(id){

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
                                     choices = c("ejercicio",
                                                 "fecha", "mes","cta_cte", "grupo"),
                                     selected = "mes" , inline = FALSE)
        )

      ),
    shiny::fluidRow(
      shiny::column(
        6, shiny::radioButtons(ns("dep_transf_int"),
                               "\u00bfDepurar Tranf. Internas?",
                               choices = c("SI", "NO"), selected = "SI")
        ),
      shiny::column(
        6, shiny::radioButtons(ns("dep_pf"),
                               "\u00bfDepurar Inversiones en PF?",
                               choices = c("SI", "NO"), selected = "SI")
        )
      ),
    shiny::fluidRow(
      shiny::column(
        6, shiny::radioButtons(ns("dep_otros"),
                               "\u00bfDepurar Cheques Remplazados y Reingresos Vs?",
                               choices = c("SI", "NO"), selected = "SI")
             ),
      shiny::column(
        6, shiny::radioButtons(ns("dep_cert_neg"),
                               "\u00bfDepurar Cheques endosados a favor de INVICO (Cert. Neg.)?",
                               choices = c("SI", "NO"), selected = "SI")
        )
      )
  )
}

#' 02_01_01_rec_vs_sscc Server Functions
#'
#' @noRd
mod_02_01_01_rec_vs_sscc_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    # #Initial DBs setting
    # db_rec <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db <- siif_comprobantes_rec_rci02() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$siif_recursos_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE)
    #                   # grupo = dplyr::case_when(
    #                   #   .data$cta_cte == "10270" ~ "FONAVI",
    #                   #   .data$cta_cte %in% c("130832-12", "334", "Macro", "Patagonia") ~ "RECUPEROS",
    #                   #   TRUE ~ "OTROS"
    #                   # )
    #                   )
    #   return(db)
    # })
    #
    # db_sscc <- reactive({
    #
    #   db_cta_cte <- primary_key_cta_cte()
    #   db <- sscc_banco_invico() %>%
    #     dplyr::mutate(cta_cte = map_values(.data$cta_cte,
    #                                             from = db_cta_cte$sscc_cta_cte,
    #                                             to = db_cta_cte$map_to,
    #                                             warn_missing = FALSE),
    #                   ejercicio = as.character(lubridate::year(.data$fecha))
    #                   # grupo = dplyr::case_when(
    #                   #   .data$cta_cte == "10270" ~ "FONAVI",
    #                   #   .data$cta_cte %in% c("130832-12", "334", "Macro", "Patagonia") ~ "RECUPEROS",
    #                   #   TRUE ~ "OTROS"
    #                   # )
    #                   )
    #   return(db)
    #
    # })

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(siif_comprobantes_rec_rci02(),
           sscc_banco_invico())
    })

    observeEvent(to_listen(), {

      db_rec <-  map_cta_cte("siif",
                             "SELECT DISTINCT cta_cte FROM comprobantes_rec_rci02",
                             "siif_recursos_cta_cte")

      db_sscc <-  map_cta_cte("sscc",
                             "SELECT DISTINCT cta_cte FROM banco_invico",
                             "sscc_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(db_rec, db_sscc)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      db_rec <- invicodatr::filter_sqlite(
        "siif",
        "SELECT DISTINCT Ejercicio FROM comprobantes_rec_rci02"
        )

      choices_rv$ejercicio <- sort(db_rec$ejercicio,
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      db_rec <- invicodatr::filter_sqlite(
        "siif",
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM comprobantes_rec_rci02")
      )

      db_sscc <- invicodatr::filter_sqlite(
        "sscc",
        paste0("SELECT MAX(fecha) as max_fecha, MIN(fecha) as min_fecha ",
               "FROM banco_invico")
      )

      db <- db_rec %>%
        dplyr::bind_rows(db_sscc)

      db <- c(db$max_fecha, db$min_fecha) %>%
        as.Date(origin = "1970-01-01")

      choices_rv$fecha <- db

      shiny::updateDateRangeInput(session, "fecha",
                                  min = min(choices_rv$fecha),
                                  max = max(choices_rv$fecha))
    })


    #Generate Table
    table <- reactiveVal(data.frame())

    observeEvent(input$update, {

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
      siif <- invicodatr::filter_sqlite(
        "siif",
        paste0("SELECT ejercicio, mes, fecha, cta_cte, ",
               "monto FROM comprobantes_rec_rci02 ",
               "WHERE invico = 0 ",
               "AND remanente = 0 ",
               "AND ejercicio = ?"),
        params = list(ejercicio_vec)
      ) %>%
        dplyr::mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_recursos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        )%>%
        dplyr::filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        db <- db %>%
          dplyr::filter(dplyr::between(.data$fecha,
                                       lubridate::ymd(input$fecha[[1]]),
                                       lubridate::ymd(input$fecha[[2]])))
      }

      #Grouping and summarising siif
      siif <- siif %>%
        dplyr::select(input$grupo %||% "mes", .data$monto) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "mes")) %>%
        dplyr::summarise(recursos_siif = sum(.data$monto, na.rm = TRUE))

      #Filtering sscc_banco_invico
      sscc <- invicodatr::filter_sqlite(
        "sscc",
        paste0("SELECT ejercicio, mes, fecha, cta_cte, codigo_imputacion, ",
               "monto FROM banco_invico ",
               "WHERE movimiento = 'DEPOSITO' ",
               "AND ejercicio = ?"),
        params = list(ejercicio_vec)
      ) %>%
        dplyr::mutate(
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$sscc_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE),
          fecha = as.Date(.data$fecha, origin = "1970-01-01")
        ) %>%
        dplyr::filter(.data$cta_cte %in% cta_cte_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        db <- db %>%
          dplyr::filter(dplyr::between(.data$fecha,
                                       lubridate::ymd(input$fecha[[1]]),
                                       lubridate::ymd(input$fecha[[2]])))
      }


      if (input$dep_transf_int == "SI") {
        sscc <- sscc %>%
          dplyr::filter(.data$codigo_imputacion != 34 &
                          .data$codigo_imputacion != 4)
      }

      if (input$dep_pf == "SI") {
        sscc <- sscc %>%
          dplyr::filter(.data$codigo_imputacion != 214 &
                          .data$codigo_imputacion != 215)
      }

      if (input$dep_otros == "SI") {
        sscc <- sscc %>%
          dplyr::filter(.data$codigo_imputacion != 3 &
                          .data$codigo_imputacion != 55 &
                          .data$codigo_imputacion != 5 &
                          .data$codigo_imputacion != 13)
      }

      if (input$dep_cert_neg == "SI") {
        sscc <- sscc %>%
          dplyr::filter(.data$codigo_imputacion != 18)
      }

      #Grouping and summarising sscc
      sscc <- sscc %>%
        dplyr::select(input$grupo %||% "mes", .data$monto) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "mes")) %>%
        dplyr::summarise(depositos_sscc = sum(.data$monto, na.rm = TRUE))

      #Joinning and calulating
      db <- siif %>%
        dplyr::full_join(sscc, by = input$grupo %||% "mes") %>%
        replace(., is.na(.), 0) %>%
        # tidyr::replace_na(list(recursos_siif = 0, depositos_sscc = 0)) %>%
        dplyr::mutate(diferencia = .data$recursos_siif - .data$depositos_sscc,
                      dif_acum = cumsum(.data$diferencia))

      total_desvio <- sum(abs(db$diferencia))

      db <- db %>%
        dplyr::mutate(prop_desv = (abs(.data$diferencia) / total_desvio))

      table(db)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_01_01_rec_vs_sscc_ui("02_01_01_rec_vs_sscc_ui_1")

## To be copied in the server
# mod_02_01_01_rec_vs_sscc_server("02_01_01_rec_vs_sscc_ui_1")
