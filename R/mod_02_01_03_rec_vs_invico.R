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
    shiny::fluidRow(
      shiny::column(
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio",
                                                 "fecha", "mes","cta_cte"),
                                     selected = "mes" , inline = FALSE)
        ),
      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio",
                                 choices = "", selected = "", multiple = TRUE,
                                 options = list(placeholder = "Todo seleccionado")),
        suppressWarnings(
          shiny::dateRangeInput(ns("fecha"), "Seleccionar Fecha", start = NA,
                                end = NA, format = "dd-mm-yyyy",
                                startview = "month", language = "es", separator = " a ")
        )

        )
      ),
    shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                          choices = "", selected = "", multiple = TRUE,
                          options = list(placeholder = "Todo seleccionado")),
    shiny::fluidRow(
      shiny::column(
        6, shiny::radioButtons(ns("dep_aju_siif"),
                               "¿Depurar Asientos AJU de la Contabilidad?",
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

    #Initial DBs setting
    db_rec <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- siif_comprobantes_rec_rci02() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$siif_recursos_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE)
                      )
      return(db)
    })

    db_cont <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db_banco <- siif_mayor_contable_rcocc31() %>%
        dplyr::filter(cta_contable == "1112-2-6",
                      tipo_comprobante != "APE") %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(auxiliar_1,
                                                from = db_cta_cte$siif_contabilidad_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE),
                      mes = stringr::str_c(stringr::str_pad(lubridate::month(fecha), 2, pad = "0"),
                                           lubridate::year(fecha), sep = "/")) %>%
        dplyr::select(-auxiliar_1, -auxiliar_2, -cta_contable)

      db <- siif_mayor_contable_rcocc31() %>%
        dplyr::filter(cta_contable == "2122-1-2",
                      auxiliar_1 == "337",
                      tipo_comprobante != "APE") %>%
        dplyr::mutate(mes = stringr::str_c(stringr::str_pad(lubridate::month(fecha), 2, pad = "0"),
                                           lubridate::year(fecha), sep = "/")) %>%
        dplyr::select(-auxiliar_2, -cta_contable) %>%
        dplyr::rename(cod_ret = auxiliar_1) %>%
        dplyr::left_join(dplyr::select(db_banco, cta_cte, ejercicio, nro_entrada),
                         by = c("nro_entrada", "ejercicio"))

      return(db)

    })

    #Updting shiny input objets
    ejercicio_var <- reactive({

      ans <- db_rec() %>%
        dplyr::select(ejercicio, fecha, cta_cte)

      ans <- db_cont() %>%
        dplyr::select(ejercicio, fecha, cta_cte) %>%
        dplyr::full_join(ans,
                         by = c("ejercicio", "fecha", "cta_cte")) %>%
        unique() %>%
        dplyr::arrange(desc(ejercicio), fecha)

      return(ans)

    })

    observeEvent(ejercicio_var, {

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = unique(ejercicio_var()$ejercicio))
      shiny::updateDateRangeInput(session, "fecha",
                                  min = min(ejercicio_var()$fecha),
                                  max = max(ejercicio_var()$fecha))
      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = sort(unique(ejercicio_var()$cta_cte)))

    })

    #Generate Table
    table <- eventReactive(input$update, {

      #Setting input$ejercicio default value
      if (is.null(input$ejercicio)) {
        shiny::updateSelectizeInput(session, "ejercicio",
                                    selected = max(as.integer(ejercicio_var()$ejercicio)))
      }

      if (is.null(input$grupo)) {
        shiny::updateCheckboxGroupInput(session, "grupo",
                                        selected = "mes")
      }

      #Filtering comp_rec_siif
      siif_rec <- db_rec() %>%
        dplyr::filter(ejercicio %in% (input$ejercicio %||%
                                        max(as.integer(ejercicio_var()$ejercicio))),
                      cta_cte %in% (input$cta_cte %||%
                                      unique(ejercicio_var()$cta_cte)),
                      # remanente == FALSE,
                      invico == TRUE)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        siif_rec <- siif_rec %>%
          dplyr::filter(dplyr::between(fecha,
                                       lubridate::ymd(input$fecha[[1]]),
                                       lubridate::ymd(input$fecha[[2]])))
      }

      #Grouping and summarising siif
      siif_rec <- siif_rec %>%
        dplyr::select(input$grupo %||% "mes", monto) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "mes")) %>%
        dplyr::summarise(recursos_siif = sum(monto, na.rm = TRUE))

      #Filtering siif_banco_invico
      siif_cont <- db_cont() %>%
        dplyr::filter(ejercicio %in% (input$ejercicio %||%
                                 max(as.integer(ejercicio_var()$ejercicio))),
                      cta_cte %in% (input$cta_cte %||%
                               unique(ejercicio_var()$cta_cte)))

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        siif_cont <- siif_cont %>%
          dplyr::filter(dplyr::between(fecha,
                                       lubridate::ymd(input$fecha[[1]]),
                                       lubridate::ymd(input$fecha[[2]])))
      }

      if (input$dep_aju_siif == "SI") {
        siif_cont <- siif_cont %>%
          dplyr::filter(tipo_comprobante != "AJU")
      }

      #Grouping and summarising siif
      siif_cont <- siif_cont %>%
        dplyr::select(input$grupo %||% "mes", creditos, debitos) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "mes")) %>%
        dplyr::summarise(gastos_337_siif = sum(creditos, na.rm = TRUE),
                         pagos_337_siif = sum(debitos, na.rm = TRUE)) %>%
        dplyr::mutate(dif_pagado_337 = gastos_337_siif - pagos_337_siif)

      #Joinning and calulating
      db <- siif_rec %>%
        dplyr::full_join(siif_cont, by = input$grupo %||% "mes") %>%
        tidyr::replace_na(list(recursos_siif = 0, gastos_337_siif = 0,
                               pagos_337_siif = 0, dif_pagado_337 = 0)) %>%
        dplyr::mutate(dif_ingresado = recursos_siif - pagos_337_siif,
                      dif_acum = cumsum(dif_ingresado))

      total_desvio <- sum(abs(db$dif_ingresado))

      db <- db %>%
        dplyr::mutate(prop_desv = (abs(dif_ingresado) / total_desvio))

      return(db)

    }, ignoreNULL = FALSE)

    return(table)


  })
}

## To be copied in the UI
# mod_02_01_03_rec_vs_invico_ui("02_01_03_rec_vs_invico_ui_1")

## To be copied in the server
# mod_02_01_03_rec_vs_invico_server("02_01_03_rec_vs_invico_ui_1")