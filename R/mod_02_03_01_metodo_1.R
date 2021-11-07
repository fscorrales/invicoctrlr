#' 02_03_01_metodo_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_03_01_metodo_1_ui <- function(id){

  ns <- NS(id)

  tagList(
    shiny::column(12, align = "center",
                  bs4Dash::actionButton(ns("update"),
                                        "Actualizar Filtros",
                                        status = "primary")),
    shiny::fluidRow(
      shiny::column(
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "mes","cta_cte"),
                                     selected = "cta_cte" , inline = FALSE)
        ),
      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio Hasta",
                                 choices = "", selected = "", multiple = FALSE),
        suppressWarnings(
          shiny::dateInput(ns("fecha"), "Fecha Hasta", format = "dd-mm-yyyy",
                          startview = "month", language = "es", value = NA)
        )

        )
      ),
    shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                          choices = "", selected = "", multiple = TRUE,
                          options = list(placeholder = "Todo seleccionado"))
  )
}

#' 02_01_03_rec_vs_invico Server Functions
#'
#' @noRd
mod_02_03_01_metodo_1_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Initial DBs setting
    db_sscc <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- sscc_banco_invico() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$sscc_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE),
                      ejercicio = as.character(lubridate::year(fecha))
                      )
      return(db)

    })

    db_rdeu <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- siif_deuda_flotante_rdeu012() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$siif_contabilidad_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE),
                      fecha = fecha_hasta,
                      ejercicio = as.character(lubridate::year(fecha)),
                      mes = stringr::str_c(stringr::str_pad(lubridate::month(fecha),
                                                            2, pad = "0"),
                                           lubridate::year(fecha), sep = "/")
        )
        # dplyr::select(-fecha_hasta, mes_hasta)

      return(db)

    })

    #Updting shiny input objets
    ejercicio_var <- reactive({

      ans <- db_sscc() %>%
        dplyr::select(ejercicio, fecha, cta_cte)

      ans <- db_rdeu() %>%
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
      shiny::updateDateInput(session, "fecha",
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
                                        selected = "cta_cte")
      }

      #Filtering sscc
      sscc <- db_sscc() %>%
        dplyr::filter(cta_cte %in% (input$cta_cte %||%
                                      unique(ejercicio_var()$cta_cte))
                      )

      if (length(input$fecha) > 0) {
        sscc <- sscc %>%
          dplyr::filter(dplyr::between(fecha,
                                       lubridate::ymd("2017/01/01"),
                                       lubridate::ymd(input$fecha)))
      } else{
        #Only for the initial loop
        if (input$ejercicio == "") {
          ejercicio_max <- max(as.integer(ejercicio_var()$ejercicio))
        } else {
          ejercicio_max <- as.integer(input$ejercicio)
        }

        sscc <- sscc %>%
          dplyr::filter(dplyr::between(ejercicio, 2017, ejercicio_max))

        }

      #Filtering siif_rdeu
      mes_hasta_rdeu <- dplyr::last(sort(sscc$fecha))
      mes_hasta_rdeu <- stringr::str_c(stringr::str_pad(lubridate::month(mes_hasta_rdeu),
                                                        2, pad = "0"),
                                       lubridate::year(mes_hasta_rdeu), sep = "/")

      rdeu <- db_rdeu() %>%
        dplyr::filter(cta_cte %in% (input$cta_cte %||%
                                      unique(ejercicio_var()$cta_cte)),
                      mes_hasta == mes_hasta_rdeu)

      #Grouping and summarising sscc
      sscc <- sscc %>%
        dplyr::select(input$grupo %||% "cta_cte", monto) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "cta_cte")) %>%
        dplyr::summarise(saldo_banco = sum(monto, na.rm = TRUE))

      #Grouping and summarising siif_rdeu
      rdeu <- rdeu %>%
        dplyr::select(input$grupo %||% "cta_cte", saldo) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "cta_cte")) %>%
        dplyr::summarise(deuda_flotante = sum(saldo, na.rm = TRUE))

      #Joinning and calulating
      db <- sscc %>%
        dplyr::full_join(rdeu, by = input$grupo %||% "mes") %>%
        tidyr::replace_na(list(saldo_banco = 0, deuda_flotante = 0)) %>%
        dplyr::mutate(remanente = saldo_banco - deuda_flotante)

      # total_desvio <- sum(abs(db$dif_ingresado))
      #
      # db <- db %>%
      #   dplyr::mutate(prop_desv = (abs(dif_ingresado) / total_desvio))

      return(db)

    }, ignoreNULL = FALSE)

    return(table)


  })
}

## To be copied in the UI
# mod_02_03_01_metodo_1_ui("02_03_01_metodo_1_1")

## To be copied in the server
# mod_02_03_01_metodo_1_server("02_03_01_metodo_1_1")
