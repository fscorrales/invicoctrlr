#' 02_03_02_metodo_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_03_02_metodo_2_ui <- function(id){

  ns <- NS(id)

  tagList(
    shiny::column(12, align = "center",
                  bs4Dash::actionButton(ns("update"),
                                        "Actualizar Filtros",
                                        status = "primary")),
    shiny::fluidRow(
      shiny::column(
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "mes", "fuente","cta_cte"),
                                     selected = "cta_cte" , inline = FALSE)
        ),
      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio Remanente",
                                 choices = "", selected = "", multiple = FALSE),
        suppressWarnings(
          shiny::dateRangeInput(ns("fecha"), "Seleccionar Fecha", start = NA,
                                end = NA, format = "dd-mm-yyyy",
                                startview = "month", language = "es", separator = " a ")
        )

        )
      ),
    shiny::fluidRow(
      shiny::column(
        6, shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                                 choices = "", selected = "", multiple = TRUE,
                                 options = list(placeholder = "Todo seleccionado"))
      ),
      shiny::column(
        6, shiny::selectizeInput(ns("fuente"), "Seleccionar Fuentes",
                                 choices = "", selected = "", multiple = TRUE,
                                 options = list(placeholder = "Todo seleccionado"))
      )
      )
  )
}

#' 02_03_02_metodo_2 Server Functions
#'
#' @noRd
mod_02_03_02_metodo_2_server <- function(id){
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

    db_gto <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- siif_comprobantes_gtos_rcg01_uejp() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$siif_gastos_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE),
                      mes = stringr::str_c(stringr::str_pad(lubridate::month(fecha),
                                                            2, pad = "0"),
                                           lubridate::year(fecha), sep = "/")
        )

      return(db)
    })

    #Updting shiny input objets
    ejercicio_var <- reactive({

      ans <- db_rec() %>%
        dplyr::select(ejercicio, mes, fecha, cta_cte, fuente)

      ans <- db_gto() %>%
        dplyr::select(ejercicio, mes, fecha, cta_cte, fuente) %>%
        dplyr::full_join(ans,
                         by = c("ejercicio", "mes", "fuente",
                                "fecha", "cta_cte")) %>%
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
      shiny::updateSelectizeInput(session, "fuente",
                                  choices = sort(unique(ejercicio_var()$fuente)))


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

      #Joining rec y gto
      rec_and_gto <- db_gto() %>%
        dplyr::select(ejercicio, fecha, mes, fuente, cta_cte, gasto = monto) %>%
        dplyr::bind_rows(dplyr::select(db_rec(), ejercicio, fecha, mes,
                                       fuente, cta_cte, recurso = monto))

      rec_and_gto <- rec_and_gto %>%
        dplyr::filter(ejercicio %in% (input$ejercicio %||%
                                        max(as.integer(ejercicio_var()$ejercicio))),
                      cta_cte %in% (input$cta_cte %||%
                                      unique(ejercicio_var()$cta_cte)),
                      fuente %in% (input$fuente %||%
                                      unique(ejercicio_var()$fuente))
                      )

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        rec_and_gto <- rec_and_gto %>%
          dplyr::filter(dplyr::between(fecha,
                                       lubridate::ymd(input$fecha[[1]]),
                                       lubridate::ymd(input$fecha[[2]])))
      }

      #Grouping and summarising
      db <- rec_and_gto %>%
        dplyr::select(input$grupo %||% "cta_cte", recurso, gasto) %>%
        dplyr::group_by(!!! rlang::syms(input$grupo %||% "cta_cte")) %>%
        dplyr::summarise(recursos = sum(recurso, na.rm = TRUE),
                         gastos = sum(gasto, na.rm = TRUE),
                         remanente = recursos - gastos) %>%
        tidyr::replace_na(list(recursos = 0, gastos = 0,
                               remanente = 0))

      return(db)

    }, ignoreNULL = FALSE)

    return(table)

  })
}

## To be copied in the UI
# mod_02_03_02_metodo_2_ui("02_03_02_metodo_2_1")

## To be copied in the server
# mod_02_03_02_metodo_2_server("02_03_02_metodo_2_1")