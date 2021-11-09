#' 02_04_01_anual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_04_01_anual_ui <- function(id){

  ns <- NS(id)

  tagList(
    shiny::column(12, align = "center",
                  bs4Dash::actionButton(ns("update"),
                                        "Actualizar Filtros",
                                        status = "primary")),
    rep_br(),

    shiny::fluidRow(
      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio Control",
                                 choices = "", selected = "", multiple = FALSE),

        shiny::selectizeInput(ns("fuente"), "Seleccionar Fuentes",
                                   choices = "", selected = "", multiple = TRUE,
                                   options = list(placeholder = "Todo seleccionado")),

        shiny::selectizeInput(ns("partida"), "Seleccionar Partidas",
                              choices = c("421", "422"), selected = c("421", "422"),
                              multiple = TRUE)
      ),
      shiny::column(
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "imputacion", "partida", "fuente"),
                                     selected = c("ejercicio", "imputacion", "partida", "fuente"),
                                     inline = FALSE),

        rep_br(2),

        checkboxInput(ns("mostrar"),
                      "Mostrar registros con diferencias distintas de 0", value = TRUE)

      )
      )
  )
}

#' 02_03_02_metodo_2 Server Functions
#'
#' @noRd
mod_02_04_01_anual_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Initial DBs setting
    db_icaro <- reactive({

      db <- icaro_carga()

      return(db)
    })

    db_siif <- reactive({

      db <- siif_ppto_gtos_fte_rf602()

      return(db)
    })

    #Updting shiny input objets
    ejercicio_var <- reactive({

      ans <- db_icaro() %>%
        dplyr::select(.data$ejercicio, .data$partida, .data$fuente)

      ans <- db_siif() %>%
        dplyr::select(.data$ejercicio, .data$partida, .data$fuente) %>%
        dplyr::full_join(ans,
                         by = c("ejercicio", "partida", "fuente")) %>%
        unique() %>%
        dplyr::arrange(dplyr::desc(.data$ejercicio))

      return(ans)

    })

    observeEvent(ejercicio_var, {

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = unique(ejercicio_var()$ejercicio))
      shiny::updateSelectizeInput(session, "partida",
                                  choices = sort(unique(ejercicio_var()$partida)),
                                  selected = c("421", "422"))
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

      if (is.null(input$partida)) {
        shiny::updateSelectizeInput(session, "partida",
                                        selected = c("421", "422"))
      }

      if (is.null(input$fuente)) {
        shiny::updateSelectizeInput(session, "fuente",
                                    selected = sort(unique(ejercicio_var()$ejercicio)))
      }

      if (is.null(input$grupo)) {
        shiny::updateCheckboxGroupInput(session, "grupo",
                                        selected = c("ejercicio", "imputacion",
                                                     "partida", "fuente"))
      }

    #   #Joining rec y gto
    #   rec_and_gto <- db_gto() %>%
    #     dplyr::select(.data$ejercicio, .data$fecha, .data$mes, .data$fuente,
    #                   .data$cta_cte, gasto = .data$monto) %>%
    #     dplyr::bind_rows(dplyr::select(db_rec(), .data$ejercicio, .data$fecha, .data$mes,
    #                                    .data$fuente, .data$cta_cte, recurso = .data$monto))
    #
    #   rec_and_gto <- rec_and_gto %>%
    #     dplyr::filter(.data$ejercicio %in% (input$ejercicio %||%
    #                                     max(as.integer(ejercicio_var()$ejercicio))),
    #                   .data$cta_cte %in% (input$cta_cte %||%
    #                                   unique(ejercicio_var()$cta_cte)),
    #                   .data$fuente %in% (input$fuente %||%
    #                                   unique(ejercicio_var()$fuente))
    #                   )
    #
    #   if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
    #     rec_and_gto <- rec_and_gto %>%
    #       dplyr::filter(dplyr::between(.data$fecha,
    #                                    lubridate::ymd(input$fecha[[1]]),
    #                                    lubridate::ymd(input$fecha[[2]])))
    #   }
    #
    #   #Grouping and summarising
    #   db <- rec_and_gto %>%
    #     dplyr::select(input$grupo %||% "cta_cte", .data$recurso, .data$gasto) %>%
    #     dplyr::group_by(!!! rlang::syms(input$grupo %||% "cta_cte")) %>%
    #     dplyr::summarise(recursos = sum(.data$recurso, na.rm = TRUE),
    #                      gastos = sum(.data$gasto, na.rm = TRUE),
    #                      remanente = .data$recursos - .data$gastos) %>%
    #     replace(., is.na(.), 0)
    #     # tidyr::replace_na(list(recursos = 0, gastos = 0,
    #     #                        remanente = 0))
    #
    #   return(db)

    }, ignoreNULL = FALSE)
    #
    # return(table)

  })
}

## To be copied in the UI
# mod_02_04_01_anual_ui("02_04_01_anual_1")

## To be copied in the server
# mod_02_04_01_anual_server("02_04_01_anual_1")
