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
                                     choices = c("ejercicio", "estructura", "partida", "fuente"),
                                     selected = c("ejercicio", "estructura", "partida", "fuente"),
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
    # db_icaro <- reactive({
    #
    #   db <- icaro_carga()
    #
    #   return(db)
    # })
    #
    # db_siif <- reactive({
    #
    #   db <- siif_ppto_gtos_fte_rf602()
    #
    #   return(db)
    # })

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(icaro_carga(),
           siif_ppto_gtos_fte_rf602())
    })

    observeEvent(to_listen(), {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_siif <- MyData$new(sql_path("siif"))

      r6_icaro$
        get_query("SELECT DISTINCT fecha FROM carga")$
        mutate(fecha = as.Date(.data$fecha, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      r6_siif$get_query("SELECT DISTINCT ejercicio FROM ppto_gtos_fte_rf602")

      choices_rv$ejercicio <- sort(c(unique(r6_icaro$data$ejercicio),
                                     r6_siif$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_icaro$get_query("SELECT DISTINCT partida FROM carga")

      r6_siif$get_query("SELECT DISTINCT partida FROM ppto_gtos_fte_rf602")

      choices_rv$partida <- unique(sort(
        c(r6_icaro$data$partida,
          r6_siif$data$partida)
        ))


      shiny::updateSelectizeInput(session, "partida",
                                  choices = choices_rv$partida,
                                  selected = c("421", "422"))

      r6_icaro$get_query("SELECT DISTINCT fuente FROM carga")

      r6_siif$get_query("SELECT DISTINCT fuente FROM ppto_gtos_fte_rf602")

      choices_rv$fuente <- unique(sort(
        c(r6_icaro$data$fuente,
          r6_siif$data$fuente)
        ))

      shiny::updateSelectizeInput(session, "fuente",
                                  choices = choices_rv$fuente)


      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_siif <- MyData$new(sql_path("siif"))

      #Setting input$ejercicio default value
      if (is.null(input$ejercicio)) {
        shiny::updateSelectizeInput(session, "ejercicio",
                                    selected = max(as.integer(choices_rv$ejercicio)))
      }

      if (is.null(input$grupo)) {
        shiny::updateCheckboxGroupInput(session, "grupo",
                                        selected = c("ejercicio", "estructura",
                                                     "partida", "fuente"))
      }

      if (is.null(input$partida)) {
        shiny::updateSelectizeInput(session, "partida",
                                    selected = c("421", "422"))
      }
#
#       if (is.null(input$fuente)) {
#         shiny::updateSelectizeInput(session, "fuente",
#                                     selected = sort(unique(ejercicio_var()$ejercicio)))
#       }

      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))

      partida_vec <- input$partida %||%
        c("421", "422")

      fuente_vec <- input$fuente %||%
        unique(choices_rv$fuente)

      grupo_vec <- input$grupo %||%
        c("ejercicio", "estructura",
          "partida", "fuente")

      #Filtering and grouping icaro
      r6_icaro$
        get_query(
          paste0("SELECT fecha, estructura, partida, fuente, ",
                 "tipo, importe AS icaro FROM carga ",
                 "WHERE partida = ? "),
          params = list(partida_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          ejercicio = as.character(lubridate::year(.data$fecha))
        )$
        select(
          -.data$fecha,
        )$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$tipo != "PA6",
               .data$fuente %in% fuente_vec)$
        select(grupo_vec, .data$icaro)$
        group_by(!!! rlang::syms(grupo_vec))$
        summarise(icaro = sum(.data$icaro, na.rm = TRUE))

      #Filtering and grouping siif
      r6_siif$
        get_query(
          paste0("SELECT ejercicio, programa, subprograma, ",
                 "proyecto, actividad, partida, fuente, ",
                 "ordenado AS siif FROM ppto_gtos_fte_rf602 ",
                 "WHERE partida = ? ",
                 "AND ejercicio = '", ejercicio_vec, "'" ),
          params = list(partida_vec)
        )$
        mutate(
          estructura = stringr::str_c(.data$programa, .data$subprograma,
                                      .data$proyecto, .data$actividad, sep = "-")
        )$
        select(
          -.data$programa, -.data$subprograma,
          -.data$proyecto, -.data$actividad
        )$
        filter(.data$fuente %in% fuente_vec)$
        select(grupo_vec, .data$siif)$
        group_by(!!! rlang::syms(grupo_vec))$
        summarise(siif = sum(.data$siif, na.rm = TRUE))

      #Joinning and calulating
      r6_siif$
        full_join(r6_icaro$data, by = grupo_vec)$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          diferencia = .data$siif - .data$icaro,
          # dif_acum = cumsum(.data$diferencia)
        )

      if (input$mostrar) {
        r6_siif$
          filter(!dplyr::near(.data$diferencia, 0))
      }

      return(r6_siif$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_04_01_anual_ui("02_04_01_anual_1")

## To be copied in the server
# mod_02_04_01_anual_server("02_04_01_anual_1")
