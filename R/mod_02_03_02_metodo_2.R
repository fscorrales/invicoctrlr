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

    rep_br(),

    shiny::fluidRow(

      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio Remanente",
                                 choices = "", selected = "", multiple = TRUE),
        suppressWarnings(
          shiny::dateRangeInput(ns("fecha"), "Seleccionar Fecha", start = NA,
                                end = NA, format = "dd-mm-yyyy",
                                startview = "month", language = "es", separator = " a ")
        )

        ),

      shiny::column(
        6, shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio", "mes", "fuente","cta_cte"),
                                     selected = "cta_cte" , inline = FALSE)
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

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(siif_comprobantes_rec_rci02(),
           siif_comprobantes_gtos_rcg01_uejp())
    })

    observeEvent(to_listen(), {

      r6_siif_rec <- MyData$new(sql_path("siif"))
      r6_siif_gto <- MyData$new(sql_path("siif"))

      r6_siif_rec$data <-  map_cta_cte("siif",
                                   "SELECT DISTINCT cta_cte FROM comprobantes_rec_rci02",
                                   "siif_recursos_cta_cte")

      r6_siif_gto$data <- map_cta_cte("siif",
                                  "SELECT DISTINCT cta_cte FROM comprobantes_gtos_rcg01_uejp",
                                  "siif_gastos_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_siif_rec$data,
                                          r6_siif_gto$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      r6_siif_rec$
        get_query("SELECT DISTINCT ejercicio FROM comprobantes_rec_rci02")

      r6_siif_gto$
        get_query("SELECT DISTINCT ejercicio FROM comprobantes_gtos_rcg01_uejp")

      choices_rv$ejercicio <- sort(unique(c(r6_siif_rec$data$ejercicio,
                                            r6_siif_gto$data$ejercicio)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

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

      r6_siif_rec$bind_rows(r6_siif_gto$data)

      choices_rv$fecha <- c(
        r6_siif_rec$data$max_fecha, r6_siif_rec$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")


      shiny::updateDateRangeInput(session, "fecha",
                             min = min(choices_rv$fecha),
                             max = max(choices_rv$fecha))

      r6_siif_rec$
        get_query("SELECT DISTINCT fuente FROM comprobantes_rec_rci02")

      r6_siif_gto$
        get_query("SELECT DISTINCT fuente FROM comprobantes_gtos_rcg01_uejp")

      choices_rv$fuente <- sort(unique(c(r6_siif_rec$data$fuente,
                                            r6_siif_gto$data$fuente)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "fuente",
                                  choices = choices_rv$fuente )

      # r6_siif_rec$finalize()
      # r6_siif_gto$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

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

      fuente_vec <- input$fuente %||%
        unique(choices_rv$fuente)


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
               .data$cta_cte %in% cta_cte_vec,
               .data$fuente %in% fuente_vec)

      if (not_na(input$fecha[[1]]) & not_na(input$fecha[[2]])) {
        r6_siif_rec$
          filter(dplyr::between(.data$fecha,
                                lubridate::ymd(input$fecha[[1]]),
                                lubridate::ymd(input$fecha[[2]])))
      }

      #Grouping and summarising sscc
      r6_siif_rec$
        select(input$grupo %||% "cta_cte", .data$recurso, .data$gasto)$
        group_by(!!! rlang::syms(input$grupo %||% "cta_cte"))$
        summarise(recursos = sum(.data$recurso, na.rm = TRUE),
                  gastos = sum(.data$gasto, na.rm = TRUE),
                  remanente = .data$recursos - .data$gastos)$
        mutate_if(is.numeric, replace_NA_0)

      return(r6_siif_rec$data)

      # r6_siif_rec$finalize()
      # r6_siif_gto$finalize()

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_03_02_metodo_2_ui("02_03_02_metodo_2_1")

## To be copied in the server
# mod_02_03_02_metodo_2_server("02_03_02_metodo_2_1")
