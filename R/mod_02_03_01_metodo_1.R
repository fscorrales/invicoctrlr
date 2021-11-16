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

#' 02_01_03_rec_vs_invico Server Functions
#'
#' @noRd
mod_02_03_01_metodo_1_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(sscc_banco_invico(),
           siif_deuda_flotante_rdeu012())
    })

    observeEvent(to_listen(), {

      r6_sscc <- MyData$new(sql_path("sscc"))
      r6_siif <- MyData$new(sql_path("siif"))

      r6_sscc$data <-  map_cta_cte("sscc",
                                   "SELECT DISTINCT cta_cte FROM banco_invico",
                                   "sscc_cta_cte")

      r6_siif$data <- map_cta_cte("siif",
                                  "SELECT DISTINCT cta_cte FROM deuda_flotante_rdeu012",
                                  "siif_contabilidad_cta_cte")

      choices_rv$cta_cte <- sort(unique(c(r6_siif$data, r6_sscc$data)))

      shiny::updateSelectizeInput(session, "cta_cte",
                                  choices = choices_rv$cta_cte)

      r6_sscc$get_query("SELECT DISTINCT ejercicio FROM banco_invico")

      r6_siif$
        get_query("SELECT DISTINCT fecha_hasta FROM deuda_flotante_rdeu012")$
        mutate(fecha = as.Date(.data$fecha_hasta, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      choices_rv$ejercicio <- sort(unique(c(r6_siif$data$ejercicio,
                                            r6_sscc$data$ejercicio)),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_sscc$get_query(
        paste0("SELECT MAX(fecha) as max_fecha, ",
               "MIN(fecha) as min_fecha ",
               "FROM banco_invico")
      )

      r6_siif$get_query(
        paste0("SELECT MAX(fecha_hasta) as max_fecha, ",
               "MIN(fecha_hasta) as min_fecha ",
               "FROM deuda_flotante_rdeu012")
      )

      r6_siif$bind_rows(r6_sscc$data)

      choices_rv$fecha <- c(
        r6_siif$data$max_fecha, r6_siif$data$min_fecha
      ) %>% as.Date(origin = "1970-01-01")


      shiny::updateDateInput(session, "fecha",
                             min = min(choices_rv$fecha),
                             max = max(choices_rv$fecha))

      r6_siif$finalize()
      r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_siif <- MyData$new(sql_path("siif"))
      r6_sscc <- MyData$new(sql_path("sscc"))

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
      # ejercicio_vec <- input$ejercicio %||%
      #   as.character(max(as.integer(choices_rv$ejercicio)))

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

      r6_siif$
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
      r6_siif$
        select(input$grupo %||% "cta_cte", .data$saldo)$
        group_by(!!! rlang::syms(input$grupo %||% "cta_cte"))$
        summarise(deuda_flotante = sum(.data$saldo, na.rm = TRUE))

      #Joinning and calulating
      r6_sscc$
        full_join(r6_siif$data, by = input$grupo %||% "cta_cte")$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          remanente = .data$saldo_banco - .data$deuda_flotante
          )

      return(r6_sscc$data)

      r6_siif$finalize()
      r6_sscc$finalize()

    })

    return(table)


  })
}

## To be copied in the UI
# mod_02_03_01_metodo_1_ui("02_03_01_metodo_1_1")

## To be copied in the server
# mod_02_03_01_metodo_1_server("02_03_01_metodo_1_1")
