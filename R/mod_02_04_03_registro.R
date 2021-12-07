#' 02_04_03_registro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_04_03_registro_ui <- function(id){

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
        6,
        ## filtro por cuenta??
        # shiny::checkboxGroupInput(ns("grupo"), "Agrupamiento del Reporte",
        #                              choices = c("ejercicio", "estructura", "partida", "fuente"),
        #                              selected = c("ejercicio", "estructura", "partida", "fuente"),
        #                              inline = FALSE),
        #
        # rep_br(2),

        checkboxInput(ns("mostrar"),
                      "Mostrar registros con al menos 1 diferencia", value = TRUE)

      )
      )
  )
}

#' 02_04_03_registro Server Functions
#'
#' @noRd
mod_02_04_03_registro_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT ejercicio, fecha, nro_entrada, partida, monto ",
      "FROM comprobantes_gtos_gpo_partida_gto_rpa03g) G LEFT JOIN " ,
      "(SELECT ejercicio, nro_entrada, fuente, cuit, cta_cte ",
      "FROM comprobantes_gtos_rcg01_uejp) C ",
      "ON (G.ejercicio = C.ejercicio ",
      "AND G.nro_entrada = C.nro_entrada)"
    )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(icaro_carga(),
           siif_comprobantes_gtos_rcg01_uejp(),
           siif_comprobantes_gtos_gpo_partida_gto_rpa03g())
    })

    observeEvent(to_listen(), {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_siif <- MyData$new(sql_path("siif"))

      r6_icaro$
        get_query("SELECT DISTINCT fecha FROM carga")$
        mutate(fecha = as.Date(.data$fecha, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      r6_siif$
        get_query(paste0("SELECT DISTINCT G.ejercicio FROM ", sql_join))

      choices_rv$ejercicio <- sort(c(unique(r6_icaro$data$ejercicio),
                                     r6_siif$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_icaro$get_query("SELECT DISTINCT partida FROM carga")

      r6_siif$
        get_query(paste0("SELECT DISTINCT G.partida FROM ", sql_join))

      choices_rv$partida <- unique(sort(
        c(r6_icaro$data$partida,
          r6_siif$data$partida)
        ))


      shiny::updateSelectizeInput(session, "partida",
                                  choices = choices_rv$partida,
                                  selected = c("421", "422"))

      r6_icaro$get_query("SELECT DISTINCT fuente FROM carga")

      r6_siif$
        get_query(paste0("SELECT DISTINCT C.fuente FROM ", sql_join))

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

      if (is.null(input$partida)) {
        shiny::updateSelectizeInput(session, "partida",
                                    selected = c("421", "422"))
      }

      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))

      partida_vec <- input$partida %||%
        c("421", "422")

      fuente_vec <- input$fuente %||%
        unique(choices_rv$fuente)

      #Filtering icaro
      r6_icaro$
        get_query(
          paste0("SELECT fecha, nro_entrada, partida, fuente, ",
                 "cuit, cta_cte, tipo, importe FROM carga ",
                 "WHERE partida = ? "),
          params = list(partida_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          ejercicio = as.character(lubridate::year(.data$fecha)),
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$icaro_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE)
        )$
        rename(
          cta_cte_icaro = .data$cta_cte,
          fuente_icaro = .data$fuente,
          cuit_icaro = .data$cuit,
          fecha_icaro = .data$fecha,
          nro_entrada_icaro = .data$nro_entrada,
          partida_icaro = .data$partida,
          monto_icaro = .data$importe
        )$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$tipo != "PA6",
               .data$fuente_icaro %in% fuente_vec)$
        select(-.data$ejercicio, -.data$tipo)

      #Filtering and grouping siif
      r6_siif$
        get_query(
          paste0("SELECT G.fecha, C.cta_cte, ",
                 "G.nro_entrada, G.partida, G.monto, ",
                 "C.cuit, C.fuente FROM ", sql_join, " ",
                 "WHERE partida = ? ",
                 "AND G.ejercicio = '", ejercicio_vec, "'"),
          params = list(partida_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          nro_entrada = sprintf("%05d", as.numeric(.data$nro_entrada)),
          nro_entrada = stringr::str_c(.data$nro_entrada,
                                       format(.data$fecha, format="%y"),
                                       sep="/"),
          cta_cte = map_values(.data$cta_cte,
                               from = primary_key_cta_cte()$siif_gastos_cta_cte,
                               to = primary_key_cta_cte()$map_to,
                               warn_missing = FALSE)
        )$
        rename(
          cta_cte_siif = .data$cta_cte,
          fuente_siif = .data$fuente,
          cuit_siif = .data$cuit,
          fecha_siif = .data$fecha,
          nro_entrada_siif = .data$nro_entrada,
          partida_siif = .data$partida,
          monto_siif = .data$monto
        )$
        filter(.data$fuente_siif %in% fuente_vec)

      #Joinning and calulating
      r6_siif$
        full_join(r6_icaro$data,
                  by = c("nro_entrada_siif" = "nro_entrada_icaro"),
                  keep = TRUE)$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          dif_nro_entrada = ifelse((.data$nro_entrada_siif == .data$nro_entrada_icaro), TRUE, FALSE),
          dif_nro_entrada = ifelse(is.na(.data$dif_nro_entrada), FALSE, .data$dif_nro_entrada),
          dif_fecha = ifelse((.data$fecha_siif == .data$fecha_icaro), TRUE, FALSE),
          dif_fecha = ifelse(is.na(.data$dif_fecha), FALSE, .data$dif_fecha),
          dif_partida = ifelse((.data$partida_siif == .data$partida_icaro), TRUE, FALSE),
          dif_partida = ifelse(is.na(.data$dif_partida), FALSE, .data$dif_partida),
          dif_fuente = ifelse((.data$fuente_siif == .data$fuente_icaro), TRUE, FALSE),
          dif_fuente = ifelse(is.na(.data$dif_fuente), FALSE, .data$dif_fuente),
          dif_monto = ifelse(dplyr::near(.data$monto_siif , .data$monto_icaro), TRUE, FALSE),
          dif_monto = ifelse(is.na(.data$dif_monto), FALSE, .data$dif_monto),
          dif_cta_cte = ifelse((.data$cta_cte_siif  == .data$cta_cte_icaro), TRUE, FALSE),
          dif_cta_cte = ifelse(is.na(.data$dif_cta_cte), FALSE, .data$dif_cta_cte),
          dif_cuit = ifelse((.data$cuit_siif == .data$cuit_icaro), TRUE, FALSE),
          dif_cuit = ifelse(is.na(.data$dif_cuit), FALSE, .data$dif_cuit)
        )$
        select(
          .data$fuente_siif, .data$fuente_icaro, .data$dif_fuente,
          .data$monto_siif, .data$monto_icaro, .data$dif_monto,
          .data$cta_cte_siif, .data$cta_cte_icaro, .data$dif_cta_cte,
          .data$cuit_siif, .data$cuit_icaro, .data$dif_cuit,
          .data$nro_entrada_siif, .data$nro_entrada_icaro, .data$dif_nro_entrada,
          .data$fecha_siif, .data$fecha_icaro, .data$dif_fecha,
          .data$partida_siif, .data$partida_icaro, .data$dif_partida
        )
        # mutate_if(is.logical, replace_logical_symbol)
        # dif_nro_entrada = ifelse(dif_nro_entrada > 0, "\u2713", "\u2718")

      if (input$mostrar) {
        r6_siif$
          filter(
            (.data$dif_fuente + .data$dif_monto + .data$dif_cta_cte + .data$dif_cuit +
               .data$dif_nro_entrada + .data$dif_fecha + .data$dif_partida) < 7
          )
      }

      r6_siif$mutate(
        dplyr::across(
          dplyr::starts_with("dif"),
          ~ ifelse(.x > 0, "\u2713", "\u2718")
        )
      )

      return(r6_siif$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_04_03_registro_ui("02_04_03_registro_1")

## To be copied in the server
# mod_02_04_03_registro_server("02_04_03_registro_1")
