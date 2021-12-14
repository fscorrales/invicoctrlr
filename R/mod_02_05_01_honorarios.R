#' 02_05_01_honorarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_05_01_honorarios_ui <- function(id){

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

        shiny::selectizeInput(ns("origen"), "Seleccionar Origen",
                                   choices = c("EPAM", "Funcionamiento"), selected = "", multiple = TRUE,
                                   options = list(placeholder = "Todo seleccionado")),

        shiny::selectizeInput(ns("partida"), "Seleccionar Partidas",
                              choices = "", selected = "",
                              multiple = TRUE,
                              options = list(placeholder = "Todo seleccionado"))
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

#' 02_05_01_honorarios Server Functions
#'
#' @noRd
mod_02_05_01_honorarios_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT ejercicio, fecha, nro_entrada, partida, monto, ",
      "glosa FROM comprobantes_gtos_gpo_partida_gto_rpa03g ",
      "WHERE grupo = '300') G INNER JOIN " ,
      "(SELECT ejercicio, nro_entrada, cuit, cta_cte ",
      "FROM comprobantes_gtos_rcg01_uejp ",
      "WHERE cuit = '30632351514') C ",
      "ON (G.ejercicio = C.ejercicio ",
      "AND G.nro_entrada = C.nro_entrada)"
    )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(slave_honorarios(),
           siif_comprobantes_gtos_rcg01_uejp(),
           siif_comprobantes_gtos_gpo_partida_gto_rpa03g())
    })

    observeEvent(to_listen(), {

      r6_slave <- MyData$new(sql_path("slave"))
      r6_siif <- MyData$new(sql_path("siif"))

      r6_slave$
        get_query("SELECT DISTINCT ejercicio FROM honorarios")

      r6_siif$
        get_query(paste0("SELECT DISTINCT G.ejercicio FROM ", sql_join))

      choices_rv$ejercicio <- sort(c(r6_slave$data$ejercicio,
                                     r6_siif$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      r6_slave$get_query("SELECT DISTINCT partida FROM honorarios")

      r6_siif$
        get_query(paste0("SELECT DISTINCT G.partida FROM ", sql_join))

      choices_rv$partida <- unique(sort(
        c(r6_slave$data$partida,
          r6_siif$data$partida)
        ))


      shiny::updateSelectizeInput(session, "partida",
                                  choices = choices_rv$partida)

      # r6_siif$finalize()
      # r6_sscc$finalize()

    })

    #Generate Table
    table <- eventReactive(input$update, {

      r6_slave <- MyData$new(sql_path("slave"))
      r6_siif <- MyData$new(sql_path("siif"))

      #Setting input$ejercicio default value
      if (is.null(input$ejercicio)) {
        shiny::updateSelectizeInput(session, "ejercicio",
                                    selected = max(as.integer(choices_rv$ejercicio)))
      }

      if (is.null(input$origen)) {
        shiny::updateSelectizeInput(session, "origen",
                                    selected = c("EPAM", "Funcionamiento"))
      }

      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))

      partida_vec <- input$partida %||%
        choices_rv$partida

      origen_vec <- input$origen %||%
        c("EPAM", "Funcionamiento")

      #Filtering slave
      r6_slave$
        get_query(
          paste0("SELECT ejercicio, fecha, nro_entrada, estructura, ",
                 "partida, importe_bruto FROM honorarios ",
                 "WHERE partida = ? "),
          params = list(partida_vec)
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          origen = ifelse(stringr::str_sub(.data$estructura, 0, 2) == "12",
                          "EPAM", "Funcionamiento")
        )$
        rename(
          fecha_slave = .data$fecha,
          nro_entrada_slave = .data$nro_entrada,
          partida_slave = .data$partida,
          monto_slave = .data$importe_bruto
        )$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$origen %in% origen_vec
               )$
        select(
          .data$nro_entrada_slave, .data$fecha_slave,
          .data$partida_slave, .data$monto_slave
        )$
        group_by(
          .data$nro_entrada_slave, .data$fecha_slave,
          .data$partida_slave
        )$
        summarise(
          monto_slave = sum(.data$monto_slave)
        )

      #Filtering and grouping siif
      r6_siif$
        get_query(
          paste0("SELECT G.ejercicio, G.fecha, G.glosa, ",
                 "C.cta_cte, G.nro_entrada, G.partida, ",
                 "G.monto FROM ", sql_join, " ",
                 "WHERE partida = ?"),
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
        filter(
          .data$cta_cte %in% c("130832-05", "130832-07"),
         (stringr::str_detect(.data$glosa, "HONOR") |
            stringr::str_detect(.data$glosa, "RECON") |
            stringr::str_detect(.data$glosa, "LOC")),
         .data$monto > 0
        )$
        mutate(
          origen = ifelse(.data$cta_cte == "103832-05",
                          "Funcionamiento", "EPAM")
        )$
        rename(
          fecha_siif = .data$fecha,
          nro_entrada_siif = .data$nro_entrada,
          partida_siif = .data$partida,
          monto_siif = .data$monto
        )$
        filter(.data$ejercicio %in% ejercicio_vec,
               .data$origen %in% origen_vec
               )$
        select(
          .data$nro_entrada_siif, .data$fecha_siif,
          .data$partida_siif, .data$monto_siif
        )$
        group_by(
          .data$nro_entrada_siif, .data$fecha_siif,
          .data$partida_siif
        )$
        summarise(
          monto_siif = sum(.data$monto_siif)
        )

      # #Joinning and calulating
      r6_siif$
        full_join(r6_slave$data,
                  by = c("nro_entrada_siif" = "nro_entrada_slave",
                         "partida_siif" = "partida_slave"),
                  keep = TRUE)$
        mutate_if(is.numeric, replace_NA_0)$
        mutate(
          dif_nro_entrada = ifelse((.data$nro_entrada_siif == .data$nro_entrada_slave), TRUE, FALSE),
          dif_nro_entrada = ifelse(is.na(.data$dif_nro_entrada), FALSE, .data$dif_nro_entrada),
          dif_fecha = ifelse((.data$fecha_siif == .data$fecha_slave), TRUE, FALSE),
          dif_fecha = ifelse(is.na(.data$dif_fecha), FALSE, .data$dif_fecha),
          dif_partida = ifelse((.data$partida_siif == .data$partida_slave), TRUE, FALSE),
          dif_partida = ifelse(is.na(.data$dif_partida), FALSE, .data$dif_partida),
          dif_monto = ifelse(dplyr::near(.data$monto_siif , .data$monto_slave), TRUE, FALSE),
          dif_monto = ifelse(is.na(.data$dif_monto), FALSE, .data$dif_monto)
        )$
        select(
          .data$nro_entrada_siif, .data$nro_entrada_slave, .data$dif_nro_entrada,
          .data$fecha_siif, .data$fecha_slave, .data$dif_fecha,
          .data$partida_siif, .data$partida_slave, .data$dif_partida,
          .data$monto_siif, .data$monto_slave, .data$dif_monto
        )
        # mutate_if(is.logical, replace_logical_symbol)
        # dif_nro_entrada = ifelse(dif_nro_entrada > 0, "\u2713", "\u2718")

      if (input$mostrar) {
        r6_siif$
          filter(
            (.data$dif_monto + .data$dif_nro_entrada +
               .data$dif_fecha + .data$dif_partida) < 4
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
# mod_02_05_01_honorarios_ui("02_05_01_honorarios_1")

## To be copied in the server
# mod_02_05_01_honorarios_server("02_05_01_honorarios_1")
