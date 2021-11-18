#' 02_04_04_pa6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_04_04_pa6_ui <- function(id){

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
                                 choices = "", selected = "", multiple = FALSE)

        # shiny::selectizeInput(ns("fuente"), "Seleccionar Fuentes",
        #                            choices = "", selected = "", multiple = TRUE,
        #                            options = list(placeholder = "Todo seleccionado")),

        # shiny::selectizeInput(ns("partida"), "Seleccionar Partidas",
        #                       choices = c("421", "422"), selected = c("421", "422"),
        #                       multiple = TRUE)
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
mod_02_04_04_pa6_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    sql_join <- paste0(
      "(SELECT ejercicio, fecha, nro_fondo, ingresos, saldo ",
      "FROM resumen_fdos_rfondo07tp ",
      "WHERE tipo_comprobante = 'ADELANTOS A CONTRATISTAS Y PROVEEDORES') F LEFT JOIN " ,
      "(SELECT ejercicio, fecha, nro_entrada, fuente, cuit, cta_cte, ",
      "monto, nro_fondo FROM comprobantes_gtos_rcg01_uejp ",
      "WHERE clase_reg = 'REG') C ",
      "ON (F.ejercicio = C.ejercicio ",
      "AND F.nro_fondo = C.nro_fondo)"
    )

    #Updting shiny input objets
    choices_rv <- rv()

    to_listen <- reactive({
      list(icaro_carga(),
           siif_comprobantes_gtos_rcg01_uejp(),
           siif_resumen_fdos_rfondo07tp())
    })

    observeEvent(to_listen(), {

      r6_icaro <- MyData$new(sql_path("icaro_new"))
      r6_siif <- MyData$new(sql_path("siif"))

      r6_icaro$
        get_query("SELECT DISTINCT fecha FROM carga")$
        mutate(fecha = as.Date(.data$fecha, origin = "1970-01-01"),
               ejercicio = as.character(lubridate::year(.data$fecha)))

      r6_siif$
        get_query(paste0("SELECT DISTINCT F.ejercicio FROM ", sql_join))

      choices_rv$ejercicio <- sort(c(unique(r6_icaro$data$ejercicio),
                                     r6_siif$data$ejercicio),
                                   decreasing = TRUE)

      shiny::updateSelectizeInput(session, "ejercicio",
                                  choices = choices_rv$ejercicio )

      # r6_icaro$get_query("SELECT DISTINCT fuente FROM carga")
      #
      # r6_siif$
      #   get_query(paste0("SELECT DISTINCT fuente FROM comprobantes_gtos_rcg01_uejp"))
      #
      # choices_rv$fuente <- unique(sort(
      #   c(r6_icaro$data$fuente,
      #     r6_siif$data$fuente)
      #   ))
      #
      # shiny::updateSelectizeInput(session, "fuente",
      #                             choices = choices_rv$fuente)


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


      #Global function variables
      ejercicio_vec <- input$ejercicio %||%
        as.character(max(as.integer(choices_rv$ejercicio)))
#
#       fuente_vec <- input$fuente %||%
#         unique(choices_rv$fuente)

      #Filtering and grouping siif
      r6_siif$
        get_query(
          paste0("SELECT F.ejercicio, F.nro_fondo, F.fecha AS fecha_pa6, ",
                 "F.ingresos AS monto_pa6, F.saldo AS saldo_pa6, ",
                 "C.nro_entrada AS nro_reg, C.monto AS monto_reg, C.cta_cte, ",
                 "C.cuit, C.fuente, C.fecha AS fecha_reg FROM ", sql_join, " ",
                 "WHERE F.ejercicio = '", ejercicio_vec, "'")
        )$
        mutate(
          fecha_pa6 = as.Date(.data$fecha_pa6, origin = "1970-01-01"),
          nro_fondo = sprintf("%05d", as.numeric(.data$nro_fondo)),
          nro_fondo = stringr::str_c(.data$nro_fondo,
                                     format(.data$fecha_pa6, format="%y"),
                                     sep="/"),
          fecha_reg = as.Date(.data$fecha_reg, origin = "1970-01-01"),
          nro_reg = sprintf("%05d", as.numeric(.data$nro_reg)),
          nro_reg = stringr::str_c(.data$nro_reg,
                                   format(.data$fecha_reg, format="%y"),
                                   sep="/"),
          dplyr::across(is.numeric, replace_NA_0),
          saldo_pa6 = ifelse((.data$monto_pa6 - .data$monto_reg) < .data$saldo_pa6,
                             (.data$monto_pa6 - .data$monto_reg), .data$saldo_pa6),
          ctrl = TRUE
        )$
        select(
          .data$nro_fondo, .data$nro_reg, .data$fecha_pa6, .data$fecha_reg,
          .data$monto_pa6, .data$saldo_pa6, .data$ctrl
          # cuit, fuente, monto_reg, cta_cte
        )

      # if (input$mostrar) {
      #   r6_siif$filter(!dplyr::near(saldo_pa6, 0))
      # }

      #Filtering icaro
      r6_icaro$
        get_query(
          paste0("SELECT fecha, nro_entrada, ",
                 "tipo, importe FROM carga")
        )$
        mutate(
          fecha = as.Date(.data$fecha, origin = "1970-01-01"),
          ejercicio = as.character(lubridate::year(.data$fecha))
        )$
        rename(
          nro_icaro = .data$nro_entrada,
          monto_icaro = .data$importe,
          tipo_icaro = .data$tipo
        )$
        filter(.data$ejercicio %in% ejercicio_vec)$
        select(-.data$ejercicio, -.data$fecha)

      #Joining with siif
      ans <- r6_icaro$data %>%
        dplyr::filter(.data$tipo_icaro == "PA6",
                      .data$nro_icaro %in% r6_siif$data$nro_fondo)
      r6_siif$
        full_join(ans, by = c("nro_fondo" = "nro_icaro"), keep = TRUE)$
        rename(nro_pa6_icaro = .data$nro_icaro,
               monto_pa6_icaro = .data$monto_icaro,
               tipo_pa6_icaro = .data$tipo_icaro)

      ans <- r6_icaro$data %>%
        dplyr::filter(.data$tipo_icaro != "PA6",
                      .data$nro_icaro %in% r6_siif$data$nro_reg)
      r6_siif$
        full_join(ans, by = c("nro_reg" = "nro_icaro"), keep = TRUE)$
        rename(nro_reg_icaro = .data$nro_icaro,
               monto_reg_icaro = .data$monto_icaro,
               tipo_reg_icaro = .data$tipo_icaro)$
        mutate(
          dplyr::across(is.numeric, replace_NA_0),
          ctrl = ifelse(.data$ctrl > 0, "\u2713", "\u2718")
        )

    #   if (input$mostrar) {
    #     r6_siif$
    #       filter(
    #         (dif_fuente + dif_monto + dif_cta_cte + dif_cuit +
    #            dif_nro_entrada + dif_fecha + dif_partida) < 7
    #       )
    #   }

      return(r6_siif$data)

    })

    return(table)

  })
}

## To be copied in the UI
# mod_02_04_04_pa6_ui("02_04_04_pa6_1")

## To be copied in the server
# mod_02_04_04_pa6_server("02_04_04_pa6_1")
