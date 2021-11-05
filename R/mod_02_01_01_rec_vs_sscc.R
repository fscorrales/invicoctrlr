#' 02_01_01_rec_vs_sscc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_01_01_rec_vs_sscc_ui <- function(id){

  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::column(
        6, shiny::checkboxGroupInput(ns("grouping"), "Agrupamiento del Reporte",
                                     choices = c("ejercicio",
                                                 "fecha", "mes","cta_cte", "grupo"),
                                     selected = "Mes" , inline = FALSE)
        ),
      shiny::column(
        6, shiny::selectizeInput(ns("ejercicio"), "Ejercicio",
                                 choices = "", selected = "", multiple = TRUE,
                                 options = list(placeholder = "Todo seleccionado")),
        shiny::dateRangeInput(ns("fecha"), "Seleccionar Fecha", start = NULL,
                              end = NULL, format = "dd-mm-yyyy",
                              startview = "month", language = "es", separator = " a ")
        )
      ),
    shiny::selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                          choices = "", selected = "", multiple = TRUE,
                          options = list(placeholder = "Todo seleccionado")),
    shiny::fluidRow(
      shiny::column(
        6, shiny::radioButtons(ns("DepurarTransfIntRecursoINVICO"),
                               "¿Depurar Transferencias Internas?",
                               choices = c("SI", "NO"), selected = "SI")
        ),
      shiny::column(
        6, shiny::radioButtons(ns("DepurarPFRecursoINVICO"),
                               "¿Depurar Inversiones en PF?",
                               choices = c("SI", "NO"), selected = "SI")
        )
      ),
    shiny::fluidRow(
      shiny::column(
        6, shiny::radioButtons(ns("DepurarOtrosRecursoINVICO"),
                               "¿Depurar Cheques Remplazados y Reingresos Vs?",
                               choices = c("SI", "NO"), selected = "SI")
             ),
      shiny::column(
        6, shiny::radioButtons(ns("DepurarCertNegRecursoINVICO"),
                               "¿Depurar Cheques endosados a favor de INVICO (Certificado Negativo)?",
                               choices = c("SI", "NO"), selected = "SI")
        )
      ),
    shiny::column(12, align = "center",
                  bs4Dash::actionButton(ns("update"),
                                        "Actualizar Filtros",
                                        status = "primary"))
  )
}

#' 02_01_01_rec_vs_sscc Server Functions
#'
#' @noRd
mod_02_01_01_rec_vs_sscc_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #Initial DBs setting
    db_rec <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- siif_comprobantes_rec_rci02() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$siif_recursos_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE),
                      grupo = dplyr::case_when(
                        cta_cte == "10270" ~ "FONAVI",
                        cta_cte %in% c("130832-12", "334", "Macro", "Patagonia") ~ "RECUPEROS",
                        TRUE ~ "OTROS"
                      ))
      return(db)
    })

    db_sscc <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- sscc_banco_invico() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$sscc_cta_cte,
                                                to = db_cta_cte$map_to,
                                                warn_missing = FALSE),
                      ejercicio = as.character(lubridate::year(fecha)),
                      grupo = dplyr::case_when(
                        cta_cte == "10270" ~ "FONAVI",
                        cta_cte %in% c("130832-12", "334", "Macro", "Patagonia") ~ "RECUPEROS",
                        TRUE ~ "OTROS"
                      ))
      return(db)

    })

    #Updting shiny input objets
    ejercicio_var <- reactive({

      ans <- db_rec() %>%
        dplyr::select(ejercicio, fecha, cta_cte)

      ans <- db_sscc() %>%
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
    table <- reactive({

      db <- db_rec() %>%
        dplyr::group_by(cta_cte, ejercicio) %>%
        dplyr::summarise(total = sum(monto)) %>%
        tidyr::pivot_wider(names_from = cta_cte,
                           values_from = total)

      # warpbreaks %>%
      #   pivot_wider(
      #     names_from = wool,
      #     values_from = breaks, #can use more than one column
      #     values_fn = mean
      #   )

      return(db)

    })

    return(table)


  })
}

## To be copied in the UI
# mod_02_01_01_rec_vs_sscc_ui("02_01_01_rec_vs_sscc_ui_1")

## To be copied in the server
# mod_02_01_01_rec_vs_sscc_server("02_01_01_rec_vs_sscc_ui_1")
