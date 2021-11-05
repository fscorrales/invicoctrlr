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
    fluidRow(
      column(6,
             selectizeInput(ns("ejercicio"), "Ejercicio",
                            choices = "", selected = "", multiple = TRUE,
                            options = list(placeholder = "Todo seleccionado"))
      ),
      column(6,
             dateRangeInput(ns("fecha"), "Seleccionar Fecha", start = NULL,
                            end = NULL, format = "dd-mm-yyyy",
                            startview = "month", language = "es", separator = " a ")
      )
    ),
    selectizeInput(ns("cta_cte"), "Seleccionar Cuentas",
                   choices = "", selected = "", multiple = TRUE,
                   options = list(placeholder = "Todo seleccionado")),
    checkboxGroupInput(ns("grouping"), "Agrupamiento del Reporte",
                       choices = c("ejercicio",
                                   "fecha", "mes","cta_cte", "grupo"),
                       selected = "Mes" , inline = FALSE),
    fluidRow(
      column(6,
             radioButtons(ns("DepurarTransfIntRecursoINVICO"), "¿Depurar Transferencias Internas?",
                          choices = c("SI", "NO"), selected = "SI")
             ),
      column(6,
             radioButtons(ns("DepurarPFRecursoINVICO"), "¿Depurar Inversiones en PF?",
                          choices = c("SI", "NO"), selected = "SI")
             )
    ),
    fluidRow(
      column(6,
             radioButtons(ns("DepurarOtrosRecursoINVICO"), "¿Depurar Cheques Remplazados y Reingresos Vs?",
                          choices = c("SI", "NO"), selected = "SI")
             ),
      column(6,
             radioButtons(ns("DepurarCertNegRecursoINVICO"), "¿Depurar Cheques endosados a favor de INVICO (Certificado Negativo)?",
                          choices = c("SI", "NO"), selected = "SI")
             )
    )
  )
}

#' 02_01_01_rec_vs_sscc Server Functions
#'
#' @noRd
mod_02_01_01_rec_vs_sscc_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    db_rec <- reactive({

      db_cta_cte <- primary_key_cta_cte()
      db <- siif_comprobantes_rec_rci02() %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte,
                                                from = db_cta_cte$siif_recursos_cta_cte,
                                                to = db_cta_cte$map_to),
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
                                                to = db_cta_cte$map_to),
                      ejercicio = lubridate::year(fecha),
                      grupo = dplyr::case_when(
                        cta_cte == "10270" ~ "FONAVI",
                        cta_cte %in% c("130832-12", "334", "Macro", "Patagonia") ~ "RECUPEROS",
                        TRUE ~ "OTROS"
                      ))
      return(db)

    })

    ## ---RECURSOS INVICO vs SSCC--- ##
    table <- reactive({

      db <- db_rec() %>%
        dplyr::group_by(cta_cte, ejercicio) %>%
        dplyr::summarise(total = sum(monto)) %>%
        tidyr::pivot_wider(names_from = ejercicio,
                           values_from = total)

      return(db)

    })

    return(table)


  })
}

## To be copied in the UI
# mod_02_01_01_rec_vs_sscc_ui("02_01_01_rec_vs_sscc_ui_1")

## To be copied in the server
# mod_02_01_01_rec_vs_sscc_server("02_01_01_rec_vs_sscc_ui_1")
