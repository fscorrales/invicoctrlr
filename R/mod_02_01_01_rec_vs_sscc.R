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
                   options = list(placeholder = "Todo seleccionado"))
  )
}

#' 02_01_01_rec_vs_sscc Server Functions
#'
#' @noRd
mod_02_01_01_rec_vs_sscc_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    ## ---RECURSOS INVICO vs SSCC--- ##
    rec_vs_sscc <- reactive({

      db_cta_cte <- primary_key_cta_cte()

      db <- siif_comprobantes_rec_rci02() %>%
        dplyr::filter(ejercicio == "2021") %>%
        dplyr::mutate(cta_cte = plyr::mapvalues(cta_cte, from = db_cta_cte$siif_recursos_cta_cte,
                                                to = db_cta_cte$map_to)) %>%
        dplyr::group_by(cta_cte) %>%
        dplyr::summarise(Total = sum(monto))

    })



  })
}

## To be copied in the UI
# mod_02_01_01_rec_vs_sscc_ui("02_01_01_rec_vs_sscc_ui_1")

## To be copied in the server
# mod_02_01_01_rec_vs_sscc_server("02_01_01_rec_vs_sscc_ui_1")
