#' 01_00_base_de_datos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_00_base_de_datos_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      collapsible = FALSE,
      width = 12,
      maximizable = TRUE,
      dropdownMenu =  bs4Dash::boxDropdown(
        icon = shiny::icon("save"),
        bs4Dash::boxDropdownItem("Link to google", href = "http://www.google.com"),
        bs4Dash::boxDropdownItem("Item with inputId", id = "dropdown_item2"),
        bs4Dash::dropdownDivider(),
        bs4Dash::boxDropdownItem("item 3", href = "#", icon = icon("th"))
      ),
      DT::DTOutput(ns(tab_id$bd))
    )
  )
}

#' 01_00_base_de_datos Server Functions
#'
#' @noRd
mod_01_00_base_de_datos_server <- function(id, data, selection = "single",
                                           DTServer = TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output[[tab_id$bd]] <- DT::renderDT({DT::datatable(data(), style = 'bootstrap',
                                                extensions = c("Scroller", "Buttons"),
                                                filter = list(position = 'top', clear = FALSE, plain=T),
                                                options = list(pageLength = 100, deferRender = T,
                                                               scroller = T, stateSave=F, filter = "top",
                                                               searching = T, scrollY = '400px', scrollX = T,
                                                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                               dom = 'Brtip'),
                                                selection = selection
    )
    }, server = DTServer)


  })
}

## To be copied in the UI
# mod_01_00_base_de_datos_ui("01_00_base_de_datos_ui_1")

## To be copied in the server
# mod_01_00_base_de_datos_server("01_00_base_de_datos_ui_1")
