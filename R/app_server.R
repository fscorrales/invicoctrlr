#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  (paste0("mod_", tab_id$bd_tesoreria, "_server")) %>%
    do.call(list(tab_id$bd_tesoreria))
  (paste0("mod_", tab_id$bd_presupuesto, "_server")) %>%
    do.call(list(tab_id$bd_presupuesto))

}
