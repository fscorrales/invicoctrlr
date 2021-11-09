#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # Your application server logic
  (paste0("mod_", tab_id$bd_presupuesto, "_server")) %>%
    do.call(list(tab_id$bd_presupuesto))
  (paste0("mod_", tab_id$bd_tesoreria, "_server")) %>%
    do.call(list(tab_id$bd_tesoreria))
  (paste0("mod_", tab_id$bd_gastos_fondos, "_server")) %>%
    do.call(list(tab_id$bd_gastos_fondos))
  (paste0("mod_", tab_id$bd_contabilidad, "_server")) %>%
    do.call(list(tab_id$bd_contabilidad))
  (paste0("mod_", tab_id$bd_sgf, "_server")) %>%
    do.call(list(tab_id$bd_sgf))
  (paste0("mod_", tab_id$bd_sscc, "_server")) %>%
    do.call(list(tab_id$bd_sscc))
  (paste0("mod_", tab_id$bd_icaro, "_server")) %>%
    do.call(list(tab_id$bd_icaro))
  (paste0("mod_", tab_id$ctrl_recursos, "_server")) %>%
    do.call(list(tab_id$ctrl_recursos))
  # (paste0("mod_", tab_id$ctrl_remamente, "_server")) %>%
  #   do.call(list(tab_id$ctrl_remamente))
  # (paste0("mod_", tab_id$ctrl_icaro, "_server")) %>%
  #   do.call(list(tab_id$ctrl_icaro))
  (paste0("mod_", tab_id$td, "_server")) %>%
    do.call(list(tab_id$td))

}
