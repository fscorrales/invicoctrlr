##Trigger sync
make_reactive_trigger <- function() {
  rv <- shiny::reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- shiny::isolate(rv$a + 1)
    }
  )
}

siif_ppto_gtos_fte_trigger <- make_reactive_trigger()
siif_ppto_gtos_fte_rf602 <- shiny::reactive({
  siif_ppto_gtos_fte_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("SIIF", "ppto_gtos_fte_rf602")
  Ans <- Ans %>%
    dplyr::mutate(estructura = paste(programa, subprograma,
                                     proyecto, actividad, sep = "-")) %>%
    dplyr::select(ejercicio, estructura, dplyr::everything())

})

siif_ppto_gtos_desc_trigger <- make_reactive_trigger()
siif_ppto_gtos_desc_rf610 <- shiny::reactive({
  siif_ppto_gtos_fte_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("SIIF", "ppto_gtos_desc_rf610")
  Ans <- Ans %>%
    dplyr::mutate(estructura = paste(programa, subprograma,
                                     proyecto, actividad, sep = "-")) %>%
    dplyr::select(ejercicio, estructura, dplyr::everything())

})
