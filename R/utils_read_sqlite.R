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
siif_ppto_gtos_fte_df <- shiny::reactive({
  siif_ppto_gtos_fte_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("SIIF", "EjecPresPorFteSIIF")
})

# EjecPresConDescSIIFTrigger <- makereactivetrigger()
# EjecPresConDescSIIF.df <- reactive({
#   EjecPresConDescSIIFTrigger$depend()
#   Ans <- LeerBD("SIIF", "EjecPresConDescSIIF")
# })
