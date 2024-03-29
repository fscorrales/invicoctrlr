##Trigger sync
sql_path <- function(db_name) {

  paste0(dirname(getwd()), "/R Output/SQLite Files/",
         db_name, ".sqlite")

}

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

primary_key_cta_cte_trigger <- make_reactive_trigger()
primary_key_cta_cte <- shiny::reactive({
  primary_key_cta_cte_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("primary_key",
                                       "cta_cte")
})

# r6_primary_key_cta_cte <- MyData$new(sql_path("primary_key"))
# primary_key_cta_cte_trigger <- make_reactive_trigger()
# primary_key_cta_cte <- shiny::reactive(primary_key_cta_cte_trigger$depend())
# observeEvent(primary_key_cta_cte(),
#   r6_primary_key_cta_cte$read_table("cta_cte"),
#   ignoreNULL = FALSE
# )

## It would be a very nice implementation of R6 (problem when connecting outside WD)
# r6_ppto_gtos_fte_rf602 <- MyData$new("siif")
# siif_ppto_gtos_fte_trigger <- make_reactive_trigger()
# siif_ppto_gtos_fte_rf602 <- shiny::reactive(siif_ppto_gtos_fte_trigger$depend())
# observeEvent(siif_ppto_gtos_fte_rf602(), {
#   r6_ppto_gtos_fte_rf602$read_table("ppto_gtos_fte_rf602")
#   r6_ppto_gtos_fte_rf602$
#     mutate(estructura = paste(programa, subprograma, proyecto, actividad, sep = "-"))$
#     select(ejercicio, estructura, dplyr::everything())$
#     arrange(desc(ejercicio), estructura)
#
# })

siif_ppto_gtos_fte_trigger <- make_reactive_trigger()
siif_ppto_gtos_fte_rf602 <- shiny::reactive({
  siif_ppto_gtos_fte_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif", "ppto_gtos_fte_rf602")
  Ans <- Ans %>%
    dplyr::mutate(estructura = paste(programa, subprograma,
                                     proyecto, actividad, sep = "-")) %>%
    dplyr::select(ejercicio, estructura, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), estructura)

})

siif_ppto_gtos_desc_trigger <- make_reactive_trigger()
siif_ppto_gtos_desc_rf610 <- shiny::reactive({
  siif_ppto_gtos_fte_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif", "ppto_gtos_desc_rf610")
  Ans <- Ans %>%
    dplyr::mutate(estructura = paste(programa, subprograma,
                                     proyecto, actividad, sep = "-")) %>%
    dplyr::select(ejercicio, estructura, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), estructura)

})

siif_comprobantes_rec_trigger <- make_reactive_trigger()
siif_comprobantes_rec_rci02 <- shiny::reactive({
  siif_comprobantes_rec_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif", "comprobantes_rec_rci02")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01"),
                  verificado = ifelse(verificado == "S", TRUE, FALSE),
                  remanente = as.logical(remanente),
                  invico = as.logical(invico)) %>%
    # dplyr::mutate(cta_cte = map_values(cta_cte,
    #                                    from = primary_key_cta_cte()$siif_recursos_cta_cte,
    #                                    to = primary_key_cta_cte()$map_to,
    #                                    warn_missing = FALSE)) %>%
    dplyr::select(ejercicio, fecha, nro_entrada, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), fecha, nro_entrada)

})

siif_pagos_trigger <- make_reactive_trigger()
siif_pagos_rtr03 <- shiny::reactive({
  siif_pagos_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif", "pagos_rtr03")
  Ans <- Ans %>%
    dplyr::mutate(fecha_pago = as.Date(fecha_pago, origin = "1970-01-01")) %>%
    dplyr::select(ejercicio, mes, fecha_pago, nro_entrada, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), fecha_pago, nro_entrada)

})

siif_retenciones_por_codigo_trigger <- make_reactive_trigger()
siif_retenciones_por_codigo_rao01 <- shiny::reactive({
  siif_retenciones_por_codigo_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif", "retenciones_por_codigo_rao01")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01")) %>%
    dplyr::select(ejercicio, mes, fecha, nro_entrada, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), fecha, nro_entrada)

})

siif_comprobantes_gtos_trigger <- make_reactive_trigger()
siif_comprobantes_gtos_rcg01_uejp <- shiny::reactive({
  siif_comprobantes_gtos_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif",
                                       "comprobantes_gtos_rcg01_uejp")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01"),
                  comprometido = as.logical(comprometido),
                  verificado = as.logical(verificado),
                  aprobado = as.logical(aprobado),
                  pagado = as.logical(pagado)) %>%
    dplyr::select(ejercicio, mes, fecha, nro_entrada,
                  nro_origen, nro_fondo, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), fecha, nro_entrada)

})

siif_comprobantes_gtos_partida_trigger <- make_reactive_trigger()
siif_comprobantes_gtos_partida_rcg01_par <- shiny::reactive({
  siif_comprobantes_gtos_partida_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif",
                                       "comprobantes_gtos_partida_rcg01_par")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01"),
                  comprometido = as.logical(comprometido),
                  verificado = as.logical(verificado),
                  aprobado = as.logical(aprobado),
                  pagado = as.logical(pagado)) %>%
    dplyr::select(ejercicio, mes, fecha, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), fecha, nro_entrada)

})

siif_comprobantes_gtos_gpo_partida_trigger <- make_reactive_trigger()
siif_comprobantes_gtos_gpo_partida_gto_rpa03g <- shiny::reactive({
  siif_comprobantes_gtos_gpo_partida_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif",
                                       "comprobantes_gtos_gpo_partida_gto_rpa03g")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01")) %>%
    dplyr::select(ejercicio, mes, fecha, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), fecha, nro_entrada)

})

siif_deuda_flotante_trigger <- make_reactive_trigger()
siif_deuda_flotante_rdeu012 <- shiny::reactive({
  siif_deuda_flotante_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif",
                                       "deuda_flotante_rdeu012")
  Ans <- Ans %>%
    dplyr::mutate(fecha_desde = as.Date(fecha_desde, origin = "1970-01-01"),
                  fecha_hasta = as.Date(fecha_hasta, origin = "1970-01-01"),
                  fecha_aprobado = as.Date(fecha_aprobado, origin = "1970-01-01")) %>%
    dplyr::select(fecha_desde, fecha_hasta, mes_hasta, fecha_aprobado,
                  dplyr::everything()) %>%
    dplyr::arrange(desc(fecha_hasta), fuente, nro_entrada)

})

siif_resumen_fdos_trigger <- make_reactive_trigger()
siif_resumen_fdos_rfondo07tp <- shiny::reactive({
  siif_resumen_fdos_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif",
                                       "resumen_fdos_rfondo07tp")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01")) %>%
    dplyr::select(ejercicio, mes, dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), nro_fondo)

})

siif_mayor_contable_trigger <- make_reactive_trigger()
siif_mayor_contable_rcocc31 <- shiny::reactive({
  siif_mayor_contable_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("siif",
                                       "mayor_contable_rcocc31")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01"),
                  fecha_aprobado = as.Date(fecha_aprobado, origin = "1970-01-01")) %>%
    dplyr::select(ejercicio, cta_contable, fecha_aprobado, fecha,
                  dplyr::everything()) %>%
    dplyr::arrange(desc(ejercicio), cta_contable, nro_entrada)

})

sgf_resumen_rend_prov_trigger <- make_reactive_trigger()
sgf_resumen_rend_prov <- shiny::reactive({
  sgf_resumen_rend_prov_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("sgf",
                                       "resumen_rend_prov")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01")) %>%
    dplyr::select(origen, ejercicio, mes, fecha,
                  dplyr::everything()) %>%
    dplyr::arrange(origen, fecha)

})

sgf_listado_prov_trigger <- make_reactive_trigger()
sgf_listado_prov <- shiny::reactive({
  sgf_listado_prov_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("sgf",
                                       "listado_prov")
  Ans <- Ans %>%
    dplyr::select(cuit, descripcion,
                  dplyr::everything()) %>%
    dplyr::arrange(descripcion)

})

sscc_banco_invico_trigger <- make_reactive_trigger()
sscc_banco_invico <- shiny::reactive({
  sscc_banco_invico_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("sscc",
                                       "banco_invico")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01"),
                  es_cheque = as.logical(es_cheque)) %>%
    dplyr::select(ejercicio, fecha, mes, movimiento, cta_cte, monto,
                  dplyr::everything()) %>%
    dplyr::arrange(desc(fecha))

})

#Unique trigger to whole icaro DB
icaro_trigger <- make_reactive_trigger()
icaro_obras <- shiny::reactive({
  icaro_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("icaro_new",
                                       "obras")
  Ans <- Ans %>%
    dplyr::select(obra, estructura, partida,
                  dplyr::everything()) %>%
    dplyr::arrange(obra)

})
icaro_carga <- shiny::reactive({
  icaro_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("icaro_new",
                                       "carga")
  Ans <- Ans %>%
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01"),
                  ejercicio = as.character(lubridate::year(fecha))) %>%
    dplyr::select(ejercicio, fecha, nro_entrada, tipo, obra,
                  dplyr::everything()) %>%
    dplyr::arrange(desc(fecha), desc(nro_entrada))

})
icaro_retenciones <- shiny::reactive({
  icaro_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("icaro_new",
                                       "retenciones")
  Ans <- Ans %>%
    dplyr::arrange( desc(nro_entrada))

})

#Provisional Slave import
slave_trigger <- make_reactive_trigger()
slave_honorarios <- shiny::reactive({
  slave_trigger$depend()
  Ans <- invicodatr::read_table_sqlite("slave",
                                       "honorarios")
  Ans <- Ans %>%
    dplyr::mutate(fecha = lubridate::as_date(lubridate::as_datetime(fecha))) %>%
    dplyr::arrange(desc(ejercicio), fecha)


})
