#' 01_00_base_de_datos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("data_table"))
  )
}

#' 01_00_base_de_datos Server Functions
#'
#' @noRd
mod_data_table_server <- function(id, data, selection = "single",
                                  DTServer = TRUE,
                                  format_curr = NULL, format_perc = NULL,
                                  format_round = NULL, format_date = NULL,
                                  format_style = NULL, ...){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$data_table <- DT::renderDT({
      ans <- DT::datatable(data(), rownames = FALSE, class = "display compact", style="default",
                    extensions = c("Scroller", "Buttons", 'ColReorder'),
                    filter = list(position = 'bottom', clear = FALSE, plain=T),
                    options = list(pageLength = 100,
                                   # lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "todos los")),
                                   filter = "top", searching = TRUE, ColReorder = TRUE,
                                   deferRender = T, stateSave=F,
                                   scrollX = T, scrollY = '400px',
                                   scroller = T,
                                   language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                   dom = 'BRtlip', ...
                                   ),
                    selection = selection
                    )

      if (not_null(format_curr)) {
        format_curr$table <- ans
        ans <- do.call(DT::formatCurrency, format_curr)
      }

      if (not_null(format_perc)) {
        format_perc$table <- ans
        ans <- do.call(DT::formatPercentage, format_perc)
      }

      if (not_null(format_round)) {
        format_round$table <- ans
        ans <- do.call(DT::formatRound, format_round)
      }

      if (not_null(format_date)) {
        format_date$table <- ans
        ans <- do.call(DT::formatDate, format_date)
      }

      if (not_null(format_style)) {
        format_style$table <- ans
        ans <- do.call(DT::formatStyle, format_style)
      }

      return(ans)

      }, server = DTServer)

    })
}

## On caller
# format_rec_vs_sscc <- rlang::expr(
#   DT::formatCurrency(columns = c("recursos_siif", "depositos_sscc",
#                                  "diferencia", "dif_acum")))

## On mod_data_table_server
# if (not_null(format_curr)) {
#   format_curr$table <- ans
#   ans <- eval(format_curr)
# }



## Style should be one of “default”, “bootstrap”, “bootstrap4”, “foundation”,
## “jqueryui”, “semanticui”

## The position of column filters may be off when scrolling is enabled
## in the table, e.g. via the options scrollX and/or scrollY. The appearance
## may be affected by Shiny sliders, as reported in #49.

## colnames(iris) is a character vector of length 5, and we replace it
# datatable(head(iris), colnames = c('Here', 'Are', 'Some', 'New', 'Names'))

## exclude the first two columns (i.e. they are always visible)
# datatable(
#   iris2, rownames = FALSE,
#   extensions = 'Buttons', options = list(
#     dom = 'Bfrtip',
#     buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
#   )
# )

## The RowGroup extension provides an easy way to use the row grouping feature.
## In the example below, cyl (number of cylinders) is used as the row group column
## and its value is displayed as the row group label.
# mtcars2 = mtcars[1:20, ]
# datatable(
#   mtcars2[order(mtcars2$cyl), ],
#   extensions = 'RowGroup',
#   options = list(rowGroup = list(dataSrc = 2)),
#   selection = 'none'
# )

## When the table has a large number of rows, you may not want to render all rows
## on the page immediately since it will be slow. The Scroller extension makes it
## possible to only render the visible portion of the table.
# m = matrix(runif(1000 * 4), ncol = 4, dimnames = list(NULL, letters[1:4]))
# m = cbind(id = seq_len(nrow(m)), round(m, 2))
# datatable(m, extensions = 'Scroller', options = list(
#   deferRender = TRUE,
#   scrollY = 200,
#   scroller = TRUE
# ))

## The argument escape determines whether the HTML entities in the table are escaped
## or not. There can be potential security problems when the table is rendered in
## dynamic web applications such as Shiny if you do not escape them. Here is a quick example:
# m = matrix(c(
#   '<b>Bold</b>', '<em>Emphasize</em>', '<a href="http://rstudio.com">RStudio</a>',
#   '<a href="#" onclick="alert(\'Hello World\');">Hello</a>'
# ), 2)
# colnames(m) = c('<span style="color:red">Column 1</span>', '<em>Column 2</em>')
# datatable(m)  # escape = TRUE by default

## DT Plugins https://rstudio.github.io/DT/plugins.html (see ellipsis and scrollResize)

## DT EXTENSIONS in https://rstudio.github.io/DT/extensions.html

## DT Buttons in https://rstudio.github.io/DT/003-tabletools-buttons.html

## DT Styling options in https://datatables.net/manual/styling/classes

## DT OPTIONS in https://rstudio.github.io/DT/options.html

## Especific column configuration in https://datatables.net/reference/option/columnDefs

## To be copied in the UI
# mod_01_00_base_de_datos_ui("01_00_base_de_datos_ui_1")

## To be copied in the server
# mod_01_00_base_de_datos_server("01_00_base_de_datos_ui_1")
