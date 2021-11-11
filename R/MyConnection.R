# Generate R6 Class
MyConnection <- R6::R6Class(
  classname = "MyConnection",
  private = list(
    conn = NULL,
    .tables = NULL,
    .fields = NULL
  ),
  active = list(
    tables = function(value) {
      if (missing(value)) {
        private$.tables
      } else {
        stop("`$tables` is read only", call. = FALSE)
      }
    },
    fields = function(value) {
      if (missing(value)) {
        private$.fields
      } else {
        stop("`$fields` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    data = NULL,
    initialize = function(sqlite_name = NULL) {

      stopifnot(is.character(sqlite_name), length(sqlite_name) == 1)
      private$conn = invicodatr::connect_sqlite(sqlite_name)
      self$list_fields()

    },
    connect_sql = function(sqlite_name = NULL) {

      stopifnot(is.character(sqlite_name), length(sqlite_name) == 1)
      private$conn = invicodatr::connect_sqlite(sqlite_name)

    },
    write_table = function(table_name, df, ...) {

      DBI::dbWriteTable(private$conn, name = table_name,
                        value = df, ...)

    },
    read_table = function(table_name, ...) {

      self$data <- DBI::dbReadTable(private$conn,
                                    table_name, ...) %>%
        dplyr::as_tibble()

    },
    list_tables = function() {

      private$.tables <- DBI::dbListTables(private$conn)
      self$tables

    },
    list_fields = function() {

      self$list_tables()
      ans <- lapply(self$tables,
                            function(x) DBI::dbListFields(private$conn, x))
      names(ans) <- self$tables
      private$.fields <- ans
      self$fields

    },
    sort_table = function(table_name, sql_sort_clause) {

      SQLquery <- paste0("CREATE TABLE COPY AS SELECT * FROM ",
                         table_name , " ",
                         "ORDER BY " , sql_sort_clause)
      DBI::dbExecute(private$conn, SQLquery)
      SQLquery <- paste0("DROP TABLE ", table_name)
      DBI::dbExecute(private$conn, SQLquery)
      SQLquery <- paste0("ALTER TABLE COPY RENAME TO ", table_name)
      DBI::dbExecute(private$conn, SQLquery)

    },
    get_query = function(sql_query, ...) {

      ans <- DBI::dbGetQuery(private$conn, sql_query, ...)
      ans <- tibble::as_tibble(ans)
      self$data <- ans

    },
    execute_sql = function(sql_query, ...) {

      rs <- DBI::dbSendStatement(private$conn, sql_query, ...)
      x <- DBI::dbGetRowsAffected(rs)
      DBI::dbClearResult(rs)
      cat("Rows affected: ", x)

    },
    finalize = function() {

      #Message
      message("Disconnecting and cleaning database.")
      #Disconnect from database
      DBI::dbDisconnect(private$conn)
      #Cleaning data
      self$data = NULL
      private$.tables = NULL
      private$.fields = NULL

    }
  )
)
