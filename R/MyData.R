MyData <- R6::R6Class(
  classname = "MyData",
  inherit  = MyConnection,
  public = list(
    # data = NULL,
    initialize = function(sqlite_name = NULL) {

      stopifnot(is.character(sqlite_name), length(sqlite_name) == 1)
      private$conn = invicodatr::connect_sqlite(sqlite_name)
      # self$list_fields()

    },
    select = function(a_list) {
      a_list$.data <- self$data
      self$data <- dplyr::select(self$data, ...)
      invisible(self)
    },
    mutate = function(...) {
      self$data <- dplyr::mutate(self$data, ...)
      invisible(self)
    },
    filter = function(...) {
      self$data <- dplyr::filter(self$data, ...)
      invisible(self)
    },
    arrange = function(...) {
      self$data <- dplyr::arrange(self$data, ...)
      invisible(self)
    },
    full_join = function(...) {
      self$data <- dplyr::full_join(self$data, ...)
      invisible(self)
    },
    left_join = function(...) {
      self$data <- dplyr::left_join(self$data, ...)
      invisible(self)
    },
    bind_rows = function(...) {
      self$data <- dplyr::bind_rows(self$data, ...)
      invisible(self)
    }


  )
)
