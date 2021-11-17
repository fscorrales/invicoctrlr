MyData <- R6::R6Class(
  classname = "MyData",
  inherit  = MyConnection,
  public = list(
    # data = NULL,
    initialize = function(sqlite_name = NULL) {

      stopifnot(is.character(sqlite_name), length(sqlite_name) == 1)
      private$conn = DBI::dbConnect(RSQLite::SQLite(),
                                    dbname = sqlite_name)
      # self$list_fields()

    },
    select = function(...) {
      self$data <- dplyr::select(self$data, ...)
      invisible(self)
    },
    rename = function(...) {
      self$data <- dplyr::rename(self$data, ...)
      invisible(self)
    },
    mutate = function(...) {
      self$data <- dplyr::mutate(self$data, ...)
      invisible(self)
    },
    mutate_if = function(...) {
      self$data <- dplyr::mutate_if(self$data, ...)
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
    group_by = function(...) {
      self$data <- dplyr::group_by(self$data, ...)
      invisible(self)
    },
    summarise = function(...) {
      self$data <- dplyr::summarise(self$data, ...)
      invisible(self)
    },
    bind_rows = function(...) {
      self$data <- dplyr::bind_rows(self$data, ...)
      invisible(self)
    }


  )
)
