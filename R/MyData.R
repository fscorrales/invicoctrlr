MyData <- R6::R6Class(
  classname = "MyData",
  inherit  = MyConnection,
  public = list(
    # data = NULL,
    mutate_ = function(mutate_list) {
      mutate_list$.data <- self$data
      ans <- do.call(dplyr::mutate, mutate_list)
      self$data <- ans
    }

  )
)
