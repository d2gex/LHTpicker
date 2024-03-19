TaxonResultExtractor <- R6::R6Class("TaxonResultExtractor", public = list(
  update_prefix = NULL,
  results = NULL,
  result_names = NULL,
  back_transform_matrix = NULL,
  initialize = function(update_prefix, result_names, back_transform_matrix, results) {
    self$update_prefix <- update_prefix
    self$result_names <- result_names
    self$back_transform_matrix <- back_transform_matrix
    self$results <- results
  },
  extract = function() {
    updated_result_names <- private$add_prefix_to_result_names()
    return(private$fetch_backtransform_results(updated_result_names))

  }
), private = list(
  add_prefix_to_result_names = function() {
    # // @formatter:off
    #' Add a prefix to the user-defined predicted LHT names
    # // @formatter:on
    prefixed_result_names <- self$result_names
    prefixed_names <- unlist(lapply(names(prefixed_result_names), function(x) { paste0(self$update_prefix, "_", x) }))
    names(prefixed_result_names) <- prefixed_names
    return(prefixed_result_names)
  },
  fetch_backtransform_results = function(lht_names) {
    # // @formatter:off
    #' Fetch the sought LHT results and backtransform them as given by their corresponding conversion function
    # // @formatter:on
    backtransformed_results <- list()
    for (prefixed_lht in names(lht_names)) {
      result_key <- lht_names[[prefixed_lht]]
      result_value <- as.numeric(self$results[result_key,])
      back_func <- self$back_transform_matrix[[result_key]]
      backtransformed_results[[prefixed_lht]] <- back_func(result_value)
    }
    return(as.data.frame(backtransformed_results))
  }
))