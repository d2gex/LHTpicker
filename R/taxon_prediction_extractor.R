#' Single Taxon Prediction Extractor class
#'
#' @description
#' Class that extracts and transform predicted values of LHTs for a single species obtained from FishLife
TaxonPredictionExtractor <- R6::R6Class("TaxonPredictionExtractor", public = list(
  update_prefix = NULL,
  predicted_results = NULL,
  lht_names = NULL,
  back_transform_matrix = NULL,
  # // @formatter:off
  #' @description
  #' Initialise the TaxonPredictionExtractor
  #'
  #' @param update_prefix string to be added as prefix to the column names keeping the obtained new LHT values
  #' @param lht_names list of user-defined LHT names associated with their FishLife's counterparts
  #' @param back_transform_matrix list of backward-transformation functions to be applied on obtained LHT from FishLife
  #' @param predicted_results matrix of predicted results from FishLife
  #' @export
  # // @formatter:on
  initialize = function(update_prefix, lht_names, back_transform_matrix, predicted_results) {
    self$update_prefix <- update_prefix
    self$lht_names <- lht_names
    self$back_transform_matrix <- back_transform_matrix
    self$predicted_results <- predicted_results
  },
  # // @formatter:off
  #' @description
  #' Extract and backtransform the updated LHT values from FishLife
  #'
  #' @returns a dataframe with backtransformed LHT values
  # // @formatter:on
  extract_and_backtransform = function() {
    updated_result_names <- private$add_prefix_to_result_names()
    return(private$fetch_backtransform_results(updated_result_names))

  }
), private = list(
  # // @formatter:off
  #' @description
  #' Add a prefix to the user-defined predicted LHT names
  # // @formatter:on
  add_prefix_to_result_names = function() {
    prefixed_result_names <- self$lht_names
    prefixed_names <- unlist(lapply(names(prefixed_result_names), function(x) { paste0(self$update_prefix, "_", x) }))
    names(prefixed_result_names) <- prefixed_names
    return(prefixed_result_names)
  },
  # // @formatter:off
  #' @description
  #' Fetch the sought LHT predicted_results and backtransform them as given by their corresponding conversion function
  # // @formatter:on
  fetch_backtransform_results = function(lht_names) {
    backtransformed_results <- list()
    for (prefixed_lht in names(lht_names)) {
      result_key <- lht_names[[prefixed_lht]]
      result_value <- as.numeric(self$predicted_results[result_key,])
      back_func <- self$back_transform_matrix[[result_key]]
      backtransformed_results[[prefixed_lht]] <- back_func(result_value)
    }
    return(as.data.frame(backtransformed_results))
  }
))