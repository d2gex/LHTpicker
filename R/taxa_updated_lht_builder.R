#' Multiple Taxon Update Extractor class
#'
#' @description
#' Class that extracts and transform predicted values of LHTs for multiple species obtained from FishLife
TaxaUpdatedLHTBuilder <- R6::R6Class("TaxaUpdatedLHTBuilder", inherit = MixinUtilities, public = list(
  update_prefix = NULL,
  lht_names = NULL,
  backtransform_function_list = NULL,
  predicting_lht_df = NULL,
  updated_lht_list = NULL,
  # // @formatter:off
  #' @description
  #' Initialise the TaxaPredictionExtractor
  #'
  #' @param update_prefix string to be added as prefix to the column names keeping the obtained new LHT values
  #' @param lht_names list of user-defined LHT names associated with their FishLife's counterparts
  #' @param backtransform_function_list list of backward-transformation functions to be applied on obtained LHT from FishLife
  #' @param predicting_lht_df dataframe keeping the original inputted LHTs per taxon
  #' @param updated_lht_list list of updated LHTs per taxon obtained from Fishlife
  #' @export
  # // @formatter:on
  initialize = function(update_prefix, lht_names, backtransform_function_list, predicting_lht_df, updated_lht_list) {
    self$update_prefix <- update_prefix
    self$lht_names <- lht_names
    self$backtransform_function_list <- backtransform_function_list
    self$predicting_lht_df <- predicting_lht_df
    self$updated_lht_list <- updated_lht_list
  },
  # // @formatter:off
  #' @description
  #' Extract and backtransform all updated LHT values per taxon that have beed obtained from Fishlife
  #'
  #' @returns dataframe that keeps the original and updated LHT values per taxon
  # // @formatter:on
  extract_and_backtransform = function() {
    results <- self$predicting_lht_df
    for (ind_species in names(self$updated_lht_list)) {
      predicted_taxon_lht <- (self$updated_lht_list[[ind_species]])$updatemean_j
      tax_extractor <- TaxonUpdatedLHTBuilder$new(self$update_prefix,
                                                  self$lht_names,
                                                  self$backtransform_function_list,
                                                  predicted_taxon_lht)
      taxon_details <- tax_extractor$extract_and_backtransform()
      results <- private$update_lht_results(results, taxon_details, ind_species)
    }
    return(results)
  }
), private = list(
  # // @formatter:off
  #' @description
  #' Update a dataframe that already holds the original LHT values with those updated for each taxon
  #'
  #' @returns updated dataframe of results with both the original and updated LHT values
  # // @formatter:on
  update_lht_results = function(results, taxon_details, taxon_name) {
    updated_lht_names <- names(taxon_details)
    result_columns <- names(results)

    # Add updated columns to results df if it is the first time updated LHT values are to be added
    if (!length(intersect(result_columns, updated_lht_names))) {
      updated_empty_df <- self$create_empty_dataframe(updated_lht_names)
      updated_empty_df[nrow(results),] <- NA
      results <- cbind(results, updated_empty_df)
    }
    for (lht_name in updated_lht_names) {
      results[results$taxon == taxon_name, lht_name] <- taxon_details[, lht_name]
    }
    return(results)
  }
))