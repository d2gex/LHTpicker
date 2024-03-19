library("R6")
source("lht_prediction/taxon_result_extractor.R")


TaxaResultExtractor <- R6Class("TaxaResultExtractor", public = list(
  update_prefix = NULL,
  result_names = NULL,
  back_transform_matrix = NULL,
  predicting_lht_df = NULL,
  predicted_lht_list = NULL,
  initialize = function(update_prefix, result_names, back_transform_matrix, predicting_lht_df, predicted_lht_list) {
    self$update_prefix <- update_prefix
    self$result_names <- result_names
    self$back_transform_matrix <- back_transform_matrix
    self$predicting_lht_df <- predicting_lht_df
    self$predicted_lht_list <- predicted_lht_list
  },
  extract = function() {
    results <- self$predicting_lht_df
    for (ind_species in names(self$predicted_lht_list)) {
      predicted_taxon_lht <- (self$predicted_lht_list[[ind_species]])$updatemean_j
      tax_extractor <- TaxonResultExtractor$new(self$update_prefix,
                                                self$result_names,
                                                self$back_transform_matrix,
                                                predicted_taxon_lht)
      taxon_details <- tax_extractor$extract()
      results <- private$update_lht_results(results, taxon_details, ind_species)
    }
    return(results)
  }
), private = list(
  update_lht_results = function(results, taxon_details, taxon_name) {
    updated_lht_names <- names(taxon_details)
    for (lht_name in updated_lht_names) {
      sought_lht_indicator <- results[results$species == taxon_name, lht_name]
      results[results$species == taxon_name, lht_name] <- ifelse(
        !is.na(sought_lht_indicator),
        taxon_details[, lht_name],
        NA
      )
    }
    return(results)
  }
))