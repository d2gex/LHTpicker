#' Multiple Taxon Collector class
#'
#' @description
#' Fetch, transform and build the predicted LHT dataframe for multiple species obtained from FishLife
PredictedLHTPicker <- R6::R6Class("PredictedLHTPicker", inherit = MixinUtilities, public = list(
  master_db = NULL,
  lht_names = NULL,
  backtransform_function_list = NULL,
  wanted_lht_df = NULL,
  # // @formatter:off
  #' @description
  #' Initialise the PredictedLHTPicker
  #'
  #' @param master_db Fishlife database
  #' @param lht_names list of user-defined LHT names associated with their FishLife's counterparts
  #' @param backtransform_function_list list of backward-transformation functions to be applied on obtained LHT from FishLife
  #' @param wanted_lht_df dataframe holding the wanted taxa details as rows and LHT as columns
  #' @export
  # // @formatter:on
  initialize = function(master_db, lht_names, backtransform_function_list, wanted_lht_df) {
    self$master_db <- master_db
    self$lht_names <- lht_names
    self$backtransform_function_list <- backtransform_function_list
    self$wanted_lht_df <- wanted_lht_df
  },
  # // @formatter:off
  #' @description
  #' Collect and backtransform all LHT values per taxon that have beed obtained from Fishlife
  #'
  #' @returns dataframe that keeps the LHT values per taxon
  # // @formatter:on
  collect_and_backtransform = function() {
    results <- self$wanted_lht_df
    for (ind_taxon in self$wanted_lht_df$taxon) {
      taxon_grabber <- TaxonLHTCollector$new(self$master_db, ind_taxon)
      taxon_details <- taxon_grabber$extract()
      subset_estimated_lhts <- private$subset_taxon_detail_matrix(taxon_details$estimated_lhts)
      results <- private$backtransform_and_fill_user_df(subset_estimated_lhts, ind_taxon, results)
    }
    return(results)
  }
), private = list(
  # // @formatter:off
  #' @description
  #' Given a full matrix of LHT values for as specific taxon, it returns a subset of those values matching
  #' the user's interest
  #'
  #' @param matrix of LHT values as returned by Fishlife
  #' @returns subset matrix of LHT values
  # // @formatter:on
  subset_taxon_detail_matrix = function(taxon_details) {

    # convert user-defined LHT names to FishLife's
    sought_user_names <- names(self$wanted_lht_df)
    sought_user_names <- sought_user_names[!(sought_user_names %in% 'taxon')]
    sought_fishlife_names <- unlist(unname(self$lht_names[sought_user_names]))

    # Subset the matrix of all return pair of values provided by Fishlife
    return(taxon_details[sought_fishlife_names])
  },
  # // @formatter:off
  #' @description
  #' Given a subset of LHT values from Fishlife matching the user's interest, it updates user-defined dataframe
  #'
  #'
  #' @param taxon_details subset matrix from the full Fishlife matrix
  #' @param taxon_name name of taxon
  #' @param results original user-defined dataframe
  #' @return updated user-defined dataframe
  # // @formatter:on
  backtransform_and_fill_user_df = function(taxon_details, taxon_name, results) {
    inverted_lht_names <- lapply(names(self$lht_names), function(x) { x })
    names(inverted_lht_names) <- unlist(unname(self$lht_names))
    for (fish_lht_name in names(taxon_details)) {
      u_lht_name <- inverted_lht_names[[fish_lht_name]]
      lht_value <- taxon_details[fish_lht_name]
      back_func <- self$backtransform_function_list[[fish_lht_name]]
      results[results$taxon == taxon_name, u_lht_name] <- back_func(lht_value)
    }
    return(results)
  }
))