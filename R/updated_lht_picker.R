#' Taxa's updated LHT picker
#'
#' @description
#' Fetch, transform and build a dataframe with the updated LHT values per taxon obtained from FishLife
UpdatedLHTPicker <- R6::R6Class("UpdatedLHTPicker", public = list(
  master_db = NULL,
  predicting_lht_df = NULL,
  updated_prefix = NULL,
  transform_function_list = NULL,
  backtransform_function_list = NULL,
  lht_names = NULL,
  # // @formatter:off
  #' @description
  #' Initialise the UpdatedLHTPicker class
  #'
  #' @param master_db Fishlife database
  #' @param predicting_lht_df dataframe keeping the original inputted LHTs per taxon
  #' @param update_prefix string to be added as prefix to the column names keeping the obtained new LHT values
  #' @param transform_function_list list of transforming function which names must conform to FishLife's expectations
  #' @param backtransform_function_list list of backward-transformation functions to be applied on obtained LHT from FishLife
  #' @param lht_names list of user-defined LHT names associated with their FishLife's counterparts
  #' @export
  # // @formatter:off
  initialize = function(master_db,
                        predicting_lht_df,
                        updated_prefix,
                        transform_function_list,
                        backtransform_function_list,
                        lht_names = NULL) {
    self$master_db <- master_db
    self$predicting_lht_df <- predicting_lht_df
    self$updated_prefix <- updated_prefix
    self$transform_function_list <- transform_function_list
    self$backtransform_function_list <- backtransform_function_list
    self$lht_names <- lht_names
  },
  # // @formatter:off
  #' @description
  #'
  #' Build a dataframe with the updated LHT for each wanted taxon as
  #' @returns a dataframe with the predicting and updated LHTs. The latter are prefixed with a keyword.
  #' @export
  # // @formatter:on
  pick_and_backtransform = function() {
    u_lht_gen <- TaxaUpdatedLHTGetter$new(
      self$master_db,
      self$predicting_lht_df,
      self$transform_function_list,
      self$lht_names
    )
    updated_lhts <- u_lht_gen$update()
    t_update_extractor <- TaxaUpdatedLHTBuilder$new(
      self$updated_prefix,
      self$lht_names,
      self$backtransform_function_list,
      self$predicting_lht_df,
      updated_lhts
    )
    updated_lht_df <- t_update_extractor$extract_and_backtransform()
    return(updated_lht_df)
  }
))