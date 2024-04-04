UpdatedLHTPicker <- R6::R6Class("UpdatedLHTPicker", public = list(
  master_db = NULL,
  predicting_lht_df = NULL,
  update_prefix = NULL,
  backtransform_function_list = NULL,
  func_domains = NULL,
  lht_names = NULL,
  initialize = function(master_db,
                        predicting_lht_df,
                        update_prefix,
                        transform_function_list,
                        backtransform_function_list,
                        lht_names = NULL) {
    self$master_db <- master_db
    self$predicting_lht_df <- predicting_lht_df
    self$update_prefix <- update_prefix
    self$transform_function_list <- transform_function_list
    self$backtransform_function_list <- backtransform_function_list
    self$lht_names <- lht_names
  },
  pick_and_transform = function() {
    u_lht_gen <- UpdatedLHTGenerator$new(
      self$master_db,
      self$predicting_lht_df,
      self$transform_function_list,
      self$lht_names
    )
    updated_lhts <- u_lht_gen$update()
    t_update_extractor <- LHTpicker::TaxaUpdateExtractor$new(
      self$update_prefix,
      self$lht_names,
      self$backtransform_function_list,
      self$predicting_lht_df,
      updated_lhts
    )
    updated_lht_df <- t_update_extractor$extract_and_backtransform()
    return(updated_lht_df)
  }
))