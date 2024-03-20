PredictedLHTGenerator <- R6::R6Class("PredictedLHTGenerator", public = list(

  master_db = NULL,
  lht_predicting_matrix = NULL,
  func_conversion_matrix = NULL,
  lht_names = NULL,
  initialize = function(master_db, lht_predicting_matrix, func_conversion_matrix, lht_names) {
    self$master_db <- master_db
    self$lht_predicting_matrix <- lht_predicting_matrix
    self$func_conversion_matrix <- func_conversion_matrix
    self$lht_names <- lht_names
  },
  predict = function() {
    all_species_local_lhts <- private$build_species_new_lhts()
    predicted_matrices <- private$generate_predicted_matrices(all_species_local_lhts)
    return(predicted_matrices)
  }
), private = list(
  df_to_not_na_list = function(df) {
    # // @formatter:off
    #' Convert a dataframe into a list of rows removing those columns per species that contain NA
    # // @formatter:on
    return(
      lapply(split(df, 1:nrow(df)), function(x) {
        x <- x %>%
          select_if(~!any(is.na(.)))
        as.list(x)
      })
    )
  },
  transcribe_lhts = function(lhts) {
    # // @formatter:off
    #' Given a list of LHTs it renames each variable into the expected FishLife names
    # // @formatter:on
    new_lhts <- list()
    for (lht_name in names(lhts)) {
      lht_value <- lhts[[lht_name]]
      fishlife_name <- self$lht_names[[lht_name]]
      new_lhts[[fishlife_name]] <- lht_value
    }
    return(new_lhts)
  },
  build_species_new_lhts = function() {
    # // @formatter:off
    #' Build a list of predicting parameters for each species
    # // @formatter:on
    user_col_names <- names(self$lht_names)

    # Generate a list per species that contain the predicting variables
    predicting_matrix <- self$lht_predicting_matrix %>%
      select_at(.vars = user_col_names)
    all_species_lhts <- private$df_to_not_na_list(predicting_matrix)
    names(all_species_lhts) <- self$lht_predicting_matrix$species

    # Transcribe the predicting variables to those names the Fishlife library expects
    all_species_lhts <- lapply(all_species_lhts, function(species_lhts) {
      private$transcribe_lhts(species_lhts)
    })

    return(all_species_lhts)
  },
  generate_predicted_matrices = function(all_species_local_lhts) {
    # // @formatter:off
    #' Generate the predicted Fishlife matrices (LHTs and LHT covariances)
    # // @formatter:on

    updated_lhts <- list()
    for (ind_species in names(all_species_local_lhts)) {
      species_local_lhts <- all_species_local_lhts[[ind_species]]
      taxon_extractor <- TaxonExtractor$new(self$master_db, ind_species)
      taxon_details <- taxon_extractor$extract()
      if (is.null(taxon_details)) {
        stop(paste("Unable to find life history traits for taxa", ind_species))
      }
      taxon_predictor <- TaxonPredictor$new(self$master_db,
                                            taxon_details$estimated_lhts,
                                            taxon_details$estimated_covariance,
                                            species_local_lhts,
                                            self$func_conversion_matrix)
      updated_lhts[[ind_species]] <- taxon_predictor$predict()
    }
    return(updated_lhts)
  }
))