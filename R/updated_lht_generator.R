#' Multiple Updated LHT Generator class
#'
#' @description
#' Class that allows the streamline-generation of predicted LHTs given new data.
UpdatedLHTGenerator <- R6::R6Class("UpdatedLHTGenerator", public = list(

  master_db = NULL,
  predicting_lht_df = NULL,
  func_domains = NULL,
  lht_names = NULL,
  # // @formatter:off
  #' @description
  #' Initialise the Updated LHT Generator class
  #'
  #' @param master_db Fishlife database
  #' @param predicting_lht_df dataframe providing the taxon details and their predicting LHT values
  #' @param func_domains list of transforming function which names must conform to FishLife's expectations
  #' @param lht_names list of user-defined LHT names associated with their FishLife's counterparts
  #' @export
  # // @formatter:on
  initialize = function(master_db, predicting_lht_df, func_domains, lht_names) {
    self$master_db <- master_db
    self$predicting_lht_df <- predicting_lht_df
    self$func_domains <- func_domains
    self$lht_names <- lht_names
  },
  # // @formatter:off
  #' @description
  #'
  #' Update the given LHTs per taxon according to the new predicted value yielded by FishLfe
  #' @returns list of LHT matrices, where each name in the list corresponds a single taxon
  #' @export
  # // @formatter:on
  update = function() {
    all_species_local_lhts <- private$build_taxa_predicting_lhts()
    predicted_matrices <- private$generate_taxa_predicted_matrices(all_species_local_lhts)
    return(predicted_matrices)
  }
), private = list(
  # // @formatter:off
  #' @description
  #' Convert a dataframe into a list of rows removing those columns per taxon that contain NA
  #'
  #' @returns a unnamed list of LHT values per taxon
  # // @formatter:on
  df_to_not_na_list = function(df) {
    return(
      lapply(split(df, 1:nrow(df)), function(x) {
        x <- x %>%
          select_if(~!any(is.na(.)))
        as.list(x)
      })
    )
  },
  # // @formatter:off
  #' @description
  #' Given a list of LHTs it renames each variable into the expected FishLife names
  #'
  #' @returns renamed list of lht which names conform to Fishlife's expected patterns
  # // @formatter:on
  transcribe_lhts = function(lhts) {
    new_lhts <- list()
    for (lht_name in names(lhts)) {
      lht_value <- lhts[[lht_name]]
      fishlife_name <- self$lht_names[[lht_name]]
      new_lhts[[fishlife_name]] <- lht_value
    }
    return(new_lhts)
  },
  # // @formatter:off
  #' @description
  #' Build a list of predicting parameters for each taxon
  #' 
  #' @returns list of LHT values (another list) for each taxon
  # // @formatter:on
  build_taxa_predicting_lhts = function() {
    user_col_names <- names(self$lht_names)

    # Generate a list per species that contain the predicting variables
    predicting_lht_df <- self$predicting_lht_df %>%
      select_at(.vars = user_col_names)
    all_species_lhts <- private$df_to_not_na_list(predicting_lht_df)
    names(all_species_lhts) <- self$predicting_lht_df$species

    # Transcribe the predicting variables to those names the Fishlife library expects
    all_species_lhts <- lapply(all_species_lhts, function(species_lhts) {
      private$transcribe_lhts(species_lhts)
    })

    return(all_species_lhts)
  },
  # // @formatter:off
  #' @description
  #' Generate the predicted Fishlife matrices (LHTs and LHT covariances)
  # // @formatter:on
  generate_taxa_predicted_matrices = function(all_species_local_lhts) {
    updated_lhts <- list()
    for (ind_species in names(all_species_local_lhts)) {
      species_local_lhts <- all_species_local_lhts[[ind_species]]
      taxon_extractor <- TaxonLHTGrabber$new(self$master_db, ind_species)
      taxon_details <- taxon_extractor$extract()
      if (is.null(taxon_details)) {
        stop(paste("Unable to find life history traits for taxa", ind_species))
      }
      taxon_predictor <- TaxonLHTPredictor$new(self$master_db,
                                            taxon_details$estimated_lhts,
                                            taxon_details$estimated_covariance,
                                            species_local_lhts,
                                            self$func_domains)
      updated_lhts[[ind_species]] <- taxon_predictor$predict()
    }
    return(updated_lhts)
  }
))