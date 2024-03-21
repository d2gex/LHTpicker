#' Single Taxon LHT Predictor class
#'
#' @description
#' Class that given some LHTs will fetch their predicted version according to FishLife covariance
TaxonLHTPredictor <- R6::R6Class("TaxonLHTPredictor", public = list(

  master_db = NULL,
  estimated_lhts = NULL,
  estimated_lht_cov = NULL,
  new_lhts = NULL,
  func_domains = NULL,
    #' @field common_columns_ds common columns across the underlying data structure of FishLife. Taxa's LHT
    #'    could end up having additional fields that the overall covariance matrix does not support.
  common_columns_ds = NULL,
    #' @description
    #'
    #' @param master_db Fishlife database
    #' @param estimated_lhts taxon's LHT numeric vector as fetched from Fishlife.
    #' @param estimated_lht_conv taxon's covariance matrix as fetched from Fishlife
    #' @param new_lhts predicting LHT list which names must conform to FishLife's expectations
    #' @param func_domains list of transforming function which names must conform to FishLife's expectations
    #' @export
  initialize = function(master_db, estimated_lhts, estimated_lht_cov, new_lhts, func_domains) {

    self$master_db <- master_db
    self$estimated_lhts <- estimated_lhts
    self$estimated_lht_cov <- estimated_lht_cov
    self$new_lhts <- new_lhts
    self$func_domains <- func_domains
    self$common_columns_ds <- intersect(names(self$estimated_lhts), colnames(self$master_db$obsCov_jj))
  },
  # // @formatter:off
  #' @description
  #' Generate the new LHT matrix in shape and mathematical domain expected by Fishlife
  #' @returns matrix-form LHT values to be passed on to FishLife
  #' @export
  # // @formatter:on
  generate_new_lht_matrix = function() {

    # Convert list of lhts to dataframe and applied logs where required.
    new_lhts <- private$list_of_vectors_to_dataframe(self$new_lhts)
    new_lhts <- private$apply_func_to_df(new_lhts, self$func_domains)
    # Build lht matrix with values and NA where required
    new_lhts <- private$build_matrix_new_lhts(new_lhts, self$common_columns_ds)
    return(new_lhts)
  },
  # // @formatter:off
  #' @description
  #' Generate a matrix with the new predicted LHTs given some initial values, both in log and log-converted space
  # // @formatter:on
  predict = function() {
    new_lht_matrix <- self$generate_new_lht_matrix()
    return(
      FishLife::update_prediction(
        predmean_j = self$estimated_lhts[self$common_columns_ds], # Estimated LHTs 'as is'
        # Estimated LHT covariance for specific species 'as is'
        predcov_jj = self$estimated_lht_cov[self$common_columns_ds, self$common_columns_ds],
        obscov_jj = self$master_db$obsCov_jj,  # Overall covariance among LHTs
        Ynew_ij = new_lht_matrix # new LHT values from which to infer the new ones
      )
    )
  }
), private = list(
  create_empty_dataframe = function(col_names) {
    df <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
    colnames(df) <- col_names
    return(df)
  },
  list_of_vectors_to_dataframe = function(lht_list) {
    if (length(unique(lengths(lht_list))) != 1) {
      stop("All nested vectors must have the same length")
    }
    return(as.data.frame(do.call(cbind, lht_list)))
  },
  apply_func_to_df = function(data, func_space) {

    func_space_ <- func_space[names(data)]
    return(data %>%
             dplyr::mutate(dplyr::across(names(func_space_), ~func_space_[[dplyr::cur_column()]](.x)))
    )
  },
  # // @formatter:off
  #' @description
  #' Build the new LHT matrix in the expected shape by FishLife
  # // @formatter:on
  build_matrix_new_lhts = function(new_lhts_df, colnames) {
    new_lht_colnames <- colnames(new_lhts_df)
    complement_cols <- setdiff(colnames, new_lht_colnames)
    empty_df <- private$create_empty_dataframe(complement_cols)
    empty_df[nrow(new_lhts_df),] <- NA
    new_lhts_df <- cbind(new_lhts_df, empty_df)
    return(as.matrix(new_lhts_df))
  }
))