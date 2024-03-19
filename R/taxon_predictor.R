TaxonPredictor <- R6::R6Class("TaxonPredictor", public = list(

  master_db = NULL,
  estimated_lhts = NULL,
  estimated_lht_cov = NULL,
  new_lhts = NULL,
  func_domains = NULL,
  columns_of_interest = NULL,
  initialize = function(master_db, estimated_lhts, estimated_lht_cov, new_lhts, func_domains) {

    self$master_db <- master_db
    self$estimated_lhts <- estimated_lhts
    self$estimated_lht_cov <- estimated_lht_cov
    self$new_lhts <- new_lhts
    self$func_domains <- func_domains
    self$columns_of_interest <- intersect(names(self$estimated_lhts), colnames(self$master_db$obsCov_jj))
  },
  generate_new_lht_matrix = function() {
    # // @formatter:off
    #' Generate the new LHT matrix in shape and mathematical domain expected by Fishlife
    # // @formatter:on
    # Convert list of lhts to dataframe and applied logs where required.
    new_lhts <- private$list_of_vectors_to_dataframe(self$new_lhts)
    new_lhts <- private$apply_func_to_df(new_lhts, self$func_domains)
    # Build lht matrix with values and NA where required
    new_lhts <- private$build_matrix_new_lhts(new_lhts, names(self$estimated_lhts))
    return(new_lhts[, self$columns_of_interest])
  },
  predict = function() {
    # // @formatter:off
    #' Generate a matrix with the new predicted LHTs given some initial values, both in log and log-converted space
    # // @formatter:on
    new_lht_matrix <- self$generate_new_lht_matrix()
    return(
      FishLife::update_prediction(
        predmean_j = self$estimated_lhts[self$columns_of_interest], # Estimated LHTs 'as is'
        # Estimated LHT covariance for specific species 'as is'
        predcov_jj = self$estimated_lht_cov[self$columns_of_interest, self$columns_of_interest],
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
  df_to_named_matrix = function(data) {
    mat <- as.matrix(data[, -1])
    rownames(mat) <- data[[1]]
    return(mat)
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
  build_matrix_new_lhts = function(new_lhts_df, colnames) {
    # // @formatter:off
    #' Build the new LHT matrix in the expected shape
    # // @formatter:on
    new_lht_colnames <- colnames(new_lhts_df)
    complement_cols <- setdiff(colnames, new_lht_colnames)
    empty_df <- private$create_empty_dataframe(complement_cols)
    empty_df[nrow(new_lhts_df),] <- NA
    new_lhts_df <- cbind(new_lhts_df, empty_df)
    return(private$df_to_named_matrix(new_lhts_df))
  }
))