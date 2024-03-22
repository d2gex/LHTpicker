#' Mixin Utility class
#'
#' @description
#' class that holds a few util methods used across all classes in the this R package
MixinUtilities <- R6::R6Class("MixinUtilities", public = list(

  # // @formatter:off
  #' @export
  # // @formatter:on
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
  }
))