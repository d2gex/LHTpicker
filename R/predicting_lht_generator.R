library("R6")
library("dplyr")
library("stringr")

PredictingLHTGenerator <- R6Class("PredictingLHTGenerator", public = list(

  pred_matrix = NULL,
  all_species_lh_parameters = NULL,
  initialize = function(pred_matrix, all_species_lh_parameters) {
    self$pred_matrix <- pred_matrix
    self$all_species_lh_parameters <- all_species_lh_parameters
  },
  generate = function() {
    all_species_lh_parameters <- self$all_species_lh_parameters %>%
      filter(fishlife_prediction == 1) %>%
      mutate(Amax = case_when(
        is.na(max_age_lit) ~ max_age_m,
        .default = max_age_lit
      )) %>%
      # species should be the same as the first two words of stocks
      rowwise() %>%
      mutate(species = paste(unlist(str_split(stocks, " "))[1:2], collapse = " "))

    common_cols <- intersect(colnames(self$pred_matrix), colnames(all_species_lh_parameters))
    all_species_lh_parameters <- all_species_lh_parameters[, c(common_cols, 'stocks')]
    fish_life_df <- merge(self$pred_matrix, all_species_lh_parameters, by = "species")

    # Ensure you get only those values for which the cell in the prediction matrix is not NA
    lht_columns <- common_cols[common_cols != 'species']
    for (col_name in lht_columns) {
      lht_col_x <- paste0(col_name, '.x')
      lht_col_y <- paste0(col_name, '.y')
      fish_life_df <- fish_life_df %>%
        mutate(!!lht_col_x := case_when(
          !is.na(.data[[lht_col_x]]) ~ .data[[lht_col_y]],
          .default = .data[[lht_col_x]]
        ))
    }
    # Rename some columns and get rid of some others
    fish_life_df <- fish_life_df %>%
      select(-contains(".y")) %>%
      rename_with(~str_replace(., '\\.x', ''))
    return(fish_life_df)
  }
))