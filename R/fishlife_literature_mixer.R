library("R6")
source("utils.R")

FishlifeLitMixer <- R6Class("FishlifeLitMixer", public = list(
  update_prefix = NULL,
  fishlife_suffix = NULL,
  run_algo = NULL,
  lit_spar_df = NULL,
  fishlife_spar_df = NULL,
  initialize = function(update_prefix, fishlife_suffix, run_algo, lit_spar_df, fishlife_spar_df) {
    self$update_prefix <- update_prefix
    self$fishlife_suffix <- fishlife_suffix
    self$run_algo <- run_algo
    self$lit_spar_df <- lit_spar_df
    self$fishlife_spar_df <- fishlife_spar_df
  },
  mix = function() {
    # // @formatter:off
    #' Overwrite the species parameters dataframe with those Fishlife predicted LHTs
    # // @formatter:on
    lit_fishlife_spar_df <- create_empty_dataframe(names(self$lit_spar_df))
    for (offset in 1:nrow(self$lit_spar_df)) {
      # Add row to new resulting dataframe
      lit_fishlife_spar_df <- rbind(lit_fishlife_spar_df, self$lit_spar_df[offset,])

      # If column is fishlife-related grab the new updated values from the fishlife dataframe and
      # write them over the existing literature ones
      if (self$lit_spar_df[offset, 'fishlife_prediction'] == 1) {
        lit_row <- self$lit_spar_df[offset,]
        species_name <- lit_row$stocks
        fishlife_row <- self$fishlife_spar_df %>% filter(stocks == species_name)
        lit_row <- private$mix_rows(lit_row, fishlife_row)
        lit_row <- private$add_suffix_to_specie_name(lit_row, self$fishlife_suffix)
        lit_fishlife_spar_df <- rbind(lit_fishlife_spar_df, lit_row)
      }
    }
    return(lit_fishlife_spar_df)
  }
), private = list(
  mix_rows = function(lit_row, fishlife_row) {
    # // @formatter:off
    #' Mix two rows corresponding to the literature and fishlife dataframes, respectively, giving precedence to
    #' fishlife values
    # // @formatter:on
    fishlife_lht_data <- fishlife_row %>%
      select(contains(self$update_prefix)) %>%
      rename_with(~str_replace(.x, paste0(self$update_prefix, '_'), ''))

    fishlife_lht_names <- names(fishlife_lht_data)
    for (lht_name in fishlife_lht_names) {
      fishlife_lht_value <- fishlife_lht_data[1, lht_name]

      # Prepare column values according to their nature
      if (lht_name == 'Amax') {
        lht_name <- 'max_age_lit'
        fishlife_lht_value <- ceiling(fishlife_lht_value)
      }
      else {
        fishlife_lht_value <- round(fishlife_lht_value, 2)
      }
      lit_lht_value <- lit_row[1, lht_name]
      lit_row[1, lht_name] <- ifelse(
        !is.na(fishlife_lht_value),
        fishlife_lht_value,
        lit_lht_value
      )
      # Ensure M_K is updated if M is part of the predicted fishlife values
      if (lht_name == 'M' & !is.na(fishlife_lht_value)) {
        lit_row[1, 'M_K'] <- round(lit_row[1, lht_name] / lit_row[1, 'K'], 2)
      }
      if (lht_name == 'L50' & !is.na(fishlife_lht_value)) {
        lit_row[1, 'L95'] <- round(1.15 * lit_row[1, lht_name], 2)
      }

    }
    lit_row[1, 'Winf'] <- NA
    lit_row[1, 'run_algo'] <- self$run_algo
    return(lit_row)
  },
  add_suffix_to_specie_name = function(lit_row, suffix) {
    stocks <- lit_row$stocks
    tokens <- unlist(str_split(stocks, '\\)'))
    if (length(tokens) == 1) {
      lit_row$stocks <- paste(stocks, paste0('(', suffix, ')'))
    }
    else {
      lit_row$stocks <- paste(tokens[1], paste0(suffix, ')'))
    }
    return(lit_row)
  }
))