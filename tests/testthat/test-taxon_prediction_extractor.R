test_that("Extraction and backtransformation of predicted LHT values", {

  # (1) Fetch ...
  t_extractor <- TaxonLHTGrabber$new(testing_db, "Trisopterus luscus")
  t.luscus_details <- t_extractor$extract()

  estimated_lhts <- t.luscus_details$estimated_lhts
  estimated_covariance <- t.luscus_details$estimated_covariance
  # --> Provide LHT values following fishlife's name convention
  new_lhts <- testing_sample_data[testing_sample_data$species == 'Trisopterus luscus',]
  new_lhts <- new_lhts[-1] # forget about species
  fishlife_lht_names <- unlist(unname(fishlife_context$lht_names))
  names(new_lhts) <- fishlife_lht_names

  # (2)... Predict ...
  t_predictor <- TaxonPredictor$new(
    testing_db,
    estimated_lhts,
    estimated_covariance,
    new_lhts,
    fishlife_context$transform_function
  )
  estimated_lhts <- t_predictor$predict()

  # (3) ... Extract and Transform ...
  t_backtransformer <- TaxonPredictionExtractor$new(fishlife_context$updated_prefix,
                                                    fishlife_context$lht_names,
                                                    fishlife_context$backtransform_function,
                                                    estimated_lhts$updatemean_j)
  results <- t_backtransformer$extract_and_backtransform()
  expect_true(all(stringr::str_detect(names(results), fishlife_context$updated_prefix)))
  expect_true(!any(is.na(results)))
})
