test_that("Extraction and backtransformation of predicted LHT values", {

  # (1) Fetch ...
  t_extractor <- TaxonPredictedLHTGetter$new(testing_db, "Trisopterus luscus")
  t.luscus_details <- t_extractor$extract()

  estimated_lhts <- t.luscus_details$estimated_lhts
  estimated_covariance <- t.luscus_details$estimated_covariance
  # --> Provide LHT values following fishlife's name convention
  input_lhts <- wanted_update_taxon_details[wanted_update_taxon_details$taxon == 'Trisopterus luscus',]
  input_lhts <- input_lhts[-1] # forget about species
  # Order both data structures to avoid mismatching of values
  input_lhts <- input_lhts %>% select(order(colnames(input_lhts)))
  lht_names <- fishlife_context$lht_names[order(names(fishlife_context$lht_names))]
  names(input_lhts) <- unlist(unname(lht_names))

  # (2)... Predict ...
  t_predictor <- TaxonUpdatedLHTGetter$new(
    testing_db,
    estimated_lhts,
    estimated_covariance,
    input_lhts,
    fishlife_context$transform_function_list
  )
  estimated_lhts <- t_predictor$predict()

  # (3) ... Extract and Transform ...
  t_backtransformer <- TaxonUpdateExtractor$new(fishlife_context$updated_prefix,
                                                lht_names,
                                                fishlife_context$backtransform_function_list,
                                                estimated_lhts$updatemean_j)
  results <- t_backtransformer$extract_and_backtransform()
  expect_true(all(stringr::str_detect(names(results), fishlife_context$updated_prefix)))
  expect_true(!any(is.na(results)))
})
