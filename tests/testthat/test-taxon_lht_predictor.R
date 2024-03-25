test_that("Testing sample data is loaded properly", {
  expect_equal(nrow(wanted_update_taxon_details), 2)
  expect_true('Temperature' %in% colnames(wanted_update_taxon_details))
})


test_that("Predict LHT for a specific taxa", {
  t_extractor <- TaxonLHTCollector$new(testing_db, "Trisopterus luscus")
  t.luscus_details <- t_extractor$extract()
  estimated_lhts <- t.luscus_details$estimated_lhts
  estimated_covariance <- t.luscus_details$estimated_covariance
  # Provide LHT values following fishlife's name convention
  input_lhts <- wanted_update_taxon_details[wanted_update_taxon_details$species == 'Trisopterus luscus',]
  input_lhts <- input_lhts[-1] # forget about species
  # Order both data structures to avoid mismatching of values
  input_lhts <- input_lhts %>% select(order(colnames(input_lhts)))
  lht_names <- fishlife_context$lht_names[order(names(fishlife_context$lht_names))]
  names(input_lhts) <- unlist(unname(lht_names))

  t_predictor <- TaxonLHTPredictor$new(
    testing_db,
    estimated_lhts,
    estimated_covariance,
    input_lhts,
    fishlife_context$transform_function
  )
  new_lht_matrix <- t_predictor$generate_new_lht_matrix()
  estimated_lhts <- t_predictor$predict()
  expect_true(sum(is.na(new_lht_matrix)) < length(colnames(new_lht_matrix))) # Some column values are NOT NA
  expect_equal(length(estimated_lhts), 2)
  expect_true(all(!is.na(estimated_lhts$updatemean_j), !is.na(estimated_lhts$updatemean_j)))
})

