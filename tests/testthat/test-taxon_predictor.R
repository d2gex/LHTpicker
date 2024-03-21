test_that("Testing sample data is loaded properly", {
  expect_equal(nrow(testing_sample_data), 2)
  expect_true('Temperature' %in% colnames(testing_sample_data))
})


test_that("Predict LHT for a specific taxa", {
  t_extractor <- TaxonLHTGrabber$new(testing_db, "Trisopterus luscus")
  t.luscus_details <- t_extractor$extract()
  estimated_lhts <- t.luscus_details$estimated_lhts
  estimated_covariance <- t.luscus_details$estimated_covariance
  # Provide LHT values following fishlife's name convention
  new_lhts <- testing_sample_data[testing_sample_data$species == 'Trisopterus luscus',]
  new_lhts <- new_lhts[-1] # forget about species
  fishlife_lht_names <- unlist(unname(fishlife_context$lht_names))
  names(new_lhts) <- fishlife_lht_names

  t_predictor <- TaxonPredictor$new(
    testing_db,
    estimated_lhts,
    estimated_covariance,
    new_lhts,
    fishlife_context$transform_function
  )
  new_lht_matrix <- t_predictor$generate_new_lht_matrix()
  estimated_lhts <- t_predictor$predict()
  expect_true(sum(is.na(new_lht_matrix)) < length(colnames(new_lht_matrix))) # Some column values are NOT NA
  expect_equal(length(estimated_lhts), 2)
  expect_true(all(!is.na(estimated_lhts$updatemean_j), !is.na(estimated_lhts$updatemean_j)))
})

