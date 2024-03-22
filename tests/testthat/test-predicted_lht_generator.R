test_that("Multiple taxon LHTS are re-predicted", {
  p_lht_gen <- PredictedLHTGenerator$new(
    testing_db,
    testing_sample_data,
    fishlife_context$transform_function,
    fishlife_context$lht_names
  )
  predicted_lhts <- p_lht_gen$predict()
  expect_true(length(predicted_lhts) == 2)
  expect_true(any(!is.na(predicted_lhts$`Trisopterus luscus`$updatemean_j)))
  expect_true(any(!is.na(predicted_lhts$`Pollachius pollachius`$updatemean_j)))
})
