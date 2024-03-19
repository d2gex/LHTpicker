test_that("Ensure testing sampling data is read properly", {
  expect_equal(nrow(testing_sample_data), 2)
  expect_true('Temperature' %in% colnames(testing_sample_data))
})

