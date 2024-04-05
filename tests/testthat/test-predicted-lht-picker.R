test_that("Pick predicted LHT values at once", {
  t_collector <- PredictedLHTPicker$new(testing_db,
                                        fishlife_context$lht_names,
                                        fishlife_context$backtransform_function_list,
                                        wanted_taxon_details)
  all_lhts_df <- t_collector$pick_and_backtransform()

  non_logarithmic_scale <- 10

  expect_true(nrow(all_lhts_df) == 2)
  expect_true(any(!is.na(all_lhts_df[all_lhts_df$taxon == 'Trisopterus luscus',])))
  expect_true(any(!is.na(all_lhts_df[all_lhts_df$taxon == 'Pollachius pollachius',])))

  # Ensure values have been backtransformed
  expect_true(all_lhts_df[all_lhts_df$taxon == 'Trisopterus luscus', 'Linf'] > non_logarithmic_scale)
  expect_true(all_lhts_df[all_lhts_df$taxon == 'Pollachius pollachius', 'Linf'] > non_logarithmic_scale)
})
