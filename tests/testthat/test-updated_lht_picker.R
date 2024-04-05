test_that("Pick updated LHT values given new data at once", {
  u_lht_picker <- UpdatedLHTPicker$new(
    testing_db,
    wanted_update_taxon_details ,
    fishlife_context$updated_prefix,
    fishlife_context$transform_function_list,
    fishlife_context$backtransform_function_list,
    fishlife_context$lht_names
  )
  updated_lht_df <- u_lht_picker$pick_and_transform()
  non_logarithmic_scale <- 10

  expect_true(nrow(updated_lht_df) == 2)
  expect_true(any(!is.na(updated_lht_df[updated_lht_df$taxon == 'Trisopterus luscus',])))
  expect_true(any(!is.na(updated_lht_df[updated_lht_df$taxon == 'Pollachius pollachius',])))

  # Ensure values have been backtransformed
  expect_true(updated_lht_df[updated_lht_df$taxon == 'Trisopterus luscus', 'Linf'] > non_logarithmic_scale)
  expect_true(updated_lht_df[updated_lht_df$taxon == 'Pollachius pollachius', 'Linf'] > non_logarithmic_scale)
  expect_equal(updated_lht_df[updated_lht_df$taxon == 'Trisopterus luscus', 'updated_Linf'], 44.24, tolerance = 0.01)
})
