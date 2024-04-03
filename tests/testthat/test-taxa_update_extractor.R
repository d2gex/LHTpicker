test_that("Multiple taxon's LHTS have been extracted and backtransformed", {
  p_lht_gen <- UpdatedLHTGenerator$new(
    testing_db,
    wanted_update_taxon_details,
    fishlife_context$transform_function_list,
    fishlife_context$lht_names
  )
  updated_lhts <- p_lht_gen$update()

  taxa_extractor <- TaxaUpdateExtractor$new(
    fishlife_context$updated_prefix,
    fishlife_context$lht_names,
    fishlife_context$backtransform_function_list,
    wanted_update_taxon_details,
    updated_lhts
  )
  all_lhts_df <- taxa_extractor$extract_and_backtransform()
  non_logarithmic_scale <- 10

  expect_true(nrow(all_lhts_df) == 2)
  expect_true(any(!is.na(all_lhts_df[all_lhts_df$taxon == 'Trisopterus luscus',])))
  expect_true(any(!is.na(all_lhts_df[all_lhts_df$taxon == 'Pollachius pollachius',])))

  # Ensure values have been backtransformed
  expect_true(all_lhts_df[all_lhts_df$taxon == 'Trisopterus luscus', 'Linf'] > non_logarithmic_scale)
  expect_true(all_lhts_df[all_lhts_df$taxon == 'Pollachius pollachius', 'Linf'] > non_logarithmic_scale)
})
