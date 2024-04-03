test_that("Multiple taxon LHTS are updated", {
  p_lht_gen <- UpdatedLHTGenerator$new(
    testing_db,
    wanted_update_taxon_details,
    fishlife_context$transform_function_list,
    fishlife_context$lht_names
  )
  updated_lhts <- p_lht_gen$update()
  expect_true(length(updated_lhts) == 2)
  expect_true(any(!is.na(updated_lhts$`Trisopterus luscus`$updatemean_j)))
  expect_true(any(!is.na(updated_lhts$`Pollachius pollachius`$updatemean_j)))
})
