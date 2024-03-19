test_that("Unable to find specific species", {
  t_extractor <- TaxonExtractor$new(testing_db, "I do not exist as species")
  species_details <- t_extractor$extract()
  expect_equal(is.null(species_details), TRUE)
})

test_that("Specific species is found", {
  t_extractor <- TaxonExtractor$new(testing_db, "Trisopterus luscus")
  species_details <- t_extractor$extract()
  expect_equal(is.null(species_details), FALSE)
})

test_that("Taxon is found and is different to species", {
  t_extractor <- TaxonExtractor$new(testing_db, "Trisopterus")
  taxon_details <- t_extractor$extract()
  t_extractor <- TaxonExtractor$new(testing_db, "Trisopterus luscus")
  species_details <- t_extractor$extract()
  expect_equal(is.null(taxon_details), FALSE)
  expect_equal(is.null(species_details), FALSE)
  expect_failure(expect_equal(species_details$db_taxon_offset, taxon_details$db_taxon_offset))
})

