TaxonExtractor <- R6::R6Class("TaxonExtractor", public = list(

  master_db = NULL,
  species_name = NULL,
  initialize = function(master_db, species_name) {
    self$master_db <- master_db
    self$species_name <- species_name
  },
  extract = function() {
    db_taxon_offset <- match(self$species_name, self$master_db$tree$tip.label)
    if (is.na(db_taxon_offset)) {
      return(NULL)
    }
    return(
      list(
        db_taxon_offset = db_taxon_offset,
        estimated_lhts = self$master_db$beta_gv[db_taxon_offset,], # named vector
        estimated_covariance = self$master_db$Cov_gvv[db_taxon_offset, ,] # bidimensional matrix
      )
    )
  }
))