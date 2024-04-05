#' Single Taxon LHT GRABBER class
#'
#' @description
#' Abstracts away the underlying FishLife dynamics and grab the predicted LHT details of a given taxon
TaxonPredictedLHTGetter <- R6::R6Class("TaxonPredictedLHTGetter", public = list(

  master_db = NULL,
  taxon = NULL,
  # @formatter:off
  #'@description
  #' Initialise Taxon LHT Grabber class
  #'
  #' @param master_db Fishlife database
  #' @param taxon string holding the taxon name
  #' @export
  # @formatter:on
  initialize = function(master_db, taxon) {
    self$master_db <- master_db
    self$taxon <- taxon
  },
  # @formatter:off
  #'@description
  #' Fetch the sought taxon details
  #'
  #' @returns A list with three elements:
  #'    a) The database unique identifier for the sought taxon
  #'    b) The predicted and stored LHTs for such taxon
  #'    c) The convariance matrix for all parameters associated to the sought taxon
  # @formatter:on
  extract = function() {
    taxa_names <- c(self$master_db$tree$tip.label, self$master_db$tree$node.label[-1])
    db_taxon_offset <- match(self$taxon, taxa_names)
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