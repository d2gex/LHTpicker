# Single Taxon LHT GRABBER class

Abstracts away the underlying FishLife dynamics and grab the predicted
LHT details of a given taxon

## Value

A list with three elements: a) The database unique identifier for the
sought taxon b) The predicted and stored LHTs for such taxon c) The
convariance matrix for all parameters associated to the sought taxon

## Methods

### Public methods

- [`TaxonPredictedLHTGetter$new()`](#method-TaxonPredictedLHTGetter-new)

- [`TaxonPredictedLHTGetter$extract()`](#method-TaxonPredictedLHTGetter-extract)

- [`TaxonPredictedLHTGetter$clone()`](#method-TaxonPredictedLHTGetter-clone)

------------------------------------------------------------------------

### Method `new()`

Initialise Taxon LHT Grabber class

#### Usage

    TaxonPredictedLHTGetter$new(master_db, taxon)

#### Arguments

- `master_db`:

  Fishlife database

- `taxon`:

  string holding the taxon name

------------------------------------------------------------------------

### Method `extract()`

Fetch the sought taxon details

#### Usage

    TaxonPredictedLHTGetter$extract()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaxonPredictedLHTGetter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
