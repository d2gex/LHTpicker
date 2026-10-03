# Single Taxon Update Extractor class

Class that extracts and transform predicted values of LHTs for a single
species obtained from FishLife

## Value

a dataframe with backtransformed LHT values

## Methods

### Public methods

- [`TaxonUpdatedLHTBuilder$new()`](#method-TaxonUpdatedLHTBuilder-new)

- [`TaxonUpdatedLHTBuilder$extract_and_backtransform()`](#method-TaxonUpdatedLHTBuilder-extract_and_backtransform)

- [`TaxonUpdatedLHTBuilder$clone()`](#method-TaxonUpdatedLHTBuilder-clone)

------------------------------------------------------------------------

### Method `new()`

Initialise the TaxonPredictionExtractor

#### Usage

    TaxonUpdatedLHTBuilder$new(
      update_prefix,
      lht_names,
      backtransform_function_list,
      predicted_results
    )

#### Arguments

- `update_prefix`:

  string to be added as prefix to the column names keeping the obtained
  new LHT values

- `lht_names`:

  list of user-defined LHT names associated with their FishLife's
  counterparts

- `backtransform_function_list`:

  list of backward-transformation functions to be applied on obtained
  LHT from FishLife

- `predicted_results`:

  matrix of predicted results from FishLife

------------------------------------------------------------------------

### Method `extract_and_backtransform()`

Extract and backtransform the updated LHT values from FishLife

#### Usage

    TaxonUpdatedLHTBuilder$extract_and_backtransform()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaxonUpdatedLHTBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
