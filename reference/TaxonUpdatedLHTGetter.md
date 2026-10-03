# Single Taxon LHT Predictor class

Class that given some LHTs will fetch their predicted version according
to FishLife covariance

## Value

matrix-form LHT values to be passed on to FishLife

## Super class

[`LHTpicker::MixinUtilities`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.md)
-\> `TaxonUpdatedLHTGetter`

## Public fields

- `common_columns_ds`:

  common columns across the underlying data structure of FishLife.
  Taxa's LHT could end up having additional fields that the overall
  covariance matrix does not support.

## Methods

### Public methods

- [`TaxonUpdatedLHTGetter$new()`](#method-TaxonUpdatedLHTGetter-new)

- [`TaxonUpdatedLHTGetter$generate_new_lht_matrix()`](#method-TaxonUpdatedLHTGetter-generate_new_lht_matrix)

- [`TaxonUpdatedLHTGetter$predict()`](#method-TaxonUpdatedLHTGetter-predict)

- [`TaxonUpdatedLHTGetter$clone()`](#method-TaxonUpdatedLHTGetter-clone)

Inherited methods

- [`LHTpicker::MixinUtilities$apply_func_to_df()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-apply_func_to_df)
- [`LHTpicker::MixinUtilities$create_empty_dataframe()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-create_empty_dataframe)
- [`LHTpicker::MixinUtilities$list_of_vectors_to_dataframe()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-list_of_vectors_to_dataframe)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    TaxonUpdatedLHTGetter$new(
      master_db,
      estimated_lhts,
      estimated_lht_cov,
      new_lhts,
      func_domains
    )

#### Arguments

- `master_db`:

  Fishlife database

- `estimated_lhts`:

  taxon's LHT numeric vector as fetched from Fishlife.

- `new_lhts`:

  predicting LHT list which names must conform to FishLife's
  expectations

- `func_domains`:

  list of transforming function which names must conform to FishLife's
  expectations

- `estimated_lht_conv`:

  taxon's covariance matrix as fetched from Fishlife

------------------------------------------------------------------------

### Method `generate_new_lht_matrix()`

Generate the new LHT matrix in shape and mathematical domain expected by
Fishlife

#### Usage

    TaxonUpdatedLHTGetter$generate_new_lht_matrix()

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Generate a matrix with the new predicted LHTs given some initial values,
both in log and log-converted space

#### Usage

    TaxonUpdatedLHTGetter$predict()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaxonUpdatedLHTGetter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
