# Multiple Taxon Collector class

Fetch, transform and build the predicted LHT dataframe for multiple
species obtained from FishLife

## Value

dataframe that keeps the LHT values per taxon

subset matrix of LHT values

## Super class

[`LHTpicker::MixinUtilities`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.md)
-\> `PredictedLHTPicker`

## Methods

### Public methods

- [`PredictedLHTPicker$new()`](#method-PredictedLHTPicker-new)

- [`PredictedLHTPicker$pick_and_backtransform()`](#method-PredictedLHTPicker-pick_and_backtransform)

- [`PredictedLHTPicker$clone()`](#method-PredictedLHTPicker-clone)

Inherited methods

- [`LHTpicker::MixinUtilities$apply_func_to_df()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-apply_func_to_df)
- [`LHTpicker::MixinUtilities$create_empty_dataframe()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-create_empty_dataframe)
- [`LHTpicker::MixinUtilities$list_of_vectors_to_dataframe()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-list_of_vectors_to_dataframe)

------------------------------------------------------------------------

### Method `new()`

Initialise the PredictedLHTPicker

#### Usage

    PredictedLHTPicker$new(
      master_db,
      lht_names,
      backtransform_function_list,
      wanted_lht_df
    )

#### Arguments

- `master_db`:

  Fishlife database

- `lht_names`:

  list of user-defined LHT names associated with their FishLife's
  counterparts

- `backtransform_function_list`:

  list of backward-transformation functions to be applied on obtained
  LHT from FishLife

- `wanted_lht_df`:

  dataframe holding the wanted taxa details as rows and LHT as columns

------------------------------------------------------------------------

### Method `pick_and_backtransform()`

Pick and backtransform all LHT values per taxon that have beed obtained
from Fishlife

#### Usage

    PredictedLHTPicker$pick_and_backtransform()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PredictedLHTPicker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
