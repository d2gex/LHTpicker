# Multiple Taxon Update Extractor class

Class that extracts and transform predicted values of LHTs for multiple
species obtained from FishLife

## Value

dataframe that keeps the original and updated LHT values per taxon

updated dataframe of results with both the original and updated LHT
values

## Super class

[`LHTpicker::MixinUtilities`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.md)
-\> `TaxaUpdatedLHTBuilder`

## Methods

### Public methods

- [`TaxaUpdatedLHTBuilder$new()`](#method-TaxaUpdatedLHTBuilder-new)

- [`TaxaUpdatedLHTBuilder$extract_and_backtransform()`](#method-TaxaUpdatedLHTBuilder-extract_and_backtransform)

- [`TaxaUpdatedLHTBuilder$clone()`](#method-TaxaUpdatedLHTBuilder-clone)

Inherited methods

- [`LHTpicker::MixinUtilities$apply_func_to_df()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-apply_func_to_df)
- [`LHTpicker::MixinUtilities$create_empty_dataframe()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-create_empty_dataframe)
- [`LHTpicker::MixinUtilities$list_of_vectors_to_dataframe()`](https://d2gex.github.io/LHTpicker/reference/MixinUtilities.html#method-list_of_vectors_to_dataframe)

------------------------------------------------------------------------

### Method `new()`

Initialise the TaxaPredictionExtractor

#### Usage

    TaxaUpdatedLHTBuilder$new(
      update_prefix,
      lht_names,
      backtransform_function_list,
      predicting_lht_df,
      updated_lht_list
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

- `predicting_lht_df`:

  dataframe keeping the original inputted LHTs per taxon

- `updated_lht_list`:

  list of updated LHTs per taxon obtained from Fishlife

------------------------------------------------------------------------

### Method `extract_and_backtransform()`

Extract and backtransform all updated LHT values per taxon that have
beed obtained from Fishlife

#### Usage

    TaxaUpdatedLHTBuilder$extract_and_backtransform()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaxaUpdatedLHTBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
