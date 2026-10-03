# Multiple Updated LHT Generator class

Allows the streamline-generation of updated LHTs given new data.

## Value

list of LHT matrices, where each name in the list corresponds a single
taxon

a unnamed list of LHT values per taxon

renamed list of lht which names conform to Fishlife's expected patterns

list of LHT values (another list) for each taxon

## Methods

### Public methods

- [`TaxaUpdatedLHTGetter$new()`](#method-TaxaUpdatedLHTGetter-new)

- [`TaxaUpdatedLHTGetter$update()`](#method-TaxaUpdatedLHTGetter-update)

- [`TaxaUpdatedLHTGetter$clone()`](#method-TaxaUpdatedLHTGetter-clone)

------------------------------------------------------------------------

### Method `new()`

Initialise the Updated LHT Generator class

#### Usage

    TaxaUpdatedLHTGetter$new(
      master_db,
      predicting_lht_df,
      transform_function_list,
      lht_names
    )

#### Arguments

- `master_db`:

  Fishlife database

- `predicting_lht_df`:

  dataframe providing the taxon details and their predicting LHT values

- `transform_function_list`:

  list of transforming function which names must conform to FishLife's
  expectations

- `lht_names`:

  list of user-defined LHT names associated with their FishLife's
  counterparts

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the given LHTs per taxon according to the new predicted value
yielded by FishLfe

#### Usage

    TaxaUpdatedLHTGetter$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaxaUpdatedLHTGetter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
