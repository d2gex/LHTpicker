# Taxa's updated LHT picker

Fetch, transform and build a dataframe with the updated LHT values per
taxon obtained from FishLife

## Value

a dataframe with the predicting and updated LHTs. The latter are
prefixed with a keyword.

## Methods

### Public methods

- [`UpdatedLHTPicker$new()`](#method-UpdatedLHTPicker-new)

- [`UpdatedLHTPicker$pick_and_backtransform()`](#method-UpdatedLHTPicker-pick_and_backtransform)

- [`UpdatedLHTPicker$clone()`](#method-UpdatedLHTPicker-clone)

------------------------------------------------------------------------

### Method `new()`

Initialise the UpdatedLHTPicker class

#### Usage

    UpdatedLHTPicker$new(
      master_db,
      predicting_lht_df,
      updated_prefix,
      transform_function_list,
      backtransform_function_list,
      lht_names = NULL
    )

#### Arguments

- `master_db`:

  Fishlife database

- `predicting_lht_df`:

  dataframe keeping the original inputted LHTs per taxon

- `transform_function_list`:

  list of transforming function which names must conform to FishLife's
  expectations

- `backtransform_function_list`:

  list of backward-transformation functions to be applied on obtained
  LHT from FishLife

- `lht_names`:

  list of user-defined LHT names associated with their FishLife's
  counterparts

- `update_prefix`:

  string to be added as prefix to the column names keeping the obtained
  new LHT values

------------------------------------------------------------------------

### Method `pick_and_backtransform()`

Build a dataframe with the updated LHT for each wanted taxon as

#### Usage

    UpdatedLHTPicker$pick_and_backtransform()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    UpdatedLHTPicker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
