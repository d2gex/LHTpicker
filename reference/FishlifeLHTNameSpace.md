# Fishlife Context class

Class that allows the transition between user-defined LHT names and
those specifically used by Fishlife

## Public fields

- `updated_prefix`:

  string added to each obtained LHT from FishLife

- `lht_names`:

  User-defined list of LHT names associated with the expected FishLife
  names

- `transform_function_list`:

  Forward function conversion required for each user-defined parameter.

- `backtransform_function_list`:

  Backward function conversion required for each obtained LHT value.

## Methods

### Public methods

- [`FishlifeLHTNameSpace$new()`](#method-FishlifeLHTNameSpace-new)

- [`FishlifeLHTNameSpace$clone()`](#method-FishlifeLHTNameSpace-clone)

------------------------------------------------------------------------

### Method `new()`

Initialise Fishlife Context class

#### Usage

    FishlifeLHTNameSpace$new(
      updated_prefix = "updated",
      lht_names = list(Linf = "log(length_infinity)", Winf = "log(weight_infinity)", K =
        "log(growth_coefficient)", M = "log(natural_mortality)", L50 =
        "log(length_maturity)", Amax = "log(age_max)", Amat = "log(age_maturity)",
        Temperature = "temperature"),
      transform_function_list = list(`log(length_infinity)` = log, `log(weight_infinity)` =
        log, `log(growth_coefficient)` = log, `log(natural_mortality)` = log,
        `log(length_maturity)` = log, `log(age_max)` = log, `log(age_maturity)` = log,
        temperature = base::identity),
      backtransform_function_list = list(`log(length_infinity)` = exp, `log(weight_infinity)`
        = exp, `log(growth_coefficient)` = exp, `log(natural_mortality)` = exp,
        `log(length_maturity)` = exp, `log(age_max)` = exp, `log(age_maturity)` = exp,
        temperature = base::identity)
    )

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FishlifeLHTNameSpace$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
