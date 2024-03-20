FishlifeLHTNameSpace <- R6::R6Class("FishlifeLHTNameSpace", public = list(

  updated_prefix = NULL,
  lht_names = NULL,
  lht_results = NULL,
  transform_function = NULL,
  backtransform_function = NULL,
  initialize = function() {
  }
))
fishlife_context <- FishlifeLHTNameSpace$new()
# User-defined LHT names to fishlife's nomencluture for input LHTs
fishlife_context$updated_prefix <- 'updated'
fishlife_context$lht_names <- list(
  Linf = "log(length_infinity)",
  Winf = "log(weight_infinity)",
  K = "log(growth_coefficient)",
  M = "log(natural_mortality)",
  L50 = "log(length_maturity)",
  Amax = "log(age_max)",
  Amat = "log(age_maturity)",
  Temperature = "temperature"
)
# Function conversion required for each parameter. See that the left hand side variables coincnide with
# the right hand side (names) of your LHTNames list
fishlife_context$transform_function <- list(
  "log(length_infinity)" = log,
  "log(weight_infinity)" = log,
  "log(growth_coefficient)" = log,
  "log(natural_mortality)" = log,
  "log(length_maturity)" = log,
  "log(age_max)" = log,
  "log(age_maturity)" = log,
  "temperature" = base::identity
)
# User-defined LHT names to fishlife's nomencluture for output LHTs
fishlife_context$lht_results <- list(
  Linf = "log(length_infinity)",
  Winf = "log(weight_infinity)",
  K = "log(growth_coefficient)",
  M = "log(natural_mortality)",
  L50 = "log(length_maturity)",
  Amax = "log(age_max)",
  Amat = "log(age_maturity)",
  Temperature = "temperature"
)
fishlife_context$backtransform_function <- list(
  "log(length_infinity)" = exp,
  "log(weight_infinity)" = exp,
  "log(growth_coefficient)" = exp,
  "log(natural_mortality)" = exp,
  "log(length_maturity)" = exp,
  "log(age_max)" = exp,
  "log(age_maturity)" = exp,
  "temperature" = base::identity
)
