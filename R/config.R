#' Fishlife Context class
#'
#' @description
#' Class that allows the transition between user-defined LHT names and those specifically used by Fishlife
FishlifeLHTNameSpace <- R6::R6Class("FishlifeLHTNameSpace", public = list(

  # // @formatter:off
  #' @field updated_prefix string added to each obtained LHT from FishLife
  # // @formatter:on
  updated_prefix = NULL,
  # // @formatter:off
  #' @field lht_names User-defined list of LHT names associated with the expected FishLife names
  # // @formatter:on
  lht_names = NULL,
  # // @formatter:off
  #' @field transform_function Forward function conversion required for each user-defined parameter.
  # // @formatter:on
  transform_function = NULL,
  # // @formatter:off
  #' @field backtransform_function Backward function conversion required for each obtained LHT value.
  # // @formatter:on
  backtransform_function = NULL,
    #' @description
    #' Initialise Fishlife Context class
    #'
    #' @export
  initialize = function(updated_prefix = 'updated',
                        lht_names = list(
                          Linf = "log(length_infinity)",
                          Winf = "log(weight_infinity)",
                          K = "log(growth_coefficient)",
                          M = "log(natural_mortality)",
                          L50 = "log(length_maturity)",
                          Amax = "log(age_max)",
                          Amat = "log(age_maturity)",
                          Temperature = "temperature"
                        ),
                        transform_function = list(
                          "log(length_infinity)" = log,
                          "log(weight_infinity)" = log,
                          "log(growth_coefficient)" = log,
                          "log(natural_mortality)" = log,
                          "log(length_maturity)" = log,
                          "log(age_max)" = log,
                          "log(age_maturity)" = log,
                          "temperature" = base::identity
                        ),
                        backtransform_function = list(
                          "log(length_infinity)" = exp,
                          "log(weight_infinity)" = exp,
                          "log(growth_coefficient)" = exp,
                          "log(natural_mortality)" = exp,
                          "log(length_maturity)" = exp,
                          "log(age_max)" = exp,
                          "log(age_maturity)" = exp,
                          "temperature" = base::identity
                        )) {
    self$updated_prefix <- updated_prefix
    self$lht_names <- lht_names
    self$transform_function <- transform_function
    self$backtransform_function <- backtransform_function
  }
))

#' @export
fishlife_context <- FishlifeLHTNameSpace$new()
