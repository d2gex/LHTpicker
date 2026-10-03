# Tutorial: Use Cases

## 1. Install `LHTpicker`

To run this vignette, install `LHTpicker` in one of R’s library paths.

``` r

# devtools::install_github("d2gex/LHTpicker", dependencies = TRUE)
```

## 2. Retrieve LHTs from FishLife

LHTpicker supports two workflows:

1.  Retrieve FishLife predictions when trait values are unavailable.
2.  Update FishLife predictions using a partial set of supplied trait
    values.

This section covers the first workflow.

### 2.1 Input taxa and requested traits

Provide a data frame with a `taxon` column and one column for each
requested LHT. In practice, this can be read from CSV. The
requested-trait columns are `NA` in this example.

``` r

taxon_lhts_to_fetch <- readRDS("data/wanted_taxon_details.rds")
head(taxon_lhts_to_fetch)
#>                   taxon Linf Winf  K L50  M Amat Amax Temperature
#> 1    Trisopterus luscus   NA   NA NA  NA NA   NA   NA          NA
#> 2 Pollachius pollachius   NA   NA NA  NA NA   NA   NA          NA
```

`fishlife_context$lht_names` maps LHTpicker column names to FishLife
field names. You only need to change it when requesting a trait outside
the default set or when FishLife changes a field name.

``` r

LHTpicker::fishlife_context$lht_names
#> $Linf
#> [1] "log(length_infinity)"
#> 
#> $Winf
#> [1] "log(weight_infinity)"
#> 
#> $K
#> [1] "log(growth_coefficient)"
#> 
#> $M
#> [1] "log(natural_mortality)"
#> 
#> $L50
#> [1] "log(length_maturity)"
#> 
#> $Amax
#> [1] "log(age_max)"
#> 
#> $Amat
#> [1] "log(age_maturity)"
#> 
#> $Temperature
#> [1] "temperature"
```

`backtransform_function_list` maps each FishLife field to the function
that converts it back to the user-facing scale. The default list
normally requires no changes.

``` r

LHTpicker::fishlife_context$backtransform_function_list
#> $`log(length_infinity)`
#> function (x)  .Primitive("exp")
#> 
#> $`log(weight_infinity)`
#> function (x)  .Primitive("exp")
#> 
#> $`log(growth_coefficient)`
#> function (x)  .Primitive("exp")
#> 
#> $`log(natural_mortality)`
#> function (x)  .Primitive("exp")
#> 
#> $`log(length_maturity)`
#> function (x)  .Primitive("exp")
#> 
#> $`log(age_max)`
#> function (x)  .Primitive("exp")
#> 
#> $`log(age_maturity)`
#> function (x)  .Primitive("exp")
#> 
#> $temperature
#> function (x) 
#> x
#> <bytecode: 0x55a9bdc34130>
#> <environment: namespace:base>
```

### 2.2 Retrieve predicted LHTs

See the [reference
documentation](https://d2gex.github.io/LHTpicker/reference/PredictedLHTPicker.html)
for the full interface.

``` r

p_lht_picker <- LHTpicker::PredictedLHTPicker$new(FishLife::FishBase_and_Morphometrics,
                                                  LHTpicker::fishlife_context$lht_names,
                                                  LHTpicker::fishlife_context$backtransform_function_list,
                                                  taxon_lhts_to_fetch)
predicted_lht_df <- p_lht_picker$pick_and_backtransform()
head(predicted_lht_df)
#>                   taxon     Linf      Winf         K      L50         M
#> 1    Trisopterus luscus 43.86770  833.9878 0.3737931 19.52203 0.5981946
#> 2 Pollachius pollachius 87.30785 5819.9735 0.1867131 34.71789 0.3085845
#>       Amat      Amax Temperature
#> 1 1.400562  6.458926    17.36961
#> 2 3.456610 12.138258    12.11773
```

If FishLife cannot match a taxon, LHTpicker preserves its input row and
leaves the requested LHT values as `NA`.

``` r

non_existent_taxon_lhts_to_fetch <- dplyr::mutate(taxon_lhts_to_fetch, taxon = dplyr::case_when(
    taxon == "Trisopterus luscus" ~ "IDoNoExist",
    .default = taxon
))
p_lht_picker <- LHTpicker::PredictedLHTPicker$new(FishLife::FishBase_and_Morphometrics,
                                                  LHTpicker::fishlife_context$lht_names,
                                                  LHTpicker::fishlife_context$backtransform_function_list,
                                                  non_existent_taxon_lhts_to_fetch)
predicted_lht_df <- p_lht_picker$pick_and_backtransform()
head(predicted_lht_df)
#> # A tibble: 2 × 9
#>   taxon                  Linf  Winf      K   L50      M  Amat  Amax Temperature
#>   <chr>                 <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>       <dbl>
#> 1 IDoNoExist             NA     NA  NA      NA   NA     NA     NA          NA  
#> 2 Pollachius pollachius  87.3 5820.  0.187  34.7  0.309  3.46  12.1        12.1
```

## 3. Update LHTs with supplied data

This section covers the second workflow: updating FishLife predictions
with supplied data.

### 3.1 Input taxa and supplied traits

Provide a data frame with one taxon per row and the available LHTs in
the remaining columns. In this example, natural mortality (`M`) and age
at maturity (`Amat`) are missing.

``` r

taxon_lhts_to_update <- readRDS("data/wanted_update_taxon_details.rds")
head(taxon_lhts_to_update)
#> # A tibble: 2 × 9
#>   taxon                  Linf  Winf     K   L50 M     Amat   Amax Temperature
#>   <chr>                 <dbl> <dbl> <dbl> <dbl> <lgl> <lgl> <dbl>       <dbl>
#> 1 Trisopterus luscus     42.4   921 0.21   19.4 NA    NA        9        14.3
#> 2 Pollachius pollachius 102.  12045 0.193  41.6 NA    NA        8        14.3
```

### 3.2 Retrieve updated LHTs

Compared with the prediction workflow, this call also needs an `updated`
prefix and a transformation list. LHTpicker prefixes each new LHT column
with `updated_`. The transformation list converts supplied values to
FishLife’s internal scale; the back-transformation list converts the
returned values to the user-facing scale. You normally do not need to
change either list.

``` r


u_lht_picker <- LHTpicker::UpdatedLHTPicker$new(
  FishLife::FishBase_and_Morphometrics,
  taxon_lhts_to_update,
  LHTpicker::fishlife_context$updated_prefix,
  LHTpicker::fishlife_context$transform_function_list,
  LHTpicker::fishlife_context$backtransform_function_list,
  LHTpicker::fishlife_context$lht_names
)
updated_lht_df <- u_lht_picker$pick_and_backtransform()
head(updated_lht_df)
#>                   taxon   Linf  Winf     K   L50  M Amat Amax Temperature
#> 1    Trisopterus luscus  42.41   921 0.210 19.45 NA   NA    9        14.3
#> 2 Pollachius pollachius 102.14 12045 0.193 41.60 NA   NA    8        14.3
#>   updated_Linf updated_Winf updated_K updated_M updated_L50 updated_Amax
#> 1     44.01880     839.5509 0.3451741 0.5872462    19.60257     6.973144
#> 2     95.04985    8387.9049 0.1881105 0.2997824    36.48049    11.111069
#>   updated_Amat updated_Temperature
#> 1     1.438878            16.75299
#> 2     3.413651            13.05912
```

Each new LHT column has the `updated_` prefix; for example, `updated_M`
and `updated_Amat` are now available. For an assessment, use the
complete updated trait set rather than mixing input and updated values,
to preserve the covariance structure among parameters (Thorson et al.,
2017).
