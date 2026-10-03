[![Documentation](https://img.shields.io/badge/documentation-LHTpicker-orange.svg?colorB=E91E63)](https://github.com/d2gex/LHTpicker)

# LHTpicker
LHTpicker is a thin wrapper around [FishLife](https://github.com/James-Thorson-NOAA/FishLife) for retrieving and updating life-history traits (LHTs) for one or more taxa. It hides FishLife's internal data structures while retaining its prediction workflow.

It supports three tasks:

1. Retrieve FishLife-predicted LHTs for selected taxa.
2. Update FishLife predictions using supplied LHT values.
3. Apply either workflow to multiple taxa in one input table.

Input is a data frame, commonly read from CSV, with one taxon per row. The default mappings cover the LHTs used in the original FishLife publication (Thorson et al., 2017). If you request additional traits or FishLife changes its field names, update the mappings in `fishlife_context` and their corresponding transformation functions.

See the [tutorial](https://d2gex.github.io/LHTpicker/articles/tutorial.html) for complete examples.

## Installation

Install the package with `devtools`:

```r
devtools::install_github("d2gex/LHTpicker", dependencies = TRUE)
```

## References

1. Thorson, J. T., S. B. Munch, J. M. Cope, and J. Gao. 2017. Predicting life history parameters for all fishes worldwide. Ecological Applications. 27(8): 2262–2276. http://onlinelibrary.wiley.com/doi/10.1002/eap.1606/full
2. Thorson, J.T., 2020. Predicting recruitment density dependence and intrinsic growth rate for all fishes worldwide using a data-integrated life-history model. Fish Fish. 21, 237–251. https://doi.org/10.1111/faf.12427
3. Thorson, J.T., Maureaud, A.A., Frelat, R., Mérigot, B., Bigman, J.S., Friedman, S.T., Palomares, M.L.D., Pinsky, M.L., Price, S.A., Wainwright, P., 2023. Identifying direct and indirect associations among traits by merging phylogenetic comparative methods and structural equation models. Methods Ecol. Evol. n/a. https://doi.org/10.1111/2041-210X.14076
