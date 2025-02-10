[![Documentation](https://img.shields.io/badge/documentation-LHTpicker-orange.svg?colorB=E91E63)](https://github.com/d2gex/LHTpicker)

# LHTpicker
This is a thin wrapper over [FishLife](https://github.com/James-Thorson-NOAA/FishLife) library that helps to abstract 
the underlying data structure from the final user, easing the fetching and update of Life History Traits (LHTs). 
The following processes have been integrated:

1. Fetch predicted LHTs from FishLife for an individual taxon.
2. Provide FishLife with '*predicting*' LHTs and fetch the **updated** predicted values back from FishLife 
for an individual taxon.
3. Streamline both processes aforementioned for multiple taxa.

A CSV input is used in all cases. **Note** that this wrapper has been tested only with LHTs included 
in the original version of the software (Thorson et al., 2017), while utilising the latest FishLife interface 
(Thorson et al., 2023). Consequently, it is likely compatible with additional parameters introduced in subsequent updates. 
Since 2017, new predicted parameters of various types have been incrementally added (Thorson et al., 2017, 2023; Thorson, 2020)."

"The wrapper is designed to handle potential name changes in predicted variables resulting from FishLife updates. 
To ensure compatibility, simply provide `LHTpicker` with a mapping of the updated field names." Please have a look at 
this [tutorial](https://d2gex.github.io/LHTpicker/articles/tutorial.html) to understand how the whole thing works.

# Installation
This R package can be installed through the devtools as follows:
```r 
  devtools::install_github("https://github.com/d2gex/LHTpicker", dep=TRUE)
```

# 3 References

1. Thorson, J. T., S. B. Munch, J. M. Cope, and J. Gao. 2017. Predicting life history parameters for all fishes worldwide. Ecological Applications. 27(8): 2262–2276. http://onlinelibrary.wiley.com/doi/10.1002/eap.1606/full
2. Thorson, J.T., 2020. Predicting recruitment density dependence and intrinsic growth rate for all fishes worldwide using a data-integrated life-history model. Fish Fish. 21, 237–251. https://doi.org/10.1111/faf.12427
3. Thorson, J.T., Maureaud, A.A., Frelat, R., Mérigot, B., Bigman, J.S., Friedman, S.T., Palomares, M.L.D., Pinsky, M.L., Price, S.A., Wainwright, P., 2023. Identifying direct and indirect associations among traits by merging phylogenetic comparative methods and structural equation models. Methods Ecol. Evol. n/a. https://doi.org/10.1111/2041-210X.14076