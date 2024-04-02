[![Documentation](https://img.shields.io/badge/documentation-LHTpicker-orange.svg?colorB=E91E63)](https://github.com/d2gex/LHTpicker)

# LHTpicker
This is a thin wrapper over [FishLife](https://github.com/James-Thorson-NOAA/FishLife) library that helps to abstract 
the underlying data structure from the final user, easing the fetching and update of local LHTs. 
The following processes have been integrated:

1. Fetch predicted LHTs **'as is'** from FishLife for an individual taxon.
2. Provide predicting LHTs and fetch the **updated** predicted values from FishLife for an individual taxon.
3. Both processes, 1 and 2 can be accomplished in a streamline fashion for multiple taxa via a csv file.

Please have a look at this [tutorial](https://d2gex.github.io/LHTpicker/articles/tutorial.html) to understand how it works.

# Installation
This R package can be installed through the devtools as follows:
```r 
  devtools::install_github("https://github.com/d2gex/LHTpicker", dep=TRUE)
```