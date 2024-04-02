[![Documentation](https://img.shields.io/badge/documentation-LHTpicker-orange.svg?colorB=E91E63)](https://github.com/d2gex/LHTpicker)

# LHTpicker

This is a thin wrapper over [FishLife](https://github.com/James-Thorson-NOAA/FishLife) library that helps to abstract 
the underlying data structure, easing the fetch and update of local LHTs. The following processes have been integrated:

1. Fetch predicted LHTs **'as is'** from FishLife.
2. Provide predicting LHTs and fetch the **updated** predicted values from FishLife.
3. Both processes, 1 and 2 can be accomplished in a streamline fashion for multiple species via a csv file.

# Installation

This R package can be installed through the devtools as follows:
```r 
  devtools::install_github("https://github.com/d2gex/LHTpicker", dep=TRUE)
```