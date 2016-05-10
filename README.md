# rzonation

[![Build Status](https://travis-ci.org/wkmor1/rzonation.svg?branch=master)](https://travis-ci.org/wkmor1/rzonation)

rzonation is an R package for running the [Zonation](http://cbig.it.helsinki.fi/software/zonation/) conservation planning software via R.

## Example usage

```
install.packages("devtools")

devtools::install_github("wkmor1/rzonation")

library(rzonation)

r1 <- raster(matrix(runif(200^2, 0, 1), 200))

r2 <- raster(matrix(runif(200^2, 0, 1), 200))

plan <- zonation(stack(r1, r2))
```
