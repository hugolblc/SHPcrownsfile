
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SHPcrownsfile

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(devtools)
devtools::install_github("https://github.com/hugolblc/SHPcrownsfile.git",
                         build_vignettes = TRUE,
                         dependencies = TRUE,
                         force = TRUE)
library(SHPcrownsfile)

vignette("update_crowns_file", package = "SHPcrownsfile")
shiny_update_crownsFile()
```
