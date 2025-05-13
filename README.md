# bioclima Package

## Bioclimatic variables using terra

## Overview

The `bioclima` package is an R package designed to efficiently generate bioclimatic variables using the `terra` package instead of `raster`. This choice of dependency makes the process significantly faster, as `terra` is optimized for large-scale raster operations. The `bioclima` package also have available the option to create a subset of variables, without requiring to build all of them. It can build the bioclimatic variables based on solar radiation and precipitation (bio20-35 following ANUCLIM numeration). Also, it has available the option to create bioclimatic variables base on other periods of time (e.g., weeks, days), define other periods than quarters (used for bio08, 09, 10, 11, 16, 17, 18, 19, 24, 25, 26, 27, 32, 33, 34, 35). Or even use real average temperature (parameter 'tavg'), instead that an approximation based on the maximum and the minimum temperature ((tmax + tmin)) / 2).

The functionality provided by `bioclima` is inspired by the `biovars` function in the `dismo` package. The goal is to streamline the process of creating bioclimatic variables for ecological and environmental modeling.

**Disclaimer: This Package is Under Development**

This R package is currently in development and may contain errors, bugs, or incomplete features.

## Important Notes

-   Use this package at your own risk. The development team does not guarantee the stability or correctness of the code.

-   Contributions and bug reports are welcome. If you encounter issues or have suggestions for improvement, please [open an issue](https://github.com/gepinillab/bioclima/issues) on our GitHub repository.

Thank you for your understanding and collaboration as we work towards improving and stabilizing this R package.

## Installation

To install `bioclima`, you can use the `remotes` package. If you don't have it installed, you can do so by running:

``` r
install.packages("remotes")
remotes::install_github("gepinillab/bioclima")
```

## Example

Install and load libraries

``` r
# Load libraries AND install them if necessary
if (!require("climateR")) {
  remotes::install_github("mikejohnson51/climateR") }
if (!require("AOI")) {
  remotes::install_github("mikejohnson51/AOI") }
if (!require("terra")) {
  install.packages("terra") }
if (!require("here")) {
  install.packages("here") }
if (!require("magrittr")) {
  install.packages("magrittr") }
if (!require("bioclima")) {
  remotes::install_github("gepinillab/bioclima") }
```

### Get monthly data

Download monthly climatic variables from [Terraclimate](https://www.climatologylab.org/terraclimate.html) database using the `climateR` package. These rasters have a spatial resolution of 2.5 arcmins and a temporal resolution of from 1958 to 2022 (each year the database is updated to include last year). The follwing example will create bioclimatic variables for El Salvador summarizing a perido of 30-years.

``` r
# Download data from El Salvador (30 years of monthly data; 360 months)
env.data <- getTerraClim(AOI = aoi_get(country = "El Salvador"),  # Area of Interest
                         varname =  c("tmin", "tmax", "ppt", "soil"),  # Variables to download
                         startDate = "1990-01-01",  # Lower limit date
                         endDate = "2019-12-01")  # Upper limit date
# Save monthly minumum temperature
tmin <- env.data$tmin
# Save monthly maximum temperature
tmax <- env.data$tmax
# Save monthly precipitation
ppt <- env.data$ppt
# Save monthly solar radiation
# srad <- env.data$srad
# Save monthly moisture
mois <- env.data$soil
```

Please check the raster inputs before saving the rasters. In Nov 15th, 2023 the climatic data obtained was flipped. Fortunately, it can be fixed using this code.

``` r
# Plot first month (1990-01) of precipitation
# Check if map if flipped
plot(ppt[[1]])
```

``` r
# RUN THIS CODE IF THE MAP VALUES ARE FLIPPED
# tmin <- rast(x = tmin, vals = values(t(tmin)))
# tmax <- rast(x = tmax, vals = values(t(tmax)))
# ppt <- rast(x = ppt, vals = values(t(ppt)))
# srad <- rast(x = srad, vals = values(t(srad)))
# mois <- rast(x = mois, vals = values(t(mois)))
```

Then, you will be able to save the monthly rasters in a unique TIFF file. Each month will be saved in the bands of a raster, so you do not need to save each month individually.

``` r
# Create a folder
dir.create(here("data"))
# Save each spatRaster
terra::writeRaster(tmin,
                   here("data", "tmin_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
terra::writeRaster(tmax,
                   here("data", "tmax_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
terra::writeRaster(ppt,
                   here("data", "ppt_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
# terra::writeRaster(srad,
#                    here("data", "srad_1990-01-to-2019-12.tif"),
#                    overwrite = TRUE)
terra::writeRaster(mois,
                   here("data", "mois_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
```

### Create bioclimatic variables

Once, you get the monthly variables. It is time to create the bioclimatic variables based on the monthly averages (*n* = 12 months). First, we need to create a monthly averages for each variable, and then use the clima() function to obtain the 19 bioclimatic variables.

``` r
tmin_avg <- tapp(tmin, index = rep(1:12, 30), fun = mean, na.rm = TRUE)
tmax_avg <- tapp(tmax, index = rep(1:12, 30), fun = mean, na.rm = TRUE)
ppt_avg <- tapp(ppt, index = rep(1:12, 30), fun = mean, na.rm = TRUE)
# srad_avg <- tapp(srad, index = rep(1:12, 30), fun = mean, na.rm = TRUE)
mois_avg <- tapp(mois, index = rep(1:12, 30), fun = mean, na.rm = TRUE)

bios <- bioclima::clima(tmin = tmin_avg, tmax = tmax_avg, prcp = ppt_avg,
                        srad = srad_avg, mois = mois_avg,
                        bios = c(1:19, 28:35), period = 3, circular = TRUE)
```

### Plot variables

``` r
# Temperature
plot(bios[[1:11]])
```

``` r
# Precipitation
plot(bios[[12:19]])
```

``` r
# Solar radiation
plot(bios[[20:27]])
```

``` r
# Moisture
plot(bios[[28:35]])
```

## Citation

As November 2023, there is not a package-specific citation for `bioclima`, but the package was first used in:

Pinilla-Buitrago, G. E. (2023). Predicting potential range shifts using climatic time series and niche models: A Neotropical montane shrew's case. Ecological Informatics, 77, 102212. <https://doi.org/10.1016/j.ecoinf.2023.102212>

### Abstract

Ecological niche models (ENMs) can forecast species' potential range shifts by transferring a model to a future climatic scenario. However, this approximation does not identify whether range shifts have occurred in species' distributional limits in the last several decades. Given ongoing anthropogenic climate change, changes in distributional limits are expected to occur (poleward and upslope for several montane species). Here, I use a climatic time series to predict potential changes in distributional limits in response to climate change over the last four decades, using the Mexican small-eared shrew (*Cryptotis mexicanus*), a montane cloud-forest species, as an example. I transferred the ENM (tuned to optimize complexity) to a series of thirty-year bioclimatic periods between 1979 and 2019 created with monthly precipitation and temperature data. I detected trends in suitability and bioclimatic variables using a Mann-Kendall test and identified which variables could be driving the suitability changes in distributional limits. The detected extent and direction of suitability trends do not suggest the uniform pole-ward or upslope shifts expected under warming conditions. Comparisons between suitability and variable trends suggest that precipitation, not temperature, plays a stronger role in explaining changes in climatic suitability for *C. mexicanus*. Where precipitation decreased, there was suitability loss, while in areas where precipitation increased, suitability gain was observed. These analyses illustrate how incorporating time-series climatic data into ENMs can aid in understanding if species are already responding to changes in climate. Here, the potential response of C. mexicanus to climate change varies across its distribution and therefore the species' range shifts may not necessarily follow expected general patterns. These findings should be verified with field data. More generally, using the valuable climatic information of the last several decades should be incorporated into studies that determine potential range shifts under recent or future climatic conditions.
