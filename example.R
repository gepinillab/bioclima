#remotes::install_github("mikejohnson51/AOI") # suggested!
#remotes::install_github("mikejohnson51/climateR")
library(climateR)
library(AOI)
library(terra)
library(here)
library(magrittr)
library(bioclima)
env.data <- getTerraClim(AOI = aoi_get("Colombia"),  # Area of Interest
                         param = c("tmin", "tmax", "prcp"),  # Variables to download
                         startDate = "1990-01-01",  # Lower limit date
                         endDate = "2019-12-31")
terra::writeRaster(env.data$terraclim_tmin,
                   here("data", "tmin_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
terra::writeRaster(env.data$terraclim_tmax,
                   here("data", "tmax_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
terra::writeRaster(env.data$terraclim_prcp,
                   here("data", "prcp_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)

tmin <- rast(here("data", "tmin_1990-01-to-2019-12.tif")) %>% 
  tapp(., index = rep(1:12, 30), fun = mean)
tmax <- rast(here("data", "tmax_1990-01-to-2019-12.tif")) %>% 
  tapp(., index = rep(1:12, 30), fun = mean)
prcp <- rast(here("data", "prcp_1990-01-to-2019-12.tif")) %>%
  tapp(., index = rep(1:12, 30), fun = mean, na.rm = TRUE)

bios <- bioclima::clima(tmin = tmin, tmax = tmax, prcp = prcp,
              bios = c(1:19), period = 3, circular = TRUE)
