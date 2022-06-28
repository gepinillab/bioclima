library(climateR)
library(AOI)
library(terra)
library(here)
env.data <- getTerraClim(AOI = aoi_get("Colombia"),  # Area of Interest
                         param = c("tmin", "tmax", "prcp"),  # Variables to download
                         startDate = "1990-01-01",  # Lower limit date
                         endDate = "2019-12-31")
terra::writeRaster(env.data$tmin,
                   here("datos", "tmin_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
terra::writeRaster(env.data$tmax,
                   here("datos", "tmax_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)
terra::writeRaster(env.data$prcp,
                   here("datos", "prcp_1990-01-to-2019-12.tif"),
                   overwrite = TRUE)

tmin <- rast(here("datos", "tmin_1990-01-to-2019-12.tif")) %>%
  tapp(., index = rep(1:12, 30), fun = mean)
tmax <- rast(here("datos", "tmax_1990-01-to-2019-12.tif")) %>%
  tapp(., index = rep(1:12, 30), fun = mean)
prcp <- rast(here("datos", "prcp_1990-01-to-2019-12.tif")) %>%
  tapp(., index = rep(1:12, 30), fun = mean, na.rm = TRUE)

bios <- clima(tmin = tmin, tmax = tmax, prcp = prcp,
              bios = c(1:19), period = 3, circular = TRUE)
