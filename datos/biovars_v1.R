# library(terra)
# # Names for layers
# nombres <- paste0("x", rep(seq(1958, 2019, by = 1), each = 12), "_",
#                   sprintf('%0.2d', 1:12))
# # Load spatRaters
# prcp <- terra::rast("C:/Users/Gonzalo/Desktop/rrr/d-enm/data/raw/raster/prcp_1958-01-to-2019-12.tif")
# tmin <- terra::rast("C:/Users/Gonzalo/Desktop/rrr/d-enm/data/raw/raster/tmin_1958-01-to-2019-12.tif")
# tmax <- terra::rast("C:/Users/Gonzalo/Desktop/rrr/d-enm/data/raw/raster/tmax_1958-01-to-2019-12.tif")
# # Rename rasters
# names(prcp) <- nombres
# names(tmin) <- nombres
# names(tmax) <- nombres
# # Subset rasters
# prcp <- prcp[[1:12]]
# tmin <- tmin[[1:12]]
# tmax <- tmax[[1:12]]
# # Check if layers has the same lenght
# if (terra::nlyr(prcp) != terra::nlyr(tmin) | terra::nlyr(tmin) != terra::nlyr(tmax)) {
#   stop('Variables should have same number of layers')
# }
# # Copy and paste from terra::Evaluate whether two SpatRaster objects have the
# # same extent, number of rows and columns, projection, resolution, and origin
# # (or a subset of these comparisons).
# if (!terra::compareGeom(prcp, tmin) | !terra::compareGeom(tmin, tmax)) {
#   stop('Variables should have same geographic parameters')
# }
# # function to check if there is a mismatch in NA values. If there is one pixel different, so TRUE!
# NAmismatch <- function(layer) {
#   num_lyr <- terra::nlyr(layer)
#   r <- unique(as.vector(sum(!is.na(layer))))
#   any(r != 0 & r != num_lyr)
# }
# # Check if all variables share smae NA pixels
# if (NAmismatch(prcp + tmin + tmax)) {
#   stop('Variables should have same NA values')
# }

# tavg <- (tmin + tmax) / 2
# P1. Annual Mean Temperature
# bio01 <- terra::app(tavg, mean)
# P2. Mean Diurnal Range(Mean(period max-min))
# bio02 <- terra::app(tmax - tmin, mean)
# P4. Temperature Seasonality (standard deviation)
# bio04 <- 100 * terra::app(tavg, stats::sd)
# P5. Max Temperature of Warmest Period
# bio05 <- terra::app(tmax, max)
# P6. Min Temperature of Coldest Period
# bio06 <- terra::app(tmin, min)
# P7. Temperature Annual Range (P5-P6)
# bio07 <- bio05 - bio06
# P3. Isothermality (P2 / P7)
# bio03 <- 100 * bio02 / bio07
# P12. Annual Precipitation
# bio12 <- terra::app(prcp, sum)
# P13. Precipitation of Wettest Period
# bio13 <-  terra::app(prcp, max)
# P14. Precipitation of Driest Period
# bio14 <-  terra::app(prcp, min)
# P15. Precipitation Seasonality(Coefficient of Variation)
# the "1 +" is to avoid strange CVs for areas where mean rainfaill is < 1)
# bio15 <- terra::app(prcp + 1, raster::cv)
#
# window <- function(x, period = 3, circular = TRUE)  {
#   lng <- length(x)
#   if (circular == TRUE) x <- c(x,  x[1:period])
#   m <- matrix(ncol = period, nrow = lng)
#   for (i in 1:period) {
#     m[, i] <- x[i:(lng + i - 1)]
#   }
#   apply(m, MARGIN = 1, FUN = sum)
# }

# # precip by quarter (3 months)
# wet <- terra::app(prcp, window, period = 3)
# # P16. Precipitation of Wettest Quarter
# bio16 <- terra::app(wet, max)
# P17. Precipitation of Driest Quarter
# bio17 <- terra::app(wet, min)
# mean temp by quarter (3 months)
# tmp <- terra::app(tavg, window) / 3
# P10 Mean Temperature of Warmest Quarter
# bio10 <- terra::app(tmp, max)
# P11 Mean Temperature of Coldest Quarter
# bio11 <- terra::app(tmp, min)
#
# wet_mat <- terra::values(wet)
# tmp_mat <- terra::values(tmp)
#
# # P8. Mean Temperature of Wettest Quarter
# wetqrt <- as.integer(apply(wet_mat, 1, FUN = which.max))
# v08 <- c()
# for (i in 1:terra::ncell(tmp)) {
#   v08 <- c(v08, tmp_mat[i, wetqrt[i]])
# }
# bio08 <- tmp[[1]]
# terra::values(bio08) <- v08
#
# # P9. Mean Temperature of Driest Quarter
# dryqrt <- as.integer(apply(wet_mat, 1, FUN = which.min))
# v09 <- c()
# for (i in 1:terra::ncell(tmp)) {
#   v09 <- c(v09, tmp_mat[i, dryqrt[i]])
# }
# bio09 <- tmp[[1]]
# terra::values(bio09) <- v09

# # P18. Precipitation of Warmest Quarter
# hot <- as.integer(apply(tmp_mat, 1, FUN = which.max))
# v18 <- c()
# for (i in 1:terra::ncell(tmp)) {
#   v18 <- c(v18, wet_mat[i, hot[i]])
# }
# bio18 <- tmp[[1]]
# terra::values(bio18) <- v18
#
# # P19. Precipitation of Coldest Quarter
# cold <- as.integer(apply(tmp_mat, 1, FUN = which.min))
# v19 <- c()
# for (i in 1:terra::ncell(tmp)) {
#   v19 <- c(v19, wet_mat[i, cold[i]])
# }
# bio19 <- tmp[[1]]
# terra::values(bio19) <- v19

