# Function to create analogous bioclimatic variables
# ----------
# P1. Period Mean Temperature
#' @export
ata <- function(tavg) {
  bio01 <- terra::app(tavg, mean, na.rm = TRUE)
  names(bio01) <- "bio01"
  return(bio01)
}

# P2. Mean Diurnal Range(Mean(period max-min))
#' @export
bosa <- function(tmin, tmax) {
  bio02 <- terra::app(tmax - tmin, mean, na.rm = TRUE)
  names(bio02) <- "bio02"
  return(bio02)
}

# P3. Isothermality (P2 / P7)
#' @export
mica <- function(bio02, bio07) {
  bio03 <- 100 * bio02 / bio07
  names(bio03) <- "bio03"
  return(bio03)
}

# P4. Temperature Seasonality (standard deviation)
#' @export
muihica <- function(tavg) {
  bio04 <- 100 * terra::stdev(tavg, pop = FALSE, na.rm = TRUE)
  names(bio04) <- "bio04"
  return(bio04)
}

# P5. Max Temperature of Warmest Period
#' @export
hisca <- function(tmax) {
  bio05 <- terra::app(tmax, max, na.rm = TRUE)
  names(bio05) <- "bio05"
  return(bio05)
}

# P6. Min Temperature of Coldest Period
#' @export
ta <- function(tmin) {
  bio06 <- terra::app(tmin, min, na.rm = TRUE)
  names(bio06) <- "bio06"
  return(bio06)
}

# P7. Temperature Periodical Range (P5-P6)
#' @export
cuhupcua <- function(bio05, bio06) {
  bio07 <- bio05 - bio06
  names(bio07) <- "bio07"
  return(bio07)
}

# P08. Mean Temperature of Wettest Period
#' @export
suhusa <- function(tmp, wet) {
  bio08 <- terra::selectRange(tmp, terra::which.max(wet))
  names(bio08) <- "bio08"
  return(bio08)
}

# P09. Mean Temperature of Driest Period
#' @export
aca <- function(tmp, wet) {
  bio09 <- terra::selectRange(tmp, terra::which.min(wet))
  names(bio09) <- "bio09"
  return(bio09)
}

# P10. Mean Temperature of Warmest Period
#' @export
ubchihica <- function(tmp) {
  bio10 <- terra::app(tmp, max, na.rm = TRUE)
  names(bio10) <- "bio10"
  return(bio10)
}

# P11. Mean Temperature of Coldest Quarter
#' @export
quihicha_ata <- function(tmp) {
  bio11 <- terra::app(tmp, min, na.rm = TRUE)
  names(bio11) <- "bio11"
  return(bio11)
}

# P12. Periodical Precipitation
#' @export
quihicha_bosa <- function(prcp) {
  bio12 <- terra::app(prcp, sum, na.rm = TRUE)
  names(bio12) <- "bio12"
  return(bio12)
}

# P13. Precipitation of Wettest Period
#' @export
quihicha_mica <- function(prcp) {
  bio13 <- terra::app(prcp, max, na.rm = TRUE)
  names(bio13) <- "bio13"
  return(bio13)
}

# P14. Precipitation of Driest Period
#' @export
quihicha_muihica <- function(prcp) {
  bio14 <- terra::app(prcp, min, na.rm = TRUE)
  names(bio14) <- "bio14"
  return(bio14)
}

# P15. Precipitation Seasonality (Coefficient of Variation)
# the "1 +" is to avoid strange CVs for areas where mean rainfaill is < 1)
#' @export
quihicha_hisca <- function(prcp) {
  bio15 <- cv_cli(prcp)
  names(bio15) <- "bio15"
  return(bio15)
}

# P16. Precipitation of Wettest Period
#' @export
quihicha_ta <- function(wet) {
  bio16 <- terra::app(wet, max, na.rm = TRUE)
  names(bio16) <- "bio16"
  return(bio16)
}

# P17. Precipitation of Driest Period
#' @export
quihicha_cuhupcua <- function(wet) {
  bio17 <- terra::app(wet, min, na.rm = TRUE)
  names(bio17) <- "bio17"
  return(bio17)
}

# P18. Precipitation of Warmest Period
#' @export
quihicha_suhusa <- function(tmp, wet) {
  bio18 <- terra::selectRange(wet, terra::which.max(tmp))
  names(bio18) <- "bio18"
  return(bio18)
}

# P19. Precipitation of Coldest Period
#' @export
quihicha_aca <- function(tmp, wet) {
  bio19 <- terra::selectRange(wet, terra::which.min(tmp))
  names(bio19) <- "bio19"
  return(bio19)
}
