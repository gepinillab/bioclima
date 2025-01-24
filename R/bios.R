# Function to create analogous bioclimatic variables
# ----------
# P1. Period Mean Temperature
#' @export
bio_01 <- function(tavg) {
  ata <- terra::app(tavg, mean, na.rm = TRUE)
  names(ata) <- "bio01"
  return(ata)
}

# P2. Mean Diurnal Range(Mean(period max-min))
#' @export
bio_02 <- function(tmin, tmax) {
  bosa <- terra::app(tmax - tmin, mean, na.rm = TRUE)
  names(bosa) <- "bio02"
  return(bosa)
}

# P3. Isothermality (P2 / P7)
#' @export
bio_03 <- function(bio02, bio07) {
   mica <- 100 * bio02 / bio07
  names(mica) <- "bio03"
  return(mica)
}

# P4. Temperature Seasonality (standard deviation)
#' @export
bio_04 <- function(tavg) {
  muihica <- 100 * terra::stdev(tavg, pop = FALSE, na.rm = TRUE)
  names(muihica) <- "bio04"
  return(muihica)
}

# P5. Max Temperature of Warmest Period
#' @export
bio_05 <- function(tmax) {
  hisca <- terra::app(tmax, max, na.rm = TRUE)
  names(hisca) <- "bio05"
  return(hisca)
}

# P6. Min Temperature of Coldest Period
#' @export
bio_06 <- function(tmin) {
  ta <- terra::app(tmin, min, na.rm = TRUE)
  names(ta) <- "bio06"
  return(ta)
}

# P7. Temperature Periodical Range (P5-P6)
#' @export
bio_07 <- function(bio05, bio06) {
  cuhupcua <- bio05 - bio06
  names(cuhupcua) <- "bio07"
  return(cuhupcua)
}

# P08. Mean Temperature of Wettest Period
#' @export
bio_08 <- function(tmp, wet) {
  suhusa <- terra::selectRange(tmp, terra::which.max(wet))
  names(suhusa) <- "bio08"
  return(suhusa)
}

# P09. Mean Temperature of Driest Period
#' @export
bio_09 <- function(tmp, wet) {
  aca <- terra::selectRange(tmp, terra::which.min(wet))
  names(aca) <- "bio09"
  return(aca)
}

# P10. Mean Temperature of Warmest Period
#' @export
bio_10 <- function(tmp) {
  ubchihica <- terra::app(tmp, max, na.rm = TRUE)
  names(ubchihica) <- "bio10"
  return(ubchihica)
}

# P11. Mean Temperature of Coldest Quarter
#' @export
bio_11 <- function(tmp) {
  quihicha_ata <- terra::app(tmp, min, na.rm = TRUE)
  names(quihicha_ata) <- "bio11"
  return(quihicha_ata)
}

# P12. Periodical Precipitation
#' @export
bio_12 <- function(prcp) {
  quihicha_bosa <- terra::app(prcp, sum, na.rm = TRUE)
  names(quihicha_bosa) <- "bio12"
  return(quihicha_bosa)
}

# P13. Precipitation of Wettest Period
#' @export
bio_13 <- function(prcp) {
  quihicha_mica <- terra::app(prcp, max, na.rm = TRUE)
  names(quihicha_mica) <- "bio13"
  return(quihicha_mica)
}

# P14. Precipitation of Driest Period
#' @export
bio_14 <- function(prcp) {
  quihicha_muihica <- terra::app(prcp, min, na.rm = TRUE)
  names(quihicha_muihica) <- "bio14"
  return(quihicha_muihica)
}

# P15. Precipitation Seasonality (Coefficient of Variation)
# the "1 +" is to avoid strange CVs for areas where mean rainfaill is < 1)
#' @export
bio_15 <- function(prcp) {
  quihicha_hisca <- cv_cli(prcp)
  names(quihicha_hisca) <- "bio15"
  return(quihicha_hisca)
}

# P16. Precipitation of Wettest Period
#' @export
bio_16 <- function(wet) {
  quihicha_ta <- terra::app(wet, max, na.rm = TRUE)
  names(quihicha_ta) <- "bio16"
  return(quihicha_ta)
}

# P17. Precipitation of Driest Period
#' @export
bio_17 <- function(wet) {
  quihicha_cuhupcua <- terra::app(wet, min, na.rm = TRUE)
  names(quihicha_cuhupcua) <- "bio17"
  return(quihicha_cuhupcua)
}

# P18. Precipitation of Warmest Period
#' @export
bio_18 <- function(tmp, wet) {
  quihicha_suhusa <- terra::selectRange(wet, terra::which.max(tmp))
  names(quihicha_suhusa) <- "bio18"
  return(quihicha_suhusa)
}

# P19. Precipitation of Coldest Period
#' @export
bio_19 <- function(tmp, wet) {
  quihicha_aca <- terra::selectRange(wet, terra::which.min(tmp))
  names(quihicha_aca) <- "bio19"
  return(quihicha_aca)
}
