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

# P20. Annual Mean Radiation
#' @export
bio_20 <- function(srad) {
  gueta <- terra::app(srad, mean, na.rm = TRUE)
  names(gueta) <- "bio20"
  return(gueta)
}

# P21. Highest Period Radiation
#' @export
bio_21 <- function(srad) {
  gueta_ata <- terra::app(srad, max, na.rm = TRUE)
  names(gueta_ata) <- "bio21"
  return(gueta_ata)
}

# P22. Lowest Period Radiation
#' @export
bio_22 <- function(srad) {
  gueta_bosa <- terra::app(srad, min, na.rm = TRUE)
  names(gueta_bosa) <- "bio22"
  return(gueta_bosa)
}

# P23. Radiation Seasonality (Coefficient of Variation)
#' @export
bio_23 <- function(srad) {
  gueta_mica <- cv_cli(srad)
  names(gueta_mica) <- "bio23"
  return(gueta_mica)
}

# P24. Radiation of Wettest Quarter
#' @export
bio_24 <- function(prad, wet) {
  gueta_muihica <- terra::selectRange(prad, terra::which.max(wet))
  names(gueta_muihica) <- "bio24"
  return(gueta_muihica)
}

# P25. Radiation of Driest Quarter
#' @export
bio_25 <- function(prad, wet) {
  gueta_hisca <- terra::selectRange(prad, terra::which.min(wet))
  names(gueta_hisca) <- "bio25"
  return(gueta_hisca)
}

# P26. Radiation of Warmest Quarter
#' @export
bio_26 <- function(prad, tmp) {
  gueta_ta <- terra::selectRange(prad, terra::which.max(tmp))
  names(gueta_ta) <- "bio26"
  return(gueta_ta)
}

# P27. Radiation of Coldest Quarter
#' @export
bio_27 <- function(prad, tmp) {
  gueta_cuhupcua <- terra::selectRange(prad, terra::which.min(tmp))
  names(gueta_cuhupcua) <- "bio27"
  return(gueta_cuhupcua)
}

# P28. Annual Mean Soil Moisture
#' @export
bio_28 <- function(soilm) {
  gueta_suhusa <- terra::app(soilm, mean, na.rm = TRUE)
  names(gueta_suhusa) <- "bio28"
  return(gueta_suhusa)
}

# P29. Highest Period Soil Moisture
#' @export
bio_29 <- function(soilm) {
  gueta_aca <- terra::app(soilm, max, na.rm = TRUE)
  names(gueta_aca) <- "bio29"
  return(gueta_aca)
}

# P30. Lowest Period Soil Moisture
#' @export
bio_30 <- function(soilm) {
  gueta_ubchihica <- terra::app(soilm, min, na.rm = TRUE)
  names(gueta_ubchihica) <- "bio30"
  return(gueta_ubchihica)
}

# P31. Soil Moisture Seasonality (Coefficient of Variation)
#' @export
bio_31 <- function(soilm) {
  gueta_quihicha_ata <- cv_cli(soilm)
  names(gueta_quihicha_ata) <- "bio31"
  return(gueta_quihicha_ata)
}

# P32. Mean Soil Moisture of the Most Moist Quarter
#' @export
bio_32 <- function(psoil) {
  gueta_quihicha_bosa <- terra::app(psoil, max, na.rm = TRUE)
  names(gueta_quihicha_bosa) <- "bio32"
  return(gueta_quihicha_bosa)
}

# P33. Mean Soil Moisture of the Least Moist Quarter
#' @export
bio_33 <- function(psoil) {
  gueta_quihicha_mica <- terra::app(psoil, min, na.rm = TRUE)
  names(gueta_quihicha_mica) <- "bio33"
  return(gueta_quihicha_mica)
}

# P34. Mean Soil Moisture of Warmest Quarter
#' @export
bio_34 <- function(psoil, tmp) {
  gueta_quihicha_muihica <- terra::selectRange(psoil, terra::which.max(tmp))
  names(gueta_quihicha_muihica) <- "bio34"
  return(gueta_quihicha_muihica)
}

# P35. Mean Soil Moisture of Coldest Quarter
#' @export
bio_35 <- function(psoil, tmp) {
  gueta_quihicha_hisca <- terra::selectRange(psoil, terra::which.min(tmp))
  names(gueta_quihicha_hisca) <- "bio35"
  return(gueta_quihicha_hisca)
}