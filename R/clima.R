#' Get several bioclimatic variables
#'
#' @param bios Numeric vector with bios number
#' @param tmin spatRaster with minimum temperature
#' @param tmax spatRaster with maximum temperature
#' @param tavg spatRaster with average temperature. It could be NULL if tmin and
#'  tmax are provided
#' @param prcp spatRaster with precipitation
#' @param period numeric. Length of period to summarize data (e.g., quarters,
#' semesters). If using monthly data, a quarter (3-months) will be use to
#' calculated Bios 08, 09, 10, 11, 16, 17, 18, 19,
#' @param circular TRUE/FALSE. Calculated periods that include first and last
#' units. For example, if using mean monthly data and quarters, circular=TRUE will also
#' calculate Nov-Dec-Jan and Dec-Jan-Feb.
#' @export
#'
clima <- function(bios, tmin = NULL, tmax = NULL, tavg = NULL, prcp = NULL,
                  period = 3, circular = FALSE, checkNA = TRUE, stopNA = TRUE) {
  # Check for same extent, number of rows and columns, projection,
  # resolution, and origin
  sameGeom <- class(purrr::reduce(list(tmin, tmax, tavg, prcp) |>
                                    purrr::discard(is.null),
                                  bioclima::testGeom))
  if (sameGeom == "SpatRaster") {
    message("SpatRasters have same extent, number of rows and columns, ",
            "projection, resolution, and origin")
  }

  # CHECKING INPUTS AVAILABLE
  # Bios that requires prcp
  req_prpc <- c(8, 9, 12, 13, 14, 15, 16, 17, 18, 19)
  if (any(req_prpc %in% bios)) {
    if (is.null(prcp)) {
      stop(paste0(paste0("Bio", sprintf("%02d", req_prpc[req_prpc %in% bios]),
                         collapse = ", "),
                  " require(s) prcp."))
    }
    if (checkNA == TRUE) {
      prcp_na <- mismatch_NA(prcp)
      if (prcp_na$logical == TRUE & stopNA == TRUE) {
        stop("prcp has unexpected NA values")
      }
      prcp_sum <- prcp_na$sum_lyr
    }
  }

  req_temp <- c(1, 2, 3, 4, 7, 8, 9, 10, 11, 18, 19)
  if (any(c(5, 6, req_temp) %in% bios)) {
    # Bios that requires just tmax
    if (5 %in% bios & is.null(tmax)) stop("Bio05 requires tmax.")
    # Bios that requires just tmin
    if (6 %in% bios & is.null(tmin)) stop("Bio06 requires tmin.")
    # Bios that requires tmin and tmax
    if (is.null(tmin) & is.null(tmax)) {
      stop(paste0(paste0("Bio", sprintf("%02d", req_temp[req_temp %in% bios]),
                         collapse = ", "),
                  " require(s) tmin and tmax."))
    }
    # Check for NA (tmax)
    if (any(c(5, req_temp) %in% bios) & checkNA == TRUE) {
      tmax_na <- mismatch_NA(tmax)
      if (tmax_na$logical == TRUE & stopNA == TRUE) {
        stop("tmax has unexpected NA values")
      }
      tmax_sum <- tmax_na$sum_lyr
    }
    # Check for NA (tmin)
    if (any(c(6, req_temp) %in% bios) & checkNA == TRUE) {
      tmin_na <- mismatch_NA(tmin)
      if (tmin_na$logical == TRUE & stopNA == TRUE) {
        stop("tmin has unexpected NA values")
      }
      tmin_sum <- tmin_na$sum_lyr
    }
  }

  if (checkNA == TRUE) {
    # Sum all rasters
    if (!exists("tmin_sum")) tmin_sum <-  NULL
    if (!exists("tmax_sum")) tmax_sum <-  NULL
    if (!exists("prcp_sum")) prcp_sum <-  NULL
    # if (!exists("srad_sum")) srad_sum <-  NULL

    intra_na <- purrr::reduce(list(tmin_sum, tmax_sum, prcp_sum) |>
                                 purrr::discard(is.null), c) |>
      sum() |>
      terra::unique() |>
      unlist()
    # Delete values that shared pixel NA in all layers
    intra_na <- intra_na[intra_na != 0]
    # Check if there is sum of pixel with values is equal
    if (length(intra_na) != 1) {
      if (stopNA == TRUE) {
        stop("SpatRaster don't share same NA values")
      } else {
        message("SpatRasters (tmin, tmax, and/or prcp) don't share same NAs values")
      }
    }
  }

  # Bios that requires tavg
  if (any(c(1, 4, 8, 9, 10, 11, 18, 19) %in% bios)) {
    if (is.null(tavg)) tavg <- bioclima::misqua(tmin = tmin, tmax = tmax)
  }

  # Bio01
  if (1 %in% bios) bio01 <- bioclima::ata(tavg = tavg)
  # Bio02
  if (any(2:3 %in% bios)) bio02 <- bioclima::bosa(tmin = tmin, tmax = tmax)
  # Bio04
  if (4 %in% bios) bio04 <- bioclima::muihica(tavg = tavg)
  # Bio05
  if (any(c(3, 5, 7) %in% bios)) bio05 <- bioclima::hisca(tmax = tmax)
  # Bio06
  if (any(c(3, 6, 7) %in% bios)) bio06 <- bioclima::ta(tmin = tmin)
  # Bio07
  if (any(c(3, 7) %in% bios)) bio07 <- bioclima::cuhupcua(bio05 = bio05, bio06 = bio06)
  # Bio03
  if (3 %in% bios) bio03 <- bioclima::mica(bio02 = bio02, bio07 = bio07)
  # Bio12
  if (12 %in% bios) bio12 <- bioclima::quihicha_bosa(prcp = prcp)
  # Bio13
  if (13 %in% bios) bio13 <- bioclima::quihicha_mica(prcp = prcp)
  # Bio14
  if (14 %in% bios) bio14 <- bioclima::quihicha_muihica(prcp = prcp)
  # Bio15
  if (15 %in% bios) bio15 <- bioclima::quihicha_hisca(prcp = prcp)
  ### Precipitation periods
  if (any(c(8:9, 16:19) %in% bios)) {
    iotuc <- bioclima::ventana(prcp, period, circular)
  }
  # Bio16
  if (16 %in% bios) bio16 <- bioclima::quihicha_ta(wet = iotuc)
  # Bio17
  if (17 %in% bios) bio17 <- bioclima::quihicha_cuhupcua(wet = iotuc)
  ### Mean temperature periods
  if (any(c(8:11, 18:19) %in% bios)) {
    chituc <- bioclima::ventana(tavg, period, circular) / period
  }
  # Window message
  if (any(c(8:11, 16:19) %in% bios)) {
    message(
      paste0(paste0("Bio", sprintf("%02d", c(8:11, 16:19)[c(8:11, 16:19) %in% bios]),
                    collapse = ", "),
             " was(were) built with a period of ", period,
             " units with", if(circular == FALSE) "out", " circularity.")
    )
  }
  # Bio08
  if (8 %in% bios) bio08 <- bioclima::suhusa(tmp = chituc, wet = iotuc)
  # Bio09
  if (9 %in% bios) bio09 <- bioclima::aca(tmp = chituc, wet = iotuc)
  # Bio10
  if (10 %in% bios) bio10 <- bioclima::ubchihica(tmp = chituc)
  # Bio11
  if (11 %in% bios) bio11 <- bioclima::quihicha_ata(tmp = chituc)
  # Bio18
  if (18 %in% bios) bio18 <- bioclima::quihicha_suhusa(tmp = chituc, wet = iotuc)
  # Bio19
  if (19 %in% bios) bio19 <- bioclima::quihicha_aca(tmp = chituc, wet = iotuc)

  # Create a unique spatRaster
  bios_rast <- terra::rast(mget(paste0("bio", sprintf("%02d", bios))))
  # bios_rast <- round(bios_rast, 0)
  return(bios_rast)
}
