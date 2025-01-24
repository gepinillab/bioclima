#' Get several bioclimatic variables
#'
#' @param bios Numeric vector with bios number
#' @param tmin spatRaster with minimum temperature
#' @param tmax spatRaster with maximum temperature
#' @param tavg spatRaster with average temperature. It could be NULL if tmin and
#'  tmax are provided
#' @param prcp spatRaster with precipitation
#' @param srad spatRaster with solar radiation
#' @param soilm spatRaster with soil moisture
#' @param period numeric. Length of period to summarize data (e.g., quarters,
#' semesters). If using monthly data, a quarter (3-months) will be use to
#' calculated Bios 08, 09, 10, 11, 16, 17, 18, 19, 24, 25, 26, 27, 32, 33, 34, 35.
#' @param circular TRUE/FALSE. Calculated periods that include first and last
#' units. For example, if using mean monthly data and quarters, circular=TRUE will also
#' calculate Nov-Dec-Jan and Dec-Jan-Feb.
#' @export
#'
clima <- function(bios, tmin = NULL, tmax = NULL, tavg = NULL, prcp = NULL,
                  srad = NULL, soilm = NULL,
                  period = 3, circular = FALSE, checkNA = TRUE, stopNA = TRUE) {
  # Check for same extent, number of rows and columns, projection,
  # resolution, and origin
  sameGeom <- class(purrr::reduce(list(tmin, tmax, tavg, prcp, srad, soilm) |>
                                    purrr::discard(is.null),
                                  bioclima::testGeom))
  if (sameGeom == "SpatRaster") {
    message("SpatRasters have same extent, number of rows and columns, ",
            "projection, resolution, and origin")
  }

  # CHECKING INPUTS AVAILABLE
  # Bios that requires prcp
  req_prpc <- c(8, 9, 12, 13, 14, 15, 16, 17, 18, 19, 24, 25)
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

  # Bios that requires temperature
  req_temp <- c(1, 2, 3, 4, 7, 8, 9, 10, 11, 18, 19, 26, 27, 34, 35)
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
  
  # Bios that requires solar radiation
  req_srad <- c(20, 21, 22, 23, 24, 25, 26, 27)
  if (any(req_srad %in% bios)) {
    if (is.null(srad)) {
      stop(paste0(paste0("Bio", sprintf("%02d", req_srad[req_srad %in% bios]),
                         collapse = ", "),
                  " require(s) srad."))
    }
    if (checkNA == TRUE) {
      srad_na <- mismatch_NA(srad)
      if (srad_na$logical == TRUE & stopNA == TRUE) {
        stop("srad has unexpected NA values")
      }
      srad_sum <- srad_na$sum_lyr
    }
  }
  
  # Bios that requires soil moisture
  req_soilm <- c(28, 29, 30, 31, 32, 33, 34, 35)
  if (any(req_soilm %in% bios)) {
    if (is.null(soilm)) {
      stop(paste0(paste0("Bio", sprintf("%02d", req_soilm[req_soilm %in% bios]),
                         collapse = ", "),
                  " require(s) soilm."))
    }
    if (checkNA == TRUE) {
      soilm_na <- mismatch_NA(soilm)
      if (soilm_na$logical == TRUE & stopNA == TRUE) {
        stop("soilm has unexpected NA values")
      }
      soilm_sum <- soilm_na$sum_lyr
    }
  }

  if (checkNA == TRUE) {
    # Sum all rasters
    if (!exists("tmin_sum")) tmin_sum <-  NULL
    if (!exists("tmax_sum")) tmax_sum <-  NULL
    if (!exists("prcp_sum")) prcp_sum <-  NULL
    if (!exists("srad_sum")) srad_sum <-  NULL
    if (!exists("soilm_sum")) soilm_sum <-  NULL

    intra_na <- purrr::reduce(list(tmin_sum, tmax_sum, prcp_sum, srad_sum, soilm_sum) |>
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
        message("SpatRasters (tmin, tmax, prcp, srad and/or soilm) don't share same NAs values")
      }
    }
  }

  # Bios that requires tavg
  if (any(c(1, 4, 8, 9, 10, 11, 18, 19) %in% bios)) {
    if (is.null(tavg)) {
      if (is.null(tmin) | is.null(tmax)) {
        stop("tavg cannot be calculated becuase tmin and/or tmax are NULL")
      } else {
        tavg <- bioclima::t_avg(tmin = tmin, tmax = tmax)
      }
    } 
  }

  ## ONLY TEMPERATURE
  # Bio01
  if (1 %in% bios) bio01 <- bioclima::bio_01(tavg = tavg)
  # Bio02
  if (any(2:3 %in% bios)) bio02 <- bioclima::bio_02(tmin = tmin, tmax = tmax)
  # Bio04
  if (4 %in% bios) bio04 <- bioclima::bio_04(tavg = tavg)
  # Bio05
  if (any(c(3, 5, 7) %in% bios)) bio05 <- bioclima::bio_05(tmax = tmax)
  # Bio06
  if (any(c(3, 6, 7) %in% bios)) bio06 <- bioclima::bio_06(tmin = tmin)
  # Bio07
  if (any(c(3, 7) %in% bios)) bio07 <- bioclima::bio_07(bio05 = bio05, bio06 = bio06)
  # Bio03
  if (3 %in% bios) bio03 <- bioclima::bio_03(bio02 = bio02, bio07 = bio07)
  
  ## ONLY PRECIPITATION
  # Bio12
  if (12 %in% bios) bio12 <- bioclima::bio_12(prcp = prcp)
  # Bio13
  if (13 %in% bios) bio13 <- bioclima::bio_13(prcp = prcp)
  # Bio14
  if (14 %in% bios) bio14 <- bioclima::bio_14(prcp = prcp)
  # Bio15
  if (15 %in% bios) bio15 <- bioclima::bio_15(prcp = prcp)
  
  
  ## ONLY PRECIPITATION PERIOD
  if (any(c(8:9, 16:19, 24:25) %in% bios)) {
    wet_period <- bioclima::get_window(prcp, period, circular)
  }
  # Bio16
  if (16 %in% bios) bio16 <- bioclima::bio_16(wet = wet_period)
  # Bio17
  if (17 %in% bios) bio17 <- bioclima::bio_17(wet = wet_period)
  
  ### ONLY TEMPERATURE PERIOD
  if (any(c(8:11, 18:19, 26:27, 34:35) %in% bios)) {
    tmp_period <- bioclima::get_window(tavg, period, circular) / period
  }
  # Bio10
  if (10 %in% bios) bio10 <- bioclima::bio_10(tmp = tmp_period)
  # Bio11
  if (11 %in% bios) bio11 <- bioclima::bio_11(tmp = tmp_period)
  
  ## ONLY SOLAR RADIATION
  # Bio20
  if (20 %in% bios) bio20 <- bioclima::bio_20(srad = srad)
  # Bio21
  if (21 %in% bios) bio21 <- bioclima::bio_21(srad = srad)
  # Bio22
  if (22 %in% bios) bio22 <- bioclima::bio_22(srad = srad)
  # Bio23
  if (23 %in% bios) bio23 <- bioclima::bio_23(srad = srad)
  
  ### GET SOLAR RADIATION PERIOD
  if (any(c(24:27) %in% bios)) {
    srad_period <- bioclima::get_window(srad, period, circular) / period
  }
  
  ## ONLY SOIL MOISTURE
  # Bio28
  if (28 %in% bios) bio28 <- bioclima::bio_28(soilm = soilm)
  # Bio29
  if (29 %in% bios) bio29 <- bioclima::bio_29(soilm = soilm)
  # Bio30
  if (30 %in% bios) bio30 <- bioclima::bio_30(soilm = soilm)
  # Bio31
  if (31 %in% bios) bio31 <- bioclima::bio_31(soilm = soilm)
  
  ### ONLY SOIL MOISTURE PERIOD
  if (any(c(32:35) %in% bios)) {
    soilm_period <- bioclima::get_window(soilm, period, circular) / period
  }
  # Bio32
  if (32 %in% bios) bio32 <- bioclima::bio_32(psoil = soilm_period)
  # Bio33
  if (33 %in% bios) bio33 <- bioclima::bio_33(psoil = soilm_period)
  
  #### COMBINED PERIODS
  # Bio08
  if (8 %in% bios) bio08 <- bioclima::bio_08(tmp = tmp_period, wet = wet_period)
  # Bio09
  if (9 %in% bios) bio09 <- bioclima::bio_09(tmp = tmp_period, wet = wet_period)
  # Bio18
  if (18 %in% bios) bio18 <- bioclima::bio_18(tmp = tmp_period, wet = wet_period)
  # Bio19
  if (19 %in% bios) bio19 <- bioclima::bio_19(tmp = tmp_period, wet = wet_period)
  # Bio24
  if (24 %in% bios) bio24 <- bioclima::bio_24(prad = srad_period, wet = wet_period)
  # Bio25
  if (25 %in% bios) bio25 <- bioclima::bio_25(prad = srad_period, wet = wet_period)
  # Bio26
  if (26 %in% bios) bio26 <- bioclima::bio_26(prad = srad_period, tmp = tmp_period)
  # Bio27
  if (27 %in% bios) bio27 <- bioclima::bio_27(prad = srad_period, tmp = tmp_period)
  # Bio34
  if (34 %in% bios) bio34 <- bioclima::bio_34(psoil = soilm_period, tmp = tmp_period)
  # Bio35
  if (35 %in% bios) bio35 <- bioclima::bio_35(psoil = soilm_period, tmp = tmp_period)
  
  # Window message
  if (any(c(8:11, 16:19, 24:27, 32:35) %in% bios)) {
    message(
      paste0(paste0("Bio", sprintf("%02d", c(8:11, 16:19, 24:27, 32:35)[c(8:11, 16:19, 24:27, 32:35) %in% bios]),
                    collapse = ", "),
             " was(were) built with a period of ", period,
             " units with", if(circular == FALSE) "out", " circularity.")
    )
  }

  # Create a unique spatRaster
  bios_rast <- terra::rast(mget(paste0("bio", sprintf("%02d", bios))))
  return(bios_rast)
}
