# Helper functions
# -----------------------

# Average Temperature
#' @export
misqua <- function(tmin, tmax) {
  tavg <- (tmin + tmax) / 2
  names(tavg) <- paste0("tavg_", 1:terra::nlyr(tavg))
  return(tavg)
}

# CV terra
#' @export
cv_cli <- function(prcp) {
  pr <- prcp + 1
  x <- terra::mean(abs(pr))
  cv <- 100 * terra::stdev(pr, pop = FALSE, na.rm = TRUE) / x
  return(cv)
}

#' Windows
#'
#' @param x spatRaster
#' @param period Length of period. Default is three. If you are using months. It will be a quarter.
#' @param circular logical Include first month/weeks?
#' @export
ventana <- function(x, period, circular)  {
  lng <- terra::nlyr(x)
  if (circular == TRUE) {
    ind <- c(1:lng,  1:(period - 1))
    m <- matrix(ncol = period, nrow = lng)
    for (i in 1:period) {
      m[, i] <- ind[i:(lng + i - 1)]
      # if (i != 3) {
      #   m[, i] <- ind[i:(lng + i - 1)]
      # } else {
      #   m[, i] <- ind[(i - 1):(lng + i - 2)]
      # }
    }
  }
  if (circular == FALSE) {
    ind <- c(1:lng)
    m <- matrix(ncol = period, nrow = lng - period + 1)
    for (i in 1:period) {
      m[, i] <- ind[i:(lng - period + i)]
    }
  }
  vent <- NULL
  for (j in 1:nrow(m)) {
    sum_period <- terra::app(x[[m[j, ]]], sum, na.rm = TRUE)
    vent <- c(vent, sum_period)
  }
  return(terra::rast(vent))
}

#' @export
testGeom <- function(x, y) {
  testGeom <- terra::compareGeom(x, y, lyrs = TRUE)
  if (testGeom == TRUE) {
    return(x)
  }
}

#' @export
mismatch_NA <- function(layer) {
  # Get number of layers
  num_lyr <- terra::nlyr(layer)
  # Create a raster that sums if cells has values for all of them
  sum_lyr <- sum(!is.na(layer))
  # Create vector to check if there is a mismatch between NAs
  v_unique <- unique(as.vector(sum_lyr))
  # Check for cell values lower than the number of layers. This means that there
  # are cells with NA values
  if (any(v_unique != 0 & v_unique != num_lyr)) {
    miss_na <- v_unique[v_unique != 0 & v_unique != num_lyr]
    cells_na <- unlist(terra::cells(sum_lyr, miss_na))
    layer_na <- sapply(as.list(cells_na),
                       function(x) {which(is.na(extract(layer, x)))})
    message(paste0("Unexpected NA value in '", substitute(layer), "' object",
                   " | Layer number: ", layer_na,
                   " | Cell ID: ", cells_na, "\n"))

  }
  return(list(logical = any(v_unique != 0 & v_unique != num_lyr),
              sum_lyr = sum_lyr))
}
