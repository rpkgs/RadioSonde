#' Precipitable Water Vapor
#' 
#' Calculates Precipitable water (in mm) up to minp (minimum pressure) using
#' dew-point temperature (td) and temperature (temp) (both in deg C).
#' 
#' @param press pressure 
#' @param td Dew-point temperature (in deg C)
#' is NULL)
#' @param temp Temperature (in deg C)
#' @param minP Minimum Pressure
#' 
#' @return single numeric value is returned.
#' @author Junhong Wang
#' 
#' @examples 
#' data(ExampleSonde)
#' d = ExampleSonde
#' PWV(d$press, d$dewpt, d$temp)
#' @export
PWV <- function(press, td, temp, minp = 400.) {
  q <- Tdew2q(temp, td, press)
  n <- length(press)
  pw <- 0.
  for (k in 2.:n) {
    if (press[k] < minp) break
    if (is.na(q[k]) || is.na(q[k - 1])) next
    
    meanq <- (q[k] + q[k - 1]) / 2.
    pw <- pw + (0.1 * meanq * (press[k - 1] - press[k])) / 9.81
  }
  return(pw)
}

Tdew2q <- function(temp, td, p) {
  # calculate specific humidity (g/kg) using temperature and dew-point T
  es <- 6.1121 * exp((temp * 17.67) / (temp + 243.5))
  e <- 6.1121 * exp((td * 17.67) / (td + 243.5))
  q <- (1000. * (0.62197 * e)) / (p - (1. - 0.62197) * e)
  return(q)
}
