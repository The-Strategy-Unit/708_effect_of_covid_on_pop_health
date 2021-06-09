
# README
# interpolate mid-year population estimates to get a weekly population exposure


# interpolate_exposure --------------------------------------------------------
interpolate_exposure <- function(df, xout) {
  
  splineLs <- spline(x = df$fracYrMid, y = df$pop, xout = xout$fracYear, method = "natural",  ties = mean)
  
  exposure <- tibble(exp = splineLs$y, x = splineLs$x, isoWk = xout$isoWk)
  
  return(exposure)
  
}

