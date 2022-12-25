
rand <- function(min, max, usefloat=FALSE) {
  if (usefloat) {
    return(floor(runif(1, min=min, max=max)))
  } else {
    return(runif(1, min=min, max=max))
  }
}

deg2rad <- function(deg) {
  return(deg * pi/180.0)
}