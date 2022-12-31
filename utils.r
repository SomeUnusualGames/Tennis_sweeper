
rand <- function(min, max, usefloat=FALSE) {
  if (usefloat) {
    return(floor(runif(1, min=min, max=max)))
  }
  return(runif(1, min=min, max=max))
}

deg2rad <- function(deg) {
  return(deg * pi/180.0)
}

rad2deg <- function(rad) {
  return(rad * 180.0/pi)
}

scale_value <- function(x, min_allowed, max_allowed, min_val, max_val) {
  return((max_allowed - min_allowed) * (x - min_val) / (max_val - min_val) + min_allowed)
}

lerp <- function(a, b, t) {
  return(a + (b - a) * t)
}

get_angle <- function(x1, y1, x2, y2) {
  return(rad2deg(atan2(y2-y1, x2-x1)))
}