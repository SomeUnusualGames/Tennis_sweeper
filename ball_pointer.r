

init_ball_pointer <- function() {
  return(
    list(
      center = c(182+112+32, 60+112+32),
      position = c(182+112+32, 60+112+32),
      movement = 1,
      lerp_movemet = 0.009
    )
  )
}

update_ball_pointer <- function(b_pointer) {
  if (is_key_down(key$up)) {
    b_pointer$position[2] <- b_pointer$position[2] - b_pointer$movement
  } else if (is_key_down(key$down)) {
    b_pointer$position[2] <- b_pointer$position[2] + b_pointer$movement
  }
  if (is_key_down(key$left)) {
    b_pointer$position[1] <- b_pointer$position[1] - b_pointer$movement
  } else if (is_key_down(key$right)) {
    b_pointer$position[1] <- b_pointer$position[1] + b_pointer$movement
  }

  if (b_pointer$position[1] != b_pointer$center[1]) {
    b_pointer$position[1] <- lerp(b_pointer$position[1], b_pointer$center[1], b_pointer$lerp_movemet)
  }
  if (b_pointer$position[2] != b_pointer$center[2]) {
    b_pointer$position[2] <- lerp(b_pointer$position[2], b_pointer$center[2], b_pointer$lerp_movemet)
  }

  return(b_pointer)
}

draw_ball_pointer <- function(b_pointer) {
  draw_circle_v(b_pointer$position, 5.0, "red")
}