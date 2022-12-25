# Constants
SQRT2 <- sqrt(2)
NORMAL_BALL <- key$n
HIGH_BALL <- key$m

init_player <- function() {
  player <- list(
    animation = init_animation("assets/graphics/rumi.png"),
    position = c(x = 175, y = 600),
    speed = 170,
    can_move = TRUE
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$IDLE,
    rectangle(0, 0, 54, 20), c(27, 20), c(1.0, 1.5)
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$SERVE,
    rectangle(0, 20, 120, 60), c(30, 60), c(1.5, 0.05, 0.05, 0.2),
    TRUE, ANIMATION_ID$IDLE
  )

  player$animation <- set_animation(player$animation, ANIMATION_ID$IDLE)
  return(player)
}

player_movement <- function(player) {
  dx <- 0
  dy <- 0
  if (is_key_down(key$w) || is_key_down(key$up)) {
    dy <- -player$speed #player$position["y"] <- player$position["y"] - player$speed * get_frame_time()
  } else if (is_key_down(key$s) || is_key_down(key$down)) {
    dy <- player$speed #player$position["y"] <- player$position["y"] + player$speed * get_frame_time()
  }
  
  if (is_key_down(key$a) || is_key_down(key$left)) {
    dx <- -player$speed #player$position["x"] <- player$position["x"] - player$speed * get_frame_time()
  } else if (is_key_down(key$d) || is_key_down(key$right)) {
    dx <- player$speed #player$position["x"] <- player$position["x"] + player$speed * get_frame_time()
  }
  
  if (dx != 0 && dy != 0) {
    dx <- dx / SQRT2
    dy <- dy / SQRT2
  }
  
  player$position["x"] <- player$position["x"] + dx * get_frame_time()
  player$position["y"] <- player$position["y"] + dy * get_frame_time()
  
  return(player)
}

update_player <- function(state, player) {
  player$animation <- update_animation(player$animation)
  if (player$can_move) {
    player <- player_movement(player)
  }
  return(player)
}

draw_player <- function(state, player) {
  draw_animation(player$animation, player$position["x"], player$position["y"], 2.5, 0.0, "white")
  #draw_rectangle_rec(rectangle(player$position["x"], player$position["y"], 60, 60), "white")
}

unload_player <- function(player) {
  unload_texture(player$animation$texture)
}