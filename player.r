# Constants
SQRT2 <- sqrt(2)

BALL_STATE <- create.enum(
  c("1", "2", "3"),
  c("WAIT_BALL", "HIT_BALL", "NONE")
)

init_player <- function() {
  player <- list(
    animation = init_animation("assets/graphics/rumi.png"),
    position = c(x = 175, y = 600),
    speed = 220, can_move = TRUE, ball_state = BALL_STATE$NONE
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$IDLE,
    rectangle(0, 0, 54, 20), c(27, 20), c(1.0, 1.5)
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$HIGHBALL,
    rectangle(0, 20, 120, 60), c(30, 60), c(0.05, 0.05, 0.05, 0.1), #c(1.5, 0.05, 0.05, 0.2),
    TRUE, ANIMATION_ID$IDLE
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$HIT,
    rectangle(0, 80, 155, 27), c(31, 27), c(0.1, 0.01, 0.01, 0.05, 0.1),
    TRUE, ANIMATION_ID$IDLE
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$HIT_REV,
    rectangle(0, 80, 155, 27), c(-31, 27), c(0.1, 0.03, 0.03, 0.05, 0.1),
    TRUE, ANIMATION_ID$IDLE
  )
  player$animation <- load_animation(
    player$animation,
    ANIMATION_ID$MOVE,
    rectangle(0, 0, 54, 20), c(27, 20), c(0.2, 0.2)
  )
  player$animation <- set_animation(player$animation, ANIMATION_ID$IDLE)
  return(player)
}

player_is_off_limits <- function(x, y, width, height) {
  return(x < 0 || x+width*2 > get_screen_width() || y-height < get_screen_height()/2 || y+height*2 > get_screen_height())
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
  
  if (dx != 0) {
    dx <- dx / SQRT2
    player$animation <- set_animation(player$animation, ANIMATION_ID$MOVE)
  }
  if (dy != 0) {
    dy <- dy / SQRT2
    player$animation <- set_animation(player$animation, ANIMATION_ID$MOVE)
  }
  if (dx == 0 && dy == 0 && player$animation$current_id == ANIMATION_ID$MOVE) {
    player$animation <- set_animation(player$animation, ANIMATION_ID$IDLE)
  }
  
  new_pos_x <- player$position["x"] + dx * get_frame_time()
  new_pos_y <- player$position["y"] + dy * get_frame_time()

  if (!player_is_off_limits(new_pos_x, new_pos_y, 27, 20)) {
    player$position["x"] <- new_pos_x
    player$position["y"] <- new_pos_y
  }
  
  return(player)
}

player_is_hitting <- function(curr_id) {
  return(curr_id == ANIMATION_ID$HIT || curr_id == ANIMATION_ID$HIGHBALL || curr_id == ANIMATION_ID$HIT_REV)
}

update_player <- function(state, player, ball) {
  player$animation <- update_animation(player$animation)

  if (!player_is_hitting(player$animation$current_id)) {
    player$can_move <- TRUE
  }

  if (is_key_pressed(key$space) && length(ball$movement_points) > 1 && player$ball_state == BALL_STATE$NONE && ball$y < player$position[2]-50) {
    player_rect <- rectangle(player$position[1]-10, player$position[2]-50, 65, 44)
    found_point <- FALSE
    for (point in ball$movement_points) {
      if (check_collision_point_rec(c(point$x, point$y), player_rect)) {
        if (!found_point) {
          found_point <- TRUE
        } else {
          # Check what animation to play here and where is the player going to shoot
          player$can_move <- FALSE
          player$animation <- set_animation(player$animation, ANIMATION_ID$HIT)
          player$animation$paused <- TRUE
          player$ball_state <- BALL_STATE$WAIT_BALL
          break
        }
      }
    }
  }

  if (player$ball_state == BALL_STATE$WAIT_BALL) {
    ball_collision <- check_collision_recs(
      rectangle(ball$x, ball$y, 10, 10),
      rectangle(player$position[1]-10, player$position[2]-50, 65, 24)
    )
    if (ball_collision) {
      player$animation$paused <- FALSE
      player$ball_state <- BALL_STATE$NONE
      ball$movement_points <- array(list(list(x = ball$x, y=ball$y)))
      ball <- shoot_ball(ball, rand(75, 105), 4.5, ball$x, ball$y, -10.0, TRUE, TRUE)
    }
  }

  #player$can_move <- !player_is_hitting(player$animation$current_id)
  if (player$can_move) {
    player <- player_movement(player)
  }
  return(list(p=player, b=ball))
}

draw_player <- function(state, player) {
  draw_animation(player$animation, player$position["x"], player$position["y"], 2.5, 0.0, "white")
  #draw_rectangle_rec(rectangle(player$position["x"], player$position["y"], 60, 60), "white")
}

unload_player <- function(player) {
  unload_texture(player$animation$texture)
}