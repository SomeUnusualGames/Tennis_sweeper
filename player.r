# Constants
SQRT2 <- sqrt(2)

BALL_STATE <- create.enum(
  c("1", "2", "3"),
  c("WAIT_BALL", "HIT_BALL", "NONE")
)

init_player <- function() {
  player <- list(
    animation = init_animation("assets/graphics/rumi.png"),
    position = c(175, 600),
    hit_ball_sounds = list(
      load_sound("assets/sfx/tennis-forehand.wav"),
      load_sound("assets/sfx/tennis-forehand2.wav")
    ),
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
    rectangle(0, 80, 155, 27), c(-31, 27), c(0.1, 0.01, 0.01, 0.05, 0.1),
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
  if (is_key_down(key$w)) {
    dy <- -player$speed
  } else if (is_key_down(key$s)) {
    dy <- player$speed
  }
  
  if (is_key_down(key$a)) {
    dx <- -player$speed
  } else if (is_key_down(key$d)) {
    dx <- player$speed
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
  
  new_pos_x <- player$position[1] + dx * get_frame_time()
  new_pos_y <- player$position[2] + dy * get_frame_time()

  if (!player_is_off_limits(new_pos_x, new_pos_y, 27, 20)) {
    player$position[1] <- new_pos_x
    player$position[2] <- new_pos_y
  }
  
  return(player)
}

player_is_hitting <- function(curr_id) {
  return(curr_id == ANIMATION_ID$HIT || curr_id == ANIMATION_ID$HIGHBALL || curr_id == ANIMATION_ID$HIT_REV)
}

update_player <- function(state, player, ball, b_pointer) {
  player$animation <- update_animation(player$animation)

  if (!player_is_hitting(player$animation$current_id)) {
    player$can_move <- TRUE
  }

  if (is_key_pressed(key$space) && length(ball$movement_points) > 1 && player$ball_state == BALL_STATE$NONE && ball$y < player$position[2]-50) {
    player_rect <- rectangle(player$position[1]-10, player$position[2]-50, 65, 44)
    for (point in ball$movement_points) {
      if (check_collision_point_rec(c(point$x, point$y), player_rect)) {
        player$can_move <- FALSE
        angle <- 180 + get_angle(
          player$position[1], player$position[2],
          b_pointer$position[1], b_pointer$position[2]
        )
        dist <- get_distance(
          player$position[1], player$position[2],
          ball$x, ball$y
        )
        if (dist < 100 && ball$z > 1.1) {
          player$animation <- set_animation(player$animation, ANIMATION_ID$HIGHBALL)
        } else if (angle >= 0) {
          player$animation <- set_animation(player$animation, ANIMATION_ID$HIT)
        } else {
          player$animation <- set_animation(player$animation, ANIMATION_ID$HIT_REV)
        }
        player$animation$paused <- TRUE
        player$ball_state <- BALL_STATE$WAIT_BALL
        break
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
      play_sound(sample(player$hit_ball_sounds, 1)[[1]])
      ball <- shoot_ball_to(
        ball, -10.0, ball$x, ball$y,
        b_pointer$position[1], b_pointer$position[2]
      )
    }
  }

  if (is_ball_offscreen(ball) && player$ball_state == BALL_STATE$WAIT_BALL) {
    player$ball_state = BALL_STATE$NONE
    player$can_move <- TRUE
    player$animation$paused <- FALSE
  }

  #player$can_move <- !player_is_hitting(player$animation$current_id)
  if (player$can_move) {
    player <- player_movement(player)
  }
  return(list(p=player, b=ball))
}

draw_player <- function(state, player) {
  draw_animation(player$animation, player$position[1], player$position[2], 2.5, 0.0, "white")
  #draw_rectangle_rec(rectangle(player$position["x"], player$position["y"], 60, 60), "white")
}

unload_player <- function(player) {
  unload_texture(player$animation$texture)
  unload_sound(player$hit_ball_sounds[[1]])
  unload_sound(player$hit_ball_sounds[[2]])
}