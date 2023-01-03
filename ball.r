init_ball <- function() {
  return(
    list(
      texture = load_texture("assets/graphics/ball.png"),
      spawn_x = 300.0, spawn_y = 10.0, # Initial position
      origin_x = 0.0, origin_y = 0.0,  # Fixed position where the ball lands
      bounce_x = 0.0, bounce_y = 0.0,  # Current bounce position
      x = 300.0, y = 10.0, z = 1.0,    # Current position and scale
      direction_angle = 0.0, force = 0.0,
      texture_angle = 0.0, speed = 0.0, i = 0.0,
      bounced_once = FALSE, bounced_twice = FALSE,
      set_flag = FALSE, game_over = FALSE,
      explosion = load_sound("assets/sfx/beep_gut_rip.wav"),
      movement_points = array(list(list(x = 300.0, y=10.0))),
      bounce_sound = load_sound("assets/sfx/tennis-bounce-ball.wav")
    )
  )
}

shoot_ball_to <- function(ball, speed, initial_x, initial_y, target_x, target_y) {
  ball$speed <- speed
  ball$direction_angle <- get_angle(initial_x, initial_y, target_x, target_y) + 180
  ball$x <- initial_x
  ball$y <- initial_y
  ball$origin_x <- initial_x
  ball$origin_y <- initial_y
  ball$z <- 1.0
  ball$i <- 0
  ball$bounced_once <- FALSE
  ball$bounced_twice <- FALSE

  # Get how many steps does our parabola need to iterate to get to the target position
  #parabola_steps <- 0
  parabola_end <- 0.0
  found_end <- FALSE
  x <- ball$origin_x + (ball$speed * cos(deg2rad(ball$direction_angle)))
  y <- ball$origin_y + (ball$speed * sin(deg2rad(ball$direction_angle)))
  for (i in 1:50) {
    x <- x + ball$speed * cos(deg2rad(ball$direction_angle))
    y <- y + ball$speed * sin(deg2rad(ball$direction_angle))
    ball$movement_points <- append(ball$movement_points, list(list(x=x, y=y)))
    if (!found_end) { # <--
      parabola_end <- parabola_end + 0.01
    }
    if (abs(target_x-x) < 5 && abs(target_y-y) < 5 && !found_end) {
      found_end <- TRUE
    }
  }
  # Same formula as in shoot_ball but switching the variables
  ball$force <- 2 / parabola_end
  return(ball)
}

shoot_ball <- function(ball, angle, force, initial_x, initial_y, speed, reset=FALSE) {
  ball$speed <- speed
  ball$direction_angle <- angle
  ball$force <- force
  ball$x <- initial_x
  ball$y <- initial_y
  ball$z <- 1.0
  ball$i <- 0

  if (reset) {
    ball$bounced_once <- FALSE
    ball$bounced_twice <- FALSE
    ball$origin_x <- ball$spawn_x
    ball$origin_y <- ball$spawn_y
  } else if (!ball$bounced_once && !ball$bounced_twice) {
    ball$origin_x <- ball$spawn_x
    ball$origin_y <- ball$spawn_y
  } else if (ball$bounced_once && !ball$bounced_twice) {
    ball$origin_x <- ball$bounce_x
    ball$origin_y <- ball$bounce_y
  }

  if (length(ball$movement_points) == 1) {
    # Skip 25 points, as they are not needed
    x <- ball$origin_x + (ball$speed * cos(deg2rad(ball$direction_angle-1))) * 25
    y <- ball$origin_y + (ball$speed * sin(deg2rad(ball$direction_angle-1))) * 25
    for (i in 1:45) {
      x <- x + ball$speed * cos(deg2rad(ball$direction_angle-1))
      y <- y + ball$speed * sin(deg2rad(ball$direction_angle-1))
      ball$movement_points <- append(ball$movement_points, list(list(x=x, y=y)))
    }
  }
  return(ball)
}

is_ball_offscreen <- function(ball) {
  return(ball$x < 0 || ball$x > get_screen_width() || ball$y < 0 || ball$y > get_screen_height())
}

update_ball <- function(ball, field) {
  if (!is_ball_offscreen(ball) && ball$speed != 0) {
    ball$texture_angle <- ball$texture_angle + 1
    if (ball$texture_angle > 360) {
      ball$texture_angle <- 0
    }
    ball$x <- ball$x + ball$speed * cos(deg2rad(ball$direction_angle))
    ball$y <- ball$y + ball$speed * sin(deg2rad(ball$direction_angle))
    ball$z <- -ball$force * ball$i^2 + 2 * ball$i + 1
    ball$i <- ball$i + 0.01
    if (ball$z < 1) {
      if (!ball$bounced_once) {
        tile_x <- ((ball$x-field$offset_x) %/% 32)
        tile_y <- ((ball$y-field$offset_y) %/% 32)
        if (!field$mines_set && is_in_bounds(field$grid_list, tile_x, tile_y)) {
          field$grid_list <- init_grid(field$grid_list, tile_x, tile_y)
          field$mines_set <- TRUE
        }
        if (is_in_bounds(field$grid_list, tile_x, tile_y) && !field$grid_list[[tile_x, tile_y]]$revealed) {
          if (ball$set_flag) {
            field$grid_list[[tile_x, tile_y]]$has_flag <- !field$grid_list[[tile_x, tile_y]]$has_flag
          } else if (field$grid_list[[tile_x, tile_y]]$is_mine && !field$grid_list[[tile_x, tile_y]]$has_flag) {
            ball$game_over <- TRUE
            play_sound(ball$explosion)
            for (x in 1:ncol(field$grid_list)) {
              for (y in 1:nrow(field$grid_list)) {
                if (field$grid_list[[x, y]]$is_mine && !field$grid_list[[x, y]]$has_flag) {
                  field$grid_list[[x, y]]$revealed <- TRUE
                }
              }
            }
          } else if (!field$grid_list[[tile_x, tile_y]]$has_flag) {
            field <- check_cell(field, tile_x, tile_y)
          }
        }
        play_sound(ball$bounce_sound)
        ball$bounced_once <- TRUE
        ball <- shoot_ball(ball, ball$direction_angle, ball$force*2, ball$x, ball$y, ball$speed)
      } else {
        play_sound(ball$bounce_sound)
        ball <- shoot_ball(ball, ball$direction_angle, ball$force*2, ball$x, ball$y, ball$speed)
        ball$bounced_twice <- TRUE
      }
    }
  } else {
    ball <- shoot_ball(game$ball, rand(75, 105), rand(3, 5), ball$spawn_x, ball$spawn_y, TRUE)
    ball$movement_points <- array(list(list(x = 300.0, y=10.0)))
    ball$speed <- 0.0
  }
  return(list(b=ball, f=field))
}

draw_ball <- function(ball) {
  #draw_circle_v(c(ball$x+3, ball$y+3), 3.0, "black")
  draw_texture_ex(ball$texture, c(ball$x, ball$y), ball$texture_angle, ball$z, ifelse(ball$set_flag, "orange", "white"))
  if (ball$speed != 0) {
    # Simplifying the quadratic formula taking: a = -ball$force, b = 2, c = 0 (see equation for ball$z)
    # c is not 1 because we want to check when the parabola is at y = 1 (the original scale of the ball)
    parabola_end <- 2 / ball$force
    # I want to know how many steps it takes the ball to get to that value
    # The x value of the parabola moves every 0.01 steps
    step_count <- ceiling(parabola_end / 0.01) + 1
    # ball$speed * cos(angle) and ball$speed * sin(angle) are constant values
    # that are added to the position of the ball every frame, so to know
    # where the ball is going to land we add the initial position + that constant times
    # the total step count
    ball$bounce_x <- ball$origin_x + (ball$speed * cos(deg2rad(ball$direction_angle))) * step_count
    ball$bounce_y <- ball$origin_y + (ball$speed * sin(deg2rad(ball$direction_angle))) * step_count
    draw_circle_v(c(ball$bounce_x+7, ball$bounce_y+7), 5.0, "yellow")
    #for (point in ball$movement_points) {
    #  draw_pixel_v(c(point$x, point$y), "red")
    #}
  }
  return(ball)
}

unload_ball <- function(ball) {
  unload_texture(ball$texture)
  unload_sound(ball$bounce_sound)
  unload_sound(ball$explosion)
}