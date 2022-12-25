init_ball <- function() {
  return(
    list(
      texture = load_texture("assets/graphics/ball.png"),
      spawn_x = 300.0, spawn_y = 10.0, # Initial position
      origin_x = 0.0, origin_y = 0.0,  # Fixed position where the ball lands
      bounce_x = 0.0, bounce_y = 0.0,  # Current bounce position
      x = 300.0, y = 10.0, z = 1.0,    # Current position and scale
      angle = 0.0, force = 0.0,
      speed = 0.0, i = 0.0,
      bounced_once = FALSE, bounced_twice = FALSE
    )
  )
}

shoot_ball <- function(ball, angle, force, initial_x, initial_y, reset=FALSE) {
  ball$speed <- 10.0
  ball$angle <- angle
  ball$force <- force
  ball$x <- initial_x
  ball$y <- initial_y
  ball$z <- 1.0
  ball$i <- 0
  if (!ball$bounced_once && !ball$bounced_twice) {
    ball$origin_x <- ball$spawn_x
    ball$origin_y <- ball$spawn_y
  } else if (ball$bounced_once && !ball$bounced_twice) {
    ball$origin_x <- ball$bounce_x
    ball$origin_y <- ball$bounce_y
  }
  if (reset) {
    ball$bounced_once <- FALSE
    ball$bounced_twice <- FALSE
    ball$origin_x <- ball$spawn_x
    ball$origin_y <- ball$spawn_y
  }
  return(ball)
}

is_ball_offscreen <- function(ball) {
  return(ball$x < 0 || ball$x > get_screen_width() || ball$y < 0 || ball$y > get_screen_height())
}

update_ball <- function(ball) {
  if (!is_ball_offscreen(ball) && ball$speed > 0) {
    ball$x <- ball$x + ball$speed * cos(deg2rad(ball$angle))
    ball$y <- ball$y + ball$speed * sin(deg2rad(ball$angle))
    ball$z <- -ball$force * ball$i^2 + 2 * ball$i + 1
    ball$i <- ball$i + 0.01
    if (ball$z < 1) {
      if (!ball$bounced_once){
        ball$bounced_once <- TRUE
        ball <- shoot_ball(ball, ball$angle, ball$force*2, ball$x, ball$y)
      } else {
        ball <- shoot_ball(ball, ball$angle, ball$force*2, ball$x, ball$y)
        ball$bounced_twice <- TRUE
        #ball$speed <- 0.0
      }
    }
  } else {
    ball <- shoot_ball(game$ball, rand(75, 105), rand(3, 5), 300.0, 10.0, TRUE)
    ball$speed <- 0.0
  }
  return(ball)
}

draw_ball <- function(ball) {
  draw_circle_v(c(ball$x+3, ball$y+3), 3.0, "black")
  draw_texture_ex(ball$texture, c(ball$x, ball$y), 0.0, ball$z, "white")
  if (ball$speed > 0) {
    # Simplifying the quadratic formula taking: a = -ball$force, b = 2, c = 0 (see equation for ball$z)
    # c is not 1 because we want to check when the parabola is at y = 1 (the original scale of the ball)
    parabola_end <- (2 / ball$force)
    # I want to know how many steps it takes the ball to get to that value
    # The x value of the parabola moves every 0.01 steps
    step_count <- ceiling(parabola_end / 0.01) + 1
    # ball$speed * cos(angle) and ball$speed * sin(angle) are constant values
    # that are added to the position of the ball every frame, so to know
    # where the ball is going to land we add the initial position + that constant times
    # the total step count
    ball$bounce_x <- ball$origin_x + (ball$speed * cos(deg2rad(ball$angle))) * step_count
    ball$bounce_y <- ball$origin_y + (ball$speed * sin(deg2rad(ball$angle))) * step_count
    draw_circle_v(c(ball$bounce_x+7, ball$bounce_y+7), 5.0, "yellow")
  }
  return(ball)
}