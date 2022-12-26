source("utils.r")
source("field.r")
source("animation.r")
source("player.r")
source("ball.r")

init_game <- function(width, height, title) {
  init_window(width, height, title)
  set_target_fps(60)
  return(
    list(
      field  = init_field(),
      player = init_player(),
      ball   = init_ball()
    )
  )
}

update_game <- function(game) {
  game$player <- update_player(game$state, game$player)
  if (is_key_pressed(key$space)) {
    game$ball <- shoot_ball(game$ball, rand(75, 105), rand(3.2, 5), 300.0, 10.0, TRUE)
  } else if (is_key_pressed(key$m)) {
    game$player$animation <- set_animation(game$player$animation, ANIMATION_ID$HIT)
  } else if (is_key_pressed(key$comma)) {
    game$player$animation <- set_animation(game$player$animation, ANIMATION_ID$HIT_REV)
  } else if (is_key_pressed(key$n)) {
    game$player$animation <- set_animation(game$player$animation, ANIMATION_ID$HIGHBALL)
  }

  if (game$ball$speed > 0) {
    game$ball <- update_ball(game$ball)
  }
  return(game)
}

draw_game <- function(game) {
  draw_field(game$field)
  draw_player(game$state, game$player)
  game$ball <- draw_ball(game$ball)
  # Rectangle test
  draw_rectangle_rec(
    rectangle(game$player$position[1]-25, game$player$position[2]-30, 95, 44),
    color(0, 0, 0, 120)
  )
  return(game)
}

unload_game <- function(game) {
  unload_field(game$field)
  unload_player(game$player)
}