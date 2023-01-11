source("utils.r")
source("field.r")
source("animation.r")
source("ball_pointer.r")
source("ball.r")
source("ball_machine.r")
source("player.r")

init_game <- function(width, height, title) {
  init_window(width, height, title)
  set_target_fps(60)
  init_audio_device()
  game <- list(
    field   = init_field(),
    player  = init_player(),
    ball    = init_ball(),
    machine = init_ball_machine(),
    ball_pointer = init_ball_pointer(),
    game_over_timer = 0.0,
    show_fps = FALSE,
    played_victory_sound = FALSE,
    cheer_sound = load_sound("assets/sfx/cheering-and-clapping-crowd-1.wav"),
    music_game = load_music_stream("assets/music/dark-beat-synth.wav")
  )
  set_music_volume(game$music, 0.15)
  play_music_stream(game$music)
  return(game)
}

update_game <- function(game) {
  update_music_stream(game$music_game)
  if (is_key_pressed(key$k)) {
    game$show_fps <- !game$show_fps
  }  

  updated_vars <- update_player(game$state, game$player, game$ball, game$ball_pointer)
  game$player <- updated_vars$p
  game$ball <- updated_vars$b

  game$ball_pointer <- update_ball_pointer(game$ball_pointer)

  if (is_key_pressed(key$q)) {
    game$ball$set_flag <- !game$ball$set_flag
  }

  if (game$field$victory) {
    game$machine$can_shoot <- FALSE
    stop_music_stream(game$music_game)
    if (!game$played_victory_sound) {
      game$played_victory_sound <- TRUE
      play_sound(game$cheer_sound)
    }
  }

  if (game$ball$speed != 0) {
    updated_vars <- update_ball(game$ball, game$field)
    game$ball <- updated_vars$b
    game$field <- updated_vars$f
  }

  if (game$ball$game_over && game$machine$can_shoot) {
    game$game_over_timer <- 1.2
    game$player$can_move <- FALSE
    game$machine$can_shoot <- FALSE
    game$ball_pointer$can_move <- FALSE
    stop_music_stream(game$music_game)
  }

  game$machine <- update_ball_machine(game$machine)
  if (game$machine$state == MACHINE_STATE$SHOOT) {
    game$ball <- shoot_ball(game$ball, game$machine$ball_angle, rand(3, 5), 300.0, 10.0, 10.0, TRUE)
    game$machine$state <- MACHINE_STATE$WAIT_BALL
  } else if (game$machine$state == MACHINE_STATE$WAIT_BALL && is_ball_offscreen(game$ball)) {
    game$machine$state <- MACHINE_STATE$WAIT_TIMER
    game$machine$shoot_timer <- 1.0
  }

  # if (is_key_pressed(key$m)) {
  #  game$player$animation <- set_animation(game$player$animation, ANIMATION_ID$HIT)
  #} else if (is_key_pressed(key$comma)) {
  #  game$player$animation <- set_animation(game$player$animation, ANIMATION_ID$HIT_REV)
  #} else if (is_key_pressed(key$n)) {
  #  game$player$animation <- set_animation(game$player$animation, ANIMATION_ID$HIGHBALL)
  #}

  return(game)
}

draw_game <- function(game) {
  draw_field(game$field)
  draw_player(game$state, game$player)
  game$ball <- draw_ball(game$ball)
  draw_ball_machine(game$machine)
  draw_ball_pointer(game$ball_pointer)
  if (game$field$victory) {
    draw_text("YOU WON!", 130, 380, 60, "black")
    draw_text("Press ESC to quit", 130, 450, 40, "black")
  }
  #if (length(game$ball$movement_points) > 1) {
  #  points <- game$ball$movement_points
  #  draw_line_v(
  #    c(points[[5]]$x, points[[5]]$y),
  #    c(points[[35]]$x, points[[35]]$y),
  #    "black"
  #  )
  #}
  # Rectangle test
  #draw_rectangle_rec(
  #  rectangle(game$player$position[1]-10, game$player$position[2]-50, 65, 44),
  #  color(255, 255, 255, 120)
  #)
  return(game)
}

unload_game <- function(game) {
  unload_music_stream(game$music_game)
  unload_field(game$field)
  unload_player(game$player)
  unload_ball(game$ball)
  unload_ball_machine(game$machine)
  unload_sound(game$cheer_sound)
}