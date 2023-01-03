library(raylibr)
library(enumerations)
source("game.r")

game <- init_game(640, 700, "Tennis Sweeper")

while (!window_should_close()) {
  if (game$ball$game_over && game$game_over_timer > 0) {
    game$game_over_timer <- game$game_over_timer - get_frame_time()
  }
  if (is_key_pressed(key$r) && game$ball$game_over && game$game_over_timer <= 0) {
    game$ball_pointer <- reset_pointer(game$ball_pointer)
    game$machine <- reset_ball_machine(game$machine)
    game$player <- reset_player(game$player)
    game$field <- reset_field(game$field)
    game$ball$game_over <- FALSE
    game$game_over_timer <- 0.0
  }
  game <- update_game(game)
  begin_drawing()
  clear_background("black")
  if (!game$ball$game_over || game$game_over_timer > 0) {
    game <- draw_game(game)
  } else {
    draw_text("GAME OVER!", 130, 330, 60, "black")
    draw_text("Press R to reset", 130, 400, 40, "black")
  }
  if (game$show_fps) {
    draw_fps(0, 0)
  }
  end_drawing()
}

unload_game(game)
close_audio_device()
close_window()