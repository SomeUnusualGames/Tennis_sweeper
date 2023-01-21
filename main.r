library(raylibr)
library(enumerations)
source("game.r")

game <- init_game(640, 700, "Tennis Sweeper")

while (!window_should_close()) {
  # When detecting a mine, wait a little before showing
  # the game over screen
  if (game$ball$game_over && game$game_over_timer > 0) {
    game$game_over_timer <- game$game_over_timer - get_frame_time()
  }

  # Reset
  if (is_key_pressed(key$r) && game$ball$game_over && game$game_over_timer <= 0) {
    game$ball_pointer <- reset_pointer(game$ball_pointer)
    game$machine <- reset_ball_machine(game$machine)
    game$player <- reset_player(game$player)
    game$field <- reset_field(game$field)
    game$ball$game_over <- FALSE
    game$game_over_timer <- 0.0
    play_music_stream(game$music_game)
  }

  game <- update_game(game)

  # Draw
  begin_drawing()
  clear_background("black")
  if (!game$ball$game_over || game$game_over_timer > 0) {
    game <- draw_game(game)
  } else {
    draw_text("GAME OVER!", 130, 330, 60, "white")
    draw_text("Press R to reset", 130, 400, 40, "white")
  }
  if (game$show_fps) {
    draw_fps(0, 0)
  }
  end_drawing()
}

unload_game(game)
close_audio_device()
close_window()