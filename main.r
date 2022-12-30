library(raylibr)
library(enumerations)
source("game.r")

game <- init_game(640, 700, "Tennis Sweeper")

while (!window_should_close()) {
  game <- update_game(game)
  begin_drawing()
  clear_background("black")
  game <- draw_game(game)
  draw_fps(0, 0)
  end_drawing()
}

unload_game(game)
close_window()