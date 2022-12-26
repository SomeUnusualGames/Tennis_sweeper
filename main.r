library(raylibr)
library(enumerations)
source("game.r")

# TODO: Player hitting the ball
# Define a couple of areas (squares) that the player can reach
# if the ball isn't in that area, don't trigger the hit animation

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