print_grid <- function(grid_list) {
  s <- ""
  for (x in 1:ncol(grid_list)) {
    for (y in 1:nrow(grid_list)) {
      if (grid_list[[x, y]]$is_mine) {
        s <- paste(s, "*")
      } else {
        s <- paste(s, grid_list[[x, y]]$num)
      }
    }
    print(s)
    s <- ""
  }
}

is_in_bounds <- function(grid_list, x, y) {
  return(x >= 1 && x <= ncol(grid_list) && y >= 1 && y <= nrow(grid_list))
}

get_tiles <- function(grid_list, tile_x, tile_y, check_mines) {
  tiles <- c()
  for (y_offset in -1:1) {
    for (x_offset in -1:1) {
      x <- tile_x + x_offset
      y <- tile_y + y_offset
      if (is_in_bounds(grid_list, x, y) && (x_offset != 0 || y_offset != 0)) {
        if (check_mines && grid_list[[x, y]]$is_mine) {
          tiles <- append(tiles, list(list(x=x, y=y)))
        } else if (!check_mines && !grid_list[[x, y]]$is_mine && !grid_list[[x, y]]$has_flag && !grid_list[[x, y]]$revealed) {
          tiles <- append(tiles, list(list(x=x, y=y)))
        }
      }
    }
  }
  return(if (check_mines) length(tiles) else tiles)
}

init_grid <- function(grid_list, tile_x, tile_y) {
  # Put the mines at random positions
  mine_num <- 7
  while (mine_num > 0) {
    x <- rand(1, 8, TRUE)
    y <- rand(1, 8, TRUE)
    if (!grid_list[[x, y]]$is_mine && x != tile_x && y != tile_y) {
      grid_list[[x, y]]$is_mine <- TRUE
      mine_num <- mine_num - 1
    }
  }
  # Set the numbers of the empty tiles
  for (x in 1:ncol(grid_list)) {
    for (y in 1:nrow(grid_list)) {
      if (!grid_list[[x, y]]$is_mine) {
        grid_list[[x, y]]$num <- get_tiles(grid_list, x, y, TRUE)
      }
    }
  }
  #print_grid(grid_list)
  return(grid_list)
}

init_field <- function() {
  field <- list(
    texture = load_texture("assets/graphics/field.png"),
    grid_list = matrix(
      list(list(
        rect = rectangle(0, 0, 0, 0),
        revealed = FALSE, is_mine = FALSE,
        has_flag = FALSE, num = 0
      )), nrow=7, ncol=7
    ),
    mines_set = FALSE,
    num_color = c("blue", "darkgreen", "yellow", "purple", "red", "maroon", "orange", "gray"),
    victory = FALSE,
    width = 7, height = 7,
    offset_x = 182, offset_y = 60
  )
  for (x in 1:field$width) {
    for (y in 1:field$height) {
      field$grid_list[[x, y]]$rect <- rectangle(field$offset_x+(x*32), field$offset_y+(y*32), 32, 32)
    }
  }
  #field$grid_list <- init_grid(field$grid_list)
  return(field)
}

check_cell <- function(field, tile_x, tile_y) {
  field$grid_list[[tile_x, tile_y]]$revealed <- TRUE
  if (field$grid_list[[tile_x, tile_y]]$num == 0) {
    valid_tiles <- get_tiles(field$grid_list, tile_x, tile_y, FALSE)
    for (tile in valid_tiles) {
      field <- check_cell(field, tile$x, tile$y)
    }
  } else {
    field$victory <- TRUE
    for (x in 1:ncol(field$grid_list)) {
      for (y in 1:nrow(field$grid_list)) {
        if (field$grid_list[[x, y]]$num > 0 && !field$grid_list[[x, y]]$revealed) {
          field$victory <- FALSE
          break
        }
      }
    }
  }
  return(field)
}

draw_field <- function(field) {
  draw_texture(field$texture, 0, 0, "white")
  for (x in 1:field$width) {
    for (y in 1:field$height) {
      tile <- field$grid_list[[x, y]]
      draw_rectangle_rec(
        rectangle(tile$rect$x-3, tile$rect$y-3, tile$rect$width+6, tile$rect$height+6),
        "black"
      )
      if (tile$revealed) {
        draw_rectangle_rec(tile$rect, "lightgray")
        if (tile$num > 0) {
          draw_text(as.character(tile$num), tile$rect$x+8, tile$rect$y+2, 30, field$num_color[tile$num])
        } else if (tile$is_mine) {
          draw_text("*", tile$rect$x+8, tile$rect$y+2, 30, "black")
        }
      } else {
        draw_rectangle_rec(tile$rect, "darkgray")
      }
    }
  }
}

unload_field <- function(field) {
  unload_texture(field$texture)
}