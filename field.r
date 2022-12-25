init_field <- function() {
  field <- list(
    texture = load_texture("assets/graphics/field.png"),
    mineList = matrix(list(list(rect = rectangle(0, 0, 0, 0), discovered = FALSE)), nrow=7, ncol=7),
    width = 7,
    height = 7
  )
  for (x in 1:field$width) {
    for (y in 1:field$height) {
      field$mineList[[x, y]]$rect <- rectangle(160+(x*32)+(x*5), 25+(y*32)+(y*5), 32, 32)
    }
  }
  return(field)
}

draw_field <- function(field) {
  draw_texture(field$texture, 0, 0, "white")
  for (x in 1:field$width) {
    for (y in 1:field$height) {
      rect <- field$mineList[[x, y]]$rect
      draw_rectangle_rec(rectangle(rect$x-3, rect$y-3, rect$width+6, rect$height+6), "black")
      draw_rectangle_rec(field$mineList[[x, y]]$rect, "gray")
    }
  }
}

unload_field <- function(field) {
  unload_texture(field$texture)
}