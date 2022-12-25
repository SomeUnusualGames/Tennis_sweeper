ANIMATION_ID <- create.enum(c("1", "2", "3"), c("IDLE", "SERVE", "NONE"))

init_animation <- function(texture_path) {
  return (
    list(
      texture = load_texture(texture_path),
      current_frame = rectangle(0, 0, 0, 0),
      current_id = ANIMATION_ID$NONE,
      current_delay = 0.0,
      delay_index = 1
    )
  )
}

load_animation <- function(animation, id, rect_texture, frame_size, delay_between_frames, loop_once=FALSE, next_id=ANIMATION_ID$NONE) {
  animation[[id]] <- list(
    rect_texture = rect_texture,
    frame_size = frame_size,
    delay_between_frames = delay_between_frames,
    loop_once = loop_once,
    next_id = next_id
  )
  return(animation)
}

set_animation <- function(animation, id) {
  animation$current_id <- id
  animation$current_frame <- rectangle(
    animation[[id]]$rect_texture$x, animation[[id]]$rect_texture$y,
    animation[[id]]$frame_size[1], animation[[id]]$frame_size[2]
  )
  animation$delay_index <- 1
  animation$current_delay <- animation[[id]]$delay_between_frames[1]
  return(animation)
}

update_animation <- function(animation) {
  current_anim <- animation[[animation$current_id]]
  animation$current_delay <- animation$current_delay - get_frame_time()
  if (animation$current_delay <= 0) {
    animation$delay_index <- animation$delay_index + 1
    if (animation$delay_index > length(current_anim$delay_between_frames)) {
      if (current_anim$loop_once) {
        animation <- set_animation(animation, current_anim$next_id)
        current_anim <- animation[[animation$current_id]]
      }
      animation$delay_index <- 1
      animation$current_frame$x <- current_anim$rect_texture$x
      animation$current_frame$y <- current_anim$rect_texture$y
    } else {
      if (animation$current_frame$x + animation$current_frame$width < current_anim$rect_texture$width) {
        animation$current_frame$x <- animation$current_frame$x + animation$current_frame$width
      } else {
        animation$current_frame$x <- current_anim$rect_texture$x
      }
      if (animation$current_frame$y + animation$current_frame$height < current_anim$rect_texture$height) {
        animation$current_frame$y <- animation$current_frame$y + animation$current_frame$height
      } else {
        animation$current_frame$y <- current_anim$rect_texture$y
      }
    }
    animation$current_delay <- current_anim$delay_between_frames[animation$delay_index]
  }
  return(animation)
}

draw_animation <- function(animation, x, y, scale, angle, color) {
  current_frame <- animation$current_frame
  draw_texture_pro(
    animation$texture,
    current_frame,
    rectangle(x, y, current_frame$width*scale, current_frame$height*scale),
    c(current_frame$width %/% 2, current_frame$height %/% 2),
    angle,
    color
  )
}

# ANIMATION_ID -> [rectangle_texture, frame_size, delay_between_frames]