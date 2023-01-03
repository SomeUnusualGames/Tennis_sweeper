MACHINE_STATE <- create.enum(
  c("1", "2", "3", "4", "5"),
  c("WAIT_TIMER", "SET_ANGLE", "SHOOT", "WAIT_BALL", "NONE")
)

init_ball_machine <- function() {
  return(
    list(
      texture = load_texture("assets/graphics/ball_machine.png"),
      angle = 180.0,
      current_angle = 180.0,
      ball_angle = 0.0,
      shoot_timer = 3.0,
      state = MACHINE_STATE$WAIT_TIMER,
      can_shoot = TRUE
    )
  )
}

update_ball_machine <- function(machine) {
  if (!machine$can_shoot) {
    return(machine)
  }
  if (machine$state == MACHINE_STATE$WAIT_TIMER) {
    machine$shoot_timer <- machine$shoot_timer - get_frame_time()
    if (machine$shoot_timer <= 0) {
      machine$state <- MACHINE_STATE$SET_ANGLE
      machine$ball_angle <- rand(75, 105)
      machine$angle <- scale_value(machine$ball_angle, 130.0, 200.0, 75.0, 105.0)
    }
  } else if (machine$state == MACHINE_STATE$SET_ANGLE) {
    machine$current_angle <- lerp(machine$current_angle, machine$angle, 0.1)
    if (abs(machine$current_angle - machine$angle) <= 0.01) {
      machine$state <- MACHINE_STATE$SHOOT
    }
  }

  return(machine)
}

draw_ball_machine <- function(machine) {
  draw_texture_pro(
    machine$texture,
    rectangle(0, 0, 56, 65), rectangle(280, -5, 56, 65),
    c(0, 0), 0.0, "white"
  )
  draw_texture_pro(
    machine$texture,
    rectangle(56, 0, 24, 20), rectangle(305, 57, 25, 20),
    c(12.5, 10), machine$current_angle, "white"
  )
}

reset_ball_machine <- function(machine) {
  machine$angle <- 180.0
  machine$current_angle <- 180.0
  machine$shoot_timer <- 3.0
  machine$state <- MACHINE_STATE$WAIT_TIMER
  machine$can_shoot <- TRUE
  return(machine)
}

unload_ball_machine <- function(machine) {
  unload_texture(machine$texture)
}