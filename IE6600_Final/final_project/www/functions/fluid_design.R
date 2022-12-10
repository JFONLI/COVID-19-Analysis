

fluid_design <- function(id) {
  fluidRow(
    div(
      id = id,
      column(
        width = 12,
        h2("Hello p1 fun")
      ),
      column(
        width = 6,
        h2("Hello p2 fun")
      ),
      column(
        width = 6,
        h2("Hello p3 fun")
      )
    )
  )
}
