#' Create a new field using a pre-built formula
#'
#' @export
mutate_as <- function(.data, ...) {
  dots  <- rlang::quos(...)
  names <- purrr::map_chr(dots, ~as.character(rlang::quo_get_expr(.x)))
  names(dots) <- names

  exprs <- purrr::map(dots, get_expr)
  mutate(.data, !!!exprs)
}
