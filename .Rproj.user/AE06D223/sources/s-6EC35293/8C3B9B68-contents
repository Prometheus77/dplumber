#' Create a new aggregation using a pre-built formula
#'
#' @export
summarise_as <- function(.data, ...) {
  dots  <- rlang::quos(...)
  names <- purrr::map_chr(dots, ~as.character(rlang::quo_get_expr(.x)))
  names(dots) <- names

  exprs <- purrr::map(dots, get_expr)
  summarise(.data, !!!exprs)
}

summarize_as <- summarise_as
