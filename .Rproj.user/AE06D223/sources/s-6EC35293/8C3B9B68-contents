#' Create a new aggregation using a pre-built formula
#'
#' @export
summarise_as <- function(.data, ...) {
  dots  <- rlang::quos(...)
  names <- purrr::map_chr(dots, ~as.character(rlang::quo_get_expr(.x)))
  names(dots) <- names

  get_expr <- function(q) {
    formula <- rlang::eval_tidy(rlang::quo_get_expr(q), rlang::quo_get_env(q))
    rlang::expr(!!formula[[2]])
  }

  exprs <- purrr::map(dots, rlang::get_expr)
  dplyr::summarise(.data, !!!exprs)
}

summarize_as <- summarise_as
