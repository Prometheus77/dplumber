#' @importFrom rlang !!
#' @importFrom rlang !!!

get_expr <- function(q) {
  formula <- rlang::eval_tidy(rlang::quo_get_expr(q), rlang::quo_get_env(q))
  rlang::expr(!!formula[[2]])
}
