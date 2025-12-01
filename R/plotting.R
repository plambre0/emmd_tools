#' @param emmd_out An object returned by `detect_emm()`.
#'
#' @return A base R plot.
#' @export
plot_emm <- function(emmd_out) {
  stopifnot(inherits(emmd_out, "emmd_result"))
  sel <- emmd_out$selected
  if (length(sel) == 0) stop("No selected effect modifiers to plot")

  est_ok <- Filter(Negate(is.null), emmd_out$estimates)
  if (length(est_ok) == 0) stop("No estimable interactions to plot")

  coefs <- sapply(est_ok, function(x) x[1, "Estimate"])
  ses   <- sapply(est_ok, function(x) x[1, "Std. Error"])

  df <- data.frame(modifier = names(coefs), estimate = coefs, se = ses, row.names = NULL)
  df <- df[order(df$estimate), ]

  y <- seq_len(nrow(df))
  plot(df$estimate, y,
       xlab = "Interaction estimate", ylab = "Modifier",
       xlim = range(df$estimate - 2 * df$se, df$estimate + 2 * df$se),
       yaxt = "n")
  axis(2, at = y, labels = df$modifier)
  arrows(df$estimate - 1.96 * df$se, y,
         df$estimate + 1.96 * df$se, y,
         angle = 90, code = 3, length = 0.05)
  abline(v = 0, lty = 2, col = "grey60")
}
