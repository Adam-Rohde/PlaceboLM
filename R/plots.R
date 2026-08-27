# Plots ------------------------------------------------------------------------
#
# Both plotting functions return the data they drew, invisibly, so that a user
# who wants a ggplot2 version does not have to re-run the bootstrap to get at
# the numbers.


#' Line plot of the adjusted estimate against relative confounding
#'
#' @description
#' Plots the partially identified estimate as a function of the
#' relative-confounding parameter, with a bootstrap confidence band, and marks
#' the three benchmark estimates. One panel is drawn per value of
#' `imperfection`.
#'
#' @param fit A `placebo_lm` object.
#' @param k,m Numeric vector of length 2 giving the range to plot on the x-axis,
#'   or a longer vector of explicit values. Supply exactly one. Defaults to
#'   `k = c(0, 2)`.
#' @param imperfection Numeric vector of placebo-imperfection values; one panel
#'   is drawn per value. Defaults to `0`.
#' @param gran Integer. Number of x-axis points per panel. Defaults to `25`.
#' @param n_boot Non-negative integer. Bootstrap replicates. Defaults to `500`.
#' @param alpha Numeric. Significance level. Defaults to `0.05`.
#' @param ci_type `"percentile"` or `"normal"`.
#' @param benchmarks Logical. Mark the benchmark estimates. Defaults to `TRUE`.
#' @param cores Integer. Cores for the bootstrap.
#' @param ... Passed to [graphics::plot()].
#'
#' @return Invisibly, the data frame of plotted values.
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#' plm_line_plot(fit, k = c(0, 2), n_boot = 50)
#'
#' @export
plm_line_plot <- function(fit, k = NULL, m = NULL, imperfection = 0,
                          gran = 25, n_boot = 500, alpha = 0.05,
                          ci_type = c("percentile", "normal"),
                          benchmarks = TRUE, cores = NULL, ...) {
  .plm_check_fit(fit)
  ci_type <- match.arg(ci_type)
  if (is.null(k) && is.null(m)) k <- c(0, 2)

  on_m   <- is.null(k)
  supplied <- if (on_m) m else k
  x_vals <- if (length(supplied) == 2L)
    seq(supplied[1], supplied[2], length.out = gran) else supplied

  dat <- if (on_m)
    plm_grid(fit, m = x_vals, imperfection = imperfection, n_boot = n_boot,
             alpha = alpha, ci_type = ci_type, cores = cores)
  else
    plm_grid(fit, k = x_vals, imperfection = imperfection, n_boot = n_boot,
             alpha = alpha, ci_type = ci_type, cores = cores)

  xcol  <- if (on_m) "m" else "k"
  xlab  <- if (on_m) "m (raw ratio of biases)" else "k (relative confounding)"
  has_ci <- "ci_lower" %in% names(dat)

  ylim <- if (has_ci) range(dat$ci_lower, dat$ci_upper) else range(dat$estimate)
  bm <- if (benchmarks) plm_benchmarks(fit) else NULL
  if (!is.null(bm)) ylim <- range(ylim, bm$estimate)

  imps <- unique(dat$imperfection)
  for (im in imps) {
    d <- dat[dat$imperfection == im, ]
    d <- d[order(d[[xcol]]), ]

    main <- if (length(imps) > 1L)
      paste0(fit$spec$sens_param, " = ", signif(im, 4)) else NULL

    graphics::plot(d[[xcol]], d$estimate, type = "n",
                   xlab = xlab, ylab = "Adjusted estimate",
                   main = main, ylim = ylim, ...)

    if (has_ci) {
      graphics::polygon(c(d[[xcol]], rev(d[[xcol]])),
                        c(d$ci_lower, rev(d$ci_upper)),
                        col = "lightsteelblue1", border = NA)
      graphics::lines(d[[xcol]], d$ci_lower, col = "blue", lty = 2)
      graphics::lines(d[[xcol]], d$ci_upper, col = "blue", lty = 2)
    }
    graphics::abline(h = 0, col = "red",  lwd = 2)
    graphics::abline(v = 0, col = "gray", lwd = 1)
    graphics::lines(d[[xcol]], d$estimate, lwd = 2)

    # Benchmarks are drawn only on the perfect-placebo panel, where they are
    # defined; on other panels they would be misleading.
    if (!is.null(bm) && isTRUE(all.equal(im, 0))) {
      bx <- if (on_m) bm$m else bm$k
      graphics::points(bx, bm$estimate,
                       col = c("navy", "darkgreen", "blue"),
                       pch = c(18, 15, 17), cex = 1.6)
      graphics::legend(
        "topright", bty = "o", bg = "white",
        legend = paste0(bm$benchmark, " = ", signif(bm$estimate, 4)),
        col = c("navy", "darkgreen", "blue"),
        pch = c(18, 15, 17)
      )
    }
  }

  invisible(dat)
}


#' Contour plot over relative confounding and placebo imperfection
#'
#' @description
#' Draws the adjusted estimate as a contour surface over the two sensitivity
#' parameters, with the zero contour highlighted -- the set of assumptions at
#' which the sign of the estimated effect changes.
#'
#' @param fit A `placebo_lm` object.
#' @param k,m Numeric vector of length 2, the x-axis range. Supply exactly one.
#'   Defaults to `k = c(0, 2)`.
#' @param imperfection Numeric vector of length 2, the y-axis range. Defaults to
#'   a symmetric range around zero scaled to the sensitivity coefficient.
#' @param gran Integer. Grid points per axis. Defaults to `60`.
#' @param nlevels Integer. Number of contour levels. Defaults to `20`.
#' @param benchmarks Logical. Mark the benchmark estimates. Defaults to `TRUE`.
#' @param ... Passed to [graphics::contour()].
#'
#' @return Invisibly, a list with `x`, `y`, and `z`.
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#' plm_contour_plot(fit)
#'
#' @export
plm_contour_plot <- function(fit, k = NULL, m = NULL, imperfection = NULL,
                             gran = 60, nlevels = 20, benchmarks = TRUE, ...) {
  .plm_check_fit(fit)
  if (is.null(k) && is.null(m)) k <- c(0, 2)
  on_m <- is.null(k)

  supplied <- if (on_m) m else k
  if (length(supplied) != 2L)
    stop("`", if (on_m) "m" else "k", "` must be length 2, c(min, max).",
         call. = FALSE)

  if (is.null(imperfection)) {
    scale <- abs(fit$coefs$sens$estimate)
    if (!is.finite(scale) || scale == 0) scale <- 1
    imperfection <- c(-scale, scale)
  }
  if (length(imperfection) != 2L)
    stop("`imperfection` must be length 2, c(min, max).", call. = FALSE)

  xv <- seq(supplied[1], supplied[2], length.out = gran)
  yv <- seq(imperfection[1], imperfection[2], length.out = gran)
  kv <- if (on_m) xv / fit$SF else xv

  # z[i, j] is the estimate at (xv[i], yv[j]), the orientation contour() wants.
  z <- outer(kv, yv, function(kk, ii) plm_estimate(fit, k = kk,
                                                   imperfection = ii))

  xlab <- if (on_m) "m (raw ratio of biases)" else "k (relative confounding)"
  graphics::contour(xv, yv, z, method = "edge", nlevels = nlevels,
                    xlab = xlab, ylab = fit$spec$sens_expr(fit$vars),
                    col = "black", ...)
  graphics::contour(xv, yv, z, add = TRUE, levels = 0, labels = "0",
                    col = "red", lwd = 2, method = "edge")

  if (benchmarks) {
    bm <- plm_benchmarks(fit)
    bx <- if (on_m) bm$m else bm$k
    graphics::points(bx, rep(0, nrow(bm)),
                     col = c("navy", "darkgreen", "blue"),
                     pch = c(18, 15, 17), cex = 1.6)
    graphics::legend(
      "topright", bty = "o", bg = "white",
      legend = paste0(bm$benchmark, " = ", signif(bm$estimate, 4)),
      col = c("navy", "darkgreen", "blue"), pch = c(18, 15, 17)
    )
  }

  invisible(list(x = xv, y = yv, z = z))
}
