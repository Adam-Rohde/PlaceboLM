# Triangulation ----------------------------------------------------------------
#
# Two different operations get called "triangulation", and the paper does both.
# They are kept separate here because they answer different questions.
#
#   plm_triangulate()         several DIFFERENT placebo variables, each giving
#                             its own bound on the same effect. Intersecting
#                             those bounds is the paper's NSW exercise, which
#                             combines 1974 earnings, 1975 earnings and 1975
#                             employment.
#
#   plm_compare_structures()  ONE placebo variable read under several structural
#                             assumptions. The paper notes that some graphs
#                             admit more than one reading -- its Table 2[c] can
#                             be analysed as an imperfect placebo treatment or
#                             as an observed confounder -- and that comparing
#                             them is informative.


#' Intersect bounds from several placebos
#'
#' @description
#' Given fits that use different placebo variables for the same treatment and
#' outcome, reports each one's bound and their intersection. An effect must lie
#' in every placebo's bound to be consistent with all of the assumptions
#' simultaneously, so the intersection is generally tighter than any single
#' bound while resting only on assumptions defended for each placebo separately.
#'
#' @param fits A named list of `placebo_lm` objects sharing the same outcome,
#'   treatment and covariates, and fitted to the same data.
#' @param k,m Either a length-2 range `c(min, max)` applied to every placebo, or
#'   a named list of length-2 ranges, one per element of `fits`. Supply exactly
#'   one of `k` or `m`. Note `m` is not comparable across placebos on different
#'   scales; `k` is.
#' @param imperfection Numeric of length 1 or 2, or a named list of these, one
#'   per fit. Defaults to `0`.
#' @param n_boot Non-negative integer. Bootstrap replicates used for each
#'   placebo's own uncertainty columns. `0` (default) reports point bounds only.
#' @param ... Passed to [plm_bounds()].
#'
#' @section What the intersection is, and is not:
#' The `intersection` row intersects the **point** bounds. It is not a
#' confidence region, and the per-placebo `*_boot_q` columns are deliberately
#' not combined into one: the fits share the same outcome, treatment and rows,
#' so their sampling errors are dependent, and treating them as independent
#' would understate uncertainty. Read the intersection as "the effects
#' consistent with all the assumed ranges", and each placebo's bootstrap columns
#' as that placebo's own sampling variability.
#'
#' An empty intersection is informative rather than an error: it means no single
#' effect is consistent with every range supplied, so at least one of the
#' assumed ranges must be wrong.
#'
#' @return A data frame with one row per placebo plus a final `intersection`
#'   row, with columns `placebo`, `k_low`, `k_high`, `lower`, `upper`, and --
#'   when `n_boot > 0` -- `lower_boot_q` and `upper_boot_q`.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' U <- rnorm(n); D <- U + rnorm(n)
#' Y <- D + U + rnorm(n)
#' dat <- data.frame(Y = Y, D = D,
#'                   P1 = U + rnorm(n), P2 = U + rnorm(n))
#'
#' f1 <- placebo_lm(dat, "Y", "D", "P1", structure = "placebo_outcome")
#' f2 <- placebo_lm(dat, "Y", "D", "P2", structure = "placebo_outcome")
#' plm_triangulate(list(P1 = f1, P2 = f2), k = c(0.5, 1))
#'
#' @export
plm_triangulate <- function(fits, k = NULL, m = NULL, imperfection = 0,
                            n_boot = 0, ...) {
  if (!is.list(fits) || length(fits) < 2L)
    stop("`fits` must be a list of at least two `placebo_lm` objects.",
         call. = FALSE)
  if (is.null(names(fits)) || any(!nzchar(names(fits))))
    stop("`fits` must be named, so each bound can be attributed to a placebo.",
         call. = FALSE)
  for (nm in names(fits)) .plm_check_fit(fits[[nm]])

  # The exercise only means anything if these are alternative placebos for the
  # SAME analysis.
  ref <- fits[[1]]
  for (nm in names(fits)[-1]) {
    f <- fits[[nm]]
    if (!identical(f$vars$Y, ref$vars$Y) || !identical(f$vars$D, ref$vars$D))
      stop("All fits must share the same outcome and treatment; '", nm,
           "' does not.", call. = FALSE)
    if (!identical(f$vars$X, ref$vars$X))
      stop("All fits must use the same covariates; '", nm, "' does not.",
           call. = FALSE)
    if (!identical(nrow(f$data), nrow(ref$data)))
      stop("All fits must be fitted to the same data; '", nm,
           "' has a different number of rows.", call. = FALSE)
  }
  if (anyDuplicated(vapply(fits, function(f) f$vars$P, character(1))))
    stop("Two fits use the same placebo variable; triangulation needs ",
         "different ones.", call. = FALSE)

  # Check this here rather than letting it surface from inside plm_bounds() on
  # the first fit, so the message names the argument the caller actually passed.
  if (is.null(k) && is.null(m))
    stop("Supply exactly one of `k` or `m`, as a length-2 range or a named ",
         "list of them.\n  `k` is scale-free and so comparable across ",
         "placebos on different scales; `m` is not.", call. = FALSE)
  if (!is.null(k) && !is.null(m))
    stop("Supply exactly one of `k` or `m`, not both.", call. = FALSE)

  per_fit <- function(arg, nm) {
    if (is.list(arg)) {
      if (is.null(arg[[nm]]))
        stop("`", nm, "` is missing from a per-placebo argument list.",
             call. = FALSE)
      arg[[nm]]
    } else arg
  }

  rows <- lapply(names(fits), function(nm) {
    b <- plm_bounds(fits[[nm]],
                    k = if (is.null(k)) NULL else per_fit(k, nm),
                    m = if (is.null(m)) NULL else per_fit(m, nm),
                    imperfection = per_fit(imperfection, nm),
                    n_boot = n_boot, ...)
    cbind(placebo = fits[[nm]]$vars$P, b, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- names(fits)

  lo <- max(out$lower); hi <- min(out$upper)
  empty <- lo > hi
  if (empty)
    warning("The bounds do not overlap: no effect is consistent with every ",
            "range supplied,\nso at least one of the assumed ranges must be ",
            "wrong.", call. = FALSE)

  inter <- out[1, , drop = FALSE]
  inter[] <- NA
  inter$placebo <- "(intersection)"
  inter$lower <- if (empty) NA_real_ else lo
  inter$upper <- if (empty) NA_real_ else hi
  rownames(inter) <- "intersection"

  rbind(out, inter)
}


#' Compare structural readings of one placebo
#'
#' @description
#' Fits the same outcome, treatment and placebo under several assumed causal
#' structures and tabulates what each implies. Useful where a graph admits more
#' than one reading -- the paper's Table 2[c] can be analysed either as an
#' imperfect placebo treatment or as an observed confounder -- and the
#' comparison is itself informative.
#'
#' The structures are *different assumptions*, not competing estimates of one
#' quantity. Where they disagree, the disagreement measures how much the
#' conclusion depends on the causal reading, which is not something the data can
#' settle.
#'
#' @param data,outcome,treatment,placebo,covariates As in [placebo_lm()].
#' @param structures Character vector of structures to compare. Defaults to all
#'   supported ones.
#' @param k Numeric scalar at which to report a comparable estimate. Defaults to
#'   `1` (equiconfounding after rescaling). `k` is used rather than `m` because
#'   `m` is not comparable across structures with different scale factors.
#' @param imperfection Numeric scalar. Defaults to `0`.
#'
#' @return A data frame with one row per structure and columns `structure`,
#'   `paper_ref`, `sens_param`, `SF`, `at_k0`, `at_k1`, `m_equals_1`, and
#'   `tipping_k`.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + P + U + rnorm(n)
#' dat <- data.frame(Y = Y, D = D, P = P)
#'
#' plm_compare_structures(dat, "Y", "D", "P")
#'
#' @export
plm_compare_structures <- function(data, outcome, treatment, placebo,
                                   covariates = NULL,
                                   structures = names(plm_structures),
                                   k = 1, imperfection = 0) {
  bad <- setdiff(structures, names(plm_structures))
  if (length(bad))
    stop("Unknown structure(s): ", paste(bad, collapse = ", "), ".\n",
         "See plm_structure_table() for the supported set.", call. = FALSE)

  rows <- lapply(structures, function(s) {
    fit <- placebo_lm(data, outcome, treatment, placebo,
                      covariates = covariates, structure = s)
    tip <- tryCatch(plm_solve(fit, target = 0,
                              imperfection = imperfection)$k,
                    error = function(e) NA_real_)
    data.frame(
      structure  = s,
      paper_ref  = fit$spec$paper_ref,
      sens_param = fit$spec$sens_param,
      SF         = fit$SF,
      at_k0      = plm_estimate(fit, k = 0, imperfection = imperfection),
      at_k1      = plm_estimate(fit, k = k, imperfection = imperfection),
      m_equals_1 = plm_estimate(fit, m = 1, imperfection = imperfection),
      tipping_k  = tip,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
