# The registry is the specification. These tests check that it faithfully
# encodes Tables 1 and 2 of Rohde and Hazlett.

test_that("every registry entry is complete", {
  for (nm in names(plm_structures)) {
    s <- plm_structures[[nm]]
    expect_type(s$label, "character")
    expect_type(s$paper_ref, "character")
    expect_type(s$describe, "character")
    expect_type(s$sens_param, "character")
    expect_true(is.function(s$regressions), info = nm)
    expect_true(is.function(s$target_coef), info = nm)
    expect_true(is.function(s$sens_coef),   info = nm)
    expect_true(is.function(s$sens_expr),   info = nm)
  }
})

test_that("plm_structure_table() lists every structure", {
  tab <- plm_structure_table()
  expect_s3_class(tab, "data.frame")
  expect_setequal(tab$structure, names(plm_structures))
  expect_equal(nrow(tab), 5L)
})

test_that("registry produces the regressions the paper specifies", {
  v <- list(Y = "Y", D = "D", P = "P", X = "X")
  f <- function(nm) lapply(plm_structures[[nm]]$regressions(v), deparse1)

  # Table 1[a],[b] Placebo Outcome: Y ~ D + X and P ~ D + X
  expect_equal(unname(unlist(f("placebo_outcome"))),
               c("Y ~ D + X", "P ~ D + X"))

  # Table 1[a],[c] Placebo Treatment: single regression Y ~ D + P + X
  expect_equal(unname(unlist(f("placebo_treatment"))), "Y ~ D + P + X")

  # Table 1[c] Observed Confounder 1: Y ~ D + P + X and P ~ D + X
  expect_equal(unname(unlist(f("observed_confounder_1"))),
               c("Y ~ D + P + X", "P ~ D + X"))

  # Table 2[e],[f] Observed Confounder 2: Y ~ D + P + X and D ~ P + X
  expect_equal(unname(unlist(f("observed_confounder_2"))),
               c("Y ~ D + P + X", "D ~ P + X"))

  # Table 2[g],[h] Post-Outcome: Y ~ D + X and P ~ Y + D + X
  expect_equal(unname(unlist(f("post_outcome"))),
               c("Y ~ D + X", "P ~ Y + D + X"))
})

test_that("mediator structures are refused with the paper's reasoning", {
  d <- plm_test_data()
  expect_error(
    placebo_lm(d, "Y", "D", "P", structure = "mediator"),
    "do not recommend"
  )
  expect_error(
    placebo_lm(d, "Y", "D", "P", structure = "mediator"),
    "Zhang and Ding"
  )
})
