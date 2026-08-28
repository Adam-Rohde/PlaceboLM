# Edge-based structure resolution.
#
# The point of this interface is that a user who states their causal
# assumptions cannot silently land on the wrong row of the paper's taxonomy.
# These tests check the mapping, the two genuine ambiguities, and that every
# way of getting it wrong produces an error rather than a number.

d <- plm_dgp("placebo_outcome", n = 500)


test_that("every edge set resolves to the structure the taxonomy says", {
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = character(0),
               placebo_role = "outcome", quiet = TRUE)$structure,
    "placebo_outcome")
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = character(0),
               placebo_role = "treatment", quiet = TRUE)$structure,
    "placebo_treatment")
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = "D->P", quiet = TRUE)$structure,
    "placebo_outcome")
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = "P->Y",
               placebo_role = "treatment", quiet = TRUE)$structure,
    "placebo_treatment")
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = "P->Y",
               placebo_role = "outcome", quiet = TRUE)$structure,
    "observed_confounder_1")
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = "P->D", quiet = TRUE)$structure,
    "observed_confounder_2")
  expect_equal(
    placebo_lm(d, "Y", "D", "P", edges = "Y->P", quiet = TRUE)$structure,
    "post_outcome")
})


test_that("edges and structure give identical fits", {
  a <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                  structure = "observed_confounder_2")
  b <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                  edges = "P->D", quiet = TRUE)
  expect_equal(a$SF, b$SF)
  expect_equal(plm_estimate(a, k = 1), plm_estimate(b, k = 1))
  expect_equal(a$formulas, b$formulas)
})


test_that("arrow notation is forgiving about spacing, case and direction", {
  target <- "observed_confounder_1"
  for (e in list("P->Y", " p -> y ", "Y<-P", "P \u2192 Y", "Y \u2190 P"))
    expect_equal(
      placebo_lm(d, "Y", "D", "P", edges = e,
                 placebo_role = "outcome", quiet = TRUE)$structure,
      target, info = e)
})


test_that("edge order does not matter", {
  # Only relevant once more than one edge is supplied; the mediator set is the
  # available two-edge case, and it must refuse in either order.
  expect_error(placebo_lm(d, "Y", "D", "P", edges = c("D->P", "P->Y")),
               "do not recommend")
  expect_error(placebo_lm(d, "Y", "D", "P", edges = c("P->Y", "D->P")),
               "do not recommend")
})


test_that("the two genuinely ambiguous edge sets demand a role", {
  expect_error(placebo_lm(d, "Y", "D", "P", edges = character(0)),
               "admits more than one reading")
  expect_error(placebo_lm(d, "Y", "D", "P", edges = "P->Y"),
               "admits more than one reading")
  # and the message explains which two readings are on offer
  expect_error(placebo_lm(d, "Y", "D", "P", edges = "P->Y"), "Table 2\\[c\\]")
})


test_that("a role supplied where it cannot matter is reported, not silently used", {
  expect_message(
    placebo_lm(d, "Y", "D", "P", edges = "P->D", placebo_role = "outcome"),
    "ignored")
})


test_that("contradictory edges are rejected", {
  expect_error(placebo_lm(d, "Y", "D", "P", edges = c("D->P", "P->D")),
               "contradict")
  expect_error(placebo_lm(d, "Y", "D", "P", edges = c("P->Y", "Y->P")),
               "contradict")
})


test_that("edges are roles, not variable names", {
  # A user writing their actual column names gets told the convention rather
  # than an obscure no-match error.
  expect_error(placebo_lm(d, "Y", "D", "P", edges = "treat->re75"),
               "ROLES D \\(treatment\\), P \\(placebo\\) and Y \\(outcome\\)")
})


test_that("structure and edges cannot both be supplied", {
  expect_error(
    placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome",
               edges = "P->Y"),
    "not both")
})


test_that("the resolved structure is reported unless silenced", {
  expect_message(
    placebo_lm(d, "Y", "D", "P", edges = "Y->P"),
    "Post-Outcome")
  expect_message(
    placebo_lm(d, "Y", "D", "P", edges = "Y->P"),
    "Table 3\\[g\\], \\[h\\]")
  expect_silent(
    placebo_lm(d, "Y", "D", "P", edges = "Y->P", quiet = TRUE))
})


test_that("omitting both structure and edges still defaults to placebo_outcome", {
  expect_equal(placebo_lm(d, "Y", "D", "P")$structure, "placebo_outcome")
})


test_that("plm_edge_table() documents every mapping the resolver implements", {
  tab <- plm_edge_table()
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 7L)
  expect_setequal(unique(tab$structure), names(plm_structures))
  # Each row of the table must actually resolve as advertised.
  for (i in seq_len(nrow(tab))) {
    e <- if (tab$edges[i] == "(none)") character(0)
         else trimws(strsplit(tab$edges[i], ",")[[1]])
    role <- if (tab$placebo_role[i] == "(either)") NULL else tab$placebo_role[i]
    expect_equal(
      placebo_lm(d, "Y", "D", "P", edges = e, placebo_role = role,
                 quiet = TRUE)$structure,
      tab$structure[i], info = tab$edges[i])
  }
})
