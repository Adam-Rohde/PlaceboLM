test_that("simple_point_estimates gives correct values", {

  set.seed(0)
  n = 1000
  Z = stats::rnorm(n)
  X = stats::rnorm(n)
  D = Z + X + stats::rnorm(n)
  P = Z + X + stats::rnorm(n)
  Y = D + P + Z + X + stats::rnorm(n)
  N = D + P + Z + X + stats::rnorm(n)

  m_Y_DPX = lm(Y ~ D + P + X)
  m_N_DPX = lm(N ~ D + P + X)

  m_Y_DPXZ = lm(Y ~ D + P + X + Z)
  m_N_DPXZ = lm(N ~ D + P + X + Z)

  beta.yp.dxz = m_Y_DPXZ$coefficients[["P"]]
  beta.nd.pxz = m_N_DPXZ$coefficients[["D"]]
  beta.np.dxz = m_N_DPXZ$coefficients[["P"]]

  ripped_po = ripp(type="placebo outcome",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D")
  ripped_pt = ripp(type="placebo treatment",lm.y.dpx = m_Y_DPX,treatment = "D",placebo_treatment = "P")
  ripped_dp = ripp(type="double placebo",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D",placebo_treatment = "P")

  spe_po = simple_point_estimates(type="placebo outcome",ripped = ripped_po)
  spe_pt = simple_point_estimates(type="placebo treatment",ripped = ripped_pt)
  spe_dp = simple_point_estimates(type="double placebo",ripped = ripped_dp)


  expect_equal(round(spe_po$SOO,4), 1.3369)
  expect_equal(round(spe_po$perfect_placebo_DID,4), 0.0066)
  expect_equal(round(spe_po$perfect_placebo_DID_lambda1,4), 0.0144)

  expect_equal(round(spe_pt$SOO,4), 1.3369)
  expect_equal(round(spe_pt$perfect_placebo_DID,4), -0.0322)
  expect_equal(round(spe_pt$perfect_placebo_DID_lambda1,4), -0.001)

  expect_equal(round(spe_dp$SOO,4), 1.3369)
  expect_equal(round(spe_dp$perfect_placebo,4), -0.0128)

})
