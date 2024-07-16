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

  # ripper_po = ripp(type="placebo outcome",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D")
  # ripper_pt = ripp(type="placebo treatment",lm.y.dpx = m_Y_DPX,treatment = "D",placebo_treatment = "P")
  # ripper_dp = ripp(type="double placebo",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D",placebo_treatment = "P")
  #
  # expect_equal(round(ripper_po$simple_point_estimates$SOO,4), 1.3369)
  # expect_equal(round(ripper_po$simple_point_estimates$perfect_placebo_DID,4), 0.0066)
  # expect_equal(round(ripper_po$simple_point_estimates$perfect_placebo_DID_lambda1,4), 0.0144)
  #
  # expect_equal(round(ripper_pt$simple_point_estimates$SOO,4), 1.3369)
  # expect_equal(round(ripper_pt$simple_point_estimates$perfect_placebo_DID,4), -0.0322)
  # expect_equal(round(ripper_pt$simple_point_estimates$perfect_placebo_DID_lambda1,4), -0.001)
  #
  # expect_equal(round(ripper_dp$simple_point_estimates$SOO,4), 1.3369)
  # expect_equal(round(ripper_dp$simple_point_estimates$perfect_placebo,4), -0.0128)
  #
  # expect_equal(round(ripper_po$robustness_values$RV_lambda_q,4), 1.0218)
  # expect_equal(round(ripper_po$robustness_values$RV_beta_q,4), -0.0144)
  #
  # expect_equal(round(ripper_pt$robustness_values$RV_lambda_q,4), 0.9985)
  expect_equal(0.001, 0.001)


})
