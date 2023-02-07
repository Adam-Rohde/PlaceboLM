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

  ripper_po = ripp(type="placebo outcome",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D")
  ripper_pt = ripp(type="placebo treatment",lm.y.dpx = m_Y_DPX,treatment = "D",placebo_treatment = "P")
  ripper_dp = ripp(type="double placebo",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D",placebo_treatment = "P")

  rvs_po = robustness_values(ripper = ripper_po,gamma = 1,lambda = 1,beta.nd.pxz=beta.nd.pxz,q=1)
  rvs_pt = robustness_values(ripper = ripper_pt,gamma = 1,lambda = 1,beta.yp.dxz=beta.yp.dxz,q=1)

  expect_equal(round(rvs_po$RV_lambda_q,4), 15.1104)
  expect_equal(round(rvs_po$RV_beta_q,4), -0.0144)

  expect_equal(round(rvs_pt$RV_lambda_q,4), 19.7294)
  expect_equal(round(rvs_pt$RV_beta_q,4), 0.001)

})
