test_that("revised_estimate gives correct values", {

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

  expect_equal(round(revised_estimate(type="placebo outcome",ripped = ripped_po,gamma = 1,lambda = 1,beta.nd.pxz=0),4), 0.0144)
  expect_equal(round(revised_estimate(type="placebo treatment",ripped = ripped_pt,gamma = 1,lambda = 1,beta.yp.dxz=0),4), -0.001)
  expect_equal(round(revised_estimate(type="double placebo",ripped = ripped_dp,beta.yp.dxz=0,beta.nd.pxz=0,beta.np.dxz=0),4), -0.0128)
  expect_equal(round(revised_estimate(type="placebo outcome",ripped = ripped_po,gamma = 1,lambda = 1,beta.nd.pxz=beta.nd.pxz),4), 0.993)
  expect_equal(round(revised_estimate(type="placebo treatment",ripped = ripped_pt,gamma = 1,lambda = 1,beta.yp.dxz=beta.yp.dxz),4), 1.0359)
  expect_equal(round(revised_estimate(type="double placebo",ripped = ripped_dp,beta.yp.dxz=beta.yp.dxz,beta.nd.pxz=beta.nd.pxz,beta.np.dxz=beta.np.dxz),4), 1)
})
