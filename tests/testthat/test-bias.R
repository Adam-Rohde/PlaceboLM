test_that("bias gives correct values", {

  set.seed(0)
  n = 1000
  Z = stats::rnorm(n)
  X = stats::rnorm(n)
  D = Z + X + stats::rnorm(n)
  P = Z + X + stats::rnorm(n)
  Y = D + Z + X + stats::rnorm(n)
  N = Z + X + stats::rnorm(n)

  m_Y_DPX = lm(Y ~ D + P + X)
  m_N_DPX = lm(N ~ D + P + X)

  ripped_po = ripp(type="placebo outcome",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D")
  ripped_pt = ripp(type="placebo treatment",lm.y.dpx = m_Y_DPX,treatment = "D",placebo_treatment = "P")
  ripped_dp = ripp(type="double placebo",lm.y.dpx = m_Y_DPX,lm.n.dpx = m_N_DPX,treatment = "D",placebo_treatment = "P")

  expect_equal(round(bias(type="placebo outcome",ripped = ripped_po,gamma = 1,lambda = 1,beta.nd.pxz=0),4), 0.3284)
  expect_equal(round(bias(type="placebo treatment",ripped = ripped_pt,gamma = 1,lambda = 1,beta.yp.dxz=0),4), 0.3607)
  expect_equal(round(bias(type="double placebo",ripped = ripped_dp,beta.yp.dxz=0,beta.nd.pxz=0,beta.np.dxz=0),4), 0.3489)
})
