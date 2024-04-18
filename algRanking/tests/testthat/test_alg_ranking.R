context("alg_ranking_mcttest")

A <-abs(rnorm(20,mean=.06,sd=.02))
B <- abs(rnorm(20,mean=.02,sd=.01))
C <- abs(rnorm(20,mean=.01,sd=.02))
data <- data.frame(A,B,C)

test_that("check that alg_ranking_mcttest function works without problems", {
  skip_on_cran()
  res <-alg_mcttest(data)
  res_ranking <-alg_ranking_mcttest(res)
  expect_equal(typeof(res_ranking$Algorithm),"character")
  expect_equal(is.numeric(res_ranking$Score),TRUE)
})

test_that("Check that the function returns values",
          {
            skip_on_cran()
            res <-alg_mcttest(data)
            expect_true( is.list(alg_ranking_mcttest(res)))
          })
