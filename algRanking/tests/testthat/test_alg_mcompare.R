context("alg_mcompare")

A <-abs(rnorm(20,mean=.06,sd=.02))
B <- abs(rnorm(20,mean=.02,sd=.01))
C <- abs(rnorm(20,mean=.01,sd=.02))
data <- data.frame(A,B,C)
test_that("check that alg_mcompare function works without problems", {
  skip_on_cran()
  res <-alg_mcompare(data,bootstrapping=FALSE)
  expect_equal(typeof(res$Decision),"character")
  expect_equal(is.numeric(res$`Adjusted P-Value`),TRUE)
  expect_equal(typeof(res$Decision),"character")
  expect_equal(is.numeric(res$` Discrepancy Supported`),TRUE)
})
test_that("Check that the function returns values",
          {
            expect_true( is.list(alg_mcompare(data)))
          })

