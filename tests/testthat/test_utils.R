

test_that("utils", {
  testthat::expect_error(getEnv("not-exists", fail.on.empty = TRUE, refresh.env = TRUE))
  loggerSetupFile(tempfile())
  testthat::expect_true(nchar(getHost()) > 0)
})

