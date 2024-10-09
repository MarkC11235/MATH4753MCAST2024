test_that("myncurve", {
  output <- capture.output(myncurve(0, 1, 2))
  expect_equal(output, c("$mu", "[1] 0", "", "$sigma", "[1] 1", "", "$area", "[1] 0.9772", ""))
})

test_that("myncurve", {
  output <- capture.output(myncurve(0, 1, 1))
  expect_equal(output, c("$mu", "[1] 0", "", "$sigma", "[1] 1", "", "$area", "[1] 0.8413", ""))
})

test_that("myncurve", {
  output <- capture.output(myncurve(5, 2, 3))
  expect_equal(output, c("$mu", "[1] 5", "", "$sigma", "[1] 2", "", "$area", "[1] 0.1587", ""))
})
