context("Output")
library(hetprior)

test_that("Output is as expected", {
  expect_equal(hetprior(2, "mean"), -4.06)
  expect_equal(is.numeric(hetprior(floor(runif(1, min=1, max=300)),"mean")),TRUE)
})

test_that("Arguments are required", {
  expect_error(hetprior(2))
  expect_error(hetprior("mean"))
})

test_that("A real prior ID is required", {
  expect_error(hetprior(2000, "mean"), "No prior available for this specification
          Double check the Prior ID")
})

test_that("Real prior ID is required", {
  expect_error(hetprior(2000, "mean"), "No prior available for this specification
          Double check the Prior ID")
})

test_that("'Normal' details are output correctly", {
expect_known_output(hetprior(2,"mean", details=TRUE),file = "../referenceoutput/normal")
# expect_known_output(hetprior(190,"shape", details=TRUE),file = "../referenceoutput/invgamma") # Need to fix UTF-8 encoding issue
  })

