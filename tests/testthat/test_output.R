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
normal <-  cat("\n",
          "Prior look-up results:", "\n",
          "Input:", "\n",
          "   Prior ID:                ", "2", "\n",
          "   Heterogeneity statistic: ", "Tau-squared", "\n",
          "   Data type:               ", "Binary", "\n",
          "   Effect measure:          ", "Log odds ratio", "\n",
          "   Distribution form:       ", "Log normal", "\n",
          "   Type of Intervention:    ", "Pharmacological vs placebo/control", "\n",
          "   Nature of outcome:       ", "All-cause mortality", "\n",
          "   Medical area:            ", "Any", "\n",
          "   Average sample size:     ", "Any", "\n", "\n",
          "Output:", "\n",
          "   Prior Mean      =  ", -4.06, "\n",
          "   Prior SD        =  ", 1.45, "\n",
          "   Prior variance  =  ", 2.103, "\n",
          "   Prior Median    =  ", 0.017, "\n",
          "   Low 95% CI      =  ", 0.001, "\n",
          "   High 95% CI     =  ", 0.3, "\n", "\n",
          "Notes:", "Notes: Fitted distribution reported as log-normal(u,o^2), where u and o are the Mean and SD presented on log scale. Median/range presented on untransformed scale.", "\n", "\n",
          "Reference:", " Reference: Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G. Thompson, and Julian PT Higgins. \"Predicting the extent of heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews.\" International Journal of Epidemiology 41, no. 3 (2012): 818-827. ", "\n")
expect_equal(hetprior(2,"mean", details=TRUE), normal)
  })

