

library(genmap)

data(data.used)

context("Test of output of grid_couting_tax function")


test_that("grid_couting_tax", {

  res <- grid_couting_tax(data.used, long = "lon", lat = "Lat", sp = "ID_Species")

  expect_equal(3, length(res))


})

