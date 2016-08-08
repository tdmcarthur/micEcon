
library(micEconIndex)
library(stringr)
context("EKS transitivity correction")


test_that("str_length is number of characters", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})


priceQuantMat <- read.csv("~/svn/micEcon/pkg/micEconIndex/tests/testthat/priceQuantMat.txt")

micEconResult <- quantityIndex(
    colnames(priceQuantMat)[substr(colnames(priceQuantMat), 1, 1) == "P"],
    colnames(priceQuantMat)[substr(colnames(priceQuantMat), 1, 1) == "Q"],
    1, priceQuantMat, method = "Fisher", EKS = TRUE)

micEconResult <- unname(micEconResult)
micEconResult <- round(micEconResult, 4)

load("~/svn/micEcon/pkg/micEconIndex/tests/testthat/TFPIPcheck.Rdata", verbose = TRUE)
# TFPIPresult

test_that("Fisher transitive index works", {
  expect_equal(micEconResult, TFPIPresult)
})

#cor(micEconResult, TFPIPresult)


#printIndices( "p",  c( "p.beef", "p.veal", "p.pork" ),
#   c( "q.beef", "q.veal", "q.pork" ), 1, Missong03E7.7 )
