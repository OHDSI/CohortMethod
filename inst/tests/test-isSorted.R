library("testthat")

test_that("IsSorted", {
  x <- data.frame(a = runif(1000),b = runif(1000))
  x <- round(x,digits=2)
  expect_false(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  x <- x[order(x$a,-x$b),]
  
  expect_true(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  expect_false(isSorted(x,c("a","b"),c(TRUE,TRUE)))
})