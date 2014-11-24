library("testthat")

test_that("isSorted data.frame", {
  x <- data.frame(a = runif(1000),b = runif(1000))
  x <- round(x,digits=2)
  expect_false(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  x <- x[order(x$a,x$b),]
  
  expect_true(isSorted(x,c("a","b")))
  expect_false(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  
  x <- x[order(x$a,-x$b),]
  expect_true(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  expect_false(isSorted(x,c("a","b")))
})

test_that("isSorted ffdf", {
  x <- data.frame(a = runif(20000000),b = runif(20000000))
  x <- round(x,digits=2)
  x <- as.ffdf(x)
  expect_false(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  x <- x[ffdforder(x[c("a","b")]),]
  
  expect_true(isSorted(x,c("a","b")))
  expect_false(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  
  x$minb <- 0-x$b
  x <- x[ffdforder(x[c("a","minb")]),]
  expect_true(isSorted(x,c("a","b"),c(TRUE,FALSE)))
  expect_false(isSorted(x,c("a","b")))
})