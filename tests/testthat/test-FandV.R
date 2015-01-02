context("Fan and Vercauteren scheme")

test_that("Encryption", {
  p <- pars("FandV")
  keys <- keygen(p)
  ct1 <- enc(keys$pk, 21)
  ct2 <- enc(keys$pk, 32)
  ct3 <- enc(keys$pk, -43)
  
  expect_that(dec(keys$sk, ct1), equals(21))
  expect_that(dec(keys$sk, ct2), equals(32))
  expect_that(dec(keys$sk, ct3), equals(-43))
})

test_that("Addition", {
  p <- pars("FandV")
  keys <- keygen(p)
  ct1 <- enc(keys$pk, 2)
  ct2 <- enc(keys$pk, 3)
  ct3 <- enc(keys$pk, -4)
  
  expect_that(dec(keys$sk, ct1+ct2), equals(5))
  expect_that(dec(keys$sk, ct1+ct3), equals(-2))
  expect_that(dec(keys$sk, (ct1+ct2)+ct3), equals(1))
  expect_that(dec(keys$sk, ct1+(ct2+ct3)), equals(1))
})

test_that("Multiplication", {
  p <- pars("FandV")
  keys <- keygen(p)
  ct1 <- enc(keys$pk, 2)
  ct2 <- enc(keys$pk, 3)
  ct3 <- enc(keys$pk, -4)
  
  expect_that(dec(keys$sk, ct1*ct2), equals(6))
  expect_that(dec(keys$sk, ct1*ct3), equals(-8))
  expect_that(dec(keys$sk, (ct1*ct2)*ct3), equals(-24))
  expect_that(dec(keys$sk, ct1*(ct2*ct3)), equals(-24))
})

test_that("Vectors", {
  p <- pars("FandV")
  keys <- keygen(p)
  ct1 <- enc(keys$pk, 2)
  ct2 <- enc(keys$pk, 3)
  ct3 <- enc(keys$pk, -4)
  
  a <- c(ct1, ct2)
  a <- c(a, ct3)
  
  b <- enc(keys$pk, c(2,3,-4))
  
  expect_that(dec(keys$sk, a[2]), equals(3))
  expect_that(dec(keys$sk, a[3:1][3]), equals(2))
  expect_that(dec(keys$sk, a[-c(2,1)]), equals(-4))
  expect_that(dec(keys$sk, b), equals(c(2,3,-4)))
})

test_that("Vector operations", {
  p <- pars("FandV")
  keys <- keygen(p)
  ct1 <- enc(keys$pk, 2)
  ct2 <- enc(keys$pk, 3)
  ct3 <- enc(keys$pk, -4)
  
  a <- c(ct1, ct2)
  a <- c(a, ct3)
  
  ct1 <- enc(keys$pk, 5)
  ct2 <- enc(keys$pk, -2)
  ct3 <- enc(keys$pk, 6)
  
  b <- c(ct2, ct3)
  b <- c(ct1, b)
  
  ct <- enc(keys$pk, 2:4)
  
  expect_that(dec(keys$sk, (a+b)[1]), equals(7))
  expect_that(dec(keys$sk, (a+b)[2]), equals(1))
  expect_that(dec(keys$sk, (a+b)[3]), equals(2))
  expect_that(dec(keys$sk, (a*b)[1]), equals(10))
  expect_that(dec(keys$sk, (a*b)[2]), equals(-6))
  expect_that(dec(keys$sk, (a*b)[3]), equals(-24))
  expect_that(dec(keys$sk, (a*ct1)[1]), equals(10))
  expect_that(dec(keys$sk, (a*ct1)[2]), equals(15))
  expect_that(dec(keys$sk, (a*ct1)[3]), equals(-20))
  expect_that(dec(keys$sk, sum(ct)), equals(9))
  expect_that(dec(keys$sk, prod(ct)), equals(24))
  expect_that(dec(keys$sk, a%*%b), equals(-20))
})
