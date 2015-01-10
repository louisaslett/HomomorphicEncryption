context("FandV scheme cipher text matrices")

test_that("Matrices", {
  p <- pars("FandV")
  keys <- keygen(p)
  m <- matrix(1:9, 3)
  ct <- enc(keys$pk, m)
  
  expect_that(dec(keys$sk, ct), equals(m))
  expect_that(dec(keys$sk, ct[2]), equals(m[2]))
  expect_that(dec(keys$sk, ct[2,]), equals(m[2,]))
  expect_that(dec(keys$sk, ct[2,,drop=FALSE]), equals(m[2,,drop=FALSE]))
  expect_that(dec(keys$sk, ct[-2,]), equals(m[-2,]))
  expect_that(dec(keys$sk, ct[,2]), equals(m[,2]))
  expect_that(dec(keys$sk, ct[,2,drop=FALSE]), equals(m[,2,drop=FALSE]))
  expect_that(dec(keys$sk, ct[,-2]), equals(m[,-2]))
  expect_that(dec(keys$sk, ct[2,3]), equals(m[2,3]))
  expect_that(dec(keys$sk, ct[2,3]), equals(m[2,3]))
  expect_that(dec(keys$sk, ct[c(1,3),-2]), equals(m[c(1,3),-2]))
  expect_that(dec(keys$sk, ct[c(1,3),3]), equals(m[c(1,3),3]))
  expect_that(dec(keys$sk, ct[c(1,3),3,drop=FALSE]), equals(m[c(1,3),3,drop=FALSE]))
  expect_that(dim(ct), equals(c(3,3)))
  expect_that(length(ct), equals(9))
  
  ct[2,3] <- enc(keys$pk, 20)
  m[2,3] <- 20
  expect_that(dec(keys$sk, ct), equals(m))
  
  expect_that(dec(keys$sk, diag(ct)), equals(diag(m)))
  m2 <- 10:12
  ct2 <- enc(keys$pk, m2)
  diag(m) <- m2
  diag(ct) <- ct2
  expect_that(dec(keys$sk, ct), equals(m))
  diag(m) <- m2[1]
  diag(ct) <- ct2[1]
  expect_that(dec(keys$sk, ct), equals(m))
  
  m <- 1:2
  ct <- enc(keys$pk, m)
  expect_that(dec(keys$sk, diag(ct)), equals(diag(m)))
  expect_that(dec(keys$sk, diag(ct, 3)), equals(diag(m, 3)))
  expect_that(dec(keys$sk, diag(ct, ncol=3)), equals(diag(m, ncol=3)))
  expect_that(dec(keys$sk, diag(ct, 2, 1)), equals(diag(m, 2, 1)))
  expect_that(dec(keys$sk, diag(ct, 5, 7)), equals(diag(m, 5, 7)))
  
  m <- 1:9
  ct <- enc(keys$pk, m)
  expect_that(dec(keys$sk, matrix(ct)), equals(matrix(m)))
  expect_that(dec(keys$sk, matrix(ct, 3)), equals(matrix(m, 3)))
  expect_that(dec(keys$sk, matrix(ct, ncol=3)), equals(matrix(m, ncol=3)))
  expect_that(dec(keys$sk, matrix(ct, 3, 3)), equals(matrix(m, 3, 3)))
  expect_that(dec(keys$sk, matrix(ct, 3, byrow=TRUE)), equals(matrix(m, 3, byrow=TRUE)))
    
  m <- 20
  ct <- enc(keys$pk, m)
  expect_that(dec(keys$sk, matrix(ct)), equals(matrix(m)))
  expect_that(dec(keys$sk, matrix(ct, 3)), equals(matrix(m, 3)))
  expect_that(dec(keys$sk, matrix(ct, 3, 2)), equals(matrix(m, 3, 2)))
})

test_that("Matrix operations", {
  p <- pars("FandV")
  keys <- keygen(p)
  
  m <- 20
  m1 <- matrix(1:6,3)
  m2 <- matrix(1:6-6,3)
  
  ct <- enc(keys$pk, m)
  ct1 <- enc(keys$pk, m1)
  ct2 <- enc(keys$pk, m2)
  
  expect_that(dec(keys$sk, ct1+ct2), equals(m1+m2))
  expect_that(dec(keys$sk, ct1*ct2), equals(m1*m2))
  expect_that(dec(keys$sk, ct1+ct), equals(m1+m))
  expect_that(dec(keys$sk, ct+ct1), equals(m+m1))
  expect_that(dec(keys$sk, ct1*ct), equals(m1*m))
  expect_that(dec(keys$sk, ct*ct1), equals(m*m1))
  
  m1 <- matrix(2:7,3)
  m2 <- matrix(1:6-6,2)
  
  ct1 <- enc(keys$pk, m1)
  ct2 <- enc(keys$pk, m2)
  
  expect_that(dec(keys$sk, ct1%*%ct2), equals(m1%*%m2))
})
