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
  expect_that(dec(keys$sk, t(ct)), equals(t(m)))
  
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

test_that("Matrix binding", {
  p <- pars("FandV")
  keys <- keygen(p)
  
  mS <- 1
  mV1 <- 2:3
  mV2 <- 4:6
  mM1 <- matrix(7:10,2)
  mM2 <- matrix(11:19,3)
  
  ctS <- enc(keys$pk, mS)
  ctV1 <- enc(keys$pk, mV1)
  ctV2 <- enc(keys$pk, mV2)
  ctM1 <- enc(keys$pk, mM1)
  ctM2 <- enc(keys$pk, mM2)
  
  # rbind
  expect_that(dec(keys$sk, rbind(ctS)), is_equivalent_to(rbind(mS)))
  expect_that(dec(keys$sk, rbind(ctS, NULL)), is_equivalent_to(rbind(mS, NULL)))
  expect_that(dec(keys$sk, rbind(NULL, ctS)), is_equivalent_to(rbind(NULL, mS)))
  expect_that(dec(keys$sk, rbind(ctV1)), is_equivalent_to(rbind(mV1)))
  expect_that(dec(keys$sk, rbind(ctV1, NULL)), is_equivalent_to(rbind(mV1,NULL)))
  expect_that(dec(keys$sk, rbind(NULL, ctV1)), is_equivalent_to(rbind(NULL, mV1)))
  expect_that(dec(keys$sk, rbind(ctM1)), is_equivalent_to(rbind(mM1)))
  expect_that(dec(keys$sk, rbind(ctM1,NULL)), is_equivalent_to(rbind(mM1,NULL)))
  expect_that(dec(keys$sk, rbind(NULL, ctM1)), is_equivalent_to(rbind(NULL, mM1)))
  expect_that(dec(keys$sk, rbind(ctM1, ctM1)), is_equivalent_to(rbind(mM1, mM1)))
  expect_that(dec(keys$sk, rbind(ctM1, ctV1)), is_equivalent_to(rbind(mM1, mV1)))
  expect_that(dec(keys$sk, rbind(ctM1, ctV2)), gives_warning())# rbind(mM1, mV2)))
  expect_that(dec(keys$sk, rbind(ctV1, ctM1)), is_equivalent_to(rbind(mV1, mM1)))
  expect_that(dec(keys$sk, rbind(ctV1, ctM2)), gives_warning())# rbind(mV1, mM2)))
  expect_that(dec(keys$sk, rbind(ctS, ctM2)), is_equivalent_to(rbind(mS, mM2)))
  expect_that(dec(keys$sk, rbind(ctM2, ctS)), is_equivalent_to(rbind(mM2, mS)))
  expect_that(dec(keys$sk, rbind(ctS, ctV1, ctM2)), gives_warning())# rbind(mS, mV1, mM2)))
  expect_that(dec(keys$sk, rbind(ctM2, ctS, ctV1)), is_equivalent_to(rbind(mM2, mS, mV1)))
  
  # cbind
  expect_that(dec(keys$sk, cbind(ctS)), is_equivalent_to(cbind(mS)))
  expect_that(dec(keys$sk, cbind(ctS, NULL)), is_equivalent_to(cbind(mS, NULL)))
  expect_that(dec(keys$sk, cbind(NULL, ctS)), is_equivalent_to(cbind(NULL, mS)))
  expect_that(dec(keys$sk, cbind(ctV1)), is_equivalent_to(cbind(mV1)))
  expect_that(dec(keys$sk, cbind(ctV1, NULL)), is_equivalent_to(cbind(mV1,NULL)))
  expect_that(dec(keys$sk, cbind(NULL, ctV1)), is_equivalent_to(cbind(NULL, mV1)))
  expect_that(dec(keys$sk, cbind(ctM1)), is_equivalent_to(cbind(mM1)))
  expect_that(dec(keys$sk, cbind(ctM1,NULL)), is_equivalent_to(cbind(mM1,NULL)))
  expect_that(dec(keys$sk, cbind(NULL, ctM1)), is_equivalent_to(cbind(NULL, mM1)))
  expect_that(dec(keys$sk, cbind(ctM1, ctM1)), is_equivalent_to(cbind(mM1, mM1)))
  expect_that(dec(keys$sk, cbind(ctM1, ctV1)), is_equivalent_to(cbind(mM1, mV1)))
  expect_that(dec(keys$sk, cbind(ctM1, ctV2)), gives_warning())# cbind(mM1, mV2)))
  expect_that(dec(keys$sk, cbind(ctV1, ctM1)), is_equivalent_to(cbind(mV1, mM1)))
  expect_that(dec(keys$sk, cbind(ctV1, ctM2)), gives_warning())# cbind(mV1, mM2)))
  expect_that(dec(keys$sk, cbind(ctS, ctM2)), is_equivalent_to(cbind(mS, mM2)))
  expect_that(dec(keys$sk, cbind(ctM2, ctS)), is_equivalent_to(cbind(mM2, mS)))
  expect_that(dec(keys$sk, cbind(ctS, ctV1, ctM2)), gives_warning())# cbind(mS, mV1, mM2)))
  expect_that(dec(keys$sk, cbind(ctM2, ctS, ctV1)), is_equivalent_to(cbind(mM2, mS, mV1)))
})
