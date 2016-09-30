context("FandV scheme cipher texts")

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

test_that("Large coefficient values", {
  p <- parsHelp("FandV", L=4, max=as.bigz(10)^27)
  keys <- keygen(p)
  ct <- enc(keys$pk, 1)
  for(i in 1:75) {
    ct <- ct + ct
  }
  
  expect_that(dec(keys$sk, ct), equals(as.bigz("37778931862957161709568")))
})
