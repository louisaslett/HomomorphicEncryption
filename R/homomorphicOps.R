#' Homomorphic operations on ciphertexts
#' 
#' These special operations overload the standard behaviour of the
#' arithmetic operations to work instead homomorphically on ciphertexts.
#' 
#' Note that not all homomorphic encryption schemes will support all operations.
#' Also, it is important to note that typically homomorphic operations cause
#' an increase in the noise within a ciphertext.  Once a certain number of 
#' operations have taken place the cipertext may no longer correctly decrypt.
#' If a scheme is *fully* homomorphic, then it may be possible to apply a
#' bootstrapping procedure which reduces the noise.
#' 
#' @name Arithmetic
#' 
#' @usage
#' ct1 + ct2
#' ct1 - ct2
#' ct1 * ct2
#' 
#' @param ct1,ct2 ciphertexts resulting from a call to \code{\link{enc}}
#' 
#' @return
#' A new ciphertext with the encrypted result of applying the operation to the
#' messages held by the two original cipertexts.
#' 
#' @examples
#' p <- pars("FandV")
#' keys <- keygen(p)
#' ct1 <- enc(keys$pk, 2)
#' ct2 <- enc(keys$pk, 3)
#' ctAdd <- ct1 + ct2
#' ctSub <- ct1 - ct2
#' ctMul <- ct1 * ct2
#' 
#' # Decrypt to 5, -1 and 6: the result of applying +, - and * to plain messages
#' dec(keys$sk, ctAdd)
#' dec(keys$sk, ctSub)
#' dec(keys$sk, ctMul)
NULL

# `+.Rcpp_FandV_ct` <- function(e1, e2) {
#   ct <- FandV_add(e1, e2)
#   
#   # Prepare return result
#   attr(ct, "FHEt") <- "ct"
#   attr(ct, "FHEs") <- "FandV"
#   ct
# }
# 
# `-.FandV` <- function(e1, e2) {
#   res <- list(c0=e1$c0-e2$c0, c1=e1$c1-e2$c1)
#   class(res) <- c("FandV", "FHE_ct")
#   res
# }
# 
# `*.FandV` <- function(e1, e2) {
#   q_t <- as.Int128(0)
#   q_t$twoPow(p$qpow-p$tpow) # <======== Hmmm, need parameters here!
#   
#   t <- as.Int128(0)
#   t$twoPow(p$tpow)
#   q <- as.Int128(0)
#   q$twoPow(p$qpow)
#   
#   res <- list(c0=t*(e1$c0*e2$c0)/q, c1=t*(e1$c0*e2$c1+e1$c1*e2$c0)/q, c2=t*(e1$c1*e2$c1)/q)
#   class(res) <- c("FandV", "FHE_ct")
#   res
# }
