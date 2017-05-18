#' Homomorphic operations on ciphertexts
#' 
#' These special operations overload the standard behaviour of the arithmetic
#' operations to work instead homomorphically on ciphertexts.
#' 
#' Some arithmetic operations have been overloaded allowing use of the standard
#' operators on ciphertexts.  Therefore, if \code{ct1} and \code{ct2} are two
#' ciphertexts then the following will be handled automatically:
#' 
#' \code{ct1 + ct2} \cr
#' \code{ct1 - ct2} \cr
#' \code{ct1 * ct2}
#' 
#' Note that not all homomorphic encryption schemes will support all operations.
#' Also, it is important to note that typically homomorphic operations cause an
#' increase in the noise within a ciphertext.  Once a certain number of 
#' operations have taken place the cipertext may no longer correctly decrypt. If
#' a scheme is *fully* homomorphic, then it may be possible to apply a 
#' bootstrapping procedure which reduces the noise.
#' 
#' @name Arithmetic (+,-,*)
#' @aliases + - *
#' @rdname Arithmetic
#'   
#' @return A new ciphertext with the encrypted result of applying the operation
#' to the messages held by the two original cipertexts.
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

#' Vectors of cipher texts
#' 
#' Vectors of cipher texts can be seemlessly created and manipulated in the same
#' way one normally works with vectors in R.
#' 
#' Vectors of cipher texts can be seemlessly created either at encryption time
#' by passing a vector message argument to \code{\link{enc}}, or after encryption
#' using the standard concatenation operator \code{\link[base]{c}}:
#' 
#' \code{enc(keys$pk, c(m1, m2, ...))} \cr
#' \code{c(ct1, ct2)}
#' 
#' Standard operations which can be used on regular vectors can also be used
#' on vectors of cipher texts, where the scheme supports it:
#' 
#' \code{ct1 + ct2} \cr
#' \code{ct1 - ct2} \cr
#' \code{ct1 * ct2} \cr
#' \code{ct1 \%*\% ct2} \cr
#' \code{sum(ct1)} \cr
#' \code{prod(ct1)}
#' 
#' As with regular scalar operations, note that not all homomorphic encryption 
#' schemes will support all vector operations.
#' Also, it is important to note that typically homomorphic operations cause
#' an increase in the noise within a ciphertext.  Once a certain number of 
#' operations have taken place the cipertext may no longer correctly decrypt.
#' If a scheme is *fully* homomorphic, then it may be possible to apply a
#' bootstrapping procedure which reduces the noise.
#' 
#' @name Vectors
#' @aliases c sum prod
#' 
#' @examples
#' p <- pars("FandV")
#' keys <- keygen(p)
#' ct1 <- enc(keys$pk, c(2,3))
#' ct2 <- enc(keys$pk, c(4,5))
#' ctConcat <- c(ct1, ct2)
#' ctAdd <- ct1 + ct2
#' ctSub <- ct1 - ct2
#' ctMul <- ct1 * ct2
#' ctIP <- ct1 %*% ct2
#' 
#' # Decrypt to the equivalent of the above operations applied to the vectors
#' # c(2,3) and c(4,5)
#' dec(keys$sk, ctConcat)
#' dec(keys$sk, ctAdd)
#' dec(keys$sk, ctSub)
#' dec(keys$sk, ctMul)
#' dec(keys$sk, ctIP)
NULL

