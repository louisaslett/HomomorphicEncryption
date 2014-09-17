#' Setup encryption scheme parameters
#' 
#' Use this function to create an encryption scheme parameters object.
#' 
#' Currently only the scheme of Fan and Vercauteren (\code{"FandV"}) is implemented.
#' 
#' For "FandV" you may specify:
#' \describe{
#'   \item{\code{d}}{power of the cyclotomic polynomial ring (default 4096);}
#'   \item{\code{sigma}}{the standard deviation of the discrete Gaussian used to 
#' induce a distribution on the cyclotomic polynomial ring (default 16.0);}
#'   \item{\code{qpow}}{the power of 2 to use for the coefficient modulus (default 128);}
#'   \item{\code{tpow}}{the power of 2 to use for the encoding modulus (default 15).}
#' }
#' 
#' This function simply sets up the parameters which must be specified to use a
#' particular encryption scheme.  Using the scheme then requires generating
#' cryptographic keys.
#' 
#' @param scheme the scheme for which to create a parameter object.  Currently
#' only Fan and Vercauteren's scheme is supported by specifying \code{"FandV"}.
#' 
#' @param ... pass the specific options for the chosen scheme as named arguments
#' to override any default values.  See the details section for options for
#' encryption schemes currently implemented.
#' 
#' @references
#' Fan, J., & Vercauteren, F. (2012). Somewhat Practical Fully Homomorphic
#' Encryption. IACR ePrint. Retrieved from \url{https://eprint.iacr.org/2012/144}
#' 
#' @seealso
#' \code{\link{keygen}} to generate encryption keys using these parameters.
#' 
#' @examples
#' # Simplest example
#' p <- pars("FandV")
#' keys <- keygen(p)
#' ct <- enc(keys$pk, 1)
#' dec(keys$sk, ct)
#' 
#' # Change degree of cyclotomic polynomial used for ciphertext
#' p <- pars("FandV", d=2048)
#' keys <- keygen(p)
#' ct <- enc(keys$pk, 1)
#' dec(keys$sk, ct)
#' 
#' @author Louis Aslett
pars <- function(scheme, ...) {
  args <- list(...)
  if(scheme=="FandV") {
    p <- list(d=4096, sigma=16, qpow=128, tpow=15)
    if("d" %in% names(args)) {
      if(as.integer(log2(args$d))!=log2(args$d)) stop("d must be a power of 2.")
      p$d <- args$d
    }
    if("sigma" %in% names(args)) {
      if(args$sigma<=0.0) stop("Sigma must be >=0.")
      p$sigma <- args$sigma
    }
    if("qpow" %in% names(args)) {
      if(args$qpow<=0) stop("qpow must be >0.")
      if(args$qpow%%2!=0) stop("qpow must be divisible by 2 (for relinearisation optimisation).")
      p$qpow <- args$qpow
    }
    if("tpow" %in% names(args)) {
      if(args$tpow<=0) stop("tpow must be >0.")
      if(args$tpow<p$qpow) stop("tpow must be >qpow.")
      p$tpow <- args$tpow
    }
    p <- new(FandV_par, p$d, p$sigma, p$qpow, p$tpow)
    attr(p, "FHEt") <- "pars"
    attr(p, "FHEs") <- "FandV"
    return(p)
  }
  stop("The scheme ", scheme, " is not recognised.  Currently only 'FandV' is implemented")
}
