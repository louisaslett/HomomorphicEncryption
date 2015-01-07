#' Fan and Vercauteren encryption scheme
#' 
#' The Fan and Vercauteren scheme is implemented in this package.
#' 
#' Description of the scheme.
#' 
#' @name FandV
#' 
#' @usage
#' p <- pars("FandV")
#' 
#' @examples
#' # Benchmark the performance of the scheme
#' library(microbenchmark)
#' p <- pars("FandV")
#' microbenchmark({ keys <- keygen(p) }, unit="ms")
#' microbenchmark({ ct1 <- enc(keys$pk, 2) }, unit="ms")
#' ct2 <- enc(keys$pk, 3)
#' microbenchmark({ ct1 + ct2 }, unit="ms")
#' microbenchmark({ ct1 * ct2 }, unit="ms")
#' microbenchmark({ dec(keys$sk, ct1) }, unit="ms")
#' 
NULL

loadModule("FandV", TRUE)

# http://stackoverflow.com/questions/18151619/operator-overloading-in-r-reference-classes
# evalqOnLoad used in package RcppBDT
evalqOnLoad({
  ##### Single ciphertexts #####
  setMethod("+", c("Rcpp_FandV_ct", "Rcpp_FandV_ct"), function(e1, e2) {
    ct <- e1$add(e2)
    # Prepare return result
    attr(ct, "FHEt") <- "ct"
    attr(ct, "FHEs") <- "FandV"
    ct
  })
  setMethod("-", c("Rcpp_FandV_ct", "Rcpp_FandV_ct"), function(e1, e2) {
    ct <- e1$sub(e2)
    # Prepare return result
    attr(ct, "FHEt") <- "ct"
    attr(ct, "FHEs") <- "FandV"
    ct
  })
  setMethod("*", c("Rcpp_FandV_ct", "Rcpp_FandV_ct"), function(e1, e2) {
    ct <- e1$mul(e2)
    # Prepare return result
    attr(ct, "FHEt") <- "ct"
    attr(ct, "FHEs") <- "FandV"
    ct
  })
  
  ##### Vectors of ciphertexts #####
  setMethod("c", "Rcpp_FandV_ct", function (x, ..., recursive = FALSE) {
    res <- new(FandV_ct_vec)
    res$push(x)
    
    args <- list(...)
    for(i in 1:length(args)) {
      if(class(args[[i]])=="Rcpp_FandV_ct") {
        res$push(args[[i]])
      } else if(class(args[[i]])=="Rcpp_FandV_ct_vec") {
        res$pushvec(args[[i]])
      } else {
        stop("only Fan and Vercauteren ciphertexts or ciphertext vectors can be concatenated")
      }
    }
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("c", "Rcpp_FandV_ct_vec", function (x, ..., recursive = FALSE) {
    res <- new(FandV_ct_vec)
    res$pushvec(x)
    
    args <- list(...)
    for(i in 1:length(args)) {
      if(class(args[[i]])=="Rcpp_FandV_ct") {
        res$push(args[[i]])
      } else if(class(args[[i]])=="Rcpp_FandV_ct_vec") {
        res$pushvec(args[[i]])
      } else {
        stop("only Fan and Vercauteren ciphertexts or ciphertext vectors can be concatenated")
      }
    }
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("[", "Rcpp_FandV_ct_vec", function(x, i, j, ..., drop=TRUE) {
    i <- as.integer(i)
    if(max(abs(i))>x$size()) {
      stop("out of bounds")
    }
    if(min(i) < 0 && max(i) > 0) {
      stop("only 0's may be mixed with negative subscripts")
    }
    if(min(i) < 0)
      res <- x$without(-sort(i)-1)
    else
      res <- x$subset(i-1)
    if(res$size() == 1) {
      ct <- res$get(0)
      attr(ct, "FHEt") <- "ct"
      attr(ct, "FHEs") <- "FandV"
      return(ct)
    }
    if(res$size() == 0) {
      return(NULL)
    }
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("[<-", signature(x="Rcpp_FandV_ct_vec", value="Rcpp_FandV_ct"), function (x, i, j, ..., value) {
    i <- as.integer(i)
    if(length(i) > 1)
      stop("only single element assignment currently supported for FandV ciphertext vectors")
    if(i<1 || i>x$size()) {
      stop("out of bounds")
    }
    x$set(i-1, value)
    
    attr(x, "FHEt") <- "ctvec"
    attr(x, "FHEs") <- "FandV"
    x
  })
  setMethod("[<-", signature(x="Rcpp_FandV_ct_vec"), function (x, i, j, ..., value) {
    stop("only a ciphertext can be assigned to this vector")
  })
  setMethod("+", c("Rcpp_FandV_ct_vec", "Rcpp_FandV_ct_vec"), function(e1, e2) {
    if(e1$size()%%e2$size()!=0 && e2$size()%%e1$size()!=0) {
      stop("longer object length is not a multiple of shorter object length")
    }
    res <- e1$add(e2)
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("*", c("Rcpp_FandV_ct_vec", "Rcpp_FandV_ct_vec"), function(e1, e2) {
    if(e1$size()%%e2$size()!=0 && e2$size()%%e1$size()!=0) {
      stop("longer object length is not a multiple of shorter object length")
    }
    res <- e1$mul(e2)
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  # This is weird.  %*% doesn't support S4 method dispatch.  I think this is because
  # this pkg 'Depend's on gmp and for some reason they force S3 dispatch on %*%
  # See gmp package source: gmp/R/matrix-prods.R, line 47 (top is if(FALSE)'ed out)
#   setMethod("%*%", c("Rcpp_FandV_ct_vec", "Rcpp_FandV_ct_vec"), function(x, y) {
#     if(x$size()!=y$size()) {
#       stop("non-conformable arguments")
#     }
#     res <- x$innerprod(y)
#     
#     attr(res, "FHEt") <- "ct"
#     attr(res, "FHEs") <- "FandV"
#     res
#   }
  setMethod("+", c("Rcpp_FandV_ct_vec", "Rcpp_FandV_ct"), function(e1, e2) {
    res <- e1$addct(e2)
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("+", c("Rcpp_FandV_ct", "Rcpp_FandV_ct_vec"), function(e1, e2) {
    res <- e2$addct(e1)
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("*", c("Rcpp_FandV_ct_vec", "Rcpp_FandV_ct"), function(e1, e2) {
    res <- e1$mulct(e2)
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("*", c("Rcpp_FandV_ct", "Rcpp_FandV_ct_vec"), function(e1, e2) {
    res <- e2$mulct(e1)
    
    attr(res, "FHEt") <- "ctvec"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("sum", c("Rcpp_FandV_ct_vec", "logical"), function(x, na.rm) {
    if(x$size() < 20) {
      res <- x$sumSerial()
    } else if(defaultNumThreads()*20>x$size()) {
      setThreadOptions(x$size()%/%20)
      res <- x$sumParallel()
    } else {
      setThreadOptions(defaultNumThreads())
      res <- x$sumParallel()
    }
    
    attr(res, "FHEt") <- "ct"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("prod", c("Rcpp_FandV_ct_vec", "logical"), function(x, na.rm) {
    res <- x$prodParallel()
    
    attr(res, "FHEt") <- "ct"
    attr(res, "FHEs") <- "FandV"
    res
  })
  
  ##### Matrices of ciphertexts #####
  # gmp package again overrides matrix and makes it S3 dispatch
#   setMethod("matrix", "Rcpp_FandV_ct_vec", function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, ...) {
#   })
  # TODO: probably not that useful, but base R will allow a matrix to concatenate
  #       with a scalar vector and just coerces the matrix to a vector columnwise
#   setMethod("c", "Rcpp_FandV_ct_mat", function (x, ..., recursive = FALSE) {
#   })
  setMethod("[", signature(x="Rcpp_FandV_ct_mat", i="missing", j="missing", drop="ANY"), function(x, i, j, ..., drop=TRUE) x)
  setMethod("[", signature(x="Rcpp_FandV_ct_mat", i="numeric", j="missing", drop="ANY"), function(x, i, j, ..., drop=TRUE) {
    i <- as.integer(i)
    if(max(abs(i))>x$size()) {
      stop("out of bounds")
    }
    if(min(i) < 0 && max(i) > 0) {
      stop("only 0's may be mixed with negative subscripts")
    }
    if(nargs() == 2) { # eg ct[1] or ct[1:4] etc
      tmp <- matrix(0:(x$size()-1), nrow=x$nrow, ncol=x$ncol)[i]
      res <- x$subsetV(as.vector(tmp))
      attr(res, "FHEt") <- "ctvec"
      attr(res, "FHEs") <- "FandV"
      if(res$size() == 1) {
        ct <- res$get(0)
        attr(ct, "FHEt") <- "ct"
        attr(ct, "FHEs") <- "FandV"
        return(ct)
      }
      if(res$size() == 0) {
        return(NULL)
      }
      
      return(res)
    } else { # eg ct[1,] or ct[1:4,]
      tmp <- matrix(0:(x$size()-1), nrow=x$nrow, ncol=x$ncol)[i,,drop=drop]
      if(is.matrix(tmp)) {
        res <- x$subset(as.vector(tmp), nrow(tmp), ncol(tmp))
        attr(res, "FHEt") <- "ctmat"
        attr(res, "FHEs") <- "FandV"
      } else {
        res <- x$subsetV(as.vector(tmp))
        attr(res, "FHEt") <- "ctvec"
        attr(res, "FHEs") <- "FandV"
      }
      if(res$size() == 1) {
        ct <- res$get(0)
        attr(ct, "FHEt") <- "ct"
        attr(ct, "FHEs") <- "FandV"
        return(ct)
      }
      if(res$size() == 0) {
        return(NULL)
      }
      
      return(res)
    }
  })
  setMethod("[", signature(x="Rcpp_FandV_ct_mat", i="missing", j="numeric", drop="ANY"), function(x, i, j, ..., drop=TRUE) {
    # eg ct[,1] or ct[,1:4]
    j <- as.integer(j)
    if(max(abs(j))>x$ncol) {
      stop("out of bounds")
    }
    if(min(j) < 0 && max(j) > 0) {
      stop("only 0's may be mixed with negative subscripts")
    }
    tmp <- matrix(0:(x$size()-1), nrow=x$nrow, ncol=x$ncol)[,j,drop=drop]
    if(is.matrix(tmp)) {
      res <- x$subset(as.vector(tmp), nrow(tmp), ncol(tmp))
      attr(res, "FHEt") <- "ctmat"
      attr(res, "FHEs") <- "FandV"
    } else {
      res <- x$subsetV(as.vector(tmp))
      attr(res, "FHEt") <- "ctvec"
      attr(res, "FHEs") <- "FandV"
    }
    if(res$size() == 1) {
      ct <- res$get(0)
      attr(ct, "FHEt") <- "ct"
      attr(ct, "FHEs") <- "FandV"
      return(ct)
    }
    if(res$size() == 0) {
      return(NULL)
    }
    
    return(res)
  })
  setMethod("[", signature(x="Rcpp_FandV_ct_mat", i="numeric", j="numeric", drop="ANY"), function(x, i, j, ..., drop=TRUE) {
    i <- as.integer(i)
    j <- as.integer(j)
    if(max(abs(i))>x$nrow || max(abs(j))>x$ncol) {
      stop("out of bounds")
    }
    if((min(i) < 0 && max(i) > 0) || (min(j) < 0 && max(j) > 0)) {
      stop("only 0's may be mixed with negative subscripts")
    }
    tmp <- matrix(0:(x$size()-1), nrow=x$nrow, ncol=x$ncol)[i,j,drop=drop]
    if(is.matrix(tmp)) {
      res <- x$subset(as.vector(tmp), nrow(tmp), ncol(tmp))
      attr(res, "FHEt") <- "ctmat"
      attr(res, "FHEs") <- "FandV"
    } else {
      res <- x$subsetV(as.vector(tmp))
      attr(res, "FHEt") <- "ctvec"
      attr(res, "FHEs") <- "FandV"
    }
    if(res$size() == 1) {
      ct <- res$get(0)
      attr(ct, "FHEt") <- "ct"
      attr(ct, "FHEs") <- "FandV"
      return(ct)
    }
    if(res$size() == 0) {
      return(NULL)
    }
    
    return(res)
  })
  setMethod("[<-", signature(x="Rcpp_FandV_ct_mat", i="numeric", j="numeric", value="Rcpp_FandV_ct"), function (x, i, j, ..., value) {
    i <- as.integer(i)
    j <- as.integer(j)
    if(length(i) > 1 || length(j) > 1)
      stop("only single element assignment currently supported for FandV ciphertext vectors")
    if((min(i) < 1 && max(i) > x$nrow) || (min(j) < 1 && max(j) > x$ncol)) {
      stop("out of bounds")
    }
    x$set(i-1, j-1, value)
    
    attr(x, "FHEt") <- "ctmat"
    attr(x, "FHEs") <- "FandV"
    x
  })
  setMethod("[<-", signature(x="Rcpp_FandV_ct_mat", value="Rcpp_FandV_ct"), function (x, i, j, ..., value) {
    stop("only single element assignment currently supported for FandV ciphertext vectors")
  })
  setMethod("[<-", signature(x="Rcpp_FandV_ct_mat"), function (x, i, j, ..., value) {
    stop("only a ciphertext can be assigned to this vector")
  })
  setMethod("+", signature(e1="Rcpp_FandV_ct_mat", e2="Rcpp_FandV_ct_mat"), function(e1, e2) {
    if(e1$nrow!=e2$nrow || e2$ncol!=e2$ncol) {
      stop("non-conformable matrix sizes")
    }
    res <- e1$add(e2)
    
    attr(res, "FHEt") <- "ctmat"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("*", signature(e1="Rcpp_FandV_ct_mat", e2="Rcpp_FandV_ct_mat"), function(e1, e2) {
    if(e1$nrow!=e2$nrow || e2$ncol!=e2$ncol) {
      stop("non-conformable matrix sizes")
    }
    res <- e1$mul(e2)
    
    attr(res, "FHEt") <- "ctmat"
    attr(res, "FHEs") <- "FandV"
    res
  })
  # This is weird.  %*% doesn't support S4 method dispatch.  I think this is because
  # this pkg 'Depend's on gmp and for some reason they force S3 dispatch on %*%
  # See gmp package source: gmp/R/matrix-prods.R, line 47 (top is if(FALSE)'ed out)
#   setMethod("%*%", c("Rcpp_FandV_ct_mat", "Rcpp_FandV_ct_mat"), function(x, y) {})
  setMethod("+", c("Rcpp_FandV_ct_mat", "Rcpp_FandV_ct"), function(e1, e2) {
    res <- e1$addct(e2)
    
    attr(res, "FHEt") <- "ctmat"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("+", c("Rcpp_FandV_ct", "Rcpp_FandV_ct_mat"), function(e1, e2) {
    res <- e2$addct(e1)
    
    attr(res, "FHEt") <- "ctmat"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("*", c("Rcpp_FandV_ct_mat", "Rcpp_FandV_ct"), function(e1, e2) {
    res <- e1$mulct(e2)
    
    attr(res, "FHEt") <- "ctmat"
    attr(res, "FHEs") <- "FandV"
    res
  })
  setMethod("*", c("Rcpp_FandV_ct", "Rcpp_FandV_ct_mat"), function(e1, e2) {
    res <- e2$mulct(e1)
    
    attr(res, "FHEt") <- "ctmat"
    attr(res, "FHEs") <- "FandV"
    res
  })
})

matrix.Rcpp_FandV_ct_vec <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, ...) {
  res <- new(FandV_ct_mat)
  res$setmatrix(data, nrow, ncol, byrow)
  
  attr(res, "FHEt") <- "ctmat"
  attr(res, "FHEs") <- "FandV"
  res
}

# See above for why this is here
`%*%.Rcpp_FandV_ct_vec` <- function(x, y) {
  if(x$size()!=y$size()) {
    stop("non-conformable arguments")
  }
  res <- x$innerprod(y)
  
  attr(res, "FHEt") <- "ct"
  attr(res, "FHEs") <- "FandV"
  res
}
# Again, see above for why this is here
`%*%.Rcpp_FandV_ct_mat` <- function(x, y) {
  if(class(y)!="Rcpp_FandV_ct_mat") {
    stop("can only multiply by another encrypted matrix")
  }
  if(x$ncol!=y$nrow) {
    stop("non-conformable arguments")
  }
  res <- x$matmulParallel(y)
  
  attr(res, "FHEt") <- "ctmat"
  attr(res, "FHEs") <- "FandV"
  res
}

loadFHE.Rcpp_FandV_ct <- function(file) {
  res <- load_FandV_ct(file)
  attr(res, "FHEt") <- "ct"
  attr(res, "FHEs") <- "FandV"
  res
}

loadFHE.Rcpp_FandV_ct_vec <- function(file) {
  res <- load_FandV_ct_vec(file)
  attr(res, "FHEt") <- "ctvec"
  attr(res, "FHEs") <- "FandV"
  res
}

loadFHE.FandV_keys <- function(file) {
  res <- load_FandV_keys(file)
  attr(res$pk, "FHEt") <- "pk"
  attr(res$pk, "FHEs") <- "FandV"
  attr(res$sk, "FHEt") <- "sk"
  attr(res$sk, "FHEs") <- "FandV"
  attr(res$rlk, "FHEt") <- "rlk"
  attr(res$rlk, "FHEs") <- "FandV"
  class(res) <- "FandV_keys"
  attr(res, "FHEt") <- "keys"
  attr(res, "FHEs") <- "FandV"
  res
}

# Dummies because:
#  1. Rcpp modules don't seem to work with S3method() in NAMESPACE
#  2. path.expand() in the saveFHE method doesn't overwrite the file argument
saveFHE.FandV_keys <- function(object, file) {
  saveFHE.FandV_keys2(object, path.expand(file))
}
saveFHE.Rcpp_FandV_ct <- function(object, file) {
  saveFHE.Rcpp_FandV_ct2(object, path.expand(file))
}
saveFHE.Rcpp_FandV_ct_vec <- function(object, file) {
  saveFHE.Rcpp_FandV_ct_vec2(object, path.expand(file))
}
