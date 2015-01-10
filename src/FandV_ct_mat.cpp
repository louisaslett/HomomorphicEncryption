/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 January 2015
*/

#include <Rcpp.h>
using namespace Rcpp;

#include <RcppParallel.h>
using namespace RcppParallel;

#include <iostream>
#include <math.h>

#include "FandV_ct.h"
#include "FandV_ct_vec.h"
#include "FandV_ct_mat.h"
#include "FandV.h"

// Construct from parameters
FandV_ct_mat::FandV_ct_mat() : nrow(0), ncol(0) { }
FandV_ct_mat::FandV_ct_mat(const std::vector<FandV_ct> v, const int nrow_, const int ncol_) : nrow(nrow_), ncol(ncol_), mat(v) { }

// Copy constructor
FandV_ct_mat::FandV_ct_mat(const FandV_ct_mat& ct_mat) : nrow(ct_mat.nrow), ncol(ct_mat.ncol), mat(ct_mat.mat) { }

// Assignment (copy-and-swap idiom)
void FandV_ct_mat::swap(FandV_ct_mat& a, FandV_ct_mat& b) {
  std::swap(a.nrow, b.nrow);
  std::swap(a.ncol, b.ncol);
  std::swap(a.mat, b.mat);
}
FandV_ct_mat& FandV_ct_mat::operator=(FandV_ct_mat ct_mat) {
  swap(*this, ct_mat);
  return(*this);
}

// Destructor
FandV_ct_mat::~FandV_ct_mat() {
  //Rcout << "DESTRUCT" << std::endl;
}

// Manipulate matrix
void FandV_ct_mat::set(int i, int j, const FandV_ct& ct) {
  mat[i + j*nrow] = ct;
}
void FandV_ct_mat::setmatrix(const FandV_ct_vec& ct_vec, int nrow_, int ncol_, int byrow) {
  std::vector<FandV_ct>().swap(mat); // clear the matrix data store and reset allocation
  if(!byrow) {
    for(int j=0; j<ncol_; j++) {
      for(int i=0; i<nrow_; i++) {
        mat.push_back(ct_vec.get((i + j*nrow_)%ct_vec.size()));
      }
    }
  } else {
    for(int i=0; i<nrow_; i++) {
      for(int j=0; j<ncol_; j++) {
        mat.push_back(ct_vec.get((i + j*nrow_)%ct_vec.size()));
      }
    }
  }
  nrow = nrow_;
  ncol = ncol_;
}

// Access ...
int FandV_ct_mat::size() const {
  return(mat.size());
}
FandV_ct FandV_ct_mat::get(int i) const {
  return(mat[i]);
}
// Vector indicies i chosen to form new matrix of nrow x ncol
// Basically allows us to push the complicated subsetting options onto R to figure out ... see method for [ in FandV.R 
FandV_ct_mat FandV_ct_mat::subset(IntegerVector i, int nrow_, int ncol_) const {
  FandV_ct_mat res;
  res.nrow = nrow_;
  res.ncol = ncol_;
  //res.mat.resize(res.nrow*res.ncol, FandC_ct(mat[0].p, mat[0].rlk));
  // This is pretty naive, come up with something better
  for(IntegerVector::iterator itI = i.begin(); itI != i.end(); ++itI) {
    res.mat.push_back(mat[*itI]);
  }
  return(res);
}
FandV_ct_vec FandV_ct_mat::subsetV(IntegerVector i) const {
  FandV_ct_vec res;
  for(IntegerVector::iterator it = i.begin(); it != i.end(); ++it) {
    res.push(mat[*it]);
  }
  return(res);
}
FandV_ct_mat FandV_ct_mat::t() const {
  FandV_ct_mat res(mat, ncol, nrow);
  
  for(int i=0; i<ncol; i++) {
    for(int j=0; j<nrow; j++) {
      res.mat[i+j*ncol] = mat[j+i*nrow];
    }
  }
  
  return(res);
}

// R level ops
FandV_ct_mat FandV_ct_mat::add(const FandV_ct_mat& x) const {
  FandV_ct_mat res(mat, nrow, ncol);
  
  if(nrow!=x.nrow || ncol!=x.ncol || mat.size()!=x.mat.size()) {
    return(res);
  }
  for(unsigned int i=0; i<mat.size(); ++i) {
    res.mat[i] = x.mat[i].add(mat[i]);
  }
  return(res);
}
FandV_ct_mat FandV_ct_mat::mul(const FandV_ct_mat& x) const {
  FandV_ct_mat res(mat, nrow, ncol);
  
  if(nrow!=x.nrow || ncol!=x.ncol || mat.size()!=x.mat.size()) {
    return(res);
  }
  for(unsigned int i=0; i<mat.size(); ++i) {
    res.mat[i] = x.mat[i].mul(mat[i]);
  }
  return(res);
}
FandV_ct_mat FandV_ct_mat::addct(const FandV_ct& ct) const {
  FandV_ct_mat res(mat, nrow, ncol);
  for(unsigned int i=0; i<mat.size(); i++) {
    res.mat[i] = mat[i].add(ct);
  }
  return(res);
}
FandV_ct_mat FandV_ct_mat::mulct(const FandV_ct& ct) const {
  FandV_ct_mat res(mat, nrow, ncol);
  for(unsigned int i=0; i<mat.size(); i++) {
    res.mat[i] = mat[i].mul(ct);
  }
  return(res);
}

struct FandV_MatMul : public Worker {
  // Input values to multiply
  const std::vector<FandV_ct>* x;
  const std::vector<FandV_ct>* y;
  const unsigned int xnrow, xncolynrow, yncol;
  
  // Output vector of cipher texts
  std::vector<FandV_ct>* res;
  
  // Constructor
  FandV_MatMul(const std::vector<FandV_ct>* x_, const std::vector<FandV_ct>* y_, std::vector<FandV_ct>* res_, const unsigned int xnrow_, const int xncolynrow_, const int yncol_) : xnrow(xnrow_), xncolynrow(xncolynrow_), yncol(yncol_) { x=x_; y=y_; res=res_; }
  
  // function call operator that work for the specified range (begin/end)
  void operator()(std::size_t begin, std::size_t end) {
    unsigned int i, j, k;
    for(std::size_t ij = begin; ij < end; ij++) {
      i = ij/yncol;
      j = ij%yncol;
      FandV_ct sum((*x)[0].p, (*x)[0].rlk);
      for(k=0; k<xncolynrow; k++) {
        sum = sum.add((*x)[i + k*xnrow].mul((*y)[k + j*xncolynrow]));
      }
      (*res)[i + j*xnrow] = sum;
    }
  }
};
FandV_ct_mat FandV_ct_mat::matmulParallel(const FandV_ct_mat& y) const {
  // Setup destination
  FandV_ct_mat res;
  res.mat.resize(nrow*y.ncol, mat[0]);
  res.nrow = nrow;
  res.ncol = y.ncol;
  
  FandV_MatMul matmulEngine(&mat, &(y.mat), &(res.mat), nrow, ncol, y.ncol);
  parallelFor(0, res.nrow*res.ncol, matmulEngine);
  
  return(res);
}
FandV_ct_mat FandV_ct_mat::matmulSerial(const FandV_ct_mat& y) const {
  FandV_ct_mat res;
  // Setup destination size
  res.mat.resize(nrow*y.ncol, mat[0]);
  res.nrow = nrow;
  res.ncol = y.ncol;
  
  // Do naive multiply ... switch for something clever like Strassen's algorithm in future
  for(unsigned int i=0; i<nrow; i++) {
    for(unsigned int j=0; j<y.ncol; j++) {
      FandV_ct sum(mat[0].p, mat[0].rlk);
      for(unsigned int k=0; k<ncol; k++) {
        sum = sum.add(mat[i + k*nrow].mul(y.mat[k + j*y.nrow]));
      }
      res.mat[i + j*nrow] = sum;
    }
  }
  return(res);
}

void FandV_ct_mat::show() const {
  Rcout << "Matrix of " << nrow << " x " << ncol << " Fan and Vercauteren cipher texts\n";
}

// Save/load
void FandV_ct_mat::save(FILE* fp) const {
  fprintf(fp, "=> FHE package object <=\nRcpp_FandV_ct_mat\nnrow=%d\nncol=%d\n", nrow, ncol);
  for(unsigned int i=0; i<mat.size(); i++) {
    mat[i].save(fp);
  }
}
FandV_ct_mat::FandV_ct_mat(FILE* fp) {
  // Check for header line
  char *buf = NULL; size_t bufn = 0;
  size_t len;
  len = getline(&buf, &bufn, fp);
  if(strncmp("=> FHE package object <=\n", buf, len) != 0) {
    Rcout << "Error: file does not contain an FHE object (CT_VEC)\n";
    free(buf);
    return;
  }
  len = getline(&buf, &bufn, fp);
  if(strncmp("Rcpp_FandV_ct_mat\n", buf, len) != 0) {
    Rcout << "Error: file does not contain a matrix of ciphertext objects\n";
    free(buf);
    return;
  }
  
  len = fscanf(fp, "nrow=%d\n", &nrow); Rcout << nrow << "\n";
  len = fscanf(fp, "ncol=%d\n", &ncol); Rcout << ncol << "\n";
  for(int i=0; i<nrow*ncol; i++) {
    FandV_ct ct(fp);
    len = getline(&buf, &bufn, fp); // Advance past the new line
    mat.push_back(ct);
  }
  free(buf);
}
