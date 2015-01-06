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

// R level ops
FandV_ct_mat FandV_ct_mat::add(const FandV_ct_mat& x) const {
  FandV_ct_mat res;
  
  if(nrow!=x.nrow || ncol!=x.ncol || mat.size()!=x.mat.size()) {
    return(res);
  }
  for(unsigned int i=0; i<mat.size(); ++i) {
    res.mat[i] = x.mat[i].add(mat[i]);
  }
  return(res);
}
FandV_ct_mat FandV_ct_mat::mul(const FandV_ct_mat& x) const {
  FandV_ct_mat res;
  
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
