/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 August 2014
*/

#include "FandV_ct.h"
#include "FandV_ct_vec.h"
#include <Rcpp.h>
#include <iostream>
#include <math.h>
#include "FandV.h"

using namespace Rcpp;

// Construct from parameters
FandV_ct_vec::FandV_ct_vec() { }
FandV_ct_vec::FandV_ct_vec(const std::vector<FandV_ct> v) : vec(v) { }

// Copy constructor
FandV_ct_vec::FandV_ct_vec(const FandV_ct_vec& ct_vec) : vec(ct_vec.vec) { }

// Assignment (copy-and-swap idiom)
void FandV_ct_vec::swap(FandV_ct_vec& a, FandV_ct_vec& b) {
  std::swap(a.vec, b.vec);
}
FandV_ct_vec& FandV_ct_vec::operator=(FandV_ct_vec ct_vec) {
  swap(*this, ct_vec);
  return(*this);
}

// Destructor
FandV_ct_vec::~FandV_ct_vec() {
  //Rcout << "DESTRUCT" << std::endl;
}

// Manipulate vector
void FandV_ct_vec::push(const FandV_ct& ct) {
  vec.push_back(ct);
}
void FandV_ct_vec::pushvec(const FandV_ct_vec& ct_vec) {
  for(int i=0; i<ct_vec.vec.size(); i++)
    vec.push_back(ct_vec.vec[i]);
}
void FandV_ct_vec::set(int i, const FandV_ct& ct_vec) {
  vec[i] = ct_vec;
}

// Access vector
int FandV_ct_vec::size() const {
  return(vec.size());
}
FandV_ct FandV_ct_vec::get(int i) const {
  return(vec[i]);
}
FandV_ct_vec FandV_ct_vec::subset(NumericVector i) const {
  FandV_ct_vec res;
  for(NumericVector::iterator it = i.begin(); it != i.end(); ++it) {
    res.push(vec[*it]);
  }
  return(res);
}
FandV_ct_vec FandV_ct_vec::without(NumericVector i) const { // NB must be sorted largest to smallest
  FandV_ct_vec res(vec);
  for(NumericVector::iterator it = i.begin(); it != i.end(); ++it) {
    res.vec.erase(res.vec.begin()+*it);
  }
  return(res);
}

// R level ops
FandV_ct_vec FandV_ct_vec::add(const FandV_ct_vec& x) const {
  int sz = vec.size(), xsz = x.vec.size();
  
  FandV_ct_vec res;
  if(sz>=xsz) {
    res.vec = vec;
    for(int i=0; i<sz; i++) {
      res.vec[i] = vec[i].add(x.vec[i%xsz]);
    }
  } else {
    res.vec = x.vec;
    for(int i=0; i<xsz; i++) {
      res.vec[i] = x.vec[i].add(vec[i%xsz]);
    }
  }
  return(res);
}
FandV_ct_vec FandV_ct_vec::mul(const FandV_ct_vec& x) const {
  int sz = vec.size(), xsz = x.vec.size();
  
  FandV_ct_vec res;
  if(sz>=xsz) {
    res.vec = vec;
    for(int i=0; i<sz; i++) {
      res.vec[i] = vec[i].mul(x.vec[i%xsz]);
    }
  } else {
    res.vec = x.vec;
    for(int i=0; i<xsz; i++) {
      res.vec[i] = x.vec[i].mul(vec[i%xsz]);
    }
  }
  return(res);
}
FandV_ct_vec FandV_ct_vec::addct(const FandV_ct& ct) const {
  FandV_ct_vec res(vec);
  for(int i=0; i<vec.size(); i++) {
    res.vec[i] = vec[i].add(ct);
  }
  return(res);
}

FandV_ct_vec FandV_ct_vec::mulct(const FandV_ct& ct) const {
  FandV_ct_vec res(vec);
  for(int i=0; i<vec.size(); i++) {
    res.vec[i] = vec[i].mul(ct);
  }
  return(res);
}

// This is a functioning parallel version of the sum function, *but*
//   it seems to use a lot more memory due to all the temporary polynomials
//   and is much slower because sum is already quite fast in serial.
// Add appropriate entries to DESCRIPTION and NAMESPACE files, and
// add to headers:
// // [[Rcpp::depends(RcppParallel)]]
// #include <RcppParallel.h>
// using namespace RcppParallel;
//
//struct FandV_Sum : public Worker {   
//  // Source vector
//  const FandV_ct_vec input;
//  
//  // Accumulated value
//  FandV_ct value;
//  
//  // Constructors
//  FandV_Sum(const FandV_ct_vec& input) : input(input), value(input.get(0).sub(input.get(0))) {}
//  FandV_Sum(const FandV_Sum& sum, Split) : input(sum.input), value(input.get(0).sub(input.get(0))) {}
//  
//  // Accumulate
//  void operator()(std::size_t begin, std::size_t end) {
//    for(; begin<end; begin++) {
//      value = value.add(input.get(begin));
//    }
//  }
//  
//  void join(const FandV_Sum& rhs) {
//    value = value.add(rhs.value);
//  }
//};
//FandV_ct FandV_ct_vec::sum() const {
//  FandV_Sum sum(vec);
//  parallelReduce(0, vec.size(), sum);
//  return(sum.value);
//}

FandV_ct FandV_ct_vec::sum() const {
  FandV_ct res(vec[0]);
  
  for(int i=1; i<vec.size(); i++) {
    res = res.add(vec[i]);
  }
  
  return(res);
}

FandV_ct FandV_ct_vec::prod() const {
  FandV_ct res(vec[0]);
  
  for(int i=1; i<vec.size(); i++) {
    res = res.mul(vec[i]);
  }
  
  return(res);
}

void FandV_ct_vec::show() const {
  Rcout << "Vector of " << vec.size() << " Fan and Vercauteren cipher texts\n";
}

// Save/load
void FandV_ct_vec::save(FILE* fp) const {
  fprintf(fp, "=> FHE package object <=\nRcpp_FandV_ct_vec\nn=%d\n", (int) vec.size());
  for(int i=0; i<vec.size(); i++) {
    vec[i].save(fp);
  }
}
FandV_ct_vec::FandV_ct_vec(FILE* fp) {
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
  if(strncmp("Rcpp_FandV_ct_vec\n", buf, len) != 0) {
    Rcout << "Error: file does not contain a vector of ciphertext objects\n";
    free(buf);
    return;
  }
  
  int vecsz;
  len = fscanf(fp, "n=%d\n", &vecsz); Rcout << vecsz << "\n";
  for(int i=0; i<vecsz; i++) {
    FandV_ct ct(fp);
    len = getline(&buf, &bufn, fp); // Advance past the new line
    vec.push_back(ct);
  }
  free(buf);
}
