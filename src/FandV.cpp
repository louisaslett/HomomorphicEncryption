/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 August 2014
*/

#include <Rcpp.h>
using namespace Rcpp;

#include <fmpzxx.h>
#include <fmpz_polyxx.h>
using namespace flint;

#include <vector>
#include <stdio.h>

#include "FandV_par.h"
#include "FandV_keys.h"
#include "FandV_ct.h"
#include "FandV_ct_vec.h"
#include "FandV_ct_mat.h"


// Do centred modulo q reduction of all coefficients of polynomial p ... [p]_q
void fmpz_polyxx_q(fmpz_polyxx& p, fmpzxx q) {
  fmpzxx tmp, qo2(q/2);
  for(int i=0; i<p.length(); i++) {
    tmp = p.get_coeff(i)%q;
    if(tmp > qo2)
      tmp -= q;
    p.set_coeff(i, tmp);
  }
}

void fmpz_rand(fmpzxx &p, unsigned int bits) { // Random number from 0 to 2^bits-1
  RNGScope scope;
  p = 0;
  for(unsigned int i=0; i<bits/32; i++) {
    p = (p << 32) + ((unsigned int) R::runif(0.0, 4294967295.0));
  }
  if(bits%32 > 0) {
    p = (p << (bits%32)) + ((unsigned int) R::runif(0.0, pow(2.0, (double) (bits%32))-1.0));
  }
}

void printPoly(const fmpz_polyxx& p) {
  static const char * const super[] = {"\xe2\x81\xb0", "\xc2\xb9", "\xc2\xb2",
    "\xc2\xb3", "\xe2\x81\xb4", "\xe2\x81\xb5", "\xe2\x81\xb6",
    "\xe2\x81\xb7", "\xe2\x81\xb8", "\xe2\x81\xb9"};
  
  bool firstDone=false;
  for(int i=p.length(); --i>=0; ) {
    if(p.get_coeff(i) == fmpzxx(0)) continue;
    if(p.get_coeff(i)>0 && firstDone && i<p.length()) Rcout << "+";
    if(p.get_coeff(i) == fmpzxx(-1) && i>0)
      Rcout << "-";
    else if(p.get_coeff(i) != fmpzxx(1) || i==0)
      Rcout << p.get_coeff(i);
    firstDone=true;
    if(i > 0)
      Rcout << "x";
    if(i > 1) {
      int j, max;
      j = i;
      max = 1;
      while(j > 9) {
        max *= 10;
        j /= 10;
      }
      j = i;
      for(int k=max; k>0; k/=10)
        Rcout << super[(j/k)%10];
    }
  }
}

void save_FandV_ct(const FandV_ct& ct, const std::string& file) {
  const char *file_c = file.c_str();
  
  FILE *fp = fopen(file_c, "w");
  if(fp == NULL) {
    perror("Error");
  }
  
  ct.save(fp);
  
  fclose(fp);
}
FandV_ct load_FandV_ct(const std::string& file) {
  const char *file_c = file.c_str();
  
  FILE *fp = fopen(file_c, "r");
  if(fp == NULL) {
    perror("Error");
  }
  
  FandV_ct ct(fp);
  
  fclose(fp);
  
  return(ct);
}

void save_FandV_ct_vec(const FandV_ct_vec& ct_vec, const std::string& file) {
  const char *file_c = file.c_str();
  
  FILE *fp = fopen(file_c, "w");
  if(fp == NULL) {
    perror("Error");
  }
  
  ct_vec.save(fp);
  
  fclose(fp);
}
FandV_ct_vec load_FandV_ct_vec(const std::string& file) {
  const char *file_c = file.c_str();
  
  FILE *fp = fopen(file_c, "r");
  if(fp == NULL) {
    perror("Error");
  }
  
  FandV_ct_vec ct_vec(fp);
  
  fclose(fp);
  
  return(ct_vec);
}

void save_FandV_keys(const List& keys, const std::string& file) {
  const char *file_c = file.c_str();
  
  FILE *fp = fopen(file_c, "w");
  if(fp == NULL) {
    perror("Error");
  }
  
  fprintf(fp, "=> FHE package object <=\nFandV_keys\n");
  
  FandV_sk sk = keys["sk"];
  sk.save(fp);
  FandV_pk pk = keys["pk"];
  pk.save(fp);
  FandV_rlk rlk = keys["rlk"];
  rlk.save(fp);
  
  fclose(fp);
}
List load_FandV_keys(const std::string& file) {
  const char *file_c = file.c_str();
  List keys;
  
  FILE *fp = fopen(file_c, "r");
  if(fp == NULL) {
    perror("Error");
  }
  
  // Check for header line
  char *buf = NULL; size_t bufn = 0;
  size_t len;
  len = getline(&buf, &bufn, fp);
  if(strncmp("=> FHE package object <=\n", buf, len) != 0) {
    Rcout << "Error: file does not contain an FHE object (KEYS)\n";
    free(buf);
    return(keys);
  }
  len = getline(&buf, &bufn, fp);
  if(strncmp("FandV_keys\n", buf, len) != 0) {
    Rcout << "Error: file does not contain key objects\n";
    free(buf);
    return(keys);
  }
  
  FandV_sk sk(fp);
  len = getline(&buf, &bufn, fp); // Advance past the new line
  FandV_pk pk(fp);
  len = getline(&buf, &bufn, fp); // Advance past the new line
  FandV_rlk rlk(fp);
  
  keys["sk"] = sk;
  keys["pk"] = pk;
  keys["rlk"] = rlk;
  
  fclose(fp);
  
  free(buf);
  return(keys);
}


RCPP_EXPOSED_CLASS(FandV_par)
RCPP_EXPOSED_CLASS(FandV_pk)
RCPP_EXPOSED_CLASS(FandV_sk)
RCPP_EXPOSED_CLASS(FandV_rlk)
RCPP_EXPOSED_CLASS(FandV_ct)
RCPP_EXPOSED_CLASS(FandV_ct_vec)
RCPP_EXPOSED_CLASS(FandV_ct_mat)

RCPP_MODULE(FandV) {
  class_<FandV_par>("FandV_par")
    .constructor<int, double, int, int>()
    .method("keygen", &FandV_par::keygen)
    .method("show", &FandV_par::show)
  ;
  
  class_<FandV_pk>("FandV_pk")
    .constructor()
    .field("p", &FandV_pk::p)
    .field("rlk", &FandV_pk::rlk)
    .method("enc", &FandV_pk::enc)
    .method("encvec", &FandV_pk::encvec)
    .method("encmat", &FandV_pk::encmat)
    .method("show", &FandV_pk::show)
  ;

  class_<FandV_sk>("FandV_sk")
    .constructor()
    .method("dec", &FandV_sk::dec)
    .method("show", &FandV_sk::show)
  ;
  
  class_<FandV_rlk>("FandV_rlk")
    .constructor()
    .method("show", &FandV_rlk::show)
  ;
  
  class_<FandV_ct>("FandV_ct")
    .constructor<FandV_par,FandV_rlk>()
    .field("p", &FandV_ct::p)
    .field("rlk", &FandV_ct::rlk)
    .method("add", &FandV_ct::add)
    .method("sub", &FandV_ct::sub)
    .method("mul", &FandV_ct::mul)
    .method("show", &FandV_ct::show)
  ;
  
  class_<FandV_ct_vec>("FandV_ct_vec")
    .constructor()
    .method("add", &FandV_ct_vec::add)
    .method("addct", &FandV_ct_vec::addct)
    .method("get", &FandV_ct_vec::get)
    .method("mul", &FandV_ct_vec::mul)
    .method("mulct", &FandV_ct_vec::mulct)
    .method("sumParallel", &FandV_ct_vec::sumParallel)
    .method("sumSerial", &FandV_ct_vec::sumSerial)
    .method("prodParallel", &FandV_ct_vec::prodParallel)
    .method("prodSerial", &FandV_ct_vec::prodSerial)
    .method("innerprod", &FandV_ct_vec::innerprod)
    .method("push", &FandV_ct_vec::push)
    .method("pushvec", &FandV_ct_vec::pushvec)
    .method("set", &FandV_ct_vec::set)
    .method("show", &FandV_ct_vec::show)
    .method("size", &FandV_ct_vec::size)
    .method("subset", &FandV_ct_vec::subset)
    .method("without", &FandV_ct_vec::without)
  ;
  
  class_<FandV_ct_mat>("FandV_ct_mat")
    .constructor()
    .field("nrow", &FandV_ct_mat::nrow)
    .field("ncol", &FandV_ct_mat::ncol)
    .method("size", &FandV_ct_mat::size)
    .method("get", &FandV_ct_mat::get)
    .method("subset", &FandV_ct_mat::subset)
    .method("subsetV", &FandV_ct_mat::subsetV)
    .method("set", &FandV_ct_mat::set)
    .method("show", &FandV_ct_mat::show)
    .method("add", &FandV_ct_mat::add)
    .method("mul", &FandV_ct_mat::mul)
    .method("addct", &FandV_ct_mat::addct)
    .method("mulct", &FandV_ct_mat::mulct)
    .method("matmul", &FandV_ct_mat::matmul)
  ;
  
  function("saveFHE.FandV_keys2", &save_FandV_keys);
  function("load_FandV_keys", &load_FandV_keys);
  function("saveFHE.Rcpp_FandV_ct2", &save_FandV_ct);
  function("load_FandV_ct", &load_FandV_ct);
  function("saveFHE.Rcpp_FandV_ct_vec2", &save_FandV_ct_vec);
  function("load_FandV_ct_vec", &load_FandV_ct_vec);
}
