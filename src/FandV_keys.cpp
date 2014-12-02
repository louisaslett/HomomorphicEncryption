/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 August 2014
*/

#include "FandV_keys.h"
#include "FandV_ct.h"
#include "FandV.h"
#include <Rcpp.h>
#include <limits.h>
#include <fmpz_polyxx.h>
#include <string>


using namespace Rcpp;

//// Public keys ////
FandV_pk::FandV_pk() : p(0, 0.0, 0, 1) { }

FandV_pk::FandV_pk(const FandV_pk& pk) : p(pk.p), rlk(pk.rlk), p0(pk.p0), p1(pk.p1) { }

// Encrypt
void FandV_pk::enc(int m, FandV_ct& ct) {
  ct.c0.realloc(p.Phi.length());
  ct.c1.realloc(p.Phi.length());
  
  fmpz_polyxx u, mP;
  u.realloc(p.Phi.length());
  mP.realloc(31);
  
  // Random numbers
  for(int i=0; i<p.Phi.length(); i++) {
    u.set_coeff(i, lround(R::rnorm(0.0,p.sigma))); // u
    ct.c0.set_coeff(i, lround(R::rnorm(0.0,p.sigma))); // e1
    ct.c1.set_coeff(i, lround(R::rnorm(0.0,p.sigma))); // e2
  }
  
  // Binary conversion of message
  int sign = 1;
  sign = copysign(sign, m);
  m = abs(m);
  for(int i=0; i<31; i++) {
    mP.set_coeff(i, (m&1)*sign);
    m >>= 1;
  }
  
  ct.c0 = ((p0*u)%p.Phi) + ct.c0 + p.Delta*mP;
  fmpz_polyxx_q(ct.c0, p.q);
  
  ct.c1 = ((p1*u)%p.Phi);
  fmpz_polyxx_q(ct.c1, p.q);
}

void FandV_pk::show() {
  Rcout << "Fan and Vercauteren public key\n";
  Rcout << "( p\u2080 = ";
  printPoly(p0);
  Rcout << ",\np\u2081 = ";
  printPoly(p1);
  Rcout << " )\n";
}

// Save/load
void FandV_pk::save(FILE* fp) const {
  fprintf(fp, "=> FHE package object <=\nRcpp_FandV_pk\n");
  print(fp, p0);
  fprintf(fp, "\n");
  print(fp, p1);
  fprintf(fp, "\n");
  
  rlk.save(fp);
  p.save(fp);
}
FandV_pk::FandV_pk(FILE* fp) {
  // Check for header line
  char *buf = NULL; size_t bufn = 0;
  size_t len;
  len = getline(&buf, &bufn, fp);
  if(strncmp("=> FHE package object <=\n", buf, len) != 0) {
    Rcout << "Error: file does not contain an FHE object (PK)\n";
    free(buf);
    return;
  }
  len = getline(&buf, &bufn, fp);
  if(strncmp("Rcpp_FandV_pk\n", buf, len) != 0) {
    Rcout << "Error: file does not contain a public key\n";
    free(buf);
    return;
  }
  
  read(fp, p0);
  read(fp, p1);
  
  len = getline(&buf, &bufn, fp); // Advance past the new line
  rlk = FandV_rlk(fp);
  len = getline(&buf, &bufn, fp); // Advance past the new line
  p = FandV_par(fp);
  free(buf);
}


//// Private keys ////
FandV_sk::FandV_sk() { }

FandV_sk::FandV_sk(const FandV_sk& sk) : s(sk.s) { }

// Decrypt
std::string FandV_sk::dec(FandV_ct& ct) {
  fmpz_polyxx res, res2;
  fmpzxx tmp(1), m(0);
  
  res = ct.c0+((ct.c1*s)%ct.p.Phi);
  fmpz_polyxx_q(res, ct.p.q);
  res2 = (ct.p.t*res)%ct.p.q;
  res = (ct.p.t*res)/ct.p.q;
  for(int i=0; i<ct.p.Phi.length(); i++) {
    if(res2.get_coeff(i) > ct.p.q/2)
      res.set_coeff(i, res.get_coeff(i)+tmp);
  }
  fmpz_polyxx_q(res, ct.p.t);
  
  for(int i=0; i<res.length(); i++) {
    m += res.get_coeff(i)*tmp;
    tmp *= 2;
  }
  
  return(m.to_string());
}

void FandV_sk::show() {
  Rcout << "Fan and Vercauteren private key\n";
  Rcout << "s = ";
  printPoly(s);
  Rcout << "\n";
}

// Save/load
void FandV_sk::save(FILE* fp) const {
  fprintf(fp, "=> FHE package object <=\nRcpp_FandV_sk\n");
  print(fp, s);
  fprintf(fp, "\n");
}
FandV_sk::FandV_sk(FILE* fp) {
  // Check for header line
  char *buf = NULL; size_t bufn = 0;
  size_t len;
  len = getline(&buf, &bufn, fp);
  if(strncmp("=> FHE package object <=\n", buf, len) != 0) {
    Rcout << "Error: file does not contain an FHE object (SK)\n";
    free(buf);
    return;
  }
  len = getline(&buf, &bufn, fp);
  if(strncmp("Rcpp_FandV_sk\n", buf, len) != 0) {
    Rcout << "Error: file does not contain a secret key\n";
    free(buf);
    return;
  }
  
  read(fp, s);
  free(buf);
}


//// Relinearisation keys ////
FandV_rlk::FandV_rlk() : p(0, 0.0, 0, 1) { }

FandV_rlk::FandV_rlk(const FandV_rlk& rlk) : p(rlk.p), rlk00(rlk.rlk00), rlk01(rlk.rlk01), rlk10(rlk.rlk10), rlk11(rlk.rlk11) { }

void FandV_rlk::show() {
  Rcout << "Fan and Vercauteren relinearisation key\n";
  Rcout << "( rlk\u2080\u2080 = ";
  printPoly(rlk00);
  Rcout << ",\nrlk\u2080\u2081 = ";
  printPoly(rlk01);
  Rcout << ",\nrlk\u2081\u2080 = ";
  printPoly(rlk10);
  Rcout << ",\nrlk\u2081\u2081 = ";
  printPoly(rlk11);
  Rcout << " )\n";
}

// Save/load
void FandV_rlk::save(FILE* fp) const {
  fprintf(fp, "=> FHE package object <=\nRcpp_FandV_rlk\n");
  print(fp, rlk00);
  fprintf(fp, "\n");
  print(fp, rlk01);
  fprintf(fp, "\n");
  print(fp, rlk10);
  fprintf(fp, "\n");
  print(fp, rlk11);
  fprintf(fp, "\n");
  p.save(fp);
}
FandV_rlk::FandV_rlk(FILE* fp) {
  // Check for header line
  char *buf = NULL; size_t bufn = 0;
  size_t len;
  len = getline(&buf, &bufn, fp);
  if(strncmp("=> FHE package object <=\n", buf, len) != 0) {
    Rcout << "Error: file does not contain an FHE object (RLK)\n";
    free(buf);
    return;
  }
  len = getline(&buf, &bufn, fp);
  if(strncmp("Rcpp_FandV_rlk\n", buf, len) != 0) {
    Rcout << "Error: file does not contain a relinearisation key\n";
    free(buf);
    return;
  }
  
  read(fp, rlk00);
  read(fp, rlk01);
  read(fp, rlk10);
  read(fp, rlk11);
  
  len = getline(&buf, &bufn, fp); // Advance past the new line
  p = FandV_par(fp);
  free(buf);
}
