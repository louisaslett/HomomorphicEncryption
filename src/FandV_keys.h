/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 August 2014
*/

#ifndef FandV_keys_H
#define FandV_keys_H

#include <Rcpp.h>
using namespace Rcpp;

#include "FandV_par.h"
#include "FandV_ct_vec.h"
#include <fmpz_polyxx.h>

using namespace flint;

class FandV_ct;
class FandV_sk;
class FandV_pk;

class FandV_rlk {
  public:
    // Constructors
    FandV_rlk();
    FandV_rlk(const FandV_rlk& rlk);
    
    // Relinearise
    //int relin(FandV_ct& ct);
    
    // Print
    void show();
    
    friend void FandV_par::keygen(FandV_pk& pk, FandV_sk& sk, FandV_rlk& rlk);
    
    // Save/load
    void save(FILE* fp) const;
    FandV_rlk(FILE* fp);
    
    FandV_par p;
    fmpz_polyxx rlk00, rlk01, rlk10, rlk11;
};

class FandV_pk {
  public:
    // Constructors/Destructors
    FandV_pk();
    FandV_pk(const FandV_pk& pk);
    
    // Encrypt
    void enc(int m, FandV_ct& ct);
    void encvec(IntegerVector m, FandV_ct_vec& ctvec);
    
    // Print
    void show();
    
    friend void FandV_par::keygen(FandV_pk& pk, FandV_sk& sk, FandV_rlk& rlk);

    // Save/load
    void save(FILE* fp) const;
    FandV_pk(FILE* fp);

    FandV_par p;
    FandV_rlk rlk;
    
  private:
    fmpz_polyxx p0, p1; // Cyclotomic polynomial defining ring modulo
};

class FandV_sk {
  public:
    // Constructors
    FandV_sk();
    FandV_sk(const FandV_sk& sk);
    
    // Decrypt
    std::string dec(FandV_ct& ct);
    
    // Print
    void show();
    
    friend void FandV_par::keygen(FandV_pk& pk, FandV_sk& sk, FandV_rlk& rlk);
    
    // Save/load
    void save(FILE* fp) const;
    FandV_sk(FILE* fp);

  private:
    fmpz_polyxx s; // Cyclotomic polynomial defining ring modulo
};

#endif
