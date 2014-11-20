/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 August 2014
*/

#ifndef FandV_par_H
#define FandV_par_H

#include <fmpz_polyxx.h>
#include <Rcpp.h>

using namespace flint;
using namespace Rcpp;

class FandV_pk;
class FandV_sk;
class FandV_rlk;

class FandV_par {
  public:
    // Constructors
    FandV_par(int d_=4096, double sigma_=16.0, int qpow_=128, int t_=32768);
    FandV_par(const FandV_par& par);
    
    // Operators
    FandV_par& operator=(FandV_par par);
    void swap(FandV_par& a, FandV_par& b);
    
    // Print
    void show();
    
    void keygen(FandV_pk& pk, FandV_sk& sk, FandV_rlk& rlk);
    
    // Save/load
    void save(FILE* fp) const;
    FandV_par(FILE* fp);
    
    // Don't private these to keep parameters object very lightweight, because
    // the keys are going to hold copies
    double sigma;
    int qpow;
    fmpzxx q, t, T, Delta; // Coefficient modulo values
    fmpz_polyxx Phi; // Cyclotomic polynomial defining ring modulo
};

#endif
