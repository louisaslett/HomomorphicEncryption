/*
 Louis Aslett (aslett@stats.ox.ac.uk)
 August 2014
*/

#ifndef FandV_H
#define FandV_H
#include <fmpz_polyxx.h>

void fmpz_polyxx_q(fmpz_polyxx& p, fmpzxx q);
void fmpz_rand(fmpzxx &p, unsigned int bits);
void printPoly(const fmpz_polyxx& p);

#endif
