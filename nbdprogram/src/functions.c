/*
 * ndbprogram/src/functions.c
 * Copyright (c) 2017 Daniele Medri
 * Distributed under the GNU GPL-2 license
*/

#include <stdio.h>
#include <math.h>

void fnbval(double *n, double *m, double *g, double *k, double *r) {
  int i;

  for (i = 0; i < 7; i++) {        
    r[i] = (1 - 1 / pow((1 + n[0] * m[0] * (g[i] / k[0] )), k[0]));
  }
}

void fnqval(double *bval, double *r) {

  int i;

  for (i = 0; i < 7; i++) {
    r[i] = (1.0 - bval[i]);
  }
}
/*
void fnqvalrest(double *bval, double *wval, double *qval[], double *k) {

  int i, j, jj;
  double a[7];
  int nr = 5;
  int nc = 7;

  for (j = 0; j < nr; j++) {
    jj = (j + 1);
    for (i = 0; i < nc; i++) {
      a[i] = (bval[i] * wval[i] / k[0]);
      qval[i + jj*nr] = ((a[i] / (1 + a[i])) * (1 - (a[i] - k[0] * a[i]) / (a[i] * j)) * qval[i + j*nr]);
    }
  }
}

void fnzval(double *bval, double *wval, double *qval[], double *zval[]) {

  int i, j, jj;
  int nr = 5;
  int nc = 7;

  for (j = 0; j < nr; j++) {
    jj = (j + 1);
    for (i = 0; i < nc; i++) {
      zval[i + jj*nr] = (j * qval[i + jj*nr]) / (wval[i] * bval[i]);
    }
  }
}
*/
void fnwval(double *n, double *m, double *g, double *k, double *r) {
  int i;

  for (i = 0; i < 7; i++) {        
    r[i] = ((n[0] * m[0] * g[i]) / (1 - 1 / pow((1 + m[0] * n[0] * g[i] / k[0]), k[0])));
  }
}


void fndval(double *bval, double *wval, double *k, double *r)
{
  int i;

  for (i = 0; i < 7; i++) {
    r[i] = (100 / bval[i]) * (1 - 2 / pow((1 + bval[i] * wval[i] / k[0]), k[0]) + 1 / pow((1 + 2 * bval[i] * wval[i] / k[0]), k[0]));
  }
}

void fnmval(double *bval, double *wval, double *k, double *r)
{
  int i;

  for (i = 0; i < 7; i++) {
    r[i] = (wval[i] * bval[i] * (1 - 1 / pow((1 + wval[i] * bval[i] / k[0]), (k[0] + 1))));
  }
}


void fneval(double *bval, double *dval, double *mval, double *r)
{
  int i;

  for (i = 0; i < 7; i++) {
    r[i] = (mval[i] / (dval[i] * bval[i] / 100));
  }
}

void fnfval(double *bval, double *wval, double *k, double *r)
{
  int i;

  for (i = 0; i < 7; i++) {
    r[i] = (pow((1 + bval[i] * wval[i] / k[0]), (k[0] + 1)) * (bval[i] - (1 - 2 * pow((1 + bval[i] * wval[i] / k[0]), (-k[0])) + pow((1 + 2 * bval[i] * wval[i] / k[0]), (-k[0])))));
  }
}
