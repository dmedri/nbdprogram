/*
 * ndbprogram/src/fnrod.c
 * Copyright (c) 2017 Daniele Medri
 * Distributed under the GNU GPL-2 license
*/

#include <stdio.h>
#include <math.h>

void fnrod(double *i, double *j, double *r)
{
        r[0] = ((i[0] * pow(10.0, j[0])) + 0.5) / pow(10.0, j[0]);
}
