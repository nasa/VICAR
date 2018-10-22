/*
   Header file for C-bridges to LSPFIT subroutines.  See lspfit.hlp or
   zlspfit.c in lspfit.com for more information.

   History:

      April 6, 1998     Thomas Huang     Initial release.
*/

#ifndef _ZLSPFIT_H
#define _ZLSPFIT_H

int zlspfit    (int, double *, double *, double *);

int zclfit     (int, double *, double *, double *);

void zmoment   (int, float *, int, double *, double *);

void zrmsfit   (int, int, float *, int, double *, double *, double *, int *);

void zpolytran (int, double *, float, float, float *, float *);

#endif

