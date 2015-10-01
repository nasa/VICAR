#ifndef MOMATI_H
#define MOMATI_H

/* Prototype for momati */

void zmomati(double oal, double oas,double ssl,double sss,double scale,
             double fl,double sclo,double scla,double angn,double range,
             void *a,void *rs);

void momati_c(double oal, double oas, double ssl, double sss,
              double scale, double fl, double sclo, double scla,
              double angn, double range, double a[3][3], double rs[3]);

#endif


