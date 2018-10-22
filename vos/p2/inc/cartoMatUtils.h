#ifndef CARTOMATUTILS_H
#define CARTOMATUTILS_H

int dceiling( double x );

double xzprod( double x1, double y1, double x2, double y2 );

/* signed triangular area, for polygon use xp,yp as fixed point */
/* in (E,N) coordinates,        clockwise is positive area */
/* in (N,E) coordinates, counterclockwise is positive area */

double triarea( double xp, double yp, double x1, double y1, double x2, double y2 );

void lin2( double * a, double * b, double * x, double eps );

void segxseg( double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double * w );

void insert_seg( int jj, int * ccount, double * p4max, double xbig, double ybig, double xjbig, double yjbig, int wchcall );

void thiessen( int npoints, int * nlinret, double reject, double skinny, int abendi, double * ptx, double * pty, int * ntriang, int ** tcon1, int ** tcon2, int ** tcon3 );

int insidetri( double x, double y, double x1, double y1, double x2, double y2, double x3, double y3 );

#endif
