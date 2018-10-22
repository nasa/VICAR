#ifndef CARTOSORTUTILS_H
#define CARTOSORTUTILS_H

#define CART_CHAR   1
#define CART_SHORT  2
#define CART_INT    3
#define CART_FLOAT  4
#define CART_DOUBLE 5
#define CART_LONG   6
#define CART_UCHAR  7
#define CART_USHORT 8
#define CART_UINT   9
#define CART_ULONG  10
#define CANNOT_COMPARE -2

void getSelectionSortIndices(void *unsorted, int *indices, int n, int type);

void sort8( double * buf, int * ptr, int n );

void sort88( double * buf, int * ptr, int n );

void sortrec4( int * key, int * ptr, int len );

void sortrec88( double * key, int * ptr, int len );

void sortrec8( double *key, int* ptr,int len );

void sortretn8( double *key, int* ptr, int len );

void sort4(int *buf, int *ptr, int n);

void sort7( float *buf, int *ptr, int n );

#endif
