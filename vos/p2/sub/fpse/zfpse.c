#include "xvmaininc.h" 
/* #include "vicmain_c" */
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* C Bridge for FPSE - FPS emulation package
/************************************************************************/

void zapinit (void)
{

   FTN_NAME2(apinit, APINIT) ( );
   return;
}


/*
calling statement:  void zapwd ();
Where:

*/

void zapwd (void)
{

   FTN_NAME2(apwd, APWD) ( );

}

/*
calling statement:  void zapwr ();
Where:

*/

void zapwr (void) 
{

   FTN_NAME2(apwr, APWR) ( );
   return;
}

/*
Move N elements from APMEM starting at specified offset OFFS to array BUF,
reformatting as specified by TYPE:
	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
	       1: in = I*4, out = I*2 ( then use VFIX)
	       2: in/out = R*4
	       3: in/out = R*4 (ignore "IBM format")

calling statement:  void zapget (&buf, offs, n, type);
Where:
	buf   : void pointer to local buffer to receive data
		move to AP memory
        offs  : offset into AP memory
        n     : number of elements to move
        type  : type of data transfer:
		0: in/out I*4
		1: in = I*4, out = I*2
		2: in/out = R*4
		3: in/out = R*4 

*/

void zapget (buf, offs, n, type) 
void *buf;
int  offs, n, type;
{

   FTN_NAME2(apget, APGET) (buf, &offs, &n, &type);
   return;

}


/*
Move N elements from array BUF to APMEM starting at specified offset OFFS,
reformatting as specified by TYPE:
	TYPE = 0: in/out = I*4 (then use VUP8, VFLT32)
	       1: in = I*2, out = I*4 ( then use VFLT)
	       2: in/out = R*4
	       3: in/out = R*4 (ignore "IBM format")

calling statement:  void zapput (&buf, offs, n, type);
Where:
	buf   : void pointer to local buffer containing data to be
		moved to AP memory
        offs  : offset into AP memory
        n     : number of elements to move
        type  : type of data transfer:
		0: in/out I*4
		1: in = I*4, out = I*2
		2: in/out = R*4
		3: in/out = R*4 

*/
void zapput (buf, offs, n, type) 
void *buf;
int  offs, n, type;
{

   FTN_NAME2(apput, APPUT) (buf, &offs, &n, &type);
   return;

}


/*
Correlate or convolve arrays A and B to obtain C:
	C(mK) = SUM (A((m+q)I) * B(qJ),	from q = 1 to N2, for m = 1,...,N1
Note that A, B, and C are offsets in APMEM.
If I & J have the same sign, thje operation is correlation, else it
is convolution.

calling statement:  void zconv (a, i, b, j, c, k, n1, n2);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'b'
        j     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        n1    : 
        n2    : 

*/
void zconv (a, i, b, j, c, k, n1, n2) 
int  a,b,c;
int  i,j,k,n1,n2;
{

   FTN_NAME2(conv, CONV) (&a, &i, &b, &j, &c, &k, &n1, &n2);
   return;

}


/*
Generates the histogram of an array starting at AP menory offset A, 
increment = I, with limits AMAX, AMIN, and puts the results in the 
AP memory array at offset C.

calling statement:  void zhist (a, i, c, n, nb, amax, amin);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	c     : Offset into AP memory for array 'c'
        n     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        n1    : 
        n2    : 

*/
void zhist (a, i, b, j, c, k, n1, n2) 
int  a,b,c;
int  i,j,k,n1,n2;
{

   FTN_NAME2(hist, HIST) (&a, &i, &b, &j, &c, &k, &n1, &n2);
   return;

}



/*
Matrix multiply arrays A and B to obtain C

calling statement:  void zmmul32 (a, i, b, j, c, k, mc, nc, na);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'c'
        j     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        mc    : 
        nc    : 
        na    : 

*/
void zmmul32 (a, i, b, j, c, k, mc, nc, na)
int  a,b,c;
int  i,j,k,mc,nc,na;
{

   FTN_NAME2(mmul32, MMUL32) (&a, &i, &b, &j, &c, &k, &mc, &nc, &na);
   return;

}




/*
Add arrays A and B to obtain C:
	C(mK) = A(mI) + B(mJ),	m = 0,...,N-1
Note that A, B, and C are offsets in APMEM.


calling statement:  void zvaddem (a, i, b, j, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'b'
        j     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        n     : number of elements in each array.
*/
void zvaddem (a, i, b, j, c, k, n) 
int  a,b,c;
int  i,j,k,n;
{

   FTN_NAME2(vaddem, VADDEM) (&a, &i, &b, &j, &c, &k, &n);
   return;

}

/*
Move array A to D, clipping it to the range (B - C):
	D(mK) = B	if A(mI) < B		m = 0,...,N-1
		A(mI) 	if B <= A(mI) < C
		C	if C <= A(mI)
Note that A, B, C, and D are offsets in APMEM.

calling statement:  void zvclip( a, i, b, c, d, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'b'
	c     : Offset into AP memory for array 'c'
	d     : Offset into AP memory for array 'd'
        k     : Increment k from 'd'
        n     : 
*/
void zvclip( a, i, b, c, d, k, n)
int  a,b,c,d;
int  i,k,n;
{

   FTN_NAME2(vclip, VCLIP) ( &a, &i, &b, &c, &d, &k, &n);
   return;

}

/*
Clears an array starting at C, increment = K.


calling statement:  void zvclr( c, k, n);
Where:
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'd'
        n     : Number of elements
*/
void zvclr( c, k, n)
int  c;
int  k,n;
{

   FTN_NAME2(vclr, VCLR) ( &c, &k, &n);
   return;

}


/*
Convert elements from Floating-point to Integer:
	C(mK) = FIX( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvfix( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvfix( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vfix, VFIX) ( &a, &i, &c, &k, &n);
   return;

}

/*
Convert elements from Floating-point to Integer:
	C(mK) = FIX( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvfix32( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvfix32( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vfix32, VFIX32) ( &a, &i, &c, &k, &n);
   return;

}

/*
Convert elements from Integer to Floating-point
	C(mK) = FLOAT( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvflt ( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvflt( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vflt, VFLT) ( &a, &i, &c, &k, &n);
   return;

}


/*
Convert elements from Integer to Floating-point
	C(mK) = FLOAT( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvflt32 ( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvflt32( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vflt32, VFLT32) ( &a, &i, &c, &k, &n);
   return;

}




/*
Move array A to C:
	C(mK) = A(mI) 
Note that A and C are offsets in APMEM.

calling statement:  void zvmov( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvmov( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vmov, VMOV) ( &a, &i, &c, &k, &n);
   return;

}


/*
Packs lo-order byte (unsigned) from 4 words of A into one word of C,
for n elements.
Note that A and C are offsets in APMEM.

calling statement:  void zvpk8( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvpk8( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vpk8, VPK8) ( &a, &i, &c, &k, &n);
   return;

}


/*
Add array A and scalar B to obtain C:
	C(mK) = A(mI) + B,	m = 0,...,N-1
Note that A, B, and C are offsets in APMEM.

calling statement:  void zvsadd( a, i, b, c, k, n);
Where:
	a     : Offset into AP memory for array A
        i     : Increment i from 'a'
	b     : Offset address for scaler B
	c     : Offset into AP memory for array C
        k     : Increment k from 'c'
        n     : Number of elements
*/

void zvsadd( a, i, b, c, k, n)
int  a,b,c;
int  i,k,n;
{

   FTN_NAME2(vsadd, VSADD) ( &a, &i, &b, &c, &k, &n);
   return;

}

/*
Multiply array A and scalar B to obtain C:
	C(mK) = A(mI) * B,	m = 0,...,N-1
Note that A, B, and C are offsets in APMEM.

calling statement:  void zvsmul( a, i, b, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvsmul( a, i, b, c, k, n)
int  a,b,c;
int  i,k,n;
{

   FTN_NAME2(vsmul, VSMUL) ( &a, &i, &b, &c, &k, &n);
   return;

}


/* Unpack 4 bytes (unsigned) from each word of A into 4 words of C.

   calling statement:  void zvup8( a, i, c, k, n);
   Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements

   Call fortran function zup8 to get address of AP memory block. ZUP8
   then calls C function zfvup8 below to perform remainder of unpack function.
*/
                   


void zvup8( a, i, c, k, n)
int  *a,*c;
int  *i,*k,*n;
{

   FTN_NAME2(vup8, VUP8) ( &a, &i, &c, &k, &n);
   return;

}


void FTN_NAME2 (zfvup8, ZFVUP8)( zbuf, a, i, c, k, n)
int *zbuf;
int *a,*c;
int *i,*k,*n;
{
int j, m, ibuf, II, ibuf2;
int A,I,C,K,N;
float *fptr;


   I = *i;
   A = *a;
   C = *c;
   K = *k;
   N = *n;

   for (m = 0; m < N; m++) {
      II = A + m * I;
      ibuf = *(zbuf + II);
      for (j = 0; j < 4; j++) {
         ibuf2 = (ibuf & 0x00ff);
         II = C + ((4 * m + j) * K);
         fptr= (float *) (zbuf + II);
         *fptr = (float)ibuf2;
         ibuf = (ibuf >> 8);
      }
   }
   return;
}

