#include "VIDSdefs.h"

/* This file contains various pixel move subroutines.  They are coded	*/
/* in MACRO for the VAX.						*/

#if !VAX_ARCH

struct pix_complex {
   float real;
   float imag;
};
 

/*----------------------------------------------------------------------*/
/*	BlockMove(SRC, DST, LEN) Moves LEN bytes from SRC to DST.	*/
/*	SRC		addr of source					*/
/*	DST		addr of destination				*/
/*	LEN		amount to move					*/
/*----------------------------------------------------------------------*/

BlockMove(src, dest, len)
unsigned char *src, *dest;
int len;
{
   zmove(src, dest, len);
}

/*----------------------------------------------------------------------*/
/*	BlockFill(VAL, DST, LEN) Fills DST with LEN bytes of VAL.	*/
/*	VAL		value to fill with				*/
/*	DST		addr of destination				*/
/*	LEN		amount to move					*/
/*----------------------------------------------------------------------*/
BlockFill(val, dst, len)
int val;		/* single byte */
unsigned char *dst;
int len;
{
   memset(dst, val, len);
}

/*----------------------------------------------------------------------*/
/*	IncreMove(SRC, DST, LEN, INC)					*/
/*									*/
/*	Moves LEN bytes from SRC to DST, incrementing the SRC counter	*/
/*	by INC for each byte.  The effect is to zoom the buffer down	*/
/*	by subsampling.							*/
/*	SRC		addr of source					*/
/*	DST		addr of destination				*/
/*	LEN		number of pixels to move			*/
/*	INC		number of bytes to incr SRC addr for each pixel	*/
/*----------------------------------------------------------------------*/
IncreMove(src, dst, len, inc)
unsigned char *src, *dst;
int len, inc;
{
   register int i;
   register unsigned char *s, *d;
   s = src;
   d = dst;

   for (i=0; i<len; i++) {
      *d++ = *s;
      s += inc;
   }
}

/*----------------------------------------------------------------------*/
/*	ExpandMove(SRC, DST, DSTLEN, LEN, INC, ZOOM, SUBPIX)		*/
/*									*/
/*	Moves LEN bytes from SRC to DST, incrementing the SRC counter	*/
/*	by INC for each byte, and making ZOOM copies of each byte.	*/
/*	Note that if DSTLEN expires before LEN pixels are moved, then	*/
/*	some pixels are left unmoved.					*/
/*	SRC		addr of source					*/
/*	DST		addr of destination				*/
/*	DSTLEN		Length in bytes of destination			*/
/*	LEN		number of pixels to move			*/
/*	INC		number of bytes to incr SRC addr for each pixel	*/
/*	ZOOM		Number of copies of each byte			*/
/*	SUBPIX		If not 0, number of copies of first byte only	*/
/*----------------------------------------------------------------------*/
ExpandMove(src, dst, dstlen, len, inc, zoom, subpix)
unsigned char *src, *dst;
int dstlen, len, inc, zoom, subpix;
{
   register unsigned char *s, *d;
   register int i, j;

   s = src;
   d = dst;

   if (subpix != 0) {			/* is the first pixel a partial one? */
      for (j=0; j<subpix && dstlen>0; j++) {
         *d++ = *s;
         dstlen--;
      }
      s += inc;
      len--;				/* one less pixel to move */
   }

   for (i=0; i<len && dstlen>0; i++) {
      for (j=0; j<zoom && dstlen>0; j++) {	/* move zoom copies of pixel */
         *d++ = *s;
         dstlen--;
      }
      s += inc;
   }
}

/*----------------------------------------------------------------------*/
/*	IncreTrans(SRC, DST, LEN, INC, FORM, SLOPE, OFFS)		*/
/*									*/
/*	Translate LEN pixels from					*/
/*	SRC		addr of source					*/
/*	DST		addr of destination				*/
/*	LEN		number of pixels to move (longword by value).	*/
/*	INC		number of bytes to incr DST addr for each pixel	*/
/*	FORM		format constant (see VIDSdefs.h)		*/
/*	SLOPE		multiply each pixel by SLOPE-- D_Floating point	*/
/*	OFFS		add OFFS to each pixel-- D_Floating point	*/
/*----------------------------------------------------------------------*/

int IncreTrans(src, dst, len, inc, form, slope, offs)
unsigned char *src, *dst;
int len, inc;
FileFormat form;
double slope, offs;
{
   register int i;
   register unsigned char *d;
   unsigned char *bs;
   short *hs;
   int *fs;
   float *rs;
   double *ds;
   struct pix_complex *cs;
   double value;

   d = dst;

   switch (form) {

      case ByteFormat:
         bs = (unsigned char *)src;
         for (i=0; i<len; i++) {
            value = *bs * slope + offs;
            if (value < 0)
               *d++ = 0;
            else if (value > 255)
               *d++ = 255;
            else
               *d++ = (unsigned char) value;
            bs += inc;
         }
         break;

      case HalfFormat:
         hs = (short *)src;
         for (i=0; i<len; i++) {
            value = *hs * slope + offs;
            if (value < 0)
               *d++ = 0;
            else if (value > 255)
               *d++ = 255;
            else
               *d++ = (unsigned char) value;
            hs += inc;
         }
         break;

      case FullFormat:
         fs = (int *)src;
         for (i=0; i<len; i++) {
            value = *fs * slope + offs;
            if (value < 0)
               *d++ = 0;
            else if (value > 255)
               *d++ = 255;
            else
               *d++ = (unsigned char) value;
            fs += inc;
         }
         break;

      case RealFormat:
         rs = (float *)src;
         for (i=0; i<len; i++) {
            value = *rs * slope + offs;
            if (value < 0)
               *d++ = 0;
            else if (value > 255)
               *d++ = 255;
            else
               *d++ = (unsigned char) value;
            rs += inc;
         }
         break;

      case DoubFormat:
         ds = (double *)src;
         for (i=0; i<len; i++) {
            value = *ds * slope + offs;
            if (value < 0)
               *d++ = 0;
            else if (value > 255)
               *d++ = 255;
            else
               *d++ = (unsigned char) value;
            ds += inc;
         }
         break;

      case CompFormat:
         cs = (struct pix_complex *)src;
         for (i=0; i<len; i++) {
            value = cs->real * slope + offs;		/* real part only */
            if (value < 0)
               *d++ = 0;
            else if (value > 255)
               *d++ = 255;
            else
               *d++ = (unsigned char) value;
            cs += inc;
         }
         break;

      default:
         return FAIL;
   }
   return SUCCESS;
}

/*----------------------------------------------------------------------*/
/*	ExpandTrans(SRC,DST,DSTLEN,LEN,INC,ZOOM,SUBPIX,FORM,SLOPE,OFFS)	*/
/*									*/
/*	Translate LEN pixels from					*/
/*	SRC		addr of source					*/
/*	DST		addr of destination				*/
/*	DSTLEN		number of bytes in destination			*/
/*	LEN		number of pixels to move (longword by value).	*/
/*	INC		number of bytes to incr SRC addr for each pixel	*/
/*	ZOOM		number of copies to make of each result in DST	*/
/*	SUBPIX		If not 0, number of copies of first byte only	*/
/*	FORM		format constant (see VIDSdefs.h)		*/
/*	SLOPE		multiply each pixel by SLOPE-- D_Floating point	*/
/*	OFFS		add OFFS to each pixel-- D_Floating point	*/
/*----------------------------------------------------------------------*/

int ExpandTrans(src, dst, dstlen, len, inc, zoom, subpix, form, slope, offs)
unsigned char *src, *dst;
int dstlen, len, inc, zoom, subpix;
FileFormat form;
double slope, offs;
{
   register int i, j;
   register unsigned char *d;
   unsigned char *bs;
   short *hs;
   int *fs;
   float *rs;
   double *ds;
   struct pix_complex *cs;
   double value;

   d = dst;

   switch (form) {

      case ByteFormat:
         bs = (unsigned char *)src;

         if (subpix != 0) {		/* is the first pixel a partial one? */
            for (j=0; j<subpix && dstlen>0; j++) {
               value = *bs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            bs += inc;
            len--;				/* one less pixel to move */
         }

         for (i=0; i<len && dstlen>0; i++) {
            for (j=0; j<zoom && dstlen>0; j++) { /* move zoom copies of pixel */
               value = *bs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            bs += inc;
         }

      case HalfFormat:
         hs = (short *)src;

         if (subpix != 0) {		/* is the first pixel a partial one? */
            for (j=0; j<subpix && dstlen>0; j++) {
               value = *hs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            hs += inc;
            len--;				/* one less pixel to move */
         }

         for (i=0; i<len && dstlen>0; i++) {
            for (j=0; j<zoom && dstlen>0; j++) { /* move zoom copies of pixel */
               value = *hs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            hs += inc;
         }
         break;

      case FullFormat:
         fs = (int *)src;

         if (subpix != 0) {		/* is the first pixel a partial one? */
            for (j=0; j<subpix && dstlen>0; j++) {
               value = *fs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            fs += inc;
            len--;				/* one less pixel to move */
         }

         for (i=0; i<len && dstlen>0; i++) {
            for (j=0; j<zoom && dstlen>0; j++) { /* move zoom copies of pixel */
               value = *fs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            fs += inc;
         }
         break;

      case RealFormat:
         rs = (float *)src;

         if (subpix != 0) {		/* is the first pixel a partial one? */
            for (j=0; j<subpix && dstlen>0; j++) {
               value = *rs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            rs += inc;
            len--;				/* one less pixel to move */
         }

         for (i=0; i<len && dstlen>0; i++) {
            for (j=0; j<zoom && dstlen>0; j++) { /* move zoom copies of pixel */
               value = *rs * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            rs += inc;
         }
         break;

      case DoubFormat:
         ds = (double *)src;

         if (subpix != 0) {		/* is the first pixel a partial one? */
            for (j=0; j<subpix && dstlen>0; j++) {
               value = *ds * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            ds += inc;
            len--;				/* one less pixel to move */
         }

         for (i=0; i<len && dstlen>0; i++) {
            for (j=0; j<zoom && dstlen>0; j++) { /* move zoom copies of pixel */
               value = *ds * slope + offs;
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            ds += inc;
         }
         break;

      case CompFormat:
         cs = (struct pix_complex *)src;

         if (subpix != 0) {		/* is the first pixel a partial one? */
            for (j=0; j<subpix && dstlen>0; j++) {
               value = cs->real * slope + offs;		/* real part only */
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            cs += inc;
            len--;				/* one less pixel to move */
         }

         for (i=0; i<len && dstlen>0; i++) {
            for (j=0; j<zoom && dstlen>0; j++) { /* move zoom copies of pixel */
               value = cs->real * slope + offs;		/* real part only */
               if (value < 0)
                  *d++ = 0;
               else if (value > 255)
                  *d++ = 255;
               else
                  *d++ = (unsigned char) value;
               dstlen--;
            }
            cs += inc;
         }

         break;

      default:
         return FAIL;
   }
}

#endif /* !VAX_ARCH */

