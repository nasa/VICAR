#include "xvmaininc.h"
#include "applic.h"
#include "rtlintproto.h"

/* Shift x[1]..x[3] right one bit by bytes, don't bother with x[0] */
#define SHIFT_RIGHT(x)						\
   { x[3] = ((x[3]>>1) & 0x7F) | ((x[2]<<7) & 0x80);		\
     x[2] = ((x[2]>>1) & 0x7F) | ((x[1]<<7) & 0x80);		\
     x[1] = (x[1]>>1) & 0x7F;					\
   }

/* Shift x[1]..x[3] left one bit by bytes, don't bother with x[0] */
#define SHIFT_LEFT(x)						\
   { x[1] = ((x[1]<<1) & 0xFE) | ((x[2]>>7) & 0x01);		\
     x[2] = ((x[2]<<1) & 0xFE) | ((x[3]>>7) & 0x01);		\
     x[3] = (x[3]<<1) & 0xFE;					\
   }

/************************************************************************/
/* Convert between IEEE and Vax single-precision floating point.	*/
/* Both formats are represented as:					*/
/* (-1)^s * f * 2^(e-bias)						*/
/* where s is the sign bit, f is the mantissa (see below), e is the	*/
/* exponent, and bias is the exponent bias (see below).			*/
/* There is an assumed leading 1 on the mantissa (except for IEEE	*/
/* denormalized numbers), but the placement of the binary point varies.	*/
/*									*/
/* IEEE format:	seeeeeee efffffff 8*f 8*f				*/
/*		where e is exponent with bias of 127 and f is of the	*/
/*		form 1.fffff...						*/
/* Special cases:							*/
/*	e=255, f!=0:		NaN (Not a Number)			*/
/*	e=255, f=0:		Infinity (+/- depending on s)		*/
/*	e=0, f!=0:		Denormalized numbers, of the form	*/
/*				(-1)^s * (0.ffff) * 2^(-126)		*/
/*	e=0, f=0:		Zero (can be +/-)			*/
/*									*/
/* VAX format:	seeeeeee efffffff 8*f 8*f				*/
/*		where e is exponent with bias of 128 and f is of the	*/
/*		form .1fffff...						*/
/* Byte swapping: Note that the above format is the logical format,	*/
/*		which can be represented as bytes SE1 E2F1 F2 F3.	*/
/*		The actual order in memory is E2F1 SE1 F3 F2 (which is	*/
/*		two half-word swaps, NOT a full-word swap).		*/
/* Special cases:							*/
/*	e=0, s=0:		Zero (no +/-)				*/
/*	e=0, s=1:		Invalid, causes Reserved Operand error	*/
/*									*/
/* The same code works on all byte-order machines because only byte	*/
/* operations are performed.  It could perhaps be done more efficiently	*/
/* on a longword basis, but then the code would be byte-order dependent.*/
/* MAKE SURE any mods will work on either byte order!!!			*/
/************************************************************************/

/************************************************************************/
/* status = vax_ieee_r(from,to)						*/
/* void *from, *to;							*/
/*									*/
/* This routine will convert VAX F floating point values to IEEE	*/
/* single precision floating point.					*/
/************************************************************************/

int v2_vax_ieee_r(unsigned char *from, unsigned char *ieee)
{
   unsigned char vaxf[4];
   unsigned char exp;

   v2_real_byte_swap(from, vaxf);	/* Put bytes in rational order */
   memcpy(ieee, vaxf, 4);		/* Since most bits are the same */

   exp = ((vaxf[0]<<1)&0xFE) | ((vaxf[1]>>7)&0x01);

   if (exp == 0) {		/* Zero or invalid pattern */
      if (vaxf[0]&0x80) {	/* Sign bit set, which is illegal for VAX */
         ieee[0] = 0x7F;		/* IEEE NaN */
         ieee[1] = 0xFF;
         ieee[2] = 0xFF;
         ieee[3] = 0xFF;
      }
      else {			/* Zero */
         ieee[0] = ieee[1] = ieee[2] = ieee[3] = 0;
      }
   }

   else if (exp >= 3) {		/* Normal case */
      exp -= 2;
      ieee[0] = (vaxf[0]&0x80) | ((exp>>1)&0x7F);   /* remake sign + exponent */
   }			/* Low bit of exp can't change, so don't bother w/it */

   else if (exp == 2) {		/* Denormalize the number */
      SHIFT_RIGHT(ieee);	/* Which means shift right 1, */
      ieee[1] = (ieee[1] & 0x3F) | 0x40;   /* Add suppressed most signif bit, */
      ieee[0] = vaxf[0] & 0x80;	/* and set exponent to 0 (preserving sign) */
   }

   else {			/* Exp==1, denormalize again */
      SHIFT_RIGHT(ieee);	/* Like above but shift by 2 */
      SHIFT_RIGHT(ieee);
      ieee[1] = (ieee[1] & 0x1F) | 0x20;
      ieee[0] = vaxf[0] & 0x80;
   }

   return (SUCCESS);
}


/************************************************************************/
/* status = ieee_vax_r(from,to)						*/
/* void *from, *to;							*/
/*									*/
/* This routine will convert IEEE single precision floating point	*/
/* values to VAX F floating point.					*/
/************************************************************************/

int v2_ieee_vax_r(unsigned char *ieee, unsigned char *to)
{
   unsigned char vaxf[4];
   unsigned char exp;

   memcpy(vaxf, ieee, 4);	/* Since most bits are the same */

   exp = ((ieee[0]<<1)&0xFE) | ((ieee[1]>>7)&0x01);

   /* Exponent 255 means NaN or Infinity, exponent 254 is too large for */
   /* VAX notation.  In either case, set to sign * highest possible number */

   if (exp == 255 || exp == 254) {		/* Infinity or NaN or too big */
      vaxf[0] = 0x7F | (ieee[0]&0x80);
      vaxf[1] = 0xFF;
      vaxf[2] = 0xFF;
      vaxf[3] = 0xFF;
   }

   else if (exp != 0) {		/* Normal case */
      exp += 2;
      vaxf[0] = (ieee[0]&0x80) | ((exp>>1)&0x7F);   /* remake sign + exponent */
   }			/* Low bit of exp can't change, so don't bother w/it */

   else {			/* exp == 0, zero or denormalized number */
      if (ieee[1] == 0 &&
	  ieee[2] == 0 &&
	  ieee[3] == 0) {		/* +/- 0 */
         vaxf[0] = vaxf[1] = vaxf[2] = vaxf[3] = 0;
      }
      else {			/* denormalized number */
         if (ieee[1] & 0x40) {	/* hi bit set (0.1ffff) */
            SHIFT_LEFT(vaxf);	/* Renormalize */
            vaxf[1] = vaxf[1] & 0x7F;	/* Set vax exponent to 2 */
            vaxf[0] = (ieee[0]&0x80) | 0x01;	/* sign, exponent==2 */
         }
         else if (ieee[1] & 0x20) {	/* next bit set (0.01ffff) */
            SHIFT_LEFT(vaxf);	/* Renormalize */
            SHIFT_LEFT(vaxf);
            vaxf[1] = vaxf[1] | 0x80;	/* Set vax exponent to 1 */
            vaxf[0] = ieee[0]&0x80;		/* sign, exponent==1 */
         }
         else {			/* Number too small for VAX */
            vaxf[0] = vaxf[1] = vaxf[2] = vaxf[3] = 0;	/* so set to 0 */
         }
      }
   }

   v2_real_byte_swap(vaxf, to);	/* Put bytes in weird VAX order */

   return (SUCCESS);
}

/************************************************************************/
/* real_byte_swap(from, to);						*/
/*									*/
/* Swap bytes from ABCD to BADC.  From and to cannot be the same.	*/
/************************************************************************/

int v2_real_byte_swap(unsigned char from[4], unsigned char to[4])
{
   to[0] = from[1];
   to[1] = from[0];
   to[2] = from[3];
   to[3] = from[2];
   return SUCCESS;
}
