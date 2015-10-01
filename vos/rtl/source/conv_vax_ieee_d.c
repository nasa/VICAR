#include "xvmaininc.h"
#include "applic.h"
#include "rtlintproto.h"

/* Shift x[1]..x[7] right three bits by bytes, putting the result    */
/* in y[1]..y[7].  Don't bother with y[0] since that's all exponent. */
#define SHIFT_RIGHT_3(y,x)						\
   { y[7] = ((x[7]>>3) & 0x1F) | ((x[6]<<5) & 0xE0);			\
     y[6] = ((x[6]>>3) & 0x1F) | ((x[5]<<5) & 0xE0);			\
     y[5] = ((x[5]>>3) & 0x1F) | ((x[4]<<5) & 0xE0);			\
     y[4] = ((x[4]>>3) & 0x1F) | ((x[3]<<5) & 0xE0);			\
     y[3] = ((x[3]>>3) & 0x1F) | ((x[2]<<5) & 0xE0);			\
     y[2] = ((x[2]>>3) & 0x1F) | ((x[1]<<5) & 0xE0);			\
     y[1] = ((x[1]>>3) & 0x1F);					\
   }

/* Shift x[1]..x[7] left three bits by bytes, putting the result    */
/* in y[1]..y[7].  Don't bother with y[0] since that's all exponent. */
#define SHIFT_LEFT_3(y,x)						\
   { y[1] = ((x[1]<<3) & 0xF8) | ((x[2]>>5) & 0x07);			\
     y[2] = ((x[2]<<3) & 0xF8) | ((x[3]>>5) & 0x07);			\
     y[3] = ((x[3]<<3) & 0xF8) | ((x[4]>>5) & 0x07);			\
     y[4] = ((x[4]<<3) & 0xF8) | ((x[5]>>5) & 0x07);			\
     y[5] = ((x[5]<<3) & 0xF8) | ((x[6]>>5) & 0x07);			\
     y[6] = ((x[6]<<3) & 0xF8) | ((x[7]>>5) & 0x07);			\
     y[7] = ((x[7]<<3) & 0xF8);						\
   }

/************************************************************************/
/* Convert between IEEE and Vax double-precision floating point (D fmt).*/
/* Both formats are represented as:					*/
/* (-1)^s * f * 2^(e-bias)						*/
/* where s is the sign bit, f is the mantissa (see below), e is the	*/
/* exponent, and bias is the exponent bias (see below).			*/
/* There is an assumed leading 1 on the mantissa (except for IEEE	*/
/* denormalized numbers), but the placement of the binary point varies.	*/
/* A difference not found in single-precision is in the number of bits	*/
/* for each part:  IEEE puts 3 more bits in the exponent, while VAX	*/
/* puts them in the mantissa.						*/
/*									*/
/* IEEE format:	seeeeeee eeeeffff 8*f 8*f 8*f 8*f 8*f 8*f		*/
/*		where e is exponent with bias of 1023 and f is of the	*/
/*		form 1.fffff...						*/
/* Special cases:							*/
/*	e=2047, f!=0:		NaN (Not a Number)			*/
/*	e=2047, f=0:		Infinity (+/- depending on s)		*/
/*	e=0, f!=0:		Denormalized numbers, of the form	*/
/*				(-1)^s * (0.ffff) * 2^(-1022)		*/
/*	e=0, f=0:		Zero (can be +/-)			*/
/*									*/
/* VAX format:	seeeeeee efffffff 8*f 8*f 8*f 8*f 8*f 8*f		*/
/*		where e is exponent with bias of 128 and f is of the	*/
/*		form .1fffff...						*/
/* Byte swapping: Note that the above format is the logical format,	*/
/*		which can be represented as bytes			*/
/*		SE1 E2F1 F2 F3 F4 F5 F6 F7.  The actual order in mem is	*/
/*		E2F1 SE1 F3 F2 F5 F4 F7 F6 (which is four half-word	*/
/*		swaps, NOT a full-word or quad-word swap).		*/
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
/* status = vax_ieee_d(from,to)						*/
/* void *from, *to;							*/
/*									*/
/* This routine will convert VAX D double-precision floating point	*/
/* values to IEEE double precision floating point.			*/
/* NOTE:  Since IEEE has fewer mantissa bits (by 3), there is less	*/
/* precision in IEEE than in VAX.  The extra three bits are truncated	*/
/* during the conversion, which amounts to a truncation toward 0,	*/
/* *NOT* a rounding.							*/
/* Note:  exp is an unsigned short, but extreme care is taken to make	*/
/* sure it will work on either byte order.				*/
/************************************************************************/

int v2_vax_ieee_d(unsigned char *from, unsigned char *ieee)
{
   unsigned char vaxf[8];
   unsigned short exp;
   unsigned char exph, expl;

   v2_double_byte_swap(from, vaxf);	/* Put bytes in rational order */

   exp = (unsigned short)((vaxf[0]<<1)&0xFE) | ((vaxf[1]>>7)&0x01);

   if (exp == 0) {		/* Zero or invalid pattern */
      if (vaxf[0]&0x80) {	/* Sign bit set, which is illegal for VAX */
         ieee[0] = 0x7F;		/* IEEE NaN */
         ieee[1] = 0xFF;
         ieee[2] = 0xFF;
         ieee[3] = 0xFF;
         ieee[4] = ieee[5] = ieee[6] = ieee[7] = 0xFF;
      }
      else {			/* Zero */
         ieee[0] = ieee[1] = ieee[2] = ieee[3] = 0;
         ieee[4] = ieee[5] = ieee[6] = ieee[7] = 0;
      }
   }

   else {				/* Normal case */
      exp += 894;			/* Change to IEEE bias */
      exph = (exp>>4) & 0x7F;		/* convert to unsigned chars */
      expl = (exp<<4) & 0xF0;
      SHIFT_RIGHT_3(ieee, vaxf);	/* Shift mantissa (NOTE: rounds to 0) */
      ieee[0] = (vaxf[0]&0x80) | exph;	/* Put back exponent and sign */
      ieee[1] = (ieee[1] & 0x0F) | expl;
   }

   return (SUCCESS);
}


/************************************************************************/
/* status = ieee_vax_d(from,to)						*/
/* void *from, *to;							*/
/*									*/
/* This routine will convert IEEE double precision floating point	*/
/* values to VAX D double precision floating point.			*/
/* NOTE:  The VAX has 3 fewer bits in the exponent than IEEE, so it is	*/
/* likely to be out-of-range.  If the exponent is too high, then	*/
/* +/- MAXFLOAT is returned.  If it's too low, 0 is returned.		*/
/* Note:  exp is an unsigned short, but extreme care is taken to make	*/
/* sure it will work on either byte order.				*/
/************************************************************************/

int v2_ieee_vax_d(unsigned char *ieee, unsigned char *to)
{
   unsigned char vaxf[8];
   unsigned short exp;
   unsigned char expl, exph;

   expl = (ieee[1] >> 4) & 0x0F;
   exph = ieee[0] & 0x7F;
   exp = ((((unsigned short)exph) << 4) & 0x07F0) | ((unsigned short)expl);

   /* Exponent 2047 means NaN or Infinity, exponents 1150 to 2046 are	*/
   /* too large for VAX notation.  In either case, set to sign *	*/
   /* highest possible number.						*/

   if (exp >= 1150) {			/* Infinity or NaN or too big */
      vaxf[0] = 0x7F | (ieee[0]&0x80);
      vaxf[1] = 0xFF;
      vaxf[2] = 0xFF;
      vaxf[3] = 0xFF;
      vaxf[4] = vaxf[5] = vaxf[6] = vaxf[7] = 0xFF;
   }

   else if (exp <= 894) {		/* Too small or zero */
      vaxf[0] = vaxf[1] = vaxf[2] = vaxf[3] = 0;
      vaxf[4] = vaxf[5] = vaxf[6] = vaxf[7] = 0;
   }

   else {				/* Normal case */
      exp -= 894;			/* Change to VAX bias */
      expl = (unsigned char)exp;	/* Now in range 1..255 */
      SHIFT_LEFT_3(vaxf, ieee);		/* Shift mantissa */
      vaxf[0] = (ieee[0]&0x80) | ((expl>>1) & 0x7F);
      vaxf[1] = (vaxf[1]&0x7f) | ((expl<<7) & 0x80);
   }

   v2_double_byte_swap(vaxf, to);	/* Put bytes in weird VAX order */

   return (SUCCESS);
}

/************************************************************************/
/* double_byte_swap(from, to);						*/
/*									*/
/* Swap bytes from ABCDEFGH to BADCFEHG.  From and to can't be the same.*/
/************************************************************************/

int v2_double_byte_swap(unsigned char from[8], unsigned char to[8])
{
   to[0] = from[1];
   to[1] = from[0];
   to[2] = from[3];
   to[3] = from[2];
   to[4] = from[5];
   to[5] = from[4];
   to[6] = from[7];
   to[7] = from[6];

   return SUCCESS;
}
