/*	JUP_Lut - description
 *
 *	Purpose:	To read or write values to the color lookup table
 *
 *	Written by:	Fred Burnette
 *	Date:		September 22, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Lut( Unit, function, lut, section, buffer )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		function:	WRITE_LUT or READ_LUT
 *		lut:		Lookup table number: r=1,g=2,b=3
 *		section:	Section of LUT (not used)
 *		buffer:		Array of 8-bit values
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Lut( Unit, Function, lut, section, buffer )
int *Unit, Function, lut, section, buffer[256];

{
   int i,j;
   int npts, inc;
   struct jggxattr ja;
   unsigned char red[256],green[256],blue[256];

/*** READ CURRENT COLOR LUT INTO RED,GREEN,BLUE ARRAYS ***/

   i = j_get_attr(&ja);

   npts = 256;
   inc = 1;
   if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)	/* 4 bits only in LUT */
   {
      npts = 16;
      inc = 0x11;
   }
   for (i=0; i < npts; i++)
   {
      red[i] = ja.ja_r[i];
      green[i] = ja.ja_g[i];
      blue[i] = ja.ja_b[i];
   }

   if (Function == WRITE_LUT)
   {

      if (lut == 1)
      {
	 for (i=0, j=0; i < npts; i++, j+=inc)
            red[i] = buffer[j];
      }

      if (lut == 2)
      {
	 for (i=0, j=0; i < npts; i++, j+=inc)
            green[i] = buffer[j];
      }

      if (lut == 3)
      {
	 for (i=0, j=0; i < npts; i++, j+=inc)
            blue[i] = buffer[j];
      }

      j_cltset(0,npts,red,green,blue);
      jfflush(J12file);

   }
   else		/* Function = READ_LUT */
   {

      if (lut == 1)
      {
	 for (i = 0; i < npts; i++)
	 {
            if (npts == 256)
	       buffer[i] = red[i];
            else
               for (j=i*16; j < (i+1)*16; j++)
                  buffer[j] = red[i];
	 }
      }

      if (lut == 2)
      {
	 for (i = 0; i < npts; i++)
	 {
            if (npts == 256)
	       buffer[i] = green[i];
            else
               for (j=i*16; j < (i+1)*16; j++)
                  buffer[j] = green[i];
	 }
      }

      if (lut == 3)
      {
	 for (i = 0; i < npts; i++)
	 {
            if (npts == 256)
	       buffer[i] = blue[i];
            else
               for (j=i*16; j < (i+1)*16; j++)
                  buffer[j] = blue[i];
	 }
      }

   }

   return (SUCCESS);
}
