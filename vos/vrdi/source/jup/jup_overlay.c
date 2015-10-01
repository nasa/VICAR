/*	JUP_Overlay
 *
 *	Purpose:	To read/write the graphics overlay color look-up
 *			table on the Jupiter J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Overlay( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Function:	READ_OVERLAY_LUT or WRITE_OVERLAY_LUT
 *		red:		Array of red values
 *		green:		Array of green values
 *		blue:		Array of blue values
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Overlay( Unit, Function, red, green, blue )

int *Unit, Function, *red, *green, *blue;

{
   int i;
   struct jggxattr ja;
   clt_vals r[16], g[16], b[16];

   if (Function == READ_OVERLAY_LUT)
   {

      i = j_get_attr(&ja);

      for ( i = 0; i < 16; i++ )
      {
         red[i] = ja.ja_ovr[i];
         green[i] = ja.ja_ovg[i];
         blue[i] = ja.ja_ovb[i];
      }

   }
   else		/* Function == WRITE_OVERLAY_LUT */
   {

      for ( i = 0; i < 16; i++ )
      {
         r[i] = red[i];
         g[i] = green[i];
         b[i] = blue[i];
      }
      j_ov_sset(0,16,r,g,b);
      j_ov_vset(0,16,r,g,b);
      jfflush(J12file);

   }

   return (SUCCESS);
}
