/*	JUP_Fill
 *
 *	Purpose:	To fill a rectangular region with a specified color
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Fill( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Imp:	Image Memory Plane
 *		Color:	Color value to fill with
 *		Mask:	Write Mask
 *		Area:	Array of corner coordinates of area to be filled
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Fill( Unit, Imp, Color, Mask, Area )

int *Unit, Imp, Color, Mask, *Area;

{
   int JUP_x, JUP_y;
   int value;
   int vmask;
   int i;

   struct jggxattr ja;

   if (Imp == OVERLAY_IMP && OVERLAY_AVAILABLE)
   {
      j_oi_sel(OI_OVERLAYS);
      vmask = Mask;
      value = Color;
   }
   else					/* Image plane */
   {

/*** GET ATTRIBUTES TO DETERMINE DISPLAY CONFIGURATION ***/

      i = j_get_attr(&ja);

      if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)
      {
         vmask = 0xFFFFFFFF & (((Mask >> 4) & 0xF) << ((Imp-1)*4) );
         value = ((Color >> 4) & 0xF) << ((Imp-1)*4);
      }
      else
      {
         vmask = 0xFFFFFFFF & (Mask << ((Imp-1)*8) );
         value = Color << ((Imp-1)*8);
      }
   }

   j_32wmask(vmask);
   jfflush(J12file);

   j_32sec(value);
   jfflush(J12file);

   JUP_x = JUP_X_IMG(Area[LEFT]);
   JUP_y = JUP_Y_IMG(Area[TOP]);

   j_mov(JUP_x,JUP_y);
   jfflush(J12file);

   JUP_x = JUP_X_IMG(Area[RIGHT]);
   JUP_y = JUP_Y_IMG(Area[BOTTOM]);

   j_dfr(JUP_x,JUP_y);
   jfflush(J12file);

   vmask = 0xFFFFFFFF;
   j_32wmask(vmask);
   j_oi_sel(OI_IMAGE);
   jfflush(J12file);

   return (SUCCESS);
}
