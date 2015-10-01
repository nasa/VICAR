/*	JUP_Circle
 *
 *	Purpose:	Draw a circle of given radius at specified location
 *			on the Jupiter J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Circle( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Imp:		Image memory plane number
 *		Xcenter:	X-coordinate of center of circle
 *		Ycenter:	Y-coordinate of center of circle
 *		Radius:		Radius of circle in pixels
 *		Value:		Pixel value of circle to be drawn
 *		Mask:		Write mask
 *
 *	Possible Error Codes:
 *
 *	What about clipping at access window?
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Circle( Unit, Imp, Xcenter, Ycenter, Radius, Value, Mask )

int *Unit, Imp, Xcenter, Ycenter, Radius, Value;
char Mask;

{
   int JUP_x,JUP_y;
   int vmask;
   int color;
   int i;

   struct jggxattr ja;

   if (Imp == OVERLAY_IMP && OVERLAY_AVAILABLE)
   {
      j_oi_sel(OI_OVERLAYS);
      vmask = Mask;
      color = Value;
   }
   else				/* Image planes */
   {

/*** GET ATTRIBUTES TO DETERMINE DISPLAY CONFIGURATION ***/

      i = j_get_attr(&ja);

      if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)
      {
         vmask = 0xFFFFFFFF & (((Mask >> 4) & 0xF) << ((Imp-1)*4) );
         color = ((Value >> 4) & 0xF) << ((Imp-1)*4);
      }
      else
      {
         vmask = 0xFFFFFFFF & (Mask << ((Imp-1)*8) );
         color = Value << ((Imp-1)*8);
      }
   }

   j_32wmask(vmask);
   jfflush(J12file);

   j_32sec(color);
   jfflush(J12file);

   JUP_x = JUP_X_IMG(Xcenter);
   JUP_y = JUP_Y_IMG(Ycenter);

   j_mov(JUP_x,JUP_y);
   j_dcl(Radius);
   jfflush(J12file);

   vmask = 0xFFFFFFFF;
   j_32wmask(vmask);
   jfflush(J12file);

   j_oi_sel(OI_IMAGE);

   return ( SUCCESS );
}
