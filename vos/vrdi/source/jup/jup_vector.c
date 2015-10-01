/*	JUP_Vector
 *
 *	Purpose:	Connect the dots
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Vector( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Imp:	Image Memory Plane number
 *		Npts:	Number of points in vector
 *		X:	Array of X-coordinates
 *		Y:	Array of Y-coordinates
 *		Value:	Pixel value to draw vector in
 *		Mask:	Write mask
 *
 *	Possible Error Codes:
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Vector( Unit, Imp, Npts, X, Y, Value, Mask )

int *Unit, Imp, Npts, *X, *Y, Value;
char Mask;

{
   int i;
   int *JUP_x,*JUP_y;
   int vmask;
   int color;

   struct jggxattr ja;

/*** CONVERT ARRAY OF X,Y COORDINATES TO JUPITER COORDINATE SYSTEM ***/

   JUP_x = malloc(Npts*4);
   JUP_y = malloc(Npts*4);

   for (i=0;i<Npts;i++)
   {
      *(JUP_x+i) = JUP_X_IMG(*(X+i));
      *(JUP_y+i) = JUP_Y_IMG(*(Y+i));
   }

   if (Imp == OVERLAY_IMP && OVERLAY_AVAILABLE)
   {
      j_oi_sel(OI_OVERLAYS);
      vmask = Mask;
      color = Value;
   }
   else						/* Image planes */
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

   j_mov(*JUP_x,*JUP_y);
   j_dmva(Npts,JUP_x,JUP_y);
   jfflush(J12file);

   vmask = 0xFFFFFFFF;
   j_32wmask(vmask);
   j_oi_sel(OI_IMAGE);
   jfflush(J12file);

   return ( SUCCESS );
}
