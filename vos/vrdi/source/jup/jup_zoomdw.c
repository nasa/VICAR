/*	JUP_ZoomDW
 *
 *	Purpose:	To zoom and pan image memory planes
 *
 *	Written by:	Fred Burnette
 *	Date:		January 1990
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_ZoomDW( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Function:	ZOOM_IMP or SET_DW
 *		Imp:    	Image Memory Plane number
 *		Zoom:		Zoom factor
 *		X:		X-coordinate for panning
 *		Y:		Y-coordinate for panning
 *
 *	Possible Error Codes:
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_ZoomDW( Unit, Function, Imp, Zoom, X, Y )

int *Unit, Function, Imp, Zoom, X, Y;

{
   int status, i;
   int JUP_x, JUP_y;

   if ( (Zoom < 1) || ( Zoom > MAX_ZOOM_FACTOR ) ) return ( CANNOT_ZOOM );

   JUP_x = JUP_X_IMG(X);		/* Start at top of screen, not bottom */
   JUP_x &= ~0x01;			/* Only pans to even addresses (!) */
   JUP_y = ( JUP_Y_IMG(Y) + 1 - (JUP_VIDEO_LINES / Zoom)) % N_LINES;

   DW_LEFT(Imp) = JUP_IMG_X(JUP_x);	/* Notify everyone of even-only pan */
   if (!(EACH_IMP_ZOOMS && EACH_IMP_HAS_DW) && Function == SET_DW)
      for (i = 1; i <= N_IMPS; i++)
         DW_LEFT(i) = JUP_IMG_X(JUP_x);

   if (EACH_IMP_ZOOMS && EACH_IMP_HAS_DW)	/* only true in mono w/graph */
   {
      if ( Imp == OVERLAY_IMP && OVERLAY_AVAILABLE)
      {
         status = SUCCESS;
         j_oi_sel(OI_OVERLAYS);
         j_zoom(JUP_x,JUP_y,Zoom,Zoom);
         j_oi_sel(OI_IMAGE);
      }
      else
      {
         status = SUCCESS;
         j_zoom(JUP_x,JUP_y,Zoom,Zoom);
      }
   }
   else			/* Must do all, incl graphics, for VRDI compat */
   {			/* (device can do graphics/image separately...) */
      status = MUST_SET_ALL_DWS;
      if ( Function == ZOOM_IMP ) status = MUST_ZOOM_ALL;
      j_zoom(JUP_x,JUP_y,Zoom,Zoom);
      if (OVERLAY_AVAILABLE)
      {
         j_oi_sel(OI_OVERLAYS);
         j_zoom(JUP_x,JUP_y,Zoom,Zoom);
         j_oi_sel(OI_IMAGE);
      }
   }

   jfflush(J12file);

   JUP_SendPanZoom(Unit);		/* tell mouse process */

   return ( status );
}
