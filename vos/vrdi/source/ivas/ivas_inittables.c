/*	IVAS_InitTables - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_InitTables( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "ivasinc.h"

FUNCTION IVAS_InitTables( Unit )
int	*Unit;
{
   AVAIL_CONFIGS = IMP_1024_BIT | VIDEO_1024_BIT | ASPECT_1_1_BIT |
   		   FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;
   DEV_UNIT_NO = 0;

   EACH_IMP_HAS_DW = FALSE;

   N_LUT_SECTIONS      = 1;
   MAY_BYPASS_LUT      = FALSE;
   MAX_LUT_VALUE       = 255;
   EACH_IMP_ZOOMS      = FALSE;
   MAY_CONNECT_IMP_LUT = TRUE;
   MAY_ZOOM_IMPS       = TRUE;
   MAX_ZOOM_FACTOR     = 16;

   MAY_CONNECT_IMP_OVERLAY = FALSE;
   MAY_BYPASS_OVERLAY_LUT  = FALSE;
   OVERLAY_IMP             = 4;
   OVERLAY_LUT_CHAR        = SEPERATE_LUT;
   N_OVERLAY_SECTIONS      = 1;
   BITS_IN_OVERLAY         = 4;

   AFG_AVAILABLE         = FALSE;
   N_AFG_LINES           = 0;
   N_AFG_CHARACTERS      = 0;
   N_AFG_CHARACTER_TYPES = 0;

   N_CURSOR_TYPES       = 7;
   N_CURSOR_BLINK_RATES = 8;
   MAY_RESIZE_CURSOR	= TRUE;
   MAY_COLOR_CURSOR	= TRUE;
   MAX_CURSOR_XSIZE     = 128;
   MAX_CURSOR_YSIZE     = 128;
   IVAS_CURSOR_FORM	= 1;
   IVAS_CURSOR_XSIZE	= 21;
   IVAS_CURSOR_YSIZE	= 21;

   if ( AUTO_TRACK_AVAILABLE ) {
     AUTO_TRACK_DEVICE    = 1;
     AUTO_TRACK_CURSOR    = 1;

     N_IO_DEVICES         = 2;

     IO_DEV_1_TYPE        = DEVICE_2D;
     IO_DEV_1_COORDINATES = ABSOLUTE;
     IO_DEV_1_PEN         = 1;
     IO_DEV_1_N_SWITCHES  = 3;

     IO_DEV_2_TYPE        = DEVICE_SWITCH;
     IO_DEV_2_COORDINATES = NO_COORDINATES;
     IO_DEV_2_PEN         = 0;
     IO_DEV_2_N_SWITCHES  = 3;
   }
   return (SUCCESS);
}
