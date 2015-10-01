/*	JUP_InitTables - description
 *
 *	Purpose:	Initialize values in Device Control Block (DCB)
 *
 *	Written by:	Fred Burnette
 *	Date:		September 27, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_InitTables( Unit )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#define JUP_INITIALIZE
#include "jupinc.h"

FUNCTION JUP_InitTables( Unit )
int	*Unit;

{
   AVAIL_CONFIGS = IMP_1280_1024_BIT | VIDEO_1280_1024_BIT | VIDEO_640_480_BIT |
		   FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT |
		   ASPECT_1_1_BIT |
		   IMP_1024_BIT | VIDEO_1024_BIT;	/* temp. only */

   DEV_UNIT_NO = 0;

   EACH_IMP_HAS_DW = FALSE;

   N_LUT_SECTIONS      = 1;
   MAY_BYPASS_LUT      = FALSE;
   MAX_LUT_VALUE       = 255;
   EACH_IMP_ZOOMS      = FALSE;
   MAY_CONNECT_IMP_LUT = FALSE;
   MAY_ZOOM_IMPS       = TRUE;
   MAX_ZOOM_FACTOR     = 16;

   BITS_IN_OVERLAY         = 4;
   MAY_CONNECT_IMP_OVERLAY = FALSE;
   MAY_BYPASS_OVERLAY_LUT  = FALSE;
   OVERLAY_IMP             = 4;		/* Temporary, reset in jup_opendevice */
   OVERLAY_LUT_CHAR        = SEPERATE_LUT;
   N_OVERLAY_SECTIONS      = 1;

   AFG_AVAILABLE         = FALSE;
   N_AFG_LINES           = 0;
   N_AFG_CHARACTERS      = 0;
   N_AFG_CHARACTER_TYPES = 0;

   N_CURSOR_TYPES       = 7;
   N_CURSOR_BLINK_RATES = 1;

   MAY_COLOR_CURSOR	= TRUE;
   MAY_RESIZE_CURSOR	= FALSE;
/*  The numbers used here are dummy numbers.  If the resizing capability  */
/*  is added, these should be replaced with the actual values.            */
   MAX_CURSOR_XSIZE	= 1;
   MAX_CURSOR_YSIZE	= 1;

   if (AUTO_TRACK_AVAILABLE)
   {
      AUTO_TRACK_DEVICE    = 1;
      AUTO_TRACK_CURSOR    = 1;

      N_IO_DEVICES         = 1;

      IO_DEV_1_TYPE	   = DEVICE_2D;
      IO_DEV_1_COORDINATES = ABSOLUTE;
      IO_DEV_1_PEN         = 0;
      IO_DEV_1_N_SWITCHES  = 3;
   }

   return (SUCCESS);
}
