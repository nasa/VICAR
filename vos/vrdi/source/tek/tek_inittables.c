/*	TEK_InitTables - description
 *
 *	Purpose: Initialize VRDI system tables for the Tektronix display device.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 10, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_InitTables(Unit)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"

#define  TEK_INITIALIZE
#include "tek.h"

FUNCTION TEK_InitTables(Unit)
int *Unit;
{
   if (DEV_TYPE == TEK_4237)
     AVAIL_CONFIGS = IMP_1024_BIT | VIDEO_1024_BIT | ASPECT_1_1_BIT |
                     FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;
   else
     AVAIL_CONFIGS = IMP_512_BIT | IMP_1024_512_BIT | VIDEO_512_BIT |
                     VIDEO_1024_512_BIT | ASPECT_1_1_BIT |
                     FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;

   DEV_UNIT_NO = 0;

   EACH_IMP_HAS_DW = TRUE;

   N_LUT_SECTIONS      = 1;
   MAY_BYPASS_LUT      = TRUE;
   MAX_LUT_VALUE       = 255;
   EACH_IMP_ZOOMS      = TRUE;
   MAY_CONNECT_IMP_LUT = TRUE;
   MAY_ZOOM_IMPS       = TRUE;
   MAX_ZOOM_FACTOR     = 8;

   MAY_CONNECT_IMP_OVERLAY = FALSE;
   MAY_BYPASS_OVERLAY_LUT  = FALSE;
   OVERLAY_IMP             = TEK_OVERLAY;
   TEK_OVERLAY_IMP         = TEK_OVERLAY;
   OVERLAY_LUT_CHAR        = SEPERATE_LUT;
   N_OVERLAY_SECTIONS      = 1;
   BITS_IN_OVERLAY         = 8;
   TEK_OVERLAY_ON          = FALSE;

   AFG_AVAILABLE         = FALSE;
   N_AFG_LINES           = 0;
   N_AFG_CHARACTERS      = 0;
   N_AFG_CHARACTER_TYPES = 0;

   N_CURSOR_TYPES       = 1;
   N_CURSOR_BLINK_RATES = 1;
   MAY_RESIZE_CURSOR	= FALSE;
   MAY_COLOR_CURSOR	= TRUE;

   if ( AUTO_TRACK_AVAILABLE ) {
      AUTO_TRACK_DEVICE    = 1;
      AUTO_TRACK_CURSOR    = 1;

      N_IO_DEVICES         = 1;

      IO_DEV_1_TYPE        = DEVICE_2D;
      IO_DEV_1_COORDINATES = ABSOLUTE;
      IO_DEV_1_PEN         = 1;
      IO_DEV_1_N_SWITCHES  = 3;

      IO_DEV_2_TYPE        = NO_DEVICE;
      IO_DEV_2_COORDINATES = NO_COORDINATES;
      IO_DEV_2_PEN         = 0;
      IO_DEV_2_N_SWITCHES  = 0;
   }

   return (SUCCESS);
}
