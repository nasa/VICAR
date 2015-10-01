/*	IP85LO_InitTables - description
 *
 *	Purpose: Initialize VRDI system tables for the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_InitTables(Unit, lun)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number (not used)
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

#define IP85LO_INITIALIZE
#include "ip85lo.h"

FUNCTION IP85LO_InitTables(Unit, lun)
int *Unit;
short lun;
{
if (DEV_TYPE == DEANZA_IP85LO)
   AVAIL_CONFIGS = IMP_512_BIT | VIDEO_512_BIT | ASPECT_1_1_BIT |
		FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;

if (DEV_TYPE == DEANZA_IP85LX)
   AVAIL_CONFIGS = IMP_512_BIT | IMP_1024_BIT |
		VIDEO_512_BIT | ASPECT_1_1_BIT |
		FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;

if (DEV_TYPE == DEANZA_IP9000)
   AVAIL_CONFIGS = IMP_512_BIT | IMP_1024_BIT | IMP_2048_BIT |
		VIDEO_512_BIT | VIDEO_1024_BIT | ASPECT_1_1_BIT |
		FULL_COLOR_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;

DEV_UNIT_NO = 0;

EACH_IMP_HAS_DW = TRUE;

N_LUT_SECTIONS      = 4;
MAY_BYPASS_LUT      = TRUE;
MAX_LUT_VALUE       = 255;
EACH_IMP_ZOOMS      = TRUE;
MAY_CONNECT_IMP_LUT = TRUE;
MAY_ZOOM_IMPS       = TRUE;
MAX_ZOOM_FACTOR     = 8;
if (DEV_TYPE == DEANZA_IP9000)
   MAX_ZOOM_FACTOR  = 16;

MAY_CONNECT_IMP_OVERLAY = TRUE;
MAY_BYPASS_OVERLAY_LUT  = FALSE;
OVERLAY_IMP             = 4;
OVERLAY_LUT_CHAR        = SEPERATE_LUT;
N_OVERLAY_SECTIONS      = 1;
BITS_IN_OVERLAY         = 8;

AFG_AVAILABLE         = FALSE;
N_AFG_LINES           = 0;
N_AFG_CHARACTERS      = 0;
N_AFG_CHARACTER_TYPES = 0;

N_CURSOR_TYPES       = 8;
N_CURSOR_BLINK_RATES = 16;

MAY_COLOR_CURSOR     = TRUE;
MAY_RESIZE_CURSOR    = FALSE;
/*  The numbers used here are dummy numbers.  If the resizing capability  */
/*  is added, these should be replaced with the actual values.            */
MAX_CURSOR_XSIZE     = 1;
MAX_CURSOR_YSIZE     = 1;

if ( AUTO_TRACK_AVAILABLE ) {
  AUTO_TRACK_DEVICE    = 1;
  AUTO_TRACK_CURSOR    = 1;

  N_IO_DEVICES         = 1;

  IO_DEV_1_TYPE        = DEVICE_2D;
  IO_DEV_1_COORDINATES = ABSOLUTE;
  IO_DEV_1_PEN         = 1;
  IO_DEV_1_N_SWITCHES  = 6;

  IO_DEV_2_TYPE        = NO_DEVICE;
  IO_DEV_2_COORDINATES = NO_COORDINATES;
  IO_DEV_2_PEN         = 0;
  IO_DEV_2_N_SWITCHES  = 0;
}

return (SUCCESS);
}
