/*	ADAGE_InitTables - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_InitTables( parameters )
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

#define ADAGE_INITIALIZE
#include "ikoninc.h"

FUNCTION ADAGE_InitTables( Unit )
int	*Unit;

{
  AVAIL_CONFIGS = IMP_512_BIT | IMP_1024_BIT | IMP_2048_BIT | 
                   VIDEO_512_BIT | VIDEO_1024_BIT | VIDEO_640_480_BIT | 
                   ASPECT_1_1_BIT | ASPECT_4_3_BIT |
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

  N_CURSOR_TYPES       = 4;
  N_CURSOR_BLINK_RATES = 0;

  MAY_COLOR_CURSOR     = FALSE;
  MAY_RESIZE_CURSOR    = FALSE;
  /*  The numbers used here are dummy numbers.  If the resizing capability  */
  /*  is added, these should be replaced with the actual values.            */
  MAX_CURSOR_XSIZE     = 1;
  MAX_CURSOR_YSIZE     = 1;
 
  if ( AUTO_TRACK_AVAILABLE ) {
    AUTO_TRACK_DEVICE    = 1;
    AUTO_TRACK_CURSOR    = 1;

    N_IO_DEVICES         = 1;

    IO_DEV_1_TYPE	= DEVICE_2D;
    IO_DEV_1_COORDINATES = ABSOLUTE;
    IO_DEV_1_PEN         = 1;
    IO_DEV_1_N_SWITCHES  = 1;
  }
  return (SUCCESS);
}
