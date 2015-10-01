/*	RAMTEK_InitTables - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_InitTables( parameters )
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

#define RAMTEK_INITIALIZE

#include "rtekinc.h"

FUNCTION RAMTEK_InitTables( Unit )
int	*Unit;

{
  AVAIL_CONFIGS = IMP_512_BIT | IMP_640_480_BIT | IMP_640_512_BIT |
                  VIDEO_640_480_BIT | 
   		  ASPECT_1_1_BIT | PSEUDO_COLOR_BIT | BLACK_AND_WHITE_BIT;
  DEV_UNIT_NO = 0;

  EACH_IMP_HAS_DW = FALSE;

  N_LUT_SECTIONS      = 4;
  MAY_BYPASS_LUT      = FALSE;
  MAX_LUT_VALUE       = 255;
  MAY_CONNECT_IMP_LUT = TRUE;

  MAY_ZOOM_IMPS       = TRUE;
  MAX_ZOOM_FACTOR     = 16;
  EACH_IMP_ZOOMS      = FALSE;

  N_CURSOR_TYPES       = 2;
  N_CURSOR_BLINK_RATES = 1;

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
    IO_DEV_1_N_SWITCHES  = 0;
  }
  else {  

    AUTO_TRACK_DEVICE    = 0;
    AUTO_TRACK_CURSOR    = 0;

    N_IO_DEVICES         = 0;
  }
  if ( AFG_AVAILABLE ) {

      AVAIL_CONFIGS = 0;
      N_AFG_LINES = 24;
      N_AFG_CHARACTERS = 80;
      N_AFG_CHARACTER_TYPES = 128;
  }
  return (SUCCESS);
}
