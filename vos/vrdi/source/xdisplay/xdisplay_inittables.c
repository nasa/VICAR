/* %Z%  %M% %I% %G% %Y% */
/*   X_InitTables

     Purpose: Initialize VRDI system tables for X Windows Version 10 or 11

     Written by: Mark Mann
     Date:       Sep 23, 1988

     Parameter List:
	     Unit:    Display device unit number
	     

*/

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#define X_INITIALIZE
#include "x11_device_main.h"

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

FUNCTION x_inittables(Unit)
     int *Unit;
{
     int unit;

     unit = *Unit;
     AVAIL_CONFIGS = 0;
     
     switch (OUTPUT_MODE)
     {
     case FULL_COLOR:
	  AVAIL_CONFIGS |= FULL_COLOR_BIT | PSEUDO_COLOR_BIT |
                           BLACK_AND_WHITE_BIT;
	  break;
     case PSEUDO_COLOR:
	  AVAIL_CONFIGS |= PSEUDO_COLOR_BIT;
	  break;
     case BLACK_AND_WHITE:
	  AVAIL_CONFIGS |= BLACK_AND_WHITE_BIT;
	  break;
     }
     switch (IMP_SIZE)
     {
     case IMP_512:
	  AVAIL_CONFIGS |= IMP_512_BIT;
	  break;
     case IMP_1024:
	  AVAIL_CONFIGS |= IMP_1024_BIT;
	  break;
     case IMP_2048:
	  AVAIL_CONFIGS |= IMP_2048_BIT;
	  break;
     case IMP_4096:
	  AVAIL_CONFIGS |= IMP_4096_BIT;
	  break;
     }
     switch (VIDEO_SIZE)
     {
     case VIDEO_512:
	  AVAIL_CONFIGS |= VIDEO_512_BIT;
	  break;
     case VIDEO_1024:
	  AVAIL_CONFIGS |= VIDEO_1024_BIT;
	  break;
     case VIDEO_640_480:
	  AVAIL_CONFIGS |= VIDEO_640_480_BIT;
	  break;
     }
     switch (ASPECT_RATIO)
     {
     case ASPECT_1_1:
	  AVAIL_CONFIGS |= ASPECT_1_1_BIT;
	  break;
     case ASPECT_4_3:
	  AVAIL_CONFIGS |= ASPECT_4_3_BIT;
	  break;
     }
     DEV_UNIT_NO = 0;
     
     
     EACH_IMP_HAS_DW = TRUE;
     
     N_LUT_SECTIONS      = 1;
     MAY_BYPASS_LUT      = TRUE;
     MAX_LUT_VALUE       = X_LUT_SIZE-1;
     EACH_IMP_ZOOMS      = TRUE;
     MAY_CONNECT_IMP_LUT = TRUE;
     MAY_ZOOM_IMPS       = TRUE;
     MAX_ZOOM_FACTOR     = 16;
     
     MAY_CONNECT_IMP_OVERLAY = TRUE;
     MAY_BYPASS_OVERLAY_LUT  = TRUE;
     OVERLAY_LUT_CHAR	     = SEPERATE_LUT;
     N_OVERLAY_SECTIONS      = 1;
     switch (X_Image_Depth) {
        case 24:
           BITS_IN_OVERLAY   = 8;
           X_Overlay_Size  = 256;
           OVERLAY_IMP	     = 4;
           X_Overlay_Imp     = 4;
           OUTPUT_MODE = FULL_COLOR;
           break;
        case 8:
           BITS_IN_OVERLAY   = 4;
           X_Overlay_Size   = 16;
           OVERLAY_IMP	     = 2;
           X_Overlay_Imp     = 2;
           OUTPUT_MODE = BLACK_AND_WHITE;
           break;
        default:
           BITS_IN_OVERLAY   = 8;
           X_Overlay_Size  = 256;
           OVERLAY_IMP	     = 4;
           X_Overlay_Imp     = 4;
           OUTPUT_MODE = FULL_COLOR;
           break;
     }

     DPR(("x_inittables:  image depth=%d, overlay size=%d\n", X_Image_Depth,
          X_Overlay_Size));
     X_Overlay_On        = FALSE;
     AFG_AVAILABLE	 = FALSE;
     N_AFG_LINES	   = 0;
     N_AFG_CHARACTERS      = 0;
     N_AFG_CHARACTER_TYPES = 0;
     
     N_CURSOR_TYPES       = 6;
     N_CURSOR_BLINK_RATES = 0;
     AUTO_TRACK_AVAILABLE = TRUE;
     AUTO_TRACK_DEVICE    = 1;
     AUTO_TRACK_CURSOR    = 1;
     
     N_IO_DEVICES	 = 1;
     
     IO_DEV_1_TYPE	= DEVICE_2D;
     IO_DEV_1_COORDINATES = ABSOLUTE;
     IO_DEV_1_PEN	 = TRUE;
     IO_DEV_1_N_SWITCHES  = 3;
     
     IO_DEV_2_TYPE	= NO_DEVICE;
     IO_DEV_2_COORDINATES = NO_COORDINATES;
     IO_DEV_2_PEN	 = 0;
     IO_DEV_2_N_SWITCHES  = 0;
     
     return (SUCCESS);
}
