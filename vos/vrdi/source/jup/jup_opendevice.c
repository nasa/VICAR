/*	JUP_OpenDevice
 *
 *	Purpose:	Open Jupiter J-Station Display Device for use
 *			by Vicar and VRDI
 *
 *	Written by:	Fred Burnette
 *	Date:		September 25, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_OpenDevice( Unit )
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

#include "jupinc.h"

FUNCTION JUP_OpenDevice( Unit )

int *Unit;

{
   int status, i;
   struct jggxattr ja;
   int lut[256];
   char *nlines;
   double atof();

/* Extract video number and GPU number from subtype field.  Subtype is	*/
/* interpreted as a hex digit (0-9, A-F), with the low-order two bits	*/
/* being the video number and the high order being the GPU number.	*/
/* This ordering is chosen because there is a hard limit of 4 videos,	*/
/* but the limit of 4 GPUs is soft, so G-J would be GPU #5, etc.	*/
/* Examples: 0:vid0,gpu0... 1:vid1,gpu0... 4:vid0,gpu1... B:vid3,gpu2	*/

   i = DIB[*Unit]->SubType - '0';
   if (i > 9)			/* if in the letters */
      i = i + '0' - 'A';
   JUP_VIDEO = i % 4;
   JUP_GPUNO = i >> 2;

/* Extract mouse number from device name */

   JUP_MOUSE = DEV_NAME[3] - '0';		/* get 4th char in: JUP0 */

/* jup_inittables assumes cursor == GCSR (graphics).  If we have to use	*/
/* MCSR (multiple cursor), then change the DCB to reflect the different	*/
/* capabilities.  If you change this check, see also jup_mouse.c, as	*/
/* the same check is made there.					*/

   if (JUP_MOUSE != 0) {			/* Multiple cursor */
      N_CURSOR_TYPES = 2;
   }

   if (OUTPUT_MODE == FULL_COLOR)
   {
      EACH_IMP_HAS_DW = FALSE;	/* IMP and graphics must be tied in color */
      EACH_IMP_ZOOMS = FALSE;	/* since all image IMPs are tied in hardware */
      if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,RGB24,1,0,4)) != NULL)
      {							/* Try 28 bits */
         N_IMPS = 4;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 4;
      }
      else if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,RGB24,1,0,0)) != NULL)
      {							/* Try 24 */
         N_IMPS = 3;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,RGB12,1,0,4)) != NULL)
      {							/* Try 16 */
         N_IMPS = 4;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 4;
      }
      else if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,RGB12,1,0,0)) != NULL)
      {							/* Try 12 */
         N_IMPS = 3;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else
         return(DEVICE_CANNOT_DO_IT);

      for (i=1; i<=3; i++)
         WHICH_IMP(i) = i;		/* Lut 2 is connected to imp 2, etc. */
   }
   else					/* Monochrome or pseudocolor */
   {
      EACH_IMP_HAS_DW = TRUE;	/* IMP and graphics can be separate */
      EACH_IMP_ZOOMS = TRUE;	/* in monochrome mode */
      if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,CLT8,1,0,4)) != NULL)
      {							/* Try 12 bits */
         N_IMPS = 2;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 2;
      }
      else if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,CLT8,1,0,0)) != NULL)
      {							/* Try 8 */
         N_IMPS = 1;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,CLT4,1,0,4)) != NULL)
      {							/* 8? */
         N_IMPS = 2;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 2;
      }
      else if ((J12file = jginit(JUP_GPUNO,JUP_VIDEO,CLT4,1,0,0)) != NULL)
      {							/* Try 4 */
         N_IMPS = 1;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else
         return(DEVICE_CANNOT_DO_IT);

      for (i=1; i<=3; i++)
         WHICH_IMP(i) = 1;		/* All luts are connected to imp 1 */
   }

/* Set up the appropriate video rate based on the default configuration */
/* only if it's not already set.					*/

   j_get_attr(&ja);		/* Get the actual number of video lines */

   if (DIB[*Unit]->DefaultConfig[2] == VIDEO_640_480) {
      status = 0;
      if ((int)ja.ja_nline == 1024)	/* don't check 480 cuz really 484 */
         status = j_vid_rst(JUP_480_VIDEO_FILE, JUP_GPUNO, JUP_VIDEO, 1);
      VIDEO_LINES = 480;
      VIDEO_SAMPLES = 640;
      MAX_ZOOM_FACTOR = 5;
      if (status != 0)
         return(DEVICE_CANNOT_DO_IT);
   }
   else {
      status = 0;
      if ((int)ja.ja_nline != 1024)
         status = j_vid_rst(JUP_1024_VIDEO_FILE, JUP_GPUNO, JUP_VIDEO, 1);
      VIDEO_LINES = 1024;
      VIDEO_SAMPLES = 1280;
      MAX_ZOOM_FACTOR = 16;
      if (status != 0)
         return(DEVICE_CANNOT_DO_IT);
   }

   j_get_attr(&ja);		/* Get the actual number of video lines */
   JUP_VIDEO_LINES = (int)ja.ja_nline; /* cuz its really 484 not 480 for NTSC */

   j_crmask(0xFFFFFFFF);		/* Make sure display masks are set */
   j_ormask(0xFFFFFFFF);
   j_pat(0);				/* Set pattern to solid interior */

   jfflush(J12file);

   if (OVERLAY_AVAILABLE && OVERLAY_ON)
      j_ormask(0x0F);		/* Set the display mask to all on for overlay */
   else {
      j_ormask(0x00);		/* Set the display mask to all off */
      OVERLAY_ON = FALSE;
   }

   jfflush(J12file);

   for (i=0; i<256; i++)
      lut[i] = i;		/* Ramp all the LUTs 'cuz they're init'd to 0 */
   for (i=1; i<=N_LUTS; i++)
      JUP_Lut(Unit, WRITE_LUT, i, 1, lut);

   j_get_attr(&ja);		/* Get the screen mode */

/* Start up (or listen to) the mouse subprocess */

   if (OVERLAY_AVAILABLE)		/* ovbits == 4 */
      status = JUP_StartMouse(Unit, JUP_MOUSE,JUP_VIDEO, ja.ja_fb, 4,JUP_GPUNO);
   else					/* ovbits == 0 */
      status = JUP_StartMouse(Unit, JUP_MOUSE,JUP_VIDEO, ja.ja_fb, 0,JUP_GPUNO);

   jfflush(J12file);

   JUP_ZoomDW(Unit, ZOOM_IMP, 1, ZOOM(1), DW_LEFT(1), DW_TOP(1));
   if (OVERLAY_AVAILABLE)				/* Reset zoom/scroll */
      JUP_ZoomDW(Unit, ZOOM_IMP, OVERLAY_IMP, ZOOM(OVERLAY_IMP),
		DW_LEFT(OVERLAY_IMP), DW_TOP(OVERLAY_IMP));

   jfflush(J12file);

   JUP_NewConfig(Unit, ja.ja_fb);

   JUP_CursorOverlay(Unit);

   return (status);		/* from StartMouse */
}
