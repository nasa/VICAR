/*	JUP_ConfigDevice
 *
 *	Purpose:	To configure Jupiter J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_ConfigDevice ( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Config:	Configuration array
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"
#include "jupmouse.h"

FUNCTION JUP_ConfigDevice( Unit, Config )

int *Unit, *Config;

{
   int i, status;
   struct jggxattr ja;
   int oldbits, newbits;
   char *hw;
   JUP_MESSAGE mess;

   oldbits = 0;

   if (J12file != NULL)
   {
      j_get_attr(&ja);
      if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)
         oldbits = 4;
      else
         oldbits = 8;
      jfflush(J12file);
   }

/* Set up the appropriate video rate based on the default configuration */
/* only if it's not already set.					*/

   j_get_attr(&ja);		/* Get the actual number of video lines */

   if (Config[2] == VIDEO_640_480) {
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

   hw = vid_parm(VIDEO_FILE, "hw", JUP_GPUNO, JUP_VIDEO);

   if (Config[0] == FULL_COLOR)
   {
      EACH_IMP_HAS_DW = FALSE;	/* IMP and graphics must be tied in color */
      EACH_IMP_ZOOMS = FALSE;	/* since all image IMPs are tied in hardware */
      if (j_config(RGB24,1,4,0,hw) != NULL)		/* Try 28 bits */
      {
         N_IMPS = 4;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 4;
      }
      else if (j_config(RGB24,1,0,0,hw) != NULL)	/* Try 24 bits */
      {
         N_IMPS = 3;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else if (j_config(RGB12,1,4,0,hw) != NULL)	/* Try 16 bits */
      {
         N_IMPS = 4;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 4;
      }
      else if (j_config(RGB12,1,0,0,hw) != NULL)	/* Try 12 bits */
      {
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
      if (j_config(CLT8,1,4,0,hw) != NULL)		/* Try 12 bits */
      {
         N_IMPS = 2;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 2;
      }
      else if (j_config(CLT8,1,0,0,hw) != NULL)		/* Try 8 bits */
      {
         N_IMPS = 1;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else if (j_config(CLT4,1,4,0,hw) != NULL)		/* another 8 bits? */
      {
         N_IMPS = 1;
         OVERLAY_AVAILABLE = TRUE;
         OVERLAY_IMP = 2;
      }
      else if (j_config(CLT4,1,0,0,hw) != NULL)		/* Try 4 bits */
      {
         N_IMPS = 1;
         OVERLAY_AVAILABLE = FALSE;
         OVERLAY_IMP = 0;
      }
      else
         return(DEVICE_CANNOT_DO_IT);

      for (i=1; i<=3; i++)
         WHICH_IMP(i) = 1;		/* All luts are connected to imp 1 */
   }

   jfflush(J12file);

   j_get_attr(&ja);		/* Check to see if we need to reset the LUTs */
   if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)
      newbits = 4;
   else
      newbits = 8;

   if (oldbits != newbits)	/* # of bits has changed */
   {
      for (i=1; i<=N_LUTS; i++)
         zdlramp(*Unit, i, 1);	/* Ramp all the LUTs 'cuz they're different */
      if (OVERLAY_AVAILABLE)
         zdglinit(*Unit, 1);
   }

   if (OVERLAY_AVAILABLE && OVERLAY_ON)
      j_ormask(0x0F);		/* Set the display mask to all on for overlay */
   else {
      j_ormask(0x00);		/* Set the display mask to all off */
      OVERLAY_ON = FALSE;
   }

   JUP_NewConfig(Unit, ja.ja_fb);

   JUP_CursorOverlay(Unit);

   return SUCCESS;
}
