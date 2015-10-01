/*	xddopen         - Makes the specified unit available for use.
 *	                  Initializes the DCB every time called.
 *	xddsmartopen    - Makes the specified unit available for use.
 *	                  Initializes the DCB only if it hasn't been already.
 *
 *
 *	Purpose:
 *
 *	Written by:	R. Mortensen
 *	Date:		September 16, 1986
 *
 *	Calling Sequence:
 *
 *	        STATUS = xddopen( Unit )
 *	        STATUS = xddsmartopen( Unit )
 *
 *	Parameter List:
 *
 *	        Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif
#if DEBUG
#define DPR(A) printf A
#else /* DEBUG */
#define DPR(A)
#endif /* DEBUG */

FUNCTION FTN_NAME(xddopen)( Unit )
INTEGER	Unit;
   {
   return(zddopen_func( *Unit, 0 ));
   }

FUNCTION FTN_NAME(xddsmartOpen)( Unit )
INTEGER	Unit;
   {
   return(zddopen_func( *Unit, 1 ));
   }

FUNCTION zddopen( unit )
int	unit;
   {
   return(zddopen_func( unit, 0 ));
   }

FUNCTION zddsmartopen( unit )
int	unit;
   {
   return(zddopen_func( unit, 1 ));
   }

FUNCTION zddopen_func( unit, request_no_init )
int	unit, request_no_init;
   {
   int	initialize, status, size, imp, lut, cursor;
   int	one=1, zero=0;
   char *ptr;

   status = SUCCESS;
   xd_current_call = DOPEN;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if ( !ZDEVICE_ALLOCATED ) {
      status = DEVICE_NOT_ALLOC;
      }
   else if (ZCHECK_DEVICE_OPEN) {
      status = DEVICE_ALREADY_OPEN;
      }
   else {

      /* This size calculation is also used in XDDALLOCATE, twice in  */
      /* x11_device.c and below when setting up the pointers.  If you */
      /* change the size calc, don't forget to change all five places.*/
      size = sizeof(struct DCB_STRUCTURE) +		  /* DCB */
             4 * sizeof(int) * DIB[unit]->nImps +	  /* access window */
             2 * sizeof(int) * DIB[unit]->nImps +	  /* display window */
             1 * sizeof(int) * DIB[unit]->nImps +	  /* zoom factor */
             1 * sizeof(int) * DIB[unit]->nLuts +	  /* imp_to_lut */
             1 * sizeof(int) * DIB[unit]->nLuts +	  /* lut section */
             2 * sizeof(int) * DIB[unit]->nCursors +      /* cursor position */
             1 * sizeof(int) * DIB[unit]->nCursors +      /* cursor form */
             1 * sizeof(int) * DIB[unit]->nCursors +      /* cursor blink */
             2 * sizeof(int) * DIB[unit]->nCursors +      /* cursor size */
             3 * sizeof(int) * DIB[unit]->nCursors +      /* cursor color */
             1 * sizeof(int) * DIB[unit]->nCursors +      /* cursor active */
             1 * sizeof(int) * DIB[unit]->nLuts +	  /* lut_bypass */
             1 * sizeof(int) *(DIB[unit]->nLuts + 1) +    /* lut_flag */
             1 * sizeof(int) *(DIB[unit]->nImps + 1);     /* image_plane_flag */

      DPR(("open: call ATTACH_SHMEM\n"));
      status = Attach_Shmem(unit, size, &DCB[unit], SHMEM_DCB);
      DPR(("open: called ATTACH_SHMEM %d\n",status));

      /*  The shared memory allocation routine returns two status  */
      /*  codes that indicate success.  The first, SUCCESS, is     */
      /*  returned if the shared memory has been successfully      */
      /*  created.  The second, SHMEM_ALREADY_ACTIVE, is returned  */
      /*  if the shared memory has been previously created.  In    */
      /*  that case, the DCB shared memory block is mapped to the  */
      /*  existing shared memory block.                            */

      if (status == SHMEM_ALREADY_ACTIVE)
        status = SUCCESS;

      if (status != SUCCESS) {
         zdesignal( status );
         ZSHARED_MEMORY_ACTIVE = FALSE;
         DPR(("open: attempt malloc\n"));
         DCB[unit] = (struct DCB_STRUCTURE *) malloc(size);
         DPR(("open: attempted malloc %d\n",DCB[unit]));
         if (DCB[unit] == 0) {
            status = MEMORY_ERROR;
            }
         else {
            status = SUCCESS;
            }
         }
      else {
         ZSHARED_MEMORY_ACTIVE = TRUE;
         }

      if (status == SUCCESS) {
         DPR(("open: start\n"));
         if (ZSHARED_MEMORY_ACTIVE && ZDCB_INITIALIZED && request_no_init) {
            DPR(("open: do not initialize\n"));
            initialize = 0;
            }
         else {
            DPR(("open: do initialize\n"));
            initialize = 1;
            }

            ZDCB_INITIALIZED = TRUE;

         if (initialize) {
            DPR(("open: init dcb\n"));
            strcpy(ZDEV_NAME, DIB[unit]->DeviceName);
            ZDEV_TYPE   = DIB[unit]->DeviceType;
            ZDEV_ACTIVE = FALSE;
            ZBATCH_MODE = FALSE;

            ZN_LUTS    = DIB[unit]->nLuts;
            ZN_IMPS    = DIB[unit]->nImps;
            ZN_LINES   = DIB[unit]->nLines;
            ZN_SAMPS   = DIB[unit]->nSamps;
            ZN_CURSORS = DIB[unit]->nCursors;

            ZOUTPUT_MODE  = DIB[unit]->DefaultConfig[0];
            ZIMP_SIZE     = DIB[unit]->DefaultConfig[1];
            ZVIDEO_SIZE   = DIB[unit]->DefaultConfig[2];
            ZASPECT_RATIO = DIB[unit]->DefaultConfig[3];

            if (ZDEV_TYPE != X_WINDOW) {
               switch (ZVIDEO_SIZE) {
                  case VIDEO_512:
                     ZVIDEO_LINES   = 512;
                     ZVIDEO_SAMPLES = 512;
                     break;

                  case VIDEO_1024:
                     ZVIDEO_LINES   = 1024;
                     ZVIDEO_SAMPLES = 1024;
                     break;

                  case VIDEO_640_480:
                     ZVIDEO_LINES   = 480;
                     ZVIDEO_SAMPLES = 640;
                     break;

                  case VIDEO_640_512:
                     ZVIDEO_LINES   = 512;
                     ZVIDEO_SAMPLES = 640;
                     break;

                  case VIDEO_1024_512:
                     ZVIDEO_LINES   = 512;
                     ZVIDEO_SAMPLES = 1024;
                     break;

                  case VIDEO_1280_1024:
                     ZVIDEO_LINES   = 1024;
                     ZVIDEO_SAMPLES = 1280;
                     break;
               }
            }

            ZOVERLAY_AVAILABLE   = DIB[unit]->Overlay;
            ZOVERLAY_ON          = FALSE;
            ZOVERLAY_LUT_SECTION = 1;
            ZDEFAULT_OVERLAY_LUT = FALSE;

            ZAFG_AVAILABLE     = DIB[unit]->AlphaNumerics;
            ZAFG_ACTIVE        = FALSE;

            ZDEV_HAS_PROCESSOR = DIB[unit]->Processor;

            ZAUTO_TRACK_AVAILABLE = DIB[unit]->AutoTracking;
            }

         DPR(("open: init tables\n"));
         status = XD_Device_Interface( &unit, INITIALIZE_TABLES );

         DPR(("open: alloc space for 2nd structures\n"));

         /*  These arrays are all one right after the other in the  */
         /*  shared memory, so add the length of the previous one   */
         /*  to its address to get the address of the new one.      */

         ptr = (char *)DCB[unit] + sizeof(struct DCB_STRUCTURE);

         access_window[unit] = (int (*)[]) ptr;
         ptr += 4 * sizeof(int) * DIB[unit]->nImps;

         display_window[unit] = (int (*)[]) ptr;
         ptr += 2 * sizeof(int) * DIB[unit]->nImps;

         zoom_factor[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nImps;

         imp_to_lut[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nLuts;

         lut_section[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nLuts;

         cursor_position[unit] = (int (*)[]) ptr;
         ptr += 2 * sizeof(int) * DIB[unit]->nCursors;

         cursor_form[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nCursors;

         cursor_blink[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nCursors;

         cursor_size[unit] = (int (*)[]) ptr;
         ptr += 2 * sizeof(int) * DIB[unit]->nCursors;

         cursor_color[unit] = (int (*)[]) ptr;
         ptr += 3 * sizeof(int) * DIB[unit]->nCursors;

         cursor_active[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nCursors;

         lut_bypass[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * DIB[unit]->nLuts;

         lut_flag[unit] = (int *) ptr;
         ptr += 1 * sizeof(int) * (DIB[unit]->nLuts + 1);

         image_plane_flag[unit] = (int *) ptr;

         DPR(("open: set up values for 2nd structures\n"));
         for ( imp = 1; imp <= ZN_IMPS; imp++ ) {
            if (initialize) { 
               ZAW_TOP(imp)    = 1;
               ZAW_LEFT(imp)   = 1;
               ZAW_BOTTOM(imp) = ZN_LINES;
               ZAW_RIGHT(imp)  = ZN_SAMPS;
               ZDW_TOP(imp)    = 1;
               ZDW_LEFT(imp)   = 1;
               ZZOOM(imp)      = 1;
               }
            }

         CLEAR_FLAG( ZCONFIG_FLAG );     /* Clear flag for default group */
         SET_FLAG( ZCONFIG_FLAG );       /* Set flag for all other groups */
         CLEAR_FLAG( ZGLUT_FLAG );
         for (lut = 0; lut <= ZN_LUTS; lut++)
            CLEAR_FLAG( ZLUT_FLAG( lut ) );
         for (imp = 0; imp <= ZN_IMPS; imp++)
            CLEAR_FLAG( ZIMP_FLAG( imp ) );

         for (lut=1; lut <= ZN_LUTS; lut++) {
            if (ZWHICH_SECTION(lut) != 1) {
               SET_FLAG( ZLUT_FLAG(lut) );
               SET_FLAG( ZLUT_FLAG(0) );
            }
         }

         DPR(("open: set which imps...\n"));
         for (lut=1, imp=1; lut <= ZN_LUTS; lut++) {
      	    if (initialize) { 
               ZWHICH_IMP(lut)     = imp;
               ZWHICH_SECTION(lut) = 1;
               }
            if (ZOUTPUT_MODE == FULL_COLOR) imp++;
            }

         ZVALID_MODE = TRUE;

         DPR(("open: call OPEN_DEVICE\n"));
         status = XD_Device_Interface( &unit, OPEN_DEVICE );
         DPR(("open: called OPEN_DEVICE stat %d\n",status));

         if ((status == SUCCESS) && ZMAY_CONNECT_IMP_LUT && initialize) {
            DPR(("open: CONNECT_IMPS_LUTS\n"));
            for (lut=1,imp=1;(lut <= ZN_LUTS) && (status==SUCCESS);lut++){
               status = XD_Device_Interface( &unit, CONNECT_IMPS_LUTS, &imp, 
                                             &lut, &one, &zero );
               if (ZOUTPUT_MODE == FULL_COLOR) imp++;
               }
            }

         if ((status == SUCCESS) && ZAFG_AVAILABLE && initialize) {
            DPR(("open: AFG_OFF\n"));
            status = XD_Device_Interface( &unit, AFG_OFF );
            }

         if ((status == SUCCESS) && ZOVERLAY_AVAILABLE && initialize) {
            DPR(("open: GRAPHICS_OFF\n"));
            status = XD_Device_Interface( &unit, GRAPHICS_OFF );
            }

         if ((status == SUCCESS) && (initialize)) {
            DPR(("open: set up cursors\n"));
            for (cursor = 1; (status == SUCCESS) && (cursor <= ZN_CURSORS);
                                                                  cursor++) {
               ZCURSOR_X(cursor) = 1;
               ZCURSOR_Y(cursor) = 1;
               ZCURSOR_FORM(cursor) = 1;
               ZCURSOR_BLINK(cursor) = 0;
               ZCURSOR_RED(cursor) = 255;
               ZCURSOR_GREEN(cursor) = 255;
               ZCURSOR_BLUE(cursor) = 255;
               ZCURSOR_XSIZE(cursor) = 21;
               ZCURSOR_YSIZE(cursor) = 21;

               if (cursor == 1) {
                  ZCURSOR_ACTIVE(cursor) = TRUE;
                  status = XD_Device_Interface( &unit, CURSOR_ON, cursor,
	                                        ZCURSOR_FORM(cursor),
                                                ZCURSOR_BLINK(cursor) );
                  }
               else {
                  ZCURSOR_ACTIVE(cursor) = FALSE;
                  status = XD_Device_Interface( &unit, CURSOR_OFF, cursor );
                  }

               if (status == SUCCESS)
                  status = XD_Device_Interface( &unit, WRITE_CURSOR,
                                                cursor, 1, 1 );
               }
            }

         if ((status == SUCCESS) && ZAUTO_TRACK_AVAILABLE && initialize) {
            DPR(("open: AUTO_ON\n"));
            status = XD_Device_Interface( &unit, AUTO_ON, ZAUTO_TRACK_DEVICE,
                                          ZAUTO_TRACK_CURSOR );
            }
         }
      }

   DPR(("open: return with status %d\n",status));
   xd_error_handler( &unit, status );
   return (status);
   }
