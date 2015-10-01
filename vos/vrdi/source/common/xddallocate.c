/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/
/*									*/
/* The VRDI version is named ".c", and the TAE version is named ".cnp",	*/
/* as per their respective naming standards.	                        */
/*									*/
/* The symbol VRDI must be defined for the VRDI version, and TAE must   */
/* be defined for the TAE version.                                      */

#ifndef TAE
#include "xvmaininc.h"
#endif

/*	xddallocate - Tries to allocate the named device.  Valid device 
 *	names are standard VMS device names, standard VMS generic device 
 *	names, or DEFAULT.  VMS device and generic names may or may not
 *	include the trailing colon (:).  If the device name is DEFAULT,
 *	the device name associated with the user's terminal in the data
 *	file is used.
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 4, 1987
 *
 *	Calling Sequence:
 *
 *	        STATUS = xddallocate( Name )
 *
 *	Parameter List:
 *
 *	        Name:	Display device name
 *
 *	Possible Error Codes:
 *
 */

#ifdef VRDI
#include "ftnbridge.h"
#endif
#include "xdexterns.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdalloc.h"

#ifdef	VRDI
#include "xdroutines.h"
#endif  /* VRDI */

#ifdef VRDI
FUNCTION FTN_NAME(xddallocate)( char *Name, ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Name, &Name, 1, 1, 1, Name);
   return ( zddallocate( CString ) );
   }
#endif /* VRDI */

FUNCTION zddallocate( Name )
char	*Name;
   {
   int status, AllocError;
   int unit;
   char Device[10];
   int size;

#ifdef	VRDI
   xd_current_call = DALLOCATE;
#endif /* VRDI */

   status = XD_Read_DIBs();
   if ( status == SUCCESS) {
      status = GetUserVAX( &UserVAX );
      if ( status == SUCCESS) {
         if ( Name[strlen(Name)-1] == ':' ) {  /* remove trailing ':'     */
            Name[strlen(Name)-1] = '\000';
            }
         if (MATCH( Name, "DEFAULT", 3 )) {    /* check for DEFAULT       */
            status = FindDefault( Device );    /* find DEFAULT device     */
            if ( status == SUCCESS) {
               status = zddallocate( Device );
               }
            }
         else if (strlen(Name) < 4) {	       /* check 4 generic name    */
            status = FindGeneric( Name, Device );  /* find generic device */
            if ( status == SUCCESS) {
               status = zddallocate( Device );
               }
            }

         /*  Try to find the specified device name in the device list.    */

         else {
            for ( DeviceNumber = 0; DeviceNumber < TotalDevices;
                  DeviceNumber++ ) {
               if ((MATCH( Name, DIB[DeviceNumber]->DeviceName, 4 )) &&
                   ((DIB[DeviceNumber]->SystemNumber == UserVAX) ||
                    (DIB[DeviceNumber]->SystemNumber == -1)))
                  break;                       /*  Must be this VAX       */
               }

         /*  Check to see if device was found.  */

         if (DeviceNumber == TotalDevices ) {
            status = NO_SUCH_DEVICE;
            }

         /*  Check to see if device is available.  */

         else if ( !DIB[DeviceNumber]->Available ) {
            status = DEVICE_NOT_AVAIL;
            }
         else {

            /*  Call the system interface to allocate a device	*/

            unit = DeviceNumber;

#ifdef VRDI
            /*  This size calculation is also used twice in XDDOPEN    */
	    /*  and twice in x11_device.c.  If you change the size     */
	    /*  calc, don't forget to change all five places.          */

            size = sizeof(struct DCB_STRUCTURE) + 	  /* DCB */
               4 * sizeof(int) * DIB[unit]->nImps +	  /* access window */
               2 * sizeof(int) * DIB[unit]->nImps +	  /* display window */
               1 * sizeof(int) * DIB[unit]->nImps +	  /* zoom factor */
               1 * sizeof(int) * DIB[unit]->nLuts +	  /* imp_to_lut */
               1 * sizeof(int) * DIB[unit]->nLuts +	  /* lut section */
               2 * sizeof(int) * DIB[unit]->nCursors +	  /* cursor position */
               1 * sizeof(int) * DIB[unit]->nCursors +	  /* cursor form */
               1 * sizeof(int) * DIB[unit]->nCursors +	  /* cursor blink */
               2 * sizeof(int) * DIB[unit]->nCursors +	  /* cursor size */
               3 * sizeof(int) * DIB[unit]->nCursors +	  /* cursor color */
               1 * sizeof(int) * DIB[unit]->nCursors +	  /* cursor active */
               1 * sizeof(int) * DIB[unit]->nLuts +	  /* lut_bypass */
               1 * sizeof(int) *(DIB[unit]->nLuts+1) +	  /* lut_flag */
               1 * sizeof(int) *(DIB[unit]->nImps+1); 	  /* image_plane_flag */

            status = Alloc_Shmem(unit, size, SHMEM_DCB);

            /*  The shared memory allocation routine returns two status  */
            /*  codes that indicate success.  The first, SUCCESS, is     */
            /*  returned if the shared memory has been successfully      */
            /*  created.  The second, SHMEM_ALREADY_ACTIVE, is returned  */
            /*  if the shared memory has been previously created.  In    */
            /*  that case, the DCB shared memory block is mapped to the  */
            /*  existing shared memory block.                            */

            if (status == SHMEM_ALREADY_ACTIVE)
              status = SUCCESS;
#else
            status = SUCCESS;
#endif /* VRDI */

            if (status == SUCCESS)
               status = XD_Allocate_Device(unit, Name);
            }
         }
      }	
   }
#ifdef	VRDI
   xd_error_handler( &NoUnit, status );
#endif /* VRDI */

   return(status);
   }
