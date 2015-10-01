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

/*	xddfree - Deallocates the named device.  As with xddallocate, the
 *	name may be a standard VMS device name, a standard VMS generic 
 *	name, or DEFAULT.
 *
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 6, 1987
 *
 *	Calling Sequence:
 *
 *	        STATUS = xddfree( Name )
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

#ifdef VRDI
#include "xdroutines.h"
#endif /* VRDI */

#ifdef VRDI
FUNCTION FTN_NAME(xddfree)( char *Name, ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Name, &Name, 1, 1, 1, Name);
   return( zddfree( CString ) );
   }
#endif /* VRDI */

FUNCTION zddfree( Name )
char	*Name;
   {
   int 	status;
   char	Device[10];
   int	unit;

#ifdef VRDI
   xd_current_call = DFREE;
#endif /* VRDI */

   status = SUCCESS;

#ifdef VRDI
   if ( !xd_initialized ) {
      status = XD_Read_DIBs();
      }
#endif /* VRDI */

#ifdef TAE
   status = XD_Read_DIBs();
#endif /* TAE */

   if ( status == SUCCESS ) {
      status = GetUserVAX ( &UserVAX );
      if ( status == SUCCESS ) {
         if ( Name[strlen(Name)-1] == ':' ) {  /* Remove the trailing ':'   */
            Name[strlen(Name)-1] = '\000';
            }

         if (MATCH(Name,"DEFAULT", 3 )) {      /* check for DEFAULT         */
            status = FindDefault( Device );    /* find DEFAULT device       */
            if (status == SUCCESS ) {
               status = zddfree( Device );     /* deallocate DEFAULT device */
               }
            }

         else if (strlen(Name) < 4) {	       /* check for generic name    */
            status = FindOwnedGeneric( Name, Device );
            if (status == SUCCESS ) {          /* find device user owns     */
               status = zddfree (Device );     /* deallocate generic device */
               }
            }

         /*  Try to find the device name in the list of devices.  */

         else {
            for ( DeviceNumber = 0; DeviceNumber < TotalDevices; DeviceNumber++ ) {
               if ( MATCH(Name,DIB[DeviceNumber]->DeviceName,4) &&
                  ((DIB[DeviceNumber]->SystemNumber == UserVAX) ||
                   (DIB[DeviceNumber]->SystemNumber == -1)) )
                  break;
               /* Must be on this VAX  */
               }

            /*  Check to see if device was found.  */
            if ( DeviceNumber == TotalDevices ) {
               status = NO_SUCH_DEVICE;
               }
            else {

               /*  Now actually free the device.  */

               unit = DeviceNumber;
               status = XD_Free_Device(unit, Name);

#ifdef VRDI
               if (status == SUCCESS) {
                  status = Remove_Shmem(unit, SHMEM_DCB);
                  }
#endif
               }
            }
         }
      }

#ifdef VRDI
   xd_error_handler( &NoUnit, status );
#endif /* VRDI */

   return ( status );
   }

