/*	xddconfigure - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xddconfigure( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xddconfigure)( Unit, Config )
INTEGER	Unit, Config;
   {
   return ( zddconfigure( *Unit, Config ) );
   }

FUNCTION zddconfigure( unit, config )
int	unit, *config;
   {
   int	status, i, lut, tmpconfig[4];

   xd_current_call = DCONFIGURE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if ((config[0] < -1) || (config[0] > 3)  ||
            (config[1] < -1) || (config[1] > 8)  ||
            (config[2] < -1) || (config[2] > 6)  ||
            (config[3] < -1) || (config[3] > 2)) {
      status = INVALID_DEVICE_REQ;
      }
   else {
      /*  Set flag to indicate that output mode has been set  */
      ZVALID_MODE = TRUE;

      for ( i=0; i<4; i++ ) {
         if (config[i] == 0) {            /*  Use default configuration  */
            tmpconfig[i] = DIB[unit]->DefaultConfig[i];
            }
         else if (config[i] == -1) {      /*  Leave configuration unchanged  */
            if (i == 0)		tmpconfig[i] = ZOUTPUT_MODE;
            else if (i == 1)	tmpconfig[i] = ZIMP_SIZE;
            else if (i == 2)	tmpconfig[i] = ZVIDEO_SIZE;
            else if (i == 3)	tmpconfig[i] = ZASPECT_RATIO;
            }
         else {                           /*  Change configuration  */
            tmpconfig[i] = config[i];
            }
         }

      if ( !BIT_TEST(ZAVAIL_CONFIGS,tmpconfig[0]+19) ||
           !BIT_TEST(ZAVAIL_CONFIGS,tmpconfig[1]-1) ||
           !BIT_TEST(ZAVAIL_CONFIGS,tmpconfig[2]+7) ||
           !BIT_TEST(ZAVAIL_CONFIGS,tmpconfig[3]+15)) {
         status = INVALID_DEVICE_REQ;
         }
      else {
         status = XD_Device_Interface( &unit, CONFIG_DEVICE, tmpconfig );

         if ((ZIMP_SIZE != tmpconfig[1]) || (ZVIDEO_SIZE != tmpconfig[2]) ||
                                            (ZASPECT_RATIO != tmpconfig[3]) ||
                                            (ZOUTPUT_MODE != tmpconfig[0])) {
            SET_FLAG(ZCONFIG_FLAG);
            }

         if (ZOUTPUT_MODE != tmpconfig[0]) {
            for (lut=0; lut <= ZN_LUTS; lut++)
               SET_FLAG(ZLUT_FLAG( lut ));
            }

         if (status == SUCCESS) {
            ZOUTPUT_MODE  = tmpconfig[0];
            ZIMP_SIZE     = tmpconfig[1];
            ZVIDEO_SIZE   = tmpconfig[2];
            ZASPECT_RATIO = tmpconfig[3];
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
