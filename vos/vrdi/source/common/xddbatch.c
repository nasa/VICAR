/*	xddbatch - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        September 24, 1992
 *
 *	Calling Sequence:
 *
 *		STATUS = xddbatch( parameters )
 *
 *	Parameter List:
 *
 *		unit:	Display device unit number
 *              flag:   True or False
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

static FUNCTION set_batch();

FUNCTION FTN_NAME(xddbatch)( Unit, Flag )
INTEGER Unit;
LOGICAL Flag;
   {
   return ( zddbatch ( *Unit, *Flag ));
   }

/* batch_off() is called ONLY by the error handler.  It works exactly	*/
/* like zddbatch() except that the error handler is not called (again)	*/
/* if an error occurs.							*/

FUNCTION batch_off(unit, flag)
int unit, flag;
   {
   return set_batch(unit, flag, FALSE);	/* don't call error handler */
   }

FUNCTION zddbatch( unit, flag )
int unit, flag;
   {
   return set_batch(unit, flag, TRUE);	/* do call error handler */
   }

static FUNCTION set_batch( unit, flag, error )
int unit, flag, error;
   {
   int	status;

   if (error)
      xd_current_call = DBATCH;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else {
      ZBATCH_MODE = MAKE_LOGICAL( flag );
      status = XD_Device_Interface( &unit, SET_BATCH_MODE, flag );
      }

   if (error)
      xd_error_handler( &unit, status );
   return (status);
   }

/* zd_batch() is a VRDI internal function to be called only by the other   */
/* VRDI routines.  It is currently used by the zdttext() routine to batch  */
/* the vector operations used to create text on those devices that do not  */
/* support text writing directly.                                          */

FUNCTION zd_batch( unit, flag )
int unit, flag;
{
   int status;

   ZBATCH_MODE = MAKE_LOGICAL( flag );
   status = XD_Device_Interface( &unit, SET_BATCH_MODE, flag );
   return (status);
}
