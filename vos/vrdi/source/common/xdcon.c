/*	xdcon - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcon( parameters )
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

#define	DEFAULT_FORM	1

FUNCTION FTN_NAME(xdcon)( Unit, Cursor, Form, Blink )
INTEGER	Unit, Cursor, Form, Blink;
   {
   return ( zdcon( *Unit, *Cursor, *Form, *Blink ) );
   }

FUNCTION zdcon( unit, cursor, form, blink )
int	unit, cursor, form, blink;
   {
   int	status, tmpForm;

   xd_current_call = CON;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_CURSOR( cursor )) {
      status = NO_SUCH_CURSOR;
      }
   else if (!ZCHECK_CURSOR_TYPE( form )) {
      status = NO_SUCH_CURSOR_FORM;
      }
   else if ((form < 0) && (!ZMAY_RESIZE_CURSOR)) {
      status = NO_SUCH_CURSOR_FORM;
      }
   else if (!ZCHECK_CURSOR_BLINK_RATE( blink )) {
      status = NO_SUCH_CURSOR_RATE;
      }
   else {
      tmpForm = (form  == 0 ? DEFAULT_FORM : form );
      ZCURSOR_FORM( cursor ) = tmpForm;
      ZCURSOR_BLINK( cursor ) = blink;
      ZCURSOR_ACTIVE( cursor ) = TRUE;
      status = XD_Device_Interface( &unit, CURSOR_ON, cursor, tmpForm, blink );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
