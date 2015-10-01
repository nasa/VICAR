/*	xdeaction - Set Error Action
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 7. 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = xdeaction ( Warning, Error, Fatal )
 *
 *	Parameter List:
 *
 *		Warning:	Action for errors in Warning category
 *		Error:		Action for errors in Error category
 *		Fatal:		Action for errors in Fatal category
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

FUNCTION FTN_NAME(xdeaction) ( Warning, Error, Fatal )
INTEGER	Warning, Error, Fatal;
   {
   return ( zdeaction ( *Warning, *Error, *Fatal ));
   }

FUNCTION zdeaction ( warning, error, fatal )
int	warning, error, fatal;
   {
   int status;

   xd_current_call = EACTION;

   if ( !ZCHECK_ERROR_ACTION ( warning ) ) {
      status = INVALID_WARNING_ACTION;
      }
   else if ( !ZCHECK_ERROR_ACTION ( error ) ) {
      status = INVALID_ERROR_ACTION;
      }
   else if ( !ZCHECK_ERROR_ACTION ( fatal ) ) {
      status = INVALID_FATAL_ACTION;
      }
   else {
      SET_BITS( xd_warn_action, warning );
      SET_BITS( xd_error_action, error );
      SET_BITS( xd_fatal_action, fatal );
      status = SUCCESS;
      }

   xd_error_handler ( &NoUnit, status );
   return (status );
   }
