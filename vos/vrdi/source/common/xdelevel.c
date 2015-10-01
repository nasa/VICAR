/*	xdelevel - Return the error level of a given error code
 *
 *	Purpose:
 *
 *	Written by:	Paul Bartholomew
 *	Date:		December 7, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdelevel ( Code )
 *
 *	Parameter List:
 *
 *		Code:	Error Status Code
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

FUNCTION FTN_NAME(xdelevel)( Code )
INTEGER	Code;
   {
   return (zdelevel( *Code ));
   }

FUNCTION zdelevel( code )
int	code;
{
   int code_level;

   if (code == SUCCESS) {
      code_level = XD_NO_ERROR;
      }
   else if ( IS_KNOWN_CODE( code )) {
      if (IS_WARNING( code ))
         code_level = XD_WARN;
      else if (IS_ERROR( code ))
         code_level = XD_ERROR;
      else if (IS_FATAL( code ))
         code_level = XD_FATAL;
      else
         code_level = XD_UNKNOWN;
      }
   else {      /* Code is not known */
      code_level = XD_UNKNOWN;
      }

   return ( code_level );
}
