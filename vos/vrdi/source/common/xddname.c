/*	xddname - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xddname( parameters )
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

FUNCTION FTN_NAME(xddname)( INTEGER Unit, INTEGER Flag, STRING Name,
	INTEGER MaxLen, INTEGER RetLen, ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   int		status;
   char		CString[256];

   status = zddname( *Unit, *Flag, CString, *MaxLen, RetLen );
   if (status == SUCCESS) {
      zsc2for(CString, *MaxLen, Name, &Unit, 5, 3, 1, RetLen);
      }
   return (status);
   }

FUNCTION zddname( unit, flag, name, maxlen, retlen )
int	unit, flag;
char	name[];
int	maxlen, *retlen;
   {
   int		status;

   xd_current_call = DNAME;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else {
      if (flag == 1) {
         strcpy(name, ZDEV_NAME);
         *retlen = MIN( maxlen, strlen(name) );
         status = SUCCESS;
         }
      else if (flag == 2) {
         strcpy(name, DIB[unit]->Make );
         *retlen = MIN( maxlen, strlen(name) );
         status = SUCCESS;
         }
      else {
         status = INVALID_ARGUMENT;
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
