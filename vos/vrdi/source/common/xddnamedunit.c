/*	xddnamedunit - description
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 12, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = xddnamedunit( Unit, Name )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Name:	Name of the display device
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
#include "xdalloc.h"

FUNCTION xddnamedunit( INTEGER Unit, STRING Name, ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Name, &Unit, 2, 2, 1, Name);
   return ( zddnamedunit( Unit, CString ) );
   }

FUNCTION zddnamedunit( Unit, name )
int	*Unit;
char	name[];
{
   int i, status; 
   int DevNumber;

   xd_current_call = DNAMEDUNIT;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      status = XD_Get_Device(&DevNumber, name);

      *Unit = DevNumber;
      if (status == SUCCESS)
         DEVICE_ALLOCATED = TRUE;
      }

   xd_error_handler ( &NoUnit, status );
   return ( status );
   }
