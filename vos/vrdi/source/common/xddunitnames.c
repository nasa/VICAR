/*	xddunitnames - description
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 12, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = xddunitnames( Ndev, Len, Devices )
 *
 *	Parameter List:
 *
 *		Ndev:	 Number of devices allocated:		out
 *		Len:	 Length of strings in Devices array:	in
 *		Devices: Array of strings holding device names:	out
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

FUNCTION FTN_NAME(xddunitnames)( INTEGER Ndev, INTEGER Len, STRING Devices,
		ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   int		status;
   char		CString[MAXIMUM_UNITS][DEVMAKE_SIZE];

   status = zddunitnames( Ndev, DEVMAKE_SIZE, CString );
   if (status == SUCCESS) {
      zsc2for_array((char *)CString, DEVMAKE_SIZE, *Ndev, Devices, Len, &Ndev,
						3, 3, 1, Devices);
      }
   return (status);
   }

FUNCTION zddunitnames( ndev, len, devices )
int	*ndev, len;
char	*devices;
{
   int i, status; 
   int DevNumber;
   int fake_unit_number;  /* used for the interface */

   xd_current_call = DUNITNAMES;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      status = XD_Get_Devices_Alloc(ndev, len, devices);
      }

   xd_error_handler ( &NoUnit, status );
   return ( status );
   }

