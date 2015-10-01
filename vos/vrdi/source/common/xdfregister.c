/*	xdfregister - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 11, 1989
 *
 *	Calling Sequence:
 *
 *		xdfregister( Group )
 *
 *	Parameter List:
 *
 *	        Group:  Group of flags to use
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

FUNCTION FTN_NAME(xdfregister)( Group )
INTEGER Group;
   {
   zdfregister( *Group );
   }

FUNCTION zdfregister( group )
int   group;
   {

   if (!ZCHECK_GROUP_NUMBER)
      group = 0;

   mask = 1 << group;
   invmask = ~mask;
   }
