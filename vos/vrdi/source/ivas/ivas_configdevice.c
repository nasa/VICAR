/*	IVAS_ConfigDevice - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_ConfigDevice ( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "ivasinc.h"

FUNCTION IVAS_ConfigDevice( Unit, Config )
int *Unit, *Config;

{
  int	lut,imp;

  IMP_SIZE = DIB[*Unit]->DefaultConfig[1];
  VIDEO_SIZE = DIB[*Unit]->DefaultConfig[2];
  ASPECT_RATIO = DIB[*Unit]->DefaultConfig[3];

  for ( lut=1, imp=1; lut <= N_LUTS; lut++ ) {
    WHICH_IMP( lut ) = imp;
    if ( Config[0] == FULL_COLOR ) imp++;
  }
  return IVAS_Connect ( Unit );
}
