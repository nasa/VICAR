/*	RAMTEK_ConfigDevice - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_ConfigDevice ( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION RAMTEK_ConfigDevice( Unit, Config )
int *Unit, *Config;

{
  int	section,lut,imp;

  OUTPUT_MODE  = Config[0];
  IMP_SIZE     = Config[1];
  VIDEO_SIZE   = Config[2];
  ASPECT_RATIO = Config[3];

  imp = 1;
  section = 1; 
  for ( lut=1; lut <= N_LUTS; lut++ ) {
    WHICH_IMP( lut ) = imp;
    WHICH_SECTION( lut ) = section;
  }
  lut = 1;
  return RAMTEK_Connect ( Unit, &imp, &lut, &section );
}
