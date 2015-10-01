/*	ADAGE_Connect - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Connect( parameters )
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

#include "ikoninc.h"

FUNCTION ADAGE_Connect( Unit, Imp )
int	*Unit, Imp;

{

/*  Connecting to an IMP requires the setting of the display window.
    Because the display window location effects all four physical
    image memory planes, when a logical IMP is connected to a LUT
    the logical IMPs connected to other LUTs may change, including
    the IMP connected to the graphics overlay LUT.
 */

  int lut, Loc_Imp, *Imp_ptr;

  Loc_Imp = Imp;
  Imp_ptr = &Loc_Imp;
  for ( lut = 1; lut <= N_LUTS; lut++ ) {
    if ( WHICH_IMP( lut )) {
      if ( VIDEO_SIZE == VIDEO_1024 ) {
	WHICH_IMP(lut) = (( Loc_Imp - 1)/3)*3 + 1 + ((WHICH_IMP(lut) - 1) % 3);
      } else {
	WHICH_IMP(lut) = (( Loc_Imp - 1)/4)*4 + 1 + ((WHICH_IMP(lut) - 1) % 4);
      }
    }
  }
  if ( OVERLAY_IMP ) {
    OVERLAY_IMP = (( Loc_Imp - 1)/4)*4 + 1 + ((OVERLAY_IMP - 1) % 4);
  }

  IK_Write_Crossbar ( Unit );

  IK_Set_Window ( Unit, Imp_ptr );

  return (SUCCESS);
}
