/*	RAMTEK_Connect - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = name( parameters )
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

#include "rtekinc.h"

FUNCTION RAMTEK_Connect( Unit, imp, lut, section )
int	*Unit, imp, lut, *section;

{
  LUT_OPCODE = LOAD_AUX_MEM;
  LUT_DEVICE = RM_Device[RM_Channel_No];
  LUT_ADDRESS = ( *section - 1 ) * 512;
  LUT_LENGTH = 0;

  RM_Lut_Section = *section;

  rmout( &RM_Channel_No, &Aux_Memory, &3 );

  return (SUCCESS);
}
