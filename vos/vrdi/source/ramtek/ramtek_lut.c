/*	RAMTEK_Lut - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Lut( parameters )
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

FUNCTION RAMTEK_Lut( Unit, function, lut, section, buffer )
int *Unit, function, lut, section, *buffer;

{
  int i;
  int phy_lut;
  unsigned char lutdata[256][4];

/*	Read segment			*/

  LUT_DEVICE = RM_Device[RM_Channel_No];
  LUT_ADDRESS = (section - 1) * 512;

  LUT_OPCODE = READ_AUX_MEM;
  LUT_LENGTH = 1024;

  rmotin( &RM_Channel_No, &Aux_Memory, &3, &lutdata, &512 );

  ADJUST_LUT( lut, phy_lut );

  if (function == WRITE_LUT) {
    
    for ( i = 0; i < 256; i++ ) {
      lutdata[i][phy_lut] = buffer[i]; 
      lutdata[i][3] = 0;
    }

    LUT_OPCODE = LOAD_AUX_MEM;
    rmout2( &RM_Channel_No, &Aux_Memory, &3, &lutdata, &512 );

  } else {
    for ( i = 0; i < 256; i++ ) { 
      buffer[i] = lutdata[i][phy_lut]; 
    }
  }
/*	Reset LUT section address	*/

  LUT_ADDRESS = (RM_Lut_Section - 1) * 512;
  LUT_LENGTH = 0;
  rmout( &RM_Channel_No, &Aux_Memory, &3 );

  return (SUCCESS);
}
