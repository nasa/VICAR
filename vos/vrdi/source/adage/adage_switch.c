/*	ADAGE_Switch - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Switch( parameters )
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

FUNCTION ADAGE_Switch( Unit, device, sw, value )
int	*Unit, device, sw, *value;

{
  int pen;
  Set_Word_Mode_DMA;

  ikbrd( &0, &TABLET_ADDRESS, &3, &MPC_Data );

  pen = MPC_PEN;
  if ( BIT_TEST(pen, 1 )) *value = 1;
  else *value = 0;

  return (SUCCESS);
}
