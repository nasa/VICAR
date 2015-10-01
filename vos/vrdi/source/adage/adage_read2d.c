/*	ADAGE_Read2d - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Read2d ( parameters )
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

FUNCTION ADAGE_Read2d( Unit, device, xvalue, yvalue, prox, pen )
int	*Unit, device, *prox, *pen;
float	*xvalue, *yvalue;

{
  int penprox;

  Set_Word_Mode_DMA;

  ikbrd( &0, &TABLET_ADDRESS, &3, &MPC_Data );

  penprox = MPC_PEN;

  if (MPC_X < 100) MPC_X = 100;
  if (MPC_X > 900) MPC_X = 900;
  if (MPC_Y < 100) MPC_Y = 100;
  if (MPC_Y > 900) MPC_Y = 900;

  *xvalue = ( ((float) MPC_X ) - 500)/400;
  *yvalue = ( ((float) MPC_Y ) - 500)/400;

  if (BIT_TEST( penprox, 0 )) *prox = 1;
  else *prox = 0;

  if (BIT_TEST( penprox, 1 )) *pen = 1;
  else *pen = 0;

  return (SUCCESS);
}
