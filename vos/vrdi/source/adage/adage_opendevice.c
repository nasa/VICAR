/*	ADAGE_OpenDevice - description
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		17-Sep-1987
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_OpenDevice( parameters )
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

FUNCTION ADAGE_OpenDevice( Unit )
int	*Unit;

{
  FBC_VIEW_LOC = 0;
  FBC_VIEW_SIZE = 0;
  FBC_WINDOW = 0;
  FBC_ZOOM = 0;
  FBC_RATE = 0;
  FBC_VIDEO = 0;
  FBC_CURSOR = 0;

  /* Initialize	*/

  kpinit( &1 );

  IK_Write_Crossbar( Unit );
  IK_Write_FBC( Unit );
  IK_Set_Window( Unit, &1 );

/* Reset the MPC */

  IK_Reset_MPC( Unit );

/* Calculate tablet data address on Ikonas bus		*/

  Set_Word_Mode_DMA;
  ikbrd( &IK_CODE, &MPC_TABLET, &2, &MPC_Data );

  TAB_ADDR_1 = MPC_X;
  TAB_ADDR_2 = MPC_Y;
  TABLET_ADDRESS = ( (TABLET_ADDRESS + 8) / 2 ) | MPC_MASK ;

/* Get MPC Command Buffer Address and Command Buffer Length Address	*/

  ikbrd( &IK_CODE, &MPC_COMMAND, &2, &Half_data );
  MPC_Buffer = ( Half_data[2] / 2 ) | MPC_MASK;
  MPC_Length = MPC_Buffer + 0x40;

  return ( SUCCESS );
}
