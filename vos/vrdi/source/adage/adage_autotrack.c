/*	ADAGE_Autotrack - description
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

#include "ikoninc.h"

FUNCTION ADAGE_AutoTrack( Unit, function, device, cursor )
int	*Unit, function, device, cursor;

{
  int i, IK_Data, status;
  int Temp_code;

  status = SUCCESS;
  Set_Word_Mode_DMA;

  if ( function == AUTO_ON ) {
    if ( !IK_Auto_On ) {
      ikbgei( &Temp_code );
      Temp_code = 0173760 & Temp_code;
      ikbsei( &0540 );				/* Halfword transfers */

      for ( i = 0; ((i < 4) && (status == SUCCESS)); i++ ) {
	status = MPC_Write( Unit, i );
      }
      if ( status == SUCCESS ) IK_Auto_On = TRUE;
      ikbsei( &Temp_code );			/* Restore ICSR	*/
    }
  } else {
    if ( IK_Auto_On ) {
      status = IK_Reset_MPC( Unit );
      IK_Auto_On = FALSE;
    }
  }

  return ( status );
}
FUNCTION MPC_Write( Unit, Command )
int *Unit, Command;
{
  static char MPC_Command[4][12] = {"SDTEL CO 1",	/* DSET LOC1	*/
                                    "SDTEL CO 2",	/* DSET LOC2	*/
                                    "SDTEL CO 4",	/* DSET LOC4	*/
                                    "IDPSAL Y" };	/* DISPLAY	*/

  short str_len, mod_str;
  int status, Tries;

  status = SUCCESS;
  str_len = strlen(MPC_Command[Command]) - 1;
  mod_str = ( str_len + 1 )/2;
  ikbwr( &IK_CODE, &MPC_Buffer, &mod_str, &MPC_Command[Command] );

  Half_data[0] = str_len;
  Half_data[1] = 0xFFFF;
  Half_data[2] = 0;
  Half_data[3] = 0;
  ikbwr( &IK_CODE, &MPC_Length, &2, &Half_data );

  for ( Tries = 0; ((Half_data[0] != 0) && (Tries < 10)); Tries++ ) {
    ikbrd ( &IK_CODE, &MPC_Length, &2, &Half_data );
  }
  if ( Half_data[0] != 0 ) status = CANNOT_RESET_MPC;

  return( status );
}
