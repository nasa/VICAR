/*	ADAGE_CurPosition - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_CurPosition( parameters )
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

FUNCTION ADAGE_CurPosition( Unit, function, cursor, x, y )
int	*Unit, function, cursor, *x, *y;
{
  short ADJ_x, ADJ_y;
  int pen;

  Set_Word_Mode_DMA;
  ADJ_x = (int) x;
  ADJ_y = (int) y;
  if (function == WRITE_CURSOR) {
    if ( IK_Auto_On ) return ( CANNOT_POS_CURSOR );
    else {
      ADJ_x = max( ADJ_x - CURSOR_OFFSET, 0 );
      if ( VIDEO_SIZE == VIDEO_1024 ) {
	ADJ_y = max( ADJ_y - CURSOR_OFFSET, 0 ) + WINDOW_1024 - 1;
      } 
      else {
	if ( VIDEO_SIZE == VIDEO_512 ) {
	  ADJ_y = max( ADJ_y - CURSOR_OFFSET, 0 ) + WINDOW_512 - 1;
	} else {
	  ADJ_x += 3;
	  ADJ_y = max( ADJ_y - CURSOR_OFFSET, 0 ) + WINDOW_640_480 - 1;
	}	    
      }
      FBC_CURSOR_X = ADJ_x;
      FBC_CURSOR_Y = ADJ_y;
      ikbwr ( &IK_CODE, &FBC_CURPOS_CREG, &1, &FBC_CURSOR );
    }
  }
  else {

    ikbrd( &IK_CODE, &TABLET_ADDRESS, &3, &MPC_Data );

    if ( VIDEO_SIZE == VIDEO_1024 ) {
      *x = MPC_X + CURSOR_OFFSET;
      *y = MPC_Y + CURSOR_OFFSET - WINDOW_1024;
    } 
    else {
      *x = ( MPC_X / 2 ) + CURSOR_OFFSET;

      if ( VIDEO_SIZE == VIDEO_512 ) {
	*y = ( MPC_Y / 2 ) + CURSOR_OFFSET - WINDOW_512 + 1;
      }
      else {
	*y = ( MPC_Y / 2 ) + CURSOR_OFFSET - WINDOW_640_480 + 1;
      }
    }
    *x = min( max( *x, 1 ), VIDEO_SAMPLES );
    *y = min( max( *y, 1 ), VIDEO_LINES );
  }
  return (SUCCESS);
}
