/*	ADAGE_Subroutines - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IK_Write_FBC( Unit )
 *		STATUS = IK_Write_Crossbar( Unit )
 *		STATUS = IK_Calc_Phy_Coords( Unit, Imp, 
 *                                           Log_x, Log_y, Phy_x, Phy_y )
 *		STATUS = IK_Calc_FSS_Coords( Unit, Imp, 
 *                                           Log_x, Log_y, FSS_x, FSS_y )
 *		STATUS = IK_Set_Window( Unit, Imp )
 *		STATUS = IK_Reset_MPC( Unit )
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

/**/
FUNCTION IK_Write_FBC( Unit )
int	*Unit;

{
  FBC_VIEW_X = 0;
  FBC_MAP_PAGE = 0;
  FBC_AUTO_CLEAR = 0;
  FBC_INTERLACED = 0;

  if ( VIDEO_SIZE == VIDEO_1024 ) {
    FBC_VIEW_Y = 28;
    FBC_SIZE_X = 1023;
    FBC_SIZE_Y = 1023;
    FBC_WINDOW_X = 0;
    FBC_WINDOW_Y = -27;
    FBC_LINE_TIME = 148;
    FBC_NUM_LINES = 1070;
    FBC_RESOLUTION = 1;
    FBC_SYNC_SELECT = 0;
    FBC_SYNC_TIMING = 1;
    FBC_PIXEL_CLOCK = 20;
  } else {
    FBC_VIEW_Y = 32;
    FBC_RESOLUTION = 0;
    FBC_SYNC_TIMING = 0;

    if ( VIDEO_SIZE == VIDEO_512 ) {
      FBC_SIZE_X = 511;
      FBC_SIZE_Y = 511;
      FBC_WINDOW_X = 0;
      FBC_WINDOW_Y = -29;
      FBC_LINE_TIME = 286;
      FBC_NUM_LINES = 552;
      FBC_SYNC_SELECT = 0;
      FBC_PIXEL_CLOCK = 44;
    } else {
      FBC_SIZE_X = 640;
      FBC_SIZE_Y = 480;
      FBC_WINDOW_X = -12;
      FBC_WINDOW_Y = -30;
      FBC_LINE_TIME = 0;
      FBC_NUM_LINES = 0;
      FBC_SYNC_SELECT = 1;
      FBC_PIXEL_CLOCK = 36;
    }
  }
  ikbwr( &IK_CODE, &FBC_CONTROL_REGS, &7, &FBC_Control );
}
/**/
FUNCTION IK_Write_Crossbar( Unit )
int	*Unit;
{
  int i, j, Output_Bits;

/*  Several routines modify the connect information in the cross bar 
    switch.  This routine takes all the connect information in the 
    internal tables and creates a new cross bar table.
 */

  XBAR_CURSOR = 32;
  XBAR_COLORMAP = 33;

  for ( i = 1; i <= N_LUTS; i++ ) {
    if (!WHICH_IMP( i )) {
      for ( j = 0; j < 8; j++ ) {
	XBAR(i-1,j) = 0x3F;
      }
    } else {
      if ( VIDEO_SIZE == VIDEO_1024 ) {
	Output_Bits = (((WHICH_IMP(i) - 1) % 3) * 8);
      } else {
	Output_Bits = (((WHICH_IMP(i) - 1) % 4) * 8);
      }
      for ( j = 0 ;
            j < 8; j++, Output_Bits++ ) {
	XBAR(i-1,j) = Output_Bits;
      }
    }
  }
  if ( OVERLAY_ON && OVERLAY_IMP ) {
    for ( i = 0, Output_Bits = (((OVERLAY_IMP - 1) % 4) * 8);
            i < 8; i++, Output_Bits++ ) {
      XBAR_OVERLAY(i) = Output_Bits;
    }
  } else {
    for (i = 0; i < 8; i++ ) {
      XBAR_OVERLAY(i) = 0x3F;
    }
  }
  Set_Word_Mode_DMA;

  ikbwr( &IK_CODE, &XBS34_XBAR_CREG, &XBS34_WORDS, &XBar_Table );

  ikbwr( &IK_CODE, &LUVO_XBAR_CREG, &1, &IK_COLOR );
}

/**/
FUNCTION IK_Calc_Phy_Coords( Unit, Imp, Log_x, Log_y, Phy_x, Phy_y )
int	*Unit, Imp, Log_x, Log_y, *Phy_x, *Phy_y;

{
  int Temp_x, Temp_y;

  if ( VIDEO_SIZE == VIDEO_1024 ) {
    Temp_x = (( Imp - 1 ) % 6 )/3;
    Temp_y = ( Imp - 1 )/6;
  }
  else {
    if ( IMP_SIZE == IMP_1024 ) {
      Temp_x = 0;
      Temp_y = ( Imp - 1 )/4;
    }
    else {
      Temp_x = (( Imp - 1 ) % 8 )/4;
      Temp_y = ( Imp - 1 )/8;
    }
  }
  *Phy_x = ( Temp_x * N_SAMPS ) + Log_x - 1;
  *Phy_y = ( Temp_y * N_LINES ) + Log_y - 1;

  if ( (( VIDEO_SIZE == VIDEO_512 ) && (Imp > 16 )) ||
       (( VIDEO_SIZE == VIDEO_640_480 ) && (Imp > 4 )) ) {
    *Phy_y = *Phy_y + 1024;
  }

}
/**/
FUNCTION IK_Calc_FSS_Coords( Unit, Imp, Log_x, Log_y, FSS_x, FSS_y )
int	*Unit, *Imp, *Log_x, *Log_y, *FSS_x, *FSS_y;

{
  IK_Calc_Phy_Coords( Unit, Imp, Log_x, Log_y, FSS_x, FSS_y );

  if (VIDEO_SIZE == VIDEO_1024 ) {
    *FSS_x -= 512;
    *FSS_y = 511 - *FSS_y;
  } else {
    *FSS_x -= 256;
    *FSS_y = 255 - *FSS_y;
  }
}
/**/
FUNCTION IK_Set_Window( Unit, Imp )
int	*Unit, *Imp;

{
  int Phy_x;
  int Phy_y;

  IK_Calc_Phy_Coords( Unit, *Imp, DW_LEFT(*Imp), DW_TOP(*Imp),
                      &Phy_x, &Phy_y );

  if ( VIDEO_SIZE == VIDEO_1024 ) {	/* Adjust for vertical blanking	*/
    Phy_y -= ( WINDOW_1024 / (FBC_ZOOM_Y + 1 ));
  } else {
    Phy_x = Phy_x << 2;			/* Needs in bits 2-10		*/
    if ( VIDEO_SIZE == VIDEO_512 ) Phy_y -= ( WINDOW_512 / (FBC_ZOOM_Y + 1 ));
    else Phy_y -= ( WINDOW_640_480 / (FBC_ZOOM_Y + 1 ));
  }
  FBC_WINDOW_X = Phy_x;
  FBC_WINDOW_Y = Phy_y;

  Set_Word_Mode_DMA;
  ikbwr( &IK_CODE, &FBC_WINDOW_CREG, &1, &FBC_WINDOW );

}

FUNCTION IK_Reset_MPC( Unit )
int	*Unit;
{
  int IK_Data, MPC_Reset, Tries;
  int start_time, check_time;

  for ( MPC_Reset = FALSE, Tries = 0; (!MPC_Reset && (Tries < 3)); Tries++ ) {

    IK_Data = -1;
    ikbwr( &IK_CODE, &MPC_RESET, &1, &IK_Data );

    time( &start_time );
    check_time = start_time;
    while ( !MPC_Reset && ( check_time - start_time < 30) ) {
      ikbrd( &IK_CODE, &MPC_RESET, &1, &IK_Data );
      MPC_Reset = (( IK_Data & 0xFFFF ) == 0);
      time( &check_time );
    }
  }
  if ( !MPC_Reset ) return( CANNOT_RESET_MPC );

  return( SUCCESS);
}
