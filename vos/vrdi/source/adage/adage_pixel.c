/*	ADAGE_Pixel - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Pixel( parameters )
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
#define Four_pixels pixel_club.pixels_4
#define Four_chars pixel_club.pixel_32

FUNCTION ADAGE_Pixel( Unit, Function, Imp, X, Y, pixel, mask )
int	*Unit, Function, Imp, X, Y;
unsigned char *pixel,mask;

{
  int Norm_Imp, Mask_32;
  int Phy_x, Phy_y, Res;
  char pixel_32[4];

  if ( VIDEO_SIZE == VIDEO_1024 ) Norm_Imp = ( Imp - 1 ) % 3;
  else Norm_Imp = ( Imp - 1 ) % 4;

  IK_Calc_Phy_Coords( Unit, Imp, X, Y, &Phy_x, &Phy_y );

  Set_Word_Mode_DMA;

  if ( Function == WRITE_PIXEL ) {
    Mask_32 = mask << (Norm_Imp * 8);
    if ( VIDEO_SIZE == VIDEO_1024 ) {
      ikpwr( &033, &0, &0, &1, &Mask_32 );	/* Hi-Res write mask */
    } else {
      if ( Phy_y > 1023 ) {
	ikpwr( &032, &0, &2048, &1, &Mask_32 );	/* Upper Lo-Res write mask */
      } else {
	ikpwr( &032, &0, &0, &1, &Mask_32 );	/* Lower Lo-Res write mask */
      }
    }
  }
  if ( VIDEO_SIZE == VIDEO_1024 ) Res = HI_RES;
  else Res = LO_RES;

  if ( Function == WRITE_PIXEL ) {
    pixel_32[Norm_Imp] = *pixel;
    ikpwr1( &Res, &Phy_x, &Phy_y, pixel_32 );
  } else {
    ikprd1( &Res, &Phy_x, &Phy_y, pixel_32 );
    *pixel = pixel_32[Norm_Imp];
  }
  
  return (SUCCESS);
}
