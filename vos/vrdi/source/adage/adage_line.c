/*	ADAGE_Line - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Line( parameters )
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

#define ODD_TEST(Val) (((Val) & 1) == 1)

FUNCTION ADAGE_Line( Unit, Function, Imp, X, Y, length, buf, mask )
int	*Unit, Function, Imp, X, Y, length;
unsigned char buf[1024], mask;

{
  int Norm_Imp, Mask_32;
  int Phy_x, Phy_y, Res, i;
  union LINE_UNION {
    int Iko_Line[1024];
    unsigned char Iko_Byte[1024][4];
  } Iko_Union;
  int ADAGE_length;

  ADAGE_length = length;
  if ( ODD_TEST( ADAGE_length )) ADAGE_length--;

  IK_Calc_Phy_Coords( Unit, Imp, X, Y, &Phy_x, &Phy_y );

  if ( VIDEO_SIZE == VIDEO_1024 ) {
    Res = HI_RES;
    Norm_Imp = ( Imp - 1 ) % 3;
  }
  else {
    Res = LO_RES;
    Norm_Imp = ( Imp - 1 ) % 4;
  }

  if ( ADAGE_length > 1 ) {
    if ( Function == WRITE_LINE ) {

      Set_Word_Mode_DMA;
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
      Set_Byte_Mode_DMA;
      ikpwr( &Res, &Phy_x, &Phy_y, &ADAGE_length, buf );

    } else {
      Set_Word_Mode_DMA;
      ikprd( &Res, &Phy_x, &Phy_y, &ADAGE_length, &Iko_Union );

      for (i = 0; i < ADAGE_length; i++ ) {
	buf[i] = Iko_Union.Iko_Byte[i][Norm_Imp];
      }
    }
  }
  if ( length != ADAGE_length ) {

    if ( Function == WRITE_LINE ) {
      return( ADAGE_Pixel( Unit, WRITE_PIXEL, Imp, X+ADAGE_length,
                           Y, buf+ADAGE_length, mask ));
    } else {
      return( ADAGE_Pixel( Unit, READ_PIXEL, Imp, X+ADAGE_length,
                           Y, buf+ADAGE_length, mask ));
    }
  }
  return (SUCCESS);
}
