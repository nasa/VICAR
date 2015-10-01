
/*	IVAS_Line - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Line( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "ivasinc.h"

FUNCTION IVAS_Line( Unit, Function, Imp, X, Y, length, buf, mask )
int	*Unit, Function, Imp, X, Y, length;
BYTE	buf;
unsigned char mask;

{
  int	IVAS_Mask, IVAS_OP, IVAS_x, IVAS_length, i, Bumpy;
  unsigned char *Bump, *even_line, IVAS_line[1024];

  IVAS_length = length;
  IVAS_x = IVAS_X_IMG(X);
  Bump = buf;

  if ( ODD_TEST( IVAS_x )) {
    IVAS_x++;
    Bump++;
    if (!( ODD_TEST( IVAS_length ))) IVAS_length--;
  }
  if ( ODD_TEST( IVAS_length )) IVAS_length--;

  IVASsyncRestrict( 0, 0 );

  if ( IVAS_length > 0 ) {
    if (Function == WRITE_LINE) {
      IVAS_OP   = PassIn;

      if (Imp == OVERLAY_IMP) {
	IVAS_Mask = mask & 0xF;
	IVASmaGraphics( Bump, IVAS_length, IVAS_OP, IVAS_length, 
			IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask );
      }
      else {
	IVAS_Mask = mask & 0xFF;
	IVASmaImage( Bump, IVAS_length, IVAS_OP, PassByte, IVAS_length,
			IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask, 0 );
      }
    }
    else {
      IVAS_OP   = PassOut;
      Bumpy = Bump;
      if ( ODD_TEST( Bumpy )) {
	even_line = &IVAS_line;
      }
      else {
	even_line = Bump;
      }
      if (Imp == OVERLAY_IMP) {
	IVAS_Mask = 0xF;
	IVASmaGraphics( even_line, IVAS_length, IVAS_OP, IVAS_length, 
			IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask );
      }
      else {
	IVAS_Mask = ALL_BITS;
	IVASmaImage( even_line, IVAS_length, IVAS_OP, PassByte, IVAS_length,
			IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask, 0 );
      }
      if ( ODD_TEST( Bumpy )) {
	for ( i = 0; i < IVAS_length; i++ ) Bump[i] = IVAS_line[i];
      }
    }
    IVASflush();
  }

  IVASsyncRestrict( 2, 3 );

  if (Function == WRITE_LINE) {
    if ( IVAS_X_IMG(X) != IVAS_x ) {
      IVAS_Pixel( Unit, WRITE_PIXEL, Imp, X, Y, buf, mask );
      IVAS_length++;
    }
    if ( length != IVAS_length ) {
      IVAS_Pixel( Unit, WRITE_PIXEL, Imp, X+length-1, Y, buf+length-1, mask );
    }	
  }
  else {
    if ( IVAS_X_IMG(X) != IVAS_x ) {
      IVAS_Pixel( Unit, READ_PIXEL, Imp, X, Y, buf, mask );
      IVAS_length++;
    }
    if ( length != IVAS_length ) {
      IVAS_Pixel( Unit, READ_PIXEL, Imp, X+length-1, Y, buf+length-1, mask );
    }	
  }
  return (SUCCESS);
}
