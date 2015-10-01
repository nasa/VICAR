/*	RAMTEK_Text - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Text( parameters )
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

FUNCTION RAMTEK_Text( Unit, imp, x, y, location, nchars, text )
int	*Unit, imp, x, y, location, *nchars, *text;

{
  int Text_Words, Size_Factor, width, stat;

  if ( FONT_NUMBER == 0 ) {

    TEXT_X_MIN = AW_LEFT( imp );
    TEXT_Y_MIN = AW_TOP( imp );
    TEXT_X_MAX = AW_RIGHT( imp );
    TEXT_Y_MAX = AW_BOTTOM( imp );

    TEXT_COLOR = FONT_COLOR;
    TEXT_MASK1 = FONT_MASK;
    TEXT_MASK2 = 0;

    Size_Factor = FONT_HEIGHT / TEXT_HEIGHT;
    if ( Size_Factor < 1 ) Size_Factor = 1;
    if ( Size_Factor > 12 ) Size_Factor = 12;
    TEXT_X_SIZE = Size_Factor - 1;
    TEXT_Y_SIZE = Size_Factor - 1;

    TEXT_X = x - 1;
    if (location == TEXT_CENTER ) 
      TEXT_X -= ( *nchars * TEXT_WIDTH * Size_Factor ) / 2;
    if (location == TEXT_RIGHT ) 
      TEXT_X -= *nchars * TEXT_WIDTH * Size_Factor;

    TEXT_Y = y - ( TEXT_HEIGHT * Size_Factor ) - 1;

    TEXT_LENGTH = *nchars;

    Text_Words = (*nchars + 1 ) / 2;
    rmout2( &RM_Channel_No, &RM_Write_Text, &WRITE_TEXT_WORDS, text, &Text_Words );
    stat = SUCCESS;
  } else stat = DEVICE_CANNOT_DO_IT;

  return ( stat );
}
