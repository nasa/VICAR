/*	ADAGE_Zoom - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Zoom( parameters )
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

FUNCTION ADAGE_Zoom( Unit, imp, zoom )
int	*Unit, imp, zoom;

{
  int Imp;

  Imp = imp;
  FBC_ZOOM_X = zoom - 1;
  FBC_ZOOM_Y = zoom - 1;

  Set_Word_Mode_DMA;

  if ( VIDEO_SIZE == VIDEO_1024 ) {
    if ( zoom == 1 ) FBC_PIXEL_CLOCK = 20;
    else {
      FBC_ZOOM_X -= 1;
      FBC_PIXEL_CLOCK = 45;
    }
    ikbwr( &IK_CODE, &FBC_VIDEO_CREG, &1, &FBC_VIDEO );
  }

  ikbwr( &IK_CODE, &FBC_ZOOM_CREG, &1, &FBC_ZOOM );
  IK_Set_Window( Unit, &Imp );

  return ( MUST_ZOOM_ALL );
}
