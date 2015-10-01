/*	ADAGE_Vector.c - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Vector.c( parameters )
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

FUNCTION ADAGE_Vector( Unit, imp, npts, x, y, color, mask )
int	*Unit, imp, npts, *x, *y, color, mask;

{
  int i, Norm_Imp, FSS_X, FSS_Y;
  short fill[4], kmask[4], fx, fy, res;

  if ( VIDEO_SIZE == VIDEO_640_480 ) return( DEVICE_CANNOT_DO_IT );

  IK_Calc_FSS_Coords( Unit, imp, x[0], y[0], &FSS_X, &FSS_Y );
  fx = FSS_X;
  fy = FSS_Y;

  for ( i = 0; i < 4; i++ ) fill[i] = 0;
  for ( i = 0; i < 4; i++ ) kmask[i] = 0;

  if ( VIDEO_SIZE == VIDEO_1024 ) {
    Norm_Imp = (imp - 1) % 3;
    res = 1;
  } else {
    Norm_Imp = (imp - 1) % 4;
    res = 0;
  }
  fill[Norm_Imp] = color & 0xFF;
  kmask[Norm_Imp] = mask & 0xFF;

  Set_Word_Mode_DMA;

  kdisab( &0 );
  kres ( &0, &res );
  krgba( &0, &fill[0], &fill[1], &fill[2], &fill[3] );
  kwmask( &0, &kmask[0], &kmask[1], &kmask[2], &kmask[3] );
  kltype( &0, &0 );
  kvstrt( &0 );
  kmove( &0, &fx, &fy, &0 );

  for ( i = 1; i < npts; i++ ) {
    IK_Calc_FSS_Coords( Unit, imp, x[i], y[i], &FSS_X, &FSS_Y );
    fx = FSS_X;
    fy = FSS_Y;
    kdraw( &0, &fx, &fy, &0 );
  }
  kvdone( &0 );

  kend( &0 );
  ksend( &BUFFER, &PC.PC, &ADDR.HLOAD, &ADDR.LLOAD );
  kgo( &0 );

  return (SUCCESS);
}
