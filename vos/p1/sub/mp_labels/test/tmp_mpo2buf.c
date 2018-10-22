#include <math.h>
#include "mp_routines.h"
#include "vicmain_c"

/* 

Test Program TMP_MPO2BUF

Author		Lucas Kamp
Date		March 1995

Purpose

Test interface to OM matrix computation for POINT_PERSPECTIVE images.
MP routines called are mpInit, mpLabelRead, mpMpo2Buf, and mpFree,
in that order.
		
*/

void main44()
{
  int 	i,j,k;
  int	status, unit1;
  double buf[20];
  char msg[200];
  MP mp_obj;

  zveaction("","");

  status = mpInit( &mp_obj );

  zvpcnt( "INP", &i );

  if ( i > 0 ) {
    status = zvunit( &unit1, "INP", 1, 0);
    status = zvopen( unit1, "OP", "READ", 0);
    ABENDif( status!=1);
    status = mpLabelRead( mp_obj, unit1);
    ABENDif( status!=mpSUCCESS);
    zvclose( unit1, 0);
  }

  /*

  Translate MP data object to 40-word buffer and print the computed
  OM matrix

  */

  status = mpMpo2Buf( mp_obj, buf );
  ABENDif( status<mpSUCCESS);

  zvmessage( "\n***\n*** Computed OM-matrix:\n***","" );

  for (i=0; i<9; i++) {
    sprintf( msg,"\tOM[%d] = %8.4f", i, buf[i]);
    zvmessage( msg, "");
  }

  mpFree( mp_obj );
}
