/*	ADAGE_Configure - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Configure( parameters )
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

FUNCTION ADAGE_Configure( Unit, Config )
int	*Unit, *Config;

{
  int	status, lut, imp;

  if (((Config[1] == IMP_512) && (Config[2] == VIDEO_1024)) ||
      ((Config[1] == IMP_2048) && (Config[2] != VIDEO_1024)))
        return( INVALID_DEVICE_REQ );

  status = SUCCESS;

  OUTPUT_MODE = Config[0];
  IMP_SIZE = Config[1];
  VIDEO_SIZE = Config[2];
  ASPECT_RATIO = Config[3];

  if ( VIDEO_SIZE == VIDEO_1024 ) {
    OVERLAY_AVAILABLE = FALSE;
    OVERLAY_ON = FALSE;
    MAY_CONNECT_IMP_OVERLAY = FALSE;
    OVERLAY_IMP = 0;

    VIDEO_LINES = 1024;
    VIDEO_SAMPLES = 1024;

    if ( IMP_SIZE == IMP_1024 ) {
      N_SAMPS = 1024;
      N_LINES = 1024;
      N_IMPS = 12;

      for ( imp = 1; imp <= N_IMPS; imp++ ) {
	AW_LEFT( imp )   = 1;
	AW_TOP( imp )    = 1;
	AW_RIGHT( imp )  = 1024;
	AW_BOTTOM( imp ) = 1024;
	DW_LEFT( imp ) = 1;
	DW_TOP( imp ) = 1;
      }
    }
    else {
      N_SAMPS = 2048;
      N_LINES = 2048;
      N_IMPS = 3;
      for ( imp = 1; imp <= N_IMPS; imp++ ) {
	AW_LEFT( imp )   = 1;
	AW_TOP( imp )    = 1;
	AW_RIGHT( imp )  = 2048;
	AW_BOTTOM( imp ) = 2048;
	DW_LEFT( imp ) = 1;
	DW_TOP( imp ) = 1;
      }
    }
  } 
  else {
    OVERLAY_AVAILABLE = TRUE;
    OVERLAY_ON = TRUE;
    MAY_CONNECT_IMP_OVERLAY = TRUE;
    OVERLAY_IMP = 4;

    if ( IMP_SIZE == IMP_512 ) {
      N_SAMPS = 512;
      N_LINES = 512;
      N_IMPS = 32;
      for ( imp = 1; imp <= N_IMPS; imp++ ) {
	AW_LEFT( imp )   = 1;
	AW_TOP( imp )    = 1;
	AW_RIGHT( imp )  = 512;
	AW_BOTTOM( imp ) = 512;
	DW_LEFT( imp ) = 1;
	DW_TOP( imp ) = 1;
      }
    }
    else {

      N_SAMPS = 1024;
      N_LINES = 1024;
      N_IMPS = 8;
      for ( imp = 1; imp <= N_IMPS; imp++ ) {
	AW_LEFT( imp )   = 1;
	AW_TOP( imp )    = 1;
	AW_RIGHT( imp )  = 1024;
	AW_BOTTOM( imp ) = 1024;
	DW_LEFT( imp ) = 1;
	DW_TOP( imp ) = 1;
      }
    }
    if ( VIDEO_SIZE == VIDEO_512 ) {

      VIDEO_LINES = 512;
      VIDEO_SAMPLES = 512;
    }
    else {
      VIDEO_LINES = 480;
      VIDEO_SAMPLES = 640;
    }
  }
  for ( lut=1, imp=1; lut <= N_LUTS; lut++ ) {
    WHICH_IMP( lut ) = imp;
    if ( Config[0] == FULL_COLOR ) imp++;
  }
  IK_Write_Crossbar( Unit );
  IK_Set_Window( Unit, &1 );
  IK_Write_FBC( Unit );

  return ( status );
}
