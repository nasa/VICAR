/*	IVAS_OpenDevice - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_OpenDevice( parameters )
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

FUNCTION IVAS_OpenDevice( Unit )
int	*Unit;

   {
   int	i, buffer[1024];

/*	General Initialization
 */
   STANDALONEInit();
   IVASopen( &DEV_UNIT_NO, DEV_NAME, strlen(DEV_NAME), 0 );
   IVASsyncRestrict( 2, 3 );
   IVASvpDepth( 8 );
   IVASgphReset();
   IVASgmReset();
   IVASvpSetup( WHICH_IMP(RED_LUT)-1, WHICH_IMP(GREEN_LUT)-1,
		WHICH_IMP(BLUE_LUT)-1, 0x1F );

/*	Set up GM memory
 */
   IVASgmFreeze( TRUE );
   IVASgmDefChars( GMdisable, 0x0FFF, 0 );		/* Character:  White */
   for (i=0; i<16; i++ ) {
      IVASgmDefGraphic( i, GMdisable, 0x0FFF, 0 );	/* Graphics:   White */
      }
   IVASgmDefImage( WHICH_IMP(RED_LUT)-1, WHICH_IMP(GREEN_LUT)-1,
			WHICH_IMP(BLUE_LUT)-1, TRUE, 0 );
   IVASgmSelChars( GMdisable, 0 );		/* Character:  Disabled */
   IVASgmSelGraphic( -1, GMdisable, 0 );	/* Graphics:   Disabled */
   IVASgmSelImage( TRUE, 0 );			/* Image:      Enabled */
   IVASgmFreeze( FALSE );

/*	Initialize Graphics
 */
   IVASgphArea( 0, 0, 1023, 1023 );
   IVASgphBlink( 1, 0 );
   IVASgphBlinkCursor( 1, 0 );
   IVASgphCompare( 0 );
   IVASgphOrigin( 0, 0 );
   IVASgphValue( 0, 0 );
   IVASgphMask( 0xFF );

/*   IVASgphMove( 0, 0, GPHabs );	*/
/*   IVASgphFill( 1023, 1023, GPHabs );	*/	/* Erase Graphics memory */

/*	Clear Character memory
 */
   for ( i = 0; i < 1024; buffer[i++] = 0 );
   for ( i = 0; i < 1024; i += 2 ) {
      IVASmaCharacter( buffer, 1024, PassIn, 0, i );
      }

/*      Initialize zoom and scroll registers
 */

   IVASvpZoomScroll( 0, 0, 1, 1, 3 );


   IVASflush();

   return (SUCCESS);
   }
