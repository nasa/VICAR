/*	IVAS_Overlay - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Overlay( parameters )
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

FUNCTION IVAS_Overlay( Unit, Function, red, green, blue )
int	*Unit, Function, *red, *green, *blue;
   {
   int	i, IVAS_Color;

   if (Function == READ_OVERLAY_LUT) {
      /*  Read the GLUT from the device dependent array since the IVAS  */
      /*  does not support the GLUT read.                               */
      for ( i = 0; i < 16; i ++ ) {
         IVAS_Color = DCB[*Unit]->DeviceDependent[i];
         red[i] = (IVAS_Color & 0x000F) << 4;
         green[i] = IVAS_Color & 0x00F0;
         blue[i] = (IVAS_Color & 0x0F00) >> 4;
         }
      }
   else {
      IVASgmFreeze( TRUE );
      for ( i = 0; i < 16; i++ ) {
         IVAS_Color = (red[i] >> 4) & 0x000F;
         IVAS_Color = IVAS_Color | (green[i] & 0x00F0);
         IVAS_Color = IVAS_Color | ((blue[i] << 4) & 0x0F00);
         if ( OVERLAY_ON ) {
            IVASgmDefGraphic( i, GMcolor, IVAS_Color, 0 );
            }
         else {
            IVASgmDefGraphic( i, GMdisable, IVAS_Color, 0 );
            }

         /*  Save the GLUT in the device dependent array since the IVAS  */
         /*  does not support GLUT read.                                 */
         DCB[*Unit]->DeviceDependent[i] = IVAS_Color;
         }
      IVASgmFreeze( FALSE );
      IVASflush();
      }  /* end if */

   return (SUCCESS);
   }
