/*	XD_Draw_Char - Draws a single character from the current font
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:	October 1, 1986
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Draw_Char( Unit, Imp, iChar, sX, sY )
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


#define	MAX_BUFFER	1024

FUNCTION int XD_Draw_Char( Unit, Imp, iChar, sX, sY )
int	*Unit, *Imp, iChar, sX, sY;

   {
   int		status, i, nDraws, px, py;
   int		X[MAX_BUFFER], Y[MAX_BUFFER];
   double	ddx, ddy;
   
   status = SUCCESS;
   if (xd_font_info.vCount[iChar] == 0) {
      return (SUCCESS);
      }
   else {
      nDraws = 0;
      for ( i = 0; i < xd_font_info.vCount[iChar]; i++ ) {
	 ddx = FONT_SCALE * (FONT_HEIGHT-1) * xd_font_info.ptrX[iChar][i];
	 ddy = (FONT_HEIGHT-1) * xd_font_info.ptrY[iChar][i];
	 px  = sX + ROUND(FONT_COSINE*ddx - FONT_SINE*ddy);
	 py  = sY - ROUND(FONT_SINE*ddx + FONT_COSINE*ddy);

	 if (xd_font_info.ptrMD[iChar][i] == 0) {
	    if (nDraws > 1) {
	       status = XD_Polyline( Unit, Imp,
		  nDraws, X, Y, &FONT_COLOR, FONT_MASK );
	       if (status != SUCCESS) return (status);
	       }

	    nDraws = 0;
	    X[nDraws] = px;
	    Y[nDraws++] = py;
	    }
	 else {
	    X[nDraws] = px;
	    Y[nDraws++] = py;

	    if (nDraws >= MAX_BUFFER) {
	       status = XD_Polyline( Unit, Imp, nDraws, X, Y,
					&FONT_COLOR, FONT_MASK );
	       if (status != SUCCESS) return (status);
	       nDraws = 0;
	       X[nDraws] = px;
	       Y[nDraws++] = py;
	       }
	    }
	 }
      if (nDraws > 1) {
	 status = XD_Polyline( Unit, Imp,
		  nDraws, X, Y, &FONT_COLOR, FONT_MASK );
	 }
      }

   return (status);
   }
