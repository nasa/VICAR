/*	XD_Clip - Clips a line to the access window.
 *
 *	Purpose:
 *
 *	Written by:  R. A. Mortensen
 *	Date:	     September 19, 1986
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Clip( X, Y, window )
 *
 *	Parameter List:
 *
 *		X, Y:	X/Y Coordinates of line
 *		window:	Access window to clip to
 *
 *	Possible Return Codes:
 *
 *		-1 - line is not in window
 *		 0 - line is completely within window
 *		 1 - line clipped at first end
 *		 2 - line clipped at second end
 *		 3 - line clipped at both ends
 */
#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"



FUNCTION int XD_Clip( X, Y, window )
int	*X, *Y, *window;

   {
   int	result, swaps;
   int	temp;
   char	accept, reject, done, code1, code2;

   result = 0;
   if ( XD_Out_Codes(X[0],Y[0],window) != 0) result |= 1;
   if ( XD_Out_Codes(X[1],Y[1],window) != 0) result |= 2;

   accept = FALSE;
   reject = FALSE;
   done   = FALSE;
   swaps  = 0;

   do {
      code1 = XD_Out_Codes( X[0], Y[0], window );
      code2 = XD_Out_Codes( X[1], Y[1], window );

      if ((reject = (code1&code2) != 0)) {
	 done = TRUE;
	 }
      else if ((accept = (code1|code2) == 0)) {
	 done = TRUE;
	 }
      else {
	 if (code1 == 0) {
	    temp = X[0];
	    X[0] = X[1];
	    X[1] = temp;
	    
	    temp = Y[0];
	    Y[0] = Y[1];
	    Y[1] = temp;
	    
	    temp  = code1;
	    code1 = code2;
	    code2 = temp;

	    swaps += 1;
	    }

	 if ( BIT_TEST(code1,0) ) {
	    X[0] = X[0] + (X[1]-X[0])*(window[BOTTOM]-Y[0])/(Y[1]-Y[0]);
	    Y[0] = window[BOTTOM];
	    }

	 else if ( BIT_TEST(code1,1) ) {
	    X[0] = X[0] + (X[1]-X[0])*(window[TOP]-Y[0])/(Y[1]-Y[0]);
	    Y[0] = window[TOP];
	    }

	 else if ( BIT_TEST(code1,2) ) {
	    Y[0] = Y[0] + (Y[1]-Y[0])*(window[RIGHT]-X[0])/(X[1]-X[0]);
	    X[0] = window[RIGHT];
	    }

	 else {
	    Y[0] = Y[0] + (Y[1]-Y[0])*(window[LEFT]-X[0])/(X[1]-X[0]);
	    X[0] = window[LEFT];
	    }
	 }
      } while (!done);

   if (!accept) {
      result = -1;
      }
   else {
      if ( BIT_TEST(swaps,0) ) {
	 temp = X[0];
	 X[0] = X[1];
	 X[1] = temp;
	 
	 temp = Y[0];
	 Y[0] = Y[1];
	 Y[1] = temp;
	 }
      }

   return (result);
   }

FUNCTION int XD_Out_Codes( X, Y, Box )
int	X, Y, *Box;

   {
   int	result;

   result = 0;
   if (Y > Box[BOTTOM]) result |= 1;
   if (Y < Box[TOP]   ) result |= 2;
   if (X > Box[RIGHT] ) result |= 4;
   if (X < Box[LEFT]  ) result |= 8;

   return (result);
   }
