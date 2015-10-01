/*	XD_Intialize - General XD initialization routine
 *
 *	Purpose:
 *
 *	Written by:  R. A. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Intialize()
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#define	XD_INITIALIZE

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "xdalloc.h"


int XD_Initialize()

   {
   int	i, status;
   
   status = XD_Read_DIBs();
   if (status == SUCCESS) {
      
      FONT_COLOR     = 255;
      FONT_MASK	  = 255;
      FONT_PRECISION = 1;
      FONT_NUMBER    = 0;
      FONT_HEIGHT    = 20;
      FONT_COSINE    = 1.0;
      FONT_SINE      = 0.0;
      FONT_SCALE     = 1.0;
      for (i=0; i<128; i++) {
	 xd_font_info.ptrX[i] = 0;
	 xd_font_info.ptrY[i] = 0;
	 xd_font_info.ptrMD[i] = 0;
	 }
      status = XD_Read_Font();
      }

   if (status == SUCCESS) {
      xd_initialized = TRUE;
      }
   
   return (status);
   }
