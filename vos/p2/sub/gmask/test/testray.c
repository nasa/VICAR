/**************************************************************
 *
 *	&black/&white ray/vector test program
 *
 * 	Author :	Justin McNeill 
 *	Date :	 	March 1991
 *
 *      April 7,  1993	REVISION    Florance Moss
 *      Replaced xa calls with za calls.
 *      Removed XASETLANGIF call.
 *      Converted optional argument in routine ZACOPYMASK 
 *      to required argument due to NARGS is not portabel.
 *      Pass address instead of value for background DN.
 **************************************************************/

#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"

#include <stdlib.h>
#include <math.h>
#include <string.h>

main44()
{
   int x, y, d, i, length ;
   float angle;
   char file[180],string[50];
   int black = 0;
   int grey = 128;
   int white = 255;
   int endline[50], endsample[50], stringlength;

   zasetdim( 1 );			/* Set BW mode	*/

   zainitialize( 256,256,&black ) ; /* Initialization for GMASK 	*/
 
   for( x=0; x<16; x++ )
	{
	angle = x * 22.5;		
	zastoreray(128,128,&angle,100,1,&white,string,&endline[x],&endsample[x]);
	}

   zacopymask(' ');

   zainitialize( 256,256,&black ) ; /* Initialization for GMASK 	*/
 
   for( x=0; x<16; x++ )
	   zastorevector( 128, 128, endline[x], endsample[x], 1, &white );

   zacopymask(' ');
}
