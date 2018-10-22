/**************************************************************
 *
 *	Test program used as a comparison of STRETCH.COM output
 * 	to GMASK stretch outputs
 *
 * 	Author :	Justin McNeill 
 *	Date :	 	January 1991
 *
 *      April 7,  1993	REVISION    Florance Moss
 *      Replaced xv calls with zv calls, xa calls with za calls.
 *      Removed XASETLANGIF call.
 *      Removed ZVSPTR call.
 *      Converted optional arguments in routines ZACOPYMASK 
 *      ZASTOREIMAGE, ZAZOOMIMAGE to required arguments due to
 *      NARGS is not portable.
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
   int elength, ethick, cthick, null, array_length ;
   int len[10], thick[10], rgb, numpoints ;
   double period[10][2], scale ;
   int inunit[10], status, count, current_file, NS, NL ;
   int flag, dummy[50], min, max ;
   float dmy[80], lsat, hsat, iscale, oscale, mean, sigma;
   unsigned int temphist2[256],temphist1[256],temphist[256], maxdn, maximum ;
   unsigned int *halfhist;
   unsigned char *Input;
   unsigned char *Buffer;
   char file[180],string[50];
   int black = 0;
   int grey = 128;
   int white = 255;
   int backgrounddn;
   float wht = 255.0;
   float blk = 0.0;

   zasetdim( 1 );   /* Set BW mode	*/

   backgrounddn = 90;
   zainitialize( 0, 0, &backgrounddn ) ; /* Initialization for GMASK 	*/

   backgrounddn = 180;
   zaoutsize( 512, 512, &backgrounddn) ; /* Set mask size at 512x512 	*/

   status = zvpcnt("INP",&count);		/* Get input files		*/
   for(current_file=1;current_file<=count;current_file++)
	status = zvunit ( &inunit[current_file-1], "INP", current_file, 0 ) ;

   /* Store a monochrome image at location (1,1)			*/
   zastoreimage( inunit[0], 1, 1, 100, 100, 1, 1, 'N', &blk, &wht, 0, 0 ) ;	

   /* Calculate and store histogram of inverse stretch */
   zacalculatehist( inunit[0], 1, 1, 100, 100, 1, 1, temphist, &max,
    	&numpoints, 'n', &blk, &wht, 0, 0 );
   zastorehistogram( temphist, 100, 1, 200, 100, max, "H", &white );

   /* Store zoomed image at location (101,101) 				*/
   zazoomimage( inunit[0], 1, 1, 50, 50, 101, 101, 2, "Interpolation",
	'N', &blk, &wht, 0, 0);

   flag = zvptst("FLAG");
   if (flag)
     zacopymask('m') ;   
   else
     zacopymask(' ') ;                     
}
