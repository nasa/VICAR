/**************************************************************
 *
 *	Black/white test program for XAOUTSIZE and inverse stretch
 * 	of XASTOREIMAGE and XAZOOMIMAGE
 *
 * 	Author :	Justin McNeill 
 *	Date :	 	January 1991
 * 
 *      April 7,  1993	REVISION  Florance Moss
 *      Replaced xv calls with zv calls, xa calls with za calls.
 *      Removed XASETLANGIF call.
 *      Removed ZVSPTR call.
 *      Converted optional arguments in routines ZACOPYMASK 
 *      ZASTOREIMAGE, ZAZOOMIMAGE to required arguments due to
 *      NARGS is not portable.
 *      Pass address instead of value for background DN.
 *      Fix error while call ZACALCULATEHIST.
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
   int backgrounddn ;
   float low, hi;
   int white = 255;

   zasetdim( 1 );			/* Set BW mode	*/

   backgrounddn = 90;
   zainitialize( 0, 0, &backgrounddn ) ; 		/* Initialization for GMASK 	*/

   backgrounddn = 180;
   zaoutsize( 512, 512, &backgrounddn ) ;		/* Set mask size at 512x512	*/

   status = zvp("INP",file,&count);		/* Get input files		*/
   for(current_file=1;current_file<=count;current_file++)
	status = zvunit ( &inunit[current_file-1], "INP", current_file, 0) ;

   /* Store an inversely stretched image at location (1,1)		*/
   low = 50.0;
   hi = 10.0;
   zastoreimage( inunit[0], 1, 1, 100, 100, 1, 1, 'F', &low, &hi, 0, 0 ) ;	

   /* Calculate and store histogram of inverse stretch */
   zacalculatehist( inunit[0], 1, 1, 100, 100, 1, 1, temphist, &max,
    	&numpoints, 'F', &low, &hi, 0, 0 );
   zastorehistogram( temphist, 100, 1, 200, 100, max, "H", &white );

   /* Store a zoomed and inverse stretched ramp at location (101,101) 	*/
   zazoomimage( inunit[0], 1, 1, 50, 50, 101 , 101, 2,"Interpolation",
	'F', &low , &hi, 0, 0);

   flag = zvptst("FLAG");
   if (flag)
     zacopymask('m') ;   
   else
     zacopymask(' ') ;                     
}
