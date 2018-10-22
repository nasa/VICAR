 /**************************************************************

	TEST PROGRAM FOR PDS/VICAR GMASK INTERFACE


	This program is written to generate mask data based on 
	VICAR inputs to verify against mask data based on PDS
	inputs.

	GMASK routines exercised in the test program for comparison
	to PDS based routines are

		ZASTOREIMAGE
		ZACALCULATEHIST

	This test program is called in the test PDS TSTVICAR.PDF.


	Author:		Justin McNeill
	Date:		August 1993
	Revisions:	None
			
******************************************************************/

#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"

#include <stdlib.h>
#include <math.h>
#include <string.h>


main44()
{
   int x, y, d, i, length ;
   int inunit[10], status, count, current_file, NS, NL ;
   int flag, dummy[50], min, max ;
   float dmy[80], lsat, hsat, iscale, oscale, mean, sigma;
   unsigned int temphist2[256],temphist1[256],temphist[256], maxdn, maximum ;
   unsigned int *halfhist;
   unsigned char *Input;
   unsigned char *Buffer;
   char file[300],string[100],str[100];
   int grey = 128;
   int indices[100], lengths[100];
   int maximum_frequency, total_points;

   float low, hi;

   low = 20.0;
   hi = 220.0;

   status = zvp("INP", file, &count) ; /* Get input files		*/
   zvsptr(file,count,indices,lengths);

   for(current_file=1;current_file<=count;current_file++)
	status = zvunit ( &inunit[current_file-1], "INP", current_file,0 ) ;

   strncpy(string,&file[0],lengths[0]);
   string[lengths[0]] = '\0';

   zasetdim( 1 );			/* Set BW mode	*/

   zainitialize( 512, 512, &grey ) ; 	/* Initialization for GMASK 	*/
 
/* Store a monochrome image at location (100,100)			*/
   zastoreimage( inunit[0], 1, 1, 312, 312, 100, 100, 
			'N', &low , &hi, 0, 0 ) ;	

   zacalculatehist( inunit[0], 1, 1, 312, 312, 1, 1, temphist, 
			&maximum_frequency, &total_points, 
			'F', &low, &hi, 0, 0 );

   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZACALCULATEHIST routine for VICAR image\n"," ");
   zvmessage("********************************************************\n "," ");

   sprintf(str,"Histogram of the file %s\n",string);
   zvmessage(str," ");
   sprintf(str,"\tDN with maximum frequency is %d",maximum_frequency);
   zvmessage(str," ");
   sprintf(str,"\tTotal number of points used in histogram calculation is %d\n",
		total_points);
   zvmessage(str," ");
   zvmessage("\tOnly non-zero values of histogram are printed below\n"," ");

   for ( x=0; x<256; x++ )
	if ( temphist[x] > 0 )
		{
		sprintf(str,"\thistogram element %d equals %d",x+1,temphist[x]);
		zvmessage(str," ");
		}

   zacopymask(' ') ;                     
}
