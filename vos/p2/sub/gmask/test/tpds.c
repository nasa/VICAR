 /**************************************************************

	TEST PROGRAM FOR PDS/VICAR GMASK INTERFACE


	GMASK routines exercised in the test program are 

		ZASTOREPDSIMAGE
		ZACALCULATEPDSHIST
		ZANAMEMASKFILE		

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
   int status, count, current_file ;
   int flag, dummy[50], min, max ;

   unsigned int	temphist[256], maxdn, maximum ;

   char file[300],string[100],str[100];

   int grey = 128;
   int black = 0;

   int indices[100], lengths[100];
   int maximum_frequency, total_points;

   float low, hi;

   low = 20.0;
   hi = 220.0;

   status = zvp("INP", file, &count) ; /* Get input files		*/
   zvsptr(file,count,indices,lengths);

   strncpy(string,&file[0],lengths[0]);
   string[lengths[0]] = '\0';
 
   zasetdim( 1 );			/* Set BW mode	*/

   zainitialize( 0, 0, &grey ) ; 	/* Initialization for GMASK 	*/

/* Store a monochrome image at location (100,100)			*/

   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZASTOREPDSIMAGE routine\n"," ");
   zvmessage("********************************************************\n "," ");

   sprintf(str,"\tImage file %s stored at location (100,100)",string);
   zvmessage(str," ");
   zvmessage("\tin mask output file.\n"," ");
   sprintf(str,"\tExpect background DN of value %d.",grey);
   zvmessage(str," ");
   zvmessage(" "," ");

   zastorePDSimage( string, 1, 1, 312, 312, 100, 100, 'N', &low , &hi, 0, 0 );	
	
   zacalculatePDShist( string, 1, 1, 312, 312, 1, 1, temphist, 
		&maximum_frequency, &total_points, 'F', &low, &hi, 0, 0 );

   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZACALCULATEPDSHIST routine\n"," ");
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

   strcpy(str,"NAMED.FILE");
   zanamemaskfile(str);

   zvmessage(" "," ");
   sprintf(string,"The output mask file name is %s.",str);
   zvmessage(string," ");
   zvmessage("********************************************************\n "," ");

   zacopymask(' ');                     
}
