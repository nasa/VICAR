 /**************************************************************

	TEST PROGRAM FOR PDS/VICAR GMASK INTERFACE


	This program is written to generate mask data based on 
	VICAR inputs to verify against mask data based on PDS
	inputs.

	GMASK routines exercised in the test program for comparison
	to PDS based routines are

		ZAZOOMIMAGE
		ZADETERMINELIMIT

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
   int low_array[10], high_array[10];

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
 
   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZAZOOMIMAGE routine\n"," ");
   zvmessage("********************************************************\n "," ");

/* Store a monochrome image at location (100,100)			*/
   zazoomimage( inunit[0], 1, 1, 156, 156, 100, 100, 
			2, "REPLICATION",'F', &low , &hi, 0, 0 ) ;	

   zacopymask(' ') ;                     

   low = 1.0;
   hi = 0.001;

   zainitialize( 512, 512, &grey ) ; 	/* Initialization for GMASK 	*/

/* Store a monochrome image at location (100,100)			*/
   zazoomimage( inunit[0], 1, 1, 156, 156, 100, 100, 
			2, "REPLICATION",'A', &low , &hi, 0, 0 ) ;	

   zadeterminelimit( &low_array, &high_array );
 
   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZADETERMINELIMIT routine\n"," ");
   zvmessage("********************************************************\n "," ");

   sprintf( string,"\tLOW_DN = %d  HIGH_DN = %d", low_array[0], high_array[0] );
   zvmessage( string , " ");

   zacopymask(' ') ;                     
}
