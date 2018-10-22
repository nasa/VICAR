 /**************************************************************

	TEST PROGRAM FOR PDS/VICAR GMASK INTERFACE


	GMASK routines exercised in the test program are 

		ZAZOOMPDSIMAGE
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

#define NUMBER_OF_FILES 10

main44()
{
   int x, y, d, i, length ;
   int inunit[10], status, count, current_file, NS, NL ;
   unsigned int temphist[256], maxdn, maximum ;
   unsigned char *Input;
   unsigned char *Buffer;
   char file[300],string[100],str[100];
   int grey = 128;
   int flag, dummy[50], min, max ;
   int indices[100], lengths[100];
   int low_array[NUMBER_OF_FILES], high_array[NUMBER_OF_FILES];

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
   zazoomPDSimage( string, 1, 1, 156, 156, 100, 100, 
			2, "REPLICATION",'F', &low , &hi, 0, 0 ) ;	

   zacopymask(' ') ;                     

   low = 1.0;
   hi = 0.001;

   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZAZOOMPDSIMAGE routine\n"," ");
   zvmessage("********************************************************\n "," ");

   zainitialize( 512, 512, &grey ) ; 	/* Initialization for GMASK 	*/
 
/* Store a monochrome image at location (100,100)			*/
   zazoomPDSimage( string, 1, 1, 156, 156, 100, 100, 
			2, "REPLICATION",'A', &low , &hi, 0, 0 ) ;	

   zadeterminelimit( &low_array, &high_array );
 

   zvmessage("********************************************************\n "," ");
   zvmessage("\tTest of ZADETERMINELIMIT routine with PDS images\n"," ");
   zvmessage("********************************************************\n "," ");

   sprintf( string,"\tLOW_DN = %d  HIGH_DN = %d",low_array[0],high_array[0] );
   zvmessage( string , " ");

   zacopymask(' ') ;                     

}
