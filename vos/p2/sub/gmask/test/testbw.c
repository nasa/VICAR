 /**************************************************************
 *
 *	Black/white test program for the GMASK routines
 *
 * 	Author :	Justin McNeill 
 *	Date :	 	April 1990
 *
 *      April 7,  1993	REVISION     Florance Moss
 *      Replaced xv calls with zv calls, xa calls with za calls.
 *      Removed XASETLANGIF call.
 *      Removed ZVSPTR call.
 *      Converted optional arguments in routines ZACOPYMASK 
 *      ZASTOREIMAGE, ZAZOOMIMAGE to required arguments due to
 *      NARGS is not portable.
 *      Use address instead of value for DN.
 *      Use &white instead of &0 throughout the code.
 *      Use zhistat2 & zhiscale instead of histat2, & hiscale.
 *      (change their calling arguments accordingly).
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
   int backgrounddn ;
   struct AnotationType
   {
      unsigned char Anotation_Position ;
      double Anotation_Start_Value ;
      double Anotation_Increment ;
      double Anotation_Modulus ;
      int Anotation_Significant_Digits ;
      int Anotation_Size ;
      unsigned char Anotation_Orientation ;
      unsigned char Anotation_Justification ;
   }Anotation[50] ;

   float low, hi;

   zasetdim( 1 );			/* Set BW mode	*/

   backgrounddn = 90;
   zainitialize( 1024, 2560, &backgrounddn ) ; 	/* Initialization for GMASK 	*/

   status =  zvp("INP", file, &count) ; /* Get input files		*/

   for(current_file=1;current_file<=count;current_file++)
	status = zvunit ( &inunit[current_file-1], "INP", current_file,0 ) ;

   /* Draw box with zaSTOREVECTOR at (1, 1) */
   zastorevector( 1, 1, 480, 1, 1, &white) ;
   zastorevector( 1, 1, 1, 480, 1, &white ) ;
   zastorevector( 480, 1, 480, 480, 1, &white) ;
   zastorevector( 1, 480, 480, 480, 1, &white) ;

   /* Draw a circle and ellipses or something like a globe */
   zastorellipse( 400, 400, 30, 30, 30, &black );
   zastorellipse( 400, 400, 30, 30, 1, &white );
   zastorellipse( 400, 400, 30, 7, 1, &white );
   zastorellipse( 400, 400, 30, 14, 1, &white );
   zastorellipse( 400, 400, 30, 22, 1, &white );
   zastorellipse( 400, 400, 7, 30, 1, &white );
   zastorellipse( 400, 400, 14, 30, 1, &white );
   zastorellipse( 400, 400, 22, 30, 1, &white );
   zastorevector( 400, 370, 400, 429, 1, &white );
   zastorevector( 370, 400, 429, 400, 1, &white );
   
   /* Test zaSTOREBUFFER at (512, 1) */
   status = zvopen(inunit[4], "OP","READ","OPEN_ACT", "SA","IO_ACT","SA",	
	           "U_FORMAT","BYTE","O_FORMAT","BYTE",0);
   status = zvget( inunit[4],"NL", &NL, "NS", &NS, 0 );
   Input = (unsigned char *)calloc(NS,sizeof(unsigned char));   
   Buffer = (unsigned char *)calloc(NL*NS,sizeof(unsigned char));   
   for( x=0; x<NL; x++ )
	{
   	status = zvread( inunit[4], Input, 0 );   
	for( y=0; y<NS; y++ )
		Buffer[x*NS + y] = Input[y];
	}
   free( Input );
   status = zvclose( inunit[4], 0 );

   zastorebuffer( Buffer, 1, 1, 180, 180, 512, 1, 1, "BW");	
   zastorebuffer( Buffer, 1, 1, 180, 180, 692, 1, -2, "BW");
   zastorebuffer( Buffer, 1, 1, 180, 180, 512, 512, 2, "BW");
   zastorebuffer( Buffer, 1, 1, 180, 180, 512, 190, -1, "BW");
   free( Buffer );

   zastorebox( 1, 1, 480, 480, &grey);	/* Store a box */

   for ( i = 0 ; i < 256 ; i += 32 )    /* Store gray scale boxes */
      {
      if ( i == 256 )
         i == 255 ;

      rgb = i;

      zastorebox( 200+i, 10, 32, 32, &rgb ) ;
      zastorebox( 300, 150+i, 32, 32, &rgb ) ;

      rgb = 255 - i;

      zastorebox( 200+i, 50, 32, 32, &rgb ) ;
      zastorebox( 350, 150+i, 32, 32, &rgb ) ;
      }

   /* Store gray scale */
   zastoregray( 10, 1, 512, 40, 0, 255, 'c' ) ;
   zastoregray( 60, 1, 512, 40, 255, 0, 'c' ) ;
   zastoregray( 110, 70, 350, 40, 0, 255, 's' ) ;
   zastoregray( 160, 70, 350, 40, 255, 0, 's' ) ;

   zastoretick( 1, 1, 1, 480, 1, 5, 5, &white) ;	/* Store tick marks */
   zastoretick( 1, 1, 480, 1, 1, 5, 5, &white) ;
   zastoretick( 480-5, 1, 480-5, 480, 1, 5, 5, &white) ;
   zastoretick( 1, 480-5, 480, 480-5, 1, 5, 5, &white) ;

   /* Print strings at various angles in various fonts in various colors */
   zasetfontangle( 90 );
   zastorestring( 50, 25, 2, &white, "This is the regular font.", 1 ) ;
   zasetfontangle( 0 );
   zastorestring( 200, 50, 4, &white, "This is the regular font.", 1 ) ;
   zasetfont( "HERSHEY", 3 ) ;
   zastringlength ( "THIS IS A TEST", 3, 3, &length ) ;
   printf("THIS IS A TEST");
   printf("\nThe string length of the above Hershey 3 Font = %d",length );

   zasetcharspacing( 5 );
   zastorestring( 230, 50, 2, &white, "Hershey #3 font (5sp)", 2 ) ;
   zasetfont( "DEFAULT", 0 ) ;
   zastringlength ( "THIS IS A TEST", 3, 1, &length ) ;
   printf("\nTHIS IS A TEST");
   printf("\nThe string length of the above Block Font = %d",length );

   zasetcharspacing( 1 );
   zastorestring( 260, 50, 4, &white,"This is a change back to regular.", 1) ;
   zasetfont( "HERSHEY", 5 ) ;

   zasetcharspacing( -1 );
   zastorestring( 300, 50, 2, &white, "This is Hershey #5 font.", 2 ) ;

   zasetfont( "DEFAULT", 5 ) ;

   /* Store string at location (50,850)					*/
   zastorestring( 50, 850, 3, &white, "ZOOMS & SHRINKS", 1 );

   /* Store a color image at location (1,512)				*/
   low = 0.0;
   hi = 255.0;
   zazoomimage( inunit[0], 100, 200, 200, 300, 1, 512, -1, "Re",
	'N', &low , &hi, 0, 0 ) ;	

   /* Store zoomed by replicaton color image at location (102,512)	*/
   zazoomimage( inunit[0], 100, 200, 200, 300, 102 , 512, 2, "Re",
	'N', &low , &hi, 0, 0);

   /* Store a monochrome image at location (1,713)			*/
   zastoreimage( inunit[0], 100, 200, 200, 300, 1, 612, 'N', &low , &hi, 0, 0 ) ;	
   zazoomimage( inunit[0], 100, 200, 200, 300, 1 , 713, 3,"Interpolation",
	'N', &low , &hi, 0, 0);
   zazoomimage( inunit[0], 100, 200, 200, 300, 1 , 713, 3,"re",'N', &low, &hi, 0, 0);

   /* Store a monochrome image at location (310,512)			*/
   zastoreimage( inunit[0], 100, 200, 200, 300, 310, 512, 'N', &low , &hi, 0, 0 ) ;	
   zazoomimage( inunit[0], 100, 200, 200, 300, 310 , 612, -2,"Interpolation",
	'N', &low , &hi, 0, 0);

   /* Store a color image at location (310,712)				*/
   zastoreimage( inunit[0], 1, 200, 200, 400, 310, 712, 'N', &low , &hi, 0, 0 ) ;	

   /* Store zoomed by interpolation color image at location (310,912)	*/	
   zazoomimage( inunit[0], 1, 200, 200, 400, 310 , 912, -4, "Interpolation",
	'N', &low , &hi, 0, 0);

   /* Store zoomed by interpolation color image at location (310,912)	*/	
   zazoomimage( inunit[0], 1, 200, 200, 400, 360 , 912, -3, "re",
	'N', &low , &hi, 0, 0.);

   /* Store a color image at location (1,1024)				*/
   zastoreimage( inunit[1], 1, 1, 512, 512, 1, 1024, 'N', &low , &hi, 0, 0 ) ;	
  
   /* Calculate histogram of fourth input file				*/
   zacalculatehist( inunit[1], 1, 1, 512, 512, 1, 1, temphist, &maxdn, 
		    &numpoints, 'n', &low, &low, dummy, 0 );

   /* Print heading for image   */
   zastorestring( 20, 1050, 5, &white, "B52:", 1 );

   /* Store vertically oriented histogram at location (50,1050) 	*/
   zastorehistogram( temphist, 50, 1050, 305, 1250, maxdn, "V", &black);

   /* Store horizontally oriented histogram at location (300,1050)     	*/
   zastorehistogram( temphist, 300, 1050, 500, 1305, maxdn, "h", &grey );

   dummy[0] = 0;

   /* Store zoomed image at (1,1536)		*/
   zazoomimage( inunit[0],1,1,512,512,1,1536,-2,"In",'N',&low,&hi,0,0 ) ;	

   /* Store zoomed & stretched image at (1,1792)		*/
   low = 1.0;
   hi = 0.05;
   zazoomimage( inunit[0],1,1,512,512,1,1792,-2,"In",'A',&low, &hi,
	dummy, 1 ) ;	

   /* Store histogram of unstretched image	*/
   low = 0.0;
   hi  = 0.0;
   zacalculatehist(inunit[0],1,1,512,512,1,1,temphist,&maxdn,&numpoints, 
	'n', &low, &hi, dummy, 0);
   zastorehistogram( temphist, 300, 1700, 400, 1955, maxdn, "h", &white); 

   /* Test of HALFWORD display, HALFWORD histograms, and HALFWORD stretched */
   zastorestring(50,2100,2,&white,"shrink halfword",1);
   zastorestring(50,2400,2,&white,"nozoom halfword",1);
   zastorestring(306,2100,2,&white,"zoom-stretch halfword (10K-30K)",1);

   halfhist = (unsigned int *)calloc(65536,sizeof(unsigned int));

   low = 60.0;
   hi = 200.0;
   zacalculatehist(inunit[2],1,1,512,512,1,1,halfhist,&maxdn,&numpoints, 
	'n', &low, &hi, dummy, 0);
   low = 0.0;
   hi = 255.0;
   zazoomimage(inunit[2],1,1,256,256,1,2305,1,"Re",'N',&low,&hi,0,0);
   zazoomimage(inunit[2],1,1,512,512,1,2048,-2,"Re",'N',&low,&hi,0,0);

   zhistat2(halfhist,numpoints,&mean,&sigma,&min,&max,&maxdn);
/*   getscale( &2, dmy, &max, &iscale, &oscale, &status );   */
   iscale = 1.0;
   oscale = 128.0;
   iscale /= oscale;
   zhiscale( halfhist, numpoints, iscale, temphist, &lsat, &hsat );
   zastorehistogram( temphist, 156, 2048, 256, 2304, maxdn, "h", &grey );

   low = 10000.0;
   hi =  30000.0;
   zacalculatehist(inunit[2],1,1,512,512,1,1,halfhist,&maxdn,&numpoints, 
	'f', &low, &hi, dummy, 0);
   zazoomimage(inunit[2],1,1,512,512,256,2048,-2,"in",'f',&low,&hi,0, 0);
   zhistat2(halfhist,numpoints,&mean,&sigma,&min,&max,&maxdn);
   /*   getscale( &2, dmy, &max, &iscale, &oscale, &status );   */
   iscale = 1.0 ;
   oscale = 128.0 ;
   iscale /= oscale;
   zhiscale( halfhist, numpoints, iscale, temphist, &lsat, &hsat );
   zastorehistogram( temphist, 412, 2048, 512, 2304, maxdn, "h", &grey );

   free(halfhist); 
   zasetfont( "HERSHEY", 3 ) ;   
   zasetfont( "DEFAULT", 0 ) ;  

   elength = 0 ;
   ethick  =  0;   
   cthick  =  12 ;
   null = 0 ;

   zadefinescale( 'C', &elength, &ethick, &cthick, null ) ;

   len[0] = 140;
   len[1] = 84;
   len[2] = 56;
   len[3] = 56;

   thick[0] = 12 ;
   thick[1] = 12 ;
   thick[2] = 12 ;
   thick[3] = 12 ;

   period[0][0] = 500.0 ;
   period[1][0] = 100.0 ;
   period[2][0] =  50.0 ;
   period[3][0] =  10.0 ; 

   period[0][1] =  0.0 ;
   period[1][1] =  0.0 ;
   period[2][1] =  0.0 ;
   period[3][1] =  0.0 ;

   array_length = 3 ;

   Anotation[0].Anotation_Start_Value = -100.0 ;
   Anotation[0].Anotation_Increment = 100.0 ;
   Anotation[0].Anotation_Significant_Digits = 0 ;
   Anotation[0].Anotation_Size = 15 ;

   Anotation[0].Anotation_Orientation = 'V' ;
   Anotation[0].Anotation_Justification = 'R' ;

   Anotation[1].Anotation_Position = 0  ;
   Anotation[2].Anotation_Position = 0  ;
   Anotation[3].Anotation_Position = 0  ;

   Anotation[0].Anotation_Position ='T'  ;

   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 

   zascale( 520, 1024, 520, 2048, &white) ;   
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zascale( 520, 1024, 520, 2048, &white) ;   

   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zascale( 520, 1024, 520, 2048, &white) ;   

   zaclearscale();
   zadefinescale( 'C', &elength, &ethick, &cthick, null ) ;

   Anotation[0].Anotation_Justification = 'R' ;
   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 1000, 1024, 1000, 2048, &white) ;   
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 1000, 1024, 1000, 2048, &white) ;   
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 1000, 1024, 1000, 2048, &white) ;   

   elength = 0 ;
   ethick  =  0;   
   cthick  =  1 ;
   null = 0 ;

   zaclearscale();
   zadefinescale( 'C', &elength, &ethick, &cthick, null ) ;

   len[0] = 10;
   len[1] = 8;
   len[2] = 6;
   len[3] = 4;

   thick[0] = 1 ;
   thick[1] = 1 ;
   thick[2] = 1 ;
   thick[3] = 1 ;

   period[0][0] = 30.0 ;
   period[1][0] = 15.0 ;
   period[2][0] =  10.0 ;
   period[3][0] =  5.0 ; 

   period[0][1] =  0.0 ;
   period[1][1] =  0.0 ;
   period[2][1] =  0.0 ;
   period[3][1] =  0.0 ;

   array_length = 3 ;

   Anotation[0].Anotation_Start_Value = 36.0 ;
   Anotation[0].Anotation_Increment = 30.0 ;
   Anotation[0].Anotation_Significant_Digits = 0 ;
   Anotation[0].Anotation_Size = 2 ;
   Anotation[0].Anotation_Modulus = 35.0;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zascale( 670, 1400, 890, 1400, &white) ;   

   flag = zvptst("FLAG");
   if (flag)
     zacopymask('m') ;   
   else
     zacopymask(' ') ;                     
}
