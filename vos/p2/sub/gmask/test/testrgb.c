/**************************************************************
 *
 *	Color test program for the GMASK routines
 *
 * 	Author :	Justin McNeill 
 *	Date :	 	April 1990
 *
 *      April 7,  1993	REVISION    Florance Moss
 *      Replaced xv calls with zv calls, za calls with za calls.
 *      Removed XASETLANGIF call.
 *      Removed ZVSPTR call.
 *      Converted optional arguments in routines ZACOPYMASK 
 *      ZASTOREIMAGE, ZAZOOMIMAGE to required arguments due to
 *      NARGS is not portable.
 *      Use zhistat2 & zhiscale instead of histat2, & hiscale.
 *      (change their calling arguments accordingly).
 *      Use &blk instead of &0.0 (etc).
 *      Use white[0] = 255; white[1] = 255; white[2] = 255; 
 *      instead of int white[] = {255,255,255}.
 *      Add 0.5 to temphist, temphist1, temphist2 to eliminate 
 *      the round up discrepancy betwn 2 machines, because
 *      of this change, there will be differences on gmaskrgbc.dat
 *      on old gmask and ported gmask on VAX.
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
   int len[10], thick[10], rgb[3], numpoints ;
   double period[10][2], scale ;
   int inunit[10], status, count, current_file, NS, NL ;
   int flag, dummy[50], min, max ;
   float dmy[80], lsat, hsat, iscale, oscale, mean, sigma;
   unsigned int temphist2[256],temphist1[256],temphist[256], maxdn, maximum ;
   unsigned int *halfhist;
   unsigned char *Input;
   unsigned char *Buffer;
   char file[180],string[50];
   int black[3];
   int white[3];
   int violet[3];
   int brown[3];
   int purple[3];
   int yellow[3];
   int orange[3];
   int blue[3];
   int green[3];
   int red[3];
   float wht = 255.0;
   float blk = 0.0;
   float sixty = 60.0;
   float twohundred = 200.0;
   float low = 10000.0;
   float hi  = 30000.0;
   int blki = 0;
   
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

   black[0]= 0;
   black[1]= 0;
   black[2]= 0;
   white[0]=255;
   white[1]=255;
   white[2]=255;
   violet[0]=160;
   violet[1]=80;
   violet[2]=255;
   brown[0]=100;
   brown[1]=60;
   brown[2]=40;
   purple[0]=100;
   purple[1]=0;
   purple[2]=150;
   yellow[0]=255;
   yellow[1]=255;
   yellow[2]=130;
   orange[0]=255;
   orange[1]=128;
   orange[2]=0;
   blue[0]=0;
   blue[1]=0;
   blue[2]=255;
   green[0]=0;
   green[1]=255;
   green[2]=0;
   red[0]=255;
   red[1]=0;
   red[2]=0;

   zasetdim( 3 );			/* Set color mode	  */

   for(x=0;x<3;x++) 			/* medium grey */
	rgb[x] = 90;

   zainitialize( 1024, 3584, rgb ) ; 	/* Initialization for GMASK 	*/

   status = zvp("INP",file,&count);		/* Get input files		*/
   for(current_file=1;current_file<=count;current_file++)
	status = zvunit ( &inunit[current_file-1], "INP", current_file, 0) ;

   zaband( inunit[0], 1 );	/* Assign input files to output bands  	*/
   zaband( inunit[1], 2 );
   zaband( inunit[2], 3 );

   zaband( inunit[5], 1 );	/* Assign input files to output bands  	*/
   zaband( inunit[6], 2 );
   zaband( inunit[7], 3 );

   /* Draw box with zaSTOREVECTOR at (1, 1) */
   zastorevector( 1, 1, 480, 1, 1, orange) ;
   zastorevector( 1, 1, 1, 480, 1, orange ) ;
   zastorevector( 480, 1, 480, 480, 1, orange ) ;
   zastorevector( 1, 480, 480, 480, 1, orange ) ;

   /* Draw a circle and ellipses or something like a globe */
   zastorellipse( 400, 400, 30, 30, 30, blue );
   zastorellipse( 400, 400, 30, 30, 1, white );
   zastorellipse( 400, 400, 30, 7, 1, white );
   zastorellipse( 400, 400, 30, 14, 1, white );
   zastorellipse( 400, 400, 30, 22, 1, white );
   zastorellipse( 400, 400, 7, 30, 1, white );
   zastorellipse( 400, 400, 14, 30, 1, white );
   zastorellipse( 400, 400, 22, 30, 1, white );
   zastorevector( 400, 370, 400, 429, 1, white );
   zastorevector( 370, 400, 429, 400, 1, white );
   
   /* Test zaSTOREBUFFER at (512, 1) */
   status = zvopen( inunit[8], "OP","READ","OPEN_ACT", "SA","IO_ACT","SA",	
	            "U_FORMAT","BYTE","O_FORMAT","BYTE", 0);
   status = zvget( inunit[8], "NL", &NL, "NS", &NS, 0 );
   Input = (unsigned char *)calloc(NS,sizeof(unsigned char));   
   Buffer = (unsigned char *)calloc(NL*NS,sizeof(unsigned char));   
   for( x=0; x<NL; x++ )
	{
   	status = zvread( inunit[8], Input, 0);   
	for( y=0; y<NS; y++ )
		Buffer[x*NS + y] = Input[y];
	}
   free( Input );
   status = zvclose( inunit[8], 0);

   zastorebuffer( Buffer, 1, 1, 180, 180, 512, 1, 1, "BW");	
   zastorebuffer( Buffer, 1, 1, 180, 180, 692, 1, -2, "BW");
   zastorebuffer( Buffer, 1, 1, 180, 180, 512, 512, 2, "BW");
   zastorebuffer( Buffer, 1, 1, 180, 180, 512, 190, -1, "BW");
   free( Buffer );

   rgb[0] = 69;	 /* sea blue */
   rgb[1] = 160;
   rgb[2] = 160;

   zastorebox( 1, 1, 480, 480, rgb) ;	/* Store a box */

   for ( i = 0 ; i < 256 ; i += 32 )    /* Store gray scale boxes */
      {
      if ( i == 256 )
         i == 255 ;

      for(d = 0; d < 3; d++)
	    rgb[d] = i;

      zastorebox( 200+i, 10, 32, 32, rgb ) ;
      zastorebox( 300, 150+i, 32, 32, rgb ) ;

      for(d = 0; d < 3; d++)
	    rgb[d] = 255 - i;

      zastorebox( 200+i, 50, 32, 32, rgb ) ;
      zastorebox( 350, 150+i, 32, 32, rgb ) ;
      }

   /* Store gray scale */
   zastoregray( 10, 1, 512, 40, 0, 255, 'c' ) ;
   zastoregray( 60, 1, 512, 40, 255, 0, 'c' ) ;
   zastoregray( 110, 70, 350, 40, 0, 255, 's' ) ;
   zastoregray( 160, 70, 350, 40, 255, 0, 's' ) ;

   rgb[0] = 96;	/* purple */
   rgb[1] = 0;
   rgb[2] = 192;

   zastoretick( 1, 1, 1, 480, 1, 5, 5, rgb ) ;	/* Store tick marks */
   zastoretick( 1, 1, 480, 1, 1, 5, 5, rgb ) ;
   zastoretick( 480-5, 1, 480-5, 480, 1, 5, 5, rgb ) ;
   zastoretick( 1, 480-5, 480, 480-5, 1, 5, 5, rgb ) ;

   rgb[0] = 192; /* light green */
   rgb[1] = 255;
   rgb[2] = 96;

   /* Print strings at various angles in various fonts in various colors */
   zasetfontangle( 90 );
   printf("THE FOLLOWING IS A TEST OF NULL STRING IN zaSTORESTRING");
   y = zastorestring( 20, 25, 2, rgb, "", 1 ) ;
   if( y == (-1) )
	printf("TEST OF NULL STRING IN zaSTORESTRING SUCCEEDED.");
   zastorestring( 50, 25, 2, rgb, "This is the regular font.", 1 ) ;
   zasetfontangle( 0 );
   zastorestring( 200, 50, 4, rgb, "This is the regular font.", 1 ) ;
   zasetfont( "HERSHEY", 3 ) ;
   zastringlength ( "THIS IS A TEST", 3, 3, &length ) ;
   printf("THIS IS A TEST");
   printf("\nThe string length of the above Hershey 3 Font = %d",length );

   zasetcharspacing( 5 );
   zastorestring( 230, 50, 2, red, "Hershey #3 font (5sp)", 2 ) ;
   zasetfont( "DEFAULT", 0 ) ;
   zastringlength ( "THIS IS A TEST", 3, 1, &length ) ;
   printf("\nTHIS IS A TEST");
   printf("\nThe string length of the above Block Font = %d",length );

   zasetcharspacing( 1 );
   zastorestring( 260, 50, 4, orange,"This is a change back to regular.", 1) ;
   zasetfont( "HERSHEY", 5 ) ;

   zasetcharspacing( -1 );
   zastorestring( 300, 50, 2, white, "This is Hershey #5 font.", 2 ) ;

   zasetfont( "DEFAULT", 5 ) ;

   rgb[0] = 25;	/* Lime green */
   rgb[1] = 224;
   rgb[2] = 196;

   /* Store string at location (50,850)					*/
   zastorestring( 50, 850, 3, rgb, "ZOOMS & SHRINKS", 1 );

   /* Store a color image at location (1,512)				*/
   zazoomimage( inunit[0], 100, 200, 200, 300, 1, 512, 1, "In",
	'N', &blk , &wht, 0, 0 ) ;	
   zazoomimage( inunit[1], 100, 200, 200, 300, 1, 512, -1, "Re",
	'N', &blk , &wht, 0, 0 ) ;	
   zazoomimage( inunit[2], 100, 200, 200, 300, 1, 512, -1, "in",
	'N', &blk , &wht, 0, 0 ) ;	
   
   /* Store zoomed by replicaton color image at location (102,512)	*/
   zazoomimage( inunit[0], 100, 200, 200, 300, 102 , 512, 2, "Re",
	'N', &blk , &wht, 0, 0);
   zazoomimage( inunit[1], 100, 200, 200, 300, 102 , 512, 2, "Re",
	'N', &blk , &wht, 0, 0);
   zazoomimage( inunit[2], 100, 200, 200, 300, 102 , 512, 2, "Re",
	'N', &blk , &wht, 0, 0);

   /* Store a monochrome image at location (1,713)			*/
   zastoreimage( inunit[0], 100, 200, 200, 300, 1, 612, 'N', &blk , &wht, 0, 0 ) ;	
   zazoomimage( inunit[0], 100, 200, 200, 300, 1 , 713, 3,"Interpolation",
	'N', &blk , &wht, 0, 0);
   zazoomimage( inunit[0], 100, 200, 200, 300, 1 , 713, 3,"re",'N', &blk, &wht, 0, 0);

   /* Store a monochrome image at location (310,512)			*/
   zastoreimage( inunit[1], 100, 200, 200, 300, 310, 512, 'N', &blk , &wht, 0, 0 ) ;	
   zazoomimage( inunit[1], 100, 200, 200, 300, 310 , 612, -2,"Interpolation",
	'N', &blk, &wht, 0, 0);

   /* Store a color image at location (310,712)				*/
   zastoreimage( inunit[0], 1, 200, 200, 400, 310, 712, 'N', &blk , &wht, 0, 0 ) ;	
   zastoreimage( inunit[1], 1, 200, 200, 400, 310, 712, 'N', &blk , &wht, 0, 0 ) ;	
   zastoreimage( inunit[2], 1, 200, 200, 400, 310, 712, 'N', &blk , &wht, 0, 0 ) ;

   /* Store zoomed by interpolation color image at location (310,912)	*/	
   zazoomimage( inunit[0], 1, 200, 200, 400, 310 , 912, -4, "Interpolation",
	'N', &blk, &wht, 0, 0);
   zazoomimage( inunit[1], 1, 200, 200, 400, 310 , 912, -4, "Interpolation",
	'N', &blk, &wht, 0, 0);
   zazoomimage( inunit[2], 1, 200, 200, 400, 310 , 912, -4, "Interpolation",
	'N', &blk, &wht, 0, 0);

   /* Store zoomed by interpolation color image at location (310,912)	*/	
   zazoomimage( inunit[0], 1, 200, 200, 400, 360 , 912, -3, "re",
	'N', &blk, &wht, 0, 0);
   zazoomimage( inunit[1], 1, 200, 200, 400, 360 , 912, -3, "re",
	'N', &blk, &wht, 0, 0);
   zazoomimage( inunit[2], 1, 200, 200, 400, 360 , 912, -3, "re",
	'N', &blk, &wht, 0, 0);

   rgb[1]= 255; /* bright green */
   rgb[2]= 0;
   rgb[0]= 89;

   /* Store a color image at location (1,1024)				*/
   zastoreimage( inunit[3], 1, 1, 512, 512, 1, 1024, 'N', &blk, &wht, 0, 0 ) ;	
  
   rgb[0] = 255; /* hot pink */
   rgb[1] = 89;
   rgb[2] = 167;

   /* Calculate histogram of fourth input file				*/
   zacalculatehist( inunit[3], 1, 1, 512, 512, 1, 1, temphist, &maxdn, 
		    &numpoints, 'n', &blki, &blki, dummy, 0 );

   /* Print heading for image   */
   zastorestring( 20, 1050, 5, rgb, "B52:", 1 );

   /* Store vertically oriented histogram at location (50,1050) 	*/
   zastorehistogram( temphist, 50, 1050, 305, 1250, maxdn, "V", rgb );

   rgb[2] = 255; /* royal blue */
   rgb[0] = 89;
   rgb[1] = 167;

   /* Store horizontally oriented histogram at location (300,1050)     	*/
   zastorehistogram( temphist, 300, 1050, 500, 1305, maxdn, "h", rgb );

   /* Store color image at (1,1536)			*/
   zazoomimage( inunit[0], 1, 1, 512, 512, 1, 1536, -4, "In", 
		'N', &blk, &wht, 0, 0 ) ;	
   zazoomimage( inunit[1], 1, 1, 512, 512, 1, 1536, -4, "In", 
		'N', &blk, &wht, 0, 0 ) ;	
   zazoomimage( inunit[2], 1, 1, 512, 512, 1, 1536, -4, "In", 
		'N', &blk, &wht, 0, 0 ) ;	

   /* Store color image at (1,2048)			*/
   zazoomimage( inunit[0], 1, 1, 512, 512, 1, 2048, -4, "In", 
		'N', &blk, &wht, 0, 0 ) ;	
   zazoomimage( inunit[1], 1, 1, 512, 512, 1, 2048, -4, "In", 
		'N', &blk, &wht, 0, 0 ) ;	
   zazoomimage( inunit[2], 1, 1, 512, 512, 1, 2048, -4, "In", 
		'N', &blk, &wht, 0, 0 ) ;	

   /* Calculate and store histograms of three color bands 		*/

   zacalculatehist(inunit[0],1,1,512,512,1,1,temphist,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);			
   maximum = maxdn;
   zacalculatehist(inunit[1],1,1,512,512,1,1,temphist1,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maxdn > maximum )	
	maximum = maxdn;
   zacalculatehist(inunit[2],1,1,512,512,1,1,temphist2,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maxdn < maximum )	
	maxdn = maximum;

   /* Red band histogram */
   zastorehistogram(temphist,50,1640,130,1895,maxdn,"h",red);	/* horizontal */
   zastorehistogram(temphist,320,1640,400,1895,maxdn,"hm",red); 
   zastorehistogram(temphist,410,1640,490,1895,maxdn,"hb",red); 
   zastorehistogram(temphist,150,2060,405,2140,maxdn,"v",red);  /* vertical */
   zastorehistogram(temphist,150,2330,405,2410,maxdn,"vm",red);
   zastorehistogram(temphist,150,2420,405,2500,maxdn,"vb",red); 

   /* Green band histogram */
   zastorehistogram(temphist1,140,1640,220,1895,maxdn,"h",green); /* horiz. */
   zastorehistogram(temphist1,320,1640,400,1895,maxdn,"hm",green); 
   zastorehistogram(temphist1,410,1640,490,1895,maxdn,"hb",green); 
   zastorehistogram(temphist1,150,2150,405,2230,maxdn,"v",green); /* vertical */
   zastorehistogram(temphist1,150,2330,405,2410,maxdn,"vm",green); 
   zastorehistogram(temphist1,150,2420,405,2500,maxdn,"vb",green); 

   /* Blue band histogram */
   zastorehistogram(temphist2,230,1640,310,1895,maxdn,"h",blue); /* horizontal */
   zastorehistogram(temphist2,320,1640,400,1895,maxdn,"hm",blue);
   zastorehistogram(temphist2,410,1640,490,1895,maxdn,"hb",blue);
   zastorehistogram(temphist2,150,2240,405,2320,maxdn,"v",blue); /* vertical */
   zastorehistogram(temphist2,150,2330,405,2410,maxdn,"vm",blue); 
   zastorehistogram(temphist2,150,2420,405,2500,maxdn,"vb",blue); 

   /* Store zoomed color image at (1,2560)			*/
   zazoomimage( inunit[0],1,1,512,512,1,2560,-2,"In",'N', &blk , &wht, 0, 0 ) ;	
   zazoomimage( inunit[1],1,1,512,512,1,2560,-2,"In",'N', &blk , &wht, 0, 0 ) ;	
   zazoomimage( inunit[2],1,1,512,512,1,2560,-2,"In",'N', &blk , &wht, 0, 0 ) ;	

   /* Store zoomed & stretched color image at (1,2816)		*/
   zazoomimage( inunit[0],1,1,512,512,1,2816,-2,"In",'f',&sixty, &twohundred, 0, 0 ) ;	
   zazoomimage( inunit[1],1,1,512,512,1,2816,-2,"In",'f',&sixty, &twohundred, 0, 0 ) ;	
   zazoomimage( inunit[2],1,1,512,512,1,2816,-2,"In",'f',&sixty, &twohundred, 0, 0 ) ;	

   /* Store histogram of unstretched image	*/
   zacalculatehist(inunit[0],1,1,512,512,1,1,temphist,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   maximum = maxdn;
   zacalculatehist(inunit[1],1,1,512,512,1,1,temphist1,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;
   zacalculatehist(inunit[2],1,1,512,512,1,1,temphist2,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maximum > maxdn )
	maxdn = maximum;

   zastorehistogram( temphist, 300, 2800, 400, 3055, maxdn, "hm", red ); 
   zastorehistogram( temphist1, 300, 2800, 400, 3055, maxdn, "hm", green ); 
   zastorehistogram( temphist2, 300, 2800, 400, 3055, maxdn, "hm", blue ); 

   /* Store histogram of stretched image	*/
   zacalculatehist(inunit[5],1,1,512,512,1,1,temphist,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   maximum = maxdn;
   zacalculatehist(inunit[6],1,1,512,512,1,1,temphist1,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;
   zacalculatehist(inunit[7],1,1,512,512,1,1,temphist2,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;

   zastorehistogram( temphist, 300, 3200, 500, 3455, maximum, "hm", violet ); 
   zastorehistogram( temphist1, 300, 3200, 500, 3455, maximum, "hm", brown ); 
   zastorehistogram( temphist2, 300, 3200, 500, 3455, maximum, "hm", yellow ); 

   /* Store histogram of unstretched image	*/
   zacalculatehist(inunit[0],1,1,512,512,1,1,temphist,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   maximum = maxdn;
   zacalculatehist(inunit[1],1,1,512,512,1,1,temphist1,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;
   zacalculatehist(inunit[2],1,1,512,512,1,1,temphist2,&maxdn,&numpoints, 
	'n', &blki, &blki, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;
  
   zastorehistogram( temphist, 300, 3200, 500, 3455, maximum, "hm", red ); 
   zastorehistogram( temphist1, 300, 3200, 500, 3455, maximum, "hm", green ); 
   zastorehistogram( temphist2, 300, 3200, 500, 3455, maximum, "hm", blue ); 

   maximum = 0;
   /* Calculate histogram of fixed stretched image	*/
   zacalculatehist(inunit[0],1,1,512,512,1,1,temphist,&maxdn,&numpoints, 
	'f', &sixty, &twohundred, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;
   zacalculatehist(inunit[1],1,1,512,512,1,1,temphist1,&maxdn,&numpoints, 
	'f', &sixty, &twohundred, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;
   zacalculatehist(inunit[2],1,1,512,512,1,1,temphist2,&maxdn,&numpoints, 
	'f', &sixty, &twohundred, dummy, 0);
   if( maximum < maxdn )
	maximum = maxdn;

    scale = 1.0;
   if( maximum > 1.0 )
	{
	scale = 100.0 / log10( (double)maximum );
 	for( x=0; x<256; x++ )
		{
		if( temphist[x] <= 1.0 )
			temphist[x] = 0;
		else
			temphist[x] = scale * log10((double)temphist[x] ) +0.5 ;
		if( temphist1[x] <= 1.0 )
			temphist1[x] = 0;
		else
			temphist1[x] = scale * log10((double)temphist1[x])+0.5;
		if( temphist2[x] <= 1.0 )
			temphist2[x] = 0;
		else
			temphist2[x] = scale * log10((double)temphist2[x] )+0.5;
		}
	}

   zastorehistogram( temphist, 410, 2800, 510, 3055, 100, "hm", red ); 
   zastorehistogram( temphist1, 410, 2800, 510, 3055, 100, "hm", green ); 
   zastorehistogram( temphist2, 410, 2800, 510, 3055, 100, "hm", blue ); 

   /* COMPOSITE HISTOGRAM OF IO.IMG and MANDRILL.IMG */

   /* Store zoomed color image at (1,3072)			*/
   zazoomimage( inunit[0],1,1,512,512,1,3072,-2,"In",'N', &blk, &wht, 0, 0 ) ;	
   zazoomimage( inunit[1],1,1,512,512,1,3072,-2,"In",'N', &blk, &wht, 0, 0 ) ;	
   zazoomimage( inunit[2],1,1,512,512,1,3072,-2,"In",'N', &blk, &wht, 0, 0 ) ;	

   /* Store zoomed & stretched color image at (1,2816)		*/
   zazoomimage( inunit[5],1,1,512,512,1,3327,-2,"In",'n',&sixty, &twohundred, 0, 0 ) ;	
   zazoomimage( inunit[6],1,1,512,512,1,3327,-2,"In",'n',&sixty, &twohundred, 0, 0 ) ;	
   zazoomimage( inunit[7],1,1,512,512,1,3327,-2,"In",'n',&sixty, &twohundred, 0, 0 ) ;	


   /* Test of HALFWORD display, HALFWORD histograms, and HALFWORD stretched */
   zastorestring(562,3356,2,red,"shrink halfword",1);
   zastorestring(818,3356,2,green,"zoom-stretch halfword (10K-30K)",1);

   halfhist = (unsigned int *)calloc(65536,sizeof(unsigned int));
   zacalculatehist(inunit[4],1,1,512,512,1,1,halfhist,&maxdn,&numpoints, 
	'n', &sixty, &twohundred, dummy, 0);
   zazoomimage(inunit[4],1,1,512,512,512,3328,-2,"Re",'N',&blk,&wht,0,0);
   zhistat2(halfhist,numpoints,&mean,&sigma,&min,&max,&maxdn);
/*   getscale( &2, dmy, &max, &iscale, &oscale, &status );    */
   iscale = 1.0;
   oscale = 128.0;
   iscale /= oscale;
   zhiscale( halfhist, numpoints, iscale, temphist, &lsat, &hsat );
   zastorehistogram( temphist, 668, 3328, 768, 3583, maxdn, "h", green );

   zacalculatehist(inunit[4],1,1,512,512,1,1,halfhist,&maxdn,&numpoints, 
	'f', &low, &hi, dummy, 0);
   zazoomimage(inunit[4],1,1,512,512,768,3328,-2,"in",'f',&low,&hi,0,0);
   zhistat2(halfhist,numpoints,&mean,&sigma,&min,&max,&maxdn);
/*   getscale( &2, dmy, &max, &iscale, &oscale, &status );   */
   iscale = 1.0;
   oscale = 128.0;
   iscale /= oscale;
   zhiscale( halfhist, numpoints, iscale, temphist, &lsat, &hsat );
   zastorehistogram( temphist, 924, 3328, 1024, 3583, maxdn, "h", green );
   free(halfhist);      

   zasetfont( "HERSHEY", 3 ) ;   
   zasetfont( "DEFAULT", 0 ) ;  

   rgb[0] = 92; /* Blue */
   rgb[1] = 150;
   rgb[2] = 255;

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
   Anotation[0].Anotation_Modulus = 0 ;
   Anotation[0].Anotation_Significant_Digits = 0 ;
   Anotation[0].Anotation_Size = 15 ;
   Anotation[0].Anotation_Orientation = 'V' ;
   Anotation[0].Anotation_Justification = 'R' ;

   Anotation[1].Anotation_Position = 0  ;
   Anotation[2].Anotation_Position = 0  ;
   Anotation[3].Anotation_Position = 0  ;

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zascale( 520, 1024, 520, 2048, red) ;   
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zascale( 520, 1024, 520, 2048, red) ;   
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zascale( 520, 1024, 520, 2048, red) ;   

   zaclearscale();
   zadefinescale( 'C', &elength, &ethick, &cthick, null ) ;

   Anotation[0].Anotation_Justification = 'R' ;
   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 1000, 1024,1000, 2048, orange) ;   
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 1000, 1024,1000, 2048, orange) ;   
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 1000, 1024,1000, 2048, orange) ;   

   rgb[0] = 255;
   rgb[1] = 255;
   rgb[2] = 255;

   len[0] = 40 ;
   len[1] = 6 ;
   len[2] = 4 ;
   len[3] = 3 ;

   thick[0] = 2 ;
   thick[1] = 1 ;
   thick[2] = 1 ;
   thick[3] = 1 ;

   period[0][0] = 64.0 ;
   period[1][0] = 16.0 ;
   period[2][0] =  8.0 ;
   period[3][0] =  4.0 ; 

   array_length = 4 ;

   Anotation[0].Anotation_Orientation = 'V' ;
   Anotation[0].Anotation_Start_Value = -1.0 ;
   Anotation[0].Anotation_Increment = 1.0 ;
   Anotation[0].Anotation_Significant_Digits = 1;
   Anotation[0].Anotation_Size = 2 ;

   Anotation[1].Anotation_Position = 0  ;
   Anotation[2].Anotation_Position = 0  ;
   Anotation[3].Anotation_Position = 0  ;

   cthick = 1;

   zadefinescale( 'C', &elength, &ethick, &cthick, null ) ;
   
   /* Print comments to go with annotation tests */
   zastorestring( 825, 3220, 2, black, "right_just,orient=L", 1);
   zastorestring( 530, 3150, 2, black, "left_just,orient=L", 1);
   zastorestring( 825, 2900, 2, black, "right_just,orient=R", 1);
   zastorestring( 530, 2650, 2, black, "left_just,orient=R", 1);
   zastorestring( 800, 2400, 2, black, "right_just,orient=V", 1);
   zastorestring( 530, 2150, 2, black, "left_just,orient=V", 1);

   /* TEST OF LEFT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='V', VERT/HORIZ */
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 2150, 750, 2150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 2250, 700, 2550, rgb ) ;   

   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 2150, 750, 2150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 2250, 700, 2550, rgb ) ;   

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 2150, 750, 2150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 2250, 700, 2550, rgb ) ;   

   /* TEST OF RIGHT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='V', VERT/HORIZ */
   Anotation[0].Anotation_Justification = 'R' ;
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 2150, 980, 2150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 2250, 925, 2550, rgb ) ;   

   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 2150, 980, 2150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 2250, 925, 2550, rgb ) ;   

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 2150, 980, 2150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 2250, 925, 2550, rgb ) ;   

   /* TEST OF LEFT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='R', VERT/HORIZ */
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Position ='C'  ;
   Anotation[0].Anotation_Orientation = 'R' ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 2650, 750, 2650, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 2750, 700, 3050, rgb ) ;   

   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 2650, 750, 2650, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 2750, 700, 3050, rgb ) ;   

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 2650, 750, 2650, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 2750, 700, 3050, rgb ) ;   

   /* TEST OF RIGHT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='R', VERT/HORIZ */
   Anotation[0].Anotation_Justification = 'R' ;
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 2650, 980, 2650, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 2750, 925, 3050, rgb ) ;   

   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 2650, 980, 2650, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 2750, 925, 3050, rgb ) ;   

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 2650, 980, 2650, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 2750, 925, 3050, rgb ) ;   

   Anotation[0].Anotation_Start_Value = -1.0 ;
   Anotation[0].Anotation_Increment = 1.0 ;
   Anotation[0].Anotation_Significant_Digits = 0 ;
   Anotation[0].Anotation_Size = 3 ;

   /* TEST OF LEFT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='L', VERT/HORIZ */
   Anotation[0].Anotation_Justification = 'L' ;
   Anotation[0].Anotation_Orientation ='L'  ;
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 3150, 750, 3150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 3225, 700, 3325, rgb ) ;   

   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 3150, 750, 3150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 3225, 700, 3325, rgb ) ;   

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 550, 3150, 750, 3150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 700, 3225, 700, 3325, rgb ) ;   

   /* TEST OF RIGHT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='L', VERT/HORIZ */
   Anotation[0].Anotation_Justification = 'R' ;
   Anotation[0].Anotation_Position ='C'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 3150, 980, 3150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 3225, 925, 3325, rgb ) ;   

   Anotation[0].Anotation_Position ='B'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 3150, 980, 3150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 3225, 925, 3325, rgb ) ;   

   Anotation[0].Anotation_Position ='T'  ;
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 820, 3150, 980, 3150, rgb ) ;   
   zadefinescale( 'L', len, thick, period, array_length, Anotation ) ; 
   zadefinescale( 'R', len, thick, period, array_length, Anotation ) ; 
   zascale( 925, 3225, 925, 3325, rgb ) ;   

   flag = zvptst("FLAG");
   if (flag)
     zacopymask('m') ;   
   else
     zacopymask(' ') ;                     
}
