/**********************************************************
 *
 * This file contains the routines to implement the
 * Hershey font character sets.  The character fonts
 * may look more appealing than the standard 5x7 characters.
 * These routines are particularly set up for font #3.
 *
 * January 24, 1991 	REVISION
 * xxaMalloc and xxaFree added for sufficient working space for
 * AsciiSet (Bob Deen's code).  	
 *
 * April 30, 1991	REVISION
 * AsciiSet revised to contain only those characters used in
 * a string to be printed, reducing the size of allocated 
 * memory used per string production.  	
 *
 * April 7,  1993	REVISION    Florance Moss
 * Replaced xxaMalloc with ported BigMalloc, xxaFree with ported BigFree
 * Replaced xv calls with zv calls.
 * Removed unnecessary include files.
 * Changed TXTsubroutines to ZTXTsubroutines, and pass value instead of 
 * address for input arguments.
 * Changed i from integer to unsigned char for ztxttext routine.
 * Added printf for all ZTXT routines status checking.
 *
 * 	Author :		Alvin Wong
 *	Date :			8/29/86
 *	Last revised : 		4/30/91
 *      Cognizant Programmer : 	Justin McNeill
 **********************************************************/

/******************************/
/* Copy font string onto mask */
/******************************/
#define SPACE			32
#define ASCIICHARNUM		128-SPACE	/* [ 33..127 ] ascii set */
						/* w/o control characters */
#define SUBIMAGELINES		250	/* Max # of subimage lines */
#define SUBIMAGESAMPLES		200	/* Max # of subimage samples */

#define WIDTH2HEIGHTRATIO	0.6	/* Character width/height */
#define ANGLE			0.0	/* Character angle */
#ifndef NULL
#define NULL			0
#endif

void xxahersheystring();
void xxahershey90string();
void xxacalcwidth();


int xxacopyhersheystring( index, line )
int line;
struct StringType *index;
{ 

   unsigned char ii;
   int i,j ;			/* General counters 		*/
   int error ;			/* Error flag 			*/
   int temp ;			/* Temporary	 		*/
   int status;                  /* return status from ZTXT call */
/*****************************************************************************/
/* If required, do preliminary font specifications and create ascii font set */
/*****************************************************************************/

   if ( line == index->StartLine )
      {
      status = ztxtfont( index->FontNum ) ;     		/* Define font # */
      if (status != 1) printf("*** ERROR : status from ztxtfont is %d",status);
      status = ztxtsize(index->CharHeight,WIDTH2HEIGHTRATIO);	/* Define character size */
      if (status != 1) printf("*** ERROR : status from ztxtsize is %d",status);
      status = ztxtrotate( ANGLE ) ;			     	/* Define character angle*/
      if (status != 1) printf("*** ERROR : status from ztxtrotate is %d",status);
      if(Dimension == 1)
      	status = ztxtcolor( index->Dn[0] ) ;	     		/* Define DN value */
      else
	status = ztxtcolor( 255 );			
      if (status != 1) printf("*** ERROR : status from ztxtcolor is %d",status);

      for( i = 0; i < strlen(index->CharString); i++ )
	index->usedcharindex[index->CharString[i]-SPACE]++;
      for( i=0, j=0; i < 96; i++ )
	if( index->usedcharindex[i] != 0 )
		j++;

      index->AsciiSet = (char *)BigMalloc( j * index->bufarea );
      if( index->AsciiSet == NULL )
	{
	zvmessage("GMASK memory allocation error"," ");
	zvmessage(" - routine aborted"," ");
	return( -2 );
	}
      					/* Init ascii array to blanks 	*/
					/* 96 is ASCII value for blanks */
      for( i = 0 ; i < j ; i++ )	
        fillbyte( &(index->AsciiSet[i*index->bufarea]), 96, index->bufarea ) ;
        
      temp = index->CharHeight + 1;
      for( i=SPACE,j=0; i<127; i++ )
	if( index->usedcharindex[i-SPACE] != 0 )
		{
		ii = (unsigned char) i;
		status = ztxttext( &(index->AsciiSet[j*index->bufarea]), 
			index->bufheight, index->bufwidth, 1, temp, 
			1, 1, &ii, &error );
                if (status != 1) printf("*** ERROR : status from ztxttext is %d",status);
		index->usedcharindex[i-SPACE] = j++;
		}

      }


   if ( index->Angle == 0 )
      xxahersheystring( index, line ) ;
   else
      xxahershey90string( index, line ) ;

   if ( line == (index->LastLine-1) )
      BigFree(index->AsciiSet);
   return(0);
}

/*************************************/
/* Copy font characters at 0 degrees */
/*************************************/

void xxahersheystring( index, line )
int line;
struct StringType *index;
{
   int HorizChar = FALSE ;	/* Keep tabs on horizontal char (-=+) 	*/
   int TailChar = FALSE ;	/* Keep tabs on 'tail' chars (gjpqy) 	*/


   int FontThickness ;		/* Font thickness 			*/
   int thick ;			/* Counter for thickness 		*/
   int i, nline, nsample ;	/* General counters 			*/
   int dummyDn, num, idx;
   int usedchar;		/* Character index for usercharindex array */
   int ch ;			/* Current character 			*/
   int width ;			/* Character width 			*/
   int offset ;			/* Offset for wider or thinner char 	*/
   
   FontThickness =  index->FontThickness ;

   ch = 0 ;
   offset = 0 ;

   for ( i = 0 ; i < strlen( index->CharString ) ; i++ )
   		{
      		ch = index->CharString[i];	/* Current Char */
      		xxacalcwidth( ch,index,&width,&HorizChar,&TailChar ) ;

      		for ( nsample = 0 ; nsample < width ; nsample++ )
      			{
		        nline = line - index->StartLine ;

/* Skip over characters without tails if we've gone over CharHeight */

         		if ( nline > index->CharHeight && TailChar == FALSE )
            			break ;

/* The star, *, seems to have been generated off centered near the top */
/* Make allowances for this */

         		if ( ch == '*' )
            			nline = ( nline + (2*index->CharHeight)/3 )
					% index->CharHeight ;

         		if ( nline >= index->bufheight )
            			return ;

			usedchar = index->usedcharindex[ch-SPACE];
         		num = index->AsciiSet[ usedchar*index->bufarea 
			   + (nline*index->bufwidth) + nsample ] & MAXDNVALUE ;

/* If color version is used, let Dn be maximum.  String.Dn substituted later */

			if(Dimension == 1)
				dummyDn = index->Dn[0];
			else
				dummyDn = 255;

/* Make allowances for horizontal characters */

			if(HorizChar==TRUE && num!=dummyDn && 
				nline-FontThickness >= 0 )
			   	{
            		  	for(thick=0;thick<FontThickness;thick++)
         			   if( (index->AsciiSet[usedchar*index->bufarea
				       +(nline-thick)*index->bufwidth+nsample]
				       & MAXDNVALUE) == dummyDn )
						num = dummyDn;
					/* Top-Bottom thickness */
                           	}
			idx = index->StartSample+offset+nsample;

         		if(num == dummyDn && idx < UserMaxSamples )
				for(thick = 0; thick < FontThickness; thick++ )
            				if ( idx < UserMaxSamples )
					   {
                                           for( d=0; d<Dimension; d++ )
						Mask[d][idx] = index->Dn[d];
						/* Side-Side thickness */
					   idx++;
					   }
      			}
	offset += width + index->CharSpac; /* Character spacing */
   	}
}
/**************************************/
/* Copy font characters at 90 degrees */
/**************************************/

void xxahershey90string( index, line )
int line ;
struct StringType *index;	
{
   int HorizChar = FALSE ;	/* Keep tabs on horizontal char (-=+) 	*/
   int TailChar = FALSE ;	/* Keep tabs on 'tail' chars (gjpqy) 	*/
   int EndSamp ;		/* Counter for end sample 		*/
   int FontThickness ;		/* Font thickness 			*/
   int thick ;			/* Counter for thickness 		*/
   int i, j, nline, nsample ;	/* General counters 			*/

   int usedchar;		/* Character index for usercharindex array */
   int ch ;			/* Current character 			*/
   int width ;			/* Character width 			*/
   int offset ;			/* Offset for wider or thinner char 	*/

   int CharIndex ;		/* Current character index 		*/
   int FontLine ;		/* Vertical line of char to write out 	*/
				/* horizontally. 			*/
   int idx ;			
   int num ;			/* Current Dn number 		*/
   int dummyDn,temp ;

   FontThickness =  index->FontThickness ;

   ch = 0 ;
   offset = 0 ;
   FontLine = index->StartLine ;

   for ( i = 0 ; i < strlen( index->CharString ) ; i++ )
      	{
	ch = index->CharString[ i ] ;	/* Current Char */

	xxacalcwidth( ch, index, &width, &HorizChar, &TailChar ) ;

      	if ( FontLine + width + index->CharSpac < line )
      		FontLine += width + index->CharSpac ;
      	else
      		{
	        CharIndex = i ;
        	FontLine = line - FontLine ;
        	break ;
      		}
      	}

   if ( FontLine >= index->bufheight )	/* Should never happen but... */
      	return ;

   if ( CharIndex >= strlen( index->CharString ) )
      	return ;


   if ( TailChar == TRUE )
         EndSamp = index->CharHeight / 2 ;
   else
         EndSamp = 0 ;

/**********************************************************/
/* Write out the character strings tilted at +-90 degrees */
/**********************************************************/
   idx = index->StartSample ;

/* If color version is used, let Dn be maximum.  String.Dn substituted later */

   if(Dimension == 1)
	dummyDn = index->Dn[0];
   else
	dummyDn = 255;

   if ( index->Angle == 90 )
   	for(nsample = index->CharHeight; nsample >= -EndSamp; nsample--)
   		{
      		temp = nsample ;
      		if ( ch == '*' )		/* Center the star character */
      			{
         		if ( nsample - index->CharHeight / 3 >= 0 )
            			temp = nsample - index->CharHeight/3;
         		else
            			temp = index->CharHeight ;
     			 }

      		if ( TailChar == TRUE )
      			{
         		if(nsample+index->CharHeight/2 < index->bufwidth )
            			temp=nsample+index->CharHeight/2;
        	 	else
            			temp = index->bufwidth - 1 ;

         		if(idx-EndSamp>=0 && nsample==index->CharHeight)
            			idx -= EndSamp ;
      			}

	        usedchar = index->usedcharindex[index->CharString[CharIndex]
			-SPACE];
      		num = index->AsciiSet[usedchar*index->bufarea 
			+ (temp*index->bufwidth) + FontLine] & MAXDNVALUE ;

   		if ( num == dummyDn && idx < UserMaxSamples )
      			{
        		for(thick = 0; thick < FontThickness ; thick++ )
        			{
        			if ( idx < UserMaxSamples )
				   {
				   for( d=0; d<Dimension; d++ )
				    	Mask[d][idx] = index->Dn[d];
				   idx++;
				   }
           			if(HorizChar==TRUE)/* Thicken hz char */
             			   for ( j = 0 ; j < FontThickness ; j++ )
                			if ( idx+j < UserMaxSamples )
					   for( d=0; d<Dimension; d++ )
						Mask[d][ idx+j ] = index->Dn[d];
         			}
         		idx -= FontThickness - 1;
      	 		}
   		else 
         		idx++ ;
   	 	}
   	
   	else		/* -90 degrees character generation here */
   	{
      	if ( ch == '*' )
         	temp = index->CharHeight / 3 ;

      	FontLine = width - FontLine ;

      	for(nsample=0;nsample<=index->CharHeight+EndSamp;nsample++)
		{
		usedchar = index->usedcharindex[index->CharString[CharIndex]
			-SPACE];
      		num = index->AsciiSet[usedchar*index->bufarea 
			+ (nsample*index->bufwidth) + FontLine] & MAXDNVALUE ;
		if(num == dummyDn && idx<UserMaxSamples)
         		{
            		for(thick = 0; thick < FontThickness ; thick++ )
            			{
                		if(ch == '*') /* Center the star char */
                   			{
					if ( idx+temp < UserMaxSamples )
					   for( d=0; d<Dimension; d++ )
					      Mask[d][idx+temp]=index->Dn[d];
					}
                		else
					{
					for( d=0; d<Dimension; d++ )
					   Mask[d][idx] = index->Dn[d];
					idx++;
					}
                		if(HorizChar==TRUE)  /* Thicken horiz. chars */
                			for(j=0;j<FontThickness;j++)
                  				if(idx+j<UserMaxSamples)
						  for( d=0; d<Dimension; d++ )
						    Mask[d][idx+j]=index->Dn[d];
            			}
            		idx -= FontThickness - 1;
         		}
         	else 
         		idx++ ;
      		}
	}
}

/*************************************************************************/
/* Determine character characteristics like width, horizontal, tail, etc */
/*************************************************************************/

void xxacalcwidth( ch, index, width, HorizChar, TailChar )
char ch ;
int *width, *HorizChar, *TailChar ;
struct StringType *index;
{

      if ( ch == 'm' )		/* Make room for widest character, m */
         *width = index->CharHeight ;
      else			/* Make room for almost widest char */
      if ( ch == '%' || ch == '@' || ch == '+' ||
           ch == 'M' ||
           ch == 'W' || ch == 'w' || ch == 'U' )
         *width = index->CharHeight * 0.9 ;
      else			/* Make sure character is not 'broken' */
      if ( ch == 'B' || ch == 'D' || ch == 'H' ||
           ch == 'L' || ch == 'K' || ch == 'O' ||
           ch == 'P' || ch == 'Q' || ch == 'R' ||
           ch == 'T' ||
           ch == 'b' || ch == 'h' || ch == 'k' ||
           ch == 'p' || ch == 'n' )
         *width = index->CharHeight * 0.7 ;
      else			/* Close up gaps for thin characters */
      if ( ch == 'i' || ch == 'I' ||
           ch == 'j' || ch == 'J' ||
           ch == 'L' || ch == 'l' ||
           ch == 'f' ||
           ch == ':' || ch == '.' || ch == ',' )
         *width = index->CharHeight * WIDTH2HEIGHTRATIO * 0.8 ;
      else			/* Regular characters */
         *width = index->CharHeight * WIDTH2HEIGHTRATIO ;

/* Flag current character is 'horizontal' for more thickness */

      if ( ch == '-' || ch == '=' || ch == '+' )
         *HorizChar = TRUE ;
      else
         *HorizChar = FALSE ;

/* Flag current character has a 'tail' below the normal character line */
/* Add '_', ',', & ';' as tail characters so it won't be chopped off at the
   bottom    3/25/93    FFM   */

      if ( ch == 'g' || ch == 'j' || ch == 'p' || ch == '_' ||
           ch == 'q' || ch == 'y' || ch == ',' || ch == ';' )
         *TailChar = TRUE ;
      else
         *TailChar = FALSE ;
}

