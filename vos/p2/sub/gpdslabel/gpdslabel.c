
#include <stdio.h>
#include <string.h>
#include "xvmaininc.h"
#include "zvproto.h"

#define OPEN 		0x0001		/* File open? */
#define MAX  		50		/* Maximum number of tasks in label */
#define RECSIZE		78		/* Number of bytes per record	    */
#define XRECSIZE	79		/* Recsize + carrage return         */

/*******************************************************************************
Purpose:	This routine allows a user to append PDS label lines to an
		existing file.  GPDSLABEL routines assume that the file is
		opened using OPEN with record attribute carriage control. There 
		is one routine available; it accepts single values as arguments 
		to PDS label item names, allowing for repetition of that single 
		value in a PDS array argument delimited by braces {}.  This 
		routine can be called from C language programs.
	
Written:		August 20, 1991
Language:		VAX VMS C
Written by:		Justin McNeill
Cognizant programmer: 	Justin McNeill

Revisions:		October 9, 1991		
	
			File format change from STREAM lines with single 
			arguments but of length greater than 78 character
			handled correctly for fixed length records. Carriage 
			return and line feed characters placed at 79th and 
			80th bytes, respectively. (JFM059)

			October 18, 1991

			File format changes to FIXED with record attribute
			Carriage Control.  Carriage return and line feed
			characters removed from string sent to fprintf routine.
			(JFM059)
			
			January 20, 1995
			
			Port to Unix.  
			(Gloria Connor)

			August 26, 1997
			Diane Conner
			Removed line-feed for VMS OS files.
			Supports change to cdgen to produce Fixed length
			carriage return carriage control file format
			on VMS OS.

                        August 12, 98

                        Ported to ANSI_C. Raj Patel.
			

****************************************************************

ZAPDSLINE( OPTR, TYPE, LABELITEM, VALUE, NUMBER_OF_VALUES )

Purpose: To add a PDS style label line to an open non-VICAR file by accepting
	 a pointer to the output file and all the necessary inputs to create
	 the label line.

Parameters passed:

	OPTR
	OPEN file descriptor of output file;

	TYPE
	a character to specify the manner in which the PDS item label value
	(VALUE) is to be treated.  TYPE may be any one of the following 
	characters:

		'C','c'		- indicates that the string (LABELITEM) will be
				  treated as the entire PDS label line and
				  VALUE will not be used.
		'I','i' 	- integer label value
		'L','l'		- literal string to be enclosed in single
				  quotes (e.g. 'GLL-E-NIMS') on the PDS 
				  label line
		'R','r','F','f' - real or floating point label value
		'S','s'		- character string to be enclosed in double
				  quotes on the PDS label line
		'W','w'		- PDS recognized string to be free of 
				  delimiters on the PDS label line;

	LABELITEM
	a character string which includes the PDS label item name, the label 
	item name's spacing from the previous carriage return and line feed, 
	and the equal sign (=).  EX. LABELITEM = "   IMAGE_ID = "

	VALUE
	the argument of LABELITEM, a pointer to the value to be passed to be 
	placed on the right side of the equal sign.  This may be a pointer to 
	a character string, a real or floating point value or an integer.

	NUMBER_OF_VALUES
	the number of times VALUE is to be printed after LABELITEM.
	If TYPE is 'C', NUMBER_OF_VALUES should be set to one (1).

*/
/************************************************************************************/
/***   :                                         */
/************************************************************************************/
void zapdsline( optr, type, labelitem, value, number )
/* int	optr;*/
FILE	*optr;
char	type;
char	labelitem[];
void    *value;
int	number;
{
int	length,		/* String length variable			*/
	records,	/* Number of records per label entry		*/
	remainder,	/* Number of bytes in last label entry record   */
	x;		/* Loop control variables			*/
static  char strng[RECSIZE], /* String variable used in multivalued items*/
	ln[1000],	/* PDS label line buffer			*/
	spaces[RECSIZE];/* Blank spaces array				*/
int	*ivalue;	/* Pointer to integer values			*/
float	*fvalue;	/* Pointer to real values			*/
char 	*cvalue;	/* Pointer to character strings 		*/


if ( number < 1 )	/* Check for zero or negative NUMBER parameter	*/
	{		/* Change to NUMBER = 1 and send message.	*/
	number = 1;
	zvmessage("*** WARNING: NUMBER of values negative or zero. ","");
	sprintf(strng,
		"***          NUMBER of values set to one (1) for ITEM = %s.",
		labelitem);
	zvmessage(strng,"");
	}

strcpy(spaces," ");
for( x=1; x<RECSIZE; x++)
	strcat(spaces," ");	/* Fill SPACES array with blank spaces	*/


switch(type)	{

case 'C':	/* String constant 	*/
case 'c':
	
		strcpy(ln,labelitem);
		records 	= strlen(ln) / RECSIZE;
		remainder	= strlen(ln) % RECSIZE;
		strncat(ln,&spaces[remainder],(RECSIZE-remainder));
		if( remainder != 0 )
			records++;
		for( x=0; x<records; x++ )
			{
			strncpy(strng,&ln[x*RECSIZE],RECSIZE);
#if VMS_OS 
                             /* strng[RECSIZE] = '\n'; */
#else  
                             strng[RECSIZE] = '\n'; 
#endif
			/* strng[RECSIZE] = '\n'; */
			strng[RECSIZE+1] = '\0';
			fprintf(optr,strng); 
			}
		break;

case 'W':	/* PDS KEYWORD (no quotes for string enclosure) */
case 'w':	cvalue = value;
		if( number == 1 )
			{
			sprintf(ln,"%s = %s",labelitem,cvalue);
			records 	= strlen(ln) / RECSIZE;
			remainder	= strlen(ln) % RECSIZE;
			strncat(ln,&spaces[remainder],(RECSIZE-remainder));
			if( remainder != 0 )
				records++;
			for( x=0; x<records; x++ )
				{
				strncpy(strng,&ln[x*RECSIZE],RECSIZE);
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
 				strng[RECSIZE+1] = '\0';
				fprintf(optr,strng);				
				}				
			}
		else
			{
			sprintf(ln,"%s = (",labelitem);
			for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%s,",cvalue);
				strcat(ln,strng);
				length = strlen(ln);
				if( length+strlen(strng) > (RECSIZE-10))
					{
					strncat(ln,&spaces[length],
						(RECSIZE-length));
					ln[RECSIZE] = '\n';
					ln[RECSIZE+1] = '\0';
					fprintf(optr,ln);
					strcpy(ln,"       ");
					/*fprintf(optr,ln);*/
					}
				}
			sprintf(strng,"%s%s)",ln,cvalue);
			length = strlen(strng);
			strncat(strng,&spaces[length],(RECSIZE-length));
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
			strng[RECSIZE+1] = '\0';
			fprintf(optr,strng);		
			}
		break;

case 'L':	/* Literal string (enclosed in single quotes) */
case 'l':	cvalue = value;
		if( number == 1 )
			{
			sprintf(ln,"%s = \'%s\'",labelitem,cvalue);
			records 	= strlen(ln) / RECSIZE;
			remainder	= strlen(ln) % RECSIZE;
			strncat(ln,&spaces[remainder],(RECSIZE-remainder));
			if( remainder != 0 )
				records++;
			for( x=0; x<records; x++ )
				{
				strncpy(strng,&ln[x*RECSIZE],RECSIZE);
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
				strng[RECSIZE+1] = '\0';
				fprintf(optr,strng);
				}
			}
		else
			{
			sprintf(ln,"%s = (",labelitem);
			for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"\'%s\',",cvalue);
				strcat(ln,strng);
				length = strlen(ln);
				if( length+strlen(strng) > (RECSIZE-10))
					{
					strncat(ln,&spaces[length],
						(RECSIZE-length));
					ln[RECSIZE] = '\n';
					ln[RECSIZE+1] = '\0';
					fprintf(optr,ln);
					strcpy(ln,"      ");
					/*fprintf(optr,ln);*/
					}				
				}
			sprintf(strng,"%s\'%s\')",ln,cvalue);
			length = strlen(strng);
			strncat(strng,&spaces[length],(RECSIZE-length));
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
			strng[RECSIZE+1] = '\0';
			fprintf(optr,strng);
			}
		break;

case 'S':	/* Character string (enclosed in double quotes) */
case 's':	cvalue = value;
		if( number == 1 )
			{
			sprintf(ln,"%s = \"%s\"",labelitem,cvalue);
			records 	= strlen(ln) / RECSIZE;
			remainder	= strlen(ln) % RECSIZE;
			strncat(ln,&spaces[remainder],(RECSIZE-remainder));
			if( remainder != 0 )
				records++;
			for( x=0; x<records; x++ )
				{
				strncpy(strng,&ln[x*RECSIZE],RECSIZE);
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
  				strng[RECSIZE+1] = '\0';
				fprintf(optr,strng);				
				}
			}
		else
			{
			sprintf(ln,"%s = (",labelitem);
			for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"\"%s\",",cvalue);
				strcat(ln,strng);
				length = strlen(ln);
				if( length+strlen(strng) > (RECSIZE-10))
					{
					strncat(ln,&spaces[length],
						(RECSIZE-length));
					ln[RECSIZE] = '\n';
					ln[RECSIZE+1] = '\0';
					fprintf(optr,ln);
					strcpy(ln,"      ");
					}
				}
			sprintf(strng,"%s\"%s\")",ln,cvalue);
			length = strlen(strng);
			strncat(strng,&spaces[length],(RECSIZE-length));
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
			strng[RECSIZE+1] = '\0';			
			fprintf(optr,strng);
			}
		
		break;

case 'I': 	/* Integer value 	*/
case 'i':	ivalue = value;
		if( number == 1 )
			{
			sprintf(ln,"%s = %d",labelitem,(*ivalue));
			records 	= strlen(ln) / RECSIZE;
			remainder	= strlen(ln) % RECSIZE;
			strncat(ln,&spaces[remainder],(RECSIZE-remainder));
			if( remainder != 0 )
				records++;
			for( x=0; x<records; x++ )
				{
				strncpy(strng,&ln[x*RECSIZE],RECSIZE);
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
                                strng[RECSIZE+1] = '\0';                        
  	                        fprintf(optr,strng);
				
				}
			}
		else
			{
 			sprintf(ln,"%s = (",labelitem);
			for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%d,",(*ivalue));
				strcat(ln,strng);
				length = strlen(ln);
				if( length+strlen(strng) > (RECSIZE-10))
					{
					strncat(ln,&spaces[length],
						(RECSIZE-length));
					ln[RECSIZE] = '\n';
					ln[RECSIZE+1] = '\0';
					fprintf(optr,ln);
					strcpy(ln,"     ");					
					}
				}
			sprintf(strng,"%s%d)",ln,(*ivalue));
			length = strlen(strng);
			strncat(strng,&spaces[length],(RECSIZE-length));
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
			strng[RECSIZE+1] = '\0';
			fprintf(optr,strng);
			}		
		break;

case 'R':
case 'r':
case 'F': 	/* floating point value 	*/
case 'f':	fvalue = value;
		if( number == 1 )
			{
			sprintf(ln,"%s = %g",labelitem,(*fvalue));
			records 	= strlen(ln) / RECSIZE;
			remainder	= strlen(ln) % RECSIZE;
			strncat(ln,&spaces[remainder],(RECSIZE-remainder));
			if( remainder != 0 )
				records++;
			for( x=0; x<records; x++ )
				{
				strncpy(strng,&ln[x*RECSIZE],RECSIZE);
#if VMS_OS 
                                    /* strng[RECSIZE] = '\n'; */
#else  
                                    strng[RECSIZE] = '\n'; 
#endif
				strng[RECSIZE+1] = '\0';
				fprintf(optr,strng);
					
				}
			}
		else
			{
 			sprintf(ln,"%s = (",labelitem);
			for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%g,",(*fvalue));
				strcat(ln,strng);
				length = strlen(ln);
				if( length+strlen(strng) > (RECSIZE-10))
					{
					strncat(ln,&spaces[length],
						(RECSIZE-length));
					ln[RECSIZE] = '\n';
					ln[RECSIZE+1] = '\0';
					fprintf(optr,ln);
					strcpy(ln,"     ");
					}
				}
			sprintf(strng,"%s%g)",ln,(*fvalue));
			length = strlen(strng);
			strncat(strng,&spaces[length],(RECSIZE-length));
#if VMS_OS 
  /* strng[RECSIZE] = '\n'; */
#else  
   strng[RECSIZE] = '\n'; 
#endif
			strng[RECSIZE+1] = '\0';
			fprintf(optr,strng);	
			}
	
		break;	

default:
		zvmessage("************************","");
		zvmessage("************************","");
		sprintf(strng,"UNDEFINED VALUE TYPE (%c)",type);
		zvmessage(strng,"");
		zvmessage(" ","");
		zvmessage("     for label item","");
		sprintf(strng,"     %s",labelitem);
		zvmessage(strng,"");
		zvmessage(" ","");
		zvmessage("  No PDS line written.","");
		zvmessage("************************","");
		zvmessage("************************","");
		break;		}
}
