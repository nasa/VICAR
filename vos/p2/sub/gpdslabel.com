$!****************************************************************************
$!
$! Build proc for MIPL module gpdslabel
$! VPACK Version 1.9, Monday, December 07, 2009, 16:22:04
$!
$! Execute by entering:		$ @gpdslabel
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module gpdslabel ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to gpdslabel.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("gpdslabel.imake") .nes. ""
$   then
$      vimake gpdslabel
$      purge gpdslabel.bld
$   else
$      if F$SEARCH("gpdslabel.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gpdslabel
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gpdslabel.bld "STD"
$   else
$      @gpdslabel.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gpdslabel.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gpdslabel.com -mixed -
	-s gpdslabel.c -
	-i gpdslabel.imake -
	-t tgpdslabel.c tgpdslabel.imake tgpdslabel.pdf tstgpdslabel.pdf -
	-o gpdslabel.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gpdslabel.c
$ DECK/DOLLARS="$ VOKAGLEVE"

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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gpdslabel.imake
#define SUBROUTINE gpdslabel
#define MODULE_LIST gpdslabel.c 

#define P2SUB
#define P2_SUBLIB

#define USES_ANSI_C


$ Return
$!#############################################################################
$Test_File:
$ create tgpdslabel.c
#include <stdio.h>
#include "vicmain_c"
#include <stdlib.h>

main44()
{
int 	count, unit, stat, x, y;
char 	output[200];
char	expanded_output[200];
char 	string[500];
int	flags;   
int	status;
char	msg[200];

FILE	*ptr;

int i;

zvp("OUT",output,&count, 0);	/* Get file name of output		  */
zvselpi(1);
status = zvfilename(output, expanded_output, 199 );

ptr = fopen(expanded_output, "w+" );

/* TEST OF STRINGS OVER 80 CHARACTERS */
strcpy(string,"CCSD3ZF000100000001NJPL3IF0PDS200SDFKLJERJTSDJFKJKLJ");
strcat(string,"CCSD3ZF000100000001NJPL3IF0PDS200000001 = SFDU_LABEL");

zapdsline(ptr,'c',string,&x,1);

zapdsline(ptr,'c',"/* File format and length */",&x,1);

zapdsline(ptr,'w',"RECORD_TYPE","FIXED_LENGTH",0);	/* WARNING Expected */

zapdsline(ptr,'w',"RECORD_TYPE","FIXED_LENGTH",1);

zapdsline(ptr,'w',"RECORD_TYPE","FIXED_LENGTH",5);
i = 100;
zapdsline(ptr,'i',"RECORD_BYTES",&i,18);
i=1000;
zapdsline(ptr,'i',"RECORD_BYTES",&i,1);

zapdsline(ptr,'l',"DATA_SET_ID","GO-V/E-SSI-EDR-E1-V1.0",1);

zapdsline(ptr,'l',"DATA_SET_ID","GO-V/E-SSI-EDR-E1-V1.0",12);

zapdsline(ptr,'s',"CURRENT_TIME","91.203 09:54:02.201",1);

/* TEST OF STRING ARGUMENTS OVER RECORD BOUNDARY */
strcpy(string,"Nineteen hundred ninty-one, day two hundred three,");
strcat(string,"Time is nine fifty-four and two thousand two hundred");
strcat(string," and one milliseconds.");
zapdsline(ptr,'s',"CURRENT_TIME_IN_ENGLISH",string,1);

/* close(ptr);	*/			/* Close output file */
}
$!-----------------------------------------------------------------------------
$ create tgpdslabel.imake

#define PROGRAM tgpdslabel

#define TEST

#define MODULE_LIST tgpdslabel.c 
#define MAIN_LANG_C
#define USES_C

#define LIB_TAE
#define LIB_RTL 
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tgpdslabel.pdf
process
parm OUT STRING
end-proc
$!-----------------------------------------------------------------------------
$ create tstgpdslabel.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"


refgbl $syschar

! PCA
!!!!dcl del scx1:[mipl]tgpdslabel.pca;*
!!!!dcl assign tstgpdslabel.pcac pcac$init
!!!!dcl assign tstgpdslabel.pcaa pcaa$init
!
!	This is what a DCL TYPE on PDS.FILE should look like.
!CCSD3ZF000100000001NJPL3IF0PDS200SDFKLJERJTSDJFKJKLJCCSD3ZF000100000001NJPL3IF
!0PDS200000001 = SFDU_LABEL                                                    
!/* File format and length */                                                  
!RECORD_TYPE = FIXED_LENGTH                                                    
!RECORD_TYPE = FIXED_LENGTH                                                    
!RECORD_TYPE = (FIXED_LENGTH,FIXED_LENGTH,FIXED_LENGTH,FIXED_LENGTH,           
!       FIXED_LENGTH)                                                          
!RECORD_BYTES = (100,100,100,100,100,100,100,100,100,100,100,100,100,          
!     100,100,100,100,100)                                                     
!RECORD_BYTES = 1000                                                           
!DATA_SET_ID = 'GO-V/E-SSI-EDR-E1-V1.0'                                        
!DATA_SET_ID = ('GO-V/E-SSI-EDR-E1-V1.0','GO-V/E-SSI-EDR-E1-V1.0',             
!      'GO-V/E-SSI-EDR-E1-V1.0','GO-V/E-SSI-EDR-E1-V1.0',                      
!      'GO-V/E-SSI-EDR-E1-V1.0','GO-V/E-SSI-EDR-E1-V1.0',                      
!      'GO-V/E-SSI-EDR-E1-V1.0','GO-V/E-SSI-EDR-E1-V1.0',                      
!      'GO-V/E-SSI-EDR-E1-V1.0','GO-V/E-SSI-EDR-E1-V1.0',                      
!      'GO-V/E-SSI-EDR-E1-V1.0','GO-V/E-SSI-EDR-E1-V1.0')                      
!CURRENT_TIME = "91.203 09:54:02.201"                                          
!CURRENT_TIME_IN_ENGLISH = "Nineteen hundred ninty-one, day two hundred three,T
!ime is nine fifty-four and two thousand two hundred and one milliseconds."    

write "Expect warning regarding NUMBER of values for ITEM = RECORD_TYPE"

tgpdslabel out=pds.file

if ($syschar(1) = "VAX_VMS")
	dcl type pds.file
else 
	more  pds.file
end-if

! PCA
!!!!!!    dcl pca scx1:[mipl]tgpdslabel.pca
!!!!!!    dcl type tstgpdslabel.pcatxt
!!!!!!    dcl del tstgpdslabel.pca*.*
!!!!!!    dcl del scx1:[mipl]tgpdslabel.pca.*
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create gpdslabel.hlp
1 GPDSLABEL routine ZAPDSLINE

ZAPDSLINE( FPTR, TYPE, LABELITEM, VALUE, NUMBER_OF_VALUES )

Purpose: To add a PDS style label line to an open non-VICAR file by accepting
	 a FILE* to the output file and all the necessary inputs to create
	 the label line.

2 Parameters passed:

	FPTR
	a FILE * that may be created with fopen(...) statement;


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

3 History

  Original Programmer: 	Justin McNeill, Jr, August 20, 1991 
  Cognizant Programmer: Justin McNeill, Jr.
  Source Language: 	C
  Revisions: 		October 9, 1991		

			File format change from STREAM lines with single 
			arguments but of length greater than 78 character
			handled correctly for fixed length records. Carriage 
			return and line feed characters placed at 79th and 
			80th bytes, respectively. (JFM059)

			October 18, 1991

			File format changes to FIXED with record attribute
			Carriage Control.  Carriage return and line feed
			characters removed from string sent to WRITE routine.
			(JFM059)
			
			January 1995
			
			Ported to Unix.
			(Gloria Connor)

			September 1997

			File format changes to VMS FIXED length record with 
			Carriage Control attributes if you are running on a VMS System
			(Diane Conner)



$ Return
$!#############################################################################
