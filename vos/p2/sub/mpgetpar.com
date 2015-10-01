$!****************************************************************************
$!
$! Build proc for MIPL module mpgetpar
$! VPACK Version 1.9, Monday, December 07, 2009, 16:29:04
$!
$! Execute by entering:		$ @mpgetpar
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
$ write sys$output "*** module mpgetpar ***"
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
$ write sys$output "Invalid argument given to mpgetpar.com file -- ", primary
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
$   if F$SEARCH("mpgetpar.imake") .nes. ""
$   then
$      vimake mpgetpar
$      purge mpgetpar.bld
$   else
$      if F$SEARCH("mpgetpar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mpgetpar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mpgetpar.bld "STD"
$   else
$      @mpgetpar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mpgetpar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mpgetpar.com -mixed -
	-s mpgetpar.c -
	-i mpgetpar.imake -
	-t tmpgetpar.c tmpgetpar.imake tmpgetpar.pdf tstmpgetpar.pdf -
	-o mpgetpar.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mpgetpar.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "xvmaininc.h"		/* Standard VICAR Include File		*/
#include "ftnbridge.h"		/* FORTRAN bridge Include FIle 		*/
#include <math.h>		/* FORTRAN bridge Include FIle 		*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_routines.h"	/* Map Projection Include File		*/
#include "mp_pconstants.h"      /* PConstants Include File              */
#include <string.h>


#define PARAMETER_NOT_FOUND -86	/* VICAR Run Time Library error code    */
				/* for a PDF parameter value not found  */

/*

VICAR SUBROUTINE		mpgetpar

Purpose				Routine to extract map projection values defined
				by VICAR/TAE procedure definition file (.PDF)
				and initialize a map projection data object
				using mp_init for subsequent use in performing
				point map transformations using mpll2xy and/or
				mpxy2ll, part of the mp_routine suite of map
				projection software.

Function			Reads in values for a specific map projection
				stored in VICAR parameter values from .pdf file
				and initializes a map projection data object
				with those values. This routines uses the VICAR
				Run Time Library (VICAR RTL) and the mp routines
				mpInit, mpSetValue.

Libraries and subroutines
required to run routine:	mp_routines suite

Main programs from which 
subroutines are called:		general application software and higher-level
				subroutines; hwgeom, hwortho.

Calling Sequence:		

from C  	status = mpGetPar( MP_DATA, pdf_parameters, 
				pds_standard_keywords, PCK_file );
from FORTRAN  	call mp_get_par( MP_DATA, pdf_parameters, 
			pds_standard_keywords, PCK_file )

Necessary include files
from calling routine 
or program:			mp.h


Arguments:
	
Name			Type		In/Out		Description
	
MP_DATA			MP		Output		Address of
							Map Projection 
							Data Object

pdf_parameters		2-D char array	Input		Actual names of
							VICAR pdf parameters
							for the map projection
							specific values. 

pds_standard_keywords	2-D char array	Input		Actual PDS standard
							keyword names used in
							map projection software
							suite (mp_routines.com)
							found in the file mp.h.
							A NULL entry for a 
							character string ('\0')
							in this array signals
							the end of the listing 
							of keywords.

PCK_file		character	Input		Full pathname of NAIF
							PCK file (planetary
							constants kernel)
Return:
	
status 		integer		0	Successful call to mpgetpar

				-1	Failure; unsuccessful reading of VICAR 
					pdf parameter values or error in setting
					of values in data object.

For character string array inputs ...

of pdf parameter names and PDS standard keyword names, the dimension of these
arrays sound be declared in the application program with the outer dimension
being set to mpNUMBER_OF_KEYWORDS. mpNUMBER_OF_KEYWORDS are the maximum number 
of keywords that can be set in a map projection data object by functions in 
the mp_routines suite. 

For the parameter "pdf_parameters", the inner dimension should be set to 
mpMAX_PARM_LENGTH + 1. mpMAX_PARM_LENGTH is the maximum allowable number of 
characters in a VICAR procedure definition file parameter name. For the 
parameter "pds_standard_keywords", the inner dimension of the array should be 
set to mpMAX_KEYWD_LENGTH+1.

Thus a sample initialization of these arrays in preparation for a call to 
MPGETPAR is shown below.

	char PDF_parm_names[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
	char PDS_keyword_names[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];


Background and References:	MPGETPAR specification by Jurgen Oberst.

Software Platform:		VICAR 11.0 (VMS/UNIX)

Hardware Platforms:		No particular hardware required; tested on 
				VAX 8650 and Sun Sparcstation.

Programming Language:		ANSI C

Specification by:		Justin McNeill, JPL.

Cognizant Programmer:		Justin McNeill, JPL
				(jfm059@ipl.jpl.nasa.gov)

Date:				March 14, 1994

History:         ...TLT...      original
		lwk		added TARGET_NAME keyword

*/
/*************************************************************************

FORTRAN Callable Version

*************************************************************************/

int FTN_NAME2_(mp_get_par, MP_GET_PAR) (MP *mp, char *pdf_parameters,
	char *pds_standard_map_values,
	char *spice_constants_file, ZFORSTR_PARAM)
{
ZFORSTR_BLOCK
int 	i,j,k;
int	status;
int 	elements,length,max_length;

char    string[100];
char 	pc_kernel_pathname[200];
char    *pdf_array,*pds_array;
char	pdf_strings[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_strings[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];

/* 

Initialize variables 

*/

max_length = 0;
elements = mpNUMBER_OF_KEYWORDS;

/*

Process FORTRAN strings

*/

zsfor2c_array(&pdf_array,&max_length,elements,pdf_parameters,&mp,4,2,1,
							spice_constants_file);
for( i=0; i<elements; i++ )
	strcpy(pdf_strings[i],pdf_array+(i*max_length));

max_length = 0;
zsfor2c_array(&pds_array,&max_length,elements,pds_standard_map_values,
	&mp,4,3,2, spice_constants_file);
for( i=0; i<elements; i++ )
	strcpy(pds_strings[i],pds_array+(i*max_length));

max_length = 200;
zsfor2c(pc_kernel_pathname,max_length,spice_constants_file,&mp,4,4,3,
							spice_constants_file);


status = mpGetPar( mp,pdf_strings,pds_strings,pc_kernel_pathname );

free(pds_array);
free(pdf_array);

return status;
}
/*************************************************************************

C Callable Version

*************************************************************************/
int mpGetPar( MP *mp, 
char pdf_parameters[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1],
char pds_standard_keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1],
char pathname_of_SPICE_PCK_file[200] )
{
int	i,j,k;
int	count;
int 	status;
int	len, num, *type, *class;
int	radii_set;
char 	string[200];
char	string_parm_value[200];
char	target_body[40];
char 	keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
float 	parm_value;
double	radii[3];

radii_set = FALSE;

status = mpInit( mp );
CHECKif( status < mpSUCCESS );

j=0;

while(  j < mpNUMBER_OF_KEYWORDS 
	&& pds_standard_keywords[j][0] != '\0'
	&& pds_standard_keywords[j][0] != '\\'
	&& pdf_parameters[j][0] != '\0'
	&& pdf_parameters[j][0] != '\\' )
	{
	/*

	Get VICAR PDF parameter value.

	*/
	
	if ( strcmp(pds_standard_keywords[j],mpTARGET_NAME) == 0 ||
	     strcmp(pds_standard_keywords[j],mpTARGET_BODY) == 0 ||
	     strcmp(pds_standard_keywords[j],mpMAP_PROJECTION_TYPE) == 0 ||
	     strcmp(pds_standard_keywords[j],mpCOORDINATE_SYSTEM_NAME) == 0 ||
	     strcmp(pds_standard_keywords[j],mpPOSITIVE_LONGITUDE_DIRECTION) 
	     == 0 ||
	     strcmp(pds_standard_keywords[j],mpCOORDINATE_SYSTEM_NAME) == 0)
		{
		status = zvp(pdf_parameters[j],string_parm_value,&count);
		if( count == 0 || status == PARAMETER_NOT_FOUND ) 
			{
			j++;
			continue;
			}
		if ( status < mpSUCCESS )
			{
			mpFree( *mp );
			zvmessage("*** MPGETPAR error"," ");
			zvmessage("mpFree called prior to return of failure"," ");
			return mpFAILURE;
			}

		status = mpSetValues( *mp, pds_standard_keywords[j], 
				string_parm_value, "" );
		if( status < mpSUCCESS )
			{
			mpFree( *mp );
			zvmessage("*** MPGETPAR error"," ");
			zvmessage("mpFree called prior to return of failure"," ");
			return mpFAILURE;
			}
		else
			if( strcmp(pds_standard_keywords[j],mpTARGET_BODY)==0
			 || strcmp(pds_standard_keywords[j],mpTARGET_NAME)==0 )
				strcpy(target_body,string_parm_value);
		}
	else
		{
		status = zvp(pdf_parameters[j],&parm_value,&count);
		if( count == 0 || status == PARAMETER_NOT_FOUND ) 
			{
			j++;
			continue;
			}
		if ( status < mpSUCCESS )
			{
			mpFree( *mp );
			zvmessage("*** MPGETPAR error"," ");
			zvmessage("mpFree called prior to return of failure"," ");
			return mpFAILURE;
			}

		status = mpSetValues( *mp,pds_standard_keywords[j],parm_value,"" );
		if( status < mpSUCCESS )
			{
			mpFree( *mp );
			zvmessage("*** MPGETPAR error"," ");
			zvmessage("mpFree called prior to return of failure"," ");
			return mpFAILURE;
			}

		if( strcmp(pds_standard_keywords[j],mpA_AXIS_RADIUS) == 0 ||
	            strcmp(pds_standard_keywords[j],mpB_AXIS_RADIUS) == 0 ||
		    strcmp(pds_standard_keywords[j],mpC_AXIS_RADIUS) == 0 )
			radii_set = TRUE;
		}		

	j++;
	}

if( radii_set == FALSE )
	{
	status = mpPConstants( *mp, pathname_of_SPICE_PCK_file, target_body );
	if( status < mpSUCCESS )
		{
		mpFree( *mp );
		zvmessage("*** MPGETPAR error"," ");
		zvmessage("mpFree called prior to return of failure"," ");
		return mpFAILURE;
		}
	}
	
return mpSUCCESS;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mpgetpar.imake
/* Imake file for MIPS subroutines MPGETPAR */

#define SUBROUTINE  	mpgetpar

#define MODULE_LIST  	mpgetpar.c 

#define USES_ANSI_C
#define FTN_STRING

#define P2_SUBLIB	

/*********

LOCAL LIBRARY for development

*********/
#define DEBUG
#define LIB_LOCAL
$ Return
$!#############################################################################
$Test_File:
$ create tmpgetpar.c
#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

int mp_debug;

/**********************************************************************
 
Test Program TMPGETPAR

Program calls mpgetpar which calls mpInit to allocate memory 
for a map projection data object and then sets values in the
data object based on values passed by the application programs
parameter list.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		March 14, 1994
Revision history:	...TLT...       original

*/

main44()
{
int	count;
int 	i,j,k;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];

double	double_value;

extern int mp_debug;

MP mp_obj;

zvmessage("***************************************************"," ");
zvmessage("\n\tTest of MPGETPAR Routine\n"," ");
zvmessage("***************************************************\n"," ");

/*

Initialize mp_debug variable to FALSE

*/

mp_debug = FALSE;

/*

Define user parameters to be retrieved from PDF file.

*/

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_BODY);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[6],"RESOLUTION");
strcpy(pds_keys[6],mpMAP_RESOLUTION);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);

strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH);

strcpy(pdf_parms[17],"RANGE");
strcpy(pds_keys[17],mpSPACECRAFT_DISTANCE);

strcpy(pdf_parms[18],"AXIS_LINE");
strcpy(pds_keys[18],mpOPT_AXIS_INTERCEPT_LINE);

strcpy(pdf_parms[19],"AXIS_SAMPLE");
strcpy(pds_keys[19],mpOPT_AXIS_INTERCEPT_SAMPLE);

strcpy(pdf_parms[20],"FOCAL_SCALE");
strcpy(pds_keys[20],mpFOCAL_PLANE_SCALE);

strcpy(pdf_parms[21],"SUB_SPACE_LAT");
strcpy(pds_keys[21],mpSUB_SPACECRAFT_LATITUDE);

strcpy(pdf_parms[22],"SUB_SPACE_LON");
strcpy(pds_keys[22],mpSUB_SPACECRAFT_LONGITUDE);

strcpy(pdf_parms[23],"CENT_LINE");
strcpy(pds_keys[23],mpPLANET_CENTER_LINE);

strcpy(pdf_parms[24],"CENT_SAMPLE");
strcpy(pds_keys[24],mpPLANET_CENTER_SAMPLE);

strcpy(pdf_parms[25],"N_ANGLE");
strcpy(pds_keys[25],mpNORTH_ANGLE);

strcpy(pdf_parms[26],"LONG_AXIS");
strcpy(pds_keys[26],mpBODY_LONG_AXIS);

strcpy(pdf_parms[27],"LL_TYPE");
strcpy(pds_keys[27],mpCOORDINATE_SYSTEM_NAME);

pdf_parms[28][0] = '\0';
pds_keys[28][0] = '\0';

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
status = zvp("PCK_PATH",PCKpath,&count);
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}

zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);

PCKfile[lengthes[0]] = '\0';
status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKfile );
if ( status < 0 )
	{
	zvmessage("Error in mpgetpar call"," ");
	zvmessage("Test failed."," ");
	return -1;
	}

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 )
	{
	zvmessage("Error in mpGetKeywords call"," ");
	zvmessage("Test failed."," ");
	return -1;
	}

for ( i=0; i<number_keywords; i++ )
	switch ( types[i] )	{

	case mpCHAR:

		status = mpGetValues( mp_obj,keys[i],string_value,"" );
		CHECKif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %s\n",keys[i],string_value);
		zvmessage(string," ");
		
		break;

	case mpDBLE:

		status = mpGetValues( mp_obj,keys[i],&double_value,"" );
		CHECKif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %4.3e\n",keys[i],double_value);
		zvmessage(string," ");

		break;

	default:

		zvmessage("PDS KEY of unacceptable data type"," ");
		break;	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test of MPGETPAR Routine\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
$!-----------------------------------------------------------------------------
$ create tmpgetpar.imake
#define PROGRAM   tmpgetpar

#define MODULE_LIST tmpgetpar.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C
#define LIB_SPICE
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB

/**********

LOCAL LIBRARY for development 

#define DEBUG
#define LIB_LOCAL
**********/
$!-----------------------------------------------------------------------------
$ create tmpgetpar.pdf
process help=*
PARM TARGET		STRING	COUNT=1
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM LONG_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=1
PARM RESOLUTION		REAL	COUNT=1
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1
PARM CARTESIAN_AZ	REAL	COUNT=0:1
PARM LINE_OFFSET	REAL	COUNT=0:1
PARM SAMPLE_OFFSET	REAL	COUNT=0:1
PARM PARALLEL_ONE	REAL	COUNT=0:1
PARM PARALLEL_TWO	REAL	COUNT=0:1
PARM PCK_PATH		STRING 	COUNT=1
end-proc
.TITLE
VICAR program TMPGETPAR
.HELP
PURPOSE:
This program is a simple test program for the MPGETPAR
subroutine. It calls zmpgetpar and prints the contents of
the map projection object after zmpgetpar is called.
.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI LONG_AXIS
Body long axis of target body.

.VARI SCALE
Map scale.

.VARI RESOLUTION
Map resolution.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.END
$!-----------------------------------------------------------------------------
$ create tstmpgetpar.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="no"
refgbl $syschar

local pckernel type=string	! variable for p_constants.ker

if ($syschar(1) = "UNIX" )
	write " copy the PCK to local directory, since lack of"
	write " READONLY qualifier in OPEN statement may prevent"
	write " SPICE from reading it in system directory ..."
	ush cp "$SPICEKER/p_constants.ker" .
	let pckernel = "p_constants.ker"
else       
	let pckernel = "SPICEKER:p_constants.ker"
end-if   

write " "
write " NOTE: Values are set only when they are"
write " valid for a particular map projection."
write " First and second standard parallels are"
write " not set for the sinusoidal projection"
write " but are set for Albers."


write " "
write " *** TEST ONE ***"
write " "
write " Values are set as given by the PDF parameters,"
write " and p_constants.ker file is read for the axes"
write " measures: A_AXIS_RADIUS, B_AXIS_RADIUS, and"
write " C_AXIS_RADIUS."
write " BODY_LONG_AXIS is also read from the kernel file"
write " "
write " (NOTE:  target changed to JUPITER, because current PCK does not"
write " have a body_long_axis for Mars, and mpPConstants fails. -lwk-)"
write " "
let $echo="yes"

tmpgetpar +
	TARGET="JUPITER"	+
	PROJ="SINUSOIDAL"	+
	SCALE=2.0		+
	RESOLUTION=20.2		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=10.8	+
	SAMPLE_OFFSET=5.9	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
write " "
write " *** TEST TWO ***"
write " "
write " Values are set as given by the PDF parameters, "
write " and p_constants.ker file is not read because Radius "
write " values are supplied."
write " "
let $echo="yes"

tmpgetpar +
	TARGET="MARS" 		+
	PROJ="ALBERS"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=2.0		+
	RESOLUTION=20.2		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=10.8	+
	SAMPLE_OFFSET=5.9	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
write " "
write " *** TEST THREE ***"
write " "
write "Additional parameter LONG_AXIS representing the body"
write "long axis value is added as a test"
write " "
let $echo="yes"

tmpgetpar +
	TARGET="MARS" 		+
	PROJ="ALBERS"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	LONG_AXIS=110.0		+
	SCALE=2.0		+
	RESOLUTION=20.2		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=10.8	+
	SAMPLE_OFFSET=5.9	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mpgetpar.hlp
1 VICAR SUBROUTINE		mpgetpar

Purpose				

Routine to extract map projection values defined
by VICAR/TAE procedure definition file (.PDF)
for an application program and initialize a 
map projection data object using mpInit for 
subsequent use in performing point map 
transformations using mpll2xy and mpxy2ll, 
part of the mp_routine suite of map projection 
software.

2 OPERATION

Reads in values for a specific map projection
stored in VICAR parmeter values from an
application program .pdf file and initializes
a map projection data object with those values. 
This routine uses the VICAR Run Time Library 
(VICAR RTL) and the mp routines mpInit, mpSetValues
and mpPConstants.

Libraries and subroutines required to run this
routine: mp_routines, NAIF SPICELIB library

Calling Sequence:		

from C  	status = mpGetPar( mp, pdf_parameters, 
				pds_standard_keywords,
				SPICE_PCK_pathname );

from FORTRAN	status = mp_get_par( mp, pdf_parameters, 
				pds_standard_keywords,
				SPICE_PCK_pathname );

Necessary include files
from calling routine 
or program:			mp.h


INPUT 

	pdf_parameters		(2-D character array)

	A two-dimensional character array that contains
	the actual names of VICAR pdf parameters
	for the map projection specific values. 

	pds_standard_keywords	(2-D character array)

	A two-dimensional character array that contains 
	the Actual PDS standard keyword names used in
	map projection software suite (mp_routines.com)
	found in the file mp.h. A NULL entry for a 
	character string ('\0') in this array signals
	the end of the listing of keywords.

	SPICE_PCK_pathname	(character string)

	The full pathname of the NAIF SPICE PCK file
	(planetary constants kernel file) to be used
	to extract target body radii if not provided
	via the pdf parameters.

OUTPUT

	mp			(MP data type)

	The address of map projection data object.
	
RETURN 		

	status 			(integer)

	This is an indicator of the success or failure of
	retrieving various values for the VICAR pdf file
	and initializing the map projection data object.

	0 	successful call to mpgetpar and
	-1 	failure in reading of VICAR pdf parameter 
		values or error in setting of values in 
		map projection data object.

2 MORE ON ARGUMENTS

For character string array inputs ...

of pdf parameter names and PDS standard keyword names, 
the dimension of these arrays should be declared in the 
application program with the outer dimension being set to 
mpNUMBER_OF_KEYWORDS. mpNUMBER_OF_KEYWORDS is the maximum 
number of keywords that can be set in a map projection 
data object by functions in the mp_routines suite. 

For the parameter "pdf_parameters", the inner dimension 
should be set to mpMAX_PARM_LENGTH + 1. mpMAX_PARM_LENGTH 
is the maximum allowable number of characters in a VICAR 
procedure definition file (pdf) parameter name. 

For the parameter "pds_standard_keywords", the inner 
dimension should be set to mpMAX_KEYWD_LENGTH + 1. 
mpMAX_KEYWD_LENGTH is the maximum allowable number of 
characters in a standard PDS keywords name. 

The values mentioned above can be found in the include 
file mp.h. Thus a sample initialization of these arrays 
in preparation for a call to MPGETPAR is shown below.

  char PDF_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
  char PDS_keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];

3 ENVIRONMENT and LANGUAGE

Software Platform:		VICAR 11.0
Hardware Platforms:		No particular hardware required;
				tested on VAX-VMS, SunOS, Solaris,
				HP-700, DEC Alpha, and SGI.
Programming Language:		ANSI C

3 HISTORY

Author:				Justin McNeill, JPL
Cognizant Engineer:		Justin McNeill, JPL
Written:			March 14, 1994
Revision history:		

May 18, 1994	     ..JFM..	Exit conditions for internal loop 
				revised to exit via the final return
				of the routine, thus reaching the
				test for radii_set flag for possible
				execution of mpPConstants (FR 82917).
				Also, free() called after calls to 
				sfor2c_array.

March 14, 1994       ..TLT..    original
$ Return
$!#############################################################################
