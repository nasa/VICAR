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
