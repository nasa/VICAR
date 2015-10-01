$!****************************************************************************
$!
$! Build proc for MIPL module mp_routines
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:17
$!
$! Execute by entering:		$ @mp_routines
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
$ write sys$output "*** module mp_routines ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Create_Other .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mp_routines.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("mp_routines.imake") .nes. ""
$   then
$      vimake mp_routines
$      purge mp_routines.bld
$   else
$      if F$SEARCH("mp_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mp_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mp_routines.bld "STD"
$   else
$      @mp_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mp_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mp_routines.com -mixed -
	-s mpinit.c mpfree.c mpll2xy.c mpxy2ll.c mpsetvalues.c mpgetvalues.c -
	   mpgetkeywords.c mpsetdebugflag.c mpinternals.c mpmpo2buf.c -
	   mp_init.h mp_private.h mp_prototypes.h persp_proj_c.c -
	   ortho_obl_c.c mp_triaxcoef.c -
	-i mp_routines.imake -
	-o mp_internals.f persp_proj.f ortho_obl.f
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mpinit.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_init.h"		/* Structure Initialization Include 	*/
#include "mp_prototypes.h"
#include "zvproto.h"
/*

SUBROUTINE	        	mpInit 

Purpose				To allocate memory for a map projection data
				object and to initialize the structure values.

Function			Allocates memory for map projection data object
				using malloc() and initializes the values of 
				the data object to zero.


Date Written:			October 1993

History:			December 1993	JFM
	
				Initializations of all MP data object values
				added for robustness.
				(FR 76817)

				May 1994	JFM	


	16oct96 -lwk- added COORDINATE_SYSTEM_NAME initialized to PLANETODETIC,
			initialized POSITIVE_LONGITUDE_DIRECTION to EAST
	1dec97  -lwk- Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE
        8feb99  -rrp- Uncommented #define FTN_STRING from mp_routines.imake
                      (AR-101298).

	SEE RELEASE_NOTES.TXT FOR DESCRIPTION OF FURTHER CHANGES

*/



int mpInit( MP *mp_obj )
{
  int i,j;
  int status;
  struct MP_STRUCTURE *pmp;

	/* Allocate memory for pointer to structure MP_STRUCTURE	*/
  *mp_obj = (MP *) malloc (sizeof(struct MP_STRUCTURE));
 
	/* If memory allocation of MP Data Object fails, print error	*/
	/* message;  else, return status equals one and set pointer to	*/
	/* MP Data Object.						*/
  if ( *mp_obj == NULL ) 
	{
	zvmessage(" "," ");
	zvmessage("*** ERROR in MP Data Object Initialization"," ");
	zvmessage("*** Memory allocation failed"," ");
	zvmessage(" "," ");
	return mpFAILURE;
  	}
  else 	
	{
	pmp = (struct MP_STRUCTURE *) *mp_obj;

        /* Set MP_DEBUG flag to FALSE           */

        status = mpSetDebugFlag( FALSE );
        CHECKif( status<mpSUCCESS );

	/* Initialization of MP data object 	*/

	/* General control variables 		*/
 
	for (i=0; i<mpNUMBER_OF_GROUPS; i++) 	
		{
		pmp->kwd_set[i] = 0;
	  	pmp->kwd_chng[i] = 0;
		}
	pmp->precalc_needed = TRUE;
	pmp->transformation_direction = 0;
	strcpy(pmp->model,"NONE");
		
	/* Global keyword values 		*/

	strcpy(pmp->glob.target_name,"NONE");
	strcpy(pmp->glob.map_projection_type,"NONE");
	strcpy(pmp->glob.coordinate_system_name,"PLANETOGRAPHIC");
	strcpy(pmp->glob.positive_longitude_direction,"EAST");
	pmp->glob.a_axis_radius = 0.0;
	pmp->glob.b_axis_radius = 0.0;
	pmp->glob.c_axis_radius = 0.0;
	pmp->glob.body_long_axis = 0.0;

	/* Map projection keyword values 	*/

	pmp->mpro.map_scale = 0.0;
	pmp->mpro.map_resolution = 0.0;
	pmp->mpro.center_latitude = -999.0;
	pmp->mpro.center_longitude = -999.0;
	pmp->mpro.spherical_azimuth = 0.0;
	pmp->mpro.cartesian_azimuth = 0.0;
	pmp->mpro.line_projection_offset = 0.0;
	pmp->mpro.sample_projection_offset = 0.0;
	pmp->mpro.first_standard_parallel = 0.0;
	pmp->mpro.second_standard_parallel = 0.0;
	
	/* Perspective keyword values 		*/

	pmp->pers.focal_length = 0.0;
	pmp->pers.focal_plane_scale = 0.0;
	pmp->pers.north_angle = 0.0;
	pmp->pers.opt_axis_intercept_line = 0.0;
	pmp->pers.opt_axis_intercept_sample = 0.0;
	pmp->pers.planet_center_line = 0.0;
	pmp->pers.planet_center_sample = 0.0;
	pmp->pers.sub_spacecraft_latitude = 0.0;	
	pmp->pers.sub_spacecraft_longitude = 0.0;	
	pmp->pers.target_center_distance = 0.0;

	/* Precalculation values 		*/
	
	for ( i=0;i<3;i++ )
		for ( j=0;j<3;j++ )
			pmp->precalc.Euler_matrix[i][j] = 0.0;

	pmp->precalc.cosine_of_CARTESIAN_AZIMUTH = 0.0;	
	pmp->precalc.sine_of_CARTESIAN_AZIMUTH = 0.0;	
	pmp->precalc.rotation_flags = 0;	

	for ( j=0;j<6;j++ )
		{
		pmp->precalc.value[j] = 0.0;
		pmp->precalc.oblate_value[j] = 0.0;
		pmp->precalc.ellipsoid_value[j] = 0.0;
		}

	return mpSUCCESS;
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpfree.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"
/*

SUBROUTINE      		mpFree

Purpose				To free memory for a map projection data object.

Function			Deallocates memory for map projection data 
				object using free().

Background and References:	MIPS Map Projection Software Users' Guide, 
				JPL, D-11810, J. McNeill, May 1994.

Programming Language:		ANSI C


Date:				October 1993
History:			Original

*/




int mpFree( MP mp_obj )
{
/* Deallocate memory for pointer to structure MP_OBJECT			*/
free( mp_obj );

return mpSUCCESS;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpll2xy.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"
#include <zvproto.h>
#include <string.h>
/*

SUBROUTINE      		mpll2xy 

Purpose				Converts one of three types of latitude and 
				longitude points on a target body to line 
				and sample values in pixels of a desired 
				map projection. 

Function			This routine automatically chooses a target
				body model based on the radii found in the
				map projection data object. 

				Supported map projections are ...

				ALBERS 
				CYLINDRICAL EQUAL-AREA
				EQUIDISTANT CYLINDRICAL
				LAMBERT AZIMUTHAL EQUAL-AREA
				LAMBERT CONFORMAL CONIC
				MERCATOR (includes Transverse Mercator)
				MOLLWEIDE (homalographic)
				ORTHOGRAPHIC
				SINUSOIDAL
				STEREOGRAPHIC

Date Written:			October 1993

History:			May 1994 	JFM
	
				Added status flag to FORTRAN version of
				mpll2xy. (FR 82916)

				March 1995	JFM

				Modified return status testing after
				mpOblate, mpSphere, and mpEllipsoid to
				ensure lower level error status flags
				are not mapped to mpFAILURE. (FR 85803)

                                September 1995  JFM
 
                                Modified how function handles model
                                set testing in order to allow multiple
                                data objects within one application.
                                (FR 87378)

	NOV-1995 -lwk- changed positive_longitude_direction to global group

        Sep-1996 -lwk- changed out_ll_type to Planetocentric for Perspective
		and Oblate Orthographic;  removed CHECKif so status != 0 is 
		not always changed to -1

	Oct-1996 -Scholten/lwk- corrected checks for centric/detic conversion;
		revised code for clarity (output lat/lon's are not always
		planetodetic!)

        Jul-1998 -lwk- changed out_ll_type to Planetocentric for Equidistant
*/

int mpll2xy( MP mp, double *line, double *sample, double latitude, 
	double longitude, int ll_type )
{
int	out_ll_type;		/* type of the output lat/lon */
int 	status;
double  dvala,dvalb,dvalc;
double  out_latitude,out_longitude; /* lat/lon output to lower-level routines */
double	radii[3];
char	string[150];
struct 	MP_STRUCTURE 	*pmp;
MP_NUMBER projection_code;
extern  int mp_debug;

/* this array of names depends on the #define's in mp_private.h: */
char ll_name[5][15] = { "PLANETOCENTRIC", "PLANETOGRAPHIC", "SNYDER_DEFINED",
 "CONFORMAL", "AUTHALIC"};

/*

Initialize values.

*/

pmp = (struct MP_STRUCTURE *) mp;
status = mpSUCCESS;
out_ll_type = PLANETODETIC;		

/* check input ll_type: */
if (ll_type<1 || ll_type>5) return mpINVALID_COORD_SYSTEM;

/*

Check that latitude and longitude are within range, and that
longitude, if specified as positive WEST, has its sign toggled.

NOTE: This software's formulae are written in a positive EAST
longitude convention, consistent with USGS Paper 1395.

*/

CHECKif( latitude > 90.0 || latitude < -90.0 );

if( strcmp(pmp->glob.positive_longitude_direction,"WEST")==0 ||
    strcmp(pmp->glob.positive_longitude_direction,"west")==0 )
	longitude = 360.0 - longitude;

longitude = check_longitude( longitude );

/*

Determine what target body radii values have been set and
ensure that all radii measures have a value.

*/
if ( strcmp(pmp->model,"NONE") == 0 ) {
  status = determine_model( mp,radii );
  if( status != mpSUCCESS ) return status;
  if( !strcmp(pmp->model,"TRIAXIAL") || ll_type>2 ) {
    status = load_coefficients( mp,radii );
    if( status != mpSUCCESS ) return status;
  }
}	

/*

Determine the map projection number from the map projection type

*/
status = determine_map_projection_number( mp,&projection_code );
if( status != mpSUCCESS ) return status;

/*

Convert latitude and longitude to planetodetic values.

*/

/* subroutines PERS_PROJ & ORTHO_OBL use planetocentric latitudes, as
   is also assumed by the equations for Equidistant Cylindrical: */
if ( projection_code == perspective || projection_code == equidistant ||
    (projection_code == orthographic && !strcmp( pmp->model,"OBLATE" )) )
  out_ll_type = PLANETOCENTRIC;

if ( strcmp( pmp->model,"SPHERICAL" ) == 0 ) ll_type = PLANETODETIC;

if ( ll_type == out_ll_type ) {
  out_latitude = latitude;
  out_longitude = longitude;
}
else {
  dvala = 1.0;
  dvalb = pmp->glob.b_axis_radius / pmp->glob.a_axis_radius;
  dvalc = pmp->glob.c_axis_radius / pmp->glob.a_axis_radius;
  status = triaxtran_c( dvala,dvalb,dvalc,
	pmp->precalc.cc_matrix,pmp->precalc.cp_matrix,
	pmp->precalc.ac_matrix,pmp->precalc.ap_matrix,
	latitude,longitude,ll_type,
	&out_latitude,&out_longitude,out_ll_type);
  if( status != mpSUCCESS ) return status;
  if (mp_debug) {
    sprintf(string,"\n\t%s -> %s lat/lon (degrees)",&(ll_name[ll_type][0]),
     &(ll_name[out_ll_type][0]));
    zvmessage(string," ");
    sprintf(string,"\n\t(%4.3e,%4.3e) -> (%4.3e,%4.3e)",
     latitude,longitude,out_latitude,out_longitude);
    zvmessage(string," ");
  } 
}

/* 

Convert latitude and longitude to radians.

*/
out_latitude = RETURN_RADIANS( out_latitude );
out_longitude = RETURN_RADIANS( out_longitude );

pmp->transformation_direction = LAT_LON_TO_LINE_SAMP;

switch ( pmp->model[0] )	{

case 'S':	/* Spherical Model  */

	status = mpSphere( mp,line,sample,
		&out_latitude,&out_longitude,
		projection_code );
   	break;

case 'O':	/* Oblate Model     */

	status = mpOblate( mp,line,sample,
		&out_latitude,&out_longitude,
		projection_code );
   	break;

case 'T':	/* Triaxial ellipsoid Model  */

	status = mpEllipsoid( mp,line,sample,
		&out_latitude,&out_longitude,
		projection_code );
	break;	

default:
	status = mpFAILURE;
	break;	}

return status;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpxy2ll.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"
#include <zvproto.h>
/*

SUBROUTINE		        mpxy2ll

Purpose				Converts line and sample points in a map 
				projected image to one of three types of 
				latitude and longitude points on a target body.

Function			This routine automatically chooses a target
				body model based on the radii found in the
				map projection data object. 

				Supported map projections are ...

				ALBERS 
				CYLINDRICAL EQUAL-AREA
				EQUIDISTANT CYLINDRICAL
				LAMBERT AZIMUTHAL EQUAL-AREA
				LAMBERT CONFORMAL CONIC
				MERCATOR (includes Transverse Mercator)
				MOLLWEIDE (homalographic)
				ORTHOGRAPHIC
				SINUSOIDAL
				STEREOGRAPHIC

History:			May 1994	JFM
		
				Added status flag argument to FORTRAN call.
				(FR 82916)

				March 1995	JFM

				Modified status evaluation after calls
				to mpSphere, mpOblate, and mpEllipsoid
				in order to ensure that lower level
				status flags are passed upwards without
				mapping to mpFAILURE. (FR 85803)

				September 1995	JFM

				Modified how function handles model
				set testing in order to allow multiple
				data objects within one application.
				(FR 87378)

        NOV-1995 -lwk- changed positive_longitude_direction to global group

        Sep-1996 -lwk- return status from mpOblate call;  removed CHECKif
		so status != 0 is not always changed to -1;  changed in_ll_type
		to Planetocentric for Perspective and Oblate Orthographic

	Oct-1996 -F.Scholten/lwk- corrected checks for centric/detic conversion;
                revised code for clarity (output lat/lon's are not always
                planetodetic!) 

        Jul-1998 -lwk- changed in_ll_type to Planetocentric for Equidistant
*/

int mpxy2ll( MP mp, double line, double sample, double *latitude, 
	double *longitude, int ll_type )
{
int	in_ll_type;		/* centric/detic type of in_lat/lon */
int 	status;
double  dvala,dvalb,dvalc;
double  in_latitude,in_longitude; /* lat/lon input from lower-level routines */
double	radii[3];
struct 	MP_STRUCTURE 	*pmp;
char	string[150];
MP_NUMBER projection_code;
extern  int mp_debug;

/*

Initialize values

*/
pmp = (struct MP_STRUCTURE *) mp;
status = mpSUCCESS;
in_ll_type = PLANETODETIC;

/* check output ll_type: */
if (ll_type<1 || ll_type>5) return mpINVALID_COORD_SYSTEM;

/*

Check input pointers.

*/
CHECKif( latitude == NULL || longitude == NULL || pmp == NULL );

/*

Determine what target body radii values have been set and
ensure that all radii measures have a value.

*/
if ( strcmp(pmp->model,"NONE") == 0 )
	{
	status = determine_model( mp, radii );
	if( status != mpSUCCESS ) return status;
	if( !strcmp(pmp->model,"TRIAXIAL") || ll_type>2 )
		{
		status = load_coefficients( mp, radii );
		if( status != mpSUCCESS ) return status;
		}
	}
		
/*

Determine the map projection number from the map projection type

*/
status = determine_map_projection_number( mp,&projection_code );
if( status != mpSUCCESS ) return status;

pmp->transformation_direction = LINE_SAMP_TO_LAT_LON;

switch ( pmp->model[0] )	{

case 'S':	/* Spherical Model  */

	status = mpSphere( mp,&line,&sample,
		&in_latitude,&in_longitude,
		projection_code );
	ll_type = PLANETODETIC;
   	break;

case 'O':	/* Oblate Model     */

	status = mpOblate( mp,&line,&sample,
		&in_latitude,&in_longitude,
		projection_code );

   	break;

case 'T':	/* Triaxial ellipsoid Model  */

	status = mpEllipsoid( mp,&line,&sample,
		&in_latitude,&in_longitude,
		projection_code );
	break;	

default:
	return mpFAILURE;
			}
if( status != mpSUCCESS ) return status;

/*

Convert latitude and longitude to planetodetic values, except for
Perspective (subroutine PERS_PROJ), Oblate Orthographic (subr.ORTHO_OBL), 
and Equidistant Cylindrical, which use planetocentric.

*/

if (projection_code == orthographic || projection_code == equidistant ||
    projection_code == perspective) in_ll_type = PLANETOCENTRIC;

if ( ll_type == in_ll_type) {
		
  *latitude = RETURN_DEGREES( in_latitude );
  *longitude = RETURN_DEGREES( in_longitude );

  /* Ensure longitude 360 is expressed as 0. */
  *longitude = fmod( *longitude, 360.0 );

}

else {

  dvala = 1.0;
  dvalb = pmp->glob.b_axis_radius / pmp->glob.a_axis_radius;
  dvalc = pmp->glob.c_axis_radius / pmp->glob.a_axis_radius;
	
  in_latitude = RETURN_DEGREES( in_latitude );
  in_longitude = RETURN_DEGREES( in_longitude );

  /* Ensure longitude 360 is expressed as 0. */
  in_longitude = fmod( in_longitude, 360.0 );

  status = triaxtran_c( dvala,dvalb,dvalc,
    pmp->precalc.cc_matrix,pmp->precalc.cp_matrix,
    pmp->precalc.ac_matrix,pmp->precalc.ap_matrix,
    in_latitude,in_longitude,in_ll_type,
    latitude,longitude,ll_type);
  if( status != mpSUCCESS ) return status;

}

/*

Check that longitude, if specified as positive WEST, has its sign toggled.

NOTE: This software's formulae are written in a positive EAST
longitude convention, consistent with USGS Paper 1395.

*/

 if( strcmp(pmp->glob.positive_longitude_direction,"WEST")==0 ||
     strcmp(pmp->glob.positive_longitude_direction,"west")==0 ) {
   if( *longitude < 0.0 )
     *longitude *= -1.0;
   else
     if( *longitude > 0.0 ) 
       *longitude = 360.0 - *longitude;
 }
 
if( mp_debug )
	{
	zvmessage("\n\tin degrees,"," ");
	sprintf(string,"\n\t(LAT,LON)=(%5.3f,%6.3f)",*latitude,*longitude);
	zvmessage(string," ");
	}

return status;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpsetvalues.c
$ DECK/DOLLARS="$ VOKAGLEVE"

				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <stdarg.h>
#include <string.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"

/********************************************************************/
	/*
	 * Function mpSetValues:
	 *
	 * routine to allow user to set a value in the MP buffer
	 * for a given keyword;
	 * this is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable versions 
	 * mp_set_value() and mp_set_value_str()
	 *
	 *   jul93  -lwk-  intial version
	 * 29jul93  -lwk-  changes per revision to mp_struct.h;  removed all
	 *		SUPPLEMENTARY keywords pending decision on kernel ids
	 *   july94 -jfm-  added BODY_LONG_AXIS setting and made FORTRAN
	 * 		version a simple call to the C version.
	 * 28nov95  -lwk-  moved POSITIVE_LONGITUDE_DIRECTION to Global group
	 * 15oct96  -lwk-  added COORDINATE_SYSTE_NAME to Global group
	 *  2dec97  -lwk- Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE,
	 *		added MIN/MAX LAT/LONG
	 *   aug99  -tyr- compute map_resolution structure item from map_scale
	 *		keyword and vice versa
	 *
	 * SEE RELEASE_NOTES.TXT FOR DESCRIPTION OF FURTHER CHANGES
	 *
	 *
	 * the actual calling sequence is:
	 *
	 *    mpSetValues( mp, key-1, val-1, ..., key-N, val-N, NULL)
	 *
	 * where:
	 *  MP_STRUCTURE mp      is the MP object being referenced
	 *  char *key-i       is the i-th keyword
	 *  <any type> val-i  value for i-th keyword 
	 */

int mpSetValues(MP mp_public, ...)
{
  va_list params;
  struct MP_STRUCTURE *mp; 
  int i, grp, kcode, kfnd;
  char *key, *sval;
  double dval;
  extern struct mpKEYWORD_TO_CODE mpKwds_Codes[];
 
  va_start(params,mp_public);
  
  mp = (struct MP_STRUCTURE *)mp_public;
  
  for (;;) {				/* loop over all user keywords */

    key = va_arg( params, char *);	/* get next keyword */

    if (key == NULL || key[0] == '\0') {
      mp->precalc_needed = TRUE;
      return mpSUCCESS;
    }
    else {

		/* search all existing keywords: */

      for (i=0, kfnd=0; i<mpNUMBER_OF_KEYWORDS && !kfnd; i++) {

	if (EQUAL( key, mpKwds_Codes[i].label_kwd)) {
	  kfnd = 1;
	  grp = mpKwds_Codes[i].group_no;
	  kcode = mpKwds_Codes[i].bit_code;

		/* set the "value set" and "value changed" flag bits:
		 * (excepting special cases, see below) */
	  if ( (grp!=0 || kcode != mpTARGET_BODY_BIT) &&
	       (grp!=2 || kcode != mpSPACECRAFT_DISTANCE_BIT) ) {
	    mp->kwd_set[grp] |= kcode;
	    mp->kwd_chng[grp] |= kcode;
	  }

		/* get the keyword value: */

	  if (mpKwds_Codes[i].kwd_type == mpCHAR)
	    sval = va_arg( params, char *);
	  else if (mpKwds_Codes[i].kwd_type == mpDBLE)
	    dval = va_arg( params, double);
	  else return mpINVALID_KEYWORD_TYPE;

		/* and assign it to the correct structure item: */

	  switch (grp) {		/* group number */

	    case 0:		/* GLOBAL group */

	      if (kcode == mpTARGET_NAME_BIT)
		strcpy( mp->glob.target_name, sval);

	      /* support this old alias, but switch it to target_name: */
	      else if (kcode == mpTARGET_BODY_BIT) {
		strcpy( mp->glob.target_name, sval);
		mp->kwd_set[grp] |= mpTARGET_NAME_BIT;
		mp->kwd_chng[grp] |= mpTARGET_NAME_BIT;
	      }

	      else if (kcode == mpMAP_PROJECTION_TYPE_BIT) 
		strcpy(mp->glob.map_projection_type, sval);

	      else if (kcode == mpCOORDINATE_SYS_NAME_BIT)
		strcpy( mp->glob.coordinate_system_name, sval);

	      else if (kcode == mpPOSITIVE_LONGIT_DIR_BIT)
		strcpy( mp->glob.positive_longitude_direction, sval);

	      else if (kcode == mpA_AXIS_RADIUS_BIT)
		mp->glob.a_axis_radius = dval;

	      else if (kcode == mpB_AXIS_RADIUS_BIT)
		mp->glob.b_axis_radius  = dval;

	      else if (kcode == mpC_AXIS_RADIUS_BIT)
		mp->glob.c_axis_radius  = dval;

	      else if (kcode == mpBODY_LONG_AXIS_BIT)
		mp->glob.body_long_axis  = dval;

	      else if (kcode == mpMINIMUM_LATITUDE_BIT)
		mp->glob.minimum_latitude = dval;

	      else if (kcode == mpMAXIMUM_LATITUDE_BIT)
		mp->glob.maximum_latitude = dval;

	      else if (kcode == mpMINIMUM_LONGITUDE_BIT)
		mp->glob.minimum_longitude = dval;

	      else if (kcode == mpMAXIMUM_LONGITUDE_BIT)
		mp->glob.maximum_longitude = dval;

	      else return mpINTERNAL_STRUCT_ERROR;

	      break;

	    case 1:		/* MAP_PROJ group */

	      if (kcode == mpMAP_SCALE_BIT) {
		mp->mpro.map_scale = dval;
		if (mpA_AXIS_RADIUS_BIT & mp->kwd_set[0])
		  mp->mpro.map_resolution = 0.0174533 * mp->glob.a_axis_radius
		   / mp->mpro.map_scale;
		else return mpKEYWORD_NOT_SET;
	      }

	      else if (kcode == mpMAP_RESOLUTION_BIT) {
		mp->mpro.map_resolution = dval;
		if (mpA_AXIS_RADIUS_BIT & mp->kwd_set[0]) 
		  mp->mpro.map_scale = 0.0174533 * mp->glob.a_axis_radius /
		   mp->mpro.map_resolution;
		else return mpKEYWORD_NOT_SET;
	      }

	      else if (kcode == mpCENTER_LATITUDE_BIT)
		mp->mpro.center_latitude = dval;

	      else if (kcode == mpCENTER_LONGITUDE_BIT)
		mp->mpro.center_longitude = dval;

	      else if (kcode == mpSPHERICAL_AZIMUTH_BIT)
		mp->mpro.spherical_azimuth = dval;

	      else if (kcode == mpCARTESIAN_AZIMUTH_BIT)
		mp->mpro.cartesian_azimuth = dval;

	      else if (kcode == mpLINE_PROJECTION_OFFSET_BIT)
		mp->mpro.line_projection_offset = dval;

	      else if (kcode == mpSAMPLE_PROJECTION_OFFSET_BIT)
		mp->mpro.sample_projection_offset = dval;

	      else if (kcode == mpFIRST_STANDARD_PARALLEL_BIT)
		mp->mpro.first_standard_parallel = dval;

	      else if (kcode == mpSECOND_STANDARD_PARALLEL_BIT)
		mp->mpro.second_standard_parallel = dval;

	      else return mpINTERNAL_STRUCT_ERROR;

	      break;

	    case 2:		/* PERSPECTIVE group */

	      if (kcode == mpFOCAL_LENGTH_BIT)
		mp->pers.focal_length = dval;

	      else if (kcode == mpFOCAL_PLANE_SCALE_BIT)
		mp->pers.focal_plane_scale = dval;

	      else if (kcode == mpNORTH_ANGLE_BIT)
		mp->pers.north_angle = dval;

	      else if (kcode == mpOPT_AXIS_INTERCEPT_LINE_BIT)
		mp->pers.opt_axis_intercept_line = dval;

	      else if (kcode == mpOPT_AXIS_INTERCEPT_SAMPLE_BIT)
		mp->pers.opt_axis_intercept_sample = dval;

	      else if (kcode == mpPLANET_CENTER_LINE_BIT)
		mp->pers.planet_center_line = dval;

	      else if (kcode == mpPLANET_CENTER_SAMPLE_BIT)
		mp->pers.planet_center_sample = dval;

	      else if (kcode == mpSUB_SPACECRAFT_LATITUDE_BIT)
		mp->pers.sub_spacecraft_latitude = dval;

	      else if (kcode == mpSUB_SPACECRAFT_LONGITUDE_BIT)
		mp->pers.sub_spacecraft_longitude = dval;

	      else if (kcode == mpTARGET_CENTER_DISTANCE_BIT)
		mp->pers.target_center_distance = dval;

	      /* support this old alias, but switch it to target_center: */
	      else if (kcode == mpSPACECRAFT_DISTANCE_BIT) {
		mp->pers.target_center_distance = dval;
		mp->kwd_set[grp] |= mpTARGET_CENTER_DISTANCE_BIT;
		mp->kwd_chng[grp] |= mpTARGET_CENTER_DISTANCE_BIT;
	      }

	      else return mpINTERNAL_STRUCT_ERROR;

	      break;

	    case 3:		/* SUPPLEMENTARY group */

	      return mpINTERNAL_STRUCT_ERROR;


	  }		/* end switch on group number */

	}	/* end "if keyword found" */

      }		/* end keyword loop */

      if (!kfnd) return mpKEYWORD_NOT_FOUND;
    }
  }
/*  return mpIMPROPER_KEYWD_VALUE_PAIR; */
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpgetvalues.c
$ DECK/DOLLARS="$ VOKAGLEVE"


				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include <stdarg.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"

/********************************************************************/
	/*
	 * Function mpGetValues:
	 *
	 * Routine to allow user to get a value in the MP buffer
	 * for a given keyword;
	 * this is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable versions 
	 * mp_get_value() and mp_get_value_str()
	 *
	 * For most keywords, this routine will simply return the value
	 * previously stored using mpSetValues.  However, if no value
	 * has been set for B_AXIS_RADIUS and C_AXIS_RADIUS, the routine
	 * will return the value for A_AXIS_RADIUS if the latter has been
	 * set.
	 *
	 * 20jul93  -lwk-  intial version
	 * 29jul93  -lwk-  changes per revision to mp_struct.h;  removed most
	 *		SUPPLEMENTARY keywords pending decision on kernel ids
         *  july94  -jfm-  added retrieval for BODY_LONG_AXIS
	 * 28nov95  -lwk-  moved POSITIVE_LONGITUDE_DIRECTION to Global group
	 * 150ct96  -lwk-  added COORDINATE_SYSTEM_NAME to Global group
	 *  2dec97  -lwk- Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE,
	 *		added MIN/MAX LAT/LONG
	 *  2nov99  -lwk- removed code for map_scale/resolution since this
	 *		has been added to mpsetvalues
	 *
	 *  SEE RELEASE_NOTES.TXT FOR DESCRIPTION OF FURTHER CHANGES
	 *
	 * The actual calling sequence is:
	 *
	 *    mpGetValues( mp, key-1, val-1, ..., key-N, val-N, NULL)
	 *
	 * where:
	 *  MP_STRUCTURE mp      is the MP object being referenced
	 *  char *key-i       is the i-th keyword
	 *  <any type> val-i  value for i-th keyword 
	 */

int mpGetValues( MP mp_public, ...)
{
  va_list params;
  struct MP_STRUCTURE *mp;
  int i, j, jsys, grp, kcode, kfnd;
  char *key, *sval;
  double *pdval;
  extern struct mpKEYWORD_TO_CODE mpKwds_Codes[];

  /* array of valid COORDINATE_SYSTEM_NAME values, see the #define's in
   * mp_private.h: */
  char ll_name[6][15] = { "PLANETOCENTRIC", "PLANETOGRAPHIC", "PLANETODETIC",
   "SNYDER_DEFINED", "CONFORMAL", "AUTHALIC"};

  va_start(params, mp_public);  
  
  mp = (struct MP_STRUCTURE *)mp_public;
  
  for (;;) {				/* loop over all user keywords */

    key = va_arg( params, char *);	/* get next keyword */

    if (key == NULL || key[0] == '\0') 
	return mpSUCCESS;
    else {

      kfnd = 0;	/* set some flags */

		/* search all existing keywords: */

      for (i=0; i<mpNUMBER_OF_KEYWORDS && !kfnd; i++) {

	if (EQUAL( key, mpKwds_Codes[i].label_kwd)) {
	  kfnd = 1;
	  grp = mpKwds_Codes[i].group_no;
	  kcode = mpKwds_Codes[i].bit_code;

		/* if any keyword was not set, return an error ...
		 * (if the routine was called with multiple kewords,
		 * user will have to do more work to find the culprit) */
	  if (!(kcode & mp->kwd_set[grp])) {
		/* the following have defaults or special processing,
		 * which will be sorted out below: */
	    if (grp==0 && kcode==mpB_AXIS_RADIUS_BIT);
	    else if (grp==0 && kcode==mpC_AXIS_RADIUS_BIT);
	    else if (grp==0 && kcode==mpTARGET_NAME_BIT);
	    else if (grp==0 && kcode==mpTARGET_BODY_BIT);
	    else if (grp==0 && kcode==mpBODY_LONG_AXIS_BIT);
	    else if (grp==2 && kcode==mpTARGET_CENTER_DISTANCE_BIT);
	    else if (grp==2 && kcode==mpSPACECRAFT_DISTANCE_BIT);
	    else return mpKEYWORD_NOT_SET;
	  }

		/* get the address for the returned keyword value: */

	  if (mpKwds_Codes[i].kwd_type == mpCHAR)
	    sval = va_arg( params, char *);
	  else if (mpKwds_Codes[i].kwd_type == mpDBLE)
	    pdval = va_arg( params, double *);
	  else return mpINVALID_KEYWORD_TYPE;

		/* and assign it to the correct structure item: */

	  switch (grp) {		/* group number */

	    case 0:		/* GLOBAL group */

	      if (kcode == mpTARGET_NAME_BIT) {
		if ( (kcode & mp->kwd_set[grp]) ||
		     (mpTARGET_BODY_BIT & mp->kwd_set[grp]) )
		  strcpy( sval, mp->glob.target_name);
		else
	    	  return mpKEYWORD_NOT_SET;
	      }

		/* support this old alias: */
	      else if (kcode == mpTARGET_BODY_BIT) {
		if ( (kcode & mp->kwd_set[grp]) ||
		     (mpTARGET_NAME_BIT & mp->kwd_set[grp]) )
		  strcpy( sval, mp->glob.target_name);
		else
	    	  return mpKEYWORD_NOT_SET;
	      }

	      else if (kcode == mpMAP_PROJECTION_TYPE_BIT) 
		strcpy( sval, mp->glob.map_projection_type);

	      else if (kcode == mpCOORDINATE_SYS_NAME_BIT) {
		jsys = -1;
		for (j=0; j<6; j++)
		  if (!strcmp( mp->glob.coordinate_system_name, ll_name[j]))
		    jsys = j;
		if (jsys<0) return mpINVALID_COORD_SYSTEM;
		/* replace detic by planetographic */
		if (jsys==2)
		  strcpy( sval, ll_name[1]);
		else
		  strcpy( sval, mp->glob.coordinate_system_name);
	      }

	      else if (kcode == mpPOSITIVE_LONGIT_DIR_BIT) {
		if (strcmp( mp->glob.positive_longitude_direction, "EAST") &&
		    strcmp( mp->glob.positive_longitude_direction, "WEST"))
		  return mpINVALID_LONG_DIRECTION;
		strcpy( sval, mp->glob.positive_longitude_direction);
	      }

	      else if (kcode == mpA_AXIS_RADIUS_BIT)
		*pdval = mp->glob.a_axis_radius;

	      else if (kcode == mpB_AXIS_RADIUS_BIT) {
		if (kcode & mp->kwd_set[grp]) 
		  *pdval = mp->glob.b_axis_radius;
		else if (mpA_AXIS_RADIUS_BIT & mp->kwd_set[grp]) 
		  *pdval = mp->glob.a_axis_radius;
		else
	    	  return mpKEYWORD_NOT_SET;
	      }

	      else if (kcode == mpC_AXIS_RADIUS_BIT) {
		if (kcode & mp->kwd_set[grp])
		  *pdval = mp->glob.c_axis_radius;
		else if (mpA_AXIS_RADIUS_BIT & mp->kwd_set[grp]) 
		  *pdval = mp->glob.a_axis_radius;
		else
	    	  return mpKEYWORD_NOT_SET;
	      }

	      else if (kcode == mpBODY_LONG_AXIS_BIT)
		*pdval = mp->glob.body_long_axis;

	      else if (kcode == mpMINIMUM_LATITUDE_BIT)
		*pdval = mp->glob.minimum_latitude;

	      else if (kcode == mpMAXIMUM_LATITUDE_BIT)
		*pdval = mp->glob.maximum_latitude;

	      else if (kcode == mpMINIMUM_LONGITUDE_BIT)
		*pdval = mp->glob.minimum_longitude;

	      else if (kcode == mpMAXIMUM_LONGITUDE_BIT)
		*pdval = mp->glob.maximum_longitude;

	      else return mpINTERNAL_STRUCT_ERROR;

	      break;

	    case 1:		/* MAP_PROJ group */

	      if (kcode == mpMAP_SCALE_BIT) 
		*pdval = mp->mpro.map_scale;

	      else if (kcode == mpMAP_RESOLUTION_BIT) 
		*pdval = mp->mpro.map_resolution;

	      else if (kcode == mpCENTER_LATITUDE_BIT)
		*pdval = mp->mpro.center_latitude;

	      else if (kcode == mpCENTER_LONGITUDE_BIT)
		*pdval = mp->mpro.center_longitude;

	      else if (kcode == mpSPHERICAL_AZIMUTH_BIT)
		*pdval = mp->mpro.spherical_azimuth;

	      else if (kcode == mpCARTESIAN_AZIMUTH_BIT)
		*pdval = mp->mpro.cartesian_azimuth;

	      else if (kcode == mpLINE_PROJECTION_OFFSET_BIT)
		*pdval = mp->mpro.line_projection_offset;

	      else if (kcode == mpSAMPLE_PROJECTION_OFFSET_BIT)
		*pdval = mp->mpro.sample_projection_offset;

	      else if (kcode == mpFIRST_STANDARD_PARALLEL_BIT)
		*pdval = mp->mpro.first_standard_parallel;

	      else if (kcode == mpSECOND_STANDARD_PARALLEL_BIT)
		*pdval = mp->mpro.second_standard_parallel;

	      else return mpINTERNAL_STRUCT_ERROR;

	      break;

	    case 2:		/* PERSPECTIVE group */

	      if (kcode == mpFOCAL_LENGTH_BIT)
		*pdval = mp->pers.focal_length;

	      else if (kcode == mpFOCAL_PLANE_SCALE_BIT)
		*pdval = mp->pers.focal_plane_scale;

	      else if (kcode == mpNORTH_ANGLE_BIT)
		*pdval = mp->pers.north_angle;

	      else if (kcode == mpOPT_AXIS_INTERCEPT_LINE_BIT)
		*pdval = mp->pers.opt_axis_intercept_line;

	      else if (kcode == mpOPT_AXIS_INTERCEPT_SAMPLE_BIT)
		*pdval = mp->pers.opt_axis_intercept_sample;

	      else if (kcode == mpPLANET_CENTER_LINE_BIT)
		*pdval = mp->pers.planet_center_line;

	      else if (kcode == mpPLANET_CENTER_SAMPLE_BIT)
		*pdval = mp->pers.planet_center_sample;

	      else if (kcode == mpSUB_SPACECRAFT_LATITUDE_BIT)
		*pdval = mp->pers.sub_spacecraft_latitude;

	      else if (kcode == mpSUB_SPACECRAFT_LONGITUDE_BIT)
		*pdval = mp->pers.sub_spacecraft_longitude;

	      else if (kcode == mpTARGET_CENTER_DISTANCE_BIT) {
		if ( (kcode & mp->kwd_set[grp]) ||
		     (mpSPACECRAFT_DISTANCE_BIT & mp->kwd_set[grp]) )
		  *pdval = mp->pers.target_center_distance;
		else
	    	  return mpKEYWORD_NOT_SET;
	      }

		/* support this old alias: */
	      else if (kcode == mpSPACECRAFT_DISTANCE_BIT) {
		if ( (kcode & mp->kwd_set[grp]) ||
		     (mpTARGET_CENTER_DISTANCE_BIT & mp->kwd_set[grp]) )
		  *pdval = mp->pers.target_center_distance;
		else
	    	  return mpKEYWORD_NOT_SET;
	      }

	      else return mpINTERNAL_STRUCT_ERROR;

	      break;

	    case 3:		/* SUPPLEMENTARY group */

	      return mpINTERNAL_STRUCT_ERROR;

	  }		/* end switch on group number */

	}	/* end "if keyword found" */

      }		/* end keyword loop */

      if (!kfnd) return mpKEYWORD_NOT_FOUND;
    }
  }
/*  return mpIMPROPER_KEYWD_VALUE_PAIR; */
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpgetkeywords.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include <time.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"

/********************************************************************/
	/*
	 * Function mp_get_keywords:
	 *
	 * routine to return a list of MP keywords whose values have
	 * been set (by previous calls to mp_set_value);  if the
	 * projection type has been set, then only keywords that are
	 * valid for that type are returned
	 *
	 * The FORTRAN calling sequence is:
	 *
	 *	INCLUDE MP_FOR_DEFS
	 *	INTEGER MP, NUM, STATUS
	 *	INTEGER KTYPE(MP_NUM_KEYWDS), KCLASS(MP_NUM_KEYWDS)
	 *	CHARACTER*MP_MAX_KEY_LEN KEYWDS(MP_NUM_KEYWDS)
	 *	....
	 *	CALL MP_GET_KEYWORDS( MP, KEYWDS, NUM, KTYPE, KCLASS, STATUS)
	 *
	 * (Note that the arrays returned by MP_GET_KEYWORDS must be declared
	 * using their maximum length, as given in MP_FOR_DEFS.FIN.)
	 *
	 *  3sep93  -lwk-  intial version
         * 09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
         *              to MP, since private version not needed here
	 * 18may94  -jfm-  added mp_get_keywords to mpgetkeywords.c
         * 22aug98  -lwk-  changes for revised mpGetKeywords calling sequence
	 */

/********************************************************************/
	/*
	 * Function mpGetKeywords:
	 *
	 * routine to return a list of MP keywords whose values have
	 * been set (by previous calls to mpSetValues);  if the
	 * projection type has been set, then only keywords that are
	 * valid for that type are returned
	 *
	 * It is the reponsibility of the calling program to allocate
	 * memory to hold the arrays returned.  If the program is
	 * called with no allocation for the keyword array, then this
	 * routine only returns the number of keywords (num_kwd).
	 * The user can then allocate and make a second call.
	 * Alternatively, the user can allocate all arrays to their
	 * maximum length, which is mpNUMBER_OF_KEYWORDS (in mp_public.h).
	 *
	 * this is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable mp_get_keywords
	 *
	 *   3aug93  -lwk-  intial version
	 *  23aug93  -lwk-  added restriction that returned keywords be
	 *		  valid for the projection
	 *   3sep93  -lwk-  remove mallocs from this routine;  allow
	 *		  mode in which only num_kwd is returned
         * 22aug98  -lwk-  changes for revised mpGetKeywords calling sequence
	 *
	 * FOR FURTHER CHANGES SEE RELEASE_NOTES.TXT
	 */

int mpGetKeywords(
  MP mp,			/* in:  user's MP structure id */
  char keywds[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1],
				/* out:  array of keywords set */
  int *num_kwd,                 /* out: outer dimension of keywds */
  int *kwd_type,		/* out: array of types, of length num_kwd */
				/*      (type = CHAR, DOUBLE, etc.) */
  int *kwd_class)		/* out: array of classes, of length num_kwd */
				/*      (class = CORE or SUPPLEMENTARY) */
{
  int grp, i, kvmask, kvalid, mset, n;
  struct MP_STRUCTURE *pmp;
  extern struct mpKEYWORD_TO_CODE mpKwds_Codes[];
  extern int mpMAP_PROJ_BITS[];
  extern char mpMAP_PROJ_TYPES[mpNUMBER_OF_PROJECTIONS][mpMAX_KEYWD_LENGTH];

  pmp = (struct MP_STRUCTURE *) mp;

	/* if map projection is set, find the valid keywords mask: */
  if (pmp->kwd_set[0] & mpMAP_PROJECTION_TYPE_BIT) {
    mset = 1;
    for (i=0, kvmask = -1; i<mpNUMBER_OF_PROJECTIONS; i++) {
      if ( EQUAL( mpMAP_PROJ_TYPES[i],  pmp->glob.map_projection_type) ) {
        kvmask = mpMAP_PROJ_BITS[i];
	break;
      }
    }
    if (kvmask == -1) return mpINVALID_PROJECTION;
  }
  else mset = 0;
  for (i=0, n=0; i<mpNUMBER_OF_KEYWORDS ;i++) { 
    grp = mpKwds_Codes[i].group_no;
    kvalid = 1;
    if (mset) {
      if (grp==1 && (kvmask & mpKwds_Codes[i].bit_code) == 0)
	kvalid = 0;
      if (grp==2 && strcmp( pmp->glob.map_projection_type, mpPOINT_PERSPECTIVE))
	kvalid = 0;
    }
    if ( (pmp->kwd_set[grp] & mpKwds_Codes[i].bit_code) && kvalid ) {
      if (keywds) {
	strcpy( keywds[n], mpKwds_Codes[i].label_kwd );
	*(kwd_type+n) = mpKwds_Codes[i].kwd_type;
	if (grp <= 2) *(kwd_class+n) = mpCORE;
	else *(kwd_class+n) = mpSUPPL;
      }
      n++;
    }
  }
  *num_kwd = n;

  return mpSUCCESS;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpsetdebugflag.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"

int mp_debug;			/* MP debug flag declaration		*/

/*

SUBROUTINE      		mpSetDebugFlag 

Purpose				To set a global value to print internal
				calculations of the MP routines to standard
				output.

Function			Sets mp_debug global external variable to TRUE.

Libraries and subroutines
required to run routine:	none




Arguments:
	
	Name		Type		In/Out		Description
	
	Flag		int		Input		Value to which the
							mp_debug flag should
							be set.  Valid values
							are TRUE (1) or 
							FALSE (0).

	Status		int		Output 		Status flag for routine.
							Return values are
							mpSUCCESS or mpFAILURE.


Date:				May 1994

History:			Original */
   

int mpSetDebugFlag( int flag )
{
int status;
extern int mp_debug;

if( flag == TRUE || flag == FALSE )
	{
	mp_debug = flag;
	status = mpSUCCESS;
	}
else
	status = mpFAILURE;

return status;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpinternals.c
$ DECK/DOLLARS="$ VOKAGLEVE"
 				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include <time.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"
#include <zvproto.h>

/***********************************************************************

ARITHMETIC FUNCTIONS

The following are a set of C functions for various arithmetic operations 
required by the map projection software.

Author: Justin McNeill

Date: 	October 1993

07feb96 -lwk-  changed positive_longitude_direction to global group

  sep96 -lwk-  numerous changes:
	1. General: added +1 to L/S offsets in Cartesian transformation;
	 check return status of Euler rotation in perform_precalcs;  
	 ensure that CHECKif does not replace an MP error (status < -1000) 
	  with simple failure to project (status = -1);
	 provide for 90 deg. case in planetodetic_conformal_trans
	2. Perspective projection: replaced map_scale by appropriate formula 
	 (Note:  an alternative would be to add map_scale to the Global group);
	 changed E.lon to West before calling PPROJ;  
	 return immediately when PPROJ returns 1;  
	 don't call perform_precalcs for this case;
	 removed confusing comments and commented-out code pertaining
	  to USGS "Vertical Perspective" equations;
	 added reference to mpGetDesc for this case 
	3. Albers:  fixed algorithm when n<0
	4. Lambert:  fixed algorithm when n<0 and at Poles;
	 (two_par only:) fixed problem in longitude -> sample computation
	5. Sinusoidal:  enforced check on longitude range in inverse mode
	6. Orthographic:  added this projection to mpOblate using ORTHO_OBL.F,
	  revised mpGetDesc accordingly

  sep96 -F.Scholten- fixes to Orthographic & Stereographic for rho=0
	and in checking limits

  oct96 -F.Scholten- added checks in longitude range to Albers & Lambert cases,
	fixed errors in precalcs for Lambert one_par and in
	planetodetic_authalic_trans

  oct96 -lwk- fixed convergence criterion in planetodetic_authalic_trans

 1dec97 -lwk- Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE

22aug98  -lwk-  changes for revised mpGetKeywords calling sequence

SEE RELEASE_NOTES.TXT FOR DESCRIPTION OF FURTHER CHANGES

*/

/*************************

FUNCTION degrees2radians		

Converts degrees to radians.

**************************/

int degrees2radians( double *value )
{
*value *= ( PI / 180.0 );
return mpSUCCESS;
}

/*************************

FUNCTION radians2degrees		

Converts radians to degrees.

*************************/

int radians2degrees( double *value )
{
*value *= ( 180.0 / PI );
return mpSUCCESS;
}

/************************ 

FUNCTION check_longitude_radians

Returns a longitude between 
  
180.0 and -180.0 radians.

************************/

double check_longitude_radians( double longitude )
{
if( longitude >= TWO_PI || longitude <= (-2.0 * PI) )
	longitude -= (TWO_PI) * floor ( longitude/(TWO_PI) );

if ( longitude > PI ) longitude -= TWO_PI;

if ( longitude <= -PI ) longitude += TWO_PI;

return longitude;
}

/************************ 

FUNCTION check_longitude

Returns a longitude between 
180.0 and -180.0 degrees.

************************/

double check_longitude( double longitude_in_degrees )
{
double longitude;

longitude = longitude_in_degrees;

if( longitude_in_degrees >= 360.0 || longitude_in_degrees <= -360.0 )
	longitude -= 360.0 * floor ( longitude_in_degrees/360.0 );

if ( longitude > 180.0 ) 	
	longitude -= 360;

if ( longitude <= -180.0 ) 	
	longitude += 360;

return longitude;
}

/************************ 

FUNCTION check_latitude

Returns a latitude between 
90.0 and -90.0 degrees.

************************/

int check_latitude( double latitude_in_degrees )
{

CHECKif( latitude_in_degrees > 90.0 || latitude_in_degrees < -90.0 );

return mpSUCCESS;
}

/****************************************************************************

determine_map_projection_number


Function to determine the map projection number from the map projection type
and the values set in the data object.

*/
int determine_map_projection_number( MP mp, MP_NUMBER *projection_number )
{
int	i, j, kk;
int 	len, num;
int	type[mpNUMBER_OF_KEYWORDS], class[mpNUMBER_OF_KEYWORDS];
int	status;

char	keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];

struct 	MP_STRUCTURE 	*pmp;

/* 

Initialization of variables

*/
pmp = ( struct MP_STRUCTURE *) mp;
len = mpNUMBER_OF_KEYWORDS * (mpMAX_KEYWD_LENGTH+1);

kk=0;

/*

Branch on map projection type (MAP_PROJECTION_TYPE) 

	comment:

	non-standard if then else structure used
	for ease of reading

		if ( ... )
		  {
		  ...
		  } else

		if ( ... )
		  {
                  ...
		  } else

*/

if( strcmp( pmp->glob.map_projection_type,mpALBERS ) == 0 )
	{

	status = mpGetKeywords( mp, keywords, &num, type, class );
	if (status != mpSUCCESS) return status;

	for( i=0,j=0; i<num; i++ )
		{
		if ( strcmp(keywords[i],mpFIRST_STANDARD_PARALLEL) == 0 )
			j++;
		if ( strcmp(keywords[i],mpSECOND_STANDARD_PARALLEL) == 0 )
			j++;
		}
	
	if( j==1 )
		*projection_number = albers_one_p; 	else
	if( j==2 ) 
		*projection_number = albers_two_p;
	else
		return mpFAILURE;

	}	else

if( strcmp( pmp->glob.map_projection_type,mpALBERS_ONE_PARALLEL ) == 0 )

	*projection_number = albers_one_p;	else

if( strcmp( pmp->glob.map_projection_type,mpALBERS_TWO_PARALLELS ) == 0 )

	*projection_number = albers_two_p;	else

if( strcmp( pmp->glob.map_projection_type,mpCYLINDRICAL_EQUAL_AREA ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpNORMAL_CYLINDRICAL) == 0 )

	*projection_number = cylindrical_equal_area;	else

if( strcmp( pmp->glob.map_projection_type,mpEQUIDISTANT ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpCYLINDRICAL ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpRECTANGULAR ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpSIMPLE_CYLINDRICAL) == 0 || 
    strcmp( pmp->glob.map_projection_type,mpOBLIQUE_CYLINDRICAL) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpOBLIQUE_SIMPLE_CYLINDRICAL) == 0 )


	*projection_number = equidistant;	else

if( strcmp( pmp->glob.map_projection_type,mpLAMBERT_AZIMUTHAL ) == 0 )

	*projection_number = lambert_azimuthal;	else

if( strcmp( pmp->glob.map_projection_type,mpLAMBERT ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpLAMBERT_CONFORMAL ) == 0 )
	{

	status = mpGetKeywords( mp, keywords, &num, type, class );
	if (status != mpSUCCESS) return status;
	
	for( i=0,j=0; i<num; i++ )
		{
		if ( strcmp(keywords[i],mpFIRST_STANDARD_PARALLEL) == 0 )
			j++;
		if ( strcmp(keywords[i],mpSECOND_STANDARD_PARALLEL) == 0 )
			j++;
		}
	
	if( j==1 )
		*projection_number = lambert_one_p; 	else	
	if( j==2 ) 
		*projection_number = lambert_two_p;
	else
		return mpFAILURE;

	}	else

if( strcmp( pmp->glob.map_projection_type,mpLAMBERT_ONE_PARALLEL ) == 0 )

	*projection_number = lambert_one_p;	else

if( strcmp( pmp->glob.map_projection_type,mpLAMBERT_TWO_PARALLELS ) == 0 )

	*projection_number = lambert_two_p;	else

if( strcmp( pmp->glob.map_projection_type,mpMERCATOR ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpTRANSVERSE_MERCATOR ) == 0 )

	*projection_number = mercator;	else

if( strcmp( pmp->glob.map_projection_type,mpMOLLWEIDE ) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpHOMALOGRAPHIC ) == 0 )

	*projection_number = mollweide;	else

if( strcmp( pmp->glob.map_projection_type,mpORTHOGRAPHIC) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpPOLAR_ORTHOGRAPHIC) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpOBLIQUE_ORTHOGRAPHIC) == 0 )

	*projection_number = orthographic;	else

if( strcmp( pmp->glob.map_projection_type,mpSINUSOIDAL) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpOBLIQUE_SINUSOIDAL) == 0 )

	*projection_number = sinusoidal;	else

if( strcmp( pmp->glob.map_projection_type,mpSTEREOGRAPHIC) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpPOLAR_STEREOGRAPHIC) == 0 ||
    strcmp( pmp->glob.map_projection_type,mpOBLIQUE_STEREOGRAPHIC) == 0 )

	*projection_number = stereographic;	else

if( strcmp( pmp->glob.map_projection_type,mpPOINT_PERSPECTIVE) == 0 )

	*projection_number = perspective;	
else
	return mpFAILURE;


return mpSUCCESS;
}


/***************************************************************************

determine_model

Determine target body model based on target body radii measures.

*/
int determine_model( MP mp, double radii[] )
{
int	i;
int	num, type[mpNUMBER_OF_KEYWORDS], class[mpNUMBER_OF_KEYWORDS];
int	status;
int 	number_of_radii;
int	a_axis, b_axis, c_axis;
char	keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
struct 	MP_STRUCTURE 	*pmp;

/*

Initialize variables

*/

a_axis = FALSE;
b_axis = FALSE;
c_axis = FALSE;
pmp = (struct MP_STRUCTURE *) mp;

number_of_radii = 0;
status = mpSUCCESS;

/*

Get all keywords currently set

*/

status = mpGetKeywords( mp, keywords, &num, type, class );
if (status != mpSUCCESS) return status;

for ( i=0; i<num; i++ )
	{
	if ( strcmp(keywords[i],mpA_AXIS_RADIUS) == 0 && a_axis == FALSE )
		{
		status = mpGetValues( mp, mpA_AXIS_RADIUS, &radii[0], NULL);
		if (status != mpSUCCESS) return status;
		a_axis = TRUE;
		number_of_radii++;
		}
	if ( strcmp(keywords[i],mpB_AXIS_RADIUS) == 0 && b_axis == FALSE )
		{
		status = mpGetValues( mp, mpB_AXIS_RADIUS, &radii[1], NULL );
		if (status != mpSUCCESS) return status;
		b_axis = TRUE;
		number_of_radii++;
		}
	if ( strcmp(keywords[i],mpC_AXIS_RADIUS) == 0 && c_axis == FALSE )
		{
		status = mpGetValues( mp, mpC_AXIS_RADIUS, &radii[2], NULL );
		if (status != mpSUCCESS) return status;
		c_axis = TRUE;
		number_of_radii++;
		}
	}

/*

Determine what radii have been set and what body model to 
use for map projections.

*/

switch( number_of_radii )	{

case 0:		/* NO radii have been set, return failure 	*/
		status = mpFAILURE;
		break;

case 1:		/* One radius value has been set	  	*/
		strcpy( pmp->model, "SPHERICAL" );
		if ( c_axis == TRUE )
			{
			mpSetValues( mp, mpA_AXIS_RADIUS, radii[2], NULL );
			mpSetValues( mp, mpB_AXIS_RADIUS, radii[2], NULL );
			}
		if ( b_axis == TRUE )
			{
			mpSetValues( mp, mpA_AXIS_RADIUS, radii[1], NULL );
			mpSetValues( mp, mpC_AXIS_RADIUS, radii[1], NULL );
			}
		if ( a_axis == TRUE )
			{
			mpSetValues( mp, mpB_AXIS_RADIUS, radii[0], NULL );
			mpSetValues( mp, mpC_AXIS_RADIUS, radii[0], NULL );
			}
		break;

case 2:		/* Two radii values have been set		*/
		if ( a_axis == TRUE && c_axis == TRUE )
			{
			mpSetValues( mp, mpB_AXIS_RADIUS, radii[0], NULL );
			if ( radii[0] == radii[2] )
				strcpy( pmp->model, "SPHERICAL" );
			else
				strcpy( pmp->model, "OBLATE" );
			}

		if ( b_axis == TRUE && c_axis == TRUE )
			{
			mpSetValues( mp, mpA_AXIS_RADIUS, radii[1], NULL );
			if ( radii[0] == radii[2] )
				strcpy( pmp->model, "SPHERICAL" );
			else
				strcpy( pmp->model, "OBLATE" );
			}

		if ( a_axis == TRUE && b_axis == TRUE )
			{
			if ( radii[0] == radii[1] )
				{
				mpSetValues( mp,mpC_AXIS_RADIUS,radii[0],NULL );
				strcpy( pmp->model, "SPHERICAL" );
				}
			else
				status = mpFAILURE;
			}
		break;

case 3:		/* Three radii values have been set			*/
		if ( radii[0] == radii[2] && radii[1] == radii[2] )
			strcpy( pmp->model, "SPHERICAL" );
		else
			if ( radii[0] == radii[1] && radii[0] != radii[2] )
				strcpy( pmp->model, "OBLATE" );
			else
				strcpy( pmp->model, "TRIAXIAL" );
		break;		};

return status;
}

/***********************************************************************

load_coefficients

Subroutine to load coefficients calculated by TRICOEF program

10Feb97 -Scholten- added code to calculate precalc.oblate_value's
*/
int load_coefficients( MP mp, double radii[] )
{
int 	status,i,j;
char	string[120];
double	dvala;
struct 	MP_STRUCTURE 	*pmp;
extern int mp_debug;

/*

Initialize values

*/

pmp = (struct MP_STRUCTURE *)mp;
i = NLIMIT * MLIMIT;
j = (MLIMIT + 1) * (KLIMIT + 1);

/* Solve for coefficient matrices */


status = mp_triaxcoef( radii,pmp->precalc.cc_matrix,
	pmp->precalc.cp_matrix,pmp->precalc.ac_matrix,
        pmp->precalc.ap_matrix);

if (status != mpSUCCESS) return status;

pmp->precalc.ellipsoid_value[0] = 1.0;
pmp->precalc.ellipsoid_value[1] = radii[1] / radii[0]; 
pmp->precalc.ellipsoid_value[2] = radii[2] / radii[0]; 

/*  eccentricity = [ 1 - (c_axis_radius/a_axis_radius)^2 ] ^0.5 */
CHECKif( pmp->glob.a_axis_radius == 0 );
dvala = pmp->glob.c_axis_radius / pmp->glob.a_axis_radius;
dvala = 1.0 - pow(dvala,2.0);
pmp->precalc.oblate_value[0] = pow(dvala,0.5);

/*  eccentricity^2  */
pmp->precalc.oblate_value[1] = dvala;

/*  1 - eccentricity^2  */
pmp->precalc.oblate_value[2] = 1.0 - pmp->precalc.oblate_value[1];

/*  1 / (2 * eccentricity)  */
pmp->precalc.oblate_value[3] = ( 1.0 / ( 2.0 * 
pmp->precalc.oblate_value[0] ) );

if(  mp_debug )
{
	sprintf(string,"\tprecalc.oblate_value[0]=%4.3e",
	pmp->precalc.oblate_value[0]);
	zvmessage(string," ");
	sprintf(string,"\tprecalc.oblate_value[1]=%4.3e",
	pmp->precalc.oblate_value[1]);
	zvmessage(string," ");
	sprintf(string,"\tprecalc.oblate_value[2]=%4.3e",
	pmp->precalc.oblate_value[2]);
	zvmessage(string," ");
	sprintf(string,"\tprecalc.oblate_value[3]=%4.3e",
	pmp->precalc.oblate_value[3]);
	zvmessage(string," ");
}

return mpSUCCESS;
}


/********************************************************************/
	/*
	 * Function mpGetDesc:
	 *
	 * Routine to return the MAP_PROJECTION_DESC item appropriate
	 * to the projection currently specified in the MP buffer.
	 * (This item contains text describing the projection.)
	 *
	 * 25aug93  -lwk-  intial version:  DUMMY PLACEHOLDER
         * Sept 94  -jfm-  Summary text from USGS Paper 1395 used as
         *                 description.
	 * Feb 95   -jfm-  Added information about lat/lon types to
	 *		   each description.
	 * Dec 95   -jfm-  Added formula number in mpSphere to
         *		   description.
	 * Oct 96   -lwk-  revised text about "planetodetic" in mpGetDesc
	 */

int mpGetDesc( MP mp, char desc[mpMAX_DESCRIPTION_LINES][100], int *desc_lines)
{
  struct MP_STRUCTURE *pmp;
  int i;

  pmp = (struct MP_STRUCTURE *) mp;

  if (!(pmp->kwd_set[0] & mpMAP_PROJECTION_TYPE_BIT)) 
	{
	strcpy( desc[0], "*** No map projection has been set. ***");
	*desc_lines = 1;
	return mpSUCCESS;
	}

  i = 0;

  if ( strcmp(pmp->glob.map_projection_type,mpALBERS) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpALBERS_ONE_PARALLEL) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpALBERS_TWO_PARALLELS) == 0 )
	{
	strcpy( desc[i],"An equal-area, conic projection where the ");
	strcat( desc[i++],"parallels are unequally spaced arcs of");
	strcpy( desc[i],"concentric circles, more closely spaced ");
	strcat( desc[i++],"at the north and south edges of the map.");
	strcpy( desc[i],"Meridians are equally spaced radii of the ");
	strcat( desc[i++],"same circles, cutting parallels at right");
	strcpy( desc[i],"angles.  There is no distortion in scale ");
	strcat( desc[i++],"or shape along the standard parallel(s).");
	strcpy( desc[i],"The poles are arcs of circles.  Used for ");
	strcat( desc[i++],"regions with predominant east-west expanse.");
	strcpy( desc[i],"Equations (14-1) through (14-11) from USGS ");
	strcat( desc[i],"Paper 1395 (pp 100,101) were used."); 
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpCYLINDRICAL_EQUAL_AREA) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpNORMAL_CYLINDRICAL) == 0 )
	{
	strcpy( desc[i],"An equal-area, cylindrical projection ");
	strcat( desc[i++],"where the meridians on normal aspect");
	strcpy( desc[i],"are equally spaced straight lines.  ");
	strcat( desc[i++],"Parallels on normal aspect are");
	strcpy( desc[i],"unequally spaced straight lines, closest ");
	strcat( desc[i++],"near the poles, cutting meridians at");
	strcpy( desc[i],"right angles.  On normal aspect, true ");
	strcat( desc[i++],"scale is along the equator, or along");
	strcpy( desc[i],"two parallels equidistant from the equator.");
	strcat( desc[i++],"  This is an orthographic projection");
	strcpy( desc[i],"of sphere onto a cylinder.  There is ");
	strcat( desc[i++],"substantial shape and scale distortion");
	strcpy( desc[i],"near points 90 degress from the central line. ");
	strcat( desc[i++],"Equations (10-1),(10-2),(10-6),(10-7) from USGS");
	strcpy( desc[i],"Paper 1395 (pp 79,80) were used."); 
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpEQUIDISTANT) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpCYLINDRICAL) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpRECTANGULAR) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpSIMPLE_CYLINDRICAL) == 0 || 
       strcmp(pmp->glob.map_projection_type,mpOBLIQUE_CYLINDRICAL) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpOBLIQUE_SIMPLE_CYLINDRICAL)== 0)
	{
	strcpy( desc[i],"A cylindrical projection that is neither ");
	strcat( desc[i++],"equal-area nor conformal.  The meridians");
	strcpy( desc[i],"and parallels are equidistant straight ");
	strcat( desc[i++],"lines, intersecting at right angles.");
	strcpy( desc[i],"Poles are shown as lines.  Note, this ");
	strcat( desc[i++],"projection is used only in spherical form.");
	strcpy( desc[i],"Equations (12-1) through(12-6) from USGS ");
	strcat( desc[i],"Paper 1395 (p 91) were used."); 
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpLAMBERT) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpLAMBERT_CONFORMAL) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpLAMBERT_ONE_PARALLEL) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpLAMBERT_TWO_PARALLELS) == 0 )
	{
	strcpy( desc[i],"A conformal, conic projection where ");
	strcat( desc[i++],"the parallels are unequally spaced");
	strcpy( desc[i],"arcs of concentric circles, more ");
	strcat( desc[i++],"closely spaced near the center of");
	strcpy( desc[i],"the map.  Meridians are equally ");
	strcat( desc[i++],"spaced radii of the same circles, ");
	strcpy( desc[i],"thereby cutting parallels at right ");
	strcat( desc[i++],"angles.  Scale is true along the");
	strcpy( desc[i],"standard parallel(s).  The pole in ");
	strcat( desc[i++],"the same hemisphere as standard ");
	strcpy( desc[i],"parallel(s) is a point, while the ");
	strcat( desc[i++],"other pole is at infinity.  Used for");
	strcpy( desc[i],"regions with a predominant east-west ");
	strcat( desc[i++],"expanse.");
	strcpy( desc[i],"Equations (14-1), (14-2), (14-4), (15-1) ");
	strcat( desc[i++],"through (15-5), (14-9) through (14-11)");
        strcpy( desc[i],"of USGS Paper 1395 (pp 106,107) were used."); 
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpLAMBERT_AZIMUTHAL) == 0 )
	{
	strcpy( desc[i],"An equal-area, azimuthal projection ");
	strcat( desc[i++],"where all meridians in the polar");
	strcpy( desc[i],"aspect, the central meridian in other ");
	strcat( desc[i++],"aspects, and the equator in the");
	strcpy( desc[i],"equatorial aspect are straight lines.  ");
	strcat( desc[i++],"The outer meridian of a hemisphere");
	strcpy( desc[i],"in the equatorial aspect (for sphere) ");
	strcat( desc[i++],"and the parallels in the polar");
	strcpy( desc[i],"aspect (sphere or ellipsoid) are ");
	strcat( desc[i++],"circles.  All other meridians and");
	strcpy( desc[i],"parallels are complex curves.  ");
	strcat( desc[i++],"Scale descreases radially as the");
	strcpy( desc[i],"distance increases from the center, ");
	strcat( desc[i++],"the only point without distortion.");
	strcpy( desc[i],"Scale increases in the direction ");
	strcat( desc[i++],"perpendicular to radii as the");
	strcpy( desc[i],"distance increases from the center.  ");
	strcat( desc[i++],"Directions from the center are");
	strcpy( desc[i],"true for the sphere and the polar ");
	strcat( desc[i++],"ellipsoidal forms.  Point opposite");
	strcpy( desc[i],"center is shown as a circle surrounding ");
	strcat( desc[i++],"the map (for sphere).");
	strcpy( desc[i],"Equations (22-4), (24-2), (24-13), (24-14), ");
	strcat( desc[i++],"(20-14), (20-15), (20-18), (24-16) of");
        strcpy( desc[i],"USGS Paper 1395 (pp 185-187) were used."); 
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpMERCATOR) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpTRANSVERSE_MERCATOR) == 0 )
	{
	strcpy( desc[i],"A conformal, cylindrical projection where ");
	strcat( desc[i++],"meridians are equally spaced straight");
	strcpy( desc[i],"lines.  Parallels are unequally spaced ");
	strcat( desc[i++],"straight lines, closest near the");
	strcpy( desc[i],"equator, cutting meridians at right ");
	strcat( desc[i++],"angles.  The scale is true along the");
	strcpy( desc[i],"equator, or along two parallels equidistant ");
	strcat( desc[i++],"from the equator.  Rhumb lines");
	strcpy( desc[i],"(loxodromes) are straight lines.  Poles ");
	strcat( desc[i++],"are at infinity with great distortion ");
	strcpy( desc[i++],"of area in polar regions.");
	strcpy( desc[i],"Equations (7-1), (7-2), (7-2a), (7-4), (7-4a), ");
	strcat( desc[i++],"(7-5) of USGS Paper 1395 (pp 43,44)");
	strcpy( desc[i],"were used.");
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpMOLLWEIDE) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpHOMALOGRAPHIC) == 0 )
	{
	strcpy( desc[i],"An equal-area, pseudocylindrical projection ");
	strcat( desc[i++],"where the central meridian is a");
	strcpy( desc[i],"straight line.  90th meridians are ");
	strcat( desc[i++],"circular arcs.  All other meridians are");
	strcpy( desc[i],"equally spaced elliptical arcs.  Parallels ");
	strcat( desc[i++],"are unequally spaced straight lines, ");
	strcpy( desc[i],"parallel to each other.  Poles are points.  ");
	strcat( desc[i++],"The scale is true along latitudes");
	strcpy( desc[i++],"40 degrees and 44 minutes North and South.");
	strcpy( desc[i],"Equations (31-1) through (31-8) of USGS Paper ");
	strcat( desc[i],"1395 (pp 251,252) were used.");
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpORTHOGRAPHIC) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpPOLAR_ORTHOGRAPHIC) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpOBLIQUE_ORTHOGRAPHIC) == 0 )
	{
	strcpy( desc[i],"An azimuthal projection that is neither ");
	strcat( desc[i++],"conformal nor equal-area.  All");
	strcpy( desc[i],"meridians and parallels are ellipses, ");
	strcat( desc[i++],"circles, or straight lines.  This");
	strcpy( desc[i],"projection resembles a globe in appearance ");
	strcat( desc[i++],"and has much distortion near the");
	strcpy( desc[i],"edges of the hemisphere shown.  There ");
	strcat( desc[i++],"is no distortion at the center only,");
	strcpy( desc[i],"and directions from the center are true.  ");
	strcat( desc[i++],"Radial scale factor decreases as");
	strcpy( desc[i],"distance increases from the center.  ");
	strcat( desc[i++],"Scale in the direction of the lines");
	strcpy( desc[i++],"of latitude is true in the polar aspect.");
	strcpy( desc[i],"In spherical form, the Equatorial aspect ");
	strcat( desc[i++],"equations (20-3),(20-13) through");
	strcpy( desc[i++],"(20-19) of USGS Paper 1395 (pp 149,150) were used.");
	strcpy( desc[i],"For the Oblate Spheroid, code from VICAR ");
	strcat( desc[i],"subroutine TRANV (q.v.) was used.");
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpPOINT_PERSPECTIVE) == 0 )
	{
	strcpy( desc[i],"Projection used to show target bodies ");
	strcat( desc[i++],"as seen by the observer (camera).");
	strcpy( desc[i],"Central meridian and a particular ");
	strcat( desc[i++],"parallel (if shown) are straight ");
	strcpy( desc[i],"lines.  Other meridians and parallels ");
	strcat( desc[i++],"are usually arcs of circles or ellipses,");
	strcpy( desc[i],"but some may be parabolas or hyperbolas.  ");
	strcat( desc[i++],"This projection is neither conformal");
	strcpy( desc[i++],"nor equal-area.");
	strcpy( desc[i],"The algorithm is described in the JPL memo: ");
	strcat( desc[i++],"'Planet-to-Camera geometry for flight images'");
        strcpy( desc[i],"Gary Yagi, 8 Oct.1985");
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpSINUSOIDAL) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpOBLIQUE_SINUSOIDAL) == 0 )
	{
	strcpy( desc[i],"An equal-area, pseudocylindrical projection ");
	strcat( desc[i++],"where the central meridian is a");
	strcpy( desc[i],"straight line.  All other meridians are ");
	strcat( desc[i++],"shown as equally spaced sinusoidal curves.");
	strcpy( desc[i],"Parallels are equally spaced straight lines, ");
	strcat( desc[i++],"parallel to each other.  Poles are");
	strcpy( desc[i],"points.  Scale is true along the central ");
	strcat( desc[i++],"meridians and all parallels.");
	strcpy( desc[i],"Equations (30-1),(30-2),(30-6),(30-7)");
	strcat( desc[i++]," of USGS Paper 1395 (pp 247,248)");
	strcpy( desc[i]," were used.");
	}

  else

  if ( strcmp(pmp->glob.map_projection_type,mpSTEREOGRAPHIC) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpPOLAR_STEREOGRAPHIC) == 0 ||
       strcmp(pmp->glob.map_projection_type,mpOBLIQUE_STEREOGRAPHIC) == 0 )
	{
	strcpy( desc[i],"A conformal, azimuthal projection where ");
	strcat( desc[i++],"the central meridian and a particular");
	strcpy( desc[i],"parallel (if shown) are straight lines.  ");
	strcat( desc[i++],"This is a perspective projection for");
	strcpy( desc[i],"the sphere.  All meridians on the polar ");
	strcat( desc[i++],"aspect and the equator on the");
	strcpy( desc[i],"equatorial aspect are straight lines.");
	strcat( desc[i++],"  All other meridians and parallels are");
	strcpy( desc[i],"shown as arcs of circles.  Directions ");
	strcat( desc[i++],"from the center of the projection are");
	strcpy( desc[i],"true (except on ellipsoidal oblique ");
	strcat( desc[i++],"and equatorial aspects).  Scale");
	strcpy( desc[i],"increases away from the center of the");
	strcat( desc[i++]," projection.  Equations (21-2), (21-3),");
	strcpy( desc[i],"(21-4), (20-14) through (20-18), (21-15) of ");
	strcat( desc[i],"USGS Paper 1395 (pp 157-159) were used.");
	}

  else
	return mpFAILURE;

  strcpy( desc[++i],"The value of the COORDINATE_SYSTEM_NAME ");
  strcat( desc[i],"item determines whether latitudes are");
  strcpy( desc[++i],"planetographic or planetocentric;  ");
  strcat( desc[i],"if this  keyword is absent, then");
  strcpy( desc[++i],"the default is planetographic.");
  strcpy( desc[++i],"The direction of increasing longitude is ");
  strcat( desc[i],"defined by the POSITIVE_LONGITUDE_DIRECTION");
  strcpy( desc[++i],"item;  if this keyword is absent, then ");
  strcat( desc[i],"the direction is determined by");
  strcpy( desc[++i],"COORDINATE_SYSTEM_NAME:  it is East if the ");
  strcat( desc[i],"system is planetographic, West if it is");
  strcpy( desc[++i],"planetocentric.");
  strcpy( desc[++i],"NOTE: Portions of above text taken from U.S. ");
  strcat( desc[i],"Geological Survey Professional Paper 1395,");
  strcpy( desc[++i],"second printing 1989, 'Map Projections ");
  strcat( desc[i],"- A Working Manual' by John Snyder.");
  strcpy( desc[++i],"See text for detailed formulae.");

  *desc_lines = ++i;

  return mpSUCCESS;
}


/****************************************************************************

mpSphere

SPHERICAL MODEL MAP PROJECTION TRANSFORMATION FUNCTIONS

The following are a set of C functions for various arithmetic operations 
required by spherical map projections. The USGS Paper 1395 is the 
main reference used for the map projection functions. The design of the
initialization (precalculations) is based on code by Peter Ford of MIT.

Map projections currently supported are:

	ALBERS ->			(one and two standard parallels)
	CYLINDRICAL EQUAL-AREA
	EQUIDISTANT CYLINDRICAL
	LAMBERT AZIMUTHAL EQUAL-AREA
	LAMBERT CONFORM CONIC ->	(one and two standard parallels)
	MERCATOR ->			(includes Transverse Mercator)
	MOLLWEIDE ->			(homalographic)
	ORTHOGRAPHIC
	SINUSOIDAL
	STEREOGRAPHIC
	PERSPECTIVE

The line (y value) of the projections calculated in this software differ
from the y values in USGS Paper 1395 by a multiplicative factor of (-1.0).
The USGS Paper 1395 defines y as the value above the horizontal axis x;
however, in image applications, the line dimension is always positive down.

Author: 		Justin McNeill

Date: 			October 1993

Revision history ...	December 1993	(JFM)
	
			A few lines in the sinusoidal projection
			arithmetic were revised to avoid an access
			violation. (FR 76817)

			October 1994	(JFM)

			Sinusoidal, Mercator, equidistant cylindrical,
			and cylindrical equal-area projections
			corrected for spherical, non-oblique and
			non-transverse projections.
			(FR 85645, 85808)

			March 1995	(JFM)
	
			Added mpINVALID_PROJECTION to return status
			value if projection_type is not specified in
			the switch statement. (FR 85803)

  24jun98 -lwk- fixed bug in Stereographic case when rho=0

*/
int	mpSphere( MP mp, double *line, double *sample, double *latitude, 
		double *longitude, MP_NUMBER projection_code )
{
  int	i;
int	status;
double	dvala, dvalb, dvalc, dvald, dvale, dvalf;
double  center_latitude,center_longitude;
double	del_lam, p2min, rho;
double	R_scale, mscale;		/* R_scale = (RADIUS / MAP_SCALE) */

float	temp_buf[40];
int 	ilat,imode;
float   line2,sample2,lat2,lon2,cas;

char 	string[500];
struct 	MP_STRUCTURE 	*pmp;

extern int mp_debug;

/*

Initialize values.

*/
pmp = ( struct MP_STRUCTURE *) mp;
rho = dvala = dvalb = dvalc = dvald = dvale = dvalf = 0;

if (projection_code == perspective) {
  /* map_scale is not a Perspective parameter */
  mscale = (pmp->pers.target_center_distance - pmp->glob.a_axis_radius) /
	  (pmp->pers.focal_length * pmp->pers.focal_plane_scale);
  R_scale = pmp->glob.a_axis_radius / mscale;

  /* Perspective is all done in PERS_PPROJ: */
  pmp->precalc_needed = FALSE;
}
else
  R_scale = pmp->glob.a_axis_radius / pmp->mpro.map_scale;

/* 

Check if precalculations and sphercial coordinate rotation
need to be performed.

*/

if( pmp->precalc_needed )
	{
	status = mp_perform_precalculations( mp,projection_code );
	if (status != mpSUCCESS) return status;

	pmp->precalc_needed = FALSE;
	}

switch ( projection_code )	{

case albers_one_p:

/* 

ALBERS projection with one standard parallel		

(AUTHALIC - USGS Paper 1395, page 98)

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}
	if (*longitude>PI) *longitude -= TWO_PI;
	if (*longitude<-PI)*longitude += TWO_PI;

	dvala = *longitude * pmp->precalc.value[1]; 
	dvalb = sin( *latitude );
	dvalb = 1 + pmp->precalc.value[1] * pmp->precalc.value[1] - 
		2 * pmp->precalc.value[1] * dvalb;
	CHECKif( dvalb < 0 );
	
	rho = pmp->precalc.value[2] * sqrt( dvalb );

	*sample	= rho * sin ( dvala );
	*line 	= pmp->precalc.value[3] - rho * cos ( dvala );

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:	

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = pmp->precalc.value[3] - *line; 
	CHECKif( dvala == 0 );

	rho = sqrt( *sample * *sample + dvala * dvala );

	dvalb = 1.0 / ( 2.0 * pmp->precalc.value[1] );

	dvalb *= ( 1.0 + pmp->precalc.value[1] * pmp->precalc.value[1] - 
	( rho * rho ) / ( pmp->precalc.value[2] * pmp->precalc.value[2] ) );
	CHECKif	(fabs(dvalb) > 1.0);

 	cas = 1.0;
	if (pmp->precalc.value[1] < 0.0) cas = -1.0;
	*longitude = atan2( cas*(*sample), cas*dvala ) / pmp->precalc.value[1];
	CHECKif	(fabs(*longitude)>PI);

	*latitude = asin( dvalb );

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break;	}

	status = mpSUCCESS;
	break;

case albers_two_p:
	
/* 

ALBERS projection with two standard parallels 	

(AUTHALIC - USGS Paper 1395, page 98)

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}
	if (*longitude>PI) *longitude -= TWO_PI;
	if (*longitude<-PI)*longitude += TWO_PI;

	dvala = pmp->precalc.value[1] * *longitude;	/* eta * lambda */
	dvalb = 2.0 * pmp->precalc.value[1] * sin ( *latitude );
	dvalb = pmp->precalc.value[5] - dvalb;
	CHECKif( dvalb < 0 );
	
	rho = ( R_scale / pmp->precalc.value[1] ) * sqrt( dvalb );

	*sample = rho * sin( dvala );	
	*line = pmp->precalc.value[4] - rho * cos( dvala );

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:	

	/* 

	Inverse mode	

	*/
	
	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = pmp->precalc.value[4] - *line;
	CHECKif( dvala == 0 );

	rho = sqrt( *sample * *sample + dvala * dvala );

	dvalb = ( rho * pmp->precalc.value[1] ) / R_scale;
	dvalb = ( pmp->precalc.value[5] - ( dvalb * dvalb ) ) / 
		( 2.0 * pmp->precalc.value[1] );
	CHECKif	(fabs(dvalb) > 1.0);
	
	*latitude = asin( dvalb );
 	cas = 1.0;
	if (pmp->precalc.value[1] < 0.0) cas = -1.0;
	*longitude = atan2( cas*(*sample), cas*dvala ) / pmp->precalc.value[1];
	CHECKif	(fabs(*longitude)>PI);

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break;	}

	status = mpSUCCESS;
	break;


case cylindrical_equal_area:

/*

CYLINDRICAL EQUAL-AREA projection

(AUTHALIC - USGS Paper 1395, page 76)

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/
	
	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	*longitude = check_longitude_radians(*longitude);	
	*sample = pmp->precalc.value[1] * *longitude;

	dvala = sin( *latitude );
	*line = pmp->precalc.value[2] * dvala;

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = *line / pmp->precalc.value[2];

	CHECKif( dvala < -1.0 || dvala > 1.0 );

	*latitude = asin( dvala );
	*longitude = *sample / pmp->precalc.value[1];
	
	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;	

case equidistant:
/* 

EQUIDISTANT CYLINDRICAL projection

(SPHERICAL ONLY - USGS Paper 1395, page 90)

SPECIAL NOTE: Modified mpOblate and mpEllipsoid to accept inputs
for equidistant projections, even though USGS Paper 1395 explicitly
states this projection is used only in spherical cases.  This was 
done to maintain consistent implementation between previous MIPS
function (TRANV) and current functions in MP_ROUTINES.

Also note that for SIMPLE_CYLINDRICAL, center_latitude and
center_longitude do not correspond to a rotation of the sphere
to an oblique projection, but to simply translate the values
of latitude and longitude prior to projection calculation.

JFM - December 1995

*/
        center_latitude = pmp->mpro.center_latitude;
        status = degrees2radians( &center_latitude );
        if (status != mpSUCCESS) return status;
 
        /*
 
        Change center longitude to "EAST" if necessary
 
        */
        if ( strcmp( pmp->glob.positive_longitude_direction,"WEST" ) == 0 ||
                strcmp( pmp->glob.positive_longitude_direction,"west" ) == 0 )
                center_longitude = 360.0 - pmp->mpro.center_longitude;
 	else
		center_longitude = pmp->mpro.center_longitude; 

	status = degrees2radians( &center_longitude );
	if (status != mpSUCCESS) return status;

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/
	
	*longitude -= center_longitude;
	*latitude -= center_latitude;

	*longitude = check_longitude_radians( *longitude );

	*sample = R_scale * *longitude * pmp->precalc.value[0];
	*line = R_scale * *latitude;

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	*longitude = *sample / ( R_scale * pmp->precalc.value[0] );
	*latitude = *line / R_scale;

	*longitude += center_longitude;
	*latitude += center_latitude;

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	break;	}

	status = mpSUCCESS;
	break;

case lambert_azimuthal:

/* 

LAMBERT AZIMUTHAL EQUAL-AREA projection

Computed at this level in its equatorial aspect
with center latitude and longitude of 0.0.
Function mp_perform_EULER_rotation takes care
of calculation of body rotation to CENTER_LATITUDE
and CENTER_LONGITUDE values supplied by user.

(AUTHALIC - USGS Paper 1395, page 182)

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	/* 

	Assumption is made that calling application checks that
	latitude to be transformed is not the negative of the
	center latitude while center longitude is +/- 180 degrees.
        This results in a 'dvala' that is undefined.  For these
	points, if the map is to cover the entire sphere, they
        are plotted as a circle of radius 2R.

        */ 
	
        dvala = 1 + cos( *latitude ) * cos( *longitude );
        CHECKif( dvala == 0.0 );

	dvala = sqrt( 2.0 / dvala ); 

	*sample = R_scale * dvala * cos( *latitude ) * sin( *longitude );

	*line = R_scale * dvala * sin( *latitude );

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	rho = sqrt( *sample * *sample + *line * *line );
        CHECKif( R_scale == 0.0 );
	
	dvala = rho / (2.0 * R_scale);
	CHECKif( dvala < -1.0 || dvala > 1.0 );

        if ( rho != 0.0 )
		{
		dvalb = 2.0 * asin( dvala );

	        *latitude = asin( (*line * sin( dvalb )) / rho );
		*longitude = atan2( (*sample * sin( dvalb )) , (rho * cos( dvalb )) );	
		}
	else
		{
		*latitude = 0.0;
		*longitude = 0.0;	
		}
 		
	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}
	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	break; 	}

	status = mpSUCCESS;
	break;

case lambert_one_p:

/* 

LAMBERT CONFORMAL CONIC projection with one standard parallel 	

(CONFORMAL - USGS Paper 1395, page 104)

Note: 	These formulae assume central meridian (lambda naught) of zero
	and phi naught of zero.

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}
	if (*longitude>PI) *longitude -= TWO_PI;
	if (*longitude<-PI)*longitude += TWO_PI;

	dvala = tan( PI_OVER_4 + ( *latitude / 2.0 ) );
	CHECKif( dvala <= 0 );

	dvala = pow( dvala, pmp->precalc.value[1] );
	dvalb = pmp->precalc.value[1] * *longitude;

	/* rho = ( RF / tan( PI_OVER_4 - lat/2 ) ) ^ eta */
	rho = pmp->precalc.value[4] / dvala;

	*sample = rho * sin( dvalb );
	*line = pmp->precalc.value[4] - rho * cos( dvalb );

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = pmp->precalc.value[4] - *line;
	CHECKif( dvala == 0 );
	
	rho = sqrt( dvala * dvala + *sample * *sample );
	if (rho == 0.0) {
	  *latitude = PI_OVER_2;
	  if (pmp->precalc.value[1] < 0.0) *latitude *= -1.0;
	}
	else {
	  /* rho takes the sign of n: */
	  if (pmp->precalc.value[1] < 0.0) rho *= -1.0;
	  dvalb = pmp->precalc.value[4] / rho;		
	  CHECKif( dvalb < 0 );
	  dvalb = pow( dvalb,pmp->precalc.value[2] ); /* (RF/rho) ^ (1/n) */
	  *latitude = 2.0 * atan( dvalb ) - PI_OVER_2;
	}
 	cas = 1.0;
	if (pmp->precalc.value[2] < 0.0) cas = -1.0;
	*longitude = pmp->precalc.value[2] * atan2( cas*(*sample), cas*dvala);
	CHECKif (fabs(*longitude)>PI);

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;

case lambert_two_p:

/* 

LAMBERT CONFORMAL CONIC projection with two standard parallels 	

(CONFORMAL - USGS Paper 1395, page 104)

Note: 	These formulae assume central meridian (lambda naught) of zero
	and phi naught of zero.

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	p2min = PI_OVER_2 - E_NEGATIVE_10;
	if ( (pmp->precalc.value[2] > 0.0 && *latitude >  p2min) ||
	     (pmp->precalc.value[2] < 0.0 && *latitude < -p2min) )
	  rho = 0.0;
	else {
	  dvala = tan( PI_OVER_4 + ( *latitude / 2.0 ) );
	  dvala = pow( dvala, pmp->precalc.value[2] );
	  rho = pmp->precalc.value[0] / dvala;
	}
	del_lam = *longitude;
	if ( del_lam < -PI ) del_lam += (2 * PI);
	if ( del_lam >  PI ) del_lam -= (2 * PI);
	dvalb = pmp->precalc.value[2] * del_lam;		/* theta   */

	*sample = rho * sin( dvalb );				/* sample  */
	*line = pmp->precalc.value[0] - rho * cos( dvalb );	/* line    */

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = pmp->precalc.value[0] - *line;
	CHECKif( dvala == 0 );
	
	rho = sqrt( *sample * *sample + dvala * dvala );
	if (rho == 0.0) {
	  *latitude = PI_OVER_2;
	  if (pmp->precalc.value[2] < 0.0) *latitude *= -1.0;
	}
	else {
	  /* rho takes the sign of n: */
	  if (pmp->precalc.value[2] < 0.0) rho *= -1.0;
	  dvalb = pmp->precalc.value[1] / rho;		
	  CHECKif( dvalb < 0 );
	  dvalb = pow( dvalb,pmp->precalc.value[3] );	
	  *latitude = 2.0 * atan( dvalb ) - PI_OVER_2;
	}
 	cas = 1.0;
	if (pmp->precalc.value[2] < 0.0) cas = -1.0;
	*longitude = pmp->precalc.value[3] * atan2( cas*(*sample), cas*dvala);
	CHECKif(fabs(*longitude)>PI);

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;	

case mercator:

/*

MERCATOR projection

(CONFORMAL - USGS Paper 1395 page 38)

Note:	These formulae assume a central meridian (lambda naught) of zero.

*/
	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	dvala = tan( PI_OVER_4 + 0.5 * *latitude);
	CHECKif( dvala <= 0 );

	*longitude = check_longitude_radians(*longitude);

	*sample = R_scale * *longitude;
	*line = R_scale * log( dvala );

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;
	
	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = sinh( *line / R_scale );

	*latitude = atan( dvala );
	*longitude = *sample / R_scale;

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break;	}

	status = mpSUCCESS;
	break;

case mollweide:

/*

MOLLWEIDE (HOMALOGRAPHIC) projection

(AUTHALIC - USGS Paper 1395 page 249)

Note: 	These formulae assume a central meridian (lambda naught) of zero.

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	dvala = 0;
	dvalb = dvalc = PI * sin( *latitude );
	dvald = 1.0 + cos( dvala );
	
	/* Loop until solution for longitude is found */

	while( fabs(dvalb) > E_NEGATIVE_12 && fabs(dvald) > E_NEGATIVE_12 )
		{
		dvalb = dvalc - dvala - sin( dvala );
		dvala += ( dvalb/dvald );
		}
	
	dvalb = dvala / 2.0;

	*longitude = check_longitude_radians(*longitude);

	*sample = ( 2.0 / PI ) * pmp->precalc.value[0] * 
		  *longitude * cos( dvalb );
	*line = pmp->precalc.value[0] * sin( dvalb );

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	dvala = *line / pmp->precalc.value[0];
	CHECKif( dvala > 1.0 || dvala < -1.0 );

	dvala = asin( dvala );

	dvalb = ( 2.0 * dvala + sin( 2.0 * dvala ) ) / PI;
	CHECKif( dvalb > 1.0 || dvalb < -1.0 );

	dvalc = cos( dvala ) * pmp->precalc.value[0];
	CHECKif( dvalc == 0 );

	*longitude = ( PI_OVER_2 * *sample ) / dvalc ;
	*latitude = asin( dvalb );

	CHECKif( *longitude < -PI || *longitude > PI+E_NEGATIVE_10 );

	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;	

case orthographic:

/*

ORTHOGRAPHIC projection

(SPHERICAL ONLY - USGS Paper 1395 page 145)

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	*sample = R_scale * cos( *latitude ) * sin( *longitude );
	*line = R_scale * sin( *latitude );
	
	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

        rho = sqrt(*sample * *sample + *line * *line);
	dvala = rho / R_scale;
	CHECKif( dvala < -1.0 || dvala > 1.0 );

        dvalb = asin(dvala);				/* C */
        if( dvalb == 0.0) {
	  *latitude=0.0;*longitude=0.0;		/* line/samp == proj_offset */
	}
        else {
	  *latitude = asin( *line / R_scale );
	  *longitude = atan( *sample / (R_scale*cos(dvalb)) );
	}
	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;	

case sinusoidal:

/*

SINUSOIDAL projection

(AUTHALIC - USGS Paper 1395 page 243)

Note:	These formulae assume a central meridian (lambda naught) of zero.

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	*longitude = check_longitude_radians( *longitude );

	*sample = R_scale * *longitude * cos( *latitude );
	*line = R_scale * *latitude;

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	*latitude = *line / R_scale ;
	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	dvala = R_scale * cos( *latitude );
	CHECKif( dvala == 0 );

	*longitude = *sample / dvala ;
	CHECKif( *longitude < -PI || *longitude > PI+E_NEGATIVE_10 );

	/* change Long from (-180,180) to (0,360) range [WHY?] */
	if ( *longitude < 0.0 ) *longitude += TWO_PI;

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;	

case stereographic:

/*

STEREOGRAPHIC projection

(CONFORMAL - USGS Paper 1395 page 154)

Note:	This is the equatorial aspect projection; thus the central meridian
	and phi naught are assumed to be zero.

*/

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	dvala = 1.0 + cos( *longitude ) * cos( *latitude );
	CHECKif( dvala == 0 );
	
	*sample = pmp->precalc.value[0] * cos( *latitude ) * sin( *longitude )
 			/ dvala;
	*line = pmp->precalc.value[0] * sin( *latitude ) / dvala;

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:

	/* 

	Inverse mode	

	*/

	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;

	rho = sqrt( *sample * *sample + *line * *line );

	if( rho == 0 ) {
	  *longitude=0.0; *latitude=0.0;
	}
        else {
	  dvala = 2.0 * atan( rho / pmp->precalc.value[0] );
	  dvalb = ( *line / rho ) * sin( dvala );
	  CHECKif( dvalb > 1.0 || dvalb < -1.0 );
	  *longitude = atan( ( *sample / rho ) * tan( dvala ) );
	  if (dvala>PI_OVER_2) *longitude += PI ;
	  *latitude = asin( dvalb );
	}
	while ( *longitude < 0.0 ) *longitude += (2 * PI);
	while ( *longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2 );

	if( pmp->precalc.rotation_flags & SPHERICAL_COORDINATE_ROTATION )
		{
		status = mp_perform_EULER_rotation( mp,latitude,longitude );
		if (status != mpSUCCESS) return status;
		}

	break; 	}

	status = mpSUCCESS;
	break;

case perspective:

/*

POINT_PERSPECTIVE projection

*/
	switch ( pmp->transformation_direction )	{
	
	case LAT_LON_TO_LINE_SAMP:

	/*

	Forward mode

	*/

	ilat = 0; 
	imode = 1;

	status = mpMpo2Buf(mp,temp_buf);
	if (status != mpSUCCESS) return status;

	temp_buf[36] = pmp->glob.b_axis_radius;

	lat2 = (float)(*latitude);
	lon2 = (float)(TWO_PI - (*longitude));

	status = pproj_mp_c(temp_buf,&line2,&sample2,&lat2,&lon2,
		imode);         
	if (status != mpSUCCESS) return status;
	if (status == 1) return status;		/* Back-Of-Planet */
	
/* (not needed!)	
	status = mpBuf2Mpo(temp_buf,mp);
	if (status != mpSUCCESS) return status;
*/
	*line = (double)(line2);
	*sample = (double)(sample2);

/*  no Cartesian transformation for Perspective, as there are no LINE/SAMPLE
 *  offset parameters   
	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;
	*line = -1.0 * *line;
 */
	break;

	case LINE_SAMP_TO_LAT_LON:

	/*

	Inverse mode

	*/

/*  no Cartesian transformation for Perspective, as there are no LINE/SAMPLE
 *  offset parameters   
	status = mp_perform_CARTESIAN_trans( mp,line,sample );
	if (status != mpSUCCESS) return status;
 */

/*	*line = -1.0 * *line;	*/	

	imode = 2;
	ilat = 0;

	status = mpMpo2Buf(mp,temp_buf);
	if (status != mpSUCCESS) return status;

	temp_buf[36] = pmp->glob.b_axis_radius;

	line2 = (float)(*line);
	sample2 = (float)(*sample);

/*  remove this, because we removed Cartesian transformation above:
	line2 = -1.0 * line2;
 */

	status = pproj_mp_c(temp_buf,&line2,&sample2,&lat2,&lon2,
		 imode);        
	if (status != mpSUCCESS) return status;

/* (not needed!)	
	status = mpBuf2Mpo(temp_buf,mp);
	if (status != mpSUCCESS) return status;
*/
	*latitude = (double)(lat2);
	*longitude = TWO_PI - (double)(lon2);

	while (*longitude < 0.0) *longitude += (2 * PI);
	while (*longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2);

	break; }

	status = mpSUCCESS;
	break;
	
default:
	
	status = mpINVALID_PROJECTION;
	break;
	}

if( mp_debug )
	{	
	/**** TEMPORARY CALCULATIONS REPORTING FOR DEBUGGING ****/
	if ( pmp->transformation_direction == LAT_LON_TO_LINE_SAMP )
		sprintf(string,"\n\tINTERNALS of %s in LL2XY mode",
			pmp->glob.map_projection_type);
	else
		sprintf(string,"\n\tINTERNALS of %s in XY2LL mode",
			pmp->glob.map_projection_type);

	zvmessage(string," ");

	zvmessage("\n\tPRECALCULATED VALUES 0 through 5:"," ");
	for( i=0; i<5; i++ )
		{
		sprintf(string,"\t\tvalue[%d]=%4.3e",i,pmp->precalc.value[i]);
		zvmessage(string," ");
		}

	zvmessage("\n\tD VALUES A through D:"," ");
	sprintf(string,"\t\tdvala=%4.3e",dvala);
	zvmessage(string," ");
	sprintf(string,"\t\tdvalb=%4.3e",dvalb);
	zvmessage(string," ");
	sprintf(string,"\t\tdvalc=%4.3e",dvalc);
	zvmessage(string," ");
	sprintf(string,"\t\tdvald=%4.3e",dvald);
	zvmessage(string," ");
	if ( pmp->transformation_direction == LAT_LON_TO_LINE_SAMP )
         {
	 zvmessage("\n\tin radians,"," ");
	 sprintf(string,"\n\t(LAT,LON)=(%7.6f,%7.6f)",*latitude,*longitude);
	 zvmessage(string," ");
	 sprintf(string,"\n\t(X,Y)=(%7.3f,%7.3f)",*sample,*line);
	 zvmessage(string," ");
         }
        else
         {
	 sprintf(string,"\n\t(X,Y)=(%7.3f,%7.3f)",*sample,*line);
	 zvmessage(string," ");
	 zvmessage("\n\tin radians,"," ");
	 sprintf(string,"\n\t(LAT,LON)=(%7.6f,%7.6f)",*latitude,*longitude);
	 zvmessage(string," ");
         }
	}

return status;
}

/****************************************************************************

mpOblate

OBLATE MODEL MAP PROJECTION TRANSFORMATION FUNCTIONS

The following subroutine uses functions to transform between planetodetic and
conformal or authalic auxiliary latitudes to perform map projections for target
bodies that are oblate spheroids (spheres flattened in along one pole).

Lower-level functions are used to transform planetodetic latitudes into the 
respective auxiliary latitudes. 
(See USGS Paper 1395, pages 13-16)

Supported map projections for the oblate spheroid are

	ALBERS ->			(one and two standard parallels)
	CYLINDRICAL EQUAL-AREA
	LAMBERT AZIMUTHAL EQUAL-AREA
	LAMBERT CONFORMAL CONIC	->	(one and two standard parallels)
	MERCATOR ->			(includes Transverse Mercator)
	MOLLWEIDE ->			(homalographic)
	SINUSOIDAL
	STEREOGRAPHIC
	EQUIDISTANT ->			(special case for MIPS backward
					 compatibility with TRANV)
	ORTHOGRAPHIC
	PERSPECTIVE

Author: 		Justin McNeill

Date: 			October 1993

Revision history 	March 1995	JFM
		
			Default case added to switch statement in order
			to trap unsupported projection types.  Routine
			now returns mpINVALID_PROJECTION when the default
			case is reached. (FR 85803)
			
			December 1995   JFM
			
			Equidistant projection is added to supported
			list in mpOblate in order to maintain compatibility
			with previous MIPS function TRANV.  It is recognized
			that this is stated for spherical use only in
			USGS paper 1395 and is non-authalic, non-conformal. 

*/
int	mpOblate( MP mp, double *line, double *sample, double *latitude, 
		double *longitude, MP_NUMBER projection_code )
{
int	status;
double 	authalic_latitude, conformal_latitude;
double	dvala;
float	temp_buf[40];
int 	imode;
float   line2,sample2,lat2,lon2;
char string[150];
struct 	MP_STRUCTURE 	*pmp;
extern int mp_debug;

pmp = ( struct MP_STRUCTURE *) mp;

/*

Check if auxiliary latitude precalculations need to be performed.

*/

if( pmp->precalc_needed )
	{
	/*	
	
	eccentricity = [ 1 - (c_axis_radius/a_axis_radius)^2 ] ^0.5

	*/
	CHECKif( pmp->glob.a_axis_radius == 0 );
	dvala = pmp->glob.c_axis_radius / pmp->glob.a_axis_radius;
	dvala = 1.0 - pow(dvala,2.0);
	pmp->precalc.oblate_value[0] = pow(dvala,0.5);

	/*	

	eccentricity^2						

	*/
	pmp->precalc.oblate_value[1] = dvala;

	/* 		

	1 - eccentricity^2 			
	
	*/
	pmp->precalc.oblate_value[2] = 1.0 - pmp->precalc.oblate_value[1];	

	/* 		

	1 / (2 * eccentricity)  		

	*/
	pmp->precalc.oblate_value[3] = ( 1.0 / ( 2.0 * 
					pmp->precalc.oblate_value[0] ) );

	if(  mp_debug )
		{
		sprintf(string,"\tprecalc.oblate_value[0]=%4.3e",
			pmp->precalc.oblate_value[0]);
		zvmessage(string," ");
		sprintf(string,"\tprecalc.oblate_value[1]=%4.3e",
			pmp->precalc.oblate_value[1]);
		zvmessage(string," ");
		sprintf(string,"\tprecalc.oblate_value[2]=%4.3e",
			pmp->precalc.oblate_value[2]);
		zvmessage(string," ");
		sprintf(string,"\tprecalc.oblate_value[3]=%4.3e",
			pmp->precalc.oblate_value[3]);
		zvmessage(string," ");
		}
	}

switch ( projection_code )	{

/*

Supported conformal map projections include:	LAMBERT CONFORMAL CONIC
						MERCATOR
						STEREOGRAPHIC

*/

case lambert_one_p:
case lambert_two_p:
case mercator:
case stereographic:

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	status = planetodetic_conformal_trans( mp,latitude,
		&conformal_latitude );
	if (status != mpSUCCESS) return status;

	status = mpSphere( mp,line,sample,&conformal_latitude,longitude,
		projection_code );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:	

	/* 

	Inverse mode	

	*/

	status = mpSphere( mp,line,sample,&conformal_latitude,longitude,
		projection_code );
	if (status != mpSUCCESS) return status;

	status = planetodetic_conformal_trans( mp,latitude,
		&conformal_latitude );
	if (status != mpSUCCESS) return status;

	break;	}

	status = mpSUCCESS;
	break;

	
/*

Supported authalic map projections include:	ALBERS
						CYLINDRICAL EQUAL-AREA
						LAMBERT AZIMUTHAL EQUAL-AREA
						MOLLWEIDE
						SINUSOIDAL

*/

case albers_one_p:
case albers_two_p:
case cylindrical_equal_area:
case lambert_azimuthal:
case mollweide:
case sinusoidal:

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	status =planetodetic_authalic_trans(mp,latitude, &authalic_latitude );
	if (status != mpSUCCESS) return status;

	status = mpSphere( mp,line,sample,&authalic_latitude,longitude,
		projection_code );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:	

	/* 

	Inverse mode	

	*/

	status = mpSphere( mp,line,sample,&authalic_latitude,longitude,
		projection_code );
	if (status != mpSUCCESS) return status;

	status =planetodetic_authalic_trans(mp,latitude, &authalic_latitude );
	if (status != mpSUCCESS) return status;

	break;	}

	status = mpSUCCESS;
	break;

/*

Supported non-authalic, non-conformal projections:	EQUIDISTANT CYLINDRICAL
							ORTHOGRAPHIC
							PERSPECTIVE
*/

case equidistant: 

	/*

	Both forward and inverse modes

	*/

	status = mpSphere( mp,line,sample,latitude,longitude,projection_code );
	if (status != mpSUCCESS) return status;

	status = mpSUCCESS;
	break;


case orthographic:

	switch ( pmp->transformation_direction )	{
	
	case LAT_LON_TO_LINE_SAMP:

	/*

	Forward mode

	*/

	status = mpMpo2Buf(mp,temp_buf);
	if (status != mpSUCCESS) return status;

	lat2 = (float)(*latitude);
	lon2 = (float)(TWO_PI - (*longitude));

	imode = 1;
	status = ortho_obl_c(imode,temp_buf,&line2,&sample2,&lat2,&lon2);
	if (status != mpSUCCESS) return status;	/* 1 = Back-Of-Planet */
	
	*line = (double)(line2);
	*sample = (double)(sample2);

	break;

	case LINE_SAMP_TO_LAT_LON:

	/*

	Inverse mode

	*/

	status = mpMpo2Buf(mp,temp_buf);
	if (status != mpSUCCESS) return status;

	line2 = (float)(*line);
	sample2 = (float)(*sample);
	imode = 2;
	status = ortho_obl_c(imode,temp_buf,&line2,&sample2,&lat2,&lon2);
	if (status != mpSUCCESS) return status;
	
	*latitude = (double)(lat2);
	*longitude = TWO_PI - (double)(lon2);

	while (*longitude < 0.0) *longitude += (2 * PI);
	while (*longitude > (2 * PI) ) *longitude -= (2 * PI);

	CHECKif( *latitude < -PI_OVER_2 || *latitude > PI_OVER_2);

	break; }

	status = mpSUCCESS;
	break;


case perspective:
	 
	/*	

	Both forward and inverse modes

	*/

	status = mpSphere( mp,line,sample,latitude,longitude,projection_code);
	if (status != mpSUCCESS) return status; /* 1 is BOP or off planet */

	status = mpSUCCESS;
	break;


default:
	
	status = mpINVALID_PROJECTION;
	break;

	}

return status;
}

/****************************************************************************

mpEllipsoid

TRIAXIAL ELLIPSOID MODEL MAP PROJECTION TRANSFORMATION FUNCTIONS

The following subroutine uses functions to transform between planetodetic and
conformal or authalic auxiliary latitudes to perform map projections for target
bodies that are triaxial ellipsoids (spheroid with 3 unique axes measures).

Lower-level functions are used to transform planetodetic latitudes into the 
respective auxiliary latitudes (TRIAXTRAN.COM)

Supported map projections for the triaxial ellipsoid are

	LAMBERT CONFORMAL CONIC	->	(one and two standard parallels)
	MERCATOR ->			(includes Transverse Mercator)
	STEREOGRAPHIC
	ALBERS
	CYLINDRICAL EQUAL-AREA
	LAMBERT AZIMUTHAL EQUAL-AREA
	MOLLWEIDE
	SINUSOIDAL
	EQUIDISTANT ->			(special case to preserve 
					 backwards compatibility with
					 MIPS TRANV function)
	PERSPECTIVE

Author: 		Justin McNeill

Date: 			October 1993

Revision history 	March 1995	(JFM)
	
			Added mpINVALID_PROJECTION to return status
			value if projection_type is not specified in
			the switch statement. (FR 85803)

			December 1995   (JFM)
			
			Equidistant projection is added to supported
			list in mpOblate in order to maintain compatibility
			with previous MIPS function TRANV.  It is recognized
			that this is stated for spherical use only in
			USGS paper 1395 and is non-authalic, non-conformal. 

*/
int	mpEllipsoid( MP mp, double *line, double *sample, double *latitude, 
		double *longitude, MP_NUMBER projection_code )
{
int	k, m, n;
int	format_one,format_two,format_three;
int	status;
double 	authalic_latitude,authalic_longitude;
double	conformal_latitude,conformal_longitude;
char string[100];
struct 	MP_STRUCTURE 	*pmp;
extern int mp_debug;

/*

Initialize values

*/
pmp = ( struct MP_STRUCTURE *) mp;
k = KLIMIT;
n = NLIMIT;
m = MLIMIT;
format_one = PLANETODETIC;
format_two = CONFORMAL;
format_three = AUTHALIC;

switch ( projection_code )	{

/*

Supported conformal map projections include:	LAMBERT CONFORMAL CONIC
						MERCATOR
						STEREOGRAPHIC

*/

case lambert_one_p:
case lambert_two_p:
case mercator:
case stereographic:

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	*latitude = RETURN_DEGREES( *latitude );
	*longitude = RETURN_DEGREES( *longitude );

status = triaxtran_c( pmp->precalc.ellipsoid_value[0],
			pmp->precalc.ellipsoid_value[1],
			pmp->precalc.ellipsoid_value[2],
			pmp->precalc.cc_matrix,pmp->precalc.cp_matrix,
			pmp->precalc.ac_matrix,pmp->precalc.ap_matrix,
			*latitude,*longitude,format_one,
			&conformal_latitude,&conformal_longitude,format_two);
	CHECKif( status != mpSUCCESS );

	conformal_latitude = RETURN_RADIANS( conformal_latitude );
	conformal_longitude = RETURN_RADIANS( conformal_longitude );

	if( mp_debug )
		{
		zvmessage("\n\tCONFORMAL lat/lon (radians)"," ");
		sprintf(string,"\n\t\t(%7.6f,%7.6f)",conformal_latitude,
			conformal_longitude);
		zvmessage(string," ");
		}
	
	status = mpSphere( mp,line,sample,&conformal_latitude,
		&conformal_longitude,projection_code );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:	

	/* 

	Inverse mode	

	*/

	status = mpSphere( mp,line,sample,&conformal_latitude,
		&conformal_longitude,projection_code );
	if (status != mpSUCCESS) return status;

	conformal_latitude = RETURN_DEGREES( conformal_latitude );
	conformal_longitude = RETURN_DEGREES( conformal_longitude );

status = triaxtran_c( pmp->precalc.ellipsoid_value[0],
			pmp->precalc.ellipsoid_value[1],
			pmp->precalc.ellipsoid_value[2],
			pmp->precalc.cc_matrix,pmp->precalc.cp_matrix,
			pmp->precalc.ac_matrix,pmp->precalc.ap_matrix,
			conformal_latitude,conformal_longitude,format_two,
			latitude,longitude,format_one);
	CHECKif( status != mpSUCCESS );

	*latitude = RETURN_RADIANS( *latitude );
	*longitude = RETURN_RADIANS( *longitude );

	break;	}

	status = mpSUCCESS;
	break;

	
/*

Supported authalic map projections include:	ALBERS
						CYLINDRICAL EQUAL-AREA
						LAMBERT AZIMUTHAL EQUAL-AREA
						MOLLWEIDE
						SINUSOIDAL

*/
case albers_one_p:
case albers_two_p:
case cylindrical_equal_area:
case lambert_azimuthal:
case mollweide:
case sinusoidal:

	switch ( pmp->transformation_direction )	{

	case LAT_LON_TO_LINE_SAMP:

	/* 

	Forward mode	

	*/

	*latitude = RETURN_DEGREES( *latitude );
	*longitude = RETURN_DEGREES( *longitude );

status = triaxtran_c( pmp->precalc.ellipsoid_value[0],
			pmp->precalc.ellipsoid_value[1],
			pmp->precalc.ellipsoid_value[2],
			pmp->precalc.cc_matrix,pmp->precalc.cp_matrix,
			pmp->precalc.ac_matrix,pmp->precalc.ap_matrix,
			*latitude,*longitude,format_one,
			&authalic_latitude,&authalic_longitude,format_three);
	CHECKif( status != mpSUCCESS );

	authalic_latitude = RETURN_RADIANS( authalic_latitude );
	authalic_longitude = RETURN_RADIANS( authalic_longitude );

	if( mp_debug )
		{
		zvmessage("\n\tAUTHALIC lat/lon (radians)"," ");
		sprintf(string,"\n\t\t(%7.6f,%7.6f)",authalic_latitude,
			authalic_longitude);
		zvmessage(string," ");
		}

	status = mpSphere( mp,line,sample,&authalic_latitude,
		&authalic_longitude,projection_code );
	if (status != mpSUCCESS) return status;

	break;

	case LINE_SAMP_TO_LAT_LON:	

	/* 

	Inverse mode	

	*/

	status = mpSphere( mp,line,sample,&authalic_latitude,
		&authalic_longitude,projection_code );
	if (status != mpSUCCESS) return status;

	authalic_latitude = RETURN_DEGREES( authalic_latitude );
	authalic_longitude = RETURN_DEGREES( authalic_longitude );

status = triaxtran_c( pmp->precalc.ellipsoid_value[0],
			pmp->precalc.ellipsoid_value[1],
			pmp->precalc.ellipsoid_value[2],
			pmp->precalc.cc_matrix,pmp->precalc.cp_matrix,
			pmp->precalc.ac_matrix,pmp->precalc.ap_matrix,
			authalic_latitude,authalic_longitude,format_three,
			latitude,longitude,format_one);
	CHECKif( status != mpSUCCESS );

	*latitude = RETURN_RADIANS( *latitude );
	*longitude = RETURN_RADIANS( *longitude );

	break;	}

	status = mpSUCCESS;
	break;

/*

Supported non-authalic, non-conformal projections: 	EQUIDISTANT CYLINDRICAL

*/

case equidistant: 

	/*

	Both forward and inverse modes

	*/

	status = mpSphere( mp,line,sample,latitude,longitude,projection_code );
	if (status != mpSUCCESS) return status;

	status = mpSUCCESS;
	break;

/*

Supported non-authalic, non-conformal projections: 	PERSPECTIVE	

*/


case perspective: 

	/*

	Both forward and inverse modes

	*/

	status = mpSphere( mp,line,sample,latitude,longitude,projection_code );
	if (status != mpSUCCESS) return status;

	status = mpSUCCESS;
	break;



default:

	status = mpINVALID_PROJECTION;
	break;
	}

return status;
}

/***********************************************************************

planetodetic_authalic_trans

Routine to perform translation between planetodetic and authalic auxiliary 
latitude for oblate sphere.

From USGS Paper 1395, page 16.

	planetodetic to authalic latitude formula:

	beta = arcsin( q / q_sub_p )

	where 	

	q = (1-e^2) * { sin(phi)/(1-e^2*sin^2(phi)) -
		(1/2e)*ln[(1-e*sin(phi))/(1+e*sin(phi))] }


	authalic to planetodetic latitude formula:

	phi = phi + (1-(e^2*sin^2(phi)))^2/2*cos(phi) *
		[ q/(1-e^2) - sin(phi)/(1-(e^2*sin^2(phi)))
		  + (1/2e)*ln( (1-e*sin(phi))/(1+e*sin(phi)) ) ]

	where

	q = q_sub_p*sin(beta)

	and 

	phi is the planetodetic latitude,
	beta is the authalic latitude,
	q_sub_p =  q @ phi of 90 degrees,
	e is eccentricity of the body model
	
*/
int planetodetic_authalic_trans( MP mp, double *planetodetic_latitude,
	double *authalic_latitude )
{
int 	status;
double 	dvala, dvalb, dvalc, dvald;
double 	phi, delta_phi;
double 	sine_lat, sine_squared_lat;
double 	q, q_sub_p;
char 	string[100];
struct 	MP_STRUCTURE *pmp;
extern int mp_debug;

pmp = ( struct MP_STRUCTURE *) mp;

switch ( pmp->transformation_direction )	{

case LAT_LON_TO_LINE_SAMP:

	sine_lat = sin( *planetodetic_latitude );
	sine_squared_lat = sine_lat * sine_lat;

	dvala = ( 1.0 - pmp->precalc.oblate_value[0] * sine_lat ) /
		( 1.0 + pmp->precalc.oblate_value[0] * sine_lat );

	q = pmp->precalc.oblate_value[2];
	q *= ( ( sine_lat / 
		( 1.0 - pmp->precalc.oblate_value[1] * sine_squared_lat ) )
		- ( pmp->precalc.oblate_value[3] * log( dvala ) ) );

	q_sub_p = log( ( 1.0 - pmp->precalc.oblate_value[0] ) / 
			( 1.0 + pmp->precalc.oblate_value[0] ) );
	q_sub_p *= pmp->precalc.oblate_value[3];
	q_sub_p = ( 1.0 / pmp->precalc.oblate_value[2] ) - q_sub_p;
	q_sub_p *= pmp->precalc.oblate_value[2];

	CHECKif( q_sub_p == 0 );

	dvalb = q / q_sub_p;
	CHECKif( dvalb < -1.0 || dvalb > 1.0 );

	*authalic_latitude = asin( dvalb );	
	status = mpSUCCESS;
	break;

case LINE_SAMP_TO_LAT_LON:

	q_sub_p = log( ( 1.0 - pmp->precalc.oblate_value[0] ) / 
			( 1.0 + pmp->precalc.oblate_value[0] ) );
	q_sub_p *= pmp->precalc.oblate_value[3];
	q_sub_p = ( 1.0 / pmp->precalc.oblate_value[2] ) - q_sub_p;
	q_sub_p *= pmp->precalc.oblate_value[2];

	q = q_sub_p * sin( *authalic_latitude );

	phi = asin( q / 2.0 );
	sine_lat = sin( phi );
	delta_phi = 1;
	while( fabs(delta_phi) > E_NEGATIVE_12 ) {
	  sine_squared_lat = sine_lat * sine_lat;
	
	  dvala = 1.0 - pmp->precalc.oblate_value[1] * sine_squared_lat;
	  dvala *= dvala;
	  dvala /= ( 2.0 * cos( phi ) );
	  dvalb = ( q / pmp->precalc.oblate_value[2] );

	  dvalc = 1.0 - pmp->precalc.oblate_value[1] * sine_squared_lat;
	  CHECKif( dvalc == 0 );
	  dvalc = sine_lat / dvalc;
	
	  dvald = ( 1.0 - pmp->precalc.oblate_value[0] * sine_lat ) /
	  	( 1.0 + pmp->precalc.oblate_value[0] * sine_lat );
	  dvald = pmp->precalc.oblate_value[3] * log( dvald );
	
	  delta_phi = dvala * ( dvalb - dvalc + dvald );
	  phi += delta_phi;

	  /* the convergence criterion is sin(lat), not lat: */
	  delta_phi = sine_lat;
	  sine_lat = sin( phi );
	  delta_phi -= sine_lat;
	}
	
	*planetodetic_latitude = phi;
	status = mpSUCCESS;
	break;

default:

	status = mpFAILURE;
	break;		}

if( mp_debug )
	{
	/**** TEMPORARY CALCULATIONS REPORTING FOR DEBUGGING ****/

	sprintf(string,"\n\tplanetodetic_latitude = %4.3e",
		*planetodetic_latitude);
	zvmessage(string," ");
	sprintf(string,"\tauthalic_latitude = %4.3e\n",*authalic_latitude);
	zvmessage(string," ");
	}

return status;
} 

/***********************************************************************

planetodetic_conformal_trans

Routine to perform translation between planetodetic and conformal auxiliary 
latitude for oblate sphere.

From USGS Paper 1395, page 15.

	planetodetic to conformal latitude formula:

	chi = 2 * arctan[ (1+sin(lat))/(1-sin(lat)) 
		* ((1-e*sin(lat))/(1+e*sin(lat)))^e ]^0.5 
		- (PI/2)

	conformal to planetodetic latitude formula:

	phi = 2 * arctan{ tan[(PI_OVER_4)+(chi/2)] * 
		[(1+e*sin(phi))/(1-e*sin(phi)]^(e/2) } - (PI/2)

	where 
	
	chi is the conformal latitude, 
	phi is the planetodetic latitude,
	e is the eccentricity of the body model,
	PI is 3.1415...
	
*/
int planetodetic_conformal_trans( MP mp, double *planetodetic_latitude,
	double *conformal_latitude )
{
int 	status;
double 	dvala, dvalb, dvalc;
double 	phi, delta_phi, new_phi;
double 	sine_lat;
char 	string[100];
struct 	MP_STRUCTURE *pmp;
extern int mp_debug;

pmp = ( struct MP_STRUCTURE *) mp;

switch ( pmp->transformation_direction )	{

case LAT_LON_TO_LINE_SAMP:

	sine_lat = sin( *planetodetic_latitude );

	dvala = pmp->precalc.oblate_value[0] * sine_lat;
	dvalb = 1.0 - sine_lat;
	if( dvalb == 0 ) {
	  *conformal_latitude = (double) PI_OVER_2;
	  status = mpSUCCESS;
	  break;
	}
	dvalb = ( 1.0 + sine_lat ) / dvalb;
	dvalc = ( 1.0 - dvala ) / ( 1.0 + dvala );
	dvalc = pow( dvalc,pmp->precalc.oblate_value[0] );
	dvalc *= dvalb;
	dvalc = sqrt( dvalc );

	*conformal_latitude = 2.0 * atan( dvalc ) - (double) PI_OVER_2;
	status = mpSUCCESS;
	break;

case LINE_SAMP_TO_LAT_LON:

	phi = *conformal_latitude;
	delta_phi = 1;
	while( delta_phi > E_NEGATIVE_12 )
		{
		dvala = 1.0 - pmp->precalc.oblate_value[0] * sin( phi );
		CHECKif( dvala == 0 );
		dvala = ( 1.0 + pmp->precalc.oblate_value[0] * sin( phi ) ) 
			/ dvala;
		dvala = pow( dvala,(pmp->precalc.oblate_value[0]/2.0) );
		dvalb = tan( PI_OVER_4 + (*conformal_latitude/2.0) );
		dvalc = dvalb*dvala;

		new_phi = 2.0 * atan( dvalc ) - PI_OVER_2;
		delta_phi = fabs( phi - new_phi );
		phi = new_phi;
		}

	*planetodetic_latitude = phi;
	status = mpSUCCESS;
	break;	

default:
	status = mpFAILURE;
	break;		}

if(  mp_debug )
	{
	/**** TEMPORARY CALCULATIONS REPORTING FOR DEBUGGING ****/

	sprintf(string,"\n\tplanetodetic_latitude = %4.3e",
		*planetodetic_latitude);
	zvmessage(string," ");
	sprintf(string,"\tconformal_latitude = %4.3e\n",*conformal_latitude);
	zvmessage(string," ");
	}

return status;
} 

/****************************************************************************

mp_perform_precalculations

PERFORM PRECALCULATIONS OF PORTIONS OF MAP PROJECTION FORMULAS

The following are a set of C functions to perform precalculations for all
spherical map projections. This function also loads an Euler rotation matrix
dependent on the spherical azimuth (SPHERICAL_AZIMUTH) value and determines
trignometric functions of the Cartesian azimuth (CARTESIAN_AZIMUTH).

Author: 		Justin McNeill

Date: 			October 1993

Revision history ...	Original

			FOR LASTEST REVISIONS, SEE USER HELP FILE.
*/
int mp_perform_precalculations( MP mp, MP_NUMBER projection_number )
{
int	status;

double	phi0, phi1;		/* First and second standard parallels 	*/
double  dvala, dvalb, dvalc;	/* Dummy double variables		*/
double  R_scale, mscale;
struct	MP_STRUCTURE *pmp;	/* Map projection object pointer	*/

pmp = ( struct MP_STRUCTURE *) mp;

if (projection_number == perspective) {
  /* map_scale is not a Perspective parameter */
  mscale = (pmp->pers.target_center_distance - pmp->glob.a_axis_radius) /
	  (pmp->pers.focal_length * pmp->pers.focal_plane_scale);
  R_scale = pmp->glob.a_axis_radius / mscale;
}
else
  R_scale = pmp->glob.a_axis_radius / pmp->mpro.map_scale;

/*

Call routine to load Euler rotation matrix to data object
and cosine and sine of Cartesian azimuth.

*/

status = mp_load_EULER_rotation_matrix( mp, projection_number);
if (status != mpSUCCESS) return status;

mp_load_cosine_sine_CARTESIAN(mp);

switch ( projection_number ) 	{

case albers_one_p:

/*

ALBERS projection with one standard parallel

*/

	phi0 =  pmp->mpro.first_standard_parallel;
	status = degrees2radians(&phi0);

	pmp->precalc.value[0] = cos( phi0 );
	pmp->precalc.value[1] = sin( phi0 );			/* eta */

	CHECKif( pmp->precalc.value[1] == 0 );

	pmp->precalc.value[2] = R_scale / sin( phi0 );

	pmp->precalc.value[3] = pmp->precalc.value[2] *		/* rho naught */
		sqrt( 1 + pmp->precalc.value[1] * pmp->precalc.value[1] );
		
	break;

case albers_two_p:

/*

ALBERS projection with two standard parallels

*/
	/*

	Check that standard parallels are not equal to each other;
	if they are equal, return mpFAILURE.

	*/
	CHECKif( pmp->mpro.first_standard_parallel == 
		 pmp->mpro.second_standard_parallel );
		
	/*

	Convert data object first and second standard parallels to
	units of radians from degrees.	

	*/

	phi0 =  pmp->mpro.first_standard_parallel;
	status = degrees2radians(&phi0);

	phi1 =  pmp->mpro.second_standard_parallel;
	status = degrees2radians(&phi1);

	pmp->precalc.value[0] = sin( phi0 );	
	pmp->precalc.value[1] = 0.5 * ( sin( phi0 ) + sin( phi1 ) ); /* eta */
	CHECKif( pmp->precalc.value[1] == 0 );

	pmp->precalc.value[2] 
		= ( 2.0 * R_scale * R_scale ) / pmp->precalc.value[1]; 
	CHECKif( pmp->precalc.value[2] == 0 );

	/* R cos( phi0 ) / sin( phi0 ) */
	pmp->precalc.value[3] 
		= ( R_scale * cos( phi0 ) ) / pmp->precalc.value[1]; 	

	dvala = cos( phi0 );
	
	dvalb = dvala * dvala + 
		2 * pmp->precalc.value[0] * pmp->precalc.value[1];
	CHECKif( dvalb == 0 );					/* C	*/

	pmp->precalc.value[4] = ( R_scale / pmp->precalc.value[1] )
				* sqrt( dvalb );		/* rho naught */
	pmp->precalc.value[5] = dvalb;

	break;

case cylindrical_equal_area:

/*

CYLINDRICAL EQUAL-AREA projection

*/
	
	phi0 =  pmp->mpro.first_standard_parallel;
	status = degrees2radians(&phi0);
	
	pmp->precalc.value[0] = cos( phi0 );
	CHECKif( pmp->precalc.value[0] == 0 );

	pmp->precalc.value[1] =  R_scale * pmp->precalc.value[0];

	pmp->precalc.value[2] =  R_scale / pmp->precalc.value[0];

	break;

case equidistant:

/*

EQUIDISTANT CYLINDRICAL projection

*/
	phi0 =  pmp->mpro.first_standard_parallel;
	status = degrees2radians(&phi0);
	
	pmp->precalc.value[0] = cos( phi0 );
	CHECKif( pmp->precalc.value[0] == 0 );
	
	break;

case lambert_azimuthal:

/*

LAMBERT AZIMUTHAL EQUAL-AREA projection

*/
	
	break;

case lambert_one_p:

/*

LAMBERT CONFORMAL CONIC projection with one standard parallel

*/

	phi0 =  pmp->mpro.first_standard_parallel;
	status = degrees2radians(&phi0);
	
	pmp->precalc.value[0] = tan( PI_OVER_4 + phi0 / 2.0 );
	pmp->precalc.value[1] = sin( phi0 );			/* n */
	pmp->precalc.value[2] = 1.0 / pmp->precalc.value[1];
	pmp->precalc.value[3] = R_scale * cos( phi0 );
	pmp->precalc.value[4] = pmp->precalc.value[3] * 
		pow( pmp->precalc.value[0],pmp->precalc.value[1] ) *
		pmp->precalc.value[2];
	/*	.value[4] is Rf ; rho naught */	
	break;

case lambert_two_p:

/*

LAMBERT CONFORMAL CONIC projection with two standard parallels

*/

	/*

	Check that standard parallels are not equal to each other;
	if they are equal, return mpFAILURE.

	*/
	CHECKif( pmp->mpro.first_standard_parallel == 
		 pmp->mpro.second_standard_parallel );
		
	/*

	Convert data object first and second standard parallels to
	units of radians from degrees.	

	*/
	phi0 =  pmp->mpro.first_standard_parallel;
	status = degrees2radians(&phi0);

	phi1 =  pmp->mpro.second_standard_parallel;
	status = degrees2radians(&phi1);

	pmp->precalc.value[0] = cos( phi0 );	
	pmp->precalc.value[1] = cos( phi1 );	
	CHECKif( pmp->precalc.value[0] <= 0 || pmp->precalc.value[1] <= 0 );

	dvala = tan( PI_OVER_4 + 0.5 * phi0 );
	dvalb = tan( PI_OVER_4 + 0.5 * phi1 );
	CHECKif( dvala <= 0 || dvalb <= 0 );

	dvalc = log( dvalb ) - log( dvala );
	CHECKif( dvalc == 0 );

	pmp->precalc.value[2] = 				/* n */
		(log(pmp->precalc.value[0]) - log(pmp->precalc.value[1]))/dvalc;
	CHECKif( pmp->precalc.value[2] == 0 );

	pmp->precalc.value[3] = 1.0 / pmp->precalc.value[2];

	dvala = pow( dvala, pmp->precalc.value[2] );
	dvala = cos(phi0) * dvala * pmp->precalc.value[3];	/* F */

	pmp->precalc.value[1] = R_scale * dvala;		/* R*F */

	/* since phi0 = 0 */
	pmp->precalc.value[0] = pmp->precalc.value[1]; 		/* rho0 */

	break;

case mercator:

/*

MERCATOR projection

*/
	
	break;

case mollweide:

/*

MOLLWEIDE (homalographic) projection

*/
	
	pmp->precalc.value[0] = sqrt( 2.0 ) * R_scale;
	CHECKif( pmp->precalc.value[0] == 0 );

	break;	

case orthographic:

/*

ORTHOGRAPHIC projection

*/
	break;	

case perspective:

/*

PERSPECTIVE projection 

*/
	break;

case sinusoidal:

/*

SINUSOIDAL projection

*/
	break;	

case stereographic:

/*

STEREOGRAPHIC projection

*/

	pmp->precalc.value[0] = 2.0 * R_scale;
	CHECKif( pmp->precalc.value[0] == 0 );
	
	break;	}


return mpSUCCESS;
}

/******************************************************************************

FUNCTION mp_load_EULER_rotation_matrix

For a given CENTER_LATITUDE, CENTER_LONGITUDE, and SPHERICAL_AZIMUTH -
map projection data object keyword values which correspond to the 
Euler angles defining a rotation of a sphere - determine the 3x3 orthogonal
rotation matrix to transform the spherical coordinate system to handle
oblique and transverse projections.

30Jan97 -Scholten-  added projection_number to argument list
08apr97 -Scholten-  force computation of authalic/conformal latitudes by 'trans'
		calls
*******************************************************************************/
int mp_load_EULER_rotation_matrix( MP mp, MP_NUMBER projection_number)
{
int 	status, i_temp;
char	string[120];
double 	center_latitude;		/* Center latitude in radians	*/
double 	center_longitude;		/* Center longitude in radians	*/
double 	spherical_azimuth;		/* Spherical azimuth in radians */
double 	cos_lat, sin_lat;		/* Precalcaluted cosines and sines */
double 	cos_lon, sin_lon;
double 	cos_azi, sin_azi;
struct 	MP_STRUCTURE *pmp;
extern int mp_debug;

pmp = ( struct MP_STRUCTURE *) mp;

/*

If Euler angles of rotation are all zero, return to calling routine.

*/

if ( 	pmp->mpro.center_latitude==0.0 && 
	pmp->mpro.center_longitude==0.0 && 
	pmp->mpro.spherical_azimuth==0.0 ) return mpSUCCESS;

/*

Check if southern hemisphere and determine angle of rotation for latitude.
Second, convert center_latitude to radians.

*/

center_latitude = -1.0 * pmp->mpro.center_latitude;
status = degrees2radians( &center_latitude );

switch ( projection_number ) 	
    {
/*
    Supported authalic map projections include:	ALBERS
						CYLINDRICAL EQUAL-AREA
						LAMBERT AZIMUTHAL EQUAL-AREA
						MOLLWEIDE
						SINUSOIDAL

*/
    case albers_one_p:
    case albers_two_p:
    case cylindrical_equal_area:
    case lambert_azimuthal:
    case mollweide:
    case sinusoidal:

    if ((strncmp(pmp->model,"O",1)==0)||(strncmp(pmp->model,"T",1)==0)) 
	{
	i_temp = pmp->transformation_direction;
	pmp->transformation_direction=LAT_LON_TO_LINE_SAMP;
	status = planetodetic_authalic_trans(mp,&center_latitude,
	 &center_latitude );
	if (status != mpSUCCESS) return status;
	pmp->transformation_direction = i_temp;
	}
    break;
    
/*
    Supported conformal map projections include: LAMBERT CONFORMAL CONIC
						MERCATOR
						STEREOGRAPHIC
*/
    case lambert_one_p:
    case lambert_two_p:
    case mercator:
    case stereographic:
	i_temp = pmp->transformation_direction;
	pmp->transformation_direction=LAT_LON_TO_LINE_SAMP;
	status = planetodetic_conformal_trans(mp,&center_latitude, 
	 &center_latitude );
	if (status != mpSUCCESS) return status;
	pmp->transformation_direction = i_temp;
    break;

    default: ;
    }

if (status != mpSUCCESS) return status;

/*

If center latitude is greater than PI over two, return error.

*/

if ( fabs(center_latitude) > PI_OVER_2 ) return mpKEYWORD_NOT_SET;

/*

Determine longitude translation longitude to rotate sphere about the z axis
without rotating the coordinate system axes. Note that center_longitude is
set to zero to avoid such a coordinate system rotation.

*/

if ( strcmp( pmp->glob.positive_longitude_direction,"EAST" ) == 0 || 
     strcmp( pmp->glob.positive_longitude_direction,"east" ) == 0 )
	pmp->precalc.translation_longitude = -1.0 * pmp->mpro.center_longitude - pmp->glob.body_long_axis;
else
	pmp->precalc.translation_longitude = pmp->mpro.center_longitude + pmp->glob.body_long_axis;

center_longitude = 0.0;

/*

Convert spherical azimuth to counter-clockwise rotation to match Euler
rotation matrix.  Second, convert spherical azimuth to radians.

*/

spherical_azimuth =  360.0 - pmp->mpro.spherical_azimuth;
status = degrees2radians( &spherical_azimuth );
if (status != mpSUCCESS) return status;
  
/*

Calculate values of Euler rotation matrix.

*/

cos_lat	= cos( center_latitude );
sin_lat	= sin( center_latitude );
cos_lon	= cos( center_longitude );
sin_lon	= sin( center_longitude );
cos_azi	= cos( spherical_azimuth );
sin_azi	= sin( spherical_azimuth );

/*

Check for zeroes hiding as very small numbers

*/

if( fabs(cos_lat) < E_NEGATIVE_10 ) cos_lat = 0;
if( fabs(sin_lat) < E_NEGATIVE_10 ) sin_lat = 0;
if( fabs(cos_lon) < E_NEGATIVE_10 ) cos_lon = 0;
if( fabs(sin_lon) < E_NEGATIVE_10 ) sin_lon = 0;
if( fabs(cos_azi) < E_NEGATIVE_10 ) cos_azi = 0;
if( fabs(sin_azi) < E_NEGATIVE_10 ) sin_azi = 0;

pmp->precalc.Euler_matrix[0][0] = cos_azi;
pmp->precalc.Euler_matrix[1][0] = cos_lat * sin_azi;
pmp->precalc.Euler_matrix[2][0] = sin_lat * sin_azi;

pmp->precalc.Euler_matrix[0][1] = -1.0 * sin_azi;
pmp->precalc.Euler_matrix[1][1] = cos_lat * cos_azi;
pmp->precalc.Euler_matrix[2][1] = sin_lat * cos_azi;

pmp->precalc.Euler_matrix[0][2] = 0;
pmp->precalc.Euler_matrix[1][2] = -1.0 * sin_lat;
pmp->precalc.Euler_matrix[2][2] = cos_lat;

pmp->precalc.rotation_flags |= SPHERICAL_COORDINATE_ROTATION;

if( mp_debug )
	{
	zvmessage("\n\tEULER matrix\n"," ");
	sprintf(string,"\tfor ctr_lat=%5.3f, ctr_lon=%6.3f, spher_az=%5.3f",
		pmp->mpro.center_latitude, 
		pmp->mpro.center_longitude, 
		pmp->mpro.spherical_azimuth);
	zvmessage(string," ");	

	sprintf(string,"\n\tE 00, 01, 02 = | %4.3e %4.3e %4.3e |",
	pmp->precalc.Euler_matrix[0][0],
	pmp->precalc.Euler_matrix[0][1],
	pmp->precalc.Euler_matrix[0][2] );	
	zvmessage(string," ");	

	sprintf(string,"\t  10, 11, 12 = | %4.3e %4.3e %4.3e |",
	pmp->precalc.Euler_matrix[1][0],
	pmp->precalc.Euler_matrix[1][1],
	pmp->precalc.Euler_matrix[1][2] );
	zvmessage(string," ");	

	sprintf(string,"\t  20, 21, 22 = | %4.3e %4.3e %4.3e |",
	pmp->precalc.Euler_matrix[2][0],
	pmp->precalc.Euler_matrix[2][1],
	pmp->precalc.Euler_matrix[2][2] );
	zvmessage(string," ");	
	}

return mpSUCCESS;
}

/******************************************************************************

FUNCTION mp_perform_EULER_rotation

For a given planetocentric latitude and longitude, perform a spherical 
coordinate system 3D rotation using the Euler rotation matrix stored in the 
map projection data object.

*******************************************************************************/
int mp_perform_EULER_rotation( 
  MP mp, 			     /* Pointer to mp data object	    */
  double *planetocentric_latitude,   /* Planetocentric latitude in radians  */
  double *planetocentric_longitude ) /* Planetocentric longitude in radians */
{
int	i,j;				/* Loop control variables	*/
char	string[80];

double	input_vector[3];		/* Initial Cartesian vector	*/
double	rotated_vector[3];		/* Rotated Cartesian vector	*/
double	scratch;			/* Dummy scratch variable	*/
double	scratch2;			/* Dummy scratch variable	*/
double  translated_longitude;		/* Amount of longitude shift	*/

struct MP_STRUCTURE *pmp;

extern int mp_debug;

pmp = ( struct MP_STRUCTURE *) mp;

/*

Perform rotation of sphere about the z-axis (u3) by CENTER_LONGITUDE
to obtain a new, artificial prime meridian at CENTER_LONGITUDE.

*/

while( *planetocentric_longitude < 0.0 ) *planetocentric_longitude += 2 * PI;

if ( pmp->transformation_direction == LAT_LON_TO_LINE_SAMP )
	translated_longitude = *planetocentric_longitude +
		RETURN_RADIANS( pmp->precalc.translation_longitude );
else
	translated_longitude = *planetocentric_longitude;

/*

Calculate input vector in 3-space for lat,lon point

*/

scratch	= cos( *planetocentric_latitude );
input_vector[0]	= -1.0 * sin( translated_longitude ) * scratch;
input_vector[1]	= cos( translated_longitude ) * scratch;
input_vector[2]	= sin( *planetocentric_latitude );

/*

Set to zero any very, very small numbers hiding below threshold precision.

*/

for( i=0;i<3;i++ )
	if( fabs(input_vector[i]) < E_NEGATIVE_10 ) input_vector[i] = 0;

/*

Perform EULER rotation

1) Rotate sphere about the axis perpendicular to translated prime meridian
   by the angle to bring CENTER_LATITUDE to the equator.
2) Rotate sphere about the new, translated z axis by the spherical azimuth, 
   but in a counter-clockwise fashion.

*/

for ( i=0; i<3; i++ )
   {
   scratch = 0.0;

   for ( j=0; j<3; j++ )
      if ( pmp->transformation_direction == LAT_LON_TO_LINE_SAMP )
	scratch += input_vector[j] * pmp->precalc.Euler_matrix[i][j];
      else
	scratch += input_vector[j] * pmp->precalc.Euler_matrix[j][i];

   rotated_vector[i] = scratch;
   }

/*

Again, set any very, very small numbers hiding below threshold precision
to zero.

*/

for( i=0;i<3;i++ )
	if( fabs(rotated_vector[i]) < E_NEGATIVE_10 ) rotated_vector[i] = 0;

if ( rotated_vector[0] > 1.0 || rotated_vector[0] < -1.0 ||
     rotated_vector[1] > 1.0 || rotated_vector[1] < -1.0 ||
     rotated_vector[2] > 1.0 || rotated_vector[2] < -1.0 ) return mpFAILURE;

/*

Calculate new, rotated latitude.

*/

*planetocentric_latitude  = asin( rotated_vector[2] );	

/*

Determine longitude of rotated vector, which entails determining the
quadrant in equatorial plane of sphere and then calculating longitude.

1) Check if one of the vector components is zero, then determine longitude.
2) If neither component is zero, determine the quadrant in which vector
   resides, starting with quadrant 1 (0-90 degrees east) and proceeding
   counter-clockwise.  Once quadrant is found, use appropriate fraction of
   vector components 1 and 2 to calculate longitude.

*/

if( rotated_vector[0] == 0 && rotated_vector[1] == 0 ) 
    *planetocentric_longitude = 0;	
else
    if( rotated_vector[0] == 0 )
	if( rotated_vector[1] > 0 )
		*planetocentric_longitude = 0;	
	else	
		*planetocentric_longitude = PI;	
    else
	if( rotated_vector[1] == 0 )
		if( rotated_vector[0] > 0 )
			*planetocentric_longitude = 1.50 * PI;	
		else	
			*planetocentric_longitude = PI_OVER_2;	
	else
		{	
		/*

		Quadrant 1 ?
	
		*/
		if( rotated_vector[0] < 0 && rotated_vector[1] > 0 )
			{
			scratch = fabs( rotated_vector[0] );
			*planetocentric_longitude = atan2( scratch, 
				rotated_vector[1] );
			}

		/*

		Quadrant 2 ?
	
		*/
		if( rotated_vector[0] < 0 && rotated_vector[1] < 0 )
			{
			scratch = fabs( rotated_vector[1] );
			scratch2 = fabs( rotated_vector[0] );
			*planetocentric_longitude = atan2( scratch, scratch2 );
			*planetocentric_longitude += PI_OVER_2;
			}

		/*

		Quadrant 3 ?
	
		*/
		if( rotated_vector[0] > 0 && rotated_vector[1] < 0 )
			{
			scratch = fabs( rotated_vector[1] );
			*planetocentric_longitude = atan2( rotated_vector[0], 
				scratch );
			*planetocentric_longitude += PI;
			}

		/*

		Quadrant 4 ?
	
		*/
		if( rotated_vector[0] > 0 && rotated_vector[1] > 0 )
			{
			*planetocentric_longitude = atan2( rotated_vector[1], 
				rotated_vector[0] );
			*planetocentric_longitude += (1.50 * PI);
			}
		}		

/*

If undoing rotation, add the negative of the translation longitude.

*/

if ( pmp->transformation_direction == LINE_SAMP_TO_LAT_LON )
	*planetocentric_longitude -= 
		RETURN_RADIANS( pmp->precalc.translation_longitude );

if ( mp_debug )
	{
	zvmessage("\n\t***",0);
	zvmessage("\t*** SHOW EULER ROTATION OF VECTOR",0);
	zvmessage("\t***",0);
	zvmessage("\t***",0);

	for ( i=0; i<3; i++ )
		{
		sprintf(string,"\t*** INPUT_VECTOR[%d] = %e",i,input_vector[i]);
		zvmessage(string,0);
		}

	zvmessage("\t*** ",0);

	for ( i=0; i<3; i++ )
		{
		sprintf(string,"\t*** ROTATED_VECTOR[%d] = %e",
			i,rotated_vector[i]);
		zvmessage(string,0);
		}

	zvmessage("\t***",0);
	sprintf(string,"\t*** Rotated (LAT,LON) is (%e,%e east)",
			RETURN_DEGREES( *planetocentric_latitude ),
			RETURN_DEGREES( *planetocentric_longitude ));
	zvmessage(string,0);
	zvmessage("\t***\n\n",0);
	}

return mpSUCCESS;
}
/******************************************************************************

FUNCTION mp_load_cosine_sine_CARTESIAN

For a given CARTESIAN_AZIMUTH, set in spherical precalculation structure of the
map projection data object the values of cos( CARTESIAN_AZIMUTH ) and 
sin( CARTESIAN_AZIMUTH ).

*******************************************************************************/
int mp_load_cosine_sine_CARTESIAN( MP mp ) 
{
struct MP_STRUCTURE *pmp;
double 	cartesian_azimuth;

pmp = ( struct MP_STRUCTURE *) mp;

if( pmp->mpro.cartesian_azimuth != 0.0 )
	{
	cartesian_azimuth = pmp->mpro.cartesian_azimuth;
	degrees2radians( &cartesian_azimuth );

	pmp->precalc.cosine_of_CARTESIAN_AZIMUTH = cos( cartesian_azimuth );
	pmp->precalc.sine_of_CARTESIAN_AZIMUTH = sin( cartesian_azimuth );

	pmp->precalc.rotation_flags |= CARTESIAN_COORDINATE_ROTATION;
	}

return mpSUCCESS;
}


/******************************************************************************

FUNCTION mp_perform_CARTESIAN_trans

For a given map projected line and sample, perform a Cartesian coordinate 
system (2D) rotation and translation using the values of SPHERICAL_AZIMUTH
and CARTESIAN_AZIMUTH from the map projection data object.

Note about values used in transformation:

CARTESIAN_AZIMUTH provides the clockwise angular rotation of the line and
sample coordinates with respect to the map projection origin. This value
defines 'up' in the map projection.

SPHERICAL_AZIMUTH provides the clockwise angular rotation about the radius
vector from the center of the sphere to the CENTER_LATITUDE, CENTER_LONGITUDE.

*******************************************************************************/
int mp_perform_CARTESIAN_trans( 
	MP mp,	 			/* Pointer to mp data object	*/
	double *line, 			/* Line of map projection	*/
	double *sample )		/* Sample of map projection	*/
{
double	scratch;			/* Dummy scratch variable	*/

struct MP_STRUCTURE *pmp;

pmp = ( struct MP_STRUCTURE *) mp;

switch ( pmp->transformation_direction ) {

case	LAT_LON_TO_LINE_SAMP:	
	
	/* 
	Perform Cartesian (2D) rotation 
	*/

	if ( pmp->precalc.rotation_flags & CARTESIAN_COORDINATE_ROTATION )
	  	{
	  	scratch = *sample * pmp->precalc.cosine_of_CARTESIAN_AZIMUTH
			+ *line * pmp->precalc.sine_of_CARTESIAN_AZIMUTH;

		*line =  - *sample *  pmp->precalc.sine_of_CARTESIAN_AZIMUTH
			+ *line * pmp->precalc.cosine_of_CARTESIAN_AZIMUTH;
		
		*sample = scratch;
		}

	/*
	Perform Cartesian translation.
	*/

	/* the Line/Sample offsets are relative to (1,1), but the
	 * projection equations generally are centered on (0,0), hence
	 * the extra +1 : */
	*line	= pmp->mpro.line_projection_offset + 1 - *line;
	*sample	+= pmp->mpro.sample_projection_offset + 1;

	break;
	
case	LINE_SAMP_TO_LAT_LON:

	/*
	Perform Cartesian translation.
	*/

	/* the Line/Sample offsets are relative to (1,1), but the
	 * projection equations generally are centered on (0,0), hence
	 * the extra +1 : */
	*line	= pmp->mpro.line_projection_offset + 1 - *line;
	*sample	-= pmp->mpro.sample_projection_offset + 1;
	
	/* 
	Perform Cartesian (2D) rotation 
	*/

	if ( pmp->precalc.rotation_flags & 
		CARTESIAN_COORDINATE_ROTATION )
	  	{
	  	scratch = *sample * pmp->precalc.cosine_of_CARTESIAN_AZIMUTH
			- *line * pmp->precalc.sine_of_CARTESIAN_AZIMUTH;

		*line = *sample *  pmp->precalc.sine_of_CARTESIAN_AZIMUTH
			+ *line * pmp->precalc.cosine_of_CARTESIAN_AZIMUTH;
		
		*sample = scratch;
		}
	
	break;

default:
	
	return mpFAILURE;
                                                }
                                                
return mpSUCCESS;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpmpo2buf.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include <time.h>
#include "mp_routines.h"
#include <ctype.h>
#include "momati.h"

#ifndef PI
#define PI ( 2.0 * acos(0.0) )
#endif

#define det2cen(lat) ( (180./PI) * atan( tan(lat*PI/180.) * rpe2 ) )

/********************************************************************/
	/*
	 * Function mpMpo2Buf:
	 *
	 * routine to convert the new MP map object to an "old"
	 * (pre-1993) VICAR map buffer;  this is the "C" interface
	 *
	 *  17sep93  -lwk-  intial version
	 *  13oct93  -lwk-  fixed "visible pole" assignment
         *  21oct93  -lwk-  replaced union in argument list with casts to
         *              new arrays
         *  19jan94  -lwk-  added +1 to LINE/SAMPLE_PROJECTION_OFFSET
         *              conversion 
         *  28jan94  -lwk-  added SPHERICAL_AZIMUTH as equivalent to CARTESIAN_
	 *		for oblique projections;  fixed problem overwriting
	 *		buffer with double-precision mpGetValues returns
         *  09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
         *              to MP, since it's public
         *  23feb95  -lwk-  corrected errors in OM-matrix computation for
	 *		Perspective projection
	 *  28nov95  -lwk-  added check for East positive_longitude_direction
	 *  15oct96  -lwk-  added check for coordinate system (detic/centric)
	 *  22jan97  -lwk-  fixed check for detic/centric (a bug caused routine
	 *		not to recognize centric)
	 *   1dec97  -lwk- Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE
	 *		(but must continue to recognize former for old labels)
	 *  18dec99  -lwk- added a few projection keyword synomyms
         */

int mpMpo2Buf( MP mp, void *buf)
{
  int i, lattype, pos_lon_east, status;
  double azi, azic, azis, dval, mom[10], rpe2;
  char maptype[mpMAX_KEYWD_LENGTH+1], pos_lon[mpMAX_KEYWD_LENGTH+1], csys[15];
  int *int_buf = (int *)buf;
  float *float_buf = (float *)buf;

/*  The structure of the old VICAR standard MAP data buffer is:
 *
 *	These two FORTRAN arrays are overlaid:
 *	REAL*4 RDATA(40)
 *	INTEGER IDATA(40)
 *
 *	IDATA(39)= PROJECTION TYPE
 *			1=POLAR ORTHOGRAPHIC
 *			2=OBLIQUE    "
 *			3=POLAR STEREOGRAPHIC
 *			4=OBLIQUE    "
 *			5=LAMBERT
 *			6=MERCATOR
 *			7=RAW UNCORRECTED IMAGE (IMAGE SPACE)
 *			8=GEOMETRICALLY CORRECTED IMAGE (OBJECT SPACE)
 *			9=NORMAL CYLINDRICAL
 *		       10=SIMPLE     "
 *                     11=OBLIQUE SIMPLE CYLINDRICAL
 *                     12=SINUSOIDAL
 *                     13=OBLIQUE SINUSOIDAL
 *                     14=MOLLWEIDE
 *                     15=TRANSVERSE MERCATOR
 *                     16=PERSPECTIVE
 *
 *    For projection codes 1-6 and 9-15:
 *
 *	RDATA(1) = SPECIAL SAMPLE POINT             
 *	RDATA(2) = SPECIAL LINE POINT               
 *	RDATA(3) = SPECIAL LATITUDE POINT           
 *	RDATA(4) = LATITUDE OF SPEC PARALLEL (DEG)  
 *	RDATA(5) = LATITUDE OF SPEC PARALLEL  (DEG) 
 *	RDATA(6) = SPECIAL LONGITUDE (WEST) (DEG)   
 *	RDATA(7) = SCALE (KM/PIXEL)                 
 *	RDATA(8) = VISIBLE POLE  1=N -1=S           
 *	RDATA(9) = NORTH ANGLE                      
 *	RDATA(25)= POLAR RADIUS (KM)                
 *	RDATA(26)= EQUATORIAL RADIUS (KM)           
 *
 *  For projection code 16:
 *
 *       RDATA(1-18)  = OM matrix              
 *       RDATA(19-24) = RS vector
 *	 RDATA(25) =  polar radius (km)            
 *	 RDATA(26) =  equatorial radius (km) 
 *       RDATA(27) = focal length 
 *       RDATA(28) = optical axis line        
 *       RDATA(29) = optical axis sample      
 *       RDATA(30) = scale in pixels/mm.      
 *       RDATA(31) = s/c latitude in degrees
 *       RDATA(32) = s/c longitude in degrees
 *       RDATA(33) = line
 *       RDATA(34) = sample
 *       RDATA(35) = North angle
 *	 RDATA(36) = Body Long Axis in degrees
 *	 RDATA(37) = B-Axis radius (km)
 *       RDATA(38) = range to target body     
 *       RDATA(39) = 16
 *
 *  For the case when the projection type is 7 or 8 (Image or Object
 *  space), the buffer is not defined.
 *
 *  Each of the above items has a corresponding element in the MP Object
 *  except:
 *
 *    Visible Pole (word 8 for non-Perspective) - this is determined by
 *	the "Special Latitude" (word 3);
 *
 *    OM-Matrix and RS-Vector (words 1-24 for Perspective) - these are
 *	computed using VICAR routine MOMATI.
 *
 *  Note that in the MAP buffer, all longitudes are West and all latitudes
 *  are planetocentric!  (In the MP routines, the reverse is the default in
 *  both cases.)
 */

	/* initialize the buffer: */
  for (i=0; i<40; i++) float_buf[i] = 0.0;

	/* these items are needed for all projections: */

  status = mpGetValues( mp, mpMAP_PROJECTION_TYPE, maptype, NULL);
  if (status != mpSUCCESS) return status;

  status = mpGetValues( mp, mpCOORDINATE_SYSTEM_NAME, csys, NULL);
  /* save value of this keyword as it has implications for other
     latitude items: */
  lattype = 0;			/* default is planetodetic */
  if (status == mpSUCCESS) {
    for (i=0; i<strlen(csys); i++) csys[i] = toupper(csys[i]);
    if (!strcmp(csys,"PLANETOCENTRIC")) lattype = 1;
  }

  status = mpGetValues( mp, mpPOSITIVE_LONGITUDE_DIRECTION, pos_lon, NULL);
  if (status != mpSUCCESS) return status;
  /* save value of this keyword as it has implications for other
     longitude items: */
  pos_lon_east = 0;
  for (i=0; i<strlen(pos_lon); i++) pos_lon[i] = toupper(pos_lon[i]);
  if (strcmp(pos_lon,"WEST")) pos_lon_east = 1;

  status = mpGetValues( mp, mpC_AXIS_RADIUS, &dval, NULL);
  if (status != mpSUCCESS) return status;
  float_buf[24] = dval;
  status = mpGetValues( mp, mpA_AXIS_RADIUS, &dval, NULL);
  if (status != mpSUCCESS) return status;
  float_buf[25] = dval;
  status = mpGetValues( mp, mpB_AXIS_RADIUS, &dval, NULL);
  if (status != mpSUCCESS) return status;
  float_buf[36] = dval;

  /* needed for detic/centric conversion: */
  rpe2 = (float_buf[24]*float_buf[24]) / (float_buf[25]*float_buf[25]);

  status = mpGetValues( mp, mpBODY_LONG_AXIS, &dval, NULL);
  if (status != mpSUCCESS) return status;
  float_buf[35] = dval;

	/* now projection-specific stuff: */

  if (EQUAL( maptype, mpPOINT_PERSPECTIVE)) {	/* PERSPECTIVE is special */

    int_buf[38] = 16;

    /* retrieve the double-precision MP items -- these must be stored
     * as single-prec. for the output buffer, but as double for MOMATI: */

    status = mpGetValues( mp, mpFOCAL_LENGTH, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[26] = dval;
    mom[5] = dval;

    status = mpGetValues( mp, mpOPT_AXIS_INTERCEPT_LINE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[27] = dval;
    mom[0] = dval;

    status = mpGetValues( mp, mpOPT_AXIS_INTERCEPT_SAMPLE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[28] = dval;
    mom[1] = dval;

    status = mpGetValues( mp, mpFOCAL_PLANE_SCALE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[29] = dval;
    mom[4] = dval;

    status = mpGetValues( mp, mpSUB_SPACECRAFT_LATITUDE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    if (lattype==0) dval = det2cen(dval);
    float_buf[30] = dval;
    mom[7] = dval;

    status = mpGetValues( mp, mpSUB_SPACECRAFT_LONGITUDE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    if (pos_lon_east) dval = 360.0 - dval;
    if (dval<0.0) dval += 360.0;
    float_buf[31] = dval;
    mom[6] = dval;

    status = mpGetValues( mp, mpPLANET_CENTER_LINE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[32] = dval;
    mom[2] = dval;

    status = mpGetValues( mp, mpPLANET_CENTER_SAMPLE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[33] = dval;
    mom[3] = dval;

    status = mpGetValues( mp, mpNORTH_ANGLE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[34] = dval;
    mom[8] = dval;

    status = mpGetValues( mp, mpTARGET_CENTER_DISTANCE, &dval, NULL);
    if (status != mpSUCCESS) {
      status = mpGetValues( mp, mpSPACECRAFT_DISTANCE, &dval, NULL);
      if (status != mpSUCCESS) return status;
    }
    float_buf[37] = dval;
    mom[9] = dval;

	/* compute OM-matrix & RS-vector */
      /* Yes, we really are stuffing doubles into float_buf 1-18 and 19-24. */
      momati_c( mom[0], mom[1], mom[2], mom[3], mom[4], mom[5], mom[6],
		mom[7], mom[8], mom[9],
		(double(*)[3])&float_buf[0], (double *)&float_buf[18]);

  } else {				/* "standard" map projections */

      /* these items are common to all standard projections: */

        /* PDS offsets are with respect to pixel (1,1), while VICAR's
         * are to (0,0): */
    status = mpGetValues( mp, mpSAMPLE_PROJECTION_OFFSET, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[0] = dval + 1.0;
    status = mpGetValues( mp, mpLINE_PROJECTION_OFFSET, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[1] = dval + 1.0;

    status = mpGetValues( mp, mpCENTER_LONGITUDE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    if (pos_lon_east) dval = 360.0 - dval;
    if (dval<0.0) dval += 360.0;
    float_buf[5] = dval;

    status = mpGetValues( mp, mpMAP_SCALE, &dval, NULL);
    if (status != mpSUCCESS) return status;
    float_buf[6] = dval;

	/* projection-specific items: */

	/* none of the old-style VICAR map projections use the two
	 * AZIMUTH items separately, but for convenience we allow either
	 * one to be specified -- if both are, and are different, then
	 * we just add them */
    if ( EQUAL( maptype, mpOBLIQUE_ORTHOGRAPHIC) ||
	 EQUAL( maptype, mpOBLIQUE_STEREOGRAPHIC) ||
         EQUAL( maptype, mpORTHOGRAPHIC) ||
	 EQUAL( maptype, mpSTEREOGRAPHIC) ||
         EQUAL( maptype, mpOBLIQUE_CYLINDRICAL) ||
         EQUAL( maptype, mpOBLIQUE_SIMPLE_CYLINDRICAL) ||
	 EQUAL( maptype, mpSINUSOIDAL) ||
	 EQUAL( maptype, mpOBLIQUE_SINUSOIDAL) ) {
      status = mpGetValues( mp, mpCARTESIAN_AZIMUTH, &azic, NULL);
      if (status != mpSUCCESS) azic = 0.0;
      status = mpGetValues( mp, mpSPHERICAL_AZIMUTH, &azis, NULL);
      if (status != mpSUCCESS) azis = 0.0;
      if (azic==azis) azi = azis;
      else if (azic==0.0) azi = azis;
      else if (azis==0.0) azi = azic;
      else azi = azis + azic;
      if ( EQUAL( maptype, mpOBLIQUE_ORTHOGRAPHIC) ||
           EQUAL( maptype, mpORTHOGRAPHIC) ||
	   EQUAL( maptype, mpSTEREOGRAPHIC) ||
	   EQUAL( maptype, mpOBLIQUE_STEREOGRAPHIC) ) float_buf[8] = azi;
      else float_buf[3] = azi;
    }

	/* only Lambert uses the 2 std. parallels */
    if (EQUAL( maptype, mpLAMBERT) ||
        EQUAL( maptype, mpLAMBERT_CONFORMAL) ||
        EQUAL( maptype, mpLAMBERT_TWO_PARALLELS) ) {
      status = mpGetValues( mp, mpFIRST_STANDARD_PARALLEL, &dval, NULL);
      if (status != mpSUCCESS) return status;
      if (lattype==0) dval = det2cen(dval);
      float_buf[3] = dval;
      status = mpGetValues( mp, mpSECOND_STANDARD_PARALLEL, &dval, NULL);
      if (status != mpSUCCESS) return status;
      if (lattype==0) dval = det2cen(dval);
      float_buf[4] = dval;
    }

	/* all projections EXCEPT Lambert & Mollweide have Center Lat */
    if (strcmp( maptype, mpLAMBERT) &&
        strcmp( maptype, mpLAMBERT_CONFORMAL) &&
        strcmp( maptype, mpLAMBERT_TWO_PARALLELS) &&
	strcmp( maptype, mpMOLLWEIDE) &&
	strcmp( maptype, mpHOMALOGRAPHIC) ) {
      status = mpGetValues( mp, mpCENTER_LATITUDE, &dval, NULL);
      if (status != mpSUCCESS) return status;
      if (lattype==0 && fabs(dval)<89.9999999) dval = det2cen(dval);
      float_buf[2] = dval;
    }

	/* "visible pole" depends on special latitude for these projections: */
    float_buf[7] = 1.0;			/* (VICAR default) */
    if ( EQUAL( maptype, mpPOLAR_ORTHOGRAPHIC) ||
	 EQUAL( maptype, mpPOLAR_STEREOGRAPHIC) ) {
      if (float_buf[2]<0.) float_buf[7] = -1.0;
    }
    else if ( EQUAL( maptype, mpLAMBERT) ||
              EQUAL( maptype, mpLAMBERT_CONFORMAL) ||
              EQUAL( maptype, mpLAMBERT_TWO_PARALLELS) ) {
      if (float_buf[3]<0.) float_buf[7] = -1.0;
    }

	/* and the map code: */
    if (EQUAL( maptype, mpPOLAR_ORTHOGRAPHIC)) int_buf[38] = 1;
    else if (EQUAL( maptype, mpORTHOGRAPHIC)) int_buf[38] = 2;
    else if (EQUAL( maptype, mpOBLIQUE_ORTHOGRAPHIC)) int_buf[38] = 2;
    else if (EQUAL( maptype, mpPOLAR_STEREOGRAPHIC)) int_buf[38] = 3;
    else if (EQUAL( maptype, mpSTEREOGRAPHIC)) int_buf[38] = 4;
    else if (EQUAL( maptype, mpOBLIQUE_STEREOGRAPHIC)) int_buf[38] = 4;
    else if (EQUAL( maptype, mpLAMBERT)) int_buf[38] = 5;
    else if (EQUAL( maptype, mpLAMBERT_CONFORMAL)) int_buf[38] = 5;
    else if (EQUAL( maptype, mpLAMBERT_TWO_PARALLELS)) int_buf[38] = 5;
    else if (EQUAL( maptype, mpMERCATOR)) int_buf[38] = 6;
    else if (EQUAL( maptype, mpNORMAL_CYLINDRICAL)) int_buf[38] = 9;
    else if (EQUAL( maptype, mpCYLINDRICAL_EQUAL_AREA)) int_buf[38] = 9;
    else if (EQUAL( maptype, mpCYLINDRICAL)) int_buf[38] = 10;
    else if (EQUAL( maptype, mpEQUIDISTANT)) int_buf[38] = 10;
    else if (EQUAL( maptype, mpRECTANGULAR)) int_buf[38] = 10;
    else if (EQUAL( maptype, mpSIMPLE_CYLINDRICAL)) int_buf[38] = 10;
    else if (EQUAL( maptype, mpOBLIQUE_CYLINDRICAL)) int_buf[38] = 11;
    else if (EQUAL( maptype, mpOBLIQUE_SIMPLE_CYLINDRICAL)) int_buf[38] = 11;
    else if (EQUAL( maptype, mpSINUSOIDAL)) int_buf[38] = 12;
    else if (EQUAL( maptype, mpOBLIQUE_SINUSOIDAL)) int_buf[38] = 13;
    else if (EQUAL( maptype, mpMOLLWEIDE)) int_buf[38] = 14;
    else if (EQUAL( maptype, mpHOMALOGRAPHIC)) int_buf[38] = 14;
    else if (EQUAL( maptype, mpTRANSVERSE_MERCATOR)) int_buf[38] = 15;
    else return mpINVALID_PROJECTION;
  }

  return mpSUCCESS;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mp_init.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*

MP_INIT.H

	Defines mpMAP_PROJ_TYPES, mpMAP_PROJ_BITS, mpKEYWORD_TO_CODE.

*/

	/* An array of all the valid, currently supported map	*/
	/* projections. Synonyms are listed directly beneath 	*/
	/* the PDS standard name for the map projection type.   */

char mpMAP_PROJ_TYPES[mpNUMBER_OF_PROJECTIONS][mpMAX_KEYWD_LENGTH] = {

	mpALBERS, 			/* Alber's Equal-Area Conic 	*/
	mpALBERS_ONE_PARALLEL, 
	mpALBERS_TWO_PARALLELS,		

	mpCYLINDRICAL_EQUAL_AREA,	/* Cylindrical Equal-Area	*/
	mpNORMAL_CYLINDRICAL, 

	mpEQUIDISTANT, 			/* Equidistant Cylindrical	*/
	mpCYLINDRICAL, 
	mpRECTANGULAR,
	mpSIMPLE_CYLINDRICAL, 
	mpOBLIQUE_SIMPLE_CYLINDRICAL,

	mpLAMBERT_AZIMUTHAL,		/* Lambert Azimuthal Equal-Area */

	mpLAMBERT, 			/* Lambert Conformal Conic	*/
	mpLAMBERT_CONFORMAL,
	mpLAMBERT_ONE_PARALLEL,
	mpLAMBERT_TWO_PARALLELS,

	mpMERCATOR, 			/* Mercator			*/
	mpTRANSVERSE_MERCATOR,

	mpMOLLWEIDE,			/* Molleweide (Homalographic)	*/
	mpHOMALOGRAPHIC,		

	mpORTHOGRAPHIC,			/* Orthographic			*/
	mpOBLIQUE_ORTHOGRAPHIC, 
	mpPOLAR_ORTHOGRAPHIC,

	mpSINUSOIDAL, 			/* Sinusoidal			*/
	mpOBLIQUE_SINUSOIDAL,

	mpSTEREOGRAPHIC, 		/* Stereographic		*/
	mpOBLIQUE_STEREOGRAPHIC, 
	mpPOLAR_STEREOGRAPHIC,

	mpPOINT_PERSPECTIVE		/* Perspective                  */
};

	/* these flags define which items in the MAP_PROJ group are 
	 * valid for the various map projections types: */

int mpMAP_PROJ_BITS[mpNUMBER_OF_PROJECTIONS] = {

	0x03773,		/* ALBERS			*/
	0x01773,		/* ALBERS, ONE PARALLEL		*/
	0x03773,		/* ALBERS, TWO PARALLELS	*/

	0x01773,		/* CYLINDRICAL EQUAL-AREA	*/

	0x01773,		/* EQUIDISTANT	 		*/
	0x01773,		/* CYLINDRICAL	 		*/
	0x01773,		/* RECTANGULAR	 		*/
	0x01773,		/* NORMAL CYLINDRICAL 		*/
	0x01773,		/* SIMPLE CYLINDRICAL 		*/
	0x01773,		/* OBLIQUE SIMPLE CYLINDRICAL 	*/

	0x01773,		/* LAMBERT AZIMUTHAL		*/

	0x03773,		/* LAMBERT  			*/
	0x03773,		/* LAMBERT CONFORMAL 		*/
	0x01773,		/* LAMBERT, ONE PARALLEL	*/
	0x03773,		/* LAMBERT, TWO PARALLELS 	*/

	0x00773,		/* MERCATOR 			*/
	0x00773,		/* TRANSVERSE MERCATOR 		*/

	0x00773,		/* MOLLWEIDE 			*/
	0x00773,		/* HOMALOGRAPHIC 		*/

	0x00773,		/* ORTHOGRAPHIC 		*/
	0x00773,		/* POLAR ORTHOGRAPHIC 		*/
	0x00773,		/* OBLIQUE ORTHOGRAPHIC 	*/

	0x00773,		/* SINUSOIDAL 			*/
	0x00773,		/* OBLIQUE SINUSOIDAL 		*/

	0x00773,		/* STEREOGRAPHIC 		*/
	0x00773,		/* POLAR STEREOGRAPHIC 		*/
	0x00773,		/* OBLIQUE STEREOGRAPHIC 	*/

	0			/* POINT PERSPECTIVE 		*/
};

	/* Initialization of the keywords & codes:
	 * (mpMAP_PROJECTION_DESC is excluded from this list since
	 * it is not treated as a normal projection keyword) */

struct mpKEYWORD_TO_CODE mpKwds_Codes[mpNUMBER_OF_KEYWORDS] = {

  {mpTARGET_NAME,		mpTARGET_NAME_BIT,		mpCHAR,	0},
  {mpTARGET_BODY,		mpTARGET_BODY_BIT,		mpCHAR,	0},
  {mpMAP_PROJECTION_TYPE,	mpMAP_PROJECTION_TYPE_BIT,	mpCHAR,	0},
  {mpCOORDINATE_SYSTEM_NAME,	mpCOORDINATE_SYS_NAME_BIT,	mpCHAR,	0},
  {mpPOSITIVE_LONGITUDE_DIRECTION, mpPOSITIVE_LONGIT_DIR_BIT,	mpCHAR,	0},
  {mpA_AXIS_RADIUS, 		mpA_AXIS_RADIUS_BIT, 		mpDBLE,	0},
  {mpB_AXIS_RADIUS, 		mpB_AXIS_RADIUS_BIT, 		mpDBLE,	0},
  {mpC_AXIS_RADIUS, 		mpC_AXIS_RADIUS_BIT, 		mpDBLE,	0},
  {mpBODY_LONG_AXIS,		mpBODY_LONG_AXIS_BIT,		mpDBLE,	0},
  {mpMINIMUM_LATITUDE,		mpMINIMUM_LATITUDE_BIT,		mpDBLE,	0},
  {mpMAXIMUM_LATITUDE,		mpMAXIMUM_LATITUDE_BIT,		mpDBLE,	0},
  {mpMINIMUM_LONGITUDE,		mpMINIMUM_LONGITUDE_BIT,	mpDBLE,	0},
  {mpMAXIMUM_LONGITUDE,		mpMAXIMUM_LONGITUDE_BIT,	mpDBLE,	0},

  {mpMAP_SCALE,			mpMAP_SCALE_BIT,		mpDBLE,	1},
  {mpMAP_RESOLUTION,		mpMAP_RESOLUTION_BIT,		mpDBLE,	1},
  {mpCENTER_LATITUDE,		mpCENTER_LATITUDE_BIT,		mpDBLE,	1},
  {mpCENTER_LONGITUDE,		mpCENTER_LONGITUDE_BIT,		mpDBLE,	1},
  {mpSPHERICAL_AZIMUTH,		mpSPHERICAL_AZIMUTH_BIT,	mpDBLE,	1},
  {mpCARTESIAN_AZIMUTH,		mpCARTESIAN_AZIMUTH_BIT,	mpDBLE,	1},
  {mpLINE_PROJECTION_OFFSET,	mpLINE_PROJECTION_OFFSET_BIT,	mpDBLE,	1},
  {mpSAMPLE_PROJECTION_OFFSET,	mpSAMPLE_PROJECTION_OFFSET_BIT,	mpDBLE,	1},
  {mpFIRST_STANDARD_PARALLEL,	mpFIRST_STANDARD_PARALLEL_BIT,	mpDBLE,	1},
  {mpSECOND_STANDARD_PARALLEL,	mpSECOND_STANDARD_PARALLEL_BIT,	mpDBLE,	1},

  {mpFOCAL_LENGTH,	 	mpFOCAL_LENGTH_BIT,		mpDBLE,	2},
  {mpFOCAL_PLANE_SCALE,		mpFOCAL_PLANE_SCALE_BIT,	mpDBLE,	2},
  {mpNORTH_ANGLE,		mpNORTH_ANGLE_BIT,		mpDBLE,	2},
  {mpOPT_AXIS_INTERCEPT_LINE,	mpOPT_AXIS_INTERCEPT_LINE_BIT,	mpDBLE,	2},
  {mpOPT_AXIS_INTERCEPT_SAMPLE,	mpOPT_AXIS_INTERCEPT_SAMPLE_BIT,mpDBLE,	2},
  {mpPLANET_CENTER_LINE,		mpPLANET_CENTER_LINE_BIT,	mpDBLE,	2},
  {mpPLANET_CENTER_SAMPLE,	mpPLANET_CENTER_SAMPLE_BIT,	mpDBLE,	2},
  {mpSUB_SPACECRAFT_LATITUDE,	mpSUB_SPACECRAFT_LATITUDE_BIT,	mpDBLE,	2},
  {mpSUB_SPACECRAFT_LONGITUDE,	mpSUB_SPACECRAFT_LONGITUDE_BIT,	mpDBLE,	2},
  {mpTARGET_CENTER_DISTANCE,	mpTARGET_CENTER_DISTANCE_BIT,	mpDBLE,	2},
  {mpSPACECRAFT_DISTANCE,	mpSPACECRAFT_DISTANCE_BIT,	mpDBLE,	2},

};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mp_private.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*************************************************************************

	Map Projection Data Object Include File

	Purpose

	This is an include file that is used by all routines of the 
	mp_ set of map projection software. This is a private include
	which is not to be included in application programs which
	call the mp_routines functions.

	Cognizant Engineer		Justin McNeill

	Date				October 1993

	Revision History ...		See Release Notes.

*/
#include "mp_routines.h"

struct GLOBAL_KEYWORDS {
	char	target_name[32];
	char	target_body[32];	/* obsolete -- used as input only */
	char	map_projection_type[40];
	char	coordinate_system_name[15];
	char	positive_longitude_direction[5];
	double	a_axis_radius;
	double	b_axis_radius;
	double	c_axis_radius;
	double  body_long_axis;
	double  minimum_latitude;
	double  maximum_latitude;
	double  minimum_longitude;
	double  maximum_longitude;
};

struct MAP_PROJ_KEYWORDS {
	double	map_scale;
	double	map_resolution;
	double	center_latitude;
	double	center_longitude;
	double	spherical_azimuth;
	double	cartesian_azimuth;
	double	line_projection_offset;
	double  sample_projection_offset;
	double	first_standard_parallel;
	double	second_standard_parallel;
};

struct PERSPECTIVE_KEYWORDS {
	double	focal_length;
	double 	focal_plane_scale;
	double	north_angle;
	double	opt_axis_intercept_line;
	double	opt_axis_intercept_sample;
	double 	planet_center_line;
	double	planet_center_sample;
	double	sub_spacecraft_latitude;
	double	sub_spacecraft_longitude;
	double	target_center_distance;
};

struct PRECALCULATIONS {
	double  Euler_matrix[3][3];		/* Translation and 	  */
	double  translation_longitude;		/* rotation variables 	  */
	double	cosine_of_CARTESIAN_AZIMUTH;
	double	sine_of_CARTESIAN_AZIMUTH;
	int	rotation_flags;

	double	value[6];			/* General variables	  */
	double	oblate_value[6];
	double	ellipsoid_value[6];

        double	cc_matrix[MLIMIT][NLIMIT];
	double	cp_matrix[MLIMIT][NLIMIT];
	double	ac_matrix[KLIMIT+1][MLIMIT+1];
        double	ap_matrix[NLIMIT];
};

struct 	MP_STRUCTURE {
	struct	GLOBAL_KEYWORDS		glob;
	struct	MAP_PROJ_KEYWORDS	mpro;
	struct	PERSPECTIVE_KEYWORDS	pers;
	struct  PRECALCULATIONS		precalc;
	char	model[30];
	int	transformation_direction;
	int	kwd_set[4];
	int	kwd_chng[4];
	int	precalc_needed;
};

/*

Include general MP file

*/

#include "mp_routines.h"

/******************************************************************************

MP_KEY_CODE.H 	-  	private include file defining the keyword names, values,
			codes, flags for the MP_STRUCTURE processing.

*/

	/* Hexadecimal Representations of MP Keywords:
	 * Each keyword has a bit code associated with it, which is used
	 * by mpGetKeywords to determine valid keywords for a given
	 * projection.  The valid keyword bit masks for the projections
	 * are defined the file mp_init.h.  NOTE that valid bits are
	 * currently only being checked for group 1 (MAP_PROJ_KEYWORDS).
	 * Also NOTE that not all possible bit values are used, so gaps
	 * in the sequence are permitted.  (E.g., every 4th bit is omitted,
	 * for some obscure reason.)
	 */

	/* GLOBAL_KEYWORDS group hexadecimal codes	*/

#define	mpTARGET_NAME_BIT			0x00000001
#define	mpMAP_PROJECTION_TYPE_BIT		0x00000002
#define	mpA_AXIS_RADIUS_BIT 			0x00000004
#define	mpB_AXIS_RADIUS_BIT			0x00000010
#define	mpC_AXIS_RADIUS_BIT 			0x00000020
#define	mpBODY_LONG_AXIS_BIT			0x00000040
#define	mpPOSITIVE_LONGIT_DIR_BIT		0x00000100
#define	mpCOORDINATE_SYS_NAME_BIT		0x00000200
#define	mpMINIMUM_LATITUDE_BIT			0x00000400
#define	mpMAXIMUM_LATITUDE_BIT			0x00001000
#define	mpMINIMUM_LONGITUDE_BIT			0x00002000
#define	mpMAXIMUM_LONGITUDE_BIT			0x00004000
#define	mpTARGET_BODY_BIT			0x00010000

	/* MAP_PROJ_KEYWORDS group hexadecimal codes	*/

#define	mpMAP_SCALE_BIT				0x00000001
#define	mpMAP_RESOLUTION_BIT			0x00000002
#define	mpCENTER_LATITUDE_BIT			0x00000010
#define	mpCENTER_LONGITUDE_BIT			0x00000020
#define	mpSPHERICAL_AZIMUTH_BIT			0x00000040
#define	mpCARTESIAN_AZIMUTH_BIT			0x00000100
#define	mpLINE_PROJECTION_OFFSET_BIT		0x00000200
#define	mpSAMPLE_PROJECTION_OFFSET_BIT		0x00000400
#define	mpFIRST_STANDARD_PARALLEL_BIT		0x00001000
#define	mpSECOND_STANDARD_PARALLEL_BIT		0x00002000

	/* PERSPECTIVE_KEYWORDS group hexadecimal codes	*/

#define	mpFOCAL_LENGTH_BIT			0x00000001
#define	mpFOCAL_PLANE_SCALE_BIT			0x00000002
#define	mpNORTH_ANGLE_BIT			0x00000004
#define	mpOPT_AXIS_INTERCEPT_LINE_BIT		0x00000010
#define	mpOPT_AXIS_INTERCEPT_SAMPLE_BIT		0x00000020
#define	mpPLANET_CENTER_LINE_BIT		0x00000040
#define	mpPLANET_CENTER_SAMPLE_BIT		0x00000100
#define	mpSUB_SPACECRAFT_LATITUDE_BIT		0x00000200
#define	mpSUB_SPACECRAFT_LONGITUDE_BIT		0x00000400
#define	mpTARGET_CENTER_DISTANCE_BIT		0x00001000
#define	mpSPACECRAFT_DISTANCE_BIT		0x00002000

		/*
		 * DEFINES of overall structure properties:
		 * Note:  mpMAX_KEYWD_LENGTH and mpNUMBER_OF_KEYWORDS
		 * are defined in mp_routines.h (formerly mp.h)
		 */

#define	mpNUMBER_OF_GROUPS	3	/* Number of structure groupings */
					/* of keywords.			 */
#define	mpKEYWORDS_MAXIMUM	32	/* Maximum number of keywords in */
					/* a structure grouping.	 */
#define mpMAX_DESCRIPTION_LINES 30	/* Maximum number of lines in a  */
					/* map projection description    */

		/* Stucture to define the relationship	*/
		/* between keyword name, type, hex code	*/
		/* and group number:			*/
struct mpKEYWORD_TO_CODE {
  char	label_kwd[mpMAX_KEYWD_LENGTH+1];
  int	bit_code;
  int	kwd_type;
  int	group_no;
};

/******************************************************************************

MP_TRANS.H

this defines and macros are used by the translation routines
mpll2xy and mpxy2ll and their supporting functions.*/

/* replaced acos by values for speed up, Roatsch, Apr-2001 
#define		PI				( 2.0 * acos(0.0) )
#define		TWO_PI				( 4.0 * acos(0.0) )
#define		PI_OVER_2			( acos(0.0) )
#define		PI_OVER_4			( acos(0.0) / 2.0 ) */

#define		PI				3.1415926535897931159979635
#define		TWO_PI				6.2831853071795862319959269
#define		PI_OVER_2			1.5707963267948965579989817
#define		PI_OVER_4			0.7853981633974482789994909

#define		E_NEGATIVE_10			(1.0e-10)
#define		E_NEGATIVE_12			(1.0e-12)

#define		SPHERICAL_COORDINATE_ROTATION	0x01
#define		CARTESIAN_COORDINATE_ROTATION	0x02

#define		PLANETOCENTRIC			1
#define		PLANETODETIC			2
#define		SNYDER_DEFINED			3
#define		CONFORMAL			4
#define		AUTHALIC			5
			

#define		PERFORM_PRECALCULATIONS		0
#define		LAT_LON_TO_LINE_SAMP		1
#define		LINE_SAMP_TO_LAT_LON		2

/* Simple function macros */

#define		RETURN_SIGN(x)		( ((x)>0) ? 1 : -1 )
#define		RETURN_RADIANS(x) ( 0.0174532925199432954743717 * (double) (x) )
#define		RETURN_DEGREES(x) (57.2957795130823228646477219 * (double) (x) )

/* Map projection number enumeration */

typedef enum	{
	albers_one_p,		/* Albers with one standard parallel 			*/
	albers_two_p,		/* Albers with two standard parallels			*/
	cylindrical_equal_area,	/* Cylindrical equal-area				*/
	equidistant,		/* Equidistant cylindrical (rectangular)		*/
	lambert_azimuthal,	/* Lambert azimuthal equal-area				*/
	lambert_one_p,		/* Lambert conformal conic with one standard parallel 	*/	
	lambert_two_p,		/* Lambert conformal conic with two standard parallels	*/	
	mercator,		/* Mercator (including transverse mercator)		*/
	mollweide,		/* Mollweide (homalographic)				*/
	orthographic,		/* Orthographic (normal,oblique,polar)			*/
	sinusoidal,		/* Sinusoidal (normal,oblique)				*/
	stereographic,		/* Stereographic (normal,oblique,polar)			*/
	perspective		/* Point perspective					*/
}	MP_NUMBER;

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mp_prototypes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************

Function prototypes for internal MP routines (mp_prototypes.h)

Author:			Justin McNeill, JPL
Cognizant Engineer:	Justin McNeill
			MS 168-414
			Jet Propulsion Laboratory
			4800 Oak Grove Drive
			Pasadena, California 91109
			(email) jfm059@ipl.jpl.nasa.gov

Written:		May 1994
Revisions:
  Apr1997 - Frank Scholten - added prototypes for check_longitude_radians
  Aug98   - Bob Deen -  revised for use with C++
*/
#ifndef MP_PROTOTYPES_H
#define MP_PROTOTYPES_H

#ifndef _NO_PROTO

/*

MP Internal function prototypes

	Listed in the order of routines declared in the file
	mp_internals.c.

*/

int degrees2radians( double * );	/* Arithmetic functions */
int radians2degrees( double * );
double check_longitude( double );
double check_longitude_radians( double );
int check_latitude( double );

int determine_map_projection_number( MP, MP_NUMBER * );
int determine_model( MP, double * );
int load_coefficients( MP, double * );

int mpGetDesc( MP, char [][100], int *);	/* used by mpLabelWrite */

int mpSphere( MP, double *, double *, double *, double *, MP_NUMBER );
int mpOblate( MP, double *, double *, double *, double *, MP_NUMBER );
int mpEllipsoid( MP, double *, double *, double *, double *, MP_NUMBER );

int mp_perform_precalculations( MP, MP_NUMBER );
int mp_load_EULER_rotation_matrix( MP, MP_NUMBER );
int mp_perform_EULER_rotation( MP, double *, double * );
int mp_load_cosine_sine_CARTESIAN( MP );
int mp_perform_CARTESIAN_trans( MP, double *, double * );

int triaxcoef_c( double *, double *, double *, double *, double *, 
		int );
int pproj_mp_c( float *, float *, float *, float *, float *, int );
int ztime( int * );

#else

/*

MP Internal function prototypes

	Listed in the order of routines declared in the file
	mp_internals.c.

*/

int degrees2radians();		/* Arithmetic functions */
int radians2degrees();
double check_longitude();
double check_longitude_radians();
int check_latitude();

int determine_map_projection_number();
int determine_model();
int load_coefficients();

int mpGetDesc();		/* used by mpLabelWrite */

int mpSphere();
int mpOblate();
int mpEllipsoid();

int mp_perform_precalculations();
int mp_load_EULER_rotation_matrix();
int mp_perform_EULER_rotation();
int mp_load_cosine_sine_CARTESIAN();
int mp_perform_CARTESIAN_trans();

int triaxcoef_c();
int pproj_mp_c();
int ztime();

#endif

#endif		/* MP_PROTOTYPES_H */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create persp_proj_c.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Fortran routine pproj_mp rewritten in C,
   Thomas Roatsch, DLR 7-Mar-2001 */
 
/*  DATA = standard 40-word geometry buffer (see subroutine CONVEV) for 
        Perspective case, except that word 37 = equatorial semi-minor axis
 
  LINE,SAMP = object space location of a point
 
  LAT,LON = planetary coordinates of point
            (radians, planetocentri  Lat., West Lon.)
 
  MODE:  1 = (LAT,LON) to (LINE,SAMP)   2 = (LINE,SAMP) to (LAT,LON) 
 
  IND = return indicator.  0=success, 1=failure. (to coincide w/mp_routines)
 
   31oct93  lwk  implemented tri-axial ellipsoid model with extra radius
                in word 31 of DATA
   15may94  lwk  fixed BOP test for triaxial ellipsoid using NAIF calls
   jun96  pxa  cleaned up for use with MP routines, changed failure indicator 
                from 0 to 1, 3rd radius from word 31 to 37 in DATA
   jul96  pxa  replaced naif/spice calls to 'surfnm','vsep',vnorm,and 'halfpi'
		 with relevant in-line code, to remove dependency of naif/
		 spice routines in mp_routines 
   29aug01  lwk  fixed bug in replacement of vnorm code */

#include <math.h>
#include "mp_private.h"

#define eps 0.000001

double mymax(double a, double b, double c)
{
double x;
if (a > b) x=a;
else x=b;
if (c > x) x=c;
return x;
}

int pproj_mp_c(float *data, float *line, float *samp,
               float *lat, float *lon, int imode)

{

int    ibop,i,j;
double cp[3],op[3],cpp[3],rs[3],xnorm[3],m;
double u1[3],u2[3],vtemp[3];
double fl,oal,oas,scale,rp,ra,rb,e1,e2;
double *help = (double *)data;
double om[3][3];
double rlat,rlon,slat,slon,clat,clon;
double d1,rb2,ra2,cln2,sln2,slt2,r;
double a1,b1,c1;
double vmax1,vmax2,vmax3,vmax4,norm1,norm2,norm3;
double vnorm1,vnorm2,vnorm3,vnorm4,vmag1,vmag2,vmag3,vsep1;
double vdot1,s;
double x,y,z,x1,y1,z1;
double a,b,c,d;

fl    = data[26];	/* camera focal length (mm) */
oal   = data[27];	/* optical axis line */
oas   = data[28];	/* optical axis sample */
scale = data[29];	/* camera scale (pix/mm) */
rp    = data[24];       /* polar radius */
ra    = data[25];       /* equatorial semi-major radius */
rb    = data[36];
/* check if rb is garbage, in which case assume oblate spheroid: */
if (rb < rp || rb > ra) rb = ra;
e1 = ra/rp;
e1 = e1*e1;
e2 = rb/rp;
e2 = e2*e2;
/*c OM-matrix (Camera-to-Body coordinates) and RS-vector (body center to camera)
c are stored in the first 24 words of the float buffer:*/
om[0][0] = help[0];
om[0][1] = help[1];
om[0][2] = help[2];
om[1][0] = help[3];
om[1][1] = help[4];
om[1][2] = help[5];
om[2][0] = help[6];
om[2][1] = help[7];
om[2][2] = help[8];

rs[0] = help[9];
rs[1] = help[10];
rs[2] = help[11];

if (imode == 1)
   { /*Here to convert (lat,lon) to (line,samp) */
   rlat = (double) (*lat);
   rlon = (double) (*lon);	
   clat = cos(rlat);
   slat = sin(rlat);
   clon = cos(rlon);
   slon = sin(rlon);
   
   /* COMPUTE GEOCENTRIC RADIUS */
   d1 = rp*rp*clat*clat;
   rb2 = rb*rb;
   ra2 = ra*ra;
   cln2 = clon*clon;
   sln2 = slon*slon;
   slt2 = slat*slat;
   r = (ra*rb*rp)/sqrt(d1*rb2*cln2+d1*ra2*sln2+ra2*rb2*slt2);
   cp[0] = r*clat*clon - rs[0];
   cp[1] = -r*clat*slon - rs[1];
   cp[2] = r*slat - rs[2];

/* *******************************************************************
   The following code replaces the functionality of 'surfnm','vsep',and
   halfpi() routines. This needed to be done to remove spice/naif
   dependencies from mp_routines
   ******************************************************************/

   op[0] = r*clat*clon; 
   op[1] = -r*clat*slon; 
   op[2] = r*slat;

   /* M=MIN(RA,RB,RP) */
   if (ra < rb) m = ra;
   else m = rb;
   if (m > rp) m = rp;
   a1=m/ra;
   b1=m/rb;
   c1=m/rp;

   xnorm[0] = op[0]*(a1*a1);
   xnorm[1] = op[1]*(b1*b1);
   xnorm[2] = op[2]*(c1*c1);

   vmax1 = mymax(fabs(xnorm[0]),fabs(xnorm[1]),fabs(xnorm[2]));

   if (vmax1 == 0) vnorm1 = 0;
   else 
      {
      norm1 = xnorm[0] / vmax1;
      norm2 = xnorm[1] / vmax1;
      norm3 = xnorm[2] / vmax1;
      vnorm1 = vmax1*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
      }
   vmag1 = vnorm1;
   if (vmag1 > 0)
      {
      xnorm[0] = xnorm[0]/vmag1;
      xnorm[1] = xnorm[1]/vmag1;
      xnorm[2] = xnorm[2]/vmag1;
      }
   else
      {
      xnorm[0] = 0;
      xnorm[1] = 0;
      xnorm[2] = 0;
      }
   ibop = 0;

/* Now compute vsep(cp,xnorm), and assign it a value */
   vmax2 = mymax(fabs(cp[0]),fabs(cp[1]),fabs(cp[2]));
  if (vmax2 == 0) vnorm2 = 0;
  else
     {
     norm1 = cp[0] / vmax2;
     norm2 = cp[1] / vmax2;
     norm3 = cp[2] / vmax2;
     vnorm2 = vmax2*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
     }
   vmag2 = vnorm2;
   if (vmag2 > 0)
      {
      u1[0] = cp[0]/vmag2;
      u1[1] = cp[1]/vmag2;
      u1[2] = cp[2]/vmag2;
      }
   else
      {
      u1[0] = 0;
      u1[1] = 0;
      u1[2] = 0;
      }
   if (vmag2 == 0) vsep1 = 0;

   vmax3 = mymax(fabs(xnorm[0]),fabs(xnorm[1]),fabs(xnorm[2]));
   if (vmax3 == 0) vnorm3 = 0;
   else
      {
      norm1 = xnorm[0] / vmax3;
      norm2 = xnorm[1] / vmax3;
      norm3 = xnorm[2] / vmax3;
      vnorm3 = vmax3*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
      }
   vmag3 = vnorm3;
   if (vmag3 > 0)
      {
      u2[0] = xnorm[0]/vmag3;
      u2[1] = xnorm[1]/vmag3;
      u2[2] = xnorm[2]/vmag3;
      } 
      else
         {
	 u2[0] = 0;
	 u2[1] = 0;
	 u2[2] = 0;
         }
   if (vmag3 == 0) vsep1 = 0;
   vdot1 = u1[0]*u2[0] + u1[1]*u2[1] + u1[2]*u2[2];
   if (vdot1 == 0.0) vsep1 = PI_OVER_2;
   else 
      {
      if (vdot1 > 0.0) {
        vtemp[0] = u1[0] - u2[0];
        vtemp[1] = u1[1] - u2[1];
        vtemp[2] = u1[2] - u2[2];
      }
      else {
        vtemp[0] = u1[0] + u2[0];
        vtemp[1] = u1[1] + u2[1];
        vtemp[2] = u1[2] + u2[2];
      }
      vmax4 = mymax(fabs(vtemp[0]),fabs(vtemp[1]),fabs(vtemp[2]));
      if (vmax4 == 0) vnorm4 = 0;
      else
         {
         norm1 = vtemp[0] / vmax4;
         norm2 = vtemp[1] / vmax4;
         norm3 = vtemp[2] / vmax4;
	 vnorm4 = vmax4*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
         }
      }
   if (vdot1 > 0.0) vsep1 = 2.0 * sin(0.5 * vnorm4);
   else vsep1 = PI - 2.0*sin(0.5*vnorm4);
   if (vsep1 < PI_OVER_2) ibop=1; 

   for (i=0; i<3; i++)
      {
      d1 = 0;
      for (j=0; j<3; j++)
         d1 = d1 + om[j][i]*cp[j];
      cpp[i] = d1;
      }
   s = fl*scale/cpp[2];
   *line = (float) (oal + s*cpp[1]);
   *samp = (float) (oas + s*cpp[0]);
   return ibop;

   }


/* ....Here to convert (line,samp) to lat,lon) */
x = (double) (*samp) - oas;
y = (double) (*line) - oal;
z = fl*scale;

for (i=0; i<3; i++)
   cp[i] = om[i][0]*x + om[i][1]*y + om[i][2]*z;

a = e2*cp[0]*cp[0] + e1*cp[1]*cp[1] + e1*e2*cp[2]*cp[2];
b = e2*cp[0]*rs[0] + e1*cp[1]*rs[1] + e1*e2*cp[2]*rs[2];
c = e2*rs[0]*rs[0] + e1*rs[1]*rs[1] + e1*e2*rs[2]*rs[2] - e1*e2*rp*rp;
d = b*b - a*c;
if (d < 0) return 1;

s = (-b-sqrt(d))/a;
for (i=0; i<3; i++)
   op[i] = s*cp[i] + rs[i];

x = op[0];
y = op[1];
z = op[2];
x1 = fabs(x);
y1 = fabs(y);
z1 = fabs(z);
d = sqrt(x*x+y*y);
if(d >= z1*eps) 
   {
   *lat = atan(z/d);         /* geocentric lat.*/
   if(y1 < x1*eps) 
      {
      *lon = 0;
      if(x < 0.) *lon=(float)PI;
      return 0;
      }

   if(x1 < y1*eps)
      {
      *lon = RETURN_RADIANS(270.0);
      if(y < 0) *lon= (float)PI_OVER_2;
      return 0;
      }

   *lon = (float) (TWO_PI - atan2(y,x)) ;
   if(*lon > TWO_PI) *lon=*lon - (float) TWO_PI;
   return 0;
   }

*lat = (float) PI_OVER_2;
if(z < 0) *lat = - (*lat);
*lon = 0;
return 0;
      
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ortho_obl_c.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Fortran routine ortho_obl rewritten in C,
   Thomas Roatsch, DLR 7-Mar-2001 

 CONVERT L,S TO LAT LONG OR LAT,LONG TO L,S FOR THE ORTHOGRAPHIC
 PROJECTION FOR AN OBLATE SPHEROID

 11SEP96 -LWK-  CODE ADAPTED FROM SUBR. TRANV, FOR USE BY MP_ROUTINES
 23Oct97 -Scholten- added check for THR near 90 deg. 
  return  0=O.K.  1=POINT OFF PLANET
  M    1=DIRECT  2=INVERSE

   
DATA
 1    X SPECIAL SAMPLE POINT
 2    Z SPECIAL LINE POINT
 3    TH  SPECIAL LATITUDE
 4    TH1  LATITUDE OF SPECIAL PARALLEL OR SPECIAL OBLIQUE LONGITUDE
 5    TH2  LATITUDE OF SPECIAL PARALLEL
 6    LAM SPECIAL LONGITUDE    WEST
 7    F  SCALE  (KM/PIXEL)
 8    CAS  +1 IF VISIBLE POLE IS N.   -1 IF VISIBLE POLE IS S.
      M  M=2  LINE,SAMPLE TO LAT,LONG   (INVERSE)
      M  M=1  LAT,LONG TO LINE,SAMP  (DIRECT)
 25   RP  POLAR RADIUS  (KM)
 26   RE  EQUATORIAL RADIUS  (KM)
 9    PSI   NORTH ANGLE
 ******  ANGLES IN DATA() IN DEGREES  ******
 ******  LAT,LONG IN RADIANS          ******
 ******  ALL LATITUDES PLANETOCENTRI******
 ******  ALL LONGITUDES WEST          ****** */

#include <math.h>
#include "mp_private.h"

#define SMALL 1e-8

/* special functions */

/* GEOCENTRIC RADIUS */
double gcr (double rpp, double rep, double thr)
{
double help1,help2,help3;

help1 = rpp*cos(thr);
help2 = rep*sin(thr);
help3 = rpp * rep/sqrt(help1*help1 + help2*help2);

return help3;
}

/* GEODETIC LATITUDE */
double phig (double rpp, double rep, double thr)
{
double help;

help = PI_OVER_2 - fabs(atan(-rpp*rpp/(rep*rep)*1.0/tan(thr)));

return help;
}

/* dsign from FORTRAN */
double orthodsign (double a, double b)
{
double dhelp;

if (b < 0) dhelp = - fabs(a);
else       dhelp =    fabs(a);

return dhelp;
}

/* dmod from FORTRAN */
double orthodmod (double a, double b)
{
double dhelp;

dhelp = a - ((int)(a/b)) * b;

return dhelp;
}

int ortho_obl_c(int m, float *data, float *line,
                 float *sample, float *lat, float *longi)
                 
{

double lam,lamr,latr,longr;
double k1,k2,k3,k3sqrt,dif1[3],dif2[3],lambar,north,l;
double lat8,long8;
double xc,zc,th,thr,thr0,f,psi,psir,rp,rpp,re,rep;
double r,phi,x11,z11,x1,y1,z1,x2,y2,z2;
double c1,c2,c3,c4,ca,ce,co,sa,so;
double rlat,rlon,cphi,cpsi;
double sinlat,coslat,sinlon,coslon,sinnor,cosnor,fl;
double req,slccpc,slcspc,scpcsl,scpccl,clcc2p,clcs2p,slcc2p;
double rpsq,resq,sinlam,coslam,sinl,cosl;
double rcp,delx,xdel,delz,zdel,apoiup;
double adel,sindel,cosdel;
double dd,a1,b1,d1,a2,b2;
double alpha,beta,gamma,delta;
double alphsq,betasq,gammsq,deltsq,d1sq,c2sq,b2sq,gresq,drpsq;
double pcpx,pcpy,pcpz,rad1,rad2;

/*  CONVERT ANGLES AND DIMENSIONS TO RADIANS AND PIXELS RESPECTIVELY
    AND float DATA ITEMS TO double */

xc  = (double) data[0];
zc  = (double) data[1];
th  = (double) data[2];
thr = RETURN_RADIANS(th);
if(thr == 0) thr=SMALL;		/* in case center_lat=0 */ 
thr0 = thr;
lam  = (double) data[5];
lamr = RETURN_RADIANS(lam);
f    = (double) data[6];
psi  = (double) data[8];
psir = RETURN_RADIANS(psi);
rp   = (double) data[24];
rpp  = rp / f;
re   = (double) data[25];
rep  = re / f;

if (m == 1 )
   { /* DIRECT */
   lat8  = (double) *lat;
   long8 = (double) *longi;
   latr  = lat8;
   longr = long8;
   r   = gcr(rpp,rep,latr);
   phi = phig(rpp,rep,thr);
   phi = orthodsign(phi,thr);
   x11 = -r*cos(latr)*sin(longr-lamr);
   z11 = r*(sin(phi)*cos(latr)*cos(longr-lamr)-cos(phi)*sin(latr));
   x1  = x11;
   z1  = z11-gcr(rpp,rep,thr)*sin(phi-thr);
   *sample = x1*cos(psir)-z1*sin(psir)+xc;
   *line   = x1*sin(psir)+z1*cos(psir)+zc;

/* BACK-OF-PLANET TEST */
   c1 = cos(thr);
   c2 = cos(TWO_PI-lamr);
   c3 = sin(thr);
   c4 = sin(TWO_PI-lamr);
   ca = cos(latr);
   co = cos(TWO_PI-longr);
   sa = sin(latr);
   so = sin(TWO_PI-longr);
   ce = ca*co*c1*c2+ca*so*c1*c4+sa*c3;	/* COSINE EMISSION ANGLE */
/*  RETURNS .TRUE. IF POINT LAT,LON IS ON BACK OF PLANET W.R.T. TH,LAM */
   if(ce < 0) return 1;
   else return 0;
   }

/*  INVERSE */
rlat = *sample-xc;
rlon = *line-zc;
if( (rlat == 0) && (rlon ==0) )
   {
   *lat   = thr;
   *longi = lamr;
   return 0;
   }

cphi  = th;
cpsi  = lam;
north = psi;
if (fabs(thr) > RETURN_RADIANS(90.0-SMALL))
    thr = orthodsign(RETURN_RADIANS(90.0-SMALL),thr);
sinlat=sin(thr);
coslat=cos(thr);
sinlon=sin(lamr);
coslon=cos(lamr);
sinnor=sin(psir);
cosnor=cos(psir);
fl=rp;
req=re;
slccpc=sinlat*coslon;
slcspc=sinlat*sinlon;
scpcsl=sinlon*coslon*sinlat;
scpccl=sinlon*coslon*coslat;
clcc2p=coslat*coslon*coslon;
clcs2p=coslat*sinlon*sinlon;
slcc2p=sinlat*coslon*coslon;

/* CALC ANGLE LAMBDA BAR */
rpsq=fl;
rpsq=rpsq*rpsq;
resq=req;
resq=resq*resq;
lambar=((coslat*coslat/resq+sinlat*sinlat/rpsq)/
        sqrt((coslat*coslat/(resq*resq)+sinlat*sinlat/(rpsq*rpsq))));
if(lambar > 1) lambar=1;
lambar=acos(lambar);
lambar=RETURN_RADIANS(cphi)+lambar;
sinlam=sin(lambar);
coslam=cos(lambar);
l=RETURN_RADIANS(cphi)-lambar;
sinl=sin(l);
cosl=cos(l);

/* GET RADIUS OF PLANET AT C.P. */
rcp= gcr(rpp,rep,thr);

/* CONVERT FROM PIXELS TO KM */
rcp = f*rcp;

/* CALC.ANGLE BETWEEN UP AND POINT OF INTEREST
   IN PLANE OF PROJECTION SUBTENDED AT CENTER OF PROJECTION */
delx=rlat;
xdel=delx;
delz=rlon;
zdel=delz;
apoiup=atan2(-xdel,-zdel);

/* CALC.SIN AND COS OF THE ANGLE BETWEEN THE DIRECTION OF
   NORTH IN THE IMAGE PLANE AND THE POINT OF INTEREST SUBTENDED AT
   THE CENTER OF PROJECTION */
adel=RETURN_RADIANS(north) + apoiup;
sindel=sin(adel);
cosdel=cos(adel);
if(sindel ==  1) cosdel=0.0;
if(sindel == -1) cosdel=0.0;

/* CALC.DISTANCE OF POINT OF INTEREST FROM
   CENTER OF PROJECTION IN PLANE OF PROJECTION
   AT TRUE SCALE */
dd=f * sqrt( (xdel*xdel) + (zdel*zdel) );

/* CHECK WHETHER POINT OF INTEREST IS OFF PLANET */
if(req < dd) return 1;

/* CALC.COEFFIEIENTS FOR TWO PLANES NORMAL
   TO PLANE OF PROJECTION.

   PLANE 1 - NORMAL TO LINE CONNECTION CENTER OF PROJECTION
   AND POINT OF INTEREST
   PLANE 2 - CONTAINS LINE CONNECTION CENTER OF
   PROJECTION AND POINT OF INTEREST

   PLANE 1 A1*X+B1*Y+C1*Z+D1=0
   PLANE 2 A2*X+B2*Y+C2*Z=0 */

a1=-sindel*sinlon-cosdel*coslon*sinlam;
b1=-sindel*coslon+cosdel*sinlon*sinlam;
c1=cosdel*coslam;
d1=-dd*sindel*sindel+rcp*cosdel*sinlam*coslat
   -rcp*sinlat*coslam*cosdel-dd*cosdel*cosdel*slcc2p*sinlam
   -dd*cosdel*cosdel*coslam*coslam
   -dd*sinlam*sinlam*cosdel*cosdel*sinlon*sinlon;
a2=-cosdel*sinlon*cosl+sindel*slccpc;
b2=-cosdel*coslon*cosl-sindel*slcspc;
c2=-coslat*sindel;

/* CALCULATE PARAMETRIC VARIABLES IN
   SIMULTANEOUS SOLN.OF PLANE 1,PLANE 2,AND SPHEROID */

alpha=a2*c1-a1*c2;
beta=a2*b1-a1*b2;
gamma=b1*c2-b2*c1;
delta=c1*b2-b1*c2;

/* CALCULATE X COORDINATE

   EQUATION IS X=K1+OR-K2*SQRT(K3) */

alphsq=alpha*alpha;
betasq=beta*beta;
gammsq=gamma*gamma;
deltsq=delta*delta;
d1sq=d1*d1;
c2sq=c2*c2;
b2sq=b2*b2;
gresq=gammsq*resq;
drpsq=deltsq*rpsq;
z1=drpsq*(alphsq+gammsq)+betasq*gresq;
k1=((alpha*c2*d1*drpsq)+(beta*b2*d1*gresq))/z1;
k2=(gamma*delta*fl)/z1;
k3=2.0*alpha*c2*beta*b2*resq;
k3=k3+(-c2sq*drpsq-b2sq*gresq-alphsq*b2sq*resq-betasq*resq*c2sq);
k3=k3*d1sq;
k3=k3+(gresq*drpsq+drpsq*resq*alphsq+resq*betasq*gresq);
if(k3 < 0) return 1;
k3sqrt=sqrt(k3);
z1=k2*k3sqrt;
x1=k1+z1;
x2=k1-z1;

/* MAKE THE BACK OF PLANET TEST */

y1=-d1*c2;
y2=y1;
y1=(y1+alpha*x1)/gamma;
y2=(y2+alpha*x2)/gamma;
z1=(-b2*d1+beta*x1)/delta;
z2=(-b2*d1+beta*x2)/delta;

/* (X1,Y1,Z1) IS VECTOR P01
   (X2,Y2,Z2) IS VECTOR P02
   PCP IS VECTOR FROM PLANET CENTER TO CENTER OF PROJECTION
   FIND WHICH VECTOR HAS MINIMUM LENGTH, P01-PCP  OR  P02-PCP */

pcpx=rcp*coslat*coslon;
pcpy=-rcp*coslat*sinlon;
pcpz=rcp*sinlat;
dif1[0]=x1-pcpx;
dif1[1]=y1-pcpy;
dif1[2]=z1-pcpz;
dif2[0]=x2-pcpx;
dif2[1]=y2-pcpy;
dif2[2]=z2-pcpz;
rad1=dif1[0]*dif1[0]+dif1[1]*dif1[1]+dif1[2]*dif1[2];
rad2=dif2[0]*dif2[0]+dif2[1]*dif2[1]+dif2[2]*dif2[2];
if(rad1 <= rad2)
   {
   /* POINT 1 IS VALID */
   rlon=TWO_PI-atan2(y1,x1);
   rlon=orthodmod(rlon+TWO_PI,TWO_PI);
   rlat=(atan(fabs(z1)/sqrt(x1*x1+y1*y1)));
   rlat=orthodsign(rlat,z1);
   *lat=rlat;
   *longi=rlon;
   return 0;
   }
/* POINT 2 IS VALID */
rlon=TWO_PI-atan2(y2,x2);
rlon=orthodmod(rlon+TWO_PI,TWO_PI);
rlat=(atan(fabs(z2)/sqrt(x2*x2+y2*y2)));
rlat=orthodsign(rlat,z2);
*lat=rlat;
*longi=rlon;
return 0;

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mp_triaxcoef.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* rewritten in C
   Thomas Roatsch, DLR, March 2001
   removed nlimit, mlimit,klimit from parameter list,
   it was already defined in mp_routines.h */
   
/*  subroutine triaxcoef

  Function extracted from program TRICOEF.COM by Jean Lorre.

	Date extracted:		October 1993
	Extracted by:		Justin McNeill

  This has been modified to now include both authali  as well as conformal
  auxiallary coefficient functionality. Code was adopted from Jean Lorre's
  tricoef routine. Recent changes also made in the randum number generator
  function, to 'rangen', to correct for errors occurring on the sgi system
  with the older 'ran1' function   -  pxa  august '96

  oct96 -lwk- hard-coded the random-number seed in order to allow comparison
		of test logs;  this will be replaced in the future by 
		allowing the user to specify this (as well as the 3 limits),
		which will require a new routine to give the user access

  jun98 -lwk- replaced rangen with modified version of RAN1 because of 
		problems on DEC-Unix  */

#include <math.h>
#include "zvproto.h"
#include "mp_private.h"

#define maxpts 800
#define maxeqs 400
#define latpts 72
#define lonpts 72

/* was Fortran common block */
static double a,b,c;
static double lat[maxpts],lon[maxpts];
static double costheta[maxpts],sintheta[maxpts],f[maxpts];

/* ran1 stuff */
#define randim 97
static int    ix1,ix2,ix3;
static double r[randim];

/*********************************************************************/
double ran1( int *idum)
{
/* Returns random number between 0.0 and 1.0. To initialize provide
   negative idum value.*/
#define m1  259200
#define ia1 7141
#define ic1 54773
#define m2  134456
#define ia2 8121
#define ic2 28411
#define m3  243000
#define ia3 4561
#define ic3 51349

int j;
double help;
double rm1,rm2;
rm1 = 1.0/m1;
rm2= 1.0/m2;

if (*idum != 1)
   {
   ix1 = (ic1 - *idum) % m1;
   ix1 = (ia1 * ix1 + ic1) % m1;
   ix2 = ix1 % m2;
   ix1 = (ia1 * ix1 + ic1) % m1;
   ix3 = ix1 % m3;
   for (j=0; j<randim; j++)
      {
      ix1 = (ia1 * ix1 + ic1) % m1;
      ix2 = (ia2 * ix2 + ic2) % m2;
      r[j] = ( (double) ix1 + ( (double) ix2) * rm2 ) * rm1;
      }
   *idum=1;
   }
ix1 = ( ia1 * ix1 + ic1) % m1;
ix2 = ( ia2 * ix2 + ic2) % m2;
ix3 = ( ia3 * ix3 + ic3) % m3;
j   = 1 + (randim * ix3)/m3;
if(j > randim) j=randim;
if(j < 1) j=1;
help = r[j-1];
r[j-1] = ( (double) ix1 + ( (double) ix2) * rm2 ) * rm1;

return help;
}
/*********************************************************************/
double p1(int m, int n, int i)      /* equation 76 */
{
double help;
help = 2.0*a*m * sintheta[i] * cos(n*lat[i]) * cos(2.0*m*lon[i]);
return help;
}

double p2(int m, int n, int i)      /* equation 77 */
{
double help;
help = -2.0*a*m * costheta[i] * sin(n*lat[i])* sin(2.0*m*lon[i]) -
       a*n*f[i] * cos(n*lat[i]) * cos(2.0*m*lon[i]);
return help;
}

double p3(int m, int n, int i)       /* equation 78 */
{
double help;
help = -2.0*a*m * cos(n*lat[i]) * cos(2.0*m*lon[i]) -
       a*n*f[i] * sin(n*lat[i]) *sin(2.0*m*lon[i]) * costheta[i];
return help;
}

double p4(int m, int n, int i)       /* equation 79 */
{
double help;
help = a*n*f[i] * cos(n*lat[i]) * cos(2.0*m*lon[i])* sintheta[i];
return help;
}
/*********************************************************************/
int mp_dsimq(double *am, double *bm, int n)

/*        PURPOSE
          OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
          AX=B

       DESCRIPTION OF PARAMETERS
          AM - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
               N BY N.
          BM  - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
              REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
          N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.

          return:
               0 FOR A NORMAL SOLUTION
               1 FOR A SINGULAR SET OF EQUATIONS */
{
double biga,save;
int    ks;
int    j,jj,jy,i,ij,it,imax,i1,i2,k;
int    iqs,ix,ixj,jx,ixjx,jjx;
int    ia,ib,ic;

/* FORWARD SOLUTION */
ks=0;
jj=-n;

for (j=0; j<n; j++)
   {
   jy=j+1;
   jj=jj+n+1;
   biga=0.0;
   it=jj-j;
   
   for (i=j-1; i<n-1; i++)
      {
      /* SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN */
      ij=it+i;
      if (fabs(am[ij]) > fabs(biga))
         {
         biga = am[ij];
         imax=i+1;
         }
      }
/* TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX) */
   if(biga == 0) return 1;

/* INTERCHANGE ROWS IF NECESSARY */
   i1=j+n*(j-1);
   it=imax-j;
   for (k=j-1; k<n-1; k++)
      {
      i1=i1+n;
      i2=i1+it;
      save=am[i1];
      am[i1]=am[i2];
      am[i2]=save;
      /* DIVIDE EQUATION BY LEADING COEFFICIENT */
      am[i1]=am[i1]/biga;
      }
   save=bm[imax];
   bm[imax]=bm[j];
   bm[j]=save/biga;

/* ELIMINATE NEXT VARIABLE */
   if (j == n-1) goto go_70; 
   iqs=n*j;
   for (ix=jy; ix<n; ix++)
      {
      ixj=iqs+ix;
      it=j-ix;
      for (jx=jy; jx<n; jx++)
         {
         ixjx=n*jx+ix;
         jjx=ixjx+it;
         am[ixjx]=am[ixjx] - am[ixj] * am[jjx];
         }
      bm[ix] = bm[ix] - bm[j] * am[ixj];
      }
   }

/* BACK SOLUTION */
      
go_70:
it=n*n-1;
for (j=0; j<n-1; j++)
   {
   ia=it-j-1;
   ib=n-j-2;
   ic=n-1;
   for (k=0; k<=j; k++)
      {
      bm[ib] = bm[ib] - am[ia] * bm[ic];
      ia=ia-n;
      ic=ic-1;
      }
   }

return 0;
}
/*********************************************************************/
double simp1(double areafun[lonpts+1][latpts+1],
           int i, int lonlmt)
 
/* Compute elements for Simpsons integration. */
{
double sum1;
int j;

sum1=areafun[0][i]+areafun[lonlmt][i];
for (j=1; j<=lonlmt-1;j=j+2)
   sum1=sum1+4.0*areafun[j][i];
for (j=2; j<=lonlmt-2; j=j+2)
   sum1=sum1+2.0*areafun[j][i];
sum1=sum1*PI_OVER_2/(lonpts*3);
return sum1;
}
/*********************************************************************/
double simp2(int latlmt, double c, double elllat[lonpts+1],
             double areafun[lonpts+1][latpts+1])
 
/* Computes areas of zones */
{
double fsimlon, sum2;
int i;

i=0;
sum2 = simp1(areafun,i,lonpts);
for (i=1; i <= latlmt-1; i=i+2)
   {
   fsimlon = simp1(areafun,i,lonpts);
   sum2 = sum2 + 4.0*fsimlon;
   }
for (i=2; i<=latlmt-2; i=i+2)
   {
   fsimlon = simp1(areafun,i,lonpts);
   sum2=sum2+2.0*fsimlon;
   }
i=latlmt;
fsimlon = simp1(areafun,i,lonpts);
sum2=sum2+fsimlon;
sum2=c*c*sum2*elllat[latlmt]/(latlmt*3);
return sum2;
}
/*********************************************************************/
void authalic_coef(double coef[KLIMIT+1][MLIMIT+1],
                   double coefp[NLIMIT])
 
/* To compute coefficients permitting the snyder to authalic computation */
{ 
double elllat[latpts+1],elllon[lonpts+1];
double areafun [lonpts+1][latpts+1];
double acoef[MLIMIT+1][latpts+1];
double autlat[latpts+1];
double autlon[lonpts+1][latpts+1];
double cb2,ba2;
int    i,j,k,m,n;
double clon,clat,slon,slat,flon,flat;
double t1,t2,t3,sinthet,costhet;
double area,tarea,sphiaut,sum1,autfunc,tcirc,fsimlon;
int    latlmt,lonlmt;

for (i=0; i<=latpts; i++)
   for (j=0; j<=MLIMIT; j++)
      acoef[j][i]=0;
for (i=0;i<=latpts; i++)
   for (j=0; j<=lonpts; j++)
      autlon[j][i]=0;
for (i=0; i<=MLIMIT; i++)
   for (j=0; j<=KLIMIT; j++)
      coef[j][i]=0;
for (i=0; i<=latpts; i++)
   autlat[i]=0;
for (i=0; i<NLIMIT; i++)
   coefp[i]=0;

ba2=b*b;
cb2=c*c/ba2;
 
for (i=0; i<latpts; i++)
   elllat[i]=i*PI_OVER_2/latpts;
elllat[latpts]=(latpts-0.001)*PI_OVER_2/latpts;
for (j=0; j<=lonpts; j++)
   elllon[j]=j*PI_OVER_2/lonpts;

/* compute area function for integration */
for (i=0; i<=latpts; i++)
   for (j=0; j<=lonpts; j++)
      {
      slon=sin(elllon[j]);
      clon=cos(elllon[j]);
      slat=sin(elllat[i]);
      clat=cos(elllat[i]);
      flon=slon*slon+ba2*clon*clon;       /* eqn 9 */
      flat=slat*slat+cb2*clat*clat;       /* eqn 10 */
      t1=(1.0-ba2)*slon*clon*slat;
      t2=sqrt(slat*slat+cb2*cb2*flon*clat*clat);
      t3=sqrt(slon*slon+ba2*ba2*clon*clon);
      costhet=t1/(t2*t3);                /*eqn 48 */
      sinthet=sqrt(fabs(1.0-costhet*costhet));
      areafun[j][i]=clat*t3*t2*sinthet/(flon*flon*flat*flat);
      }
/* compute total area of triaxial ellipsoid */
tarea = simp2(latpts,c, elllat, areafun);

/* compute authalic latitudes */
latlmt=0;
autlat[0]=0.0;
for (latlmt=6; latlmt<=latpts; latlmt=latlmt+6)
   {
   area = simp2(latlmt,c, elllat, areafun);
   sphiaut=area/tarea;
   if(fabs(sphiaut) > 0.9999999) autlat[latlmt]=PI_OVER_2;
   else
      autlat[latlmt]=
         atan(sphiaut/sqrt(fabs(1.0-sphiaut*sphiaut)));
   }
    
/* compute coefficients for authalic latitude using simpson integration */
for (n=1; n<=NLIMIT; n++)
   {
   sum1=0.0;
   for (i=6; i<=latpts-6; i=i+12)
      {
      autfunc=(autlat[i]-elllat[i])*sin(2*n*elllat[i]);
      sum1=sum1+4.0*autfunc;
      }
   for (i=12; i<=latpts-12; i=i+12)
      {
      autfunc=(autlat[i]-elllat[i])*sin(2*n*elllat[i]);
      sum1=sum1+2.0*autfunc;
      }
   coefp[n-1]=4.0*sum1/latpts;
   }

/* compute authalic longitude */
for (i=0; i<=latpts; i=i+6)
   {
   /* first compute area of zone of quadrant along latitude */
   tcirc = simp1(areafun,i,lonpts);
   /* now compute portion of zone for each step of longitude */
   autlon[0][i]=0.0;
   for (lonlmt=6; lonlmt<=lonpts; lonlmt=lonlmt+6)
      {
      fsimlon = simp1(areafun,i,lonlmt);
      autlon[lonlmt][i]=(fsimlon/tcirc)*PI_OVER_2;
      }
   }   
 
/* compute elements for simpson integration for acoef of authalic longitude */
for (m=0; m<=MLIMIT; m++)
   for (i=0; i<=latpts; i=i+6)
      {
      sum1=0.0;
      for (j=6; j<=lonpts-6; j=j+12)
         {
         autfunc=(autlon[j][i]-elllon[j])*sin(2*m*elllon[j]);
         sum1=sum1+4.0*autfunc;
         }
      for (j=12; j<=lonpts-12; j=j+12)
         {
         autfunc=(autlon[j][i]-elllon[j])*sin(2*m*elllon[j]);
         sum1=sum1+2.0*autfunc;
         }
      acoef[m][i]=4.0*sum1/lonpts;
      }
 
/* compute elements for simpson integration for C coef of authalic longitude */
for (m=0; m<=MLIMIT; m++)
   {
   k=0;
   sum1=acoef[m][0];
   for (j=6; j<=latpts-6; j=j+12) sum1=sum1+4.0*acoef[m][j];
   for (j=12; j<=latpts-12; j=j+12) sum1=sum1+2.0*acoef[m][j];
   sum1=sum1+acoef[m][latpts];
   coef[0][m]=2.0*sum1/latpts;
   }
 
for (m=1; m<=MLIMIT; m++)
   for (k=1; k<=KLIMIT; k++)
      {
      sum1=acoef[m][0];
      for (j=6; j<=latpts-6; j=j+12)
         {
         autfunc=acoef[m][j]*cos(2*k*elllat[j]);
         sum1=sum1+4.0*autfunc;
         }
      for (j=12; j<=latpts-12; j=j+12)
         {
         autfunc=acoef[m][j]*cos(2*k*elllat[j]);
         sum1=sum1+2.0*autfunc;
         }
      sum1=sum1+acoef[m][latpts]*cos(2*k*elllat[latpts]);
      coef[k][m]=4.0*sum1/latpts;
      } 
return;
}
/*********************************************************************/
void comp_points(int npoints, int *idum)
{
double degtorad,rannum;
int    i;
double slon,clon,slat,clat,flon,flat,t1,t2,t3;

degtorad=atan(1.0)/45.0;
for (i=0; i< npoints; i++)
    {
    rannum = ran1(idum);
    lat[i]=degtorad*85.*rannum;
    rannum = ran1(idum);
    lon[i]=degtorad*90.*rannum;
    }
    
/* compute & save values unique to each point.
   removed division by a since it is 1 */
for (i=0; i< npoints; i++)
   {
   slon = sin(lon[i]);
   clon = cos(lon[i]);
   slat = sin(lat[i]);
   clat = cos(lat[i]);
   flon = slon*slon + b*b * clon*clon;    /* eqn 9 */
   flat = slat*slat + c*c/b/b * clat*clat;    /* eqn 10 */
   t1   = (1.0-b*b) *slon * clon * slat;
   t2   = slat*slat + pow(c,4.0)/pow(b,4.0) * flon * clat*clat;
   t3   = slon*slon + pow(b,4.0) * clon*clon;
   costheta[i] = t1 / sqrt(t2) / sqrt(t3);  /* eqn 48 */
   sintheta[i] = sin(acos(costheta[i]));
   f[i] = cos(lat[i]) * sqrt(t3) * flat / (flon*sqrt(t2)); /* eqn 50 */
   }

}
/*********************************************************************/
 


int mp_triaxcoef( double radii[3], double cc[MLIMIT][NLIMIT],
                  double cp[MLIMIT][NLIMIT], double ac[KLIMIT+1][MLIMIT+1],
                  double ap[NLIMIT])

{

int    status;
int    i,ii,ip,iq,j,m,n,k;
double amatrix[maxeqs*maxeqs],bvector[maxeqs];

int    nequations,npoints,idum;
double t;

a=radii[0];
b=radii[1];
c=radii[2];
if ( a == 0)
   {
   zvmessage("mp_triaxcoef: radius[0] is zero","");
   return -401;
   } 

/* disable this check  -lwk- */
/*
if((c >=b) || (b >=a ))
   {
   zvmessage("mp_triaxcoef: Radii out of order, should be decreasing","");
   zvmessage("Oblate spheroids not supported","");
   return -402;
   }
*/
c=c/a;
b=b/a;
a=1.0;

nequations=NLIMIT*MLIMIT*2;
if(nequations > maxeqs)
   {
   zvmessage("mp_triaxcoef: Too many equations requested","");
   return -403;
   }

npoints=nequations*2;          /* npoints > nequations/2 (Snyder) */
if(npoints > maxpts) npoints=maxpts;

/* compute the fit points from a random grid in the range lat=0 to 85
   long=0 to 90. */
idum=-768825576;		/* establish a seed for random numbers */
comp_points(npoints, &idum);
   
/* load matrices to do the linear fit. eqn 80 */
ip=1;
iq=0;
for (i=0; i<MLIMIT*NLIMIT; i++)       /* first 1/2 of equations loop */
   {
   j=i;
   m=1;
   n=0;
   for (k=0; k<MLIMIT*NLIMIT; k++)    /* first 1/2 of terms in equation */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
         t=t  +p1(m,n,ii) * p1(ip,iq,ii) + p3(m,n,ii) * p3(ip,iq,ii);
      if(n == NLIMIT-1)
         {
         n=0;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   m=0;
   n=1;
   for (k=MLIMIT*NLIMIT; k<nequations; k++)  /* second 1/2 of terms */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
         t=t + p2(m,n,ii) * p1(ip,iq,ii) + p4(m,n,ii) * p3(ip,iq,ii);
      if(n == NLIMIT)
         {
         n=1;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   t=0.0;
   for (ii=0; ii<npoints; ii++)
      t=t + (f[ii]/cos(lat[ii]) - sintheta[ii]) * p1(ip,iq,ii) +
        (1.0-f[ii]*sintheta[ii]/cos(lat[ii])) * p3(ip,iq,ii);

    bvector[i]=t;
    if(iq == NLIMIT-1)
       {
       iq=0;
       ip=ip+1;
       }
       else iq=iq+1;

   } /* end of  first 1/2 of equations loop */

ip=0;
iq=1;
for (i=MLIMIT*NLIMIT; i<nequations; i++)  /* second 1/2 of equations loop */
   {
   j=i;
   m=1;
   n=0;
   for (k=0; k<MLIMIT*NLIMIT; k++)    /* first 1/2 of terms in equation */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
         t=t + p1(m,n,ii) * p2(ip,iq,ii) + p3(m,n,ii) * p4(ip,iq,ii);
      if(n == NLIMIT-1)
         {
         n=0;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   m=0;
   n=1;
   for (k=MLIMIT*NLIMIT; k<nequations; k++)  /* second 1/2 of terms */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
          t=t + p2(m,n,ii) * p2(ip,iq,ii) + p4(m,n,ii) * p4(ip,iq,ii);
      if(n == NLIMIT)
         {
         n=1;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   t=0.0;
   for (ii=0; ii<npoints; ii++)
      t=t + (f[ii]/cos(lat[ii]) - sintheta[ii]) * p2(ip,iq,ii) +
        (1.0-f[ii]*sintheta[ii]/cos(lat[ii])) * p4(ip,iq,ii);
    bvector[i]=t;
    if(iq == NLIMIT)
       {
       iq=1;
       ip=ip+1;
       }
       else iq=iq+1;

   } /* end of  second 1/2 of equations loop */



/* perform the solution.*/
status = mp_dsimq(amatrix,bvector,nequations);
if (status != 0) return status;

/* Recompute NPOINTS random points for RMS error computation.
   The same points won't do since we've fitted to them. */
comp_points(npoints, &idum);

k=0;
m=0;
for (i=0;i<NLIMIT; i++)
   {
   k=m*NLIMIT;
   for (j=0; j<MLIMIT; j++)
      {
      cc[j][i]=bvector[j+k];
      cp[j][i]=bvector[NLIMIT*MLIMIT+j+k];
      }
   m=m+1;
   }

/* compute authalic coefficients : authcoef1 & 2 */
authalic_coef(ac, ap);

return 0;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mp_routines.imake
/* Imake file for MIPS subroutines MP_ROUTINES   */

#define SUBROUTINE  	mp_routines

#define MODULE_LIST 	mpinit.c mpfree.c mpll2xy.c mpxy2ll.c \
			mpsetvalues.c mpgetvalues.c mpgetkeywords.c \
                        mpmpo2buf.c

#define MODULE_LIST2	mpsetdebugflag.c mpinternals.c \
                        persp_proj_c.c ortho_obl_c.c mp_triaxcoef.c

#define INCLUDE_LIST  	mp_private.h mp_init.h mp_prototypes.h

#define P1_SUBLIB

#define USES_ANSI_C

#define LIB_LOCAL	/* for development, remove on delivery */ 
#define DEBUG		/* for development, remove on delivery */ 
$ Return
$!#############################################################################
$Other_File:
$ create mp_internals.f
c
c subroutine triaxcoef
c
c Function extracted from program TRICOEF.COM by Jean Lorre.
c
c	Date extracted:		October 1993
c	Extracted by:		Justin McNeill
c
c This has been modified to now include both authalic as well as conformal
c auxiallary coefficient functionality. Code was adopted from Jean Lorre's
c tricoef routine. Recent changes also made in the randum number generator
c function, to 'rangen', to correct for errors occurring on the sgi system
c with the older 'ran1' function   -  pxa  august '96

c oct96 -lwk- hard-coded the random-number seed in order to allow comparison
c		of test logs;  this will be replaced in the future by 
c		allowing the user to specify this (as well as the 3 limits),
c		which will require a new routine to give the user access

c jun98 -lwk- replaced rangen with modified version of RAN1 because of 
c		problems on DEC-Unix 
c aug98 -lwk- made all xvmessage calls dependent on flag.eq.1 (except for
c		one initial one)

      subroutine triaxcoef( radii,cc,cp,ac,ap,nlimit,mlimit,klimit,
     +	flag,status)
      implicit real*8 (a-h,o-z)
      real*8 cc(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 ac(0:mlimit,0:klimit),ap(nlimit)
      parameter (maxpts=800) ! Changes must be made in other places too !
      parameter (maxeqs=400)
      parameter (latpts=72,lonpts=72)
      character*80 msg
      character*12 planet
      integer*4 flag
      integer*4 count,status
      real*4 rannum, ran1
      real*8 radii(3),radsph
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      real*8 amatrix(maxeqs*maxeqs),bvector(maxeqs)
      real*8 authcoef1(maxpts),authcoef2(maxpts)

      common/c1/a,b,c,lat,lon,costheta,sintheta,f

c initialize spice for planet radii retrieval.
c      call init_spice
c
c parameters
      planet='            '
      a=0.d0
c     call xvparmd('RADIUS',radii,count,def,3)

      count = 3

      if(count.gt.0)then
        a=radii(1)
        b=radii(2)
        c=radii(3)
      endif
c      call xvparm('PLANET',planet,count,def,1)
      
      count = 0 

c      if(count.eq.1)then
c      call ccase(planet,1)
c        call getplacon(planet,id,planet_data,ind)
c        if(ind.ne.0)then
c           call xvmessage('GETPLACON: unrecognizable planet',' ')
c           call xvmessage('Input name is:'//planet,' ')
c           if(a.eq.0.d0) call abend
c        endif
c        call xvmessage('Name will be placed in coefficnt file',' ')
c        a=planet_data(1)
c        b=planet_data(2)
c        c=planet_data(3)
c      endif
c
c      call xvparmd('RADIUS',radii,count,def,3)
c
c      if(count.gt.0)then
c       a=radii(1)
c       b=radii(2)
c       c=radii(3)
c     endif
c      call xvparm('MLIMIT',mlimit,count,def,1)
c      call xvparm('NLIMIT',nlimit,count,def,1)

      if (flag.eq.1) then
        write(msg,108)a,b,c
108     format('Planet radii a,b,c:',3f10.2)
c        call xvmessage(msg,' ')
      else
c	call xvmessage(' Computing triaxial coefficients ...',' ')
      endif

      if(a.eq.0.d0)then
         call xvmessage('Planet radii unspecified',' ')
         status = -401
         return
c         call abend
      endif
      if((c.ge.b).or.(b.ge.a))then
         call xvmessage('Radii out of order, should be decreasing.',' ')
         call xvmessage('Oblate spheroids not supported.',' ')
         status = -402
         return
c         call abend
      endif
      c=c/a
      b=b/a
      a=1.d0

      nequations=nlimit*mlimit*2
      if(nequations.gt.maxeqs)then
         call xvmessage('Too many equations requested',' ')
         write(msg,100) maxeqs
100      format(' Maximum number of equations = ',i5)
         call xvmessage(msg,' ')
         status = -403
         return
c         call abend
      else
	 if (flag.eq.1) then
          write(msg,110) nequations
110       format(i4,' simultaneous equations.')
          call xvmessage(msg,' ')
         endif
      endif

      npoints=nequations*2           ! npoints > nequations/2 (Snyder).
      if(npoints.gt.maxpts)then
         npoints=maxpts
	 if (flag.eq.1) then
          call xvmessage('Warning: #points must be > #equations/2',' ')
          write(msg,102) nequations
102       format(' Number of equations = ',i5)
          call xvmessage(msg,' ')
         endif
      endif
      if (flag.eq.1) then
       write(msg,111) npoints
111    format(i4,' random points selected to fit equations.')
       call xvmessage(msg,' ')
      endif

c compute the fit points from a random grid in the range lat=0 to 85
c long=0 to 90.
      idum=-768825576		! establish a seed for random numbers
c      call get_seconds(idum)
c      if (flag.eq.1) then
c      write(msg,112) idum
c112    format(' The random number seed = ',i10)
c       call xvmessage(msg,' ')
c      endif
      degtorad=datan(1.0d0)/45.d0
      do i=1,npoints
	rannum = ran1(idum)
	lat(i)=degtorad*85.*rannum
	rannum = ran1(idum)
	lon(i)=degtorad*90.*rannum
      enddo

      if (flag.eq.1) then
        call xvmessage('Printing out random Lat/Lon values ...',' ')
        do i=1,npoints
          write(msg,101) lat(i), lon(i)
          call xvmessage(msg,' ')
        enddo
        call xvmessage(' ',' ')
        call xvmessage('Printing out intermediate values ...',' ')
      endif

c compute & save values unique to each point.
      do i=1,npoints
         flon=(dsin(lon(i)))**2+((b*b)/(a*a))*(dcos(lon(i)))**2    ! eqn 9
         flat=(dsin(lat(i)))**2+((c*c)/(b*b))*(dcos(lat(i)))**2    ! eqn 10
         t1=(1.d0-(b*b)/(a*a))*dsin(lon(i))*dcos(lon(i))*dsin(lat(i))
         t2=(dsin(lat(i)))**2+(c**4/b**4)*flon*(dcos(lat(i)))**2
         t3=(dsin(lon(i)))**2+(b**4/a**4)*(dcos(lon(i)))**2
         costheta(i)=t1/(dsqrt(t2)*dsqrt(t3))                      ! eqn 48
         sintheta(i)=dsin(dacos(costheta(i)))
         f(i)=dcos(lat(i))*dsqrt(t3)*flat/(flon*dsqrt(t2))         ! eqn 50
         if (flag.eq.1) then
           write(msg,105) costheta(i), sintheta(i), f(i)
           call xvmessage(msg,' ')
         endif
      enddo

c load matrices to do the linear fit.                              ! eqn 80
      ip=1
      iq=0
      do i=1,mlimit*nlimit       ! first 1/2 of equations loop

         j=i
         m=1
         n=0
         do k=1,mlimit*nlimit    ! first 1/2 of terms in equation
            t=0.d0
            do ii=1,npoints
               t=t+p1(m,n,ii)*p1(ip,iq,ii)+p3(m,n,ii)*p3(ip,iq,ii)
            enddo
            if(n.eq.nlimit-1)then
               n=0
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         m=0
         n=1
         do k=mlimit*nlimit+1,nequations         ! second 1/2 of terms 
            t=0.d0
            do ii=1,npoints
               t=t+p2(m,n,ii)*p1(ip,iq,ii)+p4(m,n,ii)*p3(ip,iq,ii)
            enddo
            if(n.eq.nlimit)then
               n=1
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         t=0.d0
         do ii=1,npoints
            t=t+(f(ii)/dcos(lat(ii))-sintheta(ii))*p1(ip,iq,ii)+
     +        (1.d0-f(ii)*sintheta(ii)/dcos(lat(ii)))*p3(ip,iq,ii)
         enddo
         bvector(i)=t
         if(iq.eq.nlimit-1)then
            iq=0
            ip=ip+1
         else
            iq=iq+1
         endif

      enddo

c      ip=1
c      iq=0
      ip=0
      iq=1
      do i=mlimit*nlimit+1,nequations       ! second 1/2 of equations loop

         j=i
         m=1
         n=0
         do k=1,mlimit*nlimit    ! first 1/2 of terms in equation
            t=0.d0
            do ii=1,npoints
               t=t+p1(m,n,ii)*p2(ip,iq,ii)+p3(m,n,ii)*p4(ip,iq,ii)
            enddo
            if(n.eq.nlimit-1)then
               n=0
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         m=0
         n=1
         do k=mlimit*nlimit+1,nequations         ! second 1/2 of terms 
            t=0.d0
            do ii=1,npoints
               t=t+p2(m,n,ii)*p2(ip,iq,ii)+p4(m,n,ii)*p4(ip,iq,ii)
            enddo
            if(n.eq.nlimit)then
               n=1
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         t=0.d0
         do ii=1,npoints
c            if(i.eq.5)write(*,*)ip,iq,p2(ip,iq,ii),p4(ip,iq,ii)
            t=t+(f(ii)/dcos(lat(ii))-sintheta(ii))*p2(ip,iq,ii)+
     +        (1.d0-f(ii)*sintheta(ii)/dcos(lat(ii)))*p4(ip,iq,ii)
         enddo
         bvector(i)=t
c         if(iq.eq.nlimit-1)then
c            iq=0
         if(iq.eq.nlimit)then
            iq=1
            ip=ip+1
         else
            iq=iq+1
         endif

      enddo

      if (flag.eq.1) then

c print the a-matrix
        call xvmessage('A-matrix terms by row..',' ')
        do i=1,nequations*nequations,nequations
          do j=i,i+nequations-1,6
            m=j+5
            if(m.gt.i+nequations-1) m=i+nequations-1
            write(msg,104)(amatrix(k),k=j,m)
104         format(6f13.0)
            call xvmessage(msg,' ')
          enddo
        enddo

c print the b-vector

        call xvmessage('B-vector terms top down..',' ')
        do j=1,nequations,8
          m=j+7
          if(m.gt.nequations)m=nequations
          write(msg,101)(bvector(k),k=j,m)
101       format(8f10.4)
          call xvmessage(msg,' ')
        enddo

      endif

C       k=1
C       do i=1,nequations
C          check(i)=amatrix(k)
C          k=k+nequations
C       enddo
C       result=bvector(1)

c perform the solution.
      if (flag.eq.1) then
       call xvmessage('Beginning matrix solution.',' ')
      endif
      call mp_dsimq(amatrix,bvector,nequations,ks)
      if(ks.ne.0)then
         call xvmessage('Ill conditioned equations, no solution.',' ')
	 status = -1
         return
      endif

c      r=0.d0
c      do i=1,nequations
c         r=r+bvector(i)*check(i)
c      enddo
c      write(*,*)'should,actual ',result,r

c print terms
      if (flag.eq.1) then
        call xvmessage('C(m,n) coefficients. Rows m=1,2,3,...',' ')
        call xvmessage('Columns begin at n=0,1,2,3...',' ')
      endif
      m=0
      do i=1,mlimit*nlimit,nlimit
         m=m+1
         do j=i,i+nlimit-1,5
            l=j+4
            if(l.gt.i+nlimit-1) l=i+nlimit-1
	    if (flag.eq.1) then
              write(msg,105)(bvector(k),k=j,l)
105           format(5f16.11)
              call xvmessage(msg,' ')
	    endif
         enddo
      enddo
      if (flag.eq.1) then
         call xvmessage('C(m,n)primed coefficients. Rows m=0,1,2,...',
     &    ' ')
         call xvmessage('Columns begin at n=1,2,3...',' ')
      endif
      m=-1
      do i=mlimit*nlimit+1,nequations,nlimit
         m=m+1
         do j=i,i+nlimit-1,5
            l=j+4
            if(l.gt.i+nlimit-1) l=i+nlimit-1
	    if (flag.eq.1) then
              write(msg,105)(bvector(k),k=j,l)
              call xvmessage(msg,' ')
	    endif
         enddo
      enddo

c Recompute NPOINTS random points for RMS error computation.
c The same points won't do since we've fitted to them.

      do i=1,npoints
	rannum = ran1(idum)
	lat(i)=degtorad*85.*rannum
	rannum = ran1(idum)
	lon(i)=degtorad*90.*rannum
      enddo
      do i=1,npoints
         flon=(dsin(lon(i)))**2+((b*b)/(a*a))*(dcos(lon(i)))**2    ! eqn 9
         flat=(dsin(lat(i)))**2+((c*c)/(b*b))*(dcos(lat(i)))**2    ! eqn 10
         t1=(1.d0-(b*b)/(a*a))*dsin(lon(i))*dcos(lon(i))*dsin(lat(i))
         t2=(dsin(lat(i)))**2+(c**4/b**4)*flon*(dcos(lat(i)))**2
         t3=(dsin(lon(i)))**2+(b**4/a**4)*(dcos(lon(i)))**2
         costheta(i)=t1/(dsqrt(t2)*dsqrt(t3))                      ! eqn 48
         sintheta(i)=dsin(dacos(costheta(i)))
         f(i)=dcos(lat(i))*dsqrt(t3)*flat/(flon*dsqrt(t2))         ! eqn 50
      enddo

c compute rms error for the coefficient set.
      if (flag.eq.1) then
        call xvmessage('    ',' ')
        call xvmessage('The RMS error is the constraint violation',' ')
        call xvmessage('ie: angle and scale or equal area    ',' ')
      endif
      call rmserror(bvector,bvector(mlimit*nlimit+1),
     +              nlimit,mlimit,npoints,rms)
      if (flag.eq.1) then
        write(msg,106)rms
106     format(' Rms error using these coefficients is ',f15.11)
        call xvmessage(msg,' ')
      endif
c
c abandon coefficients
c      if(planet.eq.'            ') then
c        call xvmessage('Coefficients discarded',' ')
c        return
c     endif
c
c save coefficients
     
       k=0
       m=0
       if (flag.eq.1) then
	call xvmessage('These are the loaded cc,cp arrays...',' ')
       endif
       do i=1,nlimit
	 k=m*nlimit
         do j=1,mlimit
		cc(i,j)=bvector(j+k)
	        cp(i,j)=bvector(nlimit*mlimit+j+k)
c		type *,i,j,cc(i,j),cp(i,j)
	        if (flag.eq.1) then
		  write(msg,107) i,j,cc(i,j),cp(i,j)
107		  format(i4,i4,f16.11,f16.11)
		  call xvmessage(msg,' ')
		endif
         enddo
	 m=m+1
       enddo
c      endif

c *******************************************************
c compute authalic coefficients : authcoef1 & 2
c *******************************************************
      call authalic_coef(a,b,c,mlimit,klimit,nlimit,
     + 	amatrix(1),
     +  amatrix((latpts+1)*(lonpts+1)+1),
     +  amatrix((latpts+1)*(lonpts+1)*2+1),
     +  authcoef1,authcoef2,radsph)
c	print terms
      if (flag.eq.1) then
	call xvmessage('Authalic Case:',' ')
	call xvmessage('COEF(m,k) coefficients. Rows m=0,1,2...',' ')
	call xvmessage('Columns begin at k=0,1,2,....',' ')
	do k=1,(klimit+1)*(mlimit+1),mlimit+1
	  do i=k,k+mlimit,5
	    ii=i+4
	    if(ii.gt.k+mlimit) ii=k+mlimit
		write (msg,105) (authcoef1(j),j=i,ii)
		call xvmessage(msg,' ')
	   enddo
	enddo
	call xvmessage('COEFP(n) coefficient array. n=0,1,2,....',' ')
	do i=1,nlimit,5
	   ii=i+4
	   if(ii.gt.nlimit) ii=nlimit
	   write (msg,105) (authcoef2(j),j=i,ii)
	   call xvmessage(msg,' ')
	enddo
	write(msg,117)radsph
117	format('Radius of equivalent area sphere is ',f16.8)
	call xvmessage(msg,' ')
      endif
      
c compute rms error for the coefficient set
      if (flag.eq.1) then
        call xvmessage('     ',' ')
        call xvmessage('The RMS error is the constraint violation',' ')
        call authalic_rmserror(authcoef1,authcoef2,nlimit,klimit,mlimit,
     +		radsph,degtorad,a,b,c,rms)
        write(msg,116)rms
116     format('Authalic rms error using these coefficients is ',f15.11)
        call xvmessage(msg,' ')
      endif

c save coefficients
      k=0
      if (flag.eq.1) call xvmessage('Loaded ac array...',' ')
      do i=0,mlimit,1
	do j=0,klimit,1
	  ac(i,j)=authcoef1(k+1)
	  if (flag.eq.1) then
	    write(msg,135) i,j,ac(i,j)
135	    format(i4,i4,f16.11)
	    call xvmessage(msg,' ')
	  endif
	  k=k+1
	enddo
      enddo
      if (flag.eq.1) call xvmessage('Loaded ap array...',' ')
      do i=1,nlimit,1
	ap(i)=authcoef2(i)
	if (flag.eq.1) then
		write(msg,136) i,ap(i)
136		format(i4,f16.11)
		call xvmessage(msg,' ')
	endif
      enddo
	
      status=0

      return 
      end

c ********************************************************************
      subroutine rmserror(c,cp,nlimit,mlimit,npoints,rms)

c Routine to compute solution rms error equation #73.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c rms= rms error                            returned   real*8

      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,cc,lat,lon,costheta,sintheta,f

      rms=0.d0
      do i=1,npoints ! loop over random points used for fitting

c        Compute equation 67
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+n*c(n+1,m)*dsin(n*lat(i))
            enddo
            t=t+t1*dsin(2*m*lon(i))
         enddo
         eq67=a*(-t)

c        Compute equation 68
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+c(n+1,m)*dcos(n*lat(i))
            enddo
            t=t+m*t1*dcos(2*m*lon(i))
         enddo
         eq68=a*(1+2*t)

c        Compute equation 69
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+n*cp(n,m+1)*dcos(n*lat(i))
            enddo
            t=t+t1*dcos(2*m*lon(i))
         enddo
         eq69=a*(1.d0/dcos(lat(i))+t)

c        Compute equation 70
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+cp(n,m+1)*dsin(n*lat(i))
            enddo
            t=t+m*t1*dsin(2*m*lon(i))
         enddo
         eq70=a*(-2.d0*t)

         e1=eq68*sintheta(i)+eq70*costheta(i)-eq69*f(i)        !eqn 71
         e2=eq69*f(i)*sintheta(i)+eq67*f(i)*costheta(i)-eq68   !eqn 72
         rms=rms+(e1*e1+e2*e2)                                 !eqn 73
         
      enddo
      rms=dsqrt(rms/npoints)           ! equation 81

      return
      end


c*********************************************************************
c functions used often
      real*8 function p1(m,n,i)      ! equation 76
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p1=2.d0*a*m*sintheta(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))
      return
      end

      real*8 function p2(m,n,i)      ! equation 77
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p2=-2.d0*a*m*costheta(i)*dsin(n*lat(i))*dsin(2.d0*m*lon(i))-
     +   a*n*f(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))
      return
      end

      real*8 function p3(m,n,i)      ! equation 78
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p3=-2.d0*a*m*dcos(n*lat(i))*dcos(2.d0*m*lon(i))-
     +   a*n*f(i)*dsin(n*lat(i))*dsin(2.d0*m*lon(i))*costheta(i)
      return
      end

      real*8 function p4(m,n,i)      ! equation 79
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      p4=a*n*f(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))*sintheta(i)
      return
      end

c*********************************************************************
      REAL*4 FUNCTION RAN1(IDUM)
c     Returns random number between 0.0 and 1.0. To initialize provide
c     negative idum value.
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773)
      PARAMETER (M2=134456,IA2=8121,IC2=28411)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      PARAMETER (RM1=1./M1,RM2=1./M2)
      DATA IFF /0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IX1=MOD(IC1-IDUM,M1)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX2=MOD(IX1,M2)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX3=MOD(IX1,M3)
        DO 11 J=1,97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11      CONTINUE
        IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97) j=97
      IF(J.LT.1) j=1
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END

C*********************************************************************
      SUBROUTINE MP_DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL MP_DSIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8       A(1),B(1),biga,save,tol
      character*80 msg
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      write( msg, 36 ) BIGA
   36 format( 'BIGA ',D12.6 )
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

c ***********************************************************************
      subroutine authalic_coef(a,b,c,mlimit,klimit,nlimit,
     +        areafun,autlon,acoef,coef,coefp,radsph)
 
c To compute coefficients permitting the snyder to authalic computation.
c a= major axis normalized to 1              real*8  input
c b= middle axis normalized                  real*8 input
c c= minor axis normalized                   real*8 input
c mlimit= m maximum                          integer*4 input
c klimit= k maximum                          integer*4 input
c nlimit= n maximum                          integer*4 input
c arefun= scratch space for buffer areafun   real*8    input
c autlon= scratch space for buffer autlon    real*8    input
c acoef= scratch space for buffer acoef      real*8    input
c coef= longitude computation coefficients   real*8  returned
c coefp= latitude computation coefficients   real*8 returned
c radsph= radius of equivalent sphere        real*8 returned
 
      implicit real*8 (a-h,o-z)
      parameter (latpts=72,lonpts=72)
      real*8 elllat(0:latpts),elllon(0:lonpts)
      real*8 areafun(0:latpts,0:lonpts)
      real*8 acoef(0:latpts,0:mlimit),autlat(0:latpts)
      real*8 autlon(0:latpts,0:lonpts)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)

      do i=0,latpts
	do j=0,mlimit
	   acoef(i,j)=0.0
	enddo
      enddo
      do i=0,latpts
	do j=0,lonpts
	   autlon(i,j)=0.0
	enddo
      enddo
      do i=0,mlimit
	do j=0,klimit
	   coef(i,j)=0.0
	enddo
      enddo
      do i=0,latpts
	autlat(i)=0.0
      enddo
      do i=1,nlimit
	coefp(i)=0.0
      enddo
 
      dg1=datan(1.d0)/45.d0
      pi=datan(1.d0)*4.d0
      pih=pi/2.d0
      cb2=c*c/(b*b)
      ba2=b*b
 
      do i=0,latpts-1
        elllat(i)=i*dg1*90.d0/latpts
      enddo
      elllat(latpts)=(latpts-.001)*dg1*90.d0/latpts
      do j=0,lonpts
        elllon(j)=j*dg1*90.d0/lonpts
      enddo
 
c compute area function for integration
      do i=0,latpts
        do j=0,lonpts
          slon=dsin(elllon(j))
          clon=dcos(elllon(j))
          slat=dsin(elllat(i))
          clat=dcos(elllat(i))
          flon=slon*slon+ba2*clon*clon       ! eqn 9
          flat=slat*slat+cb2*clat*clat       ! eqn 10
          t1=(1.d0-ba2)*slon*clon*slat
          t2=dsqrt(slat*slat+cb2*cb2*flon*clat*clat)
          t3=dsqrt(slon*slon+ba2*ba2*clon*clon)
	  costheta=t1/(t2*t3)                ! eqn 48
          sintheta=dsqrt(dabs(1.d0-costheta*costheta))
          areafun(i,j)=clat*t3*t2*sintheta/(flon*flon*flat*flat)
        enddo
      enddo
 
c compute total area of triaxial ellipsoid
      latlmt=latpts
      call simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +           elllat,areafun,pih,area)
      tarea=area
 
c compute authalic latitudes
      latlmt=0
      autlat(0)=0.d0
      do latlmt=6,latpts,6
        call simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +             elllat,areafun,pih,area)
        sphiaut=area/tarea
        if(dabs(sphiaut).gt. 0.9999999)then
          autlat(latlmt)=pih
        else
          autlat(latlmt)=
     +        datan(sphiaut/dsqrt(dabs(1.d0-sphiaut*sphiaut)))
        endif
      enddo
 
c compute coefficients for authalic latitude using simpson integration
      do n=1,nlimit
        sum1=0.d0
        do i=6,latpts-6,12
          autfunc=(autlat(i)-elllat(i))*dsin(2*n*elllat(i))
          sum1=sum1+4.d0*autfunc
        enddo
        do i=12,latpts-12,12
          autfunc=(autlat(i)-elllat(i))*dsin(2*n*elllat(i))
          sum1=sum1+2.d0*autfunc
        enddo
        coefp(n)=4.d0*sum1/latpts
      enddo
      radsph=a*dsqrt(8.d0*tarea/(4.d0*pi)) !radius of equivalent sphere
 
c compute authalic longitude
      do i=0,latpts,6
c       first compute area of zone of quadrant along latitude
        lonlmt=lonpts
        call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
        tcirc=fsimlon
c       now compute portion of zone for each step of longitude
        lonlmt=0
        autlon(i,0)=0.d0
        do lonlmt=6,lonpts,6
          call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
          autlon(i,lonlmt)=(fsimlon/tcirc)*pih
        enddo
      enddo
 
c compute elements for simpson integration for acoef of authalic longitude
      do m=0,mlimit
        do i=0,latpts,6
          sum1=0.d0
          do j=6,lonpts-6,12
            autfunc=(autlon(i,j)-elllon(j))*dsin(2*m*elllon(j))
            sum1=sum1+4.d0*autfunc
          enddo
          do j=12,lonpts-12,12
            autfunc=(autlon(i,j)-elllon(j))*dsin(2*m*elllon(j))
            sum1=sum1+2.d0*autfunc
          enddo
          acoef(i,m)=4.d0*sum1/lonpts
        enddo
      enddo
 
c compute elements for simpson integration for C coef of authalic longitude
      do m=0,mlimit
        k=0
        sum1=acoef(0,m)
        do j=6,latpts-6,12
          sum1=sum1+4.d0*acoef(j,m)
        enddo
        do j=12,latpts-12,12
          sum1=sum1+2.d0*acoef(j,m)
        enddo
        sum1=sum1+acoef(latpts,m)
        coef(m,0)=2.d0*sum1/latpts
c	write(msg,800)(coef(m,0))
c 800	format(5f16.11)
      enddo
 
      do m=1,mlimit
        do k=1,klimit
          sum1=acoef(0,m)
          do j=6,latpts-6,12
            autfunc=acoef(j,m)*dcos(2*k*elllat(j))
            sum1=sum1+4.d0*autfunc
          enddo
          do j=12,latpts-12,12
            autfunc=acoef(j,m)*dcos(2*k*elllat(j))
            sum1=sum1+2.d0*autfunc
          enddo
          sum1=sum1+acoef(latpts,m)*dcos(2*k*elllat(latpts))
          coef(m,k)=4.d0*sum1/latpts
c	  write(msg,801)(coef(m,k))
c 801	  format(5f16.11)
        enddo
      enddo
 
      return
      end
 
 
c *********************************************************************
      subroutine simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
 
c Compute elements for Simpsons integration.
c fsimlon is returned
      implicit real*8 (a-h,o-z)
      real*8 areafun(0:latpts,0:lonpts)
 
      sum1=areafun(i,0)+areafun(i,lonlmt)
      do j=1,lonlmt-1,2
         sum1=sum1+4.d0*areafun(i,j)
      enddo
      do j=2,lonlmt-2,2
         sum1=sum1+2.d0*areafun(i,j)
      enddo
      fsimlon=sum1*pih/(lonpts*3)
      return
      end
 
c ******************************************************************
      subroutine simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +                  elllat,areafun,pih,area)
 
c Computes areas of zones
c lonlmt & area are returned
      implicit real*8 (a-h,o-z)
 
      real*8 elllat(0:latpts)
      real*8 areafun(0:latpts,0:lonpts)
 
      i=0
      lonlmt=lonpts
      call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
      sum2=fsimlon
      do i=1,latlmt-1,2
         call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
         sum2=sum2+4.d0*fsimlon
      enddo
      do i=2,latlmt-2,2
         call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
         sum2=sum2+2.d0*fsimlon
      enddo
      i=latlmt
      call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
      sum2=sum2+fsimlon
 
      area=c*c*sum2*elllat(latlmt)/(latlmt*3)
      return
      end
          
c********************************************************************
      subroutine authalic_rmserror(coef,coefp,nlimit,klimit,mlimit,
     +     radsph,degtorad,a,b,c,rmse)
 
c Routine to compute authalic solution rms error equation.
c Reference: Snyder, Survey Review Vol28, 217, July 1985
 
c COEF =  matrix of coefficients              input      real*8
c COEFP = primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c radsph= radius of equivalent sphere       input      real*8
c degtorad=degrees/radian                   input      real*8
c a= major ellipsoid radius                 input      real*8
c b= middle ellipsoid radius                 input      real*8
c c= small ellipsoid radius                 input      real*8
c rms= rms error                            returned   real*8
 
      implicit real*8 (a-h,o-z)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)
 
      bige=0.d0
      lptcount=0
      cb2=c*c/(b*b)
      ba2=b*b
      pi=datan(1.d0)*4.d0
      pih=pi/2.d0
 
      do i=0,90,5
        do j=90,0,-5
          elat=j
          elon=i
          call authtran(coef,coefp,nlimit,klimit,mlimit,
     +          elat,elon,alat,alon,1,ind)
          elat=elat*degtorad
          elon=elon*degtorad
          alat=alat*degtorad
          alon=alon*degtorad
          sp=dsin(elat)
          cp=dcos(elat)
          sl=dsin(elon)
          cl=dcos(elon)
          f1p=sp*sp+cb2*cp*cp
          f1l=sl*sl+ba2*cl*cl
          f2p=dsqrt(sp*sp+f1l*cb2*cb2*cp*cp)
          f2l=dsqrt(sl*sl+ba2*ba2*cl*cl)
          dxdp=0.d0
          dydp=1.d0
          dxdl=1.d0
          dydl=0.d0
          do m=1,mlimit
            dxdll=0.d0
            dxdpl=0.d0
            do k=0,klimit
              dxdll=dxdll+coef(m,k)*dcos(2*k*elat)
              dxdpl=dxdpl+2*k*coef(m,k)*dsin(2*k*elat)
            enddo
            dxdl=dxdl+dxdll*2.d0*m*dcos(2*m*elon)
            dxdp=dxdp-dxdpl*dsin(2*m*elon)
          enddo
          do n=1,nlimit
            dydp=dydp+n*2*coefp(n)*dcos(2*n*elat)
          enddo
          dxdl=dxdl*radsph/a
          dxdp=dxdp*radsph/a
          dydp=(dydp*radsph/a)*dcos(alat)
          dsdp=c*f2p/(dsqrt(f1l)*f1p*dsqrt(f1p))
          dsdl=c*cp*f2l/(dsqrt(f1p)*f1l*dsqrt(f1l))
          sinth=dydp/dsqrt(dxdp*dxdp+dydp*dydp)
          sh=dsqrt(dxdp*dxdp+dydp*dydp)/dsdp  ! scale factr on meridian
          sk=dxdl/dsdl                        ! scale factr on parallel
          th=(pih-datan(dxdp/dydp))/degtorad
          arerr=sh*sk*sinth-1.d0
          if(j.lt.89) then
            bige=bige+arerr*arerr
            lptcount=lptcount+1
          endif
        enddo    ! j
      enddo      ! i
      rmse=dsqrt(bige/lptcount)
 
      return
      end
 
c ***************************************************************
$!-----------------------------------------------------------------------------
$ create persp_proj.f
      subroutine pproj_mp(data,line,samp,lat,lon,imode,ind)
 
c DATA = standard 40-word geometry buffer (see subroutine CONVEV) for 
c       Perspective case, except that word 37 = equatorial semi-minor axis
 
c LINE,SAMP = object space location of a point
 
c LAT,LON = planetary coordinates of point
c           (radians, planetocentric Lat., West Lon.)
 
c MODE:  1 = (LAT,LON) to (LINE,SAMP)   2 = (LINE,SAMP) to (LAT,LON) 
 
c IND = return indicator.  0=success, 1=failure. (to coincide w/mp_routines)
 
c  31oct93  lwk  implemented tri-axial ellipsoid model with extra radius
c               in word 31 of DATA
c  15may94  lwk  fixed BOP test for triaxial ellipsoid using NAIF calls
c    jun96  pxa  cleaned up for use with MP routines, changed failure indicator 
c               from 0 to 1, 3rd radius from word 31 to 37 in DATA
c    jul96  pxa  replaced naif/spice calls to 'surfnm','vsep', 'vnorm', and 
c		 'halfpi' with relevant in-line code, to remove dependency of 
c		 naif/ spice routines in mp_routines
c  30jul01  lwk  fixed bug in replacement of vnorm code
 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CP(3),OP(3),CPP(3),OM(3,3),RS(3),xnorm(3),M
      REAL*8 EPS/1.E-6/
      REAL*8 U1(3),U2(3),VTEMP(3)
      REAL*4 LAT,LON,LINE,SAMP,DATA(*)
      DATA PI, RADDEG, DEGRAD / 3.141592653589793D0, 5.72957795130823D1,
     & 1.74532925199433D-2/
C
      HALFPI = PI/2.0
      IND = 1

      FL = DATA(27)	! camera focal length (mm)
      OAL = DATA(28)	! optical axis Line
      OAS = DATA(29)	! optical axis Sample
      SCALE = DATA(30)	! camera scale (pix/mm)

      RP = DATA(25)     ! polar radius
      RA = DATA(26)     ! equatorial semi-major radius
 
      RB = DATA(37)
c         check if RB is garbage, in which case assume oblate spheroid:
      IF (RB.LT.RP .OR. RB.GT.RA) RB = RA
      E1 = (RA/RP)
      E1 = E1*E1
      E2 = (RB/RP)
      E2 = E2*E2

c OM-matrix (Camera-to-Body coordinates) and RS-vector (body center to camera)
c are stored in the first 24 words of the buffer:
c      CALL MVL(DATA,OM,72)
c      CALL MVL(DATA(19),RS,24)
      call mve(1,72,data,om,1,1)
      call mve(1,24,data(19),rs,1,1)
C
      IF(IMODE.EQ.2) GOTO 30
C     ....Here to convert (lat,lon) to (line,samp)

      RLAT = LAT
      RLON = LON	
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
      CLON = DCOS(RLON)
      SLON = DSIN(RLON)
C          COMPUTE GEOCENTRIC RADIUS
      D1 = RP*RP*CLAT*CLAT
      RB2 = RB*RB
      RA2 = RA*RA
      CLN2 = CLON*CLON
      SLN2 = SLON*SLON
      SLT2 = SLAT*SLAT
      R = (RA*RB*RP)/DSQRT(D1*RB2*CLN2+D1*RA2*SLN2+RA2*RB2*SLT2)
      CP(1) = R*CLAT*CLON - RS(1)
      CP(2) = -R*CLAT*SLON - RS(2)
      CP(3) = R*SLAT - RS(3)
C
c  BOP test for triaxial ellipsoid
c  *******************************************************************
c  The following code replaces the functionality of 'surfnm','vsep',and
c  halfpi() routines. This needed to be done to remove spice/naif
c  dependencies from mp_routines
c  ******************************************************************
c      call surfnm( ra, rb, rp, op, xnorm)
 
c      note CP is -1 * vector from P to C, so reverse criterion:
c      ibop = 0
c      if ( vsep(cp,xnorm) .lt. halfpi()) ibop = 1

      op(1) = r*clat*clon 
      op(2) = -r*clat*slon 
      op(3) = r*slat 

      M=MIN(RA,RB,RP)
      A1=M/RA
      B1=M/RB
      C1=M/RP

      XNORM(1) = OP(1)*(A1*A1)
      XNORM(2) = OP(2)*(B1*B1)
      XNORM(3) = OP(3)*(C1*C1)

      VMAX1 = MAX(DABS(XNORM(1)),DABS(XNORM(2)),DABS(XNORM(3)))
      IF (VMAX1 .EQ. 0.) THEN
	VNORM1 = 0.
      ELSE
	VNORM1 = VMAX1*DSQRT((XNORM(1)/VMAX1)**2 + (XNORM(2)/VMAX1)**2 +
     &			(XNORM(3)/VMAX1)**2)
      END IF
      VMAG1 = VNORM1
      IF (VMAG1 .GT. 0.) THEN
	XNORM(1) = XNORM(1)/VMAG1
        XNORM(2) = XNORM(2)/VMAG1
        XNORM(3) = XNORM(3)/VMAG1
      ELSE
	XNORM(1) = 0.
	XNORM(2) = 0.
	XNORM(3) = 0.
      END IF
      ibop = 0
c     Now compute vsep(cp,xnorm), and assign it a value
      VMAX2 = MAX(DABS(CP(1)),DABS(CP(2)),DABS(CP(3)))
      IF (VMAX2 .EQ. 0.) THEN
	VNORM2 = 0.
      ELSE
	VNORM2 = VMAX2*DSQRT((CP(1)/VMAX2)**2 + (CP(2)/VMAX2)**2 +
     &			(CP(3)/VMAX2)**2)
      END IF
      VMAG2 = VNORM2
      IF (VMAG2 .GT. 0.) THEN
	U1(1) = CP(1)/VMAG2
	U1(2) = CP(2)/VMAG2
	U1(3) = CP(3)/VMAG2
      ELSE
	U1(1) = 0.
	U1(2) = 0.
	U1(3) = 0.
      END IF
      IF (VMAG2 .EQ. 0.) THEN
	VSEP1 = 0.
      END IF

      VMAX3 = MAX(DABS(XNORM(1)),DABS(XNORM(2)),DABS(XNORM(3)))
      IF (VMAX3 .EQ. 0.) THEN
	VNORM3 = 0.
      ELSE
	VNORM3 = VMAX3*DSQRT((XNORM(1)/VMAX3)**2 + (XNORM(2)/VMAX3)**2 +
     &			(XNORM(3)/VMAX3)**2)
      ENDIF
      VMAG3 = VNORM3
      IF (VMAG3 .GT. 0.) THEN
	U2(1) = XNORM(1)/VMAG3
	U2(2) = XNORM(2)/VMAG3
	U2(3) = XNORM(3)/VMAG3
      ELSE
	U2(1) = 0.
	U2(2) = 0.
	U2(3) = 0.
      END IF
      IF (VMAG3 .EQ. 0.) THEN
	VSEP1 = 0.
      END IF
      VDOT1 = U1(1)*U2(1) + U1(2)*U2(2) + U1(3)*U2(3)
      IF (VDOT1 .EQ. 0.) THEN
	VSEP1 = PI/2.0
      ELSE
        VTEMP(1) = U1(1) - U2(1)
        VTEMP(2) = U1(2) - U2(2)
        VTEMP(3) = U1(3) - U2(3)
        VMAX4 = MAX(DABS(VTEMP(1)),DABS(VTEMP(2)),DABS(VTEMP(3)))
        IF (VMAX4 .EQ. 0.) THEN
          VNORM4 = 0.
        ELSE
          VNORM4 = VMAX4*DSQRT((VTEMP(1)/VMAX4)**2 + (VTEMP(2)/VMAX4)**2 +
     &                         (VTEMP(3)/VMAX4)**2)
        END IF
        IF (VDOT1 .GT. 0.) THEN
	  VSEP1 = 2.0 * ASIN(0.5 * VNORM4)
        ELSE 				! VDOT1 .LT. 0.
	  VSEP1 = PI - 2.0*ASIN(0.5*VNORM4)
        END IF
      END IF
      IF (VSEP1 .LT. HALFPI) IBOP=1 
c
c  *****************************************************************
c 
      DO 20 I=1,3
      D1 = 0.D0
      DO 10 J=1,3
   10 D1 = D1 + OM(I,J)*CP(J)
   20 CPP(I) = D1
C
      S = FL*SCALE/CPP(3)
      LINE = OAL + S*CPP(2)
      SAMP = OAS + S*CPP(1)
      IND = IBOP
      RETURN
C
C     ....Here to convert (line,samp) to lat,lon)
 
   30 X = SAMP - OAS

      Y = LINE - OAL
      Z = FL*SCALE
C
      DO 40 I=1,3
   40 CP(I) = OM(1,I)*X + OM(2,I)*Y + OM(3,I)*Z
C
      A = E2*CP(1)*CP(1) + E1*CP(2)*CP(2) + E1*E2*CP(3)*CP(3)
      B = E2*CP(1)*RS(1) + E1*CP(2)*RS(2) + E1*E2*CP(3)*RS(3)
      C = E2*RS(1)*RS(1) + E1*RS(2)*RS(2) + E1*E2*RS(3)*RS(3)
     &   - E1*E2*RP*RP
      D = B*B - A*C
      IF (D.lt.0.) return
C
      IND = 0
      S = (-B-DSQRT(D))/A
C
      DO I=1,3
        OP(I) = S*CP(I) + RS(I)
      ENDDO
C
      X = OP(1)
      Y = OP(2)
      Z = OP(3)
      X1 = DABS(X)
      Y1 = DABS(Y)
      Z1 = DABS(Z)
      D = DSQRT(X*X+Y*Y)
      IF(D.LT.Z1*EPS) GOTO 98
      LAT = DATAN(Z/D)*RADDEG         ! GEOCENTRIC LAT.
      LAT = LAT*DEGRAD
c      ENDIF
      IF(Y1.LT.X1*EPS) GOTO 96
      IF(X1.LT.Y1*EPS) GOTO 94
      LON = 360. - DATAN2(Y,X)*RADDEG
      LON = LON*DEGRAD
c      IF(LON.GT.360.) LON=LON-360.
      IF(LON.GT.(2.*PI)) LON=LON-(2.*PI)
      RETURN
C
   94 LON = 270.
      LON = LON*DEGRAD
c      IF(Y.LT.0.) LON=90.
      IF(Y.LT.0) LON=PI/2.
      RETURN
   96 LON = 0.
      LON = LON*DEGRAD
c      IF(X.LT.0.) LON=180.
      IF(X.LT.0.) LON=PI
      RETURN
C
   98 LAT = 90.
      LAT = LAT*DEGRAD
      IF(Z.LT.0.) LAT=-LAT
      LON = 0.
      LON = LON*DEGRAD
      RETURN
      END

$!-----------------------------------------------------------------------------
$ create ortho_obl.f
      SUBROUTINE ORTHO_OBL(IND,M,DATA,LINE,SAMPLE,LAT,LONG)

C  CONVERT L,S TO LAT LONG OR LAT,LONG TO L,S FOR THE ORTHOGRAPHIC
C  PROJECTION FOR AN OBLATE SPHEROID

C  11SEP96 -LWK-  CODE ADAPTED FROM SUBR. TRANV, FOR USE BY MP_ROUTINES
c  23Oct97 -Scholten- added check for THR near 90 deg. 

C  IND  0=O.K.  1=POINT OFF PLANET
C  M  1=DIRECT  2=INVERSE

C DATA
C  1    XC  SPECIAL SAMPLE POINT
C  2    ZC  SPECIAL LINE POINT
C  3    TH  SPECIAL LATITUDE
C  4    TH1  LATITUDE OF SPECIAL PARALLEL OR SPECIAL OBLIQUE LONGITUDE
C  5    TH2  LATITUDE OF SPECIAL PARALLEL
C  6    LAM SPECIAL LONGITUDE    WEST
C  7    F  SCALE  (KM/PIXEL)
C  8    CAS  +1 IF VISIBLE POLE IS N.   -1 IF VISIBLE POLE IS S.
C       M  M=2  LINE,SAMPLE TO LAT,LONG   (INVERSE)
C       M  M=1  LAT,LONG TO LINE,SAMP  (DIRECT)
C  25   RP  POLAR RADIUS  (KM)
C  26   RE  EQUATORIAL RADIUS  (KM)
C  9    PSI   NORTH ANGLE
C
C  ******  ANGLES IN DATA() IN DEGREES  ******
C  ******  LAT,LONG IN RADIANS          ******
C  ******  ALL LATITUDES PLANETOCENTRIC ******
C  ******  ALL LONGITUDES WEST          ******

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION LAM,LAMR,LATR,LONGR
      DOUBLE PRECISION K1,K2,K3,K3SQRT,DIF1(3),DIF2(3),LAMBAR,NORTH,L
      DOUBLE PRECISION LAT8,LONG8
      REAL DATA(40),LINE,SAMPLE,LAT,LONG
      DATA SMALL/1D-8/

C  SPECIAL FUNCTIONS DEFINED

C  GEOCENTRIC RADIUS
      GCR(RPP,REP,THR)=RPP*REP/DSQRT(RPP*RPP*DCOS(THR)**2+
     *   REP*REP*DSIN(THR)**2)

C  GEODETIC LATITUDE
      PHIG(PI,RPP,REP,THR)=PI/2.D0-DABS(DATAN(-RPP*RPP/(REP*REP)*
     *1.D0/DTAN(THR)))

      PI=3.141592653589793D0
      PI2=2.0D0*PI
      D2R=pi/180.d0
      IND=0

C  CONVERT ANGLES AND DIMENSIONS TO RADIANS AND PIXELS RESPECTIVELY
C  AND R*4 DATA ITEMS TO R*8

      XC=DATA(1)
      ZC=DATA(2)
      TH=DATA(3)
      THR=TH*D2R
      IF(THR.EQ.0.D0)THR=SMALL		! in case Center_lat=0 
      THR0=THR
      LAM=DATA(6)
      LAMR=LAM*D2R
      F=DATA(7)
      PSI=DATA(9)
      PSIR=PSI*D2R
      RP=DATA(25)
      RPP=RP/F
      RE=DATA(26)
      REP=RE/F
      IF (M.NE.2) THEN
	LAT8=LAT
	LONG8=LONG
	LATR=LAT8
C	IF(LATR.EQ.0.D0)LATR=0.0000001d0	! ?? (LWK)
	LONGR=LONG8
      ENDIF

      IF(M.EQ.2) GO TO 100

C  DIRECT
      R=GCR(RPP,REP,LATR)
      PHI=PHIG(PI,RPP,REP,THR)
      PHI=DSIGN(PHI,THR)
      X11=-R*DCOS(LATR)*DSIN(LONGR-LAMR)
      Z11=R*(DSIN(PHI)*DCOS(LATR)*DCOS(LONGR-LAMR)
     *-DCOS(PHI)*DSIN(LATR))
      X1=X11
      Z1=Z11-GCR(RPP,REP,THR)*DSIN(PHI-THR)
      SAMPLE=X1*DCOS(PSIR)-Z1*DSIN(PSIR)+XC
      LINE=X1*DSIN(PSIR)+Z1*DCOS(PSIR)+ZC

C  BACK-OF-PLANET TEST
C  29-SEP-1985 JAM LIFTED FROM LUMLLP
      C1=COS(THR)
      C2=COS(PI2-LAMR)
      C3=SIN(THR)
      C4=SIN(PI2-LAMR)
      CA=COS(LATR)
      CO=COS(PI2-LONGR)
      SA=SIN(LATR)
      SO=SIN(PI2-LONGR)
      CE=CA*CO*C1*C2+CA*SO*C1*C4+SA*C3	! COSINE EMISSION ANGLE
C  RETURNS .TRUE. IF POINT LAT,LON IS ON BACK OF PLANET W.R.T. TH,LAM
      IF(CE.LT.0.)IND=1
      RETURN

100   CONTINUE
C  INVERSE
      RLAT=SAMPLE-XC
      RLON=LINE-ZC
      IF(RLAT.NE.0.0.OR.RLON.NE.0.0) GO TO 220
      LAT=THR
      LONG=LAMR
      RETURN
220   CONTINUE
      CPHI=TH
      CPSI=LAM
      NORTH=PSI
      if (dabs(thr).gt.((90.d0-SMALL)*d2r))
     & thr=dsign((90.d0-SMALL)*d2r,thr)
      SINLAT=DSIN(THR)
      COSLAT=DCOS(THR)
      SINLON=DSIN(LAMR)
      COSLON=DCOS(LAMR)
      SINNOR=DSIN(PSIR)
      COSNOR=DCOS(PSIR)
      FL=RP
      REQ=RE
      SLCCPC=SINLAT*COSLON
      SLCSPC=SINLAT*SINLON
      SCPCSL=SINLON*COSLON*SINLAT
      SCPCCL=SINLON*COSLON*COSLAT
      CLCC2P=COSLAT*COSLON*COSLON
      CLCS2P=COSLAT*SINLON*SINLON
      SLCC2P=SINLAT*COSLON*COSLON

C     CALC ANGLE LAMBDA BAR
      RPSQ=FL
      RPSQ=RPSQ*RPSQ
      RESQ=REQ
      RESQ=RESQ*RESQ
      LAMBAR=((COSLAT*COSLAT/RESQ+SINLAT*SINLAT/RPSQ)/
     &DSQRT((COSLAT*COSLAT/(RESQ*RESQ)+SINLAT*SINLAT/(RPSQ*RPSQ))))
      IF(LAMBAR.gt.1.D0)LAMBAR=1.D0
      LAMBAR=DACOS(LAMBAR)
      LAMBAR=((CPHI))*D2R+LAMBAR
      SINLAM=DSIN(LAMBAR)
      COSLAM=DCOS(LAMBAR)
      L=(CPHI)*D2R-LAMBAR
      SINL=DSIN(L)
      COSL=DCOS(L)

C     GET RADIUS OF PLANET AT C.P.
      RCP= GCR(RPP,REP,THR)

C     CONVERT FROM PIXELS TO KM
      RCP=F*RCP

C     CALC.ANGLE BETWEEN UP AND POINT OF INTEREST
C     IN PLANE OF PROJECTION SUBTENDED AT CENTER OF PROJECTION
      DELX=RLAT
      XDEL=DELX
      DELZ=RLON
      ZDEL=DELZ
      APOIUP=DATAN2(-XDEL,-ZDEL)

C     CALC.SIN AND COS OF THE ANGLE BETWEEN THE DIRECTION OF
C     NORTH IN THE IMAGE PLANE AND THE POINT OF INTEREST SUBTENDED AT
C     THE CENTER OF PROJECTION
      ADEL=((NORTH)*D2R)+APOIUP
      SINDEL=DSIN(ADEL)
      COSDEL=DCOS(ADEL)
      IF(SINDEL.EQ.1.0D0)COSDEL=0.0D0
      IF(SINDEL.EQ.-1.0D0)COSDEL=0.0D0

C     CALC.DISTANCE OF POINT OF INTEREST FROM
C     CENTER OF PROJECTION IN PLANE OF PROJECTION
C     AT TRUE SCALE
      DD=(F)*DSQRT((XDEL*XDEL)+(ZDEL*ZDEL))

C     CHECK WHETHER POINT OF INTEREST IS OFF PLANET
      IF(REQ.LT.DD) GO TO 999

C     CALC.COEFFIEIENTS FOR TWO PLANES NORMAL
C     TO PLANE OF PROJECTION.

C     PLANE 1 - NORMAL TO LINE CONNECTION CENTER OF PROJECTION
C     AND POINT OF INTEREST
C     PLANE 2 - CONTAINS LINE CONNECTION CENTER OF
C     PROJECTION AND POINT OF INTEREST

C     PLANE 1 A1*X+B1*Y+C1*Z+D1=0
C     PLANE 2 A2*X+B2*Y+C2*Z=0

      A1=-SINDEL*SINLON-COSDEL*COSLON*SINLAM
      B1=-SINDEL*COSLON+COSDEL*SINLON*SINLAM
      C1=COSDEL*COSLAM
      D1=-DD*SINDEL*SINDEL+RCP*COSDEL*SINLAM*COSLAT
     &-RCP*SINLAT*COSLAM*COSDEL-DD*COSDEL*COSDEL*SLCC2P*SINLAM
     &-DD*COSDEL*COSDEL*COSLAM*COSLAM
     &-DD*SINLAM*SINLAM*COSDEL*COSDEL*SINLON*SINLON
      A2=-COSDEL*SINLON*COSL+SINDEL*SLCCPC
      B2=-COSDEL*COSLON*COSL-SINDEL*SLCSPC
      C2=-COSLAT*SINDEL

C     CALCULATE PARAMETRIC VARIABLES IN
C     SIMULTANEOUS SOLN.OF PLANE 1,PLANE 2,AND SPHEROID

      ALPHA=A2*C1-A1*C2
      BETA=A2*B1-A1*B2
      GAMMA=B1*C2-B2*C1
      DELTA=C1*B2-B1*C2

C     CALCULATE X COORDINATE

C     EQUATION IS X=K1+OR-K2*SQRT(K3)

      ALPHSQ=ALPHA*ALPHA
      BETASQ=BETA*BETA
      GAMMSQ=GAMMA*GAMMA
      DELTSQ=DELTA*DELTA
      D1SQ=D1*D1
      C2SQ=C2*C2
      B2SQ=B2*B2
      GRESQ=GAMMSQ*RESQ
      DRPSQ=DELTSQ*RPSQ
      Z1=DRPSQ*(ALPHSQ+GAMMSQ)+BETASQ*GRESQ
      K1=((ALPHA*C2*D1*DRPSQ)+(BETA*B2*D1*GRESQ))/Z1
      K2=(GAMMA*DELTA*FL)/Z1
      K3=2.D0*ALPHA*C2*BETA*B2*RESQ
      K3=K3+(-C2SQ*DRPSQ-B2SQ*GRESQ-ALPHSQ*B2SQ*RESQ-BETASQ*RESQ*C2SQ)
      K3=K3*D1SQ
      K3=K3+(GRESQ*DRPSQ+DRPSQ*RESQ*ALPHSQ+RESQ*BETASQ*GRESQ)
      IF(K3.LT.0.D0) GO TO 999
      K3SQRT=DSQRT(K3)
      Z1=K2*K3SQRT
      X1=K1+Z1
      X2=K1-Z1

C     MAKE THE BACK OF PLANET TEST

      Y1=-D1*C2
      Y2=Y1
      Y1=(Y1+ALPHA*X1)/GAMMA
      Y2=(Y2+ALPHA*X2)/GAMMA
      Z1=(-B2*D1+BETA*X1)/DELTA
      Z2=(-B2*D1+BETA*X2)/DELTA

C     (X1,Y1,Z1) IS VECTOR P01
C     (X2,Y2,Z2) IS VECTOR P02
C     PCP IS VECTOR FROM PLANET CENTER TO CENTER OF PROJECTION
C     FIND WHICH VECTOR HAS MINIMUM LENGTH, P01-PCP  OR  P02-PCP

      PCPX=RCP*COSLAT*COSLON
      PCPY=-RCP*COSLAT*SINLON
      PCPZ=RCP*SINLAT
      DIF1(1)=X1-PCPX
      DIF1(2)=Y1-PCPY
      DIF1(3)=Z1-PCPZ
      DIF2(1)=X2-PCPX
      DIF2(2)=Y2-PCPY
      DIF2(3)=Z2-PCPZ
      RAD1=DIF1(1)*DIF1(1)+DIF1(2)*DIF1(2)+DIF1(3)*DIF1(3)
      RAD2=DIF2(1)*DIF2(1)+DIF2(2)*DIF2(2)+DIF2(3)*DIF2(3)
      IF(RAD1.GT.RAD2) GO TO 210
C     POINT 1 IS VALID
      RLON=PI2-DATAN2(Y1,X1)
      RLON=DMOD(RLON+PI2,PI2)
      RLAT=(DATAN(DABS(Z1)/DSQRT(X1*X1+Y1*Y1)))
      RLAT=DSIGN(RLAT,Z1)
      LAT=RLAT
      LONG=RLON
      RETURN
C     POINT 2 IS VALID
210   RLON=PI2-DATAN2(Y2,X2)
      RLON=DMOD(RLON+PI2,PI2)
      RLAT=(DATAN(DABS(Z2)/DSQRT(X2*X2+Y2*Y2)))
      RLAT=DSIGN(RLAT,Z2)
      LAT=RLAT
      LONG=RLON
      RETURN
999   CONTINUE
      IND=1
      RETURN

      END
$ Return
$!#############################################################################
