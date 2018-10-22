				
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
