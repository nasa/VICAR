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
