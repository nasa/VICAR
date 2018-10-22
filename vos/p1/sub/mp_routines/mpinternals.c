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

