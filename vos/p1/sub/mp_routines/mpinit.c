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
