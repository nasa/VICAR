
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
