

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
