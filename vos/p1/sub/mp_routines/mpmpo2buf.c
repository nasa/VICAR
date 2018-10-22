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

