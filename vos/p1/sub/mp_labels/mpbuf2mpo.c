				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_routines.h"

/********************************************************************/
/*
 * Function mpBuf2Mpo:
 *
 * routine to convert an "old" (pre-1993) VICAR map buffer
 * to the new MP map object;  this is the "C" interface
 *
 *  14sep93  -lwk-  intial version
 *  21oct93  -lwk-  replaced union in argument list with casts to
 *		new arrays
 *  19jan94  -lwk-  added +1 to LINE/SAMPLE_PROJECTION_OFFSET
 *		conversion
 *  16feb94  -lwk-  added items B_AXIS_RADIUS, MAP_RESOLUTION
 *  24feb94  -lwk-  added SPHERICAL_/CARTESIAN_AZIMUTH for all cases
 *  09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
 *		to MP, since it's public
 *  13mar95  -jfm-  changed mapping to Cylindrical Equal-Area
 *		projection such that XC or SAMPLE_PROJECTION_OFFSET
 *		is set to 0.0; this required modification of cases
 *		in switch statement for most all projections.
 *		Code modified to use one main switch statement and
 *		return status flag set to variable status (FR 85803)
 *		Added mpSetValues for projections that have 
 *		map resolution in the MAPXXX label.
 *  12jul95  -lwk-  change 'break' to 'return' for invalid projection
 *  28nov95  -lwk-  added POSITIVE_LONGITUDE_DIRECTION=WEST
 *  15oct96  -lwk-  added COORDINATE_SYSTEM_NAME=PLANETOCENTRIC
 *  22jan97  -lwk-  fixed B_AXIS_RADIUS setting in triaxial case
 *   1dec97  -lwk-  Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE
 *  17dec99  -lwk-  removed MAP_RESOLUTION item, as this is now done
 *		automatically in mpsetvalues when MAP_SCALE is set (it was
 *		causing an error for some projections, as it was called
 *		before RADIUS was set)
 */

int mpBuf2Mpo( void *buf, MP mp)
{
  int status;
  int *int_buf = (int *)buf;
  float *float_buf = (float *)buf;
  double x;

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
 *	RDATA(10)= MAP RESOLUTION (PIXELS/DEG) for proj. codes 6,9,10,15
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
 *       RDATA(31) = s/c latitude             
 *       RDATA(32) = s/c longitude            
 *       RDATA(33) = line                     
 *       RDATA(34) = sample                   
 *       RDATA(35) = North angle              
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
 *	determined by the other Perspective items and can be computed
 *	from them using VICAR routine MOMATI.
 *
 *  Since these elements are redundant, they are simply ignored in this
 *  routine.
 *
 *  Note that in the MAP buffer, all longitudes are West and all latitudes
 *  are planetocentric!  (In the MP routines, the reverse is the default in
 *  both cases.)
 */

  switch (int_buf[38]) {

    case 7:		/* Image and Object Space images not supported */
    case 8:
    default:

      status = mpINVALID_PROJECTION;
      return status;

    case 16:    /* Point Perspective projection - not a map */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, 
      mpPOINT_PERSPECTIVE,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFOCAL_LENGTH, (double)float_buf[26], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpOPT_AXIS_INTERCEPT_LINE, 
      (double)float_buf[27],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpOPT_AXIS_INTERCEPT_SAMPLE, 
      (double)float_buf[28],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFOCAL_PLANE_SCALE, (double)float_buf[29], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSUB_SPACECRAFT_LATITUDE, 
      (double)float_buf[30], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSUB_SPACECRAFT_LONGITUDE, 
      (double)float_buf[31], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpPLANET_CENTER_LINE, (double)float_buf[32],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpPLANET_CENTER_SAMPLE, (double)float_buf[33],
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpNORTH_ANGLE, (double)float_buf[34], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpTARGET_CENTER_DISTANCE, 
      (double)float_buf[37], NULL);
      if (status != mpSUCCESS) return status;
      break;

      /* Standard map projections */

    case 1:     /*     Polar Orthographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpPOLAR_ORTHOGRAPHIC,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 2:     /*     Oblique Orthographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpOBLIQUE_ORTHOGRAPHIC,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[8], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 3:     /*    Polar Stereographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpPOLAR_STEREOGRAPHIC,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 4:     /*     Oblique Stereographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpOBLIQUE_STEREOGRAPHIC,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[8], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 5:     /*     Lambert Conformal Conic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpLAMBERT_CONFORMAL, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFIRST_STANDARD_PARALLEL, (double)float_buf[3],
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSECOND_STANDARD_PARALLEL,
       (double)float_buf[4],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 6:     /*    Mercator            */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpMERCATOR, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 9:     /*     Cylindrical Equal Area        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpNORMAL_CYLINDRICAL,NULL);
      if (status != mpSUCCESS) return status;

      /* Set FIRST_STANDARD_PARALLEL to zero.
       * Previous map projections of Cylindrical Equal Area assumed 
       * constant scale at the equator (standard parallel defined at the
       * equator.
       */
      status = mpSetValues( mp, mpFIRST_STANDARD_PARALLEL, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, 0.0, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 10:     /*    Equidistant Cylindrical        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpSIMPLE_CYLINDRICAL,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFIRST_STANDARD_PARALLEL, (double)float_buf[3],
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 11:     /*    Oblique Equidistant Cylindrical        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE,
       mpOBLIQUE_SIMPLE_CYLINDRICAL, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[3], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 12:     /*    Sinusoidal            */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpSINUSOIDAL, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 13:     /*    Oblique sinusoidal        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpOBLIQUE_SINUSOIDAL,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[3], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 14:     /*     Mollweide            */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpMOLLWEIDE, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH,(double)0 , NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 15:     /*    Transverse Mercator        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpTRANSVERSE_MERCATOR,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

  }
	
  /* Items common to all projections */

  status = mpSetValues( mp, mpA_AXIS_RADIUS, (double)float_buf[25], NULL);
  if (status != mpSUCCESS) return status;

  /* word 36 holds the short equatorial axis for the triaxial case in
   * routine persp_proj, but for other cases this is zero ... */
  if (float_buf[36]>0.0001) {		/* (may be some garbage there) */
    status = mpSetValues( mp, mpB_AXIS_RADIUS, (double)float_buf[36], NULL);
  }
  else {
    status = mpSetValues( mp, mpB_AXIS_RADIUS, (double)float_buf[25], NULL);
  }
  if (status != mpSUCCESS) return status;

  status = mpSetValues( mp, mpC_AXIS_RADIUS, (double)float_buf[24], NULL);
  if (status != mpSUCCESS) return status;
 
/* 
  status = mpSetValues( mp, mpBODY_LONG_AXIS,(double)float_buf[35], NULL);
  if (status != mpSUCCESS) return status;
*/

  /* old VICAR always assumes West Longitudes and Planetocentric latitudes: */
  status = mpSetValues( mp, mpPOSITIVE_LONGITUDE_DIRECTION, "WEST", NULL);
  if (status != mpSUCCESS) return status;
  status = mpSetValues( mp, mpCOORDINATE_SYSTEM_NAME, "PLANETOCENTRIC", NULL);
  if (status != mpSUCCESS) return status;

  /* 
   * The remaining items are common to all standard map projections:
   */
  if ( int_buf[38] != 16 ) {

    /* PDS offsets are with respect to pixel (1,1), while VICAR's are to (0,0)*/
    x = float_buf[1] - 1.0;
    status = mpSetValues( mp, mpLINE_PROJECTION_OFFSET, x, NULL);
    if (status != mpSUCCESS) return status;
    status = mpSetValues( mp, mpCENTER_LONGITUDE, (double)float_buf[5], NULL);
    if (status != mpSUCCESS) return status;
    status = mpSetValues( mp, mpMAP_SCALE, (double)float_buf[6], NULL);
    if (status != mpSUCCESS) return status;

    /* note this item is functionally identical to CARTESIAN_AZIMUTH
     * in all VICAR map projections, so we use the latter and set
     * this to zero: */
    status = mpSetValues( mp, mpSPHERICAL_AZIMUTH, (double)0, NULL);
    if (status != mpSUCCESS) return status;
  }

  return status;
}
