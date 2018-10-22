/* Program TMP_CREATE
 *
 *  3dec97 -lwk- hacked from pgm mptest, in order to test new MP keywords
 */

#include <math.h>
#include "mp_routines.h"
#include "vicmain_c"

#define NKWDS 32		/* number of keywords in TMP_CREATE.PDF */

void main44()
{
  int cnt, def, i, in, ival, ival1, len, nl, nl1, ns, ns1, nkeys, num, out,
   status, unit1, unit2;
  int type[mpNUMBER_OF_KEYWORDS], class[mpNUMBER_OF_KEYWORDS];
  double dval, dval1;
  char cval[133], cval1[133], msg[133],
   keylist[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
  MP mp_obj;

  /* becuse TAE limits parameter names to 15 chars, we must keep track
   * of both PDF and true names ... */
  char pdfkwds[NKWDS][17] = { "MAP_SCALE", "MAP_RESOLUTION", "MAP_PROJECTION",
   "POSITIVE_LONGIT", "CENTER_LATITUDE", "CENTER_LONGITUD",
   "SPHERICAL_AZIMU", "CARTESIAN_AZIMU", "LINE_PROJECTION",
   "SAMPLE_PROJECTI", "FIRST_STANDARD_", 
   "SECOND_STANDARD", "FOCAL_LENGTH", "FOCAL_PLANE_SCA", 
   "NORTH_ANGLE", "OPT_AXIS_LINE", "OPT_AXIS_SAMP", 
   "PLANET_CENTER_L", "PLANET_CENTER_S", "SUB_SCRAFT_LATI", 
   "SUB_SCRAFT_LONG", "SPACECRAFT_DIST", "TARGET_CENTER_D",
   "A_AXIS_RADIUS", "B_AXIS_RADIUS", "C_AXIS_RADIUS", "TARGET_NAME",
   "TARGET_BODY", "MINIMUM_LATITUD", "MAXIMUM_LATITUD", "MINIMUM_LONGITU",
   "MAXIMUM_LONGITU"};
  char kwdnams[NKWDS][mpMAX_KEYWD_LENGTH] = { "MAP_SCALE", "MAP_RESOLUTION",
   "MAP_PROJECTION_TYPE",
   "POSITIVE_LONGITUDE_DIRECTION", "CENTER_LATITUDE", "CENTER_LONGITUDE",
   "SPHERICAL_AZIMUTH", "CARTESIAN_AZIMUTH", "LINE_PROJECTION_OFFSET",
   "SAMPLE_PROJECTION_OFFSET", "FIRST_STANDARD_PARALLEL", 
   "SECOND_STANDARD_PARALLEL", "FOCAL_LENGTH", "FOCAL_PLANE_SCALE", 
   "NORTH_ANGLE", "OPT_AXIS_INTERCEPT_LINE", "OPT_AXIS_INTERCEPT_SAMPLE", 
   "PLANET_CENTER_LINE", "PLANET_CENTER_SAMPLE", "SUB_SPACECRAFT_LATITUDE", 
   "SUB_SPACECRAFT_LONGITUDE", "SPACECRAFT_DISTANCE", "TARGET_CENTER_DISTANCE",
   "A_AXIS_RADIUS", "B_AXIS_RADIUS", "C_AXIS_RADIUS", "TARGET_NAME",
   "TARGET_BODY", "MINIMUM_LATITUDE", "MAXIMUM_LATITUDE", "MINIMUM_LONGITUDE",
   "MAXIMUM_LONGITUDE"};
  int ktypes[NKWDS] = { 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1};	/* 0 = char, 1 = real */

  len = mpMAX_KEYWD_LENGTH+1;

  zvmessage(" program TMP_CREATE", "");

  zveaction("","");

  status = mpInit( &mp_obj);

  zvpcnt( "INP", &in);
  if (in>0) {
    zvunit( &unit1, "INP", 1, 0);
    zvopen( unit1, "OP", "READ", 0);
    status = mpLabelRead( mp_obj, unit1);
    zvget( unit1, "nl", &nl, "ns", &ns, 0);
  }
  else {nl=10; ns=10;}

  zvparm( "NL", &nl1, &cnt, &def, 1, 0);
  zvparm( "NS", &nl1, &cnt, &def, 1, 0);
  if (cnt) {nl=nl1; ns=ns1;}

  nkeys = 0;
  for (i=0; i<NKWDS; i++) {
    if (!ktypes[i]) {
      zvparm( pdfkwds[i], cval, &cnt, &def, 1, 0);
      if (cnt) {
	status = mpSetValues( mp_obj, kwdnams[i], cval, "");
	nkeys++;
      }
    }
    else {
      zvparmd( pdfkwds[i], &dval, &cnt, &def, 1, 0);
      if (cnt) {
	status = mpSetValues( mp_obj, kwdnams[i], dval, "");
	nkeys++;
      }
    }
  }

  /* first call with zero list to return num, to verify this works ... */
  status = mpGetKeywords( mp_obj, 0, &num, type, class);
  if (num!=nkeys) zmabend(" *** mpGetKeywords returned wrong count! ***");
  status = mpGetKeywords( mp_obj, keylist, &num, type, class);

  /* print out all the keywords: */
  for (i=0; i<num; i++) {
    if (type[i] == mpCHAR) {
      status = mpGetValues( mp_obj, keylist[i], cval1, "");
      strcpy( msg, keylist[i]);
      strcat( msg, " = ");
      strcat( msg, cval1);
      zvmessage( msg, "");
    }
    else if (type[i] == mpDBLE) {
      status = mpGetValues( mp_obj, keylist[i], &dval1, "");
      strcpy( msg, keylist[i]);
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", dval1);
      strcat( msg, cval1);
      zvmessage( msg, "");
    }
  }

	/* now open the file and write the map labels */
  zvpcnt( "OUT", &out);
  if (out>0) {
    zvunit( &unit2, "OUT", 1, 0);
    zvopen( unit2, "U_FORMAT", "BYTE", "OP", "write", "U_NL", nl, "U_NS", ns,
     0);
    status = mpLabelWrite( mp_obj, unit2, "HISTORY");
    status = mpLabelWrite( mp_obj, unit2, "PROPERTY");
    zvclose( unit2, 0);
  }
}
