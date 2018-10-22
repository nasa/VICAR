/* TMP_BUF2MPO:
 *
 * use code from tmp_routines.c to construct an MP object, then
 * call mpmpo2buf, then mpbuf2mpo
 *
 *  17dec99  -lwk-
 */

#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

void main44()
{
int 	i,j,k;
int	count;
int	def;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];
int	ll_type;
int	lat_count,lon_count;
int	line_count,samp_count;
char 	lat_lon_type[20];

double	double_value;
float	lines[MAX_LATS_LONS];
float	samples[MAX_LATS_LONS];
float	latitudes[MAX_LATS_LONS];
float	longitudes[MAX_LATS_LONS];

double	latitude;
double	longitude;
double	line;
double	sample;

double	new_lat;
double	new_lon;
double	new_line;
double	new_samp;

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];

union {
  struct { float reals[38]; int ints[2];} map1;
  struct { double dbls[12]; float reals[14]; int ints[2];} map2;
} mapbuf;

MP mp_obj;

/* Define user parameters to be retrieved from PDF file. */

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_NAME);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[6],"RESOLUTION");
strcpy(pds_keys[6],mpMAP_RESOLUTION);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);

strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH);

strcpy(pdf_parms[17],"RANGE");
strcpy(pds_keys[17],mpTARGET_CENTER_DISTANCE);

strcpy(pdf_parms[18],"AXIS_LINE");
strcpy(pds_keys[18],mpOPT_AXIS_INTERCEPT_LINE);

strcpy(pdf_parms[19],"AXIS_SAMPLE");
strcpy(pds_keys[19],mpOPT_AXIS_INTERCEPT_SAMPLE);

strcpy(pdf_parms[20],"FOCAL_SCALE");
strcpy(pds_keys[20],mpFOCAL_PLANE_SCALE);

strcpy(pdf_parms[21],"SUB_SPACE_LAT");
strcpy(pds_keys[21],mpSUB_SPACECRAFT_LATITUDE);

strcpy(pdf_parms[22],"SUB_SPACE_LON");
strcpy(pds_keys[22],mpSUB_SPACECRAFT_LONGITUDE);

strcpy(pdf_parms[23],"CENT_LINE");
strcpy(pds_keys[23],mpPLANET_CENTER_LINE);

strcpy(pdf_parms[24],"CENT_SAMPLE");
strcpy(pds_keys[24],mpPLANET_CENTER_SAMPLE);

strcpy(pdf_parms[25],"N_ANGLE");
strcpy(pds_keys[25],mpNORTH_ANGLE);

strcpy(pdf_parms[26],"LONG_AXIS");
strcpy(pds_keys[26],mpBODY_LONG_AXIS);

strcpy(pdf_parms[27],"LL_TYPE");
strcpy(pds_keys[27],mpCOORDINATE_SYSTEM_NAME);

strcpy(pdf_parms[28],"TGTBOD");
strcpy(pds_keys[28],mpTARGET_BODY);

pdf_parms[29][0] = '\0';
pds_keys[29][0] = '\0';

/* Set user parameters */

status = zvparm("PCK_PATH",PCKpath,&count,&def,1,0 );
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}
zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);
PCKfile[lengthes[0]]='\0';

/* Convert input path for PCK file to absolute path. */
status = zvfilename( PCKfile, PCKpath, 0 );
ABENDif( status < VICARrtlSUCCESS );

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKpath );
if ( status < 0 ) zmabend("Error in mpgetpar call");

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 ) zmabend("Error in mpGetKeywords call");

status = mpSetDebugFlag( FALSE );
ABENDif( status<mpSUCCESS );

for ( i=0; i<number_keywords; i++ )
  switch ( types[i] )  {

  case mpCHAR:

    status = mpGetValues( mp_obj,keys[i],string_value,"" );
    ABENDif( status < mpSUCCESS );
    
    sprintf(string,"KEYWORD %s equals %s",keys[i],string_value);
    zvmessage(string," ");
    
    break;

  case mpDBLE:

    status = mpGetValues( mp_obj,keys[i],&double_value,"" );
    ABENDif( status < mpSUCCESS );
    
    sprintf(string,"KEYWORD %s equals %4.3e",keys[i],double_value);
    zvmessage(string," ");

    break;

  default:

    zvmessage("PDS KEY of unacceptable data type"," ");
    break;

  }

status = mpMpo2Buf( mp_obj, &mapbuf );
if( status != mpSUCCESS ) zvmessage(" MPO2BUF failed!", "");

zvmessage( " Old MAP Buffer:", "");
for (i=0; i<39; i++) {
  if (i<23) {
    if (mapbuf.map1.ints[0]==16) {
      j = i/2;
      if (2*j != i) continue;
      else sprintf( string, " BUF(%d:%d) = %f", i+1, i+2, mapbuf.map2.dbls[j]);
    }
    else sprintf( string, " BUF(%d) = %f", i+1, mapbuf.map1.reals[i]);
  }
  else if (i<38)
    sprintf( string, " BUF(%d) = %f", i+1, mapbuf.map1.reals[i]);
  else
    sprintf( string, " BUF(%d) = %d", i+1, mapbuf.map1.ints[i-38]);
  zvmessage( string, "");
}

status = mpBuf2Mpo( &mapbuf.map1, mp_obj);

if( status == mpSUCCESS ) zvmessage(" BUF2MPO succeeded!", "");
else zvmessage(" BUF2MPO *failed* !!", "");

mpFree( mp_obj );

}
