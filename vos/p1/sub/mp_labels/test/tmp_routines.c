#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

/**********************************************************************
 
Test Program TMP_ROUTINES

Program calls mpGetPar which calls mpInit to allocate memory 
for a map projection data object and then sets values in the
data object based on values passed by the application programs
parameter list. Then mpll2xy and mpxy2ll are called to perform
point transformations.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		May 1994
Revision history:	
			August 1994	(JFM)
		
			Revised test program to simplify output
			of points to a tabular form, ending us of
			VERBOSE/DEBUG mode of output.	
*/
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

MP mp_obj;

zvmessage("***************************************************"," ");
zvmessage("\n\tTest of MP routines in C\n"," ");
zvmessage("***************************************************\n"," ");

/*

Define user parameters to be retrieved from PDF file.

*/

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

strcpy(pdf_parms[17],"TGT_DIST");
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

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
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
/*

Convert input path for PCK file to absolute path.

*/
status = zvfilename( PCKfile, PCKpath, 0 );
ABENDif( status < VICARrtlSUCCESS );

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKpath );
if ( status < 0 )
	{
	zvmessage("Error in mpgetpar call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 )
	{
	zvmessage("Error in mpGetKeywords call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpSetDebugFlag( FALSE );
ABENDif( status<mpSUCCESS );

for ( i=0; i<number_keywords; i++ )
     	switch ( types[i] )	{

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
		break;	}


/*

Print output banner for map transformation

*/

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tTransformation results:"," ");
zvmessage(" "," ");

/*

Determine input latitude and longitude type specified as input.

*/

status = zvp("LL_TYPE",lat_lon_type,&count);
ABENDif( status < mpSUCCESS );

if ( strcmp(lat_lon_type,"PLANETOCENTRIC") == 0 )
	{
	ll_type = 1;
	zvmessage("\n\tPlanetocentric lat/lon pairs\n"," ");
	}
if ( (strcmp(lat_lon_type,"PLANETOGRAPHIC") == 0) ||
     (strcmp(lat_lon_type,"PLANETODETIC") == 0) )
	{
	ll_type = 2;
	zvmessage("\n\tPlanetographic lat/lon pairs\n"," ");
	}
if ( strcmp(lat_lon_type,"SNYDER_DEFINED") == 0 )
	{
	ll_type = 3;
	zvmessage("\n\tSnyder-defined lat/lon pairs\n"," ");
	}

/*

TRANSFORMATIONS: forward and inverse

*/

zvmessage(" ",0);
zvmessage(" Table of Point Transformations",0);
zvmessage(" ",0);

/*

Get latitude and longitude array from parameter values.

*/

status = zvp("LATITUDES",latitudes,&lat_count);
ABENDif( status < mpSUCCESS );

status = zvp("LONGITUDES",longitudes,&lon_count);
ABENDif( status < mpSUCCESS );

if ( lat_count > 0 )
	{
	zvmessage(" (Lat,Lon) -> (Line,Sample) -> (Lat',Lon')",0);
	zvmessage(" ",0);
	}
else
	{
	zvmessage(" (Line,Sample) -> (Lat,Lon) -> (Line',Sample')",0);
	zvmessage(" ",0);
	}

for( i=0; i<lat_count; i++ )
	{
	latitude = (double) latitudes[i];
	longitude = (double) longitudes[i];

	status = mpll2xy( mp_obj,&line,&sample,latitude,longitude,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpLL2XY error on lat,lon = (%e,%e)",
			latitude,longitude);
		zvmessage(string,0);
		break;
		}
	status = mpxy2ll( mp_obj,line,sample,&new_lat,&new_lon,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpXY2LL error on line,sample = (%e,%e)",
			line,sample);
		zvmessage(string,0);
		break;
		}

	sprintf(string," (%8.3f,%8.3f) -> (%8.3f,%8.3f) -> (%8.3f,%8.3f)",
		latitude,longitude,line,sample,new_lat,new_lon);
	zvmessage(string,0);
	}

/*

Get line and sample array from parameter values.

*/

status = zvp("LINES",lines,&line_count);
ABENDif( status < mpSUCCESS );

status = zvp("SAMPLES",samples,&samp_count);
ABENDif( status < mpSUCCESS );

for( i=0; i<line_count; i++ )
	{
	line = (double) lines[i];
	sample = (double) samples[i];

	status = mpxy2ll( mp_obj,line,sample,&latitude,&longitude,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpXY2LL error on line,sample = (%e,%e)",
			line,sample);
		zvmessage(string,0);
		break;
		}

	status = mpll2xy( mp_obj,&new_line,&new_samp,
			latitude,longitude,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpLL2XY error on lat,lon = (%e,%e)",
			latitude,longitude);
		zvmessage(string,0);
		break;
		}

	sprintf(string," (%8.3f,%8.3f) -> (%9.3f,%9.3f) -> (%8.3f,%8.3f)",
		line,sample,latitude,longitude,new_line,new_samp);
	zvmessage(string,0);
	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test of MP routines in C\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
