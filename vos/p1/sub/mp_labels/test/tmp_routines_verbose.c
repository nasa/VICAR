#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

/**********************************************************************
 
Test Program TMP_ROUTINES_VERBOSE

Program calls mpGetPar which calls mpInit to allocate memory 
for a map projection data object and then sets values in the
data object based on values passed by the application programs
parameter list. Then mpll2xy and mpxy2ll are called to perform
point transformations.  DEBUG/VERBOSE option is used to show
internal calculations.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		May 1994
Revision history:	Original

*/
void main44()
{
int 	i,j,k;
int	count;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];
int	ll_type;
int	lat_count,lon_count;
char 	lat_lon_type[20];

double	double_value;
double	lines[MAX_LATS_LONS];
double	samples[MAX_LATS_LONS];
float	latitudes[MAX_LATS_LONS];
float	longitudes[MAX_LATS_LONS];
double	latitude;
double	longitude;

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

pdf_parms[28][0] = '\0';
pds_keys[28][0] = '\0';

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
status = zvp("PCK_PATH",PCKpath,&count);
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
status = zvfilename( PCKfile, PCKpath, 0);
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

status = mpSetDebugFlag( TRUE );
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

Get latitude and longitude array from parameter values.

*/

status = zvp("LATITUDES",latitudes,&lat_count);
ABENDif( status < mpSUCCESS );

status = zvp("LONGITUDES",longitudes,&lon_count);
ABENDif( status < mpSUCCESS );

if ( lat_count < lon_count )
	count = lat_count;
else
	count = lon_count;

/*

FORWARD TRANSFORMATION

*/

for( i=0; i<count; i++ )
	{
	latitude = (double) latitudes[i];
	longitude = (double) longitudes[i];

	zvmessage("\n******************************************\n"," ");
	zvmessage("\t(LAT,LON) -> (X,Y)"," ");
	zvmessage("\n\twhere"," ");
	sprintf(string,"\n\t(LAT,LON) = (%5.3f,%6.3f)\n",
		latitude,longitude);
	zvmessage(string," ");

	status = mpll2xy( mp_obj,&lines[i],&samples[i],
			latitude,longitude,ll_type );

	zvmessage("\n\tTransform completed."," ");
	zvmessage("\n******************************************\n"," ");
	}
/*

INVERSE TRANSFORMATION

*/

for( i=0; i<count; i++ )
	{
	zvmessage("\n******************************************\n"," ");
	zvmessage("\t(X,Y) -> (LAT,LON)"," ");
	zvmessage("\n\twhere"," ");
	sprintf(string,"\n\t(X,Y) = (%7.3f,%7.3f)\n",
		samples[i],lines[i]);
	zvmessage(string," ");

	status = mpxy2ll( mp_obj,lines[i],samples[i],
			&latitude,&longitude,ll_type );

	zvmessage("\n\tTransform completed."," ");
	zvmessage("\n******************************************\n"," ");
	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test of MP routines in C\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
