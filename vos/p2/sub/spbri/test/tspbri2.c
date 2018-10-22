#include <stdio.h>
#include "vicmain_c"
#include <math.h>

#define MAX_INPUTS	20
#define MAX_LENGTH	80

#define CHECKif(x) if(x){ zvmessage("ERROR IN TEST ROUTINE"," "); zabend();}

/**********************************************************************
 
Test Program for C Bridges

This program tests ERRACT, ET2UTC, LDPOOL, UTC2ET and SCS2E bridge routines.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		February 1994
Revision history:	June 24, 1994	JFM
			
			Null terminator added to SCLK_time string to
			avoid failure on the DEC ALPHA. (FR 85089)

*/

void main44()
{
int	count;
int	minimum_count;
int 	i,j,k;
int	time_indices[MAX_LENGTH];
int	time_lengths[MAX_LENGTH];
int	status;
int	precision[MAX_INPUTS];
int	spacecraft_codes[MAX_INPUTS];
char	string[300];
char 	leapseconds_kernel[100],sclk_kernel[100],pc_kernel[100];
char	kernel_path[100];
char    format[MAX_INPUTS*2], cformat[2];
char 	UTC_times[MAX_INPUTS*MAX_LENGTH],SCLK_times[MAX_INPUTS*MAX_LENGTH];
char	UTC_time[MAX_LENGTH],SCLK_time[MAX_LENGTH];
double  m_values[3];
double  ephemeris_time;
float	ephemeris_times[MAX_INPUTS];
double	double_value;

zvmessage("************************************************************"," ");
zvmessage("\n\tTest of Bridges for SPICE Routines: \n"," ");
zvmessage("\n\tERRACT, ET2UTC, LDPOOL, RTPOOL, UTC2ET, SCS2E\n"," ");
zvmessage("************************************************************"," ");

/* Set NAIF SPICE Toolkit Error Handling values */
zerract("SET","IGNORE");

/*

Get user parameters from PDF file.

*/
status = zvp("SCLK_KERNEL",sclk_kernel,&count);
CHECKif( status < 0 );

status = zvp("LEAP_KERNEL",leapseconds_kernel,&count);
CHECKif( status < 0 );

status = zvp("PC_KERNEL",pc_kernel,&count);
CHECKif( status < 0 );

status = zvp("ET_INPUTS",ephemeris_times,&minimum_count);
CHECKif( status < 0 );

status = zvp("SCLK_INPUTS",SCLK_times,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

zvsptr( SCLK_times,count,time_indices,time_lengths);

status = zvp("SPACECRAFT_CODE",spacecraft_codes,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

status = zvp("PRECISION",precision,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

status = zvp("FORMAT",format,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

/* Load Kernel pools */
zldpool(sclk_kernel);

zldpool(leapseconds_kernel);

zldpool(pc_kernel);

for( i=0; i<minimum_count; i++)
	{
	j = i+1;
	k = i*2;
        cformat[0] = format[k];
        cformat[1] = '\0';

	/* print output message for each set of user input */
	sprintf(string,"\n\n*** User input %d ***\n",j);
	zvmessage(string," ");

	/* get SCLK time from input parameter array of strings */
	time_indices[i]--;
	strncpy(SCLK_time,&SCLK_times[time_indices[i]],time_lengths[i]);
	SCLK_time[time_lengths[i]] = '\0';

	/* complete translations for various times */
	zscs2e(spacecraft_codes[i],SCLK_time,&ephemeris_time);
        zet2utc(ephemeris_time,cformat,precision[i],UTC_time);
        zutc2et(UTC_time,&double_value);

	/* print results of conversion */
	sprintf(string,"Ephemeris time for S/C code %d and SCLK %s : %f",
		spacecraft_codes[i],SCLK_time,ephemeris_time);
	zvmessage(string," ");

	sprintf(string,"Corresponding UTC time (precision %d, format %c) : %s",
               precision[i],cformat,UTC_time);
	zvmessage(string," ");
	sprintf(string,"Retranslated ET from UTC above : %f",double_value);
	zvmessage(string," ");
	}

/* Return a value from the leapseconds kernel */
zrtpool( "BODY401_RADII", &i, m_values, &status );
if ( status>=0 && i==3 )
	{
	zvmessage("\n\n*** RTPOOL Test ***\n"," ");
	sprintf(string,"Body 401 radii measures from kernel %s:",pc_kernel);
	zvmessage(string," ");
	sprintf(string,"\t( %f, %f, %f )\n",
		m_values[0],m_values[1],m_values[2] );
	zvmessage(string," ");
	}

zvmessage(" "," ");
zvmessage("************************************************************"," ");
zvmessage("\n\tEnd of test\n"," ");
zvmessage("************************************************************"," ");
zvmessage(" "," ");
}
