#include <stdio.h>
#include "vicmain_c"

#define VICARrtlSUCCESS	0
#define	ABENDif(x) 	if (x) zabend()

/*

TSEARCV3

Test program for SEARCV3

*/

void main44()
{
int	i,j,k;
int	status;

int	count;
int	instance;
int	unit;
char	string[200];

int	int_data[40];
float	real_data[40];

/*

Clear arrays

*/

memset(int_data,0,160);
memset(real_data,0,160);

/*

Get unit number of file and open for reading of projection label

*/


status = zvpcnt("INP",&count);
ABENDif( status<VICARrtlSUCCESS );

for( instance=1; instance<=count; instance++ )
	{
	status = zvunit( &unit,"INP",instance,0 );
	ABENDif( status<VICARrtlSUCCESS );

	status = zvopen( unit,0 );
	ABENDif( status<VICARrtlSUCCESS );

	/*

	Call SEARCV3 to read IBM-style projection label

	*/

	status=searcv3_c( unit,real_data,int_data);
	ABENDif( status<VICARrtlSUCCESS );

	if( instance==2 )
		{
		zvmessage(" "," ");
		zvmessage(" *** 2nd Input *** "," ");
		zvmessage(" "," ");
		}

	switch( int_data[38] ) 	{

	case 1:	

	sprintf(string," PROJECTION TYPE: POLAR ORTHOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 2:

	sprintf(string," PROJECTION TYPE: OBLIQUE ORTHOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 3:	

	sprintf(string," PROJECTION TYPE: POLAR STEREOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 4:	

	sprintf(string," PROJECTION TYPE: OBLIQUE STEREOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 5:	

	sprintf(string," PROJECTION TYPE: LAMBERT (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 6:	

	sprintf(string," PROJECTION TYPE: MERCATOR (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 7:	

	sprintf(string," PROJECTION TYPE: IMAGE SPACE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	zvmessage(" *** Unsupported by this test program"," ");
	break;

	case 8:	

	sprintf(string," PROJECTION TYPE: OBJECT SPACE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	zvmessage(" *** Unsupported by this test program"," ");
	break;			

	case 9:	

	sprintf(string," PROJECTION TYPE: NORMAL CYLINDRICAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 10:	

	sprintf(string," PROJECTION TYPE: SIMPLE CYLINDRICAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 11:	

	sprintf(string,
		" PROJECTION TYPE: OBLIQUE SIMPLE CYLINDRICAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 12:	

	sprintf(string," PROJECTION TYPE: SINUSOIDAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 13:	

	sprintf(string," PROJECTION TYPE: OBLIQUE SINUSOIDAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 14:	

	sprintf(string," PROJECTION TYPE: MOLLWEIDE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 15:	

	sprintf(string," PROJECTION TYPE: TRANSVERSE MERCATOR (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 16:	

	sprintf(string," PROJECTION TYPE: PERSPECTIVE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;		
			}

	switch( int_data[38] ) 	{

	case 7:
	case 8:

	break;
	
	case 16:	

	zvmessage(" "," ");
	sprintf(string,"OM matrix\t| %f\t%f\t%f |",
		real_data[0],real_data[2],real_data[4]);
	zvmessage(string," ");
	sprintf(string,"\t\t| %f\t%f\t%f |",
		real_data[6],real_data[8],real_data[10]);
	zvmessage(string," ");
	sprintf(string,"\t\t| %f\t%f\t%f |",
		real_data[12],real_data[14],real_data[16]);
	zvmessage(string," ");
	zvmessage(" "," ");

	sprintf(string,"RS vector = { %f %f %f }",
		real_data[18],real_data[20],real_data[22]);
	zvmessage(string," ");
	zvmessage(" "," ");

        sprintf(string," FOCAL LENGTH (MM)           (27) = %f",real_data[26]);
	zvmessage(string," ");
	sprintf(string," OPTICAL AXIS LINE           (28) = %f",real_data[27]);
	zvmessage(string," ");
	sprintf(string," OPTICAL AXIS SAMPLE         (29) = %f",real_data[28]);
	zvmessage(string," ");
	sprintf(string," OBJECT SPACE SCALE          (30) = %f",real_data[29]);
	zvmessage(string," ");
	sprintf(string," SUB SPACECRAFT LATITUDE     (31) = %f",real_data[30]);
	zvmessage(string," ");
	sprintf(string," SUB SPACECRAFT LATITUDE     (32) = %f",real_data[31]);
	zvmessage(string," ");
	sprintf(string," SUB S/C OBJECT SPACE LINE   (33) = %f",real_data[32]);
	zvmessage(string," ");
	sprintf(string," SUB S/C OBJECT SPACE SAMPLE (34) = %f",real_data[33]);
	zvmessage(string," ");
	sprintf(string," NORTH ANGLE                 (35) = %f",real_data[34]);
	zvmessage(string," ");
	sprintf(string," PLANET ID                   (36) = %f",real_data[35]);
	zvmessage(string," ");
	sprintf(string," VIDICON SERIAL NUMBER       (37) = %f",real_data[36]);
	zvmessage(string," ");
	sprintf(string," DISTANCE TO PLANET          (38) = %f",real_data[37]);
	zvmessage(string," ");
	break;

	default:

	zvmessage(" "," ");
	for( i=0;i<9;i++ )
		{
		j = i + 1;
		sprintf(string," DATA                (%d) = %f",
			j,real_data[i]);
		zvmessage(string," ");
		}

	zvmessage(" "," ");
	sprintf(string," SAMPLE              (1) = %f",real_data[0]);
	zvmessage(string," ");
	sprintf(string," LINE                (2) = %f",real_data[1]);
	zvmessage(string," ");
	sprintf(string," LATITUDE            (3) = %f",real_data[2]);
	zvmessage(string," ");
	sprintf(string," LONGITUDE           (6) = %f",real_data[5]);
	zvmessage(string," ");
	sprintf(string," SCALE (KM/PIXEL)    (7) = %f",real_data[6]);
	zvmessage(string," ");
	sprintf(string," CAS                 (8) = %f",real_data[7]);
	zvmessage(string," ");
	sprintf(string," NORTH ANGLE         (9) = %f",real_data[8]);
	zvmessage(string," ");

	break;
			}

	sprintf(string," POLAR RADIUS       (25) = %f",real_data[24]);
	zvmessage(string," ");
	sprintf(string," EQUATORIAL RADIUS  (26) = %f",real_data[25]);
	zvmessage(string," ");

	switch ( int_data[38] )	{

		case 5:

		sprintf(string," LATITUDE OF 1 SPECIAL PARALLEL (4) = %f",
			real_data[3]);
		zvmessage(string," ");
		sprintf(string," LATITUDE OF 2 SPECIAL PARALLEL (5) = %f",
			real_data[4]);
		zvmessage(string," ");
		break;

		case 11:
		case 13:

		sprintf(string," OBLIQUE LONGITUDE OF ROTATION (4) = %f",
			real_data[3]);
		zvmessage(string," ");
		break;

	
		case 6:
		case 9:
		case 10:
		case 15:

		sprintf(string," MAP RESOLUTION (PXL/DEG) (10) = %f",
			real_data[9]);
		zvmessage(string," ");
		break;	
	
		default:

		break;
				}
	}
}
