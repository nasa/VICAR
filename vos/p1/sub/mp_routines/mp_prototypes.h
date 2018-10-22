/******************************************************************************

Function prototypes for internal MP routines (mp_prototypes.h)

Author:			Justin McNeill, JPL
Cognizant Engineer:	Justin McNeill
			MS 168-414
			Jet Propulsion Laboratory
			4800 Oak Grove Drive
			Pasadena, California 91109
			(email) jfm059@ipl.jpl.nasa.gov

Written:		May 1994
Revisions:
  Apr1997 - Frank Scholten - added prototypes for check_longitude_radians
  Aug98   - Bob Deen -  revised for use with C++
*/
#ifndef MP_PROTOTYPES_H
#define MP_PROTOTYPES_H

#ifndef _NO_PROTO

/*

MP Internal function prototypes

	Listed in the order of routines declared in the file
	mp_internals.c.

*/

int degrees2radians( double * );	/* Arithmetic functions */
int radians2degrees( double * );
double check_longitude( double );
double check_longitude_radians( double );
int check_latitude( double );

int determine_map_projection_number( MP, MP_NUMBER * );
int determine_model( MP, double * );
int load_coefficients( MP, double * );

int mpGetDesc( MP, char [][100], int *);	/* used by mpLabelWrite */

int mpSphere( MP, double *, double *, double *, double *, MP_NUMBER );
int mpOblate( MP, double *, double *, double *, double *, MP_NUMBER );
int mpEllipsoid( MP, double *, double *, double *, double *, MP_NUMBER );

int mp_perform_precalculations( MP, MP_NUMBER );
int mp_load_EULER_rotation_matrix( MP, MP_NUMBER );
int mp_perform_EULER_rotation( MP, double *, double * );
int mp_load_cosine_sine_CARTESIAN( MP );
int mp_perform_CARTESIAN_trans( MP, double *, double * );

int triaxcoef_c( double *, double *, double *, double *, double *, 
		int );
int pproj_mp_c( float *, float *, float *, float *, float *, int );
int ztime( int * );

#else

/*

MP Internal function prototypes

	Listed in the order of routines declared in the file
	mp_internals.c.

*/

int degrees2radians();		/* Arithmetic functions */
int radians2degrees();
double check_longitude();
double check_longitude_radians();
int check_latitude();

int determine_map_projection_number();
int determine_model();
int load_coefficients();

int mpGetDesc();		/* used by mpLabelWrite */

int mpSphere();
int mpOblate();
int mpEllipsoid();

int mp_perform_precalculations();
int mp_load_EULER_rotation_matrix();
int mp_perform_EULER_rotation();
int mp_load_cosine_sine_CARTESIAN();
int mp_perform_CARTESIAN_trans();

int triaxcoef_c();
int pproj_mp_c();
int ztime();

#endif

#endif		/* MP_PROTOTYPES_H */
