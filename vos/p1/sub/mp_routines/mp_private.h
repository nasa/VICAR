/*************************************************************************

	Map Projection Data Object Include File

	Purpose

	This is an include file that is used by all routines of the 
	mp_ set of map projection software. This is a private include
	which is not to be included in application programs which
	call the mp_routines functions.

	Cognizant Engineer		Justin McNeill

	Date				October 1993

	Revision History ...		See Release Notes.

*/
#include "mp_routines.h"

struct GLOBAL_KEYWORDS {
	char	target_name[32];
	char	target_body[32];	/* obsolete -- used as input only */
	char	map_projection_type[40];
	char	coordinate_system_name[15];
	char	positive_longitude_direction[5];
	double	a_axis_radius;
	double	b_axis_radius;
	double	c_axis_radius;
	double  body_long_axis;
	double  minimum_latitude;
	double  maximum_latitude;
	double  minimum_longitude;
	double  maximum_longitude;
};

struct MAP_PROJ_KEYWORDS {
	double	map_scale;
	double	map_resolution;
	double	center_latitude;
	double	center_longitude;
	double	spherical_azimuth;
	double	cartesian_azimuth;
	double	line_projection_offset;
	double  sample_projection_offset;
	double	first_standard_parallel;
	double	second_standard_parallel;
};

struct PERSPECTIVE_KEYWORDS {
	double	focal_length;
	double 	focal_plane_scale;
	double	north_angle;
	double	opt_axis_intercept_line;
	double	opt_axis_intercept_sample;
	double 	planet_center_line;
	double	planet_center_sample;
	double	sub_spacecraft_latitude;
	double	sub_spacecraft_longitude;
	double	target_center_distance;
};

struct PRECALCULATIONS {
	double  Euler_matrix[3][3];		/* Translation and 	  */
	double  translation_longitude;		/* rotation variables 	  */
	double	cosine_of_CARTESIAN_AZIMUTH;
	double	sine_of_CARTESIAN_AZIMUTH;
	int	rotation_flags;

	double	value[6];			/* General variables	  */
	double	oblate_value[6];
	double	ellipsoid_value[6];

        double	cc_matrix[MLIMIT][NLIMIT];
	double	cp_matrix[MLIMIT][NLIMIT];
	double	ac_matrix[KLIMIT+1][MLIMIT+1];
        double	ap_matrix[NLIMIT];
};

struct 	MP_STRUCTURE {
	struct	GLOBAL_KEYWORDS		glob;
	struct	MAP_PROJ_KEYWORDS	mpro;
	struct	PERSPECTIVE_KEYWORDS	pers;
	struct  PRECALCULATIONS		precalc;
	char	model[30];
	int	transformation_direction;
	int	kwd_set[4];
	int	kwd_chng[4];
	int	precalc_needed;
};

/*

Include general MP file

*/

#include "mp_routines.h"

/******************************************************************************

MP_KEY_CODE.H 	-  	private include file defining the keyword names, values,
			codes, flags for the MP_STRUCTURE processing.

*/

	/* Hexadecimal Representations of MP Keywords:
	 * Each keyword has a bit code associated with it, which is used
	 * by mpGetKeywords to determine valid keywords for a given
	 * projection.  The valid keyword bit masks for the projections
	 * are defined the file mp_init.h.  NOTE that valid bits are
	 * currently only being checked for group 1 (MAP_PROJ_KEYWORDS).
	 * Also NOTE that not all possible bit values are used, so gaps
	 * in the sequence are permitted.  (E.g., every 4th bit is omitted,
	 * for some obscure reason.)
	 */

	/* GLOBAL_KEYWORDS group hexadecimal codes	*/

#define	mpTARGET_NAME_BIT			0x00000001
#define	mpMAP_PROJECTION_TYPE_BIT		0x00000002
#define	mpA_AXIS_RADIUS_BIT 			0x00000004
#define	mpB_AXIS_RADIUS_BIT			0x00000010
#define	mpC_AXIS_RADIUS_BIT 			0x00000020
#define	mpBODY_LONG_AXIS_BIT			0x00000040
#define	mpPOSITIVE_LONGIT_DIR_BIT		0x00000100
#define	mpCOORDINATE_SYS_NAME_BIT		0x00000200
#define	mpMINIMUM_LATITUDE_BIT			0x00000400
#define	mpMAXIMUM_LATITUDE_BIT			0x00001000
#define	mpMINIMUM_LONGITUDE_BIT			0x00002000
#define	mpMAXIMUM_LONGITUDE_BIT			0x00004000
#define	mpTARGET_BODY_BIT			0x00010000

	/* MAP_PROJ_KEYWORDS group hexadecimal codes	*/

#define	mpMAP_SCALE_BIT				0x00000001
#define	mpMAP_RESOLUTION_BIT			0x00000002
#define	mpCENTER_LATITUDE_BIT			0x00000010
#define	mpCENTER_LONGITUDE_BIT			0x00000020
#define	mpSPHERICAL_AZIMUTH_BIT			0x00000040
#define	mpCARTESIAN_AZIMUTH_BIT			0x00000100
#define	mpLINE_PROJECTION_OFFSET_BIT		0x00000200
#define	mpSAMPLE_PROJECTION_OFFSET_BIT		0x00000400
#define	mpFIRST_STANDARD_PARALLEL_BIT		0x00001000
#define	mpSECOND_STANDARD_PARALLEL_BIT		0x00002000

	/* PERSPECTIVE_KEYWORDS group hexadecimal codes	*/

#define	mpFOCAL_LENGTH_BIT			0x00000001
#define	mpFOCAL_PLANE_SCALE_BIT			0x00000002
#define	mpNORTH_ANGLE_BIT			0x00000004
#define	mpOPT_AXIS_INTERCEPT_LINE_BIT		0x00000010
#define	mpOPT_AXIS_INTERCEPT_SAMPLE_BIT		0x00000020
#define	mpPLANET_CENTER_LINE_BIT		0x00000040
#define	mpPLANET_CENTER_SAMPLE_BIT		0x00000100
#define	mpSUB_SPACECRAFT_LATITUDE_BIT		0x00000200
#define	mpSUB_SPACECRAFT_LONGITUDE_BIT		0x00000400
#define	mpTARGET_CENTER_DISTANCE_BIT		0x00001000
#define	mpSPACECRAFT_DISTANCE_BIT		0x00002000

		/*
		 * DEFINES of overall structure properties:
		 * Note:  mpMAX_KEYWD_LENGTH and mpNUMBER_OF_KEYWORDS
		 * are defined in mp_routines.h (formerly mp.h)
		 */

#define	mpNUMBER_OF_GROUPS	3	/* Number of structure groupings */
					/* of keywords.			 */
#define	mpKEYWORDS_MAXIMUM	32	/* Maximum number of keywords in */
					/* a structure grouping.	 */
#define mpMAX_DESCRIPTION_LINES 30	/* Maximum number of lines in a  */
					/* map projection description    */

		/* Stucture to define the relationship	*/
		/* between keyword name, type, hex code	*/
		/* and group number:			*/
struct mpKEYWORD_TO_CODE {
  char	label_kwd[mpMAX_KEYWD_LENGTH+1];
  int	bit_code;
  int	kwd_type;
  int	group_no;
};

/******************************************************************************

MP_TRANS.H

this defines and macros are used by the translation routines
mpll2xy and mpxy2ll and their supporting functions.*/

/* replaced acos by values for speed up, Roatsch, Apr-2001 
#define		PI				( 2.0 * acos(0.0) )
#define		TWO_PI				( 4.0 * acos(0.0) )
#define		PI_OVER_2			( acos(0.0) )
#define		PI_OVER_4			( acos(0.0) / 2.0 ) */

#define		PI				3.1415926535897931159979635
#define		TWO_PI				6.2831853071795862319959269
#define		PI_OVER_2			1.5707963267948965579989817
#define		PI_OVER_4			0.7853981633974482789994909

#define		E_NEGATIVE_10			(1.0e-10)
#define		E_NEGATIVE_12			(1.0e-12)

#define		SPHERICAL_COORDINATE_ROTATION	0x01
#define		CARTESIAN_COORDINATE_ROTATION	0x02

#define		PLANETOCENTRIC			1
#define		PLANETODETIC			2
#define		SNYDER_DEFINED			3
#define		CONFORMAL			4
#define		AUTHALIC			5
			

#define		PERFORM_PRECALCULATIONS		0
#define		LAT_LON_TO_LINE_SAMP		1
#define		LINE_SAMP_TO_LAT_LON		2

/* Simple function macros */

#define		RETURN_SIGN(x)		( ((x)>0) ? 1 : -1 )
#define		RETURN_RADIANS(x) ( 0.0174532925199432954743717 * (double) (x) )
#define		RETURN_DEGREES(x) (57.2957795130823228646477219 * (double) (x) )

/* Map projection number enumeration */

typedef enum	{
	albers_one_p,		/* Albers with one standard parallel 			*/
	albers_two_p,		/* Albers with two standard parallels			*/
	cylindrical_equal_area,	/* Cylindrical equal-area				*/
	equidistant,		/* Equidistant cylindrical (rectangular)		*/
	lambert_azimuthal,	/* Lambert azimuthal equal-area				*/
	lambert_one_p,		/* Lambert conformal conic with one standard parallel 	*/	
	lambert_two_p,		/* Lambert conformal conic with two standard parallels	*/	
	mercator,		/* Mercator (including transverse mercator)		*/
	mollweide,		/* Mollweide (homalographic)				*/
	orthographic,		/* Orthographic (normal,oblique,polar)			*/
	sinusoidal,		/* Sinusoidal (normal,oblique)				*/
	stereographic,		/* Stereographic (normal,oblique,polar)			*/
	perspective		/* Point perspective					*/
}	MP_NUMBER;

