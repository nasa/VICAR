#ifndef MP_ROUTINES_H
#define MP_ROUTINES_H

        /*
         *      MP_ROUTINES.H
         *
	 * Formerly named MP.H, this include file specifies MP objects
         * that are for public use, e.g., keyword names, numeric codes, etc.
         * Some of the items in this file have FORTRAN equivalents
         * in MP_FOR_DEFS.FIN, and the latter must be maintained in
         * parallel with this file
         *
         * Revision History:
         *
         * May 18, 1994     	JFM     Added VICARrtlSUCCESS constant DEFINE
         *                              for status checks in source of VICAR RTL
         *                              routines' success. Reordered listing of
         *                              valid map projection type DEFINES to
         *                              to reflect correct grouping of synonyms
         *                              in map projection names, specifically
         *                              moving mpNORMAL_CYLINDRICAL to be with
         *                              mpCYLINDRICAL_EQUAL_AREA instead of
         *                              mpEQUIDISTANT. (FR 76820,82916) (JFM059)
         *
         * October 1994		JFM	Renamed to MP_ROUTINES.H from MP.H
         *				to avoid system include file conflicts.
	 *				(FR 85094) (JFM059)  K_LIMIT added
         *				for capability with new version of
         *				TRIAXTRAN routine.
	 *
         * September 1995       JFM     Revised MAX/MIN define statement to have
         *                              an identical syntax to a MAX define
         *                              with a MOTIF include file. (FR 87358)
	 *
	 * December 1995        JFM     Added DEFINE of MP_ROUTINES_H
         *  				so as to prevent multiple inclusions
         *				of the include file.
         *
         * August 1996          lwk     Changed mpNUMBER_OF_PROJECTIONS to 29
	 *
         * Oct. 1996            lwk     Added COORDINATE_SYSTEM_NAME
         *
         * Dec. 1997            lwk     Changed SPACECRAFT_DISTANCE to
         *                              TARGET_CENTER_DISTANCE; added 
	 *				MIN/MAX_LAT/LONG
	 *
	 * Aug 1998		rgd	Added prototypes for public functions;
	 *				C++ compatibility
	 *
         * July 2000            lwk     added TARGET_NAME to TARGET_BODY 
         */

#ifdef __cplusplus
extern "C" {
#endif

typedef void *MP;       /* this ensures that the MP object is private only */

        /* GENERAL DEFINES */

#ifndef TRUE
#define TRUE                    1
#endif

#ifndef FALSE
#define FALSE                   0
#endif

#ifndef NULL
#ifdef __cplusplus
#define NULL			0
#else
#define NULL                    (void*) 0
#endif
#endif

#define YES                     1
#define NO                      0

#ifndef MAX
#define MAX(x,y)                (((x)>(y)) ? (x):(y))
#endif

#ifndef MIN
#define MIN(x,y)                (((x)<(y)) ? (x):(y))
#endif

#define CHECKif(x)              if (x) return mpFAILURE
#define ABENDif(x)              if (x) zabend()
#define EQUAL(x,y)              (strcmp(x,y)==0)
#define ODD(y)                  (y & 0x00000001)

#define NLIMIT                  4       /* Defines for dimensions of */
#define MLIMIT                  4       /* CC and CP matrices of     */
                                        /* Triaxial ellips. calcs.   */
#define KLIMIT                  4       /* Define for a dimension of */
					/* AC matrix for authalic    */
					/* solutions of ellipsoid.   */

#define STRING_LENGTH           132

#define VICARrtlSUCCESS         0       /* Lowest value of VICAR RTL success */
                                        /* status flag.                      */

#define mpSUCCESS               0       /* MP routines success status value  */
#define mpFAILURE               -1      /* MP routines failure status value  */

#define mpPROPERTY_NAME         "MAP"   /* Name of MP Property group:   */

        /* DEFINES for the Core/Supplementary classes:
         * (needed for mpLabelWrite) */

#define mpCORE  0
#define mpSUPPL 1

        /* MP keyword array definitions: */

#define mpMAX_KEYWD_LENGTH      40      /* max. length excluding final 0 */
#define mpNUMBER_OF_KEYWORDS    35      /* current keywords              */
#define mpMAX_PARM_LENGTH       15      /* max. length of VICAR PDF parm */
                                        /* excluding the final 0 (NULL)  */
#define mpMAX_DESCRIPTION_LINES 30	/* Maximum number of lines in a  */
					/* map projection description    */

        /* definitions of MP Keywords: */

#define mpTARGET_NAME                   "TARGET_NAME"
  /* TARGET_BODY is supported as an obsolete variant of
   * TARGET_NAME for the sake of backwards compatibility */
#define mpTARGET_BODY                   "TARGET_BODY"
#define mpMAP_PROJECTION_TYPE           "MAP_PROJECTION_TYPE"
#define mpA_AXIS_RADIUS                 "A_AXIS_RADIUS"
#define mpB_AXIS_RADIUS                 "B_AXIS_RADIUS"
#define mpC_AXIS_RADIUS                 "C_AXIS_RADIUS"
#define mpMAP_SCALE                     "MAP_SCALE"
#define mpMAP_RESOLUTION                "MAP_RESOLUTION"
#define mpPOSITIVE_LONGITUDE_DIRECTION  "POSITIVE_LONGITUDE_DIRECTION"
#define mpCOORDINATE_SYSTEM_NAME	"COORDINATE_SYSTEM_NAME"
#define mpCENTER_LATITUDE               "CENTER_LATITUDE"
#define mpCENTER_LONGITUDE              "CENTER_LONGITUDE"
#define mpSPHERICAL_AZIMUTH             "SPHERICAL_AZIMUTH"
#define mpCARTESIAN_AZIMUTH             "CARTESIAN_AZIMUTH"
#define mpLINE_PROJECTION_OFFSET        "LINE_PROJECTION_OFFSET"
#define mpSAMPLE_PROJECTION_OFFSET      "SAMPLE_PROJECTION_OFFSET"
#define mpFIRST_STANDARD_PARALLEL       "FIRST_STANDARD_PARALLEL"
#define mpSECOND_STANDARD_PARALLEL      "SECOND_STANDARD_PARALLEL"
#define mpFOCAL_LENGTH                  "FOCAL_LENGTH"
#define mpFOCAL_PLANE_SCALE             "FOCAL_PLANE_SCALE"
#define mpNORTH_ANGLE                   "NORTH_ANGLE"
#define mpOPT_AXIS_INTERCEPT_LINE       "OPT_AXIS_INTERCEPT_LINE"
#define mpOPT_AXIS_INTERCEPT_SAMPLE     "OPT_AXIS_INTERCEPT_SAMPLE"
#define mpPLANET_CENTER_LINE            "PLANET_CENTER_LINE"
#define mpPLANET_CENTER_SAMPLE          "PLANET_CENTER_SAMPLE"
#define mpSUB_SPACECRAFT_LATITUDE       "SUB_SPACECRAFT_LATITUDE"
#define mpSUB_SPACECRAFT_LONGITUDE      "SUB_SPACECRAFT_LONGITUDE"
#define mpTARGET_CENTER_DISTANCE        "TARGET_CENTER_DISTANCE"
  /* SPACECRAFT_DISTANCE is supported as an obsolete variant of
   * TARGET_CENTER_DISTANCE, for the sake of backwards compatibility */
#define mpSPACECRAFT_DISTANCE           "SPACECRAFT_DISTANCE"
#define mpBODY_LONG_AXIS                "BODY_LONG_AXIS"
#define mpMINIMUM_LATITUDE              "MINIMUM_LATITUDE"
#define mpMAXIMUM_LATITUDE              "MAXIMUM_LATITUDE"
#define mpMINIMUM_LONGITUDE             "MINIMUM_LONGITUDE"
#define mpMAXIMUM_LONGITUDE             "MAXIMUM_LONGITUDE"
#define mpMAP_PROJECTION_DESC           "MAP_PROJECTION_DESC"

        /* Currently supported map projection types defined below. */
        /* Synonyms are listed directly beneath the PDS standard   */
        /* name for the map projection type.                       */

#define mpNUMBER_OF_PROJECTIONS         29

#define mpALBERS                        "ALBERS"
#define mpALBERS_ONE_PARALLEL           "ALBERS_ONE_PARALLEL"
#define mpALBERS_TWO_PARALLELS          "ALBERS_TWO_PARALLELS"

#define mpCYLINDRICAL_EQUAL_AREA        "CYLINDRICAL_EQUAL_AREA"
#define mpNORMAL_CYLINDRICAL            "NORMAL_CYLINDRICAL"

#define mpEQUIDISTANT                   "EQUIDISTANT"
#define mpCYLINDRICAL                   "CYLINDRICAL"
#define mpRECTANGULAR                   "RECTANGULAR"
#define mpSIMPLE_CYLINDRICAL            "SIMPLE_CYLINDRICAL"
#define mpOBLIQUE_CYLINDRICAL    	"OBLIQUE_CYLINDRICAL"
#define mpOBLIQUE_SIMPLE_CYLINDRICAL    "OBLIQUE_SIMPLE_CYLINDRICAL"

#define mpLAMBERT_AZIMUTHAL             "LAMBERT_AZIMUTHAL"

#define mpLAMBERT                       "LAMBERT"
#define mpLAMBERT_CONFORMAL             "LAMBERT_CONFORMAL"
#define mpLAMBERT_ONE_PARALLEL          "LAMBERT_ONE_PARALLEL"
#define mpLAMBERT_TWO_PARALLELS         "LAMBERT_TWO_PARALLELS"

#define mpMERCATOR                      "MERCATOR"
#define mpTRANSVERSE_MERCATOR           "TRANSVERSE_MERCATOR"

#define mpMOLLWEIDE                     "MOLLWEIDE"
#define mpHOMALOGRAPHIC                 "HOMALOGRAPHIC"

#define mpORTHOGRAPHIC                  "ORTHOGRAPHIC"
#define mpPOLAR_ORTHOGRAPHIC            "POLAR_ORTHOGRAPHIC"
#define mpOBLIQUE_ORTHOGRAPHIC          "OBLIQUE_ORTHOGRAPHIC"

#define mpSINUSOIDAL                    "SINUSOIDAL"
#define mpOBLIQUE_SINUSOIDAL            "OBLIQUE_SINUSOIDAL"

#define mpSTEREOGRAPHIC                 "STEREOGRAPHIC"
#define mpPOLAR_STEREOGRAPHIC           "POLAR_STEREOGRAPHIC"
#define mpOBLIQUE_STEREOGRAPHIC         "OBLIQUE_STEREOGRAPHIC"

#define mpPOINT_PERSPECTIVE             "POINT_PERSPECTIVE"

                /* codes for Keyword Types: */

#define mpCHAR 1
#define mpSHRT 2
#define mpLONG 3
#define mpREAL 4
#define mpDBLE 5

                /* error return codes: */

#define mpINVALID_KEYWORD_TYPE          -1001
#define mpINTERNAL_STRUCT_ERROR         -1002
#define mpKEYWORD_NOT_FOUND             -1003
#define mpIMPROPER_KEYWD_VALUE_PAIR     -1004
#define mpIMPROPER_SETVAL_CALL          -1005
#define mpIMPROPER_GETVAL_CALL          -1006
#define mpKEYWORD_NOT_SET               -1007
#define mpINVALID_PROJECTION            -1008
#define mp_NO_MAP_LABELS                -1009
#define mpINVALID_COORD_SYSTEM          -1010
#define mpINVALID_LONG_DIRECTION        -1011

                /* Type definitions:   
typedef char BYTE;
typedef int (*ROUTINE)();
typedef int FLAG;
*/

#define SEARCV3_BUFLEN 40

		/* Function prototypes */
		/* see zvproto.h (RTL) for explanation of variadic functions */

#ifdef _NO_PROTO

int mpInit();
int mpFree();
int mpll2xy();
int mpxy2ll();
int mpGetKeywords();
int mpSetDebugFlag();

int mpLabelRead();
int mpLabelWrite();
int mpMpo2Buf();
int mpBuf2Mpo();

int mpSetValues();
int mpGetValues();

#else	/* _NO_PROTO */

int mpInit( MP * );
int mpFree( MP );
int mpll2xy( MP, double *, double *, double, double, int );
int mpxy2ll( MP, double, double, double *, double *, int );
int mpGetKeywords( MP, char [][mpMAX_KEYWD_LENGTH+1], int *, int *, int * );
int mpSetDebugFlag( int );

int mpLabelRead( MP, int );
int mpLabelWrite( MP, int, char * );
int mpMpo2Buf( MP, void * );
int mpBuf2Mpo( void *, MP );

#if !defined(__cplusplus)	/* variable-parameter functions are special */
int mpSetValues(MP mp_public, ...);
int mpGetValues(MP mp_public, ...);
#else
int mpSetValues(MP, ...);
int mpGetValues(MP, ...);
#endif

int mp_triaxcoef( double radii[3], double cc[MLIMIT][NLIMIT],
                  double cp[MLIMIT][NLIMIT], double ac[KLIMIT+1][MLIMIT+1],
                  double ap[NLIMIT]);
int planetodetic_authalic_trans( MP mp, double *planetodetic_latitude,
                                 double *authalic_latitude );
int planetodetic_conformal_trans( MP mp, double *planetodetic_latitude,
                                  double *conformal_latitude );
int ortho_obl_c(int m, float *data, float *line,
                float *sample, float *lat, float *longi);
int triaxtran_c(double ain, double bin, double cin,
                double cc[MLIMIT][NLIMIT], double cp[MLIMIT][NLIMIT],
                double ac[KLIMIT+1][MLIMIT+1],double ap[NLIMIT],
                double inlat, double inlon, int infmt,
                double *outlat, double *outlon, int outfmt);
int searcv3_c( int unit, float data[SEARCV3_BUFLEN], int idata[SEARCV3_BUFLEN]);

int mpGetDesc( MP, char [][100], int *);        /* used by mpLabelWrite */

#endif	/* _NO_PROTO */

#ifdef __cplusplus
}	/* end extern "C" */
#endif

#endif

