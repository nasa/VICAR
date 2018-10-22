/******************************************************************
 *	ms_defines.h						  *
 *								  *
 * Apr.     1998  ...S.Le......   Initial release.                *
 *								  *
 * Apr. 29, 1998  ...T.Huang...   Ported onto Alpha/VMS.          *
 *                                Cleaned the list of includes.   * 
 *                                Defined constant seg_id length. *
 *                                Removed duplicated defines.     * 
 *								  *
 ******************************************************************/

#ifndef	MS_DEFINES_H
#define MS_DEFINES_H

/* txh::modified for vms include */
#include "xvmaininc.h"
#if VMS_OS
#include <rpc.h>
#else
#include <rpc/rpc.h>                    /* for bool_t & xdr     */
#endif

#include <math.h>

#ifndef C_KERNEL_TYPE
#define C_KERNEL_TYPE		2
#endif

#ifndef SP_KERNEL_TYPE
#define SP_KERNEL_TYPE		1
#endif

#ifndef MAXSTRLEN
#define MAXSTRLEN		256
#endif

#ifndef KERNEL_ID_LEN
#define KERNEL_ID_LEN		4
#endif

#ifndef KERNEL_NAME_LEN
#define KERNEL_NAME_LEN		64
#endif

#ifndef FILE_IS_READABLE
#define FILE_IS_READABLE        1
#endif
 
#ifndef FILE_IS_WRITEABLE
#define FILE_IS_WRITEABLE       2
#endif
 
#ifndef FILE_IS_READ_WRITEABLE
#define FILE_IS_READ_WRITEABLE  3
#endif

#ifndef NUM_CK_DOUBLES
#define NUM_CK_DOUBLES          2
#endif
 
#ifndef NUM_CK_INTEGERS
#define NUM_CK_INTEGERS         5
#endif
 
#ifndef NUM_CK_DESCR
#define NUM_CK_DESCR            7
#endif

#ifndef REF_B1950
#define REF_B1950		2
#endif

#ifndef REF_J2000
#define REF_J2000		1
#endif

#ifndef	ISSWA
#define ISSWA			2
#define ISSWA_STR		"ISSW"
#endif

#ifndef	ISSNA
#define ISSNA			1
#define ISSNA_STR		"ISSN"
#endif

#ifndef VGR1ISSWA
#define VGR1ISSWA              6 
#define VGR1ISSWA_STR		"ISSW"
#endif

#ifndef VGR1ISSNA
#define VGR1ISSNA		7
#define VGR1ISSNA_STR		"ISSN"
#endif

#ifndef VGR2ISSWA
#define VGR2ISSWA		4 
#define VGR2ISSWA_STR		"ISSW"
#endif

#ifndef VGR2ISSNA
#define VGR2ISSNA		5
#define VGR2SSNA_STR		"ISSN"
#endif

#ifndef PLATFORM
#define PLATFORM		1
#define PLATFORM_STR		"PLATFORM"
#endif

#ifndef CASISSWA
#define CASISSWA                2
#define CASISSWA_STR		"ISSW"
#endif

#ifndef CASISSWA_SUM22
#define CASISSWA_SUM22          22
#define CASISSWA_SUM22_STR	"ISW2"
#endif

#ifndef CASISSWA_SUM44
#define CASISSWA_SUM44          42
#define CASISSWA_SUM44_STR	"ISW4"
#endif

#ifndef CASISSNA
#define CASISSNA		1
#define CASISSNA_STR		"ISSN"
#endif

#ifndef CASISSNA_SUM22
#define CASISSNA_SUM22          21
#define CASISSNA_SUM22_STR	"ISN2"
#endif

#ifndef CASISSNA_SUM44
#define CASISSNA_SUM44          41
#define CASISSNA_SUM44_STR	"ISN4"
#endif

#ifndef CASISSNA_NAIF_ID
#define CASISSNA_NAIF_ID        -82360
#endif

#ifndef CASISSWA_NAIF_ID
#define CASISSWA_NAIF_ID        -82361
#endif

#ifndef GLL_SC_ID
#define GLL_SC_ID		-77
#endif

#ifndef VGR_1_SC_ID
#define VGR_1_SC_ID		-31
#endif

#ifndef VGR_2_SC_ID
#define VGR_2_SC_ID		-32
#endif

#ifndef VIKOR_1_SC_ID
#define VIKOR_1_SC_ID		-27
#endif

#ifndef VIKOR_2_SC_ID
#define VIKOR_2_SC_ID		-30
#endif

#ifndef CAS_SC_ID
#define CAS_SC_ID		-82
#endif

#ifndef SIM_SC_ID				/* this is a bogus value */
#define SIM_SC_ID		-11		/* should change this	 */
#endif						/* when available	 */

#ifndef SPICE_SERVER_REQ			/* define request codes	 */
#define SPICE_SERVER_REQ			/* SpiceServer can handle*/

#define GLL_GETSPICE		11		/* These should be	 */
#define GLL_PUTSPICE		12		/* unsigned short values */
#define GLL_GET_CK		13		/* from MsClients.	 */
#define GLL_PUT_CK		14
#define GLL_GET_SPK		15		/* the should be xdr'ed	 */
#define GLL_PUT_SPK		16		/* by xdr_u_short()	 */

#define CAS_GETSPICE		17		/* FROM CLT 2 SVR ONLY	 */
#define CAS_PUTSPICE		18
#define CAS_GET_CK		19
#define CAS_PUT_CK		20
#define CAS_GET_SPK		21
#define CAS_PUT_SPK		22

#define SIM_GETSPICE		23
#define SIM_PUTSPICE		24
#define SIM_GET_CK		25
#define SIM_PUT_CK		26
#define SIM_GET_SPK		27
#define SIM_PUT_SPK		28

#define VGR1_GETSPICE		29
#define VGR1_PUTSPICE		30
#define VGR1_GET_CK		31
#define VGR1_PUT_CK		32
#define VGR1_GET_SPK		33
#define VGR1_PUT_SPK		34

#define VGR2_GETSPICE		35
#define VGR2_PUTSPICE		36
#define VGR2_GET_CK		37
#define VGR2_PUT_CK		38
#define VGR2_GET_SPK		39
#define VGR2_PUT_SPK		40

#define VO1_GETSPICE		41
#define VO1_PUTSPICE		42
#define VO1_GET_CK		43
#define VO1_PUT_CK		44
#define VO1_GET_SPK		45
#define VO1_PUT_SPK		46

#define VO2_GETSPICE		47
#define VO2_PUTSPICE		48
#define VO2_GET_CK		49
#define VO2_PUT_CK		50
#define VO2_GET_SPK		51
#define VO2_PUT_SPK		52

						/* FROM SVR 2 CLT ONLY...    */
						/* codes sent from SVR2CLT   */
#define SVR_ERR_MESG		101		/* closing conn, has mesg    */
#define SVR_INFO_MESG		102		/* info mesg with text data  */
#define SVR_PKG_ACK		103		/* ack pkg, useful with files*/
#define SVR_CK_DATA		104		/* prepare for CK data	     */
#define SVR_SPK_DATA		105		/* prepare for SPK data	     */
#define OPS_COMPLETED		106		/* SUCCESS: completed req    */

#define REQ_CODE_LEN		4		/* len of different mesg  */
#define SVR_MESG_BUF_LEN	1024		/* being passed between	  */
#define SVR_DATA_BUF_LEN	2048		/* MS client & server	  */

#define MAX_TOL_TRIES		50

/**txh::changed the initial tolerance value to 1 MOD91 or 80.0 ***
#define NUMERIC_INIT_TOL	8.0
#define INITIAL_TOL_VAL		"00:00:01:00"
**/
#define NUMERIC_INIT_TOL        80.0
#define INITIAL_TOL_VAL         "00:03:00:00"

#define MAX_SEG_ID_LEN          41              /* txh::len of segment id */
#define NULL_PROV_STR		"NONE*NONE*NONENONE*NONE*000000000000NONE"
#define MAX_KERNEL_COUNT	8192
#endif

#ifndef MS_KERNEL_NAME_STRUCT
#define MS_KERNEL_NAME_STRUCT
struct msKernelNameStruct {
 char name[256];
};
typedef struct msKernelNameStruct msKernelNameStruct;

#endif

#ifndef MS_PROV_INFO_STRUCT
#define MS_PROV_INFO_STRUCT
struct msProvInfoStruct {
	char    institution[5];
        char    purpose[5];
        char    program_name[7];
        char    spk_id[5];
        char    request_number[5];
        char    year[5];
        char    month_day[5];
        char    hour_min[5];
        char    user_id[4];
        char    group_id[4];
        char    seg_id[MAX_SEG_ID_LEN];
	};

typedef struct msProvInfoStruct msProvInfoStruct;
#endif

#ifndef MS_USER_REQUEST_STRUCT
#define MS_USER_REQUEST_STRUCT
struct msUserRequestStruct {
	int     sc_id;
        int     system;
        int     scet[6];

        char    instrument_name[KERNEL_NAME_LEN];
        char    target_name[KERNEL_NAME_LEN];
        char    ck_id[KERNEL_ID_LEN];
        char    ck_name[KERNEL_NAME_LEN];
        char    ck_source[KERNEL_ID_LEN];
        char    spk_id[KERNEL_ID_LEN];
        char    spk_name[KERNEL_NAME_LEN];

        msProvInfoStruct        provInfo;
	};

typedef struct msUserRequestStruct msUserRequestStruct;
#endif

#ifndef MS_CK_STRUCT
#define MS_CK_STRUCT
struct msCkStruct {
        int     sc_id,                  /* NAIF spacecraft id           */
                instrument,             /* instrument value (sc, camid) */
                system;       		/* 1: J2000, 2: B1950           */

	int	scet[6];
        double  av[3];			/* angular velocity		*/
	u_char	avFlag;			/* do we need angular velocity	*/
        double  c_matrix[9];
        char    seg_id[MAX_SEG_ID_LEN];

	char	ck_id[KERNEL_ID_LEN+1],		/* when search or reading */
		ck_name[KERNEL_NAME_LEN+1],	/* from kernels, the	  */
		ck_source[KERNEL_ID_LEN+1];	/* priority will be as	  */
						/* shown. If the value is */
						/* not used, it should be */
						/* NULLed out.		  */
						/*			  */
					/* if (bad_id) use ck_name	  */
					/* if (bad ck_name) use ck_source */
					/* if (bad ck_source) return err  */
						
        };
 
typedef struct msCkStruct msCkStruct;
 
 
#ifndef MS_SPK_STRUCT
#define MS_SPK_STRUCT
struct msSpkStruct {
	int	sc_id;
	double	tgt_radius_l_axis,	/* target radius: long axis	*/
		tgt_radius_s_axis,	/* target radius: short axis	*/
		tgt_polar_radius,	/* target radius: polar		*/
		sc_pos_bd_centered[3],	/* sc & picture cartesian	*/
		pic_pos_sc_centered[3],	/* position, body & sc centered	*/
		rs_vector[3],		/* tgt_bd_2_sc_vector		*/
		range_pic_bd_2_sun,
		range_sc_2_central_bd_cntr,
		range_sc_2_picture_bd_cntr,
		tgt_bd_cntr_2_sun_lat,
		tgt_bd_cntr_2_sun_lon,
		tgt_bd_cntr_2_sc_lat,
		tgt_bd_cntr_2_sc_lon,
		me_matrix[9],
		om_matrix[9],
		north_angle,
		sub_sc_line,
		sub_sc_samp,
		p5_lat,
		p5_lon,
		p5_incidence_angle,
		p5_emission_angle,
		p5_phase_angle,
		p5_vert_pix_size,
		p5_horiz_pix_size,
		range_sc2p5_intercept_pt;
	};	
 
typedef struct msSpkStruct msSpkStruct;
#endif
 
#ifndef MS_INFO_STRUCT
#define MS_INFO_STRUCT
struct msInfoStruct {
	msCkStruct		ckInfo;
	msSpkStruct		spkInfo;
	msProvInfoStruct	provInfo;
        };
 
typedef struct msInfoStruct msInfoStruct;
#endif

#ifndef MS_ENV_STRUCT
#define MS_ENV_STRUCT
struct msEnvStruct {
	char	kdb[128],
		spiceker[128],
		mipsker[128],
		sclk[128],
		consts[128],
		bodyids[128],
		leapsec[128],
	        frames[128];  
	};

typedef struct msEnvStruct msEnvStruct;
#endif

#ifndef KERNEL_INFO_STRUCT
#define KERNEL_INFO_STRUCT
struct kernelInfoStruct {
	int	isloaded,
		type,
		handle;
	char    id[KERNEL_ID_LEN+1],
                source[KERNEL_ID_LEN+1],
                fname[KERNEL_NAME_LEN+1],
                utc_begin[KERNEL_NAME_LEN+1],
                utc_end[KERNEL_NAME_LEN+1];

	double	et_begin;
	double	et_end;
	};
typedef struct kernelInfoStruct kernelInfoStruct;
#endif

#ifdef __cplusplus
extern "C"
{
#endif                          /* __cplusplus          */
 
bool_t	xdr_msProvInfoStruct	(XDR*, msProvInfoStruct*);
bool_t	xdr_msUserRequestStruct	(XDR*, msUserRequestStruct*);
bool_t	xdr_msCkStruct		(XDR*, msCkStruct*);
bool_t	xdr_msSpkStruct		(XDR*, msSpkStruct*);

void	zmve (int, int, void*, void*, int, int);
int	zcam_info (int sc_id, char *cam_name, int *cam_id,
		int *cam_sn, double *rad_per_pix, float *focal_mm,
		float *opaxis_line, float *opaxis_samp, float *scale);

void	zgetcamcon(char*, int, float*, float*, float*, float*, int*);
 
#ifdef __cplusplus
}
#endif                          /* __cplusplus          */
#endif                          /* MS_CK_STRUCT         */

#endif
