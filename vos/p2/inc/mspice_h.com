$!****************************************************************************
$!
$! Build proc for MIPL module mspice_h
$! VPACK Version 1.9, Friday, October 08, 2010, 16:35:32
$!
$! Execute by entering:		$ @mspice_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module mspice_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mspice_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("mspice_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @mspice_h.bld "STD"
$   else
$      @mspice_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mspice_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mspice_h.com -mixed -
	-s ms_defines.h ms_bridge.h cltsub.h lclsub.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ms_defines.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ms_bridge.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef MS_BRIDGE_H
#define MS_BRIDGE_H

#ifdef __cplusplus
extern "C" {
#endif
void	zcorcav		(int*, float*, float*, double*, double*,
			float*, float*, float*, float*, float*,
			float*, float*, float*);
void	zms_erract	(char *x,char *y);
void	zms_vadd_mat	(double*, double*, double*);
void	zms_bodeul	(int, double, double*, double*, double*, double*);
void	zms_bodn2c	(char*, int*, int*);
void	zms_bodvar	(int, char*, int*, double*);
void	zms_ckbss	(int, double, double, int);
void	zms_ckcls	(int);
void	zms_ckpfs	(int, double*, double, double, int,
				double*, double*, double*, int*);
void	zms_cksns	(int*, double*, char*, int*);
void	zms_cklpf	(char*, int*);
void	zms_ckupf	(int);
void	zms_ckw01	(int, double, double, int, char*, int, char*,
				int, double, double*, double*);
void	zms_clpool	();
void	zms_dafada	(void*, int);
void	zms_dafbna	(int, void*, char*);
void	zms_dafcls	(int);
void	zms_dafena	();
void	zms_dafgh	(int*);
void	zms_dafopr	(char*, int*);
void	zms_dafopw	(char*, int*);
void	zms_dafps	(int, int, void*, void*, void*);
double	zms_dpr		();
void	zms_dafus	(void*, int, int, void*, void*);
void	zms_eul2m	(double, double, double, int, int, int, double*);
int	zms_failed	();
double	zms_halfpi	();
void    zms_invert      (double[3][3], double[3][3]);
void	zms_irfrot	(int, int, void*);
void	zms_ldpool	(char*);
void	zms_m2q		(void*, void*);
void	zms_mtxv	(void*, void*, void*);
void	zms_mxm		(void*, void*, void*);
void	zms_mxmt	(void*, void*, void*);
void	zms_mxv		(void*, void*, void*);
double	zms_pi	        ();
void	zms_reclat	(void*, double*, double*, double*);
void	zms_reset	();
void    zms_rotate      (double, int, void*);
void	zms_sce2t 	(int, double, double*);
void	zms_sctiks 	(int, char*, double*);
void	zms_spkapp	(int, double, char*, void*, char*, void*, double*);
void	zms_spklef	(char*, int*);
void	zms_spkssb	(int, double, char*, void*);
void	zms_spkcls	(int);
void	zms_spkuef	(int);
void	zms_spcec	(int*, int*);
void	zms_spcac	(int*, int*, char*, char*);
void	zms_spcb2a	(char*, char*);
void	zms_spca2b	(char*, char*);
void	zms_surfnm	(double, double, double, double*, double*);
void	zms_surfpt	(double*, double*, double, double,
			double, double*, int*);
void    zms_tkfram      (int, double[3][3], int*, int*);
void	zms_txtcls	(int*);
void	zms_txtopn	(char*, int*);
void	zms_txtopr	(char*, int*);
void	zms_utc2et	(char*, double*);
void	zms_vminus	(void*, void*);
double	zms_vsep	(double*, double*);
void	zms_vsub	(double*, double*, double*);
double	zms_vnorm	(double*);
void	zms_xpose	(void*, void*);

#ifdef __cplusplus
}
#endif

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cltsub.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CLT_SUB_H
#define CLT_SUB_H

/**** txh::modified includes for VMS compilation ***/
#include "xvmaininc.h"

#if VMS_OS
#include <unixlib.h>
#include <stat.h>
#include <types.h>
#include <socket.h>
#include <netdb.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netdb.h>
#define  socket_write write     /* txh::VMS has different functions */
#define  socket_read  read      /*      for socket read, write and  */
#define  socket_close close     /*      close.                      */
#endif

#define CLIENT_NAME_LEN			32
#define CONFIG_FILE_LOG_KEY		"SPICE_LOG"
#define CONFIG_FILE_PORTNO_KEY		"SPICE_TCP_PORT"
#define CONFIG_FILE_SVR_HOST_KEY	"SERVER_HOST"

#ifdef __cplusplus
extern "C"
{
#endif

/* The XDR routines dealing with longs are defined poorly... they are	*/
/* defined to pass 32 bits on the wire but use "long" as the type, which*/
/* of course is not always 32 bits.  Linux and mac use __LP64__ to	*/
/* redefine these to int * rather than long * for this reason.  Solaris	*/
/* does not seem to do this.  To "fix" this, we define our own XDR_long	*/
/* type which is defined here the same way as xdr_long() is declared.	*/
/* Use it exclusively for these routines rather than the bare "long"	*/
/* type.  So much for portability of XDR!!  rgd 2010/10/08		*/

#ifdef __LP64__
#define XDR_long int
#define XDR_u_long u_int
#else
#define XDR_long long
#define XDR_u_long u_long
#endif

int msclt_getLogFileName (char *fname);
short msclt_getPortno ();
int msclt_log (const char *sub, const char *mesg);
int msclt_read_short (int sd, short *data);
int msclt_write_short (int sd, short data);
int msclt_read_u_short (int sd, u_short *data);
int msclt_write_u_short (int sd, u_short data);
int msclt_read_long (int sd, XDR_long *data);
int msclt_write_long (int sd, XDR_long data);
int msclt_read_u_long (int sd, XDR_u_long *data);
int msclt_write_u_long (int sd, XDR_u_long data);
int msclt_read_string (int sd, char *str, int max_str_len);
int msclt_write_string (int sd, char *str, int max_str_len);
int msclt_is_readable (int sd);
int msclt_is_writeable (int sd);
int msclt_write_ck_struct (int sd, msCkStruct *ckdata);
int msclt_write_spk_struct (int sd, msSpkStruct *spkdata);
int msclt_read_req_struct (int sd, msUserRequestStruct *req);
int msclt_write_req_struct (int sd, msUserRequestStruct *req);
XDR_u_long msclt_read_resp_code (int sd);
int msclt_read_ck_struct (int sd, msCkStruct *ckdata);
int msclt_read_spk_struct (int sd, msSpkStruct *spkdata);
int msclt_connectToSvr (int *sd);
int msclt_write_client_name (int *sd);
int msclt_getSvrHostNames (char *hname);

int msclt_getspice (XDR_u_long req_code, msUserRequestStruct *req,
                msCkStruct *ckdata, msSpkStruct *spkdata);

int msclt_gllgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_casgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vo1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int msclt_vo2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);

int msclt_putspice (XDR_u_long req_code, msCkStruct *ckdata);

int msclt_gllputspice (msCkStruct *ckdata);
int msclt_casputspice (msCkStruct *ckdata);
int msclt_simputspice (msCkStruct *ckdata);
int msclt_vgr1putspice (msCkStruct *ckdata);
int msclt_vgr2putspice (msCkStruct *ckdata);
int msclt_vo1putspice (msCkStruct *ckdata);
int msclt_vo2putspice (msCkStruct *ckdata);

int msclt_bin2text_file (char *bname, char *tname);
int msclt_text2bin_file (char *tname, char *bname);
int msclt_send_text_file (int sd, char *tname);
int msclt_receive_text_file (int sd, char *tname);
int msclt_send_bin_kernel (int sd, char *kname);
int msclt_receive_bin_kernel (int sd, char *kname);
int msclt_receive_kernel (XDR_u_long req_code, char *kname, char *local_dir);
int msclt_put_kernel (XDR_u_long req_code, char *kname);

int msclt_gllgetck (char *ckname, char *local_dir);
int msclt_casgetck (char *ckname, char *local_dir);
int msclt_simgetck (char *ckname, char *local_dir);
int msclt_vgr1getck (char *ckname, char *local_dir);
int msclt_vgr2getck (char *ckname, char *local_dir);
int msclt_vo1getck (char *ckname, char *local_dir);
int msclt_vo2getck (char *ckname, char *local_dir);

int msclt_gllgetspk (char *spk, char *local_dir);
int msclt_casgetspk (char *spk, char *local_dir);
int msclt_simgetspk (char *spk, char *local_dir);
int msclt_vgr1getspk (char *spk, char *local_dir);
int msclt_vgr2getspk (char *spk, char *local_dir);
int msclt_vo2getspk (char *spk, char *local_dir);
int msclt_vo2getspk (char *spk, char *local_dir);

int msclt_gllputck (char *ckname);
int msclt_casputck (char *ckname);
int msclt_simputck (char *ckname);
int msclt_vgr1putck (char *ckname);
int msclt_vgr2putck (char *ckname);
int msclt_vo1putck (char *ckname);
int msclt_vo2putck (char *ckname);

int msclt_gllputspk (char *spk);
int msclt_casputspk (char *spk);
int msclt_simputspk (char *spk);
int msclt_vgr1putspk (char *spk);
int msclt_vgr2putspk (char *spk);
int msclt_vo1putspk (char *spk);
int msclt_vo2putspk (char *spk);

int msclt_readn (int sd, char *ptr, int nbytes);
int msclt_writen (int sd, char *ptr, int nbytes);
void msclt_printspkstruct (msSpkStruct spk);
void msclt_printckstruct (msCkStruct ck);
void msclt_printuserrequeststruct (msUserRequestStruct req);
#ifdef __cplusplus
}
#endif

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lclsub.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef LCL_SUB_H
#define LCL_SUB_H

#ifdef __cplusplus
extern "C"
{
#endif

int mslcl_getgllenv (msEnvStruct *env);
int mslcl_getcasenv (msEnvStruct *env);
int mslcl_getsimenv (msEnvStruct *env);
int mslcl_getvgr1env (msEnvStruct *env);
int mslcl_getvgr2env (msEnvStruct *env);
int mslcl_getvo1env (msEnvStruct *env);
int mslcl_getvo2env (msEnvStruct *env);

int mslcl_load_support_kernels (msEnvStruct *env);
int mslcl_scet2sclk (int sc_id, int *scet, double *sclk);
int mslcl_strcmp (char *usr_str, char *ker_str, char *def_str);
int mslcl_identical_segid(char *usr_segid, char *ker_segid);
int mslcl_kid_2_kinfo (msEnvStruct *env,
        char *kid, kernelInfoStruct *kinfo);
int mslcl_kname_2_kinfo (msEnvStruct *env,
        char *kname, kernelInfoStruct *kinfo);
int mslcl_ck_info_from_kdb (msEnvStruct *env, kernelInfoStruct *info);
int mslcl_spk_info_from_kdb (msEnvStruct *env, kernelInfoStruct *info);
int mslcl_read_kernel_info (msEnvStruct *env,
                kernelInfoStruct *info, int type);
int mslcl_use_prov_info (char *segid);
int mslcl_lowerSegidPriority (char *segid);
int mslcl_loadSpk (int sd, double et, char *spk_ref,
        char *segid, kernelInfoStruct *kinfo, int kernel_count);
int mslcl_loadCK (int sd, msUserRequestStruct *req,
	kernelInfoStruct *kinfo, int kernel_count);
int mslcl_unloadSpk (kernelInfoStruct *kinfo, int count);
void mslcl_getDateTime (char *date_time);

int mslcl_gllgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_casgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vo1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_vo2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);
int mslcl_getspice (msEnvStruct *env, msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata);

int mslcl_gllputspice  (msCkStruct *ckdata);
int mslcl_casputspice  (msCkStruct *ckdata);
int mslcl_simputspice  (msCkStruct *ckdata);
int mslcl_vgr1putspice (msCkStruct *ckdata);
int mslcl_vgr2putspice (msCkStruct *ckdata);
int mslcl_vo1putspice  (msCkStruct *ckdata);
int mslcl_vo2putspice  (msCkStruct *ckdata);
int mslcl_putspice (msEnvStruct *env, msCkStruct *ckdata);


int mssvr_getNewClient (int ld);
int mssvr_initAcceptor (int *sd);
int mssvr_readClientName (int sd, char *clt_name);
XDR_u_long mssvr_readRequestCode (int sd);
int mssvr_handleRequest (int sd);

void mssvr_gllgetspice (int sd);
void mssvr_gllputspice (int sd);
void mssvr_gllsendck(int sd);
void mssvr_gllsendspk(int sd);
void mssvr_gllreceiveck(int sd);
void mssvr_gllreceivespk(int sd);

void mssvr_casgetspice (int sd);
void mssvr_casputspice (int sd);
void mssvr_cassendck(int sd);
void mssvr_cassendspk(int sd);
void mssvr_casreceiveck(int sd);
void mssvr_casreceivespk(int sd);

void mssvr_simgetspice (int sd);
void mssvr_simputspice (int sd);
void mssvr_simsendck(int sd);
void mssvr_simsendspk(int sd);
void mssvr_simreceiveck(int sd);
void mssvr_simreceivespk(int sd);

void mssvr_vgr1getspice (int sd);
void mssvr_vgr1putspice (int sd);
void mssvr_vgr1sendck(int sd);
void mssvr_vgr1sendspk(int sd);
void mssvr_vgr1receiveck(int sd);
void mssvr_vgr1receivespk(int sd);

void mssvr_vgr2getspice (int sd);
void mssvr_vgr2putspice (int sd);
void mssvr_vgr2sendck(int sd);
void mssvr_vgr2sendspk(int sd);
void mssvr_vgr2receiveck(int sd);
void mssvr_vgr2receivespk(int sd);

void mssvr_vo1getspice (int sd);
void mssvr_vo1putspice (int sd);
void mssvr_vo1sendck(int sd);
void mssvr_vo1sendspk(int sd);
void mssvr_vo1receiveck(int sd);
void mssvr_vo1receivespk(int sd);

void mssvr_vo2getspice (int sd);
void mssvr_vo2putspice (int sd);
void mssvr_vo2sendck(int sd);
void mssvr_vo2sendspk(int sd);
void mssvr_vo2receiveck(int sd);
void mssvr_vo2receivespk(int sd);

void mssvr_getspice (int sd, msEnvStruct *env);
int mssvr_readckdata(int sd, msEnvStruct *env,
	msUserRequestStruct *req, msCkStruct *ckdata);
int t_mssvr_readckdata(int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_read_ckrecord (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata, double tol);
int mssvr_readck_by_source (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_readck_by_id (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_readck_by_name (int sd, msEnvStruct *env,
        msUserRequestStruct *req, msCkStruct *ckdata);
int mssvr_readckfromfile (int sd, msUserRequestStruct *req,
        msCkStruct *ckdata, double tol, char *ckfname);
int mssvr_readspkdata (int sd, msEnvStruct *env,
        msUserRequestStruct *request, msCkStruct *ckdata,
        msSpkStruct *spkdata);
void mssvr_putspice (int sd, msEnvStruct *env);
int mssvr_write_to_ckid (int sd, char *ckid,
                msCkStruct *ckdata, msEnvStruct *env);
int mssvr_write_to_cksource (int sd, char *cksource,
                msCkStruct *ckdata, msEnvStruct *env);
int mssvr_write_to_ckname (int sd, char *fname,
                msCkStruct *ckdata, msEnvStruct *env);
void mssvr_receive_kernel (int sd, msEnvStruct *env);
void mssvr_send_kernel (int sd, msEnvStruct *env);
int mssvr_write_err_log (int sd, const char *sub, const char *mesg);
int mssvr_write_info_log (int sd, const char *sub,
                const char *mesg, int logmesg);
int mssvr_load_spk (int sd, double et, char *spk_ref,
        char *segid, msEnvStruct *env);

#ifdef __cplusplus
}
#endif

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
