/*
 *	spc.h	allocation of send and receive buffers
 */

/* txh::modified for vms include */
#if VMS_OS
#define DONT_DECLARE_MALLOC
#include <rpc.h>
#else
#include <rpc/rpc.h>
#endif

#define bzero(s,n)	memset(s,0,n)
#define	SERVER		1
#define CLIENT		0
#define TCP_PORT	2500		/* tcp port number */
#define BUFSIZE		131072		/* buffer size for message */
#define XDR_BUF		255		/* buffer size used by xdr */
#define SpcBadParam	-2	
#define S_INACTIVE	-1000

#define MODE_LOCAL	0
#define MODE_REMOTE	1
#define MODE_PRIVATE	2

/*----------------------------------------------------
 | Additional constants for spc_log subroutine which |
 | track of the number of client request	     |
 -----------------------------------------------------
*/
#define ASCII_FILE	1			/* added by SVL 6/19/1995   */
#define BINARY_FILE	2			/* to support file transfer */
#define INIT_SERVER     -100			/* and spice95		    */
#define NEW_CLIENT      -101
#define OLD_CLIENT      -102

#define STRING		0
#define INTEGER2	1
#define INTEGER4	2
#define REAL4		3
#define REAL8		4

#define BAD_PRJT_ID	-1			/* error code for c95	*/
#define BAD_ZDAFOPW	-2			/* added by SVL		*/
#define BAD_NAIF_KRNL	-3			/* 6/19/1995		*/
#define BAD_ZDAFPS	-4
#define BAD_ZDAFBNA	-5
#define BAD_ZDAFADA	-6
#define BAD_ZDAFCLS	-7
#define BAD_USR_INPUT	-8

#define BAD_USR_INFO    -1              /* error code for spice95       */
#define BAD_KDB_ID      -2		/* added by SVL			*/
#define BAD_BUF2SCET    -3		/* 6/19/1995			*/
#define BAD_UTC2ET      -4
#define BAD_SCTIKS      -5
#define BAD_GETIDS95    -6
#define BAD_PBID        -7
#define BAD_BODVAR      -8
#define BAD_C_MAT       -9
#define BAD_LOADSPK95   -10
#define BAD_SC_POS_VEL  -11
#define BAD_TG_POS_VEL  -12
#define BAD_CT_POS_VEL  -13
#define BAD_SC_RL_POS   -14
#define BAD_TG_RL_POS   -15
#define BAD_SUN_RL_POS  -16
#define BAD_EU_ANG	-17
#define BAD_ME_MAT	-18
#define BAD_OM_MAT	-19
#define BAD_TRGT2SC	-20
#define BAD_LON_LAT	-21
#define BAD_CAM_CONST	-22
#define BAD_CORCAV	-24
#define BAD_VADD	-25
#define BAD_MULT	-26
#define BAD_TSB_LAT_LON	-27

#define DEV_SYSTEM	"DEVELOPMENT"
#define OPS_SYSTEM	"OPERATIONS"
#define TEST_SYSTEM	"INTEGRATION_AND_TEST"

#if 0
/* These do not seem to appear anywhere in the system.  Commenting	*/
/* them out just in case they're needed again.  rgd 3/2010		*/
int	spclen;				/* length of message */
int	serv_socket;			/* server socket number */
int	cli_serv_flag;
int	spc_status;
#endif

/*******************
 ------------------------------------------------------
 | These 5 variables are commented out to avoid using  |
 | global variables. Caller should supply them instead |
 -------------------------------------------------------

XDR	xdrs;
char	*get_sendbuf;			
char	*get_rcvbuf;	
char	*sendbuf;
char	*rcvbuf;
********************/

static	char	**serv_name;

static	char	*op_serv_name[] = {	/* Names of servers 	*/
	"coda4.jpl.nasa.gov",		/*  shorten by SVL	*/
	"coda3.jpl.nasa.gov",		/* 6/19/1995		*/
	"lowe.jpl.nasa.gov",		/* only include 	*/
	0				/* sun-solr machines	*/
};

static 	char	*dev_serv_name[] = {
	"pinatubo.jpl.nasa.gov",
	"wind.jpl.nasa.gov",
	"telescope.jpl.nasa.gov",
	0
};

static	char	*test_serv_name[] = {
	"cascade.jpl.nasa.gov",
	"mezenc.jpl.nasa.gov",
	"mtgarfield.jpl.nasa.gov",
	0
};

/*-------------------------------
 | Return codes for zgetspice() |
 --------------------------------
*/
static char     *getspice_err[] = {
        "ZGETSPICE: Success",					/* 0   */
        "ZGETSPICE: Unrecognized/Unsupported Project",		/* -1  */
        "ZGETSPICE: Input Parameter Error",			/* -2  */
        "ZGETSPICE: Error Opening SEDR File",			/* -3  */
        "ZGETSPICE: Cannot Locate SEDR Record/No FDS Data",	/* -4  */
        "ZGETSPICE: Error Finding SEDR Record",			/* -5  */
        "ZGETSPICE: Unknown Error",				/* -6  */
        "ZGETSPICE: Error Transferring SEDR Record",		/* -7  */
        "ZGETSPICE: Error Closing SEDR File",			/* -8  */
        "ZGETSPICE: I/O Error Reading SEDR",			/* -9  */
	"ZGETSPICE: Invalid Indicator Code",			/* -10 */
        "ZGETSPICE: Error In Body Constants File",		/* -11 */
        "ZGETSPICE: Invalid Indicator Code", 			/* -12 */
        "ZGETSPICE: Invalid Number Of Arguments",		/* -13 */
        "ZGETSPICE: Invalid Indicator Code",                    /* -14 */
        "ZGETSPICE: Invalid System Argument",			/* -15 */
        NULL
        };

/*-------------------------------
 | Return codes for zputspice() |
 --------------------------------
*/
static char	*putspice_err[] = {
	"ZPUTGLLSPICE: Success",				/*  0  */
	"ZPUTGLLSPICE: Unrecognizable Project Id",		/* -1  */
	"ZPUTGLLSPICE: Error Opening Kernel File",		/* -2  */
	"ZPUTGLLSPICE: Cannot Update NAIF KERNEL",		/* -3  */
        "ZPUTGLLSPICE: Error Packing Seg. Descrptr",		/* -4  */
        "ZPUTGLLSPICE: Error Starting New Array", 		/* -5  */
        "ZPUTGLLSPICE: Error Adding New Array",			/* -6  */
        "ZPUTGLLSPICE: Daf-file Not Close Properly",            /* -7  */
	"ZPUTGLLSPICE: Bad User Input Data",			/* -8  */
	 NULL
	};
/*--------------------------------
 | Return codes for spice95()    |
 ---------------------------------
*/
static char     *spice95_err[] = {
        "SPICE95::Success",                                             /* 0    */
        "SPICE95::Please Check User Input",                             /* -1   */
        "SPICE95::Invalid User's Kernel Id",                            /* -2   */
        "SPICE95::buf2scet() Failed:Invalid SCET Data",                 /* -3   */
        "SPICE95::zutc2et() Failed:Invalid SCET Data",                  /* -4   */
        "SPICE95::zsctik() Failed:Invalid SCLK Tolerance",              /* -5   */
        "SPICE95::GetIds95() Failed:Unknown SC instrument",             /* -6   */
        "SPICE95::zpbid() Failed: Unknown Target",                      /* -7   */
        "SPICE95::zbodvar() Failed:Invalid Radii Returned",             /* -8   */
        "SPICE95::Cmatrix95() Failed:Invalid C Matrix Returned",        /* -9   */
        "SPICE95::LoadSpk95() Failed:Cannot Load SPK Files",            /* -10  */
        "SPICE95::zspkssb() Failed:Error Getting SC Pos Vel Vector",    /* -11  */
        "SPICE95::zspkssb() Failed:Cannot Get Target Pos Vel Vector",   /* -12  */
        "SPICE95::zspkssb() Failed:Cannot Get Center Pos Vel Vector",   /* -13  */
        "SPICE95::zspkapp() Failed:Cannot Get SC Relative Position",    /* -14  */
        "SPICE95::zspkapp() Failed:Cannot Get Target Reltv Pstn",       /* -15  */
        "SPICE95::zspkapp() Failed:Cannot Get Sun Reltv Pstn",          /* -16  */
        "SPICE95::zbodeul() Failed:Cannot Get Euler Angle",             /* -17  */
	"SPICE95::zeul2m() Failed:Bad ME-MATRIX Returned",		/* -18	*/
	"SPICE95::zmxmt() Failed:Bad OM-MATRIX Returned",		/* -19	*/
	"SPICE95::zminus() Failed:Bad Target2Sc State returned",	/* -20	*/
	"SPICE95::zreclat() Failed:Bad TCB Lon & Lat",			/* -21	*/
	"SPICE95::zgetcamcon() Failed:Bad Camera Constant",		/* -22	*/	
	"SPICE95::zcorcav() Failed:Abnormal Return Status(99, -1)",	/* -24	*/
	"SPICE95::zvadd() Failed:Error Adding Matrix",			/* -25	*/
	"SPICE95::zmxv() Failed:Error Multiplying Matrix",		/* -26	*/
	"SPICE95::zreclat(): Error Getting TSB Lat & Lon",		/* -27	*/
	NULL
	};
