/******************************************************************************
 *				SPICEINC.H
 *
 *	This file includes the definitions, data structures, and function 
 *	prototypes for SPICE interface routines.
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	-----------------------------------------------
 * 10-Aug-1989	N/A		Sheila Tews - Original Delivery
 * 07-Nov-1990                  Gary Yagi   - New C-kernel format
 * 09-Feb-1994                  Thuy Truong - ported to unix
 * 16-Jun-1994 FR 85081         Thuy Truong - changed LEN_SPICE_ID to 4
 * 06-Apr-1995			Sam Le	    - changed MAX_KERNELS to 256
 *					      struct buf_union_typ
 *					    - add new constant to support
 *					      selection of data
 * 16-Apr-1997			Sam Le	    - Added MAX_KERNEL_LOADED 20
 *					      This limits the total number of
 *					      kernels (both CK & SPK) that
 *					      can be loaded simultaneously.
 *					      The NAIF toolkit allows only
 *					      20 kernels at a time. This was
 *					      inscreased from 10 since version
 *					      42 (or is it 40 ???).
 *					      The LoadSpk95() & LoadCk95()
 *					      will only load up 20 kernels.
 *					      These will return ERROR message
 *					      if the caller tries to go pass
 *					      20.
 *					      IMPORTANT: try to unload any
 *						kernel that you don't need
 *						anymore to save memory...If
 *						you ignore the error message,
 *						NAIF Toolkit will not do
 *						allow you to do anything !!!!
 * 28-Jan-2004		Vance Haemmerle	    - changed MAX_KERNELS to 8192
 *
 ******************************************************************************
 */

/*
 *=============================================================================
 * Definitions
 *=============================================================================
 */

#define MAX_KERNEL_LOADED 20	/* added by svl (4/16/1997)		*/

#define	LEN_CRAFT	5	/* Miscellaneous field lengths		*/
#define	LEN_TARGET	12
#define	LEN_INSTRUMENT	5
#define	LEN_SCET	24	/* spice's scet length			*/
#define	LEN_SOURCE	5
#define	LEN_SYSTEM	5
#define	LEN_KTYPE	3
#define LEN_SPICE_ID    4 
#define LEN_FILENAME	100
		
#define	LEN_DATE_TIME	12		/* new constant to support	*/
#define LEN_SP_C	4		/* selection of data		*/
#define LEN_PROG_NAME	6		/* entered: 4/17/95 by: svl	*/
#define LEN_PURPOSE	4
#define LEN_REQ_NO	4
#define LEN_SP_REF	4	
#define LEN_USR_ID      6
#define LEN_FILE_NAME	256

#define POS_DATE_TIME	168		/* new constants entered 	*/
#define POS_SP_C	171		/* 4/17/95 by: svl		*/
#define POS_PROG_NAME	173
#define POS_PURPOSE	172
#define POS_REQ_NO	175
#define POS_SP_REF	13
#define POS_USR_ID	176
#define POS_SOURCE	10

#define	INVALID_INPUT	-2	/* SPICE89 error status returns		*/
#define	INSUFF_DATA	-4
#define	PROGRAM_BUG	-6

#define	NIMS_NO_SPICE	-1	/* NIMS_SPICE errors			*/
#define	NIMS_BAD_SYS	-2
#define	NIMS_NO_TARG	-3
#define	NIMS_NO_C	-10	/* (lwk) */

#define	INSUFF_EPHEMERIS	-8
#define	INSUFF_POINTING		-10

#define	SUCCESS		1	/* internal SPICE89 status returns	*/
#define	FAILURE		0

#define	MAX_KERNELS	8192	/* Maximum # of kernels to be read in	*/
#define	NUM_KERNELS	14	/* Number of kernels available		*/
#define	MAX_CK		30	/* Max number of CKs you can load	*/

#define	NUM_SPK_DESCR	5	/* Various NAIF SPICE constants		*/
#define	NUM_CK_DESCR	7
#define	NUM_CK_DOUBLES	2
#define	NUM_CK_INTEGERS	5
#define	CK_IDENT_LEN	40
#define	SPK_IDENT_LEN	40
#define	DAF_IFN_LEN	60

#define	MAX_SP_SC	100	/* Max spacecraft segments per kernel	*/
#define	MAX_SP_BODY	100	/* Max body segments per kernel		*/
#define	MAX_C_SUMM	1000	/* Max body segments per kernel		*/

#define	SPK		1	/* kernel ids				*/
#define	CK		2
#define	EK		3
#define	IK		4
#define	SK		5
#define	PK		6
#define	ALL_K		7

#define	BODY_ID		1	/* switch setting for get_body...	*/
#define	BODY_NAME	2

#define	VGR_1		-31	/* spacecraft ids consistent with NAIF	*/
#define	VGR_2		-32
#define	GLL		-77

#define	ISSWA		2	/* camera #s consistent with NAIF	*/
#define	ISSNA		1
#define	PLATFORM	1	/* changed Nov 10 90 (gmy), was=2 */

#define	J2000		1	/* Indices to reference frames		*/
#define B1950		2

#define	ibuf(i)		(buf->intbuf[i-1])	/* output buffer	*/
#define	dbuf(i)		(buf->doublebuf[i-1])	/* references		*/

#define	SCET_YEAR	2	/* location of scet entities 		*/
#define	DASH		4
#define	SCET_DAY	5
#define	SLASHES		8
#define	SCET_HOUR	12
#define	SCET_MIN	15
#define	SCET_SEC	18
#define	SCET_MILL	21

#define	CK_SORC_POS	5
#define	CK_KID_POS	10
#define	CK_DATE_POS	19
#define	CK_USER_POS	28
#define	CK_KID_LEN	8
#define	CK_DATE_LEN	8
#define	CK_USER_LEN	6
	
/*
 *=============================================================================
 * Data Structures
 *=============================================================================
 */
typedef struct body_id_typ            /*Collection of body ids */
{
  int sc;
  int target;
  int center;
  int  sol;
} body_id_typ;

typedef	struct pointing_typ	/* pointing information from C kernel	*/
{
  double	ra;
  double	dec;
  double	twist;
}	pointing_typ;

typedef	struct timout_typ	/* time for which returned pointing	*/
{				/* is valid				*/
  double	scl;
  double	et;
}	timout_typ;

typedef	struct radii_typ	/* planet/satellite radii form SPICE	*/
{				/* constants				*/
  double	semi_major;
  double	semi_minor;
  double	polar;
}	radii_typ;

typedef	union buf_union_typ	/* SPICE89 output buffer type		*/
{
  double	doublebuf[100];
  int		intbuf[200];
}	buf_union_typ;

typedef	struct sc_summary_typ
{
  int		spacecraft;
  double	begin_et;
  double	end_et;
}	sc_summary_typ;

typedef	struct body_summary_typ
{
  int		body;
  double	begin_et;
  double	end_et;
}	body_summary_typ;

typedef	struct sp_summary_typ
{
  int			sc_cnt;
  int			body_cnt;
  sc_summary_typ	sc_summ[MAX_SP_SC];
  body_summary_typ	body_summ[MAX_SP_BODY];
}	sp_summary_typ;

typedef	struct point_summary_typ
{
  int		spacecraft;
  int		instrument;
  double	begin_et;
  double	end_et;
}	point_summary_typ;

typedef	struct c_summary_typ
{
  int			count;
  point_summary_typ	point[MAX_C_SUMM];
}	c_summary_typ;

typedef union begin_typ
{
  char sclk_begin[18];
  double et_begin;
}       begin_typ;

typedef union end_typ
{
  char sclk_end[18];
  double et_end;
}       end_typ;

typedef	struct kernel_db_typ
{
  char	     id[LEN_SPICE_ID];
  char	     isloaded;
  int	     type, handle;
  char	     source[LEN_SOURCE];
  char	     filename[LEN_FILE_NAME];
  char       scet_begin[21];
  char       scet_end[21];
  begin_typ  range_begin;
  end_typ    range_end;
}	kernel_db_typ;

typedef struct usr_kdb_typ			/* values supplied by	*/
{						/* usr via buf		*/
  int	     type,				/* file type: ck,spk,...*/
	     handle;				/* file handle		*/
  char	     date_time[13],			/* date & time of reqst */
	     filename[256],			/* filename & id of 	*/
	     sp_c_id[5],			/* file to search	*/
             prog_name[7],			/* name of prog created	*/
             purpose[5],			/* the data & why 	*/
             req_no[5],				/* request/job number	*/
	     source[6],				/* file source to srch/	*/
             sp_ref[5],				/* or write to		*/
 	     usr_grp_id[7],			/* usr/grp id		*/
	     segid[41];
	} usr_kdb_typ;

typedef struct prov_info_typ
{
 char	inst[5],
	purpose[5],
	prog_name[7],
	sp_ref[5],
	req_no[5],
	year[5],
	month_day[5],
	hour_min[5],
	file_id[5],
	usr_grp_id[7];
	} prov_info_typ;

typedef	struct basics_typ
{
  double c_ra;			/* C-matrix euler angles */
  double c_dec;
  double c_twist;
  double me_ra;			/* ME-matrix euler angles */
  double me_dec;
  double me_twist;
  double center_craft_pos[3];	/* central-body-to-spacecraft vector */
  double center_craft_vel[3];
  double craft_target_pos[3];	/* spacecraft-to-target vector */
  double craft_target_vel[3];
  double craft_sun_pos[3];	/* spacecraft-to-sun vector */
  double craft_sun_vel[3];
}	basics_typ;		/* vectors are in celestial coordinates */

typedef struct nims_typ	      /* For NIMS_SPICE interface		*/
{
  pointing_typ	c;
  pointing_typ	me;
  double	sc_vector[3];
  double	solar_vector[3];
}	nims_typ;

/*
 *=============================================================================
 * Function Prototypes
 *=============================================================================
 */

/* NAIF Functions							*/

#if SUN_UNIX

     void	zreset();	/* Resets error reporting		*/
     int	zfailed();	/* Checks for SPICE routine failure	*/
     double	zdpr();		/* Degrees per radian			*/
     double	zhalfpi();
     
/* SPICE utilities in SPICESUBS.COM					*/

     int	get_body_ids( char *, char *, body_id_typ * );
     int	get_body( int, int *, char * );
     void	chgvec( double *, int, int, double * );
     void	chgmat( int, int, double * );
     void    combine_scet(short,short,short,short,short,short,char *);
     void    split_scet(char *,short *,short *,short *,short *,
			short *,short *);
     void	get_latlon( radii_typ , double ,double *,double * );
     int	load_spice( int );
     int	get_kdb();
     void	eul2mat( double, double, double, double * );
     int	summarize_spk( int , sp_summary_typ * );
     int	summarize_ck( int , c_summary_typ * );
     void	massage_string( char *, char *, int );
     int     get_basics(body_id_typ ,int ,double ,double ,double ,
			char ,basics_typ *,double *,double *);

#else

    void	zreset();	
    int	zfailed();	
    double	zdpr();		
    double	zhalfpi();
  
/* SPICE utilities in SPICESUBS.COM					*/
    int	get_body_ids();
    int	get_body();
    void	chgvec();
    void	chgmat();
    void    combine_scet();
    void    split_scet();
    void	get_latlon();
    int	load_spice();
    int	get_kdb();
    void	eul2mat();
    int	summarize_spk();
    int	summarize_ck();
    void	massage_string();
    int     get_basics();

#endif
