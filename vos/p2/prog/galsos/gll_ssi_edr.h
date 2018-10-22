/*				GLL SSI EDR
 ******************************************************************************
 *	Each SSI EDR file contains 2 header records and one 800 line image.
 *	The structure of EDR file for SSI is defined in detail in the
 *	"SSI EDR Software Interface Specification".
 *
 * NOTES:
 *	This file uses some symbols & structures defined in MAIN_GLL.H
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 * 29-Apr-1991	64569		AJR - Added RS_OVERFLOW field to line header
 *  7- 7-1989	N/A		Payam Zamani - Original Delivery
 ******************************************************************************
 */

typedef	struct
	{
	FIELD		comp	: 1;	/* Set for compressed imaging formats*/
	FIELD		cmp_mode: 1;	/* Compression mode. valid if comp=1>*/
					/*> 0: rate control, 1: info. prsrvng*/
	FIELD		expo	: 1;	/* exposure mode 0:normal, 1:extended*/
	FIELD		flood	: 1;	/* Light flood status 0:off, 1:on    */
	FIELD		blem	: 1;	/* Blemish protection 0:off, 1:on    */
	FIELD		clock	: 1;	/* Parallel clock, 0:normal, 1:invert*/
	FIELD		filler	: 10;
	}
	ssi_flg_typ;

typedef	struct
	{
	UBYTE		sync_p;			/* Sync param. P	     */
	UBYTE		sync_i;			/* Sync param. I	     */
	FIELD		sync_l	: 5;		/* Sync param. L	     */
	FIELD		sync_k	: 5;		/* Sync param. K	     */
	FIELD		sync_j	: 5;		/* Sync param. J	     */
	FIELD		filler1 : 1;
	FIELD		sync_r	: 5;		/* Sync param. R	     */
	FIELD		sync_n	: 5;		/* Sync param. N	     */
	FIELD		sync_m	: 5;		/* Sync param. M	     */
	FIELD		filler2 : 1;
	}
	sync_par_typ;

typedef	struct
	{
	char		ra[8];			/* RIGHT ASCENSION	     */
	char		dcl[8];			/* DECLINATION		     */
	char		twst[8];		/* TWIST		     */
	}
	j2000_ascii_typ;

/*
 *=============================================================================
 * 	SSI EDR file header
 *=============================================================================
 */
typedef	struct	
	{
	UBYTE		record_number;		/* Always = 0 for header     */
	UBYTE		file_number;
	char		project[10];		/* Project name, "GALILEO   "*/
	char		instrument[6];		/* Inst. name, "SSI   "      */
	UWORD		phy_seq;		/* Physical sequence	     */
	UWORD		log_seq;		/* Logical sequence	     */
	ert_typ		fert;			/* FIRST EARTH RECEIVED TIME */
	ert_typ		lert;			/* LAST EARTH RECEIVED TIME  */
	sclk_typ	fsclk;			/* SCLK of the first record  */
	sclk_typ	lsclk;			/* SCLK of the last record   */
	scet_typ	scet;			/* SCET of the first record  */
	char		mipl_prd[59];		/* MIPL Phys. Rec. Data      */
	UWORD		format_id;		/* Corrected by voting alg.  */
	UINT		sync_err;		/* Sum of all bad bits       */
	FLAG		boom;			/* BOOM: no=2, maybe=1, yes=0*/
	UWORD		missings;		/* # of invalid lines        */
	UWORD		partials;		/* # of partial lines        */
	UWORD		unreadables;
	UWORD		seq_brk;		/* # of IDR,SDR gaps         */
	UWORD		src_inp;		/* Logical sum if src/inp fld*/
	UWORD		wbdl;			/* # of frames for WBDL      */
	UWORD		sdrs;			/* # of frames for SDR	     */
	UWORD		sfdus;			/* # of frames for SFDU	     */
	char		pic_no[7];		/* Picture number	     */
	ssi_lrs_p_typ	ssi_lrs;		/* SSI's LRS packet	     */
	ssi_flg_typ	flags;			/* Various flags	     */
	char		dn[6];			/* Mean DN level of picture  */
	char		trunc_bits[6];		/* Mean truncated bits/pixel */
	char		trunc_pixel[6];		/* Mean truncated pixel/line */
	char		i_f[12];		/* Mean I/F level	     */
	char		entropy[7];		/* Average entropy for pictr */
	char		entropies[15][7];	/* Entropy leve for 15 lines */
	j2000_ascii_typ	point_dir;		/* Pointing direction	     */
	char		scale[2][8];		/* Scale factors	     */
	char		slope[32];		/* Slope file name	     */
	char		offset[32];		/* offset file name	     */
	char		activity[20];
	char		filler_1;
	UBYTE		filter;			/* Filter position	     */
	UBYTE		exposure;		/* Exposure Number	     */
	UBYTE		image_mode;		/* Imaging mode		     */
	UBYTE		gain;			/* Gain state		     */
	UINT		range;			/* Target's range to Sun- km */
	UBYTE		tlm_frmt;		/* Telemetry format	     */
	short		version;		/* Catalog version number    */
	sclk_typ	ssclk;			/* Starting SCLK	     */
	sclk_typ	esclk;			/* Ending SCLK		     */
	char		reserved[318];
	UINT		hist[256];			
	}
	ssi_edr_hdr_typ;

typedef	struct
	{
	UBYTE		type;		/* Input type :			    >*/
					/*>	0 - S/C Flight Data MOS     >*/
					/*>     1 - PTM Data		    >*/
					/*>	2 - External Simulation     >*/
					/*>     3 - S/C Flight Data Test    >*/
					/*>     4 - Internal Simulation     >*/
					/*>     5..255 Not Used              */
	FIELD		SFDU	: 1;    /* Set if SFDU data present in rec.  */
	FIELD		WBDL	: 1;	/*    ""  WBDL  ""    ""     ""      */
	FIELD		SDR	: 1;	/*    ""  SDR   ""    ""     ""      */
	FIELD		IDR	: 1;	/*    ""  IDR   ""    ""     ""      */
	FIELD		EDR	: 1;	/*    ""  EDR   ""    ""     ""      */
	FIELD		RT	: 1;	/*    ""  Real-Time   ""     ""      */
	FIELD		APB	: 1;	/*    ""  APB   ""    ""     ""      */
	FIELD		filler2	: 1;
	}
	input_typ;

typedef	struct
	{
	FIELD		error	: 14;	/* # of errors in 64-bit frame id    */
	FIELD		status	:  2;	/* sysc status :		    >*/
					/*>	00 - Fully synched	    >*/
					/*>     01 - Partially synched      >*/
					/*>     11 - Unsynched		     */
	}
	sync_stat_typ;

typedef	struct
	{
	FIELD		blk0  : 2;
	FIELD		blk1  : 2;
	FIELD		blk2  : 2;
	FIELD		blk3  : 2;
	FIELD		blk4  : 2;
	FIELD		blk5  : 2;
	FIELD		blk6  : 2;
	FIELD		blk7  : 2;
	FIELD		blk8  : 2;
	FIELD		blk9  : 2;
	FIELD		blk10 : 2;
	FIELD		blk11 : 2;
	FIELD		blk12 : 2;
	FIELD		filler: 6;
	}
	trunc_typ;

/*
 *=============================================================================
 *	SSI EDR line header
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		rec_id;			/* Record identification = 2 */
	UBYTE		file_no;		/* File number		     */

	UWORD		phy_seq;		/* Physical sequence	     */

	UWORD		log_seq;		/* Logical sequence	     */
	ert_typ		ert;			/* Earth Received Time	     */
	sclk_typ	sclk;			/* Spacecraft clock	     */
	char		mpr[59];		/* MIPL physical recording   */

	UWORD		fid;			/* 16-bit corrected FID      */
	input_typ	input;			/* input source/type	     */

	UBYTE		allowed_sync_err;	/* Allowed sysncode errors   */
	UBYTE		sync_err;
	UBYTE		ssi_lrs[12];		/* SSI LRS packet	     */
	UWORD		last_pix;		/* Last pixel position       */
	sync_stat_typ	sync_stat;
	trunc_typ	truncation;		/* Truncated bits per block  */
	UWORD		truncated;		/* # of trunc. pixels	     */
	UWORD		version;		/* Catalog version number    */
	UWORD		ssnr;			/* GCF symbol signal to noise*/
	UBYTE		dsn_id;			/* As in GLL-820-13/OPS-6-8  */
	UWORD		image_line;		/* Line number 1..800        */
	FLAG		rs_overflow;		/* RS overflow during decode */
	UBYTE		filler[83];
        }
	ssi_lhdr_typ;

/*
 *==============================================================================
 *	Definition of one EDR line
 *==============================================================================
 */
typedef	struct
	{
	ssi_lhdr_typ	hdr;			/* EDR line header	     */
	short		pixel[800];		/* Pixel data.		     */
	}
	ssi_edr_ln_typ;

typedef	struct
	{
	ssi_lhdr_typ	hdr;			/* EDR line header	     */
	UBYTE		pixel[800];		/* Pixel data.		     */
	}
	ssi_udr_ln_typ;
