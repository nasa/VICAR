/*				GLL_MAIN.H
 ******************************************************************************
 *	This file includes the basic data structures for the Galileo
 *	telemetry records.
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 *  7- 7-1989	N/A		Payam Zamani - Original Delivery
 * 23- 9-1989   N/A                          - Added MINIMUM and MAXIMUM macros
 ******************************************************************************
 */

/*
 *=============================================================================
 *	Definitions
 *=============================================================================
 */
#define FOREVER			while(1)
#define RESET			0
#define SET			1
#define OFF			0
#define	ON			1

#define	V_ERR			1	/* VMS TYPE ERRORS	      */
#define	C_ERR			2	/* `C' TYPE ERRORS	      */
#define	G_ERR			3	/* GENERAL TYPE ERRORS	      */

#define	NIMS			1
#define	PWS			2
#define	SSI			4

#define	POSTMSG(l,e,t,m)	if (l<=gcb.msg_lvl) postmsg( e,t,m);
#define MINIMUM(p1, p2)		((p1<p2)?p1:p2)
#define MAXIMUM(p1, p2)		((p1>p2)?p1:p2)

#define	BOOM_FLAG_YES		0
#define	BOOM_FLAG_MAYBE		1
#define	BOOM_FLAG_NO		2
#define BOOM_FLAG_UNKNOWN	3

/*
 *=============================================================================
 *	Data types
 *=============================================================================
 */
typedef	char			BYTE;
typedef unsigned int		FIELD;
typedef	unsigned char		FLAG;
typedef int			INT;
typedef unsigned char		UBYTE;
typedef	unsigned int		UINT;
typedef unsigned short		UWORD;
typedef	short			WORD;


/* FORMAT_ID
 *=============================================================================
 *
 *	    15  14  13  12  11  10  9   8   7   6   5   4   3   2   1   0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 1 | RT ident          | Ro| Cmm mp| Map Sep   | Rec Id            | 0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 *	Rec id		Record identifier (GLL-3-280 table 1)
 *			Constant values defined in GLLCONST.H
 *
 *	Map Seq		Map sequence number. Number of times commutation map
 *			has changed.
 *			
 *
 *	Cmm mp		Commutation map (GLL-3-280) sec 3.9.2.3
 *				0 = Anomaly Investigation
 *				1 = Calibration
 *				2 = Maneuvers
 *				3 = Cruise/Encounter/Orbital Ops.
 *
 *	Ro		Memory readout	0 = Variable engineering is present
 *					1 = Memory readout data is present
 *
 *	RT ident 	Realtime identifier (GLL-3-280 table 1)
 *			Constant values defined in GLLCONST.H
 *
 * ============================================================================
 */

typedef	struct					/* Format Idendtification    */
	{
	FIELD		rec_id	 : 5;		/* Record identifier	     */
	FIELD		map_seq  : 3;		/* Map sequence no.	     */
	FIELD		comm_map : 2;		/* Commutation map	     */
	FIELD		mem_ro	 : 1;		/* Memory readout	     */
	FIELD		rt_id	 : 5;		/* Real Time Identifier      */
	}
	fid_typ;

/* SPACECRAFT_CLOCK
 *=============================================================================
 *
 *	    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 1 ! RIM							   | 0
 *         +-------------------------------+				   +
 *	 3 | MOD 91			   |				   | 2
 *	   +-------------------------------+-------------------------------+
 *	 5 | MOD 8			   | MOD 10			   | 4
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 *	RIM		Real_time Image Counter. 24 bits, increments every
 *			60-2/3 seconds.	Ranges from 0 to 1677215.
 *
 *	MOD 91		8 bit counter, increments every 2/3 seconds. Ranges
 *			from 0 to 90.
 *
 *	MOD 10		8 bit counter, increments every 66-2/3 milliseconds.
 *			Ranges from 0 to 9.
 *
 *	MOD 8		8 bit counter, increments every 8-1/3 milliseconds.
 *			Ranges from 0 to 7.
 *
 * ============================================================================
 */

typedef	struct
	{
	FIELD	rim 	: 24;			/* Real time image count */
	UBYTE	mod91;				/* mod 91 counter	 */
	UBYTE	mod10;				/* mod 10 counter	 */
	UBYTE	mod8;				/* mod 8 counter	 */
	}
	sc_sclk_typ;

typedef	struct
	{
	UINT	rim;				/* Real time image count */
	UBYTE	mod91;				/* mod 91 counter	 */
	UBYTE	mod10;				/* mod 10 counter	 */
	UBYTE	mod8;				/* mod 8 counter	 */
	}
	sclk_typ;

/* TELEM_HEADER
 *=============================================================================
 *
 *	    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 1 | FSC (PN)							   | 0
 *	 3 |								   | 2
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 5 | FID							   | 4
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 7 | SCLK							   | 6
 *	 9 |								   | 8
 *	11 |								   | 10
 *	13 |								   | 12
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 *	FSC		Frame Synchronization Code (PN code) = 0x03915ED3
 *
 *	FID		Format Identification. Defined by structure `format_id'
 *
 *	SCLK		Space craft clock. Defined by structure
 *			"spacecraft_clock".
 *
 *=============================================================================
 */

typedef	struct
	{
	int			pn_code;
	fid_typ			frmt_id;
	sc_sclk_typ		sclk;
	}
	tlm_hdr_typ;

/* EARTH_RECEIVED_TIME & SPACECRAFT EVENT TIME
 *=============================================================================
 *
 *	    15  14  13  12  11  10  9   8   7   6   5   4   3   2   1   0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 1 | day			       | year			   | 0
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *	 3 | millsec			   | minute			   | 2
 *	   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 *	year			(year - 1900)
 *
 *	day			day of the year
 *
 *	minute			minute of day
 *
 *	millsec			millisecond of minute
 *
 *=============================================================================
 */

typedef	struct
	{
	UWORD			year;		/* YEAR 		     */
	UWORD			day;		/* DAY OF YEAR		     */
	UBYTE			hour;		/* HOUR OF DAY		     */
	UBYTE			minute;		/* MINUTES OF HOUR	     */
	UBYTE			second;		/* SECONDS OF MINUTE	     */
	UWORD			msecond;	/* MILLISECOND OF SECOND     */
	}
	ert_typ;

typedef	ert_typ		scet_typ;


/*
 *=============================================================================
 *=============================================================================
 */

typedef	struct
	{
	UBYTE		frmt;
	FLAG		pb_rt;			/* 0:RT, 1:ASYNCH PLAYBACK    */
	sclk_typ	sclk;
	ert_typ		ert;
	}
	current_typ;

/*
 *=============================================================================
 *=============================================================================
 */
typedef	BYTE		byte_ballot_typ[8];


#define	RT_TLM		0
#define PB_TLM		1
static	char	*tlm_mode[2]  = { "RT", "PB"};
