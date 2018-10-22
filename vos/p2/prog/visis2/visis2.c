/*******************************************************************************

Program:	VISIS2

Purpose: 	This VICAR program provides a translation of NIMS VICAR files 
		consisting of two dimensional histogram, spectral plots,
		spectral index image, geometry cube, and merged mosaic cube
	        into one ISIS cube file containing all of these objects, 
		and having a PDS label.  Also, an ISIS/PDS cube of the same 
		construction can be broken into VICAR files.

VISIS Written by: Justin McNeill, Jr. , October 1989
Revisions:
10Dec94	-lwk- ported to Alpha and converted to GLL Phase2, renamed VISIS2
10feb95 -lwk- added Sybase interface
11sep95 -lwk- extended to support phase-0 s/w too (no separate ported VISIS)
19sep95 -lwk- fixed error that caused WAVELENGTHS, etc., to be missing;  added
	pointing source & start/stop sub-solar label items;  added 1 to map
	offsets per PDS def'n;  fixed slew_tol in inverse mode
19oct95 -lwk- corrected code to determine if cube is tube in inverse mode
28nov95 -lwk- fixed code for East Long. determination 
14dec95 -lwk- MOSNUM is now in phase-1 label too
10jan96 -lwk- fixed East Long. determination in inverse mode
16feb96 -lwk- fixed NATIVE_..._TIME mf format; band_bin mode determination;
	zlgets for DRK_AVE and RAD_SENS; North Angle is mpNORTH_ANGLE, not
	mpCARTESIAN_AZIMUTH for Perspective;
21feb96 -lwk- corrected SSC and OAL Line/Samps in inverse mode:  the label items
	are _offsets_ (relative to 1,1), although the MP items are not
28feb96 -lwk- initialize astretch[] & r/g/b_siid items to prevent illegal
	floating-point errors
06mar96 -lwk- fixed NATIVE_..._TIMEs again to avoid embedded blanks
20mar96 -lwk- minor fixes to avoid Unix build warnings
23mar96 -lwk- removed JR/JSA Target ids per PDS decision
10may96 -lwk- made SP_KERNEL_FILE_NAME independent of phase (0/2), added
	BAND_BIN_SELECTED_BAND label array;  allow for variable number of
	backplanes due to deselected GPs
29jun96 -lwk- removed underscores from NEAR_INFRARED_MAPPING_SPECTROMETER ISIS
	label item
06aug96 -lwk- ensure OBSNAME is zero-terminated;  changed waves arrays since
	NIMSCMM2 now gives array after WET deselection
15aug96 -lwk- fixed special values in floating-point case (missing final #,
	and low/hi instr.rep. keywords)
25aug96 -lwk- added BAND_MASK to output cube label in inverse mode
27aug96 -lwk- removed BAND_BIN_SELECTED_BAND, changed BAND_BIN_ORIGINAL_BAND
	to the former's definition
11sep96 -lwk- fixed error in COORDINATE_SYSTEM_NAME when Planetocentric
27sep96 -lwk- removed all planetocentric/detic latitude conversions, because
	NIMSCMM2 now writes either one consistently
05oct96 -lwk- removed underscores (_) from keyword names inside comment NOTES
	to avoid the parser confusing these with the keywords themselves in
	inverse mode
09oct96 -lwk- added photometric correction cutoff
31oct96 -lwk- allowed multiple Cal files; added min/max central body label items
05nov96 -lwk- added grating correction and instrument threshold label items;  
	changed min/max "target_center" distances to "slant"
08nov96 -lwk- fixed POINTING_OFFSET label item
28feb97 -lwk- added PHOTOMETRIC_CORRECTION_NOTE, moved POINTING_OFFSET to next
	to other pointing stuff (in History) per RM request
13may97 -lwk- changed SUB_SOLAR_AZIMUTH to SOLAR_AZIMUTH; put a copy of
	POINTING_OFFSET back into Label so that ISIS s/w can access it
15may97 -lwk- added GR_POS_nn_RIGHT_EDGE_PROJ_LINE/SAMPLE backplane names in 
	footprint tube;  increased LABELMEMORY (from 60000 to 72000) to
	acommodate extra backplanes
22may97 -lwk- fixed bug whereby the GR_POS_nn_PROJECTED_LINE & _SAMPLE 
	backplane names were interchanged in the BAND_SUFFIX_NAME array;
	move STD_DEV_SELECTED_BACKPLANE from history to label in order to
	make it available to ISIS label routines
27may97 -lwk- added Lommel-Seeliger photometric correction;  removed reference
	to tube/cube in "Qube structure" label comment
29may97 -lwk- fixed NATIVE_START/STOP_TIME format in inverse mode for cube
	output, removed these from 2D-hist and specplots;  added parameter
	HISTFILE and the capability of storing ISIS history labels
01jun97 -lwk- get obsext/mosnum from qube name in inverse mode and write them
	to vicar label, so that a following forward-mode run will be able
	to retrieve them (needed for catalog lookup)
10jul97 -lwk- save list of cube history tasknames and use these to add to
	the ISIS VICAR history
08aug97 -lwk- added quotes to Lommel-Seeliger keyword (for Unix ISIS)
14sep97 -lwk- label changes (NATIVE_TIMEs in quotes, most _NOTEs converted
	to comments) requested by PDS;  fixed conversion of NATIVE_TIME in
	inverse mode:  make get_string_value ignore double quotes (");  
	omit single quotes (') from OBSERVATION_NAME
16sep97 -lwk- remove quotes from NATIVE_TIMEs for now, in order to give ISIS
	s/w a chance to prepare for this
20sep97 -lwk- added SPECIALP parameter and SPECIAL_PROCESSING label stuff
 1oct97 -lwk- added SUPPNOTE, combined all qube NOTEs into one; removed NOTEs 
	from 2D-Hist and Spectra in inverse mode;  revised SPECIAL_PROCESSING 
	comment per RM intructions and moved it to the label (after the _TYPE 
	item)
 6oct97 -lwk- don't write geometry blackplanes and many label items for 
	CALIBRATION data;  fixed a few label bugs in inverse mode (SCET format,
	GRATING_STEPS/DELTA)
15oct97 -lwk- add single quotes (') back to OBSERVATION_NAME since these begin 
	with a numeric from C10 on;  not double quotes, since this item is a
	"literal", not a "string" (PDS distinction!)
16oct97 -lwk- changed SINUSOIDAL to 'SINUSOIDAL_EQUAL-AREA' in Map proj'n type;
	write true GP in the GR_POS_nn tube backplanes
19oct97 -lwk- changes for Unix ISIS compatibility:  added EAST/WESTERNMOST_LONG
	keywords (redundant with MINUMUM/MAXIMUM);  added OFFSET_DIRECTION, with
	switch of signs in LINE/SAMPLE_PROJECTION_OFFSET when TO_ORIGIN;  also,
	LINE/SAMPLE_PROJECTION_OFFSET should *not* have 1 added if obtained 
	thru MP calls, as those are already relative to (1,1) (since jan'94)
	-- only if mpo2buf/buf2mpo used, and for Perspective, where 
	PLANET_CENTER_LINE/SAMPLE are not offsets;  only convert Longitudes
	if POSITIVE_LONGITUDE_DIRECTION needs to be changed!
 5nov97 -lwk- added RADIANCE_FACTOR to core types
29nov97 -lwk- added MAXIMUM_PIXEL_DISTORTION label item, and comments 
	describing the footprint algorithm and label items
 9dec97 -lwk- write TO_ORIGIN keyword to label for Perspective too!  added 
	TO_ORI parameter to allow user to correct for cubes made without this;
	support B_AXIS_RADIUS in inverse mode (mpbuf2mpo uses this only for
	POV case, so should really convert to pure MP calls if this is 
	significant for other projections)
13feb98 -lwk- fixed precision of NATIVE_..._TIME conversion;  allowed multiple
	dark files (for NIMS98 calibration)
15feb98 -lwk- partially re-enabled quotes around NATIVE_..._TIMEs, since ISIS 
	is now supposed to be able to handle this -- however, for now just
	write spaces where the quotes will go, as a safer course (this allows
	simple hand-editing);  write CUBE_SIZE label item in inverse mode for 
	tube, so that a subsequent forward VISIS2 can recognize it as tube
18feb98 -lwk- fully re-enabled quotes around NATIVE_..._TIMEs
 8mar98 -lwk- added RTI to NATIVE_START_TIME for the sake of the Spike file
	conversion program
12mar98 -lwk- added keyword USECAT/NOCAT to allow suppression of catalog use
 6apr98 -lwk- don't use AACSFILE item for Calibration data;  write more items
	to label for Calibration file (solar/cenbody distances, band_suffix...
	items)
 8apr98 -lwk- fixed START/STOP times to handle cubes made in inverse mode;
	don't check task of cocube
21apr98 -lwk- changed double quotes around LOMMEL-SEELIGER to single
29apr98 -lwk- fixed code reading NATIVE_START_TIME in inverse mode to cover 
	case of old cubes where this has only two fields;  also fixed 
	NATIVE_STOP_TIME
 7may98 -lwk- allowed CORE_UNIT = DIMENSIONLESS (inverse mode)
13may98 -lwk- removed references to BELOW_THRESH  & MISSING_SENSITIVITY from 
	label
14may98 -lwk- changes due to renaming of CALIBRATION target to CAL/SKY
25may98 -lwk- changed CORE_NAME, CORE_UNIT, PRODUCT_ID for IOF cube
27may98 -lwk- added BREAK to Histogram description;  also, get this from
	HIST2D label since that's always present (but spec.plots may not be)
 7jun98 -lwk- revisions to support PTUB/GTUBs (note that Inverse mode for 
	this is TBD -- may never be needed!);  increased LABELMEMORY again 
	(to 84000) for extra backplanes
 9jun98 -lwk- revised Special Processing comment;  fixed "Footprint" note for
	tube case
22jun98 -lwk- added cutoff & sensitivity ratios for hi-gain thermal regime
	(label items only)
24jun98 -lwk- fixed processing of HISTFILE & associated label items
29jun98 -lwk- more fixes to HISTFILE (etc.) processing, for 2nd VISIS2 run
	on a cube;  ensure that VERSION_DATE reflects pgm. version
12jul98 -lwk- ensure that COORDINATE_SYSTEM_NAME=PLANETOGRAPHIC is written
	correctly in inverse mode (same fix as for NIMSCMM2 in feb97)
28jul98 -lwk- add check for zero meridian being in image when determining
	EAST/WESTERNMOST_LONGITUDE
12aug98 -lwk- added message to above check;  adjust cen_lon to (-180,180)
	range if min/max lon's are in this too
13aug98 -lwk- fixed mpxy2ll call for tube case
14aug98 -lwk- disabled mpxy2ll call and entire zero meridian check (since
	nimscmm2 does it right now), because it still fails for certain
	tube cases (e.g., R/T)
15aug98 -lwk- moved THERMAL_DETECTOR arrays out of BAND_BIN group because
	they don't have the Band dimension
19sep98 -lwk- fixed MAP_SCALE in Perspective case (take target radius into
	account)
15dec98 -lwk- changes for Unix:  made all functions that return values "int";  
	replaced variable "string" with "xstring" to avoid RTL collision
04feb99 -lwk- changed CODMAC part of DATA_SET_ID to 3 for Tube case
17feb99 -lwk- added GRATING_STEP_INFLATION to history, renamed 
	GRATING_CORRECTION to GRATING_POSITION_CORRECTION
07mar99 -lwk- allowed nbpln=5 when writing BAND_SUFFIX_NAME/UNIT for special
	(non-NIMS) processing support
21mar99 -lwk- added 2 more decimals places to precision of pshift/ainfl in label
04apr99 -lwk- fixed CORE_UNITS comment for RADIANCE_FACTOR case (PI was wrong)
09apr99 -lwk- removed MINIMUM/MAXIMUM_LONGITUDE items, revised EASTERNMOST/
	WESTERNMOST_LONGITUDE algorithm (yet again!) to ensure both items are
	always positive;  note that current NIMSCMM2 always makes min_lon
	< max_lon in Vicar label, but this will no longer be true when an ISIS
	cube has been converted back to VICAR by VISIS2 -- shouldn't matter!
11apr99 -lwk- fixed determination of mphase, which was incorrect when 
	TASK=VISIS2:  in forward mode need special keyword, in inverse must
	search cube History;  fixed OBSNAME/OBSEXT processing for phase-0
14apr99 -lwk- fixed a bug processing BAND_BIN_ORIGINAL_BAND introduced by
	previous change
26apr99 -lwk- added wobble parameters to History (only)
17may99 -lwk- added wobble cone estimate 
25may99 -lwk- removed WOBBLE_MODE item (per change in nimscmm2)
19jul99 -lwk- fixed mphase determination when task=NIMSCMM_SM
26jan00 -lwk- fixed Y2K bug in PRODUCT_CREATION_DATE
25jan00 -lwk- added SATURATION_THRESHOLD_WEIGHT label item and SCLK_GAPS
	history item
28may00 -lwk- check for SK_ID if SP_KERNEL is missing
20jun00 -lwk- fixed bug (found by BAM in VISISX on Linux, where it is fatal!)
	in get_real_value where float/double are confused:  added routine 
	get_double
03jul00 -lwk- changes for histogram-binning case
10jan01 -lwk- added label comment defining slant distance backplane
29oct01 -lwk- replaced get_string_value by get_qstring_value in keyword_value
	for string contstants
17aug02 -lwk- added code to support platform-dependent special-pixel values
20aug02 -lwk- also to support other platform-dependent label items
17sep02 -lwk- added U_NL,U_NS to zvopen calls with NOLABELS
19aug04 -lwk- fixed bug in get_recinfo() (inverse mode only)
09jun06 -lwk- disabled special code to read BEG/END_SCET (will give problems
	for certain formats!  Fix is TBD as it seems non-urgent ...)
14jun06 -lwk- added check following xlgets on RAD_CONV/BASE
07apr07 -lwk- disable NOTE processing as it crashes 14ensucomp mosaic cube;
	removed get_cat() and catalog include file; added quotes to
	START/STOP_TIMEs to satisfy ISIS on Shifty
17sep09 -lwk- disabled PRODID (not used under unix)
05feb10 -lwk- added code in write_object to update the host information in TOVIC
	mode
14mar11 -lwk- added EAST_LON keyword for C.Phillips ISIS3 work
13nov11 -lwk- changed format of START/STOP_TIME to conform to PDS stds
03feb12 -lwk- changed 'SINUSOIDAL_EQUAL-AREA' back to SINUSOIDAL (see change in
	1997 above) -- Unix ISIS seems to have changed its syntax
10feb12 -lwk- added PC for Linux to list of hosts

*******************************************************************************
TBD:

1.  VISIS2 should be more flexible in inverse mode, not requiring creation of 
 all the files present in ISIS cube.

2.  The text and keywords should be removed from the code and placed in
 separate data files for flexibility and ease of change.

3. Don't need all the strcat's, just insert \n in buffer?

******************************************************************************/

/*#include "gll_main.h"
 include gll_main.h here: */
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

#include <stdlib.h>

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
/* END GLL_MAIN.H */

#include "mp_routines.h"
#include "nims_label2.h"
/*#include "gll_nims_gen.h"  catalog stuff */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#include "vicmain_c"                                    /* Vicar */

					/*	ERROR HANDLING MACROS	     */
#define return_if_error(A)	zvsignal(A,vstat,0);	if(vstat<=0) return
#define continue_if_error(A)	zvsignal(A,vstat,0);	if(vstat<=0) continue
#define break_if_error(A)	zvsignal(A,vstat,0);	if(vstat<=0) break
#define fill_9s_error(A)	if(vstat<=0) strcpy(A,"999999999999")
#define zero_if_error(A)        if(vstat<=0) *(A)=0

	/*	DEFINE GLOBAL CONSTANTS	     */
/*#define FALSE 			0
#define TRUE 			1 (these are in mp_routines.h) */
#define FORWARD 		1
#define BACKWARD 		0
#define LABELMEMORY		84000
#define HISTMEMORY		24000
#define BUFFERSIZE		512
#define MAXBANDS		408
#define MAXPLOTS		6
#define MAXTEMPS		6
#define MAXNUMOFFILES 		10
#define MINNUMOFFILES 		3
#define LBL_BLANK_RECORDS 	10
#define HST_BLANK_RECORDS 	20
#define GEOBANDS		9
#define NUMOFPTRS		6
				/* 	DECLARATION OF GLOBAL VARIABLES	      */

struct band_bin {		/* Structure of Band Bin parameters */
  float waves[MAXBANDS];	/* Wavelength center in microns	*/
  int  	org_band[MAXBANDS];	/* List of original band numbers */
  int 	grating[MAXBANDS];	/* Logical grating positions */
  int 	detector[MAXBANDS];	/* (ng 1's, ng 2's, . . . ng 17's) */
} b;

int nbm;			/* max. # of bands for the ins.mode */
int nbb;			/* actual number of bands in the cube */
int ngp;			/* #grating positions (for Tube suffix) */
int grates[26];

struct vitems {			/* VICAR label items or CATALOG fields   */
  double projitem1,		/* 1st std. prll. LAMB; Ref. lat OBLQ   */
	projitem2,		/* 2nd std. prll. LAMB; Ref. lon OBLQ   */
	radii[3],		/* A, B, and C axes lengths		*/
	rotation,		/* Map projection rotation in degrees   */
	scale,			/* Projection scale in KM/PIXEL		*/
	cscale,			/* camera scale (pixels/mm)		*/
	focal,			/* focal length (mm)			*/
	oaline,			/* line of optical axis intercept	*/
	oasamp,			/* sample of optical axis intercept	*/
	center_line,		/* line of MM projection tiepoint	*/
	center_samp,		/* sample of MM projection tiepoint	*/
	center_lat,		/* latitude of MM projection tiepoint	*/
	center_lon,		/* longitude of MM projection tiepoint	*/
	ssc_line,		/* line of subspacecraft point		*/
	ssc_samp,		/* sample of subspacecraft point	*/
	ssc_lat,		/* latitude of ssc point		*/
	ssc_lon,		/* longitude of ssc point		*/
	range;			/* spacecraft-to-planet range		*/
  float	astretch[2],		/* Adaptive stretch parms of summary im */
	rstretch[2],		/* Red plane stret. parms of summary im */
	gstretch[2],		/* Grn plane stret. parms of summary im */
	bstretch[2],		/* Blu plane stret. parms of summary im */
	lat[MAXPLOTS],		/* Starting latitudes of spectra areas	*/
	lon[MAXPLOTS],		/* Starting longitudes of spectra areas	*/
	max_range,		/* S/C - Planet maximum distance	*/
	min_range,		/* S/C - Planet minimum distance	*/
	min_sun_d,		/* S/C - Sun minimum distance		*/
	max_sun_d,		/* S/C - Sun maximum distance		*/
	min_cb_d,		/* S/C - Central body minimum distance	*/
	max_cb_d,		/* S/C - Central body maximum distance	*/
	b_ssc_lat,		/* latitude of starting sub-s/c point	*/
	b_ssc_lon,		/* longitude of starting sub-s/c  point	*/
	e_ssc_lat,		/* latitude of ending sub-s/c point	*/
	e_ssc_lon,		/* longitude of ending sub-s/c point	*/
	b_ssl_lat,		/* latitude of starting sub-sol point	*/
	b_ssl_lon,		/* longitude of starting sub-sol point	*/
	e_ssl_lat,		/* latitude of ending sub-sol point	*/
	e_ssl_lon,		/* longitude of ending sub-sol point	*/
	min_lat,		/* Minimum latitude of MM cube		*/
	min_lon,		/* Minimum longitude of MM cube		*/
	max_lat,		/* Maximum latitude of MM cube		*/
	max_lon,		/* Maximum longitude of MM cube		*/
	inci_angle,		/* Photometric angles			*/
	emis_angle,		
	phas_angle,		
	phot_cut,		/* phot.func. cutoff wavelength		*/
	minn_exp,		/* exponent for Minnaert phot.func. 	*/
	thresh,			/* threshold for FOOTPRINT		*/
	satthrsh,		/* saturated-pixel threshold 		*/
	maxdistor,		/* max.pixel distortion, for FOOTPRINT	*/
	dpoint[2],		/* pointing offset 			*/
	slewtol,		/* slew rate tolerance			*/
	slew_rate,		/* mean slew rate relative to surface	*/
	eradf,			/* radius expansion factor		*/
	azimuth_sun,		/* Azimuth of sun in planet's coordina. */
	azimuth_sc,		/* Azimuth of spacecraft in planet's co */
	radiance[MAXBANDS],	/* DN to radiance or DN to IOF convers. */
	rad_base[MAXBANDS],	/* Radiance or IOF offset 		*/
	slant_dist_base,	/* Slant distance base for GEOCUBE	*/
	slant_dist_mult,	/* Slant distance multiplier		*/
	solarflux[MAXBANDS],	/* Solar flux vector 			*/
	radsens[MAXBANDS],	/* radiometric sensitivities		*/
	drkave[17],		/* average dark values			*/
	temp[MAXTEMPS],		/* Array of mean sensor temperatures	*/
	pshift,			/* Grating correction 			*/
	ainfl,			/* Grating step inflation		*/
	y_axis_max;		/* Highest value of HIST2D y-axis scale */
  int	gain_state[25],		/* Gain states (max. one per EDR)	*/
	fpgrid,			/* grid size for FOOTPRINT		*/
	chopper,		/* chopper mode				*/
	grating_offset,		/* Grating offset (0-31)		*/
	grating_delta,		/* Grating increment			*/
	grating_steps,		/* Grating step count			*/
	grating_start_pos,	/* Grating start position		*/
	bandedge_gp1,		/* Band edge grating position 1		*/
	bandedge_gp2,		/* Band edge grating position 2		*/
	fillnum,fillsize,	/* NIMSCMM fill parameters		*/
	sdband,sdgeo,		/* cube/cocube band #s for STD_DEV      */
	threshval[17],		/* instrument threshold values		*/
	modescale,		/* SPECPLOT mode: 1-DN, 2-IOF, 3-RAD,   */
				/* 	4-COMBINATION, 5-IOFonly 	*/
	native_times[2][3],	/* RIM.MF.RTI- start and stop	*/
	lines[MAXPLOTS],	/* Number of lines for spectra area	*/
	samples[MAXPLOTS],	/* Number of samples for spectra area 	*/
	dpstat,			/* flag for presence of DPOINT label item */
	sens_flg,		/* flag presence of sensitivities in label */
	drk_flg;		/* flag presence of dark aves in label */
  short bands[10][5],		/* Bands used in generation of SII cube */
	red_siid,		/* Band number of summary image red pln */
	grn_siid,		/* Band number of summary image grn pln */
	blu_siid,		/* Band number of summary image blu pln */
	sii_imgs,		/* Number of spectral index images	*/
	breakdet;		/* break detector if CALTYP=BOTH */
  char 	formulae[10][100],	/* Formulae of SII Cocube		*/
	binning[15],		/* NIMSCMM binning type			*/
	calfile[25][100],	/* Calibration file names		*/
	caltype[10],		/* Calibration type			*/
	darkfile[25][100],	/* Dark value file name			*/
	darktype[10],		/* Dark update type			*/
	sol_file[100],		/* Solar flux file name			*/
	deboomfile[100],	/* Deboom file name			*/
	despikefile[100],	/* Despike file name			*/
	dataformat[35],		/* Calibration type or data scale	*/
	dataunits[35],		/* Data units of calibration		*/
	edrfiles[25][101],	/* EDR file names			*/
	event_start_time[25],	/* Event start time in PDS format	*/
	event_stop_time[25],	/* Event stop time in PDS format	*/
	spkernel[100],		/* SP-kernel file name			*/
	ikernel[100],		/* I-kernel file name			*/
	mask[10],		/* Keyword for MASK or NOMASK specified */
	nimsmode[25],		/* NIMS instrument mode name		*/
	prod_id[50],		/* Product ID = Cube name		*/
	obsname[13],		/* Observation name 			*/
	obsext[2],		/* Observation extension		*/
	mosnum[3],		/* Mosaic number			*/
	prodnote[80],		/* Product note				*/
	obsnote[338],		/* Observation note			*/
	suppnote[159],		/* supplementary Obsnote		*/
	phase_name[50],		/* Mission phase name			*/
	picnos[2][30],		/* start/end SSI ridealong picture numbers*/
	photofunc[25],		/* Photometric correction type		*/
	aacs_file[100],		/* AACS file (pointing)			*/
	pfm_ck[100],		/* Platform C-kernel			*/
	rot_ck[100],		/* Rotor C-kernel			*/
	projection[41],		/* Map projection name			*/
	requestor[25],		/* Observation requestor name		*/
	stop_slide[10],		/* Stop or slide mode 			*/
	target[20],		/* Target body name			*/
	targetcode[4];		/* Target body code 			*/
}	v;			/* Pointer to VITEMS structure		*/

int	cubefiles,		/* Number of cube files: MM, GEO, SII   */
	numofbytes,		/* Number of bytes in PDS label		*/
	numoffiles,		/* Number of input files		*/
	nbpln,			/* Number of backplanes			*/
	objptr[NUMOFPTRS],	/* Pointers to objects		      	*/
	lsef,			/* Label search END flag		*/
	specplots,		/* Number of spectra files as input     */
	size,			/* Number of bytes in buffer	      	*/
	inunit[MAXNUMOFFILES],	/* Input files' unit numbers   		*/
	outunit[MAXNUMOFFILES],	/* Output files' unit numbers   	*/
	bytes[MAXNUMOFFILES],	/* Number of bytes/pixel 	     	*/
	nb[MAXNUMOFFILES],	/* Number of bands of files     	*/
	inl[MAXNUMOFFILES],	/* Number of lines of files     	*/
	ns[MAXNUMOFFILES],	/* Number of samples of files   	*/
	bmask[MAXBANDS],	/* band mask */
	vstat,			/* status variable for VICAR   	 	*/
	nedrs,			/* # of edrfiles		 	*/
	ncals,			/* # of cal files		 	*/
	ndrks,			/* # of dark files		 	*/
	tube,			/* flag that cube object is tube 	*/
	ptub,			/* flag that tube is P-tube 	*/
        cubsiz[2];		/* projected cube size (if tube) 	*/
long	inptr,			/* Input file pointer 	     		*/
        outptr;			/* Output file pointer          	*/
int	labend;			/* Pointer to end of label */
char 	*bufpoint,		/* Pointer for PDS item parsing 	*/
	*histpoint,		/* Pointer for History object   	*/
	value[8][7],		/* Object pointers	             	*/
	xstring[80],		/* character string buffer		*/
	line[120],lineb[80],	/* character string buffers	      	*/
	tasknam[9];		/* task name for labels */

int mphase;			/* 0=Phase-0 (NIMSCMM), 1=Phase-2 (NIMSCMM2) */
int ithreshval;			/* flag for instrument threshold array	*/
int pgraphic;			/* planetographic latitudes? */

int histfile;			/* History file stuff */
char hfilnam[100];
FILE *hfile,*fopen();

#define TASK_LIST_LEN 30
#define TASK_LEN 33
char cubtasks[TASK_LIST_LEN * TASK_LEN]; /* Task names in cube label */
int ncubtasks;

int specialp=0;			/* flag for SPECIAL_PROCESSING labels */
int calib;			/* flag for CAL/SKY "target" */
int iofcub;			/* flag for IOF cube */

int sclk_gaps[200], nsc_gaps;

/* wobble parameters: */
float wamp, wfreq, wphase, wcone;

int conv_lon;			/* flag to convert East long's to West */

/* definitions for platform-dependent label items -- these are tailored
 * to the ISIS system, because its program 'cv' is currently the best
 * for viewing cubes (aug02 -- lwk) */
char host[6];		/* currently SUN / VAX / ALPHA / LINUX */
char bp_type[11];	/* backplane data type */

/* definitions for platform-dependent special values:
 * (from $ISISINC/special_pixel.h) */
#if (VAX_ARCH == 1)
#define VALID_MIN_4                 "16#FFEFFFFF#"
#define NULL_4                      "16#FFFFFFFF#"
#define LOW_REPR_SAT_4              "16#FFFEFFFF#"
#define LOW_INSTR_SAT_4             "16#FFFDFFFF#"
#define HIGH_INSTR_SAT_4            "16#FFFCFFFF#"
#define HIGH_REPR_SAT_4             "16#FFFBFFFF#"
#else		/* IEEE */
#define VALID_MIN_4                "16#FF7FFFFA#"
#define NULL_4                     "16#FF7FFFFB#"
#define LOW_REPR_SAT_4             "16#FF7FFFFC#"
#define LOW_INSTR_SAT_4            "16#FF7FFFFD#"
#define HIGH_INSTR_SAT_4           "16#FF7FFFFE#"
#define HIGH_REPR_SAT_4            "16#FF7FFFFF#"
#endif

char verdat[11];		/* VERSION_DATE */
double atof();

#define FUNCTION		/* delimit modules */

/***************************************************************************/
void main44()
{
  char	inputfiles[MAXNUMOFFILES][100];	/* Input files for CUBE	     	     */
  int 	direction;			/* Direction of transformation 	     */

  /* inform user of Program version & update VERSION_DATE for label */
  zvmessage("*** VISIS2 Version 2016-02-24 ***","");
  strcpy( verdat, "2016-02-24");

  free(malloc(4096*4096));		/* 	Guarantee sufficient memory  */

  process_parms(&direction,inputfiles);	/* 	Get all user parameters      */

  if(direction==FORWARD)
    VICARtoISIS(inputfiles);
  else
    ISIStoVICAR();
}


/****************************************************************************/
FUNCTION byte_nibble(bbuf,nbuf,length)
/*
 * Create an array of nibbles from an array of bytes 
 * using only the most significant four bits of a byte.
 */
unsigned char bbuf[],nbuf[];
int *length;
{
  int samp;

  for( samp=0; samp<(*length)/2; samp++ )
	nbuf[samp] = bbuf[samp]&&240 + (bbuf[samp+1]&&240)>>4;
}


/****************************************************************************/
FUNCTION create_PDSlabel(nrecs,nlabrecs,nhistrecs,inputfiles) 
/*
 * Create PDS label for NIMS cube file.
 */
int	*nhistrecs,		/* 	Number of records in history object   */
	*nlabrecs,		/* 	Number of PDS label records	      */
	*nrecs;			/* 	Number of records for cube file	      */
char	inputfiles[][100];	/* 	Input files to generate cube          */
{
int additionalbytes, bufptr, count, index, object, labelbytes, nsi,
 i, ibk, j, len1, len2, len3, x, y, year, z;
float arval, pixelsperdegree, sav;
char cc, notes[7][80], tstr[50], geoplane[31], fobsname[20];
char obuf[160];		/* for notes processing */
char *hpoint;
time_t lt;
float sfact[3] = {47.56, 47.34, 48.29};		/* thermal sensitivity ratios */
float thoff[3] = {515.50,516.03,514.03};	/* upper range thermal offset */

struct tm *ptr;

bufpoint = (char *)calloc	/* Allocate memory for buffer */
	(LABELMEMORY,sizeof(char));	
if( bufpoint==NULL )
  zmabend( "Memory allocation error in create_PDSlabel routine");

strcpy(bufpoint,"CCSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL\r\n");
strcat(bufpoint,"\r\n/* File Structure */\r\n\r\n");
strcat(bufpoint,"RECORD_TYPE = FIXED_LENGTH\r\n");
strcat(bufpoint,"RECORD_BYTES = 512\r\n");
strcat(bufpoint,"FILE_RECORDS = XXXXXX\r\n");
strcat(bufpoint,"LABEL_RECORDS = XXXXXX\r\n");
strcat(bufpoint,"FILE_STATE = DIRTY\r\n\r\n");

strcat(bufpoint,"^HISTORY = XXXXXX\r\n");	
strcat(bufpoint,"OBJECT = HISTORY\r\n");
strcat(bufpoint,"END_OBJECT = HISTORY\r\n\r\n");

object = 0;

strcat(bufpoint,"^HISTOGRAM_IMAGE = XXXXXX\r\n");	/* 2D HISTOGRAM OBJ  */
strcat(bufpoint,"OBJECT = HISTOGRAM_IMAGE\r\n");
strcat(bufpoint,"/* Two dim histogram image structure */\r\n");
sprintf(line," LINES = %d\r\n",inl[object]);
strcat(bufpoint,line);
sprintf(line," LINE_SAMPLES = %d\r\n",ns[object]);
strcat(bufpoint,line);
strcat(bufpoint," SAMPLE_TYPE = UNSIGNED_INTEGER\r\n");
strcat(bufpoint," SAMPLE_BITS = 8\r\n");
strcat(bufpoint," SAMPLE_NAME = BAND\r\n");
strcat(bufpoint," LINE_NAME = INTENSITY\r\n");
strcat(bufpoint," NOTE = \"This is an unannotated two-dimensional histogram");
strcat(bufpoint," 'image' showing\r\n  frequency of measured 'Intensity'");
strcat(bufpoint," versus band number.  The 'Intensity'\r\n  may be DN, Radiance,");
strcat(bufpoint," or BDRF (Bi-Directional Reflectance), or a\r\n  combination");
strcat(bufpoint," of BDRF with Radiance, with BDRF below a cutoff band\r\n");
strcat(bufpoint,"  number and radiance above.  The cutoff is defined by:\r\n");
strcat(bufpoint,"  BDRF_RAD_TRANSITION_BAND_NUMBER.\r\n");
strcat(bufpoint,"  The 'Intensity' is DN only if CORE_NAME in the QUBE object");
strcat(bufpoint," is\r\n  RAW_DATA_NUMBER.\"\r\n");
sprintf(line," BDRF_RAD_TRANSITION_BAND_NUMBER = %d\r\n",v.breakdet);
strcat(bufpoint,line);
strcat(bufpoint,"END_OBJECT = HISTOGRAM_IMAGE\r\n\r\n");

if( specplots > 0 ) 	{

object+=specplots;

strcat(bufpoint,"^SAMPLE_SPECTRUM_QUBE = XXXXXX\r\n"); /* SPECTRA OBJECT  */
strcat(bufpoint,"OBJECT = SAMPLE_SPECTRUM_QUBE\r\n");
strcat(bufpoint,"/* Sample spectrum non-standard qube structure */\r\n");
strcat(bufpoint," AXES = 3\r\n");
strcat(bufpoint," AXIS_NAME = (SAMPLE,LINE,REGION)\r\n");
sprintf(line," ITEMS = (%d,%d,%d)\r\n",ns[object],inl[object],specplots);
strcat(bufpoint,line);
strcat(bufpoint," ITEM_BITS = 4\r\n");
strcat(bufpoint," ITEM_TYPE = UNSIGNED_INTEGER\r\n");

write_PDS_line(bufpoint,'F'," REGION_UPPER_LEFT_LATITUDE",v.lat,specplots,3);
write_PDS_line(bufpoint,'F'," REGION_UPPER_LEFT_LONGITUDE",v.lon,specplots,3);
write_PDS_line(bufpoint,'I'," REGION_SAMPLES",v.samples,specplots,0);
write_PDS_line(bufpoint,'I'," REGION_LINES",  v.lines,specplots,0);

strcat(bufpoint," NOTE	  = \"Each band is a partially annotated \'image\' of a spectral\r\n");
strcat(bufpoint,"  plot over a selected region in the NIMS data cube. The plot is of\r\n");
strcat(bufpoint,"  DN, radiance or BDRF (Bi-Directional Reflectance) versus NIMS_band\r\n");
strcat(bufpoint,"  or wavelength.  Nibble pixels may assume 3 values, representing\r\n");
strcat(bufpoint,"  background (usually 0), spectrum (usually 15), and an intermediate\r\n");
strcat(bufpoint,"  (gray) value used to display standard deviation over region. ");
strcat(bufpoint," BDRF\r\n  and radiance may coexist in each plot, with BDRF below a cutoff\r\n");
strcat(bufpoint,"  wavelength and radiance above.  The cutoff is defined by:\r\n");
strcat(bufpoint,"  BDRF_RAD_TRANSITION_WAVELENGTH.\r\n");
strcat(bufpoint,"  The plot is of DN only if CORE_NAME in the QUBE object");
strcat(bufpoint," is\r\n  RAW_DATA_NUMBER.\"\r\n");
sprintf(line," BDRF_RAD_TRANSITION_WAVELENGTH = %.5f\r\n",b.waves[v.breakdet-1]);
strcat(bufpoint,line);
strcat(bufpoint,"END_OBJECT = SAMPLE_SPECTRUM_QUBE\r\n\r\n");	}

object++;
strcat(bufpoint,"^QUBE = XXXXXX\r\n");	/* QUBE OBJECT		*/
strcat(bufpoint,"OBJECT = QUBE\r\n\r\n");
strcat(bufpoint,"/* Qube structure */");
strcat(bufpoint,"\r\n\r\n AXES = 3\r\n");
strcat(bufpoint," AXIS_NAME = (SAMPLE,LINE,BAND)\r\n\r\n");
strcat(bufpoint,"/*  Core description */\r\n\r\n");
sprintf(line," CORE_ITEMS = (%d,%d,%d)\r\n",ns[object],inl[object],nbb);
strcat(bufpoint,line);

sprintf(line," CORE_ITEM_BYTES = %d\r\n", bytes[object]);
strcat(bufpoint,line);
if (bytes[object]==2) {
  strcat(bufpoint," CORE_ITEM_TYPE = ");
  strcat(bufpoint,host);
  strcat(bufpoint,"_INTEGER\r\n");
}
else if (bytes[object]==4) {
  strcat(bufpoint," CORE_ITEM_TYPe = ");
  strcat(bufpoint,host);
  strcat(bufpoint,"_REAL\r\n");
}

strcat(bufpoint," CORE_BASE = 0.0\r\n CORE_MULTIPLIER = 1.0\r\n");
strcat(bufpoint,"/* Core scaling is:  True_value = base + (multiplier *");
strcat(bufpoint," stored_value)");
if (strcmp(v.dataformat,"RAW_DATA_NUMBER") && bytes[object]==2) {
  strcat(bufpoint,". */\r\n/* Scaling of radiance is band-dependent.");
  strcat(bufpoint,"  But values of CORE BASE and */\r\n");
  strcat(bufpoint,"/* CORE MULTIPLIER are set to 0.0 and");
  strcat(bufpoint," 1.0 for ISIS software compatibility. */\r\n");
  strcat(bufpoint,"/* See Band Bin scaling, below. */\r\n");
}
else
  strcat(bufpoint," */\r\n");

if (bytes[object]==2) {
  strcat(bufpoint," CORE_VALID_MINIMUM =         -32752\r\n");
/* (removed because of Unix ISIS incompatibility)
  strcat(bufpoint," CORE_MISSING_SENSITIVITY =   -32754\r\n");
  strcat(bufpoint," CORE_BELOW_THRESHOLD =       -32762\r\n");
 */
  strcat(bufpoint," CORE_HIGH_REPR_SATURATION =  -32764\r\n");
  strcat(bufpoint," CORE_HIGH_INSTR_SATURATION = -32765\r\n");
  strcat(bufpoint," CORE_LOW_INSTR_SATURATION =  -32766\r\n");
  strcat(bufpoint," CORE_LOW_REPR_SATURATION =   -32767\r\n");
  strcat(bufpoint," CORE_NULL =                  -32768\r\n");
}
else if (bytes[object]==4) {
  strcat(bufpoint," CORE_VALID_MINIMUM =         ");
  strcat( bufpoint, VALID_MIN_4);
  strcat( bufpoint, "\r\n");
  strcat(bufpoint," CORE_HIGH_REPR_SATURATION =  ");
  strcat( bufpoint, HIGH_REPR_SAT_4);
  strcat( bufpoint, "\r\n");
  strcat(bufpoint," CORE_HIGH_INSTR_SATURATION = ");
  strcat( bufpoint, HIGH_INSTR_SAT_4);
  strcat( bufpoint, "\r\n");
  strcat(bufpoint," CORE_LOW_INSTR_SATURATION =  ");
  strcat( bufpoint, LOW_INSTR_SAT_4);
  strcat( bufpoint, "\r\n");
  strcat(bufpoint," CORE_LOW_REPR_SATURATION =   ");
  strcat( bufpoint, LOW_REPR_SAT_4);
  strcat( bufpoint, "\r\n");
  strcat(bufpoint," CORE_NULL =                  ");
  strcat( bufpoint, NULL_4);
  strcat( bufpoint, "\r\n");
}
sprintf(line," CORE_NAME = %s\r\n",v.dataformat);
strcat(bufpoint,line);
sprintf(line," CORE_UNIT = %s\r\n",v.dataunits);
strcat(bufpoint,line);
if (!strcmp(v.dataformat,"SPECTRAL_RADIANCE")) {
  strcat(bufpoint,"/* Core units:  to convert these radiances to SI");
  strcat(bufpoint," units (W/m^2/sr/uM), */\r\n/* the data in the cube must");
  strcat(bufpoint," be divided by 100. */\r\n");
}
else if (!strcmp(v.dataformat,"RADIANCE_FACTOR")) 
  strcat(bufpoint,"/* 'Radiance factor' = ( PI * Radiance) / Solar_Flux */\r\n");
strcat(bufpoint,"\r\n SPATIAL_BINNING_TYPE = ");
if (!strcmp(v.binning,"FOOTPRNT")) {
  strcat(bufpoint,"FOOTPRINT_AVERAGE\r\n");
  sprintf(line," THRESHOLD_WEIGHT = %.5f\r\n",v.thresh);
  strcat(bufpoint,line);
}
else if (!strcmp(v.binning,"FTPTHSTM"))
  strcat(bufpoint,"FOOTPRINT_HISTOGRAM_MEDIAN\r\n");
else if (!strcmp(v.binning,"FTPTHSTP"))
  strcat(bufpoint,"FOOTPRINT_HISTOGRAM_PEAK\r\n");
if (v.binning[0]=='F') {
  sprintf(line," FOOTPRINT_GRID_SIZE = %d\r\n",v.fpgrid);
  strcat(bufpoint,line);
  if (v.satthrsh > 0.0) {
    sprintf(line," SATURATION_THRESHOLD_WEIGHT = %.5f\r\n",v.satthrsh);
    strcat(bufpoint,line);
  }
  if (v.maxdistor>0.0) {
    sprintf(line," MAXIMUM_PIXEL_DISTORTION = %.5f\r\n",v.maxdistor);
    strcat(bufpoint,line);
  }
  if (!tube) {
    strcat(bufpoint,"/* Each NIMS raw DN was averaged over its entire footprint");
    strcat(bufpoint,", which was */\r\n");
    strcat(bufpoint,"/* approximated by computing the location of its four");
    strcat(bufpoint," corner points and */\r\n");
    strcat(bufpoint,"/* covering the resulting quadrilateral with a grid;  the");
    strcat(bufpoint," weight of each DN */\r\n");
    strcat(bufpoint,"/* in a given output pixel is the number of grid points");
    strcat(bufpoint," falling in that */\r\n"); 
    strcat(bufpoint,"/* pixel, weighted by the instrument response function. */\r\n"); 
    strcat(bufpoint,"/* FOOTPRINT_GRID_SIZE is the number of points used in");
    strcat(bufpoint," each dimension for */\r\n");
    strcat(bufpoint,"/* the grid. */\r\n");

    if (!strcmp(v.binning,"FOOTPRNT")) {
      strcat(bufpoint,"/* THRESHOLD_WEIGHT is the lower limit for the average:");
      strcat(bufpoint," if the total weight */\r\n");
      strcat(bufpoint,"/* contributing to an output pixel is below this limit,");
      strcat(bufpoint," then the output DN */\r\n");
      /* replaced CORE_BELOW_THRESHOLD with NULL because of Unix ISIS
        incompatibility: */
      strcat(bufpoint,"/* is set to NULL. */\r\n");
    }
    if (v.maxdistor>0.0) {
      strcat(bufpoint,"/* MAXIMUM_PIXEL_DISTORTION is the upper limit to the");
      strcat(bufpoint," line and sample */\r\n");
      strcat(bufpoint,"/* extension of this quadrilateral:  if it is exceeded,");
      strcat(bufpoint," then the raw DN is */\r\n");
      strcat(bufpoint,"/* omitted. */\r\n");
    }
    if (!strncmp(v.binning,"FTPTHST",7)) {
      strcat(bufpoint,"/* The output pixel value was determined by accumulating");
      strcat(bufpoint," the histogram of */\r\n");
      strcat(bufpoint,"/* the input values */\r\n");
    }
  }
  else {		/* if Tube */
    strcat(bufpoint,
     "/* SPATIAL_BINNING_TYPE, FOOTPRINT_GRID_SIZE, THRESHOLD_WEIGHT, and */\r\n");
    strcat(bufpoint,
     "/* (for certain projections) MAXIMUM_PIXEL_DISTORTION, are parameters */\r\n");
    strcat(bufpoint,
     "/* that are not relevant to the Tube core data, but can be used in */\r\n");
    strcat(bufpoint,
     "/* subsequent ISIS processing to generate a projected cube from a */\r\n");
    strcat(bufpoint,
     "/* tube file with program NIMSGEOMF.  Note that the value: */\r\n");
    strcat(bufpoint,
     "/*     SPATIAL_BINNING_TYPE = FOOTPRINT */\r\n");
    strcat(bufpoint,
     "/* does trigger the addition of two extra backplanes per grating */\r\n");
    strcat(bufpoint,
     "/* position:  the RIGHT_EDGE_PROJ_LINE/SAMPLE backplanes. */\r\n");
  }
  strcat(bufpoint,"\r\n");
}
else if (v.binning[0]=='N') strcat(bufpoint,"NEAREST_AVERAGE\r\n");
else if (v.binning[0]=='M') strcat(bufpoint,"NEAREST_MAXIMUM\r\n");
else if (v.binning[0]=='R') strcat(bufpoint,"NEAREST_REPLACE\r\n");
else zmabend(" *** unknown binning type ***");
if (v.eradf > 0.0) {
  sprintf(line," EXPANDED_RADIUS = %.2f\r\n",v.eradf);
  strcat(bufpoint,line);
}
sprintf(line," DARK_UPDATE_TYPE = %s\r\n",v.darktype);
strcat(bufpoint,line);
sprintf(line," FILL_BOX_SIZE = %d\r\n",v.fillsize);
strcat(bufpoint,line);
sprintf(line," FILL_MIN_VALID_PIXELS = %d\r\n",v.fillnum);
strcat(bufpoint,line);
sprintf(line," PHOTOMETRIC_CORRECTION_TYPE = %s\r\n",v.photofunc);
strcat(bufpoint,line);
if( v.photofunc[0]=='M' || v.photofunc[0]=='L' || v.photofunc[1]=='L' ) {
  sprintf(line," PHOTO_CORR_CUTOFF_WAVELENGTH = %.2f\r\n\r\n",
   v.phot_cut);
  strcat(bufpoint,line);
}
if( v.photofunc[0] == 'M' ) {
  sprintf(line," MINNAERT_EXPONENT = %.4f\r\n",v.minn_exp);
  strcat(bufpoint,line);
}
if( v.photofunc[0]=='M' || v.photofunc[0]=='L' || v.photofunc[1]=='L' ) {
  strcat(bufpoint,"/* The \"photometric correction\" ");
  strcat(bufpoint,"specifies the photometric function */\r\n/* used to ");
  strcat(bufpoint,"correct for effects of viewing and illumination geometry. */\r\n/*");
  strcat(bufpoint," Currently supported functions include: */\r\n");
  strcat(bufpoint,"/*    Lambert = cos(I) */\r\n");
  strcat(bufpoint,"/*    Lommel-Seeliger = cos(I)/(cos(I)+cos(E)) */\r\n");
  strcat(bufpoint,"/*    Minnaert = cos(I)^K *");
  strcat(bufpoint," cos(E)^(K-1) */\r\n/* where I = incidence angle, and E =");
  strcat(bufpoint," emission angle, which are both found */\r\n/* in the backplanes.");
  strcat(bufpoint,"  Each radiance in the cube has been divided by this */\r\n/*");
  strcat(bufpoint," function, if its wavelength is less than the cutoff");
  strcat(bufpoint," wavelength specified */\r\n/* by the parameter");
  strcat(bufpoint," PHOTO_CORR_CUTOFF_WAVELENGTH. */\r\n");
  if( v.photofunc[0] == 'M') {
    strcat(bufpoint,"/* The value of the constant K for the Minnaert function");
    strcat(bufpoint," is stored in the parameter */\r\n/* MINNAERT_EXPONENT. */\r\n");
  }
  strcat(bufpoint,"\r\n");
}

if( cubefiles == 3 ) {
  nsi = nb[numoffiles-1];
}
else {
  nsi = 0;
}

strcat(bufpoint,"\r\n");
strcat(bufpoint,"/*  Suffix description  */\r\n\r\n");
strcat(bufpoint," SUFFIX_BYTES = 4\r\n");
sprintf(line," SUFFIX_ITEMS = (0,0,%d)\r\n",nbpln);
strcat(bufpoint,line);

if (calib) {
  strcat(bufpoint," BAND_SUFFIX_NAME = NATIVE_TIME\r\n");
  strcat(bufpoint," BAND_SUFFIX_UNIT = SECOND\r\n");
  strcat(bufpoint," BAND_SUFFIX_ITEM_BYTES = 4\r\n");
  strcat(bufpoint," BAND_SUFFIX_ITEM_TYPE = ");
  strcat(bufpoint,host);
  strcat(bufpoint,"_REAL\r\n");
  strcat(bufpoint," BAND_SUFFIX_BASE = 0.000000\r\n");
  strcat(bufpoint," BAND_SUFFIX_MULTIPLIER = 63.000000\r\n");
  goto endsuff;
}

strcat(bufpoint," BAND_SUFFIX_NAME = (");
if (!tube) {
  strcat( bufpoint, "LATITUDE,LONGITUDE,INCIDENCE_ANGLE,\r\n  ");
  if (nbpln==5)
    strcat( bufpoint, "EMISSION_ANGLE,PHASE_ANGLE");
  else {
    strcat( bufpoint, "EMISSION_ANGLE,PHASE_ANGLE,SLANT_DISTANCE,");
    strcat( bufpoint, "INTERCEPT_ALTITUDE,\r\n  ");
    if (v.sdgeo==3) strcpy(geoplane,"INCIDENCE_ANGLE");
    else if (v.sdgeo==4) strcpy(geoplane,"EMISSION_ANGLE");
    else if (v.sdgeo==5) strcpy(geoplane,"PHASE_ANGLE");
    else if (v.sdgeo==6) strcpy(geoplane,"SLANT_DISTANCE");
    else if (v.sdgeo==7) strcpy(geoplane,"INTERCEPT_ALTITUDE");
    /* the Std.Dev. backplane is never IOF, even if the core is ... */
    strcpy( obuf, v.dataformat);
    if (!strcmp(v.dataformat,"RADIANCE_FACTOR"))
     strcpy( obuf, "SPECTRAL_RADIANCE");
    sprintf(line,"%s_STD_DEV,%s_STD_DEV",geoplane,obuf);
  }
}
else {				/* if tube input */
  for (i=0; i<ngp; i++) {
    j = grates[i];
    sprintf( line, "GR_POS_%02d_LATITUDE,GR_POS_%02d_LONGITUDE,\r\n  ", j, j);
    strcat(bufpoint,line);
    sprintf( line,
     "GR_POS_%02d_PROJECTED_LINE,GR_POS_%02d_PROJECTED_SAMPLE,\r\n  ", j, j);
    strcat(bufpoint,line);
    if (v.binning[0]=='F') {
      sprintf( line,
     "GR_POS_%02d_RIGHT_EDGE_PROJ_LINE,GR_POS_%02d_RIGHT_EDGE_PROJ_SAMPLE,\r\n  ", j, j);
      strcat(bufpoint,line);
    }
    if (ptub) {
      sprintf( line,
     "GR_POS_%02d_RIGHT_ASCENSION,GR_POS_%02d_DECLINATION,\r\n  ", j, j);
      strcat(bufpoint,line);
      sprintf( line,
     "GR_POS_%02d_TWIST_ANGLE,GR_POS_%02d_SC_TG_VECT_COMP_1,\r\n  ", j, j);
      strcat(bufpoint,line);
      sprintf( line,
     "GR_POS_%02d_SC_TG_VECT_COMP_2,GR_POS_%02d_SC_TG_VECT_COMP_3,\r\n  ", j, j);
      strcat(bufpoint,line);
    }
  }
  strcat(bufpoint,"INCIDENCE_ANGLE,EMISSION_ANGLE,");
  strcat(bufpoint,"PHASE_ANGLE,SLANT_DISTANCE,\r\n");
  strcpy(line,"  INTERCEPT_ALTITUDE,NATIVE_TIME");
}
	/* SII formulae, if present: */
if( cubefiles == 3 ) {
  strcat(line,",");
  for( x = 0; x < (nsi-1); x++ ) {
    sprintf(xstring,"\'%s\',",v.formulae[x]);
    strcat(line,xstring);
    if( (strlen(line)+strlen(v.formulae[x+1])) > 70 ) {
      strcat(line,"\r\n");
      strcat(bufpoint,line);
      strcpy(line,"  ");
    }
  }
  sprintf(xstring,"\'%s\')\r\n",v.formulae[x]);
  strcat(line,xstring);
  strcat(bufpoint,line);
}
else {
  if (nbpln==5)
    strcat(bufpoint,")\r\n");
  else {
    strcat(line,")\r\n");
    strcat(bufpoint,line);
  }
}
if (!tube) {
  strcat(bufpoint," BAND_SUFFIX_UNIT = (DEGREE,DEGREE,DEGREE,DEGREE,DEGREE");
  if (nbpln>5) {
    strcat(bufpoint,",KILOMETER,\r\n");
    sprintf(line,"  KILOMETER,");
    if (v.sdgeo<6) strcat(line,"DEGREE,");
    else strcat(line,"KILOMETER,");
    sprintf(lineb,"%s",v.dataunits);
    strcat(line,lineb);
  }
}
else {				/* if tube input */
  strcat(bufpoint," BAND_SUFFIX_UNIT = (");
  for (i=0; i<ngp; i++) {
    strcat( bufpoint, "DEGREE,DEGREE,DIMENSIONLESS,DIMENSIONLESS,");
    if (v.binning[0]=='F')	/* Footprint algorithm */
      strcat( bufpoint, "DIMENSIONLESS,DIMENSIONLESS,");
    strcat( bufpoint, "\r\n  ");
    if (ptub)
      strcat( bufpoint, "RADIAN,RADIAN,RADIAN,KILOMETER,KILOMETER,KILOMETER, \r\n  ");
  }
  strcpy( line,"DEGREE,DEGREE,DEGREE,KILOMETER,KILOMETER,SECOND");
}
if( cubefiles == 3 ) {
  for( x = 0; x < (nsi-1); x++ ) {
    if( strlen(line) > 60 ) {
      strcat(line,",\r\n");
      strcat(bufpoint,line);
      sprintf(line,"  UNKNOWN");
    }
    else strcat(line,",UNKNOWN");
  }
  strcat(line,",UNKNOWN)\r\n");
  strcat(bufpoint,line);
}
else {
  if (nbpln==5)
    strcat(bufpoint,")\r\n");
  else {
    strcat(line,")\r\n");
    strcat(bufpoint,line);
  }
}
write_PDS_aline(bufpoint,'I'," BAND_SUFFIX_ITEM_BYTES",4,nbpln);
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_ITEM_TYPE",bp_type,nbpln);
arval = 0.0;
write_PDS_aline(bufpoint,'F'," BAND_SUFFIX_BASE",&arval,nbpln);
if (!tube) {
  arval = 1.0;
  write_PDS_aline(bufpoint,'F'," BAND_SUFFIX_MULTIPLIER",&arval,nbpln);
}
else {		/* Tube needs final '63' for chops */
  sprintf(line,"%s = ("," BAND_SUFFIX_MULTIPLIER");
  for( x = 0; x < nbpln-1 ; x++ ) {
    sprintf(xstring,"%f,",1.0);
    strcat(line,xstring);
    if( strlen(line) > 60 ) {
      strcat(line,"\r\n");
      strcat(bufpoint,line);
      strcpy(line,"     ");
    }
  }
  sprintf(xstring,"%f)\r\n",63.);
  strcat(line,xstring);
  strcat(bufpoint,line);
}
endsuff:
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_VALID_MINIMUM ",VALID_MIN_4,nbpln);
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_NULL          ",NULL_4,nbpln);
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_LOW_REPR_SAT  ",LOW_REPR_SAT_4,nbpln);
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_LOW_INSTR_SAT ",LOW_INSTR_SAT_4,nbpln);
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_HIGH_INSTR_SAT",HIGH_INSTR_SAT_4,nbpln);
write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_HIGH_REPR_SAT ",HIGH_REPR_SAT_4,nbpln);

strcat(bufpoint,"\r\n");

/* this is the erstwhile BAND_SUFFIX_NOTE: */
if (!tube) {
  strcat(bufpoint,"/* The backplanes contain 7 geometric parameters, the ");
  strcat(bufpoint,"standard deviation */\r\n/* of one of them, the standard ");
  strcat(bufpoint,"deviation of a selected data band, ");
}
else if (ptub) {
  if (v.binning[0]=='F') {
    strcat(bufpoint,"/* The backplanes contain 12 geometric parameters for ");
    strcat(bufpoint,"each grating position */\r\n/* (latitude, longitude, line, ");
    strcat(bufpoint,"sample, right-edge-of-NIMS-FOV line, right-edge */\r\n/* ");
    strcat(bufpoint,"sample), 3 Euler angles [RA,Dec,Twist], 3 components ");
    strcat(bufpoint,"of the \'RS-vector\' */\r\n/* from Target-body center ");
    strcat(bufpoint,"to Spacecraft), 5 \'global\' geometric parameters which ");
    strcat(bufpoint,"apply to all grating */\r\n/* positions, the time ");
    strcat(bufpoint,"(in \'chops\', see below) of the first grating ");
    strcat(bufpoint,"position, ");
  }
  else {
    strcat(bufpoint,"/* The backplanes contain 10 geometric parameters for ");
    strcat(bufpoint,"each grating position */\r\n/* (latitude, longitude, line, ");
    strcat(bufpoint,"sample, 3 Euler angles [RA,Dec,Twist], */\r\n/* 3 ");
    strcat(bufpoint,"components of the \'RS-vector\' from Target-body center ");
    strcat(bufpoint,"*/\r\n/* to Spacecraft), 5 \'global\' geometric parameters ");
    strcat(bufpoint,"which */\r\n/* apply to all grating positions, the time ");
    strcat(bufpoint,"(in \'chops\', see below) */\r\n/* of the first grating ");
    strcat(bufpoint,"position, ");
  }
}
else if (!calib) {
  if (v.binning[0]=='F') {
    strcat(bufpoint,"/* The backplanes contain 6 geometric parameters for ");
    strcat(bufpoint,"each grating position */\r\n/* (latitude, longitude, line, ");
    strcat(bufpoint,"sample, right-edge-of-NIMS-FOV line, right-edge */\r\n/* ");
    strcat(bufpoint,"sample), 5 \'global\' geometric parameters which ");
    strcat(bufpoint,"apply to all grating */\r\n/* positions, the time ");
    strcat(bufpoint,"(in \'chops\', see below) of the first grating ");
    strcat(bufpoint,"position, ");
  }
  else {
    strcat(bufpoint,"/* The backplanes contain 4 geometric parameters for ");
    strcat(bufpoint,"each grating */\r\n/* position (latitude, longitude, line, ");
    strcat(bufpoint,"sample), 5 \'global\' geometric */\r\n/* parameters ");
    strcat(bufpoint,"which apply to all grating positions, the time */\r\n/* ");
    strcat(bufpoint,"(in \'chops\', see below) of the first grating position, ");
  }
}
if (!calib) {
  strcat(bufpoint,"*/\r\n/* and 0 to 10 ");
  strcat(bufpoint,"\'spectral index\' bands, each a user-specified function");
  strcat(bufpoint," of the */\r\n/* data bands.  (See the BAND SUFFIX NAME ");
  strcat(bufpoint,"values.) */\r\n\r\n");
  strcat(bufpoint,"/* Longitude ranges from 0 to 360 degrees, with positive ");
  strcat(bufpoint,"direction */\r\n/* specified by POSITIVE LONGITUDE DIRECTION ");
  strcat(bufpoint,"in the IMAGE MAP PROJECTION */\r\n/*");
  if (pgraphic==1) strcat(bufpoint," group.  Latitudes are planetographic. */\r\n\r\n");
  else if (pgraphic==0) strcat(bufpoint,"  group.  Latitudes are planetocentric. */\r\n\r\n");
  else strcat(bufpoint,"  group. */\r\n\r\n");

  strcat(bufpoint,"/* SLANT DISTANCE contains the distance from the observer");
  strcat(bufpoint," to the */\r\n/* intercept point of the line of sight with");
  strcat(bufpoint," the target body surface. */\r\n/* Normally, this is the"); 
  strcat(bufpoint," distance at the time of observation (or */\r\n/* the mean");
  strcat(bufpoint," time, when projected pixels are averaged).  However, */\r\n");
  strcat(bufpoint,"/* in the case of a Perspective projection, the distance is");
  strcat(bufpoint," measured */\r\n/* from the perspective point of the");
  strcat(bufpoint," projection. */\r\n\r\n");

  strcat(bufpoint,"/* INTERCEPT ALTITUDE contains values for the DIFFERENCE ");
  strcat(bufpoint,"between */\r\n/* the length of the normal from the center of ");
  strcat(bufpoint,"the target body to the */\r\n/* line of sight AND the radius of");
  strcat(bufpoint," the target body.  On-target points */\r\n/* have zero values. ");
  strcat(bufpoint," Points beyond the maximum expanded radius have */\r\n");
  strcat(bufpoint,"/* null values.  This plane thus also serves as a set of ");
  strcat(bufpoint,"\"off-limb\" */\r\n/* flags.  It is meaningful only for the ");
  strcat(bufpoint,"ORTHOGRAPHIC and */\r\n/* POINT PERSPECTIVE projections; other");
  strcat(bufpoint,"wise all values are zero. */\r\n\r\n");
}
if (!tube) {
  strcat(bufpoint,"/* The geometric standard deviation backplane contains ");
  strcat(bufpoint,"the standard */\r\n/* deviation of the geometry backplane indi");
  strcat(bufpoint,"cated in its NAME. */\r\n\r\n");
  strcat(bufpoint,"/* The data band standard deviation plane is computed for ");
  strcat(bufpoint,"the NIMS data */\r\n/* band specified by STD DEV SELECTED BAND");
  strcat(bufpoint," NUMBER.  This may be either */\r\n/* a raw data number, or ");
  strcat(bufpoint,"spectral radiance, whichever is indicated by */\r\n");
  strcat(bufpoint,"/* CORE NAME. */");
}
else {
  strcat(bufpoint,"/* The NATIVE TIME band of the suffix");
  strcat(bufpoint," is the time in \"chops\" of */\r\n/* the first grating");
  strcat(bufpoint," position at the corresponding line and sample */\r\n/* after");
  strcat(bufpoint," the NATIVE START TIME, where 63 chops = 1 second. */");
}
if( cubefiles == 3 ) {		/* if SII bands present */
  strcat(bufpoint,"\r\n\r\n");
  strcat(bufpoint,"/* The spectral index bands were generated by the ");
  strcat(bufpoint,"Vicar F2 program. */\r\n/* The corresponding BAND SUFFIX");
  strcat(bufpoint," NAME is an abbreviated formula */\r\n/* for the function used");
  strcat(bufpoint,", where Bn should be read 'NIMS data band n'. */\r\n/* For ");
  strcat(bufpoint,"example: B4/B8 represents the ratio of bands 4 and 8. */");
}
strcat(bufpoint,"\"\r\n\r\n");

if (!calib) {
  sprintf(line," STD_DEV_SELECTED_BAND_NUMBER = %d\r\n", v.sdband);
  strcat(bufpoint,line);
  sprintf(line," STD_DEV_SELECTED_BACKPLANE = %d\r\n\r\n", v.sdgeo);
  strcat(bufpoint,line);
}

strcat(bufpoint,"/*  Data description: general */\r\n\r\n");
if (tube) sprintf(line," DATA_SET_ID = 'GO-%s-NIMS-3-TUBE-V1.0'\r\n",
 v.targetcode);
else sprintf(line," DATA_SET_ID = 'GO-%s-NIMS-4-MOSAIC-V1.0'\r\n",v.targetcode);
strcat(bufpoint,line);
strcat(bufpoint," SPACECRAFT_NAME = GALILEO_ORBITER\r\n");
sprintf(line," MISSION_PHASE_NAME = %s\r\n",v.phase_name);
strcat(bufpoint,line);
strcat(bufpoint," INSTRUMENT_NAME = 'NEAR INFRARED MAPPING SPECTROMETER'\r\n");
strcat(bufpoint," INSTRUMENT_ID = NIMS\r\n");
strcat(bufpoint," ^INSTRUMENT_DESCRIPTION = \"NIMSINST.TXT\"\r\n\r\n");
sprintf(line," TARGET_NAME = %s\r\n",v.target);		/* target name */
strcat(bufpoint,line);

	/* write START_TIME before NATIVE_START_TIME, so that VISIS2 
	 * does not find the latter when looking for the former in 
	 * inverse mode: */
sprintf(line," START_TIME = \"%s\"\r\n",		/* starting SCET     */
	v.event_start_time);	
strcat(bufpoint,line);
sprintf(line," STOP_TIME = \"%s\"\r\n",		/* ending SCET       */
	v.event_stop_time);
strcat(bufpoint,line); 

sprintf(line," NATIVE_START_TIME = \"%d.%02d.%d\"\r\n",	/* starting SCLK */
	v.native_times[0][0],v.native_times[0][1],v.native_times[0][2]);
strcat(bufpoint,line);
sprintf(line," NATIVE_STOP_TIME = \"%d.%02d\"\r\n\r\n", /* ending SCLK */
	v.native_times[1][0],v.native_times[1][1]);
strcat(bufpoint,line);

strcpy( fobsname, v.obsname);
if (mphase && strcmp( v.obsext, " ")) strcat( fobsname, v.obsext);
sprintf(line," OBSERVATION_NAME = \'%s\'\r\n", fobsname);
strcat(bufpoint,line);

/* OBSNOTE, SUPPNOTE, & PRODNOTE get concatenated into a single NOTE, 
 * with " // " as delimiter (for inverse mode) */

/* 07apr07:  this code is misbehaving;  since it's not essential,
 * disable it for now */
goto skipnote;

len1 = len2 = len3 = 0;		/* lengths without trailing blanks */
/* first, replace any instances of "//" (or more /'s) with a single "/",
 * to avoid confusing parser in inverse mode;  also, VICAR replaces single
 * quotes (') with pairs of same, but we don't need this, so undo it ... */
x = strlen(v.obsnote);
y = 0;
for (i=0; i<x; i++) {
  while (!strncmp( &v.obsnote[i], "//", 2)) {
    strcpy( obuf, &v.obsnote[i+1]);
    strcpy( &v.obsnote[i], obuf);
    x--;  
  }
  if (!strncmp( &v.obsnote[i], "''", 2)) {
    strcpy( obuf, &v.obsnote[i+1]);
    strcpy( &v.obsnote[i], obuf);
    x--;  
  }
  if (v.obsnote[y] != ' ') len1 = y+1;
  y++;
}
x = strlen(v.suppnote);
y = 0;
for (i=0; i<x; i++) {
  while (!strncmp( &v.suppnote[i], "//", 2)) {
    strcpy( obuf, &v.suppnote[i+1]);
    strcpy( &v.suppnote[i], obuf);
    x--;
  }
  if (!strncmp( &v.suppnote[i], "''", 2)) {
    strcpy( obuf, &v.suppnote[i+1]);
    strcpy( &v.suppnote[i], obuf);
    x--;  
  }
  if (v.suppnote[y] != ' ') len2 = y+1;
  y++;
}
x = strlen(v.prodnote);
y = 0;
for (i=0; i<x; i++) {
  while (!strncmp( &v.prodnote[i], "//", 2)) {
    strcpy( obuf, &v.prodnote[i+1]);
    strcpy( &v.prodnote[i], obuf);
    x--;  
  }
  if (!strncmp( &v.prodnote[i], "''", 2)) {
    strcpy( obuf, &v.prodnote[i+1]);
    strcpy( &v.prodnote[i], obuf);
    x--;  
  }
  if (v.prodnote[y] != ' ') len3 = y+1;
  y++;
}

	/* concatenate them */
strncpy( &v.obsnote[len1], " // \0", 5);
strcat( v.obsnote, v.suppnote);
if (len2<2) len2 = 0;
strncpy( &v.obsnote[len1+4+len2], " // \0", 5);
strcat( v.obsnote, v.prodnote);
x = len1 + len2 + 8 + len3;
if (x<2 || x>337) zmabend(" error processing NOTEs");

	/* write out entire NOTE: */
y = 0;			/* current position in obsnote */
if (x < 68) {
  sprintf(line," NOTE = \"%s\"\r\n", v.obsnote);
  strcat(bufpoint,line);
}
else {
  strncpy( line," NOTE = \"", 9);
  /* back up if a word is split at EOL: */
  for (ibk=0; ibk<66; ibk++) {
    if (v.obsnote[65-ibk] == ' ' || v.obsnote[66-ibk] == ' ') break;
  }
  if (ibk>65) {
    zvmessage(" error parsing NOTE","");
    ibk = 0;
  }
  strncpy( &line[9], v.obsnote, 66-ibk);
  strcpy( &line[75-ibk], "\r\n");
  strcat(bufpoint,line);
  x -= 66-ibk;
  y += 66-ibk;
  for (; x>0;) {
    strncpy( line,"  ", 2);
    if (x <= 74) {
      strncpy( &line[2], &v.obsnote[y], x);
      strcpy( &line[x+2], "\"\r\n");
      x = -1;
    }
    else {
      /* back up if a word is split at EOL: */
      for (ibk=0; ibk<72; ibk++) {
        if (v.obsnote[y+71-ibk] == ' ' || v.obsnote[y+72-ibk] == ' ') break;
      }
      if (ibk>71) {
        zvmessage(" error parsing NOTE","");
        ibk = 0;
      }
      strncpy( &line[2], &v.obsnote[y], 72-ibk);
      strcpy( &line[74-ibk], "\r\n");
      x -= 72-ibk;
      y += 72-ibk;
    }
    strcat(bufpoint,line);
  }
}

skipnote:
sprintf(line," PRODUCT_ID = \"%s\"\r\n", v.prod_id);
strcat(bufpoint,line);

	/* Get current date and time -- use date part only
	 * for PRODUCT_CREATION_DATE, then add time for
	 * DATE_TIME (below) */
lt = time(NULL);
ptr = localtime(&lt);
year = 1900+(*ptr).tm_year;
sprintf(tstr,"%04d",year);
sprintf( line, "-%02d-%02d", ((*ptr).tm_mon+1), (*ptr).tm_mday);
strcat( tstr, line);
sprintf( line, " PRODUCT_CREATION_DATE = %s\r\n", tstr);
strcat( bufpoint, line);
sprintf( line, "T%02d:%02d:%02d", (*ptr).tm_hour, (*ptr).tm_min, (*ptr).tm_sec);
strcat(tstr,line);	/* to be used below */

if (specialp) {
  sprintf(line," SPECIAL_PROCESSING_TYPE = %d\r\n\r\n", specialp);
  strcat(bufpoint,line);
  strcat(bufpoint, 
   "/* The EDR from which this product was made required special */\r\n");
  strcat(bufpoint, 
   "/* processing by the NIMS team due to anomalous behavior of  */\r\n");
  strcat(bufpoint, 
   "/* the NIMS instrument in the Jupiter radiation field during */\r\n");
  strcat(bufpoint, 
   "/* part of the G1 encounter.  There may be some loss of data */\r\n");
  strcat(bufpoint, 
   "/* quality.  See [DOCUMENT]SPECPROC.TXT on CD for details.   */\r\n\r\n");
}

vstat = 1;	/* for fill_if_error */
fill_if_error(v.picnos[0],"String");	/* convert blank string to " " */
if( v.picnos[0][0]!='?' && v.picnos[0][0]!=0 && strcmp(v.picnos[0]," ")!=0 
 && strcmp(v.picnos[0],"NULL")!=0 ) {
  if (!strcmp( v.picnos[0], v.picnos[1])) {	/* if begin = end */
    sprintf(line," IMAGE_ID = %s\r\n\r\n",v.picnos[0]);
  }
  else {
    sprintf(line," IMAGE_ID = (%s",v.picnos[0]); 
    strcat(line,",'...',");
    strcat(line,v.picnos[1]);
    strcat(line,")\r\n\r\n");
  }
  strcat(bufpoint,line);
}
else strcat(bufpoint," IMAGE_ID = NULL\r\n\r\n");

if (!calib) {
  sprintf(line," INCIDENCE_ANGLE = %5.2f\r\n",v.inci_angle);
  strcat(bufpoint,line);
  sprintf(line," EMISSION_ANGLE = %5.2f\r\n",v.emis_angle);
  strcat(bufpoint,line);
  sprintf(line," PHASE_ANGLE = %5.2f\r\n",v.phas_angle);
  strcat(bufpoint,line);
  sprintf(line," SOLAR_AZIMUTH = %5.2f\r\n",v.azimuth_sun);
  strcat(bufpoint,line);
  sprintf(line," SUB_SPACECRAFT_AZIMUTH = %5.2f\r\n",v.azimuth_sc);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SPACECRAFT_LATITUDE = %5.2f\r\n",v.b_ssc_lat);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SPACECRAFT_LONGITUDE = %5.2f\r\n",v.b_ssc_lon);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SPACECRAFT_LATITUDE = %5.2f\r\n",v.e_ssc_lat);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SPACECRAFT_LONGITUDE = %5.2f\r\n",v.e_ssc_lon);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SOLAR_LATITUDE = %5.2f\r\n",v.b_ssl_lat);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SOLAR_LONGITUDE = %5.2f\r\n",v.b_ssl_lon);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SOLAR_LATITUDE = %5.2f\r\n",v.e_ssl_lat);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SOLAR_LONGITUDE = %5.2f\r\n",v.e_ssl_lon);
  strcat(bufpoint,line);
  sprintf(line," MINIMUM_SLANT_DISTANCE = %9.2f\r\n",v.min_range);
  strcat(bufpoint,line);
  sprintf(line," MAXIMUM_SLANT_DISTANCE = %9.2f\r\n",v.max_range);
  strcat(bufpoint,line);
  if (v.dpstat>0) {
    sprintf(line," POINTING_OFFSET = (%f,%f)\r\n",v.dpoint[0],v.dpoint[1]);
    strcat(bufpoint,line);
  }
  sprintf(line," SCAN_RATE_TOLERANCE = %.6f\r\n", 1000.*v.slewtol);
  strcat(bufpoint,line);
  sprintf(line," MEAN_SCAN_RATE = %.6f\r\n",v.slew_rate);
  strcat(bufpoint,line);
  /* this is the former SCAN_RATE_NOTE: */
  strcat(bufpoint,"/* The unit of SCAN RATE TOLERANCE is");
  strcat(bufpoint," mrad/s. */\r\n/* The unit of MEAN SCAN RATE is the Nyquist");
  strcat(bufpoint," scanning rate, which depends on */\r\n/* the instrument mode:");
  strcat(bufpoint," it is one-half FOV (0.5 mrad) per grating cycle. */\r\n");
}
sprintf(line," MIN_SPACECRAFT_SOLAR_DISTANCE = %g\r\n",v.min_sun_d);
strcat(bufpoint,line);
sprintf(line," MAX_SPACECRAFT_SOLAR_DISTANCE = %g\r\n",v.max_sun_d);
strcat(bufpoint,line);
sprintf(line," MINIMUM_CENTRAL_BODY_DISTANCE = %9.2f\r\n",v.min_cb_d);
strcat(bufpoint,line);
sprintf(line," MAXIMUM_CENTRAL_BODY_DISTANCE = %9.2f\r\n\r\n",v.max_cb_d);
strcat(bufpoint,line);

strcat(bufpoint,"/*  Data description: instrument status  */\r\n\r\n");

sprintf(line," INSTRUMENT_MODE_ID = %s\r\n",v.nimsmode);
strcat(bufpoint,line);					/* instrument mode   */

	/* gain state(s) -- can be as many as Cal. files */
if (v.gain_state[0] <= 0) strcat( bufpoint, " GAIN_MODE_ID = NULL\r\n");
else if (ncals==1) {
  sprintf(line," GAIN_MODE_ID = %d\r\n",v.gain_state[0]);
  strcat(bufpoint,line);
}
else {
  write_PDS_line(bufpoint,'I'," GAIN_MODE_ID",v.gain_state,ncals,0);
}

strcat(bufpoint," CHOPPER_MODE_ID =");			/* chopper mode      */
if (v.chopper==0) strcat(bufpoint," \'63_HERTZ\'\r\n");
else if (v.chopper==1) strcat(bufpoint," REFERENCE\r\n");
else if (v.chopper==2) strcat(bufpoint," OFF\r\n");
else if (v.chopper==3) strcat(bufpoint," FREE_RUN\r\n");
sprintf(line," START_GRATING_POSITION = %02d\r\n",v.grating_start_pos);
strcat(bufpoint,line);
sprintf(line," OFFSET_GRATING_POSITION = %02d\r\n",v.grating_offset);
strcat(bufpoint,line);
sprintf(line," GRATING_POSITION_INCREMENT = %02d\r\n",v.grating_delta);
strcat(bufpoint,line);
if (v.nimsmode[2] == 'x' || v.nimsmode[2] == 'X') 	/* Fixed modes */
  sprintf(line," GRATING_POSITIONS = %02d\r\n\r\n",1);
else
  sprintf(line," GRATING_POSITIONS = %02d\r\n\r\n",v.grating_steps);
strcat(bufpoint,line);

switch( v.nimsmode[3] ) {
  case 'd':		/* BANDEDGE */
  case 'D':  

  sprintf(line," BAND_EDGE_GRATING_POSITION_1 = %d\r\n",v.bandedge_gp1);
  strcat(bufpoint,line);
  sprintf(line," BAND_EDGE_GRATING_POSITION_2 = %d\r\n",v.bandedge_gp2);
  strcat(bufpoint,line);
  break;

  case 'p':		/* STOP SLIDE */
  case 'P':

  sprintf(line," STOP_SLIDE_MODE_ID = %s\r\n",v.stop_slide);
  strcat(bufpoint,line);
  break;

  default:
  
  break;
}

sprintf(line," MEAN_FOCAL_PLANE_TEMPERATURE = %.2f\r\n",v.temp[0]);
strcat(bufpoint,line);
sprintf(line," MEAN_RAD_SHIELD_TEMPERATURE = %.2f\r\n",v.temp[1]);
strcat(bufpoint,line);
sprintf(line," MEAN_TELESCOPE_TEMPERATURE = %.2f\r\n",v.temp[2]);
strcat(bufpoint,line);
sprintf(line," MEAN_GRATING_TEMPERATURE = %.2f\r\n",v.temp[3]);
strcat(bufpoint,line);
sprintf(line," MEAN_CHOPPER_TEMPERATURE = %.2f\r\n",v.temp[4]);
strcat(bufpoint,line);
sprintf(line," MEAN_ELECTRONICS_TEMPERATURE = %.2f\r\n\r\n",v.temp[5]);
strcat(bufpoint,line);

if (strcmp(v.dataformat,"RAW_DATA_NUMBER")) {	/* radiance cube */
  if (v.drk_flg>0) {
    write_PDS_line(bufpoint,'F'," MEAN_DARK_DATA_NUMBER",v.drkave,17,2);
    strcat(bufpoint,"/* The \"mean dark data numbers\" are the DN value of dark sky for");
    strcat(bufpoint," each of the */\r\n/* 17 NIMS detectors, averaged over the");
    strcat(bufpoint," mirror-position-specific values used */\r\n/*");  
    strcat(bufpoint," in the computation of radiance.  The original dark values");
    strcat(bufpoint," were obtained */\r\n/* from either off-limb portions of the");
    strcat(bufpoint," observation or special \"heaven dark\" */\r\n/* observations");
    strcat(bufpoint," for an encounter. */\r\n\r\n");  
  }
  if (v.sens_flg>0) {
    write_PDS_line(bufpoint,'F'," THERMAL_DETECTOR_OFFSET", thoff, 3, 2);
    write_PDS_line(bufpoint,'F'," THERMAL_DETECTOR_SENS_RATIO", sfact, 3, 2);
    strcat(bufpoint,"\r\n");  
  }
}

	/* BAND BIN GROUP */

strcat(bufpoint," GROUP = BAND_BIN\r\n\r\n");

strcat(bufpoint,"/*  Spectral axis description */\r\n\r\n");

write_PDS_line(bufpoint,'F',"  BAND_BIN_CENTER",b.waves,nbb,4);

strcat(bufpoint,"  BAND_BIN_UNIT = MICROMETER\r\n");

write_PDS_line(bufpoint,'I',"  BAND_BIN_ORIGINAL_BAND", b.org_band,nbb,0);
write_PDS_line(bufpoint,'I',"  BAND_BIN_GRATING_POSITION",b.grating, nbb,0);
write_PDS_line(bufpoint,'I',"  BAND_BIN_DETECTOR", b.detector,nbb,0);

if (strcmp(v.dataformat,"RAW_DATA_NUMBER")) {	/* radiance cube */

  if (bytes[object]==2) {	/* scaled to halfword */
    write_PDS_line( bufpoint, 'F', "  BAND_BIN_BASE", v.rad_base, nbb, -1);
    write_PDS_line( bufpoint, 'F', "  BAND_BIN_MULTIPLIER", v.radiance,
     nbb, -1);

    strcat(bufpoint,"/* Band Bin scaling is:  True_value = base +");
    strcat(bufpoint," (multiplier * stored_value). */\r\n/* Scaling of");
    strcat(bufpoint," radiance is band-dependent.  But see also");
    strcat(bufpoint," CORE SCALING, above. */\r\n");
  }

  write_PDS_line(bufpoint,'F',"  BAND_BIN_SOLAR_FLUX",v.solarflux, nbb, 4);

  if (v.sens_flg>0) {
    write_PDS_line(bufpoint,'F',"  BAND_BIN_SENSITIVITY",v.radsens, nbb, 4);

    strcat(bufpoint,"/* \"Band Bin Sensitivity\" is the sensitivity for each");
    strcat(bufpoint," band, in units of */\r\n/* DN/radiance_unit (see CORE UNIT).");
    strcat(bufpoint," These values are functions of */\r\n/* reported focal plane");
    strcat(bufpoint," assembly temperature during the observation and */\r\n");  
    strcat(bufpoint,"/* of ground and flight calibration data.  They may be");
    strcat(bufpoint," used to construct */\r\n/* \"idealized data numbers\" (DNs");
    strcat(bufpoint," which would have been measured by an */\r\n");  
    strcat(bufpoint,"/* anomaly-free instrument) by the formula: */\r\n");  
    strcat(bufpoint,"/*	DN = dark_value + sensitivity * radiance, */\r\n");
    strcat(bufpoint,"/* where 'dark_value' is approximated by the"); 
    strcat(bufpoint," MEAN_DARK_DATA_NUMBER array, */\r\n");
    strcat(bufpoint,"/* preceding the BAND_BIN");
    strcat(bufpoint," group. */\r\n/* Note that actually");
    strcat(bufpoint," measured raw DNs are not obtainable in this way, */\r\n");
    strcat(bufpoint,"/* due to corrections for instrument anomalies (see the");
    strcat(bufpoint," referenced */\r\n/* INSTRUMENT_DESCRIPTION for details) and");
    strcat(bufpoint," possible resampling of the */\r\n/* data.  The above");
    strcat(bufpoint," formula for DN also does not hold for the higher */\r\n");  
    strcat(bufpoint,"/* intensity regime in the thermal detectors (15-17),");
    strcat(bufpoint," for which the */\r\n/* following formula applies: */\r\n");
    strcat(bufpoint,"/*      DN = thermal_offset + sensitivity * radiance");
    strcat(bufpoint," / sens_ratio */\r\n");
    strcat(bufpoint,"/* where 'thermal_offset' and 'sens_ratio' for detectors");
    strcat(bufpoint," 15, 16, and 17 are */\r\n/* given by the");
    strcat(bufpoint," THERMAL_DETECTOR_OFFSET and THERMAL_DETECTOR_SENS_RATIO */\r\n");
    strcat(bufpoint,"/* arrays, preceding the BAND_BIN group. */\r\n");

    strcat(bufpoint,"/* The radiances for which the above formula applies, are");
    strcat(bufpoint," those lying above: */\r\n");
    strcat(bufpoint,"/*       Cutoff_radiance = thermal_offset / sensitivity */\r\n");
  }
}

strcat(bufpoint," END_GROUP = BAND_BIN\r\n\r\n");

if (calib) goto endqub;

strcat(bufpoint," GROUP = IMAGE_MAP_PROJECTION\r\n");

strcat(bufpoint,"/* Projection description */\r\n");

sprintf(line,"  MAP_PROJECTION_TYPE = %s\r\n",v.projection);
strcat(bufpoint,line);

sprintf(line,"  MAP_SCALE = %6.3f\r\n",v.scale);/* Map scale in KM/PIXEL  */
strcat(bufpoint,line);

if ( v.scale != 0 )
  pixelsperdegree = (2.0 * 3.14159 * v.radii[0])/(360.0 * v.scale);
else
  pixelsperdegree = 0;
sprintf(line,"  MAP_RESOLUTION = %6.3f\r\n",pixelsperdegree);
strcat(bufpoint,line);

	/* any items obtained from the VICAR "MAP00x" labels must be
	 * corrected to planetographic lat. if requested */

if (strcmp(v.projection,"POINT_PERSPECTIVE")==0) {
  sprintf(line,"  SUB_SPACECRAFT_LATITUDE = %6.2f\r\n",v.ssc_lat);
  strcat(bufpoint,line);
  sprintf(line,"  SUB_SPACECRAFT_LONGITUDE = %6.2f\r\n",v.ssc_lon);
  strcat(bufpoint,line);
	/* subtract 1 because PDS definition is relative to (1,1) &
	 * switch signs for Unix ISIS ...  */
  sav = 1.-v.ssc_line;
  sprintf(line,"  LINE_SUB_SPACECRAFT_OFFSET = %6.2f\r\n",sav);
  strcat(bufpoint,line);
  sav = 1.-v.ssc_samp;
  sprintf(line,"  SAMPLE_SUB_SPACECRAFT_OFFSET = %6.2f\r\n",sav);
  strcat(bufpoint,line);
  sprintf(line,"  TARGET_CENTER_DISTANCE = %.1f\r\n",v.range);
  strcat(bufpoint,line);
  sav = 1.-v.oaline;
  sprintf(line,"  LINE_OPTICAL_AXIS_OFFSET = %6.2f\r\n",sav);
  strcat(bufpoint,line);
  sav = 1.-v.oasamp;
  sprintf(line,"  SAMPLE_OPTICAL_AXIS_OFFSET = %6.2f\r\n",sav);
  strcat(bufpoint,line);

	/* for POV, scale is in pixels/mm in focal plane, and
	 * the focal length in mm is also required: */
  sprintf(line,"  FOCAL_LENGTH = %5.1f\r\n",v.focal);
  strcat(bufpoint,line);
  sprintf(line,"  FOCAL_PLANE_SCALE = %6.3f\r\n",v.cscale);
  strcat(bufpoint,line);
}
else {

  sprintf(line,"  CENTER_LATITUDE = %6.2f\r\n",v.center_lat);
  strcat(bufpoint,line);
  sprintf(line,"  CENTER_LONGITUDE = %6.2f\r\n",v.center_lon);
  strcat(bufpoint,line);

  /* subtract 1 because PDS definition is relative to (1,1), and
   * switch signs for Unix ISIS ...  */
  sav = 1. - v.center_line;
  sprintf(line,"  LINE_PROJECTION_OFFSET = %6.2f\r\n",sav);
  strcat(bufpoint,line);
  sav = 1. - v.center_samp;
  sprintf(line,"  SAMPLE_PROJECTION_OFFSET = %6.2f\r\n",sav);
  strcat(bufpoint,line);
}

/* need OFFSET_DIRECTION keyword to distinguish new labels from old */
strcat( bufpoint, "  OFFSET_DIRECTION = TO_ORIGIN\r\n");

sprintf(line,"  MINIMUM_LATITUDE = %6.2f\r\n",v.min_lat);
strcat(bufpoint,line);
sprintf(line,"  MAXIMUM_LATITUDE = %6.2f\r\n",v.max_lat);
strcat(bufpoint,line);

  /* convert VICAR Min/Max Long's to East/Westmost for ISIS ... the 
   * values have already been converted to East, if required, when they 
   * were read in, so just need to ensure long's are positive for PDS 
   * (Max is always Westmost since Vicar items are West long.) */
if (v.min_lon < 0.0) v.min_lon += 360.;
sprintf(line,"  EASTERNMOST_LONGITUDE = %6.2f\r\n",v.min_lon);
strcat(bufpoint,line);
sprintf(line,"  WESTERNMOST_LONGITUDE = %6.2f\r\n",v.max_lon);
strcat(bufpoint,line);

strcat(bufpoint,"  COORDINATE_SYSTEM_TYPE = \"BODY-FIXED ROTATING\"\r\n");
if (pgraphic==1)
  strcat(bufpoint,"  COORDINATE_SYSTEM_NAME = PLANETOGRAPHIC\r\n");
else if (pgraphic==0)
  strcat(bufpoint,"  COORDINATE_SYSTEM_NAME = PLANETOCENTRIC\r\n");
if ( strcmp(v.target,"VENUS")==0 || zvptst("EAST_LON") )
  strcat(bufpoint,"  POSITIVE_LONGITUDE_DIRECTION = EAST\r\n");
else
  strcat(bufpoint,"  POSITIVE_LONGITUDE_DIRECTION = WEST\r\n");

sprintf(line,"  A_AXIS_RADIUS = %.2f\r\n",v.radii[0]); /* A, B, C axes */
strcat(bufpoint,line);
sprintf(line,"  B_AXIS_RADIUS = %.2f\r\n",v.radii[1]);
strcat(bufpoint,line);
sprintf(line,"  C_AXIS_RADIUS = %.2f\r\n",v.radii[2]);
strcat(bufpoint,line);

if( v.projection[0] == 'L' ) {
  sprintf(line,"  FIRST_STANDARD_PARALLEL = %.3f\r\n",v.projitem1);
  strcat(bufpoint,line);
  sprintf(line,"  SECOND_STANDARD_PARALLEL = %.3f\r\n",v.projitem2);
  strcat(bufpoint,line);
}

if( v.projection[4] == 'Q' ) {
  sprintf(line,"  REFERENCE_LATITUDE = %.3f\r\n",v.projitem1);
  strcat(bufpoint,line);
  sprintf(line,"  REFERENCE_LONGITUDE = %.3f\r\n",v.projitem2);
  strcat(bufpoint,line);
}
if( strcmp(v.projection,"POLAR_ORTHOGRAPHIC")==0 ||
    strcmp(v.projection,"POINT_PERSPECTIVE")==0 ||
    strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC")==0 ||
    strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC")==0 ) {
  sprintf(line,"  MAP_PROJECTION_ROTATION = %6.2f\r\n",v.rotation);
  strcat(bufpoint,line);
}
strcat(bufpoint,"  SAMPLE_FIRST_PIXEL = 1\r\n");
if (tube) 
  sprintf(line,"  SAMPLE_LAST_PIXEL = %d\r\n",cubsiz[1]);
else
  sprintf(line,"  SAMPLE_LAST_PIXEL = %d\r\n",ns[object]);
strcat(bufpoint,line);
strcat(bufpoint,"  LINE_FIRST_PIXEL = 1\r\n");
if (tube) 
  sprintf(line,"  LINE_LAST_PIXEL = %d\r\n",cubsiz[0]); 
else
  sprintf(line,"  LINE_LAST_PIXEL = %d\r\n\r\n",inl[object]);
strcat(bufpoint,line);

strcat(bufpoint," END_GROUP = IMAGE_MAP_PROJECTION\r\n\r\n");

endqub:
strcat(bufpoint,"END_OBJECT = QUBE\r\nEND\r\n");

histpoint = (char *)calloc	/* Generate history obj buff*/
	(HISTMEMORY,sizeof( char )); 
if( histpoint==NULL )
  zmabend(" Memory allocation error for history object");

if (histfile) {
  hfile = fopen(hfilnam,"r");
  /* just copy all bytes to our buffer: */
  hpoint = histpoint;
  for (;;) {
    vstat = fscanf(hfile, "%c", &cc);
    if (vstat==EOF) break;
    *(hpoint++) = cc;
  }
  fclose(hfile);
  for (x=0; x<8; x++) {
    if (!strncmp(hpoint-x,"END",3)) break;
  }
  if (x>=7) zmabend(" error reading History file");
  hpoint -= x;	/* remove the final "END" line */
  *hpoint = 0;
  strcat(histpoint,"\r\n\r\nGROUP = VISIS2\r\n\r\n");
}
else
  strcpy(histpoint,"GROUP = VISIS2\r\n\r\n");
sprintf(line,"  VERSION_DATE = %s\r\n", verdat);
strcat(histpoint,line);
sprintf(line,"  DATE_TIME = %s\r\n",tstr);
strcat(histpoint,line);
sprintf(line,"  NODE_NAME = \"MIPL\"\r\n  USER_NAME = \"%s\"\r\n",v.requestor);	
strcat(histpoint,line);
strcat(histpoint,"  SOFTWARE_DESC = \"ISIS cube file with PDS label has been");
strcat(histpoint," generated as\r\n    systematic product by MIPL using the");
strcat(histpoint," following programs:\r\n");
if (!histfile) {
  if (mphase) {
    strcat(histpoint,"      NIMSMERGE2 to create EDRs;\r\n");
    strcat(histpoint,"      NIMSCMM2 to create the merged mosaic & geometry cube;\r\n");
  }
  else {
    strcat(histpoint,"      NIMSMERGE to create EDRs;\r\n");
    strcat(histpoint,"      NIMSCMM to create the merged mosaic & geometry cube;\r\n");
  }
}
strcat(histpoint,"      HIST2D to create a two-dimensional histogram;\r\n");
if (specplots > 0) 
  strcat(histpoint,"      SPECPLOT to create the spectral plots;\r\n");
if (cubefiles == 3)
  strcat(histpoint,"      NIMSFLOAT, F2, and INSERT3D to create the SII cube;\r\n");
for (x=0; x<ncubtasks; x++) {
  if (strncmp(&cubtasks[x * TASK_LEN], "NIMSR2IO", 8) == 0) {
    strcat(histpoint,
     "      NIMSR2IOF to convert Radiance to BDRF;\r\n");
  }
  if (strncmp(&cubtasks[x * TASK_LEN], "NIMSXCA", 7) == 0) {
    strcat(histpoint,
     "      NIMSXCA to correct for the cross-cone sensitivity function;\r\n");
  }
  if (strncmp(&cubtasks[x * TASK_LEN], "NIMSBBFI", 8) == 0) {
    strcat(histpoint,
     "      NIMSBBFIT to fit Temperatures and write these to backplanes;\r\n");
  }
}
if (histfile) {
  strcat(histpoint,"      VISIS2 to create the ISIS cube.\r\n");
  strcat(histpoint,"    The input was an ISIS cube file, from which the core ");
  strcat(histpoint,"and backplanes\r\n    were extracted;  only the other ");
  strcat(histpoint,"cube objects were regenerated.\"\r\n\r\n");
}
else
  strcat(histpoint,"      VISIS2 to create the ISIS cube.\"\r\n\r\n");

sprintf(line,"  USERNOTE = \"%s\"\r\n\r\n",v.prodnote);
strcat(histpoint,line);

if (!histfile || !strcmp(v.mask,"MASK")) 
  strcat(histpoint,"  GROUP = PARAMETERS\r\n\r\n");

if (!histfile) {
  if (nedrs==1) {
    sprintf(line,"    EDR_FILE_NAME = \"%s\"\r\n",v.edrfiles);
    strcat(histpoint,line);
  }
  else if (nedrs>1) 
    write_PDS_cline( histpoint, "    EDR_FILE_NAME", v.edrfiles, nedrs, 101);

  if (nsc_gaps) {
    write_PDS_line(histpoint,'I',"    SCLK_GAPS", sclk_gaps,nsc_gaps,0);
    strcat(histpoint,"/* The SCLK_GAPS array contains a list of pairs of SCLK values which define */\r\n");
    strcat(histpoint,"/* periods for which EDR data were excluded from cube processing, within */\r\n");
    strcat(histpoint,"/* the complete period defined by the NATIVE_START_TIME and NATIVE_STOP_TIME */\r\n");
    strcat(histpoint,"/* values given in the label.  Note that NATIVE_TIME is defined as a triplet */\r\n");
    strcat(histpoint,"/* of (RIM,MF,RTI), while SCLK is defined as 100*RIM+MF. */\r\n");
  }
  if (mphase) {
    if (!strcmp(v.aacs_file,"")) {
      sprintf(line,"    PLATFORM_CKERNEL_NAME = \"%s\"\r\n",v.pfm_ck);
      strcat(histpoint,line);
      sprintf(line,"    ROTOR_CKERNEL_NAME = \"%s\"\r\n",v.rot_ck);
      strcat(histpoint,line);
    }
    else if (!calib) {
      sprintf(line,"    AACS_FILE_NAME = \"%s\"\r\n",v.aacs_file);
      strcat(histpoint,line);
    }
  }
  else {
    sprintf(line,"    POINTING_SOURCE = \"%s\"\r\n",v.pfm_ck);
    strcat(histpoint,line);
  }
  if (v.dpstat>0) {
    sprintf(line,"    POINTING_OFFSET = (%f,%f)\r\n",v.dpoint[0],v.dpoint[1]);
    strcat(histpoint,line);
  }
  if (wamp>1.e-7) {
    sprintf(line,"    WOBBLE_AMPLITUDE = %f\r\n",wamp);
    strcat(histpoint,line);
    sprintf(line,"    WOBBLE_FREQUENCY = %f\r\n",wfreq);
    strcat(histpoint,line);
    sprintf(line,"    WOBBLE_PHASE = %f\r\n",wphase);
    strcat(histpoint,line);
    if (wcone>-900.) {
      sprintf(line,"    WOBBLE_CONE_ESTIMATE = \"%f\"\r\n",wcone);
      strcat(histpoint,line);
    }
  }
  sprintf(line,"    SP_KERNEL_FILE_NAME = \"%s\"\r\n",v.spkernel);
  strcat(histpoint,line);
  sprintf(line,"    I_KERNEL_FILE_NAME = \"%s\"\r\n",v.ikernel);
  strcat(histpoint,line);
  sprintf(line,"    SPIKE_FILE_NAME = \"%s\"\r\n",v.despikefile);
  strcat(histpoint,line);
  sprintf(line,"    BOOM_FILE_NAME = \"%s\"\r\n",v.deboomfile);
  strcat(histpoint,line);
  if (ncals==1) {
    sprintf(line,"    DARK_VALUE_FILE_NAME = \"%s\"\r\n",v.darkfile);
    strcat(histpoint,line);
  }
  else if (ndrks>1) 
    write_PDS_cline( histpoint, "    DARK_FILE_NAME", v.darkfile, ndrks, 100);

  if (ncals==1) {
    sprintf(line,"    CALIBRATION_FILE_NAME = \"%s\"\r\n",v.calfile);
    strcat(histpoint,line);
  }
  else if (ncals>1) 
    write_PDS_cline( histpoint, "    CALIBRATION_FILE_NAME", v.calfile, ncals,
     100);

  sprintf(line,"    SOLAR_FLUX_FILE_NAME = \"%s\"\r\n",v.sol_file);
  strcat(histpoint,line);
  sprintf(line,"    MERGED_MOSAIC_FILE_NAME = \"%s\"\r\n",
	inputfiles[numoffiles-cubefiles]);
  strcat(histpoint,line);

  if (ithreshval) {
    write_PDS_line(histpoint,'I',"    INSTRUMENT_THRESHOLD",v.threshval,17,4);
    strcat(histpoint,"/* \"Instrument Thresholds\" are per-detector threshold values for data return */\r\n"); 
    strcat(histpoint,"/* during Galileo phase-2 operation.  When thresholding is selected, detector */\r\n");
    strcat(histpoint,"/* DNs less than the threshold value for that detector are not returned; */\r\n"); 
    strcat(histpoint,"/* those DNs are therefore known to be less than that threshold value. */\r\n"); 
    strcat(histpoint,"/* Detectors with zero threshold values are not thresholded. */\r\n"); 
  }

  sprintf(line,"    GRATING_POSITION_CORRECTION = %.4f\r\n", v.pshift);
  strcat(histpoint,line);
  sprintf(line,"    GRATING_STEP_INFLATION = %.4f\r\n", v.ainfl);
  strcat(histpoint,line);
  strcat(histpoint,"/* The \"Grating Position Correction\" and \"Grating Step Inflation\" are */\r\n");
  strcat(histpoint,"/* adjustments to the nominal grating positions, based on flight calibrations */\r\n");
  strcat(histpoint,"/* and known sharp spectral features of the target, used in determination of */\r\n");
  strcat(histpoint,"/* wavelengths.  GRATING_POSITION_CORRECTION is an additive term to the */\r\n");
  strcat(histpoint,"/* grating position and (1.0 + GRATING_STEP_INFLATION) is a multiplicative */\r\n");
  strcat(histpoint,"/* term modifying the grating stepsize. */\r\n\r\n");
}

if( strcmp(v.mask,"MASK") == 0 ) {
  sprintf(line,"  SUMMARY_IMAGE_RED_ID = %d\r\n",v.red_siid);
  strcat(histpoint,line);
  sprintf(line,"  SUMMARY_IMAGE_GREEN_ID = %d\r\n",v.grn_siid);
  strcat(histpoint,line); 
  sprintf(line,"  SUMMARY_IMAGE_BLUE_ID = %d\r\n",v.blu_siid);
  strcat(histpoint,line);
  sprintf(line,"  ADAPT_STRETCH_SAT_FRAC = %.4f\r\n",v.astretch[1]); 
  strcat(histpoint,line);
  sprintf(line,"  ADAPT_STRETCH_SAMP_FRAC = %.4f\r\n",v.astretch[0]);
  strcat(histpoint,line);
  sprintf(line,"  RED_STRETCH_RANGE = (%5.0f,%5.0f)\r\n",
	v.rstretch[0],v.rstretch[1]);
  strcat(histpoint,line);
  sprintf(line,"  GREEN_STRETCH_RANGE = (%5.0f,%5.0f)\r\n",
 	v.gstretch[0],v.gstretch[1]);
  strcat(histpoint,line);
  sprintf(line,"  BLUE_STRETCH_RANGE = (%5.0f,%5.0f)\r\n",
	v.bstretch[0],v.bstretch[1]);
  strcat(histpoint,line);
}

if (!histfile || !strcmp(v.mask,"MASK")) 
  strcat(histpoint,"  END_GROUP = PARAMETERS\r\n\r\n");

strcat(histpoint,"END_GROUP = VISIS2\r\n\r\nEND\r\n");

x=strlen(histpoint);
additionalbytes = BUFFERSIZE - x%BUFFERSIZE + HST_BLANK_RECORDS*BUFFERSIZE;
for(y=0;y<additionalbytes;y++)
	strcat(histpoint," ");
x=strlen(histpoint);
if (x > HISTMEMORY) zmabend("*** history label too big ***");

*nhistrecs = x/BUFFERSIZE;		/* Calc # of history records */

labelbytes = strlen(bufpoint);
additionalbytes = BUFFERSIZE - labelbytes%BUFFERSIZE 
		  + LBL_BLANK_RECORDS*BUFFERSIZE;
for(y=0;y<additionalbytes;y++)
	strcat(bufpoint," ");
labelbytes = strlen(bufpoint);		/* Calc # of label records   	*/
if (labelbytes > LABELMEMORY) zmabend("*** PDS label too big ***");

*nlabrecs  = labelbytes/BUFFERSIZE;

*nrecs += (*nlabrecs + *nhistrecs);	/* Update file record total	*/

					/* OBJECT FILE POINTERS    	*/
sprintf(value[0],"%6d",*nrecs);		/* Number of records in file	*/
sprintf(value[1],"%6d",*nlabrecs);	/* Number of label records	*/
sprintf(value[2],"%6d",*nlabrecs+1);	/* Pointer to history object	*/
sprintf(value[3],"%6d",			/* Pointer to 2D histogram 	*/
	*nlabrecs+*nhistrecs+1);
if( specplots > 0 )			/* Pointers to SPECTRA & CUBE	*/
	{
	for( x=0; x<2; x++ )
		sprintf(value[x+4],"%6d",*nlabrecs+*nhistrecs+objptr[x]+1);
	z = NUMOFPTRS;
	}
else		
	{
	sprintf(value[4],"%6d",*nlabrecs+*nhistrecs+objptr[0]+1);
	z = NUMOFPTRS - 1;
	}

count = bufptr = 0;			/* Place POINTERS and above 	*/
while( count < z )			/* values in PDS label		*/
	{
	if(bufpoint[bufptr]=='\r')
		if( strncmp(&bufpoint[bufptr-6],"XXXXXX",6)==0 )
			strncpy(&bufpoint[bufptr-6],value[count++],6);
	bufptr++;
	}
					/* NOTE: This should be elsewhere. */
count = bufptr = 0;			/* Replace FILE DIRTY with	*/
while( count == 0 && bufptr < 200 )	/* FILE CLEAN			*/
	{
	if(bufpoint[bufptr]=='\r')
		if(strncmp(&bufpoint[bufptr-5],"DIRTY",5)==0)
			{
			strncpy(&bufpoint[bufptr-5],"CLEAN",5);
			count = 1;
			}
	bufptr++;
	}
}


/****************************************************************************/
FUNCTION determine_band_bin_parms()
/*
 * Determine the Band Bin parameters based on instrument mode
 */
{
  int nbm1, ngp1, nbpg, x, y, z, z1;

  if (!strncmp(v.nimsmode, "FIX", 3)) {	/* FIXED GRATING */
    ngp1 = 1;   /* Number of grating positions = 1 */
    nbm1 = 17;  /* Number of wavelengths = 17      */
  }
  else if (!strncmp(v.nimsmode, "BAND", 4)) {	/* BANDEDGE */
    ngp1 = 2;    /* Number of grating positions = 2 */
    nbm1 = 34;  /* Number of wavelengths = 34      */
  }
  else if (!strncmp(v.nimsmode, "SHORT", 5)) {	/* SHORT MAP or SPECT */
    ngp1 = 6;    /* Number of grating positions = 6 */
    nbm1 = 102;  /* Number of wavelengths = 102     */
  }
  else if (!strncmp(v.nimsmode, "FULL", 4)) {	/* FULL MAP OR SPECT */
    ngp1 = 12;  /* Number of grating positions = 12 */
    nbm1 = 204;  /* Number of wavelengths = 204      */
  }
  else if (!strncmp(v.nimsmode, "LONG", 4)) {	/* LONG MAP OR SPECT */
    ngp1 = 24;  /* Number of grating positions = 24 */
    nbm1 = 408;  /* Number of wavelengths = 408      */
  }  
  else if (!strncmp(v.nimsmode, "SPECI", 5)) {	/* SPECIAL SEQUENCE */
    ngp1 = 6;    /* Number of grating positions */
    nbm1 = 102;  /* Number of wavelengths */
  }
  else if (!strncmp(v.nimsmode, "SAFE", 4)) {	/* SAFE MODE */
    ngp1 = 1;    /* Number of grating positions */
    nbm1 = 17;  /* Number of wavelengths */
  }
  else
    zmabend("** Band_bin unable to determine mode! **");

  /* verify that the nominal wavelength count agrees with that of the
   * band mask, and that the actual band count agrees with the mask
   * contents */

  if (nbm<0) 	/* no band mask, so assume all bands present */
    for (x=0; x<nbm1; x++)
      bmask[x] = 1;
  else
    if (nbm != nbm1) zmabend(" Invalid band mask dimension!");
  nbm = nbm1;

  for (x=0, y=0; x<nbm; x++)
    if (bmask[x]) y++;
  if (y != nbb) zmabend(" Cube band dimension inconsistent with band mask!");

  for ( x=0, z1=0, z=0; x<17; x++ ) {
    for ( y=0; y<ngp1; y++, z++ ) {
      if (bmask[z]) {
	b.grating[z1] = y;
	b.detector[z1] = x+1; 
	b.org_band[z1] = z+1;		/* Original band parameter  */
	z1++;
      }
    }
  }

  /* check the actual number of GPs selected */
  ngp = 0;
  for (y=0; y<ngp1; y++) {
    for (x=0; x<17; x++) {
      z = x*ngp1+y;
      if (bmask[z]) {
	grates[ngp] = y;
	ngp++;
	break;
      }
    }
  }

  /* and the number of backplanes */
  if( cubefiles == 3 ) {
    nbpln = nb[numoffiles-1] + nb[numoffiles-2];
  }
  else {
    nbpln = nb[numoffiles-1];
  }

  /* hack for Calibration data: */
  if (calib) {
    nbpln = 1;
    return;
  }

  /* use 'nbpln' to determine whether it's a P-tube */
  if (tube) {
    ptub = 0;
    nbpg = (nbpln-6)/ngp;		/* # backplanes per GP */
    if (v.binning[0]=='F') nbpg -= 2;	/* rt.-edge lin/sam */
    if (nbpg == 10) ptub = 1;
    else if (nbpg != 4) zmabend(" Invalid tube backplane count!");
  }
}				


/****************************************************************************/
FUNCTION determine_month( month, string )
/*
 * Determine month number string from month name.
 */
char	month[],
	string[];
{

switch( string[0] )	
	{
	case 'J': 	if( string[1] == 'A' )
				strcpy(month,"01");
			else
				if( string[2] == 'N' )
					strcpy(month,"06");
				else
					strcpy(month,"07");
			break;

	case 'F':	strcpy(month,"02");
			break;	

	case 'M':	if( string[2] == 'R' )
				strcpy(month,"03");
			else
				strcpy(month,"05");
			break;	

	case 'A':	if( string[1] == 'P' )
				strcpy(month,"04");
			else
				strcpy(month,"08");
			break;	

	case 'S':	strcpy(month,"09");
			break;	

	case 'O':	strcpy(month,"10");
			break;	

	case 'N':	strcpy(month,"11");
			break;	

	case 'D':	strcpy(month,"12");
			break;	

	/* otherwise, assume input is numeric */
	default:	strncpy(month,&string[1],2);
			month[2] = '\0';
			break;	
	}
}


/****************************************************************************/
FUNCTION fill_if_error(string,type)
/*
 * Replace string with " " if status of VICAR zlget is < 1; check for
 * blank strings and substitute ' ' or " ".
 */
char string[],type[];
{
  int x, y;

  if (vstat<=0)
    strcpy(string," ");
  else {
    x = strlen(string);
    for( y=0; y<x; y++ )
      if( string[y] != ' ' )
	y=x+1;
    if(y==x) {
      if ( type[0] == 's' || type[0] == 'S' )
	strcpy(string," ");
      else {
        if ( type[0] == 'l' || type[0] == 'L' )
	  strcpy(string,"' '");
	else
	  strcpy(string,"xxxxxx");
      }
    }
  }
}


/*****************************************************************************/
int FUNCTION find_keyword(char* keyword, char* buf, int* labptr, int* endptr)
/*
 * Find the keyword and prepare for reading its value 
 */
/*char	keyword[],buf[];			   Item to be found          */
/*int	*labptr;				   Pointer to label element  */
/*int	*endptr;				   end of label to search    */
{						/* Local Variables           */
int	count,					/* Counter/index             */
	found,					/* Item found indicator      */
	keylength,				/* Item name length	     */	
	startptr;				/* Value passed as labptr  */

startptr  = *labptr;				/* Record starting pointer   */
found     = FALSE;				/* Item not found yet        */
keylength = strlen(keyword);			/* Determine itemname length */
while( !found && *labptr < *endptr ) {
  count  = 0;
  while( buf[*labptr] != keyword[count] )
    if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
  if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
  count++;

  while( buf[*labptr] == keyword[count] && count < keylength )
    if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
    else count++;

  if( count == keylength && ( buf[*labptr] == ' ' || buf[*labptr] == '=' ||
   buf[*labptr] == '\r' ) )
    found = TRUE;
}

return 1;
}


/******************************************************************************/
FUNCTION for_tran( str2, str1, bands)
/*
 * Translate "spectral-index" formula: 
 * replace "INx" in the string 'str1' by "By" and store in 'str2', where
 * y = bands[x+1];
 *
 * NOTE that str1 is *not* zero-terminated, but str2 must be zero-terminated.
 */

char str1[80], str2[100];
short bands[5];
{
  int i1, i2, len, x;
  char bandc[5];

	/* find true length of str1 (ignoring blanks): */
  for (i1=79; i1>=0; i1--) if (str1[i1] != ' ') break;
  len = i1;

  for ( i1=0, i2=0; i1<=len && i2<99; ) {

    if (strncmp( &str1[i1], "IN", 2) &&
        strncmp( &str1[i1], "in", 2)) {		/* we're not at an "INx" */
      str2[i2] = str1[i1];
      i1++;
      i2++;
    }
    else {				/* found an "INx" */
      x = str1[i1+2] - 48;	/* ascii-to-numeral */
      if (x<1 || x>5) zmabend(" error in for_tran");
      strcpy( &str2[i2], "B");
      sprintf( bandc, "%d", bands[x-1]);
      strcat( &str2[i2+1], bandc);
      i1 += 3;
      i2 += strlen(bandc)+1;
    }
  }
	/* if string is too long, truncate with some trailing dots */
  if (i2>=30) strcpy( &str2[27], "...");
  else str2[i2] = 0;
}


/****************************************************************************/
int FUNCTION get_integer_value(intitem,buf,labptr)
/*
 * Get integer value.	
 */
int	*intitem,				/* Integer value pointer     */
	*labptr;				/* Label element pointer     */
char 	buf[];					/* PDS label buffer	     */

{						/* Local Variables           */
  char	integer[12];				/* String for integer        */
  int 	count, sign;

  count = 0;
  sign = 1;		/* initialize as positive */
  memset( integer, 0, 12 );

	/* Find first digit/sign */
  while (!isdigit(buf[*labptr]) && buf[*labptr] != '+' &&
   buf[*labptr] != '-') {
    lsef = incrm_check_buffer(labptr);
    if( lsef == 0 ) return 0;
  }
  if (buf[*labptr]=='-') sign = -1;

	/* space past sign: */
  if (buf[*labptr]=='-' || buf[*labptr]=='+') {
    lsef = incrm_check_buffer(labptr);
    if( lsef == 0 ) return 0;
  }
 
	/* Continue until last digit is found */
  while (isdigit(buf[*labptr])) {
    integer[count++]=buf[*labptr];
    lsef = incrm_check_buffer(labptr);
    if( lsef == 0 ) return 0;
  }

	/* Convert string into INT */
  *intitem = sign*atoi(integer);
  return 1;
}

/****************************************************************************/
FUNCTION get_label_items()
/*
 * Get all VICAR label items needed for PDS label and complete all 
 * necessary format conversions.
 */
{
  char monthstr[3], tstr[80],lattyp[15], pos_lon[6];
  float dumflt, latlon[2], matbuf[9], sav;
  double xlin, xsam, xlat, xlon;
  int area[4], cnt, dummy, i, j, x, y, z, nret, nbnd, integers[3];
  MP mpo;

  /* first open the cube file so we can get positive_longitude_direction */
  i = numoffiles - cubefiles;
  vstat = zvopen(inunit[i],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",NULL);
  lsef = mpInit( &mpo);
  if (lsef!=mpSUCCESS) zmabend(" error initializing MP object");
  mpLabelRead( mpo, inunit[i]);
  if (lsef!=mpSUCCESS) zmabend(" error reading map labels");
  lsef = mpGetValues( mpo, mpPOSITIVE_LONGITUDE_DIRECTION, pos_lon, "");
  if (lsef!=mpSUCCESS) {
    zvmessage(" POSITIVE_LONGITUDE_DIRECTION not found, WEST assumed","");
    strcpy( pos_lon, "WEST");
  }
  /* check if we must convert West longitudes to East ... this only happens 
   * if we encounter a cube with TARGET=VENUS and West longitudes ... */
  conv_lon = 0;
  vstat = zlget(inunit[i],"HISTORY",TARGET,v.target,"HIST",tasknam,NULL);
  fill_if_error(v.target,"Literal");
  if (!strcmp(v.target,"VENUS")) {
    if (!strcmp(pos_lon,"west") || !strcmp(pos_lon,"WEST")) conv_lon = 1;
    strcpy( pos_lon, "EAST");
  }
  /* also allow option for East longitudes in ISIS cube: */
  if (zvptst("EAST_LON")) conv_lon = 1;

  vstat = zlget(inunit[i],"HISTORY",CUBE_SIZE,cubsiz,"HIST",tasknam,
   "NELEMENT",-1,NULL);
  if (vstat<0) tube = 0;
  else tube = 1;

  /* check for Calibration target */
  calib = 0;
  if (!strcmp(v.target,"CAL") && strcmp(v.target,"CALLISTO")) calib = 1;
  if (!strcmp(v.target,"SKY")) calib = 1;

  /* open the 2D histogram */
  vstat= zvopen(inunit[0],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",NULL);

  vstat = zlget(inunit[0],"HISTORY","MAXDN",&v.y_axis_max,"HIST","HIST2D",NULL);

  /* get BREAK band # from Histogram label, as that is always present */
  vstat= zlget(inunit[0],"HISTORY","BREAK_NO",&v.breakdet,"HIST","HIST2D",NULL);
  if (vstat<1) v.breakdet = 1;	/* default is all RAD */

  zvclose(inunit[0],NULL);

  for( i=1; i<=specplots; i++ ) {
    vstat = zvopen(inunit[i],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",NULL);
    vstat = zlget(inunit[i],"HISTORY","LATLON",latlon,"NELEMENT",2,
     "HIST","SPECPLOT",NULL);
    if (  vstat==1 ) {
      v.lat[i-1] = latlon[0];
      if (conv_lon)
        v.lon[i-1] = 360.0 - latlon[1];
      else
        v.lon[i-1] = latlon[1];
      if (v.lon[i-1] < 0.) v.lon[i-1] += 360.0;
    }
    else {
      v.lat[i-1] = 0;
      v.lon[i-1] = 0;
    }
    vstat = zlget(inunit[i],"HISTORY","AREA",area,"NELEMENT",4,"HIST","SPECPLOT",
     NULL);
    if ( vstat==1 ) {
      v.lines[i-1]   = area[2];
      v.samples[i-1] = area[3];
    }
    else {
      v.lines[i-1]   = 0;
      v.samples[i-1] = 0;
    }
    zvclose(inunit[i],NULL);
  }

  /* extract map projection data from MP labels (except for Calibration) */
  if (calib) goto endmp;

  /* common to all projections: */
  lsef = mpGetValues( mpo, mpA_AXIS_RADIUS, &v.radii[0], "");
  lsef = mpGetValues( mpo, mpB_AXIS_RADIUS, &v.radii[1], "");
  lsef = mpGetValues( mpo, mpC_AXIS_RADIUS, &v.radii[2], "");
  lsef = mpGetValues( mpo, mpMAP_PROJECTION_TYPE, v.projection, "");

  /* ISIS wants Sinusoidal expanded: */
  /* 2012:  not any more!
  if (strcmp(v.projection,"SINUSOIDAL") == 0) 
    strcpy( v.projection, "'SINUSOIDAL_EQUAL-AREA'");
  */

  /* projection-specfic stuff: */

  if (strcmp(v.projection,"POINT_PERSPECTIVE")==0) {
    lsef = mpGetValues( mpo, mpSUB_SPACECRAFT_LATITUDE, &v.ssc_lat, "");
    lsef = mpGetValues( mpo, mpSUB_SPACECRAFT_LONGITUDE, &v.ssc_lon, "");
    if (conv_lon) v.ssc_lon = 360.-v.ssc_lon;
    lsef = mpGetValues( mpo, mpPLANET_CENTER_LINE, &v.ssc_line, "");
    lsef = mpGetValues( mpo, mpPLANET_CENTER_SAMPLE, &v.ssc_samp, "");
    lsef = mpGetValues( mpo, mpSPACECRAFT_DISTANCE, &v.range, "");
    lsef = mpGetValues( mpo, mpOPT_AXIS_INTERCEPT_LINE, &v.oaline, "");
    lsef = mpGetValues( mpo, mpOPT_AXIS_INTERCEPT_SAMPLE, &v.oasamp, "");
    lsef = mpGetValues( mpo, mpFOCAL_LENGTH, &v.focal, "");
    lsef = mpGetValues( mpo, mpFOCAL_PLANE_SCALE, &v.cscale, "");
    lsef = mpGetValues( mpo, mpNORTH_ANGLE, &v.rotation, "");
    if ((v.cscale*v.focal)==0.0) 
      v.scale = 0.0;
    else
      v.scale = (v.range-v.radii[0])/(v.cscale*v.focal);
    /* don't add 1 because the MP items are NOT relative to (1,1) -- in
     * contrast to the label items! */
    /*v.ssc_line++;
    v.ssc_samp++;
    v.oaline++;
    v.oasamp++;*/
  }
  else {
    lsef = mpGetValues( mpo, mpMAP_SCALE, &v.scale, "");
    lsef = mpGetValues( mpo, mpCENTER_LATITUDE, &v.center_lat, "");
    lsef = mpGetValues( mpo, mpCENTER_LONGITUDE, &v.center_lon, "");
    if (conv_lon) v.center_lon = 360.-v.center_lon;
    lsef = mpGetValues( mpo, mpLINE_PROJECTION_OFFSET, &v.center_line, "");
    lsef = mpGetValues( mpo, mpSAMPLE_PROJECTION_OFFSET, &v.center_samp, "");
    /* add 1, because these MP items are relative to (1,1) ... will get
     * subtracted out again when we write the ISIS item, but at least we
     * are consistent! */
    v.center_line++;
    v.center_samp++;

    if (!strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC") ||
        !strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC") ||
        !strcmp(v.projection,"OBLIQUE_SINUSOIDAL")) {
      lsef = mpGetValues( mpo, mpCARTESIAN_AZIMUTH, &v.rotation, "");
    }
    else if (!strcmp(v.projection,"LAMBERT_CONFORMAL"))
      lsef = mpGetValues( mpo, mpFIRST_STANDARD_PARALLEL, &v.projitem1, "");
      lsef = mpGetValues( mpo, mpSECOND_STANDARD_PARALLEL, &v.projitem2, "");
  }

endmp:

  /* continue with cube label ... */
  i = numoffiles - cubefiles;

  /* Instrument Mode: */  
  vstat = zlget(inunit[i],"HISTORY",INS_MODE,v.nimsmode,"HIST",tasknam,NULL);
  if (vstat < 0) zmabend(" Instrument mode missing from cube label!");
  for( x=0; x<strlen(v.nimsmode); x++ )
    if( v.nimsmode[x] == ' ' || v.nimsmode[x] == '/' ) v.nimsmode[x] = '_';
  if( v.nimsmode[x-1] == 'T' ) strcat(v.nimsmode,"ROMETER");

  /* read in the band mask */
  if (mphase) {
    vstat = zlget(inunit[i],"HISTORY",B_MASK,bmask,"NRET",&nbm,"NELEMENT",-1,
    "HIST",tasknam,NULL);
    if (vstat != 1) nbm = -1;
  }
  else nbm = -1;

  vstat = zlget(inunit[i],"HISTORY",WAVELENGTHS,b.waves,"NELEMENT",-1,
  "HIST",tasknam,NULL);
  if( vstat != 1 ) zmabend(" Wavelengths missing from cube label!");

  /* disable this, since Sun version of nimscmm2 doesn't use PRODID ...
  vstat = zlget(inunit[i],"HISTORY",PRODID,v.prod_id,"HIST",tasknam,NULL);
  fill_if_error(v.prod_id,"String");
  */
  /* fix this for IOF case, since NIMSR2IOF doesn't update the label */
  /*
  if (iofcub) {
    for (x=0; x<strlen(v.prod_id)-3; x++) 
      if (!strncmp(&v.prod_id[x],".QUB",4)) strcpy(&v.prod_id[x],".IOF");
    for (x=0; x<strlen(v.prod_id)-4; x++) 
      if (!strncmp(&v.prod_id[x],".VTUB",5)) strcpy(&v.prod_id[x],".VIOF");
  }
  */

  vstat = zlget(inunit[i],"HISTORY",OBSNOTE,v.obsnote,"HIST",tasknam,NULL);
  fill_if_error(v.obsnote,"String");

  vstat = zlget(inunit[i],"HISTORY",SUPPNOTE,v.suppnote,"HIST",tasknam,NULL);
  fill_if_error(v.suppnote,"String");

  vstat = zlget(inunit[i],"HISTORY",PRODNOTE,v.prodnote,"HIST",tasknam,NULL);
  fill_if_error(v.prodnote,"String");

  vstat = zlget(inunit[i],"HISTORY",OBSNAME,v.obsname,"HIST",tasknam,NULL);
  fill_if_error(v.obsname,"String");
  /* ensure that string is zero-terminated: */
  if (mphase) v.obsname[12] = 0; 

  if (mphase) {
    vstat = zlget(inunit[i],"HISTORY",OBSEXT,v.obsext,"HIST",tasknam,NULL);
    fill_if_error(v.obsext,"String"); 
  }

  vstat = zlget(inunit[i],"HISTORY",MOSNUM,v.mosnum,"HIST",tasknam,NULL);
  fill_if_error(v.mosnum,"String"); 

  vstat = zlget(inunit[i],"HISTORY",PHASE,v.phase_name,"HIST",tasknam,NULL);
  fill_if_error(v.phase_name,"String"); 

  vstat = zlget(inunit[i],"HISTORY",PHOT_FUNC,v.photofunc,"HIST",tasknam,NULL);
  fill_if_error(v.photofunc,"String"); 
  if( v.photofunc[0] == 'N' )
    strcpy( v.photofunc,"NONE");
  else {
    vstat = zlget(inunit[i],"HISTORY",PHOT_CUT,&v.phot_cut,"HIST",tasknam,NULL);
    if( v.photofunc[0]=='L' && v.photofunc[1]=='A' ) {
      strcpy( v.photofunc,"LAMBERT");
    }
    else if( v.photofunc[0]=='L' && v.photofunc[1]=='O' ) {
      strcpy( v.photofunc,"\'LOMMEL-SEELIGER\'");
    }
    else if( v.photofunc[0] == 'M' ) {
      strcpy( v.photofunc,"MINNAERT");
      vstat = zlget(inunit[i],"HISTORY",MINN_EXP,&v.minn_exp,"HIST",tasknam,NULL);
    }
  }

  if (v.radii[0]==v.radii[2] || v.radii[0]<0.0 ) pgraphic = -1;
  else {
    vstat = zlget(inunit[i],"HISTORY",LAT_TYPE,lattyp,"HIST",tasknam,NULL);
    if (!strcmp(lattyp,"PLANETOGRAPHIC")) pgraphic = 1;
    else pgraphic = 0;
    if (!mphase) {	/* temporary warning until NIMSCMM is fixed ... */
      zvmessage(" Warning:  VISIS2 treatment of latitudes may be inconsistent with NIMSCMM!", "");
    }
  }

  if (mphase) {
    vstat = zlget(inunit[i],"HISTORY",AACS_FILE,v.aacs_file,"HIST",tasknam,NULL);
    if (vstat<0) {
      strcpy(v.aacs_file,"");
      vstat = zlget(inunit[i],"HISTORY",PFM_CK,v.pfm_ck,"HIST",tasknam,NULL);
      fill_if_error(v.pfm_ck,"Literal");
      if (!calib && vstat<0)
	zmabend(" Cube label must have either AACS file or PCK");
      vstat = zlget(inunit[i],"HISTORY",ROT_CK,v.rot_ck,"HIST",tasknam,NULL);
      fill_if_error(v.rot_ck,"Literal");
    }
  }
  else
    vstat = zlget(inunit[i],"HISTORY",POINT_SOURCE,v.pfm_ck,"HIST",tasknam,NULL);

  vstat = zlget(inunit[i],"HISTORY",DPOINT,v.dpoint,"HIST",tasknam,
   "NELEMENT",-1,NULL);
  v.dpstat = (vstat>0);

  wamp = 0.0;
  vstat = zlget(inunit[i],"HISTORY",WAMP,&wamp,"HIST",tasknam,NULL);
  if (wamp>1.e-7) {
    vstat = zlget(inunit[i],"HISTORY",WFREQ,&wfreq,"HIST",tasknam,NULL);
    vstat = zlget(inunit[i],"HISTORY",WPHASE,&wphase,"HIST",tasknam,NULL);
    vstat = zlget(inunit[i],"HISTORY",WCONE,&wcone,"HIST",tasknam,NULL);
    if (vstat<=0) wcone = -999.;
  }

  vstat = zlget(inunit[i],"HISTORY",SC_GAPS,sclk_gaps,"HIST",tasknam,
   "NELEMENT",-1,"NRET",&nsc_gaps,NULL);

  vstat = zlget(inunit[i],"HISTORY",SLEW_TOL,&v.slewtol,"HIST",tasknam,NULL);
  vstat = zlget(inunit[i],"HISTORY",SLEW_RATE,&v.slew_rate,"HIST",tasknam,NULL);

  vstat = zlget(inunit[i],"HISTORY",EQRAD_FUDG,&v.eradf,"HIST",tasknam,NULL);
  if (vstat<=0) v.eradf = -1.;

  vstat = zlget(inunit[i],"HISTORY",SATTHRSH,&v.satthrsh,"HIST",tasknam,NULL);
  if (vstat<=0) v.satthrsh = -1.;
 
  vstat = zlget(inunit[i],"HISTORY",CAL_TYPE,v.dataformat,"HIST",tasknam,NULL);

  /* NIMSCMM only writes "NOCAL" or "RAD" for CAL_TYPE ... however, others
   * can appear due to ISIS processing and VISIS inverse mode;  also,
   * NIMSR2IOF changes the data type without updating the labels */

  if (!strcmp(v.dataformat,"RAD") && !iofcub) {
    strcpy( v.dataformat, "SPECTRAL_RADIANCE");
    strcpy( v.dataunits,  "'uWATT*CM**-2*SR**-1*uM**-1'");
  }
  else if (!strcmp(v.dataformat,"RFACT") || iofcub) {
    strcpy( v.dataformat, "RADIANCE_FACTOR");
    strcpy( v.dataunits,  "DIMENSIONLESS");
  }
  else if (!strcmp(v.dataformat,"NOCAL")) {
    strcpy( v.dataformat, "RAW_DATA_NUMBER");
    strcpy( v.dataunits,  "DIMENSIONLESS");
  }
  else zmabend(" *** unknown calibration type! ***");

  vstat = zlget(inunit[i],"HISTORY",BINNING,v.binning,"HIST",tasknam,NULL);
  fill_if_error(v.binning,"Literal");
  if (!strcmp(v.binning,"FOOTPRNT"))
    vstat = zlget( inunit[i], "HISTORY", THRESH, &v.thresh,  "HIST", tasknam,
     "NELEMENT",-1,NULL);
  if (v.binning[0]=='F') {
    v.maxdistor = 0.0;
    vstat = zlget( inunit[i], "HISTORY", MAXDISTOR, &v.maxdistor, "HIST", 
     tasknam, "NELEMENT",-1,NULL);
    vstat = zlget( inunit[i], "HISTORY", FPGRID, &v.fpgrid,  "HIST", tasknam,
     "NELEMENT",-1,NULL);
  }

  /* this call verifies that the dimension of B_MASK agrees with the actual
   * number of bands for the mode;  it also determines whether a tube is
   * a P-tube (for which v.binning is needed): */
  determine_band_bin_parms();

  vstat = zlget( inunit[i], "HISTORY", EDRS, v.edrfiles,  "HIST", tasknam,
   "ULEN", 101, "NELEMENT", -1, "NRET", &nedrs,NULL);
  fill_if_error(v.edrfiles,"S");

  vstat = zlget(inunit[i],"HISTORY",IKERNEL,v.ikernel,"HIST",tasknam,NULL);
  fill_if_error(v.ikernel,"S");

  vstat = zlget(inunit[i],"HISTORY",SP_KERNEL,v.spkernel,"HIST",tasknam,NULL);
  fill_if_error(v.spkernel,"S");
  x = strlen(v.spkernel);
  if (x<=1) {
    vstat = zlget(inunit[i],"HISTORY",SK_ID,v.spkernel,"HIST",tasknam,NULL);
    fill_if_error(v.spkernel,"S");
  }

  vstat = zlget(inunit[i],"HISTORY",CAL_FILE,v.calfile,"HIST",tasknam,
   "ULEN", 100, "NELEMENT", -1, "NRET", &ncals, NULL);
  fill_if_error(v.calfile,"S");

  vstat = zlget(inunit[i],"HISTORY",CAL_TYPE,v.caltype,"HIST",tasknam,NULL);
  fill_if_error(v.caltype,"Literal");

  vstat = zlget(inunit[i],"HISTORY",DARK_FILE,v.darkfile,"HIST",tasknam,
   "ULEN", 100, "NELEMENT", -1, "NRET", &ndrks, NULL);
  fill_if_error(v.darkfile,"S");

  vstat = zlget(inunit[i],"HISTORY",DARK_TYPE,v.darktype,"HIST",tasknam,NULL);
  fill_if_error(v.darktype,"Literal");

  vstat = zlget(inunit[i],"HISTORY",SOL_FILE,v.sol_file,"HIST",tasknam,NULL);
  fill_if_error(v.sol_file,"S");

  vstat = zlget(inunit[i],"HISTORY",DEBOOM_FILE,v.deboomfile,"HIST",tasknam,NULL);
  fill_if_error(v.deboomfile,"S");

  vstat = zlget(inunit[i],"HISTORY",DESPIKE_FILE,v.despikefile,
   "HIST",tasknam,NULL);
  fill_if_error(v.despikefile,"S");

  vstat = zlget(inunit[i],"HISTORY",STOP_SLIDE,v.stop_slide,"HIST",tasknam,NULL);
  fill_if_error(v.stop_slide,"Literal");

  vstat = zlget(inunit[i],"HISTORY",GAIN_STATE,&v.gain_state,"NRET",&cnt,
   "HIST",tasknam,"NELEMENT",-1,NULL);
  if (vstat<=0) {
    cnt = 1;
    v.gain_state[0] = 0;
  }
  if (cnt>ncals) {
    zvmessage(" too many Gains in label!  Truncated to no. of Cal files","");
  }
  else if (cnt<ncals) {
    zvmessage(" not enough Gains in label!  Padded to no. of Cal files","");
    for (x=cnt; x<ncals; x++) v.gain_state[x] = v.gain_state[cnt-1];
  }

  vstat = zlget(inunit[i],"HISTORY",CHOPPER_MODE,&v.chopper,"HIST",tasknam,NULL);
  zero_if_error(&v.chopper);

  vstat = zlget(inunit[i],"HISTORY",SOLAR_FLUX,v.solarflux,"HIST",tasknam,
   "NELEMENT",-1,NULL);

  vstat = zlget(inunit[i],"HISTORY",RAD_SENS,v.radsens,"NRET",&v.sens_flg,
   "HIST",tasknam,"NELEMENT",-1,NULL);

  vstat = zlget(inunit[i],"HISTORY",DRK_AVE,v.drkave,"NRET",&v.drk_flg,
   "HIST",tasknam,"NELEMENT",-1,NULL);

  vstat = zlget(inunit[i],"HISTORY",RAD_CONV,v.radiance,"HIST",tasknam,"NRET",
   &nret,"NELEMENT",-1,NULL);
  if (vstat==1) 
    /* fill the array if the # values is less than # wavelengths */
    for (x=nret; x<nbb; x++)
      v.radiance[x] = v.radiance[x-1];

  vstat = zlget(inunit[i],"HISTORY",RAD_BASE,v.rad_base,"HIST",tasknam,"NRET",
   &nret,"NELEMENT",-1,NULL);
  if (vstat==1) 
    /* same as for RAD_CONV */
    for (x=nret; x<nbb; x++)
      v.rad_base[x] = v.rad_base[x-1];

  vstat = zlget(inunit[i],"HISTORY",MAX_RANGE,&v.max_range,"HIST",tasknam,NULL);
  zero_if_error(&v.max_range);

  vstat = zlget(inunit[i],"HISTORY",MIN_RANGE,&v.min_range,"HIST",tasknam,NULL);
  zero_if_error(&v.min_range);

  vstat = zlget(inunit[i],"HISTORY",MAX_LAT,&v.max_lat,"HIST",tasknam,NULL);
  zero_if_error(&v.max_lat);

  vstat = zlget(inunit[i],"HISTORY",MAX_LON,&v.max_lon,"HIST",tasknam,NULL);
  zero_if_error(&v.max_lon);
  if (conv_lon) v.max_lon = 360.-v.max_lon;

  vstat = zlget(inunit[i],"HISTORY",MIN_LAT,&v.min_lat,"HIST",tasknam,NULL);
  zero_if_error(&v.min_lat);

  vstat = zlget(inunit[i],"HISTORY",MIN_LON,&v.min_lon,"HIST",tasknam,NULL);
  zero_if_error(&v.min_lon);
  /* this item can be negative, so just invert -- conversion to (0,360)
   * range will be handled when PDS label item is written */
  if (conv_lon) v.min_lon = 0.0-v.min_lon;

  vstat = zlget(inunit[i],"HISTORY",MAX_SUN_D,&v.max_sun_d,"HIST",tasknam,NULL);
  zero_if_error(&v.max_sun_d);

  vstat = zlget(inunit[i],"HISTORY",MIN_SUN_D,&v.min_sun_d,"HIST",tasknam,NULL);
  zero_if_error(&v.min_sun_d);

  vstat = zlget(inunit[i],"HISTORY",MAX_CB_D,&v.max_cb_d,"HIST",tasknam,NULL);
  zero_if_error(&v.max_cb_d);

  vstat = zlget(inunit[i],"HISTORY",MIN_CB_D,&v.min_cb_d,"HIST",tasknam,NULL);
  zero_if_error(&v.min_cb_d);

  /* Calculate slant_dist_mult & slant_dist_base  */
  v.slant_dist_mult = (v.max_range - v.min_range)/(32767+32752);
  v.slant_dist_base = v.max_range - (v.slant_dist_mult*32767);

  vstat = zlget(inunit[i],"HISTORY",FILL_SIZE,&v.fillsize,"HIST",tasknam,NULL);
  zero_if_error(&v.fillsize);

  vstat = zlget(inunit[i],"HISTORY",FILL_NUM,&v.fillnum,"HIST",tasknam,NULL);
  zero_if_error(&v.fillnum);

  vstat = zlget(inunit[i],"HISTORY",DN_STD_DEV,&v.sdband,"HIST",tasknam,NULL);
  zero_if_error(&v.sdband);

  vstat = zlget(inunit[i],"HISTORY",GEO_STD_DEV,&v.sdgeo,"HIST",tasknam,NULL);
  zero_if_error(&v.sdgeo);

  vstat = zlget(inunit[i],"HISTORY",THRESHVAL,v.threshval,"NELEMENT",-1,
  "HIST",tasknam,NULL);
  ithreshval = (vstat==1);

  vstat = zlget(inunit[i],"HISTORY",PSHIFT,&v.pshift,"HIST",tasknam,NULL);
  zero_if_error(&v.pshift);

  vstat = zlget(inunit[i],"HISTORY",AINFL,&v.ainfl,"HIST",tasknam,NULL);
  zero_if_error(&v.ainfl);

  vstat = zlget(inunit[i],"HISTORY",GRATING_START,&v.grating_start_pos,
  "HIST",tasknam,NULL);
  zero_if_error(&v.grating_start_pos);

  vstat = zlget(inunit[i],"HISTORY","B_E_G_P_1",&v.bandedge_gp1,
  "HIST",tasknam,NULL);
  zero_if_error(&v.bandedge_gp1);

  vstat = zlget(inunit[i],"HISTORY","B_E_G_P_2",&v.bandedge_gp2,
  "HIST",tasknam,NULL);
  zero_if_error(&v.bandedge_gp2);

  vstat = zlget(inunit[i],"HISTORY",GRATING_OFFSET,&v.grating_offset,
  "HIST",tasknam,NULL);
  zero_if_error(&v.grating_offset);

  vstat = zlget(inunit[i],"HISTORY",GRATING_DELTA,&v.grating_delta,
  "HIST",tasknam,NULL);
  zero_if_error(&v.grating_delta);

  vstat = zlget(inunit[i],"HISTORY",GRATING_STEPS,&v.grating_steps,
  "HIST",tasknam,NULL);
  zero_if_error(&v.grating_steps);

  vstat = zlget(inunit[i],"HISTORY",SUN_AZI,&v.azimuth_sun,"HIST",tasknam,NULL);
  zero_if_error(&v.azimuth_sun);
  if (v.azimuth_sun<0.0) v.azimuth_sun += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit[i],"HISTORY",SC_AZI,&v.azimuth_sc,"HIST",tasknam,NULL);
  zero_if_error(&v.azimuth_sc);
  if (v.azimuth_sc<0.0) v.azimuth_sc += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit[i],"HISTORY",B_SSCLAT,&v.b_ssc_lat,"HIST",tasknam,NULL);
  zero_if_error(&v.b_ssc_lat);

  vstat = zlget(inunit[i],"HISTORY",B_SSCLON,&v.b_ssc_lon,"HIST",tasknam,NULL);
  zero_if_error(&v.b_ssc_lon);
  if (conv_lon) v.b_ssc_lon = 360.-v.b_ssc_lon;
  if (v.b_ssc_lon<0.0) v.b_ssc_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit[i],"HISTORY",E_SSCLAT,&v.e_ssc_lat,"HIST",tasknam,NULL);
  zero_if_error(&v.e_ssc_lat);

  vstat = zlget(inunit[i],"HISTORY",E_SSCLON,&v.e_ssc_lon,"HIST",tasknam,NULL);
  zero_if_error(&v.e_ssc_lon);
  if (conv_lon) v.e_ssc_lon = 360.-v.e_ssc_lon;
  if (v.e_ssc_lon<0.0) v.e_ssc_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit[i],"HISTORY",B_SSOLLAT,&v.b_ssl_lat,"HIST",tasknam,NULL);
  zero_if_error(&v.b_ssl_lat);

  vstat = zlget(inunit[i],"HISTORY",B_SSOLLON,&v.b_ssl_lon,"HIST",tasknam,NULL);
  zero_if_error(&v.b_ssl_lon);
  if (v.b_ssl_lon<0.0) v.b_ssl_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit[i],"HISTORY",E_SSOLLAT,&v.e_ssl_lat,"HIST",tasknam,NULL);
  zero_if_error(&v.e_ssl_lat);

  vstat = zlget(inunit[i],"HISTORY",E_SSOLLON,&v.e_ssl_lon,"HIST",tasknam,NULL);
  zero_if_error(&v.e_ssl_lon);
  if (v.e_ssl_lon<0.0) v.e_ssl_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit[i],"HISTORY",INCI_ANG,&v.inci_angle,"HIST",tasknam,NULL);
  zero_if_error(&v.inci_angle);

  vstat = zlget(inunit[i],"HISTORY",PHAS_ANG,&v.phas_angle,"HIST",tasknam,NULL);
  zero_if_error(&v.phas_angle);

  vstat = zlget(inunit[i],"HISTORY",EMIS_ANG,&v.emis_angle,"HIST",tasknam,NULL);
  zero_if_error(&v.emis_angle);

  vstat = zlget(inunit[i],"HISTORY",T_FOCAL,&v.temp[0],"HIST",tasknam,NULL);
  zero_if_error(&v.temp[0]);

  vstat = zlget(inunit[i],"HISTORY",T_RAD_SHIELD,&v.temp[1],"HIST",tasknam,NULL);
  zero_if_error(&v.temp[1]);

  vstat = zlget(inunit[i],"HISTORY",T_TELESCOPE,&v.temp[2],"HIST",tasknam,NULL);
  zero_if_error(&v.temp[2]);

  vstat = zlget(inunit[i],"HISTORY",T_GRATING,&v.temp[3],"HIST",tasknam,NULL);
  zero_if_error(&v.temp[3]);

  vstat = zlget(inunit[i],"HISTORY",T_CHOPPER,&v.temp[4],"HIST",tasknam,NULL);
  zero_if_error(&v.temp[4]);

  vstat = zlget(inunit[i],"HISTORY",T_ELECTRONICS,&v.temp[5],
  "HIST",tasknam,NULL);
  zero_if_error(&v.temp[5]);

  vstat = zlget(inunit[i],"HISTORY",BEG_SCET,tstr,"HIST",tasknam,NULL);
  fill_if_error(tstr,"N");      /* event time         */
  if( tstr[0] != 'x' ) {
    /*for( j=0; j<25; j++) v.event_start_time[j] = tstr[j];*/
    /* this code fails for VEVPDIN1 cube ... disable it [2006jun09]: /*
    /* 13nov2011:  it works in VISISX an is PDS std, so re-enable it: */
    for( j=0; j<25; j++) v.event_start_time[j] = 0;		/* initialize */
    for( j=0; j<4; j++) v.event_start_time[j] = tstr[j];	/* Year */
    strcat( v.event_start_time,"-");
    determine_month( monthstr, &tstr[5] );
    strcat( v.event_start_time,monthstr);
    strcat( v.event_start_time,"-");
    v.event_start_time[8] = tstr[9];
    v.event_start_time[9] = tstr[10];
    strcat( v.event_start_time,"T");
    strcat( v.event_start_time,&tstr[12]);
    v.event_start_time[13] = ':';
    v.event_start_time[16] = ':';
    v.event_start_time[19] = 'Z';
  }

  vstat = zlget(inunit[i],"HISTORY",END_SCET,tstr,"HIST",tasknam,NULL);
  fill_if_error(tstr,"N");      /* event time         */
  if( tstr[0] != 'x' ) {
    /*for( j=0; j<25; j++) v.event_stop_time[j] = tstr[j];*/
    for( j=0; j<25; j++) v.event_stop_time[j] = 0;
    for( j=0; j<4; j++) v.event_stop_time[j] = tstr[j];
    determine_month( monthstr, &tstr[5] );
    strcat( v.event_stop_time,"-");
    strcat( v.event_stop_time,monthstr);
    strcat( v.event_stop_time,"-");
    v.event_stop_time[8] = tstr[9];
    v.event_stop_time[9] = tstr[10];
    strcat( v.event_stop_time,"T");
    strcat( v.event_stop_time,&tstr[12]);
    v.event_stop_time[13] = ':';
    v.event_stop_time[16] = ':';
    v.event_stop_time[19] = 'Z';
  }

  vstat = zlget(inunit[i],"HISTORY",BEG_SCLK,integers,"HIST",tasknam,
      "NELEMENT",3,NULL);
  if( vstat == 1 ) {
    v.native_times[0][0] = integers[0];
    v.native_times[0][1] = integers[1];
    v.native_times[0][2] = integers[2];
  }
  else {
    v.native_times[0][0] = 0;
    v.native_times[0][1] = 0;
    v.native_times[0][2] = 0;
  }

  vstat = zlget(inunit[i],"HISTORY",END_SCLK,integers,"HIST",tasknam,
      "NELEMENT",2,NULL);
  if( vstat == 1 ) {
    v.native_times[1][0] = integers[0];
    v.native_times[1][1] = integers[1];
  }
  else {
    v.native_times[1][0] = 0;
    v.native_times[1][1] = 0;
  }

  /* abbreviated target code for DATA_SET_ID: */
  if( strcmp(v.target,"VENUS")==0 )
    strcpy(v.targetcode,"V");
  else if( strcmp(v.target,"EARTH")==0 )
    strcpy(v.targetcode,"E");
  else if( strcmp(v.target,"MOON")==0 )
    strcpy(v.targetcode,"L");
  else if( strcmp(v.target,"JUPITER")==0 )
    strcpy(v.targetcode,"J");
  else if( strcmp(v.target,"IO")==0 || strcmp(v.target,"CALLISTO")==0 || 
           strcmp(v.target,"EUROPA")==0 || strcmp(v.target,"THEBE")==0 ||
           strcmp(v.target,"ADRASTEA")==0 || strcmp(v.target,"AMALTHEA")==0 ||
           strcmp(v.target,"LEDA")==0 || strcmp(v.target,"GANYMEDE")==0 ||
           strcmp(v.target,"HIMALIA")==0 || strcmp(v.target,"CARME")==0 ||
           strcmp(v.target,"LYSITHEA")==0 || strcmp(v.target,"ANANKE")==0 ||
           strcmp(v.target,"PASIPHAE")==0 || strcmp(v.target,"SINOPE")==0 ||
           strcmp(v.target,"METIS")==0 || strcmp(v.target,"ELARA")==0 )
    strcpy(v.targetcode,"J");
    /*strcpy(v.targetcode,"JSA");  /* JSA has been disapproved */
  else if( strcmp(v.target,"GASPRA")==0 || strcmp(v.target,"IDA")==0 )
    strcpy(v.targetcode,"A");
  else if( strcmp(v.target,"RING")==0 )
    strcpy(v.targetcode,"J");
    /*strcpy(v.targetcode,"JR");*/
  else    
    strcpy(v.targetcode,"J");  /* includes JR & JSA & CAL */

  zvclose(inunit[i],NULL);
}


/****************************************************************************/
int FUNCTION get_real_value(realitem,buf,labptr)
/*
 * Get a fixed-point or floating point value from an ASCII string
 */
float	*realitem;				/* Real value pointer        */
int	*labptr;				/* Label element pointer     */
char	buf[];					/* PDS label buffer   	     */
{
  char	number[33];
  int	a,b;

  memset( number, 0, 32 );

  /* Find first digit/sign */
  while (!isdigit(buf[*labptr]) && buf[*labptr] != '+' && buf[*labptr] != '-') {
    lsef = incrm_check_buffer(labptr);
    if( lsef == 0 ) return 0;
  }

  /* read the number into buffer: */
  a=0;
  while (isdigit(buf[*labptr]) || buf[*labptr]=='-' || buf[*labptr]=='+' 
   || buf[*labptr]=='.' || buf[*labptr]=='e' || buf[*labptr]=='E' ) {
    number[a++] = buf[*labptr];
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0 || a>=32) return 0;
  }
  (*realitem) = (float)atof(number);
  return 1;
}


/****************************************************************************/
int FUNCTION get_double(doubitem,buf,labptr)
/*
 * Get a double-precision fixed-point or floating point value from an ASCII
 * string
 */
double	*doubitem;				/* Real value pointer        */
int	*labptr;				/* Label element pointer     */
char	buf[];					/* PDS label buffer   	     */
{
  char	number[33];
  int	a,b;

  memset( number, 0, 32 );

  /* Find first digit/sign */
  while (!isdigit(buf[*labptr]) && buf[*labptr] != '+' && buf[*labptr] != '-') {
    lsef = incrm_check_buffer(labptr);
    if( lsef == 0 ) return 0;
  }

  /* read the number into buffer: */
  a=0;
  while (isdigit(buf[*labptr]) || buf[*labptr]=='-' || buf[*labptr]=='+' 
   || buf[*labptr]=='.' || buf[*labptr]=='e' || buf[*labptr]=='E' ) {
    number[a++] = buf[*labptr];
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0 || a>=32) return 0;
  }
  (*doubitem) = (double)atof(number);
  return 1;
}


/****************************************************************************/
FUNCTION get_recinfo(nlabs,nhrecs,nrecs)
/*
 * Get pointer to object files and dimension of cube.
 * Also reads the ISIS cube label and history objects into
 * memory buffers.
 */
int	*nlabs,					/* Number of label records   */
	*nhrecs,				/* Number of history records */
	*nrecs;					/* Number of file records    */
{
  char 	buffer[BUFFERSIZE];			/* First 512 characters      */
						/* of the PDS label	     */
  int 	x,y,z;					/* Loop control variables    */
  int	labptr, lptr, lptr1;			/* Pointers to label element */
  int 	object, index, nbyt, nsidp, nbotp;
  int	nsi,nbp;				/* Number of spectral index  */
						/* images and backplanes     */
  object = 0;

  /* Open ISIS cube file */
  vstat = zvopen( inunit[object], "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "READ",
   "COND", "NOLABELS", "U_NL", 1, "U_NS", BUFFERSIZE, NULL);

  /* Read first 512 bytes of the PDS label */
  zvread(inunit[object],buffer,"NSAMPS",BUFFERSIZE,NULL);
  numofbytes = BUFFERSIZE;			/* Buffer has BUFFERSIZE     */
						/* number of samples.        */
  labptr = 0;					/* Point to first character  */
  labend = 9999999;

  find_keyword("FILE_RECORDS",buffer,&labptr,&labend);
  get_integer_value(nrecs,buffer,&labptr);	/* Get FILE_RECORDS value    */

  find_keyword("LABEL_RECORDS",buffer,&labptr,&labend);
  get_integer_value(nlabs,buffer,&labptr);	/* Get LABEL_RECORDS value   */

  zvclose(inunit[object],NULL);

  /* Reopen input file with the ADDRESS option */
  vstat = zvopen(inunit[object],"OPEN_ACT","SA","IO_ACT","SA","OP","READ",
   "COND","NOLABELS","ADDRESS",&inptr,"U_NL",*nlabs,"U_NS",BUFFERSIZE,NULL);

	/* first allocate enough buffer to hold label -- then
	 * search for end of history object and re-allocate 
	 * enough to hold both */

  numofbytes = (*nlabs) * BUFFERSIZE;	/* # bytes in PDS label object */
  bufpoint = (char *)calloc(numofbytes,sizeof(char));	
  if( bufpoint == NULL ) zmabend(" *** Memory allocation error - label ***");

  zmve( 1, numofbytes, inptr, bufpoint, 1, 1);

  labptr = index = 0;
  labend = numofbytes;
						/* Get 2D HISTOGRAM dimen.   */
  vstat = find_keyword("^HISTOGRAM_IMAGE",bufpoint,&labptr,&labend);
  get_integer_value(nhrecs,bufpoint,&labptr);
  objptr[index++] = *nhrecs;

  *nhrecs -= 1;
  free( bufpoint );				/* Deallocate memory */
  numofbytes = (*nhrecs) * BUFFERSIZE;	/* # bytes in label + history objects */
  bufpoint = (char *)calloc(numofbytes,sizeof(char));	
  if( bufpoint == NULL ) zmabend(" *** Memory allocation error - history ***");
  zmve( 1, numofbytes, inptr, bufpoint, 1, 1);
  *nhrecs -= *nlabs;				/* save # history rec's only */

  labend = numofbytes;

  vstat = find_keyword("LINES",bufpoint,&labptr,&labend);
  get_integer_value(&inl[object],bufpoint,&labptr);
  vstat = find_keyword("LINE_SAMPLES",bufpoint,&labptr,&labend);
  get_integer_value(&ns[object],bufpoint,&labptr);
  nb[object] = 1;
  bytes[object++] = 1;

  vstat = find_keyword("^SAMPLE_SPECTRUM_QUBE",bufpoint,&labptr,&labend);
  if( vstat == 1 ) {
    get_integer_value(&objptr[index++],bufpoint,&labptr);

    /* Get spectra dimensions */
    vstat = find_keyword("ITEMS",bufpoint,&labptr,&labend); 
    get_integer_value(&ns[object],bufpoint,&labptr);
    get_integer_value(&inl[object],bufpoint,&labptr);
    get_integer_value(&nb[object],bufpoint,&labptr);
    specplots = nb[object];
    bytes[object] = 0.5;        /* ??? 'bytes' is integer!! TBD */
    for(object=2;object<=specplots;object++) {
      ns[object] = ns[1];
      inl[object] = inl[1];
      nb[object] = 1;
    }
  }
  else
    labptr = 0;

  find_keyword("^QUBE",bufpoint,&labptr,&labend);
  get_integer_value(&objptr[index++],bufpoint,&labptr);
  cubefiles = 1;

  find_keyword("CORE_ITEMS",bufpoint,&labptr,&labend);
  get_integer_value(&ns[object],bufpoint,&labptr);	/* Get dim of QUBE   */
  get_integer_value(&inl[object],bufpoint,&labptr);
  get_integer_value(&nb[object],bufpoint,&labptr);

  /* determine whether cube is a tube: */
  lptr = labptr;
  find_keyword( "DATA_SET_ID", bufpoint, &lptr, &labend);
  tube = 0;
  for (x=0; x<5; x++) {
    if (!strncmp("TUBE",&bufpoint[lptr+16+x],4)) {
      tube = 1;
      break;
    }
  }

  find_keyword("CORE_ITEM_BYTES",bufpoint,&labptr,&labend);
  get_integer_value(&nbyt,bufpoint,&labptr);
  if (nbyt!=2 && nbyt!=4) zmabend(" *** ILLEGAL CUBE FORMAT ***");
  bytes[object++] = nbyt;

  /* Number of bytes in backplanes. */
  find_keyword("SUFFIX_BYTES",bufpoint,&labptr,&labend);
  get_integer_value(&bytes[object],bufpoint,&labptr);

  find_keyword("SUFFIX_ITEMS",bufpoint,&labptr,&labend);
  get_integer_value(&nsidp,bufpoint,&labptr);		/* sideplanes */
  get_integer_value(&nbotp,bufpoint,&labptr);		/* bottomplanes */
  if (nsidp>0 || nbotp>0) 
    zmabend("QUBE contains side or bottom planes -- not implemented in VISIS2");
  get_integer_value(&nb[object],bufpoint,&labptr);	/* backplanes */

  nbp = nb[object];
  if (!tube) nb[object] = GEOBANDS;
  ns[object] = ns[object-1];
  inl[object] = inl[object-1];

  cubefiles++;

  /* Determine if there is an SII in cube: */

  if (!tube && nbp>GEOBANDS) {
    nb[++object]  = nbp - GEOBANDS;
    ns[object]    = ns[object-1];
    inl[object]    = inl[object-1];
    bytes[object] = bytes[object-1];
    cubefiles++;
  }

  /* Check for invalid number of output files specified by the user */
  if( numoffiles != (specplots+cubefiles+1) ) {
    sprintf(xstring,"%d output files specified; %d files in cube",
     numoffiles,(specplots+cubefiles+1));
    zmabend(xstring);
  }

  zvclose(inunit[0],NULL);
}


/****************************************************************************/
int FUNCTION incrm_check_buffer(a)
/*
 * Increment buffer index and check against maximum number of bytes in 
 * the PDS label
 */
int *a;
{
  (*a)++;
  if (*a < numofbytes)
    return 1;
  else
    return 0;
}	


/****************************************************************************/
FUNCTION ISIStoVICAR()
/*
 * Generate three VICAR format files - 3D Merged Mosaic, 2D histogram 
 * and sample-spectra - from ISIS cube file.
 */
{				/* Local Variables			     */
  int nlabrecs, nhistrecs, nrecs, bytes[MAXNUMOFFILES], nb[MAXNUMOFFILES],
   inl[MAXNUMOFFILES], ns[MAXNUMOFFILES];

  zvmessage("Inverse mode:  VICAR files will be generated from ISIS cube","");
  VICARlabels(&nlabrecs,&nhistrecs,&nrecs);	
  write_object(BACKWARD,nlabrecs,nhistrecs,nrecs);
}


/****************************************************************************/
int FUNCTION keyword_value(unit,pdsitem,type,vicaritem,objectindex)
/*
 * Copy keyword/value pairs of PDS label to VICAR label.
 */
char 	pdsitem[],				  /* PDS item name           */
	type[],				  /* Type of label item: R/I/T/C     */
	vicaritem[];				  /* VICAR item name         */
int	objectindex,			          /* Index for object files  */
	unit;					  /* Unit number of output   */

{					/* Local Variables                   */
int	count,				/* Counter/index		     */
	vlen,
	found,				/* Item found indicator              */
	intitem[MAXBANDS],		/* Array of integer values           */
	itemlength,			/* Item name length 		     */
	labptr,			/* Pointer to label character        */
	nlines;				/* Number of lines in input file     */
float	realitem[MAXBANDS];		/* Array of real values 	     */
char 	objectname[3][25],		/* Array of object file names        */
	prevval,			/* Previous buffer character	     */
	stritem[MAXBANDS][101],
	message[80];

labptr = count = 0;
prevval = 0;
memset( line, 0, 120);
vlen = strlen(vicaritem)-1;

strcpy(objectname[0],"^HISTOGRAM_IMAGE");
strcpy(objectname[1],"^SAMPLE_SPECTRUM_QUBE");
strcpy(objectname[2],"^QUBE");

lsef = find_keyword(objectname[objectindex-1],bufpoint,&labptr,&labend);
if( lsef == 0 ) goto notfound;

lsef = find_keyword(pdsitem,bufpoint,&labptr,&labend);
if( lsef == 0 ) goto notfound;

	/* (need to check specially for "-" sign, else this does
	 * not get passed on to get_real_value:) */
while(bufpoint[labptr]!='(' && bufpoint[labptr]!='\"' &&
  bufpoint[labptr]!='-' && isalnum(bufpoint[labptr])==0)
  {
  lsef = incrm_check_buffer(&labptr);
  if( lsef == 0 ) goto notfound;
  }
switch(bufpoint[labptr])
{
case '(':    /* Array of values - Real, Integer, or String */  
  if (type[0]=='R') {
    while ( bufpoint[ labptr]!=')' ) {
       lsef = get_real_value( &realitem[count++], bufpoint, &labptr);
       if( lsef == 0 ) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, realitem,  "FORMAT", "REAL",
     "NELEMENT", count,NULL);
  }
  else if (type[0]=='I') {
    while ( bufpoint[labptr]!=')' ) {
      lsef = get_integer_value( &intitem[count++], bufpoint, &labptr);
      if( lsef == 0 ) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, intitem,  "FORMAT", "INT",
     "NELEMENT", count,NULL);
  }
  else {	/* default type = 'S' or 'C' <what's the difference??> */
    lsef = incrm_check_buffer(&labptr);	/* step past the '(' */
    if( lsef == 0 ) goto notfound;
    while ( bufpoint[labptr] != ')' ) {
      lsef = get_qstring_value( stritem[count++], 100, &labptr);
      if( lsef == 0 ) goto notfound;
      if ( bufpoint[labptr] != ')' ) 
	lsef = incrm_check_buffer(&labptr);	/* step past terminator */
      if( lsef == 0 ) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, stritem,  "FORMAT", "STRING",
     "NELEMENT", count, "ULEN", 101, NULL);
  }
  break;

case '\"':    /* Value is a line comment 'Now is the time . . .'   */
  lsef = incrm_check_buffer(&labptr);
  if( lsef == 0 ) goto notfound;
  while(bufpoint[labptr]!='\"')
    {
    if(bufpoint[labptr]!='\'')
      line[count++]=bufpoint[labptr];
    lsef = incrm_check_buffer(&labptr);  
    if( lsef == 0 ) goto notfound;
    }
  zladd(unit,"HISTORY",vicaritem,line,"FORMAT","STRING",NULL);
  break;

default:    /* Value is not a line comment or array of values    */

  switch(type[0]) {    /* Check for REAL, INT, TIME, or STRING type  */

  case 'R':
    lsef = get_real_value(&realitem[0],bufpoint,&labptr);
    if( lsef == 0 ) goto notfound;
    zladd(unit,"HISTORY",vicaritem,realitem,"FORMAT","REAL",NULL);
    break;

  case 'I': 
    lsef = get_integer_value(&intitem[0],bufpoint,&labptr);
    if( lsef == 0 ) goto notfound;
    zladd(unit,"HISTORY",vicaritem,intitem,"FORMAT","INT",NULL);
    break;

  case 'T': 
    prevval=' ';
    while(bufpoint[labptr]!='\r' &&
        (bufpoint[labptr]!='*' && prevval!='/')) {
      if(isdigit(bufpoint[labptr])!=FALSE || 
          bufpoint[labptr] == '.') {
	if(count>1 && vicaritem[vlen]!='K' && vicaritem[vlen]!='T')
	  line[count-2]=bufpoint[labptr];
	else
	  line[count]=bufpoint[labptr];
      }
      else {		/* punctuation becomes spaces, Month gets extra 0 */
	if (count==4) {		/* start of Month string */
	  line[count++] = ' ';
	  line[count] = '0';
	}
	else line[count] = ' ';
      }
      count++;
      prevval=bufpoint[labptr];
      lsef = incrm_check_buffer(&labptr);
      if( lsef == 0 ) goto notfound;
    }
    zladd(unit,"HISTORY",vicaritem,line,"FORMAT","STRING",NULL);
    break;

  default:  	/* type = 'S' assumed */
    while (bufpoint[labptr]!='\r' &&
           bufpoint[labptr]!=' '  && bufpoint[labptr]!='*' &&
           bufpoint[labptr]!='\'' && prevval!='/') {
      line[count++]=bufpoint[labptr];
      prevval=bufpoint[labptr];
      lsef = incrm_check_buffer(&labptr);
       if( lsef == 0 ) goto notfound; 
    }
    if(prevval=='/')
      line[count-1]=' ';
    zladd(unit,"HISTORY",vicaritem,line,"FORMAT","STRING",NULL);
    break;
  }
  break;
}
memset( line, 0, 120);
return 1;

notfound:
  sprintf( message, " Keyword %s not found in cube label", pdsitem);
  zvmessage( message,"");
  return 0;
}


/****************************************************************************/
int FUNCTION get_string_value( buf, maxlen, lptr)
/*
 * return the first string in bufpoint following 'lptr'
 *
 * a "string" is defined here as composed of any character outside the
 * set { "\r", "\n", "/", "=", "(", ")", " " } -- the first character
 * encountered that is not in this set begins the string, and the first
 * character thereafter encountered *in* that set ends it;  note that
 * this definition is NOT the same as that used for strings in function
 * keyword_value()!
 */
char *buf;
int maxlen, *lptr;
{
  int i;

  for (i=0; i<maxlen; i++) buf[i] = 0;		/* intialize */

	/* find beginning of string: */
  while (bufpoint[*lptr] == '\r' || bufpoint[*lptr] == '\n' ||
         bufpoint[*lptr] == '/' || bufpoint[*lptr] == '=' ||
         bufpoint[*lptr] == '"' || 
         bufpoint[*lptr] == '(' || bufpoint[*lptr] == ' ' ) {
    lsef = incrm_check_buffer( lptr);
    if( lsef == 0 ) return 0;
  }

  i = 0;
  while (bufpoint[*lptr] != '\r' && bufpoint[*lptr] != '\n' &&
         bufpoint[*lptr] != '/'  && bufpoint[*lptr] != ',' &&
         bufpoint[*lptr] != '"' &&
         bufpoint[*lptr] != ')' && bufpoint[*lptr] != ' ' ) {
    if (i>maxlen) return 0;		/* item is too long for buffer */
    buf[i++] = bufpoint[*lptr];
    lsef = incrm_check_buffer( lptr);
    if (lsef == 0) return 0;
  }
  if (i<2) return 0;		/* string must be > 1 character */
  if (bufpoint[(*lptr)-1] == '/') buf[i-1] = ' ';

  return 1;
}


/****************************************************************************/
int FUNCTION get_qstring_value( buf, maxlen, lptr)
/*
 * return the first string in bufpoint following 'lptr'
 *
 * similar to get_string_value, but the string here is delimited only
 * by quotation marks ("), which are not themselves returned as part
 * of the string
 */
char *buf;
int maxlen, *lptr;
{
  int i;

  for (i=0; i<maxlen; i++) buf[i] = 0;		/* intialize */

	/* find beginning of string: */
  while (bufpoint[*lptr] != '"') {
    lsef = incrm_check_buffer( lptr);
    if( lsef == 0 ) return 0;
  }
  lsef = incrm_check_buffer( lptr);	/* move past the " */
  if( lsef == 0 ) return 0;

  i = 0;
  while (bufpoint[*lptr] != '"') {
    if (i>maxlen) return 0;		/* item is too long for buffer */
    buf[i++] = bufpoint[*lptr];
    lsef = incrm_check_buffer( lptr);
    if (lsef == 0) return 0;
  }
  if (i<2) return 0;		/* string must be > 1 character */
  if (bufpoint[(*lptr)-1] == '/') buf[i-1] = ' ';

  return 1;
}


/****************************************************************************/
FUNCTION process_parms(direction,inputfiles)
/*
 * Process all user parameters and determine direction of transformation.
 */
int *direction;				/* Direction of transform    */
char inputfiles[][100];			/* Input files to CUBE	     */
{
  int count, i, file_indices[MAXNUMOFFILES], flen[MAXNUMOFFILES], current_file;
  char file[1000];				/* Input file names	     */

  /* first determine 'host' by RTL flags (in xvmaininc.h) -- this is the
   * convention used by the ISIS 'convert' program */
  if (!strcmp( NATIVE_INTFMT, "HIGH") && !strcmp( NATIVE_REALFMT, "IEEE"))
    strcpy( host, "SUN");
  else if (!strcmp( NATIVE_INTFMT, "LOW") && !strcmp( NATIVE_REALFMT, "IEEE"))
    strcpy( host, "ALPHA");
  else if (!strcmp( NATIVE_INTFMT, "LOW") && !strcmp( NATIVE_REALFMT, "VAX"))
    strcpy( host, "VAX");
  else if (!strcmp( NATIVE_INTFMT, "LOW") && !strcmp( NATIVE_REALFMT, "RIEEE"))
    strcpy( host, "PC");
  else zmabend(" ** unsupported platform! **");

  /* next, backplane type: */
  strcpy(bp_type,host);
  strcat(bp_type,"_REAL");

  *direction=FORWARD;				/* Set default to forward    */
  zvp("OUT",file,&count);
  numoffiles = count;
  for (current_file=1; current_file<=count; current_file++)
    zvunit(&outunit[current_file-1],"OUT",current_file,NULL);

  if(count>1)					/* Determine direction       */
    *direction=BACKWARD;			

  zvp("INP",file,&count);	 	/* Get input file names	*/
  zvsptr(file,count,file_indices,flen);  /* Get pointers to input files */

  /* Get unit # of all files   */
  for(current_file=1; current_file<=count; current_file++) {
    zvunit(&inunit[current_file-1],"INP",current_file,NULL);
    strcpy(inputfiles[current_file - 1],    /* Get input file names	     */
	&file[file_indices[current_file - 1] - 1]);
  }

  /* Check if history file specified: */
  zvp( "HISTFILE", hfilnam, &histfile);

  mphase = -1;		/* undetermined */

  if( *direction == FORWARD ) {

    /* Get task name for labels */
    zvp( "TASKNAME", tasknam, &i);
    if (!strncmp(tasknam,"NIMSCMM2",8) || !strncmp(tasknam,"NIMSCMM_",8))
     mphase = 1;
    else if (!strncmp(tasknam,"NIMSCMM",7)) mphase = 0;
    if (strncmp(tasknam,"VISIS2",6) && !strncmp(tasknam,"VISIS",5)) mphase = 0;

    verify_input_files(count);

    zvp("REQUESTR",v.requestor,&i);	/* Get requestor name 	     */
    zvp( "SPECIALP", &specialp, &i);
    if (!i) specialp = 0;
  }
}


/****************************************************************************/
FUNCTION verify_input_files(count)	
/*
 * Purpose: read labels of VICAR input files 
 */
int 	count;				/* Number of input files	     */
{
  int	band,				/* Loop control variable	     */
	ln,				/* Loop control - line	     	     */
	object,				/* Loop control variable	     */
	stat1,stat2,			/* lsef flags for history labels   */
	x,y,z;				/* Loop control variables	     */
  char	task[TASK_LIST_LEN * TASK_LEN]; /* Task names			     */
  int	instances[20],			/* Array of instances of history lbl */
	file_indices[6],		/* Output file indices		     */
	numtasks;			/* Number of tasks of history label  */

  for(object=0;object<count;object++) {	/* Get inputs' dimensions    	     */
    vstat = zvopen(inunit[object],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",NULL);
    vstat = zvget(inunit[object],"NL",&inl[object],"NS",&ns[object],
		"NB",&nb[object],"PIX_SIZE",&bytes[object],NULL);
  }

  if (count < MINNUMOFFILES)
    zmabend(" VISIS requires at least 3 input files (2DH, MM, COC)");

  object = 0;		/* Reset object to zero		     		*/

  if (nb[object] > 1) {	/* Check for erroneous histogram input file 	*/
    sprintf(xstring,"Input 2DH file is not two-dimensional");
    zmabend(xstring);
  }

  vstat = 0;

  numtasks = 20;
  vstat = zlhinfo(inunit[object],task,instances,&numtasks,"ERR_ACT","SA",NULL);
  vstat = x = 0;
  while( vstat == 0 && x < numtasks )
    if(strncmp(&task[x * TASK_LEN],"VISIS2",5)==0 || strncmp(&task[x++ * TASK_LEN],"HIST2D",6)==0)
      vstat = 1;
  if( vstat!=1 ) zmabend(" INVALID HISTOGRAM INPUT FILE","");

  zvclose(inunit[object],NULL);	/* End of histogram check    */

  specplots = 0;
  stat1 = 1;

  while( stat1==1 && object<(count-1) ) {
    numtasks = 20;
    vstat = zlhinfo(inunit[++object],task,instances,&numtasks,"ERR_ACT","SA",NULL);
    stat1 = x = 0;
    while( stat1 == 0 && x < numtasks )
      if(strncmp(&task[x++ * TASK_LEN],"SPECPLOT",8)==0) {
        specplots++;
        stat1 = 1;
        if( nb[object] != 1 )
          zmabend(" INVALID SPECTRAL PLOT INPUT FILE","");
        zvclose(inunit[object],NULL);
      }    /* End of SPECPLOT check    */
  }

  numoffiles = count;
  cubefiles  = numoffiles - specplots - 1;

  nbb = nb[object];	/* store the cube band dimension */

  /* check MM cube --
   * this is also a convenient place to check for Phase & IOF cube */

  iofcub = 0;
  ncubtasks = 20;		
  vstat = zlhinfo( inunit[object], cubtasks, instances, &ncubtasks,
   "ERR_ACT", "SA", NULL);
  vstat = x = 0;
  while( x < ncubtasks ) {
    if (strncmp(&cubtasks[x * TASK_LEN],tasknam,8)==0) vstat = 1;
    if (strncmp(&cubtasks[x++ * TASK_LEN],"NIMSR2IO",8)==0) iofcub = 1;
  }
  if( vstat!=1 || (bytes[object]!=2 && bytes[object]!=4) )
    zmabend(" *** INVALID CUBE FILE ***");

  if (mphase<0 && !strcmp(tasknam,"VISIS2")) {
    /* in this case we must look for special label item */
    zlget( inunit[object], "HISTORY", "SW_PHASE", &mphase, "HIST", tasknam, NULL);
    if (mphase<0) zmabend(" unable to determine phase 0/2!");
  }

  zvclose(inunit[object++],NULL);
  zvclose(inunit[object++],NULL);	/* also close cocube */

  if( cubefiles == 3 ) {    /* Check SII cube if it exists    */
    numtasks = 20;
    vstat = zlhinfo(inunit[object],task,instances,&numtasks,"ERR_ACT","SA",NULL);
    vstat = stat1 = stat2 = 0;
    for(x=0;x<numtasks;x++) {
      if( vstat==0 )
        if(strncmp(&task[x * TASK_LEN],"NIMSFLOAT",9)==0)
          vstat = 1;
      if( stat1==0 )
        if(strncmp(&task[x * TASK_LEN],tasknam,7)==0)
          stat1 = 1;
      if( stat2==0 )
        if(strncmp(&task[x * TASK_LEN],"INSERT3D",8)==0)
          stat2 = 1;
    }
    if(( vstat!=1 && stat1!=1 && stat2!=1 ) || nb[object] > 15 ) 
      zmabend("INVALID SII CUBE FILE","");
    zvclose(inunit[object++],NULL);
  }
}


/***************************************************************************/
FUNCTION VICARlabels(nlabrecs,nhistrecs,nrecs)    
/*
 * Generate VICAR format labels and get object pointers.
 */
int	*nlabrecs,			/* Number of label records	     */
	*nhistrecs,			/* Number of history records	     */
	*nrecs;				/* Number of cube file records	     */
{
int count, i, object=0, radtyp;
char file[100];

get_recinfo(nlabrecs,nhistrecs,nrecs);	

	/* 2D HISTOGRAM output file */

vstat = zvopen(outunit[object],"OPEN_ACT","SA","IO_ACT","SA","OP","WRITE",
 "U_NL",inl[object],"U_NS",ns[object],"U_NB",1,"O_FORMAT","BYTE",
 "U_FORMAT","BYTE",NULL);
keyword_value(outunit[object],"INSTRUMENT_ID","C",INSTRUMENT,3);
keyword_value(outunit[object],"INSTRUMENT_MODE_ID","C",INS_MODE,3);
keyword_value(outunit[object],"TARGET_NAME","S",TARGET,3);
keyword_value(outunit[object],"MISSION_PHASE_NAME","C",PHASE,3);
/*keyword_value(outunit[object],"PRODUCT_ID","C",PRODID,3);*/
keyword_value(outunit[object],"OBSERVATION_NAME","C",OBSNAME,3);
zvclose(outunit[object++],NULL);

	/* Spectra output files */
if( specplots > 0 )
  for (object=1;object<=specplots;object++) { 
    vstat = zvopen(outunit[object],"OPEN_ACT","SA","IO_ACT","SA","OP","WRITE",
     "U_NL",inl[object],"U_NS",ns[object],"U_NB",1,"O_FORMAT","BYTE",
     "U_FORMAT","BYTE",NULL);
    keyword_value(outunit[object],"INSTRUMENT_ID","C",INSTRUMENT,3);
    keyword_value(outunit[object],"INSTRUMENT_MODE_ID","C",INS_MODE,3);
    keyword_value(outunit[object],"TARGET_NAME","S",TARGET,3);
    keyword_value(outunit[object],"MISSION_PHASE_NAME","C",PHASE,3);
    keyword_value(outunit[object],"OBSERVATION_NAME","C",OBSNAME,3);
    zvclose(outunit[object],NULL);
  }

	/* VICAR MM cube file	*/

if (bytes[object]==2)
  vstat = zvopen(outunit[object],"OP","WRITE","U_FORMAT","HALF","O_FORMAT",
	"HALF","U_NL",inl[object],"U_NS",ns[object],"U_NB",nb[object],
	"OPEN_ACT","SA","IO_ACT","SA",NULL);
else 			/* (bytes[object]==4) */
  vstat = zvopen(outunit[object],"OP","WRITE","U_FORMAT","REAL","O_FORMAT",
	"REAL","U_NL",inl[object],"U_NS",ns[object],"U_NB",nb[object],
	"OPEN_ACT","SA","IO_ACT","SA",NULL);

	/* first gather map projection data and write a standard
	 * Vicar MAPLABV2-format label:
	 * (this call also writes other label items that cannot be
	 * processed by keyword_value and determines mphase) */
nbb = nb[object];	/* for band_mask determination */
lsef = write_vlab( outunit[object], &radtyp);
if (lsef == 0) zvmessage("*** Unable to write Vicar MAP label ***","");

	/* write other required NIMSCMM label items:
	 * (the following items are not written, since they are
	 * are not read by VISIS2:  COMMENT, SATURATED, ERTDATE/TIME) */

keyword_value( outunit[object], "MINIMUM_LATITUDE", "R", MIN_LAT, 3);
keyword_value( outunit[object], "MAXIMUM_LATITUDE", "R", MAX_LAT, 3);

keyword_value( outunit[object], "COORDINATE_SYSTEM_NAME", "C", LAT_TYPE, 3);

keyword_value( outunit[object], "EDR_FILE_NAME", "C", EDRS, 3);

keyword_value(outunit[object],"MISSION_PHASE_NAME","C",PHASE,3);
keyword_value(outunit[object],"TARGET_NAME","C",TARGET,3);
keyword_value(outunit[object],"START_TIME","T",BEG_SCET,3);
keyword_value(outunit[object],"STOP_TIME","T",END_SCET,3);
keyword_value( outunit[object], "START_SUB_SPACECRAFT_LATITUDE", "R", B_SSCLAT,
 3);
keyword_value( outunit[object], "STOP_SUB_SPACECRAFT_LATITUDE", "R", E_SSCLAT,
 3);
keyword_value( outunit[object], "START_SUB_SOLAR_LATITUDE", "R", B_SSOLLAT,
 3);
keyword_value( outunit[object], "STOP_SUB_SOLAR_LATITUDE", "R", E_SSOLLAT,
 3);
/*keyword_value(outunit[object],"PRODUCT_ID","C",PRODID,3);*/

/* obsext and mosnum are not in PDS label as separate items, so try
 * to extract them from the cube name: */
zvp("INP",file,&count);
for (i=0; i<strlen(file); i++) if (file[i]=='.') break;
if (i<7 || i==strlen(file))
  zvmessage("Unable to determine OBSEXT & MOS_NUM from qube name","");
else {
  strncpy(v.mosnum,&file[i-2],2);
  v.mosnum[2] = 0;
  zladd( outunit[object], "HISTORY", MOSNUM, v.mosnum, "FORMAT", "STRING", NULL);
  if (mphase) {
    v.obsext[0] = file[i-7];
    v.obsext[1] = 0;
    zladd( outunit[object], "HISTORY", OBSEXT, v.obsext, "FORMAT", "STRING", NULL);
  }
}

keyword_value( outunit[object], "DARK_UPDATE_TYPE", "C", DARK_TYPE, 3);
keyword_value( outunit[object], "PHOTOMETRIC_CORRECTION_TYPE", "C",PHOT_FUNC,3);
keyword_value( outunit[object], "PHOTO_CORR_CUTOFF_WAVELENGTH", "R",PHOT_CUT,3);
keyword_value( outunit[object], "MINNAERT_EXPONENT", "R", MINN_EXP, 3);
keyword_value( outunit[object], "MEAN_SCAN_RATE", "R", SLEW_RATE, 3);

keyword_value( outunit[object], "SATURATION_THRESHOLD_WEIGHT", "R",SATTHRSH,3);

keyword_value( outunit[object], "EXPANDED_RADIUS", "R", EQRAD_FUDG, 3);
keyword_value( outunit[object], "FILL_BOX_SIZE", "I", FILL_SIZE, 3);
keyword_value( outunit[object], "FILL_MIN_VALID_PIXELS", "I", FILL_NUM, 3);

if (mphase) {
  if (!calib) {
    keyword_value( outunit[object], "AACS_FILE_NAME", "C", AACS_FILE, 3);
    keyword_value( outunit[object], "PLATFORM_CKERNEL_NAME", "C", PFM_CK, 3);
    keyword_value( outunit[object], "ROTOR_CKERNEL_NAME", "C", ROT_CK, 3);
  }
}
else
  keyword_value( outunit[object], "POINTING_SOURCE", "C", POINT_SOURCE, 3);
keyword_value( outunit[object], "POINTING_OFFSET", "R", DPOINT, 3);
keyword_value( outunit[object], "WOBBLE_AMPLITUDE", "R", WAMP, 3);
keyword_value( outunit[object], "WOBBLE_FREQUENCY", "R", WFREQ, 3);
keyword_value( outunit[object], "WOBBLE_PHASE", "R", WPHASE, 3);
keyword_value( outunit[object], "WOBBLE_CONE_ESTIMATE", "C", WCONE, 3);

keyword_value( outunit[object], "SCLK_GAPS", "I", SC_GAPS, 3);

keyword_value( outunit[object], "SP_KERNEL_FILE_NAME", "C", SP_KERNEL, 3);
keyword_value( outunit[object], "I_KERNEL_FILE_NAME", "C", IKERNEL, 3);
keyword_value( outunit[object], "SPIKE_FILE_NAME", "C", DESPIKE_FILE, 3);
keyword_value( outunit[object], "BOOM_FILE_NAME", "C", DEBOOM_FILE, 3);
keyword_value( outunit[object], "CALIBRATION_FILE_NAME", "C", CAL_FILE, 3);
keyword_value( outunit[object], "DARK_VALUE_FILE_NAME", "C", DARK_FILE, 3);
keyword_value( outunit[object], "SOLAR_FLUX_FILE_NAME", "C", SOL_FILE, 3);

keyword_value( outunit[object], "INSTRUMENT_MODE_ID", "C", INS_MODE,3);
keyword_value( outunit[object], "GAIN_MODE_ID", "I", GAIN_STATE, 3);
keyword_value( outunit[object], "OFFSET_GRATING_POSITION","I",GRATING_OFFSET,3);
keyword_value( outunit[object], "START_GRATING_POSITION", "I", GRATING_START,3);
keyword_value( outunit[object], "GRATING_POSITION_INCREMENT", "I", GRATING_DELTA,3);
keyword_value( outunit[object], "GRATING_POSITIONS", "I", GRATING_STEPS,3);

keyword_value( outunit[object], "STOP_SLIDE_MODE_ID","C",STOP_SLIDE,3);

keyword_value( outunit[object],"STD_DEV_SELECTED_BAND_NUMBER","I",DN_STD_DEV,3);
keyword_value( outunit[object],"STD_DEV_SELECTED_BACKPLANE", "I",GEO_STD_DEV,3);

keyword_value( outunit[object],"INSTRUMENT_THRESHOLD", "I",THRESHVAL,3);
keyword_value( outunit[object],"GRATING_POSITION_CORRECTION", "R",PSHIFT,3);
keyword_value( outunit[object],"GRATING_STEP_INFLATION", "R",AINFL,3);

keyword_value( outunit[object], "INCIDENCE_ANGLE", "R", INCI_ANG, 3);
keyword_value( outunit[object], "EMISSION_ANGLE", "R", EMIS_ANG, 3);
keyword_value( outunit[object], "PHASE_ANGLE", "R", PHAS_ANG, 3);
keyword_value( outunit[object], "SOLAR_AZIMUTH", "R", SUN_AZI, 3);
keyword_value( outunit[object], "SUB_SPACECRAFT_AZIMUTH", "R", SC_AZI, 3);
keyword_value( outunit[object], "MINIMUM_SLANT_DISTANCE", "R",
 MIN_RANGE, 3);
keyword_value( outunit[object], "MAXIMUM_SLANT_DISTANCE", "R",
 MAX_RANGE, 3);
keyword_value( outunit[object], "MINIMUM_CENTRAL_BODY_DISTANCE", "R",
 MIN_CB_D, 3);
keyword_value( outunit[object], "MAXIMUM_CENTRAL_BODY_DISTANCE", "R",
 MAX_CB_D, 3);

	/* read these as strings to avoid problem with floating-point
	 * format (they are the only items written with "%g" format
	 * -- needs to be fixed at some point -- TBD */
keyword_value( outunit[object], "MIN_SPACECRAFT_SOLAR_DISTANCE", "C",
 MIN_SUN_D, 3);
keyword_value( outunit[object], "MAX_SPACECRAFT_SOLAR_DISTANCE", "C",
 MAX_SUN_D, 3);

keyword_value(outunit[object],"MEAN_FOCAL_PLANE_TEMPERATURE","R",T_FOCAL,3);
keyword_value(outunit[object],"MEAN_RAD_SHIELD_TEMPERATURE","R",T_RAD_SHIELD,3);
keyword_value(outunit[object],"MEAN_TELESCOPE_TEMPERATURE","R",T_TELESCOPE,3);
keyword_value(outunit[object],"MEAN_GRATING_TEMPERATURE","R",T_GRATING,3);
keyword_value(outunit[object],"MEAN_CHOPPER_TEMPERATURE","R",T_CHOPPER,3);
keyword_value(outunit[object],"MEAN_ELECTRONICS_TEMPERATURE",
	"R",T_ELECTRONICS,3);

keyword_value( outunit[object], "BAND_BIN_CENTER", "R", WAVELENGTHS, 3);

if (radtyp) {
  keyword_value( outunit[object], "BAND_BIN_SOLAR_FLUX", "R", SOLAR_FLUX, 3);
  keyword_value( outunit[object], "BAND_BIN_SENSITIVITY", "R", RAD_SENS, 3);
  keyword_value( outunit[object], "MEAN_DARK_DATA_NUMBER", "R", DRK_AVE, 3);
  keyword_value( outunit[object], "BAND_BIN_BASE", "R", RAD_BASE, 3);
  keyword_value( outunit[object], "BAND_BIN_MULTIPLIER", "R", RAD_CONV, 3);
}

  zvclose(outunit[object++],NULL);

  /* Open VICAR GEO cube file	*/
  vstat = zvopen(outunit[object],"OP","WRITE","U_FORMAT","REAL","O_FORMAT",
	"REAL","U_NL",inl[object],"U_NS",ns[object],"U_NB",nb[object],
	"OPEN_ACT","SA","IO_ACT","SA",NULL);
  zvclose(outunit[object++],NULL);

  if( cubefiles == 3 ) {
    /* Open VICAR SII cube file	*/
    vstat = zvopen(outunit[object],"OP","WRITE","U_FORMAT","REAL","O_FORMAT","REAL",
     "U_NL",inl[object],"U_NS",ns[object],"U_NB",nb[object],"OPEN_ACT","SA",
     "IO_ACT","SA",NULL);
    zvclose(outunit[object],NULL);
  }
}


/****************************************************************************/
FUNCTION VICARtoISIS(inputfiles)
/*
 *Purpose: Generate NIMS ISIS Cube file
 */
char 	inputfiles[][100];		/* Input file names		     */
{					/* 	LOCAL VARIABLES		     */
  int band, ln, nhistrecs, nlabrecs, nrecs, object, stat1, stat2, x, y, z,
    instances[20], file_indices[6], numtasks;
  char task[20][8];

  zvmessage(" Forward mode:  ISIS cube will be generated from VICAR files","");
  z=0;
  nrecs = inl[0]*ns[0]/BUFFERSIZE; /* Determine # of 512 rec to be written     */
  if( nrecs*BUFFERSIZE != inl[0]*ns[0] ) nrecs += 1;
  objptr[z++] = nrecs;

  if( specplots > 0 ) {
    objptr[z] = 0;
    for(object=1;object<=specplots;object++)
      objptr[z] += inl[object]*ns[object];
    objptr[z] = (objptr[z]*0.5)/BUFFERSIZE + 1 + nrecs;
    nrecs = objptr[z++];
  }
  x=0;
  for(object=specplots+1;object<numoffiles;object++)
    x += inl[object]*ns[object]*nb[object]*bytes[object];
  y = x/BUFFERSIZE;
  if( y*BUFFERSIZE != x ) y += 1;
  nrecs += y;
  objptr[z++] = nrecs;

  get_label_items();      /* Get all VICAR label items    */

  /* at this point we have the observation ID, so can retrieve the
   * record from catalog (if present): */
/*if (zvptst("USECAT")) get_cat();*/

  /* Write PDS label to cube file */
  create_PDSlabel(&nrecs,&nlabrecs,&nhistrecs,inputfiles);             

  /* Write objects to cube file */
  write_object(FORWARD,nlabrecs,nhistrecs,nrecs);       
}


/****************************************************************************/
FUNCTION write_object(dir,nlrecs,nhrecs,nrecs) 
/*
 * Copies object files to NIMS cube file or appropriate 
 * VICAR files, depending on direction of processing requested.
 */
int dir,	/* Direction of processing */
  nlrecs,	/* Number of label records */
  nhrecs,	/* Number of history records */
  nrecs; 	/* Number of file records */

{
  int band, bufsample;
  long bufptr;
  int count, extrasamples, index, indx, ln, object,
    record, sample, size1, testvalues[2], x, y, y1, z;
  unsigned char  buf[1000];  /* Object file buffer           */
  unsigned char  *nibbles;  /* Object file buffer in nibbles       */  
  unsigned char  nibbuf;    /* Holding value for test of nibble   */
  unsigned char  outbuf[BUFFERSIZE];
  unsigned char  *obuf;
  char hostname[33], intfmt[30], realfmt[30];

  object=index=0;

  if (dir==FORWARD) {  /* Copy object files to ISIS cube file    */

    /* Open ISIS cube file */
    vstat = zvopen(outunit[0],"U_FORMAT","BYTE","O_FORMAT","BYTE","OP","WRITE",
     "OPEN_ACT","SA","IO_ACT","SA","U_NL",nrecs,"U_NS",BUFFERSIZE,"U_NB",1,
     "COND","NOLABELS",NULL);

    for(x=0,ln=1;x<nlrecs;x++,ln++)  /* Write PDS label    */
      zvwrit(outunit[0],&bufpoint[x*BUFFERSIZE],"NSAMPS",BUFFERSIZE,
       "LINE",ln,NULL);
    free( bufpoint );    /* Deallocate memory    */

    for( x=0;x<nhrecs;x++,ln++ )  /* Write History object    */
      zvwrit(outunit[0],&histpoint[x*BUFFERSIZE],"NSAMPS",BUFFERSIZE,
       "LINE",ln,NULL);
    free( histpoint );    /* Deallocate memory    */

    /* Open 2D histogram file  */
    vstat = zvopen(inunit[object],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",
     "ADDRESS",&inptr,NULL);

    size = inl[object]*ns[object];  /* Write 2D histogram to output  */
    for( x=0; x<size/BUFFERSIZE; x++,ln++,inptr+=BUFFERSIZE ) {
      zmve( 1, BUFFERSIZE, inptr, outbuf, 1, 1);
      zvwrit(outunit[0],outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln,NULL);
    }

    size -= (int)(size/BUFFERSIZE)*BUFFERSIZE;
    if ( size != 0 ) {
      zmve( 1, size, inptr, outbuf, 1, 1);
      memset( &outbuf[size], 0, BUFFERSIZE-size );
      zvwrit(outunit[0],outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln,NULL);
      ln++;
    }

    zvclose(inunit[object++],NULL);
    zvmessage("FILE 1 copied","");

    if( specplots > 0 ) {   /* Write SPECTRA if present  */
      size = (ns[1]*inl[1]*specplots)/2;
      nibbles=(unsigned char *)calloc(size,sizeof(unsigned char));

      for( y=0, object=1; object<=specplots; object++ ) {
        vstat = zvopen(inunit[object],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",
	 "ADDRESS",&inptr,NULL);
        for( x=0; x<size/specplots; x++, y++ ) {
          zmve( 1, 2, inptr, buf, 1, 1);
          nibbles[y] = (buf[0]&240) | (((int)buf[1]&240) >> 4);
          inptr += 2;
        }
        sprintf(xstring,"FILE %d copied",object+1);
        zvmessage(xstring,"");
        zvclose(inunit[object],NULL);
      }

      for( x=0;x<size/BUFFERSIZE;x++,ln++)
        zvwrit(outunit[0],&nibbles[x*BUFFERSIZE],
             "NSAMPS",BUFFERSIZE,"LINE",ln,NULL);
      size -= (int)(size/BUFFERSIZE)*BUFFERSIZE;
      if ( size != 0 ) {
        zmve( 1, size, &nibbles[x*BUFFERSIZE], outbuf, 1, 1);
        memset( &outbuf[size], 0, BUFFERSIZE-size );
        zvwrit(outunit[0],outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln,NULL);
        ln++;
      }
      free(nibbles);
    }

    size = 0;

    /* Write MM, GEOCUBE and SIICUBE to ISIS cube */
    while ( object < numoffiles ) {
      vstat = zvopen(inunit[object],"OP","READ","OPEN_ACT","SA","IO_ACT","SA",
       "ADDRESS",&inptr,NULL);
  
      size1 = inl[object]*ns[object]*nb[object]*bytes[object];
      extrasamples = 0;
    
      if ( size > 0 ) {
        extrasamples = BUFFERSIZE - size;
	if (size1 > extrasamples) {
          zmve( 1, extrasamples, inptr, &outbuf[size], 1, 1);
          inptr += extrasamples;
	}
	else {
          zmve( 1, size1, inptr, &outbuf[size], 1, 1);
          memset( &outbuf[size+size1], 0, BUFFERSIZE-size-size1 );
	}
        zvwrit(outunit[0],outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln,NULL);
        ln++;
      }

      /* Copy input file contents to ISIS CUBE objects  */

      size = size1 - extrasamples;

      for ( x=0;x<size/BUFFERSIZE;x++,ln++,inptr+=BUFFERSIZE ) {
        zmve( 1, BUFFERSIZE, inptr, outbuf, 1, 1);
        zvwrit(outunit[0],outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln,NULL);
      }

      size -= (int)(size/BUFFERSIZE)*BUFFERSIZE;

      if( size > 0 )
        if( object != (numoffiles-1) )
          zmve( 1, size, inptr, outbuf, 1, 1);
      else {
        zmve( 1, size, inptr, outbuf, 1, 1);
        memset( &outbuf[size], 0, BUFFERSIZE-size );
        zvwrit(outunit[0],outbuf,"NSAMPS",size,"LINE",ln,NULL);
      }
    
      zvclose(inunit[object++],NULL);
      sprintf(xstring,"FILE %d copied",object);
      zvmessage(xstring,"");
    }
    zvclose(outunit[0],NULL);
  }
  else {   /* Copy ISIS cube file objects to respective VICAR files     */

    vstat = zvopen(inunit[0],"OPEN_ACT","SA","IO_ACT","SA","ADDRESS",&inptr,
     "U_FORMAT","BYTE","O_FORMAT","BYTE","COND","NOLABELS","U_NL",nrecs,
     "U_NS",BUFFERSIZE,NULL);

    object = indx = 0;    /* Refer to 2D histogram       */

    bufptr = inptr;  
    inptr += (objptr[indx++]-1)*BUFFERSIZE;  

    vstat = zvopen(outunit[object],"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",
     "ADDRESS",&outptr,NULL);

    size = inl[object]*ns[object];
    zmve( 1, size, inptr, outptr, 1, 1);
    inptr += size;
    zvclose(outunit[object++],NULL);

    sprintf(xstring,"FILE %d created",object);
    zvmessage(xstring,"");

    /* If spectra files, decode nibbles and  write VICAR files. */
    if ( specplots > 0 ) {
      testvalues[0] = 240;	/* Value to be ANDed with byte to get   */
				/* the first nibble.      */
      testvalues[1] = 15;	/* Value to be ANDed with byte to get   */
				/* the second nibble.      */
      inptr = bufptr;  /* Calculate first spectra record     */
      inptr += (objptr[indx++]-1)*BUFFERSIZE;

      for(object=1;object<=specplots;object++) {
        size = ns[object]/2;
        nibbles=(unsigned char *)calloc(size,sizeof(unsigned char));

        vstat = zvopen(outunit[object],"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",
	 "ADDRESS",&outptr,NULL);

        for (ln=0;ln<inl[object];ln++) {
          size = ns[object]/2;
          zmve( 1, size, inptr, nibbles, 1, 1);
          inptr += size;
          for (sample=0;sample<size;sample++) { /* Decode nibbles   */
	    for(x=0;x<2;x++) {
	      nibbuf = nibbles[sample]&testvalues[x];
	      index = sample*2 + x;
	      switch ( nibbuf ) {
		default:
		case 0: buf[index] = nibbuf;	/* Plot background */
			break;
		case 48:			/* Standard dev.   */
		case 3:	buf[index] = 60;	/* gray level     */
			break;
		case 96:			/* Break band bar  */
		case 6:	buf[index] = 100;	/* gray level     */
			break;
		case 240:			/* Mean and text   */
		case 15: buf[index] = 255;	/* gray level      */
			break;
	      }
	    }
	  }
          size = ns[object];
	  zmve( 1, size, buf, outptr, 1, 1);
	  outptr += size;
	}
	zvclose(outunit[object],NULL);
	sprintf(xstring,"FILE %d created",object+1);
	zvmessage(xstring,"");
	free(nibbles);
      }
    }

    if (histfile) {      /* write out all history records to History file */
      hfile = fopen(hfilnam,"w");
      /* first determine where blank area starts, as we don't need to
       * copy this ... */
      inptr = bufptr+(nlrecs+nhrecs-1)*BUFFERSIZE;
      y1 = 0;
      for (x=0; x<nhrecs; x++) {
	zmve( 1, BUFFERSIZE, inptr, buf, 1, 1);
	for (y=BUFFERSIZE-1; y>=0; y--) {
	  if (buf[y] != ' ') {
	    y1 = y+1;
	    break;
	  }
	}
	if (y1>0) break;
	inptr -= BUFFERSIZE;
      }
      inptr = bufptr+nlrecs*BUFFERSIZE;
      for (z=0; z<nhrecs-x-1; z++) {
	zmve( 1, BUFFERSIZE, inptr, buf, 1, 1);
	inptr += BUFFERSIZE;
	for (y=0; y<BUFFERSIZE; y++)
	  vstat = fputc( buf[y], hfile);
      }
      zmve( 1, BUFFERSIZE, inptr, buf, 1, 1);
      for (y=0; y<y1-1; y++)
	vstat = fputc( buf[y], hfile);
      fclose(hfile);  
    }

    inptr = bufptr;    /* Calculate first cube record */
    inptr += (objptr[indx++]-1)*BUFFERSIZE;

    /* Transfer Cubes to separate VICAR output files */

    for( z = 0; z < cubefiles; z++ ) {
  
      vstat = zvopen(outunit[object],"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",
       "LAB_ACT","SA","CLOS_ACT","SA","ADDRESS",&outptr,NULL);

      size = ns[object] * inl[object] * bytes[object];

      for ( x = 0; x < nb[object]; x++ ) {
        zmve( 1, size, inptr, outptr, 1, 1);
        inptr += size;
        outptr += size;
      }
      zvclose(outunit[object++],NULL);
      sprintf(xstring,"FILE %d created",object);
      zvmessage(xstring,"");
    }
    zvclose(inunit[0],NULL);

    /* update label if needed -- if not specified, assume that host is the
     * current machine
     * (do this after writing data to avoid confusing Vicar)
     * THIS IS AN AD-HOC FIX, USING CODE BORROWED FROM VISISX -- BETTER IS
     * TO HAVE THE CODE CHECK THE 'CORE_ITEM_TYPE' KEYWORD IN THE ISIS CUBE -- TBD.
     * We only do this when no SII or Specplots are present.
     */
    if (numoffiles!=3) return;
    zvp("HOST",hostname,&x);
    if (!x) return;
    /* (NATIVE_HOST_LABEL is defined in $V2INC/xvmaininc.h) */
    if (!strcmp(hostname,NATIVE_HOST_LABEL)) return;
    for ( x=1; x<numoffiles; x++) {	/* just the cube & co-cube */
      vstat = zvopen(outunit[x],"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",NULL);
      vstat =zvhost(hostname,intfmt,realfmt);
      if (vstat<=0) zmabend(" invalid HOST!");
      zldel( outunit[x], "SYSTEM", "HOST", NULL);
      zladd( outunit[x], "SYSTEM", "HOST", hostname, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL);
      zldel( outunit[x], "SYSTEM", "INTFMT", NULL);
      zladd( outunit[x], "SYSTEM", "INTFMT", intfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL);
      zldel( outunit[x], "SYSTEM", "REALFMT", NULL);
      zladd( outunit[x], "SYSTEM", "REALFMT", realfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL);
      zldel( outunit[x], "SYSTEM", "BHOST", NULL);
      zladd( outunit[x], "SYSTEM", "BHOST", hostname, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL);
      zldel( outunit[x], "SYSTEM", "BINTFMT", NULL);
      zladd( outunit[x], "SYSTEM", "BINTFMT", intfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL);
      zldel( outunit[x], "SYSTEM", "BREALFMT", NULL);
      zladd( outunit[x], "SYSTEM", "BREALFMT", realfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL);
      zvclose(outunit[x],NULL);
    }
  }
}


/*****************************************************************************/
FUNCTION write_PDS_aline(buf,type,labelitem,value,number)
/*
 * Write PDS label line to a given buffer for all data types.
 * This differs from write_PDS_line in that value points to a
 * single item, which is replicated in the label array.
 */
char 	buf[],
	type,
	labelitem[];
void	*value;
int 	number;
{
int	x,y,z;
char	strng[50],
	ln[80];
int	*ivalue;
float	*fvalue;
char 	*cvalue;

switch(type)	{

case 'C':	/* String constant 	*/
case 'c':	cvalue = value;
		sprintf(ln,"%s = (",labelitem);
		for( x = 0; x < number-1 ; x++ )
			{
			sprintf(strng,"%s,",cvalue);
			strcat(ln,strng);
			if( strlen(ln) > 60 )
				{
				strcat(ln,"\r\n");
				strcat(buf,ln);
				strcpy(ln,"     ");
				}
			}
		sprintf(strng,"%s)\r\n",cvalue);
		strcat(ln,strng);
		strcat(buf,ln);
		break;

case 'I': 	/* Integer value 	*/
case 'i':	ivalue = value;
		sprintf(ln,"%s = (",labelitem);
		for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%d,",ivalue);
				strcat(ln,strng);
				if( strlen(ln) > 60 )
					{
					strcat(ln,"\r\n");
					strcat(buf,ln);
					strcpy(ln,"     ");
					}
				}
		sprintf(strng,"%d)\r\n",ivalue);
		strcat(ln,strng);
		strcat(buf,ln);
		break;

default:
case 'F': 	/* floating point value 	*/
case 'f':	fvalue = value;
		sprintf(ln,"%s = (",labelitem);
		for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%f,",*fvalue);
				strcat(ln,strng);
				if( strlen(ln) > 60 )
					{
					strcat(ln,"\r\n");
					strcat(buf,ln);
					strcpy(ln,"     ");
					}
				}
		sprintf(strng,"%f)\r\n",*fvalue);
		strcat(ln,strng);
		strcat(buf,ln);
		break;	}
}


/*****************************************************************************/
FUNCTION write_PDS_cline( buf, item, value, num, slen)
/*
 * Write PDS label line to a given buffer for character data.
 * Because of the lengths of some filenames, the buffers need
 * to exceed the normal PDS line length.
 */
char buf[], item[], *value;
int num, slen;
{
  int x,y,z;
  char strng[101], ln[101];

  if (num==1) {
    sprintf( ln, "%s = ", item);
    sprintf( strng, "%s,", value);
    if( (strlen(ln)+strlen(strng)) > 60 ) {
      strcat(ln,"\r\n");
      strcat(buf,ln);
      strcpy(ln,"     ");
    }
    strcat(ln,strng);
    strcat(buf,ln);
    return;
  }

  sprintf( ln, "%s = (", item);

  for ( x = 0; x < num-1 ; x++ ) {
    sprintf( strng, "\"%s\",", value+x*slen);
    if ( (strlen(ln)+strlen(strng)) > 60 ) {
      strcat(ln,"\r\n");
      strcat(buf,ln);
      strcpy(ln,"     ");
    }
    strcat(ln,strng);
  }
  sprintf( strng, "\"%s\")\r\n", value+(num-1)*slen);
  if ( (strlen(ln)+strlen(strng)) > 60 ) {
    strcat(ln,"\r\n");
    strcat(buf,ln);
    strcpy(ln,"     ");
  }
  strcat(ln,strng);
  strcat(buf,ln);
}


/***************************************************************************/
FUNCTION write_PDS_line(buf,type,labelitem,value,number,prec)
/*
 * Write PDS label line to a given buffer for data types other than
 * character
 */
char 	buf[],
	type,
	labelitem[];
void	*value;
int 	number,prec;
{
  int x, y, z, *ivalue;
  char strng[50], ln[80];
  float	*fvalue;

  switch(type) {

    case 'I': 	/* Integer value 	*/
    case 'i':
      ivalue = value;
      if( number == 1 ) {
	sprintf(strng,"%s = %d\r\n",labelitem,(*ivalue));
	strcat(buf,strng);
      }
      else {
	sprintf(ln,"%s = (",labelitem);
	for( x = 0; x < number-1 ; x++ ) {
	  sprintf(strng,"%d,",(*(ivalue+x)));
	  strcat(ln,strng);
	  if( strlen(ln) > 60 ) {
	    strcat(ln,"\r\n");
	    strcat(buf,ln);
	    strcpy(ln,"     ");
	  }
	}
	sprintf(strng,"%d)\r\n",(*(ivalue+x)));
	strcat(ln,strng);
	strcat(buf,ln);
      }
      break;

    default:
    case 'F': 	/* floating point value 	*/
    case 'f':	fvalue = value;
      if( number == 1 ) {
	if (prec<0)
	  sprintf(strng,"%s = %e\r\n",labelitem,(*fvalue));
	else
	  sprintf(strng,"%s = %.*f\r\n",labelitem,prec,(*fvalue));
	strcat(buf,strng);
      }
      else {
	sprintf(ln,"%s = (",labelitem);
	for( x = 0; x < number-1 ; x++ ) {
	  if (prec<0)
	    sprintf(strng,"%e,",(*(fvalue+x)));
	  else
	    sprintf(strng,"%.*f,",prec,(*(fvalue+x)));
	strcat(ln,strng);
	if( strlen(ln) > 60 ) {
	  strcat(ln,"\r\n");
	  strcat(buf,ln);
	  strcpy(ln,"     ");
	}
      }
      if (prec<0)
	sprintf(strng,"%e)\r\n",(*(fvalue+x)));
      else
	sprintf(strng,"%.*f)\r\n",prec,(*(fvalue+x)));
      strcat(ln,strng);
      strcat(buf,ln);
    }
    break;
  }
}


/*****************************************************************************/
int FUNCTION write_vlab( unit, radtyp)
/*
 * write MAPLABV2-format label to Vicar output cube;
 * also process a few other labels here that cannot be directly
 * copied from PDS label using 'keyword_value'
 */
int unit, *radtyp;
{
  int count, i, ival[3], lptr, lptr0, lptr1, lptr2, lptre, lptrq, 
   to_orig, x;
  float ritem, sav;
  double ditem;
  char buf[332], lattyp[15];
  struct { float rr[38]; int ii[2];} mdat;	/* for MAPLABV2 */
  MP mpo;

	/* zero the map buffer just to be safe: */
  for (i=0; i<38; i++) mdat.rr[i] = 0.0;
  for (i=0; i<2; i++) mdat.ii[i] = 0;

  /* first determine mphase, if not yet done: */
  if (mphase<0) {
    lptr = lptrq = 0;
    lsef = find_keyword( "NIMSCMM2", bufpoint, &lptrq, &labend);
    if (lsef>0) mphase = 1;
    else {
      lptr = lptrq = 0;
      lsef = find_keyword( "NIMSCMM", bufpoint, &lptrq, &labend);
      if (lsef>0) mphase = 0;
      else zmabend(" unable to find NIMSCMM or NIMSCMM2 in History");
    }
  }

	/* set the pointer to start of map group in qube object: */
  lptr = lptrq = 0;
  lsef = find_keyword( "^QUBE", bufpoint, &lptrq, &labend);
  if (lsef == 0) return 0;
  lptr0 = lptrq;

	/* find end of label so we don't search thru entire file: */
  lsef = find_keyword( "END_OBJECT = QUBE", bufpoint, &lptre, &labend);

  /* check if target = CAL/SKY, in which case bypass Map stuff */
  lsef = find_keyword( "TARGET_NAME", bufpoint,&lptr, &lptre);
  if (!lsef) zmabend(" Unable to find Target name!");
  lsef = get_string_value( v.target, 16, &lptr);
  if (!strcmp(v.target,"CAL") && strcmp(v.target,"CALLISTO")) calib = 1;
  if (!strcmp(v.target,"SKY")) calib = 1;
  if (calib) goto endmap;

	/* assume that MAP_PROJECTION_TYPE is the first keyword
	 * in the map projection group: */
  lsef = find_keyword( "MAP_PROJECTION_TYPE", bufpoint, &lptr0, &lptre);
  if (lsef == 0) return 0;
  lsef = get_string_value( v.projection, 24, &lptr0);
  if (lsef == 0) return 0;

	/* check if it's planetographic or planetocentric */
  lptr = lptr0;
  lsef = find_keyword( "COORDINATE_SYSTEM_NAME", bufpoint, &lptr, &lptre);
  if (lsef == 0) pgraphic = -1;
  else {
    lsef = get_string_value( lattyp, 15, &lptr);
    if (lsef == 0) return 0;
    if (!strcmp( lattyp, "PLANETOGRAPHIC")) pgraphic = 1;
    else pgraphic = 0;
    if (!mphase) {	/* temporary warning until NIMSCMM is fixed ... */
      zvmessage(" Warning:  VISIS2 treatment of latitudes may be inconsistent with NIMSCMM!", "");
    }
  }

  /* check for Longitude direction:  if East change to West for mpbuf2mpo */
  conv_lon = 0;
  lptr = lptr0;
  lsef = find_keyword( "POSITIVE_LONGITUDE_DIRECTION", bufpoint, &lptr, &lptre);
  if (lsef) {
    lsef = get_string_value( buf, 5, &lptr);
    if (lsef && !strcmp(buf,"EAST")) conv_lon = 1;
  }

	/* for the following keywords, find end of map proj. group: */
  lptr2 = lptr0;
  lsef = find_keyword( "END_GROUP = IMAGE_MAP_PROJECTION", bufpoint, &lptr2,
   &lptre);

	/* then find all required keywords: */

	/* all projections need radii: */
  lptr = lptr0;
  lsef = find_keyword( "A_AXIS_RADIUS", bufpoint, &lptr, &lptr2);
  if (lsef) get_double( &v.radii[0], bufpoint, &lptr);
  lptr = lptr0;
  lsef = find_keyword( "B_AXIS_RADIUS", bufpoint, &lptr, &lptr2);
  if (lsef) get_double( &v.radii[1], bufpoint, &lptr);
  lptr = lptr0;
  lsef = find_keyword( "C_AXIS_RADIUS", bufpoint, &lptr, &lptr2);
  if (lsef) get_double( &v.radii[2], bufpoint, &lptr);
  mdat.rr[24] = v.radii[2];	/* polar radius */
  mdat.rr[36] = v.radii[1];	/* short equatorial radius (POV only) */
  mdat.rr[25] = v.radii[0];	/* equatorial radius */

  /* check for OFFSET_DIRECTION keyword -- if absent, assume old
   * label, hence FROM_ORIGIN */
  lptr = lptr0;
  lsef = find_keyword( "TO_ORIGIN", bufpoint, &lptr, &lptr2);
  if (lsef) to_orig = 1;
  else {
    if (zvptst("TO_ORI")) to_orig = 1;	/* allow user override */
    else to_orig = 0;
  }

  if (strcmp(v.projection,"POINT_PERSPECTIVE")==0) {

    lptr = lptr0;
    lsef = find_keyword( "SUB_SPACECRAFT_LATITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.ssc_lat, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "SUB_SPACECRAFT_LONGITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double( &v.ssc_lon, bufpoint, &lptr);
      if (conv_lon) v.ssc_lon = 360.-v.ssc_lon;
    }

    lptr = lptr0;
    lsef = find_keyword( "LINE_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double( &v.ssc_line, bufpoint, &lptr);
      if (to_orig) v.ssc_line = -v.ssc_line;
      v.ssc_line++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double( &v.ssc_samp, bufpoint, &lptr);
      if (to_orig) v.ssc_samp = -v.ssc_samp;
      v.ssc_samp++;	/* relative to (1,1) */
    }

	/* allow old aliases for the above 2: */
    lptr = lptr0;
    lsef = find_keyword( "X_AXIS_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double( &v.ssc_line, bufpoint, &lptr);
      v.ssc_line++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "Y_AXIS_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double( &v.ssc_samp, bufpoint, &lptr);
      v.ssc_samp++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "TARGET_CENTER_DISTANCE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.range, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "LINE_OPTICAL_AXIS_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double( &v.oaline, bufpoint, &lptr);
      if (to_orig) v.oaline = -v.oaline;
      v.oaline++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_OPTICAL_AXIS_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double( &v.oasamp, bufpoint, &lptr);
      if (to_orig) v.oasamp = -v.oasamp;
      v.oasamp++;	/* relative to (1,1) */
    }

	/* allow old aliases for the above 2: */
    lptr = lptr0;
    lsef = find_keyword( "X_AXIS_OPTICAL_AXIS_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double( &v.oaline, bufpoint, &lptr);
      v.oaline++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "Y_AXIS_OPTICAL_AXIS_OFFSET", bufpoint, &lptr, 
     &lptr2);
    if (lsef) {
      get_double( &v.oasamp, bufpoint, &lptr);
      v.oasamp++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "FOCAL_LENGTH", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.focal, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "FOCAL_PLANE_SCALE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.cscale, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "MAP_PROJECTION_ROTATION", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.rotation, bufpoint, &lptr);

	/* stuff values into maplab buffer: */
    mdat.ii[0] = 16;
    mdat.rr[26] = v.focal;
    mdat.rr[27] = v.oaline;
    mdat.rr[28] = v.oasamp;
    mdat.rr[29] = v.cscale;
    mdat.rr[30] = v.ssc_lat;
    mdat.rr[31] = v.ssc_lon;
    mdat.rr[32] = v.ssc_line;
    mdat.rr[33] = v.ssc_samp;
    mdat.rr[34] = v.rotation;
    mdat.rr[37] = v.range;
  }
  else {

    lptr = lptr0;
    lsef = find_keyword( "MAP_SCALE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.scale, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "CENTER_LATITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.center_lat, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "CENTER_LONGITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double( &v.center_lon, bufpoint, &lptr);
      if (conv_lon) v.center_lon = 360.-v.center_lon;
    }

    /* add 1 to LINE/SAMP OFFSETs to convert to absolute line/sample
     * (mpbuf2mpo will subtract 1) */
    lptr = lptr0;
    lsef = find_keyword( "LINE_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double( &v.center_line, bufpoint, &lptr);
      if (to_orig) v.center_line = -v.center_line;
      v.center_line++;
    }
    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double( &v.center_samp, bufpoint, &lptr);
      if (to_orig) v.center_samp = -v.center_samp;
      v.center_samp++;
    }

	/* allow old aliases for the above 2: */
    lptr = lptr0;
    lsef = find_keyword( "X_AXIS_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.center_line, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "Y_AXIS_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) get_double( &v.center_samp, bufpoint, &lptr);

    v.projitem1 = 0.0;
    v.projitem2 = 0.0;
    if( v.projection[0] == 'L' ) {	/* Lambert */
      lptr = lptr0;
      lsef = find_keyword( "FIRST_STANDARD_PARALLEL", bufpoint, &lptr,
       &lptr2);
      if (lsef) get_double( &v.projitem1, bufpoint, &lptr);
      lptr = lptr0;
      lsef = find_keyword( "SECOND_STANDARD_PARALLEL", bufpoint, &lptr,
       &lptr2);
      if (lsef) get_double( &v.projitem2, bufpoint, &lptr);
    }

    v.rotation = 0.0;
    if( strcmp(v.projection,"POLAR_ORTHOGRAPHIC")==0 ||
        strcmp(v.projection,"POINT_PERSPECTIVE")==0 ||
        strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC")==0 ||
        strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC")==0 ) {
      lptr = lptr0;
      lsef = find_keyword( "MAP_PROJECTION_ROTATION", bufpoint, &lptr,
       &lptr2);
      if (lsef) get_double( &v.rotation, bufpoint, &lptr);
    }

    /* translate ISIS back to Vicarese ... */
    if (!strcmp(v.projection,"'SINUSOIDAL_EQUAL-AREA'")) 
      strcpy(v.projection,"SINUSOIDAL");

	/* stuff values into maplab buffer: */
    if  (!strcmp( v.projection, "POLAR_ORTHOGRAPHIC")) mdat.ii[0] = 1;
    else if (!strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC")) mdat.ii[0]=2;
    else if (!strcmp(v.projection,"POLAR_STEREOGRAPHIC")) mdat.ii[0]=3;
    else if (!strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC")) mdat.ii[0]=4;
    else if (!strcmp(v.projection,"LAMBERT_CONFORMAL")) mdat.ii[0]=5;
    else if (!strcmp(v.projection,"MERCATOR")) mdat.ii[0]=6;
    else if (!strcmp(v.projection,"NORMAL_CYLINDRICAL")) mdat.ii[0]=9;
    else if (!strcmp(v.projection,"SIMPLE_CYLINDRICAL")) mdat.ii[0]=10;
    else if (!strcmp(v.projection,"OBLIQUE_SIMPLE_CYLINDRICAL")) mdat.ii[0]=11;
    else if (!strcmp(v.projection,"SINUSOIDAL")) mdat.ii[0]=12;
    else if (!strcmp(v.projection,"OBLIQUE_SINUSOIDAL")) mdat.ii[0]=13;
    else if (!strcmp(v.projection,"MOLLWEIDE")) mdat.ii[0]=14;
    else if (!strcmp(v.projection,"TRANSVERSE_MERCATOR")) mdat.ii[0]=15;
    else if (!strcmp(v.projection,"POINT_PERSPECTIVE")) mdat.ii[0]=16;
    mdat.rr[0] = v.center_samp;
    mdat.rr[1] = v.center_line;
    mdat.rr[2] = v.center_lat;
    mdat.rr[3] = v.projitem1;
    mdat.rr[4] = v.projitem2;
    mdat.rr[5] = v.center_lon;
    mdat.rr[6] = v.scale;
    if (v.center_lat < 0.) mdat.rr[7] = -1.0;
    else mdat.rr[7] = 1.0;
    mdat.rr[8] = v.rotation;
  }

  if (tube) {
    zladd( unit, "HISTORY", "MAP00000",
     "** TUBE FILE: map labels apply to cocube data only **",
     "FORMAT", "STRING",NULL);
    lptr = lptr0;
    lsef = find_keyword( "LINE_LAST_PIXEL", bufpoint, &lptr, &lptr2);
    if (lsef) get_integer_value(&cubsiz[0],bufpoint,&lptr);
    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_LAST_PIXEL", bufpoint, &lptr, &lptr2);
    if (lsef) get_integer_value(&cubsiz[1],bufpoint,&lptr);
    zladd( unit, "HISTORY", CUBE_SIZE, cubsiz, "FORMAT", "INT",
     "NELEMENT", 2, NULL);
  }
	/* write the Vicar map labels: */
  lsef = mpInit( &mpo);
  if (lsef!=mpSUCCESS) zmabend(" error initializing MP object");
  lsef = mpBuf2Mpo( &mdat, mpo);
  if (lsef!=mpSUCCESS) zmabend(" error in MP translation");
  /* mpBuf2Mpo always writes "planetocentric", so fix this if we passed
   * it 'graphic' latitudes: */
  if (pgraphic==1) {
    vstat = mpSetValues( mpo, mpCOORDINATE_SYSTEM_NAME, "PLANETOGRAPHIC", "");
    if (vstat!=mpSUCCESS) zmabend(" error setting Coord.system");
  }
  lsef = mpLabelWrite( mpo, unit, "HISTORY");
  if (lsef!=mpSUCCESS) zmabend(" error writing map labels");
  lsef = mpLabelWrite( mpo, unit, "PROPERTY");
  if (lsef!=mpSUCCESS) zmabend(" error writing map labels");

endmap:
	/* PROJECT & INSTRMNT are always the same: */
  zladd( unit, "HISTORY", PROJECT, "GLL", "FORMAT", "STRING",NULL);
  zladd( unit, "HISTORY", INSTRUMENT, "NIMS", "FORMAT", "STRING",NULL);

 	/* need s/w phase in case cube is input again to VISIS2 forward mode: */
  zladd( unit, "HISTORY", "SW_PHASE", &mphase, "FORMAT", "INT", NULL);

	/* process other deviant label items: */

  /* OBSNAME needs to have OBSEXT stripped off end, also quotes, if present: */
  lptr = lptrq;
  lsef = find_keyword( "OBSERVATION_NAME", bufpoint, &lptr, &lptre);
  if (lsef>0) {
    get_string_value( buf, 14, &lptr);
    if (buf[0] == '\'') {
      strncpy( buf, &buf[1], 12);
      if (mphase) buf[12] = '\0';
      else 	/* phase-0 length is not fixed, search for trailing quote */
	for (x=0; x<13; x++)
	  if (buf[x] == '\'') {
	    buf[x] = '\0';
	    break;
	  }
    }
    zladd( unit, "HISTORY", OBSNAME, buf, "FORMAT", "STRING",NULL);
  }
  else zvmessage(" keyword OBSERVATION_NAME not found in cube label","");

  /* NOTE must be split up into 3 parts, in the new cubes;
   * (if there are separate OBSERVATION_NOTE & PRODUCT_NOTE, then this qube
   * was made before this feature was implemented) */
  lptr = lptrq;
  lsef = find_keyword( "PRODUCT_NOTE", bufpoint, &lptr, &lptre);
  if (lsef>0) {		/* old-style label */
    keyword_value( unit, "PRODUCT_NOTE","C",PRODNOTE,3);
    keyword_value( unit, "OBSERVATION_NOTE","C",OBSNOTE,3);
  }
  else {
    lptr = lptrq;
    lsef = find_keyword( "NOTE", bufpoint, &lptr, &lptre);
    get_qstring_value( v.obsnote, 331, &lptr);
    /* first remove the "\r\n  " constructs inserted to prevent long lines: */
    x = strlen(v.obsnote);
    for (i=0; i<x-5; i++) {
      if (!strncmp( &v.obsnote[i], "\r\n  ", 4)) {
        strcpy( buf, &v.obsnote[i+4]);
        strcpy( &v.obsnote[i], buf);
        x -= 4;
      }
    }
    /* then find the two " // " separators: */
    for (i=0; i<x-3; i++) {
      if (!strncmp( &v.obsnote[i], " //", 3)) {
        strcpy( v.suppnote,  &v.obsnote[i+4]);
        v.obsnote[i] = '\0';
	break;
      }
    }
    zladd( unit, "HISTORY", OBSNOTE, v.obsnote, "FORMAT", "STRING",NULL);
    if (i > x-4) zvmessage(" SUPPNOTE & PRODNOTE not found!"," ");
    else {
      x = strlen(v.suppnote);
      for (i=0; i<x-3; i++) {
        if (!strncmp( &v.suppnote[i], " //", 3)) {
          strcpy( v.prodnote,  &v.suppnote[i+4]);
          v.suppnote[i] = '\0';
	  break;
	}
      }
      if (strlen(v.suppnote) > 2)
        zladd( unit, "HISTORY", SUPPNOTE, v.suppnote, "FORMAT", "STRING",NULL);
      if (i > x-4) zvmessage(" PRODNOTE not found!","");
      else {			/* just in case ... */
        x = strlen(v.prodnote);
        for (i=0; i<x-3; i++) {
          if (!strncmp( &v.prodnote[i], " //", 3)) {
	    zvmessage(" extra delimiter found in PRODNOTE ...","");
	    strcpy( buf, &v.prodnote[i+3]);
	    strcpy( &v.prodnote[i], buf);
	    x -= 3;
	  }
	}
        zladd( unit, "HISTORY", PRODNOTE, v.prodnote, "FORMAT", "STRING",NULL);
      }
    }
  }

  lptr = lptrq;
  lsef = find_keyword( "SPATIAL_BINNING_TYPE", bufpoint, &lptr, &lptre);
  if( lsef == 0 ) 
    zvmessage(" Keyword SPATIAL_BINNING_TYPE not found in cube label","");
  else {
    get_string_value( buf, 24, &lptr);
    if (!strncmp( buf, "FOOTPRINT_AVERAGE",17)) {
      zladd( unit, "HISTORY", BINNING, "FOOTPRNT",  "FORMAT", "STRING",NULL);
      lsef = find_keyword( "THRESHOLD_WEIGHT", bufpoint, &lptr, &lptre);
      if ( lsef == 0 ) 
        zvmessage(" Keyword THRESHOLD_WEIGHT not found in cube label","");
      else {
        get_real_value( &v.thresh, bufpoint, &lptr);
        zladd( unit, "HISTORY", THRESH, &v.thresh,  "FORMAT", "REAL",NULL);
      }
      lsef = find_keyword( "FOOTPRINT_GRID_SIZE", bufpoint, &lptr, &lptre);
      if ( lsef == 0 ) 
        zvmessage( " Keyword FOOTPRINT_GRID_SIZE not found in cube label","");
      else {
        get_integer_value( &v.fpgrid, bufpoint, &lptr);
        zladd( unit, "HISTORY", FPGRID, &v.fpgrid,  "FORMAT", "INT",NULL);
      }
      lsef = find_keyword( "MAXIMUM_PIXEL_DISTORTION", bufpoint, &lptr, &lptre);
      if ( lsef != 0 ) {
        get_real_value( &v.maxdistor, bufpoint, &lptr);
        zladd( unit, "HISTORY", MAXDISTOR, &v.maxdistor, "FORMAT", "REAL",NULL);
      }
    }
    else if (!strncmp( buf, "FOOTPRINT_HIST",14)) {
      if (!strncmp( buf, "FOOTPRINT_HISTOGRAM_MEDI",24))
        zladd( unit, "HISTORY", BINNING, "FTPTHSTM", "FORMAT", "STRING",NULL);
      else
        zladd( unit, "HISTORY", BINNING, "FTPTHSTP", "FORMAT", "STRING",NULL);
      lsef = find_keyword( "FOOTPRINT_GRID_SIZE", bufpoint, &lptr, &lptre);
      if ( lsef == 0 ) 
        zvmessage( " Keyword FOOTPRINT_GRID_SIZE not found in cube label","");
      else {
        get_integer_value( &v.fpgrid, bufpoint, &lptr);
        zladd( unit, "HISTORY", FPGRID, &v.fpgrid,  "FORMAT", "INT",NULL);
      }
      lsef = find_keyword( "MAXIMUM_PIXEL_DISTORTION", bufpoint, &lptr, &lptre);
      if ( lsef != 0 ) {
        get_real_value( &v.maxdistor, bufpoint, &lptr);
        zladd( unit, "HISTORY", MAXDISTOR, &v.maxdistor, "FORMAT", "REAL",NULL);
      }
    }
    else if (!strcmp( buf, "NEAREST_AVERAGE"))
      zladd( unit, "HISTORY", BINNING, "NEAREST",  "FORMAT", "STRING",NULL);
    else if (!strcmp( buf, "NEAREST_MAXIMUM"))
      zladd( unit, "HISTORY", BINNING, "MAXIMUM",  "FORMAT", "STRING",NULL);
    else if (!strcmp( buf, "NEAREST_REPLACE"))
      zladd( unit, "HISTORY", BINNING, "REPLACE",  "FORMAT", "STRING",NULL);
    else 
      zvmessage( " Invalid value for SPATIAL_BINNING_TYPE in cube label","");
  }

  lptr = lptrq;
  lsef = find_keyword( "SCAN_RATE_TOLERANCE", bufpoint, &lptr, &lptre);
  if( lsef == 0 ) 
    zvmessage(" Keyword SCAN_RATE_TOLERANCE not found in cube label","");
  else {
    lsef = get_real_value( &ritem, bufpoint, &lptr);
    ritem *= 0.001;	/* slew_tol is in mrad in isis label, rad in vicar */
    zladd( unit, "HISTORY", SLEW_TOL, &ritem, "FORMAT", "REAL",NULL);
  }

  lptr = lptrq;
  lsef = find_keyword( "CORE_NAME", bufpoint, &lptr, &lptre);
  *radtyp = 0;
  if( lsef == 0 ) 
    zvmessage( " Keyword CORE_NAME not found in cube label","");
  else {
    get_string_value( buf, 20, &lptr);
    if (!strcmp(buf,"RAW_DATA_NUMBER"))
      zladd( unit, "HISTORY", CAL_TYPE, "NOCAL", "FORMAT", "STRING", NULL);
    else if (!strcmp(buf,"SPECTRAL_RADIANCE")) {
      *radtyp = 1;
      zladd( unit, "HISTORY", CAL_TYPE, "RAD", "FORMAT", "STRING", NULL);
    }
    else if (!strcmp(buf,"RADIANCE_FACTOR")) {
      *radtyp = 1;
      zladd( unit, "HISTORY", CAL_TYPE, "RFACT", "FORMAT", "STRING", NULL);
    }
    else {
      zvmessage(" Unrecognized value for CORE_NAME, NOCAL assumed","");
      zladd( unit, "HISTORY", CAL_TYPE, "NOCAL", "FORMAT", "STRING", NULL);
    }
  }

  lptr = lptrq;
  lsef = find_keyword( "NATIVE_START_TIME", bufpoint, &lptr, &lptre);
  if( lsef == 0 ) 
    zvmessage(" Keyword NATIVE_START_TIME not found in cube label","");
  else {
    get_string_value( buf, 16, &lptr);
    i = sscanf( buf, "%d.%d.%d", &ival[0], &ival[1], &ival[2]); /* RIM.MF.RTI */
    if (i==2) ival[2] = 0;
    zladd( unit, "HISTORY", BEG_SCLK, &ival, "FORMAT", "INT", "NELEMENT", 3, NULL);
  }

  lptr = lptrq;
  lsef = find_keyword( "NATIVE_STOP_TIME", bufpoint, &lptr, &lptre);
  if( lsef == 0 ) 
    zvmessage(" Keyword NATIVE_STOP_TIME not found in cube label","");
  else {
    get_string_value( buf, 16, &lptr);
    i = sscanf( buf, "%d.%d", &ival[0], &ival[1]); 	/* RIM.MF */
    zladd( unit, "HISTORY", END_SCLK, &ival, "FORMAT", "INT", "NELEMENT", 2, NULL);
  }

  lptr = lptrq;
  lsef = find_keyword( "CHOPPER_MODE_ID", bufpoint, &lptr, &lptre);
  if( lsef == 0 ) 
    zvmessage( " Keyword CHOPPER_MODE_ID not found in cube label","");
  else {
    get_string_value( buf, 11, &lptr);
    if (!strcmp( buf, "'63_HERTZ'")) i = 0;
    else if (!strcmp( buf, "REFERENCE")) i = 1;
    else if (!strcmp( buf, "OFF")) i = 2;
    else if (!strcmp( buf, "FREE_RUN")) i = 3;
    else {
      zvmessage( " Invalid value for CHOPPER_MODE_ID in cube label","");
      lsef = 0;
    }
  }
  if (lsef) zladd(unit,"HISTORY",CHOPPER_MODE,&i,"FORMAT","INT",NULL);

  /* these all depend on Pos.Long.Direction: */

  lptr = lptrq;
  lsef = find_keyword( "START_SUB_SPACECRAFT_LONGITUDE",bufpoint,&lptr,&lptr2);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", B_SSCLON, &ritem, "FORMAT", "REAL",NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "STOP_SUB_SPACECRAFT_LONGITUDE",bufpoint,&lptr,&lptr2);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", E_SSCLON, &ritem, "FORMAT", "REAL",NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "START_SUB_SOLAR_LONGITUDE",bufpoint,&lptr,&lptr2);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", B_SSOLLON, &ritem, "FORMAT", "REAL",NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "STOP_SUB_SOLAR_LONGITUDE",bufpoint,&lptr,&lptr2);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", E_SSOLLON, &ritem, "FORMAT", "REAL",NULL);
  }

  /* do the band_mask item for phase 2: */
  if (mphase) {
    lptr = lptrq;
    lsef = find_keyword( "INSTRUMENT_MODE_ID", bufpoint,&lptr,&lptr2);
    get_string_value( v.nimsmode, 11, &lptr);
    if (!strncmp(v.nimsmode, "FIX", 3)) {	/* FIXED GRATING */
      nbm = 17;  /* Number of wavelengths = 17      */
    }
    else if (!strncmp(v.nimsmode, "BAND", 4)) {	/* BANDEDGE */
      nbm = 34;  /* Number of wavelengths = 34      */
    }
    else if (!strncmp(v.nimsmode, "SHORT", 5)) {	/* SHORT MAP or SPECT */
      nbm = 102;  /* Number of wavelengths = 102     */
    }
    else if (!strncmp(v.nimsmode, "FULL", 4)) {	/* FULL MAP OR SPECT */
      nbm = 204;  /* Number of wavelengths = 204      */
    }
    else if (!strncmp(v.nimsmode, "LONG", 4)) {	/* LONG MAP OR SPECT */
      nbm = 408;  /* Number of wavelengths = 408      */
    }  
    else if (!strncmp(v.nimsmode, "SPECI", 5)) {	/* SPECIAL SEQUENCE */
      nbm = 102;  /* Number of wavelengths */
    }
    else if (!strncmp(v.nimsmode, "SAFE", 4)) {	/* SAFE MODE */
      nbm = 17;  /* Number of wavelengths */
    }
    else
      zmabend("** unable to determine mode! **");
    /* if # bands is nominal, no band_mask needed */
    if (nbm!=nbb) {
      /* this item will occur after ins.mode in the label: */
      lsef = find_keyword( "BAND_BIN_ORIGINAL_BAND", bufpoint,&lptr,&lptr2);
      if (!lsef) zmabend(" cannot find BAND_BIN_ORIGINAL_BAND");
      while(bufpoint[lptr]!='(') {
	lsef = incrm_check_buffer(&lptr);
	if (!lsef) break;
      }
      count = 0;
      while ( bufpoint[lptr]!=')' ) {
	lsef = get_integer_value( &b.org_band[count++], bufpoint, &lptr);
	if (!lsef) break;
      }
      if (count!=nbb) zmabend(" error reading BAND_BIN_ORIGINAL_BAND");
      for (i=0; i<nbm; i++) bmask[i] = 0;
      for (i=0; i<nbb; i++) bmask[b.org_band[i]-1] = 1;
      zladd( unit, "HISTORY", B_MASK, bmask, "NELEMENT", nbm, "FORMAT", "INT",
       NULL);
    }
  }

  /* do these last, so they are adjacent to min/max latitude ... */
  lptr = lptrq;
  lsef = find_keyword( "EASTERNMOST_LONGITUDE", bufpoint,&lptr,&lptr2);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", MIN_LON, &ritem, "FORMAT", "REAL",NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "WESTERNMOST_LONGITUDE", bufpoint,&lptr,&lptr2);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", MAX_LON, &ritem, "FORMAT", "REAL",NULL);
  }

  return 1;
}
