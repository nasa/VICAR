/*
 *	program NIMSCMM2
 *
 * This program (which runs under the VICAR executive) produces NIMS 
 * merged mosaics for Phase-2 operations.
 *
 * 26nov94 --lwk-- initial version, converted from Alpha-ported NIMSCMM
 * 08feb95 --lwk-- added Sybase catalog interface;  removed code to read
 *		temperatures from EDR label
 * 16mar95 --lwk-- fixed zvwrit() calls in Tube case
 *  5sep95 --lwk-- fixed some bugs in C-bridge calls
 *  2mar96 --lwk-- changed zreset() to zreset1()
 *  1may96 --lwk-- added recno[2] to mf[] array;  BigMalloc mf[] based on
		actual # of records, not max.possible
 * 14may96 --lwk-- skip tube backplanes for GPs that have been deselected;
 *		added in recent changes to NIMSCMM: min/max.long. algorithm,
 *		cur_edr tracking, print out map projection data & image size,
 *		use Planck to interpolate Solar flux, fix B_AXIS_RADIUS;
 *		check Target vs. ObsTab;  look at all mp's for photometric
 *		angles, not just extrema
 * 28jun96 --lwk-- don't omit MFs at gaps when slew-checking turned off;
 *		fixed check on Target in ObsTab;  initialize array returned
 *		by nims_comp_rad;  fix confusion between MF and HMF counts
 *		in assemble_pntng;  fixed bugs in output_tube and gpos mask;
 *		don't include wait states in cotube
 *  8jul96 --lwk-- use GMY's LOAD_SPICE95 (in-line) to return SP Kernel id
 * 30jul96 --lwk-- update maxclk/hiclk in assemble_pointing 
 *  2aug96 --lwk-- fixed problems with AACSFILE headers, new mpbuf2mpo,
 *		& detector mask bit order; make cone/xcone consistently
 *		in degrees; fixed bug in znimsboom bridge
 *  6aug96 --lwk-- write out RAD_BASE, etc., arrays with length NB ...
 *		only BAND_MASK should use NBB;  fixed accounting for wait
 *		states and wavelength array with WET;  apply 90-deg. twist 
 *		fix to AACSFILE; fiddled withg POV parameter specification 
 *		(again);  fixed bugs in check_thresh & processing multiple
 *		EDRs
 * 17aug96 --lwk-- fixed slew rate check to account for MF gaps due to 
 *		deselected wavelengths for which pointing exists
 * 22aug96 --lwk-- allow NORTH only to be specified in POV
 * 26aug96 --lwk-- fix sensitivities, etc., in label to reflect WET selection,
 *		fixed dark value processing
 * 27aug96 --lwk-- NORTH only *cannot* specified in POV!!
 * 26sep96 --lwk-- fix planetocentric/detic latitude processing so that
 *		whichever is specified is used consistently thru-out (TRANV
 *		is centric only, so it is converted right before/after call)
 * 30sep96 --lwk-- fixed bug in deselected-band processing
 * 04oct96 --lwk-- check for DNs set to special values (1-4) by phase-2 s/w
 * 07oct96 --lwk-- corrected error in centric/detic latitude processing for
 *		non-Perspective cases
 * 10oct96 --lwk-- added PHOTCUT, INCCUT, EMICUT parameters
 * 12oct96 --lwk-- fixed problem with cylindrical long_cut check when
 *		longitude=180
 * 26oct96 --lwk-- apply gpos[] array to weights cube too (when all detectors
 *		in a GP are deselected)
 * 31oct96 --lwk-- replaced BINPOOL load with initspice() per GMY changes;
 *		added option for multiple CAL files (for different Gains);
 *		added min/max central-body distances per RM request
 * 03nov96 --lwk-- added PSHIFT and THRESHVAL label items per RM request
 * 12nov96 --lwk-- revised find_body_motion to look only at SCLK range of
 *		observation & to return mean ra/dec instead of only total;
 *		enabled NAIF error reporting;  added extra call to 
 *		nims_ps_orient at start of assemble_pntng to avoid DAFRFR 
 *		i/o error in spkapp (unknown provenance!);  disabled
 *		Fortran WRITE(6) calls in NIMS_CAL.F
 * 13nov96 --lwk-- if DN=4 & thresholding was used then set DN to INS_LO_SAT
 * 20nov96 --lwk-- fixes in assemble_pntng:  took slant distance into account 
 *		in body motion;  changed SCLK bounds check since there can be
 *		gaps in phase-2 EDRs
 * 05dec96 --lwk-- fixed problem with non-zero-terminated ObsTab target name;
 *		tightened up 'nochk' case in assemble_pntng
 * 08dec96 --lwk-- call NIMSBOOM at start of job to avoid problems with 
 *		GETLUN
 * 11dec96 --lwk-- rewrote assemble_pntng to look at pointing data only, not
 *		EDRs;  removed recno[2] from mf[] structure;  moved all checks
 *		on EDR data contents to extract_data (this includes
 *		determination of tube line dimension)
 * 17dec96 --lwk-- fixed boom angles interpolation at last point;  tube NL
 *		must be updated by zldel/add, not zvclose/open
 * 30dec96 --lwk-- ensured dbm=0 with SPICE C-kernel
 * 10jan97 --lwk-- disable body-motion correction for boresight when R/D<0.1
 * 11jan97 --lwk-- add check for bad SCLK2 in de-garbled EDRs
 * 20jan97 --lwk-- fixed computation of NL (fm estimate of NGCS) for Tube
 * 29jan97 --lwk-- fixed computation of NL for Tube *again*:  do *not* just
 *		change the label, as this messes up file organization!
 *		also prevent overwriting INS_LO_SAT with NULLs
 *  4feb97 --lwk-- added DPT_SCLK parameter
 * 17feb97 --lwk-- final fix for computation of NL for Tube, by adding a 
 *		first pass thru the data just to count GCs
 * 20feb97 --lwk-- fixed body motion computation as an average over entire 
 *		observation (for near-range only);  don't throw out records
 *		with missing pointing or WAIT state for tube (just fill
 *		backplanes with NULL)
 * 24feb97 --lwk-- fixed emission and phase angles for ellipsoidal case (they
 *		had assumed spherical body!);  corrected COORDINATE_SYSTEM
 *		_NAME in Vicar label
 * 28feb97 --lwk-- the rewrite of assemble_pntng (11dec96) did not allow
 *		EDRs to be input out of SCLK order when find_body_motion
 *		is called ... this has now been fixed
 *  5mar97 --lwk-- make sure that sub-spacecraft & -solar points are always
 *		computed (were being skipped in some cases)
 * 10mar97 --lwk-- for tube, force BEG_SCLK to be at start of grating cycle
 * 13mar97 --lwk-- make sure that lat/long are only written to cotube for
 *		on-planet pixels (altitude=0);  fixed bug in footprint case
 *		whereby altitude was not being averaged properly (value
 *		written was about 4x too small)
 * 29mar97 --lwk-- switch cone/x-cone rotmat's of C-matrx in get_tgt_data,
 *		as check that this has no effect
 * 14may97 --lwk-- added right-edge line/samp backplanes to cotube for footprint
 * 27may97 --lwk-- added Lommel-Seeliger photometric correction;  changed
 *		PHOTCUT to make default depend on target
 * 12jun97 --lwk-- changed long_cut to longitude-180 (stat +180) for case when
 *		tiepoint is user-specified for Cylindrical projection
 * 14jun97 --lwk-- check for gaps in user-specified sclk intervals and for
 *		compression status flag in EDR;  fix misleading comments about
 *		flux units in fcn. solar()
 * 15jul97 --lwk-- fixed normalization of u_sun vector in computation of 
 *		incidence angle
 * 25jul97 --lwk-- put in check for nb=0 due to bad choice of LAMI;  make
 *		'nochk' option only depend on slew_tol<0, not twt_tol;
 *		don't skip "special tests" for tube MFs that have been
 *		rejected on basis of pointing!
 * 28aug97 --lwk-- fixed obscure code bug in output_near() for BIN=MAX case
 *  2sep97 --lwk-- initialize userInfo.passwd
 * 21sep97 --lwk-- removed RADII label item, since MP supports triaxial (unlike
 *		MAPLABV2);  fixed bug in check_thresh (only looked at pix 1,1!)
 * 28sep97 --lwk-- added SUPPNOTE
 *  6oct97 --lwk-- added CALIBRATION as a Target name (substitute JUPITER in
 *		call to SPICE), with hack to output_cotube 
 * 14oct97 --lwk-- correct error in # of backplanes for Fixed-mode tubes
 * 24oct97 --lwk-- fixed DPT_SCLK parameter to work with current structure of
 *		assemble_pntng
 * 29oct97 --lwk-- fixed initialization of altitude backplane, to prevent 
 *		off-planet tube pixels from having zero altitude
 *  3nov97 --lwk-- added TEST=6 option;  fixed treatment of pointing near gaps
 *		so that RA=-999 doesn't get used in get_tgt_data;  fixed
 *		interpolation in get_pntng:  need to rewind aacsfile;  added
 *		checks for wraparound at 360deg. in several fcns;  changed PDF
 *		default to CEXTRAP;  set BELOW_THRESH & NO_SENSITIVITY to NULL 
 *		for Unix ISIS
 * 17nov97 --lwk-- fixed bug causing cotube lat/lons to be all NULL:  rads[1]
 *		was being set to NULL, which is off-planet flag;  removed
 *		long_cut stuff for Cylindrical wraparound problem, instead
 *		find gaps in min/max_samp determination and reset tiepoint to 
 *		Samp=1 as in subr. RECTPATCH;  also correct neg.samps returned 
 *		from TRANV (internal only, not in MPlabs)
 * 18nov97 --lwk-- correct for wraparound inside single-pixel footprint in
 *		find_xy_corners (for Simp.Cyl. only!)
 * 26nov97 --lwk-- added MAXDSTOR parameter;  added code for despiking;
 *		removed spurious "offimage" count in output_ftpt when entire
 *		footprint falls into one pixel 
 *  9dec97 --lwk-- fixed despiking code
 * 16dec97 --lwk-- fixed code setting NO_SENSITIVITY4 to NULL4:  wasn't working
 *		because forgot to cast to (float)
 * 17dec97 --lwk-- use 'fgetc' to read in despike file
 * 24jan98 --lwk-- treat LO_SAT same way as HI_SAT in output_near;  also minor 
 *		fix to bm_mra correction to dra
 *  1feb98 --lwk-- allow despike files to have variable # of header records
 *  4feb98 --lwk-- remove merge function from read_dspk, make pgm. SPIKECON
 *		do this;  allow for multiple spike entries at same pixel; 
 *		fixed bug in chop count for calibration file;  write chop 
 *		count for "invalid" tube combs
 *  5feb98 --lwk-- correct user's TIELAT when resetting it to 0 for Cylindrical
 * 12feb98 --lwk-- added another fiddle to tielon determination in 
 *		set_projection for Cylindrical;  added J_RINGS as special
 *		target (= Jupiter for NAIF)
 * 14feb98 --lwk-- added option for multiple Dark files (NIMS98 calibration)
 *		and despike files;  revised spike file label processing (for
 *		PDS label);  added filetype to PRODID
 * 17feb98 --lwk-- fixed up min/max lat/long values for catalog; fixed bug
 *		in processing of multiple spike files
 * 23feb98 --lwk-- added TEST=6 option to set_pov()
 *  2mar98 --lwk-- ensure that loclk always starts on a GC boundary for
 *		tube (previous algorithm failed when loclk > minclk);
 *		fix error in min/max lon determination in set_projection;
 *		ensure that min/max lat/lons are the same whether or not
 *		catalog is updated
 *  4mar98 --lwk-- fix another bug in multiple spike files;  fixed bug in
 *		initialization of values written to tube Line/Samp backplanes
 *		for unprojected pixels;  corrected error in min/max fix of 2mar
 *  6mar98 --lwk-- ensure that MIRROMIT, INCCUT, EMICUT are only used for GCUBE;
 *		skip all projection stuff for CALIBRATION
 *  8mar98 --lwk-- allow for deviation of dark values from mean in evaluating
 *		spikes (param. drk_tol)
 * 13mar98 --lwk-- set photometric angles to -999 rather than NULL4 if they
 *		cannot be found;  tighten up find_angles() so it does not miss
 *		points at the edges;  fixed error in find_angles() for offset
 *		of angles in footprint tube
 * 15mar98 --lwk-- allow user override of PSHIFT
 * 18mar98 --lwk-- make sure that North is labelled as undefined in projections
 *		that don't use it
 * 19mar98 --lwk-- added parameter SKIPDET
 * 25mar98 --lwk-- print out min/max lat/longs (to help nsysproc)
 *  5apr98 --lwk-- corrected bug in previous fix to PSHIFT (in nims_cal.f)
 *		that resulted in bad label item when this was defaulted
 *  6apr98 --lwk-- put in check for zero delta-time in find_body_motion;
 *		don't require dummy AACS file for Calibration data;  write
 *		solar & cenbody distances to label for Cal. data
 * 20apr98 --lwk-- for determination of longitude extent for Cylindrical case
 *		in set_projection, make histogram use finer grid
 * 30apr98 --lwk-- allow SKIPDET to include >1 detector;  make sure that all 
 *		pixels in det's specified by SKIPDET are indeed set to NULL
 * 12may98 --lwk-- correct error in equations converting between planetocentric
 *		& detic;  also, dlat[] in geobuf() was always centric, even
 *		if graphic was requested -- now fixed
 * 14may98 --lwk-- changes due to renaming of CALIBRATION targets to CAL/SKY
 *  6jun98 --lwk-- added PTUBE option
 * 10jun98 --lwk-- write PTUBE backplanes regardless of whether off-planet;
 *		store Euler angles *after* rotating C-matrix for IK values
 *  6jul98 --lwk-- don't give tube/cotube different names if PTUBE
 * 15jul98 --lwk-- ensure that SKIPDET also applies to DN tube -- to do
 *		this, rearranged spaghetti code in calibration step of
 *		extract_data() to make it (slightly) more structured
 * 30jul98 -lwk- removed "missing AACS" from deboom count message (dcnt);  
 *		fixed fix of mar98 to min/max lon determination which ignored 
 *		possible presence of zero meridian in image;  also ensure
 *		that only regions with data are searched for min/max lat/lon
 *		in proj's other than POV/Ortho
 *  3aug98 -lwk- made more mods to min/max lon determination to cover cases 
 *		where pole is in image and where planet is tilted
 *  5aug98 -lwk- moved checks for thresholding & pixels below valid_min to
 *		before despiking
 *  7aug98 -lwk- changed condition for setting Tube long. range to (-180,180)
 *		from 0<ssclon<180 to 90<ssclon<270;  allow refining search
 *		in write_latlon even if zero meridian in image by converting
 *		long's to (-180,180) range;  include POV/Ortho when searching
 *		regions with data;  fixed bug in min/max lon determination in
 *		set_projection, but disable it by reducing cutoff to 0.5;
 *		removed ERT items from label & code (they were garbage, code 
 *		treated them as in phase-1 tho fields had changed);
 * 12aug98 -lwk- totally rewrote min/max determination in write_latlon using
 *		LJR algorithm
 * 15aug98 -lwk- apply LJR algorithm to min/max lat's too, as this is more
 *		consistent with the backplanes than the determination in
 *		geobuf();  added loop over bands to this to cover case where
 *		first band(s) is (are) empty (e.g., G7ENFLEXUS01C)
 * 17aug98 -lwk- check *all* bands for valid data in min/max lat/lon det'n
 *		in write_latlon, not just first one
 * 27aug98 -lwk- don't try to write PTUB backplanes for unprojectable
 *		pixels, as the 'cobuf' isn't filled
 * 17sep98 -lwk- added LO_SAT parameter to allow disabling of low instrument
 *		saturation flag for thresholded DNs
 * 22sep98 -lwk- change treatment of thresholded pixels when low-sat flag
 *		is set:  average them in as zero radiance in output_near;
 *		expand weights file to include a band for each cube band, not
 *		just for each gp (!)
 * 23oct98 -lwk- fixed problem where 'nochk' interferes with processing of
 *		multiple SCLK intervals
 *  6nov98 -lwk- changed check in output_ftpt to avoid errors when differencing
 *		two numbers that are very close
 *  4feb99 -lwk- added Normal Cylindrical projection
 * 10feb99 -lwk- add pseudo-mirror variable for tube format in spectrometer mode
 * 14feb99 --lwk-- added AINFL parameter
 * 27feb99 --lwk-- fixed bug in find_angles that causes error in photometric
 *		angles in label when cube/tube NL=1
 *  8apr99 --lwk-- added support for WDS-type spike files, which just zero
 *		the DN
 * 26apr99 --lwk-- allow for wobble to be added to pointing
 * 17may99 --lwk-- fix counter for skipped combs in extract_data()
 * 18may99 --lwk-- corrected wobble model to include effect on Twist
 * 24may99 --lwk-- revised wobble model to make it always clockwise (removed
 *		param. WMODE), added option for ang.-mom. phase angle
 *		(WFREQ=0)
 *  3jun99 --lwk-- added param. B_E_MP to fix problem of bad tube latitude
 *		limits when mirror blocking used;  added DRK_AVE to label
 *		when lami>0
 *  8jun99 --lwk-- added check for invalid pixels replaced by spike file
 * 20jul99 --lwk-- fixed treatment of data gaps in assemble_pntng:  read in
 * 		pointing for these, so that adjacent valid data aren't
 *		discarded;  added compression status check parameter
 * 21jul99 --lwk-- write min/max photometric angles to log (should write
 *		to label too, but that requires PDS ok)
 * 11aug99 --lwk-- fixed sign error in X-Cone wobble offset for the WF>0 case
 * 03sep99 --lwk-- fixed bug in mp loop limits (<empos instead of <=empos) in
 *		several places (introduced 3jun99)
 * 27sep99 --lwk-- fixed bug in mp loop limits in output_test
 * 18oct99 --lwk-- added "Fixed Long" mode (insmode=13) for I24 problem
 *		(assume that WET selects all GPs in detectors used!)
 * 21oct99 --lwk-- added SATTHRSH parameter
 *  5dec99 --lwk-- added BB_REPL option;  closed all files opened with fopen
 *		when no longer needed, moved zet2utc calls to start of pgm,
 *		other changes in attempt to fix crash in zet2utc call, which
 *		turned out to be system problem
 * 21jan00 --lwk-- added NSKIP_GP parameter for I27 tests
 *  6feb00 --lwk-- added histogram-binning option (param. HBINSIZE)
 * 15feb00 --lwk-- changed format argument in zet2utc calls per new SPBRI
 * 20feb00 --lwk-- fixed treatment of histograms with only invalid pixels in
 *		hist-bin option
 * 23feb00 --lwk-- added MAX_REPL option for saturated pixels, as a hack
 *		for J.Shirley case
 * 24feb00 --lwk-- added SC_GAPS & SATTHRSH label item
 * 20mar00 --lwk-- fixed code processing WET for Fixed Long mode with
 *		not all GPs selected
 * 16mar00 --lwk-- fixed treatment of invalid DNs in histbin option
 * 02jul00 --lwk-- compute default SATTHRSH using scale;  revised
 *		output_near again for saturated case;  revised hist2cube
 *		to write lo/hi_sat when an overflow bin is peak
 * 03jul00 --lwk-- added code to fix histogram from 7-1023 when Hbins=510
 *		& Hstep=2;  don't write threshold parameters to label
 *		when hist-binning
 * 06sep00 --lwk-- fixed phase angle computation to use LOS, not lat/lon
 *		(makes a difference for off-target data, e.g. ring!)
 * 15sep00 --lwk-- don't override user-specified gstart with ptab value
 *  8oct00 --lwk-- don't convert Det->Cen before TRANV call if Simp.Cyl.
 *		with Planetograhic, since this projection is independent
 *		of detic/centric! (also no Cen->Det after TRANV in
 *		write_latlon)
 * 13oct00 --lwk-- fixed treatment of last point in assemble_pntng (was
 *		included even if slew check should throw it out)
 * 28oct00 --lwk-- print out line #s of compression status errors when
 *		NOCMPCHK for tube
 * 11nov00 --lwk-- revised wobble model again, to make it always 
 *		counterclockwise (per insight of different AACS/NIMS
 *		definitions of X-cone rotation sense)
 * 18nov00 --lwk-- added TEST=8,9 options;  extended fix of 11nov to
 *		case where wobble is fcn. of clock (overlooked!)
 * 24nov00 --lwk-- fiddled some more with wobble model:  undid change
 *		of 18nov, as rotation by '*pxcone' is around +M, so it
 *		needs to be negated to be consistent with NIMS def'n;
 *		also fixed twist wobble component
 * 13dec00 --lwk-- changed slant backplane in POV projection to be
 *		defined by projection POV, not that at instant of obs'n
 * 17mar01 --lwk-- added check for "twisted pixel" before output_ftpt
 *		(first instance in 29JNAURORA01Q)
 * 31mar01 --lwk-- limited "twisted pixel" msg to first instance
 *  2apr01 --lwk-- fixed check for "twisted pixel" using LJR algorithm
 *  6apr01 --lwk-- added OLD_VER parameter to control whether or not
 *		fixes of 8oct00 & 13dec00 are used (determined by INITIALS
 *		by default)
 * 22apr01 --lwk-- added TEST=10 option
 * 27apr01 --lwk-- removed code setting nchops=0 for ngcs=1 (!)
 *  1may01 --lwk-- added a check to return status of a get_pntng() call, to
 *		avoid a crash when no pointing at start of an EDR
 *  4jul01 --lwk-- fixed problems with gpos & wet arrays for MPW Fixed Long
 *		mode observations
 *  9jul01 --lwk-- fixed problem with slant distance for tubes (was omitting
 *		if not oldver, but write_latlon is not called for tubes!)
 * 13jul01 --lwk-- fiddled with code for WET again, which was broken for
 *		Fixed-Map (mode 7) case;  made 7 and 13 equivalent, and
 *		revised hist2cube() to account for this
 * 23aug01 --lwk-- added correction for stellar aberration to spkapp calls
 *		(per email of njb to gmy)
 * 27sep01 --lwk-- fixed egregious bug in hist2cube
 * 30sep01 --lwk-- made SKIPDET tests more rigourous
 *  1dec01 --lwk-- added option for Std.Dev. cube;  this revealed bug in wt
 *		in sdband calc'n -- fixed
 *  2dec01 --lwk-- put in extra 1/sqrt(WT) for Std.Dev. cube, to convert
 *		s.d. of ensemble to s.d. of individual measurement
 *  4dec01 --lwk-- added Fixed Short & Full modes
 * 15feb02 --lwk-- made WCONE required when no aacsfile
 * 29mar02 --lwk-- fixed another bug in SKIPDET check
 * 22jul02 --lwk-- allow TIELAT specification in Polar proj'n (since in
 *  6aug02 --lwk-- added RM's new routines to read ascii cal/dark files:
 *		nims_get_cal_a.for & nims_get_dark_a.for
 *  9aug02 --lwk-- fixed code for slant/height in output_near;  allowed
 *		logical/env.var. for cal/dar file name
 * 10aug02 --lwk-- added parm ASC_CAL since env.vars don't work in Unix
 * 12aug02 --lwk-- replaced def'ns of ISIS special values with code
 *		taken from $ISISINC/special_pixel.h
 * 31aug02 -lwk- added ISIS special values to nims_set_rad call
 *  7sep02 -lwk- fixed bug passing double to float in zpproj
 * 20sep02 -lwk- changed slew_rate, slew_tol, tst_tol from double to float
 * 26jun04 -lwk- renamed PPROJ.F to NPPROJ to avoid picking up p2sub routine
 * 26jul04 -lwk- fixed xparm call for PDIST to xvparmd
 *  1apr07 -lwk- removed catalog code since this no longer exists;  added other
 *		minor code fixes done on Speedy
 *  4jun08 -lwk- fixed minor problem with sclkmax,sclkstrt
 *  6jun08 -lwk- allow user override of radii in kernels
 *  8jun08 -lwk- included code from obsoleted *.h and gll_nims_bin_ph2.c modules
 * 18jun08 -lwk- removed OUTORG,OBSNAM,OBSEXT,MOSNUM,PRODID,INITIALS,OLD_VER, made
 *		PHASE optional
 * 19jun08 -lwk- fixed format of slew_rate, slew_tol in zladd (apparently change
 *		of 20sep02 was not made)
 * 21aug08 -lwk- fixed bug whereby gain was not read from EDR when user specified
 *		mode (e.g., Fixed Long Map)
 * 12jan10 -lwk- fixed ANOTHER bug passing double to float in zpproj (in set_projection)
 * 19dec11 -lwk- fixed bug processing EDR gaps in extract_data()
 * 09jan12 -lwk- replaced get_body_ids() with call to zpbid(), since the env.var. BODY_IDS
 *		no longer exists
 * 10jan12 -lwk- use default north=0 in set_projection() (caused error when not 
 *		compiled debug)
 * 11jan12 -lwk- fixed bug in znims_get_dark_a and xnims_get_dark_a that caused
 *		failure on Linux
 */

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "rts_typedefs.h"

/* the following include files were obsoleted and have been included
   explicitly below:
#include "gll_rts_main.h"
#include "gll_rts_main_diff.h"
#include "gll_lrs.h"
#include "gll_nims_edr_ph2.h"
*/

#include "spiceinc.h"
#include "nims_label2.h"
#include "mp_routines.h"
/*#include "gll_nims_cmm_cat.h"		NO CATALOG */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "vicmain_c"					/* Vicar */

#define SUCCESS         1	/* need this from gll_catalog.h */

/****************************************************************************/
	/* GLOBAL DEFINITIONS FOR NIMSCMM */

	/* first, the contents of some obsoleted include files: */

/*				GLL_RTS_MAIN.H
 ******************************************************************************
 *	This file includes the basic data structures for the Galileo
 *	Phase II telemetry records.
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 *  20- 6-1989	N/A		Damon Knight - Original Delivery
 ******************************************************************************
 */

/*
 *=============================================================================
 *	Definitions
 *=============================================================================

#define	BOOM_FLAG_YES		0
#define	BOOM_FLAG_MAYBE		1
#define	BOOM_FLAG_NO		2
#define BOOM_FLAG_UNKNOWN	3

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
	UINT	rim;				/* Real time image count */
	UBYTE	mod91;				/* mod 91 counter	 */
	UBYTE	mod10;				/* mod 10 counter	 */
	UBYTE	mod8;				/* mod 8 counter	 */
	}
	sclk_typ;

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

typedef	ert_typ		rct_typ;


typedef	struct
	{
	UINT	rim;				/* Real time image count */
	UBYTE	mod91;				/* mod 91 counter	 */
	UBYTE	mod10;				/* mod 10 counter	 */
	}
	pws_sclk_typ;

typedef pws_sclk_typ    nims_ph2_sclk_typ;

typedef	struct
	{
	UWORD			year;		/* YEAR 		     */
	UBYTE                   month;          /* MONTH OF YEAR             */
        UBYTE                   day;		/* DAY OF MONTH              */
	UBYTE			hour;		/* HOUR OF DAY		     */
	UBYTE			minute;		/* MINUTES OF HOUR	     */
	UBYTE			second;		/* SECONDS OF MINUTE	     */
	UWORD			msecond;	/* MILLISECOND OF SECOND     */
	}
	pws_ert_typ;

typedef	pws_ert_typ		pws_scet_typ;
typedef	pws_ert_typ		nims_ph2_ert_typ;
typedef	pws_ert_typ		nims_ph2_scet_typ;

#define JAN  0
#define FEB  31
#define MAR  28
#define MAR1 29
#define APR  31
#define MAY  30
#define JUN  31
#define JUL  30
#define AUG  31
#define SEP  31
#define OCT  30
#define NOV  31
#define DEC  30

static short int day_tab[2][12] =
{{JAN,
  JAN+FEB,
  JAN+FEB+MAR,
  JAN+FEB+MAR+APR,
  JAN+FEB+MAR+APR+MAY,
  JAN+FEB+MAR+APR+MAY+JUN,
  JAN+FEB+MAR+APR+MAY+JUN+JUL,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV,
  JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC},
 {JAN,
  JAN+FEB,
  JAN+FEB+MAR1,
  JAN+FEB+MAR1+APR,
  JAN+FEB+MAR1+APR+MAY,
  JAN+FEB+MAR1+APR+MAY+JUN,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV,
  JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC},
};

/*		            GLL_RTS_MAIN_DIFF.H
 ******************************************************************************
 *	This file includes the basic data structures that are necessary
 *      for GLL Phase I compatability. 
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 * 23- 10-1994   N/A            Original Delivery - Damon D. Knight
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


/* DELETED NON-NIMS PARTS OF THIS FILE <LWK - 08JUN2008> */
/*				GLL_LRS.H
 ******************************************************************************
 *
 * Low Rate Science Frame, definitions and macros.
 *
 * NOTES:
 *	This file uses some symbols & structures defined in MAIN_GLL.H
 *	All lengths are given in bytes unless stated otherwise.
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 *  7- 7-1989	N/A		Payam Zamani - Original Delivery
 ******************************************************************************
 */

/* AACS_LRS_TYP
 *=============================================================================
 * AACS Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UWORD		rotor_ra;	/* Rotor Attitude Right Ascension    */
	UWORD		rotor_dec;	/* Rotor Attitude Declination	     */
	UWORD		rotor_twist;	/* Rotor Attitude Twist		     */
	UWORD		pltfrm_ra;	/* Platform Att.  Right Ascension    */
	UWORD		pltfrm_dec;	/* Platform Att.  Declination	     */
	UWORD		pltfrm_twist;	/* Platform Att.  Twist		     */
	UWORD		pltfrm_cone_rate;	/* Platform rate, cone	     */
	UWORD		pltfrm_x_cone_rate;	/* Platform rate, cross cone */
	UWORD		rotor_spin_delta;	/* Rotor Spin Motion Delta   */
	UWORD		rotor_spin_pos;		/* Rotor Spin Position angel */
	UWORD		cone_pos;		/* Cone Position, 1/2**16th  */
	UWORD		clock_pos;		/* Clock Position	     */
	}
	aacs_lrs_p_typ;

/* NIMS_LRS_TYP
 *=============================================================================
 * Near Infrared Mapping Spectrometer subsystem, Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		dsae[3];	/* Digital Status & Analog Engnrng   */
	}
	nims_lrs_p_typ;


/*		GLL_NIMS_EDR_PH2.H
 *
 * This is the include file defining the NIMS phase-2 UDR/EDR structure.
 *
 */

/* Include Files */
#include "rts_typedefs.h"

/* Error Return Status */
#define NO_NIMS_EDR_HEADER     77777777     /* No NIMS EDR/UDR header present */
#define NO_NIMS_PDS_HEADER     77776666     /* No NIMS PDS header present */

/* General Defines */
#define NIMS_EDR_RECORDS     2		/* # NIMS EDR/UDR header records */
#define NIMS_RECORD_LENGTH   1024	/* NIMS record length */
#define NIMS2_BYTE_PREFIX_SZ  344	/* NIMS record binary prefix size */
#define NIMS_MAX_LINES       364        /* Number of NIMS UDR Lines */

/* Be careful of the following as this should be number lines binary PDS */
#define NIMS_PDS_RECORDS     3      /* Number of NIMS PDS header records */

/* enable this when the ph.2 EDR has been "registered" with VICAR: */
/*#define GLL_NIMS2_EDR "GLL_NIMS2_ED" /* BLTYPE for NIMS ph.2 EDR */

/* typedef unsigned long   ULONG; */

/*
 *=============================================================================
 * Spacecraft clock, used by NIMS.  This structure differs from the real SCLK,
 * in that NIMS does not use MOD-8 counters.
 *=============================================================================
 */
typedef	struct
	{
	ULONG	rim;	   	/* Major frame (Real-time IMage) counter */
	UBYTE	mod91;		/* Minor frame (MOD-91) counter */
	UBYTE	rti;		/* RTI (MOD-10) counter	     */
	}
	nims2_sclk_typ;

/*
 *=============================================================================
 * NIMS Earth-received time format
 *=============================================================================
 */
typedef	struct
	{
	UWORD	year;
	UBYTE	month;
	UBYTE	day;
	UBYTE	hour;
	UBYTE	minute;
	UBYTE	second;
	UWORD	millisecond;
	}
	nims2_ert_typ;
/*
 *=============================================================================
 *  First NIMS UDR file header (1st header after PDS label in EDR)
 *  NOTE:  certain parts of this structure differ between the EDR and UDR, 
 *  but the same definition is used for both for the sake of convenience.
 *  See the comments after the individual items for differences between
 *  UDR and EDR usage.
 *=============================================================================
 */
typedef	struct
	{
	nims2_sclk_typ	fsclk;			/* FIRST SPACE CRAFT CLOCK   */
	nims2_sclk_typ	lsclk;			/* LAST SPACE CRAFT CLOCK    */
	nims2_ert_typ	fert;			/* FIRST EARTH RECEIVED TIME */
	nims2_ert_typ	lert;			/* LAST EARTH RECEIVED TIME  */
	UBYTE		data_present[46];	/* 364-bit mask (UDR) -- in the 
						   EDR this mask is only 182 
						   bits, and occupies the first
						   23 bytes of data_present */
	UBYTE		data_recd[46];		/* 364-bit mask (UDR only) -- 
						   this field is not used in 
						   the EDR */
	UWORD		threshval[17];		/* threshold DNs */
	UWORD		total_rec;		/* total # of records in EDR */
	UWORD		total_zero;		/* # of zero-fill recs in EDR*/
        UINT            comp_bytes;             /* total # compressed bytes */
        UINT            uncomp_bytes;           /* total # uncompressed bytes */
        char            comp_ratio[6];          /* NIMS Rice comp. ratio */
	UBYTE		reserved[850];
	}
	nims2_hdr1_typ;

/*
 *=============================================================================
 * Second NIMS UDR header (2nd header after PDS label in EDR) -- this is OBSTAB
 *=============================================================================
 */
typedef	struct
	{
	char		oapel[12];
	char		alias[12];
	char		ext;
	char		psid[2];
	char		sclk1[13];
	char		sclk2[13];
        char            partition;
        char            spare1[9];
	char		target[8];
	char		mode[2];
	char		gain;
	char		chop;
	char		grat_off;
	char		ptab_a[12];
	char		ptab_b[12];
	char		ecal;
	char		opcal;
	char		real_time;
	char		record;
	char		threshsel;
	char		spare2;
	char		rtiseldn[5];
	char		rtiselup[5];
	char		spare3;
	char		compflag;
	char		spare4;
	char		estcomp[3];
	char		estcompv[3];
	char		ratecon1[5];
	char		ratecon2[5];
	char		spare5[17];
	char		nwavetot[3];
	char		tlmmode[3];
	char		scet1[21];
	char		scet2[21];
	char		spare6[67];
	char		thresh[51];
	char		wetgid[10];
	char		wetgrpsiz[2];
	char		wetgrp[182];
	UBYTE		reserved[512];
	}
	nims2_hdr2_typ;

typedef	struct
	{
	nims2_hdr1_typ	hdr1;
	nims2_hdr2_typ	hdr2;
	}
	nims2_hdr_typ;

/* 
 *=============================================================================
 * NIMS UDR/EDR data record
 *=============================================================================
 */
typedef	struct
	{
        FIELD     overflow: 1;
        FIELD     underflow: 1;
	FIELD     fill: 6;
	}
	nims_er_flg_typ;

typedef	struct
	{
        FIELD     sclk_suspect: 1;
        FIELD     ert_val: 1;
	FIELD     scid_force: 1;
        FIELD     data_val: 1;
        FIELD     replay_flag: 1;
	FIELD     test_mode: 1;
        FIELD     data_mode: 1;
        FIELD     pb_mode: 1;
	}
	sclk_flg_typ1;

typedef	struct
	{
        FIELD     pkt_flag: 2;
	FIELD     sclk_flag: 3;
        FIELD     sclk_calc_suspect: 1;
        FIELD     sclk_unexpected: 1;
	FIELD     sclk_corr: 1;
	}
	sclk_flg_typ2;

typedef	struct
	{
        FIELD     flush_flag: 4;
	FIELD     scet_val: 1;
	FIELD     scet_int: 1;
	FIELD     less_than_max: 1;
	FIELD     spare: 1;
	}
	sclk_flg_typ3;

typedef	struct
	{
	nims2_sclk_typ	sclk;			/* data acq. SCLK */
	nims2_ert_typ	ert1;			/* ERT of 1st packet */
	nims2_ert_typ	ert2;			/* ERT of 2nd packet */
	nims2_ert_typ	ert3;			/* ERT of 3rd packet */
	UBYTE		apid;			/* Packet Application ID no.*/
	ULONG		pkt_seq[3];		/* sequencer for 3 packets */
	UBYTE		insmode;		/* instrument mode */
	UBYTE		data_complete;		/* flag (>0 means Yes) */
	BYTE		compression_stat;	/* decompression return status*/
	UWORD		subpkt_size;		/* subpacket size in bytes */
	UWORD		data_size;		/* after decomp. (in bytes) */
        UBYTE           mirror_dir;             /* mirror direction */
        UBYTE           grating_position;       /* grating position */
        nims_er_flg_typ error_flags;            /* Overflow & underflow bits */
	UINT		det_mask;		/* 17-bit detector mask */
	UINT		mirror_mask;		/* 20-bit mirror mask */
	UBYTE		rrpmf;			/* RIM rollover pkt miss'g flg*/
        sclk_flg_typ1   sclk_flag1;             /* SCLK SFDU flag 3 bytes */
        sclk_flg_typ2   sclk_flag2;
        sclk_flg_typ3   sclk_flag3;
	UBYTE		reserved[276];
	}
	nims2_pfix_typ;

typedef	struct
	{
	nims2_pfix_typ	pfix;
	UWORD		sensor[340];		/* science data */
	}
	nims2_rec_typ;

  /* these definitions are taken from gll_ssi_bin_diff.h & gll_ph2_ssi_bin.h,
   * needed for the code included from gll_nims_bin_ph2.c: */
static int byte_trans[12], byte_size;
static int half_trans[12], half_size;
static int full_trans[12], full_size;
#define TBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (from)+=byte_size;
#define THALF(from, to) zvtrans(half_trans,(from),(to),1); (from)+=half_size;
#define TFULL(from, to) zvtrans(full_trans,(from),(to),1); (from)+=full_size;
#define TSTRN(from, to, n) zmove((from),(to),(n)); (from)+=(n);
#define ROUTINE          /* */

  /* this was the start of the old nimscmm2 global defines: */

  /* compound sclk = sclk time in mf (mod91): */
#define comp_sclk(sclk) (91 * (sclk).rim + (sclk).mod91)

  /* conversion between planetodetic and planetocentric latitudes in 
   * degrees (for TRANV): */
#define cen2det(lat) ( degrad * atan( tan(lat/degrad) * rep2 ) );
#define det2cen(lat) ( degrad * atan( tan(lat/degrad) / rep2 ) );

#define max(x,y) (x>y ? x : y)
#define min(x,y) (x<y ? x : y)

#define FUNCTION				/* (for that Fortran look) */

#ifndef TRUE
  #define TRUE 1
#endif
#ifndef FALSE
  #define FALSE 0
#endif

	/*
	 * define the std. ISIS reserved "special values" for Real &
	 * Halfword data:
	 *
	 * INStrument low/high saturation means DN = 0/1023
	 * REPresentation low/high saturation overflows DN range
	 *
	 * NOTE: the float definitions are taken from the ISIS
	 * include file 'special_pixel.h'
	 */

/* Define 4-byte special pixel values for IEEE floating point */
#define IEEE_VALID_MIN_4                0xFF7FFFFA
#define IEEE_NULL_4                     0xFF7FFFFB
#define IEEE_LOW_INSTR_SAT_4            0xFF7FFFFD
#define IEEE_HIGH_INSTR_SAT_4           0xFF7FFFFE

/***************************************************************************** 
  Set the appropriate values to the special pixel variables
******************************************************************************/
static const unsigned int valid_min_4      = IEEE_VALID_MIN_4;
static const unsigned int null_4           = IEEE_NULL_4;
static const unsigned int low_instr_sat_4  = IEEE_LOW_INSTR_SAT_4;
static const unsigned int high_instr_sat_4 = IEEE_HIGH_INSTR_SAT_4;

#define VALID_MIN4      (*((const float *) &valid_min_4))
#define NULL4           (*((const float *) &null_4))
#define INS_LO_SAT4     (*((const float *) &low_instr_sat_4))
#define INS_HI_SAT4     (*((const float *) &high_instr_sat_4))
#define VALID_MAX4      (const float) 1.7E+38

/* these were the old definitions on VMS only:
 * (BELOW_THRESH, FILL_FLAG, & NO_SENS were non-standard and
 * not used in final version) */
/*#define NULL4 	-1.7014117e38			/* hex FFFFFFFF */
/*#define INS_LO_SAT4 	-1.7014115e38			/*     FFFDFFFF */
/*#define INS_HI_SAT4 	-1.7014114e38			/*     FFFCFFFF */
/*#define BELOW_THRESH4	-1.7014112e38			/*     FFFAFFFF */
/*#define FILL_FLAG 	-1.7014111e38			/*     FFF9FFFF */
/*#define NO_SENSITIVITY4 -1.7014103e38			/*     FFF1FFFF */
/*#define VALID_MIN4	-1.7014101e38			/*     FFEFFFFF */

/* integer versions: */
#define NULL2 		-32768
#define REP_LO_SAT2 	-32767
#define INS_LO_SAT2 	-32766
#define INS_HI_SAT2 	-32765
#define REP_HI_SAT2 	-32764
#define BELOW_THRESH2 	-32762
#define NO_SENSITIVITY2	-32754
#define VALID_MIN2 	-32752
#define VALID_MAX2 	 32767

#define DN_MIN 6				/* min. valid DN */

#define BADP -9999.999				/* bad projected X/Y */

#define EOL_SCLK 100				/* end of list */

#define N_GPS 26				/* max.# gp's in given mode*/
#define N_DETS 17				/* no. of detectors */

#define NB_GEO 9				/* default NB for cocube */

#define MAX_EDRS 25				/* max. # input EDRs */
#define MAX_RIMS 100				/* max. # of RIMs per EDR */

/* this is the max. histogram bin size in histbin option -- must agree
 * with COUNT field of hbinsize parameter in PDF: */
#define MAX_HBINSIZE 510

#define CK_TOL 10				/* max. gap in CK (mfs) */

#define DUMMY -9999.				/* denotes blank RA */

struct pntng_typ {
  double ra;
  double dec;
  double twist;
  double cone;
  double clock;
};

/* this defines the buffer holding pointing data for valid records,
 * as determined by the slew rate checks in assemble_pntng: */

struct mf_type {
  unsigned int sclk;			/* compound SCLK */
  int igc;			/* grating cycle count (for TUBE), (also used 
			 	   to flag entire MF rejected by pointing) */
  struct pntng_typ cang;		/* at start of each minor frame */
};

static struct mf_type *mf;		/* will be allocated with malloc */
int nmf;		/* total # of MFs in observation */
int ngcs;		/* total # of grating cycles, for Tube case */
int nbpg;		/* # of backplanes per GC, for Tube case */

struct xytype {float x,y;};             /* for mirror pos. calculations */

	/* summary of time range limits:
	 *
	 * The following are of type SCLK (RIM.MF.RTI):
	 *
	 *  loclk/hiclk start as user param.SCLK, if specified, and are reset
	 *    to minclk/maxclk if latter range is smaller
	 *
	 *  minclk/maxclk are the limits of all data in all EDRs
	 *
	 *  in a tube, lo/minclk are forced to be the start of a Grating Cycle
	 *
	 * The following are of type int (91*RIM+MF):
	 *
	 *  range_lclk/range_hclk are derived from param.SCLK, and
	 *    are zero if this was not specified
	 * 
	 *  file_lclk/file_hclk are the limits of the file currently open */

nims2_sclk_typ loclk, hiclk, minclk, maxclk;	/* range params */

unsigned int range_lclk, range_hclk;	/* comp_sclk range */
unsigned int file_lclk, file_hclk;	/* comp_sclk file lims */
unsigned int file_hrti;			/* RTI limit, to avoid read errors */
unsigned int file_sclk;			/* nom 1st sclk in file */

int rtiperplane;			/* rti's per plane */

enum {					/* mirror direction */
  UP, DOWN, WAIT, STOP
} mirror, pmirror;

/* buffers to hold translated NIMS EDR/UDR data: */
nims2_hdr_typ edrhdr;
nims2_rec_typ edrrec;

/* 
 * status of EDRs
 */
typedef struct {
  int foff;				/*  name offset  */
  int opened;
  int first_open;
  int unit;				/*  VICAR file ID  */
} edr_stat_typ;

edr_stat_typ edrs[MAX_EDRS];
int nedrs, cur_edr;
int nrec;				/* # of records in current EDR */

short wet[N_GPS][N_DETS];		/* Wavelength Edit Table */
char wet_gid[11];			/* WET ID (for label) */
int gpos[N_GPS];	/* actual GP (excluding deselected ones) vs nominal */

int nl, ns, nb;				/* CUBE DIMENSIONS */
int nbb;				/* max. nb for ins.mode */
int maxmpos=20;				/* max. no. of mirror positions */
int smpos, empos, nmpos;		/* start/end/no. of mirror pos. */
int smpos0, empos0;			/* ignoring mirror scan direction */
int ngps;				/* no. of grat. pos. per cycle */
int ngps0;				/* no. of "real" grat. pos. */
char fix_mode;			/* flag for fixed grating with mirror wait */
char fix_wait;			/* flag to process mirror wait in fix_mode */
int imode;				/* instrument mode code */
char ins_mode[20];			/*  id. as string */
int u_imode;				/* user-spec'd instrument mode */
char stop_slide[6];			/* stop/slide mode flag */
int nlgp;				/* # logical gp's for SAFE/FIXED mode */
int lami;				/* =0 unless user spec'd 1 band */
int deteci;				/* for DRK_AVE when lami>0 */
char thermal;				/* flag for thermal channel */
int geosd, lamsd;			/* band for std.dev. in cocube */
int nlamsd;				/* count for lamsd */
int s_det, e_det;			/* detector range */
int skipdet[17];			/* flags to omit detectors */
int nskipgp;				/* GPs to skip */

int nvalues, def;			/* for xviparm */
int vstat;				/* returned function status */
int display_unit;			/* display log. unit */
int data_unit;				/* data file log. unit */
char project[10];
int nparallels;			/* flag for user-spec'd Lambert parallels */
int nmaplin, nmapsam;			/* id. for special line/samp */
int nscal;				/* id. for scale */
char target[16];			/* target planet */
int calib;				/* flag for CAL/SKY target */
/*char obsnam[20];			/* observation name */
/*char obsext[2];				/* observation extension */
/*char mosnum[3];				/* mosaic id. no. */
char fname[MAX_EDRS*40];		/* input EDR file names */
int x_min = 1;				/*  counted from 1  */
int x_max = 512;
int y_min = 1;
int y_max = 512;

	/* data for map projection (TRANV): */
double latitude, longitude, map_line, map_samp;
double parallel[2];
double scale, focal;
double eq_rad, pol_rad;
/*double north=NULL4;*/
double north;
double map_pole;
int map_type;				/* map projection code */

double scale0;				/* nominal scale */
double circum, long0, mapsam0;		/* for cylindrical wraparound fix */
int inorth;				/* flag that north is user-spec'd */
int elon;				/* flag for East longitudes */
int radfudge;				/* flag to do radius fudge */
double radfact;				/* radius fudge factor */
float rnom, lfact;			/* used in radius fudge */
float rtang;				/* tangent radius for radius fudge */
double frad, frad1, frad2;		/* pole-equ. ratio */

int refsclk, refmp;			/* ref. SCLK for map projection */

double olatlon[2]; 			/* user-specified opt.axis Lat/Long */
int n_oaxis, n_oll, recenter;
double pdist;				/* user-specified s/c-planet distance */

float slant;				/* "slant distance" returned by PPROJ */

struct despike_type {                   /* despike data */
  unsigned long int rim;
  unsigned char mod91;
  unsigned char rti;
  unsigned char mp;
  unsigned char det;
  short olddn;
  short newdn;
};
static struct despike_type *pdespike;	/* will be allocated with malloc */
int nspik;                               /* # of spikes in despike file */

int drk_tol=10;			/* max. allowed deviation of dark from mean */

	/*
	 * Mirror Tables from IKERNEL:
	 */
double mirr_tim_table[40];		/* mirror sample timing table */
struct {
  double cone_up, cone_down, xcone_up, xcone_down;
} mirr_pos_table[20];			/* position tables for UP/DOWN only */
double mirr_pos_xcone_wait;
double mirr_pos_xcone_stop;
double nims_bore_cone, nims_bore_xcone;	/* NIMS offset from boresight */

char msg[100];

#define LL_LS 1				/* PPROJ/TRANV mode 1 */
#define LS_LL 2				/* PPROJ/TRANV mode 2 */

typedef double vector[3];
typedef double matrix[3][3];

struct rdata {				/* raw data format for PPROJ */
  matrix om;
  vector rs;
  float prad,e1rad;
  float focal,aline,asamp,cscale;
  float e2rad;	/* semi-minor eq.radius for new PPROJ */
} tgt_data;

struct rdata t_data_pov;		/* PPROJ buffer for POV projection */
struct rdata t_data_povf;		/* fudged copy of POV buffer */
struct rdata t_data_save;		/* for shuffling buffers around */

int footprint;				/* flag for footprint algorithm */
int fpgrid;				/* # of footprint grid vertices */
int nfpwts;				/* # total of footprint grid weights */
float thresh;				/* threshold weight to keep pixel */
int nthresh=0;
float maxdistor;			/* max. distortion to keep pixel */
int ndistor;

int use_max;				/* flag for OVERLAP=MAXIMUM */
int use_last;				/* flag for OVERLAP=REPLACE */
int do_fill;				/* flag for fill algorithm */
int flag_sat;				/* flag saturated pixels */
int repl_sat;			/* replace saturated pixels with BB est. */
int max_sat;			/* replace saturated pixels with 1022 */
float repl_alb=0.0;			/* mean albedo used in bb_repl */
int ncompalb=0, ndefalb=0, nsatrep=0;
float sat_thrsh;			/* threshold weight to flag sat */
int nsatthr=0;
char lo_sat;				/* use thresholded pixels? */
int c_extrap;				/* flag to extrapolate C-matrices */
int nochk;				/* flag that no slew rate checks done*/

body_id_typ ids;			/* for NAIF SPICE calls */
char spice_ids[4*LEN_SPICE_ID];		/* for label */
int ck_id;				/* for C-kernels */
double sclkdp;				/* SCLK for CKxxx calls */
double s_tol;				/* C-mat tolerance in ticks */

char esystem[6];			/* B1950 or J2000 */

float *odata;				/* address of (temp.) float cube */
float *gdata;				/* address of cocube file*/
float *wdata;				/* address of weights file*/
float *sddata;				/* address of std.dev. cube */

int sd_file;				/* flag that std.dev. cube requested */

int wtnb;				/* NB of weights file */
int histbin;				/* histogram-binning option flag */
int histcrit;				/* criterion for evaluating histogram */
int hbinsize;				/* size of histogram bins */
int hbinstep, hbstp2;			/* DN width & halfwidth of hist.bins */

char cal_type[5];			/* calibration type */
char dark_type[8];			/* dark value update type */
int caltyp, drktyp, radscal;

int dndark = 0;				/* use dark file for DN cube? */
float darktab[40][17];			/* dark value table for DN cube */
int drkchk;				/* flag for DRKTHRSH  option */
float drkfac;	 			/* factor for DRKTHRSH option */

int bmask[408];				/* band mask */

float sol_flux[408];			/* solar flux at std. wavelengths */
float cmult[408], cbase[408];	/* scale/offsets for radiance calibration */

float waves[408];			/* wavelengths, for label */
float sensv[408];			/* radiometric sensitivities for label*/
float dave[17];				/* dark value averages, for label */
int oldcal;		/* flag for wavelengths from old-style CAL files */
float pshift;				/* label item, from Cal routines */
float ainfl;				/* grating step inflation */

float utemps[6];			/* user-spec'd instrmt temperatures */

char cfile[MAX_EDRS*101];		/* calibration filename */
int cfnoff[MAX_EDRS];			/* pointers in cfile buffer */
int ncalfils;				/* number of cal files input */
char dfile[MAX_EDRS*101];		/* dark file name */
int dfnoff[MAX_EDRS];			/* pointers in dfile buffer */
int ndrkfils;				/* number of cal files input */
char solfile[101];			/* solar flux filename */
char dsfile[MAX_EDRS*101];		/* despike file names */
int dsfnoff[MAX_EDRS];			/* pointers in dsfile buffer */
int nspifils;                           /* # of despike files */
char dbmfile[101];			/* deboom filename */
int dbm;				/* deboom flag */
int c_spice;				/* flag for C-angles from SPICE */
int aacsf;				/* flag for AACS file */
char pckernel[101];			/* pfm C-kernel file */
char rckernel[101];			/* rotor C-kernel file */
int ihpck, ihrck;			/* handles for CKs */
char aacsfnam[101];			/* AACS filename */
FILE *aacsfil;				/*   ... and unit */
char spkernel[101];			/* SP-kernel file */
int u_spk;				/* flag for user-spec'd SPK */
char pconstants[101];			/* Planet-constants kernel file */
int u_pck;				/* flag for user-spec'd PCK */
char ikernel[101];			/* I-kernel file */

double etime;				/* ephemeris time, for SPICE */
float epherr;				/* time offset for ephemeris error */

int gain[MAX_EDRS];			/* gain state, for label */
int chopper;				/* chopper mode, for label */
int g_off;				/* grating offset, from params */
int gstart;				/* grating start, from AACS */
int gdel;				/* grating delta   "     "  */
int mirrop;				/* mirror motion flag "  "  */
int nsteps;				/* # log. g.p.'s     "   "  */
int u_gst;				/* flag for user-spec'd gstart */

int osize[2] = {0,0};			/* projected image size for TUBE */

/* we have 2 flags for planetocentric/graphic latitudes:  they are
 * usually identical, but in the spherical case, lattyp=0 (for PPROJ),
 * while pgraphic=-1 to suppress the label item: */
int pgraphic, lattyp;
double rep2, dblat, tdlat;

struct {
  vector u_sun;				/* unit vector target - sun */
  vector rs;				/* target center - s/c vector */
  float euler[3];			/* RA/Dec/Twt */
  float geom[3];			/* incidence, emission, phase */
  float rads[2];			/* s/c sdist, off-limb height */
  float latlon[2];			/* Latitude/Longitude (TUBE only) */
  float photcor;			/* photometric correction factor */
} cobuf[20];					/* geometry buffer */

double inccut,emicut;			/* incidence/emission angle cutoffs */
int cutinc,cutemi;			/* flags for above */

int pfunc;				/* photometric function flag */
double pfcut;				/* phot.function cutoff wavelength */
int npfcut;				/*  ditto for band # */
char phot_func[10];			/* same as pfunc, but text */
double p_minn;				/* Minnaert exponent */

	/* these items are for the label only: */
float sun_azi=0., sc_azi=0.;			/* azimuths */
int n_azi=0;
float minsdist = 1.e30, maxsdist = -1.e30; 	/* s/c - planet surface */
float minsun=1.e30, maxsun= -1.e30;		/* s/c - sun dist. */
float mincenbod=1.e30, maxcenbod= -1.e30;	/* s/c - central body dist. */

	/* these items are for the catalog only, except for mmlat/lon &
	 * ssclat/lon, which also go to label ... [2] arrays are either
	 * begin/end or min/max: */
float trange[2];			/* slant distance range (to target) */
float crange[2];			/* sun distance range */
float tincid[2] = {1.e30,-1.e30};	/* min/max incidence angle */
float temiss[2] = {1.e30,-1.e30};	/* min/max emission angle */
float tphase[2] = {1.e30,-1.e30};	/* min/max phase angle */
float mmlat[2] = {1.e30,-1.e30};	/* min/max latitude */
float mmlon[2] = {1.e30,-1.e30};	/* min/max longitude */
int lonrange;				/* flags how to determine min/max lon */
float ssclat[2];			/* latitude at sub-s/c point */
float ssclon[2];			/* longitude at sub-s/c point */
float ssollat[2];                       /* latitude at sub-solar point */
float ssollon[2];                       /* longitude at sub-solar point */

double slew_rate;			/* mean slew rate */
double slew_tol;			/* slew rate tolerance */
double twt_tol;				/* twist angle spike tolerance */

float dpoint[2];			/* pointing offsets */
int dpt_sclk[2];			/* SCLK range for dpoint */
int dpt_sclk0[2];
double wamp, wfreq, wphase;		/* wobble constants */
double wob_cone;			/* cone estimate for Predict pointing */
int beg_sclk;				/* for wobble computation */

int tube;				/* =0: G-cube, =1: tube */
int ptub;				/* =0: std.tube, =1: P-tube */
  /* note that ptub implies tube! */

int org;				/* cube format: BSQ/BIL/BIP */

int dunit, gunit;			/* Vicar unit #s for cube & cocube */

int cat_flag;				/* flags request to updata catalog */
int ll_rec1, ll_rec2;			/* more flags for catalog/label items */

int xtest;				/* debug/test param */

int nohkp;				/* flag to ignore housekeeping data */

int mirrsel;				/* flag to throw out mirror positions */
int nomirr[39];				/* array of positions to discard */

int bclk[100], eclk[100];		/* for SCLK limits */
int nintrvl;

float ffmin[408], ffmax[408];		/* for dynamic radiance scaling */

kernel_db_typ kernel_db[MAX_KERNELS];
int kernel_count;

int threshval[17];			/* for label items */
int threshing;				/* flag that thresholding was done */

extern double zpi();
extern double zhalfpi();
extern double ztwopi();

double degrad;		/* degrees per radian */

int idebug=0;		/* a flag that can be set in the debugger */

int is0=0;		/* SCLK of last record read from AACSFILE */

int chk_comp;			/* check compression status flag?  */

char beg_utc[40], beg_utcd[40], end_utc[40], end_utcd[40];

char initials[4];
int oldver;		/* flag to use old algorithm, keyed to INITIALS */

int cal_asc, drk_asc;	/* flags for ascii cal/dark files */

/****************************************************************************/
void main44()
{
  int def, fpar[2], i, isclk, istat, nvalues, sclk1, sclk2;
  float *dptr, *fplane;

  /* tell user which version this is */
  zvmessage(" *** NIMSCMM2 Version 2016-02-24 ***","");

  init_stuff();		/* initialize SPICE & some buffers --
			 * this does some ZVPARMS */

  get_uparms();		/* this also opens 1st input file */

  /* open all EDRs in order to find ins.mode and to determine total range
   * of EDR records: */
  cur_edr = -1;
  imode = -1;
  open_edr(0);	/* computes nrec & file_lclk/hclk, increments cur_edr */
  if (imode==-1) find_mode();
  /* at this point we are sure to have a valid SCLK, so can finally load
   * an SPK if not already done: */
  if (!u_spk) {
    nims_s2et( file_lclk, &etime);
    zload_spice95( etime, spice_ids, &istat);
    if (istat!=SUCCESS) zmabend(" *** unable to load spice for GLL ***");
  }
  while (cur_edr+1<nedrs) {
    open_edr(0);
    if (imode==-1) find_mode();
  }
  if (imode==-1) zmabend(" *** unable to determine instrument mode ***");

  /* close the EDRs to avoid problems ... */
  for (i=0; i<nedrs; i++)
    if (edrs[i].opened) {
      zvclose( edrs[i].unit, NULL);
      edrs[i].opened = FALSE;
    }

  /* at this point all EDRs have been inspected, so min/maxclk reflect
   * full range of data ... use these to reset lo/hiclk if needed;
   * for a tube, ensure minclk/loclk start on new GC */
  if (tube) {
    i = ngps * ((2*(int)minclk.mod91 + (int)minclk.rti/5) / ngps);
    minclk.mod91 = i/2;
    minclk.rti = 0;
    if (i%2 != 0) minclk.rti = 5;
    i = ngps * ((2*(int)loclk.mod91 + (int)loclk.rti/5) / ngps);
    loclk.mod91 = i/2;
    loclk.rti = 0;
    if (i%2 != 0) loclk.rti = 5;
      /* reset loclk even if it falls in the 1st GC: */
    if (comp_sclk(loclk) < comp_sclk(minclk)) loclk = minclk;
      /* set number of cotube bands per g.p. */
    nbpg = footprint ? 6 : 4;
    if (ptub) nbpg += 6;
  }
  else {
    if (comp_sclk(loclk) < comp_sclk(minclk)) loclk = minclk;
  }
  if ((comp_sclk(hiclk) > comp_sclk(maxclk)) || 
    (comp_sclk(hiclk)==0)) hiclk = maxclk;

  /* compute the start/end UTCs here because SPICE calls are flakey 
   * later on ... */
  isclk = comp_sclk( loclk);  
  nims_s2et( isclk, &etime);
  zet2utc( etime, "C", 0, beg_utc);
  zet2utc( etime, "D", 3, beg_utcd);
  isclk = comp_sclk( hiclk);  
  nims_s2et( isclk, &etime);
  zet2utc( etime, "C", 0, end_utc);
  zet2utc( etime, "D", 3, end_utcd);

  /* open the AACS file */
  if (aacsf) {
    aacsfil = fopen( aacsfnam, "r");
    if (aacsfil == NULL) zmabend(" ** unable to open AACSFILE **");
  }

  /* read in all AACS pointing data, scan sensor data for valid records,
   * also find s/c ranges & scale values */
  assemble_pntng();
  beg_sclk = comp_sclk(loclk);	 	/* for wobble tweak */

  /* if this is just a run to check out pointing, quit here ... */
  if (xtest==4 || xtest==5 || xtest==7) return;

  if (!calib) {

    /* set final map projection parameters & output size: */
    if (map_type==16) set_pov();
    else set_projection();

    /* another test option ... */
    if (xtest==6) return;
  }

  /* if binning thresholds defaulted in Footprint, determine them using
   * scale: */
  if (footprint && !nthresh) thresh = 0.1*(scale/scale0)*(scale/scale0);
  if (footprint && !nsatthr) sat_thrsh = 0.5*(scale/scale0)*(scale/scale0);

  if (tube) {	/* reset nl,ns computed in set_pov/proj */
    ns = maxmpos;
    find_tube_gcs();
    /* here is the previous algorithm, based on # of grating cycles in the
     * entire SCLK range: */
    /*
    sclk1 = comp_sclk(loclk);
    sclk2 = comp_sclk(hiclk);
    if (fix_mode)
      ngcs = 2*(sclk2-sclk1+1);
    else {
      i = ngps/2;
      ngcs = 1+(sclk2-sclk1+1)/i;
      if ((loclk.mod91)%i > (hiclk.mod91)%i) ngcs++;
    }
    */
    nl = ngcs;
  }

  if (xtest) 	{	/* test mode:  just generate plot data */
    output_test();
    return;
  }

	/* open output cube & cocube files, allocate temp. REAL cube;
	 * this also initializes to NULL */
  open_outs();

	/*
	 * read the sensor data, correct, and project them:
	 */
  extract_data();

  if (aacsf) fclose( aacsfil); 

	/* in histogram-binning option, generate output: */
  if (histbin) hist2cube();

	/*
	 * if footprint algorithm used, then check for pixels with
	 * weights below threshold (G-cube only):
	 */
  if (thresh>0. && !tube && !histbin) check_thresh();

	/* if std.dev. cube requested, convert <DN**2> to s.d. */
  if (sd_file) comp_stdev();

	/* write lat/long data to cocube for all data pixels:
	 * (must do this before FILL step, as latter uses lat plane
	 * as check for on-planet pixels) */
  if (!tube) write_latlon();

	/*
	 * option to fill the data and planes 3-6 of the cocube:
	 */
  if (do_fill) {
    zvparm( "FILPAR", fpar, &nvalues, &def, 2, 0);
    for (i=0; i<nb; i++) {
      dptr = odata + i*nl*ns;
      vstat = fill( dptr, fpar[0], fpar[1], 0);
      if (vstat<0) break;
    }
    for (i=0; i<4; i++) {
      dptr = gdata + (2+i)*nl*ns;
      fplane = 0;
      if (i==0) fplane = gdata+7*nl*ns;	/* plane 8 for fill flag */
      vstat = fill( dptr, fpar[0], fpar[1], fplane);
      if (vstat<0) break;
    }
  }

	/* scale floating-point cube data and write out as halfword: */
  if ((caltyp && radscal>0) || (!caltyp && !tube)) scale_data();

	/* call this even if we're not going to do catalog stuff in order
 	 * to have the same min/max lat/lon values in all cases: */
  update_catalog();
	/*
	 * add history labels and close the files --
	 * first close & reopen to avoid overflow of vicar label buffer
	 * with array I/O:
	 */
  zvclose( dunit, NULL);
  
	/* close edrs too: (cocube will be closed after last
	 * use in find_angles) */
  for (i=0; i<nedrs; i++) if (edrs[i].opened) zvclose( edrs[i].unit, NULL);

  zvopen( dunit, "OP", "UPDATE", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
  add_label( fpar);
  zvclose( dunit, NULL);
}


/*************************************************************************/
FUNCTION add_label( fpar)
/*
 * add VICAR history labels
 */
int fpar[2];
{
  char onbuf[91], snbuf[159], svalue[MAX_EDRS][101];
  int gaps[200], i, i1, i2, irim, imf, j, k, ivalue[3];
  float dum, eq_radf, fword, rvalue[9], x;
  char mn;

  union {
    struct { float reals[38]; int ints[2];} map1;
    struct { double dbles[12]; float reals[14]; int ints[2];} map2;
  } mapbuf;			/* for MAPLABV2 */
  MP mpo;                      /* Declaration of MP pointer */

	/* if TUBE, write a pseudo-maplab label to keep Vicar s/w
	 * from misinterpreting the label: */
  if (tube) {
    zladd( dunit, "HISTORY", "MAP00000",
     "** TUBE FILE: map labels apply to cocube data only **", 
     "FORMAT", "STRING", NULL);
    zladd( dunit, "HISTORY", CUBE_SIZE, osize, "FORMAT", "INT",
	"NELEMENT", 2, NULL);
  }

	/* first write out MAPLAB-format labels so other Vicar s/w
	 * can interface with our file: */
  zvmessage(" ","");

  if (calib) goto endmap;

  if (map_type==16) {		/* special for POV */

    for ( i=0, k=0; i<3; i++) {
      for ( j=0; j<3; j++, k++) mapbuf.map2.dbles[k] = t_data_pov.om[i][j];
    }
    for (i=0;i<3;i++) mapbuf.map2.dbles[9+i] = t_data_pov.rs[i];
    mapbuf.map2.reals[0] = t_data_pov.prad;
    mapbuf.map2.reals[1] = t_data_pov.e1rad;
    mapbuf.map2.reals[2] = t_data_pov.focal;
    mapbuf.map2.reals[3] = t_data_pov.aline;
    mapbuf.map2.reals[4] = t_data_pov.asamp;
    mapbuf.map2.reals[5] = t_data_pov.cscale;
    mapbuf.map2.reals[10] = north;
    /* fill in 2 extra slots for new MP: */
    mapbuf.map2.reals[11] = 0.;
    mapbuf.map2.reals[12] = t_data_pov.e1rad;
    sprintf( msg, " Focal Length (mm) = %.1f, Camera Scale (pix/mm) = %.1f", 
     t_data_pov.focal, t_data_pov.cscale);
    zvmessage( msg,"");
    sprintf( msg, " Optical Axis Line/Sample = %.3f, %.3f", t_data_pov.aline,
     t_data_pov.asamp);
    zvmessage( msg,"");

	/* range: */
    x = sqrt( t_data_pov.rs[0] * t_data_pov.rs[0] +
	      t_data_pov.rs[1] * t_data_pov.rs[1] +
	      t_data_pov.rs[2] * t_data_pov.rs[2] );
    mapbuf.map2.reals[13] = x;
    sprintf( msg, " Spacecraft-Target Range (km) = %.1f, North Angle = %.2f",
     x, north);
    zvmessage( msg,"");

	/* Lat & Lon(W) of subspacecraft point: */
    mapbuf.map2.reals[6] = degrad * asin( t_data_pov.rs[2] / x);
    mapbuf.map2.reals[7] = degrad * atan2( -t_data_pov.rs[1],
     t_data_pov.rs[0]);
    if (mapbuf.map2.reals[7]<0.) mapbuf.map2.reals[7] += 360.;
    sprintf( msg, " Subspacecraft Lat/Long (deg) = %.2f, %.2f", 
     mapbuf.map2.reals[6], mapbuf.map2.reals[7]);
    zvmessage( msg,"");

	/* Line/Samp of subspacecraft point: */
    zpproj( &t_data_pov, &mapbuf.map2.reals[8], &mapbuf.map2.reals[9],
     &mapbuf.map2.reals[6], &mapbuf.map2.reals[7], LL_LS, lattyp, &dum,
     &dum, &i);
    sprintf( msg, " Subspacecraft Line/Samp = %.3f, %.3f", 
     mapbuf.map2.reals[8], mapbuf.map2.reals[9]);
    zvmessage( msg,"");
  }  
  else {
    if (map_type==9 || map_type==10) {	/* restore original tielon/sam */
      longitude = long0;
      map_samp = mapsam0;
    }
    for (i=0; i<=39; i++) mapbuf.map1.reals[i] = 0.;
    mapbuf.map1.reals[0] = map_samp;
    mapbuf.map1.reals[1] = map_line;
    mapbuf.map1.reals[2] = latitude;
    mapbuf.map1.reals[3] = parallel[0];
    mapbuf.map1.reals[4] = parallel[1];
    mapbuf.map1.reals[5] = longitude;
    mapbuf.map1.reals[6] = scale;
    mapbuf.map1.reals[7] = map_pole;
    mapbuf.map1.reals[8] = north;
    mapbuf.map1.reals[24] = pol_rad;
    mapbuf.map1.reals[25] = eq_rad;
    sprintf( msg, " Tiepoint Lat/Long (deg) = %.2f, %.2f", latitude, longitude);
    zvmessage( msg,"");
    sprintf( msg, " Tiepoint Line/Sample = %.3f, %.3f",  map_line, map_samp);
    zvmessage( msg,"");
    if (north>(-360.))
      sprintf( msg, " Map scale (km/pix) = %.4f, North Angle (deg) = %.2f",
       scale, north);
    else
      sprintf( msg, " Map scale (km/pix) = %.4f", scale);
    zvmessage( msg,"");
  }
  mapbuf.map1.ints[0] = map_type;

  vstat = mpInit( &mpo);
  if (vstat!=mpSUCCESS) zmabend(" error initializing MP object");
  vstat = mpBuf2Mpo( &mapbuf, mpo);
  if (vstat!=mpSUCCESS) zmabend(" error in MP translation");
  /* mpBuf2Mpo always writes "planetocentric", so fix this if we passed
   * it 'graphic' latitudes: */
  if (pgraphic==1) {
    vstat = mpSetValues( mpo, mpCOORDINATE_SYSTEM_NAME, "PLANETOGRAPHIC", "");
    if (vstat!=mpSUCCESS) zmabend(" error setting Coord.system");
  }
  /* fix 2nd axis if tri-axial (POV only): */
  if (map_type==16 && t_data_pov.e1rad!=t_data_pov.e2rad) {
    vstat = mpSetValues( mpo, "B_AXIS_RADIUS", t_data_pov.e2rad, "");
    if (vstat!=mpSUCCESS) zmabend(" error setting B_AXIS label");
  }
  vstat = mpLabelWrite( mpo, dunit, "HISTORY");
  if (vstat!=mpSUCCESS) zmabend(" error writing map labels");
  vstat = mpLabelWrite( mpo, dunit, "PROPERTY");
  if (vstat!=mpSUCCESS) zmabend(" error writing map labels");

	/*
	 * then other stuff for VISIS & NIMSMASK:
	 */

	/* required for PDS map projection group: */
  if (mmlon[0]<400. && elon) {	/* ensure Longitudes are East for Venus */
    mmlon[0] = 360.-mmlon[0];
    mmlon[1] = 360.-mmlon[1];
  }
  zladd( dunit, "HISTORY", MIN_LAT, &mmlat[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", MIN_LON, &mmlon[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", MAX_LAT, &mmlat[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", MAX_LON, &mmlon[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

  /* write these out to log too: */
  sprintf( msg, " Min/max latitudes: %.2f, %.2f", mmlat[0], mmlat[1]);
  zvmessage( msg,"");
  sprintf( msg, " Min/max longitudes: %.2f, %.2f", mmlon[0], mmlon[1]);
  zvmessage( msg,"");

  /* write out the min/max photometric angles too: */
  sprintf( msg, " Min/max incidence angles: %.2f, %.2f", tincid[0], tincid[1]);
  zvmessage( msg,"");
  sprintf( msg, " Min/max emission angles: %.2f, %.2f", temiss[0], temiss[1]);
  zvmessage( msg,"");
  sprintf( msg, " Min/max phase angles: %.2f, %.2f", tphase[0], tphase[1]);
  zvmessage( msg,"");

  if (pgraphic==1)
    zladd( dunit, "HISTORY", LAT_TYPE, "PLANETOGRAPHIC", "FORMAT", "STRING", NULL);
  else if (pgraphic==0)
    zladd( dunit, "HISTORY", LAT_TYPE, "PLANETOCENTRIC", "FORMAT", "STRING", NULL);

endmap:

	/* identify band if only one requested: */
  if (lami && imode<=7) {
    sprintf( msg, "Band %3d only of ", lami);
    if (fix_mode || imode==0 || imode==7) sprintf( &msg[17], "FIXED mode");
    else if (imode==1 || imode==2) sprintf( &msg[17], "FULL mode");
    else if (imode==3 || imode==4) sprintf( &msg[17], "LONG mode");
    else if (imode==5 || imode==6) sprintf( &msg[17], "SHORT mode");
    zladd( dunit, "HISTORY", "COMMENT", msg, "FORMAT", "STRING", NULL);
 }

  for (i=0; i<nedrs; i++)
    strcpy( svalue[i], (fname+edrs[i].foff-1));
  zladd( dunit, "HISTORY", EDRS, svalue, "FORMAT", "STRING",
	"NELEMENT", nedrs, "ULEN", 101, NULL);

  zladd( dunit, "HISTORY", PROJECT, "GLL", "FORMAT", "STRING", NULL);

  zladd( dunit, "HISTORY", INSTRUMENT, "NIMS", "FORMAT", "STRING", NULL);

  zvparm( "PHASE", svalue[0], &nvalues, &def, 1, 0);	 
  if (strcmp(svalue[0], ""))
    zladd( dunit, "HISTORY", PHASE, svalue[0], "FORMAT", "STRING", NULL);

  zladd( dunit, "HISTORY", TARGET, target, "FORMAT", "STRING", NULL);

/* removed for VICAR-only version:
  zladd( dunit, "HISTORY", OBSNAME, obsnam, "FORMAT", "STRING", NULL);
  zladd( dunit, "HISTORY", OBSEXT, obsext, "FORMAT", "STRING", NULL);
  zladd( dunit, "HISTORY", MOSNUM, mosnum, "FORMAT", "STRING", NULL);
*/

  zvparm( "OBSNOTE", onbuf, &nvalues, &def, 1, 0);
  if (strcmp( onbuf, "")) zladd( dunit, "HISTORY", OBSNOTE,
   onbuf, "FORMAT", "STRING", NULL);

  zvparm( "SUPPNOTE", snbuf, &nvalues, &def, 1, 0);
  if (strcmp( snbuf, "")) zladd( dunit, "HISTORY", SUPPNOTE,
   snbuf, "FORMAT", "STRING", NULL);

/* removed for VICAR-only version:
  zvparm( "PRODID", svalue[0], &nvalues, &def, 1, 0);
  if (!strcmp( svalue[0], "")) {	/* form default Product ID */
/*  strcpy( svalue[0], obsnam);
    strcat( svalue[0], obsext);
    strcat( svalue[0], "_");
    strcat( svalue[0], initials);
    strcat( svalue[0], mosnum);
    if (tube)
      strcat( svalue[0], ".VTUB");
    else
      strcat( svalue[0], ".QUB");
  }
  zladd( dunit, "HISTORY", PRODID, svalue[0], "FORMAT", "STRING", NULL);
*/

  zvparm( "PRODNOTE", svalue[0], &nvalues, &def, 1, 0);
  if (strcmp( svalue[0], "")) zladd( dunit, "HISTORY", PRODNOTE,
   svalue[0], "FORMAT", "STRING", NULL);

  if (c_spice) {
    zladd( dunit, "HISTORY", PFM_CK, pckernel, "FORMAT", "STRING", NULL);
/*    zladd( dunit, "HISTORY", ROT_CK, rckernel, "FORMAT", "STRING", NULL); */
  }
  else if (aacsf)
    zladd( dunit, "HISTORY", AACS_FILE, aacsfnam, "FORMAT", "STRING", NULL);

  zladd( dunit, "HISTORY", IKERNEL, ikernel, "FORMAT", "STRING", NULL);

  if (u_spk) 		/* if user specified SPK */
    zladd( dunit, "HISTORY", SP_KERNEL, spkernel, "FORMAT", "STRING", NULL);
  else {
	/* SPK ids -- the S & P ids will usually
	 * be the same, but keep separate for generality: */
    strncpy( svalue[0], spice_ids, LEN_SPICE_ID);
    svalue[0][LEN_SPICE_ID] = 0;
    zladd( dunit, "HISTORY", SK_ID, svalue, "FORMAT", "STRING", NULL);
    strncpy( svalue[0], spice_ids, LEN_SPICE_ID);
    zladd( dunit, "HISTORY", "PK_ID", svalue, "FORMAT", "STRING", NULL);
  }

  if (u_pck) 		/* if user specified PCK */
    zladd( dunit, "HISTORY", PC_KERNEL, pconstants, "FORMAT", "STRING", NULL);

  if (dpoint[0] != 0.0 || dpoint[1] != 0.0) zladd( dunit, "HISTORY",
   DPOINT, dpoint, "FORMAT", "REAL", "NELEMENT", 2, NULL);

  if (dpt_sclk0[0] != 0 || dpt_sclk0[1] != 0) zladd( dunit, "HISTORY",
   DPT_SCLK, dpt_sclk0, "FORMAT", "INT", "NELEMENT", 2, NULL);

  if (wamp>1.e-7) {
    zladd( dunit, "HISTORY", WAMP, &wamp, "FORMAT", "REAL", "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", WFREQ, &wfreq, "FORMAT", "REAL", "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", WPHASE, &wphase, "FORMAT", "REAL", "NELEMENT", 1,
     NULL);
    if (!aacsf) {
      wob_cone *= degrad;
      zladd( dunit, "HISTORY", WCONE, &wob_cone, "FORMAT", "REAL", "NELEMENT",
       1, NULL);
    }
  }

  zladd( dunit, "HISTORY", CAL_TYPE, cal_type, "FORMAT", "STRING", NULL);

  if (radscal == -1) zladd( dunit, "HISTORY", "CAL_NOTE", 
   "Pseudo-Radiance: floating-point DN cube",  "FORMAT", "STRING", NULL);

	/* PDS label wants these even if not calibrated: */
  zladd( dunit, "HISTORY", DARK_TYPE, dark_type, "FORMAT", "STRING", NULL);

  if (!caltyp) { /* if radiance calibration done, sat. is always flagged */
    if (flag_sat) zladd( dunit, "HISTORY", SATURATED, "FLAGGED",
     "FORMAT", "STRING", NULL);
    else zladd( dunit, "HISTORY", SATURATED, "NOT_FLAGGED", 
     "FORMAT", "STRING", NULL);
  }

	/* cal.file always needed for wavelengths: */
  if (ncalfils==1) {
    zladd( dunit, "HISTORY", CAL_FILE, cfile, "FORMAT", "STRING", NULL);
  }
  else {
    for (i=0; i<ncalfils; i++) strcpy( svalue[i], (cfile+cfnoff[i]-1));
    zladd( dunit, "HISTORY", CAL_FILE, svalue, "FORMAT", "STRING",
     "NELEMENT", ncalfils, "ULEN", 101, NULL);
  }
  if ((caltyp && radscal>=0) || dndark || drkchk) {
    if (ndrkfils==1) {
    zladd( dunit, "HISTORY", DARK_FILE, dfile, "FORMAT", "STRING", NULL);
    }
    else {
      for (i=0; i<ndrkfils; i++) strcpy( svalue[i], (dfile+dfnoff[i]-1));
      zladd( dunit, "HISTORY", DARK_FILE, svalue, "FORMAT", "STRING",
       "NELEMENT", ndrkfils, "ULEN", 101, NULL);
    }
  }

	/* solar flux file name: */
  zladd( dunit, "HISTORY", SOL_FILE, solfile, "FORMAT", "STRING", NULL);

	/* despike file name: */
  if (nspifils==1) {
    zladd( dunit, "HISTORY", DESPIKE_FILE, dsfile, "FORMAT", "STRING", NULL);
  }
  else if (nspifils > 1) {
    for (i=0; i<nspifils; i++) strcpy( svalue[i], (dsfile+dsfnoff[i]-1));
    zladd( dunit, "HISTORY", DESPIKE_FILE, svalue, "FORMAT", "STRING",
     "NELEMENT", nspifils, "ULEN", 101, NULL);
  }

	/* deboom file name: */
  if (dbm) zladd( dunit, "HISTORY", DEBOOM_FILE, dbmfile, "FORMAT", "STRING", NULL);

  zladd( dunit, "HISTORY", PHOT_FUNC, phot_func, "FORMAT", "STRING", NULL);
  if (pfunc>0) {
    fword = pfcut;
    zladd( dunit, "HISTORY", PHOT_CUT, &pfcut, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  }
  if (pfunc==2) {
    fword = p_minn;
    zladd( dunit, "HISTORY", MINN_EXP, &fword, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  }

  if (drkchk) zladd( dunit, "HISTORY", "DARK_THRESH", &drkfac, 
     "FORMAT", "REAL", "NELEMENT", 1, NULL);

  x = (float)slew_rate;
  zladd( dunit, "HISTORY", SLEW_RATE, &x, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  x = (float)slew_tol;
  zladd( dunit, "HISTORY", SLEW_TOL, &x, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

  if (radfudge) {
    eq_radf = eq_rad*radfact;
    zladd( dunit, "HISTORY", EQRAD_FUDG, &eq_radf, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  }

  zladd( dunit, "HISTORY", INS_MODE, ins_mode, "FORMAT", "STRING", NULL);
  if (imode==8 || imode==9) zladd( dunit, "HISTORY", STOP_SLIDE,
   stop_slide, "FORMAT", "STRING", NULL);

  zladd( dunit, "HISTORY", GAIN_STATE, gain, "FORMAT", "INT",
	"NELEMENT", ncalfils, NULL);

  zladd( dunit, "HISTORY", CHOPPER_MODE, &chopper, "FORMAT", "INT",
	"NELEMENT", 1, NULL);

  zladd( dunit, "HISTORY", GRATING_OFFSET, &g_off, "FORMAT", "INT",
	"NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", GRATING_START, &gstart, "FORMAT", "INT",
	"NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", GRATING_DELTA, &gdel, "FORMAT", "INT",
	"NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", GRATING_STEPS, &nsteps, "FORMAT", "INT",
	"NELEMENT", 1, NULL);

  ivalue[0] = loclk.rim;
  ivalue[1] = loclk.mod91;
  ivalue[2] = loclk.rti;
  zladd( dunit, "HISTORY", BEG_SCLK, ivalue, "FORMAT", "INT", "NELEMENT", 3, NULL);
  zladd( dunit, "HISTORY", BEG_SCET, beg_utc, "FORMAT", "STRING", NULL);

  ivalue[0] = hiclk.rim;
  ivalue[1] = hiclk.mod91;
  ivalue[2] = hiclk.rti;
  zladd( dunit, "HISTORY", END_SCLK, ivalue, "FORMAT", "INT", "NELEMENT", 3, NULL);
  zladd( dunit, "HISTORY", END_SCET, end_utc, "FORMAT", "STRING", NULL);

  /* if there were any SCLK gaps, write them to label ...
   * first make sure the gaps are actually in the range used: */
  if (nintrvl>1) {
    i1 = comp_sclk(loclk);
    i2 = comp_sclk(hiclk);
    for (i=0; i<nintrvl; i++) {
      if (bclk[i]>i2 || eclk[i]<i1) {	/* interval was not used */
	nintrvl--;
	for (j=i; j<nintrvl; j++) {
	  bclk[j] = bclk[j+1];
	  eclk[j] = eclk[j+1];
	}
      }
    }
  }
  if (nintrvl>1) {
    irim = eclk[0]/91;
    imf = eclk[0]%91;
    gaps[0] = 100*irim+imf;
    j=1;
    for (i=1; i<nintrvl-1; i++) {
      irim = bclk[i]/91;
      imf = bclk[i]%91;
      gaps[j++] = 100*irim+imf;
      irim = eclk[i]/91;
      imf = eclk[i]%91;
      gaps[j++] = 100*irim+imf;
    }
    irim = bclk[nintrvl-1]/91;
    imf = bclk[nintrvl-1]%91;
    gaps[j] = 100*irim+imf;
    zladd( dunit, "HISTORY", SC_GAPS, gaps, "FORMAT", "INT", "NELEMENT", j+1,
     NULL);
  }

  if (!do_fill) {
    fpar[0] = 0;
    fpar[1] = 0;
  }
  zladd( dunit, "HISTORY", FILL_SIZE, &fpar[0], "FORMAT", "INT",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", FILL_NUM, &fpar[1], "FORMAT", "INT",
     "NELEMENT", 1, NULL);

	/* std. dev. bands in cocube: */
  zladd( dunit, "HISTORY", DN_STD_DEV, &lamsd, "FORMAT", "INT",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", GEO_STD_DEV, &geosd, "FORMAT", "INT",
     "NELEMENT", 1, NULL);

  if (footprint) {
    if (histbin) {
      if (!histcrit) strcpy( svalue[0], "FTPTHSTP");
      else strcpy( svalue[0], "FTPTHSTM");
    }
    else strcpy( svalue[0], "FOOTPRNT");
  }
  else {
    if (use_max) strcpy( svalue[0], "MAXIMUM");
    else if (use_last) strcpy( svalue[0], "REPLACE");
    else strcpy( svalue[0], "NEAREST");
  }
  zladd( dunit, "HISTORY", BINNING, svalue, "FORMAT", "STRING", NULL);
  if (footprint) {
    /* write these to the label even for a tube, since they may be
     * needed in a subsequent cube-construction step: */
    if (!histbin) {
      zladd( dunit, "HISTORY", THRESH, &thresh, "FORMAT", "REAL",
       "NELEMENT", 1, NULL);
      zladd( dunit, "HISTORY", SATTHRSH, &sat_thrsh, "FORMAT", "REAL",
       "NELEMENT", 1, NULL);
    }
    zladd( dunit, "HISTORY", FPGRID, &fpgrid, "FORMAT", "INT",
     "NELEMENT", 1, NULL);
    if (ndistor) zladd( dunit, "HISTORY", MAXDISTOR, &maxdistor, "FORMAT",
     "REAL", "NELEMENT", 1, NULL);
  }
	/* get photometric angles from cocube, and then close it: */
  if (!calib) {

    find_angles( rvalue);

    zladd( dunit, "HISTORY", INCI_ANG, &rvalue[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", EMIS_ANG, &rvalue[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", PHAS_ANG, &rvalue[2], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

	/* compute mean azimuths */
    if (n_azi > 0) {
      sun_azi = sun_azi/n_azi;
      sc_azi = sc_azi/n_azi;
    }
    zladd( dunit, "HISTORY", SUN_AZI, &sun_azi, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", SC_AZI, &sc_azi, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

    zladd( dunit, "HISTORY", MIN_RANGE, &minsdist, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", MAX_RANGE, &maxsdist, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

	/* beginning/end SSC lat/lon */
    zladd( dunit, "HISTORY", B_SSCLAT, &ssclat[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", B_SSCLON, &ssclon[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", E_SSCLAT, &ssclat[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", E_SSCLON, &ssclon[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

        /* beginning/end Sub-Solar lat/lon */
    zladd( dunit, "HISTORY", B_SSOLLAT, &ssollat[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", B_SSOLLON, &ssollon[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", E_SSOLLAT, &ssollat[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", E_SSOLLON, &ssollon[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  }
  zvclose( gunit, NULL);

  zladd( dunit, "HISTORY", MIN_SUN_D, &minsun, "FORMAT", "REAL",
   "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", MAX_SUN_D, &maxsun, "FORMAT", "REAL",
   "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", MIN_CB_D, &mincenbod, "FORMAT", "REAL",
   "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", MAX_CB_D, &maxcenbod, "FORMAT", "REAL",
   "NELEMENT", 1, NULL);

  /* P-shift from Cal file: */
  zladd( dunit, "HISTORY", PSHIFT, &pshift, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

  /* "inflation" factor */
  zladd( dunit, "HISTORY", AINFL, &ainfl, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

  /* telemetry threshold values from EDR, only if used: */
  if (threshing) zladd( dunit, "HISTORY", THRESHVAL, threshval, "FORMAT", "INT",
     "NELEMENT", 17, NULL);

  /* temperatures: */
  zladd( dunit, "HISTORY", T_FOCAL, &utemps[0], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", T_RAD_SHIELD, &utemps[1], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", T_TELESCOPE, &utemps[2], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", T_GRATING, &utemps[3], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", T_CHOPPER, &utemps[4], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  zladd( dunit, "HISTORY", T_ELECTRONICS, &utemps[5], "FORMAT", "REAL",
     "NELEMENT", 1, NULL);

  if (lami) zladd( dunit, "HISTORY", WAVELENGTHS, &waves[lami-1], 
   "FORMAT", "REAL", "NELEMENT", 1, "ERR_ACT", "SA", NULL);
  else {
    zladd( dunit, "HISTORY", WAVELENGTHS, waves, "FORMAT", "REAL",
     "NELEMENT", nb, "ERR_ACT", "SA", NULL);
    if (nb<nbb) zladd( dunit, "HISTORY", B_MASK, bmask, "FORMAT", "INT",
     "NELEMENT", nbb, "ERR_ACT", "SA", NULL);
  }

	/* before adding further large label items, we shall do a
	 * close & re-open each time for Vicar buffersize problem */

  if (caltyp && radscal>=0) {
    if (lami) {
      zladd( dunit, "HISTORY", SOLAR_FLUX, &sol_flux[lami-1], 
       "FORMAT", "REAL", "NELEMENT", 1, "ERR_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", RAD_SENS, &sensv[lami-1], "FORMAT",
       "REAL", "NELEMENT", 1, "ERR_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", DRK_AVE, &dave[deteci], "FORMAT",
       "REAL", "NELEMENT", 1, "ERR_ACT", "SA", NULL);
    }
    else {
      zvclose( dunit, NULL);
      zvopen( dunit, "OP", "UPDATE", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", SOLAR_FLUX, sol_flux, "FORMAT",
       "REAL", "NELEMENT", nb, "ERR_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", RAD_SENS, sensv, "FORMAT",
       "REAL", "NELEMENT", nb, "ERR_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", DRK_AVE, dave, "FORMAT",
       "REAL", "NELEMENT", N_DETS, "ERR_ACT", "SA", NULL);
    }
  }

	/* PDS label wants these even if not calibrated: */
  if (!caltyp || radscal<=0) {  
    cmult[0] = 1.0;
    cbase[0] = 0.0;
  } 
  if (!caltyp || radscal<=0 || radscal==3) {
    zladd( dunit, "HISTORY", RAD_BASE, cbase, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
    zladd( dunit, "HISTORY", RAD_CONV, cmult, "FORMAT", "REAL",
     "NELEMENT", 1, NULL);
  }
  else {

    if (lami) {
      zladd( dunit, "HISTORY", RAD_BASE, &cbase[lami-1], "FORMAT",
       "REAL", "NELEMENT", 1, "ERR_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", RAD_CONV, &cmult[lami-1], "FORMAT",
       "REAL", "NELEMENT", 1, "ERR_ACT", "SA", NULL);
    }
    else {
      zvclose( dunit, NULL);
      zvopen( dunit, "OP", "UPDATE", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", RAD_BASE, cbase, "FORMAT", "REAL",
       "NELEMENT", nb, "ERR_ACT", "SA", NULL);
      zvclose( dunit, NULL);
      zvopen( dunit, "OP", "UPDATE", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
      zladd( dunit, "HISTORY", RAD_CONV, cmult, "FORMAT", "REAL",
       "NELEMENT", nb, "ERR_ACT", "SA", NULL);
    }
  }
}


/************************************************************************/
FUNCTION assemble_pntng()
/*
 * read in the pointing for all valid mf's into the buffer mf[]
 *
 * Note that this approach is only useful because:
 * (a) routine set_projection makes several passes thru the data to
 *  figure out projection parameters if user didn't specify them all;
 * (b) it simplifies the slew rate test & computation.
 * If these adavantages are judged not to be compelling, and/or we can
 * get reliable C-angles from SPICE, then may as well delete mf buffer
 * and just get pointing as we go along in extract_data.
 *
 * This routine also checks mfs for missing data, and scan platform flyback 
 * (as determined from SLEW_TOL parameter).  MFs with missing data are
 * flagged but not omitted, as their C-matrices can be used for the
 * interpolation in get_tgt_data.  This latter consideration also means
 * that if an MF is thrown out on any other basis (e.g., slew rate), the
 * one before it must also be eliminated as it will otherwise try to use
 * C-matrix of the following one.
 *
 * In the slew rate test, we look for TWO consecutive MP-pairs whose slew
 * rates exceed the spec -- if found, this disqualifies the PREVIOUS two MFs
 * (the 2nd-to-last because it cannot interpolate pointing).  The rationale
 * here is:  (a) this eliminates torque spikes and prolonged fast slews;
 * (b) it does not discard too many data.
 *
 * Twist is checked in the same way as slew rate for "spikes" in the data
 *
 * REVISION for Phase-2:  since the EDRs can be very sparse, this routine
 * is driven by the AACS data *only*, and all MF's in the interval of interest
 * will be collected.  The EDRs are not read during this process, but must be
 * opened if slew rate checking is done in order to compute body motion.
 */
{
  int acnt=0, boom, del, delmin, gcnt=0, gc, gp0, gp5, i, ii, irt,
   istat, iv, j, jj, jgap, k, mid_mf, mp, nrec0, nrec1, sclk, sclk1, sclk2,
   scnt=0;
  int failed_chk=0;
  double twt_rate=0., ptwt_rate=0., prev_rate=0., cur_rate=0., bm_ra, bm_dec,
   bm_mra, bm_mdec, bm_mra1, bm_mdec1, ddec, dra, x;
  struct pntng_typ cur_ang, prev_ang;
  char segid[41];

  /* for nims_ps_orient calls */
  struct pntng_typ me_ang;
  vector sc, sol, cenbod;
  static matrix me;
  double cenboddis, solscdis;
  int ntotrec;				/* # of MFs in the interval */

	/* allocate storage for array containing pointing info;
	 * this uses special malloc to avoid page file quota limits. */
  sclk1 = comp_sclk(loclk);
  sclk2 = comp_sclk(hiclk);
  if (sclk1>sclk2) zmabend(" *** Bad SCLK range! ***");
  ntotrec = sclk2-sclk1+1;
  mf = (struct mf_type *)
    malloc( ntotrec*sizeof(struct mf_type));
  if (mf==0) zmabend(" *** insufficient scratch space available for MF ***");

  /* initialize the flags to 'valid': */
  for (i=0; i<ntotrec; i++) mf[i].igc = 0;

  zvmessage(" ","");
  zvmessage(" Extracting pointing","");
  bm_mra = bm_mdec = bm_mra1 = bm_mdec1 = 0.0;

  nrec0 = 0;
  nrec1 = 0;

  /* the following call was just put in to initialize NAIF
   * spkapp(ids.sol ...), which otherwise fails when called after
   * assemble_pntng;  problem is possibly due to intermixing C and
   * Fortran i/o, this gets the latter out of the way before most of
   * the former;
   * (use this to initialize min/max sun/cenbod distances, which are
   * otherwise not computed for Calibration data) */
  j = range_lclk;
  if (j<=0) j = file_lclk;
  nims_ps_orient( j, sc, sol, cenbod, &me_ang);
  for (i=0, solscdis=0.; i<3; i++) solscdis += sol[i]*sol[i];
  solscdis = sqrt( solscdis);
  for (i=0, cenboddis=0.; i<3; i++) cenboddis += cenbod[i]*cenbod[i];
  cenboddis = sqrt( cenboddis);
  maxsun = max( solscdis, maxsun);
  minsun = min( solscdis, minsun);
  maxcenbod = max( cenboddis, maxcenbod);
  mincenbod = min( cenboddis, mincenbod);
  eul2mat( me_ang.ra, me_ang.dec, me_ang.twist, me); /* ME: Earth -> Planet */

  j = 0;			/* global valid mf #, spanning all EDRs */
  jgap = 0;			/* counts mfs since last gap */
  mid_mf = 1;		/* flag that we are past the beginning of a new MF */
  file_hclk = 0;		/* to ensure an EDR gets opened at 1st sclk */
  delmin = -1;			/* (in case nochk=1) */

  for (sclk=sclk1; sclk<=sclk2; sclk++) {

    /* check for gaps between user-specified intervals -- treat these like
     * other gaps, see below ... */
    for (iv=0; iv<nintrvl-1; iv++) {
      if (sclk>eclk[iv] && sclk<bclk[iv+1]) {
	vstat = get_pntng( sclk, &cur_ang);
	if (dpt_sclk[0] == 0 || (dpt_sclk[0] <= sclk && dpt_sclk[1] >= sclk)) {
	  cur_ang.ra += dpoint[0];
	  cur_ang.dec += dpoint[1];
	}
	jgap = -1;
	gcnt++;
	goto store_ang;
      }
    }

    if (sclk<file_lclk || sclk>file_hclk) {
      /* since there's no guarantee that the EDRs are in SCLK order, we
       * must search all of them every time we finish the last one ... */
      cur_edr = -1;
      delmin = -1;
      while (cur_edr+1<nedrs) {
	open_edr(0);	/* new EDR, compute file_l/hclk, incr. cur_edr */
	if (sclk>=file_lclk && sclk<=file_hclk) goto sfound;
	del = file_lclk - sclk - 1;		/* track next EDR start */
	if (del>=0 && (delmin<0 || del<delmin))
	  delmin = del;
      }
      /* if we're here, then the sclk is not in any EDR -- treat it
       * like a gap in the pointing (see below), but do read in the
       * pointing so that adjacent valid data don't get thrown out */
      if (delmin<0) break;	/* if after all EDRs, then we're done */
      file_hclk = 0;		/* ensure that EDRs are searched after the gap*/
      vstat = get_pntng( sclk, &cur_ang);
      if (!vstat) {
	cur_ang.ra = -999.;
	continue;
      }
      if (dpt_sclk[0] == 0 || (dpt_sclk[0] <= sclk && dpt_sclk[1] >= sclk)) {
        cur_ang.ra += dpoint[0];
        cur_ang.dec += dpoint[1];
      }
      jgap = -1;
      if (!nochk && (j>0 && failed_chk && mf[j-1].igc>=0)) {
	scnt++;
	mf[j-1].igc = -1;
	if (j>1 && mf[j-2].igc>=0) {
	  scnt++;
	  mf[j-2].igc = -1;
	}
      }
      goto store_ang;

sfound:
      delmin = -1;
      find_body_motion( &bm_ra, &bm_dec, &nrec1, &bm_mra, &bm_mdec);
    }

    /* get the C-angles at this SCLK, saving the previous ones for the
     * slew rate check: */
    prev_ang = cur_ang;
    prev_rate = cur_rate;
    ptwt_rate = twt_rate;
    vstat = get_pntng( sclk, &cur_ang);

    /* check for gap in the pointing: */
    if (!vstat || sclk<file_lclk) {
      acnt++;
      if (!vstat) cur_ang.ra = -999.;
      mf[j].igc = -1;
      jgap = -1;	/* next MF is first after gap */
	/* if previous check was failed, but gap prevents a second check,
	 * then treat this as 2 consecutive failures: */
      if (!nochk && (j>0 && failed_chk && mf[j-1].igc>=0)) {
	scnt++;
	mf[j-1].igc = -1;
	if (j>1 && mf[j-2].igc>=0) {
	  scnt++;
	  mf[j-2].igc = -1;
	}
      }
      goto store_ang;
    }

    /* add in user offset correction (wobble is done in get_tgt_data) */
    if (dpt_sclk[0] == 0 || (dpt_sclk[0] <= sclk && dpt_sclk[1] >= sclk)) {
      cur_ang.ra += dpoint[0];
      cur_ang.dec += dpoint[1];
    }

    if (nochk) goto store_ang;

	/* compute slew rates and compare versus slew_tol --
	 * if both current and previous rates exceed tolerance, then
	 * this disqualifies the PREVIOUS two MFs ... the current
	 * mf will discarded too if isolated, on the 2nd pass;
	 * (also check twist for "spikes") */
    failed_chk = 0;
    if (jgap>0) {
      dra = cur_ang.ra - prev_ang.ra;
      /* first check for wraparound at 360 deg. */
      if (dra>zpi()) dra -= ztwopi();
      if ((-dra)>zpi()) dra += ztwopi();
      dra = (dra - bm_ra) * cos(cur_ang.dec);
      ddec = cur_ang.dec - prev_ang.dec - bm_dec;
      cur_rate = sqrt( dra*dra + ddec*ddec);
      /* here is exact equation (for historical purposes) -- was replaced
       * with small-angle approximation to allow for target body motion */
      if (idebug)
        cur_rate = acos( sin(prev_ang.dec) * sin(cur_ang.dec) +
         cos(prev_ang.dec) * cos(cur_ang.dec) * cos(cur_ang.ra-prev_ang.ra));
      twt_rate = cur_ang.twist - prev_ang.twist;
      if (twt_rate < 0) twt_rate = -twt_rate;
      if (twt_rate>zpi()) twt_rate -= ztwopi();      /* check for wraparound */

    }
    if (jgap>1) {	/* since prev_rate = 0 for jgap=1) */
	/* here is the actual slew rate check: */
      if ( (slew_tol>0.0 && cur_rate>slew_tol && prev_rate>slew_tol) || 
	   (twt_tol>0.0  && twt_rate>twt_tol  && ptwt_rate>twt_tol) ) {
	for (ii=1; ii<3; ii++) {
	  if (mf[j-ii].igc>=0) {
	    mf[j-ii].igc = -1;
	    scnt++;
	  }
	}
      }
	/* signal if only failed current check, in case of gap following: */
      if ( (slew_tol>0.0 && cur_rate>slew_tol) ||
	   (twt_tol>0.0  && twt_rate>twt_tol) )
	failed_chk = 1;
    }

store_ang:
	/* store the C-angles */
    mf[j].sclk = sclk;
    mf[j].cang = cur_ang;
    if (calib || jgap<0) mf[j].igc = -1;
    j++;			/* j is only incremented once per mf! */
    jgap++;
    /* advance sclk to next EDR if necessary: */
    if (delmin>0) sclk += delmin;
  }

  nmf = j;	/* has been incremented after last use */

  if (calib) return;

  /* second pass thru the data to tidy up pointing checks and
   * compute mean slew rate ... */

  slew_rate = 0.0;
  if (bm_mra1!=0.0 || bm_mdec1!=0.0) {
    bm_mra = bm_mra1;
    bm_mdec = bm_mdec1;
  }

  for (ii=0, i=0; i<nmf; i++) {
    /* if 2nd mf is bad, then suppress 1st mf on general principles (since
       it could not be checked), unless NOCHK ... */
    if (i==0 && mf[i].igc>=0 && mf[i+1].igc<0 && !nochk) {
      mf[i].igc = -1;
      acnt++;
    }
	/* set MFs with bad pointing on either side to invalid in order
	 * to avoid confusing get_tgt_data ... */
    if (i<(nmf-1) && i>0 && mf[i].igc>=0 && (mf[i-1].cang.ra<=-999. ||
        mf[i+1].cang.ra<=-999.)) {
      mf[i].igc = -1;
      acnt++;
    }
    if (i>0 && mf[i].igc>=0 && mf[i-1].igc>=0) {
      dra = mf[i].cang.ra - mf[i-1].cang.ra;
      /* check for wraparound */
      if (dra>zpi()) dra -= ztwopi();
      if ((-dra)>zpi()) dra += ztwopi();
      dra = (dra - bm_mra) * cos(mf[i].cang.dec);
      ddec = mf[i].cang.dec - mf[i-1].cang.dec - bm_mdec;
      slew_rate += sqrt( dra*dra + ddec*ddec);
      ii++;
    }
  }
  if (!nochk) {
    /* last point is not used if not extrapolating, or if previous one
     * is bad */
    if (!c_extrap) mf[nmf-1].igc = -1;
    if (mf[nmf-2].igc==-1) mf[nmf-1].igc = -1;
  }
  if (ii>1) slew_rate /= (double)ii;

	/* convert slew rate to units of Nyquist rate: */
  slew_rate *= 2000.;
  if (!fix_mode) slew_rate *= (double)ngps;
  sprintf( msg, " mean slew rate = %.2f * Nyquist, computed from %d pairs of mfs",
   slew_rate, ii);
  zvmessage( msg,"");

  if (scnt) {
    sprintf( msg, " %d mfs rejected by slew rate test", scnt);
    zvmessage( msg,"");
  }
  if (acnt) {
    sprintf( msg, " %d mfs rejected for missing pointing", acnt);
    zvmessage( msg,"");
  }
  if (gcnt) {
    sprintf( msg, " %d mfs fell in gaps between user-specified intervals", gcnt);
    zvmessage( msg,"");
  }

  k = nmf-scnt-acnt-gcnt;
  if (k<=0) zmabend(" ** no valid mfs found! **");
  sprintf( msg, " %d mfs will be used", k);
  zvmessage( msg,"");

	/* this is a convenient place for some test options: */
  if (xtest==4) {
    i = mf[0].sclk/91;
    j = mf[0].sclk%91;
    setup_cal( i, j);
    zvmessage("  BAND #   WAVE     BASE        MULT  ","");
    for (i=0; i<nbb; i++) {
      sprintf( msg, " %5d  %8.5f  %9.3f  %13.5e", i+1, waves[i], cbase[i],
       cmult[i]);
      zvmessage( msg,"");
    }
    return;
  }
  if (xtest==5 || xtest==7) {
    zvmessage("    SCLK       MFC      RA         Dec      Twist      Cone       Clock Valid ","");
    for (i=0; i<nmf; i++) {
      if (xtest==7 && mf[i].igc<0) continue;
      j = mf[i].sclk/91;
      jj = mf[i].sclk%91;
      ii = mf[i].sclk - mf[0].sclk;
      sprintf( msg, " %d.%02d %5d  %9.3f  %9.3f  %9.3f  %9.3f  %9.3f  %d",
       j, jj, ii, degrad*mf[i].cang.ra, degrad*mf[i].cang.dec,
       degrad*mf[i].cang.twist, mf[i].cang.cone, mf[i].cang.clock, mf[i].igc);
      zvmessage( msg,"");
    }
    return;
  }
}


/************************************************************************/
FUNCTION average_dn( word, cdata, wt, wtx, det)
/*
 * we need a special function to average DNs because of the problem
 * of the sensitivity change around DN=512 in the thermal detectors
 */

float *word, cdata, wt, wtx;
int det;
{
  int i;
  float offx[2], temp;
	/*
	 * these are the average ratios of low to high sensitivities in the
	 * the thermal detectors (14-16 as pgm. counts det):
	 * (NOTE that there are small variations by gp -- if desired, the
	 * full table of (gp,det) sensitivities could be read in, but this
	 * seems like overkill, given the rounding errors)
	 */
  static float sfact[3] = { 46.81, 47.33, 48.22};
	/* threshold between low & high gains: */
  static float dnx[3] = { 513., 513., 510.};

  i = det-14;
  if (i<0) zmabend(" invalid call to average_dn");

  offx[0] = *word-dnx[i];
  offx[1] = cdata-dnx[i];

  if (offx[0]*offx[1] >= 0) {
    *word = (wtx*cdata + wt * *word) / (wtx+wt); 
    return;
  }
	/* we have one DN in each gain regime:  convert the high-gain
	 * DN to the low-gain, perform the average, and then convert
	 * back if result is in high-gain regime: */
  if (offx[0]>0) {
    temp = dnx[i] + sfact[i]*offx[0];
    temp = (wtx*cdata + wt * temp) / (wtx+wt); 
  }
  else {
    temp = dnx[i] + sfact[i]*offx[1];
    temp = (wtx*temp+ wt * *word) / (wtx+wt); 
  }
  if (temp > dnx[i]) temp = dnx[i] + (temp-dnx[i]) / sfact[i];
  *word = temp;
}


/************************************************************************/
FUNCTION bb_repl( rad, igp)
/*
 * replace saturated radiances by estimate based on BB function
 *
 * some assumptions/approximations made:
 * 1. thermal cutoff is 3.0 mu
 * 2. if only 1 unsaturated thermal band present, this is saturated too
 * 3. mean albedo from non-thermal bands is extrapolated to thermal
 * 4. bands below 1.0 mu excluded from albedo estimate (this is for
 *   stuck-grating case)
 * 5. if no valid radiances in non-thermal bands, use albedo = 0.2 
 *   (this is unlikely to happen)
 * 6. if corrected thermal radiance is negative, use T=120K (min. T
 *   detectable by nims)
 */
float rad[N_DETS];
int igp;
{
  /* double c10=0.00001191;		/* 2 * h * c**2 */
  double c10=1.191e10;
  double c20=1.43875;			/* h * c / k */
  double alb, c1, c2, rad1, xtemp, wav;
  float xrads[N_DETS], xtemps[N_DETS];	/* (diagnostic tools) */
  int det, i, ib, isat, nn, nsat, nthrm;

  /* first check to see if we need to do anything at all */
  nsat = 0;		/* # of saturated thermal bands */
  nthrm = 0;		/* # of valid thermal bands */
  for (det=s_det; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    ib = wet[igp][det];
    wav = waves[ib];
    if (wav<3.0) continue;	/* thermal limit hard-coded to 3.0 mu */
    if (rad[det]==(float)INS_HI_SAT4) nsat++;
    if (rad[det]<VALID_MIN4) continue;
    isat = det;	/* in case only one valid value */
    nthrm++;
  }
  if (!nsat || !nthrm) return;
  if (nthrm==1) {	/* if only one unsaturated */
    rad[isat] = INS_HI_SAT4;	/* saturate it too */
    return;
  }

  /* at this point we have at least one saturated thermal band and
   * at least two unsaturated ones -- now subtract reflected sunlight
   * component from radiances, get mean BB temperature, and compute
   * radiances at saturated wavelengths */

  alb = 0.0;
  for (det=s_det, nn=0; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    ib = wet[igp][det];
    wav = waves[ib];
    if (wav<1.0) continue;	/* exclude det.1 & 2 */
    if (wav>3.0) break;
    if (rad[det]<VALID_MIN4) continue;
    alb = alb+rad[det]/sol_flux[ib];
    nn++;
  }
  if (!nn) {
    ndefalb++;
    alb = 0.2;
  }
  else {
    alb /= nn;
    repl_alb += alb;
    ncompalb++;
  }
  /* compute mean brightness T */
  for (i=1;i<N_DETS;i++) xrads[i] = xtemps[i] = 0.0;
  xtemp = 0.0;
  for (det=s_det, nn=0; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    ib = wet[igp][det];
    wav = waves[ib];
    if (wav<3.0) continue;
    if (rad[det]<VALID_MIN4) continue;
    rad1 = rad[det]-alb*sol_flux[ib];
    xrads[det] = rad1;
    if (rad1<0.0) {
      xtemps[det] = 120.0;
      xtemp += xtemps[det];
      nn++;
      continue;
    }
    /* wav *= 0.0001;	causes floating overlow! */
    /* rad1 *= 100000.0; */
    c1 = c10/(wav*wav*wav*wav*wav*rad1);
    c2 = (1.0e4*c20)/wav;
    xtemps[det] = c2/log(c1+1.);
    xtemp += xtemps[det];
    nn++;
  }
  if (nn<=0) zmabend(" error in bb_repl");	/* shouldn't happen */
  xtemp /= nn;
  /* compute replacement radiances */
  for (det=s_det, nn=0; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    ib = wet[igp][det];
    wav = waves[ib];
    if (wav<3.0) continue;
    if (rad[det]!=(float)INS_HI_SAT4) continue;
    /* wav *= 0.0001; */
    c1 = c10/(wav*wav*wav*wav*wav);
    c2 = (1.0e4*c20)/wav;
    rad1 = c1/(exp(c2/xtemp)-1.);
    /* rad1 *= 1.0e-5;		/* convert back to NIMS units */
    rad1 += alb*sol_flux[ib];	/* add in solar background */
    rad[det] = rad1;
    nsatrep++;
  }
  return;
}


/************************************************************************/
FUNCTION check_thresh()
/*
 * go thru weights cube and delete all data DNs whose weight is below
 * threshold
 */ 
{
  float *dptr, word, wt, *wptr;
  int det, gp, ix, iy, ib, offs;

  for (iy=0; iy<nl; iy++) {
    for (ix=0; ix<ns; ix++) {
      for (ib=0; ib<nb; ib++)  {
	offs = ib*nl*ns + iy*ns + ix;
	wptr = wdata + offs;
	wt = *wptr;
	if (wt>0. && wt<thresh) {
	  if (org==0) 		/* BSQ */
	    dptr = odata + ib*ns*nl + iy*ns + ix;
	  else if (org==1) 		/* BIL */
	    dptr = odata + ix + ib*ns + iy*ns*nb;
	  else if (org==2) 		/* BIP */
	    dptr = odata + ib + ix*nb + iy*ns*nb;
/*	  *dptr = BELOW_THRESH4; 	-- REMOVED FOR UNIX ISIS */
	  *dptr = NULL4;
	}
      }
    }
  }
}



/************************************************************************/
FUNCTION comp_stdev()
/*
 * go thru std.dev. cube and convert mean square values to s.d. of
 * measurement = s.d. of ensemble / sqrt(N)
 * = sqrt( variance / (sum weights) )
 */ 
{
  float *dptr, sword, *sptr, word, *wptr, wt;
  int det, gp, ix, iy, ib, offs;

  for (iy=0; iy<nl; iy++) {
    for (ix=0; ix<ns; ix++) {
      for (ib=0; ib<nb; ib++) {
	if (org==0) 			/* BSQ */
	  offs = ib*ns*nl + iy*ns + ix;
	else if (org==1) 		/* BIL */
	  offs = ix + ib*ns + iy*ns*nb;
	else if (org==2) 		/* BIP */
	  offs = ib + ix*nb + iy*ns*nb;
	dptr = odata + offs;
	word = *dptr;
	if (word<=0.0) continue;
	wptr = wdata + ib*ns*nl + iy*ns + ix;	/* WTS cube is always BSQ */
	wt = *wptr;
	sptr = sddata + offs;
	sword = *sptr;
	sword -= word*word;
	sword = max( sword, 0.0);		/* in case of roundoff error*/
	sword = sqrt(sword/wt);
	*sptr = sword;
} } } }



/************************************************************************/
FUNCTION extract_data()
/*
 * Extract and radiometrically correct the data, then write to the
 * output cube.
 *
 * For a G-cube output, the input pixels are "binned", i.e., the output
 * location is found separately for each input pixel.  There are two options:
 *
 *  1. "Nearest-neighbour", in which only the single output pixel
 * corresponding to the center of the input pixel is filled.  In case
 * of overlap, several options exist;  if averaging is selected, then
 * each input pixel has unit weight.
 *
 *  2. The footprint of the input pixel is computed and the fraction
 * of the total area falling in a given output pixel (weighted by
 * the detector sensitivity function) is used as a weight in averaging
 * overlapping pixels.
 */
{
	/* mirror pos. of pixels (centers or corners): */
  struct xytype mpos[4][20], mpos1[20];

  short s1data[20][N_DETS], s2data[N_DETS][20];
  float f1data[20][N_DETS], f2data[N_DETS][20], dist, delx, dely, detnt,
   mat[2][2], p, q, w1[2], w2[2], xskip;
  double bcone=0.0, bxcone=0.0, dbc=0.0, dbx=0.0, et1, mnerr=0.0, mnerr2=0.0,
   tssb[6];
  int ccnt=0, dcnt=0, det, discnt=0, drkcnt=0, empty, gp, i, idel, idistor, igp,
   imp, irec, irim, irim0, irim1, imf, imf0, isclk, ispik, istat, j, j1, j2,
   jrti, jsclk, nchops, new_edr, new_rim, ngc, ngc1, oops=0, offplan=0,
   offimag=0, perc, perc0, rec, rgp, rti, rti0, rval, scnt=0, skpcnt=0, 
   vcnt=0, vflag[20], xbadpk=0, xinvpik=0, xokpk=0, xzpik=0, xpik=0;
  int comperr[1000], comperrgp[1000];
  int cur_rim = -1;
  int first_twtd = 1;

  zvmessage(" ","");
  zvmessage(" Beginning data extraction","");

	/* if no calibration done, call RXM's calibration setup 
	 * once in order to get wavelengths: */
  if (!caltyp || radscal==-1) {
    cur_rim = mf[0].sclk/91;
    i = mf[0].sclk%91;
    setup_cal( cur_rim, i);
  }

	/* zero-point for nchops (tube option) -- we now force the tube
	 * to start on a new GC (in main), so don't really need these
	 * special variables, but keep them to minimize code changes ... */
  irim0 = loclk.rim;
  imf0 = loclk.mod91;
  rti0 = loclk.rti;

	/* initialize flags for begin/end quantities in get_tgt_data &
	 * find_xy: */
  ll_rec1 = ll_rec2 = 0;

  perc = 0;
  ngcs = 0;			/* reset grating cycle count (for tube) */
  ngc = irim = -1;		/* more tube flags */
  jsclk = 0;			/* initialize EDR record SCLK counter */
  file_hclk = 0;		/* to ensure an EDR gets opened at 1st rec */
  ispik = -1;
  sol_flux[0] = 0.0;		/* flag to call get_sflux

  /* loop over all pointing records (MFs): */
  for (rec=0; rec<nmf; rec++) {

    if (mf[rec].igc < 0) {
      if (!tube)
	continue; /* already counted in assemble_pntng */
    }

    isclk = mf[rec].sclk;
    if (!isclk) continue;

    new_edr = 0;
    if (isclk<file_lclk || isclk>file_hclk) {
      /* since there's no guarantee that the EDRs are in SCLK order, we
       * must search all of them every time we finish the last one ... */
      jsclk = 0;			/* initialize EDR record SCLK counter */
      cur_edr = -1;
      while (cur_edr+1<nedrs) {
	open_edr(1);	/* new EDR, compute file_l/hclk, incr. cur_edr */
	if (isclk>=file_lclk && isclk<=file_hclk) break;
      }
      if (isclk<file_lclk || isclk>file_hclk) {
	if (nochk || tube) {		/* not in any EDR */
	  file_hclk = 0;	/* make sure an EDR is opened at next SCLK */
	  continue;
	}
	else {
	  /* all records should be in an EDR after assemble_pntng ... */
	  zmabend(" EDR bookkeeping error!");
	}
      }
      /* read in associated spike file, if any */
      if (nspifils>1 && strcmp( dsfile, "DUMMY_DSPK.DAT")) {
	if (pdespike!=0) free(pdespike);
        read_dspk(dsfile+dsfnoff[cur_edr]-1);	/* this resets nspik */
	ispik = -1;		/* so reset this counter too */
      }
      new_edr = 1;
      irec = 1;		/* set record counter to start of EDR */
      if (ncalfils>1) {
	/* get new Gain state: */
	strncpy( msg, &edrhdr.hdr2.gain, 1);
	msg[1] = '\n';
	gain[cur_edr] = atoi(msg);
	sprintf( msg, " Gain state = %d", gain[cur_edr]);
	zvmessage( msg,"");
      }
    }

    /* read records from EDR until we find a valid data record at
     * this pointing record -- if none, then flag it and skip to next
     * pointing record: */
    xskip = 0.0;
    while (jsclk<isclk) {
      get_nims_edr_rec_2( edrs[cur_edr].unit, irec++, &edrrec);
      jsclk = comp_sclk(edrrec.pfix.sclk);
      if (!jsclk) {
	scnt += 20;	/* increment skipped-combs count */
	xskip += 0.5;
      }
      /* only process 1 MF at a time in this loop */
      /* this algorithm is broken!  remove it for now -lwk- dec2011 */
      /*if (xskip >= 1.0) break;*/
    }
    /*if (xskip >= 1.0) continue;*/
    if (jsclk>isclk) {		/* EDR skipped over this MF */
      /* if the gap is >1 MF, don't bother to try to count skipped records,
       * as omitted Wait-state scans are too hard to keep track of 
       * (this still causes 1 extra MF to be counted at end of a gap, but
       * that's not significant enough to worry about) */
      if (jsclk > (isclk+1)) continue;
      /* if MF is Wait state, don't count it as skipped 
       * (in full/short mode, this will be slightly hit-and miss,
       * depending on whether Wait state is in 1st or 2nd rti) */
      rti = edrrec.pfix.sclk.rti;
      gp = (2*(isclk%91) + rti/5) % ngps;
      if (gp >= nsteps) {		/* Wait state */
	if (fix_wait) vcnt += 40;	/* increment WAIT-combs count */
      }
      else 
	scnt += 40;			/* increment skipped-combs count */
      continue;				/* to next pointing record */
    }

	/* get HSK/Eng data for calibration:
	 * (OLDCAL:  did this at start of every new RIM) */
    if (oldcal) new_rim = isclk/91;
    else new_rim = 0;		/* trick to fall thru new_rim test) */
    if (caltyp && radscal>=0 && ((new_rim != cur_rim) ||
        (ncalfils>1 && new_edr))) {
      i = isclk%91;			/* mod91 */
      j = isclk/91;
      setup_cal( j, i);		/* RXM's calibration calls */
      cur_rim = new_rim;
      /* compute solar flux for label (and bb_repl, if used) */
      if (sol_flux[0]==0.0) get_sflux();
    }

    /* if boom map supplied (dbm), store the boresight clock/cone angles */
    if (dbm && mf[rec].igc >= 0) {
      bcone = mf[rec].cang.cone;
      bxcone = mf[rec].cang.clock;
      if (rec < nmf-1) {
	dbc = mf[rec+1].cang.cone-bcone;
	dbx = mf[rec+1].cang.clock-bxcone;
      }
      else {
	dbc = bcone-mf[rec-1].cang.cone;
	dbx = bxcone-mf[rec-1].cang.clock;
      }
      /* X-cone should be decreasing with time, so increase means
       * wraparound at 360 deg.: */
      if (dbx>0.) dbx -= 360.;
    }

    perc0 = perc;
    perc = 0.5 + (100*rec)/nmf;
    if (perc/10 != perc0/10) {
      sprintf( msg, " %3d%% of data extracted", perc);
      zvmessage( msg,"");
    }

    for (rti=0; rti<=5; rti+=5) {  /* 2 scans (EDR records) per mf */

      /* examine the 2 EDR records for this MF separately */
      jrti = edrrec.pfix.sclk.rti;	/* this is the RTI read in above */
      if (rti<jrti) continue;	/* rti=0, already processed */
      if (rti>jrti) {		/* rti=5 */
	/* check if EDR ends in the middle of a MF: */
	if (isclk==file_hclk && !file_hrti) break;
	get_nims_edr_rec_2( edrs[cur_edr].unit, irec++, &edrrec);
	jsclk = comp_sclk(edrrec.pfix.sclk);
	if (!jsclk) {
	  scnt += 20;		/* increment skipped-combs count */
	  continue;
	}
	jrti = edrrec.pfix.sclk.rti;
	if (jsclk!=isclk || jrti!=rti) {
	  scnt += 20;		/* increment skipped-combs count */
	  continue;
	}
      }

      /* check compression status */
      if (edrrec.pfix.compression_stat != 0) {
	ccnt++;
	if (chk_comp) continue;
	else {
	  if (tube) {
	    if (ccnt>1000) zvmessage(" COMPERR buffer overflow!","");
	    else {
	      i = ngcs;				/* preceding line count */
	      if (fix_mode || !gp) i++;		/* this is a new line */
	      comperr[ccnt-1] = i;
	      comperrgp[ccnt-1] = gp;
	    }
	  }
	}
      }

      /* copy sensor data to local buffer: */
      zmve( 2, 340, edrrec.sensor, s1data, 1, 1);

      /* quick check if any valid data in record */
      empty = 1;
      for (i=0; i<340; i++)
        if (edrrec.sensor[i] != 0) empty = 0;
      if (empty) {
	scnt += 20;		/* increment skipped-combs count */
	continue;
      }

      gp = (2*(isclk%91) + rti/5) % ngps;
      if (gpos[gp]<0) {
	scnt += 20;		/* increment skipped-combs count */
	continue;
      }
      igp = fix_mode ? 0 : gp;

      /* option to skip some GPs: */
      if (nskipgp) {
	if (gp%nskipgp != 0) {
	  skpcnt += 20;
	  continue;
	}
      }

	/*
	 * Determine mirror direction
	 */

      /* wait state takes precedence over fixed/scanning, since
       * it must be skipped if grating is not fixed */
      if (gp >= nsteps) {
	mirror = WAIT;
	if (!fix_wait) {  /* only process if grating fixed (and requested) */
	  vcnt += 20;
	  if (!tube) continue;
	}
      }
      else mirror = STOP;		/* (actually UNDEFINED) */
      if (mirror != WAIT) mirror = (gp%2)==0 ? DOWN : UP;

      /* for spectrometer mode, store this value in "pseudo-mirror" and
       * allow true mirror to be stopped -- this affects the direction
       * in which data are written to the tube & cotube */
      pmirror = mirror;				/* pseudo-mirror */
      if (!mirrop) mirror = STOP;		/* fixed mirror mode */

      /* by convention, the mirror positions are given for the DOWN
       * scan, so reverse them if scanning UP: */
      smpos = smpos0;
      empos = empos0;
      if (pmirror == UP) {
	smpos = 19-empos0;
	empos = 19-smpos0;
      }

	/* 
	 * initialize the geometry cocube buffer for find_xy --
	 * it gets filled in find_xy & get_tgt_data 
	 */

      for (i=smpos; i<=empos; i++) {
	for (j=0; j<3; j++) {
	  cobuf[i].u_sun[j] = 0.;
	  cobuf[i].rs[j] = 0.;
	  cobuf[i].geom[j] = NULL4;
	}
	cobuf[i].rads[0] = 0.;
	cobuf[i].rads[1] = NULL4;
	cobuf[i].photcor = 0.;
	/* also mpos flags for case of tube unprojected points */
	mpos[0][i].x = 0.;
	mpos[0][i].y = 0.;
      }

      /* for tube case, it is possible to reach this point in the code
       * for an unprojectable pixel -- in this case, skip the
       * projection steps and initialize mpos.y to NULL to avoid
       * confusing output_cotube (mpos.x = mpos.y = BADP means
       * off-planet, for which we still want pointing backplanes */
      if (tube && mf[rec].igc < 0) {
	for (i=smpos; i<=empos; i++) {
	  mpos[0][i].x = BADP;
	  mpos[0][i].y = NULL4;
	}
      }
      else {
	/*
	 * determine the coordinates for all 17 detectors
	 * (in case of footprint, use the 4 corners of pixel);
	 *
	 * if boom map supplied (dbm), then pass the boresight
	 * clock/cone angles to find_xy & get array of rejected
	 * pixels
	 */
	if (footprint)
	  find_xy_corners( isclk, rti, mpos, bcone, bxcone, dbc, dbx);
	else
	  find_xy_center( isclk, rti, mpos, bcone, bxcone, dbc, dbx);
      }

	/*
	 * before calibration and projection, we allow various sorts
	 * of "special screening" of the data:
	 */

      /* check for DNs set to special values in phase-2 (this is on
       * pixel basis, not comb!) -- do this before despiking so as not
       * to confuse that algorithm! */
      for (i=smpos; i<=empos; i++) {
	for (det=s_det; det<=e_det; det++) {
	  if (skipdet[det]>0) continue;
	  if (threshing && s1data[i][det] == 4) s1data[i][det] = INS_LO_SAT2;
	  else if (s1data[i][det] <= DN_MIN) s1data[i][det] = NULL2;
	}
      }

      /* check for spikes */
      if (nspik) {
	irim1 = isclk/91;
	imf = isclk%91;
	while (ispik < nspik) {
	  ispik++;	/* (initialized to -1) */
	  if (pdespike[ispik].rim > irim1) break;
	  if (pdespike[ispik].rim < irim1) continue;
	  if ((int)pdespike[ispik].mod91 > imf) break;
	  if ((int)pdespike[ispik].mod91 < imf) continue;
	  i = 5*(((int)pdespike[ispik].rti)/5);
	  if (i>rti) break;
	  if (i<rti) continue;
	  /* current spike is in this mirror scan */
	  i = 4*((int)pdespike[ispik].rti - i) + (int)pdespike[ispik].mp;
	  /* if (mirror == DOWN) i = maxmpos-1-i;	(apparently not!) */
	  det = (int)pdespike[ispik].det - 1;
	  if (!pdespike[ispik].newdn) {		/* zeroed spike:  WDS-type */
	    s1data[i][det] = 0;
	    xzpik++;
	  }
	  else if (s1data[i][det] < VALID_MIN2) {
	    s1data[i][det] = pdespike[ispik].newdn;
	    xinvpik++;
	  }
	  else {
	    idel = s1data[i][det] - pdespike[ispik].olddn;
	    if (idel>1 || idel <-1) {
	      /* flag spikes whose advertised old-DN doesn't match that in
	       * the EDR -- they are either due to dispersion in dark values 
	       * or are likely to be products of failed merges or cases where
	       * old-DN = new-DN as a result of a merge ... */
	      xbadpk++;
	      mnerr += idel;
	      mnerr2 += idel*idel;
	      /* because of large dispersion of dark values in some cases,
	       * especially Gain=3, don't reject all deviant points */
	      if (idel>drk_tol || idel<-drk_tol) continue;
	      else {	/* interpret delta as deviation of dark from mean */
	        pdespike[ispik].newdn += idel;
	        xokpk++;
	      }
	    }
	    s1data[i][det] = pdespike[ispik].newdn;
	    xpik++;
	  }
	}
        /* unless we've processed all spikes, the above loop will always
	 * be exited on an unused spike, so back up the counter: */
	if (ispik<nspik) ispik--;
      }

      /* check incidence/emission cutoffs: */
      if (cutinc || cutemi) {
	for (i=smpos; i<=empos; i++) {
	  if ( (cutinc && cobuf[i].geom[0] > inccut) ||
	       (cutemi && cobuf[i].geom[1] > emicut) ) {
	    for (det=s_det; det<=e_det; det++) s1data[i][det] = NULL2;
	  }
	}
      }

      /* option to throw out some mirror positions: */
      for (i=0; i<mirrsel; i++) {
	imp = nomirr[i]-1;
	if (imp < 20) {
	  if (rti == 5) continue;
	}
	else {
	  if (rti == 0) continue;
	  else imp -= 20;
	}
	for (det=s_det; det<=e_det; det++) s1data[imp][det] = NULL2;
      }

      /* option to skip certain detectors: */
      for (i=smpos; i<=empos; i++)
	for (det=s_det; det<=e_det; det++) {
	  if (skipdet[det]>0) s1data[i][det] = NULL2; 
	}

      /* option for dark-value threshold */
      if (drkchk) {
	for (i=smpos; i<=empos; i++) {
	  for (det=s_det; det<=e_det; det++) {
	    if (skipdet[det]>0) continue;
	    if (s1data[i][det] <= drkfac*darktab[i+4*rti][det]) {
	      s1data[i][det] = NULL2;
	      drkcnt++;
      } } } }
	/* end of "special screening" */

	/*
	 * Calibration step:
	 *
	 * 1. all cases except DN Tube are converted to float;
	 * 2. if CAL, transpose and pass to RM's cal routine;
	 *  2a. if GCUBE, transpose back.
	 *
	 * So there are two special cases:
	 * A. DN G-cube: just convert to float;
	 * B. DN Tube: just transpose.
	 * These are also done in this step, for convenience.

	 * the histogram-binning option is treated as a DN G-cube in
	 * this routine, with the difference that in output_near
	 * the data are written to histograms stored in the weights
	 * file;  this means that an unnecessary conversion from
	 * integer to float (and back again) is done, but the
	 * enormous simplication of the coding makes this worth
	 * while (for now)
	 */

      if (caltyp && radscal>=0 && !histbin) {	/* radiometrically calibrate the data */
	/* initialize arrays */
	for (det=0; det<N_DETS; det++) {
	  for (i=0; i<20; i++) {
	    f2data[det][i] = NULL4;
	    s2data[det][i] = NULL2;
	  }
 	}
	for (i=smpos; i<=empos; i++) {
	  /* if boom-obscured, leave as NULL so cal. routines can recognize
	   * and skip it */
	  if (mpos[0][i].x!=(float)BADP || mpos[0][i].y!=0.) {
	    for (det=s_det; det<=e_det; det++) {
	      if (skipdet[det]>0) continue;
	      if (s1data[i][det]==1023 && max_sat) s1data[i][det]=1022;
	      s2data[det][i] = s1data[i][det];	/* transpose matrix */
	    }
	  }
	}
	rgp = igp+1;
	/* do the calibration: */
	znims_comp_rad( rgp, 1, s2data, f2data, &istat);
	if (istat) {			/* istat=0 is ok */
	  sprintf( msg, " error in NIMS_COMP_RAD, code = %d", istat);
	  zmabend( msg);
	}
	/* set NO_SENS to NULL for Unix ISIS ... */
	/*for (i=smpos; i<=empos; i++)
	  for (det=s_det; det<=e_det; det++) {
	    if (skipdet[det]>0) continue;
	    if (f2data[det][i]==(float)NO_SENSITIVITY4) f2data[det][i] = NULL4;
	  }*/
	if (!tube)
	  for (i=smpos; i<=empos; i++)
	    for (det=s_det; det<=e_det; det++)
	      f1data[i][det] = f2data[det][i];
      }
      /* cases of "fake radiances" or DN G-cube */
      else if (radscal == -1 || !tube) {
	for (i=smpos; i<=empos; i++) 
	  for (det=s_det; det<=e_det; det++) {
	    if (skipdet[det]>0) continue;
	    if (s1data[i][det] == NULL2)
	      if (histbin) f1data[i][det] = (float)NULL2;
	      else f1data[i][det] = NULL4;
	    else if (s1data[i][det] == INS_LO_SAT2)
	      if (histbin) f1data[i][det] = (float)INS_LO_SAT2;
	      else f1data[i][det] = INS_LO_SAT4;
	    else if ((s1data[i][det] >= 1023) && flag_sat)
	      if (histbin) f1data[i][det] = (float)INS_HI_SAT2;
	      else f1data[i][det] = INS_HI_SAT4;
	    else if (s1data[i][det] <= 0)
	      if (histbin) f1data[i][det] = (float)NULL2;
	      else f1data[i][det] = NULL4;
	    else {
	      f1data[i][det] = (float)s1data[i][det];
	      if (dndark) f1data[i][det] -= darktab[i+4*rti][det];
	    }
	    if (tube) f2data[det][i] = f1data[i][det];
	  }
      }
      /* DN Tube should be only case left -- just transpose matrix */
      else if (tube) {
	for (i=smpos; i<=empos; i++) {
	  if (mpos[0][i].x==(float)BADP && mpos[0][i].y==0.) /*boom-obscured*/
	    for (det=s_det; det<=e_det; det++) {
	      if (skipdet[det]>0) continue;
	      s2data[det][i] = NULL2;
	    }
	  else {
	    for (det=s_det; det<=e_det; det++) {
	      /* omit these checks -- DNs cannot exceed 1023, and if they are 
	       * <0 then it's because they've been set to that value above,
	       * and we shouldn't overwrite it!
	      if (skipdet[det]>0) continue;
	      if (flag_sat && s1data[i][det] >= 1023)
		s2data[det][i] = INS_HI_SAT2;
	      else if (flag_sat && s1data[i][det] <= 0)
		s2data[det][i] = NULL2;
	      else *****/
		s2data[det][i] = s1data[i][det];
	    }
	  }
	}
      }
      else	/* just in case */
	zmabend(" Logic error in calibration step!");
	/* end of Calibration step */

	/*
	 * Final step:
	 * apply the photometric correction, and save the data
	 * to the cube & cocube files:
	 */

      if (tube) {	 	/* special case for TUBE option: */
	/* line # = grating cycle count, unless grating is fixed
	 * (first valid value of ngcs is 1, since it is a Line #) */
	if (fix_mode) ngcs++;
	else {		/* compute GC in RIM, increment count when it changes*/
	  irim1 = isclk/91;
	  ngc1 = (2*(isclk%91)+rti/5)/ngps;
	  if (ngc1!=ngc || irim1!=irim) {
	    ngcs++;
	    ngc = ngc1;
	    irim = irim1;
	  }
	}
	if (ngcs>nl) zmabend(" *** Tube line dimension error ***");

	/* # chops since start of range: */
	nchops = 42 * (isclk - 91*irim0 - imf0) + 4*(rti-rti0);
	if (rti != rti0) nchops += rti0==0 ? 1 : -1;
	/* force the first GC to start of cycle: */
	/* if (ngcs==1) nchops = 0;	??? why was I doing this?! */

	if (caltyp) output_tube_f( f2data, gp, ngcs, vflag);
	else output_tube_h( s2data, gp, ngcs, vflag);
/*	if (mf[rec].igc>=0 && (fix_wait || mirror!=WAIT)) {  */
	if ((fix_wait || mirror!=WAIT)) {
	  if (footprint)
	    for (j=smpos; j<=empos; j++) mpos1[j] = mpos[1][j];
	  output_cotube( mpos, mpos1, gp, ngcs, nchops, &offimag, &offplan, 
	   &dcnt, &vcnt, vflag);
	}
	continue;
      }		/* end of TUBE case */

	/* G-cube case */
      for (i=smpos; i<=empos; i++) {
	if (mpos[0][i].x == (float)BADP) {	/* off-planet or deboomed */
	  if (mpos[0][i].y == (float)BADP) offplan++;
	  else if (mpos[0][i].y == 0.) dcnt++;
	  else oops++;		/* shouldn't happen */
	  continue;
	}
	if (repl_sat) bb_repl( f1data[i], gp);	/* SATURATD=BB_REPL option */
		/* check that there are some valid data, 1 comb at a time:
		 * (also perform the photometric correction) */
	for (det=s_det, j=0; det<=e_det; det++) {
	  if (skipdet[det]>0 || wet[gp][det]<0) continue; 
	  if (caltyp && radscal>=0 && !histbin) {
	    if ((f1data[i][det]==(float)INS_HI_SAT4 && flag_sat) ||
	        (f1data[i][det]==(float)INS_LO_SAT4 && threshing) ||
	         f1data[i][det]>=VALID_MIN4) j = 1;	/* valid data flag */
	    if (f1data[i][det] >= VALID_MIN4 && wet[gp][det] <= npfcut)
	     f1data[i][det] = f1data[i][det]/cobuf[i].photcor;
	  }
	  else {
	    if ((f1data[i][det]==(float)INS_HI_SAT2 && flag_sat) ||
	        (f1data[i][det]==(float)INS_LO_SAT2 && threshing) ||
	         f1data[i][det]>=VALID_MIN2) j = 1;	/* valid data flag */
	  }
	}
	if (j==0) {
	  vcnt++;
	  continue;
	}

        if (footprint) {
	  for (j=0; j<4; j++) mpos1[j] = mpos[j][i];
	  if (ndistor) {	/* check for excessive distortion */
	    idistor = 0;
	    for (j1=0; j1<3; j1++) {
	      for (j2=j1+1; j2<4; j2++) {
		delx = mpos1[j1].x - mpos1[j2].x;
		delx = (delx>0) ? delx : -delx;
		dely = mpos1[j1].y - mpos1[j2].y;
		dely = (dely>0) ? dely : -dely;
		if (delx>maxdistor || dely>maxdistor) idistor = 1;
	      }
	    }
	    if (idistor) {
	      discnt++;
	      continue;
	    }
	  }
	  /* also check for "twisted rectangle" case -- use criterion by LJR:
	   * diagonals of rectangle must intersect inside the rectangle. 
	   * Therefore, if the 4 corners are A/B/C/D (A=0, B=1, C=3, D=2),
	   * then intersection is given by
	   *        p*A+(1-p)*C = q*B+(1-q)*D
	   * (A...D are vectors) and this is inside if 0<p<1 and 0<q<1. */
	  mat[0][0] = mpos1[0].x-mpos1[3].x;
	  mat[1][0] = mpos1[2].x-mpos1[1].x;
	  mat[0][1] = mpos1[0].y-mpos1[3].y;
	  mat[1][1] = mpos1[2].y-mpos1[1].y;
	  zminv( mat, 2, &detnt, w1, w2);	/* invert mat */
	  if (fabs(detnt)>1.e-30) {
	    p = mat[0][0]*(mpos1[2].x-mpos1[3].x) +
	        mat[1][0]*(mpos1[2].y-mpos1[3].y); 
	    q = mat[0][1]*(mpos1[2].x-mpos1[3].x) +
	        mat[1][1]*(mpos1[2].y-mpos1[3].y);
	  }
	  if (fabs(detnt)<1.e-30 || p<0.0 || p>1.0 || q<0.0 || q>1.0) {
	    irim1 = isclk/91;
	    imf = isclk%91;
	    if (first_twtd) {
	      sprintf(msg, " pixel unprojectably twisted, SCLK=%d.%d",
	       irim1, imf);
	      zvmessage( msg,"");
	      first_twtd = 0;
	    }
	    discnt++;
	    continue;
	  }

	  output_ftpt( &f1data[i][0], i, mpos1, gp, &offimag, isclk);
	}
        else output_near( &f1data[i][0], i, mpos[0][i], gp, 1.0,
	 &offimag);
      }		/* end of G-cube case */

    }	/* END OF RTI LOOP */

  }	/* END OF MF LOOP */

  if (idebug) {
    zvclose( dunit, NULL);
    zvclose( gunit, NULL);
  }			/* then exit in debugger */

	/* tell user how many pix. failed: */
  if (scnt) {
    sprintf( msg, " %d combs skipped or dummy in EDR ", scnt);
    zvmessage( msg,"");
  }
  if (dcnt) {
    sprintf( msg, " %d combs rejected by boom test ", dcnt);
    zvmessage( msg,"");
  }
  if (vcnt) {
    if (fix_mode) sprintf( msg, " %d combs had invalid data", vcnt);
    else sprintf( msg, " %d combs had invalid data or were WAIT state",
     vcnt);
    zvmessage( msg,"");
  }
  if (ccnt) {
    if (chk_comp) {
      sprintf( msg, " %d combs rejected for bad compression status", 20*ccnt);
      zvmessage( msg,"");
    }
    else {
      j1 = min(ccnt,1000);
      if (fix_mode) {
	sprintf( msg, " %d combs reported bad compression status at lines:",
	  20*ccnt);
	zvmessage( msg,"");
	for (j=0; j<j1; j++) {
	  sprintf( msg, "   %d", comperr[j]);
	  zvmessage( msg,"");
	}
      }
      else {
	sprintf( msg, " %d combs reported bad compression status at line,GP:",
	  20*ccnt);
	zvmessage( msg,"");
	for (j=0; j<j1; j++) {
	  sprintf( msg, "   %d, %d", comperr[j], comperrgp[j]);
	  zvmessage( msg,"");
	}
      }
    }
  }
  if (discnt) {
    sprintf( msg, " %d combs rejected for excessive distortion", discnt);
    zvmessage( msg,"");
  }
  if (skpcnt) {
    sprintf( msg, " %d combs rejected by SKIP_GP parameter", skpcnt);
    zvmessage( msg,"");
  }
  if (offplan) {
    sprintf( msg, " %d combs were off the planet", offplan);
    zvmessage( msg,"");
  }
  if (offimag) {
    sprintf( msg, " %d combs fell outside the image", offimag);
    zvmessage( msg,"");
  }
  if (oops) {		/* (this is always supposed to be 0) */
    sprintf( msg, " %d combs had bad flag ...", oops);
    zvmessage( msg,"");
  }
  if (drkcnt) {
    sprintf( msg, " %d pixels rejected by dark threshold test ", drkcnt);
    zvmessage( msg,"");
  }
  if (xpik) {
    sprintf( msg, " %d valid pixels replaced by spike file", xpik);
    zvmessage( msg,"");
  }
  if (xinvpik) {
    sprintf( msg, " %d invalid pixels were replaced by spike file", xinvpik);
    zvmessage( msg,"");
  }
  if (xzpik) {
    sprintf( msg, " %d pixels were zeroed out by spike file", xzpik);
    zvmessage( msg,"");
  }
  if (xbadpk) {
    mnerr /= xbadpk;
    mnerr2 /= xbadpk;
    mnerr2 = sqrt(mnerr2-mnerr*mnerr);
    sprintf( msg, " %d spikes did not match Old DN, mn.err = %f, std.dev = %f",
     xbadpk, mnerr, mnerr2);
    zvmessage( msg,"");
    sprintf( msg, " %d of these spikes were interpreted as dark deviations and used", xokpk);
    zvmessage( msg,"");
  }
  if (repl_sat) {
    if (!nsatrep) {
      zvmessage(" no saturated radiances were replaced!", "");
      return;
    }
    sprintf( msg," %d saturated radiances were replaced", nsatrep);
    zvmessage( msg,"");
    if (ndefalb) {
      sprintf( msg," %d of these used default albedoes", ndefalb);
      zvmessage( msg,"");
    }
    if (ncompalb) {
      repl_alb /= ncompalb;
      sprintf( msg," mean computed albedo was: %f", repl_alb);
      zvmessage( msg,"");
    }
  }
}


/*************************************************************************/
int FUNCTION extract_pntng( sclk, cang)
/*
 * extract instrument pointing (C-matrix Euler angles & Cone/Clock from
 * AACS file or from NAIF SPICE C-kernels (Pfm & Rotor), at beginning of
 * specified SCLK (mf) 
 *
 * AACS FILE OR CKS MUST BE OPENED BEFORE CALLING THIS ROUTINE
 *
 * currently only the AACS file option is supported if the full pointing
 * is required (incl. cone/clock) -- the CK code only gives Pfm C-angles
 *
 * return the angles in radians 
 *
 * NOTE:  GLL definition of twist differs by 90 degrees from SPICE:
 *       twist(GLL) = twist(SPICE) + 90
 * this is corrected here when necessary
 *
 * (also note that GLL telemetry is in EME1950) 
 */
int sclk;
struct pntng_typ *cang;
{
  int i, is, isclk, j, k;
  double con, timout;
  matrix ctemp;
  static double pra, pdec, ptwt, pcon, pclk;
  char buf[101];

  if (calib) {
    cang->ra = 0.0;
    cang->dec = zhalfpi();
    cang->twist = 0.0;
    return 1;
  }

  con = ztwopi() / 65536.;

  if (aacsf) {	/* search the AACS file */

    i = sclk/91;		/* RIM */
    j = sclk%91;		/* mod91 */
    isclk = 100*i+j;
    is = is0;			/* set to last record read */

    /* we assume that data in AACS file are always in SCLK order */

    if (is>isclk) return 0;	/* we are in a pointing gap */

    if (!is) {			/* first call */
      strcpy( buf, " ");
      i = 0;
      /* headers terminated by "Clock" */
      while (strcmp(buf,"Clock") && strcmp(buf,"CLOCK")) {
	vstat = fscanf( aacsfil," %s\n", buf);
	i++;
	if (i>1000) zmabend(" *** AACS file format error ***");
      }
    }
    while (is<isclk) {
      vstat = fscanf( aacsfil," %d %lf %lf %lf %lf %lf", &is, &pra, &pdec,
       &ptwt, &pcon, &pclk);
      if (vstat == EOF || vstat == 0) return 0;
      else if (vstat == 6) is0 = is;
      else zmabend(" *** error reading AACS file ***");
    }
    if (is==isclk) {
      cang->ra = pra/degrad;
      cang->dec = pdec/degrad;
      cang->twist = ptwt/degrad;
      cang->cone = pcon;
      cang->clock = pclk;
    }
    else return 0;
    if (cang->ra==0.0 && cang->dec==0.0 && cang->twist==0.0) return 0;
  }
  else {
    zreset1();				/* clear previous SPICE errors */
	/* encode SCLK into a string for NAIF: */
    i = sclk/91;		/* RIM */
    j = sclk%91;		/* mod91 */
    sprintf( msg, "%d", i);
    k = strlen( msg);
    sprintf( &msg[k], ".%d", j);
	/* get double-precision-encoded SCLK: */
    zscencd( ids.sc, msg, &sclkdp);
    if (zfailed()) zvmessage(" *** error in SCENCD ***","");
    zckgp( ck_id, sclkdp, s_tol, esystem, ctemp, &timout, &k);
    if (zfailed() || !k) return 0;
    zm2eul( ctemp, 3, 2, 3, &cang->twist, &cang->dec, &cang->ra);
    cang->dec = zhalfpi() - cang->dec;
  }
	/* it appears that the Primary CKs (hence also the NIMS CKs and
	 * the AACSFILE) use the GLL definition of twist, so correct this
	 * here too: */
  cang->twist -= zhalfpi();
  if (!strcmp( esystem, "J2000")) zrotadkx( cang, B1950, J2000, cang);
  return 1;
}


/*************************************************************************/
int FUNCTION fill( array, msize, ndata, fplane)
/*
 * fill the pixels with no data -- but only if more than 'ndata' points are
 * present within an (msize**2) neighborhood of the missing data point
 *
 * also do not fill if point is off-planet (inferred from latitude in
 * the cocube)
 *
 * 'fplane' is the plane offset in which to store a fill-flag DN
 * (only if fplane != NULL)
 */
float *array, *fplane;
int msize, ndata;
{
  int i, j;
  int mi, mj;
  int radius;
  int count;
  double dx, dy, dist;
  double sum_v;                 /* sum of weighted value */
  double sum_n;                 /* sum of inverse distance */
  float *ptr;

  if ((msize % 2) != 1) {
      zvmessage(" Fill matrix size is not a odd number -- fill aborted","");
      return -1;
    }

/*
 * duplicate the array
 */
  ptr = (float *)malloc(nl*ns*sizeof(float));
  for (i = 0; i < (nl*ns); i++) ptr[i] = array[i];

  radius = (msize - 1) / 2;		/* (not really a radius) */
  for (i = 0; i < nl; i++) {
    for (j = 0; j < ns; j++) {
      if (ptr[i*ns+j] == (float)NULL4 && gdata[i*ns+j] != (float)NULL4) {
				/* gdata is latitude: on-limb flag */
	count = 0;
	for (mi = (i - radius); mi <= (i + radius); mi++) {
	  for (mj = (j - radius); mj <= (j + radius); mj++) {
	    if (mi < 0 || mi >= nl) continue;	/* out of bound */
	    if (mj < 0 || mj >= ns) continue;	/* out of bound */
	    if (ptr[mi*ns+mj] >= VALID_MIN4) count++;
	  }
	}
	if (count < ndata) continue;	/* not qualified, do nothing */

	sum_v = 0;
	sum_n = 0;
	for (mi = (i-radius); mi <= (i+radius); mi++) {
	  for (mj = (j - radius); mj <= (j + radius); mj++) {
	    if (mi < 0 || mi>=nl) continue;		/* out of bound */
	    if (mj < 0 || mj >= ns) continue;	/* out of bound */
	    if (ptr[mi*ns+mj] >= VALID_MIN4) {
	      dy = (double)(mi - i);
	      dx = (double)(mj - j);
	      dist = (double)(dx * dx + dy * dy);
	      if (dist != 0) {
		sum_v += ptr[mi*ns+mj] / dist;
		sum_n += 1 / dist;
	      }
	    }
	  }
	}
	array[i*ns+j] = (sum_v / sum_n);
	/*if ((int)fplane > 0) fplane[i*ns+j] = FILL_FLAG;*/
	if (fplane != NULL)
	  fplane[i*ns+j] = NULL4;
      }
    }
  }
  free(ptr);
}


/************************************************************************/
FUNCTION find_angles( pang)
/*
 * retrieve the photometric angles at the center of the image (or as
 * close as we can get) from the cocube buffer, to store in label
 *
 * (Note that the angles found for a tube will be different than for
 * the corresponding G-cube, as the spatial centers are not the same
 * point -- although they should be close.)
 */

float *pang;
{
  int boffs, del, delmax, i, i0, i1, j, j0, j1, nlc, nsc;
  float *dptr;

	/* start at center of image: */
  nlc = (nl+1)/2;
  nsc = (ns+1)/2;

  boffs = 2;			/* band offset in cocube = 2 */
  if (tube) boffs = nbpg*ngps0;

  i = nsc-1;  j = nlc-1;
  dptr = gdata + (boffs*nl+j) * ns + i;		/* point to incidence angle */
  pang[0] = *dptr; 
  dptr += nl*ns;				/* point to emission angle */
  pang[1] = *dptr; 
  dptr += nl*ns;				/* point to phase angle */
  pang[2] = *dptr; 
  del = 0;
  if (pang[0] >= VALID_MIN4) goto pfound;

	/* at each step N, start at (L'-1,S'-1), where (L',S') is the
	 * the previous starting point, and move clockwise along the
	 * circumference of a square.  If we can increment the step in 
	 * both L & S, then this will result in a path of 8*N points, 
	 * none duplicating a preceding point.  If one of L,S must stop 
	 * incrementing because it ran into an edge, then some of the 
	 * points are duplicated, but we can live with this. */

  delmax = max( nl/2, ns/2);
  for ( del=1; del<=delmax; del++) {
    i0 = max( nsc-del, 0);
    i1 = min( nsc+del, ns-1);
    j0 = max( nlc-del, 0);
    j1 = min( nlc+del, nl-1);

    for ( i=i0, j=j0; i<i1; i++) {	/* top of square */
      dptr = gdata + (boffs*nl+j) * ns + i;	/* point to incidence angle */
      pang[0] = *dptr; 
      dptr += nl*ns;				/* point to emission angle */
      pang[1] = *dptr; 
      dptr += nl*ns;				/* point to phase angle */
      pang[2] = *dptr; 
      if (pang[0] >= VALID_MIN4) goto pfound;
    }
    for (; j<j1; j++) {		/* RHS of square */
      dptr = gdata + (boffs*nl+j) * ns + i;	/* point to incidence angle */
      pang[0] = *dptr; 
      dptr += nl*ns;				/* point to emission angle */
      pang[1] = *dptr; 
      dptr += nl*ns;				/* point to phase angle */
      pang[2] = *dptr; 
      if (pang[0] >= VALID_MIN4) goto pfound;
    }
    for (; i>=i0; i--) {		/* bottom of square */
      dptr = gdata + (boffs*nl+j) * ns + i;	/* point to incidence angle */
      pang[0] = *dptr; 
      dptr += nl*ns;				/* point to emission angle */
      pang[1] = *dptr; 
      dptr += nl*ns;				/* point to phase angle */
      pang[2] = *dptr; 
      if (pang[0] >= VALID_MIN4) goto pfound;
    }
    for (; j>=j0; j--) {		/* LHS of square */
      dptr = gdata + (boffs*nl+j) * ns + i;	/* point to incidence angle */
      pang[0] = *dptr; 
      dptr += nl*ns;				/* point to emission angle */
      pang[1] = *dptr; 
      dptr += nl*ns;				/* point to phase angle */
      pang[2] = *dptr; 
      if (pang[0] >= VALID_MIN4) goto pfound;
    }
  }
  zvmessage(" unable to find photometric angles!","");
  for (i=0; i<3; i++) pang[i] = -999.;
  return;

pfound:			/* stop when valid point found */
  if (del > 2) {		/* hole in the center! */
    sprintf( msg, " %d steps needed to find photometric angles", del);
    zvmessage( msg,"");
  }
}


/************************************************************************/
FUNCTION find_body_motion( pra, pdec, pnp0, pbm_mra, pbm_mdec)
/*
 * find the mean motion per mf of target body relative to s/c over the current
 * EDR range and return the RA/Dec components in rad/mf;
 * pbm_mra/dec are the previous means over pnp0 points -- will be updated with
 * current mean (weight dt)
 *
 * this routine must be called after open_edr, which sets file_lclk/hclk
 *
 * if an SCLK range has been set by the user, then only the part of the
 * EDR range falling inside this interval is used
 */
double *pra, *pdec, *pbm_mra, *pbm_mdec;
int *pnp0;
{
  int i, ii, ind, indx;
  unsigned int sclk, sclkmax=0, sclkstrt=0, losclk, hisclk;
  double ddec, dra, dt, dxy, dec1, dec2, ra1, ra2, slantmax, slantstrt,
   state1[6], state2[6], state_lt, ssb_craft[6], xrat1, xrat2;
  double vmag, maxmag=-1.;
  float lat[2],lon[2],dum,dum1;
  static matrix me, cm;
  vector sc, vab, vabmax, vv1, vv2;
  struct pntng_typ angs[2], me_ang;

  zreset1();				/* clear previous SPICE errors */

  losclk = file_lclk;
  hisclk = file_hclk;
  if (range_lclk>hisclk || range_hclk<losclk) return;
  if (range_lclk>losclk) losclk = range_lclk;
  if (range_hclk<hisclk) hisclk = range_hclk;

  /* (this code is adapted from fcn. nims_ps_orient) */

	/* convert start SCLK of file to ephem. time */
  nims_s2et( losclk, &etime);
	/* spacecraft position relative to SSB: */
  zspkssb( ids.sc, etime, esystem, ssb_craft);
	/* target body position relative to spacecraft: */
  zspkapp( ids.target, etime, esystem, ssb_craft, "LT+S", state1, &state_lt);
  if (zfailed()) zmabend(" *** unable to get mean body motion ***");
	/* ratio of target radius to range */
  for (i=0, xrat1=0.0; i<3; i++) 
    xrat1 += state1[i]*state1[i];
  xrat1 = tgt_data.e1rad / sqrt(xrat1);

	/* convert end SCLK of file to ephem. time */
  nims_s2et( hisclk, &etime);
	/* spacecraft position relative to SSB: */
  zspkssb( ids.sc, etime, esystem, ssb_craft);
	/* target body position relative to spacecraft: */
  zspkapp( ids.target, etime, esystem, ssb_craft, "LT+S", state2, &state_lt);
  if (zfailed()) zmabend(" *** unable to get mean body motion ***");
	/* ratio of target radius to range */
  for (i=0, xrat2=0.0; i<3; i++) 
    xrat2 += state2[i]*state2[i];
  xrat2 = tgt_data.e1rad / sqrt(xrat2);

  *pra = *pdec = 0.0;

  dt = hisclk-losclk;		/* elapsed time in mfs */
  if (dt <= 0.0) return;

  /* if ratio of range to target radius is large (>10), then we assume 
   * body-motion is just mean motion of body center;  otherwise, compute 
   * motion relative to body surface (since the vectors to slant and center 
   * are significantly different) */

  if (xrat1>0.1 || xrat2>1.0) {

    /* slew rate relative to body surface is approximated as:
     *  AB'/(S*(t2-t1))
     * where A and B are the 2 boresight intercept points at t1 & t2,
     * AB is the distance between them, AB' is the component of AB 
     * perpendicular to the boresight, and S is the slant distance
     *
     * rather than compute the mean of every pair of successive MFs
     * (which would theoretically have to filter out points exceeding
     * the slew rate limit), we cycle thru all points that intersect
     * the surface looking for the point that is furthest (in arc along
     * surface) from the first one;  ideally, should look for the
     * biggest arc between any 2 points, but in practice tieing down
     * the first one should work fine */

    indx = 0;
    for (sclk=losclk; sclk<=hisclk; sclk++) {

      /* get the C-angles at this SCLK */
      vstat = get_pntng( sclk, &angs[indx]);
      if (!vstat) continue;

      /* these steps are from get_tgt_data, which cannot be called 
       * because the mf[] buffer has not been filled: */
      nims_ps_orient( sclk, sc, vv1, vv2, &me_ang);
      eul2mat( me_ang.ra, me_ang.dec, me_ang.twist, me); 
      eul2mat( angs[indx].ra, angs[indx].dec, angs[indx].twist, cm); 
      zmxmt( cm, me, tgt_data.om);
      for (ii=0; ii<3; ii++) vv1[ii] = -sc[ii];
      zmxv( me, vv1, tgt_data.rs);
      /* call PPROJ for boresight with this tgt_data buffer: */
      dum = 0.0;
      zpproj( &tgt_data, &dum, &dum, &lat[indx], &lon[indx], LS_LL, lattyp,
       &dum1, &slant, &ind);
      vv2[0] = dum1;
      if (ind) {
	/* after the first on-target point, reset indx to search for
	 * 2nd point */
	if (!indx) {
	  sclkstrt = sclk;
	  slantstrt = slant;
	  indx = 1;
	  continue;
	}
	/* this actually returns vector AB, not arc ... */
        varc( lat, lon, vab);
	/* look for the largest vector so far */
	vmag = 0.;
	for (ii=0; ii<3; ii++) vmag += vab[ii]*vab[ii];
	vmag = sqrt(vmag);
	if (vmag>maxmag) {
	  maxmag = vmag;
	  sclkmax = sclk;
	  slantmax = slant;
	  for (ii=0; ii<3; ii++) vabmax[ii] = vab[ii];
	}
      }
    }
    /* check if we failed to find 2 distinct on-planet points, in
     * which case revert to old algorithm */
    if (sclkmax<=0 || sclkstrt<=0 || sclkmax==sclkstrt) goto far_case;

    /* to simplify things, we evaluate the SC-vector at the beginning,
     * rather than at the middle, as would be more accurate */
    nims_ps_orient( sclkstrt, sc, vv1, vv2, &me_ang);
    for (ii=0; ii<3; ii++) vv1[ii] = -sc[ii];
    eul2mat( me_ang.ra, me_ang.dec, me_ang.twist, me); 
    eul2mat( angs[0].ra, angs[0].dec, angs[0].twist, cm); 
    zmxmt( cm, me, tgt_data.om);

    /* to get the components we want, first transform vector to
     * Camera system and set L-component to zero, then transform
     * to EME system: */
    zmxv( tgt_data.om, vab, vab);	/* Body -> Camera system */
    /* use average slant distance here -- not consistent with other
     * assumptions, but it should always improve the approximation,
     * and is a simple fix */
    slant = 0.5*(slantstrt+slantmax);
    vab[0] /= slant;			/* convert to angles */
    vab[1] /= slant;
    vab[2] = 0;				/* component along boresight */
    zmtxv( cm, vab, vab);		/* Camera -> EME system */
    /* return RA/Dec components: */
    vrdcomp( angs[0].ra, angs[0].dec, vab, &dra, &ddec);
    /* compute body-relative rates from this new slew rate: */
    if (cos(angs[0].dec) == 0.0)
      *pra = 0.;
    else {
      *pra = angs[1].ra - angs[0].ra;
      /* check for wraparound */
      if (*pra>zpi()) *pra -= ztwopi();
      if (-(*pra)>zpi()) *pra += ztwopi();
      *pra -= dra/cos(angs[0].dec);
    }
    *pdec = (angs[1].dec - angs[0].dec - ddec) / dt;
    *pra /= dt;

    if (aacsf) {
      /* we must reset aacsfile to start of file for assemble_pntng */
      rewind( aacsfil);
      is0 = 0;
    }
  }
  else{

far_case:
    ra1 = atan2( state1[1], state1[0]);
    if (ra1<0) ra1 += ztwopi();
    dxy = sqrt( state1[0]*state1[0] + state1[1]*state1[1] );
    /*dec1 = atan2( state1[2], dxy);
    wrong!  should be: <19jan04> */
    dec1 = atan2( dxy, state1[2]);
    ra2 = atan2( state2[1], state2[0]);
    if (ra2<0) ra2 += ztwopi();
    dxy = sqrt( state2[0]*state2[0] + state2[1]*state2[1] );
    dec2 = atan2( dxy, state2[2]);

    /* ra/dec motions in rad/mf: */
    *pra = ra2-ra1;
    if (*pra>zpi()) *pra -= ztwopi();	/* check for wraparound at 2*PI */
    if (-(*pra)>zpi()) *pra += ztwopi();
    *pra /= dt;
    *pdec = (dec2-dec1)/dt;
  }

  *pbm_mra = ( (*pnp0)*(*pbm_mra) + dt*(*pra) ) / (dt+(*pnp0));
  *pbm_mdec = ( (*pnp0)*(*pbm_mdec) + dt*(*pdec) ) / (dt+(*pnp0));
  *pnp0 += dt;

  ra1 = 1500.*(*pra);
  dec1 = 1500.*(*pdec);
  sprintf( msg,
   " Mean relative body motion in RA/Dec = %.3f, %.3f (mrad/sec)",
   ra1, dec1);
  zvmessage( msg,"");

  return;
}


/************************************************************************/
FUNCTION find_gap( hist, nbin, bgap, ngap)
/*
 * find largest gap in a histogram
 */
int *bgap, *hist, nbin, *ngap;
{
  int bgap0, i, igap, ngap0;

  *bgap = 0;			/* begin bin no. of largest gap */
  bgap0 = 0;			/* begin bin no. of current gap */
  *ngap = ngap0 = 0;		/* size (in bin no.) of largest/current gap */
  igap = 0;			/* indicates we're in a gap */
  for (i=0; i<nbin; i++) {
    if (hist[i]==0) {	/* empty bin: */
      if (!igap) {			/* if not in gap, start new one */
	igap = 1;
	bgap0 = i+1;
	ngap0 = 1;
      }
      else ngap0++;			/* else just increase gap size */
    }
    else if (igap) {	/* non-empty bin: only care if we're in gap */
      igap = 0;			/* end the gap */
      if (ngap0 > *ngap) {		/* if it's biggest so far, save stuff*/
	*ngap = ngap0;
	*bgap = bgap0;
      }
    }
  }
}


/************************************************************************/
FUNCTION find_mode()
/*
 * determine the instrument mode from the EDR header 
 *
 *  currently use the convention in the NSD, numbering the modes
 *  from 0 to 16 (10/11 have ? in the doc, and the last one is marked
 *  only "?" !)
 *
 *  modes 12-15 are labelled "special seqs, RAM modes" -- we don't try
 *  to determine them
 *
 * 3FEB93: MODE 12 ("SPECIAL SEQUENCE") ENABLED FOR ONE CASE ONLY 
 */
{
  int i, i1, iprev, j, j1, k, kbi, kby, n, nn, mbands, mgps, 
   nmode, nwps, wrep;
  char gmask[N_GPS];
  char sbuf[33], chr;

  if (u_imode>=0) {		/* user-specified mode */
    imode = u_imode;
    if (!u_gst) gstart = 0;		/* indeterminate in this case! */
    switch (imode) {
      case 0:  nsteps = nlgp; gdel = 0; mirrop = 0; break;
      case 1:  nsteps = 12; gdel = 2; mirrop = 1; break;
      case 2:  nsteps = 12; gdel = 2; mirrop = 0; break;
      case 3:  nsteps = 24; gdel = 1; mirrop = 1; break;
      case 4:  nsteps = 24; gdel = 1; mirrop = 0; break;
      case 5:  nsteps = 6; gdel = 4; mirrop = 1; break;
      case 6:  nsteps = 6; gdel = 4; mirrop = 0; break;
      case 7:  nsteps = nlgp; gdel = 0; mirrop = 1; break;
      case 8:  nsteps = 1; gdel = 2; mirrop = 1; break;
      case 9:  nsteps = 1; gdel = 2; mirrop = 0; break;
      case 10:  nsteps = 24; gdel = 1; mirrop = 1; break;
      case 11:  nsteps = 24; gdel = 1; mirrop = 0; break;
      case 12:  nsteps = 6; gdel = 2; mirrop = 1; break;
      case 13:  nsteps = 24; gdel = 0; mirrop = 1; break;
      case 14:  nsteps = 6; gdel = 0; mirrop = 1; break;
      case 15:  nsteps = 12; gdel = 0; mirrop = 1; break;
      default: zmabend(" *** invalid instrument mode ***");
    }
    goto mfound;
  }
/*
 * note that modes 10/11 have TWO PTABS, the parameters given above
 * only specify the first -- this must be handled by special code (TBD)
 */

/*
 * phase-2 NIMS EDRs have the mode info in the 2nd header, so all we do
 * here is collect it and check for internal consistency:
 * (assume that the header has been read with get_nims_edr_hdr_2)
 */

  /* (avoid use of sscanf because it gets confused by spaces) */

  /* nominal MODE */
  strncpy( sbuf, edrhdr.hdr2.mode, 2);
  sbuf[2] = '\n';
  nmode = atoi(sbuf);

  /* other stuff, for label .... */
  strncpy( sbuf, &edrhdr.hdr2.chop, 1);
  sbuf[1] = '\n';
  chopper = atoi(sbuf);
  strncpy( sbuf, &edrhdr.hdr2.grat_off, 1);
  sbuf[1] = '\n';
  g_off = atoi(sbuf);

  /* PTAB A */
  strncpy( sbuf, &edrhdr.hdr2.ptab_a[2], 2);
  sbuf[2] = '\n';
  mirrop = atoi(sbuf);
  if (!u_gst) {
    strncpy( sbuf, &edrhdr.hdr2.ptab_a[6], 2);
    gstart = atoi(sbuf);
  }
  strncpy( sbuf, &edrhdr.hdr2.ptab_a[8], 2);
  gdel = atoi(sbuf);
  strncpy( sbuf, &edrhdr.hdr2.ptab_a[10], 2);
  nsteps = atoi(sbuf);

  /* need for PTAB B is TBD */

  /* now determine mode from PTAB */
  if (nsteps==0) return;	/* can't determine mode in this EDR */
  switch (gdel) {		/* grating delta determines: */
    case 0:  imode = 0; break;			/* FIXED/BANDEDGE/SAFE */
    case 1:  imode = 3; break;  		/* LONG */
    case 2:  imode = 1; break;  		/* FULL/SPECIAL-SEQ */
    case 4:  imode = 5; break;  		/* SHORT */
    default: imode = -1;
  }
  /* STOP-SLIDE modes (10/11) are also ambiguous with the above, but need
   * special code to handle them */
  if (imode==0) {		/* resolve ambiguity using mirrop & nsteps */
    if (!mirrop) imode = 0;			/* SAFE */
    else if (nsteps == 1) imode = 8;		/* BANDEDGE */
    else imode = 7;				/* FIXED */
  }
  if (imode==1) {		/* resolve ambiguity using nsteps */
    switch (nsteps) {
      case 6:  imode = 12; break;		/* SPECIAL-SEQ */
      case 12:  imode = 1; break;		/* FULL */
      default: imode = -1;			/* ??? */
    }
  }
  if (!mirrop) {		/* mirror fixed = SPECT mode */
    if (imode==7) imode = 0;		/* Fixed Spect = SAFE !! */
    else if (imode>0) imode++;
  }
  if (imode != nmode) {
    sprintf( msg, " ** inconsistent mode information in EDR %d", cur_edr+1);
    zvmessage( msg, "");
    nsteps = 0;
    return;
  }

mfound:

  /* GAIN */
  strncpy( sbuf, &edrhdr.hdr2.gain, 1);
  sbuf[1] = '\n';
  gain[0] = atoi(sbuf);
  sprintf( msg, " Gain state = %d", gain[0]);
  zvmessage( msg,"");

	/* set mirror position count: */
  nmpos = empos0-smpos0+1;
/*  if (!mirrop) nmpos = 1;	disabled because we still have 40 combs
				per MF! */

	/* find the number of "mirror wait" cycles (nits): this
	 * depends on number of logical steps (even for fixed modes !!)
	 */
  switch (nsteps) {
    case 0:  nwps = 0; break;
    case 1:  nwps = 1; break;	/* (??) -- BANDEDGE */
    case 6:  nwps = 1; break;
    case 12:  nwps = 1; break;
    case 24:  nwps = 2; break;
    default:  nwps = 1;		/* (just in case) */
  }

	/* store for label & inform user: */
  switch (imode) {
    case 0: sprintf( ins_mode, "SAFE"); break;
    case 1: sprintf( ins_mode, "FULL MAP"); break;
    case 2: sprintf( ins_mode, "FULL SPECT"); break;
    case 3: sprintf( ins_mode, "LONG MAP"); break;
    case 4: sprintf( ins_mode, "LONG SPECT"); break;
    case 5: sprintf( ins_mode, "SHORT MAP"); break;
    case 6: sprintf( ins_mode, "SHORT SPECT"); break;
    case 7: sprintf( ins_mode, "FIXED MAP"); break;
    case 8: sprintf( ins_mode, "BANDEDGE MAP"); break;
    case 9: sprintf( ins_mode, "BANDEDGE SPECT"); break;
    case 10: sprintf( ins_mode, "SLIDE & STOP MAP"); break;
    case 11: sprintf( ins_mode, "SLIDE & STOP SPECT"); break;
    case 12: sprintf( ins_mode, "SPECIAL SEQUENCE"); break;
    case 13: sprintf( ins_mode, "FIXED LONG MAP"); break;
    case 14: sprintf( ins_mode, "FIXED SHORT MAP"); break;
    case 15: sprintf( ins_mode, "FIXED FULL MAP"); break;
  }
  sprintf( msg, " Instrument mode is ");
  strcat( msg, ins_mode);
  zvmessage(" ","");
  zvmessage( msg,"");

  ngps = nsteps + nwps;

  fix_mode = (nwps!=0 && gdel==0);

  /* store the *real* # GPs (determines NB of weights file / co-tube) */
  ngps0 = fix_mode ? 1 : nsteps;

  if (histbin && ngps0>1) zmabend(" Ins. mode incompatible with histogram binningg");

	/* convert SLEW_TOL from per-g.c. to per mf: */
  if (!fix_mode) slew_tol = slew_tol/ngps;

  /* now the WET! */

  strncpy( wet_gid, edrhdr.hdr2.wetgid, 10);	/* store ID for label */
  wet_gid[10] = '\n';

  strncpy( sbuf, edrhdr.hdr2.wetgrpsiz, 2);	/* # of entries in WET */
  sbuf[2] = '\n';
  n = atoi(sbuf);

  mbands = 0;				/* total # bands from WET */
  mgps = 0;				/* total # GPs from WET */
  for (i=0; i<n; i++) {
    gmask[mgps] = 0;
    strncpy( sbuf, &edrhdr.hdr2.wetgrp[7*i], 2);	/* repeat count */
    sbuf[2] = '\n';
    wrep = atoi(sbuf);
    strncpy( sbuf, &edrhdr.hdr2.wetgrp[7*i+2], 5);	/* det.mask */
    for (j=0; j<5; j++) {		/* convert hex char's to binary */
      if (sbuf[j]<='9')
	sbuf[j] -= '0';
      else
	sbuf[j] = sbuf[j]-'A'+10;
    }
    for (j=0; j<N_DETS; j++) {	/* locate detector bits in the mask*/
      j1 = N_DETS-j-1;	/* flip detector bit order */
      kby = 4-(j/4);	/* byte # */
      kbi = j%4;	/* bit in byte */
      chr = 1<<kbi;
      if (chr&sbuf[kby]) {
	wet[mgps][j1] = 1;
	gmask[mgps] = 1;	/* at least 1 det. at this gp */
	mbands += wrep;
      }
      else wet[mgps][j1] = 0;
    }
    mgps++;
    /* repeat entry for each GP: */
    for (i1=0; i1<wrep-1; i1++) {
      for (j=0; j<N_DETS; j++) wet[mgps][j] = wet[mgps-1][j];
      gmask[mgps] = gmask[mgps-1];
      mgps++;
    }
  }
  /* check that NGPS in table matches that of mode: */
  if (ngps != mgps) zmabend("*** error reading WET ***");

  /* set band dimension and the band mask -- also convert the WET to a
   * band map (gives band # for given det,gp, =-1 if excluded): */
  nb = nbb = 0;
  for (i=0; i<408; i++) bmask[i] = 0;
  deteci = -1;
  if (!fix_mode) {
    for (i=0; i<N_DETS; i++) {
      for (j=0; j<ngps0; j++) {
        if (i<s_det || i>e_det || (lami && nbb!=(lami-1)) || !wet[j][i] ||
	 skipdet[i]>0) {
	  wet[j][i] = -1;
        }
        else {
	  bmask[nbb] = 1;
	  wet[j][i] = nb;
	  nb++;			/* # of bands used */
          if (lami && nbb==(lami-1)) deteci = i;
        }
        nbb++;		/* max. # of bands for this mode */
      }
    }
  }
  else {		/* Fixed mode: imode = 7 or 13-15 */
    /* ensure that for each Det, there is only one band */
    nb = -1;
    iprev = -1;
    for (i=0; i<N_DETS; i++) {
      for (j=0; j<nsteps; j++) {
        if (i<s_det || i>e_det || (lami && nbb!=(lami-1)) || !wet[j][i] ||
	 skipdet[i]>0) {
	  wet[j][i] = -1;
        }
        else {
	  bmask[nbb] = 1;
          if (i!=iprev) {
	    nb++;
	    iprev = i;
	  }
	  wet[j][i] = nb;
          if (lami && nbb==(lami-1)) deteci = i;
        }
      }
      nbb++;
    }
    nb++;	/* since this was initialized to -1 */
  }

  /* check that any bands left -- if not, probably bad LAMI */
  if (!nb) zmabend(" zero bands selected ... check use of LAMI");

  /* if all the detectors at a given GP are deselected, then omit that 
   * GP altogether -- so we need a GPOS array for output_cotube & for
   * weights cube;  also ensure that no wait states are included 
   * (unless fixed mode) */
  mgps = 0;
  nn = ngps;
  if (!fix_mode) nn -= nwps;  
  for (j=0; j<ngps; j++) {
    if (j>=nn) {
      gmask[j] = 0;
      gpos[j] = -1;  /* include wait states for assemble_pntng */
    }
    else {
      if (gmask[j]) 
	gpos[j] = mgps++;
      else 
	gpos[j] = -1;
    }
  }
  ngps0 = fix_mode ? 1 : mgps;		/* reset ngps0 */

  /* special check to see if we include Wait state in fixed mode: 
   * (for fix_mode, all 13 GPs have identical masks except the Wait one
   * can be all 0 ... so just check to see if any of the latter's bits are
   * set, if so it must be same as others) */
  fix_wait = 0;
  if (fix_mode) {
    for (j=0; j<N_DETS; j++)
      if (wet[12][j]) fix_wait = 1;
  }

  /* miscellaneous stuff ... */

  if (!nlamsd) lamsd = (nb+1)/2;	/* lamsd starts at 1, band # at 0 */
}


/************************************************************************/
FUNCTION find_tube_gcs()
/*
 * make a first pass thru the EDRs to see how many lines to allocate for
 * the tube (1 line per Grating Cycle);  this routine has the same main
 * loop as extract_data, except it always opens an EDR and does nothing else
 *
 */
{
  int empty, gp, i, igp, imp, irec, irim, irim1, isclk, istat, jrti,
   jsclk, ngc, ngc1, rec, rti;

  jsclk = 0;			/* initialize EDR record SCLK counter */
  ngcs = 0;			/* reset grating cycle count (for tube) */
  ngc = irim = -1;		/* more tube flags */
  file_hclk = 0;		/* to ensure an EDR gets opened at 1st rec */

  /* loop over all pointing records (MFs): */

  for (rec=0; rec<nmf; rec++) {
    isclk = mf[rec].sclk;
    if (!isclk) continue;
    if (isclk<file_lclk || isclk>file_hclk) {
      cur_edr = -1;
      while (cur_edr+1<nedrs) {
	open_edr(0);	/* new EDR, compute file_l/hclk, incr. cur_edr */
	if (isclk>=file_lclk && isclk<=file_hclk) break;
      }
      if (isclk<file_lclk || isclk>file_hclk) {	/* not in any EDR */
	file_hclk = 0;		/* make sure new EDR opened at next sclk */
	continue;
      }
      irec = 1;		/* set record counter to start of EDR */
    }
    /* read records from EDR until we find a valid data record at
     * this pointing record -- if none, then flag it and skip to next
     * pointing record: */
    while (jsclk<isclk) {
      get_nims_edr_rec_2( edrs[cur_edr].unit, irec++, &edrrec);
      jsclk = comp_sclk(edrrec.pfix.sclk);
    }
    /* if EDR skipped over this MF: */
    if (jsclk>isclk) continue;			/* to next pointing record */

    for (rti=0; rti<=5; rti+=5) {  /* 2 scans (EDR records) per mf */

      /* examine the 2 EDR records for this MF separately */
      jrti = edrrec.pfix.sclk.rti;	/* this is the RTI read in above */
      if (rti<jrti) continue;	/* rti must be 0 */
      if (rti>jrti) {		/* rti must be 5 */
	/* check if EDR ends in the middle of a MF: */
	if (isclk==file_hclk && !file_hrti) break;
	get_nims_edr_rec_2( edrs[cur_edr].unit, irec++, &edrrec);
	jsclk = comp_sclk(edrrec.pfix.sclk);
	if (!jsclk) continue;
	jrti = edrrec.pfix.sclk.rti;
	if (jsclk!=isclk || jrti!=rti) continue;
      }

      /* check compression status flag */
      if (chk_comp && edrrec.pfix.compression_stat != 0) continue;

      /* check if any valid data in record */
      empty = 1;
      for (i=0; i<340; i++)
        if (edrrec.sensor[i] != 0) empty = 0;
      if (empty) continue;

      gp = (2*(isclk%91) + rti/5) % ngps;
      if (gpos[gp]<0) continue;
      igp = fix_mode ? 0 : gp;

      if (fix_mode) ngcs++;
      else {		/* compute GC in RIM, increment count when it changes*/
	irim1 = isclk/91;
	ngc1 = (2*(isclk%91)+rti/5)/ngps;
	if (ngc1!=ngc || irim1!=irim) {
	  ngcs++;
	  ngc = ngc1;
	  irim = irim1;
	}
      }
    }
  }
  sprintf( msg, " # of GCs with valid data = %d", ngcs);
  zvmessage( msg,"");
}


/************************************************************************/
FUNCTION find_xy_center( sclk, rti, mpos, bcone, bxcone, dbc, dbx)
/*
 * Calculate the coordinates of the pixel center for all mirror positions for
 * a given scan (nit), specified by rti = 0 or 5.  
 *
 * If a boom map was supplied, then the boresight cone/xcone angles will be
 * computed and the rejected pixels marked by X = BADP and Y=0 (to distinguish
 * them from off-planet pixels, which have X = Y = BADP.
 */

int sclk;
double bcone, bxcone, dbc, dbx;
	/* mirror pos. -- only 1st element of 1st dimension is used: */
struct xytype mpos[4][20];
{
  int boom, dboom, i, ind, j, k, mp;
  float dum, lat0, lat, lon, ertang, prtang;
  double dlat[20], dlon[20], phi, cone, xcone, tcone, txcone;

  for (i=smpos; i<=empos; i++) {
    mpos[0][i].x = BADP;
    mpos[0][i].y = BADP;
    cobuf[i].latlon[0] = BADP;
    cobuf[i].latlon[1] = BADP;
    dlat[i] = 0.;
    dlon[i] = 0.;

    get_tgt_data( sclk, rti, i, 0, 1, &cone, &xcone);
    if (dbm) {
      j = i;
      if (rti) j += 20;            /* rti is 0 or 5 only */
      tcone = bcone + dbc*mirr_tim_table[j] + cone*degrad;
      txcone = bxcone + dbx*mirr_tim_table[j] + xcone*degrad;
      znimsboom( tcone, txcone, &dboom, dbmfile);
      if (dboom) {
        mpos[0][i].y = 0;  /* to distinguish pix. from off-planet */
        continue;
      }
    }

      /* get lat, lon of mp0 */
    dum = 0.0;
    zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang,
     &slant, &ind);
    lat0 = lat;		/* store since lat may get converted det->cen */

      /*
       * if point is off-planet then check if we want to try
       * the radius fudge option:  (only for map_type = 2 or 16 !)
       */
    if (!ind) {
      if (!radfudge) continue;
	/* nominal planetocentric radius at this lat/lon: */
      rplan( &rnom, lat, lon, tgt_data.e1rad, tgt_data.e2rad,
       tgt_data.prad);
      lfact = rtang/rnom;
      if (lfact> radfact) continue;
            /* only ORTHO & POV allow RADFUDGE */
      if (map_type == 2) {
	prtang = pol_rad * lfact;
	ertang = eq_rad * lfact;
	if (pgraphic==1) lat = det2cen( lat);
	ztranv( &ind, map_type, LL_LS, map_samp, map_line, latitude,
	 parallel[0], parallel[1], longitude, scale, map_pole,
	 &mpos[0][i].y, &mpos[0][i].x, &lat, &lon, prtang, ertang,
	 north);
        if (ind && (rtang < eq_rad)) continue;	/* BOP */
        if ((map_type==9 || map_type==10) && mpos[0][i].x<0.)
	  mpos[0][i].x += circum;
      }
      else {            /* must be maptype = 16 */
	t_data_povf.prad = tgt_data.prad * lfact;
	t_data_povf.e1rad = tgt_data.e1rad * lfact;
	t_data_povf.e2rad = tgt_data.e2rad * lfact;
	dum = -1.;	/* flag to ignore BOP test */
        zpproj( &t_data_povf, &mpos[0][i].y, &mpos[0][i].x, &lat, &lon,
         LL_LS, lattyp, &dum, &dum, &ind);
        if (!ind && (rtang < rnom)) continue;	/* BOP */
      }
      cobuf[i].rads[1] = rtang - rnom;		/* altitude */
    }

    else {                  /* point is on planet */
      if (map_type==16) {
	dum = 1.;	/* flag to use BOP test */
        zpproj( &t_data_pov, &mpos[0][i].y, &mpos[0][i].x, &lat, &lon,
         LL_LS, lattyp, &dum, &dum, &ind);
        if (!ind) continue;
        cobuf[i].rads[1] = 0.0;
      }
      else {
	if (pgraphic==1 && (map_type!=10 || oldver)) lat = det2cen( lat);
        ztranv( &ind, map_type, LL_LS, map_samp, map_line, latitude,
         parallel[0], parallel[1], longitude, scale, map_pole,
         &mpos[0][i].y, &mpos[0][i].x, &lat, &lon, pol_rad, eq_rad,
         north);
        if (ind) continue;
        if ((map_type==9 || map_type==10) && mpos[0][i].x<0.) 
	  mpos[0][i].x += circum;
        cobuf[i].rads[1] = 0.0; /* this is on-planet flag in output_cot */
      }
    }
    if (tube) {				/* save degrees for backplane */
      cobuf[i].latlon[0] = lat;
      cobuf[i].latlon[1] = lon;
    }
    dlat[i] = lat0/degrad;		/* save for geometry calc'n */
    dlon[i] = lon/degrad;

    cobuf[i].rads[0] = slant;            /* slant distance */

            /* save range for label: */
    maxsdist = max( slant, maxsdist);
    minsdist = min( slant, minsdist);
            /* ... and for catalog: */
    if (i==9 && !rti) {
      trange[ll_rec1] = slant;
      if (!ll_rec1) ll_rec1 = 1;
    }
  }
	/*
	 * finish computation of geometry buffer:
	 */
  geobuf( mpos, dlat, dlon);

}


/************************************************************************/
FUNCTION find_xy_corners( sclk, rti, mpos, bcone, bxcone, dbc, dbx)
/*
 * Calculate the coordinates of the 4 corners of the input pixel for all
 * mirror positions for a given scan (nit), specified by rti = 0 or 5.  
 *
 * In the case of a tube, 2 points will be computed:  the center and right
 * center edge of the pixel;  this is done to save in the number of 
 * backplanes for a tube, since it gives the Cone offset needed to compute
 * the 4 corner pixels, while the Cross-cone offset can be obtained from
 * the mirror steps.
 *
 * If a boom map was supplied, then the boresight cone/xcone angles will be
 * computed and the rejected pixels marked by X = -1 and Y=0 (to distinguish
 * them from off-planet pixels, which have X = Y = -1.
 */

int sclk;
double bcone, bxcone, dbc, dbx;
	/* mirror pos. of corners: */
struct xytype mpos[4][20];
{
  int boom, dboom, i, ind, j, jj, k, m, mp, npts;
  float del1, del2, dum, lat0, lat, lon[4], ertang, prtang;
  double delx, dlat[20], dlon[20], phi, cone, xcone, tcone, txcone;

  for (i=smpos; i<=empos; i++) {

    mpos[0][i].x = BADP;
    mpos[0][i].y = BADP;
    cobuf[i].latlon[0] = BADP;
    cobuf[i].latlon[1] = BADP;

    get_tgt_data( sclk, rti, i, 0, 1, &cone, &xcone);
    if (dbm) {
      j = i;
      if (rti) j += 20;            /* rti is 0 or 5 only */
      tcone = bcone + dbc*mirr_tim_table[j] + cone*degrad;
      txcone = bxcone + dbx*mirr_tim_table[j] + xcone*degrad;
      znimsboom( tcone, txcone, &dboom, dbmfile);
      if (dboom) {
        mpos[0][i].y = 0;  /* to distinuish pix. from off-planet */
        continue;
      }
    }

	/*
	 * get mpos at the 4 corners, relying on the fact that
	 * cone/x-cone correspond to sample/line directions:
	 * (note that pixel is 2* larger in x-cone than cone,
	 * with a non-uniform weighting function in x-cone)
	 *
	 * in Tube case, we only need the right-hand edge
	 */
    npts = 4;
    del1 = -1.0;
    del2 = -0.5;
    if (tube) {
      dlat[i] = BADP;	/* this is not averaged for tube case */
      npts = 2;
      del1 = 0.0;
      del2 = 0.0;
    }
    else {	/* these must be initialized to 0 as they are summed */
      dlat[i] = 0.0;
      cobuf[i].rads[0] = 0;
      cobuf[i].rads[1] = 0;
    }

/* OLD WAY (G-cube only):
    for (j=0, del1 = -1.0; del1 < 1.101; del1 += 2.0) {
      for (del2 = -0.5; del2 < 0.501; del2 += 1.0, j++) {
*/
      for (j=0; j<npts; j++) {
        zpproj( &tgt_data, &del1, &del2, &lat, &lon[j], LS_LL, lattyp,
	 &rtang, &slant, &ind);
	lat0 = lat;		/* store since lat may get converted det->cen */

	  /*
	   * if point is off-planet then check if we want to try
	   * the radius fudge option:  (only for map_type = 2 or 16 !)
	   */
	if (!ind) {
	  if (!radfudge) goto bad_pt;
	  /* nominal planetocentric radius at this lat/lon: */
	  rplan( &rnom, lat, lon[j], tgt_data.e1rad, tgt_data.e2rad,
	   tgt_data.prad);
	  lfact = rtang/rnom;
	  if (lfact> radfact) goto bad_pt;
          /* only ORTHO & POV allow RADFUDGE */
	  if (map_type == 2) {
	    prtang = pol_rad * lfact;
	    ertang = eq_rad * lfact;
	    if (pgraphic==1) lat = det2cen( lat);
	    ztranv( &ind, map_type, LL_LS, map_samp, map_line, latitude,
	     parallel[0], parallel[1], longitude, scale, map_pole,
	     &mpos[j][i].y, &mpos[j][i].x, &lat, &lon[j], prtang, ertang,
	     north);
	    if (ind && (rtang < eq_rad)) goto bad_pt;	/* BOP */
            if ((map_type==10 || map_type==10) && mpos[j][i].x<0.) 
	      mpos[j][i].x += circum;
	  }
	  else {            /* must be maptype = 16 */
	    t_data_povf.prad = tgt_data.prad * lfact;
	    t_data_povf.e1rad = tgt_data.e1rad * lfact;
	    t_data_povf.e2rad = tgt_data.e2rad * lfact;
	    dum = -1.;	/* flag to ignore BOP test */
	    zpproj( &t_data_povf, &mpos[j][i].y, &mpos[j][i].x, &lat, &lon[j],
	     LL_LS, lattyp, &dum, &dum, &ind);
	    if (!ind && (rtang < rnom)) goto bad_pt;	/* BOP */
	  }
	  if (!tube) {
	    cobuf[i].rads[1] += rtang - rnom;		/* altitude */
	  }
	  else if (j==0) {
	    cobuf[i].rads[1] = rtang - rnom;
	  }
	}
	else {                  /* point is on planet */
	  if (map_type==16) {
	    dum = 1.;	/* flag to use BOP test */
	    zpproj( &t_data_pov, &mpos[j][i].y, &mpos[j][i].x, &lat, &lon[j],
	     LL_LS, lattyp, &dum, &dum, &ind);
	    if (!ind) goto bad_pt;
	    cobuf[i].rads[1] = 0.0;
	  }
	  else {
	    if (pgraphic==1 && (map_type!=10 || oldver)) lat = det2cen( lat);
	    ztranv( &ind, map_type, LL_LS, map_samp, map_line, latitude,
	     parallel[0], parallel[1], longitude, scale, map_pole, 
	     &mpos[j][i].y, &mpos[j][i].x, &lat, &lon[j], pol_rad, eq_rad,
	     north);
	    if (ind) goto bad_pt;
            if ((map_type==9 || map_type==10) && mpos[j][i].x<0.)
	      mpos[j][i].x += circum;
	    cobuf[i].rads[1] = 0.0; /* this is on-planet flag in output_cot */
	  }
	}
	if (!tube) {
	  cobuf[i].rads[0] += slant;			/* slant distance */
	  dlat[i] += lat0;		/* save for geometry calc'n */
	}
	else if (j==0) {
	  cobuf[i].rads[0] = slant;
	  dlat[i] = lat0;
	}

	/* end loop over npts -- handle increments for different cases: */
	if (tube) {
	  del2 = 0.5;	/* (only j=0 case occurs) */
	}
	else {
	  switch (j) {
	    case 0: del1 = -1.0; del2 = 0.5; break;
	    case 1: del1 = 1.0; del2 = -0.5; break;
	    case 2: del1 = 1.0; del2 = 0.5; break;
	  }
	}
      }
/* OLD WAY:
    }	/* end loop over 4 corners (or 2 points, if tube) */

    /* check for wraparound problem in Cyl. projection: */
    if (map_type==9 || map_type==10) {
      for (j=0; j<npts-1; j++) {
        for (jj=j+1; jj<npts; jj++) {
	  delx = mpos[j][i].x-mpos[jj][i].x;
	  if (delx>0.5*circum) mpos[j][i].x -= circum;
	  if (delx<(-0.5*circum)) mpos[jj][i].x -= circum;
	}
      }
    }

    if (tube)
      dlon[i] = lon[0];
    else {
      cobuf[i].rads[0] = 0.25*cobuf[i].rads[0];
      cobuf[i].rads[1] = 0.25*cobuf[i].rads[1];
      dlat[i] = 0.25*dlat[i];
      /* form average long. taking account of long=0 crossing: */
      for (j=0; j<4; j++) {	/* re-order lon in monotonic incr'g sequence */
	for (k=j+1; k<4; k++) {
          if (lon[j]>lon[k]) {
	    dum = lon[j];
	    lon[j] = lon[k];
	    lon[k] = dum;
	  }
	}
      }
      for (k=0, j=1; j<4; j++) {
	if ((lon[j]-lon[j-1])>180.) {
	  k = j;
	  break;
	}
      }
      for (j=0; j<k; j++) lon[j] += 360.;
      for (dlon[i]=0.0, j=0; j<4; j++) dlon[i] += lon[j];
      dlon[i] = 0.25*dlon[i];
      if (dlon[i] > 360.) dlon[i] -= 360.;
    }
    if (tube) {				/* save degrees for backplane */
      cobuf[i].latlon[0] = dlat[i];
      cobuf[i].latlon[1] = dlon[i];
    }
    dlat[i] = dlat[i]/degrad;
    dlon[i] = dlon[i]/degrad;

	/* for label & catalog range, just use last corner: */
    maxsdist = max( slant, maxsdist);
    minsdist = min( slant, minsdist);
    if (i==9 && !rti) {
      trange[ll_rec1] = slant;
      if (!ll_rec1) ll_rec1 = 1;
    }

    continue;	/* in order to skip "error processing" below */

	/* end of mp loop -- jump here out of double loop if *any* corner 
	 * is off-planet, since in that case DN is contaminated by sky;
	 * (can be circumvented by RADFACT option) */
bad_pt:
    mpos[0][i].x = BADP;	/* only this vertex is checked subsequently! */
    mpos[0][i].y = BADP;
    cobuf[i].rads[0] = 0.;	/* clear these, just to be sure */
    cobuf[i].rads[1] = NULL4;	/* on-planet flag in output_cotube */
  }
	/*
	 * finish computation of geometry buffer, using the average
	 * lat/lon for comb:
	 */
  geobuf( mpos, dlat, dlon);

}


/************************************************************************/
FUNCTION geobuf( mpos, dlat, dlon)
/*
 * fill the geometry buffer using lat/lon values computed in find_xy
 */

struct xytype mpos[4][20];
double dlat[20], dlon[20];
{
  int i, j;
  double clat, dword, phic, phid, r, slat, tdlon, xrs;
  vector rr1, u_sc, u_zenith;

  for (i=smpos; i<=empos; i++) {
    if (mpos[0][i].x == (float)BADP) continue;

	/* find both detic and centric latitudes: */
    if (pgraphic<0) phic = phid = dlat[i];
    else if (pgraphic==1) {
      phic = atan( tan(dlat[i]) / rep2 );
      phid = dlat[i];
    }
    else {
      phic = dlat[i];
      phid = atan( rep2 * tan(dlat[i]) );
    }

    tdlon = dlon[i];	/* TO AVOID COMPILER BUG WHEN /OPT */

	/* unit zenith vector in planet coord's at (lat,lon): */
    u_zenith[0] = cos( phid) * cos( tdlon);
    u_zenith[1] = - cos( phid) * sin( tdlon);	/* - because W long. */
    u_zenith[2] = sin( phid);

	/* unit vector from surface pixel to s/c */
    if (pgraphic<0) {	/* spherical case */
      for (j=0, xrs=0.; j<3; j++) {
	u_sc[j] = cobuf[i].rs[j] - eq_rad*u_zenith[j];
	xrs += u_sc[j]*u_sc[j];
      }
    }
    else {
      /* ellipsoidal case:  first construct vector from planet center,
       * using planetocentric lat */
      slat = sin(phic);
      clat = cos(phic);
      r = eq_rad/sqrt(clat*clat+rep2*slat*slat);
      rr1[0] = r*clat*cos(tdlon);
      rr1[1] = -r*clat*sin(tdlon);
      rr1[2] = r*slat;
      for (j=0, xrs=0.; j<3; j++) {
	u_sc[j] = cobuf[i].rs[j] - rr1[j];
	xrs += u_sc[j]*u_sc[j];
      }
    }
    xrs = sqrt( xrs);
    for (j=0; j<3; j++) u_sc[j] = u_sc[j]/xrs;

	/* emission angle: */
    cobuf[i].geom[1] = acos( u_zenith[0]*u_sc[0] + u_zenith[1]*u_sc[1] +
     u_zenith[2]*u_sc[2]);

	/* incidence angle: */
    cobuf[i].geom[0] = acos( u_zenith[0]*cobuf[i].u_sun[0] +
     u_zenith[1]*cobuf[i].u_sun[1] + u_zenith[2]*cobuf[i].u_sun[2]);

	/* phase angle: (THIS IS NOW DONE IN GET_TGT_DATA) */
/*    cobuf[i].geom[2] = acos( u_sc[0]*cobuf[i].u_sun[0] +
 *     u_sc[1]*cobuf[i].u_sun[1] +
 *     u_sc[2]*cobuf[i].u_sun[2]); */

	/* photometric function correction: */
    if (pfunc==0) cobuf[i].photcor = 1.0;
    else if (pfunc==1) cobuf[i].photcor = cos( cobuf[i].geom[0]);
    else if (pfunc==2) {
      if (p_minn == 0.0) cobuf[i].photcor = cos( cobuf[i].geom[1]);
      else if (p_minn == 1.0) cobuf[i].photcor = cos( cobuf[i].geom[0]);
      else {
	dword = cos( cobuf[i].geom[0]) * cos( cobuf[i].geom[1]);
	if (dword > 0.0) cobuf[i].photcor =
	 pow( dword, p_minn) / cos( cobuf[i].geom[1]);
      }
    }
    else if (pfunc==3) cobuf[i].photcor = cos(cobuf[i].geom[0]) /
     (cos(cobuf[i].geom[0]) + cos(cobuf[i].geom[1]));
    if (cobuf[i].photcor <= 0.0) cobuf[i].photcor = 1.0;
  }

	/* min/max lat/longs for label: */
  for (i=smpos; i<=empos; i++) {
    if (mpos[0][i].x == (float)BADP || mpos[0][i].x < 0.5 ||
        mpos[0][i].x > (float)osize[1]+0.5 || mpos[0][i].y < 0.5 ||
        mpos[0][i].y > (float)osize[0]+0.5) continue;
    /* these will be replaced for G-cube in write_latlon: */
    mmlat[0] = min( degrad*dlat[i], mmlat[0]);
    mmlat[1] = max( degrad*dlat[i], mmlat[1]);
    /* lonrange>0 occurs only for Tube case if min/max long's not done in
     * set_projection (that feature has been effectively disabled) */
    if (lonrange) { 
      dword = degrad*dlon[i];
      if (lonrange==1 && dword>180.) dword -= 360.;
      if (lonrange==2 && dword<0.) dword += 360.;
      mmlon[0] = min( dword, mmlon[0]);
      mmlon[1] = max( dword, mmlon[1]);
    }
  }

/*  if (!cat_flag) return; */

	/* min/max catalog stuff (leave lon for end because of wraparound
	 * problem) */
  for (i=smpos; i<=empos; i++) {
    if (mpos[0][i].x == (float)BADP) continue;
    tincid[0] = min( degrad*cobuf[i].geom[0], tincid[0]);
    tincid[1] = max( degrad*cobuf[i].geom[0], tincid[1]);
    temiss[0] = min( degrad*cobuf[i].geom[1], temiss[0]);
    temiss[1] = max( degrad*cobuf[i].geom[1], temiss[1]);
    tphase[0] = min( degrad*cobuf[i].geom[2], tphase[0]);
    tphase[1] = max( degrad*cobuf[i].geom[2], tphase[1]);
  }
}


/*************************************************************************/
int FUNCTION get_pntng( sclk, cang)
/*
 * return instrument pointing (C-matrix Euler angles + Cone/Clock) at
 * beginning of specified SCLK (mf)
 *
 * return the angles in radians 
 */
int sclk;
struct pntng_typ *cang;
{
  int i, i1, i2, lo1, lo2, hi1, hi2;
  double delang, frac;
  struct pntng_typ cang1, cang2;

  vstat = extract_pntng( sclk, cang);

  if (!vstat) {
    if (!c_extrap) return vstat;
    if (aacsf) {
      /* reset aacsfile to start of file for assemble_pntng */
      rewind( aacsfil);
      is0 = 0;
    }
	/*
	 * if no data for this SCLK, find 2 closest records with data and
	 * inter/extrapolate;  since we prefer interpolation over
	 * extrapolation, find the 2 closest on each side (if any),
	 * and then decide which to use:
	 */
    i = lo1 = lo2 = 0;
    /* we must read the AACSFILE in ascending sclk order, so start CK_TOL
     * away and read all points up to SCLK-1, replacing lo1/2 as we go along */
    for (i=0; i<CK_TOL; i++) {
      vstat = extract_pntng( sclk-CK_TOL+i, cang);
      if (vstat) {
	lo2 = lo1;
	lo1 = sclk-CK_TOL+i;
      }
    }
    i = hi1 = hi2 = 0;
    while (hi1==0 || hi2==0) {
      i++;
      if (i>CK_TOL || sclk+i >= file_hclk) break;
      vstat = extract_pntng( sclk+i, cang);
      if (vstat) {
	if (hi1==0) hi1 = sclk+i;
	else hi2 = sclk+i;
      }
    }
    if (lo1==0 || ((sclk-lo1 > hi2-sclk+1) && hi2!=0)) {
      i1 = hi1;
      i2 = hi2;
    }
    else if (hi1==0 || ((hi1-sclk > sclk-lo2+1) && lo2!=0)) {
      i1 = lo2;
      i2 = lo1;
    }
    else {
      i1 = lo1;
      i2 = hi1;
    }
    if (i1==0 || i2==0 || i1==i2) return 0;
      
    if (aacsf) {
      /* reset aacsfile to start of file for assemble_pntng */
      rewind( aacsfil);
      is0 = 0;
    }
    vstat = extract_pntng( i1, &cang1);
    vstat = extract_pntng( i2, &cang2);
    if (aacsf) {
      /* reset aacsfile to start of file for assemble_pntng */
      rewind( aacsfil);
      is0 = 0;
    }

    /* check for wraparound in ra/twist/clock: */
    delang = cang2.ra - cang1.ra;
    if (delang>zpi()) cang1.ra += ztwopi();
    if ((-delang)>zpi()) cang2.ra += ztwopi();
    delang = cang2.twist - cang1.twist;
    if (delang>zpi()) cang1.twist += ztwopi();
    if ((-delang)>zpi()) cang2.twist += ztwopi();
    delang = cang2.clock - cang1.clock;
    if (delang>180.) cang1.clock += 360.;
    if ((-delang)>180.) cang2.clock += 360.;

    frac = (double)(sclk-i1) / (double)(i2-i1);
    cang->ra = (1.-frac) * cang1.ra + frac * cang2.ra;
    if (cang->ra>ztwopi()) cang->ra -= ztwopi();
    cang->dec = (1.-frac)* cang1.dec + frac * cang2.dec;
    cang->twist = (1.-frac) * cang1.twist + frac * cang2.twist;
    if (cang->twist>ztwopi()) cang->twist -= ztwopi();
    cang->cone = (1.-frac) * cang1.cone + frac * cang2.cone;
    cang->clock = (1.-frac) * cang1.clock + frac * cang2.clock;
    if (cang->clock>360.) cang->clock -= 360.;
  }
  return 1;
}

/************************************************************************/
FUNCTION get_omrs( pdist, slat, slon, olat, olon, pssl, psss, bop)
/*
 * compute OM-matrix and subspacecraft line & sample, if lat/long of
 * SSC and optical axis have been specified -- the OM matrix is stored
 * directly into structure t_data_pov
 *
 * if PDIST > 0, then the RS-vector is also computed
 */
double pdist, slat, slon, olat, olon, *pssl, *psss;
int *bop;
{
  double e1rad, e2rad, prad, pdist1, rolat, rolon, rslat, rslon, pscal, xnorth;
  static double vec[3] = {0.0, 0.0, 1.0};
  matrix om1;

  rolat = olat/degrad;
  rolon = olon/degrad;
  rslat = slat/degrad;
  rslon = slon/degrad;

  if (pdist>0) {
    t_data_pov.rs[0] = pdist*cos(rslat)*cos(rslon);
    t_data_pov.rs[1] = -pdist*cos(rslat)*sin(rslon);
    t_data_pov.rs[2] = pdist*sin(rslat);
    pdist1 = pdist;
  }
  else pdist1 = sqrt( t_data_pov.rs[0] * t_data_pov.rs[0] +
		      t_data_pov.rs[1] * t_data_pov.rs[1] +
		      t_data_pov.rs[2] * t_data_pov.rs[2] );

  xnorth = north/degrad - zhalfpi();

	/* convert longs to East for GETOM: */
  rslon = ztwopi()-rslon;
  rolon = ztwopi()-rolon;

	/* convert to REAL*8: */
  e1rad = t_data_pov.e1rad;
  e2rad = t_data_pov.e2rad;
  prad = t_data_pov.prad;

  zgetom( &e1rad, &e2rad, &prad, &rslat, &rslon, &pdist1, &rolat, &rolon, 
   vec, &xnorth, om1, bop);
  if (zfailed()) zmabend(" ** an error occurred in GETOM **");

	/* PPROJ wants OM^-1 */
  zxpose( om1, t_data_pov.om);

	/* convert RS to camera coordinates & compute SSC line/samp
	 * -- vec is actually negative of what we want */
  zmtxv( om1, t_data_pov.rs, vec);

  pscal = - t_data_pov.focal * t_data_pov.cscale / vec[2];
  *pssl = t_data_pov.aline - vec[1]/scale;
  *psss = t_data_pov.asamp - vec[0]/scale;

}


/*************************************************************************/
FUNCTION get_sflux()
/* compute solar flux */
{
  int i;
  double et1, tssb[6];
  float dist;

  i = comp_sclk( loclk);  
  nims_s2et( i, &et1);
  i = comp_sclk( hiclk);  
  nims_s2et( i, &etime);
  etime = 0.5*(etime+et1);	/* midpoint of observation */
  /* get state of target relative to SSB:
   * (should correct for Solar radius, but ignore that) */
  zspkssb( ids.target, etime, esystem, tssb);
  dist = sqrt( tssb[0]*tssb[0] + tssb[1]*tssb[1] + tssb[2]*tssb[2] );
  dist = 1.0e5*dist;		/* convert km to cm */
  solar( dist, sol_flux);
}


/*************************************************************************/
FUNCTION get_tgt_data( sc_time, rti, mp, nora, geo_flag, pcone, pxcone)
/*
 * Store data into the structure used by PPROJ for a given scan (nit),
 * which is designated by rti = 0 or 5.
 * 
 * Pointing for each mirror position is interpolated between mf boundaries
 *
 * cross-cone and cone offsets from SSI boresight are also returned
 * (cross-cone is *negative* of NIMS definition [= rotation around -M],
 * which means that the returned quantity is in the *same* sense as
 * Clock when Cone ~ 90 deg.)
 *
 * if nora=1, this routine is being called to compute north angle (this
 * is only done from set_pov & set_projection)
 *
 * if geo_flag > 0, then distances and photometric angles are computed,
 * (also lat/longs for catalog if cat_flag > 0)
 *
 * geo_flag<0 is for TEST=9
 */

int sc_time, rti, mp, nora, geo_flag;
double *pcone, *pxcone;
{
  int i, j, imp, xsclk;
  double alpha, delta, kappa, con, clk, azi, dmtim, tcos, tsin, wcone, x;
  matrix c_nims;
  vector v0, v1;
  static int lastmf;
  static matrix me;
  static long sclk_time = 0;
  static vector cenbod, sc, xsc, xsol, sol;
  static struct pntng_typ me_ang, xme_ang, c_ang, xc_ang;
  static double da, dd, dk, dcon, dclk, cenboddis, solscdis, soldis;
  static vector dsc;

  if (sc_time==0) zmabend(" *** invalid call to get_tgt_data ***");

	/* first, make sure optical axis line/samp set to 0
	 * (this is always the case for INPUT projections -- if
	 * POV is output, then t_data_pov has the correct values */
  tgt_data.aline = 0.;
  tgt_data.asamp = 0.;

  imp = mp;
  if (rti) imp += 20;		/* (rti s/b 0 or 5 only) */

  if (sc_time!=sclk_time) {
    sclk_time = sc_time;
    for (j=0; j<nmf; j++) if (mf[j].sclk==sclk_time) break;
    if (j>=nmf) zmabend(" *** error 1 interpolating C-angles ***");
    c_ang = mf[j].cang;
    if (j<(nmf-1)) {
      xc_ang = mf[j+1].cang;
      xsclk = mf[j+1].sclk;
    }
    lastmf = 0;
	/* if last mf must extrapolate -- also if last before gap */
    if (j==(nmf-1) || xsclk!=(sclk_time+1)) {
      lastmf = 1;
      xc_ang = mf[j-1].cang;
      xsclk = mf[j-1].sclk;
      if (!nochk && c_extrap && xsclk!=(sclk_time-1))
       zmabend(" *** error 2 interpolating C-angles ***");
    }
	/* if there's still a gap must also extrapolate, but this won't work
	 * if !c_extrap ... */
    if (xsclk!=(sclk_time+1) && xsclk!=(sclk_time-1)) {
      xsclk = sclk_time+1;
      lastmf = 0;
      vstat = extract_pntng( xsclk, &xc_ang);
      if (!vstat) zmabend(" *** error 3 interpolating C-angles ***");
    }

    /* check for wraparound in ra/twist ... */
    da = xc_ang.ra - c_ang.ra;
    if (da>zpi()) da -= ztwopi();
    if ((-da)>zpi()) da += ztwopi();
    dd = xc_ang.dec - c_ang.dec;
    dk = xc_ang.twist - c_ang.twist;
    if (dk>zpi()) dk -= ztwopi();
    if ((-dk)>zpi()) dk += ztwopi();
    if (wamp>1.e-7 && wfreq<1.e-7) {	/* wobble is fcn. of clock */
      dcon = xc_ang.cone - c_ang.cone;
      dclk = xc_ang.clock - c_ang.clock;
      if (dclk>180.) dclk -= 360.;
      if ((-dclk)>180.) dclk += 360.;
    }

    nims_ps_orient( sclk_time, sc, sol, cenbod, &me_ang);
    eul2mat( me_ang.ra, me_ang.dec, me_ang.twist, me); /* ME: Earth -> Planet */
	/* (we interpolate 'sc', but not 'me') */
    nims_ps_orient( xsclk, xsc, xsol, cenbod, &xme_ang);
    for (i = 0; i < 3; i++) dsc[i] = xsc[i] - sc[i];

	/* solar & centr.body are assumed constant during mirror scan: */
    for (i=0, solscdis=0.; i<3; i++) solscdis += sol[i]*sol[i];
    solscdis = sqrt( solscdis);
    for (i=0, cenboddis=0.; i<3; i++) cenboddis += cenbod[i]*cenbod[i];
    cenboddis = sqrt( cenboddis);

	/* make this unit vector: */
    for (i=0; i<3; i++) xsol[i] = sol[i]/solscdis; 
  }
  dmtim = mirr_tim_table[imp];
  if (lastmf) dmtim = -dmtim;	/* to cancel negative delta-abscissa */
	/*
	 * do linear interpolation of camera orientation and picture 
	 * body vector in time.
	 */
  alpha = c_ang.ra + dmtim * da;
  delta = c_ang.dec + dmtim * dd;
  kappa = c_ang.twist + dmtim * dk;
  if (wamp>1.e-7 && wfreq<1.e-7) {	/* wobble is fcn. of clock */
    con = c_ang.cone + dmtim * dcon;
    clk = c_ang.clock + dmtim * dclk;
  }

  /* rotor wobble correction, Twist component: */
  if (wamp>1.e-7) {
    if (wfreq>0.0) {
      x = wfreq*(sc_time+dmtim-beg_sclk) + wphase + 90.0;
      if (aacsf)
        wcone = c_ang.cone/degrad;
      else
        wcone = wob_cone;
      kappa -= wamp*cos(x/degrad)*sin(wcone);
    }
    else {			/* wobble is fcn. of clock */
      x = wphase-clk;
      kappa -= wamp*cos(x/degrad)*sin(con/degrad);
    }
  }

  eul2mat( alpha, delta, kappa, c_nims);	/* C: Earth -> Camera */

	/*
	 * rotate C matrix for instrument / mirror offsets ...
	 * in order not to have to go to Stator coordinate system,
	 * treat an increase in cone angle as a right-handed rotation
	 * around the +N axis, and cross-cone as a rotation around -M
	 * axis -- valid for small offsets from boresight
	 */

  *pcone = nims_bore_cone;
  *pxcone = -nims_bore_xcone;

  if (!mirrop) *pxcone -= mirr_pos_xcone_stop;	/* spectrometer mode */

  else if (mirror == UP) {
    *pcone += mirr_pos_table[mp].cone_up;
    *pxcone -= mirr_pos_table[mp].xcone_up;
  }
  else if (mirror == DOWN) {
    *pcone += mirr_pos_table[mp].cone_down;
    *pxcone -= mirr_pos_table[mp].xcone_down;
  }
  else if (mirror == WAIT) *pxcone -= mirr_pos_xcone_wait;

  /* have faith that cone_wait and cone_stop will be 0! */

  /* rotor wobble correction, Cone/X-cone components: */
  if (wamp>1.e-7) {
    if (wfreq>0.0) {
      x = wfreq*(sc_time+dmtim-beg_sclk)+wphase;
      *pcone += wamp*cos(x/degrad);
      x += 90.0;
      if (aacsf)
        wcone = c_ang.cone/degrad;
      else
        wcone = wob_cone;
      *pxcone -= wamp*cos(x/degrad)*cos(wcone);
    }
    else {			/* wobble is fcn. of clock */
      x = wphase-clk;
      *pcone += wamp*sin(x/degrad);
      *pxcone -= wamp*cos(x/degrad)*cos(con/degrad);
    }
  }

  if (*pcone!=0.0) zrotmat( c_nims, *pcone, 2, c_nims);
  if (*pxcone!=0.0) zrotmat( c_nims, *pxcone, 1, c_nims);

    /* store the angles if PTUBE or TEST=9: */
  if (ptub || (xtest==9 && !rti && mp==9 && geo_flag<0)) {
    zm2eul( c_nims, 3, 2, 3, &kappa, &delta, &alpha);
      /* note this gives the GLL version of twist, which is what is
       * wanted in the backplanes */
    if (ptub) {
      cobuf[mp].euler[0] = alpha;
      if (cobuf[mp].euler[0] < 0.0) cobuf[mp].euler[0] += ztwopi();
      cobuf[mp].euler[1] = zhalfpi()-delta;
      cobuf[mp].euler[2] = kappa;
      if (cobuf[mp].euler[2] < 0.0) cobuf[mp].euler[2] += ztwopi();
    }
    else {
      i = sc_time/91;
      j = sc_time%91;
      sprintf( msg, " %d.%02d %8.3f %8.3f %8.3f %8.4f %8.4f",
       i, j, degrad*alpha, degrad*(zhalfpi()-delta), degrad*kappa,
       degrad*(*pcone), -degrad*(*pxcone));
      zvmessage( msg,"");
    }
  }

	/* OM = ME * CT : Camera -> Planet 
	 * but TRANV wants OM**-1, so compute this directly: */
  zmxmt( c_nims, me, tgt_data.om);

	/* reverse sc-planet vector to planet-sc and rotate to planet
	 * coordinates: */
  for (i = 0; i < 3; i++) v0[i] = -(sc[i] + dmtim * dsc[i]);
  zmxv( me, v0, tgt_data.rs);	/* RS vector (planet coord's) */

  if (nora > 0) {
	 /* compute North Angle = angle from UP in image 
	  * clockwise to projection of body rotation axis 
	  */
    v1[0]=0.0; v1[1]=0.0; v1[2]=1.0; 	/* unit Z-vector */
    zmxv( tgt_data.om, v1, v1);/* rotate planet Z-axis into Camera coord's */
    north = atan2( v1[0], v1[1]);
    north = north*degrad;		/* TRANV wants degrees */
    north = 180. - north;		/* camera coordinates have Y down */
    if (north<0.) north += 360.;
  }

  if (geo_flag <= 0) return;

	/*
	 * fill parts of geometry buffer for cocube computations
	 */

  /* phase angle -- this is just angle between unit vectors
   * in LOS (negated) and to sun;
   * LOS in EME coordinates is Camera L-axis transformed: */
  v1[0] = v1[1] = 0.0;
  v1[2] = 1.0;
  zmtxv( c_nims, v1, v1);
  for (j=0; j<3; j++) v1[j] = -v1[j];
  cobuf[mp].geom[2] = acos( v1[0]*xsol[0] + v1[1]*xsol[1] + v1[2]*xsol[2] );

	/* save planet - s/c vector: */
  for (j=0; j<3; j++) cobuf[mp].rs[j] = tgt_data.rs[j];

	/* get planet-sun unit vector & transform to planet coord's: */
  soldis=0.; 
  for (j=0; j<3; j++) {
    v1[j] = sol[j]+v0[j];
    soldis += v1[j]*v1[j];
  }
  soldis = sqrt( soldis);
  for (j=0; j<3; j++) cobuf[mp].u_sun[j] = v1[j]/soldis;
  zmxv( me, &cobuf[mp].u_sun[0], &cobuf[mp].u_sun[0]);

	/* save ranges for label: */
  maxsun = max( solscdis, maxsun);
  minsun = min( solscdis, minsun);
  maxcenbod = max( cenboddis, maxcenbod);
  mincenbod = min( cenboddis, mincenbod);

  if (mp==9 && rti==0) {

	/* compute mean azimuths with s.d. -- kludge for label items --
	 * these are azimuthal angles in the (M,N) plane of the camera
	 * coordinates;  M = Sample (cone), N = Line (cross-cone)
	 * CAUTION:  these azimuths are defined to be in the plane tangent
	 * to the surface at P5 point by the definition borrowed from VGR 
	 * ... is in the image plane ok??  TBD: CHECK THIS */
    zmxv( c_nims, v1, v1); /* rotate solar vector into Camera coord's */
    azi = degrad * atan2( v1[1], v1[0]);
    sun_azi += azi;
    zmxv( c_nims, v0, v0); /* rotate s/c vector into Camera coord's */
    azi = degrad * atan2( v0[1], v0[0]);
    sc_azi += azi;
    n_azi++;
  }

  crange[ll_rec2] = solscdis;

	/* Lon(W) and Lat of subspacecraft point: */
  ssclon[ll_rec2] = degrad * atan2( -tgt_data.rs[1], tgt_data.rs[0]);
  if (ssclon[ll_rec2]<0.) ssclon[ll_rec2] += 360.;

  x = sqrt( tgt_data.rs[0] * tgt_data.rs[0] +
	    tgt_data.rs[1] * tgt_data.rs[1] +
	    tgt_data.rs[2] * tgt_data.rs[2] );
  ssclat[ll_rec2] = degrad * asin( tgt_data.rs[2] / x);

	/* and subsolar point: */
  ssollon[ll_rec2] = degrad * atan2( -cobuf[mp].u_sun[1], cobuf[mp].u_sun[0]);
  if (ssollon[ll_rec2]<0.) ssollon[ll_rec2] += 360.;
  ssollat[ll_rec2] = degrad * asin( cobuf[mp].u_sun[2]);

  if (!ll_rec2) {
    /* first time around, check to see whether this is a projection that
     * needs min/max longitude determination in geobuf, and what range 
     * (-180->180 or 0->360) should be used: */
    if (mmlon[0] <= 360. && mmlon[1] >= -180.)
      lonrange = 0;	/* was done in set_projection */
    else if (!tube)
      lonrange = 0;	/* will be done in write_latlon */
    else if (ssclon[0]<90. || ssclon[0]>270.)
      lonrange = 1;
    else
      lonrange = 2;

    ll_rec2 = 1;
  }

}


/*************************************************************************/
FUNCTION get_uparms()
/*
 * process most user parameters
 */
{
  int bop, foff[MAX_EDRS], flen[MAX_EDRS], isclk[200], jbuf[20], i, j,
   ntlat, nval1, nval2, nval3, nval4, nval5;
  double csc, lat, oal, oas, temp;
  float buf[2];
  char msg1[10], tstring[9];
  nims2_sclk_typ xsclk;

  /* use certain recent changes to code? */
  /* NOT NEEDED AFTER END OF SYS.PROC.
  zvparm( "OLD_VER", tstring, &nvalues, &def, 1, 0);
  if (nvalues) 
    oldver = !strcmp(tstring,"OLD_VER");
  else {
    zvparm( "INITIALS", initials, &nvalues, &def, 1, 0);
    oldver = !strcmp(initials,"MSY") || !strcmp(initials,"MPS");
  }
  if (oldver) zvmessage(" using old version of oct/dec00 fixes","");
  */
  oldver = 0;

  zvpcnt( "EDR", &nedrs);
  if (nedrs > MAX_EDRS) zmabend(" *** too many EDRs ***");
  zvparm( "EDR", fname, &nvalues, &def, nedrs, 0);
  zvsptr( fname, nedrs, foff, flen);

  for (i = 0; i < nedrs; i++) {
    edrs[i].foff = foff[i];
    edrs[i].opened = FALSE;
    edrs[i].first_open = FALSE;
  }

	/* read in despike data if available: */
  zvpcnt( "DSPKFILE", &nspifils);
  if (nspifils>1 && nspifils != nedrs) zmabend(
   "** must be either one spike file, or one per EDR **");
  if (nspifils) {
    zvparm( "DSPKFILE", dsfile, &nvalues, &def, nspifils, 0);
    zvsptr( dsfile, nspifils, dsfnoff, flen);
  }
  /* if there is only one spike file, read it here -- otherwise
   * read each one in as its EDR is processed */
  if (nspifils==1 && strcmp( dsfile, "DUMMY_DSPK.DAT")) read_dspk(dsfile);

  zvparm( "OUTTYPE", tstring, &nvalues, &def, 1, 0);
  if (!strcmp( tstring, "GCUBE")) tube = 0;
  else tube = 1;	/* includes TUBE & PTUBE */
  ptub = 0;
  if (!strcmp( tstring, "PTUBE")) ptub = 1;

  if (!tube && calib) 
    zmabend(" CALIBRATION target requires Tube only!");

  org = 0;
/* no need to ever implement this: 
  zvparm( "OUTORG", tstring, &nvalues, &def, 1, 0);
  if (!strcmp( tstring, "BSQ")) org = 0;
  else zmabend(" ** BIL/BIP options temporarily disabled **");
/* DISABLED UNTIL SCALE_DATA IS BROUGHT UP TO SPEED: */
/*  else if (!strcmp( tstring, "BIL")) org = 1; */
/*  else if (!strcmp( tstring, "BIP")) org = 2; */
  
  zvparm( "OUTDETS", jbuf, &nvalues, &def, 2, 0);
  if (nvalues) {
    s_det = jbuf[0]-1;
    e_det = jbuf[1]-1;
  }
  else {
    s_det = 0;
    e_det = N_DETS-1;
  }

  for (j=0; j<N_DETS; j++) skipdet[j] = 0;
  zvparm( "SKIPDET", &jbuf, &nvalues, &def, 16, 0);
  for (j=0; j<nvalues; j++) {
    i = jbuf[j]-1;	/* param. is 1-based, program variable is 0-based */
    skipdet[i] = 1;
  }

  if (!nvalues) {	/* no OUTBAND if OUTDETS was spec'd */
    lami = 0;
    zvparm( "OUTBAND", &lami, &nvalues, &def, 1, 0);
  }

  zvparm( "NSKIP_GP", &nskipgp, &nvalues, &def, 1, 0);

  zvparm( "B_E_MP", jbuf, &nvalues, &def, 2, 0);
  smpos0 = jbuf[0];
  empos0 = jbuf[1];
  if (smpos0>empos0) zmabend(" invalid B_E_MP values");

  zvparmd( "CKTOL", &s_tol, &nvalues, &def, 1, 0);

	/* pointing offset parameter: */
  zvparm( "DPOINT", dpoint, &nvalues, &def, 2, 0);

	/* SCLK range for pointing offset: */
  zvparm( "DPT_SCLK", dpt_sclk0, &nvalues, &def, 2, 0);
  /* convert to continuous sclk-count: */
  for (i=0; i<nvalues; i++) {
    xsclk.rim = dpt_sclk0[i]/100;
    xsclk.mod91 = dpt_sclk0[i]%100;
    dpt_sclk[i] = comp_sclk(xsclk);
  }

	/* pointing wobble parameters (amplitude, phase): */
  zvparm( "WAMP", &wamp, &nvalues, &def, 1, 0);
  if (wamp>1.e-7) {
    zvparm( "WFREQ", &wfreq, &nvalues, &def, 1, 0);
    /* (wfreq>=0, by PDF constraint!) */
    zvparm( "WPHASE", &wphase, &nvalues, &def, 1, 0);
    if (!aacsf) {
      if (wfreq<1.e-7) zmabend(" need WFREQ if no AACSFILE given!");
      zvparm( "WCONE", &wob_cone, &nvalues, &def, 1, 0);
      if (!nvalues) zmabend(" need WCONE if no AACSFILE given!");
      wob_cone /= degrad;
    }
  }

	/* simulate ephemeris error by time offset: */
  zvparm( "EPHERR", &epherr, &nvalues, &def, 1, 0);

	/* deboom file: */
  zvparm( "DBMFILE", dbmfile, &nvalues, &def, 1,0);
  if (!nvalues || !strcmp( dbmfile, "DUMMY_DBM.DAT")) dbm = 0;
  else if (c_spice) dbm = 0;	/* until Rotor kernels are supported */
  else {
    dbm = 1;
    /* make a dummy call to NIMSBOOM to open the file and avoid
     * problems with GETLUN: */
    znimsboom( 0., 0., &i, dbmfile);
  }

  zvparm( "SDBAND", &lamsd, &nlamsd, &def, 1, 0);
		/* if not specified, will be set after nb is determined */

  zvparm( "SDGEO", &geosd, &nvalues, &def, 1, 0);

/*
  zvparm( "OBSNAME", obsnam, &nvalues, &def, 1, 0);

  zvparm( "OBSEXT", obsext, &nvalues, &def, 1, 0);

  zvparm( "MOS_NUM", &i, &nvalues, &def, 1, 0);
  sprintf( mosnum, "%02d", i);
*/

  zvparm( "TEST", &xtest, &nvalues, &def, 1, 0);

  zvparm( "CALTYPE", cal_type, &nvalues, &def, 1, 0);
  if (xtest) caltyp = 0;
  else if (!strcmp( cal_type, "NOCAL")) {
    caltyp = 0;
    sprintf( msg, " No radiance calibration done, raw DN is output");
    zvmessage(" ","");
    zvmessage( msg,"");
  }
  else if (!strcmp( cal_type, "RAD")) {
    caltyp = 1;
    sprintf( msg, " Radiance calibration will be done");
    zvmessage(" ","");
    zvmessage( msg,"");
  }

  radscal = 0;
  if (caltyp) zvparm( "RADSCAL", &radscal, &nvalues, &def, 1, 0);

  zvparm( "DARKTYPE", dark_type, &nvalues, &def, 1, 0);
  if (!strcmp( dark_type, "NOUPDAT")) drktyp = 0;
  else if (!strcmp( dark_type, "NEARVAL")) drktyp = 1;
  else if (!strcmp( dark_type, "PREVVAL")) drktyp = 2;
  else if (!strcmp( dark_type, "INTERP")) drktyp = 3;

  zvparm( "DRKTHRSH", &drkfac, &nvalues, &def, 1, 0);
  drkchk = (nvalues>0);

  zvparm( "PHOTFUNC", phot_func, &nvalues, &def, 1, 0);
  if (!strcmp( phot_func, "LAMBERTF")) {
    sprintf( phot_func, "LAMBERT");	/* remove the "F" */
    sprintf( msg, "Lambert ");
    pfunc = 1;
  }
  else if (!strcmp( phot_func, "MINNAERT")) {
    sprintf( msg, "Minnaert ");
    pfunc = 2;
    zvparmd( "MINN_EXP", &p_minn, &nvalues, &def, 1, 0);
  }
  else if (!strcmp( phot_func, "LOMMEL")) {
    sprintf( msg, "Lommel-Seeliger ");
    pfunc = 3;
  }
/*  else if (!strcmp( phot_func, "HAPKE") {   -- TBD IN FUTURE? */
  else pfunc = 0;

  if (pfunc > 0) {
    zvparmd( "PHOTCUT", &pfcut, &nvalues, &def, 1, 0);
    if (!nvalues) {
      if (!strcmp(target,"JUPITER")) pfcut = 4.0;
      else pfcut = 5.3;
    }
    strcat( msg, "photometric function applied for wavelength (mu) < ");
    sprintf( msg1, "%.2f", pfcut);
    strcat( msg, msg1);
    zvmessage( msg,"");
  }

  zvparm( "GOFFSET", &g_off, &nvalues, &def, 1, 0);

  zvparm( "GSTART", &gstart, &u_gst, &def, 1, 0);

  zvparm( "GAIN", &gain[0], &nvalues, &def, 1, 0);

  zvparm( "CHOPPER", &chopper, &nvalues, &def, 1, 0);

  /* check compression status flag? */
  chk_comp = zvptst( "COMP_CHK");

  /* check for missing LRS/Housekeeping data */
  nohkp = zvptst( "NOHKP");

  /* option to throw out some mirror positions (GCUBE only!): */
  mirrsel = 0;
  if (!tube) zvparm( "MIRROMIT", nomirr, &mirrsel, &def, 39, 0);

  zvparm( "INSMODE", &u_imode, &nvalues, &def, 1, 0);
  if (nvalues==0) u_imode = -1;
  if (u_imode<0 && nohkp) zmabend("*** must specify mode if NOHKP ***");

  zvparm( "NLGP", &nlgp, &nvalues, &def, 1, 0);

  zvparm( "STOPSLID", stop_slide, &nvalues, &def, 1, 0);

	/* up to 100 SCLK intervals allowed: */
  zvparm( "SCLK", isclk, &nintrvl, &def, 200, 0);
  if (nintrvl/2 != (nintrvl+1)/2) zmabend(" SCLK must have even # items");
  nintrvl = nintrvl/2;
	/* unpack into begin/end CLK for each interval;
	 * also store initial/end SCLK into LOCLK/HICLK:  */
  for (i=0; i<nintrvl; i++) {
    hiclk.rim = isclk[2*i]/100;
    hiclk.mod91 = isclk[2*i]%100;
    hiclk.rti = 0;
    bclk[i] = comp_sclk( hiclk);
    if (i==0) loclk = hiclk;
    hiclk.rim = isclk[2*i+1]/100;
    hiclk.mod91 = isclk[2*i+1]%100;
    eclk[i] = comp_sclk( hiclk);
  }
  range_lclk = bclk[0];
  range_hclk = eclk[nintrvl-1];

  refsclk = 0;
  zvparm( "REFSCLK", &refsclk, &nvalues, &def, 1, 0);
  if (nvalues) {
    i = refsclk / 100;
    j = refsclk % 100;
    refsclk = 91*i + j;	/* convert to internal definition of sclk */
    zvparm( "REFMP", &refmp, &nvalues, &def, 1, 0);
  }

  nl = ns = 0;				/* flag in case not specified */
  zvparm( "OUTSIZ", osize, &nvalues, &def, 2, 0);
  if (nvalues) {
    nl = osize[0];
    ns = osize[1];
  }

  footprint = zvptst( "FOOTPRNT") && !xtest;	/* "BIN" parameter */

  thresh = 0.;
  if (footprint) {
    zvparm( "THRESH", &thresh, &nthresh, &def, 1, 0);
    zvparm( "MAXDSTOR", &maxdistor, &ndistor, &def, 1, 0);
    zvparm( "FPNGRID", &fpgrid, &nvalues, &def, 1, 0);
    nfpwts = fpgrid*fpgrid*fpgrid;
  }
  else {
	/* check if other than AVERAGE was selected for overlapping pixels */
    use_max = zvptst( "MAXIMUM");		/* "OVERLAP" parameter */
    use_last = zvptst( "REPLACE");
  }

  histbin = 0;
  if (!tube) {
    /* histogram-binning option: */
    zvparm( "HBINSIZE", &hbinsize, &histbin, &def, 1, 0);
    /* hbinsize must be <= MAX_HBINSIZE -- rely on PDF to enforce that */
    if (histbin) {
      zvparm( "HBINSTEP", &hbinstep, &nvalues, &def, 1, 0);
      hbstp2 = hbinstep/2;	/* (for convenience) */
      zvparm( "HISTCRIT", tstring, &nvalues, &def, 1, 0);
      histcrit = 0;
      if (!strcmp(tstring,"MEDIAN")) histcrit = 1;
    }
  }

  /* exclude incidence/emission ranges for GCUBE only */
  cutinc = emicut = 0;
  if (!tube) {
    zvparmd( "INCCUT", &inccut, &cutinc, &def, 1, 0);
    if (cutinc) inccut /= degrad;
    zvparmd( "EMICUT", &emicut, &cutemi, &def, 1, 0);
    if (cutemi) emicut /= degrad;
  }

  c_extrap = zvptst( "CEXTRAP");

  zvparmd( "SLEW_TOL", &slew_tol, &nvalues, &def, 1, 0);

  /* set flag if no rate checking to be done: */
  nochk = slew_tol<0.0;

  /* convert from units of NIMS resolution elements to radians
   * -- will be converted from per-grating-cycle in find_mode */
  slew_tol = slew_tol * 0.0005;

  zvparmd( "TWISTTOL", &twt_tol, &nvalues, &def, 1, 0);

  if (tube) do_fill = 0;
  else do_fill = zvptst( "FILL");
  if (do_fill && org>0) {
    do_fill = 0;
    zvmessage(" no fill performed when output ORG is not BSQ!","");
  }

	/* SATURATD parameter: */
  flag_sat = zvptst( "FLAG");
  repl_sat = zvptst( "BB_REPL");
  max_sat = zvptst( "MAX_REPL");

  zvparm( "SATTHRSH", &sat_thrsh, &nsatthr, &def, 1, 0);

  lo_sat = zvptst( "LO_SAT");

  oldcal = zvptst( "OLDCAL");

  cat_flag = zvptst( "CATUPDT");

  zvparm( "PROJ", project, &nvalues, &def, 1, 0);

  if      (!strcmp(project,"POLORTH"))  map_type = 1;	/* polar orthographic*/
  else if (!strcmp(project,"ORTHO"))    map_type = 2;	/*oblique orthographic*/
  else if (!strcmp(project,"POLSTER"))  map_type = 3;	/* polar stereographic*/
  else if (!strcmp(project,"STEREO"))   map_type = 4;	/* oblique stereo */
  else if (!strcmp(project,"LAMBERT"))  map_type = 5;
  else if (!strcmp(project,"MERCATOR")) map_type = 6;
  else if (!strcmp(project,"NORMCYL"))  map_type = 9;	/* normal cylindrical */
  else if (!strcmp(project,"CYLINDRI")) map_type = 10;	/* simple cylindrical */
  else if (!strcmp(project,"SINUSOID")) map_type = 12;
  else if (!strcmp(project,"POV"))      map_type = 16;	/* Perspective */
  else if (!strcmp(project,"PERSPECT")) map_type = 16;	/* Perspective */

  scale = 0.0;
  zvparmd( "SCALE", &scale, &nscal, &def, 1, 0);

	/* radius fudge factor for off-limb data: */
  radfudge = 0;
  if (map_type==2 || map_type==16)
    zvparmd( "RADFACT", &radfact, &radfudge, &def, 1, 0);

	/*
	 * set up data used by TRANV for the selected map projection
	 * (if not specified here, will be computed in set_projection)
	 */
  if (map_type==5) {					/* Lambert*/
    zvparm( "PARALLEL", buf, &nparallels, &def, 2, 0);
    if (nparallels) {
      for (i=0;i<2;i++) parallel[i] = buf[i];
      if (parallel[0] * parallel[1] < 0) zmabend(
        " *** LAMBERT PARALLELS MUST BE IN SAME HEMISPHERE ***");
      if (parallel[0] < parallel[1]) {			/*if wrong order*/
        temp = parallel[0];				/*swap them*/
        parallel[0] = parallel[1];
        parallel[1] = temp;
      }
    }
  }

	/*
	 * POV projection -- can use REFSCLK instead of special point
	 */
  if (map_type == 16) {

    zvmessage(" Output image will be in perspective (POV) projection","");

	/* copy fixed parts of PPROJ buffer: */
    t_data_pov = tgt_data;

    if (nscal) {
      sprintf( msg, " POV scale (km/pix) has been multiplied by %g", scale);
      zvmessage( msg,"");
      t_data_pov.cscale = t_data_pov.cscale / scale;	/* output proj'n only */
      /* note that "map scale" (km/pix) has *opposite* effect of cam.scale */
    }
    else scale = 1.0;
    scale0 = 1.0;		/* nominal scale */

    zvparmd( "NORTH", &north, &inorth, &def, 1, 0);

    zvparm( "OAXIS", buf, &n_oaxis, &def, 2, 0);
    if (n_oaxis == 2) {
      t_data_pov.aline = buf[0];
      t_data_pov.asamp = buf[1]; 
    }
    else {
      n_oaxis = 0;
      t_data_pov.aline = 0.5*nl;
      t_data_pov.asamp = 0.5*ns;
    }
    /* option to ignore optical axis even if not specified: */
    recenter = zvptst("RECENTER");

    zvparm( "OLATLON", buf, &n_oll, &def, 2, 0);
    if (n_oll == 2) {
      olatlon[0] = buf[0];
      if (buf[1] < 0.0) buf[1] += 360.;
      if (elon) olatlon[1] = 360 - buf[1];
      else olatlon[1] = buf[1];
    }
    else {
      n_oll = 0;
      olatlon[0] = 0.0;
      olatlon[1] = 0.0;
    }

    if (refsclk) return;	/* don't need any more if REFSCLK spec'd */

	/* ... else, check if user has specified complete viewing geometry;
	 * if not, return as POV is done */

    zvparmd( "TIELON", &longitude, &nval1, &def, 1, 0);
    if (nval1) {
      if (longitude < 0.0) longitude += 360.;
      if (elon) longitude = 360 - longitude;
    }
    zvparmd( "TIELAT", &latitude, &nval2, &def, 1, 0);
    zvparmd( "PDIST", &pdist, &nval3, &def, 1, 0);

    if (n_oll==2) {

	/* GET_OMRS adds stuff directly to structure t_data_pov */
      get_omrs( pdist, latitude, longitude, olatlon[0], olatlon[1], &map_line,
       &map_samp, &bop);
      if (!bop) zmabend(" ** specified OLATLON is back-of-planet **");

    }
    else {		/* don't need SSC line/samp if OLATLON spec'd */

      zvparmd( "TIELINE", &map_line, &nval4, &def, 1, 0);
      zvparmd( "TIESAMP", &map_samp, &nval5, &def, 1, 0);

      /* if at least one parameter is missing, cannot specify projection */
      if (!inorth || !nval1 || !nval2 || !nval3 || !nval4 || !nval5) {
	/* if at least one parameter was specified, notify the user ... */
        if (inorth || nval1 || nval2 || nval3 || nval4 || nval5)
	  zvmessage(" Partial specification of POV ignored;  OM & RS will be computed from defaults ...","");
	return;
      }

	/* load real*8 items for MOMATI: */
      oal = t_data_pov.aline;
      oas = t_data_pov.asamp;
      csc = t_data_pov.cscale;
      zmomati( oal, oas, map_line, map_samp, csc, focal, longitude,
       latitude, north, pdist, t_data_pov.om, t_data_pov.rs);
    }

    refsclk = -1;	/* flag to SET_POV not to use default REFSCLK */

    return;	/* no more params for POV */
  }

  /* projections other than POV: */

  zvparmd( "TIELON", &longitude, &nvalues, &def, 1, 0);
  if (nvalues) {
    if (longitude < 0.0) longitude += 360.;
    if (elon) longitude = 360 - longitude;
  }
  else longitude = -999.;		/* flag in case not specified */

  if (map_type == 6) {		/* Mercator has L=S=1 */
    nmaplin = nmapsam = 1;
    map_line = map_samp = 1.;
    refsclk = 0;
  }
  else {
    zvparmd( "TIELINE", &map_line, &nmaplin, &def, 1, 0);
    zvparmd( "TIESAMP", &map_samp, &nmapsam, &def, 1, 0);
  }

  /* latitude = -999.;
  if (map_type==1 || map_type==3 )
    refsclk = 0;	/* Polar: set to 90 in set_proj */
  /* else */
  zvparmd( "TIELAT", &latitude, &ntlat, &def, 1, 0);

  /* Sinusoidal & Cylindrical require Tielat=0 */
  if (map_type==10) {		/* Simple Cylindrical */
    if (ntlat) {	
      /* if user specified a tielat, move it to 0 -- assuming tieline and
       * scale were specified too */
      if (nscal && nmaplin) {
	lat = latitude;
	if (pgraphic==1 && oldver) lat = det2cen( latitude);
	map_line += latitude*eq_rad/(degrad*scale);
      }
      else
	zvmessage(" TIELAT given without TIELINE/SCALE -- ignored!","");
    }
    latitude = 0.;
    refsclk = 0;
  }
  if (map_type==9 || map_type==12) {		/* Norm. Cyl. & Sinusoidal */
    if (ntlat && fabs(latitude)<0.00001)
      zvmessage(" TIELAT specification ignored!","");
    latitude = 0.;
    refsclk = 0;
  }

	/* Ortho & Stereo can have north angle specified: */
  if (map_type==2 || map_type==4) {
    zvparm( "NORTH", &buf[0], &inorth, &def, 1, 0);
    north = buf[0];
    if (north<0.) north += 360.;
  }
  else north = -900.0;		/* undefined in these projections */
}


/*************************************************************************/
FUNCTION hist2cube()
/*
 * generate output cube data from histograms accumulated in extract_data
 * if the histogram-binning option is set
 * 
 * see output_histbin() for structure of histograms
 *
 * (this option is currently only allowed for fixed-grating modes!)
 */
{
  int det, gp0, i, ib, idn0, il, is, istat, j, maxi, nbad, nhi, nlo, nnomed,
   word;
  float dn0, wts[MAX_HBINSIZE], *dptr, *wptr;
  double maxwt, sumwts, sumlo, sumhi, totwt, totwt1, wt2;
  short sdata[N_DETS][20];
  float fdata[N_DETS][20];

  sumwts = sumlo = sumhi = 0.0;
  nlo = nhi = nnomed = 0;

  /* since the WET can subselect GPs even in fixed mode, find a GP
   * with valid data -- we assume for now that all selected GPs have
   * the same band set */
  for (i=0; i<N_DETS; i++) {
    for (j=0; j<N_GPS; j++) {
      if (wet[j][i]>=0) {
	gp0 = j;
	goto foundgp;
      }
    }
  }
  zmabend(" hist2cube unable to find valid GP");
  foundgp:
  for (is=0; is<ns; is++) {
  for (il=0; il<nl; il++) {
    for (i=0; i<N_DETS; i++) sdata[i][0] = NULL2;
    for (det=s_det; det<=e_det; det++) {
      if (skipdet[det]>0) continue;
      ib = wet[gp0][det];
      if (ib<0) continue;
      /* get location of start of histogram for this pixel: */
      wptr = wdata + (hbinsize+1)*ib*nl*ns + il*ns + is;
      dn0 = *wptr;
      idn0 = (int)dn0;
      if (!idn0) continue;	/* only invalid DNs in histogram */
      totwt = maxwt = 0.0;
      maxi = -1;
      for (i=0; i<hbinsize; i++) {
	wptr += nl*ns;
	wts[i] = *wptr;
	totwt += wts[i];
	if (wts[i]>maxwt) {
	  maxwt = wts[i];
	  maxi = i+1;
	}
      }
      sumwts += totwt;
      sumlo += wts[0];
      sumhi += wts[hbinsize-1];
      /* use the normal Footprint "thresh" parameter as cutoff: */
      if (totwt<thresh) continue;
      /* some statistics ... */
      if (maxi==1) nlo++;
      if (maxi==hbinsize) nhi++;
      /* options for evaluating histogram: */
      if (!histcrit) {			/* mode=0:  peak value */
	if (maxi==1) word = INS_LO_SAT2;
	else if (maxi==hbinsize) word = INS_HI_SAT2;
	else word = idn0 + (maxi-1)*hbinstep + hbstp2;
      }
      else if (histcrit==1) {		/* mode=1:  median value */
	wt2 = 0.5*totwt;
	if (wts[0]>wt2) word = INS_LO_SAT2;
	else if (wts[hbinsize-1]>wt2) word = INS_HI_SAT2;
	else {
	  totwt1 = 0.0;
	  for (i=0; i<hbinsize; i++) {
	    totwt1 += wts[i];
	    if (totwt1>wt2) break;
	  }
	  word = idn0 + i*hbinstep + hbstp2;
	}
      }
      if ((word>=VALID_MIN2 && word<0) || word>1023) nbad++;
      else if (word==1023) sdata[det][0] = INS_HI_SAT2;
      else sdata[det][0] = word;
    }
    /* GP=-1 flags mirror-independent data */
    znims_comp_rad( -1, 1, sdata, fdata, &istat);
    if (istat) {			/* istat=0 is ok */
      sprintf( msg, " error in NIMS_COMP_RAD, code = %d", istat);
      zmabend( msg);
    }
    for (det=s_det; det<=e_det; det++) {
      if (skipdet[det]>0) continue;
      ib = wet[gp0][det];
      if (ib<0) continue;
      /* store the final selected value: */
      if (org==0) 		/* BSQ */
        dptr = odata + is + il*ns + ib*ns*nl;
      else if (org==1) 		/* BIL */
        dptr = odata + is + ib*ns + il*ns*nb;
      else if (org==2) 		/* BIP */
        dptr = odata + ib + is*nb + il*ns*nb;
      *dptr = fdata[det][0];
    }
  }}

  /* print out statistics ... */
  sprintf( msg, " total of all histogram weights = %f", sumwts);
  zvmessage( msg,"");
  sprintf( msg, " total weights below histogram ranges = %f", sumlo);
  zvmessage( msg,"");
  sprintf( msg, " total weights above histogram ranges = %f", sumhi);
  zvmessage( msg,"");
  if (nnomed) {
    sprintf( msg, " %d pixels had median outside histogram range ", nnomed);
    zvmessage( msg,"");
  }
  if (nlo) {
    sprintf( msg, " %d pixels had peak value below histogram range ", nlo);
    zvmessage( msg,"");
  }
  if (nhi) {
    sprintf( msg, " %d pixels had peak value above histogram range ", nhi);
    zvmessage( msg,"");
  }


}


/*************************************************************************/
FUNCTION init_stuff()
/*
 * perform necessary initializations, including reading the despike file
 *
 * all SPICE stuff is done here
 */
{
  int dim_radii, i, j, istat;
  int flen[MAX_EDRS];
  char target1[16];
  float delr;
  double pos, r;
  radii_typ rad;
  int nkern;

  north = (double)NULL4;

	/*
	 * initialize SPICE
	 */
  initspice();	/* load binary CONSTANTS, SCLK, LEAPSECS files */

  /* turn on NAIF error reporting: */
  zerract("SET","REPORT");
  zerrprt("SET","ALL");

  degrad = 180./zpi();

  zvparm( "TARGET", target, &nvalues, &def, 1, 0);
  /* Calibration is a special case (and must not be confused with
   * Callisto!) */
  calib = 0;
  if (!strcmp(target,"CAL") && strcmp(target,"CALLISTO")) calib = 1;
  if (!strcmp(target,"SKY")) calib = 1;

	/* set flag for east long.: */
  if (!strncmp( target, "VENUS", 5)) elon = 1;
  else elon = 0;

  /* for Calibration data, must fake up a valid Target;  also for
   * Ring, NAIF needs Jupiter ... */
  strcpy(target1,target);
  if (calib) strcpy(target1,"JUPITER");
  if (!strcmp( target, "J_RINGS")) strcpy(target1,"JUPITER");

/*istat = get_body_ids( "GLL", target1, &ids);
replace above with: */
  istat = zpbid( target1, &j);
  if (istat!=SUCCESS) zmabend(" *** unable to find target body ***");
  ids.target = j;
  /* this code is from get_body_ids(), slightly modified: */
  if (j<10 || j>999) ids.center = j;
  else ids.center = ((j/100)*100) + 99;
  ids.sc = -77;
  ids.sol = 10;

	/* in case need C-kernels */
  ck_id = ids.sc * 1000 - PLATFORM;		/* SSI instrument id */
	/*
	 * set 'system' for SPICE calls;
	 * just use B1950 for now -- since this is purely internal
	 * to the program it should make no difference
	 */
  strcpy( esystem, "B1950");

  read_ikernel();

  zvparm( "SPKERNEL", spkernel, &u_spk, &def, 1, 0);
  if (u_spk) {
    zspklef( spkernel, &i);
    if (zfailed()) zmabend(" *** unable to load SP-kernel ***");
  }
  /* if not, then load SPK from the MIPS kernel DB -- but wait until
   * we have the loclk (in assembler_pntng) */

  zvparm( "PCONSTNT", pconstants, &u_pck, &def, 1, 0);
  if (u_pck) {
    zldpool( pconstants);
    if (zfailed()) zmabend(" *** unable to load Planet constants kernel ***");
  }

	/*
	 * if user specified CSOURCE=SPICE we need an additional kernel
	 * (load it AFTER all others, as the last is read first by spice)
	 */
  c_spice = zvptst( "SPICE");
  if (c_spice) {
    zvparm( "PCKERNEL", pckernel, &nkern, &def, 1, 0);
    if (!nkern) zmabend(" No Pfm. CKERNEL specified for CSOURCE=SPICE");
    else {
      zcklpf( pckernel, &ihpck);
      if (zfailed()) {
        sprintf( msg, " *** unable to load kernel %s ***", pckernel);
        zmabend( msg);
      }
    }
    /* Rotor CK processing isn't implemented yet, but this code acts
     * as placeholder: */
    if (dbm) {	/* 'DBM' IS ACTUALLY SET LATER ON IN GET_UPARMS */
      zvparm( "RCKERNEL", rckernel, &nkern, &def, 1, 0);
      if (!nkern) zmabend(" No Rotor CKERNEL specified for CSOURCE=SPICE");
      else {
        zcklpf( rckernel, &ihrck);
        if (zfailed()) {
          sprintf( msg, " *** unable to load kernel %s ***", rckernel);
          zmabend( msg);
        }
      }
    }
  }
  else if (!calib) zvparm( "AACSFILE", aacsfnam, &nvalues, &def, 1, 0);
  aacsf = (!c_spice && !calib);

	/*
	 * get target body radii
	 */
  zbodvar( ids.target, "RADII", &dim_radii, &rad);
  if (zfailed()) zmabend(" *** unable to get target radii ***");

	/* allow user override: */
  zvpcnt( "RADII", &i);
  if (i==3) zvparmd( "RADII", &rad, &i, &def, 3, 0);

	/* add in the atmospheric radius correction: */
  zvparm( "DELRAD", &delr, &i, &def, 1, 0);

	/* store for TRANV: */
  pol_rad = rad.polar + delr; 
  eq_rad = (rad.semi_major + rad.semi_minor) / 2;	/* avg. eq. radius*/
  eq_rad += delr; 
  frad = pol_rad / eq_rad;
	/* and for PPROJ: */
  tgt_data.e1rad = rad.semi_major + delr;
  tgt_data.e2rad = rad.semi_minor + delr;
  tgt_data.prad = pol_rad;
  frad1 = tgt_data.prad / tgt_data.e1rad;
  frad2 = tgt_data.prad / tgt_data.e2rad;

  rep2 = 1.0/(frad1*frad1);

	/* check if we need to worry about planetographic/-detic */
  if (rad.semi_major == rad.polar) {
    pgraphic = -1;
    lattyp = 0;
  }
  else {
    pgraphic = zvptst("PGRAPHIC") || zvptst("PDETIC");
    if (pgraphic==1) {
      if (rad.semi_major != rad.semi_minor) {
        zvmessage(" ** Body is not a spheroid, planetographic is undefined","");
        pgraphic = 0;
      }
    }
    lattyp = pgraphic;
  }

	/*
	 * fill in fixed part of PPROJ buffer ...
	 * what PPROJ actually uses is (focal * camera scale), in pixels,
	 * this is pretty much arbitrary since we are always looking
	 * at Line = Sample = 0  (unless map proj'n is POV, which is
	 * fixed in set_pov)
	 *
	 * NOTE: 
	 *   CAMERA SCALE = 1/RES   
	 *   RES = resolution element of detector
	 *	 = size of 1 pixel [mm] in focal plane
	 *   FOV = NIMS detector angular dimension = 0.5 mrad
	 * We have, from the geometry of the optics:
	 *   FOV = RES/FOCAL
	 * Hence:
	 *   CSCALE = 1/(FOV*FOCAL)
	 */
  tgt_data.focal = focal;
  tgt_data.cscale = 1.0 / (focal * 0.0005);

	/* instrument temperatures: */
  zvparm( "INS_TEMP", utemps, &nvalues, &def, 6, 0);

	/* initialize min/max values for radiance scaling: */
  for (i=0; i<408; i++) {
    ffmin[i] = (-1)*VALID_MIN4;
    ffmax[i] = VALID_MIN4;
  }
}


/************************************************************************/
FUNCTION nims_ps_orient( sclk, sc, sol, cenbod, pme)
/*
 * return the the spacecraft-to-planet and s/c-to-sun vectors and the
 * Euler angles for the ME matrix at given compound SCLK
 */
int sclk;
struct pntng_typ *pme;
vector cenbod, sc, sol;
{
  int i;
  double state[6], state_lt, state_lt1, etime_lt, lambda, ssb_craft[6];

  zreset1();				/* clear previous SPICE errors */

  nims_s2et( sclk, &etime);		/* convert SCLK to ephem. time */

	/* simulate ephemeris error by a time offset: */
  etime += epherr;

	/* spacecraft position relative to SSB: */
  zspkssb( ids.sc, etime, esystem, ssb_craft);

	/* target body position relative to spacecraft: */
  zspkapp( ids.target, etime, esystem, ssb_craft, "LT+S", state, &state_lt);
  if (zfailed()) zmabend(" *** unable to get target body position ***");
  for (i=0; i<3; i++) sc[i] = state[i];		/* (last 3 are velocities) */

	/* solar position relative to spacecraft: */
  zspkapp( ids.sol, etime, esystem, ssb_craft, "LT+S", state, &state_lt1);
  if (zfailed()) zmabend(" *** unable to get solar position ***");
  for (i=0; i<3; i++) sol[i] = state[i];	/* (last 3 are velocities) */

	/* central body position relative to spacecraft: */
  zspkapp( ids.center, etime, esystem, ssb_craft, "LT+S", state, &state_lt1);
  if (zfailed()) zmabend(" *** unable to get central-body position ***");
  for (i=0; i<3; i++) cenbod[i] = state[i];	/* (last 3 are velocities) */

	/* Euler angles */
  etime_lt = etime - state_lt;
  zbodeul( ids.target, etime_lt, &pme->ra, &pme->dec, &pme->twist, &lambda);
	/* (lambda is longitude of longest axis -- irrelevant) */
  if (zfailed()) zmabend(" *** unable to get ME Euler angles ***");
  if (!strcmp( esystem, "B1950")) zrotadkx( pme, J2000, B1950, pme);
}


/************************************************************************/
FUNCTION nims_s2et( sclk, pet)
/*
 * convert compound SCLK to Ephemeris time using SPICE routines
 *
 * currently we do NOT worry about Partition -- this will need to 
 * be fixed some day
 */
int sclk;
double *pet;
{
  int i, j;

	/* encode SCLK into a string for NAIF: */
  i = sclk/91;		/* RIM */
  j = sclk%91;		/* mod91 */
  sprintf( msg, "%d.%d", i, j);
  
  zscs2e( ids.sc, msg, pet);
  if (zfailed()) zvmessage(" *** error in SCS2E ***","");
}


/************************************************************************/
int FUNCTION open_edr( prnt)
/*
 * increments cur_edr and then opens the data file pointed to by cur_edr
 *
 * also sets file_lclk/hclk
 */
int prnt;
{
  static int file_number;
  int frim, i, j, lrim, nlb_pds, nnx, nx[6], rim,
   rim_record, record, total_record;
  float ltx[6];
  nims2_sclk_typ sclk1, sclk2;
  char target1[16];

  if (cur_edr<-1 || cur_edr+1 >= nedrs) return -1;
  cur_edr++;
  if (!edrs[cur_edr].opened) {
    file_number++;
    zvunit( &edrs[cur_edr].unit, "NONE", file_number, "U_NAME",
     (fname+edrs[cur_edr].foff-1), NULL);
    zvopen( edrs[cur_edr].unit, "COND", "BINARY", "OPEN_ACT", "SA",
     "IO_ACT", "SA", NULL);
    edrs[cur_edr].opened = TRUE;
  }

  /* read in the binary headers using system-dependent translator: */
  get_nims_edr_hdr_2( edrs[cur_edr].unit, &edrhdr);

  /* store total # of records: */
  vstat = zvget( edrs[cur_edr].unit, "NL", &nrec, NULL);

  /* only get the EDR label stuff on first open: */
  if (edrs[cur_edr].first_open != TRUE) {
    edrs[cur_edr].first_open = TRUE;

	/* get limits for label: */
    if (!cur_edr || comp_sclk(minclk) > comp_sclk(edrhdr.hdr1.fsclk)) {
      minclk.rim = edrhdr.hdr1.fsclk.rim;
      minclk.mod91 = edrhdr.hdr1.fsclk.mod91;
      minclk.rti = edrhdr.hdr1.fsclk.rti;
    }
    if (!cur_edr || comp_sclk(maxclk) < comp_sclk(edrhdr.hdr1.lsclk)) {
      maxclk.rim = edrhdr.hdr1.lsclk.rim; 
      maxclk.mod91 = edrhdr.hdr1.lsclk.mod91; 
      maxclk.rti = 0;
    }

    /* and telemetry threshold values: */
    for (threshing=0, i=0; i<17; i++) {
      threshval[i] = edrhdr.hdr1.threshval[i];
      threshing += threshval[i];
    }
    /* if LO_SAT was nulled, then turn thresholding off */
    if (!lo_sat) threshing = 0;

    /* check the Target field vs. user param.: */
    strncpy( target1, edrhdr.hdr2.target, 8);
    if (strncmp(target1,target,strlen(target))) {
      zvmessage(" Warning: user Target does not match Target in ObsTab!","");
      sprintf( msg, " User Target = %s,  ObsTab Target = %s", target, target1);
      zvmessage( msg,"");
    }
  }

  sclk1 = edrhdr.hdr1.fsclk;
  sclk2 = edrhdr.hdr1.lsclk;

  /* check for invalid SCLK2, which occurs in some de-garbled files
   * obtained directly from NIMS: */
  get_nims_edr_rec_2( edrs[cur_edr].unit, nrec, &edrrec);
  if (edrrec.pfix.sclk.rim != 0) sclk2 = edrrec.pfix.sclk;

  file_lclk = comp_sclk( sclk1);
  file_hclk = comp_sclk( sclk2);
  file_hrti = edrhdr.hdr1.lsclk.rti;

   /* if the range in the file is wider than that specified by the user, 
    * we only process the latter ... */
  if (file_lclk < range_lclk) {
    sclk1.rim = loclk.rim;
    sclk1.mod91 = loclk.mod91;
  }
  if (range_hclk && file_hclk > range_hclk) {
    sclk2.rim = hiclk.rim;
    sclk2.mod91 = hiclk.mod91;
  }
  if (prnt) {
    if ((range_hclk && file_lclk > range_hclk) || file_hclk < range_lclk) {
      sprintf( msg, " no data in this range in EDR #%d", cur_edr+1);
      zvmessage( msg,"");
    }
    else {
      sprintf( msg, " processing range %d.%02d to %d.%02d ", sclk1.rim,
       sclk1.mod91, sclk2.rim,sclk2.mod91 );
      zvmessage( msg,"");
    }
  }
  return 0;
}


/************************************************************************/
FUNCTION open_outs()
/*
 * open output files, including the temporary weights file;
 * also initialize the files to NULL
 *
 * in certain cases we need to allocate a temporary floating-point array
 * to hold the entire cube dataset;  in that case, we also open the output
 * cube file here, even tho it's not used until end of processing (in
 * scale_data), to ensure there's enough disk space 
 *
 * (NOTE that the TFC created by BigMalloc is actually a scratch disk
 * file)
 *
 * The following table defines the cases where we need the Temporary
 * Float Cube -- entries are:
 *    Need TFC? -- Y(es) or N(o)
 *   / Output cube format -- H(alf) or R(eal)
 *   / S(equential access) or A(rray IO) for cube
 *
 *                  radiance/scaling:
 *   output      none       RAD,       RAD,
 *     is:       (DN)     no scal    scaled
 *
 *    Tube      N/H/S      N/R/S     Y/H/S
 * 
 *    G-cube    Y+/H/S     N/R/A     Y/H/S
 *
 *    + The reason we need a TFC for a DN G-cube is to preserve accuracy while
 *      averaging -- if OVERLAP = REPLACE or MAXIMUM was specified, we don't
 *      need it!  But these are sufficiently obscure cases not to warrant
 *      the extra code.
 */
{
  char orgs[4], cname[101], gname[101], wtfile[101], sdfile[101];
  int i, j, k, len, n, nbg, n1, wtunit, sdunit;
  short xbuf[20];
  float dn0, *wptr;

  if (org==0) strcpy( orgs, "BSQ");
  else if (org==1) strcpy( orgs, "BIL");
  else if (org==2) strcpy( orgs, "BIP");

	/* primary output:  G-cube/tube file */
  zvparm( "CUBE", cname, &nvalues, &def, 1, 0);	
  zvunit( &dunit, "NONE0", 1, "U_NAME", cname, NULL);

  if (caltyp && radscal<=0)		/* no TFC, Real cube */

    zvopen( dunit, "ADDRESS", &odata, "OPEN_ACT", "SA", "IO_ACT", "SA", 
     "OP", "WRITE", "O_FORMAT","REAL", "U_NL", nl, "U_NS", ns,
     "U_NB", nb, "U_ORG", orgs, NULL);

  else if (caltyp || !tube) {		/* TFC, Halfword cube */

    odata = (float *) malloc( nl*ns*nb*sizeof(float));
    if (odata==0)
      zmabend(" *** insufficient scratch space available for ODATA ***");
    zvopen( dunit, "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "WRITE", "O_FORMAT",
     "HALF", "U_NL", nl, "U_NS", ns, "U_NB", nb, "U_ORG", orgs, NULL);
  }
  else {			/* Tube/DN:  no TFC, Halfword cube */

    zvopen( dunit, "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "WRITE", "O_FORMAT",
     "HALF", "U_NL", nl, "U_NS", ns, "U_NB", nb, "U_ORG", orgs, NULL);
	/* initialize the file: */
    for (i=0; i<maxmpos; i++) xbuf[i] = NULL2;
    for (j=0; j<nb; j++)
      for (i=0; i<nl; i++)
	zvwrit( dunit, xbuf, NULL);
  }

	/* initialize the cube array for all cases except DN tube: */
  if (!tube || caltyp)
    for (i=0; i<nl*ns*nb; i++) *(odata + i) = NULL4;

	/* geometry cocube file */
  zvparm( "COCUBE", gname, &nvalues, &def, 1, 0);
  if (!nvalues) {		/* construct gname out of cname */
    for ( n1= -1;;) {		/* find last "." in name */
      len = strlen( &cname[n1+1]);
      n = strcspn( &cname[n1+1], ".");
      if (n>=len) break;
      n1 += n+1;
    }
    if (n1<=0) {
      if (tube) strcpy( gname, "CUBE.COT");
      else strcpy( gname, "CUBE.COC");
    }
    else {
      strncpy( gname, cname, n1);
      if (tube) strcpy( &gname[n1], ".COT");
      else strcpy( &gname[n1], ".COC");
    }
  }
  if (calib) nbg = 1;
  else if (tube) nbg = nbpg*ngps0 +6;
  else nbg = (int)NB_GEO;

  zvunit( &gunit, "NONE1", 1, "U_NAME", gname, NULL);
  zvopen( gunit, "ADDRESS", &gdata, "OPEN_ACT", "SA", "IO_ACT", "SA",
 	 "OP", "WRITE", "O_FORMAT", "REAL", "U_NL", nl, "U_NS", ns,
  	 "U_NB", nbg, "U_ORG", "BSQ", NULL);
/* FOR DEBUG WATCHPOINTS, REPLACE ABOVE WITH (if not too big!):
 *  gdata = (float *)malloc( nl*ns*nb*sizeof(float)); */
  for (i=0; i<nl*ns*nbg; i++) *(gdata + i) = NULL4;

  if (tube) return;

	/* open the weights file */
  zvparm( "WTFIL", wtfile, &nvalues, &def, 1, 0);	
  zvunit( &wtunit, "NONE2", 1, "U_NAME", wtfile, NULL);
  wtnb = nb + 3;		/* + Geometry-weight & 2 squares */
  if (histbin) wtnb = nb*(hbinsize+1) + 1;	/* NB hist's + geom.weight */
  zvopen( wtunit, "ADDRESS", &wdata, "OPEN_ACT", "SA", "IO_ACT", "SA",
   "OP","WRITE", "O_FORMAT", "REAL", "U_NL", nl, "U_NS", ns, "U_NB", wtnb, NULL);
  for (i=0; i<nl*ns*wtnb; i++) *(wdata+i) = 0.0;

  /* if histogram is fixed, then store DN0=7 at all entries: */
  if (hbinsize==MAX_HBINSIZE && hbinstep==2) {
    dn0 = (float)(1023-hbinstep*(hbinsize-2));
    for (i=0; i<ns; i++) {
      for (j=0; j<nl; j++) {
        for (k=0; k<nb; k++)  {
	  wptr = wdata + (hbinsize+1)*k*nl*ns + (j-1)*ns + i-1;
	  *wptr = dn0;			/* store dn0 */
  } } } }

	/* open the std.dev. cube if requested */
  zvparm( "SDFIL", sdfile, &sd_file, &def, 1, 0);
  if (!sd_file) return;
  zvunit( &sdunit, "NONE3", 1, "U_NAME", sdfile, NULL);
  zvopen( sdunit, "ADDRESS", &sddata, "OPEN_ACT", "SA", "IO_ACT", "SA",
   "OP", "WRITE", "O_FORMAT", "REAL", "U_NL", nl, "U_NS", ns, "U_NB", nb,
   "U_ORG", orgs, NULL);
  for (i=0; i<nl*ns*nb; i++) *(sddata+i) = 0.0;

  return;
}


/************************************************************************/
FUNCTION output_cotube( mpos, mpos1, gp, iy, nchops, offimag, offplan, dcnt,
 vcnt, vflag)
/*
 * output data for a complete mirror scan to a co-tube file
 *
 * 'mpos' contains the map-projected x/y locations of the pixel centers
 *
 * 'mpos1' contains the map-projected x/y locations of the right edges of
 *   the pixel, for the footprint case (it is ignored otherwise)
 *
 * cdata contains the sensor data, included for flag counts only
 */
struct xytype *mpos, *mpos1;
int gp, iy, nchops, *offimag, *offplan, *dcnt, *vcnt, *vflag;
{
  int det, i, igp, ix, j, mp, mp1;
  float *gptr;

  /* special case for Calibration data -- only write chop count: */
  if (calib) {
    for (mp=smpos; mp<=empos; mp++) {
      ix = mp+1;
      if (pmirror == DOWN) ix = maxmpos-mp;
      mp1 = mp;
      if (gdel && pmirror == UP) mp1 = maxmpos-1-mp;
      gptr = gdata + (iy-1)*ns + ix-1;
      if (*gptr==(float)NULL4) *gptr = nchops + mp1;
    }
    return;
  }

  for (mp=smpos; mp<=empos; mp++) {

	/* write the map projection cocube bands: */
    ix = mp+1;
    if (pmirror == DOWN) ix = maxmpos-mp;

	/* for non-fixed-grating modes, the chop count is nominally
	 * written on the DOWN scan (since this is the first in a
	 * grating cycle), so if it's UP then switch direction:
	 * (because it is possible that a scan other than the first
	 * one is the one on which the chop count gets written, due
	 * to the preceding ones being empty)
	 */
    mp1 = mp;
    if (gdel && pmirror == UP) mp1 = maxmpos-1-mp;

    /* lat/long only get written for truly on-planet pixels (Altitude=0) */
    igp = gpos[gp];
    if (ngps0==1) igp = 0;      /* special check for Fixed Long mode */ 
    gptr = gdata + (nbpg*igp*nl + iy-1)*ns + ix-1;
    if (cobuf[mp].latlon[0] == (float)BADP) {
      *gptr = NULL4;					/* latitude */
      gptr += nl*ns;
      *gptr = NULL4;					/* longitude */
    }
    else {
      if (cobuf[mp].rads[1]==0.0) *gptr = cobuf[mp].latlon[0];	/* latitude */
      gptr += nl*ns;
      if (cobuf[mp].rads[1]==0.0) {
	if (elon) *gptr = -cobuf[mp].latlon[1];			/* longitude */
	else *gptr = cobuf[mp].latlon[1];
      }
      /* force into 0-360 range */
      if (*gptr<0. && *gptr>=VALID_MIN4) *gptr += 360.;
      if (*gptr > 360.) *gptr -= 360.;
    }
    gptr += nl*ns;
    if (mpos[mp].x == (float)BADP) {
      *gptr = NULL4;					/* line */
      gptr += nl*ns;
      *gptr = NULL4;					/* sample */
      gptr += nl*ns;
      if (footprint) {
	*gptr = NULL4;					/* right-edge line */
	gptr += nl*ns;
	*gptr = NULL4;					/* right-edge sample */
	gptr += nl*ns;
      }
      if (ptub) {
	if (mpos[mp].y == (float)NULL4) {	/* unprojectable pixel */
	  for (j=0; j<6; j++) {
	    *gptr = NULL4;
	    gptr += nl*ns;
	  }
        }
	else {
	  for (j=0; j<3; j++) {
	    *gptr = cobuf[mp].euler[j];
	    gptr += nl*ns;
	  }
	  for (j=0; j<3; j++) {
	    *gptr = -(float)cobuf[mp].rs[j];	/* store INVERSE of RS-vector */
	    gptr += nl*ns;
	  }
	}
      }
    }
    else {
      *gptr = mpos[mp].y;				/* line */
      gptr += nl*ns;
      *gptr = mpos[mp].x;				/* sample */
      gptr += nl*ns;
      if (footprint) {
	*gptr = mpos1[mp].y;				/* right-edge line */
	gptr += nl*ns;
	*gptr = mpos1[mp].x;				/* right-edge sample */
	gptr += nl*ns;
      }
      if (ptub) {
        for (j=0; j<3; j++) {
	  *gptr = cobuf[mp].euler[j];
	  gptr += nl*ns;
        }
        for (j=0; j<3; j++) {
	  *gptr = -(float)cobuf[mp].rs[j];	/* store INVERSE of RS-vector */
	  gptr += nl*ns;
	}
      }
    }

	/* also count how many combs are bad, for reporting purposes 
	 * (in same order as for cube!):
	 * first off-planet/deboomed: */
    if (mpos[mp].x == (float)BADP) {
      if (mpos[mp].y == (float)BADP) (*offplan)++;
      else if (mpos[mp].y == 0.) (*dcnt)++;
    }
    else {
	/* then check if there were valid data for this comb */
      if (!vflag[mp]) *vcnt += 1;
	/* if there are, then check for off-image: */
      else {
	i = mpos[mp].x + 0.5;	/* (adequate because neg. values ignored) */
	j = mpos[mp].y + 0.5;
	if (i<1 || i>osize[1] || j<1 || j>osize[0]) *offimag += 1;
      }
    }

	/* now write bands N+1 - N+6 of cotube: (N = NBPG*NG)
	 * for NGPS>1 there is only 1 entry for ALL gp's, so don't
	 * overwrite what's already there */
    gptr = gdata + (nbpg*ngps0*nl + iy-1)*ns + ix-1;

	/* bands N+1 - N+3 are photometric angles: */
    for (i=0; i<3; i++) {
      if (cobuf[mp].geom[i] >= VALID_MIN4 && *gptr==(float)NULL4)
       *gptr = degrad*cobuf[mp].geom[i];
      gptr += nl*ns;
    }
	/* bands N+4 (slant distance) & N+5 (height): */
    for (i=0; i<2; i++) {
      if (*gptr==(float)NULL4) *gptr = cobuf[mp].rads[i];
      gptr += nl*ns;
    } 
    
	/* band N+6 is time in chops from start of observation
	 * for gp=0 (which is DOWN) */
    if (*gptr==(float)NULL4) *gptr = nchops + mp1;
  }
}


/************************************************************************/
FUNCTION output_ftpt( cdata, mp, cpos, gp, offimag, isclk)
/*
 * output one comb of calibrated data into the footprint in the output
 * image, defined by the 4 vertex positions in the CPOS array
 *
 * 'cdata' is the array of output data (DN or radiances)
 *
 * the footprint is (approximately) filled by dividing it up into a grid
 * (nominally 10x10, but extends twice as far in x-cone with tent function,
 * plus user param. can change spacing) and then calling output_near for
 * each pixel with at least 1 grid point
 */

float *cdata;
struct xytype cpos[4];
int gp, mp, *offimag;
{
  int det, i, ix, iy, j, k, maxx, maxy, minx, miny, off1;
  int *wbuf;
  float wt, xa, xb, xc, xd, ya, yb, yc, yd;
  struct xytype pos;

	/* check that none of the points coincide (the algorithm should
	 * actually work for any single pair of adjacent points coincident, 
	 * i.e., a triangle, but this is easier to check for and should not
	 * occur either): */
  for (i=0; i<3; i++) 
    for (j=i+1; j<4; j++) 
      if (cpos[i].x==cpos[j].x && cpos[i].y==cpos[j].y) 
        zmabend(" two of the footprint points coincide");

	/*
	 * first determine the box that contains all 4 corners --
	 * if this is only 1 pixel, then there's no need to set up
	 * the grid;  otherwise, need array of this size to hold the
	 * pixel weights
	 */
  minx = ns+1;
  miny = nl+1;
  maxx = 0;
  maxy = 0;
  for (i=0; i<4; i++) {
    if (cpos[i].x >= 0.0) ix = cpos[i].x + 0.5;
    else ix = cpos[i].x - 0.5;
    if (cpos[i].y >= 0.0) iy = cpos[i].y + 0.5;
    else iy = cpos[i].y - 0.5;
    minx = min( minx, ix);
    miny = min( miny, iy);
    maxx = max( maxx, ix);
    maxy = max( maxy, iy);
  }
  if (maxx<1 || maxy<1 || minx>ns || miny>nl) {		/* all 4 off image */
    *offimag += 1;
    return;
  }
  if (minx==maxx && miny==maxy) {	/* entire grid in 1 pixel */
    wt = 1.0;
    output_near( cdata, mp, cpos[0], gp, wt, &i);
    return;
  }
	/* find dimensions of box & allocate memory: */
  maxx -= (minx-1);
  maxy -= (miny-1);
  wbuf = (int *)malloc( 4*maxx*maxy);
  for (i=0; i<maxx*maxy; i++) wbuf[i] = 0;
	/* 
	 * per algorithm in find_xy_corners, the 4 vertices are stored
	 * such that their sequence going clockwise from 0 is: 0, 1, 3, 2;
	 * (XCONE is the 0-2 & 1-3 direction, and these sides are twice
	 * as long as the CONE sides)
	 * find (2*fpgrid-1) points (-1 to make the weights come out nicely)
	 * on the long sides & fpgrid on the short, connect corresponding ones
	 * on opposite sides, and find the intersections:
	 *   point Ai is on side 0-1
	 *   point Bj is on side 0-2
	 *   point Ci is on side 2-3
	 *   point Dj is on side 1-3
	 * The weights are a "tent" function in XCONE direction,
	 * uniform in CONE.
	 */
  for (i=0; i<fpgrid; i++) {
    xa = cpos[0].x + (cpos[1].x-cpos[0].x)*i/(fpgrid-1);
    ya = cpos[0].y + (cpos[1].y-cpos[0].y)*i/(fpgrid-1);
    xc = cpos[2].x + (cpos[3].x-cpos[2].x)*i/(fpgrid-1);
    yc = cpos[2].y + (cpos[3].y-cpos[2].y)*i/(fpgrid-1);
    for (j=1; j<2*fpgrid; j++) {	/* lines 0 & 2*N have 0 weight */
      xb = cpos[0].x + (cpos[2].x-cpos[0].x)*j/(2*fpgrid);
      yb = cpos[0].y + (cpos[2].y-cpos[0].y)*j/(2*fpgrid);
      xd = cpos[1].x + (cpos[3].x-cpos[1].x)*j/(2*fpgrid);
      yd = cpos[1].y + (cpos[3].y-cpos[1].y)*j/(2*fpgrid);

	/* intersection of AC with BD gives point (i,j): */
      pos.x = ( (yd*xb-yb*xd)*(xa-xc) - (ya*xc-yc*xa)*(xd-xb) ) /
	      (       (yd-yb)*(xa-xc) -       (ya-yc)*(xd-xb) );
	/* analysis shows that above denominator cannot be = 0 --
	 * so don't check, just let pathological case crash;
	 * however, next denominator can be 0: */
      if (fabs(xc-xa)<1.e-10 && fabs(xd-xb)<1.e-10) {
	i = isclk/91;
	j = isclk%91;
	sprintf(msg, " error 1 in output_ftpt, SCLK=%d.%d", i, j);
	zmabend(msg);
      }
      if (fabs(xc-xa)<fabs(xd-xb)) pos.y = yb + (yd-yb)*(pos.x-xb)/(xd-xb);
      else pos.y = ya + (yc-ya)*(pos.x-xa)/(xc-xa);
      ix = pos.x + 0.5;
      iy = pos.y + 0.5;
      k = maxx*(iy-miny) + ix-minx;
      if (k>4*maxx*maxy) {
	i = isclk/91;
	j = isclk%91;
	sprintf(msg, " error 2 in output_ftpt, SCLK=%d.%d", i, j);
	zmabend(msg);
      }
      if (j<=fpgrid) wbuf[k] += j;
      else wbuf[k] += 2*fpgrid-j;
    }
  }

  off1 = 0;
  for (ix = minx; ix < minx+maxx; ix++) {
    for (iy = miny; iy < miny+maxy; iy++) {
      pos.x = ix;
      pos.y = iy;
      k = maxx*(iy-miny) + ix-minx;
      if (ix<1 || ix>ns || iy<1 || iy>nl) off1 += wbuf[k];
      else if (wbuf[k] > 0) {
	wt = (float)wbuf[k]/(nfpwts);
	output_near( cdata, mp, pos, gp, wt, &i);
      }
    }
  }
  free( wbuf);		/* to avoid memory overflow */

	/* label points as off-image on a statistical basis: */
  if (off1 > nfpwts/2) *offimag += 1;
}


/************************************************************************/
FUNCTION output_histbin( pdata, ix, iy, lambda, wtp)
/*
 * add one pixel to the histogram for the current (X,Y,Lambda) position
 *
 * the weights file has, for each X,Y location, NB histograms plus one
 * geometry weight (used for cocube calculations), stored in band order
 *
 * each histogram consists of one real*4 word defining the starting
 * DN value (DN0), and hbinsize (HBS) words containing the accumulated 
 * weights for the following values:

 *      Bin #    DN range
 *      1        <DN0 
 *      2        DN0 -> (DN0+dDN)
 *      ...
 *      i        (DN0+(i-1)*dDN) -> (DN0+i*dDN) 
 *      ...
 *      HBS-1    (DN0+(HBS-2)*dDN) -> (DN0+(HBS-1)*dDN) 
 *      HBS      >= (DN0+(HBS-1)*dDN)
 *
 * where "->" means: "up to but not including"
 * and dDN = hbinstep.
 *
 * the first pixel in a histogram is placed in the center (bin HBS/2),
 * unless this would make DN0 less than DN_MIN, in which case DN0
 * is set to DN_MIN;  if a subsequent pixel would fall outside the
 * histogram range, then we try to adjust this (by varying DN0) to fit 
 * it in, if possible;  once both "overflow" bins (1 and HBS) are
 * non-zero, no further adjustment is possible
 *
 * the relationship between bin # (i) and pixel value (IDN) is:
 *      IDN = DN0+(i-1)*dDN + dDN/2
 *
 * When HBS=510 (max. allowed) and dDN=2, then DN0 is fixed at 7,
 * since the histogram then covers essentially the entire allowed
 * range 7-1022.  (Strictly speaking, 6 is also allowed, but in
 * practice this has little effect.)
 */
int ix, iy, lambda;
float *pdata, *wtp;	/* passed by reference to make visible in debugger */
			/* (strange Alpha debugger bug?) */
{
  int det, i, i0, idn, idn0, inom, wband;
  float dn0, fword, word, wt, wtout, *wptr, *wptr0, *wptro, *wptrn;

  /* start location of this histogram: */
  wptr = wdata + (hbinsize+1)*lambda*nl*ns + (iy-1)*ns + ix-1;

  /* put NULL and SAT DNs into overflow bin (and don't change DN0),
     in order not to bias the histogram;  other invalid DNs are simply 
     ignored;  don't need to worry about deboomed pixels as they are 
     filtered out before this step */
  if (*pdata < (float)DN_MIN) {
    if (*pdata <= (float)INS_LO_SAT2) {
      wptr += nl*ns;			/* point to bin 1 */
      *wptr += *wtp;
    }
    if (*pdata == (float)INS_HI_SAT2) {
      wptr += hbinsize*nl*ns;			/* point to bin HBS */
      *wptr += *wtp;
    }
    return;
  }
  idn = (int)*pdata;		/* convert DN back to integer */

  dn0 = *wptr;
  idn0 = (int)dn0;

  /* if it's zero, then this is a new histogram ... */
  if (dn0 == 0.0) {
    i0 = idn - (hbinsize/2-1)*hbinstep;	/* DN0 to put IDN in center bin */
    if (i0<DN_MIN) i0 = DN_MIN;
    dn0 = (float)i0;
    *wptr = dn0;			/* store dn0 */
    i = 1+(idn-i0-hbstp2)/hbinstep;
    wptr += i*nl*ns;
    *wptr = *wtp;			/* store weight */
    return;
  }

  /* adding a weight to an existing histogram ... */
  inom = 1+(idn-idn0-hbstp2)/hbinstep;	/* nominal bin # */
  if (inom>1 && inom<hbinsize) {	/* fits in current hist. */
    wptr += inom*nl*ns;
    *wptr += *wtp;			/* store weight */
    return;
  }
  /* it doesn't fit ... check the overflow bins;  we can't adjust
   * it if the one on the side of the current point is non-zero */
  wptr0 = wptr+nl*ns;
  if (inom<=1 && *wptr0>0.0) {
    *wptr0 += *wtp;
    return;
  }
  wptrn = wptr+hbinsize*nl*ns;
  if (inom>=hbinsize && *wptrn>0.0) {
    *wptrn += *wtp;
    return;
  }
  /* the histogram is still open ... see if we can adjust DN0 to
   * fit the current point, by moving all points up in the opposite
   * direction;  the limit is when the total weight that would be
   * put into the overflow bin at that edge exceeds the current weight,
   * or all points have been moved off the histogram;  whatever shift
   * is calculated is applied even if it doesn't succeed in placing
   * current point in the histogram, as presumably it makes for better
   * balance */
  i0 = inom;		/* adjusted bin # for current point */
  if (inom<=1) {	/* point is at low edge of histogram */
    wptro = wptrn;
    wtout = *wptro;	/* total overflow weight so far */
    while (wtout<*wtp && i0<=1 && wptro>wptr0 && idn0>DN_MIN) {
      i0++;			/* move bins up one */
      idn0--;
      wptro -= nl*ns;		/* last bin overflows */
      wtout += *wptro;
    }
    /* can't move it any further -- if it moved at all, shift all
     * weights up and revise DN0 */
    if (i0>inom) {
      dn0 = (float)idn0;
      *wptr = dn0;
      *wptrn = wtout;
      for (i=hbinsize-1-i0+inom; i>1; i--) {
	wt = *(wptr+i*nl*ns);
	*(wptr+(i+i0-inom)*nl*ns) = wt;
      }
      for (i=2; i<3+i0-inom; i++) {
	*(wptr+i*nl*ns) = 0.0;
      }
    }
    /* and add in current point */
    if (i0<=1) *wptr0 += *wtp;
    else *(wptr0+i0*nl*ns) += *wtp;
    return;
  }
  /* else point is at high end of histogram ... */
  wptro = wptr0;
  wtout = *wptro;	/* total overflow weight so far */
  while (wtout<*wtp && i0>=hbinsize && wptro<wptrn && idn0<1023) {
    i0--;			/* move bins down one */
    wptro += nl*ns;		/* last bin overflows */
    wtout += *wptro;
  }
  /* can't move it any further -- if it moved at all, shift all
   * weights down */
  if (i0<inom) {
    dn0 = (float)idn0;
    *wptr = dn0;
    *wptrn = wtout;
    for (i=2; i<=hbinsize-1+i0-inom; i++) {
      wt = *(wptr+(i+inom-i0)*nl*ns);
      *(wptr+i*nl*ns) = wt;
    }
    for (i=hbinsize-inom+i0; i<hbinsize-1; i++) {
      *(wptr+i*nl*ns) = 0.0;
    }
  }
  /* and add in current point */
  if (i0>=hbinsize) *wptrn += *wtp;
  else *(wptr0+i0*nl*ns) += *wtp;
  return;
}


/************************************************************************/
FUNCTION output_near( cdata, mp, pos, gp, wtx, offimag)
/*
 * output one comb of data and associated cocube data, using the
 * nearest-neighbour approximation
 *
 * 'cdata' is an array of N_DET sensor data values, 'pos' contains
 * the x/y locations;  cocube data are extracted from the geometry
 * buffer
 *
 * mean values are computed, weighted by WTX (weights file contains
 * total weights of inputs contributing to this output)
 *
 * mean cocube quantities are averaged over all grating positions
 * contributing to this pixel
 */
float *cdata;
struct xytype pos;
int gp, mp, *offimag;
float wtx;
{
  int det, i, ix, iy, lambda, offs, wband;
  float dnstd, dnstdm, *dptr, fword, geomn, geostd, *gptr, phasem, scal,
   *sptr, word, wt, wtg, wtsd, wt0, *wptr, *wptrg, *wptrp;

  ix = pos.x + 0.5;		/* "C" truncates in int-to-float conversion */
  iy = pos.y + 0.5;		/* (wrong when x/y<0, but doesn't matter) */
  if (ix<1 || ix>ns || iy<1 || iy>nl) {
    *offimag += 1;
    return;
  }

  if (histbin) {
    for (det=s_det; det<=e_det; det++) {
      if (skipdet[det]>0) continue;
      lambda = wet[gp][det];
      if (lambda<0) continue;
      output_histbin( &cdata[det], ix, iy, lambda, &wtx);
    }
    goto cocbands;
  }

  dnstd = NULL4;		/* initialize */

  for (det=s_det; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    lambda = wet[gp][det];
    if (lambda<0) continue;

	/* weights file has a band for each cube band */
    wptr = wdata + lambda*nl*ns + (iy-1)*ns + ix-1;
    wt = *wptr;
    wt0 = wt;		/* for std.dev's */
    if ((lambda+1)==lamsd) wtsd = wt;

    if (org==0) 		/* BSQ */
      offs = ix-1 + (iy-1)*ns + lambda*ns*nl;
    else if (org==1) 		/* BIL */
      offs = ix-1 + lambda*ns + (iy-1)*ns*nb;
    else if (org==2) 		/* BIP */
      offs = lambda + (ix-1)*nb + (iy-1)*ns*nb;
      				/* no more */
    dptr = odata + offs;

    word = *dptr;			/* get whatever is already there */

 	/* NOTE:  the treatment of HI_SAT pixels here is not very
	 * satisfactory for the case where many input pixels contribute
	 * to an output pixel, since the result will be unduly
	 * influenced by the first few pixels encounterd;  should keep
	 * track of total counts of saturated and unsaturated weights,
	 * but this will greatly complicate the bookkeeping ... an
	 * alternative is to use histogram-binning when this is a
	 * problem */

	/* can't average anything with saturated pixels, but check
	 * weights: */
    if (word==(float)INS_HI_SAT4) {
      if (cdata[det]==(float)INS_HI_SAT4) {
	wt += wtx;
	*wptr = wt;
	continue;
      }
      else if (wt<sat_thrsh && wtx>wt) {
	word = cdata[det];
	*wptr = wtx;
      }
      else continue;
    }

	/* saturated cdata also beats all -- but only if its weight
	 * is high enough: */
    else if (cdata[det]==(float)INS_HI_SAT4 && wtx>sat_thrsh && flag_sat) {
      word = cdata[det];
      *wptr = wtx;
    }

	/* if thresholding is done, then if either DN is LO_SAT we must
	 * treat this specially ...
	 * (Note this code does not check 'use_last' (REPLACE option) --
	 * it is very unlikely that this will ever be used with thresholded
	 * data!) */
    else if (word==(float)INS_LO_SAT4 || cdata[det]==(float)INS_LO_SAT4) {
	/* if both are LO_SAT, nothing changes: */
      if (word==(float)INS_LO_SAT4 && cdata[det]==(float)INS_LO_SAT4) continue;
	/* if either is NULL or below wts-thresh, it is skipped: */
      else if (cdata[det]==(float)NULL4 || wtx<thresh) continue;
      else if (word==(float)NULL4 || wt<thresh) {
	word = cdata[det];
	*wptr = wtx;
      }
	/* here if both are above wts-thresh and only one is LO_SAT --
	 * average the latter in as zero radiance: */
      else {
	if (word==(float)INS_LO_SAT4) {
	  if (caltyp || det<14) word = wtx*cdata[det] / (wtx+wt); 
	  else {
	    word = 0.0;
	    average_dn( &word, cdata[det], wt, wtx, det);
	  }
	}
	else {
	  if (caltyp || det<14) word = wt*word / (wtx+wt); 
	  else {
	    cdata[det] = 0.0;
	    average_dn( &word, cdata[det], wt, wtx, det);
	  }
	}
	wt += wtx;
	*wptr = wt;
      }
    }

	/* if cdata is some other bad value, just keep stored value */
    else if (cdata[det] < VALID_MIN4) continue;

    /* the only other value < VALID_MIN should be NULL:  if this is stored, 
     * then nothing has been seen yet, in which case we store new DN whatever 
     * it is */
    else if (word<VALID_MIN4) {
      word = cdata[det];
      *wptr = wtx;
    }

    else {		/* here if both stored and new values are ok */
      if (use_max) word = max( word, cdata[det]);   /* if MAXIMUM requested */
      else if (use_last) word = cdata[det];	/* if REPLACE requested */
      else {					/* average */
	if (caltyp || det<14) word = (wtx*cdata[det]+wt*word) / (wtx+wt); 
        else average_dn( &word, cdata[det], wt, wtx, det);
	wt += wtx;
	*wptr = wt;
      }
    }

    *dptr = word;

	/* for radiance scaling factors: */
    if (radscal==1 && word >= VALID_MIN4) {
      ffmin[lambda] = min( ffmin[lambda], word);
      ffmax[lambda] = max( ffmax[lambda], word);
    }

	/* lamsd starts at 1, lambda at 0 */
    if ((lambda+1)==lamsd && cdata[det] >= VALID_MIN4) { /* save for below */
      dnstd = cdata[det];
      dnstdm = word;
    }

	/* now the std.dev. cube, if requested ...
	 * during main processing, we store <DN**2> here, and
	 * convert it to sqrt(variance) at the end */
    if (!sd_file || cdata[det]<VALID_MIN4) continue;
    sptr = sddata + offs;		/* has same structure as cube */
    fword = *sptr;
    fword = (wtx*cdata[det]*cdata[det] + wt0*fword) / (wt0+wtx);
    *sptr = fword;
  }

cocbands:
	/*
	 * now write bands 3-9 of cocube:
	 * (1,2 are lat/long, get written at end of job)
	 *
	 * (if we have got this far, we're on-planet and there must 
	 * be at least one det with valid data, so no need to check
	 * for VALID_MIN)
	 *
	 * bands 3-5 are photometric angles: */
  wptrg = wdata + nb*nl*ns + (iy-1)*ns + ix-1;
  if (histbin) wptrg = wdata + (wtnb-1)*nl*ns + (iy-1)*ns + ix-1;
  wtg = *wptrg;
  gptr = gdata + 2*nl*ns + (iy-1)*ns + ix-1;
  for (i=0; i<3; i++) {
    fword = *gptr;
    if (fword < VALID_MIN4 || use_last) fword = degrad*cobuf[mp].geom[i];
    else fword = (wtx*degrad*cobuf[mp].geom[i] + wtg*fword) / (wtx + wtg);
    if (fword > 180.) fword -= 360.;
    if ((i+3) == geosd) {		/* save for std.dev. calc'n */
      geostd = degrad*cobuf[mp].geom[i];
      geomn = fword;
    }
    *gptr = fword;
    gptr += nl*ns;
  }

	/* band 6 (slant distance): 
         * POV slant is written in write_latlon, unless it's old_ver */
  if (map_type!=16 || oldver) {
    fword = *gptr;
    if (fword < VALID_MIN4 || use_last) fword = cobuf[mp].rads[0];
    else fword = (wtx*cobuf[mp].rads[0] + wtg*fword) / (wtx + wtg);
    if (geosd==6) {		/* save for std.dev. calc'n */
      geostd = cobuf[mp].rads[0];
      geomn = fword;
    }
    *gptr = fword;
  }
  gptr += nl*ns;

	/* band 7 (height) -- POV & expanded-radius only: */
  if (map_type==16 && radfudge) {
    fword = *gptr;
    if (fword < VALID_MIN4 || use_last) fword = cobuf[mp].rads[1];
    else fword = (wtx*cobuf[mp].rads[1] + wtg*fword) / (wtx + wtg);
    if (geosd==7) {		/* save for std.dev. calc'n */
      geostd = cobuf[mp].rads[1];
      geomn = fword;
    }
    *gptr = fword;
  }
  gptr += nl*ns;

  if (histbin) {			/* no std.dev.s in this option */
    wtg += wtx;
    *wptrg = wtg;    /* update geom. weight */
    return;
  }

	/* bands 8,9 require reading squares from WTFIL */
  wptrp = wptrg + nl*ns;			/* location of <GEO**2> */

	/* band 8 = GEO STD.DEV. */
  if (geostd >= VALID_MIN4 && !use_last) {
    fword = *wptrp;
    fword = (wtx*geostd*geostd + wtg*fword) / (wtg+wtx);  /* new <GEO**2> */
    *wptrp = fword;	
    fword -= geomn*geomn;			/* SD = <X**2> - <X>**2 */
    fword = max( fword, 0.0);			/* in case of roundoff error*/
    fword = sqrt( fword);
    *gptr = fword;				/* new GEO STD.DEV. */
    gptr += nl*ns;
  }

	/* band 9 = DN STD.DEV. */
  if (dnstd >= VALID_MIN4 && !use_last) {
    wptrp += nl*ns;				/* location of <DN**2> */
    fword = *wptrp;
    fword = (wtx*dnstd*dnstd + wtsd*fword) / (wtsd+wtx);
    *wptrp = fword;				/* new <DN**2> */
    fword -= dnstdm*dnstdm;			/* SD = <X**2> - <X>**2 */
    fword = max( fword, 0.0);			/* in case of roundoff error*/
    fword = sqrt( fword);
    *gptr = fword;				/* new DN STD.DEV. */
  }

	/* update geom. weight */
  wtg += wtx;
  *wptrg = wtg;
}


/************************************************************************/
FUNCTION output_test()
/*
 * only called in test mode:
 *  TEST=1:  output an ascii file containing mirror track data
 *  TEST=2:  output listing of SCLK, MPOS/LAT-LON, RADIUS.
 *  TEST=3:  output listing of SCLK, OM-matrix, & RS-vector.
 *  TEST=4:  output listing of wavelengths and radiance scaling constants
 *		by band #
 *  TEST=10: same as 1, but only MP=0 
 *
 * this routine has the same basic structure as extract_data, (but
 * does not extract the data)
 *
 * to restrict output (TEST=1-3) to one gp, use OUTBAND
 */
{
  int dj, gp, i, ind, isclk, j, jsclk, hmfc, mp, rti, rval, xmp;
  float dum, xlat, xlon;
  double cone, xcone;
  struct xytype mpos[20];
  FILE *fil;
  nims2_sclk_typ sclk;

  dbm = 0;		/* no debooming for TEST */

  if (xtest==1 || xtest==10) {
    fil = fopen( "MTRACK.DAT", "w");
    i = mf[0].sclk/91;
    j = mf[0].sclk%91;
    fprintf( fil," Starting SCLK = %d.%02d\n", i, j);
    fprintf( fil,"    HMFC    MP    X     Y\n");
  }
  else if (xtest==2 || xtest==8)
   zvmessage("  SCLK    MP    LAT    LON    RADIUS","");
  else if (xtest==9)
   zvmessage("    SCLK     RA         Dec      Twist      D(CON)    D(XCON)","");

  if (fix_mode) lami = 0;

  for (i=0; i<nmf; i++) {			/* for each mf */
    isclk = mf[i].sclk;
    if (xtest==1 || xtest==10) hmfc = 2*(isclk-mf[0].sclk);
    if (xtest>=2) jsclk = 100*(isclk/91) + isclk%91;

    for (rti=0; rti<=5; rti+=5) {		/* 2 scans per mf */

      if (mf[i].igc<0) continue;
      if (xtest==1 || xtest==10) hmfc += rti/5;

      gp = (2*(isclk%91) + rti/5) % ngps;
      if (gpos[gp]<0) continue;

      if (!mirrop) mirror = STOP;
      else if (gp >= nsteps) mirror = WAIT;
      else mirror = (gp%2)==0 ? DOWN : UP;

      smpos = smpos0;
      empos = empos0;
      if (mirror == UP) {
	smpos = 19-empos0;
	empos = 19-smpos0;
      }
      if (xtest==10) smpos = empos = 0;

      if (xtest==1 || xtest==10) {

	find_xy_center( isclk, rti, mpos, 0,0,0,0);

	for (mp=smpos; mp<=empos; mp++) 
	  if (mpos[mp].x >= 0.) fprintf( fil," %10d  %2d %7.3f %7.3f\n", 
	   hmfc, mp, mpos[mp].x, mpos[mp].y);
      }
      else {
	dj = 1;
	if (xtest==3) dj = 19;
	if (xtest==8) {
	  if (rti) continue;
	  smpos = empos = 9;
	}
	for (j=smpos; j<=empos; j+=dj) {
	  get_tgt_data( isclk, rti, j, 0, -1, &cone, &xcone);
	  dum = 0.0;
	  zpproj( &tgt_data, &dum, &dum, &xlat, &xlon, LS_LL, lattyp, &rtang,
	   &slant, &ind);
	  xmp = j+maxmpos*(rti/5);
	  xlon = 360.-xlon;		/* convert to W.Long */
	  if (xtest==2 || xtest==8) {
	    sprintf( msg, " %8d  %2d  %8.4f  %8.4f  %7.2f", jsclk,
	     xmp, xlat, xlon, rtang);
	    zvmessage( msg,"");
	  }
	  else if (xtest==3) {
	    sprintf( msg, " SCLK=%d, mp=%d: ", jsclk, xmp);
	    zvmessage( msg,"");
	    zvmessage( " OM-Matrix:","");
	    for (i=0; i<3; i++) {
	      sprintf( msg, "  %f %f %f", tgt_data.om[i][0], 
	       tgt_data.om[i][1], tgt_data.om[i][2]);
	      zvmessage( msg,"");
	    }
	    sprintf( msg, "  %f %f %f", tgt_data.rs[0], 
	     tgt_data.rs[1], tgt_data.rs[2]);
	    zvmessage( msg,"");
	  }
	}
      }
    }
  }
  if (xtest==1 || xtest==10) fclose( fil);
}


/************************************************************************/
FUNCTION output_tube_h( cdata, gp, iy, vflag)
/*
 * output data for a complete mirror cycle to a halfword Tube file
 *
 * since this routine is only called for a DN cube, no need to do
 * photometric function correction
 *
 * 'cdata' contains the sensor data
 */
short cdata[][20];
int gp, iy, *vflag;
{
  short hdata[20];
  int det, ix, j, lambda, mp;

  for (det=s_det; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    lambda = wet[gp][det];
    if (lambda<0) continue;

    for (mp=smpos; mp<=empos; mp++) {
      ix = mp;
      if (pmirror == DOWN) ix = maxmpos-mp-1;
      hdata[ix] = cdata[det][mp];
    }
    zvwrit( dunit, hdata, "LINE", iy, "BAND", lambda+1, NULL);
  }

	/* check if there were some valid data, 1 comb at a time,
	 * for output_cotube: */
  for (mp=smpos; mp<=empos; mp++) {
    for (det=s_det, j=0; det<=e_det && j<1; det++) {
      if (wet[gp][det]<0) continue;
      if (cdata[det][mp] >= VALID_MIN2) j = 1;
    }
    vflag[mp] = j;
  }
}


/************************************************************************/
FUNCTION output_tube_f( cdata, gp, iy, vflag)
/*
 * output data for a complete mirror cycle to a floating-point Tube 
 * (which may be a file or an internal array -- see open_outs)
 *
 * also apply the photometric function correction, if requested
 *
 * 'cdata' contains the sensor data
 */
float cdata[][20];
int gp, iy, *vflag;
{
  int det, ix, j, lambda, mp;
  float *dptr;

  for (det=s_det; det<=e_det; det++) {
    if (skipdet[det]>0) continue;
    lambda = wet[gp][det];
    if (lambda<0) continue;

    for (mp=smpos; mp<=empos; mp++) {
      ix = mp+1;
      if (pmirror == DOWN) ix = maxmpos-mp;
      dptr = odata + (lambda*nl + iy-1)*ns + ix-1;
      if (pfunc>0 && lambda<=npfcut && cobuf[mp].photcor > 0. &&
          cdata[det][mp] >= VALID_MIN4) 
        cdata[det][mp] = cdata[det][mp] / cobuf[mp].photcor;
      *dptr = cdata[det][mp];

	/* for radiance scaling factors: */
      if (radscal==1 && *dptr >= VALID_MIN4) {
        ffmin[lambda] = min( ffmin[lambda], *dptr);
        ffmax[lambda] = max( ffmax[lambda], *dptr);
      }
    }
  }

	/* check if there were some valid data, 1 comb at a time,
	 * for output_cotube: */
  for (mp=smpos; mp<=empos; mp++) {
    for (det=s_det, j=0; det<=e_det && j<1; det++) {
      if (wet[gp][det]<0) continue; 
      if (cdata[det][mp] >= VALID_MIN4) j = 1;
    }
    vflag[mp] = j;
  }
}


/*************************************************************************/
FUNCTION read_dspk(dsfil1)
/*
 * read a despike file;  its structure is:
 * RIM MF RTI MP DET OLDDN NEWDN SC RC (where SC & RC are codes that are
 * ignored)
 */
char *dsfil1;
{
  int eol, i, ios, isp, j, llen, lno, maxlen, nhead;
  float s1, s2;
  FILE *fil;
  char buf[81];
  struct despike_type pdspk;

  /* DEBUG: */
  int x1,x2,x3,x4,x5,x6,x7;

  fil = fopen( dsfil1, "r");
  if (fil == NULL) zmabend(" ** error opening DESPIKE file **");

  /* because of problems encountered reading files created by Fortran i/o,
   * we read the lines in 1 character at a time, taking all non-printing
   * characters as terminators 
   */

  nspik = 0;
  maxlen = 80;
  nhead = -1;		/* # of header records */
  ios = 0;		/* not EOF (?) */
  lno = 0;		/* line number counter */
  llen = 0;		/* line length counter */
  eol = 1;		/* end-of-line flag */
  isp = 0;		/* spike count */
  while (ios != EOF) {
    ios = fgetc(fil);	/* read one character */
    if (ios < 32) {		/* non-printing character */
      if (eol) continue;
      eol = 1;			/* just ended a line */
      buf[llen] = '\0';
    }
    else {
      if (eol) {		/* start a new line */
        eol = 0;
        llen = 0;
        lno++;
      }
      if (llen<maxlen-1) {
        buf[llen] = (char)ios;
        llen++;
      }
      continue;
    }
    /* at this point, we have just read a complete line into 'buf'
     * (note that 'lno' counter starts at 1 !) */

    /* the ROWS label line contains the # of spikes */
    if (!strncmp(buf, " ROWS =", 7)) {
      sscanf( buf," ROWS = %d ", &nspik);
      if (!nspik) {
	zvmessage(" despike file is empty!","");
        goto closfil;
      }
      else {
	/* allocate storage for the array: */
	pdespike =
	 (struct despike_type *)malloc( nspik*sizeof(struct despike_type));
	if (pdespike==0) zmabend(" insufficient memory for DESPIKE array");
      }
    }
    if (nhead<0) {
      if (!strncmp(buf,"/*   RIM MF R M",15)) {
	if (!nspik) zmabend(" *** Spike file read error ***");
	nhead = lno+1;
      }
    }
    if (nhead>0 && lno>nhead) {		/* it's a data line (or empty) */
/*    i = sscanf( buf," %d, %d, %d, %d, %d, %d, %d", &pdespike[isp].rim,
       &pdespike[isp].mod91, &pdespike[isp].rti, &pdespike[isp].mp,
       &pdespike[isp].det, &pdespike[isp].olddn, &pdespike[isp].newdn);*/
      /* DEBUG */
      i = sscanf( buf," %d, %d, %d, %d, %d, %d, %d", &x1,&x2,&x3,&x4,&x5,&x6,&x7);
      if (i==7) {
	pdespike[isp].rim = x1;
	pdespike[isp].mod91 = x2;
	pdespike[isp].rti = x3;
	pdespike[isp].mp = x4;
	pdespike[isp].det = x5;
	pdespike[isp].olddn = x6;
	pdespike[isp].newdn = x7;
	isp++;
  } } }
  if (!nspik) zvmessage(" despike file is empty!","");
  else {
    sprintf( msg, " %d data lines read in from spike file", nspik);
    zvmessage( msg,"");
  }
closfil:
  fclose( fil);
}


/*************************************************************************/
FUNCTION read_ikernel()
/* 
 * get instrument properties and mirror tables from the NIMS IKERNEL
 * (must convert all angles from mrad to rad)
 */
{
  int i, istat;
  float mdel;
  double buf[40], xconv=0.001;

  zvparm( "IKERNEL", ikernel, &i, &def, 1, 0);

	/* temporary test parm. for mirror asymmetry: */
  zvparmd( "MIRDEL", &mdel, &i, &def, 1, 0);

  zldpool( ikernel);
  if (zfailed()) zmabend(" *** unable to read IKERNEL ***");

  zrtpool( "INS-77000_BORESIGHT_XCONE_OFFSET", &i, &nims_bore_xcone, &istat);
  if (istat == 0) zmabend(" *** error 1 reading IKERNEL ***");
  nims_bore_xcone = xconv*nims_bore_xcone;

  zrtpool( "INS-77000_BORESIGHT_CONE_OFFSET", &i, &nims_bore_cone, &istat);
  if (istat == 0) zmabend(" *** error 2 reading IKERNEL ***");
  nims_bore_cone = xconv*nims_bore_cone;

  zrtpool( "INS-77000_POS_TBL_XCONE_DOWN", &i, buf, &istat);
  if (istat == 0 || i != 20) zmabend(" *** error 3 reading IKERNEL ***");
  for (i=0; i<20; i++)
    mirr_pos_table[i].xcone_down = xconv * (buf[i] - mdel);

  zrtpool( "INS-77000_POS_TBL_CONE_DOWN", &i, buf, &istat);
  if (istat == 0 || i != 20) zmabend(" *** error 4 reading IKERNEL ***");
  for (i=0; i<20; i++)
    mirr_pos_table[i].cone_down = xconv*buf[i];

  zrtpool( "INS-77000_POS_TBL_XCONE_UP", &i, buf, &istat);
  if (istat == 0 || i != 20) zmabend(" *** error 5 reading IKERNEL ***");
  for (i=0; i<20; i++) 
    mirr_pos_table[i].xcone_up = xconv * (buf[i] + mdel);

  zrtpool( "INS-77000_POS_TBL_CONE_UP", &i, buf, &istat);
  if (istat == 0 || i != 20) zmabend(" *** error 6 reading IKERNEL ***");
  for (i=0; i<20; i++)
    mirr_pos_table[i].cone_up = xconv*buf[i];

  zrtpool( "INS-77000_TIME_TABLE", &i, buf, &istat);
  if (istat == 0 || i != 40) zmabend(" *** error 7 reading IKERNEL ***");
  for (i=0; i<40; i++)
    mirr_tim_table[i] = buf[i];

	/*
	 * get_tgt_data assumes there is only one xcone_wait, and
	 * that cone_wait and (x)cone_stop are all 0 -- since this seems
	 * plausible, and I don't want to make alot of changes now,
	 * these tables will only be checked here, not copied into
	 * the main buffers
	 */

  zrtpool( "INS-77000_POS_TBL_XCONE_WAIT", &i, buf, &istat);
  if (istat == 0) zmabend(" *** error 8 reading IKERNEL ***");
  for (i=1; i<20; i++) 
    if (buf[i] != buf[0]) zmabend(" *** XCONE_WAIT table is not constant ***");
  mirr_pos_xcone_wait = xconv*buf[0];

  zrtpool( "INS-77000_POS_TBL_CONE_WAIT", &i, buf, &istat);
  if (istat == 0) zmabend(" *** error 9 reading IKERNEL ***");
  for (i=1; i<20; i++) 
    if (buf[i] != 0.0) zmabend(" *** CONE_WAIT is non-zero ***");

  zrtpool( "INS-77000_POS_TBL_XCONE_STOP", &i, buf, &istat);
  if (istat == 0) zmabend(" *** error 10 reading IKERNEL ***");
  for (i=1; i<20; i++) 
    if (buf[i] != buf[0]) zmabend(" *** XCONE_STOP table is not constant ***");
  mirr_pos_xcone_stop = xconv*buf[0];

  zrtpool( "INS-77000_POS_TBL_CONE_STOP", &i, buf, &istat);
  if (istat == 0) zmabend(" *** error reading IKERNEL ***");
  for (i=1; i<20; i++) 
    if (buf[i] != 0.0) zmabend(" *** CONE_STOP is non-zero ***");

  zrtpool( "INS-77000_FOCAL_LENGTH", &i, &focal, &istat);
  if (istat == 0) zmabend(" *** error 11 reading IKERNEL ***");
}


/************************************************************************/
FUNCTION rplan( r, rlat, rlon, ra, rb, rp)
/*
 * return the planetocentric radius at given lat,lon, for known
 * radii along principal axes
 */
  float *r, rlat, rlon, ra, rb, rp;
{
  double lat, lon, clat, cln2, clon, d1, ra2, rb2, slat, slon, sln2, slt2;

  lat = rlat/degrad;
  lon = rlon/degrad;
  clat = cos(lat);
  slat = sin(lat);
  clon = cos(lon);
  slon = sin(lon);
  d1 = rp*rp*clat*clat;
  rb2 = rb*rb;
  ra2 = ra*ra;
  cln2 = clon*clon;
  sln2 = slon*slon;
  slt2 = slat*slat;
  *r = (ra*rb*rp)/sqrt(d1*rb2*cln2+d1*ra2*sln2+ra2*rb2*slt2);

  return;
}


/************************************************************************/
FUNCTION scale_data()
/*
 * scale the floating-point cube data to halfword one band at a time,
 * and write to output cube (if it's a DN cube, then this is simply a
 * data conversion without scaling)
 *
 * NOTE:  THIS PERFORMS THE SAME FUNCTION AS RXM'S ROUTINE 'RADSCL',
 * AND MUST BE MAINTAINED IN PARALLEL WITH THAT SUBROUTINE!
 * (RADSCL IS NOT USED HERE BECAUSE IT OPERATES ON UNPROJECTED ARRAYS)
 *
 * the scaling constants are written to the label as arrays RAD_BASE & 
 * RAD_CONV, where:
 *    Radiance = BASE + CONV*DN
 * Radiance = floating-point, DN = halfword integer
 *
 */ 
{
  float *dptr, tempdn;
  int gp, ib, ix, iy, ngp, offs;
  short *obuf;

  obuf = (short *)malloc( 2*ns);

  for (ib=0; ib<nb; ib++)  {
    if (radscal==2) break;
    else if (radscal==1) {	/* dynamic band-dependent scaling */
      if (ffmin[ib] >= (-1)*VALID_MIN4 || ffmax[ib] <= VALID_MIN4 ||
	  ffmax[ib] <= ffmin[ib] ) {
	cmult[ib] = 1.0;
	cbase[ib] = 0.0;
      }
      else {
	cmult[ib] = (ffmax[ib]-ffmin[ib]) / (VALID_MAX2 - VALID_MIN2);
	cbase[ib] = ffmax[ib] - cmult[ib]*VALID_MAX2;
      }
    }
    else if (radscal==3) {	/* uniform scaling */
      cmult[ib] = cmult[0];
      cbase[ib] = cbase[0];
    }
    else if (radscal<=0) {	/* no scaling */
      cmult[ib] = 1.0;
      cbase[ib] = 0.0;
    }
  }

  for (ib=0; ib<nb; ib++) {
    dptr = odata + ib*nl*ns;			/* BSQ assumed! */
    for (iy=0; iy<nl; iy++) {
      for (ix=0; ix<ns; ix++) {

		/* replace all the ISIS special values separately: */

	if      (*dptr == (float)NULL4) obuf[ix] = NULL2;
	else if (*dptr == (float)INS_LO_SAT4) obuf[ix] = INS_LO_SAT2;
	else if (*dptr == (float)INS_HI_SAT4) obuf[ix] = INS_HI_SAT2;
/*	else if (*dptr == (float)BELOW_THRESH4) obuf[ix] = BELOW_THRESH2; */
/*	else if (*dptr == (float)NO_SENSITIVITY4) obuf[ix] = NO_SENSITIVITY2;*/

	else if (*dptr < VALID_MIN4) obuf[ix] = NULL2;
		/* (this should be a redundant check!) */

	else {

	  tempdn = (*dptr-cbase[ib]) / cmult[ib];

	  if ((tempdn-0.5) < (float)VALID_MIN2) obuf[ix] = REP_LO_SAT2;
	  else if ((tempdn+0.5) > (float)VALID_MAX2) obuf[ix] = REP_HI_SAT2;

	  else if (tempdn >= 0.0) obuf[ix] = tempdn + 0.5;
		/* be careful when rounding negative values! */
	  else obuf[ix] = tempdn - 0.5;
	}
	dptr++;
      }
      zvwrit( dunit, obuf, NULL);
    }
  }
  free( obuf);
}


/************************************************************************/
FUNCTION set_mp( isclk, imp, pmp, prti)
/*
 * generate mp, rti, mirror for input sclk & mp in range (0,39)
 * (used by set_projection & set_pov)
 */
int imp, isclk, *pmp, *prti;
{
  int gp;

  *pmp = imp;
  *prti = 0;
  if (imp >= maxmpos) {
    *pmp -= maxmpos;
    *prti = 5;
  }
  if (!mirrop) mirror = STOP;
  else {
    gp = (2*(isclk%91) + *prti/5) % ngps;
    if (gp >= nsteps) mirror = WAIT;
    else mirror = (gp%2)==0 ? DOWN : UP;
  }
}


/************************************************************************/
FUNCTION set_pov()
/*
 * find the total extent of data and compute output cube dimensions 
 * for POV projection if OUTSIZ and OAXIS have been defaulted;  in 
 * this case, the routine requires one complete pass thru the data 
 * (but only processes 1 point per mf = 5% of the data)
 */
{
  int bop, i, j, k, ind, nbop, nora, ref1, ref2, rti, xmp;
  float del_oal, del_oas, dum, fpix, lin, sam, lat, lon, scal0;
  double maxlin, minlin, maxsam, minsam, oal, oas, cone, xcone, x;

	/*
	 * first we go through some song & dance to figure out
	 * where we're going to be looking from -- REFSCLK
	 * (TBD?? -- default should be computed from central lat/long 
	 * rather than just midpoint SCLK)
	 * get_tgt_data wants refsclk to be one of the stored
	 * sclks, so look for the closest one if not spot on
	 */
  if (!refsclk) {
    refsclk = (mf[0].sclk+mf[nmf-1].sclk)/2;
    refmp = 9;				/* close to center of mirror scan */
  }
  if (refsclk == -1) {		/* user specified complete viewing geometry */
    if (radfudge) t_data_povf = t_data_pov;
    goto vdone;
  }
  else if (refsclk<mf[0].sclk || refsclk>mf[nmf-1].sclk) {
    zvmessage(" REFSCLK lies outside valid data range, reset to limit","");
    if (refsclk<mf[0].sclk) refsclk = mf[0].sclk;
    else refsclk = mf[nmf-1].sclk;
  }
  else {	/* check that refsclk has valid data (for convenience,
		 * just look at range beyond initial value) */
    for (i=0; i<nmf-1; i++) if (refsclk<=mf[i].sclk && mf[i].igc>=0) break;
    refsclk = mf[i].sclk;
  }
  if (i==nmf-1) zmabend(" *** no valid data at or after mid-SCLK ***");
  i = refsclk/91;
  j = refsclk%91;
  sprintf( msg," REFSCLK = %d.%d, MP=%d", i, j, refmp);
  zvmessage( msg,"");

	/* find OM-matrix & RS-vector for this sclk: */
  set_mp( refsclk, refmp, &xmp, &rti);	/* also sets mirror */

  nora = 1;		 	/* to compute North angle */
  get_tgt_data( refsclk, rti, xmp, nora, 0, &cone, &xcone);

	/* cannot just copy tgt_data to t_data_pov because some
	 * elements may be different: */
  zmve( 8, 12, tgt_data.om, t_data_pov.om, 1, 1);

  if (n_oll) {	/* use the RS computed above, but revise the OM */
    longitude = degrad * atan2( -t_data_pov.rs[1], t_data_pov.rs[0]);
    x = sqrt( t_data_pov.rs[0] * t_data_pov.rs[0] +
	      t_data_pov.rs[1] * t_data_pov.rs[1] +
	      t_data_pov.rs[2] * t_data_pov.rs[2] );
    latitude = degrad * asin( t_data_pov.rs[2] / x);
    get_omrs( 0., latitude, longitude, olatlon[0], olatlon[1], &map_line,
     &map_samp, &bop);
    if (!bop) zmabend(" ** specified OLATLON is back-of-planet **");
  }

  if (radfudge) t_data_povf = t_data_pov;

vdone:
	/* compute & print out the scale (km/pix), as it is not
	 * stored anywhere */
  x = sqrt( t_data_pov.rs[0] * t_data_pov.rs[0] +
	    t_data_pov.rs[1] * t_data_pov.rs[1] +
	    t_data_pov.rs[2] * t_data_pov.rs[2] );
  scal0 = 0.0005 * scale * (x - t_data_pov.e1rad);
  sprintf( msg, " Scale at sub-s/c point [km/pix] is: %g", scal0);
  zvmessage( msg,"");

  if (n_oaxis && nl && ns) goto pdone;
	/*
	 * project points from all mfs in order to determine extent
	 * of data
	 *
	 * to save time just compute one point per mf, cycling thru
	 * mirror pos. in case the scan extremes fall off planet
	 */
  maxlin = maxsam = -1.e20;
  minlin = minsam =  1.e20;
  nbop = 0;
  if (xtest==6)
    zvmessage( "   SCLK   MP    LAT     LON    LIN    SAMP","");
  for (i=1; i<nmf; i++) {
    if (mf[i].igc<0) continue;
    xmp = (3*i)%(2*maxmpos);	/* one point per 2 scans */
    set_mp( mf[i].sclk, xmp, &xmp, &rti);	/* also sets mirror */
    get_tgt_data( mf[i].sclk, rti, xmp, 0, 0, &cone, &xcone);
    dum = 0.0;
    zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang, &slant,
     &ind);
    if (ind) {
      dum = 1.;		/* flag to use BOP test */
      zpproj( &t_data_pov, &lin, &sam, &lat, &lon, LL_LS, lattyp, &dum, &slant,
       &ind);
    }
    else if (radfudge) {
	/* nominal planetocentric radius at this lat/lon: */
      rplan( &rnom, lat, lon, tgt_data.e1rad, tgt_data.e2rad,
       tgt_data.prad);
      lfact = rtang/rnom;
      if (lfact < radfact) {
	t_data_povf.prad = tgt_data.prad * lfact;
	t_data_povf.e1rad = tgt_data.e1rad * lfact;
	t_data_povf.e2rad = tgt_data.e2rad * lfact;
	dum = -1.;	/* flag to ignore BOP test */
	zpproj( &t_data_povf, &lin, &sam, &lat, &lon, LL_LS, lattyp, &dum,
	 &slant, &ind);
	if (!ind && (rtang > rnom)) ind = 1;	/* not really BOP */
      }
    }
    if (!ind) {
      nbop++;
      continue;
    }
    maxlin = max( lin, maxlin);
    minlin = min( lin, minlin);
    maxsam = max( sam, maxsam);
    minsam = min( sam, minsam);
    if (xtest==6) {
      j = mf[i].sclk / 91;
      k = mf[i].sclk % 91;
      sprintf( msg, " %d.%02d  %2d  %6.2f %6.2f %6.1f %6.1f ", j, k, xmp,
       lat, lon, lin, sam);
      zvmessage( msg,"");
    }
  }
  if (maxlin<= -1.e20) zmabend(" *** all points off planet ***");
  if (nbop) {
    sprintf( msg, " %d pixels were back of planet in set_pov", nbop);
    zvmessage( msg,"");
  }

	/* if one of NL,NS were specified, check scale and print
	 * warning if too big */
  if (nl) {
    scal0 = (maxlin-minlin+1) / nl;
    if (scal0 > 1.0) {
      sprintf( msg, " Warning: Lines do not fit; increase NL or scale by %g",
       scal0);
      zvmessage( msg,"");
    }
  }
  else {
    nl = 1.1 * (maxlin-minlin+1);	/* add 10% for error margin*/
  }

  if (ns) {
    scal0 = (maxsam-minsam+1) / ns;
    if (scal0 > 1.0) {
      sprintf( msg, " Warning: Samples do not fit; increase NS or scale by %g",
       scal0);
      zvmessage( msg,"");
    }
  }
  else {
    ns = 1.1 * (maxsam-minsam+1);	/* add 10% for error margin*/
  }
  sprintf( msg, " output image size: NL= %d, NS=%d", nl, ns);
  zvmessage( msg,"");
	/*
	 * we now recenter the image to put minlin/sam at the origin
	 * (to allow for errors in the algorithm we put it 5% away
	 * from (1,1)) 
	 */
  if (recenter) {
    del_oal = 0.05*nl - minlin;
    del_oas = 0.05*ns - minsam;
    t_data_pov.aline += del_oal;
    t_data_pov.asamp += del_oas;
    if (radfudge) {
      t_data_povf.aline = t_data_pov.aline;
      t_data_povf.asamp = t_data_pov.asamp;
    }
    sprintf( msg, " OAL,OAS set to (%7.2f, %7.2f)", t_data_pov.aline,
     t_data_pov.asamp);
    zvmessage( msg,"");
  }

pdone:
  /* if tube, nl,ns do not refer to output file */
  osize[0] = nl;
  osize[1] = ns;
}


/************************************************************************/
FUNCTION set_projection()
/*
 * find the total extent of data and compute output cube dimensions 
 * and any unspecified map projection parameters
 *
 * this routine requires one complete pass thru the data if special
 * lat/long (and parallels if Lambert) are not specified, and another
 * if any of the following are not specified: scale; output size; 
 * special line/sample
 *
 * TBD(?): an interesting approach (when attitude is unspecified) would
 * be to collect locus of all points in some pseudo-image space, find
 * the principal axes, and select a projection that would make the
 * longest axis horizontal
 */
{
  int bgap, i, ih, i0, ind, j, k, lhist[360], nbop, ngap, nl0, ns0, rti, sclk1,
   xmp;
  float clin, csam, clat, clon, del, dum, lin, sam, lat, lon, fdist0;
  double dist0=0., f, maxdist, mindist, maxlin, minlin, maxsam,
   minsam, maxlat, minlat, minlon, north1, sav, scal0, sc1, sc2, ml0, ms0,
   cone, xcone, eqrad, polrad;
  struct rdata tdata0, tdata0_f;

	/*
	 * if user specified REFSCLK, then this determines 
	 * TIELAT/LON:
	 */
  if (refsclk) {
    if (refsclk<mf[0].sclk || refsclk>mf[nmf-1].sclk)
      zmabend(" *** REFSCLK lies outside valid data range ***");
    set_mp( refsclk, refmp, &xmp, &rti);	/* also sets mirror */
    get_tgt_data( refsclk, rti, xmp, 0, 0, &cone, &xcone);
    dum = 0.0;
    zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang, &slant,
     &ind);
    if (!ind && radfudge) {
      rplan( &rnom, lat, lon, tgt_data.e1rad, tgt_data.e2rad,
       tgt_data.prad);
      if (rtang/rnom < radfact) ind = 1;
    }
    if (!ind) zmabend(" *** REFSCLK is an off-planet point ***");
    latitude = lat;
    longitude = lon;
    dist0 = slant;
  }

  clat = latitude;	/* for map_pole determination below */

  /* if north is defaulted, use 0 */
  if (!inorth) north = 0.0;

  /* we use north angle of *input* projection below, so compute 
   * this at (roughly) middle of observation, which needs C & ME,
   * so use get_tgt_data: */
  sav = north;		/* save the output North angle */
  mirror = STOP;		/* arbitrary choice */
  for (i0=nmf/2; ; i0++) if (mf[i0].igc>=0) break;
  get_tgt_data( mf[i0].sclk, 0, 0, 1, 0, &cone, &xcone);
  north1 = north;		/* this is for input proj. */
  north = sav;			/* restore output one */

	/* see if the first pass is necessary: */
  if (latitude < -100. || (longitude < -900. && map_type!=1 && map_type!=3) 
   || (map_type==5 && !nparallels)) {
	/*
	 * find extent & center of the data: get the approximate (X,Y) 
	 * coord's in pseudo-object space for a few points in each mf
	 * (don't work in Lat/Lon space because of problem of wraparound
	 * at Lon=360)
	 *
	 * to save time just compute one point per mf, cycling thru
	 * mirror pos. in case the scan extremes fall off planet
	 *
	 * the projection is almost arbitrary, so use POV with first point
	 * on planet as center (unless REFSCLK is specified)
	 * NOTE this could give a problem if it causes points near the
	 * limb to become "back-of-planet", hence the 'nbop' test
	 */
    for (i0=0; i0<nmf; i0++) {	/* find first point on planet */
      if (mf[i0].igc<0) continue;
      xmp = (3*i0)%(2*maxmpos);	/* one point per 2 scans */
      set_mp( mf[i0].sclk, xmp, &xmp, &rti);	/* also sets mirror */
      get_tgt_data( mf[i0].sclk, rti, xmp, 0, 0, &cone, &xcone);
      dum = 0.0;
      zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang,
       &slant, &ind);
      if (radfudge && !ind) {
	rplan( &rnom, lat, lon, tgt_data.e1rad, tgt_data.e2rad,
	 tgt_data.prad);
	if (rtang/rnom < radfact) ind = 1;
      }
      if (ind) break;
    }
    if (!ind) zmabend(" *** all sampled points off planet ***");
    tdata0 = tgt_data;
    tdata0_f = tgt_data;

    maxlin = maxsam = maxlat = maxdist = -1.e20;
    minlin = minsam = minlat = mindist =  1.e20;
    nbop = 0;
    for (i=i0; i<nmf; i++) {
      if (mf[i].igc<0) continue;
      xmp = (3*i)%(2*maxmpos);	/* one point per 2 scans */
      set_mp( mf[i].sclk, xmp, &xmp, &rti);	/* also sets mirror */
      get_tgt_data( mf[i].sclk, rti, xmp, 0, 0, &cone, &xcone);
      dum = 0.0;
      zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang,
       &slant, &ind);
      if (ind)
        zpproj( &tdata0, &lin, &sam, &lat, &lon, LL_LS, lattyp, &dum, &dum,
	 &ind);
      else if (radfudge) {
	/* nominal planetocentric radius at this lat/lon: */
	rplan( &rnom, lat, lon, tgt_data.e1rad, tgt_data.e2rad,
	 tgt_data.prad);
	lfact = rtang/rnom;
	if (lfact < radfact) {
	  tdata0_f.prad = tgt_data.prad * lfact;
	  tdata0_f.e1rad = tgt_data.e1rad * lfact;
	  tdata0_f.e2rad = tgt_data.e2rad * lfact;
	  dum = -1.;	/* flag to ignore BOP test */
	  zpproj( &tdata0_f, &lin, &sam, &lat, &lon, LL_LS, lattyp, &dum, &dum,
	   &ind);
	  if (!ind && (rtang > rnom)) ind = 1;	/* not really BOP */
	}
      }
      if (!ind) {
	nbop++;
	continue;
      }
      maxlat = max( lat, maxlat);
      minlat = min( lat, minlat);
      maxlin = max( lin, maxlin);
      minlin = min( lin, minlin);
      maxsam = max( sam, maxsam);
      minsam = min( sam, minsam);
      maxdist = max( slant, maxdist);
      mindist = min( slant, mindist);
    }
    if (maxlin<= -1.e20) zmabend(" *** all points off planet ***");
    if (nbop) {
      sprintf( msg, " %d pixels were back of planet in set_projection",
       nbop);
      zvmessage( msg,"");
    }

    /* if necessary, fix up min/max lat/lon for Mercator to correspond to
     * ULH corner, and find center lat/lon for other proj's avoiding the
     * longitude wraparound problem: */
    if (latitude < -100. || longitude < -900.) {
      if (map_type==6) zpproj( &tdata0, &minlin, &minsam, &minlat, &minlon,
       LS_LL, lattyp, &rtang, &slant, &ind);
      else {
	clin = (maxlin+minlin)/2.;
	csam = (maxsam+minsam)/2.;
	zpproj( &tdata0, &clin, &csam, &clat, &clon, LS_LL, lattyp, &rtang,
	 &slant, &ind);
      }
      if (!ind) zmabend(" *** unable to determine projection center ***");
    }

    if (latitude<-100. && (map_type==1 || map_type==3)) {
      /* Polar:  pick nearest pole */
      if (clat<0.) latitude = -90.;
      else latitude = 90.;
      if (longitude < -900.) longitude = 0.;
    }
    if (map_type == 5) {		/* Lambert: must specify 2 parallels */
      if (!nparallels) {
	if (clat>=0.) {
          parallel[0] = 59.17;		/* (use MAP2 defaults) */
	  parallel[1] = 35.83;
	}
	else {
	  parallel[0] = -35.83;
	  parallel[1] = -59.17;
	}
      }
    }
	/* if special lat/lon still unspecified, use center points
	 * (or ULH corner for MERCATOR) */
    if (latitude < -100.) latitude = map_type==6 ? minlat : clat;
    if (longitude < -900.) longitude = map_type==6 ? minlon : clon;

    /* if observation's north is oriented more or less vertically (10 degrees
     * is an arbitrary cutoff), find approximate min/max longitude for label &
     * catalog items:
     * (aug98:  reduced cutoff to 0.001 deg. in order to effectively disable
     * this option) */
    del = 0.001;
    if (north1<del || (north1>(180.-del) && north1<(180.+del)) ||
     north1>(360.-del)) {
      if (north1>(180.-del) && north1<(180.+del)) {	/* upside down */
        sav = maxsam;
        maxsam = minsam;
        minsam = sav;
      }
      ml0 = (minlin+maxlin)/2.;
      zpproj( &tdata0, &ml0, &maxsam, &minlat, &minlon, LS_LL, lattyp, &rtang,
       &slant, &ind);
      if (ind) mmlon[0] = minlon;
      zpproj( &tdata0, &ml0, &minsam, &minlat, &minlon, LS_LL, lattyp, &rtang,
       &slant, &ind);
      if (ind) mmlon[1] = minlon;	/* (actually max W.long) */
      sprintf( msg, " Set_projection found min/max longitudes: %.2f, %.2f",
       mmlon[0], mmlon[1]);
      zvmessage( msg,"");
    }

	/* save the mean distance for scale computation: */
    dist0 = 0.5*(mindist+maxdist);

  }		/* ** end of tie_lat/long determination ** */

  else {
	/* need dist0 for default scale if scale/size *or* THRESH defaulted: */
    if ( ( (!nscal && nl<=0) || (footprint && !nthresh) ) && !refsclk) { 
      for (i0=nmf/2; ; i0++) if (mf[i0].igc>=0) break;
      sclk1 = mf[i0].sclk;
      set_mp( sclk1, 0, &xmp, &rti);
      get_tgt_data( sclk1, rti, xmp, 0, 0, &cone, &xcone);
      dum = 0.0;
      zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang,
       &fdist0, &ind);
      dist0 = fdist0;
    }
  }

  map_pole = (clat >= 0) ? 1. : -1.;

  if (dist0 > 0.0 && (!nscal || !nthresh)) {
    scale0 = 0.0005*dist0;	/* nominal pixel scale at mean distance */
	/* since this may serve as user default, truncate it to 3
	 * signficant figures for convenience: */
    i = (int)log10(scale0);
    f = pow(10.,3-i);
    i = (int)(f*scale0+0.5);
    scale0 = (double)(i)/f;
    sprintf( msg, "Nominal scale = %f km/pixel", scale0);
    zvmessage( msg,"");
  }

  /* if both SCALE and OUTSIZ were defaulted, use nominal scale: */
  if (!nscal && nl<=0) {
    scale = scale0;
    if (scale<=0.) zmabend("*** unable to determine default scale ***");
    sprintf( msg, " Nominal scale used");
    zvmessage( msg,"");
    nscal = 1;
  }

  if (nscal>0 && nl>0 && ns>0 && nmaplin>0 && nmapsam>0) {
    /* for Cylindrical proj., compute circumference for wraparound fix
     * and store long0, mapsam0 for add_label() */
    if (map_type==9 || map_type==10) {
      circum = ztwopi()*eq_rad/scale;
      long0 = longitude;
      mapsam0 = map_samp;
    }
    sprintf( msg, " output image size: NL= %d, NS=%d", nl, ns);
    zvmessage( msg,"");
    osize[0] = nl;
    osize[1] = ns;
    return;
  }

	/*
	 * determine output image dimensions and any missing map
	 * projection parameters by computing projections assuming
	 * map_line/samp are at (0,0) if unspecified
	 */
  scal0 = (nscal>0) ? scale : 1.0;	/* in case scale was defaulted */
  ml0 = (nmaplin) ? map_line : 0.;
  ms0 = (nmapsam) ? map_samp : 0.;
  nl0 = nl;
  ns0 = ns;
  /* for Cylindrical proj., reset long/samp tiepoint to S=1 (as in subr.
   * RECTPATCH)
   * (TBD:  perhaps this and the 'circum' wraparound fix should be done for
   * all cylindrical projections, but this is not straightofrward, because
   * shape is not independent of center long. for the other projections) */
  if (map_type==9 || map_type==10) {
    long0 = longitude;		/* save original tielon */
    circum = ztwopi()*eq_rad/scal0;
    if (ms0!=1.0) {
      longitude += (ms0-1.0)*360./circum;
      if (longitude == 0.0) longitude = 360.;
      while (longitude > 360.0) longitude -= 360.;
      ms0 = 1.0;
    }
    /* initialize the histogram to determine the gaps: */
    for (i=0; i<360; i++) lhist[i] = 0;
  }
  maxlin = maxsam = -1.e20;
  minlin = minsam =  1.e20;
  if (xtest==6)
    zvmessage( "   SCLK   MP    LAT     LON    LIN    SAMP","");
  for (i=0; i<nmf; i++) {
    if (mf[i].igc<0) continue;
    xmp = (3*i)%(2*maxmpos);	/* one point per 2 scans */
    set_mp( mf[i].sclk, xmp, &xmp, &rti);	/* also sets mirror */
    eqrad = eq_rad;
    polrad = pol_rad;
    get_tgt_data( mf[i].sclk, rti, xmp, 0, 0, &cone, &xcone);
    dum = 0.0;
    zpproj( &tgt_data, &dum, &dum, &lat, &lon, LS_LL, lattyp, &rtang, &slant,
     &ind);
    if (radfudge && !ind) {
      rplan( &rnom, lat, lon, tgt_data.e1rad, tgt_data.e2rad,
       tgt_data.prad);
      if (rtang/rnom < radfact) {
	eqrad = eq_rad * rtang / rnom;
	polrad = pol_rad * rtang / rnom;
	ind = 1;
    } }
    if (!ind) continue;
    if (pgraphic==1 && (map_type!=10 || oldver)) lat = det2cen( lat);
    ztranv( &ind, map_type, LL_LS, ms0, ml0, latitude,
     parallel[0], parallel[1], longitude, scal0, map_pole, 
     &lin, &sam, &lat, &lon, polrad, eqrad, north);
    if (ind) continue;
	/* cylindrical: accumulate histogram of long's for wraparound
	 * problem -- pick 1-degree bins, arbitrarily */
    if (map_type==9 || map_type==10) {
      if (sam<0.) sam += circum;
      if (sam>circum) sam -= circum;
      ih = sam*360./circum;
      if (ih >= 360) ih = 359;
      lhist[ih]++;
    }
    if (lin>maxlin) maxlin = lin;
    if (lin<minlin) minlin = lin;
    if (sam>maxsam) maxsam = sam;
    if (sam<minsam) minsam = sam;
    if (xtest==6) {
      j = mf[i].sclk / 91;
      k = mf[i].sclk % 91;
      sprintf( msg, " %d.%02d  %2d  %6.2f %6.2f %6.1f %6.1f ", j, k, xmp,
       lat, lon, lin, sam);
      zvmessage( msg,"");
    }
  }
  /* if cylindrical spans entire planet, reset min/max_samp to start/end
   * of biggest gap (if user did not specify map_sam) */
  if (!nmapsam && (map_type==9 || map_type==10) && (maxsam-minsam)>0.9*circum) {
    find_gap( lhist, 360, &bgap, &ngap);
    if (bgap>0 && ngap>0) {
      minsam = (bgap+ngap-1)*circum/360.;
      maxsam = (bgap-1)*circum/360. + circum;
      if (minsam > circum/2.) {
	minsam -= circum;
	maxsam -= circum;
      }
    }
  }

  if (maxlin<= -1.e20) zmabend(" *** all points off planet ***");

  if (!nscal) { /* scale still unknown, NL/NS must have been specified */
    sc1 = 1.1 * (maxlin-minlin) / (double)nl;
    sc2 = 1.1 * (maxsam-minsam) / (double)ns;
    scale = max( sc1, sc2);
    sprintf( msg, "Scale is %g km/pixel", scale);
    zvmessage( msg,"");

    nl0 = 1.1 * (maxlin-minlin) / scale + 0.5;
    if (nl0<1) nl0 = 1;
    ns0 = 1.1 * (maxsam-minsam) / scale + 0.5;
    if (ns0<1) ns0 = 1;
	/* shift & rescale for map_line/samp below: */
    minsam = (minsam - 0.05*(maxsam-minsam)) / scale;
    minlin = (minlin - 0.05*(maxlin-minlin)) / scale;
    ms0 /= scale;
  }
  else if (nl==0) {	/* NL/NS defaulted, but scale specified */
    nl0 = 1.1 * (maxlin-minlin);
    ns0 = 1.1 * (maxsam-minsam);
    minsam = (minsam - 0.05*(maxsam-minsam));
    minlin = (minlin - 0.05*(maxlin-minlin));
  }

	/* shift map_line/samp from (0,0) */
  if (nmaplin==0) map_line = ml0-minlin;
  if (nmapsam==0) map_samp = ms0-minsam; 

  /* for Cylindrical proj., compute circumference for wraparound fix &
   * reset long/samp tiepoint to S=1 (as in subr. RECTPATCH) */
  if (map_type==9 || map_type==10) {
    longitude = long0;		/* restore original tielon */
    mapsam0 = map_samp;		/* save original tiesam */
    circum = ztwopi()*eq_rad/scale;
    if (map_samp!=1.0) {
      longitude += (map_samp-1.0)*360.*scale/(ztwopi()*eq_rad);
      if (longitude == 0.0) longitude = 360.;
      while (longitude > 360.0) longitude -= 360.;
      map_samp = 1.0;
    }
  }

	/* set the cube dimensions:
	 * (if nl/ns were defaulted then scale was specified) */
  if (nl==0) {
    nl = nl0;
    ns = ns0;
  }
	/* cut off surplus image, but only if user did not specify all: */
  if (!nscal && nl>nl0) nl = nl0;
  if (!nscal && ns>ns0) ns = ns0;

  sprintf( msg, " output image size: NL= %d, NS=%d", nl, ns);
  zvmessage( msg,"");

  /* if tube, nl,ns do not refer to output file */
  osize[0] = nl;
  osize[1] = ns;
}


/************************************************************************/
FUNCTION setup_cal( rim, mod91)
/*
 * read in engineering & housekeeping data for new RIM and pass them to 
 * RXM's calibration routines
 *
 * if caltyp=0, then no calibration is done and only WAVES is returned
 *
 * mod91 gives the first record in the RIM with valid data
 */
int rim, mod91;
{
  static char first = 1;
  UBYTE lrshsk[91][64], hrshsk[91][10][6], hrsbkg[91][10][4], *pbyt;
  nims2_sclk_typ sclk;
  int crti, i, iop, i1, j, ngpos, rec, rec0, rec1, rec2, rr, r1, sb, stat;
  int flen[MAX_EDRS];
  float waves0[408], sensv0[408], cbase0[408], cmult0[408];
  static int drk_nl, drk_ns;
  static float drkbuf[712];	/* leave this here for now until we figure out
			   what to do with it -- TBD */
  char *callog, *drklog, *getenv_vic();

  if (first) {

	/* find and read in the calibration file(s): */
    zvpcnt( "CALFILE", &ncalfils);
    if (ncalfils > nedrs) zmabend(" *** too many Calibration files! ***");
    if (ncalfils > 1 && ncalfils < nedrs) zmabend(
     " *** invalid number of Calibration files! ***");
    zvparm( "CALFILE", cfile, &nvalues, &def, ncalfils, 0);
    /* check for ascii cal file: */
    cal_asc = zvptst("ASC_CAL");
    if (!cal_asc) {
      for (i=0; i<strlen(cfile); i++) {
        if (!strncmp(&cfile[i],".tab",4) || !strncmp(&cfile[i],".TAB",4)) {
	  cal_asc = 1;
	  break;
      } }
      /* also check for logical name: */
      if (!cal_asc) {
        callog = getenv_vic(cfile);
        if (callog!=NULL) {
          for (i=0; i<strlen(callog); i++) {
            if (!strncmp(&callog[i],".tab",4) || !strncmp(&callog[i],".TAB",4)) {
	      cal_asc = 1;
	      break;
    } } } } }
    zvsptr( cfile, ncalfils, cfnoff, flen);

    zvparm( "PSHIFT", &pshift, &nvalues, &def, 1, 0);
    if (!nvalues) pshift = -999.;	/* use default value in Cal file */

    zvparm( "AINFL", &ainfl, &nvalues, &def, 1, 0);

    /* load the first one: */
    if (cal_asc) znims_get_cal_a( cfile, &vstat);
    else znims_get_cal( cfile, &vstat);
    if (vstat) zmabend(" *** error reading calibration file ***");

    if (caltyp || drkchk) {
	/* find and read in the dark files: */
      zvpcnt( "DARKFILE", &ndrkfils);
      if (ndrkfils > nedrs) zmabend(" *** too many Dark files! ***");
      if (ndrkfils > 1 && ndrkfils < nedrs) zmabend(
       " *** invalid number of Dark files! ***");
      zvparm( "DARKFILE", dfile, &nvalues, &def, ndrkfils, 0);
      /* check for ascii dark file: */
      drk_asc = zvptst("ASC_CAL");
      if (!drk_asc) {
        for (i=0; i<strlen(dfile); i++) {
          if (!strncmp(&dfile[i],".tab",4) || !strncmp(&dfile[i],".TAB",4)) {
	    drk_asc = 1;
	    break;
        } }
        /* also check for logical name: */
        if (!drk_asc) {
          drklog = getenv_vic(dfile);
          if (drklog!=NULL) {
            for (i=0; i<strlen(drklog); i++) {
              if (!strncmp(&drklog[i],".tab",4) || !strncmp(&drklog[i],".TAB",4)) {
	        drk_asc = 1;
	        break;
      } } } } }
      zvsptr( dfile, ndrkfils, dfnoff, flen);
      if (radscal == -1 && strcmp( dfile, "DUMMY_DARK.DAT")) dndark = 1;
      if (radscal >= 0 || dndark) {
        /* load the first one: */
	if (drk_asc) znims_get_dark_a( dfile, &drk_ns, &drk_nl, drkbuf, &vstat);
        else znims_get_dark( dfile, &drk_ns, &drk_nl, drkbuf, &vstat);
	if (vstat) zmabend(" *** error reading dark file ***");
	if (drkchk || dndark) zmve( 7, 680, &drkbuf[32], darktab, 1, 1);
      }
    }

    iop = 0;
    if (!caltyp || radscal<0) iop = 1;
    if (radscal==3) iop = 2;	/* uniform scaling requested */
    ngpos = nsteps;
    if (fix_mode) ngpos = 1;
    znims_set_rad( iop, imode, g_off, gstart, gdel, ngpos, lrshsk, hrshsk,
     hrsbkg, utemps, waves, sensv, cbase, cmult, &pshift, &ainfl,
     NULL4, INS_LO_SAT4, INS_HI_SAT4, &stat);
    /* remove bands deselected by WET: */
    if (nb<nbb) {
      i1 = 0;
      for (i=0; i<nbb; i++) {
	if (bmask[i]>0) {
	  waves[i1] = waves[i];
	  sensv[i1] = sensv[i];
	  cbase[i1] = cbase[i];
	  cmult[i1++] = cmult[i];
	}
      }
    }
    /* determine the phot.fcn. band cutoff */
    if (pfunc) {
      for (i=0; i<nb; i++) {
	if (waves[i]<pfcut) npfcut = i;
      }
    }
  }
  else {

    /* if there are multiple Cal files, load the next one: */
    if (ncalfils>1) {
      if (cal_asc) znims_get_cal_a( (cfile+cfnoff[cur_edr]-1), &vstat);
      else znims_get_cal( (cfile+cfnoff[cur_edr]-1), &vstat);
      if (vstat) zmabend(" *** error reading calibration file ***");
      if (ndrkfils>1)  {
	if (drk_asc) znims_get_dark_a( (dfile+dfnoff[cur_edr]-1), &drk_ns,
	 &drk_nl, drkbuf, &vstat);
	else znims_get_dark( (dfile+dfnoff[cur_edr]-1), &drk_ns,
	 &drk_nl, drkbuf, &vstat);
	if (vstat) zmabend(" *** error reading dark file ***");
	if (drkchk || dndark) zmve( 7, 680, &drkbuf[32], darktab, 1, 1);
      }
      /* and reset the calibration parameters -- but ignore the
	 returned wavelengths, etc. */
      iop = 0;
      if (!caltyp || radscal<0) iop = 1;
      if (radscal==3) iop = 2;	/* uniform scaling requested */
      ngpos = nsteps;
      if (fix_mode) ngpos = 1;
      znims_set_rad( iop, imode, g_off, gstart, gdel, ngpos, lrshsk, hrshsk,
       hrsbkg, utemps, waves0, sensv0, cbase0, cmult0, &pshift, &ainfl,
       NULL4, INS_LO_SAT4, INS_HI_SAT4, &stat);
    }
    znims_check_rad( lrshsk, hrshsk, hrsbkg, &vstat);	/* (this is a stub) */
    if (vstat) zmabend(" *** error in NIMS_CHECK_RAD ***");
  }
  first = 0;

  if (!caltyp || radscal<0) return;

  znims_check_dark( drkbuf, drkbuf, drk_nl, drktyp, rim, mod91, dave,
   &vstat);
  if (vstat) zmabend(" *** error in NIMS_CHECK_DARK ***");
}


/******************************************************************************/
FUNCTION solar( dist, fsol)
/*
 * returns solar flux at given distance from sun in uWatt/cm**2/mu
 * NOTE: all calculations are in "modified" cgs (wavelengths in mu), but at
 * the end convert to "mixed" per NIMS convention
 *
 * dist = distance from sun in cm
 */
float dist, *fsol;
{
  float dil, flog, fs, fsun, planck;
  /* Planck constants: */
  float c1=37.4151, c2=14387.9, c4, c5, temp=5900.;
  int i, n, npts;
  char buf[10];

  /* these arrays will be read in from SOLFILE: */
  float *tfsun;	/* emergent solar flux in 10**10 erg/cm**2/sec/mu */
  float *twav;	/* the corresponding wavelengths in mu */

  FILE *fil;

  zvparm( "SOLFILE", solfile, &nvalues, &def, 1, 0);
  fil = fopen( solfile, "r");
  if (fil == NULL) zmabend(" ** error opening SOLFILE **");

  fscanf( fil," %d%s/n", &npts, buf);		/* "N points" */

	/* allocate storage for the arrays: */
  tfsun = (float *)malloc( 4*npts);
  twav = (float *)malloc( 4*npts);

  if (tfsun==0 || twav==0)
    zmabend(" insufficient memory for SOLAR arrays");

  c1 = c1/zpi();

  /* and read arrays in & compute Planck: */
  for (i=0; i<npts; i++) {
    fscanf( fil," %f%f/n", &twav[i], &fsun);
    c4 = c2/(twav[i]*temp);
    c5 = twav[i]*twav[i]*twav[i]*twav[i]*twav[i];
    planck = c1/(c5*(exp(c4)-1.0));	/* actually 10^-10 * Planck */
    tfsun[i] = log(fsun/planck);
  }

  for (n=0; n<nb; n++) {

	/* find first i at which twav[i] > waves[n]:
	 * (note we cannot assume that waves is in monotonic order!) */
    for (i=1; i<(npts-1) && waves[n] > twav[i]; i++);
	
    flog = tfsun[i-1] + (tfsun[i]-tfsun[i-1]) * (waves[n]-twav[i-1]) /
     (twav[i]-twav[i-1]);
    dil = 6.96e10/dist;			/* 6.96e10 = solar radius in cm */

	/* PI converts from intensity to flux;
	 * constant of 1.e9 includes 1.e10 (for tabulated fsol & Planck
	 * above) and 1.e-1 to convert from modified-cgs to mixed: */
    c4 = c2/(waves[n]*temp);
    c5 = waves[n]*waves[n]*waves[n]*waves[n]*waves[n];
    planck = c1/(c5*(exp(c4)-1.0));	/* actually 10^-10 * Planck */
    fsol[n] = zpi() * exp( flog) * 1.e9 * dil * dil * planck;
  }
  fclose( fil);
}


/************************************************************************/
FUNCTION update_catalog()
/*
 * update the nimsmosaic table of the catalog 
 * (the record will normally already have been created and partially written
 * by CATNIMS2, but this is ignored here)
 */
{
/*  int cnt, i, j, nrows;*/
/*  int cstat = CAT_SUCCESS;*/
/*  float x;*/
/*  char *skid;*/
/*  cat_nimsCmmMosaic_struct_query_typ mosaicQueryStruct;*/
/*  cat_user_struct_typ userInfo;*/

  /* Catalog insists that max. longitude be in (0,360) range: */
/*  if (mmlon[1]>-400. && mmlon[1]<0.) mmlon[1] += 360.;*/
  /* (try minlon too, tho this may be reversed by next test ...) */
/*  if (mmlon[0]<0.) mmlon[0] += 360.;*/
  /* ... also that min/max lon are in correct order: */
/*  if (mmlon[0]<400. && mmlon[0]>mmlon[1]) mmlon[0] -= 360;*/

  /* catalog does not like "bad" values for these ... */
/*  if (mmlon[0]>400.) mmlon[0] = 0.;*/
/*  if (mmlon[1]<-400.) mmlon[1] = 360.;*/
/*  if (mmlat[0]>400.) mmlat[0] = -90.;*/
/*  if (mmlat[1]<-400.) mmlat[1] = 90.;*/

/*  if (!cat_flag) return;*/

/*  i = strlen( obsnam);*/
/*  if (i==0) {*/
/*    zvmessage(" OBSNAME is blank, cannot update catalog","");*/
/*    return;*/
/*  }*/
  /* initialize passwd in case user omits this: */
/*  userInfo.passwd[0] = 0;*/

/*  zcatGetUserData( userInfo.server, userInfo.db, userInfo.user,*/
/*   userInfo.passwd);	/* this routine calls ZVP on the CATxx parms! */
/*  strcpy(userInfo.progname,"nimscmm2");*/
/*  userInfo.printflag = 1;  /* print out all the results structures */

  /* Initialize descriptors for the program and login to Sybase. */
/*  cstat = catLogin(&userInfo);*/
/*  if (cstat != CAT_SUCCESS) {*/
/*    zvmessage(" *** cannot login to catalog","");*/
/*    return;*/
/*  }*/

/*  OLD DTR CODE (NO NEED FOR SYBASE EQUIVALENT?):
  cstat = get_nims_mosaic_rec( obsnam, mosnum, &rec);
  if (cstat == NOT_FOUND) {
    zvmessage(" No record in Catalog for this OBSNAME","");
    store_cat_string( rec.obs, obsnam, L_OBS);
    rec.mos_number = mosnum[0];
  }
  else if (cstat != SUCCESS) return;
*/

/*  strcpy( mosaicQueryStruct.obsid, obsnam);*/
/*  strcpy( mosaicQueryStruct.obsidext, obsext);*/
/*  strcpy( mosaicQueryStruct.mosaicnum, mosnum);*/
/*  strcpy( mosaicQueryStruct.targname, target);*/

/*  mosaicQueryStruct.sclkstrtcnt = 100*loclk.rim + loclk.mod91;*/

  /* don't worry about partition for now: */
/*  mosaicQueryStruct.sclkpartition  = 1;*/

  /* parse UTC SCET for Sybase: */
/*  sscanf( &beg_utcd[21], "%3d", &i);*/
/*  mosaicQueryStruct.startscetmilli = i;*/
/*  xconv_date( beg_utcd);*/
/*  strncpy( mosaicQueryStruct.startscet, beg_utcd, 20);*/
/*  mosaicQueryStruct.startscet[20] = '\0';*/

  /* geometry stuff: */
/*  mosaicQueryStruct.starttargctrdist = trange[0]; */
/*  mosaicQueryStruct.startctrbdydist = crange[0]; */
/*  mosaicQueryStruct.minincidang = tincid[0];*/
/*  mosaicQueryStruct.minemissang = temiss[0];*/
/*  mosaicQueryStruct.minphsang = tphase[0];*/
/*  mosaicQueryStruct.minlat = mmlat[0];*/
/*  mosaicQueryStruct.minlon = mmlon[0];*/
/*  mosaicQueryStruct.startsubsclat = ssclat[0];*/
/*  mosaicQueryStruct.startsubsclon = ssclon[0];*/

  /* now same items for end of mosaic: */

/*  mosaicQueryStruct.sclkstopcnt = 100*hiclk.rim + hiclk.mod91;*/

/*  sscanf( &end_utcd[21], "%3d", &i);*/
/*  mosaicQueryStruct.stopscetmilli = i;*/
/*  xconv_date( end_utcd);*/
/*  strncpy( mosaicQueryStruct.stopscet, end_utcd, 20);*/
/*  mosaicQueryStruct.stopscet[20] = '\0';*/

/*  mosaicQueryStruct.stoptargctrdist = trange[1];*/
/*  mosaicQueryStruct.stopctrbdydist = crange[1];*/
/*  mosaicQueryStruct.maxincidang = tincid[1];*/
/*  mosaicQueryStruct.maxemissang = temiss[1];*/
/*  mosaicQueryStruct.maxphsang = tphase[1];*/
/*  mosaicQueryStruct.maxlat = mmlat[1];*/
/*  mosaicQueryStruct.maxlon = mmlon[1];*/
/*  mosaicQueryStruct.stopsubsclat = ssclat[1];*/
/*  mosaicQueryStruct.stopsubsclon = ssclon[1];*/

/*  strcpy(mosaicQueryStruct.note ," ");*/

/* OLD DTR CODE FOR KERNELS -- not replaced for Sybase since full Kernel 
 * names are stored in Processing domain -- */

/* 
  store_cat_string( rec.spice_s_id, &spice_ids[0], LEN_SPICE_ID); 
  store_cat_string( rec.spice_p_id, &spice_ids[4], LEN_SPICE_ID); 
 */
	/* C-kernel is special:  if we use one, it must be the
	 * MIPS_NIMS_CK: */
/*
  if (c_spice) store_cat_string( rec.spice_c_id, "MNCK", LEN_SPICE_ID);
  else store_cat_string( rec.spice_c_id, "AACS", LEN_SPICE_ID);
 */
	/* and so is I-kernel, as we made up our own: */
/*
  store_cat_string( rec.spice_i_id, "NIMS", LEN_SPICE_ID);
 */

/*  cstat = catNimsCmmMosaic( userInfo.printflag, &nrows,&mosaicQueryStruct);
/*  if (cstat == CAT_SUCCESS) {
/*    message2("cstat = %d, nrows = %d",cstat,nrows);
/*  }
/*  else {
/*    message2("error returned: cstat %d:\n%s",cstat,zcatGetMsg(cstat));
/*  }

/*  catLogout();*/
}



/************************************************************************/
FUNCTION varc( lat, lon, vab)
/*
 * returns the vector VAB from point P0 at lat[0],lon[0] to P1 at lat[1],lon[1],
 * on an ellipsoidal body 
 *
 * West longitudes assumed;  input LAT and LON values are assumed to be in
 * degrees
 */
float lat[2], lon[2];
vector vab;
{
  int i;
  double lam, phi, slam, clam, sphi, cphi, r;
  vector rr1[2];

  for (i=0; i<2; i++) {
    lam = lat[i]/degrad;
    if (pgraphic==1) lam = atan( tan(lam)/ rep2 );
    phi = lon[i]/degrad;
    slam = sin(lam);
    clam = cos(lam);
    sphi = sin(phi);
    cphi = cos(phi);
    r = eq_rad/sqrt(clam*clam+rep2*slam*slam);
    rr1[i][0] = r*clam*cphi;
    rr1[i][1] = -r*clam*sphi;
    rr1[i][2] = r*slam;
  }
  for (i=0; i<3; i++) vab[i] = rr1[1][i] - rr1[0][i];
}


/************************************************************************/
FUNCTION vrdcomp( ra, dec, dv, dra, ddec)
/*
 * given a unit vector VEC defined by the Euler angles RA and DEC, and
 * a small tangential addition to this vector, DV defined by its (X,Y,Z)
 * components, return the equivalent Delta-RA and Delta-DEC components
 *
 * VEC = (cosRA*cosDEC, sinRA*cosDEC, sinDEC)
 * 
 * so DV = dVEC/dRA * DRA + dVEC/dDEC * DDEC
 *
 * differentiating the equation for VEC and equating the components to
 * those of DV gives ...
 */
vector dv;
double ra, dec, *dra, *ddec;
{
  int i;
  double d0,d1,d2, mag;

  /* check validity of "small DV" assumption ... */
  mag = 0.;
  for (i=0; i<3; i++) mag += dv[i]*dv[i];
  mag = sqrt(mag);
  if (mag>0.01) {
    sprintf( msg, " warning:  small-angle assumption violated, |DV| = %f", mag);
    zvmessage( msg,"");
  }

  if (cos(dec)==0.0)
    *dra = 0.0;
  else
    *dra = (-dv[0]*sin(ra) + dv[1]*cos(ra)) / cos(dec);

  d0 = (dv[0]>0.) ? dv[0] : -dv[0];
  d1 = (dv[1]>0.) ? dv[1] : -dv[1];
  d2 = (dv[2]>0.) ? dv[2] : -dv[2];
  if (dec==0.0)
    *ddec = dv[2];
  else if (d2<d0 || d2<d1 || cos(dec)==0.0)
    *ddec = (-dv[0]*cos(ra) - dv[1]*sin(ra)) / sin(dec);
  else
    *ddec = dv[2]/cos(dec);
}


/************************************************************************/
FUNCTION write_latlon()
/*
 * write lat/long values to first 2 planes of geometry cocube;
 * for POV projection, also write the slant distance for the projection
 * (band 6 of cocube)
 *
 * also write 0's to the corresponding pixels in plane 7 (height), which
 * helps to mark on-planet pixels
 *
 * also search backplanes for min/max lat/lon values, if these have not
 * been determined yet
 */
{
  int i, ib, ind, ix, iy, k, npts;
  float dum, word, dword, latlon[2], *gptr, *optr, *ppd, scl, x, y;
  double alpha, beta, theta, tmin, tmax, xlat, xlon, xave, yave, zave,
   *xx, *yy, *zz, zmin, zmax;

  for (iy=1; iy<=nl; iy++) {
    for (ix=1; ix<=ns; ix++) {
      x = (float)ix;
      y = (float)iy;
      if (map_type==16) {
 	zpproj( &t_data_pov, &y, &x, &latlon[0], &latlon[1], LS_LL, lattyp,
	 &rtang, &slant, &ind);
	if (!ind) continue;
      }
      else {
	ztranv( &ind, map_type, LS_LL, map_samp, map_line, latitude,
	 parallel[0], parallel[1], longitude, scale, map_pole,
	 &y, &x, &latlon[0], &latlon[1], pol_rad, eq_rad, north);
	if (pgraphic==1 && (map_type!=10 || oldver))
	 latlon[0] = cen2det( latlon[0]);
	if (ind) continue;
      }
	/* fix up longitudes:  ensure they run from 0 to 360
	 * and convert them to East Long. for VENUS: */
      if (elon) latlon[1] = -latlon[1];
      if (latlon[1] < 0.) latlon[1] += 360.;
      if (latlon[1] > 360.) latlon[1] -= 360.;

	/* store lat/lon in cocube: */
      gptr = gdata + (iy-1)*ns + ix-1;
      for (i=0; i<2; i++) {
	*gptr = latlon[i];
	gptr += nl*ns;
      }
      if (!oldver && map_type==16) {	/* store slant */
	gptr += 3*nl*ns;
	*gptr = slant;
	gptr += nl*ns;
      }
      else 
	gptr += 4*nl*ns;
      /* emulate a "fill" for band 7 -- but be sure not to overwrite
       * a valid height: */
      if (*gptr == (float)NULL4) *gptr = 0.;
    }
  }
	/*
	 * if min/max lat/longs were not determined so far, then scan 
	 * backplanes for these -- for lat could just do straight max/min,
	 * but this doesn't work for lon if zero meridian is in image
	 * -- need to convert to vectors and start from center point
	 * (the LJR algorithm) ...
	 * (also check if any data exist at same point, as alot of
	 * empty space can be included for some projections!)
	 */
  if (mmlat[0] < 0.9e30 && mmlat[1] > -0.9e30 && mmlon[0] < 0.9e30 &&
   mmlon[1] > -0.9e30) return;
  zvmessage(" min/max lat/lon determined in write_latlon", "");
  xx = (double *)malloc(nl*ns*sizeof(double));
  yy = (double *)malloc(nl*ns*sizeof(double));
  zz = (double *)malloc(nl*ns*sizeof(double));
  xave = yave = zave = 0.0;
  for (npts=0, ix=1; ix<=ns; ix++) {
    for (iy=1; iy<=nl; iy++) {
      gptr = gdata + (iy-1)*ns + ix-1;
      xlat = (double)*gptr;
      gptr += nl*ns;
      xlon = (double)*gptr;
      if (xlat>=VALID_MIN4 && xlon>=VALID_MIN4) {
	for (ib=0; ib<nb; ib++) {
	  optr = odata + ib*nl*ns + (iy-1)*ns + ix-1;
	  dword = *optr;
	  if (dword>=(float)VALID_MIN4) break;
	}
	if (dword>=(float)VALID_MIN4) {
	  xlat /= degrad;
	  xlon /= degrad;
	  xx[npts] = cos(xlat)*cos(xlon);
	  yy[npts] = -cos(xlat)*sin(xlon);	/* West longitude */
	  zz[npts] = sin(xlat);
	  xave += xx[npts];
	  yave += yy[npts];
	  zave += zz[npts];
	  npts++;
	}
      }
    }
  }
  if (!npts) {
    zvmessage(" no on-planet points with valid data!","");
    return;
  }
  xave /= (double)npts;
  yave /= (double)npts;
  zave /= (double)npts;
  alpha = atan2(yave,xave);
  zmin = 99.;		/* -1.0 < z < +1.0 */
  zmax = -99.;
  tmin = 99.;		/* -pi < t < +pi */
  tmax = -99.;
  for (i=0; i<npts; i++) {
    if (zz[i]<zmin) zmin = zz[i];
    if (zz[i]>zmax) zmax = zz[i];
    beta = atan2(yy[i],xx[i]);
    theta = beta-alpha;	/* east longitude distance from ave */
    if (theta>zpi()) theta -= ztwopi();
    if (theta<(-zpi())) theta += ztwopi();
    if (theta<tmin) tmin = theta;
    if (theta>tmax) tmax = theta;
  }
  xlat = degrad*asin(zmin);
  mmlat[0] = (float)xlat;
  xlat = degrad*asin(zmax);
  mmlat[1] = (float)xlat;
  xlon = -degrad*(alpha+tmax);	/* convert to West long. */
  mmlon[0] = (float)xlon;	/* actually Easternmost long. */
  xlon = -degrad*(alpha+tmin);
  mmlon[1] = (float)xlon;	/* actually Westernmost long. */
  /* update_catalog may do some further adjustments ... */
}


/************************************************************************/
FUNCTION xconv_date( date)
/*
 * convert UTC date from SPICE to Sybase format
 * 
 * SPICE format:  "YYYY-DDD // HH:MM:SS.SSS"
 * Sybase format: "Mmm DD YYYY HH:MM:SS.SSSXX" (XX = AM or PM)
 */
char *date;
{
  char dxbuf[41], dtemp[10];	/* local buffers */
  char months[12][3] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
   "Aug", "Sep", "Oct", "Nov", "Dec"};
  int day, i, leapyear, month, year;

  if (strlen(date) > 40)
    zvmessage(" *** xconv_date: date string too long","");

  strcpy( dxbuf, date);		/* copy string to local buffer */
  strcpy( date, "                          ");	/* initialize with 26 blanks */

  /* then work our way left to right ... */

  /* can just copy the Year string: */
  strncpy( &date[7], dxbuf, 4);

  /* but must also extract it for leap-year det'n */
  sscanf( dxbuf, "%4d", &year);

  /* extract day-of-year: */
  sscanf( &dxbuf[5], "%3d", &day);

  /* figure out month and day-of-month: */
  leapyear = 0;
  if( year%4 == 0 )		/* (this is valid until 2100) */
    leapyear = 1;
  if( day <= 31 ) { 			/* January */
    month = 1;
  }
  else if( day <= 59 + leapyear ) {	/* February */
    month = 2;
    day -= 31;
  }
  else if( day <= 90 + leapyear ) {	/* March */
    month = 3;
    day -= 59 + leapyear;
  }
  else if( day <= 120 + leapyear ) {	/* April */
    month = 4;
    day -= 90 + leapyear;
  }
  else if( day <= 151 + leapyear ) {	/* May */
    month = 5;
    day -= 120 + leapyear;
  }
  else if( day <= 181 + leapyear ) {	/* June */
    month = 6;
    day -= 151 + leapyear;
  }
  else if( day <= 212 + leapyear ) {	/* July */
    month = 7;
    day -= 181 + leapyear;
  }
  else if( day <= 243 + leapyear ) {	/* August */
    month = 8;
    day -= 212 + leapyear;
  }
  else if( day <= 273 + leapyear ) {	/* September */
    month = 9;
    day -= 243 + leapyear;
  }
  else if( day <= 304 + leapyear ) {	/* October */
    month = 10;
    day -= 273 + leapyear;
  }
  else if( day <= 334 + leapyear ) {	/* November */
    month = 11;
    day -= 304 + leapyear;
  }
  else if( day <= 365 + leapyear ) {	/* December */
    month = 12;
    day -= 334 + leapyear;
  }
  else	{			/* INVALID DAY */
    month = 0;
    day = 0;
    return;
  }

  /* (forced to put day-month in the WRONG order!) */
  strncpy( date, months[month-1], 3);
  sprintf( dtemp, "%2d", day);
  strncpy( &date[4], dtemp, 2);

  /* check if AM or PM: */
  sscanf( &dxbuf[12], "%2d", &i);
  if (i>12) {
    i -= 12;
    sprintf( dtemp, "%2d", i);
    strncpy( &dxbuf[12], dtemp, 2);
    strncpy( &dxbuf[24], "PM", 2);
  }
  else
    strncpy( &dxbuf[24], "AM", 2);

  strncpy( &date[12], &dxbuf[12], 14);
}


/************************************************************************/
FUNCTION zgetom( r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
/*
 *  C-bridge for in-line routine GETOM.F
 */
float *r1, *r2, *r3, *r4, *r5, *r6, *r7, *r8, *r9, *r10, *r11, *r12;
{
FTN_NAME( getom)( r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12);
}


/************************************************************************/
FUNCTION znimsboom( cone, clock, iobs, filnam)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 * it calls NIMSBOOM via XNIMSBOOM (in-line with NIMSBOOM).
 */
float cone, clock;
int *iobs;
char *filnam;
{
  int i;
  i = strlen(filnam);
  FTN_NAME( xnimsboom)( &cone, &clock, iobs, filnam, &i);
}

/************************************************************************/
FUNCTION znims_check_rad( lrshsk, hrshsk, hrsbkg, pstatus)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 */
int *lrshsk, *hrshsk, *hrsbkg, *pstatus;
{
  FTN_NAME(nims_check_rad)( lrshsk, hrshsk, hrsbkg, pstatus);
}


/************************************************************************/
FUNCTION znims_check_dark( drkbuf, drkbuf1, drk_nl, drktyp, rim, mod91, dave,
  pstatus)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 */
int *drkbuf, *drkbuf1, drk_nl, drktyp, rim, mod91, *dave, *pstatus;
{
  FTN_NAME(nims_check_dark)( drkbuf, drkbuf1, &drk_nl, &drktyp, &rim, &mod91,
   dave, pstatus);
}


/************************************************************************/
FUNCTION znims_comp_rad( i, j, p1, p2, p3)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 */
int i, j, *p1, *p2, *p3;
{
  FTN_NAME(nims_comp_rad)( &i, &j, p1, p2, p3);
}


/************************************************************************/
FUNCTION znims_get_cal( file, pstatus)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 * it calls the NIMS_CAL routine via an "X..." routine, which is included
 * in nims_cal.f
 */
char *file;
int *pstatus;
{
  int i;
  i = strlen(file);
  FTN_NAME( xnims_get_cal)( file, &i, pstatus);
}


/************************************************************************/
FUNCTION znims_get_dark( file, drk_ns, drk_nl, drkbuf, pstatus)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 * it calls the NIMS_CAL routine via an "X..." routine, which is included
 * in nims_cal.f
 */
char *file;
int *drk_ns, *drk_nl, *drkbuf, *pstatus;
{
  int i;
  i = strlen(file);
  FTN_NAME( xnims_get_dark)( file, &i, drk_ns, drk_nl, drkbuf, pstatus);
}


/************************************************************************/
FUNCTION znims_get_cal_a( file, pstatus)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 * it calls the NIMS_CAL routine via an "X..." routine, which is included
 * in nims_cal.f
 */
char *file;
int *pstatus;
{
  int i;
  i = strlen(file);
  FTN_NAME( xnims_get_cal_a)( file, &i, pstatus);
}


/************************************************************************/
FUNCTION znims_get_dark_a( file, drk_ns, drk_nl, drkbuf, pstatus)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 * it calls the NIMS_CAL routine via an "X..." routine, which is included
 * in nims_cal.f
 */
char *file;
int *drk_ns, *drk_nl, *pstatus;
float drkbuf[712];
{
  int i;
  i = strlen(file);
  FTN_NAME( xnims_get_dark_a)( file, &i, drk_ns, drk_nl, drkbuf, pstatus);
}


/************************************************************************/
FUNCTION znims_set_rad( iop, imode, g_off, gstart, gdel, ngpos, lrshsk, hrshsk,
 hrsbkg, utemps, wavs, sensv, cbase, cmult, ppshift, painfl, xnull, xlsat,
 xhsat, pstat)
/*
 * this is part of the "C-bridge" nonsense needed for porting code ...
 */
int iop, imode, g_off, gstart, gdel, ngpos, *lrshsk, *hrshsk,
 *hrsbkg, *utemps, *wavs, *sensv, *cbase, *cmult, *ppshift, *painfl, *pstat;
float xnull, xlsat, xhsat;
{
  FTN_NAME(nims_set_rad)( &iop, &imode, &g_off, &gstart, &gdel, &ngpos,
   lrshsk, hrshsk, hrsbkg, utemps, wavs, sensv, cbase, cmult, ppshift, painfl,
   &xnull, &xlsat, &xhsat, pstat);
}


/************************************************************************/
FUNCTION zpproj( tdata, y, x, lat, lon, i, j, r, s, k)
/*
 * "C-bridge" to avoid errors passing numerical values for i & j
 */
int *tdata, *y, *x, *lat, *lon, i, j, *r, *s, *k;
{
  FTN_NAME(npproj)( tdata, y, x, lat, lon, &i, &j, r, s, k);
}

/************************************************************************/
FUNCTION zrotadkx( r1, i, j, r2)
/*
 * "C-bridge" 
 */
int i, j, *r1, *r2;
{
  FTN_NAME(rotadkx)( r1, &i, &j, r2);
}


/************************************************************************/
FUNCTION zload_spice95( et, spk, i)
/*
 * "C-bridge" 
 */
double et;
int *spk, *i;
{
  FTN_NAME(load_spice95)( &et, spk, i);
}


/*****************************************************************************/
/* THIS CONTAINS THE PARTS OF
/*     GLL_NIMS_BIN_PH2
/* THAT ARE NEEDED BY NIMSCMM2 */

/*** Externalized Function Prototypes ***/

int get_nims_edr_hdr_2();
int get_nims_edr_rec_2();

/*** Static Function Prototypes ***/

static void trin_nims_ert_typ();
static void trin_nims_sclk_typ();
static void trin_aacs_lrs_p_typ();
static void trin_nims_lrs_p_typ();
static void trin_decoder_snr_typ();
static void trin_nims_data_typ();
static void trin_nims_er_flg_typ();

	/* Next three lines added to eliminate ANSI C compile errors (GMY) */
static int init_trans_inbn();
static int init_pixsizebn();

/*****************************************************************************/
/* This routine reads and translates the NIMS EDR/UDR binary header.  The    */
/* header is composed of two records.  The translation is from NATIVE_HOST   */
/* (VAX) into the NATIVE format.                                             */
/*****************************************************************************/
ROUTINE int get_nims_edr_hdr_2(unit, dest)
int unit;		/* must be an open file with COND BINARY set */
nims2_hdr_typ *dest;
{
   unsigned char buf[NIMS_EDR_RECORDS*NIMS_RECORD_LENGTH],
		*p = buf;
   int            recsize,i,vstat = SUCCESS,
                  nlb,     /* number of lines of binary */
                  nlb_pds, /* number of lines of pds */
                  line;    /* line to get */


   init_trans_inbn(unit,byte_trans,half_trans,full_trans); 
   vstat = init_pixsizebn(unit,&byte_size,&half_size,&full_size);
   if (vstat != SUCCESS) return vstat;

   vstat = zvget(unit,"RECSIZE",&recsize,"NLB",&nlb,NULL);
   if (vstat != SUCCESS) return vstat;
   if (recsize > NIMS_RECORD_LENGTH) {
     zvmessage(" *** get_nims_edr_hdr_2: invalid record length ***"," ");
     return (-1);
   }

   if (nlb>2) {  /*Skip over PDS labels of an EDR */
      nlb_pds = 0;
      (void) zlget(unit,"HISTORY","NLB_PDS",&nlb_pds,NULL);
      line = nlb_pds + 1;
   }
   else line = 1;

   for ( i=0; i<NIMS_EDR_RECORDS; i++, line++) {
     vstat = zvread(unit,buf + (recsize * i),"LINE",line,NULL);
     if (vstat != SUCCESS) return vstat;
   }

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   /* translate first record */

   trin_nims_sclk_typ(&p, &dest->hdr1.fsclk);
   trin_nims_sclk_typ(&p, &dest->hdr1.lsclk);
   trin_nims_ert_typ(&p, &dest->hdr1.fert);
   trin_nims_ert_typ(&p, &dest->hdr1.lert);
   zvtrans( byte_trans, p, dest->hdr1.data_present, 46);
   p += 46 * byte_size;                      
   zvtrans( byte_trans, p, dest->hdr1.data_recd, 46);
   p += 46 * byte_size;                      
   zvtrans( half_trans, p, dest->hdr1.threshval, 17);
   p += 17 * half_size;                      
   zvtrans( half_trans, p, &dest->hdr1.total_rec, 1);
   p += half_size;                      
   zvtrans( half_trans, p, &dest->hdr1.total_zero, 1);
   p += half_size;                      
   TFULL(p, &dest->hdr1.comp_bytes);
   TFULL(p, &dest->hdr1.uncomp_bytes);
   zvtrans( byte_trans, p, &dest->hdr1.comp_ratio, 6);
   p += 6 * byte_size;                      

   zvtrans(byte_trans, p, dest->hdr1.reserved, 850);
   p += 850 * byte_size;                      

   /* now second record */

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   /* translate second record */

   TSTRN(p, dest->hdr2.oapel, 512);	/* do whole table in one call */
   zvtrans(byte_trans, p, dest->hdr2.reserved, 512);

   return vstat;	
} /* end get_nims_edr_hdr */

/*****************************************************************************/
/* This routine reads and translates one NIMS EDR/UDR data record            */
/* The translation is from NATIVE_HOST (VAX) into the NATIVE format.         */
/*****************************************************************************/
ROUTINE int get_nims_edr_rec_2(unit, line, dest)
int unit,		/* must be an open file with COND BINARY set */
    line;               /* line to retrieve binary prefix from */

nims2_rec_typ *dest;
{  
   unsigned char buf[NIMS_RECORD_LENGTH], 
		*p = buf;
   int            nlb, /* number of lines of binary */
                  nbb, /* number of bits of binary */
                  recsize,i,
                  vstat = SUCCESS;   /* return vstat initialized */
   UWORD          bits_half; 
   unsigned char byte = 0;

   init_trans_inbn(unit,byte_trans,half_trans,full_trans); 
   vstat = init_pixsizebn(unit,&byte_size,&half_size,&full_size);
   if (vstat != SUCCESS) return (-1);

   vstat = zvget(unit,"RECSIZE",&recsize,"NLB",&nlb,NULL);
   if (vstat != SUCCESS) return vstat;
   if (recsize != NIMS_RECORD_LENGTH) {
     zvmessage(" *** write_nims_edr_hdr_2: invalid record length ***"," ");
     return (-1);
   }

   vstat = zvread(unit,buf,"LINE",line+nlb,"NSAMPS",recsize,NULL);
   if (vstat != SUCCESS) return vstat;

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   trin_nims_sclk_typ(&p, &dest->pfix.sclk);
   trin_nims_ert_typ(&p, &dest->pfix.ert1);
   trin_nims_ert_typ(&p, &dest->pfix.ert2);
   trin_nims_ert_typ(&p, &dest->pfix.ert3);
   TBYTE(p, &dest->pfix.apid);
   TFULL(p, &dest->pfix.pkt_seq[0]);
   TFULL(p, &dest->pfix.pkt_seq[1]);
   TFULL(p, &dest->pfix.pkt_seq[2]);
   TBYTE(p, &dest->pfix.insmode);
   TBYTE(p, &dest->pfix.data_complete);
   TBYTE(p, &dest->pfix.compression_stat);
   THALF(p, &dest->pfix.subpkt_size);
   THALF(p, &dest->pfix.data_size);
   TBYTE(p, &dest->pfix.mirror_dir);
   TBYTE(p, &dest->pfix.grating_position);
   trin_nims_er_flg_typ(&p, &dest->pfix.error_flags);
/*   TBYTE(p, &dest->pfix.spare1); */
   TFULL(p, &dest->pfix.det_mask);
   TFULL(p, &dest->pfix.mirror_mask);
   TBYTE(p, &dest->pfix.rrpmf);

   TBYTE(p, &byte);
   dest->pfix.sclk_flag1.sclk_suspect = byte&0x01; 
   dest->pfix.sclk_flag1.ert_val = (int) (byte&0x02) >> 1; 
   dest->pfix.sclk_flag1.scid_force = (int) (byte&0x04) >> 2; 
   dest->pfix.sclk_flag1.data_val = (int) (byte&0x08) >> 3; 
   dest->pfix.sclk_flag1.replay_flag = (int) (byte&0x10) >> 4; 
   dest->pfix.sclk_flag1.test_mode = (int) (byte&0x20) >> 5; 
   dest->pfix.sclk_flag1.data_mode = (int) (byte&0x40) >> 6; 
   dest->pfix.sclk_flag1.pb_mode = (int) (byte&0x80) >> 7; 
   byte = 0;
   TBYTE(p, &byte);
   dest->pfix.sclk_flag2.pkt_flag = (int) (byte&0xC0) >> 6; 
   dest->pfix.sclk_flag2.sclk_flag = (int) (byte&0x38) >> 3; 
   dest->pfix.sclk_flag2.sclk_calc_suspect = (int) (byte&0x04) >> 2; 
   dest->pfix.sclk_flag2.sclk_unexpected = (int) (byte&0x02) >> 1; 
   dest->pfix.sclk_flag2.sclk_corr = (byte&0x01); 
   byte = 0;
   TBYTE(p, &byte);
   dest->pfix.sclk_flag3.flush_flag = (int) (byte&0xF0) >> 4;
   dest->pfix.sclk_flag3.scet_val = (int) (byte&0x08) >> 3; 
   dest->pfix.sclk_flag3.scet_int = (int) (byte&0x04) >> 2; 
   dest->pfix.sclk_flag3.less_than_max = (int) (byte&0x02) >> 1; 

   zvtrans( byte_trans, p, dest->pfix.reserved, 276);
   p += 276 * byte_size;

   /* these are *image* data, so have already been translated by zvread! */
   zmve( 2, 340, p, dest->sensor, 1, 1);

   return vstat;
} /* end get_nims_edr_rec_2 */


/*****************************************************************************/
/* Translation routines for specific datatypes: translate any input host fmt */
/*****************************************************************************/
ROUTINE static void trin_nims_ert_typ(from, to)
unsigned char **from;
nims2_ert_typ *to;
{
   THALF(*from, &to->year);
   TBYTE(*from, &to->month);
   TBYTE(*from, &to->day);
   TBYTE(*from, &to->hour);
   TBYTE(*from, &to->minute);
   TBYTE(*from, &to->second);
   THALF(*from, &to->millisecond);
}

ROUTINE static void trin_nims_sclk_typ(from, to)
unsigned char **from;
nims2_sclk_typ *to;
{
   TFULL(*from, &to->rim);
   TBYTE(*from, &to->mod91);
   TBYTE(*from, &to->rti);
}

ROUTINE static void trin_nims_er_flg_typ(from, to)
unsigned char **from;
nims_er_flg_typ *to;
{
   to->overflow = **from & 0x01;
   to->underflow = (**from>>1) & 0x01;
   to->fill = (**from>>2) & 0x1F;
   *from+=1;
}

/****************************************************************************/
/* This routine sets up translation buffers for input, converting from the  */
/* host representation into the machine's native representation.            */
/****************************************************************************/
ROUTINE static int init_trans_inbn(unit,byte_tr,half_tr,full_tr)
int unit,
    *byte_tr,
    *half_tr,
    *full_tr;
{
   int vstat;

   vstat = zvtrans_inb(byte_tr, "BYTE", "BYTE", unit);
   if (vstat != SUCCESS) return vstat;
   vstat = zvtrans_inb(half_tr, "HALF", "HALF", unit);
   if (vstat != SUCCESS) return vstat;
   vstat = zvtrans_inb(full_tr, "FULL", "FULL", unit);
   return vstat;
} /* end init_trans_inbn */

/****************************************************************************/
/* This routine returns the size of a binary label value (in bytes) from a  */
/* file.                                                                    */ 
/****************************************************************************/
ROUTINE static int init_pixsizebn(unit,byte_sz,half_sz,full_sz)
int unit,
    *byte_sz,
    *half_sz,
    *full_sz;
{
   int vstat;
   char aline[80];
 
   vstat = zvpixsizeb(byte_sz, "BYTE", unit);
   if (vstat != SUCCESS) return vstat;
   if (byte_sz == 0) {
     (void) sprintf(aline,
         "init_pixsizebn> error in byte pixel size determination, status %d",
         byte_sz);
     zvmessage(aline,0);
     return (-1);
   }

   vstat = zvpixsizeb(half_sz, "HALF", unit);
   if (vstat != SUCCESS) return vstat;
   if (half_sz == 0) {
     (void) sprintf(aline,
         "init_pixsizebn> error in halfword pixel size determination, status %d",
         half_sz);
     zvmessage(aline,0);
     return (-1);
   }

   vstat = zvpixsizeb(full_sz, "FULL", unit);
   if (vstat != SUCCESS) return vstat;
   if (full_sz == 0) {
     (void) sprintf(aline,
    "init_pixsizebn> error in fullword pixel size determination, status %d",full_sz);
     zvmessage(aline,0);
     return (-1);
   }
   return vstat;
} /* end init_pixsizebn */
/****************************************************************************/
/*  end module                                                              */
/****************************************************************************/
