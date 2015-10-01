$!****************************************************************************
$!
$! Build proc for MIPL module galsos
$! VPACK Version 1.9, Monday, April 23, 2012, 21:23:49
$!
$! Execute by entering:		$ @galsos
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   DOC         Only the documentation files are created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module galsos ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Doc = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "DOC" then Create_Doc = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Create_Doc .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to galsos.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Doc then gosub Doc_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Create_Doc = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("galsos.imake") .nes. ""
$   then
$      vimake galsos
$      purge galsos.bld
$   else
$      if F$SEARCH("galsos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake galsos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @galsos.bld "STD"
$   else
$      @galsos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create galsos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack galsos.com -mixed -
	-s galsos.c gll_ssi_bin.c gll_ssi_bin_ph2.c gllcalname.c -
	   get_ssi_prefix.c get_ssi_ph2_prefix.c gll_lrs.h gll_rts_main.h -
	   gll_ssi_edr.h gll_tlm_code.h gll_ph2_ssi_bin.h gll_rts_main_diff.h -
	   gll_ph2_ssi_edr.h gll_ssi_bin.h gll_ict_qqc.h gll_decompression.h -
	   gll_main.h gll_ssi_bin_diff.h -
	-i galsos.imake -
	-p galsos.pdf -
	-d get_ssi_ph2_prefix.hlp gll_ssi_bin.hlp gllcalname.hlp -
	   get_ssi_prefix.hlp gll_ssi_bin_ph2.hlp -
	-t tstgalsos.pdf tstgalsos.log_solos tstgalsos.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create galsos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Program GALSOS  */

/*
 Copyright 2011, by the California Institute of Technology. ALL RIGHTS
 RESERVED. United States Government Sponsorship acknowledged. Any
 commercial use must be negotiated with the Office of Technology Transfer
 at the California Institute of Technology.
 
 This software may be subject to U.S. export control laws. By accepting
 this software, the user agrees to comply with all applicable U.S. export
 laws and regulations. User has the responsibility to obtain export
 licenses, or other export authority as may be required before exporting
 such information to foreign countries or providing access to foreign
 persons.
*/

/*
  This version of GALSOS is intended only to generate EDR files
  (radiometrically calibrated) from the REDR files stored on PDS.
  All references to sybase/catalog and GROUNDCAL mission phase
  have been removed -lwk- jul2011.
*/

#include <stdio.h>
#include <stdlib.h>
#include "vicmain_c"
#include <math.h>
#include <string.h>
#include "rts_typedefs.h"
#include "gll_rts_main.h"
#include "gll_rts_main_diff.h"
#include "gll_lrs.h"
#include "gll_tlm_code.h"
#include "gll_ssi_edr.h"
#include "gll_ph2_ssi_edr.h"
#include "gll_ssi_bin.h"
#include "able86.h"
#ifdef NSAMPS_SE
#undef NSAMPS_SE
#endif
#include "gll_ph2_ssi_bin.h"
#include "gll_decompression.h"
#include "gll_ict_qqc.h"
#include "file_name.h"

extern int zvptst();

#define MAX_BLEMS	10000	/* Max number of camera blemishes */
#define MAX_OBJ_LTH	299	/* Max objects in bad-data value record */

#define KM_PER_AU   149597900.0
#define LOWEST_DN  -32768 
#define LOWEST_DN1  0
#define HIGHEST_DN  32767

        /* Mission Phase */
#define PHASE1	1
#define PHASE2	2

char cal_filename[121];		/* radiometric file spec		*/
char dc_filename[121];		/* dark-current file spec		*/
char blem_filename[121];	/* blemish file spec			*/
char offset_filename[121];	/* shutter-offset file spec		*/

char dirdefault[256];		/* default calibration file directory(UNIX)  */
char dircal[256];		/* radiometric file directory	(UNIX)	*/
char dirdc[256];	       	/* dark-current file directory	(UNIX)	*/
char dirblm[256];		/* blemish file directory	(UNIX)	*/
char diroff[256];		/* shutter-offset file directory(UNIX)  */
int g1,g2,g3,g4,g5;		/* presence of dirdefault, dircal, etc. */

int mission_phase;		/* 1=ground [removed], 2=phase1, 3=phase2 */
int iunit;			/* input image unit number	*/
int ounit;			/* output image	unit number	*/
int cal_unit;			/* radiometric file unit number	*/
int dc_unit;			/* dark-current file unit number*/

int sflag;    			/* =2 for summation-mode, =1 for full frame */
int badlabel;  			/* =1 if input has baddata value labels */
int blemcor;			/* =1 if input has been blemish corrected */
UBYTE first_task[10];		/* name of first task in history label */

int nl,ns;			/* output image size		*/
int nlb;			/* # of binary hdr recs in input*/
int nlbout;			/* # of binary hdr recs in output */
able_86_typ gll;		/* able86 buffer		 */
		/* UDR/EDR telemetry header record */
ssi_edr_hdr_typ tlm_hdr;      	/* Phase1 */
ssi_hdr_typ ph2_tlm_hdr;      	/* Phase2 */
		/* Bad data value objects */
ssi_bdvh_typ ddo={3,2,0};	/* Data drop outs */
ssi_bdvh_typ sat={4,2,0};	/* Saturated pixels */
ssi_bdvh_typ rso={7,2,0};	/* Reed/Solomon overflow */
ssi_bdvh_typ lfw={5,3,0};	/* Low full-well pixels */
int max_objs;			/* max number of objects in BDV rec */
int obj_size;			/* max byte size of BDV rec */
		/* Phase 1 EDR */ 
typedef struct {
  ssi_lhdr_typ prefix;
  short	pixels[800];
} out_line_typ; 
out_line_typ *out_image;		/* Phase 1 */
ssi_edr_line_typ *out_ph2_image;	/* Phase 2  */
		/* Blemish file */
typedef	struct { 
  short	line;		/* line number of blemish	*/
  short samp;		/* sample number of blemish	*/
  short class;		/* blemish classification	*/
  short sat_dn;		/* low-full-well saturation DN	*/
} blem_typ;
blem_typ blemishes[MAX_BLEMS];	/* blemish data	*/
int nblem;			/* number of blemishes */
blem_typ *current_blem;		/* next uncorrected blem	*/
blem_typ *last_blem;		/* addr. of last blem		*/
blem_typ *zap_index;		/* zap_index to current blem	*/

		/* Entropy calculation */
int calc_entropy;		/* calculate entropy */
int img_dhist[512];		/* difference histogram for image */
int line_dhist[512];		/* diff hist for every 50th line */
float entropies[15];		/* line entropies */
float entropy;			/* image entropy */

		/* radiometric correction parameters */
int scale_type;		/* =1 for IOF/DN; =2 for nW/DN  */
float conv1;		/* I/F per Ft-Lambert conversion factor */
float conv2;		/* Nanowatts per Ft-Lambert conversion  */
float a1;		/* I/F per DN			*/
float a2;		/* Nanowatts per DN		*/
float scale;		/* output DN per Ft-Lambert	*/
float dc_scale;		/* dark-current picture scale	*/
int dc_fmt;		/* dc format (1=byte, 2=half)   */
float t0[800];		/* line-dependent shutter offsets*/
float wght[256];	/* uneven bit-weight look-up tbl*/

		/* Output image statistics */
int sat_cnt;		/* Number of saturated pixels	*/
int c_blem_cntr = 0;	/* Number of corrected blemishes */
int neg_dn = 0;		/* Number of negative Dns */
int zero_dn = 0;	/* Number of ZERO Dns     */
int pos_dn = 0;		/* Number of positive Dns */
int vld_pxls = 0;	/* number of valid pixels in image */
int sum_iof = 0;	/* sum of output pixels   */
double mean_iof;
 
int sclk,partition,debug;
char msg[128];

static void add_segment(ssi_bdvh_typ *,int,int,int,int *,int);
static void bitwght(int,int,float *);
static void calc_diff_hist(int *,int,int,int,int,unsigned char *);
static void calculate_entropy(int *,float *);
static int check_camera_parameters(int,int,int,int,int,int,int,int,int,int);
static void decal1(short *,UBYTE *,UBYTE *,float *,float *,float,int,int,int,int);
static void decal2(short *,UBYTE *,short *,float *,float *,float,
		float,int,int,int,int);
static int getblems(char *,blem_typ *,int *,int *,int *,int *,able_86_typ *);
static void getlabval(int,char *,int *,int *);
static int getoffsets(char *,float *,able_86_typ *);
static void find_bdv(UBYTE *,int,int,int,int,int,ssi_bdvh_typ *,
		ssi_bdvh_typ *,int *,int);
static int open_cal_files();
static int opencal(int *,char *,int *,int *,able_86_typ *);
static int opendc(int *,char *,int *,int *,float *,int *,able_86_typ *);
static int openinp(int *,int *,int *,int *, int *,int *,int *,int *,
	int *,int *,able_86_typ *,int *);
static int openout(int *,able_86_typ *);
static int opscale(int,int,int,float *,float *,float *,int *,able_86_typ *);
static void remove_blemishes(short *,short *,short *,int);
#if 0 /* RRP Removed */
static void trans_blem(int,int,blem_typ *);
#endif
static int update_corrected_table(int,int,float,int,able_86_typ *);
static int update_overview_table(int,int);
static void verify_class(blem_typ *,short *,short *,short *);
static int write_history_labels(float *);
static void zap_lfw_pixels(UBYTE *,int,int,int,int,int,int *,
	ssi_bdvh_typ *,int);

/* added for 2011 Unix version: */
static void copy_binary_headers();
static void get_solar_range( able_86_typ *, int *);

void main44()
{
  int i,line,ind,idn,samp;
  short *pixels;
  for (i=0; i < 20 ; i++){
       gll.rad_fn[i] = '\0';
       gll.dc_fn[i]  = '\0';
  }
  zvmessage(" GALSOS version 19-Jul-11","");
  debug = zvptst("DEBUG");
	/* Open input and get label & data information */
  ind = openinp(&iunit,&badlabel,&blemcor,&sflag,&nl,&ns,&nlb,
		&max_objs,&obj_size,&calc_entropy,&gll,&mission_phase);
  if (!badlabel) calc_entropy =TRUE; /* IF ENTROPY WAS NOT CALCULATED BY */
  if (!ind) goto FATAL_ERR;          /* BADLABELS THEN CALCULATE ENTROPY */
	/* Allocate memory for output EDR */
  if (mission_phase==PHASE2 && nlb>0) {
     out_ph2_image = (ssi_edr_line_typ *) malloc(nl*sizeof(ssi_edr_line_typ));
     if (!out_ph2_image) goto MEMORY_ERR;
     }
  else {
     out_image = (out_line_typ *) malloc(nl*sizeof(out_line_typ));
     if (!out_image) goto MEMORY_ERR;
     }

  ind = open_cal_files();	/* Open calibration files */
  if (!ind) goto FATAL_ERR;

  ind = openout(&ounit,&gll);	/* Open output EDR */
  if (nlb > 0) copy_binary_headers();
  ind = decalibrate_image();	/* Perform radiometric calibration */
  ind = write_history_labels(&entropy);

		/* Write out image lines */

  vld_pxls = 0;
  sum_iof = 0;

  for (line=1; line<=nl; line++) {
     if (nlb == 0) zvwrit(ounit,out_image[line-1].pixels,NULL);
     else {
        if (mission_phase==PHASE1) {
           write_gll_edr_ln(ounit,line,&out_image[line-1]);
           pixels = out_image[line-1].pixels;
        }
        else {
           write_gll_ph2_edr_ln(ounit,line,&out_ph2_image[line-1]);
           pixels = out_ph2_image[line-1].pixels;
        }
        for (samp=1; samp<=ns; samp++) {
            idn = *pixels++;
            if (idn != LOWEST_DN) {
               sum_iof += idn;
               if (idn > 0) pos_dn++;
               else if (idn < 0) neg_dn++;
               else if (idn == 0) zero_dn++;
               vld_pxls++;
            }
        }
     }
  }

  if (nlb > 0) update_telemetry_header();

  zvmessage(" GALSOS task completed","");
  return;

MEMORY_ERR:
  zvmessage(" ***Error allocating memory",""); 

FATAL_ERR:
  zmabend(" *** GALSOS task cancelled ***");
} /* end main44 */


/************************************************************************
 * Add BDV line or column segment to BDV record.			*
 * May be called two ways:						*
 *    add_segment(bdv,line,ss,es,nlbout,ounit)				*
 * or add_segment(bdv,samp,sl,el,nlbout,ounit)				*
 * If record is full, it is written out and nlbout is updated.		*
 ************************************************************************/
static void add_segment(bdv,line,ss,es,nlbout,ounit)
int line,ss,es,*nlbout,ounit;
ssi_bdvh_typ *bdv;
{
  bdv->coords.line_data[bdv->nobjs].line = line;
  bdv->coords.line_data[bdv->nobjs].ss = ss;
  bdv->coords.line_data[bdv->nobjs++].ns = es - ss + 1;

	/* write bdv record if full */
  if (bdv->nobjs == max_objs) {
     (*nlbout)++;
     write_ssi_bdv_hdr_rec(ounit,*nlbout,bdv);
     memset(bdv->coords.line_data,0,obj_size);
     bdv->nobjs = 0;
  }
} 
/***************************************************************************
 * Adjustments to DN values to correct for uneven bit weighting            *
 ***************************************************************************/
static void bitwght(igain,sflag,wght)
    int igain;		/* 1=400K, 2=100K, 3=40K, 4=10K */ 
    int sflag;		/* 1=full-frame, 2=summation-mode */
    float wght[];	/* output corrected DN value table */
{
/***	Table received from Herb Breneman, 11 May 89, based on 1/89	***/
/***	calibration data						***/
  static float weight[4][256]= {
		/***   Full 10/40K  ***/
{  0.000,  1.000,  2.000,  3.000,  4.000,  5.000,  6.000,  6.645,  7.645,
   8.998,  9.998, 10.838, 11.838, 13.104, 14.104, 15.118, 16.118, 17.118,
  18.118, 18.900, 19.900, 21.107, 22.107, 23.094, 24.094, 25.036, 26.036,
  26.883, 27.883, 29.037, 30.037, 31.013, 32.013, 33.082, 34.082, 34.937,
  35.937, 37.081, 38.081, 39.191, 40.191, 41.073, 42.073, 42.910, 43.910,
  45.045, 46.045, 47.062, 48.062, 49.060, 50.060, 50.934, 51.934, 53.050,
  54.050, 55.172, 56.172, 57.041, 58.041, 58.931, 59.931, 61.025, 62.025,
  63.236, 64.236, 65.045, 66.045, 66.955, 67.956, 69.027, 70.027, 71.190,
  72.190, 73.018, 74.018, 74.956, 75.956, 77.003, 78.004, 78.993, 79.993,
  81.040, 82.040, 82.984, 83.984, 84.999, 85.999, 87.190, 88.190, 88.998,
  89.998, 90.954, 91.954, 92.970, 93.970, 95.195, 96.195, 96.999, 97.999,
  98.983, 99.983,100.980,101.980,103.178,104.178,104.974,105.974,106.974,
 107.974,108.949,109.949,110.880,111.880,112.972,113.972,114.992,115.992,
 116.956,117.956,119.186,120.186,120.948,121.948,122.975,123.975,124.921,
 125.921,127.541,128.674,129.137,130.003,131.037,132.037,132.951,133.951,
 135.161,136.161,136.938,137.938,139.005,140.005,140.906,141.906,142.826,
 143.826,144.942,145.942,147.024,148.024,148.922,149.922,151.166,152.166,
 152.929,153.929,155.018,156.018,156.900,157.900,159.281,160.281,160.934,
 161.935,163.042,164.042,164.902,165.902,167.140,168.140,168.901,169.901,
 171.017,172.017,172.874,173.874,174.702,175.702,176.915,177.915,179.041,
 180.041,180.902,181.902,183.163,184.163,184.914,185.914,187.036,188.036,
 188.882,189.883,191.355,192.355,192.906,193.906,195.090,196.090,196.871,
 197.871,199.192,200.192,200.870,201.870,203.028,204.028,204.788,205.788,
 206.593,207.593,208.841,209.841,211.043,212.043,212.813,213.813,215.208,
 216.209,216.821,217.821,219.041,220.042,220.800,221.800,223.349,224.349,
 224.885,225.885,227.146,228.146,228.849,229.849,231.231,232.231,232.837,
 233.837,235.100,236.100,236.794,237.794,238.571,239.571,240.851,241.851,
 243.119,244.119,244.829,245.829,247.280,248.280,248.822,249.822,251.113,
 252.113,252.823,253.823,255.000
},
		/***   Full 100K  ***/
{  0.000,  1.000,  2.000,  3.000,  4.000,  4.695,  5.695,  7.141,  8.141,
   9.064, 10.064, 10.889, 11.889, 13.096, 14.096, 15.063, 16.063, 17.163,
  18.163, 18.940, 19.940, 21.150, 22.150, 23.201, 24.201, 24.893, 25.894,
  26.826, 27.826, 28.980, 29.980, 30.987, 31.987, 33.083, 34.083, 34.900,
  35.900, 37.067, 38.068, 39.121, 40.121, 41.055, 42.055, 42.922, 43.922,
  45.078, 46.078, 47.001, 48.001, 48.985, 49.985, 50.892, 51.893, 53.028,
  54.028, 55.098, 56.098, 56.964, 57.964, 58.890, 59.890, 61.022, 62.022,
  63.048, 64.048, 65.045, 66.045, 66.944, 67.944, 69.041, 70.041, 71.138,
  72.138, 73.022, 74.022, 74.943, 75.943, 77.015, 78.015, 78.933, 79.933,
  81.024, 82.025, 82.955, 83.956, 85.015, 86.016, 87.154, 88.154, 89.007,
  90.007, 90.997, 91.998, 92.914, 93.914, 95.057, 96.057, 96.961, 97.961,
  98.973, 99.973,100.963,101.963,103.131,104.131,104.944,105.944,106.951,
 107.951,108.939,109.939,110.799,111.800,112.949,113.950,114.980,115.980,
 117.001,118.002,119.142,120.143,120.969,121.970,122.982,123.982,124.962,
 125.962,127.147,128.148,129.024,130.024,131.023,132.023,133.006,134.006,
 135.125,136.125,136.986,137.986,139.021,140.021,140.969,141.969,142.830,
 143.831,145.002,146.003,147.037,148.037,149.011,150.011,151.118,152.118,
 152.971,153.971,155.050,156.050,156.953,157.953,159.252,160.252,160.993,
 161.994,163.071,164.071,164.980,165.981,167.138,168.139,168.958,169.958,
 171.063,172.063,172.950,173.950,174.753,175.753,176.996,177.996,179.078,
 180.078,181.014,182.014,183.211,184.212,185.077,186.077,187.065,188.065,
 188.893,189.893,191.236,192.236,192.917,193.917,195.122,196.122,196.866,
 197.866,199.151,200.151,200.868,201.868,203.105,204.105,204.836,205.836,
 206.597,207.597,208.885,209.885,211.097,212.097,212.824,213.824,215.178,
 216.178,216.800,217.800,219.079,220.079,220.803,221.803,223.371,224.371,
 224.887,225.887,227.126,228.126,228.885,229.885,231.180,232.180,232.854,
 233.854,235.106,236.106,236.838,237.838,238.561,239.561,240.883,241.883,
 243.119,244.119,244.886,245.886,247.230,248.230,248.870,249.870,251.111,
 252.111,252.868,253.868,255.000
},
		/***   Summation 10/40K  ***/
{  0.000,  1.256,  2.257,  3.235,  4.235,  5.262,  6.262,  6.517,  7.518,
   9.239, 10.239, 11.206, 12.206, 12.990, 13.545, 13.571, 15.016, 17.257,
  18.257, 19.244, 20.244, 21.284, 22.284, 22.536, 23.536, 25.221, 26.221,
  27.215, 28.215, 29.264, 30.264, 30.513, 31.513, 33.259, 34.260, 35.238,
  36.238, 37.265, 38.265, 38.520, 39.521, 41.242, 42.242, 43.209, 44.209,
  44.993, 45.548, 45.574, 47.019, 49.260, 50.260, 51.247, 52.247, 53.287,
  54.287, 54.539, 55.539, 57.078, 58.078, 59.054, 60.054, 61.269, 62.269,
  62.476, 63.476, 65.201, 66.201, 67.085, 68.085, 69.219, 70.219, 70.492,
  71.492, 73.154, 74.154, 75.069, 76.069, 77.095, 77.815, 77.907, 79.187,
  81.173, 82.173, 83.076, 84.076, 85.284, 86.284, 86.518, 87.518, 89.225,
  90.225, 91.219, 92.219, 93.268, 94.268, 94.517, 95.517, 97.263, 98.264,
  99.242,100.242,101.269,102.269,102.524,103.525,105.246,106.246,107.213,
 108.213,108.997,109.552,109.578,111.023,113.264,114.264,115.251,116.251,
 117.291,118.291,118.543,119.543,121.228,122.228,123.222,124.222,125.271,
 126.271,126.520,127.520,129.266,130.267,131.245,132.245,133.272,134.272,
 134.527,135.528,137.318,138.318,139.186,140.186,140.980,141.509,141.528,
 142.999,145.321,146.321,147.270,148.270,149.337,150.337,150.543,151.543,
 153.309,154.309,155.242,156.242,157.297,158.297,158.540,159.540,161.327,
 162.327,163.293,164.293,165.334,166.334,166.535,167.535,169.300,170.300,
 171.263,172.263,173.089,173.717,173.726,175.098,177.316,178.316,179.276,
 180.276,181.322,182.322,182.527,183.527,185.274,186.274,187.216,188.216,
 189.337,190.337,190.474,191.474,193.277,194.277,195.298,196.299,197.282,
 198.283,198.535,199.536,201.255,202.255,203.289,204.289,205.085,205.708,
 205.719,207.097,209.260,210.260,211.346,212.346,213.296,214.296,214.536,
 215.537,217.225,218.225,219.289,220.289,221.203,222.203,222.601,223.601,
 225.283,226.283,227.333,228.333,229.266,230.266,230.557,231.557,233.231,
 234.231,235.293,236.293,236.871,237.268,237.271,238.874,241.271,242.271,
 243.332,244.332,245.267,246.267,246.598,247.598,249.277,250.277,251.330,
 252.330,253.271,254.271,254.530
},
		/***   Summation 100/400K  ***/
{  0.000,  1.000,  2.000,  3.000,  4.000,  4.864,  5.864,  7.227,  8.227,
   9.202, 10.203, 11.054, 12.054, 13.280, 14.280, 15.031, 16.032, 17.289,
  18.289, 19.015, 20.015, 21.292, 22.292, 23.212, 24.212, 25.249, 26.249,
  26.931, 27.931, 29.186, 30.186, 30.987, 31.987, 33.254, 34.254, 35.030,
  36.030, 37.236, 38.236, 39.124, 40.124, 41.223, 42.224, 43.015, 44.015,
  45.216, 46.216, 47.151, 48.151, 49.121, 50.121, 50.957, 51.957, 53.171,
  54.171, 55.063, 56.064, 57.133, 58.133, 58.944, 59.945, 61.165, 62.165,
  63.016, 64.016, 65.145, 66.145, 66.973, 67.973, 69.155, 70.155, 71.051,
  72.051, 73.121, 74.121, 74.971, 75.971, 77.097, 78.097, 78.866, 79.866,
  81.147, 82.147, 82.988, 83.988, 85.132, 86.132, 87.049, 88.049, 89.115,
  90.115, 90.987, 91.987, 93.169, 94.169, 95.011, 96.011, 97.107, 98.107,
  98.985, 99.985,101.114,102.114,103.001,104.001,105.089,106.089,106.965,
 107.966,109.052,110.052,110.740,111.740,113.070,114.070,114.980,115.981,
 117.063,118.064,119.009,120.010,121.033,122.034,122.974,123.975,125.045,
 126.045,126.956,127.956,129.075,130.075,131.015,132.016,133.081,134.081,
 134.940,135.940,137.043,138.044,138.986,139.986,141.031,142.031,142.682,
 143.682,145.065,146.066,146.995,147.995,149.049,150.050,150.946,151.946,
 153.006,154.006,154.994,155.994,157.003,158.003,159.053,160.053,161.038,
 162.038,162.999,164.000,165.034,166.034,166.888,167.888,168.988,169.988,
 170.988,171.988,173.027,174.027,174.615,175.615,177.027,178.028,179.001,
 180.001,181.013,182.013,182.896,183.896,185.036,186.036,186.963,187.963,
 188.946,189.946,190.932,191.932,192.963,193.963,195.007,196.007,196.932,
 197.932,198.840,199.840,200.892,201.892,202.974,203.974,204.910,205.720,
 206.215,207.406,208.926,209.926,210.974,211.974,212.923,213.923,214.833,
 215.833,216.881,217.881,218.965,219.965,220.883,221.883,223.089,224.089,
 224.919,225.919,226.971,227.971,228.893,229.893,230.812,231.812,232.853,
 233.853,234.951,235.951,236.868,237.548,237.954,239.274,240.889,241.889,
 242.974,243.974,244.917,245.917,246.812,247.812,248.865,249.865,250.940,
 251.940,253.000,254.000,255.000
}};
  int i,ix;

	/* Point to correct table:			*/
	/*   weight[0] = full-frame 10K or 40K		*/
	/*   weight[1] = full-frame 100K		*/
	/*   weight[2] = summation-mode 10K or 40K 	*/
	/*   weight[3] = summation-mode 100K or 400K 	*/
    	/*   igain: 1=400K, 2=100K, 3=40K, 4=10K 	*/ 

  if (sflag == 1) {	/* full-frame */
     if (igain>2) ix=0; /* 10K or 40K */
     else ix=1;		/* 100K */
  }
  else {		/*summation mode */
     if (igain>2) ix=2; /* 10K or 40K */
     else ix=3;		/* 100K or 400K */
  }

  for (i=0; i<256; i++) wght[i]=weight[ix][i];
} /* end subroutine bitwght */


/*****************************************************************************/
/*    Calculate "difference" histogram for each line input.                  */
/*****************************************************************************/
static void calc_diff_hist(hist,ss1,es1,ss2,es2,pix)
int hist[512],ss1,es1,ss2,es2;
unsigned char pix[800];
{
  int i;

  for (i=ss1; i<es1; i++) hist[pix[i]-pix[i-1]+255]++;
  hist[511] += es1 - ss1;

  if (ss2 != 0) {
     for (i=ss2; i<es2; i++) hist[pix[i]-pix[i-1]+255]++;
     hist[511] += es2 - ss2;
  }
  return;
}
/*****************************************************************************/
/* Calculate image entropy from difference histogram.                        */ 
/*****************************************************************************/
static void calculate_entropy(hist,entropy)
int hist[512];
float *entropy;
{
  double total_ns,pj,log_2;
  int i;

  log_2 = log10((double)2.0);
  total_ns = (double) hist[511];
  *entropy = 0.0;
  if (hist[511]<=0) return;
  
  for (i=0; i<511; i++) {
     if (hist[i]) {
	pj = (double)hist[i]/total_ns;
	*entropy = *entropy - pj*(log10(pj)/log_2);
     }
  }
}
/*****************************************************************************/
/* Check camera parameters between image and cal files for consistency.      */ 
/*****************************************************************************/
static int check_camera_parameters(ifilt,cfilt,bfilt,igain,cgain,
				   dc_gain,bgain,ifr,dc_fr,brate)
int ifilt,cfilt,bfilt;		/* filters for image, cal, blemish */
int igain,cgain,dc_gain,bgain;  /* gain states for image cal, dc, blem */
int ifr,dc_fr,brate;		/* frame rates for image, dc, blem */
{
  int ind;
  char msg[80];

  zvmessage(" ","");
  zvmessage("              PIC   CAL   DC   BLM","");
  zvmessage("              ===   ===   ==   ===","");
  sprintf(msg," FILTER       %3d   %3d    X   %3d",
	 ifilt,  /* Image Filter */
	 cfilt,  /* CAL Filter   */
	 bfilt); /* BLEM Filter  */
  zvmessage(msg,"");
  sprintf(msg," GAIN         %3d     X   %2d   %3d",igain,dc_gain,bgain);
  zvmessage(msg,"");
  sprintf(msg," FRAME-RATE   %3d     X  %3d     X",ifr,dc_fr);
  zvmessage(msg,"");
  zvmessage(" ","");

  ind = 0;
  if (ifilt!=cfilt || ifilt!=bfilt) {
     zvmessage(" *** WARNING: image/calibration Filter-Position mismatch","");
     ind = 1;
  }
  if (igain!=bgain || igain!=dc_gain) {
     zvmessage(" *** WARNING: image/calibration Gain-State mismatch","");
     ind = 1; 
  }
  if (ifr!=dc_fr) {
     zvmessage(" *** WARNING: image/calibration Frame-Rate mismatch","");
     ind = 1;
  }
  if (ind) {
     if (!zvptst("NOCHECK")) return(0);
     zvmessage(" *** Continuing with unmatched files","");
  }
  return(1);
} /* end subroutine check_camera_parameters */


/************************************************************************
 * Copy binary labels from input UDR to ouput EDR.  Also, add the	*
 * following to the telemetry header: cal_filename, dc_filename, a1, a2.*
 ************************************************************************/
static void copy_binary_headers()
{
  int i,j,line,ss,ns,nobjs;
	/* Copy telemetry header records from input to output image and
           add calibration file names. */
  if (mission_phase == PHASE1) {
     get_ssi_telem_hdr(iunit,&tlm_hdr);	/* Read telemetry header */
     sprintf(msg,"%8.4f",a1);
     memcpy(tlm_hdr.scale[0],msg,8);
     sprintf(msg,"%8.4f",a2);
     memcpy(tlm_hdr.scale[1],msg,8);
     memcpy(tlm_hdr.slope,gll.rad_fn,20);
     memcpy(tlm_hdr.offset,gll.dc_fn,20);
     write_ssi_telem_hdr(ounit,&tlm_hdr); /* Write telemetry header */
  }

  if (mission_phase == PHASE2) { 
     get_ssi_ph2_telem_hdr(iunit,&ph2_tlm_hdr); /* Read telemetry hdr */
     sprintf(msg,"%8.4f",a1);
     memcpy(ph2_tlm_hdr.scale_fact[0],msg,8);
     sprintf(msg,"%8.4f",a2);
     memcpy(ph2_tlm_hdr.scale_fact[1],msg,8);
     memcpy(ph2_tlm_hdr.slope_file,gll.rad_fn,20);
     memcpy(ph2_tlm_hdr.offset_file,gll.dc_fn,20);
     write_ssi_ph2_telem_hdr(ounit,&ph2_tlm_hdr); /* Write telmtry hdr */
  }
  nlbout = sflag;
	/* Copy bad-data label records (if present) */
  for (j=3; j<=nlb; j++) {	/* Start from third record */
     get_ssi_bdv_hdr_rec(iunit,j,&ddo);
     if (sflag==1 || ddo.code!=2) {
        write_ssi_bdv_hdr_rec(ounit,++nlbout,&ddo);
        continue;
     }
     nobjs = 0;		/* clean up summation-mode mess */
     for (i=0; i<ddo.nobjs; i++) {
         line = ddo.coords.line_data[i].line;
         if (line > 400) break;
         ss = ddo.coords.line_data[i].ss;
         ns = ddo.coords.line_data[i].ns;
         if (ss > 400) continue;
         if (ss+ns > 401) ns=401-ss;
         ddo.coords.line_data[nobjs].line = line;
         ddo.coords.line_data[nobjs].ss = ss;
         ddo.coords.line_data[nobjs++].ns = ns;
     }
     if (nobjs == 0) continue;
     if (nobjs < ddo.nobjs) {
        for (i=nobjs; i<ddo.nobjs; i++) {
            ddo.coords.line_data[i].line = 0;
            ddo.coords.line_data[i].ss = 0;
            ddo.coords.line_data[i].ns = 0;
        }
     }
     ddo.nobjs = nobjs;
     write_ssi_bdv_hdr_rec(ounit,++nlbout,&ddo);
  }
}
/************************************************************************
 * Decalibrate image.							*
 ************************************************************************/
int decalibrate_image()
{
typedef struct {	/* Phase 1 UDR line record */
  ssi_lhdr_typ prefix;	       
  UBYTE	pixels[800];
} in_line_typ; 
		/* UDR line record */
in_line_typ  in_image;		/* Phase 1 */
ssi_line_typ in_ph2_image;	/* Phase 2 */
		/* Calibration data */
float cal_buf[800];		/* radiometric file record	*/
UBYTE dc_buf1[800];		/* dark-current record, dc_fmt=1 */
short dc_buf[800];		/* dark-current record, dc_fmt=2 */

UBYTE *ipixels;			/* Pointer to input pixels */
short *previous_line;		/* output pixels of previous line */
short *current_line;		/* output pixels of current line  */ 
short *next_line;		/* output pixels of next line     */ 

int line;
int ss1,es1,ss2,es2;		/* starting and ending samples of 2 segments */
float t;			/* exposure time in msec */
float oscale;			/* output DN per Ft-Lambert msec*/
int icnt,idef,errmsg,ind;

errmsg = 0;
zvparm("EXPOSURE",&t,&icnt,&idef,0,0);
t=gll.exposure;
sprintf(msg," Exposure = %.4e msec",t);
zvmessage(msg,"");


if (mission_phase==PHASE2 && nlb>0) {
   previous_line = out_ph2_image[0].pixels;
   current_line = out_ph2_image[0].pixels;
   ipixels = in_ph2_image.pixels;
}
else {	/* Phase 1 or non-UDR format */
   previous_line = out_image[0].pixels;
   current_line = out_image[0].pixels;
   ipixels = in_image.pixels;
}

current_blem = blemishes;
last_blem = blemishes + nblem - 1;
zap_index = blemishes;

	/* Radiometric and blemish correction of each line */
for (line=1; line<=nl; line++) {
   if (nlb == 0) {	/* Read non-UDR image */
      next_line = out_image[line-1].pixels;
      zvread(iunit,in_image.pixels,NULL);
      ss1 = 1;
      es1 = ns;
      ss2 = es2 = 0;
   }
   else if (mission_phase == PHASE1) {	/* Read phase 1 UDR */
      next_line = out_image[line-1].pixels;
      get_gll_udr_ln(iunit,line,&in_image);
      memcpy(&out_image[line-1],&in_image,sizeof(ssi_lhdr_typ));
      ss1 = 1;
      if (in_image.prefix.rs_overflow != 0) {
	 add_segment(&rso,line,1,ns,&nlbout,ounit);
         es1 = 0;		/* entire line is invalid */
      }
      else es1=in_image.prefix.last_pix;
      if (es1 == 0) ss1=0;
      if (es1 > ns) es1=ns;
      ss2 = es2 = 0;
   }
   else {	/* Read Phase 2 UDR */
      next_line = out_ph2_image[line-1].pixels;
      get_gll_ph2_udr_ln(iunit,line,&in_ph2_image);
      memcpy(&out_ph2_image[line-1],&in_ph2_image,sizeof(ssi_prefix_typ));
      ss1 = in_ph2_image.prefix.ss1;
      es1 = in_ph2_image.prefix.es1;
      ss2 = in_ph2_image.prefix.ss2;
      es2 = in_ph2_image.prefix.es2;
      ind = 1;		/* check for invalid segment field */
      if (ss1<0 || ss2<0) ind=0;
      if (es1>ns || es2>ns) ind=0;
      if (es1<ss1 || es2<ss2) ind=0;
      if (ss2>0 && ss2<es1) ind=0;
      if (ind==0) {	/* if error detected, distrust field */
         if (errmsg==0) zvmessage(" ***Segment field is invalid",NULL);
         errmsg = 1;
         ss1 = 1;
         es1 = ns;
         ss2 = 0;
         es2 = 0;
      }
   }

	/* Add data gaps and RS errs to BDV records */
   if (nlb>0 && badlabel==0) {
      if (mission_phase==1 && in_image.prefix.rs_overflow!=0) {
	 add_segment(&rso,line,1,ns,&nlbout,ounit) ;
	 es1 = 0;		/* entire line is invalid */
      }
      find_bdv(ipixels,line,ss1,es1,ss2,es2,&ddo,&sat,&nlbout,ounit);
   }
	/* Generate difference histogram for entropy calculations */
   if (calc_entropy) {
      calc_diff_hist(img_dhist,ss1,es1,ss2,es2,ipixels);
         memset((void *)line_dhist,0,(sizeof(line_dhist)));
      if (line%50==0 && line!=nl) {
	 calc_diff_hist(line_dhist,ss1,es1,ss2,es2,ipixels);
         calculate_entropy(line_dhist,&entropies[line/50-1]);
      }
   }
 
   if (blemcor==0) {
      if (nlb > 0) zap_lfw_pixels(ipixels,line,
			     ss1,es1,ss2,es2,&nlbout,&lfw,ounit);
      else zap_lfw_pixels(ipixels,line,ss1,es1,ss2,es2,0,0,-1);
   }

   zvread(cal_unit,cal_buf,NULL);    /* read calibration data for line */
   oscale = scale/(t - t0[sflag*(line-1)]);
   if (dc_fmt==1) {
      zvread(dc_unit,dc_buf1,NULL);
      decal1(next_line,ipixels,dc_buf1,cal_buf,
		wght,oscale,ss1,es1,ss2,es2);
   }
   else {
      zvread(dc_unit,dc_buf,NULL); 
      decal2(next_line,ipixels,dc_buf,cal_buf,
		wght,oscale,dc_scale,ss1,es1,ss2,es2);
   }

   if (line > 1) remove_blemishes(current_line,previous_line,next_line,line-1);
   previous_line = current_line; /* Scroll down one line in the image */
   current_line = next_line;     /* by switching the pointers.	 */
}

	/* end of line loop.  Write out any remaining BDV segments */
  remove_blemishes(current_line,previous_line,next_line,nl);
  if (nlb == 0) return(1);
 
	/* Write remaining Bad Data Value Headers */
  if (badlabel == 0) {
     if (ddo.nobjs > 0) write_ssi_bdv_hdr_rec(ounit,++nlbout,&ddo);
     if (sat.nobjs > 0) write_ssi_bdv_hdr_rec(ounit,++nlbout,&sat);
     if (rso.nobjs > 0) write_ssi_bdv_hdr_rec(ounit,++nlbout,&rso);
  }

  if (blemcor==0 && lfw.nobjs>0) write_ssi_bdv_hdr_rec(ounit,++nlbout,&lfw);
}
/*****************************************************************************/
/* Decalibrate an image line using byte dark-current data.                   */ 
/*****************************************************************************/
static void decal1(
	short obuf[],		/* pointer to output image line		*/
	UBYTE in_buf[],		/* pointer to input image line		*/
	UBYTE dc_buf[],		/* pointer to dark-current for line	*/
	float cal_buf[],	/* pointer to calibration data for line */
	float wght[],		/* uneven bit-weight correction table   */
	float scale,		/* input image scale factor		*/
	int ss1,		/* starting sample, first data segment	*/
	int es1,		/* ending sample, first data segment	*/
	int ss2,		/* starting sample, second data segment */
	int es2)		/* ending sample, second data segment	*/
{
  int i,j,es;
  double d1,dn;

  d1 = scale;
	/* Fill first gap with invalid pixels (if it exists) */
  if (ss1 < 1) {
     for (i=0; i<ns; i++) obuf[i]=LOWEST_DN;
     return;
  }

  for (i=0; i<ss1; i++) obuf[i]=LOWEST_DN;
	/* Decalibrate first data segment */
  for (i=ss1-1; i<es1; i++) {
     dn = d1*(cal_buf[i]*(wght[in_buf[i]]-wght[dc_buf[i]]));
     if (dn < 0) {
	if (dn<LOWEST_DN) dn=LOWEST_DN;	/* If sample is negative, */
	obuf[i] = dn - 0.5;		/* round down.		  */
     }
     else {
	if (dn>HIGHEST_DN) dn=HIGHEST_DN;
	obuf[i] = dn + 0.5;		/* else, round up.	  */
     }
  }

	/* Fill second gap with invalid pixels (if it exists) */
  es = ns;
  if (ss2 > 0) es=ss2;
  for (i=es1; i<es; i++) obuf[i]=LOWEST_DN;
  if (ss2 == 0) return;
	/* Decalibrate second data segment */
  for (i=ss2-1; i<es2; i++) {
     dn = d1*(cal_buf[i]*(wght[in_buf[i]]-wght[dc_buf[i]]));
     if (dn < 0) {
	if (dn < LOWEST_DN) dn=LOWEST_DN;
	obuf[i] = dn - 0.5;
     }
     else {
	if (dn > HIGHEST_DN) dn=HIGHEST_DN;
	obuf[i] = dn + 0.5;
     }
  }
	/* Fill third gap with invalid pixels (if it exists) */
  for (i=es2; i<ns; i++) obuf[i]=LOWEST_DN;
}  /* end subroutine decal1 */


/*****************************************************************************/
/* Decalibrate an image line using halfword dark-current data.               */ 
/*****************************************************************************/
static void decal2(
  short	buf[],		/* pointer to output image line		*/
  UBYTE	in_buf[],	/* pointer to input image line		*/
  short	dc_buf[],	/* pointer to dark-current for line	*/
  float	cal_buf[],	/* pointer to calibration data for line */
  float	wght[],		/* uneven bit-weight correction table   */
  float	scale,		/* input image scale factor		*/
  float	dc_scale,	/* dc scale factor			*/
  int ss1,		/* starting sample, first data segment	*/
  int es1,		/* ending sample, first data segment	*/
  int ss2,		/* starting sample, second data segment */
  int es2)		/* ending sample, second data segment	*/
{
  int i,j,es;
  double d1,d2,dn;

  d1 = scale;
  d2 = dc_scale;
	/* Fill first gap with invalid pixels (if it exists) */
  if (ss1 < 1) {
     for (i=0; i<ns; i++) buf[i]=LOWEST_DN;
     return;
  }

  for (i=0; i<ss1; i++) buf[i]=LOWEST_DN;
  if (ss1 == 0) return;
	/* Decalibrate first data segment */
  for (i=ss1-1; i<es1; i++) {
     dn = d1*(cal_buf[i]*(wght[in_buf[i]]-d2*dc_buf[i]));
     if (dn < 0) {
	if (dn < LOWEST_DN) dn=LOWEST_DN;
	  buf[i] = dn - 0.5;
     }
     else {
	if (dn > HIGHEST_DN) dn=HIGHEST_DN;
	buf[i] = dn + 0.5;
     }
  }
	/* Fill second gap with invalid pixels (if it exists) */
  es = ns;
  if (ss2 > 0) es=ss2;
  for (i=es1; i<es; i++) buf[i]=LOWEST_DN;
  if (ss2 == 0) return;
	/* Decalibrate second data segment */
  for (i=ss2-1; i<es2; i++) {
     dn = d1*(cal_buf[i]*(wght[in_buf[i]]-d2*dc_buf[i]));
     if (dn < 0) {
	if (dn < LOWEST_DN) dn=LOWEST_DN;
	  buf[i] = dn - 0.5;
     }
     else {
	if (dn > HIGHEST_DN) dn=HIGHEST_DN;
	buf[i] = dn + 0.5;
     }
    }
	/* Fill third gap with invalid pixels (if it exists) */
  if (ss2) for (i=es2; i<ns; i++) buf[i]=LOWEST_DN;
	/* Decalibrate second data segment */
} /* end subroutine decal2 */


/*****************************************************************************/
/* Get blemishes from Blemish File.  Returns blemish file name, blemish data,*/
/* and number of blemishes.						     */
/*****************************************************************************/
static int getblems(blem_filename,blemishes,nblem,fltr,gain,rate,gll)
char *blem_filename;		/* Blemish file specification   */
blem_typ *blemishes;		/* Blemish data			*/
int *nblem;			/* Number of blemishes		*/
int *fltr,*gain,*rate;
able_86_typ *gll;
{
  int ind,icnt,idef,count,blem_unit,nlx,nsx,qver;
  int type = 3;               /* BLM file type */
  char format[5];
  char qpath[256],qname[121],qext[11];
  char filespec[371];
  able_86_typ blem_able;
  char *tempfilename;

  memset(qpath,'\0',strlen(qpath));
  memset(qname,'\0',strlen(qname));
  memset(qext,'\0',strlen(qext));
  memset(filespec,'\0',strlen(filespec));

  if (g1!=0  && g4==0) {
     strcpy(dirblm,dirdefault);  
     g4 = 1;
  }

  zvparm("BLEM",gll->blem_fn,&icnt,&idef,0,0);
  if (idef!=1) {	/* if specified, check syntax */
     if (strchr(gll->blem_fn,':') || strchr(gll->blem_fn,'/')) { 
        zvmessage(" ***Directory name not allowed in BLEM","");
        return(0);
     }
  }
  else ind=zgllcalname(gll->blem_fn,
	&type,&gll->fltr,&gll->frm_rate,&gll->gain,&gll->mofibe,&gll->rmode);

  strcpy(filespec,dirblm);
  strcat(filespec,gll->blem_fn);
	/* Convert Unix file spec to host file spec */
  tempfilename = (char *) filename_for_host(filespec);
  strcpy(blem_filename,tempfilename);
  ind=zvunit(&blem_unit,"B",1,"U_NAME",blem_filename,NULL);
  zvsignal(blem_unit,ind,1);
  ind=zvopen(blem_unit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  ind=zvget(blem_unit,"FORMAT",format,"NL",&nlx,"NS",&nsx,NULL);

  blem_able.lbl_typ = sizeof(able_86_typ)/4;
  zable86(&ind,blem_unit,&blem_able);

  *nblem = nsx/4;
  if (strcmp(format,"WORD")!=0 && strcmp(format,"HALF")!=0) goto INVALID_FILE;
  if (*nblem*4 != nsx) goto INVALID_FILE;
  if (*nblem > MAX_BLEMS) goto WAY_TOO_MANY;
  *fltr = blem_able.fltr;
  *gain = blem_able.gain;
  *rate = blem_able.frm_rate;
  zvread(blem_unit,blemishes,NULL);
  /* trans_blem(blem_unit,*nblem,blemishes); */
  zvclose(blem_unit,NULL);
  return(1);

INVALID_FILE:
  zvmessage(" *** Invalid Blemish File","");
  return(0);
WAY_TOO_MANY:
  zvmessage(" *** Invalid Blemish File ... Too many blemish entries","");
  return(0);
} /* end subroutine getblems */


/*****************************************************************************
 * Return last value (VALUE) of a label item (KEY)			     *
 * Outputs: VALUE, IND							     *
 * Upon return, IND=1 if item is found, =0 otherwise.                        *
 *****************************************************************************/
static void getlabval(int iunit,char *key,int *value,int *ind)
{
  char tasks[25][8];
  int instances[25],icnt=25,lc;

  *ind=zlhinfo( iunit, (char *)tasks, instances, &icnt, NULL);

  for (lc=icnt-1; lc>=0; lc--) {
     *ind=zlget(iunit,"HISTORY",key,value,"FORMAT","INT",
		"HIST",&tasks[lc][0],"INSTANCE",instances[lc],NULL);
    if (*ind == 1) return;
  }

  *ind = 0;
  return;
} /* end getlabval */


/*****************************************************************************
 * Read shutter-offset file and return values in offsets array.              * 
 * Return status=1 if success, =0 if failed.				     *
 * Upon return, IND=1 if item is found, =0 otherwise.                        *
 *****************************************************************************/
static int getoffsets(offset_filename,offsets,gll)
     char offset_filename[64];	/* input shutter-offset filename */
     float *offsets;		/* output line-dependent shutter-offsets*/
     able_86_typ *gll;
{
  int ind,icnt,idef,count,offset_unit,nlx,nsx,qver;
  int type=4;                   /* SO file type */
  char format[5];
  char qpath[256],qname[121],qext[11];
  char filespec[371];
  char *tempfilename;

  memset(qpath,'\0',strlen(qpath));
  memset(qname,'\0',strlen(qname));
  memset(qext,'\0',strlen(qext));
  memset(filespec,'\0',strlen(filespec));

  if (g1!=0 && g5==0) {
     strcpy(diroff,dirdefault);  
     g5 = 1;
  }

  zvparm("OFFSETS",gll->so_fn,&icnt,&idef,0,0);
  if (idef != 1) {	/* if specified, check syntax */
     if (strchr(gll->so_fn,':') || strchr(gll->so_fn,'/')) { 
        zvmessage(" *** Directory name NOT allowed in OFFSETS","");
        return(0);
     }
  }
  else ind=zgllcalname(gll->so_fn,&type,
	&gll->fltr,&gll->frm_rate,&gll->gain,&gll->mofibe,&gll->rmode);

  strcpy(filespec,diroff);
  strcat(filespec,gll->so_fn);
	/* Convert Unix file spec to host file spec */
  tempfilename = (char *) filename_for_host(filespec);
  strcpy(offset_filename,tempfilename);
  ind=zvunit(&offset_unit,"W",1,"U_NAME",offset_filename,NULL);
  zvsignal(offset_unit,ind,1);
  ind=zvopen(offset_unit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  ind=zvget(offset_unit,"FORMAT",format,"NL",&nlx,"NS",&nsx,NULL);
  if (strcmp(format,"REAL")!=0) goto INVALID_FILE;
  if (nlx != 1) goto INVALID_FILE;
  if (nsx != 800) goto INVALID_FILE;
  zvread(offset_unit,offsets,NULL);
  zvclose(offset_unit,NULL);
  return(1);

INVALID_FILE:
  zvmessage(" *** Invalid Shutter-Offset File","");
  return(0);
} /* end get_offsets */


/****************************************************************************
 * Find data drop-outs and saturated pixels and record as bad-data value    *
 * records.                                                                 *
 ****************************************************************************/
static void find_bdv(ipixels,line,ss1,es1,ss2,es2,ddo,sat,nlbout,ounit)
UBYTE ipixels[800];
int line,ss1,es1,ss2,es2,*nlbout,ounit;
ssi_bdvh_typ *ddo,*sat;
{
  int s,samp;

	/* Data Drop Outs (DDOs) */
  if (ss1 == 0) add_segment(ddo,line,1,ns,nlbout,ounit);
  if (ss1 > 1) add_segment(ddo,line,1,ss1-1,nlbout,ounit);
  if (ss1>0 && es1<ns) {
     if (ss2 == 0) add_segment(ddo,line,es1+1,ns,nlbout,ounit);
     else {
        add_segment(ddo,line,es1+1,ss2-1,nlbout,ounit);
        if (es2 < ns) add_segment(ddo,line,es2+1,ns,nlbout,ounit);
     }
  }
  if (ss1 == 0) return; 
	/* Scan for saturated pixels in first data segment */
  for (samp=ss1; samp<=es1; samp++) {
     s = samp; 
     while (s <= es1 && (ipixels[s-1]==0 || ipixels[s-1]==255)) s++;
     if (s > samp) {
	add_segment(sat,line,samp,s-1,nlbout,ounit);
	samp = s;
     }
  }
 
  if (ss2 == 0) return;
	/* Scan for saturated pixels in second data segment */
  for (samp=ss2; samp<=es2; samp++) { 
     s = samp; 
     while (s <= es1 && (ipixels[s-1]==0 || ipixels[s-1]==255)) s++;
     if (s > samp) {
	add_segment(sat,line,samp,s-1,nlbout,ounit);
	samp = s;
     }
  }
} /* end subroutine find_bdv */


/************************************************************************
 * Open all calibration files and read in blemish and shutter-offset    *
 * data.  The following global variables are set:			*
 *	dircal,dirdc,dirblm,diroff					*
 *	cal_filename,dc_filename,blem_filename,offset_filename		*
 *	cal_unit,dc_unit,dc_fmt,dc_scale				*
 *	blemishes,nblem,t0						*
 *	scale,scale_type						*
 *	gll.ubwc, wght							*
 ************************************************************************/
static int open_cal_files()
{
  int ifilt;			/* filter position (0-7)	*/
  int igain;			/* 1=400K, 2=100K, 3=40K, 4=10K	*/
  int ifr;			/* 1=2 1/3, 2=8 2/3, 3=30 1/3,  */
                                /* 4=60 2/3, 5=15 1/6         	*/
  int cfilt,cgain;		/* radiometric file filter,gain */
  int dc_gain,dc_fr;		/* dark-current file frame rate */
  int bfilt,bgain,brate;	/* blemish file filter,gain,frame rate */
  int i,ind,icnt,idef;
  char *tempdir;

	/* Get input image camera parameters */
  zvparm("FILTER",&ifilt,&icnt,&idef,0,0);
  if (idef==1) ifilt=gll.fltr;
  zvparm("GAIN",&igain,&icnt,&idef,0,0);
  if (idef==1) igain=gll.gain;
  ifr = gll.frm_rate;

	/* Set uneven-bit-weight correction table */
  if (zvptst("UBWC")) {
     bitwght(igain,sflag,wght);		/* Get bit-weight table */
     gll.ubwc = TRUE;
  }
  else {				/* else use identity transform */
     for (i=0; i<256; i++) wght[i]=i;
     gll.ubwc = FALSE;
  }
	/* Get calibration file directory specifications */
  zvp("DIR",dirdefault,&g1);	/* Default directory */
  zvp("DIRCAL",dircal,&g2);	/* radiometric file directory */
  zvp("DIRDC",dirdc,&g3);	/* dark-current file directory */
  zvp("DIRBLM",dirblm,&g4);	/* blemish file directory */
  zvp("DIROFF",diroff,&g5);	/* Shutter-offset file directory  */

/*
  if (g1==0 && (g2==0 || g3==0 || g4==0 || g5==0)) {
     zvmessage("***Please specify calibration file directories","");
     zvmessage("***See parameters DIR, DIRCAL, DIRDC, ...","");
     return(0); 
  }
*/
  memset(cal_filename,0,sizeof(cal_filename));
  memset(dc_filename,0,sizeof(dc_filename));
  memset(blem_filename,0,sizeof(blem_filename));
  memset(offset_filename,0,sizeof(offset_filename));

	/* Open cal files and get associated camera parameter info */
  ind = opencal(&cal_unit,cal_filename,&cfilt,&cgain,&gll);
  if (!ind) return(0);
  ind = opendc(&dc_unit,dc_filename,&dc_gain,&dc_fr,&dc_scale,&dc_fmt,&gll);
  if (!ind) return(0); 
  ind = getblems(blem_filename,blemishes,&nblem,&bfilt,&bgain,&brate,&gll);
  if (!ind) return(0);
  ind = getoffsets(offset_filename,t0,&gll);
  if (!ind) return(0);
	/* Check for mismatch between input image and cal files */
  ind = check_camera_parameters(ifilt,cfilt,bfilt,igain,cgain,dc_gain,bgain,
              ifr,dc_fr,brate);
  if (!ind) return(0);
	/* Compute output radiometric scale */
  ind = opscale(ifilt,igain,cgain,&a1,&a2,&scale,&scale_type,&gll);
  return(1);
}


/****************************************************************************
 * Open Radiometric file and return unit number and filename.               *
 ****************************************************************************/
static int opencal(cal_unit,cal_filename,cfilt,cgain,gll)
  int	*cal_unit;	/* logical unit # of dark-current file	*/
  char	*cal_filename;	/* calibration file specification	*/
  int	*cfilt;		/* calibration file filter position	*/
  int	*cgain;		/* calibration file gain-state		*/
  able_86_typ	*gll;
{
  int	ind,icnt,idef,nlx,nsx,qver;
  int   type = 1;       /* CAL file type */
  char	format[5];
  char  qpath[256], qname[121], qext[11];
  char  filespec[371];
  able_86_typ	rad_able;
  char  *tempfilename;

  memset(qpath,'\0',strlen(qpath));
  memset(qname,'\0',strlen(qname));
  memset(qext,'\0',strlen(qext));
  memset(filespec,'\0',strlen(filespec));

  if (g1!=0 && g2==0) {
     strcpy(dircal,dirdefault); 
     g2 = 1;
  }
	/* Get user specified Radiometric filename */
  zvparm("CAL",gll->rad_fn,&icnt,&idef,0,0);
  if (idef!=1) {	/* If specified, check syntax */
     if (strchr(gll->rad_fn,':') || strchr(gll->rad_fn,'/')) { 
        zvmessage(" ***Directory name NOT allowed in CAL","");
        return(0);
     }
  }	/* if not specified, construct it from camera parameters */
  else ind=zgllcalname(gll->rad_fn,&type,
	&gll->fltr,&gll->frm_rate,&gll->gain,&gll->mofibe,&gll->rmode);

	/* Create Unix filespec and convert to host's filespec */
  strcpy(filespec,dircal);
  strcat(filespec,gll->rad_fn);
  tempfilename = (char *) filename_for_host(filespec);
  strcpy(cal_filename,tempfilename);

  ind = zvunit(cal_unit,"X",1,"U_NAME",cal_filename,NULL);
  zvsignal(*cal_unit,ind,1);
  ind = zvopen(*cal_unit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  ind = zvget(*cal_unit,"FORMAT",format,"NL",&nlx,"NS",&nsx,NULL);
  if (strcmp(format,"REAL") != 0) goto INVALID_FILE;
  if ((nlx != nl) || (nsx != ns)) goto INVALID_FILE;

	/* Get camera parameters from label and user parameters */
  rad_able.lbl_typ = sizeof(able_86_typ)/4;
  zable86(&ind,*cal_unit,&rad_able);

  zvparm("CFILTER",cfilt,&icnt,&idef,0,0);
  if (idef==1) *cfilt=rad_able.fltr;

  zvparm("CGAIN",cgain,&icnt,&idef,0,0);
  if (idef==1) *cgain=rad_able.gain;
  return(1);

INVALID_FILE:
  zvmessage(" *** Invalid Radiometric File","");
  return(0);
} /* end opencal */


/*****************************************************************************
 * Open dark-current file and return unit number, filename		     * 
 * and scale.								     *	
 *****************************************************************************/
static int opendc(dc_unit,dc_filename,dc_gain,dc_fr,dc_scale,dc_fmt,gll)
  int	*dc_unit;	/* logical unit # of dark-current file	*/
  char	*dc_filename;	/* dark-current file specification	*/
  int	*dc_gain;	/* dark-current gain-state		*/
  int	*dc_fr;		/* dark-current frame-rate		*/
  float	*dc_scale;	/* output dc scale factor		*/
  int	*dc_fmt;	/* dc format (1=byte, 2=half)		*/
  able_86_typ *gll;
{
  int	ind,icnt,idef,iscale,nlx,nsx,qver;
  int type = 2;         /* DC file type */
  char format[5];
  char qpath[256],qname[121],qext[11];
  char filespec[371];
  able_86_typ	dc_able;
  char *tempfilename;

  memset(qpath,'\0',strlen(qpath));
  memset(qname,'\0',strlen(qname));
  memset(qext,'\0',strlen(qext));
  memset(filespec,'\0',strlen(filespec));

  if (g1!=0  && g3==0) {
     strcpy(dirdc,dirdefault);  
     g3 = 1;
  }

  zvparm("DC",gll->dc_fn,&icnt,&idef,0,0);
  if (idef!=1) {	/* if specified, check syntax */
     if (strchr(gll->dc_fn,':') || strchr(gll->dc_fn,'/')) {
        zvmessage(" *** Directory name not allowed in DC ","");
        return(0);
     }
  }	/* else if not specified, construct it from camera parameters */
  else ind=zgllcalname(gll->dc_fn,&type,
	&gll->fltr,&gll->frm_rate,&gll->gain,&gll->mofibe,&gll->rmode);

  strcpy(filespec,dirdc);
  strcat(filespec,gll->dc_fn);
     /* Convert UNIX file spec to host file spec */
  tempfilename = (char *) filename_for_host(filespec);
  strcpy(dc_filename,tempfilename);
  ind=zvunit(dc_unit,"Y",1,"U_NAME",dc_filename,NULL);
  zvsignal(*dc_unit,ind,1);
  ind=zvopen(*dc_unit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  ind=zvget(*dc_unit,"FORMAT",format,"NL",&nlx,"NS",&nsx,NULL);
  if (strcmp(format,"WORD")!=0
      && strcmp(format,"HALF")!=0
      && strcmp(format,"BYTE")!=0) goto INVALID_FILE;
/*  if (nlx != nl  || nsx != ns) goto INVALID_FILE; */
/* summation-mode dc are 800x800 for no good reason 6-18-93 */

  dc_able.lbl_typ = sizeof(able_86_typ)/4;
  zable86(&ind,*dc_unit,&dc_able);

  zvparm("DCGAIN",dc_gain,&icnt,&idef,0,0);
  if (idef==1) *dc_gain=dc_able.gain;

  *dc_fr = dc_able.frm_rate;
  getlabval(*dc_unit,"PICSCALE",&iscale,&ind);
  if (ind != 1) {
     if (strcmp(format,"BYTE") != 0)  goto AMBIGUOUS_FILE;
     iscale = 1;
  }

  *dc_scale = 1.0/iscale;
  if (strcmp(format,"BYTE")==0) *dc_fmt=1;
  else *dc_fmt=2;
  return(1);

INVALID_FILE:
  zvmessage(" *** Invalid Dark-Current File","");
  return(0);
AMBIGUOUS_FILE:
  zvmessage(" *** Scale of DC file is ambiguous","");
  return(0);
} /* end opendc */


/************************************************************************/
/* Open input image and return camera parameters and processing history */
/* All arguments are outputs.						*/
/************************************************************************/
static int openinp(iunit,badlabel,blemcor,sflag,nl,ns,nlb,
	max_objs,obj_size,calc_entropy,gll,mission_phase)
int *iunit;		/* input image logical unit #	*/
int *badlabel;		/* =1 if BADLABELS has been run */
int *blemcor;		/* =1 if GLLBLEMCOR has been run */
int *sflag;		/* =2 for summation, =1 for full-frame */
int *nl,*ns;		/* number of lines and samples	*/
int *nlb;		/* number of binary header records */
int *max_objs;		/* max number of objects in BDV record */
int *obj_size;		/* byte size of BDV object */
int *calc_entropy;	/* =1 if entropy is to be calculated */
able_86_typ *gll;	/* able86 buffer */
int *mission_phase;	/* 1=ground calibration [removed], 2=phase1, 3=phase2 */
{
  int ind,icnt,i,idef;
  int nbb;			/* number of bytes in binary prefix */
  char format[5];
  char taskname[12][8];
  int instant[12];
  int ntask=12;
  float entr;
  float solrange;
  ssi_bdvh_typ *bdv;
	/* solar ranges: Jupiter, Venus, Earth, Moon, Gaspra, Ida */

  ind = zvunit(iunit,"INP",1, NULL);
  zvsignal(*iunit,ind,1);
  ind = zvopen(*iunit,"COND","BINARY","OPEN_ACT","SA","IO_ACT","SA", NULL);
  ind = zvget(*iunit,"FORMAT",format,"NL",nl,"NS",ns,"NLB",nlb,"NBB",&nbb, NULL);

  gll->lbl_typ = sizeof(able_86_typ)/4;
  zable86(&ind,*iunit,gll);
  *mission_phase = gll->lbl_typ - 1;

        /* Check if BADLABEL and GLLBLEMCOR have been run */ 
  *badlabel = *blemcor = 0;
  zlhinfo( *iunit, (char *)taskname, instant, &ntask, NULL);
  for (i=0; i<ntask; i++) {
      if (!strncmp(taskname[i],"BADLABEL",8)) *badlabel=1;
      else if (!strncmp(taskname[i],"GLLBLEMC",8)) *blemcor=1;
  }

  if (*blemcor==1)  zvmessage(" Blemishes have been removed by GLLBLEMCOR","");
  if (*badlabel==0 && *blemcor==1) {
      zvmessage(" ***Invalid input image","");
      zvmessage(" ***BADLABELS must be run prior to GLLBLEMCOR","");
      return(0);
  }
	/* Check if entropy has been calculated */
  /* CHECK FOR ENTROPY LABEL TAKEN OUT. THE calc_entropy IS SET TO
     TRUE ONLY IF PRINT FLAG IS SPECIFIED OR ENTROPY WERE NOT
     ADDED BY BADLABELS. (RRP)
  ind = zlget(*iunit,"HISTORY","ENTROPY",&entr,"HIST",taskname[0],
   "FORMAT","REAL", NULL);
  if (ind!=1 || zvptst("PRINT")) { */
  if (zvptst("PRINT")) {
     *calc_entropy = TRUE;
     memset((void *)img_dhist,0,(sizeof(img_dhist)));
  }
	/* Summation-mode images are 400x400.  However, an input flight UDR
           is ALWAYS 800x800.  Only the upper-left corner contains image
           data.  The remaining 3/4 of the image is zero filled.  The output
           EDR will be 400x400. */

  if (gll->frm_rate==1 || gll->frm_rate==5) {
     *sflag = 2;	/*summation mode at 2 1/3 sec & 15 1/6 sec */ 
     *max_objs = 165;	/* Max number of objects in BDV record */
  }
  else {
     *sflag = 1;	/* full-frame mode */
     *max_objs = 299;
  }
  *obj_size = sizeof(bdv->coords.line_data);

/*------------------------------------------------------------------------
   Check for valid input image:
   1) must be byte format
   2) must be REDR,
         a) binary header and prefix must exist
         b) size must be 800x800
         c) BADLABELS must have been run
            (Changed:  allow BADLABELS to be missing, since otherwise
            cannot compare with old ADC runs!)
  ------------------------------------------------------------------------*/
  if (strcmp(format,"BYTE")!=0) goto INVALID_FILE;
/* if (*badlabel==0) goto NO_BADLABELS; */
  if (*badlabel==0) zvmessage(" Warning: BADLABELS was not run!","");
  if (*nlb == 0) goto NO_BINARY_LABELS;
  if (nbb == 0) goto NO_BINARY_PREFIX;
  if (*nl != 800 || *ns != 800) goto INVALID_SIZE;  
  if (*sflag==2) *nl= *ns=400;  /* Set output size for summation-mode */

  sclk = gll->frm_nmbr;
  if (sclk<1) goto INVALID_SCLK;
  partition = gll->partition;
  if (partition < 1) partition=1;

  get_solar_range(gll,mission_phase);	/* Put solar range in gll */
  return(1);

INVALID_FILE:
  zvmessage(" ***Invalid input image: not byte data","");
  return(0);
INVALID_SIZE:
  zvmessage(" ***Invalid input image size","");
  return (0);
/*NO_BADLABELS:
  zvmessage(" ***Invalid input image: BADLABELS was not run","");
  return(0);*/
NO_BINARY_LABELS:
  zvmessage(" ***Invalid input image: no binary labels","");
  return(0);
NO_BINARY_PREFIX:
  zvmessage(" ***Invalid input image: no binary prefix","");
  return(0);
INV_BINARY_HEADER:
  zvmessage(" ***Invalid input image: contains binary header","");
  return(0);
INV_BINARY_PREFIX:
  zvmessage(" ***Invalid input image: contains binary prefix","");
  return(0);
INVALID_SCLK:
  zvmessage(" ***Invalid SCLK","");
  return(0);
} /* end openinp */


/* Routine to determine the solar range and store it in gll */
static void get_solar_range(gll,mission_phase)
able_86_typ *gll;                /* able86 buffer */
int *mission_phase;
{
  int icnt,idef,status,i,id,sclk;
  float solrange;
  char user_target[13];
  double buf[100];  
  char planet_name[6][13]={"GASPRA","IDA","VENUS","EARTH"," ","JUPITER"};
  float range[6] = {2.2016,2.9485,0.723331,1.0,0.,5.2};	 /* in AU */

  zvparm("SOLRANGE",&solrange,&icnt,&idef,0,0);
  if (idef != 1) {
     gll->sol_range = solrange;
     sprintf(msg,"Solar range from user parameter=%11.1f\n",gll->sol_range);
     zvmessage(msg,0);
     return;
  }

  if (*mission_phase==PHASE2 && gll->sol_range > 0.0) {
     sprintf(msg,"Solar range from image label=%11.1f\n",gll->sol_range);
     zvmessage(msg,0);
     return;
  }

  zvparm("TARGET",user_target,&icnt,&idef,0,0);
  if (icnt == 1) {
     zuprcase(user_target);
     strcpy(gll->trgt_nm,user_target);
  }
  for (i=0; i<12; i++) if (gll->trgt_nm[i]==' ') gll->trgt_nm[i]=0;

	/* Determine planet-of-encounter from target id */
  status = zpbid(gll->trgt_nm,&id);
  if (status == 1) {
     i = id/100;
     if (id == 9511010) i=0;
     if (id == 2431010) i=1;
  }
  else i = -1;			/* Invalid target name */

  if (i<0 || i==4 || i>5) {
     sclk = gll->frm_nmbr;	/* use SCLK to determine planet-of-encounter*/
     i = 5;				/* default is Jupiter */
     if (sclk <= 207000000) i=1;	/* Ida */
     if (sclk <= 195860000) i=3;	/* Earth */
     if (sclk <= 108000000) i=0;	/* Gaspra */
     if (sclk <= 107000000) i=3;	/* Earth */
     if (sclk <=  19200000) i=2;	/* Venus */
     if (*mission_phase==PHASE2) i=5;	/* Jupiter */
     strcpy(gll->trgt_nm,planet_name[i]);
     zvmessage("***Replacing target with planet-of-encounter",0);
  }

  status = zgetspice3("GLL  ",gll->trgt_nm,1,&gll->frm_nmbr,
	&gll->scet_year,0,buf);
  gll->sol_range = buf[24];
  if (status==1 && buf[24]>0.) {
     sprintf(msg,"Solar range extracted from SPICE=%11.1f\n",gll->sol_range);
     zvmessage(msg,0);
     sprintf(msg,"Target=%s\n",gll->trgt_nm);
     zvmessage(msg,0);
     return;
  }
  
  gll->sol_range = range[i]*KM_PER_AU;
  sprintf(msg,"Solar range copied from table=%11.1f\n",gll->sol_range);
  zvmessage(msg,0);
  strcpy(gll->trgt_nm,planet_name[i]);
  sprintf(msg,"Target=%s\n",gll->trgt_nm);
  zvmessage(msg,0);
  return;
}
/*********************************************************************/
/* Open output image and write various ASCII label items             */
/*********************************************************************/
static int openout(ounit,gll)
  int *ounit;		/* output image logical unit #	*/
  able_86_typ *gll;
{
  int ind,nlbout,nbbout;
  int count,instances,nhist;

  nlbout = nlb;	/* nlbout is unknown at this point and is updated later */

  ind = zvunit(ounit,"OUT",1, NULL);
  zvsignal(*ounit,ind,1);
  if (nlb > 0) ind=zvopen(*ounit,"OP","WRITE","U_FORMAT","HALF","O_FORMAT",
		"HALF","COND","BINARY","U_NLB",nlbout,"U_NBB",200,
		"U_NL",nl,"U_NS",ns,"OPEN_ACT","SA","IO_ACT","SA", NULL);
  else ind=zvopen(*ounit,"OP","WRITE","U_FORMAT","HALF","O_FORMAT","HALF",
		"U_NL",nl,"U_NS",ns,"OPEN_ACT","SA","IO_ACT","SA", NULL);

	/* Add label items */
  nhist = 1;
  ind = zlhinfo( *ounit, (char *)first_task, &instances, &nhist, NULL);
  ind=zladd(*ounit,"HISTORY","DIRCAL",dircal,"FORMAT","STRING", NULL);
  ind=zladd(*ounit,"HISTORY","CAL",gll->rad_fn,"FORMAT","STRING", NULL);
  ind=zladd(*ounit,"HISTORY","DIRDC",dirdc,"FORMAT","STRING", NULL);
  ind=zladd(*ounit,"HISTORY","DC",gll->dc_fn,"FORMAT","STRING", NULL);
  ind=zladd(*ounit,"HISTORY","DIROFF",diroff,"FORMAT","STRING", NULL);
  ind=zladd(*ounit,"HISTORY","SO",gll->so_fn,"FORMAT","STRING", NULL);

	/* Write BLEM information if blemishes are corrected in GALSOS */    
  if (blemcor == 0) {  
     ind=zladd(*ounit,"HISTORY","DIRBLM",dirblm,"FORMAT","STRING", NULL);
     ind=zladd(*ounit,"HISTORY","BLM",gll->blem_fn,"FORMAT","STRING", NULL); 
  }

  if (gll->ubwc) ind=zladd(*ounit,"HISTORY","UBWC","ON","FORMAT","STRING",
	      "HIST",first_task,"MODE","REPLACE", NULL);
  else ind=zladd(*ounit,"HISTORY","UBWC","OFF","FORMAT","STRING",
	      "HIST",first_task,"MODE","REPLACE", NULL);

  ind = zladd(*ounit,"HISTORY","IOF",&gll->iof,"FORMAT","REAL",
        "HIST",first_task,"MODE","REPLACE", NULL);
  ind = zladd(*ounit,"HISTORY","CNV",&gll->cnv,"FORMAT","REAL",
        "HIST",first_task,"MODE","REPLACE", NULL);
  ind = zladd(*ounit,"HISTORY","SOLRANGE",&gll->sol_range,"FORMAT","REAL",
        "HIST",first_task,"MODE","REPLACE","ELEMENT",1, NULL);
  return ind;
} /* end openout */


/************************************************************************
 * Compute radiometric scale of output image                            *
 ************************************************************************/
static int opscale(ifilt,igain,cgain,a1,a2,scale,scale_type,gll)
  int ifilt,igain;	/* Input filter, gain			*/
  int cgain;		/* Gain of calibration file		*/
  float	*a1;		/* Output scale, I/F per DN		*/
  float	*a2;		/* Output scale, Nanowatts/DN		*/
  float	*scale;		/* Output DN/Ft-Lambert			*/
  int *scale_type;	/* 1=IOF, 2=CONV			*/
  able_86_typ *gll;
{
		/* I/F per Ft-Lambert for each filter */
/* Reflectivity/FtL and radiance/FtL tables from K. Klaasen memo July 2, 1991,
   and installed on Aug 30 1991. */

	/* Reflectivity/FtL at Venus (cover on) */
  static float reflect_venus[8]={
	1.200e-02,  /*  1.028e-02, 9.4110e-03,  0=clear */
  	6.574e-03,  /*  5.601e-03, 4.4773e-03,  1=green */
	1.223e-02,  /*  1.047e-02, 8.7484e-03,  2=red   */
	2.913e-03,  /*  2.416e-03, 2.0935e-03,  3=violet*/
	1.877e-02,  /*  1.606e-02, 1.5563e-02,  4=7560A */
	5.152e-02,  /*  4.374e-02, 5.6132e-02,  5=9680A */
	1.804e-02,  /*  1.542e-02, 1.3268e-02,  6=7270A */
	2.763e-02}; /*  2.356e-02  2.9945e-02	7=8890A */

	/*  Reflectivity/FtL at EARTH-1 (cover on) */
  static float reflect_earth[8]={
	1.356e-02,  /*  1.028e-02, 9.4110e-03,  0=clear */
  	7.430e-03,  /*  5.601e-03, 4.4773e-03,  1=green */
	1.382e-02,  /*  1.047e-02, 8.7484e-03,  2=red   */
	3.292e-03,  /*  2.416e-03, 2.0935e-03,  3=violet*/
	2.122e-02,  /*  1.606e-02, 1.5563e-02,  4=7560A */
	5.823e-02,  /*  4.374e-02, 5.6132e-02,  5=9680A */
	2.039e-02,  /*  1.542e-02, 1.3268e-02,  6=7270A */
	3.116e-02}; /*  2.356e-02  2.9945e-02	7=8890A */

/* Updates from Klassen memo Nov 18 1997 */
/* Previous updates from Klaasen memo Mar 18 1996 */
	/*  Reflectivity/FtL at GASPRA (cover off) */
  static float reflect_gaspra[8]={
	4.358e-03,  /* 4.376e-03  4.510e-03  4.650e-03  4.387e-03   0=clear */
	2.440e-03,  /* 2.435e-03  2.509e-03  2.626e-03  2.075e-03   1=green */
	5.155e-03,  /* 5.141e-03  5.298e-03  5.278e-03  5.788e-03   2=red   */
	4.384e-04,  /* 4.398e-04  4.532e-04  4.284e-04  1.297e-04   3=violet*/
	8.007e-03,  /* 7.871e-03  8.111e-03  8.235e-03  9.474e-03   4=7560A */
	1.486e-02,  /* 1.485e-02  1.503e-02  1.580e-02  1.490e-02   5=9680A */
	7.587e-03,  /* 7.509e-03  7.738e-03  7.935e-03  8.511e-03   6=7270A */
	1.120e-02}; /* 1.113e-02  1.147e-02  1.066e-02  1.245e-02   7=8890A */

	/*  Reflectivity/FtL for G29 and beyond (no light flood).  IOM,
	    "Updated GALSOS Calibration Factors", K. Klaasen, 11 Feb 2002. */
  static float reflect_G29[8]={
	4.358e-03,	/* 0=clear */
	2.324e-03,  	/* 1=green */
	5.094e-03,	/* 2=red   */
	4.598e-04,	/* 3=violet*/
	8.210e-03,	/* 4=7560A */
	1.749e-02,	/* 5=9680A */
	7.686e-03,	/* 6=7270A */
	1.275e-02};	/* 7=8890A */

        /* Radiance/FtL at Venus (cover on) */
  static float radiance_venus[8]={
	2.118e+01,    /*  1.817e+01,  1.8937e+01, 0=clear */ 
	1.431e+01,    /*  1.220e+01,  9.7407e+00, 1=green */
	2.218e+01,    /*  1.900e+01,  1.6066e+01, 2=red   */
	5.372e+00,    /*  4.473e+00,  3.7474e+00, 3=violet*/
	2.760e+01,    /*  2.361e+01,  2.2987e+01, 4=7560A */
	4.729e+01,    /*  4.013e+01,  5.1375e+01, 5=9680A */
	2.809e+01,    /*  2.400e+01,  2.0848e+01, 6=7270A */
	3.147e+01};   /*  2.683e+01,  3.4134e+01  7=8890A */

	/* Radiance/FtL at Earth-1 (cover on) */
  static float radiance_earth[8]={
	2.393e+01,    /*  1.817e+01,  1.8937e+01, 0=clear */ 
	1.617e+01,    /*  1.220e+01,  9.7407e+00, 1=green */
	2.507e+01,    /*  1.900e+01,  1.6066e+01, 2=red   */
	6.070e+00,    /*  4.473e+00,  3.7474e+00, 3=violet*/
	3.119e+01,    /*  2.361e+01,  2.2987e+01, 4=7560A */
	5.344e+01,    /*  4.013e+01,  5.1375e+01, 5=9680A */
	3.173e+01,    /*  2.400e+01,  2.0848e+01, 6=7270A */
	3.549e+01};   /*  2.683e+01,  3.4134e+01  7=8890A */

/* Updates from Klaasen memo Nov 18 1997 */
/* Previous updates from Klaasen memo Mar 18 1996 */
	/*  Radiance/FtL at Gaspra (cover off) */
  static float radiance_gaspra[8]={
	7.853e-00,   /*7.858e-00  8.098e+00  8.283e+00  7.963e+00  0=clear */ 
	5.315e-00,   /*5.303e-00  5.465e+00  5.718e+00  4.521e+00  1=green */
	9.363e-00,   /*9.338e-00  9.623e+00  9.581e+00  1.035e+01  2=red   */
	8.107e-01,   /*8.136e-01  8.385e-01  7.664e-01  2.131e-01  3=violet*/
	1.177e+01,   /*1.157e+01  1.192e+01  1.211e+01  1.382e+01  4=7560A */
	1.356e+01,   /*1.331e+01  1.371e+01  1.443e+01  1.370e+01  5=9680A */
	1.181e+01,   /*1.169e+01  1.204e+01  1.235e+01  1.314e+01  6=7270A */
	1.276e+01};  /*1.268e+01  1.306e+01  1.214e+01  1.420e+01  7=8890A */

	/*  Radiance/FtL at G29 and beyond (no light flood) */
  static float radiance_G29[8]={
	7.926e-00,	/* 0=clear */ 
	5.063e-00,	/* 1=green */
	9.259e-00,	/* 2=red   */
	8.510e-01,	/* 3=violet*/
	1.208e+01,	/* 4=7560A */
	1.597e+01,	/* 5=9680A */
	1.197e+01,	/* 6=7270A */
	1.452e+01};	/* 7=8890A */

	/* Updated values from Klaasen's memo dated April, 1992 */
     static float gain_ratio[4] = {47.135, 9.771, 4.824, 1.0} ;    
   /*   static float gain_ratio[4]={47.091,9.809,4.799,1.0};   */

  int icnt,idef,rtn_val=0;
  char msg[128];
  double rsun;

	  /* Get IOF/ftL and Nanowatts/ftL scale factors (conv1,conv2) */
  if (mission_phase == PHASE1) { 
     if (sclk>0 && sclk<30000001) {
	zvmessage(" Mission Phase=Venus Encounter",""); 
	conv1 = reflect_venus[ifilt];
	conv2 = radiance_venus[ifilt];
      }
      if (sclk>30000000 && sclk<63000001) {
	 zvmessage(" Mission Phase=Earth/Moon-1 Encounter","");
	 conv1 = reflect_earth[ifilt];
	 conv2 = radiance_earth[ifilt];
      }
      if (sclk>63000000) {
	 zvmessage(" Mission Phase=Phase1","");
	 conv1 = reflect_gaspra[ifilt];
	 conv2 = radiance_gaspra[ifilt];
      }
  }

  if (mission_phase == PHASE2) {
     zvmessage(" Mission Phase=Phase2","");
     if (sclk<584054600) {
        conv1 = reflect_gaspra[ifilt];
        conv2 = radiance_gaspra[ifilt];
        }
     else {
        conv1 = reflect_G29[ifilt];
        conv2 = radiance_G29[ifilt];
     }
  }

  sprintf(msg," Reflectivity per Foot-Lambert = %.4f",conv1);
  zvmessage(msg,"");
  sprintf(msg," Radiance per Foot-Lambert = %.4f",conv2);
  zvmessage(msg,"");
  zvmessage(" Reflectivity units apply to a solar-illuminated","");
  zvmessage(" surface at 5.2 AU ","");

  rsun = (double)(gll->sol_range)/(5.2*KM_PER_AU);

  zvparm("CONV",a2,&icnt,&idef,0,0);
  if (idef == 0) {
     zvmessage(" Scaling Type:  CONV ","");
     *scale_type = 2;
     *a1 = *a2*(conv1/conv2)*(rsun*rsun)*10000.0;	
     *scale = (conv2/(*a2))*((gain_ratio[igain-1])/(gain_ratio[cgain-1])); 
     gll->cnv = *a2;
     gll->iof = *a1;    
     rtn_val = 2;
     sprintf(msg," Scaling values to be used = %.4e ",*scale);
  }
  else {
     zvp("IOF",a1,&icnt);
     zvmessage(" Scaling Type:  IOF ","");
     *scale_type = 1;
     *a2 = *a1*(conv2/conv1)/(rsun*rsun*10000);
     *scale = 10000.0*(conv1/(*a1))*((gain_ratio[igain-1])/
				   (gain_ratio[cgain-1]))*(rsun*rsun);
     gll->iof = *a1;
     gll->cnv = *a2;
     rtn_val = 1;
     sprintf(msg," Scaling values to be used = %.4e ",*scale);
  }
  
  zvmessage(msg,"");
  return (rtn_val);
} /* end opscale */


/************************************************************************
 * Remove blemishes from current line.  This requires previous and next *
 * lines for interpolation.                                             * 
 ************************************************************************/
static void remove_blemishes(current_line,previous_line,next_line,line)
short current_line[];		/* pointer to line to correct	*/
short previous_line[];		/* pointer to previous line in image*/
short next_line[];		/* pointer to next line in image*/
int line;			/* current line number in image	*/
{
  int i;			/* index of current samp*/
  short	*sample;		/* current sample in line*/
  char msg[128];
  int j1,j2,j3,j4,j5;

  if (line < current_blem->line) return;
  if (current_blem > last_blem) return;

  for (;(current_blem <= last_blem) && (line == current_blem->line);
	    current_blem++) {
     i = current_blem->samp - 1;
     sample = current_line + i;
     if (current_blem->sat_dn > 0) continue; /*    not saturated	*/

     verify_class(current_blem,previous_line,current_line,next_line);
     if (current_blem->class != 0) c_blem_cntr++;
     switch (current_blem->class) {
	case 0  :		/* always set to lowest Dn	*/
	  *sample = LOWEST_DN;  break;
	case 1  :		/* pair  1			*/
	  *sample = (previous_line[i-1] + next_line[i+1] + 1)/2;
	  break;
	case 2  :		/* pair  2			*/
	  *sample = (previous_line[i] + next_line[i] + 1)/2;
	  break;
	case 3  :		/* pairs 1,2			*/
	  *sample = (previous_line[i-1] + previous_line[i]
		     + next_line[i] + next_line[i+1] + 2)/4;
	  break;
	case 4  :		/* pair  3			*/
	  *sample = (previous_line[i+1] + next_line[i-1] + 1)/2;
	  break;
	case 5  :		/* pairs 1,3			*/
	  *sample = (previous_line[i-1] + previous_line[i+1]
		     + next_line[i-1] + next_line[i+1] + 2)/4;
	  break;
	case 6  :		/* pairs 2,3			*/
	  *sample = (previous_line[i] + previous_line[i+1]
		     + next_line[i-1] + next_line[i] + 2)/4;
	  break;
	case 7  :		/* pairs 1,2,3			*/
	  *sample = (previous_line[i-1] + previous_line[i]
		     + previous_line[i+1] + next_line[i-1]
		     + next_line[i] + next_line[i+1] + 3)/6;
	  break;
	case 8  :		/* pair  4			*/
	  *sample = (*(sample-1) + *(sample+1) + 1)/2;
	  break;
	case 9  :		/* pairs 1,4			*/
	  *sample = (*(sample-1) + *(sample+1)
			   + previous_line[i-1] + next_line[i+1] + 2)/4;
	  break;
	case 10 :		/* pairs 2,4			*/
	  *sample = (*(sample-1) + *(sample+1)
		     + previous_line[i] + next_line[i] + 2)/4;
	  break;
	case 11 :		/* pairs 1,2,4			*/
	  *sample = (*(sample-1) + *(sample+1)
		     + previous_line[i-1] + next_line[i+1]
		     + previous_line[i] + next_line[i] + 3)/6;
	  break;
	case 12 :		/* pairs 3,4			*/
	  *sample = (*(sample-1) + *(sample+1)
		     + previous_line[i+1] + next_line[i-1] + 2)/4;
	  break;
	case 13 :		/* pairs 1,3,4			*/
	  *sample = (*(sample-1) + *(sample+1)
		     + previous_line[i-1] + next_line[i+1]
		     + previous_line[i+1] + next_line[i-1] + 3)/6;
	  break;
	case 14 :		/* pairs 2,3,4			*/
	  *sample = (*(sample-1) + *(sample+1)
		     + previous_line[i] + next_line[i]
		     + previous_line[i+1] + next_line[i-1] + 3)/6;
	  break;
	case 15 :		/* pairs 1,2,3,4		*/
	  *sample = (*(sample-1) + *(sample+1)
		     + previous_line[i-1] + next_line[i+1]
		     + previous_line[i] + next_line[i]
		     + previous_line[i+1] + next_line[i-1] + 4)/8;
	  break;

/* Double column blemishes, blemish on right column...*/
	case 17 :		/* pair  1			*/
	  *sample = (previous_line[i-2] + next_line[i+1] + 1)/2;
	  break;
	case 18 :		/* pair  2			*/
	  *sample = (*(sample-2) + *(sample+1) + 1)/2;
	  break;
	case 19 :		/* pairs 1,2			*/
	  *sample = (previous_line[i-2] + next_line[i+1]
		     + *(sample-2) + *(sample+1) + 2)/4;
	  break;
	case 20 :		/* pair  3			*/
	  *sample = (previous_line[i+1] + next_line[i-2] + 1)/2;
	  break;
	case 21 :		/* pairs 1,3			*/
	  *sample = (previous_line[i-2] + previous_line[i+1]
		     + next_line[i-2] + next_line[i+1] + 2)/4;
	  break;
	case 22 :		/* pairs 2,3			*/
	  *sample = (*(sample-2) + *(sample+1)
		     + previous_line[i+1] + next_line[i-2] + 2)/4;
	  break;
	case 23 :		/* pairs 1,2,3			*/
	  *sample = (previous_line[i-2] + previous_line[i+1]
		     + *(sample-2) + *(sample+1)
		     + next_line[i-2] + next_line[i+1] + 3)/6;
	  break;

/* Double column blemishes, blemish on left column.... */
	case 25 :		/* pair  1			*/
	  *sample = (previous_line[i-1] + next_line[i+2] + 1)/2;
	  break;
	case 26 :		/* pair  2			*/
	  *sample = (*(sample-1) + *(sample+2) + 1)/2;
	  break;
	case 27 :		/* pairs 1,2			*/
	  *sample = (previous_line[i-1] + next_line[i+2]
		     + *(sample-1) + *(sample+2) + 2)/4;
	  break;
	case 28 :		/* pair  3			*/
	  *sample = (previous_line[i+2] + next_line[i-1] + 1)/2;
	  break;
	case 29 :		/* pairs 1,3			*/
	  *sample = (previous_line[i-1] + previous_line[i+2]
		     + next_line[i-1] + next_line[i+2] + 2)/4;
	  break;
	case 30 :		/* pairs 2,3			*/
	  *sample = (*(sample-1) + *(sample+2)
		     + previous_line[i+2] + next_line[i-1] + 2)/4;
	  break;
	case 31 :		/* pairs 1,2,3			*/
	  *sample = (previous_line[i-1] + previous_line[i+2]
		     + *(sample-1) + *(sample+2)
		     + next_line[i-1] + next_line[i+2] + 3)/6;
	  break;
	  
	default : 
	  sprintf(msg," Blem class (%d); pixel (%d,%d) not corrected.",
		  current_blem->class,current_blem->line,
		  current_blem->samp);
	  zvmessage(msg,"");
	}     /*  end Switch   */
  }/* end FOR loop */

  return;
}      /* end subroutine remove_blemishes */


/************************************************************************
 * Read blemish file and return it in blem_struct.                      * 
 ************************************************************************/
#if 0 /* RRP Removed */

static void trans_blem(bunit,bnum,blem_struct)
int bunit;                        /* input */
int bnum;                         /* input - number of blemishes */
blem_typ *blem_struct;            /* output */
{
  int i,n,rec_length;
  short data[4*MAX_BLEMS];
  blem_typ *temp;

  zvread(bunit,data, NULL);
  n = 0;
  rec_length = sizeof(blem_typ);
  temp = (blem_typ *) malloc(rec_length);

  for (i=0; i<bnum; i++) {
     n = i*4;
     temp->line = data[n];
     temp->samp = data[n+1];
     temp->class = data[n+2];
     temp->sat_dn = data[n+3];
     memcpy(blem_struct+i,temp,rec_length);
  }

  free(temp);
  return;
}  /* end subroutine trans_blem */


#endif

/************************************************************************
 * Update EDR telemetry header.						*
 ************************************************************************/
int update_telemetry_header()
{
  int ind,i;
  char msg[80];

  mean_iof = sum_iof/(vld_pxls*10000.0);   /* scale_type=1, for IOF */
  if (scale_type==2) mean_iof *= a1/a2;	/* scale_type=2, for CNV */
  sprintf(msg,"%12.9f",mean_iof);
  if (mission_phase==PHASE1) memcpy(tlm_hdr.i_f,msg,12);
  else memcpy(ph2_tlm_hdr.mean_if,msg,12);
  if (calc_entropy) {
     sprintf(msg,"%7.4f",entropy);
     if (mission_phase==PHASE1) memcpy(tlm_hdr.entropy,msg,7);
     else memcpy(ph2_tlm_hdr.entrop_avg,msg,7);
     for (i=0; i<15; i++) {
	sprintf(msg,"%7.4f",entropies[i]);
	if (mission_phase==PHASE1) memcpy(tlm_hdr.entropies[i],msg,7);
	else memcpy(ph2_tlm_hdr.entropies[i],msg,7);
     }
  }
  zvclose(ounit, NULL);
  ind=zvopen(ounit,"COND","BINARY","OP","UPDATE","U_FORMAT","HALF",
	"O_FORMAT","HALF","OPEN_ACT","SA","IO_ACT","SA", NULL);
  if (mission_phase==PHASE1) write_ssi_telem_hdr(ounit,&tlm_hdr);
  else write_ssi_ph2_telem_hdr(ounit,&ph2_tlm_hdr);
  return ind;
}
/************************************************************************
 * Check if surrounding samples contain valid samples for interpolation *
 * of blemishes.  If invalid samples exist, downgrade blemish class.    * 
 ************************************************************************/
static void verify_class(blemish,previous,current,next)
  blem_typ *blemish;
  short	*previous;		/* pointer to previous line in image*/
  short	*current;		/* pointer to line to correct	*/
  short	*next;			/* pointer to next line in image*/
{ 
  int	samp,
	line,
        left = -2,
        right = 1;

  samp = blemish->samp - 1;
  line = blemish->line -1;

  if (blemish->class & 0x10) {		/* Double column Blemishes */
     if (blemish->class & 0x08) { 
	left = -1;
	right = 2;
     }
		/* Upper-Left & Lower-Right */
     if (previous[samp+left]==LOWEST_DN || next[samp+right]==LOWEST_DN) 
	blemish->class &= ~(0x01);
		/* Left-Center & Right-Center */
     if (current[samp+left]==LOWEST_DN || current[samp+right]==LOWEST_DN)
	blemish->class &= ~(0x02);
		/* Lower-Left & Upper-Right */
     if (previous[samp+left] == LOWEST_DN || next[samp+right] == LOWEST_DN)
	blemish->class &= ~(0x04);
  }
  else {				/*  Single column Blemishes */
 		/* Upper-Left & Lower-Right */
     if (previous[samp-1]==LOWEST_DN || next[samp+1]==LOWEST_DN)
 	blemish->class &= ~(0x01);
 		/* Upper-Center & Lower-Center */
     if (previous[samp]==LOWEST_DN || next[samp]==LOWEST_DN)
	blemish->class &= ~(0x02);
		/* Lower-Left & Upper-Right */
     if (previous[samp+1]==LOWEST_DN || next[samp-1]==LOWEST_DN)
	blemish->class &= ~(0x04);
		/* Left-Center & Right-Center */
     if (current[samp-1]==LOWEST_DN || current[samp+1]==LOWEST_DN)
	blemish->class &= ~(0x08);
  }

}  /* end subroutine verify_class */


/************************************************************************
 * Write history labels.						*
 ************************************************************************/
static int write_history_labels(entropy)
float *entropy;		/* output image entropy in bits/DN */
{
  int i,ind;

  if (nlb > 0) {
     zldel(ounit,"SYSTEM","NLB", NULL);
     ind=zladd(ounit,"SYSTEM","NLB",&nlbout,"FORMAT","INT", NULL);
  }
		/* Calculate Entropy */
  if (calc_entropy) calculate_entropy(img_dhist,entropy);
  if (zvptst("PRINT")) {	 /* Print Entropy results */
     sprintf(msg,"Image Entropy: %7.4f",*entropy);
     zvmessage(msg,"");
     for (i=0; i<15; i++) {
	sprintf(msg,"Line %03d Entropy: %7.4f",(i+1)*50,entropies[i]);
	zvmessage(msg,"");
     }
  }
  if (calc_entropy) {
     ind = zladd(ounit,"HISTORY","ENTROPY",entropy,"FORMAT","REAL",
        "HIST",first_task,"MODE","REPLACE","ELEMENT",1, NULL);
  }
  sprintf(msg," GALSOS replaced %d pixels by interpolation",c_blem_cntr);
  zvmessage(msg,"");
  ind=zladd(ounit,"HISTORY","COMMENT",&msg[1],"FORMAT","STRING", NULL);
  return ind;
}
/************************************************************************
 * If a lfw pixel is greater than sat_dn, then flag it as a blemish and *
 * store it in the bdv record.						*
 ************************************************************************/
static void zap_lfw_pixels(current_line,line,ss1,es1,ss2,es2,nlbout,lfw,ounit)
UBYTE *current_line;	/* current image line		*/
int line;		/* current line number		*/
int ss1,es1,ss2,es2;	/* starting and ending samples of data segments */
int *nlbout;		/* # of binary headers in output image	*/
int ounit;		/* Output image unit number	*/
ssi_bdvh_typ *lfw;	/* LFWP bad-data-record		*/
{
  UBYTE	*current_pixel;  	/* pointer to indiv. samp in line */
  static UBYTE lfw_flag[800];	/* Flag for each column of the image */

  if (line == 1) memset(lfw_flag,0,sizeof(lfw_flag));

  while (zap_index->line < line) zap_index++;
  for (; zap_index->line==line; zap_index++) {
     if (lfw_flag[(zap_index->samp-1)]) zap_index->sat_dn=0;
     if (zap_index->sat_dn != 0) { 
	current_pixel = current_line + zap_index->samp - 1;
	if (((int) (*current_pixel) > zap_index->sat_dn) && 
	   (ss1 <= zap_index->samp) && (es1 >= zap_index->samp) ||
	   (ss2 <= zap_index->samp) && (es2 >= zap_index->samp)) {
	      lfw_flag[(zap_index->samp-1)] = TRUE;
	      zap_index->sat_dn = 0;
	      if (ounit >= 0)
		add_segment(lfw,zap_index->samp,line,nl,nlbout,ounit);
	 }
     }
  } 
  return;
}  /* end subroutine zap_lfw_pixels */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_bin.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* Subroutines which read/write the GLL EDR/UDR binary bad data value header */
/* M. O'Shaughnessy, R. Deen (telemetry header code )                        */
/*                                                                           */
/* Contents:                                                                 */
/*                                                                           */
/* Routines to read and write the binary bad-data value header:              */
/*       get_ssi_bdv_hdr_rec (unit,line,dest)                                */
/*       write_ssi_bdv_hdr_rec (ounit,line,source)                           */
/*     The "line" parameter is absolute line; line=1 is the first record in  */
/*     the file. See "UDR_THDR_RECS" and "EDR_THDR_RECS" in BDVH.H for more  */
/*     information.                                                          */
/*                                                                           */
/* Routines to read and write an EDR image line:                             */
/*       get_gll_edr_ln (unit,line,dest)                                     */
/*       write_gll_edr_ln (ounit,line,source)                                */
/*                                                                           */
/* Routines to read and write a UDR image line:                              */
/*       get_gll_udr_ln (unit,line,dest)                                     */
/*       write_gll_udr_ln (ounit,line,source)                                */
/*                                                                           */
/* Routines to read and write the binary telemetry header:                   */
/*       get_ssi_telem_hdr (unit,dest)                                       */
/*       write_ssi_telem_hdr (ounit,source)                                  */
/*                                                                           */
/* Routine to write a binary image line prefix:                              */
/*       write_ssi_prefix(ounit,line,source)                                 */
/*                                                                           */
/* In *ALL* the above subroutines, the unit or ounit most be that of a file  */
/* which has already been opened with "COND","BINARY".  Host information     */
/* should also be set in the zvopen() call; see gllfillin.com for examples.  */
/*                                                                           */
/* Internal routines:                                                        */
/*    The trin* and trout* routines translate specific gll datatypes.        */
/*       trin_scet_typ(from,to)                                              */
/*       trin_ert_typ(from,to)                                               */
/*       trin_sclk_typ(from,to)                                              */
/*       trin_ssi_lrs_p_typ(from,to)                                         */
/*       trin_ssi_flg_typ(from,to)                                           */
/*       trin_j2000_ascii_typ(from,to)                                       */
/*                                                                           */
/*       trout_scet_typ(from,to)                                             */
/*       trout_ert_typ(from,to)                                              */
/*       trout_sclk_typ(from,to)                                             */
/*       trout_ssi_lrs_p_typ(from,to)                                        */
/*       trout_ssi_flg_typ(from,to)                                          */
/*       trout_j2000_ascii_typ(from,to)                                      */
/*                                                                           */
/*       get_ssi_data_format(unit,format)                                    */
/*                                                                           */
/*       init_trans_out(byte_trans,half_trans,full_trans)                    */
/*       init_trans_inb(unit,byte_trans,half_trans,full_trans)               */
/*       init_pixsizeb(unit,byte_size,full_size,half_size)                   */
/*****************************************************************************/
#include "xvmaininc.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "gll_main.h"
#include "gll_lrs.h"
#include "gll_ssi_edr.h"
#include "gll_ssi_bin.h"
#include "zvproto.h"

int gll_ssi_bin_debug = FALSE; /* Flag for turning debugging on.             */

/*****************************************************************************/
/* Get and translate one SSI EDR/UDR binary bad data value header record.    */
/* Translation is from any format, into native format.                       */
/*****************************************************************************/
ROUTINE int get_ssi_bdv_hdr_rec(unit,line,dest)
int            unit,            /* must be an open file with COND BINARY set */
               line;            /* file line! This number is never less than
                                   2 if there is a telemetry header on the 
                                   image.                                    */
ssi_bdvh_typ  *dest;
{
   unsigned char *buf, 
                 *p;
   int recsize, 
       status = SUCCESS,
       i,
       data_exists;

   init_trans_inb(unit,byte_trans,half_trans,full_trans);
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);
   status = zvget(unit, "RECSIZE", &recsize, NULL);
   if (status != SUCCESS) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> failed zvget at line %d, status %d",line,status);
     zvmessage(aline,0);
     zvsignal(unit,status,TRUE);
   }
   buf = (unsigned char *) malloc(recsize);
   if (buf == NULL) return BAD_MALLOC;
   status = zvread(unit,buf,"LINE",line,NULL); 
   if (status != SUCCESS) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> failed zvread at line %d, status %d",line,status);
     zvmessage(aline,0);
     zvsignal(unit,status,TRUE);
   }
   p = buf;

   zvtrans(half_trans, p, &dest->record_id, 1); 
   /* Check to see if this is good data */
   if ((dest->record_id != BDV_DATA_DROPOUT)    && 
       (dest->record_id != BDV_SATURATED_PX)    &&
       (dest->record_id != BDV_LFW_PX)          &&
       (dest->record_id != BDV_SINGLE_PX_SPIKE) &&
       (dest->record_id != BDV_RS_OVERFLOW)) { 
          /* Image data structure uses rec_id as first byte */
     if ((dest->record_id & 0xFF) == BDV_IMAGE_DATA) {
         /* we got image data! */       
/*       sprintf(aline,
       "get_ssi_bdv_hdr_rec> first image line found at file line %d",line);
       zvmessage(aline,0);
*/
       return (END_BDVH);
     } 
     else if (dest->record_id == BDV_EMPTY_REC) { /*empty record found*/
       sprintf(aline,
      "get_ssi_bdv_hdr_rec> empty bdv data record found at line %d",line);
       zvmessage(aline,0);
       /* keep going, and return the empty line to the calling routine. */
     }
     else { /* we got garbage */
       sprintf(aline, 
       "get_ssi_bdv_hdr_rec> found unrecognizable record ID = %d at line %d",
         dest->record_id,line);
       zvmessage(aline,0);
       zabend();
     }
   }
   p += half_size;

   THALF(p, &dest->code); 
   if ((dest->code != BDV_SPX)      &&
       (dest->code != BDV_LINE_SEG) &&
       (dest->code != BDV_SAMP_SEG)) { /* bad data code was found */
      sprintf(aline,
      "get_ssi_bdv_hdr_rec> found unrecognizable BDV object code = %d at line %d",
      dest->code,line);
      zvmessage(aline,0);
      zabend();
   }

   THALF(p, &dest->nobjs); 
   if ((dest->code == BDV_SPX)      && (dest->nobjs > MAX_FFE_SPX_OBJ)) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> bad number of bdv objects in line %d",line);
     zvmessage(aline,0);
     sprintf(aline,
       "get_ssi_bdv_hdr_rec> %d objects indicated, max allowed is %d for single pixel",
       dest->nobjs, MAX_FFE_SPX_OBJ);
     zvmessage(aline,0);
     zabend();
   } 
   if ((dest->code == BDV_LINE_SEG) && (dest->nobjs > MAX_FFE_LINE_OBJ)) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> bad number of bdv objects in line %d",line);
     zvmessage(aline,0);
     sprintf(aline,
       "get_ssi_bdv_hdr_rec> %d objects indicated, max allowed is %d for line triplet",
       dest->nobjs, MAX_FFE_LINE_OBJ);
     zvmessage(aline,0);
     zabend();
   }
   if ((dest->code == BDV_SAMP_SEG) && (dest->nobjs > MAX_FFE_COL_OBJ)) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> bad number of bdv objects in line %d",line);
     zvmessage(aline,0);
     sprintf(aline,
      "get_ssi_bdv_hdr_rec> %d objects indicated, max allowed is %d for sample triplet",
      dest->nobjs, MAX_FFE_COL_OBJ);
     zvmessage(aline,0);
     zabend();
   }

   if (dest->code == BDV_SPX) { 
     for (i=0; i < dest->nobjs; i++) {
       THALF(p, &dest->coords.pixel_data[i].line);   
       THALF(p, &dest->coords.pixel_data[i].sample);   
     }
   } 
   else if (dest->code == BDV_LINE_SEG) { 
     for (i=0; i < dest->nobjs; i++) {
       THALF(p, &dest->coords.line_data[i].line);   
       THALF(p, &dest->coords.line_data[i].ss);   
       THALF(p, &dest->coords.line_data[i].ns);   
     }
   }
   else if (dest->code == BDV_SAMP_SEG) { 
     for (i=0; i < dest->nobjs; i++) {
       THALF(p, &dest->coords.column_data[i].sample);   
       THALF(p, &dest->coords.column_data[i].sl);   
       THALF(p, &dest->coords.column_data[i].nl);   
     }
   }
   if (dest->record_id == BDV_EMPTY_REC)
      return (EMPTY_BDVH_REC);
   else {
      return (SUCCESS);
   }
} /* end ssi_get_bdv_hdr() */


/*****************************************************************************/
/* Write one SSI EDR/UDR binary bad data value header record.                */
/* Translation is from native format into VAX format.                        */
/* This routine does not return any values; the contents of "dest" get       */
/* written directly to the output file "ounit".                              */
/*                                                                           */
/* NOTE: Unlike get_ssi_bdv_hdr_rec(), this routine does not check against   */
/* the record_id or other data to make sure that the data is valid!!         */
/*****************************************************************************/
ROUTINE int write_ssi_bdv_hdr_rec(ounit,line,source)
int            ounit,           /* must be an open file with COND BINARY set */
               line;            
ssi_bdvh_typ  *source;          /* data to be written to ounit */
{
   unsigned char *buf, 
                 *p;            /* destination buffer */
   int recsize,
       i,
       data_exists,
       status = SUCCESS;

   init_trans_out(byte_trans,half_trans,full_trans);
   init_pixsizeb(ounit,&byte_size,&half_size,&full_size);
   status = zvget(ounit, "RECSIZE", &recsize, NULL);
   zvsignal(ounit,status,TRUE);
   buf = (unsigned char *) malloc(recsize);
   if (buf == NULL) return BAD_MALLOC;
   p = buf;

/* Start filling up the buffer with pieces from the *source structure,       */
/* until one line is packed. Then write it out.                              */
   TOHALF(&source->record_id, p);
   TOHALF(&source->code, p);
   TOHALF(&source->nobjs, p); 

   /* check nobjs to be sure we have a rational number for looping */
   if ((source->nobjs < 1) || (source->nobjs > MAX_FFE_SPX_OBJ)) {
     sprintf(aline,"SSI_WRITE_BDV_HDR> invalid nobjs = %d",source->nobjs);
     zvmessage(aline,0);
     zabend();
   }

   if (source->code == BDV_SPX) { 
     for (i=0; i < source->nobjs; i++) {
       TOHALF(&source->coords.pixel_data[i].line, p); 
       TOHALF(&source->coords.pixel_data[i].sample, p);   
     }
   } 
   else if (source->code == BDV_LINE_SEG) { 
     for (i=0; i < source->nobjs; i++) {
       TOHALF(&source->coords.line_data[i].line, p);   
       TOHALF(&source->coords.line_data[i].ss, p);   
       TOHALF(&source->coords.line_data[i].ns, p);   
     }
   }
   else if (source->code == BDV_SAMP_SEG) { 
     for (i=0; i < source->nobjs; i++) {
       TOHALF(&source->coords.column_data[i].sample, p);   
       TOHALF(&source->coords.column_data[i].sl, p);   
       TOHALF(&source->coords.column_data[i].nl, p);   
     }
   }
   else {
     sprintf(aline,"SSI_WRITE_BDV_HDR> invalid code = %d",source->code);
     zvmessage(aline,0);
     zabend();
   }

   status = zvwrit(ounit,buf,"LINE",line,NULL); 
   zvsignal(ounit,status,TRUE);

   free(buf);

   return SUCCESS;
} /* end ssi_write_bdv_hdr() */

/*****************************************************************************/
/* Get one image line from an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_edr_ln(unit, line, dest)
int             unit,           /* must be an open file with COND BINARY set */
                line;           /* image line number (starting at zero)      */
ssi_edr_ln_typ *dest;
{  
   int i, ns, nlb,
       status = SUCCESS;

   /* Grab the binary prefix. get_ssi_prefix adds nlb to "line" internally. */

   get_ssi_prefix(unit, line, &(dest->hdr));  

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* flled.)  The pixel array is of type UWORD.                           */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvread(unit, &dest->pixel[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   if (status != SUCCESS) {
     sprintf(aline,"get_gll_edr_ln> bad zvread, failing line = %d\n",line);
     zvmessage(aline,0);
   }
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end get_gll_edr_ln */

/*****************************************************************************/
/* Write one image line to an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_edr_ln(unit, line, source)
int            unit,             /* must be an open file with COND BINARY set */
               line;             /* image line number */

ssi_edr_ln_typ *source;
{  
   int i, ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_prefix adds nlb to "line"          */
   /* internally.                                                           */

    write_ssi_prefix(unit, line, &source->hdr);

   /* Fill the dest structure, bypassing the binary prefix.                 */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvwrit(unit, &source->pixel[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end write_gll_edr_ln */

/*****************************************************************************/
/* Get one image line from an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_udr_ln(unit, line, dest)
int            unit,               /* must be an open file with COND BINARY set */
               line;               /* image line number */
ssi_udr_ln_typ *dest;
{  
   int i, ns, nlb,
       status = SUCCESS;
 
   /* Grab the binary prefix. get_ssi_prefix adds nlb to "line" internally. */

   get_ssi_prefix(unit, line, &dest->hdr);

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* filled. The pixel array is of type UBYTE. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvread(unit, &dest->pixel[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end get_gll_udr_ln */

/*****************************************************************************/
/* Write one image line to an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_udr_ln(unit, line, source)
int            unit,            /* must be an open file with COND BINARY set */
               line;            /* image line number */
ssi_udr_ln_typ *source;
{  
   int i, ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_prefix adds nlb to "line"          */
   /* internally.                                                           */
    write_ssi_prefix(unit, line, &source->hdr);

   /* Fill the dest structure, bypassing the binary prefix. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvwrit(unit, &source->pixel[0], 
                "LINE", line+nlb,
                "SAMP", BYTE_PREFIX_SZ+1,  
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end write_gll_udr_ln */


/****************************************************************************/
/* Read in a GLL SSI EDR binary telemetry header, and translate it to native	*/
/* format.  The structure of the file is defined by how the VAX/VMS	*/
/* compiler interprets the structures in "gll_ssi_edr.h".  The		*/
/* structure cannot be used to overlay the data read from the file,	*/
/* since different compilers will pack the structure differently.  So,	*/
/* the data elements are in the order they are defined in the structure,*/
/* packed together as tightly as possible.				*/

/* It is assumed that the entire binary header fits in the first line	*/
/* of the file.  If this is not the case, modify the malloc and read	*/
/* statusements appropriately.						*/

/* The caller is responsible for setting an error action to deal with	*/
/* any errors that occur during the read.				*/

/* This is not technically correct because a signed conversion (HALF or	*/
/* FULL) is used for numbers defined to be unsigned... but this is ok	*/
/* for all currently defined integer types.				*/

/* The strings could be defined to be longer in the destination		*/
/* structure, and a NULL terminator appended.  The dest structure does	*/
/* NOT have to have any resemblance to the file structure if it's not	*/
/* convenient.  Constants are used for string lengths; it is assumed	*/
/* these are defined in the file format and will not change.  A sizeof	*/
/* operator should not be used on the destination string, as that may	*/
/* not be the same size as the file.  Symbolic constants would be	*/
/* better, but since this and the associated write routine are the only	*/
/* routines that should touch the file structure, literal constants	*/
/* are acceptable here.							*/

/* An assumption is made that fields that are declared "char x[n]",	*/
/* such as trunc_pixel and i_f, really *are* characters.  If this is	*/
/* not the case, the conversions will have to be modified for those	*/
/* fields.								*/

ROUTINE int get_ssi_telem_hdr(unit, dest)
int unit;		/* must be an open file with COND BINARY set */
ssi_edr_hdr_typ *dest;
{
   unsigned char *buf,
                 *p;
   int            recsize,i,status = SUCCESS,
                  format,  /* data format */
                  nrecs;   /* number of records to get */

   init_trans_inb(unit,byte_trans,half_trans,full_trans); 
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   get_ssi_data_format(unit,&format);
   if (format == BYTE_DATA) 
     nrecs = UDR_THDR_RECS;
   else if (format == FF_HALF_DATA)
     nrecs = FF_EDR_THDR_RECS;
   else if (format == SM_HALF_DATA)
     nrecs = SM_EDR_THDR_RECS;

   status = zvget(unit,"RECSIZE",&recsize,NULL);
   zvsignal(unit,status,TRUE);
   buf = (unsigned char *) malloc(recsize*nrecs);
   if (buf == NULL)
      return BAD_MALLOC;

   /* Fill up buf with the entire telemetry header, reading the number of    */
   /* records indicated by the data format.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvread(unit,buf+(i*recsize),"LINE",1+i,NULL);
     zvsignal(unit,status,TRUE);
   }
   p = buf;

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   TBYTE(p, &dest->record_number);
   TBYTE(p, &dest->file_number);
   TSTRN(p, dest->project, 10);
   TSTRN(p, dest->instrument, 6);
   THALF(p, &dest->phy_seq);
   THALF(p, &dest->log_seq);
   trin_ert_typ(&p, &dest->fert);	/* increments p */
   trin_ert_typ(&p, &dest->lert);
   trin_sclk_typ(&p, &dest->fsclk);
   trin_sclk_typ(&p, &dest->lsclk);
   trin_scet_typ(&p, &dest->scet);
   TSTRN(p, dest->mipl_prd, 59);
   THALF(p, &dest->format_id);
   TFULL(p, &dest->sync_err);
   TBYTE(p, &dest->boom);
   THALF(p, &dest->missings);
   THALF(p, &dest->partials);
   THALF(p, &dest->unreadables);
   THALF(p, &dest->seq_brk);
   THALF(p, &dest->src_inp);
   THALF(p, &dest->wbdl);
   THALF(p, &dest->sdrs);
   THALF(p, &dest->sfdus);
   TSTRN(p, dest->pic_no, 7);
   trin_ssi_lrs_p_typ(&p, &dest->ssi_lrs);
   trin_ssi_flg_typ(&p, &dest->flags);
   TSTRN(p, dest->dn, 6);
   TSTRN(p, dest->trunc_bits, 6);
   TSTRN(p, dest->trunc_pixel, 6);
   TSTRN(p, dest->i_f, 12);
   TSTRN(p, dest->entropy, 7);
   TSTRN(p, dest->entropies, 15 * 7);
   trin_j2000_ascii_typ(&p, &dest->point_dir);
   TSTRN(p, dest->scale, 2 * 8);
   TSTRN(p, dest->slope, 32);
   TSTRN(p, dest->offset, 32);
   TSTRN(p, dest->activity, 20);
   p += byte_size;                                 /* skip filler_1 */
   TBYTE(p, &dest->filter);
   TBYTE(p, &dest->exposure);
   TBYTE(p, &dest->image_mode);
   TBYTE(p, &dest->gain);
   TFULL(p, &dest->range);
   TBYTE(p, &dest->tlm_frmt);
   THALF(p, &dest->version);
   trin_sclk_typ(&p, &dest->ssclk);
   trin_sclk_typ(&p, &dest->esclk);
   p += 318;                                    /* skip reserved */
   zvtrans(full_trans, p, dest->hist, 256);
   p += 256 * full_size;

   return SUCCESS;	
} /* end get_ssi_telem_hdr */

/****************************************************************************/
/* write the telemetry header out                                           */
/****************************************************************************/
ROUTINE write_ssi_telem_hdr(unit, source)
int              unit;		/* must be an open file with COND BINARY set */
ssi_edr_hdr_typ *source;
{
   unsigned char *buf, 
                 *p;
   int            recsize,i,status = SUCCESS,
                  format,  /* data format */
                  nrecs;   /* number of records to get */

   init_trans_out(byte_trans,half_trans,full_trans);
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   get_ssi_data_format(unit,&format);
   if (format == BYTE_DATA) 
     nrecs = UDR_THDR_RECS;
   else if (format == FF_HALF_DATA)
     nrecs = FF_EDR_THDR_RECS;
   else if (format == SM_HALF_DATA)
     nrecs = SM_EDR_THDR_RECS;

   status = zvget(unit, "RECSIZE", &recsize, NULL);
   zvsignal(unit,status,TRUE);
   buf = (unsigned char *) malloc(recsize*nrecs);
   if (buf == NULL)
      return BAD_MALLOC;
   p = buf;

   /* Fill in the buffer with elements from the structure.  Bytes are       */
   /* translated (although that's probably unnecessary, it's a good idea),  */
   /* while characters are just moved.		         	      	    */

   TOBYTE(&source->record_number, p);
   TOBYTE(&source->file_number, p);
   TOSTRN(source->project, p, 10);
   TOSTRN(source->instrument, p, 6);
   TOHALF(&source->phy_seq, p);
   TOHALF(&source->log_seq, p);
   trout_ert_typ(&source->fert, &p);	/* increments p */
   trout_ert_typ(&source->lert, &p);
   trout_sclk_typ(&source->fsclk, &p);
   trout_sclk_typ(&source->lsclk, &p);
   trout_scet_typ(&source->scet, &p);
   TOSTRN(source->mipl_prd, p, 59);
   TOHALF(&source->format_id, p);
   TOFULL(&source->sync_err, p);
   TOBYTE(&source->boom, p);
   TOHALF(&source->missings, p);
   TOHALF(&source->partials, p);
   TOHALF(&source->unreadables, p);
   TOHALF(&source->seq_brk, p);
   TOHALF(&source->src_inp, p);
   TOHALF(&source->wbdl, p);
   TOHALF(&source->sdrs, p);
   TOHALF(&source->sfdus, p);
   TOSTRN(source->pic_no, p, 7);
   trout_ssi_lrs_p_typ(&source->ssi_lrs, &p);
   trout_ssi_flg_typ(&source->flags, &p);
   TOSTRN(source->dn, p, 6);
   TOSTRN(source->trunc_bits, p, 6);
   TOSTRN(source->trunc_pixel, p, 6);
   TOSTRN(source->i_f, p, 12);
   TOSTRN(source->entropy, p, 7);
   TOSTRN(source->entropies, p, 15 * 7);
   trout_j2000_ascii_typ(&source->point_dir, &p);
   TOSTRN(source->scale, p, 2 * 8);
   TOSTRN(source->slope, p, 32);
   TOSTRN(source->offset, p, 32);
   TOSTRN(source->activity, p, 20);   
   p += byte_size;                           /* skip filler_1 */
   TOBYTE(&source->filter, p);
   TOBYTE(&source->exposure, p);
   TOBYTE(&source->image_mode, p);
   TOBYTE(&source->gain, p);
   TOFULL(&source->range, p);
   TOBYTE(&source->tlm_frmt, p);
   TOHALF(&source->version, p);
   trout_sclk_typ(&source->ssclk, &p);
   trout_sclk_typ(&source->esclk, &p);   
   p += 318;                                /* skip reserved */
   zvtrans(full_trans, source->hist, p, 256);
   p += 256 * full_size;

   /* Write out the entire telemetry header, by writing the number of        */
   /* records indicated by the data format.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvwrit(unit,buf+(i*recsize),"LINE",1+i,NULL);
     zvsignal(unit,status,TRUE);
   }

   free(buf);

   return SUCCESS;
} /* end write_ssi_telem_hdr */

/*****************************************************************************/
/* This routine translates one line of binary image line prefix into native  */
/* format (DEFAULT_HOST) and writes it out to the file specified by ounit.   */
/*****************************************************************************/
ROUTINE int write_ssi_prefix(ounit,line,source)
int           ounit,
              line;
ssi_lhdr_typ *source;
{
  unsigned char *p, 
                *buf;
  UBYTE          bits_byte = 0;      
  UWORD          bits_half = 0;  
  UINT           bits_full = 0;
  int            status = SUCCESS,
                 nlb,          /* # lines of binary header */
                 nbb;          /* # bytes of binary prefix data */

 status =  zvget(ounit, "NLB", &nlb, "NBB", &nbb, NULL);
 zvsignal(ounit,status,TRUE);
 buf = (unsigned char *) malloc (nbb);         
 if (buf == NULL) 
   return BAD_MALLOC;
 p = buf;

 init_trans_out(byte_trans,half_trans,full_trans);
 init_pixsizeb(ounit,&byte_size,&half_size,&full_size);
   
 TOBYTE(&source->rec_id, p);
 TOBYTE(&source->file_no, p);
 TOHALF(&source->phy_seq, p);

 TOHALF(&source->log_seq, p);

 TOHALF(&source->ert.year, p);
 TOHALF(&source->ert.day, p);
 TOBYTE(&source->ert.hour, p);
 TOBYTE(&source->ert.minute, p);
 TOBYTE(&source->ert.second, p);
 TOHALF(&source->ert.msecond, p);

 TOFULL(&source->sclk.rim, p);
 TOBYTE(&source->sclk.mod91, p);
 TOBYTE(&source->sclk.mod10, p);
 TOBYTE(&source->sclk.mod8, p);

 TOSTRN(source->mpr, p, 59);
 TOHALF(&source->fid, p) ;
 TOBYTE(&source->input.type, p);

 /* input source */
 if (source->input.SFDU)  bits_byte  |= 0x01;
 if (source->input.WBDL)  bits_byte  |= 0x02;
 if (source->input.SDR)   bits_byte  |= 0x04;
 if (source->input.IDR)   bits_byte  |= 0x08;
 if (source->input.EDR)   bits_byte  |= 0x10;
 if (source->input.RT)    bits_byte  |= 0x20;
 if (source->input.APB)   bits_byte  |= 0x40;
 TOBYTE(&bits_byte, p);

 TOBYTE(&source->allowed_sync_err, p);
 TOBYTE(&source->sync_err, p);
 zvtrans(byte_trans, &source->ssi_lrs[0], p, 12);
 p += 12 * byte_size;
 
 TOHALF(&source->last_pix, p);

 bits_half |= (source->sync_stat.error & 0x3FFF);
 bits_half |= (source->sync_stat.status & 0x03) << 14;
 TOHALF(&bits_half, p);

 bits_full |= (source->truncation.blk0 & 0x03);
 bits_full |= (source->truncation.blk1 & 0x03) << 2;
 bits_full |= (source->truncation.blk2 & 0x03) << 4;
 bits_full |= (source->truncation.blk3 & 0x03) << 6;
 bits_full |= (source->truncation.blk4 & 0x03) << 8;
 bits_full |= (source->truncation.blk5 & 0x03) << 10;
 bits_full |= (source->truncation.blk6 & 0x03) << 12;
 bits_full |= (source->truncation.blk7 & 0x03) << 14;
 bits_full |= (source->truncation.blk8 & 0x03) << 16;
 bits_full |= (source->truncation.blk9 & 0x03) << 18;
 bits_full |= (source->truncation.blk10 & 0x03) << 20;
 bits_full |= (source->truncation.blk11 & 0x03) << 22;
 bits_full |= (source->truncation.blk12 & 0x03) << 24;
 TOFULL(&bits_full, p);

 TOHALF(&source->truncated, p);
 TOHALF(&source->version, p);
 TOHALF(&source->ssnr, p);
 TOBYTE(&source->dsn_id, p);
 TOHALF(&source->image_line, p);
 TOBYTE(&source->rs_overflow, p);

 status = zvwrit(ounit, buf, "LINE", line+nlb, "NSAMPS", nbb, NULL);
 zvsignal(ounit,status,TRUE);

 free(buf);
  
 return SUCCESS;
} /* end write_ssi_prefix */

/*****************************************************************************/
/* Translation routines for specific datatypes: translate any input host fmt */
/*****************************************************************************/
ROUTINE static void trin_scet_typ(from, to)
unsigned char **from;
scet_typ *to;
{
   trin_ert_typ(from, (ert_typ *)to);		/* scet_typ same as ert_typ */
}


ROUTINE static void trin_ert_typ(from, to)
unsigned char **from;
ert_typ *to;
{
   THALF(*from, &to->year);
   THALF(*from, &to->day);
   TBYTE(*from, &to->hour);
   TBYTE(*from, &to->minute);
   TBYTE(*from, &to->second);
   THALF(*from, &to->msecond);
}

ROUTINE static void trin_sclk_typ(from, to)
unsigned char **from;
sclk_typ *to;
{
   TFULL(*from, &to->rim);
   TBYTE(*from, &to->mod91);
   TBYTE(*from, &to->mod10);
   TBYTE(*from, &to->mod8);
}

ROUTINE static void trin_ssi_lrs_p_typ(from, to)
unsigned char **from;
ssi_lrs_p_typ *to;
{
   int i;

   for (i=0; i<3; i++) {
      zvtrans(byte_trans, *from, to->packet[i].stnd_hk, 2);
      *from += 2*byte_size;
      zvtrans(byte_trans, *from, to->packet[i].img_hk, 2);
      *from += 2*byte_size;
   }
}

ROUTINE static void trin_ssi_flg_typ(from, to)
unsigned char **from;
ssi_flg_typ *to;
{
   unsigned short int bits;

   zvtrans(half_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += half_size;
   to->comp = bits & 0x01;
   to->cmp_mode = (bits >> 1) & 0x01;
   to->expo = (bits >> 2) & 0x01;
   to->flood = (bits >> 3) & 0x01;
   to->blem = (bits >> 4) & 0x01;
   to->clock = (bits >> 5) & 0x01;
   /* to->filler is not set */
}

ROUTINE static void trin_j2000_ascii_typ(from, to)
unsigned char **from;
j2000_ascii_typ *to;
{
   TSTRN(*from, to->ra, 8);
   TSTRN(*from, to->dcl, 8);
   TSTRN(*from, to->twst, 8);
}

/*****************************************************************************/
/* Translation routines for specific datatypes: write NATIVE output          */
/*****************************************************************************/
ROUTINE static void trout_scet_typ(from, to)
scet_typ *from;
unsigned char **to;
{
   trout_ert_typ((ert_typ *)from, to);		/* scet_typ same as ert_typ */
}

ROUTINE static void trout_ert_typ(from, to)
ert_typ *from;
unsigned char **to;
{
   TOHALF(&from->year, *to);
   TOHALF(&from->day, *to);
   TOBYTE(&from->hour, *to);
   TOBYTE(&from->minute, *to);
   TOBYTE(&from->second, *to);
   TOHALF(&from->msecond, *to);
}

ROUTINE static void trout_sclk_typ(from, to)
sclk_typ *from;
unsigned char **to;
{
   TOFULL(&from->rim, *to);
   TOBYTE(&from->mod91, *to);
   TOBYTE(&from->mod10, *to);
   TOBYTE(&from->mod8, *to);
}

ROUTINE static void trout_ssi_lrs_p_typ(from, to)
ssi_lrs_p_typ *from;
unsigned char **to;
{
   int i;

   for (i=0; i<3; i++) {
      zvtrans(byte_trans, from->packet[i].stnd_hk, *to, 2);
      *to += 2*byte_size;
      zvtrans(byte_trans, from->packet[i].img_hk, *to, 2);
      *to += 2*byte_size;
   }
}

ROUTINE static void trout_ssi_flg_typ(from, to)
ssi_flg_typ *from;
unsigned char **to;
{
   unsigned short int bits = 0;
   if (from->comp)     bits |= 0x01;
   if (from->cmp_mode) bits |= 0x02;
   if (from->expo)     bits |= 0x04;
   if (from->flood)    bits |= 0x08;
   if (from->blem)     bits |= 0x10;
   if (from->clock)    bits |= 0x20;
   zvtrans(half_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += half_size;
   /* from->filler is not set */
}

ROUTINE static void trout_j2000_ascii_typ(from, to)
j2000_ascii_typ *from;
unsigned char **to;
{
   TOSTRN(from->ra, *to, 8);
   TOSTRN(from->dcl, *to, 8);
   TOSTRN(from->twst, *to, 8);
}

/****************************************************************************/
/* This routine determines whether an SSI image is one of the following:   */
/*     full-frame EDR   (FF_HALF_DATA)                                     */
/*     sum-mode   EDR   (SM_HALF_DATA)                                     */
/*                UDR   (BYTE_DATA)                                        */
/* Full-frame and sum-mode UDRs are identical as far as binary information */
/* is concerned, so they are not distinguished.                            */
/*                                                                         */
/* This routine is not static void since it's used in gllfillin.c.         */
/****************************************************************************/
ROUTINE void get_ssi_data_format(unit,format)
int  unit,   /* unit number of the file */
    *format; /* output */
{
  char fmt[32];
  int nl, status = SUCCESS;

  status = zvget(unit,"format",fmt,NULL);
  zvsignal(unit,status,TRUE);

  if (strcmp(fmt,"BYTE") == 0) *format = BYTE_DATA;
  else if (strcmp(fmt,"HALF") == 0) {
    status = zvget(unit, "NL", &nl, NULL); 
    zvsignal(unit,status,TRUE);
    if (nl == NLINES)         *format = FF_HALF_DATA; /* Full-Frame EDR */ 
    else if (nl == NLINES_SE) *format = SM_HALF_DATA; /* Sum-mode EDR */
  }
  else {
    zvmessage("get_data_format> Error! Image format is not byte or halfword",0);
    zabend();
  }
}
/****************************************************************************/
/* This routine sets up translation buffers for output, converting from the */
/* machine's native representation into the DEFAULT_HOST (VAX)              */
/* representation.                                                          */
/****************************************************************************/
ROUTINE static void init_trans_out(byte_tr,half_tr,full_tr)
int *byte_tr,
    *half_tr,
    *full_tr;
{
   int status;
   char intfmt[12],realfmt[12];

   status = zvhost(DEFAULT_HOST,intfmt,realfmt);
   status = zvtrans_out(byte_tr, "BYTE", "BYTE", intfmt, realfmt);
   status = zvtrans_out(half_tr, "HALF", "HALF", intfmt, realfmt);
   status = zvtrans_out(full_tr, "FULL", "FULL", intfmt, realfmt);
 } /* end init_trans_out */

/****************************************************************************/
/* This routine sets up translation buffers for input, converting from the  */
/* host representation into the machine's native representation.            */
/****************************************************************************/
ROUTINE static void init_trans_inb(unit,byte_tr,half_tr,full_tr)
int unit,
    *byte_tr,
    *half_tr,
    *full_tr;
{
   int status = SUCCESS;

   status = zvtrans_inb(byte_tr, "BYTE", "BYTE", unit);
   zvsignal(unit,status,TRUE);
   status = zvtrans_inb(half_tr, "HALF", "HALF", unit);
   zvsignal(unit,status,TRUE);
   status = zvtrans_inb(full_tr, "FULL", "FULL", unit);
   zvsignal(unit,status,TRUE);
 } /* end init_trans_inb */

/****************************************************************************/
/* This routine returns the size of a binary label value (in bytes) from a  */
/* file.
/****************************************************************************/
ROUTINE static void init_pixsizeb(unit,byte_sz,half_sz,full_sz)
int unit,
    *byte_sz,
    *half_sz,
    *full_sz;
{
   int status;
   char aline[80];
 
   status = zvpixsizeb(byte_sz, "BYTE", unit);
   zvsignal(unit,status,TRUE);
   if (byte_sz == 0) {
     sprintf(aline,
     "init_pixsizeb> error in byte pixel size determination, status %d",byte_sz);
     zvmessage(aline,0);
     zabend();
   }

   status = zvpixsizeb(half_sz, "HALF", unit);
   zvsignal(unit,status,TRUE);
   if (half_sz == 0) {
     sprintf(aline,
    "init_pixsizeb> error in halfword pixel size determination, status %d",half_sz);
     zvmessage(aline,0);
     zabend();
   }

   status = zvpixsizeb(full_sz, "FULL", unit);
   zvsignal(unit,status,TRUE);
   if (full_sz == 0) {
     sprintf(aline,
    "init_pixsizeb> error in fullword pixel size determination, status %d",full_sz);
     zvmessage(aline,0);
     zabend();
   }
} /* end init_pixsizeb */
/* end module */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_bin_ph2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* Subroutines which read/write the GLL EDR/UDR Phase II binary  header      */
/* F, Moss, R. Deen (telemetry header code )                                 */
/*                                                                           */
/* Contents:                                                                 */
/*                                                                           */
/* Routines to read and write an EDR image line:                             */
/*       get_gll_ph2_edr_ln (unit,line,dest)                                 */
/*       write_gll_ph2_edr_ln (ounit,line,source)                            */
/*                                                                           */
/* Routines to read and write a UDR image line:                              */
/*       get_gll_ph2_udr_ln (unit,line,dest)                                 */
/*       write_gll_ph2_udr_ln (ounit,line,source)                            */
/*                                                                           */
/* Routines to write the binary telemetry header:                            */
/*       write_ssi_ph2_telem_hdr (ounit,source)                              */
/*                                                                           */
/* Routine to write a binary image line prefix:                              */
/*       write_ssi_ph2_prefix(ounit,line,source)                             */
/*                                                                           */
/* In *ALL* the above subroutines, the unit or ounit most be that of a file  */
/* which has already been opened with "COND","BINARY".  Host information     */
/* should also be set in the zvopen() call; see gllfillin.com for examples.  */
/*                                                                           */
/* Internal routines:                                                        */
/*    The trin* and trout* routines translate specific gll datatypes.        */
/*       trin_scet_typ(from,to)                                              */
/*       trin_ert_typ(from,to)                                               */
/*       trin_sclk_typ(from,to)                                              */
/*       trin_flg_comp_typ(from,to)                                          */
/*       trin_word23_typ(from, to)                                           */
/*       trin_word24_typ(from, to)                                           */
/*       trin_word25_typ(from, to)                                           */
/*       trin_word26_typ(from, to)                                           */
/*                                                                           */
/*       trout_scet_typ(from,to)                                             */
/*       trout_ert_typ(from,to)                                              */
/*       trout_sclk_typ(from,to)                                             */
/*       trout_word23_typ(from, to)                                          */
/*       trout_word24_typ(from, to)                                          */
/*       trout_word25_typ(from, to)                                          */
/*       trout_word26_typ(from, to)                                          */
/*                                                                           */
/*       init_trans_out(byte_trans,half_trans,full_trans)        */
/*       init_trans_inb(unit,byte_trans,half_trans,full_trans)   */
/*       init_pixsizeb(unit,byte_size,full_size,half_size)       */
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "xvmaininc.h"   
#include "rts_typedefs.h" 
#include "gll_rts_main.h"
#include "gll_rts_main_diff.h"
#include "gll_ph2_ssi_edr.h"
#include "gll_ssi_bin_diff.h" 
#include "gll_ph2_ssi_bin.h" 
#include "zvproto.h" 

/*    routine is declared as void, routine is called first(default returned   */
/*    value is int) before it is declared,so it needs to put prototype first  */ 
static void trin_word23_typ();  
static void trin_word24_typ();
static void trin_word25_typ();
static void trin_word26_typ();
static void trout_word23_typ();
static void trout_word24_typ();
static void trout_word25_typ();
static void trout_word26_typ();

int gll_ssi_bin_ph2_debug = FALSE; /* Flag for turning debugging on.         */

/*****************************************************************************/
/* Get one image line from an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_ph2_edr_ln(unit, line, dest)
int             unit,           /* must be an open file with COND BINARY set */
                line;           /* image line number (starting at zero)      */
ssi_edr_line_typ *dest;
{  
   int ns, nlb,
       status = SUCCESS;

   /* Grab the binary prefix. get_ssi_ph2_prefix adds nlb to "line" internally. */

   get_ssi_ph2_prefix(unit, line, &(dest->prefix));  

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* flled.)  The pixel array is of type UWORD.                           */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvread(unit, &dest->pixels[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   if (status != SUCCESS) {
     sprintf(ph2_aline,"get_gll_ph2_edr_ln> bad zvread, failing line = %d\n",line);
     zvmessage(ph2_aline,0);
   }
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end get_gll_ph2_edr_ln */

/*****************************************************************************/
/* Write one image line to an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_ph2_edr_ln(unit, line, source)
int            unit,             /* must be an open file with COND BINARY set */
               line;             /* image line number */

ssi_edr_line_typ *source;
{  
   int ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_ph2_prefix adds nlb to "line"          */
   /* internally.                                                           */

    write_ssi_ph2_prefix(unit, line, &source->prefix);

   /* Fill the dest structure, bypassing the binary prefix.                 */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvwrit(unit, &source->pixels[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /* end write_gll_ph2_edr_ln */

/*****************************************************************************/
/* Get one image line from an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_ph2_udr_ln(unit, line, dest)
int            unit,               /* must be an open file with COND BINARY set */
               line;               /* image line number */
ssi_line_typ *dest;
{  
   int ns, nlb,
       status = SUCCESS;
 
   /* Grab the binary prefix. get_ssi_ph2_prefix adds nlb to "line" internally. */

   get_ssi_ph2_prefix(unit, line, &dest->prefix);

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* filled. The pixel array is of type UBYTE. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   if (status != SUCCESS) return status;

   status = zvread(unit, &dest->pixels[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	

   return status;
} /*end get_gll_ph2_udr_ln */

/*****************************************************************************/
/* Write one image line to an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_ph2_udr_ln(unit, line, source)
int            unit,            /* must be an open file with COND BINARY set */
               line;            /* image line number */
ssi_line_typ *source;
{  
   int ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_ph2_prefix adds nlb to "line"          */
   /* internally.                                                           */
    write_ssi_ph2_prefix(unit, line, &source->prefix);

   /* Fill the dest structure, bypassing the binary prefix. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   if (status != SUCCESS) return status;

   status = zvwrit(unit, &source->pixels[0], 
                "LINE", line+nlb,
                "SAMP", BYTE_PREFIX_SZ+1,  
                "NSAMPS", ns,
                NULL);	

   return status;
} /*end write_gll_ph2_udr_ln */


/****************************************************************************/
/* Read in a GLL SSI EDR binary telemetry header, and translate it to native	*/
/* format.  The structure of the file is defined by how the VAX/VMS	*/
/* compiler interprets the structures in "gll_ph2_ssi_edr.h".  The		*/
/* structure cannot be used to overlay the data read from the file,	*/
/* since different compilers will pack the structure differently.  So,	*/
/* the data elements are in the order they are defined in the structure,*/
/* packed together as tightly as possible.				*/

/* The caller is responsible for setting an error action to deal with	*/
/* any errors that occur during the read.				*/

/* This is not technically correct because a signed conversion (HALF or	*/
/* FULL) is used for numbers defined to be unsigned... but this is ok	*/
/* for all currently defined integer types.				*/

/* The strings could be defined to be longer in the destination		*/
/* structure, and a NULL terminator appended.  The dest structure does	*/
/* NOT have to have any resemblance to the file structure if it's not	*/
/* convenient.  Constants are used for string lengths; it is assumed	*/
/* these are defined in the file format and will not change.  A sizeof	*/
/* operator should not be used on the destination string, as that may	*/
/* not be the same size as the file.  Symbolic constants would be	*/
/* better, but since this and the associated write routine are the only	*/
/* routines that should touch the file structure, literal constants	*/
/* are acceptable here.							*/

/* An assumption is made that fields that are declared "char x[n]",	*/
/* such as trunc_pixel and i_f, really *are* characters.  If this is	*/
/* not the case, the conversions will have to be modified for those	*/
/* fields.								*/

ROUTINE int get_ssi_ph2_telem_hdr(unit, dest)
int unit;		/* must be an open file with COND BINARY set */
ssi_hdr_typ *dest;
{
   unsigned char *buf,
                 *p;
   int            recsize,i,status = SUCCESS,
                  nrecs;   /* number of records to get */

   init_trans_inb(unit,byte_trans,half_trans,full_trans); 
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   status = zvget(unit, "RECSIZE", &recsize, NULL);
   zvsignal(unit,status,TRUE);

   if (recsize == 1800) nrecs =1;
   else if (recsize == 1000) nrecs = 2;
   else return BAD_MALLOC;

   buf = (unsigned char *) malloc (recsize*nrecs);
   if (buf == NULL) 
     return BAD_MALLOC;
   p = buf;

    /* Fill up buf with the entire telemetry header, reading the number of    */
   /* records indicated by the record size.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvread(unit,&buf[i*recsize],"LINE",1+i,NULL);
     if (status != SUCCESS) return status;
   }

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   TBYTE(p, &dest->record_id);
   TBYTE(p, &dest->reserved1);
   TSTRN(p, dest->project, 10);
   TSTRN(p, dest->instrument, 6);
   THALF(p, &dest->reserved2);
   THALF(p, &dest->log_seq);
   trin_ert_typ(&p, &dest->first_ert);	/* increments p */
   trin_ert_typ(&p, &dest->last_ert);
   trin_sclk_typ(&p, &dest->first_sclk_rec);
   trin_sclk_typ(&p, &dest->last_sclk_rec);
   trin_scet_typ(&p, &dest->scet);
   TSTRN(p, dest->mips_prd.os, 8);
   TSTRN(p, dest->mips_prd.reserved1, 26);
   TSTRN(p, dest->mips_prd.cpu, 8);
   TSTRN(p, dest->mips_prd.date, 11);
   TSTRN(p, dest->mips_prd.filler, 6);
   THALF(p, &dest->rec_format);
   TFULL(p, &dest->reserved3A);
   TBYTE(p, &dest->boom);
   THALF(p, &dest->miss_lines);
   THALF(p, &dest->part_lines);
   THALF(p, &dest->reserved4);
   THALF(p, &dest->seq_break);
   THALF(p, &dest->reserved5[0]);  
   THALF(p, &dest->reserved5[1]);  
   THALF(p, &dest->reserved5[2]);   /*  or p += 3 * half_size  */  
   THALF(p, &dest->sfdus);
   TSTRN(p, dest->pic_no, 7);
/*   trin_ssi_lrs_p_typ(&p, &dest->ssi_lrs);   */
   p += 12 * byte_size;              /*  reserved6[12]    */
   trin_flg_comp_typ(&p, &dest->flags);
   TSTRN(p, dest->mean_dn, 6);
   TSTRN(p, dest->trun_bits, 6);
   TSTRN(p, dest->trun_pixel, 6);
   TSTRN(p, dest->mean_if, 12);
   TSTRN(p, dest->entrop_avg, 7);
   TSTRN(p, dest->entropies, 15 * 7);
   TSTRN(p, dest->pointing, 3*8);
   TSTRN(p, dest->scale_fact, 2 * 8);
   TSTRN(p, dest->slope_file, 32);
   TSTRN(p, dest->offset_file, 32);
   TSTRN(p, dest->activity, 20);
   p += byte_size;                                 /* skip filler_1 */
   TBYTE(p, &dest->filter);
   TBYTE(p, &dest->exposure);
   TBYTE(p, &dest->image_mode);
   TBYTE(p, &dest->gain);
   TFULL(p, &dest->range);
   TBYTE(p, &dest->reserved7);
   THALF(p, &dest->version);
   trin_sclk_typ(&p, &dest->start_sclk_img);
   trin_sclk_typ(&p, &dest->end_sclk_img);

   TSTRN(p, dest->ssi3_pkt.ra, 8);
   TSTRN(p, dest->ssi3_pkt.dec, 8);
   TSTRN(p, dest->ssi3_pkt.twist, 8);
   TSTRN(p, dest->ssi3_pkt.clock_angle, 8);
   TBYTE(p, &dest->ssi3_pkt.ccd_temp_fine);
   TBYTE(p, &dest->ssi3_pkt.ccd_temp_coarse);
   TBYTE(p, &dest->ssi3_pkt.pic_no);
   trin_word23_typ(&p, &dest->ssi3_pkt.word23);
   trin_word24_typ(&p, &dest->ssi3_pkt.word24);
   trin_word25_typ(&p, &dest->ssi3_pkt.word25);
   trin_word26_typ(&p, &dest->ssi3_pkt.word26);

/*
   TBYTE(p, &dest->ssi3_pkt.word23);
   TBYTE(p, &dest->ssi3_pkt.word24);
   TBYTE(p, &dest->ssi3_pkt.word25);
   TBYTE(p, &dest->ssi3_pkt.word26);

   THALF(p, &dest->ssi3_pkt.stan_hskping[0]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[1]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[2]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[3]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[4]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[5]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[6]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[7]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[8]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[9]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[10]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[11]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[12]);

   zvtrans(half_trans, p, dest->ssi3_pkt.stan_hskping, 13);
   p += 13 * half_size;                                      skip reserved */

   p += 279 * byte_size;                                    /* skip reserved */
   zvtrans(full_trans, p, dest->histogram, 256);

   return SUCCESS;	
} /* end get_ssi_ph2_telem_hdr */

/****************************************************************************/
/* write the telemetry header out                                           */
/****************************************************************************/
int write_ssi_ph2_telem_hdr(unit, source)
int              unit;		/* must be an open file with COND BINARY set */
ssi_hdr_typ *source;
{
   unsigned char *buf, 
                 *p;
   int            recsize,i,status = SUCCESS,
                  nrecs;   /* number of records to get */

   init_trans_out(byte_trans,half_trans,full_trans);
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   status = zvget(unit, "RECSIZE", &recsize, NULL);
   zvsignal(unit,status,TRUE);

   if (recsize == 1800) nrecs =1;
   else if (recsize == 1000) nrecs = 2;
   else return BAD_MALLOC;

   buf = (unsigned char *) malloc (recsize*nrecs);
   if (buf == NULL) 
     return BAD_MALLOC;
   p = buf;

   /* Fill in the buffer with elements from the structure.  Bytes are       */
   /* translated (although that's probably unnecessary, it's a good idea),  */
   /* while characters are just moved.		         	      	    */

   TOBYTE(&source->record_id, p);
   TOBYTE(&source->reserved1, p);
   TOSTRN(source->project, p, 10);
   TOSTRN(source->instrument, p, 6);
   TOHALF(&source->reserved2, p);
   TOHALF(&source->log_seq, p);
   trout_ert_typ(&source->first_ert, &p);	/* increments p */
   trout_ert_typ(&source->last_ert, &p);
   trout_sclk_typ(&source->first_sclk_rec, &p);
   trout_sclk_typ(&source->last_sclk_rec, &p);
   trout_scet_typ(&source->scet, &p);
   TOSTRN(source->mips_prd.os, p, 8);
   TOSTRN(source->mips_prd.reserved1, p, 26);
   TOSTRN(source->mips_prd.cpu, p, 8);
   TOSTRN(source->mips_prd.date, p, 11);
   TOSTRN(source->mips_prd.filler, p, 6);
   TOHALF(&source->rec_format, p);
   TOFULL(&source->reserved3A, p);
   TOBYTE(&source->boom, p);
   TOHALF(&source->miss_lines, p);
   TOHALF(&source->part_lines, p);
   TOHALF(&source->reserved4, p);
   TOHALF(&source->seq_break, p);
   TOHALF(&source->reserved5[0], p);
   TOHALF(&source->reserved5[1], p);
   TOHALF(&source->reserved5[2], p);
   TOHALF(&source->sfdus, p);
   TOSTRN(source->pic_no, p, 7);
/*   trout_ssi_lrs_p_typ(&source->ssi_lrs, &p);   */
   p += 12 * byte_size;
   trout_flg_comp_typ(&source->flags, &p);
   TOSTRN(source->mean_dn, p, 6);
   TOSTRN(source->trun_bits, p, 6);
   TOSTRN(source->trun_pixel, p, 6);
   TOSTRN(source->mean_if, p, 12);
   TOSTRN(source->entrop_avg, p, 7);
   TOSTRN(source->entropies, p, 15 * 7);
/*   trout_j2000_ascii_typ(&source->point_dir, &p); */
   TOSTRN(source->pointing, p, 3 * 8);
   TOSTRN(source->scale_fact, p, 2 * 8);
   TOSTRN(source->slope_file, p, 32);
   TOSTRN(source->offset_file, p, 32);
   TOSTRN(source->activity, p, 20);   
   p += byte_size;                           /* skip filler_1 */
   TOBYTE(&source->filter, p);
   TOBYTE(&source->exposure, p);
   TOBYTE(&source->image_mode, p);
   TOBYTE(&source->gain, p);
   TOFULL(&source->range, p);
   TOBYTE(&source->reserved7, p);
   TOHALF(&source->version, p);
   trout_sclk_typ(&source->start_sclk_img, &p);
   trout_sclk_typ(&source->end_sclk_img, &p);   

   TOSTRN(source->ssi3_pkt.ra, p, 8);
   TOSTRN(source->ssi3_pkt.dec, p, 8);
   TOSTRN(source->ssi3_pkt.twist, p, 8);
   TOSTRN(source->ssi3_pkt.clock_angle, p, 8);
   TOBYTE(&source->ssi3_pkt.ccd_temp_fine, p);
   TOBYTE(&source->ssi3_pkt.ccd_temp_coarse, p);
   TOBYTE(&source->ssi3_pkt.pic_no, p);
   trout_word23_typ(&source->ssi3_pkt.word23,&p);
   trout_word24_typ(&source->ssi3_pkt.word24,&p);
   trout_word25_typ(&source->ssi3_pkt.word25,&p);
   trout_word26_typ(&source->ssi3_pkt.word26,&p);

/* commented out -----
   TOBYTE(&source->ssi3_pkt.word23, p);
   TOBYTE(&source->ssi3_pkt.word24,P);
   TOBYTE(&source->ssi3_pkt.word25,P);
   TOBYTE(&source->ssi3_pkt.word26,P);

   zvtrans(half_trans,source->ssi3_pkt.stan_hskping, p , 13);
   p += 13 * half_size;                                  -----skip reserved */

   p += 279 * byte_size;                                    /* skip reserved */
   zvtrans(full_trans, source->histogram, p, 256);

   /* Write out the entire telemetry header, by writing the number of        */
   /* records indicated by the data format.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvwrit(unit,&buf[i*recsize],"LINE",1+i,NULL);
     if (status != SUCCESS) return status;
   }

   return SUCCESS;
} /* end write_ssi_ph2_telem_hdr */

/*****************************************************************************/
/* This routine translates one line of binary image line prefix into native  */
/* format (DEFAULT_HOST) and writes it out to the file specified by ounit.   */
/*****************************************************************************/
ROUTINE int write_ssi_ph2_prefix(ounit,line,source)
int           ounit,
              line;
ssi_prefix_typ *source;
{
  unsigned char  buf[SSI_PRFX_LEN],
		*p = buf;
  UBYTE          bits_byte = 0;      
  UINT           bits_full = 0;
  int            status = SUCCESS,
                 nlb,          /* # lines of binary header */
                 nbb;          /* # bytes of binary prefix data */

 status =  zvget(ounit, "NLB", &nlb, "NBB", &nbb, NULL);
 if (status != SUCCESS) return status;
 if (nbb > SSI_PRFX_LEN) return BAD_MALLOC;

 init_trans_out(byte_trans,half_trans,full_trans);
 init_pixsizeb(ounit,&byte_size,&half_size,&full_size);
   
 TOBYTE(&source->record_id, p);
 TOBYTE(&source->reserved1A, p);
 TOHALF(&source->reserved1B, p);

 TOHALF(&source->log_seq, p);

 TOHALF(&source->ert.year, p);
 TOHALF(&source->ert.day, p);
 TOBYTE(&source->ert.hour, p);
 TOBYTE(&source->ert.minute, p);
 TOBYTE(&source->ert.second, p);
 TOHALF(&source->ert.msecond, p);

 TOFULL(&source->sclk.rim, p);
 TOBYTE(&source->sclk.mod91, p);
 TOBYTE(&source->sclk.mod10, p);
 TOBYTE(&source->sclk.mod8, p);

 TOSTRN(source->mips_prd.os, p, 8);
 TOSTRN(source->mips_prd.reserved1, p, 26);
 TOSTRN(source->mips_prd.cpu, p, 8);
 TOSTRN(source->mips_prd.date, p, 11);
 TOSTRN(source->mips_prd.filler, p, 6);

 TOHALF(&source->record_format, p) ;
 TOBYTE(&source->input_type, p);

 /* input source */
 if (source->source.SFDU)  bits_byte  |= 0x01;
 if (source->source.WBDL)  bits_byte  |= 0x02;
 if (source->source.SDR)   bits_byte  |= 0x04;
 if (source->source.IDR)   bits_byte  |= 0x08;
 if (source->source.EDR)   bits_byte  |= 0x10;
 if (source->source.MIPS)  bits_byte  |= 0x20;
 if (source->source.APB)   bits_byte  |= 0x40;
 TOBYTE(&bits_byte, p);

 TOBYTE(&source->reserved3A, p);
 TOBYTE(&source->reserved3B, p);
 zvtrans(byte_trans, &source->reserved3C[0], p, 12);
 p += 12 * byte_size;
 
 TOHALF(&source->reserved3D, p);
 TOHALF(&source->reserved3E, p);

 bits_full |= (source->trun_bits.block0 & 0x03);
 bits_full |= (source->trun_bits.block1 & 0x03) << 2;
 bits_full |= (source->trun_bits.block2 & 0x03) << 4;
 bits_full |= (source->trun_bits.block3 & 0x03) << 6;
 bits_full |= (source->trun_bits.block4 & 0x03) << 8;
 bits_full |= (source->trun_bits.block5 & 0x03) << 10;
 bits_full |= (source->trun_bits.block6 & 0x03) << 12;
 bits_full |= (source->trun_bits.block7 & 0x03) << 14;
 bits_full |= (source->trun_bits.block8 & 0x03) << 16;
 bits_full |= (source->trun_bits.block9 & 0x03) << 18;
 bits_full |= (source->trun_bits.block10 & 0x03) << 20;
 bits_full |= (source->trun_bits.block11 & 0x03) << 22;
 bits_full |= (source->trun_bits.block12 & 0x03) << 24;
 TOFULL(&bits_full, p);

 TOHALF(&source->trun_pix, p);
 TOHALF(&source->version, p);
 TOHALF(&source->reserved4, p);
 TOBYTE(&source->dsn_id, p);
 TOHALF(&source->line_no, p);
 TOBYTE(&source->reserved5, p);
 TOHALF(&source->ss1, p);
 TOHALF(&source->es1, p);
 TOHALF(&source->ss2, p);
 TOHALF(&source->es2, p);
 bits_byte = 0;
 bits_byte |= (source->line_con.part_pkts & 0x0F);
 bits_byte |= (source->line_con.full_pkts & 0x0F) << 4;
 
 TOBYTE(&bits_byte, p);

 TOBYTE(&source->apid, p);
 TOFULL(&source->pkt_seq, p);
 TOHALF(&source->pkt_pix_srt, p);
 TOHALF(&source->strt_pix, p);
 TOHALF(&source->stop_pix, p);
 TOHALF(&source->rct.year, p);
 TOHALF(&source->rct.day, p);
 TOBYTE(&source->rct.hour, p);
 TOBYTE(&source->rct.minute, p);
 TOBYTE(&source->rct.second, p);
 TOHALF(&source->rct.msecond, p);
 TOBYTE(&source->decomp_stat, p);

 TOSTRN(source->comp_ratio, p, 6);

 zvtrans(byte_trans, &source->reserved6[0], p, 47);
 p += 47 * byte_size;

 status = zvwrit(ounit, buf, "LINE", line+nlb, "NSAMPS", nbb, NULL);

 return status;
} /* end write_ssi_ph2_prefix */

/*****************************************************************************/
/* Translation routines for specific datatypes: translate any input host fmt */
/*****************************************************************************/
ROUTINE static void trin_scet_typ(from, to)
unsigned char **from;
scet_typ *to;
{
   trin_ert_typ(from, (ert_typ *)to);		/* scet_typ same as ert_typ */
}


ROUTINE static void trin_ert_typ(from, to)
unsigned char **from;
ert_typ *to;
{
   THALF(*from, &to->year);
   THALF(*from, &to->day);
   TBYTE(*from, &to->hour);
   TBYTE(*from, &to->minute);
   TBYTE(*from, &to->second);
   THALF(*from, &to->msecond);
}

ROUTINE static void trin_sclk_typ(from, to)
unsigned char **from;
sclk_typ *to;
{
   TFULL(*from, &to->rim);
   TBYTE(*from, &to->mod91);
   TBYTE(*from, &to->mod10);
   TBYTE(*from, &to->mod8);
}

ROUTINE static void trin_flg_comp_typ(from, to)
unsigned char **from;
flag_comp_typ *to;
{
   unsigned short int bits;

   zvtrans(half_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += half_size;
   to->barc_comp = bits & 0x01;
   to->barc_mode = (bits >> 1) & 0x01;
   to->expo = (bits >> 2) & 0x01;
   to->flood = (bits >> 3) & 0x01;
   to->blem = (bits >> 4) & 0x01;
   to->clock = (bits >> 5) & 0x01;
   to->ict_comp = (bits >> 6) & 0x01;
   to->huff_comp = (bits >> 7) & 0x01;
   /* to->filler is not set */
}

/*****************************************************************************/
/* Translation routines for specific datatypes: write NATIVE output          */
/*****************************************************************************/
ROUTINE static void trout_scet_typ(from, to)
scet_typ *from;
unsigned char **to;
{
   trout_ert_typ((ert_typ *)from, to);		/* scet_typ same as ert_typ */
}

ROUTINE static void trout_ert_typ(from, to)
ert_typ *from;
unsigned char **to;
{
   TOHALF(&from->year, *to);
   TOHALF(&from->day, *to);
   TOBYTE(&from->hour, *to);
   TOBYTE(&from->minute, *to);
   TOBYTE(&from->second, *to);
   TOHALF(&from->msecond, *to);
}

ROUTINE static void trout_sclk_typ(from, to)
sclk_typ *from;
unsigned char **to;
{
   TOFULL(&from->rim, *to);
   TOBYTE(&from->mod91, *to);
   TOBYTE(&from->mod10, *to);
   TOBYTE(&from->mod8, *to);
}

ROUTINE static void trout_flg_comp_typ(from, to)
flag_comp_typ *from;
unsigned char **to;
{
   unsigned short int bits = 0;
   if (from->barc_comp)     bits |= 0x01;
   if (from->barc_mode) bits |= 0x02;
   if (from->expo)     bits |= 0x04;
   if (from->flood)    bits |= 0x08;
   if (from->blem)     bits |= 0x10;
   if (from->clock)    bits |= 0x20;
   if (from->ict_comp)    bits |= 0x40;
   if (from->huff_comp)    bits |= 0x80;
   zvtrans(half_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += half_size;
   /* from->filler is not set */
}
/****************************************************************************/
/* This routine sets up translation buffers for output, converting from the */
/* machine's native representation into the host representation.            */
/****************************************************************************/
ROUTINE static void init_trans_out(byte_tr,half_tr,full_tr)
int *byte_tr,
    *half_tr,
    *full_tr;
{
   char intfmt[12],realfmt[12];
   int status;

   status = zvhost(DEFAULT_HOST,intfmt,realfmt);
   status = zvtrans_out(byte_tr, "BYTE", "BYTE", intfmt, realfmt);
   status = zvtrans_out(half_tr, "HALF", "HALF", intfmt, realfmt);
   status = zvtrans_out(full_tr, "FULL", "FULL", intfmt, realfmt);
 } /* end init_trans_out */

/****************************************************************************/
/* This routine sets up translation buffers for input, converting from the  */
/* host representation into the machine's native representation.            */
/****************************************************************************/
ROUTINE static void init_trans_inb(unit,byte_tr,half_tr,full_tr)
int unit,
    *byte_tr,
    *half_tr,
    *full_tr;
{
   int status = SUCCESS;

   status = zvtrans_inb(byte_tr, "BYTE", "BYTE", unit);
   zvsignal(unit,status,TRUE);
   status = zvtrans_inb(half_tr, "HALF", "HALF", unit);
   zvsignal(unit,status,TRUE);
   status = zvtrans_inb(full_tr, "FULL", "FULL", unit);
   zvsignal(unit,status,TRUE);
 } /* end init_trans_inb */

/****************************************************************************/
/* This routine returns the size of a binary label value (in bytes) from a  */
/* file.								    */
/****************************************************************************/
ROUTINE static void init_pixsizeb(unit,byte_sz,half_sz,full_sz)
int unit,
    *byte_sz,
    *half_sz,
    *full_sz;
{
   int status;
   char ph2_aline[80];
 
   status = zvpixsizeb(byte_sz, "BYTE", unit);
   zvsignal(unit,status,TRUE);
   if (byte_sz == 0) {
     sprintf(ph2_aline,
     "init_pixsizeb> error in byte pixel size determination, status %d",byte_sz);
     zvmessage(ph2_aline,0);
     zabend();
   }

   status = zvpixsizeb(half_sz, "HALF", unit);
   zvsignal(unit,status,TRUE);
   if (half_sz == 0) {
     sprintf(ph2_aline,
    "init_pixsizeb> error in halfword pixel size determination, status %d",half_sz);
     zvmessage(ph2_aline,0);
     zabend();
   }

   status = zvpixsizeb(full_sz, "FULL", unit);
   zvsignal(unit,status,TRUE);
   if (full_sz == 0) {
     sprintf(ph2_aline,
    "init_pixsizeb> error in fullword pixel size determination, status %d",full_sz);
     zvmessage(ph2_aline,0);
     zabend();
   }
} /* end init_pixsizeb */

ROUTINE static void trin_word23_typ(from, to)
unsigned char **from;
word23_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->exposure_no = bits & 0x1F;  /* bits 0-4, bits 0 is the rightmost(lsb) */
   to->cmnd_gain = (bits >> 5) & 0x03;   /*   bits 5-6   */
   to->flood = (bits >> 7) & 0x01;   /*  bit 7  */
}

ROUTINE static void trin_word24_typ(from, to)
unsigned char **from;
word24_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->cmnd_filter_pos = bits & 0x07;         /*  bits 0-2    */
   to->cmnd_filter_step = (bits >> 3) & 0x01; /*  bits 3      */
   to->cmnd_blemish = (bits >> 4) & 0x01;     /*  bits 4      */
   to->cmnd_expo_mode = (bits >>5) & 0x01;    /*  bits 5      */
   to->cmnd_expo_cycle = (bits>>6) & 0x01;    /*  bits 6      */
   to->skip = bits >> 7; /*  bit 7  */
}

ROUTINE static void trin_word25_typ(from, to)
unsigned char **from;
word25_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->state_used = bits & 0x03;        /*   bits 0-1   */
   to->comp_stat = (bits >>2) & 0x03;    /*   bits  2-3  */
   to->long_expo = (bits >> 4) & 0x01;      /*   bits  4  */
   to->img_mode = (bits >> 5) & 0x07;      /*   bits  5-7  */
}

ROUTINE static void trin_word26_typ(from, to)
unsigned char **from;
word26_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->odd_parity = bits & 0x01;         /*   bit 0        */
   to->filter = (bits >> 1) & 0x07;      /*   bits   1-3   */
   to->blemish = (bits >> 4) & 0x01;     /*   bits   4     */
   to->watchdog = (bits >> 5) & 0x01;    /*   bits   5     */
   to->par_clock = (bits >> 6) & 0x01;   /*   bits   6     */
   to->mem_write = (bits >> 7) & 0x01;   /*   bits   7     */
}

ROUTINE static void trout_word23_typ(from, to)
word23_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->flood;                      /*   bits 7     */
   bits <<= 2;
   bits |= from->cmnd_gain;                  /*   bits 5-6   */
   bits <<= 5;
   bits |= from->exposure_no;                /*   bits 0-4   */
   zvtrans(byte_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}

ROUTINE static void trout_word24_typ(from, to)
word24_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->cmnd_expo_cycle;                       /*  bits 6    */
   bits <<= 1;
   bits |= from->cmnd_expo_mode;                        /*  bits 5    */
   bits <<= 1;
   bits |= from->cmnd_blemish;                          /*  bits 4    */
   bits <<= 1;
   bits |= from->cmnd_filter_step;                      /*  bits 3    */
   bits <<= 3;
   bits |= from->cmnd_filter_pos;                       /*  bits 0-2  */
   zvtrans(byte_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}

ROUTINE static void trout_word25_typ(from, to)
word25_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->img_mode;                     /*   bits 5-6   */
   bits <<= 1;
   bits |= from->long_expo;                    /*   bits 4     */
   bits <<= 2;
   bits |= from->comp_stat;                    /*   bits 2-3   */
   bits <<= 2;
   bits |= from->state_used;                   /*   bits 0-1   */

   zvtrans(byte_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}

ROUTINE static void trout_word26_typ(from, to)
word26_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->mem_write;                     /*  bits 7     */
   bits <<= 1;
   bits |= from->par_clock;                     /*  bits 6     */
   bits <<= 1;
   bits |= from->watchdog;                      /*  bits 5     */
   bits <<= 1;
   bits |= from->blemish;                       /*  bits 4     */
   bits <<= 3;
   bits |= from->filter;                        /*  bits 1-3   */
   bits <<= 1;
   bits |= from->odd_parity;                    /*  bits 0     */

   zvtrans(byte_trans, &bits, *to, 1);     /*  byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}
/* end module */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gllcalname.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"
#include "zvproto.h"
#include <ctype.h>
#include <string.h>

/* prototypes: */
int zgllcalname( char *, int *,  int *, int *, int *, int *, int *);
int gllradcal( char *, int *,  int *);
int glldccal( char *, int *,  int *, int *, int *, int *);
int gllblemcal( char *, int *,  int *, int *);
int gllsocal( char *);

/****************************************************************************
  These external variables are only needed by the routines in this
  module.  Once a calibration catelog has been implemented these static
  variables can be removed (of course the code needs to be changed too).
*****************************************************************************/

static char	*fltr_strg[] = {"CLR", "GRN", "RED", "VLT", 
                                "756" ,"968", "727", "889"},
		*gain_strg[] = {"?", "1", "2", "3", "4"},
		*mode_strg[] = {"F", "S"},
		*type_strg[] = {".NUL", ".CAL", ".DC", ".BLM", ".SO"},
		*rate_strg[] = {"?", "2", "8", "30", "60", "15"};

/****************************************************************************
		             GLLCALNAME

   Returns a calibration file name based on various parameters.
*****************************************************************************
                        Fortran-Callable Version 
*****************************************************************************/

void FTN_NAME (gllcalname)(file_name,type,status,filter,frame_rate,gain,fibe, 
                           rmode,FORSTR_PARAM)
  char	*file_name;	/*  Returned file name	*/
  int	*type,		/*  Requested calibration file type	*/
			/*	1 - Radiometric		*/
			/*	2 - Dark Current	*/
			/*	3 - Blemish		*/
			/*	4 - Shutter Offset	*/
	*status,	/*  Returned status indicator	*/
	*filter,	/*  Filter	*/
	*frame_rate,	/*  Frame rate (Indirectly includes frame mode)	*/
	*gain,		/*  Gain state	*/
	*fibe,		/*  Clock_invert; Blem_prot & Ext_exposure	*/
        *rmode;		/*  SSI readout mode (0=not applicable,1=sample,
			    2=contiguous)	*/
FORSTR_DEF
{
   FORSTR_BLOCK
   char c_string [256];

   /* Obtain the filename of the proper calibration file from the set of
   passed-in defining parameters */

   *status = zgllcalname (c_string,type,filter,frame_rate,gain,fibe,rmode);

   /* Convert the returned c_string file name to a FORTRAN string 
   before returning to the calling FORTRAN program*/

   sc2for (c_string, 0, file_name, &file_name, 8, 1, 1);
   zccase(file_name, -1, strlen(file_name));

   return;
}

/****************************************************************************
                         C-Callable Version
*****************************************************************************/

int zgllcalname (file_name,type,filter,frame_rate,gain,fibe,rmode)
  char	*file_name;	/*  Returned file name	*/
  int	*type,		/*  Requested calibration file type	*/
			/*	1 - Radiometric		*/
			/*	2 - Dark Current	*/
			/*	3 - Blemish		*/
			/*	4 - Shutter Offset	*/
	*filter,	/*  Filter	*/
	*frame_rate,	/*  Frame rate (Indirectly includes frame mode)	*/
	*gain,		/*  Gain state	*/
	*fibe,		/*  Clock_invert; Blem_prot & Ext_exposure	*/
	*rmode;		/*  SSI readout mode (0=not applicable,1=sample,
			    2=contiguous) 	*/
{ 

  int   status;	        /*  Returned status indicator	*/
  int   frame_mode;     /* Frame mode */

  /***  Detemine frame mode from frame_rate parameter  ***/
  /***  0 - Full Frame;  1 - Summation mode         ***/
  if (*frame_rate==1 || *frame_rate==5) frame_mode=1;
  else frame_mode=0;

  /***  Call routines for different supported calibration files  ***/

  switch (*type) {

    case 1: /***  Radiometric Calibration File  ***/
            status = gllradcal(file_name,filter,&frame_mode);
            zccase(file_name, -1, strlen(file_name));
            break;

    case 2: /***  Dark_Current Calibration File  ***/
            status = glldccal(file_name,&frame_mode,gain,frame_rate,fibe,rmode);
            zccase(file_name, -1, strlen(file_name));
            break;

    case 3: /***  Blemish Calibration File  ***/
            status = gllblemcal(file_name,filter,&frame_mode,gain);
            zccase(file_name, -1, strlen(file_name));
            break;

    case 4: /***  Shutter Offset Calibration File  ***/
            status = gllsocal(file_name);
            zccase(file_name, -1, strlen(file_name));
            break;

    default:
            zvmessage ("GLLCALNAME - Invalid calibration file type requested","");
            status = FALSE; /***  SOME BAD NEWS INDICATOR  ***/

         break;
  }

  return status;
}

/****************************************************************************
				GLLRADCAL

  Returns the radiometric calibration file based on filter position
  and frame mode.
*****************************************************************************/

int gllradcal(file_name,filter,frame_mode)
  char	*file_name;
  int	*filter,
	*frame_mode;
{
  if ((*filter < 0 || *filter > 7) || (*frame_mode < 0 || *frame_mode > 1))
  { zvmessage ("GLLRADCAL - Invalid filter position or frame mode","");
    return FALSE;
  }

  strcpy(file_name,fltr_strg[*filter]);
  strcat(file_name,mode_strg[*frame_mode]);
  strcat(file_name,type_strg[1]);
  return TRUE;
}

/****************************************************************************
				GLLDCCAL

  Returns the Dark Current calibration file name based on frame mode,
  gain state, frame rate and FIBE flags.
*****************************************************************************/

int glldccal(file_name,frame_mode,gain,frame_rate,fibe,rmode)
  char	*file_name;
  int	*frame_mode,
	*gain,
	*frame_rate,
	*fibe,
	*rmode;
{
  if ((*frame_mode < 0 || *frame_mode > 1) || (*gain < 1 || *gain > 4) ||
      (*frame_rate < 1 || *frame_rate > 5))
  { zvmessage ("GLLDCCAL - Invalid frame mode, gain state or frame rate","");
    return FALSE;
  }
  strcpy(file_name,gain_strg[*gain]);
  strcat(file_name,mode_strg[*frame_mode]);
  strcat(file_name,rate_strg[*frame_rate]);
  if (((*fibe % 1000)/100) == 1) strcat(file_name,"I");
  if (((*fibe % 100)/10) == 1) strcat(file_name,"B");
  if (((*fibe % 10) == 1) || ((*fibe / 100000) == 1)) strcat(file_name,"X");
  if (*rmode == 1) strcat(file_name,"R");
  if (*rmode == 2) strcat(file_name,"C");
  strcat(file_name,type_strg[2]);
  return TRUE;
}

/****************************************************************************
				GLLBLEMCAL

  Returns the Blemish calibration file name based on the filter position,
  frame mode and gain state.
*****************************************************************************/
int gllblemcal(file_name,filter,frame_mode,gain)
  char	*file_name;
  int	*filter,
	*frame_mode,
	*gain;
{
  if ((*filter < 0 || *filter > 7) || (*frame_mode < 0 || *frame_mode > 1) ||
      (*gain < 1 || *gain > 4))
  { zvmessage 
        ("GLLBLEMCAL - Invalid filter position, frame mode or gain state","");
    return FALSE;
  }
  strcpy(file_name,fltr_strg[*filter]);
  strcat(file_name,gain_strg[*gain]);
  strcat(file_name,mode_strg[*frame_mode]);
  strcat(file_name,type_strg[3]);
  return TRUE;
}

/****************************************************************************
				GLLSOCAL

	Returns the Shutter Offset calibration file name (only one for now).
*****************************************************************************/
int gllsocal(file_name)
  char	*file_name;
{
  strcpy(file_name,"CALIBRATION");
  strcat(file_name,type_strg[4]);
  return TRUE;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_ssi_prefix.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include "xvmaininc.h"   
#include "gll_main.h"
#include "gll_lrs.h"
#include "gll_ssi_edr.h"
#include "zvproto.h"
/*******************************************************************************

Purpose:	This subroutine gets the GLL SSI Binary PREFIX information 
	
Written:	Sept 16,  1992 

Language:	C  (MSTP Portable)

Written by:	Wen-Piao  Lee  

*******************************************************************************/


/*  RGD's macros to reduce typing   */

#define TBYTE(from, to)  zvtrans(byte_conv,(from),(to),1); (from) += byte_size;
#define THALF(from, to)  zvtrans(half_conv,(from),(to),1); (from) += half_size;
#define TFULL(from, to)  zvtrans(full_conv,(from),(to),1); (from) += full_size;
#define TSTRN(from, to, n) zmove((from), (to), (n));  (from) += (n) ;


get_ssi_prefix (iu, lin, dest)  
int            iu ;
int            lin ;  
ssi_lhdr_typ   *dest ;

{
 int     ind ;               /*  Status indicator  */
 unsigned  char  *sorc ;
 unsigned  char  *pt ;
 int  byte_conv[12], byte_size ;
 int  half_conv[12], half_size ;
 int  full_conv[12], full_size ;
 UBYTE   bits_byte ;      
 UWORD   bits_half ;  
 UINT    bits_full ;
 static  int   nlb ; 
 static  int   nbb ;

 zvget(iu, "NLB", &nlb, "NBB", &nbb , NULL) ;
 pt = (unsigned char *) malloc (nbb) ;         
 zvread(iu, pt, "LINE", lin+nlb, "NSAMPS", nbb, NULL) ;
 
 sorc = pt ; 

      /*  set up translation buffers  */

 ind = zvtrans_inb(byte_conv, "BYTE", "BYTE", iu) ;
 ind = zvtrans_inb(half_conv, "HALF", "HALF", iu) ;
 ind = zvtrans_inb(full_conv, "FULL", "FULL", iu) ;

 ind = zvpixsizeb( &byte_size, "BYTE", iu) ;
 ind = zvpixsizeb( &half_size, "HALF", iu) ;
 ind = zvpixsizeb( &full_size, "FULL", iu) ;
   
 TBYTE(sorc, &dest->rec_id) ;
 TBYTE(sorc, &dest->file_no) ;
 THALF(sorc, &dest->phy_seq) ;

 THALF(sorc, &dest->log_seq) ;

 THALF(sorc, &dest->ert.year) ;
 THALF(sorc, &dest->ert.day) ;
 TBYTE(sorc, &dest->ert.hour) ;
 TBYTE(sorc, &dest->ert.minute) ;
 TBYTE(sorc, &dest->ert.second) ;
 THALF(sorc, &dest->ert.msecond) ;

 TFULL(sorc, &dest->sclk.rim) ;
 TBYTE(sorc, &dest->sclk.mod91) ;
 TBYTE(sorc, &dest->sclk.mod10) ;
 TBYTE(sorc, &dest->sclk.mod8) ;

 TSTRN(sorc, dest->mpr, 59) ;
 THALF(sorc, &dest->fid)  ;

 TBYTE(sorc, &dest->input.type) ;
 TBYTE(sorc, &bits_byte ) ;
 dest->input.SFDU =  bits_byte           &  01 ;
 dest->input.WBDL = (bits_byte >> 1)     &  01 ;
 dest->input.SDR =  (bits_byte >> 2)     &  01 ;
 dest->input.IDR =  (bits_byte >> 3)     &  01 ;
 dest->input.EDR =  (bits_byte >> 4)     &  01 ;
 dest->input.RT =   (bits_byte >> 5)     &  01 ;
 dest->input.APB =  (bits_byte >> 6)     &  01 ;

 TBYTE(sorc, &dest->allowed_sync_err) ;
 TBYTE(sorc, &dest->sync_err) ;
 zvtrans(byte_conv, sorc, &dest->ssi_lrs[0], 12) ;
 sorc += 12 * byte_size ;
 
 THALF(sorc, &dest->last_pix) ;
 THALF(sorc, &bits_half) ;
 dest->sync_stat.error =   bits_half & 0x3FFF ;
 dest->sync_stat.status = (bits_half >> 14)  &  0x03  ;

 TFULL(sorc, &bits_full) ;
 dest->truncation.blk0   =    bits_full          &   0x03 ;
 dest->truncation.blk1   =   (bits_full >>  2)   &   0x03 ;
 dest->truncation.blk2   =   (bits_full >>  4)   &   0x03 ;
 dest->truncation.blk3   =   (bits_full >>  6)   &   0x03 ;
 dest->truncation.blk4   =   (bits_full >>  8)   &   0x03 ;
 dest->truncation.blk5   =   (bits_full >> 10)   &   0x03 ;
 dest->truncation.blk6   =   (bits_full >> 12)   &   0x03 ;
 dest->truncation.blk7   =   (bits_full >> 14)   &   0x03 ;
 dest->truncation.blk8   =   (bits_full >> 16)   &   0x03 ;
 dest->truncation.blk9   =   (bits_full >> 18)   &   0x03 ;
 dest->truncation.blk10  =   (bits_full >> 20)   &   0x03 ;
 dest->truncation.blk11  =   (bits_full >> 22)   &   0x03 ;
 dest->truncation.blk12  =   (bits_full >> 24)   &   0x03 ;
   
 THALF(sorc, &dest->truncated) ;
 THALF(sorc, &dest->version) ;
 THALF(sorc, &dest->ssnr) ;
 TBYTE(sorc, &dest->dsn_id) ;
 THALF(sorc, &dest->image_line) ;
 TBYTE(sorc, &dest->rs_overflow) ;

 free (pt) ;  

}        /*    End of Subroutine       */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_ssi_ph2_prefix.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include "xvmaininc.h"   
#include "rts_typedefs.h" 
#include "gll_rts_main.h"
/* #include "gll_lrs.h" */
#include "gll_ph2_ssi_edr.h"
#include "zvproto.h"
/*******************************************************************************

Purpose:	This subroutine gets the GLL Phase II SSI Binary Line PREFIX 
                information 
	
Written:	August 10,  1994 

Language:	C  (MSTP Portable)

Written by:	F. Moss

*******************************************************************************/


/*  RGD's macros to reduce typing   */

#define TBYTE(from, to)  zvtrans(byte_conv,(from),(to),1); (from) += ph2_byte_size;
#define THALF(from, to)  zvtrans(half_conv,(from),(to),1); (from) += ph2_half_size;
#define TFULL(from, to)  zvtrans(full_conv,(from),(to),1); (from) += ph2_full_size;
#define TSTRN(from, to, n) zmove((from), (to), (n));  (from) += (n) ;


get_ssi_ph2_prefix (iu, lin, dest)  
int            iu ;
int            lin ;  
ssi_prefix_typ *dest ;

{
 int     ind ;               /*  Status indicator  */
 unsigned  char  *sorc ;
 unsigned  char  *pt ;
 int  byte_conv[12], ph2_byte_size ;
 int  half_conv[12], ph2_half_size ;
 int  full_conv[12], ph2_full_size ;
 UBYTE   bits_byte,      
         bits_byte1,      
         bits_byte2,      
         bits_byte3;      
 UWORD   bits_half ;  
 UINT    bits_full ;
 static  int   nlb ; 
 static  int   nbb ;

 zvget(iu, "NLB", &nlb, "NBB", &nbb , NULL) ;
 pt = (unsigned char *) malloc (nbb) ;         
 zvread(iu, pt, "LINE", lin+nlb, "NSAMPS", nbb, NULL) ;
 
 sorc = pt ; 

      /*  set up translation buffers  */

 ind = zvtrans_inb(byte_conv, "BYTE", "BYTE", iu) ;
 ind = zvtrans_inb(half_conv, "HALF", "HALF", iu) ;
 ind = zvtrans_inb(full_conv, "FULL", "FULL", iu) ;

 ind = zvpixsizeb( &ph2_byte_size, "BYTE", iu) ;
 ind = zvpixsizeb( &ph2_half_size, "HALF", iu) ;
 ind = zvpixsizeb( &ph2_full_size, "FULL", iu) ;
   
 TBYTE(sorc, &dest->record_id) ;
 TBYTE(sorc, &dest->reserved1A) ;
 THALF(sorc, &dest->reserved1B) ;

 THALF(sorc, &dest->log_seq) ;

 THALF(sorc, &dest->ert.year) ;
 THALF(sorc, &dest->ert.day) ;
 TBYTE(sorc, &dest->ert.hour) ;
 TBYTE(sorc, &dest->ert.minute) ;
 TBYTE(sorc, &dest->ert.second) ;
 THALF(sorc, &dest->ert.msecond) ;

 TFULL(sorc, &dest->sclk.rim) ;
 TBYTE(sorc, &dest->sclk.mod91) ;
 TBYTE(sorc, &dest->sclk.mod10) ;
 TBYTE(sorc, &dest->sclk.mod8) ;

 TSTRN(sorc, dest->mips_prd.os, 8);
 TSTRN(sorc, dest->mips_prd.reserved1, 26);
 TSTRN(sorc, dest->mips_prd.cpu, 8);
 TSTRN(sorc, dest->mips_prd.date, 11);
 TSTRN(sorc, dest->mips_prd.filler, 6);

 THALF(sorc, &dest->record_format)  ;

 TBYTE(sorc, &dest->input_type) ;
 TBYTE(sorc, &bits_byte ) ;
 dest->source.SFDU =  bits_byte           &  01 ;
 dest->source.WBDL = (bits_byte >> 1)     &  01 ;
 dest->source.SDR =  (bits_byte >> 2)     &  01 ;
 dest->source.IDR =  (bits_byte >> 3)     &  01 ;
 dest->source.EDR =  (bits_byte >> 4)     &  01 ;
 dest->source.MIPS = (bits_byte >> 5)     &  01 ;
 dest->source.APB =  (bits_byte >> 6)     &  01 ;

 TBYTE(sorc, &dest->reserved3A) ;
 TBYTE(sorc, &dest->reserved3B) ;
 zvtrans(byte_conv, sorc, &dest->reserved3C[0], 12) ;
 sorc += 12 * ph2_byte_size ;
 
 THALF(sorc, &dest->reserved3D) ;
 THALF(sorc, &dest->reserved3E) ;

 TFULL(sorc, &bits_full) ;
 dest->trun_bits.block0   =    bits_full          &   0x03 ;
 dest->trun_bits.block1   =   (bits_full >>  2)   &   0x03 ;
 dest->trun_bits.block2   =   (bits_full >>  4)   &   0x03 ;
 dest->trun_bits.block3   =   (bits_full >>  6)   &   0x03 ;
 dest->trun_bits.block4   =   (bits_full >>  8)   &   0x03 ;
 dest->trun_bits.block5   =   (bits_full >> 10)   &   0x03 ;
 dest->trun_bits.block6   =   (bits_full >> 12)   &   0x03 ;
 dest->trun_bits.block7   =   (bits_full >> 14)   &   0x03 ;
 dest->trun_bits.block8   =   (bits_full >> 16)   &   0x03 ;
 dest->trun_bits.block9   =   (bits_full >> 18)   &   0x03 ;
 dest->trun_bits.block10  =   (bits_full >> 20)   &   0x03 ;
 dest->trun_bits.block11  =   (bits_full >> 22)   &   0x03 ;
 dest->trun_bits.block12  =   (bits_full >> 24)   &   0x03 ;
   
 THALF(sorc, &dest->trun_pix) ;
 THALF(sorc, &dest->version) ;
 THALF(sorc, &dest->reserved4) ;
 TBYTE(sorc, &dest->dsn_id) ;
 THALF(sorc, &dest->line_no) ;
 TBYTE(sorc, &dest->reserved5) ;
 THALF(sorc, &dest->ss1) ;
 THALF(sorc, &dest->es1) ;
 THALF(sorc, &dest->ss2) ;
 THALF(sorc, &dest->es2) ;

 TBYTE(sorc, &bits_byte) ;
 dest->line_con.part_pkts =    bits_byte          &   0x0F ; 
 dest->line_con.full_pkts =   (bits_byte >>  4)   &   0x0F ;
 

 TBYTE(sorc, &dest->apid) ;
 TFULL(sorc, &dest->pkt_seq) ;
 THALF(sorc, &dest->pkt_pix_srt) ;
 THALF(sorc, &dest->strt_pix) ;
 THALF(sorc, &dest->stop_pix) ;
 THALF(sorc, &dest->rct.year) ;
 THALF(sorc, &dest->rct.day) ;
 TBYTE(sorc, &dest->rct.hour) ;
 TBYTE(sorc, &dest->rct.minute) ;
 TBYTE(sorc, &dest->rct.second) ;
 THALF(sorc, &dest->rct.msecond) ;
 TBYTE(sorc, &dest->decomp_stat) ;

 zvtrans(byte_conv, sorc, &dest->comp_ratio, 6) ;

 free (pt) ;  

}        /*    End of Subroutine       */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_lrs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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


/* ENG_LRS_TYP
 *=============================================================================
 * Engineering Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		hlm_1a[5];
	UBYTE		llm_1a[6];
	UBYTE		llm_2a[2];
	UBYTE		hlm_1b[5];
	UBYTE		llm_1b[6];
	UBYTE		llm_2b[2];
	UBYTE		aacs[16];
	UBYTE		filler;		
	UBYTE		p1[5];
	UBYTE		p2[5];
	UBYTE		p3[5];
	UBYTE		p4[5];
	UBYTE		p5[5];
	UBYTE		p6[5];
	UBYTE		p7[5];
	UBYTE		p8[5];
	UBYTE		p9[5];
	}
	eng_lrs_p_typ;

/* PWS_LRS_TYP
 *=============================================================================
 * Plasma Wave Subsytem, Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		digital_stat;	/* Digital Status	*/
	UBYTE		analog_eng;	/* Analog Engineering	*/
	UBYTE		filter[7];	/* Filter Channels	*/
	UBYTE		data_qlty;	/* Data Quality		*/
	UBYTE		wave[10];	/* Waveform Survey	*/
	}
	pws_lrs_p_typ;

/* SSI_LRS_TYP
 *=============================================================================
 * Solid State Imaging, Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{   struct
	    {
	    UBYTE		stnd_hk[2];	/* Standard Housekeeping     */
	    UBYTE		img_hk[2];	/* Imaging Housekeeping Data */
	    }	packet[3];
	}
	ssi_lrs_p_typ;

/* LRS_PACKET_TYP
 *=============================================================================
 * Compressed Low Rate Science packet lengths, NO GOLAY symbols.
 *=============================================================================
 */
#define		LRS_DDS_LEN		 2
#define		LRS_EPD1_LEN		50
#define		LRS_EPD2_LEN		26
#define		LRS_GOLAY_LEN		54	/* 4 packetes, same length   */
#define		LRS_HIC_LEN		12
#define		LRS_MAG_LEN		10	/* 2 packetes, same length   */
#define		LRS_PLS_LEN		51
#define		LRS_PPR_LEN		18
#define		LRS_UVS_LEN		84

typedef	struct
	{
	tlm_hdr_typ	header;			/* TLM header		    */
	eng_lrs_p_typ	eng;			/* Engineering packet	    */
	UBYTE		uvs[LRS_UVS_LEN];	/* Ultra Violet packet	    */
	UBYTE		hic[LRS_HIC_LEN];
	ssi_lrs_p_typ	ssi;
	UBYTE		pls[LRS_PLS_LEN];
	nims_lrs_p_typ	nims;
	UBYTE		dds[LRS_DDS_LEN];
	UBYTE		reserve[2];
	UBYTE		epd_1[LRS_EPD1_LEN];
	UBYTE		epd_2[LRS_EPD2_LEN];
	UBYTE		ppr[LRS_PPR_LEN];
	UBYTE		mag_1[LRS_MAG_LEN];
	UBYTE		mag_2[LRS_MAG_LEN];
	pws_lrs_p_typ	pws;
	aacs_lrs_p_typ	aacs;
	}
	lrs_typ;

/*
 *==============================================================================
 *	Same as "lrs_typ" but the Golay symbols ARE included
 *==============================================================================
 */
typedef	struct
	{
	tlm_hdr_typ	header;			/* TLM header		    */
	eng_lrs_p_typ	eng;			/* Engineering packet	    */
	UBYTE		uvs[LRS_UVS_LEN];	/* Ultra Violet packet	    */
	UBYTE		hic[LRS_HIC_LEN];
	ssi_lrs_p_typ	ssi;
	UBYTE		pls[LRS_PLS_LEN];
	nims_lrs_p_typ	nims;
	UBYTE		golay1[LRS_GOLAY_LEN];
	UBYTE		dds[LRS_DDS_LEN];
	UBYTE		reserve[2];
	UBYTE		epd_1[LRS_EPD1_LEN];
	UBYTE		golay2[LRS_GOLAY_LEN];
	UBYTE		epd_2[LRS_EPD2_LEN];
	UBYTE		ppr[LRS_PPR_LEN];
	UBYTE		mag_1[LRS_MAG_LEN];
	UBYTE		golay3[LRS_GOLAY_LEN];
	UBYTE		mag_2[LRS_MAG_LEN];
	pws_lrs_p_typ	pws;
	aacs_lrs_p_typ	aacs;
	UBYTE		golay4[LRS_GOLAY_LEN];
	}
	lrs_golay_typ;
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_rts_main.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef MIPS_GLL_RTS_MAIN_INCLUDED
#define MIPS_GLL_RTS_MAIN_INCLUDED 1

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
*/

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

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_edr.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_tlm_code.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*				GLL_TLM_CODE.H
 ******************************************************************************
 *	This file defines the symbols for all Galileo telemetry formats
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 *  7- 7-1989	N/A		Payam Zamani - Original Delivery
 ******************************************************************************
 */

/*				GLL_TLM_CODE
 *=============================================================================
 *	Constant codes for GLL telemetry modes
 *=============================================================================
 */

#define		LPB		0x00
#define		EHR		0x01
#define		BPB		0x02
#define		MPB		0x03
#define		XPW		0x04
#define		XCM		0x05
#define		XED		0x06
#define		XPB		0x07
#define		XPN		0x08
#define		XRW		0x09
#define		HPB		0x0A
#define		HPJ		0x0B
#define		HRW		0x0C
#define		HCJ		0x0D
#define		MPP		0x0E
#define		MPR		0x0F
#define		HPW		0x10
#define		HIM		0x11
#define		HCM		0x12
#define		LRS		0x13
#define		MPW		0x14
#define		PW8		0x15
#define		IM8		0x16
#define		AI8		0x17
#define		PW4		0x18
#define		IM4		0x19
#define		ESS		0x1D
#define		ELS		0x1E
#define		LAST_FORMAT	ELS
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ph2_ssi_bin.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* GLL_PH2_SSI_BIN.H                                                         */
/*    written by F. Moss, 11-Aug-1994                                        */
/*****************************************************************************/
#define DEFAULT_HOST     "VAX-VMS"     /* Defines the host type for the data */
                                       /* that is being written out.         */
#define GLL_PH2_SSI_EDR  "GLL_PH2_SSI_EDR" /* Binary label type for GLL SSI EDRs */
                                           /* and UDRs.                          */

/* statuses */

#define BAD_MALLOC       -1

/* In some cases, full-frame and sum-mode EDRs must be processed             */
/* differently, so there is a flag for each one. However, the structures     */
/* are declared the same for both (i.e. the number of pixels is 800 for each */
/* one, even though there's only 400 pixles in a SM EDR.)  Be sure to read   */
/* or write the correct number of data items if you are working with a       */
/* sum-mode EDR.                                                             */

#define FF_HALF_DATA     11   /* random numbers--no significance */
#define SM_HALF_DATA     22
#define BYTE_DATA        33    

#define NLINES           800     /* number of lines in a UDR or FF EDR image */
#define NSAMPS           NLINES  /* number of samples in a UDR or FF EDR image */

#define NLINES_SE        400        /* number of lines in a summation EDR img*/
#define NSAMPS_SE        400        /* number of samples in a summation EDR img*/

/*****************************************************************************/
/* Number of samples in the image line  header               (binary prefix) */
/*****************************************************************************/
#define BYTE_PREFIX_SZ        200                                

/*****************************************************************************/
/* These two #defines are useful in setting up calls for                     */
/* get_ssi_bdv_hdr_rec() and write_ssi_bdv_hdr_rec(). They indicate the      */
/* number of lines in the binary telemetry header, for UDRs and EDRs         */
/* respectively.  To get the first line in the bad data value header, check  */
/* the data format of the file (half or byte) to determine whether it's an   */
/* EDR or a UDR. Line #1 of the bdv header is UDR_THDR_RECS+1, if it's a UDR,*/
/* or EDR_THDR_RECS+1, if it's an EDR.                                       */
/*                                                                           */
/* Cut-off points:                                                           */
/* UDR:    1000 bytes in record 1.   hdr->record_id through hdr->hist[55]    */
/*          800 bytes in record 2.   hdr->hist[56] through hdr->hist[255]    */
/*                                                                           */
/* FF EDR: 1800 bytes in record 1. (Whole header fits in one record.)        */
/*                                                                           */
/* SM EDR: 1000 bytes in record 1.   hdr->record_id through hdr->hist[55]    */
/*          800 bytes in record 2.   hdr->hist[56] through hdr->hist[255]    */
/*****************************************************************************/
#define UDR_THDR_RECS    2     /* # records in a UDR binary telemetry header */
#define FF_EDR_THDR_RECS 1     /* # recs in a full-frame EDR binary telemetry header */
#define SM_EDR_THDR_RECS 2     /* # recs in a sum-mode EDR binary telemetry header */

/*****************************************************************************/
/* General defines. */
#define ROUTINE          /* */
#define SUCCESS          1    
#ifndef TRUE
#define TRUE             1    
#endif
#ifndef FALSE
#define FALSE            0
#endif

/*****************************************************************************/
/* These are globals used for datatype conversion. */
/*
static int ph2_byte_trans[12], ph2_byte_size;
static int ph2_half_trans[12], ph2_half_size;
static int ph2_full_trans[12], ph2_full_size;
*/
/*****************************************************************************/
/* Convenience macros & routine prototypes to reduce typing */

/* For translating any format that's read in... (RGD) */
/*
#define TBYTE(from, to) zvtrans(ph2_byte_trans,(from),(to),1); (from)+=ph2_byte_size;
#define THALF(from, to) zvtrans(ph2_half_trans,(from),(to),1); (from)+=ph2_half_size;
#define TFULL(from, to) zvtrans(ph2_full_trans,(from),(to),1); (from)+=ph2_full_size;
#define TSTRN(from, to, n) zmove((from),(to),(n)); (from)+=(n);
*/
/* For writing out in native format.....*/
/*
#define TOBYTE(from, to) zvtrans(ph2_byte_trans,(from),(to),1); (to)+=ph2_byte_size;
#define TOHALF(from, to) zvtrans(ph2_half_trans,(from),(to),1); (to)+=ph2_half_size;
#define TOFULL(from, to) zvtrans(ph2_full_trans,(from),(to),1); (to)+=ph2_full_size;
#define TOSTRN(from, to, n) zmove((from),(to),(n)); (to)+=(n);
*/
/*****************************************************************************/
/* static function prototypes for translating any format that's read in      */

static void trin_scet_typ();         
static void trin_ert_typ();
static void trin_sclk_typ();
static void trin_flg_comp_typ();

/* static function prototypes for writing out in native format               */
static void trout_scet_typ();     
static void trout_ert_typ();
static void trout_sclk_typ();
static void trout_flg_comp_typ();

/* more static function prototypes                                           */
static void init_trans_out();
static void init_trans_inb();
static void init_pixsizeb();

void get_ssi_data_format();
char ph2_aline[80];                /* For status messages, etc. One text line.   */

/* end module */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_rts_main_diff.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef  GLL_RTS_MAIN_DIFF_INCLUDED
#define  GLL_RTS_MAIN_DIFF_INCLUDED 1

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
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ph2_ssi_edr.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef  MIPS_GLL_PH2_SSI_EDR_INCLUDED
#define  MIPS_GLL_PH2_SSI_EDR_INCLUDED 1

#include "rts_typedefs.h"
#include "gll_rts_main.h"
#include "gll_decompression.h"
#include "gll_ict_qqc.h"   

/**************************************************************************** 
 * This include file defines the UDR and EDR structure for Galileo Phase II *
 * telemetry records.                                                       *
 *                                                                          *   
 * History:                                                                 *
 *                                                                          *
 * Date                Reference       Description                          *
 * __________________  ______________  __________________________________   *
 * 18- 4-1994          UDR SISes       Damon D. Knight - Original Delivery  *
 * Mar 4 1995          FR 85639        GMY-Change EDR format from UBYTE to  *
 *                                     short int.                           *
 *                                                                          *
 ****************************************************************************/


#define  SSI_UDR_LEN		1000
#define  SSI_PRFX_LEN		 200
#define  SSI_UDR_PIXELS		 800
#define  SSI_HDR_LEN		1800
#define  SSI_MAX_LINES		 800
#define  SSI_EDR_PIXELS		1600

/* SSI3 Housekeeping */

typedef struct
        {
         FIELD           exposure_no:5;  /* Exposure Number (bits 0-4)        */
         FIELD           cmnd_gain:2;    /* Commanded Gain State (bits 5-6)   */
         FIELD           flood:1;        /* Light Flood (bit 7)               */
        }
        word23_typ;

typedef struct
        {
         FIELD           cmnd_filter_pos:3;   /* Commanded Filter Pos. (bits 0-2)  */
         FIELD           cmnd_filter_step:1;  /* Commanded Filter Step. (bits 3)   */
         FIELD           cmnd_blemish:1;      /* Commanded Blemish Pro. (bits 4)   */
         FIELD           cmnd_expo_mode:1;    /* Commanded Exposure Mode (bit 5)   */
         FIELD           cmnd_expo_cycle:1;   /* Commanded Exposure Cycle (bits 6) */
         FIELD           skip:1;         /* Skip (bit 7)                      */
        }
        word24_typ;

typedef struct
        {
         FIELD           state_used:2;   /* State Used (bits 0-1)             */
         FIELD           comp_stat:2;    /* Compressor Status (bits 2-3)      */
         FIELD           long_expo:1;    /* Long Exposure Cycle (bit 4)       */
         FIELD           img_mode:3;     /* Imaging Mode (bits 5-7)           */
        }
        word25_typ;

typedef struct
        {
         FIELD           odd_parity:1;   /* Odd Parity (bits 0)               */
         FIELD           filter:3;       /* Actual Filter (bits 1-3)          */
         FIELD           blemish:1;      /* Blemish Protection Status (bits 4)*/
         FIELD           watchdog:1;     /* Watchdog Trip Flag (bits 5)       */
         FIELD           par_clock:1;    /* Parallel Clock State (bits 6)     */
         FIELD           mem_write:1;    /* Imaging Mode (bits 7)             */
        }
        word26_typ;


/* SSI 3 Housekeeping Packet */

typedef struct
        {
         char            ra[8];            /* (0-7)                       */
         char            dec[8];           /* (8-15)                       */
         char            twist[8];         /* (16-23)                       */
         char            clock_angle[8];   /* (24-31)                       */
         UBYTE           ccd_temp_fine;    /* (32)                         */
         UBYTE           ccd_temp_coarse;  /* (33)                         */
         UBYTE           pic_no;           /* (34)                        */
         word23_typ      word23;           /* (35)                        */
         word24_typ      word24;           /* (36)                        */
         word25_typ      word25;           /* (37)                        */
         word26_typ      word26;           /* (38)                        */
        }
        ssi3_hk_typ;

/* Compression Flags */

typedef struct
        {
         FIELD           barc_comp:1;    /* BARC Compression (1, bit 0)       */
         FIELD           barc_mode:1;    /* Barc Comp. Mode (1, bit 1)        */
         FIELD           expo:1;         /* Exposure Mode (1, bit 2)          */
         FIELD           flood:1;        /* Light Flood Status (1, bit 3)     */
         FIELD           blem:1;         /* Blemish Protection (1, bit 4)     */
         FIELD           clock:1;        /* Parallel Clock State (1, 5)       */
         FIELD           ict_comp:1;     /* ICT Comp. (1, bit 6)              */
         FIELD           huff_comp:1;    /* Huffman Comp. (1, bit 7)          */
         UBYTE           reserved1;      /* Reserved bits (2)                 */
        }
        flag_comp_typ;

/* Input Source Structure */

typedef struct
        {
         FIELD           SFDU:1; /* bit 0 */
         FIELD           WBDL:1; /* bit 1 */
         FIELD           SDR:1; /* bit 2 */
         FIELD           IDR:1; /* bit 3 */
         FIELD           EDR:1; /* bit 4 */
         FIELD           MIPS:1; /* bit 5 */
         FIELD           APB:1; /* bit 6 */
         FIELD           reserved:1;  /* bit 7 */
        }
        input_src;

/* Number of Truncation bits */

typedef struct
        {
         FIELD           block0:2;       /* (0, 0-1)                         */
         FIELD           block1:2;       /* (0, 2-3)                         */
         FIELD           block2:2;       /* (0, 4-5)                         */
         FIELD           block3:2;       /* (0, 6-7)                         */
         FIELD           block4:2;       /* (1, 0-1)                         */
         FIELD           block5:2;       /* (1, 2-3)                         */
         FIELD           block6:2;       /* (1, 4-5)                         */
         FIELD           block7:2;       /* (1, 6-7)                         */
         FIELD           block8:2;       /* (2, 0-1)                         */
         FIELD           block9:2;       /* (2, 2-3)                         */
         FIELD           block10:2;      /* (2, 4-5)                         */
         FIELD           block11:2;      /* (2, 6-7)                         */
         FIELD           block12:2;      /* (3, 0-1)                         */
         FIELD           filler1:6;      /* (3, 2-7)                         */
        }
        trun_bits_typ;


/* Line Construction */

typedef struct
        {
         FIELD           part_pkts:4;    /* No. tlm pkts for line (125,0-3)  */
         FIELD           full_pkts:4;    /* No. full pkts for line (125,4-7) */
        }
        line_con_typ;

/* MIPS Physical Recording Words */

typedef struct
        {
         BYTE            os[8];          /* Operating system version (0-7)   */
         BYTE            reserved1[26];  /* Used during Phase I              */
         BYTE            cpu[8];         /* CPU name                         */
         BYTE            date[11];       /* Generation date                  */
         BYTE            filler[6];      /* No Description                   */
        }
        mips_prd_typ;

/* UDR, REDR and EDR Image Line Record */

typedef struct
        {
         UBYTE           record_id;      /* Record Identifier (0)            */
         UBYTE           reserved1A;     /* Used during Phase I (1-3)        */
         UWORD           reserved1B;
         UWORD           log_seq;        /* Logical sequence (4-5)           */
         ert_typ         ert;            /* Earth Received Time (6-14)       */
         sclk_typ        sclk;           /* SCLK of 1st minor frame (15-21)  */
         mips_prd_typ    mips_prd;       /* Recording & valid device (22-80) */
         UWORD           record_format;  /* Record Format (81-82)            */
         UBYTE           input_type;     /* Input type (83)                  */
         input_src       source;         /* Input source (84)                */
         UBYTE           reserved3A;     /* Used during Phase I (85-102)     */
         UBYTE           reserved3B;  
         UBYTE           reserved3C[12];  
         UWORD           reserved3D;  
         UWORD           reserved3E;  
         trun_bits_typ   trun_bits;      /* Truncated bits/pixel (103-105)   */
         UWORD           trun_pix;       /* No. pixels truncated (107-108)   */
         UWORD           version;        /* Catalog ver. ident img (109-110) */
         UWORD           reserved4;      /* Used during Phase I (111-112)    */
         UBYTE           dsn_id;         /* Defined GLL-820-13/OPS-6-8 (113) */
         UWORD           line_no;        /* Image line number (114-115)      */
         UBYTE           reserved5;      /* Used during Phase I              */
         UWORD           ss1;            /* Starting sample (117-118)        */
         UWORD           es1;            /* Ending sample (119-120)          */
         UWORD           ss2;            /* Starting sample (121-122)        */
         UWORD           es2;            /* Ending sample (123-124)          */
         line_con_typ    line_con;       /* Line construction (125)          */
         UBYTE           apid;           /* Application pkt ID (126)         */
         UINT            pkt_seq;        /* Tlm pkt is 1st pixel (127-130)   */
         UWORD           pkt_pix_srt;    /* Starting samp of pkt (131-132)   */
         UWORD           strt_pix;       /* Truth table position (133-134)   */
         UWORD           stop_pix;       /* Truth table position (135-136)   */
         rct_typ         rct;            /* TIS Record Creation (137-145)    */
         BYTE            decomp_stat;    /* Decompression status/errors (146)*/
         char            comp_ratio[6];  /* Compression Ratio (147-152)      */
         BYTE            reserved6[47];  /* Reserved (153-199)               */
        }
        ssi_prefix_typ;

/* Telemetry Header */

typedef struct                                    /* Format Identification   */
        {
         UBYTE           record_id;      /* Record Identifier (0)             */
         UBYTE           reserved1;      /* Used during Phase I (1)           */
         char            project[10];    /* Project name = "GALILEO" (2-11)   */
         char            instrument[6];  /* Instrument name = "SSI" (12-17)   */
         UWORD           reserved2;      /* Used during Phase 1 (18-19)       */
         UWORD           log_seq;        /* Logical Sequence (20-21)          */
         ert_typ         first_ert;      /* ERT of first packet (22-30)       */
         ert_typ         last_ert;       /* ERT of last packet (31-39)        */
         sclk_typ        first_sclk_rec; /* SCLK of first record (40-46)      */
         sclk_typ        last_sclk_rec;  /* SCLK of last record (47-53)       */
         scet_typ        scet;           /* SCET middle open shutter (54-62)  */
         mips_prd_typ    mips_prd;       /* MIPS Phy. Record Data (63-121)    */
         UWORD           rec_format;     /* Record Format (122-123)           */
         UINT            reserved3A;     /* Used during Phase I (124-127)     */
         UBYTE           boom;           /* Boom obscuration flag (128)       */
         UWORD           miss_lines;     /* Lines with no pixels (129-130)    */
         UWORD           part_lines;     /* Lines with valid pixels (131-132) */
         UWORD           reserved4;      /* Used during Phase I (133-134)     */
         UWORD           seq_break;      /* Tot no. of missing pkts (135-136) */
         UWORD           reserved5[3];   /* Used during Phase I (137-142)     */
         UWORD           sfdus;          /* Total no. minor frames (143-144)  */
         char            pic_no[7];      /* Picture no. (145-151)             */
         UBYTE           reserved6[12];  /* Used during Phase I (152-163)     */
         flag_comp_typ   flags;          /* Compression flags (164-165)       */
         char            mean_dn[6];     /* Mean DN of all levels (166-171)   */
         char            trun_bits[6];   /* Mean no. of trun bits (172-177)   */
         char            trun_pixel[6];  /* Mean no. of trun pixels (178-183) */
         BYTE            mean_if[12];    /* Mean I/F Level (184-195)          */
         BYTE            entrop_avg[7];  /* Entropy level picture (196-202)   */
         BYTE            entropies[15][7]; /* Entropy level 15 lines (203-307)*/
         BYTE            pointing[3][8]; /* Scan platform coords (308-331)    */
         BYTE            scale_fact[2][8]; /* Radio. Calib. scale fact (332-347) */
         BYTE            slope_file[32]; /* Radiometric file for proc. (348-379) */     
         BYTE            offset_file[32]; /* Radiometric file for proc.(380-411) */     
         char            activity[20];   /* 20 ASCII Chars. (412-431)         */
         UBYTE           filler;         /* (432)                             */
         UBYTE           filter;         /* (433)                             */
         UBYTE           exposure;       /* Exposure number (434)             */
         UBYTE           image_mode;     /* Imaging_mode (435)                */
         UBYTE           gain;           /* Camera gain state (436)           */
         UINT            range;          /* Target's range sun (437-440)      */
         UBYTE           reserved7;      /* Used during Phase I (441)         */
         UWORD           version;        /* MIPS Cat. ver. no. (442-443)      */
         sclk_typ        start_sclk_img; /* SCLK start of image (444-450)     */
         sclk_typ        end_sclk_img;   /* SCLK end of image (451-457)       */
         ssi3_hk_typ     ssi3_pkt;       /* Data SSI3 tlm pkt (458-496)       */
         UBYTE           reserved8[279];     
         UINT            histogram[256]; /* 256 32-bit histogram (776-1799)   */
        }
        ssi_hdr_typ;


typedef	struct
	{
	ssi_prefix_typ			prefix;
	unsigned char			pixels[SSI_UDR_PIXELS];
	}
	ssi_line_typ;

typedef	struct
	{
	ssi_prefix_typ			prefix;
	short			        pixels[SSI_EDR_PIXELS];
	}
	ssi_edr_line_typ;

#endif     
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_bin.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* GLL_SSI_BIN.H                                                             */
/* Bad data value header interface code                                      */
/*    written by M. O'Shaughnessy, 2-Nov-1992                                */
/* 30may96  -lwk- fixed NSAMPS_SE					     */
/*****************************************************************************/
#define DEFAULT_HOST     "VAX-VMS"     /* Defines the host type for the data */
                                       /* that is being written out.         */
#define GLL_SSI_EDR      "GLL_SSI_EDR" /* Binary label type for GLL SSI EDRs */
                                       /* and UDRs.                          */
/* statuses */

#define BAD_MALLOC       -1
#define NO_BDVH          -2 /* No bad data value header was found */
#define END_BDVH         -3 /* End of binary header reached successfully */
                            /* (Image data was found.) */
#define OVERRUN_BDVH     -4 /* binary header exceeded NLB */
#define EMPTY_BDVH_REC   -5 /* blank bdvh line */

/* In some cases, full-frame and sum-mode EDRs must be processed             */
/* differently, so there is a flag for each one. However, the structures     */
/* are declared the same for both (i.e. the number of pixels is 800 for each */
/* one, even though there's only 400 pixles in a SM EDR.)  Be sure to read   */
/* or write the correct number of data items if you are working with a       */
/* sum-mode EDR.                                                             */

#define FF_HALF_DATA     11   /* random numbers--no significance */
#define SM_HALF_DATA     22
#define BYTE_DATA        33    

#define NLINES           800     /* number of lines in a UDR or FF EDR image */
#define NSAMPS           NLINES  /* number of samples in a UDR or FF EDR image*/

#define NLINES_SE        400     /* number of lines in a summation EDR img*/
#define NSAMPS_SE        400	 /* number of samples in a summation EDR img */

/* Structure & defines for the GLL EDR/UDR binary bad data value header */
/* These #defines identify the type of bad data in a particular bdv rec */

#define BDV_EMPTY_REC       0
#define BDV_IMAGE_DATA      2     /* If you get this one, you've run out of 
                                   bdv rec's and are in image data. 
                                   Reference: UDR and EDR SIS */
#define BDV_DATA_DROPOUT    3
#define BDV_SATURATED_PX    4
#define BDV_LFW_PX          5
#define BDV_SINGLE_PX_SPIKE 6
#define BDV_RS_OVERFLOW     7

/* These #defines identify whether a bad data group is a single pixel,  */
/* a line segment, or a column (sample) segment. They are used with the */
/* "code" field of the bdvh structure, shown below.                     */

#define BDV_SPX             1
#define BDV_LINE_SEG        2
#define BDV_SAMP_SEG        3
    
/* Maximum number of element coordinates in a bdv record.  Only the      */
/* largest set (for FFE) is used to dimension arrays, but the others are */
/* provided for completeness.                                            */

/* The "E" in FFE and SME indicates EDR format */
#define MAX_FFE_SPX_OBJ     448 /* full frame single pixel errors        */
#define MAX_FFE_LINE_OBJ    299 /* full frame line segemnt               */
#define MAX_FFE_COL_OBJ     299 /* full from column segment              */

#define MAX_SME_SPX_OBJ     248 /* summation mode single pixel errors    */
#define MAX_SME_LINE_OBJ    165 /* summation mode line segment           */
#define MAX_SME_COL_OBJ     165 /* summation mode column segment         */

/* The "U" in FFU and SMU indicates UDR format */
#define MAX_FFU_SPX_OBJ     248 /* full frame single pixel errors        */
#define MAX_FFU_LINE_OBJ    165 /* full frame line segemnt               */
#define MAX_FFU_COL_OBJ     165 /* full from column segment              */

#define MAX_SMU_SPX_OBJ     248  /* summation mode single pixel errors   */
#define MAX_SMU_LINE_OBJ    164  /* summation mode line segment          */
#define MAX_SMU_COL_OBJ     164  /* summation mode column segment        */

/* the structure below can be used for any of the four formats: ff or    */
/* summation mode edrs or udrs. The data arrays have a maximum size equal */
/* to the dimensions of the largest required arrays (for ffe).           */

typedef struct {
  short record_id;               /* identifies the type of bad data */
  short code;                    /* 1=single pix, 2=line seg, 3=samp seg */
  short nobjs;                   /* number of objects in the list */
  union error_coords {
    struct single_pixel_element {  /* coordinates of a single pixel error */
      short line;
      short sample;
    } pixel_data [MAX_FFE_SPX_OBJ];
    struct line_element {         /* coords of a line segment */
      short line;
      short ss;
      short ns;
    } line_data [MAX_FFE_LINE_OBJ];
    struct column_element {      /* coords of a sample segment */
      short sample;
      short sl;
      short nl;
    } column_data [MAX_FFE_COL_OBJ];
  } coords;
} ssi_bdvh_typ; 

/*****************************************************************************/
/* Number of samples in the image line bad-data value header (binary prefix) */
/*****************************************************************************/
#define BYTE_PREFIX_SZ        200                                

/*****************************************************************************/
/* These two #defines are useful in setting up calls for                     */
/* get_ssi_bdv_hdr_rec() and write_ssi_bdv_hdr_rec(). They indicate the      */
/* number of lines in the binary telemetry header, for UDRs and EDRs         */
/* respectively.  To get the first line in the bad data value header, check  */
/* the data format of the file (half or byte) to determine whether it's an   */
/* EDR or a UDR. Line #1 of the bdv header is UDR_THDR_RECS+1, if it's a UDR,*/
/* or EDR_THDR_RECS+1, if it's an EDR.                                       */
/*                                                                           */
/* Cut-off points:                                                           */
/* UDR:    1000 bytes in record 1.   hdr->record_id through hdr->hist[55]    */
/*          800 bytes in record 2.   hdr->hist[56] through hdr->hist[255]    */
/*                                                                           */
/* FF EDR: 1800 bytes in record 1. (Whole header fits in one record.)        */
/*                                                                           */
/* SM EDR: 1000 bytes in record 1.   hdr->record_id through hdr->hist[55]    */
/*          800 bytes in record 2.   hdr->hist[56] through hdr->hist[255]    */
/*****************************************************************************/
#define UDR_THDR_RECS    2     /* # records in a UDR binary telemetry header */
#define FF_EDR_THDR_RECS 1     /* # recs in a full-frame EDR binary telemetry header */
#define SM_EDR_THDR_RECS 2     /* # recs in a sum-mode EDR binary telemetry header */

/*****************************************************************************/
/* General defines. */
#define ROUTINE          /* */
#define KEY_SZ           8    
#define SUCCESS          1    
#define FAILURE          0
#define TRUE             1    
#define FALSE            0

/*****************************************************************************/
/* These are globals used for datatype conversion. */
static int byte_trans[12], byte_size;
static int half_trans[12], half_size;
static int full_trans[12], full_size;
/*****************************************************************************/
/* Convenience macros & routine prototypes to reduce typing */

/* For translating any format that's read in... (RGD) */
#define TBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (from)+=byte_size;
#define THALF(from, to) zvtrans(half_trans,(from),(to),1); (from)+=half_size;
#define TFULL(from, to) zvtrans(full_trans,(from),(to),1); (from)+=full_size;
#define TSTRN(from, to, n) zmove((from),(to),(n)); (from)+=(n);

/* For writing out in native format.....*/
#define TOBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (to)+=byte_size;
#define TOHALF(from, to) zvtrans(half_trans,(from),(to),1); (to)+=half_size;
#define TOFULL(from, to) zvtrans(full_trans,(from),(to),1); (to)+=full_size;
#define TOSTRN(from, to, n) zmove((from),(to),(n)); (to)+=(n);

/*****************************************************************************/
/* static function prototypes for translating any format that's read in      */

static void trin_scet_typ();         
static void trin_ert_typ();
static void trin_sclk_typ();
static void trin_ssi_lrs_p_typ();
static void trin_ssi_flg_typ();
static void trin_j2000_ascii_typ();

/* static function prototypes for writing out in native format               */
static void trout_scet_typ();     
static void trout_ert_typ();
static void trout_sclk_typ();
static void trout_ssi_lrs_p_typ();
static void trout_ssi_flg_typ();
static void trout_j2000_ascii_typ();

/* more static function prototypes                                           */
static void init_trans_out();
static void init_trans_inb();
static void init_pixsizeb();

void get_ssi_data_format();
char aline[80];                /* For status messages, etc. One text line.   */

/* end module */
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ict_qqc.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef  MIPS_GLL_ICT_QQC_INCLUDED
#define  MIPS_GLL_ICT_QQC_INCLUDED 1


/* 
   This include file defines the SFDU header structures for the ICT 
   Compression QQC.
   
   History:

   Date                Reference       Description
   __________________  ______________  __________________________________
   19- 8- 1994                          Damon D. Knight - Original Delivery
                                                                            */

#include   "rts_typedefs.h"
#include   "gll_rts_main.h"

/* Compression Ratio Structure */

typedef struct
        {
         FIELD           s:1;             /* Sign (0:7)                     */
         FIELD           exponent:8;      /* Exponent (0:6-0,1:7)         */
         FIELD           mantissa:23;     /* Mantissa (1:6-0,2,3)           */
        }
        ieee_flt_pt_typ;


/* Record Creation Time */

typedef struct
        {
         UWORD           days;            /* Days since Jan. 1, 1958 (0-1)  */
         UWORD           milliseconds;    /* Millisecs of current day (2-3) */
        }
        sfdu_rct_typ;


/* Compression Slice Structure */

typedef struct
        {
         ieee_flt_pt_typ comp_ratio;       /* Compression Ratio (0-3)       */
         UBYTE           status;           /* Status of Decompression (4)   */
         UBYTE           qfactor;          /* Spare (5)                 */
        }
        comp_str_typ;

/* NIMS Compression Slice Structure */

typedef struct
        {
         UWORD           bytes_decomp;     /* Bytes Decompressed (0-1)      */
         UWORD           bytes_comp;       /* Bytes Compressed (2-3)        */
         UBYTE           status;           /* Status of Decompression (4)   */
         UBYTE           qfactor;          /* Spare (5)                     */
        }
        nims_comp_str_typ;


/* Primary Label Structure */

typedef struct
        {
         BYTE            author_id[4];     /* Control Authority ID (0-3)    */
         UBYTE           version_id;       /* Version ID (4)                */
         BYTE            class_id;         /* Identifies label class (5)    */
         UWORD           spares;           /* Each set to ASCII (6-7)       */
         BYTE            ddp_id[4];        /* Data Descr. Pack. ID (8-11)   */
         UINT            blk_length[2];    /* Length of the SFDU (12-19)    */
        }
        qqc_pri_lab_typ;


/* Subheader Aggregation CHDO Structure */

typedef struct
        {
         UWORD           chdo_type;        /* Subhdr aggregation type (0-1)  */
         UWORD           chdo_length;      /* Length of all subheaders (2-3) */
        }
        qqc_aggr_typ;


/* Primary Header Structure */

typedef struct
        {
         UWORD           chdo_type;        /* Tlm Pri Hdr CHDO type (0-1)    */
         UWORD           chdo_length;      /* Lgth of CHDO value field (2-3) */
         UBYTE           major;            /* SFDU major type (4)            */
         UBYTE           minor;            /* SFDU minor type (5)            */
         UBYTE           mission_id;       /* Mission identifier code (6)    */
         UBYTE           format;           /* SFDU format type (7)           */
        }
        qqc_pri_hdr_typ;


/* QQC Secondary Header Structure */

typedef struct
        {
         UWORD           chdo_type;        /* Secondary Hdr CHDO type (0-1)   */
         UWORD           chdo_length;      /* Lgth of CHDO value field (2-3)  */
         sfdu_rct_typ    rct;              /* Record Creation Time (4-9)      */
         sclk_typ        first_sclk;       /* SCLK of first slice (10-15)     */
         sclk_typ        last_sclk;        /* SCLK of last slice (16-21)      */
         UBYTE           num_of_slices;    /* Number of slices (22)           */
         UBYTE           spare1;           /* Spare 1 (23)     	              */
         UINT            spare2;           /* Spare 2 (24-27)                 */
         comp_str_typ    slice[255];   /* Compress. data slices (28-1557) */
        }
        qqc_sec_hdr_typ;

/* NIMS QQC Secondary Header Structure */

typedef struct
        {
         UWORD              chdo_type;       /* Secondary Hdr CHDO type (0-1)   */
         UWORD              chdo_length;     /* Lgth of CHDO value field (2-3)  */
         sfdu_rct_typ       rct;             /* Record Creation Time (4-9)      */
         sclk_typ           first_sclk;      /* SCLK of first slice (10-15)     */
         sclk_typ           last_sclk;       /* SCLK of last slice (16-21)      */
         UBYTE              num_of_slices;   /* Number of slices (22)           */
         UBYTE              spare1;          /* Spare 1 (23)     	              */
         UINT               spare2;          /* Spare 2 (24-27)                 */
         nims_comp_str_typ  nims_slice[255]; /* Compress. data slices (28-1557) */
        }
        nims_qqc_sec_hdr_typ;


/* QQC Header Structure */

typedef struct
        {
         qqc_pri_lab_typ pri_lab;
         qqc_aggr_typ aggr;
         qqc_pri_hdr_typ pri_hdr;
         qqc_sec_hdr_typ sec_hdr;
        }
        qqc_hdr_typ;

/* NIMS QQC Header Structure */

typedef struct
        {
         qqc_pri_lab_typ pri_lab;
         qqc_aggr_typ aggr;
         qqc_pri_hdr_typ pri_hdr;
         nims_qqc_sec_hdr_typ sec_hdr;
        }
        nims_qqc_hdr_typ;

/* Slice Information Structure */

typedef struct
        {
         UBYTE             found;
         UBYTE             comp_stat;
         float             compression;
        }
        slice_info_typ;

/* NIMS Slice Information Structure */

typedef struct
        {
         UBYTE             found;
         UBYTE             comp_stat;
         UWORD             bytes_decomp;
         UWORD             bytes_comp;
        }
        nims_slice_info_typ;

/* QQC Information Structure */

typedef struct
        {
         UBYTE             app_id;
         UWORD             rct_days;
         UWORD             rct_milliseconds;
         int               rim;
         UBYTE             mod91;
         UBYTE             mod10;
         UBYTE             mod8;
         int               last_rim;
         UBYTE             last_mod91;
         UBYTE             last_mod10;
         UBYTE             last_mod8;
         UBYTE             num_of_slices;
         UBYTE             telm_frmt;
         UBYTE             qfactor;
         slice_info_typ    slice[255];
        }
        qqc_info_typ;

/* NIMS QQC Information Structure */

typedef struct
        {
         UBYTE                  app_id;
         UWORD                  rct_days;
         UWORD                  rct_milliseconds;
         int                    rim;
         UBYTE                  mod91;
         UBYTE                  mod10;
         UBYTE                  mod8;
         int                    last_rim;
         UBYTE                  last_mod91;
         UBYTE                  last_mod10;
         UBYTE                  last_mod8;
         int                    sec_rim;
         UBYTE                  sec_mod91;
         UBYTE                  sec_mod10;
         UBYTE                  sec_mod8;
         int                    sec_last_rim;
         UBYTE                  sec_last_mod91;
         UBYTE                  sec_last_mod10;
         UBYTE                  sec_last_mod8;
         UBYTE                  num_of_slices1;
         UBYTE                  num_of_slices2;
         UBYTE                  telm_frmt;
         UBYTE                  qfactor;
         nims_slice_info_typ    nims_slice[364];
        }
        nims_qqc_info_typ;




#endif     
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_decompression.h
$ DECK/DOLLARS="$ VOKAGLEVE"

#ifndef MIPS_GLL_DECOMPRESSION_INCLUDED
#define MIPS_GLL_DECOMPRESSION_INCLUDED 1

/*****************************************************************************
 *  The purpose of this file is for defining structures the structure that   *
 *  will be used to stored decompression results.                            *
 *                                                                           *
 *  Delivery Date     Programmer    Comment                                  *
 *  -------------     ----------    ---------------------------------------- *
 *    07-05-94           DDK        Original Delivery                        *
 *                                                                           *
 ****************************************************************************/


typedef struct
    {
         int ndcmp;
         int tndcmp;
         int tntb;
         int tndcmp50;
         int tntb50;
	 int ncalls;
         unsigned char ntbpp[13];
         unsigned char mode_count[2];
         float avgntb;
         float avgntp;
         short int barc_complete;
         int bits_read;
         short int ict_complete;
         short int good_blocks;
         short int sync_mark;
         short int slice_no;
         short int full_pkt;
         short int partial_pkt;
         float max_bytes_read;
         float min_bytes_read;
         float max_bytes_lossless;
         float min_bytes_lossless;
         float tot_bytes_read;
         float tot_bytes_lossless;
   }
    comp_stat_typ;

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_main.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_bin_diff.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* GLL_SSI_BIN_DIFF.H                                                        */
/*    written by F. Moss, 11-Aug-1994                                        */
/*****************************************************************************/
/* In some cases, full-frame and sum-mode EDRs must be processed             */
/* differently, so there is a flag for each one. However, the structures     */
/* are declared the same for both (i.e. the number of pixels is 800 for each */
/* one, even though there's only 400 pixles in a SM EDR.)  Be sure to read   */
/* or write the correct number of data items if you are working with a       */
/* sum-mode EDR.                                                             */

#define FF_HALF_DATA     11   /* random numbers--no significance */
#define SM_HALF_DATA     22
#define BYTE_DATA        33    


/* These are globals used for datatype conversion. */
static int byte_trans[12], byte_size;
static int half_trans[12], half_size;
static int full_trans[12], full_size;
/*****************************************************************************/
/* Convenience macros & routine prototypes to reduce typing */

/* For translating any format that's read in... (RGD) */
#define TBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (from)+=byte_size;
#define THALF(from, to) zvtrans(half_trans,(from),(to),1); (from)+=half_size;
#define TFULL(from, to) zvtrans(full_trans,(from),(to),1); (from)+=full_size;
#define TSTRN(from, to, n) zmove((from),(to),(n)); (from)+=(n);

/* For writing out in native format.....*/
#define TOBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (to)+=byte_size;
#define TOHALF(from, to) zvtrans(half_trans,(from),(to),1); (to)+=half_size;
#define TOFULL(from, to) zvtrans(full_trans,(from),(to),1); (to)+=full_size;
#define TOSTRN(from, to, n) zmove((from),(to),(n)); (to)+=(n);
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create galsos.imake
/***********************************************************************
 
                     IMAKE FILE FOR PROGRAM galsos
 
   To Create the build file give the command:  vimake galsos
 
************************************************************************/
#define PROGRAM galsos
 
#define MODULE_LIST galsos.c gll_ssi_bin.c gll_ssi_bin_ph2.c gllcalname.c \
 get_ssi_prefix.c get_ssi_ph2_prefix.c
#define INCLUDE_LIST gll_lrs.h gll_rts_main.h gll_ssi_edr.h gll_tlm_code.h \
 gll_ph2_ssi_bin.h gll_rts_main_diff.h gll_ph2_ssi_edr.h gll_ssi_bin.h \
 gll_ict_qqc.h gll_decompression.h gll_main.h gll_ssi_bin_diff.h
 
#define MAIN_LANG_C
#define USES_ANSI_C
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_FORTRAN
#define LIB_MATH77
/*#define DEBUG */	/* comment out on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create galsos.pdf
process help=*
PARM INP      TYPE=STRING  COUNT=1
PARM OUT      TYPE=STRING  COUNT=1
PARM DIR      TYPE=STRING  COUNT=0:1			        DEFAULT=--
PARM DIRCAL   TYPE=STRING  COUNT=0:1        			DEFAULT=--    
PARM DIRDC    TYPE=STRING  COUNT=0:1        			DEFAULT=--
PARM DIRBLM   TYPE=STRING  COUNT=0:1        			DEFAULT=--
PARM DIROFF   TYPE=STRING  COUNT=0:1        			DEFAULT=--
PARM CAL      TYPE=STRING  COUNT=0:1               		DEFAULT=--
PARM DC       TYPE=STRING  COUNT=0:1               		DEFAULT=--
PARM BLEM     TYPE=STRING  COUNT=0:1               		DEFAULT=--
PARM OFFSETS  TYPE=STRING  COUNT=0:1               		DEFAULT=--
PARM IOF      TYPE=REAL	   COUNT=0:1	   		  	DEFAULT=1.0
PARM CONV     TYPE=REAL    COUNT=0:1				DEFAULT=--
PARM SOLRANGE TYPE=REAL    COUNT=0:1				DEFAULT=-- 
PARM BITWT    TYPE=KEYWORD COUNT=1   VALID=(UBWC,NOUBWC)  	DEFAULT=NOUBWC
PARM PENTROPY TYPE=KEYWORD COUNT=1   VALID=(PRINT,NOPRINT)	DEFAULT=NOPRINT
PARM NOCHECK  TYPE=KEYWORD COUNT=0:1 VALID=NOCHECK		DEFAULT=--
PARM FILTER   TYPE=INTEGER COUNT=0:1 VALID=(0:7)		DEFAULT=--
PARM GAIN     TYPE=INTEGER COUNT=0:1 VALID=(1:4)		DEFAULT=--
PARM EXPOSURE TYPE=REAL	   COUNT=0:1				DEFAULT=--
PARM CFILTER  TYPE=INTEGER COUNT=0:1 VALID=(0:7)		DEFAULT=--
PARM CGAIN    TYPE=INTEGER COUNT=0:1 VALID=(1:4)		DEFAULT=--
PARM DCGAIN   TYPE=INTEGER COUNT=0:1 VALID=(1:4)		DEFAULT=--
PARM TARGET     TYPE=(STRING,12) COUNT=0:1                      DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1                        DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM DEBUG    TYPE=KEYWORD COUNT=0:1 VALID=DEBUG		DEFAULT=--
END-PROC

.TITLE
Vicar2 Program GALSOS
.HELP
PURPOSE:

GALSOS will radiometrically correct Galileo SSI images, converting
each raw DN value (as acquired by the camera) to units of reflectance or
radiance.  GALSOS will also remove camera blemishes, compute the image
entropy, and encode the locations of invalid pixels as bad-data records.

EXECUTION:

  GALSOS  INP=PIC  OUT=OPIC  user-parameters...
where 
  PIC  is the image to be radiometrically corrected (byte image).
  OPIC is the radiometrically corrected output image (16-bit integer).

The input image (PIC) must be an SSI REDR image containing a binary label,
on which BADLABELS has been run.  Both Phase 1 and Phase2 UDR formats are
supported.

Note that, unlike the version of this program that was used during Galileo
systematic processing, no UDR or calibration files are accepted as the 
primary input.  (For test purposes, the program was revised so as not to 
abort when the input is a UDR, but the results are unpredictable.)

The output image (OPIC) is an SSI EDR.

.page
NOTE ON THE ACCURACY OF THE CALIBRATION

During the testing of the revived version of GALSOS in 2011, comparisons 
were made with EDRs generated by GALSOS in Feb.1998 and with EDRs produced 
by ISIS2 and ISIS3 calibration software at the SETI Institute.  It was 
found that the output of the 2011 version can differ fairly significantly 
from that of the 1998 GALSOS (typically by ~7%), but that very good agreement 
exists with the contemporary ISIS ouput.  However, even there some differences 
exist, mostly at the edges of the image (first/last line or sample), but
sometimes along (apparently random) lines or columns.  It has also been 
observed that the ISIS2 and ISIS3 outputs differ between themselves.  
Therefore, it should be concluded that the Galileo SSI calibration is still 
to some extent a 'work in progress' and that uncertainties of order 1% in 
most pixels are to be expected.

.page
REFERENCES:

1) JPL D-4264, "MIPL Software Structural Design for the Instrument Calibration
   of GLL SSI Science Processing", by Gary Yagi, June, 1987.
2) GMY:384-94-011, "Galileo SSI Picture Label, Revision 5", April 7, 1994.
3) 625-210, "Galileo SSI Calibration Report Part 1", K. Klaasen, H. Breneman,
   November 1, 1988.
4) GLL SIS 232-04, "SSI Unprocessed Data Record for OPNAV"
5) GLL SIS 232-07, "SSI Experiment Data Record"
6) JPL IOM 384-91-3, "Tracking GLL SSI Bad-Data Values, Binary Label Design,
   Revision 2", May 14, 1991.
7) GLL SIS 232-16, "SSI Raw Experiment Data Record CDROM", April 20, 1992.
8) GLL SIS 232-12, "SSI Image Catalog (Jupiter Encounter)", 1995?.

.page
OPERATION:

GALSOS will radiometrically correct the input image, using  a linear
light-transfer model to convert each raw DN value to units of reflectance or
radiance.  GALSOS will also remove camera blemishes.

The processing parameters and calibration files used are recorded in the VICAR
label.  This label information is sufficient to reverse the radiometric
correction process (see program GALSOSI).

Detailed information is included below under the following topic headings:

   PROCESSING OF REDRs
   REQUIRED INPUT CALIBRATION DATA
   DETERMINATION OF CAMERA PARAMETERS
   CORRECTING ERRONEOUS LABEL INFORMATION
   RATIOMETRIC CORRECTION
   CREATING THE OUTPUT IMAGE LABEL
   TIMING
   EXAMPLES
   PROGRAM HISTORY

.page
PROCESSING OF REDRs:

A UDR is a raw SSI image containing ancillary information in the binary header
and binary prefix portion of the VICAR label (ref. 4).  An REDR is a UDR upon
which one or more of the following programs have been executed: CATLABEL,
BADLABELS, GLLBLEMCOR, GLLFILLIN, ADESPIKE (ref 7).  The order in which these
programs are executed is optional, except that BADLABELS must precede any
execution of GLLBLEMCOR, GLLFILLIN, or ADESPIKE.  This is because BADLABELS
adds the bad-data label records necessary for the proper functioning of the
other programs (ref 6).

Note that if GLLFILLIN has been run, its effect will be cancelled since GALSOS
fills all truncated or missing lines with -32768 DN (see below).  This side-
effect can be countered by re-running GLLFILLIN after executing GALSOS.

GALSOS will identify bad-data values and record their locations as bad-data
records in the binary header portion of the VICAR label (ref. 6).  The following
types of bad-data values are handled:

  (1) Data dropouts	    Line gaps or lines truncated by the data compressor.
  (2) Low-full-well pixels  Samples whose values exceed full-well (ref. 1).
  (3) Camera blemishes      Samples which cannot be calibrated.
  (4) Saturated pixels      Samples whose raw values are 0 or 255 DN.
  (5) Reed/Solomon errors   Reed/Solomon overflow records.

Data drop-outs are detected by examining the segements field in the binary
prefix of each image line.  No radiometric correction is performed on data
gaps and -32768 DN is stored in these output samples.

Low-full-well pixels and camera blemishes are identified from data retrieved
from the Blemish File.  These pixels are replaced by interpolating over
neighboring radiometrically corrected samples.

Saturated pixels are identified by scanning the pixels for 0 and 255 DN.
These pixels are radiometrically corrected unless they fall in another of the
above categories.

Reed/Solomon overflow errors can be detected in Phase 1 UDRs or REDRs by
examining the binary prefix portion of each image line.  Since it is not
possible to determine what portion of the line is effected by the overflow,
the entire line is declared to be invalid.

Data drop-outs, low-full-well pixels, saturated pixels, and Reed-Solomon
overflow records are recorded as invalid data in the bad-data value portion
of the binary header.  Note, however, that camera blemishes are not recorded
in the binary header since their positions are defined in the Blemish File.

If BADLABELS has already been executed on an input REDR, GALSOS copies the
bad-data value header records for data drop-outs, and saturated pixels from
the REDR to the output EDR (instead of creating these records as above).

Similarly, if GLLBLEMCOR has already been executed, the bad-data value header
records for low-full-well pixels are copied from the REDR.

.page
REQUIRED INPUT CALIBRATION DATA:

In addition to the input image (PIC), GALSOS requires the following calibration
data:

  (1) a radiometric file generated from calibration data acquired at the same
      filter position as the input image.
  (2) a dark-current file acquired at the same camera gain-state, frame-rate,
      PNI, and blemish-protect mode (BPM) settings.  Note also that extended-
      exposure mode frames require an extended-exposure mode dark-current file.
  (3) a blemish file generated from calibration data acquired at the same filter
      position, gain-state, and camera mode (full-frame or summation).
  (4) a shutter-offset file.

The radiometric and dark-current files are generated by the radiometric
calibration program GALGEN.  Individual dark-current frames or PICSUMs of
multiple dark-current frames may also be used.  The blemish file is generated
by BLEMGEN and the shutter-offset file is generated by CCDRECIP (Ref. 1).

GALSOS will automatically construct
the filenames for the radiometric, dark-current, blemish, and shutter-offset
files using camera parameter information extracted from the VICAR label
(see DETERMINATION OF CAMERA PARAMETERS below).  This assumes that all
calibration filenames adhere to the naming convention documented in subroutine
GLLCALNAME.  The directory locations, however, must be specified via one or
more of the following parameters:

  DIR		Default disk directory for all calibration files.
  DIRCAL	Directory containing the radiometric file.
  DIRDC		Directory containing the dark-current file.
  DIRBLM	Directory containing the blemish file.
  DIROFF	Directory containing the shutter-offset file.

For example,

  GALSOS  INP=S1.1  OUT=S1.EDR  DIR=/gll/law/cal/  DIRDC=/gll1/gmy/

The calibration filenames constructed by GALSOS may be overridden via the 
following parameters:

  CAL		Radiometric file
  DC		Dark-current file
  BLEM		Blemish file
  OFFSETS	Shutter-offset file

For example,

  GALSOS  INP=S12345.1  OUT=S12345.EDR  DIR=GLL:[LAW]  CAL=R2D2.RC

specifies that radiometric file R2D2.RC should be used. All other calibration
files are expected to be in directory GLL:[LAW].

.page
DETERMINATION OF CAMERA PARAMETERS

The radiometric correction process is dependent on the following camera
settings: filter position, exposure time, gain state, frame rate,
extended-exposure mode, inverted/noinverted mode (PNI), and blemish
protection mode (BPM).  These parameters effect both the calibration files
selected and the equations used to perform the correction.

GALSOS extracts the filter position, exposure time, and frame-rate
from the label of the input.

GALSOS extracts the filter position from the label of the radiometric file.

GALSOS extracts the frame-rate, extended exposure mode, PNI, and BPM settings
from the label of the dark-current file.

GALSOS compares the camera parameters for the input images with those for each
of the calibration files.  If the input calibration files are incompatible with
the input image, processing is aborted.

If the keyword 'NOCHECK is specified, a warning message is printed whenever
a parameter mismatch is detected but processing continues.

.page
CORRECTING ERRONEOUS LABEL INFORMATION

As mentioned above, GALSOS extracts various camera parameters from the VICAR
labels of the input image and the radiometric, dark-current, and blemish
files.  In addition, label item SOLRANGE is required to convert the output
pixels into units of reflectance (see below).  When the VICAR label contains
erroneous information, it must be corrected prior to input to GALSOS using
program LABEL-REPLACE.

Since LABEL-REPLACE cannot be used to correct ground calibration labels,
because of the obsolete IBM label format, the following parameters may be used
for these files only:

  (1) Parameters FILTER, EXPOSURE, and GAIN override the input image label.
  (2) Parameters CFILTER and CGAIN override the radiometric file label.
  (3) Parameter DCGAIN overrides the dark-current file label.

Note: All ground calibration frames logged prior to November 10, 1988 contained
erroneous values for the gain-state.  Some have since been corrected manually.

.page
RADIOMETRIC CORRECTON:

GALSOS radiometrically corrects the input image using a linear model of the
light-transfer function.  The radiometric correction is performed on a pixel-
by-pixel basis using the following steps:
 
 1. If the pixel is identified in the Blemish File as a low-full-well
    pixel, then the pixel is checked to see if its DN value exceeds
    the full-well-saturation DN for that pixel.  If so, it is flagged
    as a blemish internally to be interpolated over.

 2. The radiometric correction is applied:
                e = z(d - dc)
    where z is retrieved from the Radiometric File and dc is retrieved from
    the Dark-Current File.

 3. Pixels which have been flagged internally as blemishes are interpolated
    over.

 4. The output pixel is scaled to radiometric units R (units of reflectance
    or units of radiance). The user determines the output picture scale by 
    specifying the parameter IOF (reflectance) or CONV (radiance).  The default
    is IOF=1.  (See IOF and CONV parameters)  The scaling is as follows:

	A1 = number of 'I over F' units per DN, where 10,000 I/F units
             would be produced by normal incidence of sunlight on a 
             a Lambert disk at the planet's distance from the sun.
        A2 = number of nanowatts per cm**2 per steradian per nanometer
             wavelength per DN.
.page
    If IOF is specified, then

                                   S1       K
                r = 10000 * e * -------- * --- (D/5.2)**2
                                A1(t-to)    Ko
    where

        S1 = filter-dependent conversion factor from ft-Lamberts to
             I/F units for a reference distance of 5.2 AU from the Sun.
        t  = commanded exposure time of the input image (msec).
        to = line-dependent shutter offset.
        K  = system gain constant for the gain-state of the image.
        Ko = system gain constant for the calibration file gain-state.
        D  = target distance from the Sun (in AU).

    The target distance from the sun is extracted from VICAR label item 
    SOLRANGE.

    Because SOLRANGE was unavailable in Phase 1 images, the solar range
    is determined using SPICE (for these images).  Note that this requires
    knowledge of the target body.  If the target body in the label is
    incorrect, it should be input via the TARGET parameter.  If the solar
    range cannot be determined via SPICE, the following constants (in AU)
    are used: Venus=0.723331, Earth=1.0, Gaspra=2.2016, Ida=2.9485,
    Jupiter=5.2. For ground calibration images, SOLRANGE=5.2.  SOLRANGE
    can be specified directly via the SOLRANGE parameter.

.page
    If CONV is specified, then

                           S2       K
                r = e * -------- * ---
                        A2(t-to)    Ko

    where S2 is the filter-dependent conversion factor from ft-Lamberts
    to units of nanowatts/cm**2/steradian/nanometer.

 5. The output DN is converted to the output 16-bit data format.  The
    output DN range is -32768 to +32767.  Although a negative DN value
    has no physical meaning, this may occur if the dark-current frame has
    a higher dark-current level than that of the input image.

The use of the IOF parameter is to control that the range of DNs output by 
GALSOS are within the halfword range and above zero DN.  As a rule of thumb, 
a target of Jupiter should use the default IOF=1 value, satellites should 
use .1 and rings should use .01.  The user should remember to use the IOF 
value from the VICAR label when converting from DN to reflectance.

If the keyword 'UBWC is specified, the input DN values (d) will be corrected
for uneven-bit-weighting due to the Analog-To-Digital Converter (ADC).  There
are inaccuracies in the operation of the ADC that result in DN bins of unequal 
width.  A table to adjust for the uneven bin distribution is used by GALSOS to
define the correct DN value.  If the input dark-current file is in byte 
format (i.e. an individual dark-current frame), then the correction
will be applied to the dark-current as well.  The current correction table was
supplied by Herb Breneman, 2 Mar 89, and is based on 1975 calibration data.

The UBWC parameter should be used at all times but the correction is most 
effective in when no noise is introduced by compression.  In other words, UBWC
is more effective on lossless compressed images and least effective in lossy 
compressed images.  

.page
ENTROPY CALCULATION:

GALSOS also calculates the entropy of the input (raw) image and stores the
results in the VICAR label.  The average entropy for every 50th line is
calculated and stored in the binary header portion of the VICAR label (ref. 5).
The average entropy of the entire image is stored in the visible portion of the
VICAR label.  Both sets of calculations ignore data drop-outs and Reed/Solomon
overflow lines.  Entropy is in units of bits/pixel.

If the entropy has already been calculated by BADLABELS, this information is
copied from the input image and the entropy calculation is suppressed.  Note,
however, that the user can force a recalculation of the entropy by specifying
'PENTROPY.

Keyword 'PENTROPY causes the entropy for every 50th line and the entire image
to be printed.

.page
CREATING THE OUTPUT IMAGE LABEL:

If the input image (PIC) is a UDR or REDR, the output image will be in EDR
format (ref 5).  Otherwise, the output will contain no binary label information.

GALSOS copies all binary label information from the input REDR to the
output EDR.  Bad-data records are copied or added as described above (see
TRACKING BAD-DATA VALUES).

The following information is added to the EDR header record:
        SCALE(1)        DN-to-reflectance scale factor
        SCALE(2)        DN-to-radiance scale factor
        Slope-File      Radiometric file name
        Offset-File     Dark-current file name

The ASCII text portion of the label is copied from the UDR/REDR and the
following label items added:

        IOF             DN-to-reflectance scale factor
        CNV             DN-to-radiance scale factor
        CAL             Radiometric file name
        DC              Dark-current file name
        BLM             Blemish file name
        SO              Shutter-offset file name

Finally label item NLB is updated to reflect all binary labels added.

.page
EXAMPLES:

  1) GALSOS  A  B  DIR=/gll/law/

     The input image A is radiometrically corrected.  Since the radiometric
     scaling is defaulted, the output image B will be in units of reflectance
     (IOF), with an IOF scaling factor of 1.0.  All calibration files are
     assumed to be located in directory /gll/law and to follow the file
     naming conventions defined by subroutine GLLCALNAME.

  2) GALSOS  A  B  DIR=/gll/law/  DIRDC=/gll2/law/  CAL=CAL.GRN  DC=DC.GRN +
         BLEM=BLEM.DAT  OFFSETS=OFFSETS.DAT

     All calibration files are specified.  They are found in disk directory
     /gll/law, except for the dark-current file which is in /gll2/law. 
     Processing is to continue even if the calibration files are considered
     incompatible with the input image ('NOCHECK keyword). 

.page
PROGRAM HISTORY:

Original  Programmer of GALSOS:  Gary Yagi, 1979
Current Cognizant Programmer:  Lucas Kamp

During the Venus and Earth-1 encounters, images were taken through an optics
cover.  The cover was jettisoned prior to the Gaspra encounter.  Before the
G29 orbit, all exposures were preceded by a light flood.  Beginning with G29,
all exposures were taken without a light flood.

Because of changes to camera sensitivity, different ft-Lamberts to reflectivity
and ft-Lamberts to nanowatts conversion factors (S1 and S2 above) are used for
the following phases of the mission: (1) Venus, (2) Earth-1, (3) Gaspra,
(4) G29 and beyond.

The original conversion tables are from Ken Klaasen's memo, July 2, 1991.
The values for Gaspra are from Klaasen's memo, Nov 18, 1997.
The values for G29 are from Klaasen's memo, Feb 11, 2002.

The current system gain constants are from a memo by Klaasen dated April 1992.

Revisions:
  13 Dec 10   LWK  Revised pgm to work on PDS REDRs only.
  22 Feb 02   GMY  Add reflectance and radiance coversion tables for G29.
  19 MAY 99   HBM           Update help for UBWC and IOF
  26 Oct 98   GMY           Extract solar range from label or SPICE.
  13 AUG 98...R.R.PATEL.....REMOVED TRANS_BLEM SINCE IT WAS CAUSING PROBLEM
                            ON VMS. IT IS NOW REPLACED WITH ZVREAD (REFER TO
                            AR-100482).
  30 MAR 98...R.R.PATEL.....UPDATED TO WORK WITH SUMMATION MODE FLIGHT-IMAGES
                            WITH NO BINARY HEADER.
   7 Dec 97   G. Yagi       Update radiance scale to Nov 18 memo
  30 APR 97...R.R.PATEL.....REMOVED THE CHECK FOR HISTORY LABEL ENTROPY AND
                            ADDED CHECK TO SEE IF BADLABELS HAS BEEN RUN ON
                            INPUT.
   7 Feb 97   G. Yagi	    Correct call to zlget to retrieve ENTROPY from
                            label (FR 89917)
  14 Aug 96   G. Yagi	    Add SSI readout mode to call to GLLCALNAME
                            (FR 89118)
   1 Jul 96   G. Yagi       Update Gaspra reflectance/radiance scale factors
  21 Jun 95   G. Yagi       Fix badlabels (FR 85892)
                            Modify so it compiles when Sybase is not available.
  10 May 95   G. Yagi       Update reflectivity and radiance constants as per
                            Klaasen memo dated May 8 1995 (FR 85151).
  10 Apr 95   G. Yagi       Check for invalid SEGMENTS field in Ph2 UDRs
                                (FR 85642).
  27 Mar 95   G. Yagi       Fix bug getting PICSCALE for halfword DC (FR 85641).
  10 Mar 95   G. Yagi       More Phase2 changes
  18 Nov 94   T. Truong	    Added PhaseII and Sybase interface
                            Removed support for XED telemetry format
   5 May 94   G. Yagi       Recognize Ida as a target (FR 85136).
  15 Dec 93   G. Yagi       Skip blemish removal if line is missing (FR 81894).
  13 Dec 93   G. Yagi       Changed to accept 800x800 summation-mode dark
                            current files (FR 81876).
                            Changed to extract last instance of PICSCALE in the
                            VICAR label (FR 81877).
                            Increase blemish file size to 10,000.
  03 Aug 93   G. Yagi       Added Ida and Moon as valid targets (FR 81828)
  13 Jul 93   G. Yagi       Fixed low-full-well bug (FR 81855)
                            Store FTLTORAD and FTLTOIOF in catalog (FR 81826)
                            Get SCLK from ABLE86 (FR 81747)
  04 Sep 92...W. Lee........Resolved Catalog probelm (FR #75744)
  02 Sep 92...W. Lee........Fixed Entropy calculation for Reed-Solomon UDR
                            (FR #76878)
  08 Jul 92...W. Lee........Fixed "White Spot" troubles associated with 
                            GLLBLEMCORed REDR (FR #75754 -- GLLBLEMCOR)
  02 Jul 92...W. Lee........Removed Gain-Constant-Ratio dependency in Radio-
                            metric correction (FR #76877)
  30 Jun 92...W. Lee........Removed Reed-Solomon Overflows from Entropy Cal-
                            culations (FR #76878)
  25 Jun 92...W. Lee........Disabled the usage of Reed-Solomon Overflow for
                            LFW corrections (FR #75724)
  15 Jun 92...W. Lee........Restructured Input Logical Mechanism for Cali-
                            bration Files (FR #75726)
  10 Jun 92...W. Lee........Added informative messages about the loading of
                            the SSI_CORRECTED catalogue (FR #75744) 
  01 Jun 92...G. Yagi.......Major upgrade to help file.  
  25 Feb 92...W. Lee........Miscellaneous Upgrades:  (NO FRs !)
                            Updated HELPs, Included EXT for calibration 
                            files in Label, Rejected Input if it is a BLEM-
                            ished UDR etc.  
  25 Feb 92...W. Lee........Implemented the "Consolidated Slope" approach
                            ONLY for the RADIOMETRIC mode (FR #68966)
  18 Feb 92...W. Lee........Removed large discrepancies among EDRs generated 
                            via different "routes" (FR #73797)
  12 Feb 92...W. Lee........Updated Gain Ratios to the 1991 values
                            (FR #66567)
  07 Feb 92...W. Lee........Resolved problem associated with the "GLLBLEMCOR" 
                            message (FR #73795)  
  05 Feb 92...W. Lee........Resolved DIR & DISK issue for CALIBRATION file in
                            association with the RADIOMETRIC mode (FR #73800)
  24 Jan 92...W. Lee........Fixed DEV crash problem  (FR #73782)
  27 Dec 91...W. Lee........Changed "BLM" Label insertion so that output EDR can
                            be processed by BADSHOW (FR #73714)
  16 Dec 91...W. Lee........Modified to correctly verify BADLABEL & GLLBLEMCOR
                            processings for an input CDROM REDR (FR #70966)
  14 Nov 91...W. Lee........Closed FR #70937 with NO software corrections 
  07 Nov 91...W. Lee........Incorporated PCA Capability
  28 Oct 91...W. Lee........ON-OFF changed to UBWC-NOUBWC (#70927)
  28 Oct 91...W. Lee........All References to VIO changed to VLT (FR #70926)
  07 Oct 91...W. Lee........Added a preceding statement about the 'DISALLOW 
                            keyword (FR #70928)
  30 Aug 91...W. Lee........Conversion Factors depend on Mission PHASE
                            (FR #70924)
  30 Aug 91...W. Lee........Fixed Empty Blank problem for OFFSET in binary
                            header (FR # 70925)
  14 Aug 91...W. Lee........Fixed Ground Summation problem (FR #70904)
  09 Aug 91...W. Lee........Updated Conversion Factors based on latest memo
                            by K. Klaasen (FR #70902)
  30 Jul 91...W. Lee........Implemented modifications to process REDR image
                            (SCR #B071)
  24 Jun 91...W. Lee........Put Dark-Current file in the OFFSET location in
                            Binary Header (FR #66503)
  10 Jun 91...W. Lee........Added Radiometric Table Interface for Calibration
                            Files extraction
  05 Jun 91...W. Lee........Changed "ON/OFF" to "UBWC/NOUBWC" for Bit-Weighting
                            (LKW320 Mail, dated June-04-91) 
  17 May 91...W. Lee........Added "RS_OVERFLOW" BDV in Binary Header
                            (IOM 384-91-3 by GMY, dated May-14-91, FR #63282)
  07 May 91...W. Lee........Set Edr_Status = 1 (= On DISK) in Corrected Table
                            (FR #68831) 
  30 Apr 91...W. Lee........Implemented Independent Directories for Calibra-
                            tion Files (FR #68858)
  25 Apr 91...W. Lee........Updated Filter Paramters for Venus and Earth-1
                            Mission (FR #64651) 
  09 Apr 91...W. Lee........Added "GALSOS_DONE" in Overview Table (FR #64653)
  02 Apr 91...W. Lee........Added GASPRA to TARGET (FRs #64650 & 66673)
                            If TARGET is NOT specified by User, pull the Target
                            name from VICAR label
  01 Apr 91...W. Lee........Disabled Frame-Rate inconsistency check between
                            input Image and Blemish file (FR #64617)
  31 Mar 91...W. Lee........Moved ground calibration parameters to the bottom
                            of the PDF parameter list (FR #66589)
  30 Nov 90...A.Runkle......Changed VICAR label keyword ENTRPY to ENTROPY
  11 Nov 90...A.Runkle......Changed parameters so GALSOS can run in a
                            proceedure properly
                            Added assignment of RAW_VERSION in catalog update
                            Added Blemish file checks for filter, gain & rate
                            Corrects summation mode usage of shutter offset
                            file
  30 Oct 90...G.Yagi........Adjusted filter factors for optics cover
  04 Oct 90...A.Runkle......Changed NOCAT parameter to CATALOG and added a
                            a new default & new values
                            Changed IOF and CONV parameters to SCALETYP and
                            SCALEVAL for easier use in procs.
  23 Aug 90...A.Runkle......Test script update
                            Replace SOLRANGE when TARGET specified
  15 Jul 90...A.Runkle......Changed locdation of closing files
                            Added another status check in catalog code
  01 Jun 90...A.Runkle......Added PENTROPY keyword & printing code
  25 Apr 90...A.Runkle......Added NOCAT keyword & catalog entry code
                            Added BADFILES keyword & bypass code
                            Changed QPRINTS to XVMESSAGE
                            Corrected GLLCALNAME call for shutter offset file
  29 Mar 90...A.Runkle......Added TARGET keyword
                            Corrected variable declaration and usage
                            Fixed GLLCALNAME parameter list
  14 Feb 90...A.Runkle......Corrected algorithm for calculating A1 & A2 values
                            Added entropy calculations
  26 Oct 89...A.Runkle......Corrected test script
                            New ABLE86 parameter structure
  14 Jul 89...A.Runkle......Support Bad Data Value records
  28 Mar 89...A.Runkle......Support flight data (binary headers)
  12 Mar 89...G.Yagi........Delete halfword input capability.
  08 Mar 89...G.Yagi........Add uneven-bit-weight correction option.
  01 Nov 88...G.Yagi........Fix S1 and S2 conversion tables.
  26 OCT 88...G.Yagi........Fix check for summation mode offsets.
  20 OCT 88...G.Yagi........Fixed EXPOSURE parameter.
  28 MAY 88...G.Yagi........Minor changes to help file (FR #36087).
                            Add processing of double-column blemishes
                            Add CGAIN, DCGAIN parameters
  01 APR 88...G.Yagi........Scale output DN to radiometric units
  10 Dec 87...G.Yagi........New radiometric and offset file formats
  18 May 87...G.Yagi........Do scaling in DECAL subroutine
  20 Mar 86...G.M.YAGI......Assembler MDECAL1 & MDECAL2
  15 MAY 85...D.F.STANFILL..NEW VERSION WITH BLEMGEN IN VAX C
  24 OCT 84...G.M.YAGI......CONVERSION TO VAX VICAR*2
  20 APR 84...M.E.MORRILL...REWRITTEN IN VAX FORTRAN VICAR1*

.LEVEL1
.VARIABLE INP
STRING--REQUIRED
INP=raw image
.VARIABLE OUT
STRING--REQUIRED
Output radiometrically
corrected image.
.VARIABLE DIR
STRING--OPTIONAL
Default directory for
calibration files.
.VARIABLE DIRCAL
STRING--Optional
Directory containing
Radiometric File
.VARIABLE DIRDC
STRING--Optional
Directory containing
Dark Current File
.VARIABLE DIRBLM
STRING--Optional
Directory containing
Blemish File
.VARIABLE DIROFF
STRING--Optional
Directory containing
Shutter OFFset File
.VARIABLE CAL
STRING--Optional
Input Radiometric File.
.VARIABLE DC
STRING--optional
Input Dark-Current File.
.VARIABLE BLEM
STRING--optional
Input Blemish File.
.VARIABLE OFFSETS
STRING--optional
Input Shutter-Offset File.
.VARIABLE SCALETYP
KEYWORD--OPTIONAL
IOF or CONV
.VARIABLE SCALEVAL
Real--optional
IOF per DN or
Radiance per DN 
.VARIABLE BITWT
KEYWORD--OPTIONAL
Uneven-bit-weighting
correction
('UBWC /'NOUBWC)
.VARIABLE SOLRANGE
KEYWORD--OPTIONAL
Solar range (km)
.VARIABLE NOCHECK
KEYWORD--OPTIONAL
Suppresses checking for
parameter mismatches.
.VARIABLE PENTROPY
KEYWORD--OPTIONAL
Print entropy values
.VARIABLE FILTER
INTEGER--OPTIONAL
Filter position for
input image (0-7)
.VARIABLE GAIN
INTEGER--OPTIONAL
Gain state for
input image (1-4)
.VARIABLE EXPOSURE
INTEGER--OPTIONAL
Exposure time for
input image (msec).
.VARIABLE CFILTER
INTEGER--OPTIONAL
Filter position for
Radiometric file (0-7)
.VARIABLE CGAIN
INTEGER--OPTIONAL
GAIN state for
Radiometric file (1-4).
.VARIABLE DCGAIN
INTEGER--OPTIONAL
Gain state for
Dark-Current File (1-4)
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.var DEBUG
Optional keyword
Print diagnostic messages
.LEVEL2
.VARIABLE INP
   INP=IN
 where IN is an image to be radiometrically corrected (byte or halfword).
.VARIABLE OUT
 OUT is the radiometrically corrected output image (16-bit data).
.VARIABLE DIR
 DIR specifies the default disk and directory location of all calibration files.
 DIR may be specified as a VMS DISK:[DIR] or Unix pathname.
	DIR=WMS_GLL:[SSI.RAD]
     or DIR=/project/gll/ssi/rad/
 DIR may be overridden for specific files via parameters DIRCAL, DIRDC, DIRBLM,
 and DIROFF (see below).
.VARIABLE DIRCAL
	DIRCAL=WMS_GLL:[SSI.RAD]	(VMS)
     or DIRCAL=/project/gll/ssi/rad/	(Unix)
 DIRCAL specifies the directory location of the radiometric (slope) file.
 When specified, DIRCAL overrides the default directory (see DIR parameter) for
 the radiometric file only.
.VARIABLE DIRDC
	DIRDC=WMS_GLL:[SSI.RAD]	(VMS)
     or DIRDC=/project/gll/ssi/rad/	(Unix)
 DIRDC specifies the directory location of the dark-current file.
 When specified, DIRDC overrides the default directory (see DIR parameter) for
 the dark-current file only.
.VARIABLE DIRBLM
	DIRBLM=WMS_GLL:[SSI.RAD]	(VMS)
     or DIRBLM=/project/gll/ssi/rad/	(Unix)
 DIRBLM specifies the directory location of the blemish file.
 When specified, DIRBLM overrides the default directory (see DIR parameter) for
 the blemish file only.
.VARIABLE DIROFF
	DIROFF=WMS_GLL:[SSI.RAD]	(VMS)
     or DIROFF=/project/gll/ssi/rad/	(Unix)
 DIROFF specifies the directory location of the shutter-offset file.
 When specified, DIROFF overrides the default directory (see DIR parameter) for
 the shutter-offset file only.
.VARIABLE CAL
 STRING
 Input Radiometric File, as generated by program GALGEN.  The Radiometric
 File's filter position must match the filter position of the input image.
 The filename should NOT include the directory of the file; refer to the 
 DIR and DIRCAL parameters for specifying the directory.
.VARIABLE DC
 STRING
 Input dark current file, in byte or halfword format.  Dark current files
 may be (1) zero exposure frames, (2) a sum of zero exposure frames (see
 PICSUM), or (3) as computed by program GALGEN.

 The dark current file must match the input image in gain state and frame rate.
 Also, extended-exposure mode frames require extended-exposure dark currents.
 The filename should NOT include the directory of the file; refer to the DIR
 and DIRDC parameters for specifying the directory.

.VARIABLE BLEM
 STRING
 Input Blemish File, as generated by program BLEMGEN.  The filename should NOT
 include the directory of the file; refer to the DIR and DIRBLM parameters for 
 specifying the directory.
.VARIABLE OFFSETS
 STRING
 Input Shutter-Offset File, as generated by program CCDRECIP.  The
 Shutter-Offset File is independent of all camera modes, and a single file
 may be used (for a fixed temperature).  The filename should NOT include the
 directory of the file; refer to the DIR and DIROFF parameters for specifying
 the directory.
.VARIABLE IOF
	IOF=r
where r specifies the output DN scale in number of 'I over F' units per DN.
10,000 'I over F' units would be produced by normal incidence of sunlight
on a Lambert disk at Jupiter's distance from the sun (5.2 A.U.).
The default is IOF=1.0.
.VARI CONV
	CONV=r
where r specifies the output DN scale in number of nanowatts per cm**2 per
steradian per nanometer wavelength per DN.
If not specified, the output is in units of IOF.
.VARIABLE BITWT
Keyword 'UBWC specifies uneven-bit-weight correction (default is 'NOUBWC).
.VARIABLE ENTROPY
Keyword 'NOCALC suppresses calculation of image entropy (default is 'CALC).
.VARI SOLRANGE
	SOLRANGE=r
where r specifies the solar range (distance for target to sun) in km.

If not specified the following actions are taken:
  (1) If the input image is in Phase1 format, the following constants are
      used: Venus=0.723331 AU, Earth=1.0 AU, Gaspra=2.2016 AU, Ida=2.9485 AU,
      Jupiter=5.2 AU.
  (2) For Phase 2, SOLRANGE is extracted from the VICAR label.
  (3) For ground calibration images, SOLRANGE=5.2 AU.
.VARIABLE NOCHECK
Keyword 'NOCHECK specifies that processing should continue even though the
specified calibration files do not match the input files parameters: gain,
frame-rate, filter & FIBE (where applicable).  Warning messages identifying
the suspected files are displayed along with a continuation with error
message.  The default is to terminate processing if mismatches are detected.
.VARIABLE PENTROPY
Keyword 'PRINT causes printing of the results of the entropy calculations for
the entire image and for every 50th line (default is 'NOPRINT).  Note that
this keyword can be used to force the entropy to be calculated, even if it
has been previously calculated by BADLABELS.
.VARIABLE FILTER
INTEGER--OPTIONAL (ground calibration data only)
Filter position (0-7)
	FILTER=0  for Clear
	      =1  for Green
	      =2  for Red
	      =3  for Violet
	      =4  for 7560
	      =5  for 9680
	      =6  for 7270
	      =7  for 8890
.VARIABLE GAIN
INTEGER--OPTIONAL (ground calibration data only)
Specifies camera gain state for input image.
	GAIN=1  for 400K (lowest gain state)
	    =2  for 100K
	    =3  for 40K
	    =4  for 10K  (highest gain state)
The default gain-state is 4 for 800x800 images, and 1 for 400x400
(summation-mode) images.
.VARIABLE EXPOSURE
INTEGER--OPTIONAL (ground calibration data only)
Specifies the exposure time of the input image (msec).
.VARIABLE CFILTER
INTEGER--OPTIONAL (ground calibration data only)
Filter position for
calibration file (0-7)
.VARIABLE CGAIN
INTEGER--OPTIONAL (ground calibration data only)
Specifies camera gain state for calibration file.
       CGAIN=1  for 400K (lowest gain state)
	    =2  for 100K
	    =3  for 40K
	    =4  for 10K  (highest gain state)
The default gain-state is 2 for 800x800 images, and 1 for 400x400
(summation-mode) images.
.VARIABLE DCGAIN
INTEGER--OPTIONAL (ground calibration data only)
Specifies gain state for the Dark-Current File.
      DCGAIN=1  for 400K (lowest gain state)
	    =2  for 100K
	    =3  for 40K
	    =4  for 10K  (highest gain state)
The default gain-state is 2 for 800x800 images, and 1 for 400x400
(summation-mode) images.
.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.var DEBUG
'DEBUG causes diagnostic messages to be printed (for debugging the program).
.End
$ Return
$!#############################################################################
$Doc_File:
$ create get_ssi_ph2_prefix.hlp
$ DECK/DOLLARS="$ VOKAGLEVE"
1  Subroutine  get_ssi_ph2_prefix

     Calling Sequence:   get_ssi_ph2_prefix (unit, line, dest)

2  Argument 

   unit:  Unit #,  Input
   line:  Line #,  Input
   dest:  Prefix   buffer, Output

2  Operation 

   Subroutine get_ssi_ph2_prefix reads & loads the binary prefix of a GLL SSI 
   line record(for Phase II).The parameter "unit" refers to the INPUT file 
   unit #, and "line" is the line # of the file to be read.  The input file 
   must be open prior to the get_ssi_ph2_prefix calling.

2  History

   Original Programmer:  F. Moss  Aug-1994
   Source Language:      C (MSTP  Portable)
   Program History:
    27 Jan 98  GMY  Switched from C compiler to ANSI C (package=signedchar).
                    Fixed two bugs in test program.
    09 Feb 98  GMY  Moved test image to WMS.
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_bin.hlp
$ DECK/DOLLARS="$ VOKAGLEVE"
1 GLL_SSI_BIN

	GLL_SSI_BIN is a UNIX-ported set of subroutines which provide the
	following following functions for Galileo EDR and UDRS:
        - read/write EDR and UDR binary telemetry headers:

		get_ssi_telem_hdr
		write_ssi_telem_hdr

        - read/write one line of EDR or UDR binary bad data value header 
	  information:
	
		get_ssi_bdv_hdr_rec
		write_ssi_bdv_hdr_rec

        - read/write one EDR or UDR image line:

		get_gll_edr_ln
		write_gll_edr_ln
		get_gll_udr_ln
		write_gll_udr_ln

        - write EDR or UDR binary image line prefixes

		write_ssi_prefix

	(The functionality for reading EDR and UDR binary image prefixes is
	 given by the subroutine "GET_SSI_PREFIX".)

2 CALLING SEQUENCE

	These routines are C-callable only. 

	For examples on how to call any of these routines, see the program
	GLLFILLIN. GLLFILLIN also contains upper-level routines which
	read and write entire EDR and UDR bad data value headers and images.

        get_ssi_telem_hdr(unit,dest)
		int unit;	
		ssi_edr_hdr_typ *dest;

	write_ssi_telem_hdr(unit,source)
		int unit;
		ssi_edr_hdr_type *source;

	get_ssi_bdv_hdr_rec(unit,line,dest)
		int unit, line;
		ssi_bdvyh_typ *dest;

	write_ssi_bdv_hdr_rec(unit,line,source)
		int unit, line;
		ssi_bdvh_typ *source;

	get_gll_edr_ln(unit,line,dest)
		int unit, line;
		ssi_edr_ln_typ *dest;

	write_gll_edr_ln(unit,line,source)
		int unit,line;
		ssi_edr_ln_typ *source;

	get_gll_udr_ln(unit,line,dest)
		int unit, line;
		ssi_udr_ln_typ *dest;

	write_gll_udr_ln(unit,line,source)
		int unit,line;
		ssi_udr_ln_typ *source;
   	

	write_ssi_prefix(unit,line,source)
		int unit, line;
		ssi_lhdr_typ *source;

2 HISTORY
	
	Original Programmer: Megan O'Shaughnessy (credit for telemetry routines
	                     and many internal routines, to R. Deen.)

        Note: the unit test for this module is the unit test for the program
	      GLLFILLIN.

	Revision:	
	21 Jun 1993	MOS. Original implementation.
	25 Aug 1993	MOS. FR 81795. Corrected minor problem in imake file
			which was causing compilation warnings under the 
			new UNIX compilers.
        01 Jul 1994     RNR(CRI) changed comment to indicate files are 
                        written in the VAX format for binary data. Added mask
                        of 0xFF in get_ssi_bdv_hdr_rec() at test for line image.

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gllcalname.hlp
$ DECK/DOLLARS="$ VOKAGLEVE"
Purpose:

This routine returns the file name of the proper calibration file from
a set of defining parameters.  This routine only works for consolidated
radiometric files (i.e., those files which scale a gain state ratio instead
of using a specific file for each gain state).

Calling Sequence:

   Fortran:          GLLCALNAME ( File_name, Type, Status, 
                                  Filter, Frame_rate, Gain, FIBE, Rmode )

   C:       status = gllcalname ( File_name, Type,
                                  Filter, Frame_rate, Gain, FIBE, Rmode )
.page
Arguments:

   Input:
	Type		- The type of calibration file requested.
				1 - Radiometeric
				2 - Dark Current
				3 - Blemish
				4 - Shutter Offset
	Filter		- Camera filter used for exposure (0 thru 7).
	Frame_rate	- Frame rate (1 thru 4 for phase I, 1 thru 5 for phase II).
	Gain		- Camera gain state (1 thru 4).
	FIBE		- Integer value from FIBE label element in LAB02 label
			  item.  The field contains the flags for Light flood,
			  Parallel clock, Blemish protection and Extended
			  exposure.  All but the Light flood are used.
			  The FIBE is stored as if it were a binary number.
			  Each character refers to a bit position of the binary
			  number.  As an example, the number 1011 would specify
			  that the options: Light flood, Blemish protection &
			  Extended exposure were active.
	Rmode		- SSI readoutmode (0=not applicable,1=sample,
			  2=contiguous)
.page			
   Output:
	File_name	- Character array which will contain the calibration
			  file name after the routine is called.
	Status		- Using Fortran, the status is returned in the
                          parameter list.  
                        - Using C, the status is returned in the function 
                          return.
                        - A status of 'TRUE' is returned if all required 
                        - parameters are correct;
			- FALSE if any required parameter is incorrect.
.page
	The required parameters will differ for the different types of
calibration files requested, although all the parameters must have an
associated value.  Those parameters not needed will be ignored.

	The parameter's value corresponds to the output values of the
ABLE86 routine.  Required parameters which do not correspond will return
an error.

NOTE: The returned file name does not include disk or directory information.
.page
Operation:

	Until the calibration files are maintained by a database, the file names
will be subject to a strict naming convention.


Calibration File Names:

	The calibration file name is broken into six descrete sections relating
to the different parameters which define the calibration file to be used.  The
sections are ordered as follows to define a calibration file name:

    <Fltr Pos> <Gain> <Frm Mode> <Frm Rate> <FIBE> <Rmode> . <Cal Type>

If certain parameters are not required for a given calibration type, then that
section should be ignored (i.e., Radiometric calibration is based only on the
filter position and frame mode, hence only those two sections are specified).
Since the Shutter Offset calibration file does not rely on any parameters for
determining the file name, there is only one file.  It as arbitrarily been
named "CALIBRATION.SO".
.page
	Sections 	Valid Mnemonics

	<Fltr Pos>	CLR - Clear filter	(position 0)
			GRN - Green filter	(position 1)
			RED - Red filter	(position 2)
			VLT - Violet filter	(position 3)
			756 - 7560 filter	(position 4)
			968 - 9680 filter	(position 5)
			727 - 7270 filter	(position 6)
		or	889 - 8890 filter	(position 7)

	<Gain>		1 - 400 k    Corresponds directly to the gain state
			2 - 100 k    used.
			3 -  40 k
		or	4 -  10 k

	<Frm Mode>	F - Full Frame mode
		or	S - Summation mode
.page
	Sections 	Valid Mnemonics

	<Frm Rate>		Actual		VICAR lbl	Calibration
				Value		  Value		 Log Value
			     ==========		=========	===========
			2  -  2 1/3 sec		    1		     3
			8  -  8 2/3 sec		    2		     1
			30 - 30 1/3 sec		    3		     2
			60 - 60 2/3 sec		    4		     0
	            or  15 - 15 1/6 sec		    5    	     ? (Phase II)

	<FIBE>		I - if the Inverted Clock option is used
				(FIBE = x1xx; x's mean value doesn't matter)
			B - if the Blemish Protection option is used
				(FIBE = xx1x; x's mean value doesn't matter)
		and/or  X - if the Extended Exposure option is used
				(FIBE = xxx1; x's mean value doesn't matter)
			If there are more than one flag set, the characters
			will be appended in the order I, B then X.
.page
	Sections 	Valid Mnemonics

	<Rmode>		C - if SSI readoutmode is "CONTIGUOUS"
			R - if SSI readoutmode is "SAMPLE"

	<Cal Type>	CAL - Radiometeric
			DC  - Dark Current
			BLM - Blemish
			SO  - Shuter Offset
.page
	Examples:

		Dark current file name:	1F30IXR.DC
			Gain:	1
			Mode:	Full Frame
			Rate:	30 1/3 Seconds
			Opts:	Inverted
				Extended Exposure
			Rmode:	1 (sample)

		Radiometric file name:	VIOS.CAL
			Filter:	Violet (position 3)
			Mode:	Summation

		Blemish file name:	RED2F.BLM
			Filter:	Red (position 2)
			Gain:	2
			Mode:	Full Frame
.page
		Shutter Offset:		CALIBRATION.SO
			Since the Shutter Offset calibration file does not
		rely on any parameters for determining the file name, there is
		only one file.  It has arbitrarily been named "CALIBRATION.SO".
.page
Future Extensions:

	When the calibration files are maintained by a database system, this
routine will access the data base for the proper filename.  Additional
capabilities might include:

	1 - Naming convention may be relaxed.
	2 - Inclusion of directory name for the appropriate calibration file.
	3 - Additional use of date & time in determining proper files.
	4 - Additional use of Jovian orbit number in determining proper files.

History:

  Original Programmer:	   A. Runkle   10 March '89
  Cognizant Programmer:	   A. Runkle

  Source Language: 	   C
  Fortran Bridge Language: C

-  01-Apr-91...W. Lee.....Changed "VIO" to "VLT" for Violet Filter (FR #64616) 
-  Made portable for UNIX (CRI)                                 21 April 1994  
-  Modify to work with Phase II(FFM)                            21 Nov   1994  
-  Modify code to always return filename in lower case          2  Dec   1994
-  Fix bug so it recognizes 15 1/3 sec frame rates (85143)(GMY) 23 Apr   1995
-  13 AUG 96 GMY Add "R" to dark current file name for SAMPLE readout (89118)
   You can use tstgalsos.pdf to test this subroutine.
Jul 99  GMY  For HMA or HCA, add "C" to dark current filename for SAMPLE
             readout
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_ssi_prefix.hlp
$ DECK/DOLLARS="$ VOKAGLEVE"
1  Subroutine  get_ssi_prefix

     Calling Sequence:   get_ssi_prefix (unit, line, dest)

2  Argument 

   unit:  Unit #,  Input
   line:  Line #,  Input
   dest:  Prefix   buffer, Output

2  Operation 

   Subroutine get_ssi_prefix reads & loads the binary prefix of a GLL SSI 
   line record.  The parameter "unit" refers to the INPUT file unit #, and
   "line" is the line # of the file to be read.  The input file must be open 
   prior to the get_ssi_prefix calling.

2  History

   Original Programmer:  Wen-Piao  Lee    Sept-1992
   Source Language:      C (MSTP  Portable)

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gll_ssi_bin_ph2.hlp
$ DECK/DOLLARS="$ VOKAGLEVE"
1 gll_ssi_bin_ph2

	gll_ssi_bin_ph2 is a set of subroutines which provide the
	following following functions for Galileo SSI Phase 2 EDRs and UDRs:
        - read/write EDR and UDR binary telemetry headers:

		get_ssi_ph2_telem_hdr
		write_ssi_ph2_telem_hdr

        - read/write one EDR or UDR image line:

		get_gll_ph2_ssi_edr_ln
		write_gll_ph2_ssi_edr_ln
		get_gll_ph2_ssi_udr_ln
		write_gll_ph2_ssi_udr_ln

        - write EDR or UDR binary image line prefixes

		write_ssi_ph2_prefix

	(The functionality for reading EDR and UDR binary image prefixes is
	 given by the subroutine "GET_SSI_PH2_PREFIX".)

2 CALLING SEQUENCE

	These routines are C-callable only. 

	For examples on how to call any of these routines, see the program
	GLLFILLIN. 

        get_ssi_ph2_telem_hdr(unit,dest)
		int unit;	
		ssi_hdr_typ *dest;

	write_ssi_ph2_telem_hdr(unit,source)
		int unit;
		ssi_hdr_type *source;

	get_gll_ph2_edr_ln(unit,line,dest)
		int unit, line;
		ssi_edr_line_typ *dest;

	write_gll_ph2_edr_ln(unit,line,source)
		int unit,line;
		ssi_edr_line_typ *source;

	get_gll_ph2_udr_ln(unit,line,dest)
		int unit, line;
		ssi_line_typ *dest;

	write_gll_ph2_udr_ln(unit,line,source)
		int unit,line;
		ssi_line_typ *source;
   	

	write_ssi_ph2_prefix(unit,line,source)
		int unit, line;
		ssi_prefix_typ *source;

2 HISTORY
	
	Original Programmer: F. Moss

        Note: the unit test for this module is the unit test for the program
	      GLLFILLIN.

	Revision:	
	10 AUG 1994	FFM. Original implementation.
        17 Jan 1995     FFM. Added "&" while calling trout_word23_typ in 
                             routine write_ssi_ph2_telem_hdr to avoid access
                             violation.(FR 85878).
         3 MAY 1996     OAM. Fixed code in get_ssi_ph2_telem_hdr and
                             write_ssi_ph2_telem_hdr to handle phase 2 
                             data in full mode.(FR 87159, FR 89146).
         2 Jun 96  GMY  Fix write_ssi_ph2_prefix to store rct properly.
        26 Sep 96  HBM/DDK Merge Damon's fixes for RA, DEC, TWIST and 
                        CLOCK_ANGLE with previous OAM and GMY fixes.  Damon's 
			fixes are to save the values as ASCII floats and 
			not halfword integers.
        04 Dec 96  HBM  Fix trout_word23_typ, trout_word24_typ, trout_word25_typ,
                        trout_word26_typ routines to store the values of the data
                        instead of doing nothing with the data. (FR 89919)
        03 Mar 97  HBM  Fix trin_word25 for img_mode to extract 3 bits instead of 2 bits.
        01 Feb 98  GMY  Change to ANSI C compiler.
$ VOKAGLEVE
$ Return
$!#############################################################################
$Test_File:
$ create tstgalsos.pdf
procedure help=*
!*****************************************************************************
! tstgalsos.pdf - GALSOS unit test for Unix platforms
!*****************************************************************************
refgbl $echo
refgbl $syschar
refgbl $autousage
body

local PATH1 TYPE=STRING init="/project/test_work/testdata/gll/"

let _onfail="stop"
let $echo="yes"
let $autousage="none"
!  
! This EDR was downloaded from PDS imaging node:
galsos &"PATH1"7213r.img 7213.cal dircal=&"PATH1" diroff=&"PATH1" +
 dirdc=&"PATH1" dirblm=&"PATH1"

! check that output is the same:
f2 (7213.cal &"PATH1"7213.cal) d func="in1-in2"
hist d 'nohist

! NOTE: - file 3f8c.dc was downloaded from PDS file 3f8c_dc04.dat and renamed.
!       - file vlt3f.blm was downloaded from PDS file vlt3f_blm02.img and renamed.
!

end-proc
$!-----------------------------------------------------------------------------
$ create tstgalsos.log_solos
tstgalsos
let $autousage="none"
galsos /project/test_work/testdata/gll/7213r.img 7213.cal dircal=/p+
roject/test_work/testdata/gll/ diroff=/project/test_work/testdata/gll/   dirdc=/project/test_work/testdata/gll/ dirblm=/project/tes+
t_work/testdata/gll/
Beginning VICAR task galsos
 GALSOS version 19-Jul-11
Solar range from image label=752771968.0

 
              PIC   CAL   DC   BLM
              ===   ===   ==   ===
 FILTER         3     3    X     3
 GAIN           3     X    3     3
 FRAME-RATE     2     X    2     X
 
 Mission Phase=Phase2
 Reflectivity per Foot-Lambert = 0.0004
 Radiance per Foot-Lambert = 0.8107
 Reflectivity units apply to a solar-illuminated
 surface at 5.2 AU 
 Scaling Type:  IOF 
 Scaling values to be used = 2.0268e+00 
 Exposure = 9.5833e+01 msec
 GALSOS replaced 222 pixels by interpolation
 GALSOS task completed
f2 (7213.cal /project/test_work/testdata/gll/7213.cal) d func="in1-in2"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3153 TIMES
hist d 'nohist
Beginning VICAR task hist
HIST version 11-SEP-11


AVERAGE GRAY LEVEL=0.000000       STANDARD DEVIATION=0.000000       NUMBER ELEMENTS=  640000
MIN. DN=         0
MAX. DN=         0

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgalsos.log_linux
tstgalsos
let $autousage="none"
galsos /project/test_work/testdata/gll/7213r.img 7213.cal dircal=/p+
roject/test_work/testdata/gll/ diroff=/project/test_work/testdata/gll/   dirdc=/project/test_work/testdata/gll/ dirblm=/project/tes+
t_work/testdata/gll/
Beginning VICAR task galsos
 GALSOS version 19-Jul-11
Solar range from image label=752771968.0

 
              PIC   CAL   DC   BLM
              ===   ===   ==   ===
 FILTER         3     3    X     3
 GAIN           3     X    3     3
 FRAME-RATE     2     X    2     X
 
 Mission Phase=Phase2
 Reflectivity per Foot-Lambert = 0.0004
 Radiance per Foot-Lambert = 0.8107
 Reflectivity units apply to a solar-illuminated
 surface at 5.2 AU 
 Scaling Type:  IOF 
 Scaling values to be used = 2.0268e+00 
 Exposure = 9.5833e+01 msec
 GALSOS replaced 222 pixels by interpolation
 GALSOS task completed
f2 (7213.cal /project/test_work/testdata/gll/7213.cal) d func="in1-in2"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3156 TIMES
hist d 'nohist
Beginning VICAR task hist
HIST version 27-JUL-11


AVERAGE GRAY LEVEL=0.000002       STANDARD DEVIATION=0.002165       NUMBER ELEMENTS=  640000
MIN. DN=        -1
MAX. DN=         1

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
