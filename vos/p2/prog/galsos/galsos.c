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
  zvmessage(" GALSOS version 2016-02-13","");
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
	if ((((int) (*current_pixel) > zap_index->sat_dn) && 
	     (ss1 <= zap_index->samp) && (es1 >= zap_index->samp)) ||
	    ((ss2 <= zap_index->samp) && (es2 >= zap_index->samp))) {
	      lfw_flag[(zap_index->samp-1)] = TRUE;
	      zap_index->sat_dn = 0;
	      if (ounit >= 0)
		add_segment(lfw,zap_index->samp,line,nl,nlbout,ounit);
	 }
     }
  } 
  return;
}  /* end subroutine zap_lfw_pixels */
