/******************************************************************************/
/*                                                                            */
/*  This include file contains the defines and data structures needed for the */
/*  R/T image display subroutines contained in writimag.com, and the programs */
/*  that call these subroutines--e.g., the R/T display programs and Browse.   */
/*                                                                            */
/*  Cognizant Programmer:  Paul Bartholomew                                   */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  03/11/93   N/A   PDB - Officially ported to Unix (didn't actually change  */
/*                      anything, though).                                    */
/*                                                                            */
/******************************************************************************/

#define	RAW		 0
#define	MANUAL		 1
#define	TABLE		 2
#define ENDSIN  	 3
#define LOG     	 4
#define AUTO    	 5

#define ODD		 0			/*values of line_ and*/
#define EVEN		 1			/*  sample_select    */

#define	NEW_IMAGE	 0			/*image data event flags*/
#define OLD_IMAGE	 1
#define NEW_SPECTRA	 2			/*pws data event flags*/
#define OLD_SPECTRA	 3
#define NEW_CUBE	 4			/*nims data event flags*/
#define OLD_CUBE	 5
#define MIN_TIMER	 6			/*mintimer event flag*/
#define	MAX_TABLE	16			/*max pairs, table stretch*/ 
#define NUM_BUF		 4			/*number of frame buffers*/
#define LUT_ERROR	 1			/*returned errors */
#define IMAGE_ERROR 	 2
#define GRAPHICS_ERROR   4
#define DEVICE_ERROR	 8
#define SYSTEM_ERROR    16
#define FORMAT_ERROR    32
#define FPS_ERROR	64
#define IMAGE		 1			/*last display = image*/

#define ISS		 0			/*instrument = ISS *//* VGR */
#define PRA		 1			/*instrument = PRA *//* VGR */
#define PWS		 2			/*instrument = PWS *//* VGR */
#define SSI		 3			/*instrument = SSI *//* GLL */

#define WA		 0			/*narrow angle stretch*/
#define NA		 1			/*wide angle stretch*/

#define DT_SPACECRAFT	 6
#define DT_FILTER	 8
#define DT_INSTRUMENT	 7
#define DT_EXP_TIME	10
#define DT_SHUT_MODE	10
#define DT_DATA_MODE	10
#define DT_TARGET	14
#define DT_PICNO	14

struct STRETCH {
   int type;
   struct {
      struct {
         int inlo;
         int inhi;
      } manual;
      struct {
         int nvalues;
         int dn[MAX_TABLE*2];
      } table;
      struct {
         int stlo;
         int sthi;
         float perlo;
         float perhi;
         int outlo;
         int outhi;
      } endsin;
   } parm;
};					
 
struct display {
   int   filter_on;
   char  ftype[4];
   int   length;
   int   thresh;
   int   addback;
   int   pweight;
   int   nweight;
   int   medbias;
   int   fdsc_mod16;
   int   fdsc_mod60;
   int   ert_year;
   int   ert_day;
   int   ert_hour;
   int   ert_min;
   char  picno[DT_PICNO];
   char  target[DT_TARGET];
   char  spacecraft[DT_SPACECRAFT];
   int   range;
   float resolution;
   int   solar_angle;
   int   sc_angle;
   int   phase_angle;
   char  filter[DT_FILTER];
   char  exp_time[DT_EXP_TIME];
   int   inst_mode;
   char  shut_mode[DT_SHUT_MODE];
   char  data_mode[DT_DATA_MODE];
   int   instrument;
   char  instr_ascii[DT_INSTRUMENT];
   int   which_stretch;
   int   histogram[256];
   int   buf_num;		/*frame buffer number*/	
   int   line_select;		/*}see ODD, EVEN above*/
   int   sample_select;		/*} 	        */
   int   start_sample;		/*1st sample to process (ODD)*/
   int   number_samples;	/*num samples to process (EVEN)*/
   int   start_line;		/*1st line to process (ODD)*/
   int   number_lines;		/*num lines to process (EVEN)*/
   int   compression_on;	/*indicates use of valid_pixels[]*/
   char  version[4];
   char  program[5];
   char  fill[3];
   char  device[6];		/*display device*/
   struct STRETCH stretch[2];	/*param set selected by which_stretch*/
   int   finil;			/*final low/high in/out parameters*/
   int   finih;			/*	(set by stretch routine)  */
   int   finol;
   int   finoh;
   int   errors;		/*errors in write_image()*/
   int   exit;			/*write_image should exit*/
   int   min_time;		/*min display time*/
   int   dc_correct;		/*dc correction on*/
   int   last_display;		/*which display format up*/
   int   pws_exit;		/*write_pws should exit*/
   int   pws_raw;		/*0:spectrogram, 1:raw */
   int   im2w;			/*0:use MINTIME, 1:use preset value */
   struct STRETCH pws_stretch;
   int   unit;
   int   new_mask;		/* new mask needed */
   char  spare[358];		/*fills out the page*/
};

typedef struct display display_typ;
