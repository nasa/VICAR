/************************************************************************/
/*									*/
/*			mgeom.c						*/
/*									*/
/*			by Steve Hwan					*/
/*			11 October 1994					*/
/*									*/
/*	This program is based on MGEOM and imwarp.com, both written	*/
/* by Al Zobrist, now of the RAND corporation.  The purpose of this	*/
/* program is to take a tie pointed image and warp it.			*/
/*									*/
/*  REVISION HISTORY
       4-96   SP    Added doubleprecision for bilinear interpolation
                    as for GEOMA to improve agreement between platforms.
                    Corrected to use NINT for rounding to nearest integer.
                    Corrected handling of SIZE field and handling of case
                    where the four corners of the image are not the four corners
                    of the tiepoint grid.
       1-96   SP    Reset deltal_samp, deltas_samp after each zvwrit
                    in warp_tile.  Added NSAMPS to zvwrite calls.
  */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "errdefs.h"
#include "vicmain_c"


  /* Note the order of information in the tiepoint table is: */
  /* output line, output sample, input line, input sample */
/*  The following line can be changed when testing to aid in debugging.  */
#define IMWARP_DEBUG 0
#define OUT_L	0
#define OUT_S	1
#define IN_L	2
#define IN_S	3

	/*  NINT define taken from program BICUBIC:round to nearest int */
#define NINT(a) (short int) (a > 0 ? a + 0.5 : a - 0.5)

#define MAX_TILE_BYTES  (1000000)

/*****			GLOBAL VARIABLES			*****/
/* parameters from input parblock */
int NAH, NAV;		/* number of grid cells horiz and vert. */
float *TIEPOINT;			/* list of tie points */
int i_unit,o_unit;			/* input unit, output unit */
int sline, ssamp, nline, nsamp;		/* User specified size of output */
int in_line, in_samp;			/* size of primary input */
int interp_mode;

/* Variables that are used on several levels */
int curr_grid_ndx;
float line_in_start,samp_in_start;
float line_in_start2,samp_in_start2;
float line_out_start,samp_out_start;
double o2i_line_ratio, o2i_samp_ratio; 
double o2i_line_ratio2, o2i_samp_ratio2; 
double o2i_stage2_denom;
/*  The output tiepoints are arranged on horizontal and vertical lines.
    The following 2 arrays contain the 
    line coordinates of the horizontal lines (LINES[0] to LINES[NAV]) and the 
    sample coordinates of the vertical lines (SAMPS[0] to SAMPS[NAH])  */
float *LINES;				/* list of tie points */
float *SAMPS;				/* list of tie points */

/* Function prototypes */
void open_files();
void read_parblock_info();
void verify_grid_points();
void find_input();
void find_extremes();
void process_tile();
void warp_tile();


/*	This module uses the following global variables:		*/
/*		int curr_grid_ndx;					*/
/*		double o2i_samp_ratio, o2i_samp_ratio2; 			*/
/*		double o2i_samp_ratio, o2i_samp_ratio2; 			*/
/*		double o2i_stage2_denom;					*/
void main44(void)
{
  int hgrid,vgrid;
  int curr_line,curr_samp;		/* counters */
  int eline, esamp;			/* ending line and sample from SIZE */
  int vbeg,hbeg;			/* The indexes of the grid cell that
                                           contains the point (sline,ssamp). */
  int vend,hend;			/* The indexes of the grid cell that
                                           contains the point (eline,esamp). */
  float line_out_beg,samp_out_beg;      /* Ending line,samp of grid cell  */
  float line_out_end,samp_out_end;      /* Ending line,samp of grid cell  */

/*  ===============BEGINNING OF EXECUTABLE CODE=============================  */

  zifmessage("MGEOM version 29-Apr-96");
  read_parblock_info();
  verify_grid_points();
  open_files();

/*  The following section is designed to correct handling of SIZE field and 
    handling of case where the four corners of the image are not the four 
    corners of the tiepoint grid.  The code and comments are adapted from
    program GEOMZ.
C..STORE THE LINE AND SAMPLE NUMBERS FOR THE ROWS AND COLUMNS OF THE TIEPOINT
C..GRID (output grid).
  */

  LINES = (float *) malloc((NAV+1) * sizeof(float));
  SAMPS = (float *) malloc((NAH+1) * sizeof(float));
  for(vgrid=0; vgrid<NAV+1; vgrid++)
      LINES[vgrid] = TIEPOINT[4*vgrid*(NAH+1) + OUT_L];

  for(hgrid=0; hgrid<NAH+1; hgrid++) 
      SAMPS[hgrid] = TIEPOINT[4*hgrid + OUT_S];


  eline = sline+nline-1;  /*  end coordinates of SIZE field  */
  esamp = ssamp+nsamp-1;

/* 
C..FIND THE GRID CELLS THAT CONTAIN (sline,ssamp) and (eline,esamp).
C..USUALLY THIS WILL BE THE FIRST AND THE LAST, AND WE WILL END UP WITH
C..   vbeg=hbeg=0  and vend=NAV-1 AND hend=NAH-1.  The idea is to extrapolate
      or interpolate from the output tiepoint grid to the edges of the 
      SIZE field. (Theoretically, the window set by the SIZE field can be
      anywhere in relationship to the tiepoint grid.)
 */

  vbeg = 0;
  while( LINES[vbeg+1] <= sline  && vbeg < NAV-1 )
         ++vbeg;  /*  continue if there are more cells and sline not in here  */

  vend = NAV-1;
  while( LINES[vend] >= eline  && vend > 0 )
         --vend;

  hbeg = 0;
  while( SAMPS[hbeg+1] <= ssamp  && hbeg < NAH-1 )
         ++hbeg;

  hend = NAH-1;
  while( SAMPS[hend] >= esamp  && hend > 0 )
         --hend;


  /*	The tiling/subdivision scheme works as follows: */
  /*		- Break image into regions as defined by tiepoints and SIZE
                  field                                          	*/
  /*		- Send each tile to be processed			*/
  /*		- If the tile is too big, break it into 2 halves along	*/
  /*		  the biggest side					*/

  /* Do this for each cell, not each tiepoint */
  for(vgrid=vbeg; vgrid<=vend; vgrid++)
    for(hgrid=hbeg; hgrid<=hend; hgrid++) {
      /* Find the value for all parameters that apply for the entire tile */
      curr_grid_ndx= (vgrid* (NAH+1))+hgrid;
      /* Find starting corner of tile */
      line_out_start = TIEPOINT[curr_grid_ndx*4+OUT_L];
      samp_out_start = TIEPOINT[curr_grid_ndx*4+OUT_S];
      line_in_start = TIEPOINT[curr_grid_ndx*4+IN_L];
      samp_in_start = TIEPOINT[curr_grid_ndx*4+IN_S];
      line_in_start2 = TIEPOINT[(curr_grid_ndx+(NAH+1))*4+IN_L];
      samp_in_start2 = TIEPOINT[(curr_grid_ndx+(NAH+1))*4+IN_S];

      /* Note that all ratios are calculated with samples on the denom. */
      /* This is because they are to be used in the first stage of */
      /* bilinear interpolation. */
      o2i_line_ratio =  /* top line */
        (TIEPOINT[(curr_grid_ndx+1)*4+IN_L] - line_in_start)/
        (TIEPOINT[(curr_grid_ndx+1)*4+OUT_S] - samp_out_start);
      o2i_line_ratio2 = /* bottom line */
        (TIEPOINT[(curr_grid_ndx+(NAH+1)+1)*4+IN_L] - line_in_start2)/
        (TIEPOINT[(curr_grid_ndx+(NAH+1)+1)*4+OUT_S] - samp_out_start);
      o2i_samp_ratio =  /* top line */
        (TIEPOINT[(curr_grid_ndx+1)*4+IN_S] - samp_in_start)/
        (TIEPOINT[(curr_grid_ndx+1)*4+OUT_S] - samp_out_start);
      o2i_samp_ratio2 = /* bottom line */
        (TIEPOINT[(curr_grid_ndx+(NAH+1)+1)*4+IN_S] - samp_in_start2)/
        (TIEPOINT[(curr_grid_ndx+(NAH+1)+1)*4+OUT_S] - samp_out_start);

      o2i_stage2_denom =
         TIEPOINT[(curr_grid_ndx+NAH+1)*4+OUT_L] -line_out_start;
#if IMWARP_DEBUG >= 2
  printf("o2i_line_ratio =  %f\n", o2i_line_ratio);
  printf("o2i_samp_ratio =  %f\n", o2i_samp_ratio);
  printf("o2i_line_ratio2 =  %f\n", o2i_line_ratio2);
  printf("o2i_samp_ratio2 =  %f\n", o2i_samp_ratio2);
  printf("o2i_stage2_denom =  %f\n", o2i_stage2_denom);
#endif

      if (vgrid == vbeg)
          line_out_beg = sline;
      else
          line_out_beg = line_out_start;

      if (vgrid == vend)
          line_out_end = eline;
      else
          line_out_end = LINES[vgrid+1];

      if (hgrid == hbeg)
          samp_out_beg = ssamp;
      else
          samp_out_beg = samp_out_start;

      if (hgrid == hend)
          samp_out_end = esamp;
      else
          samp_out_end = SAMPS[hgrid+1];



      /* note that process_tile may recursively call itself with different */
      /* parameters in the first 4 positions, so I cannot get away with */
      /* just passing curr_grid_ndx without the TIEPOINTs. */
      process_tile( line_out_beg, samp_out_beg,
                    line_out_end,			/* last output line */
                    samp_out_end);			/* last output samp */
    }
  /* Clean up */
  free(TIEPOINT);
  free(LINES);
  free(SAMPS);
  zvclose(o_unit, NULL);
  zvclose(i_unit, NULL);
}

void read_parblock_info()
{
  int status,count,def;
  int num_tie_pts;

  /* get tie point information from parblock */
  status = zvp("NAH",&NAH,&count);
  status = zvp("NAV",&NAV,&count);
  num_tie_pts =  4 * (NAH+1) * (NAV+1);
  TIEPOINT = (float *) malloc(num_tie_pts * sizeof(float));
  status = zvparm("TIEPOINT",TIEPOINT,&count,&def,num_tie_pts,0);
  if (count != num_tie_pts) {
    zvmessage("Number of tiepoints does not match NAV/NAH","");
    zabend();
  }
  /* Get other info from parblock */
  interp_mode=0;
  /* Horizontal spacing may vary - note that this really isn't even */
  /* a special case and is more or less for backwards compatibility */
  /* with MGEOM. */
  if (zvptst("HVARY"))  interp_mode=3;
  /* No interpolation */
  if (zvptst("NOIN"))  interp_mode=2;
  /* No interpolation for points with 0 DN -huh? */
  if (zvptst("ZNOIN")) interp_mode=1;
  return;
}

void verify_grid_points()
{
  int vgrid,hgrid;

  /* Verify that all of the grid points line up perfectly */
  /*	The reason for this requirement is that we want to be able */
  /* to uniquely determine a location for a point in output space.  */
  /* If the points are not straight, we may have an ambiguity. */
  /* Note the order of information in the tiepoint table is: */
  /* output line, output sample, input line, input sample */

  for (vgrid=1; vgrid<=NAV; vgrid++) {
    for (hgrid=1; hgrid<=NAH; hgrid++) {
      /* row check (lines in an output row must be the same) */
      if (TIEPOINT[(vgrid* (NAH+1)* 4) + (hgrid* 4) + OUT_L] !=
          TIEPOINT[(vgrid* (NAH+1)* 4) + (0    * 4) + OUT_L] ) {
        zvmessage("Invalid tiepoint.  Output line not straight.","");
        zabend();
      }
      /* column check (samples in an output column must be the same) */
      if (TIEPOINT[(vgrid* (NAH+1)* 4) + (hgrid* 4) + OUT_S] !=
          TIEPOINT[(0    * (NAH+1)* 4) + (hgrid* 4) + OUT_S] ) {
        zvmessage("Invalid tiepoint.  Output line not straight.","");
        zabend();
      }
    }
  }

  /* Old version(MGEOM) used to require even spacing between rows of */
  /* tiepoint and in columns of tiepoints.  Now, it is not really a */
  /* concern since having those constant really doesn't save us much. */
  return;
}


/************************************************************************/
/*									*/
/*		open_files()						*/
/*	This routine opens the input and output image files		*/
/*									*/
/************************************************************************/
void open_files()
{
  int status;
  char fmt_str[10];

  /***********************/
  /* open the input file */
  /***********************/
  status = zvunit( &i_unit, "INP", 1, NULL);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
  /* should have checked status to make sure file openned properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str, NULL);
  if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF")) {
    zvmessage("Invalid input data format.  Use BYTE or HALF.","");
    zabend();
  }
  /***************************/
  /* open up the output file */
  /***************************/
  zvsize( &sline, &ssamp, &nline, &nsamp, &in_line, &in_samp);
  status=zvunit( &o_unit, "OUT", 1, NULL);
  /* note that zvopen is intelligent enough to default to the same */
  /* format as the input file.  */
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp, "U_FORMAT","HALF",
		"OP", "WRITE",
		"OPEN_ACT", "SA",
		"IO_ACT", "SA", NULL);
  return;
}


/************************************************************************/
/*									*/
/*	this routine finds the input point which corresponds to a	*/
/* given output point.							*/
/*	This module uses the following global variables:		*/
/*		float o2i_samp_ratio, o2i_samp_ratio2; 			*/
/*		float o2i_samp_ratio, o2i_samp_ratio2; 			*/
/*		float o2i_stage2_denom;					*/
/*		float line_out_start,samp_out_start;			*/
/*									*/
/************************************************************************/
void find_input(out_line,out_samp,in_line,in_samp)
float out_line,out_samp;
float *in_line,*in_samp;
{
  float stage2_frac;
  float int_part1l,int_part2l;
  float int_part1s,int_part2s;

  stage2_frac = (out_line - line_out_start)/o2i_stage2_denom;
  int_part1l = line_in_start  + (out_samp- samp_out_start) * o2i_line_ratio;
  int_part2l = line_in_start2 + (out_samp- samp_out_start) * o2i_line_ratio2;
  *in_line= int_part1l* (1-stage2_frac) + int_part2l* stage2_frac;
  int_part1s = samp_in_start  + (out_samp- samp_out_start) * o2i_samp_ratio;
  int_part2s = samp_in_start2 + (out_samp- samp_out_start) * o2i_samp_ratio2;
  *in_samp= int_part1s* (1-stage2_frac) + int_part2s* stage2_frac;

#if IMWARP_DEBUG >= 2
  printf("stage2_frac = %lf\n", stage2_frac);
  printf("Top line interpolation: Actual out samp: %15.3lf\n",out_samp);
  printf("Starting sample: %15.3lf LINE: %15.3lf SAMP: %15.3lf\n",
	samp_out_start, int_part1l,int_part1s);
  printf("Starting sample: %15.3lf LINE: %15.3lf SAMP: %15.3lf\n",
	samp_out_start, int_part2l,int_part2s);
  printf("Resulting input point: LINE: %15.3lf SAMP: %15.3lf\n",
	*in_line,*in_samp);
#endif
  return;
}


/* Note CHECK_POINT is only used in find_extremes */
/* CHECK_POINT is just a macro that will compute the input point */
/* corresponding to a given output point. */
#define CHECK_POINT(test_line,test_samp) \
  find_input(test_line,test_samp,&temp_line_in,&temp_samp_in);	\
  if (temp_line_in < *min_line_in) *min_line_in = temp_line_in; \
  if (temp_line_in > *max_line_in) *max_line_in = temp_line_in; \
  if (temp_samp_in < *min_samp_in) *min_samp_in = temp_samp_in; \
  if (temp_samp_in > *max_samp_in) *max_samp_in = temp_samp_in;

/************************************************************************/
/*									*/
/*	This routine finds the extemes in the input corresponding to	*/
/* the four output corners of a given cell subdivision.  It is really	*/
/* on the brute force side, but i couldn't think of a faster more	*/
/* elagant way to accomplish this, since the output points are not	*/
/* necessarily tie points.						*/
/*	This module uses the following global variables:		*/
/*		int curr_grid_ndx;					*/
/*		float o2i_samp_ratio, o2i_samp_ratio2; 			*/
/*		float o2i_samp_ratio, o2i_samp_ratio2; 			*/
/*		float o2i_stage2_denom;					*/
/*									*/
/************************************************************************/
void find_extremes(min_line_in,max_line_in,min_samp_in,max_samp_in,
			line_out1,samp_out1,
			line_out2,samp_out2)
float *min_line_in,*max_line_in,*min_samp_in,*max_samp_in;
float line_out1,samp_out1;
float line_out2,samp_out2;
{
  float temp_line_in,temp_samp_in;

  /* initialize all extremes to the first point */
  find_input(line_out1,samp_out1,max_line_in,max_samp_in);
  *min_line_in = *max_line_in;
  *min_samp_in = *max_samp_in;

  CHECK_POINT(line_out1,samp_out2)
  CHECK_POINT(line_out2,samp_out1)
  CHECK_POINT(line_out2,samp_out2)

#if IMWARP_DEBUG >= 2
  printf("find_extremes called.  Resulting ranges:\n");
  printf("LINE: %15.3lf to %15.3lf\n",*min_line_in, *max_line_in);
  printf("SAMP: %15.3lf to %15.3lf\n",*min_samp_in, *max_samp_in);
#endif
  return;

}
#undef CHECK_POINT

/************************************************************************/
/*									*/
/*	process_tile is where the recursive subdivision routines	*/
/* happen.								*/
/*									*/
/************************************************************************/
void process_tile(sl_out, ss_out, ll_out, ls_out)
float sl_out, ss_out, ll_out, ls_out;
{
  int tile_size;
  int num_line_tli, num_samp_tli;
  float min_line_tli,max_line_tli,min_samp_tli,max_samp_tli; /* inputs */
  float min_line_cut1,max_line_cut1,min_samp_cut1,max_samp_cut1;
  float min_line_cut2,max_line_cut2,min_samp_cut2,max_samp_cut2;
  int big_side;		/* 0=lines, 1=samps */

#if IMWARP_DEBUG >= 1
  printf("process_tile called with parameters:\n");
  printf("Line range: %7f to %7f  Samp range %7f to %7f\n",
		sl_out, ll_out, ss_out, ls_out);
#endif
  find_extremes(&min_line_tli,&max_line_tli,&min_samp_tli,&max_samp_tli,
			sl_out,ss_out,   ll_out,ls_out);
  num_line_tli = (int)
	 (ceil((double)max_line_tli) - floor((double)min_line_tli)) + 1;
  num_samp_tli = (int)
	 (ceil((double)max_samp_tli) - floor((double)min_samp_tli)) + 1;
  tile_size= num_line_tli* num_samp_tli;
  tile_size *= (int) sizeof(short int);
#if IMWARP_DEBUG >= 1
  printf("Number of lines: %d  Number of samples: %d\n",
			num_line_tli, num_samp_tli);
  printf("Tile size: %d bytes\n",tile_size);
#endif
  if (tile_size>MAX_TILE_BYTES) {
    /* tile is too big - subdivide the tile and make a recursive call */
    if (num_line_tli<num_samp_tli)
      big_side=1;
    else
      big_side=0;
    find_extremes(&min_line_cut1,&max_line_cut1,&min_samp_cut1,&max_samp_cut1,
			sl_out,ss_out,
			ll_out,(float) floor((double)(ss_out+ls_out)/2.0));
    find_extremes(&min_line_cut2,&max_line_cut2,&min_samp_cut2,&max_samp_cut2,
			sl_out,ss_out,
			(float) floor((double) (sl_out+ll_out)/2.0) ,ls_out);
    if ( ((big_side==0) &&
	      (max_line_cut1-min_line_cut1) < (max_line_cut2-min_line_cut2) )
	|| ((big_side==1) &&
	      (max_samp_cut1-min_samp_cut1) < (max_samp_cut2-min_samp_cut2) ))
	{
          process_tile(sl_out, ss_out,
			ll_out,(float) floor((double)(ss_out+ls_out)/2.0));
          process_tile( sl_out,(float) floor((double)(ss_out+ls_out)/2.0)+1.0,
 			ll_out, ls_out);
	} else {
          process_tile(sl_out, ss_out,
			(float) floor((double) (sl_out+ll_out)/2.0) ,ls_out);
          process_tile( (float) floor((double) (sl_out+ll_out)/2.0)+1.0 ,ss_out,
			ll_out, ls_out);
        }
	return;
  }
  warp_tile( sl_out, ss_out, ll_out, ls_out);
  return;
}


/************************************************************************/
/*									*/
/*		warp_tile performs the actual warp.  There are 3 	*/
/* varieties within this after the initial parameters are calculated.	*/
/* The 3 modes vary on the amount of interpolation is done.		*/
/*									*/
/************************************************************************/
void warp_tile( sl_out, ss_out, ll_out, ls_out)
float sl_out, ss_out, ll_out, ls_out;
{
  float min_line_tli,max_line_tli,min_samp_tli,max_samp_tli; /* inputs */
  int num_line_tli, num_samp_tli;
  short int *tile,*tmp_tile;
  int tile_size,tile_ndx;

  float I00l, I00s;
  double deltal_line,deltas_line;
  double deltal_samp,deltas_samp;
  double delta2l, delta2s;
#if IMWARP_DEBUG >= 3
  float loff,soff;
#endif

  double first_s,first_l;
  int lastl_int, lasts_int;

  double curr_iline,curr_isamp;
  int up_left_ndx;
  float pix_value;
  short int dn0,dn1,dn2,dn3;	/* used for no interp for 0 DN */
  double frac_line, frac_samp;
  short int *output_line,*curr_o_pixel;

  int status,curr_line,curr_samp,samp_per_line,osamp_per_line;

  int infile_start_l, infile_end_l, infile_start_s, infile_end_s;
  int infile_num_l, infile_num_s;
  int offset_l, offset_s;

#if IMWARP_DEBUG >= 2
  printf("warp_tile called with parameters:\n");
  printf("Line range: %7f to %7f  Samp range %7f to %7f\n",
		sl_out, ll_out, ss_out, ls_out);
#endif
  /* Find which infile area corresponds to this output tile.		*/
  find_extremes(&min_line_tli,&max_line_tli,&min_samp_tli,&max_samp_tli,
			sl_out,ss_out,ll_out,ls_out);
  min_line_tli = (float) floor((double)min_line_tli);
  max_line_tli = (float) ceil((double)max_line_tli);
  min_samp_tli = (float) floor((double)min_samp_tli);
  max_samp_tli = (float) ceil((double)max_samp_tli);
  num_line_tli = (int) (max_line_tli - min_line_tli) + 1;
  num_samp_tli = (int) (max_samp_tli - min_samp_tli) + 1;

  /* Allocate space for tile+1pixel border on right and bottom */
  /* and read it into memory */
  tile_size=(num_line_tli+1) * (num_samp_tli+1);
  tile = (short int *) malloc (tile_size * sizeof(short int));
  for (tile_ndx=0; tile_ndx< tile_size; tile_ndx++)
    tile[tile_ndx]=0;

  if ((min_line_tli>(float)in_line) || (max_line_tli<1.0)
        || (min_samp_tli>(float)in_samp) || (max_samp_tli<1.0))
    ; /* skip the tile reading */
  else { /*** read the tile from the input file ***/
    if (min_line_tli<1.0) {
      infile_start_l = 1;
      offset_l = (int)(1.0 - min_line_tli);
    } else {
      infile_start_l = (int) min_line_tli;
      offset_l = 0;
    }
    if (max_line_tli>(float)in_line)
      infile_end_l = in_line;
    else
      infile_end_l = max_line_tli;
    if (min_samp_tli<1.0) {
      infile_start_s = 1;
      offset_s = (int)(1.0 - min_samp_tli);
    } else {
      infile_start_s = (int) min_samp_tli;
      offset_s = 0;
    }
    if (max_samp_tli>(float)in_samp)
      infile_end_s = in_samp;
    else
      infile_end_s = max_samp_tli;
    infile_num_l = infile_end_l - infile_start_l + 1;
    infile_num_s = infile_end_s - infile_start_s + 1;
  
    tmp_tile = &tile[offset_l* (num_samp_tli+1)];
    status = zvread(i_unit,&tmp_tile[offset_s],
  		"LINE", infile_start_l,
  		"SAMP", infile_start_s, "NSAMPS", infile_num_s, NULL);
    tmp_tile[num_samp_tli] = tmp_tile[num_samp_tli-1];
    tmp_tile += (num_samp_tli+1);
    for(curr_line=1; curr_line<infile_num_l; curr_line++)
    {
      status = zvread(i_unit,&tmp_tile[offset_s],
  		"SAMP", infile_start_s, "NSAMPS", infile_num_s, NULL);
      tmp_tile[num_samp_tli] = tmp_tile[num_samp_tli-1];
      tmp_tile += (num_samp_tli+1);
    }
    /* copy last line, so we don't have do a special case check during	*/
    /* the interpolation.						*/
    tmp_tile = &(tile[(num_line_tli-1)*(num_samp_tli+1)]);
    for(curr_samp=0; curr_samp<=num_samp_tli; curr_samp++,tmp_tile++) {
      tmp_tile[(num_samp_tli+1)] = *tmp_tile;
    }
  }
#if IMWARP_DEBUG >=3
  printf("Input tile:\n");
  for(curr_line=0; curr_line<= num_line_tli; curr_line++) {
    for(curr_samp=0; curr_samp <= num_samp_tli; curr_samp++)
      printf("%4d",tile[curr_line* (num_samp_tli+1) +curr_samp]);
    printf("\n");
  }
#endif

  /* calculate the input point corresponding to the first output point */
  find_input(sl_out,ss_out,&I00l,&I00s);

  /* calculate line,sample input displacement per output line increment */
  deltal_line = (TIEPOINT[(curr_grid_ndx+(NAH+1))*4+IN_L] -line_in_start) /
                  o2i_stage2_denom;
  deltas_line = (TIEPOINT[(curr_grid_ndx+(NAH+1))*4+IN_S] -samp_in_start) /
                  o2i_stage2_denom;
  /* calculate line,sample input displacement per output sample increment */
  deltal_samp = o2i_line_ratio;
  deltas_samp = o2i_samp_ratio;
  delta2l = (o2i_line_ratio2 - o2i_line_ratio) / o2i_stage2_denom;
  delta2s = (o2i_samp_ratio2 - o2i_samp_ratio) / o2i_stage2_denom;
#if IMWARP_DEBUG >= 2
  printf("deltal_line =  %f\n", deltal_line);
  printf("deltal_samp =  %f\n", deltal_samp);
  printf("deltas_line =  %f\n", deltas_line);
  printf("deltas_samp =  %f\n", deltas_samp);
  printf("delta2l =  %f\n", delta2l);
  printf("delta2s =  %f\n", delta2s);
#endif

  first_l = I00l;
  first_s = I00s;
  /* Offset to beginning of buffer */
  first_l -= floor((double)min_line_tli);
  first_s -= floor((double)min_samp_tli);

#if IMWARP_DEBUG >= 3
  loff = I00l - first_l;
  soff = I00s - first_s;
#endif

  samp_per_line = (int) num_samp_tli + 1;
  lastl_int = (int) ll_out;
  lasts_int = (int) ls_out;
  osamp_per_line = lasts_int - (int) ss_out + 1;
  output_line= (short int *) malloc(osamp_per_line * sizeof(short int));


  /* At first glance, this may look really stupid to replicate a code */
  /* segment 4 times then it's really the same thing.  I mean, since */
  /* the inside part is the only part that's different, wouldn't it */
  /* make more sense to put the switch on the inside?  NO!  NO!  A */
  /* THOUSAND TIMES NO!  WHY would anyone want the execution of this */
  /* program have to make a switch for every single point in the loop */
  /* when it could get away with doing it just once?  Though it is */
  /* less aestetically pleasing to see the code duplicated like this, */
  /* It should run a bit faster. */
  /* However, it may be possible in the future to put some of the */
  /* replicated coding in #define statements. */
  switch (interp_mode) {
    case 0: /* normal interpolation */
    case 3: /* HVAR - really isn't a special case */
      for(curr_line=(int) sl_out; curr_line <= lastl_int; curr_line++) {
        curr_iline=first_l;
        curr_isamp=first_s;
        curr_o_pixel=output_line;
#if IMWARP_DEBUG >= 3
        printf("===================== Output line %5d =====================\n",
		curr_line);
#endif
        for (curr_samp=(int) ss_out; curr_samp <= lasts_int; curr_samp++) {
    		/* process image */
          frac_line=curr_iline - floor((double)curr_iline);
          frac_samp=curr_isamp - floor((double)curr_isamp);
          up_left_ndx= ((int)curr_iline)* samp_per_line +(int)curr_isamp;
#if IMWARP_DEBUG >= 3
          printf("Current input LINE: %5f SAMP: %5f  (index: %5d) DN: %d\n",
	   curr_iline+loff, curr_isamp+soff, up_left_ndx, tile[up_left_ndx]);
#endif
          pix_value = 
            (1.0-frac_line) * ((1.0-frac_samp) * ((double)tile[up_left_ndx]) +
		frac_samp * ((double)tile[up_left_ndx+1])) +
            frac_line* ((1.0-frac_samp)* ((double)tile[up_left_ndx+samp_per_line])+
		frac_samp * ((double)tile[up_left_ndx+samp_per_line+1]));
          *curr_o_pixel =  NINT(pix_value);
          curr_o_pixel++;
          curr_iline += deltal_samp;
          curr_isamp += deltas_samp;
        }
#if IMWARP_DEBUG >=3
  for(curr_samp=0; curr_samp <= (int)(ls_out-ss_out); curr_samp++)
    printf("%4d",output_line[curr_samp]);
  printf("\n");
#endif
        zvwrit(o_unit,output_line,"LINE",curr_line - sline+1,
               "SAMP",(int) ss_out -ssamp+1,"NSAMPS", osamp_per_line, NULL);
        first_l += deltal_line;
        first_s += deltas_line;
        deltal_samp += delta2l;
        deltas_samp += delta2s;
      }
      break;


    case 1: /* No interpolation if any corner is 0 DN */
      for(curr_line=(int) sl_out; curr_line <= lastl_int; curr_line++) {
        curr_iline=first_l;
        curr_isamp=first_s;
        curr_o_pixel=output_line;
        for (curr_samp=(int) ss_out; curr_samp <= lasts_int; curr_samp++) {
    		/* process image */
          up_left_ndx= ((int)curr_iline)*samp_per_line +(int)curr_isamp;
          dn0=tile[up_left_ndx];
          dn1=tile[up_left_ndx+1];
          dn2=tile[up_left_ndx+samp_per_line];
          dn3=tile[up_left_ndx+samp_per_line+1];
          if ((dn0==0) || (dn1==0) || (dn2==0) || (dn3==0)) {
            /* No interpolation */
            *curr_o_pixel = tile[((int)(curr_iline+0.5))*samp_per_line
					+(int)(curr_isamp+0.5)];
          } else {
            /* Interpolate */
            frac_line=curr_iline - floor((double)curr_iline);
            frac_samp=curr_isamp - floor((double)curr_isamp);
            pix_value = 
              (1-frac_line) * ((1-frac_samp) * (double)(dn0) +
  		frac_samp * (double) (dn1)) +
              frac_line* ((1-frac_samp)* (double)(dn2)+
  		frac_samp * (double)(dn3));
            *curr_o_pixel = NINT(pix_value);
          }
          curr_o_pixel++;
          curr_iline += deltal_samp;
          curr_isamp += deltas_samp;
        }
        zvwrit(o_unit,output_line,"LINE",curr_line - sline+1,
               "SAMP",(int) ss_out -ssamp+1,"NSAMPS", osamp_per_line, NULL);
        first_l += deltal_line;
        first_s += deltas_line;
        deltal_samp += delta2l;
        deltas_samp += delta2s;
      }
      break;


    case 2: /* No interpolation */
      for(curr_line=(int) sl_out; curr_line <= lastl_int; curr_line++) {
        curr_iline=first_l;
        curr_isamp=first_s;
        curr_o_pixel=output_line;
        for (curr_samp=(int) ss_out; curr_samp <= lasts_int; curr_samp++) {
    		/* process image */
          *curr_o_pixel = tile[((int)(curr_iline+0.5))*samp_per_line
					+(int)(curr_isamp+0.5)];
          curr_o_pixel++;
          curr_iline += deltal_samp;
          curr_isamp += deltas_samp;
        }
        zvwrit(o_unit,output_line,"LINE",curr_line - sline+1,
               "SAMP",(int) ss_out -ssamp+1,"NSAMPS", osamp_per_line, NULL);
        first_l += deltal_line;
        first_s += deltas_line;
        deltal_samp += delta2l;
        deltas_samp += delta2s;
      }
      break;

    default: /* error - invalid mode */
      zvmessage("Invalid interpolation mode chosen.","");
      zabend();
      break;
  }
  free((char *) output_line);
  free((char *) tile);
  return;
}
