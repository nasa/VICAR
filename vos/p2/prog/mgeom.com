$!****************************************************************************
$!
$! Build proc for MIPL module mgeom
$! VPACK Version 1.9, Monday, December 07, 2009, 16:44:07
$!
$! Execute by entering:		$ @mgeom
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
$ write sys$output "*** module mgeom ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mgeom.com file -- ", primary
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
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("mgeom.imake") .nes. ""
$   then
$      vimake mgeom
$      purge mgeom.bld
$   else
$      if F$SEARCH("mgeom.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mgeom
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mgeom.bld "STD"
$   else
$      @mgeom.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mgeom.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mgeom.com -mixed -
	-s mgeom.c -
	-i mgeom.imake -
	-p mgeom.pdf -
	-t tstmgeom.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mgeom.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mgeom.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mgeom

   To Create the build file give the command:

		$ vimake mgeom			(VMS)
   or
		% vimake mgeom			(Unix)


************************************************************************/


#define PROGRAM	mgeom

#define MODULE_LIST mgeom.c

#define MAIN_LANG_C
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create mgeom.pdf
process help=*
 !
 PARM INP     TYPE=STRING COUNT=1
 PARM OUT     TYPE=STRING COUNT=1
 PARM SIZE    TYPE=INTEGER COUNT=0:4 DEFAULT=(1,1,0,0)
 PARM SL      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM SS      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM NL      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NS      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM FORMAT  KEYWORD COUNT=0:1 VALID=HALF DEFAULT=--
 PARM INTERP  TYPE=KEYWORD COUNT=0:1 VALID=(NOIN,ZNOIN) DEFAULT=--
 PARM NAH TYPE=INTEGER,VALID=(1:200) default=1
 PARM NAV TYPE=INTEGER,VALID=(1:3000) default=1
 PARM HVARY KEYWORD COUNT=0:1 VALID=HVARY DEFAULT=--
 PARM TIEPOINT TYPE=REAL,COUNT=0:600 default=--
 PARM PARMS   TYPE=STRING  COUNT=0:1  DEFAULT=--
 !
 END-PROC
!
! HELP TEXT FOR MGEOM
.TITLE
MGEOM - Program to perform geometric transformations on images.
.HELP
PURPOSE
     MGEOM is a VICAR applications program which makes geometric
     changes in pictures.  It can be used for many purposes including
     correcting geometric distortion, increasing picture size, reducing 
     picture size, and rotating a picture.  The ported MGEOM uses a 
     different algorithm than the unported MGEOM and has different
     characteristics.  The following are the principal differences
     with respect to the programs LGEOM and GEOMA (note the difference
     with the claims of the unported MGEOM):

	1. MGEOM is NO LONGER as fast or faster.
	2. MGEOM and GEOMA are more accurate as of April 1996.
           LGEOM should have comparable accuracy soon after.
	3. Preparation of "sharp edges" for mosaicking appears to
           be available using ZNOIN (MGEOM) and NOIZ (LGEOM).
           It appears that LGEOM is better at this.
	5. No limits on size of input/output pictures in MGEOM.
	6. No limits on size of interpolation grid in MGEOM.
	7. MGEOM now has "bad cases" involving excessive rereading of files.

     The unported version of MGEOM had the following requirement:
	1. Maximum allowed rotation is 85 degrees.
     I do not believe this to be the case with the ported version.


CALL
     mgeom INPUT OUTPUT SIZE '(QUALIFIERS) TIEPOINT-GRID
  WHERE:
     INPUT          is the input data set.
     OUTPUT         is the output data set.
     SIZE           is a VICAR size field for the output file
     QUALIFIERS     consist of any of the following keywords:
          NOIN          no interpolation is to be done.
          ZNOIN         no interpolation for points with 0 DN.
     TIEPOINT-GRID      is a collection of parameters as follows:


     The TIEPOINT-GRID is a set of points describing the relation
of the output file to that of the input file using the keywords
NAH, NAV, and TIEPOINT.
   NAH=nah  NAV=nav
     tiepoint=(nl1,ns1,ol1,os1,nl2,ns2,ol2,os2,...
                   ...nlk,nsk,olk,osk)

where the grid is rectangular in the output image space, nah is
the number of grid cells horizontally (across the top in the sample
direction), nav is the number of grid cells vertically in the output
image space, the point pairs (nli,nsi,oli,osi) are line-sample
coordinates in the output (new) and input (old) spaces respectively.
the number of pairs k must equal (nah+1)*(nav+1).  The grid must be
perfectly rectangular in the output image space (the rows and
columns must be perfectly horizontal and vertical respectively).

The input image may either be byte or halfword data.  The data format is taken
from the VICAR label of the input file.  The output image has the same data 
format (byte or halfword) as the input image.  

The HELP for program GEOMA has additional details.

     For those used to the unported version, note that there used
to be an output file parameter, IDS.  I.e. you used to have to
specify an intermediate data set.  This is no longer the case
with MGEOM.  Also, for a non-uniform tie point grid, there used
to be an 'HVARY qualifier.  Non-uniform spacing of tie points is
no longer a special case, so the HVARY parameter is now obsolete.
However, for backwards compatability, you can still specify the
parameter; it will simply be ignored.

OPERATION

MGEOM uses a moderate amount of space in its calculation arrays.  The
program implements a recursive tiling scheme so that it only loads a
fraction of the input file into memory if the entire input file is too
large.  However, this tiling scheme should be transparent
to the user.

PERFORMANCE

Each cell of the tiepoint grid is a separate tile (or more than 1) in this 
algorithm with its own set of disk reads and writes.  For a typical tiepoint
grid, this may result in a large time (elapsed wall-clock time) spent on 
disk I/O.

For example, the following statistics were obtained on an Alpha (coda2) for
the case of a BYTE input image with a size of 1024 lines by 1024 samples and
a tiepoint grid with NAH=9=NAV (smaller than typical).

             CPU TIME     WALL-CLOCK (CONNECT) TIME
LGEOM          1.34s       2.89s
MGEOM          3.45s      22.75s

The number of disk reads and writes is proportional to NAH*NAV.  The defaults
from PICREG are NAH=20=NAV.
.PAGE
Restrictions
------------

Warning:  If "nl" or "ns" exceeds the grid corner tie points, the interpolation
algorithm may become unstable and cause the program to crash.  It is advised 
that the user limit the number of output lines and samples to the grid corner
tie point limits. 
.PAGE
Original Programmer: A. L. Zobrist, 6 Dec. 1979
Complete scratch rewrite: S.V. Hwan, 8 Nov. 1994
Current Cognizant Programmer: B. A. McGuffie
.LEVEL1
.VARI INP
Input file name
.VARI OUT
Output file name
.VARI SIZE
Standard VICAR Size Field
.VARI SL
Starting line for output
.VARI SS
Starting sample for output
.VARI NL
Number of lines for output
* See restrictions
.VARI NS
Number of samples for output
* See restrictions
.VARI INTERP
interpolation options
Valid: NOIN,ZNOIN
.VARI FORMAT
FORMAT is ignored.
.VARI NAH
number of grid
cells horizontally
.VARI NAV
number of grid
cells vertically
.VARI TIEPOINT
grid corner tiepoints in
rows NL1,NS1,OL1,OS1,...
.VARI PARMS
previously saved parameter dataset
.LEVEL2
.VARI INP
Input file name.  This parameter is input as:
     INP=innam
where "innam" is the input file name.
.VARI OUT
Output and intermediate file names. This parameter is input as:
     OUT=outnam
where:
"outnam" is the output file name, and

.VARI SIZE
The size field is specified with four arguments,
     SIZE=(a,b,c,d)
where:
a is the starting line number of the output picture.
b is the starting sample of the output picture.
c is the number of lines, and
d is the number of samples
For example, SIZE=(1,1,40,50)
would create an output picture of size 40 lines by 50 bytes.
The size field can be thought of as a window relative to the output
grid.  The first two values offset the window down and to the right
causing the features in the image to move up and to the left.
.VARI SL
SL can be used to specify the starting line of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SL - 1.)  The default for SL is 1.
.VARI SS
SS can be used to specify the starting sample of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SS - 1.)  The default for SS is 1.
.VARI NL
NL can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of lines for output.
* See restrictions for more information
.VARI NS
NS can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of bytes for output.
* See restrictions for more information
.VARI INTERP
This parameter has two valid keyword values: NOIN and ZNOIN.

NOIN means no interpolation.  The default method (used when neither keyword 
is specified) for computing the
DN values of the output picture is to use a bi-linear interpolation
on the four nearest neighbors in the input picture.  With NOIN, the
value of the nearest point is simply used.
For example, say a point in the output picture was determined
to have come from point (R,P) in the input picture.  Since R and P
are real values, we must somehow calculate a DN value for that
point.  Take IR and IP as the truncated values.  We then have
          VAL1                                 VAL2
           *                                    *
         (IR,IP)                              (IR,IP+1)
                     POINT
                       *
                     (R,P)
          VAL3                                 VAL4
           *                                    *
         (IR+1,IP)                           (IR+1,IP+1)
Here, POINT is the result of a bilinear interpolation using
VAL1, VAL2, VAL3, and VAL4.
If NOIN is specified, then POINT would be VAL1, the nearest
neighbor.

ZNOIN specifies that a four-point interpolation is done except
when one or more of the points used has a value equal to zero. 
In that case the nearest method is used.
This allows preparation of sharp edges (no interpolation rolloff)
for mosaicking.

.VARI FORMAT
The format is obtained from the input image label. 
.VARI NAH
the nah is number of grid cells horizontally, the number of tiepoints 
across is one larger (nah+1).
.VARI NAV
the nav is number of grid cells vertically, the number of tiepoints
vertically is one larger (nav+1).
.VARI TIEPOINT
There are four real numbers for each tiepoint , the first two are the
line-sample coordinate in the output, the second two are the line-sample
coordinate in the input which is mapped to the point in the output.  There must
be (nah+1)*(nav+1) tiepoints (quadruple)s aligned in a perfectly horizontal 
and vertical grid.
.VARI PARMS
A parameter data set containing the geom parameters.  This file should
have been written by a program which uses the XVP routines for writing
parameter data sets.  This is the most common means by which the parameters
NAH, NAV, and TIEPOINT are passed.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmgeom.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!   TEST SCRIPT FOR MGEOM
!
!
! Now generate BYTE input data set
! 
gen MGTEST 10 10 SINC=40 LINC=40
! 
! Verify existence of input file
list MGTEST
!
!  Try some copies.
!  Check case of grid bigger than image.
mgeom MGTEST MGTEST1 NAH=1 NAV=1+ 
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
            20.,1.,20.,1., 20.,20.,20.,20.)
difpic (MGTEST1 MGTEST)

!  Check case of grid smaller than image.
mgeom MGTEST MGTEST1 NAH=1 NAV=1+ 
   TIEPOINT=(1.,1.,1.,1.,   1.,2.,1.,2.,+
             2.,1.,2.,1.,   2.,2.,2.,2.)
difpic (MGTEST1 MGTEST)

! 
! Perform simple enlargement to 2X size
mgeom MGTEST MGENLARG NAH=1 NAV=1+ 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
                20.,1.,10.,1.,20.,20.,10.,10.)
! 
! Print it out
list MGENLARG
! 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
mgeom MGTEST MGROTAT NAH=1 NAV=1+ 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
!
! Print it out
list MGROTAT
!
! Perform test of size field handling 
mgeom MGTEST MGROTAT1 NAH=1 NAV=1+ 
   SIZE=(1,1,20,10)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

mgeom MGTEST MGROTAT2 NAH=1 NAV=1+ 
   SIZE=(1,11,20,1)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

mgeom MGTEST MGROTAT3 NAH=1 NAV=1+ 
   SIZE=(1,12,20,9)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Concatenate the three images.  
mss (MGROTAT1,MGROTAT2,MGROTAT3) MGROTATA 
difpic (MGROTATA,MGROTAT) 

!
! Perform the same operation, but without interpolation
mgeom MGTEST MGROTAT NAH=1 NAV=1+ 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)+
   INTERP=NOIN
! Print it out
list MGROTAT
!
! Perform the same operation, but without interpolation
mgeom MGTEST MGROTAT NAH=1 NAV=1+ 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)+
   INTERP=ZNOIN
! Print it out
list MGROTAT
! 
! Now generate BYTE input data set
! 
gen MGTEST2 1000 1000 SINC=1 LINC=1
! 
! Verify existence of input file
list MGTEST2 SIZE=(1,1,15,15)
! 
! DO LONG THIN CASE WITH 45 DEG ROTATION.
mgeom MGTEST2 MGENTHIN NAH=1 NAV=1+ 
   SIZE=(1,1,2,1000)+
   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
            2.,1.,1001.,2.,2.,1000.,2.,1001.)
!
! Print it out
list MGENTHIN 'NOEJECT
! 
gen a 1000 1000
!  Test bilinear interpolation for FR 87169
mgeom							+
inp=a					+
out=b					+
nl=900 ns=900						+
nav=9							+
nah=9							+
tiepoint=(						+
001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
001,800,001,800,001,900,001,900,100,001,100,001,+
100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
100,900,100,900,		+
200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
200,800,200,800,200,900,200,900,300,001,300,001,+
300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
300,900,300,900,		+
400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
400,800,400,800,400,900,400,900,500,001,500,001,+
500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
500,900,500,900,		+
600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
600,800,600,800,600,900,600,900,700,001,700,001,+
700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
700,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
700,900,700,900,		+
800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
800,800,800,800,800,900,800,900,900,001,900,001,+
900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
900,900,900,900)
list b (50,50,850,850) linc=100 sinc=100
!
! Now do simple tests for half
! Now generate HALF input data set
! 
gen MGTEST 10 10 SINC=40 LINC=40 'HALF
! 
! Verify existence of input file
list MGTEST
! 
!
!  Try some copies.
!  Check case of grid bigger than image.
mgeom MGTEST MGTEST1 NAH=1 NAV=1+ 
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
            20.,1.,20.,1., 20.,20.,20.,20.)
difpic (MGTEST1 MGTEST)

!  Check case of grid smaller than image.
mgeom MGTEST MGTEST1 NAH=1 NAV=1+ 
   TIEPOINT=(1.,1.,1.,1.,   1.,2.,1.,2.,+
             2.,1.,2.,1.,   2.,2.,2.,2.)
difpic (MGTEST1 MGTEST)

! Perform simple enlargement to 2X size
mgeom MGTEST MGENLARG NAH=1 NAV=1+ 
   SIZE=(1,1,20,20)   +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
                20.,1.,10.,1.,20.,20.,10.,10.)
! 
! Print it out
list MGENLARG
! 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
mgeom MGTEST MGROTAT NAH=1 NAV=1+ 
   SIZE=(1,1,20,20)  +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
!
! Print it out
list MGROTAT
! 
! Now generate HALF input data set
! 
gen MGTEST2 1000 1000 SINC=1 LINC=1 'HALF
! 
! Verify existence of input file
list MGTEST2 SIZE=(1,1,15,15)
! 
! DO LONG THIN CASE WITH 45 DEG ROTATION.
mgeom MGTEST2 MGENTHIN NAH=1 NAV=1+ 
   SIZE=(1,1,2,1000) +
   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
            2.,1.,1001.,2.,2.,1000.,2.,1001.)
!
! Print it out
list MGENTHIN 'NOEJECT
!
gen a 1000 1000 'half
!  Test bilinear interpolation for FR 87169
mgeom							+
inp=a					+
out=b					+
nl=900 ns=900						+
nav=9							+
nah=9							+
tiepoint=(						+
001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
001,800,001,800,001,900,001,900,100,001,100,001,+
100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
100,900,100,900,		+
200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
200,800,200,800,200,900,200,900,300,001,300,001,+
300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
300,900,300,900,		+
400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
400,800,400,800,400,900,400,900,500,001,500,001,+
500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
500,900,500,900,		+
600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
600,800,600,800,600,900,600,900,700,001,700,001,+
700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
700,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
700,900,700,900,		+
800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
800,800,800,800,800,900,800,900,900,001,900,001,+
900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
900,900,900,900)
list b (50,50,850,850) linc=100 sinc=100
end-proc
$ Return
$!#############################################################################
