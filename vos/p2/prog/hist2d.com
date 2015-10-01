$!****************************************************************************
$!
$! Build proc for MIPL module hist2d
$! VPACK Version 1.9, Monday, December 07, 2009, 16:27:16
$!
$! Execute by entering:		$ @hist2d
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
$ write sys$output "*** module hist2d ***"
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
$ write sys$output "Invalid argument given to hist2d.com file -- ", primary
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
$   if F$SEARCH("hist2d.imake") .nes. ""
$   then
$      vimake hist2d
$      purge hist2d.bld
$   else
$      if F$SEARCH("hist2d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hist2d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hist2d.bld "STD"
$   else
$      @hist2d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hist2d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hist2d.com -mixed -
	-s hist2d.c -
	-p hist2d.pdf -
	-t tsthist2d.pdf nims_spicedefs -
	-i hist2d.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hist2d.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************

  PROGRAM HIST2D                    

  HIST2D will generate a two-dimensional histogram file from a VICAR
  file of three dimensions. The two-dimensional histogram file has as 
  its Y-axis (lines) the range of DN values of the input file or such 
  values scaled for radiance or reflectance, I/F, and as its X-axis 
  (samples) the input's band numbers.  Thus each line of the histogram 
  file represents the frequency of a data number (DN) through all the 
  bands of the input file.  A sample slice through all the lines of the 
  histogram file represents a one-dimensional histogram for a band of 
  the input file.  The frequencies of each band's histogram are 
  represented by the pixels or data numbers (DNs) of the file, 
  corresponding to the number of occurrences of DN, I/F or radiance 
  values in the input file.

  Cognizant Programmer: L.W.Kamp
  Original Author:      Justin McNeill
                             
  Revisions:      

  26may03 -lwk-  increased MAXBANDS from 408 to 1024 (one of the test cases
		has 512 -- failed in unix, but not vms!);  made NIMS_SPEC_R
		a parameter (R_MIN);  renamed "end[3]" to "xend[3]" to avoid
		conflict with system variable

  19jun98 -lwk-  set RAD_BASE/CONV when not in cube label

  27may98 -lwk-  write BREAK_NO to label for IOF-only case too

  14may98 -lwk-  put in check & abend for empty cube (instead of letting
		pgm crash)

  17feb98 -lwk-  fixed bug in floating-point scale for RAD mode

  26jan98 -lwk-  fixed floating-point scale yet again

  Oct-1997 -lwk- corrected error computing histogram scale for floating-point 
	(Radiance) data

  Jul-1997 -lwk- allow floating-point (Radiance) data

  Aug-1996 -lwk- fixes for Phase-2 data:  allowed for non-integer sample
	replication factor

  Nov.1995  fixed retrieval of wavelengths from label  -lwk-

  Jun.1995  Tweaked LOG scale for sparse histograms to allow points with
	    freq=1 to appear -lwk-

  Apr.1995  Disable LOG option when maxfreq=1 (for TSTSYSNIMS2) -lwk-

  Feb.1995  Added BREAK_UM option (LWK)

  Jan.1995  Ported to Alpha - lwk -

  May.1994  Fixed bug when radiance > maxs[35] (LWK)

  Feb.1994  Get solar flux from label instead of internal routine;
      flux units changed to uWatt/cm**2/micron/ster;
      added keyword SCALTYPE  (LWK)

  Nov.1993  Fixed default lower LIMITS of -32752 for RAD/IOF case;
      PCA stuff removed (LWK)
  
  May 1992  Corrected upper limit of NIMS special values to 
      -32752.
  
  Feb 1992  Program will handle merged mosaic cubes with or
      without per-band scaling. Interface with NIMSMASK
      (MAXDN) corrected. Test option added for
      development purposes. Histogram calculation for
      byte and halfword simplified. PCA added. (JFM)
                       
  Sept 1991  Changed NELEMENTS to NELEMENT in XLGET per new Vicar 
      exec; removed SUN_D check from RAD case. (LWK)       
                       
  June 1991  Per band radiance offset and scaling implemented.    

  Oct  1990     RAD_BASE offset to convert DN to IOF or RAD   
      added in calculations. Test script revised.       
                       
  Sept 1990     Code added to prevent division by zero. FR 63252     

  Aug  1990   Window label item revised to be (SL,SS,SB,NL,NS,NB). 
         DN and LOG parameters added to help and tutor.       
      Samplewidth problem corrected and negative halfword  
      data properly handled.  Ref. FRs 63221 & 63222       
      Improved error checking of user parameters.          
      Revised test script.             
      Solar flux units changed to WATT/CM**CM/MICRON/STER  
                       
  July 1990  Handling of IOF and RAD y-axis scales is provided.   
                             
  June 1990  Addition of output size parameters OUT_NL & OUT_NS;  
      write output file such that band and DN dimensions   
      have a common origin; provide parameter to provide   
      for replication of band samples to fit in 512        
      samples.                 
                             
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "nims_label2.h"
#include "vicmain_c"
#include <stdlib.h>

    /* GLOBAL CONSTANTS */
#define FALSE 0
#define TRUE 1
#define NIMS_SPECIAL -32752	/* Upper limits of NIMS SPECIAL VALS */
#define IEEE_VALID_MIN_4 0xFF7FFFFA	/* see nimscmm2.c */
#define MAXSAMPLE 512		/* Default sample dimension          */
#define MAXLINE 512		/* Default line dimension       */
#define MINLINES 5		/* Minimum number of lines in 2dhist */
#define MAXLINES 20000		/* Maximum number of lines in 2dhist */
#define MAXSAMPS 20000		/* Maximum number of samples in 2d H */
#define MAXBANDS 1024		/* Maximum number of bands       */

          /* GLOBAL VARIABLES         */
int bytes,		/* Number of bytes in input file */
  dimensions[3],	/* Dimensions of input file */
  xend[3],		/* Ending line, samp and band */
  inc[3],		/* SINC, LINC, and BINC parameters */
  inunit,		/* Input file unit number */
  limits[2],		/* Limits on DN displayed */
  nb,			/* Number of bands in input file */
  outunit,		/* Histogram file unit number */
  out_nl,		/* Number of output lines */
  out_ns,		/* Number of output samples */
  replication,		/* Replication of band samples parm  */
  scalemode,		/* Mode determining scale: DN,IOF,RAD,mixed */
  dtyp,
  nbrk,
  vstat,		/* Status number of VICAR cmd. */
  wstat,
  window[6];		/* SL, SS, SB, NL, NS and NB parms */
char cal_type[8],	/* CAL TYPE: NOCAL, IOF, RAD  */
  h2dscal[5],		/* scale type written to label:  DN/IOF/RAD/COMB */
  format[5],		/* Output data format parameter */
  infile[40],		/* File name buffer */
  outfile[40],		/* File name buffer */
  strng[80];		/* String buffer */
float s_flux[MAXBANDS],	/* solar flux array */
  wave[MAXBANDS],	/* array of wavelengths in cube */
  wbrk,			/* wavelength of break between RAD & IOF */
  maxy1,		/* Maximum value of Y axis */
  maxy2,		/* Maximum value of 2nd Y axis in COMB mode */
  R_MIN,
  radiance[MAXBANDS],	/* Radiance multiplier array */
  rad_base[MAXBANDS];	/* Radiance offset array */

void main44(void)       /* MAIN PROGRAM           */
{
  zvmessage(" *** Program HIST2D version 26-May-03 ***","");
  process_parms();	/* Process all user parameters       */
  gen_histogram();	/* Create 2D histogram         */
  write_labelitems();	/* Write necessary label items       */
}
/******************************************************************************

Purpose: Process all user parameters and get input and output file unit numbers
                       */
process_parms()
{
int count, i, j;
char fmt[6];

memset(infile,0,40);      /* Determine input file unit number  */
zvp("INP",infile,&count);
zvunit(&inunit,"INP",1, NULL);
  
memset(outfile,0,40);      /* Determine output file unit number */
zvp("OUT",outfile,&count);
zvunit(&outunit,"OUT",1, NULL);

zvopen(inunit,"OPEN_ACT","SA","IO_ACT","SA", NULL);
zvget(inunit,"NL",&dimensions[0],"NS",&dimensions[1],"NB",&dimensions[2],
  "FORMAT",fmt,"PIX_SIZE",&bytes, NULL);

if (bytes>=4 && strcmp(fmt,"REAL")) zmabend(" only BYTE/HALF/REAL formats supported");

if(dimensions[2]==1)
  zmabend("*** INPUT FILE IS NOT 3-DIMENSIONAL ***");

if (bytes==1) {
  zvclose(inunit, NULL);
  zvopen(inunit,"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
}

if (bytes==4) {
  zvmessage(" data format is real, radiances assumed","");
  zvp("RLIMIT",&R_MIN,&count);
  if (!count) R_MIN = IEEE_VALID_MIN_4;
}

nb = dimensions[2]; 

/* check if user specified a data mode: */
zvparm( "BREAK_UM", &wbrk, &i, &j, 1, 0);
dtyp = -1;
if (zvptst("DN")) dtyp = 0;
if (zvptst("RAD")) dtyp = 1;
if (zvptst("IOF")) dtyp = 2;
if (wbrk>0.) {
  if (zvptst("TEST")) zmabend(" TEST option not available for COMB mode");
  dtyp = 3;
}

zvp("LIMITS",limits,&count);    /* Get limits of DN values       */
if( count==0 ) {
  limits[0] = 0;
  if (bytes == 1)       /* If limits of DN are default       */
    limits[1] = 255;      /* to (0,255) for 8 bit data        */
  else if (bytes == 2) {
    limits[1] = 32767;      /* (0,32767) for 16 bit DN's.        */
    if (dtyp>0) 
      limits[0] = NIMS_SPECIAL;    /* but (-32752,32767) for RAD/IOF    */
  }
}
else {
  if( bytes==1 ) {
    if( limits[0]<0 || limits[0]>255 ) {
      zvmessage("***  Invalid lower limit for LIMITS, set to 0 ***","");
      limits[0] = 0;
    }
    if( limits[1]<0 || limits[1]>255 ) {
      zvmessage("*** Invalid upper limit for LIMITS, set to 255 ***","");
      limits[1] = 255;
    }
  }
  else if (bytes == 2) {
    if( limits[0]<-32768 || limits[0]>32767 ) {
      zvmessage("*** Invalid lower limit for LIMITS, set to -32768 ***","");
      limits[0] = -32768;
    }
    if( limits[1]<-32768 || limits[1]>32767 ) {
      zvmessage("*** Invalid upper limit for LIMITS, set to 32767 ***","");
      limits[1] = 32767;
    }
  }
}

zvp("FORMAT",format,&count);
zvp("SINC",&inc[1],&count);
zvp("LINC",&inc[0],&count);
zvp("BINC",&inc[2],&count);

zvp("SL",&window[0],&count);  /* Check starting line of window parm   */
if( window[0] > dimensions[0] || window[0] < 1 ) {
  zvmessage("*** Invalid starting line, SL set to 1 ***","");
  window[0] = 1;
}

zvp("SS",&window[1],&count);  /* Check starting sample of window parm */
if( window[1] > dimensions[1] || window[1] < 1 ) {
  zvmessage("*** Invalid starting sample, SS set to 1 ***","");
  window[1] = 1;
}

zvp("SB",&window[2],&count);  /* Check starting band of window parm   */
if( window[2] > nb || window[2] < 1 ) {
  zvmessage("*** Invalid starting band, SB set to 1 ***","");
  window[2] = 1;
}

zvp("NL",&window[3],&count);  /* Check number of lines for window parm   */
if( count != 0 ) {
  if( window[3]+window[0] > dimensions[0] || window[3] < 1 ) {
    window[3] = dimensions[0] - window[0] + 1;
    sprintf(strng,"*** Invalid number of lines, NL set to %04d. ***",
     window[3]);
    zvmessage(strng,"");
  }
}
else
  window[3] = dimensions[0] - window[0] + 1;

zvp("NS",&window[4],&count);  /* Check number of samples for window parm */
if( count != 0 ) {
  if( window[4]+window[1] > dimensions[1] || window[4] < 1 ) {
    window[4] = dimensions[1] - window[1] + 1;
    sprintf(strng,"*** Invalid number of samples, NS set to %04d. ***",
     window[4]);
    zvmessage(strng,"");
  }
}
else
  window[4] = dimensions[1] - window[1] + 1;

zvp("NB",&window[5],&count);  /* Check number of bands for window parm  */
if( count != 0 ) {
  if( window[2]+window[5] > nb || window[5] < 1 ) {
    window[5] = dimensions[2] - window[2] + 1;
    sprintf(strng,"*** Invalid number of bands, NB set to %04d. ***",
     window[5]);
    zvmessage(strng,"");
  }
}
else
  window[5] = dimensions[2] - window[2] + 1;

zvp("OUT_NL",&out_nl,&count);  /* Check output file number of lines   */
if( out_nl < MINLINES || out_nl > MAXLINES ) {
  zvmessage("*** OUT_NL out of range, set to 512 ***","");
  out_nl = 512;
}  
zvp("OUT_NS",&out_ns,&count);  /* Check output file number of samples  */
if( out_ns < window[5] || out_ns > MAXSAMPS ) {
  zvmessage("*** OUT_NS out of range, set to 512 ***","");
  out_ns = 512;
}

for( count=0; count<3; count++ )  /* Calculate ending line, samp & band */
  xend[count] = window[count+3] + window[count] - 1;    

}
/*******************************************************************************

Purpose: Generate a histogram image of dimension  out_nl x out_ns.
                        */
gen_histogram()
{
float binsize,			/* Histogram bin size          */
  DNmax,			/* Maximum DN of histogram image      */
  lsat,hsat,			/* High and low saturation percent    */
  mean,				/* Mean of histogram          */
  sigma,			/* Standard deviation of histogram    */
  scale1, scale2,		/* scale factors to Hist range        */
  radi,				/* radiance */
  xval,
  repfact,			/* float replication factor of samples*/
  indxscale,			/* Histogram indx scale if REAL*4 */
  **value;			/* Pointer to histogram point magn's  */
char tasks[20][10];		/* Task names            */
int  above_out_nl,		/* Number of DN's saturated high end  */
  below_zero,			/* Number of DN's saturate low end    */
  bandnumber,			/* Current band number          */
  base_values, conv_values,	/* Number of radiance scaling values  */
  bin,				/* Current bin number           */
  histsamp,			/* Histogram output sample value      */
  idn,				/* Convenient maximum DN value        */
  indx,				/* indx of temporary histogram       */
  indxoffset,			/* Histogram indx offset        */
  indxsize,			/* Histogram indx size in bins        */
  instances[20],		/* Instances of history labels        */
  integer,			/* dummy variable          */
  maxdnband,			/* Band at which maximum DN occurs    */
  maxdn,			/* Maximum DN value of one band       */
  mindn,			/* Minimum DN value of one band        */
  maxfreq,			/* Maximum frequency of all histogrms */
  maxfreqband,			/* Band number where max. freq occurs */
  nhist,			/* Number of history labels         */
  numtasks,			/* Number of tasks in VICAL labels    */
  numpoints,			/* Number of points in band image     */
  np,				/* Number of points in voxel        */
  samplewidth,			/* Replication factor of samples      */
  band,				/* band loop variable          */
  line,				/* Current line number of loop        */
  samp,				/* Current sample number of loop      */
  stat1,stat2,stat3,		/* Additional status flag        */
  nwav, 
  ii,i,j,x,y,z;			/* Loop variable */
double mf;			/* Maximum frequency */
short int *DN;			/* DN array */
float *RDN;			/* Real*4 DN array */
unsigned int *hist;		/* Halfword histogram array */
unsigned int *temphist;		/* Histogram buffer */
int save;	/* TEMPORARY KLUDGE */

/* Search for RAD_CONV, RAB_BASE label items and if IOF mode, MIN(MAX)SUN_D
 * label items */

numtasks = 20;
vstat = zlhinfo(inunit,(char *)tasks,instances,&numtasks,"ULEN",10,
						"ERR_ACT","", NULL);

vstat = stat1 = stat2 = stat3 = wstat = 0;

for( x=(numtasks-1); x>=0; x-- ) {
  if( vstat != 1 ) {
    vstat = zlget( inunit, "HISTORY", RAD_CONV, radiance, "HIST", tasks[x],
     "FORMAT", "REAL", "NELEMENT", -1, "NRET", &conv_values, NULL);

    /* If there is a single value in RAD_CONV label item, replicate 
       that value for the number of bands in the cube. */
    if( conv_values == 1 && vstat == 1 )
      for(y=0; y<nb; y++)
        radiance[y] = radiance[0];
  }
  if( stat1 != 1 ) {
    stat1 = zlget( inunit, "HISTORY", RAD_BASE, rad_base, "HIST", tasks[x],
     "FORMAT", "REAL", "NELEMENT", -1, "NRET", &base_values, NULL);

    /* If there is a single value in RAD_BASE label item, replicate 
       that value for the number of bands in the cube. */
    if( base_values == 1 && stat1 == 1 )
      for(y=0; y<nb; y++)
        rad_base[y] = rad_base[0];
  }
  if( stat2 != 1 )
    stat2 = zlget( inunit, "HISTORY", CAL_TYPE, cal_type, "HIST", tasks[x],
     "FORMAT", "STRING", NULL);
  if( stat3 != 1 )
    stat3 = zlget( inunit, "HISTORY", SOLAR_FLUX, s_flux, "HIST", tasks[x],
     "NELEMENT", -1, NULL);
  if (wstat!=1)
    wstat = zlget( inunit, "HISTORY", WAVELENGTHS, wave, "NELEMENT", -1,
     "NRET", &nwav, "HIST", tasks[x], NULL);
}

/* VISIS2 inverse mode doesn't write RAD_CONV/BASE for radiance cubes, so
 * set them to defaults: */
if (vstat == -38 || !conv_values ) {
  for(y=0; y<nb; y++) {
    radiance[y] = 1.0;
    rad_base[y] = 0.0;
  }
}

if (bytes==4) {
  cal_type[0] = 'R';
  if (stat3!=1 && dtyp>1) {
    zvmessage("*** Solar fluxes not found, IOF option not done ***","");
    dtyp = 1;
  }
}
else {
  if (((vstat!=1 || stat1!=1 || stat2!=1) && dtyp>0) || (stat3!=1 && dtyp>1)) {
    cal_type[0] = 'N';
    zvmessage("*** Label incomplete, DN scale assumed for Y-axis ***","");
  }
}

if (cal_type[0] == 'R') {
  if (dtyp==0)
    scalemode = 1;		/* DN scale and RAD type   */
  else {
    /* NIMS SPECIAL VALUE */
    if(limits[0]<NIMS_SPECIAL)
      limits[0] = NIMS_SPECIAL;
    if (dtyp == -1) {		/* not specified by user */
      scalemode = 2;		/* RAD scale and RAD type  */
      zvmessage(" *** RAD scale assumed","");
    }
    else
      scalemode = dtyp+1;
  }
}
else {
  scalemode = 1;		/* DN scale and NOCAL type  */
  if (dtyp>0)
    zvmessage("*** Label says NOCAL type, Radiance/IOF cancelled ***","");
}

if (scalemode == 3) nbrk = nb;
else if (scalemode == 4) {
  if (wstat != 1 || nwav != nb) {
    zvmessage(" could not find all wavelengths in label, approximating NBREAK",
     "");
    nbrk = ((float)nb/4.5)*(wbrk-0.7)+0.5; 
    /* (not a very good approx'n, but it's the one used by SPECPLOT) */
  }
  else {
    for (i=0; i<nb; i++)
      if (wave[i]>wbrk) break;
    nbrk = i;
    if (nbrk<2) {
      zvmessage(" Break wavelength < minimum, setting to RAD mode","");
      scalemode = 2;
    }
    else if (nbrk>nb-1) {
      zvmessage(" Break wavelength > maximum, setting to IOF mode","");
      scalemode = 3;
    }
  }
}

DNmax = 32767.0;      
if(format[0]=='B' || format[0]=='b') DNmax = 255.0;

indxsize = 65536;
indxoffset = 32768;
if (bytes==1) {
  indxsize = 256;
  indxoffset = 0;
}

/* Allocate memory to read in image line  */
if (bytes==4) {
  RDN = (float *)calloc(dimensions[1],sizeof(float));
  if(RDN == NULL) zmabend("*** Insufficient memory for image line read ***");
}
else {
  DN = (short int *)calloc(dimensions[1],sizeof(short int));
  if(DN == NULL) zmabend("*** Insufficient memory for image line read ***");
}

/* Allocate memory to store two dimensional histogram   */
value = (float **)calloc(out_nl,sizeof(float *));  
if( value == NULL ) {
  sprintf( strng, " out_nl = %d", out_nl);
  zvmessage( strng, "");
  zmabend("*** Insufficient memory for histogram calculation ***");
}

for(x=0;x<out_nl;x++) {
  value[x] = (float *)calloc(out_ns,sizeof(float));
  if( value[x] == NULL ) {
    sprintf( strng, " x = %d, out_ns = %d", out_ns);
    zvmessage( strng, "");
    zmabend("*** Insufficient memory for histogram calculation ***");
  }
}

maxfreq = 0;      /* Initialize variables   */
maxdnband = 0;    
maxfreqband = 0;    
samplewidth = 1;
if( zvptst("REPLICATE") ) repfact = ((float)out_ns / (float)window[5]);       

/* Determine Y_axis scale value maxima and the band number of the maxima */
if (bytes==4) {
  get_maximum_Y_axis_R(&maxdnband,&indxscale,&indxoffset,RDN);
  sprintf( strng, " indxscale = %f, indxoffset = %d", indxscale, indxoffset);
  zvmessage( strng, "");
  scale1 = out_nl / (maxy1 * indxscale);
}
else {
  get_maximum_Y_axis(&maxdnband,DN);  
  scale1 = out_nl / maxy1;
}
if (scalemode==4) {
  scale2 = out_nl / maxy2;
  if (bytes==4) scale2 /= indxscale;
}

bandnumber = 0;        
          /* Allocate memory for hist buffers */
hist = (unsigned int *)calloc(indxsize,sizeof(unsigned int));
if( hist == NULL ) 
  zmabend("*** Insufficient memory for HIST array allocation ***");
     
temphist = (unsigned int *)calloc(out_nl,sizeof(unsigned int));
if( temphist == NULL ) 
  zmabend("*** Insufficient memory for TEMPHIST array allocation ***");
     
/* Calculate raw histogram band by band      */
for(band=window[2];band<=xend[2];band+=inc[2],bandnumber+=samplewidth) {
  numpoints = 0;
  above_out_nl = below_zero = 0;
  for(line=window[0];line<=xend[0];line+=inc[0]) {
    if (bytes<4) {
      zvread( inunit, DN, "LINE", line, "BAND", band, "NSAMPS", window[4],
       "SAMP", window[1], NULL);
      for(samp=0;samp<window[4];samp+=inc[1]) {
        if( DN[samp] >= limits[0] && DN[samp] <= limits[1] ) {
          hist[DN[samp]+indxoffset]++;
          numpoints++;
        }
      }
    }
    else {
      zvread( inunit, RDN, "LINE", line, "BAND", band, "NSAMPS", window[4],
       "SAMP", window[1], NULL);
      for(samp=0;samp<window[4];samp+=inc[1]) {
        if (RDN[samp] >= R_MIN) {
	  ii = RDN[samp]*indxscale + indxoffset;
          hist[ii]++;
          numpoints++;
        }
      }
    }
  }
  
  if( band==maxdnband && zvptst("TEST") )
    gen_test_option_output(indxsize,indxoffset,hist,numpoints);

  switch( scalemode ) {
  
  case 1:	/* DN */
    for( samp=limits[0]; samp<=limits[1]; samp++ )
      if( hist[samp+indxoffset] > 0 ) {
        indx = (samp+0.1)*scale1;
        if( indx < 0 )
          below_zero += hist[samp+indxoffset];
        else if( indx > out_nl )
	  above_out_nl += hist[samp+indxoffset];
	else
	  temphist[indx] += hist[samp+indxoffset];
      }
    break;

  case 2:	/* RAD */
    for( samp=limits[0]; samp<=limits[1]; samp++ )
      if( hist[samp+indxoffset] > 0 ) {
	indx = ((samp+0.1)*radiance[band-1]+rad_base[band-1]) * scale1;
	if( indx < 0 )
          below_zero += hist[samp+indxoffset];
	else if( indx > out_nl )
	  above_out_nl += hist[samp+indxoffset];
	else
	  temphist[indx] += hist[samp+indxoffset];
      }
    break;

  case 3:	/* IOF */
    for( samp=limits[0]; samp<=limits[1]; samp++ )
      if( hist[samp+indxoffset] > 0 ) {
	indx = (((samp+0.1)*radiance[band-1]+rad_base[band-1])/s_flux[band-1])
	 * scale1;
	if( indx < 0 )
          below_zero += hist[samp+indxoffset];
	else if( indx > out_nl )
	  above_out_nl += hist[samp+indxoffset];
	else
	  temphist[indx] += hist[samp+indxoffset];
      }
    break;

  case 4:	/* COMB IOF/RAD */
    for (samp=limits[0]; samp<=limits[1]; samp++)
      if (hist[samp+indxoffset] > 0) {
	radi = (samp+0.1)*radiance[band-1]+rad_base[band-1];
	indx = radi * scale2;
	if (band-1<nbrk) {
	  radi /= s_flux[band-1];
	  indx = radi * scale1;
	}
	if( indx < 0 )
          below_zero += hist[samp+indxoffset];
	else if( indx > out_nl )
	  above_out_nl += hist[samp+indxoffset];
	else
	  temphist[indx] += hist[samp+indxoffset];
      }
    break;
  }

  if( zvptst("EXCLUDE") )
    temphist[0] = 0;			/* Exclude values out of rng.*/
  else {
    temphist[0] += below_zero;		/* Append values below zero  */
    temphist[out_nl-1] += above_out_nl;	/* and above OUT_NL to the   */
  }					/* extremes of the histogram */

  for( samp=0; samp<out_nl; samp++ )	/* Determine maximum freq.   */
    if( temphist[samp] > maxfreq ) {
      maxfreq = temphist[samp];
      maxfreqband = band;
    }

  if( zvptst("REPLICATE") ) {		/* Check for replication keyword      */
    samplewidth = repfact*band - bandnumber;	/* (band counts from 1) */
    for(x=(out_nl-1);x>=0;x--)
      for(i=0; i<samplewidth; i++)
        value[x][bandnumber+i] = (float)temphist[x];
  }
  else
    for(x=(out_nl-1);x>=0;x--)
      value[x][bandnumber]=(float)temphist[x];

	/* Clear histogram buffers */
  memset(hist,0,4*indxsize);
  memset(temphist,0,4*out_nl);
}

free(hist);		/* Deallocate memory for halfword buffer        */
free(temphist);		/* Deallocate memory for temporary histogram    */

zvclose(inunit, NULL);	/* Close input file       */

zvselpi(0);  /* Disable the copying of VICAR label items from input file  */
zvopen( outunit, "OP", "WRITE", "O_FORMAT", format, "U_FORMAT", "REAL",
 "OPEN_ACT", "SA", "IO_ACT", "SA", "U_NL", out_nl, "U_NS", out_ns, "U_NB", 1, NULL);

/* For test option, print band at which the maximum dn occurs, the scale of 
   the histogram's y-axis and the band at which the maximum frequency occurs */

if( zvptst( "TEST" ) ) {      
  zvmessage("********************************************","");
  zvmessage(" ","");
  sprintf(strng,"   MAXIMUM DN OCCURS AT BAND # %d",maxdnband);
  zvmessage(strng,"");
  zvmessage(" ","");
  sprintf(strng,"   SCALE OF Y-AXIS = %f pixels/DN",scale1);
  zvmessage(strng,"");  
  zvmessage(" ","");
  sprintf(strng,"   MAXIMUM FREQUENCY OCCURS AT BAND # %d", maxfreqband);
  zvmessage(strng,"");  
  zvmessage(" ","");
  zvmessage("*******************************************","");
}

if( zvptst("LOG") && (maxfreq>1) ) {	/* Perform log base 10 on histogram  */
  mf = maxfreq;
  scale1 = 0.0;
  /* add 1 to allow sparse histograms to show 1-entry vlaues */
  if( mf != 0.0 ) scale1 = DNmax / log10( 1.0+mf );
  for( bin = (out_nl-1); bin >= 0; bin-- ) {
    for( histsamp = 0; histsamp < out_ns; histsamp++ )
      value[bin][histsamp] = scale1 * log10( 1.0+value[bin][histsamp]);
    zvwrit(outunit,value[bin], NULL);		/* Write line to output file */
  }
}
else {
  for(bin=(out_nl-1);bin>=0;bin--) {		/* Normalize Histogram magn. */
    for(histsamp=0;histsamp<out_ns;histsamp++) {
      if (maxfreq<=0)
	value[bin][histsamp] = 0.;
      else
        value[bin][histsamp] = (int)((value[bin][histsamp]*(DNmax/maxfreq))+0.5);
    }
    zvwrit(outunit,value[bin], NULL);		/* Write line to output file */
  }
}
for(x=0;x<out_nl;x++)        /* Free nested allocated mem.*/
  free( value[x] );      
free( value );          
}          


/******************************************************************************

Purpose: Write necessary values to VICAR label.  
                             */
write_labelitems()
{
  float maxys[2];

  zladd( outunit, "HISTORY", "INP_FILE", infile, "HIST", "HIST2D",
   "FORMAT", "STRING", NULL);
  zladd( outunit, "HISTORY", "LIMITS", limits, "NELEMENT", 2, "HIST", "HIST2D",
   "FORMAT", "INT", NULL);
  zladd( outunit, "HISTORY", "INC_LSB", inc, "NELEMENT", 3, "HIST", "HIST2D",
   "FORMAT", "INT", NULL);
  zladd( outunit, "HISTORY", "WINDOW", window, "NELEMENT", 6, "HIST", "HIST2D",
   "FORMAT", "INT", NULL);
  zladd( outunit, "HISTORY", "SCALTYPE", h2dscal, "HIST", "HIST2D", "FORMAT",
   "STRING", NULL);
  maxys[0] = maxy1;
  maxys[1] = maxy2;
  zladd( outunit, "HISTORY", "MAXDN", maxys, "HIST", "HIST2D", "FORMAT",
   "REAL", "NELEMENT", 2, NULL);
  if (scalemode >= 3) {
    zladd( outunit, "HISTORY", "BREAK_NO", &nbrk, "HIST", "HIST2D", "FORMAT",
     "INT", "NELEMENT", 1, NULL);
  }
  zvclose(outunit, NULL);      /* Close histogram file      */
}


/******************************************************************************

Purpose: Determine DN, IOF or RAD scaling factors
                       */
get_maximum_Y_axis(maxband,DN)
int  *maxband;  /* Band number of maximum DN, IOF or RAD     */  
short int DN[];    /* Image line            */
{
  int   i,
  band,        /* band loop variable          */
  line,        /* Current line number of loop        */
  samp;        /* Current sample number of loop      */
  float idn, yval;
  float maxs[36]={
    .0001,.0002,.0004,.0005,
    .001,.002,.004,.005,
    .01,.02,.04,.05,
    .1,.2,.4,.5,1.,2.,4.,5.,
    10.,20.,40.,50.,
    100.,200.,400.,500.,
    1000.,2000.,4000.,5000.,
    10000.,20000.,40000.,50000.};
  float max1, max2;

  max1 = max2 = 0.0;  /* Initialize max values to zero */

  switch( scalemode )  {

  case 1:    /* DN mode   */

    strcpy( h2dscal, "DN");
    for(band=window[2];band<=xend[2];band+=inc[2]) {
      for(line=window[0];line<=xend[0];line+=inc[0]) {
      zvread( inunit, DN, "LINE", line, "BAND", band, "NSAMPS", window[4],
       "SAMP", window[1], NULL);
      for(samp=0;samp<window[4];samp+=inc[1])
        if( DN[samp] > max1 ) {
          max1 = DN[samp];
          *maxband = band;
        }
      }  
    }
    idn = 256;
    i = 0;
    while ( max1 > idn )
      idn *= 2;
    maxy1 = idn;

    break;

  case 2:    /* RAD mode   */

    strcpy( h2dscal, "RAD");
    for(band=window[2];band<=xend[2];band+=inc[2]) {
      for(line=window[0];line<=xend[0];line+=inc[0]) {
        zvread( inunit, DN, "LINE", line, "BAND", band, "NSAMPS", window[4],
         "SAMP", window[1], NULL);
        for(samp=0;samp<window[4];samp+=inc[1]) {
          if((DN[samp]*radiance[band-1]+rad_base[band-1]) > max1) {
            max1 = DN[samp]*radiance[band-1]+rad_base[band-1];
            *maxband = band;
          }
        }
      }
    }
    i = 0;
    while ( max1 > maxs[i] && i < 35 )
      i++;
    maxy1 = maxs[i];

    break;
  
  case 3:    /* IOF mode   */

    strcpy( h2dscal, "IOF");
    for(band=window[2];band<=xend[2];band+=inc[2]) {
      for(line=window[0];line<=xend[0];line+=inc[0]) {
        zvread( inunit, DN, "LINE", line, "BAND", band, "NSAMPS", window[4],
         "SAMP", window[1], NULL);
        for(samp=0;samp<window[4];samp+=inc[1]) {
          if((DN[samp]*radiance[band-1]+rad_base[band-1])/s_flux[band-1]>max1) {
	    max1 = (DN[samp]*radiance[band-1]+rad_base[band-1]) / s_flux[band-1];
            *maxband = band;
	  }
	}
      }
    }
/*    if (max1 > 1.0)  REMOVE THIS LIMIT!!
      max1 = 1.0;			/* Physical upper limit of IOF */
    i = 0;
    while ( max1 > maxs[i] && i < 35 )
      i++;
    maxy1 = maxs[i];

    break;      

  case 4:    /* COMB (RAD/IOF) mode   */

    strcpy( h2dscal, "COMB");
    for(band=window[2];band<=xend[2];band+=inc[2]) {
      for(line=window[0];line<=xend[0];line+=inc[0]) {
        zvread( inunit, DN, "LINE", line, "BAND", band, "NSAMPS", window[4],
         "SAMP", window[1], NULL);
        for(samp=0;samp<window[4];samp+=inc[1]) {
	  yval = DN[samp]*radiance[band-1]+rad_base[band-1];
	  if (band-1<nbrk) {
	    yval /= s_flux[band-1];
            if (yval>max1) max1 = yval;
	  }
	  else
            if (yval>max2) max2 = yval;
	}
      }
    }
/*    if (max1 > 1.0)  REMOVE THIS LIMIT!!
      max1 = 1.0;			/* Physical upper limit of IOF */
    i = 0;
    while (max1 > maxs[i] && i < 35 )
      i++;
    maxy1 = maxs[i];
    i = 0;
    while (max2 > maxs[i] && i < 35 )
      i++;
    maxy2 = maxs[i];

    break;      
  }
  if (max1<=0 && max2<=0) zmabend(" *** no data in cube! ***");
}


/******************************************************************************

Purpose: Determine DN, IOF or RAD scaling factors for Real*4 data
                       */
get_maximum_Y_axis_R(maxband,indxscale,indxoffset,RDN)
int  *maxband;  /* Band number of maximum DN, IOF or RAD     */  
float *indxscale;
int *indxoffset;
float RDN[];    /* Image line            */
{
  int   i,
  band,        /* band loop variable          */
  line,        /* Current line number of loop        */
  samp;        /* Current sample number of loop      */
  float yval;
  float maxs[36]={
    .0001,.0002,.0004,.0005,
    .001,.002,.004,.005,
    .01,.02,.04,.05,
    .1,.2,.4,.5,1.,2.,4.,5.,
    10.,20.,40.,50.,
    100.,200.,400.,500.,
    1000.,2000.,4000.,5000.,
    10000.,20000.,40000.,50000.};
  float max0, max1, max2, min1;

  max0 = max1 = max2 = 0.0;  /* Initialize max values to zero */
  min1 = 1.e38;  /* Initialize min values to large */

  /* first determine limits[] and indxscale/offset for base histogram */

  for(band=window[2];band<=xend[2];band+=inc[2]) {
    for(line=window[0];line<=xend[0];line+=inc[0]) {
      zvread( inunit, RDN, "LINE", line, "BAND", band, "NSAMPS", window[4],
       "SAMP", window[1], NULL);
      for(samp=0;samp<window[4];samp+=inc[1]) {
        if (RDN[samp] > max1) {
          max1 = RDN[samp];
          *maxband = band;
        }
        if (RDN[samp]>=R_MIN && RDN[samp]<min1) min1 = RDN[samp];
      }
    }
  }
  if (max1<=1.e-10 && min1>=0.9e38) zmabend(" *** no data in cube! ***");
  *indxscale = 65535./(max1-min1);
  limits[0] = min1*(*indxscale);
  limits[1] = max1*(*indxscale);
  *indxoffset = -min1*(*indxscale);

  /* then find maxy for each case */

  switch( scalemode )  {

  case 1:    /* DN mode (assumed = RAD if REAL*4) */
  case 2:    /* RAD mode   */

    strcpy( h2dscal, "RAD");
    i = 0;
    while ( max1 > maxs[i] && i < 35 )
      i++;
    maxy1 = maxs[i];
    break;
  
  case 3:    /* IOF mode   */

    max1 = max2 = 0.0;
    strcpy( h2dscal, "IOF");
    for(band=window[2];band<=xend[2];band+=inc[2]) {
      for(line=window[0];line<=xend[0];line+=inc[0]) {
        zvread( inunit, RDN, "LINE", line, "BAND", band, "NSAMPS", window[4],
         "SAMP", window[1], NULL);
        for(samp=0;samp<window[4];samp+=inc[1]) {
          if (RDN[samp]/s_flux[band-1] > max1) {
	    max1 = RDN[samp]/ s_flux[band-1];
            *maxband = band;
	  }
          if (RDN[samp]>=R_MIN && RDN[samp]/s_flux[band-1] < min1)
	   min1 = RDN[samp]/s_flux[band-1];
	}
      }
    }
/*    if ( max1 > 1.0 )   REMOVE THIS LIMIT!!
      max1 = 1.0;			/* Physical upper limit of IOF */
    i = 0;
    while ( max1 > maxs[i] && i < 35 )
      i++;
    maxy1 = maxs[i];

    break;      

  case 4:    /* COMB (RAD/IOF) mode   */

    max1 = max2 = 0.0;
    strcpy( h2dscal, "COMB");
    for(band=window[2];band<=xend[2];band+=inc[2]) {
      for(line=window[0];line<=xend[0];line+=inc[0]) {
        zvread( inunit, RDN, "LINE", line, "BAND", band, "NSAMPS", window[4],
         "SAMP", window[1], NULL);
        for(samp=0;samp<window[4];samp+=inc[1]) {
	  yval = RDN[samp];
	  if (yval>=R_MIN) {
	    if (yval>max0) max0 = yval;
	    if (yval<min1) min1 = yval;
	    if (band-1<nbrk) {
	      yval /= s_flux[band-1];
              if (yval>max1) max1 = yval;
	    }
	    else {
              if (yval>max2) max2 = yval;
	    }
	  }
	}
      }
    }
/*    if (max1 > 1.0)  REMOVE THIS LIMIT!!
      max1 = 1.0;			/* Physical upper limit of IOF */
    i = 0;
    while (max1 > maxs[i] && i < 35 )
      i++;
    maxy1 = maxs[i];
    i = 0;
    while (max2 > maxs[i] && i < 35 )
      i++;
    maxy2 = maxs[i];

    break;      
  }
}


/******************************************************************************

Purpose: Output raw histogram for comparison to HISTGEN output.
                     */
gen_test_option_output(indxsize,indxoffset,hist,numpoints)
int     indxsize,indxoffset;
unsigned int  hist[];
int    numpoints;
{
float    mean,sigma;
int    below_zero,
    dummy,mindn,maxdn,
    line,sample,
    numwords,
    nso,nlo,
    tunit,
    x,y,z,
    *buffer,
    *ln;
char     testfile[40];

below_zero = 0;    /* Set histogram count for DN's below 0 to 0   */

for( sample=(-1*indxoffset); sample<0; sample++ )
  below_zero += hist[sample+indxoffset];

hist[indxoffset] += below_zero;

if(indxsize==256)  /* Get mean and standard deviation of histgram  */
  {
  zhistat(hist,numpoints,&mean,&sigma,&dummy);
  numwords = 256 + 4;
  }
else
  {
  zhistat2(hist,numpoints,&mean,&sigma,&mindn,&maxdn,&dummy);
  numwords = 32768 + 4;
  }

buffer = (int *)calloc(numwords,sizeof(int));  /* Allocate memory      */
if( buffer == NULL ) 
  zmabend("*** Insufficient memory for BUFFER array allocation ***");

if ( numwords < 4000 )  /* Calculate number of lines and samples output */
  nso = numwords;
else
  nso = 4000;
nlo = (numwords-1)/nso + 1;

ln = (int *)calloc(nso,sizeof(int));
if ( ln==NULL ) 
  zmabend("*** Insufficient memory for LN array allocation ***");

memset(testfile,0,40);		/* Determine output file unit number   */
zvp("TOUT",testfile,&x);
zvunit(&tunit,"TOUT",1,"U_NAME",testfile, NULL);

	/* Open test file  */
zvopen( tunit, "OP", "WRITE",  "OPEN_ACT", "SA", "IO_ACT", "SA", "U_FORMAT",
 "FULL", "O_FORMAT", "FULL", "U_NL", nlo, "U_NS", nso, "U_NB", 1, NULL);

buffer[0] = numwords - 4;      /* Load number of words */
buffer[1] = (int)( 1000.0 * mean + 0.5 );  /* in histogram, mean & */
buffer[2] = (int)( 1000.0 * sigma + 0.5 );  /* standard deviation.  */

for(x=0; x<buffer[0]; x++)    /* Copy histogram to output buffer */
  buffer[3+x] = (int)hist[indxoffset+x];

for( line=1; line<=nlo; line++ ) {	/* Write histogram out to file */
					/* line by line.             */
  sample = (line-1)*nso + 1;
  if( line==nlo )
    nso = numwords-sample+1;
  for(x=0; x<nso; x++)
    ln[x] = buffer[(sample-1) + x];
  vstat = zvwrit(tunit, ln, "LINE", line, "SAMP", 1, "NSAMPS", nso, NULL);
}

zvclose(tunit, NULL);			/* Close test output file      */
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create hist2d.pdf
process help=*
PARM INP 	STRING  COUNT=1
PARM OUT	STRING  COUNT=1
PARM TOUT	STRING  DEFAULT="HIST.TST"
PARM LIMITS     INTEGER DEFAULT=-- COUNT=0:2
PARM RLIMIT     REAL    DEFAULT=-- COUNT=0:1
PARM SINC	INTEGER DEFAULT=1
PARM LINC	INTEGER DEFAULT=1
PARM BINC	INTEGER DEFAULT=1
PARM EXCLUDE	KEYWORD VALID=(EXCLUDE,NOEXCLUDE) DEFAULT=NOEXCLUDE
PARM FORMAT	KEYWORD VALID=(BYTE,HALF) DEFAULT=BYTE
PARM SL		INTEGER DEFAULT=1 
PARM SS      	INTEGER DEFAULT=1 
PARM SB      	INTEGER DEFAULT=1 
PARM NL      	INTEGER DEFAULT=-- count=0:1
PARM NS      	INTEGER DEFAULT=-- count=0:1
PARM NB      	INTEGER DEFAULT=-- count=0:1
PARM OUT_NS 	INTEGER DEFAULT=512
PARM OUT_NL 	INTEGER DEFAULT=512
PARM REPLIC     KEYWORD VALID=(REPLICATE,NOREP) DEFAULT=NOREP
PARM DN		KEYWORD VALID=(DN,IOF,RAD) count=0:1 DEFAULT=--
PARM BREAK_UM   REAL    DEFAULT=0.0
PARM LOG	KEYWORD VALID=(LOG,NOLOG)	DEFAULT=LOG
PARM TEST	KEYWORD VALID=(TEST,NOTEST)	DEFAULT=NOTEST
END-PROC
.title 
VICAR PROGRAM HIST2D
.help
PURPOSE and OUTPUT FILE DESCRIPTION

HIST2D will generate a two-dimensional histogram file from a VICAR file of 
three dimensions. The two-dimensional histogram file has as its Y-axis (lines) 
the range of DN values of the input file or such values scaled for radiance or 
reflectance, I/F, and as its X-axis (samples) the input's band numbers.  Thus 
each line of the histogram file represents the frequency of a data number (DN) 
through all the bands of the input file.  A sample slice through all the lines 
of the histogram file represents a one-dimensional histogram for a band of the 
input file. Please note the values of the Y-axis decrease from their maximum to
zero as file line number increases. Band number of the X-axis increases as file
sample number increases.  This provides a common origin for both DN or scaled
DN value and band dimensions.
.page
OUTPUT FILE DESCRIPTION continued 
  
The frequencies of each band's histogram are represented by the pixels or data 
numbers (DNs) of the file, corresponding to the number of occurrences of DN, I/F
or radiance values in the input file. Histogram data numbers have a range of 
either (0,255) or (0,32767), depending on the output data format specified by 
the user.  Frequencies are normalized to 255 for byte output format and 32767 
for halfword output format.
.page
EXECUTION

To execute the program HIST2D, the following syntax should be used:

	HIST2D   INP   OUT   user-parameters

where INP is the three-dimensional (3-D) VICAR format file used to generate the 
two-dimensional (2-D) histogram output file, OUT.  

.page
USER PARAMETERS - SUMMARY

User parameters consist of the histogram range (LIMITS), format of output data 
(FORMAT), input file windowing parameters (SL,SS,SB,NL,NS,NB), incrementing 
values for various input file dimensions to speed execution of histogram 
(SINC,LINC,BINC), a parameter to exclude or include the input file data number 
value of 0 in the histogram range (EXCLUDE), parameters to control the output 
size (OUT_NL,OUT_NS), a parameter to replicate sample values (band numbers) 
to fit in the vertical dimension (REPLICATE), a parameter to define the y-axis
scale of the two-dimensional histogram (DN, IOF, RAD) and a parameter to take
the logarithm of the two-dimensional histogram values to increase resolution
(LOG).
.page
USER PARAMETERS

LIMITS

This allows the user to restrict the data values of the input file used to 
generate the two-dimensional histogram output file.  Valid limits for byte
input data should be between (0,255).  Valid limits for halfword input data
should be between (-32768,32767).  Defaults: (0,255) for byte; (0,32767 for 
halfword; (-32752,32767 for halfword of calibration type RADIANCE or IOF,
which correspond to the ISIS "Special Values" range.

The range specified by the limits parameter is then mapped into the number of
output lines in the file, such that an output file of 512 lines with limits
(-32768,32767) would map 32768 values into 512 discrete units.  DN values 
between -32768 and 0 are accumulated for each band's histogram and added to the
zero (0) DN value frequency of the respective histogram.

RLIMIT

This sets a lower limit to valid floating point (REAL*4) data, which can be
useful for treating ISIS data which assigns the lower end of the range to
"special values".  Note that this is platform-dependent;  the default
(0xFF7FFFFA) is for IEEE floating point.

.page
USER PARAMETERS continued

FORMAT 

Allows the user to specify an output file of data type byte (BYTE) or halfword
(HALF).  The default is BYTE.

SINC, LINC, BINC

Sample, line, and band increments are used to control the amount of the input 
image file used in the calculation of the two-dimensional histogram.  The
default for all incrementations is one (1).

.page
USER PARAMETERS continued

SL,SS,SB,NL,NS,NB

Starting line, starting sample, starting band, number of lines, number of 
samples and number of bands are used to control what portion of the image file
is used to compute the two-dimensional histogram.  The default is for all 
starting values to be one (1) and the number of (NL, NS, NB) values are the
values obtained by XVGET of the input file.

OUT_NL, OUT_NS

These control the output file (two-dimensional histogram) dimensions of 
number of lines and of samples.  OUT_NS should not be smaller than the
number of bands specified for the window of the input file bands. The
defaults are OUT_NL=512 and OUT_NS=512.

.page
USER PARAMETERS continued

DN, IOF, RAD

The output file's y-axis may be scaled in raw DN, IOF or radiance (RAD).
If RAD or IOF is specified, the following label items must be found 
in the history labels of the input file: "CAL_TYPE='RAD'", "RAD_CONV",
"RAD_BASE", and "SOLAR_F".  These items make possible the conversion 
between DN and radiance and IOF.  Radiance and Solar Flux must be provided 
in units of uWATT/cm**2/micron/steradian.  The default keyword is DN.
.page
USER PARAMETERS continued

LOG,NOLOG

This keyword controls whether or not the logarithm is taken of the DN's of the
output file, the two-dimensional histogram.  Default keyword is LOG.

TEST, NOTEST

These keywords control whether or not a test histogram file and screen display 
are produced which are used for verification of the correct operation of the 
program.  This option is for the software developers' and testers' use.  The 
following are output to screen: band number at which maximum DN occurs in 
histogram plot; scale of the Y-axis in terms of DN per pixel; scale of 
histogram file's DN to histogram's frequency axis.
.page
EXECUTION EXAMPLES

VICAR> HIST2D  IO_LIMB.IMG  IO_LIMB.HST  FORMAT=HALF LIMITS=(123,3000)

The above example takes input file IO_LIMB.IMG and generates a 2-D histogram 
file, IO_LIMB.HST, of data format HALF.  Data numbers -32768 to 122 and 3000 to 
32767 are not included in the generation of the histogram.  In this case the
resolution of the Y-axis will be (0,4096).  HIST2D follows HICCUP.COM handling 
of halfword inputs, except negative halfword values are included in the scaling
of the output.  A convenient maximum value for the Y-axis is chosen based on 
the maximum DN value found in the input file which happens to be less than the 
upper value of the user specified LIMITS parameter.  IF THE LOWER LIMIT IS LESS
THAN ZERO, the absolute value of that lower LIMIT is added into the maximum.
This maximum is always a multiple of 256 in the DN mode of operation.  Please 
note that DN 0 is not always the Y-axis origin for halfword data; the origin
is the lower LIMIT value.  LIMIT defaults are (0,255) for byte; (0,32767) for
halfword data; and (-32752,32767) for halfword data of calibration type RADIANCE
or IOF.
.page
EXECUTION EXAMPLES (continued)

VICAR> HIST2D  JUPITER_LIMB.IMG  JUPITER_LIMB.HST  'EXCLUDE SL=20 SS=120 NB=201

This example takes input file JUPITER_LIMB.IMG and generates a 2-D histogram
file, JUPITER_LIMB.HST, of default data format BYTE.  If the input file is
of data format BYTE, the histogram range of bin DNs is (1,255) because of the
EXCLUDE parameter.  Byte data allows the user specified range endpoints to be 
mapped to 0 and 511 on the Y-axis of the two dimensional histogram.  The bin 
size is 255/512 or 0.498, thus mapping the 255 values into 512 bins.   
Finally, HIST2D, because of "SL=20 SS=120", ignores the first 19 lines and 
first 119 samples of each band of the input file JUPITER_LIMB.IMG.  Also, due
to "NS=201", only bands 1 through 201 are included in the processing.  
.page
VICAR LABELS OF 2-D HISTOGRAM FILE

In order to provide the necessary information to a display routine or mask 
program, a number of VICAR label items are added to the histogram file.  These 
items include the input file data number (DN) range of the histogram (LIMITS - 
revised depending on the EXCLUDE parameter value), the incrementing values of 
the input file dimensions (INC_LSB - LSB is an acronym for line, sample, band), 
the input file windowing parameters (WINDOW - array of SL,SS,SB,NL,NS,NB), and 
the input file name of HIST2D (INP_FILE).  MAXDN is also a label item which
describes the DN, IOF, or RAD value that represents the maximum pixel value
of the y-axis of the two histogram.  All these label items provide sufficient 
information to properly display the two-dimensional histogram.
.page
POSSIBLE ERROR CONDITIONS

The program HIST2D does not accept two-dimensional files as inputs.  That is to
say that input files must have at least two bands to be processed; otherwise the
program aborts.  Also, input files must be of data format BYTE or HALF.  Other 
forms of input will result in a program abort.  
.page
BIN CALCULATION

For the users' benefit, the following formula is used to calculate the appro-
priate bin in which a byte input file data number (DN) will fall.

	BIN_size = [UPPER_limit - LOWER_limit + 1] / OUT_NL

	BIN 	 = [INPUT_data_number - LOWER_limit] / BIN_size

where 	UPPER_limit is the upper limit of the histogram range (units - DN),
	LOWER_limit is the lower limit of the histogram range (units - DN),
	OUT_NL is the number of lines in the histogram file (default is 512),
	BIN_size is the size of the bin (units - DN),
	INPUT_data_number is the data number to be placed in a bin, and
	BIN is the bin number of the INPUT_data_number (0,511).
.page
HISTORY

Written by: Justin F. McNeill, November 1989
Cognizant Programmer: L.W.Kamp
Revisions: 

	Nov.1993	Fixed default lower LIMITS of -32752 for RAD/IOF case
			PCA stuff removed (LWK)

	May 1992	Corrected upper limit of NIMS special values to 
			-32752.
	
	Feb 1992	Program will handle merged mosaic cubes with or
			without per-band scaling. Interface with NIMSMASK
			(MAXDN) corrected. Test option added for
			development purposes. Histogram calculation for
			byte and halfword simplified. PCA added. (JFM)
									     
	Sept 1991	Changed NELEMENTS to NELEMENT in XLGET per new Vicar 
			exec; removed SUN_D check from RAD case. (LWK)	     
									     
	June 1991	Per band radiance offset and scaling implemented.    

	Oct  1990   	RAD_BASE offset for conversion of DN to IOF or RAD   
			added in calculations. Test script revised.	     
									     
	Sept 1990   	Code added to prevent division by zero. FR 63252     

	Aug  1990 	Window label item revised to be (SL,SS,SB,NL,NS,NB). 
		   	DN and LOG parameters added to help and tutor.       
			Samplewidth problem corrected and negative halfword  
			data properly handled.	Ref. FRs 63221 & 63222	     
			Improved error checking of user parameters.          
			Revised test script.				     
			Solar flux units changed to WATT/CM**CM/MICRON/STER  
									     
	July 1990	Handling of IOF and RAD y-axis scales is provided.   
								             
	June 1990	Addition of output size parameters OUT_NL & OUT_NS;  
			write output file such that band and DN dimensions   
			have a common origin; provide parameter to provide   
			for replication of band samples to fit in 512 samples.
.LEVEL1
.VARI INP
Input image file name
.VARI OUT
2D Histogram file name
.VARI TOUT
Test output file name
.VARI LIMITS
Histogram range of input DNs
.VARI RLIMIT
Lower histogram range of input
data if floating-point
(NOTE: the default is specific
to IEEE!)
.VARI EXCLUDE
Variable to exclude DN 0 
from input
.VARI FORMAT
Data format of histogram DNs
.VARI SINC
Increment through input file 
by SINC number of samples
.VARI LINC
Increment through input file 
by LINC number of lines
.VARI BINC
Increment through input file 
by BINC number of bands
.VARI SL
Starting line of input file
.VARI SS
Starting sample of input file
.VARI SB
Starting band of input file
.VARI NL
Number of lines to be used 
of input file
.VARI NS
Number of samples to be used 
of input file
.VARI NB
Number of bands to be used 
of input file
.VARI OUT_NL
Number of desired lines in 
output; default is 512.
.VARI OUT_NS
Number of desired samples in 
output; default is 512.
.VARI REPLIC
Keyword to replicate the number
of bands to fit within 512.
.VARI DN
Keyword to control the y-axis or
DN scale of the two-dimensional
histogram.
.VARI LOG
Keyword to specify that the 
logarithm be applied to the 
histogram.
.VARI TEST
Keyword to specify that test
option for correct program
output is specified.
.VARI BREAK_UM
Wavelength below which Radiance
is divided by Solar Flux
.LEVEL2
.VARI INP
The file name of the three-dimensional input file used to generate the histogram
.VARI OUT
The two-dimensional histogram file name generated by this program
.VARI TOUT
The test file output generated by this program is a histogram of the band con-
taining the greatest DN value. The default name for this output is HIST.TST.
This file is of the same format as HISTGEN output and should be used for com-
parison to HISTGEN output. 
.VARI LIMITS
Histogram bin range in data number units (DNs).  Defaults are (0,255) for byte 
data; (0,32767) for halfword data; and (-32752,32767) for halfword data of 
calibration type RADIANCE or IOF.
.VARI RLIMIT
This sets a lower limit to valid floating point (REAL*4) data, which can be
useful for treating ISIS data which assigns the lower end of the range to
"special values".  Note that this is platform-dependent;  the default
(0xFF7FFFFA) is for IEEE floating point.
.VARI EXCLUDE
Variable to exclude DN 0, or -32768 for LIMITS=(-32768,upper_value),of 3-D input
file from histogram bin range.  Default value is not to exclude DN 0 from 
histogram processing.
.VARI FORMAT
Data format - byte or half - of two-dimensional histogram file.  
Default value is BYTE.
.VARI SINC
Increment through input file by SINC number of samples to speed histogram
generation.  Default value is 1.
.VARI LINC
Increment through input file by LINC number of lines to speed histogram 
generation.  Default value is 1.
.VARI BINC
Increment through input file by BINC number of bands to speed histogram 
generation.  Default value is 1.
.VARI SL
Window parameter of input file; first line of input file to be included in the
processing of the histogram.  Default value is 1.
.VARI SS
Window parameter of input file; first sample of input file to be included in the
processing of the histogram.  Default value is 1.
.VARI SB
Window parameter of input file; first band of input file to be included in the
processing of the histogram.  Default value is 1.
.VARI NL
Number of lines of input file to be used in the processing of the histogram.
Default value is the number of lines in VICAR label of input file.
.VARI NS
Number of samples of input file to be used in the processing of the histogram.
Default value is the number of samples in VICAR label of input file.
.VARI NB
Number of bands of input file to be used in the processing of the histogram.
Default value is the number of bands in VICAR label of input file.
.VARI OUT_NL
Number of desired lines in output. Default is 512.
.VARI OUT_NS
Number of desired samples in output. Default is 512.
.VARI REPLIC
Keyword to replicate the number of bands to fit within 512. The default is
not to replicate, 'NOREP.
.VARI DN
Keyword to control the y-axis or DN scale of the two-dimensional histogram.
Possible y-axis scales are radiance in units uWatt/m**2/micron/steradian 
(RAD), Bidirectional Reflectance (Radiance / SolarFlux) (IOF), or raw DN 
(DN).  Default is DN.
.VARI LOG
Keyword to specify that the logarithm be applied to the histogram.  This
allows for a greater range of frequencies to be displayed, thus increasing
resolution of the histogram frequencies.  Valid keywords are LOG and NOLOG.
LOG is the default.
.VARI TEST
This keyword controls whether output files and screen displays are printed 
which are used for verification of the correct operation of the program.  This
option is for the software developers' and testers' use.  The following are
output to screen: band number at which maximum DN occurs in histogram plot; 
scale of the Y-axis in terms of DN per pixel; scale of histogram file's DN to 
histogram's frequency axis.  An output histogram is also generated and is 
specified by the parameter TOUT.  It is the histogram of the band at which 
maximum DN of all bands occurs.
.VARI BREAK_UM
Wavelength (in microns) below which Radiance is divided by Solar Flux when
displaying the histogram.  This can prevent the long-wavelength end of the
histogram for cool bodies illuminated by sunlight from being overwhelmed by
the bright shorter wavelengths.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tsthist2d.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"     

write "This is the test file for program HIST2D"


write "CREATE NIMS CUBE"

write " WARNING:  nimscmm2 requires the old Spice definitions, so do the"
write " following before entering VICAR:"
write " "
write " > source nims_spicedefs"

write " First, generate a DN (uncalibrated, halfword) cube: "

NIMSCMM2 edr=/project/test_work/testdata/gll/g7jnfeap4101a.3 +
        cube=G7JNFEAP4101A.cub 'NOCAL +
        wtfil=wtfil  +
        aacsfil=/project/test_work/testdata/gll/g7jnfeap4101a.aacs +
        calfil=/project/test_work/testdata/gll/nims98a_gs4_ref_g1_01.tab +
	spkernel=/project/spice/ops/sun-solr/s980326b.bsp +
        ikernel=/project/test_work/testdata/gll/nims_ikernel_mab.dat +
	solfile=/project/test_work/testdata/gll/nims_solar.dat +
	dbmfile=/project/test_work/testdata/gll/boom_obscuration.nim +
        obsname=G7JNFEAP4101 obsext=A +
        prodnote="testbed EDR with simulated pointing"  +
        obsnote="testbed EDR with simulated pointing"  +
        target=GANYMEDE phase=GANYMEDE_7_ENCOUNTER +
        proj=pov slew_tol=-1. +
	outsiz=(9,5)

WRITE "LIST SYSTEM LABEL OF CUBE"

label-list G7JNFEAP4101A.cub 'system

write "CREATE 2-D HISTOGRAM FROM CUBE"

hist2d G7JNFEAP4101A.cub hist1 out_nl=256 out_ns=68 'DN

label-list hist1
list hist1 linc=2 sinc=8

! test REPLICATE keyword:
hist2d G7JNFEAP4101A.cub hist2 out_nl=256 out_ns=408 'DN 'REPLICATE
list hist2 linc=2 sinc=8

write " Next, generate a calibrated, floating-point cube: "

NIMSCMM2 edr=/project/test_work/testdata/gll/g7jnfeap4101a.3 +
        cube=G7JNFEAP4101A_2.cub +
        wtfil=wtfil  +
        aacsfil=/project/test_work/testdata/gll/g7jnfeap4101a.aacs +
        calfil=/project/test_work/testdata/gll/nims98a_gs4_ref_g1_01.tab +
        darkfil=/project/test_work/testdata/gll/jup_sl9_ave.tab +
	spkernel=/project/spice/ops/sun-solr/s980326b.bsp +
        ikernel=/project/test_work/testdata/gll/nims_ikernel_mab.dat +
	solfile=/project/test_work/testdata/gll/nims_solar.dat +
	dbmfile=/project/test_work/testdata/gll/boom_obscuration.nim +
        obsname=G7JNFEAP4101 obsext=A +
        prodnote="testbed EDR with simulated pointing"  +
        obsnote="testbed EDR with simulated pointing"  +
        target=GANYMEDE phase=GANYMEDE_7_ENCOUNTER +
        proj=pov slew_tol=-1. +
	outsiz=(9,5)
label-list G7JNFEAP4101A_2.cub 'system
! specify RLIMIT here so it will work on Linux too:
hist2d G7JNFEAP4101A_2.cub hist3 out_nl=256 out_ns=68 rlimit=-1.0e38
label-list hist3
list hist3 linc=2 sinc=8


write "The following is a test for byte input files."
write "****************************************************************"
write "RAMP1 should differ from RAMP2 by one pixel at line 256"
write "and sample 1.  See listing of image A."
write "****************************************************************"
write "Images RAMP2 and RAMP3 should be identical."
write "****************************************************************"

gen image 10 10 256 ival=0 sinc=0 linc=0
hist2d image ramp1 out_nl=256 out_ns=256 'nolog
hist2d image ramp2 out_nl=256 out_ns=256 'exclude
hist2d image ramp3 out_nl=256 out_ns=256 limits=(1,255)
difpic (ramp1,ramp2) a
list a 'noeje 'nofeed
difpic (ramp2,ramp3)

write "The only difference between RAMP1 and RAMP4 should be"
write "the number of lines and samples of output file."
write "****************************************************************"
write "RAMP4 should be 512x512 and RAMP1.IMG should be 256x256."
write "****************************************************************"
write "RAMP5 should be similar to RAMP4, except samples 1 through 99"
write "should have zero values for all lines."
write "****************************************************************"

hist2d image ramp4 
list ramp1 sl=250 ss=1 ns=10 'noeje 'nofeed
list ramp4 sl=250 ss=1 ns=10 'noeje 'nofeed
lablist ramp1 'full
lablist ramp4 'full

hist2d image ramp5 limits=(100,255)
list ramp5 ss=95 ns=10 sl=250 'noeje 'nofeed
list ramp5 ss=250 sl=1 nl=10 'noeje 'nofeed

write "TEST ERROR CHECKING OF PARAMETERS"
write "The following HIST2D call should produce ten error messages"
write "****************************************************************"

hist2d image testerr limits=(-1,257) sl=11 ss=0 sb=257 nl=11 ns=10 +
	nb=-1 out_ns=20001 out_nl=-10

write "RAMP7 generation should result in a HIST2D warning regarding"
write "invalid OUT_NS value.  It should be set to 512 by HIST2D."
write "****************************************************************"
write "RAMP8 should differ from RAMP7 by a solid line at line 512"
write "from sample 1 through sample 257."
write "****************************************************************"
write "RAMP7 and RAMP9 should differ by one pixel at line 512,"
write "sample 257."
write "****************************************************************"

gen image 5 5 512 format=half ival=-32768 sinc=0 linc=0 binc=128
hist2d image ramp7 out_ns=256 
list ramp7 linc=16 sl=16 sinc=32 'noeje 'nofeed
hist2d image ramp8 limits=(-32768,32767)
list ramp8 linc=16 sl=16 sinc=32 'noeje 'nofeed
hist2d image ramp9 'exclude
list ramp9 linc=16 sl=16 sinc=32 'noeje 'nofeed

write "RAMP10 should have three values of 255 on line 10 at samples"
write "1, 2 and 3.  It should have two values of 255 on line 9 at"
write "samples 4 and 5."
write "****************************************************************"

gen image 10 10 5 ival=0 linc=0 sinc=0 binc=10 'half
hist2d image ramp10 out_nl=10 out_ns=10 
list ramp10 'noeje 'nofeed

write "Check illegal formats:"
gen image 10 10 5 'full
hist2d image dum
gen image 10 10 5 'doub
hist2d image dum

end-proc
$!-----------------------------------------------------------------------------
$ create nims_spicedefs
# to execute:  > source nims_spicedefs

# do the old Spice definitions

setenv SPICEKER      /project/spice/ops/$VICCPU
setenv BODY_IDS      /project/spice/ops/bodid.dat
setenv LEAPSECONDS   /project/spice/ops/leapseconds.ker
setenv SCLK          /project/spice/ops/sclk.ker
setenv CONSTANTS     /project/spice/ops/p_constants.ker
setenv BINPOOL       /project/spice/ops/$VICCPU/binpool.ker
setenv KERNELDB      /project/spice/ops/kdb.gll
$ Return
$!#############################################################################
$Imake_File:
$ create hist2d.imake
#define PROGRAM hist2d
#define MODULE_LIST hist2d.c 
#define MAIN_LANG_C
#define USES_C
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
/*#define DEBUG			/* remove on delivery */
/*#define LIB_LOCAL		/* remove on delivery */
$ Return
$!#############################################################################
