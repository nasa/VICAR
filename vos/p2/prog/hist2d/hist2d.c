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
