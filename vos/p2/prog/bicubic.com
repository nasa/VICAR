$!****************************************************************************
$!
$! Build proc for MIPL module bicubic
$! VPACK Version 1.9, Monday, December 07, 2009, 16:00:50
$!
$! Execute by entering:		$ @bicubic
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
$ write sys$output "*** module bicubic ***"
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
$ write sys$output "Invalid argument given to bicubic.com file -- ", primary
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
$   if F$SEARCH("bicubic.imake") .nes. ""
$   then
$      vimake bicubic
$      purge bicubic.bld
$   else
$      if F$SEARCH("bicubic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bicubic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bicubic.bld "STD"
$   else
$      @bicubic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bicubic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bicubic.com -mixed -
	-s bicubic.c -
	-i bicubic.imake -
	-p bicubic.pdf -
	-t tstbicubic.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bicubic.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Vicar Program BICUBIC - Enlarges images by integer zoom factors by 
]			convolutional interpolation with various filters.

	Written by:	R. Mortensen
	Date:		7-Oct-88
	Place:		Jet Propulsion Laboratory

***********************************************************************/
/*  #include main  */
#include <math.h>
#include <stdio.h>
#include <stdlib.h> 
#include "vicmain_c"  

#define PI 3.1415926
#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a < b ? a : b)
#define ABS(a)   (a > 0 ? a : -(a))
#define NINT(a)  (int) (a > 0 ? a + 0.5 : a - 0.5)

#define ForceByte(a) (unsigned char) MAX(0, MIN(255, NINT(a)))
#define ForceHalf(a) (short) MAX(-32768, MIN(32767, NINT(a)))
#define ForceFull(a) (long) NINT(a)
#define ForceReal(a) (float) (a)
#define ForceDouble(a) (double) (a)

#define Extrapolate(a, b) (b + b - a)
/*  #define ExtrapByte(a, b) ForceByte(Extrapolate(a,b)) */
#define ExtrapHalf(a, b) ForceHalf(Extrapolate(a,b))
#define ExtrapFull(a, b) ForceFull(Extrapolate(a,b))
#define ExtrapReal(a, b) ForceReal(Extrapolate(a,b))
#define ExtrapDouble(a, b) ForceDouble(Extrapolate(a,b))

int nl,ns,nlo,nso,ss,sl,nli,nsi;
int nl1,ns1,hzoom[2],zoom[2],nlw,nsw;
typedef enum {Byte, Half, Full, Real, Double} PixelType;
int ns3,nl3;

/* unsigned char Extrap_Byte(unsigned char a, unsigned char b); */

void main44(void)
{
  int line;
  double **weights;
  unsigned char *ipixels[4], *buffers[4], *opixels, *tmp;
  int iunit,ounit,cnt,stat;
  int width,interp_line;
  int first=1;
  int lines_read,i,pixelBytes;
  char formatStr[31];
  PixelType format;
  int el,es,size_changed;
  char message[80];
  int odometer;

  extern nl,ns,nlo,nso,ss,sl,nli,nsi;
  extern hzoom[2],zoom[2],nlw,nsw;
       /* nl3 and ns3 now globally defined */

/* Out put ported revision date */

   zvmessage("BICUBIC version 01-JULY-94", "" );

/* Get the zoom factor */
  zvp("ZOOM", zoom, &cnt);
  if (cnt == 1)			/* If only line is given... */
    zoom[1] = zoom[0];;		/*  use it for both.  */

  hzoom[0] = zoom[0]/2;
  hzoom[1] = zoom[1]/2;

  /* Calculate the filter (weights) size.  */

  width = 2;
  nlw = zoom[0]*width;
  nsw = zoom[1]*width;

  /* Get the input and output unit numbers.  */

  zvunit(&iunit, "INP", first, NULL);
  zvunit(&ounit, "OUT", first, NULL);

  /* Open the input for array I/O and get size.  */

  zvopen(iunit, "OP", "READ", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL );
  zvsize(&sl, &ss, &nl, &ns, &nli, &nsi);

  /* Check the size. */

  size_changed = 0;
  el = sl + nl - 1;
  es = ss + ns - 1;

  if (sl < 1) {
    sl = 1;
    size_changed = 1;
  }
  if (ss < 1) {
    ss = 1;
    size_changed = 1;
  }
  if (el > nli) {
    el = nli;
    size_changed = 1;
  }
  if (es > nsi) {
    es = nsi;
    size_changed = 1;
  }

  nl = el - sl + 1;
  ns = es - ss + 1;
  if ((nl < 2) || (ns < 2))
  {
    zvmessage("***** There must be at least two lines and samples", "");
    zvmessage("***** in the image or sub-area.", "" );
    zabend();
  }
  else
    if (size_changed)   
       zvmessage("***** Sub-area adjusted...", "" );

  if (size_changed || (sl != 1) || (ss != 1) || (nl != nli) || (ns != nsi))
  {
    sprintf(message, "***** Using sub-area (%d, %d, %d, %d).",
	    sl, ss, nl, ns);
    zvmessage(message, "");
  }

  /* Get the pixel format. */

  zvget(iunit, "FORMAT", formatStr, NULL );
  if (strncmp(formatStr,"BYTE",4) == 0) {
    format = Byte;
    pixelBytes = sizeof (unsigned char);
  }
  else if (strncmp(formatStr,"HALF",4) == 0) {
    format = Half;
    pixelBytes = sizeof (short);
  }
  else if (strncmp(formatStr,"FULL",4) == 0) {
    format = Full;
    pixelBytes = sizeof (long);
  }
  else if (strncmp(formatStr,"REAL",4) == 0) {
    format = Real;
    pixelBytes = sizeof (float);
  }
  else if (strncmp(formatStr,"DOUB",4) == 0) {
    format = Double;
    pixelBytes = sizeof (double);
  }
  else {
    sprintf(message, "*** Unknown format type:  '%s'", formatStr);
    zvmessage(message, "");
    zabend();
  }

  /* Calculate the output size and let user know.  */

  nlo = nl*zoom[0];
  nso = ns*zoom[1];
  sprintf(message, "***** Output image size:  %d lines by %d samples.",
	  nlo, nso);
  zvmessage(message, ""); 

  /* Open the output. */

  zvopen(ounit, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
	 "U_NL", nlo, "U_NS", nso, NULL);

  /* Allocate memory for pixels. */

  for (i = 0; i < 4; i++) {
    ipixels[i] = (unsigned char *) malloc((nso+4)*pixelBytes);
    if (ipixels[i] == NULL) {
      zvmessage("  ***** Ran out of memory.", ""); 
      zabend();
    }
  }

  opixels = (unsigned char *) malloc(nso*pixelBytes);
  if (opixels == NULL) {
    zvmessage("  ***** Ran out of memory.", ""); 
    zabend();
  }

  /* Get the weights of the interpolation function.  */

  GetWeights(&weights, nlw, nsw, zoom[0], zoom[1]);

  nl3 = nl+3;
  ns3 = ns+3;

  /* Read first three lines that will be used */

  zvread(iunit, ipixels[2]+2*pixelBytes,
	 "LINE", sl, "SAMP", ss, "NSAMPS", ns, NULL);
  zvread(iunit, ipixels[3]+2*pixelBytes,
	 "SAMP", ss, "NSAMPS", ns, NULL );
  lines_read = 2;

  /* Extrapolate Lines */

  ExtrapolateLine(format, ipixels[3], ipixels[2], ipixels[1]);
  ExtrapolateLine(format, ipixels[2], ipixels[1], ipixels[0]);

  /* Extrapolate Samples */

  ExtrapolateSamps(format, ipixels[0]);
  ExtrapolateSamps(format, ipixels[1]);
  ExtrapolateSamps(format, ipixels[2]);
  ExtrapolateSamps(format, ipixels[3]);

  interp_line = hzoom[0] + 1;
  odometer = nlo/10;
  for (line=0; line<nlo; line++) {
    if ((line+1)%odometer == 0) {
      sprintf(message, "***** %d%% Done...", 100*(line+1)/nlo);
      zvmessage(message, "" ); 
    }

    if (interp_line == zoom[0]) {
      interp_line = 0;

      /* Swap lines into proper places and read next line */

      tmp = ipixels[0];
      for (i = 0; i < 3; i++) {
	ipixels[i] = ipixels[i+1];
      }
      ipixels[3] = tmp;
      if (lines_read >= nl) {
	ExtrapolateLine(format, ipixels[1], ipixels[2], ipixels[3]);
      }
      else {
	zvread(iunit, ipixels[3]+2*pixelBytes,
	       "SAMP", ss, "NSAMPS", ns, NULL );
	lines_read ++;
      }
      ExtrapolateSamps(format, ipixels[3]);
    }
    
    switch (format) {
    case Byte:
      doByteInterp(ipixels, opixels, weights, interp_line);
      break;
    case Half:
      doHalfInterp(ipixels, opixels, weights, interp_line);
      break;
    case Full:
      doFullInterp(ipixels, opixels, weights, interp_line);
      break;
    case Real:
      doRealInterp(ipixels, opixels, weights, interp_line);
      break;
    case Double:
      doDoubleInterp(ipixels, opixels, weights, interp_line);
      break;
    }

    zvwrit(ounit, opixels, NULL);
    interp_line ++;
  }
  zvclose(iunit, NULL);
  zvclose(ounit, NULL);
}

/* Line extrapolation routines.  Given two lines, these routines use
   a linear extrapolation to produce a third line. Note that the first
   and last two samples are not extrapolated since at this point they
   are undefined; the sample extrapolation routines take care of them. */

ExtrapolateLine(format, buf1, buf2, buf3)
     PixelType format;
     unsigned char *buf1, *buf2, *buf3;
{
  switch (format) {
  case Byte: ExtrapByteLine(buf1, buf2, buf3); break;
  case Half: ExtrapHalfLine(buf1, buf2, buf3); break;
  case Full: ExtrapFullLine(buf1, buf2, buf3); break;
  case Real: ExtrapRealLine(buf1, buf2, buf3); break;
  case Double: ExtrapDoubleLine(buf1, buf2, buf3); break;
  }
}

ExtrapByteLine(buf1, buf2, buf3)
     unsigned char *buf1, *buf2, *buf3;
    
{
  int i;

  for (i = 2; i < ns+2; i++)
    buf3[i] = Extrap_Byte(buf1[i], buf2[i]);
}

ExtrapHalfLine(buf1, buf2, buf3)
     short *buf1, *buf2, *buf3;
    
{
  int i;

  for (i = 2; i < ns+2; i++)
    buf3[i] = ExtrapHalf(buf1[i], buf2[i]);
}

ExtrapFullLine(buf1, buf2, buf3)
     long *buf1, *buf2, *buf3;
    
{
  int i;

  for (i = 2; i < ns+2; i++)
    buf3[i] = ExtrapFull(buf1[i], buf2[i]);
}

ExtrapRealLine(buf1, buf2, buf3)
     float *buf1, *buf2, *buf3;
    
{
  int i;

  for (i = 2; i < ns+2; i++)
    buf3[i] = ExtrapReal(buf1[i], buf2[i]);
}

ExtrapDoubleLine(buf1, buf2, buf3)
     double *buf1, *buf2, *buf3;
    
{
  int i;

  for (i = 2; i < ns+2; i++)
    buf3[i] = ExtrapDouble(buf1[i], buf2[i]);
}

/* Sample extrapolation routines.  Given a line of pixels these routines
   extrapolate the missing two pixels at either end of the line using a 
   linear extrapolation. */

ExtrapolateSamps(format, buf)
     PixelType format;
     unsigned char *buf;

{
  switch (format) {
  case Byte: ExtrapByteSamps(buf); break;
  case Half: ExtrapHalfSamps(buf); break;
  case Full: ExtrapFullSamps(buf); break;
  case Real: ExtrapRealSamps(buf); break;
  case Double: ExtrapDoubleSamps(buf); break;
  }
}

ExtrapByteSamps(buf)
     unsigned char *buf;

{
  buf[1] = Extrap_Byte(buf[3], buf[2]);
  buf[0] = Extrap_Byte(buf[2], buf[1]);
  buf[ns+2] = Extrap_Byte(buf[ns], buf[ns+1]);
  buf[ns+3] = Extrap_Byte(buf[ns+1], buf[ns+2]);
}

ExtrapHalfSamps(buf)
     short *buf;

{
  buf[1] = ExtrapHalf(buf[3], buf[2]);
  buf[0] = ExtrapHalf(buf[2], buf[1]);
  buf[ns+2] = ExtrapHalf(buf[ns], buf[ns+1]);
  buf[ns+3] = ExtrapHalf(buf[ns+1], buf[ns+2]);
}

ExtrapFullSamps(buf)
     long *buf;

{
  buf[1] = ExtrapFull(buf[3], buf[2]);
  buf[0] = ExtrapFull(buf[2], buf[1]);
  buf[ns+2] = ExtrapFull(buf[ns], buf[ns+1]);
  buf[ns+3] = ExtrapFull(buf[ns+1], buf[ns+2]);
}

ExtrapRealSamps(buf)
     float *buf;

{
  buf[1] = ExtrapReal(buf[3], buf[2]);
  buf[0] = ExtrapReal(buf[2], buf[1]);
  buf[ns+2] = ExtrapReal(buf[ns], buf[ns+1]);
  buf[ns+3] = ExtrapReal(buf[ns+1], buf[ns+2]);
}

ExtrapDoubleSamps(buf)
     double *buf;

{
  buf[1] = ExtrapDouble(buf[3], buf[2]);
  buf[0] = ExtrapDouble(buf[2], buf[1]);
  buf[ns+2] = ExtrapDouble(buf[ns], buf[ns+1]);
  buf[ns+3] = ExtrapDouble(buf[ns+1], buf[ns+2]);
}

/* Interpolation routines. */

doByteInterp(ipixels, opixels, weights, interp_line)
     unsigned char *ipixels[5];
     unsigned char *opixels;
     double *weights[];
     int interp_line;

{
  extern nl,ns,nlo,nso,ss,sl,nli,nsi;
  extern hzoom[2],zoom[2],nlw,nsw;
     /*  nl3,ns3 not included ( not found via extern -- also not used) */



  int start_s;
  int samp,which_samp;
  int interp_samp;
  double value;
  int i,j,l,s,pixel;

  interp_samp = hzoom[1] + 1;
  start_s = 0;
  for (samp=0; samp<nso; samp++) {
    if (interp_samp == zoom[1]) {
      interp_samp = 0;
      start_s ++;
    }

    value = 0.0;
    j = interp_line + zoom[0];
    for (l = 0; l < 4; l++) {
      if (ABS(j) < nlw) {
	i = interp_samp + zoom[1];
	for (s = 0; s < 4; s++) {
	  if (ABS(i) < nsw) {
	    which_samp = MAX(0,MIN(ns3,start_s+s));
	  
	    pixel = ipixels[l][which_samp];
	    value += (double) pixel * weights[ABS(j)][ABS(i)];
	  }
	  i -= zoom[1];
	}
      }
      j -= zoom[0];
    }
    
    opixels[samp] = ForceByte(value);
    interp_samp ++;
  }
}

doHalfInterp(ipixels, opixels, weights, interp_line)
     short *ipixels[5];
     short *opixels;
     double *weights[];
     int interp_line;

{
  extern nl,ns,nlo,nso,ss,sl,nli,nsi;
  extern hzoom[2],zoom[2],nlw,nsw;
          /*  nl3,ns3 not included ( not found via extern -- also not used) */

  int start_s;
  int samp,which_samp;
  int interp_samp;
  double value;
  int i,j,l,s,pixel;

  interp_samp = hzoom[1] + 1;
  start_s = 0;
  for (samp=0; samp<nso; samp++) {
    if (interp_samp == zoom[1]) {
      interp_samp = 0;
      start_s ++;
    }

    value = 0.0;
    j = interp_line + zoom[0];
    for (l = 0; l < 4; l++) {
      if (ABS(j) < nlw) {
	i = interp_samp + zoom[1];
	for (s = 0; s < 4; s++) {
	  if (ABS(i) < nsw) {
	    which_samp = MAX(0,MIN(ns3,start_s+s));
	  
	    pixel = ipixels[l][which_samp];
	    value += (double) pixel * weights[ABS(j)][ABS(i)];
	  }
	  i -= zoom[1];
	}
      }
      j -= zoom[0];
    }
    
    opixels[samp] = ForceHalf(value);
    interp_samp ++;
  }
}

doFullInterp(ipixels, opixels, weights, interp_line)
     long *ipixels[5];
     long *opixels;
     double *weights[];
     int interp_line;

{
  extern nl,ns,nlo,nso,ss,sl,nli,nsi;
  extern hzoom[2],zoom[2],nlw,nsw;
         /*  nl3,ns3 not included ( not found via extern -- also not used) */

  int start_s;
  int samp,which_samp;
  int interp_samp;
  double value;
  int i,j,l,s;
  long pixel;

  interp_samp = hzoom[1] + 1;
  start_s = 0;
  for (samp=0; samp<nso; samp++) {
    if (interp_samp == zoom[1]) {
      interp_samp = 0;
      start_s ++;
    }

    value = 0.0;
    j = interp_line + zoom[0];
    for (l = 0; l < 4; l++) {
      if (ABS(j) < nlw) {
	i = interp_samp + zoom[1];
	for (s = 0; s < 4; s++) {
	  if (ABS(i) < nsw) {
	    which_samp = MAX(0,MIN(ns3,start_s+s));
	  
	    pixel = ipixels[l][which_samp];
	    value += (double) pixel * weights[ABS(j)][ABS(i)];
	  }
	  i -= zoom[1];
	}
      }
      j -= zoom[0];
    }
    
    opixels[samp] = ForceFull(value);
    interp_samp ++;
  }
}

doRealInterp(ipixels, opixels, weights, interp_line)
     float *ipixels[5];
     float *opixels;
     double *weights[];
     int interp_line;

{
  extern nl,ns,nlo,nso,ss,sl,nli,nsi;
  extern hzoom[2],zoom[2],nlw,nsw;
         /*  nl3,ns3 not included ( not found via extern -- also not used) */

  int start_s;
  int samp,which_samp;
  int interp_samp;
  double value,pixel;
  int i,j,l,s;

  interp_samp = hzoom[1] + 1;
  start_s = 0;
  for (samp=0; samp<nso; samp++) {
    if (interp_samp == zoom[1]) {
      interp_samp = 0;
      start_s ++;
    }

    value = 0.0;
    j = interp_line + zoom[0];
    for (l = 0; l < 4; l++) {
      if (ABS(j) < nlw) {
	i = interp_samp + zoom[1];
	for (s = 0; s < 4; s++) {
	  if (ABS(i) < nsw) {
	    which_samp = MAX(0,MIN(ns3,start_s+s));
	  
	    pixel = ipixels[l][which_samp];
	    value += (double) pixel * weights[ABS(j)][ABS(i)];
	  }
	  i -= zoom[1];
	}
      }
      j -= zoom[0];
    }
    
    opixels[samp] = ForceReal(value);
    interp_samp ++;
  }
}

doDoubleInterp(ipixels, opixels, weights, interp_line)
     double *ipixels[5];
     double *opixels;
     double *weights[];
     int interp_line;

{
  extern nl,ns,nlo,nso,ss,sl,nli,nsi;
  extern hzoom[2],zoom[2],nlw,nsw;
        /*  nl3,ns3  not included ( not found via extern -- also not used) */

  int start_s;
  int samp,which_samp;
  int interp_samp;
  double value,pixel;
  int i,j,l,s;

  interp_samp = hzoom[1] + 1;
  start_s = 0;
  for (samp=0; samp<nso; samp++) {
    if (interp_samp == zoom[1]) {
      interp_samp = 0;
      start_s ++;
    }

    value = 0.0;
    j = interp_line + zoom[0];
    for (l = 0; l < 4; l++) {
      if (ABS(j) < nlw) {
	i = interp_samp + zoom[1];
	for (s = 0; s < 4; s++) {
	  if (ABS(i) < nsw) {
	    which_samp = MAX(0,MIN(ns3,start_s+s));
	  
	    pixel = ipixels[l][which_samp];
	    value += (double) pixel * weights[ABS(j)][ABS(i)];
	  }
	  i -= zoom[1];
	}
      }
      j -= zoom[0];
    }
    
    opixels[samp] = ForceDouble(value);
    interp_samp ++;
  }
}

GetWeights(w, nlw, nsw, zl, zs)
     double ***w;
     int nlw,nsw,zl,zs;

{
  int i, j, cnt;
  double X, Y, sigX, sigY, varX, varY, wY;
  float b,c;
  double a1,b1,c1,d1,a2,b2,c2,d2;

  *w = (double **) malloc(sizeof (double *)*nlw);
  if (*w == NULL) {
    zvmessage("**** Ran out of memory.", "");  
    zabend();
  }

  for (i = 0; i < nlw; i++) {
    (*w)[i] = (double *) malloc((sizeof (double))*nsw);
    if ((*w)[i] == NULL) {
      zvmessage("**** Ran out of memory.", "");  
      zabend();
    }
  }

  zvp("B", &b, &cnt);
  zvp("C", &c, &cnt);

  a1 = (12 - 9*b - 6*c)/6;
  b1 = (-18 + 12*b + 6*c)/6;
  c1 = 0;
  d1 = (6 - 2*b)/6;

  a2 = (-b - 6*c)/6;
  b2 = (6*b + 30*c)/6;
  c2 = (-12*b - 48*c)/6;
  d2 = (8*b + 24*c)/6;

  for (j = 0; j < nlw; j++) {
    Y = (double) j / (double) zl;
    if (Y < 1)
      wY = ((a1*Y + b1)*Y + c1)*Y + d1;
    else if (Y < 2)
      wY = ((a2*Y + b2)*Y + c2)*Y + d2;
    else
      wY = 0;

    for (i = 0; i < nsw; i++) {
      X = (double) i / (double) zs;
      if (X < 1)
	(*w)[j][i] = wY*(((a1*X + b1)*X + c1)*X + d1);
      else if (X < 2)
	(*w)[j][i] = wY*(((a2*X + b2)*X + c2)*X + d2);
      else
	(*w)[j][i] = 0;
    }
  }
}


Extrap_Byte(a, b)
   unsigned char a, b;

 /***************************************************************/
 /* Extrap_Byte                                                 */
 /*   Provides a portable replacement for the ExtrapByte macro  */
 /*   ( ">" & "SCONV change in ansi C requires explicit cast)   */
 /***************************************************************/

{
  unsigned char temp_chara, temp_charb;
  unsigned char CharZero = 0;
  
  temp_chara = b + b - a;  /*  Extrapolate a,b */
  temp_charb = MIN(255, NINT(temp_chara));
  if (CharZero > temp_charb)
     return CharZero;
  else
     return temp_charb;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bicubic.imake
#define PROGRAM  bicubic

#define MODULE_LIST bicubic.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#if  SGI_ARCH
#define C_OPTIONS -Wf,-XNh4000
#endif

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create bicubic.pdf
PROCESS HELP=*
PARM INP STRING
PARM OUT STRING
PARM SIZE INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL INTEGER DEFAULT=1
PARM SS INTEGER DEFAULT=1
PARM NL INTEGER DEFAULT=0
PARM NS INTEGER DEFAULT=0
PARM ZOOM INTEGER COUNT=1:2
PARM B REAL DEFAULT=.333333333
PARM C REAL DEFAULT=.333333333
END-PROC
.TITLE
Vicar Program BICUBIC -- Enlarges images by integer zoom factors using cubic
			convolutional reconstruction filters.
.HELP
BICUBIC is a Vicar program that enlarges images by integer zoom factors using
convolutional reconstruction techniques.  A piecewise cubic convolutional 
filter is used.

USAGE:
	BICUBIC input output [size] ZOOM=zoom [parms]

OPERATION:

BICUBIC determines the output image size from the specified zoom factor(s) and
the input image size or sub-area (i.e. NLO = NL*ZOOM(1) and NS = NSO*ZOOM(2)).
The output image is then written line by line, pixel by pixel.  The value of an
output pixel is determined by convolving the input image with a piecewise cubic
interpolation function.  For details on convolutional interpolation, the user
is referred to [HOU78], [KEY81], [MIT88] and [PAR83].

In order to perform the interpolation near the image (or sub-area) edges, the
pixels need to be extrapolated in all directions.  This is done using a simple
linear extrapolation of the two edge pixels in any column or row.
.page
The piecewise cubic interpolation filters are described in [MIT88].  The 
piecewise cubic function is:

	           / (12 - 9*B - 6*C)*|x|**3 +		for |x| < 1
		  /	(-18 + 12*B + 6*C)*|x|**2 +
		 /	(6 - 2*B)
	 	/
	f(x) = <     (-B - 6*C)*|x|**3 + 		for 1 <= |x| < 2
	 	\	(6*B + 30*C)*|x|**2 +
	 	 \	(-12*C - 48*C)*|x| + (8*B + 24*C)
		  \
		   \ 0					otherwise

where B and C are parameters that control the shape of the cubic curves
and thus the appearance of the output image (see CONTROLLING THE CUBIC 
FUNCTION below).
.page 
CONTROLLING THE CUBIC FUNCTION:

The piecewise cubic interpolation function can be controlled by two parameters
(B and C) as shown in the above formulas.  The effects of changing these
parameters is generalized in the graph of B and C on the following page.  This
graph is the result of a subjective test in which nine expert observers were
shown images reconstructed with random values of B and C [MIT88].  The
observers were asked to classify the appearence of the images into one of
these categories: blurring, ringing, anisotropy, and satisfactory.  The graph
shows the results of the test over the range of 0.0 to 1.0.  Of course B and C
may be outside this range, the effects will be more noticeable.
.page
		1.0 +-------------+---------------+
		    |             |               |
		    |             |               |
	     B	0.8 +           _~ \      II      |	Regions:
	      	    |     I    /    \             |	-------
	     P	    |         <      ~-_         _+
	     a	0.6 +          >        \     _-~ |	I   - Blurring
	     r	    |      __-~         |~-_-~    |	II  - Anisotropy
	     a	    +__--~~            /          |	III - Ringing
	     m	0.4 + \               |           |	IV  - Anisotropy
	     e	    |  \       V     /            |	V   - Satisfactory
	     t	    |   \           /      III    |
	     e	0.2 +    ~\         |             |
	     r	    |  IV  \        \             |
		    |       ~~~\     |            |
		0.0 +-----+-----+----++-----+-----+		
		   0.0   0.2   0.4   0.6   0.8   1.0
			      C Parameter 
.page

The default values for (B, C) are (1/3, 1/3) which is recommended by [MIT88].
Other interesting values are:

	(1, 0)	 - Equivalent to the Cubic B-Spline,
	(0, 0.5) - Equivalent to the Catmull-Rom Spline,
	(0, C)	 - The family of Cardinal Cubic Splines,
	(B, 0)   - Duff's tensioned B-Splines [DUF86].

.page
EXAMPLES:

	BICUBIC TEST.IMG TEST2.IMG ZOOM=3

	BICUBIC TEST.IMG TEST3.IMG ZOOM=(5,4)

	BICUBIC TEST.IMG TEST4.IMG ZOOM=7 B=1 C=0

.page
REFERENCES:

[DUF86]	Duff, Tom, "Splines in Animation and Modeling", State of the Art in
	Image Synthesis, SIGGRAPH 86 Course Notes.

[HOU78]	Hou, Hsieh S. and Andrews, Harry C., "Cubic Splines for Image
	Interpolation and Digital Filtering", IEEE Trans. Acoustics, Speech,
	and Signal Processing, Vol. ASSP-26, No. 6, December 1978, pp.
	508-517.

[KEY81]	Keys, Robert G., "Cubic Convolution Interpolation for Digital Image
	Processing", IEEE Trans. Acoustics, Speech, and Signal Processing,
	Vol. ASSP-29, No. 6, December 1981, pp. 1153-1160.

[MIT88]	Mitchell, Don P. and Netravali, Arun N., "Reconstruction Filters in
	Computer Graphics", Computer Graphics, Vol. 22, No. 4, August 1988,
	pp. 221-228.

[PAR83]	Park, Stephen K. and Schowengerdt, Robert A., "Image Reconstruction by
	Parametric Cubic Convolution", Computer Vision, Graphics and Image
	Processing", Vol. 23, No. 3, September 1983, pp. 258-272.
.page
HISTORY:

Written by:		R. Mortensen
Cognizant Programmer:	R. Mortensen

Revisions:
	Initial Version		7-Oct-88
.LEVEL1
.VARIABLE INP
Input Vicar image file.
.VARIABLE OUT
Output Vicar image file.
.VARIABLE SIZE
Sub-area of input image to
be zoomed. (SL,SS,NL,NS).
.VARIABLE SL
Starting line of sub-area.
.VARIABLE SS
Starting sample of sub-area.
.VARIABLE NL
Number of lines in sub-area.
.VARIABLE NS
Number of samples in sub-area.
.VARIABLE ZOOM
Integer ZOOM factor(s).
(LZOOM,SZOOM) -- if SZOOM
is omitted, LZOOM is used.
.VARIABLE B
Parameter to control CUBIC
spline function. 
(See HELP *)
.VARIABLE C
Parameter to control CUBIC
spline function.
(See HELP *)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstbicubic.pdf
procedure
refgbl $autousage
body
let $autousage="none"
write ""
write " First a BYTE image is GEN'ed and sized using SIZE and BICUBIC."
write " The results are compared using F2 and HIST."
write ""
gen a 10 10 linc=10 sinc=10 ival=30
size a b zoom=5 scale=1
bicubic a c zoom=5
f2 (b,c) d func="in1-in2+128"
write ""
write " The result of this HIST should be that all values are 128."
write ""
hist d 'nohist
!
write ""
write " Next a HALFWORD image is GEN'ed and tested in the same manner."
write " This time a sub-area is also tested."
write ""
gen a 10 10 linc=100 sinc=100 ival=-500 'half
write " TEST NOTE: The following command currently (6/28/94) fails:"
write "                  size a b area=(2 2 9 9) zoom=5 scale=1 "
write " As a Work around the above is replaced with: "
write "                  size a b (2 2 9 9) zoom=5 scale=1"
!size a b area=(2 2 9 9) zoom=5 scale=1 "
size a b (2 2 9 9) zoom=5 scale=1
bicubic a c ss=2 sl=2 zoom=5
f2 (b,c) d func="in1-in2"
write ""
write " The result of this HIST should be that all values are 0."
write ""
hist d 'nohist
!
write ""
write " Next a FULLWORD image is GEN'ed and and tested in the same manner."
write " Another sub-area is tested."
write ""
gen a 10 10 linc=1000 sinc=1000 ival=-5000 'full
write " TEST NOTE: The following command currently (6/28/94) fails:"
write "               size a b area=(3,4,7,5) lzoom=5 szoom=7 scale=1"
write " As a Work around the above is replaced with: "
!size a b area=(3,4,7,5) lzoom=5 szoom=7 scale=1
write "               size a b (3,4,7,5) lzoom=5 szoom=7 scale=1"
size a b (3,4,7,5) lzoom=5 szoom=7 scale=1
bicubic a c (3,4,7,5) zoom=(5,7)
f2 (b,c) d func="in1-in2"
write ""
write " The result of this HIST should be that all values are 0."
write ""
write " TEST NOTE: Currently (6/28/94) the released version of HIST"
write "      causes an arithmetic fault, as a WORK AROUND a modified"
write "      version of HIST has been compiled into the local directory"
write ""
hist d 'nohist
!
write ""
write " Next a REAL image is GEN'ed and tested in the same manner."
write ""
gen a 10 10 linc=0.1 sinc=0.1 ival=0 'real
size a b zoom=5 scale=1
bicubic a c zoom=5
f2 (b,c) d func="in1-in2"
write ""
write " The result of this HIST should be that all values are near 0."
write " (An order of magnitude near E-7 should be fine.)"
write ""
hist d 'nohist
!
write " DOUBLE images cannot be tested in this manner."
!write ""
!write " Next a DOUBLE image is GEN'ed and tested in the same manner."
!write ""
!gen a 10 10 linc=0.1 sinc=0.1 ival=0 'doub
!size a b zoom=5 scale=1	! SIZE does not work on DOUB images
!bicubic a c zoom=5
!f2 (b,c) d func="in1-in2"
!write ""
!write " The result of this HIST should be that all values are near 0."
!write " (An order of magnitude near E-7 should be fine.)"
!write ""
!hist d 'nohist
end-proc
$ Return
$!#############################################################################
