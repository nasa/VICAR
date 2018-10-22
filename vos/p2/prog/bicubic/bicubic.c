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

