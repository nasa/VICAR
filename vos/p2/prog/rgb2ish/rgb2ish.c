#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "vicmain_c.h"

void rgb2ish (double R, double G, double B, double * I, double * S, double * H);
void ish2rgb (double I, double S, double H, double * R, double * G, double * B);

/*
  program rgb2ish

  1) Reads byte, half or full word VICAR image
  2) Writes same, with RGB/ISH conversion
  3) Conversion direction depends on "reverse" parm
  4) Negative values can be forced to zero

  Parameters:
  inp - the name of the input file
  out - the name of the output file
  reverse - the direction flag (0=> rgb2ish; !0=> ish2rgb)
  noneg - flag to replace negative values with zero.

*/

void main44(void)
{
  int nl, ns;
  int parmct, parmdf;
  int status;
  char inpfilename [3] [99];
  char outfilename [3] [99];
  int reverse, noneg;
  int inpunit [3];
  int outunit [3];
  int tmpnl, tmpns;
  char fmtStr [100];
  char tmpFmtStr [100];
  int i;
  char msgBuf [1000];
  char * tmpVicNameIn [3] = {0, 0, 0}, * tmpVicNameOut [3] = {0, 0, 0};

  zifmessage ("rgb2ish version 2016-09-12");

  /* fetch params */
  zvparm ("inp", inpfilename, &parmct, &parmdf, 3, 99);
  zvparm ("out", outfilename, &parmct, &parmdf, 3, 99);
  zvp ("reverse", &reverse, &parmct);
  zvp ("noneg", &noneg, &parmct);


  /* open input files and get nl, ns, format */
  for (i = 0; i < 3; i ++) {
    if (zvunit (&inpunit[i], "INP", i + 1, NULL) != 1) {
      sprintf (msgBuf, "zvunit failed on input \"%s\"", inpfilename [i]);
      zmabend (msgBuf);	/* die */
    }

    if (zvopen (inpunit [i], "OPEN_ACT", "SA", "IO_ACT", "SA",NULL) != 1) {
      sprintf (msgBuf, "zvopen failed on input \"%s\"", inpfilename [i]);
      zmabend (msgBuf);	/* die */
    }
  }

  /* make sure that the three input files are the same shape and pixel size */
  zvget (inpunit[0], "NL", &nl, NULL);
  zvget (inpunit[0], "NS", &ns, NULL);
  zvget(inpunit[0], "FORMAT", fmtStr, NULL);

  if (strcmp (fmtStr, "BYTE") && strcmp (fmtStr, "HALF"))
    zmabend ("Input must be BYTE or HALF word files");

  zvget (inpunit [1], "NL", &tmpnl, NULL);
  zvget (inpunit [1], "NS", &tmpns, NULL);
  zvget(inpunit[1], "FORMAT", tmpFmtStr, NULL);

  if (tmpnl != nl || tmpns != ns)
    zmabend ("Input file 2 is a different shape than file 1");
  if (strcmp (tmpFmtStr, fmtStr))
    zmabend ("Input file 2 has a different pixel type than file 1");

  zvget (inpunit [2], "NL", &tmpnl, NULL);
  zvget (inpunit [2], "NS", &tmpns, NULL);
  zvget(inpunit[2], "FORMAT", tmpFmtStr, NULL);

  if (tmpnl != nl || tmpns != ns)
    zmabend ("Input file 3 is a different shape than file 1");
  if (strcmp (tmpFmtStr, fmtStr))
    zmabend ("Input file 3 has a different pixel type than file 1");

  /* create output files */
  for (i = 0; i < 3; i ++) {
    if (zvunit (&outunit[i], "OUT", i + 1, NULL) != 1) {
      sprintf (msgBuf, "zvunit failed on output \"%s\"", outfilename [i]);
      zmabend (msgBuf);	/* die */
    }

    if (zvopen (outunit [i], "U_NL", nl, "U_NS", ns, "O_FORMAT", fmtStr,
		"OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",NULL) != 1) {
      sprintf (msgBuf, "zvopen failed on output \"%s\"", outfilename [i]);
      zmabend (msgBuf);	/* die */
    }
  }

  if (! strcmp (fmtStr, "BYTE")) {
    {
      /* allocate buffers */
      unsigned char * inpLineBufRI = (unsigned char *) malloc (ns);
      unsigned char * inpLineBufGS = (unsigned char *) malloc (ns);
      unsigned char * inpLineBufBH = (unsigned char *) malloc (ns);
      unsigned char * outLineBufRI = (unsigned char *) malloc (ns);
      unsigned char * outLineBufGS = (unsigned char *) malloc (ns);
      unsigned char * outLineBufBH = (unsigned char *) malloc (ns);
      int line, pixel;
      double R, G, B, I, S, H;

      for (line = 0; line < nl; line ++) {
	/* read in a line */
	if ((status = zvread (inpunit [0], inpLineBufRI, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on first channel");
	if ((status = zvread (inpunit [1], inpLineBufGS, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on second channel");
	if ((status = zvread (inpunit [2], inpLineBufBH, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on third channel");

	for (pixel = 0; pixel < ns; pixel ++) {
	  /* convert */
	  if (reverse) {	/* ish2rgb */
	    I = inpLineBufRI [pixel] / (255.0 / 3.0);
	    S = inpLineBufGS [pixel] / 255.0;
	    H = inpLineBufBH [pixel] / (255.0 / 3.0);

	    ish2rgb (I, S, H, &R, &G, &B);

	    outLineBufRI [pixel] = R * 255.0;
	    outLineBufGS [pixel] = G * 255.0;
	    outLineBufBH [pixel] = B * 255.0;
	  } else {		/* rgb2ish */
	    R = inpLineBufRI [pixel] / 255.0;
	    G = inpLineBufGS [pixel] / 255.0;
	    B = inpLineBufBH [pixel] / 255.0;

	    rgb2ish (R, G, B, &I, &S, &H);

	    outLineBufRI [pixel] = I * (255.0 / 3.0) + 0.5; /* add 0.5 to round, vs. truncate */
	    outLineBufGS [pixel] = S * 255.0 + 0.5;
	    outLineBufBH [pixel] = H * (255.0 / 3.0) + 0.5;
	  }
	}

	/* write out a line */
	zvwrit (outunit [0], outLineBufRI,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [1], outLineBufGS,
		"LINE", line + 1,
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [2], outLineBufBH,
		"LINE", line + 1,
		"SAMP", 1,
		"NSAMPS", ns, NULL);
      }
    }
  } else if (! strcmp (fmtStr, "HALF")) {
    {
      /* allocate buffers */
      short * inpLineBufRI = (short *) malloc (ns * 2);
      short * inpLineBufGS = (short *) malloc (ns * 2);
      short * inpLineBufBH = (short *) malloc (ns * 2);
      short * outLineBufRI = (short *) malloc (ns * 2);
      short * outLineBufGS = (short *) malloc (ns * 2);
      short * outLineBufBH = (short *) malloc (ns * 2);
      int line, pixel;
      double R, G, B, I, S, H;

      for (line = 0; line < nl; line ++) {
	/* read in a line */
	if ((status = zvread (inpunit [0], inpLineBufRI, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on first channel");
	if ((status = zvread (inpunit [1], inpLineBufGS, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on second channel");
	if ((status = zvread (inpunit [2], inpLineBufBH, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on third channel");

	for (pixel = 0; pixel < ns; pixel ++) {
	  /* force non negative if necessary since VICAR half word is signed, unlike VICAR byte */
	  if (noneg) {
	    if (inpLineBufRI [pixel] < 0)
	      inpLineBufRI [pixel] = 0;
	    if (inpLineBufGS [pixel] < 0)
	      inpLineBufGS [pixel] = 0;
	    if (inpLineBufBH [pixel] < 0)
	      inpLineBufBH [pixel] = 0;
	  }

	  /* convert */
	  if (reverse) {
	    I = inpLineBufRI [pixel] / (32767.0 / 3.0);
	    S = inpLineBufGS [pixel] / 32767.0;
	    H = inpLineBufBH [pixel] / (32767.0 / 3.0);
	    ish2rgb (I, S, H, &R, &G, &B);
	    outLineBufRI [pixel] = R * 32767.0;
	    outLineBufGS [pixel] = G * 32767.0;
	    outLineBufBH [pixel] = B * 32767.0;
	  } else {
	    R = inpLineBufRI [pixel] / 32767.0;
	    G = inpLineBufGS [pixel] / 32767.0;
	    B = inpLineBufBH [pixel] / 32767.0;
	    rgb2ish (R, G, B, &I, &S, &H);
	    outLineBufRI [pixel] = I * (32767.0 / 3.0) + 0.5; /* add 0.5 to round, vs. truncate */
	    outLineBufGS [pixel] = S * 32767.0 + 0.5;
	    outLineBufBH [pixel] = H * (32767.0 / 3.0) + 0.5;
	  }
	}

	/* write out a line */
	zvwrit (outunit [0], outLineBufRI,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [1], outLineBufGS,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [2], outLineBufBH,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
      }
    }
  } else
    zmabend ("Input must be BYTE or HALF word files");

  /* done with VICAR images */
  for (i = 0; i < 3; i ++) {
    zvclose (inpunit [i], NULL);
    zvclose (outunit [i], NULL);
  }
}

/* Source http://ij.ms3d.de/pdf/ihs_transforms.pdf */

#include <stdlib.h>
#include <math.h>
#include <string.h>

void rgb2ish (double R, double G, double B, double *I, double *S, double *H) {
  if (R < 0.0 || R > 1.0 || G < 0.0 || G > 1.0 || B < 0.0 || B > 1.0)
    zmabend ("rgb value out of range!");

  *I = R + G + B;

  if (*I == 0.0) {
    *S = 0.0;
    *H = 0.0;
    return;
  }

  if (R == G && G == B) {
    *S = 0.0;
    *H = 0.0;
    return;
  }

  if (B <= R && B <= G)
    *H = (G - B) / (*I - 3 * B);
  else if (R <= G && R <= B)
    *H = (B - R) / (*I - 3 * R) + 1;
  else
    *H = (R - G) / (*I - 3 * G) + 2;

  if (*H < 1)
    *S = (*I - 3 * B) / *I;
  else if (*H < 2)
    *S = (*I - 3 * R) / *I;
  else
    *S = (*I - 3 * G) / *I;
}

void ish2rgb (double I, double S, double H, double *R, double *G, double *B) {
  double domainOffset = 0.0;

  if (I < 0.0 || I > 3.0 || S < 0.0 || S > 1.0 || H < 0.0 || H > 3.0)
    zmabend ("ish value out of range!");

  if (H < 1) {
    *R = I * (1 + 2 * S - 3 * S * H) / 3.0;
    *G = I * (1 - S + 3 * S * H) / 3.0;
    *B = I * (1 - S) / 3.0;
  } else if (H < 2) {
    *R = I * (1 - S) / 3.0;
    *G = I * (1 + 2 * S - 3 * S * (H - 1)) / 3.0;
    *B = I * (1 - S + 3 * S * (H - 1)) / 3.0;
  } else {
    *R = I * (1 - S + 3 * S * (H - 2)) / 3.0;
    *G = I * (1 - S) / 3.0;
    *B = I * (1 + 2 * S - 3 * S * (H - 2)) / 3.0;
  }
}
