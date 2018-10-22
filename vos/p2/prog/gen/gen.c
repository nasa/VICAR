#include <math.h>
#include "vicmain_c"
#include <string.h>
#include <stdlib.h>

#define TYPE_BYTE       0
#define TYPE_HALF       1
#define TYPE_FULL       2
#define TYPE_REAL       3
#define TYPE_DOUB       4
#define TYPE_COMP       5

#define EQUAL(x,y)      (strcmp(x,y)==0)

/************************************************************************/
/* Program GEN								*/
/************************************************************************/
/*
/* 24 SEP 02   ...RGD...  Changed parameters to double to avoid		*/
/*                        precision loss problems			*/
/* 13 FEB 90   ...RGD...  Rewritten in C with Unix compatibility;	*/
/*			  removed all sublib references, added COMPlex.	*/
/* 23 JAN 85   ...HBD...  Handle REAL8 properly				*/
/*  84-7-18    ...LWK...  fixed bugs when sinc=0 & format=REAL		*/
/*  8 JUN 84   ...HBD...  Converted to Vicar2, rewrote GEN		*/
/* 17 AUG 83   ...HBD...  ALLOW FLOATING POINT NUMBERS ON VAX VERSION	*/
/*   82-12     ...LWK...  convert to Vax/Vicar1				*/
/* 29 MAR 82   ...GMY...  INCREASE MAX OUTPUT SAMPLES TO 60K		*/
/* 12 APR 81   ...GMY...  ALLOW FLOATING POINT NUMBERS			*/

/* This version of GEN does not distinguish between integer and real	*/
/* number SINC, LINC, BINC, IVAL, and MODULO values. All values are now	*/
/* processed as real numbers. ...HBD					*/

void main44(void)
{
    int unit;
    int data_type;
    int ns, nl, nb;
    double modulo[2], sinc[2], linc[2], binc[2], ival[2];
    int cnt, def, nitems, temp;
    double ftemp;
    char format[15], org[15], msgbuf[80];

    zvmessage("GEN Version 6", "");

    zveaction("sa","");

    zvunit(&unit, "out", 1, NULL);

    /* Determine data type */

    zvp("FORMAT", format, &cnt);

    if (EQUAL(format, "REAL4")) strcpy(format, "REAL");
    if (EQUAL(format, "REAL8")) strcpy(format, "DOUB");

    if      (EQUAL(format, "BYTE")) data_type = TYPE_BYTE;
    else if (EQUAL(format, "HALF")) data_type = TYPE_HALF;
    else if (EQUAL(format, "FULL")) data_type = TYPE_FULL;
    else if (EQUAL(format, "REAL")) data_type = TYPE_REAL;
    else if (EQUAL(format, "DOUB")) data_type = TYPE_DOUB;
    else if (EQUAL(format, "COMP")) data_type = TYPE_COMP;
    else {
	sprintf(msgbuf, "'%s' is not a valid data format.", format);
	zvmessage(msgbuf, "GEN-BADFMT");
	zabend();
    }

    /* Open output file with specified values */

    zvp("NL", &nl, &cnt);
    zvp("NS", &ns, &cnt);
    zvp("NB", &nb, &cnt);
    zvp("ORG", org, &cnt);

    zvopen(unit, "U_FORMAT", format, "O_FORMAT", format, "OP", "write",
	   "U_NL", nl, "U_NS", ns, "U_NB", nb, "U_ORG", org, NULL);

    /* Get MODULO value and check if valid */

    zvparmd("MODULO", modulo, &cnt, &def, 2, 0);
    if (modulo[0] < 0.0) {
	zvmessage("Modulo value cannot be negative", "GEN-BADPARM");
	zabend();
    }
    if (data_type == TYPE_COMP) {
	if (cnt > 1) {
	    if (modulo[1] < 0.0) {
		zvmessage("Modulo value cannot be negative", "GEN-BADPARM");
		zabend();
	    }
	}
	else
	    modulo[1] = 0.0;
    }

    /* Get IVAL, SINC, LINC, and BINC values and place in history label */

    nitems = 1;			/* # of items to put in the label */

    zvparmd("IVAL", ival, &cnt, &def, 2, 0);
    if (modulo[0] > 0.0)
	ival[0] = fmod(ival[0], modulo[0]);
    if (data_type == TYPE_COMP) {
	nitems = 2;
	if (cnt <= 1)
	    ival[1] = 0.0;
	if (modulo[1] > 0.0)
	    ival[1] = fmod(ival[1], modulo[1]);
    }
    zladd(unit, "history", "IVAL", ival,
	  "format", "doub", "nelement", nitems, NULL);

    zvparmd("SINC", sinc, &cnt, &def, 2, 0);
    if (data_type == TYPE_COMP && cnt <= 1)
	sinc[1] = 1.0;
    zladd(unit, "history", "SINC", sinc,
	  "format", "doub", "nelement", nitems, NULL);

    zvparmd("LINC", linc, &cnt, &def, 2, 0);
    if (data_type == TYPE_COMP && cnt <= 1)
	linc[1] = 1.0;
    zladd(unit, "history", "LINC", linc,
	  "format", "doub", "nelement", nitems, NULL);

    zvparmd("BINC", binc, &cnt, &def, 2, 0);
    if (data_type == TYPE_COMP && cnt <= 1)
	binc[1] = 1.0;
    zladd(unit, "history", "BINC", binc,
	  "format", "doub", "nelement", nitems, NULL);

    zladd(unit, "history", "MODULO", modulo,
	  "format", "doub", "nelement", nitems, NULL);

    /* Now a little trick... Rearrange NL,NS,NB, LINC,SINC,BINC according */
    /* to the given ORG.  From here on, SAMP refers to slice1, the inner- */
    /* most dimension, regardless of the actual file organization.  Like- */
    /* wise, LINE is slice2, and BAND is slice3.			  */

    if (EQUAL(org, "BIL") || EQUAL(org, "BIP")) {
	temp = nl;
	nl = nb;
	nb = temp;
	ftemp = linc[0];
	linc[0] = binc[0];
	binc[0] = ftemp;
	ftemp = linc[1];
	linc[1] = binc[1];
	binc[1] = ftemp;
    }
    if (EQUAL(org, "BIP")) {
	temp = nl;			/* used to be nb above */
	nl = ns;
	ns = temp;
	ftemp = linc[0];
	linc[0] = sinc[0];
	sinc[0] = ftemp;
	ftemp = linc[1];
	linc[1] = sinc[1];
	sinc[1] = ftemp;
    }

    /* Call the appropriate GEN routine based on the data type.  This	*/
    /* could be done with only one (double) loop, using the u_format	*/
    /* data type conversion facilities of the RTL, but it is much more	*/
    /* efficient to generate the correct data type in the first place.	*/

    switch(data_type) {

	case TYPE_BYTE:
	    gen_byte(unit, ns, nl, nb,
		     (double)sinc[0], (double)linc[0], (double)binc[0],
		     (double)ival[0], (double)modulo[0]);
	    break;

	case TYPE_HALF:
	    gen_half(unit, ns, nl, nb,
		     (double)sinc[0], (double)linc[0], (double)binc[0],
		     (double)ival[0], (double)modulo[0]);
	    break;

	case TYPE_FULL:
	    gen_full(unit, ns, nl, nb,
		     (double)sinc[0], (double)linc[0], (double)binc[0],
		     (double)ival[0], (double)modulo[0]);
	    break;

	case TYPE_REAL:
	    gen_real(unit, ns, nl, nb,
		     (double)sinc[0], (double)linc[0], (double)binc[0],
		     (double)ival[0], (double)modulo[0]);
	    break;

	case TYPE_DOUB:
	    gen_doub(unit, ns, nl, nb,
		     (double)sinc[0], (double)linc[0], (double)binc[0],
		     (double)ival[0], (double)modulo[0]);
	    break;

	case TYPE_COMP:
	    gen_comp(unit, ns, nl, nb,
		     sinc, linc, binc, ival, modulo);
	    break;

	default:
	    zvmessage("GEN Internal error!  Notify cognizant programmer.",
			"GEN-INTERR");
	    zabend();
    }

    zvclose(unit, NULL);
    zvmessage("GEN task completed", "");
}

/************************************************************************/
/* The following (rather involved) macro implements the nested GEN	*/
/* loops for any given data type.  It is a macro because the code for	*/
/* each data type is exactly the same, except for the type declaration,	*/
/* sizeof statement, and type cast when storing the value in the buffer.*/
/* It would be extremely inefficient to have a switch statement in the	*/
/* inner loop for type conversion.  It would also not be good to repeat	*/
/* the same code five times, as it becomes unmaintainable.		*/
/* Note that TYPE_COMP does it's own loops because two operations (real	*/
/* and imaginary) are required at each step.				*/
/*									*/
/* There is a bug in the Linux compiler which causes the following to	*/
/* fail:  double x = 32770.0; short int s = (short int)x;		*/
/* The result of the cast is 0 instead of x mod 32768 like every other	*/
/* compiler.  To get around this, we cast twice... once to int, then	*/
/* again to short int or char (it happens on both).  To make this easy,	*/
/* we just add a CAST parameter to GEN_LOOP which is the type cast to	*/
/* use (you don't want to cast float/double to int first!).  This	*/
/* double casting doesn't hurt anywhere else, so no need to make it	*/
/* platform conditional.						*/
/************************************************************************/

#define GEN_LOOP(TYPE, CAST)	\
    int s, l, b;							\
    double sval, lval, bval;						\
    TYPE *buf, *pbuf;							\
									\
    buf = (TYPE *)malloc(ns * sizeof(TYPE));					\
    if (buf == NULL) {							\
	zvmessage("Insufficient memory to allocate internal buffer",	\
		  "GEN-INSUFMEM");					\
	zabend();							\
    }									\
									\
    bval = ival;			/* band starting value */	\
    for (b = 0; b < nb; b++) {						\
	lval = bval;			/* line starting value */	\
	for (l = 0; l < nl; l++) {					\
	    sval = lval;		/* sample value */		\
	    if (linc != 0.0 || l == 0) {  /* don't redo loop if no change */ \
		pbuf = buf;						\
		if (sinc == 0.0) {	/* for speed */			\
		    for (s = 0; s < ns; s++)				\
			*pbuf++ = CAST sval;				\
		}							\
		else if (modulo <= 0.0) {	/* no modulo, do it faster */ \
		    for (s = 0; s < ns; s++) {				\
			*pbuf++ = CAST sval;				\
			sval += sinc;					\
		    }							\
		}							\
		else {		/* modulo & sinc, do it the slow way */	\
		    for (s = 0; s < ns; s++) {				\
			*pbuf++ = CAST sval;				\
			sval += sinc;					\
			sval = fmod(sval, modulo);			\
		    }							\
		}							\
	    }								\
	    zvwrit(unit, buf, NULL);					\
	    lval += linc;						\
	    if (modulo > 0.0)						\
		lval = fmod(lval, modulo);				\
	}								\
	bval += binc;							\
	if (modulo > 0.0)						\
	    bval = fmod(lval, modulo);					\
    }									\
    free(buf);

/************************************************************************/
/* BYTE data								*/
/************************************************************************/

gen_byte(unit, ns, nl, nb, sinc, linc, binc, ival, modulo)
int unit;
int ns, nl, nb;
double sinc, linc, binc;
double ival, modulo;
{

    GEN_LOOP(unsigned char, (unsigned char)(int) );
}

/************************************************************************/
/* HALF data								*/
/************************************************************************/

gen_half(unit, ns, nl, nb, sinc, linc, binc, ival, modulo)
int unit;
int ns, nl, nb;
double sinc, linc, binc;
double ival, modulo;
{

    GEN_LOOP(short int, (short int)(int) );
}

/************************************************************************/
/* FULL data								*/
/************************************************************************/

gen_full(unit, ns, nl, nb, sinc, linc, binc, ival, modulo)
int unit;
int ns, nl, nb;
double sinc, linc, binc;
double ival, modulo;
{

    GEN_LOOP(int, (int) );
}

/************************************************************************/
/* REAL data								*/
/************************************************************************/

gen_real(unit, ns, nl, nb, sinc, linc, binc, ival, modulo)
int unit;
int ns, nl, nb;
double sinc, linc, binc;
double ival, modulo;
{

    GEN_LOOP(float, (float) );
}

/************************************************************************/
/* DOUB data								*/
/************************************************************************/

gen_doub(unit, ns, nl, nb, sinc, linc, binc, ival, modulo)
int unit;
int ns, nl, nb;
double sinc, linc, binc;
double ival, modulo;
{

    GEN_LOOP(double, (double) );
}

/************************************************************************/
/* COMP data								*/
/************************************************************************/

gen_comp(unit, ns, nl, nb, sinc, linc, binc, ival, modulo)
int unit;
int ns, nl, nb;
double sinc[2], linc[2], binc[2];
double ival[2], modulo[2];
{
    int s, l, b;
    float sval[2], lval[2], bval[2];
    float *buf, *pbuf;

    buf = (float *)malloc(ns * sizeof(float) * 2);  /* *2 for (real,imag) */
    if (buf == NULL) {
	zvmessage("Insufficient memory to allocate internal buffer",
		  "GEN-INSUFMEM");
	zabend();
    }

    bval[0] = ival[0];			/* band starting value */
    bval[1] = ival[1];
    for (b = 0; b < nb; b++) {
	lval[0] = bval[0];		/* line starting value */
	lval[1] = bval[1];
	for (l = 0; l < nl; l++) {
	    sval[0] = lval[0];		/* sample value */
	    sval[1] = lval[1];
	    if (linc[0] != 0.0 || linc[1] != 0.0 || l == 0) {   /* no change */
		pbuf = buf;
		for (s = 0; s < ns; s++) {
		    *pbuf++ = sval[0];
		    *pbuf++ = sval[1];
		    sval[0] += sinc[0];
		    sval[1] += sinc[1];
		    if (modulo[0] > 0.0)
			sval[0] = fmod(sval[0], modulo[0]);
		    if (modulo[1] > 0.0)
			sval[1] = fmod(sval[1], modulo[1]);
		}
	    }
	    zvwrit(unit, buf, NULL);
	    lval[0] += linc[0];
	    lval[1] += linc[1];
	    if (modulo[0] > 0.0)
		lval[0] = fmod(lval[0], modulo[0]);
	    if (modulo[1] > 0.0)
		lval[1] = fmod(lval[1], modulo[1]);
	}
	bval[0] += binc[0];
	bval[1] += binc[1];
	if (modulo[0] > 0.0)
	    bval[0] = fmod(lval[0], modulo[0]);
	if (modulo[1] > 0.0)
	    bval[1] = fmod(lval[1], modulo[1]);
    }

    free(buf);
}
