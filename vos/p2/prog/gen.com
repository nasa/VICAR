$!****************************************************************************
$!
$! Build proc for MIPL module gen
$! VPACK Version 1.9, Monday, December 07, 2009, 16:20:01
$!
$! Execute by entering:		$ @gen
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
$ write sys$output "*** module gen ***"
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
$ write sys$output "Invalid argument given to gen.com file -- ", primary
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
$   if F$SEARCH("gen.imake") .nes. ""
$   then
$      vimake gen
$      purge gen.bld
$   else
$      if F$SEARCH("gen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gen.bld "STD"
$   else
$      @gen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gen.com -mixed -
	-s gen.c -
	-i gen.imake -
	-p gen.pdf -
	-t tstgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gen.imake
#define PROGRAM gen

#define MODULE_LIST gen.c

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE

/* There is a bug in the C compiler optimizer on the HP.  Optimization	*/
/* level 2 (the default) failes, but 1 succeeds.  To demonstrate the	*/
/* problem (to see if it has been fixed), issue the command		*/
/* gen a 3 3 'doub							*/
/* Then "list a".  The first pixel (line 1, sample 1) should be 0; if	*/
/* this bug occurs it shows up as 1.					*/

#if HP700_ARCH
#define C_OPTIONS +O1
#endif

$ Return
$!#############################################################################
$PDF_File:
$ create gen.pdf
PROCESS HELP=*
PARM OUT    TYPE=STRING		               DEFAULT=GEN
PARM NL     TYPE=INTEGER		       DEFAULT=10
PARM NS     TYPE=INTEGER		       DEFAULT=10
PARM NB	    TYPE=INTEGER		       DEFAULT=1
PARM FORMAT TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL,DOUB,COMP,REAL4,REAL8) +
 DEFAULT=BYTE
PARM MODULO TYPE=REAL COUNT=1:2                DEFAULT=(0.0,0.0)
PARM LINC   TYPE=REAL COUNT=1:2                DEFAULT=(1.0,1.0)
PARM SINC   TYPE=REAL COUNT=1:2                DEFAULT=(1.0,1.0)
PARM BINC   TYPE=REAL COUNT=1:2		       DEFAULT=(1.0,1.0)
PARM IVAL   TYPE=REAL COUNT=1:2                DEFAULT=(0.0,0.0)
PARM ORG    TYPE=KEYWORD  COUNT=0:1 VALID=(BSQ,BIL,BIP) DEFAULT=BSQ

!# annot function="Generating Synthetic Images"
!# annot keywords=("3-D picture",line,samp,band)
END-PROC
.TITLE
Create synthetic (ramp) image
.HELP

PURPOSE:

 GEN  generates a 3 dimensional picture, given an initial value,
 a line increment, a sample increment, and a band increment.  It
 is  used  primarily  to  format  disk  data  sets  and to begin
 generation of test pictures.

OPERATION:

 The  image  file  generated is in three dimensions, LINE, SAMP,
 and BAND.  The output DN at SAMP I, LINE J, BAND K will be:

 (IVAL + (I-1)*SINC + (J-1)*LINC + (K-1)*BINC) (MODULO MOD)
.page
 If  floating point output is specified, then the calculation is
 carried  out  using  floating point arithmetic.  The parameters
 IVAL, SINC, LINC, BINC, and MOD will be treated as real numbers
 in the program.

EXECUTION:

	GEN can be invoked by typing
		GEN OUT NL NS NB PARAMS
	where PARAMS consists of the following parameters:
		FORMAT	IVAL 	SINC     LINC     
		BINC	MOD
	Each is described in their respective  parameter section.
.page
TIMING:
	 None available for the VAX

WRITTEN BY: H. J. Frieden      12/22/70

COGNIZANT PROGRAMMER: R. G. Deen   2/12/90

REVISION: New

.LEVEL1
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of Samples.
.VARIABLE NB
Number of Bands.
.VARIABLE OUT
Output filename.
.VARIABLE FORMAT
Data format. Valid: 
BYTE,HALF,FULL,
REAL,DOUB,COMP.
.VARIABLE IVAL
Initial value.
.VARIABLE SINC
Sample increment.
.VARIABLE LINC
Line increment.
.VARIABLE BINC
Band increment.
.VARIABLE MODULO
Modulo.
.VARIABLE ORG
Desired file
organization.
.LEVEL2
.VARIABLE NL
 NL  (integer)  specifies  the  size  of  the  image in the line
 direction.

 Default is 10.
.VARIABLE NS
 NS  (integer)  specifies  the  size  of the image in the sample
 direction, i.e., the number of samples per line.

 Default is 10.
.VARIABLE NB
 NB  (integer)  specifies  the  size  of  the  image in the band
 direction, or the number of image planes in the file.  The file
 size is NL x NS x NB pixels.

 Default is one.
.VARIABLE OUT
 OUT  is the standard VICAR output filename.  It is a string of
 form   "name.type",where  "name"  is  a  string  of  up  to  39
 alphanumeric  characters,  starting  with  an  alphabetic,  and
 "type"   is  an  optional  string  of  up  to  39  alphanumeric
 characters.

 Default is GEN.
.VARIABLE FORMAT
 This  parameter  specifies  the data format.  If it is omitted,
 BYTE (unsigned INTEGER*1) data is assumed.  The number of bytes below
 is typical for VAX and Sun-type architechtures; it may be different
 on other kinds of machines.

 Valid values are:

 BYTE:  one byte unsigned integer (0 -> 255) (INTEGER*1)
 HALF:  two byte signed integer (-32768 -> +32767) (INTEGER*2)
 FULL:  four byte signed integer (INTEGER*4)
 REAL:  four byte (single precision) floating point (REAL*4)
 DOUB:  eight byte (double precision) floating point (REAL*8)
 COMP:  two four-byte (single precision) floating point numbers,
        in the order (real,imaginary).
 In  addition,  in  order  to  maintain compatability with older
 versions,  REAL4  and REAL8 are allowed as alternatives to REAL
 and DOUB respectively.  REAL4 and REAL8 should not generally be
 used, however, as they are being phased out.
.VARI IVAL
 The  value  of  IVAL  (integer  or floating point) is the value
 assigned to the initial DN at line 1, sample 1, band 1.  For
 COMPlex data only, IVAL can have both a real and an imaginary part.
 Normally, only the first (real) value is given.

 Default is 0.0 (and 0.0 imaginary).
.VARIABLE SINC
 The value of SINC (integer or floating point) is the horizontal
 increment,  i.e., on a given line in a given band:

 (sample i+1) = (sample i) + SINC.

 For COMPlex data only, SINC can have both a real and an imaginary part.
 Normally, only the first (real) value is given.

 Default is 1.0 (and 1.0 imaginary).
.VARIABLE LINC
 The  value  of LINC (integer or floating point) is the vertical
 increment, i.e., for a given band:

 (line i+1, sample j) = (line i, sample j) + LINC.

 For COMPlex data only, LINC can have both a real and an imaginary part.
 Normally, only the first (real) value is given.

 Default is 1.0 (and 1.0 imaginary).
.VARIABLE BINC
 The  value  of  BINC  is  used  as  an  increment  in  the BAND
 direction, so that

 (line i, sample j, band k+1) = (line i, sample j, band k) + BINC

 For COMPlex data only, BINC can have both a real and an imaginary part.
 Normally, only the first (real) value is given.

 Default is 1.0 (and 1.0 imaginary).
.VARIABLE MODULO
 The  value  of  MODULO  (integer  or floating point) is used to
 limit  the  output  values.   The  grey level values are output
 modulo  MODULO.   (Default is MODULO = 256 for byte pictures, =
 2**16 for halfword, = 2**32 for fullword.)

 For COMPlex data only, MODULO can have both a real and an imaginary part.
 Normally, only the first (real) value is given.

.VARIABLE ORG
 ORG  indicates  the organization of the output file.  The valid
 values are:

 BSQ -- Band Sequential
 BIL -- Band Interleaved by Line
 BIP -- Band Interleaved by Pixel

 For more info, see the example on the following page.

 The default organization is BSQ.

.page
 Example:

 Using GEN to generate an image file as follows,

 GEN A NL=3 NS=4 NB=3 IVAL=1 SINC=0 LINC=0 BINC=1 ,

 the  resultant  image  would  be  a  series  of  records  whose
 organization  would  depend  on the ORG keyword, and the result
 would be:
					123
	1111		1111		123
	1111		2222		123
	1111		3333		123
	2222		1111		123
 BSQ:	2222	BIL:	2222	BIP:	123
	2222		3333		123
	3333		1111		123
	3333		2222		123
	3333		3333		123
					123
					123
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgen.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!  THIS IS A TEST FILE FOR GEN
! Throw in a few label-lists to check the output labels
!THESE FOLLOWING TESTS SHOW THE USE OF GEN WITH FLOATING POINT NUMBERS
!AND WITH ONLY 2D DATA
GEN TGEN1 10 10 IVAL=50.5 SINC=10.5 LINC=-20.5
LIST TGEN1
LABEL-LIST TGEN1
GEN TGEN1 10 20 'HALF IVAL=-100.5 SINC=0.0 LINC=0.0
LIST TGEN1
GEN TGEN3 10 10 'REAL IVAL=0.0 SINC=1.5 LINC=25.
LIST TGEN3
LABEL-LIST TGEN3
! Test double-precision parameters
GEN TGEN3 10 10 'DOUB LINC=.1 SINC=.01
LIST TGEN3 NDIGITS=16
LABEL-LIST TGEN3
!THESE TESTS SHOW THE USE OF GEN WITH VALUES AS INTEGERS
GEN TGEN3 10 10 'FULL IVAL=0 SINC=20 LINC=-3
LIST TGEN3
GEN TGEN4 10 10 'DOUB IVAL=-1000 SINC=20 LINC=3
LIST TGEN4
!THESE TESTS SHOW THE USE OF MOD
GEN TGEN1 10 10 MOD=10 LINC=3 SINC=2
LIST TGEN1
LABEL-LIST TGEN1
GEN TGEN1 10 10 MOD=1.5
LIST TGEN1
! Complex numbers
GEN TGEN1 10 10 IVAL=(10,-10) SINC=.5 LINC=(10,-10) 'COMP
LIST TGEN1
LABEL-LIST TGEN1
!THESE FOLLOWING TEST SHOW THE USE OF GEN WITH THE DIFFERENT OUTPUT OPTIONS
!AND THE DEFAULT VALUES
GEN TGEN3 10 10 
LIST TGEN3
GEN TGEN3 10 10 'REAL4
LIST TGEN3
GEN TGEN4 10 10 'DOUB
LIST TGEN4
end-proc
$ Return
$!#############################################################################
