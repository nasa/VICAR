$!****************************************************************************
$!
$! Build proc for MIPL module list
$! VPACK Version 1.9, Monday, December 07, 2009, 16:36:48
$!
$! Execute by entering:		$ @list
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
$ write sys$output "*** module list ***"
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
$ write sys$output "Invalid argument given to list.com file -- ", primary
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
$   if F$SEARCH("list.imake") .nes. ""
$   then
$      vimake list
$      purge list.bld
$   else
$      if F$SEARCH("list.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake list
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @list.bld "STD"
$   else
$      @list.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create list.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack list.com -mixed -
	-s list.c -
	-i list.imake -
	-p list.pdf -
	-t tstlist.pdf tst_real.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create list.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h" 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MTASKS 20			/* Max # of tasks for zlhinfo */
#define LINES_PER_PAGE	53

#define TYPE_BYTE	0
#define TYPE_HALF	1
#define TYPE_FULL	2
#define TYPE_REAL	3
#define TYPE_DOUB	4
#define TYPE_COMP	5
#define TYPE_HEX	6

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif
#define EQUAL(x,y)	(strcmp(x,y)==0)

/************************************************************************/
/* Program LIST                                                         */
/************************************************************************/
/*  83-10 ..ANON... RENAMED FROM PRNT TO AVOID CONFUSION WITH THE	*/
/*                  SUBROUTINE PRNT					*/
/*  84-01 ...LWK... CHANGED 64 (EBCDIC SPACE) TO 32 (ASCII)		*/
/*  84-11 ...LWK... FIXES TO REDUCE QPRINT LINE LENGTH & SUPRESS	*/
/*		   "NO LABELS" MSG. (BOTH SHOULD BE TEMPORARY!)		*/
/*  85-02 ...JRS... CONVERT TO VICAR2					*/
/*  85-04 ...JRS... CHANGE DCODE FROM 9 TO 10 FOR COMPLEX DATA TYPE	*/
/*                  TO BE CONSISTENT WITH OTHER VICAR PROGRAMS		*/
/*  87-04 ...SP.... DELETED NONFUNCTIONAL PRINT AND SHIFT PARAMETERS.	*/
/*  87-06 ...RGD... MAJOR REVISIONS; ADDED 3D FILE CAPABILITY AND	*/
/*		    READING OF UNLABELED FILES.				*/
/*  90-02 ...RGD... Rewritten in C with Unix compatibility; removed all	*/
/*		    sublib references (outcon, prnt, etc.).		*/
/*  95-05 ...BAM... Corrected error with hex dump option.  Disables hex */
/*                  dump for DOUB/COMP.  FR 79143                       */
/*  95-11 ...BAM... added memcpy in MOVX and removed pointer moves      */
/*                                       FR 87132                       */
/*                also, modified tstlist.pdf to use cform rather than c */
/*  1998-06 ..TIH.. Added check for INSIZE parameter when file is not   */
/*                  labeled. Changed tstlist.pdf to conform to UNIX     */
/*                  (see AR-9635)                                       */
/*                  Added NDIGITS parameter to specify the precision    */
/*                  used to display real numbers (REAL, DOUB, COMP)     */
/************************************************************************/

void main44(void)
{
int i, status, unit, dum;
int count, def;
int nlab;
int dump, zero, eject, display, nofeed;
int binc, linc, sinc;
int space;
int shuffle, allzero, empty, tof, newstrip, inzero, newslice3;
int accuracy, nr_digits, col_width; /* # of digits to be displayed = NDIGITS */
int nsi, nli, nbi;		/* # of samples, lines, bands in input image */
int ssin, slin, sbin;		/* user given start sample, line, band       */
int nsin, nlin, nbin;		/* user given # of samples, lines, bands     */
int nsclip, nlclip, nbclip;	/* ns, nl, nb adjusted for specified window  */
int esin, elin, ebin;
int sl1st, sl1n, sl1inc;
int sl2st, sl2n, sl2inc;
int sl3st, sl3n, sl3inc;
int sl3ni;
int sl1end;
int nspl1, nsx, nlx, llx, dumplines;
int ilx, sx, ssout, sstrip, estrip;
int b2, l2;
int slice1, slice2, slice3;
int t1, t2, dt, si, s1, nso, tx;
int oline;
int nl[3];
int itype, jtype;
int psout, psin;
char *buf;			/* pointer to data buffer */
char *n_cmd;
char *sl1_cmd, *sl2_cmd, *sl3_cmd;
char form[11], oform[11];
char orgin[5], orgout[5];
char msgbuf[133];
char dmsg[80], temp[80];
char tasks[MTASKS][10];
int inst[MTASKS];
char user1[10], usern[10];
char date1[33], daten[33];
char *samp_heading, *line_heading, *band_heading;

static char data[][9] = {		/* indexed by jtype */
	"  BYTE  ",
	"HALFWORD",
	"FULLWORD",
	" REAL*4 ",
	" REAL*8 ",
	"COMPLEX ",
	"  HEX   "
};

static int acc2nspl[15] = { 12, 10, 10, 9, 8, 8, 7, 7, 6, 6, 6, 5, 5, 5, 5 };  /* helps map from #digits -> samps per line  */
static int acc2widt[15] = { 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 };  /* #digits -> width of 1 column */
static int start[7]  = {  9, 11, 16, 17, 17, 29,  5 };	/* heading start pos */
static int outlen[7] = {  8,  6, 11, 11, 11, 22,  9 };	/* sl1 heading spacing*/
static int nspl[7]   = { 30, 15, 10, 10, 10,  5, 40 };	/* samps per line */
static int nspace[7] = {  2,  4,  8,  8,  8,  8,  0 };	/* for "display"param */
/*			  B   H   F   R   D   C   H	*/
/*			  Y   A   U   E   O   O   E	*/
/*			  T   L   L   A   U   M   X	*/
/*			  E   F   L   L   B   P		*/

/************************************************************************/

zveaction("SA", "");

/* Open input file */
zvunit(&unit, "inp", 1, NULL);
status = zvopen(unit, "open_act", " ", "io_act", "SA", NULL);

if (status != SUCCESS) {		/* Open unlabeled file */
    zvparm("format", form, &count, &def, 0, 11);

    if (count != 0) {			/* Convert archaic data types */
	if (EQUAL(form, "HEX"))   strcpy(form, "BYTE");
	if (EQUAL(form, "WORD"))  strcpy(form, "HALF");
	if (EQUAL(form, "REAL4")) strcpy(form, "REAL");
	if (EQUAL(form, "REAL8")) strcpy(form, "DOUB");
	if (EQUAL(form, "DOUBLE")) strcpy(form, "DOUB");
	if (EQUAL(form, "COMPLEX")) strcpy(form, "COMP");
	zvadd(unit, "U_FORMAT", form, "I_FORMAT", form, NULL);
    }

    zvparm("insize", nl, &count, &def, 3, 0);
    if (!def)
	zvadd(unit, "u_nl", nl[0], "u_ns", nl[1], "u_nb", nl[2], NULL);
    else {  /* input file must have INSIZE param to figure out the size */
      sprintf(msgbuf, " ** You must enter INSIZE if input file is unlabeled ** ");
      zvmessage(msgbuf, "");
      zabend();
    }

    zvparm("org", orgin, &count, &def, 0, 5);
    if (!def)
	zvadd(unit, "org", orgin, NULL);

    zvopen(unit, "open_act", "sa", "io_act", "sa", "cond", "nolabels", NULL);
}

zvget(unit, "nl", &nli, "ns", &nsi, "nb", &nbi, NULL);
zvget(unit, "format", form, "org", orgin, NULL);

/************************************************************************/
/* Obtain input data format						*/
/************************************************************************/

itype = data_type(form);
psin = pixel_size(form);	/* Pixel size in bytes of input data */
if (psin <= 0) {
    sprintf(msgbuf, " ** %s is an illegal input file format.", form);
    zvmessage(msgbuf, "");
    zabend();
}

/* Obtain output data format */

jtype = -1;
zvparm("format", oform, &count, &def, 0, 11);
if (count != 0) {
    jtype = data_type(oform);
    psout = pixel_size(oform);

/* disable hex dump for double or comp inputs       BAM */
if ( (itype == TYPE_DOUB || itype == TYPE_COMP) && jtype == TYPE_HEX )
    {
    sprintf(msgbuf, " HEX DUMP disabled");
    zvmessage(msgbuf, "");
    if ( itype == TYPE_DOUB ) jtype = TYPE_DOUB;
    if ( itype == TYPE_COMP ) jtype = TYPE_COMP;
    }

/* fix for hex dumps       BAM */ 
    if ( jtype == 6 ) psout = psin;

    if (psout <= 0) {
	sprintf(msgbuf, " ** %s is an illegal input file format.", oform);
	zvmessage(msgbuf, "");
	zabend();
    }
}
else {		/* Format param not specified, so override with input*/
    jtype = itype;
    psout = psin;
}

/**************************************************************************/
/* Get number of digits precision - tih                                   */
/**************************************************************************/
zvparm("ndigits", &nr_digits, &count, &def, 0, 0);

if (def == 1) {                       /* if no user input, use the default*/
  accuracy = nr_digits - 1;           /* values for accuracy and precision*/ 
  col_width = 10;
}

if (count != 0) {
    accuracy = nr_digits-1; /* count for digit before decimal point */
    /* compute the number of samples diplayed on one line */ 
    if (jtype == TYPE_REAL || jtype == TYPE_DOUB ) {
      nspl[jtype] = acc2nspl[accuracy - 1];      
      col_width = acc2widt[accuracy - 1]; 
      outlen[jtype] = 1 + acc2widt[accuracy - 1];
      start[jtype] += (accuracy - 3);
    }
    else if (jtype == TYPE_COMP ) {
      nspl[jtype] = (acc2nspl[accuracy - 1]) / 2;      
      col_width = acc2widt[accuracy - 1]; 
      outlen[jtype] = 2 + 2*acc2widt[accuracy - 1]; 
      start[jtype] += 2*(accuracy - 3);
    }
}

/* Obtain output organization */

zvparm("org", orgout, &count, &def, 0, 5);
if (count == 0)
    strcpy(orgout, orgin);		/* it wasn't entered */

/* We need to shuffle the first slice only if we are doing an org	*/
/* conversion and either of them is BIP.  BIL and BSQ are compatible	*/
/* since both have the same slice1 (samples).				*/

shuffle = FALSE;
if (!EQUAL(orgin, orgout))		/* organization conversion */
    shuffle = (EQUAL(orgin, "BIP") || EQUAL(orgout, "BIP"));

/* We can't handle shuffling and format conversion where the number of	*/
/* bytes change, so check for that and print an error message.		*/
/* Actually, this message lies.  I do allow both conversions if the org	*/
/* conversion is BSQ<->BIL (both have the same lower dimension (SAMP))	*/
/* or if the file and output formats have the same number of bytes	*/
/* (HEX<->BYTE, FULL<->REAL, DOUBLE<->COMPLEX) because these cases	*/
/* require no extra code.						*/

if (shuffle && (psout != psin)) {
    zvmessage(" ** Sorry, I can't do both organization and format", "");
    zvmessage(" ** conversion at the same time.  I am using the", "");
    zvmessage(" ** input file format and the requested organization.", "");
    jtype = itype;
    psout = psin;
}

/************************************************************************/
/* Obtain size parameters.						*/
/************************************************************************/

zvsize(&slin, &ssin, &nlin, &nsin, &dum, &dum);
zvbands(&sbin, &nbin, &dum);

if (shuffle) {
    if (EQUAL(orgout, "BIP"))
	buf = malloc(nbi * psin);
    else
	buf = malloc(nsi * psin);
}
else {
    if (EQUAL(orgin, "BIP"))
	buf = malloc(nbi * psin);
    else
	buf = malloc(nsi * psin);
}

if (buf == NULL) {
    zvmessage(" *** Unable to allocate memory for internal buffer", "");
    zvmessage(" *** Please notify the cognizant programmer for LIST", "");
    zabend();
}

/* Adjust for differing pixel sizes between file format and output fmt */

if (EQUAL(orgout, "BIP"))
    nbi = (nbi * psin) / psout;
else
    nsi = (nsi * psin) / psout;

ebin = MIN(nbi, sbin+nbin-1);
nbclip = MAX(ebin-sbin+1, 0);
if (nbin != nbclip) {
    zvmessage(" ** Requested area exceeds size of input picture.", "");
    zvmessage(" ** Number of bands printed reduced.", "");
}
if (nbclip == 0) {
    zvmessage(" ** Specified window contains no data.", "");
    return;				/* Return to TAE */
}

elin = MIN(nli, slin+nlin-1);
nlclip = MAX(elin-slin+1, 0);
if (nlin != nlclip) {
    zvmessage(" ** Requested area exceeds size of input picture.", "");
    zvmessage(" ** Number of lines printed reduced.", "");
}
if (nlclip == 0) {
    zvmessage(" ** Specified window contains no data.", "");
    return;				/* Return to TAE */
}

esin = MIN(nsi, ssin+nsin-1);
nsclip = MAX(esin-ssin+1, 0);
if (nsin != nsclip) {
    zvmessage(" ** Requested area exceeds size of input picture.", "");
    zvmessage(" ** Number of samples printed reduced.", "");
}
if (nsclip == 0) {
    zvmessage(" ** Specified window contains no data.", "");
    return;				/* Return to TAE */
}

/************************************************************************/
/* Get label info							*/
/************************************************************************/

nlab = MTASKS;
status = zlhinfo(unit, (char *)tasks, inst, &nlab, "ulen", 10, "err_act", "",
							NULL);
if (status != SUCCESS) {
    if (status == FILE_HAS_NO_LABEL)
	nlab = 0;
    else
	zvsignal(unit, status, TRUE);		/* doesn't return */
}

/* Get task, user, & date from first and last tasks */

if (nlab > 0) {
    zlget(unit, "HISTORY", "DAT_TIM", date1, "err_act","s",
		"hist", tasks[0], "instance", inst[0], "ulen", 33, NULL);
    zlget(unit, "HISTORY", "USER", user1, "err_act","s",
		"hist", tasks[0], "instance", inst[0], "ulen", 10, NULL);
}
if (nlab > 1) {
    zlget(unit, "HISTORY", "DAT_TIM", daten, "err_act","s",
		"hist", tasks[nlab-1], "instance", inst[nlab-1], "ulen", 33, NULL);
    zlget(unit, "HISTORY", "USER", usern, "err_act","s",
		"hist", tasks[nlab-1], "instance", inst[nlab-1], "ulen", 10, NULL);
}

/************************************************************************/
/* Get other keywords							*/
/************************************************************************/

dump = zvptst("dump");
zero = zvptst("zeroes");
eject = !zvptst("noeject");
display = zvptst("display");
if (display)
    eject = FALSE;

nofeed = zvptst("nofeed");	/* suppress form feeds */
zvp("$RUNTYPE", msgbuf, &count);	/* only do form feeds in batch mode */
if (EQUAL(msgbuf, "INTERACTIVE"))
    nofeed = TRUE;

zvp("space", &space, &count);
zvp("binc", &binc, &count);
zvp("linc", &linc, &count);
zvp("sinc", &sinc, &count);

zvparm("incremen", &i, &count, &def, 0, 0);
if (def == FALSE) {		/* not defaulted */
    sinc = i;
    linc = i;
}

/* Add checking for zero increments: AR-9036 */
if (binc < 1 || linc < 1 || sinc < 1 || i < 1) {
    zvmessage(" *** Increment values have to be 1 or greater ***", "");
    zabend();
}

/* End parameter processing */

if (nofeed)
    sprintf(dmsg, "\n   %-8.8s samples are interpreted as %-8.8s data",
		form, data[jtype]);
else
    sprintf(dmsg, "\f   %-8.8s samples are interpreted as %-8.8s data",
		form, data[jtype]);

if (display)
    space = nspace[jtype];

if (EQUAL(orgin, "BIP"))
    n_cmd = "NBANDS";
else
    n_cmd = "NSAMPS";

/************************************************************************/
/* Set the slice variables and messages according to the desired output	*/
/* organization.  Slice 1, 2, and 3 refer to the output organization,	*/
/* not necessarily the file organization.				*/
/*									*/
/* Some of the variable names in the rest of this program refer to	*/
/* samples, lines, and bands.  This is for historical reasons.  Think	*/
/* of it in terms of BSQ organization so samples always refer to slice	*/
/* 1, lines to slice 2, and bands to slice 3 even though the file or	*/
/* output may be in a different organization.				*/
/************************************************************************/

if (EQUAL(orgout, "BSQ")) {
    sl1st  = ssin;			/* slice 1 start */
    sl1n   = nsclip;			/* slice 1 number */
    sl1inc = sinc;			/* slice 1 increment */

    sl2st  = slin;
    sl2n   = nlclip;
    sl2inc = linc;

    sl3st  = sbin;
    sl3n   = nbclip;
    sl3inc = binc;

    sl3ni  = nbi;			/* number of slice3's in input file */

    samp_heading = " Samp";		/* Messages for headings */
    line_heading = " Line";
    band_heading = " Band";

    sl1_cmd = "SAMP";			/* Parameters for zvread call */
    sl2_cmd = "LINE";
    sl3_cmd = "BAND";
}
else if (EQUAL(orgout, "BIL")) {
    sl1st  = ssin;			/* slice 1 start */
    sl1n   = nsclip;			/* slice 1 number */
    sl1inc = sinc;			/* slice 1 increment */

    sl2st  = sbin;
    sl2n   = nbclip;
    sl2inc = binc;

    sl3st  = slin;
    sl3n   = nlclip;
    sl3inc = linc;

    sl3ni  = nli;			/* number of slice3's in input file */

    samp_heading = " Samp";		/* Messages for headings */
    line_heading = " Band";
    band_heading = " Line";

    sl1_cmd = "SAMP";			/* Parameters for zvread call */
    sl2_cmd = "BAND";
    sl3_cmd = "LINE";
}
else {			/* orgout == "BIP" */
    sl1st  = sbin;			/* slice 1 start */
    sl1n   = nbclip;			/* slice 1 number */
    sl1inc = binc;			/* slice 1 increment */

    sl2st  = ssin;
    sl2n   = nsclip;
    sl2inc = sinc;

    sl3st  = slin;
    sl3n   = nlclip;
    sl3inc = linc;

    sl3ni  = nli;			/* number of slice3's in input file */

    samp_heading = " Band";		/* Messages for headings */
    line_heading = " Samp";
    band_heading = " Line";

    sl1_cmd = "BAND";			/* Parameters for zvread call */
    sl2_cmd = "SAMP";
    sl3_cmd = "LINE";
}
/************************************************************************/
/* List out picture in vertical strips with "nspl" elements per line	*/
/************************************************************************/

nspl1 = nspl[jtype];		/* samps per line (strip) */
nsx   = (sl1n-1) / sl1inc + 1;	/* # samps after decimation */
nlx   = (nsx-1) / nspl1 + 1;	/* # strips to output */
llx   = nsx - (nlx-1)*nspl1;	/* # samps in last strip */
slice1 = 1;			/* current sample number */

if (dump) {
    nspl1 = nsx;
    dumplines = nlx;
    nlx = 1;
    llx = nsx;
}
allzero = TRUE;
empty = TRUE;
oline = 0;

/************************************************************************/
/* Strip Loop								*/
/************************************************************************/

for (ilx = 1; ilx <= nlx; ilx++) {
    tof = TRUE;
    newstrip = TRUE;
    inzero = FALSE;

    sx = nspl1;			/* # of samps this line */
    if (ilx == nlx)
	sx = llx;		/* last strip */

    ssout = sl1st;
    sstrip = slice1;
    estrip = sstrip + sx - 1;

/************************************************************************/
/* Slice 3 Loop								*/
/************************************************************************/

    b2 = sl3st + sl3n - 1;
    for (slice3 = sl3st; slice3 <= b2; slice3 += sl3inc) {
	newslice3 = TRUE;
	tof = TRUE;

/************************************************************************/
/* Slice 2 Loop								*/
/************************************************************************/
	l2 = sl2st + sl2n - 1;
	for (slice2 = sl2st; slice2 <= l2; slice2 += sl2inc) {
	    empty = FALSE;

	    sl1end = sl1st + sl1n - 1;


	    if (shuffle) {		/* must reorganize slice 1 */
		i = 0;
		for (s1 = sl1st; s1 <= sl1end; s1 += sl1inc) {
		    zvread(unit, buf+i, sl2_cmd, slice2,
			   sl3_cmd, slice3, sl1_cmd, s1, n_cmd, 1, NULL);
		    i += psin;		/* increment by pixel size */
		}
	    }
	    else {
		zvread(unit, buf, sl2_cmd, slice2, sl3_cmd, slice3, NULL);

		/* Extract the window */
		movx(itype, jtype, sl1st, sl1end, &nso, sl1inc, buf);
	    }

	    /* Extract the strip */
	    movx(itype, jtype, sstrip, estrip, &nso, 1, buf);

	    if (!zero && all_zero(jtype, buf, nso))
		inzero = TRUE;
	    else {

/* Print headings, including history labels and sample numbers */

		if ((oline >= LINES_PER_PAGE) && eject)
		    tof = TRUE;

		if (tof) {
		    tof = FALSE;
		    oline = 1;
		    if (newstrip) {
			zvmessage(dmsg, "");	/* ...samps interp as...data */
			newstrip = FALSE;
		    }
		    else {
			if (nofeed)
			    zvmessage("\n", "");
			else
			    zvmessage("\f", "");
		    }

/* List task, user, & date from first and last tasks */

		    if (nlab > 0) {
			print_label(tasks[0], user1, date1);
			oline++;
			if (nlab > 1) {
			    print_label(tasks[nlab-1], usern, daten);
			    oline++;
			}
		    }

/* Print band title unless this is the only band and it is number 1 */
		    if (newslice3 && (sl3ni != 1 || slice3 != 1)) {
			newslice3 = FALSE;
			zvmessage(" ***********", "");
			sprintf(msgbuf, "%s =%6d", band_heading, slice3);
			zvmessage(msgbuf, "");
			zvmessage(" ***********", "");
			oline += 3;
		    }

		    if (!dump) {	/* dump doesn't get sample numbers */

			memset(msgbuf, ' ', sizeof(msgbuf));

			if (jtype == TYPE_HEX)
			    strncpy(msgbuf, samp_heading, 5);
			else
			    strncpy(msgbuf+4, samp_heading, 5);

			t1 = (slice1-1) * sl1inc + sl1st;
			t2 = t1 + (sx-1) * sl1inc;

			if (jtype == TYPE_HEX)
			    dt = 4*sl1inc;	/* delta for sample numbers */
			else if (jtype == TYPE_BYTE)
			    dt = 2*sl1inc;
			else
			    dt = sl1inc;

			si = start[jtype];

                        if ( jtype == TYPE_HEX )
    		            {
                            if ( itype == TYPE_HALF ) t2 *= 2;              
                            if ( itype == TYPE_FULL ) t2 *= 4;              
                            if ( itype == TYPE_REAL ) t2 *= 4;              
                            if ( itype == TYPE_DOUB ) t2 *= 8;              
		            }

			for (tx = t1; tx <= t2; tx += dt) {
			    sprintf(temp, "%6d", tx);
			    strncpy(msgbuf+si, temp, 6); 
			    si += outlen[jtype];
			}

							/* null terminate */
			msgbuf[MIN(si-outlen[jtype]+6,sizeof(msgbuf)-1)] = '\0';
			zvmessage(msgbuf, "");
			oline++;
		    }
		    if (jtype == TYPE_HEX)
			zvmessage(line_heading, "");
		    else {
			sprintf(msgbuf, "  %s", line_heading);
			zvmessage(msgbuf, "");
		    }
		    oline++;
		}		/* End of headings */

/* print blank line to indicate that zeroes were skipped */

		if (inzero) {
		    inzero = FALSE;
		    zvmessage("", "");
		    oline++;
		}
		allzero = FALSE;

                if ( jtype == TYPE_HEX )
		    {
                    if ( itype == TYPE_HALF ) nso *= 2;                    
                    if ( itype == TYPE_FULL ) nso *= 4;                    
                    if ( itype == TYPE_REAL ) nso *= 4;                    
                    if ( itype == TYPE_DOUB ) nso *= 8;                    
		    }
		print_line(jtype, nso, buf, slice2, nspl[jtype], psout, accuracy, col_width);
		oline++;

		if (dump)
		    oline = oline - 1 + dumplines;

		if (space != 0) {
		    for (i=0; i<space; i++)
			zvmessage("", "");
		    oline += space;
		}
	    }		/* end if (!zero && all_zero...) */
	}		/* end slice2 loop */
    }			/* end slice3 loop */
    slice1 += nspl1;
}			/* end strip loop */

if (empty)
    zvmessage(" ** Specified window contains no data.", "");

if (allzero && !empty)
    zvmessage(" ** The specified window is all zero.", "");

return;			/* return to TAE */

}

/************************************************************************/
/* Print task, user, and date_time from an input file label		*/
/************************************************************************/

print_label(task, user, time)
char *task;
char *user;
char *time;
{
char line[80];

sprintf(line, " Task:%-8.8s  User:%-8.8s  Date_Time:%-24.24s", task, user, time);
zvmessage(line, "");

}

/************************************************************************/
/* Return type code for a given data type				*/
/************************************************************************/

int data_type(type)
char *type;
{
char line[80];

if (strncmp(type, "HEX", 3) == 0)
    return TYPE_HEX;
if (strncmp(type, "BYTE", 4) == 0)
    return TYPE_BYTE;
if (strncmp(type, "HALF", 4) == 0)
    return TYPE_HALF;
if (strncmp(type, "WORD", 4) == 0)	/* obsolete */
    return TYPE_HALF;
if (strncmp(type, "FULL", 4) == 0)
    return TYPE_FULL;
if (strncmp(type, "REAL8", 5) == 0)	/* obsolete */
    return TYPE_DOUB;
if (strncmp(type, "REAL", 4) == 0)
    return TYPE_REAL;
if (strncmp(type, "DOUB", 4) == 0)
    return TYPE_DOUB;
if (strncmp(type, "COMP", 4) == 0)
    return TYPE_COMP;

sprintf(line, " ** %-6.6s is an invalid data format.", type);
zvmessage(line, "");

}

/************************************************************************/
/* Return pixel size for a given data type				*/
/************************************************************************/

int pixel_size(type)
char *type;
{
char line[80];

if (strncmp(type, "HEX", 3) == 0)
    return sizeof(unsigned char);
if (strncmp(type, "BYTE", 4) == 0)
    return sizeof(unsigned char);
if (strncmp(type, "HALF", 4) == 0)
    return sizeof(short int);
if (strncmp(type, "WORD", 4) == 0)	/* obsolete */
    return sizeof(short int);
if (strncmp(type, "FULL", 4) == 0)
    return sizeof(int);
if (strncmp(type, "REAL8", 5) == 0)	/* obsolete */
    return sizeof(double);
if (strncmp(type, "REAL", 4) == 0)
    return sizeof(float);
if (strncmp(type, "DOUB", 4) == 0)
    return sizeof(double);
if (strncmp(type, "COMP", 4) == 0)
    return sizeof(float) * 2;

sprintf(line, " ** %-6.6s is an invalid data format.", type);
zvmessage(line, "");

}

/************************************************************************/
/* Move a range of samples in a line buffer to the beginning of the buf	*/
/************************************************************************/

movx(itype, type, start, end, ns, inc, buf)
int itype;		/* input data type */
int type;		/* jtype of buffer */
int start;		/* start pixel offset */
int end;		/* end pixel offset */
int *ns;		/* out: number of pixels moved */
int inc;		/* increment */
char *buf;		/* buffer */
{
int i;
int hex;
unsigned char *bbuf;
short int *sbuf;
int *ibuf;
float *fbuf;
double *dbuf;
struct complex {
    float r, i;
} *cbuf;

*ns = 0;


        if ( type == TYPE_HEX )
            {
            hex = type;
            if ( itype == TYPE_BYTE ) type = TYPE_BYTE; 
            if ( itype == TYPE_HALF ) type = TYPE_HALF; 
            if ( itype == TYPE_FULL ) type = TYPE_FULL; 
            if ( itype == TYPE_REAL ) type = TYPE_REAL; 
            if ( itype == TYPE_DOUB ) type = TYPE_DOUB; 
    	    }




switch (type) {

    case TYPE_BYTE:
	bbuf = (unsigned char *)buf;
	for (i=start; i<=end; i+=inc)
            memcpy ( &bbuf[(*ns)++],&bbuf[i-1], 1L);
/*	    bbuf[(*ns)++] = bbuf[i-1]; */
	break;

    case TYPE_HALF:
	sbuf = (short int *)buf;
	for (i=start; i<=end; i+=inc)
	    memcpy( &sbuf[(*ns)++], &sbuf[i-1], 2L);
/*	    sbuf[(*ns)++] = sbuf[i-1]; */
	break;

    case TYPE_FULL:
	ibuf = (int *)buf;
	for (i=start; i<=end; i+=inc)
	    memcpy( &ibuf[(*ns)++], &ibuf[i-1], 4L);
/*	    ibuf[(*ns)++] = ibuf[i-1]; */
	break;

    case TYPE_REAL:
	fbuf = (float *)buf;
	for (i=start; i<=end; i+=inc)
	    memcpy( &fbuf[(*ns)++], &fbuf[i-1], 4L);
/*	    fbuf[(*ns)++] = fbuf[i-1]; */
	break;

    case TYPE_DOUB:
	dbuf = (double *)buf;
	for (i=start; i<=end; i+=inc)
	    memcpy( &dbuf[(*ns)++], &dbuf[i-1], 8L);
/*	    dbuf[(*ns)++] = dbuf[i-1]; */
	break;

    case TYPE_COMP:
	cbuf = (struct complex *)buf;
	for (i=start; i<=end; i+=inc) {
	    cbuf[(*ns)  ].r = cbuf[i-1].r;
	    cbuf[(*ns)++].i = cbuf[i-1].i;
	}
	break;
}
    if ( hex == TYPE_HEX ) type = TYPE_HEX;
}

/************************************************************************/
/* Check to see if the buffer of the given type is all zero.		*/
/************************************************************************/

int all_zero(type, buf, ns)
int type;		/* data type (jtype) */
char *buf;		/* buffer */
int ns;			/* number of pixels */
{
int i;
unsigned char *bbuf;
short int *sbuf;
int *ibuf;
float *fbuf;
double *dbuf;
struct complex {
    float r, i;
} *cbuf;

switch (type) {

    case TYPE_HEX:
    case TYPE_BYTE:
	bbuf = (unsigned char *)buf;
	for (i=0; i<ns; i++)
	    if (bbuf[i] != 0)
		return FALSE;
	break;

    case TYPE_HALF:
	sbuf = (short int *)buf;
	for (i=0; i<ns; i++)
	    if (sbuf[i] != 0)
		return FALSE;
	break;

    case TYPE_FULL:
	ibuf = (int *)buf;
	for (i=0; i<ns; i++)
	    if (ibuf[i] != 0)
		return FALSE;
	break;

    case TYPE_REAL:
	fbuf = (float *)buf;
	for (i=0; i<ns; i++)
	    if (fbuf[i] != 0.0)
		return FALSE;
	break;

    case TYPE_DOUB:
	dbuf = (double *)buf;
	for (i=0; i<=ns; i++)
	    if (dbuf[i] != 0.0)
		return FALSE;
	break;

    case TYPE_COMP:
	cbuf = (struct complex *)buf;
	for (i=0; i<ns; i++)
	    if ((cbuf[i].r != 0.0) || (cbuf[i].i != 0.0))
		return FALSE;
	break;
}

return TRUE;			/* all zero */

}

/************************************************************************/
/* Prints a line of data, which may span multiple output lines in 'DUMP	*/
/* mode.  We only print "nspl" pixels per output line, then we continue	*/
/* on the next line (without the header).				*/
/************************************************************************/

print_line(type, ns, buf, header, nspl, pix_size, accuracy, col_width)
int type;			/* type of data */
int ns;				/* number of samples */
char *buf;			/* pixel buffer */
int header;			/* line number to print first */
int nspl;			/* max # of samps per line (needed for 'dump) */
int pix_size;			/* size of pixels (only to avoid switch stmt) */
int accuracy;                   /* # of precision digits */
int col_width;                  /* width of output column*/
{

while (ns > 0) {
    print_one_line(type, MIN(ns, nspl), buf, header, accuracy, col_width);
    header = 0;			/* don't print it again */
    buf += nspl * pix_size;
    ns -= nspl;
}
}



/************************************************************************/
/* Prints a line of data						*/
/************************************************************************/

print_one_line(type, ns, buf, header, accuracy, col_width)
int type;			/* type of data */
int ns;				/* number of samples */
char *buf;			/* pixel buffer */
int header;			/* line number to print first (only if non-0) */
int accuracy;                   /* # of precision digits */
int col_width;                  /* width of output column*/

{
int i, j;
char line[255];
char temp[80];
unsigned char *bbuf;
short int *sbuf;
int *ibuf;
float *fbuf;
double *dbuf;
struct complex {
    float r, i;
} *cbuf;

if (header != 0)
    sprintf(line, " %6d", header);
else
    strcpy(line, "       ");

switch (type) {

    case TYPE_HEX:
	strcat(line, " ");
	bbuf = (unsigned char *)buf;
	for (i=0; i<ns; i++) {		/* hex representation */
	    if ((i % 4) == 0)
		strcat(line, " ");
	    sprintf(temp, "%02X", bbuf[i]);
	    strcat(line, temp);
	}
	strcat(line, "  ");
	j = strlen(line);
	for (i=0; i<ns; i++) {		/* ascii representation */
	    if (bbuf[i] < 32)
		line[j++] = '|';	/* non-printing char */
	    else if (bbuf[i] >= 128)
		line[j++] = '#';	/* any other illegal char */
	    else
		line[j++] = bbuf[i];
	}
	line[j] = '\0';			/* null terminator */

	break;

    case TYPE_BYTE:
	strcat(line, "    ");
	bbuf = (unsigned char *)buf;
	for (i=0; i<ns; i++) {
	    sprintf(temp, " %3d", bbuf[i]);
	    strcat(line, temp);
	}
	break;

    case TYPE_HALF:
	strcat(line, "    ");
	sbuf = (short int *)buf;
	for (i=0; i<ns; i++) {
	    sprintf(temp, "%6d", sbuf[i]);
	    strcat(line, temp);
	}
	break;

    case TYPE_FULL:
	strcat(line, "    ");
	ibuf = (int *)buf;
	for (i=0; i<ns; i++) {
	    sprintf(temp, "%11d", ibuf[i]);
	    strcat(line, temp);
	}
	break;

/*!!!!!!!!!!!*/
    case TYPE_REAL:
	strcat(line, "    ");
	fbuf = (float *)buf;
	for (i=0; i<ns; i++) {
	    sprintf(temp, " %*.*E",col_width, accuracy, fbuf[i]);
	    strcat(line, temp);
	}
	break;

    case TYPE_DOUB:
	strcat(line, "    ");
	dbuf = (double *)buf;
	for (i=0; i<ns; i++) {
	    sprintf(temp, " %*.*E", col_width, accuracy, dbuf[i]);
	    strcat(line, temp);
	}
	break;

    case TYPE_COMP:
	strcat(line, "    ");
	cbuf = (struct complex *)buf;
	for (i=0; i<ns; i++) {
	    sprintf(temp, " %*.*E", col_width, accuracy, cbuf[i].r);
	    strcat(line, temp);
	    sprintf(temp, " %*.*E", col_width, accuracy, cbuf[i].i);
	    strcat(line, temp);
	}
	break;
/*!!!!!!!!!!!*/

}

zvmessage(line, "");
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create list.imake
#define PROGRAM list

#define MODULE_LIST list.c

#define MAIN_LANG_C
#define R2LIB

/* Comment this out before delivery.
#define DEBUG
*/

#define USES_C

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create list.pdf
PROCESS HELP=*
PARM INP     TYPE = STRING
PARM SIZE    TYPE = INTEGER               COUNT = 4   DEFAULT=(1,1,0,0)
PARM BANDS   TYPE = INTEGER		  COUNT = 2   DEFAULT=(1,0)
PARM SL      TYPE = INTEGER	          COUNT = 1   DEFAULT = 1
PARM SS      TYPE = INTEGER	          COUNT = 1   DEFAULT = 1
PARM SB	     TYPE = INTEGER		  COUNT = 1   DEFAULT = 1
PARM NL      TYPE = INTEGER 	          COUNT = 1   DEFAULT = 0
PARM NS      TYPE = INTEGER    	          COUNT = 1   DEFAULT = 0
PARM NB	     TYPE = INTEGER		  COUNT = 1   DEFAULT = 0
PARM LINC    TYPE = INTEGER 		  COUNT = 1   DEFAULT = 1
PARM SINC    TYPE = INTEGER               COUNT = 1   DEFAULT = 1
PARM BINC    TYPE = INTEGER               COUNT = 1   DEFAULT = 1
PARM INCREMEN TYPE= INTEGER               COUNT = 1   DEFAULT = 1
PARM SPACE   TYPE = INTEGER               COUNT = 1   DEFAULT = 0
PARM NDIGITS TYPE = INTEGER VALID=(2:16)  COUNT = 0:1 DEFAULT = 4
PARM DUMP    TYPE = KEYWORD VALID=DUMP    COUNT = 0:1 DEFAULT = --
PARM ZEROES  TYPE = KEYWORD VALID=ZEROES  COUNT = 0:1 DEFAULT = --
PARM NOEJECT TYPE = KEYWORD VALID=NOEJECT COUNT = 0:1 DEFAULT = --
PARM DISPLAY TYPE = KEYWORD VALID=DISPLAY COUNT = 0:1 DEFAULT = --
PARM NOFEED  TYPE = KEYWORD VALID=NOFEED  COUNT = 0:1 DEFAULT = --
PARM FORMAT  TYPE = KEYWORD               COUNT = 0:1 DEFAULT = -- +
     VALID=(HEX,BYTE,HALF,WORD,FULL,REAL,REAL4,REAL8,DOUBLE,COMPLEX)  
PARM ORG     TYPE = KEYWORD		  COUNT = 0:1 DEFAULT = -- +
     VALID=(BSQ,BIL,BIP)
PARM INSIZE  TYPE = INTEGER		  COUNT = 0:3 DEFAULT = --

!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=(LIST,INP,size,INSIZE,DUMP)

END-PROC
!# ANNOT ICON = list
.TITLE
 Print the DN values of an image area
.HELP
 PURPOSE:
 LIST will give a numerical listing of a VICAR dataset.
 It offers a large assortment of print formats and input data formats.
 All parameters except INP are optional. Only the first four characters of
 string parameters (other than INP) are checked by the program.
 Caution should be used in defaulting the SIZE parameter as this will result
 in the entire picture being listed. For a 900 x 900 byte picture, this
 would tie up the line printer for 30 minutes.

 If the keyword DUMP is specified, the output may be a tape data set.
 If DUMP is not specified, the data set is listed in vertical strips
 30 pixels wide.  Since this will generally result in multiple
 passes through the data, the picture should be on a disk data set.
.page
 LIST can handle unlabeled files if the parameter INSIZE is given.  The
 input file format and organization is taken from the FORMAT and ORG
 parameters, if present.

 The maximum record size (number of samples * pixel size) that LIST
 can handle is 200,000 bytes.  If you try to list even just a part
 of an image that is bigger than 200,000 bytes, you will get an error.

 EXECUTION:
    The following is the execution statement for LIST:
            LIST  INP  SIZE  PARAMS
 where INP, SIZE, and PARAMS are parameters discussed in their
 respective parameter section in TUTOR mode.
.page
 TIMING:                   None available for the VAX

 WRITTEN BY:               Gary Yagi          01/10/77
 CONVERSION TO VAX:        Lucas Kamp         12/20/82
 CONVERSION TO VICAR2:     Ray Stagner        02/15/85
 REVISION:                 Ray Stagner        04/25/85
 3D File Extension:        Bob Deen           06/18/87
 Added stdio.h header file Amy Chen           10/06/00
 Cognizant Programmer:     Bob Deen
.LEVEL1
.VARIABLE INP
 Input dataset
.VARIABLE SIZE
 Image size
 (See HELP FORMAT)
.VARIABLE BANDS
 Image band size
 (See HELP FORMAT)
.VARIABLE SL
 Integer - Starting
 Line
.VARIABLE SS
 Integer - Starting
 Sample
 (See HELP FORMAT)
.VARIABLE SB
 Integer - Starting
 Band
 (See HELP FORMAT)
.VARIABLE NL
 Integer - Number
 of Lines
.VARIABLE NS
 Integer - Number
 of Samples
 (See HELP FORMAT)
.VARIABLE NB
 Integer - Number
 of Bands
 (See HELP FORMAT)
.VARIABLE FORMAT
 Data format 
 Valid: HEX,BYTE,HALF
 WORD,FULL,REAL,REAL4,
 REAL8,DOUBLE,COMPLEX
.VARIABLE LINC
 Line increment
.VARIABLE SINC
 Sample increment
 (See HELP FORMAT)
.VARIABLE BINC
 Band increment
 (See HELP FORMAT)
.VARIABLE INCREMEN
 Line and sample
 increment
.VARIABLE DUMP
 Record dump? 
 Valid: DUMP.
.VARIABLE ZEROES
 Print zeroes?
 Valid: ZEROES.
.VARIABLE NOEJECT
 Repeat header?
 Valid: NOEJECT.
.VARIABLE SPACE
 Line spacing
.VARIABLE NDIGITS
 Number of significant digits.
.VARIABLE DISPLAY
 Display spacing?
 Valid: DISPLAY.
.VARIABLE NOFEED
 Suppress form feeds?
 Valid: NOFEED
.VARIABLE ORG
 Output organization
 Valid: BSQ,BIL,BIP.
.VARIABLE INSIZE
 (nl,ns,nb) of input
 if unlabeled
.LEVEL2
.VARI INP
 Standard VICAR1 input dataset parameter.
.VARI SIZE
 Standard VICAR1 image size parameter.
.VARI BANDS
 Standard VICAR1 band size parameter.
.VARIABLE FORMAT
 The value of the FORMAT keyword specifies the (LOGICAL) input data format
 and the resulting print format. The actual input data format is obtained
 from the image label, if available.
  FORMAT   INPUT FORMAT      PRINT FORMAT
    HEX      BYTE          Hexadecimal and alphanumeric
    BYTE     BYTE          30 3-digit integers per line
    HALF     INTEGER*2     20 5-  "       "     "   "
    WORD     INTEGER*2     20 5-  "       "     "   "     Synonym for HALF
    FULL     INTEGER*4     10 9-  "       "     "   "
    REAL     REAL*4        10 E-format numbers per line  
    REAL4    REAL*4        10     "       "     "   "     Synonym for REAL
    REAL8    REAL*8        10     "       "     "   "     Synonym for DOUBLE
    DOUBLE   REAL*8        10     "       "     "   "
    COMPLEX  COMPLEX*8     10     "       "     "   "
 If FORMAT is not specified, the information is obtained from the image label.
 If the information is not in the label, byte data are assumed. FORMAT also
 controls the number of bytes per sample of the SIZE,SINC,SS and NS parameters,
 or the BANDS,BINC,SB, and NB parameters if the organization is BIP.

 nb. HEX - not available for DOUB or COMP inputs.  If the input file in not 
	   Vicar formatted, it is recommended that the file is input as byte
	   format. 
.VARIABLE LINC
 The value of LINC (integer) specifies the line increment: every LINC-th line
 will be printed, starting with the first.
 Default: LINC = 1.
.VARIABLE SINC
 The value of SINC (integer) specifies the sample increment: every SINC samples
 of a line will be printed, starting with the first. 
 Default: SINC = 1.
.VARIABLE BINC
 The value of BINC (integer) specifies the band increment: every BINC-th band
 will be printed, starting with the first.
.VARIABLE INCREMEN
 The value of INCREMEN (integer) specifies the line AND sample increment:
 every INCREMEN samples of of every INCREMEN line will be printed,
 starting with the first.
 Default: INCREMEN = 1. 
.VARIABLE DUMP
 This parameter has one valid keyword value: DUMP.
 
 If DUMP is specified, a line-by-line listing will be produced, 
 i.e., all elements on a line within the area specified by the 
 SIZE parameter will be listed before proceeding to the next line.
 
 If DUMP is NOT specified (default), the listing format will be 
 that described in the program documentation, i.e. formatted for
 a line printer.
.VARIABLE ZEROES
 This parameter has one valid keyword value: ZEROES.
 
 If ZEROES is specified, lines containing all zero values are printed.
 
 If ZEROES is NOT specified (default), lines containing all zeroes will
 not be printed.
.VARIABLE NOEJECT
 This parameter has one valid keyword value: NOEJECT.
 
 If NOEJECT is specified, the printing of labels and column headings
 in all but the first page of each strip is suppressed.
 
 If NOEJECT is NOT specified (default), labels and column headings
 are repeated each page.
.VARIABLE SPACE
 The value of SPACE (integer) specifies the spacing between lines in the
 output listing.
 Default: SPACE = 1.
.VARIABLE NDIGITS
 The number of significant digits (2-16) to be displayed for real numbers 
 (REAL, DOUBLE and COMP).
 Default: NDIGITS = 4.
.VARIABLE DISPLAY
 This parameter has one valid keyword value: DISPLAY.
 
 If DISPLAY is specified, the spacing of the output is adjusted so that
 the aspect ratio of the listing is the same as the aspect ratio of the
 image.
 It is suggested that this option only be used if FORMAT is BYTE or HALF.
 
 If DISPLAY is NOT specified (default), the default spacing is used.
.VARIABLE NOFEED
 This parameter has one valid keyword value: NOFEED.
 
 If NOFEED is specified, then the form feed that normally separates
 each strip is suppressed.  This allows much more compact printing of
 images with many samples but only a few lines.

 The default behavior (if NOFEED is not given) is to issue a form feed
 before every strip of printing, only in batch mode.  If LIST is run
 interactively, no form feed is ever generated.
.VARIABLE ORG
 This parameter has three valid keyword values: BSQ, BIL, and BSQ.

 ORG specifies the organization that the file will be printed in.  The
 meaning of bands, lines, and samples does not change with different
 organizations; they are just printed in a different order.  Sample
 number 1 in line 2 and band 3 will always be sample 1, line 2, and
 band 3 under any organization, although it might be printed in a
 different place.

 If ORG is not specifed (default), the input file organization is used.

 You may not do both organization conversion and format conversion at
 the same time unless one (or both) of these conditions are met:

 1.  You are converting from BSQ to BIL or vice versa.

 2.  The output format has the same number of bytes as the input
     format, e.g. HEX<->BYTE, FULL<->REAL, or DOUBLE<->COMPLEX.
.VARIABLE INSIZE
 The size of the input file if it has no label.  If the input file
 has a label, then this parameter is ignored, and the actual file
 size is used.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlist.pdf
!TSTLIST.PDF
procedure
refgbl $autousage
refgbl $syschar
refgbl $echo

body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

!This is a test of program LIST

!Test compatibility with 2D files
!First test the program on BYTE data (with auto type determination).
!exercise all parameter options on this data type (FORMAT=BYTE)
gen a.dat nl=5 ns=10 sinc=1
list a.dat size=(1,1,5,5) 'DUMP 'ZEROES
list a.dat size=(1,1,5,5) linc=2 sinc=2 space=2
list a.dat size=(1,1,5,5) 'DISPLAY

!Test FORMAT=HEX
gen a.dat nl=5 ns=10 sinc=1 ival=65
list a.dat size=(1,1,5,5) 'HEX

!Test HALFWORD format
gen a.dat nl=5 ns=10 'HALF sinc=1 ival=2000
list a.dat size=(1,1,5,10) 

!Test FULLWORD format
gen a.dat nl=5 ns=40 'FULL sinc=1 ival=99999
list a.dat size=(1,1,5,20) 

!Test REAL*4 format
gen a.dat nl=5 ns=40 'REAL4 sinc=100 linc=100 ival=99999
list a.dat size=(1,1,5,20) 'REAL4

!Test REAL*8 format
gen a.dat nl=5 ns=80 'REAL8 sinc=1 ival=1000
list a.dat size=(1,1,5,40)

!Test COMPLEX format
gen a.dat nl=8 ns=8 'REAL
cform a.dat b.dat oform=COMPLEX
list a.dat
list b.dat
list b.dat size=(2,2,7,7)  

!Test format override capabilities
gen a.dat nl=5 ns=10 'HALF ival=16705
list a.dat
list a.dat 'BYTE ns=20
list a.dat 'HEX  ns=20
list a.dat 'HALF
list a.dat 'FULL

!Create a large image with three labels
gen a.dat ns=120 nl=120 ival=1
stretch a.dat b.dat linear=(0,255)
stretch b.dat a.dat linear=(0,255)

if ($syschar(1)="UNIX")
   ush rm b.dat
else
   dcl del b.dat;*
end-if

!List the labels
label-list a.dat

!Test LINC and SINC
list a.dat linc=8 sinc=2
list a.dat linc=8 sinc=3
list a.dat linc=8 sinc=4
list a.dat linc=8 sinc=5
list a.dat linc=8 sinc=6
list a.dat linc=8 sinc=8

!Test SIZE parameters and list by strips
list a.dat size=(1,1,20,15)
list a.dat size=(5,5,10,15)
list a.dat size=(11,11,20,90)

!This is a test for mixed formats
!***** source is BYTE
gen a.dat nl=22 ns=64 'BYTE
list a.dat
list a.dat 'HALF  
list a.dat 'FULL

!***** source is HALF
gen a.dat nl=22 ns=32 'HALF
list a.dat
list a.dat 'FULL
list a.dat 'BYTE ns=64

!***** source is FULL
gen c.dat nl=22 ns=16 'FULL
list c.dat
list c.dat 'HALF  ns=32
list c.dat 'BYTE  ns=64

!***** test ZERO and DUMP
gen b.dat nl=2 ns=2 sinc=2
size b.dat a.dat zoom=30 'NOIN
list b.dat
list a.dat 'ZERO
list a.dat
list a.dat 'DUMP

!***** test input(L,S) --> output(L,S) algorithm bug
gen a.dat nl=3 ns=107
list a.dat ns=15
!  0 1 2 3 4 5 6 7 8  9  10 11 12 13 14
!  1 2 3 4 5 6 7 8 9  10 11 12 13 14 15
!  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16

list a.dat ss=3 ns=1 'HALF
!  1284 = 5*256+4
!  1541 = 6*256+5
!  1798 = 7*256+6

list a.dat ss=3 ns=1 'FULL
!  185207048 = ((11*256+10)*256+ 9)*256+8
!  202050057 = ((12*256+11)*256+10)*256+9
!  218893066 = ((13*256+12)*256+11)*256+10

! Now test 3D files
gen a.dat nb=10 binc=100 linc=10 'HALF
list a.dat nb=2
list a.dat 'BIL sl=3 nl=2
list a.dat 'BIP sl=3 nl=2 sb=5 nb=3
gen a.dat nb=10 nl=5 binc=100 linc=10 'FULL 'BIL
list b.dat
list b.dat 'BSQ nb=2 ns=20 'HEX
list b.dat 'BIP nl=2 'DISPLAY
gen c.dat nb=10 nl=5 binc=100 linc=10 'REAL8 'BIP
list c.dat  ! should go in 2 strips
list c.dat 'BSQ nb=1
list c.dat 'BIL 'DUMP nl=2
gen a.dat nb=60 nl=2 ns=20 binc=100 linc=10 'HALF 'BIP
list a.dat 'NOEJECT
list a.dat 'NOEJECT 'DUMP 'BIL

! Test unlabeled files, the output should generate an ABEND
gen a.dat
label-remove a.dat b.dat
list b.dat

! Test unlabeled files with insize; this should NOT generate an ABEND
gen a.dat nl=100
label-remove a.dat b.dat
list b.dat insize=(50,10,2)
list b.dat insize=(10,10,10) sl=4 nl=5 ss=3 ns=3 sb=7 nb=2

if ($syschar(1)="UNIX")
   ush rm *.dat
else
   dcl del *.dat;*
end-if

end-proc
$!-----------------------------------------------------------------------------
$ create tst_real.pdf
!TSTLIST.PDF
procedure
refgbl $autousage
refgbl $syschar
refgbl $echo

body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

!This is a test of program LIST
!testing REAL, DOUB and COMP real number formatting - see AR9635
!NOTE:  the output is easier to see on a terminal window with 132 columns

write "Testing BYTE - should be unchanged from unit test"
gen a.dat nl=5 ns=10 sinc=1
list a.dat size=(1,1,5,5) 'DUMP 'ZEROES
list a.dat size=(1,1,5,5) linc=2 sinc=2 space=2
list a.dat size=(1,1,5,5) 'DISPLAY


write "Testing HEX - should be unchanged from unit test"
!Test FORMAT=HEX
gen a.dat nl=5 ns=10 sinc=1 ival=65
list a.dat size=(1,1,5,5) 'HEX


write "Testing HALFWORD - should be unchanged from unit test"
!Test HALFWORD format
gen a.dat nl=5 ns=10 'HALF sinc=1 ival=2000
list a.dat size=(1,1,5,10) 


write "Testing FULLWORD - should be unchanged from unit test"
!Test FULLWORD format
gen a.dat nl=5 ns=40 'FULL sinc=1 ival=99999
list a.dat size=(1,1,5,20) 

write "Testing Single Precision real numbers using default precision"
!Test REAL*4 format
gen a.dat nl=5 ns=15 'REAL4 sinc=100 linc=100 ival=99999
list a.dat 'REAL4

write "Using different precision:"
list a.dat ndigits=2
list a.dat ndigits=3
list a.dat ndigits=6
list a.dat ndigits=7

write "Testing Double Precision real numbers using default precision"
!Test REAL*8 format
gen a.dat nl=5 ns=15 'REAL8 sinc=1 ival=1000
list a.dat 

write "Using different precision:"
list a.dat ndigits=2
list a.dat ndigits=8
list a.dat ndigits=15
list a.dat ndigits=16

write "Testing Complex numbers using default precision"
!Test COMPLEX format
gen a.dat nl=8 ns=8 'REAL
cform a.dat b.dat oform=COMPLEX
list a.dat
list b.dat

write "Using different precision:"
list b.dat ndigits=2
list b.dat ndigits=3
list b.dat ndigits=6
list b.dat ndigits=7

if ($syschar(1)="UNIX")
   ush rm *.dat
else
   dcl del *.dat;*
end-if

end-proc
$ Return
$!#############################################################################
