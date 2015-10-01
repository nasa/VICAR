$!****************************************************************************
$!
$! Build proc for MIPL module append
$! VPACK Version 1.9, Monday, December 07, 2009, 16:00:10
$!
$! Execute by entering:		$ @append
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
$ write sys$output "*** module append ***"
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
$ write sys$output "Invalid argument given to append.com file -- ", primary
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
$   if F$SEARCH("append.imake") .nes. ""
$   then
$      vimake append
$      purge append.bld
$   else
$      if F$SEARCH("append.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake append
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @append.bld "STD"
$   else
$      @append.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create append.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack append.com -mixed -
	-s append.c -
	-i append.imake -
	-p append.pdf -
	-t tstappend.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create append.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* C     MODIFIED FOR VAX CONVERSION BY ASM, 26 AUG 1983		*/
/* C									*/
/* C     84-11-26   ...LWK... CONVERTED TO VICAR2			*/
/* C									*/
/* C     85-5-2     ...BAM... ADDED ABILITY TO INPUT 30 INPUT DATA SETS	*/
/* C                          ( MAXIMUM VICAR ALLOWS )			*/
/* C									*/
/* C     89-4-3     BAM - INCREASED BUFFER SIZE TO 300000		*/
/* C									*/
/* 14 July 1994	SVH	Completely trashed the old program since it	*/
/*			did a lot of comparisons in a silly way, and	*/
/*			had enough flaws and missing capabilities	*/
/*			that it required a scratch rewrite.  As long	*/
/*			as I had to rewrite, I rewrote in C.		*/
/*			And also it's ported to UNIX now.		*/
#include <stdio.h>
#include <string.h>
#if UNIX_OS
#include <memory.h>
#endif
#include "taeconf.inp"
#include "vicmain_c"
#include "parblk.inc"
#include "pgminc.inc"


void main44(void)
{
  int i,j;			/* counter variables */
  int def;			/* default flag for zv routines */
  char dataline[80];
  char *buf;
  char infiles[30][80];		/* input file names */
  int u[30];			/* unit number */
  int nds;			/* number of data files */
  int nli[30];			/* number of lines in data file */
  int nsi;			/* number of samples in current input file */
  int ibytes;			/* bytes used in current format for 1 pt. */
  char fcod[10],prev_fcod[10];  /* Current file formats */
  char ofmt[10];		/* output file format */
  char error_msg[80];
  
  int oun;
  int nline=0;			/* total number of lines for output file */
  int nsamp=0;			/* number of samples on a line -output file */
  int nbyt=0;			/* Number of bytes on a line */
  int fbyt=0;			/* max # bytes in input file */
  int flag=0;			/* 0=all same, 1=mixture */

  zvparm( "INP", infiles, &nds, &def,30,80);

  /* Read all the vicar labels to find the size of the output pic. */

  for(i=0; i<nds; i++) {
    /* get parameters from this file */
    zvunit(&u[i], "INP", i+1, NULL);
    zvopen( u[i], "OPEN_ACT", "SA", "IO_ACT", "SA",NULL);
    zvget( u[i], "NL", &nli[i], "NS", &nsi, "FORMAT", fcod,NULL);
    zvpixsize(&ibytes, fcod, "LOCAL", "LOCAL");

    /* issue warning if we are mixing file types */
    if (i==0) {
      strcpy(prev_fcod,fcod);
      strcpy(ofmt,fcod);
    } else if ( strcmp(fcod,prev_fcod)) {  /* if they are _NOT_ the same type */
      zvmessage("** WARNING: YOU ARE MIXING DATA TYPES **","");
      flag=1;
      if (strcmp(ofmt,"COMP")) 		/* if old isn't complex-the maximum */
        if (!strcmp(fcod,"COMP")) 	/* if new format complex */
          strcpy(ofmt,"COMP");
        else if (!strcmp(fcod,"DOUB")) 	/* if new format double */
            strcpy(ofmt,"DOUB");
          else if (strcmp(ofmt,"DOUB"))/* if old isn't double - the 2nd max */
            if (!strcmp(fcod,"REAL")) 	/* if new format real */
              strcpy(ofmt,"REAL");
            else if (strcmp(ofmt,"REAL")) 	/* if old isn't real-3rd max */
              if (!strcmp(fcod,"FULL")) 	/* if new format full */
                strcpy(ofmt,"FULL");
              else if (strcmp(ofmt,"FULL")) 	/* if old isn't full */
                if (!strcmp(fcod,"HALF")) 	/* if new format half */
                  strcpy(ofmt,"HALF");
                else if (strcmp(ofmt,"HALF")) 	/* if old isn't half */
                  if (!strcmp(fcod,"BYTE")) 	/* if new format byte */
                    strcpy(ofmt,"BYTE");
                  else {
                    sprintf(error_msg,
				"** ERROR: UNRECOGNIZED TYPE %s **\n",fcod);
                    zvmessage(error_msg,"");
                  }
    }

    /* update final output parameters */
    nline += nli[i];
    nbyt = max(nbyt, nsi* ibytes);
    fbyt = max(fbyt, nsi* nli[i]* ibytes);
    nsamp = max( nsamp, nsi);

    /* close the file - right now it is in its own format. */
    zvclose(u[i],NULL);
  }
  buf = (char *) malloc(fbyt);

  /* Clue in the user on what's gonna happen. */
  sprintf(dataline," OUTPUT: NL=%6d, NS=%6d, FORMAT=%s.\n",nline, nsamp, ofmt);
  zvmessage(dataline,"");

  /* open and write the final output file */
  zvunit( &oun, "OUT", 1, NULL);
  zvopen( oun, "OP", "WRITE", "U_NL", nline, "U_NS", nsamp, "O_FORMAT", ofmt,
	"U_FORMAT", ofmt, "OPEN_ACT", "SA", "IO_ACT", "SA",NULL);
  for(i=0; i<nds; i++) {		/* for every file */
    zvopen( u[i], "OPEN_ACT", "SA", "IO_ACT", "SA","U_FORMAT",ofmt,NULL);
    memset(buf,0,nbyt);
    for(j=0; j<nli[i]; j++) {		/* copy over every line in the file */
      zvread (u[i],buf,NULL);
      zvwrit (oun,buf,NULL);
    }
    zvclose(u[i],NULL);
  }
  zvclose(oun,NULL);

  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create append.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM append

   To Create the build file give the command:

		$ vimake append			(VMS)
   or
		% vimake append			(Unix)


************************************************************************/


#define PROGRAM	append

#define MODULE_LIST append.c

#define MAIN_LANG_C
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create append.pdf
PROCESS HELP=*
PARM INP TYPE=STRING COUNT=2:30
PARM OUT TYPE=STRING
!# parm inp(3-30) hints=default
END-PROC
.TITLE
VICAR Program APPEND
.HELP
PURPOSE:
APPEND accepts up to thirty data sets and writes them out, one after
another, as a single data set.

EXECUTION:

Example
	APPEND INP=(A,B,C) OUT=D    will write A, B, and C out to D.

Size and parameter fields are not used.

Note:  APPEND outputs the total number of lines and samples written;
if input images are of varying data types, the "number of samples written"
will be the maximum number of samples of all the inputs. If real input 
is mixed with integer, the data type of the output will be real.
.PAGE
OPERATION:
After opening all the inputs, APPEND computes the size of the output 
data set. It will choose the format of the largest data type, where
the order (largest to smallest) is: COMP, DOUB, REAL, FULL, HALF,
and BYTE.  Each input is then copied into the output data set, in the 
same order as specified in the command.  If the inputs are of varying 
sample length, the output lines are padded with zeroes on the end.  
All output lines are of the same length, the length of the longest 
input line(maximum # of samples x largest format).
	Note that APPEND does not copy over any binary labels.

Currently BYTE, HALF, FULL, REAL, DOUB, and COMP formats are supported.
.page
HISTORY:

  WRITTEN BY:  Ron Alley, 25 October 1978
  CONVERTED TO VAX BY:  A. S. Mazer,  26 Aug. 1983
  CONVERTED TO VICAR2 BY:  L. W. Kamp,  26 Nov. 1984
  PORTED TO UNIX BY: S. V. Hwan, 20 July 1994

  COGNIZANT PROGRAMMER:  S. V. Hwan

REVISIONS: 
 Number of input images increased to 30 -- BAM --  26 Nov. 1984.

 In mixed case, chooses largest type now instead of smallest. -SVH 20 July 1994
 Switched from FORTRAN to C -- SVH -- 20 July 1994
 Added capability for DOUB and COMP data types. -- SVH -- 20 July 1994

.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to thirty are allowed.
.VARIABLE OUT
OUT specifies the output data set.  The number of lines in the output will 
be the sum of the number of lines in all the inputs.  The number of samples 
will be the maximum of the number of samples of all the inputs.  The format
of the output will the the format of the largest input data type.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstappend.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! BYTE + BYTE 
GEN        A1
LABEL-LIST A1
LIST       A1
GEN        B1
LABEL-LIST B1
LIST       B1
APPEND     (A1 B1) C
LABEL-LIST C
LIST       C

! HALF + HALF
GEN        A2 'HALF
LABEL-LIST A2
LIST       A2
GEN        B2 'HALF
LABEL-LIST B2
LIST       B2
APPEND     (A2 B2) C
LABEL-LIST C
LIST       C

! FULL + FULL
GEN        A4 'FULL
LABEL-LIST A4
LIST       A4
GEN        B4 'FULL
LABEL-LIST B4
LIST       B4
APPEND     (A4 B4) C
LABEL-LIST C
LIST       C

! REAL + REAL 
GEN        A7 'REAL4
LABEL-LIST A7
LIST       A7
GEN        B7 'REAL4
LABEL-LIST B7
LIST       B7
APPEND     (A7 B7) C
LABEL-LIST C
LIST       C

! DOUB + DOUB 
GEN        A8 'DOUB
LABEL-LIST A8
LIST       A8
GEN        B8 12 12 'DOUB
LABEL-LIST B8
LIST       B8
APPEND     (A8 B8) C
LABEL-LIST C
LIST       C

! COMP + COMP 
GEN        A9 'COMP
LABEL-LIST A9
LIST       A9
GEN        B9 4 4 'COMP
LABEL-LIST B9
LIST       B9
APPEND     (A9 B9) C
LABEL-LIST C
LIST       C

! BYTE + HALF
APPEND     (A1 B2) C
LABEL-LIST C
LIST       C

! HALF + FULL
APPEND     (A2 B4) C
LABEL-LIST C
LIST       C

! BYTE + HALF + FULL
APPEND     (A1 B2 B4) C
LABEL-LIST C
LIST       C

! BYTE + HALF + REAL
APPEND     (A1 B2 B7) C
LABEL-LIST C
LIST       C

! REAL + FULL
APPEND     (A7 B4) C
LABEL-LIST C
LIST       C

! DOUBLE + COMPLEX
! size should be 12 samples
APPEND     (B8 B9) C
LABEL-LIST C
LIST       C

! BYTE + REAL + DOUBLE
! size should be 12 samples
APPEND     (A1 B7 B8) C
LABEL-LIST C
LIST       C

! BYTE + COMPLEX
! should have 10 samples
APPEND     (A1 B9) C
LABEL-LIST C
LIST       C

end-proc
$ Return
$!#############################################################################
