$!****************************************************************************
$!
$! Build proc for MIPL module hiscale
$! VPACK Version 1.9, Monday, December 07, 2009, 16:22:40
$!
$! Execute by entering:		$ @hiscale
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module hiscale ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to hiscale.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("hiscale.imake") .nes. ""
$   then
$      vimake hiscale
$      purge hiscale.bld
$   else
$      if F$SEARCH("hiscale.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hiscale
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hiscale.bld "STD"
$   else
$      @hiscale.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hiscale.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hiscale.com -mixed -
	-s hiscale.f zhiscale.c -
	-i hiscale.imake -
	-t thiscale.f tzhiscale.c thiscale.imake thiscale.pdf tsthiscale.pdf -
	-o hiscale.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hiscale.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Compute compressed output histogram OHIST:
C   Scale the output levels by SCALE
C   Set all values below 0 DN to 0 DN
C   Set all values above 255 DN to 255 DN
C
      SUBROUTINE HISCALE(HIST,NPTS,SCALE,ohist,lsat,hsat)
      INTEGER*4 HIST(-32768:32767),OHIST(0:255)
      REAL*4 LSAT,HSAT,SCALE
      INTEGER*4 ODN

      NLOW = 0			!Number of pixels below 0 DN
      NHIGH = 0			!Number of pixels above 255 DN
C     ....Compute compressed output histogram OHIST
      CALL ZIA(OHIST,256)

      IF (NPTS.EQ.0) THEN
	LSAT = 0.0D0
	HSAT = 0.0D0
	GOTO 15
      ENDIF
	
      DO J=-32768,-1
         NLOW = NLOW + HIST(J)
      ENDDO

      DO J=0,32767
         NPIXELS = HIST(J)
         IF (NPIXELS.GT.0) THEN
            ODN = (J+.1)*SCALE		!Scale DN value for output histogram
            IF (ODN.GT.255) THEN
               NHIGH = NHIGH + NPIXELS
            ELSE
               OHIST(ODN) = OHIST(ODN) + NPIXELS
            ENDIF
         ENDIF
      ENDDO

      OHIST(0) = OHIST(0) + NLOW
      OHIST(255) = OHIST(255) + NHIGH
      LSAT = 100.0*OHIST(0)/NPTS	!Percent saturation at low DN
      HSAT = 100.0*OHIST(255)/NPTS	!Percent saturation at high DN

   15 RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zhiscale.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zhiscale(hist,npts,scale,ohist,lsat,hsat)
void   *hist,*ohist;	
int    npts;
double scale;	
float  *lsat,*hsat;	

{
float tscale;

tscale = (float) scale;
FTN_NAME2(hiscale, HISCALE) (hist,&npts,&tscale,ohist,lsat,hsat);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hiscale.imake
/* Imake file for VICAR subroutine HISCALE */

#define SUBROUTINE hiscale

#define MODULE_LIST hiscale.f zhiscale.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create thiscale.f
      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     INTEGER*4 LABUF(80)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)

      INTEGER*4 OHIST(0:255)

      LOGICAL XVPTST
      INTEGER*4 SL,SS,EL
      REAL*4 ISCALE,SCALE,LSAT,HSAT
      REAL*4 MEAN,SIGMA
      CHARACTER*8 FORMAT
C     CHARACTER*5 PROJECT
      CHARACTER*80 MSG
  800 FORMAT(' Percent saturation at low end of histogram=',F6.2)
  801 FORMAT(' Percent saturation at high end of histogram=',F6.2)
  803 FORMAT(' Mean=',F14.8,'   Sigma=',F14.8,
     &   '   Total number of pixels=',I10)
  804 FORMAT(' Histogram scale is ',F14.8,' IOF')
  805 FORMAT(' Histogram scale is ',F14.8,
     &   ' nanowatts/cm**2/steradian/nanometer')

C     ....Get unit number and open input image
      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      IF (SL+NL-1 .GT. NLI) GOTO 902
      IF (SS+NS-1 .GT. NSI) GOTO 903
      IF (NS.GT.32768) GOTO 904
      NPTS = NL*NS

      EL = SL+NL-1

      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'HALF' .AND.  FORMAT .NE. 'WORD') GOTO 990

      ITYPE = 2

C     ...Generate histogram,(replace COMPHIST2, until COMPHIST2 is ported)
C     ....Compute histogram of input image
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.

      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,
     &              'NSAMPS',NS,' ')
         DO J=1,NS
            IDN = IBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      IF (XVPTST('IOF')) ITYPE=3
      IF (XVPTST('RADIANCE')) ITYPE=4
C
C  ... The following comments need to be removed once routines are ported
C      IF (ITYPE.EQ.3.OR.ITYPE.EQ.4) THEN
C         CALL GETPROJ(IUNIT,project,icam,ifds,ind)
C         IF (IND.EQ.1) GOTO 999
C         CALL GETLABCON(IUNIT,PROJECT,labuf,ind)
C      ENDIF

       IF (.NOT.XVPTST('INCLUDE')) THEN
           NPTS = NPTS - HIST(-32768)
           HIST(-32768) = 0
       ENDIF
       CALL HISTAT2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
C
C  ... The following comments need to be removed once GETSCALE is ported
C        CALL GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
C        IF (IND.EQ.0) GOTO 999
C
C  ... The following values are hard-coded, needs to be removed when GETSCALE
C      is ported

         ISCALE = 9.9999997e-05
         OSCALE = 3.9062500e-03

         MEAN =ISCALE*MEAN
         SIGMA = ISCALE*SIGMA
 	 SCALE = ISCALE/OSCALE

         CALL Xvmessage('Test from FORTRAN',' ')
         CALL HISCALE(HIST,NPTS,SCALE,ohist,lsat,hsat)
C     ....Print number of pixels saturated at low and high ends
      IF (LSAT+HSAT.GT.0) THEN
         WRITE(MSG,800) LSAT
         CALL Xvmessage(MSG,' ')
         WRITE(MSG,801) HSAT
         CALL Xvmessage(MSG,' ')
      ENDIF
      CALL PRNT(4,256,OHIST,'Output histogram=. ')

         CALL Xvmessage('Test from C',' ')
         lsat=0.0
         hsat=0.0
         CALL tzhiscale(HIST,NPTS,SCALE,ohist,lsat,hsat)
C     ....Print number of pixels saturated at low and high ends
      IF (LSAT+HSAT.GT.0) THEN
         WRITE(MSG,800) LSAT
         CALL Xvmessage(MSG,' ')
         WRITE(MSG,801) HSAT
         CALL Xvmessage(MSG,' ')
      ENDIF
      CALL PRNT(4,256,OHIST,'Output histogram=. ')

      MAXFREQ = 0
      DO I=1,254
         NPIXELS = OHIST(I)
         IF (NPIXELS.GT.MAXFREQ) MAXFREQ=NPIXELS
      ENDDO


C     ....Print mean, standard deviation, and total number of pixels
      WRITE (MSG,803) MEAN,SIGMA,NPTS
      CALL Xvmessage(MSG,' ')
C     ....Print histogram scale
      IF (ITYPE.LE.2) THEN
         CALL Xvmessage(' Histogram scale is DN',' ')
      ELSE IF (ITYPE.EQ.3) THEN
         WRITE (MSG,804) OSCALE
         CALL Xvmessage(MSG,' ')
      ELSE
         WRITE (MSG,805) OSCALE
         CALL Xvmessage(MSG,' ')
      ENDIF

      RETURN

  902 CALL Xvmessage(' *** # lines requested exceeds input size',' ')
      GOTO 999
  903 CALL Xvmessage(' *** # samples requested exceeds input size', ' ')
      GOTO 999
  904 CALL Xvmessage(' ***Input image #samples exceeds limit', ' ')
      GOTO 999
  990 CALL Xvmessage(' ***Invalid input data format',' ')
      CALL Xvmessage(' ***Inputs must be halfword',' ')
      GOTO 999
  999 CALL Xvmessage(' THISCALE task cancelled', ' ')
      CALL ABEND
      END
$!-----------------------------------------------------------------------------
$ create tzhiscale.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzhiscale)(hist,npts,scale,ohist,lsat,hsat) 
void *hist,*ohist;
int *npts;
float *lsat, *hsat, *scale;
{
/*  ============================================  */
      zhiscale(hist,*npts,*scale,ohist,lsat,hsat);
}
$!-----------------------------------------------------------------------------
$ create thiscale.imake
/* Imake file for Test of VICAR subroutine HISCALE */

#define PROGRAM thiscale

#define MODULE_LIST thiscale.f tzhiscale.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create thiscale.pdf
process help=*
PARM INP      TYPE=STRING
PARM SIZE     TYPE=INTEGER COUNT=4     			DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1				DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1				DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1				DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1				DEFAULT=0
PARM IOF      TYPE=KEYWORD COUNT=(0:1)  VALID=IOF		DEFAULT=--
PARM RADIANCE TYPE=KEYWORD COUNT=(0:1)  VALID=RADIANCE		DEFAULT=--
PARM INCLUDE  TYPE=KEYWORD COUNT=(0:1)  VALID=INCLUDE		DEFAULT=--
END-PROC
$!-----------------------------------------------------------------------------
$ create tsthiscale.pdf
procedure
Refgbl $SYSCHAR
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "This is a test of subroutine HISCALE"

Local PATH STRING
if ($SYSCHAR(1)="VAX_VMS")
    let PATH="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
else
    let PATH="/project/test_work/testdata/vgr/"
end-if

WRITE "Test on Voyager halfword flight image"
thiscale "&PATH"f1636832.fic  'IOF 
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create hiscale.hlp
1 HISCALE

  Purpose: HISCALE compresses a 64K grey-level histogram of a halfword image
  to 256 grey levels by scaling the DN values.  See also GETSCALE.

  Calling Sequence:

	CALL HISCALE(HIS,NPXLS,SCALE,OHIS,LSAT,HSAT)
  where
	INTEGER*4 HIS(65536)	!Input histogram
	INTEGER*4 NPXLS		!Total number of pixels in histogram (input)
	REAL*4 SCALE		!Scale applied to histogram (input)
	INTEGER*4 OHIS(256)	!Output histogram
	REAL*4 LSAT,HSAT	!Percent of pixels saturated at low and hight
				!ends of the histogram (outputs)

  The subroutines HISCALE and GETSCALE are used to prepare histograms for
  display.  See program HICCUP for an example of their use.

2 Operation

  The input histogram is assumed to cover the full halfword (16-bit integer)
  range, where

	HIS(1) = number of pixels at -32768 DN
	HIS(2) = number of pixels at -32767 DN
          .    .   .
          .    .   .
          .    .   .
	HIS(65536) = number of pixels at +32767 DN

  HISCALE rescales the histogram so that it is equivalent to the histogram of
  an image whose pixels have been multiplied by the given scale factor, and
  truncated so that all DN values less than 0 are replaced by 0, and all DN
  values greater than 255 are replaced by 255.  The elements of OHIS are
  defined as follows:

	OHIS(1) = number of pixels at 0 DN
	OHIS(2) = number of pixels at 1 DN
          .    .   .
          .    .   .
	OHIS(256) = number of pixels at 255 DN

  Upon return, LSAT and HSAT will contain the percentage of pixels truncated
  at the low and high ends of the histogram (i.e. LSAT=percentage less than
  0 DN and HSAT=percentage greater than 255 DN).

2 History

Original Programmer: Gary Yagi, Jan 22, 1990
Current Cognizant Programmer: Gary Yagi
Source Language: FORTRAN

Revisions:	April 01, 1992		JFM059		FR 75746
		Test of NPTS parameter to avoid divide by zero error.

12 NOV 92  FFM  Made portable, revised test program
24 MAR 93  FFM  Modified HISCALE's IMAKE file, changed subroutine's name from
                upper case to lower case, so it will delete the IMAKE file, etc,
                after it is built.
12 JUL 93  FFM  Commented out LIB_LOCAL in THISCALE.IMAKE file. (FR 81858)
03 Feb 98  GMY  Included min and max DN values in LSAT and HSAT calculations.
01 Apr 98  GMY  Corrected alpha pathname in test file (AR9703)
$ Return
$!#############################################################################
