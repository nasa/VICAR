$!****************************************************************************
$!
$! Build proc for MIPL module histat
$! VPACK Version 1.9, Monday, December 07, 2009, 16:22:51
$!
$! Execute by entering:		$ @histat
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
$ write sys$output "*** module histat ***"
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
$ write sys$output "Invalid argument given to histat.com file -- ", primary
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
$   if F$SEARCH("histat.imake") .nes. ""
$   then
$      vimake histat
$      purge histat.bld
$   else
$      if F$SEARCH("histat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histat.bld "STD"
$   else
$      @histat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histat.com -mixed -
	-s histat.f zhistat.c -
	-i histat.imake -
	-t thistat.f tzhistat.c thistat.imake thistat.pdf tsthistat.pdf -
	-o histat.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create histat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE histat(OHIST,NPTS,mean,sigma,maxfreq)
C   Compute mean and standard deviation (for byte inputs only).
C
      INTEGER*4 OHIST(0:255)
      REAL*4 MEAN,SIGMA
      REAL*8 DMEAN,DSIGMA

      DMEAN = 0.0D0		!Mean DN of input image
      DSIGMA = 0.0D0		!Standard deviation of input image
      MAXFREQ = 0

      IF (NPTS.EQ.0) THEN	!Test number of pixels against 0
	MEAN = DMEAN            !Set mean, std. dev. & max. freq.
	SIGMA = DSIGMA		!to 0; then exit routine.
        GOTO 15
      ENDIF 

      DO 10 J=0,255
      NPIXELS = OHIST(J)
      IF (NPIXELS.EQ.0) GOTO 10
      DN = J
      DMEAN = DMEAN + NPIXELS*DN
      DSIGMA = DSIGMA + NPIXELS*DN**2
      IF (J.NE.0.AND.J.NE.255.AND.NPIXELS.GT.MAXFREQ) MAXFREQ=NPIXELS
   10 CONTINUE
    
      DMEAN = DMEAN/NPTS
      MEAN = DMEAN
      SIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
   15 RETURN
      END
C
C   Compute mean and standard deviation (for halfword inputs only).
C
      SUBROUTINE histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 MEAN,SIGMA
      REAL*8 DMEAN,DSIGMA

      DN = 0.0			!DN value
      DMEAN = 0.0D0		!Mean DN of input image
      DSIGMA = 0.0D0		!Standard deviation of input image
      MINDN = 32767
      MAXFREQ = 0

      IF (NPTS.EQ.0) THEN	!Test number of pixels against 0
	MEAN = DMEAN		!Set mean, std. dev. & max. freq.
	SIGMA = DSIGMA		!to 0; set max. and min. DN to 
	MAXDN = MINDN		!32767; then exit program.
        GOTO 15
      ENDIF 

      DO 10 J=-32768,32767
      NPIXELS = HIST(J)
      IF (NPIXELS.EQ.0) GOTO 10
      DN = J
      DMEAN = DMEAN + NPIXELS*DN
      DSIGMA = DSIGMA + NPIXELS*DN**2
      IF (J.LT.MINDN) MINDN=J
      IF (J.NE.-32768.AND.J.NE.32767.AND.NPIXELS.GT.MAXFREQ)
     &		 MAXFREQ=NPIXELS
   10 CONTINUE

      MAXDN = DN + 0.5
      DMEAN = DMEAN/NPTS
      MEAN = DMEAN
      SIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
   15 RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zhistat.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void   zhistat(ohist,npts,mean,sigma,maxfreq)
void   *ohist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *maxfreq;	

{
FTN_NAME2(histat, HISTAT) (ohist, &npts, mean, sigma, maxfreq);
}
void   zhistat2(hist,npts,mean,sigma,mindn,maxdn,maxfreq)
void   *hist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *mindn,*maxdn,*maxfreq;	

{
FTN_NAME2(histat2, HISTAT2) (hist, &npts, mean, sigma,mindn,maxdn, maxfreq);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histat.imake
/* Imake file for VICAR subroutine histat */

#define SUBROUTINE histat

#define MODULE_LIST histat.f zhistat.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create thistat.f
      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      COMMON/C1/HIST(-32768:32767),IBUF(32768)
      INTEGER*4 HIST
      INTEGER*2 IBUF

      COMMON/C2/OBUF(10),OHIST(0:255)	!Histogram record data structure
      INTEGER*4 OHIST
      INTEGER*4 BUF(10)
      EQUIVALENCE (BUF,OBUF)

      LOGICAL XVPTST
      INTEGER*4 SL,SS,EL
      REAL*4 MEAN,SIGMA
      CHARACTER*8 FORMAT
      CHARACTER*80 MSG


  803 FORMAT(' Mean=',F14.8,'   Sigma=',F14.8,
     &   '   Total number of pixels=',I10)

C     ....Get unit number and open input image
      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      IF (SL+NL-1 .GT. NLI) GOTO 902
      IF (SS+NS-1 .GT. NSI) GOTO 903
      IF (NS.GT.32768) GOTO 904
      NPTS = NL*NS
C     ....Compute histogram of input image.  If the image is byte
C     ....the histogram is stored directly in OHIST.  If halfword, 
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.

      EL = SL+NL-1
      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.EQ.'BYTE') THEN
         ITYPE = 1
C     ....Generate histogram
         DO LINE=SL,EL
            CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,
     &                 'NSAMPS',NS,' ')
            CALL HSUB(1,NS,IBUF,OHIST)
         ENDDO
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         ITYPE = 2
         DO LINE=SL,EL
            CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,
     &                 'NSAMPS',NS,' ')
            DO J=1,NS
              IDN = IBUF(J)
              HIST(IDN) = HIST(IDN) + 1
            ENDDO
         ENDDO
      ELSE
         GOTO 990
      ENDIF

      IF (ITYPE.EQ.1) THEN
         CALL histat(OHIST,NPTS,mean,sigma,maxfreq)
C     ....Print mean, standard deviation, and total number of pixels
c         CALL QPRINT(' ***Test from Fortran')
          CALL XVMESSAGE(' ***Test from Fortran',' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c        CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
         mean=0.0
         sigma=0.0
         maxfreq=0
         CALL tzhistat(OHIST,NPTS,mean,sigma,maxfreq)
c         CALL QPRINT(' ***Test from C')
         CALL Xvmessage(' ***Test from C', ' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c         CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
      ELSE
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
C     ....Print mean, standard deviation, and total number of pixels
c         CALL QPRINT(' ***Test from Fortran')
         CALL Xvmessage(' ***Test from Fortran', ' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c         CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
         mean=0.0
         sigma=0.0
         mindn=0
         maxdn=0
         maxfreq=0
         CALL tzhistat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
c         CALL QPRINT(' ***Test from C')
         CALL Xvmessage(' ***Test from C', ' ')
         WRITE (MSG,803) MEAN,SIGMA,NPTS
c         CALL QPRINT(MSG,80)
         Call Xvmessage(msg, ' ')
      ENDIF

      RETURN

c  902 CALL QPRINT(' ***Number of lines requested exceeds input size')
  902 CALL Xvmessage(' *** # lines requested exceeds input size',' ')
      GOTO 999
c  903 CALL QPRINT(' ***Number of samples requested exceeds input size')
  903 CALL Xvmessage(' *** # samples requested exceeds input size', ' ')
      GOTO 999
c  904 CALL QPRINT(' ***Input image number of samples exceeds limit')
  904 CALL Xvmessage(' ***Input image #samples exceeds limit', ' ')
      GOTO 999
c  990 CALL QPRINT(' ***Invalid input data format')
  990 CALL Xvmessage(' ***Invalid input data format',' ')
c      CALL QPRINT(' ***Inputs must be byte or halfword')
      CALL Xvmessage(' ***Inputs must be byte or halfword',' ')
      GOTO 999
c  999 CALL QPRINT(' thistat task cancelled')
  999 CALL Xvmessage(' thistat task cancelled', ' ')
      CALL ABEND
      END
$!-----------------------------------------------------------------------------
$ create tzhistat.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzhistat)(ohist,npts,mean,sigma,maxfreq) 
void *ohist;
int *npts,*maxfreq;
float *mean, *sigma;
{
/*  ============================================  */

      zhistat(ohist,*npts,mean,sigma,maxfreq );
}
/************************************************************************/
void FTN_NAME(tzhistat2)(hist,npts,mean,sigma,mindn,maxdn,maxfreq) 
void *hist;
int *npts,*mindn,*maxdn,*maxfreq;
float *mean, *sigma;
{
/*  ============================================  */

      zhistat2(hist,*npts,mean,sigma,mindn,maxdn,maxfreq );
}
$!-----------------------------------------------------------------------------
$ create thistat.imake
/* Imake file for Test of VICAR subroutine histat */

#define PROGRAM thistat

#define MODULE_LIST thistat.f tzhistat.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL

#define LIB_TAE
#define LIB_LOCAL
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create thistat.pdf
process help=*
PARM INP      TYPE=STRING
PARM SIZE     TYPE=INTEGER COUNT=4     			DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1				DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1				DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1				DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1				DEFAULT=0
PARM ZEROES   TYPE=KEYWORD COUNT=(0:1)  VALID=ZEROES		DEFAULT=--
PARM INCLUDE  TYPE=KEYWORD COUNT=(0:1)  VALID=INCLUDE		DEFAULT=--
END-PROC
$!-----------------------------------------------------------------------------
$ create tsthistat.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "This is a test of subroutine histat"

GEN A 128 128
thistat A  (4,4,100,100) 'ZEROES

WRITE "Test on halfword image (ramp)"
GEN A 1000 1000 'HALF
thistat A 'ZEROES

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create histat.hlp
1 histat,histat2

Given the histogram, subroutines histat and histat2 compute the mean and
standard-deviation of a byte and halfword image, respectively.  histat2
will also return the minimum and maximum DN value in the image.

Calling Sequence:

    CALL histat(HIS,NPTS,mean,sigma,maxfreq)
    CALL histat2(HIS2,NPTS,mean,sigma,mindn,maxdn,maxfreq)
where
    INTEGER*4 HIS(256) is the histogram of the byte image (input).
    INTEGER*4 HIS2(65536) is the histogram of the halfword image (input).
    INTEGER*4 NPTS is the number of pixels in the image (input).
    REAL*4 MEAN,SIGMA are the output mean and standard-deviation.
    INTEGER*4 MINDN,MAXDN are the output minimum and maximum DN.
    INTEGER*4 MAXFREQ is the maximum frequency in HIS or HIS2, excluding
       the end-points (0 and 255 DN or -32768 and 32767 DN).

2 OPERATION

HIS2 must extend of the full halfword DN range (i.e. HIS2(1) must be
the sample frequency at -32768 DN and HIS(65536) must be the sample
frequency at +32767 DN).

If the number of pixels in the image, NPTS, is zero, then the mean, standard
deviation and maximum frequency are set to zero, (MEAN, SIGMA, MAXFREQ).  
The output minimum and maximum DNs for histat2 are set to 32767.

3 History

  Original Programmer: Gary Yagi, Jan 22, 1990
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN
  Revisions:
   10 APR 90  GMY   Exclude end-points in MAXFREQ.
   01 APR 92  JFM   NPTS parameter check for zero to avoid divide by zero
   25 OCT 92  FFM   Made portable, revised test program

$ Return
$!#############################################################################
