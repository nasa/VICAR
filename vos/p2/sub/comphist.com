$!****************************************************************************
$!
$! Build proc for MIPL module comphist
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:07
$!
$! Execute by entering:		$ @comphist
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
$ write sys$output "*** module comphist ***"
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
$ write sys$output "Invalid argument given to comphist.com file -- ", primary
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
$   if F$SEARCH("comphist.imake") .nes. ""
$   then
$      vimake comphist
$      purge comphist.bld
$   else
$      if F$SEARCH("comphist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake comphist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @comphist.bld "STD"
$   else
$      @comphist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create comphist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack comphist.com -mixed -
	-s comphist.f zcomphist.c -
	-i comphist.imake -
	-t tcomphist.f tzcomphist.c tcomphist.imake tcomphist.pdf -
	   tstcomphist.pdf -
	-o comphist.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create comphist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Compute a histogram of a byte image.
C
      SUBROUTINE COMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
      INTEGER*4 OHIST(256)
      INTEGER*2 IBUF(32768)
      INTEGER*4 SL,SS,EL

      CALL ZIA(OHIST,256)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         CALL HSUB(1,NS,IBUF,OHIST,0,255)
      ENDDO
      RETURN
      END
C Compute a histogram of a halfword image.
C
      SUBROUTINE COMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)
      INTEGER*4 SL,SS,EL

      CALL ZIA(HIST,65536)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS
            IDN = IBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zcomphist.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zcomphist(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    iunit,sl,ss,nl,ns;
{
FTN_NAME2(comphist, COMPHIST) (&iunit,&sl,&ss,&nl,&ns,ohist,ibuf);
}
void   zcomphist2(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    iunit,sl,ss,nl,ns;
{
FTN_NAME2(comphist2, COMPHIST2) (&iunit,&sl,&ss,&nl,&ns,ohist,ibuf);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create comphist.imake
/* Imake file for VICAR subroutine comphist */

#define SUBROUTINE comphist

#define MODULE_LIST comphist.f zcomphist.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tcomphist.f
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
         CALL Xvmessage(' ***Test from Fortran', ' ')
         CALL COMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
         CALL histat(OHIST,NPTS,mean,sigma,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
CCC
         CALL Xvmessage(' ***Test from C', ' ')
         CALL tzCOMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
         CALL histat(OHIST,NPTS,mean,sigma,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
CCC
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         CALL Xvmessage(' ***Test from Fortran', ' ')
         CALL COMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
CCC
         CALL Xvmessage(' ***Test from C', ' ')
         CALL tzCOMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
         WRITE (MSG,803) MEAN,SIGMA,NPTS
         Call Xvmessage(msg, ' ')
      ELSE
         GOTO 990
      ENDIF



      RETURN

  902 CALL Xvmessage(' *** # lines requested exceeds input size',' ')
      GOTO 999
  903 CALL Xvmessage(' *** # samples requested exceeds input size', ' ')
      GOTO 999
  904 CALL Xvmessage(' ***Input image #samples exceeds limit', ' ')
      GOTO 999
  990 CALL Xvmessage(' ***Invalid input data format',' ')
      CALL Xvmessage(' ***Inputs must be byte or halfword',' ')
      GOTO 999
  999 CALL Xvmessage(' thistat task cancelled', ' ')
      CALL ABEND
      END

$!-----------------------------------------------------------------------------
$ create tzcomphist.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzcomphist)(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    *iunit,*sl,*ss,*nl,*ns;
{
/*  ============================================  */

      zcomphist(*iunit,*sl,*ss,*nl,*ns,ohist,ibuf);
}
/************************************************************************/
void FTN_NAME(tzcomphist2)(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    *iunit,*sl,*ss,*nl,*ns;
{
/*  ============================================  */

      zcomphist2(*iunit,*sl,*ss,*nl,*ns,ohist,ibuf);
}
$!-----------------------------------------------------------------------------
$ create tcomphist.imake
/* Imake file for Test of VICAR subroutine comphist */

#define PROGRAM tcomphist

#define MODULE_LIST tcomphist.f tzcomphist.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tcomphist.pdf
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
$ create tstcomphist.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "This is a test of subroutine comphist"

GEN A 128 128
tcomphist A  (4,4,100,100) 'ZEROES

WRITE "Test on halfword image (ramp)"
GEN A 1000 1000 'HALF
tcomphist A 'ZEROES

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create comphist.hlp
1 COMPHIST

  COMPHIST and COMPHIST2 compute the histogram of a byte and halfword image,
  respectively.

  Calling Sequence:

	CALL COMPHIST(IUNIT,SL,SS,NL,NS,HIS,IBUF)
	CALL COMPHIST2(IUNIT,SL,SS,NL,NS,HIS2,IBUF2)

  where
	INTEGER*4 IUNIT		!Logical unit number of image
	INTEGER*4 SL,SS,NL,NS	!Size field of image area used
	INTEGER*4 HIS(256)	!Output 256 grey-level histogram
	INTEGER*4 HIS2(256)	!Output 64K grey-level histogram
	LOGICAL*1 IBUF(NS)	!Buffer for reading byte image
	INTEGER*2 IBUF2(NS)	!Buffer for reading halfword image

  IUNIT,SL,SS,NL,NS are input arguments.  HIS,HIS2,IBUF,IBUF2 are outputs.
  IBUF and IBUF2 must be large enough to hold NS samples of the byte or
  halfword image, respectively.

2 Operation

  The input image must be opened with U_NS defaulted (i.e. no data
  conversion is permitted).  COMPHIST will compute the histogram by
  reading in the specified area of the image.  The histogram of a byte
  image will contain 256 elements, representing the sample frequency
  for DN values 0 to 255.  The histogram of a halfword image will
  contain 64K elements, representing the sample frequency for DN
  values -32768 to 32767.

  See program HICCUP for an example of usage of these subroutines.

2 History

  Original Programmer: Gary Yagi, Jan 22, 1990
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN
  Revisions: FFM ...4/21/92... Minor modifications to TEST PDF
                                (FR 63263)
  5 JAN 93  FFM   Made portable, revised test program
  21 Sep 99 GMY   Added ilow,ihigh args to hsub calls for portability.
$ Return
$!#############################################################################
