$!****************************************************************************
$!
$! Build proc for MIPL module astrch
$! VPACK Version 1.9, Monday, December 07, 2009, 16:07:39
$!
$! Execute by entering:		$ @astrch
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
$ write sys$output "*** module astrch ***"
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
$ write sys$output "Invalid argument given to astrch.com file -- ", primary
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
$   if F$SEARCH("astrch.imake") .nes. ""
$   then
$      vimake astrch
$      purge astrch.bld
$   else
$      if F$SEARCH("astrch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astrch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astrch.bld "STD"
$   else
$      @astrch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astrch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astrch.com -mixed -
	-s astrch.f zastrch.c -
	-i astrch.imake -
	-t tastrch.f tzastrch.c tastrch.imake tastrch.pdf tstastrch.pdf -
	-o astrch.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create astrch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute low and high stretch limits by saturating lper and hper of histogram.
C
      SUBROUTINE ASTRCH(HIS,LOWDN,HIGHDN,LPER,HPER,N)
      REAL*4 LPER,HPER
      INTEGER HIS(1),LOWDN,HIGHDN,N
      INTEGER LSUM,HSUM,FSUM,SUM,I,ILO,IHI
C     ....Compute number of pixels to be saturated at LPER and HPER
      CALL SUMV(4,N,HIS,FSUM,1)
      IF (FSUM.EQ.0) THEN
         LOWDN = 0
         HIGHDN = 1
         RETURN
      ENDIF
      LSUM = (FSUM*LPER)*.01 + 0.5
      HSUM = (FSUM*HPER)*.01 + 0.5
C     ....Position index at first non-zero entry
      I = 1
    5 IF (HIS(I).GT.0) GOTO 6
      I = I + 1
      IF (I.LT.N) GOTO 5
      LOWDN = N - 2
      HIGHDN = N - 1
      RETURN

    6 SUM0 = HIS(I)
C
   10 SUM = SUM0 + HIS(I+1)
      IF (SUM.GE.LSUM) THEN
         IF (LSUM-SUM0.GT.SUM-LSUM) I=I+1
         GOTO 20
      ENDIF
      SUM0 = SUM
      I = I + 1
      IF (I.LT.N-1) GOTO 10

   20 ILO = I
      I = N
   25 IF (HIS(I).GT.0) GOTO 26
      I = I - 1
      GOTO 25

   26 SUM0 = HIS(I)

   30 SUM = SUM0 + HIS(I-1)
      IF (SUM.GT.HSUM) THEN
         IF (HSUM-SUM0.GT.SUM-HSUM) I=I-1
         GOTO 40
      ENDIF
      SUM0 = SUM
      I = I - 1
      IF (I.GT.2) GOTO 30

   40 IHI = I
C     ....Indices start at 1, DNs start at 0
      LOWDN  = ILO - 1
      HIGHDN = IHI - 1
C     ....Make sure low and high stretch limits are'nt equal
      IF (LOWDN.LT.HIGHDN) RETURN
      IF (LOWDN.GT.0) THEN
         LOWDN = LOWDN - 1
      ELSE
         HIGHDN = HIGHDN + 1
      ENDIF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zastrch.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of ASTRCH                                         */
/************************************************************************/

void zastrch(his,lowdn,highdn,lper,hper,n)
void  *his;   /* input histogram; 1D array of int (input) */
int   *lowdn;  /* output low-end stretch limit (output) */
int   *highdn; /* output high-end stretch limit (output) */
double lper;   /* low-end percent saturation (input) */
double hper;   /* high-end percent saturation (input) */
int    n;      /* dimension of his (input) */

{
float f_lper, f_hper;
f_lper = (float) lper;
f_hper = (float) hper;

FTN_NAME2(astrch, ASTRCH) (his,lowdn,highdn,&f_lper,&f_hper,&n);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create astrch.imake
/* Imake file for VICAR subroutine ASTRCH */

#define SUBROUTINE astrch

#define MODULE_LIST astrch.f zastrch.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tastrch.f
C*****************************************************************************
C Unit test program TASTRCH.F for subroutine ASTRCH
C Ported to UNIX 11/10/1992
C*****************************************************************************
      include 'VICMAIN_FOR'
      subroutine main44
      integer*4 h(256),lowdn,highdn,nlev,i
      real*4 lper,hper

C Test the Fortan-callable subroutine
      do i=1,256
         h(i)=5
      end do
      lowdn=0
      highdn=0
      lper=25.0
      hper=25.0
      nlev=256
      call astrch(h,lowdn,highdn,lper,hper,nlev)
      call xvmessage('Testing Fortran-callable subroutine',' ')
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')
      call zia(h,256)
      h(256) = 100
      call astrch(h,lowdn,highdn,lper,hper,nlev)
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')

c Test the C-bridge....
      do i=1,256
         h(i)=5
      end do
      lowdn=0
      highdn=0
      lper=25.0
      hper=25.0
      nlev=256
      call tzastrch(h,lowdn,highdn,lper,hper,nlev)
      call xvmessage('Testing C-callable subroutine',' ')
      call xvmessage('Results should be identical',' ')   
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')
      call zia(h,256)
      h(256) = 100
      call tzastrch(h,lowdn,highdn,lper,hper,nlev)
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')
      return
      end
$!-----------------------------------------------------------------------------
$ create tzastrch.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TASTRCH.F */
/************************************************************************/
void FTN_NAME(tzastrch) (his,lowdn,highdn,lper,hper,n)
void  *his;   /* input histogram; 1D array of int (input) */
int   *lowdn;  /* output low-end stretch limit (output) */
int   *highdn; /* output high-end stretch limit (output) */
float *lper;   /* low-end percent saturation (input) */
float *hper;   /* high-end percent saturation (input) */
int   *n;      /* dimension of his (input) */

{
      zastrch(his,lowdn,highdn,*lper,*hper,*n);
}



$!-----------------------------------------------------------------------------
$ create tastrch.imake
/* Imake file for Test of VICAR subroutine astrch */

#define PROGRAM tastrch

#define MODULE_LIST tastrch.f tzastrch.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tastrch.pdf
!*****************************************************************************
! TASTRCH.PDF - pdf for test program TASTRCH.F for the subroutine ASTRCH
!*****************************************************************************
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstastrch.pdf
!****************************************************************************
! TSTASTRCH.PDF, unit test procedure for subroutine ASTRCH.F
!****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="no"
write "Test subroutine ASTRCH"
write "     There are 256 levels in the histogram"
write "     Each level has frequency 5"
write "     After saturating 25% at the low and high ends,"
write "     the results should be LOWDN=63 and HIGHDN=192"
write " "
write "     2nd test: When all DNs are at 255"
write "     Results should be LOWDN=254, HIGHDN=255"
let $echo="yes"
TASTRCH
end-proc
.title TSTASTRCH.PDF - unit test for subroutine ASTRCH
.help
This unit test creates no files and uses little CPU. Just run it in batch,
with no parameters, and verify the output.
.end
$ Return
$!#############################################################################
$Other_File:
$ create astrch.hlp
1 ASTRCH

  ASTRCH is a UNIX-ported, FORTRAN and C callable subroutine for determining 
  the linear stretch limits of a picture which will saturate a specifiable
  percentage of the pixels at the low and high end of its histogram.

  FORTRAN Calling Sequence:

	CALL ASTRCH(H,LOWDN,HIGHDN,LPER,HPER,NLEV)

	INTEGER*4 H(NLEV)	Input histogram
	INTEGER*4 LOWDN,HIGHDN  Output stretch limits
	REAL*4 LPER,HPER	Low and high percent saturation (input)
        INTEGER*4 NLEV		Number of levels in histogram (input)

  C Calling Sequence:
        
        zastrch(*his,*lowdn,*highdn,lper,hper,nlev);
        
        void   his;    /* input histogram; 1D array of int  (input) */
        int    lowdn;  /* output low-end stretch limit      (output) */
        int    highdn; /* output high-end stretch limit     (output) */
        double lper;   /* low-end percent saturation        (input) */
        double hper;   /* high-end percent saturation       (input) */
        int    nlev;   /* dimension of his                  (input) */
   
        NOTE: lper and hper are casted to floats within the C-bridge.

2 Operation

  The histogram of a picture is a grey level frequency table such that:

      H(i) = number of occurences of pixels with DN value i,
             for i=1,1,2,...,NLEV

  Define an area function A(i,j), representing the area under the histogram
  between indices i and j.  That is, A(i,j) is the sum of all H(k),
  for k=i,i+1,i+2,...,j.

  Then A(1,NLEV) represents the total area under the histogram (i.e. the total
  number of pixels in the image).  ASTRCH determines two indices ILOW and 
  IHIGH such that:

      a) ILOW is the largest i which satisfies the equation
 	        A(1,i) < LPER*A(1,NLEV)
      b) IHIGH is the smallest i which satisfies the equation
	        A(i,NLEV) < HPER*A(1,NLEV)

  Since the indices start at 1 and DNs start at 0, the output variables
  LOWDN and HIGHDN are calculated as LOWDN=ILOW-1 and HIGHDN=IHIGH-1.

2 History

  Original Programmer: Gary Yagi 03-11-80
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN and C
  Program History:
    24 OCT 79   GMY   INITIAL RELEASE
    26 MAY 84   FFM   CONVERT TO VAX
    26 MAR 91   GMY   Make algorithm compatible to VIDS
    10 NOV 92   MOS   Ported to UNIX, updated help file
    27 Jul 99   GMY   Fix case where LPER=HPER=0
    07 Oct 99   GMY   Fix case where all pixels are 255 DN
$ Return
$!#############################################################################
