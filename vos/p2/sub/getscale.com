$!****************************************************************************
$!
$! Build proc for MIPL module getscale
$! VPACK Version 1.9, Monday, December 07, 2009, 16:21:08
$!
$! Execute by entering:		$ @getscale
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
$ write sys$output "*** module getscale ***"
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
$ write sys$output "Invalid argument given to getscale.com file -- ", primary
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
$   if F$SEARCH("getscale.imake") .nes. ""
$   then
$      vimake getscale
$      purge getscale.bld
$   else
$      if F$SEARCH("getscale.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getscale
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getscale.bld "STD"
$   else
$      @getscale.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getscale.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getscale.com -mixed -
	-s getscale.f zgetscale.c -
	-i getscale.imake -
	-t tgetscale.f tzgetscale.c tgetscale.imake tgetscale.pdf -
	   tstgetscale.pdf -
	-o getscale.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getscale.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C If input image is halfword, get scale of output histogram:
C   ITYPE=3 if IOF is specified.
C        =4 if RADIANCE is specified.
C        =2 otherwise.
C   ISCALE=input image scale:
C         =IOF value used in FICOR*10-4 if MAXIOF is specified,
C          or CONV value used in FICOR*10-9 if MAXRAD is specified,
C          or 1.0 if neither MAXIOF or MAXRAD is specified.
C   OSCALE=output histogram scale
C
      SUBROUTINE GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
      REAL*4 ISCALE
      REAL*4 LABUF(80)		!GETLABCON buffer
      REAL*4 MAXIOF,MAXRAD
      REAL*4 MAXS(40)/.00001,.00002,.00004,.00005,
     &		     .0001,.0002,.0004,.0005,
     &		     .001,.002,.004,.005,
     &		     .01,.02,.04,.05,
     &               .1,.2,.4,.5,
     &               1.,2.,4.,5.,
     &               10.,20.,40.,50.,
     &               100.,200.,400.,500.,
     &               1000.,2000.,4000.,5000.,
     &               10000.,20000.,40000.,50000./
      IND = 1
      IF (ITYPE.EQ.3) GOTO 30
      IF (ITYPE.EQ.4) GOTO 40
C     ....Here if ITYPE=2
      ISCALE = 1.0
      IDN = 256
      DO I=1,8
         IF (MAXDN.LE.IDN) GOTO 20
         IDN = 2*IDN
      ENDDO
   20 OSCALE = IDN/256.
      RETURN
C
C     ....Here if ITYPE=3
   30 ISCALE = LABUF(28)
      IF (ISCALE.EQ.0.0) GOTO 995
      MAXIOF = ISCALE*MAXDN		!Compute maximum IOF in histogram
      IF (MAXIOF.GT.1.0) MAXIOF=1.0	!Physical upper limit
      DO I=6,21
         IF (MAXIOF.LE.MAXS(I)) GOTO 31
      ENDDO
   31 IF (I.EQ.8) I=9
      MAXIOF = MAXS(I)
      OSCALE = MAXIOF/256.
      RETURN
C
C     ....Here if ITYPE=4
   40 ISCALE = LABUF(29)*1.E+12		!Get radiance scale of input image
      IF (ISCALE.EQ.0.0) GOTO 996
      MAXRAD = ISCALE*MAXDN		!Compute maximum radiance in histogram
      DO I=1,40
         IF (MAXRAD.LE.MAXS(I)) GOTO 41
      ENDDO
   41 MAXRAD = MAXS(I)
      OSCALE = MAXRAD/256.
      RETURN
C
  995 CALL XVMESSAGE(' ***Invalid use of IOF scale',' ')
      GOTO 998
  996 CALL XVMESSAGE(' ***Invalid use of RADIANCE scale',' ')
  998 CALL XVMESSAGE(' Input image was not radiometrically corrected',
     &' ')
      IND = 0
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zgetscale.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zgetscale(itype,labuf,maxdn,iscale,oscale,ind)
void   *labuf;	
int    itype,*ind,maxdn;
float  *iscale,*oscale;	

{
FTN_NAME2(getscale, GETSCALE) (&itype,labuf,&maxdn,iscale,oscale,ind);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getscale.imake
/* Imake file for VICAR subroutine getscale */

#define SUBROUTINE getscale

#define MODULE_LIST getscale.f zgetscale.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tgetscale.f
      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      INTEGER*4 LABUF(80)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)

      INTEGER*4 OHIST(0:255)

      LOGICAL XVPTST
      INTEGER*4 SL,SS
      REAL*4 ISCALE,SCALE,LSAT,HSAT
      REAL*4 MEAN,SIGMA
      CHARACTER*8 FORMAT
      CHARACTER*5 PROJECT
      
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
C     ....Compute histogram of input image.  If the image is byte
C     ....the histogram is stored directly in OHIST.  If halfword, 
C     ....the histogram is first stored in HIST, and then
C     ....compressed into OHIST.
      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'HALF' .AND.  FORMAT .NE. 'WORD') GOTO 990
      IF (FORMAT.EQ.'BYTE') THEN
         ITYPE = 1
         CALL COMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
      ELSE IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
         ITYPE = 2
         CALL COMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
      ELSE
         GOTO 990
      ENDIF
      IF (XVPTST('IOF')) ITYPE=3
      IF (XVPTST('RADIANCE')) ITYPE=4
      IF (ITYPE.EQ.3.OR.ITYPE.EQ.4) THEN
         IF (FORMAT.EQ.'BYTE') GOTO 992
         CALL GETPROJ(IUNIT,project,icam,ifds,ind)
         IF (IND.EQ.1) GOTO 999
         CALL GETLABCON(IUNIT,PROJECT,labuf,ind)
      ENDIF

      IF (ITYPE.NE.1) THEN
	 IF (.NOT.XVPTST('INCLUDE')) THEN
	    NPTS = NPTS - HIST(-32768)
	    HIST(-32768) = 0
         ENDIF
         CALL HISTAT2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)

         CALL Xvmessage('Test from Fortran',' ')
         CALL GETSCALE(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
         IF (IND.EQ.0) GOTO 999
	 SCALE = ISCALE/OSCALE
         CALL PRNT(7,1,iscale,'ISCALE=')
         CALL PRNT(7,1,oscale,'OSCALE=')
         CALL PRNT(7,1,scale,'SCALE=')

         CALL Xvmessage('Test from C',' ')
         iscale = 0.0
         oscale = 0.0
         CALL tzgetscale(ITYPE,LABUF,MAXDN,iscale,oscale,ind)
         IF (IND.EQ.0) GOTO 999
	 SCALE = ISCALE/OSCALE
         CALL PRNT(7,1,iscale,'ISCALE=')
         CALL PRNT(7,1,oscale,'OSCALE=')
         CALL PRNT(7,1,scale,'SCALE=')

         MEAN =ISCALE*MEAN
         SIGMA = ISCALE*SIGMA

         CALL HISCALE(HIST,NPTS,SCALE,ohist,lsat,hsat)
         MAXFREQ = 0
         DO I=1,254
            NPIXELS = OHIST(I)
            IF (NPIXELS.GT.MAXFREQ) MAXFREQ=NPIXELS
         ENDDO
      ENDIF

      CALL PRNT(4,256,ohist,'OHIST=')
      CALL PRNT(7,1,lsat,'LSAT=')
      CALL PRNT(7,1,hsat,'HSAT=')
      CALL PRNT(7,1,mean,'MEAN=')
      CALL PRNT(7,1,sigma,'SIGMA=')
      CALL PRNT(4,1,maxfreq,'MAXFREQ=')

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
  992 CALL Xvmessage(' Keywords IOF and RADIANCE invalid for byte data',
     &               ' ')
  999 CALL Xvmessage(' Tgetscale task cancelled', ' ')
      CALL ABEND
      END
$!-----------------------------------------------------------------------------
$ create tzgetscale.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzgetscale)(itype,labuf,maxdn,iscale,oscale,ind)
void *labuf;
int *itype, *maxdn, *ind;
float *iscale, *oscale;
{
/*  ============================================  */
      zgetscale(*itype,labuf,*maxdn,iscale,oscale,ind);
}
$!-----------------------------------------------------------------------------
$ create tgetscale.imake
/* Imake file for Test of VICAR subroutine getscale */

#define PROGRAM tgetscale

#define MODULE_LIST tgetscale.f tzgetscale.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL

#define LIB_TAE
/* #define LIB_LOCAL  */
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tgetscale.pdf
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
$ create tstgetscale.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "This is a test of subroutine getscale"

write " In order to test this subroutine, the test data"
write " MIPL:[MIPL.VGR]F1636832.FIC has to be copied(or FTPed)"
write " to your personal directory, and name it as f1636832.fic"

WRITE "Test on Voyager halfword flight image"
tgetscale f1636832.fic  'IOF 
tgetscale f1636832.fic  'RADIANCE 

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getscale.hlp
1 GETSCALE

  GETSCALE chooses a convenient scale for compressing a 64K grey-level
  histogram into 256 grey levels.  The output scale may be in units of
  DN, IOF, or radiance.

  Calling Sequence:

	CALL GETSCALE(ITYPE,LABEL,MAXDN,ISCALE,OSCALE,IND)
  where
	INTEGER*4 ITYPE		!Histogram units (2=DN, 3=IOF, 4=radiance)
	REAL*4 LABEL(80)	!Label array returned by subroutine GETLABCON
	INTEGER*4 MAXDN		!Maximum DN in 64K histogram
	INTEGER*4 ISCALE	!Scale of 64K histogram
	INTEGER*4 OSCALE	!Scale of 256 grey-level histogram
	INTEGER*4 IND		!Return indicator (1=normal, 0=failure)

  The subroutines GETSCALE and HISCALE are used to prepare histograms for
  display.  See program HICCUP for an example of their use.

2 Operation

  The histogram scale may be in units of DN (ITYPE=2), IOF (ITYPE=3), or
  radiance (ITYPE=4).  In each of these three scales, the histogram is
  compressed by setting all negative DN values to 0 DN and scaling the
  positive DNs as follows:

  If ITYPE=2, the input DNs are divided by some power of 2, chosen so that the
  maximum input DN value is scaled to a value that is greater than 128 but
  less than 256.

  If the keyword 'IOF is specified, the histogram is scaled to units of
  IOF times some scale factor, chosen so that:
    (a) the maximum input DN value is scaled to a value that is greater
        than 128 but less than 256.
    (b) 256 on the output scale is some convenient IOF unit (one of the
        digits 1,2,4, or 5 multiplied by a power of 10).

  If the keyword 'RADIANCE is specified, the histogram is scaled to units
  of nanowatts/cm**2/steradian/nanometer times some convenient scale
  chosen as in 'IOF above.

  LABEL is the array returned from a call to GETLABCON to retrieve the
  label information for the image whose histogram is to be compressed.
  The radiometric scale of the input image is retrieved from this array.

2 History

  Original Programmer: Gary Yagi, Jan 22, 1990
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN
  Revision history:
    07 June 90  GMY  Restrict max IOF between .0001 and 1.
    29 Sep  93  FFM  Made portable, revised test program
     7 Jun  94  FFM  Revised test & imake files (FR 85106, 85107)


$ Return
$!#############################################################################
