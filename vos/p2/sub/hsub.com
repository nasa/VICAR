$!****************************************************************************
$!
$! Build proc for MIPL module hsub
$! VPACK Version 1.9, Monday, December 07, 2009, 16:23:11
$!
$! Execute by entering:		$ @hsub
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
$ write sys$output "*** module hsub ***"
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
$ write sys$output "Invalid argument given to hsub.com file -- ", primary
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
$   if F$SEARCH("hsub.imake") .nes. ""
$   then
$      vimake hsub
$      purge hsub.bld
$   else
$      if F$SEARCH("hsub.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hsub
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hsub.bld "STD"
$   else
$      @hsub.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hsub.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hsub.com -mixed -
	-s hsub.f zhsub.c -
	-i hsub.imake -
	-t thsub.f tzhsub.c thsub.imake thsub.pdf tsthsub.pdf -
	-o hsub.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hsub.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE HSUB(DCODE ,NS ,BUF ,HIST , ILOW , IHIGH)
C
C  92-9-30 ...SP.... Changed to use fortport.fin as improved portability
C                    method.  Went back from MACRO to FORTRAN CMS generation 1.
C                    Changed ILOW and IHIGH to required parameters.
C                    (Defaults were 0 & 255 for byte, 0 & 32767 for half.)
C
C  83-6-1  ...LWK... support byte data, make ILOW/HIGH optional
C  83-5-2  ...LWK... found in LIBSOR: (UCL routine?)
C
C     HSUB PRODUCES CUMULATIVE HISTOGRAM OF INPUT ARRAY( BUF)
C
C     DCODE      :- DATA FORMAT FOR INPUT ARRAY BUF
C        (1=byte, 2=halfwd)
C     NS         :- NUMBER OF SAMPLES IN ARRAY  BUF
C     BUF        :- INPUT ARRAY CONTAINING INPUT SAMPLES
C     HIST       :- ARRAY HOLDING SAMPLE FREQUENCIES
C     ILOW       :- LOWEST   DN IN HISTOGRAM RANGE (required)
C     IHIGH      :- HIGHEST  DN IN HISTOGRAM RANGE     "
C
      INTEGER   HIST(*) , ILOW , IHIGH , DCODE
      INTEGER*2 BUF(*) 
C==================================================================
      IF (DCODE.EQ.2) THEN
	ILO = ILOW
	IHI = IHIGH
	IF (IHI.LT.ILO) IHI = ILO+1
      ENDIF
      IF (DCODE.EQ.1) CALL HSUBB(NS,BUF,HIST)
      IF (DCODE.EQ.2) CALL HSUBH(NS,BUF,HIST,ILO,IHI)
      RETURN
      END
C
      SUBROUTINE HSUBH(NS,BUF,HIST,ILOW,IHIGH)
      IMPLICIT INTEGER (A-Z)
      DIMENSION HIST(*)
      INTEGER*2 BUF(*)
      DO 10 I = 1,NS
	K = BUF(I)
	IF(K .LT. ILOW ) K = ILOW
	IF(K .GT. IHIGH) K = IHIGH
	K = K - ILOW + 1
	HIST(K) = HIST(K) + 1
   10 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE HSUBB(NS,BUF,HIST)

      IMPLICIT INTEGER (A-Z)
      include  'fortport'  ! defines BYTE2INT.

      DIMENSION HIST(*)
      BYTE BUF(*)
C==================================================================

      DO 10 I = 1,NS
	K = BYTE2INT(BUF(I))+1
	HIST(K) = HIST(K) + 1
   10 CONTINUE
C
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zhsub.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


void zhsub( dcode, ns, buf, hist, ilow, ihigh)
int dcode;	
int ns;		
void *buf;	
void *hist;	
int ilow;	
int ihigh;	

{
FTN_NAME2(hsub, HSUB) ( &dcode, &ns, buf, hist, &ilow, &ihigh);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hsub.imake
/* Imake file for VICAR subroutine HSUB */

#define SUBROUTINE hsub

#define MODULE_LIST hsub.f zhsub.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create thsub.f
c  test subroutine HSUB
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C test pgm for subroutine HSUB
      IMPLICIT INTEGER (A-Z)
      BYTE BUF(1000000)
      DIMENSION HIST(65536)
      CHARACTER*8 FORMAT
C==================================================================
      CALL XVUNIT(IUNIT,'INP',1,STAT, ' ')
      CALL XVOPEN(IUNIT,STAT, ' ')
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT, ' ')
      DCODE=0
      IF(FORMAT.EQ.'BYTE') DCODE=1
      IF(FORMAT.EQ.'HALF') DCODE=2
      IF (DCODE.LT.1.OR.DCODE.GT.2) THEN
	CALL XVMESSAGE(' ** ILLEGAL FORMAT **',' ')
	CALL ABEND
      ENDIF
      CALL XVPARM('IHI',IHI,ICOUNT,IDEF,1)
      CALL XVPARM('ILO',ILO,ICOUNT,IDEF,1)
      IF (DCODE .EQ. 2 .AND. IHI .EQ. 0) IHI=32767

      call zia(HIST, 65536)   ! INITIALIZE HISTOGRAM
      DO I = 1,NLO
         CALL XVREAD(IUNIT,BUF,STAT,'LINE',I,'NSAMPS',NSO, ' ')
         IF ( MOD(I,2) .EQ. 1)  THEN
            CALL HSUB(DCODE,NSO,BUF,HIST,ILO,IHI)   ! test from Fortran
         ELSE
            call tzhsub(DCODE,NSO,BUF,HIST,ILO,IHI) ! and C.
         END IF
      ENDDO

      IF (DCODE.EQ.1) THEN
	MAX = 1
	DO I=1,256
	  IF (HIST(I).NE.0) MAX=I
	ENDDO
	CALL PRNT(4,MAX,HIST,' HIST:.')
      ELSE
        IF (IHI .LT. ILO) THEN
            CALL PRNT(4,ILO-IHI+1,HIST,' HIST:.')
        ELSE
	    CALL PRNT(4,IHI-ILO+1,HIST,' HIST:.')
        END IF
      ENDIF

      CALL XVCLOSE(IUNIT,STAT, ' ')
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzhsub.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzhsub)(dcode,nso,buf,hist,ilo,ihi) 
int *dcode, *nso, *ilo, *ihi;
int *hist;
void *buf;
{
/*  ============================================  */

      zhsub(*dcode,*nso,buf,hist,*ilo,*ihi );
}
$!-----------------------------------------------------------------------------
$ create thsub.imake
/* Imake file for Test of VICAR subroutine hsub */

#define PROGRAM thsub

#define MODULE_LIST thsub.f tzhsub.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create thsub.pdf
! pdf for test pgm for subroutine HSUB
PROCESS
PARM INP TYPE=STRING
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM DCODE TYPE=INTEGER
PARM ILO TYPE=INTEGER DEFAULT=0
PARM IHI TYPE=INTEGER DEFAULT=0
END-PROC
$!-----------------------------------------------------------------------------
$ create tsthsub.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage = "no"
! test for subroutine HSUB
!  BYTE DATA:
gen a 10 10
thsub a dco=1
!  halfword data:
gen b 10 10 'half ival=-5
list b 'half
thsub b dco=2 ilo=-3 ihi=20
thsub b dco=2 ilo=-3 ihi=-4
thsub b dco=2 ilo=0 ihi=20
thsub b dco=2 ilo=1 ihi=20
gen c 10 10 'half ival=-1000
list c 'half
thsub c dco=2 ilo=-999 ihi=-997
gen d 10 10 'half ival=-32767
list d 'half
thsub d dco=2 ilo=-32767 ihi=-32760
gen e 10 10 'half ival=32750
list e 'half
thsub e dco=2 ilo=32740
gen f 20 20 'half ival=100
thsub f dco=2 ilo=100 ihi=120
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create hsub.hlp
1  HSUB

     HSUB may be used to generate a cumulative histogram of byte or
     halfword arrays.

  CALLING SEQUENCE:  

     CALL HSUB( DCODE, NS, BUF, HIST, ILOW, IHIGH)

  ARGUMENTS: 

     DCODE  is the input data format code:
          = 1 for byte data, = 2 for halfword data.

     NS	    is the number of samples in array BUF.

     BUF    is an array containing the input samples.

     HIST   is an INTEGER*4 array in which the sample freqencies for
	    array BUF will be accumulated.

     ILOW   is the lowest DN in the histogram range.

     IHIGH  is the highest DN in the histogram range.

2 OPERATION

     For byte data, ILOW, and IHIGH are ignored and the histogram is as-
     sumed to have a range from 0 to 255.  For halfword data, the above
     call to HSUB is equivalent to the following FORTRAN:

            		DO 10 I = 1,NS
			K = BUFF(I)
			IF (K.LT.ILOW) K=ILOW
			IF (K.GT.IHIGH) K=IHIGH
			K = K - ILOW + 1
		   10   HIST(K) = HIST(K) + 1
    
     All input samples with DN values less than ILOW or greater than
     IHIGH are truncated.  

     If IHIGH < ILOW, then IHIGH = ILOW + 1
2 HISTORY:
 
     Original Programmer: Budak Barkan, Jan. 1985
     Current Cognizant Programmer: Florance Moss
     Source Language: Macro

$ Return
$!#############################################################################
