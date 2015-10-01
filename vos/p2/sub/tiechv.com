$!****************************************************************************
$!
$! Build proc for MIPL module tiechv
$! VPACK Version 1.7, Thursday, July 15, 1993, 11:35:12
$!
$! Execute by entering:		$ @tiechv
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
$ write sys$output "*** module tiechv ***"
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
$ write sys$output "Invalid argument given to tiechv.com file -- ", primary
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
$   if F$SEARCH("tiechv.imake") .nes. ""
$   then
$      vimake tiechv
$      purge tiechv.bld
$   else
$      if F$SEARCH("tiechv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tiechv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tiechv.bld "STD"
$   else
$      @tiechv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tiechv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tiechv.com -
	-s tiechv.f ztiechv.c -
	-i tiechv.imake -
	-t ttiechv.f tztiechv.c ttiechv.imake ttiechv.pdf tsttiechv.pdf -
	-o tiechv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tiechv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c*****************************************************************************
c     Subroutine TIECHV
C
C     CHECK FOR 'TIEPOINTS' OPTION ERRORS AND
C     OPTIONALLY CORRECT DISTORTION
C
C     ARGUMENTS:
C       IND   - INDICATOR ON RETURN (0 IF OK)
C       ITYPE - 0 = NO DISTORTION CORRECTION
C               1 = CORRECT FOR DISTORTION (USE GEOMA ROUTINE GEMDST)
C       PAR   - PARAMETER BUFFER
C               IN ORDER OF LINES,SAMPLES,LAT,LONG,......
C       NPOINT- NUMBER OF POINTS
C       CONV  - GEOMA parameters to be used by TRITRA   
C               for geometric correction of image space
C               tiepoints.
C               The GEOMA parameters are in the format:
C               conv(1)='NAH '
C               conv(2)='    '
C               conv(3)=value (value of 'NAH',number of horizontal areas)
C               conv(4)='NAV '
C               conv(5)='    '
C               conv(6)=value (value of 'NAV',number of vertical areas)
C               conv(7)='TIEP'
C               conv(8)='    '
C               conv(9)=   beginning of GEOMA tiepoint parameters
C
C     HISTORY:
C      13 JUL 93  T. L. TRUONG  PORTED TO UNIX
C      16 JAN 78    ...JJL...   INITIAL RELEASE
c*****************************************************************************
C
      SUBROUTINE TIECHV(IND,ITYPE,PAR,NPOINT,CONV)
      CHARACTER*132 MSG
      REAL*4 PAR(*)
      INTEGER*4 CONV(*)
      IND=0
      J=NPOINT*4
      IF(NPOINT.LT.3) GO TO 140
      IF(NPOINT.GT.20) GO TO 130
      WRITE (MSG,9900) NPOINT
9900  FORMAT (' ',I2,' TIEPOINTS SUPPLIED BY USER')
      CALL XVMESSAGE(MSG(2:30),' ')
131   DO 101 I=1,J,4
      IF(ABS(PAR(I+2)).LT.90.005) GO TO 120
      IND=-1
      CALL XVMESSAGE(' TIEPTS LATITUDE ERROR',' ')
      RETURN
120   IF(ABS(PAR(I+3)).LT.360.005) GO TO 110
      IND=-1
      CALL XVMESSAGE(' TIEPTS LONGITUDE ERROR',' ')
      RETURN
110   IF(ITYPE.EQ.0) GO TO 101
C***************************************
C     CORRECT LINE SAMPLE FOR DISTORTION
C***************************************
      CALL TRITRA(IND,CONV(9),CONV(3)+1,CONV(6)+1,PAR(I),PAR(I+1),1)
101   CONTINUE
      RETURN
140   CALL XVMESSAGE(' ***TOO FEW TIEPOINTS',' ')
      IND=-1
      RETURN
130   CALL XVMESSAGE(' ***TOO MANY TIEPOINTS SUPPLIED--FIRST 20 '//
     +			'USED, REST IGNORED',' ')
      NPOINT=20
      J=NPOINT*4
      GO TO 131
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ztiechv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of TIECHV                                         */
/************************************************************************/

void ztiechv(indr,ityp,pbuf,npnt,convp)
int indr;     /* indicator on return--0 if OK (output) */
int ityp;     /* 0 = no distortion correction
                 1 = perform distortion correction (input) */
void *pbuf;   /* tiepoint parameter buffer   
                 in order line,sample,lat,long,... (input) */
int npnt;   /* number of points (input) */
void *convp;  /*  GEOMA parameters to be used by TRITRA 
               for geometric correction of image space
               tiepoints (input) */

{
FTN_NAME(tiechv)(&indr,&ityp,pbuf,&npnt,convp);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tiechv.imake
/* Imake file for VICAR subroutine TIECHV */

#define SUBROUTINE tiechv

#define MODULE_LIST tiechv.f ztiechv.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create ttiechv.f
C*****************************************************************************
C Unit test program TTIECHV.F for subroutine TIECHV
C 
C Ported to UNIX 7/13/93
C*****************************************************************************
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44()
      real tp(20),tiep(20),conv(2216),isres(404)
      data TIEP/400.,400.,2.6662,179.1568,
     *           400.,500.,8.8652,156.9667,
     *           500.,500.,-13.8541,149.8141,
     *           600.,400.,-47.9389,172.3825,
     *           600.,500.,-39.3800,140.5490/
      data TP/400.,400.,2.6662,179.1568,
     *           400.,500.,8.8652,156.9667,
     *           500.,500.,-13.8541,149.8141,
     *           600.,400.,-47.9389,172.3825,
     *           600.,500.,-39.3800,140.5490/
C**********************************
C FORTRAN-callable
C**********************************
      call xvmessage('**** FORTRAN-callable RFT2 ****',' ')
      call xvmessage(
     *' test of tiechv without distortion correction',' ')
       CALL XVMESSAGE(
     *' these points were determined using PHOTCALV2 with', ' ')
       CALL XVMESSAGE(
     +' vgr image 1636832 using nominal GEOMA correction ',' ')
       CALL XVMESSAGE('parameters',' ')
       call xvmessage(
     +'                      LINE      SAMPLE    LATITUDE 
     * LONGITUDE   ',' ')
       do i=1,5
          call prnt(7,4,tiep(i*4-3),' new tiepoint=')
       enddo
       call tiechv(ind,0,tiep,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tiepoints are in correct format',' ')
       call prnt(4,1,IND,' tiechv return ind=.')
       call getres(isres,7)
       call geomav(conv,7,isres)
       call xvmessage(' tiepoints after geometric correction',' ')
       call tiechv(ind,1,tiep,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tiepoints are in correct
     * format',' ')
       call prnt(4,1,IND,' tiechv return ind=.')
       do i=1,5
          call prnt(7,4,tiep(i*4-3),' new tiepoint=')
       enddo
C*****************************************************************************
c C CALLable
C*****************************************************************************
        call xvmessage('**** C-callable RFT2 ****',' ')
       call xvmessage(
     *' test of tiechv without distortion correction',' ')
       CALL XVMESSAGE(
     *' these points were determined using PHOTCALV2 with', ' ')
       CALL XVMESSAGE(
     +' vgr image 1636832 using nominal GEOMA correction ',' ')
       CALL XVMESSAGE('parameters',' ')
       call xvmessage(
     +'                      LINE      SAMPLE    LATITUDE 
     * LONGITUDE   ',' ')
       do i=1,5
          call prnt(7,4,tp(i*4-3),' new tpoint=')
       enddo
       call tztiechv(ind,0,tp,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tpoints are in correct
     * format',' ')
       call prnt(4,1,IND,' tztiechv return ind=.')
       call getres(isres,7)
       call geomav(conv,7,isres)
       call xvmessage(' tpoints after geometric correction',' ')
       call tztiechv(ind,1,tp,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tpoints are in correct
     * format',' ')
       call prnt(4,1,IND,' tztiechv return ind=.')
       do i=1,5
          call prnt(7,4,tp(i*4-3),' new tpoint=')
       enddo
       return
       end
$!-----------------------------------------------------------------------------
$ create tztiechv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TTIECHV.F */
/************************************************************************/
void FTN_NAME(tztiechv) (indr,ityp,pbuf,npnt,convp)
int *indr;     /* indicator on return--0 if OK (output) */
int *ityp;     /* 0 = no distortion correction
                 1 = perform distortion correction (input) */
void *pbuf;    /* tiepoint parameter buffer   
                 in order line,sample,lat,long,... (input) */
int *npnt;   /* number of points (input) */
void *convp;   /*  GEOMA parameters to be used by TRITRA 
               for geometric correction of image space
               tiepoints (input) */
{
      ztiechv(*indr,*ityp,pbuf,*npnt,convp);
}
$!-----------------------------------------------------------------------------
$ create ttiechv.imake
/* Imake file for Test of VICAR subroutine tiechv */

#define PROGRAM ttiechv

#define MODULE_LIST ttiechv.f tztiechv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
$!-----------------------------------------------------------------------------
$ create ttiechv.pdf
!*****************************************************************************
! TTIECHV.PDF - pdf for test program TTIECHV.F for the subroutine TIECHV
!*****************************************************************************
process
end-proc
$!-----------------------------------------------------------------------------
$ create tsttiechv.pdf
!****************************************************************************
! TSTTIECHV.PDF, unit test procedure for subroutine TIECHV.F
!****************************************************************************
procedure help=*
refgbl $echo

body
let _onfail="continue"
let $echo="yes"
ttiechv

end-proc
.title TSTTIECHV.PDF - unit test for subroutine TIECHV
.end
$ Return
$!#############################################################################
$Other_File:
$ create tiechv.hlp
1 TIECHV

       TIECHV checks the validity of tiepoints used in the TIEPOINTS
   mode of MAP2 and other programs. Points can be converted to object
   space using GEOMAV.

2  CALLING SEQUENCE

   FORTRAN calling sequence and parameters:

      CALL TIECHV(IND,ITYPE,PAR,NPOINT,CONV)

   where...

      IND      0=normal return                           integer
      ITYPE    0=no distortion correction                integer
               1= perform distortion correction          integer
      PAR      tiepoint parameter buffer 
               in order of line,sample,lat,long,...      real*4
      NPOINT   number of points                          integer
      CONV(*)  GEOMA parameters to be used by TRITRA     integer*4
               for geometric correction of image space
               tiepoints
               The GEOMA parameters are in the format:
               conv(1)='NAH '
               conv(2)='    '
               conv(3)=value (value of 'NAH',number of horizontal areas)
               conv(4)='NAV '
               conv(5)='    '
               conv(6)=value (value of 'NAV',number of vertical areas)
               conv(7)='TIEP'
               conv(8)='    '
               conv(9)=   beginning of GEOMA tiepoint parameters

   C calling sequence and parameters:

      tiechv(ind,itype,par,npoint,conv)

   similarly...

	int ind;
	int itype;
	float par;
	int npoint;
	int conv;
               
2  HISTORY

      Ported to UNIX: T. L. Truong, 13 July 1993	
      Original Programmer: J. J. Lorre, 16 June 1977
      Current Cognizant Programmer: Joel Mosher
      Source Language: Fortran
      Latest Revision: VICAR1* 16-FEB-1985



$ Return
$!#############################################################################
