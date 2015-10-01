$!****************************************************************************
$!
$! Build proc for MIPL module convev
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:16
$!
$! Execute by entering:		$ @convev
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
$ write sys$output "*** module convev ***"
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
$ write sys$output "Invalid argument given to convev.com file -- ", primary
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
$   if F$SEARCH("convev.imake") .nes. ""
$   then
$      vimake convev
$      purge convev.bld
$   else
$      if F$SEARCH("convev.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake convev
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @convev.bld "STD"
$   else
$      @convev.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create convev.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack convev.com -mixed -
	-s convev.f zconvev.c -
	-i convev.imake -
	-t tconvev.f tzconvev.c tconvev.imake tconvev.pdf tstconvev.pdf -
	-o convev.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create convev.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  CONVERT POINTS FROM LINE,SAMP TO LAT,LON AND THE REVERSE
C  MODE=2 L,S TO LAT,LON     MODE=1 LAT,LON TO L,S
C
      SUBROUTINE CONVEV(IND,DATA,IDATA,LINE,SAMP,LAT,LON,MODE,CONV)
      IMPLICIT NONE
      REAL DATA(40),LINE,SAMP,LAT,LON
      INTEGER IND,MODE,IDATA(40),CONV(*)

      INTEGER*4 PTYPE,ICAM,I,ITIEP,NAH,NAV,IX
      REAL*4 PTS(3),FLAG,OSLINE,OSSAMP,A,B,ELO
      DOUBLE PRECISION EMATRX(9),XC,ZC,TH,TH1,TH2,LAM,F,CAS,RP,RE,PSI,
     .                 OM(3,3),RS(3)
      character*5 project ! used in IS to OS conversions for 'GLL  '
      CHARACTER*4 CONVSTR ! USED IN FINDING ASCII IN CONV ARRAY.

      IND=0
      FLAG=-400.
      PTYPE = IDATA(39)			!Get projection type
C   Do some preliminary shuffling for certain map projections.

      IF(PTYPE.EQ.6)CALL MERCPATCH(DATA)
CCCCCCC      IF(PTYPE.EQ.9)CALL CYLPATCH(DATA)
      IF(PTYPE.EQ.10)CALL RECTPATCH(DATA)

C   Check parameters. exit if invalid.

      IF(MODE.LT.1.OR.MODE.GT.2) GO TO 99
      IF(PTYPE.LT.1.OR.PTYPE.GT.16) GO TO 98


      IF(PTYPE.EQ.7.OR.PTYPE.EQ.8.OR.PTYPE.EQ.16) GO TO 10

C  MAP PROJECTION MODE       Most common case uses TRANV.

      XC=DATA(1)
      ZC=DATA(2)
      TH=DATA(3)
      TH1=DATA(4)
      TH2=DATA(5)
      LAM=DATA(6)
      F=DATA(7)
      CAS=DATA(8)
      RP=DATA(25)
      RE=DATA(26)
      PSI=DATA(9)
      CALL TRANV(IND,PTYPE,MODE,XC,ZC,TH,TH1,TH2,LAM,F,CAS,
     *  LINE,SAMP,LAT,LON,RP,RE,PSI)
      RETURN

C  RAW OR OBJECT SPACE PICTURE     !!!! Other cases are handled according ...

10    CONTINUE

c If image space extract reseau matrix parameters
      IF(PTYPE.EQ.7) then
        CALL MVLC ( CONV, PROJECT, 5 )
        if(project.ne.'GLL  ')then
          DO 12 I=1,20
             CALL MVLC( CONV(I), CONVSTR, 4)
             IF(CONVSTR.EQ. 'TIEP')  ITIEP=I+2
             IF(CONVSTR.EQ. 'NAH ')  NAH=CONV(I+2)
             IF(CONVSTR.EQ. 'NAV ')  NAV=CONV(I+2)
12        CONTINUE
        else
           icam = CONV(3)	!GLL camera serial number
        endif
      ENDIF

      IF(MODE.EQ.1) GO TO 30            !!!! ... to MODE.

C  L,S TO LAT,LON

      DO IX=1,9
         EMATRX(IX) = 0.D0
      ENDDO
      EMATRX(1)=1.D0
      EMATRX(5)=1.D0
      EMATRX(9)=DBLE(DATA(26))/DBLE(DATA(25))

C  CONVERT POINT FROM I.S. TO O.S.
      IF(PTYPE.EQ.7) then
        call convisos(project,icam,line,samp,osline,ossamp,1,
     &		conv(itiep),nah+1,nav+1,ind)
        line=osline
        samp=ossamp      
      endif

      PTS(1)=DATA(26)
      PTS(2)=DATA(27)*DATA(30)
      PTS(3)=DATA(38)
      CALL MVE(4,18,DATA(1), OM, 1,1) !TRICK TO INSURE THIS DATA IS DOUBLE
      CALL MVE(4, 6,DATA(19),RS, 1,1) !PRECISION ALIGNED
      CALL IPPCOV(LAT,LON,LINE,SAMP,PTS,RS, OM,
     *  EMATRX,DATA(28),DATA(29),FLAG)
      IF(LAT.EQ.FLAG) IND=1
      LAT=LAT*180./3.141592654
      LON=360.-LON*180./3.141592654
      RETURN

C  LAT,LON TO L,S

30    CONTINUE
      A=DATA(27)*DATA(30)
      B=DATA(26)-DATA(25)
      ELO=360.-LON
      CALL MVE(4,18,DATA(1), OM, 1,1) !TRICK TO INSURE THIS DATA IS DOUBLE
      CALL MVE(4, 6,DATA(19),RS, 1,1) !PRECISION ALIGNED
      CALL CORCAV(IND,LAT,ELO,OM,RS,A,DATA(26),
     * B,LINE,SAMP,DATA(28),DATA(29),FLAG)

C  CHECK FOR BEHIND THE PLANET, IF TRUE RETURN IND=1
      IF (IND.EQ.99) THEN
	IND=1
	RETURN
      ELSE
	IND=0
      ENDIF
      IF(LINE.EQ.FLAG.AND.SAMP.EQ.FLAG) IND=1

C  CONVERT FROM O.S. TO I.S.
      IF(PTYPE.EQ.7) then
        osline=line
        ossamp=samp
        call convisos(project,icam,line,samp,osline,ossamp,0,
     &		conv(itiep),nah+1,nav+1,ind)
      endif

      RETURN

99    CALL XVMESSAGE('MODE NOT 1 OR 2 - CONVEV',' ')
      IND=1
      RETURN
98    CALL XVMESSAGE('ILLEGAL IMAGE TYPE - CONVEV',' ')
      IND=1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zconvev.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zconvev - Converts from line,sample to lat,long and 
   the reverse 								*/
/************************************************************************/

void zconvev(ind,data,idata,line,samp,lat,lon,mode,conv)

int *ind,*idata;
int  mode;
float *line, *samp, *lat, *lon;
float *data, *conv;

{
FTN_NAME2(convev, CONVEV) (ind,data,idata,line,samp,lat,lon,&mode,conv);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create convev.imake
/* Imake file for VICAR subroutine CONVEV */

#define SUBROUTINE convev

#define MODULE_LIST convev.f zconvev.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tconvev.f
      INCLUDE 'VICMAIN_FOR'
C
C         TEST PROGRAM FOR CONVEV SUBPROGRAM
      SUBROUTINE MAIN44
      IMPLICIT NONE
      REAL CONV(4000),RLOC(404)
      INTEGER I,J

      COMMON IND,LINE,SAMP,LAT,LONG,MODE,DATA
      REAL LINE,SAMP,LAT,LONG,DATA(40)
      REAL*8 OM1(3,3),RS1(3)
      INTEGER IND,MODE,IDATA(40)
      EQUIVALENCE (DATA,IDATA,OM1),(DATA(19),RS1)

      DATA ((OM1(I,J),I=1,3),J=1,3)/0.40706,0.11232,0.90647,-0.87481,
     1                 -0.23747,0.42227,0.26269,-0.96488,0.0015944/,
     2     (RS1(I),I=1,3)/-730950.0,-339690.0,-1209.3/,
     3     (DATA(I),I=25,35),DATA(38)/1815.0,1815.0,1502.4,500.0,
     4           500.0,84.821,-0.02489,155.07,400.,400.,15.056,
     5           806060.0/,
     6     IDATA(36),IDATA(37),IDATA(39),IDATA(40)/6,1,7,0/

      CALL XVMESSAGE('   ',' ')
      CALL XVMESSAGE(
     .     '********** TEST VGR IMAGE SPACE/ OBJECT SPACE ******',' ')
      CALL GETRES(RLOC,7)
      CALL GEOMAV(CONV,7,RLOC)
      MODE=1
      LINE = 400.0
      SAMP = 400.0
      LAT = -13.8018
      LONG = 150.1259
      CALL XVMESSAGE('BEFORE LAT/LONG TO LINE/SAMP - RAW PICTURE',' ')
      CALL DUMP(1)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LAT/LONG TO LINE/SAMP - IMAGE SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LINE/SAMP TO LAT/LONG - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      CALL XVMESSAGE(
     *'ANSWER: IMAGE SPACE LINE 500 SAMP 500 = LAT-13.8 LONG 150.1',' ')
      CALL XVMESSAGE('AND OBJECT SPACE LINE 614.4, SAMP 615.1',' ')

      CALL XVMESSAGE('   ',' ')
      CALL XVMESSAGE(
     .     '********** TEST GLL IMAGE SPACE/ OBJECT SPACE ******',' ')
      CALL MVCL('GLL  ',conv,5)
      CALL MVE(4,1,1,conv(3),1,1)
      MODE=1
      LINE = 400.0
      SAMP = 400.0
      LAT = -13.8018
      LONG = 150.1259
      CALL XVMESSAGE('BEFORE LAT/LONG TO LINE/SAMP - RAW PICTURE',' ')
      CALL DUMP(1)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LAT/LONG TO LINE/SAMP - IMAGE SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LINE/SAMP TO LAT/LONG - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      CALL XVMESSAGE(
     *'ANSWER: IMAGE SPACE LINE 500 SAMP 500 = LAT-13.8 LONG 150.1',' ')
      CALL XVMESSAGE('AND OBJECT SPACE LINE 614.4, SAMP 615.1',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('********** TEST PERSPECTIVE ******',' ')
      idata(39)=16
      MODE=1
      LINE = 400.0
      SAMP = 400.0
      LAT = -13.8018
      LONG = 150.1259
      CALL XVMESSAGE('BEFORE LAT/LONG TO LINE/SAMP ',' ')
      CALL DUMP(1)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LAT/LONG TO LINE/SAMP - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LINE/SAMP TO LAT/LONG - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      CALL XVMESSAGE(
     *'ANSWER: IMAGE SPACE LINE 500 SAMP 500 = LAT-13.8 LONG 150.1',' ')
      CALL XVMESSAGE('AND OBJECT SPACE LINE 614.4, SAMP 615.1',' ')


      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('************** TEST MERCATOR ***************',' ')
      IDATA(39)=6
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(3)=50.
      DATA(6)=360.
      DATA(7)=100.
      CALL XVMESSAGE(
     .     'BEFORE:LAT/LONG TO LINE/SAMP W/MERCATOR PROJECTION',' ')
      LINE=0.
      SAMP=0.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LAT/LONG TO LINE/SAMP W/MERCATOR PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL XVMESSAGE(
     .     'AFTER:LINE/SAMP TO LAT/LONG W/MERCATOR PROJECTION',' ')
      CALL DUMP(2)
      CALL XVMESSAGE(
     .     'ANSWER: LATI=-10,LONG=200,LINE=22.53,SAMP=51.68',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('********* TEST NORMAL CYLINDIRICAL ********',' ')
      IDATA(39)=9
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=1.
      DATA(2)=1.
      DATA(3)=85.7461
      DATA(6)=239.916
      DATA(7) = 10.
      CALL CYLPATCH(DATA) !computes data(2,3,1,6)
      CALL XVMESSAGE(
     .     'BEFORE:LAT/LONG TO LINE/SAMP W/NORM CYL PROJECTION',' ')
      LINE=0.
      SAMP=0.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LAT/LONG TO LINE/SAMP W/NORM CYL PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LINE/SAMP TO LAT/LONG W/NORM CYL PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      CALL XVMESSAGE(
     .     'ANSWER: LINE=213.5,SAMP=127.4,LATI=-10.,LONG=200.',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('*********TEST SIMPLE CYLINDRICAL *********',' ')
      IDATA(39)=10             
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=50.
      DATA(2)=50.
      DATA(3)=-64.6828
      DATA(6)=205.31716    ! changed from 205.3172 so SAMP 1 is long. 360 
      DATA(7) = 100.       ! instead of long.  0.00006. See correction in
      CALL XVMESSAGE(      ! RECTPATCH.
     .     'BEFORE:LAT/LONG TO LINE/SAMP W/LATLON PROJECTION',' ')
      LINE=0.
      SAMP=0.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LAT/LONG TO LINE/SAMP W/LATLON PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LINE/SAMP TO LAT/LONG W/LATLON PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      CALL XVMESSAGE(
     .     'ANSWER: LINE=32.7,SAMP=51.7,LATI=-10.,LONG=200.',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************POLAR ORTHOGRAPHIC*********',' ')
      IDATA(39)=1
      MODE=1
      LAT=30.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=90.
      DATA(6)=167.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************POLAR STEREOGRAPHIC*********',' ')
      IDATA(39)=3
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=-90.
      DATA(6)=167.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE('***************OBLIQUE ORTHOGRAPHIC*********',' ')
      IDATA(39)=2
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=-34.
      DATA(6)=167.
      DATA(7) =7.89
      DATA(9)=40.
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE(
     .     '***************OBLIQUE STEREOGRAPHIC************',' ')
      IDATA(39)=4
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(6)=67.
      DATA(7) =7.89
      DATA(9)=40.
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************LAMBERT*********',' ')
      IDATA(39)=5
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(4)=30.
      DATA(5)=40.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('*********OBLIQUE SIMPLE CYLINDRICAL*********',' ')
      IDATA(39)=11
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(4)=30.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************SINUSOIDAL*********',' ')
      IDATA(39)=12
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)


      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***********OBLIQUE SINUSOIDAL*********',' ')
      IDATA(39)=13
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(4)=30.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('*************MOLLWEIDE*********',' ')
      IDATA(39)=14
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('************TRANSVERSE MERCATOR*********',' ')
      IDATA(39)=15
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface: zconvev', ' ')

      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(7) =7.89
      LINE=400.
      SAMP=400.
      call tzconvev(IND,LINE,SAMP,LAT,LONG,MODE,DATA,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      RETURN
      END

      SUBROUTINE DUMP(WHICH)
      COMMON IND,LINE,SAMP,LAT,LONG,MODE,DATA
      DOUBLE PRECISION OM1(3,3),RS1(3)
      REAL DATA(40),LINE,LAT,LONG
      INTEGER IDATA(40),WHICH
      EQUIVALENCE (IDATA,DATA),(DATA,OM1),(DATA(19),RS1)

      CHARACTER*640 BUFFER

      IF (WHICH .EQ. 1) WRITE(BUFFER,110)IND,LINE,SAMP,LAT,LONG,MODE,
     1                  ((OM1(I,J),J=1,3),I=1,3),(RS1(I),I=1,3),
     2                  (DATA(I),I=25,35),IDATA(36),IDATA(37),
     3                  DATA(38),IDATA(39),IDATA(40)
      IF (WHICH .EQ. 2) WRITE(BUFFER,100)IND,LINE,SAMP,LAT,LONG,MODE,
     1                  (DATA(I),I=1,35),(IDATA(I),I=36,37),DATA(38),
     2                  (IDATA(I),I=39,40)
      CALL XVMESSAGE(BUFFER(  2: 80),' ')
      CALL XVMESSAGE(BUFFER( 82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(BUFFER(242:320),' ')
      CALL XVMESSAGE(BUFFER(322:400),' ')
      CALL XVMESSAGE(BUFFER(402:480),' ')
      CALL XVMESSAGE(BUFFER(482:560),' ')
      CALL XVMESSAGE(' ',' ')
      RETURN
  100 FORMAT(' IND: ',I2,'  LINE: ',F7.1,'  SAMP: ',F7.1,'  LAT: ',
     1       F5.1,'  LONG: ',F7.1,'  MODE: ',I1,6X,' DATA: ',2(7
     2       (F8.4,2X),10X),4(F8.6,2X),3(F8.0,2X),10X,2(7(F8.2,
     3       2X),10X),I8,2X,10X,I8,2X,F8.1,2X,2(I8,2X),30X)
  110 FORMAT(' IND: ',I2,'  LINE: ',F7.1,'  SAMP: ',F7.1,'  LAT: ',
     1       F5.1,'  LONG: ',F7.1,'  MODE: ',I1,6X,' OM: ',5(E12.5,3X),
     2       5X,4(E12.5,3X),15X,' RS: ',3(E20.6,5X),' DATA: ',
     3       7(F8.2,2X),10X,4(F8.2,2X),I8,2X,I8,2X,F8.1,12X,
     4       2(I8,2X),30X)

      END
$!-----------------------------------------------------------------------------
$ create tzconvev.c
#include "xvmaininc.h"
#include "ftnbridge.h"


void FTN_NAME(tzconvev)(ind,line,samp,lat,lon,mode,data,conv)

int    *ind, *mode;
float  *line, *samp, *lat, *lon;
float  *data, *conv;
{
/*  ==================================================================  */
        zvmessage("TRANS MERC", "");
	zconvev(ind,data,data,line,samp,lat,lon,*mode,conv);
}
$!-----------------------------------------------------------------------------
$ create tconvev.imake
/* Imake file for Test of VICAR subroutine CONVEV */

#define PROGRAM tconvev

#define MODULE_LIST tconvev.f tzconvev.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tconvev.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstconvev.pdf
procedure
refgbl $echo
body
let _onfail="continue"
! TEST SCRIPT FOR CONVEV SUBROUTINE
!
write " CONVEV IS TESTED BY A PROGRAM THAT WILL CALL CONVEV WITH"
Write " BOTH INFORMATION FOR A RAW PICTURE AND SOME PROJECTIONS"
write " CONVEV WILL BE CALLED FOR A LAT/LONG TO LINE/SAMP AND BACK"
write " FOR EACH PICTURE TYPE."
let $echo="yes"
!
tconvev
!
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create convev.hlp
1  CONVEV - Converts from line, sample to latitude, longitude and the reverse.

FORTRAN Calling Sequence:

   INTEGER*4 IND	!Return status: 0=Success, 1=Point off planet
   REAL*4 DATA(40)	!Map projection or navigation data.  See below.
   REAL*4 LINE,SAMP	!Line,sample coordinates
   REAL*4 LAT		!Latitude (degrees)
   REAL*4 LON		!Longitude (degrees west)
   INTEGER*4 MODE	! 1=LAT,LON to LINE,SAMP, 2= LINE,SAMP to LAT,LON
   INTEGER*4 CONV(2216) !Geometric correcton parameters

   CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LON,MODE,CONV)


C Calling Sequence:

   int ind;		/* Return status: 0=Success, 1=Point off planet */
   float data[40];	/* Map projection or navigation data.  See below */
   float line,samp;	/* Line,sample coordinates */
   float lat;		/* Latitude (degrees) */
   float lon;		/* Longitude (degrees west) */
   int mode;		/* 1=LAT,LON to LINE,SAMP, 2= LINE,SAMP to LAT,LON */
   int conv[2216];	/* Geometric correcton parameters */

   zconvev(&ind,data,data,&line,&samp,&lat,&lon,mode,conv);

   Note the & for all scalar arguments except for the mode argument
   denoting the passing by address.

2  ARGUMENTS

   DATA - 40 word buffer containing map projection or nagivation data.
   The exact contents of DATA is determined by the projection type (located
   in DATA(39)).
       
   If the projection type is a map projection, the table below
   is filled. All the data values are input although some may 
   be modified by CONVEV.

	DATA(1) = special sample point             R*4
	DATA(2) = special line point               R*4
	DATA(3) = special latitude point           R*4
	DATA(4) = latitude of spec parallel        R*4
  		or spec oblique longitude (deg)
	DATA(5) = latitude of spec parallel  (deg) R*4
	DATA(6) = special longitude (west) (deg)   R*4
	DATA(7) = scale (km/pixel)                 R*4
	DATA(8) = visible pole  1=N -1=S           R*4
	DATA(9) = north angle                      R*4
	DATA(25)= polar radius (KM)                R*4
	DATA(26)= equatorial radius (KM)           R*4
	DATA(39)= projection type                  I*4
		1=Polar Orthographic 	2=Oblique Orthographic
		3=Polar Stereographic   4=Oblique Stereographic
		5=Lambert Conformal     6=Mercator
		7=Raw Uncorrected Image 8=Geometrically Corrected Image
		9=Normal Cylindrical   10=Simple Cylindrical
 	       11=Oblique Simple Cyl   12=Sinusoidal Equal Area
  	       13=Oblique Sinusoidal   14=Mollweide
               15=Transverse Mercator  16=Perspective
  	
    If the projection type is 7,8 or 16 (Image, object or perspective)
    the following table should be filled on input:
        
        DATA(1)  OM matrix           r*8 words 1-18
        DATA(19) RS vector           r*8 words 19-24
        DATA(25) polar radius        r*4 kilometers
        DATA(26) equatorial radius   r*4 kilometers
        DATA(27) focal length        r*4 millimeters
        DATA(28) optical axis line   r*4 object space pixels
        DATA(29) optical axis sample r*4 object space pixels
        DATA(30) scale               r*4 object space pixels/millimeter
        data(31)-data(37) not used
        DATA(38) distance of planet from spacecraft r*4 kilometers
        DATA(39) integer as above

   CONV is ignored unless DATA(39)=7, image-space (in which case CONV can be
   of length 1).  The following applies to image-space only:

   CONV is the geometric correcton parameters, as created by the GEOMAV
   subroutine.  This buffer is used by subroutine TRITRA to convert image
   line-sample values from raw (image-space) coordinates to geometrically
   corrected (object-space coordinates) or vice-versa.  This type of correction
   is applicable to removing vidicon camera distortions.

   For Galileo SSI, CONV(1)-CONV(2) contain the CHARACTER*5 project name
   'GLL  ', and CONV(3) contains the camera serial number.  (Both of these
   values are as returned by routine GETPROJ).  The remainder of the buffer
   is ignored.  Galileo uses an optical distortion model since there is no
   measurable distortion in the CCD.

   When converting from line-sample to latitude-longitude (MODE=2), CONVEV will
   first remove the geometric distortion by calling CONVISOS.  (CONVISOS in
   turn calls TRITRA for vidicon cameras, or GLLGCOR for the Galileo SSI.)
   IPPCOV is then called to convert to latitude-longitude.

   When converting from latitude-longitude to line-sample (MODE=1), CORCAV is
   called to convert to line-sample coordinates in object-space.  CONVISOS is
   then called to convert to image-space coordinates.


2  HISTORY

     Original Programmer: J. J. Lorre, 16 June 1977
     Current Cognizant Programmer: J J Lorre
     Source Language: Fortran
     Ported to UNIX:  Steve Pohorsky
     Revisions: 
       30-Nov-1985 ...JAM... return IND=1 for point behind planet
                             in MODE=1 image & object space.
       13-Apr-1986 ...jam... CALL MERCPATCH & RECTPATCH, added more
                             digits to PI.
       20-Mar-1987 ...RMB... added types 11 and 12
       10-Jun-1988 ...RMB... added type 13
       25 oct   89    jjl    added project in conv words 1-2 for GLL
       1  June  91    jjl    many changes to clean up logic. new test
      22  JAN 93      SP     Ported to UNIX; Added zconvev for calls from C.
                             Added MVE calls to align data for OM and RS.

2  Subroutines called:

      TRANV, CONVISOS, CORCAV, IPPCOV, MERCPATCH, RECTPATCH
$ Return
$!#############################################################################
