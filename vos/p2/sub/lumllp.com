$!****************************************************************************
$!
$! Build proc for MIPL module lumllp
$! VPACK Version 1.5, Thursday, May 20, 1993, 15:47:03
$!
$! Execute by entering:		$ @lumllp
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
$ write sys$output "*** module lumllp ***"
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
$   if F$SEARCH("lumllp.imake") .nes. ""
$   then
$      vimake lumllp
$      purge lumllp.bld
$   else
$      if F$SEARCH("lumllp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lumllp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lumllp.bld "STD"
$   else
$      @lumllp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lumllp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lumllp.com -
	-s lumllp.f -
	-i lumllp.imake -
	-t tlumllp.f tlumllp.imake tlumllp.pdf tstlumllp.pdf -
	-o lumllp.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lumllp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE LUMLLP(SLAT,SLON,PLAT,PLON,RPOL,REQU,RANGE,RLAT,RLON,
     *  TG,CG,CI,CE,LUMLAT,LUMLON,MODE,ifirst)

C  SLAT,SLON= SUBSOLAR POINT , WEST
C  PLAT,PLON= SUBSPACECRAFT POINT , WEST
C  RLAT,RLON = PIXEL POSITION , WEST
C  RPOL,REQU = PLANET RADIUS (KM)
C  ALL VALUES IN RADIANS
C  RANGE = S.C. TO PLANET CENTER DISTANSE (KM)   IF(RANGE)=0. MEANS=INFI
C  RETURNS TG=TAN PHASE ANGLE ,MAX VALUE LIMITED TO 1000.
C  RETURNS CG=COSINE PHASE ANGLE
C  RETURNS CI=COSINE INCIDENCE ANGLE
C  RETURNS CE=COSINE EMISSION ANGLE
C  IF RANGE=0.0 CE IS COMPUTED FOR RANGE=INFINITY
C  RETURNS LUMLAT,LUMLON = LUMINANCE LATITUDE,LONGITUDE  IF(MODE.NE.0)
C  LUMLON IS EAST LONGITUDE
C  MODE =0 FOR RETURN WITHOUT LUMLAT,LUMLON COMPUTED

      SAVE  ! SAVE VARIABLES BETWEEN SUCCESSIVE CALLS

      REAL AS(3),AL(3),C(10),HR(3,3),PI,PI2
      REAL LUMLAT,LUMLON,PIO2
      INTEGER FIRST
      DATA PI/3.141592654/,PI2/6.283185308/
      DATA PIO2/1.570796327/
      DATA FIRST/1/
C==================================================================
      IF (IFIRST.EQ.1) FIRST=1
      IF(FIRST.NE.1) GO TO 10

C  SET UP INITIALIZATION FOR FIRST PASS ONLY
      FIRST=0
      C(1)=COS(SLAT)
      C(2)=COS(PI2-SLON)
      C(3)=SIN(SLAT)
      C(4)=SIN(PI2-SLON)
      C(5)=COS(PLAT)
      C(6)=COS(PI2-PLON)
      C(7)=SIN(PLAT)
      C(8)=SIN(PI2-PLON)
      C(9)=C(5)*C(6)*C(1)*C(2)+C(5)*C(8)*C(1)*C(4)+C(7)*C(3)
      U=ACOS(C(9))
      IF(AMOD(SLON-PLON +4.*PI,PI2).GT.PI) U=-U
      C(10)=SIN(U)
      X=C(1)*C(4)*C(7)-C(5)*C(8)*C(3)
      Y=C(5)*C(6)*C(3)-C(1)*C(2)*C(7)
      Z=C(1)*C(2)*C(5)*C(8)-C(5)*C(6)*C(1)*C(4)
      AAA=0.0
      IF(X.NE.0.0.OR.Y.NE.0.0) AAA=ATAN2(Y,X)
      BBB=ASIN(Z/SQRT(X*X+Y*Y+Z*Z))
      CCC=PI2-PLON
      COSB=COS(BBB)
      SINB=SIN(BBB)
      COSA=COS(AAA)
      SINA=SIN(AAA)
      SINCA=SIN(CCC-AAA)
      COSCA=COS(CCC-AAA)
      IF(COSCA.EQ.0.) GO TO 411
      TANPSI=SINB*SINCA/COSCA
      COSPSI=-COSB*C(7)+SINB*C(5)*COSCA
      SINPSI=TANPSI*COSPSI
      GO TO 412
411   IF(SINB.EQ.0.) GO TO 413
      COSPSI=0.0
      SINPSI=SIGN(1.,SINB)
      GO TO 412
413   COSPSI=0.0
      SINPSI=1.0
      IF(AAA.NE.0.) SINPSI=-1.
412   HR(1,1)=COSPSI*SINB*COSA-SINPSI*SINA
      HR(1,2)=COSPSI*SINB*SINA+SINPSI*COSA
      HR(1,3)=-COSPSI*COSB
      HR(2,1)=-SINPSI*SINB*COSA-COSPSI*SINA
      HR(2,2)=-SINPSI*SINB*SINA+COSPSI*COSA
      HR(2,3)=SINPSI*COSB
      HR(3,1)=COSB*COSA
      HR(3,2)=COSB*SINA
      HR(3,3)=SINB
      RP=RPOL*RPOL
      RE=REQU*REQU
      RAD=RPOL
10    CONTINUE
      CA=COS(RLAT)
      CO=COS(PI2-RLON)
      SA=SIN(RLAT)
      SO=SIN(PI2-RLON)
      IF(RPOL.EQ.REQU) GO TO 11
      RAD=RE/SQRT(RE*CA*CA+RP*SA*SA)
11    CI=CA*CO*C(1)*C(2)+CA*SO*C(1)*C(4)+SA*C(3)
      CT=CA*CO*C(5)*C(6)+CA*SO*C(5)*C(8)+SA*C(7)
      CE=CT
      IF(RANGE.NE.0.) GO TO 20
      CG=C(9)
      GO TO 50
20    IF (CT.GT.1.0) CT=1.0
      IF (CT.LT.-1.0) CT=-1.0
      TH=ACOS(CT)
      ST=SIN(TH)
      IF(AMOD(CCC-(PI2-RLON)+4.*PI,PI2).GT.PI) ST=-ST
      DENOM=CT-RAD/RANGE
      IF(DENOM.NE.0.) GO TO 30
      CE=0.
      SE=SIGN(1.,ST)
      GO TO 40
30    E=ATAN2(ST,DENOM)
      CE=COS(E)
      SE=SIN(E)
40    if(st.eq.0.0)st=0.00001 ! try to avoid divide by zero
      CG=CI*CE+SE*(C(9)-CI*CT)/ST
50    TG=1000.
      G=ACOS(CG)
C  G LIES BETWEEN 0 AND PI
      IF(ABS(G-PIO2).GT..001) TG=TAN(G)
      IF(MODE.EQ.0) RETURN
      AS(1)=RAD*CO*CA
      AS(2)=RAD*SO*CA
      AS(3)=RAD*SA
      DO 70 I=1,3
          AL(I)=0.
          DO 71 II=1,3
71           AL(I)=AL(I)+AS(II)*HR(II,I)
70    CONTINUE
      LUMLON=ATAN2(AL(2),AL(1))*57.29578
      LUMLAT= ASIN(AL(3)/RAD)*57.29578
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lumllp.imake
/* Imake file for VICAR subroutine lumllp */

#define SUBROUTINE lumllp

#define MODULE_LIST lumllp.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tlumllp.f
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer(a-z)
      real slat,slon,plat,plon,rpol,requ,range,rlat,rlon,tg,cg,ci,ce,
     *   lumlat,lumlon,rad
      DATA rad/57.2957795/

      call XVMESSAGE(
     .    'this pix vgr:1636832 image space line 500,samp 500',' ')
      slat=.55539604/rad
      slon=169.91901/rad
      plat=-.08596189/rad
      plon=155.07442/rad
      rpol=1815.
      requ=1815.
      range=806022.25
      rlat=-13.8540/rad
      rlon=149.8150/rad
      mode=1
      call lumllp(slat,slon,plat,plon,rpol,requ,range,rlat,rlon,
     *  tg,cg,ci,ce,lumlat,lumlon,mode,0)
      tg=rad*(atan(tg))
      cg=rad*(acos(cg))
      ci=rad*(acos(ci))
      ce=rad*(acos(ce))
      call prnt(7,1,tg,    'phase=    .')
      call prnt(7,1,cg,    'phase=    .')
      call prnt(7,1,ci,    'incidence=.')
      call prnt(7,1,ce,    'emission= .')
      call prnt(7,1,lumlat,'lumlat=   .')
      call prnt(7,1,lumlon,'lumlon=   .')
      call XVMESSAGE('answer: phase=14.85,inci=24.57,emis=14.75',' ')
      call XVMESSAGE('lumlat=-12.586,lumlon=55.616',' ')

      call XVMESSAGE
     *('this pix is vgr 1634522 image space line 276.5 samp 429.97 ',
     .     ' ')
      slat=.55803038/rad
      slon=12.827187/rad
      plat=1.1882980/rad
      plon=17.688618/rad
      rpol=1815.
      requ=1815.
      range=2612172.5
      rlat=1./RAD
      rlon=17./rad
      first=1
      call lumllp(slat,slon,plat,plon,rpol,requ,range,rlat,rlon,
     *  tg,cg,ci,ce,lumlat,lumlon,mode,first)
      tg=rad*(atan(tg))
      cg=rad*(acos(cg))
      ci=rad*(acos(ci))
      ce=rad*(acos(ce))
      call prnt(7,1,tg,    'phase=    .')
      call prnt(7,1,cg,    'phase=    .')
      call prnt(7,1,ci,    'incidence=.')
      call prnt(7,1,ce,    'emission= .')
      call prnt(7,1,lumlat,'lumlat=   .')
      call prnt(7,1,lumlon,'lumlon=   .')
      call XVMESSAGE('answer:phase=4.902,inic=4.9156,emiss=0.71436',' ')
      call XVMESSAGE('lumlat=-1.9970,lumlon=-0.95807',' ')
      call XVMESSAGE
     *('this is pix is vgr 1634522 image space line 300 sample 400',' ')
      rlat=19.6076/rad
      rlon=41.2608/RAD
      first=0
      call lumllp(slat,slon,plat,plon,rpol,requ,range,rlat,rlon,
     *  tg,cg,ci,ce,lumlat,lumlon,mode,first)
      tg=rad*(atan(tg))
      cg=rad*(acos(cg))
      ci=rad*(acos(ci))
      ce=rad*(acos(ce))
      call prnt(7,1,tg,    'phase=    .')
      call prnt(7,1,cg,    'phase=    .')
      call prnt(7,1,ci,    'incidence=.')
      call prnt(7,1,ce,    'emission= .')
      call prnt(7,1,lumlat,'lumlat=   .')
      call prnt(7,1,lumlon,'lumlon=   .')
      call XVMESSAGE('answer:phase=4.8850,inci=33.73,emis=29.53',' ')
      call XVMESSAGE('lumlat=-23.42,lumlon=20.972',' ')
      return
      end
$!-----------------------------------------------------------------------------
$ create tlumllp.imake
/* Imake file for Test of VICAR subroutine lumllp */

#define PROGRAM tlumllp

#define MODULE_LIST tlumllp.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tlumllp.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstlumllp.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tlumllp
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create lumllp.hlp
1 LUMLLP

 Subroutine to compute phase, incidence, emission, luminance longitude and
luminance latitude angles for a planet.  LUMLLP is used in photometric
function calculations. 

 FORTRAN Calling sequence:

 CALL LUMLLP(SLAT,SLON,PLAT,PLON,RPOL,REQU,RANGE,RLAT,RLON,TG,CG,CI,
     CE,LUMLAT,LUMLON,MODE,FIRST)

2 ARGUMENTS

 SLAT subsolar latitude                      input    R*4 radians
 SLON subsolar longitude                     input    R*4 radians
 PLAT subspacecraft latitude                 input    R*4 radians
 PLON subspacecraft longitude                input    R*4 radians
 RPOL planet polar radius                    input    R*4 km
 REQU planet equatorial radius               input    R*4 km
 RANGE planet center to spacecraft distance. input    R*4 km
       RANGE=0.0 implies infinite separation.
 RLAT pixel position latitude                input    R*4 radians
 RLON pixel position longitude(west)         input    R*4 radians
 TG   tangent of phase angle (max of 1000.)  returned R*4 --
 CG   cosine of phase angle                  returned R*4 --
 CI   cosine of incidence angle              returned R*4 --
 CE   cosine of emission angle               returned R*4 --
 LUMLAT luminance latitude                   returned R*4 degrees
 LUMLON luminance longitude                  returned R*4 degrees
 MODE 0 specifies a return without computing    input I*4 --
        LUMLAT or LUMLON
 FIRST  an argument to force LUMLLP            input  I*4 
        to do the extensive set up 
        calculations before beginning.
        (See under method.)
2 METHOD
 Warning... an extensive calculation is performed the first time LUMLLP is
called to set certian parameters which depend upon SLAT, SLON, PLAT PLON,
REQU, RPOL.  If the arguments SLAT, SLON,PLAT,PLON,REQU, or RPOL should change,
then the user must set FIRST to 1 to force the extensive recalculation.
If FIRST is not 1, LUMLLP will do the extensive calculations as 
described above on the first call to LUMLLP. Upon subsequent calls to LUMLLP
those extensive setup calculations will be skipped.
If FIRST is set to equal 1, LUMLLP will always do the extensive setup 
calculations. 
 According to Jean Lorre (3-93) the values computed for luminance,
latitude, and longitude are of unknown meaning.  This does not appear to
matter under the current usage of LUMLLP.  Joel Mosher has a version of
LUMLLP where the values for luminance, latitude, and longitude are meaningful.

The values for emission, incidence, and phase have the regular meaning in this
version.
2 HISTORY
 Original Programmer: unknown  
 Converted to Vax by: Joel Mosher   17-APR-1986 
 Ported to UNIX: Steve Pohorsky
 Current Cognizant Programmer: Steve Pohorsky
 Source Language: FORTRAN
 Revision:
   26 AUG 80 -JJL- INITIAL RELEASE
   26 JUN 83 -JAM- CONVERTED TO VAX
   20 APR 87 -jam- check for zero divide at label 40
   21 Aug 89 -GMY- check for CT in range -1 to 1 before taking ACOS.
   22 Mar 93  SP   Made portable for UNIX.  Changed optional parameter (FIRST)
                   to mandatory for portability.
   19 May 93  SP   Grouped DATA statements after other declarations to meet
                   ANSI Fortran 77.
$ Return
$!#############################################################################
