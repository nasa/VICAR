$!****************************************************************************
$!
$! Build proc for MIPL module tranv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:39:45
$!
$! Execute by entering:		$ @tranv
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
$ write sys$output "*** module tranv ***"
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
$ write sys$output "Invalid argument given to tranv.com file -- ", primary
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
$   if F$SEARCH("tranv.imake") .nes. ""
$   then
$      vimake tranv
$      purge tranv.bld
$   else
$      if F$SEARCH("tranv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tranv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tranv.bld "STD"
$   else
$      @tranv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tranv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tranv.com -mixed -
	-s tranv.f ztranv.c -
	-i tranv.imake -
	-t ttranv.f tztranv.c ttranv.imake ttranv.pdf tsttranv.pdf -
	-o tranv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tranv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE TRANV(IND,TYPE,M,XC,ZC,TH,TH1,TH2,LAM,F,CAS,
     *  LINE,SAMPLE,LAT,LONG,RP,RE,PSI)
C           SYSTEMATIC CHANGES 
C         OLD FUNCTION     NEW FUNCTION
C         DARSIN           DASIN
C         DARCOS           DACOS
C         DCOTAN          1.D0/DTAN
C  CONVERT L,S TO LAT LONG OR LAT,LONG TO L,S

C  IND  0=O.K.  1=POINT OFF PLANET
C  M  1=DIRECT  2=INVERSE
C  TYPE PROJECTION TYPE
C      1=POLAR ORTHOGRAPHIC
C      2=OBLIQUE ORTHOGRAPHIC
C      3=POLAR STEREOGRAPHIC
C      4=OBLIQUE STEREOGRAPHIC
C      5=TWO-STANDARD LAMBERT CONFORMAL CONIC
C      6=MERCATOR
C      9=CYLINDRICAL
C     10=LAT-LONG (SIMPLE CYLINDRICAL)
C     11=OBLIQUE SIMPLE CYLINDRICAL
C     12=SINUSOIDAL
C     13=OBLIQUE SINUSOIDAL
C     14=MOLLWEIDE
C     15=TRANSVERSE MERCATOR
C

C DATA
C  1    XC  SPECIAL SAMPLE POINT
C  2    ZC  SPECIAL LINE POINT
C  3    TH  SPECIAL LATITUDE
C  4    TH1  LATITUDE OF SPECIAL PARALLEL OR SPECIAL OBLIQUE LONGITUDE
C  5    TH2  LATITUDE OF SPECIAL PARALLEL
C  6    LAM SPECIAL LONGITUDE    WEST
C  7    F  SCALE  (KM/PIXEL)
C  8    CAS  +1 IF VISIBLE POLE IS N.   -1 IF VISIBLE POLE IS S.
C       M  M=2  LINE,SAMPLE TO LAT,LONG   (INVERSE)
C       M  M=1  LAT,LONG TO LINE,SAMP  (DIRECT)
C  25   RP  POLAR RADIUS  (KM)
C  26   RE  EQUATORIAL RADIUS  (KM)
C  9    PSI   NORTH ANGLE
C
C  ******  ALL ANGLES IN DEGREES        ******
C  ******  ALL LATITUDES PLANETOCENTRIC ******
C  ******  ALL LONGITUDES WEST          ******

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D,DL, AA, BB,LAM,LAMR,LATR,LONGR,DEGKM,DSCALE
      DOUBLE PRECISION K1,K2,K3,K3SQRT,DIF1(3),DIF2(3),LAMBAR,NORTH,L
      DOUBLE PRECISION LAT8,LONG8,PIOVER180,LAM1,THH1,DXC,DZC
      DOUBLE PRECISION SMALL
      DATA SMALL/1D-8/
      REAL LINE,SAMPLE,LAT,LONG,A3,A4,A5,A6,A7,A8,A9
      INTEGER TYPE
      LOGICAL BOP,LSTAT
      DOUBLE PRECISION DD, LONG_HORIZON, HORIZON_THETA, HORIZON_X

C  SPECIAL FUNCTIONS DEFINED

C  GEOCENTRIC RADIUS
      GCR(RPP,REP,THR)=RPP*REP/DSQRT(RPP*RPP*DCOS(THR)**2+
     *   REP*REP*DSIN(THR)**2)
C  EPSILON
      EPSIL(RPP,REP)=DSQRT(1.D0-RPP*RPP/(REP*REP))
C  GEODETIC LATITUDE
      PHIG(PI,RPP,REP,THR)=PI/2.D0-DABS(DATAN(-RPP*RPP/(REP*REP)*
     *1.D0/DTAN(THR)))
C  GEOCENTRIC LATITUDE
      THEG(PI,RPP,REP,PHI)=DATAN(-RPP*RPP/(REP*REP)*1.D0/DTAN(
     *   DABS(PHI)-PI/2.D0))
C  RT SIDE EQ. 11
      RCHI(EPS,PHI,PI)=DTAN(PI/4.D0+PHI/2.D0)*
     *  ((1.D0-EPS*DSIN(PHI))/(1.D0+EPS*DSIN(PHI)))**(EPS/2.D0)
C  LOCAL RADIUS OF CURVATURE
      RN(REP,RPP,PHI)=REP*REP/DSQRT(REP*REP*DCOS(PHI)**2+
     *  RPP*RPP*DSIN(PHI)**2)
C  Q VALUE  (NOT SAME AS IN DOCUMENT)
      Q(EPS,PHI,CAS)=((1.D0+EPS*DSIN(CAS*PHI))/(
     *  1.D0-EPS*DSIN(CAS*PHI)))**(EPS/2.D0)
C  MAGNITUDE VECTOR SQUARED  (not used)
c      VM(X,Y,Z,X1,Y1,Z1)=(X-X1)**2+(Y-Y1)**2+(Z-Z1)**2
C  FOR TYPES 12 AND 13 - SEE USGS BULL. 1532, EQUATION (3-12)
c  computes authalic latitude from planetodetic latitude
      QAUT(SIN_PHI,EPSILON) =
     1	 (1 - EPSILON ** 2) *
     2   (SIN_PHI / (1 - (EPSILON * SIN_PHI) ** 2) -
     3    (1 / (2 * EPSILON)) * DLOG((1 - EPSILON * SIN_PHI) /
     4			 	   (1 + EPSILON * SIN_PHI)))

      D=0.0
      DL=0.0
      AA=0.0
      BB=0.0
      LAMR=0.0
      LATR=0.0
      LONGR=0.0
      DEGKM=0.0
      DSCALE=0.0
      K1=0.0
      K2=0.0
      K3=0.0
      K3SQRT=0.0
      CALL ZIA(DIF1,3)
      CALL ZIA(DIF2,3)
      LAMBAR=0.0
      NORTH=0.0
      L=0.0
      LAT8=0.0
      LONG8=0.0
      PIOVER180=0.0
      LAM1=0.0
      THH1=0.0
      DXC=0.0
      DZC=0.0
      SMALL=0.0
      A3=0.0
      A4=0.0
      A5=0.0
      A6=0.0
      A7=0.0
      A8=0.0
      A9=0.0
      DD=0.0
      LONG_HORIZON=0.0
      HORIZON_THETA=0.0
      HORIZON_X=0.0
      

      PI=3.141592653589793D0
      piover180=pi/180.d0
      IND=0

C  CONVERT ANGLES AND DIMENSIONS TO RADIANS AND PIXELS RESPECTIVELY

      RPP=RP/F
      REP=RE/F
      THR=TH*PIOVER180
      IF(THR.EQ.0.D0)THR=0.0000001d0
      THR0=THR
      THR1=TH1*PIOVER180
      THR2=TH2*PIOVER180
      LAMR=LAM*PIOVER180
      PSIR=PSI*PIOVER180
      IF(THR1.EQ.0.D0)THR1=0.0000001d0
      IF(THR2.EQ.0.D0)THR2=0.0000001d0
      IF (M.NE.2) THEN
	LAT8=LAT
	LONG8=LONG
	LATR=LAT*PIOVER180
	IF(LATR.EQ.0.D0)LATR=0.0000001d0
	LONGR=LONG*PIOVER180
      ENDIF

      GO TO (100,200,300,400,500,600,700,800,900,1000,
     1       1100,1200,1300,1400,1500,1600),TYPE

      CALL XVMESSAGE('TRANV: Unknown projection type',' ')
      IND=1
      RETURN
700   CONTINUE
      CALL XVMESSAGE('TRANV DOES NOT SUPPORT IMAGE SPACE',' ')
      IND=1
      RETURN
800   CONTINUE
      CALL XVMESSAGE('TRANV DOES NOT SUPPORT OBJECT SPACE',' ')
      IND=1
      RETURN
1600  CONTINUE
      CALL XVMESSAGE('TRANV DOES NOT SUPPORT PERSPECTIVE',' ')
      IND=1
      RETURN

C  POLAR ORTHOGRAPHIC

c DATA array elements used: 1,2,3,6,7,8,25,26
100   CONTINUE
      if (CAS .NE. 1.D0 .AND. CAS .NE. -1.D0)   
     .      call mabend('TRANV: CAS MUST BE 1.D0 OR -1.D0')
      IF(M.EQ.2) GO TO 150
C  DIRECT
      R=GCR(RPP,REP,LATR)
      LINE=-R*DCOS(LATR)*DCOS(LONGR-LAMR)+ZC
      SAMPLE=R*DCOS(LATR)*DSIN(LONGR-LAMR)*CAS+XC
      IF(DABS(LAT8-TH).GT.90.)IND=1 ! TEST FOR BACKOF PLANET
      RETURN

150   CONTINUE
C  INVERSE
      IF(ZC.EQ.LINE.AND.SAMPLE.EQ.XC)THEN  ! DATAN2 CAN'T HAVE A NEGAITVE DIVISOR
         DELL=-1.570796326794897D0
         RAD=0.001
      ELSE
         DELL=DATAN2(CAS*(SAMPLE-XC),-(LINE-ZC))
         RAD=DSQRT((SAMPLE-XC)**2+(LINE-ZC)**2)
      ENDIF
      LONG=(DELL+LAMR)*180./PI
      IF(LONG.LT.0.) LONG=LONG+360.
      IF(LONG.GE.360.)LONG=LONG-360.
      A=RPP*RPP/(RAD*RAD)-RPP*RPP/(REP*REP)
      IF(A.GT.0.D0) GO TO 151
      IND=1
      RETURN
151   LAT=CAS*DATAN(DSQRT(A))*180./PI
      RETURN

C  OBLIQUE ORTHOGRAPHIC

c DATA array elements used: 1,2,3,6,7,9,25,26
200   IF(M.EQ.2) GO TO 250
C  DIRECT
      R=GCR(RPP,REP,LATR)
      PHI=PHIG(PI,RPP,REP,THR)
      PHI=DSIGN(PHI,THR)
      X11=-R*DCOS(LATR)*DSIN(LONGR-LAMR)
      Z11=R*(DSIN(PHI)*DCOS(LATR)*DCOS(LONGR-LAMR)
     *-DCOS(PHI)*DSIN(LATR))
      X1=X11
      Z1=Z11-GCR(RPP,REP,THR)*DSIN(PHI-THR)
      SAMPLE=X1*DCOS(PSIR)-Z1*DSIN(PSIR)+XC
      LINE=X1*DSIN(PSIR)+Z1*DCOS(PSIR)+ZC
      LSTAT=BOP(TH*PIOVER180,LAM*PIOVER180,LAT8*PIOVER180,
     *         LONG8*PIOVER180)  
      IF(LSTAT)IND=1 
      RETURN
250   CONTINUE
C  INVERSE
      RLAT=SAMPLE-XC
      RLON=LINE-ZC
      IF(RLAT.NE.0.0.OR.RLON.NE.0.0) GO TO 220
      LAT=TH
      LONG=LAM
      RETURN
220   CONTINUE
      CPHI=TH
      CPSI=LAM
      NORTH=PSI
      SINLAT=DSIN(THR)
      COSLAT=DCOS(THR)
      SINLON=DSIN(LAMR)
      COSLON=DCOS(LAMR)
      SINNOR=DSIN(PSIR)
      COSNOR=DCOS(PSIR)
      FL=RP
      REQ=RE
      DPI=PI
      SLCCPC=SINLAT*COSLON
      SLCSPC=SINLAT*SINLON
      SCPCSL=SINLON*COSLON*SINLAT
      SCPCCL=SINLON*COSLON*COSLAT
      CLCC2P=COSLAT*COSLON*COSLON
      CLCS2P=COSLAT*SINLON*SINLON
      SLCC2P=SINLAT*COSLON*COSLON

C     CALC ANGLE LAMBDA BAR
      RPSQ=FL
      RPSQ=RPSQ*RPSQ
      RESQ=REQ
      RESQ=RESQ*RESQ
      LAMBAR=((COSLAT*COSLAT/RESQ+SINLAT*SINLAT/RPSQ)/
     &DSQRT((COSLAT*COSLAT/(RESQ*RESQ)+SINLAT*SINLAT/(RPSQ*RPSQ))))
       ! 13-FEB-1986 JAM PREVENT INVALID ARGUMENT TO MATH LIBRARY
       ! 28-JAN-1993 SP  CHANGED ABOVE CORRECTION TO USE 1.D0 INSTEAD OF .99999
       !                 SINCE INVALID ARG ONLY COMES FROM LAMBAR>1.D0.
      IF(LAMBAR.gt. 1.D0)LAMBAR=1.D0
      LAMBAR=DACOS(LAMBAR)
      LAMBAR=((CPHI))*PIOVER180+LAMBAR
      SINLAM=DSIN(LAMBAR)
      COSLAM=DCOS(LAMBAR)
      L=(CPHI)*PIOVER180-LAMBAR
      SINL=DSIN(L)
      COSL=DCOS(L)

C     GET RADIUS OF PLANET AT C.P.
      RCP= GCR(RPP,REP,THR)

C     CONVERT FROM PIXELS TO KM
      RCP=F*RCP

C     CALC.ANGLE BETWEEN UP AND POINT OF INTEREST
C     IN PLANE OF PROJECTION SUBTENDED AT CENTER OF PROJECTION
      DELX=RLAT
      XDEL=DELX
      DELZ=RLON
      ZDEL=DELZ
      APOIUP=DATAN2(-XDEL,-ZDEL)

C     CALC.SIN AND COS OF THE ANGLE BETWEEN THE DIRECTION OF
C     NORTH IN THE IMAGE PLANE AND THE POINT OF INTEREST SUBTENDED AT
C     THE CENTER OF PROJECTION
      ADEL=((NORTH)*PIOVER180)+APOIUP
      SINDEL=DSIN(ADEL)
      COSDEL=DCOS(ADEL)
      IF(SINDEL.EQ.1.0D0)COSDEL=0.0D0
      IF(SINDEL.EQ.-1.0D0)COSDEL=0.0D0

C     CALC.DISTANCE OF POINT OF INTEREST FROM
C     CENTER OF PROJECTION IN PLANE OF PROJECTION
C     AT TRUE SCALE
      DD=    (F)*DSQRT((XDEL*XDEL)+(ZDEL*ZDEL))

C     CHECK WHETHER POINT OF INTEREST IS OFF PLANET
      IF(    (REQ).LT.DD) GO TO 999

C     CALC.COEFFIEIENTS FOR TWO PLANES NORMAL
C     TO PLANE OF PROJECTION.

C     PLANE 1 - NORMAL TO LINE CONNECTION CENTER OF PROJECTION
C     AND POINT OF INTEREST
C     PLANE 2 - CONTAINS LINE CONNECTION CENTER OF
C     PROJECTION AND POINT OF INTEREST

C     PLANE 1 A1*X+B1*Y+C1*Z+D1=0
C     PLANE 2 A2*X+B2*Y+C2*Z=0

      A1=-SINDEL*SINLON-COSDEL*COSLON*SINLAM
      B1=-SINDEL*COSLON+COSDEL*SINLON*SINLAM
      C1=COSDEL*COSLAM
      D1=-DD*SINDEL*SINDEL+RCP*COSDEL*SINLAM*COSLAT
     &-RCP*SINLAT*COSLAM*COSDEL-DD*COSDEL*COSDEL*SLCC2P*SINLAM
     &-DD*COSDEL*COSDEL*COSLAM*COSLAM
     &-DD*SINLAM*SINLAM*COSDEL*COSDEL*SINLON*SINLON
      A2=-COSDEL*SINLON*COSL+SINDEL*SLCCPC
      B2=-COSDEL*COSLON*COSL-SINDEL*SLCSPC
      C2=-COSLAT*SINDEL

C     CALCULATE PARAMETRIC VARIABLES IN
C     SIMULTANEOUS SOLN.OF PLANE 1,PLANE 2,AND SPHEROID

      ALPHA=A2*C1-A1*C2
      BETA=A2*B1-A1*B2
      GAMMA=B1*C2-B2*C1
      DELTA=C1*B2-B1*C2

C     CALCULATE X COORDINATE

C     EQUATION IS X=K1+OR-K2*SQRT(K3)

      ALPHSQ=ALPHA*ALPHA
      BETASQ=BETA*BETA
      GAMMSQ=GAMMA*GAMMA
      DELTSQ=DELTA*DELTA
      D1SQ=D1*D1
      C2SQ=C2*C2
      B2SQ=B2*B2
      GRESQ=GAMMSQ*RESQ
      DRPSQ=DELTSQ*RPSQ
      Z1=DRPSQ*(ALPHSQ+GAMMSQ)+BETASQ*GRESQ
      K1=((ALPHA*C2*D1*DRPSQ)+(BETA*B2*D1*GRESQ))/Z1
      K2=(GAMMA*DELTA*    (FL))/Z1
      K3=2.D0*ALPHA*C2*BETA*B2*RESQ
      K3=K3+(-C2SQ*DRPSQ-B2SQ*GRESQ-ALPHSQ*B2SQ*RESQ-BETASQ*RESQ*C2SQ)
      K3=K3*D1SQ
      K3=K3+(GRESQ*DRPSQ+DRPSQ*RESQ*ALPHSQ+RESQ*BETASQ*GRESQ)
      IF(K3.LT.0.D0) GO TO 999
      K3SQRT=DSQRT(K3)
      Z1=K2*K3SQRT
      X1=K1+Z1
      X2=K1-Z1

C     MAKE THE BACK OF PLANET TEST

      Y1=-D1*C2
      Y2=Y1
      Y1=(Y1+ALPHA*X1)/GAMMA
      Y2=(Y2+ALPHA*X2)/GAMMA
      Z1=(-B2*D1+BETA*X1)/DELTA
      Z2=(-B2*D1+BETA*X2)/DELTA

C     (X1,Y1,Z1) IS VECTOR P01
C     (X2,Y2,Z2) IS VECTOR P02
C     PCP IS VECTOR FROM PLANET CENTER TO CENTER OF PROJECTION
C     FIND WHICH VECTOR HAS MINIMUM LENGTH, P01-PCP  OR  P02-PCP

      PCPX=RCP*COSLAT*COSLON
      PCPY=-RCP*COSLAT*SINLON
      PCPZ=RCP*SINLAT
      DIF1(1)=X1-PCPX
      DIF1(2)=Y1-PCPY
      DIF1(3)=Z1-PCPZ
      DIF2(1)=X2-PCPX
      DIF2(2)=Y2-PCPY
      DIF2(3)=Z2-PCPZ
      RAD1=DIF1(1)*DIF1(1)+DIF1(2)*DIF1(2)+DIF1(3)*DIF1(3)
      RAD2=DIF2(1)*DIF2(1)+DIF2(2)*DIF2(2)+DIF2(3)*DIF2(3)
      IF(RAD1.GT.RAD2) GO TO 210
C     POINT 1 IS VALID
      RLON=360.D0-(180.D0*DATAN2(Y1,X1)/DPI)
      RLON=DMOD(RLON+360.D0,360.D0)
      RLAT=(DATAN(DABS(Z1)/DSQRT(X1*X1+Y1*Y1)))*180.D0/DPI
      RLAT=DSIGN(RLAT,Z1)
      LAT=RLAT
      LONG=RLON
      RETURN
C     POINT 2 IS VALID
210   RLON=360.D0-(180.D0*DATAN2(Y2,X2)/DPI)
      RLON=DMOD(RLON+360.D0,360.D0)
      RLAT=(DATAN(DABS(Z2)/DSQRT(X2*X2+Y2*Y2)))*180.D0/DPI
      RLAT=DSIGN(RLAT,Z2)
      LAT=RLAT
      LONG=RLON
      RETURN
999   CONTINUE
      IND=1
      RETURN

C  POLAR STEREOGRAPHIC

c DATA array elements used: 1,2,3,6,7,8,25,26
300   CONTINUE
      if (CAS .NE. 1.D0 .AND. CAS .NE. -1.D0)   
     .      call mabend('TRANV: CAS MUST BE 1.D0 OR -1.D0')
      IF(M.EQ.2) GO TO 350
C  DIRECT
      EPS=EPSIL(RPP,REP)
      PHI=PHIG(PI,RPP,REP,LATR)
      PHI=DSIGN(PHI,LATR)
      A=2.D0*REP*(1.D0+EPS)**(-(1.D0-EPS)/2.D0)
      B=(1.D0-EPS)**(-(1.D0+EPS)/2.D0)
      C=DTAN(PI/4.D0-CAS*PHI/2.D0)
      D=((1.D0+EPS*DSIN(CAS*PHI))/(1.D0-EPS*DSIN(CAS*PHI)))**(EPS/2.D0)
      RHO=A*B*C*D
      SAMPLE=CAS*RHO*DSIN(LONGR-LAMR)+XC
      LINE=-RHO*DCOS(LONGR-LAMR)+ZC
      RETURN
350   CONTINUE
C  INVERSE
      EPS=EPSIL(RPP,REP)
      A=(1.D0+EPS)**(-(1.D0-EPS)/2.D0)
      B=(1.D0-EPS)**(-(1.D0+EPS)/2.D0)
      C=A*B*2.D0*REP
      RHO=DSQRT((SAMPLE-XC)**2+(LINE-ZC)**2)
      DLINE=LINE-ZC
      DSAMP=SAMPLE-XC
      LONG=LAMR*180./PI   ! FIX IF LAT IS AT POLE
C      IF(DSAMP.NE.0.0D0.AND.DLINE.NE.0.0D0)THEN
C          LONG=(DATAN2(CAS*(DSAMP),-(DLINE)))*180./PI+LONG
C      ENDIF
      IF(DSAMP.EQ.0D0.AND.DLINE.EQ.0D0)THEN
         LONG=LAMR*180./PI
       ELSE
          LONG=(DATAN2(CAS*(DSAMP),-(DLINE)))*180./PI+LONG
      ENDIF
      IF(LONG.LT.0.) LONG=LONG+360.
      CHI=2.D0/CAS*(PI/4.D0-DATAN(RHO/C))
      CALL ITER(CHI,PHI,EPS,PI)
      DLAT=PHI   !   FIX IF LAT IS AT POLE
      IF(DABS(PHI).NE.PI/2.D0)THEN
             DLAT=THEG(PI,RPP,REP,PHI)
      ENDIF
      LAT=DSIGN(DLAT,PHI)*180./PI
      RETURN

C  OBLIQUE STEREOGRAPHIC

c DATA array elements used: 1,2,3,6,7,9,25,26
400   IF(M.EQ.2) GO TO 450
C  DIRECT
      EPS=EPSIL(RPP,REP)
      PHI0=PHIG(PI,RPP,REP,THR)
      PHI0=DSIGN(PHI0,THR)
      CHI0=2.*(DATAN(RCHI(EPS,PHI0,PI))-PI/4.)
      PHI=PHIG(PI,RPP,REP,LATR)
      PHI=DSIGN(PHI,LATR)
      CHI=2.*(DATAN(RCHI(EPS,PHI,PI))-PI/4.)
      RN0=RN(REP,RPP,PHI0)
      RINT=RN0*DCOS(PHI0)/DCOS(CHI0)
      A=-2.*RINT*DCOS(CHI)*DSIN(LONGR-LAMR)
      X1=A/(1.+DSIN(CHI0)*DSIN(CHI)+DCOS(CHI0)*DCOS(CHI)*DCOS(LONGR-LAMR
     *))
      A=-2.*RINT*(DCOS(CHI0)*DSIN(CHI)-DSIN(CHI0)*DCOS(CHI)*DCOS(LONGR-L
     *AMR))
      Z1=A/(1.+DSIN(CHI0)*DSIN(CHI)+DCOS(CHI0)*DCOS(CHI)*DCOS(LONGR-LAMR
     *))
      SAMPLE=X1*DCOS(PSIR)-Z1*DSIN(PSIR)+XC
      LINE=X1*DSIN(PSIR)+Z1*DCOS(PSIR)+ZC
      RETURN
450   CONTINUE
C  INVERSE
      X1=(SAMPLE-XC)*DCOS(PSIR)+(LINE-ZC)*DSIN(PSIR)
      Y1=-(SAMPLE-XC)*DSIN(PSIR)+(LINE-ZC)*DCOS(PSIR)
      EPS=EPSIL(RPP,REP)
      PHI0=PHIG(PI,RPP,REP,THR)
      PHI0=DSIGN(PHI0,THR)
      RN0=RN(REP,RPP,PHI0)
      CHI0=2.*(DATAN(RCHI(EPS,PHI0,PI))-PI/4.)
      RINT=RN0*DCOS(PHI0)/DCOS(CHI0)
      Z=RINT*(X1*X1+Y1*Y1-4.*RINT**2)/(X1*X1+Y1*Y1+4.*RINT**2)
      X=-X1*(Z+RINT)/(2.*RINT)+X1
      Y=-Y1*(Z+RINT)/(2.*RINT)+Y1
      SINLA0=DSIN(LAMR)
      COSLA0=DCOS(LAMR)
      SINCH0=DSIN(CHI0)
      COSCH0=DCOS(CHI0)
      XF=X*SINLA0+Y*SINCH0*COSLA0-Z*COSCH0*COSLA0
      YF=X*COSLA0-Y*SINCH0*SINLA0+Z*COSCH0*SINLA0
      ZF=-Y*COSCH0-Z*SINCH0
      LONG=DATAN2(-YF,XF)*180./PI
      IF(LONG.LT.0.) LONG=LONG+360.
      CHI=DASIN(ZF/RINT)
      CALL ITER(CHI,PHI,EPS,PI)
      DLAT=THEG(PI,RPP,REP,PHI)
      LAT=DSIGN(DLAT,PHI)*180./PI
      RETURN

C  LAMBERT

c DATA array elements used: 1,2,3,4,5,6,7,8,25,26
500   CONTINUE
      if (CAS .NE. 1.D0 .AND. CAS .NE. -1.D0)   
     .      call mabend('TRANV: CAS MUST BE 1.D0 OR -1.D0')
      PHI1=PHIG(PI,RPP,REP,THR1)
      PHI1=DSIGN(PHI1,THR1)
      PHI2=PHIG(PI,RPP,REP,THR2)
      PHI2=DSIGN(PHI2,THR2)
      RN1=RN(REP,RPP,PHI1)
      RN2=RN(REP,RPP,PHI2)
      A=DLOG(RN1*DCOS(PHI1))-DLOG(RN2*DCOS(PHI2))
      B=DTAN(PI/4.-CAS*PHI1/2.)
      C=DTAN(PI/4.-CAS*PHI2/2.)
      EPS=EPSIL(RPP,REP)
      D=Q(EPS,PHI1,CAS)
      RK=A/(DLOG(D*B)-DLOG(Q(EPS,PHI2,CAS)*C))
      C=RN1*DCOS(PHI1)/(RK*(D*B)**RK)
      IF(M.EQ.2) GO TO 550
C  DIRECT
      PHI=PHIG(PI,RPP,REP,LATR)
      PHI=DSIGN(PHI,LATR)
      RHO=C*(Q(EPS,PHI,CAS)*DTAN(PI/4.-CAS*PHI/2.))**RK
      G=LONGR-LAMR
553   IF(G.GE.-PI) GO TO 551
      G=G+2.*PI
      GO TO 553
551   IF(G.LE.PI) GO TO 552
      G=G-2.*PI
      GO TO 551
552   CONTINUE
      SAMPLE=-RHO*DSIN(RK*G)+XC
      LINE=CAS*RHO*DCOS(RK*G)+ZC
      RETURN
550   CONTINUE
C  INVERSE
      IF(SAMPLE.EQ.XC.AND.LINE.EQ.ZC)THEN
        RHO=.001
        Z=0.
      ELSE
         RHO=DSQRT((SAMPLE-XC)**2+(LINE-ZC)**2)
         Z=DATAN2(SAMPLE-XC,CAS*(LINE-ZC))/RK
      ENDIF
      IF(DABS(Z).LE.PI) GO TO 560
      IND=1
      RETURN
560   LONG=(LAMR-Z)*180./PI
      IF(LONG.LT.0.) LONG=LONG+360.
      E=DATAN((C/RHO)**(1./RK))
      CHI=(E-PI/4.)*2.D0/CAS
      CALL ITER(CHI,PHI,EPS,PI)
      DLAT=THEG(PI,RPP,REP,PHI)
      LAT=DSIGN(DLAT,PHI)*180./PI
      RETURN

C  MERCATOR

c DATA array elements used: 3,6,7,25,26
600   CONTINUE
C  COMPUTE ZC AS LINE OF EQUATOR
      PHIL1=PHIG(PI,RPP,REP,THR)
      PHIL1=DSIGN(PHIL1,THR)
C  SOLVE EQ. 67 FOR ZC
      EPS=EPSIL(RPP,REP)
      A=((1.D0-EPS*DSIN(PHIL1))/(1.D0+EPS*DSIN(PHIL1)))**(EPS/2.D0)
      B=DTAN(PI/4.D0+PHIL1/2.D0)
      ZC=1.D0+REP*DLOG(A*B)
      IF(M.EQ.2) GO TO 650
C  DIRECT
      DELL=LONGR-LAMR
605   IF(DABS(DELL).LE.PI) GO TO 610
      IF(DELL.GT.0.0D0) GO TO 620
      DELL=DELL+2.D0*PI
      GO TO 605
620   DELL=DELL-2.D0*PI
      GO TO 605
610   CONTINUE
      SAMPLE=-REP*DELL+XC
      DPHI=PHIG(PI,RPP,REP,LATR)
      PHI=DSIGN(DPHI,LATR)
      A=((1.D0-EPS*DSIN(PHI))/(1.D0+EPS*DSIN(PHI)))**(EPS/2.D0)
      B=DTAN(PI/4.D0+PHI/2.D0)
      if (b.eq.0.0d0) b=1.0d-15
      LINE=-REP*DLOG(A*B)+ZC
      RETURN
650   CONTINUE
C  INVERSE
      A=(SAMPLE-XC)/REP
      IF(A.GE.-PI*2.D0.AND.A.LE.PI*2.D0)GO TO 660
C     THE NEXT LINE WAS ORIGINAL CODE
C     IF(A.GE.0.D0.AND.A.LE.PI*2.D0)GO TO 660
      IND=1
      RETURN
660   LONG=(-A+LAMR)*180./PI
      IF(LONG.LT.0.) LONG=LONG+360.
      A=DEXP(-(LINE-ZC)/REP)
      CHI=2.D0*(DATAN(A)-PI/4.D0)
      CALL ITER(CHI,PHI,EPS,PI)
      DLAT=THEG(PI,RPP,REP,PHI)
      LAT=DSIGN(DLAT,PHI)*180./PI
      RETURN

C  CYLINDRICAL

c DATA array elements used: 2,6,7,25,26
900   CONTINUE
      A3=RP
      A4=RE
      A5=PI
      A6=-99999.
      A7=LAM
      A8=F
      A9=ZC
      IF(M.EQ.2) GO TO 950
C  DIRECT  LAT,LON TO L,S
      LINE=LONG
      SAMPLE=LAT
      CALL CYLCAL(2,SAMPLE,LINE,A9,A7,A8,A3,A4,A5,A6)
      IF(LINE.EQ.-99999.) IND=1
      RETURN
C  INVERSE  L,S TO LAT,LON
950   CONTINUE
      LAT=SAMPLE
      LONG=LINE
      CALL CYLCAL(1,LAT,LONG,A9,A7,A8,A3,A4,A5,A6)
      IF(LAT.EQ.-99999.) IND=1
      RETURN

C LAT LONG, RECTANGULAR  (SIMPLE CYLINDRICAL)

c DATA array elements used: 1,2,3,6,7,26
1000        CONTINUE
            DEGKM=360.D0/(2.D0*RE*PI)
            DSCALE=F*DEGKM
            DZC=(ZC-1)*DSCALE
            DXC=(XC-1)*DSCALE
            THH1=TH+DZC
            LAM1=DMOD((LAM+DXC),360.D0)
            IF(M.EQ.2) GOTO 1050

C           DIRECT

            LINE=((THH1-LAT)/DSCALE)+1
            DL = LONG - LAM
            DL = DMOD(DL,360.D0)
            SAMPLE =XC-(DL/DSCALE)
            RETURN

C           INVERSE

1050        CONTINUE
            AA=(XC-SAMPLE)*DSCALE
            LONG=LAM+AA
            LONG=AMOD(LONG,360.0)
            IF(LONG.LT.0.D0) LONG=360.0+LONG

            BB=(LINE-ZC)*DSCALE
            LAT=TH-BB
            IF(LAT.GE.-90.0.AND.LAT.LE.90.0) GOTO 1070

C           THIS SECTION HANDLES POLAR LATITUDE WRAP AROUND
C           NOT INSTALLED AS OF 14-JAN-80

            IND=1

1070        RETURN

C       OBLIQUE SIMPLE CYLINDRICAL

c DATA array elements used: 1,2,3,4,6,7,26
C	UPPER AXIS OF PROJECTION IS AT (PHI,LAM); CENTER OF PROJECTION IS
C	TH1 DEGREES TO THE LEFT OF PROJECTED NORTH POLE AND IS AT (ZC,XC).

1100    CONTINUE

        IF(M .EQ. 1) THEN

C       DIRECT

  	    CALL OBLIQUE(LATR,LONGR,THR0,LAMR,THR1,1)
	    LINE = ZC - LATR * REP
	    SAMPLE = (PI - LONGR) * REP + XC

  	ELSE

C       INVERSE

	    LATR = (ZC - LINE) / REP
	    LONGR = (XC - SAMPLE) / REP + PI
  	    CALL OBLIQUE(LATR,LONGR,THR0,LAMR,THR1,2)
  	
	    LAT = LATR / PIOVER180	
	    LONG = LONGR / PIOVER180
	    IF(LONG .LT. 0) LONG = LONG + 360
  	    IF(LONG .GE. 360) LONG = LONG - 360


  	ENDIF

        RETURN

1200	CONTINUE

C	SINUSOIDAL
C	OF AUTHALIC SPHERE -- C.F. USGS BULL. 1532, P. 19 

c DATA array elements used: 1,2,3,6,7,25,26
  	IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	    EPSILON = EPSIL(RP,RE)			!ECCENTRICIY
  	    QP = QAUT(1.D0,EPSILON)
            rep=rep*dsqrt(qp/2.d0)                      ! authalic radius
            temp=PHIG(pi,rpp,rep,thr0)                  ! geodetic lat
            thr0=dsign(temp,thr0)
            thr0=DASIN(QAUT(DSIN(thr0),EPSILON) / QP) !AUTHALIC LATITUDE
  	END IF

  	IF(M .EQ. 1) THEN

C	DIRECT

  	    IF (RE .NE. RP) THEN			!IF ELLIPSOID
                temp=PHIG(pi,rpp,rep,latr)              ! geodetic lat
                LATR=dsign(temp,latr)
                temp=qaut(dsin(latr),epsilon)/qp
                if(temp.lt.-1.d0) temp=-1.d0
                if(temp.gt.1.d0) temp=1.d0
                latr=dasin(temp)                      ! authalic latitude
c 		LATR = DASIN(QAUT(DSIN(LATR),EPSILON) / QP)   !AUTHALIC LATITUDE
  	    END IF

  	    LINE = ZC - (LATR - THR0) * REP
  	    DELTA = LAMR - LONGR
  	    IF(DELTA .LT. -PI) DELTA = DELTA + 2*PI
  	    IF(DELTA .GE. PI) DELTA = DELTA - 2*PI
  	    SAMPLE = DELTA * DCOS(LATR) * REP + XC

  	ELSE

C	INVERSE

  	    LATR = (ZC - LINE) / REP + THR0
  	    IF(LATR .LT. -PI/2 .OR. LATR .GT. PI/2) THEN
  		IND = 1
  		RETURN
  	    END IF
  	    IF(PI/2 - DABS(LATR) .GT. SMALL) THEN
  		DELTA = (XC - SAMPLE) / (REP * DCOS(LATR))
  		IF(DELTA .LT. -PI .OR. DELTA .GE. PI) THEN
  		    IND = 1
  		    RETURN
  		END IF
  	        LONGR = DELTA + LAMR
  		IF(LONGR .LT. 0) LONGR = LONGR + 2*PI
  		IF(LONGR .GE. 2*PI) LONGR = LONGR - 2*PI
  	    ELSE
  		LONGR = LAMR
  	    END IF
  		
  	    IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	      CALL AUTHALIC(LATR,QP,EPSILON)       ! authalic to geodetic
              temp=THEG(pi,rpp,rep,latr)           ! to geocentric     
              latr=dsign(temp,latr)
  	    END IF

  	    LAT = LATR / PIOVER180
  	    LONG = LONGR / PIOVER180

  	END IF

  	RETURN


C	OBLIQUE SINUSOIDAL
C	OF OBLIQUE AUTHALIC SPHERE -- C.F. USGS BULL. 1532, P. 19 

C	UPPER POLE OF PROJECTION IS AT (PHI,LAM); CENTER OF PROJECTION IS
C	TH1 DEGREES TO THE LEFT OF PROJECTED NORTH POLE AND IS AT (ZC,XC).

c DATA array elements used: 1,2,3,4,6,7,25,26
1300	CONTINUE

  	IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	    EPSILON = EPSIL(RP,RE)			!ECCENTRICIY
  	    QP = QAUT(1.D0,EPSILON)
            rep=rep*dsqrt(qp/2.d0)                      ! authalic radius
  	END IF

  	IF(M .EQ. 1) THEN

C	DIRECT

  	    IF (RE .NE. RP) THEN			!IF ELLIPSOID
                temp=PHIG(pi,rpp,rep,latr)              ! geodetic lat
                LATR=dsign(temp,latr)
                temp=qaut(dsin(latr),epsilon)/qp
                if(temp.lt.-1.d0) temp=-1.d0
                if(temp.gt.1.d0) temp=1.d0
                latr=dasin(temp)                      ! authalic latitude
c 		LATR = DASIN(QAUT(DSIN(LATR),EPSILON) / QP)   !AUTHALIC LATITUDE
  	    END IF

  	    CALL OBLIQUE(LATR,LONGR,THR0,LAMR,THR1,1)	!OBLIQUE LAT,LON

  	    LINE = ZC - LATR * REP			!LINE,SAMPLE
  	    DELTA = PI - LONGR
  	    IF(DELTA .LT. -PI) DELTA = DELTA + 2*PI
  	    IF(DELTA .GE. PI) DELTA = DELTA - 2*PI
  	    SAMPLE = DELTA * DCOS(LATR) * REP + XC

  	ELSE

C	INVERSE

  	    LATR = (ZC - LINE) / REP			!OBLIQUE LAT,LON
  	    IF(LATR .LT. -PI/2 .OR. LATR .GT. PI/2) THEN
  		IND = 1
  		RETURN
  	    END IF
  	    IF(PI/2 - DABS(LATR) .GT. SMALL) THEN
  		DELTA = (XC - SAMPLE) / (REP * DCOS(LATR))
  		IF(DELTA .LT. -PI .OR. DELTA .GE. PI) THEN
  		    IND = 1
  		    RETURN
  		END IF
  	        LONGR = DELTA + PI
  	    ELSE
  		LONGR = PI
  	    END IF
  		
  	    CALL OBLIQUE(LATR,LONGR,THR0,LAMR,THR1,2)	!AUTHALIC LAT,LON

  	    IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	      CALL AUTHALIC(LATR,QP,EPSILON)		! to geodetic
              temp=THEG(pi,rpp,rep,latr)           ! to geocentric     
              latr=dsign(temp,latr)
  	    END IF

  	    LAT = LATR / PIOVER180
  	    LONG = LONGR / PIOVER180
	    IF(LONG .LT. 0) LONG = LONG + 360
  	    IF(LONG .GE. 360) LONG = LONG - 360

  	END IF

	RETURN
C
C
C       MOLLWEIDE PROJECTION
C
c DATA array elements used: 1,2,7,26
1400	CONTINUE

  	IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	    EPSILON = EPSIL(RP,RE)			!ECCENTRICIY
  	    QP = QAUT(1.D0,EPSILON)
            rep=rep*dsqrt(qp/2.d0)                      ! authalic radius
  	END IF

	IF ( M .EQ. 1 ) THEN

C	DIRECT

  	    IF (RE .NE. RP) THEN			!IF ELLIPSOID
                temp=PHIG(pi,rpp,rep,latr)              ! geodetic lat
                LATR=dsign(temp,latr)
                temp=qaut(dsin(latr),epsilon)/qp
                if(temp.lt.-1.d0) temp=-1.d0
                if(temp.gt.1.d0) temp=1.d0
                latr=dasin(temp)                      ! authalic latitude
c 		LATR = DASIN(QAUT(DSIN(LATR),EPSILON) / QP)   !AUTHALIC LATITUDE
  	    END IF

           THETA = LATR
	   CALL MOLLWEIDE_ITERATEr ( THETA, LATR )
	   RX = (2*DSQRT(2.D+00)/PI)*(rep)*(LAMR-LONGR)*DCOS(THETA)
	   RY = DSQRT(2.D+00)*(rep)*DSIN(THETA)

	   LINE = ZC - RY
	   SAMPLE = XC + RX

	   RETURN

	ELSE

C	INVERSE

	   RY = ( ZC - LINE ) 
	   RX = ( SAMPLE - XC ) 

	   DD = ( RY / ( DSQRT(2.D+00)*(rep) ))

	   IF ( (DD .GT. 1) .OR. (DD .LT. -1) ) THEN
	      IND=1
C	      IF DD > 1 THEN WE ARE OFF THE PLANET AND THE
C	      PROSPECTIVE TIEPOINT CAN NOT BE USED.
	      RETURN
	   END IF

	   THETA = DASIN ( DD )
	   LATR = DASIN ( ( 2*THETA + DSIN(2*THETA) ) / PI )

C	   CHECK IF WE ARE ON THE PLANET

	   IND = 0
           LONG_HORIZON = LAMR - PI -.5
	   HORIZON_THETA = THETA
	   HORIZON_X = (2*DSQRT(2.D+00)/PI)*(rep)*(LAMR-LONG_HORIZON)
     1                   *DCOS(HORIZON_THETA)
	   IF ( RX .GT. HORIZON_X ) IND=1

           LONG_HORIZON = LAMR + PI + .5
	   HORIZON_X = (2*DSQRT(2.D+00)/PI)*(rep)*(LAMR-LONG_HORIZON)
     1                   *DCOS(HORIZON_THETA)
	   IF ( RX .LT. HORIZON_X ) IND=1

	   IF ( IND .EQ. 1 ) RETURN

C	   IF WE RETURN HERE, WE ARE OFF THE PLANET

           LONGR = LAMR-PI*RX/( 2*DSQRT(2.D+00)*(rep)*DCOS(THETA))

  	    IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	      CALL AUTHALIC(LATR,QP,EPSILON)		! to geodetic
              temp=THEG(pi,rpp,rep,latr)           ! to geocentric     
              latr=dsign(temp,latr)
  	    END IF

	   LAT = LATR / PIOVER180
           LONG = LONGR / PIOVER180
	   IF (LONG .LT. 0) LONG = LONG + 360
  	   IF ((LONG .GE. 360) .AND. (LONG/360. .GE. 2)) THEN

		IND = 1
C
C		ITS POSSIBLE TO WRAP AROUND THE WORLD SEVERAL
C		TIMES, IN WHICH CASE WE DROP THE TIEPOINT
C
		RETURN

	   END IF 
           LONG = AMOD(LONG,360.)

	END IF

        RETURN
C
C
C	TRANSVERSE MERCATOR
C
C
c DATA array elements used: 1,2,3,7,25,26
1500	CONTINUE

	DELL=LONGR-LAMR
1505	IF(DABS(DELL).LE.PI) GO TO 1510
	IF(DELL.GT.0.0D0) GO TO 1520
	DELL=DELL+2.D0*PI
	GO TO 1505
1520	DELL=DELL-2.D0*PI
	GO TO 1505
1510	CONTINUE
	A = RE
  	IF(RE .NE. RP) THEN				!IF ELLIPSOID
  	    EPSILON = EPSIL(RP,RE)			!ECCENTRICIY
            temp=phig(pi,rp,re,thr0)                    ! to geodetic
            thr0=dsign(temp,thr0)
            rhs=rchi(epsilon,thr0,pi)
            chi=2.d0*(datan(rhs)-pi/4.d0)               ! to conformal
            a=rn(re,rp,thr0)*dcos(thr0)/dcos(chi)       ! conformal radius
            thr0=chi
  	END IF

  	IF(M .EQ. 1) THEN

C	DIRECT

  	    IF (RE .NE. RP) THEN			    !IF ELLIPSOID, USE
               temp=phig(pi,rp,re,latr)                     ! to geodetic
               latr=dsign(temp,latr)
               rhs=rchi(epsilon,latr,pi)
               latr=2.d0*(datan(rhs)-pi/4.d0)               ! to conformal
    	    END IF
	    B=DCOS(LATR)*DSIN(DELL)

	    !LINE,SAMPLE CALCULATION BEGINS
	    IF (LATR.GT.-PI/2.AND.LATR.LT.PI/2.AND.
     *          DELL.GT.-PI/2.AND.DELL.LT.PI/2) THEN
	    	LINE=ZC-(A/F)*(DATAN(DTAN(LATR)/DCOS(DELL))-THR0)
  	    	SAMPLE=XC-(0.5D0)*(A/F)*DLOG((1+B)/(1-B))	
	    ELSE IF (LATR.LE.-PI/2)  THEN
		LINE=ZC-(A/F)*(-PI/2-THR0)
		SAMPLE=XC
	    ELSE IF (LATR.GE.PI/2)  THEN
		LINE=ZC-(A/F)*(PI/2-THR0)
		SAMPLE=XC
	    ELSE IF (DABS(DELL).GT.PI/2.AND.DABS(DELL).LE.PI) THEN
		IF (LATR.GT.-PI/2.AND.LATR.LT.0.D0) THEN
	    		LINE=ZC-(A/F)*(-PI+DATAN(DTAN(LATR)/DCOS(DELL))-THR0)
	   	ELSE IF (LATR.GE.0.D0.AND.LATR.LT.PI/2) THEN
	    		LINE=ZC-(A/F)*(PI+DATAN(DTAN(LATR)/DCOS(DELL))-THR0)
		ENDIF
  	    	SAMPLE=XC-(0.5D0)*(A/F)*DLOG((1+B)/(1-B))
	    ELSE IF (DELL.EQ.-PI/2.OR.DELL.EQ.PI/2) THEN
		     IF (LATR.GE.-0.00001.AND.LATR.LE.0.00001)  THEN
			SAMPLE=999999.9
			LINE=ZC
		     ELSE IF (LATR.LT.0.D0) THEN
			LINE=(ZC-(A/F)*((-PI/2+0.0001)-THR0))
			SAMPLE=XC-(0.5D0)*(A/F)*DLOG((1+B)/(1-B))
		     ELSE
			LINE=ZC-(A/F)*(PI/2-THR0-0.00001)
			SAMPLE=XC-(0.5D0)*(A/F)*DLOG((1+B)/(1-B))
		     ENDIF
	    ENDIF
  	ELSE

C	INVERSE

	    D = (ZC-LINE)*F/A+THR0
	    LATR = DASIN(DSIN(D)/DCOSH((SAMPLE-XC)*F/A))
	    IF ((SAMPLE-XC).EQ.0.D0.AND.(D.LE.-PI/2.OR.D.GE.PI/2)) THEN
		LONGR=0.0
	    ELSE IF (D.GE.-PI/2.AND.D.LE.PI/2) THEN
		LONGR = LAMR-DATAN(DSINH((SAMPLE-XC)*F/A)/DCOS(D))
	    ELSE IF (D.LT.-PI/2.OR.D.GT.PI/2) THEN
		LONGR = LAMR-PI-DATAN(DSINH((SAMPLE-XC)*F/A)/DCOS(D))
	    ENDIF
  	    IF(RE .NE. RP) THEN				!IF ELLIPSOID
               call iter(latr,phi,epsilon,pi)   ! conformal to geodetic
               temp=theg(pi,rpp,rep,phi)
               latr=dsign(temp,phi)             ! to geocentric
  	    END IF

  	    LAT = LATR / PIOVER180
  	    LONG = LONGR / PIOVER180
	    IF(LONG .LT. 0) LONG = LONG + 360
  	    IF(LONG .GE. 360) LONG = LONG - 360

  	END IF
	   
	RETURN

            END


  	SUBROUTINE AUTHALIC(PHI,QP,EPSILON)

C	THIS SUBROUTINE CONVERTS AUTHALIC LATITUDE PHI TO GEODETIC LATITUDE.
C	QP IS QUAT(1,EPSILON), AND EPSILON IS THE ECCENTRICITY OF THE OBLATE
C	SPHEROID.
 
  	IMPLICIT NONE
  	DOUBLE PRECISION PHI,QP,EPSILON
  	DOUBLE PRECISION SIN_BETA,SIN_LAT,DELTA_LAT
        DOUBLE PRECISION SMALL
        DATA SMALL/1D-8/

  		SIN_BETA = DSIN(PHI)
  		PHI = DASIN(QP * SIN_BETA / 2)
  		DELTA_LAT = 1
  		DO WHILE(DABS(DELTA_LAT) .GT. SMALL)	!GET PLANET LAT
	            SIN_LAT = DSIN(PHI)
  		    DELTA_LAT =
     1			((1 - (EPSILON * SIN_LAT) ** 2) ** 2) /
     2  	        (2 * DCOS(PHI)) *
     3		   	(QP * SIN_BETA / (1 - EPSILON ** 2) -
     4		 	 SIN_LAT / (1 - (EPSILON * SIN_LAT) ** 2) +
     5			 1 / (2 * EPSILON) *
     6			  DLOG((1 - EPSILON * SIN_LAT) /
     7			       (1 + EPSILON * SIN_LAT)))	
  		    PHI = PHI + DELTA_LAT
  		END DO
  
  		RETURN
  		END


  	SUBROUTINE OBLIQUE(PHI,LAMBDA,ALPHA,WBETA,WLAMBDA0,MODE)

C	WHEN MODE = 1, THIS SUBROUTINE CONVERTS LATITUDE, PHI, AND W. LONGITUDE,
C  	LAMBDA, TO OBLIQUE COORDINATES IN THE SYSTEM WHERE THE OBLIQUE NORTH
C	POLE IS AT PLANETODETIC LATITUDE, ALPHA, AND LONGITUDE, BETA AND THE
C  	OBLIQUE	MERIDIAN AT LONGITUDE LAMBDA0 COOINCIDES WITH THE MERIDIAN AT
C  	PLANET LONGITUDE BETA.  WHEN MODE = 2, THE INVERSE TRANSFORMATION IS	
C  	PERFORMED.  INTERNALLY, THIS SUBROUTINE USES E. LONGITUDE. 
C	(C.F. USGS BULL. 1532, P. 35)

  	IMPLICIT NONE
  	DOUBLE PRECISION PHI,LAMBDA,ALPHA,BETA,LAMBDA0,WBETA,WLAMBDA0
  	INTEGER MODE
  	DOUBLE PRECISION OPHI,OLAMBDA
  	DOUBLE PRECISION SIN_ALPHA,COS_ALPHA,SIN_PHI,COS_PHI,SIN_DELTA,
     .                   COS_DELTA
        DOUBLE PRECISION PI
        DATA PI/3.141592653589793D0/
        DOUBLE PRECISION SMALL
        DATA SMALL/1D-8/

  	SIN_ALPHA = DSIN(ALPHA)
  	COS_ALPHA = DCOS(ALPHA)
  	BETA = - WBETA
        LAMBDA0= - WLAMBDA0

        IF(MODE .EQ. 1) THEN

C       DIRECT

	    LAMBDA = - LAMBDA  	
	    SIN_PHI = DSIN(PHI)
	    COS_PHI = DCOS(PHI)
	    SIN_DELTA = DSIN(LAMBDA - BETA)
	    COS_DELTA = DCOS(LAMBDA - BETA)

	    OPHI = DASIN(SIN_ALPHA * SIN_PHI +
     1   	         COS_ALPHA * COS_PHI * COS_DELTA)
	    IF(PI/2 - DABS(OPHI) .GT. SMALL) THEN
		OLAMBDA = DATAN2(COS_PHI * SIN_DELTA,
     1   		         SIN_ALPHA * COS_PHI * COS_DELTA -
     2  		         COS_ALPHA * SIN_PHI) 
     3  	            + LAMBDA0
	    ELSE
		OLAMBDA = PI
	    END IF

 	    PHI = OPHI
  	    LAMBDA = - OLAMBDA
	    IF (LAMBDA .LT. 0) LAMBDA = LAMBDA + 2*PI

  	ELSE

C       INVERSE

	    OPHI = PHI
	    OLAMBDA = - LAMBDA

	    SIN_PHI = DSIN(OPHI)
	    COS_PHI = DCOS(OPHI)
	    SIN_DELTA = DSIN(OLAMBDA - LAMBDA0)
	    COS_DELTA = DCOS(OLAMBDA - LAMBDA0)

	    PHI = DASIN(SIN_ALPHA * SIN_PHI -
     1                  COS_ALPHA * COS_PHI * COS_DELTA)
	    IF(PI/2 - DABS(PHI) .GT. SMALL) THEN
		LAMBDA = DATAN2(COS_PHI * SIN_DELTA,
     1			        SIN_ALPHA * COS_PHI * COS_DELTA +
     2  		        COS_ALPHA * SIN_PHI)
     3		           + BETA
	    ELSE
		LAMBDA = 0
	    END IF

	    LAMBDA = - LAMBDA
	    IF (LAMBDA .LT. 0) LAMBDA = LAMBDA + 2*PI

  	ENDIF

        RETURN

  	END

      LOGICAL FUNCTION BOP(SLAT,SLON,RLAT,RLON)
C     29-SEP-1985 JAM LIFTED BOP FROM LUMLLP
C  SLAT,SLON = PIXEL 1 POINT , WEST
C  RLAT,RLON = PIXEL 2 POSITION , WEST
C  ALL VALUES IN RADIANS
C  RETURNS .TRUE. IF POINT PLAT,PLON IS ON BACK OF PLANET W.R.T. SLAT,SLON
      DOUBLE PRECISION SLAT,SLON,RLAT,RLON
      REAL C(10), PI2
      DATA PI2/6.28318530/
      C(1)=COS(SLAT)
      C(2)=COS(PI2-SLON)
      C(3)=SIN(SLAT)
      C(4)=SIN(PI2-SLON)
      CA=COS(RLAT)
      CO=COS(PI2-RLON)
      SA=SIN(RLAT)
      SO=SIN(PI2-RLON)
      CE=CA*CO*C(1)*C(2)+CA*SO*C(1)*C(4)+SA*C(3) ! COSINE EMISSION ANGLE
      BOP=.FALSE.
      IF(CE.LT.0.)BOP=.TRUE.
      RETURN
      END
      
      SUBROUTINE  MOLLWEIDE_ITERATEr ( THETA, PHI )

      DOUBLE PRECISION THETA, PHI

      DOUBLE PRECISION DEL_THETA, ERROR, PI,x1,x2,x3

      PI=3.141592653589793D0
      ERROR = 1.0D-10

10	x1 = dsin(theta)
	x2 = dsin(phi)
	x3 = dcos(theta)
	del_theta = -(theta+x1-pi*x2)/(1.0+x3)
c  (The above 4 lines were added in replacement of the next
c  2 -- commented out -- ones to resolve a problem on the
c  Red Hat Linux system, where the compiler appeared to lose
c  the double precision required and went into an infinite loop
c  for a case where THETA was close to -PI and PHI close to
c  -PI/2.  This was documented in MIPS request #6902.
c  -lwk-  20jan05)
c10      DEL_THETA = - ( THETA + DSIN(THETA) - PI*DSIN(PHI))/
c     +             (1+DCOS(THETA))
        THETA = THETA + DEL_THETA
        if(dabs(del_theta).gt.error)goto 10
c       if(dabs(del_theta)/(dabs(theta)+.01).gt.error) go to 10

      THETA = THETA/2

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ztranv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ztranv - Converts from line,sample to lat,long and 
   the reverse for map projections					*/
/************************************************************************/

void ztranv(ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
            line,sample,lat,lon,rp,re,psi)
int *ind;
int itype, m;
float *line, *sample, *lat, *lon;
double xc,zc,th,th1,th2,lam,f,cas,rp,re,psi;

{
FTN_NAME(tranv)( ind, &itype,&m,&xc,&zc,&th,&th1,&th2,&lam,&f,&cas,
                 line,sample,lat,lon,&rp,&re,&psi);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tranv.imake
/* Imake file for VICAR subroutine TRANV */

#define SUBROUTINE tranv

#define MODULE_LIST tranv.f ztranv.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

/*#define DEBUG		/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */

$ Return
$!#############################################################################
$Test_File:
$ create ttranv.f
      INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	DOUBLE PRECISION XC,ZC,TH,TH1,TH2,LAM,F,CAS,PSI,RP,RE
	REAL LINE,SAMP,LAT,LON

        DO 1000 I= 1,15
         IF( I .EQ. 7 .OR. I .EQ. 8) GOTO 1000
        ITYPE =I
C
	M = 2
	LINE = 500.
	SAMP = 500.
	IF(ITYPE .EQ. 1) THEN 
           CALL XVMESSAGE('POLAR ORTH',' ')
	   XC = 400.0000       
      	ZC = 400.0000       ! This swarm of hard constants comes from the log
   	TH = 90.00000       ! of tsttranv before it was ported to UNIX, when
   	TH1 = 0.0000000D+00 ! it called MAP3 and searcv2.
   	TH2 = 0.0000000D+00
   	LAM = 150.0000       
   	F = 7.000000       
   	CAS = 1.000000      
   	PSI = 0.0000000D+00   
   	RP = 1815.300    
   	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 2) THEN 
           CALL XVMESSAGE('OBLIQ ORTH',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000       
	PSI = 15.23100       
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 3) THEN 
           CALL XVMESSAGE('POLAR STER',' ')
	XC = 400.0000       
	ZC = 400.0000       
	TH = 90.00000      
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 4) THEN 
           CALL XVMESSAGE('OBLIQ STER',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000       
	PSI = 15.23100       
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 5) THEN 
           CALL XVMESSAGE('LAMBERT   ',' ')
	XC = 400.0000       
	ZC = 400.0000       
	TH = 90.00000       
	TH1 = 59.17000       
	TH2 = 35.83000    
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 6) THEN 
           CALL XVMESSAGE('MERCATOR  ',' ')
	XC = 1.000000       
	ZC = 1.000000       
	TH = 65.58600      
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 237.7170       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 9) THEN 
           CALL XVMESSAGE('NORMAL CYL',' ')
	XC = 1082.000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 236.9170       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 10) THEN 
           CALL XVMESSAGE('SIMPLE CYL',' ')
	XC = 1.000000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 237.6500       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 11) THEN 
           CALL XVMESSAGE('OBLIQUE SIMPLE CYL',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 12) THEN 
           CALL XVMESSAGE('SINUSOIDAL',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 13) THEN 
           CALL XVMESSAGE('OBLIQUE SINUSOIDAL',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00   
	TH1 = 40.00000      
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 14) THEN 
           CALL XVMESSAGE('MOLLWEIDE',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 15) THEN 
           CALL XVMESSAGE('TRANS MERC',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
        END IF
	CALL TRANV(IND,ITYPE,m,XC,ZC,TH,TH1,th2,LAM,F,CAS,LINE,SAMP,
     1             LAT,LON,RP,RE,PSI)
	IF(IND .NE. 0) GO TO 998
	CALL PRNT(7,2,LINE,'FROM L,S.')
	CALL PRNT(7,2,LAT,'TO LT,LN.')
	M = 1
	CALL TRANV(IND,ITYPE,m,XC,ZC,TH,TH1,th2,LAM,F,CAS,LINE,SAMP,
     1             LAT,LON,RP,RE,PSI)
	IF(IND .NE. 0) GO TO 998
	CALL PRNT(7,2,LINE,'AND BACK TO L,S.')

c  special check for LAT=90 in Mercator:
	if (itype.eq.6) then
	  lat = 90.
	  lon = 0.
	  call tranv(ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,line,samp,
     1             lat,lon,rp,re,psi)
	  call prnt(7,2,line,'Mercator (90,0) gives L,S=.')
	endif

1000    CONTINUE

      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface: ztranv', ' ')

      call tztranv

	RETURN
998	CALL PRNT(4,1,IND,'TRANV IND = .')
	RETURN
	END
$!-----------------------------------------------------------------------------
$ create tztranv.c
#include "xvmaininc.h"
#include "ftnbridge.h"


void FTN_NAME(tztranv)() 
{
        int    ind, m, itype;
	double xc,zc,th,th1,th2,lam,f,cas,psi,rp,re;
	float  line,samp,lat,lon;
        char pbuf[81];
/*  ==================================================================  */
        zvmessage("TRANS MERC", "");
        itype =15;
	m = 2;
	line = 500.;
	samp = 500.;
	xc = 400.0000;       
	zc = 400.0000;     
	th = 0.0000000;  
	th1 = 0.0000000;  
	th2 = 0.000000;
	lam = 150.0000;       
	f = 7.000000;       
	cas = 1.00000;      
	psi = 0.000000;   
	rp = 1815.300;  
	re = 1830.000;
        ztranv(&ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
               &line,&samp,&lat,&lon,rp,re,psi);
        if (ind != 0) zmabend("Error in ztranv");

        sprintf( pbuf, "FROM L,S  %f   %f", line, samp);
        zvmessage(pbuf, "");
        sprintf( pbuf, "TO LT,LN  %f   %f", lat, lon);
        zvmessage(pbuf, "");
	m = 1;
        ztranv(&ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
               &line,&samp,&lat,&lon,rp,re,psi);
        if (ind != 0) zmabend("Error in ztranv");
        sprintf( pbuf, "AND BACK TO L,S  %f   %f", line, samp);
        zvmessage(pbuf, "");
}
$!-----------------------------------------------------------------------------
$ create ttranv.imake
/* Imake file for Test of VICAR subroutine TRANV */

#define PROGRAM ttranv

#define MODULE_LIST ttranv.f tztranv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define LIB_LOCAL	/* remove on delivery */
$!-----------------------------------------------------------------------------
$ create ttranv.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tsttranv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
ttranv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create tranv.hlp
1 TRANV

Converts from line,sample to lat,long and the reverse for map projections

  FORTRAN Calling Sequence:  CALL TRANV(IND,ITYPE,M,XC,ZC,TH,TH1,TH2,LAM,F,CAS,
                                        LINE,SAMPLE,LAT,LONG,RP,RE,PSI)
  C Calling Sequence:        ztranv(&ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
                                    &line,&sample,&lat,&long,rp,re,psi);

  Arguments: 
	 IND   The returned flag value from the call.  
       ITYPE   Specifies the type of projection being converted.
	   M   Specifies the type of conversion being attempted
	  XC   Special sample point (pixels)
	  ZC   Special line point (pixels)
	  TH   Special latitude point (degrees)
	  TH1  Latitude of special parallel or
  		special oblique longitude (degrees)
	  TH2  Latitude of special parallel (degrees)
	  LAM  Special longitude west (degrees)
	  F    Scale (Km/pixel)
	  CAS  Visible pole flag (+1.D0:North, -1.D0:South)
	  LINE Line value of point (pixels)
	  SAMP Sample value of point (pixels)
	  LAT  Latitude (degrees)
	  LONG Longitude (degrees west)
	  RP   Polar radius (Km)
	  RE   Equatorial Radius (Km)
	  PSI  North angle

2 Arguments

  The only output arguments are IND, and depending on the mode (M), either
  LINE and SAMP, or LAT and LONG.  Accordingly for ztranv, the call should
  have a & for ind, line, samp, lat, and long as shown in the C calling 
  sequence, or they should be declared as pointers: int * for ind, float *
  for the other four.  For ztranv, the other arguments are passed by value.

  IND   I*4 The returned flag value from the call.  TRANV will return zero for
        a normal execution and a one for a point off the planet.
  ITYPE  I*4 Specifies the type of projection being converted.
	1 - Polar Orthopgraphic
	2 - Oblique Orthographic
	3 - Polar Stereographic
	4 - Oblique Stereographic
	5 - Lambert
	6 - Mercator
	9 - Normal Cylindrical
	10 - Simple Cylindrical
  	11 - Oblique Simple Cylindrical
  	12 - Sinusoidal Equal Area
  	13 - Oblique Sinusoidal
        14 - Mollweide
	15 - Transverse Mercator
  M     I*4 Specifies the type of conversion being attempted:
        1) Direct - latitude & longitude to line & sample
        2) Inverse - line & sample to latitude and longitude
  XC   R*8 Special sample point (pixels)
  ZC   R*8 Special line point (pixels)
  TH   R*8 Special latitude point (degrees)
  TH1  R*8 Latitude of special parallel or
  		 special oblique longitude (degrees)
  TH2  R*8 Latitude of special parallel (degrees)
  LAM  R*8 Special longitude west (degrees)
  F    R*8 Scale (Km/pixel)
  CAS  R*8 Visible pole flag (+1.D0:North, -1.D0:South)
  LINE R*4 Line value of point (pixels)
  SAMP R*4 Sample value of point (pixels)
  LAT  R*4 Latitude (degrees)
  LONG R*4 Longitude (degrees west)
  RP   R*8 Polar radius (Km)
  RE   R*8 Equatorial Radius (Km)
  PSI  R*8 North angle

Usage of these arguments for projection types 1-10 are described in the
documentation for program MAP2. 

Type 11 is an oblique simple cylindrical projection of a sphere of radius,
RE, with scale of F at the oblique equator.  The upper axis of the
projection is located at latitude, PHI, and longitude, LAM.  The center of
the projection is TH1 degrees to the left of the projected north pole and
is located at line, ZC, and sample, XC. 

Type 12 is a sinusoidal equal area projection of the authalic sphere, with
scale of F along the equator.  The center of the projection is at
longitude, LAM, and the equator.  The point at latitude, TH, and
longitude, LAM, is located at line, ZC, and sample, XC.  This projection
will run an order of magnitude faster if RE is set equal to RP. 

Type 13 is an oblique sinusoidal equal area projection of the authalic
sphere, with scale of F along the oblique equator.  The upper pole of the
projection is located at latitude, PHI, and longitude, LAM.  The center of
the projection is TH1 degrees to the left of the projected north pole and
is located at line, ZC, and sample, XC.  This projection will run an order
of magnitude faster if RE is set equal to RP. 

2 Restrictions

  RP must be <= RE.

2 History

  Original Programmer: J. J. LORRE (6-16-77)
  Current Cognizant Programmer: J. J. Lorre
  Source Language: FORTRAN
  Ported to UNIX:  Steve Pohorsky
  Revisions:
  05-Jun-1998 RRP  Initialize all uninitialized variables. AR-9644
  11-sep-1995 LWK  add check for |LAT|=90 in Mercator, which causes log(0)
		on Alpha (but not on the VAX!)
  28-JAN-1993 SP   CHANGED CORRECTION of 13 FEB 1986 TO USE 1.D0 INSTEAD OF
                   .99999 SINCE INVALID ARG ONLY COMES FROM LAMBAR>1.D0.
                   CORECTED BOP BY DECLARING ARGS TO BE DOUBLE PRECISION.
  22  JAN 93  SP   Ported to UNIX; Added ztranv for calls from C.
   1  JUL 91  JJL  Changes to: sinusoidal,oblique sinusoidal,mollweide,
                               simple cylindrical,oblique simple cylindrical,
                               transverse mercator.
                   To include: Authalic latitudes and conformal latitudes.
   1  JUN 91  JJL  Test file upgrade
   23 DEC 89  MAG  add mollweide projection
   10 JUN 88  RMB  ADD TYPE 13
   20 MAR 87  RMB  ADD TYPES 11 AND 12
   26 MAR 86  LWK  FIX BUG WHEN M=2 AND LAT/LONG NOT INITIALIZED
   13 FEB 86  JAM  Prevent dacos(1.00) invalid arg to math lib
   25 SEP 85  JAM  FIX BACK OF PLANET TEST FOR POLAR ORTH
   21 JUL 85  JAM  INCLUDE BACK OF PLANET (BOP) TEST FOR ORTH, PORTH
    7 JUL 85  JAM  FIX ERROR IN POLAR STERO WHEN POINT IS ON SAME LINE AS POLE
                   IN INVERSE CASE
   30 JUN 85  JAM  Correct degeneracy in Polar Orth and Lambert in calulating
                   lat and long when point is at "special" point
   30 MAR 85  JAM  FIX DATAN2(0.,0.) ERROR IN POLAR ORTH, LAMBERT CASES.
   25 MAR 84  JAM  FIX DIVIDE BY ZERO IN THEG FOR POLAR STEREO
                   FIX IT TO RESOLVE DEGENERACY WHEN LINE,SAMP ARE AT CENTER OF
                   PROJ FOR POLAR STER.
                   PUT IN CODE TO MAKE COSDEL=0 WHEN SINDEL=1 OR -1
                   PUT IN CODE TO INSURE LONGITUDE RETURNED IN POLAR ORTH IS
                   LESS THAN 360
   17 MAR 84  JAM  PUT IN FIX TO MAKE 4TH ARGUMENT TO PHIG NONZERO TO AVOID
                   ZERO DIVIDE
   21 MAY 83  JAM  CONVERTED TO VAX   
   12 OCT 81  JAM  MADE CHANGE TO CALCULATION OF SAMPLE IN RECT PROJ

$ Return
$!#############################################################################
