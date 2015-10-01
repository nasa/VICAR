$!****************************************************************************
$!
$! Build proc for MIPL module map3
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:24:08
$!
$! Execute by entering:		$ @map3
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module map3 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to map3.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("map3.imake") .nes. ""
$   then
$      vimake map3
$      purge map3.bld
$   else
$      if F$SEARCH("map3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake map3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @map3.bld "STD"
$   else
$      @map3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create map3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack map3.com -mixed -
	-s map3.f map3a.f -
	-i map3.imake -
	-p map3.pdf -
	-t tstmap3.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create map3.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***********************************************************************
C     
C     VICAR PROGRAM MAP3
C
C  Maps an image from image/object space to a standard cartographic plane.
C
C
ccccccccc
C  Revision History
C     1-96   SP  Ported to UNIX, making use of Thuy Truong's updates.
C                Added call to GLLGCOR to Farenc algorithm for Galileo support.
C                This version writes MP labels but computes the transform
C                with CONVEV because MP_XY2LL etc. has not been fully debugged.
C                On most machines nlmax and nsmax can be increased to 
C                accomadate larger images.  Changed to get SCET from
C                GETLABCON if GETSPICE is not called.
C     5-96   SP  Added proper initialization of various variables.  Modified to
C                use GETSPICE2.
C     9-96   SP  Partial modification for GLL summation mode.  Corrections to
C                setDnvalue.
C    10-96   SP  Divided source file into two parts so debugger will work on
C                major part (without ENTRY statements).  Additional changes
C                for summation mode FR 89818.
C    12-96   SP  Corrected handling of North angle from input MP label.
C                Added checks for illegal ATAN2 arguments, and checks for
C                invalid LR & LOR values.  Added error message and abend if
C                 QUAM,TIEPTS, or 'far encounter algorithm' selected when
C                the input file has an MP label.   Added PRINT mode.
C    11-97   SP  Restored equation for RMAG_PERS for perspective projection.
C                (Correction for FR 90527.  Comments at end of map3.f.)
C                Added check for SCALE (CSCALE parameter) not being initialized.
C                Added check for FOCL (FOCL parameter) not being initialized.
C     2-99   rrp Changed USES_C USES_ANSI_C
c   Nov 99  lwk  added some checks for silly values of NL, NS, SCALE, RADIUS;
c		added check for PAR1/2 for Lambert;  fixed problem with
c		Julian date ("Y2K" correction was being done twice);
c		increased parameters ngrid, nsomax
c   Feb 00  lwk  changes to allow program to compile under Linux -- mostly
c		minor, but in one case ROT8 was being passed an array of the
c		wrong type
c   Jan 13  lwk  fixed CHARACTER continuation lines for new compiler flag on Solaris
CCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      subroutine MAIN44


      include 'mp_for_defs'  !needed for MP software

      parameter (ngrid=1000,nlmax=1150,nsmax=1250,nsomax=10000)
c..parameters in setDnValue must agree with above.

      COMMON/C1/D(7),F,RPOL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      COMMON/C2/FOCL,XREF,ZREF,SCALE,CL,CS
      COMMON/C3/MTY,MTD,MTH,MTM,MTS,MTSM,ALTI,SLA,PHAR,VA,LR,
     &          LOR,SRR,SLAT,SLONG,RMAG,ZLAT,ZLONG,NOR,PHEYT,PWIDT
     &          ,VR,C,PIID,ME,RS,RRP5,OM
      COMMON/UNITS/UNIT
      common/ippcox/e,pts  ! real*4 pts(3) real*8 e(9)
      common/zvp/nzvp,zvp(2,1000)

      REAL*8 D,COSLAT,COSLON,SINLAT,SINLON,COSNOR,SINNOR,PIFAC8,
     &       XJD,OM(3,3),RSX(8),e(9),RSVECTOR(3),sbuf8(100)
      real*8 xcoef(4,ngrid),ycoef(4,ngrid),amat1(16),amat2(16)
      real*8 temp1,temp2,temp3,temp4,delta_time, xcor8,zcor8,
     &       real_nli,real_nsi

      REAL*4 A(8),NOR,NORTH,CPHI,CPSI,xlat,SCALE,RF,SBUF(200),
     &FOCL,LIN1,LIN2,P1DF,P2DF,XREF(5),ZREF(5),RS(3),RRP5(3),SLONG,ZLAT,
     &ZLONG,PHAR(5),SLA(5),VA(5),LR(5),LOR(5),CLAT,CLONG,ME(3,3),
     &C(3,3),VR(3),CT(3,3),MTEMP(3,3),RMAG,OSSPTL,OSSPTS,
     &pts(3),b(6),conv(3000),RDATA(40),RPAR(200),SSCPT(2),TIEPOINTS(80)
      real*4 xy(4,ngrid),xylast(4,ngrid),lat_auth,lat_auth2,cvev(4000)
      real*4 lat_conf,lat_conf2,zvp
      REAL   GRIDLAT(ngrid),GRIDLON(ngrid)
      integer*2 dn(nsmax,nlmax),obuf(nsomax) !input image read into dn array.

      INTEGER counts(ngrid)
      INTEGER PRINTBEG,PRINTEND
      INTEGER*4 ISTAT                       !for mp routines
      REAL*8 MP
      INTEGER*4 dnthresh                    !dn threshold.
      INTEGER   IPAR(200),ALTI,PWIDT,PHEYT,JDN,TARGID,FLIGHT,SRR(5),
     &          OMFLAG,MEFLAG,RSFLAG,RAFLAG,LDAS(4),LCAM(4),
     &          UNIT(14),DEF,COUNT,STAT,CAMERA,
     &          FDS,MTY,MTD,MTH,MTM,MTS,MTSM,SEDR,GEOM,
     &          VIKSN(8),VGRSN(8),JPAR(200),PARLST(67),IDATA(40),
     &          ISBUF(100),labdata(100)
      integer*4 nzvp,reftime(6)

      CHARACTER*3600 LABI     ! once used in maplabv2, no longer used really.
      character*5 project
      CHARACTER*80 WARNING
      CHARACTER*12 PLANET
      CHARACTER*16 PIID
      CHARACTER*8 TARGET
      character*4 real_source
      LOGICAL FIRST, PRINT, FIRST1,HEADER
      logical interpolate
      logical provenance

      equivalence (sbuf,sbuf8,isbuf)
      EQUIVALENCE (SINLAT,D(1)),(COSLAT,D(2)),(SINLON,D(3)),
     &(COSLON,D(4)),(SINNOR,D(5)),(COSNOR,D(6)),
     &(IDATA(1),RDATA(1)),(JPAR(1),RPAR(1))!,(IPAR(1),PAR(1))
C   DEFINE KEYWORD POINTERS
      INTEGER*4 NOIN,ILON,ILAT,HALF,ISTE,ILIN,ISAM,ILAM,ILI1,ILI2,
     &IPA1,IPA2,INOL,ISOU,IMER,ISCA,INOR,IORT,IPRINT,IPOL,IRTAS,IDECL,
     &DISTOR,MM71,MVM73,ITARG,QUAM,NOGEOM,PAROM,PARCVC,PARVRV,PARDAS,
     &PARFDS,PARCAM,PARNOR,PARMAG,PARRSV,ICLAT,ICLONG,ISLAT,ISLONG,
     &PARMEM,PARTIM,PARPIX,ICLINE,ICSAMP,PARFCL,FARENC,TIEPT,VI76,SHORT,
     &PARFSC,RECENT,PLAT,PLON,PLIN,PSAM,RADII,LORA,CYLI,MJS77,OSSCPT,
     &ISSCPT,GEODET,NAM2,RECT,IOBCY,ISINU,IOBSI,IMOLL,ITMER,IPERS

C     EQUIVALENCE KEYWORD POINTERS TO POINTER LIST
      EQUIVALENCE
     &   (NOIN,PARLST(1)),(ILON,PARLST(2)),(ILAT,PARLST(3)),
     &   (HALF,PARLST(4)),(ISTE,PARLST(5)),(ILIN,PARLST(6)),
     &   (ISAM,PARLST(7)),(ILAM,PARLST(8)),(ILI1,PARLST(9)),
     &   (ILI2,PARLST(10)),(IPA1,PARLST(11)),(IPA2,PARLST(12)),
     &   (INOL,PARLST(13)),(ISOU,PARLST(14)),(IMER,PARLST(15)),
     &   (ISCA,PARLST(16)),(INOR,PARLST(17)),(IORT,PARLST(18)),
     &   (IPRINT,PARLST(19)),(IPOL,PARLST(20)),(IRTAS,PARLST(21)),
     &   (IDECL,PARLST(22)), (DISTOR,PARLST(23)),(MM71,PARLST(24)), 
     &   (MVM73,PARLST(25)),(ITARG,PARLST(26)), (QUAM,PARLST(27)),
     &   (NOGEOM,PARLST(28)),(PAROM,PARLST(29))
      EQUIVALENCE (PARCVC,PARLST(30)),(PARVRV,PARLST(31)),
     &   (PARDAS,PARLST(32)),(PARFDS,PARLST(33)),(PARCAM,PARLST(34)),
     &   (PARNOR,PARLST(35)),(PARMAG,PARLST(36)),(PARRSV,PARLST(37)),
     &   (ICLAT,PARLST(38)) ,(ICLONG,PARLST(39)),(ISLAT,PARLST(40)),
     &   (ISLONG,PARLST(41)),(PARMEM,PARLST(42)),(PARTIM,PARLST(43)),
     &   (PARPIX,PARLST(44)),(ICLINE,PARLST(45)),(ICSAMP,PARLST(46))
      EQUIVALENCE (PARFCL,PARLST(47)),(FARENC,PARLST(48)),
     &   (TIEPT,PARLST(49)), (VI76,PARLST(50)),(SHORT,PARLST(51)),
     &   (PARFSC,PARLST(52)),(RECENT,PARLST(53)),(PLAT,PARLST(54)),
     &   (PLON,PARLST(55)),(PLIN,PARLST(56)),(PSAM,PARLST(57)),
     &   (RADII,PARLST(58)),(LORA,PARLST(59)),(CYLI,PARLST(60)),
     &   (MJS77,PARLST(61)),(OSSCPT,PARLST(62)),(ISSCPT,PARLST(63)),
     &   (GEODET,PARLST(64)),(NAM2,PARLST(65)),(RECT,PARLST(66))

C     FLIGHT LABEL DESIGNATIONS
      CHARACTER*75 MSC
      CHARACTER*49 MSGRS

      character*40 maptype
      LOGICAL XVPTST,use_zonal_flow
      CHARACTER*12 CPAR

c DEFINE MAP3 FUNCTIONS
      real*8 geocen            
      REAL*4 RADIUS
      integer*4 lampar

c
C DATA INITIALIZATIONS

c   real
      data PAR1/0./, PAR2/0./
c   real*8
      data e/1.,0.,0.,0.,1.,0.,0.,0.,1./
c   integer*4
      DATA TARGID/0/,FLIGHT/0/,OMFLAG/0/,MEFLAG/0/,RSFLAG/0/,RAFLAG/0/,
     &     LDAS/131,86,222,85/,
     &     LCAM/87,152,79,77/,UNIT/14*-999/,CAMERA/0/,
     &     VIKSN/0,0,0,2,5,4,1,3/,VGRSN/0,0,0,4,3,2,1,0/,
     &     PARLST/67*0/
      DATA IMOLL/0/, IOBCY/0/, IOBSI/0/, IPERS/0/,   !Initialize various flags
     &     ISINU/0/, ITMER/0/,    !that are not covered in PARLST EQUIVALENCE.
     &     NFLAG/0/
c   character
      DATA project/'     '/
      data PLANET/'            '/
      data TARGET/'        '/
      DATA MSC/' /CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/'/
      DATA MSGRS/'  XXXXXXXXXXXXXXX YYYYYYYYYYYYYYY ZZZZZZZZZZZZZZZ'/


C         INITIALIZATION
      WARNING='*** WARNING: INPUT IMAGE MAY BE OBJECT SPACE AND A GEOM '
     & // 'FILE WAS INPUT'
      call XVMESSAGE('Map3 version 10-Jan-2013',' ')
      CALL INIT_SPICE
      PI=3.141592654
      PIFAC=PI/180.
      SLAT=0.0
      ZLAT=0.0
      LR(3)=0.0

      PRINT = .FALSE.
      CALL XVPARM('PRINT',JPAR,COUNT,DEF,0)
      IF(COUNT.EQ.2)THEN   
         PRINT = .TRUE.
         PRINTBEG = JPAR(1)     ! PRINT INFO FOR A RANGE OF OUTPUT LINES.
         PRINTEND = JPAR(2)
         IF (PRINTBEG .GT. PRINTEND)
     .       CALL MABEND('ERROR: PRINT BEGIN GT PRINT END.')
      ENDIF

      use_zonal_flow = .false.

      if(xvptst('NOINTERP')) then
         interpolate=.false.
         dnthresh = 0
      else
         call xvparm('DNTHRESH',dnthresh,count,def,1)
         interpolate=.true.
      endif

      IF(XVPTST('GEODET'))THEN
         GEODET=1
      ELSE
         GEODET=0

      ENDIF
      IF(XVPTST('HALF'))THEN
         HALF=1
      ELSE
         HALF=0
      ENDIF

      IF(XVPTST('BYTE'))THEN
         HALF=0
      ENDIF

      IF(XVPTST('HALF').AND.XVPTST('BYTE'))THEN
           CALL MABEND('CAN''T SPECIFY BOTH HALF AND BYTE,ABEND')
      ENDIF

      IF(XVPTST('NOLABEL'))THEN
        INOL=1
      ELSE
        INOL=0
      ENDIF

      IF(XVPTST('SHORT'))THEN
          SHORT=1
      ELSE
          SHORT=0
      ENDIF

      IF(XVPTST('NOGEOM'))THEN
          NOGEOM=1
      ELSE
          NOGEOM=0
      ENDIF

      IF(XVPTST('RECENTER'))THEN
          RECENT=1
      ELSE
          RECENT=0
      ENDIF

      P1DF=59.17
      P2DF=35.83

      CALL XVPARM('PAR1',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN   
          PAR1=RPAR(1)
          IPA1=1
      ENDIF

      CALL XVPARM('PAR2',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          PAR2=RPAR(1)
          IPA2=1
      ENDIF

      NORTH=0.

      CPHI=90.
      CALL XVPARM('LATITUDE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          CPHI=RPAR(1)
          ILAT=1
      ENDIF
      CPSI=0.
      CALL XVPARM('LONGITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
           CPSI=RPAR(1)
           ILON=1
      ENDIF
      RF=80.
      RLORA=0.
      XSCALE=0.0
      SCALE=0.0
      FOCL = 0.0

      CALL XVPARM('SCALE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          F=RPAR(1)
          if (f.le.0.0) call mabend(' illegal scale')
          ISCA=1
        ELSE
          F=1.
      ENDIF
c     FLAG=9999.99
      FLAG=1.0e+15
      SEDR=0
      GEOM=0

      do i=1,5
        lr(i)=flag
        lor(i)=flag
      enddo

      CALL XVPCNT('INP',IPAR(7))
      IF(IPAR(7).GT.0)NI=IPAR(7)
      CALL XVPCNT('OUT',IPAR(8))
      IF(IPAR(8).GT.0)NOUTDS=IPAR(8)

      DO I=1,NI
          CALL XVUNIT(UNIT(I+1),'INP',I,STAT,' ')
      ENDDO
      DO I=1,NOUTDS
          J=I
          IF(J.GT.1)J=I+11
          CALL XVUNIT(UNIT(J),'OUT',I,STAT,' ')
      ENDDO
      CALL XVOPEN(UNIT(2),STAT,'U_FORMAT','HALF','OPEN_ACT','AS',
     .             'IO_ACT', 'SA', ' ')
      CALL XVSIZE(IPAR(1),IPAR(2),IPAR(3),IPAR(4),IPAR(5),IPAR(6))
      NL=IPAR(3)
      NS=IPAR(4)
      NLI=IPAR(5)
      NSI=IPAR(6)
      CALL XVPARM('NL',I,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0) NL=I
      CALL XVPARM('NS',I,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0) NS=I

      inc=10

      ind=0
      if(ns/inc.ge.ngrid)then
         call prnt(4,1,ngrid,'# NGRID storage exceeded,limit=.')
         call XVMESSAGE('ngrid=#output pixels/grid size of 10',' ')
         ind=1
      endif
      if(ns.ge.nsomax)then
         call prnt(4,1,nsomax,'# NSOMAX storage exceeded,limit=.')
         call XVMESSAGE('nsomax=max # output pixels permitted',' ')
         ind=1
      endif
      if(nli.gt.nlmax) call prnt(4,1,nlmax,'input truncated to nl=.')
      if(nsi.gt.nsmax) call prnt(4,1,nsmax,'input truncated to ns=.')
      if(ind.eq.1) call abend

       IF(XVPTST('NOSEDR'))THEN
          SEDR=0
       ELSE
          SEDR=1
C          CALL XVMESSAGE('SEDR PROVIDED VIA DISK',' ')
       ENDIF

      !USER PARAMETER PROCESSING
      ILOC=2
      !READ PICTURE LABELS AND SAVE

C        DETERMINE WHETHER INPUT PICTURE IS IMAGE SPACE OR OBJECT SPACE
C        FIRST READ THE LABEL, 
C        THEN LOOK FOR GEOMA FILE, 
C        THEN CHECK USER PARAMETERS

      mpflag = 0                    ! initialize to no MP label.
      call mp_init( mp,istat)
      if(istat.ne.mp_success) call mabend('error in mp_init')
      call mp_label_read( mp, unit(2), istat)
      if(istat.eq.mp_success) then
         CALL MP_GET_VALUE_STR(MP,'MAP_PROJECTION_TYPE',MAPTYPE,STAT)
         IF(STAT.EQ.0) THEN
            mpflag = 1                 ! set flag if MP label found.
            if(maptype(1:10).eq.'POINT_PERS')then !map3 perspective label
               call xvmessage(
     +             'Label specifies map3 perspective input mode',' ')
               distor=0           ! object space
               omflag=1           ! ommatrix provided
               rsflag=1           ! rs vector provided
               sedr=0             ! no need of sedr
               raflag=1

               call mp_mpo2buf( mp, rdata, istat)    !C MP
               if (istat.lt.mp_SUCCESS) 
     .             call mabend(' error in MP_MPO2BUF')
               call mve(8,9,idata(1),om,1,1)
               call mve(-9,3,idata(19),rs,1,1)
               ra=rdata(26)
               rb=ra
               rc=rdata(25)
               req=(ra+rb)/2.0
               rpol=rc
               focl=rdata(27)
               scale=rdata(30)
               rmag=rdata(38)
               cl=rdata(28)
               cs=rdata(29)
               icline=1
               icsamp=1
               slat=rdata(31)
               slong=rdata(32)

C     ANGLE OF NORTH IN MP LABEL IS EXPRESSED 
C     AS DEGREES CLOCKWISE FROM UP.  CHANGE TO REFLECT SEDR
C     CONVENTIONS (DEGREES CLOCKWISE FROM INCREASING SAMPLE)
               NOR=AMOD(RDATA(35)+270.,360.)
               NFLAG=1
            else
               call mabend(
     &            'Error:INPUT IS A MAP PROJECTION. See MAPTRAN')
            endif
         ELSE
            call mabend('MAP3: mp_get_value_str error')
         ENDIF
      elseif(istat.eq.MP_NO_MAP_LABELS) then !old-style VICAR labels
         CALL SEARC_DISTOR(UNIT(2),IND)
         IF(IND.EQ.0)THEN
            DISTOR=1
            CALL XVMESSAGE(
     &             ' LABEL SAYS INPUT PICTURE IS IMAGE SPACE',' ')
         ELSE IF(IND.EQ.1)THEN
            DISTOR=0
            CALL XVMESSAGE(
     &              ' LABEL SAYS INPUT PICTURE IS OBJECT SPACE',' ')
         ELSE
            CALL MABEND('MAP3: searc_distor error')
         ENDIF
      else
         CALL MABEND('MAP3: mp_label_read error')
      endif


      IF(XVPTST('DISTOR').OR.XVPTST('IMAGE'))DISTOR=1
      IF(NI.GT.1)THEN
C           DETERMINE IF GEOMA FILES WAS INPUT
            GEOM=2
            IF(DISTOR.EQ.0)CALL XVMESSAGE(WARNING,' ')
            CALL XVMESSAGE('GEOM FILE PROVIDED',' ')
            DISTOR=1
      ENDIF
      IF(XVPTST('OBJECT'))DISTOR=0
      IF( (XVPTST('OBJECT').AND.XVPTST('IMAGE')) .OR.
     &    (XVPTST('OBJECT').AND.XVPTST('DISTOR')))
     &      CALL MABEND('IMAGE AND OBJECT ARE MUTUALLY EXCLUSIVE')
      IF(NI.GT.1.AND.DISTOR.EQ.0)THEN
           CALL XVMESSAGE(
     & 'WARNING: INPUT IMAGE IS OBJECT SPACE AND GEOM FILE WAS ENTERED',
     & ' ')
      ENDIF

c get the project id
      flight=1
      IF(XVPTST('NOPROJEC'))THEN
        FLIGHT=0
        SEDR=0
        DISTOR=0
      else
        call getproj(unit(2),project,camera,fds,ind)
        if(ind.ne.0)then
          call prnt(4,1,ind,'GETPROJ: bad indicator, ind=.')
        endif
      ENDIF

      IF(XVPTST('MM71')) project='MAR-9'
      IF(XVPTST('MVM73'))project='MAR10'
      IF(XVPTST('VI76')) project='VIKOR'
      IF(XVPTST('VGR1'))project='VGR-1'  
      IF(XVPTST('VGR2'))project='VGR-2'  
      IF(XVPTST('GLL'))  project='GLL  '  

C        CHECK IF FDS, CAMERA, OR CAMERA SCALE (PIXELS/MM) SPECIFIED

C      IF(PARDAS.NE.0)FDS=IPAR(PARDAS)
      CALL XVPARM('DAS',JPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)FDS=JPAR(1)

C      IF(PARFDS.NE.0)FDS=IPAR(PARFDS)
      CALL XVPARM('FDS',JPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)FDS=JPAR(1)

C      IF(PARFSC.NE.0)FDS=IPAR(PARFSC)
      CALL XVPARM('FSC',JPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)FDS=JPAR(1)

C      IF(PARCAM.NE.0)CAMERA=IPAR(PARCAM)
      CALL XVPARM('CAMERA',JPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CAMERA=JPAR(1)

      CALL XVPARM('PLANET',CPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)PLANET=CPAR

      CALL PRNT(4,1,CAMERA,'    CAMERA=.')
      CALL XVPARM('ISSCPT',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         SSCPT(1)=RPAR(1)
         SSCPT(2)=RPAR(2)
         ISSCPT=1
         FARENC=1
      ENDIF

C...Adjust NLI,NSI for summation mode UDRs.  Should be ok for EDRs.
      if (project .eq. 'GLL  ' .and. camera .eq. 2) then
            NLI=400
            NSI=400
      end if

      CALL XVPARM('OSSCPT',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         SSCPT(1)=RPAR(1)
         SSCPT(2)=RPAR(2)
         OSSCPT=1
         FARENC=1
      ENDIF
      CALL XVPARM('TIEPTS',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)TIEPT=1

      IF(XVPTST('QUAM'))QUAM=1
      if (mpflag .eq. 1) then
         IF (FARENC .EQ. 1)  CALL MABEND(
     .     'ERR: wrong to use Far Enc. Alg. when there is an MP label')
         IF (TIEPT .EQ. 1)  CALL MABEND(
     .     'ERR: wrong to use TIEPOINTS Alg. when there is an MP label')
         IF (QUAM .EQ. 1)  CALL MABEND(
     .     'ERR: wrong to use QUAM Alg. when there is an MP label')
      end if

      CALL XVPARM('OMMATRIX',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)PAROM=1
      CALL XVPARM('RSVECTOR',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)PARRSV=1
      CALL XVPARM('RADII',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
        RADII=1
        IF(COUNT.NE.3)THEN
           CALL MABEND(' YOU MUST ENTER 3 VALUES FOR THE RADII')
        ENDIF
      ENDIF

      mty=0
      mtd=0
      mth=0
      mtm=0
      mts=0
      mtsm=0

c obtain data from the picture label - for routine getspice
      if(project.ne.'    ')then
        call getlabcon(unit(2),project,labdata,ind)
        if(ind.eq.1) call prnt(4,1,ind,'GETLABCON: warning ind=.')
        if(ind.gt.1)then
          call prnt(4,1,ind,'GETLABCON: fatal error, ind=.')
          call abend
        endif
        if(labdata(1).eq.0) 
     .     call XVMESSAGE('GETLABCON: invalid label',' ')
        if(labdata(1).eq.1) 
     .     call XVMESSAGE('ground calibration label',' ')
        if(labdata(1).eq.2) then 
           call XVMESSAGE('flight label',' ')
           if ( labdata(8) .gt. 0 ) then  !get SCET in case not using SPICE
c              mty= labdata( 8) + 1900    !VGR convention had just last 2 digits
               mty= labdata( 8)		! fixed by Y2K changes
               mtd= labdata( 9)
               mth= labdata(10)
               mtm= labdata(11)
               mts= labdata(12)
               mtsm= labdata(13)
           end if
        end if
      endif

c set up the use of the zonal flow model if keyword REFTIME is used.
      call xvparm('REFTIME',reftime,count,DEF,0)
      if(count.gt.0)then
         call XVMESSAGE('Reading zonal velocity profile',' ')
c        read profile...
         call get_zvp
c        compute time difference between input image and reference time
         call time_diff(reftime,labdata(8),delta_time)
         use_zonal_flow = .true.
      endif

c extract the sedr/spice
c

      IF(SEDR.NE.0)THEN 
         IF(FARENC+TIEPT+QUAM+PAROM.NE.0)OMFLAG=-1
         IF(FARENC+TIEPT+QUAM+PARRSV.NE.0)RSFLAG=-1
         IF(RADII+ITARG.NE.0)RAFLAG=-1
		!GET SEDR VARIABLES
         if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))
     +        planet='            '

         provenance = .true.
         call getspice2(unit(2),provenance,sbuf,ind)
         if (ind .ne. 1) then
           call prnt(4,1,ind,'GETSPICE2: fatal error, ind=.')
           call abend
         endif

         call mvlc(isbuf(11),real_source,4)
         call XVMESSAGE('Actual source of SEDR is: '//real_source,' ')
         mty=isbuf(3)
         mtd=isbuf(4)
         mth=isbuf(5)
         mtm=isbuf(6)
         mts=isbuf(7)
         mtsm=isbuf(8)
         slat=sbuf8(30)
         slong=sbuf8(31)
         rmag=sbuf8(27)
         zlat=sbuf8(28)
         zlong=sbuf8(29)
         nor=sbuf8(68)
         NFLAG = 1
         pheyt=sbuf8(83)
         pwidt=sbuf8(82)
         vr(1)=-sbuf8(19)
         vr(2)=-sbuf8(20)
         vr(3)=-sbuf8(21)
         call mve(-9,9,sbuf8(41),c,1,1)
         call mve(-9,9,sbuf8(50),me,1,1)
         meflag=1
         clat=sbuf8(77)
         lr(3)=clat
         clong=sbuf8(78)
         lor(3)=clong
         if(omflag.eq.0)then
           call mve(8,9,sbuf8(59),om,1,1)
           omflag=1
           call XVMESSAGE('OM matrix obtained from SEDR',' ')
         endif
         if(rsflag.eq.0)then
           rs(1)=sbuf8(22)
           rs(2)=sbuf8(23)
           rs(3)=sbuf8(24)
           rsflag=1
           call XVMESSAGE('RS vector obtained from SEDR',' ')
         endif
         if(raflag.eq.0) then
           ra=sbuf8(13)
           rb=sbuf8(14)
           rc=sbuf8(15)
           req=(ra+rb)/2
           rpol=rc
           raflag=1
           call XVMESSAGE('Target radii obtained from SEDR',' ')
         endif
      ENDIF
c      ENDIF   ! come from go to 50

c get planetary constants
      IF(RAFLAG.NE.1.AND.TARGET.NE.'        ')THEN 
         call getplacon(planet,targid,sbuf,ind)
         if(ind.ne.0) call prnt(4,1,ind,'GETPLACON: bad ind=.')
         RA=SBUF(1)
         RB=SBUF(2)
         RC=SBUF(3)
         REQ=(RA+RB)/2.
         RPOL=RC
         RLORA=SBUF(4)
         RAFLAG=1
      ENDIF

C        RADII
      IF(RADII.NE.0)THEN 
         CALL  XVPARM('RADII',RPAR,COUNT,DEF,0)
         IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
           RA=RPAR(1)
           RB=RPAR(2)
           RC=RPAR(3)
	   if (ra.le.0.0 .or. rb.le.0.0 .or. rc.le.0.0)
     1           call mabend(' illegal radius')
         ENDIF
         REQ=(RA+RB)/2.
         RPOL=RC
         RAFLAG=1
      ENDIF

      CALL XVPARM('LORANGLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
           LORA=1
           RLORA=-RPAR(1)
      ENDIF
      IF(RAFLAG.EQ.0)THEN
         CALL XVMESSAGE('NEED RADII',' ')
         CALL ABEND
      ENDIF

C        GET CAMERA CONSTANTS
      if(project.ne.'     ')then
        call getcamcon(project,camera,focl,cl,cs,xscale,ind)
        if(ind.ne.0) call prnt(4,1,ind,'GETCAMCON: bad ind=.')
        scale=xscale
      endif
 
c set 5 reticle points in the input. LR & LOR will be computed
c from these.
      zref(3)=nli/2
      xref(3)=nsi/2
      zref(1)=1.
      xref(1)=1.
      zref(2)=1.
      xref(2)=nsi
      zref(4)=nli
      xref(4)=1.
      zref(5)=nli
      xref(5)=nsi

C     ANGLE OF NORTH IN INPUT FRAME IS EXPRESSED IN PARAMETER
C     LIST AS DEGREES CLOCKWISE FROM UP.  CHANGE TO REFLECT SEDR
C     CONVENTIONS (DEGREES CLOCKWISE FROM INCREASING SAMPLE)
C     NOR=AMOD(PAR(PARNOR)+270.,360.)
      CALL XVPARM('NORANGLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
              NOR=AMOD(RPAR(1)+270.,360.)
              NFLAG=1
      ENDIF

      IF (NFLAG .EQ. 0)  CALL MABEND('ERROR: NORANGLE not specified')

C      IF(PARPIX.GT.0)XSCALE=PAR(PARPIX)
      CALL XVPARM('CSCALE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)then
        XSCALE=RPAR(1)
        scale=xscale
      endif

      if (scale .eq. 0.0)  call mabend(
     .     'Error: CSCALE parameter not entered for NOPROJECt case.')

      CALL XVPARM('RSVECTOR',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         CALL MVE(7,3,RPAR,RS,1,1)
         RSFLAG=1
         RMAG=SQRT(RS(1)*RS(1)+RS(2)*RS(2)+RS(3)*RS(3))
         SLAT=(ASIN(RS(3)/RMAG))*57.2957795
         SLONG=(-ATAN2(RS(2),RS(1)))*57.2957795
      ENDIF

      CALL XVPARM('OMMATRIX',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         DO I=1,3
            DO J=1,3
                OM(I,J)=RPAR(3*(I-1)+J)
            ENDDO
         ENDDO
         OMFLAG=1
      ENDIF

      CALL XVPARM('CMATRIX',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         DO I=1,3
            DO J=1,3
                C(I,J)=RPAR(3*(I-1)+J)
            ENDDO
         ENDDO
      ENDIF

      CALL XVPARM('VRVECTOR',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         DO I=1,3
            VR(I)=RPAR(I)
         ENDDO
      ENDIF

      CALL XVPARM('MEMATRIX',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         DO I=1,3
            DO J=1,3
                ME(I,J)=RPAR(3*(I-1)+J)
            ENDDO
         ENDDO
         MEFLAG=1
      ENDIF

      CALL XVPARM('TIME',JPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         MTY=JPAR(1)
         MTD=JPAR(2)
         MTH=JPAR(3)
         MTM=JPAR(4)
         MTS=JPAR(5)
         MTSM=JPAR(6)
C        USE NATIONAL RADIO ASTRONOMY OBSERVATORY ALGORITHM TO
C        COMPUTE JULIAN DATE.
      ENDIF
      JDN=1721060-(4-MOD(MTY+0,4))/4+(100-MOD(MTY+0,100))/100
     &-(400-MOD(MTY+0,400))/400+MTY*365+MTY/4-MTY/100+MTY/400+MTD
      XJD=DFLOAT(JDN)-0.5D0+(MTH+(MTM+(MTS+MTSM/1000.)/60.)/60.)/24.D0
C        XJD IS CORRECTED FOR LEAP YEARS
      CALL PRNT(8,1,XJD,' JULIAN DATE OF EVENT FRAME=.')

      CALL XVPARM('SLATITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          SLAT=RPAR(1)
          ISLAT=1
      ENDIF
      CALL XVPARM('SLONGITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          SLONG=RPAR(1)
          ISLONG=1
      ENDIF
      CALL XVPARM('CLATITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          ICLAT=1
          CLAT=RPAR(1)
      ENDIF
      CALL XVPARM('CLONGITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          CLONG=RPAR(1)
          ICLONG=1
      ENDIF
C  CONVERT GEODETIC LATITUDES TO GEOCENTRIC LATITUDES
      PIFAC8 =3.141592653589793D0/180.D0
      IF(GEODET.NE.0)THEN  
           IF(ICLAT.NE.0)CLAT=GEOCEN(CLAT*PIFAC8)/PIFAC8
           IF(ISLAT.NE.0)SLAT=GEOCEN(SLAT*PIFAC8)/PIFAC8
      ENDIF
      CALL XVPARM('RMAGNITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         RMAG=RPAR(1)
         PARMAG=1
      ENDIF

C      IF(PARFCL.GT.0)FOCL=PAR(PARFCL)
      CALL XVPARM('FOCL',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)FOCL=RPAR(1)
      if (FOCL .eq. 0.0)  call mabend(
     .     'Error: FOCL parameter not entered for NOPROJECt case.')

      CALL XVPARM('CLINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          CL=RPAR(1)
          ICLINE=1
      ENDIF
      CALL XVPARM('CSAMP',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          CS=RPAR(1)
          ICSAMP=1
      ENDIF
      IF(ICLINE.EQ.0.AND.FLIGHT.EQ.0)CL=FLOAT(NLI)/2.
      IF(ICSAMP.EQ.0.AND.FLIGHT.EQ.0)CS=FLOAT(NSI)/2.

      NORTH=NOR+90.
      NORTH=AMOD(NORTH,360.)

      IF(PAR1.LT.PAR2.and.ilam.eq.1)THEN
         CALL XVMESSAGE('WARNING-PAR1 IS LESS THAN PAR2',' ')
         CALL XVMESSAGE('MAP3 WILL SWITCH THE VALUES',' ')
         RTEMP=PAR1
         PAR1=PAR2
         PAR2=RTEMP
      ENDIF

      IF(XVPTST('MERCATOR'))IMER=1
      IF(XVPTST('LAMBERT'))ILAM=1
      IF(XVPTST('STEREOGR'))ISTE=1
      IF(XVPTST('RECTANGU'))RECT=1
      IF(XVPTST('ORTHOGRA'))IORT=1
      IF(XVPTST('CYLINDRI'))CYLI=1
      IF(XVPTST('OBCYLIND'))IOBCY=1
      IF(XVPTST('SINUSOID'))ISINU=1
      IF(XVPTST('OBSINUSO'))IOBSI=1
      IF(XVPTST('MOLLWEID'))IMOLL=1
      IF(XVPTST('TMERCATO'))ITMER=1
      IF(XVPTST('PERSPECT'))IPERS=1

      IF(IMER+ILAM+IORT+ISTE+CYLI+RECT+IOBCY+ISINU+IOBSI+
     +   IMOLL+ITMER+IPERS .GT.1)THEN
          CALL XVMESSAGE(
     .      'MORE THAN ONE PROJECT TYPE WAS SPECIFIED,ABEND',' ')
          CALL ABEND
      ENDIF

      IF(IMER+ILAM+IORT+ISTE+CYLI+RECT+IOBCY+ISINU+IOBSI+
     +   IMOLL+ITMER+IPERS .EQ.0)THEN
         IF(SEDR.EQ.0.AND.ICLAT.EQ.0)THEN
C           DEFAULT TO OBLIQUE ORTHOGRAPHIC
            IORT=1
            GO TO 770
         ENDIF
C        USE CENTER RETICLE LATITUDE TO FIND DEFAULT PROJECTION
         TF=ABS(LR(3))
         IF(TF .LT. 30.)THEN
              IMER=1
         ELSE IF(TF.GE.30. .AND. TF.LT.65.)THEN
              ILAM=1
         ELSE IF(TF .GE. 65.)THEN
            IPOL=1
            ISTE=1
         ENDIF
      ENDIF

770   PRFL=FOCL * SCALE
      CALL XVMESSAGE(MSC(2:72),' ')
      WRITE (MSC(3:11),'(F9.1)') PRFL
      WRITE (MSC(13:21),'(F9.3)') RA
      WRITE (MSC(43:51),'(F9.1)') RMAG
      WRITE (MSC(53:61),'(F9.3)') SLAT
      WRITE (MSC(63:71),'(F9.3)') SLONG
      WRITE (MSC(23:31),'(F9.3)') RB
      WRITE (MSC(33:41),'(F9.3)') RC
      CALL XVMESSAGE(MSC(2:72),' ')
      IF(DISTOR.EQ.1)THEN
        IF (PROJECT .EQ. 'GLL ')  THEN
          CALL MVCL( 'GLL  ', CONV(1), 5)
          CALL MVE( 4,1, CAMERA,CONV(3),1,1)
          ISTART=1
        ELSE IF (GEOM .NE. 0) THEN
          call iread_tiepoints(unit(geom+1),nah,nav,3000,CONV,4)
          ISTART=1
        ELSE     ! NO FILE SPECIFIED: USE NOMINALS
          call getgeom(0,project,camera,0,conv,
     +                 conv,nah,nav,ind)
          if(ind.ne.0)call MABEND('GETGEOM: bad indicator')

          ISTART=9    ! geomav starts VGR nominal tiepoints in conv(9)
        END IF        ! MM71A, VOSN7, ... too
      ENDIF

      IF(TIEPT.NE.0)THEN 
C           *** TIEPOINTS ALGORITHM ***
C        THIS ALGORITHM USES A SERIES OF TIEPOINTS
C        (LINE,SAMPLE,LATITUDE,LONGITUDE) IN THE INPUT FRAME
C        TO GENERATE A LEAST SQUARES APPROXIMATION TO
C        THE 'OM' (TARGET TO CAMERA) TRANSFORMATION MATRIX
         CALL XVPARM('TIEPTS',RPAR,COUNT,DEF,0)
         IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
            TIEPT=1
            CALL MVE(7,COUNT,RPAR,TIEPOINTS,1,1)
            NTIEPT=COUNT/4
            CALL PRNT(4,1,NTIEPT,' # TIEPOINTS=.')
            CALL PRNT(4,1,COUNT,' # VALUES=.')
            IF(MOD(COUNT,4).ne.0)THEN
                CALL XVMESSAGE('# OF VALUES NOT A MULTIPLE OF 4',' ')
                call abend
            ENDIF
         ENDIF
         IF(DISTOR.EQ.0)then
               CALL TIECHV(IND,0,TIEPOINTS,NTIEPT,CONV)
         ELSE
                 ! "1" CONVERTS I.S. TIEPOINTS TO O.S. 
               CALL TIECHV(IND,1,TIEPOINTS,NTIEPT,CONV)
         ENDIF
         IF(IND.NE.0)THEN
             CALL PRNT(4,1,IND,' TIECHV ERROR,IND=.',IND)
             CALL ABEND
         ENDIF
         IF(PARRSV.EQ.0)CALL SPHREC(RS,RMAG,SLAT*PIFAC,-SLONG*PIFAC)
         B(1)=PRFL
         B(2)=REQ-RPOL
         B(3)=RMAG
         B(4)=SLAT
         B(5)=SLONG
         B(6)=REQ
         CALL FOMCLV(IND,NTIEPT,TIEPOINTS,B,OM,RSVECTOR,CL,CS)
         IF(IND.NE.0)CALL MABEND('FOMCLV: ERROR,ABEND')
         call mve(-9,3,rsvector,rs,1,1)
         GO TO 1640
      ENDIF

      IF(FARENC.NE.0)THEN 

C           *** FARENCOUNTER ALGORITHM ***

C        THIS IS DENIS ELLIOTT'S ALGORITHM TO
C        CALCULATE THE OM MATRIX OF A FULL DISK FAR ENCOUNTER FRAME.

         CALL XVPARM('NAM2',RPAR,COUNT,DEF,0)
         IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
               NAM2=1
         ELSE
               NAM2=0
         ENDIF
         IF(NAM2.NE.0)THEN
C              NORTH ANGLE IS IN MAP3 FORMAT (NOT FROM A SEDR)
               NAM2=0
         ELSE
C              NORTH ANGLE IS FROM A SEDR
               NAM2=1
         ENDIF
         IF(DISTOR.EQ.1)THEN
               IF(ISSCPT.EQ.0.AND.OSSCPT.EQ.0)THEN
                    CALL MABEND('NEED SUBSPACECRAFT LINE AND SAMPLE')
               ENDIF
               IF(OSSCPT.EQ.0)THEN
                 IF (PROJECT .EQ. 'GLL') THEN
                   CALL GLLGCOR(IND,SSCPT(1),SSCPT(2),OSSPTL,OSSPTS,1,
     *                          CAMERA)
                   SSCPT(1) = OSSPTL
                   SSCPT(2) = OSSPTS
                 ELSE
                   CALL TRITRA(IND,CONV(ISTART),NAH+1,NAV+1,SSCPT(1),
     *                  SSCPT(2),1)
                   IF(IND.NE.0)THEN
                       CALL XVMESSAGE('TRITRA ERROR IN FARENC MODE',' ')
                       CALL ABEND
                   ENDIF
                 END IF
               ENDIF
         ENDIF
         IF(ISSCPT.NE.0)THEN
              OSSPTL=SSCPT(1)
              OSSPTS=SSCPT(2)
         ENDIF
         IF(OSSCPT.NE.0)THEN
              OSSPTL=SSCPT(1)
              OSSPTS=SSCPT(2)
         ENDIF
         CALL MOMAT(DBLE(CL),DBLE(CS),DBLE(OSSPTL),DBLE(OSSPTS),
     *   DBLE(SCALE),DBLE(FOCL),DBLE(SLONG),DBLE(SLAT),DBLE(AMOD(NOR+
     #   90.,360.)),DBLE(RMAG),OM,RSX,NAM2)
         CALL MVE(-9,3,RSX,RS,1,1)

         CALL XVMESSAGE(
     .       'OM MATRIX COMPUTED USING FARENCOUNTER ALGOR.',' ')
         GO TO 1640
      ENDIF
      IF(QUAM.NE.0)THEN 

C           *** QUAM ALGORITHM ***

C        THIS ALGORITHM USES THE TARGET TO SPACECRAFT VECTOR (RS) AND
C        THE RADIUS VECTOR TO INTERSECTION OF LINE OF SIGHT TO THE TARGET
C        (RRP5).

C          CALCULATE TARGET RADIUS AT LATITUDE LR(3)
C            JAM 7-JUL-1985 set f=1. to make it agree with old map2 
c            it doesn't really make much difference in the OM matrix
                 f=1.
         RADI=RADIUS(LR(3)*PIFAC)*F

C           CALCULATE RS VECTOR
         IF(PARRSV.EQ.0)CALL SPHREC(RS,RMAG,SLAT*PIFAC,-SLONG*PIFAC)

C        CALCULATE RRP5 VECTOR
         CALL SPHREC(RRP5,RADI,LR(3)*PIFAC,-LOR(3)*PIFAC)

C        CALCULATE OM MATRIX
         CALL PEGCAL(LR(3)*PIFAC,-LOR(3)*PIFAC,RS,RRP5,NOR*PIFAC,OM)
         CALL XVMESSAGE('OM MATRIX COMPUTED USING QUAM ALGOR.',' ')
C          set f back to user specified value
         if(isca.ne.0)then
           call xvparm('SCALE',rpar,count,DEF,0)
           f=rpar(1)
         endif
         GO TO 1640

C        *** DEFAULT ALGORITHM ***
      ENDIF
      IF(OMFLAG.NE.1)THEN 

C           POGASIS ALGORITHM TO GET ME MATRIX
         CALL XVPARM('RTAS',RPAR,COUNT,DEF,0)
         IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
               A(1)=RPAR(1)
               IRTAS=1
         ENDIF
C         IF(IRTAS.NE.0)A(1)=PAR(IRTAS)
C         IF(IDECL.NE.0)A(2)=PAR(IDECL)
         CALL XVPARM('DECLINAT',RPAR,COUNT,DEF,0)
         IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
               A(2)=RPAR(1)
         ENDIF
         IF(MEFLAG.EQ.0)CALL MATRXI(XJD,ME,IRTAS,A)

C           COMPUTE OM MATRIX USING ME AND C MATRICES
         CALL mTRANS(C,CT)
         CALL MMUL(CT,ME,MTEMP)
         DO 463 J=1,3
            DO 463 K=1,3
463           OM(J,K)=MTEMP(J,K)
      ENDIF

      IF(RSFLAG.NE.1)THEN ! GO TO 1640
         CALL mTRANS(ME,MTEMP)
         CALL VMUL(VR,MTEMP,RS)
         IF(project.eq.'VIKOR')
     +      CALL SPHREC(RS,RMAG,SLAT*PIFAC,-SLONG*PIFAC)
      ENDIF
C  *****************************************************
1640  CALL XVMESSAGE('    OM MATRIX',' ')
      DO I=1,3
         DO J=1,3
            WRITE (MSC(J*10+1-8:J*10+1),'(F9.6)') OM(I,J)
         ENDDO
         CALL XVMESSAGE(MSC(2:31),' ')
      ENDDO
      CALL XVMESSAGE('    RS VECTOR (TARGET COORDINATES)',' ')
      DO I=1,3
          WRITE (MSGRS(I*16+1-14:I*16+1),'(F15.1)') RS(I)
      ENDDO
      CALL XVMESSAGE(MSGRS(2:49),' ')

      A(1)=RA  ! requator
      A(2)=RB  ! requator
      A(3)=RC  ! rpole
      A(4)=PRFL
      A(5)=RMAG
      A(6)=RLORA
       pts(1)=ra
       pts(2)=a(4)
       pts(3)=a(5)
       e(9)=a(1)/a(3)
c         print *,' *** rpol=',rpol,' req=',req,' ****'
       CALL MVE(9,3,RS,RSVECTOR,1,1)! IPPCOR EXPECTS RS AS REAL*8
      
       ind=0
         IF(project.eq.'MAR-9')THEN
              CALL LBL71(INOL,LR,LOR,ZREF,XREF,A,RS,OM,CL,CS,ALTI,PWIDT,
     &                   PHEYT,VA(3),SLA(3),PHAR(3),PIID,RSVECTOR)
         ELSE IF(project.eq.'MAR10')THEN
              CALL LBL73(INOL,LR,LOR,ZREF,XREF,A,RS,OM,CL,CS,
     &                   ALTI,PWIDT,PHEYT,VA(3),SLA(3),PHAR(3),RSVECTOR)
         ELSE IF(project.eq.'VIKOR')THEN
              CALL LBL76(INOL,LR,LOR,A,RS,OM,ALTI,PWIDT,
     &                  PHEYT,VA(3),SLA(3),PHAR(3),RSVECTOR)
         ELSE IF((project.eq.'VGR-1').or.(project.eq.'VGR-2'))THEN
              CALL LBL79(INOL,LR,LOR,A,RS,OM,ALTI,PWIDT,
     &                   PHEYT,VA(3),SLA(3),PHAR(3),RSVECTOR)
         ELSE IF(project.eq.'GLL  ')THEN
              CALL LBL79(INOL,LR,LOR,A,RS,OM,ALTI,PWIDT,
     &                   PHEYT,VA(3),SLA(3),PHAR(3),RSVECTOR)
         else
              CALL LBL0(LR,LOR,ZREF,XREF,A,RS,OM,CL,CS,NLI,NSI,RSVECTOR)
      ENDIF

c compute center of projection ( clat & clong )
      if(iclat.eq.0.and.iclong.eq.0)then
        CALL DISTAL(LR,LOR,CLAT,CLONG,IND,slat,slong)
        IF(IND.NE.0)THEN 
           IF(ILON.EQ.0.OR.ILAT.EQ.0.OR.ISCA.EQ.0)THEN
                CALL XVMESSAGE('CANNOT FIND TARGET',' ')
                CALL ABEND
           ENDIF
        ENDIF
      endif

      CPHI=CLAT
      CPSI=CLONG
      APHI=CLAT
      APSI=CLONG
      XC=(1+NS)/2
      ZC=(1+NL)/2
      XARB=XC
      ZARB=ZC
      CALL XVPARM('LINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
           ZC=RPAR(1)
           ILIN=1
      ENDIF
       
      CALL XVPARM('SAMPLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
           XC=RPAR(1)
           ISAM=1
      ENDIF

      CALL XVPARM('LATITUDE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPHI=RPAR(1)

      CALL XVPARM('LONGITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPSI=RPAR(1)

      CALL XVPARM('PLATITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         APHI=RPAR(1)
         PLAT=1
      ENDIF

      CALL XVPARM('PLONGITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
         APSI=RPAR(1)
         PLON=1
      ENDIF

      CALL XVPARM('PLINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
           ZARB=RPAR(1)
           PLIN=1
      ENDIF

      CALL XVPARM('PSAMPLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          XARB=RPAR(1)
          PSAM=1
      ENDIF

      IF(XVPTST('POLE'))IPOL=1
C     COMPUTE DEFAULT NORTH ANGLE FOR OBLIQUE ORTH & STER PROJECTIONS
C     THIS IS DONE BY COMPUTING LINE & SAMP IN OUTPUT OF A POINT
C     ON SAME LINE AS C.P. IN INPUT, THEN COMPUTING NORTH TO FORCE IT
C     IT TO LIE ON SAME LINE AS C.P. IN OUTPUT AS WELL

      CALL XVPARM('NORTH',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          INOR=1
          NORTH=RPAR(1)
      ENDIF
      IF(INOR.NE.0)GO TO 794
      SINLAT=SIN(CPHI*PIFAC)
      COSLAT=COS(CPHI*PIFAC)
      call corcav(ind,cphi,-cpsi,om,rsvector,prfl,rb,rb-rc,zcor,xcor,
     &cl,cs,flag)
      IF(IND.NE.0)GO TO 794
      XCOR=XCOR-NSI/2.

       call ippcov(rlat,rlon,zcor,xcor,pts,rsvector,om,e,cl,cs,flag)

C     IF RETICLE 3 OFF TARGET, USE SEDR VALUE OF NORTH
      IF(RLAT.NE.FLAG)THEN    
         RLON=-RLON
         DLAM=RLON-CPSI*PIFAC
         SINLAM=SIN(DLAM)
         COSLAM=COS(DLAM)
         XCOR=-COS(RLAT)*SINLAM
         ZCOR=SINLAT*COS(RLAT)*COSLAM-COSLAT*SIN(RLAT)
         NORTH=ATAN2(XCOR,ZCOR)/PIFAC
         NORTH=AMOD(450.+NORTH,360.)

      ENDIF

c  do some elementary checks 
794   if (nl.lt.1 .or. ns.lt.1) call mabend(' illegal output size')

c This branch sets up each projection 
      IL=1
      IS=1
      IF(IPOL.NE.0)CPHI=SIGN(90.,CLAT)
      IF(CPHI.NE.0.)CAS=SIGN(1.,CPHI)
      IF(ISTE.NE.0)L=4
      IF(ISTE.NE.0.AND.ABS(CPHI).EQ.90.)L=3
      IF(ILAM.NE.0)L=5
      IF(IORT.NE.0)L=2
      IF(IORT.NE.0.AND.ABS(CPHI).EQ.90.)L=1
      IF(IMER.NE.0)L=6
      IF(CYLI.NE.0)L=9
      IF(RECT.NE.0)L=10
      IF(IOBCY.NE.0)L=11
      IF(ISINU.NE.0)L=12
      IF(IOBSI.NE.0)L=13
      IF(IMOLL.NE.0)L=14
      IF(ITMER.NE.0)L=15
      IF(IPERS.NE.0)L=16

      GO TO (105,110,120,125,115,130,1350,1350,1300,1500,
     +       1600,1700,1800,1900,2000,2100),L
1350  call prnt(4,1,L,'Unsupported projection type.')
      call abend      


C   *** POLAR ORTHOGRAPHIC ***
C     PRESERVE ORIENTATION OF INPUT FRAME
C     FORCE OUTPUT LINE OF RETICLE 1 TO EQUAL OUTPUT LINE OF RETICLE 2
C     SPHERICAL EQUATIONS USED FOR SIMPLICITY
105   IF(ILON.NE.0)GO TO 106

      IF (LR(1) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
      IF (LR(2) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
      IF (LR(4) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
      IF (LOR(1) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
      IF (LOR(2) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
      IF (LOR(4) .EQ. flag) CALL MABEND('ERROR; Specify LONG')

      CALL XVMESSAGE('IF ATAN2 ABEND, SPECIFY LONG',' ')
      DLAM=(LOR(2)-LOR(1))*PIFAC
      COSLAM=COS(DLAM)
      SINLAM=SIN(DLAM)
      COS1=COS(LR(1)*PIFAC)
      COS2=COS(LR(2)*PIFAC)
      IF (COS1 - COS2*COSLAM .EQ. 0. .AND. COS2*SINLAM .EQ. 0.)
     .   CALL MABEND('ATAN2 ERROR; Specify LONG')
      DLAM=ATAN2(COS1-COS2*COSLAM,COS2*SINLAM)
      CPSI=DLAM+LOR(1)*PIFAC
C     NOW CHECK TO BE SURE WE HAVEN'T FLIPPED THE PICTURE BY 180 DEGREES
      ZCOR=-COS1*COS(DLAM)
      XCOR=-COS(LR(4)*PIFAC)*COS(LOR(4)*PIFAC-CPSI)
      IF(ZCOR.GT.XCOR)CPSI=CPSI+PI
      CPSI=CPSI/PIFAC
      CPSI=AMOD(CPSI+360.,360.)
106   IF(ISCA.EQ.0)CALL mDSCALE(1,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     &PAR2)
C     IF LINE OR SAMP SPECIFIED, HE WANTS POLE IN A CERTAIN SPOT---
C     DO NOT CHANGE IT
      IF(ILIN+ISAM.EQ.0)THEN
C        OTHERWISE, PERFORM RECENTERING USING PLAT,PLON,PLIN,PSAM
C        PORTCN COMPUTES XC & ZC SO THAT LAT=APHI,LON=APSI FALL AT
C        LINE ZARB,SAMP XARB IN OUTPUT
         CALL PORTCN(CPSI,CPHI,APSI,APHI,XARB,ZARB,XC,ZC)
      ENDIF
10600 IF(ILAT.EQ.1)THEN ! IF USER SPECIFIED LAT, USE IT IN LABEL
         CALL XVPARM('LATITUDE',XLAT,COUNT,DEF,0)
      ELSE
         XLAT=CPHI
      ENDIF
      CALL LABEL(L,LNUM,ZC,XC,XLAT,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

C   *** OBLIQUE ORTHOGRAPHIC ***
110   CALL INIT(CPHI,CPSI,NORTH)
      IF(ISCA.EQ.0)CALL mDSCALE(2,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     &PAR2)
      IF(RECENT+PLIN+PSAM+PLAT+PLON.NE.0)
     &CALL ORTCEN(CPSI,CPHI,APSI,APHI,XARB,ZARB,NORTH,XC,ZC)
      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
c     if(rlon.ne.flag) rlon=amod(rlon,360.)
      GO TO 200

C   *** LAMBERT CONFORMAL CONIC ***
C               LAMBERT CONFORMAL PARAMETER POSSIBLES...
C    /CASE NO./    /SPECIFIED BY USER/      /DEFAULTED/
C        1.)        NONE                     SCAL,LIN1
C        2.)        SCAL                     LIN1
C        3.)        LIN1                     SCAL
C        4.)        LIN2                     SCAL
C        5.)        LINE                     SCAL
C        6.)        SCAL,LIN1                ---
C        7.)        SCAL,LIN2                ---
C        8.)        SCAL,LINE                ---
C        9.)        LIN1,LIN2                ---
C       10.)        LIN1,LINE                ---
C       11.)        LIN2,LINE                ---

C     FIRST,ASSIGN DEFAULT VALUES TO PARAMETERS...THEN GET USER
C      PARAMETERS, THEN DEFINE C AND K WITH WHATEVER WE HAVE...
115   ZC=.5*(1.+NL)
      LIN1=(1+NL)/4
      CALL XVPARM('LIN1',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          LIN1=RPAR(1)
          ILI1=1
      ENDIF
      LIN2=3.*(1+NL)/4
      CALL XVPARM('LIN2',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)THEN
          LIN2=RPAR(1)
          ILI2=1
      ENDIF
      PAR1=P1DF
      PAR2=P2DF
      IF(XVPTST('SOUTH'))ISOU=1
      IF(INOR.NE.0.OR.(ISOU.EQ.0.AND.CLAT.GT.0))GO TO 116
         PAR1=-P2DF
         PAR2=-P1DF
  116 XC=.5*(1+NS)
      CPSI=AMOD(CPSI+360.,360.)

      CALL XVPARM('PAR1',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)PAR1=RPAR(1)

      CALL XVPARM('PAR2',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)PAR2=RPAR(1)

      if (par1*par2.le.0.0) call mabend(
     & ' PAR1 and PAR2 must be of same sign and non-zero')
      IF(PAR1.LT.PAR2)THEN
         CALL XVMESSAGE('WARNING-PAR1 IS LESS THAN PAR2',' ')
         CALL XVMESSAGE('MAP3 WILL SWITCH THE VALUES',' ')
         RTEMP=PAR1
         PAR1=PAR2
         PAR2=RTEMP
      ENDIF

      CALL XVPARM('LIN1',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)LIN1=RPAR(1)

      CALL XVPARM('LIN2',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)LIN2=RPAR(1)

      CALL XVPARM('LINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)ZC=RPAR(1)

      CALL XVPARM('SAMPLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)XC=RPAR(1)

      CALL XVPARM('LONGITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPSI=RPAR(1)

      IF(ISCA.EQ.0)GO TO 11600
      GO TO 11601
11600 IF(ILI1+ILI2+ILIN.GE.2)GO TO 117
      CALL mDSCALE(5,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,PAR2)
11601 IF(RECENT+PLIN+PSAM+PLAT+PLON.EQ.0)GO TO 117
      IF(ILI1+ILI2+ILIN+ISAM.NE.0)THEN
         CALL XVMESSAGE('OVERSPECIFIED PROJECTION',' ')
         CALL ABEND
      ENDIF
      ILIN=100
      CALL LAMCEN(PAR1,PAR2,CPSI,APSI,APHI,XARB,ZARB,XC,ZC)
117   NCASE=LAMPAR(ISCA,ILI1,ILI2,ILIN)

C     WHEN N=1,2 WE DO NOT HAVE LIN1...SPECIFY CPHI AS CALC'D...
      IF(NCASE.LE.2)LIN1=CPHI
      CALL GETCK(PAR1,PAR2,LIN1,LIN2,ZC,NCASE)

C     PARAMETERS ARE NOW FULLY DEFINED...START PROJECTION
      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *    RDATA,IDATA,MP)
      ICAS=1
      IF(PAR1.LT.0.)ICAS=-1
      GO TO 200

C   *** POLAR STEREOGRAPHIC ***
120   IF(ILON.EQ.0)THEN 

        IF (LR(1) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
        IF (LR(2) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
        IF (LR(4) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
        IF (LOR(1) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
        IF (LOR(2) .EQ. flag) CALL MABEND('ERROR; Specify LONG')
        IF (LOR(4) .EQ. flag) CALL MABEND('ERROR; Specify LONG')

         CALL XVMESSAGE('***IF ATAN2 ABEND, SPECIFY LONG ***',' ')
         DLAM=(LOR(2)-LOR(1))*PIFAC
         COSLAM=COS(DLAM)
         SINLAM=SIN(DLAM)
         COS1=COS(LR(1)*PIFAC)
         COS2=COS(LR(2)*PIFAC)
         TANA=TAN(PI/4.-CAS*LR(1)*PIFAC/2.)
         TANB=TAN(PI/4.-CAS*LR(2)*PIFAC/2.)
         IF (TANA/TANB-COSLAM .EQ. 0. .AND. SINLAM .EQ. 0.)
     .      CALL MABEND('ATAN2 ERROR; Specify LONG')
         DLAM=ATAN2(TANA/TANB-COSLAM,SINLAM)
         CPSI=DLAM+LOR(1)*PIFAC
C        AGAIN BE SURE NOT TO TURN PICTURE UPSIDE DOWN
         ZCOR=-COS(DLAM)*TANA
         TANB=TAN(PI/4.-CAS*LR(4)*PIFAC/2.)
         XCOR=-COS(LOR(4)*PIFAC-CPSI)*TANB
         IF(ZCOR.GT.XCOR)CPSI=CPSI+PI
         CPSI=CPSI/PIFAC
         CPSI=AMOD(CPSI+360.,360.)
C        GET DEFAULT SCALE
      ENDIF
      IF(ISCA.EQ.0)CALL mDSCALE(3,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *PAR2)
      IF(ILIN+ISAM.EQ.0)THEN 
C        DON'T ADJUST POLE IF USER SPECIFIED IT
C        OTHERWISE PERFORM RECENTERING
         CALL PSTRCN(CPSI,CPHI,APSI,APHI,XARB,ZARB,XC,ZC)
      ENDIF
      IF(ILAT.EQ.1)THEN ! IF USER SPECIFIED LAT, USE IT IN LABEL
         CALL XVPARM('LATITUDE',XLAT,COUNT,DEF,0)
      ELSE
         XLAT=CPHI
      ENDIF
      CALL LABEL(L,LNUM,ZC,XC,XLAT,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      CALL INIT(CPHI,CPSI,NORTH)
c      RLON=AMOD(RLON+720.,360.)
      GO TO 200

C   *** OBLIQUE STEREOGRAPHIC ***
125   CALL INIT(CPHI,CPSI,NORTH)
      IF(ISCA.EQ.0)CALL mDSCALE(4,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *PAR2)
      IF(RECENT+PLIN+PSAM+PLAT+PLON.NE.0)
     &CALL STRCEN(CPSI,CPHI,APSI,APHI,XARB,ZARB,NORTH,XC,ZC)
      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

C   *** MERCATOR ***
130   IF(ILIN+PLIN.EQ.0)ZC=1.
      CALL XVPARM('PLINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)ZC=RPAR(1)

      IF(ISAM+PSAM.EQ.0)XC=1.
      CALL XVPARM('PSAMPLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)XC=RPAR(1)

      IF(ISCA.EQ.0)CALL mDSCALE(6,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *PAR2)
C     NOW DETERMINE LAT-LON OF UPPER LEFT CORNER
      CALL XVPARM('PLATITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPHI=RPAR(1)

      CALL XVPARM('PLONGITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPSI=RPAR(1)

      CALL MERCEN(CPHI,CPSI,XC,ZC,ILON+PLON+0,ILAT+PLAT+0,LR,LOR,NL,NS)
C     UPON RETURN, CPHI & CPSI WILL BE LAT-LON OF LINE 1 SAMP 1, AND
C     ZC & XC WILL HAVE BEEN SET TO 1.0 IF NECESSARY
      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

C   *** CYLINDRICAL ***
C     SETUP PARAMETERS
1300  IF(ILIN+PLIN.EQ.0)ZC=1.
      CALL XVPARM('PLINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)ZC=RPAR(1)

      IF(ISAM+PSAM.EQ.0)XC=1.
      CALL XVPARM('PSAMPLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)XC=RPAR(1)

      IF(ISCA.EQ.0)CALL mDSCALE(9,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *   PAR2)
C     NOW DETERMINE LAT-LON OF UPPER LEFT CORNER
      CALL XVPARM('PLATITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPHI=RPAR(1)

      CALL XVPARM('PLONGITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPSI=RPAR(1)

      CALL CYLCEN(CPHI,CPSI,XC,ZC,ILON+PLON+0,ILAT+PLAT+0,
     &LR,LOR,NL,NS,CSAM00)

C     UPON RETURN ZC WILL BE THE LINE INTERCEPTING LAT=0
C     AND CPSI WILL BE LONG OF SAMP 1 .  XC WILL HAVE BEEN RESET
C     TO 1.0 IF NECESSARY.  CSAM00 IS THE SAMPLE OF LONGITUDE ZERO.
      CALL LABEL(L,LNUM,ZC,CSAM00,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
C     The new CYLCAL seems to give the same results as the old if
c     we pass ZC=0 in the calling sequence.
c     We also add 1. to the value of RLAT for the same reason.
c     There is a basic difference in the code that makes these true.
c     The documentation is fuzzy.  21-JUN-1985 ...JAM...
      GO TO 200

C   *** RECTANGULAR (simple cylindrical)***
C       THIS PROJECTION RESULTS IN A CONSTANT NO. OF LINES PER
C       DEGREE OF LATITUDE.
C     SET UP PARAMETERS
1500  IF(ILIN+PLIN.EQ.0)ZC=1.
      CALL XVPARM('PLINE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)ZC=RPAR(1)

      IF(ISAM+PSAM.EQ.0)XC=1.
      CALL XVPARM('PSAMPLE',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)XC=RPAR(1)

      IF(ISCA.EQ.0)CALL mDSCALE(10,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,
     *     PAR1,PAR2)
C     DETERMINE LAT-LON OF UPPER LEFT CORNER
      CALL XVPARM('PLATITUD',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPHI=RPAR(1)

      CALL XVPARM('PLONGITU',RPAR,COUNT,DEF,0)
      IF(DEF.EQ.0.AND.COUNT.GT.0)CPSI=RPAR(1)

      CALL RECCEN(CPHI,CPSI,XC,ZC,ILON+PLON,ILAT+PLAT,LR,LOR,NL,NS,
     &               CSAM00)

C     UPON RETURN ZC WILL BE THE LINE INTERCEPTING LAT=0
C     AND CPSI WILL BE LONG OF SAMP 1 . XC WILL HAVE BEEN RESET
C     TO 1.0 IF NECESSARY.  CSAM00 IS THE SAMPLE OF LONGITUDE ZERO.
      CALL LABEL(L,LNUM,ZC,1.,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)!TRY 1. FOR CSAM00
      GO TO 200

c *** OBLIQUE SIMPLE CYLINDRICAL ***
1600  continue

      if(ilon+ilat.eq.0)then
         cphi=90.
         cpsi=0.0
      endif
      if(ipa1.eq.0) par1=0.0

      IF(ISCA.EQ.0)CALL mDSCALE(L,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *     PAR2)

      if(recent.gt.0)then
c        rotate the lat & lon of the center of the input
         call XVMESSAGE(
     .    'Center of projection before oblique rotation:',' ')
         call prnt(7,1,aphi,'Latitude = .')
         call prnt(7,1,apsi,'Longitude=.')
         call oblique_rot(aphi,apsi,cphi,cpsi,par1,1) ! rotate sphere
         call XVMESSAGE(
     .    'Center of projection after oblique rotation:',' ')
         call prnt(7,1,aphi,'Latitude = .')
         call prnt(7,1,apsi,'Longitude=.')
         zc=zarb+aphi*pifac*ra/f  ! ra=equatorial_radius f=scale 
         xc=xarb-(pi-apsi*pifac)*ra/f
      endif

      CALL LABEL(L,LNUM,ZC,XC,cphi,cpsi,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

c *** SINUSOIDAL ***
1700  continue

      IF(ISCA.EQ.0)CALL mDSCALE(L,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *     PAR2)

      if(recent.gt.0)then
c       convert latitudes to authalic
        call toauthalic(rpol,req,cphi,rad_auth,lat_auth) ! cphi
        call toauthalic(rpol,req,aphi,rad_auth,lat_auth2) ! aphi
        zc=zarb+(lat_auth2-lat_auth)*pifac*rad_auth/f
        delta=(cpsi-apsi)*pifac
        if(delta.lt.-pi) delta=delta+2.0*pi
        if(delta.ge.pi)  delta=delta-2.0*pi
        xc=xarb-delta*cos(lat_auth2*pifac)*rad_auth/f
      endif

      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

c *** OBLIQUE SINUSOIDAL ***
1800  continue
      IF(ISCA.EQ.0)CALL mDSCALE(L,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *     PAR2)

      if(ilon+ilat.eq.0)then
         cphi=90.
         cpsi=0.0
      endif
      if(ipa1.eq.0) par1=0.0

      if(recent.gt.0)then
c        rotate the lat & lon of the center of the input
         call XVMESSAGE(
     .    'Center of projection before oblique rotation:',' ')
         call prnt(7,1,aphi,'Latitude = .')
         call prnt(7,1,apsi,'Longitude=.')
         call oblique_rot(aphi,apsi,cphi,cpsi,par1,1) ! rotate sphere
         call XVMESSAGE(
     .    'Center of projection after oblique rotation:',' ')
         call prnt(7,1,aphi,'Latitude = .')
         call prnt(7,1,apsi,'Longitude=.')
c        convert latitude to authalic
         call toauthalic(rpol,req,aphi,rad_auth,lat_auth) ! aphi
         zc=zarb+lat_auth*pifac*rad_auth/f
         delta=(180.-apsi)*pifac
         if(delta.lt.-pi) delta=delta+2.0*pi
         if(delta.ge.pi)  delta=delta-2.0*pi
         xc=xarb-delta*cos(lat_auth*pifac)*rad_auth/f
      endif

      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

c *** MOLLWEIDE ***
1900  continue
      IF(ISCA.EQ.0)CALL mDSCALE(L,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *     PAR2)

      if(recent.gt.0)then
c        convert latitude to authalic
         call toauthalic(rpol,req,aphi,rad_auth,lat_auth) ! aphi
         aphi_mol=lat_auth
         call mollweide_iterate(aphi_mol,lat_auth) ! returns aphi_mol
         rx_mol=(2.0*sqrt(2.0)/pi)*(rad_auth/f)*(cpsi-apsi)*pifac*
     +           cos(aphi_mol * pifac)
         ry_mol=sqrt(2.0)*(rad_auth/f)*sin(aphi_mol * pifac)
         zc=zarb + ry_mol
         xc=xarb - rx_mol
      endif

      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)

      GO TO 200

c *** TRANSVERSE MERCATOR ***
2000  continue
      IF(ISCA.EQ.0)CALL mDSCALE(L,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *     PAR2)

      if(recent.gt.0)then
c       convert latitudes to conformal
c       note! the toconformal calls are in order for a reason.
c             rad_conf is a function of cphi.
        call toconformal(rpol,req,aphi,rad_conf,lat_conf) ! aphi
        call toconformal(rpol,req,cphi,rad_conf,lat_conf2) ! cphi
c       compute ZC & XC
        call tmerccen(rad_conf,f,lat_conf2,cpsi,lat_conf,
     +             apsi,zarb,xarb,zc,xc)
      endif        

      CALL LABEL(L,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,LIN1,LIN2,
     *   RDATA,IDATA,MP)
      GO TO 200

c *** PERSPECTIVE ***
2100  continue
      if(isca.gt.0) then         ! recompute range to planet
         rmag_pers=f*scale*focl + rpol  ! see comments at end of map3.f.
      else
         rmag_pers=rmag
      endif

      if(ilat.gt.0)then
         slat_pers=cphi
      else
         slat_pers=slat
      endif

      if(ilon.gt.0)then
         slong_pers=cpsi
      else
         slong_pers=slong
      endif

      cl_pers=zc
      cs_pers=xc
      if(recent.gt.0)then
c        compute OM & RS only (no label update)
         call label16(L,slat_pers,slong_pers,zc,xc,north,rmag_pers,
     +             cl_pers,cs_pers,scale,focl,rpol,
     +             req,rdata,idata,mp,1)

c        compute the line of the PLAT, PLON
         call convev(ind,rdata,rdata,r_line,r_samp,aphi,apsi,1,cvev)
c        move the center over so the desired plat,plon is at pline,psamp
         zc=zc + zarb - r_line
         xc=xc + xarb - r_samp
c        keep optical axis at center of projection for mosaicking purposes
         cl_pers=zc
         cs_pers=xc
      endif
c     compute the OM RS & update label
      call label16(L,slat_pers,slong_pers,zc,xc,north,rmag_pers,
     +             cl_pers,cs_pers,scale,focl,rpol,
     +             req,rdata,idata,mp,0)
      GO TO 200

c End of the projection setup branch
200   continue

      if(nogeom.eq.1) return

c load input image into memory array DN
      do line=1,min(nli,nlmax)
        call xvread(unit(2),dn(1,line),stat,'NSAMPS',min(nsi,nsmax),' ')
      enddo
      call xvclose(unit(2),stat,' ')

      IF (PRINT)  THEN   ! PRINT INFO FOR A RANGE OF LINES.
          print *,' GRID POINT INFO'
          PRINT *,
     . '  LINE  SAMP         LATITUDE   LONGITUDE    IN_LINE    IN_SAMP'
      END IF

c main grid loop
      real_nli=min(nli,nlmax)
      real_nsi=min(nsi,nsmax)
      do 321 il=1,nl+inc,inc  ! line loop
         L=0

c        Save the last row in xylast
         call mve(7,4*ngrid,xy,xylast,1,1)

c        Compute a row of tiepoints
         do 322 is=1,ns+inc,inc  ! sample loop
            L=L+1

c           Compute output frame grid lat & lon
            call convev(ind,rdata,rdata,real(il),real(is),rlat,rlon,
     +            2,cvev)
            rlon=amod(rlon+720.,360.)
            if(ind.ne.0) goto 323 

c           convert the longitude from the reference time to the input time.
c           this corrects for zonal flow.
            if(use_zonal_flow) then
               call zonal_movement(rlat,delta_time,rdata(25),
     +                    rdata(26),
     +                    zonal_vel,travel_m,travel_deg)
               rlon=rlon + travel_deg
               rlon=amod(rlon+720.,360.)
            endif

c           Compute zcor & xcor the input picture line & sample
            call corcav(ind,rlat,-rlon,om,
     +            rsvector,prfl,rb,rb-rc,zcor,xcor,
     +            cl,cs,flag)
            IF(IND.NE.0)GO TO 323

c           Compute image space line & sample from object line & samp
            IF(DISTOR.EQ.1)then
              call convisos(project,camera,zcor2,xcor2,
     +                 zcor,xcor,0,conv(istart)
     +                 ,nah+1,nav+1,ind)
              if(ind.ne.0)then
                call XVMESSAGE('CONVISOS: error,abend',' ')
                call abend
              endif
              zcor=zcor2
              xcor=xcor2
            endif

c           store image coordinates
            xy(1,L)=il    ! output line
            xy(2,L)=is    ! output samp
            xy(3,L)=zcor  ! input line
            xy(4,L)=xcor  ! input samp

            IF (PRINT)  THEN   ! PRINT INFO FOR A RANGE OF LINES.
            IF (PRINTBEG .LE. il .AND. il .LE. PRINTEND) THEN
                PRINT 9001, il,is, rlat,rlon,zcor,xcor
9001            FORMAT (1X, 2I6,6X,4F12.5)

                GRIDLAT(L) = rlat         ! for PRINT 9002.
                GRIDLON(L) = rlon
            END IF
            END IF

            goto 322

323         xy(1,L)=0.0

322      continue

         if(il.eq.1) goto 321  ! need 2 rows to begin

c        Loop on each square
         L=0
         do 324 is=1,ns,inc
            L=L+1

c           Count # corners on planet for each square
            n=0
            if(xy(1,L).ne.0.0) n=n+1
            if(xy(1,L+1).ne.0.0) n=n+1
            if(xylast(1,L).ne.0.0) n=n+1
            if(xylast(1,L+1).ne.0.0) n=n+1
            counts(L)=n

c           Compute polynomial coefficients if all 4 corners are
c           on the planet.
c           input line= c1*L+c2*s+c3*L*s+c4
c           input samp= c1*L+c2*s+c3*L*s+c4
            if(n.eq.4)then
               amat1(1)=xylast(1,L)
               amat1(2)=xylast(1,L+1)
               amat1(3)=xy(1,L)
               amat1(4)=xy(1,L+1)
               amat1(5)=xylast(2,L)
               amat1(6)=xylast(2,L+1)
               amat1(7)=xy(2,L)
               amat1(8)=xy(2,L+1)
               amat1(9)=amat1(1)*amat1(5)
               amat1(10)=amat1(2)*amat1(6)
               amat1(11)=amat1(3)*amat1(7)
               amat1(12)=amat1(4)*amat1(8)
               amat1(13)=1.d0
               amat1(14)=1.d0
               amat1(15)=1.d0
               amat1(16)=1.d0
               ycoef(1,L)=xylast(3,L)
               ycoef(2,L)=xylast(3,L+1)
               ycoef(3,L)=xy(3,L)
               ycoef(4,L)=xy(3,L+1)
               call mve(8,16,amat1,amat2,1,1)
               call mDSIMQ(amat1,ycoef(1,L),4,i)
               if(i.ne.0)then
                  if(iysing_sol.eq.0) then 
                    call XVMESSAGE('singular y solutn',' ')
                    call XVMESSAGE('input:',' ')
                    call prnt(7,1,xylast(1,L),'line.')
                    call prnt(7,1,xylast(1,L+1),'line.')
                    call prnt(7,1,xy(1,L),'line.')
                    call prnt(7,1,xy(1,L+1),'line.')
                    call prnt(7,1,xylast(2,L),'sample.')
                    call prnt(7,1,xylast(2,L+1),'sample.')
                    call prnt(7,1,xy(2,L),'sample.')
                    call prnt(7,1,xy(2,L+1),'sample.')
                    call XVMESSAGE('output:',' ')
                    call prnt(7,1,xylast(3,L),'line.')
                    call prnt(7,1,xylast(3,L+1),'line.')
                    call prnt(7,1,xy(3,L),'line.')
                    call prnt(7,1,xy(3,L+1),'line.')
                  endif                     
                  iysing_sol=iysing_sol+1
                  counts(L)=0
                  goto 324
               endif
               xcoef(1,L)=xylast(4,L)
               xcoef(2,L)=xylast(4,L+1)
               xcoef(3,L)=xy(4,L)
               xcoef(4,L)=xy(4,L+1)
               call mDSIMQ(amat2,xcoef(1,L),4,i)
               if(i.ne.0)then
                  if(ixsing_sol.eq.0) then
                    call XVMESSAGE('singular x solutn',' ')
                    call XVMESSAGE('input:',' ')
                    call prnt(7,1,xylast(1,L),'line.')
                    call prnt(7,1,xylast(1,L+1),'line.')
                    call prnt(7,1,xy(1,L),'line.')
                    call prnt(7,1,xy(1,L+1),'line.')
                    call prnt(7,1,xylast(2,L),'sample.')
                    call prnt(7,1,xylast(2,L+1),'sample.')
                    call prnt(7,1,xy(2,L),'sample.')
                    call prnt(7,1,xy(2,L+1),'sample.')
                    call XVMESSAGE('output:',' ')
                    call prnt(7,1,xylast(4,L),'line.')
                    call prnt(7,1,xylast(4,L+1),'line.')
                    call prnt(7,1,xy(4,L),'line.')
                    call prnt(7,1,xy(4,L+1),'line.')
                  endif
                  ixsing_sol=ixsing_sol+1
                  counts(L)=0
                  goto 324
               endif
            endif
324      continue

         IPRINTDN = DNTHRESH+20
         HEADER   = .FALSE.   ! 'PRINT' HEADER NOT YET PRINTED.

c        Perform the GEOM operation on the output lines contained
c        between the last 2 rows of tiepoints.
         do 326 line=il-inc,il-1
            j=100000

            IF (PRINT) THEN   !PRINT INFO FOR A RANGE OF OUTPUT LINES.
            IF (PRINTBEG .LE. LINE .AND. LINE .LE. PRINTEND) THEN
                FIRST= .TRUE.
                FIRST1= .TRUE.
            ELSE
                FIRST = .FALSE.
                FIRST1 = .FALSE.
            END IF
            END IF

            L=0
            do 327 i=1,ns
               ii=i  ! copy so we do not pass do loop index to external routine.
               j=j+1
               if(j.gt.inc) then    ! start new rectangle in output
                  j=1
                  L=L+1
c                 precompute redundant polynomial terms
                  temp1=ycoef(1,L)*line+ycoef(4,L)
                  temp2=ycoef(3,L)*line+ycoef(2,L)
                  temp3=xcoef(1,L)*line+xcoef(4,L)
                  temp4=xcoef(3,L)*line+xcoef(2,L)
               endif
               if(counts(L).eq.0)then       ! 4 corners off planet
                  obuf(i)=0
               else if(counts(L).eq.4)then  ! 4 corners on planet
ccc......              zcor=ycoef(1,L)*line+ycoef(2,L)*I+ycoef(3,L)*line
ccc......     +                 *i+ycoef(4,L)
ccc......                 xcor=xcoef(1,L)*line+xcoef(2,L)*I+xcoef(3,L)*line
ccc......     +                 *i+xcoef(4,L)
                  zcor8=temp1+temp2*i
                  xcor8=temp3+temp4*i
               call setDnValue(zcor8,xcor8,ii,obuf,interpolate,
     +                           real_nli,real_nsi,dn,dnthresh)  

               IF (PRINT) THEN    !PRINT INFO FOR A RANGE OF LINES.
               IF (FIRST1) THEN
               IF (OBUF(I) .GT. IPRINTDN) THEN   ! CHOOSE A GOOD POINT.
               IF (.NOT. HEADER)  THEN  
                  print *,' INFO FOR SOME OUTPUT POINTS'
                  PRINT *,
     . '  LINE  SAMP   DN    LATITUDE   LONGITUDE    IN_LINE    IN_SAMP'
                  HEADER = .TRUE.
               END IF
                  PRINT 9002, LINE, II, OBUF(II),GRIDLAT(L),GRIDLON(L),
     .                        ZCOR8,XCOR8
9002              FORMAT (1X, 3I6,F12.5,'*',F11.5,'*',F11.5,F12.5)
                  FIRST1 = .FALSE.    !  * INDICATES approximate value from grid
               END IF
               END IF
               END IF

               else                        ! exact solution at each pnt
c                 Compute output frame point lat & lon
                  call convev(ind,rdata,rdata,real(line),
     +                        real(i),rlat,rlon,2,cvev)
                  rlon=amod(rlon+720.,360.)
                  IF(IND.NE.0) THEN
                     obuf(i) =0
                     goto 327
                  ENDIF

c                 convert the longitude from the reference time to the 
c                 input time.
c                 this corrects for zonal flow.
                  if(use_zonal_flow) then
                     call zonal_movement(rlat,delta_time,rdata(25),
     +                          rdata(26),
     +                          zonal_vel,travel_m,travel_deg)
                     rlon=rlon + travel_deg
                     rlon=amod(rlon+720.,360.)
                  endif

c                 Compute zcor & xcor the input picture line & sample
                  call corcav(ind,rlat,-rlon,om,
     +                        rsvector,prfl,rb,rb-rc,zcor,xcor,
     +                        cl,cs,flag)
                  IF(IND.NE.0) THEN
                     obuf(i) =0
                     goto 327
                  ENDIF
                     zcor8=zcor
                     xcor8=xcor

c                 Compute image space line & sample from 
c                 object line & samp
                  IF(DISTOR.EQ.1)then
                     call convisos(project,camera,zcor2,xcor2,
     +                 zcor,xcor,0,conv(istart)
     +                 ,nah+1,nav+1,ind)
                     if(ind.ne.0)then
                       call XVMESSAGE('CONVISOS: error,abend',' ')
                       call abend
                     endif
                     zcor8=zcor2
                     xcor8=xcor2
                  endif

                  call setDnValue(zcor8,xcor8,ii,obuf,interpolate,
     +                           real_nli,real_nsi,dn,dnthresh)  

                  IF (PRINT) THEN    !PRINT INFO FOR A RANGE OF LINES.
                  IF (FIRST) THEN

                  IF (.NOT. HEADER)  THEN  
                     print *,' INFO FOR SOME OUTPUT POINTS'
                     PRINT *,
     . '  LINE  SAMP   DN    LATITUDE   LONGITUDE    IN_LINE    IN_SAMP'
                     HEADER = .TRUE.
                  END IF

                    PRINT 9003, LINE, II, OBUF(II),RLAT,RLON,ZCOR8,XCOR8
9003                FORMAT (1X, 3I6,4F12.5)
                    FIRST = .FALSE.
                  END IF
                  END IF

               endif
327         continue
            
c           write a processed line to the output
            call xvwrit(unit(1),obuf,stat,' ')    ! line=line
326     continue       
        
321   continue
      call xvclose(unit(1),stat,' ')
      if(iysing_sol.gt.0.or.ixsing_sol.gt.0)then
       call prnt(4,1,iysing_sol,'#singular y solutions in grid=.')
       call prnt(4,1,ixsing_sol,'#singular x solutions in grid=.')
      endif

 111  call mp_free(mp)
      return
      END

c*************************************************************
c  computes output dn value
c
      subroutine setDnValue(z,x,i,obuf,interp,r_nli,r_nsi,dn,dnt)

      parameter (nlmax=1150,nsmax=1250,nsomax=10000)

      real*8    z,x,dnyt,dnyb,r_nli,r_nsi
      integer*4 dnt ! dnthreshold        
      integer*2 dn(nsmax,nlmax),obuf(nsomax) !input image read into dn array.
      integer*2 pts(4)
      logical   interp,PTn(4)
C==================================================================

      if(z.gt.1.d0 .and.z.lt.r_nli.and.
     +               x.gt.1.d0 .and.x.lt.r_nsi)then
               iyin=z
               ixin=x  
               pts(1)=dn(ixin,iyin)
               pts(2)=dn(ixin+1,iyin)
               pts(3)=dn(ixin,iyin+1)
               pts(4)=dn(ixin+1,iyin+1)
               n=0           !count number of adj pixel > dnt
               do index=1,4
                 if(pts(index).gt.dnt) then 
                   n=n+1
                   PTn(index)=.true.
                 else
                   PTn(index)=.false.
                  end if      
               enddo                
         if (interp.and.n.gt.0) then    ! interpolation on         
               if (n.eq.4) then 
                    dnyt=pts(1)*(ixin+1-x)+pts(2)*(x-ixin)
                    dnyb=pts(3)*(ixin+1-x)+pts(4)*(x-ixin)
                    obuf(i)=nint(dnyt*(iyin+1-z)+dnyb*(z-iyin))
               else if (n.eq.3) then
                    if (.not.PTn(1)) then
                       pts(1)=pts(2)+pts(3)-pts(4)
                       dnyt=pts(1)*(ixin+1-x)+pts(2)*(x-ixin)
                       dnyb=pts(3)*(ixin+1-x)+pts(4)*(x-ixin)
                       obuf(i)=nint(dnyt*(iyin+1-z)+dnyb*(z-iyin))
                    end if
                    if (.not.PTn(2)) then
                       pts(2)=pts(1)+pts(4)-pts(3)
                       dnyt=pts(1)*(ixin+1-x)+pts(2)*(x-ixin)
                       dnyb=pts(3)*(ixin+1-x)+pts(4)*(x-ixin)
                       obuf(i)=nint(dnyt*(iyin+1-z)+dnyb*(z-iyin))
                    end if
                    if (.not.PTn(3)) then
                       dnyt=pts(1)*(ixin+1-x)+pts(2)*(x-ixin)
                       pts(3)=pts(1)+pts(4)-pts(2)
                       dnyb=pts(3)*(ixin+1-x)+pts(4)*(x-ixin)
                       obuf(i)=nint(dnyt*(iyin+1-z)+pts(4)*(z-iyin))
                    end if
                    if (.not.PTn(4)) then
                       dnyt=pts(1)*(ixin+1-x)+pts(2)*(x-ixin)
                       pts(4)=pts(2)+pts(3)-pts(1)
                       dnyb=pts(3)*(ixin+1-x)+pts(4)*(x-ixin)
                       obuf(i)=nint(dnyt*(iyin+1-z)+pts(3)*(z-iyin))
                    end if
               else if (n.eq.2) then
                    if(.not.PTn(1).and..not.PTn(2)) then
                       obuf(i)=nint(pts(3)*(ixin+1-x)+pts(4)*(x-ixin))
                    end if
                    if(.not.PTn(3).and..not.PTn(4)) then
                       obuf(i)=nint(pts(1)*(ixin+1-x)+pts(2)*(x-ixin))
                    end if
                    if(.not.PTn(1).and..not.PTn(3)) then
                       obuf(i)=nint(pts(2)*(iyin+1-z)+pts(4)*(z-iyin))
                    end if
                    if(.not.PTn(2).and..not.PTn(4)) then
                       obuf(i)=nint(pts(1)*(iyin+1-z)+pts(3)*(z-iyin))
                    end if    
                    if(.not.PTn(1).and..not.PTn(4)) then
                      dnyt=(pts(2)+pts(3))*.5*(ixin+1-x)+pts(2)*(x-ixin)
                      dnyb=pts(3)*(ixin+1-x)+(pts(2)+pts(3))*.5*(x-ixin)
                      obuf(i)=nint(dnyt*(iyin+1-z)+dnyb*(z-iyin))
                    end if 
                    if(.not.PTn(2).and..not.PTn(3)) then
                      dnyt=pts(1)*(ixin+1-x)+(pts(1)+pts(4))*.5*(x-ixin)
                      dnyb=(pts(1)+pts(4))*.5*(ixin+1-x)+pts(4)*(x-ixin)
                      obuf(i)=nint(dnyt*(iyin+1-z)+dnyb*(z-iyin))
                    end if                                     
               else if (n.eq.1) then
                 do index=1,4
                    if (PTn(index)) obuf(i)=pts(index) 
                 enddo
               end if        
         else                   ! interpolation off or all neighbors < dnt
            iyin=nint(z)        ! determine the nearest neighbor            
            ixin=nint(x)
            obuf(i)=dn(ixin,iyin)
         endif
       else
         obuf(i)=0              ! point is outside of image
       endif
       return
       end

c*************************************************************
c  computes authalic latitude latauth from planetocentric latitude
c   latr. latitudes in degrees.
c  computes authalic radius rauth from polar radius rp and equatorial
c   radius re.

      subroutine toauthalic(rp,re,latr,rauth,latauth)
      implicit real*8 (a-z)
      real*4 rp,re,latr,rauth,latauth           

c no-op
      if(rp.eq.re)then
         rauth=re
         latauth=latr
         return
      endif

      pi=3.141592653589d0
      degrad=pi/180.d0

      drp=dble(rp)
      dre=dble(re)
      epsilon=dsqrt(1.d0-(drp*drp)/(dre*dre))

c convert latr from geocentric to geodetic
      glatr=datan(((dre*dre)/(drp*drp))*dtan(latr*degrad))

      sin_phi=1.d0
      qp= (1 - EPSILON ** 2) *
     2   (SIN_PHI / (1 - (EPSILON * SIN_PHI) ** 2) -
     3    1 / (2 * EPSILON) * DLOG((1 - EPSILON * SIN_PHI) /
     4			 	   (1 + EPSILON * SIN_PHI)))
      rauth=dre*dsqrt(qp/2.d0)

      sin_phi=dsin(glatr*degrad)
      qp2= (1 - EPSILON ** 2) *
     2   (SIN_PHI / (1 - (EPSILON * SIN_PHI) ** 2) -
     3    1 / (2 * EPSILON) * DLOG((1 - EPSILON * SIN_PHI) /
     4			 	   (1 + EPSILON * SIN_PHI)))
      latauth=(dasin(qp2/qp))/degrad
      return
      end     

c*************************************************************
c  computes conformal latitude latconf from planetocentric latitude
c   latr. latitudes in degrees.
c  computes conformal radius rconf from polar radius rp and equatorial
c   radius re.

      subroutine toconformal(rp,re,latr,rconf,latconf)
      implicit real*8 (a-z)
      real*4 rp,re,latr,rconf,latconf

c no-op
      if(rp.eq.re)then
         rconf=re
         latconf=latr
         return
      endif

      pi=3.141592653589d0
      degrad=pi/180.d0

      drp=dble(rp)
      dre=dble(re)
      epsilon=dsqrt(1.d0-(drp*drp)/(dre*dre))

c convert latr from geocentric to geodetic
      glatr=datan(((dre*dre)/(drp*drp))*dtan(latr*degrad))

      x=dtan(pi/4.d0+glatr/2.d0)*
     +   ((1.d0-epsilon*dsin(glatr))/(1.d0+epsilon*dsin(glatr)))
     +   **(epsilon/2.d0)
      chi=2.d0*(datan(x)-pi/4.d0)
      latconf=chi/degrad

      radcurv=dre*dre/dsqrt(dre*dre*(dcos(glatr))**2+
     +                      drp*drp*(dsin(glatr))**2)
      rconf=radcurv*dcos(glatr)/dcos(chi)
      return
      end     

c**************************************************************
c returns zc,xc for transverse mercator
c a= conformal radius
c F=scale km/pixel
c thr0=special lat (conformal)
c lamr=special longitude
c longr=longitude
c latr= conformal latitude
      subroutine tmerccen(Aa,Ff,thr,lam,lat,long,line,sample,zc,xc)
      implicit real*8 (a-z)
      real*4 aa,ff,thr,lam,long,lat,zc,xc,line,sample

      pi=3.141592653589d0
      degrad=pi/180.d0
      latr=dble(lat*degrad)
      longr=dble(long*degrad)
      lamr=dble(lam*degrad)
      thr0=dble(thr*degrad)
      f=dble(ff)
      a=dble(aa)

	DELL=LONGR-LAMR
1505	IF(DABS(DELL).LE.PI) GO TO 1510
	IF(DELL.GT.0.0D0) GO TO 1520
	DELL=DELL+2.D0*PI
	GO TO 1505
1520	DELL=DELL-2.D0*PI
	GO TO 1505
1510	CONTINUE

C	DIRECT
	    B=DCOS(LATR)*DSIN(DELL)

	    !LINE,SAMPLE CALCULATION BEGINS
	    IF (LATR.GT.-PI/2.AND.LATR.LT.PI/2.AND.
     *          DELL.GT.-PI/2.AND.DELL.LT.PI/2) THEN
	    	ZC=LINE+(A/F)*(DATAN(DTAN(LATR)/DCOS(DELL))-THR0)
  	    	XC=SAMPLE+(0.5D0)*(A/F)*DLOG((1+B)/(1-B))	
	    ELSE IF (LATR.LE.-PI/2)  THEN
		ZC=LINE+(A/F)*(-PI/2-THR0)
		XC=SAMPLE
	    ELSE IF (LATR.GE.PI/2)  THEN
		ZC=LINE+(A/F)*(PI/2-THR0)
		XC=SAMPLE
	    ELSE IF (DABS(DELL).GT.PI/2.AND.DABS(DELL).LE.PI) THEN
		IF (LATR.GT.-PI/2.AND.LATR.LT.0.D0) THEN
         	ZC=LINE+(A/F)*(-PI+DATAN(DTAN(LATR)/DCOS(DELL))-THR0)
	   	ELSE IF (LATR.GE.0.D0.AND.LATR.LT.PI/2) THEN
         	ZC=LINE+(A/F)*(PI+DATAN(DTAN(LATR)/DCOS(DELL))-THR0)
		ENDIF
  	    	XC=SAMPLE+(0.5D0)*(A/F)*DLOG((1+B)/(1-B))
	    ELSE IF (DELL.EQ.-PI/2.OR.DELL.EQ.PI/2) THEN
		     IF (LATR.GE.-0.00001.AND.LATR.LE.0.00001)  THEN
                        call XVMESSAGE(
     .                       'TMERC: undefined value for XC',' ')
			XC=SAMPLE
			ZC=LINE
		     ELSE IF (LATR.LT.0.D0) THEN
			ZC=LINE+(A/F)*((-PI/2+0.0001)-THR0)
			XC=SAMPLE+(0.5D0)*(A/F)*DLOG((1+B)/(1-B))
		     ELSE
			ZC=LINE+(A/F)*(PI/2-THR0-0.00001)
			XC=SAMPLE+(0.5D0)*(A/F)*DLOG((1+B)/(1-B))
		     ENDIF
	    ENDIF
      return
      end


c************************************************************************
      SUBROUTINE  MOLLWEIDE_ITERATE ( THETA_in, PHI_in )
c used in Mollweide projection. Returns theta. phi=latitude.
      REAL*8 THETA, PHI
      REAL*8 DEL_THETA, ERROR, PI
      PI=3.141592653589D0
      theta=theta_in*pi/180.d0
      phi=phi_in*pi/180.
      ERROR = 1.0D-10
      DEL_THETA = 100.
      DO WHILE ( DABS(DEL_THETA) .GT. ERROR )
	DEL_THETA = - ( THETA + DSIN(THETA) - 
     +PI*DSIN(PHI))/(1+DCOS(THETA))
        THETA = THETA + DEL_THETA
      END DO
      THETA_in = (THETA/2)*180.d0/pi
      RETURN
      END

c *********************************************************************
  	SUBROUTINE OBLIQUE_ROT(PHI4,LAMBDA4,ALPHA4,BETA4,LAMBDA04,MODE)

C	WHEN MODE = 1, THIS SUBROUTINE CONVERTS LATITUDE, PHI, AND W. LONGITUDE,
C  	LAMBDA, TO OBLIQUE COORDINATES IN THE SYSTEM WHERE THE OBLIQUE NORTH
C	POLE IS AT PLANETODETIC LATITUDE, ALPHA, AND LONGITUDE, BETA AND THE
C  	OBLIQUE	MERIDIAN AT LONGITUDE LAMBDA0 COOINCIDES WITH THE MERIDIAN AT
C  	PLANET LONGITUDE BETA.  WHEN MODE = 2, THE INVERSE TRANSFORMATION IS	
C  	PERFORMED.  INTERNALLY, THIS SUBROUTINE USES E. LONGITUDE. 
C	(C.F. USGS BULL. 1532, P. 35)
c       All angles in degrees.

  	IMPLICIT NONE
        real*4 phi4,lambda4,alpha4,beta4,lambda04
  	REAL*8 PHI,LAMBDA,ALPHA,BETA,LAMBDA0
  	INTEGER*4 MODE
  	REAL*8 OPHI,OLAMBDA
  	REAL*8 SIN_ALPHA,COS_ALPHA,SIN_PHI,COS_PHI,SIN_DELTA,COS_DELTA
        REAL*8 PI/3.141592653589D0/
        real*8 degrad
        REAL*8 SMALL/1D-8/

        degrad=pi/180.
        phi=phi4*degrad
        lambda=lambda4*degrad
        alpha=alpha4*degrad
        beta=-beta4*degrad         ! switch to East long
        lambda0=-lambda04*degrad   !       "

  	SIN_ALPHA = DSIN(ALPHA)
  	COS_ALPHA = DCOS(ALPHA)

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

        phi4=phi/degrad
        lambda4=lambda/degrad
        RETURN

  	END

c ********************************************************************
C returns the zonal velocity profile in common array ZVP
      subroutine get_zvp
      COMMON/ZVP/NZ,ZVP(2,1000)
      REAL*4 ZVP
      CHARACTER*256 FNAME

      CALL XVPARM('ZVP',fname,icnt,idef,0)	!Get file name
      CALL XVUNIT(iunit,'Z',1,IND,'U_NAME',FNAME,' ')
      CALL XVSIGNAL(IUNIT,IND,.TRUE.)
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT,IND,'NL',nl,'NS',ns,' ')
      IF (NL.eq.1) then
        NZ = NS/2
        CALL XVREAD(IUNIT,zvp,ind,' ')
        CALL XVCLOSE(IUNIT,ind,' ')
      else
        CALL XVMESSAGE(
     .    '***Invalid Zonal Velocity Profile file format',' ')
        call abend
      endif
      RETURN
      END


c ********************************************************************
c TIME_DIF returns the time difference in seconds for time1-time2.
c INPUT:
c time1(1-6) year, day, hour, minute, second, millisecond (input integer*4)
c time2(1-6) year, day, hour, minute, second, millisecond (input integer*4)
c delta      time1-time2 in seconds  (returned real*8).
      subroutine time_diff(time1,time2,delta)
      integer*4 time1(6),time2(6)
      real*8 t1,t2,delta
      t1=dble(time1(6))/1000.d0 + dble(time1(5)) +
     +   60.d0*(dble(time1(4)) + 60.d0*(dble(time1(3)) +
     +   24.d0*(dble(time1(2)) + 365.25d0*dble(time1(1))  )))
      t2=dble(time2(6))/1000.d0 + dble(time2(5)) +
     +   60.d0*(dble(time2(4)) + 60.d0*(dble(time2(3)) +
     +   24.d0*(dble(time2(2)) + 365.25d0*dble(time2(1))  )))
      delta=t1 - t2
      return
      end

c ********************************************************************
C Given planetocentric latitude RLATdeg in degrees
c Given time interval delta in seconds
c Given planet radii r_pole, r_equator in km.
c compute zonal velocity U from zonal velocity profile in meters/sec.
c compute and distance travelled in longitude in meters.
c compute the distance travelled in longitude in degrees.
C
      SUBROUTINE ZONAL_movement(RLATdeg,delta,r_pole,r_equator,u,
     +                 travel_m,travel_deg)
      COMMON/ZVP/NZ,ZVP(2,1000)
      real*8 delta
      REAL*4 ZVP,pi/3.141592654/

      IF (RLATdeg.EQ.-999.0) GOTO 990

c     convert geocentric latitude to radians
      rlatgc=RLATdeg*pi/180.

c     convert to geodetic latitude in radians.
      if(r_pole.eq.r_equator)then
         rlat=rlatgc
      else
        if(RLATdeg.lt.-89.999)then
           rlat=-89.99*pi/180.
        else if(RLATdeg.gt.89.999)then
           rlat=89.99*pi/180.
        else
           rlat=atan( ((dble(r_equator))/(dble(r_pole)))**2 *
     +                tan(rlatgc) )
        endif
      endif

      I = NZ/2
C     ....Search so that  ZVP(i) < RLAT < ZVP(i+1)
   10 IF (RLAT.GE.ZVP(1,I).AND.RLAT.LE.ZVP(1,I+1)) GOTO 20
      I0 = I
      I = I + (RLAT-ZVP(1,I))/(ZVP(1,I+1)-ZVP(1,I))
      IF (I.LT.1) I=1
      IF (I.GE.NZ-1) I=NZ-2
      IF (I.EQ.I0) GOTO 990
      GOTO 10
C     ....Interpolate between points
   20 U = ZVP(2,I) + (ZVP(2,I+1)-ZVP(2,I))*(RLAT-ZVP(1,I))/
     &		(ZVP(1,I+1)-ZVP(1,I))
      travel_m = u*delta
      if(r_pole.eq.r_equator)then
        r=r_pole
      else
        r=dble(r_pole)*dble(r_equator)/dsqrt(
     +  (dble(r_pole))**2 *(dcos(dble(rlatgc)))**2   +
     +  (dble(r_equator))**2 *(dsin(dble(rlatgc)))**2   )
      endif
      travel_deg=(travel_m * 180.)/(r * 1000. * cos(rlatgc) * pi)
      RETURN

C     ....All latitudes outside range of table are set to zero
  990 U = 0.0
      travel_m=0.0
      travel_deg=0.0
      RETURN
      END


C *********************************************************************
C        SUBROUTINE DSIMQ
C
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL mDSIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
C
      SUBROUTINE mDSIMQ(A,B,N,KS)
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END


c ****************************************************************
      SUBROUTINE mDSCALE(IPROJ,LR,LOR,CPHI,CPSI,NL,NS,CAS,NORTH,PAR1,
     *      PAR2)
      COMMON/C1/D(7),F,RPOL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,XC,ZC,TH,LAM
      REAL*4 LR(5),LOR(5),Y(5),X(5),MAXDX,MAXDY,NORTH,LINE
      REAL*4 VALS(9)/.1,.2,.3,.4,.5,.6,.7,.8,.9/
      CHARACTER*3600 LABI
C     29-OCT-1985 -JAM- ADDED REAL*4 LINE OTHERWISE WE GET A BAD "F"

C     DEFAULT SCALE CALCULATED FOR SPECIFIED PROJECTION

      PIFAC=PI/180.
      XC=0.
      ZC=0.
      TH=0.
      LAM=0.
      J=0
      DO 10 I=1,5
      IF(LR(I).EQ.FLAG)GO TO 10
      J=J+1
      CALL TRANV(IND,IPROJ,1,XC,ZC,TH,DBLE(PAR1),DBLE(PAR2),LAM,1.D0,
     &           DBLE(CAS),LINE,SAMP,LR(I),LOR(I),DBLE(RPOL),DBLE(REQ),
     &           DBLE(NORTH))
      X(I)=SAMP
      Y(I)=LINE
10    CONTINUE
      IF(J.LT.2)GO TO 30

      MAXDY=0.
      MAXDX=0.
      DO 20 I=1,4
         K=I+1
         DO 21 J=K,5
            IF(LR(I).EQ.FLAG.OR.LR(J).EQ.FLAG)GO TO 21
            DY=ABS(Y(I)-Y(J))
            DX=ABS(X(I)-X(J))
            IF(DY.GT.MAXDY)MAXDY=DY
            IF(DX.GT.MAXDX)MAXDX=DX
21       CONTINUE
20    CONTINUE
      F=AMAX1(MAXDY/NL,MAXDX/NS)
      GO TO 50

C        ASSUME ENTIRE DISK IS IN PICTURE
30    DMAX=RA*PI
      F=AMAX1(DMAX/NL,DMAX/NS)

C        ROUND F UP TO CLOSEST GREATER VALUE IN VALS
50    EXP=INT(ALOG10(F)+.00001)
      FRAC=F/(10.**(EXP+1))
      DO 25 I=1,9
         IF(FRAC.GT.VALS(I))GO TO 25
         FRAC=VALS(I)
         GO TO 15
   25 CONTINUE
      FRAC=1.0
   15 F=FRAC*10.**(EXP+1)
      CALL PRNT(7,1,F,' SCALE SELECTED=.')
      RETURN
      END

c *******************************************************************
      SUBROUTINE RECCEN(CPHI,CPSI,XC,ZC,LONF,LATF,LR,LOR,NL,NS,CSAM00)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D
      CHARACTER*3600 LABI
      REAL*4 LR(5),LOR(5)

C     THIS ROUTINE COMPUTES ZC, THE LINE INTERCEPTING THE EQUATOR,
C           CPSI, THE LONGITUDE AT SAMPLE 1,
C           CSAM00, THE SAMPLE OF LONGITUDE ZERO

      PIFAC=PI/180.
      IF(LONF+LATF.EQ.0)GO TO 100

C      IF HERE, CPHI & CPSI ARE LAT & LONG AT XC & ZC

      IF(XC.EQ.1.0 .AND.CPHI.EQ.0.)GO TO 50
C      GET WEST LONGITUDE AT SAMPLE 1
    2 CPSI=F*(XC-1.)/REQ/PIFAC+CPSI
      XC=1.
      CPSI=AMOD(720.+CPSI,360.)
C      FIND LINE OF EQUATOR
      ZC=ZC+(CPHI*PIFAC*REQ)/F
C       ROUND OFF
      ZC=FLOAT(INT(ZC+0.5))
      CPHI=0.
C      CALCULATE SAMPLE OF LONGITUDE ZERO
50    CONTINUE
      CSAM00=REQ*CPSI*PIFAC/F+1.
C       ROUND OFF
      CSAM00=FLOAT(INT(CSAM00+0.5))
C      ADJUST LONGITUDE OF SAMPLE 1.
      CPSI=F*(CSAM00-1.)/REQ/PIFAC
      RETURN
C       IF HERE NO LATITUDE OR LONGITUDE WAS SPECIFIED
C       PUT MIDDLE OF INPUT IN MIDDLE OF OUTPUT
100   CPHI=LR(3)
      CPSI=LOR(3)
      ZC=(NL+1)/2
      XC=(NS+1)/2
      GO TO 2
      END

c ******************************************************************
      SUBROUTINE LBL0(LR,LOR,ZREF,XREF,A,RS,OM,CL,CS,NLI,NSI,RSVECTOR)
C     THIS ROUTINE FILLS LR & LOR & ZREF & XREF ARRAYS FOR NOPROJ PIX
C     THESE ARRAYS ARE USEFUL IN DEFAULT SCALE AND NORTH ANGLE COMPS
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      common/ippcox/e,pts  
      real*4 pts(3)
      real*8 e(9),RSVECTOR(3)
      REAL*8 D,OM(3,3)
      REAL*4 A(8),LR(5),LOR(5),XREF(5),ZREF(5),RS(3)
      CHARACTER*3600 LABI
      CHARACTER*78 CENMSG
      DATA CENMSG/' COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =      , LONGITUDE =       '/
      ZREF(1)=1.
      XREF(1)=1.
      ZREF(2)=1.
      XREF(2)=NSI
      ZREF(3)=(NLI+1)/2
      XREF(3)=(NSI+1)/2
      ZREF(4)=NLI
      XREF(4)=1.
      ZREF(5)=NLI
      XREF(5)=NSI
      DO 100 I=1,5
       call ippcov(rlat,rlon,zref(i),xref(i),pts,rsvector,om,e,cl,cs,
     &       flag)
C          CALL IPPCOR(RLAT,RLON,ZREF(I),XREF(I),A,RS,OM,CL,CS,FLAG)
          LR(I)=RLAT
          LOR(I)=RLON
          IF(RLAT.NE.FLAG)LR(I)=RLAT*180./PI
          IF(RLON.NE.FLAG)LOR(I)=AMOD(360.-RLON*180./PI,360.)
100   CONTINUE
      IF (LR(3).LT.100.) WRITE (CENMSG(53:58),'(F6.2)') LR(3)
      IF (ABS(LOR(3)).LT.1000.) WRITE (CENMSG(73:78),'(F6.2)') LOR(3)
      CALL XVMESSAGE(CENMSG(2:78),' ')
      RETURN
      END

c ********************************************************
      SUBROUTINE LBL71(INOL,LR,LOR,ZREF,XREF,A,RS,OM,CL,CS,
     &ALTI,PWIDT,PHEYT,VA,SLA,PHAR,PIID,RSVECTOR)

C     THIS ROUTINE CALCULATES SUB-RETICLE POINT(PLANETARY IMAGE
C     OF RETICLE)FOR THE FIVE PRINCIPLE RETICLES
C     (CENTER AND THE FOUR CORNERS)

C     ILATP  LATITUDE OF SUB-RETICLE PT
C     ILONP  LONGITUDE OF SUB-RETICLE PT

      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      common/ippcox/e,pts
      double precision e(9),RSVECTOR(3),d,om(3,3)
      real*4 pts(3),F,FL,REQ,PI,FLAG,A(8),LR(5),LOR(5),ZREF(5),
     +     XREF(5),RS(3),VA,SLA,PHAR,rlat,rlon,cl,cs
      INTEGER*4 ALTI,PWIDT,PHEYT,INOL
      CHARACTER*16 PIID
      CHARACTER*78 CENMSG
      CHARACTER*3600 LABI
      INTEGER ILATP(5),ILONP(5)
      DATA CENMSG/' COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =      , LONGITUDE =       '/
      DATA ILATP/469,475,454,481,487/,ILONP/397,403,382,409,415/

      DO 78 I=1,5
         call ippcov(rlat,rlon,zref(i),xref(i),pts,rsvector,om,e,cl,cs,
     &       flag)
         LR(I)=RLAT
         LOR(I)=RLON
         IF(RLAT.NE.FLAG)LR(I)=RLAT*180./PI
         IF(RLON.NE.FLAG)LOR(I)=AMOD(360.-RLON*180./PI,360.)
         IF(INOL.NE.0)GO TO 78
         WRITE (LABI(ILATP(I)-4:ILATP(I)),'(F5.1)') RLAT*180./PI
         IF(RLAT.NE.FLAG)GO TO 76
         DO 75 J=1,5
75         LABI(ILATP(I)-J+1:ILATP(I)-J+1)='*'
76       WRITE (LABI(ILONP(I)-4:ILONP(I)),'(F5.1)') 
     +         AMOD(720.-RLON*180./PI,360.)
         IF(RLON.NE.FLAG)GO TO 78
         DO 77 J=1,5
77          LABI(ILONP(I)-J+1:ILONP(I)-J+1)='*'
78    CONTINUE
      IF (RLAT.NE.FLAG) WRITE (CENMSG(53:58),'(F6.2)') RLAT*180./PI
      IF (RLON.NE.FLAG) WRITE (CENMSG(73:78),'(F6.2)') 
     +AMOD(720.-RLON*180./PI,360.)
      CALL XVMESSAGE(CENMSG(2:78),' ')
      IF(INOL.NE.0)GO TO 100
      WRITE (LABI(221:225),'(I5)') ALTI
      WRITE (LABI(254:257),'(I4)') PWIDT
      WRITE (LABI(276:279),'(I4)') PHEYT
      WRITE (LABI(307:310),'(F4.1)') VA
      WRITE (LABI(332:335),'(F4.1)') SLA
      WRITE (LABI(350:353),'(F4.1)') PHAR
C        INSERT PICTURE IDENT (REV/CAM/PIC.NO./INTEREST GP)
      LABI(84:86) = PIID(1:3)
      LABI(88:89) = PIID(5:6)
      LABI(90:90) = '/'
      LABI(91:93) = PIID(7:9)
      LABI(160:160)=' '
      LABI(161:165) = PIID(10:14)
100   RETURN
      END

c ***********************************************************
      SUBROUTINE LBL73(INOL,LR,LOR,ZREF,XREF,A,RS,OM,CL,CS,
     &     ALTI,PWIDT,PHEYT,VA,SLA,PHAR,RSVECTOR)
c
C     MVM73 MISSION
c
      implicit none

      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      common/ippcox/e,pts  

      double precision e(9),RSVECTOR(3),d,om(3,3)
      real*4 ra,rb,rc,rlora
      REAL*4 F,FL,REQ,PI,FLAG,A(8),LR(5),LOR(5),ZREF(5),XREF(5)
      REAL*4 RS(3),VA,SLA,PHAR,pts(3),cl,cs,rlat,rlon
      INTEGER*4 ALTI,PWIDT,PHEYT,INOL,ILATP(5),ILONP(5),MAXLAB,LONG
      integer i,il,ill,ill2,j
c      
      CHARACTER*3600 LABI
      character*1 EL
      character*720 MVM73L
      CHARACTER*113 CENMSG
      CHARACTER*6 CENTER
      CHARACTER*72 COL4,COL5,COL6,COL7,COL8,COL9,COL10,COL11,COL12
c
      EQUIVALENCE (COL4 ,MVM73L(1:)),(COL5 ,MVM73L(73:)),
     *   (COL6 ,MVM73L(145:)),(COL7 ,MVM73L(217:)),(COL8 ,MVM73L(289:)),
     *   (COL9 ,MVM73L(361:)),(COL10,MVM73L(433:)),(COL11,MVM73L(505:)),
     *   (COL12,MVM73L(577:))
c
      data MAXLAB/50/,LONG/9/
      data EL/'L'/,CENTER/'CENTER'/
      DATA CENMSG/' COORDINATES OF CENTER OF INPUT PICTURE---  '/
      DATA COL4/'XXXXXXX X       SCET  YR=XX DAY=XXX GMT=XX/XX/XX                      C'/
      DATA COL5/'CENTER  SL.RANGE=XXXXXXXX KM   KM/LINE=XXX.XX   KM/SAMPLE=XXX.XX       C'/
      DATA COL6/'      PHASE ANG=XXX.X DEG   VIEW ANG=XX.X DEG ILLUM ANG=XX.X DEG       C'/
      DATA COL7/'        LONGITUDE=XXXX.X    LATITUDE=XXXX.X                           C'/
      DATA COL8/'CORNERS LONGITUDE=XXXX.X(UL)  XXXX.X(UR)   XXXX.X(LL)  XXXX.X(LR)     C'/
      DATA COL9/'        LATITUDE =XXXX.X(UL)  XXXX.X(UR)   XXXX.X(LL)  XXXX.X(LR)     C'/
      DATACOL10/'SUBSOLAR PIXEL S=XXXXX, L=XXXXX  SUBSPACECRAFT PIXEL S=XXXXX L=XXXXX   C'/
      DATACOL11/'APPROX IMAGE SIZE  WIDTH=XXXXXXX KM  HEIGHT=XXXXXXX KM                C'/
      DATACOL12/'                                                                      C'/
      DATA ILATP/600,612,475,625,638/,ILONP/528,540,456,553,566/
c===================================================================

      IF(INOL.GT.0)GO TO 600
c
C     IF THERE ARE LESS THAN FIVE LABELS ASSUME MVM73 SHORT FORM LABEL
      IF(LABI(216:216).eq.EL .OR. LABI(288:288).eq.EL)GO TO 500
c
C     THERE ARE AT LEAST FIVE LABELS.
C     CHECK IF FIFTH LABEL BEGINS WITH 'CENTER'
C     IF IT DOES THEN ASSUME MVM71 LONG FORM LABEL
C     IF NOT , EXPAND LABEL SET AND INSERT MVM73 LONG FORM LABEL
      IF(CENTER(1:6).eq.LABI(289:294))GO TO 600
c
C     FIND LAST LABEL
 500  DO 510 I=1,MAXLAB
         IL=I
         IF(LABI(I*72:I*72).eq.EL)GO TO 520
 510  CONTINUE
C     LAST LABEL NOT FOUND
      CALL XVMESSAGE(' LAST LABEL NOT FOUND',' ')
      CALL MABEND(' ')
c
C     TERMINATE IF MORE THAN(MAXLAB-4-LONG)LABELS
 520  IF(IL.GT.MAXLAB-4-LONG)CALL XVMESSAGE(' TOO MANY LABELS',' ')
      IF(IL.GT.MAXLAB-4-LONG)CALL MABEND(' ')
      IF(IL.LE.3)MVM73L(LONG*72:LONG*72)=EL
      IF(IL.LE.3)GO TO 550
c
C     EXPAND LABEL
      ILL=72*(IL-3)
      ill2 = 217+72*LONG
      LABI(ill2:ill2+ill-1) = LABI(217:216+ill)
c
C     INSERT MVM73 LONG FORM LABEL
 550  LABI(217:216+LONG*72) = MVM73L(1:long*72)
c
 600  DO 78 I=1,5
         call ippcov(rlat,rlon,zref(i),xref(i),pts,rsvector,om,e,cl,cs,
     *        flag)
         LR(I)=RLAT
         LOR(I)=RLON
         IF(RLAT.NE.FLAG)LR(I)=RLAT*180./PI
         IF(RLON.NE.FLAG)LOR(I)=AMOD(360.-RLON*180./PI,360.)
         IF(INOL.NE.0)GO TO 78
         WRITE (LABI(ILATP(I)-5:ILATP(I)),'(F6.1)') RLAT*180./PI
         IF(RLAT.NE.FLAG)GO TO 76
         DO 75 J=1,5
 75         LABI(ILATP(I)-J+1:ILATP(I)-J+1)='*'
 76      WRITE (LABI(ILONP(I)-5:ILONP(I)),'(F6.1)') 
     +           AMOD(720.-RLON*180./PI,360.)
         IF(RLON.NE.FLAG)GO TO 78
         DO 77 J=1,5
 77         LABI(ILONP(I)-J+1:ILONP(I)-J+1)='*'
 78   CONTINUE
      IF(INOL.NE.0)GO TO 79
      CENMSG(42:41+72) = LABI(433:432+72)
      CALL XVMESSAGE(CENMSG(2:113),' ')
      WRITE (LABI(746:752),'(I7)') PWIDT
      WRITE (LABI(765:771),'(I7)') PHEYT
      WRITE (LABI(398:401),'(F4.1)') VA
      WRITE (LABI(417:420),'(F4.1)') SLA
      WRITE (LABI(377:381),'(F5.1)') PHAR
      RETURN
 79   CENMSG(42:41+72) = COL7
      WRITE (CENMSG(79:86),'(F8.3)') LR(3)
      WRITE (CENMSG(60:67),'(F8.3)') AMOD(360.+LOR(3),360.)
      CALL XVMESSAGE(CENMSG(2:113),' ')
      RETURN
      END

c ****************************************************************
      SUBROUTINE LBL76(INOL,LR,LOR,A,RS,OM,ALTI,PWIDT,PHEYT,VA,SLA,
     &                 PHAR,RSVECTOR)
      implicit none

      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      COMMON/C2/FOCL,XREF,ZREF,SCALE,CL,CS
      common/ippcox/e,pts  

      double precision e(9),RSVECTOR(3),d,om(3,3)
      real*4 ra,rb,rc,rlora,scale,VA,SLA,PHAR,PWIDT,PHEYT
      REAL*4 F,FL,REQ,PI,FLAG,A(8),LR(5),LOR(5),ZREF(5),XREF(5),
     &     RS(3),pts(3),rlat,rlon,cl,cs,focl
      INTEGER*4 ALTI,INOL,ILATP(5),ILONP(5)
      integer i,j,nint
      CHARACTER*78 CENMSG
      CHARACTER*3600 LABI
      data ILATP/603,615,591,627,639/,ILONP/675,687,663,699,711/
      DATA CENMSG/' COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =      , LONGITUDE =       '/
c
C     FILL IN RETICLE LATITUDES & LONGITUDES
c
      INOL=100
      DO 100 I=1,5
         call ippcov(rlat,rlon,zref(i),xref(i),pts,rsvector,om,e,cl,cs,
     *         flag)
         LR(I)=RLAT
         LOR(I)=RLON
         IF(RLAT.NE.FLAG)LR(I)=RLAT*180./PI
         IF(RLON.NE.FLAG)LOR(I)=AMOD(360.-RLON*180./PI,360.)
         IF(INOL.NE.0)GO TO 100
         WRITE (LABI(ILATP(I)-5:ILATP(I)),'(F6.2)') RLAT*180./PI
         IF(RLAT.NE.FLAG)GO TO 101
         DO 102 J=1,6
             LABI(ILATP(I)-J+1:ILATP(I)-J+1)='*'
 102     CONTINUE
 101     WRITE (LABI(ILONP(I)-5:ILONP(I)),'(F6.2)') 
     +        AMOD(720.-RLON*180./PI,360.)
         IF(RLON.NE.FLAG)GO TO 100
         DO 104 J=1,6
            LABI(ILONP(I)-J+1:ILONP(I)-J+1)='*'
 104     CONTINUE
 100  CONTINUE
c
      IF(INOL.NE.0)GO TO 200
      WRITE (LABI(310:313),'(I4)') NINT(PWIDT)
      WRITE (LABI(325:328),'(I4)') NINT(PHEYT)
      WRITE (LABI(469:470),'(I2)') NINT(VA)
      WRITE (LABI(455:456),'(I2)') NINT(SLA)
      WRITE (LABI(509:511),'(I3)') NINT(PHAR)
      CENMSG(53:58) = LABI(ILATP(3)-5:ILATP(3))
      CENMSG(73:72+6) = LABI(ILONP(3)-5:ILONP(3))
      CALL XVMESSAGE(CENMSG(2:),' ')
      RETURN
 200  IF (LR(3).LT.100.) WRITE (CENMSG(53:58),'(F6.2)') LR(3)
      IF (ABS(LOR(3)).LT.1000.) WRITE (CENMSG(73:78),'(F6.2)') LOR(3)
      CALL XVMESSAGE(CENMSG(2:),' ')
      RETURN
      END

c *****************************************************************
      SUBROUTINE LBL79(INOLX,LR,LOR,A,RS,OM,ALTI,PWIDT,PHEYT,VA,SLA,
     &PHAR,RSVECTOR)
      implicit none

      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      COMMON/C2/FOCL,XREF,ZREF,SCALE,CL,CS
      common/ippcox/e,pts

      double precision e(9),RSVECTOR(3),d,om(3,3)
      REAL*4 F,FL,REQ,PI,FLAG,A(8),LR(5),LOR(5),ZREF(5),XREF(5),
     &     RS(3),VA,SLA,PHAR,FOCL,pts(3),rlat,rlon,cl,cs,PWIDT,PHEYT
      real*4 ra,rb,rc,rlora,scale,inolx
      INTEGER*4 ALTI,INOL,ILATP(5),ILONP(5),i,j
c
      CHARACTER*3600 LABI
      CHARACTER*78 CENMSG
      data ILATP/942,950,974,958,966/,ILONP/1014,1022,1046,1030,1038/
      DATA CENMSG/' COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =      , LONGITUDE =       '/
c
C     FILL IN RETICLE LATITUDES & LONGITUDES
C     DISABLE LABEL UPDATE CAPABILITY
      INOL=1
      DO 100 I=1,5
         call ippcov(rlat,rlon,zref(i),xref(i),pts,rsvector,om,e,cl,cs,
     *       flag)
         LR(I)=RLAT
         LOR(I)=RLON
         IF(RLAT.NE.FLAG)LR(I)=RLAT*180./PI
         IF(RLON.NE.FLAG)LOR(I)=AMOD(360.-RLON*180./PI,360.)
         IF(INOL.NE.0)GO TO 100
         WRITE (LABI(ILATP(I)-5:ILATP(I)),'(F6.2)') RLAT*180./PI
         IF(RLAT.NE.FLAG)GO TO 101
         DO 102 J=1,6
 102        LABI(ILATP(I)-J+1:ILATP(I)-J+1)='*'
 101     WRITE (LABI(ILONP(I)-5:ILONP(I)),'(F6.2)') 
     +           AMOD(720.-RLON*180./PI,360.)
         IF(RLON.NE.FLAG)GO TO 100
         DO 104 J=1,6
 104        LABI(ILONP(I)-J+1:ILONP(I)+1)='*'
 100  CONTINUE
      IF(INOL.NE.0)GO TO 200
      WRITE (LABI(828:831),'(I4)') NINT(PWIDT)
      WRITE (LABI(850:853),'(I4)') NINT(PHEYT)
      WRITE (LABI(761:762),'(I2)') NINT(VA)
      WRITE (LABI(736:737),'(I2)') NINT(SLA)
      WRITE (LABI(782:784),'(I3)') NINT(PHAR)
      CENMSG(53:52+6) = LABI(ILATP(3)-5:ILATP(3))
      CENMSG(73:72+6) = LABI(ILONP(3)-5:ILONP(3))
      CALL XVMESSAGE(CENMSG(2:),' ')
      RETURN
 200  IF (LR(3).LT.100.) WRITE (CENMSG(53:58),'(F6.2)') LR(3)
      IF (ABS(LOR(3)).LT.1000.) WRITE (CENMSG(73:78),'(F6.2)') LOR(3)
      CALL XVMESSAGE(CENMSG(2:),' ')
      RETURN
      END

c ********************************************************************
      SUBROUTINE CYLCEN(CPHI,CPSI,XC,ZC,LONF,LATF,LR,LOR,NL,NS,CSAM00)
      implicit none

      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI

      REAL*8 D
      REAL*4 LR(5),LOR(5),csam00,xc,zc,cphi,cpsi,pifac,eps,f,fl,req,
     +       phi,sinphi,radius,RA,RB,RC,RLORA,flag,pi
      integer*4 lonf,latf,nl,ns
      CHARACTER*3600 LABI
c
C     THIS ROUTINE COMPUTES ZC, THE LINE INTERCEPTING THE EQUATOR,
C     CPSI = THE LONGITUDE AT SAMPLE 1,
C     AND CSAM00 = THE SAMPLE OF LONG ZERO.
c
      PIFAC=PI/180.
      EPS=SQRT(1.-FL*FL/REQ/REQ)
      IF(LONF+LATF.EQ.0)GO TO 100
c
C     IF HERE, CPHI & CPSI ARE LAT & LON AT XC & ZC
c
      IF(XC.EQ.1. .AND.CPHI.EQ.0.)GO TO 50
C     GET W. LONG AT SAMPLE 1
 2    CPSI=F*(XC-1.)/REQ/PIFAC+CPSI
      XC=1.
      CPSI=AMOD(720.+CPSI,360.)
C     FIND LINE OF EQUATOR
      PHI=CPHI*PIFAC
      SINPHI=SIN(PHI)
      ZC=ZC+SINPHI*RADIUS(PHI)
c
C     ROUND
      ZC=FLOAT(INT(ZC+0.5))
      CPHI=0.
c
C        CALCULATE SAMP OF LONG ZERO AND ROUND
 50   CSAM00=REQ*CPSI*PIFAC/F+1.
      CSAM00=FLOAT(INT(CSAM00+0.5))
c
C     ADJUST LONGITUDE OF SAMPLE 1.
      CPSI=F*(CSAM00-1.)/REQ/PIFAC
      RETURN
C     IF HERE NO LATI OR LONG SPECIFIED---COMPLETE RECENTERING NEEDED
C     THE TECHNIQUE IS SIMPLE---PUT MIDDLE OF INPUT IN MIDDLE OF
C     OUTPUT, THEN PROCEED AS IF ALL THE PARAMETERS HAD BEEN SPECIFIED
c
 100  CPHI=LR(3)
      CPSI=LOR(3)
      ZC=(NL+1)/2
      XC=(NS+1)/2
      GO TO 2
      END

c ****************************************************************
      SUBROUTINE LABEL(IPROJ,LNUM,ZC,XC,CPHI,CPSI,NORTH,PAR1,PAR2,
     &                  LIN1,LIN2,RDATA,IDATA,MP)

      include 'mp_for_defs'
      integer*4 unit,istat
      real*8 mp

      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      COMMON/UNITS/UNIT(14)
      REAL*8 D
      REAL*4 LIN1,LIN2,RDATA(40),NORTH
      INTEGER*4 IDATA(40)
      CHARACTER*3600 LABI

      CALL ZIA(RDATA,40)     ! CLEAR RDATA
      IDATA(39)=IPROJ
      IF(IPROJ.EQ.1)THEN
          CALL XVMESSAGE('PROJECTION IS POLAR ORTHOGRAPHIC',' ')
      ELSE IF(IPROJ.EQ.2)THEN
          CALL XVMESSAGE('PROJECTION IS OBLIQ ORTHOGRAPHIC',' ')
      ELSE IF(IPROJ.EQ.3)THEN
          CALL XVMESSAGE('PROJECTION IS POLAR STEREOGRAPHIC',' ')
      ELSE IF(IPROJ.EQ.4)THEN
          CALL XVMESSAGE('PROJECTION IS OBLIQ STEREOGRAPHIC',' ')
      ELSE IF(IPROJ.EQ.5)THEN
          CALL XVMESSAGE('PROJECTION IS LAMBERT CONFORMAL CONIC',' ')
      ELSE IF(IPROJ.EQ.6)THEN
          CALL XVMESSAGE('PROJECTION SPECIFIED IS MERCATOR  ',' ')
      ELSE IF(IPROJ.EQ.9)THEN
          CALL XVMESSAGE('PROJECTION IS NORMAL CYLINDRICAL',' ')
      ELSE IF(IPROJ.EQ.10)THEN
          CALL XVMESSAGE('PROJECTION IS SIMPLE CYLINDRICAL',' ')
      ELSE IF(IPROJ.EQ.11)THEN
          CALL XVMESSAGE('OBLIQUE SIMPLE CYLINDRICAL PROJECTION ',' ')
      ELSE IF(IPROJ.EQ.12)THEN
          CALL XVMESSAGE('PROJECTION SPECIFIED IS SINUSOIDAL',' ')
      ELSE IF(IPROJ.EQ.13)THEN
          CALL XVMESSAGE('PROJECTION IS OBLIQUE SINUSOIDAL',' ')
      ELSE IF(IPROJ.EQ.14)THEN
          CALL XVMESSAGE('PROJECTION SPECIFIED IS MOLLWEIDE',' ')
      ELSE IF(IPROJ.EQ.15)THEN
          CALL XVMESSAGE('PROJECTION IS TRANSVERSE MERCATOR',' ')
      ENDIF
      RDATA(1)=XC         ! SPECIAL SAMPLE
      RDATA(2)=ZC         ! SPECIAL LINE
      RDATA(3)=CPHI       ! SPECIAL LATITUDE DEGREES
      RDATA(4)=PAR1       ! LAT OF NORTH PARALLEL DEGREES (LAMBERT CASE)
      RDATA(5)=PAR2       ! LAT OF SOUTH PARALLEL DEGREES (LAMBERT CASE)
      RDATA(6)=CPSI       ! SPECIAL LONGITUDE DEGREES
      RDATA(7)=F          ! SCALE KM/PXL
      RDATA(8)=SIGN(1.,CPHI)! VISIBLE POLE +1 FOR NORTH -1 FOR SOUTH
C       SPECIAL CASE FOR LAMBERT
      IF(IPROJ.EQ.5)THEN
          IF(PAR1/PAR2.LT.0.)THEN
              CALL MABEND(
     *       'FOR LAMBERT BOTH PARALLELS MUST ON SAME SIDE OF EQUATOR')
          ENDIF
          IF(PAR1.GE.0.0)THEN
               RDATA(8)=1.
          ELSE
               RDATA(8)=-1.
          ENDIF
      ENDIF
      RDATA(9)=NORTH      ! NORTH ANGLE DEGREES
      RDATA(25)=RC        ! POLAR RADIUS KM
      RDATA(26)=RA        ! EQUATORIAL RADIUS
      CALL PRNT(7,38,RDATA(1),' DATA=.')

      call mp_buf2mpo( rdata, mp, istat)    !builds mp structure.
      if (istat.lt.mp_SUCCESS) call mabend(' error in MP_BUF2MPO')

      CALL XVOPEN(UNIT(1),ISTAT,'OP','WRITE',
     *    'OPEN_ACT','SA','IO_ACT','SA', 'U_FORMAT','HALF',' ')

C...WRITE MP MAP LABEL

      call mp_label_write( mp, unit(1), 'HISTORY', istat)
      if (istat.lt.mp_SUCCESS) call mabend(' error in MP_LABEL_WRITE')
      call mp_label_write( mp, unit(1), 'PROPERTY', istat)
      if (istat.lt.mp_SUCCESS) call mabend(' error in MP_LABEL_WRITE')

      RETURN
      END

c ****************************************************************
      SUBROUTINE LABEL16(IPROJ,slat,slong,zc,xc,nor,rmag,
     &                   cl,cs,scale,focl,rpol,req,rdata,
     &                   idata,mp,iflag)
c For perspective projection only.

      include 'mp_for_defs'
      integer*4 unit,istat
      real*8 mp

      COMMON/UNITS/UNIT(14)
      REAL*4 RDATA(40),nor
      INTEGER*4 IDATA(40)

      call XVMESSAGE('Projection is Perspective.',' ')

c load data buffer
      CALL ZIA(RDATA,40)     ! CLEAR RDATA
      IDATA(39)=IPROJ
      rdata(33)=zc
      rdata(34)=xc
      rdata(35)=nor
      rdata(38)=rmag
      rdata(28)=cl
      rdata(29)=cs
      rdata(30)=scale
      rdata(27)=focl
      rdata(25)=rpol
      rdata(26)=req
      rdata(31)=slat
      rdata(32)=slong

c compute RS & OM matrix
      call momati(dble(rdata(28)),dble(rdata(29)),dble(rdata(33)),
     +            dble(rdata(34)),dble(rdata(30)),dble(rdata(27)),
     +            dble(rdata(32)),dble(rdata(31)),dble(rdata(35)),
     +            dble(rdata(38)),rdata(1),rdata(19))
      if(iflag.eq.1)return

      CALL PRNT(7,14,RDATA(25),' DATA(25-38)=.')

      call mp_buf2mpo( rdata, mp, istat)    !builds mp structure.
      if (istat.lt.mp_SUCCESS) call mabend(' error in MP_BUF2MPO')

      CALL XVOPEN(UNIT(1),ISTAT,'OP','WRITE',
     *    'OPEN_ACT','SA','IO_ACT','SA', 'U_FORMAT','HALF',' ')

C...WRITE MP MAP LABEL

      call mp_label_write( mp, unit(1), 'HISTORY', istat)
      if (istat.lt.mp_SUCCESS) call mabend(' error in MP_LABEL_WRITE')
      call mp_label_write( mp, unit(1), 'PROPERTY', istat)
      if (istat.lt.mp_SUCCESS) call mabend(' error in MP_LABEL_WRITE')

      RETURN
      END

C**********************************************************************
      SUBROUTINE DISTAL(LR,LOR,CLAT,CLONG,IND,slat,slong)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D
      CHARACTER*3600 LABI
      REAL*4 LR(1),LOR(1)

C     FIND A GOOD DEFAULT CENTER OF PROJECTION USING XYZ COORDS
      PIFAC=PI/180.
      X=0.
      Y=0.
      Z=0.
      J=0
      DO I=1,5
         IF(LR(I).NE.FLAG)THEN
            R=RADIUS(LR(I)*PIFAC)
            A=R*SIN(PIFAC*LR(I))
            B=R*COS(PIFAC*LR(I))
            C=COS(PIFAC*LOR(I))
            DD=SIN(PIFAC*LOR(I))
            X=X+B*C
            Y=Y-B*DD
            Z=Z+A
            J=J+1
         ENDIF
      ENDDO
c      IND=-1
      IF(J.EQ.0)then     ! you got to return something if all reticle
         clat=slat+.001   ! points are off planet. sub s/c pt should be
         clong=slong+.001 ! as good as any -jam- 4-feb-1986
         ind=0            ! ADD .001 TO KEEP FROM CRASHING LATER
         RETURN
      endif
      IND=0
      CLONG=ATAN2(-Y,X)/PIFAC
      CLAT=ATAN(Z/SQRT(X*X+Y*Y))/PIFAC
      CLONG=AMOD(360.+CLONG,360.)
      RETURN
      END

c ***************************************************************
      SUBROUTINE MERCEN(CPHI,CPSI,XC,ZC,LONF,LATF,LR,LOR,NL,NS)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,GEODET,GEOCEN,TAN0,PHI0,PHI1,SINPH0,TAN1
      CHARACTER*3600 LABI
      REAL*4 LR(5),LOR(5)

C     THIS ROUTINE COMPUTES CPHI & CPSI, THE LAT-LON AT LINE 1 SAMP 1,
C     TWO CASES CAN ARISE---EITHER LATI AND LONG BOTH UNSPECIFIED OR NOT
C     IF BOTH UNSPECIFIED, RECENTERING IS REQUIRED
C     OTHERWISE, IF XC = ZC = 1. RETURN IMMEDIATELY  (NO WORK TO DO)
C     BUT IF XC OR ZC NOT = 1. AND LATI OR LONG SPECIFIED THEN
C     COMPUTE NEW CPHI AND CPSI AND RESET XC & ZC TO 1.

      PIFAC=PI/180.
      EPS=SQRT(1.-FL*FL/REQ/REQ)
      IF(LONF+LATF.EQ.0)GO TO 100

C     IF HERE, CPHI & CPSI ARE LAT & LON AT XC & ZC

      IF(XC.EQ.1.0 .AND.ZC.EQ.1.)RETURN
C     GET W. LONG AT SAMPLE 1
2     CPSI=F*(XC-1.)/REQ/PIFAC+CPSI
      XC=1.
C     FIND LINE OF EQUATOR
      PHI=GEODET(DBLE(CPHI*PIFAC))
      SINPHI=SIN(PHI)
      ZC=ZC+REQ/F*ALOG(TAN(PI/4.+PHI/2.)*((1.-EPS*SINPHI)/(1.+EPS*SINPHI
     &))**(EPS/2.))
C     PERFORM ITERATIVE SOLUTION FOR LATITUDE OF LINE 1
      TAN0=EXP((ZC-1.)*F/REQ)
      PHI0=2.D0*(DATAN(TAN0)-PI/4.D0)
      IF(PHI0.EQ.0.D0.OR.REQ.EQ.FL)GO TO 30
      I=0
1     SINPH0=DSIN(PHI0)
      TAN1=TAN0*((1.D0+EPS*SINPH0)/(1.D0-EPS*SINPH0))**(EPS/2.D0)
      PHI1=2.D0*(DATAN(TAN1)-PI/4.D0)
      IF(DABS(PHI1-PHI0).LT.1.D-7)GO TO 10
      I=I+1
      IF(I.GT.10)CALL XVMESSAGE('CONVERGENCE FAILURE',' ')
      IF(I.GT.10)CALL ABEND
      PHI0=PHI1
      GO TO 1
10    PHI0=PHI1
      PHI0=GEOCEN(PHI0)
30    CPHI=PHI0/PIFAC
      ZC=1.
      RETURN
C     IF HERE NO LATI OR LONG SPECIFIED---COMPLETE RECENTERING NEEDED
C     THE TECHNIQUE IS SIMPLE---PUT MIDDLE OF INPUT IN MIDDLE OF
C     OUTPUT, THEN PROCEED AS IF ALL THE PARAMETERS HAD BEEN SPECIFIED

100   CPHI=LR(3)
      CPSI=LOR(3)
      ZC=(NL+1)/2
      XC=(NS+1)/2
      GO TO 2
      END


c ***************************************************************
      SUBROUTINE ORTCEN(CPSI,CPHI,APSI,APHI,XARB,ZARB,NORTH,XC,ZC)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,COSPHI,SINPHI,COSPSI,SINPSI,COSNOR,SINNOR,GEODET
      CHARACTER*3600 LABI
      EQUIVALENCE (SINPHI,D(1)),(COSPHI,D(2)),(SINPSI,D(3)),
     &     (COSPSI,D(4)),(SINNOR,D(5)),(COSNOR,D(6))

C     THIS ROUTINE COMPUTES LINE & SANPLE OF C.P. SO THAT SOME SPECIFIED
C     POINT WILL PROJECT TO (XARB,ZARB) IN AN OBLIQUE ORTHOGRAPHIC PROJ

      PIFAC=PI/180.
      PHIA=CPHI*PIFAC
C     GET GEODETIC LATITUDE OF C.P.---NEEDED BELOW
      PHIA=GEODET(DBLE(PHIA))
      SINPHA=SIN(PHIA)
      COSPHA=COS(PHIA)
      COSLAT=COS(APHI*PIFAC)
      SINLAT=SIN(APHI*PIFAC)
      DLAM=(APSI-CPSI)*PIFAC
      SINLAM=SIN(DLAM)
      COSLAM=COS(DLAM)
      R1=RADIUS(APHI*PIFAC)
      R0=RADIUS(CPHI*PIFAC)
      SINDIF=SIN(PHIA-CPHI*PIFAC)
      X=-R1*COSLAT*SINLAM
      Z=R1*(SINPHA*COSLAT*COSLAM-COSPHA*SINLAT)-R0*SINDIF
      XPR=X*COSNOR-Z*SINNOR
      ZPR=X*SINNOR+Z*COSNOR
      XC=XARB-XPR
      IF(XC.EQ.0.)GO TO 1200
      I=XC+SIGN(.5,XC)
      XC=I
1200  ZC=ZARB-ZPR
      IF(ZC.EQ.0.)RETURN
      I=ZC+SIGN(.5,ZC)
      ZC=I
      RETURN
      END
      REAL FUNCTION RADIUS(RLAT)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D
      CHARACTER*3600 LABI

C     FUNCTION VALUE IS RADIUS OF TARGET AT LATITUDE RLAT (IN RADIANS)
C      ALLOWING FOR FLATTENING...

      RFL=(REQ/FL)**2
      RADIUS=REQ/(F*SQRT(RFL+(1.-RFL)*COS(RLAT)**2))
      RETURN
      END

c ***************************************************************
      SUBROUTINE STRCEN(CPSI,CPHI,APSI,APHI,XARB,ZARB,NORTH,XC,ZC)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,COSNOR,SINNOR,GEODET
      CHARACTER*3600 LABI

C     THIS ROUTINE PERFORMS SAME FUNCTION AS ORTCEN, BUT FOR
C     THE OBLIQUE STEREOGRAPHIC PROJECTION
C     THE EQUATIONS USED ARE EXACT

      EQUIVALENCE (SINNOR,D(5)),(COSNOR,D(6))
      PIFAC=PI/180.
      PHIA=CPHI*PIFAC
C     GEODETIC LATITUDE OF CPHI NEEDED BELOW
      PHIA=GEODET(DBLE(PHIA))
      SINPHA=SIN(PHIA)
      COSPHA=COS(PHIA)
      EPS=SQRT(1.-FL*FL/REQ/REQ)
      R=REQ/SQRT(1.-EPS*EPS*SINPHA*SINPHA)*COSPHA
      Z0=TAN(PI/4.+PHIA/2.)*((1.-EPS*SINPHA)/(1.+EPS*SINPHA))**(EPS/2.)
      SINPHA=(Z0*Z0-1.)/(Z0*Z0+1.)
      COSPHA=2.*Z0/(Z0*Z0+1.)
C     SINPHA & COSPHA NOW REFER TO CONFORMAL LATITUDE CHI ZERO
      R=R/COSPHA/F
C     R NOW EQUALS RADIUS OF SPHERE TO WHICH SPHEROID HAS BEEN
C     CONFORMALLY MAPPED BEFORE BEING CONFORMALLY MAPPED TO PLANE
      DLAM=(APSI-CPSI)*PIFAC
      COSLAM=COS(DLAM)
      SINLAM=SIN(DLAM)
      PHI=APHI*PIFAC
C     GET GEODETIC LATITUDE OF THE SUPPLIED POINT
      PHI=GEODET(DBLE(PHI))
      SINLAT=SIN(PHI)
      Z0=TAN(PI/4.+PHI/2.)*((1.-EPS*SINLAT)/(1.+EPS*SINLAT))**(EPS/2.)
      SINLAT=(Z0*Z0-1.)/(Z0*Z0+1.)
      COSLAT=2.*Z0/(Z0*Z0+1.)
C     WE NOW HAVE CONFORMAL LATITUDE FOR APHI
      DENOM=1.+SINPHA*SINLAT+COSPHA*COSLAT*COSLAM
      X=-2.*R*COSLAT*SINLAM/DENOM
      Z=-2.*R*(COSPHA*SINLAT-SINPHA*COSLAT*COSLAM)/DENOM
      XPR=X*COSNOR-Z*SINNOR
      ZPR=X*SINNOR+Z*COSNOR
      XC=XARB-XPR
      IF(XC.EQ.0.)GO TO 1200
      I=XC+SIGN(.5,XC)
      XC=I
1200  ZC=ZARB-ZPR
      IF(ZC.EQ.0.)RETURN
      I=ZC+SIGN(.5,ZC)
      ZC=I
      RETURN
      END

c ***************************************************************
      SUBROUTINE INIT(CPHI,CPSI,NORTH)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,COSLAT,COSLON,SINLAT,SINLON,COSNOR,SINNOR
      CHARACTER*3600 LABI
      EQUIVALENCE(SINLAT,D(1)),(COSLAT,D(2)),(SINLON,D(3)),(COSLON,D(4))
     +,(SINNOR,D(5)),(COSNOR,D(6))
      REAL NORTH
      D(1)=DSIN(DBLE(CPHI*PI/180.))
      D(2)=DCOS(DBLE(CPHI*PI/180.))
      D(3)=DSIN(DBLE(CPSI*PI/180.))
      D(4)=DCOS(DBLE(CPSI*PI/180.))
      D(5)=DSIN(DBLE(NORTH*PI/180.))
      D(6)=DCOS(DBLE(NORTH*PI/180.))
      RETURN
      END

c ***************************************************************
      SUBROUTINE PSTRCN(CPSI,CPHI,APSI,APHI,XARB,ZARB,XC,ZC)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,GEODET
      CHARACTER*3600 LABI
C     THIS ROUTINE COMPUTES THE LINE & SAMPLE OF THE POLE GIVEN
C     THE LINE AND SAMPLE OF A POINT, ITS COORDINATES, AND THE UP-
C     LONGITUDE---FOR POLAR STEREOGRAPHIC PROJECTION
C     IT USES EQUATIONS FROM RICHARDUS & ADLER, P. 165
      CAS=SIGN(1.,CPHI)
      PIFAC=PI/180.
      DLAM=(APSI-CPSI)*PIFAC
      SINLAM=SIN(DLAM)
      COSLAM=COS(DLAM)
      PHI=APHI*PIFAC
C     CONVERT TO GEODETIC
      PHI=GEODET(DBLE(PHI))
      SINPHI=SIN(CAS*PHI)
      PHI=PI/4.-CAS*PHI/2.
      EPS=SQRT(1.-FL*FL/REQ/REQ)
      REPS=((1.+EPS*SINPHI)/(1.-EPS*SINPHI))**(EPS/2.)
      RHO=2./F*REQ*(1.+EPS)**((EPS-1.)/2.)*(1.-EPS)**((-1.-EPS)/2.)*
     &TAN(PHI)*REPS
      X=RHO*SINLAM*CAS
      Z=-RHO*COSLAM
      XC=XARB-X
      IF(XC.EQ.0.)GO TO 1200
      I=XC+SIGN(.5,XC)
      XC=I
1200  ZC=ZARB-Z
      IF(ZC.EQ.0.)RETURN
      I=ZC+SIGN(.5,ZC)
      ZC=I
      RETURN
      END

c ***************************************************************
      INTEGER FUNCTION LAMPAR(ISCA,ILI1,ILI2,ISAM)
C     RETURNS CASE NUMBER FOR LAMBERT CONFORMAL PARAMETER
C     EVALUATION. CASES =1-11 DEPEND ON WHICH PARAMETERS HAVE
C     BEEN SPECIFIED...

      INTEGER*4  ISCA,ILI1,ILI2,ISAM,MAP(16),LFUN
      logical*1 Q(4)
      data MAP/1,5,4,11,3,10,9,0,2,8,7,0,6,3*0/,LFUN/0/

      Q(1)=(ISCA.NE.0)
      Q(2)=(ILI1.NE.0)
      Q(3)=(ILI2.NE.0)
      Q(4)=(ISAM.NE.0)
      DO 10 I=1,4
      IF(Q(I))LFUN=LFUN+2**(4-I)
   10 CONTINUE
      LAMPAR=MAP(LFUN+1)
      IF(LAMPAR.EQ.0)CALL XVMESSAGE('PROJECTION OVERSPECIFIED',' ')
      IF(LAMPAR.EQ.0)CALL ABEND
      CALL PRNT(4,1,LAMPAR,' LAMBERT CASE =.')
      RETURN
      END

C     ******************************************************************
      SUBROUTINE ROT8(I,A,B)
C     IDENTIFICATION
C         ROT8/EIGHT ROTATION MATRICES
C         J.R. FOSTER
C         FORTRAN IV/1108
C         10/9/69
C     PURPOSE
C         THIS SUBROUTINE FORMS EIGHT ROTATION MATRICES
C     METHOD
C         THE EIGHT MATRICES HAVE BASIC FORMS

C                  COS B  -SIN B  0
C         (1,B) =  SIN B  COS B  0
C                  0      0       1

C                  1      0       0
C         (2,B) =  0      SIN B  -COS B
C                  0      COS B   SIN B

C                  COS B  0       SIN B
C         (3,B) =  0      1       0
C                 -SIN B  0       COS B
C         THE EIGHT MATRICES ARE
C         1)  (1,B+PI/2)
C         2)  (2,B)
C         3)  (1,B+PI)
C         4)  (2,B+PI/2)
C         5)  (1,B)
C         6)  (1,-B)
C         7)  (3,B)
C         8)  (3,-B)
C         9)  (2,PI/2-B)
C     USE
C         CALLING SEQUENCE   CALL ROT8(I,B,A)
C         I  FLAG TO INDICATE THE MARIX TO BE FORMED
C         B  ANGLE
C         A  ROTATION MATRIX

      REAL PI
      DIMENSION B(3,3)
      DATA PI/3.1415927/

      DO 10 J=1,3
      DO 10 K=1,3
10    B(J,K)=0.
      GO TO(1,2,3,4,5,6,7,8,9),I
  1   C=A+PI/2.
      GO TO 70
  2   C=A
      GO TO 80
  3   C=A+PI
      GO TO 70
  4   C=A+PI/2.
      GO TO 80
  5   C=A
      GO TO 70
  6   C=-A
      GO TO 70
    7 C=A
      GO TO 90
    8 C=-A
      GO TO 90
 9    C=PI/2.-A
      GO TO 80
  70  B(1,1)=COS(C)
      B(2,1)=SIN(C)
      B(2,2)=B(1,1)
      B(1,2)=-B(2,1)
      B(3,3)=1.
      RETURN
  80  B(1,1)=1.
      B(2,2)=SIN(C)
      B(3,2)=COS(C)
      B(2,3)=-B(3,2)
      B(3,3)=B(2,2)
      RETURN
   90 B(1,1)=COS(C)
      B(1,3)=SIN(C)
      B(3,1)=-B(1,3)
      B(3,3)=B(1,1)
      B(2,2)=1.
      RETURN
      END

C     ******************************************************************
      FUNCTION ATAN3(A,B)
C     IDENTIFICATION
C         ATAN3/ARCTANGENT
C         AUTHOR
C         FORTRAN IV/1108
C         10/10/69
C     PURPOSE
C         TO DETERMINE THE ARCTANGENT OF A DIVIDED BY B
C     USE
C         ANG=ATAN3(A,B)
C         ANG  THE ANGLE WHOSE TANGENT IS A DIVIDED BY B   0 TO 360

      REAL PI
      REAL TWOPI
      DATA PI/3.1415927/
      DATA TWOPI/6.2831853/
      IF(B.NE.0.)GO TO 1000
      C=SIGN(PI/2.,A)
      GO TO 1010
 1000 C=ATAN2(A,B)
 1010 IF(C.LT.0.)C=C+TWOPI
      ATAN3=C
      RETURN
      END

c ***************************************************************
      SUBROUTINE PEGCAL(LAT,LONG,RS,RRP5,NORTH,OM)
      REAL*4 LAT,LONG,T(3),TP(3),R1(3,3),R2(3,3),R3(3,3),NORTH
      REAL*4 RY(3,3),RS(3),RRP5(3)
      REAL*8 OM(3,3)
      REAL*4 V(3)
      REAL MAKROT
      DO 10 J=1,3
10    V(J)=RRP5(J)-RS(J)
C  ROTATE IN X-Y PLANE
      S1=MAKROT(V(1),V(2),R1,1,2,3)

C  ROTATE IN X-Z PLANE
      S2=MAKROT(V(3),-S1,R2,1,3,2)

C  R3 ROTATES P INTO Z-AXIS
      CALL MMUL(R2,R1,R3)
      T(1)=-SIN(LAT)*COS(LONG)
      T(2)=-SIN(LAT)*SIN(LONG)
      T(3)=COS(LAT)
      CALL VMUL(T,R3,TP)
      YANG=ATAN2(TP(2),TP(1))-NORTH
      RY(2,2)=COS(YANG)
      RY(1,1)=COS(YANG)
      RY(1,2)=SIN(YANG)
      RY(2,1)=-SIN(YANG)
      RY(3,3)=1.
      RY(3,1)=0.
      RY(1,3)=0.
      RY(3,2)=0.
      RY(2,3)=0.
      CALL MMUL(RY,R3,R1)
      DO 20 J=1,3
      DO 20 I=1,3
20    OM(I,J)=R1(I,J)
      RETURN
      END

c ***************************************************************
      SUBROUTINE MATRXI(XJD,XME,ISW,RPOL2)
C     IDENTIFICATION
C         MATRXI/MATRXD TRANSFORMATION MATRIX FORMING ROUTINE
C         D. PIPPEN
C         FORTRAN
C         3/3/70
C     PURPOSE
C         TO FORM AND UPDATE TRANSFORMATION MATRICES NEEDED BY POGASIS
C     USE
C         CALL MATRXI(XJD) - TIME INDEP AND DEP MATRICES ARE FORMED
C         CALL MATRXD(XJD) - ONLY TIME DEP MATRICES ARE UPDATED
C         XJD=JULIAN DATE

      COMMON/C4/THA,HAREF,HART,CRPLE(3),CDPLE(3),COBAR(4),
     &          CIBAR(3),JD1905,JD1950,HARTER
      REAL*8 HAREF,HART,CRPLE,CDPLE,COBAR,CIBAR,HARTER,XJD,HA
      real*8 twopi,radeg,tcen
      REAL AA(3,3),XMEM(3,3),AAT(3,3),TMP(3,3),XMEMT(3,3),XME(3,3)

      DIMENSION RPOL2(2)
C  TARGET BODY HOUR ANGLE REFERENCE EPOCH (JULIAN DATE)
      REAL*8 THA
C  TARGET BODY HOUR ANGLE AT REFERENCE EPOCH  (DEG)
C     HAREF
C  TARGET BODY ROTATIONAL RATE (DEG/DAY)
C     HART
C  OBLIQUITY OF THE ECLIPTIC 1950.0 (DEG)
      REAL*4 OBE50
C  OBLIQUITY OF THE ECLIPTIC AT 'TOBE' (DEG)
      REAL*4 OBEREF
C  COEFICIENTS TO COMPUTE THE OBLIQUITY OF THE ECLIPTIC OF DATE
      REAL*4 cobe(3)
C  COEFFICIENTS TO COMPUTE RIGHT ASCENSION OF TARGET BODY POLE OF DATE
C     CRPLE(2)
C  COEFFICIENTS TO COMPUTE THE DECLINATION OF TARGET BODY POLE OF DATE
C     CDPLE(2)
C  COEFFICIENTS REFERENCED TO 1905 TO COMPUTE LONGITUDE OF ASCENDING
C  NODE OF TARGET BODY ORBIT RELATIVE TO ECLIPTIC AND EQUINOX OF DATE
C     COBAR(4)
C  COEFFICIENTS REFERENCED TO 1905 TO COMPUTE TARGET BODY ORBIT
C  INCLINATION RELATIVE TO ECLIPTIC AND EQUINOX OF DATE
C     CIBAR(3)
C  COEFFICIENTS REFERENCED TO 1950 TO COMPUTE THE 'AA' MATRIX WHICH
C  ROTATES FROM EARTH EQUATOR AND EQUINOX 1950 TO EARTH EQUATOR AND
C  EQUINOX MEAN OF DATE
      REAL*4 CAA(15)
C  CONVERSION FACTOR, SECONDS PER DAY
      REAL*4 SECDAY
c
c real function
      real*4 atan3
      integer j
      INTEGER*2 ISW(2)
c
c  data initialization
      data z/0.0/
      data TWOPI/6.2831853072D0/,RADEG/57.2957795D0/
      data TOBE/2433282.5D0/
      data NJDYR/365.2421988D0/
      data NJDCEN/36525.0D0/
      data DUT /41.95D0/
      data CAA/15*0./
      data COBE/3*0./
      data OBE50/2.3445789E1/
      data OBEREF/23.4457889/
      data SECDAY /86400./

C  CONVERT APPROPRIATE CONSTANTS TO RADIAN MEASURE
      HAREF=HAREF/RADEG
      HART=HART/RADEG
      OBE50=OBE50/RADEG
      OBEREF=OBEREF/RADEG
      DO 20 J=1,2
      CRPLE(J)=CRPLE(J)/RADEG
      CDPLE(J)=CDPLE(J)/RADEG
20    CONTINUE
      DO 30 J=1,3
      COBE(J)=COBE(J)/RADEG
      CIBAR(J)=CIBAR(J)/RADEG
30    CONTINUE
      DO 40 J=1,4
      COBAR(J)=COBAR(J)/RADEG
40    CONTINUE
      TCEN=(XJD-TOBE)/NJDCEN
      OBE=(OBEREF+TCEN*(COBE(1)+TCEN*(COBE(2)+TCEN*COBE(3))))
      T05=(XJD-JD1905)/NJDYR
      RPOLE=CRPLE(1)+CRPLE(2)*T05
      DPOLE=CDPLE(1)+CDPLE(2)*T05
      IF(ISW(1).NE.0)RPOLE=RPOL2(1)/SNGL(RADEG)
      IF(ISW(2).NE.0)DPOLE=RPOL2(2)/SNGL(RADEG)
      OBAR=COBAR(1)+TCEN*(COBAR(2)+TCEN*(COBAR(3)+TCEN*COBAR(3)))
      XIBAR=CIBAR(1)+TCEN*(CIBAR(2)+TCEN*(CIBAR(3)+TCEN*CIBAR(3)))
      CE=COS(OBE)
      SE=SIN(OBE)
      CR=COS(RPOLE)
      SR=SIN(RPOLE)
      CO=COS(OBAR)
      SO=SIN(OBAR)
      CI=COS(XIBAR)
      SI=SIN(XIBAR)
C      Z=ACOS3(CE*SO*CR-CO*SR)
      CALL MABEND(' MATRXI ERROR')
      TEMP1=-CE*CO*CR-SO*SR
      TEMP2=SE*CR
      X=ATAN3(TEMP2,TEMP1)
      TEMP1=CE*SO*SR+CO*CR
      TEMP2=SE*SO
      Y=ATAN3(TEMP2,TEMP1)
      CI=COS(X-XIBAR)
      SI=SIN(X-XIBAR)
      CO=COS(Y-DPOLE)
      SO=SIN(Y-DPOLE)
      CR=COS(Z)
      TEMP1=-CI*CO+SI*SO*CR
      TEMP2=SIN(Z)*SI
      DEL=ATAN3(TEMP2,TEMP1)
C         COMPUTE INCLINATION OF TP EQUAT WITH TP ORBITAL PLANE USED IN
C         CLASSICAL ELEMENTS UPDATING ROUTINE BUT NOT HERE
C      XIT=ACOS3(CI*SO+SI*CO*CR)
      CALL ROT8(3,DEL,XMEM)
      CALL ROT8(2,DPOLE,XMEMT)
      CALL MMUL(XMEMT,XMEM,AAT)
      CALL ROT8(1,RPOLE,XMEM)
      CALL MMUL(XMEM,AAT,XMEMT)
      T50=(XJD-JD1950)/NJDCEN-0.5D0
      T502=T50**2
      T503=T502*T50
      AA(1,1)=1.0+CAA(1)*T502+CAA(2)*T503
      AA(1,2)=CAA(3)*T50+CAA(4)*T502+CAA(5)*T503
      AA(1,3)=CAA(6)*T50+CAA(7)*T502+CAA(8)*T503
      AA(2,1)=-AA(1,2)
      AA(2,2)=1.0+CAA(9)*T502+CAA(10)*T503
      AA(2,3)=CAA(11)*T502+CAA(12)*T503
      AA(3,1)=-AA(1,3)
      AA(3,2)=CAA(11)*T502+CAA(15)*T503
      AA(3,3)=1.0+CAA(13)*T502+CAA(14)*T503

C     AAT EARTH EQUAT AND EQUINOX OF DATE TO EQUAT AND EQUINOX 1950

      CALL mTRANS(AA,AAT)
      CALL MMUL(AAT,XMEMT,XMEM)

C     TMP TRUE OF DATE TARGET EQUAT : PRIME MERIDIAN TO EQUAT : EQUINOX

 1000 HA=HAREF+HART*    (XJD+DUT/SECDAY- THA)
      HA=DMOD(HA,TWOPI)
      HA4 = HA
      CALL ROT8(5,HA4,TMP)

C     XME TRUE OF DATE TARGET EQUAT : PRIME MERIDIAN TO 1950 EARTH EQUAT

      CALL MMUL(XMEM,TMP,XME)
      RETURN
      END

c ***************************************************************
      REAL FUNCTION MAKROT(U,V,R,I,J,K)
      REAL R(3,3)
C  CONSTRUCT ROTATION MATRIX TO ROTATE (U,V)INTO (1,0)
      S=SQRT(U**2+V**2)
      IF(S.NE.0.)GO TO 10
C  DEGENERATE CASE
      S=1.
      U=S
10    R(J,J)=U/S
      R(I,I)=U/S
      R(I,J)=V/S
      R(J,I)=-V/S
      R(K,K)=1.
      R(J,K)=0.
      R(I,K)=0.
      R(K,J)=0.
      R(K,I)=0.
      MAKROT=S
      RETURN
      END

c ***************************************************************
      SUBROUTINE PORTCN(CPSI,CPHI,APSI,APHI,XARB,ZARB,XC,ZC)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D
      CHARACTER*3600 LABI
C     THIS ROUTINE COMPUTES THE LINE & SAMPLE OF THE POLE GIVEN
C     THE LINE & SAMPLE OF A POINT OF GIVEN COORDINATES, & THE UPWARD
C     LONGITUDE FOR A POLAR ORTHOGRAPHIC PROJECTION
      CAS=SIGN(1.,CPHI)
      IF(SIGN(1.,APHI).NE.CAS) CALL XVMESSAGE(
     +     'RECENTERING POINT NOT VISIBLE',' ')
      IF(SIGN(1.,APHI).NE.CAS)CALL ABEND
      PIFAC=PI/180.
      DLAM=(APSI-CPSI)*PIFAC
      COSLAM=COS(DLAM)
      SINLAM=SIN(DLAM)
      COSPHI=COS(APHI*PIFAC)
      R=RADIUS(APHI*PIFAC)
C     R IS NOW IN PIXELS
      X=R*CAS*COSPHI*SINLAM
      Z=-R*COSPHI*COSLAM
      XC=XARB-X
      IF(XC.EQ.0.)GO TO 1200
      I=XC+SIGN(.5,XC)
      XC=I
1200  ZC=ZARB-Z
      IF(ZC.EQ.0.)RETURN
      I=ZC+SIGN(.5,ZC)
      ZC=I
      RETURN
      END

c *************************************************************
      SUBROUTINE GASUB(LINE,SAMP,TAB)
      REAL LINE,SAMP,TAB(8)
      TL=LINE
      TS=SAMP
      SAMP=TAB(1)*TS+TAB(2)*TL+TAB(7)*TL*TS+TAB(3)
      LINE=TAB(4)*TS+TAB(5)*TL+TAB(8)*TL*TS+TAB(6)
      RETURN
      END
c *************************************************************
C  Comments for FR 90527 correction.
C
cFrom:	SMTP%"jjl@telescope.JPL.NASA.GOV"  6-NOV-1997 15:57:18.27
c
cSubj:	Re: equation for perspective projection in MAP3
c
cOn Thu, 6 Nov 1997 SXP059@CODA2.JPL.NASA.GOV wrote:
c
c> Hi, 
c>   Last December I delivered MAP3 with the code segment below for
c> perspective projection.  In January, this section was changed slightly,
c> the  ' + rpol' was removed.  AS far as I can tell, the change is incorrect
c> and the code below is correct.  (I did not write this code; I believe it was
c> that way before it was ported.)
c>   I would like some help either (experimental or theoretical) determining
c> if the code below is correct.  Any suggestions?  Even a good test image
c> or an idea for an experiment would be a help.
c> 
c>   I will tell you why I think the code below is correct FYI as background.
c> rmag seems to be used consistently in map3 to represent the magnitude of
c> the RS vector, which is the vector from the target center to the camera.
c> In the perspective projection map3 allows one to "move the camera" closer
c> to the target.  This is done by specifying the SCALE parameter, which is stored
c> in the variable f.  This seems to match the help for perspective projections:
c> 
c> "        SCALE     The scale in km/pixel in the output.
c>                    Defaults to the scale of the input picture."
c> 
c> Here are the variable definitions:
c> isca - flag that is > 0 if SCALE parameter entered,
c> f    - value of SCALE parameter
c> scale - camera scale in pixels/mm
c> focl  - camera focal length in milimeters.
c> rmag  - magnitude of the RS vector, from SEDR or MPlabel or RS or RMAG parameter
c> 
c> rmag_pers gets passed to MOMATI, which appears to interpret it as the
c> distance from the target center to the new camera position.
c> 
c> 
c> 
c> c *** PERSPECTIVE ***
c> 2100  continue
c>       if(isca.gt.0) then         ! recompute range to planet
c>          rmag_pers=f*scale*focl + rpol 
c>       else
c>          rmag_pers=rmag
c>       endif
c> 
c
C   I don't think that an experiment is needed. It depends upon one's
C   interpretation of the meaning of rmag. Since rmag is passed to momati the
C   meaning must be the same as momati's, ie distance from the s/c to the
C   planet center. The user specifies the scale. I would assume he is
C   interested in the scale at the near side of the planet, not the scale at
C   the planet center which he cannot see. 
C   If a camera sees a planet such that a km images on a spot b mm in the
C   image plane then
C   a/b = (rmag-rpol)/focl
C   solving for rmag:
C   rmag=focl*(a/b)+rpol
C   a/b is f*scale 
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create map3a.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE LAMCEN(PAR1,PAR2,CPSI,APSI,APHI,XARB,ZARB,XC,ZC)
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PI,FLAG,LABI
      REAL*8 D,PHI1,PHI2,R1,R2,TAN1,TAN2,DK,DC,RP1,RP2
      REAL*8 EPS,E1,E2,TANA,RHO,EA,PHIA,NRAD,DPHI,GEODET
      LOGICAL*4 EFLAG
      CHARACTER*3600 LABI
      REAL*4 LIN1,LIN2
      EQUIVALENCE (DK,D(1)),(DC,D(2))
      DATA EFLAG/.FALSE./
      NRAD(X,Y,DPHI)=X*X/DSQRT(X*X*DCOS(DPHI)*DCOS(DPHI)+Y*Y*DSIN(DPHI)
     &*DSIN(DPHI))
80    PHI1=DBLE(PAR1)*DBLE(PI)/1.8D2
C     CHANGE YOUR LATITUDES TO GEODETIC FROM GEOCENTRIC
      PHI1=DABS(GEODET(PHI1))
      PHI2=DBLE(PAR2)*DBLE(PI)/1.8D2
      PHI2=DABS(GEODET(PHI2))
      CAS=SIGN(1.,PAR1)
      R1=NRAD(REQ,FL,PHI1)*DCOS(PHI1)
      R2=NRAD(REQ,FL,PHI2)*DCOS(PHI2)
      EPS=DSQRT(1.D0-FL*FL/REQ/REQ)
      E1=((1.D0+EPS*DSIN(PHI1))/(1.D0-EPS*DSIN(PHI1)))**(EPS/2.D0)
      TAN1=DTAN(PI/4.D0-PHI1/2.D0)*E1
      E2=((1.D0+EPS*DSIN(PHI2))/(1.D0-EPS*DSIN(PHI2)))**(EPS/2.D0)
      TAN2=DTAN(PI/4.D0-PHI2/2.D0)*E2
      DK=DLOG(R1/R2)/DLOG(TAN1/TAN2)
      IF(EFLAG)GO TO 81
      EFLAG=.TRUE.
      DC=R1/(DBLE(F)*DK*TAN1**DK)
      PHIA=DBLE(APHI)*DBLE(PI)/1.8D2
      PHIA=GEODET(PHIA)
      PHIA=PHIA*CAS
1000  EA=((1.D0+EPS*DSIN(PHIA))/(1.D0-EPS*DSIN(PHIA)))**(EPS/2.D0)
      TANA=DTAN(PI/4.D0-PHIA/2.D0)*EA
      RHO=DC*TANA**DK
C     FIND DLAM SO THAT -180.)DLAM)+180.
      DLAM=APSI-CPSI
      DLAM=AMOD(720.+DLAM,360.)
      IF(DLAM.GT.180.)DLAM=DLAM-360.
      DLAM=DLAM*PI/180.
      XC=XARB+RHO*DSIN(DK*DLAM)
      IF(XC.EQ.0.)GO TO 1200
      I=XC+SIGN(.5,XC)
      XC=I
1200  ZC=ZARB-CAS*RHO*DCOS(DK*DLAM)
      IF(ZC.EQ.0.)RETURN
      I=ZC+SIGN(.5,ZC)
      ZC=I
      RETURN
      ENTRY GETCK(PAR1,PAR2,LIN1,LIN2,ZPOL,NCASE)

C     NCASE DETERMINES WAY TO CALCULATE DC AND DK CONSTANTS FOR
C     TWO-STANDARD OTHOMORPHIC CONIC (LAMBERT CONFORMAL) PROJECTION
C     FOR NCASE=...      PARAMETERS GIVEN..  CONTROL PASSES TO...
C     1,2                F,CPHI              70
C     3,6                F,LIN1              10
C     4,7                F,LIN2              20
C     5,8                F,ZPOL              30
C     9                  LIN1,LIN2           40
C     10                 LIN1,ZPOL           50
C     11                 LIN2,ZPOL           60


C     PARAMETERS IN THIS SUBROUTINE ARE AS FOLLOWS...
C     PARI=LATITUDE OF PARALLEL I,I=1,2
C     LINI=PIXEL LINE LOCATION OF PARALLELI IN OUTPUT PIX,I=1,2
C     ZPOL=LINE VALUE OF POLE IN OUTPUT PIX
C     PHII=PARI IN RADIANS,I=1,2
C     RI=RADIUS OF PARI IN KM,I=1,2
C     TANI=TAN((90.-PARI)/2),I=1,2
C     DK=CONSTANT K FOR CONE...SEE SUPERMAP WRITE-UP...
C     DC=CONSTANT C FOR CONE
C     NOTE...DK DEPENDS ONLY ON PAR1 AND PAR2
C      BUT DC DEPENDS ON F.
C      LIN1,LIN2,AND ZPOL DEPEND ON DC AND F.IF ONE OF THE THREE IS
C      SPECIFIED,THE OTHERS CAN BE FOUND

C     NOTE...DK CAN ALWAYS BE CALCULATED AS FOLLOWS...

      IF(EFLAG)GO TO 81
      EFLAG=.TRUE.
      GO TO 80
81    GO TO (70,70,10,20,30,10,20,30,40,50,60), NCASE

C     IN THIS CASE, F (KM/PXL) IS SPECIFIED.TO FIND DC, WE NEED
C      RADIUS OF PAR1 IN PIXELS. WE CAN CALCULATE IT IN KM, THEREBY
C      GETTING RESULT BY USING F...

   10 DC=R1/(DBLE(F)*DK*TAN1**DK)
      RP1=R1/(DBLE(F)*DK)
      ZPOL=LIN1-CAS*SNGL(RP1)
      RP2=DC*TAN2**DK
      LIN2=ZPOL+CAS*SNGL(RP2)
      GO TO 100

C     IN THIS CASE,F AND LIN2 ARE SPECIFIED.
C     CALCULATE DC THE SAME AS ABOVE,THEN LIN1,ZPOL

   20 DC=R2/(DBLE(F)*DK*TAN2**DK)
      RP2=R2/(DBLE(F)*DK)
      ZPOL=LIN2-CAS*SNGL(RP2)
      RP1=DC*TAN1**DK
      LIN1=ZPOL+CAS*SNGL(RP1)
      GO TO 100

C     IN THIS CASE, F AND ZPOL ARE SPECIFIED. WE CAN AGAIN CALCULATE
C      DC FROM F, THEN LIN1,LIN2

   30 DC=R1/(DBLE(F)*DK*TAN1**DK)
      RP1=R1/(DBLE(F)*DK)
      RP2=DC*TAN2**DK
      LIN1=ZPOL+CAS*SNGL(RP1)
      LIN2=ZPOL+CAS*SNGL(RP2)
      GO TO 100

C     HERE,WE ARE GIVEN LIN1 AND LIN2.
C     CALCULATE DC FROM EQUATION...ABS(LIN1-LIN2)=RP2-RP1
C                                                =DC*(TAN1**DK-TAN2**DK)

   40 DC=DABS(DBLE(LIN2-LIN1)/(TAN2**DK-TAN1**DK))
      RP1=DC*TAN1**DK
      RP2=DC*TAN2**DK

C     R1 IS IN KM AT THIS POINT

      F=SNGL(R1/RP1*DK)
      ZPOL=LIN1-CAS*SNGL(RP1)
      GO TO 100

C     HERE,WE ARE GIVEN LIN1 AND ZPOL
C     RP1 IS THEREFORE IMMEDIATE

   50 RP1=DBLE(ABS(LIN1-ZPOL))
      DC=RP1/TAN1**DK
      RP2=DC*TAN2**DK
      LIN2=ZPOL+CAS*SNGL(RP2)
      F=SNGL(R1/RP1*DK)
      GO TO 100

C     HERE,LIN2 AND ZPOL ARE SPECIFIED...ANALOGOUS TO ABOVE

   60 RP2=DBLE(ABS(LIN2-ZPOL))
      DC=RP2/TAN2**DK
      LIN1=CAS*SNGL(RP1)+ZPOL
      F=SNGL(R2/RP2*DK)
      GO TO 100
C     THE FOLLOWING CODE IS THE OLD CRUDE CENTERING TECHNIQUE USED WHEN
C     NONE OF LIN1,LIN2,AND LINE WERE SPECIFIED
C     IT WILL BE EXECUTED NOW ONLY IF RECENTERING WAS ALSO UNSPECIFIED
   70 DC=R1/(DBLE(F)*DK*TAN1**DK)
      LIN1=LIN1*PI/180.
      LIN1=GEODET(DBLE(LIN1))
      LIN1=LIN1*CAS
      EA=((1.D0+EPS*SIN(LIN1))/(1.D0-EPS*SIN(LIN1)))**(EPS/2.D0)
      LIN2=TAN(PI/4.-LIN1/2.)*EA
      ZPOL=ZPOL-CAS*SNGL(DC)*LIN2**SNGL(DK)
      RP1=R1/(DBLE(F)*DK)
      RP2=DC*TAN2**DK
      LIN2=ZPOL+CAS*SNGL(RP2)
      LIN1=ZPOL+CAS*SNGL(RP1)
  100 CONTINUE
      RETURN
      END

c *********************************************************************
      SUBROUTINE VECTOR
      REAL R(3),RMAG,LAT,LONG
      REAL A(3),B(3),C(3)
      REAL D(3,3),E(3,3),F(3,3)
      ENTRY      SPHREC(R,RMAG,LAT,LONG)
C  CONVERT SPHERCAL COORDINATES TO RECTANGULAR VECTOR
      R(1)=RMAG*COS(LAT)*COS(LONG)
      R(2)=RMAG*COS(LAT)*SIN(LONG)
      R(3)=RMAG*SIN(LAT)
      RETURN
      ENTRY      RECSPH(R,RMAG,LAT,LONG)
C  CONVERT KECTANGULAR VECTOR COMPONENTS T SPHERICAL
      RMAG=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      LAT=ATAN2(R(3),SQRT(R(1)**2+R(2)**2))
      LONG=ATAN2(R(2),R(1))
      RETURN
      ENTRY      UNIT(R)
C  MAKE R A UNIT VECTOR IN SMAE DIRECTION
      X=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      DO 10 J=1,3
10    R(J)=R(J)/X
      RETURN
      ENTRY      CROSS(A,B,C)
C  VECTOR CROSS PRODUCT C=A X B
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      ENTRY MMUL(D,E,F)
C  3 X 3 MATRIX PRODUCT F=D*E
      DO 20 I=1,3
      DO 15 J=1,3
      S=0.
      DO 12 K=1,3
12    S=S+D(I,K)*E(K,J)
      F(I,J)=S
15    CONTINUE
20    CONTINUE
      RETURN
      ENTRY VMUL(A,D,R)
C        VECTOR (3 X 1)=MATRIX (3 X 3) * VECTOR (3 X 1)  R=D*A
      DO 50 I=1,3
      S=0.
      DO 40 J=1,3
40    S=S+D(I,J)*A(J)
50    R(I)=S
      RETURN

      ENTRY mTRANS(D,E)
C        TRANSPOSE 3 X 3 MATRIX  E=TRANSPOSE(D)
      DO 60 I=1,3
      DO 60 J=1,3
   60 E(J,I)=D(I,J)
      RETURN
      END

c ***************************************************************
      REAL*8 FUNCTION GEODET(PHI)
      REAL*8 PHI,PI/3.141592653589793/,D,APHI,GEOCEN
      COMMON/C1/D(7),F,FL,REQ,RA,RB,RC,RLORA,PIE,FLAG,LABI
      CHARACTER*3600 LABI

C     GIVEN GEOCENTRIC LATITUDE PHI IN RADIANS
C     COMPUTE GEODETIC LATITUDE IN RADIANS

      IF(DABS(PHI).GT.PI/2.D0-1.D-6)GO TO 1
      IF(PHI.EQ.0.D0)GO TO 1
      APHI=PI/2.D0-DABS(DATAN(-FL*FL/REQ/REQ/DTAN(PHI)))
      GEODET=APHI
      IF(PHI.LT.0.D0)GEODET=-APHI
      RETURN
1     GEODET=PHI
      RETURN

C     GIVEN GEODETIC LATITUDE PHI IN RADIANS
C     COMPUTE GEOCENTRIC LATITUDE

      ENTRY GEOCEN(PHI)
      IF(DABS(PHI).GT.PI/2.D0-1.D-6)GO TO 2
      APHI=DABS(PHI)
      APHI=DATAN(-FL*FL/REQ/REQ/DTAN(APHI-PI/2.D0))
      GEOCEN=APHI
      IF(PHI.LT.0.D0)GEOCEN=-APHI
      RETURN
2     GEOCEN=PHI
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create map3.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM map3

   To Create the build file give the command:

		$ vimake map3			(VMS)
   or
		% vimake map3			(Unix)


************************************************************************/
#define PROGRAM	map3

#define MODULE_LIST map3.f map3a.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_ANSI_C


#define FTNINC_LIST mp_for_defs  
#define R2LIB

#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77
/*#define LIB_LOCAL /* remove on delivery */
/************************* End of Imake file ***************************/

$ Return
$!#############################################################################
$PDF_File:
$ create map3.pdf
process help=*
PARM INP      TYPE=STRING  COUNT=(1:2)
PARM OUT      TYPE=STRING  COUNT=1
PARM SIZE     TYPE=INTEGER COUNT=(0:4)                DEFAULT=--
PARM NL       TYPE=INTEGER COUNT=(0:1)                DEFAULT=--
PARM NS       TYPE=INTEGER COUNT=(0:1)                DEFAULT=--
PARM NOSEDR   TYPE=KEYWORD COUNT=(0:1) VALID=NOSEDR   DEFAULT=--
PARM NOINTERP TYPE=KEYWORD COUNT=(0:1) VALID=NOINTERP DEFAULT=--
PARM DNTHRESH TYPE = INTEGER  COUNT=1  DEFAULT=0
PARM PLANET   TYPE=STRING  COUNT=(0:1)                DEFAULT=""
PARM MM71     TYPE=KEYWORD COUNT=(0:1) VALID=MM71     DEFAULT=--
PARM MVM73    TYPE=KEYWORD COUNT=(0:1) VALID=MVM73    DEFAULT=--
PARM VI76     TYPE=KEYWORD COUNT=(0:1) VALID=VI76     DEFAULT=--
PARM VGR1     TYPE=KEYWORD COUNT=(0:1) VALID=VGR1     DEFAULT=--
PARM VGR2     TYPE=KEYWORD COUNT=(0:1) VALID=VGR2     DEFAULT=--
PARM GLL      TYPE=KEYWORD COUNT=(0:1) VALID=GLL      DEFAULT=--
PARM NOPROJEC TYPE=KEYWORD COUNT=(0:1) VALID=NOPROJEC DEFAULT=--
PARM DAS      TYPE=INTEGER COUNT=(0:1)                DEFAULT=--
PARM FDS      TYPE=INTEGER COUNT=(0:1)                DEFAULT=--
PARM FSC      TYPE=INTEGER COUNT=(0:1)                DEFAULT=--
PARM RADII    TYPE=REAL    COUNT=(0:3)                DEFAULT=--
PARM LORA     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM CSCALE   TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM FOCL     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM CSAMP    TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM CLINE    TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM CAMERA   TYPE=INTEGER COUNT=(0:1)                DEFAULT=--
PARM DISTOR   TYPE=KEYWORD COUNT=(0:1)	VALID=(DISTOR,IMAGE,OBJECT) DEFAULT=--
PARM CMATRIX  TYPE=REAL    COUNT=(0:9)                DEFAULT=--
PARM MEMATRIX TYPE=REAL    COUNT=(0:9)                DEFAULT=--
PARM OMMATRIX TYPE=REAL    COUNT=(0:9)    	      DEFAULT=--
PARM VRVECTOR TYPE=REAL    COUNT=(0:3)                DEFAULT=--
PARM RSVECTOR TYPE=REAL    COUNT=(0:3)                DEFAULT=--
PARM SLATITUD TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM SLONGITU TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM RMAGNITU TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM TIME     TYPE=INTEGER COUNT=(0:6)                DEFAULT=--
PARM RTAS     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM DECLINAT TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM QUAM     TYPE=KEYWORD COUNT=(0:1)	VALID=QUAM    DEFAULT=--
PARM CLATITUD TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM CLONGITU TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM NORANGLE TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM ISSCPT   TYPE=REAL    COUNT=(0:2)                DEFAULT=--
PARM OSSCPT   TYPE=REAL    COUNT=(0:2)                DEFAULT=--
PARM MERCATOR TYPE=KEYWORD COUNT=(0:1) VALID=MERCATOR DEFAULT=--
PARM LAMBERT  TYPE=KEYWORD COUNT=(0:1) VALID=LAMBERT  DEFAULT=--
PARM STEREOGR TYPE=KEYWORD COUNT=(0:1) VALID=STEREOGR DEFAULT=--
PARM CYLINDRI TYPE=KEYWORD COUNT=(0:1) VALID=CYLINDRI DEFAULT=--
PARM RECTANGU TYPE=KEYWORD COUNT=(0:1) VALID=RECTANGU DEFAULT=--
PARM ORTHOGRA TYPE=KEYWORD COUNT=(0:1) VALID=ORTHOGRA DEFAULT=--
PARM OBCYLIND TYPE=KEYWORD COUNT=(0:1) VALID=OBCYLIND DEFAULT=--
PARM SINUSOID TYPE=KEYWORD COUNT=(0:1) VALID=SINUSOID DEFAULT=--
PARM OBSINUSO TYPE=KEYWORD COUNT=(0:1) VALID=OBSINUSO DEFAULT=--
PARM MOLLWEID TYPE=KEYWORD COUNT=(0:1) VALID=MOLLWEID DEFAULT=--
PARM TMERCATO TYPE=KEYWORD COUNT=(0:1) VALID=TMERCATO DEFAULT=--
PARM PERSPECT TYPE=KEYWORD COUNT=(0:1) VALID=PERSPECT DEFAULT=--
PARM SOUTH    TYPE=KEYWORD COUNT=(0:1) VALID=SOUTH    DEFAULT=--
PARM POLE     TYPE=KEYWORD COUNT=(0:1) VALID=POLE     DEFAULT=--
PARM SCALE    TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM LINE     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM SAMPLE   TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM LATITUDE TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM LONGITUD TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM PAR1     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM PAR2     TYPE=REAL    COUNT=(0:1)   	      DEFAULT=--
PARM LIN1     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM LIN2     TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM NORTH    TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM PLATITUD TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM PLONGITU TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM PLINE    TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM PSAMPLE  TYPE=REAL    COUNT=(0:1)                DEFAULT=--
PARM RECENTER TYPE=KEYWORD COUNT=(0:1)	VALID=RECENTER DEFAULT=--
PARM NOGEOM   TYPE=KEYWORD COUNT=(0:1)	VALID=NOGEOM  DEFAULT=--
PARM SHORT    TYPE=KEYWORD COUNT=(0:1)	VALID=SHORT   DEFAULT=--
PARM NOLABEL  TYPE=KEYWORD COUNT=(0:1)	VALID=NOLABEL DEFAULT=--
PARM BYTE     TYPE=KEYWORD COUNT=(0:1)  VALID=BYTE    DEFAULT=--
PARM HALF     TYPE=KEYWORD COUNT=(0:1)  VALID=HALF    DEFAULT=--
PARM GEODET   TYPE=KEYWORD COUNT=(0:1)	VALID=GEODET  DEFAULT=--
PARM NAM2     TYPE=KEYWORD COUNT=(0:1)	VALID=NAM2    DEFAULT=--
PARM TIEPTS   TYPE=REAL    COUNT=(0:80)               DEFAULT=--
PARM REFTIME TYPE=INTEGER COUNT=(0,6) DEFAULT=--
PARM ZVP TYPE=STRING COUNT=(0:1)                               +
     default="/project/test_work/testdata/mipl/vgr/jupiter.zvp"
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PRINT      TYPE=INTEGER     COUNT=(0,2)                    DEFAULT=--
END-PROC
.TITLE
VICAR program MAP3 -- performs map projection rubber-sheeting
.HELP
PURPOSE
	MAP3 performs the necessary calculations of a rubber-sheet stretch
 on a picture to convert it from a perspective projection (in which 
 latitudes and longitudes are complicated functions of line and sample
 depending on the position and orientation of the camera) to a standard
 cartographic projection (in which longitude and latitude are simpler
 functions of line and sample that are independent of camera viewing
 geometry).  The following projections are currently implemented:
		Mercator
		Lambert Two-Standard Conformal Conic
		Oblique and Polar Orthographic
		Oblique and Polar Stereographic
		Cylindrical (Normal)
		Simple Cylindrical (Rectangular)
                Transverse Mercator
                Oblique Simple Cylindrical.
                Sinusoidal
                Oblique Sinusoidal
                Mollweid
                Perspective 
.PAGE
 EXECUTION
	      map3 INP OUT SIZE PARAMS
    -or-
	      map3 (INP,GEOM) OUT SIZE PARAMS

    SEE Parameter Section for explanation of parameters.

.page
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels:

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
When specified, the CKID parameter overrides the CKNAME parameter.  If the
camera pointing data is not found in the requested C kernel, the other C kernels
are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific instance of
camera pointing based upon this provenance information:

CDATE specifies the date and time the camera pointing was created.
REQNUM identifies the request number associated with the camera pointing.
PURPOSE identifies the purpose for creating the camera pointing.
PROGRAM identifies the program which created the camera pointing.
SPKID identifies the SP-kernel used to create the camera pointing.
USERID identifies the user who created the camera pointing.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

A complete list of CK and SPK IDs are located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

The above parameters are optional, and if defaulted (or if no data is found for
the requested version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.

           'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

.PAGE
 OPERATION
	MAP3 begins by examining the input picture label to determine if it
contains a map label for a perspective projection.  If so, the program will
not access the SPICE/SEDR and will extract the needed input navigation from
this label.
Then the program checks the label to see if the  frame comes from one of the
standard flights:  Mariner 9, Mariner 10, Viking  Orbiters 1 and 2 Voyager or
Galileo.  If so, and if SPICE/SEDR data is available, camera 
 pointing information and the latitudes and longitudes of the four corners and 
 center of the input frame are read from the SEDR.  Otherwise the parameter 
 list is checked to see if latitude and longitude of the center of the input 
 frame are specified.  The latitudes and longitudes are used only to determine 
 a projection type if the user does not specify one via parameters.  However, 
 the pointing information is vital and must be obtained by MAP3 somehow or it 
 will ABEND.  See VIEWING GEOMETRY PARAMETERS section for details.
	Next, the program mathematically specifies the output projection in
 full.  The required data consists of the projection type and projection-depen-
 dent information such as the center of projection, standard parallels,
 central meridian, angle of north in the output picture, and position in the
 output of the center of projection pole or equator, etc. See Reference (2)
 for a detailed description of which parameters specify which projection.  A
 description of the size and shape of the planet is also required at this
 stage.  MAP3 checks the parameter list for user specifications, then computes
 the remaining parameters (if any) using a set of default algorithms.
	Next, the program checks the parameter list for any user overrides or
 supplements to the camera optical geometry information and viewing geometry
 data.  Then the 'output' picture is spanned by a grid of tiepoints.
 At each point the projection descriptive data determines the latitude and
 longitude which must map there.  The camera optics and viewing geometry data
 are used to compute, from the latitude and longitude, the line and sample of
 the corresponding point in an undistorted (geometrically) input picture.  If
 instructed to do so, MAP3 transforms the undistorted input line and sample
 to a distorted (image space) line and sample.  The result of this process is a
 set of points in the output picture for which the corresponding input
 points are known, whether or not the input frame has been geometrically
 corrected.  Map3 then performs the GEOM operation on the input image to
 form the output image.
        The geom operation is performed upon each grid square independently.
 The operation depends upon the number of vertices of the square which
 fall on the planet:
 1. No vertices on the planet:
	The output is set to zero for this square.
 2. All 4 vertices are on the planet:
	The output is interpolated from the input using a polynomial model to
	map from output to input of the form:
       		line_in=c1*L+c2*s+c3*L*s+c4
       		samp_in=k1*L+k2*s+k3*L*s+k4
 3. One to three vertices are on the planet:
	Compute the exact transformation to map output to input.

.PAGE
 PARAMETERS
	MAP3 is designed to be easy to use, at least for standard flight
 pictures.  But it is also a flexible program performing a complex task.  It
 needs a large amount of auxiliary information concerning planet size and
 shape, camera optics, viewing geometry, and a precise specification of the
 projection desired.  For standard flights, the planetary data and camera
 specifications are built into MAP3.  SEDR's are usually available to provide
 viewing geometry parameters, and the projection description parameters may
 always be defaulted.  But to provide sufficient program flexibility to
 handle non-standard flight pictures, bad or missing SEDR's, and mosaicking
 capability, a very large number of parameters are available.
	Each of the following sections describes some subset of the parameters,
 grouped according to function.  Each section is preceded by an explanatory
 introduction.  The following conventions are observed.  Within each section
 numerical parameters are numbered consecutively.  Parameters required to be
 integers begin with N; floating point parameters begin with R. NOTE:  
ALL LATITUDES ARE  GEOCENTRIC; ALL LONGITUDES ARE WEST.  ALL PARAMETERS ARE
EXPLAINED IN DETAIL  IN THE PARAMS SECTION. 

.PAGE
 FLIGHT RELATED PARAMETERS
	MAP3 normally determines the flight from the picture label. It
 recognizes Mariner 9, Mariner 10, Viking Orbiter, Voyager and Galileo labels.
 If the label is bad or missing, the following parameters can be used to
 specify the flight.
 
          MM71  MVM73  VI76  VGR1  VGR2  GLL  noprojec
 
 If the picture label is not from one of the standard flights and these
 parameters are defaulted, a non-standard flight is assumed, requiring the
 user to enter all necessary information through the parameter list.
                   DAS         FDS          FSC
.PAGE
 PLANET DESCRIPTIVE PARAMETERS
	MAP3 must know the radii of the target body being projected.  In the
 case of Voyager, these are normally obtained from the SEDR.  For other
 flights, or to override the Voyager SEDR values, the following keywords can
 be used.
                    RADII        TARGET         LORA
.PAGE
 CAMERA DESCRIPTIVE PARAMETERS
	This group defines the camera optical system.  All can be defaulted
 for standard flights because correct values are built into MAP3.
 These should be specified (unless there is a default) if NOPROJEC is specified.
     CSCALE     FOCL     CLINE     CSAMP     CAMERA
     DISTOR     IMAGE    OBJECT

.PAGE
 VIEWING GEOMETRY PARAMETERS
	The camera viewing geometry is specified by a vector and rotation
 matrix.  The vector, called the RS vector, is the vector from the center of
 the planet to the spacecraft, specified in the planet coordinate system.  The
 matrix, called OM, rotates vectors from the planet coordinate system to the
 camera system.  Reference (3) contains a complete description of how camera
 optics data, planetary radii, and the RS vector and OM matrix allow a line
 and sample to be computed from a latitude and longitude.
	MAP3 can obtain RS and OM in several different ways.  They can be
 specified in the parameter list or computed by one of four different
 algorithms.  In addition, they can be obtained from the SPICE/SEDR for some 
 Voyager and for Galileo frames.
  The following sections describe these various methods.
     1.  Direct Specification by the User
         Parameters in this subsection can be used by themselves to
         completely specify RS and OM.  Or they can be used individually to
         override specific SEDR entries or to supply necessary information
         needed by the algorithms described in later subsections.
         CMATRIX     MEMATRIX     OMMATRIX      VRVECTOR     RSVECTOR     
                 SLAT         SLON          RMAG
     2. Default Algorithm
        In this algorithm (the default), the RS vector is computed from
        the ME matrix and the VR vector.  The OM matrix is computed from
        the C and ME matrices.  This algorithm is considered inferior
        because the camera pointing information in the C matrix usually
        does not have very great accuracy.  The C matrix and the VR vector
        are normally obtained from the SEDR.  The ME matrix is usually
        obtained from the SEDR also, except for Mariner 9, whose SEDR does
	not contain an ME matrix.  If an ME matrix is not obtained from the
	SEDR or parameter list, MAP3 will attempt to calculate it using
	built-in ephemeris data.  This is possible only for the five inner
	planets and the moon.  The following parameters can be used to
	override data used to calculate the ME matrix.
		TIME          RTAS          DECLINAT(Declination)
     3. QUAM Algorithm
	If the RS vector is known, the OM matrix can be computed from
	another vector, called RRP5, and the angle of north in the image
	plane, using an algorithm developed by Lynn Quam.  The RRP5 vector
	is the vector, in the planet coordinate system, from the planet
	center to the intersection of the camera's optical axis and the
	planet's surface (in other words to the point in the center of the
	input frame).
	      QUAM         CLAT        CLON        NORA
     4. FAR ENCOUNTER Algorithm
	Given the RS vector and the north angle, the OM matrix can be com-
	puted if the line and sample of the subspacecraft point in the input
	picture are known.  The RS vector is computed from SLAT, SLON, and
	RMAG (see subsection 1).  The algorithm, developed by Denis Elliott,
	is usually applicable only for pictures containing all or most of the
	planet, since only then can the line and sample of the subspacecraft
	point be determined directly from the input image.
		ISSCPT       NORA       OSSCPT
     5. TIEPOINTS Algorithm
	If the latitude and longitude of three or more points at specified
	lines and samples are known, as well as the RS vector, the OM matrix
	may be computed using still another algorithm, developed by Arnie
	Schwartz.  The RS vector is obtained in the same way as for the FAR
	ENCOUNTER algorithm.
			TIEPTS
.PAGE
 PROJECTION DESCRIPTIVE PARAMETERS
	The parameters in this group are used to describe the output
 projection to the program.  The projection type, scale, and position of the
 input frame in the output frame are under user control.  For oblique and
 polar orthographic and stereographic, the orientation of the input in the
 output is also under user control.
	Some of the parameters have slightly different meanings depending on
 the projection type.  So this section will be divided into projection-related
 subsections.  Some parameters will be discussed more than once.
	All parameters in all subgroups are optional.  The default philosophy
 is: "Where applicable, choose a center of projection in the center of the
 input picture.  Center the input in the output.  Where orientation is not
 fixed (Mercator and Lambert always have north at the top), preserve the
 orientation of the input in the output.  Subject to the above restrictions,
 which have priority, scale the output projection so that the input frame
 nearly fills the output frame without loss of data over the edge. Also make
 the scale come out so that it is one of nine standard numbers times a power
 of ten."

.PAGE
     1. Choice of Projection
	The parameters in this subgroup are used to specify the projection
	type.  If the user makes no choice the program decides as follows:
	For Standard flights with the SEDR available, the latitude of the
	center of the input image is obtained from the SEDR.  If the absolute
	value of the latitude is less than 30 degrees, a Mercator projection 
	results.  If it is greater than 65 degrees, a Polar Stereographic
	projection is used.  Otherwise a Lambert Two-Standard Conformal Conic
	is chosen, with standard parallels at 59 degrees.17 and 35 degrees.83
	or -35 degrees.83 and 59 degrees.17, depending on the sign of the 
	latitude.  If no SEDR is available (as for non-standard flight
	pictures) the program checks to see if CLAT was specified (see QUAM
	algorithm subsection).  If so, its value is used in the above test. 
	If not, an Orthographic projection is chosen.
        
.PAGE
        The choices of projection are:

        ORTHOGRA                 Oblique Orthographic
        ORTHOGRA POLE            Polar Orthographic
        STEREOGR                 Oblique Stereographic
        STEREOGR POLE            Polar Stereographic
        LAMBERT                  Lambert Two Standard Conformal Conic
        MERCATOR                 Mercator
        TMERCATO                 Transverse Mercator
        CYLINDRI                 Normal Cylindrical
        RECTANGU                 Simple Cylindrical or Rectangular
        OBCYLIND                 Oblique Simple Cylindrical
        SINUSOID                 Sinusoidal
        OBSINUSO                 Oblique Sinusoidal
        MOLLWEID                 Mollweide
        PERSPECT                 Perspective 

.PAGE
     2. Projection Scaling
	In all map projections, there is at least one point at which the scale
	of the projection equals the scale on the sphere.  The scale at such
	points, in km/pixel, determines the size of the input picture in the
	output area.  For the Mercator projection, scale is correct everywhere
	on the equator.  For the Lambert, it is valid all along both standard
	parallels.  For the Orthographic and Stereographic, it is valid at the
	center of projection.  If no scale is specified, one will be computed
	so that the input picture is as large as possible without loss of
	data.  Then this scale is rounded to one of nine standard values in
	such a way as to make the projection of the input frame slightly
	smaller.
				SCALE

.PAGE
     3. Projection Descriptive Parameters for Mosaicking
	The Mercator projection, for which the LINE, SAMPLE, LATITUDE, and
	LONGITUDE parameters can be used to place any desired point anywhere
	in the output, and the Polar Stereographic and Orthographic, for which
	the default is to center the input picture in the output frame REGARD-
	LESS of the position of the pole, are easy to use to produce large
	scale mosaics with the above parameters.  For such mosaics it is impor-
	tant that the scale, latitude and longitude of special points, meri-
	dians, or parallels, and orientation be constant in all projections.
	Then good results can be obtained by simple translation of the pro-
	jected pictures.  But for the Lambert and Oblique Stereographic and
	Orthographic projections, the previously discussed parameters are
	clumsy to use for mosaicking.  Frequently, the center of projection or
	central meridian is far outside the input frame.  Defaulting LINE and
	SAMPLE then leads to an all zero output frame.  To overcome the
	problem, users were formerly forced to compute line and sample
	parameters using the equations describing their projection.  A new 
	group of parameters, explained below, allows the user to specify an
	approximate line and sample for an arbitrary point unrelated to the
	center of projection or the central meridian.  The program then 
	computes values of the LINE and SAMPLE parameters consistent with the 
	user's specification and acts as if these values were present in the 
	parameter list (see the Lambert subsection for restrictions on the use 
	of the LINE parameters).  Note also that the value of SCALE is required
 	in order to perform the calculations.  For the Lambert case, this means
	that LIN1 and LIN2 may not be used if any of the keywords below are 
	present.
		The group below is collectively called "recentering parameters"
	because the center of projection is repositioned from its normal 
	default location.  The presence of any of the recentering parameters 
	implies the use of repositioning algorithms which, to repeat, have the 
	effect of overriding default values of LINE and SAMPLE.  Therefore, 
	LINE and SAMPLE should not be used if recentering is used.  The LINE 
	and SAMPLE values are computed so that a specified point projects as 
	close as possible to a specified output pixel, subject to the 
	constraint that the center of projection (pole for Lambert) projects 
	to an integral pixel. This restriction ensures that translational 
	offsets for various parts of a mosaic are all integral numbers of 
	pixels.
		PLAT     PLON     PLINE     PSAMPLE     RECENTER

.PAGE
     4. Lambert Projection Parameters
        LINE      The line in the output to which the pole will project.
        SAMPLE    The sample to which the central meridian projects,
                  specified by LONGITUDE.
        SCALE     The scale in km/pixel at the standard parallels.
        PAR1      The latitude in degrees of the northernmost of the two 
		  standard parallels. 
                  Default is 59.17 for northern hemisphere input frames 
		  and -35.83 for southern.  
        PAR2      The southernmost of the two  standard parallels. 
                  Default is 35.83 or -59.17. PAR1 and PAR2 must have the same 
		  sign or the projection is undefined.
        LIN1      The line in the output frame at which the northern standard 
		  parallel's projected arc is to intersect the central 
		  meridian.
        LIN2      The same as LIN1 but refers to the  southern standard 
		  parallel.
        LATITUDE  The projected parallel which will intersect the central 
		  meridian at line NL/2, where NL is obtained from the size 
		  field on the EXEC card.  R3 need not be a standard parallel. 
		  Default is to use the latitude of the center of the input 
		  picture for R3.  This can have unpleasant results (such as a 
		  completely black output picture) if the central meridian does
		  not appear in the output frame (see the mosaicking subsection
		  for ways to avoid this problem). 
        LONGITUD  The longitude, in degrees, to be used as the central
                  meridian.  The default is the longitude of the center of the
                  input picture.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.  It is suggested that you 
		  use this parameter along with PLAT and PLONG !  The line and
		  sample that goes with a certain latitude and longitude can 
		  be specified using PLINE, PSAMP, PLATITUD, PLONGITU. 
		  Otherwise these will default to the center of the output and 
		  the lat/lon in the center of the input image respectively.

        NOTES:
	The scale and vertical placement of the output image can be specified
	in a variety of ways.  Do not attempt to overspecify the projection.
	Further explanation will follow keywords below.
 	  LATITUDE     LONGITUD     SAMPLE     PAR1     PAR2     LIN1
		   LIN2		LINE
	In the Lambert projection, any two of the parameters SCALE, LINE, LIN1,
	and LIN2 are sufficient to specify the other two.  In view of this, 
	MAP3 proceeds as follows:

  	  If three or more of the above are specified, 
		the program ABENDS because it probably cannot satisfy all the 
		demands. The projection has been overspecified.
	  If two are specified, 
		their values are used and the remaining two are computed from 
		them.
	  If any of the line parameters are specified, but neither SCALE nor 
	  the other line parameters are, 
		the value of the specified parameter is used. Then SCALE is 
		computed in the usual default manner. Then the other two line 
		parameters follow.
	  If SCALE is specified but none of the line parameters, 
		the value of the LATITUDE parameter obtained from the parameter
		list or by default is used to compute the values of LINE, LIN1,
 		and LIN2 in a consistent manner.
	  If none of the parameters are specified, 
		SCALE is computed in the usual way and then the program 
		proceeds as if SCALE alone were specified.

	From the above discussion it should be clear that if any of LINE, LIN1,
	or LIN2 appear as parameters, the LATITUDE parameter is ignored.

.PAGE
     5. Mercator projection parameters:
        LINE      The line in the output to which LATITUDE will project.
                  Defaults to 1.0.
        SAMPLE    The sample in the output to which LONGITUDE will project.
                  Defaults to 1.0.
        LATITUDE  The latitude of LINE in the output.
                  Default computes latitude to center input in output.
        LONGITUDE The longitude of SAMPLE in the output.
                  Default computes longitude to center input in output.
        SCALE     The scale in km/pixel at the equator.        
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.  It is suggested that you 
		  use this parameter along with PLAT and PLONG !  The line and 
		  sample that goes with a certain latitude and longitude can 
		  be specified using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise 
		  these will default to the center of the output and the 
		  lat/lon in the center of the input image respectively.

.PAGE
     6. Cylindrical projection parameters:
        LINE      The line in the output to which LATITUDE will project.
                  Defaults to 1.0.
        SAMPLE    The sample in the output to which LONGITUDE will project.
                  Defaults to 1.0.
        LATITUDE  The latitude of LINE in the output.
                  Default computes latitude to center input in output.
        LONGITUDE The longitude of SAMPLE in the output.
                  Default computes longitude to center input in output.
        SCALE     The scale in km/pixel at the equator.        
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
     7. Simple Cylindrical (rectangular) projection parameters:
        LINE      The line in the output to which LATITUDE will project to.
                  Defaults to 1.0
        SAMPLE    The sample in the output to which LONGITUDE will project to.
                  Defaults to 1.0
        LATITUDE  The latitude of LINE in the output.
                  Default computes latitude to center input in output.
        LONGITUDE The longitude of SAMPLE in the output.
                  Default computes longitude to center input in output.
        SCALE     The scale in km/pixel at the equator.        
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
     8. Oblique Stereographic projection parameters.
        LINE      The line of the center of projection in the output.
                  Defaults to nlout/2.
        SAMPLE    The sample of the center of projection in the output.
                  Defaults to nsout/2.
        LATITUDE  The latitude of the center of projection.
                  Defaults to the latitude at the center of the input.
        LONGITUDE The longitude of the center of projection.
                  Defaults to the longitude at the center of the input.
        SCALE     Scale in km/pixel at the center of projection.
        NORTH     Angle of the projected spin axis of the north pole
                  measured in degrees clockwise from up in the output.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
     9. Oblique Orthographic projection parameters.
        LINE      The line of the center of projection in the output.
                  Defaults to nlout/2.
        SAMPLE    The sample of the center of projection in the output.
                  Defaults to nsout/2.
        LATITUDE  The latitude of the center of projection.
                  Defaults to the latitude at the center of the input.
        LONGITUDE The longitude of the center of projection.
                  Defaults to the longitude at the center of the input.
        SCALE     Scale in km/pixel at the center of projection.
        NORTH     Angle of the projected spin axis of the north pole
                  measured in degrees clockwise from up in the output.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
    10. Polar Stereographic projection parameters.
        LINE      The line in the output to which the pole will project. 
                  Defaults to nlout/2.
        SAMPLE    The sample in the output to which the pole will project. 
                  Defaults to nsout/2.
        LATITUDE  If latitude is + or - 90 degrees a 
                  polar projection results 
                  whether or not the parameter POLE is also specified. 
                  If POLE is specified, the sign of the LATITUDE 
                  parameter (if present) determines which pole is used 
                  as the center of projection.
        LONGITUDE The west longitude in degrees of the meridian which
                  will point up from the pole in the output. Default is to 
                  compute lon so that the upper left and upper right corners of
                  the input are projected to the same line in the output. 
                  This algorithm minimizes rotation from input to output 
                  regardless of the location of the pole in either picture.
                  This algorithm will not work if either of the upper corners 
                  of the picture is off the planet. In this case LONGITUD must 
                  be specified. Note that the default calculation of
                  orientation is, in a sense, more powerful for polar than 
                  oblique projections. In the oblique projections the algorithm
                  used minimizes rotation only if the center of projection 
                  lies within the input frame. A superior algorithm is 
                  available for the polar projections because of the many
                  simplifications of the projection equations due to the fact
                  that longitude meridians are all straight lines.
        SCALE     The scale in km/pixel at the center of projection. 
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
    11. Polar Orthographic projection parameters.
        LINE      The line in the output to which the pole will project.
                  Defaults to nlout/2.
        SAMPLE    The sample in the output to which the pole will project. 
                  Defaults to nsout/2.
        LATITUDE  If latitude is + or - 90 degrees a 
                  polar projection results 
                  whether or not the parameter POLE is also specified. 
                  If POLE is specified, the sign of the LATITUDE 
                  parameter (if present) determines which pole is used 
                  as the center of projection.
        LONGITUDE The west longitude in degrees of the meridian which
                  will point up from the pole in the output. Default is to 
                  compute lon so that the upper left and upper right corners of
                  the input are projected to the same line in the output. 
                  This algorithm minimizes rotation from input to output 
                  regardless of the location of the pole in either picture.
                  This algorithm will not work if either of the upper corners 
                  of the picture is off the planet. In this case LONGITUD must 
                  be specified. Note that the default calculation of
                  orientation is, in a sense, more powerful for polar than 
                  oblique projections. In the oblique projections the algorith 
                  used minimizes rotation only if the center of projection 
                  lies within the input frame. A superior algorithm is 
                  available for the polar projections because of the many
                  simplifications of the projection equations due to the fact
                  that longitude meridians are all straight lines.
        SCALE     The scale in km/pixel at the center of projection. 
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
    12. Oblique Simple Cylindrical projection parameters.
        LINE      Line in output to which latitude=0 will project.
                  Defaults to nl/2.
        SAMPLE    Sample in output to which longitude=180 will project.
                  Defaults to ns/2.
        LATITUDE  The new latitude of the position to which the North pole
                  will move in the oblique space.
                  Defaults to +90 degrees.
        LONGITUDE The new longitude of the position to which the North pole
                  will move in the oblique space.
                  Defaults to 0.0.
        PAR1      The new longitude in the oblique space to which
                  LONGITUDE will move.
        SCALE     The scale in km/pixel.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.
        
        Suggestions: Use RECENTER and PLAT,PLONG. If you use RECENT, then
        the program will know where you sent the pole (using LATI,LONG)
        and will assure that the PLAT,PLONG you specified maps to the
        center of the output. For example, if you specified:
                LATI=0 LONG=180 PAR1=180 
        the North pole would rotate to latitude=0 longitude=180.
        If you also specified:
                'RECENT PLAT=90. PLONG=60.
        then you would be assured to see the pole centered in the output
        but projected as though it were really at latitude=0 longitude=180.
.PAGE
    13. Sinusoidal projection parameters.
        LINE      The output line of the latitude specified by LATITUDE.
                  Defaults to the center of the output.
        SAMPLE    The output sample of the reference longitude 
                  specified by LONGITUDE.
                  Defaults to the center of the output.
        LATITUDE  The latitude placed at LINE in the output.
                  Defaults to the center of the input.
        LONGITUDE The REFERENCE longitude for the projection
                  ( that longitude which maps to a vertical line ).
                  Defaults to the center of the input.
        SCALE     The scale in km/pixel at the equator.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
    14. Oblique Sinusoidal projection parameters.
        LINE      Line in output to which latitude=0 will project.
                  Defaults to nl/2.
        SAMPLE    Sample in output to which longitude=180 will project.
                  Defaults to ns/2.
        LATITUDE  The new latitude of the position to which the North pole
                  will move to in the oblique space.
                  Defaults to +90 degrees.
        LONGITUDE The new longitude of the position to which the North pole
                  will move to in the oblique space.
                  Defaults to 0.0.
        PAR1      The new longitude in the oblique space to which
                  LONGITUDE will move.
        SCALE     The scale in km/pixel on the oblique equator.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.
        
        Suggestions: Use RECENTER and PLAT,PLONG. If you use RECENT then
        the program will know where you sent the pole (using LATI,LONG)
        and will assure that the PLAT,PLONG you specified maps to the
        center of the output. For example if you specified:
                LATI=0 LONG=180 PAR1=180 
        the North pole would rotate to latitude=0 longitude=180.
        If you also specified:
                'RECENT PLAT=90. PLONG=60.
        then you would be assured to see the pole centered in the output
        but projected as though it were really at latitude=0 longitude=180.

.PAGE
    15. Mollweide projection parameters.
        LINE      The output line of latitude 0.
                  Defaults to the center of the output.
        SAMPLE    The output sample of the reference longitude 
                  specified by LONGITUDE.
                  Defaults to the center of the output.
        LATITUDE  not used.
        LONGITUDE The REFERENCE longitude for the projection.
                  Will be a vertical line in the output.
                  Defaults to the center of the input.
        SCALE     The scale in km/pixel at latitude 40 deg 44 min N or S.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG !
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
    16. Transverse Mercator projection parameters.
        LINE      The output line of the latitude specified by LATITUDE.
                  Defaults to the center of the output.
        SAMPLE    The output sample of the reference longitude 
                  specified by LONGITUDE.
                  Defaults to the center of the output.
        LATITUDE  The latitude placed at LINE in the output.
                  Defaults to the center of the input.
        LONGITUDE The REFERENCE longitude for the projection
                  ( That longitude which maps to a vertical line ).
                  Defaults to the center of the input.
        SCALE     The scale in km/pixel at central meridian.
        RECENTER  Computes LINE and SAMPLE such that PLATITUD PLONGITU
                  go to that line and sample in the output.
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.

.PAGE
    17. Perspective projection parameters.
        In this projection the user may specify a "new camera" location (and
        even new camera descriptive parameters) to generate an output image
        corresponding to the perspective from the new location.  Changing
        the SCALE (specifying SCALE) changes the distance of the "new camera"
        to the planet.

        LINE      The output line of the center of projection. Is 
		  the line of
                  the new sub-spacecraft point in the output.
                  Defaults to center of output.
        SAMPLE    The output sample of the center of projection. Is 
                  the sample of the new sub-spacecraft point in the output.
                  Defaults to center of output.
        LATITUDE  The new sub-spacecraft point latitude in the output. 
                  Defaults to the input sub-spacecraft latitude.
        LONGITUDE The new sub-spacecraft longitude in the output.
                  Defaults to the input sub-spacecraft longitude.
        NORTH     The angle of the new north spin axis measured clockwise
                  from up in the output.
                  Defaults to the same as the input.
        SCALE     The scale in km/pixel in the output.
                  Defaults to the scale of the input picture.
        RECENTER  Computes LINE and SAMPLE such that the center of the input
                  maps to the center of the output.
                  It is suggested that you use this parameter along with
                  PLAT and PLONG if the field of view is so small that
                  the LATITUDE LONGITUD coordinate does not appear in
                  the input. 
                  The line and sample that goes with a certain latitude
                  and longitude can be specified
                  using PLINE,PSAMP,PLATITUD,PLONGITU. Otherwise these
                  will default to the center of the output and the lat/lon
                  in the center of the input image respectively.
.PAGE
PRECISION:

This portable version of MAP3 will produce basically the same output on
all of the MIPS-supported platforms.  This output will frequently not be
identical to the output of the unported version of MAP3, but the differences
will be trivial and should be improvements.

This section is required by the MSTP SRD for all programs that
do not meet the precision requirements given in the SRD.  The two requirements
not met by MAP3 are:
1) Integer data output by a ported program must be the same as that produced
   by the unported program.
2) Integer data output by a ported program must be the same on all of the
   MIPS-supported platforms.
All of the programs that do geometric transformations are going to have problems
meeting one or both of these requirements.   This includes MAP3, LGEOM, MGEOM,
GEOMA, and several others.  MAP3 was modified to increase the use of
Double Precision floating point arithmetic so that the second requirement is met
"well enough" and the first requirement is met "well enough". 
The definition of the above qouted term is given towards the end of this 
section.

In the port, MAP3 was modified to retain the full precision returned from
subroutine DSIMQ.  This was done to minimize the differences
produced on different platforms.  The results are somewhat more accurate
than those of the unported MAP3, but since the output DN values are rounded
to the nearest integer, the output images are essentially the same as for the
unported MAP3.

In computing the output pixel value (DN), MAP3 first determines if the pixel
lies on the planet.  If it does not, the output pixel value is set to 0.
If the pixel does lie on the (map-projected) planet, MAP3 uses subroutine 
CONVEV to compute the latitude and longitude from the output line and sample
coordinates.  Then it uses subroutine CORCAV to compute (in floating point)
the the input image line and sample coordinates from the latitude and
longitude.  Since the floating point line and sample coordinates generally lie
between the integer line and sample coordinates of four adjacent input pixels,
MAP3 uses bilinear interpolation to compute a pixel value for the floating
point location.  This is the value given to the output pixel. (This is a
simplification, but is adequate for this discussion of precision.) 

The image a2.map3 used in the MAP3 test pdf shows what type of
differences are likely to occur between the ported MAP3 and the unported MAP3.
The output file from the ported MAP3 is very slightly different
from the output file from the unported MAP3.  99.997% of the pixels are
identical.  All but one of the rest differ in value by 1.  The other pixel
differs in value by 129.

The cause of the differences is that the single precision computations
(unported MAP3) round differently than the double precision computations
(ported MAP3).  MAP3 rounds the results to the nearest integer.  Usually the
differences make no difference once the rounding occurs.  Only rarely do the
differences cause rounding up in one case and rounding down in the other, which
causes a difference of 1 in the output value.  In one case, the differences
caused the program to conclude the output pixel did not correspond to
an input pixel in one case but did in the other.  This occurred at the border 
of the non-zero section of the image  (planet limb or terminator.)
These two type of differences combines to produce
a slight but insignificant difference.  This difference is estimated at
less than 1% of the difference between using 'NOINTERP and using regular
interpolation, which is the definition I will use for "well enough".
I believe that such differences will not be humanly observable in an image
display.  

The image a2.map3 used in the MAP3 test pdf shows what type of what type of
differences are likely to occur (with the ported MAP3) between machines. 
The output file from the ported MAP3 on the DEC Alpha is very slightly different
from the output file on the Sun SparcStation.  99.997% of the pixels are
identical.  All but one of the rest differ in value by 1.  The other pixel
differs in value by 7.

In general, the differences between the results for the ported MAP3 on different
machines are comparable to the differences between the ported MAP3 and the
unported MAP3.  Both types of differences are on the order of the expected
rounding in single precision computations.  Although the bilinear interpolation
computation was upgraded to double precision, parts of the CONVEV and CORCAV
computation are in single precision.  
For a 1000 by 1000 byte image from 10 to 1000 slightly different pixels may be
expected.  This assumes matching SPICE data.  If the SPICE data differ 
significantly, the output images will not be comparable.
.PAGE
REFERENCES:

  1) MAP2 User's Guide 900-639, Vol. III (November 1973).
  2) JPL Publication 77-7, "Transformations from an Oblate Spheroid to a
     Plane and Vice Versa", (January 1977).
  3) "Digital Cartographic Projection", Proceedings of the Caltech/JPL
     Conference on Image Processing Technology, Data Sources and Software
     for Commercial and Scientific Applications, (November 1976).
  4) MIPS Map projection Software User's Guide D-11810, Ver. 1.1 (May 1994).

PROGRAM HISTORY:

 WRITTEN BY: A. R. Gillespie and C. A. Rofer     22 June 1972
 MODIFIED BY: Arnie Schwartz, Denis Elliott, Joel Mosher, Jean Lorre,
   Charles Avis, Helen (DeReuda) Mortenson, Florance Moss, John Reimer,
   Dave Glackin, Gary Yagi, Thuy Truong
 CURRENT COGNIZANT PROGRAMMER: Jean Lorre
 REVISION HISTORY:
     Nov 99  lwk  added some checks for silly values of NL, NS, SCALE, RADIUS;
                  added check for PAR1/2 for Lambert;  fixed problem with
                  Julian date ("Y2K" correction was being done twice)
     Nov 97  SP   Restored equation for RMAG_PERS for perspective projection.
                  This had been changed in Jan. 1997 erroneously, causing
                  distorted results, particularly when the new camera location
                  was close to the planet.
     Oct 96  SP   Correct handling of GLL summation mode.  Corrections for
                  handling DNTHRESH parameter and and interpolation.
                  Please note that even when the DNTHRESH parameter is 
                  defaulted, the associated algorithm will occasionally result
                  in minor changes (improvements) because pixels with a DN
                  value of 0 will not be used in the interpolation.  Differences
                  will typically be found where a zero pixel and a non-zero
                  pixel are adjacent.
     Aug 96  OAM  Added DNTHRESH parameter and subroutine setDnValue to
                  handle interpolation. 
     May 96  SP   Added proper initialization of variables.  Modified to
                  use GETSPICE2 and associated PDF parameters.
     Jan 96  SP   Reported to UNIX using many of changes from TLT but using
                  CONVEV instead of MP_XY2LL.  Modified to expect any second
                  input file to be in IBIS tiepoint format.
                  Added call to GLLGCOR to Farenc algorithm for Galileo support.
     AUG 94  TLT  Ported to UNIX (only suppports GLL input, does not support
                  output PERSPECTIVE projection).
                  Replaced CONVEV, TRANV, SEARCV2, MAPLABV2 w/ MP software.
                  Added keyword SPICE_SERVER.
     Sep 93  jjl  Permit space telescope data
     AUG 93  JJL  Permits perspective projection inputs.
     JUN 91  JJL  Extensive cleanup to help, test, added 6 projections.
     DEC 89  JJL  Perform own geom.
     Oct 89  JJL  Converted to project independent.
  29 OCT 89  GMY  Deleted FARENC keyword (invoked via ISSCPT or OSSCPT)
  23 Aug 89  GMY  Delete fiddling with FDS for WA of simultaneous exposure pair
  22 jun 89  jjl  Change flag to 1.0e+15 to fix lgeom artifact
  10 may 88  FFM  Add keyword 'SEDRSRC
  01 mar 88  GMY  Add check for NAV and NAV2 flags in SEDR when one or more
		  tiepoints is off planet.
  10-MAY-88  FFM  INCORPORATE "SEDRSRC" KEYWORD
  22-FEB-87  JAM  INCORPORATED SMT,SXP CHANGES 
                  SMT COMMENTED OUT CALLS TO SEDR73 AND ACOS3
                  SXP CHANGED OACALL TO VOLAB TO VOLABV2
  10-FEB-87  JAM  WARNING IF INPUT IS O.S. AND GEOM FILE INPUT
  28-DEC-86  JAM  INCORPORATE "NOPROJEC" KEYWORD
  23 Feb 86  JAM  FIX DISTAL
  23 Jan 86  JAM  Add 'IMAGE,'OBJECT
   4-JUL-85  JAM  START REORGANIZING PARAMETER PROCESSING
  28-JUN-85  JAM  REPLACE *CAL SUBROUTINES WITH R2LIB:CONVEV
  26-JUN-85  JAM  REPLACE TIECHK WITH R2LIB:TIECHV
  23-JUN-85  JAM  REPLACE FOMCL2 WITH R2LIB:FOMCLV
  23-JUN-85  JAM  REPLACE CORCAL WITH R2LIB:CORCAV
  22-JUN-85  JAM  REPLACE IPPCOR WITH R2LIB:IPPCOV
  15-JUN-85  JAM  CONVERT TO VICAR2
  23 APR 84  HBD  RESTRUCTURED SOME CODE AND DELETED SOME CALLS
                         TO OPEN AND CLOSE
   5 APR 84  HBD  DELETED CALLS TO OPEN BEFORE GLABEL CALLS
  24 MAR 84  JAM  CHANGE CALL TO LABEL FOR RECTANGULAR PROJECTION
  15 DEC 83  CCA  REMOVE IDS;MAKE DSRN 1=PDS (LABELED)
  02 AUG 83  JAM  CONVERT TO VAX
  02 MAY 83  JHR  BUGS
  10 JAN 82  JHR  CODE MODIFICATIONS, SLAT FROM SEDR79
                         CHANGED TO GEOCENTRIC
  12 APR 81  JHR  ADDED SIMPLE CYLINDRICAL PROJECTION
  10 MAR 81  JAM  READ OM MATRIX FROM VGR SEDR
  16 FEB 81  JAM  ALLOW MAP3 TO GET OMMATRIX FROM FARENC
   8 OCT 80  JJL  ADD KEYWORDS:
   8 OCT 80  JJL  OSSCPT,ISSCPT,LORA,GEODET,NAM2,HALF
  19 JUN 80  JAM  ADD VOYAGER FLIGHT INFORMATION
  28 MAY 80  JAM  CHANGE ROUTCO CALLS TO OUTCON
                         MUST INCLUDE OUTCON2 IN LINKEDIT
  25 MAY 80  JAM  PUT IN HALFWORD OPTION
     JAN 79  DLG  ADD VOYAGER MISSION MODULE
  30 AUG 78  DAE  TRIAXIAL SOLID
  03 JAN 77  DAE  USE EXACT EQUATIONS FOR SPHEROIDAL PLANETS
                  REDO DEFAULT NORTH ANGLE COMPUTATION
                  INSTALL RECENTERING OPTION FOR EASIER MOSAICING
  12 MAY 76  DAE  ADD FINAL VI76 CHANGES
  23 APR 76  DAE  RESTRUCTURE LINK EDIT TREE AND DISTOR SECTION
  05 FEB 76  DAE  CONVERT TO OS AND MAKE VO76 A STANDARD MISSION
  27 JUN 75  DAH  CHANGES FOR CONVERSION TO 360/OS
   1 MAR 75  DAE&AAS  VERSION XXX RELOCATABLE, FARENC & TIEPTS MODES
  01 JAN 73  AAS  LAMB.CONF.FIX/LABEL MISCOUNT PROBLEM/MULTPHASING
   SUPERMAP A.R.GILLESPIE
   ALGORITHMIC DEVELOPMENT - J.B.SEIDMAN
   MAP3 C.A.ROFER

.LEVEL1

.VARI INP
 STRING-(1:3)Required and
 optional input datasets.

.VARI OUT
 STRING-1 Required
 output dataset.

.VARI SIZE
 INTEGER-(1:4) Starting line,
 starting sample, number of
 lines, number of samples
 for output.

.VARI NL
 INTEGER-Number of lines in
 output.

.VARI NS
 INTEGER-Number of samples in
 output.

.VARI PLANET
 STRING-Target body name
 12 characters

.VARI NOSEDR
 KEYWORD-do not read project SEDR for
 pointing, trajectory,etc. data

.VARI NOINTERP
 KEYWORD-suppress interpolation

.VARI DNTHRESH
Interpolation DN
threshold. Pixel 
DNs > DNTHRESH
will be included.

.VARI NOGEOM
 KEYWORD to suppresses the
 fetch of LGEOM.

.VARI REFTIME
 INTEGER-Time of projection.
 For zonal flow
 correction.

.VARI ZVP
 STRING-Name of zonal
 flow file.

.VARI SHORT
 KEYWORD-implying SEDR input
 NOT the master Viking
 Orbiter SEDR.

.VARI NOLABEL
 KEYWORD-to supress label
 updating activity.

.VARI FORMAT
 KEYWORD-for data format.
 Valid:('HALF,'BYTE)

.VARI GEODET
 KEYWORD-to specify geodetic
 latitudes.

.VARI NAM2
 KEYWORD-to request projection
 for north angle.

.VARI MM71
 KEYWORD-Mariner 9 project

.VARI MVM73
 KEYWORD-Mariner 10 project

.VARI VI76
 KEYWORD-Viking project

.VARI VGR1
 KEYWORD-Voyager 1 project

.VARI VGR2
 KEYWORD-Voyager 2 project

.VARI GLL
 KEYWORD-Galileo project

.vari noprojec
 KEYWORD-non-flight project

.VARI DAS
 INTEGER-Used in default
 processing.

.VARI FDS
 INTEGER-Used in default
 processing.

.VARI FSC
 INTEGER-Used in default
 processing.

.VARI RADII
 REAL-Radii of projected target
 body.

.VARI LORA
 REAL-Longitude of target body.

.VARI CSCALE
 REAL-Number of pixels/mm on
 focal plane.

.VARI FOCL
 REAL-Camera focal length in mm.

.VARI CLINE
 REAL-Line of optic axis 
 picture.

.VARI CSAMP
 REAL-Specifies sample of
 optical axis.

.VARI CAMERA
 INTEGER-Camera number.

.VARI DISTOR
 KEYWORD-Input frame has
 NOT been corrected.
 Same as 'IMAGE

.VARI IMAGE
 KEYWORD-Input frame has NOT
 been corrected.
 Same as 'DISTOR

.VARI OBJECT
 KEYWORD-Input frame has
 been corrected.

.VARI CMATRIX
 REAL-Specifies C-matrix in
 row major order.

.VARI MEMATRIX
 REAL-Specifies ME matrix
 in row major order.

.VARI OMMATRIX
 REAL-Specifies OM matrix
 in row major order.

.VARI VRVECTOR
 REAL-Specifies X, Y, Z
 components of the VR vector.

.VARI RSVECTOR
 REAL-Specifies RS vector.

.VARI SLATITUD
 REAL-Spherical target body
 coordinate.

.VARI SLONGITU
 REAL-Spherical target body
 coordinate.

.VARI RMAGNITU
 REAL-Spherical target body
 coordinate.

.VARI TIME
 INTEGER-Time frame was taken.

.VARI RTAS
 REAL-Right ascension of target
 body's north celestrial pole.

.VARI DECLINAT
 REAL-Declination of target's
 north pole.

.VARI QUAM
 KEYWORD-Specifies use of QUAM
 algorithm.

.VARI CLATITUD
 REAL-Center latitude of input
 image.

.VARI CLONGITU
 REAL-West longitude of above
 point.

.VARI NORANGLE
 REAL-Angle of north in degrees.

.VARI ISSCPT
 REAL-Subspacecraft point in
 image space.

.VARI OSSCPT
 REAL-Subspacecraft point in
 object space.

.VARI TIEPTS
 REAL-Specifies tiepoint mode.

.VARI MERCATOR
 KEYWORD-map projection type

.VARI LAMBERT
 KEYWORD-map projection type

.VARI STEREOGR
 KEYWORD-map projection type

.VARI CYLINDRI
 KEYWORD-map projection type

.VARI RECTANGU
 KEYWORD-map projection type

.VARI ORTHOGRA
 KEYWORD-map projection type

.VARI POLE
 KEYWORD-Forces polar projection.

.VARI OBCYLIND
 KEYWORD-map projection type

.VARI SINUSOID
 KEYWORD-map projection type

.VARI OBSINUSO
 KEYWORD-map projection type

.VARI MOLLWEID
 KEYWORD-map projection type

.VARI TMERCATO
 KEYWORD-map projection type

.VARI PERSPECT
 KEYWORD-map projection type 

.VARI SOUTH
 KEYWORD-Causes negative default
 values.

.VARI SCALE
 REAL-Scale in km/pixel.

.VARI LINE
 REAL-Line in output picture.

.VARI SAMPLE
 REAL-Sample in output picture.

.VARI LATITUDE
 REAL-Specifies latitude.

.VARI LONGITUD
 REAL-Specifies longitude

.VARI PAR1
 REAL-Northernmost lat. of 2
 standard parallels.

.VARI PAR2
 REAL-Southernmost lat. of 2
 standard parallels.

.VARI LIN1
 REAL-Line in output frame.

.VARI LIN2
 REAL-Line in output frame.

.VARI NORTH
 REAL-Angle in degrees north.

.VARI PLATITUD
 REAL-Latitude of a point.

.VARI PLONGITU
 REAL-Longitude of a point.

.VARI PLINE
 REAL-Line near point
 (PLAT,PLON).

.VARI PSAMPLE
 REAL-Sample near point
 (PLAT,PLON).

.VARI RECENTER
 KEYWORD-Specifies recentering
 algorithms.

.VARI HALF
 KEYWORD-Input is 
 16 bits per pixel

.VARI BYTE
 KEYWORD-Input is 
 8 bits per pixel

.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created
.VARI PRINT
PRINT=(startline,endline)
prints LAT,LON,IN_LINE,IN_SAMP
for the range of output lines.  

.LEVEL2

.VARI INP
 STRING-INP=(IN,GEOM)
 INP is the input dataset to be projected where: IN (REQUIRED)
 is the input dataset filename. GEOM (OPTIONAL) is an IBIS dataset
 which, if supplied, must contain a valid set of GEOMA parameters
 capable of transforming the input frame from image space
 (geometrically distorted) to object space (undistorted). Such a
 dataset can be produced by OLDGEOMA2IBIS, or RESLOCVO(Viking Orbiters 1 and 2)
 or RESLOC or FARENC(Voyager). 

.VARI OUT
 STRING-OUT=OUTIMAGE
 OUT is the output version of IN.  Unless the NOGEOM parameter is specified
 (see NOGEOM Parameters write-up), it must be NL lines by NS samples, where
 NL and NS come from the size field, plus extra lines for labels. 

.VARI SIZE
 INTEGER-SIZE=(1,1,NL,NS) 
 SIZE is a standard VICAR size field with NL as the number of lines and NS
 as the number of samples. 

.VARI NL
 INTEGER-
 NL is the number of lines in the output dataset.

.VARI NS
 INTEGER-
 NS is the number of samples in the output dataset.

.VARI PLANET
 STRING-
 PLANET is the target body name, 12 characters fully spelled out.  This will 
 determine the planet radii and the SPICE.  
 THIS IS A PLANET DESCRIPTIVE PARAMETER.

.VARI NOSEDR
 KEYWORD-(Valid:'NOSEDR)
 'NOSEDR requests for MAP3 to not read the project SEDR for pointing,
 trajectory, etc. data.  The user must input all relevent information. 

.VARI DNTHRESH
Specifies a DN threshold used only in interpolation mode. If, when
interpolating a dn value in the input file, one or more of the four
neighbors is less than or equal to DNTHRESH it will not be used
in the interpolation.
Several combinations of input dn values can occur.
Cases:
If 4 input neighbors are > dnthresh then interpolation is done.
If 3 input neighbors are > dnthresh the fourth is extrapolated and then
   interpolation is done.
If 2 input neighbors are > dnthresh the output pixel is determined from
   a linear weighting of the 2 good pixels.
If 1 input neighbor is > dnthresh it is copied to the output.
If all the input neighbors are <= dnthresh the output is determined from
   the nearest neighbor.

.VARI NOINTERP
 KEYWORD-(Valid:'NOINTERP)
 'NOINTERP will suppress interpolation between input DN values. Use nearest 
 neighbor to determine output DN.

.VARI NOGEOM 
 KEYWORD-(Valid - 'NOGE)
 NOGEOM will suppress the fetch of LGEOM. 

.VARI REFTIME
 INTEGER-REFTIME=(N1,N2,N3,N4,N5,N6)
 REFTIME is the reference time for the output projection. 
 Specifying REFTIME invokes the zonal differential flow correction (see
 the ZVP keyword). REFTIME specifies the time of the output projection
 to which the different input latitudes must be shifted using the
 zonal flow model. To correctly mosaic images of a gas planet, all the
 input images must be projected to the same reference time so that
 the different latitudes match.  Defaults to the same time as the input image
 (no zonal correction).Format is 6 integers, Nn, in the order:
           year day hour minute second millisecond.
 Example:  reftime=(92,88,56,2,33,0) in the same sense as the GLL sclk time
 found in the picture label; ie: year 1992 day 88...

.VARI ZVP
 STRING-
 ZVP is the name of the zonal flow file used by map3. Only used
 if REFTIME is specified and if the default name is inappropriate.

.VARI SHORT
 KEYWORD-(Valid - 'SHORT) 
 Applicable for Viking Orbiter 1 and 2 ONLY, 'SHORT indicates that the
 SEDR input is NOT the master Viking Orbiter SEDR.  Instead it is a dataset
 containing a VICAR label and one other record of 2000 bytes containing
 one SEDR record. Such a dataset can be produced as an alternate output
 from VOLOG, the Orbiter logging program, or directly from a SEDR tape
 using VSAR. When a master SEDR file is in use, the DAS, FDS or FSC time
 is used to access the correct record in the file. If no corresponding
 entry appears in the dataset's directory, MAP3 abends. If SHORT is specified,
 the first non-label record is used and no check on FSC time is made. 

.VARI NOLABEL
 KEYWORD-(Valid - 'NOLABEL)
 'NOLABEL will suppress certain label updating activities of MAP3. This
 parameter does NOT allow MAP3 to run on non-VICAR datasets and does NOT
 suppress the addition of several logical label records which
 mathematically describe the output projection. The logging programs for
 the standard missions usually generate several label records containing
 the latitude and longitude of the four corners and center of the input
 frame. Normally, MAP3 updates these labels using the same camera pointing
 information it uses to project the picture. The updated values may differ
 from their old values, but still refer to the input frame, not the
 output. Mariner 10 labels did not contain these entries, so MAP3
 creates them and fills them in. The primary use of NOLABEL was to
 suppress this activity for Mariner 10 frames in order to improve the
 picture to label size ratio on MASK'ed pictures. 

.VARI HALF
 KEYWORD-(Valid - 'HALF)
 'HALF indicates that the input and output are halfword data.

.VARI BYTE 
 KEYWORD-(Valid - 'BYTE)
 'BYTE indicates that the input and output are byte data.

.VARI GEODET
 KEYWORD-(Valid - 'GEODET)
 'GEODET indicates that CLAT and SLAT, entered by the user via the
 parameter list, are geodetic latitudes. They will be converted to
 geocentric before processing. 

.VARI NAM2
 KEYWORD-(Valid - 'NAM2)
 'NAM2 specifies that the north angle is to be measured as if it were
 the angle from up of the planet spin axis projected on the image,
 i.e., the MAP3 convention. The default is that the north angle is
 measured as if it were the angle from up of the planet spin axis
 projected on the image after the cameras have been slewed to place
 the optical axis coincident with the planet center. This is the SEDR
 convention. 

.VARI MM71
 KEYWORD-(Valid - 'MM71)
 'MM71 forces the program to assume a Mariner 9 input frame, regardless
 of the picture label.  The default is to obtain the project from the input
 image label.  THIS IS A FLIGHT RELATED PARAMETER.

.VARI MVM73
 KEYWORD-(Valid - 'MVM73)
 'MVM73 forces Mariner 10 processing, regardless of the picture label. 
 The default is to obtain the project from the input image label.
 THIS IS A FLIGHT RELATED PARAMETER.

.VARI VI76
 KEYWORD-(Valid - 'VI76)
 'VI76 forces Viking Orbiter 1 and 2 processing, regardless of the
 picture label.  The default is to obtain the project from the input
 image label.  THIS IS A FLIGHT RELATED PARAMETER. 

.VARI VGR1
 KEYWORD-(Valid - 'VGR1)
 'VGR1 forces Voyager-1 processing, regardless of the picture label. 
 The default is to obtain the project from the input image label.
 THIS IS A FLIGHT RELATED PARAMETER. 

.VARI VGR1
 KEYWORD-(Valid - 'VGR2)
 'VGR2 forces Voyager-2 processing, regardless of the picture label. 
 The default is to obtain the project from the input image label.
 THIS IS A FLIGHT RELATED PARAMETER. 

.VARI GLL
 KEYWORD-(Valid - 'GLL)
 'GLL forces Galileo processing, regardless of the picture label. 
 The default is to obtain the project from the input image label.
 THIS IS A FLIGHT RELATED PARAMETER. 

.VARI NOPROJEC
 KEYWORD-(Valid - 'NOPROJEC)
 'NOPROJEC forces non-flight processing.  The default is to obtain the
 project from the input image label.

.VARI DAS
 INTEGER-DAS=N1
 Where N1 is an integer, DAS means the same as the FDS and FSC parameters. 
 They are available for user convenience.  N1 is the DAS(MM71), 
 FDS(MVM73, Voyager), or FSC(Viking Orbiter) time of the input picture. 
 It is necessary if data is to be obtained from a SEDR and is normally 
 obtained from the picture label.  THIS IS A FLIGHT RELATED PARAMETER. 

.VARI FDS
 INTEGER-FDS=N1
 Where N1 is an integer, FDS means the same as the DAS and FSC parameters. 
 They are available for user convenience. N1 is the DAS(MM71), 
 FDS(MVM73, Voyager), or FSC(Viking Orbiter) time of the input picture. 
 It is necessary if data is to be obtained from a SEDR and is normally 
 obtained from the picture label.  THIS IS A FLIGHT RELATED PARAMETER. 

.VARI FSC
 INTEGER-FSC=N1
 Where N1 is an integer, FSC means the same as the FDS and DAS parameters. 
 They are available for user convenience. N1 is the DAS(MM71), 
 FDS(MVM73, Voyager), or FSC(Viking Orbiter) time of the input picture. 
 It is necessary if data is to be obtained from a SEDR and is normally 
 obtained from the picture label.  THIS IS A FLIGHT RELATED PARAMETER. 

.VARI RADII
 REAL-RADII=(RA,RB,RC)
 RADII is the radii of the projected target.  RA, RB, RC are reals and
 each specify the following:
	RA is the long equatorial radius (km).
	RB is the short equatorial radius (km).
	RC is the polar radius (km).
 THIS IS A PLANET DESCRIPTIVE PARAMETER.

.VARI LORA
 REAL-LORA=R1
 Where R1 is a real number, LORA is the longitude of RA (the long equatorial
 radius).  This parameter is only meaningful when RA does NOT equal RB (the
 short equitorial radius) and even then the prime meridian is usually defined
 at RA. The default is LORA=0.0.  THIS IS A PLANET DESCRIPTIVE PARAMETER. 

.VARI CSCALE
 REAL-CSCALE=R1
 Where R1 is a real number, CSCALE is the number of pixels per millimeter on
 the focal plane once geometric distortion is removed.  
 THIS IS A CAMERA DESCRIPTIVE PARAMETER. 

.VARI FOCL
 REAL-FOCL=R2
 Where R2 is a real number, FOCL is the camera focal length in millimeters. 
 THIS IS A CAMERA DESCRIPTIVE PARAMETER. 

.VARI CLINE
 REAL-CLINE=R3
 Where R3 is a real number, CLINE is the line in an object space picture at
 which the optical axis intersects the image plane. Default non-standard
 flight pictures is NLI/2, where NLI is the number of lines in the input
 picture.  THIS IS A CAMERA DESCRIPTIVE PARAMETER. 

.VARI CSAMP
 REAL-CSAMP=R4
 Where R4 is a real number, CSAMP is the sample of the optical axis. Default
 for non-standard flight pictures is NSI/2, where NSI is the number of samples
 in the input frame.  THIS IS A CAMERA DESCRIPTIVE PARAMETER. 

.VARI CAMERA
 INTEGER-CAMERA=N1
 Where N1 is an integer, CAMERA is the number specifying which camera took the
 picture. It is normally obtained from the picture label, and is needed to
 obtain the correct CSCALE, FOCL, CLINE and CSAMP values if they are defaulted.
 N1 has the following meaning according to each flight. 
	Mariner 9 and Mariner 10
		N1 = 1 for A Camera
		N1 = 2 for B Camera
	Viking Orbiter
		N1 = 1 for Viking Orbiter 1, A Camera, S/N=7
		N1 = 2 for Viking Orbiter 1, B Camera, S/N=4
		N1 = 3 for Viking Orbiter 2, A Camera, S/N=8
		N1 = 4 for Viking Orbiter 2, B Camera, S/N=6
		N1 = 5 for Ground Test Camera, S/N=5
	Voyager
		N1 = 1 for Voyager 1, Narrow Angle, S/N=7
		N1 = 2 for Voyager 1, Wide Angle, S/N=6
		N1 = 3 for Voyager 2, Narrow Angle, S/N=5
		N1 = 4 for Voyager 2, Wide Angle, S/N=4
 THIS IS A CAMERA DESCRIPTIVE PARAMETER.

.VARI DISTOR
 KEYWORD-Valid:('DISTOR, 'IMAGE)
 DISTOR tells MAP3 the input frame has NOT been corrected for vidicon
 scene-dependent geometric distortion and causes a distortion correction
 to be included as well as a map projection. Thus there is no need to GEOM
 a frame twice to get a corrected map projection. DISTOR is superfluous if
 a GEOM dataset is supplied, since its presence automatically induces the
 correction.  For MM71, MVM73, and Viking Orbiter, default "nominal" GEOMA
 parameters are built into the program for each camera. If DISTOR is
 specified and a GEOM dataset is not provided, the nominals for the
 given camera are used. If neither DISTOR nor GEOM are given, the
 input image label is read to determine if geometric correction has
 been performed. THIS IS A CAMERA DESCRIPTIVE PARAMETER. 

.VARI IMAGE
 KEYWORD-Valid:('DISTOR,'IMAGE)
 See DISTOR.  IMAGE and DISTOR are synonyms.

.VARI OBJECT
 KEYWORD-Valid:('OBJECT)
 'OBJECT tells MAP3 that the input image is geometrically correct. OBJECT
 overrides the information obtained from the input image label as to the
 geometric state of the input image. OBJECT is the antonym of IMAGE
 and DISTOR.

.VARI CMATRIX
 REAL-CMATRIX=(R1,R2,...,R9)
 Rn is a real number and specifies the C-matrix in row major order
 (C11, C12, C13, etc.). This matrix transforms camera coordinates to
 reference frame coordinates. The reference frame is the Earth Mean
 Equator 1950.0 System. (See Reference (3) for details.)
 THIS IS A VIEWING PARAMETER used in method 1. 

.VARI MEMATRIX
 REAL-MEMATRIX=(R10,R11,...,R18)
 Rn is a real number and specifies the ME matrix in row major order
 (C11,C12,C13,etc.). This matrix tranforms planet coordinates to
 reference frame coordinates. The reference frame is the Earth Mean
 Equator 1950.0 System. (See Reference (3) for details.)
 THIS IS A VIEWING PARAMETER used in method 1. 

.VARI OMMATRIX
 REAL-OMMATRIX=(R19,R20,...,R27)
 Rn is a real number and specifies the OM matrix in row major order
 (C11, C12, C13, etc.). This matrix transforms planet coordinates to
 camera coordinates. The reference frame is the	Earth Mean Equator
 1950.0 System. (See Reference (3) for details.)
 THIS IS A VIEWING PARAMETER. 

.VARI VRVECTOR
 REAL-VRVECTOR=(R28,R29,R30)
 Rn is a real number and specifies, in order, the X, Y, and Z components
 of the VR vector. This is the vector from the target body center to the
 spacecraft in reference frame coordinates. THIS IS A VIEWING PARAMETER.

.VARI RSVECTOR 
 REAL-RSVECTOR=(R31,R32,R33)
 Rn is a real number and specifies the RS vector. This is the vector from
 the target body center to the spacecraft in planet XYZ coordinates.
 THIS IS A VIEWING PARAMETER.
 
.VARI SLATITUD
 REAL-SLAT=R34
 Where R34 is a real number, SLAT along with SLON and RMAG are the spherical
 target body coordinates of the RS vector. They are used to compute the RS
 vector if QUAM, FARENC or TIEPTS is specified. Default for standard flights
 is to obtain all three from the SEDR.  SLAT is the subspacecraft latitude and
 SLON the subspacecraft longitude (west) in degrees. RMAG is the planet center
 to spacecraft range in kilometers. 
 THIS IS A VIEWING PARAMETER used in method 1. 

.VARI SLONGITU
 REAL-SLON=R35
 Where R35 is a real number, SLON along with SLAT and RMAG are the spherical
 target body coordinates of the RS vector. They are used to compute the RS
 vector if QUAM, FARENC or TIEPTS is specified. Default for standard flights
 is to obtain all three from the SEDR.  SLAT is the subspacecraft latitude and
 SLON the subspacecraft longitude (west) in degrees. RMAG is the planet center
 to spacecraft range in kilometers. 
 THIS IS A VIEWING PARAMETER used in method 1. 

.VARI RMAGNITU
 REAL-RMAG=R36
 Where R36 is a real number, RMAG along with SLAT and SLON are the spherical
 target body coordinates of the RS vector. They are used to compute the RS
 vector if QUAM, FARENC or TIEPTS is specified. Default for standard flights
 is to obtain all three from the SEDR.  SLAT is the subspacecraft latitude and
 SLON the subspacecraft longitude (west) in degrees. RMAG is the planet center
 to spacecraft range in kilometers.
 THIS IS A VIEWING PARAMETER used in method 1. 

.VARI TIME
 INTEGER-TIME=(N1,N2,N3,N4,N5,N6)
 Where Nn is an integer, TIME is the time (GMT) at which the frame was taken.
 The Nn's specify, in order, the year, day, hour, minute, second, and
 millisecond of the middle (ideally) of the exposure. Default is to obtain
 this data from the SEDR if available.  (The ME matrix is time dependent.)
 THIS IS A VIEWING PARAMETER. 

.VARI RTAS
 REAL-RTAS=R1
 Where R1 is a real number, RTAS is the right ascension (in degrees) of the
 target body's north celestial pole. Values for the inner five planets and
 the moon are built into the program. See Reference (1) for an illustration of 
 the meaning of right ascension and declination. THIS IS A VIEWING PARAMETER. 

.VARI DECLINAT
 REAL-DECLINAT=R2
 Where R2 is a real number, DECLINAT is the declination in degrees of the 
 target's north pole.  Values for the inner five planets and the moon are
 built into the program.  THIS IS A VIEWING PARAMETER. 

.VARI QUAM
 KEYWORD-Valid:('QUAM)
 'QUAM indicates the QUAM algorithm is to be used to obtain OM.	
 THIS IS A VIEWING PARAMETER.

.VARI CLATITUD
 REAL-CLAT=R4
 Where R4 is a real number, CLAT is the latitude, in degrees, of the point at
 the center of the input image -- more precisely , at the intersection of the
 optical axis and the planet's surface. Default is to obtain CLAT from the
 SEDR.  THIS IS A VIEWING PARAMETER . Used in QUAM algorithm.

.VARI CLONGITU
 REAL-CLON=R5
 Where R5 is a real number, CLON is the west longitude, in degrees, of the
 above point.  Default is to get CLON from the SEDR. 
 THIS IS A VIEWING PARAMETER.  Used in QUAM algorithm.

.VARI NORANGLE
 REAL-NORA=R6
 Where R6 is a real number, NORA is the angle of north in degrees. It is used
 as a VIEWING PARAMETER in the QUAM algorithm and also in the FAR ENCOUNTER
 algorithm. In the QUAM algorithm, NORA is measured in the image plane, at
 the optical axis, clockwise from up. Default is to obtain NORA from the SEDR.
 The NORA in the SEDR differs slightly from the NORA actually desired. The
 error is a complicated function of the camera pointing and spacecraft
 position, but is not significant except for very oblique viewing angles or
 when quantitative accuracy for mosaicking is desired.  NORA, as used in the
 FAR ENCOUNTER algorithm, is measured in the image plane at the subspacecraft
 point clockwise from up. Note the important difference in the definition of
 NORA in the QUAM and FAR ENCOUNTER algorithms. The default for NORA is the
 same as for the QUAM algorithm, with similar errors involved. In a far
 encounter frame, NORA can and should be measured directly from the image. 

.VARI ISSCPT
 REAL-ISSCPT=(R1,R2)
 Where Rn is a real number, R1 specifies the line and R2 the sample of the
 subspacecraft point in image space (the picture before it is corrected for
 camera distortion).  There are no defaults.  THIS IS A VIEWING PARAMETER. 

.VARI OSSCPT
 REAL-OSSCPT=(R3,R4)
 Where Rn is a real number, R3 specifies the line and R4 the sample of the
 subspacecraft point in object space (picture after it's been corrected for
 camera distortion).  There are no defaults.  THIS IS A VIEWING PARAMETER. 

.VARI TIEPTS
 REAL-TIEPTS=(R11,R21,R31,R41,R12,R22,R32,R42,...R1N,R2N,R3N,R4N)
 Where RnN is a real number, TIEPTS instructs MAP3 to use the tiepoints
 mode and that a tiepoints list follows.  This MUST be the LAST
 keyword specified.  R1N is the line coordinate in the input picture
 of the Nth tiepoint.  R2N is the sample of the Nth tiepoint.  R3N is
 the latitude of the Nth tiepoint in degrees.  R4N is the west
 longitude of the Nth tiepoint in degrees.  3<=N<=20 is expected.  If
 N<3 the program ABENDS. If N>20 only the first twenty points are
 used and a warning is printed.
 THIS IS A VIEWING PARAMETER used in method 5. 

.VARI MERCATOR
 KEYWORD-Valid:('MERCATOR)
 'MERCATOR forces MAP3 to generate a Mercator projection.  This
 projection maps the sphere, except for the two poles, onto a strip
 on the plane.  The width of the strip is equal to the scaled
 circumference of the planet at the equator.  It extends infinitely
 in both vertical directions.  Longitudinal lines project to the
 infinitely long, vertical straight lines which are equally spaced.
 Latitudinal circles become horizontal line segments whose spacing
 increases without limit as you approach the pole.  The Mercator is a
 conformal projection (scale errors at any point are equal in all
 directions, so shapes of small areas are preserved).
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI LAMBERT
 KEYWORD-Valid:('LAMBERT)
 'LAMBERT specifies a Two-Standard Lambert Conformal Conic
 projection.  In this projection, two latitudinal parallels on the same
 side of the equator are chosen as "standard".  There will be no
 scale error anywhere on these parallels.  In addition, a longitudinal 
 line is chosen as "central".  The projection is developed on a
 cone which intersects the planet at the standard parallels.  This
 cone is cut along the meridian 180 degrees away from the central
 meridian.  The result is that the sphere is mapped onto the region
 between two semi-infinite rays emanating from the projection of the
 pole in the same hemisphere as the standard parallels.  The opposite
 pole is the only point on the sphere which is not mapped.  Longitudinal
 meridians become semi-infinite rays emanating from the visible
 pole, but not at their true angle since all longitudes are confined
 to lie between the two outermost rays, each of which represent the
 meridian along which the cone was cut.  The central meridian is the
 only vertical line among the longitudinal meridians.  Latitudinal circles
 become circular arcs centered on the projected pole and confined
 between the outer longitudinal meridians.  The longitudinal rays are
 equally spaced.  The latitudinal circles are too widely spaced outside 
 the standards and too closely spaced between them.  The spacing of
 the latitudinal circles increases without limit as you approach the
 opposite pole.  As its name implies, this projection is conformal.
 Neither the Mercator nor the Lambert is a true perspective
 projection.  You cannot produce them with a model containing a
 globe, some paper, and a point source of light. 
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI STEREOGR
 KEYWORD-(Valid-'STEREOGR)
 'STEREOGR requests a Stereographic projection.  If POLE (see POLE
 parameter) is not specified and if LATITUDE (see LATITUDE
 parameter) does not specify + or - 90 degrees, an Oblique
 Stereographic projection results.  The stereographic is a true
 perspective projection.  A plane is placed tangent to the sphere at
 the center of projection.  Perspective lines emanate from the point
 on the sphere diametrically opposite from the center of projection.
 Thus the entire sphere, except for one point, is mapped to the entire
 plane.  Longitudinal lines and latitudinal circles project to ellipses
 whose spacing and orientation vary in a complicated way.  The
 projection is conformal.  Features are expanded more and more
 without limit as you move away from the center of projection.
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI CYLINDRI
 KEYWORD-(Valid-'CYLINDRI)
 'CYLINDRI requests the cylindrical (normal) projection.  This
 projection maps the sphere onto a strip on the plane.  The width of
 the strip is equal to the scaled circumference of the planet at the
 equator.  Longitudinal lines project to vertical lines which are
 equally spaced and extend from one pole to the other.  Latitudinal
 circles become horizontal lines whose spacing varies as the cosine
 of the latitude.  The cylindrical projection is an equal area
 projection. 
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER used in method 1. 

.VARI RECTANGU
 KEYWORD-(Valid-'RECTANGU)
 'RECTANGU requests the Simple Cylindrical (Rectangular) projection.
 This projection is similar to the Normal Cylindrical except that
 the spacing of the latitudinal circles (horizontal lines) is constant
 with and is equal to the spacing of the longitudinal lines.
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI ORTHOGRA
 KEYWORD-(Valid-'ORTHOGRA)
 'ORTHOGRA requests an Orthographic projection.  This is a true 
 perspective projection with perspective point at infinity.  The 
 projection plane is tangent to the planet at the center of
 projection.  Perspective lines are parallel to each other,
 perpendicular to the projection plane.  Thus one hemisphere centered
 at the center of projection is mapped to a circle on the plane of
 radius Req.  Longitudinal meridians and latitudinal circles map to
 ellipses.  Features are compressed relative to their true scale as
 you move away from the center of projection.  No point more than 90
 degrees away from the center can be projected.  This projection is
 frequently used by space projects because it makes the input frame
 appear much as it would from the spacecraft if it were directly
 above the center of projection (except for the stairsteps on the
 limb.-ed.)
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER used in method 1. 

.VARI POLE
 KEYWORD-Valid:('POLE)
 'POLE is only meaningful for Orthographic and Stereographic projections.
 It causes the center of projection to be the pole nearest the center of
 the input frame.  Polar projections are treated separately by MAP3 because
 many of the equations normally used have singularities when the center of
 projection is a pole.  In both polar projections (Orthographic and
 Stereographic) longitudinal meridians are straight lines intersectiong
 at the pole at their correct angles.  Latitudinal circles project to
 complete circles centered on the pole.  The difference between the
 two lie in the spacing of the latitudinal circles and the fact that
 the orthographic only fills a circular region on the plane of
 projection, not the entire plane.
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI OBCYLIND
 KEYWORD-Valid:('OBCYLIND)
 'OBCYIND requests the Oblique simple cylindrical projection (oblique
 rectangular).  Same as the rectangular projection but the planet can be
 rotated before performing the projection.  Neither conformal nor equal area.

.VARI SINUSOID
 KEYWORD-Valid:('SINUSOID)
 'SINUSOID requests the Sinusoidal projection.  Equal area.  Latitudes are
 horizontal parallel lines.  Longitudes converge on the poles.  Scale is
 true along straight central meridian and all latitudes.

.VARI OBSINUSO
 KEYWORD-Valid:('OBSINUSO)
 'OBSINUSO requests Oblique sinusoidal projection. Same as sinusoidal except
 the sphere can be rotated before the projection is performed.

.VARI MOLLWEID
 KEYWORD-Valid:('MOLLWEID)
 'MOLLWEID requests the mollweid projection. Equal area. Latitudes are
 straight parallel lines. Longitudes converge on the poles. Scale is true
 at latitudes +/- 40 deg 44 min.

.VARI TMERCATO
 KEYWORD-Valid:('TMERCATO)
 'TMERCATO requests the Transverse mercator projection. Same as Mercator
 except the central meridian is substituted for the equator, permitting both
 poles to be seen. Central meridian, other meridians 90 degrees distant,
 and the equator are straight lines.

.VARI PERSPECT
 KEYWORD-Valid:('PERSPECT)
 'PERSPECT requests the Perspective projection, as seen from a framing camera
 not at infinity.  This is the same as the VICAR "Object Space" with this
 difference:  the Perspective projection has in its label all the data 
 required to define the projection, whereas the Image/Object-space images
 require access to the SEDR/SPICE to get this information.

.VARI SOUTH
 KEYWORD-Valid:('SOUTH) 
 'SOUTH is meaningful only for the Lambert projection when the latitudes of
 the standard parallels are defaulted. It causes the defaults to have negative
 values regardless of the latitude of the center of the input frame. This
 is a rarely used parameter. THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI SCALE
 REAL-SCALE=R1 
 Where R1 is a real number, SCALE is the scale, in km/pixel, at the
 undistorted part of the projection.
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI LINE
 REAL-LINE=R1 
 Where R1 is a real number, LINE is used as a PROJECTION DESCRIPTIVE
 PARAMETER.  Each is explained as follows:
                                  METHOD  

.VARI SAMPLE
 REAL-SAMPLE=R2
 Where R2 is a real number, SAMPLE is used as a PROJECTION DESCRIPTIVE 
 PARAMETER. Each is explained as follows:
                                 METHOD 

.VARI LATITUDE
 REAL-LATITUDE=R3
 Where R3 is a real number, LATITUDE is used as a PROJECTION DESCRIPTIVE
 PARAMETER .Each is explained as follows: 
                                 METHOD

.VARI LONGITUD
 REAL-LONGITUD=R4 
 Where R4 is a real number, LONGITUD is used as a PROJECTION DESCRIPTIVE
 PARAMETER. Each is explained as follows: 
                                 METHOD

.VARI PAR1
 REAL-PAR1=R1
 Where R1 is a real number, PAR1 is the latitude in degrees of the 
 northernmost of the two standard parallels. Default is 59.17 for northern
 hemisphere input frames and -35.83 for southern. 
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI PAR2
 REAL-PAR2=R2 
 Where R2 is a real number, PAR2 is the southernmost of the two standard
 parallels. Default is 35.83 or -59.17. PAR1 and PAR2 must have the same sign
 or the projection is undefined.  THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI LIN1
 REAL-LIN1=R3 
 Where R3 is a real number, LIN1 is the line in the output frame  at which
 the northern standard parallel's projected arc is to intersect the central
 meridian.  THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI LIN2
 REAL-LIN2=R4
 Where R4 is a real number, LIN2 is the same as LIN1 but refers to the
 southern standard parallel.  THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI NORTH
 REAL-NORTH=R5
 Where R5 is a real number, NORTH is the angle in degrees of north in the
 output picture.  This angle is measured in  the projection plane at the
 center of projection clockwise from up.  NORTH need not be related to NORA,
 discussed in the Viewing Geometry section.  The default is to compute NORTH
 so that the center of projection and another point which lies on the same
 line in the input picture as the C.P. fall on the same line in the output
 picture. As long as the C.P. lies somewhere in the input frame this
 technique minimizes relative rotation from input to output.  If, due
 to the C.P. lying near the limb, the program is unable to perform
 the above calculation, the value of NORA obtained from the SEDR or
 parameter list is used.  If the C.P. lies near a limb and no SEDR is
 available, it is risky to default NORTH.  R5 in method 6 (Polar
 Stereographic and Orthographic) is meaningless  and is ignored in
 polar projections.  THIS IS A PROJECTION DESCRIPTIVE PARAMETER (Oblique
 Stereographic and Orthographic).

.VARI PLATITUD
 REAL-PLAT=R1 
 Where R1 is a real number, PLAT is the latitude of an arbitrary point. 
 Default is the latitude of the center of the input picture. 
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI PLONGITU
 REAL-PLON=R1 
 Where R1 is a real number, PLON is the longitude of an arbitrary point. 
 Default is the longitude of the center of the input frame.  
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER used in method 7. 

.VARI PLINE
 REAL-PLINE=R1 
 Where R1 is a real number, PLINE is a line near which the point at (PLAT,PLON)
 will project.  Default is NL/2.  THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI PSAMPLE
 REAL-PSAMPLE=R1 
 Where R1 is a real number, PSAMPLE is a sample near which the point at  
 (PLAT,PLON) will project. Default is NS/2.  
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI RECENTER
 KEYWORD-Valid:('RECENTER) 
 'RECENTER turns on the recentering algorithms. It need be specified only if
 you want defaults for all of PLAT, PLON, PLINE, and PSAMPLE. Using this
 keyword alone forces the input picture to fall in the center of the output
 picture even if the LATITUDE and LONGITUDE parameters specify a point (or
 meridian) far outside the input frame.  
 THIS IS A PROJECTION DESCRIPTIVE PARAMETER. 

.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.

.VARI PRINT
The PRINT parameter prints computational information for the specified range
of output lines.  (By default this info is not printed.)  This is intended as 
an aid in interpreting the output image and as a means of checking Map3
operations.  (This data does not go into the session log.)

The first set of information printed is the GRID POINT INFO.  For a grid of
points in the output image, Map3 computes latitude, longitude, and the
corresponding line and sample coordinates in the input image.

The second set of information printed is the INFO FOR SOME OUTPUT POINTS.
This set usually contains fewer lines than the GRID POINT INFO. The two sets
are interleaved as Map3 works down through the output image.  Each set of
INFO FOR SOME OUTPUT POINTS is preceded by a header and can be distinguished
from the GRID POINT INFO by the presence of the output DN value.  (The
subsequent GRID POINT INFO follows the Output Point info without a separating
line or header.)  If the header "INFO FOR SOME OUTPUT POINTS" does not appear,
these lines in the output image are either zeros or not much greater than
the DNTHRESH value.  
There are two forms used for output point info.  If there is an asterisk
after the latitude and longitude, these values are approximate, being taken
from one of the adjacent grid points.  A better value can be obtained 
by interpolating between the adjacent four grid points, remembering that
these functions are often non-linear.  If there is no asterisk after
latitude and longitude, these values are precise.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmap3.pdf
procedure
!  TO RUN ON UNIX or Alpha, TYPE   tstmap3

refgbl $echo
refgbl $syschar
LOCAL DIR    TYPE=STRING 
LOCAL GDIR    TYPE=STRING 
LOCAL INPIC   TYPE=STRING
LOCAL OSPIC   TYPE=STRING
LOCAL OVERPIC   TYPE=STRING
LOCAL ZVPFILE   TYPE=STRING
LOCAL TIEPOINTS   TYPE=STRING
LOCAL EARTHPIC   TYPE=STRING
LOCAL SUMMATION   TYPE=STRING
LOCAL PATH       TYPE=STRING
body
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
   LET DIR   ="/project/test_work/testdata/mipl/vgr/"
   LET GDIR  ="/project/test_work/testdata/mipl/gll/"
   LET PATH  ="/project/test_work/testdata/gll/"
else 
   LET DIR   ="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
   LET GDIR  ="WMS_TEST_WORK:[TESTDATA.MIPL.GLL]"
   LET PATH  ="WMS_TEST_WORK:[TESTDATA.GLL]"
end-if
LET INPIC = "&DIR"//"f1636832.fic"      ! VGR-1 IMAGE OF IO - IMAGE SPACE
LET OSPIC = "&DIR"//"f1636832.geo"      ! GEOMETRICALLY CORRECTED= OBJECT SPACE
LET OVERPIC = "&DIR"//"io.map3"         ! Object space with overlay grid.
LET TIEPOINTS = "&DIR"//"f1636832.gpribis"
LET ZVPFILE   = "&DIR"//"jupiter.zvp"
LET EARTHPIC = "&GDIR"//"s0061515400.1"         ! image space GALILEO image.
LET SUMMATION ="&GDIR"//"s0349632122.1"         ! summation mode GALILEO image.
!
!!!FYI, OVERPIC was generated as follows from OSPIC.
!!!
!!!! place overlay grid on the object space image of IO
!!!
!!!cform inp=&OSPIC out=io.byte irange=(0,6000) orange=(0,255) +
!!!   oform=BYTE
!!!map3 io.byte io.mplab  'local +
!!!  'noproj 'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
!!!  slat=-.0962 slon=155. nora=285 cscale=84.82 +
!!!  focl=1500.19 cline=500 csamp=500 +
!!!  osscpt=(542.,609.) scale=5. north=0 'perspect long=150 lati=0 +
!!!  line=500 samp=500
!!!overlay inp=io.mplab out=&OVERPIC
!
!Because I could not access the VGR SEDR at the time of testing, I commented out
!calls to map3 that tried to access the SEDR, using !!orig as an indicator
!of these calls.  Ideally these calls will be tested at a later time.
!I replaced these calls with an alternate call that does not access the
!SEDR.

!TEST ALL DEFAULTS
!!orig............ map3 &INPIC a1.map3 'local 'distor
map3 &OVERPIC a1.map3 
label-list &OVERPIC 
label-list a1.map3 
list a1.map3 linc=100 sinc=100

! show that algorithm for default scale takes account of the input size:
size &overpic a1.dum zoom=-2
map3 a1.dum a1z.map3

!
! these files use a geometrically corrected image
! This part checks each projection for correctness of the output map.

! Polar Orthographic projection
map3 &OVERPIC a2.map3  NL=500 NS=500 'ORTH SCALE=5. +
  LINE=1.  SAMP=250. LATI=90. LONG=330. 'POLE 'NOINTERP
label-list a2.map3
list a2.map3 linc=50 sinc=50
!
! Polar Stereographic projection
map3 &OVERPIC a3.map3  NL=500 NS=500 'STER SCALE=10. +
    LINE=500. SAMP=250. LATI=-90. LONG=180. 'POLE 'SOUTH 
label-list a3.map3
list a3.map3 linc=50 sinc=50
!
! oblique orthographic
map3 &OVERPIC a4.map3  NL=500 NS=500 'ORTH SCAL=20. +
    LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30.
label-list a4.map3
list a4.map3 linc=50 sinc=50
!
! oblique stereographic
map3 &OVERPIC a5.map3  NL=500 NS=500 'STER SCAL=20. +
   LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30. 
label-list a5.map3
list a5.map3 linc=50 sinc=50
!
! Lambert projection
map3 &OVERPIC a6.map3  NL=500 NS=500 'LAMB SCALE=10. +
    LINE=500. SAMP=250. LATI=-80. LONG=150. PAR1=-30. +
    PAR2=-60. 'SOUTH 
label-list a6.map3
list a6.map3 linc=50 sinc=50
!
! Mercator projection
map3 &OVERPIC a7.map3  NL=500 NS=500 'MERC SCALE=10. +
     LATI=70. LONG=240. 
label-list a7.map3
list a7.map3 linc=50 sinc=50
!
! Test of zonal flow correction simulated on IO , Mercator projection
map3 &OVERPIC a7z.map3  NL=500 NS=500 'MERC SCALE=10. +
     LATI=70. LONG=240. reftime=(79,63,19,45,0,-999) ZVP=&ZVPFILE
label-list a7z.map3
list a7z.map3 linc=50 sinc=50
!
! Cylindrical projection; this one will have 360 lines only.
map3 &OVERPIC a8.map3  NL=360 NS=500 'CYLI SCALE=10. +
    LINE=0. SAMP=1. LATI=90. LONG=240. 
label-list a8.map3
list a8.map3 linc=50 sinc=50
!
! Rectangular projection
map3 &OVERPIC a9.map3  NL=500 NS=500 'RECT SCALE=10. +
     SAMP=1.  LATI=90. LONG=225. 
label-list a9.map3
list a9.map3 linc=50 sinc=50
!
! Oblique simple cylindrical projection
! no planet rotation, sees the center of the input.
map3 &OVERPIC a10.map3  NL=500 NS=500 'OBCY SCALE=10. +
     'RECENT 
label-list a10.map3
list a10.map3 linc=50 sinc=50
!
! Oblique simple cylindrical projection
! puts pole on equator and views the pole.
map3 &OVERPIC a11.map3  NL=500 NS=500 'OBCY SCALE=10. +
     'RECENT latitude=0. longitud=180. par1=180. +
      plat=90. plong=180.
label-list a11.map3
list a11.map3 linc=50 sinc=50
!
! Perspective projection
! defaulting all but the scale
map3 &OVERPIC a12.map3  +
  nl=500 ns=500 scale=10. 'perspect
label-list a12.map3
list a12.map3 linc=50 sinc=50
!
! Sinusoidal projection
! project so optical axis is reference meridian.
map3 &OVERPIC a13.map3  +
  nl=500 ns=500 scale=10. 'sinusoid latitude=20 longitud=150 +
  line=250 samp=250
label-list a13.map3
list a13.map3 linc=50 sinc=50
!
! Sinusoidal projection
! defaulting all but the scale
map3 &OVERPIC a14.map3  +
  nl=500 ns=500 scale=10. 'sinusoid 
label-list a14.map3
list a14.map3 linc=50 sinc=50
!
! Sinusoidal projection
! project as above but lat=10 long=130 is centered in window
map3 &OVERPIC a15.map3  +
  nl=500 ns=500 scale=10. 'sinusoid +
  'recenter plat=10 plong=130
label-list a15.map3
list a15.map3 linc=50 sinc=50
!
! Oblique Sinusoidal projection
! no rotation , observe center of input
map3 &OVERPIC a16.map3  +
  nl=500 ns=500 scale=10. 'obsinuso +
  'recenter
label-list a16.map3
list a16.map3 linc=50 sinc=50
!
! Oblique Sinusoidal projection
! put pole on equator & observe lat=-30 long=150
map3 &OVERPIC a17.map3  +
  nl=500 ns=500 scale=10. 'obsinuso +
  'recenter   latitude=0. longitud=180. par1=180. +
   plat=-30. plong=150.   
label-list a17.map3
list a17.map3 linc=50 sinc=50
!
! Mollweide projection
! center input in output
map3 &OVERPIC a18.map3  +
  nl=500 ns=500 scale=10. 'mollweid +
  'recenter   
label-list a18.map3
list a18.map3 linc=50 sinc=50
!
! Mollweide projection
! center lat=-30 long=150 use 180 as central meridian
map3 &OVERPIC a19.map3  +
  nl=500 ns=500 scale=10. 'mollweid +
  'recenter longitud=180.  +
   plat=-30. plong=150.   
label-list a19.map3
list a19.map3 linc=50 sinc=50
!
! Perspective projection
map3 &OVERPIC a20.map3  +
  nl=500 ns=500 scale=10. 'perspect +
  north=45. latitude=80. longitud=150. line=200 samp=200
label-list a20.map3
list a20.map3 linc=50 sinc=50
!
! Perspective projection
map3 &OVERPIC a21.map3  +
  nl=500 ns=500 scale=10. 'perspect +
  north=45. latitude=80. longitud=150. 'RECENTER
label-list a21.map3
list a21.map3 linc=50 sinc=50
!
! Transverse Mercator projection
! central meridian defaults to p5 point
map3 &OVERPIC a22.map3  +
  nl=500 ns=500 scale=10. 'tmercato
label-list a22.map3
list a22.map3 linc=50 sinc=50
!
! Transverse Mercator projection
! central meridian set to 150
map3 &OVERPIC a23.map3  +
  nl=500 ns=500 scale=10. 'tmercato +
  latitude=30. longitud=150.
label-list a23.map3
list a23.map3 linc=50 sinc=50
!
! Transverse Mercator projection
! recenter
map3 &OVERPIC a24.map3  +
  nl=500 ns=500 scale=10. 'tmercato 'recenter +
   latitude=20. longitud=180.  +
   plat=-30. plong=150.   
label-list a24.map3
list a24.map3 linc=50 sinc=50
!
! these test pointing options
! test FARENC mode in image space with geom file
!!orig............ map3 (&INPIC,&TIEPOINTS) a25.map3 +
!!orig............  'local NL=360 NS=500 'CYLI 'HALF SCALE=10. +
!!orig............   LINE=0. SAMP=250. LATI=90. LONG=150 +
!!orig............   NORA=15.06 ISSCP=(437.90,494.89) 
map3 (&INPIC,&TIEPOINTS) a25.map3 +
     NL=360 NS=500 'CYLI SCALE=10. +
    'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
     slat=-.0962 slon=155. cscale=84.82 +
     focl=1500.19 cline=500 csamp=500 +
     LINE=0. SAMP=250. LATI=90. LONG=150 +
     NORA=285.06 ISSCP=(437.90,494.89) 
label-list a25.map3
list a25.map3 linc=50 sinc=50
!
! Stereographic projection farenc mode 
! in image space with nominal geometric correction
!!orig............ map3 &INPIC a26.map3 'local NL=500 NS=500 'STER 'HALF SCALE=5. +
!!orig............     LINE=10. SAMP=250. LATI=90. LONG=320. 'POLE 'distor +
!!orig............     OSSCPT=(542.00,608.73) 
map3 &INPIC a26.map3 NL=500 NS=500 'STER SCALE=5. +
     LINE=10. SAMP=250. LATI=90. LONG=320. 'POLE 'distor +
     'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
     slat=-.0962 slon=155. cscale=84.82 +
     focl=1500.19 cline=500 csamp=500 +
     OSSCPT=(542.00,608.73) nora=285.06
label-list a26.map3
list a26.map3 linc=50 sinc=50
!
! Lambert projection tiepoints mode in object space
map3 &INPIC a27.map3  NL=500 NS=500 'LAMB SCALE=10. +
    LINE=1.0 SAMP=250. LATI=80. LONG=140. PAR1=60. PAR2=30. +
    'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
     slat=-.0962 slon=155. cscale=84.82 +
     focl=1500.19 cline=500 csamp=500 +
     OSSCPT=(542.00,608.73) nora=285.06 +
    TIEP=(498.56,498.59 2.5138,179.2941 +
          497.67,731.78 ,15.0845,132.3491 +
          613.97,614.03 ,-13.7929,150.1308 +
          729.82,496.08 ,-47.3593,172.8527 +
          728.87,730.38 ,-31.1938,112.3693) 
label-list a27.map3
list a27.map3 linc=50 sinc=50
!
! Rectangular projection
map3 &INPIC a28.map3  NL=500 NS=500 'RECT SCALE=10. +
    'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
     slat=-.0962 slon=155. cscale=84.82 +
     focl=1500.19 cline=500 csamp=500 +
   LINE=-1. SAMP=1. LATI=90. LONG=225. OSSCP=(542.00,608.73)  NORA=285.06 
label-list a28.map3
list a28.map3 linc=50 sinc=50
!
! Quam algorithm
map3 &INPIC a29.map3  NL=500 NS=500 'MERC SCAL=10. +
    'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
     slat=-.0962 slon=155. cscale=84.82 +
     focl=1500.19 cline=500 csamp=500 +
LATI=70. LONG=240. 'QUAM CLATITUD=2.31 CLONGITU=178.91  NORA=285.06 
label-list a29.map3
list a29.map3 linc=50 sinc=50
!
! THIS PART USES GEOMA FILE FOR GEOMETRIC CORRECTION
! Polar Orthographic projection with geoma file
!!orig............ map3 (&INPIC,&TIEPOINTS) a30.map3 'local NL=500 NS=500 'ORTH 'HALF SCALE=5. + 
!!orig............  LINE=1. SAMP=250. LATI=90. LONG=340. 'POLE 
map3 (&INPIC,&TIEPOINTS) a30.map3 NL=500 NS=500 'ORTH SCALE=5. + 
     LINE=1. SAMP=250. LATI=90. LONG=340. 'POLE +
    'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
     slat=-.0962 slon=155. nora=285 cscale=84.82 +
     focl=1500.19 cline=500 csamp=500 +
     osscpt=(542.,609.) 
label-list a30.map3
list a30.map3 linc=50 sinc=50
!
! THIS PART TESTS NOMINAL GEOMETRIC CORRECTION
! Mercator projection and specify planet to get SEDR info
!!orig............ map3 &INPIC a31.map3 'local NL=500 NS=500 'MERC 'HALF SCALE=10. +
!!orig............    LATI=70. LONG=240. 'DISTOR 
map3 &INPIC a31.map3 NL=500 NS=500 'MERC SCALE=10. +
     LATI=70. LONG=240. 'DISTOR  +
     'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
      slat=-.0962 slon=155. nora=285 cscale=84.82 +
      focl=1500.19 cline=500 csamp=500 +
      osscpt=(542.,609.) 
label-list a31.map3
list a31.map3 linc=50 sinc=50
!
WRITE " ******* TEST DISTOR/GEOM FILE, IT SHOULD USE THE GEOM FILE****"
WRITE " THIS OUTPUT FILE WILL NOT BE A CORRECT PROJECTION"
!!orig............ map3 (&OSPIC,&TIEPOINTS) a32.map3 'local
map3 (&OSPIC,&TIEPOINTS) a32.map3 +
     'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
      slat=-.0962 slon=155. nora=285 cscale=84.82 +
      focl=1500.19 cline=500 csamp=500 +
      osscpt=(542.,609.) 
list a32.map3 linc=50 sinc=50
!
!TEST of GLL project 
map3 &EARTHPIC a33.map3 'REMOTE CKNAME=FARE 'ORTH
list a33.map3 linc=100 sinc=100
!
map3 &EARTHPIC a33f.map3 ISSCP=(420,430) 'REMOTE CKNAME=FARE 'ORTH
list a33f.map3 linc=100 sinc=100
!
! Test perspective projection fix for FR 90527.
! Put new subspacecraft point at pixel (251,251) of output.
! Print Grid Point Info using PRINT parameter.
!Since SCALE parameter is .1, the distance between pixel (251,251) and pixel
! (251,261) should be 1.0 km.  Verify that map3 writes to the screen
! (not to session log) the grid point info with the following two lines
! near the middle.  (Floating point values may differ slightly between
! different machines.)
!   251   251         -34.06600    39.97998   463.20178   486.52213
!   251   261         -34.06753    39.99066   463.19708   486.55682
!
! The distance component from longitude between these two points is
! computed:  delta = 39.99066 - 39.97998 = 0.01068 degrees.
!            distance from lon. = sin(delta) * cos(latitude) * planet radius
!                               = sin(.01068)*cos(-34.066) * 6378.14 km
!                               = .9867 km
!
! The distance component from latitude between these two points is
! computed:  delta = -34.06600 - 34.06753 = 0.00153 degrees.
!            distance from lat. = sin(delta) * planet radius
!                               = sin(.00153) * 6378.14 km
!                               = .1670 km
!
! The total distance is SQRT( .9867**2 + .1670**2 ) = 1.00 km. (approx.)
! This shows that the program is generating an output image with the
! proper scale.
! 
map3 &EARTHPIC a33p.map3 'REMOTE CKNAME=FARE lat=-34.066 long=39.98 +
  nl=500 ns=500 scale=.1 'perspect line=251 samp=251 print=(251,251)
label-list a33p.map3
list a33p.map3 linc=50 sinc=50
!
map3 &SUMMATION a33s.map3 (1,1,400,400) 'REMOTE CKNAME=FARE  'ORTH
list a33s.map3 linc=50 sinc=50   
write "Make sure center longitude <= 360"
label-list a33s.map3

!Test of noproject & nosedr
! (NOTE:  'nosedr is used above in all runs that used the &INPIC input,
! as long as the VGR SEDR is not working.  When that has been implemented,
! the 'nosedr parameter should be removed, along with other changes
! preserved in the lines labelled "!orig")  -- LWK
map3 &INPIC a34.map3 (1,1,500,500)  +
  'noproj 'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
  slat=-.0962 slon=155. cscale=84.82 +
  focl=1500.19  NORA=285.06 cline=500 csamp=500 +
  osscpt=(542.,609.) scale=10. north=0 'orth long=150 lati=0 +
  line=250 samp=250
list a34.map3 linc=50 sinc=50
!

!Testing the DNTHRESH parameter:
!dn_threshold_test.img is a special Galileo 400x400 Sum Mode image. 
!    This test image has data bordered on all sides by 
!    a 20-pixel wide swath of DN=5, which represents "nondata".  The 
!    test will be whether parm DNTHRESH can threshold the data from 
!    the "nondata" when MAP3 interpolates during the rubber-sheeting 
!    process.
!
!    If the test succeeds, the output image "out.img" will have sharp
!    edges where data meets "nondata".  If the test failed, the edges
!    will be blurry, as data would have been erroneously interpolated
!    with the bordering "nondata".
     
map3 &"path"dn_threshold_test.img out.img (1,1,600,700) +
     'RECT NORTH=0 CKNAME="DAVI" SPICE=remote DNTHRESH=5


WRITE "*********************************************************************"
write "NOTE TO TESTERS: THE FOLLOWING CALLS TO MAP3 SHOULD 'ABEND GRACEFULLY'"

! Lambert projection tiepoints mode in object space
map3 &OVERPIC a27x.map3  NL=500 NS=500 'LAMB SCALE=10. +
    LINE=1.0 SAMP=250. LATI=80. LONG=140. PAR1=60. PAR2=30. +
    TIEP=(318,278   30 195 +
          317,723   30 105 +
          500,500    0 150 +
          681,276  -30 105 +
          681,724  -30 105 )
!
! Rectangular projection
map3 &OVERPIC a28x.map3  NL=500 NS=500 'RECT SCALE=10. +
   LINE=-1. +
SAMP=1. LATI=90. LONG=225. OSSCP=(542.00,608.73) NORA=15.06 
!
! Quam algorithm
map3 &OVERPIC a29x.map3  NL=500 NS=500 'MERC SCAL=10.+
LATI=70. LONG=240. 'QUAM CLATITUD=2.31 CLONGITU=178.91 NORA=15.06
!
!Test of noproject & nosedr
map3 &OVERPIC a34x.map3 (1,1,500,500)  +
  'noproj 'nosedr radi=(1830.,1818.7,1815.3) rmag=806022. +
  slat=-.0962 slon=155. cscale=84.82 +
  focl=1500.19 nora=0 cline=500 csamp=500 +
  osscpt=(542.,609.) scale=10. north=0 'orth long=150 lati=0 +
  line=250 samp=250
!
map3 &SUMMATION a33s.map3 (1,1,400,400) 'REMOTE CKNAME=FARE  'ORTH 'POLE
!
map3 &SUMMATION a33s.map3 (1,1,400,400) 'REMOTE CKNAME=FARE  'STER 'POLE

end-proc
$ Return
$!#############################################################################
