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
