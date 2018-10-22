CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to display the image
c Return indicator IND=0 if no action, =1 if image is redisplayed,
C =2 if other.
C
      SUBROUTINE DISPLAY(PIC,HPIC,NL,NS,ind)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB

      COMMON/CHIST/HIS(256),HFLG,HBEG,HINC,NSPIKES,LHPER(2)
      INTEGER HIS,HFLG,HBEG,HINC
      REAL*4 LHPER

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CONST/PI,DTR,RTD

      LOGICAL XST,XDIFILL,PARMTST,RPARMTST,XVIPTST,XDCSET

      INTEGER*4 HIST(-32768:32767)
      INTEGER LOHI(2)

      IND = 2

      IF (PARMTST('CZOOM',IVAL,I)) THEN		!Trackball zoom routine
         IF (IVAL.EQ.0) THEN
            CALL XVMESSAGE('***CZOOM=0 is invalid',' ')
            RETURN
         ENDIF
         CALL CURSOR(RLINE,RSAMP)		! Read cursor position
	 IZOOM = MIN0(IVAL,4)			! Get zoom factor
	 ZOOM = IZOOM		
         IF (IZOOM.LT.0) ZOOM = -1.0/IZOOM
	 Z = 2.*ZOOM
	 SL = RLINE - NLDS/Z			! and center display
	 SS = RSAMP - NSDS/Z			! about it
         IF (SS.LT.1) SS=1
         IF (SL.LT.1) SL=1
	 CALL DPIC(PIC,SL,SS,NL,NS)		! Display image
         XST = XDCSET(IDEV,TB,NSDS/2,NLDS/2)	! Center cursor
         HFLG = 0
         IND = 1
         RETURN
      ENDIF

      IF (PARMTST('STRETCH',lohi,I)) THEN	! STRECH picture
         IF (ICODE.EQ.2) THEN
            CALL HSTRETCH(HPIC,SL,SS,NL,NS,	! Scale HPIC to PIC
     &		pic,lohi(1),lohi(2),nlev,hbeg,hinc)
            CALL DPIC(PIC,SL,SS,NL,NS)	! Display image
            HFLG = 0
         ENDIF
         CALL STRECH(LOHI(1),LOHI(2),stbl)
         CALL LUTWRITE(IDEV,STBL)
         RETURN
      ENDIF

      IF (RPARMTST('ASTRETCH',LHPER,I)) THEN	! STRECH picture
         IF (ICODE.EQ.1) THEN
            CALL HISTGEN1(PIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,his,hflg)
            CALL ASTRCH(HIS,ilow,ihigh,LHPER(1),LHPER(2),256)
         ELSE
            CALL HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
            CALL ASTRC2(HIST,NFREQ,LHPER(1),LHPER(2),ilow,ihigh)
            CALL HSTRETCH(HPIC,SL,SS,NL,NS,
     &		pic,ilow,ihigh,nlev,hbeg,hinc)
            DO J=0,255	!Compress the histogram to 256 grey-levels
               CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J+1),1)
            ENDDO
            CALL DPIC(PIC,SL,SS,NL,NS)	!Redisplay image
            HFLG = 1
         ENDIF
         CALL STRECH(ILOW,IHIGH,STBL)
         CALL LUTWRITE(IDEV,STBL)
         RETURN
      ENDIF

      IF (XVIPTST('H'))	THEN		!Restore original picture
         IF (ICODE.EQ.1) THEN
            ILOW = 0			!Restore the original
            IHIGH = 255			!stretch limits
         ELSE
            SL = 1
            SS = 1
            IZOOM = MAX0((NL-1)/NLDS,(NS-1)/NSDS) + 1
            IF (IZOOM.GT.1) THEN
               ZOOM = 1.0/IZOOM
               IZOOM = -IZOOM
            ELSE
               ZOOM = 1.0D0
               IZOOM = 1
            ENDIF
            CALL HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
            CALL ASTRC2(HIST,NFREQ,0.5,0.5,ilow,ihigh)
            CALL HSTRETCH(HPIC,SL,SS,NL,NS,
     &		pic,ilow,ihigh,nlev,hbeg,hinc)
            DO J=0,255	!Compress the histogram to 256 grey-levels
               CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J+1),1)
            ENDDO
         ENDIF
         CALL HOME(PIC,NL,NS,NLDS,NSDS,sl,ss,izoom,zoom)
         CALL STRECH(ILOW,IHIGH,STBL)
         CALL LUTWRITE(IDEV,STBL)
         HFLG = 0
         IND = 1
         RETURN
      ENDIF

      IF (PARMTST('SPIKES',N,I)) NSPIKES=N

      IF (XVIPTST('HIST')) THEN  !Compute and display the histogram
         IF (ICODE.EQ.1) THEN
            CALL HISTGEN1(PIC,SL,SS,NL,NS,NLDS,NSDS,IZOOM,ZOOM,
     &		his,HFLG)
         ELSEIF (HFLG.EQ.0) THEN
            CALL HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
            DO J=0,255	!Compress the histogram to 256 grey-levels
               CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J+1),1)
            ENDDO
         ENDIF
         CALL HDISPLAY(IDEV,G,HIS,101,101,100,HBEG,HINC,NSPIKES)
      ENDIF

      IF (XVIPTST('GERASE')) THEN
         XST = XDIFILL(IDEV,G,XDB)
         IND = 1
         RETURN
      ENDIF

      IF (XVIPTST('SRES')) THEN
         CALL DRAWCURVE(RES,202,0)
         IND = 2
         RETURN
      ENDIF

      IF (XVIPTST('SBLEMS')) THEN
         CALL DRAWBLEMS(RES,BLEM,NBLEMS,3)
         IND = 2
         RETURN
      ENDIF

      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to support display of the planet
C Return indicator IND=0 if no action, =1 if image is redisplayed,
C =2 if other
C
      SUBROUTINE PDISPLAY(PIC,HPIC,NL,NS,IND) 
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CPAR/PAR(20),PAR2(20)
      REAL*4 PAR,PAR2

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CONST/PI,DTR,RTD
      LOGICAL XST,PARMTST,RPARMTST,XVIPTST,XDCSET

      REAL*4 R4
      include 'fortport'

      CHARACTER*80 MSG
  110 FORMAT('(L,S)=(',F8.2,',',F8.2,')  (LAT,LON)=(',
     &         F6.2,',',F7.2,')')
  112 FORMAT('PHASE=(',F7.2,')   DN=(',I6,')   RANGE=(',
     &         F10.1,')   SCALE=(',F8.1,')')  !VRH 3/1/94 new data

      IND = 2

      IF (XVIPTST('C')) THEN
   10    CALL CURSOR(RLINE,RSAMP)
         CALL LATLON(ISTATUS,RLINE,RSAMP,RLAT,RLON)
         IF (ISTATUS.EQ.1) THEN
c             CALL DRAWDOT(RLINE,RSAMP,255)  !VRH 7/31/89 remove drawing dot
             R = GEODET(RLAT,RLON)
             WRITE(MSG,110) RLINE,RSAMP,R*RTD,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RLAT,RLON,PHA,RANGE,SCALE,DUMMY) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN,RANGE,SCALE !VRH 3/1/94 new data
             CALL XVMESSAGE(MSG,' ')
         ENDIF
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 10
         RETURN
      ENDIF

      IF (PARMTST('OVERLAY',int,i)) THEN  !Overlay planet with lat-lon grid
         CALL OVERLAY1(INT)
         RETURN
      ENDIF

      IF (RPARMTST('LATI',r4,i)) THEN
	RLAT = R4
        CALL OVERLAT(RLAT*DTR)
        RETURN
      ENDIF

      IF (RPARMTST('LONG',r4,i)) THEN
	RLON = R4
        CALL OVERLON(RLON*DTR)
        RETURN
      ENDIF

      IF (RPARMTST('LL',par,i)) THEN		! User inputs (lat,lon)
         RLAT = PAR(1)
         RLON = PAR(2)*DTR
         IF (DABS(RLAT).GT.90.D0) THEN
            CALL XVMESSAGE('***Invalid latitude value',' ')
            RETURN
         ENDIF
         RLAT = GEOCEN(RLAT*DTR,RLON)
         CALL LINSAM(ISTATUS,RLAT,RLON,RLINE,RSAMP) !Compute corresponding (l,s)
         IF (ISTATUS.EQ.1) THEN
             WRITE(MSG,110) RLINE,RSAMP,PAR(1),PAR(2)
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RLAT,RLON,PHA,RANGE,SCALE,DUMMY) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN,RANGE,SCALE !VRH 3/1/94 new data
             CALL XVMESSAGE(MSG,' ')
             ILINE = (RLINE-SL)*ZOOM + 1.5
             ISAMP = (RSAMP-SS)*ZOOM + 1.5
             IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         ENDIF
         RETURN
      ENDIF

      IF (RPARMTST('LS',par,i)) THEN
         RLINE = PAR(1)
         RSAMP = PAR(2)
         CALL LATLON(ISTATUS,RLINE,RSAMP,RLAT,RLON)
         IF (ISTATUS.EQ.1) THEN
            R = GEODET(RLAT,RLON)
            WRITE(MSG,110) RLINE,RSAMP,R*RTD,RLON*RTD
            CALL XVMESSAGE(MSG,' ')
            CALL PHASE(RLAT,RLON,PHA,RANGE,SCALE,DUMMY) !VRH 3/1/94 new arg
            IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
               IDN = 0
            ELSE IF (ICODE.EQ.1) THEN
               IDN = BYTE2INT(PIC(RSAMP,RLINE))
            ELSE
               IDN = HPIC(RSAMP,RLINE)
            ENDIF
            WRITE(MSG,112) PHA*RTD,IDN,RANGE,SCALE !VRH 3/1/94 new data
            CALL XVMESSAGE(MSG,' ')
         ENDIF
         ILINE = (RLINE-SL)*ZOOM + 1.5
         ISAMP = (RSAMP-SS)*ZOOM + 1.5
         IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         RETURN
      ENDIF

      IND = 0
      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to support display of the ring plane
C Return indicator IND=0 if no action, =1 if image is redisplayed, =2 if other
C
      SUBROUTINE RDISPLAY(PIC,HPIC,NL,NS,IND)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRINGC/ RINGS
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CONST/PI,DTR,RTD
      REAL*4 PAR(2)     !VRH added to support RL,LS options 1/19/89
      LOGICAL XST,RPARMTST,XVIPTST,XDCSET

      INCLUDE 'fortport'

      CHARACTER*80 MSG
  110 FORMAT('(L,S)=(',F7.2,',',F7.2,') (R,LON)=(',
     &         F10.1,',',F7.2,')')
  111 FORMAT('Reference plane is that of ',A1,'-Ring')
  112 FORMAT('PHASE=(',F7.2,')   DN=(',I6,')')
  113 FORMAT(' RANGE=(',F10.1,')   SCALE=(',
     &         F8.1,')   RSCALE=(',F8.1,')') !VRH 3/1/94
      IND = 2

      IF (XVIPTST('C')) THEN
         CALL GETRING(MRING)
         IF (MRING.EQ.1) THEN
             CALL XVMESSAGE('Reference plane is planet''s equator',' ')
         ELSE
             WRITE(MSG,111) RINGS(MRING)
             CALL XVMESSAGE(MSG,' ')
         ENDIF

   10    CALL CURSOR(RLINE,RSAMP)
c         CALL DRAWDOT(RLINE,RSAMP,255)  !VRH 7/31/89 remove drawing dot
         CALL LATLON(ISTATUS,RLINE,RSAMP,RADIUS,RLON)
         IF (ISTATUS.EQ.1) THEN
             WRITE(MSG,110) RLINE,RSAMP,RADIUS,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,113) RANGE,SCALE,RSCALE !VRH 3/1/94
             CALL XVMESSAGE(MSG,' ')
         ENDIF
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 10
         RETURN
      ENDIF

      IF (RPARMTST('RL',PAR,I)) THEN		!User inputs (RADIUS,LON)
         RADIUS = PAR(1)                        !VRH added RL option 1/19/89
         RLON = PAR(2)*DTR
         CALL GETRING(MRING)
         CALL LINSAM(ISTATUS,RADIUS,RLON,rline,rsamp) !Compute (l,s)
         IF (ISTATUS.EQ.1) THEN
             IF (MRING.EQ.1) THEN
                 CALL XVMESSAGE('Reference plane is planet''s equator',
     &		  ' ')
             ELSE
                 WRITE(MSG,111) RINGS(MRING)
                 CALL XVMESSAGE(MSG,' ')
             ENDIF
             WRITE(MSG,110) RLINE,RSAMP,RADIUS,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,113) RANGE,SCALE,RSCALE !VRH 3/1/94
             CALL XVMESSAGE(MSG,' ')
             ILINE = (RLINE-SL)*ZOOM + 1.5
             ISAMP = (RSAMP-SS)*ZOOM + 1.5
             IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         ENDIF
         RETURN
      ENDIF

      IF (RPARMTST('LS',PAR,I)) THEN ! VRH added LS option to RDISPLAY 1/19/89
         RLINE = PAR(1)
         RSAMP = PAR(2)
         CALL GETRING(MRING)
         CALL LATLON(ISTATUS,RLINE,RSAMP,RADIUS,RLON)
         IF (ISTATUS.EQ.1) THEN
             IF (MRING.EQ.1) THEN
                 CALL XVMESSAGE('Reference plane is planet''s equator',
     &		  ' ')
             ELSE
                 WRITE(MSG,111) RINGS(MRING)
                 CALL XVMESSAGE(MSG,' ')
             ENDIF
             WRITE(MSG,110) RLINE,RSAMP,RADIUS,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,113) RANGE,SCALE,RSCALE !VRH 3/1/94
             CALL XVMESSAGE(MSG,' ')
         ENDIF
         ILINE = (RLINE-SL)*ZOOM + 1.5
         ISAMP = (RSAMP-SS)*ZOOM + 1.5
         IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         RETURN
      ENDIF

      IF (XVIPTST('PROFILE')) THEN
          CALL PROFILE(PIC,HPIC,NL,NS)
          RETURN
      ENDIF

      IF (XVIPTST('PHASPROF')) THEN
          CALL PHASPROF(PIC,HPIC,NL,NS)
          RETURN
      ENDIF

      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to support display of the celestial sphere.
C Return indicator IND=0 if no action, =1 if image is redisplayed,
C =2 if other
C
      SUBROUTINE SDISPLAY(PIC,HPIC,NL,NS,IND) 
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG

      COMMON/CSTAR1/ISAO,SAOFILE
      CHARACTER*72 SAOFILE

      COMMON/CSTAR2/STARNAME,STARTYPE
      CHARACTER*13 STARNAME(2000)
      CHARACTER*3 STARTYPE(2000)

      COMMON/CONST/PI,DTR,RTD
      REAL*4 PAR(2)

      LOGICAL XST,RPARMTST,XVIPTST,XDCSET

CBTC  ...convert *TO* GSC or SAO reference frame 
      logical from /.false./
      CHARACTER*7 CSYSTEM

      INCLUDE 'fortport'

      CHARACTER*80 MSG
  110 FORMAT('(L,S)=(',F8.2,',',F8.2,')  RA=',I2,':',I2.2,':',F4.1, 
     &         ' DEC=',I3,' deg',I3,'''',F4.1,'"',1x,a7)
  112 FORMAT('DN=(',I6,')')

      IF (ISAO .EQ. -2) THEN
         CSYSTEM = '(J2000)'
      ELSE
         CSYSTEM = '(EME50)'
      ENDIF

      IND = 2

      IF (XVIPTST('C')) THEN
   10    CALL CURSOR(rline,rsamp)
c         CALL DRAWDOT(RLINE,RSAMP,255)  !VRH 7/31/89 remove drawing dot
         CALL LATLON(ISTATUS,RLINE,RSAMP,ra,dec)
CBTC
         RA0 = ra
         DEC0 = dec
CBTC  ...convert *TO* Catalog (SAO or GSC) reference frame
         call fromorto_star( ra, dec, from)
CBTC
         IF (ISTATUS.EQ.1) THEN
             CALL RADTOHMS(RA,ih,im,rs)
             CALL RADTODMS(DEC,ideg,imin,rsec)
             WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC,csystem
CBTC             WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC
             IF (MSG(37:37) .EQ. ' ') MSG(37:37) = '0'
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,'(a33,2f8.3,a,2f8.3)') 
     &         'RA,DEC '//csystem// ' // (local):  '
     &         , ra*rtd, dec*rtd, ' // ', ra0*rtd, dec0*rtd
             CALL XVMESSAGE(MSG,' ')
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) IDN
             CALL XVMESSAGE(MSG,' ')
C		****Temporary code for debugging proper motion***
         RMIN = 1.E+10
         IMIN = 1
         DO I=1,NSTARS		!Find nearest star in catalog
             R = (SPTS(1,I)-RLINE)**2 + (SPTS(2,I)-RSAMP)**2
             IF (R.LT.RMIN) THEN
                RMIN = R
                IMIN = I
             ENDIF
         ENDDO
CBTC
         ra = stars(1,imin)
         dec = stars(2,imin)
         RA0 = ra
         DEC0 = dec
CBTC
CBTC convert to catalog reference frame
         call fromorto_star( ra, dec, from)
         WRITE(MSG,'(a,i4,a)') 'Closest star info: (star #',IMIN,')'
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,'(a33,2f8.3,a,2f8.3)') 
     &         'RA,DEC '//csystem// ' // (local):  '
     &         , ra*rtd, dec*rtd, ' // ', ra0*rtd, dec0*rtd
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,'(a,f8.2,a1,f8.2,a1)') '(L,S)=('
     &             , spts(1,imin), ',', spts(2,imin), ')'
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,'(a,f8.3)') 'MAG = ', stars(3,imin) 
         IF (STARNAME(IMIN) .NE. ' ')
     &       WRITE(MSG(15:),113) STARNAME(IMIN)
         IF (STARTYPE(IMIN).NE.' ') 
     &      WRITE(MSG(36:),114) STARTYPE(IMIN)
  113    FORMAT(' NAME = ',a13)
  114    FORMAT(' TYPE = ',a3)
         CALL XVMESSAGE(MSG,' ')
CBTC
CBTC         WRITE(MSG,111) IMIN,(STARS(J,IMIN)*RTD,J=1,2),STARS(3,IMIN)
CBTC  111 FORMAT('Closest star is ',I3,' (RA,DEC,MAG)=',3F8.3,' (EME50)')
CBTC         CALL XVMESSAGE(MSG,' ')

         ENDIF
C		***End temporary code ***
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 10
         RETURN
      ENDIF

      IF (RPARMTST('RD',PAR,I)) THEN		!User inputs (RA,DEC)
         RA = PAR(1)*DTR
         DEC = PAR(2)*DTR
         CALL LINSAM(ISTATUS,RA,DEC,rline,rsamp) !Compute corresponding (l,s)
         IF (ISTATUS.EQ.1) THEN
             CALL RADTOHMS(RA,ih,im,rs)
             CALL RADTODMS(DEC,ideg,imin,rsec)
             WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC
             IF (MSG(37:37) .EQ. ' ') MSG(37:37) = '0'
             CALL XVMESSAGE(MSG,' ')
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) IDN
             CALL XVMESSAGE(MSG,' ')
             ILINE = (RLINE-SL)*ZOOM + 1.5
             ISAMP = (RSAMP-SS)*ZOOM + 1.5
             IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         ENDIF
         RETURN
      ENDIF

      IF (RPARMTST('LS',PAR,I)) THEN
         RLINE = PAR(1)
         RSAMP = PAR(2)
         CALL LATLON(ISTATUS,RLINE,RSAMP,ra,dec)
         IF (ISTATUS.EQ.1) THEN
            CALL RADTOHMS(RA,ih,im,rs)
            CALL RADTODMS(DEC,ideg,imin,rsec)
            WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC
            IF (MSG(37:37) .EQ. ' ') MSG(37:37) = '0'
            CALL XVMESSAGE(MSG,' ')
            IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
               IDN = 0
            ELSE IF (ICODE.EQ.1) THEN
               IDN = BYTE2INT(PIC(RSAMP,RLINE))
            ELSE
               IDN = HPIC(RSAMP,RLINE)
            ENDIF
            WRITE(MSG,112) IDN
            CALL XVMESSAGE(MSG,' ')
         ENDIF
         ILINE = (RLINE-SL)*ZOOM + 1.5
         ISAMP = (RSAMP-SS)*ZOOM + 1.5
         IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         RETURN
      ENDIF

      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generic plot routine
C Routine to draw a plot onto graphics screen.
C Outputs: X0,Y0,DX,DY plot transformation on graphics
C
      SUBROUTINE PLOTDISP(PTS,NVAL,NPTS,ix,iy,BADVAL1,L0,S0
     &                   ,X0,Y0,DX,DY)
      include 'fortport'        ! defines int2byte

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      REAL*8 PI,DTR,RTD
      COMMON/CONST/PI,DTR,RTD
C
      INTEGER*4 NVAL,NPTS, ix, iy
      REAL*4 PTS(NVAL,NPTS), BADVAL1
      INTEGER*4 L0,S0
      REAL*4 X0,Y0,DX,DY
C
      INTEGER*4 XX(2),YY(2), SLEN
      INTEGER ILOG
      REAL*4 MAX,MIN,XDIFF,YDIFF
      REAL*4 SDIVX,BDIVX,SDIVY,BDIVY,NUM
      LOGICAL XST,XDIPOLYLINE,XDTROTATE,XDTTEXT
      CHARACTER*6 NUMB

      bmdn = int2byte(xdw)

      LLEN = NINT( NLDS * 0.4)
      L1 = L0 - LLEN
      SLEN = NINT( NSDS * 0.4)
      IF ((NPTS/NSDS).GT.2) SLEN = NINT( NSDS * 0.8)
      S1 = S0 + SLEN

      XX(1) = S0
      YY(1) = L0
      XX(2) = S1 
      YY(2) = L0
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XX(2) = S0
      YY(2) = L1
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XST = XDTTEXT(IDEV,G,S0-8,L1-3,1,2,'DN')
c
c      IF (RPLOT) THEN
c        CALL XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'R')
c      ELSE
c        CALL XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'L')
c      ENDIF
C 
      X0 = PTS(1,1)
      XDIFF = PTS(1,NPTS) - X0
c
c      IF(.NOT.RPLOT) THEN
c        X0 = RTD*X0
c        XDIFF = RTD*XDIFF
c      END IF
c
      ISIGN = NINT(XDIFF/ABS(XDIFF))
      IF (ABS(XDIFF).LT.2.) XDIFF = 2.*ISIGN
      ILOG = INT(LOG10(ABS(XDIFF)))
      NUM  = ABS(XDIFF)/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVX = 0.1*10.**ILOG
        BDIVX = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVX = 0.2*10.**ILOG
        BDIVX = 1.0*10.**ILOG
      ELSE
        SDIVX = 0.5*10.**ILOG
        BDIVX = 2.0*10.**ILOG
      ENDIF
      IF (XDIFF.LT.0.) THEN
        SDIVX = -SDIVX
        BDIVX = -BDIVX
      ENDIF
      IF (MOD(X0,SDIVX).EQ.0) THEN
         X0 = X0 - SDIVX*ISIGN
         XDIFF = XDIFF + SDIVX*ISIGN
      ENDIF
      X0 = INT(X0/SDIVX)*SDIVX
      DX = (INT((X0+XDIFF+SDIVX)/SDIVX)*SDIVX-X0)/FLOAT(SLEN)

      DO I = 1,100
        NUM = X0 + (I-1)*SDIVX
        XX(1) = S0 + (NUM-X0)/DX + 0.5
        IF(XX(1).GT.S1) GOTO 100
        YY(1) = L0
        XX(2) = XX(1)
        YY(2) = L0 + 2
        IF(MOD(NINT(NUM/SDIVX),NINT(BDIVX/SDIVX)).EQ.0) THEN !VRH 7/28/89
           YY(2) = L0 + 5
c
            WRITE(NUMB,'(F6.1)') X0+(I-1)*SDIVX
c           IF (RPLOT) THEN
c               WRITE(NUMB,'(I6)') NINT(X0+(I-1)*SDIVX)
c           ELSE
c               WRITE(NUMB,'(F6.1)') X0+(I-1)*SDIVX
c           ENDIF
c
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,XX(1),L0+15,2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC         call mvcl(numb,number,1)
CBTC         CALL XDTTEXT(IDEV,G,XX(1)-4*NCHAR,L0+15,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

  100 MAX = -32768.
      MIN =  32767.
      DO I = 1,NPTS
        IF(PTS(2,I).GE.MAX) MAX = PTS(2,I)
        IF(PTS(2,I).LE.MIN) MIN = PTS(2,I)
      ENDDO

      Y0 = MIN
      YDIFF = MAX - MIN
      IF (YDIFF.LT.2.) YDIFF = 2.
      ILOG = INT(LOG10(YDIFF))
      NUM = YDIFF/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVY = 0.1*10.**ILOG
        BDIVY = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVY = 0.2*10.**ILOG
        BDIVY = 1.0*10.**ILOG
      ELSE
        SDIVY = 0.5*10.**ILOG
        BDIVY = 2.0*10.**ILOG
      ENDIF
      IF (MOD(Y0,SDIVY).EQ.0.OR.Y0.LT.0.) THEN
         Y0 = Y0 - SDIVY
         YDIFF = YDIFF + SDIVY
      ENDIF
      Y0 = INT(Y0/SDIVY)*SDIVY
      DY = -(INT((Y0+YDIFF+SDIVY)/SDIVY)*SDIVY-Y0)/FLOAT(LLEN)

      XST = XDTROTATE(90.) !Y-axis text vertical
      DO I = 1,100
        NUM = Y0 + (I-1)*SDIVY
        YY(1) = L0 + (NUM-Y0)/DY + 0.5
        IF(YY(1).LT.L1) GOTO 200
        XX(1) = S0
        YY(2) = YY(1)
        XX(2) = S0 - 2
        IF(MOD(NINT(NUM/SDIVY),NINT(BDIVY/SDIVY)).EQ.0) THEN
           XX(2) = S0 - 5
           IF(BDIVY.LT.1.) THEN
              WRITE(NUMB,'(F6.1)') Y0+(I-1)*SDIVY
           ELSE
              WRITE(NUMB,'(I6)') NINT(Y0+(I-1)*SDIVY)
           ENDIF
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,S0-7,YY(1),2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC	   call mvcl(numb,number,1)
CBTC           CALL XDTTEXT(IDEV,G,S0-7,YY(1)+4*NCHAR,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

200   XST = XDTROTATE(0.) !Return text to horizontal
      DO I=1,NPTS
         XX(1) = S0 + (PTS(1,I)-X0)/DX + 0.5
c
c        IF (RPLOT) THEN
c          XX(1) = S0 + (PTS(1,I)-X0)/DX + 0.5
c        ELSE
c          XX(1) = S0 + (RTD*PTS(1,I)-X0)/DX + 0.5
c        ENDIF
c
        YY(1) = L0 + (PTS(2,I)-Y0)/DY + 0.5
        XX(2) = XX(1)
        YY(2) = YY(1)
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
        IF (.NOT. XST) THEN
           CALL PRNT(4,1,XST,'XDIPOLYLINE Error=.')
           RETURN
        ENDIF
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw a profile plot onto graphics screen.
C Outputs: X0,Y0,DX,DY plot transformation on graphics
C
      SUBROUTINE PRDISPLAY(PTS,NPTS,RPLOT,L0,S0,X0,Y0,DX,DY)
      include 'fortport'        ! defines int2byte
      REAL*4 PTS(2,NPTS)
      LOGICAL RPLOT

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD

      INTEGER*4 NPTS
      INTEGER*4 S0,S1,SLEN
      INTEGER*4 XX(2),YY(2)
      INTEGER ILOG
      REAL*4 X0,Y0,DX,DY,MAX,MIN,XDIFF,YDIFF
      REAL*4 SDIVX,BDIVX,SDIVY,BDIVY,NUM
      LOGICAL XST,XDIPOLYLINE,XDTROTATE,XDTTEXT
      CHARACTER*6 NUMB

      bmdn = int2byte(xdw)

      LLEN = 220
      L1 = L0 - LLEN
      SLEN = LLEN
      IF (NPTS.GT.1000) SLEN = 476
      S1 = S0 + SLEN

      XX(1) = S0
      YY(1) = L0
      XX(2) = S1 
      YY(2) = L0
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XX(2) = S0
      YY(2) = L1
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XST = XDTTEXT(IDEV,G,S0-8,L1-3,1,2,'DN')
      IF (RPLOT) THEN
        XST = XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'R')
      ELSE
        XST = XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'L')
      ENDIF
C 
      X0 = PTS(1,1)
      XDIFF = PTS(1,NPTS) - X0
      IF(.NOT.RPLOT) THEN
        X0 = RTD*X0
        XDIFF = RTD*XDIFF
      END IF
      ISIGN = NINT(XDIFF/ABS(XDIFF))
      IF (ABS(XDIFF).LT.2.) XDIFF = 2.*ISIGN
      ILOG = INT(LOG10(ABS(XDIFF)))
      NUM  = ABS(XDIFF)/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVX = 0.1*10.**ILOG
        BDIVX = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVX = 0.2*10.**ILOG
        BDIVX = 1.0*10.**ILOG
      ELSE
        SDIVX = 0.5*10.**ILOG
        BDIVX = 2.0*10.**ILOG
      ENDIF
      IF (XDIFF.LT.0.) THEN
        SDIVX = -SDIVX
        BDIVX = -BDIVX
      ENDIF
      IF (MOD(X0,SDIVX).EQ.0) THEN
         X0 = X0 - SDIVX*ISIGN
         XDIFF = XDIFF + SDIVX*ISIGN
      ENDIF
      X0 = INT(X0/SDIVX)*SDIVX
      DX = (INT((X0+XDIFF+SDIVX)/SDIVX)*SDIVX-X0)/FLOAT(SLEN)

      DO I = 1,100
        NUM = X0 + (I-1)*SDIVX
        XX(1) = S0 + (NUM-X0)/DX + 0.5
        IF(XX(1).GT.S1) GOTO 100
        YY(1) = L0
        XX(2) = XX(1)
        YY(2) = L0 + 2
        IF(MOD(NINT(NUM/SDIVX),NINT(BDIVX/SDIVX)).EQ.0) THEN !VRH 7/28/89
           YY(2) = L0 + 5
           IF (RPLOT) THEN
               WRITE(NUMB,'(I6)') NINT(X0+(I-1)*SDIVX)
           ELSE
               WRITE(NUMB,'(F6.1)') X0+(I-1)*SDIVX
           ENDIF
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,XX(1),L0+15,2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC         call mvcl(numb,number,1)
CBTC         CALL XDTTEXT(IDEV,G,XX(1)-4*NCHAR,L0+15,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

  100 MAX = -32768.
      MIN =  32767.
      DO I = 1,NPTS
        IF(PTS(2,I).GE.MAX) MAX = PTS(2,I)
        IF(PTS(2,I).LE.MIN) MIN = PTS(2,I)
      ENDDO

      Y0 = MIN
      YDIFF = MAX - MIN
      IF (YDIFF.LT.2.) YDIFF = 2.
      ILOG = INT(LOG10(YDIFF))
      NUM = YDIFF/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVY = 0.1*10.**ILOG
        BDIVY = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVY = 0.2*10.**ILOG
        BDIVY = 1.0*10.**ILOG
      ELSE
        SDIVY = 0.5*10.**ILOG
        BDIVY = 2.0*10.**ILOG
      ENDIF
      IF (MOD(Y0,SDIVY).EQ.0.OR.Y0.LT.0.) THEN
         Y0 = Y0 - SDIVY
         YDIFF = YDIFF + SDIVY
      ENDIF
      Y0 = INT(Y0/SDIVY)*SDIVY
      DY = -(INT((Y0+YDIFF+SDIVY)/SDIVY)*SDIVY-Y0)/FLOAT(LLEN)

      XST = XDTROTATE(90.) !Y-axis text vertical
      DO I = 1,100
        NUM = Y0 + (I-1)*SDIVY
        YY(1) = L0 + (NUM-Y0)/DY + 0.5
        IF(YY(1).LT.L1) GOTO 200
        XX(1) = S0
        YY(2) = YY(1)
        XX(2) = S0 - 2
        IF(MOD(NINT(NUM/SDIVY),NINT(BDIVY/SDIVY)).EQ.0) THEN
           XX(2) = S0 - 5
           IF(BDIVY.LT.1.) THEN
              WRITE(NUMB,'(F6.1)') Y0+(I-1)*SDIVY
           ELSE
              WRITE(NUMB,'(I6)') NINT(Y0+(I-1)*SDIVY)
           ENDIF
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,S0-7,YY(1),2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC	   call mvcl(numb,number,1)
CBTC           CALL XDTTEXT(IDEV,G,S0-7,YY(1)+4*NCHAR,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

200   XST = XDTROTATE(0.) !Return text to horizontal
      DO I=1,NPTS
        IF (RPLOT) THEN
          XX(1) = S0 + (PTS(1,I)-X0)/DX + 0.5
        ELSE
          XX(1) = S0 + (RTD*PTS(1,I)-X0)/DX + 0.5
        ENDIF
        YY(1) = L0 + (PTS(2,I)-Y0)/DY + 0.5
        XX(2) = XX(1)
        YY(2) = YY(1)
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
        IF (.NOT. XST) THEN
           CALL PRNT(4,1,XST,'XDIPOLYLINE Error=.')
           RETURN
        ENDIF
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Scale HPIC to PIC, stretching from ILOW TO IHIGH.
C
      SUBROUTINE HSTRETCH(HPIC,SL,SS,NL,NS,
     &		pic,ilow,ihigh,nlev,hbeg,hinc)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      INTEGER*4 HBEG,HINC

      NLEV = IABS(IHIGH-ILOW) + 1	!Number of halfword grey levels
      HINC = (NLEV-1)/256 + 1
      IF (ILOW.LT.IHIGH) THEN
         HBEG = (ILOW/HINC)*HINC
      ELSE
         HBEG = (IHIGH/HINC)*HINC
      ENDIF
      CALL HWTOBYTE(HPIC,HBEG,HINC,NL*NS,pic) !Convert image to byte
      ILOW = (ILOW-HBEG)/HINC
      IHIGH = (IHIGH-HBEG)/HINC
      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Display the input entire input image (at possibly reduced resolution).
C
      SUBROUTINE HOME(PIC,NL,NS,NLDS,NSDS,sl,ss,izoom,zoom)
      BYTE PIC(NS,NL)
      INTEGER*4 SL,SS

      SL = 1
      SS = 1
      IZOOM = MAX0((NL-1)/NLDS,(NS-1)/NSDS) + 1
      IF (IZOOM.GT.1) THEN
         ZOOM = 1.0/IZOOM
         IZOOM = -IZOOM
      ELSE
         ZOOM = 1.0D0
         IZOOM = 1
      END IF
      CALL DPIC(PIC,SL,SS,NL,NS)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw a curve on the graphics screen.
C All arguments are inputs.
C   IFLAG=1 erase graphics plane before drawing curve, =0 otherwise.
C 
      SUBROUTINE DRAWCURVE(PTS,NPTS,IFLAG)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      REAL*4 PTS(2,NPTS)
      INTEGER SAMP,LINE
      LOGICAL XST,XDIPIXELWRITE,XDIFILL

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      include 'fortport'	! for int2bte
C
      IF (IFLAG.EQ.1) XST=XDIFILL(IDEV,G,XDB)	!Erase graphics plane
C
      DO 10 I=1,NPTS
      RLINE = PTS(1,I)
      RSAMP = PTS(2,I)
      IF (RLINE.LT.0.0) THEN			!Removing these comments
          IF (RLINE.EQ.-99.0) GOTO 10	!will cause obscured
          RLINE = ABS(RLINE)			!parts of curve to be
          RSAMP = ABS(RSAMP)			!displayed as well.
      ENDIF
      LINE = ZOOM*(RLINE-SL) + 1.5
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) GOTO 10
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) GOTO 10
      XST = XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(xdw))
      IF (.NOT. XST) RETURN
   10 CONTINUE
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw boxes around the camera blemishes on the graphics
C screen.  All arguments are inputs.
C 
      SUBROUTINE DRAWBLEMS(RES,BLEM,NBLEM,IRMAX)
      include 'fortport'        ! defines int2byte
      REAL*4 RES(2,202),BLEM(4,1000)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER X(5),Y(5),SAMP,LINE
      LOGICAL XST,XDIPOLYLINE,XDIPIXELWRITE

      bmdn = int2byte(xdw)

      DO 10 I=1,NBLEM
      M = BLEM(1,I)
      RLINE = BLEM(2,I) + RES(1,M)
      RSAMP = BLEM(3,I) + RES(2,M)
      LINE = ZOOM*(RLINE-SL) + 1.5	!Convert to screen coordinates
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) GOTO 10
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) GOTO 10
      IRADIUS = BLEM(4,I)
      IF (IRADIUS.GT.IRMAX) IRADIUS=IRMAX
      IF (IRADIUS.LE.0) THEN		!If radius=0, draw a dot.
         XST=XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(xdw))
         GOTO 10
      ENDIF
      X(1) = SAMP - IRADIUS	!Define the box
      Y(1) = LINE - IRADIUS
      X(2) = SAMP + IRADIUS
      Y(2) = LINE - IRADIUS
      X(3) = SAMP + IRADIUS
      Y(3) = LINE + IRADIUS
      X(4) = SAMP - IRADIUS
      Y(4) = LINE + IRADIUS
      X(5) = X(1)
      Y(5) = Y(1)
      XST = XDIPOLYLINE(IDEV,G,bmdn,5,X,Y) !Draw it
      IF (.NOT. XST) GOTO 990
   10 CONTINUE
      RETURN

  990 CALL PRNT(4,1,XST,' ***XDIPOLYLINE Error=.')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw the star background on the graphics screen.
C All arguments are inputs.
C   IFLAG=1 erase graphics plane before drawing curve, =0 otherwise.
C 
      SUBROUTINE DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,IFLAG)
      include 'fortport'        ! defines int2byte
      REAL*4 STARS(3,NSTARS),SPTS(2,NSTARS)
      INTEGER LOMAG,HIMAG

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER X(5),Y(5),SAMP,LINE
      LOGICAL XST,XDIPOLYLINE,XDIPIXELWRITE,XDIFILL
C
      bmdn = int2byte(xdw)

      IF (IFLAG.EQ.1) XST=XDIFILL(IDEV,G,XDB)	!Erase graphics plane
C
      DO 10 I=1,NSTARS
      IF (STARS(3,I).GT.FLOAT(HIMAG).AND.STARS(3,I).NE.99.99) GOTO 10 
      IMAG = STARS(3,I)
      RLINE = SPTS(1,I)
      RSAMP = SPTS(2,I)
      LINE = ZOOM*(RLINE-SL) + 1.5		!Convert to screen coordinates
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) GOTO 10
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) GOTO 10
      IF (STARS(3,I).EQ.99.99) THEN  !VRH 8/12/89 Special added stars
          X(1) = SAMP - 2	!Define the box
          Y(1) = LINE - 2
          X(2) = SAMP + 2
          Y(2) = LINE - 2
          X(3) = SAMP + 2
          Y(3) = LINE + 2
          X(4) = SAMP - 2
          Y(4) = LINE + 2
          X(5) = X(1)
          Y(5) = Y(1)
          XST = XDIPOLYLINE(IDEV,G,BMDN,5,X,Y) !Draw it
          GOTO 10
      END IF
      IRADIUS = 4 + LOMAG - IMAG
CBTC      IF (IRADIUS.LT.1) IRADIUS=1
      IRADIUS = min(10,max(1,iradius))
      X(1) = SAMP + IRADIUS		!Draw horizontal line
      Y(1) = LINE
      X(2) = SAMP - IRADIUS
      Y(2) = LINE
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
      IF (.NOT. XST) GOTO 990
      X(1) = SAMP			!Draw vertical line
      Y(1) = LINE + IRADIUS
      X(2) = SAMP
      Y(2) = LINE - IRADIUS
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
      IF (.NOT. XST) GOTO 990
      IF (IRADIUS.GT.2) THEN
         IRADIUS = IRADIUS - 2
         X(1) = SAMP + IRADIUS
         Y(1) = LINE + IRADIUS
         X(2) = SAMP - IRADIUS
         Y(2) = LINE - IRADIUS
         XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
         IF (.NOT. XST) GOTO 990
         X(1) = SAMP + IRADIUS
         Y(1) = LINE - IRADIUS
         X(2) = SAMP - IRADIUS
         Y(2) = LINE + IRADIUS
         XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
         IF (.NOT. XST) GOTO 990
      ENDIF
      XST = XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(xdb))
   10 CONTINUE
C
      RETURN

  990 CALL PRNT(4,1,XST,' ***XDIPOLYLINE Error=.')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw a dot on the graphics screen.
C   If DN=255, the dot is drawn.  If DN=0, the dot is erased.
C 
      SUBROUTINE DRAWDOT(RLINE,RSAMP,DN)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM
      LOGICAL XST,XDIPIXELWRITE

      common/navdv2/xdw,xdb
      integer xdw,xdb

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER SAMP,LINE,DN,lcldn
      include 'fortport'	! for int2bte
C
      LINE = ZOOM*(RLINE-SL) + 1.5	! 0.5 is added for rounding
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) RETURN
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) RETURN
      if (dn.eq.255) then
        lcldn = xdw
      elseif (dn.eq.0) then
        lcldn = xdb
      else
        lcldn = dn
      endif
      XST = XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(lcldn))
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routines to overlay a lat-lon grid over the planet
C
C OVERLAY -- Draw lat-lon grid
C OVERLAT -- Draw a parallel at latitude RLAT1
C OVERLON -- Draw a meridian at longitude RLON1
C
      SUBROUTINE OVERLAY1(INT)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      REAL*4 LPTS(2,2000)

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CONST/PI,DTR,RTD

      real*4 osnl,osns,rl4,rs4

      EPS0 = (RC/(RA*RB))**2
      RA4 = RA**4
      RB4 = RB**4
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),
     &	osnl,osns,1,CONV,NPH,NPV,ind)

      DO 20 J=-90,90,INT	!Draw parallels at INT intervals
      IF (J.EQ.90.OR.J.EQ.-90) GOTO 20
      
      DO 10 I=0,359		! Draw dots at 1 degree intervals
      RLON = I*DTR
      RLAT = J*DTR
      IF (IGEO.EQ.1) THEN
         EPS = EPS0*DSQRT(RA4 + (RB4-RA4)*DCOS(RLON-RLORA)**2)
         RLAT = DATAN(DTAN(RLAT)*EPS)
      ENDIF
      CALL PLAINV(ind,RLAT,RLON,rline,rsamp,
     &		OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 10
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 10
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 10
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(rline),sngl(rsamp),
     &		0,CONV,NPH,NPV,ind)
	 rline=rl4
	 rsamp=rs4
      ENDIF
      CALL DRAWDOT(sngl(RLINE),sngl(RSAMP),255)
   10 CONTINUE
   20 CONTINUE

      DO 40 J=0,359,INT		!Draw meridians at INT intervals
      RLON = J*DTR
      EPS = (RC/(RA*RB))**2*
     &         DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON-RLORA)**2)

      DO 30 I=-90,90		!Draw dots at 1 degree intervals
      RLAT = I*DTR
      IF (IGEO.EQ.1.AND.IABS(I).LT.90) RLAT=DATAN(DTAN(RLAT)*EPS)
      CALL PLAINV(IND,RLAT,RLON,RLINE,RSAMP,
     &        OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 30
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 30
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 30
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,SNGL(RLINE),SNGL(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	 RLINE=RL4
	 RSAMP=RS4
      ENDIF
      CALL DRAWDOT(SNGL(RLINE),SNGL(RSAMP),255)
   30 CONTINUE
   40 CONTINUE

      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,0.D0,0.D0,OAL,OAS,ZSCALE,
     &    SL,SS,NINT(NLDS/ZOOM),NINT(NSDS/ZOOM),0,0,0,1.D0,3.D0,
     &    0,LPTS,NPTS)
      IF (NPTS.GT.0) CALL DRAWCURVE(LPTS,NPTS,0)
      RETURN

C Routine to draw a parallel at latitude RLAT1
      ENTRY OVERLAT(RLAT1)
      EPS0 = (RC/(RA*RB))**2
      RA4 = RA**4
      RB4 = RB**4
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),
     &	osnl,osns,1,CONV,NPH,NPV,ind)
C
      DO 50 I=0,359		! Draw dots at 1 degree intervals
      RLON = I*DTR
      RLAT = RLAT1
      IF (IGEO.EQ.1) THEN
         EPS = EPS0*DSQRT(RA4 + (RB4-RA4)*DCOS(RLON-RLORA)**2)
         RLAT = DATAN(DTAN(RLAT1)*EPS)
      ENDIF
      CALL PLAINV(IND,RLAT,RLON,RLINE,RSAMP,
     &        OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 50
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 50
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 50
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,SNGL(RLINE),SNGL(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	 RLINE=RL4
	 RSAMP=RS4
      ENDIF
      CALL DRAWDOT(sngl(RLINE),sngl(RSAMP),255)
   50 CONTINUE
      RETURN

C Routine to draw a meridian at longitude RLON1
C
      ENTRY OVERLON(RLON1)
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),
     &	osnl,osns,1,CONV,NPH,NPV,ind)

      EPS = (RC/(RA*RB))**2*
     &              DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON1-RLORA)**2)
C
      DO 60 I=-90,90            !Draw dots at 1 degree intervals
      RLAT = I*DTR
      IF (IGEO.EQ.1.AND.IABS(I).LT.90) RLAT=DATAN(DTAN(RLAT)*EPS)
      CALL PLAINV(IND,RLAT,RLON1,rline,rsamp,
     &               OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 60
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 60
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 60
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,SNGL(RLINE),SNGL(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	 RLINE=RL4
	 RSAMP=RS4
      ENDIF
      CALL DRAWDOT(sngl(RLINE),sngl(RSAMP),255)
   60 CONTINUE
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute a histogram of displayed image area.
C Output: HIS = byte histogram (256 grey-levels)
C Updated: HFLG=1 if histogram exists
C
      SUBROUTINE HISTGEN1(PIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,his,hflg)
      BYTE PIC(NS,NL)
      INTEGER*4 HIS(256),SL,SS,EL,HFLG
      REAL*4 ZOOM

      IF (HFLG.EQ.1) RETURN
      NLO = NLDS/ZOOM + 0.001
      NSO = NSDS/ZOOM + 0.001
      NLO = MIN0(NL-SL+1,NLO)
      NSO = MIN0(NS-SS+1,NSO)
      INC = 1
      IF (IZOOM.LT.0) INC=-IZOOM
      EL = SL + NLO - 1
      CALL ZIA(HIS,256)
      DO L=SL,EL,INC
          CALL HSUB(1,NSO,PIC(SS,L),HIS)
      ENDDO
      HFLG = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute a histogram of displayed image area.
C Output: HIST = halfword histogram (65536 grey-levels)
C         NFREQ=total number of frequencies
C
      SUBROUTINE HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
      INTEGER*2 HPIC(NS,NL)
      INTEGER*4 HIST(-32768:32767),SL,SS,EL
      REAL*4 ZOOM

      NLO = NLDS/ZOOM + 0.001
      NSO = NSDS/ZOOM + 0.001
      NLO = MIN0(NL-SL+1,NLO)
      NSO = MIN0(NS-SS+1,NSO)
      INC = 1
      IF (IZOOM.LT.0) INC=-IZOOM
      EL = SL + NLO - 1
      CALL ZIA(hist,65536)
      DO L=SL,EL,INC
         CALL HSUB2(HPIC(SS,L),NSO,INC,HIST)
      ENDDO
      NFREQ = ((NLO-1)/INC+1)*((NSO-1)/INC+1)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE HSUB2(BUF,NS,INC,hist)
      INTEGER*2 BUF(1)
      INTEGER*4 HIST(-32768:32767)

      DO I=1,NS,INC
         IDN = BUF(I)
         HIST(IDN) = HIST(IDN) + 1
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to display a histogram on the graphics plane G, starting at
C pixel coordinates (L0,S0).
C
      SUBROUTINE HDISPLAY(IDEV,G,HIS,L0,S0,IHGHT,HBEG,HINC,NSPIKES)
c      IMPLICIT LOGICAL(X)	! ??? (lwk)
      include 'fortport'        ! defines int2byte
      INTEGER IDEV,G,HBEG,HINC
      INTEGER XX(10),YY(10)
      CHARACTER*132 MSG
CBTC      INTEGER HIS(1),S0,X,Y,DX,TIC
      INTEGER HIS(256),S0,X,Y,DX,TIC
      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      BYTE BMDN
      LOGICAL XST,XDIPOLYLINE,XDTTEXT

      BMDN = INT2BYTE(XDW)

C          LINC must be a power of two
      N = NSPIKES + 1
      MAXS = HIS(2)          ! INIITIALIZE to first element in set.
C          Scale histogram to max frequency
      DO J=1,N
         MAX = 0
         DO I=2,254
            IFREQ = HIS(I)
            IF (IFREQ.GT.MAX.AND. (IFREQ.LT.MAXS.OR.J.EQ. 1)) MAX=IFREQ
         END DO
         MAXS = MAX
      END DO
C
      HEIGHT = IHGHT
      ZSCALE = 1.
      IF (MAX.GT.1) ZSCALE=HEIGHT/ALOG10(FLOAT(MAX))
      X = S0
      Y = L0
      IDN = HBEG

      DO 50 L=0,255
      TIC = 0
      IF (MOD(L,16).EQ.0) TIC=2
      IF (MOD(L,32).EQ.0) THEN
         WRITE (MSG(1:6),'(I6)') IDN
         XST = XDTTEXT(IDEV,G,X-56,Y+4,1,6,MSG)
         IF (.NOT.XST) CALL XVMESSAGE('HDISPLAY: Error in XDTTEXT',' ')
         TIC = 4
         IDN = IDN + 32*HINC
      ENDIF

      IFREQ = HIS(L+1)
      IF (IFREQ.GT.1) THEN
           DX = ZSCALE*ALOG10(FLOAT(IFREQ))
      ELSE
           DX = 0.0
      ENDIF

      IF(DX.LE.HEIGHT) GOTO 30
      DX = HEIGHT
      XX(1) = X + DX + 2
      YY(1) = Y
      XX(2) = X + DX + 2
      YY(2) = Y
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,XX,YY)
      IF (.NOT.XST) CALL XVMESSAGE('HDSPLY:Error in XDIPOLYLINE',' ')
   30 XX(1) = X - TIC
      YY(1) = Y
      XX(2) = X + DX
      YY(2) = Y
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,XX,YY)
      IF (.NOT.XST) CALL XVMESSAGE('HDSPLY:Error in XDIPOLYLINE',' ')
      Y = Y + 1
   50 CONTINUE
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate a stretch table (STBL) to linearly stretch an image.
C
      SUBROUTINE STRECH(I1,I2,STBL)
      IMPLICIT INTEGER(A-Z)
      integer STBL(1)
C
      IMAX = 255
      D = I2 - I1
      IF (D .EQ. 0) D = 1
      OFFSET = D/2 - IMAX*I1
      K = 0
C
   10 I = (IMAX*K+OFFSET)/D
      IF (I .LT. 0) I=0
      IF (I .GT. IMAX) I=IMAX
      STBL(K+1) = I
      K = K + 1
      IF (K .LE. IMAX) GOTO 10
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine stretch limits from the histogram
C
      SUBROUTINE ASTRC2(HIST,NFREQ,LPER,HPER,min,max)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 LPER,HPER

      MIN = -32767
      MAX = 32766

      IF (NFREQ.EQ.0) RETURN

      ISUM = 0
      KOUNT = NFREQ*.01*LPER + 1
      DO WHILE (ISUM.LT.KOUNT)
         ISUM = ISUM + HIST(MIN)
         MIN = MIN + 1
      ENDDO

      ISUM = 0
      KOUNT = NFREQ*.01*HPER+1
      DO WHILE(ISUM.LT.KOUNT)
         ISUM = ISUM + HIST(MAX)
         MAX = MAX - 1
      ENDDO

      IF (MIN.GT.32767) MIN=32767
      IF (MAX.LT.MIN) MAX=MIN
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to read the cursor position and translate from display
C coordinates to image coordinates.
C
      SUBROUTINE CURSOR(RLINE,RSAMP)
      real*8 rline,rsamp

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      INTEGER LINE,SAMP
      LOGICAL XST,XDCLOCATION

      XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
      RLINE = (LINE-1)/ZOOM + SL
      RSAMP = (SAMP-1)/ZOOM + SS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open and initialize the video plane, graphics plane, and cursor.
C Return IND=1 if successful, =0 otherwise.
C
      SUBROUTINE DEVICE(ind)
      IMPLICIT INTEGER (A-W,Y-Z),LOGICAL (X)
      include 'fortport'	! defines int2byte
      COMMON/DEV/IDEV,V1,G1,TB1,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 V1,G1,TB1,STBL
      REAL*4 S,ZOOM
      INTEGER INFO(80),CON(4),XDSVNL,XDSVNS,xdgcolor,xdssection
      LOGICAL XDIFILL,XDCSET

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB

      character*8 gcolor
      integer ncol

      IND = 0
      V1 = 1
      TB1 = 1
      IF (.NOT.XDEACTION(2,2,3)) RETURN	!Define VRDI error action
      IF (.NOT.XDDUNIT(IDEV)) RETURN	!Get VRDI unit number
      IF (.NOT.XDDOPEN(IDEV)) RETURN	!Open device
      IF (.NOT.XDDACTIVATE(IDEV,.TRUE.)) RETURN	!Activate device
      IF (.NOT.XDDINFO(IDEV,1,80,INFO)) RETURN
c      G1 = 4
c      g1 = info(34)	! see below ...

      MAXTB = MIN0(INFO(48),INFO(60))
      NLDS = XDSVNL(IDEV)
      NSDS = XDSVNS(IDEV)
C     ....Set up initial configuration attempt
      CON(1) = 3			!Monochrome
      CON(2) = INFO(12)			!Image Memory Plane size
      CON(3) = 0			!Video Output size
      CON(4) = 0                        !1x1 Aspect ratio

      IF (.NOT.XDDCONFIGURE(IDEV,CON)) RETURN !Lo Res Err

      IF (.NOT.XDIFILL(IDEV,V1,0)) RETURN	!Erase display plane

      nluts = info(3)
      if (nluts.eq.3) then
	nsection = xdssection( idev, n1)
	if (.not. xdglinit( idev, nsection)) return
	do n1 = 1, nluts
          if (.not. xdlramp( idev, n1, nsection )) return
	  if (.not. xdlconnect( idev, 1, n1, nsection, .false. ))
     1     return
	end do
      else
	call xvmessage(' *** device does not support LUTs',' ')
      endif

      igraph = info(30)
      if (igraph.gt.0) then
	g1 = xdsgraph(idev)
	if (.not.xddinfo(idev,35,1,isec)) return
	if (.not.xdgconnect(idev,g1,isec,.false.)) return
	if (.not.xdgon(idev)) return		!Turn G1 on
	if (.not.xdglinit(idev,isec)) return
      else
	g1 = 0
	call xvmessage('*** no Graphics plane available',' ')
	return
       endif

c      IF (.NOT.XDIFILL(IDEV,G1,VAL)) RETURN	!Erase graphics plane
c      R = 255
c      G = 255
c      B = 255
c      IF (.NOT.XDGLCONSTANT(IDEV,SEC,R,G,B)) RETURN  !Set G1 color

CBTC      xdw = xdgcolor(idev,'white')
      xdw = xdgcolor(idev,'cyan')
c      xdw = 255			! above doesn't seem to work ...
      xdb = xdgcolor(idev,'transparent')
      IF (.NOT.XDIFILL(IDEV,G1,xdb)) RETURN	!Erase graphics plane
c      bmdn = int2byte(xdw)
c      bidn = int2byte(xdb)
c      if (.not.xdimfill( idev, g1, bmdn, bidn)) return

      FONT = 30
      IF (.NOT.XDTFONT(FONT)) RETURN		!Set font type
      H = 8
      S = 1.0
      IF (.NOT.XDTSIZE(H,S)) RETURN		!Set text size
      IF (.NOT.XDTROTATE(0.)) RETURN
CBTC      IF (.NOT.XDTROTATE(0)) RETURN
      BLNK = 0
      FORM = 1
      IF (.NOT.XDCON(IDEV,TB1,FORM,BLNK)) RETURN !Turn cursor on
      XST = XDCSET(IDEV,TB1,(1+nsds)/2,(1+nlds)/2)	! Center cursor
      IF (MAXTB.EQ.0) THEN
         CALL XVMESSAGE('***No trackballs available',' ')
         RETURN
      ELSE IF (MAXTB.EQ.1) THEN
         IF (.NOT.XDCAUTOTRACK(IDEV,TB1,TB1,.TRUE.)) RETURN
      ENDIF
      IND = 1
      RETURN
CBTC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C set graphics color - available from LIMBFIT, RINGFIT, & STARFIT
C via GCOLOR parameter
C
      entry isetcolor
C
      call xviparm( 'GCOLOR', gcolor, ncol, ndef, 0)
      if ( ncol .lt. 1) return
C
C get non-blank portion of gcolor
C
      ncol = len(gcolor)
      dowhile ( ncol .gt. 1 .and. 
     &   index(' '//char(0)//char(9), gcolor(ncol:ncol)) .gt. 0)
        ncol = ncol - 1
      enddo
C
C test for valid color
C
      ndef = xdgcolor(idev,gcolor(1:ncol))
      if ( ndef .gt. 0) xdw = ndef
C
      return
CBTC
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine devoff
      COMMON/DEV/IDEV,V1,G1,TB1,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      call xddclose(idev)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine lutwrite(idev,stbl)
      integer stbl(256)
      integer xdlwrite

      do lut = 1, 3
         ierr = xdlwrite(idev,lut,1,stbl)
         if (ierr .ne. 1) call XVMESSAGE('LUTWRITE:Error in xdlwrite',
     1    ' ')
      end do
      return
      end
