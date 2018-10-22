      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44  ! maplabprog
      IMPLICIT INTEGER(A-Z)

      include 'common.fin'

      common /error/ mp,err 	! for mp 
      integer*4 err
      real*8 mp

c      data FORMAT/'BYTE'/
      DATA INCR/1/,SWTCH/1/

!  start code 
      call xvmessage('*** program MAPLABPROG, version 29-Mar-2000 ***',
     & 0)

      CALL INIT_SPICE

      err = 0                   ! mp errors
      call mp_init( mp, status )
      if ( status .ne. 0 ) then 
          call xvmessage(' MP error on init',0) 
          call xvmessage(' Program terminated',0)
          return
      end if

      CALL MAPA
      if ( err .ne. 0 ) then
          call xvmessage(' MP error on read',0) 
          call xvmessage(' Program terminated',0)
          return
      end if

      err = 0                   ! mp errors
      CALL MAPB
      if ( err .ne. 0 ) then
          call xvmessage(' MP error on label-write',0)
      end if

      RETURN
      END


C**********************************************************


      SUBROUTINE MAPA

      include 'common.fin'

      INTEGER*4 DEF,COUNT
      character*80 fnam
      REAL*4 PBBUF(20)

      REAL*8 FIC,LO,SO,PIXPMM,PHI,BL,LSSP,SSSP,THT,VABS,om(9),rs(3)
      LOGICAL*4 LABFLAG/.FALSE./,XVPTST

      common /error/ mp,err 	! for mp 
      integer*4 err
      real*8 mp
      integer*4 status

      CHARACTER*12 NAME/'        '/

      DATA ISTER/0/,IPOLE/0/,IORTH/0/,P11ST/0/,
     *  P12ST/0/,P21ST/0/,P22ST/0/,LO/500./,SO/500./,IN/0/,
     *   THT/0./,VABS/0./,FIC/1500./,PIXPMM/84.821/,BL/0./,PHI/0./,
     *   ILO/0/,ISO/0/

      nl = 0           !Initialize
      ns = 0
      do i=1,40
	rdata(i) = -9999.0	! to check completeness of project'n spec
      enddo

C  -lwk- moved these before open of input, as user should not override
c  input format
      IF(XVPTST('HALF'))THEN
           FORMAT='HALF'
           PAR(9)=2
      ENDIF
      IF(XVPTST('BYTE'))THEN
         FORMAT='BYTE'
         PAR(9)=1
      ENDIF

      CALL XVPARM('INP',fnam,count,def,0)

      PAR(7) = COUNT

      IF(PAR(7).GT.0)THEN

         input = 1
c        open input and get label data and put in rdata for output file
c        we'll update it using user parameters

         CALL XVUNIT(UNIT2,'INP',1,ind,' ')
         CALL CHKSTAT(IND,' ERR IN XVUNIT,IND=',1,ind,1)
         CALL XVOPEN(UNIT2,IND,'U_FORMAT','HALF', ' ')
         CALL CHKSTAT(IND,' ERR IN XVOPEN,IND=',1,ind,1)
         CALL XVGET(UNIT2,IND,'FORMAT',FORMAT,'PIX_SIZE',PAR(9), ' ')
         CALL CHKSTAT(IND,' ERR IN XVGET,IND=',1,ind,1)


!	for porting to UNIX - BAM 10/95 
!		remove call to searcv2 and insert mp routines
!               as follows
!
!         CALL SEARCV2(UNIT2,NLR2,WORK,IDATA,RDATA)

         call mp_label_read(mp,unit2,status)
         if ( status .eq. 0 ) then

             call mp_mpo2buf(mp,rdata,status)
             if ( status .ne. 0 ) then 
    	         err = -1
 	         return
             end if

	     do l = 1, 40		! move into integer buffer
	         idata(l) = rdata(l)
	     end do

         end if
! 		end searcv2 code

         IF(IDATA(39).EQ.5)P11ST=1
         CALL XVSIZE(PAR(1),PAR(2),PAR(3),PAR(4),PAR(5),PAR(6),' ')

c        labflag indicates whether a map2 label was found

         IF(IDATA(39).NE.7.AND.IDATA(39).NE.8)LABLFAG=.TRUE.
      ENDIF


      CALL XVPARM('NL',IVAL,count,def,0)
      IF (COUNT.GT.0) then
	PAR(3)=IVAL(1)
        if (ival(1).le.0) call mabend(' Invalid NL value.')
	nl = ival(1)
      end if

      CALL XVPARM('NS',IVAL,count,def,0)
      IF (COUNT.GT.0) then
	PAR(4)=IVAL(1)
        if (ival(1).le.0) call mabend(' Invalid NS value.')
	ns = ival(1)
      end if

c  note that the usual SIZE parameter is supported, but SL and SS are ignored
      CALL XVPARM('SIZE',IVAL,count,def,0)
      if (count.eq.4) then	! only count=0 or 4 allowed, see .PDF
	nl = ival(3)
	ns = ival(4)
	PAR(3) = nl
	PAR(4) = ns
        if (nl.le.0 .or. ns.le.0) call mabend(' Invalid SIZE value.')
      endif

      RDATA(33)=PAR(3)/2
      RDATA(34)=PAR(4)/2
      RDATA(28)=PAR(5)/2
      RDATA(29)=PAR(6)/2

C      IF(NUMPAR.EQ.10)GO TO 1000
C      M=-2
C      J=7
C  80  J=J+1
C      M=M+1
C  90  j=j+1
C      m=m+1
C 100  j=j+2
C      m=m+1
C      IF(J.GT.NUMPAR)GO TO 1000
c      CALL KEYGO(4,PAR(J),
C     1'HALF',&110,'REQU',&120,'RPOL',&130,'RADI',&140,'MERC',&150,
C     *'LAMB',&160,'STER',&170,'ORTH',&180,'POLE',&190,'SCAL',&200,
C     *'LINE',&210,'SAMP',&220,'LATI',&230,'LONG',&240,'PAR1',&250,
C     *'PAR2',&260,'LIN1',&270,'LIN2',&280,'NORT',&290,'INCR',&300,
C     6'INC ',&300,'VENU',&310,'CYLI',&330,'JUPI',&340,'IO  ',&350,
C     *'EURO',&360,'GANY',&370,'CALL',&380,'MCRY',&390,'MARS',&400,
C     *'MOON',&410,            'EART',&430,'RECT',&440,'LATL',&440,
C     *'OS  ',&450,'SSCP',&460,'NORA',&470,'RMAG',&480,'FOCL',&490,
C     *'FARE',&500,'PSCA',&510,'SLAT',&520,'SLON',&530,'OBJE',&540,
C     3'LAXI',&550,'SAXI',&560,'BYTE',&570,'TARG',&580)
C      CALL PRNT(4,1,M,'0***ABEND,ERROR IN  USERS PARAMETER.')
C      CALL PARERR(PAR(J))
C      CALL ABEND

      CALL XVPARM('REQUATOR',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(26)=RVAL(1)

      CALL XVPARM('RPOLE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(25)=RVAL(1)

      CALL XVPARM('RADIUS',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(26)=RVAL(1)
         RDATA(25)=RVAL(1)
      ENDIF

      IF(XVPTST('STEREOGR'))ISTER=1
      IF(XVPTST('ORTHOGR'))IORTH=1
      IF(XVPTST('POLE'))   IPOLE=1
      IF(XVPTST('LAMBERT'))IDATA(39)=5
      IF(XVPTST('MERCATOR'))IDATA(39)=6
      IF(XVPTST('CYLINDR'))IDATA(39)=9   ! cylindrical projection
      IF(XVPTST('RECTANG'))IDATA(39)=10  ! latlon simple cylindrical projection
      IF(XVPTST('LATLON'))IDATA(39)=10
      IF(XVPTST('OBCYLIND'))IDATA(39)=11
      IF(XVPTST('SINUSOID'))IDATA(39)=12
      IF(XVPTST('OBSINUS'))IDATA(39)=13
      IF(XVPTST('MOLLWEID'))IDATA(39)=14
      IF(XVPTST('TMERCATO'))IDATA(39)=15
      IF(XVPTST('OS'))IDATA(39)=16        ! object space
      IF(XVPTST('OBJECT'))IDATA(39)=16    ! object space
      IF(XVPTST('PERSPECT'))IDATA(39)=16 
      IF(ISTER.EQ.1.AND.IPOLE.EQ.0)IDATA(39)=4
      IF(ISTER.EQ.1.AND.IPOLE.EQ.1)IDATA(39)=3
      IF(IORTH.EQ.1.AND.IPOLE.EQ.0)IDATA(39)=2
      IF(IORTH.EQ.1.AND.IPOLE.EQ.1)IDATA(39)=1

      CALL XVPARM('SCALE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(7)=RVAL(1)

      CALL XVPARM('LINE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(2)=RVAL(1)

      CALL XVPARM('SAMPLE',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(1)=RVAL(1)
         ISAMP=1
      ENDIF

      CALL XVPARM('LATITUDE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(3)=RVAL(1)

      CALL XVPARM('LONGITUD',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(6)=RVAL(1)
         IF(RDATA(6).LT.0.)RDATA(6)=RDATA(6)+360.
         ILONG=1
      ENDIF

      CALL XVPARM('PAR1',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(4)=RVAL(1)
         P11ST=1
      ENDIF
      ! special fix for bug in mpbuf2mpo:
      if (idata(39).eq.10) then
	rdata(4) = 0.
      endif

      CALL XVPARM('PAR2',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(5)=RVAL(1)
         P21ST=1
      ENDIF

c  not used:  (lwk)
c      CALL XVPARM('LIN1',RVAL,count,def,0)
c      IF(COUNT.GT.0)RLIN11=RVAL(1)
c      CALL XVPARM('LIN2',RVAL,count,def,0)
c      IF(COUNT.GT.0)RLIN22=RVAL(1)

      CALL XVPARM('NORTH',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(9)=RVAL(1)

      CALL XVPARM('INCR',ival, count,def,0)
      IF(COUNT.GT.0)INCR=IVAL(1)
c     if ( ival(1).le. 0 ) then   ! fr 88287 - fixed bam 1/96
      if (incr.le.0) call mabend( ' Invalid INCR value.')

      CALL XVPARM('TARGET',CVAL,count,def,0)
      IF(COUNT.GT.0)NAME=CVAL

      CALL XVPARM('SSCPT',RVAL,count,def,0)        ! subspacecraft point
      IF(COUNT.GT.0)THEN
         RDATA(33)=RVAL(1)
         RDATA(34)=RVAL(2)
      ENDIF

      CALL XVPARM('NORANGLE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(35)=RVAL(1)

      CALL XVPARM('RMAG',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(38)=RVAL(1)

      CALL XVPARM('FOCL',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(27)=RVAL(1)

      CALL XVPARM('PSCALE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(30)=RVAL(1)      ! camera scale pixels/mm

      CALL XVPARM('SLATITUD',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(31)=RVAL(1)      !sub spacecraft latitude

      CALL XVPARM('SLONGITU',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(32)=RVAL(1)      ! subspacecraft longitude

      CALL XVPARM('LAXIS',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(28)=RVAL(1)               ! line of optical axis
         ILO=1
      ELSEIF (IDATA(39).EQ.16) THEN
	RDATA(28) = NL/2
      ENDIF

      CALL XVPARM('SAXIS',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(29)=RVAL(1)                ! sample of optical axis
         ISO=1
      ELSEIF (IDATA(39).EQ.16) THEN
	RDATA(29) = NS/2
      ENDIF

      IF(XVPTST('MCRY'))    NAME='MERCURY'
      IF(XVPTST('MERCURY')) NAME='MERCURY'
      IF(XVPTST('VENUS'))   NAME='VENUS'
      IF(XVPTST('EARTH'))   NAME='EARTH'
      IF(XVPTST('MOON'))    NAME='MOON'
      IF(XVPTST('MARS'))    NAME='MARS'
      IF(XVPTST('PHOBOS'))  NAME='PHOBOS'
      IF(XVPTST('DEIMOS'))  NAME='DEIMOS'

      IF(XVPTST('JUPITER')) NAME='JUPITER'
      IF(XVPTST('IO'))      NAME='IO'
      IF(XVPTST('EUROPA'))  NAME='EUROPA'
      IF(XVPTST('GANYMEDE'))NAME='GANYMEDE'
      IF(XVPTST('CALLISTO'))NAME='CALLISTO'
      IF(XVPTST('AMALTHEA'))NAME='AMALTHEA'

      IF(XVPTST('SATURN'))  NAME='SATURN'
      IF(XVPTST('MIMAS'))   NAME='MIMAS'
      IF(XVPTST('ENCELADU'))NAME='ENCELADU'
      IF(XVPTST('THETHYS')) NAME='THETHYS'
      IF(XVPTST('DIONE'))   NAME='DIONE'
      IF(XVPTST('RHEA'))    NAME='RHEA'
      IF(XVPTST('DIONE'))   NAME='DIONE'
      IF(XVPTST('TITAN'))   NAME='TITAN'
      IF(XVPTST('HYPERION'))NAME='HYPERION'
      IF(XVPTST('IAPETUS')) NAME='IAPETUS'
      IF(XVPTST('PHOEBE'))  NAME='PHOEBE'

      IF(XVPTST('URANUS'))  NAME='URANUS'
      IF(XVPTST('MIRANDA')) NAME='MIRANDA'
      IF(XVPTST('ARIEL'))   NAME='ARIEL'
      IF(XVPTST('UMBRIEL')) NAME='UMBRIEL'
      IF(XVPTST('TITANIA')) NAME='TITANIA'
      IF(XVPTST('OBERON'))  NAME='OBERON'

      IF(XVPTST('NEPTUNE')) NAME='NEPTUNE'
      IF(XVPTST('TRITON'))  NAME='TRITON'
      IF(XVPTST('NEREID'))  NAME='NEREID'

      IF(XVPTST('PLUTO'))   NAME='PLUTO'
      IF(XVPTST('CHARON'))  NAME='CHARON'

      IF(NAME.NE.'            ')THEN
         CALL xvmessage('TARGET='//NAME,0)
         CALL PBDATA(NAME,PBBUF,*1200)
         RDATA(26)=PBBUF(1)
         RDATA(25)=PBBUF(3)
      ENDIF
      
      IF(IDATA(39).EQ.9.AND.ILONG.EQ.1.AND.ISAMP.EQ.1)THEN
          CALL xvmessage(' RECALCULATING LONGITUDE, SAMPLE',0)
          CALL CYLPATCH(RDATA)
          CALL PRNT(7,1,RDATA(6),' at sample=1,long=.')
          CALL PRNT(7,1,RDATA(1),' long 0 at sample=.')
      ENDIF

      RDATA(8)=1.
      IF((IDATA(39).EQ.1.OR.IDATA(39).EQ.3).AND.RDATA(3).LT.0.0)
     1      RDATA(8)=-1.0
      CALL PRNT(7,2,RDATA(25),'RADII=.')
      CALL PRNT(4,1,IDATA(39),'OUTPUT PIX PROJECTION TYPE=.')

      IF(IDATA(39).LE.0)CALL MABEND(' ERROR IN PROJECTION TYPE')
      IF (IDATA(39).EQ.7) CALL mabend('IMAGE SPACE IS NOT SUPPORTED')
      IF(IDATA(39).EQ.5)THEN
         IF(P11ST.NE.1.OR.P21ST.NE.1) CALL mabend(
     1			 'STANDARD OUTPUT PARALLELS NOT SET')
      ENDIF
      IF(RDATA(25).EQ.0..OR.RDATA(26).EQ.0.) call mabend(
     & '***RADII NOT SPECIFIED',0)

      IF (IDATA(39).EQ.8 .OR. IDATA(39).EQ.16) THEN
	! Perspective, calculate ommatrix and rsvector
	CALL PRNT(7,12,RDATA(27),' DATA(27-38)=.')
	FIC=RDATA(27)		! focal length
	if (fic.le.0.0) call mabend(
     1		 ' FOCL required for Perspective projection')
        LO=RDATA(28)
        SO=RDATA(29)
	if (lo.le.-9999. .or. so.le.-9999.) call mabend(
     1		 ' LAXIS,SAXIS required for Perspective projection')
        PIXPMM=RDATA(30)	! camera scale
	if (pixpmm.le.0.0) call mabend(
     1		 ' PSCAL required for Perspective projection')
        PHI=RDATA(31)		! s/c latitude
        BL=RDATA(32)		! s/c longitude
	if (phi.le.-9999. .or. bl.le.-9999.) call mabend(
     1		 ' LATI,LONG required for Perspective projection')
        LSSP=RDATA(33)
        SSSP=RDATA(34)
	if (lssp.le.-9999. .or. sssp.le.-9999.) call mabend(
     1		 ' SSCPT required for Perspective projection')
        THT=RDATA(35)		! North angle
        VABS=RDATA(38)		! range
	if (vabs.le.0.0) call mabend(
     1		 ' RMAG required for Perspective projection')
        CALL MOMATI(LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,VABS,OM,RS)
        CALL PRNT(8,9,OM ,' OMMATRIX =.')
        CALL PRNT(8,3,RS ,' RSVECTOR =.')
      ELSEIF (IDATA(39).NE.8) THEN
	CALL PRNT(7,10,RDATA,' RDATA=.')
      ENDIF

      RETURN

1200  CALL xvmessage(' PBDATA ERROR',0)
      CALL ABEND
      END


C**********************************************************


      SUBROUTINE MAPB
      IMPLICIT INTEGER(A-Z)

      include 'common.fin'

      REAL CIRCUM
      INTEGER NPAR(5000)
      INTEGER*2 BUF(30000)
      CHARACTER*32 MSG

      EQUIVALENCE (NPAR,BUF)

      common /error/ mp,err 	! for mp 
      integer*4 err
      real*8 mp
      integer*4 status


      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI,' ')
      IF (NL.LE.0 .OR. NS.LE.0) CALL MABEND(
     & 'MUST SPECIFY OUTPUT SIZE IF NO INPUT IMAGE')

      if (incr.gt.nl/2 .or. incr.gt.ns/2) 
     &  call mabend( ' Invalid INCR value.')

      IF (IDATA(39).NE.7 .AND. IDATA(39).NE.8 .AND.
     & IDATA(39).NE.16) THEN
         IF(RDATA(7).EQ.0.)CALL MABEND(' SCALE NOT SPECIFIED')
         CIRCUM=2.*3.14159*RDATA(25)/RDATA(7)
         MSG=' CIRCUMFERENCE=           PIXELS'
         WRITE (MSG(16:25),'(F10.1)') CIRCUM
         CALL xvmessage(MSG,0)
      ENDIF

      CALL XVUNIT(UNIT1,'OUT',1,ind,' ')
      CALL CHKSTAT(IND,' ERR IN XVUNIT,IND=',1,ind,1)
      CALL XVOPEN(UNIT1,ind,'OP','WRITE','U_NL',NL/INCR,'U_NS',NS/INCR,
     *   'U_FORMAT','HALF','O_FORMAT',FORMAT, ' ')
      CALL CHKSTAT(IND,' ERR IN XVOPEN,IND=',1,ind,1)

c     update map2 labels
      IF(IDATA(39).EQ.10) CALL RECTPATCH(IDATA)

! 	Ported to UNIX - eliminated maplabv2
!		inserted mp routines    bam 10/95

!      CALL MAPLABV2(IDATA,IDATA,UNIT1,NLR1,NPAR)


      call mp_buf2mpo( rdata, mp, status )
      if ( status .ne. 0 ) then 
	  err = -1
          return
      end if

      call mp_label_write( mp, unit1, 'HISTORY', status )
      if ( status .ne. 0 ) then 
	  err = -1
          return
      end if

      call mp_label_write( mp, unit1, 'PROPERTY', status )
      if ( status .ne. 0 ) then 
	  err = -1
          return
      end if

c     copy file
      OREC=0
      EL=SL+NL-1
      IF(EL.GT.PAR(3)) EL=PAR(3)+SL-1
      CALL ZIA(BUF,30000/4)
      I5=(NL+4)/5
      IF(I5.EQ.0)I5=1
      IF(FORMAT.EQ.'BYTE')SWTCH=1
      IF(FORMAT.EQ.'HALF')SWTCH=2
      IF(FORMAT.EQ.'WORD')SWTCH=2

      if (ns*swtch.gt.60000) call mabend(' NS too large for buffer!')

      DO I=SL,EL,INCR
        IF(PAR(7).GT.0)THEN
C           CALL READ(IND,2,NLR2+I,SWTCH,SB-1,NBI,BUF,0)
C           CALL JJLCHK(IND,2,I,0)
           CALL XVREAD(UNIT2,BUF,IND,'LINE',I,'SAMP',SS,'NSAMPS',NS,' ')
           CALL CHKSTAT(IND,' ERR IN XVREAD,IND=',1,ind,1)
           IF(INCR.NE.1) CALL MVE(SWTCH,NBI,BUF,BUF,INCR,1)
        ENDIF

        OREC=OREC+1
C        CALL WRITE(IND,1,NLR1+OREC,SWTCH,0,NBO,BUF,0)
C        CALL JJLCHK(IND,1,I,1)
        CALL XVWRIT(UNIT1,BUF,IND,'LINE',OREC,'NSAMPS',NS/INCR,' ')
        CALL CHKSTAT(IND,' ERR IN XVWRIT,IND=',1,ind,1)
        IF(MOD(OREC,I5).EQ.0)CALL PRNT(4,1,OREC,' RECORD WRITTEN=.')
      ENDDO

C     CLOSE FILES
      IF(PAR(7).GT.0)CALL XVCLOSE(UNIT2,IND, ' ')

      CALL XVCLOSE(UNIT1,ind,' ')

      RETURN
      END
