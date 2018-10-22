	INCLUDE 'VICMAIN_FOR'
C
C VICAR Program NEWMOS: Mosaicking program
C 10-July-95...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C 30-July-96 ..lwk..  fixed projection type check; replaced MP_XY2LL with
c			CONVEV for Perspective case (temporary!);  fixed
c			some bugs in Perspective case
C 15-Oct-96  ..lwk..  fixed format problem when NUMIPT>6, write History
c			map labels as well as Property

      SUBROUTINE MAIN44


      IMPLICIT NONE

      EXTERNAL MOSB
c      include 'fortport'
      COMMON/C1/PAR,NUMIPT,NUMLIN,LSET,RSET,LTHRES,RTHRES,LSEQ,
     + RSEQ,LNIB,RNIB,LNIBST,RNIBST,NSEQ,ITHRES,ITHRST,BLKSIZ,NSI,
     + LINE,SAMP,SCP,NSO,NIBB,NIBBST,NSEQST,RSEQST,LSEQST,LCP,
     + LTHRST,RTHRST,NOSAMP,NOLINE,REFIN,DCLE,INCR,ISWTCH,NLO,
     + IAVER,ISMOOTH,NLR,REQ,RPOLE,LAB,weight,NOBAND,JSB,NBO,NBI
      COMMON /FILES/ INFIL, OUTFIL
      real*4 weight(80),REQ,RPOLE 
      INTEGER REFIN(80),LENGTH,NUMIPT,NUMLIN,LSET,LTHRES
      INTEGER LSEQ,NIBB,LNIB,LNIBST,NSEQ,ITHRES,ITHRST
      INTEGER NSI,NSO,NIBBST,NSEQST,LSEQST,LCP,LTHRST,INCR,ISWTCH
      INTEGER NLO,IAVER,ISMOOTH,ISB,INDEX,I,ISTAT,NPXL,JSL,JSS,JNSI
      INTEGER NSB,NS4,NLR,LENCHK
	 INTEGER JSB,NBO,NBI
      INTEGER PAR(100),RTHRST,RSEQST,RSEQ,RNIB,RNIBST,RTHRES,RSET,BLKSIZ
      INTEGER LINE(80),SAMP(80),SCP,DCLE,NOLINE(80),
     1	    NOSAMP(80),NOBAND(80),LAB(80)
      INTEGER INFIL(80),OUTFIL
	 CHARACTER*3 ORGIN

c  tell user what version we have:
      call ifmessage('*** NEWMOS version 18-Aug-03 ***')

C     Map Routine Functions
C     INITIALIZE ALL VARIABLES TO PREVENT ANY UNEXPECTED
C     BEHAVIOUR.

      CALL ZIA(PAR,100)
      CALL ZIA(REFIN,80)
      CALL ZIA(LINE,80)
      CALL ZIA(SAMP,80)
      CALL ZIA(NOLINE,80)
      CALL ZIA(NOSAMP,80)
	 CALL ZIA(NOBAND,80)
      CALL ZIA(LAB,80)
      CALL ZIA(INFIL,80)
      LENGTH=0
      NUMIPT=0
      NUMLIN=0
      LSET=0
      LTHRES=0
      LSEQ=0
      NIBB=0
      LNIB=0
      LNIBST=0
      NSEQ=0
      ITHRES=0
      ITHRST=0
      NSI=0
      NSO=0
      NIBBST=0
      NSEQST=0
      LSEQST=0
      LCP=0
      LTHRST=0
      INCR=0
      ISWTCH=0
      NLO=0
      IAVER=0
      ISMOOTH=0
      ISB=0
      INDEX=0
      I=0
      ISTAT=0
      NPXL=0
      JSL=0
      JSS=0
      JNSI=0
      NSB=0
      NS4=0
      NLR=0
      LENCHK=0
      RTHRST=0
      RSEQST=0
      RSEQ=0
      RNIB=0
      RNIBST=0
      RTHRES=0
      RSET=0
      BLKSIZ=0
      SCP=0
      DCLE=0
      OUTFIL=0
      CALL ZIA(weight,80)
      REQ=0.0
      RPOLE=0.0

      ISB = 4
      LENGTH = 1
      INDEX = 1
      nsi = 0
c
      call xvpcnt( 'INP', numipt)
      do i=1,numipt
        weight(i)=1.0
      enddo
C
C  PRIMARY INPUT:
      CALL XVUNIT(INFIL(1),'INP',1,ISTAT,' ')
      CALL XVOPEN(INFIL(1),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +          'U_FORMAT','HALF',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INFIL(1),ISTAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

      CALL XVGET(INFIL(1),ISTAT,'NL',NOLINE(1),'NS',NOSAMP(1),
     +          'NB',NOBAND(1),'PIX_SIZE',NPXL,' ')
      NSI = NSI+NOSAMP(1)	! NSI is the number of samples total in input
      REFIN(1) = INDEX
      INDEX = NOSAMP(1) + INDEX
      ISWTCH = 1
      IF (NPXL.EQ.2) ISWTCH = 0
      IF (NPXL.GT.2) CALL MABEND( '** ILLEGAL FORMAT **')
      IF (NPXL .EQ. 1)
     *          CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS BYTE',' ')
      IF (NPXL .EQ. 2)
     *      CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS HALFWORD',' ')

      CALL XVCLOSE(INFIL(1),ISTAT,' ')

C  OTHER INPUTS:
      DO I = 2,NUMIPT
         CALL XVUNIT(INFIL(I),'INP',I,ISTAT,' ')
         CALL XVOPEN(INFIL(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +          'U_FORMAT','HALF',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INFIL(I),ISTAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

         CALL XVGET(INFIL(I),ISTAT,'NL',NOLINE(I),'NS',NOSAMP(I),
     +          'NB',NOBAND(I),'PIX_SIZE',NPXL,' ')
         NSI = NSI+NOSAMP(I)      ! NSI is the number of samples total in input
         REFIN(I) = INDEX
         INDEX = NOSAMP(I) + INDEX

	 CALL XVCLOSE(INFIL(I),ISTAT,' ')

      ENDDO
C
      CALL XVSIZE( JSL, JSS, NLO, NSO, NUMLIN, JNSI)
      CALL XVBANDS(JSB, NBO, NBI)
	
      IF ( JSB .GT. NBI ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( JSB + NBO - 1 .GT. NBI) THEN
	 CALL XVMESSAGE('***Number of bands truncated', ' ') 
	 NBO = NBI + 1 - JSB
      ENDIF
	
C
      CALL XVUNIT(OUTFIL,'OUT',1,ISTAT,' ')		! output file
C
C  OUTPUT FILE IS OPENED IN SUBROUTINE MOSA, ONCE ITS SIZE IS FOUND.
C
      CALL MOSA
C
C  REFIN is starting byte of each pix in input buffer INBUF
C  NSB is length of  the output buffer in bytes
C
      NSB = NSO * 2
      LENGTH = NSI * 2
      NS4 = NSO*4
      LENCHK = LENGTH + NSB + NS4 + NS4
C
      CALL STACKA(8,MOSB,5,LENGTH,NSB,NS4,NS4,NS4,LENCHK)
C
C      DO I = 1, NUMIPT			! Close files
C	 CALL XVCLOSE(INFIL(I),ISTAT,' ')
C      END DO
      CALL XVCLOSE(OUTFIL,ISTAT,' ')

   11 RETURN
      END

C******************************************************************************
      SUBROUTINE MOSA

      COMMON/C1/PAR,NUMIPT,NUMLIN,LSET,RSET,LTHRES,RTHRES,LSEQ,
     + RSEQ,LNIB,RNIB,LNIBST,RNIBST,NSEQ,ITHRES,ITHRST,BLKSIZ,NSI,
     + LINE,SAMP,SCP,NSO,NIBB,NIBBST,NSEQST,RSEQST,LSEQST,LCP,
     + LTHRST,RTHRST,NOSAMP,NOLINE,REFIN,DCLE,INCR,ISWTCH,NLO,
     + IAVER,ISMOOTH,NLR,REQ,RPOLE,LAB,weight,NOBAND,JSB,NBO,NBI
      COMMON /FILES/ INFIL, OUTFIL
      real*4 weight(80)
      INTEGER*4 REFIN(80)
      INTEGER PAR(100),RTHRST,RSEQST,RSEQ,RNIB,RNIBST,RTHRES,RSET,BLKSIZ
	 INTEGER JSB,NBO,NBI
      INTEGER LINE(80),SAMP(80),SCP,DCLE,NOLINE(80),NOSAMP(80),LAB(80),
     1	 NOBAND(80)
      INTEGER INFIL(80), OUTFIL
      CHARACTER*4 OFORM
      character*80 msgout
      logical xvptst
      LOGICAL NOMIN
c
C         PARAMETER PROCESSER
C
c threshold
	ithrst = 0
	lthrst = 0
	rthrst = 0
	call xvparm( 'THRESH', ithres, i ,j,1)
	if (ithres.ne.1) ithrst = 1
	call xvparm( 'LTHRESH', lthres, i, j,1)
	if (lthres.ne.1) lthrst = 1
	call xvparm( 'RTHRESH', rthres, i, j,1)
	if (rthres.ne.1) rthrst = 1
c
	call xvparm( 'ETHRESH', ipar, i, j,1)
	if (i.gt.0) then
	  lthres = ipar
	  rthres = ipar
	endif
c
c  no. of threshold points for trigger
	nseqst = 0
	call xvparm( 'NSEQ', nseq, i, j,1)
	if (nseq.ne.1) nseqst = 1
c
	lseqst = 0
	call xvparm( 'LSEQ', lseq, i, j,1)
	if (lseq.ne.1) lseqst = 1
c
	rseqst = 0
	call xvparm( 'RSEQ', rseq, i, j,1)
	if (rseq.ne.1) rseqst = 1
c
c  nibbles in from edge of picture data
	nibbst= 0
	call xvparm( 'NIBB', nibb, i, j,1)
	if (nibb.ne.0) nibbst = 1
c
	lnibst = 0
	call xvparm( 'LNIB', lnib, i, j,1)
	if (lnib.ne.0) lnibst = 1
c
	rnibst = 0
	call xvparm( 'RNIB', rnib, i, j,1)
	if (rnib.ne.0) rnibst = 1
c
c  increment in edge search
	call xvparm( 'INCR', incr, i, j,1)
c
c  (line,samp) for common points for all inputs
	call xvparm( 'PIXL',  par, icnt, j,100)
	nls = icnt/2
	if (nls*2.ne.icnt) call mabend(' Error in PIXL count')
	do ii = 1,nls
	  line(ii) = par(2*ii-1)
	  samp(ii) = par(2*ii)
	enddo
	numpix = nls
c
c  (line,samp) for common point in output
	call xvparm( 'LCP', lcp, i, j,1)
	call xvparm( 'SCP', scp, i, j,1)
c
c  set dclevel of background
	call xvparm( 'DCLEV', dcle, i, j,1)
c
c  output format
	if (xvptst( 'BYTE')) iswtch = 1
	if (xvptst( 'HALF')) iswtch = 0
c
c  radii specified by target body or directly:
	if (xvptst( 'JUPI')) then
	  req = 71400.
	  rpole = 66773.
	elseif (xvptst( 'IO') .or. xvptst( 'J1')) then
	  req = 1816.
	  rpole = req
	elseif (xvptst( 'EURO') .or. xvptst( 'J2')) then
	  req = 1569.
	  rpole = req
	elseif (xvptst( 'GANY') .or. xvptst( 'J3')) then
	  req = 2631.
	  rpole = req
	elseif (xvptst( 'CALL') .or. xvptst( 'J4')) then
	  req = 2400.
	  rpole = req
	elseif (xvptst( 'MOON')) then
	  req = 1738.09
	  rpole = req
	elseif (xvptst( 'MCRY')) then
	  req = 2439.
	  rpole = req
	else
	  call xvparm( 'RADIUS', req, i, j,1)
	  if (req.eq.0.) then
	    call xvparm( 'REQ', req, i, j,1)
	    call xvparm( 'RPOL', rpole, i, j,1)
	  else
	    rpole = req
	  endif
	endif
c
c  other params
        call xvparm('WEIGHT',weight,i,j,80)

	iaver= 0
	if (xvptst( 'AVER')) iaver = 1

        ismooth=0
	if (xvptst( 'SMOOTH')) ismooth=1
c
	iadapt = 0
	imap2 = 0
	if (xvptst( 'ADAPT')) then
	  iadapt = 1
	  imap2 = 1
	endif
	if (xvptst( 'MAP2')) imap2 = 1
        NOMIN = XVPTST('NOMIN')
c
      IF (IMAP2 .EQ. 1) THEN	!FIND OFFSETS IN PROJECTIONS & OPEN OUTPUT
	CALL OFFSET( NUMIPT, REQ, RPOLE, LINE(1), SAMP(1), LCP, SCP,
     &   NLO, NSO, NBO, NOLINE(1), NOSAMP(1), IADAPT, NLR, LAB, 
     &   ISWTCH,NOMIN)
      ELSE			!JUST OPEN OUTPUT
	OFORM = 'BYTE'
	IF (ISWTCH.EQ.0) OFORM = 'HALF'
	CALL XVOPEN( OUTFIL, ISTAT, 'U_NL', NLO, 'U_NS', NSO, 'OPEN_ACT',
     &   'SA', 'IO_ACT', 'SA', 'OP', 'WRITE', 'U_FORMAT', 'HALF',
     &	 'O_FORMAT', OFORM,'U_NB',NBO,' ')
      ENDIF
C
      call xvmessage(' ',' ')
      nblocks = (numipt+5)/6
      do iblock=1,nblocks
	i1 = (iblock-1)*6+1
	i2 = iblock*6
	if (i2.gt.numipt) i2 = numipt
        msgout ='LINE= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') line(i)
        enddo
        call xvmessage(msgout,' ')
        msgout ='SAMP= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') samp(i)
        enddo
        call xvmessage(msgout,' ')
        msgout ='NOLINE= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') noline(i)
        enddo
        call xvmessage(msgout,' ')
        msgout ='NOSAMP= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') nosamp(i)
        enddo
        call xvmessage(msgout,' ')
        call xvmessage(' ',' ')
      enddo
      IF (IADAPT .EQ. 1) RETURN
      IF (NUMPIX .EQ. NUMIPT) RETURN
      msgout='***NUMBER OF OFFSETS= '
      write(msgout(23:26), '(I4)') numpix
      call xvmessage(msgout,' ')	
      msgout='***NUMBER OF INPUTS= '
      write(msgout(23:26), '(I4)') numipt
      call xvmessage(msgout,' ')	

      RETURN
      END

C***************************************************************************
      SUBROUTINE OFFSET(NUMIPT,REQ,RPOLE,LINE,SAMP,LCP,SCP,NLO,NSO,NBO,
     1NOLINE,NOSAMP,IADAPT,NLR,LAB,ISWTCH, NOMIN)

      INCLUDE 'mp_for_defs'

      COMMON /FILES/ INFIL,OUTFIL
      INTEGER CIRCUM,SSTART(80),SSTOP(80),TPROJ(80),NOLINE(80)
      INTEGER NOSAMP(80),LINE(80),SAMP(80)
      LOGICAL NOMIN
      REAL RDATA(40),TSAMP(80),TLINE(80),TLAT(80),TLAT1(80),TLAT2(80)
      REAL TLONG(80),TSCALE(80),TNOANG(80),RDATAO(40)
      REAL REQCK(80),RPOLCK(80)
      integer idata(40)
      REAL*8 XC,ZC,TH,TH1,TH2,LAM,F,PSI,RP   ! ARGUMENTS FOR TRANV
      REAL*8 FLINE,FSAMP,FLAT,FLON          !arguments for mp_ll2xy
      real*8 MAP_RSLTN
C        INTEGER MODE
	 INTEGER*4 NBO
      INTEGER SCP,LPAR1(80),LPAR2(80),LAB(80)
      integer number_keywords
      integer types(mp_number_of_keywords)
      integer classes(mp_number_of_keywords)
      INTEGER INFIL(80), OUTFIL
      integer status,i
      real*8 mp,RLCP,RSCP

      CHARACTER*4 OFORM
      character*(mp_max_keywd_length) keys(mp_number_of_keywords)
      character*(mp_max_keywd_length) mpt(mp_number_of_keywords)
      character*(mp_max_keywd_length) keytype, mkey
      logical blank
      CHARACTER*132 MSG3,MSG5
      character*100 msgout
      EQUIVALENCE (RDATA(7),SCALE)
      EQUIVALENCE (RDATA,IDATA)
C
      CALL ZIA(SSTART,80)
      CALL ZIA(SSTOP,80)
      CALL ZIA(TPROJ,80)
      CIRCUM=0
      CALL ZIA(IDATA,40)
      CALL ZIA(TSAMP,80)
      CALL ZIA(TLINE,80)
      CALL ZIA(TLAT,80)
      CALL ZIA(TLAT1,80)
      CALL ZIA(TLAT2,80)
      CALL ZIA(TLONG,80)
      
      CALL ZIA(TSCALE,80)
      CALL ZIA(TNOANG,80)
      CALL ZIA(RDATAO,40)
      CALL ZIA(REQCK,80)
      CALL ZIA(RPOLCK,80)
      XC=0.0
      ZC=0.0
      TH=0.0
      TH1=0.0
      TH2=0.0
      LAM=0.0
      F=0.0
      PSI=0.0
      RP =0.0  ! ARGUMENTS FOR TRANV
      FLINE=0.0
      FSAMP=0.0
      FLAT=0.0
      FLON =0.0         !arguments for mp_ll2xy
      MAP_RSLTN=0.0
      do i=1,mp_number_of_keywords
         mpt(i)=' '
      enddo
      
      IFLAG = 0
      RLCP = LCP
      RSCP = SCP
      RAD = 57.29578
C
C     PROJECTION TYPES
      mpt(1) = 'POLAR_ORTHOGRAPHIC'
      mpt(2) = 'OBLIQUE_ORTHOGRAPHIC'
      mpt(3) = 'POLAR_STEREOGRAPHIC'
      mpt(4) = 'OBLIQUE_STEREOGRAPHIC'
      mpt(5) = 'LAMBERT'
      mpt(6) = 'MERCATOR'
      mpt(9) = 'NORMAL_CYLINDRICAL' 
      mpt(10) = 'SIMPLE_CYLINDRICAL'   !   (RECTANGULAR)
      mpt(11) = 'OBLIQUE_SIMPLE_CYLINDRICAL' 
      mpt(12) = 'SINUSOIDAL'
      mpt(13) = 'OBLIQUE_SINUSOIDAL'
      mpt(14) = 'MOLLWEIDE'
      mpt(15) = 'TRANSVERSE_MERCATOR'
      mpt(16) = 'POINT_PERSPECTIVE'

      DO 99 J = 1, NUMIPT
         CALL ZIA(RDATA,40)
c         CALL ZIA(WORK,1800)

c  map routine conversion
         CALL XVUNIT(INFIL(J),'INP',J,ISTAT,' ')
         CALL XVOPEN(INFIL(J),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +          'U_FORMAT','HALF',' ')

         call mp_init(mp,status)
         call mp_label_read(mp,infil(j),status)

c         call xvp('PCK_PATH',pck_file,count)
c         call mp_get_par(mp,pdf_parms,pds_keys,pck_file)

         call mp_get_keywords(mp,keys,number_keywords,types,classes,
     +                        status)
         call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',keytype,
     +				status)
         if (status.ne.mp_success) then
           call mabend('*** MIPS STATUS ERROR ***')
         endif
         tproj(J) = 0
         do i=1,mp_number_of_keywords
	   mkey = mpt(i)
	   ! find length of keyword, excluding blanks
	   len = mp_max_keywd_length
	   blank = .true.
           do while (blank)
	     if (mkey(len:len).ne.' ' .or. len.eq.1) then
		blank = .false.
	     else
		len = len-1
	     endif
	   enddo
           if (mkey.eq.keytype(:len))  tproj(J) = i
         enddo
         
	 L = tproj(J)
         if ((L .le.0).or.(L .gt.16)) then
           call mabend('** MAP PROJECTION TYPE ERROR **')         
         endif
         IF (L.EQ.7 .OR.L.EQ.8) THEN
           CALL XVMESSAGE(
     +      '***  THE FOLLOWING DATA SET IS NOT A PROJECTION:',' ')
           CALL PRNT(4,1,J,'FILE = .')
         else
	   call mp_mpo2buf( mp, rdata, status)
	   if (status.ne.mp_success) call mabend(
     +	    ' *** ERROR IN MPO2BUF ***')
         END IF

	if (l.ne.16) then
C     Redundant variables:      also equivalent:      name:
	 xc = RDATA(1) 		!  TSAMP(J)		SAMPLE POINT
	 zc = RDATA(2) 		!  TLINE(J)		LINE POINT
	 th = RDATA(3) 		!  TLAT(J)		LATITUDE POINT
	 th1 = RDATA(4) 	!  TLAT1(J)		LATITUDE ATA PARALLEL
	 th2 = RDATA(5) 	!  TLAT2(J)
	 lam = RDATA(6) 	!  TLONG(J)		LONGITUDE WEST 
	 tlong(j)=lam         
	 f = RDATA(7) 		!  TSCALE(J)		SCALE
	 MAP_RSLTN = RDATA(7)
c	 RDATA(8) = 0 ! visible pole not implemented in new MIPS, was: TPOLE(J)
	 psi = RDATA(9) 	!   TNOANG(J)		NORTH ANGLE
	endif
	 rp = RDATA(25) 	!   RPOLCK(J)
	 req = RDATA(26) 	!   REQCK(J) 
         TPROJ(J) = idata(39) 	!   PROJECTION TYPE 

C
C     IF RADIUS HAS NOT BEEN SPECIFIED BY USER,
C     USE IT IF IT IS IN MAP2 LABEL OF FIRST INPUT
         IF (REQ .EQ. 0.)   REQ = REQCK(J)
         IF (RPOLE .EQ. 0.) RPOLE = RPOLCK(J)
         IF (REQ .NE. 0.)   RDATA(26) = REQ
         IF (RPOLE .NE. 0.) RDATA(25) = RPOLE
         IF (REQ .NE. 0.)   REQCK(J) = REQ
         IF (RPOLE .NE. 0.) RPOLCK(J) = RPOLE
C	     IF USER SPECIFIED RADIUS INFO, IT OVERRIDES SEARCW
C         IF(REQ.NE.0.)RDATA(26) = REQ
C         IF(RPOLE.NE.0.)RDATA(25) = RPOLE
C
C     CIRCUM IS CIRCUMFERENCE OF PLANET IN PIXELS AT EQUATOR
         IF (SCALE .NE. 0.) CIRCUM = REQ*6.283185/SCALE  +.5
 	
         GO TO (8,8,8,8,8,7,9,2,6,5,8,8,8,8,8,2), L
         CALL MABEND('** PROGRAM ERROR **')	! (SHOULD BE CAUGHT ABOVE)

C     OBJECT SPACE:  CHECK FOCAL, OPT LINE,OPT SAMP TO SEE IF MAP2 LABELS
C     WERE USED
                  
 2	DO I = 27,30
            IF(RDATA(I).EQ.0.)GO TO 9
         ENDDO
         LINE(J) = RDATA(33) + .5
         IF (RDATA(33) .LT. 0.) LINE(J) = RDATA(33)-.5
         SAMP(J) = RDATA(34) + .5
         IF (RDATA(34) .LT. 0.) SAMP(J) = RDATA(34) - .5
         GO TO 9

    5    CONTINUE
C     SIMPLE CYLINDRICAL PROJECTION:  LONGITUDE CORRESPONDS TO SAMP = 1

         FLAT = 0.0      ! USE CONVEV TO FIND THE LINE,SAMP FOR 
         FLON = 0.0      ! LAT=0 AND LONG=0.
c  map routine canversion
         call mp_ll2xy(mp,fline,fsamp,flat,flon,1,status)
	 IF(status .NE. mp_success) THEN 
           if (status .EQ. -1008) then
              msg3='*************************************************'
              call xvmessage(msg3,' ')
              write(msg3(5:22),'(A)' ) ' MAP PROJECTION:'
              call xvmessage(msg3,' ')
              msg3='******  '
              write(msg3(8:),'(A)' ) keytype
              call xvmessage(msg3,' ')
              msg3='******  is NOT supported by MP_LL2XY ************'      
              call xvmessage(msg3,' ')
              msg3='*************************************************'
              call xvmessage(msg3,' ')
                call abend()     
            else
	       msg3='mp_ll2xy returned status = '
	       write(msg3(30:35),'(I6)') status   
               call xvmessage(msg3,' ')
               CALL MABEND('ERROR COMPUTING OFFSET')
            endif
         END IF
         LINE(J) = NINT( FLINE )
         SAMP(J) = NINT( FSAMP )
	 IF (SAMP(J).LT.1) SAMP(J) = SAMP(J) + CIRCUM
         IF (IADAPT.EQ.1) GO TO 9
         SSTART(J) = SCP-SAMP(J)
         SSTOP(J) = SCP-SAMP(J)+NOSAMP(J)-1
         IF(SSTOP(J).LT.0)SAMP(J) = SAMP(J)-CIRCUM
         GO TO 9

    6    CONTINUE

C     NORMAL CYLINDRICAL PROJECTION:  SAMP CORRESPONDS TO LONGITUDE = 0
         LINE(J) = NINT( RDATA(2) )
	 IF (NOMIN) THEN
            samp(j) = lam * MAP_RSLTN + 1! CENTER_LONGITUDE * MAP_RSLTN
	 ELSE
           RSAMP = (REQ/RDATA(7))*((RDATA(6))/RAD) + 1.
           SAMP(J) = RSAMP+.5
	 ENDIF
         IF(RSAMP.LT.0.)SAMP(J) = RSAMP-.5

C...........usually lat should be 0;  if not call CONVEV/TRANV to
c...........get line and samp for lat=0, long=0.

         IF (th.NE.0. .AND. .NOT.NOMIN)  THEN
           FLAT = 0.0      ! USE CONVEV TO FIND THE LINE,SAMP FOR 
           FLON = 0.0      ! LAT=0 AND LONG=0.

c  map routine conversion

c
           call mp_set_value(mp,'SAMPLE_PROJECTION_OFFSET',xc-1,
     1     status)
           call mp_ll2xy(mp,fline,fsamp,flat,flon,1,status)
           IF (status.NE.mp_success) THEN 
               CALL MABEND('TP1 ERROR COMPUTING OFFSET')
           END IF
           LINE(J) = NINT( FLINE )
           SAMP(J) = NINT( FSAMP )
         END IF

         GO TO 9
C
    7    CONTINUE
C     MERCATOR

         RLAT = RDATA(3)/RAD
         E = SQRT(1-(RPOLE/REQ)**2.)
         RLINE = (REQ/RDATA(7))*ALOG((1.-E*SIN(RLAT)/1.+E*SIN(RLAT))
     *        **(E/2.)*TAN(3.14159/4.+RLAT/2.))
         RSAMP = 1. + (REQ/RDATA(7)) * ((RDATA(6))/RAD)
         LINE(J) = RLINE+.5
         SAMP(J) = RSAMP+.5
         IF (RLINE .LT. 0.) LINE(J) = RLINE-.5
         IF (RSAMP .LT. 0.) SAMP(J) = RSAMP-.5
         IF (IADAPT .EQ. 1) GO TO 9
         SSTART(J) = SCP - SAMP(J)
         SSTOP(J) = SCP - SAMP(J) + NOSAMP(J) - 1
         IF (SSTOP(J) .LT. 0) SAMP(J) = SAMP(J) - CIRCUM
         GO TO 9

    8    CONTINUE

C     ORTHOGRAPHIC AND STEREOGRAPHIC PROJECTIONS

         LINE(J) = RDATA(2)+.5
         SAMP(J) = RDATA(1)+.5
         IF(RDATA(2).LT.0.)LINE(J) = RDATA(2)-.5
         IF(RDATA(1).LT.0.)SAMP(J) = RDATA(1)-.5
C     LPAR'S ARE LINES OF STANDARD PARALLELS
         IF(L.NE.5)GO TO 9
         LPAR1(J) = RDATA(4)+.5
         LPAR2(J) = RDATA(5)+.5
         IF(RDATA(4).LT.0.)LPAR1(J) = RDATA(4)-.5
         IF(RDATA(5).LT.0.)LPAR2(J) = RDATA(5)-.5

    9 call mp_free(mp)
      CALL XVCLOSE(INFIL(J),ISTAT,' ')

   99 CONTINUE
C
      nblocks = (numipt+5)/6
      do iblock=1,nblocks
        msgout='LONG= '
        ij=0
	i1 = (iblock-1)*6+1
	i2 = iblock*6
	if (i2.gt.numipt) i2 = numipt
        do i=i1,i2
          ij=ij+12
          write(msgout(ij:),'(E10.4)' ) tlong(i)
        enddo
        call xvmessage(msgout,' ')
        if (L.ne.6) then
	  msgout='SEARCW FOUND SAMP= '
	  ij=10
	  do i=i1,i2
	    ij=ij+10
	    write(msgout(ij:ij+3),'(I4)') samp(i)
	  enddo
	  call xvmessage(msgout,' ')
        endif
      enddo
C
C
C        CHECK FOR PICTURE COMPATABILITY
C
      CALL CHICK(IFLAG,REQCK,1,NUMIPT)
      CALL CHICK(IFLAG,RPOLCK,1,NUMIPT)
      CALL CHICK(IFLAG,TPROJ,1,NUMIPT)
      GO TO (10,10,10,10,20,30,40,40,35,35),L
C         ORTHOGRAPHIC AND STEREOGRAPHIC CHECK
   10 CALL CHICK(IFLAG,TLAT,2,NUMIPT)
      CALL CHICK(IFLAG,TLONG,2,NUMIPT)
      CALL CHICK(IFLAG,TSCALE,2,NUMIPT)
      CALL CHICK(IFLAG,TNOANG,2,NUMIPT)
      GO TO 40
C         LAMBERT CHECK
   20 CALL CHICK(IFLAG,TLAT1,2,NUMIPT)
      CALL CHICK(IFLAG,TLAT2,2,NUMIPT)
      CALL CHICK(IFLAG,TLONG,2,NUMIPT)
      CALL CHICK(IFLAG,TSCALE,2,NUMIPT)
      GO TO 40
C         MERCATOR CHECK
   30 CALL CHICK(IFLAG,TSAMP,2,NUMIPT)
      CALL CHICK(IFLAG,TLINE,2,NUMIPT)
      CALL CHICK(IFLAG,TSCALE,2,NUMIPT)
      GO TO 40
C         CYLINDRICAL CHECK
   35 CALL CHICK(IFLAG,TSCALE,2,NUMIPT)

   40 CONTINUE
      IF (IADAPT.EQ.0)GO TO 90
      CALL ADAPT(LINE,SAMP,NUMIPT,NLO,NSO,LCP,SCP,NOLINE,NOSAMP,L,
     +      CIRCUM, NOMIN)
      RLCP = LCP
      RSCP = SCP
      WRITE (MSG5,9900) LCP,SCP
9900  FORMAT (' THE PROGRAM CHOSE LCP=',I6,' AND SCP=',I6)
      CALL XVMESSAGE(MSG5(2:44),' ')
   90 CONTINUE
C
C  OPEN OUTPUT & ADD NEW MAP2 LABELS
C

	OFORM = 'BYTE'
	IF (ISWTCH.EQ.0) OFORM = 'HALF'
	CALL XVOPEN( OUTFIL, ISTAT, 'U_NL', NLO, 'U_NS', NSO, 'OPEN_ACT',
     &   'SA', 'IO_ACT', 'SA', 'OP', 'WRITE', 'U_FORMAT', 'HALF',
     &	 'O_FORMAT', OFORM,'U_NB',NBO,' ')
C
      DO  I = 1,39
         RDATAO(I) = RDATA(I)
      ENDDO
      if (l.ne.16) then
	RDATAO(1) = RSCP
	RDATAO(2) = RLCP
      endif

      call mp_init(mp,status)
      call mp_buf2mpo(rdatao,mp,status)

c  in Perspective case, if we change SCL,LCP, must change the OM-matrix!
      if (l.eq.16 .and. (rscp.ne.rdatao(34) .or.
     & rlcp.ne.rdatao(33))) then
	call mp_set_value( mp, 'PLANET_CENTER_LINE', rlcp, ind)
	call mp_set_value( mp, 'PLANET_CENTER_SAMPLE', rscp, ind)
	call mp_mpo2buf(mp,rdatao,status)
      endif

      call mp_label_write(mp,outfil,'PROPERTY',status)
      call mp_label_write(mp,outfil,'HISTORY',status)

c      IF (L .EQ. 6) THEN
C        Fixup Mercator label , find lat,long for line = samp = 1.
c        RDATAO(6) = RSCP/(REQ/(RDATAO(7)))*RAD
c        IF(RDATAO(6).GT.360.)RDATAO(6) = RDATAO(6)-360.
c        IF(RDATAO(6).LT.0.)RDATAO(6) = RDATAO(6)+360.
c        RDATAO(3) = RAD*(2*ATAN(EXP(RLCP/(REQ/RDATAO(7))))-3.14159/2)

c      ELSE IF ( L .EQ. 9 .OR. L .EQ. 10)  THEN
c        RDATAO(6) = 0.               ! FOR NORMAL OR SIMPLE CYLINDRICAL,
c        RDATAO(3) = 0.               ! LCP,SCP IS LAT 0, LONG 0.
c      ELSE
c        FLINE = LINE(NUMIPT)     ! USE CONVEV TO FIND THE LAT,LONG FOR LCP,SCP.
c        FSAMP = SAMP(NUMIPT)      

c  the above special cases were for peculiar requirements of maplabv2 
c  ... it makes more sense to apply the following to all projections:

	if (l.eq.16) then
	  fsamp = rdatao(34) 
	  fline = rdatao(33) 
	else
	  fsamp = RDATAO(1) 
	  fline = RDATAO(2) 
	endif

C        MODE = 2
c  map conversion routine -- CONVEV replaced by MP calls

c  for now use CONVEV for Perspective case as MP doesn't work yet
c	 if (l.eq.16) then
c	   call convev(ind,RDATAO,RDATAO,fline,fsamp,flat,flon,2,dum) 
c	   if (ind.ne.0) then
c             CALL MABEND('TP2 ERROR COMPUTING OFFSET')
c	   endif
c	 else
           call mp_xy2ll(mp,fline,fsamp,flat,flon,1,status)
	   IF(status .NE. mp_success) THEN 
	     msgout='Status returned by mp_xy2ll: '
	     write(msgout(30:35),'(I6)') status
	     call xvmessage(msgout,' ')
             CALL MABEND('TP2 ERROR COMPUTING OFFSET')
           END IF
c	 endif
         RDATAO(3) = FLAT
         RDATAO(6) = FLON
c      END IF
      call mp_free(mp)

C         Add new map2 labels to output
c  map routine conversion
C
      msgout='RPOL,REQ USED= '
      WRITE(MSGOUT(21:),'(E10.4)') RDATA(25)
      WRITE(MSGOUT(32:),'(E10.4)') RDATA(26)
      call xvmessage(msgout,' ')
 800  CONTINUE
C        Tell user how many lines and samples will be in output
      WRITE (MSG3,9910) NLO,NSO,NBO
9910  FORMAT (' THE OUTPUT PICTURE WILL CONTAIN',I6,' LINES, ',I6,
     +' SAMPLES AND ',I4,' BAND(S)')
      CALL XVMESSAGE(MSG3(2:83),' ') !63

 
  900 RETURN
      END

C*****************************************************************************
      SUBROUTINE CHICK(IND,INDATA,DCODE,NUMIPT)
C     THIS SUBROUTINE COMPARES ALL THE INPUTS TO THE FIRST TO SEE IF
C     THEY ARE MOSAICKABLE
      INTEGER INDATA(80),DCODE

      IF(NUMIPT.EQ.1) RETURN
      DO  I = 2,NUMIPT
         IF(INDATA(1).NE.INDATA(I))THEN
            CALL PRNT(4,1,I,'***DATA INCOMPATIBLE,INPUT NUMBER=.')
            IND = 1
         ENDIF
      ENDDO
      RETURN
      END

C*****************************************************************************
      SUBROUTINE MOSB(INBUF,LENGTH,OBUF,NSB,NUMBUF,NS4,
     &	   AVGBUF,NS4j,INARR,NS4I,LENCHK)
      INTEGER*2 INBUF(LENGTH),OBUF(NSO)
      REAL*4 AVGBUF(NSO),NUMBUF(NSO)

      COMMON/C1/PAR,NUMIPT,NUMLIN,LSET,RSET,LTHRES,RTHRES,LSEQ,
     + RSEQ,LNIB,RNIB,LNIBST,RNIBST,NSEQ,ITHRES,ITHRST,BLKSIZ,NSI,
     + LINE,SAMP,SCP,NSO,NIBB,NIBBST,NSEQST,RSEQST,LSEQST,LCP,
     + LTHRST,RTHRST,NOSAMP,NOLINE,REFIN,DCLE,INCR,ISWTCH,NLO,
     + IAVER,ISMOOTH,NLR,REQ,RPOLE,LAB,weight,NOBAND,JSB,NBO,NBI
      INTEGER RSET,RTHRES,RSEQ,RNIB,RNIBST,BLKSIZ,RSEQST,RTHRST
      real*4 weight(80)
	 INTEGER*4 JSB,NBO,NBI
      COMMON/C2/N,I,KK,LABO,J,IND,OREC,NK,M,K,NUML,NUMR,NSAM,REFINL,
     +	II,IL,REFINR,IR,REND,LEND,L,LL,LINO,RDN,RNUM
      INTEGER OREC,REFINL,REFINR,REND
      INTEGER LINE(80),SAMP(80),SCP,DCLE,NOLINE(80),PAR(100),
     +	    NOSAMP(80),NOBAND(80),LAB(80),REFIN(80)
      COMMON/C3/LSTART(80),LSTOP(80),SSTART(80),SSTOP(80),TSTART(80)
      INTEGER SSTART,SSTOP,TSTART
	 INTEGER*4 BAND
      COMMON/FILES/INFIL,OUTFIL
      INTEGER infil(80), OUTFIL

      CHARACTER*132 MSG
      character*80 msgout
      INTEGER FLAG(80), FILEOPEN(80), NUM_FILESOPEN
	 INTEGER*4 BANDOUT,LINEOUT

      INTEGER INARR(NSO), INOVER(80)
 1111	format(A13,I6)

      NUM_FILESOPEN = 0
      CALL ZIA(INOVER,80)
      CALL ZIA(FLAG,80)
      CALL ZIA(FILEOPEN,80)
      CALL ZIA(INARR,NSO)
      IF (LENCHK.NE.LENGTH+NSB+NS4+NS4) GOTO 217
C
C     ....Resolve nibble parameter hierarchy
      IF (NIBBST.NE.0) THEN
         IF (LNIBST.EQ.0) LNIB=NIBB
         IF (RNIBST.EQ.0) RNIB=NIBB
         IF (NIBBST.EQ.1) LNIBST=1
         IF (NIBBST.EQ.1) RNIBST=1
      ENDIF
C
C     ....Resolve threshold parameter hierarchy
      IF (ITHRST.NE.0) THEN
         IF (LTHRST.EQ.0 .AND. ITHRST.EQ.1) LTHRES=ITHRES
         IF (RTHRST.EQ.0 .AND. ITHRST.EQ.1) RTHRES=ITHRES
      ENDIF
      IF (NSEQST.NE.0) THEN
         IF (LSEQST.EQ.0) LSEQ=NSEQ
         IF (RSEQST.EQ.0) RSEQ=NSEQ
      ENDIF
C     ....Compute amount of nibbling
      IF (LNIBST.NE.1) THEN
         LSET = (LSEQ-1)*INCR
      ELSE
         LSET = (LSEQ-1)*INCR-LNIB
      ENDIF
      IF (RNIBST.NE.1) THEN
         RSET = (RSEQ-1)*INCR
      ELSE
         RSET = (RSEQ-1)*INCR-RNIB
      ENDIF
      msgout='LTHRES,RTHRE= '
      write(msgout(17:22),'(I6)' ) LTHRES
      write(msgout(27:32),'(I6)' ) RTHRES
      call xvmessage(msgout,' ')
	
      msgout='ITHRES= '
      write(msgout(17:22), '(I6)' ) ITHRES
      call xvmessage(msgout,' ')
      msgout='DCLE= '
      write(msgout(15:20), '(I6)' ) DCLE
      call xvmessage(msgout,' ')
C

C     ....Determine offsets of inputs in output picture
      DO J=1,NUMIPT
         LSTART(J) = LCP-LINE(J)+1	!OUTPUT LINE FOR LINE 1 OF INPUT J
         LSTOP(J) = LCP-LINE(J)+NOLINE(J) !LINE NL(J)
         SSTART(J) = SCP-SAMP(J)+1	!SAMP 1 OF J
         SSTOP(J) = SCP-SAMP(J)+NOSAMP(J) !SAMP NS(J)
         TSTART(J) = SSTART(J)-REFIN(J)
         IF (LSTART(J).GT.NLO .OR. LSTOP(J).LT.1 .OR.
     &		SSTOP(J).LT.1 .OR. SSTART(J).GT.NSO) THEN
            WRITE (MSG,9920) J,LSTART(J),SSTART(J)
9920  FORMAT (' INPUT ',I2,' DOES NOT LIE IN OUTPUT FIRST LINE =',I6,
     +' FIRST SAMPLE =',I6)
            CALL XVMESSAGE(MSG(2:72),' ')
         ENDIF
      ENDDO
C
      call xvmessage(' ',' ')
      nblocks = (numipt+5)/6
      do iblock=1,nblocks
	i1 = (iblock-1)*6+1
	i2 = iblock*6
	if (i2.gt.numipt) i2 = numipt
        j=6
        msgout='LSTART= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') LSTART(i)
        enddo
        call xvmessage(msgout,' ')
        j=6
        msgout='LSTOP= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') LSTOP(i)
        enddo
        call xvmessage(msgout,' ')
        j=6
        msgout='SSTART= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') SSTART(i)
        enddo
        call xvmessage(msgout,' ')
        j=6
        msgout='SSTOP= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') SSTOP(i)
        enddo
        call xvmessage(msgout,' ')
        call xvmessage(' ',' ')
      enddo
C
      numip=numipt
      if(ismooth.eq.1) numip=numipt/2 ! Only use half the input images
C

C     ....Start of band loop
	 BANDOUT = 0
	 DO 250 BAND=JSB,JSB+NBO-1
	    BANDOUT = BANDOUT + 1
	    LINEOUT = 0
C     ....Start of line loop
      DO 200 LINO=1,NLO			!LINO is line number in output
         OREC = LINO + NLR
	    LINEOUT = LINEOUT + 1
	 do ii=1,nso
	    obuf(ii)=DCLE
	    numbuf(ii)=0.0
	    avgbuf(ii)=0.0
	 enddo
C
C     ....Read in the input lines
      DO J=1,NUMIPT
C         CALL XVUNIT(INFIL(J),'INP',J,ISTAT,' ')
C         CALL XVOPEN(INFIL(J),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
C     +          'U_FORMAT','HALF',' ')
         IF (SSTOP(J).GE.1 .AND. SSTART(J).LE.NSO .AND.
     &        LINO.GE.LSTART(J) .AND. LINO.LE.LSTOP(J)) THEN
            IF (FILEOPEN(J) .EQ. 0) THEN
              
              CALL XVOPEN(INFIL(J),ISTAT,'OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','HALF',' ')
              FILEOPEN(J) = 1
              NUM_FILESOPEN = NUM_FILESOPEN + 1
            ENDIF
            FLAG(J) = 0		!Output line is in input j
            CALL XVREAD(INFIL(J),INBUF(REFIN(J)),ISTAT,
     &        'NSAMPS',NOSAMP(J),'LINE',LINO-LSTART(J)+1,
     &	    'BAND',BAND,' ')
         ELSE
            FLAG(J) = 1		!Output line does not lie in input
         ENDIF
C
C 45 is the max number of file that can be open. This will actually
C vary on different machines, but 45 is a good number to start with

C IF OUTPUT LINE DOES NOT LIE IN INPUT IMAGE AND ALSO THAT IMAGE IS
C OPEN THEN CLOSE THAT IMAGE FILE. ELSE IF 45 FILES HAVE BEEN OPEN
C THEN FIND A FILE THAT IS OPEN AND CLOSE IT.

         IF (FLAG(J) .EQ. 1 .AND. FILEOPEN(J) .EQ. 1) THEN
           CALL XVCLOSE(INFIL(J),ISTAT,' ')
           FILEOPEN(J) = 0
           NUM_FILESOPEN = NUM_FILESOPEN - 1
         ELSE IF (NUM_FILESOPEN .GE. 45) THEN
           DO M=1,NUMIP
             IF (FILEOPEN(M) .EQ. 1) GOTO 101
           ENDDO
           GOTO 299
 101	   CALL XVCLOSE(INFIL(M),ISTAT,' ')
           FILEOPEN(M)=0
           NUM_FILESOPEN=NUM_FILESOPEN-1
         ENDIF
      ENDDO
      if (num_filesopen .lt. 0) then
        call xvmessage('*******************It is here',' ')
      endif
C

C     ....All input data sets are now in buffer
      DO 80 M=1,NUMIP
      K = NUMIP - M + 1	!Look at last input data set first
      IF (FLAG(K).EQ.1) GOTO 80 !Skip if output line is not in input
      NUML = 0
      NUMR = 0
      NSAM = NOSAMP(K)
C     ....Search for left threshold trigger
      REFINL = REFIN(K) - 1
      DO I=1,NSAM,INCR
         IL = I + REFINL
         IF (INBUF(IL).LT.LTHRES) THEN
            NUML = 0
         ELSE
            NUML = NUML+1
            IF (NUML.GE.LSEQ) GOTO 30
         ENDIF
      ENDDO
      GOTO 80		!Skip if trigger not found
C

C     ....Look for right threshold trigger
   30 REFINR = NSAM + REFIN(K)
      DO I=1,NSAM,INCR
         IR = REFINR - I
         IF (INBUF(IR).LT.RTHRES) THEN
            NUMR = 0
         ELSE
            NUMR = NUMR + 1
            IF (NUMR.GE.RSEQ) GOTO 50
         ENDIF
      ENDDO
      GOTO 80		!Skip if trigger not found
C

C     ...,Both left and right triggers have been found
C     ....Check that more than 2*nseq triggers exist
   50 IF (IR-IL.LT.LSEQ+RSEQ) GOTO 80
C     ....A valid threshold has been found
      REND = IR+RSET
      LEND = IL-LSET

      IF (IAVER.EQ.1) THEN	!Use averaging routine
	DO L=LEND,REND
          LL = L+TSTART(K)
          IF (LL.GT.0 .AND. LL.LE.NSO .AND. INBUF(L).GE.ITHRES) THEN
	    AVGBUF(LL) = INBUF(L)*weight(k) + AVGBUF(LL)
	    NUMBUF(LL) = NUMBUF(LL) + weight(k)
	  ENDIF
	ENDDO

      ELSE IF (ISMOOTH.EQ.1) THEN	!Use weighting routine
	DO L=LEND,REND
          LL = L+TSTART(K)
          IF (LL.GT.0 .AND. LL.LE.NSO .AND. INBUF(L).GE.ITHRES) THEN
            lmask=L-refin(k)+refin(k+numip) ! corresponding mask pixel
	    AVGBUF(LL) = real(INBUF(L))*real(inbuf(lmask))*weight(k) + 
     +                   AVGBUF(LL)
	    NUMBUF(LL) = NUMBUF(LL) + inbuf(lmask)*weight(k)
	  ENDIF
	ENDDO

      ELSE                      ! First in gets priority option
	DO L=LEND,REND
	  LL = L+TSTART(K)	!Add in offsets
	  IF (LL.GT.0.AND.LL.LE.NSO) THEN
	    IF (INBUF(L).GE.ITHRES) OBUF(LL)=INBUF(L)
	  ENDIF
	ENDDO
      ENDIF

   80 CONTINUE

c  now do smoothing and/or averaging ...
      IF (IAVER.EQ.1.or.ismooth.eq.1) THEN	!Find weighted average
	DO I=1,NSO
	  IF (NUMBUF(I).NE.0.0) THEN
	    OBUF(I) = nint(avgbuf(i)/numbuf(i))
	  ELSE
	    OBUF(I) = DCLE
	  ENDIF
	ENDDO
      ENDIF

C     ....Write output data set
c      CALL XVWRIT(OUTFIL,OBUF,ISTAT,'LINE',OREC,' ')
	 CALL XVWRIT(OUTFIL,OBUF,ISTAT,'LINE',LINEOUT,'BAND',BANDOUT,' ')
C
  200 CONTINUE
 250	 CONTINUE
      RETURN

  217 CALL XVMESSAGE('***Not enough memory',' ')
      CALL ABEND
 299	CALL XVMESSAGE('***UNKNOWN ERROR***',' ')
        CALL XVMESSAGE('***ACCORDING TO NUM_FILESOPEN THERE',' ')
        CALL XVMESSAGE('   THE LIMIT OF OPEN FILES HAVE BEEN',' ')
        CALL XVMESSAGE('   REACHED. BUT ACCORDING TO FILEOPEN',' ')
        CALL XVMESSAGE('   THERE IS NO OPEN FILE!!!!!',' ')
        CALL ABEND
      END

C*****************************************************************************
      SUBROUTINE ADAPT(LINE,SAMP,NUMIPT,NLO,NSO,LCP,SCP,NOLINE,NOSAMP,
     & L,CIRCUM, NOMIN)
C
C 12/29/82 -JAM- ORIGINAL CODE BEFORE MODS
C 15-JAN-87 ...LWK... FIXED CODE FOR CASE OF LONGITUDE OVERLAP
C 11-NOV-87 ...LWK... FIXED BUGS IN THE 'IRANK' DETERMINATION
C 12/30/87  ...SP.... ADDED NOMIN KEYWORD, ADDED 1 TO SMAX FOR CASE OF NO GAP.
C 10-July-95...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C  THIS SUBROUTINE DETERMINES THE SIZE OF A MOSAIC.
C  CHOOSE LCP AND SCP, NLO, AND NSO TO CONTAIN INPUT PICTURES
C  COMMON POINT IS CHOSEN TO BE AT THE MAXIMUM (LINE,SAMP) IN THE OUTPUT.
C
      COMMON/C4/I,J,INLO,INSO,LMAX,LMIN,SMAX,SMIN,ISTART
      INTEGER CIRCUM
      LOGICAL NOMIN
      INTEGER LINE(80),SAMP(80),NOSAMP(80),NOLINE(80),SCP,SMAX,SMIN,
     & IRANK(80)
C
      CALL ZIA(I,9)
      IFLAG = 0
      IF (L.LT.6 .OR. L.EQ.7 .OR. L.EQ.8 .OR. L .GT. 10 ) GOTO 40
      IF (NOMIN) GO TO 40

C  CHECK CYLINDRICAL TYPE PROJECTIONS FOR WRAPAROUND.
C  DETERMINE GAPS IN OVERLAP IN SAMPLE (LONGITUDE) DIRECTION,
C  AND PUT START OF OUTPUT IMAGE AT EDGE OF (LARGEST) GAP:
C
C  INITIALIZE RANKING ARRAY, AND ADD OFFSET TO ANY IMAGES THAT
C  CONTAIN LONG=0.
C  (IRANK(I) = IMAGE NUMBER THAT IS RANKED NUMBER I)
      DO I = 1,NUMIPT
	IRANK(I) = I
      ENDDO
C  REMOVE FROM CONSIDERATION ANY IMAGES THAT LIE ENTIRELY
C  INSIDE ANOTHER IMAGE (IN LONG.).
      NUMI = NUMIPT
      DO I = 1,NUMIPT
	IF (I.GT.NUMI) GO TO 20
	II = IRANK(I)
        DO J = 1,NUMI
	  IF (I.EQ.J) GO TO 10
5	  JJ = IRANK(J)
          IF ((SAMP(II).LE.SAMP(JJ)) .AND.
     &     (SAMP(II)-NOSAMP(II).GE.SAMP(JJ)-NOSAMP(JJ))) THEN
	    IF (I.EQ.NUMI) THEN	! IF IT'S THE LAST IMAGE
	      NUMI = NUMI-1	! ... JUST REMOVE IT
	      GO TO 20		! ... AND QUIT
	    ENDIF
	    ISAVE = IRANK(NUMI)
	    IRANK(NUMI) = II	! ASSIGN TO II RANK NUMI
C  ASSIGN RANK I TO JJ RATHER THAN SIMPLY SWITCHING NUMI & I, BECAUSE
C  WE CAN INFER THAT IMAGE JJ DOES NOT LIE (IN SAMP DIMENSION) INSIDE
C  IMAGES OF RANKS 1,..,J-1, BUT THIS IS NOT NECESSARILY TRUE OF ISAVE.
	    IRANK(I) = JJ	
	    II = JJ		! REDEFINE THIS TEMPORARY VARIABLE
	    IRANK(J) = ISAVE
	    NUMI = NUMI-1	! EXCLUDE RANK NUMI
	    GO TO 5		! NOW WE MUST CHECK THE NEW J
	  ENDIF
10	  CONTINUE
	ENDDO
      ENDDO

C  SORT IMAGES IN DECR'G SAMPLE DIRECTION:
20    DO I = 1,NUMI-1
	II = IRANK(I)
	DO J = I+1, NUMI
	  JJ = IRANK(J)
	  IF (SAMP(II) .LT. SAMP(JJ)) THEN
	    IRANK(I) = JJ
	    IRANK(J) = II
	    II = IRANK(I)
	  ENDIF
	ENDDO
      ENDDO
C  NOW FIND LARGEST GAP (LGAP) AND THE IMAGE RANK # FOLLOWING IT (IGAP):
      LGAP = 0
      IGAP = 0
      DO I = 1,NUMI
	IF (I.EQ.1) THEN
	  J1 = IRANK(NUMI)
	ELSE
	  J1 = IRANK(I-1)
	ENDIF
	J2 = IRANK(I)
	LG = SAMP(J1)-NOSAMP(J1)-SAMP(J2)
	IF (SAMP(J1).LT.SAMP(J2)) LG = LG+CIRCUM
	IF (LG.GT.LGAP) THEN
	  LGAP = LG
	  IGAP = I
	ENDIF
      ENDDO
C
      IF (IGAP.EQ.0) THEN		!NO LONG. GAPS:  SET EDGE AT LONG.=0
	SMAX = CIRCUM+1
	SMIN = 1
      ELSE
	SMAX = SAMP(IRANK(IGAP))
	SMIN = SMAX+LGAP
	IF (SMIN.GT.SMAX) THEN
	  SMIN = SMIN-CIRCUM
	  DO I = 1,NUMIPT
            IF (SAMP(I).GT.SMAX) SAMP(I) = SAMP(I)-CIRCUM
	  ENDDO
	ENDIF
      ENDIF
C
C  DETERMINE LINE RANGE:
      LMAX = LINE(1)
      LMIN = LINE(1) - NOLINE(1)
      DO  I = 2, NUMIPT
        IF (LINE(I).GT.LMAX) LMAX = LINE(I)
        IF (LINE(I)-NOLINE(I).LT.LMIN) LMIN = LINE(I)-NOLINE(I)
      ENDDO
C
      GO TO 50
C
C  ALIGN ALL INPUTS RELATIVE TO COMMON POINT TO DETERMINE TOTAL
C  SIZE OF OUTPUT:
   40 LMAX = LINE(1)
      LMIN = LINE(1) - NOLINE(1)
      SMAX = SAMP(1)
      SMIN = SAMP(1) - NOSAMP(1)
      DO  I = 2, NUMIPT
        IF (LINE(I)-NOLINE(I).LT.LMIN) LMIN = LINE(I)-NOLINE(I)
        IF (LINE(I).GT.LMAX) LMAX = LINE(I)
        IF (SAMP(I)-NOSAMP(I).LT.SMIN) SMIN = SAMP(I)-NOSAMP(I)
        IF (SAMP(I).GT.SMAX) SMAX = SAMP(I)
      ENDDO
C
C  FIND LCP AND SCP.  IMAGE FURTHEST TO THE LEFT SHOULD START IN SAMPLE 1.

   50 LCP = LMAX
      SCP = SMAX
C
c  FIND NLO AND NSO NEEDED TO HOLD ALL FRAMES, SUBJECT TO MAXIMA
C  SPECIFIED BY USER:
      INLO = LMAX - LMIN
      INSO = SMAX - SMIN
      IF (NLO.LT.INLO) THEN
	CALL XVMESSAGE('***LINES TRUNCATED',' ')
      ELSE
	NLO = INLO
      ENDIF
      IF (NSO.LT.INSO) THEN
	CALL XVMESSAGE('***SAMPLES TRUNCATED',' ')
      ELSE
	NSO = INSO
      ENDIF
C
      RETURN
      END
