      INCLUDE 'VICMAIN_FOR'
C  95-1-2  ...AS....  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C  85-3-28 ...LWK...  CONVERTED TO VAX VICAR2, ADDED STACKA CALL
C                      & 'SHIFT' KEYWORD
      SUBROUTINE MAIN44
c      IMPLICIT INTEGER (A-Z)
	implicit none
	integer*4 iun,istat,sl,ss,nlo,nso,nl,ns,cnt,def
	integer*4 nb1,nb2,icode
      	INTEGER*4 AREA(4)
      	EXTERNAL WORK
c
       	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*8 format
      CALL IFMESSAGE('PSF version 2016-06-08')
      CALL XVEACTION('SA',' ')

      CALL XVUNIT( IUN, 'INP', 1, ISTAT,' ')
c      CALL XVOPEN( IUN, ISTAT, 'U_FORMAT', 'HALF', ' ')
c      CALL XVSIZE( SL, SS, NLO, NSO, NL, NS)
        call xvopen(iun,istat,'OP', 'READ','OPEN_ACT','SA','IO_ACT','SA',' ')
	call xvsize( sl, ss, nlo, nso, nl, ns)
        call xvget(iun,istat,'FORMAT',format,' ')

        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend
        endif
        call xvclose(iun,istat,' ')

        call xvopen(iun,istat,'OPEN_ACT','SA','IO_ACT','SA',
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')            !FMT(INCODE),' ')

      CALL XVPARM( 'AREA', AREA, CNT, DEF, 4)	! (NO DEFAULT ALLOWED)
      IF (AREA(3).EQ.(AREA(3)/2)*2) THEN
	AREA(3)=AREA(3)+1
	CALL XVMESSAGE('NL OF AREA INCREASED BY 1',' ')
      ENDIF
      IF (AREA(4).EQ.(AREA(4)/2)*2) THEN
	AREA(4)=AREA(4)+1
	CALL XVMESSAGE('NS OF AREA INCREASED BY 1',' ')
      ENDIF

      NB1 = 4*MAX0( NS, NSO)		! 'IN' BUFFER SIZE (IN BYTES)
      NB2 = 4*AREA(4)			! PSF BUFFER SIZE (IN BYTES)
c	10 parms, name, 2 arrays, arr1 size, arr2 size, other variables...
      CALL STACKA( 11,WORK, 2, NB1, NB2, NL, NS, NLO, NSO, AREA, IUN, icode)

      RETURN
      END

C*****************************************************************
      SUBROUTINE WORK( IN, NB1, PSF, NB2, NL, NS, NLO, NSO, AREA, IUN, icode)
c      IMPLICIT INTEGER (A-Z)
	implicit none
      	integer*4 nb1,nb2
      	REAL*4 IN( NB1/4), PSF(NB2/4)
      	INTEGER*4 AREA(4)
	integer*4 nl,ns,nlo,nso,iun,icode,j,l,n
	integer*4 icenx,iceny,ilin,ir,lb
	integer*4 il,lt,nlin,nlin2,nlinl,npix,nsa2,nsal
	integer*4 olin,oun,istat
      	LOGICAL*4 XVPTST,negval
	real*4 mean, xn, limit
      	REAL*4 XDN, CENX, CENY
      	CHARACTER*4 format
c
       	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/

	print *,'NB1,NB2, = ',nb1,nb2
c      CALL XVGET( IUN, STAT, 'FORMAT', FMT,' ')
c      IF (FMT.NE.'BYTE' .AND. FMT.NE.'HALF' .AND. FMT.NE.'WORD') THEN
c	CALL MABEND('??E -  ONLY BYTE & HALFWORD FORMATS SUPPORTED **')
c      ENDIFa
	negval = .false.
	format = fmt(icode)
      if (format.eq.fmt(1)) then
	LIMIT = 255.
      else if (format .eq. fmt(2)) then
	LIMIT = 32767.
      else if (format .eq. fmt(3)) then
	LIMIT = 65535.
      else if (format .eq. fmt(4)) then
	LIMIT = 3.4028234e38
      endif

      LT = AREA(1)
      LB = AREA(3)+LT-1
      IL = AREA(2)
      IR = AREA(4)+IL-1
      IF (LB.GT.NL) LB=NL
      IF (IR.GT.NS) IR=NS
      IF (IL.GT.NS .OR. LT.GT.NL) THEN
	CALL MABEND('??E - AREA IS OUTSIDE INPUT IMAGE **')
      ENDIF
      NLIN=LB-LT+1
      NPIX=IR-IL+1
      NLIN2=NLIN/2
      NLINL=NLIN-NLIN2
      NSA2=NPIX/2
      NSAL=NPIX-NSA2

      CALL XVUNIT( OUN, 'OUT', 1, istat,' ')
      CALL XVOPEN( OUN, istat, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS',
     .	NSO, 'U_FORMAT', fmt(4), 'O_FORMAT',fmt(icode), ' ')

C  READ BORDER POINTS AND GET BKG DN
C
      N=0
      MEAN=0.0
      CALL XVREAD( IUN, IN, istat, 'LINE', LT,' ')
      DO L=LT,LB
	IF (L.EQ.LT .OR. L.EQ.LB) THEN
	  DO J=IL,IR
	    N=N+1
	    MEAN=MEAN+IN(J)
	    if (IN(J).lt.0) negval=.true.
	  ENDDO
	ELSE
	  N=N+2
	  if (IN(IL).lt.0 .or. IN(IR).lt.0) negval=.true.
	  MEAN=MEAN+IN(IL)+IN(IR)
	ENDIF
	IF (L.LT.LB) CALL XVREAD( IUN, IN, istat,' ')
      ENDDO
      MEAN=NINT(MEAN/FLOAT(N))
      CALL PRNT(7,1,MEAN,'BACKGROUND DN =')
        if (negval) then
           call xvmessage ('??W - WARNING: Image contains negative values',' ' )
        endif
C
C  DETERMINE CENTROID - negative values are zeroed 
C
      XDN = 0.0
      CENX = 0.0
      CENY = 0.0
      xn = 0.0
      CALL XVREAD( IUN, IN, istat, 'LINE', LT,' ')
      DO L=LT,LB
	DO J=IL,IR
	  XN = IN(J)-MEAN
	  IF (XN.LT.0.0) XN=0.0
	  XN = XN*XN
	  XDN = XDN+XN
	  CENX = CENX+FLOAT(J-IL)*XN
	  CENY = CENY+FLOAT(L-LT)*XN
	ENDDO
	IF (L.LT.LB) CALL XVREAD( IUN, IN, istat,' ')
      ENDDO
      ICENX = CENX/XDN+IL+.5	! Round up
      ICENY = CENY/XDN+LT+.5
      CALL PRNT(4,1,ICENX,'X CENTROID =')
      CALL PRNT(4,1,ICENY,'Y CENTROID =')
C
C  SET UP AREA TO CONTAIN PSF
C IF 'SHIFT' WAS SPECIFIED, RETAIN ORIGINAL SIZE, MOVE CENTER.
C ELSE RETAIN ORIGINAL AREA & QUARTER THE PSF UNEQUALLY.
C **> (MAY BE BETTER TO REDUCE SIZE & QUARTER SYMMETRICALLY!)
      IF (XVPTST( 'SHIFT')) THEN
	LB=ICENY+NLIN2		! BOTTOM LINE OF PSF
	LT=ICENY-NLIN2		! TOP LINE OF PSF
	IR=ICENX+NSA2		! RIGHT EDGE OF PSF
	IL=ICENX-NSA2		! LEFT EDGE OF PSF
	IF (LB.GT.NL) LB=NL
	IF (IR.GT.NS) IR=NS
	IF (LT.LT.1) LT=1
	IF (IL.LT.1) IL=1
	NLIN=LB-LT+1		! NL OF PSF
	NPIX=IR-IL+1		! NS OF PSF
	NSA2=NPIX/2
	NLIN2=NLIN/2
      ELSE
	NLIN2 = ICENY-LT+1
	NSA2 = ICENX-IL+1
      ENDIF
      NLINL=NLIN-NLIN2
      NSAL=NPIX-NSA2
C
C  COPY DATA TO OUTPUT AND SUBTRACT BACKGROUND
C
      N=0
      DO OLIN = 1, NLO			! OUTPUT LINE NUMBER
	CALL MVE(7,NSO,0,IN,0,1)	! ZERO THE OUTPUT BUFFER
	call mve(7,NB2/4,0,PSF,0,1)		! ZERO THE PSF
	ILIN = 0			! ZERO THE INPUT LINE NUMBER
	IF (OLIN.LE.NLINL)
     .	 ILIN = OLIN+LT-1+NLIN2  	! BOTTOM OF PSF
	IF (OLIN.GE.NLO-NLIN2+1)
     .	 ILIN = OLIN-NLO+NLIN2+LT-1	! TOP OF PSF
	IF (ILIN.NE.0) THEN
	  CALL XVREAD( IUN, PSF, istat, 'LINE', ILIN, 'SAMP', IL,
     .	   'NSAMPS', NPIX,' ')		! READ THE PSF AREA
	  CALL SUBV(7,NPIX,MEAN,PSF,0,1) ! SUBTRACT BACKGROUND
	  CALL TRUNC(PSF,0.0,LIMIT,NPIX)
	  CALL MVE(7,NSA2,PSF,IN(NSO-NSA2+1),1,1) ! LEFT SIDE OF PSF
	  CALL MVE(7,NSAL,PSF(NSA2+1),IN,1,1)	! RIGHT SIDE OF PSF
	ENDIF

	CALL XVWRIT( OUN, IN, istat,' ')
      ENDDO

	call outparm (negval)
      RETURN
      END
c==============================================================
	subroutine outparm (negval)
c
c	return output parameter negative
c
      include 'pgminc'            ! FOR XQINI...

	integer*4 istat, negative
	integer*4 PARB(xprdim)
	logical*4 negval
c
	negative = 0
	if (negval) negative = 1	
	call xqini(parb, xprdim, xabort)
	call xqintg(parb,'NEGATIVE',1,negative,xadd,istat)      
	call xqout(parb,istat)
	return
	end
c==========================================================
      SUBROUTINE TRUNC( BUF, LO, HI, N)
C  TRUNCATE N BUF VALUES TO RANGE (LO, HI).
      IMPLICIT none 
      integer*4 i,n
      real*4 lo,hi,BUF(1)


      DO I=1,N
	IF (BUF(I).LT.LO) BUF(I) = LO
	IF (BUF(I).GT.HI) BUF(I) = HI
      ENDDO
      RETURN
      END
