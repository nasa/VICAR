C  PROGRAM WIENER  --  APPLY WIENER FILTER TO A FOURIER TRANSFORM

c  Nov 1996  JJL
C  2-JAN-95 ..CRI..   MSTP S/W Conversion (VICAR Porting)
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C  APR-85  ...LWK...  INITIIAL VERSION, FROM IBM PGM 'RESTORE'
C  MAY-85  ...LWK...  ADDED AP CODE, FROM IBM PGM 'GPFILT'

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT INTEGER (A-Z)
      parameter(maxsize=4096)
      INTEGER*4 IUN(3), OUN
      REAL*4 SN,rnorm,psfnor,outnor,mtf,factor
      complex im(maxsize),otf(maxsize),restor(maxsize)
      complex ratio
      CHARACTER*8 FMT
      logical xvptst

      CALL IFMESSAGE('WIENER version Nov 1996')

c open & check inputs
      CALL XVPCNT( 'INP', NIDS)
      DO I=1,NIDS
	CALL XVUNIT( IUN(I), 'INP', I, ISTAT,' ')
	CALL XVOPEN(IUN(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XVGET( IUN(I), STATUS, 'FORMAT', FMT, 'NL', NL1, 'NS',
     .	 NS1,' ')
	IF (FMT.NE.'COMPLEX' .AND. FMT.NE.'COMP') THEN
	  CALL XVMESSAGE('ALL INPUT FILES MUST BE COMPLEX',' ')
	  CALL ABEND
	ENDIF
	IF (I.EQ.1) THEN
	  NL = NL1
	  NS = NS1
	ELSEIF (NL1.NE.NL .OR. NS1.NE.NS) THEN
	  CALL XVMESSAGE('ALL INPUT FILES MUST BE SAME SIZE',' ')
	  CALL ABEND
	ENDIF
      ENDDO

c open outputs
      CALL XVUNIT( OUN, 'OUT', 1, ISTAT,' ')
      CALL XVOPEN( OUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     . 'OP', 'WRITE',' ')

c get parameters
      CALL XVPARM( 'SN', SN, I, J,1)
      SN = 1./SN**2

      if(xvptst('wiener'))then
        factor=1.0/(1.0+sn)
        factor=1.0/factor
      endif

C process image
      DO L=1,NL
	CALL XVREAD( IUN(1), IM, I,' ')
	CALL XVREAD( IUN(2), OTF, I,' ')

	IF (L.EQ.1)then
           RNORM = REAL(OTF(1))
           psfnor=real(IM(i))
        endif

c       normalize the otf
        do i=1,ns
          otf(i)=otf(i)/rnorm
        enddo

c       Apply direct otf  filter
        if(xvptst('direct'))then
  	  DO I=1,NS
	    RESTOR(I)=OTF(I)*IM(I)
	  ENDDO
        endif

c       Apply wiener restoration filter
        if(xvptst('wiener'))then
  	  DO I=1,NS
	    RESTOR(I)=IM(i)*factor*CONJG(OTF(I))/
     +        ((REAL(OTF(I)))**2+(AIMAG(OTF(I)))**2+SN)
	  ENDDO
cccccc      if(L.eq.1)restor(1)=cmplx(psfnor,0.0)
        endif

c       Obtain ratio restoration filter
        if(xvptst('ratio'))then
  	  DO I=1,NS
            if((real(OTF(i)))**2+(aimag(OTF(i)))**2.ne.0.)then
              ratio=IM(i)/(psfnor*OTF(i))
       	      RESTOR(I)=CONJG(ratio)/
     +        ((REAL(ratio))**2+(AIMAG(ratio))**2+SN)
            else
              RESTOR(i)=cmplx(0.,0.)
            endif
	  ENDDO
        endif

c       Apply amplitude restoration filter
        if(xvptst('amplitude'))then
  	  DO I=1,NS
            mtf=sqrt((real(IM(i)))**2+(aimag(IM(i)))**2)/
     +         (psfnor*sqrt((real(OTF(i)))**2+(aimag(OTF(i)))**2+
     +         .00001))
	    RESTOR(I)=IM(i)*mtf/(mtf*mtf+SN)
	  ENDDO
          if(L.eq.1)restor(1)=cmplx(psfnor,0.0)
        endif

c       Optionally APPLY MTF OF DESIRED OUTPUT
	IF (NIDS.EQ.3) THEN
	  CALL XVREAD( IUN(3), OTF, I,' ')
	  IF (L.EQ.1) outnor = REAL(OTF(1))
	  DO I=1,NS
	    RESTOR(I)=RESTOR(I)*OTF(I)/outnor
	  ENDDO
          if(L.eq.1)restor(1)=cmplx(psfnor,0.0)
	ENDIF

	CALL XVWRIT( OUN, RESTOR, I,' ')
      ENDDO

      RETURN
      END
