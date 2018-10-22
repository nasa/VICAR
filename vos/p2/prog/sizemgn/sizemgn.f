      INCLUDE 'VICMAIN_FOR'
C Definition of global constants:
C ICODE,OCODE are the input and output data format codes
C   =1 for byte, =2 for 16-bit integer, =3 for 32-bit integer,
C   =4 for REAL*4
C
C GSCALE optionally used to scale the output (see SCALE parameter).
C		OUTDN=OUTDN*GSCALEC
C
C ILO,IHI = low and high limits of the output DN (see LIMITS parameter).
C              ILO .LE. out-DN .LE. IHI
C LFLAG=1 if a check for saturation is necessary (ILO,IHI)
C
C           SIZEMGN  IN  OUT  (1,1,NLO,NSO)  user-parameters...
C       or  SIZEMGN  IN  OUT  ZOOM=2.5  user-parameters...
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,OUNIT	!Input and output logical unit numbers
      INTEGER*4 ICODE,OCODE	!Input and ouput image format
      INTEGER*4 SLI,SSI,NLI,NSI	!Input image area (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
	INTEGER*4 IB,SB,EB        !variables for 3rd dimension
      REAL*4 ZOOML,ZOOMS	!Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM	!Integer zoom factors (=0 if not integer)
      INTEGER*4 ILO,IHI		!Min and max DN limits for output image
      INTEGER*4 LFLAG		!=1 if need to check for ILO,IHI saturation
      REAL*4 GSCALE		!Output DN scale factor

      COMMON/C1/TBL(0:255),TBLH(-32768:32767),TABLE(0:255)
      LOGICAL*1 TBL
      INTEGER*2 TBLH
      REAL*4 TABLE		!Scale DN to Volts (Magellan only)

c      COMMON/C2/RBUF(100000),BUF(100000,2),OBUF(100000)
c      COMMON/C2/SAMP(100000),WGHT(100000)
      INTEGER*4 SAMP(100000)
      REAL*4 BUF(100000),RBUF(100000),OBUF(100000),WGHT(100000)

      INTEGER*4 I,L,N,IND,INCODE
      REAL*4 SLOPE,OFFSET
      LOGICAL*4 XVPTST,INTERP
      CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
  
      CALL XVMESSAGE(' SIZEMGN version 06 Jun 2012',' ')
C     ....Open input picture
      CALL IPOPEN(iunit,icode,sb,eb)
C     ....Determine zoom factor and size of input and output images
      CALL GETSIZE(IUNIT,interp,sli,ssi,nli,nsi,slo,sso,nlo,nso,
     &		nlout,nsout,zooml,zooms,lzoom,izoom)
C     ....Open output picture
      CALL OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLOUT,NSOUT,
     &	sb,eb,ICODE,ocode,ounit)
C     ....Get scale and limits parameters
      CALL GETSCALE(ICODE,OCODE,gscale,ilo,ihi,lflag)
C     ....If format conversion is required, reopen input
      INCODE = MAX0(ICODE,OCODE)
      IF (INTERP) INCODE=4
c       force all input to be converted to real on the fly
         CALL XVCLOSE(IUNIT,IND,' ')
         CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &          'I_FORMAT',FMT(ICODE),'U_FORMAT',FMT(4),' ')            !FMT(INCODE),' ')

      CALL UPDATE_LABEL(IUNIT,OUNIT,SLI,SSI,SLO,SSO,ZOOML,ZOOMS)
C     ... start loop over bands
      DO 100 IB=SB,EB

C     ....If there is an offset, write top lines before offset
      DO I=1,NSOUT
         OBUF(I) = 0.
      ENDDO
      IF (ICODE.LT.4) CALL ZIA(OBUF,NSOUT)
      DO L=1,SLO-1
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',L,'BAND',IB,' ')
      ENDDO 

C     ....Perform the magnification or reduction
	print *,'before lookup'
      IF (XVPTST('LOOKUP')) THEN
         CALL LOOKUP(INTERP,nsi,nso,buf,rbuf)		!Table look-up
      ELSE IF (XVPTST('VOLTS')) THEN		!Do calculations in volts
         CALL DTVTABLE(slope,offset,table)	!Compute DN-to-volts table
         CALL SMGN(INTERP,TABLE,SLOPE,OFFSET,NSI,NSO,
     &		buf,rbuf)
      ELSE IF (INTERP) THEN
         CALL SINTRP(ib,NSO,buf,rbuf,obuf,samp,wght) !Interpolation
      ELSE
         CALL SNOIN(ib,nso,buf,rbuf,obuf,samp)		!No interpolation
      ENDIF

C     ....If output size is larger than sized input, write remaining lines
      N = NLOUT - (SLO+NLO-1)
      IF (N.LE.0) GOTO 100
      DO I=1,NSOUT
         OBUF(I) = 0.
      ENDDO
      IF (OCODE.LT.4) CALL ZIA(OBUF,NSOUT)
      DO L=1,N
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+NLO+L-1,'BAND',IB,' ')
      ENDDO 

  100 CALL XVMESSAGE(' SIZEMGN task completed',' ')
      RETURN

      CALL XVMESSAGE('??E - SIZEMGN task cancelled',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input image and determine data format
C
      SUBROUTINE IPOPEN(iunit,icode,sb,eb)
      IMPLICIT NONE
      INTEGER*4 IUNIT
      INTEGER*4 ICODE		!1=byte, 2=half, 3=full, 4=real

      INTEGER*4 IND,SB,EB,NB,NBI
      CHARACTER*5 FORMAT
	CHARACTER*8 ORG
      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVSIGNAL(IUNIT,IND,.TRUE.)
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT,ind,'FORMAT',format,'ORG',org,' ')	!Determine data format
      call xvbands( sb, nb, nbi)
      ! nbi is from input label
      ! nb is from param NB or BANDS, whichever is non-zero;  else zero
      if (nbi.gt.1) then
        if (org.eq.'BIP') then
          CALL XVMESSAGE(
     &  '??E - BIP files not supported, use program TRAN to convert to BSQ',
     &    ' ')
          call abend
        endif
        if (org.eq.'BIL') call xvmessage(
     &   '??W - BIL format may cause performance degradation',' ')
      endif
      if ((sb+nb-1).gt.nbi) then
        call xvmessage(' ??W - NB too large, reduced to fit input',' ')
        nb = nbi-sb+1
      endif
      if (nb.le.0) nb = nbi-sb+1
      eb = sb+nb-1

      ICODE = 0
      IF (FORMAT.EQ.'BYTE') ICODE=1
      IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') ICODE=2
      IF (FORMAT.EQ.'FULL') ICODE=3
      IF (FORMAT.EQ.'REAL') ICODE=4
      IF (ICODE.NE.0) RETURN
      CALL XVMESSAGE('??E - Unknown data format for input image',' ')
	call abend
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine output data format, open output file, and put zoom
C factors in label. 
C
C For the no-interpolation option, mixed data formats (e.g. byte input
C and halfword output) are handled by using VICAR I/O to convert them
C to the same format as follows:
C    If ICODE.LT.OCODE, then the input is converted to the output format
C       by XVREAD.  The input file is closed and re-opened to set this
C       option, and ICODE is set equal to OCODE.
C    If ICODE.GT.OCODE, then the input is converted to the output format
C       by XVWRITE.
C
C Outputs:  OCODE=output image data format code
C           OUNIT=output image logical unit number
C           MGN=1 if magellan DN scaling is requested
C           ICODE=input data format (may be modified due to conversion)
C
      SUBROUTINE OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLOUT,NSOUT,
     &	sb,eb,ICODE,ocode,ounit)
      IMPLICIT NONE
      LOGICAL*4 INTERP
      REAL*4 ZOOML,ZOOMS
      INTEGER*4 LZOOM,IZOOM,NLOUT,NSOUT,ICODE,OCODE,OUNIT

      INTEGER*4 I,IND,NCHAR,OUTCODE,sb,eb,nb
      REAL*4 F1,F2
      LOGICAL*4 XVPTST
      CHARACTER*72 LAB
      CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
      CHARACTER*80 MSG
      byte byt(80)
      DATA LAB /' '/                    !initialize character buffer
C  100 FORMAT(' Input data format=',A4,'  Output data format=',A4)
  102 FORMAT(' PICTURE SIZE SCALED BY ',F12.5,'*NL, ',F12.5,'*NS')
  103 FORMAT(' PICTURE SIZE SCALED BY ',I6,'*NL, ',I6,'*NS')

C     ....Determine output picture format
      OCODE = ICODE			!Default is same format as input
      IF (XVPTST('BYTE')) OCODE=1
      IF (XVPTST('HALF')) OCODE=2
      IF (XVPTST('FULL')) OCODE=3
      IF (XVPTST('REAL')) OCODE=4
c      WRITE (MSG,100) FMT(ICODE),FMT(OCODE)
c      CALL XVMESSAGE(MSG,' ')

      OUTCODE = MAX0(ICODE,OCODE)
      IF (INTERP) THEN
         OUTCODE = 3
         IF (OCODE.EQ.4) OUTCODE=4
      ENDIF
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUNIT,IND,.TRUE.)
      nb = eb-sb+1
c Feb 5, 2010 - internal format is real - convert to ouput of ocoide
      CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',NLOUT,'U_NS',NSOUT,
     & 'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',FMT(OCODE),
     & 'U_FORMAT',FMT(4),' ')                           !,FMT(OUTCODE),' ')

      IF (IZOOM.EQ.0.OR.LZOOM.EQ.0) THEN	!Print real zoom factor
         WRITE(MSG,102) ZOOML,ZOOMS
         NCHAR = 36
         IF (ZOOML.NE.ZOOMS) NCHAR=56
         CALL XVMESSAGE(MSG,' ')
      elseif (izoom.lt.0 .and. lzoom.lt.0) then
	  ! (don't worry about case where one is <0 and other >0)
	 f1 = -1.0/float(lzoom)
	 f2 = -1.0/float(izoom)
         write(msg,102) f1, f2
         nchar = 36
         if (zooml.ne.zooms) nchar=56
         call xvmessage(msg,' ')
      ELSE
         WRITE(MSG,103) LZOOM,IZOOM
         NCHAR = 30
         IF (LZOOM.NE.IZOOM) NCHAR=44
         CALL XVMESSAGE(MSG,' ')
      ENDIF

      do i=2,nchar
           byt(i-1) = ichar(msg(i:i))
      end do

      CALL MVLC(byt,LAB,NCHAR-1)

      CALL XLADD(OUNIT,'HISTORY','COMMENT',LAB,IND,
     &            'FORMAT','STRING','ULEN',NCHAR-1,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get optional scaling and DN limits of output image.
C Outputs: GSCALE=optional scaling of output DN (ODN = GSCALE*ODN).
C          ILO,IHI=low and high limits of output DN values
C          LFLAG=1 if a check for saturation (ILO,IHI) is required.
C
      SUBROUTINE GETSCALE(ICODE,OCODE,gscale,ilo,ihi,lflag)
	IMPLICIT NONE
      INTEGER*4 ICODE,OCODE
	REAL*4 GSCALE
	INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,
      INTEGER*4 INUM,IDEF,PAR(2)
c      REAL*4 EPS
      INTEGER*4 LOLIM(3)
      INTEGER*4 HILIM(3)
c      DATA EPS/1.E-6/
      DATA LOLIM(1)/0/,LOLIM(2)/-32768/,LOLIM(3)/-2147483648/
      DATA HILIM(1)/255/,HILIM(2)/32767/,HILIM(3)/2147483647/

C     ....Determine max and min limits of output DN
      IF (OCODE.LT.4) THEN
         ILO = LOLIM(OCODE)
         IHI = HILIM(OCODE)
      ENDIF

      IF (ICODE.GT.OCODE) THEN	!If the output DN range is smaller than
         LFLAG = 1		!the input range, check for saturation.
      ELSE				
         LFLAG = 0		!Else, no check is necessary.
      ENDIF

      CALL XVP('LIMITS',PAR,INUM)
      IF (INUM.EQ.2) THEN 
          ILO = MAX0(PAR(1),ILO)
          IHI = MIN0(PAR(2),IHI)
          LFLAG = 1
      ENDIF

      CALL XVPARM('SCALE',GSCALE,INUM,IDEF,1)  !Optional scaling of output DNs
      IF (IDEF.NE.1) THEN 
          IF (GSCALE.EQ.0.) GOTO 990
          LFLAG = 1
      ENDIF
      RETURN

  990 CALL XVMESSAGE('??E - Zero is an invalid SCALE value',' ')
      call abend
      END
