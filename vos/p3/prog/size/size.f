	INCLUDE 'VICMAIN_FOR'
C Definition of global constants:
C ICODE,OCODE are the input and output data format codes
C   =1 for byte, =2 for 16-bit integer, =3 for 32-bit integer,
C   =4 for REAL*4
C
C INPUT AREA=(SLI,SSI,NLI,NSI)
C OUTPUT AREA=(1,1,NLO,NSO)
C
C ZOOML,ZOOMS are the floating point zoom factors.
C LZOOM,IZOOM are the integral zoom factors
C       =0 if zoom factor is not an integer
C
C GSCALE optionally used to scale the output (see SCALE parameter).
C		OUTDN=OUTDN*GSCALEC
C
C ILO,IHI = low and high limits of the output DN (see LIMITS parameter).
C              ILO .LE. out-DN .LE. IHI
C LFLAG=1 if a check for saturation is necessary (ILO,IHI)
C
C           SIZE  IN  OUT  (1,1,NLO,NSO)  user-parameters...
C       or  SIZE  IN  OUT  ZOOM=2.5  user-parameters...
C
      SUBROUTINE MAIN44
      PARAMETER (MAXSIZE=70000)
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI,OUNIT,OCODE,NLO,NSO,
     &		ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI,SSI,OUNIT,OCODE

      COMMON/C1YY/TBL,TBLH
      LOGICAL*1 TBL(0:255)
      INTEGER*2 TBLH(-32768:32767)
C
      INTEGER*2 SAMP(MAXSIZE)
      INTEGER*4 BUF(MAXSIZE)
      REAL*4 RBUF(MAXSIZE,2),WGHT(MAXSIZE)
C
      LOGICAL XVPTST,INTERP
C
      CALL XVMESSAGE(' SIZE version 7.29.02',' ')
C     ....Open input picture
      CALL IPOPEN(IUNIT,ICODE,SLI,SSI,NLI,NSI,NLO,NSO,*999)
C     ....Determine if pixel interpolation is required
      INTERP = .NOT.XVPTST('NOIN')
C     ....Get zoom parameters
      CALL GETZOOM(LZFLG,IZFLG,ZOOML,ZOOMS,LZOOM,IZOOM,*999)
C     ....Determine zoom factor and size of output image
      CALL GETSIZE(NLI,NSI,LZFLG,IZFLG,INTERP,
     &		ZOOML,ZOOMS,LZOOM,IZOOM,NLO,NSO,MAXSIZE,*999)
      IF (LZOOM.EQ.1.AND.IZOOM.EQ.1) INTERP=.FALSE.
C     ....Open output picture
      CALL OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLO,NSO,IUNIT,
     &		ICODE,OUNIT,OCODE)
C     ....Get scale and limits parameters
      CALL GETSCALE(ICODE,OCODE,GSCALE,ILO,IHI,LFLAG)
C
C     ....Perform the magnification or reduction
            IF (INTERP) THEN
         CALL SINTRP(NSI,NSO,LZOOM,IZOOM,buf,rbuf,samp,wght) !Interpolation
      ELSE
         CALL SNOIN(NSI,NSO,buf,rbuf,samp)		!No interpolation
      ENDIF
      CALL XVMESSAGE(' SIZE task completed',' ')
      RETURN
C
  999 CONTINUE
      CALL XVMESSAGE(' ***SIZE task cancelled',' ')
      CALL ABEND
      RETURN
      END
C******************************************************************************
C Open input image, determine data format, and requested image area
C
      SUBROUTINE IPOPEN(IUNIT,ICODE,SLI,SSI,NLI,NSI,NLO,NSO,*)
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 PAR(4)
      CHARACTER*5 FORMAT
      CHARACTER*50 MSG
C
      CALL XVUNIT(IUNIT,'INP',1,IND,' ')
      CALL XVSIGNAL(IUNIT,IND,.TRUE.)
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
C     ....Determine data format of input image
      CALL XVGET(IMG,IND,'FORMAT',FORMAT,' ')
      ICODE = 0
      IF (FORMAT.EQ.'BYTE') ICODE=1
      IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') ICODE=2
      IF (FORMAT.EQ.'FULL') ICODE=3
      IF (FORMAT.EQ.'REAL') ICODE=4
C
      CALL XVSIZE(SLI,SSI,NLO,NSO,NLI,NSI)     ! SIZE PARAMETERS.
      CALL XVPARM('AREA',PAR,INUM,IDEF,0)
      IF (IDEF.EQ.0) THEN 
         SLI = PAR(1)
         SSI = PAR(2)
         NLIN = PAR(3)
         NSIN = PAR(4)
	 WRITE (MSG,100) SLI,SSI,NLIN,NSIN
  100	 FORMAT('      INPUT AREA=(',I5,',',I5,',',I5,',',I5,')')
	 CALL XVMESSAGE(MSG,' ')
      ELSE
         NLIN = 0
         NSIN = 0
      ENDIF
C
      NLI = NLI - SLI + 1
      NSI = NSI - SSI + 1
      IF (NLIN.EQ.0) NLIN=NLI
      IF (NSIN.EQ.0) NSIN=NSI
C
C     ....Check if requested area lies within picture boundary
      IF (SLI.LT.1.OR.SSI.LT.1) GOTO 980
      IF (NLIN.LT.1.OR.NSIN.LT.1) GOTO 980
      IF (NLIN.GT.NLI.OR.NSIN.GT.NSI) GOTO 980
      NLI = NLIN
      NSI = NSIN
      RETURN
C
  980 CONTINUE
      CALL XVMESSAGE(' ***Invalid input picture area',' ')
      RETURN1
      END
C******************************************************************************
C Check for ZOOM, LZOOM, or SZOOM parameters
C Outputs: ZOOML,ZOOMS = floating pt zoom factors
C          LZOOM,IZOOM = integral zoom factors (=0 if not an integer)
C          LZFLG,IZFLG = 1 if zoom factors are specified, =0 otherwise
C
      SUBROUTINE GETZOOM(LZFLG,IZFLG,ZOOML,ZOOMS,LZOOM,IZOOM,*)
C
      IZFLG = 0
      LZFLG = 0
C
      CALL XVPARM('ZOOM',R,INUM,IDEF,0)
      IF (IDEF.EQ.0) THEN 
          IF (R.EQ.0.0) GOTO 998
          CALL QPAR(N,R)		!Check for integral zoom
          ZOOML = R
          ZOOMS = R
          LZOOM = N
          IZOOM = N
          IF (IZOOM.EQ.-1) IZOOM=1
          IF (LZOOM.EQ.-1) LZOOM=1
          IZFLG = 1
          LZFLG = 1
      ENDIF
C
      CALL XVPARM('LZOOM',R,INUM,IDEF,0)
      IF (IDEF.EQ.0) THEN 
          IF (R.EQ.0.0) GOTO 999
          CALL QPAR(N,R)
          ZOOML = R
          LZOOM = N
          IF (LZOOM.EQ.-1) LZOOM=1
          LZFLG = 1
      ENDIF
C
      CALL XVPARM('SZOOM',R,INUM,IDEF,0)
      IF (IDEF.EQ.0) THEN 
          IF (R.EQ.0.0) GOTO 999
          CALL QPAR(N,R)
          ZOOMS = R
          IZOOM = N
          IF (IZOOM.EQ.-1) IZOOM=1
          IZFLG = 1
      ENDIF
C
      RETURN
C
  998 CALL XVMESSAGE(' ***ZOOM cannot be zero',' ')
      RETURN1
  999 CALL XVMESSAGE(' ***LZOOM and SZOOM cannot be zero',' ')
      RETURN1
      END
C******************************************************************************
C Determine size of output picture.
C Inputs:  NLI,NSI
C          LZFLG,IZFLG=1 if zoom factor is specified, =0 otherwise
C          INTERP=.TRUE. if interpolation is specified
C Outputs: NLO,NSO
C	   ZOOML,ZOOMS
C          LZOOM,IZOOM
C
      SUBROUTINE GETSIZE(NLI,NSI,LZFLG,IZFLG,INTERP,ZOOML,ZOOMS,LZOOM,
     +		         IZOOM,NLO,NSO,MAXSIZE,*)
C
      REAL*4 EPS/1.E-6/
      LOGICAL INTERP
      CHARACTER*80 MSG
C
C    ....Check horizontal zoom
      IF (IZFLG.EQ.0) THEN	!If horizontal zoom is not specified,
	 ZOOMS = (1.0*NSO)/NSI  !compute it from size field.
         IF (NSO.LT.NSI) THEN
	    N = NSI/NSO
            S = 1./ZOOMS - N
            IF (ABS(S).LT.1.E-6) IZOOM=-N
	 ELSE
            N = NSO/NSI
            S = ZOOMS - N
            IF (ABS(S).LT.1.E-6) IZOOM=N
	 ENDIF
         GOTO 40
      ENDIF
C
      IF (IZOOM.NE.0) THEN	!If an integral zoom is specified,
         IF (IZOOM.LT.0) THEN	!and it is negative, then
            IF (INTERP) THEN
	       NSO = -NSI/IZOOM !output is smaller than input.
            ELSE
               NSO = -(NSI-1)/IZOOM + 1
            ENDIF
	 ELSE			!If it is positive, then
   	    NSO = NSI*IZOOM	!output is bigger than input.
	 ENDIF
      ELSE			!Floating point zoom is specified.
         IF (.NOT.INTERP.AND.ZOOMS.LT.1.0) THEN
            NSO = (NSI-1)*ZOOMS + 1.0 + EPS
         ELSE
	    NSO = ZOOMS*NSI + EPS
         ENDIF
      ENDIF

      IF (NSO.LE.0) THEN	!If NSO=0,
         NSO = 1		!set NSO=1
         IF (NSI.GT.1) THEN	!and adjust zoom factors
            IZOOM = -NSI
            ZOOMS = 1.0/NSI
         ELSE
            IZOOM = 1
            ZOOMS = 1.
	 ENDIF
         CALL XVMESSAGE(' ***SZOOM adjusted to sample size',' ')
      ENDIF
C
C    ....Check vertical zoom
   40 IF (LZFLG.EQ.0) THEN	!If vertical zoom is not specified,
         ZOOML = (1.*NLO)/NLI	!compute it frome size field.
         IF (NLO.LT.NLI) THEN
            N = NLI/NLO
            S = 1.0/ZOOML - N
            IF (ABS(S).LT.1.E-6) LZOOM=-N
	 ELSE
            N = NLO/NLI
            S = ZOOML - N
            IF (ABS(S).LT.1.E-6) LZOOM=N
	 ENDIF
         GOTO 50
      ENDIF
C
      IF (LZOOM.NE.0) THEN	!If an integral zoom is specified,
         IF (LZOOM.LT.0) THEN	!and it is negative, then
            IF (INTERP) THEN
	       NLO = -NLI/LZOOM	!output is smaller than input.
            ELSE
               NLO = -(NLI-1)/LZOOM + 1
            ENDIF
	 ELSE			!If it is positive, then
            NLO = NLI*LZOOM	!output is bigger than input
  	 ENDIF
      ELSE			!Floating point zoom is specified.
         IF (.NOT.INTERP.AND.ZOOML.LT.1.0) THEN
            NLO = (NLI-1)*ZOOMS + 1.0 + EPS
         ELSE
	    NLO = ZOOML*NLI + EPS
         ENDIF
      ENDIF
C
      IF (NLO.LE.0) THEN	!If NLO=0
         NLO = 1		!set NLO=1
         IF (NLI.GT.1) THEN	!and adjust zoom factors
            LZOOM = -NLI
            ZOOML = 1.0/NLI
         ELSE
            LZOOM = 1
            ZOOML = 1.0
         ENDIF
         CALL XVMESSAGE(' ***LZOOM adjusted to line size',' ')
      ENDIF
C
   50 CONTINUE
      IF (NSI .GT. MAXSIZE) THEN
         WRITE (MSG,70) NSI,MAXSIZE
   70    FORMAT(' Input size of',I7,' samples exceeds the maximum of',
     +		I7)
         CALL XVMESSAGE(MSG,' ')
	 CALL ABEND
      END IF
      IF (NSO .GT. MAXSIZE) THEN
         WRITE (MSG,80) NSO,MAXSIZE
   80    FORMAT(' Output size of',I7,' samples exceeds the maximum of',
     +          I7)
         CALL XVMESSAGE(MSG,' ')
	 CALL ABEND
      END IF
      WRITE (MSG,100) NLO,NSO
  100 FORMAT('     OUTPUT SIZE =',I5,' X',I6)
      CALL XVMESSAGE(MSG,' ')
      RETURN
      END
C******************************************************************************
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
C           ICODE=input data format (may be modified due to conversion)
C
      SUBROUTINE OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLO,NSO,IUNIT,
     &		ICODE,OUNIT,OCODE)
C
      INTEGER*4 OUNIT,OCODE
      LOGICAL INTERP
      LOGICAL XVPTST
      CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
      CHARACTER*80 MSG
C
C     ....Determine output picture format
      OCODE = ICODE			!Default is same format as input
      IF (XVPTST('BYTE')) OCODE=1
      IF (XVPTST('HALF')) OCODE=2
      IF (XVPTST('FULL')) OCODE=3
      IF (XVPTST('REAL')) OCODE=4
      WRITE (MSG,100) FMT(ICODE),FMT(OCODE)
  100 FORMAT(' Input data format=',A4,'  Output data format=',A4)
      CALL XVMESSAGE(MSG,' ')
C
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUNIT,IND,.TRUE.)
      IF (.NOT.INTERP.AND.ICODE.GT.OCODE) THEN
         CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',NLO,'U_NS',NSO,
     &             'OPEN_ACT','SA','IO_ACT','SA',
     &             'O_FORMAT',FMT(OCODE),'U_FORMAT',FMT(ICODE),' ')
      ELSE
         CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',NLO,'U_NS',NSO,
     &             'OPEN_ACT','SA','IO_ACT','SA',
     &             'O_FORMAT',FMT(OCODE),'U_FORMAT',FMT(OCODE),' ')
      ENDIF
C
      IF (IZOOM.EQ.0.OR.LZOOM.EQ.0) THEN	!Print real zoom factor
	 IF (ZOOML.EQ.ZOOMS) THEN
	    WRITE (MSG,101) ZOOML
  101	    FORMAT(' PICTURE SIZE SCALED BY ',F12.5)
	 ELSE
	    WRITE(MSG,102) ZOOML,ZOOMS
  102	    FORMAT(' PICTURE SIZE SCALED BY ',F12.5,'*NL, ',F12.5'*NS')
	 END IF
	 CALL XVMESSAGE(MSG,' ')
      ELSE
	 IF (LZOOM.EQ.IZOOM) THEN
	    WRITE(MSG,103) LZOOM
  103	    FORMAT(' PICTURE SIZE SCALED BY ',I6)
	 ELSE
	     WRITE(MSG,104) LZOOM,IZOOM
  104	     FORMAT(' PICTURE SIZE SCALED BY ',I6,'*NL, ',I6,'*NS')
	 END IF
	 CALL XVMESSAGE(MSG,' ')
      ENDIF
      CALL XLADD(OUNIT,'HISTORY','COMMENT',MSG,IND,'FORMAT','STRING',
     +		 ' ')
C
C     ....If format conversion is required, reopen input
      IF (.NOT.INTERP.AND.ICODE.LT.OCODE) THEN
         CALL XVCLOSE(IUNIT,IND,' ')
         CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',FMT(ICODE),'U_FORMAT',FMT(OCODE),' ')
         ICODE = OCODE
      ENDIF
      RETURN
      END
C******************************************************************************
C Get optional scaling and DN limits of output image.
C Outputs: GSCALE=optional scaling of output DN (ODN = GSCALE*ODN).
C          ILO,IHI=low and high limits of output DN values
C                  i.e.   ILO .LE. output-DN .LE. IHI
C          LFLAG=1 if a check for saturation (ILO,IHI) is required.
C
      SUBROUTINE GETSCALE(ICODE,OCODE,GSCALE,ILO,IHI,LFLAG,*)
C
      INTEGER*4 OCODE
      INTEGER*4 LOLIM(3)/0,-32768,-2147483648/
      INTEGER*4 HILIM(3)/255,32767,2147483647/
      INTEGER PAR(2)
C
C     ....Determine max and min limits of output DN
      IF (OCODE.LT.4) THEN
         ILO = LOLIM(OCODE)
         IHI = HILIM(OCODE)
      ENDIF
C
      IF (ICODE.GT.OCODE) THEN	!If the output DN range is smaller than
         LFLAG = 1		!the input range, check for saturation.
      ELSE				
         LFLAG = 0		!Else, no check is necessary.
      ENDIF
C
      CALL XVPARM('LIMITS',PAR,INUM,IDEF,0)
      IF (IDEF.EQ.0) THEN 
          ILO = MAX0(PAR(1),ILO)
          IHI = MIN0(PAR(2),IHI)
          LFLAG = 1
      ENDIF
C
      CALL XVPARM('SCALE',GSCALE,ICNT,IDEF,0)  !Optional scaling of output DNs
      IF (IDEF.EQ.0) THEN 
          IF (GSCALE.EQ.0.) GOTO 990
          LFLAG = 1
      ENDIF
      RETURN
C
  990 CALL XVMESSAGE(' ***Zero is an invalid SCALE value',' ')
      RETURN1
      END
C******************************************************************************
C Check for integer or real zoom factor.   An integer value is returned
C if possible.
C
C Input: R=real zoom factor specified via ZOOM=R parameter.
C Outputs:  N=integer zoom factor.  This has same meaning as in VIDS.
C            =0 if zoom factor is not an integer.
C If R is negative, it is converted to a fraction.
C E.g. if R=-2, then R=0.5 on output.
C
      SUBROUTINE QPAR(N,R)
      REAL R, S
      REAL*4 EPS/1.E-6/
C
      N = 0
      IF (R.EQ.0.) RETURN
C
      IF (R.GE.1.0) GOTO 20
C     ....Here for compression.  Compute shrink factor S.
      IF (R.GT.0.) THEN
         S = 1./R
      ELSE
         S = -R
         R = 1./S		!Convert negative zoom to fraction
      ENDIF
C
      N = S + EPS		!Check for integer shrink factor
      S = S - N
      IF (ABS(S).LE.EPS) THEN
         N = -N
      ELSE
         N = 0
      ENDIF
      RETURN
C
C     ....Here for magnification (R.GT.1.0)
   20 N = R + EPS		!Check for integer zoom factor
      S = R - N
      IF (ABS(S).LE.EPS) THEN
         R = N
      ELSE
         N = 0
      ENDIF
C
      RETURN
      END
C******************************************************************************
C Magnify or reduce an image via interpolation.
C
      SUBROUTINE SINTRP(NSI,NSO,LZOOM,IZOOM,BUF,RBUF,SAMP,WGHT)
      REAL*4 RBUF(NSO,2),WGHT(NSO)
      INTEGER*2 SAMP(NSO)
      INTEGER*4 BUF(NSI)
C
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSIX,OUNIT,OCODE,NLO,NSOX,
     &		ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI,SSI,OUNIT,OCODE
C
      RLO = ILO
      RHI = IHI
      R = 1./ZOOMS
C
C     ....Set up correspondence between input and output samples
      IF (ZOOMS.GT.1.0) THEN	!Magnify in horizontal direction
         SCALE = 1.0 
         DO I=1,NSO
            X0 = R*(I-.5) + .5  !Translate center of pixel
            I1 = X0		!I1 = left neighbor
            I1 = MAX0(I1,1)
            I2 = MIN0(I1+1,NSI) !I2 = right neighbor
            WGHT(I) =  I2 - X0  !Store weight A
            SAMP(I) = I2	!Store index of right neighbor
         ENDDO
      ELSE			!Compress in horizontal direction
         SCALE = ZOOMS
         DO I=1,NSO
            X = R*I		!Translate right margin of pixel
            I1 = X 		!I1 = left neighbor
            I2 = MIN0(I1+1,NSI) !I2 = right neighbor
            WGHT(I) = 1.0 - (I2-X) !Compute weight 1.0-A
            SAMP(I) = I2	!Store index of right neighbor
         ENDDO
      ENDIF
C
      SCALE = SCALE*GSCALE
C
      IF (NLI.EQ.1) THEN		!Special case: NLI=1
         CALL SREAD(ICODE,SLI,SSI,NSI,NSO,IZOOM,ZOOMS,
     &		SAMP,WGHT,BUF,RBUF)
         CALL IOUT(RBUF,BUF,BUF,SCALE,OCODE,RHI,RLO,NSO,BUF,BUF)
         DO L=1,NLO
            CALL XVWRIT(OUNIT,BUF,IND,' ')
         ENDDO
         RETURN
      ENDIF
C
      IF (LZOOM.EQ.1) THEN		!Special case: LZOOM=1
         J = SLI - 1
         DO L=1,NLO
            CALL SREAD(ICODE,J+L,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,BUF,RBUF)
            CALL IOUT(RBUF,BUF,BUF,SCALE,OCODE,RHI,RLO,NSO,BUF,BUF)
            CALL XVWRIT(OUNIT,BUF,IND,' ')
         ENDDO
         RETURN
      ENDIF
C
      IF (ZOOML.GT.1.) THEN	!If vertical zoom .gt. 1, magnify it.
         CALL MAGNIFY(SCALE,IZOOM,NSI,NSO,SAMP,WGHT,BUF,RBUF)
      ELSE 			!Else, compress it.
         CALL COMPRESS(SCALE,LZOOM,IZOOM,NSI,NSO,SAMP,WGHT,BUF,RBUF)
      ENDIF
      RETURN
      END
C******************************************************************************
C Magnify an image.  The vertical direction is magnified.  The
C horizontal dimension may be magnified or compressed.
C Input and output images may be byte, halfword, fullword, or real*4.
C Mixed data types (e.g. byte input, halfword output) are permitted.
C
      SUBROUTINE MAGNIFY(SCALE,IZOOM,NSI,NSO,SAMP,WGHT,BUF,RBUF)
      INTEGER*2 SAMP(NSO)
      INTEGER*4 BUF(NSI)
      REAL*4 RBUF(NSO,2),WGHT(NSO)
C
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSIX,OUNIT,OCODE,NLO,NSOX,
     &		ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI,SSI,OUNIT,OCODE
      INTEGER*4 ELI
C
      RLO = ILO
      RHI = IHI
      R = 1.0/ZOOML
      REC = SLI - .5
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      JSAVE = 0
      J2 = 0
C
      DO L=1,NLO
        Y0 = R*(L-.5) + REC
        J1 = Y0
        J1 = MAX0(J1,SLI)
        J1 = MIN0(J1,ELI-1)
        IF (J1.NE.JSAVE) THEN
          JSAVE = J1
          ITEMP = I1
          I1 = I2
          I2 = ITEMP
          IF (J1.GT.J2) CALL SREAD(ICODE,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &  			SAMP,WGHT,BUF,RBUF(1,I1))
          J2 = J1 + 1
          CALL SREAD(ICODE,J2,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,BUF,RBUF(1,I2))
        ENDIF
        D = Y0 - J1		!Compute vertical weights
        C = (1.-D)*SCALE
        D = D*SCALE
        CALL INTRPV(RBUF(1,I1),RBUF(1,I2),BUF,BUF,
     &		    C,D,OCODE,NSO,RHI,RLO,BUF,BUF)
	CALL XVWRIT(OUNIT,BUF,IND,' ')
      END DO
C
      RETURN
      END
C******************************************************************************
C Compress an image.  The vertical direction is compressed.  The
C horizontal direction may be magnified or compressed.
C Input and output images may be byte, halfword, fullword, and real*4.
C Mixed data types (e.g. byte input, halfword output) are permitted.
C
      SUBROUTINE COMPRESS(SCALE,LZOOM,IZOOM,NSI,NSO,SAMP,WGHT,BUF,RBUF)
      INTEGER*2 SAMP(NSO)
      INTEGER*4 BUF(NSI)
      REAL*4 RBUF(NSO,2),WGHT(NSO)
C
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSIX,OUNIT,OCODE,NLO,NSOX,
     &		ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI,SSI,OUNIT,OCODE
C
      INTEGER*4 ELI
C
      RLO = ILO
      RHI = IHI
      R = 1.0/ZOOML
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      IF (LZOOM.LT.0) GOTO 40
C
C     ....Here to compress an image using a real zoom in the vertical
C     ....direction.  The horizontal zoom may be real or integral valued.
      J1 = SLI
      C = ZOOML*SCALE
      D = 0.
      CALL SREAD(ICODE,J1,SSI,NSI,NSO,IZOOM,ZOOMS,SAMP,WGHT,
     +		 BUF,RBUF(1,I2))
C
      DO L=1,NLO
         CALL SMUL(NSO,RBUF(1,I2),RBUF,1.-D)
         Y2 = L*R + SLI
         J2 = Y2
         J2 = MIN0(J2,ELI)
         D = Y2 - J2
         J1 = MIN0(J1+1,ELI)
         DO WHILE (J1.NE.J2)
            CALL SREAD(ICODE,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,BUF,RBUF(1,I2))
            CALL ADD(4,7,NSO,RBUF(1,I2),RBUF)
            J1 = MIN0(J1+1,ELI)
	 ENDDO
         CALL SREAD(ICODE,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,BUF,RBUF(1,I2))
         CALL INTRPV(RBUF,RBUF(1,I2),BUF,BUF,
     &		C,D*C,OCODE,NSO,RHI,RLO,BUF,BUF)
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN
C
C     ....Here to compress vertical scale by an integral lzoom
   40 INC = -LZOOM
      N = INC - 1
      LL = SLI
      C = ZOOML*SCALE
C
      DO L=1,NLO
         CALL SREAD(ICODE,LL,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,BUF,RBUF(1,I2))
         DO J=1,N
            CALL SREAD(ICODE,LL+J,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,BUF,RBUF)
            CALL ADD(4,7,NSO,RBUF,RBUF(1,I2))
         ENDDO
         LL = LL + INC
         CALL IOUT(RBUF(1,I2),BUF,BUF,C,OCODE,RHI,RLO,NSO,BUF,BUF)
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN
      END
C******************************************************************************
C Read an image line and magnify or reduce is via interpolation.
C The input line (BUF) may be byte, halfword, fullword, or real*4.
C The output line (RBUF) is REAL*4.
C
      SUBROUTINE SREAD(ICODE,LINE,SSI,NSI,NSO,IZOOM,ZOOMS,
     &		SAMP,WGHT,BUF,RBUF)
      REAL*4 WGHT(NSO),RBUF(NSO)
      INTEGER*2 SAMP(NSO)
      INTEGER*4 BUF(NSI),SSI
      INTEGER*4 MCODE(4)/17,27,47,7/
C
      IF (IZOOM.EQ.1.AND.ICODE.EQ.4) THEN
         CALL XVREAD(IUNIT,RBUF,IND,'LINE',LINE,'SAMP',SSI,'NSAMPS',NSI,
     +		     ' ')
      ELSE
         CALL XVREAD(IUNIT,BUF,IND,'LINE',LINE,'SAMP',SSI,'NSAMPS',NSI,
     +		     ' ')
      ENDIF
C
      IF (NSI.EQ.1) THEN		!Special case: NSI=1
	 CALL MVE(MCODE(ICODE),1,BUF,R,1,1)
         CALL MVE(7,NSO,R,RBUF,0,1)	!Replicate pixel NSO times.
      ELSE IF (IZOOM.EQ.1) THEN		!Special case: IZOOM=1
         IF (ICODE.NE.4) CALL MVE(MCODE(ICODE),NSI,BUF,RBUF,1,1)
      ELSE IF (ZOOMS.GT.1.) THEN	!Magnify the line
         CALL MAG(BUF,BUF,RBUF,SAMP,WGHT,NSO,ICODE,BUF,BUF)
      ELSE IF (IZOOM.LT.0) THEN		!Compress line via integral zoom
         INC = -IZOOM
         CALL SHRINZ(BUF,BUF,RBUF,INC,NSO,ICODE,BUF,BUF)
      ELSE				!Compress line via real zoom
         CALL SHRINK(BUF,BUF,RBUF,SAMP,WGHT,ICODE,NSO,BUF,BUF)
      ENDIF
      RETURN
      END
C******************************************************************************
C Magnifies an image line via interpolation.
C Input may be byte (BUF), halfword (BUFH), fullword (FBUF),
C or real*4 (SBUF).  The output line (RBUF) is real*4.
C
      SUBROUTINE MAG(BUF,BUFH,RBUF,SAMP,WGHT,NSO,ICODE,FBUF,SBUF)
      BYTE BUF(*)
      INTEGER*2 BUFH(*),SAMP(NSO)
      INTEGER*4 FBUF(*)
      REAL*4 SBUF(*),RBUF(NSO),WGHT(NSO)
C
      I0 = 0
      IF (ICODE.EQ.2) GOTO 100
      IF (ICODE.EQ.3) GOTO 200
      IF (ICODE.EQ.4) GOTO 300
C
C     ....Here for byte input
      D2 = MYZEXT(BUF(1))
      DO I=1,NSO
         I2 = SAMP(I)
         IF (I2.NE.I0) THEN
            I0 = I2
            D1 = D2
	    D2 = MYZEXT(BUF(I2))
         ENDIF
         A = WGHT(I)
         RBUF(I) = A*D1 + (1.0-A)*D2
      ENDDO
      RETURN
C
C     ....Here for halfword input
  100 D2 = BUFH(1)

      DO I=1,NSO
         I2 = SAMP(I)
         IF (I2.NE.I0) THEN
            I0 = I2
            D1 = D2
            D2 = BUFH(I2)
         ENDIF
         A = WGHT(I)
         RBUF(I) = A*D1 + (1.0-A)*D2
      ENDDO
      RETURN
C
C     ....Here for fullword input
  200 D2 = FBUF(1)

      DO I=1,NSO
         I2 = SAMP(I)
         IF (I2.NE.I0) THEN
            I0 = I2
            D1 = D2
            D2 = FBUF(I2)
         ENDIF
         A = WGHT(I)
         RBUF(I) = A*D1 + (1.0-A)*D2
      ENDDO
      RETURN
C
C     ....Here for real*4 input
  300 D2 = SBUF(1)

      DO I=1,NSO
         I2 = SAMP(I)
         IF (I2.NE.I0) THEN
            I0 = I2
            D1 = D2
            D2 = SBUF(I2)
         ENDIF
         A = WGHT(I)
         RBUF(I) = A*D1 + (1.0-A)*D2
      ENDDO
      RETURN
      END
C******************************************************************************
C Compresses a line by a floating pt zoom factor
C Input may be byte (BUF), halfword (BUFH), fullword (FBUF),
C or real*4 (SBUF).  The output is real*4 (RBUF).
C
      SUBROUTINE SHRINK(BUF,BUFH,RBUF,SAMP,ABUF,ICODE,NSO,FBUF,SBUF)
      BYTE BUF(*)
      INTEGER*2 BUFH(*),SAMP(NSO)
      INTEGER*4 FBUF(*)
      REAL*4 SBUF(*),RBUF(NSO),ABUF(NSO)
C
      I1 = 1
      IF (ICODE.EQ.2) GOTO 100
      IF (ICODE.EQ.3) GOTO 200
      IF (ICODE.EQ.4) GOTO 300
C
C     ....Here for byte input 
      D1 = MYZEXT(BUF(1))
      D0 = 0.0
 
      DO I=1,NSO
         D = D1 - D0		!Remaining fraction of previous pixel
         I2 = SAMP(I)
         I1 = I1 + 1
         IF (I1.LT.I2) THEN
            ISUM = 0
            DO WHILE (I1.LT.I2)
               ISUM = ISUM + MYZEXT(BUF(I1))
               I1 = I1 + 1
            ENDDO
            D = D + ISUM
         ENDIF
         D1 = MYZEXT(BUF(I2))
         D0 = ABUF(I)*D1
         RBUF(I) = D + D0
      ENDDO
      RETURN
C
C     ....Here for halfword input 
  100 D1 = BUFH(1)
      D0 = 0.0
 
      DO I=1,NSO
         D = D1 - D0
         I2 = SAMP(I)
         I1 = I1 + 1
         IF (I1.LT.I2) THEN
            ISUM = 0
            DO WHILE (I1.LT.I2)
               ISUM = ISUM + BUFH(I1)
               I1 = I1 + 1
            ENDDO
            D = D + ISUM
         ENDIF
         D1 = BUFH(I2)
         D0 = ABUF(I)*D1
         RBUF(I) = D + D0
      ENDDO
      RETURN
C
C     ....Here for fullword input 
  200 D1 = FBUF(1)
      D0 = 0.0
 
      DO I=1,NSO
         D = D1 - D0
         I2 = SAMP(I)
         I1 = I1 + 1
         IF (I1.LT.I2) THEN
            RSUM = 0
            DO WHILE (I1.LT.I2)
               RSUM = RSUM + FBUF(I1)
               I1 = I1 + 1
            ENDDO
            D = D + RSUM
         ENDIF
         D1 = FBUF(I2)
         D0 = ABUF(I)*D1
         RBUF(I) = D + D0
      ENDDO
      RETURN
C
C     ....Here for real*4 input 
  300 D1 = SBUF(1)
      D0 = 0.0
 
      DO I=1,NSO
         D = D1 - D0
         I2 = SAMP(I)
         I1 = I1 + 1
         IF (I1.LT.I2) THEN
            RSUM = 0
            DO WHILE (I1.LT.I2)
               RSUM = RSUM + SBUF(I1)
               I1 = I1 + 1
            ENDDO
            D = D + RSUM
         ENDIF
         D1 = SBUF(I2)
         D0 = ABUF(I)*D1
         RBUF(I) = D + D0
      ENDDO
      RETURN
      END
C*******************************************************************************
C Compresses a line by an integer zoom factor
C Input is byte, halfword, fullword, or real*4
C
      SUBROUTINE SHRINZ(BUF,BUFH,RBUF,INC,NSO,ICODE,FBUF,SBUF)
      REAL RBUF(NSO)
      BYTE BUF(*)
      INTEGER*2 BUFH(*)
      INTEGER*4 FBUF(*)
      REAL*4 SBUF(*)
C
      N = INC - 1
      II = 1
C
      IF (ICODE.EQ.2) GOTO 100
      IF (ICODE.EQ.3) GOTO 200
      IF (ICODE.EQ.4) GOTO 300
C
C     ....Here for byte input data
C
      DO I=1,NSO
         ISUM = MYZEXT(BUF(II))
         II = II + 1
         DO J=1,N
            ISUM = ISUM + MYZEXT(BUF(II))
            II = II + 1
         ENDDO
         RBUF(I) = ISUM
      ENDDO
      RETURN
C
C     ....Here for halfword input data
  100 DO I=1,NSO
         ISUM = BUFH(II)
         II = II + 1
         DO J=1,N
            ISUM = ISUM + BUFH(II)
            II = II + 1
         ENDDO
         RBUF(I) = ISUM
      ENDDO
      RETURN
C
C     ....Here for fullword input data
  200 DO I=1,NSO
         RSUM = FBUF(II)
         II = II + 1
         DO J=1,N
            RSUM = RSUM + FBUF(II)
            II = II + 1
         ENDDO
         RBUF(I) = RSUM
      ENDDO
      RETURN
C
C     ....Here for REAL*4 input data
  300 DO I=1,NSO
         RSUM = SBUF(II)
         II = II + 1
         DO J=1,N
            RSUM = RSUM + SBUF(II)
            II = II + 1
         ENDDO
         RBUF(I) = RSUM
      ENDDO
      RETURN
      END
C*******************************************************************************
C Multiply a line by a constant C
C
      SUBROUTINE SMUL(NSO,RIN,ROUT,C)
      REAL*4 RIN(NSO),ROUT(NSO)
C
      DO I=1,NSO
         ROUT(I) = C*RIN(I)
      ENDDO
      RETURN
      END
C*******************************************************************************
C Compute an output image line by iterpolating between two image
C lines BUF1 and BUF2.  The input lines are real*4 (products of SREAL).
C The output line may be byte (BUF), halfword (BUFH), fullword (FBUF),
C or real*4 (SBUF).
C
      SUBROUTINE INTRPV(BUF1,BUF2,BUF,BUFH,
     &		C,D,OCODE,NSO,RHI,RLO,FBUF,SBUF)
      BYTE BUF(NSO)
      INTEGER*2 BUFH(NSO)
      INTEGER*4 FBUF(NSO),OCODE
      REAL*4 SBUF(NSO),BUF1(NSO),BUF2(NSO)
C
      IF (OCODE.EQ.2) GOTO 100
      IF (OCODE.EQ.3) GOTO 200
      IF (OCODE.EQ.4) GOTO 300
C
C     ....Here for byte output
      DO I=1,NSO
         DN = BUF1(I)*C + BUF2(I)*D + 0.5
         IF (DN.GT.RHI) THEN
            DN = RHI
         ELSE IF (DN.LT.RLO) THEN
            DN = RLO
 	 ENDIF
         IDN = DN
	 IF (IDN .LE. 127) THEN
	    BUF(I) = IDN
	 ELSE
	    BUF(I) = IDN - 256
	 END IF
      ENDDO
      RETURN
C
C    ....Here for halfword output
  100 DO I=1,NSO
         DN = BUF1(I)*C + BUF2(I)*D
         IF (DN.GE.0.) THEN
            DN = DN + 0.5
         ELSE
            DN = DN - 0.5
         ENDIF
         IF (DN.GT.RHI) THEN
            DN = RHI
         ELSE IF (DN.LT.RLO) THEN
            DN = RLO
 	 ENDIF
         BUFH(I) = DN
      ENDDO
      RETURN
C
C    ....Here for fullword output
  200 DO I=1,NSO
         DN = BUF1(I)*C + BUF2(I)*D
         IF (DN.GE.0.) THEN
            DN = DN + 0.5
         ELSE
            DN = DN - 0.5
         ENDIF
         IF (DN.GT.RHI) THEN
            DN = RHI
         ELSE IF (DN.LT.RLO) THEN
            DN = RLO
 	 ENDIF
         FBUF(I) = DN
      ENDDO
      RETURN
C
C    ....Here for real*4 output
  300 DO I=1,NSO
         SBUF(I) = BUF1(I)*C + BUF2(I)*D
      ENDDO
      RETURN
      END
C*******************************************************************************
C Scale REAL*4 line (RBUF) to byte (BUF), halfword (BUFH),
C fullword (FBUF), or real*4 (SBUF).
C
      SUBROUTINE IOUT(RBUF,BUF,BUFH,C,OCODE,RHI,RLO,NSO,FBUF,SBUF)
      REAL*4 RBUF(*),SBUF(NSO)
      LOGICAL*1 BUF(NSO)
      INTEGER*2 BUFH(NSO)
      INTEGER*4 FBUF(NSO),OCODE
C
      IF (OCODE.EQ.2) GOTO 100
      IF (OCODE.EQ.3) GOTO 200
      IF (OCODE.EQ.4) GOTO 300
C
C     ....Here to scale to byte output
      DO I=1,NSO
         DN = RBUF(I)*C + 0.5
         IF (DN.GT.RHI) THEN
            DN = RHI
         ELSE IF (DN.LT.RLO) THEN
            DN = RLO
 	 ENDIF
         IDN = DN
	 IF (IDN .LE. 127) THEN
	    BUF(I) = IDN
	 ELSE
	    BUF(I) = IDN - 256
	 END IF
      ENDDO
      RETURN
C
C    ....Here for halfword output
  100 DO I=1,NSO
         DN = RBUF(I)*C
         IF (DN.GE.0.) THEN
            DN = DN + 0.5
         ELSE
            DN = DN - 0.5
         ENDIF
         IF (DN.GT.RHI) THEN
            DN = RHI
         ELSE IF (DN.LT.RLO) THEN
            DN = RLO
 	 ENDIF
         BUFH(I) = DN
      ENDDO
      RETURN
C
C    ....Here for fullword output
  200 DO I=1,NSO
         DN = RBUF(I)*C
         IF (DN.GE.0.) THEN
            DN = DN + 0.5
         ELSE
            DN = DN - 0.5
         ENDIF
         IF (DN.GT.RHI) THEN
            DN = RHI
         ELSE IF (DN.LT.RLO) THEN
            DN = RLO
 	 ENDIF
         FBUF(I) = DN
      ENDDO
      RETURN
C
C    ....Here for real*4 output
  300 DO I=1,NSO
         SBUF(I) = RBUF(I)*C
      ENDDO
      RETURN
      END
C*******************************************************************************
C Expand or reduce an image without interpolation.
C Input and output data formats are either both byte or both halfword.
C Mixed data modes (e.g. byte input, halfword output) are handled by
C enabling VICAR I/O data conversion via the XVOPEN calls.
C
      SUBROUTINE SNOIN(NSI,NSO,BUF,OBUF,SAMP)
      INTEGER*2 BUF(NSI)
      INTEGER*2 OBUF(NSO),SAMP(NSO)
C
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSIX,OUNIT,OCODE,NLO,NSOX,
     &		ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI,SSI,OCODE
C
      COMMON/C1YY/TBL,TBLH
      LOGICAL*1 TBL(0:255)
      INTEGER*2 TBLH(-32768:32767)
C
      RLO = ILO
      RHI = IHI
      R = 1./ZOOMS
      OFFSET = 0.5
      IF (ZOOMS.LT.1.0) OFFSET=1.0
C
C     ....Assign input pixel to each pixel on output image line.
      DO I=1,NSO		    !I=output pixel index
         J = R*(I-OFFSET) + 1.00002 !J=corresponding input pixel index
         IF (J.LT.1) THEN
            J = 1
         ELSE IF (J.GT.NSI) THEN
            J = NSI
         ENDIF
         SAMP(I) = J		    !Build sample look-up table
      ENDDO
C
C     ....Set up stretch table for byte and halfword input
      IF (LFLAG.EQ.1) THEN
         IF (ICODE.EQ.1) CALL TBLGEN(GSCALE,ILO,IHI,tbl)
         IF (ICODE.EQ.2) CALL TBLGENH(GSCALE,ILO,IHI,tblh)
      ENDIF
C
      R = 1./ZOOML			!Input line increment
      RSLI = SLI + .000002		!Float input starting-line
      LAST = 0				!Last input line number
C
      DO 100 L=1,NLO
      ILINE = R*(L-OFFSET) + RSLI	!Compute input line number
      IF (ILINE.EQ.LAST) GOTO 100	!Skip if already read
      CALL XVREAD(IUNIT,BUF,IND,'LINE',ILINE,'SAMP',SSI,'NSAMPS',NSI,
     +		  ' ')
      LAST = ILINE			!Last input line number

      IF (LFLAG.EQ.0.OR.ICODE.GT.2) THEN	!No table look-up
         IF (ICODE.EQ.1) THEN
            CALL EXPAND(NSO,BUF,SAMP,OBUF)
         ELSE IF (ICODE.EQ.2) THEN
            CALL EXPANDH(NSO,BUF,SAMP,OBUF)
         ELSE IF (ICODE.EQ.3) THEN
            CALL EXPANDF(NSO,BUF,SAMP,OBUF,GSCALE,RLO,RHI,LFLAG)
         ELSE
            CALL EXPANDR(NSO,BUF,SAMP,OBUF,GSCALE)
         ENDIF
      ELSE					!Use look-up table
         IF (ICODE.EQ.1) THEN
            CALL EXPANDTBL(NSO,BUF,SAMP,TBL,OBUF)
         ELSE
            CALL EXPANDTBLH(NSO,BUF,SAMP,TBLH,OBUF)
         ENDIF
      ENDIF
C
  100 CALL XVWRIT(OUNIT,OBUF,IND,' ')
      RETURN
      END
C*******************************************************************************
C Generate look-up table (TBL) for byte input.  Table causes scaling of input
C by GSCALE and truncation of output at ILO and IHI.
C
      SUBROUTINE TBLGEN(GSCALE,ILO,IHI,TBL)
      BYTE TBL(0:255)
C
      DO I=0,255			!Do DN=0 to 255
         D = GSCALE*I			!Scale DN by GSCALE
	 IDN = MIN(IHI, MAX(ILO,NINT(D)))
	 IF (IDN .LE. 127) THEN
	    TBL(I) = IDN
	 ELSE
	    TBL(I) = IDN - 256
	 END IF
      ENDDO
      RETURN
      END
C*******************************************************************************
C Generate look-up table (TBLH) for halfword input.  Table causes scaling of
C input by GSCALE and truncation of output at ILO and IHI.
C
      SUBROUTINE TBLGENH(GSCALE,ILO,IHI,TBLH)
      INTEGER*2 TBLH(-32768:32767)
C
      DO I=-32768,32767
         D = GSCALE*I
	 IDN = MIN(IHI, MAX(ILO,NINT(D)))
         TBLH(I) = IDN
      ENDDO
      RETURN
      END
C*******************************************************************************
C Magnify image line without interpolation.
C Input line may be byte, halfword, fullword, or REAL*4 format.
C Output line (OBUF) will be in same format as input.
C No scaling or checking for saturation is performed for byte or halfword.
C
      SUBROUTINE EXPAND(NSO,BUF,SAMP,OBUF)
      LOGICAL*1 BUF(*),OBUF(NSO)
      INTEGER*2 BUFH(*),OBUFH(NSO),SAMP(NSO)
      INTEGER*4 BUFF(*),OBUFF(NSO)
      REAL*4 BUFR(*),OBUFR(NSO)
C
      DO I=1,NSO			!Byte data
         OBUF(I) = BUF(SAMP(I))
      ENDDO
      RETURN
C...............................................................................
      ENTRY EXPANDH(NSO,BUFH,SAMP,OBUFH)
      DO I=1,NSO			!Halfword data
         OBUFH(I) = BUFH(SAMP(I))
      ENDDO
      RETURN
C...............................................................................
      ENTRY EXPANDF(NSO,BUFF,SAMP,OBUFF,SCALE,RLO,RHI,LFLAG)
      IF (LFLAG.EQ.1) THEN
         DO I=1,NSO
            DN = NINT(SCALE*BUFF(SAMP(I)))
            OBUFF(I) = MIN(RHI, MAX(RLO,DN))
         ENDDO
      ELSE
         DO I=1,NSO			!Fullword data
            OBUFF(I) = BUFF(SAMP(I))
         ENDDO
      ENDIF
      RETURN
C...............................................................................
      ENTRY EXPANDR(NSO,BUFR,SAMP,OBUFR,SCALE)
      IF (SCALE.NE.1.0) THEN
         DO I=1,NSO
            OBUFR(I) = SCALE*BUFR(SAMP(I))
         ENDDO
      ELSE
         DO I=1,NSO			!REAL*4 data
            OBUFR(I) = BUFR(SAMP(I))
         ENDDO
      ENDIF
      RETURN
      END
C*******************************************************************************
C Magnify an image line without interpolation.
C Input and output data formats are either both bytes or both halfword.
C Scaling is performed using table look-up
C
      SUBROUTINE EXPANDTBL(NSO,BUF,SAMP,TBL,OBUF)
      BYTE BUF(*),OBUF(*),TBL(0:255)
      INTEGER*2 BUFH(*),OBUFH(*),TBLH(-32768:32767)
      INTEGER*2 SAMP(*)
C
      DO I=1,NSO
         IDN = MYZEXT(BUF(SAMP(I)))
         OBUF(I) = TBL(IDN)
      ENDDO
      RETURN
C...............................................................................
      ENTRY EXPANDTBLH(NSO,BUFH,SAMP,TBLH,OBUFH)
      DO I=1,NSO
         OBUFH(I) = TBLH(BUFH(SAMP(I)))
      ENDDO
      RETURN
      END
C*******************************************************************************
c  function to replace old inline fcn. using ZEXT ...
      integer*4 function MYZEXT(half)
      integer*4 long
      integer*2 half
      long = half
      if (long.lt.0) long = 65536+long
      MYZEXT = long
      return
      end

