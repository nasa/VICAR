CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify or reduce an image via interpolation.
C
	SUBROUTINE SINTRP(ib,n,buf,rbuf,obuf,samp,wght)
      IMPLICIT NONE
	INTEGER*4 N     !Dummy since nso is put here but available at CP
      INTEGER*4 IB,SAMP(N)					!,OBUF(N),BUF(N)
      REAL*4 RBUF(N,2),WGHT(N),OBUF(N),BUF(N)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,OUNIT     !Input and output logical unit numbers
      INTEGER*4 ICODE,OCODE     !Input and ouput image format
      INTEGER*4 SLI,SSI,NLI,NSI !Input image area (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
      REAL*4 ZOOML,ZOOMS        !Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM     !Integer zoom factors (=0 if not integer)
      INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,IHI saturation
      REAL*4 GSCALE             !Output DN scale factor

      INTEGER*4 I,J,L,I1,I2,IND
      REAL*4 R,SCALE,X0,X

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
      IF (NLI.EQ.1) GOTO 60
      IF (LZOOM.EQ.1) GOTO 50
      IF (ZOOML.GT.1.) THEN	!If vertical zoom .gt. 1, magnify it.
         CALL MAGNIFY(SCALE,NSO,SAMP,WGHT,ib,buf,rbuf,obuf(sso))
      ELSE 			!Else, compress it.
         CALL COMPRESS(SCALE,NSO,SAMP,WGHT,ib,buf,rbuf,obuf(sso))
      ENDIF
      RETURN         

C     ....Special case: LZOOM=1
   50 J = SLI - 1
      DO L=1,NLO
         CALL SREAD(IUNIT,L+J,SSI,NSI,NSO,IZOOM,ZOOMS,SAMP,WGHT,
     &	  ib,buf,rbuf)
         CALL OUT_SCALE(OCODE,SCALE,ILO,IHI,NSO,RBUF,
     &	  obuf(sso))
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+L-1,'BAND',IB,' ')
      ENDDO
      RETURN

C     ....Special case: NLI=1
   60 CALL SREAD(IUNIT,SLI,SSI,NSI,NSO,IZOOM,ZOOMS,SAMP,WGHT,
     & ib,buf,rbuf)
      CALL OUT_SCALE(OCODE,SCALE,ILO,IHI,NSO,RBUF,obuf(sso))
      DO L=1,NLO
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+L-1,'BAND',IB,' ')
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE OUT_SCALE(OCODE,SCALE,ILO,IHI,NSO,RBUF,obuf)
      IMPLICIT NONE
      INTEGER*4 OCODE,ILO,IHI,NSO
      REAL*4 SCALE,RBUF(NSO),OBUF(NSO)

      INTEGER*4 I,DN

      IF (OCODE.NE.4) THEN
         DO I=1,NSO
            DN = NINT(SCALE*RBUF(I))         
            IF (DN.GT.IHI) THEN
               DN = IHI
            ELSE IF (DN.LT.ILO) THEN
               DN = ILO
            ENDIF
            OBUF(I) = REAL(DN)
         ENDDO
      ELSE
         DO I=1,NSO
            OBUF(I) = SCALE*RBUF(I)
         ENDDO
      ENDIF

      RETURN
      END
