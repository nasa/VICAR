C Routine to list integer histogram array
C
      SUBROUTINE PHIST(FREQ,NS,ILOW,IHIGH,ISPIKE,IMODE)
      INTEGER NS,ILOW,IHIGH,ISPIKE,IMODE,COUNT
      INTEGER FREQ(1),MAXT/7.4752E+05/
      CHARACTER*131 LISTO,MSG
      DATA LISTO /' '/
C
      LISTO(131:131) = '*'
      N = IHIGH - ILOW + 1
      IF(N.LE.0.OR.NS.LE.0) GOTO 1000
      D = 1./NS
      R = 100.*D
      msg = ' GRAY      FREQ   PERCENT   0        10        20' //
     C'        30        40        50        60        70        80'//
     C'        90       100'
      CALL xvmessage (msg, ' ')
      N1 = N - 1
   10 MAXS = MAXT
C     ....Search for n+1st highest freq, ignoring lowest and highest levels.
      DO J=1,ISPIKE+1
      MAX = 0
         DO I=2,N1
            IF (FREQ(I).GT.MAX.AND.FREQ(I).LT.MAXS) MAX=FREQ(I)
         ENDDO
      MAXS = MAX
      ENDDO

      IF (MAX.EQ.0) THEN		!If max frequency is zero
         IF (ISPIKE+1.GT.1) THEN        !and spikes.gt.1 then
            ISPIKE = ISPIKE - 1         !reduce number of spikes
            GOTO 10                     !and try again
         ELSE
            MAX=MAX0(FREQ(1),FREQ(N))   !otherwise, use the ends.
            IF (MAX.EQ.0) GOTO 1001     !If all levels zero, print err.
         ENDIF
      ENDIF

      IFLAG = 0
      AVG = 0.
      SIGMA = 0.
      RI = 0.
C
      DO 905 I=1,N
      NCHAR = 129
      IFREQ = FREQ(I)
      IF(IFREQ.GT.0.OR.IMODE.GT.0) GOTO 900
      IF(IFLAG.EQ.0) CALL XVMESSAGE(' ',' ') 
      IFLAG = 1
      GOTO 905
  900 IFLAG = 0
      RFREQ = IFREQ
      AVG = AVG + RFREQ*RI
      SIGMA = SIGMA + RFREQ*RI*RI
      PERCEN = RFREQ*R
      IVAL = MAX0(100*IFREQ/MAX+1,1)
      IF(IVAL.LE.101) GOTO 902
      IVAL = 101
      NCHAR = 131
902   WRITE (LISTO(1:6),'(I5)') ILOW+I-1
      WRITE (LISTO(11:18),'(I5)') IFREQ
      WRITE (LISTO(17:25),'(F8.3)') PERCEN+.0005
      LISTO(30:129) = ' '
      DO COUNT = 39,129,10
           LISTO(COUNT:COUNT)='+'
      ENDDO
      DO IUNCON  =  29,29+IVAL-1
          LISTO(IUNCON:IUNCON) = '*'
      ENDDO
      CALL XVMESSAGE(LISTO(1:NCHAR),' ')
  905 RI = I
C
      SIGMA =(SIGMA - AVG*AVG*D)*D
      IF(SIGMA.GT.0.) SIGMA =SQRT(SIGMA)
    	AVGREY = AVG*D
        CALL XVMESSAGE(' ',' ')
        WRITE(MSG,100)AVGREY
        CALL XVMESSAGE(MSG,' ')
  100   FORMAT('AVERAGE GRAY LEVEL = ', E11.4)
        CALL XVMESSAGE(' ',' ')

        WRITE(MSG,200)SIGMA
        CALL XVMESSAGE(MSG,' ')
  200   FORMAT('STANDARD DEVIATION = ', E11.4)
        CALL XVMESSAGE(' ',' ')

        WRITE(MSG,300)NS
        CALL XVMESSAGE(MSG,' ')
  300   FORMAT('NUMBER OF ELEMENTS = ', I9)
        CALL XVMESSAGE(' ',' ')
      RETURN
C
 1000 CALL XVMESSAGE(' **ERR IN PHIST ARGUMENT LIST',' ')
      CALL XVMESSAGE(' NO HISTOGRAM WILL BE PRINTED',' ')
      RETURN
 1001 CALL XVMESSAGE(' ***Histogram contains all zeroes',' ')
      RETURN
      END
