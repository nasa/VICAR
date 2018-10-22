      INCLUDE 'VICMAIN_FOR'
C
C**********************************************************************
C
C     BMSS will combine input images into a single 3-D BIL format image.
C     Each band of the various inputs
C     must contain the same number of lines and samples.
C
C**********************************************************************
C
      SUBROUTINE MAIN44
C
      IMPLICIT NONE
C
      INTEGER*4     I, INCNT, J, K, OUTUNIT, STATUS, TOTNB
      INTEGER*4     INUNIT(50), NB(50), NL(50), NS(50)
      REAL*4        BUF(32767)
      CHARACTER*4   INFMT(50)
C
      CALL XVPCNT('INP',INCNT)
      DO I = 1,INCNT
         CALL XVUNIT(INUNIT(I),'INP',I,STATUS,' ')
         CALL XVOPEN(INUNIT(I),STATUS,'U_FORMAT','REAL','OPEN_ACT',
     &        'SA','IO_ACT','SA',' ')
         CALL XVGET(INUNIT(I),STATUS,'NL',NL(I),'NS',NS(I),'NB',
     &        NB(I),'FORMAT',INFMT(I),' ')
      ENDDO
      DO I = 2,INCNT
         IF (NL(I).NE.NL(1)) THEN
	    CALL XVMESSAGE(
     &		' INPUTS MUST HAVE THE SAME NUMBER OF LINES',' ')
	    CALL ABEND
	 ENDIF
         IF (NS(I).NE.NS(1)) THEN
	    CALL XVMESSAGE(
     &		' INPUTS MUST HAVE THE SAME NUMBER OF SAMPLES',' ')
	    CALL ABEND
	 ENDIF
         IF (INFMT(I).NE.INFMT(1)) THEN
	    CALL XVMESSAGE(' INPUTS MUST HAVE THE SAME FORMAT',' ')
	    CALL ABEND
	 ENDIF
      ENDDO
C
      TOTNB = 0
      DO I = 1,INCNT
         TOTNB = TOTNB + NB(I)
      ENDDO
C
C  OUTPUT DATA
C
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NL(1),
     &     'U_NS',NS(1),'U_NB',TOTNB,'U_ORG','BIL','U_FORMAT',
     &     'REAL','OPEN_ACT','SA','IO_ACT','SA',' ')
C
      DO I = 1,NL(1)
         DO J = 1,INCNT
            DO K = 1,NB(J)
               CALL XVREAD(INUNIT(J),BUF,STATUS,'SAMP',1,'NSAMPS',
     &              NS(1),'LINE',I,'BAND',K,' ')
               CALL XVWRIT(OUTUNIT,BUF,STATUS,' ')
            ENDDO
         ENDDO
      ENDDO
      DO I = 1,INCNT
         CALL XVCLOSE(INUNIT(I),STATUS,' ')
      ENDDO
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END
