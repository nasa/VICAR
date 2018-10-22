C 5 SEP 1994 .... CRI .... MSTP S/W CONVERSION (VICAR PORTING)

      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44 ! DROPOUT
      IMPLICIT INTEGER(A-Z)
      INTEGER DROPPED(2000)
      INTEGER bufsize
      INTEGER*2 BUF(1000000)
      CHARACTER*80 MSG1
      LOGICAL MISSING, MORE
      COMMON/C1/SL,SB,NLO,NBO,NLI,NBI

      DATA BUFSIZE/1000000/
      CALL IFMESSAGE('DROPOUT version 5-SEP-94')
      CALL XVEACTION('SA',' ')

C     Open input data
      CALL XVUNIT(UNIT2,'INP',1,IND,' ')
      CALL XVOPEN(UNIT2,IND,'U_FORMAT','HALF',' ')

C     Open output data 
      CALL XVUNIT(UNIT1,'OUT',1,IND,' ')
      CALL XVOPEN(UNIT1,IND,'OP','WRITE',' ')

C     Get size field
      CALL XVSIZE(SL,SB,NLO,NBO,NLI,NBI)
      EL=SL+NLO-1
      IF(EL.GT.NLI)NLO=NLI-SL+1

C     Param processing
      CALL XVPARM('THRESH',THRESH,COUNT,DEF,1)

      NDROP=0
      NSO=NBO
      DO I=SL,EL
         CALL XVREAD(UNIT2,BUF,IND,' ')
         J=0
         MISSING=.TRUE.
         MORE=.TRUE.
         DO WHILE(MORE.AND.MISSING)!LOOK FOR DROPPED LINE
            J=J+1
            IF(BUF(J).GT.THRESH)MISSING=.FALSE.
            IF(J.EQ.NSO)MORE=.FALSE.
         ENDDO
         IF(MISSING)THEN  ! SAVE THE LINE NUMBER OF MISSING LINES
              NDROP=NDROP+1
              DROPPED(NDROP)=I
         ELSE
              CALL FIX(BUF,NSO,THRESH)  ! FIX GROUPS OF MISSING PIXELS
         ENDIF
         CALL XVWRIT(UNIT1,BUF,IND,' ')
      ENDDO
      call xvclose(unit1,ind,' ')
      CALL XVOPEN(UNIT1,IND,'OP','UPDATE','U_FORMAT','HALF',' ')
c             fix missing lines, groups of missing lines should be consecutive
      IF(NDROP.GT.0)THEN
         START=DROPPED(1)
         STOP=START
         DO I=1,NDROP
            IF(DROPPED(I+1).GT.STOP+1.or.i.eq.ndrop)THEN
               if((stop-start+2)*nso.lt.bufsize)then
                  CALL FIXLINES(START,STOP,BUF,NSO,STOP-START+1,UNIT1)
                  START=DROPPED(I+1)
                  STOP=START
               else
                  write(MSG1,101)START,STOP
101               format('too many lines to fix, start=',I4,' stop=',I4)
                  CALL XVMESSAGE(MSG1,' ')
               endif
            ELSE
               STOP=STOP+1
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE FIXLINES(START,STOP,BUF,NSO,NLINES,UNIT1)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 BUF(NSO,NLINES+2)
      REAL F1,F2,DY
      CHARACTER*80 MSG1
      COMMON/C1/SL,SB,NLO,NBO,NLI,NBI
      IF(START.EQ.1.OR.STOP.EQ.NLO)RETURN
       REC=START-SL
       CALL XVREAD(UNIT1,BUF(1,1),IND,'LINE',REC,' ')
c        last row in buffer is next good line
      REC=STOP-SL+2
      CALL XVREAD(UNIT1,BUF(1,NLINES+2),IND,'LINE',REC,' ')
      WRITE(MSG1,102)START,STOP
102   FORMAT(' lines ',I4,' to ',I4,' are missing')
      CALL XVMESSAGE(MSG1,' ')
      DO I=1,NSO
          F1=BUF(I,1)
          F2=BUF(I,NLINES+2)
          DY=(F2-F1)/(NLINES+1)
          DO J=1,NLINES
             F1=F1+DY
             BUF(I,J+1)=F1
          ENDDO
      ENDDO
      DO I=1,NLINES
           REC=START-1+I-SL+1
           CALL XVWRIT(UNIT1,BUF(1,I+1),IND,'LINE',REC,' ')
      ENDDO
      RETURN
      END
      SUBROUTINE FIX(BUF,NBO,THRESH)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 BUF(10000)
      LOGICAL MORE,MORE2
      MORE=.TRUE.
C     SKIP OVER MASK
      LEFT=0
      DO WHILE(MORE)
         LEFT=LEFT+1
         IF(BUF(LEFT).GT.THRESH)MORE=.FALSE.
         IF(LEFT.EQ.NBO)MORE=.FALSE.
      ENDDO
      MORE=.TRUE.
      RIGHT=NBO+1
      DO WHILE(MORE)
         RIGHT=RIGHT-1
         IF(BUF(RIGHT).GT.THRESH)MORE=.FALSE.
         IF(RIGHT.EQ.1)MORE=.FALSE.
      ENDDO
      MORE=.TRUE.
      I=LEFT-1
      DO WHILE(MORE)
         I=I+1
         IF(I.ge.RIGHT)MORE=.FALSE.
         IF(BUF(I).LE.THRESH)THEN
            START=I
            MORE2=.TRUE.
            DO WHILE(MORE2)
               I=I+1
               if(i.ge.right)more2=.false.
               IF(BUF(I).GT.THRESH)THEN
                  MORE2=.FALSE.  ! 
                  STOP=I-1
               ENDIF
            ENDDO
            if(stop.ge.start)then
               DNSTART=BUF(START-1)
               DNSTOP=BUF(STOP+1)
               CALL INTRPA(2,STOP-START+2,BUF(START-1),DNSTART,DNSTOP)
            endif
         ENDIF
      ENDDO
      RETURN
      END
