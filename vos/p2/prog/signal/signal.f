      INCLUDE 'VICMAIN_FOR'
C
C Program signal
C
C 28 JUL 1997 ...RRD...      Ported to UNIX.
C 17 NOV 1995 ...JRY...      Written
C
      SUBROUTINE MAIN44
      INTEGER*2 DN
      INTEGER*4 CNT, DEF, ICNT, IND, ISTAT, NI, NL, NS
      INTEGER*4 EXPOS(50), INSTANCES(30), IUNIT(50), LS(2), PICSCALE(50)
      REAL*4    SIGNAL, REXPOS
      CHARACTER*255 LFNAME, TBL
      CHARACTER*8  TASKS(30)
      CHARACTER*1 TAB

      TAB = CHAR(9)
      CALL IFMESSAGE('signal version 28-Jul-97')
      CALL XVPCNT('INP',NI)	
      CALL XVP('LIST',LFNAME,ICNT)
      CALL XVPARM('TBL',TBL,CNT,DEF,0)
      CALL XVPARM('LS',LS,CNT,DEF,0)

      IF (NI .NE. 0) THEN
         DO I=1,NI
            CALL XVUNIT(IUNIT(I),'INP',I,STAT,' ')
            CALL XVOPEN(IUNIT(I),STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &                  'U_FORMAT','HALF',' ')
         ENDDO
      ELSE IF (ICNT .NE. 0) THEN    !input frames are in SRCH-list
     	 OPEN(UNIT=99,FILE=LFNAME,STATUS='OLD',ERR=999)
	 READ(99,FMT=1) LFNAME                      !SKIP FIRST LINE
1        FORMAT(A)
 	 DO I=1,51
	    READ(99,FMT=1,END=11,ERR=999) LFNAME
	    IF (I .GE. 51) GO TO 990
            NI = I
	    CALL XVUNIT(IUNIT(I),'NONE',I,ISTAT,'U_NAME',LFNAME,' ')
	    CALL XVOPEN(IUNIT(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     &                  'U_FORMAT','HALF',' ')
         ENDDO
11       CONTINUE
      ELSE
         CALL XVMESSAGE('Specify input files as INP or LIST',' ')
         CALL ABEND
      END IF

C     Get PICSCALE from file label (number of frames PICSUM'd for each input)

      CALL XVGET(IUNIT(1),IND,'NL',NL,'NS',NS,' ')
      IF (LS(1) .GT. NL .OR. LS(2) .GT. NS) THEN
         CALL XVMESSAGE('LS EXCEEDS SIZE OF IMAGE', ' ')
         CALL ABEND         
      END IF
      DO 5 I=1,NI
         CNT = 30
         CALL XLGET(IUNIT(I),'PROPERTY','EXPOSURE_DURATION',REXPOS,
     &              ISTAT,'PROPERTY','CASSINI-ISS','FORMAT','REAL',' ')
         IF (ISTAT .NE. 1) THEN
            CALL MABEND('ERROR READING LABEL EXPOSURE_DURATION')
         END IF
         EXPOS(I) = IFIX(REXPOS)
         CALL XLHINFO(IUNIT(I),TASKS,INSTANCES,CNT,IND,' ')
         DO J=CNT,1,-1		!Search for last value of picture scale
            CALL XLGET(IUNIT(I),'HISTORY','PICSCALE',PICSCALE(I),IND,
     &            'HIST',TASKS(J),'INSTANCE',INSTANCES(J),
     &            'FORMAT','INT',' ')
            IF (IND.EQ.1) GOTO 5
         ENDDO

C Didn't find a PICSCALE
         PICSCALE(I) = 1		!Default picture scale
5     CONTINUE

C Open output table
      OPEN(15,FILE=TBL,STATUS='UNKNOWN',IOSTAT=JST,ERR=992)
      WRITE(15,100) ' EXPOSURE',TAB,' SIGNAL'
100   FORMAT(A9,A1,A7)

      DO I=1,NI 	!Read data from each flat-field frame...
         CALL XVREAD(IUNIT(I),DN,IND,'LINE',LS(1),'SAMP',LS(2),
     &               'NSAMPS',1,' ')
         SIGNAL = FLOAT(DN)/PICSCALE(I)
         WRITE(15,110) EXPOS(I),TAB,SIGNAL
      ENDDO
110   FORMAT(I10,A1,F8.2)

C Close inputs and output
      DO I=1,NI
        CALL XVCLOSE(IUNIT(I),IND,' ')
      ENDDO
      CLOSE(15)

      CALL XVMESSAGE('signal task completed',' ')
      RETURN

  992 CALL XVMESSAGE('ERROR WRITING OUTPUT TABLE', ' ')
      CALL ABEND
  990 CALL XVMESSAGE('MORE THAN 50 FILENAMES IN LIST', ' ')
      CALL ABEND
  999 CALL XVMESSAGE('ERROR OPENING INPUT SRCH LIST FILE', ' ')
      CALL ABEND
      RETURN
      END
