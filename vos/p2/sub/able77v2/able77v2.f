C***************************************************************************
C      SUBROUTINE ABLE77V2
C
C  PARAMETERS
C  ----------
C       IND     0=OK
C              -1=ONE OR MORE ITEMS COULD NOT BE FOUND,AND ARE SET TO -999
C                   (NUMBERS) OR "?" (CHARACTERS) IN ARR
C
C       DSRN       UNIT NUMBER
C
C       ARR       I*4 ARRAY OF RETURNED DATA (SEE ABLE77 DOCUMENT UPSTAIRS).
C              USER MUST INSERT REQUIRED LENGTH OF ARRAY TO BE FILLED,INTO
C              ARR(1) BEFORE CALLING.
C              THE FOLLOWING ITEMS HAVE BEEN IMPLEMENTED SO FAR:
C              1      =3 for a valid Voyager label, =0 otherwise
C              2      GIVES AN INTEGER FDS COUNT : THE REAL VALUE MULTIPLIED
C                     BY 100
C              3      EXPOSURE
C              4      FILTER POSITION
C              5      SCAN RATE
C              6      CAMERA SERIAL NUMBER,IE
C                     FOR VOYAGER 2,  WIDE ANGLE,CAMERA S/N IS 4
C                     FOR VOYAGER 2,NARROW ANGLE,CAMERA S/N IS 5
C                     FOR VOYAGER 1,  WIDE ANGLE,CAMERA S/N IS 6
C                     FOR VOYAGER 1,NARROW ANGLE,CAMERA S/N IS 7
C             7       CAMERA 1=NA 2=WA
C             8       GAIN 1=HI 0=LO
C            10       EVENT YEAR
C            11       EVENT DAY
C	     12       EVENT HOUR   
C	     13       EVENT MINUTE  
C	     14       EVENT SECOND   
C	     19       S/C ID
C	     20       PICNO
C	     29       INPUT TAPE
C	     31       OUTPUT TAPE
C	     33       INPUT FILE - 1
C	     34       OUTPUT FILE   
C	     35       ERT YEAR   
C	     36       ERT DAY
C	     37       ERT HOUR
C	     38       ERT MINUTE
C	     39       ERT SECOND
C
C  OPERATION
C  ---------
C
C     THE DATA SET MUST BE OPENED PRIOR TO CALLING ABLE77V2.
C     NOT-FOUND NUMERIC DATA IS RETURNED AS -999
C     NOT-FOUND CHARACTER DATA IS RETURNED AS "!"
C     PROGRAM DOES ASSUME STANDARD VOYAGER-TYPE VICAR LABELS,BUT IT WILL
C     SUCCESSFULLY IGNORE ANY OTHER LABELS PRIOR TO THE ONE STARTING 
C     "VGR-....".
C
C       NICK MARRIAGE       30 SEPTEMBER 1981
C     
C***************************************************************************
       SUBROUTINE ABLE77V2 (IND,DSRN,ARR)
c
       INTEGER*4 IND,DSRN,ARR(*),SIZE,VOYAGER,ANGLE,CAM
       INTEGER*4 VGRIND,IFDS1,IFDS2
       REAL*4 EXPOSURE,TMP
       CHARACTER*7200 ALABEL
c
       SIZE=ARR(1)              ! CALLER PASSES LENGTH OF ARRAY
       IF (SIZE.LT.1) THEN
          CALL XVMESSAGE( ' ABLE77V2 -SIZE IS LESS THAN 1',' ')
          CALL ABEND
       END IF
       IF (SIZE.GT.50)SIZE=50

       IND=0                     ! UNLESS SOMETHING GOES WRONG
c
C--SET ARR(1) TO DEFAULT VGR FLIGHT LABEL
       ARR(1)=3                        

C--NOW GET THE FIRST LABEL BLOCK INTO ALABEL...
        BUFSIZE=7200
       CALL XLGETLABEL(DSRN,ALABEL,BUFSIZE,ISTAT)
       CALL CHKSTAT(ISTAT,' ABLE77V2  ERR, ISTAT=',1,ISTAT,1)
        ISTAT=INDEX(ALABEL(1:),'VGR-')
        IF(ISTAT.EQ.0)ARR(1)=0
C--NOW START FINDING THINGS AND INSERTING INTO ARRAY...
c
C--FIRST FIND WHICH VOYAGER IT WAS
        IF(ALABEL(ISTAT+4:ISTAT+4).eq.'1')THEN
            VOYAGER=1
            VGRIND=0
        ELSE IF(ALABEL(ISTAT+4:ISTAT+4).eq.'2')THEN
            VOYAGER=2
            VGRIND=0
        ELSE
            VGRIND=1
        ENDIF
C
C--FIND THE FDS COUNT
       IF (2 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' FDS ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+5:ISTAT+5).NE.'*') THEN
           READ(ALABEL(ISTAT+5:),'(BN,I5)') IFDS1
           READ(ALABEL(ISTAT+11:),'(BN,I2)') IFDS2
           ARR(2) = IFDS1*100 + IFDS2
       ELSE
           CALL XVMESSAGE( ' ABLE77V2 -FDS COUNT NOT FOUND',' ')
           ARR(2) = -999
           IND = -1
       END IF
C
C--FIND EXPO TIME
       IF (3 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' EXP ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+5:ISTAT+5).NE.'*')  THEN
          READ(ALABEL(ISTAT+5:),'(BN,F7.0)')  EXPOSURE
          CALL MVe(4,1,EXPOSURE,ARR(3),1,1)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -EXPO TIME NOT FOUND',' ') ! FLAG ERROR
         ARR(3) = -999
         IND = -1
       ENDIF
C
C--FILTER POSITION
       IF (4 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' FILT ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+6:ISTAT+6).NE.'*') THEN 
          READ(ALABEL(ISTAT+6:),'(BN,I1)')  ARR(4)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -FILTER POSITION NOT FOUND',' ') ! FLAG MISTAKE MADE
         ARR(4) = -999
         IND=-1
       ENDIF
C
C--SCAN RATE
C--FIX CODE TO HANDLE 10:1 SCAN RATE ... FFM
       IF (5 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),'  SCAN RATE')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+12:ISTAT+12) .NE. '*') THEN
          READ(ALABEL(ISTAT+12:),'(BN,I2)') ARR(5)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -SCAN RATE NOT FOUND',' ')
         ARR(5)=-999
         IND=-1
       ENDIF
C
C--FIND CAMERA SERIAL NUMBER (ARR(6)
C       FOR VOYAGER 2,  WIDE ANGLE,CAMERA S/N IS 4
C       FOR VOYAGER 2,NARROW ANGLE,CAMERA S/N IS 5
C       FOR VOYAGER 1,  WIDE ANGLE,CAMERA S/N IS 6
C       FOR VOYAGER 1,NARROW ANGLE,CAMERA S/N IS 7
C  AND
C--CAMERA NA=1 WA=2 (CAM)
C
       IF (6 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),'NA CAMERA')
       IF(ISTAT.GT.0)THEN
         CAM = 1
         ANGLE=1
       ELSE IF(INDEX(ALABEL(1:),'WA CAMERA').GT.0)THEN
         CAM  = 2
         ANGLE = 0
       ELSE                            ! FLAG IT AS 'WRONG FORMAT LABEL'
         CALL XVMESSAGE( ' ABLE77V2 -CAMERA NOT FOUND',' ')
         CAM  = -999
         IND = -1
       ENDIF
c
       IF (VOYAGER .EQ. 1) THEN
          ARR(6) = ANGLE + 6
       ELSE IF (VOYAGER .EQ. 2) THEN
          ARR(6) = ANGLE + 4
       ELSE                     ! FLAG IT AS "BAD LABEL" AGAIN
          CALL XVMESSAGE( ' ABLE77V2 -CAM SERIAL # NOT FOUND',' ')
          ARR(6) = -999
          IND = -1
       END IF
c
       IF (7 .GT. SIZE) RETURN	   !CAMERA
       ARR(7) = CAM
C
C--FIND GAIN
       IF (8 .GT. SIZE) RETURN
       IF(INDEX(ALABEL(1:),' LO GAIN').GT.0)THEN
         ARR(8) = 0
       ELSE IF(INDEX(ALABEL(1:),' HI GAIN ').GT.0)THEN
         ARR(8) = 1
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -GAIN NOT FOUND',' ')
         ARR(8) = -999
         IND = -1
       END IF
C
C--GET TEMP (WHICH IS ALWAYS WRONG)
      IF (9 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),' VIDICON TEMP ')
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+15:ISTAT+15) .NE. '*') THEN
          READ(ALABEL(ISTAT+14:),'(BN,F4.0)') TMP
          CALL MVE(4,1,TMP,ARR(9),1,1)
      ELSE
        CALL XVMESSAGE( ' ABLE77V2 -TEMP NOT FOUND',' ')
        ARR(9) = -999
        IND = -1
      END IF
C
C--WHICH YEAR WAS IT?
       IF (10 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' SCET ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+6:ISTAT+6) .NE. '*') THEN
          READ(ALABEL(ISTAT+6:),'(BN,I2)') ARR(10)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -YEAR NOT FOUND',' ')
         ARR(10)=-999                  ! NOT FOUND
         IND=-1
       ENDIF
C
C--WHICH DAY WAS IT?
       IF (11 .GT. SIZE) RETURN
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+9:ISTAT+9) .NE. '*') THEN
          READ(ALABEL(ISTAT+9:),'(BN,I3)') ARR(11)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -DAY NOT FOUND',' ')
         ARR(11)=-999       ! NOT FOUND
         IND=-1
       ENDIF
C
C--WHICH HOUR IS IT?
      IF (12 .GT. SIZE) RETURN
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+13:ISTAT+13) .NE. '*') THEN
          READ(ALABEL(ISTAT+13:),'(BN,I2)') ARR(12)
      ELSE
        CALL XVMESSAGE( ' ABLE77V2 -HOUR NOT FOUND',' ')
        ARR(12)=-999       ! NOT FOUND
        IND=-1
      ENDIF
C
C--WHICH MINUTE IS IT?
      IF (13 .GT. SIZE) RETURN
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+16:ISTAT+16) .NE. '*') THEN
          READ(ALABEL(ISTAT+16:),'(BN,I2)') ARR(13)
      ELSE   
        CALL XVMESSAGE(' ABLE77V2 -MINUTE NOT FOUND',' ')
        ARR(13)=-999       ! NOT FOUND
        IND=-1
      ENDIF
C
C--WHICH SECOND IS IT?
      IF (14 .GT. SIZE) RETURN
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+19:ISTAT+19) .NE. '*') THEN
          READ(ALABEL(ISTAT+19:),'(BN,I2)') ARR(14)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -SECOND NOT FOUND',' ')
        ARR(14)=-999       ! NOT FOUND
        IND=-1
      ENDIF
C
C--FILL IN WHICH VOYAGER IT WAS
       IF (19 .GT. SIZE) RETURN
       IF (VGRIND .NE. 0) THEN           ! A SILLY NUMBER IF CORRUPT LABEL
         CALL XVMESSAGE(' ABLE77V2 -VOYAGER FLIGHT # NOT FOUND',' ')
         ARR(19)=-999
         IND=-1
       ELSE
          ARR(19)=VOYAGER
       ENDIF
C
C--GET THE PICNO
      IF (20 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),' PICNO ')
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+7:ISTAT+7).NE.'x'
     +    .and. ALABEL(ISTAT+7:ISTAT+7).NE.'*'))THEN
            CALL MVCL(ALABEL(ISTAT+7:),ARR(20),10)
      else
            call mvcl('!        ',arr(20),10)  !PICNO # NOT FOUND
            IND = -1
      END IF
C
C--GET EDR TAPE AND FILE
      IF (29 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),'IN/')
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+3:ISTAT+3).NE.'x'
     +    .and. ALABEL(ISTAT+3:ISTAT+3).NE.'*'))THEN
            CALL MVCL(ALABEL(ISTAT+3:),ARR(29),6)
      else
            call mvcl('!    ',arr(29),6)          !EDR TAPE NOT FOUND
            IND = -1
      END IF
         
      IF (33 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+10:ISTAT+10).NE.'x'
     +    .and. ALABEL(ISTAT+10:ISTAT+10).NE.'*'))THEN
          READ(ALABEL(ISTAT+10:),'(BN,I2)') ARR(33)
      ELSE
            ARR(33) = -999        !EDR FILE NOT FOUND
            IND = -1
      END IF
C
C--GET OUTPUT TAPE AND FILE
      IF (31 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),' OUT/')
      if(istat.gt.0 .and. (alabel(istat+5:istat+5).ne.'x'
     +    .and. ALABEL(ISTAT+5:ISTAT+5).NE.'*'))THEN
            CALL MVCL(ALABEL(ISTAT+5:),ARR(31),6)
      ELSE
            call mvcl('!    ',arr(31),6)          !output TAPE NOT FOUND
            IND = -1
      ENDIF
      IF (34 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+12:ISTAT+12).NE.'x'
     +    .and. ALABEL(ISTAT+12:ISTAT+12).NE.'*'))THEN
           READ(ALABEL(ISTAT+12:),'(BN,I2)') ARR(34)
      ELSE
              ARR(34) = -999           !output file not found
              IND = -1         
      ENDIF
c
C--WHICH ERT YEAR WAS IT?
      IF (35 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),'ERT ')
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+4:ISTAT+4).NE.'*')THEN
         READ(ALABEL(ISTAT+4:),'(BN,I2)') ARR(35)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT YEAR NOT FOUND',' ')
        ARR(35) = -999
        IND = -1
      END IF
c
C--WHICH ERT DAY WAS IT?
      IF (36 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+7:ISTAT+7).NE.'*')THEN
         READ(ALABEL(ISTAT+7:),'(BN,I3)') ARR(36)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT DAY NOT FOUND',' ')
        ARR(36) = -999
        IND = -1
      END IF
c
C--WHICH ERT HOUR IS IT?
      IF (37 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+12:ISTAT+12).NE.'*')THEN
         READ(ALABEL(ISTAT+11:),'(BN,I2)') ARR(37)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT HOUR NOT FOUND',' ')
        ARR(37) = -999
        IND = -1
      END IF
C
C--WHICH ERT MINUTE IS IT
      IF (38 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+14:ISTAT+14).NE.'*')THEN
         READ(ALABEL(ISTAT+14:),'(BN,I2)') ARR(38)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT MINUTE NOT FOUND',' ')
        ARR(38) = -999
        IND = -1
      END IF
C
C--WHICH ERT SECOND IS IT?
      IF (39 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+17:ISTAT+17).NE.'*')THEN
         READ(ALABEL(ISTAT+17:),'(BN,I2)') ARR(39)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT SECOND NOT FOUND',' ')
        ARR(39) = -999
        IND = -1
      END IF
1     RETURN
      END
