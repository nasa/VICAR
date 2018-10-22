c*********************************************************
c
c     Test program for subroutine ABLE77V2
c
c*********************************************************
c
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c
      INTEGER IND, ARR(50), UNIT, NUM(3)
      DATA NUM /39,6,22/
      CHARACTER*132 MSG
c      
      CALL XVUNIT(UNIT,'INP',1,ISTAT,' ')
      CALL XVOPEN(UNIT,ISTAT,' ')
      IF(ISTAT.NE.1)THEN
         CALL XVMESSAGE(' CANT OPEN INPUT',' ')
         CALL ABEND
      ENDIF
c
      CALL XVMESSAGE('*****************FORTRAN CALLABLE*********',' ')
c      
      DO I = 1, 3
         WRITE (MSG,9900) NUM(I)
9900  FORMAT (' ABLE77 TEST RUN--SIZE=',I4,'.')
         CALL XVMESSAGE(MSG(2:28),' ')
         ARR(1) = NUM(I)
         CALL ABLE77V2(IND,UNIT,ARR)
         CALL PRNT(4,1,ARR(1),' label type=.')
         CALL PRNT(4,1,ARR(2),' FDS COUNT=.')
         CALL PRNT(7,1,ARR(3),' EXPOSURE=.')
         CALL PRNT(4,1,ARR(4),' FILTER POSITION=.')
         CALL PRNT(4,1,ARR(5),' SCAN RATE=.')
         CALL PRNT(4,1,ARR(6),' CAMERA SERIAL NUMBER=.')
c
         IF (NUM(I).EQ.6) THEN
            CALL XVMESSAGE('6 VALUES RETURNED, ................',' ')
            GO TO 100
         ENDIF
         CALL PRNT(4,1,ARR(7),' CAMERA =.')
         CALL PRNT(4,1,ARR(8),' GAIN = .')
         CALL PRNT(4,1,ARR(10),' EVENT YEAR =.')
         CALL PRNT(4,1,ARR(11),' EVENT DAY = .')
         CALL PRNT(4,1,ARR(12),' EVENT HOUR =.')
         CALL PRNT(4,1,ARR(13),' EVENT MINUTE =.')
         CALL PRNT(4,1,ARR(14),' EVENT SECOND =.')
         CALL PRNT(4,1,ARR(19),' S/C ID =.')
         CALL PRNT(99,10,ARR(20),'PICNO =.')
c         
         IF (NUM(I).EQ.22) THEN
           CALL XVMESSAGE(' 22 VALUES RETURNED, ................',' ')
           GO TO 100
         ENDIF
         CALL PRNT(99,6,ARR(29),'INPUT TAPE =.')
         CALL PRNT(99,6,ARR(31),'OUTPUT TAPE =.')
         CALL PRNT(4,1,ARR(33),'INPUT FILE =.')
         CALL PRNT(4,1,ARR(34),'OUTPUT FILE =.')
         CALL PRNT(4,1,ARR(35),'ERT YEAR =.')
         CALL PRNT(4,1,ARR(36),'ERT DAY = .')
         CALL PRNT(4,1,ARR(37),'ERT HOUR =.')
         CALL PRNT(4,1,ARR(38),'ERT MINUTE =.')
         CALL PRNT(4,1,ARR(39),'ERT SECOND =.')
100      CALL PRNT(4,1,IND,'IND =.')
      END DO
C
      CALL XVMESSAGE('*****************C  CALLABLE***************',' ')
c      
      DO I = 1, 3
         WRITE (MSG,9990) NUM(I)
9990  FORMAT (' ABLE77 TEST RUN--SIZE=',I4,'.')
         CALL XVMESSAGE(MSG(2:28),' ')
         ARR(1) = NUM(I)
         CALL TZABLE77V2(IND,UNIT,ARR)
         CALL PRNT(4,1,ARR(1),' label type=.')
         CALL PRNT(4,1,ARR(2),' FDS COUNT=.')
         CALL PRNT(7,1,ARR(3),' EXPOSURE=.')
         CALL PRNT(4,1,ARR(4),' FILTER POSITION=.')
         CALL PRNT(4,1,ARR(5),' SCAN RATE=.')
         CALL PRNT(4,1,ARR(6),' CAMERA SERIAL NUMBER=.')
c
         IF (NUM(I).EQ.6) THEN
            CALL XVMESSAGE('6 VALUES RETURNED, ................',' ')
            GO TO 200
         ENDIF
         CALL PRNT(4,1,ARR(7),' CAMERA =.')
         CALL PRNT(4,1,ARR(8),' GAIN = .')
         CALL PRNT(4,1,ARR(10),' EVENT YEAR =.')
         CALL PRNT(4,1,ARR(11),' EVENT DAY = .')
         CALL PRNT(4,1,ARR(12),' EVENT HOUR =.')
         CALL PRNT(4,1,ARR(13),' EVENT MINUTE =.')
         CALL PRNT(4,1,ARR(14),' EVENT SECOND =.')
         CALL PRNT(4,1,ARR(19),' S/C ID =.')
         CALL PRNT(99,10,ARR(20),'PICNO =.')
c         
         IF (NUM(I).EQ.22) THEN
           CALL XVMESSAGE(' 22 VALUES RETURNED, ................',' ')
           GO TO 200
         ENDIF
         CALL PRNT(99,6,ARR(29),'INPUT TAPE =.')
         CALL PRNT(99,6,ARR(31),'OUTPUT TAPE =.')
         CALL PRNT(4,1,ARR(33),'INPUT FILE =.')
         CALL PRNT(4,1,ARR(34),'OUTPUT FILE =.')
         CALL PRNT(4,1,ARR(35),'ERT YEAR =.')
         CALL PRNT(4,1,ARR(36),'ERT DAY = .')
         CALL PRNT(4,1,ARR(37),'ERT HOUR =.')
         CALL PRNT(4,1,ARR(38),'ERT MINUTE =.')
         CALL PRNT(4,1,ARR(39),'ERT SECOND =.')
200      CALL PRNT(4,1,IND,'IND =.')
      END DO
C
      RETURN
      END
