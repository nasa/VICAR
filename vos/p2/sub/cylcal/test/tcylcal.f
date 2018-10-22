      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C                 VARIABLE DEFINITIONS
C
      CHARACTER*480 BUFFER
C==================================================================
      PI = 3.14159265
      REQ = 6378.15
      FL = 6356.76
      F = 0.5
      CPSI = 120.0
      ZC = 0.0
      ITYPE = 1
C
      RLAT = 500.0
      RLON = 500.0
C
      WRITE(BUFFER,100)ITYPE,RLAT,RLON,FLAG,ZC,CPSI,F,FL,
     1              REQ,PI
      CALL XVMESSAGE(' ',' ')  
      CALL XVMESSAGE(BUFFER(2:80),' ')
      CALL XVMESSAGE(BUFFER(82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(' ',' ')
C
      CALL CYLCAL(ITYPE,RLAT,RLON,ZC,CPSI,F,FL,REQ,PI,FLAG)
C
      WRITE(BUFFER,110)ITYPE,RLAT,RLON,FLAG,ZC,CPSI,F,FL,
     1              REQ,PI
      CALL XVMESSAGE(BUFFER(2:80),' ')
      CALL XVMESSAGE(BUFFER(81:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(' ',' ')
C
      ITYPE = 2
      CALL CYLCAL(ITYPE,RLAT,RLON,ZC,CPSI,F,FL,REQ,PI,FLAG)
C      
      WRITE(BUFFER,120)ITYPE,RLAT,RLON,FLAG,ZC,CPSI,F,FL,
     1              REQ,PI
      CALL XVMESSAGE(BUFFER(2:80),' ')
      CALL XVMESSAGE(BUFFER(81:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(' ',' ')
C      
      RETURN
  100 FORMAT(' INITIAL CONDITIONS FOR LINE/SAMPLE TO LAT/LONG:',
     1       32X,' ITYPE: ',I2,'  LINE:     ',F10.2,'  SAMPLE:    ',
     2       F10.2,'  FLAG: ',F10.2,7X,' ZC: ',F3.1,'  CPSI: ',F7.2,
     3       '  F: ',F5.2,'  FL: ',F8.1,'  REQ: ',F8.1,'  PI: ',F6.4,
     4       '   ')
  110 FORMAT(' AFTER CYLCAL CALL:',61X,
     1       'ITYPE: ',I2,'  LATITUDE: ',F10.2,'  LONGITUDE: ',F10.2,
     2       '  FLAG: ',F10.2,9X,'ZC: ',F3.1,'  CPSI: ',F7.2,'  F: ',
     3       F5.2,'  FL: ',F8.1,'  REQ: ',F8.1,'  PI: ',F6.4,5X)
  120 FORMAT(' AFTER CYLCAL CALL FOR LAT/LONG TO LINE/SAMPLE:',33X,
     1       'ITYPE: ',I2,'  LINE:     ',F10.2,'  SAMPLE:    ',F10.2,
     2       '  FLAG: ',F10.2,9X,'ZC: ',F3.1,'  CPSI: ',F7.2,'  F: ',
     3       F5.2,'  FL: ',F8.1,'  REQ: ',F8.1,'  PI: ',F6.4,5X)
      END
