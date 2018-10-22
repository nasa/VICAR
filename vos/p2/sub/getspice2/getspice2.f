C Get GETLABCON label buffer and call GETSPICE4
C
C Notes added July 2011 by lwk:
C These subroutines provide interfaces to routine getspice95, which returns
C SPICE navigation data for a flight-project image.
C Users should be aware that these routines clear the SPICE kernel pool before
C returning, so subsequent calls to SPICE routines will fail unless INIT_SPICE()
C or an equivalent routine is called.
C
      SUBROUTINE GETSPICE2(UNIT,PROVENANCE,buf,ind)
      IMPLICIT NONE
      INTEGER*4 UNIT			!Input image logical unit
      LOGICAL PROVENANCE		!Check provenance flag
      DOUBLE PRECISION BUF(100)	!Returned SPICE buffer
      INTEGER*4 IND			!Returned status

      INTEGER*4 CAMERA_SN,SCLK,DATA(80)
      CHARACTER*5 PROJECT

      CALL GETPROJ(UNIT,project,camera_sn,sclk,ind)	!Get project ID
      CALL GETLABCON(UNIT,PROJECT,data,ind)		!Get label data
      CALL GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)
      RETURN
      END
C Construct fake GETLABCON buffer and use it to call GETSPICE4
C
      SUBROUTINE GETSPICE3(PROJECT,TARGET,CAMERA_SN,SCLK,SCET,
     &		PROVENANCE,buf,ind)
      IMPLICIT NONE
      CHARACTER*5 PROJECT
      CHARACTER*12 TARGET
      INTEGER*4 CAMERA_SN
      INTEGER*4 SCLK
      INTEGER*4 SCET(6)
      LOGICAL PROVENANCE		!Check provenance flag
      DOUBLE PRECISION BUF(100)	!Returned SPICE buffer
      INTEGER*4 IND			!Returned status
      INTEGER*4 LCV                     !Loop Control Variable

      INTEGER*4 DATA(80)		!Fake GETLABCON buffer

      DO LCV = 1,80
         DATA(LCV) = -999
      ENDDO

      DATA(2) = SCLK
      DATA(6) = CAMERA_SN
      CALL MVE(4,6,SCET,data(8),1,1)
      CALL MVCL(TARGET,data(25),12)
      CALL GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)
      RETURN
      END


C Extract data from VICAR label and user parameters to set up call
C to GETSPICE95
C
      SUBROUTINE GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)
      IMPLICIT NONE
      CHARACTER*5 PROJECT
      LOGICAL PROVENANCE		!Check provenance flag
      INTEGER*4 DATA(80)      
      DOUBLE PRECISION BUF(100)	!Returned SPICE buffer
      INTEGER*4 IND,STATUS		!Returned status

      INTEGER*4 I,SC_ID,SYSTEM,MODE,COUNT,DEF
      INTEGER*4 SCET(6),CAMERA_SN,SCLK
      CHARACTER*11 DEFAULT
      CHARACTER*5 CAMERA
      CHARACTER*12 TARGET,USER_TARGET,CDATE
      CHARACTER*4 YEAR,MONTH_DAY,HOUR_MINUTE
      CHARACTER*10 SOURCE
      CHARACTER*6 USERID
      CHARACTER*3 GROUPID
      CHARACTER*10 USR_INFO(10)
      LOGICAL XVPTST

      SCLK = DATA(2)
      CAMERA_SN = DATA(6)

C     ....verify scet_date format
      call scet_update (project, data,status)

      CALL MVE(4,6,DATA(8),scet,1,1)
      CALL MVLC(DATA(25),target,12)
C     ....Get User parameter items
      CALL XVPARM('TARGET',user_target,count,def,0)
      IF (COUNT.EQ.1) CALL UCASE(USER_TARGET,TARGET)
      CALL XVPARM('CKNAME',source,count,def,0)
      CALL UCASE(SOURCE,source)
      CALL XVPARM('CKID',usr_info(9),count,def,0)

      IF (PROVENANCE) THEN
         DO I=1,5
            USR_INFO(I) = '          '
         ENDDO
         CALL XVPARM('INSTITUTE',usr_info(1),count,def,0)
         CALL XVPARM('PURPOSE',usr_info(2),count,def,0)
         CALL XVPARM('PROGRAM',usr_info(3),count,def,0)
         CALL XVPARM('SPKID',usr_info(4),count,def,0)
         CALL XVPARM('REQNUM',usr_info(5),count,def,0)
         CALL XVPARM('CDATE',cdate,count,def,0)
         YEAR = CDATE(1:4)
         MONTH_DAY = CDATE(5:8)
         HOUR_MINUTE = CDATE(9:12)
         USR_INFO(6) = YEAR
         USR_INFO(7) = MONTH_DAY
         USR_INFO(8) = HOUR_MINUTE
         USERID = '*NONE*'
         CALL XVPARM('USERID',userid,count,def,0)
         CALL XVPARM('GROUPID',groupid,count,def,0)
         IF (COUNT.EQ.1) USERID(4:6) = GROUPID(1:3)
         USR_INFO(10) = USERID
      ELSE
         USR_INFO(1) = 'NONE'		!institute
         USR_INFO(2) = 'NONE'		!purpose
         USR_INFO(3) = '*NONE*'		!program
         USR_INFO(4) = 'NONE'		!skid
         USR_INFO(5) = 'NONE'		!reqnum
         USR_INFO(6) = '0000'		!year
         USR_INFO(7) = '0000'		!month,day
         USR_INFO(8) = '0000'		!hour,min
         USR_INFO(10) = '*NONE*'	!userid
      ENDIF

      DO I=1,10
         CALL UCASE(USR_INFO(I),usr_info(i))
      ENDDO

      IF (PROJECT.EQ.'VO-1 ') SC_ID=-27
      IF (PROJECT.EQ.'VO-2 ') SC_ID=-30
      IF (PROJECT.EQ.'VO-1 '.OR.PROJECT.EQ.'VO-2 ') THEN
         CALL XVMESSAGE('***SPICE data not available for VO',' ')
         IND = -1
         RETURN
      ENDIF

      IF (PROJECT.EQ.'VGR-1') THEN		!Voyager 1
         SC_ID = -31
         IF (CAMERA_SN.NE.6 .AND. CAMERA_SN.NE.7) GOTO 990
         CAMERA = 'ISSW'
         IF (CAMERA_SN.EQ.7) CAMERA='ISSN'
         SYSTEM = 2
      ENDIF

      IF (PROJECT.EQ.'VGR-2') THEN		!Voyager 2
         SC_ID = -32
         IF (CAMERA_SN.NE.4 .AND. CAMERA_SN.NE.5) GOTO 990
         CAMERA = 'ISSW'
         IF (CAMERA_SN.EQ.5) CAMERA='ISSN'
         SYSTEM = 2
      ENDIF

      IF (PROJECT.EQ.'GLL  ') THEN
         SC_ID = -77
         IF (CAMERA_SN.NE.1 .AND. CAMERA_SN.NE.2) GOTO 990
         CAMERA = 'SSI1'
         IF (CAMERA_SN.EQ.2) CAMERA='SSI2'
         SYSTEM = 1
      ENDIF

      IF (PROJECT.EQ.'CASSI') THEN
         SC_ID = -82
         CAMERA = '?'
         IF (CAMERA_SN.EQ.1) CAMERA='ISSN'
         IF (CAMERA_SN.EQ.21) CAMERA='ISN2'
         IF (CAMERA_SN.EQ.41) CAMERA='ISN4'
         IF (CAMERA_SN.EQ.2) CAMERA='ISSW'
         IF (CAMERA_SN.EQ.22) CAMERA='ISW2'
         IF (CAMERA_SN.EQ.42) CAMERA='ISW4'
         IF (CAMERA.EQ.'?') GOTO 990
         SYSTEM = 1
      ENDIF

      IF (XVPTST('REMOTE')) THEN
         MODE=1
      ELSE IF (XVPTST('LOCAL')) THEN
         MODE = 0
      ELSE
         CALL XGETENV_VIC('DEFAULTSPICE',default)
         MODE = 0
         IF (DEFAULT.EQ.'REMOTESPICE') MODE=1 
      ENDIF          

      CALL GETSPICE95(MODE,SC_ID,CAMERA,SCET,TARGET,
     &          SYSTEM,SOURCE,USR_INFO,buf,ind)
      IF (IND.NE.1) CALL PRNT(4,1,IND,'***SPICE error=.')
      RETURN

  990 CALL XVMESSAGE('***Invalid camera serial number',' ')
      IND = -1		!Bad user input
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FORTRAN bridge for zgetspice3 and zgetspice4, second stage
C
      SUBROUTINE XGETSPICE4(PROJECT,N,PROVENANCE,DATA,buf,ind)
      BYTE PROJECT(1)
      INTEGER*4 N,PROVENANCE,DATA(80),IND
      REAL*8 BUF(100)

      CHARACTER*5 CPROJECT

      CALL MVLC(PROJECT,cproject,n)

      CALL GETSPICE4(CPROJECT,PROVENANCE,DATA,buf,ind)
      RETURN
      END
