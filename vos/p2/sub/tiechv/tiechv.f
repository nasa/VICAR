c*****************************************************************************
c     Subroutine TIECHV
C
C     CHECK FOR 'TIEPOINTS' OPTION ERRORS AND
C     OPTIONALLY CORRECT DISTORTION
C
C     ARGUMENTS:
C       IND   - INDICATOR ON RETURN (0 IF OK)
C       ITYPE - 0 = NO DISTORTION CORRECTION
C               1 = CORRECT FOR DISTORTION (USE GEOMA ROUTINE GEMDST)
C       PAR   - PARAMETER BUFFER
C               IN ORDER OF LINES,SAMPLES,LAT,LONG,......
C       NPOINT- NUMBER OF POINTS
C       CONV  - GEOMA parameters to be used by TRITRA   
C               for geometric correction of image space
C               tiepoints.
C               The GEOMA parameters are in the format:
C               conv(1)='NAH '
C               conv(2)='    '
C               conv(3)=value (value of 'NAH',number of horizontal areas)
C               conv(4)='NAV '
C               conv(5)='    '
C               conv(6)=value (value of 'NAV',number of vertical areas)
C               conv(7)='TIEP'
C               conv(8)='    '
C               conv(9)=   beginning of GEOMA tiepoint parameters
C
C     HISTORY:
C      13 JUL 93  T. L. TRUONG  PORTED TO UNIX
C      16 JAN 78    ...JJL...   INITIAL RELEASE
c*****************************************************************************
C
      SUBROUTINE TIECHV(IND,ITYPE,PAR,NPOINT,CONV)
      CHARACTER*132 MSG
      REAL*4 PAR(*)
      INTEGER*4 CONV(*)
      IND=0
      J=NPOINT*4
      IF(NPOINT.LT.3) GO TO 140
      IF(NPOINT.GT.20) GO TO 130
      WRITE (MSG,9900) NPOINT
9900  FORMAT (' ',I2,' TIEPOINTS SUPPLIED BY USER')
      CALL XVMESSAGE(MSG(2:30),' ')
131   DO 101 I=1,J,4
      IF(ABS(PAR(I+2)).LT.90.005) GO TO 120
      IND=-1
      CALL XVMESSAGE(' TIEPTS LATITUDE ERROR',' ')
      RETURN
120   IF(ABS(PAR(I+3)).LT.360.005) GO TO 110
      IND=-1
      CALL XVMESSAGE(' TIEPTS LONGITUDE ERROR',' ')
      RETURN
110   IF(ITYPE.EQ.0) GO TO 101
C***************************************
C     CORRECT LINE SAMPLE FOR DISTORTION
C***************************************
      CALL TRITRA(IND,CONV(9),CONV(3)+1,CONV(6)+1,PAR(I),PAR(I+1),1)
101   CONTINUE
      RETURN
140   CALL XVMESSAGE(' ***TOO FEW TIEPOINTS',' ')
      IND=-1
      RETURN
130   CALL XVMESSAGE(' ***TOO MANY TIEPOINTS SUPPLIED--FIRST 20 '//
     +			'USED, REST IGNORED',' ')
      NPOINT=20
      J=NPOINT*4
      GO TO 131
      END
