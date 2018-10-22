C Print the source of the C matrix in following format:
C       CKNAME=FARE  SPKID=NO11  PROGRAM=IBISUPD  GMY059  MIPL  09/22/96
C
      SUBROUTINE CMSOURCE(SBUF,isource)
      INTEGER*4 SBUF(200)	!getspice95 buffer (input)
      INTEGER ISOURCE

      CHARACTER*4 CKID,SPKID,INSTITUTE,CKNAME
      CHARACTER*6 USER,PROGRAM
      CHARACTER*8 DATE
      LOGICAL*1 LBUF(4)
      CHARACTER*80 MSG

      CKNAME='NAIF'     ! default to NAIF
      ISOURCE=7
      CALL MVLC(SBUF(172),ckid,4)
      IF (CKID.EQ.'M901') THEN
        CKNAME='AMOS'
        ISOURCE=6
      END IF
      IF (CKID.EQ.'M902') THEN
        CKNAME='NEAR'
        ISOURCE=5
      END IF
      IF (CKID.EQ.'M903') THEN
        CKNAME='NAV2'
        ISOURCE=4
      END IF
      IF (CKID.EQ.'M904') THEN
        CKNAME='FARE'
        ISOURCE=2
      END IF
      IF (CKID.EQ.'M905') THEN
        CKNAME='NAV'
        ISOURCE=3
      END IF
      IF (CKID.EQ.'M906') THEN
        CKNAME='DAVI'
        ISOURCE=1
      END IF
      CALL MVLC(SBUF(14),spkid,4)
      CALL MVLC(SBUF(174),program,6)
      CALL MVLC(SBUF(177),user,6)
      CALL MVLC(SBUF(189),institute,4)
      CALL MVE(1,4,SBUF(170),lbuf,1,1)
      CALL MVLC(LBUF,date(1:2),2)		!Insert month
      DATE(3:3) = '/'
      CALL MVLC(LBUF(3),date(4:5),2)		!Insert day
      DATE(6:6) = '/'
      CALL MVE(1,4,SBUF(169),lbuf,1,1)		!Insert year
      CALL MVLC(LBUF(3),date(7:8),2)
      WRITE(MSG,110) CKNAME,SPKID,PROGRAM,USER,INSTITUTE,DATE
  110 FORMAT('CKNAME=',A4,'  SPKID=',A4,'  PROGRAM=',A6,2X,A6,2X,
     &  A4,2X,A8)
      CALL XVMESSAGE(MSG,' ')
      RETURN
      END
