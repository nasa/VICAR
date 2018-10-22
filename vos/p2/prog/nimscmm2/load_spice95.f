C Read kernel database (KERNELDB) and load all kernels containing data
C since input ET.  The IDs of the last NAIF CK and SPK loaded are returned.

c  8jul96 -lwk- modified for use by NIMSCMM2:  load SPKs only
c 10aug02 -lwk- removed READONLY & SHARED params from OPEN statement for Linux;
c		(Note that this routine doesn't work in Unix anyway, need to
c		specify SPK explicitly;  so we probably should toss it)

      SUBROUTINE LOAD_SPICE95(ET,spk_id,ind)
      IMPLICIT NONE
      DOUBLE PRECISION ET
c      BYTE CK_ID,SPK_ID(4)		!Output CK and SPK IDs (N001)
      BYTE SPK_ID(4)

c      CHARACTER*4 CK_SN,SK_SN,CK_SOURCE
      CHARACTER*4 SK_SN
c      INTEGER LUN,J,L,IND,NCK,NSK,CK_HANDLE,SK_HANDLE
      INTEGER LUN,J,L,IND,NCK,NSK,SK_HANDLE
      DOUBLE PRECISION BEG_ET,END_ET
      CHARACTER*131 KBUF,MSG
      CHARACTER*20 UTC
      CHARACTER*32 FILENAME
      LOGICAL FAILED,XVPTST
      LOGICAL MIPS/.FALSE./
  100 FORMAT(' ***Err in SCET, KDB record=',I1000)

C     ...Open the kernel database
      CALL GETLUN(LUN)
      OPEN(LUN,FILE='KERNELDB',ERR=900,STATUS='OLD')

      IF (XVPTST('MIPS')) MIPS=.TRUE.
C     ...Read the records in sequentially
      NCK = 0		!Number of applicable C-kernels
      NSK = 0		!Number of applicable SP-kernels
C
      DO 50 J=1,1000
      READ(LUN,101,IOSTAT=IND,END=110) KBUF
  101 FORMAT(A131)
      IF (IND.NE.0) GOTO 910
      UTC = KBUF(46:65)
      CALL UTC2ET(UTC,beg_et)
      UTC = KBUF(68:87)
      CALL UTC2ET(UTC,end_et)
      IF (FAILED()) THEN
         WRITE (MSG,100) J
         CALL XVMESSAGE(MSG,' ')
         CALL RESET1()
         GOTO 50
      ENDIF
      IF (ET.LT.BEG_ET-2000000.0.OR.ET.GT.END_ET+1.D0) GOTO 50
      FILENAME = KBUF(13:21)
      L = INDEX(FILENAME,' ')
      FILENAME(L:32) = ':'//KBUF(23:44)

c      IF (KBUF(6:6).EQ.'2') THEN	!Here if CK
c         CK_SOURCE = KBUF(8:11)
c         IF (MIPS.OR.CK_SOURCE.EQ.'NAIF') THEN
c            NCK = NCK + 1
c            CALL CKLPF(FILENAME,ck_handle)
c            IF (FAILED()) GOTO 920
c            IF (CK_SOURCE.EQ.'NAIF') CK_SN=KBUF(1:4)
c         ENDIF
c      ELSE		!Here if SPK
      IF (KBUF(6:6).ne.'2') THEN
         NSK = NSK + 1
         CALL SPKLEF(FILENAME,sk_handle)
         IF (FAILED()) GOTO 930
         SK_SN = KBUF(1:4)
      ENDIF
   50 CONTINUE

  110 CLOSE(LUN,IOSTAT=IND)
      IF (IND.NE.0) GOTO 990
c      IF (NCK.EQ.0) GOTO 940
      IF (NSK.EQ.0) GOTO 950
c      CALL MVCL(CK_SN,ck_id,4)
      CALL MVCL(SK_SN,spk_id,4)

      write (msg,102) nsk
102   format(' LOAD_SPICE loaded ', i3, ' SP-kernels')
      call xvmessage(msg,' ')

      IND = 1
      RETURN

  900 CALL XVMESSAGE(' ***Error opening KERNELDB',' ')
      IND = -4
      RETURN
  910 CALL XVMESSAGE(' ***Err reading KERNELDB',' ')
      IND = -4
      RETURN
c  920 CALL XVMESSAGE(' ***Error loading C-kernel',' ')
c      IND = -1
c      RETURN
  930 CALL XVMESSAGE(' ***Error loading SP-kernel',' ')
      IND = -2
      RETURN
c  940 CALL XVMESSAGE(' ***No C-kernel within SCLK range',' ')
c      IND = -1
c      RETURN
  950 CALL XVMESSAGE(' ***No SP-kernel within SCLK range',' ')
      IND = -2
      RETURN
  990 CALL XVMESSAGE(' ***Err closing KERNELDB',' ')
      IND = -3
      RETURN
      END
