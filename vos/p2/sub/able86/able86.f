C      include 'VICMAIN_FOR'
C
      Subroutine ABLE86(IND,UNIT,BUF)
      INTEGER*4 UNIT		!Input unit number of image
      INTEGER*4 BUF(*)		!Array of label items returned
      INTEGER   IND
      Real*4  XYZ
      Character*20  CXYZ
      Integer*4  SIZE,OFF,IJK
      INTEGER*4 INSTANCE(30)
      CHARACTER*9 TASKS(30)

      Character*32     BLANKS 
      Character*7200   ALABEL
      Character*3    CMon
      REAL*4 R999 
      CHARACTER*100  SSS       
      INTEGER*4 IR999
 
      CHARACTER*40 ETYPE    ! Encoding type, Integer Cosine Transform? 
      LOGICAL PHASEII
      EQUIVALENCE(R999,IR999)

      DATA BLANKS /'!                               '/, R999 /-999/

      SIZE = BUF(1)              ! Caller passes length of BUF
      If (SIZE.LT.1) Then
         Call Xvmessage(' ***ABLE86 -size is less than 1', ' ')
         CALL Abend
      ENDIF

      If (SIZE.GT.83) SIZE=83
      IND = 0     !  INDicator initially normal
c
C     ....Initialize all label items in BUF(*) as invalid
      BUF(1) = 0
      Do I=2,SIZE       ! Typical SIZE ~= 50 for FLIGHT Label
         BUF(I) = -999
      EndDO

      IF (SIZE.GE.83) BUF(83) = 0
c
c   Defaulted  Floats and Char arrays 
c
      If (SIZE.GE. 3) BUF(3) = IR999                ! Exposure,  Float
      If (SIZE.GE.17) Call MVCL(BLANKS,BUF(17),12)  ! Target name
      If (SIZE.GE.20) BUF(20)= IR999                 ! IOF,       Float
      If (SIZE.GE.21) BUF(21)= IR999                 ! CNV,       Float
      If (SIZE.GE.22) BUF(22)= IR999                 ! Sol_Range, Float
      If (SIZE.GE.23) Call MVCL(BLANKS,BUF(23),20)
      If (SIZE.GE.28) Call MVCL(BLANKS,BUF(28),20)
      If (SIZE.GE.33) Call MVCL(BLANKS,BUF(33),20)
      If (SIZE.GE.38) Call MVCL(BLANKS,BUF(38),20)
      If (SIZE.GE.43) Call MVCL(BLANKS,BUF(43),8)
      If (SIZE.GE.47) Call MVCL(BLANKS,BUF(47),8)
      If (SIZE.GE.50) BUF(50)= IR999
      If (SIZE.GE.58) Call MVCL(BLANKS,BUF(51),32)
      If (SIZE.GE.66) Call MVCL(BLANKS,BUF(59),32)
      If (SIZE.GE.74) Call MVCL(BLANKS,BUF(67),32)
      If (SIZE.GE.82) Call MVCL(BLANKS,BUF(75),32)

      NINSTANCE = 30
      Call XLHINFO(UNIT,TASKS,INSTANCE,NINSTANCE,ISTAT,' ')
c
C  ...Read label into ALABEL, a 7200-Byte String, where 7200 = 72 * 100
      Call  VIC1LAB(UNIT,istat,nlabs,alabel,20)	  ! IBM or NOT ?
      Call  CHKSTAT(ISTAT,' ***ABLE86 ERR,ISTAT=',1,ISTAT,1)
      PHASEII = .FALSE.
      If (NLABS.EQ.0) GoTo  200           ! NON-IBM, goto flight_label
c
c  Ground Calibration, search for GLL ID
      OFF = INDEX(ALABEL(1:),'GLL/S')	  ! GLL ?
      If (OFF.EQ.0) Return                ! If not GLL, return
c
C   ....Here if GLL SSI ground calibration label (in IBM Format)
      BUF(1) = 1       
c
C     ....Get frame number
      If (SIZE.LT.2) Return              ! Less than 8 Bytes
      I = INDEX(ALABEL(OFF:),' FRAME')   ! Search fo FRAME starting from GLL/S
      If (I.NE.0) Then
c 
c  Put Label (Char String) into INT*4 PAR, 20 Bytes 
         Read (ALABEL(OFF+I+5:),'(BN,I2)') BUF(2)
      Else
         Call Xvmessage(' ***ABLE86 -err in frame number',' ')
         IND = -1
      EndIF
c
C     ....Get exposure time(msec)
      If (SIZE.LT.3) Return
      I = INDEX(ALABEL(OFF:),'EXP=')
      If (I.NE.0) Then
c
        Read (ALABEL(OFF+I+3:),'(BN,F7.1)') XYZ
        Call MVLC(XYZ,CXYZ,4)
        Call MVCL(CXYZ,BUF(3),4)
      Else
         Call Xvmessage(' ***ABLE86 -expo time error', ' ')
         IND = -1
      EndIF
c
C     ....Get filter position
      If (SIZE.LT.4) Return
      I = INDEX(ALABEL(OFF:),'  FILTER=')
      If (I.NE.0) Then
c  Convert ASCII integer to Decimal value BUF(4) 
         Read (ALABEL(OFF+I+8:),'(BN,I1)') BUF(4) 
      Else
         Call Xvmessage(' ***ABLE86 -filter position error', ' ')
         IND = -1
      EndIF
c
C     ....Get frame-rate
      If (SIZE.LT.5) Return
      I = INDEX(ALABEL(OFF:),' FR.RATE=')  
      If (I.NE.0) Then
         Read (ALABEL(OFF+I+8:),'(BN,I1)') IFR
         BUF(5) = 4                     !66 2/3 sec
         If (IFR.EQ.2) BUF(5)=1	        ! 2 1/3 sec
         If (IFR.EQ.8) BUF(5)=2	        ! 8 2/3 sec
         If (IFR.EQ.3) BUF(5)=3	        !33 1/3 sec
      Else
        Call Xvmessage(' ***ABLE86 -Scan-rate error', ' ')
        IND = -1
      ENDIF

      If (SIZE.LT.6) Return
      IND = -1			!No FIBE in calibration label
      If (SIZE.LT.7) Return
c
C     ....Get gain state
      If (SIZE.LT.8) Return
      I = INDEX(ALABEL(OFF:),' GAIN=')
      If (I.NE.0) Then
         Read (ALABEL(OFF+I+5:),'(BN,I1)') BUF(8) 
      Else
         Call Xvmessage(' ***ABLE86 gain error', ' ')
      EndIF

      IF (Size.LT.9) Return     !  No MOD10 for ground calibration
C     ....Get year
      If (SIZE.LT.10) Return
      I = INDEX(ALABEL(OFF:),' LEVEL=')
      If (I.EQ.0) Return
      Do K=1,30	                !  Replace ":" with blanks
         J = OFF + I + K
         If (ALABEL(J:J).EQ.':') ALABEL(J:J)=' '
      EndDO
      Read (ALABEL(OFF+I+36:),'(BN,I4)') BUF(10)   ! year = 38th after ' LEVEL'

c
C     ....Get day-of-year (should it be day-of-month ?)
      If (SIZE.LT.11) Return
      Read (ALABEL(OFF+I+32:),'(BN,I2)') IJK       ! IJK is day-of-month
      Read (ALABEL(OFF+I+27:),'(BN,A3)') CMon      ! Month in Character 
      Mon = 13    !  Default 'BAD' flag 
      If (Cmon(1:3) .Eq. 'JAN') Mon =  1
      If (Cmon(1:3) .Eq. 'FEB') Mon =  2
      If (Cmon(1:3) .Eq. 'MAR') Mon =  3
      If (Cmon(1:3) .Eq. 'APR') Mon =  4
      If (Cmon(1:3) .Eq. 'MAY') Mon =  5
      If (Cmon(1:3) .Eq. 'JUN') Mon =  6
      If (Cmon(1:3) .Eq. 'JUL') Mon =  7
      If (Cmon(1:3) .Eq. 'AUG') Mon =  8
      If (Cmon(1:3) .Eq. 'SEP') Mon =  9
      If (Cmon(1:3) .Eq. 'OCT') Mon = 10
      If (Cmon(1:3) .Eq. 'NOV') Mon = 11
      If (Cmon(1:3) .Eq. 'DEC') Mon = 12
      Nyear  = Buf(10)
c
c     Calculate Day-of-Year & put in BUF(11)
      Call JDAY(Mon, IJK, Nyear, Nout)
      BUF(11) = Nout
c
C     ....Get time-of-day, Hour-Minute-Second
      If (SIZE.LT.12)  Return
      Read (ALABEL(OFF+I+17:),'(BN,I2)') BUF(12)     !  Hour 
      If (SIZE.LT.13)  Return
      Read (ALABEL(OFF+I+20:),'(BN,I2)') BUF(13)     !  Minute
c
      If (SIZE.LT.14)  Return
      Read (ALABEL(OFF+I+23:),'(BN,I2)') BUF(14)     !  Second
      GOTO 300
c
c     !!!   Now,  NON-IBM Label !!!
200   Call XLGET(UNIT,'HISTORY','MISSION',SSS,ISTAT,
     *           'FORMAT','STRING','HIST',TASKS(1),' ')
      If (SSS(1:7).NE.'GALILEO') Return         ! Return if not Galileo
      Call XLGET(UNIT,'HISTORY','SENSOR',SSS,ISTAT,
     *          'FORMAT','STRING','HIST',TASKS(1),' ')
      If (SSS(1:3).NE.'SSI') Return             !  Return if not SSI

      Call XLGET(UNIT,'HISTORY','ENCODING_TYPE',ETYPE,ISTAT,
     *          'FORMAT','STRING','HIST',TASKS(1),' ')
      If (ISTAT .EQ. 1)
     *    PHASEII = .TRUE.
C     ....Set FLAG = 2, or 3 if Galileo SSI flight label
      If (PHASEII) Then
          BUF(1) = 3
      Else
          BUF(1) = 2
      EndIF
C     ....Get spacecraft clock (=100*RIM + MOD91)
      If (SIZE.LT.2) Return
      Call XLGET(UNIT,'HISTORY','RIM',BUF(2),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      Call XLGET(UNIT,'HISTORY','MOD91',MOD91,ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.EQ.1) Then
	 BUF(2)=100*BUF(2) + MOD91
      Else
         IND = -1
      EndIF
C     ....Get exposure time (msec)
      If (SIZE.LT.3) Return      
      CaLL XLGET(UNIT,'HISTORY','EXP',BUF(3),ISTAT,
     *           'FORMAT','REAL','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get filter position (0-7)
      If (SIZE.LT.4) Return
      Call XLGET(UNIT,'HISTORY','FILTER',BUF(4),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get frame rate (1-4)
      If (SIZE.LT.5) Return
      Call XLGET(UNIT,'HISTORY','RATE',BUF(5),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get flood,invert/non-invert,blem-protect,extended-exposure(PhaseI)
C     ....Get on-chip mosaic, edited OpNav image, flood, invert/non-invert,
C     ....blem-protect, extended-exposure(PhaseII)
      If (SIZE.LT.6) Return
      If (PHASEII) Then
         Call XLGET(UNIT,'HISTORY','MOFIBE',SSS,ISTAT,
     *              'FORMAT','STRING','HIST',TASKS(1),' ')
      Else
         Call XLGET(UNIT,'HISTORY','FIBE',SSS,ISTAT,
     *              'FORMAT','STRING','HIST',TASKS(1),' ')
      EndIF
      If (ISTAT.EQ.1) Then
         If (PHASEII) Then
            Read (SSS(1:7),'(BN,I6)') BUF(6)     !  MOFIBE
         Else
            Read (SSS(1:5),'(BN,I4)') BUF(6)     !  FIBE
         EndIF
      Else
         IND = -1
      EndIF
C     ....Get boom-obscuration flag
      If (SIZE.LT.7)  Return
      Call  XLGET(UNIT,'HISTORY','BOOM',SSS,ISTAT,
     *            'FORMAT','STRING','HIST',TASKS(1),' ')
      If  (ISTAT.EQ.1) Then
         If (SSS(1:1).EQ.'P') BUF(7)=1
         If (SSS(1:1).EQ.'N') BUF(7)=0
         If (SSS(1:1).EQ.'V') BUF(7)=2
      Else
         IND = -1
      EndIF
C     ....Get camera gain-state
      If (SIZE.LT.8) Return
      Call XLGET(UNIT,'HISTORY','GAIN',BUF(8),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get 10*MOD10 + MOD8
      If (SIZE.LT.9) Return
      Call XLGET(UNIT,'HISTORY','MOD10',BUF(9),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      Call XLGET(UNIT,'HISTORY','MOD8',MOD8,ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.EQ.1) Then
	 BUF(9)=10*BUF(9) + MOD8
      Else
         IND = -1
      EndIF
C     ....Get spacecraft-event time
      If (SIZE.LT.10) Return
      Call XLGET(UNIT,'HISTORY','SCETYEAR',BUF(10),ISTAT,
     *		'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      If (SIZE.LT.11) Return
      Call XLGET(UNIT,'HISTORY','SCETDAY',BUF(11),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      If (SIZE.LT.12) Return
      Call XLGET(UNIT,'HISTORY','SCETHOUR',BUF(12),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      If (SIZE.LT.13) Return
      Call XLGET(UNIT,'HISTORY','SCETMIN',BUF(13),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      If (SIZE.LT.14) Return
      Call XLGET(UNIT,'HISTORY','SCETSEC',BUF(14),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
      Call XLGET(UNIT,'HISTORY','SCETMSEC',BUF(15),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get RIM count partition number
      Call XLGET(UNIT,'HISTORY','PARTITION',BUF(16),ISTAT,
     *           'FORMAT','INT','HIST',TASKS(1),' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get picture number
      If (SIZE.LT.16) Return
      Call XLGET(UNIT,'HISTORY','PICNO',SSS,ISTAT,'HIST',TASKS(1),
     *          'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Then
         Call  MVCL(SSS,BUF(47),length)
      Else
         IND = -1
      EndIF
C     ....Target name
      If (SIZE.LT.17) Return
      Call  XLGET(UNIT,'HISTORY','TARGET',SSS,ISTAT,'HIST',TASKS(1),
     *      'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Then
         Call MVCL(SSS,BUF(17),length)
      Else
         IND = -1
      EndIF
      If (SIZE.LT.20)  Return
C     ....Solar range
      If (SIZE.GE.22 .AND. PHASEII)
     *		Call XLGET(UNIT,'HISTORY','SOLRANGE',BUF(22),
     *		ISTAT,'FORMAT','REAL','HIST',TASKS(1),' ')

C         Scan for GALSOS task
  300 IG = 0	!Index of GALSOS task
      JG = 1			!First assume info is in FIRST task
      DO J=1,NINSTANCE
         IF (TASKS(J).EQ.'GALSOS') IG=J
      ENDDO
      IF (IG.EQ.0) GOTO 400	! Skip if no GALSOS info
C     ....DN-to-reflectance conversion factor
      Call XLGET(UNIT,'HISTORY','IOF',BUF(20),ISTAT,
     *           'FORMAT','REAL','HIST',TASKS(1),' ')
C     ....DN-to-radiance conversion factor
      If (SIZE.LT.21)  Return
      Call  XLGET(UNIT,'HISTORY','CNV',BUF(21),ISTAT,
     *            'FORMAT','REAL','HIST',TASKS(1),' ')
      If (SIZE.LT.22) Return
      If (.NOT.PHASEII) Call XLGET(UNIT,'HISTORY','SOLRANGE',BUF(22),
     *		ISTAT,'FORMAT','REAL','HIST',TASKS(1),' ')
      If (SIZE.LT.27) Return
C     ....The GALSOS cal files are recorded in the first task or the
C     ....GALSOS task (later versions of GALSOS).  Determine where
C     ....they are stored and use JG to point to them.
C     ....Dark-current filename
      Call XLGET(UNIT,'HISTORY','DC',SSS,ISTAT,'HIST',
     *	TASKS(1),'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.NE.1) THEN
         Call XLGET(UNIT,'HISTORY','DC',SSS,ISTAT,'HIST',
     *     TASKS(IG),'FORMAT','STRING','LENGTH',length,' ')
         IF (ISTAT.EQ.1) JG=IG	!Info is in GALSOS task
      ENDIF
      If (ISTAT.EQ.1) Call MVCL(SSS,BUF(23),length)
C     ....Get radiometric filename                    
      If (SIZE.LT.32) Return
      Call XLGET(UNIT,'HISTORY','CAL',SSS,ISTAT,'HIST',TASKS(JG),
     *		 'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Call MVCL(SSS,BUF(28),length)
C     ....Get blemish filename
      If (SIZE.LT.37) Return
      Call XLGET(UNIT,'HISTORY','BLM',SSS,ISTAT,'HIST',TASKS(JG),
     *		'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Call MVCL(SSS,BUF(33),length)
C     ....Get shutter-offset filename
      If (SIZE.LT.42) Return
      Call XLGET(UNIT,'HISTORY','SO',SSS,ISTAT,'HIST',TASKS(JG),
     *		'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Call MVCL(SSS,BUF(38),length)
C     ....Get EDR tape-ID
  400 If (SIZE.LT.44) Return
      Call XLGET(UNIT,'HISTORY','EDRTAPE',SSS,ISTAT,'HIST',
     *           TASKS(1),'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Call MVCL(SSS,BUF(43),length)
C     ....Get EDR tape file number
      If (SIZE.LT.45) Return
      Call XLGET(UNIT,'HISTORY','EDRFILE',BUF(45),ISTAT,
     *          'FORMAT','INT','HIST',TASKS(1),' ')
C     ....Get uneven-bit-weighting correction flag
      If (SIZE.LT.46) Return
      Call XLGET(UNIT,'HISTORY','UBWC',SSS,ISTAT,
     *          'FORMAT','STRING','HIST',TASKS(1),' ')
      If (ISTAT.EQ.1) Then
        If (SSS(1:3).EQ.'OFF') BUF(46)=0
        If (SSS(1:2).EQ.'ON')  BUF(46)=1
      EndIF

C     ....Store SEQNO for PHASEII
      If (SIZE.LT.49) Return
      If (PHASEII) Then
      Call XLGET(UNIT,'HISTORY','SEQNO',buf(49),ISTAT,
     *             'FORMAT','INT','HIST',TASKS(1),' ')
      EndIF

C     ....Get image entropy
      If (SIZE.LT.50) Return
      Call XLGET(UNIT,'HISTORY','ENTROPY',BUF(50),ISTAT,'HIST',
     *		TASKS(1),'FORMAT','REAL',' ')
      If (ISTAT.NE.1) IND=-1
C     ....Get Dark-Current file disk and directory name
      If (SIZE.LT.58) Return
      Call XLGET(UNIT,'HISTORY','DIRDC',SSS,ISTAT,'HIST',TASKS(JG),
     *		'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Then
         Call MVCL(SSS,BUF(51),32)
      Else
         IND = -1
      EndIF
C     ....Get Radiometric file disk and directory name
      If (SIZE.LT.66) Return
      Call XLGET(UNIT,'HISTORY','DIRCAL',SSS,ISTAT,'HIST',TASKS(JG),
     *		'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Then
         Call MVCL(SSS,BUF(59),32)
      Else
         IND = -1
      EndIF
C     ....Get Blemish file disk and directory name
      If (SIZE.LT.74) Return
      Call XLGET(UNIT,'HISTORY','DIRBLM',SSS,ISTAT,'HIST',TASKS(JG),
     *		'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Then
         Call MVCL(SSS,BUF(67),32)
      Else
         IND = -1
      EndIF
C     ....Get Shutter-Offset file disk and directory name
      If (SIZE.LT.82) Return
      Call XLGET(UNIT,'HISTORY','DIROFF',SSS,ISTAT,'HIST',TASKS(JG),
     *		'FORMAT','STRING','LENGTH',length,' ')
      If (ISTAT.EQ.1) Then
         Call MVCL(SSS,BUF(75),32)
      Else
         IND = -1
      EndIF
C     ....Get SSI readout mode
      If (SIZE.LT.83) Return
      BUF(83) = 0
      IF (PHASEII) THEN
         Call XLGET(UNIT,'HISTORY','TLMFMT',SSS,ISTAT,'HIST',
     &      TASKS(1),'FORMAT','STRING','LENGTH',length,' ')
         If (ISTAT.EQ.1) Then
            IF (SSS.EQ.'HCA'.OR.SSS.EQ.'HMA') THEN
               Call XLGET(UNIT,'HISTORY','READOUTMODE',SSS,ISTAT,
     &		  'HIST',
     &		  TASKS(1),'FORMAT','STRING','LENGTH',length,' ')
               If (ISTAT.EQ.1) THEN
                  BUF(83) = 2
                  IF (SSS.EQ.'SAMPLE') BUF(83)=1
               Else
                  IND = -1
               EndIF
            ENDIF
         ENDIF
      ENDIF
      Return
      End
