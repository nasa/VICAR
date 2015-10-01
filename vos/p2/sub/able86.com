$!****************************************************************************
$!
$! Build proc for MIPL module able86
$! VPACK Version 1.9, Monday, December 07, 2009, 16:06:41
$!
$! Execute by entering:		$ @able86
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module able86 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to able86.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("able86.imake") .nes. ""
$   then
$      vimake able86
$      purge able86.bld
$   else
$      if F$SEARCH("able86.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake able86
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @able86.bld "STD"
$   else
$      @able86.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create able86.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack able86.com -mixed -
	-s able86.f zable86.c -
	-i able86.imake -
	-t table86.f tzable86.c table86.imake table86.pdf tstable86.pdf -
	-o able86.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create able86.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zable86.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version ZABLE86  (See Fortran Source ABLE86)             */
/************************************************************************/

void  zable86 (ind,unit,buf)  
int   *ind;          /* ind = outputted status indicator   */
int   unit;          /* VICAR  unit #      */
int   *buf;          /* data buffer        */
{
FTN_NAME2(able86, ABLE86) (ind, &unit, buf) ;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create able86.imake
/* Imake file for VICAR subroutine ABLE86   */

#define SUBROUTINE  able86
#define MODULE_LIST  able86.f  zable86.c
#define P2_SUBLIB
#define USES_FORTRAN
#define USES_C
#define FTNINC_LIST  fortport
$ Return
$!#############################################################################
$Test_File:
$ create table86.f
      INCLUDE 'VICMAIN_FOR'
c
C Test program for subroutine ABLE86
C
      Subroutine  MAIN44
      Integer  BUF(83),UNIT

      Call  Xvmessage(' ', ' ')
      Call  Xvmessage(' ******  Testing FORTRAN Version  ******',' ')

      Call  XVUNIT(UNIT,'INP',1,ISTAT,' ')
      Call  XVOPEN(UNIT,ISTAT,' ')
      If (ISTAT.NE.1) Then
         Call Xvmessage(' *** Call NOT Open INPUT', ' ')
         Call Abend
      EndIF
c
      CALL XVP('NPAR',NPAR,ICNT)
      BUF(1) = NPAR
      Call  ABLE86(IND,UNIT,BUF)
      Call  PRNT(4,1,BUF(1),' LABEL TYPE=.')
      IF (BUF(1) .EQ. 1) Call Xvmessage(' *** GROUND CALIB DATA', ' ')
      IF (BUF(1) .EQ. 2) Call Xvmessage(' *** PHASE I DATA', ' ')
      IF (BUF(1) .EQ. 3) Call Xvmessage(' *** PHASE II DATA', ' ')
      Call  PRNT(4,1,BUF(2),' FRAME NO=.')
      Call  PRNT(7,1,BUF(3),' EXPOSURE=.')     !  Floating point 
      Call  PRNT(4,1,BUF(4),' FILTER POSITION=.')
      Call  PRNT(4,1,BUF(5),' FRAME RATE=.')
      Call  PRNT(4,1,BUF(6),' FIBE/MOFIBE=.')
      Call  PRNT(4,1,BUF(7), ' BOOM FLAG =.')
      Call  PRNT(4,1,BUF(8), ' GAIN =.')
      Call  PRNT(4,1,BUF(9), ' MOD10 =.')
      Call  PRNT(4,1,BUF(10),' EVENT YEAR =.')
      Call  PRNT(4,1,BUF(11),' EVENT DAY =.')
      Call  PRNT(4,1,BUF(12),' EVENT HOUR =.')
      Call  PRNT(4,1,BUF(13),' EVENT MINUTE =.')
      Call  PRNT(4,1,BUF(14),' EVENT SECOND =.')
      Call  PRNT(4,1,BUF(15),' EVENT MSEC =.')
      Call  PRNT(4,1,BUF(16),' PARTITION =.')
      Call  PRNT(99,12,BUF(17),' TARGET =.')
      Call  PRNT(7,1,BUF(20),' IOF =.')
      Call  PRNT(7,1,BUF(21),' CONV=.')
      Call  PRNT(7,1,BUF(22),' SORANGE =.')
      Call  PRNT(99,20,BUF(23),' DARK CURRENT FILE=.')
      Call  PRNT(99,20,BUF(28),' RADIOMETRIC FILE=.')
      Call  PRNT(99,20,BUF(33),' BLEMISH FILE=.')
      Call  PRNT(99,20,BUF(38),' SHUTTER-OFFSET FILE=.')
      Call  PRNT(99,8,BUF(43),' EDR TAPE =.')
      Call  PRNT(4,1,BUF(45),' EDR FILE =.')
      Call  PRNT(4,1,BUF(46),' UBWC =.')
      Call  PRNT(99,7,BUF(47),' PICNO=.')
      Call  PRNT(4,1,BUF(49),' SEQNO =.')
      Call  PRNT(7,1,BUF(50),' ENTROPY=.')
      IF (NPAR.GT.50) THEN
         Call  PRNT(99,32,BUF(51),' DARK CURRENT DIRECTORY=.')
         Call  PRNT(99,32,BUF(59),' RADIOMETRIC DIRECTORY=.')
         Call  PRNT(99,32,BUF(67),' BLEMISH DIRECTORY=.')
         Call  PRNT(99,32,BUF(75),' SHUTTER-OFFSET DIRECTORY=.')
         Call  PRNT(4,1,BUF(83),' READOUTMODE=.')
      ENDIF
      Call  PRNT(4,1,IND,'IND =.')
c
c    Testing the C-Bridge
c
      Call XVCLOSE(unit, istat, ' ')
      Call tzable86(NPAR)
      Return
      End
$!-----------------------------------------------------------------------------
$ create tzable86.c
/*  A C-bridge routine, called by TABLE86.F, that tests the C-bridge version
    of ABLE86, Zable86.c    
*/

#include "xvmaininc.h"
#include "ftnbridge.h"
void FTN_NAME(tzable86) (npar)
int *npar;
{
  int  istat, id, iu ;  
  int  zuf[100];

  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge Version  ****** ", " ");

  istat = zvunit(&iu, "INP", 1, 0) ;
  istat = zvopen(iu,"OP","READ", "OPEN_ACT","SA", 0) ;
  if (istat != 1) 
  {
    zvmessage("  Can NOT open Input ...  Abending .......", " ") ;
    zabend();
  }
    zuf[0] = *npar;
    zable86(&id , iu, &zuf[0]) ; 
    zprnt(4, 1, &zuf[0], " LABEL TYPE=.") ;
    if (zuf[0]==1)
       zvmessage(" Ground Calib Data.......", " ") ;
    else if (zuf[0]==2)
       zvmessage(" Phase I Data.......", " ") ;
    else if (zuf[0]==3)
       zvmessage(" Phase II Data.......", " ") ;
    zprnt(4, 1, &zuf[1], " FRAME NO=.") ;
    zprnt(7, 1, &zuf[2], " EXPOSURE=.")  ;   
    zprnt(4, 1, &zuf[3], " FILTER POSITION=.") ;
    zprnt(4, 1, &zuf[4], " FRAME RATE=.") ;
    zprnt(4, 1, &zuf[5], " FIBE/MOFIBE=.");
    zprnt(4, 1, &zuf[6], " BOOM FLAG =.");
    zprnt(4, 1, &zuf[7], " GAIN =.");
    zprnt(4, 1, &zuf[8], " MOD10 =.");
    zprnt(4, 1, &zuf[9], " EVENT YEAR =.");
    zprnt(4, 1, &zuf[10], " EVENT DAY =.");
    zprnt(4, 1, &zuf[11], " EVENT HOUR =.");
    zprnt(4, 1, &zuf[12], " EVENT MINUTE =.");
    zprnt(4, 1, &zuf[13], " EVENT SECOND =.");
    zprnt(4, 1, &zuf[14], " EVENT MSEC =.");
    zprnt(4, 1, &zuf[15], " PARTITION =.");
    zprnt(99, 12, &zuf[16], " TARGET =.");
    zprnt(7, 1, &zuf[19], " IOF =.");
    zprnt(7, 1, &zuf[20], " CONV =.");
    zprnt(7, 1, &zuf[21], " SORANGE =.");

    zprnt(99, 20, &zuf[22], " DARK CURRENT FILE =.");
    zprnt(99, 20, &zuf[27], " RADIOMETRIC FILE =.");
    zprnt(99, 20, &zuf[32], " BLEMISH FILE =.");
    zprnt(99, 20, &zuf[37], " SHUTTER-OFFSET FILE =.");
    zprnt(99,  8, &zuf[42], " EDR TAPE =.");
    zprnt(4, 1, &zuf[44], " EDR FILE =.");
    zprnt(4, 1, &zuf[45], " UBWC =.");
    zprnt(99,  7, &zuf[46], " PICNO=.");
    zprnt(4, 1, &zuf[48], " SEQNO =.");
    zprnt(7, 1, &zuf[49], " ENTROPY =.");
    if (*npar > 50) {
       zprnt(99, 32, &zuf[50], " Dark-Current directory=.");
       zprnt(99, 32, &zuf[58], " Radiometric directory=.");
       zprnt(99, 32, &zuf[66], " Blemish directory=.");
       zprnt(99, 32, &zuf[74], " Shutter-offset directory=.");
       zprnt(4, 1, &zuf[82], " READOUTMODE =.");
       }
    zprnt(4, 1, &id, " IND =.");
 }
$!-----------------------------------------------------------------------------
$ create table86.imake
/* Imake file for Fortran-Test of VICAR subroutine  ABLE86  */

#define PROGRAM table86

#define MODULE_LIST table86.f  tzable86.c
#define MAIN_LANG_FORTRAN
#define TEST
#define USES_FORTRAN
#define USES_C
#define LIB_RTL
#define LIB_TAE       
#define LIB_P2SUB    
$!-----------------------------------------------------------------------------
$ create table86.pdf
Process
PARM  INP  TYPE=STRING
PARM  NPAR TYPE=INTEGER COUNT=1
End-Proc
$!-----------------------------------------------------------------------------
$ create tstable86.pdf
Procedure help=*
Refgbl $echo
refgbl $autousage
refgbl $syschar
Body
local PATH1 TYPE=STRING init="wms_test_work:[testdata."
local PATH2 TYPE=STRING init="mipl.gll]"
local PATH3 TYPE=STRING init="sitod1.test_data.images]"
local PATH4 TYPE=STRING init="gll]" 
local PATH5 TYPE=STRING init="wms_gll:[ssi.rad]"
local PATH6 TYPE=STRING init="wms_test_work:[stream.udr]"
local PATH7 TYPE=STRING init="dev:[ffm059.frfix]"
if ($syschar(1)="UNIX")
	let PATH1="/project/test_work/testdata/"
        let PATH2="mipl/gll/"
        let PATH3="sitod1/test_data/images/"
        let PATH4="gll/"
        let PATH5="/project/gll/ssi/rad/"
        let PATH6="/project/test_work/stream/udr/"
        let PATH7="/home/ffm/frfix/"
      end-if
!
!Let _onfail="continue"
let $autousage="none"
Let $echo="no"
!  
write " "
Write " The Test Data are handled for both VMS and UNIX in this PDF. "
write  " THIS IS A TEST OF MODULE ABLE86"
write  " Test GLL SSI ground-calibration label"
let $echo="yes"
label-list &PATH1"&PATH2"gllsumexp2.tst
table86    &PATH1"&PATH2"gllsumexp2.tst NPAR=50

GALSOS     &PATH1"&PATH2"gllsumexp2.tst gllsumexp2_galsos.out	+
           NOCHECK=NOCHECK                	                +
           DIR=&PATH1"&PATH3"  		           		+   
           CAL=CLRS.100  DC=DCS.100	           		+	
           BLEM=CLR100SUM.BLM		           		+     
           OFFSET=CALIBRATION.SO                           

!
let $echo="no"
write "  Test new GALSOS labels "
write " "
write "  NOTE:  The calibration directory locations are longer than"
write "  32 characters and are shown to be truncated.  This is not "
write "  expected to be an issue in normal operation, thus no fix  "
write "  required to the software as per GMY and HBM request."
!
let $echo="yes"
label-list gllsumexp2_galsos.out
table86    gllsumexp2_galsos.out NPAR=83
!
let $echo="no"
write  " Test Phase I GLL SSI flight label"
let $echo="yes"
label-list  &PATH1"&PATH2"flight2.img
table86     &PATH1"&PATH2"flight2.img  NPAR=83
!
let $echo="no"
write " Test Phase I GLL SSI GALSOSED flight label  "
write " "
write " NOTE:  The file redr.dat contains entropy in the GALSOS task."
write " ABLE86 will not find it there and will report 0.  Able86     "
write " has been coded to look in the first task.  GALSOS and        "
write " BABLABELS have or will be coded to add entropy in the        "
write " first task.  GMY &  HBM."

let $echo="yes"
label-list   &PATH1"&PATH4"redr.dat
table86      &PATH1"&PATH4"redr.dat  NPAR=83
!

let $echo="no"
write "  GALSOS Phase II Data "
let $echo="yes"
!
GALSOS &"PATH6"s0018506300.1  s0018506300_galsos.out    +
       DIR=&PATH5				        +
       CAL=vlts.cal02  DC=2s15.dc		        +
       BLEM=vlt2f.blm02					+
       OFFSET=CALIBRATION.SO02 'nocheck
let $echo="no"
write "  Test new GALSOS labels "
let $echo="yes"
gedrlist s0018506300_galsos.out 
label-list s0018506300_galsos.out
table86    s0018506300_galsos.out NPAR=83
! Test for FR 89118
table86 &"PATH7"ddk1.inp npar=83
if ($syschar(1)="UNIX")
	ush rm  gllsumexp2_galsos.out
        ush rm  s0018506300_galsos.out
else
	dcl del gllsumexp2_galsos.out;*
        dcl del s0018506300_galsos.out;* 
end-if

End-proc
.Title
Test Procedure for subroutine ABLE86.
.Help
The test consists of the following steps:

1. Ground calibration image labels (old IBM format):
   a) List the label via LABEL-LIST
   b) List the label via test program TABLE86, which calls ABLE86 and
      ZABLE86 to obtain the label information.

2. Flight image labels:
   a) List the label via LABEL-LIST
   b) List the label via test program TABLE86, which calls ABLE86 and
      ZABLE86 to obtain the label information.
   c) List the label of a GALSOSed Phase I image.
   d) List the label of a raw and GALSOSed Phase 2 image.
   e) Test C bridge routine zable86.

NOTE: 
   1. See description of ground calibration and flight labels in the GALILEO
      development notebook.
   2.The max size for datafile paths is 32. For this reason, Dark-Current , 
     Radiometric ,Blemish and the Shutter-offset directories may show
     just its first 32 characters in this test pdf.
.End
$ Return
$!#############################################################################
$Other_File:
$ create able86.hlp
1   ABLE86 
    ------

ABLE86 extracts data from the Galileo SSI flight and ground calibration
labels.

2   Calling Sequence  
    ----------------

           CALL ABLE86(IND,UNIT,BUF)
           INTEGER*4 IND,UNIT,BUF(83)
     where
             IND  = return indicator (output)
             UNIT = logical unit number of input label (input)
             BUF  = array containing extracted label data (output)

3   Arguments
    ---------

  IND:  is the return indicator (output integer):
	IND<0 if one or more data items are missing
	IND=0 if all data items are present

  UNIT: specifies the VICAR file unit number of the input label	(input integer).

  BUF:  is an INTEGER*4 array of at least 1 element to receive the extracted
	data. Upon entry, the first word of the array must contain an integer
	value specifying the length of the array (in fullwords).  Upon return,
	the array is filled as shown in Table 1.

4   Operation
    ---------

Before calling ABLE86, the input file (UNIT) must be opened.  Upon return, the
file remains open.

The subroutine first reads all the labels and determines the label type.
Ground-calibration labels are identified by the key "LAB01" and the string
"GLL/SSI".  Flight labels are identified by the keys MISSION=GLL and
SENSOR=SSI.  If the label cannot be identified, the subroutine returns
immediately.

If ABLE86 is unable to successfully extract a data item, it fills the corres-
ponding array element with a "not found" code of -999 for INTEGER*4 items,
-999.0 for REAL*4 items, or "!" for alphanumeric items. 
				TABLE 1

  1      Label type: 1=ground-calib  2=flight(Phase I) 3=flight(PhaseII)
                     0=unknown
  2      Frame number = 100*RIM +MOD91
  3      Exposure time (REAL*4, milliseconds)
  4      Filter position (0-7)
  5      Frame rate (1=2 1/3 sec, 2=8 2/3, 3=30 1/3, 4=60 2/3 5=15 1/6)
  6      Camera flags.  Four digit integer FIBE(Phase I), where
		F=light flood (1=on, 0=off)
		I=clock (1=inverted,0=non-inverted)
		B=blemish protect (1=on, 0=off)
		E=extended-exposure (1=extended, 0=normal)
           e.g. FIBE=101 means light flood off (0), clock inverted (1),
                     blemish protect off(0), extended-exposure (1).
         Six digit integer MOFIBE(Phase II), where
		M=on-chip mosaic (1=on, 0=off)
		O=edited OpNav image (1=on, 0=off)
		F=light flood (1=on, 0=off)
		I=clock (1=inverted,0=non-inverted)
		B=blemish protect (1=on, 0=off)
		E=extended-exposure (1=extended, 0=normal)
  7      Boom flag (2=obscured, 1=possibly obscured, 0=not)
  8      Camera gain-state (1=400K,2=100K,3=40k,4=10k)
  9      10*MOD10 + MOD8 (66 2/3 msec clock and 8 1/3 msec)
 10      Spacecraft-Event-Time year
 11      Spacecraft-Event-Time day-of-year
 12      Spacecraft-Event-Time hour
 13      Spacecraft-Event-Time minute
 14      Spacecraft-Event-Time second
 15      Spacecraft-Event-Time milliseconds
 16      Partition, indicating number of times RIM count was reset (INTEGER*4)
 17-19   Target name (12 ASCII characters)
 20      Conversion factor from dn to reflectance (REAL*4)
 21      Conversion factor from dn to radiance (REAL*4)
 22      Sun to target body distance in km (REAL*4)
 23-27   Dark-current file name (20 ASCII chars)
 28-32   Radiometric file name (20 ASCII chars)
 33-37   Blemish file name (20 ASCII characters)
 38-42   Shutter offset file name (20 ASCII characters)
 43-44   EDR tape id (8 ASCII chars)
 45      EDR file number
 46      Uneven bit-weighting correction (1=on, 0=off)
 47-48   PICNO (7 ASCII characters)
 49      SEQNO (INTEGER*4) Phase II only
 50      Image entropy in bits/pixel (REAL*4)
 51-58   Dark-current file disk and directory name (32 ASCII chars)
 59-66   Radiometric file disk and directory name (32 ASCII chars)
 67-74   Blemish file disk and directory name (32 ASCII chars)
 75-82   Shutter-offset file disk and directory name (32 ASCII chars)
 83      SSI readout mode (0=not applicable, 1=sample, 2=contiguous)
NOTE: All data items are INTEGER*4 unless otherwise specified.

5   History
    -------

  Original Programmer: Joel Mosher  28-MAR-1986
  Current Cognizant Programmer: Gary Yagi
  Source Language: Fortran
  Revisions:
   01 Oct 85  JAM   ...ABLE86 created as conversion of ABLE77
   28 May 88  GMY   ...Add frame-rate.
   27 Mar 89  MAG   ...Added flight label support.
   18 Oct 89  GMY   ...Increased TARGET to 12 characters.  Shuffled reflectance,
		    ...radiance, and uneven-bit-weighting to make room.
		    ...Massive changes to source code and documentation.
   05 Mar 90  GMY   ...Added PARTITION and MSEC items.
   15 May 90  GMY   ...Update for new label format (RIM,MOD91,MOD10,MOD8 are
		    ...separate label items.
   16 May 90  GMY   ...Add check for valid date.
   29 Jan 92  FFM   ...Fix help file for FIBE, use MIPL:[MIPL.GLL]flight2.img
                       instead of nonexist dev:[gmy059]flight.img, add test 
                       case for FIBE, added PCA. (FRs 66699,63262)
   Ported for MSTP UNIX conversion by:   
     W.P. Lee,  July-28-1992
     Code written in FORTRAN, with additioanl C-interface implemented and
     tested
   02 Jul 93  GMY   ...Check for CAL, BLM, SO, DC in TASK=GALSOS (FR 81757)
   03 Aug 93  GMY   ...Added fields for entropy and calibration file directories
   01 Jul 94  RNR   ...(CRI) MSTP S/W Conversion terminator missing in original
                       port. Moved initialization of galsos label index.
   8  Nov 94  FFM   ...Add PhaseII interface
   27 Mar 96  OAM   ...Check for SEQNO. FRs 86961, 89145. Changes to tstable86.pdf
   06 May 96  OAM   ...Update to tstable86.pdf for a filename change.
   14 Aug 96  GMY   Add SSI readout mode (FR 89118).   
   26 Aug 96  SMC   fixed so output buffer element 83 only initializes if 
                      input size is greater or equal to 83 (DFR)
   22 Oct 98  GMY   For Phase 2, extract solar range even if GALSOS has not
                    been run.
   14 Jul 99  GMY   Changed readoutmode so that 0=N/A and 2=contiguous.
$ Return
$!#############################################################################
