      INCLUDE 'VICMAIN_FOR'
C 31 OCT 94 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C Voyager radiometric correction program "fixvgr"
C Fixes scale of images that have been radiometrically corrected by
C "ficor77".
C
C	fixvgr INP=A OUT=B user-parameters
C
C	IUNIT = Input unit number (points to A)
C	OUNIT = Output unit number (points to B)
C
C The scale correction factor is retrieved from the Scale Correction
C File (SCF) via subroutine CORRECT.
C
C The factors in the SCF have been updated at least once.  Images which
C have been scaled via "fixvgr" using the old scale factors may be rescaled
C by running "fixvgr" again.  The program determines the old scale factor
C by identifying (by date) the "fixvgr" version used for the previous run
C (VERSION) and the planet ID (IPLAN0) used for that run.  This points
C "fixvgr" to the appropriate scale table in the SCF to undo the previous
C scaling.
C	
      SUBROUTINE MAIN44
      COMMON/C1/TBUF(20000)
      INTEGER*2 TBUF

      INTEGER*4 VGR(39)		!Camera parameters from ABLE77V2
      CHARACTER*256 SCFNAME	!Filename of SCF
      CHARACTER*8 DATE		!Version date of SCF table used.
      CHARACTER*7 PNAME		!Planet of encounter for input image.
      CHARACTER*62 MSG
      LOGICAL XVPTST,NONEG,REPEAT,NOCO
      INTEGER*4 OUNIT,SUNIT,DN,SL,SS,VERSION
      REAL*4 FIC

      DATA NOCO/.FALSE./
      MSG=
     & ' PICTURE MULTIPLIED BY              FIXVGR     2/02/86 VERSION'
      CALL IFMESSAGE('FIXVGR Version 31-OCT-94')
C     ...Open input and output images
      CALL XVUNIT(IUNIT,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVOPEN(OUNIT,IND,'OP','WRITE','OPEN_ACT','SA',
     &            'IO_ACT','SA',' ')
C     ...Open Scale Correction File
      CALL XVP('SCF',SCFNAME,ICNT)	!Get SCF file name
      CALL XVUNIT(SUNIT,'X',1,IND,'U_NAME',SCFNAME,' ')
      CALL XVOPEN(SUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &            'CONVERT','OFF',' ')  !To handle reals & character mix
C     ...Get camera serial number and filter position
      VGR(1) = 39
      CALL ABLE77V2(IND,IUNIT,VGR)	!Get camera parameters from VGR label
      CALL XVP('CAMERA',ICAM,ICNT)	!Get camera id from user parameter
      IF (ICNT.EQ.0) ICAM=VGR(6)	!If not specified, use label value
      CALL XVP('FILTER',IFILT,ICNT)
      IF (ICNT.EQ.0) IFILT=VGR(4)
      CALL PRNT(4,1,ICAM,' Camera=.')
      CALL PRNT(4,1,IFILT,' Filter=.')
      IF (ICAM.LT.4.OR.ICAM.GT.7) GOTO 990	!Invalid camera serial number
      IF (IFILT.LT.0.OR.IFILT.GT.7) GOTO 991	!Invalid filter position
C
C     ...Check if FIXVGR has been run before on this image (repeat?)
      CALL REPCHK(IUNIT,SUNIT,ICAM,IFILT,repeat,scale0,ibm,*999)
C
C     ...Determine planet-of-encounter
      CALL PLANETID(VGR,iplan,pname,*999)
C
C     ...Get "I over F" scale factor from label
      CALL FICOR(IUNIT,TBUF,fic,1)
C     ...Get the scale correction factor
      VERSION = 0			!Set latest version flag
      CALL CORRECT(SUNIT,IFILT,ICAM,IPLAN,version,date,scale,*999)
      RMULT = SCALE*FIC
      IF (REPEAT) THEN 			!If repeat FIXVGR run, undo
          RMULT = RMULT/SCALE0		!previous scale correction.
          IF (IBM.EQ.0) RMULT=RMULT/FIC
      ENDIF

      IF (XVPTST('NOCORREC')) RMULT=1.0
      IF (RMULT.GT.0.99999 .AND. RMULT.LT.1.00001) NOCO=.TRUE.

      CALL PRNT(7,1,RMULT,' Correction factor=.')
      WRITE (MSG(26:30),'(F5.2)') RMULT
      MSG(47:54) = DATE
      CALL XLADD(OUNIT,'HISTORY','COMMENT',MSG,IND,
     &           'FORMAT','STRING','ULEN',62,' ')

      CALL XLADD(OUNIT,'HISTORY','SCALE',PNAME,
     &        IND,'FORMAT','STRING','ULEN',7,' ')

      NONEG = XVPTST('NONEG')
      NUM = 0
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      LL = SL + NL - 1


      DO 100 I=SL,LL		!Loop from starting line to last line
      CALL XVREAD(IUNIT,TBUF,IND,'NSAMPS',NS,'SAMP',SS,'LINE',I,' ')

      IF (.NOT.NOCO) THEN	!Perform correction if requested
          DO J=1,NS
             DN = TBUF(J)*RMULT + .5
             IF (DN.LT.-32768) DN=-32768
             IF (DN.GT.32767)  DN=32767
             TBUF(J)=DN
          ENDDO
      ENDIF

      IF (NONEG) THEN		!Set all negative DN to zero
          DO J=1,NS
             IF (TBUF(J).LT.0) THEN
                TBUF(J)=0
                NUM=NUM+1
             ENDIF
          ENDDO
      ENDIF

  100 CALL XVWRIT(OUNIT,TBUF,IND,'NSAMPS',NS,' ')

      IF (NONEG)
     *    CALL PRNT(4,1,NUM,' Number of negative DNs set to zero=.')
      CALL XVCLOSE(IUNIT,IND,' ')
      CALL XVCLOSE(OUNIT,IND,' ')
      CALL XVMESSAGE('FIXVGR task completed',' ')
      RETURN
C
C     ...Error conditions:
  990 CALL XVMESSAGE('***Invalid camera serial number',' ')
      GOTO 999
  991 CALL XVMESSAGE('***Invalid filter position',' ')
      GOTO 999
  999 CALL XVMESSAGE('***FIXVGR task cancelled',' ')
      CALL ABEND
      END
C Check if FIXVGR has previously been run on this image...
C Inputs: IUNIT,SUNIT,ICAM,IFILT
C Outputs: REPEAT=.TRUE. if FIXVGR has been run before
C          SCALE0 = Scale correction factor used in previous run
C          IBM = 1 if IBM or VICAR1* version of FIXVGR was run
C
      SUBROUTINE REPCHK(IUNIT,SUNIT,ICAM,IFILT,REPEAT,SCALE0,IBM,*)
      INTEGER*4 SUNIT
      LOGICAL REPEAT

      INTEGER*4 VERSION
      CHARACTER*8 DATE
      CHARACTER*7 PNAME
      INTEGER*4 INSTANCES(20)
      CHARACTER*8 TASKS(20)
      CHARACTER*5 KEY
      CHARACTER*7 PLANET(5)
      CHARACTER*72 CLAB, LBUF
      CHARACTER*6 FIXVGR
      DATA PLANET/'NOCORR ','JUPITER','SATURN ','URANUS ','NEPTUNE'/
      BYTE A,B

      FIXVGR='FIXVGR'
      IBM = 0
C
C     ...Look in Vicar2 labels to see if FIXVGR has been run before
      ICNT = 20
      CALL XLHINFO(IUNIT,TASKS,INSTANCES,ICNT,IND,' ')
      DO I=1,ICNT
         IF (TASKS(I).EQ.'FIXVGR  ') GOTO 10
      ENDDO
C     ...If FIXVGR has not been run, check if scale has been fixed
C     ...by FICOR77.
      CALL XLGET(IUNIT,'HISTORY','COMMENT',CLAB,IND,
     &           'FORMAT','STRING',' ')
      IF (IND.LT.0) GOTO 15
      IF (CLAB(37:41).NE.'FICOR') GOTO 15

C     ...Here if image has been scaled by FICOR77 or FIXVGR
10    CALL XLGET(IUNIT,'HISTORY','SCALE',PNAME,IND,
     &           'FORMAT','STRING',' ')              !Get planet name

      IF (IND.EQ.-38) THEN	!If planet name is not in label, then
          IBM = 1		!IBM or VICAR1* version of FIXVGR was run
          VERSION = 1		!so use the first version of the table
          IPLAN = 2		!and the planet is always Jupiter.
          PNAME = PLANET(2)
          CALL CORRECT(SUNIT,IFILT,ICAM,IPLAN,VERSION,DATE,SCALE0,*999)
          GOTO 90
      ENDIF

      IF (IND.LT.0) GOTO 990
C     ...Here if planet name was found in label.
      DO J=1,5
         IF (PNAME.EQ.PLANET(J)) IPLAN=J	!Find planet ID
      ENDDO

      IF (IPLAN.EQ.1) GOTO 90	!If nocorrect, no scaling was done.
C     ...Get date of FIXVGR version from label
      CALL XLGET(IUNIT,'HISTORY','COMMENT',CLAB,IND,
     &           'FORMAT','STRING',' ')
      IF (IND.LT.0) GOTO 991
      DATE = CLAB(47:54)
      VERSION = -1		!Set flag to match date
      CALL CORRECT(SUNIT,IFILT,ICAM,IPLAN,VERSION,DATE,SCALE0,*999)
      GOTO 90
C
C     ...If FIXVGR not found, look in Vicar1* labels:
   15 KEY = 'LAB01'
      CALL XLGET(IUNIT,'HISTORY','NLABS',NLABS,IND,
     &         'HIST',TASKS(1),'INSTANCE',1,'FORMAT','INT',' ')

C     ...Scan labels for 'FIXVGR' only.  Older versions of FICOR77 did
C     ...not fix scale.
      DO I=1,NLABS
        CALL XLGET(IUNIT,'HISTORY',KEY,LBUF,IND,'HIST',TASKS(1),
     &        	   'INSTANCE',1,'FORMAT','STRING',' ')
        DO J=1,66
          IF (LBUF(J:J+5).EQ.'FIXVGR') GOTO 20   !CMPR in baseline
        ENDDO
        A=ICHAR(KEY(5:5))+1
        IF (A.GT.ICHAR('9')) THEN
          A=ICHAR('0')
          B=ICHAR(KEY(4:4))+1
          KEY(4:4)=CHAR(B)
        ENDIF
        KEY(5:5)=CHAR(A)
      ENDDO
      REPEAT = .FALSE.		!Here if FIXVGR not found.
      RETURN

C     ...Here if FIXVGR found in VICAR1* label
   20 IBM = 1			!IBM or VICAR1* version of FIXVGR
      IPLAN = 2			!The planet is always Jupiter	
      VERSION = 1			!and version 1 was used.
      CALL CORRECT(SUNIT,IFILT,ICAM,IPLAN,VERSION,DATE,SCALE0,*999)
      A=ICHAR(KEY(5:5))+1
      IF (A.GT.ICHAR('9')) THEN
        A=ICHAR('0')
        B=ICHAR(KEY(4:4))+1
        KEY(4:4)=CHAR(B)
      ENDIF
      KEY(5:5)=CHAR(A)
      CALL XLGET(IUNIT,'HISTORY',KEY,LBUF,IND,'HIST',TASKS(1),
     &            'INSTANCE',1,'FORMAT','STRING',' ')
      READ (LBUF,9000) RMULT    !Get scale factor
9000  FORMAT(22x,F8.2)
      IF (RMULT.EQ.1.0) IPLAN=1	!If factor is 1.0, nocorrect option
      PNAME = PLANET(IPLAN)
C
C     ...Here if this is a repeat run of FIXVGR
   90 IF (IPLAN.EQ.1) THEN
          CALL xvmessage
     &         ('FIXVGR has already been run with no correction',' ')
          REPEAT = .FALSE.
      ELSE
          CALL xvmessage(
     &     '***FIXVGR has already been run with scale for '//PNAME,' ')
          REPEAT = .TRUE.
      ENDIF
      RETURN

C     ...Error conditions
  990 CALL CHKSTAT(IND,' ***Label error')
      GOTO 999
  991 CALL CHKSTAT(IND,' ***Bad FIXVGR label ***')
  999 RETURN1
      END
C Get the scale correction factor from the Scale Correction File (SCF).
C Each record of the SCF consists of a table of scale factors and the
C version date of the table.
C
C Inputs: UNIT=logical unit number of SCF
C         ICAM=camera serial number
C         IFILT=filter number
C         IPLAN=Planet ID (1=nocorr,2=Jupiter,3=Saturn,
C                            4=Uranus,5=Neptune)
C         VERSION=version number of scale factor table
C         DATE=version date (required if VERSION<0)
C Outputs: scale=scale correction factor
C          If VERSION=0 on input, VERSION and DATE are set to the most
C             recent version.
C          If VERSION<0 on input, then VERSION is set.
C Alternate return on error.
C
C The scale correction factor is retrieved from a table as follows:
C    scale = TABLE(IFILT+1,IPLAN-1,ICAM-3)
C
C The version of the table used is determined by the input values of
C VERSION and DATE:
C    1) If VERSION=0, then the most recent table is used.
C    2) If VERSION>0, then it specifies the version number.
C    3) If VERSION<0, the version is determined by scanning each record
C       of the SCF for a match to DATE.
C
C At least two versions of the table exist.  The original table was
C installed on 3/16/81 and updated 2/02/86.
C
      SUBROUTINE CORRECT(UNIT,IFILT,ICAM,IPLAN,version,date,scale,*)
      INTEGER*4 UNIT,version,TRN_BUF(12),STAT
      CHARACTER*8 date
      CHARACTER*8 LDATE

      REAL*4 TABLE(8,4,4),RTABLE(8,4,4),RDATE(2)

      CHARACTER*8 IDATE

      CALL XVTRANS_INU(TRN_BUF,'REAL','REAL',UNIT,STAT)
      IF (STAT.NE.1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')
      CALL XVGET(UNIT,IND,'NL',nl,'NS',ns,' ')
      IF (NL.LT.1.OR.NS.NE.130) GOTO 990	!If wrong size, bad file.

      IF (VERSION.EQ.0) version=NL		!Use most recent table

      IF (VERSION.GT.0) THEN			!If the version is known,
        CALL XVREAD(UNIT,TABLE,IND,'LINE',VERSION,    !read in table
     &              'NSAMPS',NS-2,' ')
        CALL XVREAD(UNIT,RDATE,IND,'LINE',VERSION,    !read in date
     &              'SAMP',NS-1,'NSAMPS',2,' ')
        CALL XVTRANS(TRN_BUF,TABLE,RTABLE,NS-2) !convert to native format
        scale = RTABLE(IFILT+1,IPLAN-1,ICAM-3)	!scale from the table,
        CALL MVLC(RDATE,LDATE,8)                !convert to character
        DATE=LDATE              		!get the date,
        RETURN					!and exit.
      ENDIF
C
C     ...Here to search for table with matching date
      DO 22 version=1,NL		!Read each record of the SCF
        CALL XVREAD(UNIT,TABLE,IND,'LINE',VERSION,    !read in table
     &              'NSAMPS',NS-2,' ')
        CALL XVREAD(UNIT,RDATE,IND,'LINE',VERSION,    !read in date
     &              'SAMP',NS-1,'NSAMPS',2,' ')
        CALL XVTRANS(TRN_BUF,TABLE,RTABLE,NS-2)  !convert to native format
        CALL MVLC(RDATE,LDATE,8)                 !convert to character
        IDATE=LDATE	                !Get date from record
        IF (DATE.EQ.IDATE) THEN		!If the date matches, then
          scale = RTABLE(IFILT+1,IPLAN-1,ICAM-3)	!get the scale from
          RETURN				!the table and exit.
        ENDIF
   22 CONTINUE

      CALL XVMESSAGE('***Err searching for scale table--CORRECT',' ')
      GOTO 999
  990 CALL XVMESSAGE('***Invalid scale Correction File',' ')
  999 RETURN1
      END
C Determine planet ID and planet name
C Input: IPIC(39)
C Outputs: iplan,pname
C
      SUBROUTINE PLANETID(IPIC,iplan,pname,*)
      INTEGER*4 IPIC(39)
      CHARACTER*7 pname, PLANET(5)
      CHARACTER*16 ctemp
      LOGICAL XVPTST
      DATA PLANET/'NOCORR ','JUPITER','SATURN ','URANUS ','NEPTUNE'/

      ICAM = IPIC(6)
      IF (ICAM.GT.5.) THEN		!If VGR-1 or if nanowatts,
          iplan = 2			!planet is always Jupiter
          GOTO 10
      ENDIF

      DO I=2,5				!Check for user-specified planet
         IF (XVPTST(PLANET(I))) THEN
            iplan=I
            GOTO 10
         ENDIF
      ENDDO
C
C     ...Determine planet-of-encounter using Picno
      DO I=2,5
         call mvlc (IPIC(21),CTEMP,16)
         IF (ctemp(1:1).EQ.PLANET(I)(1:1)) THEN
            iplan = I
            GOTO 10
         ENDIF
      ENDDO

C     ...If no luck, try the Spacecraft-Event-Time
      IYEAR = IPIC(10)
      IF (IYEAR.LT.74) IYEAR=IPIC(35)	!If zero, use Earth-Received-Time
      IF (IYEAR.LT.74) GOTO 990		!Too bad...
      iplan = 2			!Planet is initially Jupiter
      IF (IYEAR.GE.80) iplan=3	!81 and 82 Saturn encounters
      IF (IYEAR.GE.85) iplan=4	!86 Uranus encounter
      IF (IYEAR.GE.88) iplan=5  !89 Neptune encounter

   10 pname = PLANET(iplan)		!Set planet name
      RETURN

  990 CALL XVMESSAGE('***Planet ID is indeterminate',' ')
      CALL XVMESSAGE('***Use PLANET keyword to specify planet',' ')
      RETURN1
      END
