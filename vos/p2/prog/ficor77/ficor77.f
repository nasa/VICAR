	include 'VICMAIN_FOR'
C VICAR applications program FICOR77
C
C FICOR77 removes vidicon camera system induced radiometric
C distortion from Voyager images.
C
C Notes useful to the programmer are in FICOR77.DOC which may be generated
C by: vunpack ficor77.com DOC
C
C     ficor77 INP=(PIC,C1,C2,...,DC1,DC2,...) OUT=OPIC user-parameters...
C
      SUBROUTINE MAIN44
      IMPLICIT NONE

C      COMMON/C1/WORK(10000)		!A large work area

      INTEGER*4 NC,CNS			!NOTE: Now in label
      REAL*4 EMAX,LTBL(10)
  
      REAL*4 DTBL(9)			!Differences of LTBL
      REAL*4 LUT(256)			!Look-up tables for scaling input DNs
      INTEGER*4 IPIC(39),IDARK(10)	!Camera params for PIC,DC
      INTEGER*4 MODE, MODEDC            !Camera mode and dark-current camera mode
      INTEGER*4 SLO,SSO,IBUG,IUNIT,NLO,NSO,NSI,IPLAN,OUNIT,MAXDN
      INTEGER*4 NONEGS,ICAM,IUNITC,IFLT,IUNITD,IGAIN,IDCGAIN
      INTEGER*4 IND,NLC,NSC,ISPEED,IFILT,A1,A2,BADSCALE
      INTEGER*4 MSP,MUP,NSP,NNP,ITOT,IFDS,L
      REAL*4 FSCALE,DIV,DCSCALE
      CHARACTER*7 PNAME			!Planet-of-encounter
      CHARACTER*8 DATE			!Version date of SCF
      LOGICAL XVPTST
C
      CALL IFMESSAGE('FICOR77 version Sep 3 2015')
C
      IBUG = 0
      IF (XVPTST('DBUG')) IBUG=1	!Set flag for diagnostic messages
C
C     ...Open input and get frame information
      CALL IPOPEN(iunit,ipic,mode,slo,sso,nlo,nso,nsi,*999)
C
C     ...Get planet ID and planet name
      CALL PLANETID(IPIC,iplan,pname,*999)
C
C     ...Open output picture and get output parameters
      CALL OPOPEN(NSO,ounit,maxdn,nonegs)
C
C     ...Get user-specified camera parameters
      MODE = 0			! initialize to invalid value
      MODEDC = 0		! initialize to invalid value
      CALL CPARAM(ipic,idark,mode,modedc,*999)
      ICAM = IPIC(6)		!Camera serial number
      IFILT = IPIC(4)		!Filter position
C
C     ...Search input file list for calibration and DC files
      CALL FSEARCH(IPIC,MODE,idark,modedc,iunitc,iunitd,div,*999)
C
C     ...Scale DNs of input image and DC.
      IGAIN = IPIC(8)		!Gain-state (0=low,1=high)
      IDCGAIN = IDARK(8)	!Dark-current gain-state
      CALL IPSCALE(ICAM,IGAIN,IDCGAIN,DIV,IBUG,lut,dcscale)
C
      CALL XVGET(IUNITC,IND,'NL',NLC,'NS',NSC,' ')  !Get size of Cal File
      CALL XLGET(IUNITC,'HISTORY','NUM_DATA_PTS',NC,IND,
     &           'FORMAT','INT',' ')
      CALL XLGET(IUNITC,'HISTORY','NSAMPS_REC',CNS,IND,
     &           'FORMAT','INT',' ')
      CALL XLGET(IUNITC,'HISTORY','DEF_SCALE_FACTOR',EMAX,IND,
     &           'FORMAT','REAL',' ')
      CALL XLGET(IUNITC,'HISTORY','LUM_VALUES',LTBL,IND,
     &           'FORMAT','REAL','NELEMENT',10,' ')
      CNS = NSC/NC	!number of samples per calibration record
C
C     ...Scale DNs of output image
      ISPEED = IPIC(3)		!Shutter speed (msec,REAL*4)
      CALL OPSCALE(ICAM,IFILT,IPLAN,ISPEED,MAXDN,NC,IBUG,
     &       ltbl,dtbl,emax,a1,a2,badscale,fscale,date,*999)
C
      CALL FCOSET2(LTBL,DTBL,NC,CNS,MAXDN,EMAX,NONEGS) !Set LCOR constants
C
C     ...Correct the input image
      CALL LICOR(IUNIT,IUNITC,IUNITD,OUNIT,LUT,DCSCALE,
     &			SLO,SSO,NLO,NSO,NSI,CNS,NC)
C
      CALL FCOEND2(msp,mup,nsp,nnp,itot)  !Get processing statistics
C
C     ...Add labels to output image
      IFDS = IDARK(2)		!FDS count of dark-current frame
      CALL OLABEL(OUNIT,IFDS,A1,A2,BADSCALE,FSCALE,PNAME,DATE,MSP,NSP)      

      CALL PRNT(7,1,ITOT/(NSO*NLO*1.),'Mean DN of DC=.')
      CALL PRNT(4,1,MSP,'Minimum value of a saturated pixel=.')
      CALL PRNT(4,1,MUP,'Maximum value of an unsaturated pixel=.')
      CALL PRNT(4,1,NSP,'Total number of saturated pixels=.')
      CALL PRNT(4,1,NNP,'Total number of negative pixels=.')
      CALL XVMESSAGE('FICOR task completed',' ')
      RETURN
C
C     ...ERROR CONDITIONS
  999 CALL XVMESSAGE('***FICOR task cancelled',' ')
      IF (L.GT.0) CALL PRNT(4,1,L,'***Line=.')
      CALL ABEND
      END
C Get camera parameters, planet-of-encounter, and image size from
C input image label.
C
      SUBROUTINE IPOPEN(iunit,ipic,mode,slo,sso,nlo,nso,nsi,*)
      IMPLICIT NONE

      INTEGER*4 ipic(39),slo,sso,iunit,mode,nlo,nso,nsi
      INTEGER*4 ind,nlabs,nli,icam,a2,icnt,idef

      COMMON/C3/LAB		!Picture label of input image
      CHARACTER*7200 LAB

      CHARACTER*5 FORMAT

      CALL XVUNIT(IUNIT,'INP',1,IND,' ')	!Get unit number
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C     ...Get camera parameters for input image and store in ipic.
      ipic(1) = 39	                !Tell ABLE we want 39 values returned
      CALL ABLE77V2(IND,IUNIT,ipic)     !Get camera params for input image
      CALL GETLABX(IUNIT,NLABS,lab)	!Put VICAR1 label into LAB
      READ (LAB(489:489),9100) MODE
9100  FORMAT(I1)	        !Get camera mode
C
C     ...Get image size.
      CALL XVSIZE(slo,sso,nlo,nso,nli,nsi)
C     ...Input image size is nli x nsi.  Output size is nlo x nso.
      IF (NSI.NE.800) GOTO 990	!Input must be full-sized VGR image
      IF (slo+nlo-1.GT.NLI) GOTO 994	!Bad size field
      IF (sso+nso-1.GT.NSI) GOTO 994	!Bad size field

      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT,' ')	!Get input image format
      IF (FORMAT.NE.'BYTE') THEN	!If not byte, print warning message
          CALL XVMESSAGE
     &         ('***Label indicates input image is not byte',' ')
          CALL XVMESSAGE('***Byte format will be assumed',' ')
      ENDIF
      RETURN
C
  990 CALL XVMESSAGE('***Input must be an 800x800 VGR image',' ')
      GOTO 999
  994 CALL XVMESSAGE
     &     ('***Requested area exceeds boundaries of image',' ')
  999 RETURN1
      END
C Determine planet ID and planet name
C Input: IPIC(39)
C Outputs: iplan,pname
C
      SUBROUTINE PLANETID(IPIC,iplan,pname,*)
      IMPLICIT NONE

      INTEGER*4 IPIC(39),iplan,icam,a2,icnt,idef,i,iyear
      CHARACTER*7 pname,PLANET(5)
      CHARACTER*16 ctemp
      LOGICAL XVPTST

      DATA PLANET/'NOCORR ','JUPITER','SATURN ','URANUS ','NEPTUNE'/
      ICAM = IPIC(6)
      CALL XVPARM('CONV',a2,icnt,idef,1)
      IF (ICAM.GT.5.OR.			  !If VGR-1
     &    (IDEF.EQ.0.AND.ICNT.GT.0)) THEN !or nanowatts,
          iplan = 2			  !planet is always Jupiter
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
         CALL MVLC (IPIC(21),CTEMP,16)
         IF (CTEMP(1:1).EQ.PLANET(I)(1:1)) THEN
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
C Open output frame and get processing information
C
      SUBROUTINE OPOPEN(NSO,ounit,maxdn,nonegs)
      IMPLICIT NONE

      CHARACTER*4 OFORMAT(3)
      INTEGER*4 OCODE,OUNIT,MAXDN,NONEGS,ICNT,IND,NSO,N,IDEF,ICAM
      LOGICAL XVPTST
C
      DATA OFORMAT/'BYTE','HALF','REAL'/
C     ...Lower and upper limits on output DNs
      nonegs=0
      IF (XVPTST('NONEG')) nonegs=1	!Set all negative output DN=0

      CALL XVP('MAXDN',maxdn,ICNT)	!Max output DN
C
C     ...Determine output image format
      IF (XVPTST('BYTE')) THEN
          OCODE=1			!Output format is BYTE
          nonegs=1			!No negative output DNs allowed
          maxdn=255			!Max output DN=255
      ELSE IF (XVPTST('REAL')) THEN
          OCODE = 3			!Output format is REAL
      ELSE
          OCODE = 2			!Default output format is HALF
      END IF

      CALL XVUNIT(ounit,'OUT',1,IND,' ')	!Get output frame unit number
      CALL XVOPEN(OUNIT,IND,'OP','WRITE','OPEN_ACT','SA',
     &   'IO_ACT','SA','U_FORMAT','REAL','O_FORMAT',OFORMAT(OCODE),
     &   'U_NS',NSO,' ')
      RETURN
      END
C Get user-specified camera parameters:
C
C Outputs: IDARK,MODEDC
C Updated: IPIC,MODE
C
      SUBROUTINE CPARAM(ipic,idark,mode,modedc,*)
      IMPLICIT NONE

      INTEGER*4 IPIC(10),IDARK(10),MODE,MODEDC,R,N,ICNT,IDEF,ICAM,IFILT
      EQUIVALENCE (R,N)

      CALL XVPARM('EXPO',R,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL PRNT(7,1,R,'Exposure time overridden, EXPO=.')
          IPIC(3) = N
      END IF
C
      CALL XVPARM('FILTER',N,ICNT,IDEF,1) !Filter position...
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          IPIC(4)=N
          CALL XVMESSAGE('Filter position overridden.',' ')
      END IF

      CALL XVPARM('SCAN',N,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('Scan rate overridden',' ')
          IPIC(5) = N
      END IF
C
      CALL XVPARM('CAMERA',N,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('Camera serial number overridden',' ')
          IPIC(6) = N
      END IF
C
      CALL XVPARM('GAIN',N,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('Gain overridden',' ')
          IPIC(8) = N
      END IF

      CALL XVPARM('MODE',N,ICNT,IDEF,1)	!Mode of input image
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
           CALL XVMESSAGE('Input image mode overridden',' ')
           MODE=N
      ENDIF
C
C     ...Get camera parameters for IDARK-current from user and store in IDARK
C     ...These are supplemented from the picture label in subroutine FSEARCH
C
      CALL ZIA(IDARK,8)			!IDARK(I)=0 if not user-specified

      CALL XVPARM('DCSCAN',N,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('DC scan rate overridden',' ')
          IDARK(5) = N
      END IF
C
      CALL XVPARM('DCCAM',N,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('DC camera serial number overridden',' ')
          IDARK(6) = N
      END IF
C
      CALL XVPARM('DCGAIN',N,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('DC gain overridden',' ')
          IDARK(8) = N
      END IF
C
      CALL XVPARM('DCMODE',MODEDC,ICNT,IDEF,1)  !MODE of DC picture
C
      ICAM = IPIC(6)
      IFILT = IPIC(4)
      IF (ICAM.LT.4.OR.ICAM.GT.7) GOTO 990	!Invalid camera serial number
      IF (IFILT.LT.0.OR.IFILT.GT.7) GOTO 991	!Invalid filter position
      RETURN
C     ...Error conditions
  990 CALL XVMESSAGE('***Invalid camera serial number',' ')
      GOTO 999
  991 CALL XVMESSAGE('***Invalid filter position',' ')
  999 RETURN1
      END
C Search for calibration file and dark-current frame with camera
C parameters matching those of input image.
C
C Inputs: IPIC(10),MODE=camera params for input image (from label or user)
C Outputs: IUNITC,IUNITD=logical unit numbers of cal and DC files
C	   DIV is user specified or extracted from cal file label
C Updated: IDARK(10),MODEDC=camera parameters for dark-current
C          On input IDARK contains the parameters as pecified by the
C          user (=0 if unspecified).  On output, any missing values
C	   are filled from the dark-current frame label.
C
      SUBROUTINE FSEARCH(IPIC,MODE,idark,modedc,iunitc,iunitd,div,*)
      IMPLICIT NONE

      INTEGER*4 IPIC(10),IDARK(10),IFILT,ISCAN,ICAM,IGAIN,NOCH,NI,MAXC
      INTEGER*4 IPCDF,IDC,MAXD,I,IUNITX,IND,NLABX,NSX,N,IMODE,J,MODEDC
      INTEGER*4 MODEDCX,IUNITD,IUNITC,MODE
      REAL*4 DIV

      COMMON/C4/LABEL		!Temporary storage for labels
      CHARACTER*7200 LABEL

      INTEGER*4 ITEST(10),ICAL(10),DARK(10),PCDF,DC
      LOGICAL XVPTST
C
C     ...Input picture camera parameters
      IFILT = IPIC(4)
      ISCAN = IPIC(5)
      ICAM   = IPIC(6)
      IGAIN = IPIC(8)

      NOCH = 0
      IF (XVPTST('NOCHECK')) THEN
          CALL XVMESSAGE('No label checking will be done',' ')
          NOCH = 1
      END IF

      CALL XVPCNT('INP',NI)		!Get number of inputs NI
C
C     ...Find calibration and dark current files (PCDF,DC) from list
C     ...of input files.
      MAXC  = 0		!Measure of match btwn input frame and cal file
      MAXD  = 0		!Measure of match btwn input frame and DC frame
      PCDF  = 0		!Index of acceptable calibration file
      DC    = 0		!Index of acceptable dark-current frame
      IPCDF = 0		!Index of calibration file with best match
      IDC   = 0		!Index of dark-current frame with best match
C     ...PCDF=IPCDF and DC=IDC only if the matches are acceptable.
C     ...Loop through the input files looking for a best match by examining
C     ...the labels.  The first input is skipped because it is assumed to be
C     ...the input image.

      DO 5 I=2,NI
      IF (PCDF*DC.GT.0) GOTO 6	!Exit if Cal and DC files are found...
      CALL XVUNIT(IUNITX,'INP',I,IND,' ')
      CALL XVOPEN(IUNITX,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL GETLABX(IUNITX,NLABX,LABEL)	!Put VICAR1 label into LABEL buf
C     ...Get image format and number of samples
      CALL XVGET(IUNITX,IND,'NS',NSX,' ')
      IF (NSX.LT.3000) GOTO 2	!DC frame if record is small 

C     ...Here if input is a calibration file
      IF(PCDF.GT.0) GOTO 5	  !Skip if we already have a match
      READ (LABEL(109:109),8000) ITEST(6)    !Extract camera serial number,
      READ (LABEL(137:137),8000) ITEST(4)    !filter position,
      READ (LABEL(227:228),8100) ITEST(5)    !scan-rate,
      READ (LABEL(238:238),8000) ITEST(8)    !gain-state from label
8000  FORMAT(I1)
8100  FORMAT(I2)
      N = 31
      IF (NOCH.EQ.0) THEN
         IF(ICAM.NE.ITEST(6)) N=N-16	  !and compare camera, filter,
         IF(IFILT.NE.ITEST(4)) N=N-8	  !and scan-rate with values
         IF(ISCAN.NE.ITEST(5)) N=N-4	  !from input image.
         IF(N.LT.MAXC) GOTO 5		  !Reject if better match exists.
      ENDIF
C
C	Here if possible match...
      IPCDF = I			     !Ith input is candidate cal file
      MAXC = N			     !This match is the best found so far
      CALL MVE(4,10,ITEST,ICAL,1,1)  !Save the camera parameters
      IF(N.GE.27) PCDF=I	     !Accept match if same camera and filter
      GOTO 5
C
C     ...Here if input is a dark-current file
    2 IF (DC.GT.0) GOTO 5	!Skip if acceptable match already found
C        Get DC camera parameters from picture label...
      ITEST(1) = 8
      CALL ABLE77V2(IND,IUNITX,ITEST)  !label parameter no longer returned
      READ (LABEL(489:489),8000) IMODE
C	 Override with user parameters if available...
      DO 3 J=4,8
      IF (IDARK(J).NE.0) ITEST(J)=IDARK(J)
    3 CONTINUE
      IF (MODEDC.GT.0) IMODE=MODEDC
C
      N = 31
      IF (ICAM.NE.ITEST(6)) N=N-16	!Check camera serial number,
      IF (ISCAN.NE.ITEST(5)) N=N-4	!and scan-rate.
      IF (ICAM.NE.4.AND.ICAM.NE.6) GOTO 4 !If wide-angle camera,
      IF (IMODE.EQ.MODE) GOTO 4		!check also for operating mode.
      IF (MODE.EQ.5.OR.IMODE.EQ.5.OR.	!If mode disagrees, mark down
     &    MODE.EQ.7.OR.IMODE.EQ.7) N=N-1!if BSIMAN or BOTSIM
    4 IF (NOCH.EQ.1) N=31
C        Here if possible match...
      IF (N.LT.MAXD) GOTO 5	!Reject if a better match already exists
      IDC = I			!Ith input is best match
      MAXD = N
      CALL MVE(4,10,ITEST,DARK,1,1)  !Save camera parameters
      MODEDCX = IMODE
      IF(N.GE.31) DC=IDC	!Accept camera, scan-rate, and mode agree
    5 CALL XVCLOSE(IUNITX,IND,' ')
C
    6 IF (IDC.EQ.0) GOTO 984	!Abend if no dark-current found
      CALL MVE(4,10,DARK,IDARK,1,1)
      MODEDC = MODEDCX
      CALL PRNT(4,1,IDC,'Dark-Current File input=.')
      CALL XVUNIT(IUNITD,'INP',IDC,IND,' ')
      CALL XVOPEN(IUNITD,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &   'U_FORMAT','HALF',' ')

      IF (IPCDF.EQ.0) GOTO 985	!Abend if no cal file found
      CALL PRNT(4,1,IPCDF,'Calibration File input=.')
      CALL XVUNIT(IUNITC,'INP',IPCDF,IND,' ')
      CALL XVOPEN(IUNITC,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &   'U_FORMAT','HALF',' ')
C
C     ...Print DC and calibration file labels and camera parameters
      CALL PLABEL(IUNITC,IUNITD,IPIC,ICAL,DARK,MODE,MODEDC,
     &        MAXC,MAXD,div)

      IF (PCDF.EQ.0) GOTO 980		!Unacceptable Cal File
      IF (DC.EQ.0) GOTO 981		!Unacceptable Dark-Current
      RETURN

C     ...Error conditions
  980 CALL XVMESSAGE('***No acceptable calibration file found',' ')
      GOTO 999
  981 CALL XVMESSAGE('***No acceptable dark-current frame found',' ')
      GOTO 999
  984 CALL XVMESSAGE('**No dark current input.',' ')
      GOTO 999
  985 CALL XVMESSAGE('***No calibration file input.',' ')
  999 RETURN1
      END
C Print dark-current and calibration file labels and camera parameters.
C
C Inputs: IUNITC,IUNITD=logical unit numbers of calibration and DC files.
C         IPIC(8),ICAL(8),IDARK(8)=camera params for image, cal, & DC
C	  MODE,DCMODE=camera mode for input image and DC
C         MAXC0=measure of match between input image and cal file.
C         MAXD0=measure of match between input image and dark-current.
C Outputs: DIV is user specified or extracted from cal file label
C Work area: COMMON/C1/
C
      SUBROUTINE PLABEL(IUNITC,IUNITD,IPIC,ICAL,IDARK,MODE,MODEDC,
     &     MAXC0,MAXD0,div)
      IMPLICIT NONE

      INTEGER*4 IPIC(8),ICAL(8),IDARK(8),MAXC,MAXC0,MAXD,MAXD0,IUNITD
      INTEGER*4 NLABX,N,I,MAX,IUNITC,ICNT,IDEF,IMOD,J,NCHAR,MODE
      INTEGER*4 MODEDC,IGAIN
      REAL*4 DIV,SCALE,DCSCALE

      COMMON/C5/LABEL,PBUF 	!Temporary storage for labels
      CHARACTER*7200 LABEL
      CHARACTER*121 PBUF

C     ...DMODE(MODE)=camera operating mode
      CHARACTER*8 DMODE(8)
      CHARACTER*8 DABLE(4)
      INTEGER*2 ORDER(4)
C
      DATA DMODE/'NOSHUT  ','NA ONLY ','WA ONLY ','BOTALT  ',
     &           'BSIMAN  ','BODARK  ','BOTSIM  ','XXXXXXXX'/
      DATA DABLE/'S/N     ','FILTER  ','SCANRATE','GAIN    '/
      DATA ORDER/6,4,5,8/
      MAXC = MAXC0	!Measure of match btwn input frame and cal file
      MAXD = MAXD0	!Measure of match btwn input frame and DC frame
C
C     ...Print out dark current label (of best match)
      CALL GETLABX(IUNITD,NLABX,LABEL)	!Put VICAR1 label into LABEL buf
      PBUF=' '
      CALL XVMESSAGE('DARK-CURRENT LABEL...',' ')
      CALL XVMESSAGE(' ',' ')
      N = 72*NLABX
      DO I=1,N,72
         PBUF(11:79)= LABEL(I:I+68)
         CALL XVMESSAGE(PBUF,' ')
      END DO
C
      MAX = MAXD	!Check match of dark current file
      IF(MAX.LT.16) CALL XVMESSAGE('***CAMERA S/N DO NOT MATCH.',' ')
      MAX = MOD(MAX,8)
      IF(MAX.LT.4) CALL XVMESSAGE('***SCAN RATES DO NOT MATCH',' ')
      MAX = MOD(MAX,2)
      IF(MAX .LT.1) CALL XVMESSAGE
     &   ('***CONFLICT BETWEEN INPUT PICTURE AND DARK CURRENT MODE',' ')
C
C     ...Print out calibration file label (of best match)
      CALL GETLABX(IUNITC,NLABX,LABEL)		!Put VICAR1 labels in LABEL
      PBUF=' '
      CALL XVMESSAGE('CALIBRATION FILE LABEL...',' ')
      CALL XVMESSAGE(' ',' ')
      N = 72*NLABX
      DO I=1,N,72
         PBUF(11:79)= LABEL(I:I+68)
         CALL XVMESSAGE(PBUF,' ')
      END DO
C
      MAX = MAXC	!Check match of calibration file
      IF(MAX.LT.16) CALL XVMESSAGE('***CAMERA S/N DO NOT MATCH.',' ')
      MAX = MOD(MAX,16)
      IF(MAX.LT.8) CALL 
     &               XVMESSAGE('***FILTER POSITIONS DO NOT MATCH.',' ')
C
C     ...Get low pass filter divisor (DIV)
      CALL XVPARM('DIV',DIV,ICNT,IDEF,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          CALL XVMESSAGE('Calibration file divisor overridden',' ')
      ELSE					!If not user-specified,
          READ (LABEL(203:210),9000) DIV	!get DIV from the label
9000      FORMAT (F8.3)
      END IF
      CALL PRNT(7,1,DIV,'DIV=.')
C
C     ...Print out camera parameters
      CALL XVMESSAGE('  CAMERA PARAMETERS...',' ')
      PBUF=' '
      PBUF(17:43)='INPUT   DARK    CALIBRATION'
      CALL XVMESSAGE(PBUF,' ')
      PBUF=' '
      PBUF(17:36)='FRAME   CURRENT FILE'
      CALL XVMESSAGE(PBUF,' ')
      PBUF=' '
      PBUF(40:68)='***PARAMETERS DO NOT MATCH***'
      IMOD = 16
C
      DO 15 I=1,4
        J = ORDER(I)
        PBUF(6:13) = DABLE(I)
        WRITE (PBUF(18:19),'(I2)') IPIC(J)
        WRITE (PBUF(26:27),'(I2)') IDARK(J)
        WRITE (PBUF(34:35),'(I2)') ICAL(J)
        NCHAR = 35
        IF (I.EQ.1.AND.MIN0(MAXC,MAXD).LT.IMOD) NCHAR=68  !Camera mismatch
        IF (I.EQ.2.AND.MAXC.LT.IMOD) NCHAR=68		!Filter mismatch
        IF (I.EQ.3.AND.MAXD.LT.IMOD) NCHAR=68		!Scan-rate mismatch
        MAXC = MOD(MAXC,IMOD)
        MAXD = MOD(MAXD,IMOD)
        IMOD = IMOD/2
   15 CALL XVMESSAGE(PBUF(1:NCHAR),' ')
C
      IF(MODE.LT.1.OR.MODE.GT.7) MODE=8
      IF(MODEDC.LT.1.OR.MODEDC.GT.7) MODEDC=8

      PBUF(1:35)=' '
      PBUF(6:13)='MODE    '
      PBUF(17:23) = DMODE(MODE)(1:7)
      PBUF(26:32) = DMODE(MODEDC)(1:7)
      NCHAR = 35
      IF(MAXD.EQ.0) NCHAR=68
      CALL XVMESSAGE(PBUF(1:NCHAR),' ')
      RETURN
      END
C Scale the DN values of the input image and dark-current frame to
C equivalent units as the data points in the calibration file.
C
C Inputs: ICAM = Camera serial number (4,5,6,7)
C         IGAIN = Gain-state of input image (0=low, 1=high)
C	  IDCGAIN = Gain-state of dark-current frame
C         DIV = Cal file divisor
C	  IBUG = Flag to print diagnostics.
C Outputs: lut(256) = look-up table for converting DNs of input image
C	   dcscale = scale factor for converting DNs of dark-current
C          
      SUBROUTINE IPSCALE(ICAM,IGAIN,IDCGAIN,DIV,IBUG,lut,dcscale)
      IMPLICIT NONE

      INTEGER*4 IBUG,IGAIN,ICAM,IDCGAIN,NUMDC,ICNT,IDEF,I
      REAL*4 lut(256),SCALE,DIV,DCSCALE
C
C     ...GAINS(icam-3)=camera gain-state ratio (high/low).
      REAL*4 GAINS(4)
      DATA GAINS/3.7642, 4.2531, 3.9424, 3.9240/
C
C     ...Scale to adjust for low-pass filter
      SCALE = DIV			!Input image scale factor
      DCSCALE = DIV			!Dark-current scale factor
C
C     ...Adjust for high gain
      IF (IGAIN.EQ.1) SCALE=SCALE/GAINS(ICAM-3)
      IF (IDCGAIN.EQ.1) DCSCALE=DCSCALE/GAINS(ICAM-3)
C
C     ...Adjust for summed dark-current input
      CALL XVPARM('NUMDC',numdc,icnt,idef,1)
      DCSCALE = DCSCALE/NUMDC
C
C     ...Generate look up table to adjust DNs if input image to same scale
C     ...as data points in CAL file (which is in low-gain).
C
      DO I=1,256
          lut(I) = SCALE*(I-1)
      ENDDO
C
      IF (IBUG.EQ.1) CALL PRNT(7,256,LUT,'LUT=.')
      RETURN
      END
C Scale luminance table (ltbl) so that output DN is output user-specified
C units (a1 or a2).
C
C Inputs: ICAM,IFILT,IPLAN,SPEED,MAXDN,NC,IBUG
C Outputs: a1 = I over F per DN
C	   a2 =	Nanowatts/DN
C          badscale = No good scale for this filter
C	   fscale = Scale correction a la FIXVGR
C          luminances ltbl and emax are scaled.
C	   luminance differences dtbl are computed and scaled.
C	   pname = Planet-of-encounter
C	   date = Version date of SCF table used to get fscale
C
      SUBROUTINE OPSCALE(ICAM,IFILT,IPLAN,SPEED,MAXDN,NC,IBUG,
     &    ltbl,dtbl,emax,a1,a2,badscale,fscale,date,*)
      IMPLICIT NONE

      INTEGER*4 IBUG,I,NC,ICAM,IFILT,ICNT,IDEF,MAXDN,JDEF,IPLAN,K
      REAL*4 ltbl(10),dtbl(9),EMAX,SPEED,EADJ,CONIOF,CONX,SCL,OSCALE
      REAL*4 A1,A2,FSCALE,SCALE
      CHARACTER*8 date
      LOGICAL BADSCALE,XVPTST
      CHARACTER*121 PBUF
C
C     ...Print luminances (unadjusted)
      CALL XVMESSAGE('MAXL and luminances of calibration frames--',' ')
      CALL XVMESSAGE(' ',' ')
      PBUF=' '   
      DO I=1,NC
         WRITE (PBUF(10*I+1:10*I+10),'(F10.2)') ltbl(I)
      ENDDO
      WRITE (PBUF(1:10),'(F10.2)') emax
      CALL XVMESSAGE(PBUF,' ')
C
C     ...Exposure ajustment for shutter speed.
      CALL PRNT(7,1,SPEED,'Exposure time=.')
      IF (SPEED.LE.0.) GOTO 970		!Abend if bad exposure time.
      EADJ = 1000.0/SPEED		!reference_speed/actual_speed
C          
C     ...Get IOF/ftL and Nanowatts/ftL scale factors (CONIOF,CONX)
      CALL FTLAMBERT(ICAM,IFILT,coniof,conx,badscale)
C
C     ...Calculate conversion factors relating output DNs to absolute
C     ...units
      CALL XVPARM('SCALE',scl,icnt,idef,1)
      IF (IDEF.EQ.0.AND.ICNT.GT.0) THEN
          OSCALE = SCL*emax/MAXDN	!Compute a2, OSCALE, and a1
          a2 = OSCALE*CONX
          a1 = OSCALE*CONIOF
          CALL XVMESSAGE('**SCALE keyword not implemented--ABEND.',' ')
          RETURN1
      ENDIF

      CALL XVPARM('CONV',a2,icnt,jdef,1)
      IF (JDEF.EQ.0.AND.ICNT.GT.0) THEN
        a1 = a2*CONIOF/CONX		!compute a1, OSCALE and SCL
        OSCALE = a2/CONX		!OSCALE=Ft-Lamberts/outputDN
        SCL = OSCALE*MAXDN/emax
        CALL XVMESSAGE('Conversion factor specified',' ')
      ELSE				  !If a2 is not specified,
        CALL XVPARM('IOF',a1,icnt,idef,1) !get a1 and use it to
        a2 = a1*CONX/CONIOF		  !compute a2, OSCALE, and SCL
        OSCALE = a1/CONIOF
        SCL = OSCALE*MAXDN/emax
      ENDIF
C
C     ...Fix output DN scale a la FIXVGR
      IF (XVPTST('NOCORREC')) THEN
          fscale = 0.0
      ELSE
          CALL FIXSCALE(A1,ICAM,IFILT,IPLAN,fscale,date,*999)
      ENDIF
C
C     ...Scale the luminances to units of output DN
      SCALE = EADJ/OSCALE
      IF (FSCALE.NE.0.0) SCALE=SCALE*FSCALE
      DO K=1,NC      
          ltbl(K) = SCALE*ltbl(K)
      ENDDO
      emax = SCALE*emax
C
C     ...Store differences of calibration frame luminances in dtbl.
      DO K=2,NC
          dtbl(K-1)=ltbl(K)-ltbl(K-1)
      ENDDO
C
      IF (IBUG.EQ.1) THEN
          CALL PRNT(7,10,LTBL,'LTBL=.')
          CALL PRNT(7,9,DTBL,'DTBL=.')
      ENDIF
      RETURN
C
C     ...Error conditions
  970 CALL XVMESSAGE('***Exposure time is incorrect',' ')
      IF (SPEED.EQ.0.)
     &   CALL XVMESSAGE('FICOR does not work on 0 exposure frames',' ')
  999 RETURN1
      END
C Returns conversion factors for "I over F" per DN and Nanowatts per DN.
C Inputs: ICAM,IFILT
C Outputs: coniof,conx,badscale
C
      SUBROUTINE FTLAMBERT(ICAM,IFILT,coniof,conx,badscale)
      IMPLICIT NONE

      REAL*4 CONX,CONIOF,VERSION
      INTEGER*4 IFILT,ICAM,ICNT,IND
      LOGICAL badscale,XVPTST
C
C    ...SUN(ifilt,icam)=solar irradiance in nanowatts/steradian*cm**2/nm
C    ...LC(ifilt,icam)=radiance for xenon light-cannon
C    ...IOFCON(ifilt,icam)=conversion between ft-L and I over F at
C    ...Jupiter's distance from the sun.  Albedo of 1.=10,000 I/F units
      REAL*4 SUN(8,8), LC(8,8), IOFCON(8,8)

      DATA SUN/0. , 0. , 0. , 0. , 0. , 0. , 0. , 0. ,
     &  0.    , 0.    , 0.    , 0.    , 0.    , 0.    , 0.    , 0.     ,
     &  3.52  , 2.34  , 4.00  , 5.09  , 3.52  , 4.83  , 4.83  , 0.     ,
     &  0.010 , 4.512 , 4.306 , 3.150 , 0.010 , 4.799 , 0.010 , 4.877  ,
     &  3.285 , 2.282 , 3.916 , 5.021 , 3.285 , 4.789 , 4.804 , 0.4526 ,
     &  0.010 , 4.538 , 4.406 , 3.158 , 0.010 , 4.792 , 0.010 , 4.887  ,
     &  3.339 , 2.290 , 3.912 , 5.020 , 3.339 , 4.850 , 4.820 , 0.2195 ,
     &  1.00  , 4.59  , 3.97  , 3.22  , 1.00  , 4.96  , 1.00  , 4.92   /
C
      DATA LC/0. , 0. , 0. , 0. , 0. , 0. , 0. , 0. ,
     &  0.   , 0.   , 0.   , 0.   , 0.   , 0.   , 0.   , 0.    ,
     &  4.85 , 2.73 , 3.45 , 5.04 , 4.85 , 4.93 , 4.93 , 0.272 ,
     &  5.03 , 5.10 , 4.60 , 3.45 , 4.90 , 5.09 , 4.69 , 4.82  ,
     &  4.30 , 2.38 , 4.33 , 5.03 , 4.30 , 4.85 , 4.85 , 0.522 ,
     &  5.08 , 4.79 , 4.59 , 3.52 , 4.91 , 4.82 , 4.68 , 4.82  ,
     &  4.25 , 2.46 , 4.34 , 5.03 , 4.29 , 4.85 , 4.88 , 0.455 ,
     &  1.00 , 4.68 , 4.37 , 3.28 , 1.00 , 4.69 , 1.00 , 4.80  /
C
      DATA IOFCON/
     &   0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,
     &   0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,
     &   0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,
     &   0.0  , 19.530, 19.490, 14.790,  0.0  , 22.240,  0.0  , 23.750,
     &  16.330, 12.370, 16.930, 23.810, 16.330, 22.270, 22.360,  2.075,
     &   0.0  , 19.700, 20.080, 14.800, 00.000, 22.430, 00.000, 22.540,
     &  16.510, 12.400, 16.940, 24.200, 16.510, 22.650, 22.460,  2.014,
     &   0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  ,  0.0  /
C
      IF (XVPTST('LC')) THEN		!Light-cannon illumination
           conx = LC(IFILT+1,ICAM)	!Nanowatts/ft-L
           CALL XVMESSAGE('Radiance refers to xenon spectrum',' ')
      ELSE				!Solar illumination
           conx = SUN(IFILT+1,ICAM)	!Nanowatts/ft-L
           CALL XVMESSAGE('Radiance refers to solar spectrum',' ')
      ENDIF
      CALL PRNT(7,1,CONX,'FtL-sec to nanowatt conv factor=.')
C
      coniof = IOFCON(IFILT+1,ICAM)	!IOF/ft-L
      CALL PRNT(7,1,CONIOF,'Ft-L to I/F conv factor=.')
C
      IF (conx.GT..1) THEN
          badscale = .FALSE.
      ELSE		!Certain filters did not have ft-L to nanowatts
          conx = 4.	!or I/F conversions.  Choose some reasonable
          coniof = 20.	!values and set bad scale flag
          badscale = .TRUE.
      ENDIF		
      RETURN
      END
C Fix the output DN scale using the scale correction factor, as
C retrieved from the Scale Correction File (SCF).  This routine
C implements the scaling performed by FIXVGR.
C
C Inputs: A1,ICAM,IFILT
C Outputs: fscale=Scale correction
C          pname=Planet-of-encounter
C          date=Version date of table in the SCF
C
      SUBROUTINE FIXSCALE(A1,ICAM,IFILT,IPLAN,fscale,date,*)
      IMPLICIT NONE

      CHARACTER*8 date		!Version date of SCF table used.

      INTEGER*4 SUNIT,ICNT,IND,IFILT,ICAM,IPLAN
      REAL*4 VERSION,FSCALE,A1
      CHARACTER*256 SCFNAME	!Filename of SCF

C     ...Open Scale Correction File
      CALL XVP('SCF',SCFNAME,ICNT)	!Get SCF file name
      CALL XVUNIT(SUNIT,'X',1,IND,'U_NAME',SCFNAME,' ')
      CALL XVOPEN(SUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &            'CONVERT','OFF',' ')  !see fixvgr.com for details
C
C     ...Get scale correction factor and date
      VERSION = 0		!Set flag to get latest version
      CALL CORRECT(SUNIT,IFILT,ICAM,IPLAN,version,date,fscale,*999)
      CALL PRNT(7,1,fscale,'Scale correction factor=.')
      RETURN

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
      IMPLICIT NONE

      INTEGER*4 UNIT,version,TRN_BUF(12),STAT !see fixvgr.com for details
      CHARACTER*8 date                        !for trn_buf and xvtrans
      INTEGER*4 IND,NL,NS,IFILT,IPLAN,ICAM

      REAL*4 TABLE(8,4,4),RTABLE(8,4,4),RDATE(2),SCALE
      CHARACTER*8 LDATE

      CHARACTER*8 IDATE

      CALL XVTRANS_INU(TRN_BUF,'REAL','REAL',UNIT,STAT)
      IF (STAT.NE.1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')

      CALL XVGET(UNIT,IND,'NL',nl,'NS',ns,' ')
      IF (NL.LT.1.OR.NS.NE.130) GOTO 990	!If wrong size, bad file.

      IF (VERSION.EQ.0) version=NL		!Use most recent table

      IF (VERSION.GT.0) THEN			!If the version is known,
         CALL XVREAD(UNIT,TABLE,IND,'LINE',VERSION,
     &               'NSAMPS',NS-2,' ')             !read it in, get the
         CALL XVREAD(UNIT,RDATE,IND,'LINE',VERSION, !scale from the table,
     &               'SAMP',NS-1,'NSAMPS',2,' ')    !get the date,
         CALL XVTRANS(TRN_BUF,TABLE,RTABLE,NS-2)
         scale = RTABLE(IFILT+1,IPLAN-1,ICAM-3)	    
         CALL MVLC(RDATE,LDATE,8)
         DATE=LDATE		    
         RETURN					    !and exit.
      ENDIF
C
C     ...Here to search for table with matching date
      DO 22 version=1,NL		!Read each record of the SCF
         CALL XVREAD(UNIT,TABLE,IND,'LINE',VERSION,
     &               'NSAMPS',NS-2,' ')             !read it in, get the
         CALL XVREAD(UNIT,RDATE,IND,'LINE',VERSION, !scale from the table,
     &               'SAMP',NS-1,'NSAMPS',2,' ')     !get the date,
         CALL XVTRANS(TRN_BUF,TABLE,RTABLE,NS-2)
         CALL MVLC(RDATE,LDATE,8)
         IDATE=LDATE		    
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
C Radiometrically correct the input image
C All arguments are inputs.
C
      SUBROUTINE LICOR(IUNIT,IUNITC,IUNITD,OUNIT,
     &			LUT,DCSCALE,SLO,SSO,NLO,NSO,NSI,CNS,NC)
      IMPLICIT NONE

      REAL*4 LUT(256),DCSCALE
      INTEGER*4 SLO,SSO,S,CNS,CL,CS,NSIC,NC,LINC,LLO,NLO,L,IS,IUNIT,IND
      INTEGER*4 NSO,IUNITD,IJ,NSI,IUNITC,NS,OUNIT

      COMMON/C6/BUF,D,DBUF,RBUF
      BYTE BUF(800)	        !Input image (one line)
      INTEGER*2 D(3200)		!Calibration file data (one record)
      INTEGER*2 DBUF(800)	!Dark-current (one line)
      REAL*4 RBUF(800)		!Output image (one line)

      NSIC = CNS*NC     !Number of samples per calibration file record
      LINC = 0		!Current calibration line in buffer D
      LLO = SLO+NLO-1   !Last image line out
C
C		Loop through each image line...
      DO 200 L=SLO,LLO		!Do from starting line to last line...
      IS = 1   
      CALL XVREAD(IUNIT,BUF,IND,'LINE',L,'SAMP',SSO,'NSAMPS',NSO,' ')
      CALL XVREAD(IUNITD,DBUF,IND,'LINE',L,'SAMP',SSO,'NSAMPS',NSO,' ')
   80 S = IS + SSO - 1		!(L,S) is line-sample of next input
      IJ = (L-1)*NSI + S	! pixel to be processed.
      CL = (IJ-1)/CNS + 1	!(CL,CS) points to corresponding data
      CS = IJ - (CL-1)*CNS	! samples in calibration file.
      IF(LINC.LT.CL) THEN	!Read new calibration record if needed
         LINC = CL	
         CALL XVREAD(IUNITC,D,IND,'LINE',LINC,'NSAMPS',NSIC,' ')
      ENDIF
      NS = MIN0(NSO-IS,CNS-CS) + 1	!Number of samples to process
      CALL FCOR(BUF(IS),DBUF(IS),D(NC*(CS-1)+1),RBUF(IS),
     &           LUT,DCSCALE,NS,NC)	!Do the radiometric correction
      IS = IS + NS			!Step to next sample on line
      IF (IS.LE.NSO) GOTO 80		!Loop back if not at end of line.
  200 CALL XVWRIT(OUNIT,RBUF,IND,' ')	!Write out the corrected line
C
      RETURN
      END
C Ficor pixel loop to decalibrate one line of video data
C This FORTRAN version of MACRO routine LCOR is saved for posterity.
C The last argument (NC) is not required in MACRO version since it is
C passed via LCOSET.
C
      SUBROUTINE FCOR(BUF,DBUF,D,OBUF,LUT,DCSCALE,NS,NC)
      IMPLICIT NONE

      BYTE BUF(*)
      REAL*4 RMAXDN,EMAX,RMSP,RMUP,RS,DCSCALE,EMAX1,A1,A2,FDSDC,FSCALE
      INTEGER*4 NC,NONEGS,NSP,NNP,ITOT,J,I,NS,MAXDN,NONEGS1,MSP,MUP,INSP
      INTEGER*4 INNP,IITOT,NC1,IND,IFDS
      INTEGER*2 DBUF(*),D(NC,*)
      REAL*4 LUT(*),LTBL(10),DTBL(9),LTBL1(*),DTBL1(*),LUM
      REAL*4 OBUF(*)
      INTEGER DN,DC,S,CNS,CNS1

C     Common globals
      COMMON/CFCOR/LTBL,DTBL,CNS,RMAXDN,EMAX,NONEGS,RMSP,RMUP,NSP,NNP,
     +             ITOT

      INCLUDE 'fortport'    !for dn and lval byte2int and int2byte

C     ...RMSP = minimum luminance of a saturated pixel
C     ...RMUP = maximum luminance of an unsaturated pixel
C     ...NSP = number of saturated pixels
C     ...NNP = number of negative pixels
C     ...ITOT = running sum of dark-current DNs

      DN = 0
      J = 1			!Start search at S(1)

      DO 40 I=1,NS		!Loop through each input sample
         DN=BYTE2INT(BUF(I))
         DC = DBUF(I)
         ITOT = ITOT + DC		!ITOT=running sum of DC
         LUM = 0.			!Initialize L=0
         IF (D(NC,I).EQ.0) GOTO 40	!If cal file data=0, then L=0

C     ...Convert input pixel to low gain using look-up table
C     ...and subtract dark-current.
         RS = LUT(DN+1) - DCSCALE*DC	!S = DN - DC
         S  = INT(RS)
C
C     ...Find S(j) and S(j+1) such that  S(j) < S < S(j+1)
         IF (S.LE.0) THEN		!If S<0, use lowest two
            J = 1			!points on curve.
            IF (S.EQ.0) GOTO 40	!If S=0, set L=0
            GOTO 25
         ENDIF

         IF (S.LE.D(J,I)) GOTO 20	! S < S(j)
C
C     ...Here if  S(j) < S.  Slide up curve until S < S(j+1)
   12    IF (J.EQ.NC) GOTO 16
         IF (S.LE.D(J+1,I)) GOTO 25
         J = J + 1
         GOTO 12

C     ...Here if  S(nc) < S.  Extrapolate using unsaturated part of curve.
   16    J = J - 1			!J = NC - 1
         IF (D(J,I).LT.D(J+1,I)) GOTO 25
         IF (J.GT.1) GOTO 16
         LUM = EMAX
         GOTO 30

C     ...Here if  S < S(j)  Slide down curve until  S(j) < S
   20    IF (J.EQ.1) GOTO 25	!If  S < S(1), extrapolate.
         J = J - 1
         IF (D(J,I).GE.S) GOTO 20

C     ...Calculate output luminance
   25    LUM = DTBL(J)*(RS-D(J,I))/(D(J+1,I)-D(J,I)) + LTBL(J)
         IF (LUM.LT.0.0) THEN
             NNP = NNP + 1		!Count number of negative pixels
             IF (NONEGS.EQ.1) THEN
                LUM = 0.0
             ELSE
                LUM = MAX(LUM,-RMAXDN)
             ENDIF
             GOTO 40
         ENDIF

   30    IF (DN.LT.255) THEN
             IF (LUM.GT.RMUP) RMUP=LUM !Maximum luminance of unsaturated pixel
         ELSE
             NSP = NSP + 1		!Count number of saturated input pixels
             IF (LUM.LT.RMSP) RMSP=LUM !Minimum luminance of saturated pixel
         ENDIF
         IF (LUM.GT.RMAXDN) LUM=RMAXDN

   40 OBUF(I) = LUM		!Store output luminance L
      RETURN

C     ...Routine to set lcor constants
C     ...Note: NC1 not used here, but required for MACRO version
      ENTRY FCOSET(LTBL1,DTBL1,NC1,CNS1,MAXDN,EMAX1,NONEGS1)
      CALL MVE(7,10,LTBL1,LTBL,1,1)
      CALL MVE(7,9,DTBL1,DTBL,1,1)
      CNS    = CNS1
      RMAXDN  = MAXDN
      EMAX   = EMAX1
      NONEGS = NONEGS1
      RMSP   = RMAXDN
      RMUP   = 0.
      NSP    = 0
      NNP    = 0
      ITOT   = 0
      RETURN

C     ...Routine to return lcor statistics
      ENTRY FCOEND(MSP,MUP,INSP,INNP,IITOT)
      MSP = RMSP
      MUP = RMUP
      INSP = NSP
      INNP = NNP
      IITOT = ITOT
      RETURN
      END

      SUBROUTINE FCOSET2(LTBL1,DTBL1,NC1,CNS1,MAXDN,EMAX1,NONEGS1)
C     Parms
      REAL*4 LTBL1(10)
      REAL*4 DTBL1(9)
      INTEGER*4 NC1,CNS1,MAXDN
      REAL*4 EMAX1
      INTEGER*4 NONEGS1

C     Common globals
      REAL*4 LTBL(10)
      REAL*4 DTBL(9)
      INTEGER*4 CNS
      REAL*4 RMAXDN
      REAL*4 EMAX
      INTEGER*4 NONEGS
      REAL*4 RMSP
      REAL*4 RMUP
      INTEGER*4 NSP
      INTEGER*4 NNP
      INTEGER*4 ITOT
      COMMON/CFCOR/LTBL,DTBL,CNS,RMAXDN,EMAX,NONEGS,RMSP,RMUP,NSP,NNP,
     +             ITOT

      CALL MVE(7,10,LTBL1,LTBL,1,1)
      CALL MVE(7,9,DTBL1,DTBL,1,1)
      CNS    = CNS1
      RMAXDN  = MAXDN
      EMAX   = EMAX1
      NONEGS = NONEGS1
      RMSP   = RMAXDN
      RMUP   = 0.
      NSP    = 0
      NNP    = 0
      ITOT   = 0
      RETURN
      END

      SUBROUTINE FCOEND2(MSP,MUP,INSP,INNP,IITOT)
C     Parms
      INTEGER*4 MSP,MUP,INSP,INNP,IITOT

C     Common globals
      REAL*4 LTBL(10)
      REAL*4 DTBL(9)
      INTEGER*4 CNS
      REAL*4 RMAXDN
      REAL*4 EMAX
      INTEGER*4 NONEGS
      REAL*4 RMSP
      REAL*4 RMUP
      INTEGER*4 NSP
      INTEGER*4 NNP
      INTEGER*4 ITOT
      COMMON/CFCOR/LTBL,DTBL,CNS,RMAXDN,EMAX,NONEGS,RMSP,RMUP,NSP,NNP,
     +             ITOT

      MSP = RMSP
      MUP = RMUP
      INSP = NSP
      INNP = NNP
      IITOT = ITOT
      RETURN
      END

C Add radiometric scale and processing info to picture label and
C output label.  All arguments are inputs.
C
      SUBROUTINE OLABEL(OUNIT,IFDS,A1,A2,BADSCALE,FSCALE,
     &                PNAME,DATE,MSP,NSP)
      IMPLICIT NONE

      INTEGER*4 OUNIT,MSP,NSP,IND,IFDS
      REAL*4 A1,A2,FDSDC,FSCALE
      LOGICAL BADSCALE
      CHARACTER*7 PNAME
      CHARACTER*8 DATE
      CHARACTER*62 MSG1, MSG2, MSG3, MSG4, MSG5, MSG6

      MSG1=
     & 'FICOR77  MINSAT=***** NUMSAT=******                           '
      MSG2=
     & 'FOR NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY XXXX.XXXXX   '
      MSG3=
     & 'FOR (I/F)*10000., MULTIPLY DN VALUE BY           XXXX.XXXXX   '
      MSG4=
     & 'FICOR77  DARK CURRENT FDS = *****.**                          '
      MSG5=
     & 'ABSOLUTE CALIBRATION NOT GOOD,SHADING CORRECTION ONLY WAS DONE'     
      MSG6=
     & ' PICTURE MULTIPLIED BY              FICOR      2/02/86 VERSION'

      WRITE (MSG1(17:21),'(I5)') MSP
      WRITE (MSG1(30:35),'(I6)') NSP
      CALL XLADD(OUNIT,'HISTORY','LABEL1',MSG1,IND,
     &		'FORMAT','STRING','ULEN',62,' ')

      WRITE (MSG2(50:60),'(F11.5)') A2
      CALL XLADD(OUNIT,'HISTORY','LABEL2',MSG2,IND,
     &		'FORMAT','STRING','ULEN',62,' ')

      WRITE (MSG3(50:60),'(F11.5)') A1
      CALL XLADD(OUNIT,'HISTORY','LABEL3',MSG3,IND,
     &		'FORMAT','STRING','ULEN',62,' ')

      FDSDC = IFDS/100.		!FDS of the dark-current
      WRITE (MSG4(29:36),'(F8.2)') FDSDC
      CALL XLADD(OUNIT,'HISTORY','LABEL4',MSG4,IND,
     &		'FORMAT','STRING','ULEN',62,' ')

      IF (BADSCALE) CALL XLADD(OUNIT,'HISTORY','LABEL5',MSG5,IND,
     &		'FORMAT','STRING','ULEN',62,' ')

      IF (FSCALE.EQ.0.0) RETURN
      WRITE (MSG6(26:30),'(F5.2)') FSCALE
      MSG6(47:54) = DATE
      CALL XLADD(OUNIT,'HISTORY','COMMENT',MSG6,IND,
     &		'FORMAT','STRING','ULEN',62,' ')

      CALL XLADD(OUNIT,'HISTORY','SCALE',PNAME,IND,
     &		'FORMAT','STRING','ULEN',7,' ')
      RETURN
      END
C Read VICAR1 labels from file (IUNIT) and store in LAB
C Input: IUNIT=logical unit of file 
C Outputs: NLABS=number of 72-byte labels
C	   LAB(72,25)=VICAR1 labels
C
      SUBROUTINE GETLABX(IUNIT,NLABS,LAB)
      IMPLICIT NONE

      CHARACTER*72 LAB(25)
      INTEGER*4 CNT,INSTANCES(100),IUNIT,NLABS,IND,I
      CHARACTER*8 TASKS(100)
      CHARACTER*5 IBM_LABEL
      BYTE A,B

      CNT=100
      CALL XLHINFO(IUNIT,TASKS,INSTANCES,CNT,IND,' ')
C     ...Get number of 72-byte labels (NLABS)
      CALL XLGET(IUNIT,'HISTORY','NLABS',NLABS,IND,'HIST',TASKS(1),
     &           'INSTANCE',INSTANCES(1),'FORMAT','INT',' ')
      IF (IND.EQ.-38) THEN
         CALL XVMESSAGE
     &        ('***Truncated input label, may cause problems',' ')
         NLABS = 7		!Let's hope there are at least 7 labels...
      ENDIF
      IBM_LABEL='LAB01'

      DO I=1,NLABS
         CALL XLGET(IUNIT,'HISTORY',IBM_LABEL,LAB(I),IND,
     &             'HIST', TASKS(1),'INSTANCE',INSTANCES(1),
     &             'FORMAT','STRING',' ')
         A=ICHAR(IBM_LABEL(5:5))+1       !replaces keyinc
         IF (A.GT.ICHAR('9')) THEN
            A=ICHAR('0')
            B=ICHAR(IBM_LABEL(4:4))+1
            IBM_LABEL(4:4)=CHAR(B)
         ENDIF
         IBM_LABEL(5:5)=CHAR(A)
      END DO

      RETURN
      END
