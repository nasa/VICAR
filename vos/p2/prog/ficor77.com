$!****************************************************************************
$!
$! Build proc for MIPL module ficor77
$! VPACK Version 1.9, Monday, July 21, 2014, 15:38:59
$!
$! Execute by entering:		$ @ficor77
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   DOC         Only the documentation files are created.
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
$ write sys$output "*** module ficor77 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Doc = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "DOC" then Create_Doc = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Create_Doc .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ficor77.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Doc then gosub Doc_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Create_Doc = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ficor77.imake") .nes. ""
$   then
$      vimake ficor77
$      purge ficor77.bld
$   else
$      if F$SEARCH("ficor77.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ficor77
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ficor77.bld "STD"
$   else
$      @ficor77.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ficor77.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ficor77.com -mixed -
	-s ficor77.f -
	-i ficor77.imake -
	-p ficor77.pdf -
	-t tstficor77.pdf tstficor77.log -
	-d ficor77.doc
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ficor77.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
      CALL IFMESSAGE('FICOR77 version 21 Jul 2014')
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
      CALL FCOSET(LTBL,DTBL,NC,CNS,MAXDN,EMAX,NONEGS) !Set LCOR constants
C
C     ...Correct the input image
      CALL LICOR(IUNIT,IUNITC,IUNITD,OUNIT,LUT,DCSCALE,
     &			SLO,SSO,NLO,NSO,NSI,CNS,NC)
C
      CALL FCOEND(msp,mup,nsp,nnp,itot)  !Get processing statistics
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
      SAVE LTBL,DTBL,CNS,RMAXDN,EMAX,NONEGS
      SAVE RMSP,RMUP,NSP,NNP,ITOT
      INTEGER DN,DC,S,CNS,CNS1
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
C Add radiometric scale and processing info to picture label and
C output label.  All arguments are inputs.
C
      SUBROUTINE OLABEL(OUNIT,IFDS,A1,A2,BADSCALE,FSCALE,
     &                PNAME,DATE,MSP,NSP)
      IMPLICIT NONE

      INTEGER*4 OUNIT,MSP,NSP,IND,IFDS
      REAL*4 A1,A2,FDSDC,FSCALE
      LOGICAL BADSCALE
      CHARACTER*8 PNAME
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ficor77.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ficor77

   To Create the build file give the command:

		$ vimake ficor77			(VMS)
   or
		% vimake ficor77			(Unix)


************************************************************************/


#define PROGRAM	ficor77
#define R2LIB

#define MODULE_LIST ficor77.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ficor77.pdf
process help=*
PARM INP      TYPE=STRING   COUNT=(3:30)
PARM OUT      TYPE=STRING
PARM SIZE     TYPE=INTEGER  COUNT=4			DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER  COUNT=1 			DEFAULT=1
PARM SS       TYPE=INTEGER  COUNT=1 			DEFAULT=1
PARM NL       TYPE=INTEGER  COUNT=1 			DEFAULT=0
PARM NS       TYPE=INTEGER  COUNT=1 			DEFAULT=0
PARM IOF      TYPE=REAL     COUNT=1			DEFAULT=1.0
PARM CONV     TYPE=REAL     COUNT=0:1			DEFAULT=--
PARM CAMERA   TYPE=INTEGER  COUNT=0:1	VALID=(4:7)	DEFAULT=--
PARM FILTER   TYPE=INTEGER  COUNT=0:1	VALID=(0:7)  	DEFAULT=--
PARM GAIN     TYPE=INTEGER  COUNT=0:1	VALID=(0,1) 	DEFAULT=--
PARM SCAN     TYPE=INTEGER  COUNT=0:1			DEFAULT=--
PARM MODE     TYPE=INTEGER  COUNT=0:1	VALID=(0:7)	DEFAULT=--
PARM EXPO     TYPE=REAL     COUNT=0:1			DEFAULT=--
PARM DCCAM    TYPE=INTEGER  COUNT=0:1	VALID=(4:7)	DEFAULT=--
PARM DCGAIN   TYPE=INTEGER  COUNT=0:1	VALID=(0,1) 	DEFAULT=--
PARM DCSCAN   TYPE=INTEGER  COUNT=0:1			DEFAULT=--
PARM DCMODE   TYPE=INTEGER  COUNT=0:1	VALID=(0:7)	DEFAULT=--
PARM NUMDC    TYPE=INTEGER  COUNT=1	VALID=(1:1000)	DEFAULT=1
PARM PLANET   TYPE=KEYWORD  COUNT=0:1 +
		VALID=(JUPITER,SATURN,URANUS,NEPTUNE)   DEFAULT=--
PARM OFORMAT  TYPE=KEYWORD  COUNT=0:1	VALID=(BYTE,HALF,REAL)+
							DEFAULT=HALF
PARM NOCORREC TYPE=KEYWORD  COUNT=0:1	VALID=NOCORREC	DEFAULT=--
PARM NOCHECK  TYPE=KEYWORD  COUNT=0:1	VALID=NOCHECK	DEFAULT=--
PARM NONEG    TYPE=KEYWORD  COUNT=0:1	VALID=NONEG	DEFAULT=--
PARM MAXDN    TYPE=INTEGER  				DEFAULT=32767
PARM LC       TYPE=KEYWORD  COUNT=0:1	VALID=LC	DEFAULT=--
PARM DIV      TYPE=REAL     COUNT=0:1			DEFAULT=--
PARM SCF      TYPE=STRING   COUNT=0:1 DEFAULT="vgrscf.dat"
PARM DBUG     TYPE=KEYWORD  COUNT=0:1	VALID=DBUG 	DEFAULT=--
PARM SCALE    TYPE=REAL     COUNT=0:1			DEFAULT=--
END-PROC
.TITLE
"ficor77" -- Voyager radiometric correction program
.HELP
PURPOSE:

ficor77 is a Vicar applications program which removes vidicon camera
induced radiometric distortion from Voyager images.

EXECUTION STATEMENT:

    ficor77 INP=(PIC,C1,C2,...,D1,D2,...) OUT=OPIC  user-parameters...

The input files consist of the image to be corrected (PIC), radiometric
files (C1,C2,...) as generated by the program MJSPFCF, and dark-current
frames (D1,D2,...).  PIC must appear as the first input.  The remaining
input files may appear in any order.  At least one radiometric file and
dark-current frame must be specified.

PIC must be an 800x800 (geometrically uncorrected) Voyager image in byte
data format.  The dark-current may consist of a single frame or the sum
of multiple dark-current exposures (byte or halfword format).  If a
summed dark-current is input, the parameter NUMDC must be used to
specify the number of frames summed together.

The output file (OPIC) consists of the radiometrically corrected image.
OPIC is normally output in halfword data format.  To specify byte or
floating point output data format, use the OFORMAT parameter to specify
BYTE or REAL.

.page
BACKGROUND INFORMATION:

"ficor77" will radiometrically correct the input image, converting the input
DN values to some specified radiometric unit.  The program requires a
radiometric file containing calibration data acquired via the same camera
and filter position as the input image (PIC), and dark-current data acquired
via the same camera and scan-rate.

The dark-current (offset) consists of thermally induced charge which must
be subtracted from the signal.  This charge accumulates on the vidicon,
and increases linearly with time.  Dark-current build-up occurs during the
time between frame erasure and read-out.  It is therefore a function of
image scan-rate.  Also, simultaneous-exposure wide-angle frames will have
a greater dark-current build-up than normal-exposure frames since they
are read out during the next frame cycle (the narrow-angle frame is read
out first).

To measure the dark-current, zero-exposure (dark-current) frames are
acquired during cruise and encounter.  Because the rate of dark-current
build-up is time variant and highly sensitive to temperature changes,
the user should specify a dark-current frame with an FDS time reasonably
close to the FDS time of the input image.

The radiometric file contains calibration data obtained from a light-transfer
sequence (see program MJSPFCF for details).  For each pixel, the file
contains the nominal camera output S0,S1,S2,...,Sn (in units of DN) at each
of a sequence of exposures E0,E1,E2,...,En (in foot-Lambert-msecs).

The data samples S0,S1,S2,...,Sn are stored in the file for the low-gain
state, and have been scaled up by a factor of 9 as a result of application
of a 3x3 low-pass filter (used to suppress shot noise).  The dark-current
is subtracted from these data samples before insertion into the file.
(See the documentation of VICAR program MJSPFCF for details).

The first record of the radiometric file contains the number of data points
in the file (n), and luminance values L0,L1,L2,...,Ln corresponding to
exposures E0,E1,E2,...,En for a reference shutter speed of 1 second.
The remaining records contain the data samples Si for each pixel.  Each
ordered pair (Li,Si) comprises a data point on the light-transfer curve which
characterizes that pixel's response to exposure to a light source.

.page
OPERATION:

"ficor77" assumes that the first input file is the image to be radiometrically
corrected (PIC).  Upon entry, "ficor77" extracts the camera ID (serial number),
filter position, gain-state, scan-rate, and camera mode from the input picture
label.  (These values may be overridden via optional parameters CAMERA,
FILTER, GAIN, SCAN, and MODE.)

The remaining inputs are a list of radiometric and dark-current files, which
may appear in any order.  The program automatically selects the correct
radiometric file and dark-current frame by scanning this list for a radiometric
file with matching camera ID and filter, and a dark-current frame with matching
camera ID and scan-rate, as indicated in the picture labels for these files.
(The camera ID, gain-state, scan-rate, and operating mode obtained from the
dark-current picture label may be overridden via optional parameters DCCAM, 
DCGAIN, DCSCAN, and DCMODE.)

For each pixel of the input image, the radiometric correction process
consists of the following steps:

  1) The DN values D and DC for the pixel are extracted from the input
     image and dark-current frame respectively.  If the input image
     (or dark-current) was taken in high-gain, the DN value is converted
     to equivalent low-gain DN by dividing by the gain ratio (see GAIN
     parameter).  The pixels are also scaled by 9 (see DIV parameter) to
     match the scale of the samples in the radiometric file.  If the
     input dark-current frame represents a sum of multiple dark-currents,
     the dark-current DNs are scaled by dividing by the number of frames
     summed together (see NUMDC parameter).

  2) The dark current is removed:  S = D - DC

  3) The resulting signal (S) is converted to luminance:  A piece-wise
     linear model of the light-transfer function is used, such that each
     data point (Li,Si) is assumed to lie on the curve, and the curve is
     assumed to be linear between data points.  Let
		S  < S < S
		 i        i+1
     The signal is converted to luminance by interpolating between the
     data points:

		    L    - L 
		     i+1    i
		L = -----------(S - S ) + L
		    S    - S         i     i
	             i+1    i

     where L is in units of foot-Lamberts.

  4) The output pixel is scaled to radiometric units R (Ref. 4).  The
     user determines the output picture scale by specifying one of the
     following (see IOF and CONV parameters.):

	A1 = number of 'I over F' units per DN, where 10,000 I/F units
             would be produced by normal incidence of sunlight on a 
	     a Lambert disk at the planet's distance from the sun.

 	A2 = number of nanowatts per cm**2 per steradian per nanometer
             wavelength per DN.

     If A1 is specified, then

		        to     S1	
		R = L * --- * ---- * (D/5.2)**2
			 t     A1
     where

	to = reference shutter speed of the data points (msec),
	t  = shutter speed of the input image (msec).
	S1 = filter-dependent conversion factor from ft-Lamberts to
	     I/F units for a reference distance of 5.2 AU from the Sun.
	D  = planet distance from the Sun (in AU).  D is 5.2 AU for Jupiter,
	     9.51 AU for Saturn, 19.122 AU for Uranus, 30.21 AU for Neptune.

     For Voyager 2, the planet-of-encounter is determined by searching the
     frame label for a valid PICNO, Spacecraft-Event-Time (SCET), or Earth-
     Received-Time (ERT), in that order.  This may be overridden via the
     PLANET keyword.

     For Voyager 1, the planet-of-encounter is always set to Jupiter, and this
     may not be overridden.

     NOTE: Prior to March 1988, the correction for solar distance (D/5.2)**2
     was not applied, and all frames were scaled to Jupiter's distance from
     the sun.  The program "fixvgr" was used to scale the frames to Saturn or
     Uranus, and to correct for errors in the ground calibration values of
     S1 (see below).

     If A2 is specified, then

			 S2
		R = L * ----
			 A2

     where S2 is the filter-dependent conversion factor from ft-Lamberts
     to units of nanowatts/cm**2/steradian/nanometer.

     For images taken during ground calibration, the keyword LC should be
     used to specify Xenon light-cannon (rather than solar) illumination.
     Values for S2 are retrieved from tables stored in the program for
     each camera-filter combination for both solar and light-cannon
     illumination.

  5) The output DN is converted to the specified data format.  For byte
     output, the DN range is ALWAYS 0 to 255.  Values outside this range are
     truncated to 0 or 255, as appropriate.   For halfword (16-bit integer)
     and real output, the default range is -32768 to +32767 (i.e. the full
     halfword range).  Although a negative DN value has no physical meaning,
     this may occur if the dark-current frame has a higher dark-current
     level than that of the input image.  Negative DNs may be set equal
     to zero via the NONEG keyword.  The upper limit may be modified
     via the MAXDN parameter.

.page
OUTPUT SCALE CORRECTION:

As of March 1988, the output DN scale is corrected using factors derived
from analysis of calibration plaque data (see Ref. 4) and comparisons with
ground-based data (see Ref. 5).  This scale correction may be suppressed
via the NOCORRECT parameter.

The following scale correction is applied to each pixel:

			R' = S*R

where the constant S is a function of camera and filter position.
S is retrieved from a table stored in the Scale Correction File (SCF).
This file may be specified by the SCF parameter.  (Note: The constants
in the file have the target-to-Sun distance correction built into them).

The scale correction is identical to the correction applied by program
"fixvgr", except that "fixvgr" rescales the output DN values to 1 'IOF' unit
per DN.  Images which have been scale-corrected by "ficor77" need not be
processed by "fixvgr".  If processed by "fixvgr", no re-scaling is done unless
the constant has been changed.  Images scale-corrected by "ficor77" may be
identified by the following picture label:

  (e.g) COMMENT=' PICTURE MULTIPLIED BY  0.93   FICOR	2/02/86 VERSION'
	SCALE='URANUS'

where 0.93 is the constant S, URANUS is the planet-of-encounter, and
2/02/86 is the version date of the SCF.  See the help file for program
"ficorgen" for source documentation of the current and past scale correction
values.

.page
PARAMETERS:

All user parameters are optional, and are entered in one of the following
forms:
		KEYWORD=string
		KEYWORD=n
		KEYWORD=r
		'KEYWORD
where 'string' is an ASCII character string, 'n' is an integer value and
'r' a real value.  The following is a summary of the parameters.  Use the
TUTOR mode for a more complete description.

	CAMERA STATE PARAMETERS
	-----------------------
	EXPO=r		- Override input image exposure time (msec)
	FILTER=n        - Override input image filter position (-1<n<8)
	GAIN=n		- Override input image gain (N=0 low, N=1 high)
	MODE=n		- Override input image mode (1-7)
	SCAN=n		- Override input image scan rate
	CAMERA=n	- Override input image camera serial number

	DCGAIN=n	- Override dark current gain
	DCMODE=n	- Override dark current mode
	DCSCAN=n	- Override dark current scan rate
	DCCAM=n		- Override dark current image camera serial no.

	'NOCHECK	- Bypass matching of camera parameters; the calibration
			  file must be the second input and the dark-current
			  must be the third input.

	OUTPUT IMAGE SCALE
	------------------
	CONV=r		- Number of nanowatts per cm*2 per steradian per
			  nanometer wavelength per output DN.
	IOF=r		- Number of "I over F " units per output DN.
        SCL=string	- Scale Correction File
	'planet		- where planet is JUPITER, SATURN, URANUS, or NEPTUNE.
	'NOCORRECT	- Suppress FIXVGR scale correction

	DATA FORMAT KEYWORDS
	--------------------
	'BYTE		- Output image in byte format
	'HALF		- Output image in halfword format
	'REAL		- Output image in floating point format
	'NONEG		- Set all negative output DN to zero
	MAXDN=n 	- Maximum DN in the output image
	NUMDC=n		- Number of dark-current frames summed together

	MISCELLANEOUS
	-------------
	'DBUG		- Some diagnostics output
	'LC		- Input image taken under light-cannon illumination
	DIV=r		- Calibration file scale factor

.page
EXAMPLES:

"ficor77" is most commonly used as follows:

	ficor77 (PIC,CAL,DC) OPIC GAIN=0

The calibration file (CAL) must be for the same camera and filter as PIC.
The dark-current file (DC) must be for the same camera and scan-rate.
The output frame (OPIC) will be in halfword (16-bit integer) format.

The value of IOF is chosen to maximize use of the output 16-bit DN range
(0-32767).  The default value for IOF (IOF=1) is appropriate for most
targets.  For targets of low albedo (e.g. Saturn's rings), IOF is chosen
within the range  0.01 < IOF < 1.0.

Most of the remaining parameters are used to work around occasional errors
in picture label information.  Since high-gain images were acquired for
only a small percentage of VGR-1 Jupiter frames, the parameter GAIN=0 is
regularly used to override the camera gain-state value to low-gain.

.page
ERROR MESSAGES:

All error messages are preceded by three asterisks (***).  An error is fatal
only if it is immediateley followed by the message:
		"***FICOR task cancelled"

TIMING:

"ficor77" requires 21 CPU seconds on the VAX 8600.  It will complete within
30 seconds (wall-clock) on a single-user system and about 3 minutes on a
system supporting 20 users.  The program reports its progress by printing
the line count at 100-line intervals.

.page
REFERENCES:

1) JPL Technical Memorandum 33-628 "A User's Guide to the Mariner 9
   Television Reduced Data Record", 1973.

2) JPL Document 618-802 "Voyager Imaging Science Subsytem Calibration
   Report", M. Benesh and P. Jepsen, 31 July 1978.

3) P. Jepsen, et.al., "Voyager Image Processing at the Image Processing
   Laboratory", Journal of the British Interplanetary Society, Vol. 33,
   pp 315-322, 1980.

4) G.E.Danielson, et.al., "Radiometric Performance of the Voyager Cameras",
   JGR Vol 86, NO.A10, pp 8683-8689, 30 Sep 1981.

5) T.V. Johnson, "Corrections to Danielson et.al. Calibration", memorandum
   to the Voyager Imaging Team, 16 Jan 1986.

.page
PROGRAM HISTORY:

Written by: Joel A. Mosher	Jan 78
UCL Implementors: M.L. Kendall & R.F.T. BARREY	Jan 82
Cognizant programmer: Gary Yagi
Revisions:
  21 Jul 14  WLB  ...Initialized camera mode variables MODE and MODEDC to
                     get consistent results between linux and sun.
  24 May 99  GMY  ...Corrected problem with negative and 0 DNs.
  09 Nov 98  GMY  ...Updated test script to point to where test files are.
  31 Oct 94  AMS  ...(CRI) Made portable for UNIX
  09 Jul 93  GMY  ...Check for input image with zero exposure (FR 79136)
  24 May 92  GMY  ...Update test script (FR 64507)
  21 DEC 88  GMY  ...Fix processing of parameters passed via procedures.
  07 JUN 88  GMY  ...Fix processing of EXPO keyword.
  25 FEB 88  GMY  ...Fix SCAN=10, bad mode (FR 33244)
  01 FEB 88  GMY  ...Major code clean-up and enhancements:
			0) Rewrite help file.
			1) Add FIXVGR scale correction factors.
			2) Delete redundant keywords SN,FP,HSAT,SUN
			3) Delete null keyword FICOR
			4) Fix OUTREAL keyword
			5) Fix bug reading last Cal-File record.
			6) Modified processing of negative DN (LCOR)
			7) Delete FORMAT keyword
			8) Fix bug in processing high-gain pictures
			9) Accept input of summed dark-current frames.
  01 OCT 85  FFM  ...Change to VICAR2 I/O and convert subroutine
	             LCOR from fortran to assembler
RESTRICTIONS:

The vgrscf.dat input file is generated on the MIPLs only.
See "fixvgr" for more information.

.LEVEL1
.VARIABLE INP
3 to 10 files: Input image,
radiometric file(s), and
dark-current frame(s).
.VARIABLE OUT
Output corrected image
.VARIABLE SIZE
4 integers--optional
Standard Vicar size field 
.VARIABLE SL
Integer--optional
Starting line
.VARIABLE SS
Integer--optional
Starting sample
.VARIABLE NL
Integer--optional
Number of Lines
.VARIABLE NS
Integer--optional
Number of Samples
.VARIABLE CAMERA
Integer--optional
Camera serial number
(4,5,7, or 8)
.VARIABLE FILTER
Integer--optional
Filter position (0 to 7)
.VARIABLE GAIN
Integer--optional
Gain state (Low=0,High=1) 
.VARIABLE SCAN
Integer--optional
Scan rate (1 to 10)
.VARIABLE MODE
Integer--optional
Camera mode (1 to 7)
.VARIABLE EXPO
Real--optional
Exposure time (msec)
.VARIABLE DCCAM
Integer--optional
Camera serial number 
of dark-current file
.VARIABLE DCGAIN
Integer--optional
Dark-current gain-state
(Low=0,High=1)
.VARIABLE DCSCAN
Integer--optional
Dark-current scan-rate
.VARIABLE DCMODE
Integer--optional
Dark-current camera mode
.VARIABLE NUMDC
Integer--optional
Number of dark-current
frames summed together
.VARIABLE IOF
Real--optional
I over F units per DN 
.VARIABLE CONV
Real--optional
Number of nanowatts per
cm**2 per steradian per
nanometer wavelength per DN 
.VARI PLANET
Keyword--optional
Planet-of-encounter
Valid values are:
Jupiter, Saturn,
Uranus, or Neptune.
.VARIABLE OFORMAT
KEYWORD--OPTIONAL
Output image data format
Valid: BYTE,HALF,REAL
.VARIABLE MAXDN
INTEGER--OPTIONAL
Maximum DN for output
.VARIABLE NOCHECK
KEYWORD--OPTIONAL
Suppresses matching
of camera parameters
.VARIABLE NONEG
KEYWORD--OPTIONAL
Negative dn's set to zero
.VARIABLE LC
KEYWORD--OPTIONAL
Specifies xenon light-cannon
illumination
.VARIABLE DIV
REAL--OPTIONAL
Number of pixels in
low pass filter
.VARIABLE NOCORREC
KEYWORD--OPTIONAL
Do not correct scale.
.VARIABLE SCF
STRING--OPTIONAL
Scale Correction File.
.VARIABLE SCALE
(Not implemented)
.LEVEL2
.VARIABLE INP
	INP=(PIC,C1,C2,...,D1,D2,...)
The input files consist of the image to be corrected (PIC), radiometric
files (C1,C2,...) as generated by the program MJSPFCF, and dark-current
files (D1,D2,...).  PIC must appear as the first input.  The remaining input
files may appear in any order.  At least one radiometric and dark-current
file must be specified.

PIC must be an 800x800 (geometrically uncorrected) Voyager image in byte
data format.  The dark-current may consist of a single frame or the sum
of multiple dark-current exposures (byte or halfword format).  If a
summed dark-current is input, the parameter NUMDC must be used to
specify the number of frames summed together.

If more than one radiometric file are specified, "ficor77" will select
the correct file by matching the camera serial number and filter position,
as extracted from the VGR frame label.  If more than one dark-current file
are specified, "ficor77" will select the correct file by matching the camera
serial number and scan-rate.
.VARIABLE OUT
    Ex:	OUT=OPIC
The output file (OPIC) consists of the radiometrically corrected image.
OPIC is normally output in halfword format.  Use the OFORMAT parameter
to specify BYTE or REAL output.
.VARIABLE SIZE
    Ex: SIZE=(sl,ss,nl,ns)
The standard Vicar size field (starting line, starting sample, number of
lines, number of samples).
.VARIABLE FILTER
	Ex: FILTER=4
Specifies the camera filter position for the input image.  The filters are
mounted on an 8-position filter wheel as follows:

	#  Narrow-Angle   Wide-Angle
	-  ------------   ----------
	0  Clear	  Methane (J-S-T)
	1  Violet	  Blue	  
	2  Blue		  Clear
	3  Orange	  Violet
	4  Clear	  Na-D
	5  Green	  Green
	6  Green	  Methane (U)
	7  Ultraviolet	  Orange

This parameter overrides the filter position extracted from the label of
the input image.  Note that the filter position must agree with the
filter position of the radiometric file.
.VARIABLE SCAN
	Ex:  SCAN=5	(specifies 5:1 scan-rate)
Specifies the scan-rate of the input image.  Valid scan-rates are 1,2,3,5,
and 10.  This parameter overrides the scan-rate extracted from the label
of the input image.  Note that the scan-rate of the input image must agree
with the scan-rate of the dark-current frame.
.VARIABLE DCSCAN
	Ex:  DCSCAN=5	(specifies 5:1 scan-rate)
Specifies the scan-rate of the dark-current frame.  Valid scan-rates are 1,2,3,]
5, and 10.  This parameter overrides the scan-rate extracted from the label
of the dark-current frame.

.VARIABLE CAMERA
  Ex: CAMERA=4
Specifies the camera ID (serial number) of the input image (PIC).  This
overrides the camera ID extracted from the image's picture label.  The
camera serial numbers of the Voyager spacecraft are:

	4 = VGR-2 WA		6 = VGR-1 WA
	5 = VGR-2 NA		7 = VGR-1 NA

The camera ID for the input image must agree with the camera ID of the
radiometric file and dark-current frame.
.VARIABLE GAIN
Specifies the camera gain-state of the input image (PIC).  This overrides
the camera ID extracted from the image's picture label.  Valid values are:
	GAIN = 0  for low gain
	GAIN = 1  for high gain
The calibration data in the radiometric file are stored for the low-gain
state only.  If high-gain images or dark-current frames are input, the
DN values are converted to equivalent low-gain DN by dividing by the
gain-ratio between high and low gain for the appropriate camera:
		S/N 4   -   3.7642
		S/N 5   -   4.2531
		S/N 6   -   3.9424
		S/N 7   -   3.9240
.VARIABLE DCCAM
  Ex: CAMERA=4
Specifies the camera ID (serial number) of the dark-current frame.  This
overrides the camera ID extracted from the picture label of the dark-current
frame.  The camera serial numbers of the Voyager spacecraft are:

	4 = VGR-2 WA		6 = VGR-1 WA
	5 = VGR-2 NA		7 = VGR-1 NA
.VARIABLE DCGAIN
Specifies the camera gain-state of the dark-current frame.  This overrides
the camera ID extracted from the picture label of the dark-current frame.
Valid values are:
	GAIN = 0  for low gain
	GAIN = 1  for high gain
.VARIABLE NONEG
Specifies that all resultant DNs which are negative will be set to zero.
This keyword is a relic of the IBM days.  At one time, GEOMA would die
whenever it ingested a negative DN.
.VARIABLE MODE
Overrides the camera mode extracted from the input picture label.  Valid
values are:
      MODE     TITLE
       1      NOSHUT
       2      NA ONLY
       3      WA ONLY
       4      BOTALT
       5      BSIMAN
       6      BODARK
       7      BOTSIM
.VARIABLE DCMODE
Overrides the camera mode extracted from the picture label of the dark-
current frame.  Valid values are:
      MODE     TITLE
       1      NOSHUT
       2      NA ONLY
       3      WA ONLY
       4      BOTALT
       5      BSIMAN
       6      BODARK
       7      BOTSIM
.VARIABLE NUMDC
	NUMDC=n
Specifies that the input dark-current represents the sum of n dark-current
exposures.
.VARIABLE EXPO
Specifies the exposure time of the input frame in milliseconds.  This value
overrides the exposure time extracted from the input picture label.
.VARIABLE IOF
	IOF=r
where r specifies the output DN scale in number of 'I over F' units per DN.
10,000 'I over F' units would be produced by normal incidence of sunlight
on a Lambert disk at Jupiter's distance from the sun (5.2 A.U.).
Default is IOF = 1.0.  If CONV is specified, do not specify IOF.
.VARIABLE CONV
	CONV=r
where r specifies the output DN scale in number of nanowatts per cm**2 per
steradian per nanometer wavelength per DN.
The default is IOF=1.0 and CONV is calculated.  If IOF is specified, do not
specify CONV.
.VARI PLANET
KEYWORD--OPTIONAL
Specifies the planet-of-encounter.  Valid values are Jupiter, Saturn, Uranus,
or Neptune.

For Voyager 1, the planet-of-encounter is always set to Jupiter, and this
keyword is ignored (only Jupiter scale factors are available for VGR-1).

If defaulted for Voyager 2, the planet-of-encounter is determined by
searching the frame label for a valid PICNO, Spacecraft-Event-Time (SCET)
or Earth-Received-Time (ERT), in that order.  If the fifth character of the
PICNO is a 'J','S','U', or 'N', this is accepted as a valid planet ID.
Otherwise, if the SCET or ERT year is greater than 74, the planet is determined
as follows:

	YEAR		PLANET	   VGR-1 Encounter   VGR-2 Encounter
	--------	------	   ---------------   ---------------
	less than 80	Jupiter	     Mar  5 79		Jul  9 79
	80 to 84	Saturn	     Nov 12 80		Aug 25 81
	85 to 87	Uranus				Jan 24 86
	88 and beyond	Neptune				Aug 24 89
.VARIABLE SCALE
	SCALE=s
Used to specify the output DN scale.  SCALE is currently not implemented as
either IOF or CONV is in effect.
.VARIABLE AUTO
Specifies automatic scaling of the output DNs.  AUTO is currently not
implemented as either IOF or CONV is in effect.  AUTO activates a FICOR77
mode which scans the input picture, sampling every tenth line to
determine the mean DN of the picture.  The program selects the appropriate
value of SCALE in order to output a picture with a mean DN approximating
the mean DN of the input picture.
.VARIABLE THRESH
THRESH is an integer specifying the lower threshold to be used in the
computation of the mean DN (see AUTO keyword).  Since AUTO is not
implemented, THRESH has no effect.
.VARIABLE OFORMAT
OFORMAT is a keyword which specifies the data format of the output picture.
Valid values are BYTE, HALF, and REAL, where
	 BYTE specifies 8 bit integer format.
	 HALF specifies 16 bit integer format.
	 REAL specifies floating point format.
The default is HALF.
.VARIABLE MAXDN
 MAXDN specifies the maximum DN in the output
 picture.
 Default is : 255 for byte data
              32767 for 16-bit or real data
.VARIABLE LC
LC specifies the xenon light-cannon as the light source for the input image.
(The light-cannon was used during ground camera calibration).  If not
specified, solar illumination is assumed.
.VARIABLE NOCHECK
NOCHECK specifies that no label checking will be done to match the camera
parameters between the input frame (PIC) and the radiometric file and dark-
current frame.  The radiometric file is assumed to be the second input file
and the dark-current frame is assumed to be the third input file:
	Ex:  INP=(PIC,CAL,DC)
If defaulted, "ficor77" will attempt the find a radiometric file from the
same camera and filter, and a dark-current frame from the same camera and
scan-rate.  The program will terminate (ABEND) if unsuccessful.
.VARIABLE DIV
The DN values in the radiometric file are scaled by a factor n due to
application of a low-pass filter (of unit weights) on the frames input to
program MJSPFCF.  DIV is used to specify this scaling:
	DIV=n
where n is an integer specifying the number of pixels in the filter area.
Normally, a 3x3 filter window is used so DIV=9.  If DIV is not specified,
the scale factor is determined from the picture label of the radiometric
file.
.VARIABLE NOCORREC
KEYWORD--OPTIONAL
Valid: 'NOCORREC
Specifies that no scale correction is to be done.  If both 'NOCORRECT and
'NONEG are specified, the output image is the same as the input image except
that all negative DNs have been set to zero.  If 'NOCORRECT is specified but
'NONEG is not specified, the output image is an exact copy of the input
image.  In practice, the NONEG and NOCORRECT keywords are no longer used.
.VARIABLE SCF
STRING--OPTIONAL
	Ex:  SCF="MIPL:[MIPL.VGR]VGRSCF.DAT"
Specifies the Scale Correction File, as generated by program "ficorgen".
Note that since this file is rarely updated, the default usually points to
the most recent file and this parameter is normally not specified.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstficor77.pdf
!TEST OF FICOR77:
!  Tests both nanowatts and I/F scaling, scale correction, and compatibility
!  with FIXVGR.
procedure
refgbl $autousage
refgbl $syschar
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
local path type=string init="wms_test_work:[testdata.mipl.vgr]"
if ($syschar(1) = "UNIX")
    let path="/project/test_work/testdata/mipl/vgr/"
end-if

write "***** Test nanowatts scale *****"
ficor77 (&"path"f1636832.raw,&"path"ficor77.cal,&"path"dc.cal) +
                A (500,500,10,10) 'NOCO CONV=1.
fixvgr A B scf=&"path"vgrscf.dat
label-l B
ficor77 (&"path"f1636832.raw,&"path"ficor77.cal,&"path"dc.cal) +
        C (500,500,10,10) CONV=1. scf=&"path"vgrscf.dat
list A		!CONV=1.
list B		!CONV=1. and scale corrected
list C		!Same as C

write "*****Test I/F scale*****"
ficor77 (&"path"f1636832.raw,&"path"ficor77.cal,&"path"dc.cal) +
                A (500,500,10,10) IOF=.5 'NOCO
fixvgr A B scf=&"path"vgrscf.dat
label-l B
ficor77 (&"path"f1636832.raw,&"path"ficor77.cal,&"path"dc.cal) +
             C (500,500,10,10)scf=&"path"vgrscf.dat
label-l C
list A		!IOF=.5
list B		!IOF=1.0 and scale corrected
list C		!Same as B
end-proc
$!-----------------------------------------------------------------------------
$ create tstficor77.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

local path type=string init="wms_test_work:[testdata.mipl.vgr]"
if ($syschar(1) = "UNIX")
    let path="/project/test_work/testdata/mipl/vgr/"
end-if
write "***** Test nanowatts scale *****"
***** Test nanowatts scale *****
ficor77 (/project/test_work/testdata/mipl/vgr/f1636832.raw,/proj+
ect/test_work/testdata/mipl/vgr/ficor77.cal,/project/test_work/testdata/mipl/vgr/dc.cal)                  A (500,500,10,10) 'NOCO C+
ONV=1.
Beginning VICAR task ficor77
FICOR77 version 24 May 1999
Dark-Current File input=          3
Calibration File input=          2
DARK-CURRENT LABEL...

          77                   800     800 800 800 L 1
          VGR-1   FDS 13852.12   PICNO 0767J1-085   SCET 78.344 22:18:59
          NA CAMERA  EXP 00360.0 MSEC  FILT 1(VIOLET)  LO GAIN  SCAN RATE  1:1
          ERT 78.344 22:53:05   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C
          IN/011340/22 OUT/******/**                 DSS #**   BIT SNR    5.781
           000E5 A/47000BD7 B/68FF C/4C7F D/007F0000 ETLM/CC00A1CA6F6ADEA11F12Q
          NA OPCAL 00(*****.* MSEC) PIXAVG 000/0 OPERATIONAL MODE 2(NAONLY)
          CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE
          NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL
          WA   NO   PREP  NO    YES   NO    NO    NO    NO    0 S  7 NORMAL
          ADJACENT LINE PIXELS CHANGED      5. SAME LINE PIXELS CHANGED     9
          ADESPIKE          12 23 78 13:42:57          CCA31
          ADESPIKE
          ADJACENT LINE PIXELS CHANGED      1. SAME LINE PIXELS CHANGED     1
          RESSAR77          12 23 78 13:46:19          CCA31
          RESSAR77
          INSERT            12 23 78 13:53:46          CCA31
          SAR               12 27 78 08:57:55          CCA31
          1 1 4 800
          SAR
          SPOTREM           12 27 78 08:59:20          CCA31
          MINRAD 7 SPOT 4 380 238 738 434 714 634 36 564
          SPOTREM
          INSERT            12 27 78 09:02:02          CCA31
          INSERT            08 09 80 15:47:45          CCA317
CALIBRATION FILE LABEL...

          77                  2001    640020016400 I 2
          MJS77 PHOTOMETRIC DATA FILE     S/N=7  1 CAMERA FILTER POSITION 0
          DATE GENERATED  12/14/78  FRAMES LOW PASS FLTRD,DIV BY       9.000
          SCAN RATE= 1   GAIN =0     TEMP  0.00   DEG. C  EXPOSURE *60  MSEC
          INTAP=ISS241G  OUTAP=JS0565   FILE= 3
             TEMP   32/1  31/2  27/3  29/0




DIV=  9.0000000
  CAMERA PARAMETERS...
                INPUT   DARK    CALIBRATION
                FRAME   CURRENT FILE
     S/N          7       7       7
     FILTER       0       1       0
     SCANRATE     1       1       1
     GAIN         0       0       0
     MODE       XXXXXXX  NA ONLY
MAXL and luminances of calibration frames--

    120.00      0.00      1.44      3.08      6.16     12.30     24.60     37.00     49.30     74.00     98.00
Exposure time=  360.00000
Radiance refers to solar spectrum
FtL-sec to nanowatt conv factor=  3.3390000
Ft-L to I/F conv factor=  16.510000
Conversion factor specified
Mean DN of DC=  10.930000
Minimum value of a saturated pixel=      32767
Maximum value of an unsaturated pixel=        793
Total number of saturated pixels=          0
Total number of negative pixels=          0
FICOR task completed
fixvgr A B scf=/project/test_work/testdata/mipl/vgr/vgrscf.dat
Beginning VICAR task fixvgr
FIXVGR Version 31-OCT-94
 Camera=          7
 Filter=          0
SECOND CONV FACTOR =  4.9446E+00
 Correction factor=  4.6034136
FIXVGR task completed
label-l B
Beginning VICAR task label
************************************************************
 
        ************  File B ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: CONV12 -- User: DFS -- Thu May  3 11:31:13 1984 ----
LAB01=
'77                   800     800 800 800 L 1                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/JS4459/02                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'INSERT            05 06 83 13:12:50          JAM                      GL'
NLABS=11
---- Task: FICOR77 -- User: wlb -- Mon Jul 21 13:32:06 2014 ----
LABEL1='FICOR77  MINSAT=32767 NUMSAT=     0'
LABEL2='FOR NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY     1.00000'
LABEL3='FOR (I/F)*10000., MULTIPLY DN VALUE BY               4.94459'
LABEL4='FICOR77  DARK CURRENT FDS = 13852.12'
---- Task: FIXVGR -- User: wlb -- Mon Jul 21 13:32:06 2014 ----
COMMENT=' PICTURE MULTIPLIED BY    4.60      FIXVGR     8/06/90 VERSION'
SCALE='JUPITER'
 
************************************************************
ficor77 (/project/test_work/testdata/mipl/vgr/f1636832.raw,/proj+
ect/test_work/testdata/mipl/vgr/ficor77.cal,/project/test_work/testdata/mipl/vgr/dc.cal)          C (500,500,10,10) CONV=1. scf=/pr+
oject/test_work/testdata/mipl/vgr/vgrscf.dat
Beginning VICAR task ficor77
FICOR77 version 24 May 1999
Dark-Current File input=          3
Calibration File input=          2
DARK-CURRENT LABEL...

          77                   800     800 800 800 L 1
          VGR-1   FDS 13852.12   PICNO 0767J1-085   SCET 78.344 22:18:59
          NA CAMERA  EXP 00360.0 MSEC  FILT 1(VIOLET)  LO GAIN  SCAN RATE  1:1
          ERT 78.344 22:53:05   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C
          IN/011340/22 OUT/******/**                 DSS #**   BIT SNR    5.781
           000E5 A/47000BD7 B/68FF C/4C7F D/007F0000 ETLM/CC00A1CA6F6ADEA11F12Q
          NA OPCAL 00(*****.* MSEC) PIXAVG 000/0 OPERATIONAL MODE 2(NAONLY)
          CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE
          NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL
          WA   NO   PREP  NO    YES   NO    NO    NO    NO    0 S  7 NORMAL
          ADJACENT LINE PIXELS CHANGED      5. SAME LINE PIXELS CHANGED     9
          ADESPIKE          12 23 78 13:42:57          CCA31
          ADESPIKE
          ADJACENT LINE PIXELS CHANGED      1. SAME LINE PIXELS CHANGED     1
          RESSAR77          12 23 78 13:46:19          CCA31
          RESSAR77
          INSERT            12 23 78 13:53:46          CCA31
          SAR               12 27 78 08:57:55          CCA31
          1 1 4 800
          SAR
          SPOTREM           12 27 78 08:59:20          CCA31
          MINRAD 7 SPOT 4 380 238 738 434 714 634 36 564
          SPOTREM
          INSERT            12 27 78 09:02:02          CCA31
          INSERT            08 09 80 15:47:45          CCA317
CALIBRATION FILE LABEL...

          77                  2001    640020016400 I 2
          MJS77 PHOTOMETRIC DATA FILE     S/N=7  1 CAMERA FILTER POSITION 0
          DATE GENERATED  12/14/78  FRAMES LOW PASS FLTRD,DIV BY       9.000
          SCAN RATE= 1   GAIN =0     TEMP  0.00   DEG. C  EXPOSURE *60  MSEC
          INTAP=ISS241G  OUTAP=JS0565   FILE= 3
             TEMP   32/1  31/2  27/3  29/0




DIV=  9.0000000
  CAMERA PARAMETERS...
                INPUT   DARK    CALIBRATION
                FRAME   CURRENT FILE
     S/N          7       7       7
     FILTER       0       1       0
     SCANRATE     1       1       1
     GAIN         0       0       0
     MODE       XXXXXXX  NA ONLY
MAXL and luminances of calibration frames--

    120.00      0.00      1.44      3.08      6.16     12.30     24.60     37.00     49.30     74.00     98.00
Exposure time=  360.00000
Radiance refers to solar spectrum
FtL-sec to nanowatt conv factor=  3.3390000
Ft-L to I/F conv factor=  16.510000
Conversion factor specified
Scale correction factor= 0.93099999
Mean DN of DC=  10.930000
Minimum value of a saturated pixel=      32767
Maximum value of an unsaturated pixel=        738
Total number of saturated pixels=          0
Total number of negative pixels=          0
FICOR task completed
list A
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FICOR77   User:wlb       Date_Time:Mon Jul 21 13:32:06 2014
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       746   750   740   733   732   754   758   775   793   760
      2       739   750   742   745   725   736   747   748   754   731
      3       753   732   724   724   744   759   753   732   708   719
      4       720   705   720   707   732   759   755   720   713   727
      5       731   726   707   723   749   748   739   753   745   717
      6       748   727   728   751   754   760   756   754   741   729
      7       753   742   764   771   760   751   751   745   753   747
      8       737   754   768   776   754   768   751   761   766   755
      9       747   758   770   772   772   768   767   773   773   779
     10       757   764   776   773   767   771   759   765   763   769
list B
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FIXVGR    User:wlb       Date_Time:Mon Jul 21 13:32:06 2014
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      3434  3453  3407  3374  3370  3471  3489  3568  3651  3499
      2      3402  3453  3416  3430  3337  3388  3439  3443  3471  3365
      3      3466  3370  3333  3333  3425  3494  3466  3370  3259  3310
      4      3314  3245  3314  3255  3370  3494  3476  3314  3282  3347
      5      3365  3342  3255  3328  3448  3443  3402  3466  3430  3301
      6      3443  3347  3351  3457  3471  3499  3480  3471  3411  3356
      7      3466  3416  3517  3549  3499  3457  3457  3430  3466  3439
      8      3393  3471  3535  3572  3471  3535  3457  3503  3526  3476
      9      3439  3489  3545  3554  3554  3535  3531  3558  3558  3586
     10      3485  3517  3572  3558  3531  3549  3494  3522  3512  3540
list C
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FICOR77   User:wlb       Date_Time:Mon Jul 21 13:32:06 2014
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       694   699   689   682   681   702   706   721   738   708
      2       688   699   690   693   675   685   695   697   702   681
      3       701   681   674   674   692   706   701   681   659   669
      4       670   657   670   659   682   706   703   671   663   677
      5       680   676   658   673   698   696   688   701   694   667
      6       696   677   677   699   702   708   704   702   690   678
      7       701   691   711   718   708   699   699   693   701   696
      8       686   702   715   723   702   715   699   709   713   703
      9       695   706   717   719   719   715   714   719   720   725
     10       705   711   722   720   714   718   707   713   710   715
write "*****Test I/F scale*****"
*****Test I/F scale*****
ficor77 (/project/test_work/testdata/mipl/vgr/f1636832.raw,/proj+
ect/test_work/testdata/mipl/vgr/ficor77.cal,/project/test_work/testdata/mipl/vgr/dc.cal)                  A (500,500,10,10) IOF=.5 +
'NOCO
Beginning VICAR task ficor77
FICOR77 version 24 May 1999
Dark-Current File input=          3
Calibration File input=          2
DARK-CURRENT LABEL...

          77                   800     800 800 800 L 1
          VGR-1   FDS 13852.12   PICNO 0767J1-085   SCET 78.344 22:18:59
          NA CAMERA  EXP 00360.0 MSEC  FILT 1(VIOLET)  LO GAIN  SCAN RATE  1:1
          ERT 78.344 22:53:05   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C
          IN/011340/22 OUT/******/**                 DSS #**   BIT SNR    5.781
           000E5 A/47000BD7 B/68FF C/4C7F D/007F0000 ETLM/CC00A1CA6F6ADEA11F12Q
          NA OPCAL 00(*****.* MSEC) PIXAVG 000/0 OPERATIONAL MODE 2(NAONLY)
          CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE
          NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL
          WA   NO   PREP  NO    YES   NO    NO    NO    NO    0 S  7 NORMAL
          ADJACENT LINE PIXELS CHANGED      5. SAME LINE PIXELS CHANGED     9
          ADESPIKE          12 23 78 13:42:57          CCA31
          ADESPIKE
          ADJACENT LINE PIXELS CHANGED      1. SAME LINE PIXELS CHANGED     1
          RESSAR77          12 23 78 13:46:19          CCA31
          RESSAR77
          INSERT            12 23 78 13:53:46          CCA31
          SAR               12 27 78 08:57:55          CCA31
          1 1 4 800
          SAR
          SPOTREM           12 27 78 08:59:20          CCA31
          MINRAD 7 SPOT 4 380 238 738 434 714 634 36 564
          SPOTREM
          INSERT            12 27 78 09:02:02          CCA31
          INSERT            08 09 80 15:47:45          CCA317
CALIBRATION FILE LABEL...

          77                  2001    640020016400 I 2
          MJS77 PHOTOMETRIC DATA FILE     S/N=7  1 CAMERA FILTER POSITION 0
          DATE GENERATED  12/14/78  FRAMES LOW PASS FLTRD,DIV BY       9.000
          SCAN RATE= 1   GAIN =0     TEMP  0.00   DEG. C  EXPOSURE *60  MSEC
          INTAP=ISS241G  OUTAP=JS0565   FILE= 3
             TEMP   32/1  31/2  27/3  29/0




DIV=  9.0000000
  CAMERA PARAMETERS...
                INPUT   DARK    CALIBRATION
                FRAME   CURRENT FILE
     S/N          7       7       7
     FILTER       0       1       0
     SCANRATE     1       1       1
     GAIN         0       0       0
     MODE       XXXXXXX  NA ONLY
MAXL and luminances of calibration frames--

    120.00      0.00      1.44      3.08      6.16     12.30     24.60     37.00     49.30     74.00     98.00
Exposure time=  360.00000
Radiance refers to solar spectrum
FtL-sec to nanowatt conv factor=  3.3390000
Ft-L to I/F conv factor=  16.510000
Mean DN of DC=  10.930000
Minimum value of a saturated pixel=      32767
Maximum value of an unsaturated pixel=       7845
Total number of saturated pixels=          0
Total number of negative pixels=          0
FICOR task completed
fixvgr A B scf=/project/test_work/testdata/mipl/vgr/vgrscf.dat
Beginning VICAR task fixvgr
FIXVGR Version 31-OCT-94
 Camera=          7
 Filter=          0
SECOND CONV FACTOR =  5.0000E-01
 Correction factor= 0.46550000
FIXVGR task completed
label-l B
Beginning VICAR task label
************************************************************
 
        ************  File B ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: CONV12 -- User: DFS -- Thu May  3 11:31:13 1984 ----
LAB01=
'77                   800     800 800 800 L 1                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/JS4459/02                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'INSERT            05 06 83 13:12:50          JAM                      GL'
NLABS=11
---- Task: FICOR77 -- User: wlb -- Mon Jul 21 13:32:06 2014 ----
LABEL1='FICOR77  MINSAT=32767 NUMSAT=     0'
LABEL2='FOR NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY     0.10112'
LABEL3='FOR (I/F)*10000., MULTIPLY DN VALUE BY               0.50000'
LABEL4='FICOR77  DARK CURRENT FDS = 13852.12'
---- Task: FIXVGR -- User: wlb -- Mon Jul 21 13:32:06 2014 ----
COMMENT=' PICTURE MULTIPLIED BY    0.47      FIXVGR     8/06/90 VERSION'
SCALE='JUPITER'
 
************************************************************
ficor77 (/project/test_work/testdata/mipl/vgr/f1636832.raw,/proj+
ect/test_work/testdata/mipl/vgr/ficor77.cal,/project/test_work/testdata/mipl/vgr/dc.cal)               C (500,500,10,10)scf=/projec+
t/test_work/testdata/mipl/vgr/vgrscf.dat
Beginning VICAR task ficor77
FICOR77 version 24 May 1999
Dark-Current File input=          3
Calibration File input=          2
DARK-CURRENT LABEL...

          77                   800     800 800 800 L 1
          VGR-1   FDS 13852.12   PICNO 0767J1-085   SCET 78.344 22:18:59
          NA CAMERA  EXP 00360.0 MSEC  FILT 1(VIOLET)  LO GAIN  SCAN RATE  1:1
          ERT 78.344 22:53:05   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C
          IN/011340/22 OUT/******/**                 DSS #**   BIT SNR    5.781
           000E5 A/47000BD7 B/68FF C/4C7F D/007F0000 ETLM/CC00A1CA6F6ADEA11F12Q
          NA OPCAL 00(*****.* MSEC) PIXAVG 000/0 OPERATIONAL MODE 2(NAONLY)
          CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE
          NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL
          WA   NO   PREP  NO    YES   NO    NO    NO    NO    0 S  7 NORMAL
          ADJACENT LINE PIXELS CHANGED      5. SAME LINE PIXELS CHANGED     9
          ADESPIKE          12 23 78 13:42:57          CCA31
          ADESPIKE
          ADJACENT LINE PIXELS CHANGED      1. SAME LINE PIXELS CHANGED     1
          RESSAR77          12 23 78 13:46:19          CCA31
          RESSAR77
          INSERT            12 23 78 13:53:46          CCA31
          SAR               12 27 78 08:57:55          CCA31
          1 1 4 800
          SAR
          SPOTREM           12 27 78 08:59:20          CCA31
          MINRAD 7 SPOT 4 380 238 738 434 714 634 36 564
          SPOTREM
          INSERT            12 27 78 09:02:02          CCA31
          INSERT            08 09 80 15:47:45          CCA317
CALIBRATION FILE LABEL...

          77                  2001    640020016400 I 2
          MJS77 PHOTOMETRIC DATA FILE     S/N=7  1 CAMERA FILTER POSITION 0
          DATE GENERATED  12/14/78  FRAMES LOW PASS FLTRD,DIV BY       9.000
          SCAN RATE= 1   GAIN =0     TEMP  0.00   DEG. C  EXPOSURE *60  MSEC
          INTAP=ISS241G  OUTAP=JS0565   FILE= 3
             TEMP   32/1  31/2  27/3  29/0




DIV=  9.0000000
  CAMERA PARAMETERS...
                INPUT   DARK    CALIBRATION
                FRAME   CURRENT FILE
     S/N          7       7       7
     FILTER       0       1       0
     SCANRATE     1       1       1
     GAIN         0       0       0
     MODE       XXXXXXX  NA ONLY
MAXL and luminances of calibration frames--

    120.00      0.00      1.44      3.08      6.16     12.30     24.60     37.00     49.30     74.00     98.00
Exposure time=  360.00000
Radiance refers to solar spectrum
FtL-sec to nanowatt conv factor=  3.3390000
Ft-L to I/F conv factor=  16.510000
Scale correction factor= 0.93099999
Mean DN of DC=  10.930000
Minimum value of a saturated pixel=      32767
Maximum value of an unsaturated pixel=       3652
Total number of saturated pixels=          0
Total number of negative pixels=          0
FICOR task completed
label-l C
Beginning VICAR task label
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: CONV12 -- User: DFS -- Thu May  3 11:31:13 1984 ----
LAB01=
'77                   800     800 800 800 L 1                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/JS4459/02                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'INSERT            05 06 83 13:12:50          JAM                      GL'
NLABS=11
---- Task: FICOR77 -- User: wlb -- Mon Jul 21 13:32:06 2014 ----
LABEL1='FICOR77  MINSAT=32767 NUMSAT=     0'
LABEL2='FOR NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY     0.20224'
LABEL3='FOR (I/F)*10000., MULTIPLY DN VALUE BY               1.00000'
LABEL4='FICOR77  DARK CURRENT FDS = 13852.12'
COMMENT=' PICTURE MULTIPLIED BY    0.93      FICOR      8/06/90 VERSION'
SCALE='JUPITER'
 
************************************************************
list A
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FICOR77   User:wlb       Date_Time:Mon Jul 21 13:32:06 2014
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      7380  7425  7324  7253  7243  7463  7500  7665  7845  7523
      2      7316  7425  7337  7371  7177  7280  7389  7406  7460  7235
      3      7456  7243  7165  7160  7357  7508  7448  7239  7004  7114
      4      7124  6980  7120  7001  7246  7506  7468  7128  7051  7194
      5      7231  7189  6998  7158  7415  7403  7313  7447  7373  7092
      6      7402  7198  7200  7426  7459  7523  7478  7460  7330  7210
      7      7452  7346  7559  7633  7521  7434  7432  7368  7446  7395
      8      7291  7460  7602  7680  7458  7600  7429  7533  7579  7472
      9      7387  7505  7622  7642  7640  7600  7592  7647  7651  7707
     10      7493  7561  7675  7650  7590  7633  7515  7574  7549  7605
list B
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FIXVGR    User:wlb       Date_Time:Mon Jul 21 13:32:06 2014
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      3435  3456  3409  3376  3372  3474  3491  3568  3652  3502
      2      3406  3456  3415  3431  3341  3389  3440  3447  3473  3368
      3      3471  3372  3335  3333  3425  3495  3467  3370  3260  3312
      4      3316  3249  3314  3259  3373  3494  3476  3318  3282  3349
      5      3366  3346  3258  3332  3452  3446  3404  3467  3432  3301
      6      3446  3351  3352  3457  3472  3502  3481  3473  3412  3356
      7      3469  3420  3519  3553  3501  3461  3460  3430  3466  3442
      8      3394  3473  3539  3575  3472  3538  3458  3507  3528  3478
      9      3439  3494  3548  3557  3556  3538  3534  3560  3562  3588
     10      3488  3520  3573  3561  3533  3553  3498  3526  3514  3540
list C
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FICOR77   User:wlb       Date_Time:Mon Jul 21 13:32:06 2014
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      3435  3456  3409  3376  3371  3474  3491  3568  3652  3502
      2      3405  3456  3415  3431  3340  3389  3439  3447  3472  3367
      3      3470  3371  3335  3333  3425  3495  3467  3370  3260  3311
      4      3316  3249  3314  3259  3373  3494  3476  3318  3282  3348
      5      3366  3346  3257  3332  3451  3446  3404  3466  3432  3301
      6      3445  3350  3352  3457  3472  3502  3481  3472  3412  3356
      7      3468  3419  3519  3553  3501  3460  3459  3429  3466  3442
      8      3394  3472  3538  3575  3471  3537  3458  3506  3528  3478
      9      3439  3493  3548  3557  3556  3538  3534  3560  3561  3587
     10      3488  3519  3572  3561  3533  3553  3498  3526  3514  3540
end-proc
$ Return
$!#############################################################################
$Doc_File:
$ create ficor77.doc
$ DECK/DOLLARS="$ VOKAGLEVE"



		FICOR77 PROGRAMMER'S GUIDE

This document contains information useful to the programmer responsible
for maintaining and modifying the VICAR program FICOR77.  Read the Help
and PDF files first.

HISTORY:
-------
FICOR77 is a modification of the Mariner 9 program FICOR.  It was implemented
on the IBM by Joel Mosher in time for the Voyager-Jupiter encounters.  It
was converted to the VAX by M. Kendall and Bob Barrey at University College,
London.  Florence Moss did the VICAR2 conversion and wrote the LCOR pixel loop
in MACRO.  Major changes were made in Feb 1988 by Gary Yagi to clean up the
code and add the FIXVGR scale correction.

OPERATION:
---------
    ficor77 INP=(PIC,C1,C2,...,DC1,DC2,...) OUT=OPIC user-parameters...
where
    PIC	is the image to be radiometrically corrected (byte)
    C1,...  one or more radiometric calibration files (byte or halfword)
    DC1,...	one or more dark-current frames (byte or halfword)
    OPIC is the radiometrically corrected image (byte,half,or real)

Files PIC,CAL,DC and OPIC are referenced by logical unit numbers IUNIT,IUNITC,
IUNITD, and OUNIT, respectively.


DESCRIPTION OF SUBROUTINES:
--------------------------
FICOR77 executes these subroutines (in the order listed):

IPOPEN    --Open PIC and extract camera parameters from label
PLANETID  --Determine planet-of-encounter from user params, PICNO, SCET, or ERT.
OPOPEN    --Open OPIC and process keywords NONEG,MAXDN,BYTE,REAL
CPARAM    --Process camera keywords EXPO,FILTER,SCAN,CAMERA,GAIN,etc.
FSEARCH   --Scan input file list for calibration file and dark-current
	    frame which matches PIC (Call them CAL and DC).
	    Call PLABEL to print CAL and DC image labels and summary of
	    camera parameters.
IPSCALE   --Generate look-up table LUT and scale factor DCSCALE to scale the
	    input DNs to equivalent units as the calibration file data samples.
OPSCALE   --Scale the luminances (LTBL) to output units (I/F or nanowatts).
	      1) Compute adjustment for shutter speed (to/t)
	      2) Call FTLAMBERT to get conversion factors from Ft-L to I/F
		 or nanowatts (S1 or S2).
	      3) Get user-specified scale (A1 or A2).
	      4) Call FIXSCALE to get FIXVGR scale correction (S)
	      5) Apply 1-4 to LTBL
	    Compute difference table DTBL
FCOSET	  --Pass LTBL, DTBL and other constants to LCOR
FICOR     --Loops through each image line, calling LCOR to perform the
	    radiometric correction.
FCOEND    --Retrieve processing statistics from LCOR.
OLABEL    --Add radiometric scale information to output image label.

The subroutine CORRECT is called by FIXSCALE to access the SCF.
The subroutine GETLABX (which gets the VICAR1 label) is called by IPOPEN
and FSEARCH.

Subroutine convention: The input arguments of a subroutine almost always
appear first, and in UPPER CASE, followed by the updated and output
arguments in lower case.

FCOSET, FCOR, and FCOEND are entry points to the Macro subroutine FCOR.
The old Macro equivalent to the FORTRAN code in the subroutine FCOR, was
replaced due to not being portable.

CAMERA PARAMETERS:
-----------------
The camera parameters for the input image and dark-current frame are
maintained in arrays IPIC(39) and IDARK(8), as returned by ABLE77V2, where:

	 IPIC(N)          VARIABLE
	 -------	  --------
	    1             On input, number of parameters to be returned
	    2             FDS count (frame number)
	    3             Exposure time (msec)
	    4             Filter position (0-7)
	    5             Scan rate (=5 if 5:1)
	    6             Camera serial number
	    7             Camera (NA=1,WA=2)
	    8             Gain (low=0,high=1)
	   10		  SCET year
	   21-22	  PICNO
	   35		  ERT year

RADIOMETRIC FILE:
----------------
The radiometric (or calibration) file contains data in labels and data
records containing light-transfer data (in DN) for each pixel of the
image.  The data in the labels is as follows:

	NC = number of data points on curve.
	CNS = number of samples per data record.
	EMAX = default relative ft-L to DN scaling factor
	LTBL(i) = luminance of data point i at reference shutter speed in
               relative ft-L, for i=1,NC

The data points are stored in the data records as vector of the form
(s(1),s(2),...,s(NC)) where s(i) is the pixel output at luminance LTBL(i)
for the reference shutter speed.  The current Voyager calibration file
uses a curve with 10 data points.  The first point is always zero (zero
exposure level).  Each data record contains 3200 data points, or enough
for 320 pixels of the input image.
$ VOKAGLEVE
$ Return
$!#############################################################################
