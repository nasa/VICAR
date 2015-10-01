$!****************************************************************************
$!
$! Build proc for MIPL module fixvgr
$! VPACK Version 1.9, Wednesday, October 06, 1999, 16:55:59
$!
$! Execute by entering:		$ @fixvgr
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
$ write sys$output "*** module fixvgr ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to fixvgr.com file -- ", primary
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
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("fixvgr.imake") .nes. ""
$   then
$      vimake fixvgr
$      purge fixvgr.bld
$   else
$      if F$SEARCH("fixvgr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fixvgr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fixvgr.bld "STD"
$   else
$      @fixvgr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fixvgr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fixvgr.com -
	-s fixvgr.f -
	-i fixvgr.imake -
	-p fixvgr.pdf -
	-t tstfixvgr.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fixvgr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fixvgr.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fixvgr

   To Create the build file give the command:

		$ vimake fixvgr			(VMS)
   or
		% vimake fixvgr			(Unix)


************************************************************************/


#define PROGRAM	fixvgr
#define R2LIB

#define MODULE_LIST fixvgr.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fixvgr.pdf
process help=*
PARM INP      TYPE=STRING 
PARM OUT      TYPE=STRING
PARM SIZE     TYPE=INTEGER COUNT=0:4     	        DEFAULT=--
PARM SL       TYPE=INTEGER COUNT=0:1   	                DEFAULT=--
PARM SS	      TYPE=INTEGER COUNT=0:1	                DEFAULT=--
PARM NL	      TYPE=INTEGER COUNT=0:1	                DEFAULT=--
PARM NS	      TYPE=INTEGER COUNT=0:1	                DEFAULT=--
PARM PLANET   TYPE=KEYWORD COUNT=0:1 +
	      VALID=(JUPITER,SATURN,URANUS,NEPTUNE)	DEFAULT=--
PARM CAMERA   TYPE=INTEGER COUNT=0:1 VALID=(4:7)	DEFAULT=--
PARM FILTER   TYPE=INTEGER COUNT=0:1 VALID=(0:7)	DEFAULT=--
PARM NONEG    TYPE=KEYWORD COUNT=0:1 VALID=NONEG 	DEFAULT=--
PARM NOCORREC TYPE=KEYWORD COUNT=0:1 VALID=NOCORREC	DEFAULT=--
PARM SCF      TYPE=STRING  COUNT=0:1 +
	 DEFAULT="/project/test_work/testdata/mipl/vgr/vgrscf.dat"
END-PROC
.TITLE
"fixvgr" -- Voyager radiometric scale correction program.
.HELP
PURPOSE:

"fixvgr" corrects the DN scale of Voyager images by multiplying each pixel
by a constant.  The resulting image is radiometrically more accurate.

	fixvgr INP=A OUT=B
where
	A = an image that has been radiometrically corrected by "ficor77".
	B = scaled output image.

Both input and output images are in 16-bit integer (halfword) data format.
The input may be any size (e.g. 800x800 geometrically uncorrected,
1000x1000 geometrically corrected, map projected) as long as it is in
the same radiometric scale as the "ficor77" output.

NOTE: As of March 1988, the scale correction was implemented directly into
"ficor77".  "fixvgr" need not be applied to images processed by "ficor77"
after this date (unless they need to be rescaled).

.page
REFERENCES:

1) G.E.Danielson, et.al., "Radiometric Performance of the Voyager Cameras",
   JGR Vol 86, NO.A10, pp 8683-8689, 30 Sep 1981.

2) T.V. Johnson, "Corrections to Danielson et.al. Calibration", memorandum
   to the Voyager Imaging Team, 16 Jan 1986.

OPERATION:

The applied scaling corrects for errors in ground calibration of the camera
system's response through each of the filters, and adjusts for target distance
from the sun using the inverse square law.
		
The scale correction factor is a function of camera, filter, and planet-of-
encounter (target distance from the sun).  The camera serial number and
filter position are extracted automatically from the input picture label.
These values may be overridden via the CAMERA and FILTER parameters.

For Voyager 2, the planet-of-encounter is determined by searching the frame
label for a valid PICNO, Spacecraft-Event-Time (SCET), or Earth-Received-
Time (ERT), in that order.  This may be overridden via the PLANET keyword.

For Voyager 1, the planet-of-encounter is always set to Jupiter, and this
may not be overridden.

The scale factor is retrieved from a table stored in the Scale Correction
File (SCF).  This file may be specified by the SCF parameter.

The following scale correction is applied to each pixel:

	ODN = S*A1*IDN
where
	IDN is the DN value of the input pixel
	ODN is the DN value of the output pixel
	S is the scale factor retrieved from the SCF
	A1 is the number of 'I over F' units per DN, as reported in the
	    picture label by "ficor77".

Images scale-corrected by "fixvgr" may be identified by the following picture
label:

  (e.g) COMMENT=' PICTURE MULTIPLIED BY  0.93   FIXVGR	2/02/86 VERSION'
	SCALE='URANUS'

where 0.93 is the product S*A1, URANUS is the planet-of-encounter, and
2/02/86 is the version date of the SCF.

The resulting output image has a scale of 1 I/F unit per DN, where
10,000 I/F units would be produced by normal incidence of sunlight on a
Lambert disk at the target-body's distance from the sun (5.2 AU for
Jupiter, 9.51 AU for Saturn, 19.122 AU for Uranus, and xx.x AU for Neptune).

After March 1988, the scale correction was implemented directly into
"ficor77".  "fixvgr" need not be run on images processed by "ficor77" 
after this date.  However, if rescaling is desired (because of any future
update to the table), "fixvgr" may be used to perform this.

.page
RESCALING:

The table of scale factors was first installed on 1/16/81 using the values
from Reference 1.  The values for the violet, orange, and UV filters for
the VGR-2 narrow-angle camera (filter positions 1,3, and 7) were updated
2/2/86 using the values from Reference 2.  Provision exits for future
updates to the table.

Images which have been scale corrected by "ficor77" or "fixvgr" using an older
version of the table may be rescaled by running "fixvgr".  "fixvgr" may also be
used to rescale an image to a new target-body.  The rescaling consists of:

	       S
	ODN = ---*IDN
	       So

where So and S are the old and new scale factors retrieved from the SCF.

.page
EXAMPLE:

"fixvgr" is normally executed as follows:

	fixvgr E1 E2  scf=/project/test_work/testdata/mipl/vgr/vgrscf.dat

where E1 is the input image and E2 is the output image.  The following
example illustrates how each of the parameters are entered:

   fixvgr E1 E2 (301,301,200,200) 'SATURN CAMERA=4 FILTER=7 'NONEG

The output will be a 200 x 200 image, representing the sub-area of the
input image beginning at pixel coordinates (301,301).


PROGRAM HISTORY:

Written by:  	Joel A. Mosher		27 October 1980
Converted to VAX by:	Helen De Rueda	25 May 1984
Cognizant programmer:  G.M.Yagi
Revisions:
24 May 99  GMY... Update location of test files in test script
31 Oct 94  AMS... (CRI) Made portable for UNIX
22 May 92  GMY... Update test script (FR 64503)
 1 Feb 88  GMY... Retrieve scale from Scale Correction File
	          Add check for "ficor77 scale correction.
 2 FEB 86  LWK... New values for VGR-2 NA filters 1,3,7 (version 3)
15 JUN 85  LWK... Add planet-specific corrections (version 2)
25 MAY 84  HBD... Convert to VICAR2
17 MAR 82  JAM... Check for previous use of program
26 NOV 81  JAM... Add NOCORRECT option
 9 NOV 81  JAM... System label updates format code
30 OCT 81  JAM... Put in labelc change to modify pixel code to halfword
	          Add a half dn to result before truncating to integer
10 APR 81  JAM... Add new camera correction constants
16 MAR 81  JAM... Change constants from danielson
18 DEC 80  JAM... Initital release (IBM version)

.LEVEL1
.VARIABLE INP
STRING--REQUIRED
The input image.
.VARIABLE OUT
STRING--REQUIRED
The output image.
.VARIABLE SIZE
4 INTEGERS--OPTIONAL
Vicar size field. 
.VARIABLE SL
INTEGER--OPTIONAL
Starting Line.
.VARIABLE SS
INTEGER--OPTIONAL
Starting Sample.
.VARIABLE NL
INTEGER--OPTIONAL
Number of lines.
.VARIABLE NS
INTEGER--OPTIONAL
Number of samples.
.VARI PLANET
KEYWORD--OPTIONAL
Planet-of-encounter
Valid values are:
Jupiter, Saturn,
Uranus, or Neptune.
.VARIABLE CAMERA
INTEGER--OPTIONAL
Camera serial number.
.VARIABLE FILTER
INTEGER--OPTIONAL
Camera filter position
.VARIABLE NONEG
KEYWORD--OPTIONAL
Set negative DNs to zero.
.VARIABLE NOCORREC
KEYWORD--OPTIONAL
Do not correct scale.
.VARIABLE SCF
STRING--OPTIONAL
Scale Correction File.
.LEVEL2
.VARIABLE INP
STRING--REQUIRED
Specifies the input image.  The image must have been radiometrically
corrected by "ficor77" and be in 16-bit integer (halfword) data format.
It may be any size (e.g. 800x800 geometrically uncorrected,
1000x1000 geometrically corrected, map projected) as long as it is in
the same radiometric scale as the "ficor77" output.
.VARIABLE OUT
STRING--REQUIRED
Specifies the output image.
.VARIABLE SIZE
4 INTEGERS--OPTIONAL
	SIZE=(sl,ss,nl,ns)
Specifies the input image area to be processed by starting line, starting
sample, number of lines, and number of samples.  The output image will
be nl x ns samples in area.
If defaulted, the entire image is processed.
.VARIABLE SL
INTEGER--OPTIONAL
The Starting Line in the input picture to be used for the output 
picture.  The default is SL=1.
.VARIABLE SS
INTEGER--OPTIONAL
The Starting Sample in the input picture to be used for the output
picture.  The default is SS=1.
.VARIABLE NL
INTEGER--OPTIONAL
The Number of Lines in the input picture to be processed to the
output picture.  If defaulted, all image lines are processed.
.VARIABLE NS
INTEGER--OPTIONAL
The Number of Samples in the input picture to be processed to the
output picture.  If defaulted, NS is the entire width of the input image.
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

	ERT YEAR	PLANET	   VGR-1 Encounter   VGR-2 Encounter
	--------	------	   ---------------   ---------------
	less than 80	Jupiter	     Mar  5 79		Jul  9 79
	80 to 84	Saturn	     Nov 12 80		Aug 25 81
	85 to 87	Uranus				Jan 24 86
	88 and beyond	Neptune				Aug 24 89
.VARIABLE CAMERA
INTEGER--OPTIONAL
  Ex: CAMERA=4
Specifies the camera ID (serial number) of the input image (PIC).  This
overrides the camera ID extracted from the image's picture label.  The
camera serial numbers of the Voyager spacecraft are:
	4 = VGR-2 WA		6 = VGR-1 WA
	5 = VGR-2 NA		7 = VGR-1 NA
.VARIABLE FILTER
INTEGER--OPTIONAL
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
the input image.
.VARIABLE NONEG
KEYWORD--OPTIONAL
Valid: 'NONEG
Specifies that all negative DNs will be set to zero.  The default is to output
rescaled negative numbers.  NONEG is a relic of the IBM days when "geoma" 
would die by ingesting negative DN values.
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
	Ex:  SCF="/project/test_work/testdata/mipl/vgr/vgrscf.dat"
Specifies the Scale Correction File, as generated by program "ficorgen".
Note that since this file is rarely updated, the default usually points to
the most recent file and this parameter is normally not specified.

Note: The default location for the SCF points to a location at the MIPS
facility.  When running FIXVGR at another facility, please copy the SCF and
point to the new location.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfixvgr.pdf
!TEST OF FIXVGR
!  Checks scaling, re-scaling and planet correction.
!  Note: Tests for compatibility with FICOR77 are included in the FICOR77
!        test script.
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

fixvgr &"path"vioh.img A (30,30,5,5) scf=&"path"vgrscf.dat !First, do it right
fixvgr &"path"vioh.img B (30,30,5,5) scf=&"path"vgrscf.dat 'SATURN  !Start with wrong planet
fixvgr B C scf=&"path"vgrscf.dat 'JUPITER	!Now correct it
label-list C				!Note labels added by fixvgr
list &"path"vioh.img (30,30,5,5)	!Compare the input with output...
list A			 	        !Scale should be 1.0689*VIOH
list B				        !Scale should be 3.345*A (Saturn)
list C				        !Scale should be same as A
end-proc
$ Return
$!#############################################################################
