$!****************************************************************************
$!
$! Build proc for MIPL module galgen
$! VPACK Version 1.9, Monday, December 07, 2009, 16:19:06
$!
$! Execute by entering:		$ @galgen
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
$ write sys$output "*** module galgen ***"
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
$ write sys$output "Invalid argument given to galgen.com file -- ", primary
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
$   if F$SEARCH("galgen.imake") .nes. ""
$   then
$      vimake galgen
$      purge galgen.bld
$   else
$      if F$SEARCH("galgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake galgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @galgen.bld "STD"
$   else
$      @galgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create galgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack galgen.com -mixed -
	-s galgen.f -
	-i galgen.imake -
	-p galgen.pdf -
	-t tgalgen.f tgalgen.imake tgalgen.pdf tstgalgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create galgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C Radiometric calibration program GALGEN
C
C 22 MAY 2003 ...VRH...      updated to be compatable with Cassini Tour
C 30 JUN 1997 ...TXH...      added changes made by CCA to the ported version.
C 19 JUN 1997 ...CCA...      stopped use of saturated levels in fits, mod help
C 18 JUN 1997 ...TXH...      ported from VAX/VMS to Unix and ALPHA/VMS. 
C 25 OCT 1995 ...CCA...      more digits in line number printout
C 16 MAY 1995 ...CCA...      Changed lab.lumin to .radiance, added report of
c                            filename if ABLE97 has error, commented out open
c                            of primary input (conflict with frames in list),
c                            changed max list files from 500 to 10.
C 09 JAN 1995 ...JRY...      Changed LUMINANC to LIGHT
C 15 AUG 1994 ...JRY...      Added Cassini capability, removed SOFFSETS,
C                            changed parameter LC to LUMINANC 
C 01 JUL 1994 ...CRI...      MSTP S/W CONVERSION (VICAR PORTING)
c  25 Jan 91...A.Runkle......Added messages to identify processing progression
c                            Split the dark current files into separate
c			     parameters
c  20 Nov 88...G.M.Yagi......Fixed processing of summation-mode shutter-offset
C  17 Nov 88...G.M.Yagi......Fixed use of LFW pixel table (FLAGLFWP)
C                      ......Fixed summation-mode LFW pixel table so LFW
C		       ......only at 400K
C  30 Sep 88...G.M.Yagi......Update LFW pixel table (Klaasen 16 Sept 88)
C  28 Apr 88...G.M.Yagi......Minor changes to help file.
C  25 Feb 88...G.M.Yagi......Fix built-in tables for low-full-well pixels.
C  22 Nov 87...G.M.Yagi......Delete quadratic model, change format of files,
C	                     built-in tables for low-full-well pixels.
C  15 NOV 85...R.A.MORTENSEN.Added shutter OFFSETS parameter.
C  06 APR 85...D.F.STANFILL..Change to DN=f(BRITE).  Added RMS output file
C  24 OCT 84...G.M.YAGI......CONVERSION TO VAX VICAR*2
C  20 APR 84...M.E.MORRILL...REWRITTEN IN VAX FORTRAN VICAR1*
C   3 SEP 82...J.J.LORRE.....INITIAL RELEASE
C
      SUBROUTINE MAIN44

         COMMON/C1/DNSCALE(14),BRITE(14),ENERGY(1024,14),MAXDEV(1024,14)
         COMMON/C1/FF(1024,13),EDC(1024),SHUTTER_OFFSET_TABLE(1024)
         COMMON/C1/CAL(1024),SAT(1024),FIT(1024),RMS(1024),DC(1024)
         COMMON/C1/LFWP_LINE(20),LFWP_SAMP(20),LFWP_DN(20)
         COMMON/C1/IFIT,SCALE,FSCALE,ISATDN,ISKIP,NLFWP
         COMMON/C1/NL,NS,NI,NO,NFF,NDC,FIRST_EDC,NEDC,IFF

         REAL*4 MAXDEV,ERROR(2)

         INTEGER*2 FF,EDC,SAT,FIT,RMS,DC
         INTEGER FIRST_EDC,SO_INC,NUMB(14)
         INTEGER IUNIT(13),OUNIT(5),INSTANCES(20),CNT
         INTEGER LFWP_CNT,BADFIT

         CHARACTER*8 TASKS(20)
         CHARACTER*30 MISSION
         CHARACTER*80 LFNAME, FN
         CHARACTER*128 F_MSG,L_MSG,T_FILE
         CHARACTER*128 MSG
	 CHARACTER*20 FORMAT

         LOGICAL XVPTST

         EQUIVALENCE (NUMB,DNSCALE)

         CALL IFMESSAGE('GALGEN version MAY 22, 2003')
         CALL XVPCNT('INP',NI)		!NI = number of inputs
         CALL XVPCNT('OUT',NO)		!NO = number of outputs
         IFLAG = 0

C     Open each input and extract picture scale (DNSCALE)...

C     Open DC file if specified
         CALL XVP('DC',T_FILE,IFF)
         IF (IFF .NE. 0) THEN
            CALL XVUNIT(IUNIT(1),'DC',1,IND,'U_NAME',T_FILE,' ')
            CALL XVOPEN(IUNIT(1),IND,'U_FORMAT','HALF',
     &                  'OPEN_ACT','SA','IO_ACT','SA',' ')
            IFF = 1
            NI = NI + 1
            CALL XVMESSAGE('... Dark current file specified',' ')
         ELSE
            IFF = 0
            CALL XVMESSAGE('... No Dark current file specified',' ')
         ENDIF

C     Open all flat-field frames
         CALL XVP('LIST',LFNAME,ICNT)

         IF (ICNT .NE. 0) THEN    !input frames are in SRCH-list

C     Open the files in the SRCH list
       	    OPEN(UNIT=99,FILE=LFNAME,STATUS='OLD',ERR=999)
            READ(99,FMT=1) FN                      !SKIP FIRST LINE
1           FORMAT(A)
 	    DO I=(IFF+1),(IFF+11)
	       READ(99,FMT=1,END=11) FN
	       IF (I .EQ. IFF+11) GO TO 992
               CALL XVUNIT(IUNIT(I),'NONE',I,IST,'U_NAME',FN,' ')
               CALL XVOPEN(IUNIT(I),IST,'OPEN_ACT','SA','IO_ACT',
     &                     'SA',' ')
            ENDDO
11          NI = I - 1
         ELSE                    !input frames are on command line
            DO I=(IFF+1),NI
               CALL XVUNIT(IUNIT(I),'INP',(I-IFF),IND,' ')
               CALL XVOPEN(IUNIT(I),IND,'U_FORMAT','HALF',
     &                     'OPEN_ACT','SA','IO_ACT','SA',' ')
            ENDDO
         ENDIF

         F_MSG = '... 00 flat-field frames opened'
         WRITE (F_MSG(5:6),'(I2)') (NI-IFF)
         CALL XVMESSAGE(F_MSG,' ')

C     Open Extended Dark-current (if specified) -- GALILEO only
         CALL XVP('EDC',T_FILE,CNT)
         IF (CNT .EQ. 1) THEN
            NI = NI + 1
            CALL XVUNIT(IUNIT(NI),'EDC',1,IND,'U_NAME',T_FILE,' ')
            CALL XVOPEN(IUNIT(NI),IND,'U_FORMAT','HALF',
     &                  'OPEN_ACT','SA','IO_ACT','SA',' ')

            CALL XVMESSAGE(
     &         '... Extended Dark-current file specified',' ')
         ENDIF

C     Get PICSCALE from file label (number of frames PICSUM'd for each input)
         DO 5 I=1,NI
            ICNT = 20
            CALL XLHINFO(IUNIT(I),TASKS,INSTANCES,ICNT,IND,' ')
            DO J=ICNT,1,-1           !Search for last value of picture scale
               CALL XLGET(IUNIT(I),'HISTORY','PICSCALE',NUMB(I),IND,
     &                'HIST',TASKS(J),'INSTANCE',INSTANCES(J),
     &                'FORMAT','INT',' ')
               IF (IND.EQ.1) GOTO 5
            ENDDO

C       Didn't find a PICSCALE
            NUMB(I) = 1		!Default picture scale for byte data
            CALL XVGET(IUNIT(I),IND,'FORMAT',FORMAT,' ')
            IF (FORMAT .NE. 'BYTE') IFLAG=1   !Picture scale ambiguous...
    5    CONTINUE

C     Solve for slope term only or for slope and offset terms
         IF (XVPTST('SLOPE')) THEN	
            IFIT = 1			
            IF (NO .GE. 5) GOTO 940
         ELSE
            IFIT = 2			
         ENDIF

C     Open output radiometric slope file
         CALL XVUNIT(OUNIT(1),'OUT',1,IND,' ')
         CALL XVOPEN(OUNIT(1),IND,'OP','WRITE','U_NS',IFIT*NS,
     &               'U_FORMAT','REAL','O_FORMAT','REAL',
     &               'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XLADD(OUNIT(1),'HISTORY','FILE','RADIOMETRIC',
     &              IND,'FORMAT','STRING',' ')

C     Open other output files
         DO I=2,NO
            CALL XVUNIT(OUNIT(I),'OUT',I,IND,' ')
            CALL XVOPEN(OUNIT(I),IND,'OP','WRITE','U_FORMAT','HALF',
     &            'O_FORMAT','HALF','OPEN_ACT','SA','IO_ACT','SA',' ')
         ENDDO

C     Add to label of Dark-current output file
         IF (NO .EQ. 5) THEN
            CALL XLADD(OUNIT(5),'HISTORY','FILE','DARK-CURRENT',
     &             IND,'FORMAT','STRING',' ')
            CALL XLADD(OUNIT(5),'HISTORY','PICSCALE',128,
     &             IND,'FORMAT','INT',' ')
         ENDIF

C     Get Image size from input label and determine the increment
C     needed when reading the full size Shutter-offset file
         CALL XVGET(IUNIT(1),IND,'NL',NL,'NS',NS,' ')  

         SO_INC = 1                  !Default Shutter-offset file increment

C     Galileo summation
         IF (NL .EQ. 400) SO_INC=2

C     Cassini summations
         CALL XLGET(IUNIT(IFF+1),'PROPERTY','MISSION_NAME',MISSION,
     &              ISTAT,'FORMAT','STRING','ERR_ACT',' ',
     &              'PROPERTY','CASSINI-ISS',' ')
	 IF (ISTAT.EQ.1) THEN
             CALL XLGET(IUNIT(IFF+1),'PROPERTY','MISSION_NAME',MISSION,
     &                  ISTAT,'FORMAT','STRING','ERR_ACT',' ',
     &                  'PROPERTY','CASSINI-ISS2',' ')
	 ENDIF
	 IF (ISTAT.EQ.1) THEN
             CALL XLGET(IUNIT(IFF+1),'PROPERTY','MISSION_NAME',MISSION,
     &                  ISTAT,'FORMAT','STRING','ERR_ACT',' ',
     &                  'PROPERTY','IDENTIFICATION',' ')
	 ENDIF

         IF (MISSION(1:7) .EQ. 'CASSINI') THEN
            IF (NL .EQ. 512)  SO_INC=2
            IF (NL .EQ. 256)  SO_INC=4
         ENDIF

         F_MSG = '... 0 output calibration files opened'
         WRITE (F_MSG(5:5),'(I1)') NO
         CALL XVMESSAGE(F_MSG,' ')
         CALL XVMESSAGE('... Files ready for processing',' ')

c     Get user parameters
         CALL GPARAM(IUNIT,MISSION,IFLAG,ERROR,*990)  
         CALL XVMESSAGE('... Parameter processing complete',' ')
C
C
         LFWP_CNT = 0
         BADFIT = 0
         DO LINE=1,NL	!Loop through each image line...

            DO I=1,NFF	!Read data from each flat-field frame...
               CALL XVREAD(IUNIT(I),FF(1,I),IND,' ')
               IF (MISSION(1:7) .EQ. 'CASSINI') THEN
                  DO J=1,NS
                     IF (BRITE(I) .GT. 0.0) THEN
                        ENERGY(J,I) = BRITE(I)
     &	                   - SHUTTER_OFFSET_TABLE(SO_INC*(J-1)+1)
                     ELSE
                        ENERGY(J,I) = 0.0
                     ENDIF
                     MAXDEV(J,I) = ERROR(1)*ENERGY(J,I) + ERROR(2)
                  ENDDO
               ELSE
                  DO J=1,NS
                     IF (BRITE(I) .GT. 0.0) THEN
                        ENERGY(J,I) = BRITE(I)
     &	                   - SHUTTER_OFFSET_TABLE(SO_INC*LINE)
                     ELSE
                        ENERGY(J,I) = 0.0
                     ENDIF
                     MAXDEV(J,I) = ERROR(1)*ENERGY(J,I) + ERROR(2)
                  ENDDO
               ENDIF
            ENDDO

            IF (NEDC .EQ. 1) CALL XVREAD(IUNIT(NI),EDC,IND,' ')

            CALL MVE(-6,NS,ISATDN,SAT,0,1)	!Initialize SAT buffer

            IF (NLFWP .GT. 0)
     &          CALL FLAGLFWP(LINE)	!Flag saturated pixels as -32000
            IF (IFIT .EQ. 1) THEN
	        CALL FITSLOP(MISSION,LFWP_CNT,BADFIT)	!Solve for slope only
            ELSE
               CALL FITLINE(MISSION,LFWP_CNT,BADFIT) !Solve for slope and offset
            ENDIF

C        Write line data to output files
            CALL XVWRIT(OUNIT(1),CAL,IND,' ')
            IF (NO .GE. 2) CALL XVWRIT(OUNIT(2),SAT,IND,' ')
            IF (NO .GE. 3) CALL XVWRIT(OUNIT(3),FIT,IND,' ')
	    IF (NO .GE. 4) CALL XVWRIT(OUNIT(4),RMS,IND,' ')
	    IF (NO .GE. 5) CALL XVWRIT(OUNIT(5),DC,IND,' ')

C        Write progress message
            IF (MOD(LINE,100) .EQ. 0) THEN
               WRITE (L_MSG,9900) LINE
9900           FORMAT ('... Processing through line ',I4,' completed')
               CALL XVMESSAGE(L_MSG,' ')
            ENDIF
         ENDDO

         MSG = '        low-full-well pixels'
         WRITE (MSG(1:7),'(I7)') LFWP_CNT
         CALL XVMESSAGE(MSG,' ')
         MSG = '        pixels with bad fit'
         WRITE (MSG(1:7),'(I7)') BADFIT
         CALL XVMESSAGE(MSG,' ')

C     Close inputs and outputs
         DO I=1,NI
           CALL XVCLOSE(IUNIT(I),IND,' ')
         ENDDO
         DO I=1,NO
           CALL XVCLOSE(OUNIT(I),IND,' ')
         ENDDO

         CALL XVMESSAGE('GALGEN task completed',' ')
         RETURN

  940    CALL XVMESSAGE(
     &  '*** If SLOPE only, do not specify output dark-current',' ')
         CALL ABEND
  990    CALL XVMESSAGE('*** GALGEN task cancelled ***',' ')
         CALL ABEND
  992    CALL XVMESSAGE('MORE THAN 10 FILENAMES IN LIST',' ')
         CALL ABEND
  999    CALL XVMESSAGE('ERROR OPENING INPUT SRCH LIST FILE',' ')
         CALL ABEND
         RETURN
      END



C Parameter processor...
C
      SUBROUTINE GPARAM(IUNIT,MISSION,IFLAG,ERROR,*)

         INCLUDE 'cas_isslab'
c         INCLUDE 'cas_isslab.fin'           !remove before delivery

         COMMON/C1/
     &      DNSCALE(14),BRITE(14),ENERGY(1024,14),MAXDEV(1024,14)
         COMMON/C1/FF(1024,13),EDC(1024),SHUTTER_OFFSET_TABLE(1024)
         COMMON/C1/CAL(1024),SAT(1024),FIT(1024),RMS(1024),DC(1024)
         COMMON/C1/LFWP_LINE(20),LFWP_SAMP(20),LFWP_DN(20)
         COMMON/C1/IFIT,SCALE,FSCALE,ISATDN,ISKIP,NLFWP
         COMMON/C1/NL,NS,NI,NO,NFF,NDC,FIRST_EE,NEDC,IFF

         REAL*4 MAXDEV,ERROR(2),LC,EXPOSURE_TABLE(32)
         REAL*4 LUMIN

         INTEGER*2 FF,EDC,SAT,FIT,RMS,DC,IST
         INTEGER FIRST_EE,IUNIT(13),NUMB(14),EXP(14)
         INTEGER EXPOS(14)

         CHARACTER*4 MODE
         CHARACTER*5 CAMERA, GAIN, FILT1, FILT2
         CHARACTER*8 UNITS
         CHARACTER*30 MISSION
         CHARACTER*256 SHUTTER_OFFSET_FILE
         CHARACTER*50 ERRFN

         LOGICAL XVPTST

C       For Galileo only
         DATA EXPOSURE_TABLE	
     &       /           0,    4.166667,       6.25,    8.333333,
     &                12.5,    16.66667,         25,    33.33333,
     &                  50,    66.66667,        100,    133.3333,
     &                 200,    266.6667,        400,    533.3333,
     &                 800,    1066.667,       1600,    2133.333,
     &                3200,    4266.667,       6400,    8533.333,
     &               12800,    17066.67,      25600,    34133.33,
     &               51200,           0,          0,           0/
         EQUIVALENCE (NUMB,DNSCALE)

C  Param processor and default values
         ISATDN = 32767        !value for flagging good pixels in SAT file


         IF (MISSION(1:7) .EQ. 'CASSINI') THEN
C    Make sure that the inputs all have the same camera, mode, gain,
C    filter 1 and filter2
            CALL ABLE97(ISTAT,IUNIT(IFF+1))
            IF (ISTAT .EQ. -1)  THEN
	       CALL XVGET(IUNIT(IFF+1),IST,'NAME',ERRFN)
	       CALL XVMESSAGE(ERRFN,' ')
	       GOTO 870
	    END IF
            CAMERA = LAB_CAMERA
            GAIN = LAB_GAIN
            MODE = LAB_MODE
            EXPOS(IFF+1) = LAB_EXPOS
            FILT1 = LAB_FILTER1
            FILT2 = LAB_FILTER2
            LUMIN = LAB_RADIANCE
            DO I = (IFF+2),NI
               CALL ABLE97(ISTAT,IUNIT(I))
               IF (ISTAT .EQ. -1)  THEN
	          CALL XVGET(IUNIT(I),IST,'NAME',ERRFN)
	          CALL XVMESSAGE(ERRFN,' ')
	          GOTO 870
	       END IF
               IF ((CAMERA .NE. LAB_CAMERA) .OR.
     &             (GAIN .NE. LAB_GAIN) .OR.
     &             (MODE .NE. LAB_MODE) .OR. 
     &             (FILT1 .NE. LAB_FILTER1) .OR.
     &             (FILT2 .NE. LAB_FILTER2))
     &            GOTO 880
               EXPOS(I) = LAB_EXPOS
            ENDDO
C    If there is a DC, make sure that it has the same camera, mode, and gain
C    as the inputs
            IF (IFF .EQ. 1) THEN
               CALL ABLE97(ISTAT,IUNIT(1))
               IF (ISTAT .EQ. -1)  THEN
	          CALL XVGET(IUNIT(1),IST,'NAME',ERRFN)
	          CALL XVMESSAGE(ERRFN,' ')
	          GOTO 870
	       END IF
               IF ((CAMERA .NE. LAB_CAMERA) .OR. 
     &             (GAIN .NE. LAB_GAIN) .OR.
     &             (MODE .NE. LAB_MODE)) GOTO 890
            ENDIF

c    Get data from the Cassini labels or override with parameters
c    Get Gains from parameter or labels
            CALL XVP('GAIN',IGAIN,N)
            IF (N .EQ. 0) THEN
               IF (GAIN .EQ. '24K') THEN
                  IGAIN = 24
               ELSEIF (GAIN .EQ. '40K' .OR. GAIN(1:2) .EQ. '12') THEN
                  IGAIN = 40
               ELSEIF (GAIN .EQ. '100K' .OR. GAIN(1:2) .EQ. '29') THEN
                  IGAIN = 100
               ELSEIF (GAIN .EQ. '400K' .OR. GAIN(1:2) .EQ. '95') THEN
                  IGAIN = 400
               ELSEIF (GAIN .EQ. '1400K' .OR. GAIN(1:3) .EQ. '215') THEN
                  IGAIN = 1400
               ELSE
                  CALL XVMESSAGE('INVALID GAIN_MODE_ID IN VICAR LABEL',
     &               ' ')
                  CALL ABEND
               ENDIF
            ENDIF

c       Get Exposure times in msec from the parameters or labels
            CALL XVP('EXPOSURE',BRITE(IFF+1),NFF)
            IF (NFF .EQ. 0) THEN
               DO I=(IFF+1),NI
                  BRITE(I) = EXPOS(I)
               ENDDO
               NFF = NI-IFF
            ENDIF

c       Get Light in relative-foot-Lamberts or picoamp-milliseconds
c       from parameters or labels
            CALL XVP('LIGHT',LC,N)
            IF (N .EQ. 0)  LC = LUMIN

c     Get data from GALILEO labels or override
         ELSE	
            CALL XVP('GAIN',IGAIN,N)
            IF (N .EQ. 0) GOTO 900
            CALL XVP('EXPOSURE',BRITE(IFF+1),NFF)	!Exposure times in msec
            IF (NFF .EQ. 0) THEN
               CALL XVP('SHUTTER',EXP(IFF+1),NFF)	!Shutter settings
               IF (NFF .EQ. 0) GOTO 980
               DO I=1,NFF
                  BRITE(I+IFF) = EXPOSURE_TABLE(EXP(I+IFF)+1) !Convert to msec
               ENDDO
            ENDIF

            CALL XVP('LIGHT',LC,N)  !Light cannon in relative-foot-Lamberts or
      				    !picoamp-milliseconds
            IF (N .EQ. 0) GOTO 910
         ENDIF

         CALL XVP('SCALE',SCALE,COUNT)	!Slope term scaling factor
         CALL XVP('FITSCALE',FSCALE,N)	!ERR and RMS terms scaling factor

C        Parameters for low-full-well-pixel test...
         CALL XVP('SKIP',ISKIP,N)   !Number of points to skip
         CALL XVP('ERROR',ERROR,N)  !Slope and offset of error function

C     If LFW pixel test is specified, initialize # of LFW pixels.
C        Else, retrieve LFW pixels from tables and set flag to 
C        skip LFW pixel test
         IF (XVPTST('LFWPT')) THEN		
            NLFWP = 0		
         ELSE			
            CALL LFWP(IGAIN,NS,MISSION,lfwp_line,lfwp_samp,
     &                lfwp_dn,nlfwp)
            ISKIP = 99		
         ENDIF

c     GALILEO extended exposure dat
         CALL XVP('EXTEXPO',FIRST_EE,N)	!First frame for edc correction
         IF (N .EQ. 0) THEN
            FIRST_EE = 32000		!Flag for no edc correction
            NEDC = 0		 	!Number of Extended dark current frames
            CALL XVMESSAGE('No extended-dark-current correction',' ')
         ELSE
            NEDC = 1
            CALL PRNT(4,1,FIRST_EE,'Extended DC correction from frame.')
            FIRST_EE = FIRST_EE + IFF
         ENDIF
C
C           Compute exposures (BRITE) in relative-foot-Lambert-msecs or
C           picoamp-milliseconds
         NDC = IFF				!Dark Current file present
         NFF = NFF + NDC			!DC file added
         IF (NDC .EQ. 1) BRITE(1) = 0.0	!DC file's value automatically set

         IF ((NEDC .EQ. 1) .AND. (NDC .EQ. 0)) GOTO 970
         IF ((NFF+NEDC) .NE. NI) GOTO 990

c     Save Light*Exposure time and check again for ascending values
         DO I=1,NFF
            BRITE(I) = LC*BRITE(I)	!Light * msec
            IF ((I .GT. 1) .AND. (BRITE(I) .LT. BRITE(I-1))) GOTO 930
         ENDDO

         CALL XVPARM('UNITS',UNITS,N,IDEF,1)

c     Print Light*Exposure values
         IF (UNITS .EQ. 'RADIANCE') THEN
            CALL PRNT(7,(NFF-NDC),BRITE(IFF+1), !Don't print DC value if present
     &        'Exposures in picoamp-milliseconds=.')
         ELSE
            CALL PRNT(7,(NFF-NDC),BRITE(IFF+1),!Don't print DC value if present
     &     'Exposures in relative-foot-Lambert-milliseconds=.')
         ENDIF

C     Read the Shutter offset file
         CALL XVPARM('OFFSETS',SHUTTER_OFFSET_FILE,N,IDEF,256)
         CALL XVUNIT(UNIT,'X',1,IND,'U_NAME',SHUTTER_OFFSET_FILE,' ')
         CALL XVOPEN(UNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')

C     Make sure that it Is the correct shutter offset file for the camera

         IF (MISSION(1:7) .EQ. 'CASSINI') THEN
            CALL ABLE97(ISTAT,UNIT)
            IF (ISTAT .EQ. -1)  THEN
               CALL XVGET(UNIT,IST,'NAME',ERRFN)
	       CALL XVMESSAGE(ERRFN,' ')
	       GOTO 870
            END IF
            IF (CAMERA .NE. LAB_CAMERA) GOTO 955
         ENDIF

         CALL XVREAD(UNIT,SHUTTER_OFFSET_TABLE,IND,' ')
         CALL XVGET(UNIT,IND,'NS',NS_SO,' ')  

C     Save the Light*SO times
         DO I=1,NS_SO  !Relative-foot-Lamberts * msec or picoamp-milliseconds
	    SHUTTER_OFFSET_TABLE(I) = LC*SHUTTER_OFFSET_TABLE(I)
         ENDDO

C     Get Input picture scales (how many frames PICSUM'd for each input)
         CALL XVP('NUMB',NUMB(1),N)
         IF (N.EQ.0.AND.IFLAG.EQ.1) GOTO 960
         IF (N.NE.0.AND.N.NE.NI) GOTO 950
         CALL PRNT(4,NI,NUMB(1),'Input picture scales=.')
         DO I=1,NI
            DNSCALE(I) = 1.0/NUMB(I)
         ENDDO

         RETURN

C        Error conditions...
  870    CALL XVMESSAGE('*** Cassini label missing items', ' ')
         RETURN1
  880    CALL XVMESSAGE
     &   ('*** Inputs must have the same camera, mode,',' ')
         CALL XVMESSAGE('     gain,filter 1 and filter2', ' ')
         RETURN1
  890    CALL XVMESSAGE('*** DC must have the same camera, mode,',' ')
         CALL XVMESSAGE('    gain as the inputs', ' ')
         RETURN1
  900    CALL XVMESSAGE('*** GAIN must be given', ' ')
         RETURN1
  910    CALL XVMESSAGE('*** LIGHT must be given', ' ')
         RETURN1
  930    CALL XVMESSAGE
     &('*** Input frames must be in order of increasing exposure',' ')
         RETURN1
  950    CALL XVMESSAGE
     &   ('*** NUMB keyword must match total # of inputs',' ')
         RETURN1
  955    CALL XVMESSAGE
     &   ('*** Wrong shutter offset file (camera does not match)',' ')
         RETURN1
  960    CALL XVMESSAGE('*** Input picture scale is ambiguous',' ')
         CALL XVMESSAGE
     &   ('*** Use NUMB parameter to specify picture scale',' ')
         RETURN1
  970    CALL XVMESSAGE
     &   ('*** Normal DC required if FIRSTEDC is used',' ')
         RETURN1
  980    CALL XVMESSAGE('*** Exposures not specified',' ')
         RETURN1
  990    CALL XVMESSAGE
     &   ('*** Exposure must match number of flat fields',' ')
         RETURN1
         END



C Retrieve low-full-well-pixel data from tables...
C Outputs: LFWP_LINE,LFWP_SAMP,LFWP_DN,NLFWP
C
      SUBROUTINE LFWP(IGAIN,NS,MISSION,LFWP_LINE,LFWP_SAMP,
     &                LFWP_DN,NLFWP)

         INTEGER LFWP_LINE(1),LFWP_SAMP(1),LFWP_DN(1)
         CHARACTER*30 MISSION

C Low-full-well-pixel table reference: "Revised Specifications for SSI
C Low-Full-Well Pixels", K. Klaasen, 31 March 1989.
C Table matrix composed of: Line-Sample-Dn triplet ... in that order

         INTEGER N40,N100,N400
         INTEGER L40(3,1)
         INTEGER L100(3,9)		!NOTE: Data must be ordered by line
         INTEGER L400(3,9)

         DATA N40/1/,N100/9/,N400/9/
         DATA L40/76,466,167/
         DATA L100/
     &          76,466, 82,
     &         390, 96,237,
     &         421,269,168,
     &         659,578,171,
     &         668,520,207,
     &         678,351,227,
     &         696,594,189,
     &         743,673,219,
     &         754,790,214/

         DATA L400/
     &          38,233, 94,
     &         195, 48,250,
     &         211,135,171,
     &         330,289,186,
     &         334,260,223,
     &         339,176,250,
     &         348,297,200,
     &         372,337,234,
     &         377,395,228/


C *** WHEN THE LOW-FULL-WELL-PIXELS ARE DETERMINED FOR CASSINI, THE INFO ****
C *** MUST BE ENTERED IN THIS ROUTINE                                    ****

         IF (MISSION(1:7) .EQ. 'CASSINI') THEN
            NLFWP = 0
         ELSE
C           No low-full-well pixels
            IF ((NS .EQ. 400) .AND. (IGAIN .LT. 400)) THEN 
               NLFWP = 0                           !for summation mode if
            ELSEIF (IGAIN .EQ. 40) THEN
               NLFWP = N40
               DO I=1,NLFWP
                  LFWP_LINE(I) = L40(1,I)
                  LFWP_SAMP(I) = L40(2,I)
                  LFWP_DN(I) = L40(3,I)
               ENDDO
            ELSEIF (IGAIN .EQ. 100) THEN
               NLFWP = N100
               DO I=1,NLFWP
                  LFWP_LINE(I) = L100(1,I)
                  LFWP_SAMP(I) = L100(2,I)
                  LFWP_DN(I) = L100(3,I)
               ENDDO
            ELSEIF (IGAIN .EQ. 400) THEN
               NLFWP = N400
               DO I=1,NLFWP
                  LFWP_LINE(I) = L400(1,I)
                  LFWP_SAMP(I) = L400(2,I)
                  LFWP_DN(I) = L400(3,I)
               ENDDO
            ELSE
               NLFWP = 0
            ENDIF
         ENDIF

         RETURN
      END


C Flag saturated pixels (low-full-well) as -32000
C
      SUBROUTINE FLAGLFWP(LINE)

         COMMON/C1/DNSCALE(14),BRITE(14),ENERGY(1024,14),MAXDEV(1024,14)
         COMMON/C1/FF(1024,13),EDC(1024),SHUTTER_OFFSET_TABLE(1024)
         COMMON/C1/CAL(1024),SAT(1024),FIT(1024),RMS(1024),DC(1024)
         COMMON/C1/LFWP_LINE(20),LFWP_SAMP(20),LFWP_DN(20)
         COMMON/C1/IFIT,SCALE,FSCALE,ISATDN,ISKIP,NLFWP
         COMMON/C1/NL,NS,NI,NO,NFF,NDC,FIRST_EDC,NEDC,IFF

         REAL*4 MAXDEV

         INTEGER*2 FF,EDC,SAT,FIT,RMS,DC
         INTEGER FIRST_EDC,SATDN

         DO 90 J=1,NLFWP       !Flag all saturated low-full-well pixels
            L = LFWP_LINE(J)
            IF (L .GT. LINE) RETURN
            ISAMP = LFWP_SAMP(J)	!Sample position of low-full-well pixel
            SATDN = LFWP_DN(J)	!DN at which pixel saturates
            SAT(ISAMP) = SATDN	!Set saturation DN

            DO I=1,NFF
               DN = FF(ISAMP,I)*DNSCALE(I)
               IF (DN .GT. SATDN) THEN
                  DO K=I,NFF
                     FF(ISAMP,K)=-32000	!Flag saturated pixels
                  ENDDO
                  GOTO 90
               ENDIF
            ENDDO

   90    CONTINUE

         RETURN
      END



c
C Fit the data points to the line:  Y = SLOPE*X + OFFSET
C Outputs: CAL,SAT,FIT,RMS,DC
C
      SUBROUTINE FITLINE(MISSION,LFWP_CNT,BADFIT)

         CHARACTER*30 MISSION

         INTEGER BADFIT, LFWP_CNT

         COMMON/C1/
     &      DNSCALE(14),BRITE(14),ENERGY(1024,14),MAXDEV(1024,14)
         COMMON/C1/FF(1024,13),EDC(1024),SHUTTER_OFFSET_TABLE(1024)
         COMMON/C1/CAL(1024),SAT(1024),FIT(1024),RMS(1024),DC(1024)
         COMMON/C1/LFWP_LINE(20),LFWP_SAMP(20),LFWP_DN(20)
         COMMON/C1/IFIT,SCALE,FSCALE,ISATDN,ISKIP,NLFWP
         COMMON/C1/NL,NS,NI,NO,NFF,NDC,FIRST_EE,NEDC,IFF

         REAL*4 MAXDEV

         INTEGER*2 FF,EDC,SAT,FIT,RMS,DC
         INTEGER FIRST_EE

         REAL*4 RDC(1024)
         REAL*8 DN(14),SUMX(14),SUMX2(14),DENOM(14)
         REAL*8 X,Y,Y0,SUMY,SUMXY,SLOPE,OFFSET,NUMERATOR
         REAL*8 DIF,MAXDIF,RMSQ,MAX_DN

         EDC_CORR = 0.0
         MAX_DN=255.D0
         IF (MISSION(1:7) .EQ. 'CASSINI') MAX_DN=4095.D0

         DO J=1,NS	!Loop through each pixel of image line...
            IF (NEDC .EQ. 1) 
     &         EDC_CORR = FF(J,1) * DNSCALE(1) - EDC(J) * DNSCALE(NFF+1)
            SUMY = 0.0
            SUMXY = 0.0
            SUMX(1) = ENERGY(J,1)
            SUMX2(1) = ENERGY(J,1)**2

            DO N=1,NFF	!Loop through each data point for pixel...
               I = FF(J,N)			!Get DN value
               IF (I .EQ. -32000) THEN	 	!Reject all higher data points
                  NPTS = N - 1		!Number of good points on curve
                  IF (ISKIP .LT. NFF) THEN
                     SAT(J)=DN(N-1)	!If LFWPT, set SAT
                     LFWP_CNT = LFWP_CNT + 1
                  ENDIF
                  GOTO 40
               ENDIF
!if reached saturation, don't use more pints
               Y = I*DNSCALE(N)  !Y is real so may not get exactly 4095 back
               IF (Y .GT. MAX_DN-1.D0) THEN
                  NPTS = N - 1   !Number of good points on curve
                  GOTO 40
               ENDIF
C              Extended dark-current correction
               IF (N .GE. FIRST_EE) Y=Y+EDC_CORR 
               Y = DMAX1(Y,0.D0)
	       X = ENERGY(J,N)
               DN(N) = Y
               IF (N.GT.1) THEN
                  SUMX(N)  = SUMX(N-1) + ENERGY(J,N)
                  SUMX2(N) = SUMX2(N-1) + ENERGY(J,N)**2
                  DENOM(N) = N*SUMX2(N) - SUMX(N)**2
               ENDIF

               IF (N .GT. ISKIP) THEN		!Low-full-well test
                  NPTS = N - 1
                  NUMERATOR = NPTS*SUMXY - SUMX(NPTS)*SUMY
                  IF (NUMERATOR .EQ. 0) GOTO 90
                  SLOPE  = NUMERATOR/DENOM(NPTS)	   !SLOPE
                  OFFSET = (SUMY-SLOPE*SUMX(NPTS))/NPTS  !Dark-current
                  Y0 = SLOPE*X + OFFSET	!Ideal DN based on last slope
                  IF (Y .LT. (Y0-MAXDEV(J,N))) THEN
                      IF (Y0 .LE. MAX_DN) THEN
                         SAT(J)=DN(N-1)
                         LFWP_CNT = LFWP_CNT + 1
                      ENDIF
                      GOTO 40
                  ENDIF
               ENDIF

               SUMY  = SUMY  + Y
               SUMXY = SUMXY + X*Y
            ENDDO

            NPTS = NFF

   40       IF (NPTS .LT. 2) GOTO 90		!Insufficient points
            NUMERATOR = NPTS*SUMXY - SUMX(NPTS)*SUMY
            IF (NUMERATOR .EQ. 0) GOTO 90
            SLOPE  = NUMERATOR/DENOM(NPTS)		! SLOPE
            OFFSET = (SUMY-SLOPE*SUMX(NPTS))/NPTS! Dark-current
            CAL(J) = SCALE/SLOPE		! 1/SLOPE
            RDC(J) = 128.0*OFFSET		! Scaled dark-current

            IF (NO .GT. 2) THEN		!Compute MAXDIF and RMS...
               MAXDIF = 0.0
               RMSQ = 0.0
               DO K=1,NPTS
                  DIF = SLOPE*ENERGY(J,K)+OFFSET - DN(K)
                  MAXDIF = DMAX1(MAXDIF,DABS(DIF))
                  RMSQ = RMSQ + DIF**2
               ENDDO
               FIT(J) = FSCALE*MAXDIF + 0.5
               RMS(J) = FSCALE*DSQRT(RMSQ/NPTS) + 0.5
            ENDIF
            GOTO 100

C            Insufficient points or bad fit...
   90       CAL(J) = 0.0
            RDC(J) = 0
            IF (ISKIP .LT. NFF) THEN
               SAT(J)=-1
               BADFIT = BADFIT + 1
            ENDIF
            FIT(J) = -1
            RMS(J) = -1

  100       IF (RDC(J) .GE. 32767) THEN
               DC(J) = 32767
            ELSEIF (RDC(J) .LE. -32768) THEN
               DC(J) = -32768
            ELSE
               DC(J) = RDC(J)
            ENDIF
         ENDDO
         RETURN
      END



C Fit the data points to the line:  Y = SLOPE*X  where Y=DN-DC.
C Outputs: CAL,SAT,FIT,RMS
C
      SUBROUTINE FITSLOP(MISSION,LFWP_CNT,BADFIT)

         CHARACTER*30 MISSION

         INTEGER BADFIT, LFWP_CNT

         COMMON/C1/DNSCALE(14),BRITE(14),ENERGY(1024,14),MAXDEV(1024,14)
         COMMON/C1/FF(1024,13),EDC(1024),SHUTTER_OFFSET_TABLE(1024)
         COMMON/C1/CAL(1024),SAT(1024),FIT(1024),RMS(1024),DC(1024)
         COMMON/C1/LFWP_LINE(20),LFWP_SAMP(20),LFWP_DN(20)
         COMMON/C1/IFIT,SCALE,FSCALE,ISATDN,ISKIP,NLFWP
         COMMON/C1/NL,NS,NI,NO,NFF,NDC,FIRST_EE,NEDC,IFF

         REAL*4 MAXDEV

         INTEGER*2 FF,EDC,SAT,FIT,RMS,DC
         INTEGER FIRST_EE

         REAL*8 DN(14),SUMX2(14),X,Y,Y0,SUMXY,SLOPE,DIF,MAXDIF,RMSQ
         REAL*8 MAX_DN

         MAX_DN=255.D0
         IF (MISSION(1:7) .EQ. 'CASSINI') MAX_DN=4095.D0

         DO 100 J=1,NS	!Loop through each pixel of image line...
            DC_LEVEL = FF(J,1)*DNSCALE(1)
            IF (NEDC .EQ. 1) EDC_LEVEL=EDC(J)*DNSCALE(NFF+1)
            SUMY = 0.0
            SUMXY = 0.0
            SUMX2(1) = ENERGY(J,1)*2      

            DO N=2,NFF	!Loop through each data point for pixel...
               I = FF(J,N)			!Get DN value
               IF (I .EQ. -32000) THEN	 	!Reject all higher data points
                  NPTS = N - 1		!Number of good points on curve
                  IF (ISKIP .LT. NFF) THEN
                     SAT(J)=DN(N-1)  !If LFWPT, set SAT
                     LFWP_CNT = LFWP_CNT + 1
                  ENDIF
                  GOTO 40
               ENDIF

!if reached saturation, don't use more points
               Y = I*DNSCALE(N)  !Y is real so may not get exactly 4095 back
               IF (Y .GT. MAX_DN-1.D0) THEN
                  NPTS = N-1     !Number of good points on curve
                  GOTO 40
               ENDIF
               IF (N .GE. FIRST_EE) THEN   !If extended-exposure, then
                  Y = Y - EDC_LEVEL   !subtract extended-exposure dark-current
               ELSE
                  Y = Y - DC_LEVEL    !subtract normal dark-current

C                  Y = I*DNSCALE(N)-EDC_LEVEL !subtract extended-exposure 
C                                             !dark-current
C               ELSE				!else
C                  Y = I*DNSCALE(N)-DC_LEVEL	!subtract normal dark-current

               ENDIF

               Y = DMAX1(Y,0.D0)
	       X = ENERGY(J,N)
               DN(N) = Y
               SUMX2(N) = SUMX2(N-1) + ENERGY(J,N)**2

               IF (N .GT. ISKIP) THEN		!Low-full-well test
                  NPTS = N - 1
                  IF (SUMXY .EQ. 0) GOTO 90
                  SLOPE  = SUMXY/SUMX2(NPTS)	! SLOPE
                  Y0 = SLOPE*X		!Ideal DN based on last slope
                  IF (Y .LT. Y0-MAXDEV(J,N)) THEN
                     IF (Y0 .LE. MAX_DN) THEN
                        SAT(J)=DN(N-1)+DC_LEVEL
                        LFWP_CNT = LFWP_CNT + 1
                     ENDIF
                     GOTO 40
                  ENDIF
               ENDIF

               SUMXY = SUMXY + X*Y
            ENDDO

            NPTS = NFF

   40       IF (NPTS .LT. 1) GOTO 90		!Insufficient points
            IF (SUMXY .EQ. 0) GOTO 90
            SLOPE  = SUMXY/SUMX2(NPTS)		! SLOPE
            CAL(J) = SCALE/SLOPE		! 1/SLOPE

            IF (NO .GT. 2) THEN		!Compute MAXDIF and RMS...
               MAXDIF = 0.0
               RMSQ = 0.0
               DO K=2,NPTS
                  DIF = SLOPE*ENERGY(J,K) - DN(K)
                  MAXDIF = DMAX1(MAXDIF,DABS(DIF))
                  RMSQ = RMSQ + DIF**2
               ENDDO
               FIT(J) = FSCALE*MAXDIF + 0.5
               RMS(J) = FSCALE*DSQRT(RMSQ/(NPTS-1)) + 0.5
            ENDIF
            GOTO 100

C          Insufficient points or bad fit...
   90       CAL(J) = 0.0
            IF (ISKIP .LT. NFF) THEN
               SAT(J)=-1
               BADFIT = BADFIT + 1
            ENDIF
            FIT(J) = -1
            RMS(J) = -1
  100       CONTINUE

            RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create galgen.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM galgen

   To Create the build file give the command:

		$ vimake galgen			(VMS)
   or
		% vimake galgen			(Unix)


************************************************************************/


#define PROGRAM	galgen

#define MODULE_LIST galgen.f

#define FTNINC_LIST cas_isslab

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

/* #define LIB_LOCAL  /* remove before delivery */
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create galgen.pdf
PROCESS HELP=*
  PARM INP	TYPE=STRING   COUNT=1:11
  PARM OUT	TYPE=STRING   COUNT=1:5
  PARM DC	TYPE=STRING   COUNT=0:1			DEFAULT=--
  PARM EDC	TYPE=STRING   COUNT=0:1			DEFAULT=--
  PARM OFFSETS	TYPE=STRING   COUNT=1
  PARM GAIN     TYPE=INTEGER  COUNT=0:1  VALID=(10,40,100,400,1400) +
                                                        DEFAULT=--
  PARM UNITS    TYPE=KEYWORD  COUNT=0:1  VALID=(RADIANCE,LUMINANC) +     
                                                        DEFAULT=RADIANCE
  PARM LIGHT	TYPE=REAL     COUNT=0:1                 DEFAULT=--
  PARM EXPOSURE	TYPE=REAL     COUNT=0:13		DEFAULT=--
  PARM SHUTTER	TYPE=INTEGER  COUNT=0:13 VALID=0:31	DEFAULT=--
  PARM NUMB	TYPE=INTEGER  COUNT=0:13 VALID=1:128	DEFAULT=--
  PARM EXTEXPO	TYPE=INTEGER  COUNT=0:1  VALID=2:12	DEFAULT=--
  PARM SCALE	TYPE=REAL     COUNT=0:1			DEFAULT=1.0
  PARM FIT	TYPE=KEYWORD  VALID=(SLOPE,LINEAR)	DEFAULT=LINEAR
  PARM FITSCALE	TYPE=REAL     COUNT=0:1			DEFAULT=1.0
  PARM LFWPT	TYPE=KEYWORD  COUNT=0:1  VALID=LFWPT	DEFAULT=--
  PARM SKIP	TYPE=INTEGER  COUNT=0:1	 VALID=2:13	DEFAULT=4
  PARM ERROR	TYPE=REAL     COUNT=2			DEFAULT=(0.05,1)
  PARM LIST     TYPE=STRING   COUNT=0:1                 DEFAULT=--
END-PROC
.TITLE
VICAR Program GALGEN
.HELP
PURPOSE:
   GALGEN is a VICAR applications program used to generate radiometric
calibration (slope) and dark current files.  These files are used to radiome-
trically correct flight images via the program GALSOS (Galileo) or DECAL 
(Cassini).  The program also generates files containing statistical data 
measuring the linearity at each pixel.  These latter files may be input 
to the program BLEMGEN to generate camera blemish files.

   GALGEN was originally specific to the Galileo SSI camera.  It will also
work for the Cassini ISS camera.  The program assumes a linear light-transfer 
curve.  See also GLLPFCF.

References: 
	D-4264  MIPL Software Structural Design for the Instrument
	        Calibration of GLL SSI Science Processing.
        tbd     Software Design Document for Instrument Calibration -
                Cassini ISS

.page
EXECUTION STATEMENT:

  GALGEN INP=(D1,D2,D3,...,Dn) OUT=(CAL,SAT,ERR,RMS,DC) DC=DC EDC=EDC +
         <user-params>

     or

  GALGEN INP=D1 LIST=LT.LIST OUT=(CAL,SAT,ERR,RMS,DC) DC=DC EDC=EDC +
         <user-params>



  The input frames D1,D2,D3,...,Dn should comprise a light-transfer sequence,
consisting of not more than 10 images of a featureless, uniformly 
illuminated target (flat-field frames) in order of increasing exposure.  
Their filenames may be input to the program in the form of a SRCH-format 
text file (see program SRCH).  If extended-exposure frames are included 
in the sequence (Galileo), an extended-dark-current frame (EDC) must be 
specified.  If multiple frames are taken at any given exposure, they may 
be summed via program PICSUM and the resulting summed frame input to GALGEN.  
The inputs may be in byte or 16-bit integer format.

  The output files (CAL,SAT,ERR,RMS,DC) are images in the same format as the
input flat-field frames (400x400 or 800x800 for Galileo, 256x256, 512x512, or
1024x1024 for Cassini):

  CAL is the output radiometric slope file (REAL*4),
  SAT is an output file identifying the low-full-well pixels (16-bit data),
  ERR is an output file containing the maximum absolute difference (in DN)
       between the input data samples and the fitted curve (16-bit data),
  RMS is an output file containing the rms error for the fit (16-bit data),
  DC is the output dark-current file (linear model only, 16-bit data).

.page
OPERATION:
  GALGEN assumes a linear model of the light-transfer function.  For a
given line-sample coordinate (i,j), the light-transfer function is of the
form:
	d(i,j) = c(i,j)e(i) + dc(i,j)		(1)

where e(i) is the exposure at image line i, d(i,j) is the output camera
signal (DN), and c(i,j) and dc(i,j) are the slope and offset (dark-current)
terms.

  GALGEN assumes that the intensity of the light source is held constant
and that the exposure is varied by varying the shutter speed of the camera.
The exposure e(i) is computed from the light of the light cannon, the commanded
shutter time t, and the shutter offset to(i):

	e(i) = l*[t - to(i)]			(2)

  For Galileo, the light of the light cannon must be specified via the LIGHT
parameter, the camera gain state must be specified via the GAIN parameter, and
the commanded exposure times (in msec) for input frames D1,D2,D3,...,Dn must be
specified via the EXPOSURE parameter.  Alternatively, the shutter settings may
be input via the SHUTTER parameter.  The Shutter-Offset File is specified via
the OFFSET parameter.  

  For Cassini, specifying LIGHT, GAIN, and EXPOSURE is optional (the parameter
SHUTTER is not used for Cassini).  The values are extracted from the VICAR
label if the parameters are not given.

  To determine whether LIGHT is in relative foot-lamberts or picoamps,
sr/nanometer, UNITS is used.  If UNITS=LUMINANC, LIGHT is in relative
foot-lamberts).  If UNITS=RADIANCE, LIGHT is in picoamps.
(UNITS should be LUMINANC for Galileo and RADIANCE for Cassini).

  GALGEN operates in two modes.  In the default mode (FIT=LINEAR), GALGEN
solves for both the slope and offset terms at each pixel position in the
image by fitting a straight line (via least squares) through the input
data points (extracted from input files (D1,D2,D3,...).  The inverse of the
slope (1/c) is output to the radiometric slope file (CAL) and the offset is 
output to DC.  If FIT=SLOPE is specified, then GALGEN solves for the 
slope-term only by fitting the input points to the function:

	d(i,j) = c(i,j)e(i) - do(i,j)		(3)

where do(i,j) is extracted from the input dark-current file (DC).  The inverse
of the slope (1/c) is output to CAL and no DC file is output.

  If the input frames have been pre-processed by PICSUM to sum multiple
frames, GALGEN will normalize the data by dividing by the picture scale
(extracted from the picture label) prior to performing the fit.  The picture
scale may be optionally input via the NUMB parameter.

  For pixels with unsuccessful fits, the CAL file will receive a 0.0, the
SAT, ERR, and RMS files will receive -1.0 and the DC file will receive 0.

While processing each pixel, if a saturated level is detected, that level
and all higher levels are ignored.  This saturation value is 255 for Galileo
and 4095 for Cassini.

.page
Notes:
For Slope model, the DC parameter is not required.  The dark-current file
can be in the input list as the first frame.  This model will subtract
the first input file from all the others before performing fit.  However,
it assumes that this first file is a true dark-current, this Slope model should
yield slope values which go thru the origin.

For Linear model, all input files are fitted without adjustment.  A fifth
output file is produced which is the best fit offsets associated with the
output slope values.

.page
PROGRAM FLOW
  The processing GALGEN performs can be broken into three distinct
modules which are processed sequentially.  First all the input and output
files are opened and prepared for use.  Any input file errors will be
flagged at this point.  Second, the program parameters will be examined
and verified with the input files as necessary.  Finally, all specified
output calibration files will be generated simultaneously line by line.
As processing progresses, informational messages will be given to keep
the user apprised of the current processing.

  If there is a program or system failure, it will not be possible to
recover from a mid-point in the file processing.

.page
EXTENDED-EXPOSURE DARK-CURRENT CORRECTION FOR GALILEO:
  If extended-exposure mode frames are included in the light-transfer
sequence, the EXTEXPO parameter must be used to specify the exposure
at which the extended-exposure data begins, and an extended-exposure
dark-current file (EDC) must be specified.  An adjustment is made to
correct for the difference in dark-current level between extended-exposure
mode frames and frames taken at the normal exposures.  If SLOPE is
specified, this adjustment consists of replacing the normal exposure
dark-current (DC) by the extended-exposure dark-current in equation 3.
If LINEAR is specified (the default), the extended-exposure dark-current
is subtracted from all extended-exposure data and the normal dark-current
is added.

  Sequences which consist entirely of extended-exposure mode data could
be processed like normal exposure data.  I.e., the extended-exposure
dark-current should be specified in the DC parameter; the EXTEXPO parameter
should not be used.

LOW-FULL-WELL-PIXEL TEST:
  The keyword LFWPT enables the low-full-well-pixel test for automatically
scanning for low-full-well pixels by checking for non-linearities in the
input data points (see LFWPT, SKIP, and ERROR parameters).  The SAT file
contains the DN at full-well for each LFW pixel and 32767 for all good
pixels.

.page
RESTRICTIONS:

1.  A maximum of 10 exposure levels (not counting the optiona DC) are
    allowed.


EXAMPLE:

If multiple frames are acquired at each exposure, they may be combined via
program PICSUM and the resulting summed frames input to GALGEN:

   PICSUM (DC1,DC2,DC3,DC4,DC5) DC	     !Sum dark-current frames
   PICSUM (A1,A2,A3,A4,A5) A		     !Sum frames at exposure 133.22
   PICSUM (B1,B2,B3,B4,B5) B		     !Sum frames at exposure 200.0
   PICSUM (C1,C2,C3,C4) C		     !Sum frames at exposure 266.67
   PICSUM (D1,D2,D3) D			     !Sum frames at exposure 400.0
   PICSUM (E1,E2) E			     !Sum frames at exposure 533.33
   PICSUM (EDC1,EDC2,EDC3) EDC		     !Sum extended-dark-current frames
					     !Input summed dark-current,

   GALGEN INP=(A,B,C,D,E) + 		     !Exposures
     OUT=(CAL.GRN,SAT,ERR,RMS,DC.GRN) +	     !Output files
     DC=DC EDC=EDC			     |Dark-current files
     EXPO=(133.22,200,266.67,400,533.33) +   !Commanded exposure times (in msec)
     EXTEXPO=5 LIGHT=3.54 +
     GAIN=100 +
     ERROR=(0,20) SKIP=4 +
     OFFSETS=OFFSETS.DAT

NOTE:  The frames must be input in order of ascending exposure.  The
exposures, specified by the EXPO parameter, must be given in the same order
(It is not necessary to specify the exposure for DC or EDC).  EXTEXPO=5
specifies that the 5th exposure above the dark-current (533.33 msec) is the
beginning of the extended-exposure data.  All data is in the 100K gain-state.

.page
PROGRAM HISTORY:

Written by Jean Lorre, 3 Sep 82
Current cognizant programmer:  Gary Yagi
Revisions:
  30 JUN 1997 ...TXH...     added changes made by CCA to the ported version.
  19 JUN 1997 ...CCA...     stopped use of saturated levels in fits, mod help
  18 JUN 1997 ...TXH...     ported from VAX/VMS to UNIX and ALPHA/VMS.
  25 OCT 1995 ...CCA...     more digits in line number printout
  16 MAY 1995 ...CCA...     Changed lab.lumin to .radiance, added report of
                            filename if ABLE97 has error, commented out open
                            of primary input (conflict with frames in list),
                            changed max list files from 500 to 10.
  09 Jan 95...J.Yoshimizu...Changed LUMINANC to LIGHT.  Added parameter UNITS.
  15 Aug 94...J.Yoshimizu...Changed to work on Cassini ISS data.  Took out the
      			    subroutine SOFFSETS since it was never called.
      			    Changed LC to LUMINANC.
  01 Jul 94...A.Scop..(CRI).Made portable for UNIX
  25 Jan 91...A.Runkle......Added messages to identify processing progression
                            Split the dark current files into separate
				parameters
  20 Nov 88...G.M.Yagi......Fixed processing of summation-mode shutter-offset
  17 Nov 88...G.M.Yagi......Fixed use of LFW pixel table (FLAGLFWP)
                      ......Fixed summation-mode LFW pixel table so LFW
		      ......only at 400K
  30 Sep 88...G.M.Yagi......Update LFW pixel table (Klaasen 16 Sept 88)
  28 Apr 88...G.M.Yagi......Minor changes to help file.
  25 Feb 88...G.M.Yagi......Fix built-in tables for low-full-well pixels.
  22 Nov 87...G.M.Yagi......Delete quadratic model, change format of files,
  	                    built-in tables for low-full-well pixels.
  15 NOV 85...R.A.MORTENSEN.Added shutter OFFSETS parameter.
  06 APR 85...D.F.STANFILL..Change to DN=f(BRITE).  Added RMS output file
  24 OCT 84...G.M.YAGI......CONVERSION TO VAX VICAR*2
  20 APR 84...M.E.MORRILL...REWRITTEN IN VAX FORTRAN VICAR1*
   3 SEP 82...J.J.LORRE.....INITIAL RELEASE
.LEVEL1
.VARIABLE INP
 STRING--REQUIRED
 Flat-field & DC images.
.VARIABLE OUT
 STRING--REQUIRED
 1-5 output images.  
 OUT=(CAL,SAT,ERR,RMS,DC)
.VARIABLE DC
 STRING--OPTIONAL
 Input Dark-current file
.VARIABLE EDC
 STRING--OPTIONAL
 Extended Dark-current
.VARIABLE OFFSETS
 STRING--REQUIRED
 Specifies Shutter-Offset
 File.
.VARIABLE GAIN
 INTEGER
 Camera gain state.
.VARIABLE UNITS
 KEYWORD
Specifies whether LIGHT
is in RADIANCE or
LUMINANC
.VARIABLE LIGHT
 REAL
 Light cannon level in
 foot-Lamberts or
 nanowatts/cm**2/sr/
 nanometer
.VARIABLE EXPOSURE
 REAL
 Specifies the exposure
 time (in msec) for each
 input image (excluding
 the DC & EDC frames).
.VARIABLE SHUTTER
 INTEGER
 Shutter setting for each
 input image (excluding
 the DC & EDC frames).
.VARIABLE NUMB
 INTEGER--OPTIONAL
 Specifies how many
 images were PICSUMed for
 each input flat field.
.VARIABLE EXTEXPO
 INTEGER--OPTIONAL
 Specifies input frame
 at which extended-dark-
 current correction will
 be applied.
.VARIABLE SCALE
 REAL--OPTIONAL
 Specifies scale factor
 for scaling output
 slopes (CAL).
.VARIABLE FIT
 KEYWORD--OPTIONAL 
 Specifies type of
 curve fit (SLOPE
 or LINEAR).
.VARIABLE FITSCALE
 REAL--OPTIONAL
 Specifies scaling applied
 to MAXERR and RMS outputs.
.VARIABLE LFWPT
 KEYWORD--OPTIONAL
 Enables low-full-well
 pixel test.
.VARIABLE SKIP
 INTEGER--OPTIONAL
 Specifies number of
 exposures to skip
 before applying
 low-full-well-pixel
 test.
.VARIABLE ERROR
 REAL--OPTIONAL
 Specifies slope and offset
 of linear error function
 applied in low-full-well
 pixel test.
.VARIABLE LIST
 STRING--OPTIONAL
 SRCH-format file containing
 the names of the files
 to be processed.
.LEVEL2
.VARIABLE INP
 STRING--REQUIRED
	INP=(D1,D2,D3,...,Dn)
 Inputs are flat-field images in order of increasing exposure.  If multiple
 frames are acquired at a given exposure, these frames may be combined
 via PICSUM.
.VARIABLE OUT
 STRING--REQUIRED
	OUT=(CAL,SAT,ERR,RMS,DC)
 The output files (CAL,SAT,ERR,RMS,DC) are images in the same format as the
 input flat-field frames (400x400 or 800x800 for Galileo, 256x256, 512x512, or
 1024x1024 for Cassini):).  If the keyword  LINEAR is specified (the default),
 all five outputs are required.  If SLOPE  is specified, then DC should not be
 specified, and SAT, ERR, and RMS are optional.

 CAL is the Radiometric File, and contains the inverse slope term of the
 light-transfer function for each pixel position (i,j).  The elements of CAL
 are REAL*4 values z(i,j) such that:
	z(i,j) = SCALE/c  for normal pixels (inverse slope term)
	       = 0.0   for an unsuccessful fit.	
 
 SAT contains the saturation DN value (DNfw) for each low-full-well pixel.
 The elements of SAT are 16-bit integers s(i,j) such that:
	s(i,j) = 32767  for normal pixels
	       = DNfw   for low-full-well pixels
	       = -1     for an unsuccessful fit.
 
 ERR contains the maximum absolute deviation (Emax) of the input pixels
 from the fitted light-transfer curve.  The elements of ERR are 16-bit
 integers f(i,j) such that:
	f(i,j) = -1           for an unsuccessful fit
	       = Emax*FSCALE  otherwise.
 where FSCALE is specified by the FITSCALE parameter (default=1.0).
 
 RMS contains the RMS error (Erms) resulting from the fit.  The elements
 of RMS are 16-bit integers r(i,j) such that:
	r(i,j) = -1           for an unsuccessful fit
	       = Erms*FSCALE  otherwise.
 where FSCALE is specified by the FITSCALE parameter (default=1.0).

 If the keyword LINEAR is specified, the output Dark-Current File (DC)
 contains the offset terms (Do) resulting from the fit.  The elements of DC
 are 16-bit integers d(i,j) such that:
	d(i,j) = 0  for an unsuccessful fit
	       = 128*Do  otherwise.
.VARIABLE DC
 STRING--OPTIONAL
 Specifies the input Dark-current file.  This parameter is required when the
 parameter FIT is equal to SLOPE.
.VARIABLE EDC
 STRING--OPTIONAL
 Specifies the input extended Dark-current file.  This paremter is required
 when extened exposure flat fields are used in conjuntion with normal exposure
 flat fields as input.
 NOTE:  The parameters DC & EXTEXP must also be specified.
.VARIABLE GAIN
 INTEGER--REQUIRED for Galileo
 Specifies camera gain state.  Valid values are 10, 40, 100, 400 for Galileo,
 40, 100, 400, 1400 for Cassini.  This parameter must be given for Galileo, but
 if not given for Cassini is extracted from the VICAR label.
.VARIABLE UNITS
 Specifies whether LIGHT is in LUMINANC (Relative-foot-Lamberts ) or RADIANCE
 (nanowatts/cm**2/sr/nanometer).  UNITS should be LUMINANC for Galileo and
 RADIANCE for Cassini).
.VARIABLE LIGHT
 REAL-REQUIRED for Galileo
 LIGHT gives the light cannon setting (in foot-lamberts or nanowatts/cm**2/sr/
 nanometer) at which the flat  field images were exposed (assumed to be
 constant throughout the sequence).  This parameter must be given for Galileo,
 but if not given for Cassini is extracted from the VICAR label.
.VARIABLE EXPOSURE
 REAL--OPTIONAL
 Specifies the commanded exposure times (in milliseconds) for each input
 frame in the light-transfer sequence.  The exposures must be input either
 via the EXPOSURE or SHUTTER parameter (see below).  Not needed for the
 DC or EDC input frames.  EXPOSURE or SHUTTER  must be given for Galileo, but
 if not given for Cassini is extracted from the VICAR label.
.VARIABLE SHUTTER
 INTEGER--OPTIONAL
 This parameter is for Galileo only.   Gives the shutter setting for each flat
 field input image.  The shutter setting is related to commanded exposure time
 as follows:

       Shutter          Exposure     | Shutter           Exposure
       Setting          time (ms)    | Setting           time (ms)
      -------------------------------|---------------------------------
         0                 0         |   16                 800
         1                 4 1/6     |   17                1066 2/3
         2                 6 1/4     |   18                1600
         3                 8 1/3     |   19                2133 1/3
         4                12 1/2     |   20                3200
         5                16 2/3     |   21                4266 2/3
         6                25         |   22                6400
         7                33 1/3     |   23                8533 1/3
         8                50         |   24               12800
         9                66 2/3     |   25               17066 2/3
        10               100         |   26               25600
        11               133 1/3     |   27               34133 1/3
        12               200         |   28               51200
        13               266 2/3     |   29                   0
        14               400         |   30                   0
        15               533 1/3     |   31                   0

.VARIABLE NUMB
 INTEGER--OPTIONAL
 Specifies the picture scale for each input frame.
     E.g. NUMB=([Ndc,]N1,N2,N3,...,Nn[,Nedc])
 If an input frame Di represents multiple frames that have been combined via
 PICSUM, then Ni should equal the number of frames added together (=1 for
 single frames).  When either a dark-current (DC) or an extended-dark-current
 frame (EDC) are specified, then the picture scales: "Ndc" and "Nedc" should
 be included, as needed.  If NUMB is defaulted, the picture scale is retrieved
 from the image labels.
.VARIABLE EXTEXPO
 INTEGER--OPTIONAL - GALILEO only
 EXTEXPO specifies the exposure at which the extended-dark-current
 correction is to be applied.
    galgen (D1,D2,D3,D4,D5,D6,D7) CAL DC=DC EDC=EDC EXTEXPO=5
 The correction will be applied here to frames D5, D6, and D7.
 NOTE:  The parameters DC & EDC must also be specified.
.VARIABLE SCALE
 REAL--OPTIONAL--Default=1.0
 SCALE specifies scaling of the output slope terms.  Instead of outputing
 1/c in the slope file, SCALE/C is output.
.VARIABLE FIT
 KEYWORD--OPTIONAL--Default=LINEAR
 FIT specifies whether, while fitting the input data points to the light-
 transfer curve, to solve for the slope term only (FIT=SLOPE) or to solve
 for both the slope and offset terms (FIT=LINEAR).

 If FIT=LINEAR is specified, the input data points D1,D2,D3,...,Dn are
 fitted to the curve:
		Di = C*Ei + DC
 where the exposures Ei are computed from the commanded exposure times,
 light-cannon setting, and shutter-offsets (see EXPOSURE, SHUTTER, LUMINANC, and
 OFFSET parameters).  The inverses of the resulting slopes (1/C) are output
 to the Radiometric File (CAL).  The resulting offsets are output to the
 output dark-current file (DC).  Even though the input DC frame is not used
 in any calculations, it still must be included as the last input frame.

 If FIT=SLOPE is specified, the input data points D1,D2,D3,...,Dn are fitted
 to the curve:
		Di = C*Ei + DC
 and DC is the input dark-current extracted from the DC parameter.
 The inverses of the resulting slopes (1/C) are output to the Radiometric File
 (CAL).
.VARIABLE FITSCALE
 REAL--OPTIONAL--Default=1.0
 Specifies a number (FSCALE) used to scale the maximum residual error (ERR)
 and rms error (RMS) resulting from the least squares fit.  See OUT parameter
 for details.
.VARIABLE LFWPT
 KEYWORD--OPTIONAL
 The keyword LFWPT enables the low-full-well-pixel test for automatically
 scanning for low-full-well pixels by checking for non-linearities in the
 input data points.  Starting with the first n data points (where n is
 specified via the SKIP parameter), the points are iteratively included
 in the fit.  The next point (not yet included) is checked to see if it
 falls more than a certain distance (determined from the ERROR parameter)
 below the fitted curve (in units of DN).  If a point is rejected, then
 all points above this level are also thrown out, and the raw dn of the last
 pixel used in the fit is placed into the SAT file as a record of the
 saturated DN level (low-full-well for the CCD).

 If the keyword LFWPT is not specified, then the test is suppressed, and
 the positions of the low-full-well pixels are retrieved from tables built
 into the program.
.VARIABLE SKIP
 INTEGER--OPTIONAL--Default=4
 Specifies number of exposures at low end to skip before applying the low
 full well pixel test.  See LFWPT for details.
.VARIABLE ERROR
 REAL--OPTIONAL-Default=(.05,1.)
      ERROR=(delta1,delta2)
 The ERROR parameter specifies the error threshold EPS (in DN) as a linear
 function of the exposure e (in relative-foot-Lambert-msec or picoamp-msec):
      EPS = delta1*e + delta2
 The error threshold is applied in the low-full-well-pixel test (see
 parameter LFWPT).
.VARIABLE OFFSETS
 STRING--REQUIRED
 Specifies the Shutter-Offset File.  The file is generated by program
 CCDRECIP.
.VARIABLE LIST
 STRING--OPTIONAL
 Specifies the name of the SRCH-format text file containing the names of
 the input files to be processed.  This is an alternative to listing the 
 filenames on the command line with the INP parameter.  

 Inputs are flat-field images in order of increasing exposure.  If multiple
 frames are acquired at a given exposure, these frames may be combined
 via PICSUM.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tgalgen.f
      INCLUDE 'VICMAIN_FOR'
C Create GALGEN test frames...
C         GGEN (DC,D1,D2,D3,D4,D5,D6,D7,D8)
C
      SUBROUTINE MAIN44
         INTEGER*2 BUF(9,9)
         DATA BUF/
     &         10,10,10,10,10,10,10,10,10,
     &         15,20,20,20,20,20,20,20,20,
     &         10,25,30,30,30,30,30,30,30,
     &         10,20,35,40,40,40,40,40,40,
     &         10,15,30,45,50,50,50,50,50,
     &         10,10,25,40,55,60,60,60,60,
     &         10,10,20,35,50,65,70,70,70,
     &         10,10,15,30,45,60,75,80,80,
     &         10,10,10,25,40,55,70,85,90/

         CALL XVEACTION('SA',' ')
         DO I=1,9
            DO J=1,9
               BUF(I,J) = 2*BUF(I,J)
            ENDDO
         ENDDO

         DO I=1,9
            CALL XVUNIT(OUNIT,'OUT',I,IND,' ')
            CALL XVSIGNAL(OUNIT,IND,.TRUE.)
            CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',1,'U_NS',9,
     &                  'U_FORMAT','HALF','O_FORMAT','HALF',' ')
            CALL XLADD(OUNIT,'HISTORY','PICSCALE',2,IND,'FORMAT',
     &                 'INT',' ')
            CALL XVWRIT(OUNIT,BUF(1,I),IND,' ')
            CALL PRNT(2,9,BUF(1,I),' ')
            CALL XVCLOSE(OUNIT,IND,' ')
         ENDDO

         RETURN
      END
$!-----------------------------------------------------------------------------
$ create tgalgen.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tgalgen

   To Create the build file give the command:

		$ vimake tgalgen			(VMS)
   or
		% vimake tgalgen			(Unix)


************************************************************************/


#define PROGRAM	tgalgen
#define R2LIB

#define MODULE_LIST tgalgen.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$!-----------------------------------------------------------------------------
$ create tgalgen.pdf
process help=*
PARM OUT TYPE=STRING COUNT=9
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgalgen.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
local dir string

if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
end-if

LET $echo="no"
WRITE " **** Perfom testing with generic offsets. ****"
WRITE ""
LET $echo="yes"

   gen fakeoffsets.dat 1 800 sin=0 'real
LET $echo="no"
WRITE ""
LET $echo="yes"
   tgalgen (d0.dat,d1.dat,d2.dat,d3.dat,d4.dat,d5.dat,d6.dat,d7.dat,d8.dat)
LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen inp=(d1.dat,d2.dat,d3.dat,d4.dat,d5.dat,d6.dat,d7.dat,d8.dat) +
          out=(cal.out,sat.out,err.out,rms.out,dc.out) +
          dc=d0.dat  gain=100 light=10. expo=(1,2,3,4,5,6,7,8) +
          offsets=fakeoffsets.dat skip=2 error=(0,1) 'lfwpt 
LET $echo="no"
WRITE ""
LET $echo="yes"
   list cal.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list dc.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list sat.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list err.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list rms.out

LET $echo="no"
WRITE ""
WRITE ""
WRITE "  *** Testing LIST option ***  "
let $echo="yes"

!create list file
createfile galgen.srchlist
addtofile galgen.srchlist "NEXT FILE=0001"
addtofile galgen.srchlist "d1.dat"
addtofile galgen.srchlist "d2.dat"
addtofile galgen.srchlist "d3.dat"
addtofile galgen.srchlist "d4.dat"
addtofile galgen.srchlist "d5.dat"
addtofile galgen.srchlist "d6.dat"
addtofile galgen.srchlist "d7.dat"
addtofile galgen.srchlist "d8.dat"

LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen inp=d1.dat list=galgen.srchlist +
          out=(cal1.out,sat1.out,err1.out,rms1.out,dc1.out) +
          dc=d0.dat  gain=100 light=10. expo=(1,2,3,4,5,6,7,8) +
          offsets=fakeoffsets.dat skip=2 error=(0,1) 'lfwpt
LET $echo="no"
WRITE ""
LET $echo="yes"
   list cal1.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list dc1.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list sat1.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list err1.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list rms1.out

LET $echo="no"
WRITE ""
WRITE ""
WRITE "-- There shouldn't be any differences on the outputs from previous test."
LET $echo="yes"

difpic (cal.out, cal1.out)
difpic (dc.out, dc1.out)
difpic (sat.out, sat1.out)
difpic (err.out, err1.out)
difpic (rms.out, rms1.out)

LET $echo="no"
WRITE ""
WRITE ""
WRITE " *** Testing Extended-exposure mode *** "
WRITE ""
LET $echo="yes"
   copy d1.dat edc.dat
LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen inp=(d1.dat,d2.dat,d3.dat,d4.dat,d5.dat,d7.dat,d8.dat)  +
          out=(cal.out,sat.out,err.out,rms.out,dc.out) +
          dc=d0.dat edc=edc.dat gain=100 light=10. expo=(1,2,3,4,5,6,7) +
          offsets=fakeoffsets.dat skip=2 error=(0,1) 'lfwp extexpo=5
LET $echo="no"
WRITE ""
LET $echo="yes"
   list cal.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list sat.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list err.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list rms.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list dc.out

LET $echo="no"
WRITE ""
WRITE ""
WRITE " *** Testing Slope mode *** "
WRITE ""
LET $echo="yes"

   galgen inp=(d1.dat,d2.dat,d3.dat,d4.dat,d5.dat,d6.dat,d7.dat,d8.dat) +
          out=(cal.out,sat.out,err.out,rms.out) +
          dc=d0.dat gain=100 light=10. expo=(1,2,3,4,5,6,7,8) +
          offsets=fakeoffsets.dat skip=2 error=(0,1) 'lfwpt 'slope
LET $echo="no"
WRITE ""
LET $echo="yes"
   list cal.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list sat.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list err.out
LET $echo="no"
WRITE ""
LET $echo="yes"
   list rms.out

if ($syschar(1)="UNIX")
   ush rm *.dat
   ush rm *.out
   ush rm galgen.srchlist
else
   dcl del *.dat;*
   dcl del *.out;*
   dcl del galgen.srchlist;*
end-if

LET $echo="no"
WRITE ""
WRITE ""
WRITE " *** Testing with Cassini data *** "
LET $echo="yes"

! Create uniform value files
f2 &"dir"rcp_1.byte d.1 'half func=100
f2 &"dir"rcp_1.byte d.2 'half func=200
f2 &"dir"rcp_1.byte d.3 'half func=700
f2 &"dir"rcp_1.byte d.4 'half func=1100
f2 &"dir"rcp_1.byte d.5 'half func=1600
f2 &"dir"rcp_1.byte d.6 'half func=2600
f2 &"dir"rcp_1.byte d.7 'half func=3600

!Make zero shutter-offset file with cassini label
f2 &"dir"sos.dat fake_so.dat 'REAL func=0.0

LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen (d.2,d.3,d.4,d.5,d.6,d.7) out=(f.cal1,f.sat1,f.err1,f.rms1,f.dc1) +
           dc=d.1 offset=fake_so.dat 'linear expos=(1,6,10,15,25,35) +
           light=100. numb=(1,1,1,1,1,1,1)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.cal1 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.sat1 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.err1 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.rms1 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.dc1  (1,1,5,5)

LET $echo="no"
WRITE ""
WRITE ""
WRITE " -- do slope fit only & change dnscale to 2.  Slope should be doubled."
WRITE ""
LET $echo="yes"
   galgen (d.2,d.3,d.4,d.5,d.6,d.7) out=(f.cal2,f.sat2,f.err2,f.rms2) dc=d.1 +
           offset=fake_so.dat 'slope expos=(1,6,10,15,25,35)  light=100. +
           numb=(2,2,2,2,2,2,2)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.cal2 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.sat2 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.err2 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.rms2 (1,1,5,5)

LET $echo="no"
WRITE ""
WRITE ""
WRITE " -- change expos of 5th input so that lt curve appears to deviate there."
WRITE " -- all pixels should be reported as lfwp with dnfw of 1600. "
WRITE ""
LET $echo="yes"

   galgen (d.2,d.3,d.4,d.5,d.6,d.7) out=(f.cal3,f.sat3,f.err3,f.rms3) dc=d.1 +
           offset=fake_so.dat 'linear expos=(1,6,10,15,32,35) 'lfwp  +
           light=100. numb=(1,1,1,1,1,1,1)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.cal3 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.sat3 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.err3 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.rms3 (1,1,5,5)

LET $echo="no"
WRITE ""
WRITE ""
WRITE " -- change dn of samp 3 of 4th input so that lt curve appears to "
WRITE " -- deviate there.  All pixels at samp 3 should be reported as lfwp "
WRITE " -- with dnfw of 1200. "
WRITE ""
LET $echo="yes"

   f2 d.4 d.4x func="(SAMP.EQ.3)*1200.+(SAMP.NE.3)*IN1"
LET $echo="no"
WRITE ""
LET $echo="yes"
   list d.4x (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen (d.2,d.3,d.4x,d.5,d.6,d.7) out=(f.cal4,f.sat4,f.err4,f.rms4) dc=d.1 +
        offset=fake_so.dat 'linear expos=(1,6,10,15,25,35) 'lfwp  light=100. +
        numb=(1,1,1,1,1,1,1)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.cal4 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.sat4 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.err4 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.rms4 (1,1,5,5)

!throw in some bad pixel data
LET $echo="no"
WRITE ""
LET $echo="yes"
   f2 d.4x d.4y func="IN1+(LINE.EQ.4)*20000.-(SAMP.EQ.2)*25000."
LET $echo="no"
WRITE ""
LET $echo="yes"
   list d.4y (1,1,5,5)

LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen (d.2,d.3,d.4y,d.5,d.6,d.7) out=(f.cal5,f.sat5,f.err5,f.rms5) dc=d.1 +
        offset=fake_so.dat 'linear expos=(1,6,10,15,25,35) 'lfwp  light=100. +
        numb=(1,1,1,1,1,1,1)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.cal5 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.sat5 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.err5 (1,1,5,5)
LET $echo="no"
WRITE ""
LET $echo="yes"
   list f.rms5 (1,1,5,5)

LET $echo="no"
WRITE ""
WRITE ""
WRITE " *** Testing with PICSUM'd frames ***"
WRITE ""
LET $echo="yes"

flot &"dir"sum2.1 a.dat 'coun
flot &"dir"sum2.2 b.dat 'coun
picsum (a.dat,b.dat) g.dc
flot &"dir"sum2.7 a.dat 'coun
flot &"dir"sum2.7 b.dat 'coun
!sum2.8 no longer exists
!flot &"dir"sum2.8 b.dat 'coun
picsum (a.dat,b.dat) g.1
flot &"dir"sum2.14 a.dat 'coun
flot &"dir"sum2.15 b.dat 'coun
picsum (a.dat,b.dat) g.2
flot &"dir"sum2.24 a.dat 'coun
flot &"dir"sum2.25 b.dat 'coun
picsum (a.dat,b.dat) g.3
flot &"dir"sum2.36 a.dat 'coun
flot &"dir"sum2.37 b.dat 'coun
picsum (a.dat,b.dat) g.4

LET $echo="no"
WRITE ""
LET $echo="yes"
   galgen (g.1,g.2,g.3,g.4) out=(g.cal1,g.sat1,g.err1,g.rms1,g.dc1) dc=g.dc +
           offset=&"dir"sos.dat

if ($syschar(1)="UNIX")
   ush rm d.*
   ush rm f.*
   ush rm g.*
   ush rm *.dat
else
   dcl del d.*;*
   dcl del f.*;*
   dcl del g.*;*
   dcl del *.dat;*
end-if

end-proc
$ Return
$!#############################################################################
