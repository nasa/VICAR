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
