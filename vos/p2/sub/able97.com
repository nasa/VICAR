$!****************************************************************************
$!
$! Build proc for MIPL module able97
$! VPACK Version 1.9, Monday, March 29, 2010, 16:42:05
$!
$! Execute by entering:		$ @able97
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
$ write sys$output "*** module able97 ***"
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
$ write sys$output "Invalid argument given to able97.com file -- ", primary
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
$   if F$SEARCH("able97.imake") .nes. ""
$   then
$      vimake able97
$      purge able97.bld
$   else
$      if F$SEARCH("able97.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake able97
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @able97.bld "STD"
$   else
$      @able97.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create able97.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack able97.com -mixed -
	-s able97.f zable97.c -
	-i able97.imake -
	-t table97.f tzable97.c table97.imake table97.pdf tstable97.pdf -
	-o able97.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create able97.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE ABLE97(IND,UNIT)
C##################################################################
C  NAME OF MODULE
C      ABLE97 
C
C  PURPOSE
C      ABLE97 extracts data from the Cassini ISS flight or ground calibration
C      labels.
C  ENVIRONMENT
C      VMS or UNIX  with VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C   01 March 94  CCA   ...ABLE97 created as conversion of ABLE86
C   05 April 95  CCA   ...modified label adding lossy compression parms and
C                         missinglines and radiance
C    1-97  SP   Changed from a VMS Fortran STRUCTURE (previously third argument)
C               to a COMMON block for portability.  
C   !! This COMMON block is NOT TO BE WRITTEN INTO BY MODULES OTHER THAN ABLE97.
C               Changed INTEGER*2 ... TO INTEGER ala VICAR PORTING GUIDE. 
C               Made portable for UNIX.  Added a separate C bridge (zable97)
C               that uses the original structure ala section 9.4.2 of the
C               Porting Guide.
C    2-97  SP   Changed xable97 and cas_isslab.h to be free of I*2 for
C               portability and modifiability.  Added FORMAT optional to
C               all xlget calls.
C   PROGRAMMING NOTES
C               ABLE97 uses a COMMON block as a vehicle for returning data
C               instead of a long argument list or a buffer of various values
C               of various data types.  This method was chosen because it
C               appears to be an adequate replacement for Fortran STRUCTURES
C               and RECORDS (VMS Fortran extensions) and because the fast
C               porting schedule suggest using the first adequate idea that
C               comes to mind.  It allows the set of arguments to be changed
C               or added unto without requiring existing applications to
C               be changed to reflect a new calling sequence.  If the argument
C               set needs to be changed, this can be done by modifying
C               cas_isslab.fin, cas_isslab.h, able97,xable97, and zable97.
C               Note that the order of elements in cas_isslab.fin no longer
C               needs to match the order in cas_isslab.h.
C
C    11-99 TLT  Added IMAGETIME
C               Renamed LAB_PREPDUR to LAB_PREPCYCLE
C               Renamed LAB_READOUTDUR to LAB_READOUTCYCLE
C               Renamed READOUT_CYCLE_DURATION to READOUT_CYCLE_INDEX
C               Renamed PREPARE_CYCLE_DURATION to PREPARE_CYCLE_INDEX
C		Added SHUTTER_EVENT_TYPE
C    01-2000 TLT changed REXPOS to LAB_EXPOS
C    08-2000 AYS Updated to accomodate new vicar property labels
C    10-2000 AYS Added IMAGE_OBSERVATION_TYPE and SEQUENCE_TITLE 
C    05-2001 VRH Increased portability, fixed SENSOR_HEAD_ELEC_TEMPERATURE
C    04-2003 VRH Upated to accomodate Tour format labels
C    01-2004 VRH Fixed MISSION_NAME bug, change ULEN to 15 for all calls
C    29-2010 LWK Ensured line lengths don't exceed 72 characters for new compiler
C------------------------------------------------------------------
      INTEGER UNIT,TMPI4
      CHARACTER*15 MISSION
      CHARACTER*14 PROPERTYNAME
      CHARACTER*5 LAB_FILTER (2)
      REAL LAB_EXPECTMAX (2)
      REAL LAB_CMPRSRATE (2)
      REAL LAB_OPTICSTEMP (2)
      INTEGER LAB_CMPRSPARAM (4)
      INTEGER LAB_VALIDMAXM (2)
      CHARACTER*14 LAB_SPCSTARTCNT
      CHARACTER*14 LAB_SPCSTOPCNT 
c      include 'cas_isslab.fin'   !remove before delivery!
                                !Specifies COMMON block for ABLE97
                                          !CASSINI-ISS label
      include 'cas_isslab'               !Specifies COMMON block for ABLE97
                                          !CASSINI-ISS2 additional label items
C==================================================================
      IND = 0                   !Return indicator initially normal
      CALL ABLE97_INIT          !Set initial values.

C-------MISSION NAME
	CALL XLGET(UNIT,'PROPERTY','MISSION_NAME',MISSION,ISTAT,
     1             'PROPERTY','CASSINI-ISS','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',15,' ')
	IF (ISTAT.EQ.1) THEN
           PROPERTYNAME = 'CASSINI-ISS   '
        ELSE
          CALL XLGET(UNIT,'PROPERTY','MISSION_NAME',MISSION,ISTAT,
     1             'PROPERTY','CASSINI-ISS2','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',15,' ')
	  IF (ISTAT.EQ.1) THEN
             PROPERTYNAME = 'CASSINI-ISS2  '
          ELSE
	     CALL XLGET(UNIT,'PROPERTY','MISSION_NAME',MISSION,ISTAT,
     1             'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',15,' ')
	     IF (ISTAT.EQ.1) THEN
                PROPERTYNAME = 'IDENTIFICATION'
	     ELSE
		CALL XVMESSAGE('MISSION_NAME MISSING',' ')
		   IND=-1
		   RETURN
	     END IF
          END IF
        END IF
	IF (MISSION(1:7) .NE. 'CASSINI') THEN
		CALL XVMESSAGE('NOT CASSINI LABEL',' ')
		IND=-1
		RETURN
	END IF

C-------MISSION PHASE TYPE
	IF (PROPERTYNAME .EQ. 'CASSINI-ISS   ') THEN
	   CALL XLGET(UNIT,'PROPERTY','MISSION_PHASE_TYPE',LAB_PHASE,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('MISSION_PHASE_TYPE MISSING',' ')
		IND=-1
		RETURN
	   END IF
	ELSEIF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
	   CALL XLGET(UNIT,'PROPERTY','MISSION_PHASE_NAME',LAB_PHASE,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('MISSION_PHASE_NAME MISSING',' ')
		IND=-1
		RETURN
	   END IF
        ELSE
	   CALL XLGET(UNIT,'PROPERTY','MISSION_PHASE_NAME',LAB_PHASE,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('MISSION_PHASE_NAME MISSING',' ')
		IND=-1
		RETURN
	   END IF          
	END IF

C-------CAMERA ID
	CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_ID',LAB_CAMERA,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',5,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_ID MISSING',' ')
		IND=-1
		RETURN
	END IF

C-------NOT ISS
	IF (LAB_CAMERA(1:3) .NE. 'ISS') GO TO 200

C-------SCLK
	CALL XLGET(UNIT,'PROPERTY','IMAGE_NUMBER',LAB_SCLK,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('IMAGE_NUMBER MISSING',' ')
		IND=-1
		RETURN
	END IF

C-------SCET AND IMAGETIME
	CALL XLGET(UNIT,'PROPERTY','IMAGE_TIME',LAB_IMAGETIME,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',21,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('IMAGE_TIME MISSING',' ')
		IND=-1
		RETURN
	END IF

	READ (LAB_IMAGETIME(1:4),'(BN,I4)')  LAB_SCETY
	READ (LAB_IMAGETIME(6:8),'(BN,I3)')  LAB_SCETD
	READ (LAB_IMAGETIME(10:11),'(BN,I2)')  LAB_SCETH
	READ (LAB_IMAGETIME(13:14),'(BN,I2)')  LAB_SCETM
	READ (LAB_IMAGETIME(16:17),'(BN,I2)')  LAB_SCETS
	READ (LAB_IMAGETIME(19:21),'(BN,I3)')  LAB_SCETMS
	
C-------FLIGHT OR GROUND LABEL
	IF (LAB_SCETY .LT. 1998) THEN
		LAB_LABTYPE = 'ISSGRND'
	ELSE
	   IF (PROPERTYNAME .EQ. 'CASSINI-ISS   ') THEN
		LAB_LABTYPE = 'ISSFLT '
           END IF
	   IF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
		LAB_LABTYPE = 'ISSFLT2'
	   END IF
	   IF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
		IF (MISSION .EQ. 'CASSINI') THEN
		    LAB_LABTYPE = 'ISSFLT3'
		ELSE IF (MISSION .EQ. 'CASSINI-HUYGENS') THEN
		    LAB_LABTYPE = 'ISSFLT4'
		END IF
	   END IF
	END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1       (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------SOFTWARE VERSION
      CALL XLGET(UNIT,'PROPERTY','SOFTWARE_VERSION_ID',LAB_SWVERS,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',20,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SOFTWARE_VERSION_ID MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------SOFTWARE VERSION
      CALL XLGET(UNIT,'PROPERTY','SOFTWARE_VERSION_ID',LAB_SWVERS,ISTAT,
     1             'PROPERTY','TELEMETRY','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',20,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SOFTWARE_VERSION_ID MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1       (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------INSTRUMENT_MODE_ID
	CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_MODE_ID',LAB_MODE,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',4,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_MODE_ID MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------INSTRUMENT_MODE_ID
	CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_MODE_ID',LAB_MODE,ISTAT,
     1             'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',4,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_MODE_ID MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1       (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-----------FILTER1_NAME
           CALL XLGET(UNIT,'PROPERTY','FILTER1_NAME',LAB_FILTER1,ISTAT,
     1          'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2          'FORMAT','STRING','ULEN',5,' ')
 	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('FILTER1_NAME MISSING',' ')
		IND=-1
		RETURN
  	   END IF
C-----------FILTER2_NAME
	   CALL XLGET(UNIT,'PROPERTY','FILTER2_NAME',LAB_FILTER2,ISTAT,
     1           'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2           'FORMAT','STRING','ULEN', 5,' ')
 	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('FILTER2_NAME MISSING',' ')
		IND=-1
		RETURN
	    END IF
        ELSE
C-----------FILTER1_NAME AND FILTER2_NAME ARE COMBINED INTO FILTER_NAME
C-----------FILTER_NAME
	    CALL XLGET(UNIT,'PROPERTY','FILTER_NAME',LAB_FILTER,ISTAT,
     1           'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2           'FORMAT','STRING','NELEMENT', 2,' ')
            LAB_FILTER1 = LAB_FILTER(1)
            LAB_FILTER2 = LAB_FILTER(2)
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('FILTER_NAME MISSING',' ')
		IND=-1
		RETURN
	    END IF
        END IF

C-------GAIN_MODE_ID
	IF (PROPERTYNAME .EQ. 'CASSINI-ISS   ') THEN
	   CALL XLGET(UNIT,'PROPERTY','GAIN_MODE_ID',LAB_GAIN,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 20,' ')
	ELSEIF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
	   CALL XLGET(UNIT,'PROPERTY','GAIN_MODE_ID',LAB_GAIN,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 20,' ')
        ELSE
	   CALL XLGET(UNIT,'PROPERTY','GAIN_MODE_ID',LAB_GAIN,ISTAT,
     1             'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 20,' ')
    
	END IF
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('GAIN_MODE_ID MISSING',' ')
		IND=-1
		RETURN
	END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------EXPOSURE_DURATION
	CALL XLGET(UNIT,'PROPERTY','EXPOSURE_DURATION',LAB_EXPOS,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('EXPOSURE_DURATION MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------EXPOSURE_DURATION
	CALL XLGET(UNIT,'PROPERTY','EXPOSURE_DURATION',LAB_EXPOS,ISTAT,
     1             'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('EXPOSURE_DURATION MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

C	LAB_EXPOS = IFIX(REXPOS)


	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-----------ENCODING_TYPE
	  CALL XLGET(UNIT,'PROPERTY','ENCODING_TYPE',LAB_COMPRSN,ISTAT,
     1        'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2        'FORMAT','STRING','ULEN', 8,' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ENCODING_TYPE MISSING',' ')
		IND=-1
		RETURN
	    END IF
        ELSE
C-----------INST_CMPRS_TYPE
	    CALL XLGET(UNIT,'PROPERTY','INST_CMPRS_TYPE',LAB_COMPRSN,
     1         ISTAT,'PROPERTY','COMPRESSION','ERR_ACT',' ',
     2         'FORMAT','STRING','ULEN', 8,' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INST_CMPRS_TYPE MISSING',' ')
		IND=-1
		RETURN
	    END IF
         END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-----------CONVERSION_TYPE
	    CALL XLGET(UNIT,'PROPERTY','CONVERSION_TYPE',LAB_CONVRSN,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',6,' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('CONVERSION_TYPE MISSING',' ')
		IND=-1
		RETURN
	    END IF
        ELSE
C-----------DATA_CONVERSION_TYPE
	    CALL XLGET(UNIT,'PROPERTY','DATA_CONVERSION_TYPE',
     1       LAB_CONVRSN,ISTAT,'PROPERTY','IMAGE','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',6,' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DATA_CONVERSION_TYPE MISSING',' ')
		IND=-1
		RETURN
	    END IF
        END IF



	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------DETECTOR_TEMPERATURE
	CALL XLGET(UNIT,'PROPERTY','DETECTOR_TEMPERATURE',LAB_CCDTEMP,
     1             ISTAT, 'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DETECTOR_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------DETECTOR_TEMPERATURE
	CALL XLGET(UNIT,'PROPERTY','DETECTOR_TEMPERATURE',LAB_CCDTEMP,
     1             ISTAT, 'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DETECTOR_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------OPTICS_TEMPERATURE
	CALL XLGET(UNIT,'PROPERTY','OPTICS_TEMPERATURE',LAB_OPTTEMP,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('OPTICS_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------OPTICS_TEMPERATURE
	IF (MISSION .NE. 'CASSINI-HUYGENS') THEN
	CALL XLGET(UNIT,'PROPERTY','OPTICS_TEMPERATURE',LAB_OPTTEMP,
     1             ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('OPTICS_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
	ELSE
C-------OPTICS_TEMPERATURE (2 values)
	CALL XLGET(UNIT,'PROPERTY','OPTICS_TEMPERATURE',LAB_OPTICSTEMP,
     1             ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','REAL','NELEMENT', 2,' ')
        LAB_OPTTEMP = LAB_OPTICSTEMP(1)
        LAB_REAROPTTEMP = LAB_OPTICSTEMP(2)
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('OPTICS_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
	END IF
        END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------FILTER_TEMPERATURE
	CALL XLGET(UNIT,'PROPERTY','FILTER_TEMPERATURE',LAB_FLTTEMP,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('FILTER_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------FILTER_TEMPERATURE
	CALL XLGET(UNIT,'PROPERTY','FILTER_TEMPERATURE',LAB_FLTTEMP,
     1             ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('FILTER_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

C-------SENSOR_HEAD_ELEC_TEMPERATURE
	IF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
	   CALL XLGET(UNIT,'PROPERTY','SENSOR_HEAD_ELEC_TEMP',
     1		LAB_SENSORTEMP,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',
     2          ' ','FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SENSOR_HEAD_ELEC_TEMP MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF
	IF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN 
	   CALL XLGET(UNIT,'PROPERTY','SENSOR_HEAD_ELEC_TEMPERATURE',
     1             LAB_SENSORTEMP,ISTAT,'PROPERTY','INSTRUMENT',
     2             'ERR_ACT',' ','FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
	     CALL XVMESSAGE('SENSOR_HEAD_ELEC_TEMPERATURE MISSING',' ')
		IND=-1
		RETURN
	   END IF
	END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------LIGHT_FLOOD_STATE_FLAG
	CALL XLGET(UNIT,'PROPERTY','LIGHT_FLOOD_STATE_FLAG',LAB_LTFLD,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',3,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('LIGHT_FLOOD_STATE_FLAG MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------LIGHT_FLOOD_STATE_FLAG
	CALL XLGET(UNIT,'PROPERTY','LIGHT_FLOOD_STATE_FLAG',LAB_LTFLD,
     1             ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',3,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('LIGHT_FLOOD_STATE_FLAG MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------ANTIBLOOMING_STATE_FLAG
	CALL XLGET(UNIT,'PROPERTY','ANTIBLOOMING_STATE_FLAG',
     1   LAB_ANTIBLM,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',3,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ANTIBLOOMING_STATE_FLAG MISSING',' ')
		IND=-1
		RETURN
	END IF
        ELSE
C-------ANTIBLOOMING_STATE_FLAG
	CALL XLGET(UNIT,'PROPERTY','ANTIBLOOMING_STATE_FLAG',
     1   LAB_ANTIBLM,ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',3,' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ANTIBLOOMING_STATE_FLAG MISSING',' ')
		IND=-1
		RETURN
	END IF
        END IF

C-------CALIBRATION_LAMP_STATE_FLAG
	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
	CALL XLGET(UNIT,'PROPERTY','CALIB_LAMP_STATE_FLAG',LAB_CALLAMP,
     1             ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 3,' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('CALIB_LAMP_STATE_FLAG MISSING',' ')
		IND=-1
		RETURN
	    END IF
        ELSE
	CALL XLGET(UNIT,'PROPERTY','CALIBRATION_LAMP_STATE_FLAG',
     1   LAB_CALLAMP,ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 3,' ')
	    IF (ISTAT.NE.1) THEN
	      CALL XVMESSAGE('CALIBRATION_LAMP_STATE_FLAG MISSING',' ')
 	      IND=-1
	      RETURN
	    END IF
        END IF

	IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN     
C-------GROUP_BLOCKS
	    CALL XLGET(UNIT,'PROPERTY','GROUP_BLOCKS',TMPI4,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('GROUP_BLOCKS VALUE MISSING',' ')
		TMPI4=-999
	    END IF
	    LAB_BLOCKS=TMPI4               
C-------ALGORITHM
	    CALL XLGET(UNIT,'PROPERTY','ALGORITHM',TMPI4,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ALGORITHM VALUE MISSING',' ')
		TMPI4=-999
	    END IF
	    LAB_ALGORITHM=TMPI4               
C-------BLOCK_TYPE
	    CALL XLGET(UNIT,'PROPERTY','BLOCK_TYPE',TMPI4,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('BLOCK_TYPE VALUE MISSING',' ')
		TMPI4=-999
	    END IF
	    LAB_BTYPE=TMPI4               
C-------QUANTIZATION_FACTOR_INDEX
	    CALL XLGET(UNIT,'PROPERTY','QUANTIZATION_FACTOR_INDEX',
     1             TMPI4,ISTAT,'PROPERTY',PROPERTYNAME,
     2             'ERR_ACT',' ','FORMAT','INT',' ')
	    IF (ISTAT.NE.1) THEN
	      CALL XVMESSAGE('QUANTIZATION_FACTOR_INDEX VALUE MISSING',
     1         ' ')
	      TMPI4=-999
	    END IF
	    LAB_QFACTOR=TMPI4               
        ELSE
C-------INSTRUMENT_CMPRS_PARAM
  	    CALL XLGET(UNIT,'PROPERTY','INST_CMPRS_PARAM',
     1             LAB_CMPRSPARAM,ISTAT,'PROPERTY','COMPRESSION',
     2             'ERR_ACT',' ','FORMAT','INT','NELEMENT',4,' ')
            LAB_ALGORITHM = LAB_CMPRSPARAM(1)
            LAB_BTYPE = LAB_CMPRSPARAM(2)
            LAB_BLOCKS = LAB_CMPRSPARAM(3)
            LAB_QFACTOR = LAB_CMPRSPARAM(4)
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INST_CMPRS_PARAM VALUE MISSING',' ')
		IND=-1
		RETURN
	    END IF
        END IF               

C-------OFFSET / BIAS / BIAS_STRIP_MEAN
	IF (PROPERTYNAME .EQ. 'CASSINI-ISS   ') THEN
	   CALL XLGET(UNIT,'PROPERTY','OFFSET',LAB_BIAS,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('OFFSET MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF             
	IF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
	   CALL XLGET(UNIT,'PROPERTY','BIAS',LAB_BIAS,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('BIAS MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF 
	IF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN 
           CALL XLGET(UNIT,'PROPERTY','BIAS_STRIP_MEAN',LAB_BIAS,ISTAT,
     1             'PROPERTY','IMAGE','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('BIAS_STRIP_MEAN MISSING',' ')
		IND=-1
		RETURN
	   END IF
	END IF

C-------DARK_CURRENT / EXTENDED_PIXEL_VALUE / DARK_STRIP_MEAN
	IF (PROPERTYNAME .EQ. 'CASSINI-ISS   ') THEN
	  CALL XLGET(UNIT,'PROPERTY','DARK_CURRENT',LAB_EXTPIXVAL,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DARK_CURRENT MISSING',' ')
		IND=-1
		RETURN
	   END IF
	ELSEIF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
	   CALL XLGET(UNIT,'PROPERTY','EXTENDED_PIXEL_VALUE',
     1		LAB_EXTPIXVAL,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',
     2          '  ','FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('EXTENDED_PIXEL_VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        ELSE
	   CALL XLGET(UNIT,'PROPERTY','DARK_STRIP_MEAN',LAB_EXTPIXVAL,
     1		   ISTAT,'PROPERTY','IMAGE','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DARK_STRIP_MEAN MISSING',' ')
		IND=-1
		RETURN
	   END IF
	END IF

       IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------COMPRESSION_RATIO
       CALL XLGET(UNIT,'PROPERTY','COMPRESSION_RATIO',LAB_COMPRAT,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('COMPRESSION_RATIO MISSING',' ')
		IND=-1
		RETURN
	    END IF
       ELSE
       CALL XLGET(UNIT,'PROPERTY','INST_CMPRS_RATIO',LAB_COMPRAT,ISTAT,
     1             'PROPERTY','COMPRESSION','ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INST_CMPRS_RATIO MISSING',' ')
		IND=-1
		RETURN
	    END IF
       END IF

       IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C---------TARGET_NAME
	  CALL XLGET(UNIT,'PROPERTY','TARGET_NAME',LAB_TARGET,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('TARGET_NAME MISSING',' ')
		LAB_TARGET='UNKNOWN'
	   END IF
       ELSE
C---------TARGET_NAME
	  CALL XLGET(UNIT,'PROPERTY','TARGET_NAME',LAB_TARGET,ISTAT,
     1             'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('TARGET_NAME MISSING',' ')
		LAB_TARGET='UNKNOWN'
	   END IF
       END IF

       IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C----------OBSERVATION_ID
         CALL XLGET(UNIT,'PROPERTY','OBSERVATION_ID',LAB_OBSERVID,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('OBSERVATION_ID MISSING',' ')
		IND=-1
		RETURN
	   END IF
       ELSE
C----------OBSERVATION_ID
         CALL XLGET(UNIT,'PROPERTY','OBSERVATION_ID',LAB_OBSERVID,ISTAT,
     1             'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',30,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('OBSERVATION_ID MISSING',' ')
		IND=-1
		RETURN
	   END IF
       END IF

       IF (PROPERTYNAME .EQ. 'CASSINI-ISS   ') THEN
C------ILLUMINANT
	   CALL XLGET(UNIT,'PROPERTY','ILLUMINANT',LAB_SOURCE,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN',18,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ILLUMINANT MISSING',' ')
		LAB_SOURCE='UNKNOWN'
	   END IF
C------RADIANCE
	   CALL XLGET(UNIT,'PROPERTY','RADIANCE',LAB_RADIANCE,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('RADIANCE VALUE MISSING',' ')
		LAB_RADIANCE=1.0
	   END IF
	END IF

        IF ((PROPERTYNAME .EQ. 'CASSINI-ISS   ') .OR. 
     1      (PROPERTYNAME .EQ. 'CASSINI-ISS2  ')) THEN
C-------MISSING_LINES
	CALL XLGET(UNIT,'PROPERTY','MISSING_LINES',LAB_MISSING,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('MISSING_LINES VALUE MISSING',' ')
		LAB_MISSING=-999
	END IF
        ELSE
C-------MISSING_LINES
	CALL XLGET(UNIT,'PROPERTY','MISSING_LINES',LAB_MISSING,ISTAT,
     1             'PROPERTY','IMAGE','ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('MISSING_LINES VALUE MISSING',' ')
		LAB_MISSING=-999
	END IF
        END IF

        IF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
C-------COMPRESSION_PARAMETER_VALUE
            CALL XLGET(UNIT,'PROPERTY','COMPRESSION_PARAMETER_VALUE',
     1             LAB_COMPPARAM,ISTAT,'PROPERTY',PROPERTYNAME,
     2		   'ERR_ACT',' ','FORMAT','INT',' ')
            IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE
     1          ('COMPRESSION_PARAMETER_VALUE VALUE MISSING',' ')
	        LAB_COMPPARAM = -999
                RETURN
            END IF
        END IF

	IF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
C-------INSTRUMENT_DATA_RATE
	   CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_DATA_RATE',
     1      LAB_DATARATE,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','REAL',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_DATA_RATE VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF
        IF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
C-------INSTRUMENT_DATA_RATE
        CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_DATA_RATE',LAB_DATARATE,
     1   ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2   'FORMAT','REAL',' ')
          IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_DATA_RATE VALUE MISSING',' ')
		IND=-1
		RETURN
          END IF
        END IF

        IF  (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
C-------SHUTTER_MODE_ID (ENABLED, DISABLED)
	   CALL XLGET(UNIT,'PROPERTY','SHUTTER_MODE_ID',LAB_SHUTID,ISTAT,
     1             'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 10,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SHUTTER_MODE_ID VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        ELSEIF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
C-------SHUTTER_MODE_ID (NACONLY, WACONLY, BOTSIM)
	   CALL XLGET(UNIT,'PROPERTY','SHUTTER_MODE_ID',LAB_SHUTTERMODE,
     1             ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 8,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SHUTTER_MODE_ID VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF

        IF  (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN        
C-------DELAYED_READOUT_FLAG
	   CALL XLGET(UNIT,'PROPERTY','DELAYED_READOUT_FLAG',
     1      LAB_DLYREADOUT,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 3,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DELAYED_READOUT_FLAG VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        ELSEIF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
C-------DELAYED_READOUT_FLAG
	   CALL XLGET(UNIT,'PROPERTY','DELAYED_READOUT_FLAG',
     1      LAB_DLYREADOUT,ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 3,' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DELAYED_READOUT_FLAG VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF

        IF  (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
C-------SHUTTER_EVENT_TYPE (NACONLY, WACONLY, BOTSIM)
	   CALL XLGET(UNIT,'PROPERTY','SHUTTER_EVENT_TYPE',
     1 	    LAB_SHUTTERMODE,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 8, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SHUTTER_EVENT_TYPE VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF
        END IF

        IF  (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
C-------PREPARE_CYCLE_INDEX
	   CALL XLGET(UNIT,'PROPERTY','PREPARE_CYCLE_INDEX',
     1      LAB_PREPCYCLE,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('PREPARE_CYCLE_INDEX VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        ELSEIF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
C-------PREPARE_CYCLE_INDEX
	   CALL XLGET(UNIT,'PROPERTY','PREPARE_CYCLE_INDEX',
     1      LAB_PREPCYCLE,ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2             'FORMAT','INT',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('PREPARE_CYCLE_INDEX VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF

        IF (PROPERTYNAME .EQ. 'CASSINI-ISS2  ') THEN
C-------READOUT_CYCLE_INDEX
	   CALL XLGET(UNIT,'PROPERTY','READOUT_CYCLE_INDEX',
     1      LAB_READOUTCYCLE,ISTAT,'PROPERTY',PROPERTYNAME,'ERR_ACT',
     2      ' ','FORMAT','INT',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('READOUT_CYCLE_INDEX VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        ELSEIF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
C-------READOUT_CYCLE_INDEX
	   CALL XLGET(UNIT,'PROPERTY','READOUT_CYCLE_INDEX',
     1      LAB_READOUTCYCLE,ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',
     2      ' ','FORMAT','INT',' ')
	   IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('READOUT_CYCLE_INDEX VALUE MISSING',' ')
		IND=-1
		RETURN
	   END IF
        END IF


        IF (PROPERTYNAME .EQ. 'IDENTIFICATION') THEN
C-------MISSING_PACKET_FLAG
	   CALL XLGET(UNIT,'PROPERTY','MISSING_PACKET_FLAG',
     1 	    LAB_MISPACFLG,ISTAT,'PROPERTY','TELEMETRY','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 9, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('MISSING_PACKET_FLAG VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------SHUTTER_STATE_ID  (ENABLED, DISALED)
	   CALL XLGET(UNIT,'PROPERTY','SHUTTER_STATE_ID',LAB_SHUTID,
     1		   ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 10, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SHUTTER_STATE_ID VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------SPACECRAFT_CLOCK_CNT_PARTITION
	   CALL XLGET(UNIT,'PROPERTY','SPACECRAFT_CLOCK_CNT_PARTITION',
     1		   LAB_SCCP,ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',
     2		   ' ','FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE(
     1        'SPACECRAFT_CLOCK_CNT_PARTITION VALUE MISSING',' ')
 	     IND=-1
 	     RETURN
           END IF

C-------COMMAND_SEQUENCE_NUMBER
	   CALL XLGET(UNIT,'PROPERTY','COMMAND_SEQUENCE_NUMBER',
     1 	    LAB_COMDSEQNUM,ISTAT,'PROPERTY','COMMAND','ERR_ACT',' ',
     2		   'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('COMMAND_SEQUENCE_NUMBER VALUE MISSING',' ')
 	     IND=-1
 	     RETURN
           END IF

C-------SPACECRAFT_CLOCK_START_COUNT
	   CALL XLGET(UNIT,'PROPERTY','SPACECRAFT_CLOCK_START_COUNT',
     1		   LAB_SPCSTARTCNT,ISTAT,'PROPERTY','IDENTIFICATION',
     2		   'ERR_ACT',' ','FORMAT','STRING','ULEN', 14, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE(
     1		'SPACECRAFT_CLOCK_START_COUNT VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF
           READ (LAB_SPCSTARTCNT(1:10),'(BN,I10)') LAB_SCLKSTART
           READ (LAB_SPCSTARTCNT(12:14),'(BN,I3)') LAB_SCLKSTARTSUB
 
C-------SPACECRAFT_CLOCK_STOP_COUNT
	   CALL XLGET(UNIT,'PROPERTY','SPACECRAFT_CLOCK_STOP_COUNT',
     1		   LAB_SPCSTOPCNT,ISTAT,'PROPERTY','IDENTIFICATION',
     2		   'ERR_ACT',' ','FORMAT','STRING','ULEN', 14, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE(
     1		'SPACECRAFT_CLOCK_STOP_COUNT VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF
           READ (LAB_SPCSTOPCNT(1:10),'(BN,I10)') LAB_SCLKSTOP
           READ (LAB_SPCSTOPCNT(12:14),'(BN,I3)') LAB_SCLKSTOPSUB

C-------EARTH_RECEIVED_START_TIME
	   CALL XLGET(UNIT,'PROPERTY','EARTH_RECEIVED_START_TIME',
     1 	   LAB_ERTSTART,ISTAT,'PROPERTY','TELEMETRY','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 21, ' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('EARTH_RECEIVED_START_TIME VALUE MISSING',
     1        ' ')
             IND=-1
             RETURN
           END IF

	READ (LAB_ERTSTART(1:4),'(BN,I4)')  LAB_SCETY2
	READ (LAB_ERTSTART(6:8),'(BN,I3)')  LAB_SCETD2
	READ (LAB_ERTSTART(10:11),'(BN,I2)')  LAB_SCETH2
	READ (LAB_ERTSTART(13:14),'(BN,I2)')  LAB_SCETM2
	READ (LAB_ERTSTART(16:17),'(BN,I2)')  LAB_SCETS2
	READ (LAB_ERTSTART(19:21),'(BN,I3)')  LAB_SCETMS2

C-------EARTH_RECEIVED_STOP_TIME
	   CALL XLGET(UNIT,'PROPERTY','EARTH_RECEIVED_STOP_TIME',
     1 	    LAB_ERTSTOP,ISTAT,'PROPERTY','TELEMETRY','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 21, ' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('EARTH_RECEIVED_STOP_TIME VALUE MISSING',
     1        ' ')
             IND=-1
             RETURN
           END IF

	READ (LAB_ERTSTOP(1:4),'(BN,I4)')  LAB_SCETY3
	READ (LAB_ERTSTOP(6:8),'(BN,I3)')  LAB_SCETD3
	READ (LAB_ERTSTOP(10:11),'(BN,I2)')  LAB_SCETH3
	READ (LAB_ERTSTOP(13:14),'(BN,I2)')  LAB_SCETM3
	READ (LAB_ERTSTOP(16:17),'(BN,I2)')  LAB_SCETS3
	READ (LAB_ERTSTOP(19:21),'(BN,I3)')  LAB_SCETMS3

C-------FLIGHT_SOFTWARE_VERSION_ID
	CALL XLGET(UNIT,'PROPERTY','FLIGHT_SOFTWARE_VERSION_ID',
     1       LAB_FLTSOFTVERSID,
     2	     ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     3       'FORMAT','STRING','ULEN', 30, ' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('FLIGHT_SOFTWARE_VERSION_ID VALUE MISSING',
     1        ' ')
             IND=-1
             RETURN
           END IF

C-------START_TIME
	   CALL XLGET(UNIT,'PROPERTY','START_TIME',LAB_STARTTIME,
     1		   ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 21, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('START_TIME VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

	READ (LAB_STARTTIME(1:4),'(BN,I4)')  LAB_SCETY4
	READ (LAB_STARTTIME(6:8),'(BN,I3)')  LAB_SCETD4
	READ (LAB_STARTTIME(10:11),'(BN,I2)')  LAB_SCETH4
	READ (LAB_STARTTIME(13:14),'(BN,I2)')  LAB_SCETM4
	READ (LAB_STARTTIME(16:17),'(BN,I2)')  LAB_SCETS4
	READ (LAB_STARTTIME(19:21),'(BN,I3)')  LAB_SCETMS4

C-------CALIBRAION_LAMP_DURATION
	IF (MISSION .NE. 'CASSINI-HUYGENS') THEN
	   CALL XLGET(UNIT,'PROPERTY','CALIBRATION_LAMP_DURATION'
     1             ,LAB_CLD,
     2             ISTAT,'PROPERTY','COMMAND','ERR_ACT',' ',
     3             'FORMAT','REAL',' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('CALIBRATION_LAMP_DURATION VALUE MISSING',
     1        ' ')
 	     IND=-1
 	     RETURN
           END IF
	END IF

C-------ELECTRONICS_BIAS
           CALL XLGET(UNIT,'PROPERTY','ELECTRONICS_BIAS',LAB_ELBIAS,
     1             ISTAT,'PROPERTY','COMMAND','ERR_ACT',' ',
     2             'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ELECTRONICS_BIAS VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------PARALLEL_CLOCK_VOLTAGE_INDEX
           CALL XLGET(UNIT,'PROPERTY','PARALLEL_CLOCK_VOLTAGE_INDEX'
     1             ,LAB_PCVI,
     2             ISTAT,'PROPERTY','COMMAND','ERR_ACT',' ',
     3             'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('PARALLEL_CLOCK_VOLTAGE_INDEX VALUE MISSING'
     1             ,' ')
 	     IND=-1
 	     RETURN
           END IF

C-------DATA_SET_ID
           CALL XLGET(UNIT,'PROPERTY','DATA_SET_ID',LAB_DSID,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 40, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DATA_SET_ID VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------INSTRUMENT_HOST_NAME
           CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_HOST_NAME',LAB_IHN,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 15, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_HOST_NAME VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------INSTRUMENT_NAME
           CALL XLGET(UNIT,'PROPERTY','INSTRUMENT_NAME',LAB_IN,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 38, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INSTRUMENT_NAME VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF


	IF (MISSION .NE. 'CASSINI-HUYGENS') THEN
C-------NOTE
           CALL XLGET(UNIT,'PROPERTY','NOTE',LAB_NOTE,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 100, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('NOTE VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF
	ELSE
C-------DESCRIPTION
           CALL XLGET(UNIT,'PROPERTY','DESCRIPTION',LAB_DESCRIPTION,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 250, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('DESCRIPTION VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF
	END IF


C-------PRODUCT_CREATION_TIME
           CALL XLGET(UNIT,'PROPERTY','PRODUCT_CREATION_TIME',
     1             LAB_PCTIME,
     2             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     3             'FORMAT','STRING','ULEN', 21, ' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('PRODUCT_CREATION_TIME VALUE MISSING',' ')
 	     IND=-1
 	     RETURN
           END IF

C-------PRODUCT_ID
           CALL XLGET(UNIT,'PROPERTY','PRODUCT_ID',LAB_PRID,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 40, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('PRODUCT_ID VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------PRODUCT_VERSION_TYPE
           CALL XLGET(UNIT,'PROPERTY','PRODUCT_VERSION_TYPE',
     1             LAB_PVT,
     2             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     3             'FORMAT','STRING','ULEN', 11, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('PRODUCT_VERSION_TYPE VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------SEQUENCE_ID
           CALL XLGET(UNIT,'PROPERTY','SEQUENCE_ID',LAB_SID,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 30, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SEQUENCE_ID VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------COMMAND_FILE_NAME
           CALL XLGET(UNIT,'PROPERTY','COMMAND_FILE_NAME',LAB_CMDNAME,
     1             ISTAT,'PROPERTY','COMMAND','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 120, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('COMMAND_FILE_NAME VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

	IF (MISSION .NE. 'CASSINI-HUYGENS') THEN
C-------IMAGE_OBSERVATION_TYPE
           CALL XLGET(UNIT,'PROPERTY','IMAGE_OBSERVATION_TYPE',
     1             LAB_IMGOBSTYPE(1),
     2             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     3             'FORMAT','STRING','ULEN', 15, ' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('IMAGE_OBSERVATION_TYPE VALUE MISSING',' ')
 	     IND=-1
 	     RETURN
           END IF
        ELSE
C-------IMAGE_OBSERVATION_TYPE (list, 5max)
           CALL XLGET(UNIT,'PROPERTY','IMAGE_OBSERVATION_TYPE',
     1             LAB_IMGOBSTYPE,
     2             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     3             'FORMAT','STRING','ULEN', 15,'NELEMENT', 5, ' ')
           IF (ISTAT.NE.1) THEN
 	     CALL XVMESSAGE('IMAGE_OBSERVATION_TYPE VALUE MISSING',' ')
 	     IND=-1
 	     RETURN
           END IF
	END IF


C-------SEQUENCE_TITLE
           CALL XLGET(UNIT,'PROPERTY','SEQUENCE_TITLE',LAB_SEQTITLE,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','STRING','ULEN', 60, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SEQUENCE_TITLE VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

        END IF

C Tour items

	IF (MISSION .EQ. 'CASSINI-HUYGENS') THEN

C-------EXPECTED_PACKETS
           CALL XLGET(UNIT,'PROPERTY','EXPECTED_PACKETS',
     1             LAB_EXPPACKETS,
     2             ISTAT,'PROPERTY','TELEMETRY','ERR_ACT',' ',
     3             'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('EXPECTED_PACKETS VALUE MISSING'
     1             ,' ')
		IND=-1
		RETURN
           END IF

C-------ORDER_NUMBER
           CALL XLGET(UNIT,'PROPERTY','ORDER_NUMBER',LAB_ORDERNUM,
     1             ISTAT,'PROPERTY','COMMAND','ERR_ACT',' ',
     2             'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('ORDER_NUMBER VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------RECEIVED_PACKETS
           CALL XLGET(UNIT,'PROPERTY','RECEIVED_PACKETS',
     1             LAB_RECPACKETS,
     2             ISTAT,'PROPERTY','TELEMETRY','ERR_ACT',' ',
     3             'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('RECEIVED_PACKETS VALUE MISSING'
     1             ,' ')
		IND=-1
		RETURN
           END IF

C-------SEQUENCE_NUMBER
           CALL XLGET(UNIT,'PROPERTY','SEQUENCE_NUMBER',LAB_SEQNUM,
     1             ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2             'FORMAT','INT',' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('SEQUENCE_NUMBER VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------VALID_MAXIMUM
	    CALL XLGET(UNIT,'PROPERTY','VALID_MAXIMUM',LAB_VALIDMAXM,
     1           ISTAT,'PROPERTY','COMPRESSION','ERR_ACT',' ',
     2           'FORMAT','INT','NELEMENT', 2,' ')
            LAB_VALIDMAX = LAB_VALIDMAXM(1)
            LAB_VALIDMAXFW = LAB_VALIDMAXM(2)
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('VALID_MAXIMUM VALUE MISSING',' ')
		IND=-1
		RETURN
	    END IF

C-------EXPECTED_MAXIMUM
	    CALL XLGET(UNIT,'PROPERTY','EXPECTED_MAXIMUM',
     1           LAB_EXPECTMAX,ISTAT,'PROPERTY','COMPRESSION',
     2           'ERR_ACT',' ','FORMAT','REAL','NELEMENT', 2,' ')
            LAB_EXPMAX = LAB_EXPECTMAX(1)
            LAB_EXPMAXFW = LAB_EXPECTMAX(2)
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('EXPECTED_MAXIMUM VALUE MISSING',' ')
		IND=-1
		RETURN
	    END IF

C-------INST_CMPRS_RATE
	    CALL XLGET(UNIT,'PROPERTY','INST_CMPRS_RATE',LAB_CMPRSRATE,
     1           ISTAT,'PROPERTY','COMPRESSION','ERR_ACT',' ',
     2           'FORMAT','REAL','NELEMENT', 2,' ')
            LAB_COMPRATEPRED = LAB_CMPRSRATE(1)
            LAB_COMPRATEACT = LAB_CMPRSRATE(2)
	    IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('INST_CMPRS_RATE VALUE MISSING',' ')
		IND=-1
		RETURN
	    END IF

C-------IMAGE_MID_TIME
	   CALL XLGET(UNIT,'PROPERTY','IMAGE_MID_TIME',LAB_MIDTIME,
     1		   ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 21, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('IMAGE_MID_TIME VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

	   READ (LAB_MIDTIME(1:4),'(BN,I4)')  LAB_SCETY5
	   READ (LAB_MIDTIME(6:8),'(BN,I3)')  LAB_SCETD5
	   READ (LAB_MIDTIME(10:11),'(BN,I2)')  LAB_SCETH5
	   READ (LAB_MIDTIME(13:14),'(BN,I2)')  LAB_SCETM5
	   READ (LAB_MIDTIME(16:17),'(BN,I2)')  LAB_SCETS5
	   READ (LAB_MIDTIME(19:21),'(BN,I3)')  LAB_SCETMS5

C-------METHOD_DESC
	   CALL XLGET(UNIT,'PROPERTY','METHOD_DESC',LAB_METHODDESC,
     1		   ISTAT,'PROPERTY','INSTRUMENT','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 100, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('METHOD_DESC VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------TARGET_DESC
	   CALL XLGET(UNIT,'PROPERTY','TARGET_DESC',LAB_TARGETDESC,
     1		   ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 75, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('TARGET_DESC VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------TARGET_LIST
	   CALL XLGET(UNIT,'PROPERTY','TARGET_LIST',LAB_TARGETLIST,
     1		   ISTAT,'PROPERTY','IDENTIFICATION','ERR_ACT',' ',
     2		   'FORMAT','STRING','ULEN', 30,'NELEMENT', 10, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('TARGET_LIST VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF

C-------TELEMETRY_FORMAT_ID
	   CALL XLGET(UNIT,'PROPERTY','TELEMETRY_FORMAT_ID',
     1             LAB_TELEMID,
     2		   ISTAT,'PROPERTY','TELEMETRY','ERR_ACT',' ',
     3		   'FORMAT','STRING','ULEN', 10, ' ')
           IF (ISTAT.NE.1) THEN
		CALL XVMESSAGE('TELEMETRY_FORMAT_ID VALUE MISSING',' ')
		IND=-1
		RETURN
           END IF


	END IF

        RETURN



C-------OTHER CASSINI INSTRUMENTS
200     CALL XVMESSAGE('ONLY ISS INSTRUMENT IMPLEMENTED',' ')
	IND=-1

	RETURN
	END

C==================================================================

      SUBROUTINE ABLE97_INIT      ! set initial values for arguments returned.
c      include 'cas_isslab.fin'               !remove before delivery! 
                                !Specifies COMMON block for ABLE97
                                          !CASSINI-ISS label
      include 'cas_isslab'               
C!Specifies COMMON block for ABLE97
       LAB_SCLK  = -999
       LAB_SCETY  = -999
       LAB_SCETD  = -999
       LAB_SCETH  = -999
       LAB_SCETM  = -999
       LAB_SCETS  = -999
       LAB_SCETMS  = -999
       LAB_SCETY2  = -999
       LAB_SCETD2  = -999
       LAB_SCETH2  = -999
       LAB_SCETM2  = -999
       LAB_SCETS2  = -999
       LAB_SCETMS2  = -999
       LAB_SCETY3  = -999
       LAB_SCETD3  = -999
       LAB_SCETH3  = -999
       LAB_SCETM3  = -999
       LAB_SCETS3  = -999
       LAB_SCETMS3  = -999
       LAB_SCETY4  = -999
       LAB_SCETD4  = -999
       LAB_SCETH4  = -999
       LAB_SCETM4  = -999
       LAB_SCETS4  = -999
       LAB_SCETMS4  = -999
       LAB_SCETY5  = -999
       LAB_SCETD5  = -999
       LAB_SCETH5  = -999
       LAB_SCETM5  = -999
       LAB_SCETS5  = -999
       LAB_SCETMS5  = -999
       LAB_EXPOS  = -999
       LAB_CCDTEMP  = -999
       LAB_OPTTEMP  = -999
       LAB_FLTTEMP  = -999
       LAB_BLOCKS  = -999
       LAB_ALGORITHM  = -999
       LAB_BTYPE  = -999
       LAB_QFACTOR  = -999
       LAB_COMPRAT  = -999
       LAB_MISSING  = -999
       LAB_RADIANCE  = -999
       LAB_PREPCYCLE  = -999
       LAB_READOUTCYCLE  = -999
       LAB_BIAS  = -999
       LAB_EXTPIXVAL  = -999
       LAB_SENSORTEMP  = -999
       LAB_DATARATE  = -999
       LAB_COMPPARAM = -999
       LAB_SCCP = -999
       LAB_COMDSEQNUM = -999
       LAB_SCLKSTART = -999
       LAB_SCLKSTARTSUB = -999
       LAB_SCLKSTOP = -999
       LAB_SCLKSTOPSUB = -999 
       LAB_CLD = -999
       LAB_ELBIAS = -999
       LAB_PCVI = -999
       LAB_EXPPACKETS = -999
       LAB_ORDERNUM = -999
       LAB_RECPACKETS = -999
       LAB_SEQNUM = -999
       LAB_VALIDMAX = -999
       LAB_VALIDMAXFW = -999
       LAB_EXPMAX = -999
       LAB_EXPMAXFW = -999
       LAB_COMPRATEPRED = -999
       LAB_COMPRATEACT = -999
       LAB_REAROPTTEMP  = -999
       LAB_LABTYPE = ' '              ! fills with blanks.
       LAB_PHASE = ' ' 
       LAB_CAMERA = ' ' 
       LAB_SWVERS = ' ' 
       LAB_MODE = ' ' 
       LAB_FILTER1 = ' '
       LAB_FILTER2 = ' ' 
       LAB_GAIN = ' ' 
       LAB_COMPRSN = ' ' 
       LAB_CONVRSN = ' '
       LAB_LTFLD = ' ' 
       LAB_ANTIBLM = ' ' 
       LAB_CALLAMP = ' ' 
       LAB_TARGET = ' '
       LAB_OBSERVID = ' ' 
       LAB_SOURCE = ' ' 
       LAB_SHUTTERMODE = ' '
       LAB_DLYREADOUT = ' '
       LAB_IMAGETIME = ' '
       LAB_MISPACFLG = ' '
       LAB_SHUTID = ' '
       LAB_ERTSTART = ' '
       LAB_ERTSTOP = ' '
       LAB_FLTSOFTVERSID = ' '
       LAB_STARTTIME = ' '
       LAB_DSID = ' '
       LAB_IHN = ' '
       LAB_IN = ' '
       LAB_NOTE = ' '
       LAB_PCTIME = ' '
       LAB_PRID = ' '
       LAB_PVT = ' '
       LAB_SID = ' '
       LAB_CMDNAME = ' '
       DO I=1,5
          LAB_IMGOBSTYPE(i) = ' '
       END DO
       LAB_SEQTITLE = ' '
       LAB_DESCRIPTION = ' '
       LAB_MIDTIME = ' '
       LAB_METHODDESC = ' '
       LAB_TARGETDESC = ' '
       DO I=1,10
          LAB_TARGETLIST(i) = ' '
       END DO
       LAB_TELEMID = ' '
      RETURN
      END

      SUBROUTINE XABLE97(IND,UNIT,SCLK, 
     .       SCETY,     SCETD,      SCETH, 
     .       SCETM,     SCETS,      SCETMS,
     .       SCETY2,    SCETD2,     SCETH2, 
     .       SCETM2,    SCETS2,     SCETMS2,
     .       SCETY3,    SCETD3,     SCETH3, 
     .       SCETM3,    SCETS3,     SCETMS3,
     .       SCETY4,    SCETD4,     SCETH4, 
     .       SCETM4,    SCETS4,     SCETMS4,
     .       SCETY5,    SCETD5,     SCETH5, 
     .       SCETM5,    SCETS5,     SCETMS5,
     .       SCLKSTART, SCLKSTARTSUB,
     .       SCLKSTOP,  SCLKSTOPSUB, SCCP,
     .       CLD,       EXPOS,      CCDTEMP,    
     .       OPTTEMP,   FLTTEMP,    BLOCKS,     
     .       ALGORITHM, BTYPE,      QFACTOR,    
     .       COMPRAT,
     .       MISSING,   RADIANCE,   PREPCYCLE,  
     .       READOUTCYCLE, BIAS,    EXTPIXVAL,  
     .       SENSORTEMP,DATARATE,   COMPPARAM,
     .       COMDSEQNUM, ELBIAS, 
     .       PCVI,      EXPPACKETS, ORDERNUM,
     .       RECPACKETS,SEQNUM,     VALIDMAX,
     .       VALIDMAXFW,EXPMAX,     EXPMAXFW,
     .       COMPRATEPRED, COMPRATEACT, REAROPTTEMP,
     .       LABTYPE,    PHASE,
     .       CAMERA,    SWVERS,     MODE,
     .       GAIN,
     .       COMPRSN,   CONVRSN,    LTFLD,
     .       ANTIBLM,   CALLAMP,    TARGET,
     .       OBSERVID,  SOURCE,
     .       SHUTTERMODE,DLYREADOUT,IMAGETIME,
     .       FILTER1,   FILTER2,
     .       MISPACFLG, SHUTID,
     .       ERTSTART,  ERTSTOP, FLTSOFTVERSID,
     .       STARTTIME, DSID, IHN, IN, NOTE, 
     .       PCTIME, PRID, PVT, SID, CMDNAME,
     .       IMGOBSTYPE,SEQTITLE,   DESCRIPTION,
     .       MIDTIME,   METHODDESC, TARGETDESC,
     .       TARGETLIST, TELEMID)

C##################################################################
C  NAME OF MODULE
C      XABLE97 
C
C  PURPOSE
C      XABLE97 is the Fortran bridge between zable97 and ABLE97.
C  ENVIRONMENT
C      VMS or UNIX  with VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    	1-97	SP  Original version ala VICAR Porting Guide section 9.4.2.
C	2-99 	JRY Added changes for cassini-iss2
C	11-99 	TLT Added IMAGETIME and COMPRESSION_PARAMETER_VALUE
C  01-2000 TLT	changed EXPOS from INTEGER to REAL
C  08-2000 AYS  Updated to accomodate new vicar property labels
C  05-2001 VRH  Changed Characters to Byte arrays in calling statement
C  04-2003 VRH  Updated to accomodate Tour format label
C------------------------------------------------------------------
      INTEGER UNIT,IND
c      include 'cas_isslab.fin'                !remove before delivery
C                                !Specifies COMMON block for ABLE97
C                                          !CASSINI-ISS label
      include 'cas_isslab'               
C !Specifies COMMON block for ABLE97

       INTEGER SCLK, MISSING
       INTEGER SCETY, SCETD, SCETH, SCETM, 
     .     SCETS, SCETMS, SCETY2, SCETD2, 
     .     SCETH2, SCETM2, SCETS2, SCETMS2, 
     .     SCETY3, SCETD3, SCETH3, SCETM3, 
     .     SCETS3, SCETMS3,SCETY4, SCETD4, 
     .     SCETH4, SCETM4, SCETS4, SCETMS4,
     .     SCETY5, SCETD5, SCETH5, SCETM5,
     .     SCETS5, SCETMS5,
     .     BLOCKS, ALGORITHM,BTYPE, QFACTOR
       INTEGER PREPCYCLE, READOUTCYCLE, COMPPARAM
       INTEGER SCCP, COMDSEQNUM, ELBIAS, PCVI
       INTEGER SCLKSTART, SCLKSTARTSUB, SCLKSTOP, SCLKSTOPSUB
       INTEGER EXPPACKETS, ORDERNUM, RECPACKETS, SEQNUM
       INTEGER VALIDMAX, VALIDMAXFW
       REAL CCDTEMP,  OPTTEMP,  FLTTEMP,  COMPRAT
       REAL RADIANCE, EXPOS, CLD
       REAL BIAS, EXTPIXVAL, SENSORTEMP, DATARATE
       REAL EXPMAX, EXPMAXFW, COMPRATEPRED, COMPRATEACT, REAROPTTEMP

       BYTE LABTYPE(8), PHASE(31), CAMERA(5), SWVERS(21), MODE(5), 
     .     GAIN(21), COMPRSN(9), CONVRSN(7), 
     .     LTFLD(4), ANTIBLM(4), CALLAMP(4), TARGET(31), OBSERVID(31), 
     .     SOURCE(19), SHUTTERMODE(9), DLYREADOUT(4),
     .     IMAGETIME(22), FILTER1(6), FILTER2(6),
     .     MISPACFLG(10), SHUTID(11), ERTSTART(22), ERTSTOP(22),
     .     FLTSOFTVERSID(31), STARTTIME(22), DSID(41),
     .     IHN(16), IN(39), NOTE(101), PCTIME(22), PRID(41), PVT(12),
     .     SID(31), CMDNAME(121), IMGOBSTYPE(16,5),
     .     SEQTITLE(61), DESCRIPTION(251), MIDTIME(22),
     .     METHODDESC(101), TARGETDESC(76), TARGETLIST(31,10),
     .     TELEMID(11)

C==================================================================

      CALL ABLE97(IND,UNIT)   ! CALL ACTUAL ROUTINE.

C...Load XABLE97 arguments from COMMON.

       SCLK  = LAB_SCLK
       SCETY  = LAB_SCETY
       SCETD  = LAB_SCETD
       SCETH  = LAB_SCETH
       SCETM  = LAB_SCETM
       SCETS  = LAB_SCETS
       SCETMS  = LAB_SCETMS
       SCETY2  = LAB_SCETY2
       SCETD2  = LAB_SCETD2
       SCETH2  = LAB_SCETH2
       SCETM2  = LAB_SCETM2
       SCETS2  = LAB_SCETS2
       SCETMS2  = LAB_SCETMS2
       SCETY3  = LAB_SCETY3
       SCETD3  = LAB_SCETD3
       SCETH3  = LAB_SCETH3
       SCETM3  = LAB_SCETM3
       SCETS3  = LAB_SCETS3
       SCETMS3  = LAB_SCETMS3
       SCETY4  = LAB_SCETY4
       SCETD4  = LAB_SCETD4
       SCETH4  = LAB_SCETH4
       SCETM4  = LAB_SCETM4
       SCETS4  = LAB_SCETS4
       SCETMS4  = LAB_SCETMS4
       SCETY5  = LAB_SCETY5
       SCETD5  = LAB_SCETD5
       SCETH5  = LAB_SCETH5
       SCETM5  = LAB_SCETM5
       SCETS5  = LAB_SCETS5
       SCETMS5  = LAB_SCETMS5
       SCLKSTART = LAB_SCLKSTART
       SCLKSTARTSUB = LAB_SCLKSTARTSUB
       SCLKSTOP = LAB_SCLKSTOP
       SCLKSTOPSUB = LAB_SCLKSTOPSUB
       SCCP = LAB_SCCP
       CLD = LAB_CLD
       EXPOS  = LAB_EXPOS  
       CCDTEMP  = LAB_CCDTEMP  
       OPTTEMP  = LAB_OPTTEMP  
       FLTTEMP  = LAB_FLTTEMP  
       BLOCKS  = LAB_BLOCKS  
       ALGORITHM  = LAB_ALGORITHM
       BTYPE  = LAB_BTYPE
       QFACTOR  = LAB_QFACTOR
       COMPRAT  = LAB_COMPRAT
       MISSING  = LAB_MISSING  
       RADIANCE  = LAB_RADIANCE  
       PREPCYCLE  = LAB_PREPCYCLE
       READOUTCYCLE  = LAB_READOUTCYCLE
       BIAS  = LAB_BIAS
       EXTPIXVAL  = LAB_EXTPIXVAL
       SENSORTEMP  = LAB_SENSORTEMP
       DATARATE  = LAB_DATARATE
       COMPPARAM = LAB_COMPPARAM
       COMDSEQNUM = LAB_COMDSEQNUM
       ELBIAS = LAB_ELBIAS
       PCVI = LAB_PCVI
       EXPPACKETS = LAB_EXPPACKETS
       ORDERNUM = LAB_ORDERNUM
       RECPACKETS = LAB_RECPACKETS
       SEQNUM = LAB_SEQNUM
       VALIDMAX = LAB_VALIDMAX
       VALIDMAXFW = LAB_VALIDMAXFW
       EXPMAX = LAB_EXPMAX
       EXPMAXFW = LAB_EXPMAXFW
       COMPRATEPRED = LAB_COMPRATEPRED
       COMPRATEACT = LAB_COMPRATEACT
       REAROPTTEMP = LAB_REAROPTTEMP

C... Character variables
       CALL ABLE97_TRIM(LAB_LABTYPE , LABTYPE , 7)
       CALL ABLE97_TRIM(LAB_PHASE , PHASE , 30)
       CALL ABLE97_TRIM(LAB_CAMERA , CAMERA , 5)
       CALL ABLE97_TRIM(LAB_SWVERS , SWVERS , 20)
       CALL ABLE97_TRIM(LAB_MODE , MODE , 4)
       CALL ABLE97_TRIM(LAB_GAIN , GAIN , 20)
       CALL ABLE97_TRIM(LAB_COMPRSN , COMPRSN , 8)
       CALL ABLE97_TRIM(LAB_CONVRSN , CONVRSN , 5)
       CALL ABLE97_TRIM(LAB_LTFLD , LTFLD , 3)
       CALL ABLE97_TRIM(LAB_ANTIBLM , ANTIBLM , 3)
       CALL ABLE97_TRIM(LAB_CALLAMP , CALLAMP , 3)
       CALL ABLE97_TRIM(LAB_TARGET , TARGET , 30)
       CALL ABLE97_TRIM(LAB_OBSERVID , OBSERVID , 30)
       CALL ABLE97_TRIM(LAB_SOURCE , SOURCE , 18)
       CALL ABLE97_TRIM(LAB_SHUTTERMODE , SHUTTERMODE , 8)
       CALL ABLE97_TRIM(LAB_DLYREADOUT , DLYREADOUT , 3)
       CALL ABLE97_TRIM(LAB_IMAGETIME, IMAGETIME, 21)
       CALL ABLE97_TRIM(LAB_FILTER1 , FILTER1 , 5)
       CALL ABLE97_TRIM(LAB_FILTER2 , FILTER2 , 5)
       CALL ABLE97_TRIM(LAB_MISPACFLG, MISPACFLG, 9)
       CALL ABLE97_TRIM(LAB_SHUTID, SHUTID, 10)
       CALL ABLE97_TRIM(LAB_ERTSTART, ERTSTART, 21)
       CALL ABLE97_TRIM(LAB_ERTSTOP, ERTSTOP, 21)
       CALL ABLE97_TRIM(LAB_FLTSOFTVERSID, FLTSOFTVERSID, 30)
       CALL ABLE97_TRIM(LAB_STARTTIME, STARTTIME, 21)
       CALL ABLE97_TRIM(LAB_DSID, DSID, 40)
       CALL ABLE97_TRIM(LAB_IHN, IHN, 15)       
       CALL ABLE97_TRIM(LAB_IN, IN, 38)
       CALL ABLE97_TRIM(LAB_NOTE, NOTE, 100)
       CALL ABLE97_TRIM(LAB_PCTIME, PCTIME, 21)
       CALL ABLE97_TRIM(LAB_PRID, PRID, 40)
       CALL ABLE97_TRIM(LAB_PVT, PVT, 11)
       CALL ABLE97_TRIM(LAB_SID, SID, 30)
       CALL ABLE97_TRIM(LAB_CMDNAME, CMDNAME, 120)
       DO I=1,5
          CALL ABLE97_TRIM(LAB_IMGOBSTYPE(i), IMGOBSTYPE(1,i), 15)
       END DO
       CALL ABLE97_TRIM(LAB_SEQTITLE, SEQTITLE, 60)
       CALL ABLE97_TRIM(LAB_DESCRIPTION, DESCRIPTION, 250)
       CALL ABLE97_TRIM(LAB_MIDTIME, MIDTIME, 21)
       CALL ABLE97_TRIM(LAB_METHODDESC, METHODDESC, 100)
       CALL ABLE97_TRIM(LAB_TARGETDESC, TARGETDESC, 75)
       DO I=1,10
          CALL ABLE97_TRIM(LAB_TARGETLIST(i), TARGETLIST(1,i), 30)
       END DO
       CALL ABLE97_TRIM(LAB_TELEMID, TELEMID, 10)

      RETURN
      END

C==================================================================

      SUBROUTINE ABLE97_TRIM(CHAR, ARRAY, LENGTH)  ! Like MVCL but
                                                   ! Null-terminates
      CHARACTER*(*) CHAR
      BYTE ARRAY(LENGTH+1)          !Byte array should be 1 longer than
      INTEGER I, STRLEN, MAXLEN     !length of string

C...Sanity check on the length      
      MAXLEN = MIN(LEN(CHAR), LENGTH)

C...Find the last non-blank character
      STRLEN = MAXLEN
      DO I=MAXLEN,1,-1
        STRLEN = I
        IF (CHAR(I:I) .NE. ' ') GOTO 100
      END DO

C...Blank string
      ARRAY(1) = 0
      RETURN

C...Create a null-terminated C string (byte array)
100   CALL MVCL(CHAR, ARRAY, STRLEN)
      ARRAY(STRLEN+1) = 0

      RETURN
      END


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zable97.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**********************************************************************
    Revision History
     1-97  SP   Original version of C bridge.
     2-99  JRY  Added to able97_typ for CASSINI-ISS2 labels
    10-99  TLT  Added imagetime and compparam
                Renamed lab_prepdur to lab_prepcycle
		Renamed lab_readoutdur to lab_readoutcycle
		Added lab_shuttertype
     5-00  AYS  Modified calls to sfor2c to output to the c string
     8-00  AYS  Updated to accomodate new vicar property labels.
     5-01  VRH  Not to be called by Fortran, removed Fortran tags
                Moved c string creation to xlabel97
     4-03  VRH  Updated to accomodate tour labels.
**********************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "cas_isslab.h"
/*---------------------------------------------------------------------------*/
/* c-callable version                                                        */
/*---------------------------------------------------------------------------*/
void zable97( ind, unit, lab)

      int  *ind, unit;
      able97_typ *lab;
{

FTN_NAME2(xable97, XABLE97) ( ind, &unit, &lab->sclk, 
             &lab->scety,     &lab->scetd,      &lab->sceth, 
             &lab->scetm,     &lab->scets,      &lab->scetms,
             &lab->scety2,     &lab->scetd2,      &lab->sceth2, 
             &lab->scetm2,     &lab->scets2,      &lab->scetms2,
             &lab->scety3,     &lab->scetd3,      &lab->sceth3, 
             &lab->scetm3,     &lab->scets3,      &lab->scetms3,
             &lab->scety4,     &lab->scetd4,      &lab->sceth4, 
             &lab->scetm4,     &lab->scets4,      &lab->scetms4,
             &lab->scety5,     &lab->scetd5,      &lab->sceth5, 
             &lab->scetm5,     &lab->scets5,      &lab->scetms5,
             &lab->sclkstart, &lab->sclkstartsub,
             &lab->sclkstop, &lab->sclkstopsub, &lab->sccp, &lab->cld,
             &lab->expos,     &lab->ccdtemp,    &lab->opttemp, &lab->flttemp, 
             &lab->blocks,    &lab->algorithm,  &lab->btype,   &lab->qfactor, 
             &lab->comprat,   &lab->missing, 
             &lab->radiance,  &lab->prepcycle,   &lab->readoutcycle,
             &lab->bias,     &lab->extpixval, &lab->sensortemp,
             &lab->datarate, &lab->compparam, &lab->comdseqnum, 
             &lab->elbias, &lab->pcvi,
             &lab->exppackets, &lab->ordernum, &lab->recpackets, &lab->seqnum,
             &lab->validmax, &lab->validmaxfw, &lab->expmax, &lab->expmaxfw,
             &lab->compratepred, &lab->comprateact, &lab->rearopttemp,
             lab->labtype,    lab->phase,     lab->camera, 
             lab->swvers,     lab->mode,
             lab->gain,       lab->comprsn,   lab->convrsn,  lab->ltfld,
             lab->antiblm,    lab->callamp,   lab->target,   lab->observid,
             lab->source,     lab->shuttermode,
	     lab->dlyreadout,lab->imagetime,
	     lab->filter1, lab->filter2,
             lab->mispacflg, lab->shutid,
             lab->ertstart, lab->ertstop, lab->fltsoftversid, 
             lab->starttime, lab->dsid, lab->ihn, lab->in, 
             lab->note, lab->pctime, lab->prid,
             lab->pvt, lab->sid, lab->cmdname, lab->imgobstype, 
             lab->seqtitle,
             lab->description, lab->midtime, lab->methoddesc, lab->targetdesc,
             lab->targetlist, lab->telemid);

}


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create able97.imake
/* Imake file for VICAR subroutine able97 */

#define SUBROUTINE able97
#define P2_SUBLIB

#define MODULE_LIST able97.f zable97.c

#define FTNINC_LIST cas_isslab

#define USES_FORTRAN
#define USES_C

#define FTN_STRING

/*#define LIB_LOCAL	/* remove on delivery */
/*#define DEBUG		/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create table97.f
      INCLUDE 'VICMAIN_FOR'
C Test of subroutine ABLE97
C 
C History:
C     tlt  Renamed LAB_PREPDUR to LAB_PREPCYCLE
C     tlt  Renamed LAB_READOUTDUR to LAB_READOUTCYCLE
C     ays  Updated to accomodate new vicar property
C          labels
C
      SUBROUTINE MAIN44
      INTEGER UNIT
      INCLUDE 'cas_isslab'          !SETS UP COMMON BLOCK
c      INCLUDE 'cas_isslab.fin'          !REMOVE FOR DELIVERY

      CALL XVUNIT(UNIT,'INP',1,ISTAT,' ')
      CALL XVOPEN(UNIT,ISTAT,' ')
      IF(ISTAT.NE.1)THEN
         CALL XVMESSAGE('***CAN NOT OPEN INPUT',' ')
         CALL ABEND
      ENDIF
	
      CALL ABLE97(IND,UNIT)

      CALL PRINTSTR(7,LAB_LABTYPE,'LABEL TYPE = ')
      CALL PRINTSTR(30,LAB_PHASE,'MISSION PHASE = ')
      CALL PRINTSTR(5,LAB_CAMERA,'CAMERA ID = ')
      CALL PRNT(4,1,LAB_SCLK,'SCLK = ')
      CALL PRNT(4,1,LAB_SCETY,'EVENT TIME YEAR = ')
      CALL PRNT(4,1,LAB_SCETD,'EVENT TIME DAY = ')
      CALL PRNT(4,1,LAB_SCETH,'EVENT TIME HOUR = ')
      CALL PRNT(4,1,LAB_SCETM,'EVENT TIME MINUTE = ')
      CALL PRNT(4,1,LAB_SCETS,'EVENT TIME SECOND = ')
      CALL PRNT(4,1,LAB_SCETMS,'EVENT TIME MSEC = ')
      CALL PRNT(4,1,LAB_SCETY2,'EARTH RECEIVED START TIME YEAR = ')
      CALL PRNT(4,1,LAB_SCETD2,'EARTH RECEIVED START TIME DAY = ')
      CALL PRNT(4,1,LAB_SCETH2,'EARTH RECEIVED START TIME HOUR = ')
      CALL PRNT(4,1,LAB_SCETM2,'EARTH RECEIVED START TIME MINUTE = ')
      CALL PRNT(4,1,LAB_SCETS2,'EARTH RECEIVED START TIME SECOND = ')
      CALL PRNT(4,1,LAB_SCETMS2,'EARTH RECEIVED START TIME MSEC = ')
      CALL PRNT(4,1,LAB_SCETY3,'EARTH RECEIVED STOP TIME YEAR = ')
      CALL PRNT(4,1,LAB_SCETD3,'EARTH RECEIVED STOP TIME DAY = ')
      CALL PRNT(4,1,LAB_SCETH3,'EARTH RECEIVED STOP TIME HOUR = ')
      CALL PRNT(4,1,LAB_SCETM3,'EARTH RECEIVED STOP TIME MINUTE = ')
      CALL PRNT(4,1,LAB_SCETS3,'EARTH RECEIVED STOP TIME SECOND = ')
      CALL PRNT(4,1,LAB_SCETMS3,'EARTH RECEIVED STOP TIME MSEC = ')
      CALL PRNT(4,1,LAB_SCETY4,'START TIME YEAR = ')
      CALL PRNT(4,1,LAB_SCETD4,'START TIME DAY = ')
      CALL PRNT(4,1,LAB_SCETH4,'START TIME HOUR = ')
      CALL PRNT(4,1,LAB_SCETM4,'START TIME MINUTE = ')
      CALL PRNT(4,1,LAB_SCETS4,'START TIME SECOND = ')
      CALL PRNT(4,1,LAB_SCETMS4,'START TIME MSEC = ')
      CALL PRNT(4,1,LAB_SCETY5,'MID TIME YEAR = ')
      CALL PRNT(4,1,LAB_SCETD5,'MID TIME DAY = ')
      CALL PRNT(4,1,LAB_SCETH5,'MID TIME HOUR = ')
      CALL PRNT(4,1,LAB_SCETM5,'MID TIME MINUTE = ')
      CALL PRNT(4,1,LAB_SCETS5,'MID TIME SECOND = ')
      CALL PRNT(4,1,LAB_SCETMS5,'MID TIME MSEC = ')
      CALL PRNT(4,1,LAB_SCLKSTART,'SCLKSTART = ')
      CALL PRNT(4,1,LAB_SCLKSTARTSUB,'SCLKSTARTSUB = ')
      CALL PRNT(4,1,LAB_SCLKSTOP, 'SCLKSTOP = ')
      CALL PRNT(4,1,LAB_SCLKSTOPSUB, 'SCLKSTOPSUB = ')
      CALL PRINTSTR(20,LAB_SWVERS,'IMAGE BUILDER S/W VERSION = ')
      CALL PRINTSTR(4,LAB_MODE,'INSTRUMENT MODE = ')
      CALL PRINTSTR(20,LAB_GAIN,'GAIN STATE/_MODE_ID  = ')
      CALL PRNT(7,1,LAB_EXPOS,'EXPOSURE TIME =')
      CALL PRINTSTR(8,LAB_COMPRSN,'COMPRESSION = ')
      CALL PRINTSTR(6,LAB_CONVRSN,'ENCODING/DATA_CONVERSION_TYPE = ')
      CALL PRNT(7,1,LAB_CCDTEMP,'CDD TEMP = ')
      CALL PRNT(7,1,LAB_OPTTEMP,'OPTICS TEMP = ')
      CALL PRNT(7,1,LAB_REAROPTTEMP,'REAR OPTICS TEMP = ')
      CALL PRNT(7,1,LAB_FLTTEMP,'FILTER TEMP = ')
      CALL PRINTSTR(3,LAB_LTFLD,'LIGHT FLOOD STATE = ')
      CALL PRINTSTR(3,LAB_ANTIBLM,'ANTI-BLOOMING STATE = ')
      CALL PRINTSTR(3,LAB_CALLAMP,'CALIBRATION LAMP STATE = ')
      CALL PRNT(4,1,LAB_BLOCKS,'BLOCKS/GROUP = ')
      CALL PRNT(4,1,LAB_ALGORITHM,'ALGORITHM = ')
      CALL PRNT(4,1,LAB_BTYPE,'BLOCK TYPE = ')
      CALL PRNT(4,1,LAB_QFACTOR,'Q FACTOR INDEX = ')
      CALL PRNT(7,1,LAB_COMPRAT,'CMPRSN RATIO = ')
      CALL PRNT(4,1,LAB_MISSING,'MISSING LINES = ')
      CALL PRINTSTR(30,LAB_TARGET,'TARGET_NAME = ')
      CALL PRINTSTR(30,LAB_OBSERVID,'OBSERVATION ID = ')
      CALL PRINTSTR(18,LAB_SOURCE,'ILLUMINANT = ')
      CALL PRNT(7,1,LAB_RADIANCE,'RADIANCE = ')
      CALL PRNT(4,1,LAB_PREPCYCLE,'PREPARE CYCLE INDEX = ')
      CALL PRNT(4,1,LAB_READOUTCYCLE,'READOUT CYCLE INDEX = ')
      CALL PRNT(7,1,LAB_BIAS,'BIAS/BIAS_STRIP_MEAN = ')
      CALL PRNT(7,1,LAB_EXTPIXVAL,'DARK/EXTENDED PIXEL VALUE = ')
      CALL PRNT(7,1,LAB_SENSORTEMP,'SENSOR HEAD ELEC TEMP = ')
      CALL PRNT(7,1,LAB_DATARATE,'INSTRUMENT DATA RATE = ')
      CALL PRINTSTR(8,LAB_SHUTTERMODE,
     1              'SHUTTER MODE (NACONLY/WACONLY/BOTSIM) = ')
      CALL PRINTSTR(3,LAB_DLYREADOUT,'DELAYED READOUT FLAG = ')
      CALL PRINTSTR(21,LAB_IMAGETIME,'IMAGETIME = ')
      CALL PRNT(4,1,LAB_COMPPARAM,'COMPPARAM = ')
      CALL PRNT(4,1,LAB_SCCP,'SPACECRAFT CLOCK CNT PARTITION = ')
      CALL PRNT(4,1,LAB_COMDSEQNUM,'COMMAND SEQUENCE NUMBER = ')
      CALL PRINTSTR(5,LAB_FILTER1,'FILTER_NAME1 = ')
      CALL PRINTSTR(5,LAB_FILTER2,'FILTER_NAME2 = ')
C      CALL PRINTSTR(21,LAB_CMPRSPARAM,'INSTRUMENT_CMPRS_PARAM = ')
      CALL PRINTSTR(9,LAB_MISPACFLG,'MISSING_PACKET_FLAG = ')
      CALL PRINTSTR(10,LAB_SHUTID,'SHUTTER STATE (ENABLED/DISABLED) = ')
      CALL PRINTSTR(21,LAB_ERTSTART,'EARTH_RECEIVED_START_TIME = ')
      CALL PRINTSTR(21,LAB_ERTSTOP,'EARTH_RECEIVED_STOP_TIME = ')
      CALL PRINTSTR(30,LAB_FLTSOFTVERSID,
     1    'FLIGHT_SOFTWARE_VERSION_ID = ')
      CALL PRINTSTR(21,LAB_STARTTIME,'START_TIME = ')
      CALL PRNT(7,1,LAB_CLD,'CALIBRATION_LAMP_DURATION = ')
      CALL PRNT(4,1,LAB_ELBIAS,'ELECTRONIC_BIAS = ')
      CALL PRNT(4,1,LAB_PCVI,'PARALLEL_CLOCK_VOLTAGE_INDEX = ')
      CALL PRNT(4,1,LAB_EXPPACKETS,'EXPECTED_PACKETS = ')
      CALL PRNT(4,1,LAB_RECPACKETS,'RECEIVED_PACKETS = ')
      CALL PRNT(4,1,LAB_ORDERNUM,'ORDER_NUMBER = ')
      CALL PRNT(4,1,LAB_SEQNUM,'SEQUENCE_NUMBER = ')
      CALL PRNT(4,1,LAB_VALIDMAX,'VALID_MAXIMUM = ')
      CALL PRNT(4,1,LAB_VALIDMAXFW,'VALID_MAXIMUM (FULL WELL) = ')
      CALL PRNT(7,1,LAB_EXPMAX,'EXPECTED_MAXIMUM (% DN) = ')
      CALL PRNT(7,1,LAB_EXPMAXFW,'EXPECTED_MAXIMUM (% FULL WELL) = ')
      CALL PRNT(7,1,LAB_COMPRATEPRED,'INST_CMPRS_RATE (PREDICTED) = ')
      CALL PRNT(7,1,LAB_COMPRATEACT,'INST_CMPRS_RATE (ACTUAL) = ')
      CALL PRINTSTR(40,LAB_DSID,'DATA_SET_ID = ')
      CALL PRINTSTR(15,LAB_IHN,'INSTRUMENT_HOST_NAME = ')
      CALL PRINTSTR(38,LAB_IN,'INSTRUMENT_NAME = ')
      CALL PRINTSTR(100,LAB_NOTE,'NOTE = ')
      CALL PRINTSTR(21,LAB_PCTIME,'PRODUCT_CREATION_TIME = ')
      CALL PRINTSTR(40,LAB_PRID,'PRODUCT_ID = ')
      CALL PRINTSTR(11,LAB_PVT,'PRODUCT_VERSION_TYPE = ')
      CALL PRINTSTR(30,LAB_SID,'SEQUENCE_ID = ')
      CALL PRINTSTR(120,LAB_CMDNAME,'COMMAND_FILE_NAME = ')
      CALL PRINTSTR(15,LAB_IMGOBSTYPE(1),
     1     'IMAGE_OBSERVATION_TYPE = ')
      DO I=2,5
         CALL PRINTSTR(15,LAB_IMGOBSTYPE(i),
     1     '                       = ')
      END DO
      CALL PRINTSTR(60,LAB_SEQTITLE,'SEQUENCE_TITLE = ')
      CALL PRINTSTR(21,LAB_MIDTIME,'IMAGE_MID_TIME = ')
      CALL PRINTSTR(250,LAB_DESCRIPTION,'DESCRIPTION = ')
      CALL PRINTSTR(100,LAB_METHODDESC,'METHOD_DESC = ')
      CALL PRINTSTR(75,LAB_TARGETDESC,'TARGET_DESC = ')
      CALL PRINTSTR(30,LAB_TARGETLIST(1),'TARGET_LIST = ')
      DO I=2,10
         CALL PRINTSTR(30,LAB_TARGETLIST(i),'            = ')
      END DO
      CALL PRINTSTR(10,LAB_TELEMID,'TELEMETRY_FORMAT_ID = ')
 
C
      CALL PRNT(4,1,IND,'IND = ')

C..REPEAT TEST FROM C
      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface:', ' ')

      CALL TZABLE97(UNIT)
      RETURN
      END

      SUBROUTINE PRINTSTR( LENGTH, STR, TITLE)
C
C      SUBSTITUTE FOR prnt for DCODE=99.  PRNT does not handle CHARACTER.

      CHARACTER STR*(*), TITLE*(*)
      CHARACTER*249 PBUF
C      Can handle maximum of 249 characters (Len STR + Len TITLE <249) 
C==================================================================
      L = LEN(TITLE)

C...Find the last non-blank character
      DO I=LENGTH,1,-1
        ILENGTH = I
        IF (STR(I:I) .NE. ' ') GOTO 100
      END DO

100   LLAST = L + ILENGTH
      IF (LLAST .GE. 250) THEN
         CALL XVMESSAGE(TITLE, ' ')
         CALL XVMESSAGE(STR(1:ILENGTH), ' ')
      ELSE
         PBUF(1:L) = TITLE
         PBUF(L+1:MIN(LLAST,249)) = STR          ! CONCATENATE
         CALL XVMESSAGE(PBUF(1:MIN(LLAST,249)), ' ')
      ENDIF
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzable97.c
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "cas_isslab.h"

void FTN_NAME(tzable97)(unit)
int *unit;
{
      int ind;
      int i;
      able97_typ lab;
/*  ==================================================================  */

      zable97(&ind,*unit,&lab);

      zprintstr(7,lab.labtype,"LABEL TYPE = ");
      zprintstr(30,lab.phase,"MISSION PHASE = ");
      zprintstr(5,lab.camera,"CAMERA ID = ");
      zprnt(4,1,&lab.sclk,"SCLK = ");
      zprnt(4,1,&lab.scety,"EVENT TIME YEAR = ");
      zprnt(4,1,&lab.scetd,"EVENT TIME DAY = ");
      zprnt(4,1,&lab.sceth,"EVENT TIME HOUR = ");
      zprnt(4,1,&lab.scetm,"EVENT TIME MINUTE = ");
      zprnt(4,1,&lab.scets,"EVENT TIME SECOND = ");
      zprnt(4,1,&lab.scetms,"EVENT TIME MSEC = ");
      zprnt(4,1,&lab.scety2,"EARTH RECEIVED START TIME YEAR = ");
      zprnt(4,1,&lab.scetd2,"EARTH RECEIVED START TIME DAY = ");
      zprnt(4,1,&lab.sceth2,"EARTH RECEIVED START TIME HOUR = ");
      zprnt(4,1,&lab.scetm2,"EARTH RECEIVED START TIME MINUTE = ");
      zprnt(4,1,&lab.scets2,"EARTH RECEIVED START TIME SECOND = ");
      zprnt(4,1,&lab.scetms2,"EARTH RECEIVED START TIME MSEC = ");
      zprnt(4,1,&lab.scety3,"EARTH RECEIVED STOP TIME YEAR = ");
      zprnt(4,1,&lab.scetd3,"EARTH RECEIVED STOP TIME DAY = ");
      zprnt(4,1,&lab.sceth3,"EARTH RECEIVED STOP TIME HOUR = ");
      zprnt(4,1,&lab.scetm3,"EARTH RECEIVED STOP TIME MINUTE = ");
      zprnt(4,1,&lab.scets3,"EARTH RECEIVED STOP TIME SECOND = ");
      zprnt(4,1,&lab.scetms3,"EARTH RECEIVED STOP TIME MSEC = ");
      zprnt(4,1,&lab.scety4,"START TIME YEAR = ");
      zprnt(4,1,&lab.scetd4,"START TIME DAY = ");
      zprnt(4,1,&lab.sceth4,"START TIME HOUR = ");
      zprnt(4,1,&lab.scetm4,"START TIME MINUTE = ");
      zprnt(4,1,&lab.scets4,"START TIME SECOND = ");
      zprnt(4,1,&lab.scetms4,"START TIME MSEC = ");
      zprnt(4,1,&lab.scety5,"MID TIME YEAR = ");
      zprnt(4,1,&lab.scetd5,"MID TIME DAY = ");
      zprnt(4,1,&lab.sceth5,"MID TIME HOUR = ");
      zprnt(4,1,&lab.scetm5,"MID TIME MINUTE = ");
      zprnt(4,1,&lab.scets5,"MID TIME SECOND = ");
      zprnt(4,1,&lab.scetms5,"MID TIME MSEC = ");
      zprnt(4,1,&lab.sclkstart,"SCLKSTART = ");
      zprnt(4,1,&lab.sclkstartsub, "SCLKSTARTSUB = ");
      zprnt(4,1,&lab.sclkstop, "SCLKSTOP = ");
      zprnt(4,1,&lab.sclkstopsub, "SCLKSTOPSUB = ");
      zprintstr(20,lab.swvers,"IMAGE BUILDER S/W VERSION = ");
      zprintstr(4,lab.mode,"INSTRUMENT MODE = ");
      zprintstr(20,lab.gain,"GAIN STATE/_MODE_ID = ");
      zprnt(7,1,&lab.expos,"EXPOSURE TIME =");
      zprintstr(8,lab.comprsn,"COMPRESSION = ");
      zprintstr(6,lab.convrsn,"ENCODING/DATA_CONVERSION_TYPE = ");
      zprnt(7,1,&lab.ccdtemp,"CDD TEMP = ");
      zprnt(7,1,&lab.opttemp,"OPTICS TEMP = ");
      zprnt(7,1,&lab.rearopttemp,"REAR OPTICS TEMP = ");
      zprnt(7,1,&lab.flttemp,"FILTER TEMP = ");
      zprintstr(3,lab.ltfld,"LIGHT FLOOD STATE = ");
      zprintstr(3,lab.antiblm,"ANTI-BLOOMING STATE = ");
      zprintstr(3,lab.callamp,"CALIBRATION LAMP STATE = ");
      zprnt(4,1,&lab.blocks,"BLOCKS/GROUP = ");
      zprnt(4,1,&lab.algorithm,"ALGORITHM = ");
      zprnt(4,1,&lab.btype,"BLOCK TYPE = ");
      zprnt(4,1,&lab.qfactor,"Q FACTOR INDEX = ");
      zprnt(7,1,&lab.comprat,"CMPRSN RATIO = ");
      zprnt(4,1,&lab.missing,"MISSING LINES = ");
      zprintstr(30,lab.target,"TARGET_NAME = ");
      zprintstr(30,lab.observid,"OBSERVATION ID = ");
      zprintstr(18,lab.source,"ILLUMINANT = ");
      zprnt(7,1,&lab.radiance,"RADIANCE = ");
      zprnt(4,1,&lab.prepcycle,"PREPARE CYCLE INDEX = ");
      zprnt(4,1,&lab.readoutcycle,"READOUT CYCLE INDEX = ");
      zprnt(7,1,&lab.bias,"OFFSET/BIAS/BIAS_STRIP_MEAN = ");
      zprnt(7,1,&lab.extpixval,"DARK/EXTENDED PIXEL VALUE = ");
      zprnt(7,1,&lab.sensortemp,"SENSOR HEAD ELEC TEMP = ");
      zprnt(7,1,&lab.datarate,"INSTRUMENT DATA RATE = ");
      zprintstr(8,lab.shuttermode,"SHUTTER MODE (NACONLY/WACONLY/BOTSIM) = ");
      zprintstr(3,lab.dlyreadout,"DELAYED READOUT FLAG = ");
      zprintstr(21,lab.imagetime,"IMAGETIME = ");
      zprnt(4,1,&lab.compparam,"COMPPARAM = ");
      zprnt(4,1,&lab.sccp,"SPACECRAFT CLOCK CNT PARTITION = ");
      zprnt(4,1,&lab.comdseqnum,"COMMAND SEQUENCE NUMBER = ");
      zprintstr(5,lab.filter1,"FILTER_NAME1 = ");
      zprintstr(5,lab.filter2,"FILTER_NAME2 = ");
      zprintstr(9,lab.mispacflg,"MISSING_PACKET_FLAG = ");
      zprintstr(10,lab.shutid,"SHUTTER STATE (ENABLED/DISABLED) = ");
      zprintstr(21,lab.ertstart,"EARTH_RECEIVED_START_TIME = ");
      zprintstr(21,lab.ertstop,"EARTH_RECEIVED_STOP_TIME = ");
      zprintstr(30,lab.fltsoftversid,"FLIGHT_SOFTWARE_VERSION_ID = ");
      zprintstr(21,lab.starttime,"START_TIME = ");
      zprnt(7,1,&lab.cld,"CALIBRATION_LAMP_DURATION = ");
      zprnt(4,1,&lab.elbias,"ELECTRONIC_BIAS = ");
      zprnt(4,1,&lab.pcvi,"PARALLEL_CLOCK_VOLTAGE_INDEX = ");
      zprnt(4,1,&lab.exppackets,"EXPECTED_PACKETS = ");
      zprnt(4,1,&lab.recpackets,"RECEIVED_PACKETS = ");
      zprnt(4,1,&lab.ordernum,"ORDER_NUMBER = ");
      zprnt(4,1,&lab.seqnum,"SEQUENCE_NUMBER = ");
      zprnt(4,1,&lab.validmax,"VALID_MAXIMUM = ");
      zprnt(4,1,&lab.validmaxfw,"VALID_MAXIMUM (FULL WELL) = ");
      zprnt(7,1,&lab.expmax,"EXPECTED_MAXIMUM (% DN) = ");
      zprnt(7,1,&lab.expmaxfw,"EXPECTED_MAXIMUM (% FULL WELL) = ");
      zprnt(7,1,&lab.compratepred,"INST_CMPRS_RATE (PREDICTED) = ");
      zprnt(7,1,&lab.comprateact,"INST_CMPRS_RATE (ACTUAL) = ");
      zprintstr(40,lab.dsid,"DATA_SET_ID = ");
      zprintstr(15,lab.ihn,"INSTRUMENT_HOST_NAME = ");
      zprintstr(38,lab.in,"INSTRUMENT_NAME = ");
      zprintstr(100,lab.note,"NOTE = ");
      zprintstr(21,lab.pctime,"PRODUCT_CREATION_TIME = ");
      zprintstr(40,lab.prid,"PRODUCT_ID = ");
      zprintstr(11,lab.pvt,"PRODUCT_VERSION_TYPE = ");
      zprintstr(30,lab.sid,"SEQUENCE_ID = ");
      zprintstr(120,lab.cmdname, "COMMAND_FILE_NAME = ");
      zprintstr(15,lab.imgobstype[0], "IMAGE_OBSERVATION_TYPE = ");
      for (i=1; i<5; i++) {
          zprintstr(15,lab.imgobstype[i], "                       = ");
      }
      zprintstr(60,lab.seqtitle, "SEQUENCE_TITLE = ");
      zprintstr(21,lab.midtime, "IMAGE_MID_TIME = ");
      zprintstr(250,lab.description, "DESCRIPTION = ");
      zprintstr(100,lab.methoddesc, "METHOD_DESC = ");
      zprintstr(75,lab.targetdesc, "TARGET_DESC = ");
      zprintstr(30,lab.targetlist[0], "TARGET_LIST = ");
      for (i=1; i<10; i++) {
          zprintstr(30,lab.targetlist[i], "            = ");
      }
      zprintstr(10,lab.telemid, "TELEMETRY_FORMAT_ID = ");
      zprnt(4,1,&ind,"IND = ");
}

zprintstr(n, buf, title)  /*  print title and then string of n characters.  */

int n;          /* number of characters to print  */
char *title;    /* string to be printed in front of data */
char *buf;        /* string to be printed*/
{
       char pbuf[251];
       int ns;
/*  ==================================================================  */

       if (strlen(title) + strlen(buf) > 250) {
         zvmessage(title,"");
         zvmessage(buf,"");
       }
       else {
         strcpy(pbuf,title);
         ns = n;
         if(n > 250 - strlen(title)) ns = 250 - strlen(title);
         strncat(pbuf,buf,ns);
         zvmessage(pbuf,"");
       }
}




$!-----------------------------------------------------------------------------
$ create table97.imake
/* Imake file for Test of VICAR subroutine able97 */

#define PROGRAM table97

#define MODULE_LIST table97.f tzable97.c
#define FTNINC_LIST cas_isslab

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

/*#define LIB_LOCAL	/* REMOVE FOR DELIVERY */
/*#define LOCAL_LIBRARY `ar t sublib.a`	/* REMOVE FOR DELIVERY */
/*#define DEBUG		/* REMOVE FOR DELIVERY */
$!-----------------------------------------------------------------------------
$ create table97.pdf
PROCESS
PARM INP  TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tstable97.pdf
procedure help=*
refgbl $echo
refgbl $syschar
LOCAL DIR    TYPE=STRING 
LOCAL INPIC   TYPE=STRING
body
let _onfail="continue"
let $echo="no"

WRITE "THIS IS A TEST OF MODULE ABLE97"
WRITE " "
write " Test Cassini ISS ground label"
WRITE " "
if ($syschar(1) = "UNIX")
   LET INPIC   ="/project/test_work/testdata/cassini/iss/labtest.img"
else 
   LET INPIC   ="WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]LABTEST.IMG"
end-if

let $echo="yes"
label-list &INPIC

TABLE97 &INPIC

let $echo="no"
WRITE " "
write " Test Cassini ISS2 label"
WRITE " "
if ($syschar(1) = "UNIX")
   LET INPIC   ="/project/test_work/testdata/cassini/casIss/y2.img"
else 
   LET INPIC   ="WMS_TEST_WORK:[TESTDATA.CASSINI.CAS$I$SS]Y2.IMG"
end-if

let $echo="yes"
label-list &INPIC

TABLE97 &INPIC

let $echo="no"
WRITE " "
write " Test Cassini ISS new cruise label"
WRITE " "

if ($syschar(1) = "UNIX")
   LET INPIC   ="/project/test_work/testdata/cassini/casIss/n1356781097.2"
else 
   LET INPIC   ="WMS_TEST_WORK:[TESTDATA.CASSINI.CAS$I$SS]N1356781097.2"
end-if

let $echo="yes"
label-list &INPIC

TABLE97 &INPIC

let $echo="no"
WRITE " "
write " Test Cassini ISS tour label"
WRITE " "

if ($syschar(1) = "UNIX")
   LET INPIC   = "/project/test_work/testdata/cassini/iss/N1358285193_7.IMG"
else 
   LET INPIC   ="WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]$N1358285193_7.IMG"
end-if

let $echo="yes"
label-list &INPIC

TABLE97 &INPIC

end-proc
.title
Test Procedure for the subroutine, ABLE97.
.help

   Intended to provide the unit test for the subroutine, ABLE97.
   The test comprises:

       1. Listing the label of a Cassini ISS ground label LABEL-LIST,
       2. And comparing that with the list from the test program.
          TABLE97, which uses ABLE97 to obtain the same information.

   NOTE: Users should understand the Cassini flight and calibration labels
         to correctly analyze the unit test results. The description of
         the labels is documented in a SIS called EGSE VICAR IMAGE DATA FILE,
         C. Avis and S. Kaki.
.end

$ Return
$!#############################################################################
$Other_File:
$ create able97.hlp
ABLE97 - Label extractor for Cassini ISS

PURPOSE:

ABLE97 extracts data from the Cassini ISS flight and ground calibration
labels.

2  CALLING SEQUENCE

  FORTRAN Calling Sequence:  
                   INTEGER IND,UNIT
                   INCLUDE 'cas_isslab'      !Specifies COMMON block for ABLE97

                   CALL ABLE97(IND,UNIT)

  C Calling Sequence:  #include "cas_isslab.h"
                        able97_typ lab;

                        zable97(&ind,unit,&lab);

where
  IND  = return indicator (output)
  UNIT = VICAR unit number of input file (input)
  lab  = (only for zable97) structure containing extracted label data (output)


Arguments

  IND:  is the return indicator (output integer):
	IND<0 if one or more data items are missing
	IND=0 if all data items are present

  UNIT: specifies the VICAR file unit number of the input file	(input integer).

  lab:  (only for zable97) is a structure containing the items of extracted
	data. The structure of lab is defined in file cas_isslab.h.
        Upon return, the structure is filled as shown in Table 1.
  CAS_ISSLAB:  (only for Fortan calls to ABLE97)  
                ABLE97 uses the COMMON block CAS_ISSLAB as a vehicle for 
                returning data instead of a long argument list or a buffer of 
                various values of various data types.  The elements of this
                common block are shown in Table 2.  (If a Fortran application
                needs to call able97 for more than one image, remember that
                there is only 1 common block named CAS_ISSLAB.  In such a case,
                it is recommended that any elements needed by the application
                be copied immediately after the calls to able97.)

Operation

Before calling ABLE97, the input file (UNIT) must be opened.  Upon return, the
file remains open.

The subroutine extracts label items from the first task of the label.
Ground-calibration labels are identified by the SCETY < 1998.  All other
labels are assumed to be flight.  

Only the ISS instruments are implemented.

If the mission is not CASSINI, the subroutine returns immediately with 
IND=-1.

Most missing label items will cause the subroutine to return immediately with
IND=-1.  

For structure definition used in zable97, see cas_isslab.h
For the Common Block CAS_ISSLAB for Fortran calls to able97, see cas_isslab.fin
For description and valid values of ISS VICAR labels, see the document
 "ISS VICAR Image Data File" located in http://eis/afs/jpl/home/c/cavis/public/SIS-DOIS-002.html

HISTORY
  Original Programmer: Charlie Avis 3/1/94
  Current Cognizant Programmer: Charlie Avis
  Source Language: Fortran
  Revisions:
   01 March 94  CCA   ...ABLE97 created as conversion of ABLE86
   05 April 95  CCA   ...modified label adding lossy compression parms and
                         missinglines and radiance
C    1-97  SP   Changed from a VMS Fortran STRUCTURE (previously third argument)
C               to a COMMON block for portability.  
C               Changed INTEGER*2 ... TO INTEGER ala VICAR PORTING GUIDE. 
C               Made portable for UNIX.  Added a separate C bridge (zable97)
C               that uses the original structure ala section 9.4.2 of the
C               Porting Guide.
C    2-99  JRY  Modified to include CASSINI-ISS2 label changes
C    11-99 TLT  Added IMAGETIME
C               Renamed LAB_PREPDUR to LAB_PREPCYCLE
C               Renamed LAB_READOUTDUR to LAB_READOUTCYCLE
C               Renamed READOUT_CYCLE_DURATION to READOUT_CYCLE_INDEX
C               Renamed PREPARE_CYCLE_DURATION to PREPARE_CYCLE_INDEX
C		Added SHUTTER_EVENT_TYPE
c		Changed REXPOS to LAB_EXPOS
C    5-00  AYS  Modified zable97.c sfor2c calls to output to a c string
C    8-00  AYS  Updated to accomodate new vicar property labels
C    10-00 AYS  Added IMAGE_OBSERVATION_TYPE and SEQUENCE_TITLE
C    4-03  VRH  Updated to accomodate tour labels



$ Return
$!#############################################################################
