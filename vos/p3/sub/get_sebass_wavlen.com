$!****************************************************************************
$!
$! Build proc for MIPL module get_sebass_wavlen
$! VPACK Version 1.8, Thursday, March 12, 1998, 16:43:21
$!
$! Execute by entering:		$ @get_sebass_wavlen
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
$ write sys$output "*** module get_sebass_wavlen ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to get_sebass_wavlen.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("get_sebass_wavlen.imake") .nes. ""
$   then
$      vimake get_sebass_wavlen
$      purge get_sebass_wavlen.bld
$   else
$      if F$SEARCH("get_sebass_wavlen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake get_sebass_wavlen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @get_sebass_wavlen.bld "STD"
$   else
$      @get_sebass_wavlen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create get_sebass_wavlen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack get_sebass_wavlen.com -
	-s get_sebass_wavlen.f -
	-i get_sebass_wavlen.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create get_sebass_wavlen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE GET_SEBASS_WAVLEN(IDATE,WAVLEN)
C
C	This routine accepts a date, IDATE, (in yyyymmdd format), finds
C	the appropriate SEBASS calibration file, then loads the central
C	wavelengths of the 128 channels into the WAVLEN array.
C
	REAL WAVLEN(128)
	CHARACTER*100 INDEXNAME,P3INC,CALTABNAME
	CHARACTER*80 MSG
C								open index file
	CALL XGETENV_VIC('P3INC',P3INC)
	INDEXNAME = P3INC(1:LNBLNK(P3INC)) // '/sebass.index'
	OPEN (85,FILE=INDEXNAME,ERR=900,STATUS='OLD')
C							       search index file
C							  for proper calibration
	READ (85,*) JDATE
	DO WHILE (IDATE .GT. JDATE)
	    IDATE_OF_CAL = JDATE
	    READ (85,*,END=100) JDATE
	END DO
  100	CONTINUE
	CLOSE(85)
	WRITE (MSG,200) IDATE_OF_CAL
  200	FORMAT(I10,' SEBASS calibration file being used.')
	CALL XVMESSAGE(MSG,' ')
C							   open calibration file
	CALTABNAME = P3INC(1:LNBLNK(P3INC)) // '/sebass.' // MSG(3:10)
	OPEN (85,FILE=CALTABNAME,ERR=920,STATUS='OLD')

	CALL XVUNIT(IUNIT,'CALIB',1,ISTAT,'U_NAME',LUTNAME,' ')
C							   read calibration file
	DO I=1,128
	    READ (85,*,ERR=950) ICHAN,WAVLEN(I)
	END DO
	CLOSE(85)
	RETURN
C								error conditions
  900	CALL XVMESSAGE('SEBASS index file not found',' ')
	CALL ABEND
  920   CALL XVMESSAGE('SEBASS calibration file not found',' ')
	CALL ABEND
  950   CALL XVMESSAGE('SEBASS calibration file read error',' ')
	CALL ABEND
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create get_sebass_wavlen.imake
#define SUBROUTINE get_sebass_wavlen

#define MODULE_LIST get_sebass_wavlen.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
