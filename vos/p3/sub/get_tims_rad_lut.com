$!****************************************************************************
$!
$! Build proc for MIPL module get_tims_rad_lut
$! VPACK Version 1.8, Thursday, October 08, 1998, 15:34:00
$!
$! Execute by entering:		$ @get_tims_rad_lut
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
$ write sys$output "*** module get_tims_rad_lut ***"
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
$ write sys$output "Invalid argument given to get_tims_rad_lut.com file -- ", primary
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
$   if F$SEARCH("get_tims_rad_lut.imake") .nes. ""
$   then
$      vimake get_tims_rad_lut
$      purge get_tims_rad_lut.bld
$   else
$      if F$SEARCH("get_tims_rad_lut.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake get_tims_rad_lut
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @get_tims_rad_lut.bld "STD"
$   else
$      @get_tims_rad_lut.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create get_tims_rad_lut.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack get_tims_rad_lut.com -
	-s get_tims_rad_lut.f -
	-i get_tims_rad_lut.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create get_tims_rad_lut.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE GET_TIMS_RAD_LUT(IDATE,SHIFT,RAD_LUT)
C
C	Modified for Y2K compliance 10/7/1998
C
	COMMON /RAWFIL/ FILTER,WVLEN,ICALDATE
C
	REAL FILTER(158,6),WVLEN(158)
	INTEGER*2 RAD_LUT(40000,6)
	CHARACTER*100 LUTNAME,P3INC
	CHARACTER*6 CALDATE
C
	CALL GETFIL(IDATE,0)
C
	IF (SHIFT .EQ. 0.0) THEN
C						find and read in the appropriate
C						temp to radiance lookup table
	    CALL XGETENV_VIC('P3INC',P3INC)
	    WRITE (CALDATE,'(I6)') ICALDATE
	    IF (ICALDATE .LT. 10) CALDATE(1:5) = '00000'
	    IF (ICALDATE .LT. 100) CALDATE(1:4) = '0000'
	    IF (ICALDATE .LT. 1000) CALDATE(1:3) = '000'
	    IF (ICALDATE .LT. 10000) CALDATE(1:2) = '00'
	    IF (ICALDATE .LT. 100000) CALDATE(1:1) = '0'
C    
	    LUTNAME = P3INC(1:LNBLNK(P3INC)) // '/tims_rad_lut.'
     +		      // CALDATE
	    CALL XVUNIT(LUTUNIT,'CALIB',1,ISTAT,'U_NAME',LUTNAME,' ')
	    CALL XVOPEN(LUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	    DO ICHAN=1,6
		CALL XVREAD(LUTUNIT,RAD_LUT(1,ICHAN),ISTAT,' ')
	    END DO
	ELSE
C						LUT must be built, since a
C						wvlen shift screws everything up
	    DO I=1,158
		WVLEN(I) = WVLEN(I) + SHIFT
	    END DO
C
	    CALL XVMESSAGE('Building new radiance LUT',' ')
	    DO ICHAN=1,6
		FILTER_NORM = 0.0
		DO I=1,158
		    FILTER_NORM = FILTER_NORM + FILTER(I,ICHAN)
		END DO
C								       build LUT
		DO ITEMP=1,40000
		    TEMP = FLOAT(ITEMP)/100.0
C						  compute radiance for this temp
		    RAD = 0.0
		    DO I=1,158
			RAD = RAD+FILTER(I,ICHAN)*PLANCK(WVLEN(I),TEMP)
		    END DO
		    RAD = 1000.0*RAD/FILTER_NORM
		    IRAD = MIN(NINT(RAD),32767)
		    RAD_LUT(ITEMP,ICHAN) = IRAD
		END DO
	    END DO
	END IF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create get_tims_rad_lut.imake
#define SUBROUTINE get_tims_rad_lut

#define MODULE_LIST get_tims_rad_lut.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
