$!****************************************************************************
$!
$! Build proc for MIPL module to3d
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:47
$!
$! Execute by entering:		$ @to3d
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
$ write sys$output "*** module to3d ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("to3d.imake") .nes. ""
$   then
$      vimake to3d
$      purge to3d.bld
$   else
$      if F$SEARCH("to3d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake to3d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @to3d.bld "STD"
$   else
$      @to3d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create to3d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack to3d.com -
	-s to3d.f -
	-p to3d.pdf -
	-i to3d.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create to3d.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	INTEGER INST(200)
	CHARACTER*10 TASK(200),FORMAT
	CHARACTER*3 ORG
C							   open dataset (update)
	CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(IUNIT,ISTAT,'OP','UPDATE','OPEN_ACT','SA',
     +		    'IO_ACT','SA',' ')
C
	NHIST = 200
	CALL XLHINFO(IUNIT,TASK,INST,NHIST,ISTAT,' ')
	DO WHILE (NHIST.GT.0)
	    CALL XLINFO(IUNIT,'HISTORY','3D_NL',FORMAT,LEN,NUM,ISTAT,
     +			'HIST',TASK(NHIST),'INSTANCE',INST(NHIST),' ')
	    IF (ISTAT.NE.1) THEN
		NHIST = NHIST-1
	    ELSE
		CALL XLGET(IUNIT,'HISTORY','3D_NL',NL,ISTAT,
     +			   'FORMAT','INT','ERR_ACT','SA','INSTANCE',
     +			   INST(NHIST),'HIST',TASK(NHIST),' ')
		CALL XLGET(IUNIT,'HISTORY','3D_NS',NS,ISTAT,
     +			   'FORMAT','INT','ERR_ACT','SA','INSTANCE',
     +			   INST(NHIST),'HIST',TASK(NHIST),' ')
		CALL XLGET(IUNIT,'HISTORY','3D_NB',NB,ISTAT,
     +			   'FORMAT','INT','ERR_ACT','SA','INSTANCE',
     +			   INST(NHIST),'HIST',TASK(NHIST),' ')
		CALL XLGET(IUNIT,'HISTORY','3D_ORG',ORG,ISTAT,
     +			   'FORMAT','STRING','ERR_ACT','SA','INSTANCE',
     +			   INST(NHIST),'HIST',TASK(NHIST),' ')
C
		CALL XVGET(IUNUT,ISTAT,'NL',NLX,'NS',NSX,' ')
		IF (NL*NS*NB .NE. NLX*NSX) THEN
		    CALL XVMESSAGE(
     +		    ' Conflict in image dimensions; nothing changed',' ')
		    CALL ABEND
		ENDIF
C							       delete old values
		CALL XLDEL(IUNIT,'HISTORY','3D_NL',ISTAT,
     +			   'ERR_ACT','SA','INSTANCE',INST(NHIST),
     +			   'HIST',TASK(NHIST),' ')
		CALL XLDEL(IUNIT,'HISTORY','3D_NS',ISTAT,
     +			   'ERR_ACT','SA','INSTANCE',INST(NHIST),
     +			   'HIST',TASK(NHIST),' ')
		CALL XLDEL(IUNIT,'HISTORY','3D_NB',ISTAT,
     +			   'ERR_ACT','SA','INSTANCE',INST(NHIST),
     +			   'HIST',TASK(NHIST),' ')
		CALL XLDEL(IUNIT,'HISTORY','3D_ORG',ISTAT,
     +			   'ERR_ACT','SA','INSTANCE',INST(NHIST),
     +			   'HIST',TASK(NHIST),' ')
		CALL XLDEL(IUNIT,'SYSTEM','NL',ISTAT,'ERR_ACT','SA',' ')
		CALL XLDEL(IUNIT,'SYSTEM','NS',ISTAT,'ERR_ACT','SA',' ')
		CALL XLDEL(IUNIT,'SYSTEM','NB',ISTAT,'ERR_ACT','SA',' ')
		CALL XLDEL(IUNIT,'SYSTEM','ORG',ISTAT,'ERR_ACT','SA',
     +			   ' ')
C								  add new values
		CALL XLADD(IUNIT,'SYSTEM','NL',NL,ISTAT,'FORMAT','INT',
     +			   ' ')
		CALL XLADD(IUNIT,'SYSTEM','NS',NS,ISTAT,'FORMAT','INT',
     +			   ' ')
		CALL XLADD(IUNIT,'SYSTEM','NB',NB,ISTAT,'FORMAT','INT',
     +			   ' ')
		CALL XLADD(IUNIT,'SYSTEM','ORG',ORG,ISTAT,
     +			   'FORMAT','STRING',' ')
		NHIST = -1
	    END IF
	END DO
	IF (NHIST.EQ.0) THEN
	    CALL XVMESSAGE(' 3-D dimensions not found.',' ')
	    CALL ABEND
	ENDIF
C
	CALL XVCLOSE(IUNIT,ISTAT,' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create to3d.pdf
PROCESS HELP=*
PARM INP     TYPE=STRING
END-PROC
.TITLE
TO3D
.HELP
     TO3D is a companion program to TO2D, and will only work on datasats
modified by TO2D. 
     TO2D alters the VICAR label of a 3-D dataset, to make it appear to be
a 2-D (1 band, BSQ) dataset. This program (TO3D) restores the VICAR label to
its original 3-D form.
.LEVEL1
.VARIABLE INP
Dataset to be updated
.LEVEL2
.VARIABLE INP
The dataset to be updated; there is no output dataset.
$ Return
$!#############################################################################
$Imake_File:
$ create to3d.imake
#define  PROGRAM   to3d

#define MODULE_LIST to3d.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
