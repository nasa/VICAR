$!****************************************************************************
$!
$! Build proc for MIPL module swapper
$! VPACK Version 1.8, Friday, February 04, 2000, 19:41:35
$!
$! Execute by entering:		$ @swapper
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
$ write sys$output "*** module swapper ***"
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
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to swapper.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
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
$   if F$SEARCH("swapper.imake") .nes. ""
$   then
$      vimake swapper
$      purge swapper.bld
$   else
$      if F$SEARCH("swapper.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake swapper
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @swapper.bld "STD"
$   else
$      @swapper.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create swapper.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack swapper.com -
	-s swapper.f -
	-p swapper.pdf -
	-i swapper.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create swapper.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'				      
	SUBROUTINE MAIN44
C	THIS PROGRAM CONVERTS IBM HALFWORD TO VAX 16 BIT WORD 
C	-JAM- 5/24/83
C       JHR: CONVERT TO VICAR2 24-JUNE-85
C	REA: EXTEND TO 4-BYTE FORMATS     17-AUG-98
C
	INTEGER*4 OUNIT,STAT,SL,SS,EL
	INTEGER*2 BUF(100000)
	CHARACTER*4 FORMAT
	CHARACTER*3 ORG
	BYTE BBUF(200000),HOLD
	EQUIVALENCE (BUF,BBUF)
C
        CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
        CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
        CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
        CALL XVOPEN(OUNIT,STAT,'OP','WRITE','OPEN_ACT','SA',
     +		    'IO_ACT','SA',' ')
C
        CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
	CALL XVBANDS(ISB,NB,NBI)
	CALL XLGET(IUNIT,'SYSTEM','ORG',ORG,STAT,'FORMAT','STRING',' ')
	CALL XLGET(IUNIT,'SYSTEM','FORMAT',FORMAT,STAT,
     +		   'FORMAT','STRING',' ')
	IF (FORMAT.EQ.'FULL' .OR. FORMAT.EQ.'REAL') THEN
	    NBPP = 4
	ELSE
	    NBPP = 2
	END IF
	EL = SL + NLO - 1
	IEB = ISB + NB - 1
	NBYTES = NBPP*NSO
C
	IF (ORG .EQ. 'BSQ') THEN
	  IF (NBPP .EQ. 2) THEN
	    DO IB=ISB,IEB
		DO IL=SL,EL
		    CALL XVREAD(IUNIT,BUF,STAT,'BAND',IB,'LINE',IL,
     +				'SAMP',SS,'NSAMPS',NSO,' ')
	    	    DO I=1,NBYTES,NBPP
			HOLD = BBUF(I)
			BBUF(I) = BBUF(I+1)
			BBUF(I+1) = HOLD
		    END DO
		    CALL XVWRIT(OUNIT,BUF,STAT,' ')
		END DO
	    END DO
	  ELSE
	    DO IB=ISB,IEB
		DO IL=SL,EL
		    CALL XVREAD(IUNIT,BUF,STAT,'BAND',IB,'LINE',IL,
     +				'SAMP',SS,'NSAMPS',NSO,' ')
	    	    DO I=1,NBYTES,NBPP
			HOLD = BBUF(I)
			BBUF(I) = BBUF(I+3)
			BBUF(I+3) = HOLD
			HOLD = BBUF(I+1)
			BBUF(I+1) = BBUF(I+2)
			BBUF(I+2) = HOLD
		    END DO
		    CALL XVWRIT(OUNIT,BUF,STAT,' ')
		END DO
	    END DO
	  END IF
	ELSE IF (ORG .EQ. 'BIL') THEN
	  IF (NBPP .EQ. 2) THEN
	    DO IL=SL,EL
		DO IB=ISB,IEB
		    CALL XVREAD(IUNIT,BUF,STAT,'BAND',IB,'LINE',IL,
     +				'SAMP',SS,'NSAMPS',NSO,' ')
		    DO I=1,NBYTES,NBPP
			HOLD = BBUF(I)
			BBUF(I) = BBUF(I+1)
			BBUF(I+1) = HOLD
		    END DO
		    CALL XVWRIT(OUNIT,BUF,STAT,' ')
		END DO
	    END DO
	  ELSE
	    DO IL=SL,EL
		DO IB=ISB,IEB
		    CALL XVREAD(IUNIT,BUF,STAT,'BAND',IB,'LINE',IL,
     +				'SAMP',SS,'NSAMPS',NSO,' ')
		    DO I=1,NBYTES,NBPP
			HOLD = BBUF(I)
			BBUF(I) = BBUF(I+3)
			BBUF(I+3) = HOLD
			HOLD = BBUF(I+1)
			BBUF(I+1) = BBUF(I+2)
			BBUF(I+2) = HOLD
		    END DO
		    CALL XVWRIT(OUNIT,BUF,STAT,' ')
		END DO
	    END DO
	  END IF
	ELSE
	    CALL XVMESSAGE(' Only BSQ and BIL orgs are supported',' ')
	    CALL ABEND
	END IF
C
	CALL XVCLOSE(IUNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create swapper.pdf
PROCESS HELP=*
PARM INP TYPE=(STRING,40)
PARM OUT TYPE=(STRING,40)
PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
END-PROC
.HELP
The byte order on PC's and VAX'es is opposite than the byte order on 
most unix platforms (including Sun and SGI).  This program swaps the
byte order of two or four byte data (HALF, FULL, or REAL). For BYTE
datasets no action is needed, and SWAPPER should not be used. The input
dataset may be in BSQ or BIL format, but processing for BIP format
datasets has not been implemented.
.LEVEL1
.VARIABLE INP
Input dataset
.VARIABLE OUT
Output byte-swapped dataset
.VARIABLE SIZE
VICAR Size field
.VARIABLE SL
Starting Line
.VARIABLE SS
Starting Sample
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of Samples
.LEVEL2
.VARIABLE INP
Input dataset
.VARIABLE OUT
Output byte-swapped dataset
.VARIABLE SIZE
VICAR Size field
.VARIABLE SL
Starting Line
.VARIABLE SS
Starting Sample
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of Samples
.END
$ Return
$!#############################################################################
$Imake_File:
$ create swapper.imake
#define  PROGRAM   swapper

#define MODULE_LIST swapper.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
