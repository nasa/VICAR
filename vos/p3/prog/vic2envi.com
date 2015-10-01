$!****************************************************************************
$!
$! Build proc for MIPL module vic2envi
$! VPACK Version 1.8, Monday, January 07, 2002, 16:26:17
$!
$! Execute by entering:		$ @vic2envi
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
$ write sys$output "*** module vic2envi ***"
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
$ write sys$output "Invalid argument given to vic2envi.com file -- ", primary
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
$   if F$SEARCH("vic2envi.imake") .nes. ""
$   then
$      vimake vic2envi
$      purge vic2envi.bld
$   else
$      if F$SEARCH("vic2envi.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vic2envi
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vic2envi.bld "STD"
$   else
$      @vic2envi.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vic2envi.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vic2envi.com -
	-s vic2envi.f -
	-i vic2envi.imake -
	-p vic2envi.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vic2envi.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C     initial release          6/14/95 Ron Alley
C     PC keyword added         2/13/98 Ron Alley
C
C**********************************************************************
      SUBROUTINE MAIN44
C
      CHARACTER*60 INFILE,OUTFILE
      CHARACTER*10 FORMAT,ORG,INTFMT
      CHARACTER*4 FMT(6)/'BYTE','HALF','FULL','REAL','DOUB','COMP'/
      LOGICAL XVPTST
      BYTE CR(2)/13,10/
C								open input
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVP('INP',INFILE,ICOUNT)
C							      get label contents
      CALL XLGET(INP,'SYSTEM','LBLSIZE',LBLSIZE,ISTAT,'FORMAT','INT',
     +		' ')
      CALL XLGET(INP,'SYSTEM','NL',NL,ISTAT,'FORMAT','INT',' ')
      CALL XLGET(INP,'SYSTEM','NS',NS,ISTAT,'FORMAT','INT',' ')
      CALL XLGET(INP,'SYSTEM','NB',NB,ISTAT,'FORMAT','INT',' ')
      CALL XLGET(INP,'SYSTEM','FORMAT',FORMAT,ISTAT,'FORMAT','STRING',
     +		' ')
      CALL XLGET(INP,'SYSTEM','ORG',ORG,ISTAT,'FORMAT','STRING',' ')
      CALL XLGET(INP,'SYSTEM','INTFMT',INTFMT,ISTAT,'FORMAT','STRING',
     +		' ')
C							determine data type flag
      IFMT = 0
      I = 1
      DO WHILE (IFMT.EQ.0 .AND. I.LE.6)
	  IF (FORMAT(1:4) .EQ. FMT(I)) IFMT = I
	  I = I+1
      END DO
      IF (IFMT .EQ. 0) IFMT=1
C							determine byte order flg
      IF (INTFMT .EQ. 'HIGH') THEN
	  INTFLAG = 1
      ELSE
	  INTFLAG = 0
      END IF
C							create header file name
      I = 1
      DO WHILE (INFILE(I:I) .NE. ' ')
	  I = I + 1
      END DO
      OUTFILE = INFILE(1:I-1) // '.hdr'
C							write out header file
      OPEN(71,FILE=OUTFILE)
      IF (XVPTST('PC')) THEN
          WRITE(71,200) CR,NS,CR,NL,CR,NB,CR,LBLSIZE,CR,CR,IFMT,CR,ORG,
     +                  CR,INTFLAG
      ELSE
          WRITE(71,100) NS,NL,NB,LBLSIZE,IFMT,ORG,INTFLAG
      END IF
      CLOSE(71)
  100 FORMAT('ENVI',/,'samples = ',I5,/,'lines = ',I5,/,'bands = ',I5,/,
     +       'header offset = ',I5,/,'file type = VICAR',/,
     +       'data type = ',I1,/,'interleave = ',A3,/,'byte order = ',
     +       I1)
  200 FORMAT('ENVI',2A1,'samples = ',I5,2A1,'lines = ',I5,2A1,
     +       'bands = ',I5,2A1,'header offset = ',I5,2A1,
     +       'file type = VICAR',2A1,'data type = ',I1,2A1,
     +       'interleave = ',A3,2A1,'byte order = ',I1)
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vic2envi.imake
#define  PROGRAM   vic2envi

#define MODULE_LIST vic2envi.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create vic2envi.pdf
process help=*
PARM INP   TYPE=(STRING,60)
PARM PC    TYPE=KEYWORD VALID="PC" COUNT=(0:1) DEFAULT=--
END-PROC
.TITLE
VIC2ENVI
.HELP
PURPOSE:
VIC2ENVI creates an ENVI header file (.hdr) for a VICAR labelled file.  The
input VICAR file will remain unaltered, and may be used for further 
processing under VICAR.  Upon completion of the program VIC2ENVI, the file
may also be accessed by ENVI, without the need for the user to supply image
size and format information.
 
Example:
	VIC2ENVI vicarfile

.LEVEL1
.VARIABLE INP
Input VICAR image file name
.VARIABLE PC
Use to go from unix to PC
platforms
.LEVEL2
.VARIABLE INP
This is the name of the input VICAR image file.  The contents of this file
are not changed by this program.  A separate ENVI header file, a file with
a ".hdr" extension is created as output.
.VARIABLE PC
The header (.hdr) file that gets created is an ASCII text file, and the
newline conventions differ on unix workstation and PC platforms. The PC
option creates a header file with correct newlines for use on a PC, even
when generated on a unix system.  If the ENVI use and the VICAR use are
on the same type of platform (either both unix or both PC), this keyword
is not needed.
.END
$ Return
$!#############################################################################
