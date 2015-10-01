$!****************************************************************************
$!
$! Build proc for MIPL module envi2vic
$! VPACK Version 1.8, Thursday, March 01, 2001, 19:47:36
$!
$! Execute by entering:		$ @envi2vic
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
$ write sys$output "*** module envi2vic ***"
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
$ write sys$output "Invalid argument given to envi2vic.com file -- ", primary
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
$   if F$SEARCH("envi2vic.imake") .nes. ""
$   then
$      vimake envi2vic
$      purge envi2vic.bld
$   else
$      if F$SEARCH("envi2vic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake envi2vic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @envi2vic.bld "STD"
$   else
$      @envi2vic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create envi2vic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack envi2vic.com -
	-s envi2vic.f -
	-i envi2vic.imake -
	-p envi2vic.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create envi2vic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C
C	initial release		6/16/95 Ron Alley
C	add datatypes > 6       3/1/01  Ron Alley
C
C	This program creates a VICAR labelled file, using the size and format
C	info contained in an ENVI .hdr file.
C
	SUBROUTINE MAIN44
C
	PARAMETER (IBUFSIZE=1000000)
	BYTE BUFFER(IBUFSIZE)
	INTEGER NBPP(15)/1,2,4,4,8,8,99999,99999,16,4,4,2,4,8,8/
	CHARACTER*80 INLINE,KEY,VALUE
	CHARACTER*60 INFILE,HDRFILE
	CHARACTER*4 FORMAT
	CHARACTER*4 FMT(15)/'BYTE','HALF','FULL','REAL','DOUB','COMP',
     +			    '0007','0008','0009','FULL','FULL','HALF',
     +			    'FULL','0014','0015'/
	CHARACTER*3 ORG
C					get input and output file unit numbers
	CALL XVP('ENVI',INFILE,ICNT)
	I = 1
	DO WHILE (INFILE(I:I).NE.' ' .AND. I.LT.60)
	    I = I + 1
	END DO
	HDRFILE = INFILE(1:I-1) // '.hdr'
	CALL XVUNIT(INUNIT,'DUMMY',1,ISTAT,'U_NAME',INFILE,' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
C
C					get VICAR label info from ENVI .hdr file
	OPEN (71,FILE=HDRFILE)
	MASK = 0
	DO WHILE (MASK .NE. 63)
	    READ (71,100,END=900) INLINE
	    CALL UPRCASE(INLINE)
  100	    FORMAT(A)
	    CALL FINDKEY(INLINE,KEY,VALUE)
	    IF (KEY(1:7) .EQ. 'SAMPLES') THEN
		READ (VALUE,*) NS
		MASK = MASK + 1
	    ELSE IF (KEY(1:5) .EQ. 'LINES') THEN
		READ (VALUE,*) NL
		MASK = MASK + 2
	    ELSE IF (KEY(1:5) .EQ. 'BANDS') THEN
		READ (VALUE,*) NB
		MASK = MASK + 4
	    ELSE IF (KEY(1:12).EQ.'HEADEROFFSET') THEN
		READ (VALUE,*) LBLSIZE
		MASK = MASK + 8
	    ELSE IF (KEY(1:8).EQ.'DATATYPE') THEN
		READ (VALUE,*) ITYPE
		FORMAT = FMT(ITYPE)
		NBYTES = NBPP(ITYPE)
		MASK = MASK + 16
	    ELSE IF (KEY(1:10) .EQ. 'INTERLEAVE') THEN
		ORG = VALUE(1:3)
		CALL UPRCASE(ORG)
		MASK = MASK + 32
	    END IF
	END DO
C						check for unsupported data types
	IF ((ITYPE.GE.7 .AND. ITYPE.LE.9) .OR. ITYPE.GT.13) THEN
	    CALL XVMESSAGE('VICAR does not support this ENVI DATATYPE',
     +						' ')
	    CALL ABEND
	ELSE IF (ITYPE .EQ. 12) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('WARNING:',' ')
	    CALL XVMESSAGE(
     +		'VICAR does not support unsigned 16-bit integer.',' ')
	    CALL XVMESSAGE('Pixel values greater than 32767 will be',' ')
	    CALL XVMESSAGE(
     +		'erroneously represented as negative numbers.',' ')
	    CALL XVMESSAGE(' ',' ')
	ELSE IF (ITYPE .EQ. 13) THEN
	    CALL XVMESSAGE('WARNING:',' ')
	    CALL XVMESSAGE(
     +		'VICAR does not support unsigned 32-bit integer.',' ')
	    CALL XVMESSAGE(
     +		'Pixel values greater than 2,147,483,647 will',' ')
	    CALL XVMESSAGE(
     +		'be erroneously represented as negative numbers.',' ')
	    CALL XVMESSAGE(' ',' ')
	END IF
C
	NTOT = NBYTES * NL * NS * NB
	IF (LBLSIZE .EQ. 0) THEN
	    IRECLEN = NBYTES * NS
	ELSE
	    IRECLEN = LBLSIZE
	END IF
	IF (ORG .EQ. 'BIP') THEN
	    IOUTRECL = NBYTES * NB
	ELSE
	    IOUTRECL = NBYTES * NS
	END IF
C								open datasets
	CALL XVOPEN(INUNIT,ISTAT,'U_NS',IRECLEN,'COND','NOLABELS',
     +		    'OPEN_ACT','SA',' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_ORG',ORG,'U_NL',NL,
     +		    'U_NS',NS,'U_NB',NB,'U_FORMAT',FORMAT,
     +		    'O_FORMAT',FORMAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C								copy the data
C
	IF (LBLSIZE .NE. 0) CALL XVREAD(INUNIT,BUFFER,ISTAT,' ')
	LOC = 1
	ILEN = IBUFSIZE
	NTOGO = NTOT
	DO WHILE (NTOGO .GT. 0)
	    CALL READM(INUNIT,BUFFER(LOC),ILEN,NTOGO,IRECLEN)
	    ILEN = ILEN + LOC - 1
	    CALL WRITM(IOUTUNIT,BUFFER,ILEN,IOUTRECL)
	    LOC = ILEN + 1
	    ILEN = IBUFSIZE - ILEN
	END DO
C
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVCLOSE(IOUTUNIT,ISTAT,' ')
	RETURN
C
  900	CONTINUE
	CALL XVMESSAGE('Unable to read ENVI .hdr file',' ')
	CALL XVMESSAGE('No output written',' ')
	RETURN
	END
C******************************************************************************
	SUBROUTINE FINDKEY(INLINE,KEY,VALUE)
C
	CHARACTER*80 INLINE,KEY,VALUE
C
	DO I=1,80
	    KEY(I:I) = ' '
	    VALUE(I:I) = ' '
	END DO
C
	I = 1
	J = 1
	DO WHILE (I.LE.80 .AND. INLINE(I:I).NE.'=')
	    IF (INLINE(I:I) .NE. ' ') THEN
		KEY(J:J) = INLINE(I:I)
		J = J + 1
	    END IF
	    I = I + 1
	END DO
C
	I = I + 1
	J = 1
	DO WHILE (I .LE. 80)
	    IF (INLINE(I:I) .NE. ' ') THEN
		VALUE(J:J) = INLINE(I:I)
		J = J + 1
	    END IF
	    I = I + 1
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE READM(INUNIT,BUFFER,ILEN,NTOGO,IRECLEN)
C
	BYTE BUFFER(ILEN)
C
	LOC = 1
	IF (ILEN .GE. NTOGO) THEN
	    DO WHILE (NTOGO .GT. 0)
		IF (NTOGO .GE. IRECLEN) THEN
		    CALL XVREAD(INUNIT,BUFFER(LOC),ISTAT,' ')
		    LOC = LOC + IRECLEN
		    NTOGO = NTOGO - IRECLEN
		ELSE
		    CALL XVREAD(INUNIT,BUFFER(LOC),ISTAT,
     +				'NSAMPS',NTOGO,' ')
		    LOC = LOC + NTOGO
		    NTOGO = 0
		END IF
	    END DO
	ELSE
	    DO WHILE (LOC + IRECLEN -1 .LE. ILEN)
		CALL XVREAD(INUNIT,BUFFER(LOC),ISTAT,' ')
		LOC = LOC + IRECLEN
		NTOGO = NTOGO - IRECLEN
	    END DO
	    ILEN = LOC - 1
	END IF
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE WRITM(IOUTUNIT,BUFFER,ILEN,IRECLEN)
C
	BYTE BUFFER(ILEN)
C
	LOC = 1
	DO WHILE (LOC + IRECLEN - 1 .LE. ILEN)
	    CALL XVWRIT(IOUTUNIT,BUFFER(LOC),ISTAT,' ')
	    LOC = LOC + IRECLEN
	END DO
C
	ILEN = ILEN - LOC + 1
	IF (ILEN .GT. 0) CALL MVE(1,ILEN,BUFFER(LOC),BUFFER(1),1,1)
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create envi2vic.imake
#define  PROGRAM   envi2vic

#define MODULE_LIST envi2vic.f

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
$ create envi2vic.pdf
process help=*
PARM ENVI  TYPE=(STRING,60)
PARM OUT   TYPE=(STRING,60)
END-PROC
.TITLE
ENVI2VIC
.HELP
PURPOSE:
ENVI2VIC takes as input an ENVI image file (that is, a file that has an
associated ".hdr" ENVI header file) and creates as output a VICAR labelled
file.
 
Example 
	ENVI2VIC envifile vicarfile

	There are no optional parameters, and only the ENVI image file
	(not its header file) should be given as input.
 
.LEVEL1
.VARIABLE ENVI
Input ENVI image file name
.VARIABLE OUT
Output VICAR labelled file
.LEVEL2
.VARIABLE ENVI
This is the name of the input ENVI image file.  It must have associated with
it an ENVI header file.  That is, if you wish to convert the file "myfile" to
vicar, there must also be a "myfile.hdr" in the same directory.
.VARIABLE OUT
This is the name of the VICAR labelled file that will be produced as output.
.END
$ Return
$!#############################################################################
