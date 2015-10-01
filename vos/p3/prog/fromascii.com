$!****************************************************************************
$!
$! Build proc for MIPL module fromascii
$! VPACK Version 1.8, Wednesday, April 03, 2002, 18:41:10
$!
$! Execute by entering:		$ @fromascii
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
$ write sys$output "*** module fromascii ***"
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
$ write sys$output "Invalid argument given to fromascii.com file -- ", primary
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
$   if F$SEARCH("fromascii.imake") .nes. ""
$   then
$      vimake fromascii
$      purge fromascii.bld
$   else
$      if F$SEARCH("fromascii.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fromascii
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fromascii.bld "STD"
$   else
$      @fromascii.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fromascii.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fromascii.com -
	-s fromascii.f -
	-p fromascii.pdf -
	-i fromascii.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fromascii.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     APR 03 2002   ...rea... SLINE and NLINES parameters added
C
      REAL RBUF(150),ROUT(150)
      INTEGER ICOL(150),IBUF(150),IOUT(150)
      CHARACTER*50 PRINTFMT,PRT
      CHARACTER*40 INFILE
      CHARACTER*8 OFMT,UFMT
      LOGICAL QFORMAT
C                                                                    open input
      CALL XVPARM('ASCII',INFILE,ICNT,IDEF,0)
      OPEN (1,FILE=INFILE,STATUS='OLD')
C
      CALL XVPARM('DATA',OFMT,ICNT,IDEF,0)
      IF (OFMT .EQ. 'REAL') THEN
          UFMT = 'REAL'
      ELSE
          UFMT = 'FULL'
      END IF
      CALL XVPARM('NCOL',NCOL,ICNT,IDEF,0)
C                                                    get or set up COLUMNS array
      CALL XVPARM('COLUMNS',ICOL,NS,IDEF,0)
      IF (IDEF.EQ.1) THEN
          NS = NCOL
          DO I=1,NS
              ICOL(I) = I
          END DO
      END IF
C							 get START_LINE and skip
C								  unwanted lines
      CALL XVPARM('SLINE',ISL,ICNT,IDEF,0)
      IF (ISL .NE. 1) THEN
          DO I=1,ISL-1
              READ (1,*,END=500)
          END DO
      END IF
C								   get NUM_LINES
      CALL XVPARM('NLINES',NLOUT,ICNT,IDEF,0)
      IF (ICNT .EQ. 0) NLOUT = 99999999
C                                                               get print format
      CALL XVPARM('FORMAT',PRINTFMT,ICNT,IDEF,0)
      QFORMAT = (IDEF.EQ.0)
C                                           count out the number of output lines
      NL = 0
      IF (QFORMAT) THEN
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,PRINTFMT,END=500,ERR=100) (RBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  100             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,PRINTFMT,END=500,ERR=200) (IBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  200             CONTINUE
              END DO
          END IF
      ELSE
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,*,END=500,ERR=300) (RBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  300             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,*,END=500,ERR=400) (IBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  400             CONTINUE
              END DO
          END IF
      END IF
  500 CONTINUE
C
      IF (NL.LT.1) THEN
          CALL XVMESSAGE(' No records found that match the format',' ')
	  CALL ABEND
      ELSE
          WRITE (PRT,600) NL
  600     FORMAT(I10,' Records output')
          CALL XVMESSAGE(PRT,' ')
      END IF
      CLOSE(1)
      OPEN (1,FILE=INFILE,STATUS='OLD')
      IF (ISL .NE. 1) THEN
          DO I=1,ISL-1
              READ (1,*,END=500)
          END DO
      END IF
C                                                                  open output
      CALL XVUNIT(IOUTPUT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(IOUTPUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT',UFMT,'O_FORMAT',OFMT,'OP','WRITE',
     +            'U_NL',NL,'U_NS',NS,'U_NB',1,'U_ORG','BSQ',' ')
C                                                                  copy output
      NLOUT = NL
      NL = 0
      IF (QFORMAT) THEN
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,PRINTFMT,END=1500,ERR=1100) (RBUF(J),J=1,NCOL)
                  DO J=1,NS
                      ROUT(J) = RBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,ROUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
                  
 1100             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,PRINTFMT,END=1500,ERR=1200) (IBUF(J),J=1,NCOL)
                  DO J=1,NS
                      IOUT(J) = IBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,IOUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
 1200             CONTINUE
              END DO
          END IF
      ELSE
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,*,END=1500,ERR=1300) (RBUF(J),J=1,NCOL)
                  DO J=1,NS
                      ROUT(J) = RBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,ROUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
 1300             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,*,END=1500,ERR=1400) (IBUF(J),J=1,NCOL)
                  DO J=1,NS
                      IOUT(J) = IBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,IOUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
 1400             CONTINUE
              END DO
          END IF
      END IF
 1500 CONTINUE
C
      CLOSE(1)
      CALL XVCLOSE(IOUTPUT,ISTAT,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create fromascii.pdf
process help=*
  PARM ASCII      TYPE=(STRING,40)
  PARM OUT        TYPE=(STRING,40)
  PARM NCOL       INTEGER
  PARM SLINE      INTEGER DEFAULT=1
  PARM NLINES     INTEGER COUNT=0:1 DEFAULT=--
  PARM DATA       TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL) DEFAULT=REAL
  PARM FORMAT     TYPE=(STRING,80) DEFAULT=""
  PARM COLUMNS    INTEGER COUNT=0:150 DEFAULT=--
END-PROC
.help
      FROMASCII is a VICAR program to convert part or all of a tabular (ASCII)
file into a VICAR image.  The user may provide a FORTRAN format specifier, or
the data may be read as free format.  In either case, lines that do not fit
the form (such as headers, page counts, and trailers) are ignored.  The user 
may select and interchange up to 150 columns for output.
.level1
.vari ASCII
Input ASCII file name
.vari OUT
Output VICAR file name
.vari NCOL
Number of columns in input
.vari SLINE
First Line to be converted
.vari NLINES
Number of lines to be output
.vari COLUMNS
Columns (sample positions)
to be output.
DEFAULT: Output all columns
.VARI FORMAT
Input print format
.VARI DATA
Output data type
Valid: BYTE, HALF, FULL, REAL
.level2
.vari ASCII
Name of the tabular ASCII input file.
.vari out
Name of the VICAR output file.
.vari ncol
NCOL is used to indicate the number of columns in the input file.  If the user
provides a format for reading the input, this is the number of columns actually
read by the input format.  Any columns skipped, by use of the X format 
descriptor, are not counted.
.vari SLINE
SLINE indicates the first line to be converted into the output VICAR
file. For example, if SLINE=10, then the first 9 lines are discarded. 
The count of this parameter is of all lines; both valid data lines and
invalid lines (such as titles or lines of headings) are included in the
count.  The default is to start with the first line.
.vari NUM_LINES
NLINES indicates the desired number of lines to be output. Once this
number of lines has been written to output, no further reading of the 
input, or conversion to output is performed.  If the end of the output 
file is reached before NUM_LINES are converted, the program reports
and stores in the VICAR label the actual number of lines in the output
file.  The default is to read and convert all lines, until the end-of-file
is reached.
.vari columns
COLUMNS is a parameter that allows the user to select and order the columns to
be used as input.  For example, COLUMNS=(3,7,2,4) will cause the output to be
four samples wide, with the first sample coming from Column 3, the second from
Column 7, the third from Column 2, and the fourth from Column 4.  If COLUMNS is
defaulted, all columns are selected and output in their original order.
.vari format
The user may override the default print format by supplying a valid FORTRAN
FORMAT statement as the value of this parameter.  For example,
FORMAT="(14X,F5.1,6X,F5.1)" would, in the following example,  read the "Temp"
and "Wet Bulb" columns only.  The two header lines, since they do not fit the
given format, are discarded.  All other columns are ignored.

                  Weatherstation:  Tonopah, Nevada
      Time     Temp     Wet Bulb   Wind   Dir   Pressure
      8:00     57.5       33.2      9     NE     30.02
      9:00     59.8       33.7      5     NNE    30.02
     10:00     62.9       33.9      5     NE     30.01
     11:00     66.1       34.3      1     NE     29.99

Note that parentheses enclosing the full format expression are required, with
quotes enclosing the parentheses, and that the specifiers must be in agreement
with the DATA parameter.  If FORMAT is defaulted, the program assumes that
there are NCOL columns, separated by blanks, tabs, or commas.
.vari data
DATA is used to specify the data type of the output VICAR image.  BYTE, HALF,
FULL, and REAL are valid, with REAL being the default.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create fromascii.imake
#define  PROGRAM   fromascii

#define MODULE_LIST fromascii.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
