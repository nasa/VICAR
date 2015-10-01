$!****************************************************************************
$!
$! Build proc for MIPL module toascii
$! VPACK Version 1.8, Tuesday, May 08, 2001, 15:26:47
$!
$! Execute by entering:		$ @toascii
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
$ write sys$output "*** module toascii ***"
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
$ write sys$output "Invalid argument given to toascii.com file -- ", primary
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
$   if F$SEARCH("toascii.imake") .nes. ""
$   then
$      vimake toascii
$      purge toascii.bld
$   else
$      if F$SEARCH("toascii.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake toascii
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @toascii.bld "STD"
$   else
$      @toascii.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create toascii.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack toascii.com -
	-s toascii.f -
	-p toascii.pdf -
	-i toascii.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create toascii.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     May 7 2001   rea    Support double precision input
C
      REAL*8 RBUF(10000)
      INTEGER ICOL(1000),IBUF(10000)
      CHARACTER*50 PRINTFMT
      CHARACTER*40 OUTFILE
      CHARACTER*8 DATAFMT,UFMT
      CHARACTER*3 ORG
C
      CALL XVMESSAGE('TOASCII Version May 7, 2001',' ')
C                                                          open input, get size
      CALL XVUNIT(INPUT,'INP',1,ISTAT,' ')
      CALL XVOPEN(INPUT,ISTAT,'OPEN_ACT','SA',' ')
      CALL XVGET(INPUT,ISTAT,'ORG',ORG,'FORMAT',DATAFMT,' ')
      IF (DATAFMT.EQ.'REAL' .OR. DATAFMT.EQ.'DOUB') THEN
          UFMT = 'DOUB'
      ELSE
          UFMT = 'FULL'
      END IF
      CALL XVCLOSE(INPUT,ISTAT,' ')
      CALL XVOPEN(INPUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT',UFMT,' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      CALL XVBANDS(ISB,NB,NBIN)
C                                                    get or set up COLUMNS array
      CALL XVPARM('COLUMNS',ICOL,NCOL,IDEF,0)
      IF (IDEF.EQ.1) THEN
          IF (ORG.NE.'BIP') THEN
              DO I=1,NS
                  ICOL(I) = ISS+I-1
              END DO
              NCOL = NS
          ELSE
              DO I=1,NB
                  ICOL(I) = ISB+I-1
              END DO
              NCOL = NB
          END IF
      END IF
C                                                     get or set up print format
      CALL XVPARM('FORMAT',PRINTFMT,ICNT,IDEF,0)
      IF (IDEF.EQ.1) THEN
          IF (DATAFMT.EQ.'BYTE') THEN
              WRITE (PRINTFMT,100) NCOL
  100         FORMAT( '(', I4, 'I4)' )
          ELSE IF (DATAFMT.EQ.'HALF') THEN
              WRITE (PRINTFMT,110) NCOL
  110         FORMAT( '(', I4, 'I7)' )
          ELSE IF (DATAFMT.EQ.'FULL') THEN
              WRITE (PRINTFMT,120) NCOL
  120         FORMAT( '(', I4, 'I13)' )
          ELSE
              WRITE (PRINTFMT,130) NCOL
  130         FORMAT( '(', I4, 'G14.6)' )
          END IF
      END IF
C                                                                    open output
      CALL XVPARM('OUT',OUTFILE,ICNT,IDEF,0)
      OPEN (1,FILE=OUTFILE,STATUS='UNKNOWN')
C                                                             single band output
      IF (NB .EQ. 1) THEN
          IF (UFMT.NE.'DOUB') THEN
              DO I=ISL,ISL+NL-1
                  CALL XVREAD(INPUT,IBUF,ISTAT,'BAND',ISB,'LINE',I,' ')
                  WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
              END DO
          ELSE
              DO I=ISL,ISL+NL-1
                  CALL XVREAD(INPUT,RBUF,ISTAT,'BAND',ISB,'LINE',I,' ')
                  WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
              END DO
          END IF
C                                                                        BSQ
      ELSE IF (ORG .EQ. 'BSQ') THEN
          IF (UFMT.NE.'DOUB') THEN
              DO J=ISB,ISB+NB-1
                  WRITE (1,200) J
  200             FORMAT(/,' Band',I3)
                  DO I=ISL,ISL+NL-1
                      CALL XVREAD(INPUT,IBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          ELSE
              DO J=ISB,ISB+NB-1
                  WRITE (1,200) J
                  DO I=ISL,ISL+NL-1
                      CALL XVREAD(INPUT,RBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          END IF
C                                                                        BIL
      ELSE IF (ORG .EQ. 'BIL') THEN
          IF (UFMT.NE.'DOUB') THEN
              DO I=ISL,ISL+NL-1
                  WRITE (1,300) I
  300             FORMAT(/,' Line',I5)
                  DO J=ISB,ISB+NB-1
                      CALL XVREAD(INPUT,IBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          ELSE
              DO I=ISL,ISL+NL-1
                  WRITE (1,300) I
                  DO J=ISB,ISB+NB-1
                      CALL XVREAD(INPUT,RBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          END IF
C                                                                        BIP
      ELSE
          IF (UFMT.NE.'DOUB') THEN
              DO J=ISL,ISL+NL-1
                  WRITE (1,300) J
                  DO I=ISS,ISS+NS-1
                      CALL XVREAD(INPUT,IBUF,ISTAT,'LINE',J,'SAMP',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          ELSE
              DO J=ISL,ISL+NL-1
                  WRITE (1,300) J
                  DO I=ISS,ISS+NS-1
                      CALL XVREAD(INPUT,RBUF,ISTAT,'LINE',J,'SAMP',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          END IF
      END IF
C
      CLOSE(1)
      CALL XVCLOSE(INPUT,ISTAT,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create toascii.pdf
process help=*
  PARM INP  TYPE=(STRING,40)
  PARM OUT  TYPE=(STRING,40)
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM BANDS TYPE=INTEGER COUNT=0:2 DEFAULT=--
  PARM SL INTEGER DEFAULT=1
  PARM SS INTEGER DEFAULT=1
  PARM SB INTEGER DEFAULT=1
  PARM NL INTEGER DEFAULT=0
  PARM NS INTEGER DEFAULT=0
  PARM NB INTEGER DEFAULT=0
  PARM COLUMNS INTEGER COUNT=0:50 DEFAULT=--
  PARM FORMAT TYPE=(STRING,80) DEFAULT=""
END-PROC
.help
      TOASCII is a VICAR program to convert part or all of a VICAR image file
into an ASCII table of values.  It is somewhat similar to the program LIST, but
has the following differences:
           1. Page headers, row headers, and column headers are not printed.
              The only headers printed are, in the case of multiple output
              bands, the band numbers for BSQ images and the line numbers for
              BIL or BIP images.
           2. The output is not split into strips, even if there are many 
              columns of output.  That is, TOASCII assumes an arbitrarily large
              screen or paper.
           3. The columns of pixels output need not be consecutive or even in
              order.  For example, the user may output samples 3, 7, 1, and 6
              - in that order.
           4. The user may provide a FORTRAN format statement to descibe the
              output format.
.level1
.vari inp
Input VICAR file name
.vari out
Output ASCII file name
.vari size
Window into input
.vari bands
Window into input
in band dimension
.vari sl
Starting line
.vari ss
Starting sample
.vari sb
Starting band
.vari nl
Number of lines
.vari ns
Number of samples
.vari nb
Number of bands
.vari COLUMNS
Columns (sample positions)
to be output.
DEFAULT: Uses size field
.VARI FORMAT
Output print format
.level2
.vari inp
Name of the VICAR input file.
.vari out
Name of the ASCII output file.  This file will have no VICAR labels.
.vari size
The size parameter determines the portion of the input file to be converted
to ASCII data and output.  It is specified as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample (column)
	NL is the number of lines to be output
	NS is the number of samples (pixels) in each line
.vari bands
The bands parameter determines the bands in the input file to be converted to
ASCII data and output.  It is specified as (SB,NB), where
	SB is the starting band
	NB is the number of bands to be output
.vari format
The user may override the default print format by supplying a valid FORTRAN
FORMAT statement as the value of this parameter.  For example,
FORMAT="(9X,I5,3X,I7)" would yield columns as follows:

         #####   #######
         #####   #######
         #####   #######
         #####   #######
         #####   #######
         #####   #######
Note that parentheses enclosing the full format expression are required, with
quotes enclosing the parentheses.
.vari columns
COLUMNS is an alternate method (to the SIZE field) of describing which sample 
positions are to be output.  It is an explicit list of the sample positions to
be output.  For example, COLUMNS=(4,5,6) would yield the same result as SS=4,
NS=3.  The output columns need not be consecutive or sequential;
COLUMNS=(1,9,4) will output Sample 1 as Column 1, Sample 9 as Column 2, and
Sample 4 as Column 3.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create toascii.imake
#define  PROGRAM   toascii

#define MODULE_LIST toascii.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
