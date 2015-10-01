$!****************************************************************************
$!
$! Build proc for MIPL module reallist
$! VPACK Version 1.9, Sunday, March 07, 2010, 12:02:36
$!
$! Execute by entering:		$ @reallist
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
$ write sys$output "*** module reallist ***"
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
$ write sys$output "Invalid argument given to reallist.com file -- ", primary
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
$   if F$SEARCH("reallist.imake") .nes. ""
$   then
$      vimake reallist
$      purge reallist.bld
$   else
$      if F$SEARCH("reallist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake reallist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @reallist.bld "STD"
$   else
$      @reallist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create reallist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack reallist.com -mixed -
	-s reallist.f -
	-p reallist.pdf -
	-i reallist.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create reallist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C     23 SEPT 93   ...REA...    INITIAL RELEASE
C     11 APRIL 02  ...REA...    add SB, NB parameters
C     16 APRIL 02  ...REA...    add DECIMAL parameter
C      7 MAY 03    ...REA...    PRECISE keyword added
C
	REAL*8 BUF(10)
	CHARACTER*140 PR, fstrng
	CHARACTER*3 ORG
	LOGICAL XVPTST
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBIN)
C							set width parameters
	IF (XVPTST('WIDE')) THEN
	    INCS = 10
	ELSE
	    INCS = 6
	END IF
C							set precision
	IF (XVPTST('PRECISE')) THEN
	    IWIDE = 20
	    INCS = 6
	ELSE
	    IWIDE = 11
	END IF
	CALL XVPARM('DECIMAL',IDEC,NUM,IDEF,0)
C						  	set ends of loops
	IEL = ISL+NL-1   
	IES = ISS+NS-1
	IEB = ISB+NB-1
C							get file organization
	CALL XVGET(INUNIT,ISTAT,'ORG',ORG,' ')
	IF (ORG .EQ. 'BIP') THEN
	    CALL XVMESSAGE('REALLIST does not support BIP format',' ')
	    CALL ABEND
	ELSE
	    DO IB=ISB,IEB
		CALL XVMESSAGE(' ',' ')
		IF (NBIN .NE. 1) THEN
		    WRITE (PR,400) IB
  400		    FORMAT('Band',I3)
		    CALL XVMESSAGE(PR,' ')
		END IF
C
		DO I=ISS,IES,INCS
		    IF (I .NE. ISS) CALL XVMESSAGE(' ',' ')
		    NSS = MIN(IES-I+1,INCS)
c		    WRITE (PR,500) (J,J=I,I+NSS-1)
c 500		    FORMAT('  Line',I<IWIDE>,9I<IWIDE+1>)
c  above is not std.Fortran, replace with:
		    fstrng = ' '
		    write(fstrng,500) iwide,iwide+1
500		    format('(''  Line'',I',i2,',9I',i2,')')
		    write(pr,fstrng) (j,j=i,i+nss-1)
		    CALL XVMESSAGE(PR,' ')
C							loop thru all lines	
		    DO J=ISL,IEL
			CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',J,'SAMP',I,
     +				'NSAMPS',NSS,'BAND',IB,' ')
c			WRITE (PR,600) J,(BUF(K),K=1,NSS)
c 600			FORMAT(I6,10(1X,F<IWIDE>.<IDEC>))
		        fstrng = ' '
		        write(fstrng,600) iwide,idec
600		        format('(I6,10(1X,F',i2,'.',i2,'))')
		        write(pr,fstrng) j,(buf(k),k=1,nss)
		 	CALL XVMESSAGE(PR,' ')
		    END DO
		END DO
	   END DO
	END IF
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create reallist.pdf
PROCESS HELP=*
PARM INP     TYPE=(STRING,64)
PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
PARM SL INTEGER DEFAULT=1
PARM SS INTEGER DEFAULT=1
PARM NL INTEGER DEFAULT=0
PARM NS INTEGER DEFAULT=0
PARM SB INTEGER DEFAULT=1
PARM NB INTEGER DEFAULT=0
PARM WIDE KEYWORD COUNT=0:1 DEFAULT=-- VALID=WIDE
PARM PRECISE KEYWORD COUNT=0:1 DEFAULT=-- VALID=PRECISE
PARM DECIMAL INTEGER DEFAULT=4 VALID=(1:17)

END-PROC
.TITLE
	Program REALLIST
.HELP
PURPOSE:
      REALLIST produces a listing of a REAL*4 VICAR dataset. It is a limited
version of list, with a different output format.  If possible, values are
printed in decimal notation. Exponential notation is used only when decimal
values will not fit within the columns.  In the default mode, the image is
printed in strips that are six pixels wide. If WIDE is specified the strips
are 10 pixels wide and require wide screens and paper.
.LEVEL1
.VARIABLE INP
input datasat
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines listed
.VARIABLE NS
Number of samples listed
.VARIABLE SB
Starting band
.VARIABLE NB
Number of bands listed
.VARIABLE WIDE
Use WIDE for wide print format
.VARIABLE PRECISE
Use PRECISE for wide columns
.VARIABLE DECIMAL
Number of decimal places
.LEVEL2
.VARIABLE INP
The input dataset.
.VARIABLE SIZE
Standard VICAR size field. 
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample listed
.VARIABLE NL
Number of lines listed
The default is to list all lines
.VARIABLE NS
Number of samples listed
The default is to list all samples
.VARIABLE SB
Starting band
.VARIABLE NB
Number of bands listed
The default is to list all bands
.VARIABLE WIDE
If WIDE is specified, the list is printed 10 pixels to a swath, requiring
wide paper printing or screen width=132 (or 216, if PRECISE is also used).
If defaulted, only 6 pixels are printed in a swath, and everything fits on 
normal screens or paper (as long as PRECISE is not specified, either).
.VARIABLE PRECISE
If PRECISE is specified, the pixel values are in columns 20 characters wide.
The default is columns of 11 characters.
.VARIABLE DECIMAL
DECIMAL indicates the number of decimal places to display in the listing
format. The range of valid values is 1 through 9. 4 is the default.
Number of decimal places
.END
$ Return
$!#############################################################################
$Imake_File:
$ create reallist.imake
#define  PROGRAM   reallist

#define MODULE_LIST reallist.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
