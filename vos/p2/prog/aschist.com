$!****************************************************************************
$!
$! Build proc for MIPL module aschist
$! VPACK Version 1.9, Monday, December 07, 2009, 16:00:21
$!
$! Execute by entering:		$ @aschist
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
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module aschist ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to aschist.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("aschist.imake") .nes. ""
$   then
$      vimake aschist
$      purge aschist.bld
$   else
$      if F$SEARCH("aschist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake aschist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @aschist.bld "STD"
$   else
$      @aschist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create aschist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack aschist.com -mixed -
	-s aschist.f -
	-i aschist.imake -
	-p aschist.pdf -
	-t tstaschist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create aschist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   REVISION HISTORY
C      3-97   SP  Made portable for UNIX.  Removed non-portable carriage-control
C                 modifier in FORTRAN OPEN statement.  Added "1X" in FORMAT 
C                 statements to compensate.  Changed PSCL from I*2 to I*4
C                 to work on all machines with XLGET.  Corresponding changes
C                 for DCODE parameter for PRNT and DIVV.  Added FORMAT INT
C                 for xlget.

	subroutine main44
	implicit integer (A-Z)
	INTEGER*4 HBUF(32768),RNG(2),INSTANCES(20),PSCL
	INTEGER*2 INBUF(60000)
        CHARACTER*1 TAB
	CHARACTER*5 FORM
        CHARACTER*80 TABLE_DS
	CHARACTER*8 TASKS(20)
	LOGICAL XVPTST
C
	CALL XVUNIT(INU,'INP',1,IST,' ')
	CALL XVOPEN(INU,IST,'U_FORMAT','HALF','IO_ACT', 'SA' ,' ')
	IF (IST .NE. 1) GO TO 998
	CALL XVGET(INU,IST,'FORMAT',FORM,'NS',NS,'NL',NL,' ')
	IF (FORM .NE. 'HALF' .AND. FORM .NE. 'BYTE') THEN
		CALL XVMESSAGE('ONLY BYTE AND HALF FORMATS HANDLED',' ')
		CALL ABEND
	END IF
C
C-------GET PARAMETERS
	CALL XVPARM('RANGE',RNG,ICNT,IDEF,2)
	IF (ICNT .EQ. 2) GO TO 5
	IF (FORM .EQ. 'BYTE') THEN
		RNG(1) = 0
		RNG(2) = 255
	ELSE 
		RNG(1) = 0
		RNG(2) = 4096
	END IF

5	CONTINUE
	LOW = MAX(0,RNG(1))
	HIGH = MIN(32767,RNG(2))
C
c-------Was PICSUM run?  If so, get scale (number of images used) so it
c-------can be used in divison below.
C-------Search for last value of picture scale in the label.
        ICNT = 20
        CALL XLHINFO(INU,TASKS,INSTANCES,ICNT,IND,' ')
        DO J=ICNT,1,-1		
           CALL XLGET(INU,'HISTORY','PICSCALE',PSCL,IND,'FORMAT','INT',
     &            'HIST',TASKS(J),'INSTANCE',INSTANCES(J),' ')
           IF (IND.EQ.1) GOTO 7
        ENDDO
        PSCL = 1		!Default picture scale
7	CONTINUE
	CALL PRNT(4,1,PSCL,'IMAGES SUMMED=.')
C
C-------Could make my own loop for dividing and histograming but
c-------might be faster to call the two assembler routines (div & hsub)
C
C-------ACCUMULATE HISTOGRAM
	DO I=1,NL
	   CALL XVREAD(INU,INBUF,IST,' ')
	   IF (PSCL .GT. 1) CALL DIVV(-6,NS,PSCL,INBUF,0,1)
	   CALL HSUB(2,NS,INBUF,HBUF,LOW,HIGH)
	END DO
	CALL XVCLOSE(INU,IST,' ')
C
C-------WRITE OUTPUT FILE 
C-------ASCII TEXT WITH COLUMNS: DN and POPULATION
	CALL XVPARM('TABLE',TABLE_DS,ICNT,IDEF,1) 
	OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',
     1       IOSTAT=JST,ERR=999)
	TAB= CHAR(9)

C-------Write header and table, leave out the DN column
	IF (XVPTST('NODN')) THEN
	  IF (.NOT.XVPTST('NOHEADER'))	
     1         WRITE(12,8) 'POPULATION',TAB

	  DO I=LOW,HIGH
		WRITE(12,11) HBUF(I-LOW+1),TAB
	  END DO

	ELSE

C-------Write header and table with DN column
	  IF (.NOT.XVPTST('NOHEADER'))	
     1         WRITE(12,6) 'DN',TAB,'POPULATION',TAB

	  DO I=LOW,HIGH
		WRITE(12,10) I,TAB,HBUF(I-LOW+1),TAB
	  END DO

	END IF


	CLOSE(12)
	RETURN
C.................ERROR HANDLING
998	CALL XVMESSAGE('ERROR OPENING INPUT FILE',' ')
	CALL ABEND
999	CALL XVMESSAGE('ERROR OPENING OUTPUT FILE',' ')
	CALL PRNT(4,1,JST,'IOSTAT.')
	CALL ABEND
C
8	FORMAT(1X,A10,A1)
6	FORMAT(1X,A2,A1,A10,A1)
10	FORMAT(1X,I5,A1,I7,A1)
11	FORMAT(1X,I7,A1)
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create aschist.imake
#define  PROGRAM   aschist

#define MODULE_LIST aschist.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create aschist.pdf
PROCESS HELP=*
PARM INP STRING
PARM TABLE STRING
PARM RANGE INTEGER COUNT=(0,2) DEFAULT=--
PARM MODE  KEYWORD COUNT=0:1 VALID=(NOHEADER,HEADER) DEFAULT=HEADER
PARM DNCOL KEYWORD COUNT=0:1 VALID=(NODN,DN) DEFAULT=DN

!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=(ASCII,tabular,histogram,DN)
END-PROC
.TITLE
Create a tab-delimited ASCII histogram file
.HELP
PURPOSE:
ASCHIST will create a tabular histogram file from byte or halfword data.

.PAGE
EXECUTION:

	ASCHIST in table user-parameters
.PAGE
OPERATION:
ASCHIST collects a histogram of the input file and outputs it as a
tab-delimitted ASCII file of two columns.  The first column is the DN
value, and the second is the population of that level.  Column headings
are optional.  If the input file's VICAR label contains the PICSCALE 
keyword (i.e., the image has been PICSUM'd), input image will be divided 
by the PICSCALE value (the number of images summed) prior to the generation
of the histogram. 

RESTRICTIONS:
The input image must not exceed 60000 samples per line.

.PAGE
ORIGINAL PROGRAMMER: Charlie Avis, 8/15/94
CURRENT COGNIZANT PROGRAMMER: charlie avis
REVISION HISTORY:

      15 Aug 94 ...cca... initial release
      23 Jan 95 ...cca... make DN column optional
      10 Mar 97 ...sp.... Made portable for UNIX.  
                          Increased max number of samples to 60000.
.LEVEL1
.VARIABLE INP
The input VICAR image
.VARIABLE TABLE
The output histogram
table file.
.VARIABLE RANGE
The range of DN values
to be included in the
output histogram.
.VARIABLE MODE
Specifies whether to
generate column headers.
.VARIABLE DNCOL
Specifies whether to 
generate the DN column
or not.
.LEVEL2
.VARIABLE INP
STRING
The input VICAR image.  This may be a PICSUM'd image.
.VARIABLE TABLE
STRING
The output histogram table file.  This is a ASCII tab-delimitted text file
containing two columns (with optional text headers).  Column 1 is the 
DN level (from RANGE(1) to RANGE(2)).  Column 2 is the population of the
levels.

If the input was PICSUM'd, the data are scaled down by the PICSCALE scale 
factor prior the the histogram generation.
.VARIABLE RANGE
INTEGER - COUNT=2 - OPTIONAL
The range of DN values to be included in the output histogram.
The default for byte data is (0,255).  For halfword data, the default is
(0,4096).
.VARIABLE MODE
KEYWORD - OPTIONAL - Valid=(HEADER,NOHEADER)
Specifies whether to generate column headers.  The default is to generate 
the column headers.
.VARIABLE DNCOL
KEYWORD - OPTIONAL - Valid=(NODN,DN) - Default=DN
Specifies whether to generate the column of DN values.  The default is 
to generate both DN and population columns.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaschist.pdf
PROCEDURE           !The Xterm "Log to File" feature was used to make log file.
refgbl $syschar
LOCAL DIR    TYPE=STRING 
LOCAL INPIC   TYPE=STRING
refgbl $echo
BODY
let $echo="yes"
let _onfail="continue"
if ($syschar(1) = "UNIX")
   LET DIR   ="/project/test_work/testdata/cassini/iss/"
   defcmd-replace typeit "ush cat"
else 
   LET DIR   ="WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]"
   defcmd-replace typeit "dcl type"
end-if
!BYTE - DEFAULT RANGE 0 255
gen a 10 10
aschist a a.tbl 
typeit a.tbl
!HALF
gen b 100 100 'HALF
aschist b b.tbl RANGE=(50,100) 'NOHEADER
typeit b.tbl
!Eliminate the DN column
aschist b c.tbl RANGE=(50,100) 'NOHEADER 'NODN
typeit c.tbl
!REAL WORLD - COMPARE TO HIST PRINTOUT
LET INPIC = "&DIR"//"sum2.15"      
aschist &INPIC d.tbl RANGE=(0,150)
typeit d.tbl
hist &INPIC
!Try on a PICSUM'd image
LET INPIC = "&DIR"//"pcsm.4"      
label-l &INPIC
hist &INPIC
!The ASCHIST output should combine levels of the HIST output.
!e.g., because of the PICSCALE of 4, HIST levels 0,1,2,3 will 
!be equivalent to ASCHIST level 1.
aschist &INPIC e.tbl range=(0,200)
typeit e.tbl
END-PROC
$ Return
$!#############################################################################
