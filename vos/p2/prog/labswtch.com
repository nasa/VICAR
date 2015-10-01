$!****************************************************************************
$!
$! Build proc for MIPL module labswtch
$! VPACK Version 1.7, Tuesday, June 07, 1994, 14:38:49
$!
$! Execute by entering:		$ @labswtch
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
$ write sys$output "*** module labswtch ***"
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
$ write sys$output "Invalid argument given to labswtch.com file -- ", primary
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
$   if F$SEARCH("labswtch.imake") .nes. ""
$   then
$      vimake labswtch
$      purge labswtch.bld
$   else
$      if F$SEARCH("labswtch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake labswtch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @labswtch.bld "STD"
$   else
$      @labswtch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create labswtch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack labswtch.com -
	-s labswtch.f -
	-i labswtch.imake -
	-p labswtch.pdf -
	-t tstlabswtch.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create labswtch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

C-----PROGRAM LABSWTCH                  LABSWTCH (A,B) C SIZE
C-----THIS PROGRAM WILL OUTPUT THE DATA OF THE 2ND INPUT WITH
C-----THE USER LABELS OF THE 1ST INPUT, AND THE SYSTEM LABEL
C-----DETERMINED BY THE 2ND INPUT OR THE SIZE FIELD.
C-----IF THE 1ST AND 2ND INPUTS ARE NOT THE SAME SIZE, THE SIZE
C-----FIELD MUST BE USED OR THE OUTPUT MUST BE PREALLOCATED.


C   REVISION HISTORY
C      3-87   SP  ADDED XVSIGNAL CALLS FOR ERROR CHECKING (FOR FR 14986).
C      9-94   AS  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C
      CHARACTER*32 FORMAT
      INTEGER*2 BUF(16000)
      INTEGER SL,SS,NLO,NSO,NLI,NSI,NL,NS,LASTLINE,STATUS
      INTEGER INUNIT(2),OUTUNIT
C
      CALL IFMESSAGE('LABSWTCH version 05-SEP-94')
      CALL XVEACTION('SA',' ')
C--Open input files
      DO I = 1,2
          CALL XVUNIT(INUNIT(I),'INP',I,STATUS,' ')
           CALL XVSIGNAL(INUNIT( I), STATUS, .TRUE.)
          CALL XVOPEN(INUNIT(I),STATUS,' ')
           CALL XVSIGNAL(INUNIT( I), STATUS, .TRUE.)
      END DO
C
C--Get SIZE values if specified and determine number of 
C--lines to be copied
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
C
C--Get system information from 2nd input
C--Get FORMAT, NL, and NS from internal control block
      CALL XVGET(INUNIT(2),STATUS,'FORMAT',FORMAT,'NL',NL,'NS',NS,' ')
       CALL XVSIGNAL(INUNIT( 2), STATUS, .TRUE.)
      IF (NLO .NE. NLI) NL = NLO
      IF (NSO .NE. NSI) NS = NSO
C
C--Now produce output file with system label of 2nd input and history
C--labels of the 1st input.
C
C--Open output with specified FORMAT, NL, and NS
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
       CALL XVSIGNAL(OUTUNIT, STATUS, .TRUE.)
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +            'U_FORMAT',FORMAT,'O_FORMAT',FORMAT,' ')
       CALL XVSIGNAL(OUTUNIT, STATUS, .TRUE.)
C
C--Copy dataset
      LASTLINE = SL + NL - 1
      DO LINE = SL, LASTLINE
          CALL XVREAD(INUNIT(2),BUF,STATUS,'LINE',LINE,'SAMP',SS,
     +                'NSAMPS',NS,' ')
            CALL XVSIGNAL(INUNIT( 2), STATUS, .TRUE.)
          CALL XVWRIT(OUTUNIT,BUF,STATUS,'LINE',LINE,'SAMP',SS,
     +                'NSAMPS',NS,' ')
            CALL XVSIGNAL(OUTUNIT, STATUS, .TRUE.)
      END DO
C
C--Close files and return
      CALL XVCLOSE(INUNIT(1),STATUS,' ')
       CALL XVSIGNAL(INUNIT(1), STATUS, .TRUE.)
      CALL XVCLOSE(INUNIT(2),STATUS,' ')
       CALL XVSIGNAL(INUNIT(2), STATUS, .TRUE.)
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
       CALL XVSIGNAL(OUTUNIT, STATUS, .TRUE.)
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create labswtch.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM labswtch

   To Create the build file give the command:

		$ vimake labswtch			(VMS)
   or
		% vimake labswtch			(Unix)


************************************************************************/


#define PROGRAM	labswtch
#define R2LIB

#define MODULE_LIST labswtch.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create labswtch.pdf
process help=*
PARM INP  TYPE=STRING   COUNT=2
PARM OUT  TYPE=STRING
PARM SIZE TYPE=INTEGER  COUNT=0:4             DEFAULT=--
PARM SL   TYPE=INTEGER  COUNT=0:1             DEFAULT=--
PARM SS   TYPE=INTEGER  COUNT=0:1             DEFAULT=--
PARM NL   TYPE=INTEGER  COUNT=0:1             DEFAULT=--
PARM NS   TYPE=INTEGER  COUNT=0:1             DEFAULT=--
END-PROC
.TITLE
 "labswtch"
.HELP
 "labswtch" creates an output file containing the following:
		the system label of the second input 
		the history labels of the first input file 
		the data of the second input file 
.PAGE
 INVOKATION OF "labswtch"

	labswtch (A,B) C PARMS

 where A B and C are VICAR labeled files.

 Current Cognizant Programmer: Helen De Rueda		May 10, 1984
 Made portable for UNIX: A. Scop (CRI)                  Sep 5,  1994
.LEVEL1
.VARI INP
 STRING - input files
.VARI OUT
 STRING - output file
.VARI SIZE
 INTEGER - size field
.VARI SL
 INTEGER - Starting line of size
 field
.VARI SS
 INTEGER - Starting samp of size
 field
.VARI NL 
 INTEGER - Number of lines of size
 field
.VARI NS 
 INTEGER - Number of samps of size
 field
.LEVEL2
.VARI INP
 (IN1,IN2) - Two input files required.  The system label and data
 of the second file and the history labels of the first are sent
 to the output.
.VARI OUT
 ONE OUTPUT FILE TO RECEIVE THE DATA AND LABELS.
.VARI SIZE
 VICAR size field. Default is from the system label.
.VARI SL
 Starting line of size field. Default is from the system label.
.VARI SS
 Starting samp of size field. Default is from the system label.
.VARI NL 
 Number of lines of size field. Default is from the system label.
.VARI NS 
 Number of samps of size field. Default is from the system label.
$ Return
$!#############################################################################
$Test_File:
$ create tstlabswtch.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
write " "
write " The Test Data are handled for both VMS and UNIX in this PDF."
write " At present (Sep 1994), in order to run the program, the"
write " following data files MUST be copied to the LOCAL directory"
write " where the program resides:"
write "                              OLD     NEW (VMS or UNIX execution)"
write "  MIPLDISK:[MIPL.VGR]F1636832.GEO ==> F1636832.GEO"
write " "
write " This UNIX restriction on the data will be changed eventually."
write " "
write " "
write "prepare some files for the test"
gen B nl=10 ns=10
write "list label of F1636832.GEO and B"
label-list (F1636832.GEO,B)
write "Put the label of F1636832.GEO on the data of B and call it C"
labswtch (F1636832.GEO,B) C
write "The image should have labels from an IO picture and with a format"
write "of byte, nl=10 ns=10 and a 10 by 10 byte image"
label-list C
list C 
write "Now do the same but copy only part of the image"
labswtch (F1636832.GEO,B) C SIZE=(5,5,5,5)
label-list C
list C
END-PROC
$ Return
$!#############################################################################
