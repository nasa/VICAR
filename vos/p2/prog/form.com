$!****************************************************************************
$!
$! Build proc for MIPL module form
$! VPACK Version 1.7, Monday, September 12, 1994, 16:10:05
$!
$! Execute by entering:		$ @form
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
$ write sys$output "*** module form ***"
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
$ write sys$output "Invalid argument given to form.com file -- ", primary
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
$   if F$SEARCH("form.imake") .nes. ""
$   then
$      vimake form
$      purge form.bld
$   else
$      if F$SEARCH("form.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake form
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @form.bld "STD"
$   else
$      @form.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create form.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack form.com -
	-s form.f -
	-i form.imake -
	-p form.pdf -
	-t tstform.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create form.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C--2-JAN-1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'pgminc'            ! TAE CONSTANTS & PARAMETERS
      CHARACTER*8  FORMAT
      INTEGER      STATUS, VBLOCK(xprdim), NLSB(3)

      CALL IFMESSAGE('FORM version 2-JAN-1995')
      CALL XVEACTION('SA',' ')
C--OPEN INPUT FILE & GET ITS FORMAT PARAMETERS:
      CALL XVUNIT( IUN, 'INP', 1, STATUS,' ')
      CALL XVOPEN( IUN, STATUS,' ')
      CALL XVGET ( IUN, STATUS, 'FORMAT', FORMAT, 'NL', NLSB(1),
     . 'NS', NLSB(2), 'NB', NLSB(3),' ')

      CALL XVCLOSE( IUN, STATUS,' ')

C--CREATE V-BLOCK:
      CALL XQINI( VBLOCK, xprdim, xabort, STATUS)
      CALL XQSTR( VBLOCK, 'FORMAT', 1, FORMAT,xadd, STATUS)
      CALL XQINTG( VBLOCK, 'NL', 1, NLSB(1), xadd, STATUS)
      CALL XQINTG( VBLOCK, 'NS', 1, NLSB(2), xadd, STATUS)
      CALL XQINTG( VBLOCK, 'NB', 1, NLSB(3), xadd, STATUS)
      CALL XVQOUT( VBLOCK, STATUS)

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create form.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM form

   To Create the build file give the command:

		$ vimake form			(VMS)
   or
		% vimake form			(Unix)


************************************************************************/


#define PROGRAM	form
#define R2LIB

#define MODULE_LIST form.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create form.pdf
PROCESS HELP=*
  LOCAL DUMMY   INTEGER
  LOCAL DUMSTR  STRING
  PARM INP      (STRING,80)
  PARM FORMAT   NAME    DEFAULT=DUMSTR
  PARM NL	NAME    DEFAULT=DUMMY
  PARM NS	NAME    DEFAULT=DUMMY
  PARM NB	NAME    DEFAULT=DUMMY

!# annot function="Vicar Procedure Generation"
!# annot keywords=("data format","Output parameter","TAE variable")
END-PROC
.TITLE
Returns image format and size as TAE variables
.HELP
 "form" opens an input file and returns the data format, NB, NL, and NS
 as output parameters.

 The following procedure is an example of its use to obtain the
 format of a given image file:
.PAGE
PROCEDURE
  PARM  INP	TYPE=STRING
  LOCAL FORMAT  TYPE=KEYWORD
  LOCAL NL	TYPE=INTEGER
  LOCAL NS	TYPE=INTEGER
  LOCAL NB	TYPE=INTEGER
BODY
  form  FILE_NAME  FORMAT  NL  NS
  write "FORMAT="&FORMAT
  write "NL=&NL"
  write "NS=&NS"
  write "NB=&NB"
END-PROC
.PAGE
 HISTORY:

 WRITTEN BY:               Ray Stagner   03/05/85
 CHANGES:
  13-AUG-1990 --LWK-- added NB keyword.
   2-JAN-1995 --AS--- (CRI) Made portable for UNIX

 COGNIZANT PROGRAMMER:     L.W.Kamp

.LEVEL1
.VARIABLE INP
 Input file name
.vari FORMAT
 Format of INP.
 (output)
.VARI NL
 Number of lines in INP.
 (output)
.VARI NS
 Number of samples per line
 in INP.  (output)
.VARI NB
 Number of bands in INP. 
 (output)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstform.pdf
procedure
refgbl $autousage
refgbl $echo
LOCAL FORMAT (STRING,8)
LOCAL NL INTEGER
LOCAL NS INTEGER
LOCAL NB INTEGER
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MODULE FORM   
  gen A 5 10
  form A FORMAT NL NS NB
  write "FORMAT="&FORMAT
  write "NL=&NL"
  write "NS=&NS"
  write "NB=&NB"
END-PROC
$ Return
$!#############################################################################
