'pdf$!****************************************************************************
$!
$! Build proc for MIPL module common_subpdf
$! VPACK Version 1.8, Thursday, May 11, 1995, 08:29:29
$!
$! Execute by entering:		$ @common_subpdf
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module common_subpdf ***"
$!
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to common_subpdf.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("common_subpdf.imake") .nes. ""
$   then
$      vimake common_subpdf
$      purge common_subpdf.bld
$   else
$      if F$SEARCH("common_subpdf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake common_subpdf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @common_subpdf.bld "STD"
$   else
$      @common_subpdf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create common_subpdf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack common_subpdf.com -
	-p common_save_par.pdf common_proc_done.pdf common_subpdf.pdf -
	-i common_subpdf.imake -
	-t tcommon_subpdf.c tcommon_subpdf.imake tcommon_subpdf.pdf -
	   tcommon_subpdf.mdf tcommon_subpdf_general.pdf tcommon_func1.pdf -
	   tcommon_func2.pdf tcommon_func3.pdf tcommon_global.pdf -
	-o common_subpdf.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create common_save_par.pdf
procedure option=selftutor help=*

!-----------------------------------------------------------------------------
! COMMON_SAVE_PAR.PDF
!
! This PDF-file was developed in order to provide various sub-PDF's which 
! can be used by a menu-driven PDF (i.e. a so-called MDF).
! This sub-PDF specifies an entry point in Menu where the user can specify 
! a name for the TAE-parameter file which is generated by the
! program (the default name in VICAR programs: LAST.PAR). This is useful 
! because in a Menu there is no 'save'-command to save a parameter file with 
! a user-specified name (e.g. "save proc_name.par").
!-----------------------------------------------------------------------------



	parm SAVE_PAR_temp	type=(string,40) count=0:1 default=--
	parm SAVE_PAR	type=(string,40) count=0:1 default=last.par
	parm MAIN_PROC_NAME string

	PARMSET name=common_save_sub help=*

		parm SAVE_PAR	type=(string,40) count=1 default=last.par

	END-PROC

body

	if (_tutor=1)

	  	restore-parm common_save_par.par
	  	restore-parm common_proc_name.par


		if (SAVE_PAR="last.par")
	 		let SAVE_PAR_temp="&MAIN_PROC_NAME"//".par"
		else
			let SAVE_PAR_temp="&SAVE_PAR"
	  	end-if

	   	tutor common_save_sub 					   +
		   |restore=common_save_par.par, save=common_save_par.par| +
		   SAVE_PAR=&SAVE_PAR_temp

	   	return


	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " in Tutor Mode. "
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

!# annot function="VICAR Utilities"
!# annot keywords=("sub-PDF",Menu,"TAE-parameter","LAST.PAR")
end-proc
.Title
Various sub-PDFs which can be used by menu-driven PDFs
.help
This sub-PDF specifies an entry point in Menu where the user can specify 
a name for the TAE-parameter file which is generated by the program (the 
default name in VICAR programs: LAST.PAR). This is useful because in a Menu 
there is no 'save'-command to save a parameter file with a user-specified name 
(e.g. "save proc_name.par").

EXECUTION :

USER ACTION				RESULT

don't call this menu point		last.par

exit this menu point with 'exit'	last.par

exit this menu point with 'run'		the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'


Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)
.level1
.var save_par
file name for 'par-file'
.level2
.var save_par
This is the name of the file where all the parameters used in the final
execution of the application program are being stored 
(similarly to "last.par").
.end
$!-----------------------------------------------------------------------------
$ create common_proc_done.pdf
procedure option=selftutor help=*

  refgbl $menus

  parm MAIN_PROC_NAME string

!-----------------------------------------------------------------------------
! COMMON_PROC_DONE.PDF
!
! This PDF-file was developed in order to provide various sub-PDF's which can
! be used by Menu-driven PDF's (i.e. so-called MDF's).
! This sub-PDF is the entry point which is selected to execute the program
! with the keyword readparam="read" ('&"MAIN_PROC_NAME" 'read', see below). 
!-----------------------------------------------------------------------------



body

	if (_tutor=1)
	
		restore-parm common_proc_name.par

		!**********************************************************
		! Execution of &"MAIN_PROC_NAME".PDF (and, therefore, of  
		! the main program). The keyword parameter "readparam" is set 
		! to 'READ'
		!**********************************************************

		let _ONFAIL="continue"

		&MAIN_PROC_NAME 'read

		!**********************************************************
		! The stack of menus (i.e. $MENUS) is set back to the Null
		! value after the pho_demo main program has been executed.
		!**********************************************************

		let $MENUS=--

		!**********************************************************
		! There are three interactive operation modes in TAE:
		! 1) Menu Mode	(driven by the MDF)
		! 2) TCL Mode	(command line)
		! 3) Tutor Mode	(driven by a PDF)
		! To switch between the different modes, see the following
		! table:
		!
		! From		To		Transition
		!
		! Menu Mode	Tutor Mode	procedure selection	   (1)
		!     "		TCL		"COMMAND"		   (2)
		! Tutor Mode	Menu Mode	"EXIT" or proc completion  (3)
		!     "		TCL		"EXIT" or proc completion  (4)
		!				(Note that tutor mode, in  
		!				order to distinguish bet-
		!				ween (3) and (4), remem-
		!				bers whether the user is a
		!				Menu user or a TCL user.
		! TCL Mode	Menu Mode	"MENU"
		!    "		Tutor Mode	"TUTOR"
		!
		! The command COMMAND (see below) exits the Menu Mode and 
		! enters the Command Mode of TAE after the main program
		! part of our demo has been run. When &MAIN_PROC_NAME has
		! been run without any problems, "command" is the last
		! command executed by this procedure.
		!**********************************************************

		command

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs. "
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

!# annot function="VICAR Utilities"
!# annot keywords=("sub-pdf","menu-driven")

end-proc

.Title
Various sub-PDFs which can be used by menu-driven PDFs

.help
This PDF-file was developed in order to provide various sub-PDF's which can
be used by Menu-driven PDF's (i.e. so-called MDF's).
This sub-PDF is the entry point which is selected to execute the program.
There are no parameters to specify. 

OPERATION:

'common_proc_done' calls the main-PDF with the option 
"readparam=read" which is needed to make sure that main_PDF reads (restores) 
all parameter files which have been created and saved by 
the previous calls to the separate PDF's. Finally, the main-PDF calls 
the main application pogram .

.page

Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)
.end
$!-----------------------------------------------------------------------------
$ create common_subpdf.pdf
procedure help=*
body
write " "
write "**********************************************************************"
write " "
write " This procedure is only a dummy and does nothing. "
write " It helps to install the common_subpdf's into the VICAR. "
write " "
write "**********************************************************************"

!# annot function="VICAR Utilities"

end-proc

.title
Various sub-PDFs which can be used by menu-driven PDFs        
$ Return
$!#############################################################################
$Imake_File:
$ create common_subpdf.imake
#define PROCEDURE 	common_subpdf
$ Return
$!#############################################################################
$Test_File:
$ create tcommon_subpdf.c
/* Program TCOMMON_SUBPDF  */

#include "vicmain_c"

void main44()
{
  
  zvmessage( " ", "");
  zvmessage(" This is a message from the main-program 'tcommon_subpdf.c'", "");
  zvmessage( " ", "");

  return;
}
$!-----------------------------------------------------------------------------
$ create tcommon_subpdf.imake
#define PROGRAM  	tcommon_subpdf

#define MODULE_LIST  	tcommon_subpdf.c

#define MAIN_LANG_C

#define USES_ANSI_C

#define TEST

#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB

/********************************************
LOCAL LIBRARY and DEBUGGER for development 

#define LIB_LOCAL
#define DEBUG

*******************************************/


$!-----------------------------------------------------------------------------
$ create tcommon_subpdf.pdf
  procedure option=selftutor help=*

  !*******************************************************************
  ! If OPTIONS=SELFTUTOR (see the first line of this procedure), the
  ! TAE Terminal Monitor (TM) does not do the tutoring, i.e. when a
  ! user requests t(utor) for a SELFTUTOR procedure, the procedure
  ! is executed immediately to perform its own version of tutor.
  !*******************************************************************


	!*************************************************************
	! The process of the main program part (written in C) is named
	! TCOMMON_SUBPDF and has to be declared here.
	!*************************************************************

	process name=tcommon_subpdf
	end-proc

	!*************************************************************
	! The following definitions/defaults will be used in 
	! the batch modus :
	!*************************************************************
	


  ! general input parameters :


	! dummy inputs :

	parm inp	type=(string,32) count=0:1	default=inp.img
	parm out	type=(string,32) count=0:1	default=out.img


	! function --> name of the second entry point of the menu :

	parm FUNCTION type=(string,32) count=1 		+
			valid = (			+
				FUNC1,			+
				FUNC2,			+
				FUNC3			+
				) 	default=FUNC2


	! illumination conditions (additional dummy general parameters) :

        parm INC_ANG	real count=0:1 			default=30
        parm EM_ANG 	real count=0:1 			default=40
        parm PHAS_ANG 	real count=0:1 			default=50




  ! Function parameters:

       parm PARAM1 	real count=0:1 			default=--
       parm PARAM2 	real count=0:1 			default=--
       parm PARAM3 	real count=0:1 			default=--
       parm PARAM4 	real count=0:1 			default=--
       parm PARAM5 	real count=0:1 			default=--


      ! local variable for the function parameters to enable different
      !	default values of the same parameter in different functions :

       local PARAM1_count 	int 
       local PARAM2_count 	int 
       local PARAM3_count 	int
       local PARAM4_count 	int
       local PARAM5_count	int




  ! the names of the parameter file and of the application program :

       parm SAVE_PAR	type=(string,40) 	count=1  default=last.par
       parm MAIN_PROC_NAME 	string		count=1  default=tcommon_subpdf



  ! for running the main program: 

       parm readparam	keyword		 count=0:1	valid=read def=--

	!*******************************************************************
	! The globals "$MENUOPT" and "$SYSCHAR" are used in this proc,
	! so we have to declare they here with the command REFGBL
	!*******************************************************************

	refgbl $menuopt
	refgbl $syschar

  body


  !*************************************************************************
  ! "tcommon_global.pdf" is being executed in the following line. 
  ! In this PDF the global COMMON_FUNC_type is defined. 
  !*************************************************************************

  tcommon_global TCOMMON_FUNC = "&FUNCTION" TMAIN_PROC_NAME="&_PROC"



  ! for different funtions :

  !*******************************************************************
  ! set the value of the local variable parameter-count to the beginning 
  ! parameter_count:
  !*******************************************************************

  let PARAM1_count 	=$count(PARAM1)
  let PARAM2_count 	=$count(PARAM2)
  let PARAM3_count 	=$count(PARAM3)
  let PARAM4_count 	=$count(PARAM4)
  let PARAM5_count 	=$count(PARAM5)




    !*************************************************************************
    ! _TUTOR (type=integer) is an implicit local variable in a procedure
    ! with OPTIONS=SELFTUTOR (see first line of this proc). When TAE
    ! receives an initial tutor request for a procedure declared as selftutor,
    ! _TUTOR is set to one, otherwise it is set to zero.
    !*************************************************************************

    if (_tutor=1)

  	!********************************************************************
  	! dummy files in case the procs aren't called :
  	! The save-variable (sub-)commmand is used to save the specified
  	! variables into the save file.
  	!
  	! Command:
  	! SAVE-VARIABLE FILE = save_file_name, VARIABLE = variable_list
  	!*******************************************************************

  	!*******************************************************************
  	! Saving the parameter-file with the function parameters
  	! either the inputed parameters or if did no parameter input - the  
  	! default parameter for the actual function :
  	!******************************************************************* 	
	!*******************************************************************
	! At this position you can input function-specific default values 
	! for the function parameter for the tutor modus : 
	!*******************************************************************




  ! FUNC1:

	if (PARAM1_count=0)				let PARAM1=0.1
	if (PARAM2_count=0)				let PARAM2=0.2
	if (PARAM3_count=0)				let PARAM3=0.3
	if (PARAM5_count=0)				let PARAM5=0.4

          save-var tcommon_func1.par, (		+
				PARAM1,		+
				PARAM2,		+
				PARAM3,		+
				PARAM5		+
				)

	if (PARAM1_count=0)	let PARAM1=--
	if (PARAM2_count=0)	let PARAM2=--
	if (PARAM3_count=0)	let PARAM3=--
	if (PARAM5_count=0)	let PARAM5=--


  ! FUNC2:

	if (PARAM1_count=0)				let PARAM1=0.5
	if (PARAM2_count=0)				let PARAM2=0.6
	if (PARAM3_count=0)				let PARAM3=0.7
	if (PARAM4_count=0)				let PARAM4=0.7
	if (PARAM5_count=0)				let PARAM5=0.9

          save-var tcommon_func2.par, (		+
				PARAM1,		+
				PARAM2,		+
				PARAM3,		+
				PARAM4,		+
				PARAM5		+
				)

	if (PARAM1_count=0)	let PARAM1=--
	if (PARAM2_count=0)	let PARAM2=--
	if (PARAM3_count=0)	let PARAM3=--
	if (PARAM4_count=0)	let PARAM3=--
	if (PARAM5_count=0)	let PARAM5=--


  ! FUNC3:

	if (PARAM2_count=0)				let PARAM2=1.0
	if (PARAM3_count=0)				let PARAM3=1.1
	if (PARAM4_count=0)				let PARAM4=1.2
	if (PARAM5_count=0)				let PARAM5=1.3

          save-var tcommon_func3.par, (		+
				PARAM2,		+
				PARAM3,		+
				PARAM4,		+
				PARAM5		+
				)

	if (PARAM2_count=0)	let PARAM2=--
	if (PARAM3_count=0)	let PARAM3=--
	if (PARAM4_count=0)	let PARAM3=--
	if (PARAM5_count=0)	let PARAM5=--





	!*******************************************************************
	! other dummy files in case the procs aren't called :
	!*******************************************************************

          save-var &"_PROC"_general.par, (	+
				inp,		+
				out, 		+
				FUNCTION,	+
				INC_ANG,	+
				EM_ANG,		+
				PHAS_ANG	+
				)

	  save-var common_save_par.par, SAVE_PAR

	  save-var common_proc_name.par, MAIN_PROC_NAME





	  !****************************************************************
	  ! The string global variable $MENUOPT allows suppression of the
	  ! "Press RETURN key for menu" message. Hence, when typing
	  ! "VICAR> tutor TCOMMON_SUBPDF" the menu can be entered directly 
	  ! without the need to press the RETURN key first.
	  !****************************************************************

	    let $menuopt="NO_PRESS_FOR_MENU"

	  !****************************************************************
	  ! "menu" puts VICAR into the Menu Mode, i.e. here we enter the
	  !  menu of 'TCOMMON_SUBPDF.MDF'
	  !****************************************************************

	    menu &"_PROC".mdf

   end-if

    write "returned into main-PDF &_PROC"

! passing the parameters from tutor mode and then running the process :

	!***************************************************************
	! $COUNT returns the current number of elements for "readparam":
	! If the keyword parameter "readparam" has been set to 'read',
	! $count(readparam)=1, else $count(readparam)<>1 (see below).
	!***************************************************************

	if ($count(readparam)=1) 


		!****************************************************
		! The restore-parms (sub-)command is used to restore
		! only the parameters in the save file.
		!
		! Command:
		! RESTORE-PARMS FILE = save_file_name
		!****************************************************

		restore-parm common_save_par.par

		restore-parm &"_PROC"_general.par

		if ("&FUNCTION" = "FUNC1" ) restore-parm tcommon_func1.par
		if ("&FUNCTION" = "FUNC2" ) restore-parm tcommon_func2.par
		if ("&FUNCTION" = "FUNC3" ) restore-parm tcommon_func3.par



		save-var "&SAVE_PAR", (		+
			inp,			+
			out, 			+
			FUNCTION,		+
			INC_ANG,		+
			EM_ANG,			+
			PHAS_ANG,		+
			PARAM1,			+
			PARAM2,			+
			PARAM3,			+
			PARAM4,			+
			PARAM5			+
			SAVE_PAR		+
			)



		!*********************************************
		! The parameter and their values are displayed
		! on the terminal ...
		!*********************************************

!		display-parms

		!*************************************************
		! ... and the main program TCOMMON_SUBP is run
		!*************************************************

		&_PROC

	end-if



! running the process in the batch modus :

	if ($count(readparam) <> 1) 

		!***********************************************************
		! At this position you can input function-specific default 
		! values for the function parameter for the batch modus : 
		!***********************************************************


		if ( "&FUNCTION" = "FUNC1" )
			if (PARAM1_count=0)		let PARAM1=0.1
			if (PARAM2_count=0)		let PARAM2=0.2
			if (PARAM3_count=0)		let PARAM3=0.3
			if (PARAM5_count=0)		let PARAM5=0.4
		end-if

		if ( "&FUNCTION" = "FUNC2" )
			if (PARAM1_count=0)		let PARAM1=0.5
			if (PARAM2_count=0)		let PARAM2=0.6
			if (PARAM3_count=0)		let PARAM3=0.7
			if (PARAM4_count=0)		let PARAM4=0.8
			if (PARAM5_count=0)		let PARAM5=0.9
		end-if

		if ( "&FUNCTION" = "FUNC" )
			if (PARAM2_count=0)		let PARAM2=1.0
			if (PARAM3_count=0)		let PARAM3=1.1
			if (PARAM4_count=0)		let PARAM4=1.2
			if (PARAM5_count=0)		let PARAM5=1.3
		end-if





		!************************************************
		! The parameter and their values can be 
		! displayed on the terminal ...
		!************************************************

!		display-parms

		!************************************************
		! ... and the main program TCOMMON_SUBPDF is run
		!************************************************

		&_PROC

	end-if



! delete the temporary .par files only for the batch :

	if ($count(readparam) = 1) 

	    if ($syschar(1) = "UNIX")

		ush /bin/rm -f tcommon_subpdf_general.par;		+
		    /bin/rm -f tcommon_func1.par;			+
		    /bin/rm -f tcommon_func2.par;			+
		    /bin/rm -f tcommon_func3.par;

		ush /bin/rm -f common_proc_name.par;			+
		    /bin/rm -f common_save_par.par

	    else

	      dcl if f$search ("tcommon_subpdf_general.par;*") .nes. "" +
		 	then delete tcommon_subpdf_general.par;*	
	      dcl if f$search ("tcommon_func1.par;*") .nes. "" 		+
		 	then delete tcommon_func1.par;*			
 	      dcl if f$search ("tcommon_func2.par;*") .nes. "" 		+
		 	then delete tcommon_func2.par;*			
	      dcl if f$search ("tcommon_func3.par;*") .nes. "" 		+
		 	then delete tcommon_func3.par;*

              dcl if f$search ("common_proc_name.par;*") .nes. "" 	+
		 	then delete common_proc_name.par;*		
	      dcl if f$search ("common_save_par.par;*") .nes. "" 	+
		 	then delete common_save_par.par;*

	    end-if

	end-if

	! delete all photometrical globals:

!	delete-global common_global 

!# annot function="VICAR Utilities"
!# annot keywords=(test,"menu-driven",photometric,calculus,MDF)

  end-proc

.title
Various sub-PDFs which can be used by menu-driven PDFs

.help
PURPOSE:	TCOMMON_SUBPDF tests the common sub-pdf's.
	
FUNCTION:

TCOMMON_SUBPDF is a program to test the common sub-pdf's and demonstrate the 
usage of menu-driven input of the large number of different PDF parameters 
(as example related to photometric calculus) into the main program (including 
their various permitted ranges and default settings).  

The MDF is used to organize PDF parameters into different "menus".  The goal of 
such menus is to organize input parameters into parameter groups related to 
specific topics (e.g. I/O-parameters, photometric parameters, map parameters,  
SPICE parameters) in order to facilitate the preparation and the execution of a 
program to the user.

This program uses most of the sub PDFs delivered by the 'COMMON_SUBPDF.COM'.  

This test program shows to the software user how to
	-navigate through a menu;
	-switch between the interactive operation modes of TAE (i.e. Command  
	  Line Mode [TCL], Tutor Mode, and Menu Mode);
	-save the parameter set for later re-processing;
	-delete temporary parameter files.

This program demonstrates to the software developer how to
	-integrate sub PDFs into their main program
	-set the different defaults for parameters for a tutor-session 
	 or the batch mode;

The programmers are welcome to cut out and vary the tcommon_subpdf.pdf 
tcommon_subpdf_general.pdf, tcommon_subpdf.mdf and the tcommon_global.pdf for 
use in their own programs using other defaults, additional parameters, and 
different or additional submenues (see also the demonstration program PHODEM). 
But be carefully for creating names for global variables, because these global
defintions are for the hole VICAR-session.  Note that the default values of 
the SUB-PDF's will be overwritten by the default values of the main-pdf (in 
this case by TCOMMON_SUBPDF.PDF).


EXECUTION:

There are separate PDFs for each entry point seen in the main menu.  If you 
select a particular menu point, you will enter the normal tutor mode of 
this SUBPDF.  In this demo program, the menu points have the following 
(dummy/simulated!) meanings:

1. Select the first menu point to input the general parameters for a program
   such as the names of input/output files, the name of the second entry 
   point, and so on.
2. Select this menu point to enter the parameters associated to the 
   selected function.
   In this case, the name of this menu point (the SUBPDF pertaining to this 
   menu point and) are changing depending on your input of the parameter 
   FUNCTION in the first menu point.
3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'


4. This menu point is the entry point to execute the main program.


You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


HELPS :

- You will get the common help contained in the ".mdf" file (tcommon_subpdf.mdf)
  by typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF 
  (tcommon_subpdf.pdf) by processing of "help-help" applied to the program 
  (should be verry similary).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.




VICAR PARAMETERS:

NOTE: All parameters (including those from the function subPDFs) have to be 
defined in the main-PDF (TCOMMON_SUBPDF.pdf).

Name		Type		Description

inp		string		File name of the input image (dummy) 	
				
out		string		File name for the output image(dummy)
						
FUNCTION	string		This parameter selects the menu point for 
				input the parameters accossiated with this 
				function.
				When returning to the highest level of the 
				menu (i.e. the MDF-file) you will see that 
 				the second selection point has been changed 
				according to your input of FUNCTION .

PARAM1		real		Parameter valid for the FUNC1, FUNC2 and FUNC3. 

PARAM2		real		Parameter valid for the FUNC1, FUNC2 and FUNC3.

PARAM3		real		Parameter valid for the FUNC1, FUNC2 and FUNC3.

PARAM4		real		Parameter valid for the FUNC2 and FUNC3.

PARAM5		real		Parameter valid for the FUNC1 and FUNC3.


PARAMETERS SORTED BY SUB-PDFS :

Name of PDF file	PDF parameters	default		comments
(will appear as a	within this	setting	
separate sub-menu)	particular 
			sub-menu			


1) GENERAL INPUT / OUTPUT :
	
tcommon_subpdf_general	INP

			OUT

			FUNCTION	FUN2 	Desired Function/mode	
						determined the second menu point
									
						Permitted values :

						FUNC1, 
						FUNC2, 
						FUNC3 

			INC_ANG			other parameter independent of 
						the functions

			EM_ANG			other parameter independent of 
						the functions

			PHAS_ANG		other parameter independent of 
						the functions


2) FUNCTION MENU :

Note:  All PDF parameters must be defined in the main program's PDF in order to 
access the different submenues correctly.  

actuell second menu	Parameter 	default	Comment
(subpdf-name.pdf)	Names

COMMON_FUNC1		PARAM1		0.1	parameter 1 
(common_func1.pdf)	PARAM2		0.2	parameter 2
			PARAM3		0.3	parameter 3
			PARAM5		0.4	parameter 5

COMMON_FUNC2		PARAM1		0.5	parameter 1
(common_func2.pdf)	PARAM2		0.6	parameter 2 
			PARAM3		0.7	parameter 3
			PARAM4		0.8	parameter 4
			PARAM5		0.9	parameter 5

COMMON_FUNC3		PARAM2		1.0	parameter 2
(common_func3.pdf)	PARAM3		1.1	parameter 3
			PARAM4		1.2	parameter 4
			PARAM5		1.3	parameter 5



3) PAR-FILE NAME

common_save_par		SAVE_PAR		Name of the TAE-parameter file 
						containing all parameters 
						needed to running the program.
                                                default:
						name of the main program 
						with the extention ".par

4) RUN  MAIN PROGRAM

common_proc_done		-		runs the program


GLOBAL VARIABLE:
The following global variables defined by the tcommon_global.pdf must be 
referenced:

	Name		Type			Description

	TCOMMON_FUNC 	string			It containes the names of the 
						valid functions (to pass 
						into the menu).

	TMAIN_PROC_NAME	string			Name of the main procedure.


	

BACKGROUND AND REFERENCES :	TAE Command Language (TCL)
				Programmers's Manual
				Version 5.1
				NASA Goddard Space Flight Centre, 1991


SOFTWARE PLATFORM :		VICAR 13, TAE 5.2
				(VMS/AXP/SUNOS/SOLARIS)

HARDWARE PLATFORM :		No particular hardware required;
				tested on VMS/AXP/SUNOS/SOLARIS

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Friedel Oschuetz, Sept. '94, original

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)





.LEVEL1

.VARI inp
File name of the input image (dummy)

.VARI out
File name for the output image (dummy)

.VARI FUNCTION
Function --> second menue point name

.VARI PARAM1
Parameter 1

.VARI  PARAM2	
Parameter 2

.VARI  PARAM3
Parameter 3

.VARI  PARAM4
Parameter 4

.VARI  PARAM5
Parameter 5

.VARI INC_ANG
Incidence angle in degree.

.VARI EM_ANG
Emission angle in degree.

.VARI PHAS_ANG
Phase angle in degree.



.LEVEL2

.VARI inp
File name of the input image (dummy)

.VARI out
File name for the output image(dummy)

.VARI FUNCTION
Function :

This parameter selects the menu point for input the parameters accossiated with 
this function.
When returning to the highest level of the menu (i.e. the MDF-file) you will 
see that the second selection point has been changed accordingly to your 
input of FUNCTION .


.VARI  PARAM1
Parameter valid for the FUNC1, FUNC2 and FUNC3.


.VARI  PARAM2	
Parameter valid for the FUNC1, FUNC2 and FUNC3

.VARI  PARAM3
Parameter valid for the FUNC1, FUNC2 and FUNC3.

.VARI  PARAM4
Parameter valid for the FUNC2 and FUNC3.

.VARI  PARAM5
Parameter valid for the FUNC1 and FUNC3.

.VARI INC_ANG
Incidence angle in degree (dummy).

.VARI EM_ANG
Emission angle in degree (dummy).

.VARI PHAS_ANG
Phase angle in degree (dummy).

.end
$!-----------------------------------------------------------------------------
$ create tcommon_subpdf.mdf
.TITLE
VICAR demonstrationprogram  '&"TMAIN_PROC_NAME"'

.proc &"TMAIN_PROC_NAME"_general
Enter general input parameters and select function of the second menu-point 
(type RUN when done)
This point has to be addapted to the photometry application program

.proc tcommon_&TCOMMON_FUNC
Enter parameters for the "&TCOMMON_FUNC" function 
(type RUN when done)

.proc common_save_par
Enter the name for the par-file where you want to save your parameters
(type RUN when done)

.proc common_proc_done
Run main application program '&"TMAIN_PROC_NAME"'



.help
Name of Program:	TCOMMON_SUBPDF
	
PURPOSE:	TCOMMON_SUBPDF tests the common sub-pdf's.
	
FUNCTION:

TCOMMON_SUBPDF is a program to test the common sub-pdf's and demonstrate the 
usage of menu-driven input of the large number of different PDF parameters 
(as example related to photometric calculus) into the main program (including 
their various permitted ranges and default settings).  

The MDF is used to organize PDF parameters into different "menus".  The goal of 
such menus is to organize input parameters into parameter groups related to 
specific topics (e.g. I/O-parameters, photometric parameters, map parameters,  
SPICE parameters) in order to facilitate the preparation and the execution of a 
program to the user.

This program uses most of the sub PDFs delivered by the 'COMMON_SUBPDF.COM'.  

This test program shows to the software user how to
	-navigate through a menu;
	-switch between the interactive operation modes of TAE (i.e. Command  
	  Line Mode [TCL], Tutor Mode, and Menu Mode);
	-save the parameter set for later re-processing;
	-delete temporary parameter files.

This program demonstrates to the software developer how to
	-integrate sub PDFs into their main program
	-set the different defaults for parameters for a tutor-session 
	 or the batch mode;

The programmers are welcome to cut out and vary the tcommon_subpdf.pdf 
tcommon_subpdf_general.pdf, tcommon_subpdf.mdf and the tcommon_global.pdf for 
use in their own programs using other defaults, additional parameters, and 
different or additional submenues (see also the demonstration program PHODEM). 
But be carefully for creating names for global variables, because these global
defintions are for the hole VICAR-session.  Note that the default values of 
the SUB-PDF's will be overwritten by the default values of the main-pdf (in 
this case by TCOMMON_SUBPDF.PDF).


EXECUTION:

There are separate PDFs for each entry point seen in the main menu.  If you 
select a particular menu point, you will enter the normal tutor mode of 
this SUBPDF.  In this demo program, the menu points have the following 
(dummy/simulated!) meanings:

1. Select the first menu point to input the general parameters for a program
   such as the names of input/output files, the name of the second entry 
   point, and so on.
2. Select this menu point to enter the parameters associated to the 
   selected function.
   In this case, the name of this menu point (the SUBPDF pertaining to this 
   menu point and) are changing depending on your input of the parameter 
   FUNCTION in the first menu point.
3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'


4. This menu point is the entry point to execute the main program.


You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


HELPS :

- You will get the common help contained in the ".mdf" file (tcommon_subpdf.mdf)
  by typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF 
  (tcommon_subpdf.pdf) by processing of "help-help" applied to the program 
  (should be verry similary).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.




VICAR PARAMETERS:

NOTE: All parameters (including those from the photometry subPDFs) have to be 
defined in the mainPDF (TPHOPDF.pdf).

Name		Type		Description

inp		string		File name of the input image (dummy) 						
out		string		File name for the output image(dummy)
						
FUNCTION	string		This parameter selects the menu point for 
				input the parameters accossiated with this 
				function.
				When returning to the highest level of the 
				menu (i.e. the MDF-file) you will see that 
 				the second selection point has been changed 
				according to your input of FUNCTION .

INC_ANG		real		other parameter independent of the functions

EM_ANG		real		other parameter independent of the functions

PHAS_ANG	real		other parameter independent of the functions

PARAM1		real		Parameter valid for the FUNC1, FUNC2 and FUNC3. 

PARAM2		real		Parameter valid for the FUNC1, FUNC2 and FUNC3.

PARAM3		real		Parameter valid for the FUNC1, FUNC2 and FUNC3.

PARAM4		real		Parameter valid for the FUNC2 and FUNC3.

PARAM5		real		Parameter valid for the FUNC1 and FUNC3.


PARAMETERS SORTED BY SUB-PDFS :

Name of PDF file	PDF parameters	default		comments
(will appear as a	within this	setting	
separate sub-menu)	particular 
			sub-menu			


1) GENERAL INPUT / OUTPUT :
	
tcommon_subpdf_general	INP

			OUT

			FUNCTION	FUN2 	Desired Function/mode	
						determined the second menu point
									
						Permitted values :

						FUNC1, 
						FUNC2, 
						FUNC3 

			INC_ANG			other parameter independent of 
						the functions

			EM_ANG			other parameter independent of 
						the functions

			PHAS_ANG		other parameter independent of 
						the functions


2) FUNCTION MENU :

Note:  All PDF parameters must be defined in the main program's PDF in order to 
access the different submenues correctly.  

actuell second menu	Parameter 	default	Comment
(subpdf-name.pdf)	Names

COMMON_FUNC1		PARAM1		0.1	parameter 1 
(common_func1.pdf)	PARAM2		0.2	parameter 2
			PARAM3		0.3	parameter 3
			PARAM5		0.4	parameter 5

COMMON_FUNC2		PARAM1		0.5	parameter 1
(common_func2.pdf)	PARAM2		0.6	parameter 2 
			PARAM3		0.7	parameter 3
			PARAM4		0.8	parameter 4
			PARAM5		0.9	parameter 5

COMMON_FUNC3		PARAM2		1.0	parameter 2
(common_func3.pdf)	PARAM3		1.1	parameter 3
			PARAM4		1.2	parameter 4
			PARAM5		1.3	parameter 5



3) PAR-FILE NAME

common_save_par		SAVE_PAR		Name of the TEA-parameter file 
						containing all parameters 
						needed to running the program.
                                                default:
						name of the main program 
						with the extention ".par

4) RUN  MAIN PROGRAM

common_proc_done		-			runs the program


GLOBAL VARIABLE:
The following global variables defined by the tcommon_global.pdf must be 
referenced:

	Name		Type			Description

	TCOMMON_FUNC	string	      		It containes the names of the 
						valid functions (to pass 
						into the menu).

	TMAIN_PROC_NAME	string			Name of the main procedure.


	

BACKGROUND AND REFERENCES :	TAE Command Language (TCL)
				Programmers's Manual
				Version 5.1
				NASA Goddard Space Flight Centre, 1991


SOFTWARE PLATFORM :		VICAR 13, TAE 5.2
				(VMS/AXP/SUNOS/SOLARIS)

HARDWARE PLATFORM :		No particular hardware required;
				tested on VMS/AXP/SUNOS/SOLARIS

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Friedel Oschuetz, Sept. '94, original

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)



.end
$!-----------------------------------------------------------------------------
$ create tcommon_subpdf_general.pdf
procedure option=selftutor help=*

!-----------------------------------------------------------------------------
! TCOMMON_SUBPDF_GENERAL.PDF
!
! This is the PDF for the first menu point of the MDF file.
! In this PDF file general parameters are defined like the names of 
! input/output files, the name of the second entry point, and so on.
!
!-----------------------------------------------------------------------------

	!**********************************************************************
	! The global variables TCOMMON_FUNC, TMAIN_PROC_NAME (and $MENUS) will  
	! be used in this procedure, so they have to be declared here.
	! The global TCOMMON_FUNC (the desired function) will be
	! used to change the second menu point of the MDF file (see above)
	! according to the input to FUNCTION in this PDF.
	! The global $MENUS keeps the active stack of Menu Definition File
	! names. $MENUS(1) is the root menu name, $MENUS(2) the menu selected
	! from the root menu, and so on. The current menu stack can be
	! displayed with "DISPLAY $MENUS". Since there is only one MDF in
	! this demo, it doesn't make too much sense to have that global
	! included here. It is just referenced for completeness.
	!**********************************************************************

	refgbl TCOMMON_FUNC 
	refgbl TMAIN_PROC_NAME 
	refgbl $menus

	parm MAIN_PROC_NAME string

	procedure name=tcommon_subpdf_general_sub help=*

		! dummy inputs :

		parm inp	type=(string,32) count=0:1 default=--
		parm out	type=(string,32) count=0:1 default=--

		! functions :

		parm FUNCTION type=(string,32) count=1 	+
			valid = ( 			+
				FUNC1,			+
				FUNC2,			+
				FUNC3			+
						)	+
						 default="&TCOMMON_FUNC"

		! other parameters :

    		parm INC_ANG	real count=0:1 +
					default=--
    		parm EM_ANG 	real count=0:1 +
					default=--
    		parm PHAS_ANG 	real count=0:1 +
					default=--



	body

	!*******************************************************************
	! "tcommon_global.pdf" is being executed in the following line. 
	! In this PDF, two global are defined (TCOMMON_FUNC, 
	! TMAIN_PROC_NAME)
	!*******************************************************************
 
	tcommon_global TCOMMON_FUNC="&FUNCTION"

	end-proc
body

	if (_tutor=1)

 	   tutor tcommon_subpdf_general_sub 				+
			|restore=&"TMAIN_PROC_NAME"_general.par 	+
		 	 save=&"TMAIN_PROC_NAME"_general.par|
	else
	   write " ************************************************"
	   write " "
	   write " This program works only when run from tutor mode"
           write " of other programs."
	   write " "
	   write " ************************************************"
	end-if

end-proc

.TITLE
VICAR sub-menu TCOMMON_SUBPDF_GENERAL

.HELP
PURPOSE:
This menu point is dedicated to input general parameters for the program such 
as the names of input/output images the illumination conditions, and so on.

NOTE : The parameter FUNCTION is need to change the second entry point of 
       the menu :  When returning to the highest level of the menu 
      (i.e. the &"TMAIN_PROC_NAME".MDF-file) you will see that the second 
      selection point has been changed according to your input of FUNCTION in 
      this menu.


.PAGE
Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

HISTORY:
original version	F. Oschuetz DLR



.LEVEL1

.VARI inp
Input image

.VARI out
Output image

.VARI FUNCTION
Function --> second menu point

.VARI INC_ANG
incidence angle

.VARI EM_ANG
emission angle

.VARI PHAS_ANG
phase angle


.LEVEL2

.VARI inp
File name of the input image (dummy).

.VARI out
File name for the output image (dummy).

.VARI FUNCTION
The parameter FUNTION is need to change the second entry point of 
the menu :  When returning to the highest level of the menu 
(i.e. the &"TMAIN_PROC_NAME".MDF-file) you will see that the second 
selection point has been changed according to your input of FUNCTION in 
this menu.

.VARI INC_ANG
Incidence angle in degree (dummy general parameter).

.VARI EM_ANG
Emission angle in degree (dummy general parameter).

.VARI PHAS_ANG
Phase angle in degree (dummy general parameter).

.end
$!-----------------------------------------------------------------------------
$ create tcommon_func1.pdf
procedure option=selftutor help=*

	parm PARAM1_temp	 real count=0:1 			def=0.1 
	parm PARAM2_temp	 real count=0:1 			def=0.2
	parm PARAM3_temp	 real count=0:1 			def=0.3
	parm PARAM5_temp	 real count=0:1 			def=0.4

	parm PARAM1	 real count=0:1 	def=-- 
	parm PARAM2	 real count=0:1 	def=--
	parm PARAM3	 real count=0:1 	def=--
	parm PARAM5	 real count=0:1 	def=--

	PARMSET name=tcommon_func1_sub help=*

		parm PARAM1	 real count=0:1 def=-- 
		parm PARAM2	 real count=0:1 def=--
		parm PARAM3	 real count=0:1 def=--
		parm PARAM5	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm tcommon_func1.par


	  if ($count(PARAM1)=0)
	 						let PARAM1_temp=0.1
	  else
	 	let PARAM1_temp=&PARAM1
	  end-if


	  if ($count(PARAM2)=0)
	 						let PARAM2_temp=0.2
	  else
	 	let PARAM2_temp=&PARAM2
	  end-if


	  if ($count(PARAM3)=0)
	 						let PARAM3_temp=0.3
	  else
	 	let PARAM3_temp=&PARAM3
	  end-if


	  if ($count(PARAM5)=0)
	 						let PARAM5_temp=0.4
	  else
	 	let PARAM5_temp=&PARAM5
	  end-if



	   tutor tcommon_func1_sub 					+
		|restore=tcommon_func1.par, save=tcommon_func1.par|	+
		PARAM1=&PARAM1_temp,					+
		PARAM2=&PARAM2_temp,					+
		PARAM3=&PARAM3_temp,					+
		PARAM5=&PARAM5_temp

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs. "
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
&_PROC 

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the first 
function FUNC1. 

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

HSTORY:

original	F. Oschuetz	Sept.'94


.level1

.VARI PARAM1
parameter 1 

.VARI PARAM2
parameter 2 

.VARI PARAM3
parameter 3 

.VARI PARAM5
parameter 5 



.level2

.VARI PARAM1
Parameter 1 accossiated to the function FUNC1.

.VARI PARAM2
Parameter 2 accossiated to the function FUNC1.

.VARI PARAM3
Parameter 3 accossiated to the function FUNC1.

.VARI PARAM5
Parameter 5 accossiated to the function FUNC1.

.end
$!-----------------------------------------------------------------------------
$ create tcommon_func2.pdf
procedure option=selftutor  help=*

	parm PARAM1_temp	 real count=0:1 			def=0.5 
	parm PARAM2_temp	 real count=0:1 			def=0.6
	parm PARAM3_temp	 real count=0:1 			def=0.7
	parm PARAM4_temp	 real count=0:1 			def=0.8
	parm PARAM5_temp	 real count=0:1 			def=0.9

	parm PARAM1	 real count=0:1 	def=-- 
	parm PARAM2	 real count=0:1 	def=--
	parm PARAM3	 real count=0:1 	def=--
	parm PARAM4	 real count=0:1 	def=--
	parm PARAM5	 real count=0:1 	def=--

	PARMSET name=tcommon_func2_sub help=*

		parm PARAM1	 real count=0:1 def=-- 
		parm PARAM2	 real count=0:1 def=--
		parm PARAM3	 real count=0:1 def=--
		parm PARAM4	 real count=0:1 def=--
		parm PARAM5	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm tcommon_func2.par


	  if ($count(PARAM1)=0)
	 						let PARAM1_temp=0.5
	  else
	 	let PARAM1_temp=&PARAM1
	  end-if


	  if ($count(PARAM2)=0)
	 						let PARAM2_temp=0.6
	  else
	 	let PARAM2_temp=&PARAM2
	  end-if


	  if ($count(PARAM3)=0)
	 						let PARAM3_temp=0.7
	  else
	 	let PARAM3_temp=&PARAM3
	  end-if


	  if ($count(PARAM4)=0)
	 						let PARAM4_temp=0.8
	  else
	 	let PARAM4_temp=&PARAM4
	  end-if


	  if ($count(PARAM5)=0)
	 						let PARAM5_temp=0.9
	  else
	 	let PARAM5_temp=&PARAM5
	  end-if



	   tutor tcommon_func2_sub 					+
		|restore=tcommon_func2.par, save=tcommon_func2.par|	+
		PARAM1=&PARAM1_temp	      				+
		PARAM2=&PARAM2_temp					+
		PARAM3=&PARAM3_temp					+
		PARAM4=&PARAM4_temp					+
		PARAM5=&PARAM5_temp

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs. "
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
&_PROC 

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the second 
function FUNC2. 

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

HSTORY:

original	F. Oschuetz	Sept.'94


.level1

.VARI PARAM1
parameter 1 

.VARI PARAM2
parameter 2 

.VARI PARAM3
parameter 3 

.VARI PARAM4
parameter 4 

.VARI PARAM5
parameter 5 



.level2

.VARI PARAM1
Parameter 1 accossiated to the function FUNC2.

.VARI PARAM2
Parameter 2 accossiated to the function FUNC2.

.VARI PARAM3
Parameter 3 accossiated to the function FUNC2.

.VARI PARAM4
Parameter 4 accossiated to the function FUNC2.

.VARI PARAM5
Parameter 5 accossiated to the function FUNC2.

.end
$!-----------------------------------------------------------------------------
$ create tcommon_func3.pdf
procedure option=selftutor help=*

	parm PARAM2_temp	 real count=0:1 			def=1.0
	parm PARAM3_temp	 real count=0:1 			def=1.1
	parm PARAM4_temp	 real count=0:1 			def=1.3
	parm PARAM5_temp	 real count=0:1 			def=1.4

	parm PARAM2	 real count=0:1 	def=--
	parm PARAM3	 real count=0:1 	def=--
	parm PARAM4	 real count=0:1 	def=--
	parm PARAM5	 real count=0:1 	def=--

	PARMSET name=tcommon_func3_sub help=*

		parm PARAM2	 real count=0:1 def=--
		parm PARAM3	 real count=0:1 def=--
		parm PARAM4	 real count=0:1 def=--
		parm PARAM5	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm tcommon_func3.par


	  if ($count(PARAM2)=0)
	 						let PARAM2_temp=1.0
	  else
	 	let PARAM2_temp=&PARAM2
	  end-if


	  if ($count(PARAM3)=0)
	 						let PARAM3_temp=1.1
	  else
	 	let PARAM3_temp=&PARAM3
	  end-if


	  if ($count(PARAM4)=0)
	 						let PARAM4_temp=1.2
	  else
	 	let PARAM4_temp=&PARAM4
	  end-if


	  if ($count(PARAM5)=0)
	 						let PARAM5_temp=1.3
	  else
	 	let PARAM5_temp=&PARAM5
	  end-if



	   tutor tcommon_func3_sub 					+
		|restore=tcommon_func3.par, save=tcommon_func3.par|	+
		PARAM2=&PARAM2_temp					+
		PARAM3=&PARAM3_temp					+
		PARAM4=&PARAM4_temp					+
		PARAM5=&PARAM5_temp

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs. "
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
&_PROC 

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the second 
function FUNC3. 

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

HSTORY:

original	F. Oschuetz	Sept.'94


.level1

.VARI PARAM2
parameter 2 

.VARI PARAM3
parameter 3 

.VARI PARAM4
parameter 4 

.VARI PARAM5
parameter 5 



.level2

.VARI PARAM2
Parameter 2 accossiated to the function FUNC3.

.VARI PARAM3
Parameter 3 accossiated to the function FUNC3.

.VARI PARAM4
Parameter 4 accossiated to the function FUNC3.

.VARI PARAM5
Parameter 5 accossiated to the function FUNC3.

.end
$!-----------------------------------------------------------------------------
$ create tcommon_global.pdf
globals

!-----------------------------------------------------------------------------
! TCOMMON_GLOBAL.PDF
!
! This is a Globals PDF which defines global variables 
! for menu-driven application programs.
!-----------------------------------------------------------------------------

	parm TCOMMON_FUNC type=string count=1 valid = ( 	+
			FUNC1,					+
			FUNC2,					+
			FUNC3					+
						)	 	+
						 default=FUNC2

	parm TMAIN_PROC_NAME type=string count=0:1 default=--

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create common_subpdf.hlp
COMMON_SUBPDF.COM - help information

This COM-file was developed in order to provide various sub-PDF's which can
be used by Menu-driven PDF's (i.e. so-called MDF's). The sub-PDF's included
in COMMON.COM are intended to be used commonly by different main Menus,
so they don't have to be included in these program's COM-files. Instead, they
will be included in P2$LIB (UNIX: $P2LIB) so they can be used by everybody.
The programmer has to use their names in his MDF to invoke the entry points
for the sub-PDF's and he/she has to specify all parameters used in the
sub-PDF's in his/her main PDF (which has to be associated with each
Menu-driven PDF).

Currently, only two sub-PDF's which could be of general interest are contained
in COMMON.COM:

COMMON_SAVE_PAR.PDF	-	This sub-PDF specifies an entry point in
				Menu where the user can specify a name for
				the TAE-parameter file which is generated by the
				program (the default name in VICAR programs:
				LAST.PAR). This is useful because in a Menu
				there is no 'save'-command to save a para-
				meter file with a user-specified name (e.g.
				"save proc_name.par").

COMMON_PROC_DONE	-	This sub-PDF is the entry point which is
				selected to execute the program.
$ Return
$!#############################################################################
