$!****************************************************************************
$!
$! Build proc for MIPL module lab2tcl
$! VPACK Version 1.9, Wednesday, March 18, 2015, 17:23:18
$!
$! Execute by entering:		$ @lab2tcl
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
$ write sys$output "*** module lab2tcl ***"
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
$ write sys$output "Invalid argument given to lab2tcl.com file -- ", primary
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
$   if F$SEARCH("lab2tcl.imake") .nes. ""
$   then
$      vimake lab2tcl
$      purge lab2tcl.bld
$   else
$      if F$SEARCH("lab2tcl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lab2tcl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lab2tcl.bld "STD"
$   else
$      @lab2tcl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lab2tcl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lab2tcl.com -mixed -
	-s lab2tcl.f -
	-i lab2tcl.imake -
	-p lab2tcl.pdf -
	-t tstlab2tcl.pdf tstlab2tcl_linux.log tstlab2tcl_sun.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lab2tcl.f
$ DECK/DOLLARS="$ VOKAGLEVE"

!	VICAR Program LAB2TCL
!
!   LAB2TCL copies information from the system and history sections of a VICAR
!   label into user specified TCL variables.
!
!   31 OCT 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
!

	include 'VICMAIN_FOR'
	subroutine main44

	implicit none
	include 'pgminc'	                ! definitions for returning 
						! values to user

	integer*4 MAX_LABEL_ITEM_SIZE, MAX_REQ, MAX_NTASKS, FNDKEY
	parameter (
     +	  MAX_LABEL_ITEM_SIZE = 512,	! max length allowed for keyword value
     +	  MAX_REQ = 20,	! max number of keywords that can be looked up
     +	  MAX_NTASKS = 100,	! max # of tasks that can be grabbed from label
     +	  FNDKEY = -38)	! error from xlinfo: key not found

	logical*4 xvptst		! routine to test for parameter switch
	integer*4 unit,		! i/o unit for input file
     +	  status,		! status from xv* calls
     +	  max_length,		! max value len. found for multivalued keyword
     +	  n_elements,		! num. of values found for multivalued keyword
     +	  num_requests,	        ! number of keywords to search for
     +	  val_length,		! length of a particular keyword's value
     +	  vblock(xprdim),	! vehicle for returning values to user
     +	  i,			! index for current keyword
     +	  i_format,		! integer version of format, below
     +	  srch_which_sect,	! switch indicating search SYS, HIST, or ALL
     +	  retrv_from_sect	! switch indicating read SYS or HIST
	character message*80,	! prep area for messages to user
     +	  format*12,		! format type of keyword value
     +	  lbl_section(2)*8,	! buffer to contain strings SYSTEM & HISTORY
     +	  param_name*3,	        ! for generating names of the variable params Vn
     +	  key_word(MAX_REQ)*8   ! keywords to search for

	integer*4 int_value	! generic label value: integer
	real*4 real_value		! generic label value: real
	character char_value*(MAX_LABEL_ITEM_SIZE) ! generic label value: char

! The subroutine package used by LAB2TCL to access the image label--XL*--is
! very specific. The routine for searching for a keyword in label--XLINFO--
! will only look in one section of the label: SYSTEM or HISTORY. LAB2TCL will
! look in both, and it does this via two XLINFO calls. A search within the
! HISTORY section, however, has an additional restriction: it will only look 
! at the current task. Since the desired capability is to find the keyword no
! matter how many tasks the image has, a special search back through the task
! history is instituted if the search fails in the current task.
!
! This search calls XLHINFO to establish a table of tasks and task instances.
! This table lists the first MAX_NTASKS tasks in order of occurence along with 
! a modifier for each task telling the instance of that task occurence.
!
! For example, consider the following history:
!
!	  #	tasks	instances
!	-------------------------
!	  1	  A	   1
!	  2	  B	   1
!	  3	  A	   2
!	  4	  C	   1
!	  5	  B	   2
!	  6	  A	   3
!	  7	  A	   4
!
! Task "A" occurs 4 times. The first occurence is labeled instance #1; the
! fourth occurence is labeled instance #4.
!
! Once the table is set up, we start working backwards through the table.
! As before, we make XLINFO calls, but we modify them with the HIST (task
! name) and INSTANCE (task instance number) optional arguments. The XLGET
! call used to retrieve a found keyword is also modified with these arguments.
!
! If the number of tasks in the table is the maximum--MAX_NTASKS--we start the 
! backwards search with table entry #MAX_NTASKS, even if it is the current task.
! If the table contains fewer than MAX_NTASKS entries, we start with the second
! to last entry.
!
	integer*4 num_tasks,	! number of tasks found in image label
     +	    use_task,		! which task in table to use for search/retrv
     +	    instances(MAX_NTASKS) ! task instances
	character tasks(MAX_NTASKS)*8 ! task names

        call ifmessage('LAB2TCL - 09 Dec 2012 - rjb - (64-bit)')

	i_format = 0
	retrv_from_sect = 0		!initialize
! Init the label type string buffer
	lbl_section(1) = 'SYSTEM'
	lbl_section(2) = 'HISTORY'

! Get file unit number and open the image file
	call xvunit (unit, 'INP', 1, status,' ')
	call xvopen (unit, status, 'OPEN_ACT', 'SA',' ')

! Create the variable block used to return values through variables. We'll
! initialize the block to abort the program if there's an error.
	call xqini (vblock, xprdim, xabort)

! Get the label section for the search; if we're only searching in one 
! section, srch_which_sect is set to an value that can be used as an 
! index into the label section string buffer lbl_section. If we're searching
! both sections, we'll indicate that with a third, special, value.
! retrv_from_sect is used once we find the keyword: it is set to the section
! in which we found the keyword.
	if (xvptst('ALL')) then
	    srch_which_sect = 3			! SYSTEM & HISTORY
	else if (xvptst('SYSTEM')) then
	    srch_which_sect = 1			! SYSTEM
	    retrv_from_sect = 1
	else
	    srch_which_sect = 2			! HISTORY
	    retrv_from_sect = 2
	endif

! Get the name of the keyword to read
	call xvp ('KEYWORD', key_word, num_requests)

! Loop, searching for each supplied keyword
	do i = 1, num_requests

!     Should we use the results from the last keyword? If first character
!     of the keyword is a blank, yes.
	   if (key_word(i)(1:1) .eq. ' ') then

!	 Insure that this is not the first keyword
	      if (i .eq. 1) then
		 write (message, 10)
 10		 format (' The first keyword in a KEYWORD list ',
     +'cannot be an empty string.')
		    call xvmessage(message,' ')
		    go to 900
	      endif

	   else
!	 Look for the keyword in the label
	      if (srch_which_sect .lt. 3) then
!	     Search SYSTEM or HISTORY
		 call xlinfo (unit, lbl_section(srch_which_sect), 
     +		              key_word(i), format, max_length,
     +                        n_elements, status,' ')

	      else
!	     Search both, starting with SYSTEM (don't bother looking in
!	     HISTORY if the error from the SYSTEM search is "success" or
!	     something other than "key not found"
		call xlinfo (unit, lbl_section(1), key_word(i),
     +		 	     format, max_length, n_elements, status,' ')
		   if (status .eq. 1) then
		      retrv_from_sect = 1
		   elseif (status .eq. FNDKEY) then
		      retrv_from_sect = 2
		      call xlinfo (unit, lbl_section(2), key_word(i),
     +			     format, max_length, n_elements, status,' ')
		   endif
	      endif

!	 Did we find the label?
	      if (status.eq.FNDKEY.and.retrv_from_sect.eq.2) then
!	     Didn't find key, and our last search was in the HISTORY section
!	     This triggers a special backwards search through the image's
!	     task history.

!	     Create the task table, limited in size to MAX_NTASKS
	         num_tasks = MAX_NTASKS
		 call xlhinfo(unit,tasks,instances,num_tasks,status,' ')

!	     If the num_tasks in the table is MAX_NTASKS, start search there,
!	     otherwise, start search at num_tasks-1
		    if (num_tasks .lt. MAX_NTASKS) then
			num_tasks = num_tasks - 1
		    endif

!	     Search!
		    do use_task = num_tasks, 1, -1
			call xlinfo (unit, lbl_section(2), key_word(i),
     +			    format, max_length, n_elements, status,
     +			    'HIST', tasks(use_task), 'INSTANCE',
     +			    instances(use_task),' ')
			if (status .ne. FNDKEY) go to 450
		    enddo

!	     No success; set status so that the case where there's only one
!	     task in the label is handled (if there's only one task, the do 
!	     loop will not be executed and status will equal 1 from the
!	     xlhinfo call; use_task is not used if status=FNDKEY)
		    status = FNDKEY
 450		    continue

		else
!	     Set use_task to 0 to indicate that we're not using the task
!	     table during the xlget call (execution is faster if we can
!	     omit optional arguments)
		    use_task = 0
		endif

		if (status .ne. 1) then
!	     Didn't find key when looking for SYSTEM alone, didn't find key
!	     during special backwards search thru HISTORY section, or error 
!	     encountered during either a SYSTEM or HISTORY search
		    write (message, 
     +			'('' Keyword '',A,'' not found in label.'')')
     +			key_word(i)
		    call xvmessage (message,' ')
		    go to 900

		else
!	 Read the desired label. We'll make a distinction among INT, REAL, and
!	 STRING for the xlget call because we want to return the variable using
!	 its proper type. If the type isn't INT or REAL, we'll return it as a
!	 STRING to protect the software from future types (such as BINARY).
!	 We'll distinguish between task table use and non-use for the xlget
!	 call for efficiency.
		  if (format(1:3) .eq. 'INT') then
		     if (use_task .eq. 0) then
		        call xlget (unit,lbl_section(retrv_from_sect), 
     +			            key_word(i), int_value, status,
     +                              'FORMAT','INT',' ')
		     else
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i), int_value, status, 'HIST',
     +			     tasks(use_task), 'INSTANCE',
     +			     instances(use_task),'FORMAT','INT',' ')
		     endif
		     i_format = 1

		  else if (format(1:3) .eq. 'REA') then
		     if (use_task .eq. 0) then
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i), real_value, status,
     +                       'FORMAT','REAL',' ')
		     else
		        call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i), real_value, status, 'HIST',
     +			     tasks(use_task), 'INSTANCE',
     +			     instances(use_task),'FORMAT','REAL',' ')
		     endif
			i_format = 2
		  else
		     if (use_task .eq. 0) then
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i),char_value,status, 'FORMAT',
     +			     'STRING', 'LENGTH', val_length,' ')
		     else
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i),char_value,status,'FORMAT',
     +			     'STRING', 'LENGTH', val_length,'HIST',
     +			     tasks(use_task), 'INSTANCE',
     +			     instances(use_task),' ')
		     endif
		     i_format = 3
		  endif
		endif
	    endif

!     Generate the name of the parameter for this value: one of the set
!     {V1, V2, V3,..., V20}
	    if (i .lt. 10) then
		write (param_name, '(''V'',I1,'' '')') i
	    else
		write (param_name, '(''V'',I2)') i
	    endif

!     Return what we have: either a newly read value, or the value from the
!     last read; the write is to the variable pointed to by the parameter 
!     whose name is param_value and we're adding 1 value.

	    go to (610, 620, 630), i_format

!     Integer output
 610	    call xqintg (vblock, param_name, 1, int_value, xadd, status)
	    go to 680

!     Real output
 620	    call xqreal (vblock, param_name, 1, real_value, xadd, status)
	    go to 680

!     Character output
 630	    call xqstr (vblock, param_name, 1, char_value(1:val_length), 
     +		xadd, status)
	    go to 680

 680	    continue
	enddo  ! i from 1 to num_requests


! Send the variable block to the user
	call xvqout (vblock, status)

! Close input file
	call xvclose (unit, status,' ')
	return

! Error: close input file and terminate program abruptly
 900	call xvclose (unit, status,' ')
	call abend

	end	! main44

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lab2tcl.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lab2tcl

   To Create the build file give the command:

		$ vimake lab2tcl			(VMS)
   or
		% vimake lab2tcl			(Unix)


************************************************************************/


#define PROGRAM	lab2tcl
#define R2LIB

#define MODULE_LIST lab2tcl.f

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
$ create lab2tcl.pdf
PROCESS HELP=*
    local dumint	type=integer

    parm INP		type=string
    parm V1		type=name default=dumint
    parm V2		type=name default=dumint
    parm V3		type=name default=dumint
    parm V4		type=name default=dumint
    parm V5		type=name default=dumint
    parm V6		type=name default=dumint
    parm V7		type=name default=dumint
    parm V8		type=name default=dumint
    parm V9		type=name default=dumint
    parm V10		type=name default=dumint
    parm V11		type=name default=dumint
    parm V12		type=name default=dumint
    parm V13		type=name default=dumint
    parm V14		type=name default=dumint
    parm V15		type=name default=dumint
    parm V16		type=name default=dumint
    parm V17		type=name default=dumint
    parm V18		type=name default=dumint
    parm V19		type=name default=dumint
    parm V20		type=name default=dumint
!    parm KEYWORD	type=(string,8) count=(1:20)
    parm keyword	type=(string,8) count=(1:20)
    parm type		type=keyword valid=(system,history,all) default=history

!# annot function="VICAR Procedure Generation"
!# annot keywords=(system,history,INP,V1,linc,integer,real,string,nl,ns,+
!#  format,sinc,"Output Parameter","TAE variable")
END-PROC
.TITLE
Copies of VICAR label items to TAE variable

.HELP
PURPOSE
"lab2tcl" copies information from the system and history sections of a VICAR
label into user specified TCL variables.


EXECUTION

    procedure
    local lineInc real
    body
	lab2tcl  INP=file.img  V1=lineInc  KEYWORD=linc
    end-proc

In this example, "lab2tcl" searches for keyword "linc" in the HISTORY section
of image file "file.img"'s label (note that the program requires a variable
definition prior to use). If the keyword is found, its value is written into
the variable "lineInc"; if the keyword is not found, the message:
    Keyword LINC not found in label.
is displayed; the program will stop without searching for other specified
keywords. The label's value is returned as either an integer, a real, or a
string; label-add sets the value type, e.g.: 

	Integer		Real		String
	-------		----		------
	 787		787.		'787'
	 +3		+3e0		 '3'
	 -45		-45.78		'negative forty five.'

The variable whose name is given to the V1 parameter should be declared to be
of the same type as the expected value. 

Some of the label's information is stored in the SYSTEM section of the image
file. lab2tcl looks in the SYSTEM section if the 'SYSTEM switch is specified: 

    procedure
    local numlines integer
    body
	lab2tcl  INP=file.img  v1=numlines  KEYWORD=nl 'SYSTEM
    end-proc

In this example, "lab2tcl" searches for keyword "nl" in the SYSTEM section
of image file "file.img"'s label. "lab2tcl" will look in both sections of the
image file--in the order SYSTEM,HISTORY--if the 'ALL switch is specified:

    procedure
    local sampinc real
    body
	lab2tcl  INP=file.img  v1=sampinc  KEYWORD=sinc 'ALL
    end-proc

In this example, lab2tcl searches for keyword "sinc" in the SYSTEM section
first, then, if it wasn't found, in the HISTORY section. The "not found"
message will appear if the keyword is not found in the HISTORY section, either.

More than one keyword can be read at a time, setting a corresponding number
of variables (note that separate parameters are used for each of the variable
names; the parameters are numbered v1 to v20):

    procedure
    local numl integer
    local nums integer
    local fmt string
    local linc real
    local sinc real
    body
	lab2tcl  INP=file.img  v1=numl v2=nums v3=fmt v4=linc v5=sinc  +
	    KEYWORD=(nl,ns,format,linc,sinc)  'ALL
    end-proc

"lab2tcl" looks for each keyword individually, so that, in this example, it
will first look for keyword "nl" in SYSTEM then in HISTORY, then it will look
for "ns" in SYSTEM and HISTORY, and so on. The first missing keyword
terminates "lab2tcl" for all keyword retrieval.

If an empty string is specified for one of the keywords other than the first,
the corresponding variable will be assigned the value from the last retrieved 
variable:

    procedure
    local nl1 integer
    local nl2 integer
    local ns1 integer
    local ns2 integer
    body
	lab2tcl  INP=file.img  v1=nl1 v2=nl2 v3=ns1 v4=ns2  +
	    KEYWORD=(nl,"",ns,"") 'SYSTEM
    end-proc

In this example, variables nl1 and nl2 are assigned the value found for keyword
"nl", while ns1 and ns2 are assigned the value associated with "ns". Variable 
names cannot be omitted. The message:
    The first keyword in a KEYWORD list cannot be an empty string.
is displayed if an empty string is supplied for the first keyword; the program
will stop without searching for other specified keywords.


RESTRICTIONS
 1. A maximum of 20 variables can be returned at a time.
 2. The parameter KEYWORD must be explicitly mentioned, as in "KEYWORD=ns", 
    since each variable has its own parameter (V1,V2,V3,...,V20).
 3. Only the first value in a multivalued keyword is returned; this 
    restriction corresponds to label-add, which will only permit the
    first value to be written.
 4. If the keyword appears more than once in a section, the keyword 
    in the "current task" is used, where the "current task" is the
    last Task entry depicted in the label-list display.
 5. During a HISTORY search, "lab2tcl" looks at the current task for a
    keyword. If the keyword is not found, the program then scans
    backwards through the tasks starting with the second-to-last task.
    "lab2tcl" is limited to 100 tasks, however, including the current task.


WRITTEN BY:		M. K. Tschudi	July 14, 1987
COGNIZANT PROGRAMMER:	R. J. Bambery
REVISIONS:

    1994-10-31 AMS (CRI) - Made portable for UNIX
    2010-01-19 R. J. Bambery - Revised pdf file to remove 80 character
               limit on input file
    2012-06-05 R. J. Bambery - fix uninitialized variable
    2012-06-06 R. J. Bambery - fixed TYPE parm to use 20 vals
    2012-12-09 R. J. Bambery - fixed uninitialized variable i_format

.LEVEL1
.VARIABLE INP
Input image whose label value
is sought
.VARIABLE V1
Parameters V1,V2,...,V20 each
contain the name of a TCL 
variable that is to receive a
value corresponding to a KEYWORD
V1 corresponds to the first
KEYWORD
.VARIABLE V2
The variable to receive the
value for the second KEYWORD
.VARIABLE V3
The variable to receive the
value for the third KEYWORD
.VARIABLE V4
The variable to receive the
value for the fourth KEYWORD
.VARIABLE V5
The variable to receive the
value for the fifth KEYWORD
.VARIABLE KEYWORD
One or more names of label
entries; each label entry's 
associated value is returned to
the corresponding VAR
.VARIABLE TYPE
Switch indicating if the 
HISTORY (default) or SYSTEM
portion of a label is to be
searched for KEYWORD, or if
both SYSTEM & HISTORY should
be searched (ALL)

.LEVEL2
.VARIABLE INP
    This parameter is used to specify a labeled VICAR image to examine.


    Type:	string, 80 characters
    Count:	1
    Valid:	any
    Default:	none

.VARIABLE V1
    The purpose of "lab2tcl" is to transfer label information into a TCL
    variable; the Vn parameters are used to specify the variable's name.
    If the specified keyword is found in the image's label, then the
    variable is set to the keyword's associated value; if the keyword is
    not found, then the variable is undefined. 

    Label values are typed: integer, real, or string. "lab2tcl" returns
    the value using the value type; the variable should be declared to
    be of the same type as the expected value. See either this program's
    EXECUTION section or program label-add for a description of how

    Parameter names "V1", "V2", etc., may be omitted from the calling 
    line. You cannot skip a parameter in the sequence, for example:
	V1=a V2=b V4=c
    This usage will lead to an error.

    The number of specified keywords determines how many variables are
    set. In the example:
	procedure
	local a integer
	local b integer
	local c integer
	local d integer
	body
	! Create an image and add some info to it
	    gen tempimage
	    label-add tempimage items="a=7 b=14 c=1987 d=3098470"
	! Read the information
	    lab2tcl  INP=tempimage  V1=a V2=b V3=c V4=d  KEYWORD=(a,b)
	    write "&a/&b"
	    write "&c  (&d)"
	end-proc
    only the variables for parameters V1 and V2 are set; c and d are
    undefined.


    Example:
	Look for keywords a, b, c, d, e, f, & g (the "lab2tcl" produces 
	the same result):
	    procedure
	    local a integer
	    local b string
	    local c string
	    local d integer
	    local e real
	    local f integer
	    local g real
	    refgbl $prompt
	    body
	    ! Create an image and add some info to it
		gen tempimage
		label-add tempimage  +
		    items="a=1,b='ABC',c='DEF',d=44,e=42.319,f=312,g=5.2e4"
		label-add tempimage items="prompt='Test'"
	    ! Read the information
		lab2tcl INP=tempimage V1=a V2=b V3=c V4=d V5=e V6=f V7=g  +
		    V8=$prompt  KEYWORD=(a,b,c,d,e,f,g,prompt)
		write "a=&a, b=&b, c=&c, d=&d, e=&e,"
		write "f=&f, g=&g"
		lab2tcl tempimage  a b c d e f g  KEYWORD=(a,b,c,d,e,f,g)
		write "a=&a, b=&b, c=&c, d=&d, e=&e,"
		write "f=&f, g=&g"
	    end-proc


    Type:	name
    Count:	1
    Valid:	any
    Default:	none
.VARIABLE V2
    See parameter V1 for more information.
.VARIABLE V3
    See parameter V1 for more information.
.VARIABLE V4
    See parameter V1 for more information.
.VARIABLE V5
    See parameter V1 for more information.
.VARIABLE KEYWORD
    Label information is stored in (keyword,value) pairs. This parameter
    is used to enter the names of the keywords of interest. Keywords are 
    stored in uppercase characters, so upper or lower case characters
    can be used.

    Examples:
	Look for keyword "acreage" (the "lab2tcl" produces the same
	result):
	    procedure
	    local acres real
	    body
	    ! Create an image and add some info to it
		gen tempimage
		label-add tempimage items="acreage=367483.3"
	    ! Read the information
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=acres KEYWORD=Acreage
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=acres KEYWORD=ACREAGE
		write "Acres = &acres"
	    end-proc


    Type:	string, 8 characters
    Count:	1 to 20
    Valid:	any, including empty string ("")
    Default:	none
.VARIABLE TYPE
    A label has at least two categories of information: SYSTEM and
    HISTORY. The SYSTEM category contains keywords such as:
	Integer-valued		String-valued
	name	example		name	example
	---------------		---------------
	LBLSIZE    234		FORMAT	'BYTE'
	BUFSIZ	  2000		TYPE	'IMAGE'
	DIM	     3		ORG	'BSQ'
	EOL	     1
	RECSIZE	    13
	NL	     8
	NS	    13
	N1	    13
	N2	     8
	N3	     0
	N4	     0
	NBB	     0
	NLB	     0

    while the HISTORY category contains keywords entered by tasks as well
    as the label-add program. "lab2tcl" will look in only one category or
    in both categories; this switch permits you to specify lab2tcl's actions.

    Examples:
	Using HISTORY category only (the "lab2tcl" produces the same
	result):
	    procedure
	    local acres real
	    body
	    ! Create an image and add some info to it
		gen tempimage
		label-add tempimage items="acreage=+33.289e9"
	    ! Read the information
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage 'HISTORY
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage TYPE=HISTORY
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage TYPE=history
		write "Acres = &acres"
	    end-proc

	Using SYSTEM category only (the "lab2tcl" produces the same
	result):
	    procedure
	    local numlines integer
	    body
	    ! Create an image, then read some of its info
		gen tempimage
		lab2tcl INP=tempimage V1=numlines KEYWORD=nl 'SYSTEM
		write "image contains &numlines lines"
		lab2tcl INP=tempimage V1=numlines KEYWORD=nl TYPE=SYSTEM
		write "image contains &numlines lines"
		lab2tcl INP=tempimage V1=numlines KEYWORD=nl TYPE=system
		write "image contains &numlines lines"
	    end-proc

	Using first SYSTEM then HISTORY categories (the first two "lab2tcl"
	uses happen to produce the same result as the HISTORY only example,
	while the third use would be the same as the SYSTEM only example):
	    procedure
	    local acres real
	    local numlines integer
	    body
	    ! Create an image and add some info to it
		gen tempimage
		label-add tempimage items="acreage=-748.32"
	    ! Read the information
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage 'ALL
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=acres KEYWORD=acreage 'all
		write "Acres = &acres"
		lab2tcl INP=tempimage V1=numlines KEYWORD=nl 'ALL
		write "image contains &numlines lines"
	    end-proc


    Type:	keyword
    Count:	1
    Valid:	SYSTEM, HISTORY, ALL
    Default:	HISTORY
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlab2tcl.pdf
procedure
! Regular tests
    local ival integer
    local rval real
    local cval string
    local sum_1 string count=2
    local sum_2 integer count=5
    local i1 integer
    local i2 integer
    local i3 integer
    local i4 integer
    local i5 integer
    local c1 string
    local c2 string

! Mass read test
    local a integer         !LBLSIZE
    local b string          !FORMAT
    local c string          !TYPE
    local d integer         !BUFSIZ
    local e integer         !DIM
    local f integer         !EOL  
    local g integer         !RECSIZE        
    local h string          !ORG
    local i integer         !NL
    local j integer         !NS
    local k integer         !NB
    local l integer         !NBB
    local m integer         !NLB
    local n real            !R
    local o string          !YUMMY
    local p integer         !I
    local q string          !NEWPROMP
    local r string          !C
    local s real            !SINC
    local t real            !LINC

    refgbl $echo
    refgbl $prompt
    refgbl $autousage
! Jun 24, 2012 - RJB
! TEST SCRIPT FOR LAB2TCL
! tests IBIS tabular files
!
! Vicar Programs:
!       gen label-add label-list
!
! External Programs:
!   <none> 
!
! Parameters:
!   <none>
!
! Requires NO external test data: 
!
    body
    let $echo="no"
        let $autousage="none"
    let _onfail="stop"
    let $echo="yes"

! Create an image with some history labels added
    gen image nl=8 ns=13
    label-add image items="c='a string', i=5, r=1.452 newpromp='Hi!'"
    label-list image 'dump
             
! Read the integer, real, and character labels
    lab2tcl image v1=ival keyword=i
    let $echo="no"
    write "Integer value from image: (&ival)"
    let $echo="yes"
    lab2tcl image v1=rval keyword=r 'HISTORY
    let $echo="no"
    write "Real value from image: (&rval)"
    let $echo="yes"
    lab2tcl image v1=cval keyword=c
    let $echo="no"
    write "String value from image: (&cval)"

! Redefine a global variable using lab2tcl
    let cval = $prompt
    let $echo="yes"
    lab2tcl image $prompt keyword=newpromp 'History
    let $echo="no"
    write "New prompt is (&$prompt)"
    let $prompt = cval
    let _onfail="continue"
    write "**************************************************"
    write "Next call is ABEND - system label from history area"
    write "**************************************************"
    let $echo="yes"
! Try to read a system label from the history area; expect an error
    
    lab2tcl image v1=ival keyword=nl 

    let _onfail="stop"
! Read labels while looking in both sections
    lab2tcl image ival keyword=i 'all
    let $echo="no"
    write "Integer value from image: (&ival)"
    let $echo="yes"
        lab2tcl image ival keyword=nl 'all
    let $echo="no"
    write "Number of lines is (&ival)"
    let $echo="yes"
! Read several values at a time
    lab2tcl image i1 i2 i3 i4 i5 keyword=(dim,nl,ns,nb,bufsiz) 'system
    let sum_2 = (i1,i2,i3,i4,i5)
    let $echo="no"
    write "Image dim,nl,ns,nb,bufsiz = &sum_2"
    let $echo="yes"
    lab2tcl image c1 c2 keyword=(format,type) 'System
    let sum_1 = (c1,c2)
    let $echo="no"
    write "Image format,type = &sum_1"
    let _onfail="continue"
    write "**************************************************"
    write "Next call is ABEND - Cannot have blank initial key"
    write "**************************************************"

    let $echo="yes"
! Fill a list with repeat values, beginning with an error (blank initial key)
    lab2tcl image i1 i2 i3 i4 keyword=("","",bufsiz,"") 'system
    let _onfail="stop"
    lab2tcl image i1 i2 i3 i4 i5 keyword=(dim,"","",bufsiz,"") 'all
    let sum_2 = (i1,i2,i3,i4,i5)
    let $echo="no"
    write "dim*3, bufsiz*2: &sum_2"
    let $echo="yes"
! Add a new task with some new keywords and some repeat keywords, then do a 
! massive read. We should get the second versions of the repeated keywords.
    label-add image image2  +
        items="r=448e8 yummy='avocado',i=1984, newpromp='Hello.'"
    label-list image2 'dump
    let _onfail="continue"
    lab2tcl image2   +
            a b c d e f g h i j k l m n o p q r s t  +
        keyword=(LBLSIZE,FORMAT,TYPE,BUFSIZ,DIM,EOL,RECSIZE,ORG, +
        NL,NS,NB,NBB,NLB,R,YUMMY,I,NEWPROMP,C,SINC,LINC) +
        type=all
    let $echo="no"
    write "System info (LBLSIZE to NB, NBB, NLB (see help)):"
    write "    &a,&b,&c,&d,&e,"
    write "    &f,&g,&h,&i,&j,"
    write "    &k,&l,&m"
    write "History info from first task (SINC,LINC,C):"
    write "    &s,&t,&r"
    write "History info from second task (R,YUMMY,I,NEWPROMP):"
    write "    &n,&o,&p,&q"
end-proc

$!-----------------------------------------------------------------------------
$ create tstlab2tcl_linux.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

    gen image nl=8 ns=13
Beginning VICAR task gen
GEN Version 6
GEN task completed
    label-add image items="c='a string', i=5, r=1.452 newpromp='Hi!'"
Beginning VICAR task label
Keyword c added
Keyword i added
Keyword r added
Keyword newpromp added
    label-list image 'dump
Beginning VICAR task label
************************************************************
 
        ************  File image ************
LBLSIZE=338
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=131072
DIM=3
EOL=1
RECSIZE=13
ORG='BSQ'
NL=8
NS=13
NB=1
N1=13
N2=8
N3=1
N4=0
NBB=0
NLB=0
HOST='X86-LINUX'
INTFMT='LOW'
REALFMT='RIEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
TASK='GEN'
USER='wlb'
DAT_TIM='Wed Mar 18 17:02:31 2015'
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
C='a string'
I=5
R=1.452
NEWPROMP='Hi!'
 
************************************************************
    lab2tcl image v1=ival keyword=i
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Integer value from image: (5)
    lab2tcl image v1=rval keyword=r 'HISTORY
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Real value from image: (1.452000021935e+00)
    lab2tcl image v1=cval keyword=c
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
String value from image: (a string)
    lab2tcl image $prompt keyword=newpromp 'History
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
New prompt is (Hi!)
**************************************************
Next call is ABEND - system label from history area
**************************************************
    lab2tcl image v1=ival keyword=nl
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
 Keyword nl       not found in label.
 ** ABEND called **
continue
    let _onfail="stop"
    lab2tcl image ival keyword=i 'all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Integer value from image: (5)
        lab2tcl image ival keyword=nl 'all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Number of lines is (8)
    lab2tcl image i1 i2 i3 i4 i5 keyword=(dim,nl,ns,nb,bufsiz) 'system
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let sum_2 = (i1,i2,i3,i4,i5)
    let $echo="no"
Image dim,nl,ns,nb,bufsiz = (3,8,13,1,131072)
    lab2tcl image c1 c2 keyword=(format,type) 'System
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let sum_1 = (c1,c2)
    let $echo="no"
Image format,type = (BYTE,IMAGE)
**************************************************
Next call is ABEND - Cannot have blank initial key
**************************************************
    lab2tcl image i1 i2 i3 i4 keyword=("","",bufsiz,"") 'system
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
 The first keyword in a KEYWORD list cannot be an empty string.
 ** ABEND called **
continue
    let _onfail="stop"
    lab2tcl image i1 i2 i3 i4 i5 keyword=(dim,"","",bufsiz,"") 'all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let sum_2 = (i1,i2,i3,i4,i5)
    let $echo="no"
dim*3, bufsiz*2: (3,3,3,131072,131072)
    label-add image image2   +
        items="r=448e8 yummy='avocado',i=1984, newpromp='Hello.'"
Beginning VICAR task label
Keyword r added
Keyword yummy added
Keyword i added
Keyword newpromp added
    label-list image2 'dump
Beginning VICAR task label
************************************************************
 
        ************  File image2 ************
LBLSIZE=494
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=131072
DIM=3
EOL=1
RECSIZE=13
ORG='BSQ'
NL=8
NS=13
NB=1
N1=13
N2=8
N3=1
N4=0
NBB=0
NLB=0
HOST='X86-LINUX'
INTFMT='LOW'
REALFMT='RIEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
TASK='GEN'
USER='wlb'
DAT_TIM='Wed Mar 18 17:02:31 2015'
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
C='a string'
I=5
R=1.452
NEWPROMP='Hi!'
TASK='LABEL'
USER='wlb'
DAT_TIM='Wed Mar 18 17:02:31 2015'
R=4.48e+10
YUMMY='avocado'
I=1984
NEWPROMP='Hello.'
 
************************************************************
    let _onfail="continue"
    lab2tcl image2    +
            a b c d e f g h i j k l m n o p q r s t   +
        keyword=(LBLSIZE,FORMAT,TYPE,BUFSIZ,DIM,EOL,RECSIZE,ORG,  +
        NL,NS,NB,NBB,NLB,R,YUMMY,I,NEWPROMP,C,SINC,LINC)  +
        type=all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
System info (LBLSIZE to NB, NBB, NLB (see help)):
    494,BYTE,IMAGE,131072,3,
    1,13,BSQ,8,13,
    1,0,0
History info from first task (SINC,LINC,C):
    1.000000000000e+00,1.000000000000e+00,a string
History info from second task (R,YUMMY,I,NEWPROMP):
    4.480000000000e+10,avocado,1984,Hello.
$!-----------------------------------------------------------------------------
$ create tstlab2tcl_sun.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

    gen image nl=8 ns=13
Beginning VICAR task gen
GEN Version 6
GEN task completed
    label-add image items="c='a string', i=5, r=1.452 newpromp='Hi!'"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword c added
Keyword i added
Keyword r added
Keyword newpromp added
    label-list image 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File image ************
LBLSIZE=364
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=1
RECSIZE=13
ORG='BSQ'
NL=8
NS=13
NB=1
N1=13
N2=8
N3=1
N4=0
NBB=0
NLB=0
HOST='SUN-SOLR'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='GEN'
USER='wlb'
DAT_TIM='Wed Mar 18 17:05:30 2015'
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
C='a string'
I=5
R=1.452
NEWPROMP='Hi!'
 
************************************************************
    lab2tcl image v1=ival keyword=i
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Integer value from image: (5)
    lab2tcl image v1=rval keyword=r 'HISTORY
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Real value from image: (1.452000021935e+00)
    lab2tcl image v1=cval keyword=c
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
String value from image: (a string)
    lab2tcl image $prompt keyword=newpromp 'History
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
New prompt is (Hi!)
**************************************************
Next call is ABEND - system label from history area
**************************************************
    lab2tcl image v1=ival keyword=nl
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
 Keyword nl       not found in label.
 ** ABEND called **
continue
    let _onfail="stop"
    lab2tcl image ival keyword=i 'all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Integer value from image: (5)
        lab2tcl image ival keyword=nl 'all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
Number of lines is (8)
    lab2tcl image i1 i2 i3 i4 i5 keyword=(dim,nl,ns,nb,bufsiz) 'system
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let sum_2 = (i1,i2,i3,i4,i5)
    let $echo="no"
Image dim,nl,ns,nb,bufsiz = (3,8,13,1,24576)
    lab2tcl image c1 c2 keyword=(format,type) 'System
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let sum_1 = (c1,c2)
    let $echo="no"
Image format,type = (BYTE,IMAGE)
**************************************************
Next call is ABEND - Cannot have blank initial key
**************************************************
    lab2tcl image i1 i2 i3 i4 keyword=("","",bufsiz,"") 'system
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
 The first keyword in a KEYWORD list cannot be an empty string.
 ** ABEND called **
continue
    let _onfail="stop"
    lab2tcl image i1 i2 i3 i4 i5 keyword=(dim,"","",bufsiz,"") 'all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let sum_2 = (i1,i2,i3,i4,i5)
    let $echo="no"
dim*3, bufsiz*2: (3,3,3,24576,24576)
    label-add image image2   +
        items="r=448e8 yummy='avocado',i=1984, newpromp='Hello.'"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword r added
Keyword yummy added
Keyword i added
Keyword newpromp added
    label-list image2 'dump
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File image2 ************
LBLSIZE=533
FORMAT='BYTE'
TYPE='IMAGE'
BUFSIZ=24576
DIM=3
EOL=1
RECSIZE=13
ORG='BSQ'
NL=8
NS=13
NB=1
N1=13
N2=8
N3=1
N4=0
NBB=0
NLB=0
HOST='SUN-SOLR'
INTFMT='HIGH'
REALFMT='IEEE'
BHOST='VAX-VMS'
BINTFMT='LOW'
BREALFMT='VAX'
BLTYPE=''
COMPRESS='NONE'
EOCI1=0
EOCI2=0
TASK='GEN'
USER='wlb'
DAT_TIM='Wed Mar 18 17:05:30 2015'
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
C='a string'
I=5
R=1.452
NEWPROMP='Hi!'
TASK='LABEL'
USER='wlb'
DAT_TIM='Wed Mar 18 17:05:31 2015'
R=4.48e+10
YUMMY='avocado'
I=1984
NEWPROMP='Hello.'
 
************************************************************
    let _onfail="continue"
    lab2tcl image2    +
            a b c d e f g h i j k l m n o p q r s t   +
        keyword=(LBLSIZE,FORMAT,TYPE,BUFSIZ,DIM,EOL,RECSIZE,ORG,  +
        NL,NS,NB,NBB,NLB,R,YUMMY,I,NEWPROMP,C,SINC,LINC)  +
        type=all
Beginning VICAR task lab2tcl
LAB2TCL - 09 Dec 2012 - rjb - (64-bit)
    let $echo="no"
System info (LBLSIZE to NB, NBB, NLB (see help)):
    533,BYTE,IMAGE,24576,3,
    1,13,BSQ,8,13,
    1,0,0
History info from first task (SINC,LINC,C):
    1.000000000000e+00,1.000000000000e+00,a string
History info from second task (R,YUMMY,I,NEWPROMP):
    4.480000000000e+10,avocado,1984,Hello.
$ Return
$!#############################################################################
