!	Change log:
!
!	01-03-87	Make TAE 1.4 UNIX and VMS versions agree
!			(based on JM update to UNIX) and correct
!			some spelling...nhe
!	26-May-87	Merge Facelift additions, run spell on whole thing...ljn
!	15-Jul-87	Add messages for Tutor SELECT...gc
!       26-Sep-87       Add QUALDEL and GBLREDEF...tpl
!       28-Sep-87       Fix MAXDEFC User Action...rww
!       28-Sep-87       Change QUALDEL explanation wording...rww
!       24-nov-87       Merge in 2.2 messages...tpl
!	07-feb-88	UNRECTTY: Unknown terminal type...ljn
!	19-apr-88	NOXLIB...ljn
!	26-aug-88	New 3.11 stuff...nhe
!	07-oct-88	Add NOFILE...nhe
!	09-feb-89	New errors for old PAR files...palm
!       18-dec-89       Add FUNUNAV for function unavailable...tpl
!       05-jan-90       Correct TNOQUAL and manual name...tpl
!	23-may-90	Remove RCJM stuff...ljn
!	02-nov-90	Remove FACELIFT stuff...ljn
!
.KEY ABNSHELL
Explanation:
.if UNIX

                The last command executed by the UNIX/shell,
                invoked  via the USH command from TAE, has
                terminated abnormally.


User action:

                Resubmit the shell command correctly.
.ifend
.KEY ACCERR
Explanation:

    		Defined ACCESS values are:

    		    IN 	    - the file exists
    		    OUT     - the file will be created
    		    INOUT   - the file exists and can be 
    			      written to
    		    NOCHECK - no check is to be made


User action:

    		Correct the ACCESS specification according to one
    		of the options listed above.
.KEY ACCNOTALL 
Explanation:

    		ACCESS is only allowed for FILE variables.


User action:

    		Change the variable type or delete the ACCESS clause.
.KEY AMBIGPAR
Explanation:

    		The abbreviation used for a parameter identifies
    		more than one parameter for this command.


User Action:

    		Use more characters in the name of the parameter.
		Use tutor to determine the full name for the parameter.
                
.KEY AMBIGSUB
Explanation:

    		The abbreviation used for a subcommand identifies
    		more than one subcommand for this command.


User Action:

    		Use more characters in the name of the subcommand.
		If you do not know the correct name of the 
		subcommand, request the tutor subcommand display:

			TUTOR commandname-
.KEY ARIERR
Explanation:

    		One of the following occurred:

    		-  The result of an integer addition or 
    		   multiplication is greater than the
    		   greatest integer that can be held
    		   in the host system.

    		-  The result of a floating point addition 
    		   or multiplication is greater than the
    		   largest floating point number that can 
	           be held in the host system.
	
    		-  The result of a floating point division
    		   requires too much precision for 
    		   floating point numbers in the
    		   host system.


User action:

    		See local system documentation or the System
    		Manager to determine the possible size of
    		numbers on your system, and correct the
    		command or procedure accordingly.
.KEY ATROOT
Explanation:

    		The root menu is the currently displayed
    		menu.


User action:

    		None required.
.KEY BADABB
Explanation:

    		An abbreviation was specified (using the abc*def 
    		form) in which the asterisk was the first character.
    		You must specify at least one character before the
    		asterisk.


User action:

    		Correct the input command accordingly.
.KEY BADCMD
Explanation:

    		The specified command is not available in the
    		mode in which it was used.  It may or may not
    		be available in some other mode.

    		The TCL command modes are normal mode and interrupt
    		mode (entered when you type the attention sequence
                to interrupt an executing proc), and dynamic prompt
                mode (entered when you type two escapes while prompted
                for additional input to a program or proc).



User action:
    		Type "HELP" while in a given mode to determine 
    		the commands available in that mode.
.KEY BADESC
Explanation:

    		BREAK and NEXT commands are only valid inside a loop.


User action:

    		Correct the PDF to delete the BREAK or NEXT.  Place
    		it inside a loop as appropriate.

		Descriptions of BREAK and NEXT are available using the
		TCL commands: 

			HELP BREAK
			HELP NEXT
                
.KEY BADFILE
Explanation:

    		The named file cannot be found or the name of the
    		file is not correct for the local operating
    		system.


User action:

    		Determine if the named file has correct spelling.
    		If it does, see the TAE System Manager or Appendix A
    		of the TAE user's manual to check for the format 
		of correct file names on your system.
    		                
.KEY BADHLPDIR
Explanation:
    		The help file contains an unrecognized directive, 
    		that is, an unrecognized word following 
    		a period in column one.  Recognized directives
    		in help files are:

    			.ELSEIF
    			.END
    			.HELP
    			.IF
    			.IFEND
    			.INCLUDE
    			.LEVEL1
    			.LEVEL2
    			.PAGE
    			.SUBCMD
    			.TITLE
    			.VARIABLE or .VAR

User action:

    		Examine the help file and ensure that all
		directives are valid help directives.
.KEY BADKEY
Explanation:

    		The proper format for a message key is "xxx-yyy"
    		and should be typed exactly as shown in the square 
		brackets ([ and ]) of the error message.


User action:

		Type the key as indicated above.
.KEY BADLETSUBS
Explanation:

    		The subscript in the LET command has one of the
    		following faults:

    	    (1) It is larger than the largest subscript allowed for
    		the variable.

	    (2) It is more than one greater than the current
		value count of the variable.

    	    (2) It is not a positive integer.

    	    (3) It is multi-valued.

User action:
    		Determine which of the above faults is the cause
    		of the error and fix the command accordingly.
.KEY BADLET 
Explanation:

    		The valid form for the LET is

    		    LET variable = expression

    		See the section in the TAE Application Programmer's
		Manual on expressions for more details.

User action:

    		Correct the command to use the proper form.                
.KEY BADMENCMD
Explanation:

    		The command is not a known menu
    		command.


User action:

    		Correct the command.  Type "HELP" while
    		in menu mode for a list of menu commands.
.KEY BADMENDIR
Explanation:

    		The menu definition file contains an
    		unrecognized directive.  (A word is
    		interpreted as a menu directive if it
    		follows a period in column one in a
    		menu definition file.)

    		The menu directives are:

    		     .COMMAND
    		     .TITLE
    		     .PROC
    		     .MENU
    		     .HELP
    		     .PAGE    		
.page
User action:

    		Correct the menu file to use only the directives
    		listed above.
.KEY BADOP
Explanation:

    		An operator was used inappropriately:

    		- The +, /, -, and * operators are restricted to
    		  real and integer data types.

    		- The // operator (concatenation) is restricted to
    		  string data types.

    		- The NOT, AND, and OR operators are restricted
    		  to real and integer data types.
 		
    		- The >, <, >=, and <= operators are restricted
    		  to real and integer data types.


User action:

    		Determine the cause of the incorrect usage
    		and correct the command.
.KEY BADPARENT
Explanation:
    		In a WINDOW-CREATE operation, a parent
    		window was specified that doesn't exist.
.KEY BADPARFILE
Explanation:

		An attempt was made to execute a PAR file
		but the PAR file could not be recognized
		(that is, it did not have the proper 
		identifying flag).


User action:

		Try "disp-parfile" to display the contents
		of the file.  A probable cause is that the 
		file was copied over.
.KEY BADPFILE
Explanation:

    		Possible causes:

    	    (1) A parameter file read during a RESTORE
    		or dynamic parameter operation does not have
    		a valid format for a parameter file.

    	    (2) The parameter file could not be opened.

    	    (3) There was an error while reading the parameter
    		file.

	    (4) The file was created by an older version 
    		of TAE.   Type "HELP CONVERT" or "TUTOR CONVERT"
		for information on converting such files.
User action:

    		Check the spelling of the file name.  
    		If the spelling is correct, determine 
    		whether the named file was created
    		by a SAVE operation or by a process.  If
    		by a process, determine if the process
    		is properly using the XQWRTB call.

    		If the file could not be opened then
    		the file may not exist, or you may
    		not have the privileges to read the file.

    		If there was an error while reading the
    		parameter file, then the file may be
    		corrupted; see the TAE System Manager.
.KEY BADTITLE
Explanation:

    		The title block--the text following .TITLE--
    		in the help file or proc definition file
    		is incorrect. In general, the title is
    		too long to fit on the screen.


User action:

    		Correct the text.
.KEY BADTUTCMD
Explanation:

    		The command is not a known tutor
    		command.


User action:

    		Correct the command.  Type "HELP" 
    		for a list of tutor commands.

.KEY BADVALCT 
Explanation:

    		The number of values specified for a parameter
		is less than the minimum or more than the maximum
		defined for the parameter.


User action:

    		If the variable is a parameter defined via the
		PARM statement,	use HELP-PARM to determine the
		allowable minimum and maximum number of values
		and re-issue the command accordingly.

		If the variable was defined in a proc via the
		LOCAL statement, inspect the PDF file and
		determine the allowable number of values by
		checking the COUNT field.
.KEY BATCHRUN
Explanation:

		On a UNIX system, the attempt to submit a batch 
		job failed.  


User action:

		The most common cause of this error is a
		lack of file descriptors in the TAE Monitor.

		See your TAE system manager.
.KEY BCHOPN
Explanation:

    		An I/O error occurred while creating the log
    		file for the batch job.


User action:

    		Use local system utilities to check if 
    		you have the appropriate privileges 
    		to write to the file.  
.if VAX_VMS

                If you have used the DCL command, SET DEFAULT,
                to change to another user's directory, you
                may not have the appropriate privileges.
.ifend

                Use the local operating system I/O error code
                listed in the error message to determine the
                source of the error.  See the documentation
                on the operating system or the System Manager
                for further action.             

.KEY BCHRD
Explanation:

    		The batch job file is the initial source of commands
    		when the TAE Monitor is operating in batch mode.
    		An I/O error occurred while the TAE Monitor
    		was reading command records from the submitted
    		batch file.
    		

User action:

    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the TAE System Manager
    		for further action.		

.KEY BCHSTAT  
Explanation:							** VAX/VMS **

    		Under VAX/VMS, the batch job status is received
    		from a mailbox.  The attempt to create the mailbox
    		has failed.  The VAX/VMS code is listed.


User action:

    		Report the problem to the TAE System Manager.
.KEY BCHWRT
Explanation:

    		The batch job file is the file created when
    		a batch job is submitted.  It contains the
    		host-dependent batch commands as well as
    		the commands for the submitted proc.  An I/O error 
    		occurred while writing to this file.


User action:                

    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the System Manager
    		for further action.		
.KEY BIGSUBS
Explanation:

    		The subscript specified for a variable is
    		larger than the maximum defined for that
    		variable.


User action:

    		If the variable is a command parameter, use 
    		the TCL command 

    		  HELP-PARM <parameter name> <command name>

    		or the tutor command "HELP <parameter name>" to
    		determine the maximum number of	values for 
    		the parameter.

    		If the variable is a global variable, determine
    		the name of the proc that defines the global,
    		enter tutor for that proc, and use tutor as
    		above to find information on the variable.

    		If the variable is a local variable defined 
    		in a PDF, check the LOCAL command in the PDF.
.KEY BTNNACT
Explanation:

                The specified button has been disabled since
                it has no meaning in the present context.

User action:

                Take some action which will make the specified
                button relevant and thereby re-activate the button.

.KEY CMDCNFLCT
Explanation:

    		DEFCMD may not used to replace an existing command.


User action:

    		Use a different name for the new command, or 
    		use DELCMD to delete the old command.  You
		may display a list of the existing commands defined
		with DEFCMD by typing "DISPLAY $DEFCMD0".
.KEY CNTERR
Explanation:

    		One of the values for COUNT in a PARM or LOCAL
    		declaration is less that one, or greater than
    		the maximum number of values allowed, or the
    		COUNT specification does not follow the proper
    		format.

    		The proper format for COUNT is either a single
    		integer or "a:b", where "a" is the minimum 
    		number of values to be allowed, and "b" is the
    		maximum.
    		
                COUNT will allow a 0 to indicate a nullable
                parameter as long as there is a valid range
                given as well, i.e. "COUNT=(0,4:20)             
.page
User action:

    		Correct the COUNT specification.

    		See the TAE System Manager for the maximum
    		number of values allowed for a parameter
    		on your system.
.KEY COMINI
Explanation:                                                    
.if VAX_VMS

                The TAE Monitor, in its initialization under
                VAX/VMS, creates a permanent subprocess.  This
                creation has failed.

                On VAX/VMS systems, the problem is often caused
                by a depleted BYTLM quota or by exceeding the
                maximum number of processes allowed in the system.

User action:

                Restart TAE and report the problem to the
                TAE System Manager.
.elseif UNIX

                You should never receive this error message
                from the Terminal monitor when running under
                UNIX.

User Action:

                Report the problem to the TAE System Manager.
.ifend
.KEY CONTROLC
Explanation:

    		A CONTROL/C was entered while a TCL procedure was
    		waiting on X Window System events.


User action:

    		No action may be necessary.  A procedure may disable 
    		interrupts by a procedure declaration

    			PROCEDURE OPTIONS=NOINTERRUPT
.KEY DELREF
Explanation:

    		A global variable may not be deleted if there is
    		a currently active proc with a REFGBL on that global.


User action:

    		Determine the procedure that references the global
    		and either delete the global after the proc terminates
    		or remove the reference.
.KEY DFTERR
Explanation:

    		The default or initial value does not conform
    		to the rest of the declaration.  For example,
    		a variable declared to be an integer must
    		have an integer default that is within the
    		range specified in the VALID specification.		


User action:

    		Correct the default or initial value.  Request
    		HELP on the parameter in tutor mode to display
    		a list of the valid values for the parameter.
    		
.KEY DIVZERO
Explanation:

    		An arithmetic expression included a division
    		with a zero in the denominator.


User action:

    		Determine why a zero is in the denominator and
    		correct the condition.                
.KEY DUPPANEL
Explanation:

    		An attempt was made to create a panel with the
    		same name as an existing panel.


User action:

    		Delete the existing panel (using WPT-ERASE) or
    		rename the new panel.
.KEY ERPROC	
Explanation:

    		The command name is incorrectly formatted.


User action:

    		The command name is the first field on a command
    		line (after a label).  Correct the command name.  
    		A valid command name is	the name of a TCL intrinsic 
    		command or the name of a proc.  Type HELP while in 
    		TCL for a list of intrinsic commands and for valid 
    		abbreviations of intrinsic commands.
.page
    		If the command is a proc, a valid name is any 
    		valid file specification on the host.  See the
    		TAE System Manager or Appendix A of the TAE User's
		Manual.
                
.KEY EXEPARERR
Explanation:

		An attempt was made to execute a PAR file, but
		the PAR file could not be read.


User action:

		The host-specific reason for the inability to
		read the file is indicated in the message.  
.KEY EXHELP
Explanation:

    		The amount of help information exceeds the
    		internal capacity of the TAE Monitor for
    		an index of page start locations.


User action:

    		You may continue to use the help display,
    		but no PAGE capability is available.	
.KEY EXPR
Explanation:

    		The expression used in a LET, FOR, or IF command
    		does not follow the rules for the proper format
    		of expressions in TCL.  (See the TAE Application
		Programmer's Manual for the rules concerning expressions.)

User action:

    		Check for one of the following common problems,
    		and correct the command accordingly.

    		- Missing closing parenthesis
    		- No characters after an "="
    		- More than one "="
    		- Invalid numeric (e.g., 1..0e2)
    		- Unrecognized operator
.KEY FLTUTWAIT
Explanation:

		This is an information message indicating that TAE
		is currently busy, reading files and constructing
		the necessary data structures for the tutor panels.
.KEY FMTERR
Explanation:

    		A field in the command does not follow the 
    		proper format for that command.  This occurs usually
    		when a field has too many characters in it, or
		when a closing quotation mark has been omitted.


User action:

    		Check the proper format of the command, by
    		typing "HELP XXX" for help on command XXX.
.KEY FUNUNAV
Explanation:

    		This function is not available until the invalid 
    	 	value is corrected.


User action:

    		Correct the invalid value.
.KEY GBLREDEF
Explanation:

                    		
                A mismatch of types occur while attempting to
                redefine a global variable.
		For example, a REAL value 
		was targeted for an INTEGER global variable.

User action:

		Check the type of the global variable.
.KEY IMSGERR
Explanation:
.if VAX_VMS

                An I/O error occurred while sending the process
                initialization message to the process.  The message
                usually contains initial process parameters.


User action:

                There is a configuration problem or a host 
                system problem.  See the System Manager to check
                your quotas and the hardware.
.elseif UNIX

                The requested process could not be executed.


User action:

                Use the local system utilities to determine
                if an executable image exists for the
                specified process.  Determine if you
                have privileges to run that image.

                If the executable image is named correctly
                and you have the proper privileges, then,
                check if there is a configuration problem
                or a hardware problem.  See the System Manager
                to check your quotas and the hardware.
.ifend
.KEY INITXRLIB
Explanation:

		The TAE Monitor could not initialize the 
		Xr library.


User action:

		Make sure that the X server is running on
		your system.

		If an Xr error code is given in the 
		accompanying error message, look up the 
		Xr error code definition in the Xr 
		include file "defs.h" and correct the
		problem if possible. Else, talk to your
		system manager.
.KEY INTERR
Explanation:

    		The TAE Monitor has failed an internal integrity
    		check.


User action:

    		Report the problem to the TAE System Manager.
.KEY INVALIDPRE 
Explanation:

                The parameter's type is incompatible with the
                presentation type specified in the view file.

User action:

		Edit the view file and reenter the command. 

.KEY INVBQNAM
Explanation:
.if UNIX
                The name of the batch queue specified in the
                RUNTYPE qualifier is invalid.
                The valid batch queue names are NOHUP and NICE.
                NOHUP is the default name.

User action:

        
                Enter the queue name correctly.
.ifend
.KEY INVDCL
Explanation:

    		The last character on a TCL "DCL" command
    		must not be a dash.


User action:

    		Remove the dash.  If it is necessary to continue
    		a line, you may use the TCL continuation indicator, 
    		"+".
.KEY INVHEX
Explanation:

    		A number with an invalid hexadecimal format was
    		used in a command that expects a hexadecimal
    		number.


User action:

    		Correct the number to make it a valid hex number.

    		Type "HELP xxx" for help on the "xxx" command.
.KEY INVIFILE
Explanation:

		The parameter has type FILE but the named file
		does not match one of the following access
		rights:

	    (1)	The file can be accessed for input.

	    (2) The file can be accessed for both input and
		output.


User action:

		Determine through tutor or "HELP-PARM" the
		expected access rights for the file.

		Using local system utilities, determine if 
		the file exists and if you have appropriate
		rights to read the file.  If the parameter 
    		is defined to have INOUT access, then 
    		determine if you have appropriate rights 
    		to write to the file.
.KEY INVINT
Explanation:

    		The value for an integer does not conform to
    		the rules for acceptable integer values.


User action:

    		Correct the value to follow the rules.  An
    		acceptable integer is one of the following:

    		    - a number without a fraction (e.g., 1001)
    		    - a number with a zero fraction (e.g., 2.0)
    		    - a number with an exponent, but no fraction 
    		      (e.g., 2e2 or 2.0e2, but not 2e-2 and not 2.2e3)
.KEY INVNAME
Explanation:

    		The name of a variable does not follow the
    		rules for variable naming.  


User action:

    		Correct the variable name.  A valid variable
    		name contains eight or fewer characters, starts 
    		with a letter or underscore or dollar sign,
    		and may contain any of the following characters:

    		    - all number (0-9)
    		    - all letters (A-Z, a-z)
    		    - dollar sign ($)
    		    - underscore (_)                
.KEY INVOUT
Explanation:

		An output value from a process is not compatible
		with the target TCL variable.  The candidate 		
		value must have attributes (COUNT, TYPE, etc.) that
		are consistent with the declaration of the target.

    		Note that, for parameters of type NAME, the target
		variable is the referenced variable.

User action:
    		Determine the name of the variable that was
    		referenced, and the attributes (type, count,
    		etc.) of that variable.  Examine the proc
    		that generated the output to determine why
    		the value was set incorrectly.                
.KEY INVPNAME
Explanation:

    		A parameter name has an invalid format.

User action:

    		Correct the parameter name.  A valid parameter 
    		name consists of eight or fewer letters or numbers.
.KEY INVPVAL
Explanation:

    		The value of a parameter or command qualifier is 
		incorrectly formatted.

User action:

    		Correct the value.  A valid value may be enclosed in
    		quotation marks or unquoted.  If unquoted, it should
    		consist of 134 or fewer characters, and must not 
    		contain the characters ","  or ")" or "(" or a space 
    		or tab.  If the value contains more than one element, 
    		the list of elements should be enclosed within parentheses.
.KEY INVREAL
Explanation:

    		The value for a real variable does not conform to
    		the rules for acceptable real values.

		Examples of valid real numbers are:

			81
			100.
			478.345
			456.E-5
User action:

    		Correct the value. An acceptable real value 
		is any FORTRAN-like REAL number 
		acceptable to your host system.
.KEY INVSHL
Explanation:

		The last character on a "ush" command (or other
		shell entry command) must not be a "\" (the shell
		continuation indicator).


User action:

		Use the TCL "+" character for continuation.
.KEY INVSTR 
Explanation:

    		The value of a variable does not match any of
    		the strings in the list of valid strings defined
    		for the variable.                
                

User action:

    		If the variable is a command parameter, use 
    		the TCL command 

    		  HELP-PARM <parameter name> <command name>

    		or the tutor command "HELP <parameter name>" to
    		determine the valid strings for the parameter.

    		If the variable is a global variable, determine
    		the name of the proc that defines the global,
    		enter tutor for that proc, and use tutor as
    		above to find information on the variable.

    		If the variable is a local variable defined 
    		in a PDF, check the LOCAL command in the PDF.

.KEY INVSUBNAME	
Explanation:

    		The name of a variable used in substitution 
    		(i.e., following an "&") does not follow the
    		rules for variable naming.  


User action:

    		Correct the variable name.  A valid variable
    		name contains eight or fewer characters, starts 
    		with a letter or underscore or dollar sign,
    		and may contain any of the following characters:

    		    - all number (0-9)
    		    - all letters (A-Z, a-z)
    		    - dollar sign ($)
    		    - underscore (_)                
.KEY INVSUBS
Explanation:

    		The subscript for a variable is either not
    		an integer or not greater than zero.


User action:

    		Determine why the subscript is not a positive 
    		integer; correct the condition.                
.KEY INVVAL
Explanation:

    		If the variable is a string, then the value of the
    		variable does not match any of the strings in the 
    		list of valid strings defined for the variable.

    		If the variable is not a string, then the value
    		of the variable is not in the range (or ranges)
    		acceptable for the variable.
                

.page
User action:

    		If the variable is a command parameter, use 
    		the TCL command 

    		  HELP-PARM <parameter name> <command name>

    		or the tutor command "HELP <parameter name>" to
    		determine the valid values for the parameter.

    		If the variable is a global variable, determine
    		the name of the proc that defines the global,
    		enter tutor for that proc, and use tutor as
    		above to find information on the variable.

    		If the variable is a local variable defined 
    		in a PDF, check the LOCAL command in the PDF.

.KEY KILLED	
Explanation:

		This is an information message.  The process
                was terminated because the user typed "ABORT"
                while in dynamic prompt mode or "EXIT while
                in dynamic tutor.


User action:

		None required.                
.KEY LIBS
Explanation:

    		The length of the library list is greater
    		than the maximum allowed.


User action:

    		Provide fewer libraries in the list.
.KEY LONGCMDLINE
Explanation:

    		The command line is longer than the maximum allowed
    		line.  Note that this may be a result of substitution 
    		(i.e., a string replacing a variable name starting 
    		with an ampersand).


User action:
		If the line overflow  is caused by substitution,
		the de-referencing technique may remedy the
		situation.  See the TAE Application Programmer's Manual.
.KEY LONGJOBFILE
Explanation:							** VAX/VMS **

    		Under VAX/VMS, the largest allowable name for
    		a batch job log file is nineteen characters.


User action:

    		Change the file name.
.KEY LONGQNAME
Explanation:							** VAX/VMS **

    		Under VAX/VMS, the largest allowable name for
    		a batch queue is fifteen characters.


User action:

    		Change the queue name.
.KEY LONGSTR
Explanation:

    		The number of characters in a string is greater
    		than the defined maximum length of the string.


User action:

    		Determine the maximum length of a string for
    		the specified variable, and correct the command 
    		entry accordingly.                
.KEY LONGVSTR 
Explanation:

    		A variable was declared with a VALID string
    		that contained more than 24 characters.

User action:

    		Use a shorter VALID string.                
.KEY MALMDF
Explanation:

    		The specified entry in the menu definition 
    		file has a ".MENU" or ".PROC" with no
    		associated menu or proc name.

    		See the TAE Application Programmer's Manual for
		a description of menu definition files.

User action:

    		Correct the entry in the menu definition
    		file to specify a file name.                
.KEY MALMSGF
Explanation:

    		Possible causes:

    	    (1) There was an I/O error in reading the help 
    		message file.

    	    (2) The file of indexes for the message file
    		does not correspond to the current message file.


User action:

    		Use the MSGBLD utility to re-build the 
    		index file from the message file.

    		Note for VAX/VMS and UNIX users:  the index 
    		file has type ".INX", and the message file
    		has type ".MSG".
.KEY MAXDEFC	
Explanation:

    		The maximum number of user-defined commands
    		has already been reached.  The DEFCMD that
    		resulted in the error message is ignored.


User action:

    		$DEFCMD0, $DEFCMD1,...,$DEFCMD9 contain the 
    		current list of user-defined commands.  Use 
    		DELCMD to delete a user-defined command.
.KEY MAXMEN
Explanation:

    		The last MENU command or selection of a new 
    		menu entry caused the TAE Monitor limit on
    		successive menu references to be exceeded.

User action:

    		Go back to the root menu by typing "TOP".

    		Note that this condition may be caused by
    		interspersing menu selection by entry
    		number with menu selection using the "MENU"
    		command.
.KEY MAXNEST
Explanation:

    		The TCL limit on nested constructs was reached.


User action:

    		Correct the procedure commands to eliminate the
    		extra nesting.                
.KEY MDFOPEN	
Explanation:

    		The requested menu is not available.  This
    		is generally because of one of the following:

    		   - incorrect spelling of the menu name
    		   - the menu does not exist in the specified 
    		     library
    		   - you do not have appropriate access rights
    		     to read the menu


User action:

    		Determine which of the conditions above is
    		the cause of the problem.

    		If the menu does not exist or you do not
    		have appropriate access rights, or there
    		is some other cause, see the TAE System Manager.	
.KEY MEMOVR
Explanation:

    		The TAE Monitor does not have sufficient 
    		internal memory to handle the current command
    		in addition to the memory already in use as a
    		result of previous commands.

                
User action:

    		Attempt to reduce the memory required.  The
    		major user-controllable uses of the TAE
    		Monitor internal memory	store are:

    		    - Each variable (including parameters, local
    		      variables, and global variables) requires 
    		      storage for the variable characteristics 
    		      and values.

    		    - Each nested invocation of a proc requires
    		      storage for proc context.

    		    - Each nested invocation of a menu requires 
    		      storage for menu context.

    		    - Each DEFCMD requires storage for the
    		      command definition. 

    		    - Each library added to the search list
    		      adds storage for the list.
.KEY MISEQ
Explanation:

    		A command line using the FOR or LET command did not
    		follow the proper format for these commands.

User action:

    		Type "HELP FOR" or "HELP LET", and correct the
    		command line according to the help information
    		displayed.
.KEY MISINTRO	
Explanation:

    		All proc definition files in TCL must start with 
    		one of PROCEDURE, PROCESS, GLOBAL, or PARMSET,
    		depending on the type of proc.  

User action:

    		Insert the necessary introductory command.

    		Type "HELP xxx"	to display information on 
    		the "xxx" command.
.KEY MISSLIB
Explanation:

    		The specified library is not on the current
    		library list.


User action:

    		None required.  Use "SHOW" to display the current
    		library search list.
.KEY MISPAR
Explanation:

    		The specified command requires that the listed
    		parameters be entered on the command line.


User action:

    		Re-issue the command with values for the missing
    		parameters.

    		If you are unsure of the proper values for this
    		command, Type "TUTOR xxx" to tutor for the values
    		for command "xxx", or type "HELP-PARM yyy xxx"
    		to display information on the "yyy" parameter
    		of the "xxx" command.

.KEY MIXTYPE
Explanation:

    		The type of a variable used on the left side in a 
    		LET or FOR command does not match the type of a
    		variable or constant used on the right-hand side.
    		All variables and constants used in LET and FOR
    		commands must have the same type.
    		
    		For example, if a user types

    		    LET II = 1.2

    		then II should be a REAL variable.

    		The TCL data types are INTEGER, STRING, REAL, and 
    		FILE; see the TAE Application Programmer's Manual for 
    		the rules on allowable values for each data type.


User action:

    		Correct the command to use matching types.                
.KEY MIXVAR 		
Explanation:

    		More than one data type is used on the right side
    		of a LET or FOR command, or the type of a variable 
    		or constant used on the left side in a LET or FOR 
    		command does not match the type of a variable or 
    		constant used on the right-hand side.

    		All variables and constants used in LET and FOR
    		commands must have the same type.
    		
    		For example, if a user types

    		    LET II = J + 1.2

    		then II and J should be REAL variables.

    		The TCL data types are INTEGER, STRING, REAL, and 
    		FILE; see the TAE programmer's manual for 
    		the rules on allowable values for each data type.


User action:

    		Correct the command to use matching types.                
.KEY MULVAR
Explanation:

    		The only operation supported for multivalued
    		expressions is assignment.  Only scalars may
    		be used in arithmetic, string, or logical
    		expressions.


User action:

    		Use subscripts to apply the operations to
    		the individual components of the vector.
.KEY NAMERR
Explanation:

    		VALID, ACCESS, and COUNT do not apply for 
    		TYPE=NAME.  In performing checks on data 
    		assignments to variables with type NAME, 
    		the TAE Monitor uses the VALID, ACCESS,
    		TYPE, and COUNT of the ultimately referenced
    		variable.


User action:

    		Remove the VALID, or ACCESS, or COUNT clause
    		from the command line.	
.KEY NESTMUL  
Explanation:

    		TCL does not support the placement of a 
    		parenthesized list embedded within
    		another parenthesized list.  For example,
    		the following is not legal:

    			LET I = (1, 2, (2, 3) )


User action:

    		Determine the cause of the nested 
    		parenthesized lists; remove the inner 
    		parentheses or correct the commands 
    		that generated the lists.
.KEY NOBATCH
Explanation:

    		The submission of the batch job to the local
    		operating system was rejected.


User action:

    		Use the documentation on the local operating
    		system to interpret the error code.  See the
    		TAE System Manager for further action.
.KEY NOBCHSTA
Explanation:							** VAX/VMS **

    		Under VAX/VMS, the batch job status is read
    		from a VMS mailbox; an I/O error was
    		encountered while reading from this mailbox.


User action:

    		Use the documentation on the local operating
    		system to interpret the error code.  See the
    		TAE System Manager for further action.
.KEY NOCOMM	
Explanation:

    		An I/O error occurred while the process
    		was sending a message containing parameters
    		to the TAE Monitor.


User action:

    		Report the problem to the TAE System Manager.

    		For VAX/VMS users:  the I/O error occurred while
    		writing to a mailbox.  The error code is shown
    		on the message line.
.KEY NOCOMMAND
Explanation:
    		This command has been implemented on some TAE 
    		versions, but not the version you are using.
.KEY NOFAC
Explanation:

    		Either the message index file for the specified 
    		facility cannot be found or opened, or the message 
    		file itself cannot be found or opened.  (The 
    		"facility" is the string of characters before the 
    		dash in the message key.)


User action:

    		See the TAE System Manager.
.page
    		Note for VAX/VMS and UNIX users:  the index 
    		file has type" .INX", and the message file 
    		has type ".MSG".
.KEY NOFILE
Explanation:

		The specified message file could not be found.


User action:

		Check your typing against the intended name. 
		Note that the default type is "msg".
.KEY NOHELP	
Explanation:

    		There is no help information on the named
    		command or menu.


User action:

    		See the TAE System Manager or the author
    		of the menu or command.
.KEY NOKEY
Explanation:

    		There is no help information on the specified
    		message	key in the message file for the
    		specified facility.  (The "facility" is
    		the string of characters before the dash
    		in the message key.)


User action:

    		See the TAE System Manager or the author of
    		the message file.
                
.KEY NOLETVAL
Explanation:

    		A LET command must have a value on the right 
    		hand side.


User action:

    		Correct the LET command to have an appropriate value.
.KEY NONGBL  
Explanation:

    		A global variable may be deleted only by using
    		the DELETE-GLOBALS command with the name of a GLOBAL
    		proc that defines the variables, or using DELETE-VAR.

User action:

    		Determine the GLOBAL proc that defined the variable
    		to be deleted, and use DELETE accordingly.

    		For more information on DELETE type "HELP DELETE".
.KEY NOPANEL
Explanation:

    		In a WPT operation, the named panel does not exist.


User action:

    		Review your TCL code that created the panels.  The
    		panel name is in the TCL variable VIEW._panel.name, 
    		where VIEW is the view variable through which the 
    		panel attributes are set.   

.KEY NOPARENS
Explanation:

    		Parentheses are required around logical expressions.

    		Example:

    		    IF (s = "myfile" AND t = "yourfile")...

User action:

    		Insert the necessary parentheses around the logical
    		expression.
.KEY NOPARHELP
Explanation:

    		No help information exists on the specified 
    		parameter in the help file for the specified proc.


User action:

    		See the TAE System Manager or the author of the
    		proc.

    		Note that you may type "TUTOR xxx" to be tutored 
    		for the parameter values on command "xxx".
.KEY NOPARM
Explanation:

    		The specified command is to be typed using
    		only the command name.  There are no parameters.

User action:

    		Retype the command name alone.                
.KEY NOPDFPARM
Explanation:

                This is an information message.  The specified 
		PDF does not have any parameters.

User action:

                None required.
.KEY NOPROC
Explanation:

    		The first word on the command line is
    		expected to be a command name.  If it
    		is not a TCL intrinsic command, then
    		TAE  searches for the name in the proc
    		libraries.

User action:

    		If a proc search was intended, then
    		use the SHOW command to display the
    		current search list.  Also the 
		PROCS utility in the utilities 
		library may be used to display the
		names of available procs.  
.KEY NOQUAL
Explanation:

    		Command qualifiers are not defined for 
    		TCL intrinsic commands.

		Qualifiers are special parameters enclosed
		in vertical bars, for example:

		command|RUNTYPE=BATCH, RESTORE=file| 

User action:

    		Remove the qualifier from the command line.
.KEY NOSUB
Explanation:

    		SUBCMD/END-SUB are proper brackets for
    		parameters associated with a subcommand.

    		This error message also appears when
    		subcommands are nested.


User action:

    		Insert "SUBCMD" at the appropriate location
    		in the procedure or delete the "END-SUB".
.KEY NOSUBC
Explanation:

                No subcommand has been selected.

User action:
                Select a subcommand then select the DO button.

.KEY NOTARGET
Explanation:

    		The variable for the "target" qualifier set 
    		required for creating the panel does not exist.


User action:

    		Correct your TCL code to refer to the appropriate
    		variable.  (See the WPT command in the "TAE
    		Application Programmer's Guide" for more 
    		information on using WPT-PANEL.)
.KEY NOTBATCHCMD
Explanation:

    		The specified command is available only if
    		you are running in an interactive session.

    		See the TAE User's Manual for the list of
    		commands not allowed in batch.                
User action:

    		Eliminate the command from the procedure.
		Note that a procedure may "adapt" to its 
		environment as follows:

		IF ($RUNTYPE = "INTERACTIVE") 
		    commands-restricted-to-interactive-sessions
		END-IF

.KEY NOTEXT
Explanation:

    		The help file exists for the requested
    		information but there is no text following
    		the ".HELP" directive.


User action:

    		See the author of the proc or the TAE
    		System Manager.
.KEY NOTFND
Explanation:
                No parameter or subcommand name was found 
                matching the string specified. This message
                is also displayed when the only occurrence of
                the string is the current parameter or subcommand.

User action:

                Specify another string for which to search.
.KEY NOTINBODY
Explanation:

    		The specified command is allowed only in the
    		declaration section of the PDF.


User action:

    		Either eliminate the command or place it in
    		the declaration section.
.KEY NOTINPDF
Explanation:

    		The specified command is allowed only at the 
    		interactive level.


User action:

    		Remove the command from the PDF.                
.KEY NOTINPRE
Explanation:

    		The specified command is allowed only 
    		after the BODY command of a procedure.


User action:

    		Move the command to follow BODY.
.KEY NOTSTR
Explanation:

    		The argument to the $STRLEN function must 
    		be a string.


User action:

    		Correct the $STRLEN argument.                
.KEY NOVALID
Explanation:

    		A parameter with type NAME was declared with a
    		VALID list.

User action:

    		Remove the VALID list from the NAME parameter.  
    		(VALID lists may be included in the declaration of
    		the variable referenced through the NAME parameter,
		but not associated with the NAME parameter itself.)

.KEY NOVAL
Explanation:

    		A command referenced a variable that
    		has never been assigned a value.

		A variable may be declared without any
		value.  

User action:
		The variable must have a value in order to
		participate in the expression.   A variable
		may be assigned a value with the LET command,
		for example:

			LET  STRING = "hello"
.KEY NOVIEW
Explanation:

    		The variable for the "view" qualifier set 
    		required for creating the panel does not exist.


User action:

    		Correct your TCL code to refer to the appropriate
    		variable.  (See the WPT command in the "TAE
    		Application Programmer's Guide" for more 
    		information on using WPT-PANEL.)
.KEY NOWINDOW
Explanation:

    		In a WINDOW operation, a window that does not exist
    		was named.


User action:

    		Use WINDOW-INFO to determine the names of windows
    		currently displayed.
.KEY NOXINIT
Explanation:

    		The X Window System has not been initialized.


User action:

    		WPT-INIT initializes the X Window System.
.KEY NOXLIB
Explanation:

		TAE was not installed on your system with X Windows 
		support.

User action:

		If X Windows is available on your system, check
		with your system manager about re-installing TAE
		with X Windows support.
.KEY NUMPARM
Explanation:

    		The specified command is defined to 
    		use only the number of parameters shown.


User action:

    		Type "HELP commandname" to determine the format
    		of the command.                
.KEY OLDPARFILE
Explanation:

		The parameter file you tried to read has an
		obsolete format.  You must convert the file
		in order to use it.

User action:
		
		Type "HELP CONVERT" or "TUTOR CONVERT" for
		information on how to convert old files.
.KEY OLDRESFILE
Explanation:

		The resource file you tried to read has an
		obsolete format.  You must convert the file
		in order to use it.

User action:
		
		Type "HELP CONVERT" or "TUTOR CONVERT" for
		information on how to convert old files.
.KEY ONFAIL
Explanation:

    		Two possible causes:

    	    (1) The command string that was assigned in a 
    		procedure to the _ONFAIL variable has a 
    		command format that cannot be interpreted 
    		by the TAE Monitor.

    	    (2) The _ONFAIL variable has been assigned a command
    		that is not allowed for _ONFAIL.  The only
    		commands allowed for _ONFAIL are GOTO, CONTINUE,
    		RETURN, BREAK, NEXT, or STOP.


User action:

    		Correct the _ONFAIL assignment.                

.KEY OPNRD
Explanation:

		The command you issued cannot open the file
		you specified on the command line.  The file 
    		does not exist, or the file is protected
		against read access, or the file specification
		is incorrect.

		(Note that there is no hierarchy search for
		scripts or parameter files; if the file is not 
		located in the default library, you must name 
		the library explicitly.)

User action:

    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the System Manager
    		for further action.		

    		Use local system utilities to check if the
    		file exists and to check if you have
    		appropriate privileges to read the file.
    		If the problem is not then evident, see
    		the System Manager.
.KEY OPNWRT
Explanation:

		The command you issued cannot open the file you
		specified on the command line.  The error is
		usually related to one of the following:

		-- The file specification has incorrect syntax.

		-- The explicit directory is protected from access.

		-- The default directory is protected from access.

User action:

    		Use local system utilities to check if 
    		you have the appropriate privileges 
    		to write to the file.  (If, under VAX/VMS, 
    		you have used the DCL command, SET DEFAULT, 
    		to change to another user's directory, you 
    		may not have the appropriate privileges.)

    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the System Manager
    		for further action.		
.KEY PAGENUM
Explanation:

    		A negative or zero page number was typed or the page
    		number was a string.  


User action:

    		Re-issue the command with the desired page
    		number.
.KEY PAROPN
Explanation:

		A new PAR file could not be created because of
		a failure in the open operation.


User action:

		More specific information on the cause of the
		error is given in the message.  The most likely
		cause is an attempt to write into a directory
		for which the user does not have "write" access.
		Another possible cause is an error in typing
		the file specification.
.KEY PARS
Explanation:

    		There are fewer parameters defined for the
    		specified command than are given on the command
    		line, or too many command or parameter qualifiers 
    		are specified.


User action:

    		Re-issue the command with the correct number
    		of parameter values or qualifiers.

    		If you do not know the correct number, type 
    		"TUTOR xxx" to be tutored for the parameters
    		of command "xxx", or type "HELP xxx" to display
    		information about the command.
.KEY PDFRD
Explanation:

		The proc definition file has been found but
		cannot be opened.  This is usually caused by
		a file protection error.


User action:

    		Use host system utilities to check the integrity
    		of the proc file (that is, determine if the
    		file can be read by host system utilities).

    		See the TAE System Manager for further help.
.KEY PFOPN	
Explanation:

    		This condition is a result of a problem
    		associated with the executable code of a 
    		process or the file configuration.

    		The file specified in an XRRDB call could
    		not be opened.


User action:

    		Use the error code shown in the message
    		to determine why the file could not be
    		opened.  Correct the file configuration
    		or the program accordingly.
.KEY PFRD	
Explanation:

    		This condition is a result of a problem
    		in the executable code of a process or
    		the file configuration.

    		An I/O error occurred while reading the
    		specified parameter file from the process
    		(in an XRRDB call).


User action:

    		Use the error code given in the message to
    		determine the cause of the error.  Correct
    		the file configuration or the program
    		accordingly.                
.KEY PFWRT
Explanation:

    		This condition is a result of a problem
    		in the executable code of a process or 
    		the file configuration.

    		An I/O error occurred while writing to the
    		specified parameter file from the process
    		(in an XQWRTB call).


User action:

    		Use the error code given in the message to
    		determine the cause of the error.  Correct
    		the file configuration or the program
    		accordingly.                
.KEY POSERR
Explanation:

    		The format of a proc invocation command line requires
    		that values specified by position precede values specified
    		by name for command parameters, and for command or
    		parameter qualifiers.

    		Example:

    		    CMD 1, 2, 3, P4=4, P5=5

    		In this example, the first three values are specified
    		by position, and the last two by name.
.KEY PRCSABT
Explanation:

    		A system error was encountered when trying to abort 
    		a process.


User action:

    		There is a configuration problem or a host system
    		problem.  See the System Manager.
.KEY PRCSCOM
Explanation:
.if UNIX

                The terminal monitor could not establish
                communication with the application process.


User action:

                This may happens if the terminal monitor
                exceeds the open file quota limit or if there
                is a hardware problem.  See the TAE System Manager
                to check your quotas and the hardware.

.ifend
.KEY PRCSINI
Explanation:
.if VAX_VMS

                The requested process cannot be initiated.


User action:

                Use the local system utilities to determine
                if an executable image exists for the
                specified process.  Determine if you
                have privileges to run that image.

                If the executable image is named correctly
                and you have the proper privileges, then
                check whether there is a configuration problem
                or a hardware problem.  See the System Manager.
.elseif UNIX

                Under UNIX operating system, the TAE Monitor
                first creates a child process to run an
                application program. This creation has failed.

                This problem may be caused by exceeding the
                maximum number of processes allowed in the system.


User action:

                Restart TAE and report the problem to the
                TAE System Manager.
.ifend
.KEY PRCSTRM
Explanation:

    	    	This is an information message.  The executed
    		process returned an "abnormal" status.

.KEY PREEOF
Explanation:

    		An end-of-file was read in a proc definition
    		file before the	END-PROC command was encountered.


User action:

    		Place an END-PROC at the end of the proc 
    		definition file (or, if there is help information,
    		before the help information).    		
.KEY PRMRCV	
Explanation:

    		This condition is a result of a problem
    		associated with the executable code of 
    		the process or the environment in which
    		the user is running.

    		The call to XRINIM failed.


User action:

    		Use the code shown on the message line to
    		determine the cause of the problem.  Correct
    		the program or the user's environment
    		accordingly.
.KEY PRMSIZ	
Explanation:

    		This condition is a result of a problem in the
    		executable code for the process.

    		A call to XRINIM or XRRDB failed because
    		the DIM argument is not large enough to
    		receive the variable values.


User action:

    		Determine the appropriate size for DIM and
    		correct the program accordingly.
.KEY PROCONLY
Explanation:

    		The specified command is not available for interactive
    		use.


User action:

    		Place the command in a procedure, or use a different
    		command.

    		Type "HELP xxx" for more information on command "xxx".
.KEY PSETOVER
Explanation:

    		The internal memory store reserved by the TAE
    		Monitor for process parameter values is not
    		large enough to accommodate the current values.

                
User action:

    		Reduce one or more of the following:

    		    - the number of values per parameter
    		    - the number of parameters
    		    - the number of characters in string
    		      parameters.                
.KEY QUALDEL
Explanation:

                Deleting a qualifier of a variable is not allowed.

User action:
		
.KEY QUALERR
Explanation:

    		The qualifier after the proc name or parameter is 
    		not enclosed within '|' or the format for the 
    		qualifiers do not follow the general parameter 
    		list format.

User action:

    		Enclose the qualifiers within '|' or fix the qualifier
    		list.  The proper format for the list is shown in the 
    		following example:

    			PROC|RESTORE=myfile, RUNTYPE=BATCH| PAR=3

    		which sets the RESTORE qualifier to "myfile" and the
    		RUNTYPE to BATCH.
.page
    		Another example:

    			PR1 x|3|

    		Here, proc PR1 sets the parameter qualifier for
    		the x parameter to 3.
.KEY RANGE
Explanation:

    		The value of the variable is not in the acceptable 
    		range (or ranges) for the variable.
                

User action:

    		If the variable is a command parameter, use 
    		the TCL command 

    		  HELP-PARM <parameter name> <command name>

    		or the tutor command "HELP <parameter name>" to
    		determine the valid range for the parameter.

    		If the variable is a global variable, determine
    		the name of the proc that defines the global,
    		enter tutor for that proc, and use tutor as
    		above to find information on the variable.

    		If the variable is a local variable defined 
    		in a PDF, check the LOCAL command in the PDF.
.KEY RCVERR
Explanation:

    		An I/O error occurred while receiving a message
    		from a process.  The message usually contains
    		process parameters for dynamic tutor.


User action:

    		There is a configuration problem or a host
    		system problem.  See the System Manager.
.KEY RDERR
Explanation:

    		An I/O error was encountered while
    		reading records from the specified file.
    		The file is either missing or corrupt.


User action:

    		If an error code is given, use local system 
    		utilities to interpret the error code; also
    		use local system utilities to check the integrity 
    		of the file.  If the problem is not then 
    		evident, see the System	Manager.
.KEY REDEF
Explanation:

    		If a GLOBAL proc contains a PARM command for
    		a global that already exists, it must use the
    		same TYPE, ACCESS, and COUNT, as the existing 
    		global.


User action:

    		Correct the PARM commands to agree, or rename
    		one of the variables.  To determine the 
    		attributes of the existing global named gx, 
    		determine the defining PDF, tutor on it, and 
    		type "HELP gx"
.KEY REFEX
Explanation:

    		A REFGBL command may not refer to a global 
		variable with the same name as an existing 
		local variable or parameter.


User action:

    		Change the name of the conflicting local or
		parameter variable, or do not reference 
		the global variable.
.KEY RESERVED
Explanation:

    		The name used for a variable is a reserved 
    		word in TCL and may not be used as a variable name.


User action:

    		Rename the variable.  See the TAE Application
    		Programmer's Manual for a list of reserved names.
.KEY RESFORMAT
Explanation:

		The indicated TAE Plus resource file could not 
		be read because of a problem accessing the records.  
		The file has been corrupted.


User action:

		Repeat the operation.  If the operation continues
		to fail, the resource file must be re-created.
.KEY RUNSHELL
Explanation:
.if UNIX
                
                The Terminal monitor could not invoke the
                UNIX/shell in response to the USH command.


User action:

                This problem may be caused if the system
                process table is full or due to configuration
                or hardware problems. See the TAE System Manager.
.ifend
.KEY RUNTYPE 
Explanation:

    		The RUNTYPE qualifier, if specified, must
    		be set to "INTERACTIVE" (the default),
    		"BATCH", or "ASYNC".

    		This situation sometimes occurs when a qualifier
    		is placed in the wrong position after the "|".


User action:

    		Re-issue the command with "BATCH" or "ASYNC" as 
    		the qualifier, or with the qualifiers in their
    		appropriate positions.
.KEY SAVERR
Explanation:

    		The parameter save operation cannot be performed
    		because of an I/O error writing to the file,
    		because the file cannot be opened, or because
    		the TAE Monitor does not have sufficient
    		internal storage to accommodate all the parameter
    		values.


User action:

    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the System Manager
    		for further action.		
.KEY SIZERR
Explanation:

		On a UNIX system, the number and size of all
		parameters are too large to transmit to the
		application process. (Note that the total
		size required is affected by both the number
		of parameters and the number of components
		in all parameters.)


User action:

		Try to find a way to reduce the size of the
		amount of information to send in the parameter
		block.  For example, try using a shell environment
		variable for long file names.  If this is not
		possible, reduce the number of parameters required
		or the number of components in parameters.
.KEY SNDERR
Explanation:

    		An I/O error occurred while sending a message
    		to a process.  The message usually contains
    		process parameters.

User action:

    		There is a configuration problem or a host 
    		system problem.  See the System Manager to 
    		check your quotas (especially the BYTLM quota).
.KEY STDOOPN
Explanation:

    		The standard output could not be opened.
    		The standard output file is the file
    		specified through the STDOUT qualifier
    		on a command line.  Note that the
    		STDOUT qualifier may have been used
    		in a procedure.


User action:

    		Use local system utilities to check if the
    		file exists and to check if you have the
    		appropriate privileges to write to the
    		file.  (If, under VAX/VMS, you have used
    		the DCL command, SET DEFAULT, to change
    		to another user's directory, you may not
    		have the appropriate privileges.)

    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the System Manager
    		for further action.		
.KEY STDOWRT
Explanation:

    		An I/O error was encountered while 
    		writing to the standard output file.
    		The standard output file is the file
    		specified through the STDOUT qualifier
    		on a command line.  Note that the
    		STDOUT qualifier may have been used
    		in a procedure.
                
User action:

    		There is a configuration problem or a host
    		system problem.  See the System Manager to check
    		your quotas and the hardware.  
                
    		Use the local operating system I/O error code
    		listed in the error message to determine the
    		source of the error.  See the documentation
    		on the operating system or the System Manager
    		for further action.		
.KEY STRSIZ
Explanation:

    		The declaration of the type for the named
    		variable included a string size that is
    		greater than the maximum.


User action:

    		Correct the string size.  The longest string
		in TAE is defined by the TAE configuration file.
.KEY SUBCFMT
Explanation:

    		The subcommand name, that is, the word after 
    		the dash, is incorrectly formatted.


User action:

    		Correct the command to use a proper subcommand
    		format.  A valid subcommand name contains
    		eight or fewer characters, starts with
    		a letter or underscore or dollar sign,
    		and may contain any of the following characters:

    		    - all number (0-9)
    		    - all letters (A-Z, a-z)
    		    - dollar sign ($)
    		    - underscore (_)                
.KEY SUBREQ
Explanation:

    		The command must be accompanied by a subcommand.
    		A subcommand selects a group of parameters
    		from among several groups the command may
    		recognize.  The format to include a subcommand
    		is 

    			proc-subcommand parameters

    		to invoke "proc" with subcommand "subcommand";
    		the dash is required.

    		An example:

    			HELP-PARM p1 proc1		                
.KEY SUBTWC
Explanation:

		One of the following errors has occurred:

		1. The subcommand typed by the user is 
		ambiguous, i.e., the subcommand is a
		truncated abbreviation for more than 
		one of the valid subcommands.

		2. The proc definition is incorrect 
		because a given subcommand is defined
		twice.

User action:

		1. Select a non-ambiguous spelling for
		the desired subcommand and re-enter
		the command.

		2. Fix the multiple definition of the
		subcommand in the proc definition file.

    		Use "TUTOR pr-" to determine the subcommands
    		for proc "pr".

.KEY TMEQASS
Explanation:

    		The tutor parameter assignment command is
    		missing an "=".


User action:

    		Use the following format for parameter value
    		assignment:

    		    parameter-name = value

    		Examples:

    		     CYCLE = 2
    		     BIN(2) = 3
    		     VECTOR = 1, 2, 3, 4                
.KEY TMISPAR
Explanation:

    		The tutor RUN command was used while
    		the listed parameters did not have 
    		values.


User action:

    		Use the tutor parameter value assignment
    		command (parameter = value) to set the
    		values of the listed parameters, and
    		repeat the RUN command.
.KEY TNISUBSC
Explanation:

    		The value of a subscript for a variable
    		is not a positive integer.


User action:

    		Correct the value and repeat the command.                
.KEY TNODEFSUB
Explanation:

    		While in tutor subcommand display, a hyphen
    		was typed with no following word, thereby
    		choosing the default subcommand.  The current
    		command, however, has no default subcommand.
                

User action:

    		Choose one of the subcommands listed in the
    		subcommand display, and type "-<subcommand name>".
    		(If a subcommand is the default subcommand, it
    		is marked "** DEFAULT **".)
.KEY TNODEF
Explanation:

    		While in tutor mode, a parameter was assigned a
    		default value, using "parameter-name = ",
    		(i.e., no explicit value on the right side of "=".)
    		The named parameter has no default value.                


User action:

    		Use "parameter-name = value" to assign
    		an explicit value.
.KEY TNOLATCH
Explanation:

    		When the TUTOR command is typed with no proc name,
    		tutor attempts to run using the "latched" parameters.
    		Parameters are latched (i.e., remembered) only when 
    		the previous proc invocation had missing or incorrect
    		parameters.

User action:

    		Re-issue the TUTOR command with a proc name.
.KEY TNOTUSED
Explanation:

		A parameter file was restored, or parameters were
		provided by a program for a dynamic parameter session,
		in which one of the following is true for one or more of 
		the parameters:

	    (1) A parameter in the parameter file does not exist 
    		in the PDF used for the tutor or dynamic parameter 
    		session.

	    (2) The parameter does not have a valid value (according
		to the declaration in the PDF).

.page
		A common cause of this condition is the use of RESTORE
		on a file that does not match the current PDF.  For
		example, the PDF may have been changed since the
		parameter file was SAVEd.


User action:

		Check that you have use the intended parameter file 
		name.  If a dynamic parameter session has been initiated, 
		determine if the generating program refers to the correct 
		PDF, and determine if implied characteristics of the 
		parameters used in the program agree with the declaration 
		for the parameters in the PDF.
.KEY TNOTFOUND
Explanation:
		The string specified in the SEARCH command could not
		be found anywhere in the list of valid strings.

User action:
		Check the spelling of the specified search string.
		If it was correct, that string is not available.
		Try the SEARCH command with a different string.
.KEY TSELNAVAIL
Explanation:
		The SELECT display is not available for the specified
		parameter.  The SELECT display is restricted to parameters
		that have a specified list of valid string values.

User action:
		Do not attempt to use SELECT display for this parameter.

.KEY TNPNAM
Explanation:

    		While searching through help data to find 
    		information on a named variable, a ".VARIABLE"
    		help directive was encountered with no parameter
    		name.


User action:

    		Correct the help file to specify a name with
    		".VARIABLE" or inform the help file author.
.KEY TNPRCSYN 
Explanation:

    		Tutor is available for any proc or for any
    		TCL intrinsic command that has proc 
		invocation syntax.  Commands like LET 
		have syntax incompatible with tutor mode.

.KEY TNSCR
Explanation:

    		A tutor command only available in the NOSCREEN 
    		tutor mode was typed.

User action:

    		Type "HELP" to display the commands available in
    		the current tutor mode.  
.KEY TOOFEW
Explanation:

    		The number of values specified for a parameter
    		is less than the minimum defined for the parameter.


User action:

    		Use tutor to determine the allowable minimum 
    		number of values and re-issue the command 
    		accordingly.
.KEY TOOMANY
Explanation:

    		The number of values specified for a parameter
    		is greater than the maximum defined for the parameter.

User action:

    		Use tutor to determine the allowable maximum 
    		number of values and re-issue the command 
    		accordingly. Parameter help available in tutor 
    		mode should describe the number of values allowed.

.KEY TOOMNYFOR
Explanation:

    		The number of values that may be assigned using
    		a FOR list is limited to 10 values. 

		(A longer FOR list may be simulated using a 
    		multi-valued local variable, an index, and 
    		a LOOP construct.)

User action:
    		Delete values from the FOR list.                

.KEY TPAGNSC
Explanation:

    		There are no tutor pages while in NOSCREEN tutor.


User action:

    		Use the LIST command to list the parameter
    		values, and the DISPLAY command to display 
    		the parameter descriptions.
.KEY TPAGNUM
Explanation:

    		The tutor PAGE command (or an abbreviation,
    		e.g., "PA") was typed followed by a negative 
    		number or zero.  
                

User action:

    		Use a positive page number.
.KEY SAVRST
Explanation:

                Your parameters have been successfully saved to
                or restored from the specified file.
                     
User action:
                No action, the message is informational.

.KEY TSELNOTSCR
Explanation:
		The tutor command SELECT brings up the TUTOR/SELECT
		screen.  The TUTOR/SELECT screen is not available in
		NOSCREEN tutor. 

User action:
		Do not use the SELECT command when using tutor in 
		NOSCREEN mode.
.KEY TSUNTCMD
Explanation:

    		The command is not a known command
    		for tutor subcommand display.


User action:

    		Correct the command.  Type "HELP" for a 
    		list of commands available while in tutor
    		subcommand display.
.KEY TTOOVAL
Explanation:

    		There are too many words on a DISPLAY or LIST
    		command.


User action:

    		The format for a DISPLAY or LIST command is:

    		1) DISPLAY/LIST
    		2) DISPLAY/LIST parameter name
    		3) DISPLAY/LIST (list of parameter names)
.page
    		In #1, all parameters are displayed or listed.

    		In #2, the specified parameter is displayed or
    		listed.

    		In #3, all the parameters listed in parentheses
    		are displayed or listed.
.KEY TUNEXCH
Explanation:

    		The command line extends beyond the expected
    		end for this command.


User action:
                
    		Type "HELP" to display information on the correct
    		format for each tutor command.                
.KEY TUNRECPAR
Explanation:

    		The word after the tutor command is not
    		recognized as an option associated with
    		that command.


User action:

    		Type "HELP" to display information on the options
    		for tutor commands.
.KEY TYPERRN
Explanation:

    		Only proc interface parameters (declared using
    		PARM) may be declared to have type NAME; NAMEs
    		not supported for global variables, or for 
    		local variables.


User action:

    		Change the declaration to use a different type.
.KEY TYPERR
Explanation:

    		The allowed TYPE values are REAL, INTEGER,
    		STRING, KEYWORD, NAME, or FILE.  In addition, 
		for a STRING declaration, the maximum size 
		of the string may be declared using:

		TYPE = (STRING, <size>)

User action:

    		Correct the command to use one of the types
    		listed above.                
.KEY UNBALCON
Explanation:

    		The FOR, LOOP, and IF commands require 
    		END-FOR, END-LOOP, END-IF, respectively,
    		to close the construct.


User action:

    		Provide the necessary closing command.
.KEY UNBALPAR
Explanation:

    		An IF command is missing a right parenthesis.


User action:

    		Insert the necessary closing parenthesis.
.KEY UNDEFCMD
Explanation:

    		The named command is not a user-defined command.
    		The command is a TCL intrinsic command (which
    		may not be deleted), or the command has not been
    		defined during the current session.


User action:

    		Correct the named command.

    		The list of defined commands may be displayed
    		using "DISPLAY $DEFCMD0".
.KEY UNDEFGBL
Explanation:

    		A procedure contains a REFGBL command for
    		a global variable that has not been defined.

User action:

    	    	Determine if the name of the global is correctly
    		specified.  The following TAE command will
		list the names of all currently defined globals:

			DISPLAY-GLOBALS
.KEY UNDEFSUB
Explanation:

    		A subcommand name was used that is either not defined
    		for this command or is an ambiguous abbreviation.

User action:

    		Type "TUTOR xxx-" to tutor for any subcommand
    		associated with command "xxx", or type 
    		"HELP xxx" to display information on the
    		command.
.KEY UNDEFVAR
Explanation:

    		The named variable has not been defined.

User action:

    		Check the command.  If a LET command is used
    		to set the value of a string variable, 
    		then the string value must be quoted. String
		constants in IF, FOR, and ELSE-IF commands
		must also be quoted. Examples:

			LET STRING = "hello"	!quotes required
			IF (STRING = "hello") 	!quotes required

    		If the variable reference is intentional, then
    		the procedure in which the LET occurs should
    		contain a LOCAL or PARM command for the named
    		variable, or there must be a REFGBL for a global
    		variable and the variable must be defined
    		in a GLOBAL proc.

    		If the variable is a parameter with type NAME,
    		then the referenced variable is not local
    		to the procedure and should be in the scope
    		of the calling procedure.
.KEY UNDLABEL
Explanation:

    		A GOTO command refers to a label that does
    		not exist in the current procedure.

		Note that GOTO commands may only reference 
    		a label in the forward direction.

User action:

    		Correct the procedure to eliminate the
    		reference or insert the label.

		If you need to do a backwards branch, use
		the LOOP command.

.KEY UNEXCH
Explanation:

    		The command line extends beyond the expected
    		end for this command.


User action:
                
    		If the command is a TCL command, type 
    		"HELP <command name>" to display information
    		on the correct format for the command.

    		In tutor mode, or in a menu, "HELP" alone 
    		will display information on all commands.
.KEY UNRECCMD  
Explanation:

    		The named command is not a TCL intrinsic
    		command.


User action:

    		This condition usually arises when the
    		introductory command (PROCEDURE, PROCESS,
    		etc.) in a proc definition file is missing
    		and a proc invocation command appears before
    		the BODY command.
.KEY UNRECSUB
Explanation:

    		The named subcommand is not defined for the
    		associated command.  (The subcommand is
    		the first word following "<command name>-",
    		e.g., in "HELP-PARM", "PARM" is the subcommand.)


User action:

    		Type "TUTOR xxx-" for a list of subcommands 
    		appropriate command xxx.
.KEY UNRECTTY  
Explanation:

		Your terminal is not a known type. Therefore
		it can not be supported by the TAE terminal package.

User action:

		If this is a hardcopy device, no action is necessary.

.if VAX_VMS
		If this is a CRT device, if TERM$TABLOC is defined,
		check that your terminal is of a type listed in the
		file to which it points. If TERM$TABLOC is not
		defined, make sure that your terminal type is listed
		in the default VMS terminal table.
.elseif UNIX
	  	If this is a CRT device, check that the Unix environment
		variable, TERM, is defined to be a terminal type 
		listed in the file pointed to by $TERMCAP. If
		$TERMCAP is undefined, make sure that your terminal
		type is listed in the default UNIX terminal table.
.ifend
.KEY VALIDERR
Explanation:
    		A variable declaration includes a VALID 
    		specification with an incorrect format for
    		the range specification.  

User action:

    		Find the errant declaration and correct the 
    		range specification.  A valid range specification
    		is formatted "a:b" where "a" is the lower
    		number, and "b" is the higher number.  The colon
    		is required and there may not be any spaces
    		in the specification.

    		Example:

    		PARM I TYPE=INTEGER VALID=(1:80, 100:150)		
.KEY VARCNFLCT
Explanation:
		A variable already exists with the same name.
	
		If this error message is received interactively,
		then the name conflicts with a local that
		has been declared interactively or with
		any of the global variables.

User action:

    		Rename the variable.                
.KEY VBADCOUNT
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		The COUNT argument in an XQ call is either 
    		less than the minimum or greater than the 
    		maximum allowed for the specified variable.


User action:

    		Correct the program so that the count
    		is within the variable's limits.
.KEY VBADNAME 
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		The NAME argument in an XR call refers
    		to a variable that does not exist in the 
    		V-block, or an XQ call has the mode argument 
		set to "xupdat" and the NAME argument 
		refers to a variable that does not exist.


User action:

		Correct the program or the PDF so that the 
		NAME argument refers to an existing variable.
.KEY VBADTYPE 
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		In an XR call or an XQ call with MODE set to
    		"xupdat", the type of the variable in the
    		V-block does not agree with the type implied
    		by the name of the subroutine.  (E.g., XRREAL
    		expects a real variable.)


User action:

    		Determine the type of the named variable and
    		correct the program accordingly.                
.KEY VDIMEN   
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		In XR the DIM argument is not large enough
    		to receive the number of values for the specified
    		variable.


User action:

    		Determine the type of the COUNT of the named 
    		variable and correct the program accordingly.                
.KEY VDUPNAME	
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		In an XQ call, the MODE was set to "xadd"
    		and the NAME of the variable was the name
    		of a variable already in the V-block.


User action:

    		Use a different name for one of the variables,
		or use the "xupdat" MODE for the XQ call.
.KEY VNOROOM	
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		In an XQ call, the V-block has insufficient
    		space to accommodate the values specified.


User action:

    		Increase the size of the block.  (See XQINI.)
.KEY VTRANS	
Explanation:

    		This condition is caused by a problem in the 
    		executable code of a process.

    		In XRFILE, a variable containing a file
    		specification was read and could not be
    		translated using the Catalog Manager or the host
    		operating system.


User action:

    		If the user typed the file name correctly 
    		determine why the file specification could
    		not be translated, and correct the program
    		accordingly.
.KEY WINDEXISTS
Explanation:

		An attempt was made to create a window
		with the same name as a window that already
		exists.


User action:

		Use WINDOW-INFO to determine the name of
		windows that already exist.
.KEY WINDOWCREATE
Explanation:

    		In a WINDOW-CREATE operation, the requested 
    		window could not be created.  

User action:

    		Check that the X window system is active, and
    		that you are on an appropriate bit-mapped
    		workstation.
.KEY WPTPANEL
Explanation:

    		The requested panel could not be created.


User action:
    		Check that you have done a WPT-INIT.
    		Check that the X window system is active on
    		your display system.
.KEY NOCOLOR
Explanation:

    		In a WINDOW-DRAW operation, a color was
    		requested that does not exist.


User action:

    		Run the fontbrowse proc to determine the
    		available colors on your system.
.KEY BADQUERY
Explanation:

    		The information requested on a window could
    		not be obtained.
.KEY BADNAME
Explanation:

    		The specified variable does not exist.
.KEY NOFONT
Explanation:

    		The specified font does not exist on this
    		system.


User action:

    		The TAE utility proc fontbrowse can be used
    		to display the available fonts on your system.

.KEY XTRAELSE
Explanation:

    		An ELSE command was encountered without a
    		matching IF.


User action:

    		Correct the procedure to eliminate the unmatched
    		ELSE or insert the IF.                
.KEY XTRAENDFOR
Explanation:

    		An END-FOR command was encountered without a
    		matching FOR.


User action:

    		Correct the procedure to eliminate the unmatched
    		END-FOR or insert the FOR.                
.KEY XTRAENDLP
Explanation:

    		An END-LOOP command was encountered without a
    		matching LOOP.


User action:

    		Correct the procedure to eliminate the unmatched
    		END-LOOP or insert the LOOP.
.KEY XTRELSIF
Explanation:

    		An ELSE-IF command was encountered without a
    		matching IF.


User action:

    		Correct the procedure to eliminate the unmatched
    		ELSE-IF or insert the IF.                
.KEY XTRENDIF 
Explanation:

    		An END-IF command was encountered without a
    		matching IF.


User action:

    		Correct the procedure to eliminate the unmatched
    		END-IF or insert the IF.                
.KEY NOVALUE	!cmdfield
Explanation:
		An attempt to substitute for a TCL variable
		was made and the variable currently has no
		value.  (Substitution is indicated by the
		ampersand character.)

User action:
		If the command line was typed interactively,
		re-type the command, requesting substitution
		only for variables which have values.

		If the error occurred in a proc, then the
		proc definition is incorrect and must be
		corrected.

.KEY VALIDREQ	!declare
Explanation:
		The definition of a proc parameter is incorrect.
		The parameter is declared to be a KEYWORD
		parameter, but the VALID list has been omitted.

		KEYWORD parameters require a VALID list.


User action:
		The PDF must be corrected.  See the TAE Application 
    		Programmer's Manual for a discussion on parameters 
    		of type keyword.
.KEY BADREF	!declare
Explanation:
		The definition of a proc parameter is incorrect.
		The parameter is of type NAME but the DEFAULT		
		specification names a variable which is not defined
		in the proc.   DEFAULT specifications for NAME
		parameters are resolved locally.
User action:
		The proc definition must be corrected.
		

.KEY NOTUSED	!dyncmd
Explanation:
		This error indicates a problem in one of the 
		following areas:

		--A process has malfunctioned.  The process has called
		the XQDYNP subroutine with a V-block that contains
		one or more TCL variables that are not defined in
		PDF identified by the XQDYNP call.
		
		--The parameter file specified by a RESTORE
		command qualifier contains some parameters
		that do not apply to the command.

		This is not necessarily an error condition;
		the parameters that are known to the PDF
		have been properly retrieved.
.KEY DYNBATCH	!dyncmd
Explanation:
		A process attempted to request dynamic parameters
		(by calling the XQDYNP subroutine) in batch mode.
		XQDYNP is not allowed in batch mode.
		

User action:
		A process that calls XQDYNP may only be run
		interactively.

.KEY NUMNULL	!evalexp
Explanation:
		One of the variables in a numeric expression contained
		a null value ("--"). The null value is not allowed in 
		numeric expressions ("+","-","/","*").

User Action:
		The variable must be given a non-null value.

    		Note that if the $COUNT TCL function returns a 0
    		for the count of a variable then the current value
    		is the null value.
.KEY LOGNULL	!evalexp
Explanation:
		One of the variables in a logical expression contained
		a null value ("--"). The null value is not allowed in 
		logical expressions ("AND", "OR", "NOT").

User Action:
		The variable must be given a non-null value.


    		Note that if the $COUNT TCL function returns a 0
    		for the count of a variable then the current value
    		is the null value.
.KEY RELNULL	!evalexp
Explanation:
		One of the variables in a relational expression with the 
		operator ">", "<", "<=", or ">=" contained
		a null value ("--"). The null value is not allowed in 
		relational expressions except for "=" and "<>".

User Action:
		The variable must be given a non-null value.

    		Note that if the $COUNT TCL function returns a 0
    		for the count of a variable then the current value
    		is the null value.
.KEY CONCNULL	!evalexp
Explanation:
		One of the string variables in an expression using 
		the concatenation operator contained a null value ("--"). 
		The null value is not allowed in a concatenation operation.

User Action:
		The variable must be given a non-null string value.

.KEY NULLELEM	!evalexp
Explanation:
		The null value syntax ("--") appeared as a list
		element of a multi-value list.   The null value
		is not acceptable as a component of another value.
		Individual components of a variable may not be 
		nulled; only the variable itself may be nulled.

		
.KEY BADMSG	!exits
Explanation:
		The TAE Monitor has received a data block
		from a process and the block has invalid format.  
		This represents a major failure in the TAE
		Monitor or in the TAE subroutine library.


.KEY NOMSG	!help.c -- doesn't look like this one can happen
Explanation:
		The message key is missing from the HELP-MESSAGE
		command.

User action:
		The HELP-MESSAGE command requires a key.  For
		example:

		HELP-MESSAGE TAE-HELLO

.KEY LONGNAME	!let
Explanation:
		The variable name on the left side of a LET
		assignment statement is too long.  The name
		of TCL variables is limited to eight characters.


.KEY BADIVAL	!let
Explanation:
		The left side of a LET assignment statement
		is indexed but the right side does not represent
		a singly-valued variable or constant.  

		The number of component values on the right side 
		must be exactly one; the null value ("--") is
		not acceptable and multiple values are
		not acceptable.

		Examples:

		LET X(24) = 100		!valid
		LET X(24) = --		!invalid: null value 
		LET X(24) = (1, 3)	!invalid: multiple values
.KEY TOOFEWVAL	!let
Explanation:
		An attempt has been made to set a component value
		of a variable, but the operation is unacceptable
		because the resulting component count would 
		have been inconsistent with the variable's declaration.

		For example, if X were declared to have between
		4 and 8 values and to be nullable, the following
		sequence would produce the TOOFEWVAL error:

		LET X = --	!null value to X
		LET X(1) = 10	!invalid: resulting count would be 1


.KEY TOLENB	!log
Explanation:
		Terminal output logging was already active when
		the user attempted to execute the ENABLE-TOL
		command.

.KEY FORVARBAD	!loops
Explanation:
		The target variable of a FOR statement is not
		recognizable.   The command element following the
		characters "FOR" must be the name of a TCL
		variable and is limited to eight or fewer characters.
		
		The target variable may not be indexed.			

User action:
		Correct the proc definition containing the FOR
		statement and re-run.

.KEY LOMENNUM	!menu
Explanation:
		The menu selection number is less than 1.  
		A menu entry must be selected that is between
		1 and the highest entry number shown on
		the menu display.

User action:
		Re-enter the selection number; type "HELP" for
		instructions on running the menu mode of TAE.

.KEY HIMENNUM	!menu.c
Explanation:
		The menu selection number is greater than the
		highest menu entry shown on the screen.  A
		menu entry selection must be between 1 and the
		number of the highest entry shown on the menu
		display.

User action:
		Re-enter the selection number; type "HELP" for
		instructions on running the menu mode of TAE.


.KEY SYNERR	!misscmd (and etc?)
Explanation:
		The command cannot be processed because the
		format is unrecognizable.  The error message 
    		attempts to identify the area in the command 
    		where the format error occurred.		

User action:
		Re-enter the command.  For information on the
		general rules for commands, type "HELP".  
		

.KEY SUBNST	!misccmd
Explanation:
		An attempt to nest subcommand definitions occurred.
		Within the statements bracketed by SUBCMD and END-SUB,
		only parameter and variable definitions are allowed;
		another subcommand bracket is not allowed.

User action:
		Correct the procedure containing the nested 
		subcommands.

.KEY SUBAVL	!misccmd
Explanation:
		This message lists the names of the subcommands 
		available for the proc that you attempted to execute.
		Subcommands are keywords that select a sub-function
		of a command.

User action:
		Re-enter the command and prefix one of the subcommands
		to the command name.  The subcommand should be separated 
		from the command with a dash.  For example:

			SETLIB-ADD (lib1, lib2, lib3)

		To obtain a description of each subcommand, type the
		following:

			TUTOR command-
		
.KEY SUBERR
Explanation:

		An internal TAE error has occurred while
		tutoring on a subcommand.  The most common 
		cause of the error is that TAE has run
		out of dynamic memory.
.KEY PRCSACT	!misccmd
Explanation:
		The program associated with the proc cannot be found
		on disk.   If the "EXECUTE=" specification is omitted
		from the "PROCESS" statement for the proc, then the
		program name is assumed to be in the same library as
		the proc.

		If the "EXECUTE=" specification is present, then the
		file name cannot be accessed; either the file does
		not exist or the the file is protected from access.
User action:
		Correct the proc definition or create the program file.

.KEY PARBLK	!pdfprc
Explanation:
		TAE has received a data block from a process and the 
		block is incorrectly formatted.  This usually
		means that the program has called the XQDYNP subroutine
		or the XQOUT subroutine with a V-block that is
		improperly formatted.  This can be caused by one
		or more of the following:

		--The V-block was properly constructed but has been
		destroyed at some point before the XQ call.

		--The V-block was not properly constructed using the
		XQ subroutines.   A typical sequence of V-block
		construction is: XQINI, XQINTG, XQREAL, and XQSTR.

.KEY SAVEWRT !saveres
Explanation:
		The disk file that is to hold the variable values from
		a SAVE command cannot be written.  This typically
		indicates a disk error or a disk space quota problem.

.KEY NOGBLRES	!saveres
Explanation:
		This message indicates that the disk file referenced
		on a RESTORE command does not contain any global
		variables.  This is a information message and may not
		necessarily indicate that an error condition exists.

		The message may be avoided by using the 
		"RESTORE-LOCALS" command.

.KEY NOLOCRES !saveres
Explanation:
		This message indicates that the disk file referenced
		on a RESTORE command does not contain any local 
		variables.  This is a warning message and may not
		necessarily indicate that an error condition exists.

		The message may be avoided by using the 
		"RESTORE-GLOBALS" command.

.KEY FMTRESTR	!saveres
Explanation:
		The file used in a RESTORE command does not have
		the proper format, i.e., the file was not created
		by the SAVE command.  

		This message might also indicate that the file
		was created by SAVE, but while running under a different
		version of TAE.  Parameter files are not necessarily
		compatible between different versions of TAE.		
.KEY MAXPNEST	!tm
Explanation:
		
    		The maximum number of nested procedure calls has
    		been reached. This error usually occurs when a 
    		procedure accidently activates itself, and there 
    		is no special logic to resolve the recursion.


User action:

    		Correct the procedure's self-reference or reduce
    		the depth of nesting required by your application.
    		See the TAE System Manager to determine the
    		maximum depth available on your system.
.KEY NOTNULL	!tm
Explanation:
		An attempt was made to assign the null value to 
		a parameter, but the definition of the parameter 
		does not permit	the null value.  The null value
		is indicated by two adjacent dashes, i.e., "--".

User action:
		The "--" must be replaced by a valid value.
		You can learn more about the specific parameter
		by entering tutor mode and requesting help
		on the parameter.  If you are already in tutor,
		type "HELP parm".
.KEY KEYSYNTAX
Explanation:
		The apostrophe character has been incorrectly used in
		a command line.  Normally, the apostrophe is used
		to "flag" keyword values, and every unquoted apostrophe
		must be followed by a keyword value.		

User action:
		Re-type the command: if the apostrophe was intended
		to flag a keyword, correct the command; if the
		apostrophe was unintentional, remove the apostrophe.

.KEY KEYWORD	!tm
Explanation:
		This error indicates that an invalid or ambiguous
		keyword has been used in a command string.  The
		keyword was flagged with the apostrophe character.

		The keyword may have been used for a parameter or
		for a qualifier.

User action:
		Either the keyword needs to be correctly spelled
		or more characters are required to resolve the
		ambiguity.   The keyword value must be uniquely
		identified among all of the keyword values for
		the proc.
.KEY BADPAR	!tm
Explanation:
		The parameter is not known to the command,
		or an unknown command or parameter qualifier 
    		has been used. 	Every command (or proc) has 
    		a defined set of parameter names, and only 
    		parameters from this set may appear on the 
    		command line.

		Note that the existence of a subcommand 
		changes the set of parameters, i.e.,
		a command may have different parameter
		sets depending upon the subcommand in use.

User action:
		Re-enter the command, naming the parameters
    		and any command or parameter qualifiers
		correctly.  You may use the TUTOR command 
		to get an exact list of the parameter names.		

		Type "HELP" for a description of command and
		parameter qualifiers.  See the TAE User's
    		Manual for details on qualifiers.
.KEY TTLERR
Explanation:
		The title information for a tutor, menu, or
		help screen cannot be read from disk.  This
		message usually indicates a disk failure.		

.KEY TUTOERR
Explanation:

		The error message just prior to this one
		explains the cause of this error.  If there
		is no such error message, then the error
		is an internal TAE error.
.KEY TUTSUB	!tutor
Explanation:
		The ampersand character was used incorrectly
		in a tutor command.   When an ampersand is
		used, it must be followed immediately by the
		name of a TCL variable--the value of the
		variable is then substituted, replacing the
		ampersand and the variable name.

		An error occurs when an ampersand is not 
		followed by a variable name or when the
		variable is not known on the interactive 
		level.

		If you want an ampersand character itself	
		to appear in a quoted string, use two
		adjacent ampersands.
.KEY TNOLEV2	!tutor
Explanation:
		The programmer who created the proc has not
		specified detailed help for the parameter
		or subcommand.

User action:
		Often the description of the proc itself will
		supply additional information concerning
		parameters.  Type "HELP *".

.KEY COMPNULL
Explanation:
		The null value syntax ("--") appeared as a list
		element of a multi-value list.   The null value
		is not acceptable as a component of another value.
		Individual components of a variable may not be 
		nulled; only the variable itself may be nulled.

User action:
		Re-enter the command replacing the "--" with
		an actual value.
.KEY SAVELAST
Explanation:	Tutor is unable to save the current parameter set
		on disk. (Whenever you RUN from tutor mode, your
		set of parameters is saved in a parameter file
		named "LAST".)   This error is usually caused by
		one of the following:

		-- The current default directory is protected
		against file creation.

		-- Your disk space or file quota has been exceeded.

		-- In VMS, the logical name LAST has been assigned
		to some file specification that is not valid.

User action:
		Despite the error message, tutor has properly 
		executed the proc.  
.KEY TSHOWPRM
Explanation:
		The SHOW command requires that you type the
		name and index of the value that you want
		to see.   


User action:
		Re-type the SHOW command.  Example:

			SHOW X(4)

.KEY KEYCONFLICT		!valid
Explanation:
		The proc definition is incorrect.  There exist
		one or more keyword parameters that have
		conflicting (i.e., duplicate) keyword values.

User action:
		The proc definition must be corrected. Each
		parameter declared TYPE=KEYWORD must have a	
		VALID list of keywords that are unique among
		all keyword values in the proc.

.KEY VBADPFILE		!parmfile
Explanation:
		An application program attempted to read a
		parameter file by calling the XRRDB subroutine
		and the parameter file is incorrectly formatted.
		Some possible causes of this error are:

		-- The file is not a parameter file.  Parameter
		files must be created by TAE's SAVE command or
		by calling XQWRTB from a program.

		-- The file was incorrectly created by XQWRTB
		because the V-block argument was not properly
		constructed.

		-- The file is a parameter file created by an
		old version of TAE.

.KEY OVER
Explanation:
		An application program has malfunctioned.
		The file name being received by a call to
		XRFILE is longer than the declaration of
		the character variable designated to receive
		the name.		

		The program has been terminated at the
		point of the XRFILE call.
.KEY EMPTYASY
Explanation:

		The user attempted to remove, abort, or wait on a 
		specific asynchronous job (or all asynchronous jobs)
		when the asynchronous job list was empty.


.KEY ABORTOK
Explanation:

		This is an information message, indicating that 
		the specified asynchronous job has been 
		successfully aborted.


.KEY NOABO
Explanation:

		The specific asynchronous job could not be 		
		aborted because it is no longer active.  This 
		means the job either has run to completion, or 
		has already been aborted by the user.


		This message is also generated in response to a 
		REMOVE command for a specific or ALL asynchro-
		nous jobs, showing the name of the active job(s)
		that are not removed from the async job list.  

.page
User action:
		
		- None, if the job to be aborted is already 
		  complete.
		- None, if you have used the REMOVE ALL command.
		- Else, (in case of REMOVE job name) wait until 
		  the job is completed, and then reissue the 
		  command. (You may abort the job if you do not
		  want to wait until it completes.)

.KEY NOJOB
Explanation:

		The user specified asynchronous job is not in 
		the async job list. Either the job was never 
		submitted for asynchronous execution, or
		the job has already been removed from the list.


User action:

		Check the job name, and if the name was 
		misspelled, re-enter the command with the 
		correct name.

    		You may display the async job list using
    		SHOW-ASYNC.
.KEY ZEROJOB
Explanation:

		The user wanted to abort all asynchronous 
		jobs when there was none active.


User action:

		Check the list of completed asynchronous jobs 
		using SHOW-ASYNC. 
.KEY RESALL
Explanation:

		TAE reserves the word "ALL" to imply all 
		asynchronous jobs. 


User action:

		Resubmit the job with a different acceptable 
		job name.
.KEY LONGJNAME
Explanation:

		The user specified asynchronous job name is longer 
		than the maximum limit allowed by TAE.


User action:	

		Check the maximum allowed length specified in 
		the error message. Resubmit the job with a 
		shorter name.
.KEY USEDNAME
Explanation:

		The user has already submitted another asynchronous 
		job with the same job name.

User action:

		Either submit the  asynchronous  job with a 
		different name, or if the previous job with that 
		name is complete, remove it from the list (using 
		the REMOVE command) and then resubmit the current 
		job.
.KEY TOOASYNC
Explanation:

		If a proc is repeatedly submitted to run 
		asynchronously, TAE maintains unique job names 
		for the proc by appending a unique two digit 
		sequence number to the proc name.  If all the 
		sequence numbers have already been assigned, it 
		can no longer create a job name for that proc.


User action:

		Either submit the asynchronous job with a
		unique, explicit job name, or remove the completed 
		asynchronous jobs from the list (using the 
		REMOVE command) and resubmit your job.
.KEY NOASYROOM
Explanation:

		
		The TAE Monitor maintains housekeeping 
		information for up to 100 asynchronous jobs.  
		If you submit more than 100 jobs
		without removing completed jobs from 
		the list, TM tries to remove the first inactive 
		job from the list to make room for the new job.  
		However, if all the jobs in the list are active, 
		the new job cannot be run asynchronously.


User action:

		Wait until at least one active job is complete. 
		You may check that by typing the SHOW-ASYNC 
		command. Then resubmit your job.
.KEY REMOVE
Explanation:

		If you submit more asynchronous jobs than TM can 
		maintain in its asynchronous job list, TAE tries 
		to remove the first  inactive job from the async
		list to make room for the incoming job.  If 
		successful, TM informs you the name of the removed 
		inactive job with this message.


User action:

		None
.KEY NOWAIT
Explanation:

		The user issued a reply command with no job name 
		when there are no asynchronous jobs waiting for 
		dynamic parameters, or, a reply command is issued 
		for a specific job which is not waiting.


User action:

		Check the asynchronous job status by typing 
		SHOW-ASYNC.  In case of a misspelled job name, 
		re-enter the original command with the correct 
		name.
.KEY REPWAIT
Explanation:

		Either there was a request to wait on a job
		that is itself waiting for dynamic parameters,
		or a request for dynamic parameters came from
		a job during a wait.

		If a job is waiting for dynamic parameters,
		the job must be explicitly continued before
		you can wait for its completion.


User action:

		Use the REPLY command to either abort or
		continue the job.		
.KEY ASYSND
Explanation:

		The interactive TAE job is unable to send
		parameters to the asynchronous job.  The 
    		cause of the problem is supplied as the 
		host-dependent error code in the error 
    		message.


User action:

		Find the explanation for the host-dependent error 
		code printed with the message.  Correct the problem,
    		if possible, and resubmit the job.  If you cannot
    		correct the problem see your System Manager.
.KEY ASYRCV
Explanation:

		An error has been encountered by TAE in receiving and 
		processing a dynamic parameter request from 
		an asynchronous job.


User action:

		Find the explanation for the host-dependent 
		error code printed with the error message.  
		See your system manager if you cannot correct
    		the problem.
.KEY FMTCOMM
Explanation:

		This is a TAE internal error. 

		When an asynchronous job requests dynamic 
		parameters, the data communication between the 
		asynchronous job and the interactive TAE monitor 
		takes place in the form of files.  In this case,
		the file sent by the async job was found to 
		have the incorrect format.
.KEY COMOPEN
Explanation:

		The TAE monitor could not open the file, either 
		containing information from the asynchronous 
		job, or to be sent to the asynchronous job.
		This may happen if you do not have write
		access to the current default directory.

User action:

		Check the host-related explanation  given in the error 
		message. If not related to protection violation,
		inform your system manager.
.KEY COMREAD
Explanation:

		After a REPLY command, the TAE monitor could not 
    		read the asynchronous job communication file.
		

User action:

		Check the explanation given in the error message.  
		Inform your system manager.
.KEY COMWRITE
Explanation:

		This is a TAE internal error.

		After a REPLY command, the TAE monitor could not 
    		write the dynamic parameter values to the asynchronous 
    		job communication file.


User action:

		Check the host-specific explanation given in the 
    		error message.  Inform your System Manager.
.KEY ASYCRE
Explanation:

		TAE is unable to initiate an asynchronous job 
		because certain resource quotas (assigned to the 
		user by the system manager), have been exceeded.
		
.page
User action:

		If your problem is related to resource quotas 
		(indicated by a VAX/VMS error code of 28), and you
		do not have any asynchronous jobs active, see
		your system manager to increase your quotas. 
		The TAE System's Manager guide 	has guidelines 
    		for quota assignments.		

		Otherwise, if you have active asynchronous jobs, 
		wait until one of them completes. You may 
		check the status of asynchronous jobs with
		the SHOW-ASYNC command.

.KEY ASYNCJOB
Explanation:

		This message informs the user that TAE was 
    		successful in creating the asynchronous job.
		Note, however, that this does not guarantee 
    		that the job will run; the job may abort 
    		because of system quota limitations after 
    		a successful initiation.


User action:

		Check the status of the submitted job by typing
		SHOW-ASYNC. If the job could not be run, the SKEY 
		field should contain a key value such as 
		TAE-COMINI.
.KEY COMWRT
Explanation:

		Following a REPLY command, the TAE monitor could 
    		not write the dynamic parameter values, to the 
    		communication file sent to the asynchronous job.

		This is an internal or host system error.


User action:

		Check the explanation given in the error message.  
		Inform your System Manager.
.KEY MISMATCH
Explanation:

		A target variable of a RESTORE operation does
		not have the proper definition for the corresponding 
    		variable in the parameter file.

		For example, a REAL variable in a parameter file
		was targeted for an INTEGER variable, or a "valid"
    		list changed after the variable was SAVEd.

User action:

		Check for correct file name if 	you are restoring 
		explicitly by using the RESTORE command. 
.KEY NOWAIT
Explanation:

		You cannot wait on an asynchronous job that is 
    		itself waiting for dynamic parameters from the 
    		user. If you type "WAIT-ASYNC name", to wait on
    		job "name" (or ALL), TAE displays the name(s) 
    		of the job(s) that are waiting for dynamic 
    		parameters.
.KEY ASYPARM
Explanation:
		
		An asynchronous proc invoked by either a batch or
		asynchronous job requested dynamic parameters.
		TAE does not support this request because there is
		no associated interactive user.
.KEY RCVMON
Explanation:	
		
		The asynchronous job, waiting for dynamic parameters
		from the interactive TAE monitor, was unsuccessful
		in receiving a reply message. This may happen due 
    		to a TAE internal error or a host system problem.


User action:

		Find an explanation for the host error code printed
		with the error message. If you cannot correct the 
    		problem, see your System Manager.
.KEY ASYRCV
Explanation:	
		
		The asynchronous job, waiting for dynamic parameters
		from the interactive TAE monitor, was unsuccessful
		in receiving a reply message.


User action:

		Find an explanation for the host error code printed
		with the error message. If you cannot correct the 
    		problem, see your System Manager.
.KEY ASYNCREQ
Explanation:

		This message informs the user that one or more 
    		asynchronous jobs are waiting for dynamic parameters.

		The REPLY command may be used to supply the
		parameter values to the asynchronous job.
		Type "HELP REPLY" for additional information.

User action:

		If more than one asynchronous job is active,
		find the job(s) requesting the dynamic parameters
		by typing the SHOW-WAITING command. Then use the
		REPLY command, with job name if necessary, to
		initiate a dynamic parameter session.
.KEY NOTASYNCMD
Explanation:

		An asynchronous proc is trying to execute a TCL
		intrinsic command that is not available from 
		asynchronous jobs. An example is the ENABLE-LOG
		command.


User action:
		
		Delete the command from the proc, or run the proc
		interactively.
.KEY BADTYPE
Explanation:

    		There is a type mismatch between a declared parameter 
    		and the value provided for the parameter on the proc 
    		invocation line.

    		This often occurs when the de-reference operation
    		is used on a variable of a type different than the type
    		expected by the proc.

    		Example:

    			local i integer
    			display @i	!'display' wants a string


User action:

    		Correct the invocation line so that the
    		proc parameters use the expected data types.
.KEY MSGHANDS
Explanation:

		When an executing process sends a message to 
		the TAE Monitor (via the XMPUT subroutine call),
		TM sends a handshake message back to the process
		acknowledging the reception of the original 
		message. The process is unable to receive this
		handshake message.

		This is an internal error. 

.KEY NOPARMRES
Explanation:

		The user requested that TAE restore all 
    		the parameters from a saved file in which
		no parameters there are no parameters.


User action:
		
		Check the file name for correctness.
.KEY NOVAR
Explanation:

		The user requested that TAE restore the value(s) 
    		of a global, local, or parameter variable that 
		does not exist in the specified parameter file.


User action: 

		Check the variable name and the saved file name
		for correctness.  The DISPLAY-PARFILE command
		may be used to display the variable names in
		the parameter file.
		
.KEY BADTARG
Explanation:

		The user had issued a RESTORE command to restore 
		a set of variables from a previously saved file 
		to a different set of target variables. The number
		of VARIABLE values does not match the number
		of TARGETs.


User action:

		Reenter the command with matching number of 
		targets. 
.KEY NOGLB
Explanation:

		The user requested help on a TCL global variable
		that does not exist.


User action:

		Check the name of the global variable.  You may 
    		display the existing globals by using the DISPLAY-GLOBAL 
    		command. 
.KEY LONGINDX
Explanation:

		The variable name or expression provided for a variable 
    		index in a LET statement is too long. For example:

			LET var(THIS FIELD) = XXX


User action:

		Shorten the index value length to be shorter than 
		or equal to the number of characters specified 
		in the error message.
.KEY NOTNAME
Explanation:

		The argument to a $COUNT or a $GLOBAL function 
		is not a variable name. For example: 

	    	LET I = $COUNT(1),  or  LET J = $GLOBAL(123).

		Here the arguments 1 and 123 are numbers rather 
		than names.
	
User action:

		If the variable name is misspelled, type it
		correctly. 
.KEY RESTPARM
Explanation:

		TAE does not allow parameters to be created
		by a RESTORE command.  There exists a 
		parameter in the file for which there is 
		no corresponding parameter in the proc
		of the RESTORE command.

.page
User action:

		You may first create a local variable corresponding 
		to each non-existent parameter and then RESTORE 
		the parameters into these locals using the TARGET 
		key word.
.kEY JOBOPN
Explanation:

    		While starting a batch or asynchronous job, TAE 
    		encountered a file error opening a JOB file.  The 
    		JOB file is used to communicate the job's parameters 
    		to the asynchronous or batch TAE Monitor.

    		This error is generally caused by one of the 
    		following:

    		(1) a file quota problem, or

    		(2) a privilege violation (generally occurring
    		    when you run in someone else's account)

.page
User action:

    		Interpret the host-specific error code and
    		confirm that you have access to files in the
    		current directory.  If you do, see the System 
    		Manager to determine if you have sufficient 
    		file quota.
.kEY JOBWRT
Explanation:

    		TAE encountered a file error writing to a JOB 
    		file. The JOB file is used to communicate the 
    		batch or asynchronous job's parameters to the 
    		batch or asynchronous TAE Monitor.

    		This typically indicates a disk error or a disk 
    		space quota problem. 


User action:

    		Interpret the host-specific error code and
    		confirm that you have access to files in the
    		current directory.  If you do, see the System 
    		Manager to determine if you have sufficient 
    		disk space quota.
.key TRANSFAIL
Explanation:							** VAX/VMS **

    		On VAX/VMS systems, TAE finds the current directory
    		tree (which contains all TAE system files) using
    		the VMS logical name "TAE".  


User action:

    		Check that the logical name "TAE" is defined in 
    		your user process; if it is, see the System Manager.
.KEY JOBREDUN
Explanation:

    		This message is for information only.

    		The JOB command qualifier and the second component
    		of the RUNTYPE qualifier may be used to specify
    		an asynchronous or batch job name.  If both are
    		specified, the second component of the RUNTYPE
    		qualifier is ignored.


User action:

    		If you have specified the command qualifiers 
    		positionally, check to see that the values
    		you specified are the ones intended.
.key ASYONLY
Explanation:

    		The command you typed is only available from
    		within an asynchronous job;  see the TAE
    		Application Programmers' Manual for 
    		an explanation of the use of this command.
.key ARESTERR
Explanation:

    		In an asynchronous job, there was an error
    		updating the job context with the results of
    		a dynamic parameter session.

    		Because the results are communicated to the
    		asynchronous TAE Monitor through files, this
    		error generally occurs when another user
    		has created a file with the same name as
    		the communication file.  


User action:
    		See the TAE System Manager for an explanation
    		of the naming of TAE communication files.
.KEY ABOWAIT
Explanation:

    		This is the value of the TCL global variable
    		$SKEY after the user has used the attention
    		sequence to abort a WAIT command.
.key RDEOF
Explanation:

    		During the second phase of a PDF compilation, 
    		an unexpected end-of-file was encountered while 
    		re-reading the partially compiled file.

    		The file was corrupted during the compilation.


User action:

    		Re-try the compilation; if it fails again, see the
    		TAE System Manager.    		    		
.key WRGREC
Explanation:

    		During the second phase of a PDF compilation, 
    		an unexpected record format was encountered while 
    		re-reading the partially compiled file.

    		The file was corrupted during the compilation.


User action:

    		Re-try the compilation; if it fails again, see the
    		TAE System Manager.    		    		
.key WRTERR
Explanation:

    		An I/O error was encountered while TAE was writing
    		the compilation output file.  


User action:

    		Interpret the associated host error code and take
    		the appropriate action.
.key EOFERR
Explanation:

    		An unexpected EOF was encountered while TAE was
    		reading the symbol table in a compiled PDF.


User action:

    		This error message generally indicates that the 
    		compiled PDF was corrupted (or is not, in fact,
    		a compiled PDF).
.key CPDFMALF
Explanation:

    		The symbol table in a compiled PDF was not
    		properly formatted.


User action:

    		This error message generally indicates that the 
    		compiled PDF was corrupted (or is not, in fact,
    		a compiled PDF).
.key UNEXPECTH
Explanation:

    		Help text was encountered in the middle of a
    		PDF, i.e., before the "end-proc".


User action:

    		Inform the author of the proc that the help text
    		is located in the incorrect place in the PDF.
.key CPDFBHDR
Explanation:

    		The header in a compiled PDF was not
    		properly formatted.


User action:

    		This error message generally indicates that the 
    		compiled PDF was corrupted (or is not, in fact,
    		a compiled PDF).
.key COMPDEFG
Explanation:

    		A PDF contains a TCL command that cannot be
    		compiled.


User action:

    		See the proc author or remove the command from
    		the PDF.  See the TAE Application Programmer's 
    		Manual for a list of commands that cannot be
    		compiled.
.key REFS
Explanation:

    		A proc contains more REFGBL statements than are
    		supported at this TAE installation.


User action:

    		Delete one or more of the REFGBL statements.  See
    		the TAE System Manager to determine the number
    		of REFGBL statements supported at your installation.
    		(The number is defined by the MAXREF parameter
    		in the TAECONF include file.)
.key DYNINTERN
Explanation:

    		A program attempted to initiate a dynamic parameter
    		session for an internal proc.

    		TAE does not support requests for dynamic parameters
    		in internal procs.


User action:

    		In the requesting program, check the name of the file 
    		for which dynamic parameters were requested.
.key NULLARG
Explanation:

    		A null argument--a variable with the "null" value--
    		was passed to a TCL function that does not support 
    		null arguments.


User action:

    		Determine why the argument was null and correct
    		the problem.

    		See the TAE Application Programmer's Manual for an
    		explanation of TCL functions and null arguments.

.key UNEXEOL
Explanation:

    		The TAE expression analyzer encountered the end
    		of the line before the line analysis was complete.


User action:

    		This error message generally indicates an expression
    		was encountered that does not conform to the TCL
    		rules for expression.  Examples are arithmetic
    		expressions that end with an operator (e.g., "1-")
    		or omitted right parentheses.
.key MISPROC
Explanation:

    		A help request was given without a proc specified.

    		This may occur if, in a help-parm or help-global 
    		request, a subcommand was given with no command.
.key NOSUBHELP
Explanation:

    		No help text is defined in the PDF or help file
    		for the associated specified sub-command.


User action:

    		See the author of the proc.
.key AMBIGVAL
Explanation:

    		An "ambiguous" value was assigned to a variable.
    		An ambiguous value is a value that is the left
    		substring of more than one of the valid values
    		for the variable.


User action:

    		Determine the valid values for the variable (by 
    		requesting help on a parameter in tutor or by
    		finding the declaration statement for the variable
    		and consulting its "VALID" list), and re-type
    		the errant command using a unique substring of
    		the desired value.
.key SCRCNT
Explanation:

    		One of the parameters to the ENABLE-SCRIPT command
    		is the number of times the script is to be executed.
    		
    		A negative number for this parameter has no meaning.
.key PRCDONLY
Explanation:

    		The TCL command you typed is allowed only in TAE 
    		procedures.

    		See the TAE User's Manual for a list
    		of commands available to a terminal user.
.key BADONFAIL
Explanation:

    		In a procedure, the local variable _ONFAIL was
    		set to a command that is not permitted as an 
    		"onfail" command, or the onfail command is
    		is an improper command (an unsatisfied GOTO, 
    		for example).


User action:

    		Correct the procedure to set _ONFAIL to a valid
    		command.  See the TAE Application Programmer's 
    		Manual for a list of allowed "onfail" commands.
.key NONAME
Explanation:

    		A proc contains another proc, but the contained
    		("internal") proc does not have a name.

    		PROCESS, PROCEDURE, and PARMSET commands that are
    		internal to another proc must be identified by
    		a value for the "NAME" parameter.


User action:

    		If an internal proc is desired, name it using a
    		value for the NAME parameter.

    		See the TAE Application Programmer's Manual for
		details on internal procs.
.key DUBINTNAME
Explanation:

    		There are two internal procs with the same name.

    		Internal procs are identified by PROCESS, PROCEDURE, 
    		or PARMSET commands that have a value for the
    		NAME parameter.
.key LOCCONFLICT
Explanation:

    		An attempt has been made to declare a TCL global 
    		variable with the same name as an existing local
    		variable.  Global variables must have names different
    		from any local variable on the current procedure 
    		level.


User action:

    		Use DISPLAY to list the names of all variables,
    		then rename one of the variables in conflict.
.key INTPDF
Explanation:

    		TAE determined that the requested proc was an internal
    		proc, but could not re-open the PDF in which the
    		proc is located.

    		Internal procs may only be invoked from within the
    		PDF that declares them.  If the PDF cannot be
    		re-opened then there may be one of the following 
    		problems:

    		(1)  the PDF was inadvertently deleted;

    		(2)  there is insufficient file quota;

    		(3)  there is a host system or TAE internal 
    		     problem.
.page
User action:

    		Verify that the PDF is still in the expected directory
    		(you can use tutor to do this), then see the TAE System 
    		Manager.
.key PROCSYN
Explanation:

    		The name of the proc to be invoked does not follow
    		the TAE rules for proc names.


User action:

    		Correct the name.   The rules for correct proc
    		names are given in the TAE User's 
    		Manual.  (If the proc name you have given is
    		a host file spec, see the appendix A to that 
    		manual.)
.key ABORT
Explanation:

    		The TCL global variable $SKEY will assume this 
    		value after you have typed an ABORT command.
.key BADDECL
Explanation:

    		A command appeared in a PDF before the BODY 
    		command for procedures, or anywhere for processes,
    		that cannot be processed by the TAE interpreter.

    		Only TCL "declaration" commands may appear in
    		process PDFs or before the BODY command in
    		procedures.

    		
User action:

    		If the proc is a procedure, check that the BODY 
    		command is in the correct place.  

    		See the TAE Application Programmer's Manual for
    		the list of TCL declaration commands.
.key BADCOMBO
Explanation:

    		A RESTORE command may restore a specified variable
    		into an existing variable of a different name.
    		You may not restore into a target variable if
    		you are restoring all variables from a file.
.key BADJOBFILE
Explanation:

    		The mechanism by which a TAE batch or asynchronous
    		job receives information from the initiating parent
    		is through a "job" file.

    		If this message appears, the job file has been corrupted.


User action:

    		See the TAE System Manager to list the job file and
    		determine the problem.
.key NOSYNC
Explanation

    		In proc interrupt mode--the mode TAE enters after 
    		the attention sequence is typed--a user may type 
    		most TCL intrinsic commands, or initiate an asynchronous 
    		or batch proc.  A user may not invoke a proc in proc
    		interrupt mode unless the proc is to be run asynchronously 
    		(RUNTYPE qualifier set to "ASYNC") or in batch (RUNTYPE 
    		set to "BATCH").
.key ITRPQUAL
Explanation

    		In a proc, a parameter is qualified by setting the 
    		QUAL parameter of the PARM declaration to the name 
    		of a proc that defines the qualifiers.  This proc 
    		must be an internal proc or another PDF; it cannot 
    		be an intrinsic command.


User action:

    		Check the name of the referenced qualifier proc.
    		If it is the name of an existing proc, the proc
    		must be renamed.
.key RUNINT
Explanation:

    		Internal procs may only be run interactively
    		that is, they may not be invoked with the 
    		command qualifier RUNTYPE set to "BATCH" or
    		"ASYNC".

    		See the TAE Application Programmer's Manual for
    		details on internal procs.
.key COMPDRFG
Explanation:

    		A proc that de-references a global variable 
    		(using the @ symbol) in the declaration section 
    		of the proc cannot be compiled.
.key BADPQUAL
Explanation:

    		A proc has been invoked with an incorrectly formatted
    		parameter qualifier.  A parameter qualifier is a string
    		between two vertical bars following the value of a 
    		parameter.  Parameter qualifier values have the
    		same format as parameters.


User action:

    		See the TAE User's Manual for a description
    		of parameter qualifiers and the proper format for
    		parameter values.
.key TNOQUAL
Explanation:

    		The QUALIFY command in tutor mode tells TAE to enter
    		tutor mode for parameter qualifiers.  In this case, 
    		there are no parameter qualifiers defined for the 
    		specified parameter.


User action:

    		Check the name of the parameter and check the declaration
    		of the parameter in the PDF.

    		The method for declaring parameter qualifiers is
    		described in the TAE Application Programmer's Manual.
.key TINVSUBHLP
Explanation:

    		Help on a subcommand other than the current subcommand
    		has been requested.  The "current" subcommand is
    		established in tutor when you name it in a tutor
    		subcommand display, or, by default.


User action:

    		The "current" subcommand is indicated in the upper
    		left corner of the tutor mode display.  If you
    		wish help on another subcommand you must request 
    		it from TCL (using "HELP xxx-yyy" to get help on 
    		subcommand yyy in proc xxx) or enter tutor mode 
    		for subcommands ("TUTOR xxx-").
.key TQUNTCMD
Explanation:

    		The requested command is not available when tutoring
    		for parameter qualifiers.


User action:

    		The list of available commands is given at the bottom
    		of the qualifier screen.

    		For an explanation of each command, type HELP.
.KEY TSLUNTCMD
Explanation:

    		The requested command is not available when using the
		tutor select display.


User action:

    		The list of available commands is given at the bottom
    		of the tutor select screen.

    		For an explanation of each command, type HELP.
.key VNOPARM
Explanation:

    		A program has attempted to add a qualifier value
    		to a V-block when the parameter being qualified
    		does not exist.


User action:

    		Check the program against the PDF and make sure
    		that the parameter being qualified is defined
    		in the intended PDF.
.key VOLDPARM
Explanation:

    		A program has attempted to qualify a variable
    		that was created under a version of TAE prior
    		to Version 1.3.

    		Versions of TAE prior to version 1.3 did not
    		support parameter qualifiers.  Variables
    		saved under old versions of TAE cannot be
    		qualified.
.key BADSIZE
Explanation:

    		A program has attempted to set a string variable
    		to a string with more than the maximum number
    		of characters supported.


User action:

    		See the TAE System Manager to determine the 
    		maximum number for characters and correct the
    		program.  (The maximum is set by the MAXSTRSIZ
    		parameter in the TAECONF file.)
.KEY STDOPEN
Explanation:

    		This key is returned as status in $SKEY when
    		a program cannot open the standard output.

    		The standard output, used for default program
    		output, is specified using the STDOUT command 
    		qualifier; it defaults to a the terminal
    		if the proc is run interactively.


User action:

    		Check the name of the standard output file;
    		confirm that you have privileges to write to
    		that file.
.page
    		See the TAE User's Manual for more
    		details on standard output.
.key NONE
Explanation:

    		This string appears as the value of the $SKEY
    		TCL global after a listing command has been issued 
    		that results in no entries listed.

.key EXIT
Explanation:

    		This string appears as the value of the $SKEY
    		TCL global after dynamic tutor session has
    		been terminated using EXIT.
.key NODISABLE
Explanation:


    		A path cannot be disabled while a job is active.


User action:

    		Abort the current job, or wait for it to terminate.

    		To show the status of all jobs, use SHOW-ASYNC; to
    		abort a job, use ABORT-ASYNC.
.key BADPROC
Explanation:


    		An incorrectly formatted proc specification
    		was entered. See the TAE User's Manual for 
    		the correct format of proc specs.
.key MAXREDEF
Explanation:


    		The LOCAL-REDEF command does not support lists
    		of variables.
.key NODYNPDF
Explanation:


    		The name of a PDF to be used for dynamic tutoring
    		was not found where expected.

    		This is an indication of a problem in the TAE 
    		software.
.key PARSIZE
Explanation:


    		The structure used to send information for dynamic
    		tutoring (the "parblk" structure) is not large enough 
    		to handle all the variables requested.
.key STDOUTOPEN         ! similar to STDOOPN and STDOPEN
Explanation:


                This key is returned as status in $SKEY when
                a program cannot open the standard output.

                The standard output, used for default program
                output, is specified using the STDOUT command 
                qualifier.


.page
User action:


                Use local system utilities to check if the
                file exists and to check if you have the
                appropriate privileges to write to the
                file.  
.if VAX_VMS

                If you have used the DCL command, SET DEFAULT, 
                to change to another user's directory, you may 
                not have the appropriate privileges.
.ifend

                Use the local operating system I/O error code
                listed in the error message to determine the
                source of the error.  See the documentation
                on the operating system or the System Manager
                for further action.             

.page
User action:


                See the TAE User's Manual for more
                details on standard output.
.page
User action:


    		See the TAE User's Manual for more
    		details on standard output.
.key ASYINTRPT
Explanation:

		This is an information message.

		The user typed the attention sequence
		(e.g., CONTROL/C) after	"REPLYing" to
		and "RUNning" an asynchronous proc.

		One reason for doing this might be that
		the TM subprocess created to execute the
		asynchronous job was terminated.  The
		parent TM then "hangs" while attempting
		to send a message (via the reply_do
		subroutine call) to the TM subprocess.

.page
User action:

		See system manager to determine why the
		TM subprocess was terminated.
.key DELVAR
Explanation:
		Certain local variables are implicitly defined
		for all procs and may never be deleted.

		The DELETE-LOCALS TCL command causes this message
		to be displayed and all non-implicit local variables
		to be deleted.

		The implicitly defined local variables are the
		following:

			_PROC   : the name of a proc
			_SUBCMD : the active subcommand
			_STDOUT : standard output file name
			_ONFAIL : command to be executed upon error

		Only _STDOUT is implicitly defined when TM is in
		interactive mode.
.page
User action:


    		See the TAE Application Programmer's Manual
		for more details.
.key NOBATCHCMD
Explanation:
		
		A batch job attempted to request dynamic parameters.
		TAE does not support this request because there is
		no associated interactive user.

		The TCL command GETPAR requests dynamic parameters
		for procedures.

		The TAE library routine XQDYNP requests dynamic
		parameters for processes.

User action:

		Submit the job interactively.
.key ASYNCDONE
Explanation:

		This message informs the user that a job invoked
		with the value of the command qualifier "ASYNCEND"
		set to "NOTIFY" has terminated.

User action:

		None.
.Key SYNTUTHLD
Explanation:

		This message informs the user that dynamic tutor
		has been put on "hold" for an asynchronous job.

		The asynchronous job requested dynamic parameters,
		the user entered REPLY, and then HOLD to postpone
		the selection of parameter values.

		The TCL command GETPAR requests dynamic parameters
		for procedures.

		The TAE library routine XQDYNP requests dynamic
		parameters for processes.
.page
User action:

		When ready to enter parameter values for the
		asynchronous proc, enter REPLY again.
.KEY NOTPROCESS
Explanation:

		ASYNC-PROCESS is only allowed in conjunction with
		process procs.


User action:

		Resubmit the request using a proc which is a 
		process.
.KEY DISHOST
Explanation:

.if VAX_VMS
                The 'EXIT' and 'DCL' commands have been disabled.
.elseif UNIX
                The 'EXIT' and 'USH' commands have been disabled.
.ifend

User action:            

                Continue working or execute the LOGOFF command
                while in a menu.  For further assistance, see
                your System Manager.
.key COMMERR
Explanation:

    		An I/O error occurred while receiving or sending
		a message from a procedure via a path (a mailbox 
		under VMS, a socket under UNIX).  The message 
		usually contains parameters.


User action:

		Find an explanation for the host error code printed
		with the error message. If you cannot correct the 
    		problem, see your System Manager.

.key NOENABLE
Explanation:

		There was a request to receive variables
		from another task at a path which has not 
		been established.


User Action:

		Establish a path with ENABLE-RECVAR TCL command
		before requesting to receive messages.

User action:

		Find an explanation for the host error code printed
		with the error message.  If you cannot correct the 
    		problem, see your System Manager.

.key PATH
Explanation:

                The requested path cannot be established.


User action:

                Find an explanation for the host error code printed
                with the error message.  If you cannot correct the
                problem, see your System Manager.

.key VBLOCK
Explanation:

    		The V-block has insufficient space to accommodate 
		the variables specified in a SENDVAR request.


User action:

    		Divide the list of variables into smaller lists and
		do multiple SENDVAR requests.

.key INCOMPAT
Explanation:

		The definition of a variable received with a RECVAR 
		command is incompatible with the definition for the 
		corresponding variable in the current procedure.


User action:

		Check the declaration of the variable in the receiving
		and sending tasks for matching variable types, 
		and for value vector size and value(s) sent to be within 
		the range defined in the receiving task.

.key MISWINPARM
Explanation:

		Should never be displayed. Associated with an
		unsupported feature contained in tae$tm:action.c
.key DUMMY
Explanation:

		Should never be displayed. Associated with an
		unsupported feature contained in tae$tm:action.c
.key INAPRN
Explanation:

		Should never be displayed. Referenced by code
		in tae$tm:misccmd.c which, due to conditional
		compilation, will never be compiled.
.key ATTN
Explanation:

		Should never be displayed. Referenced by code
		in tae$tm:misccmd.c which, due to conditional
		compilation, will never be compiled.
.key TEST
Explanation:

		Should never be displayed. Referenced by code
		in tae$olb:message.c which, due to conditional
		compilation, will only be compiled when doing
		internal testing.
.KEY FDSAVAIL

		This message tells you the number of file
		descriptors available for your process.  UNIX
		only.
.KEY NOGBL
Explanation:
		TAE is unable to find enough dynamic memory to
		create some of its internal variables.

User Action:
		Report to your System Manager.
.KEY NOPRMNM
Explanation:
		You have not specified which parameter you want 
		help on.

User Action:
		Re-issue the command, specifying the parameter
		name.
.KEY NOMODHELP
Explanation:
		No help is available on this mode of TAE.

User Action:
		See your System Manager.
.KEY NULLLIST
Explanation:
		This is an information message.

		A proc is requesting dynamic parameter input but
		has provided no list of parameters against which
		to request information.

User Action:
		No action is required.  The proc continues execution.
.KEY UNRPSCMD
Explanation:
		The command is not a recognized prompt style command.

User Action:
		Re-issue the command or type HELP.
.KEY NOTMOUNTED
Explanation:
		You are attempting to access a file on a tape which
		is not mounted.

User Action:
		Make sure the tape is properly mounted, then re-issue
		the command.
.KEY SYMBOL
Explanation:
		You have issued a tape drive specific command
		using a symbolic name which is not currently defined.

User Action:
		Re-issue the command using the correct symbolic name
		or device specification.

.if VAX_VMS
		NOTE:  If you are entering a MOUNT command and you
		have not allocated the drive, you must enter both
		the symbolic name and the actual device name.
.ifend
.KEY BADDEV
Explanation:
		You have entered a device specification which is 
		not recognized.

User Action:
		Re-issue the command using the correct device 
		specification.
.KEY NOSYMNAM
Explanation:
		You have entered the MOUNT command without specifying
		a symbolic name for the tape drive.  Since you did
		not allocate the drive, both the symbolic name and
		the actual device name are required.

User Action:
		Re-issue the MOUNT command, specifying both the 
		symbolic name and the actual device name.  An
		alternative is to allocate the drive first.
.KEY NODEVID
Explanation:
		You have issued a tape drive specific command without
		specifying a drive.

User Action:
		Re-issue the command, specifying the drive or request
		HELP on the command.
.KEY INCONS
Explanation:
		You have entered a tape drive specific command using
		a symbolic name which is inconsistent with the
		device name you have entered.

User Action:
		Re-issue the command specifying the names correctly.
		Alternatively, specify only the symbolic name OR
		the actual device name.
.KEY MANYTAPES
Explanation:
		The maximum number of tapes are already allocated
		or mounted.

User Action:
		Wait until other users have dismounted and 
		deallocated their tape drives and try again.
.KEY MOUNTED
Explanation:
		You have attempted to mount or deallocate a tape
		drive which is currently mounted.

User Action:
		If you are attempting to mount, use another tape 
		drive or wait until the current user of this drive 
		has dismounted and deallocated.

		If you are attempting to deallocate, dismount the
		tape first.
.KEY NOMOUNT
Explanation:
		The host returned an error when attempting to mount
		your tape.  The host message given with this key
		should explain the error.

User action:
		Interpret the host message and act accordingly.
.KEY NOTMOUNT
Explanation:
		You have issued a DISMOUNT command for a tape
		drive which is not currently mounted.

User Action:
		Re-issue the command using the correct symbolic
		name or actual device name.
.KEY INUSE
Explanation:
		The symbolic name you have specified is currently
		in use.

User Action:
		Re-issue the command using another symbolic name.
.KEY NOTAVAIL
Explanation:
		VMS was unable to allocate the drive you requested.

User Action:
		Interpret the host message given and act accordingly.
		If the problem is not obvious, consult VMS manuals
		or see your System Manager.
.KEY ALLOC
Explanation:
		You have attempted to allocate or deallocate a tape 
		drive which is already allocated to another user.

User Action:
		Allocate an alternate tape drive or wait for the
		user of the current drive to deallocate it.
.KEY NOTALLOC
Explanation:
		You have attempted to deallocate a tape drive which
		is not currently allocated.

User Action:
		Re-issue the command, correctly specifying the
		symbolic or actual device name of the drive you
		wish to deallocate.
.KEY NOWRTRNG
Explanation:
		This is an informational message only.  A tape was
		mounted with PROTECT=WRITE but no write ring, so
		the tape is being dismounted and the operator is
		forced to put a write ring on the tape or abort
		the job.

User Action:
		None needed.
.KEY HLPRD
Explanation:
		An I/O error was encountered while reading the 
		help file.

User Action:
		This error occurs after a successful open, and therefore
		probably indicates a hardware problem.  See the system 
		manager.
.KEY HLPWRT
Explanation:
		An I/O error occurred while writing the requested
		help file.

User Action:
		This error occurs after a successful open, and therefore
		probably indicates a hardware problem.  See the system 
		manager.
.KEY MAXMEM
Explanation:
		An attempt was made to allocate more than the maximum
		allowed number of memory files.

User Action:
		The maximum number of memory files allowed is controlled
		by a parameter in TAE.  If a larger number is necessary,
		see the system programmer about increasing the ceiling.
.KEY MALFAIL
Explanation:
		The MAL (Memory file ALlocate) operation failed due to a
		host-specific problem.

User action:
		Interpret the host message and act accordingly.

.KEY NOMEMFIL
Explanation:
		An attempt was made to delete a memory file which
		is not currently allocated.

User action:
		None needed.  If the memory file does not exist, it
		does not need to be deleted.  In addition, all memory
		files are automatically deleted upon exiting TAE.
.KEY DALFAIL
Explanation:
		The DAL (Memory file Deallocate) operation failed due to a
		host-specific problem.

User action:
		Interpret the host message and act accordingly.

.KEY MEMEXIST
Explanation:
		The memory file requested has already been allocated.
		Only one memory file may be allocated with a given
		name at a time.  

User action:
		Avoid double allocations.  If a memory file size needs
		to be changed, use the DAL command to delete it, and
		then use MAL to create a new one.
.KEY NOTDEALL
Explanation:
		The host returned an error when attempting to deallocate
		a device.  The host message given with this key should
		explain the error.

User action:
		Interpret the host message and act accordingly.
.KEY MNTABORT
Explanation:
		A mount request was aborted during execution.  The abort
		could have been a control-C typed by the user, an operator
		abort, or a situation with which the MOUNT command does
		not know how to deal.

User action:
		The message given should be self explanatory.  If it is
		unclear, consult the system programmer.
.KEY TNOTPROC
Explanation:

    		When in dynamic command mode, the only way tutor
    		may be entered is by using "TUTOR" alone, with no
    		proc name.  It is not possible to tutor on
    		procs other than the current proc.


User action:

    		Type "TUTOR" to enter tutor mode.                
.KEY STOLEN
Explanation:
		The tape drive was stolen by another user during
		the attempt to initialize the tape.  Due to a
		bug in VMS, the tape must be initialized in the
		subprocess which means the drive must be deallocated
		from the parent and reallocated in the subprocess.
		During this time, there is a small chance that the
		drive can get stolen by another user.  The same
		thing happens after the initialization to transfer
		control back to the parent.
		
User action:
		Please notify the VICAR system programmers if this
		happens.  Either wait for the tape drive to free
		up again, or get a different drive.
.KEY DISMNTFAIL
Explanation:
		The host returned an error when attempting to dismount
		your tape.  The host message given with this key should
		explain the error.

User action:
		Interpret the host message and act accordingly.
.KEY NOWRITE
Explanation:
		You specified the 'INIT keyword on the mount command,
		but did not specify 'WRITE.  The tape must be writeable
		in order to be initialized.

User action:
		Re-issue the command with the 'WRITE keyword.
.KEY ALLOCLOG
Explanation:
		VICAR was unable to define the logical name in the
		job table for the ALLOC command.  This logical name
		is the same as the symbolic name, with a value of
		the actual device name.  The host message given with
		this key should explain the error further.

User action:
		Interpret the host message and act accordingly.
.KEY ASGNFAIL
Explanation:
		The host returned an error when attempting to assign
		a channel to the tape drive in order to do a rewind.
		The host message given with this key should explain
		the error.

User action:
		Interpret the host message and act accordingly.
.KEY REWFAIL
Explanation:
		The host returned an error when attempting to rewind
		your tape.  The host message given with this key should
		explain the error.

User action:
		Interpret the host message and act accordingly.
.KEY CANTALLDEV
Explanation:
		An attempt was made to allocate (USE) a display
		device allocated to another user.

User Action:
		Do a SHOWDISP to determine which devices are
		available to be allocated and USE one of them.
.KEY CANTDALLDEV
Explanation:
		A system error occurred in the device deallocation
		routine while attempting to deallocate the device.

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY CRELNMERR
Explanation:
		A system error occurred in the device allocation
		routine while attempting to create the logical name
		required by the VRDI routines. 

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY DELNMERR
Explanation:
		A system error occurred in the device deallocation
		routine while attempting to delete the logical name
		created by the device allocation routine.

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY DEVALLOC
Explanation:
		An attempt to allocate a display device has
		been made when the device is already allocated.

User Action:
		Use a different device, or wait for it to be freed up.
.KEY DEVNOTAVL
Explanation:
		An attempt has been made to allocate a
		device which is not currently available for use.

User Action:
		Allocate another device.
.KEY DIBERROR
Explanation:
		An unexpected error occurred while reading the
		device information block file.

User Action:
		Have the VRDI cognizant programmer correct the
		error.
.KEY DIBOPEN
Explanation:
		An unexpected error occurred while opening the
		device information block file.

User Action:
		Have the VRDI cognizant programmer correct the
		error.
.KEY GETOPIDERR
Explanation:
		A system error occurred in the device allocation
		routine while attempting to get the process
		identification number of the owner of the display
		device you wish to allocate.

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY GETERMERR
Explanation:
		A system error occurred in the device allocation
		routine while attempting to get the name of the
		terminal you are currently using.

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY GETPIDERR
Explanation:
		A system error occurred in the device allocation
		routine while attempting to get your process
		identification number.

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY GETVAXERR
Explanation:
		A system error occurred in the device allocation
		routine while attempting to get the value of
		the VAX system symbol.

User Action:
		Report the error to the VRDI cognizant programmer.
.KEY NOAVLGDEV
Explanation:
		An attempt was made to allocate (USE) a display
		device using a generic device name and no such device
		is available to be allocated.

User Action:
		Do a SHOWDISP to determine which devices are
		available to be allocated and USE one of them.
.KEY NODEFDEV
Explanation:
		An attempt was made to allocate (USE) a display
		device using the default device option and the
		terminal used does not have a default device assigned
		to it.

User Action:
		Do a SHOWDISP to determine which devices are
		available to be allocated and USE one of them.
.KEY NOOWNGDEV
Explanation:
		An attempt was made to deallocate (FREE) a display
		device using a generic device name and no such device
		had been allocated.

User Action:
		Do a SHOWDISP to determine which device is
		currently allocated to you and FREE it.
.KEY NOSUCHDEV
Explanation:
		An attempt was made to allocate (USE) a display
		device unknown to the system.

User Action:
		Do a SHOWDISP to determine which devices are
		available to be allocated and USE one of them.

