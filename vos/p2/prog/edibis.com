$!****************************************************************************
$!
$! Build proc for MIPL module edibis
$! VPACK Version 1.9, Thursday, January 29, 2015, 19:52:01
$!
$! Execute by entering:		$ @edibis
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
$ write sys$output "*** module edibis ***"
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
$ write sys$output "Invalid argument given to edibis.com file -- ", primary
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
$   if F$SEARCH("edibis.imake") .nes. ""
$   then
$      vimake edibis
$      purge edibis.bld
$   else
$      if F$SEARCH("edibis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake edibis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @edibis.bld "STD"
$   else
$      @edibis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create edibis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack edibis.com -mixed -
	-s edibis.f edibis_windows.c edibis_terminal.c edibis.fin -
	-i edibis.imake edibis.afmake -
	-p edibis.pdf -
	-t tstedibis.pdf tstedibis.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create edibis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	
C==============S T A R T   O F  EDIBIS_MAIN   ( E D I B I S)============
C
C
	SUBROUTINE MAIN44
C
C	IBIS PROGRAM 'EDIBIS'
C
C	Interactive Edit and Display of IBIS interface files (EDIBIS)
C
C	Original Programmer: Frank Evans	January 1987
C
C	Cognizant Programmer: Ray Bambery  
C
C	REVISION:	5		14 August 2011
C
C	REVISION HISTORY
c
c		Rev 6           RJB		08 December 2012
C		--- Fixes edibis_windows.c and dibis_terminal.c
C		    For POSIX compliance with RedHat Linus 6.2 and 
C		    MacOSX 10.7.4
C
c		Rev 5		RJB             14 August 2011
c		--- Fixes for gcc 4.4.4 under Linux
c		    In SUBROUTINE CONVERT_STRING_TO_COMMAND
c       	    had to add new call CALL FIND_ROWCOL
c                   THISROW and THISCOL are found from list directed read
c                   in gcc 4.4.4:
c                   READ (CMDLINE(CMDPOS:),*,ERR=332) THISCOL i
c                   does not detect properly the trailing ")" as a terminator
C                   It gives an error, where the older compilers evidently 
c                   recognized ")" as a proper terminator
c
C		REV 4		NDR		17 November 1994
C		--- Changed code to use direct IBIS2 routines
C		    for forward compatibility. Converted all
C		    FORTRAN I/O to C for portable coding.
C                   Added scripting and command-line features.
C                   and portable "curses" library windowing.
C
C		REV 3		NDR		4 February 1991
C		--- FIXED BUG CAUSING LOCKOUT WHEN "NEXT SCRN"
C		    KEYS HIT ON WYSE TERMINALS (FAILED IN INPUTKEY).
C		    Ref:   FR #66574 by C. Avis
C
C		REV 2		NDR		7 October 1987
C		--- ADDED SEARCH AND QUIT COMMANDS
C
CC
	IMPLICIT NONE
	INTEGER*4	COMMAND, ARGLEN
	CHARACTER*64 ARGSTRING
	INCLUDE 'edibis.fin'

C--------------------------------------------------------------------------
C          EDIBIS EXECUTABLE CODE STARTS HERE
C--------------------------------------------------------------------------

	call xvmessage ("edibis - version Jan 29, 2015 - WLB"," ")
	CALL FILESETUP		! Open files, do transfers, etc.

	CALL INITIALIZE		! Initialize variables and screen

	CALL FILLBUFFER 

	IF (.NOT. BATCH_MODE) THEN
		CALL DISPLAYHEADER
		CALL DISPLAYSCREEN 
		CALL HIGHLIGHT
	ENDIF

	COMMAND = NONE
	DO WHILE ((COMMAND .NE. ENDEDIT).AND.(COMMAND.NE.QUIT))
	    CALL GETCOMMAND (COMMAND, ARGSTRING, ARGLEN)
	    CALL DOCOMMAND (COMMAND, ARGSTRING, ARGLEN)
	ENDDO

	RETURN
	END



C *****************************************************************
C          * * * * * * *  SUBROUTINES * * * * * * *
C *****************************************************************


c*************************************************************************
	SUBROUTINE IBISSCRATCH (OUTUNIT)
C
C	Opens a new temp unit in OUTUNIT
C	with name specified in "SCRATCH" parm

	IMPLICIT INTEGER(A-Z)
	CHARACTER*150 FNAME
C
	CALL XVP('SCRATCH',FNAME,CNT)
	CALL XVUNIT (OUTUNIT,'  ',INST,STATUS,'U_NAME',FNAME,' ')
	RETURN
	END


c*************************************************************************
	SUBROUTINE DOCOMMAND (COMMAND, ARGSTRING, ARGLEN)
C	    Jumps to appropriate routine depending on command number.
	IMPLICIT NONE
	INTEGER*4	COMMAND, ARGLEN
	CHARACTER*(*)  ARGSTRING
	INTEGER*4	CMDNUM
	INCLUDE 'edibis.fin'

	CMDNUM = 0
	IF (ARGLEN .GT. 0) THEN
	    READ (ARGSTRING(1:ARGLEN),*,ERR=199) CMDNUM
	ENDIF
199	CONTINUE

	GOTO (101,102,103,104,105,106,107,108,109,110,111,112,113,
     +	      114,115,116,117,118,119,120,121,122,123,124,125,101)
     +        COMMAND

101	CALL DO_TEXTSTRING (ARGSTRING, ARGLEN,COMMAND)
	RETURN
102	CALL DO_ENDEDIT
	RETURN
103	CALL DO_UPARROW
	RETURN
104	CALL DO_DOWNARROW
	RETURN
105	CALL DO_LEFTARROW
	RETURN
106	CALL DO_RIGHTARROW
	RETURN
107	CALL DO_PAGEUP
	RETURN
108	CALL DO_PAGEDOWN
	RETURN
109	CALL DO_PAGELEFT
	RETURN
110	CALL DO_PAGERIGHT
	RETURN
111	CALL DO_FILETOP
	RETURN
112	CALL DO_FILEBOTTOM
	RETURN
113	CALL DO_GOTOROW (CMDNUM)
	RETURN
114	CALL DO_GOTOCOL (CMDNUM)
	RETURN
115	CALL DO_DELETEROW (CMDNUM)
	RETURN
116	CALL DO_INSERTROW (CMDNUM)
	RETURN
117	CALL DO_ZEROCELL
	RETURN
118	CALL DO_RECALLCELL
	RETURN
119	CALL DO_CHNGFORMAT (ARGSTRING, ARGLEN)
	RETURN
120	CALL DO_REFRESH
	RETURN
121	CALL DO_HELP
	RETURN
122	CALL DO_QUIT
	COMMAND=ENDEDIT
	RETURN
123	CALL DO_SEARCH
	RETURN
124	CALL DO_INITSEARCH
	RETURN
125     RETURN !Bad command-do nothing

	END



C *****************************************************************
C          COMMAND EXECUTION SUBROUTINES 
C *****************************************************************

	SUBROUTINE DO_TEXTSTRING (ARGSTRING, ARGLEN,COMMAND)
	IMPLICIT NONE
	INTEGER*4	ARGLEN,COMMAND
	CHARACTER*(*)  ARGSTRING
	INTEGER*4	PTR, L, INTVALUE,INCOL
	REAL*4	VALUE
	EQUIVALENCE (VALUE,INTVALUE)
	INCLUDE 'edibis.fin'

	IF (ARGLEN.EQ.0) RETURN

	
	IF (COMMAND.NE.SETTEXTVAL) THEN 
        ! check for string-command
 	  IF (ARGSTRING(1:4) .EQ. 'HELP' .OR.  ! If person types help
     +	      ARGSTRING(1:4) .EQ. 'help') THEN ! at a numeric column
	         CALL DO_HELP		       ! then he needs it!
		 RETURN
	  ELSEIF (ARGSTRING(1:4) .EQ. 'QUIT' .OR.
     +	      ARGSTRING(1:4) .EQ. 'quit') THEN
	         CALL DO_QUIT
		 COMMAND=QUIT
		 RETURN
	  ELSEIF (ARGSTRING(1:4) .EQ. 'EXIT' .OR.
     +	      ARGSTRING(1:4) .EQ. 'exit') THEN
	         CALL DO_ENDEDIT
		 COMMAND=ENDEDIT
		 RETURN
	  ENDIF
	ENDIF

	IF (READONLY) THEN
		CALL EMIT_BEEP
		CALL DISPLAY_STATUS('File is Read-only!')
		RETURN
	ENDIF

C         Interpret the string as appropriate input to row/column
	 PTR = CURROW - FIRSTBUFROW + 1
	 INCOL = CURCOL - FIRSTBUFCOL + 1
	 IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
		READ (ARGSTRING,*, ERR=199) IBISBUF(PTR,INCOL)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
		READ (ARGSTRING,*, ERR=199) IBISBUF_FULL(PTR,INCOL)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
		READ (ARGSTRING,*, ERR=199) IBISBUF_DOUB(PTR,INCOL)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
		READ (ARGSTRING,*, ERR=199) IBISBUF_COMP(PTR,INCOL)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
		L = MIN (ARGLEN, 4)
		IBISBUF_A4(PTR,INCOL) = ARGSTRING(1:L)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
		L = MIN (ARGLEN, 16)
		IBISBUF_A16(PTR,INCOL) = ARGSTRING(1:L)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
		L = MIN (ARGLEN, 32)
		IBISBUF_A32(PTR,INCOL) = ARGSTRING(1:L)
	 ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
		L = MIN (ARGLEN, 64)
		IBISBUF_A64(PTR,INCOL) = ARGSTRING(1:L)
	 ENDIF
C				! Change the cell
	 COLCHANGED(CURCOL) = .TRUE.
	 IF (BATCH_MODE) RETURN
	 CALL HIGHLIGHT

199	CONTINUE

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_ENDEDIT
	IMPLICIT NONE
	INTEGER*4	STATUS
	INCLUDE 'edibis.fin'

	IF (.NOT. BATCH_MODE) THEN
		CALL ENDSCREEN
		IF (.NOT.READONLY) 
     +             CALL XVMESSAGE('*** SAVING FILE... ***',' ')
	ENDIF
	
	CALL OUTPUTBUFFER

	IF (READONLY.OR.OUTUNIT.EQ.-1) THEN
		CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS) 
		IF (STATUS.NE.1) THEN
			CALL IBIS_SIGNAL( IBIS,STATUS,1)
		ENDIF
	ELSE
		! Copy data back from working file and delete it
		CALL IBIS_FILE_CLOSE(IBIS,'UKEEP',STATUS)
		CALL IBIS_FILE_UNIT_OPEN(IBIS,STATUS)
		CALL IBISCOPY(IBIS,OUTUNIT,.FALSE.)
		CALL XVCLOSE (UNIT, STATUS,'CLOS_ACT','DELETE',' ')
	ENDIF

	RETURN
	END

c*************************************************************************
	SUBROUTINE DO_QUIT
	IMPLICIT NONE
	INTEGER*4	STATUS
	INCLUDE 'edibis.fin'

	CALL ENDSCREEN
	CALL XVMESSAGE('*** QUITTING EDIBIS ***',' ')
	IF (READONLY.OR.OUTUNIT.EQ.-1) RETURN
	CALL XVCLOSE (UNIT, STATUS,'CLOS_ACT','DELETE',' ')

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_UPARROW
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	CURROW = CURROW - 1
	IF (CURROW .LT. 1) THEN
	    CURROW = 1
	    CALL EMIT_BEEP
	ELSE IF (CURROW .LT. DISPROW) THEN
	    DISPROW = DISPROW - 1
	    CALL CHECKBUFPOS (DISPROW)
	ENDIF

	CALL UPDATEROWDISPLAY
	CALL DISPLAYHEADER
	CALL DISPLAYSCREEN
	CALL HIGHLIGHT

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_DOWNARROW
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	CURROW = CURROW + 1
	IF (CURROW .GT. CLEN) THEN
	    CURROW = CLEN
	    CALL EMIT_BEEP
	ELSE IF (CURROW .GT. DISPROW+NUMDISPROWS-1) THEN
	    DISPROW = DISPROW + 1
	    CALL CHECKBUFPOS (DISPROW)
	ENDIF

	CALL UPDATEROWDISPLAY
	CALL DISPLAYHEADER
	CALL DISPLAYSCREEN
	CALL HIGHLIGHT

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_LEFTARROW
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	CURCOL = CURCOL - 1
	IF (CURCOL .LT. 1) THEN
	    CURCOL = 1
	    CALL EMIT_BEEP
	ELSE IF (CURCOL .LT. DISPCOL) THEN
	    CALL WRITEBUFCOL (DISPCOL+NUMDISPCOLS-1)
	    CALL SETDISPCOL (DISPCOL-1)
	    CALL SETUPCOLDISPLAY 
	    CALL READBUFCOL (DISPCOL)
	    CALL DISPLAYFORMATS
	    CALL DISPLAYSCREEN
	ENDIF

	CALL UPDATECOLDISPLAY
	CALL HIGHLIGHT

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_RIGHTARROW
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	CURCOL = CURCOL + 1
	IF (CURCOL .GT. NCOL) THEN
	    CURCOL = NCOL
	    CALL EMIT_BEEP
	ELSE IF (CURCOL .GT. DISPCOL+NUMDISPCOLS-1) THEN
	    CALL WRITEBUFCOL (DISPCOL)
	    CALL SETDISPCOL (DISPCOL+1)
	    CALL SETUPCOLDISPLAY 
	    CALL READBUFCOL (DISPCOL+NUMDISPCOLS-1)
	    CALL ADJUST_RIGHT
	    CALL DISPLAYFORMATS
	    CALL DISPLAYSCREEN
	ENDIF

	CALL UPDATECOLDISPLAY
	CALL HIGHLIGHT

	RETURN
	END


c*************************************************************************
	SUBROUTINE ADJUST_RIGHT
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	! Sometimes we have to go over more than one
	! column to accomodate the new one:
	DO WHILE (COLXPOS(CURCOL)+COLXLEN(CURCOL).GT.SCREENXSIZE)
	    CALL WRITEBUFCOL (DISPCOL)
	    CALL SETDISPCOL (DISPCOL+1)
	    CALL SETUPCOLDISPLAY 
	    CALL READBUFCOL (DISPCOL+NUMDISPCOLS-1)
	ENDDO

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_PAGEUP
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	IF (DISPROW .GT. 1) THEN
	    DISPROW = DISPROW - NUMDISPROWS
	    DISPROW = MAX (DISPROW, 1)
	    CURROW = DISPROW
	    CALL CHECKBUFPOS (DISPROW)
	    CALL DISPLAYSCREEN
	    CALL UPDATEROWDISPLAY
	    CALL HIGHLIGHT
	ELSE
	    IF (CURROW .EQ. 1) THEN
		CALL EMIT_BEEP
	    ELSE
		CURROW = 1
		CALL UPDATEROWDISPLAY
		CALL HIGHLIGHT
	    ENDIF
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_PAGEDOWN
	IMPLICIT NONE
	INTEGER*4	MAXDISPROW
	INCLUDE 'edibis.fin'

	MAXDISPROW = CLEN - NUMDISPROWS + 1
	IF (DISPROW .LT. MAXDISPROW) THEN
	    DISPROW = DISPROW + NUMDISPROWS
	    DISPROW = MIN (DISPROW, MAXDISPROW)
	    CURROW = DISPROW
	    CALL CHECKBUFPOS (DISPROW)
	    CALL DISPLAYSCREEN
	    CALL UPDATEROWDISPLAY
	    CALL HIGHLIGHT
	ELSE
	    IF (CURROW .EQ. CLEN) THEN
		CALL EMIT_BEEP
	    ELSE
		CURROW = CLEN
		CALL UPDATEROWDISPLAY
		CALL HIGHLIGHT
	    ENDIF
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_PAGELEFT
	IMPLICIT NONE
	INTEGER*4 NEWCOL
	INCLUDE 'edibis.fin'

	IF (DISPCOL .GT. 1) THEN
	    CALL OUTPUTBUFFER
	    NEWCOL = DISPCOL - NUMDISPCOLS
	    NEWCOL = MAX (NEWCOL, 1)
	    CALL SETDISPCOL (NEWCOL)
	    CURCOL = DISPCOL
	    CALL FILLBUFFER
	    CALL SETUPCOLDISPLAY 
	    CALL DISPLAYFORMATS
	    CALL DISPLAYSCREEN
	    CALL UPDATECOLDISPLAY
	    CALL HIGHLIGHT
	ELSE
	    IF (CURCOL .EQ. 1) THEN
		CALL EMIT_BEEP
	    ELSE
		CURCOL = 1
		CALL UPDATECOLDISPLAY
		CALL HIGHLIGHT
	    ENDIF
	ENDIF

	RETURN
	END



c*************************************************************************
	SUBROUTINE DO_PAGERIGHT
	IMPLICIT NONE
	INTEGER*4	MAXDISPCOL
	INTEGER*4 NEWCOL
	INCLUDE 'edibis.fin'

	MAXDISPCOL = NCOL - NUMDISPCOLS + 1
	IF (DISPCOL .LT. MAXDISPCOL) THEN
	    CALL OUTPUTBUFFER
	    NEWCOL = DISPCOL + NUMDISPCOLS
	    NEWCOL = MIN (NEWCOL, NCOL)
	    CALL SETDISPCOL (NEWCOL)
	    CURCOL = DISPCOL
	    CALL FILLBUFFER
	    CALL SETUPCOLDISPLAY 
	    CALL DISPLAYFORMATS
	    CALL DISPLAYSCREEN
	    CALL UPDATECOLDISPLAY
	    CALL HIGHLIGHT
	ELSE
	    IF (CURCOL .EQ. NCOL) THEN
		CALL EMIT_BEEP
	    ELSE
		CURCOL = NCOL
		CALL UPDATECOLDISPLAY
		CALL HIGHLIGHT
	    ENDIF
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_FILETOP
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	IF (DISPROW .GT. 1 .OR. BATCH_MODE) THEN
	    DISPROW = 1
	    CURROW = 1
	    CALL CHECKBUFPOS (DISPROW)
	    IF (BATCH_MODE) RETURN
	    CALL DISPLAYSCREEN
	    CALL UPDATEROWDISPLAY
	    CALL HIGHLIGHT
	ELSE
	    IF (CURROW .EQ. 1) THEN
		CALL EMIT_BEEP
	    ELSE
		CURROW = 1
		CALL UPDATEROWDISPLAY
		CALL HIGHLIGHT
	    ENDIF
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_FILEBOTTOM
	IMPLICIT NONE
	INTEGER*4	MAXDISPROW
	INCLUDE 'edibis.fin'

	MAXDISPROW = CLEN - NUMDISPROWS + 1
	IF (DISPROW .LT. MAXDISPROW .OR. BATCH_MODE) THEN
	    DISPROW = MAXDISPROW
	    CURROW = CLEN
	    CALL CHECKBUFPOS (DISPROW)
	    IF (BATCH_MODE) RETURN
	    CALL DISPLAYSCREEN
	    CALL UPDATEROWDISPLAY
	    CALL HIGHLIGHT
	ELSE
	    IF (CURROW .EQ. CLEN) THEN
		CALL EMIT_BEEP
	    ELSE
		CURROW = CLEN
		CALL UPDATEROWDISPLAY
		CALL HIGHLIGHT
	    ENDIF
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_GOTOROW (CMDNUM)
	IMPLICIT NONE
	INTEGER*4	CMDNUM
	INCLUDE 'edibis.fin'

	IF (CMDNUM .GT. 0) THEN
	    CURROW =  MIN( MAX(CMDNUM,1), CLEN)
	    IF (CURROW .GT. DISPROW + NUMDISPROWS -1
     +           .OR. CURROW.LT.DISPROW) THEN
	       DISPROW = MIN( MAX(CMDNUM,1), CLEN-NUMDISPROWS+1)
	       CALL CHECKBUFPOS (DISPROW)
	       IF (.NOT. BATCH_MODE) CALL DISPLAYSCREEN
	    ENDIF
	    IF (BATCH_MODE) RETURN
	    CALL UPDATEROWDISPLAY
	    CALL HIGHLIGHT
	ENDIF

	RETURN
	END

c*************************************************************************
	SUBROUTINE DO_GOTOCOL (CMDNUM)
	IMPLICIT NONE
	INTEGER*4	CMDNUM
	INTEGER*4 NEWCOL
	INCLUDE 'edibis.fin'

	IF (CMDNUM .GT. 0) THEN
	    CURCOL =  MIN( MAX(CMDNUM,1), NCOL)
	    NEWCOL = MIN( MAX(CMDNUM,1), NCOL-NUMDISPCOLS+1)
	    IF (CURCOL .GT. DISPCOL+NUMDISPCOLS-1 .OR.
     +            (CURCOL .LT. DISPCOL) ) THEN
	    	CALL OUTPUTBUFFER
	    	CALL SETDISPCOL (NEWCOL)
		IF (.NOT. BATCH_MODE) CALL SETUPCOLDISPLAY 
		CALL READBUFCOL (CURCOL)
		IF (.NOT. BATCH_MODE) THEN
			CALL ADJUST_RIGHT
			CALL DISPLAYHEADER
			CALL DISPLAYSCREEN
		ENDIF
	    ENDIF
	    IF (BATCH_MODE) RETURN
	    CALL UPDATECOLDISPLAY
	    CALL HIGHLIGHT
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_DELETEROW (CMDNUM)
C	    Deletes rows
	IMPLICIT NONE
	INTEGER*4	CMDNUM,STATUS
	INTEGER*4	NUMDELETE
	INCLUDE 'edibis.fin'

	IF (READONLY) THEN
		CALL EMIT_BEEP
		CALL DISPLAY_STATUS('File is Read-only!')
		RETURN
	ENDIF

	CALL OUTPUTBUFFER
	
	NUMDELETE = 1
	IF (CMDNUM .GT. 0)  NUMDELETE = MIN (MAX(CMDNUM,1), CLEN-CURROW+1)
	
	CALL IBIS_ROW_DELETE(IBIS,CURROW,NUMDELETE,STATUS)
	
	CLEN = CLEN - NUMDELETE
	NUMDISPROWS = MIN (MAXROWS, CLEN)
	CURROW = MIN (CURROW, CLEN)
	DISPROW = MIN (DISPROW, CLEN-NUMDISPROWS+1)
	DISPROW = MAX (DISPROW, 1)
	FIRSTBUFROW = 128*((DISPROW-64)/128) + 1
	FIRSTBUFROW = MIN (FIRSTBUFROW, 128*((CLEN-129)/128)+1)
	FIRSTBUFROW = MAX (FIRSTBUFROW, 1)
	!CALL CLEAR_DATA
	CALL FILLBUFFER
	IF (BATCH_MODE) RETURN
	CALL DISPLAYHEADER
	CALL DISPLAYSCREEN
	CALL HIGHLIGHT
	
	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_INSERTROW (CMDNUM)
C	    Inserts rows
	IMPLICIT NONE
	INTEGER*4	CMDNUM,STATUS
	INTEGER*4	NUMINSERT
	INCLUDE 'edibis.fin'

	IF (READONLY) THEN
		CALL EMIT_BEEP
		CALL DISPLAY_STATUS('File is Read-only!')
		RETURN
	ENDIF

	CALL OUTPUTBUFFER
	NUMINSERT = 1
	IF (CMDNUM .GT. 0)  NUMINSERT = MAX (CMDNUM, 1)

	CALL IBIS_ROW_NEW(IBIS,CURROW,NUMINSERT,STATUS)

	CLEN = CLEN + NUMINSERT
	NUMDISPROWS = MIN (MAXROWS, CLEN)
	FIRSTBUFROW = 128*((DISPROW-64)/128) + 1
	FIRSTBUFROW = MIN (FIRSTBUFROW, 128*((CLEN-129)/128)+1)
	FIRSTBUFROW = MAX (FIRSTBUFROW, 1)
	CALL FILLBUFFER
	IF (BATCH_MODE) RETURN
	CALL DISPLAYHEADER
	CALL DISPLAYSCREEN
	CALL HIGHLIGHT

	RETURN
	END



c*************************************************************************
	SUBROUTINE DO_ZEROCELL
	IMPLICIT NONE
	INTEGER*4	PTR,INCOL
	INCLUDE 'edibis.fin'

	IF (READONLY) THEN
		CALL EMIT_BEEP
		CALL DISPLAY_STATUS('File is Read-only!')
		RETURN
	ENDIF
	
	PTR = CURROW - FIRSTBUFROW + 1
	INCOL = CURCOL - FIRSTBUFCOL + 1
	IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
	    SAVECELLVALUE = IBISBUF(PTR,INCOL) 
	    IBISBUF(PTR,INCOL) = 0.0
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
	    SAVE_FULL = IBISBUF_FULL(PTR,INCOL) 
	    IBISBUF_FULL(PTR,INCOL) = 0
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
	    SAVE_DOUB = IBISBUF_DOUB(PTR,INCOL) 
	    IBISBUF_DOUB(PTR,INCOL) = 0.0
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
	    SAVE_COMP = IBISBUF_COMP(PTR,INCOL) 
	    IBISBUF_COMP(PTR,INCOL) = (0.0,0.0)
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
	    SAVE_A4 = IBISBUF_A4(PTR,INCOL) 
	    IBISBUF_A4(PTR,INCOL) = '    '
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
	    SAVE_A16 = IBISBUF_A16(PTR,INCOL) 
	    IBISBUF_A16(PTR,INCOL) = '                '
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
	    SAVE_A32 = IBISBUF_A32(PTR,INCOL) 
	    IBISBUF_A32(PTR,INCOL) = '                                '
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
	    SAVE_A64 = IBISBUF_A64(PTR,INCOL) 
	    IBISBUF_A64(PTR,INCOL) =
     +'                                                                '
	ENDIF
	COLCHANGED(CURCOL) = .TRUE.
	IF (BATCH_MODE) RETURN
	CALL HIGHLIGHT

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_RECALLCELL
	IMPLICIT NONE
	INTEGER*4	PTR,INCOL
	INCLUDE 'edibis.fin'

	IF (READONLY) THEN
		CALL EMIT_BEEP
		CALL DISPLAY_STATUS('File is Read-only!')
		RETURN
	ENDIF

	PTR = CURROW - FIRSTBUFROW + 1
	INCOL = CURCOL - FIRSTBUFCOL + 1
	IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
	    IBISBUF(PTR,INCOL) = SAVECELLVALUE
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
	    IBISBUF_FULL(PTR,INCOL) = SAVE_FULL
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
	    IBISBUF_DOUB(PTR,INCOL) = SAVE_DOUB
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
	    IBISBUF_COMP(PTR,INCOL) = SAVE_COMP
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
	    IBISBUF_A4(PTR,INCOL) = SAVE_A4
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
	    IBISBUF_A16(PTR,INCOL) = SAVE_A16
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
	    IBISBUF_A32(PTR,INCOL) = SAVE_A32
	ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
	    IBISBUF_A64(PTR,INCOL) = SAVE_A64
	ENDIF
	COLCHANGED(CURCOL) = .TRUE.
	IF (BATCH_MODE) RETURN
	CALL HIGHLIGHT

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_CHNGFORMAT (ARGSTRING, ARGLEN)
	IMPLICIT NONE
	INTEGER*4	ARGLEN, ASC, FORMCODE, I, SLEN,STATUS
	CHARACTER*(*)  ARGSTRING
	CHARACTER*64 STRING
	CHARACTER*32 TMPFORMAT
	CHARACTER*1  CHR
	INCLUDE 'edibis.fin'


	! This will only work for IBIS-1, which has no
	! internal formatting.
	
	IF (FILEVERSION(6:6).NE.'2' .AND. ARGLEN .GT. 0) THEN
	    FORMCODE = 0
	    TMPFORMAT = ARGSTRING
	    DO I = 1,32		! Find out the data type real, int, or char
		ASC = ICHAR(TMPFORMAT(I:I))
		IF (ASC .GE. 96)  ASC = ASC - 32
		CHR = CHAR(ASC)
		IF (CHR .EQ. 'F' .OR. CHR .EQ. 'E')  FORMCODE = FMT_REAL
		IF (CHR .EQ. 'I')  FORMCODE = FMT_FULL
		IF (CHR .EQ. 'A')  FORMCODE = FMT_A4
	    ENDDO
	    IF (FORMCODE .EQ. FMT_REAL) THEN	! See if legal format and find size
		CALL IBIS_COLUMN_SET( IBIS,'FORMAT','REAL',CURCOL,STATUS)
		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)
		CALL IBIS_COLUMN_SET( IBIS,'U_FORMAT','REAL',CURCOL,STATUS)
		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)
		WRITE (STRING, TMPFORMAT ,ERR=199) 7.0
	    ELSE IF (FORMCODE .EQ. FMT_FULL) THEN
		CALL IBIS_COLUMN_SET( IBIS,'FORMAT','FULL',CURCOL,STATUS)
		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)
		CALL IBIS_COLUMN_SET( IBIS,'U_FORMAT','FULL',CURCOL,STATUS)
		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)
		WRITE (STRING, TMPFORMAT ,ERR=199) 7
	    ELSE IF (FORMCODE .EQ. FMT_A4) THEN
		CALL IBIS_COLUMN_SET( IBIS,'FORMAT','A3',CURCOL,STATUS)
		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)
		CALL IBIS_COLUMN_SET( IBIS,'U_FORMAT','A4',CURCOL,STATUS)
		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)
		WRITE (STRING, TMPFORMAT ,ERR=199) 'XXXX'
	    ELSE
		GOTO 199
	    ENDIF
	    COLXLEN(CURCOL) = SLEN(STRING)	! Change the format
	    COLFORMAT(CURCOL) = TMPFORMAT 
	    COLFORMCODE(CURCOL) = FORMCODE
            CALL READBUFCOL (CURCOL) 		!Refresh buffer
	    IF (BATCH_MODE) RETURN
	    CALL SETUPCOLDISPLAY
	    CALL DISPLAYFORMATS			! Redisplay everything
	    CALL DISPLAYSCREEN
	    CALL HIGHLIGHT
	ENDIF
199	CONTINUE

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_REFRESH
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	CALL NEWSCREEN
	CALL DISPLAYHEADER
	CALL DISPLAYSCREEN
	CALL HIGHLIGHT

	RETURN
	END

c*************************************************************************
	SUBROUTINE DO_HELP
	IMPLICIT NONE
	INTEGER*4	KEY

	CALL SHOW_HELP
	CALL KEYINPUT (KEY)
	IF (KEY.EQ.13) THEN
		CALL SHOW_NOKEYPAD_HELP
		CALL KEYINPUT (KEY)
	ENDIF
	CALL DO_REFRESH

	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_INITSEARCH
	IMPLICIT NONE
	INTEGER*4 CDUM,STRLEN,STAT
	CHARACTER*80 RANGESTRING
	INCLUDE 'edibis.fin'

	INITSET = .TRUE.
	CALL DISPLAY_PROMPT('ENTER RANGE] ')

	CALL OUTPUTBUFFER   !YOU WANT TO SEARCH ON CURRENT WORK FILE.
	CALL GET_COMMAND_FROM_KEYBOARD(CDUM,RANGESTRING,STRLEN)
	CALL SET_SEARCH_STRING(RANGESTRING,STRLEN,STAT)
	IF (STAT.EQ.0) GOTO 199
	
	CALL DO_SEARCH
	RETURN

199	CONTINUE
     	CALL EMIT_BEEP
	CALL DISPLAY_STATUS('SYNTAX: VAL or MIN:MAX')
	RETURN
	END

c*************************************************************************
	SUBROUTINE SET_SEARCH_STRING(RANGESTRING,STRLEN,STAT)
	IMPLICIT NONE
	INTEGER*4 STRLEN,I,STAT
	INTEGER*4 LEN1, LEN2
	CHARACTER*80 RANGESTRING
	INCLUDE 'edibis.fin'

	STAT=1
	INITSET = .TRUE.

	I=0
	DO WHILE ((RANGESTRING(I:I).NE.':').AND.(I.LE.STRLEN))
		I = I + 1
	ENDDO
	LEN1 = I-1
	LEN2 = STRLEN - I

	IF ((I.EQ.1).AND.(STRLEN.GT.0)) THEN  ! :MAX OPTION
	
	    IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
		MINVAL = -1.6E38
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
		MINVAL_FULL = -2147483647
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
	    	! how can we do this portably??
		MINVAL_DOUB = -1.6D38
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
		MINVAL_COMP = (-1.6E38,0)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
		MINVAL_A4 = '    '
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
		MINVAL_A16 = '                '
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
		MINVAL_A32 = '                                '
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
		MINVAL_A64 = 
     +'                                                                '
	    ENDIF	

	ELSE !GET MINVAL
	    IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
		READ (RANGESTRING(1:LEN1),*, ERR=199) MINVAL
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
		READ (RANGESTRING(1:LEN1),*, ERR=199) MINVAL_FULL
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
		READ (RANGESTRING(1:LEN1),*, ERR=199) MINVAL_DOUB
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
		READ (RANGESTRING(1:LEN1),*, ERR=199) MINVAL_COMP
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
		MINVAL_A4 = RANGESTRING(1:LEN1)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
		MINVAL_A16 = RANGESTRING(1:LEN1)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
		MINVAL_A32 = RANGESTRING(1:LEN1)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
		MINVAL_A64 = RANGESTRING(1:LEN1)
	    ENDIF
	ENDIF

	IF ((I.EQ.STRLEN).AND.(STRLEN.GT.0)) THEN  ! MIN: OPTION
	    IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
		MAXVAL = 1.6E38
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
		MAXVAL_FULL = 2147483647
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
		MAXVAL_DOUB = 1.6D38
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
		MAXVAL_COMP = (1.6E38,0)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
		MAXVAL_A4 = '~~~~'
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
		MAXVAL_A16 = '~~~~~~~~~~~~~~~~'
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
		MAXVAL_A32 = '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
		MAXVAL_A64 =
     +'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
	    ENDIF	

	ELSE   ! GET MAXVAL
            IF (LEN2.LT.1) THEN
                MAXVAL_A64 = MINVAL_A64
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
		READ (RANGESTRING(I+1:),*, ERR=199) MAXVAL
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
		READ (RANGESTRING(I+1:),*, ERR=199) MAXVAL_FULL
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
		READ (RANGESTRING(I+1:),*, ERR=199) MAXVAL_DOUB
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
		READ (RANGESTRING(I+1:),*, ERR=199) MAXVAL_COMP
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
		MAXVAL_A4 = RANGESTRING(I+1:)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
		MAXVAL_A16 = RANGESTRING(I+1:)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
		MAXVAL_A32 = RANGESTRING(I+1:)
	    ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
		MAXVAL_A64 = RANGESTRING(I+1:)
	    ENDIF

	ENDIF

	RETURN

199	STAT=0
	RETURN
	END


c*************************************************************************
	SUBROUTINE DO_SEARCH
	IMPLICIT NONE
	LOGICAL*4 NOTFOUND
	INTEGER*4 ROWSEARCHVAL,HALFDISP
	INCLUDE 'edibis.fin'

	CALL OUTPUTBUFFER   !YOU WANT TO SEARCH ON CURRENT WORK FILE.
	IF (INITSET) THEN
	   CURROW = ROWSEARCHVAL(NOTFOUND)
	   IF (BATCH_MODE) THEN
	   	IF (NOTFOUND) THEN
			CALL XVMESSAGE('??E -STRING NOT FOUND',' ')
			CALL ABEND
		ELSE
			CALL DO_GOTOROW(CURROW)
		ENDIF
		RETURN
	   ENDIF
	   IF (NOTFOUND) THEN
	     	CALL EMIT_BEEP
		CALL DISPLAY_STATUS('NOT FOUND')
	   ELSE  ! DISPLAY IT
		IF ((CURROW.GT.DISPROW+NUMDISPROWS-1).OR.
     +		(CURROW.LT.DISPROW)) THEN
		  HALFDISP = NUMDISPROWS/2
		  DISPROW = MAX(1, CURROW-HALFDISP)
		  DISPROW = MIN(DISPROW, CLEN-NUMDISPROWS+1)
		  CALL CHECKBUFPOS (DISPROW)
		  CALL DISPLAYSCREEN
		ENDIF
	        CALL UPDATEROWDISPLAY
	   	CALL HIGHLIGHT
	   ENDIF
	ELSE
	   CALL DO_INITSEARCH
	ENDIF

	RETURN
	END

c*************************************************************************
	FUNCTION ROWSEARCHVAL(EOFL)
	IMPLICIT NONE
	INTEGER*4 BUFSIZE, ROWSEARCHVAL, INCREMENT, ROW
	INTEGER*4 POS,STATUS
	INTEGER*4 TOPROW,MAXBLKS
	INTEGER*4 ROWSNOW,BUFINC
	PARAMETER (MAXBLKS = 1)
	PARAMETER (BUFSIZE = 128*MAXBLKS)  !WINDOW FOR SEARCH
	LOGICAL*4 EOFL,OUTOFBOUNDS,SAMEBUF
	CHARACTER*64 COLBUF_A64(BUFSIZE)
	CHARACTER*32 COLBUF_A32(BUFSIZE)
	CHARACTER*16 COLBUF_A16(BUFSIZE)
	CHARACTER*4 COLBUF_A4(BUFSIZE)
	COMPLEX*8 COLBUF_COMP(BUFSIZE)
	REAL*8 COLBUF_DOUB(BUFSIZE)
	INTEGER*4 COLBUF_FULL(BUFSIZE)
	REAL*4 COLBUF(BUFSIZE)
	EQUIVALENCE(COLBUF,COLBUF_FULL,COLBUF_COMP,COLBUF_DOUB)
	EQUIVALENCE(COLBUF,COLBUF_A4,COLBUF_A16,
     +              COLBUF_A32,COLBUF_A64)
	INCLUDE 'edibis.fin'

C---- Set up the buffer position
	IF (FORWARDMODE) THEN
	   INCREMENT = 1
	   ROW = CURROW + INCREMENT
	   TOPROW = ROW
	   BUFINC = BUFSIZE
	ELSE 
	   INCREMENT = -1
	   ROW = CURROW + INCREMENT
	   TOPROW = ROW + 1 - BUFSIZE
	   BUFINC = -BUFSIZE
	ENDIF

	OUTOFBOUNDS = .TRUE.
	EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
	
C---- Begin the search loop

	DO WHILE (OUTOFBOUNDS.AND.(.NOT.EOFL))
	
	   SAMEBUF = .TRUE.
	   ROWSNOW = BUFSIZE
	   IF (TOPROW .LT. 1) TOPROW=1
	   IF (TOPROW .GT. CLEN) TOPROW=CLEN
	   POS = ROW+1-TOPROW
	   IF (TOPROW + BUFSIZE-1.GT. CLEN) 
     +              ROWSNOW=CLEN+1-TOPROW

	   IF (COLFORMCODE(CURCOL) .LT. FMT_A4) THEN
	      CALL IBIS_COLUMN_READ(IBIS,COLBUF,
     +                      CURCOL,TOPROW,ROWSNOW,STATUS)
     	      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	   ENDIF
	   
	   IF (COLFORMCODE(CURCOL) .EQ. FMT_REAL) THEN
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF(POS).GT.MAXVAL)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.(COLBUF(POS).LT.MINVAL)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
	      
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_FULL) THEN
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_FULL(POS).GT.MAXVAL_FULL)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.
     +			(COLBUF_FULL(POS).LT.MINVAL_FULL)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
	      
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_DOUB) THEN
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_DOUB(POS).GT.MAXVAL_DOUB)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.
     +			(COLBUF_DOUB(POS).LT.MINVAL_DOUB)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
	      
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_COMP) THEN
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_COMP(POS).NE.MINVAL_COMP)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
	      
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A4) THEN
	      CALL IBIS_COLUMN_READ(IBIS,COLBUF_A4,
     +                      CURCOL,TOPROW,ROWSNOW,STATUS)
     	      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_A4(POS).GT.MAXVAL_A4)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.
     +			(COLBUF_A4(POS).LT.MINVAL_A4)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
		    
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A16) THEN
	      CALL IBIS_COLUMN_READ(IBIS,COLBUF_A16,
     +                      CURCOL,TOPROW,ROWSNOW,STATUS)
     	      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_A16(POS).GT.MAXVAL_A16)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.
     +			(COLBUF_A16(POS).LT.MINVAL_A16)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
		    
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A32) THEN
	      CALL IBIS_COLUMN_READ(IBIS,COLBUF_A32,
     +                      CURCOL,TOPROW,ROWSNOW,STATUS)
     	      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_A32(POS).GT.MAXVAL_A32)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.
     +			(COLBUF_A32(POS).LT.MINVAL_A32)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
		    
	   ELSE IF (COLFORMCODE(CURCOL) .EQ. FMT_A64) THEN
	      CALL IBIS_COLUMN_READ(IBIS,COLBUF_A64,
     +                      CURCOL,TOPROW,ROWSNOW,STATUS)
     	      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	      DO WHILE (OUTOFBOUNDS.AND.SAMEBUF.AND.(.NOT.EOFL))
		    OUTOFBOUNDS = (COLBUF_A64(POS).GT.MAXVAL_A64)
		    OUTOFBOUNDS = OUTOFBOUNDS .OR.
     +			(COLBUF_A64(POS).LT.MINVAL_A64)
		    ROW = ROW + INCREMENT
		    POS = POS + INCREMENT
	   	    EOFL = ((ROW.GT.CLEN).OR.(ROW.LT.1))
		    SAMEBUF = (POS.LE.ROWSNOW).AND.(POS.GE.1)
	      ENDDO
	    ENDIF
	    TOPROW = TOPROW + BUFINC
	ENDDO

	IF (OUTOFBOUNDS) THEN       !YOU HIT THE END OF FILE
		ROWSEARCHVAL = CURROW
	ELSE			    !YOU FOUND SOMETHING !
		ROWSEARCHVAL = ROW - INCREMENT
		EOFL = .FALSE.
	ENDIF
	    
	RETURN
	END



C ***********************************************************
C          INITIALIZATION SUBROUTINES 
C *****************************************************************
	SUBROUTINE GET_INFO_FROM_PARMS
C	 Gets File parameters from PARMS
	IMPLICIT NONE
	INCLUDE 'edibis.fin'
	INTEGER*4	 COUNT

	CALL XVP ('NCOL',    NCOL,        COUNT)
	CALL XVP ('LCOL',    CLEN,        COUNT)
	CALL XVP ('ORG',     FILEORG,     COUNT)
	CALL XVP ('DEFFMT',  DEFLTFMT,    COUNT)
	CALL XVP ('VERSION', FILEVERSION, COUNT)
	CALL XVP ('CFORMAT', COLFILFMT,   COUNT)
	CALL XVP ('HOST',  FHOST,   COUNT)

	RETURN
	END

c*************************************************************************
	SUBROUTINE GET_INFO_FROM_FILE( IBISIN,USE_COL)
C	 Gets File parameters from IBIS file
	IMPLICIT NONE
	INTEGER*4	 STATUS,COUNT,IBISIN,INNCOL
	INTEGER*4	 DCOL, COL
	INTEGER*4  NCOLDEF, COLSDEF,INCLEN
	INTEGER*4  IBIS_FILE_GET
	LOGICAL*4  USE_COL
	INCLUDE 'edibis.fin'

	!Get File attributes
	COUNT=IBIS_FILE_GET(IBISIN, 'NC',NCOL,1,1)
	COUNT=IBIS_FILE_GET(IBISIN, 'NR',CLEN,1,1)
	COUNT=IBIS_FILE_GET(IBISIN, 'ORG',FILEORG,1,1)
	COUNT=IBIS_FILE_GET(IBISIN, 'HOST',FHOST,1,1)
	COUNT=IBIS_FILE_GET(IBISIN, 'FMT_DEFAULT',DEFLTFMT,1,1)
	COUNT=IBIS_FILE_GET(IBISIN, 'VERSION',FILEVERSION,1,1)

	IF (USE_COL) THEN  ! Check Column params
		INNCOL = NCOL
		INCLEN = CLEN
		CALL XVPARM ('NCOL', NCOL, COUNT, NCOLDEF,0)
		CALL XVPARM ('COLS', COLS, DCOL, COLSDEF,0)
		IF (NCOLDEF .EQ. 1 .AND. COLSDEF .EQ. 1)  NCOL = INNCOL
		IF (NCOLDEF .EQ. 1 .AND. COLSDEF .EQ. 0)  NCOL = DCOL
		CALL XVP ('LCOL', CLEN, COUNT)
		CLEN = MAX (CLEN, INCLEN)
	ENDIF

	DO COL=1,NCOL
		IF (.NOT.USE_COL.OR.COLSDEF.EQ.1) COLS(COL) = COL
		CALL IBIS_COLUMN_GET(IBISIN, 'FORMAT',COLFILFMT(COL),
     +					    COLS(COL), STATUS)
	ENDDO

	RETURN
	END
c*************************************************************************
	SUBROUTINE SETUP_COLUMN_TRANS(IBIS)
C	 Sets up the column translation stuff
	IMPLICIT NONE
	INTEGER*4 COUNT,NC,COL,IBIS,IBIS_FILE_GET
	INTEGER*4 STATUS
	LOGICAL*4 IBIS2
	CHARACTER*10 VERS,FMT
	CHARACTER*1 CHR
	INTEGER*4 ASIZE

	COUNT = IBIS_FILE_GET( IBIS, 'VERSION', VERS, 1,1)
	COUNT = IBIS_FILE_GET( IBIS, 'NC', NC, 1,1)
	
	IBIS2 = (VERS(6:6).EQ.'2')
	
	IF (IBIS2) THEN
		DO COL=1,NC

			 ! set up the translation; default if other.
	
			CALL IBIS_COLUMN_GET( IBIS, 'FORMAT', FMT,
     +					COL,STATUS)
     			CHR = FMT(1:1)
			IF (CHR .EQ. 'A') THEN
			   READ(FMT(2:),*) ASIZE
			   IF (ASIZE.LE.4) THEN
			   	CALL IBIS_COLUMN_SET( IBIS,
     +					'U_FORMAT','A4',COL,STATUS)
			   ELSE IF (ASIZE.LE.16) THEN
			   	CALL IBIS_COLUMN_SET( IBIS,
     +					'U_FORMAT','A16',COL,STATUS)
			   ELSE IF (ASIZE.LE.32) THEN
			   	CALL IBIS_COLUMN_SET( IBIS,
     +					'U_FORMAT','A32',COL,STATUS)
			   ELSE
			   	CALL IBIS_COLUMN_SET( IBIS,
     +					'U_FORMAT','A64',COL,STATUS)
			   ENDIF
			ELSE IF (CHR.EQ.'B'.OR.
     +			   CHR.EQ.'H'.OR.CHR.EQ.'F') THEN
			   CALL IBIS_COLUMN_SET( IBIS,
     +					'U_FORMAT','FULL',COL,STATUS)
    			ENDIF

		ENDDO
	ENDIF
	
	RETURN
	END
	
c*************************************************************************
	SUBROUTINE CREATE_IBIS_FILE(NEWUNIT,NEWIBIS)
C	 Creates new IBIS file from info
	IMPLICIT NONE
	INCLUDE 'edibis.fin'
	INTEGER*4	 NEWUNIT,NEWIBIS,STATUS

	CALL IBIS_FILE_UNIT(NEWUNIT,NEWIBIS,'WRITE',
     +                   NCOL, CLEN,COLFILFMT,FILEORG, STATUS)
	IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(NEWUNIT,STATUS,1)
	CALL IBIS_FILE_SET( NEWIBIS, 'VERSION', FILEVERSION, STATUS )
	IF (STATUS .NE. 1) CALL IBIS_SIGNAL(NEWIBIS,STATUS,1)
	CALL IBIS_FILE_SET( NEWIBIS, 'FMT_DEFAULT', DEFLTFMT,STATUS )
	IF (STATUS .NE. 1) CALL IBIS_SIGNAL(NEWIBIS,STATUS,1)
	CALL IBIS_FILE_SET( NEWIBIS, 'AUTO_INIT', 'OFF',STATUS )
	IF (STATUS .NE. 1) CALL IBIS_SIGNAL(NEWIBIS,STATUS,1)
	CALL IBIS_FILE_UNIT_OPEN(NEWIBIS,STATUS)
	IF (STATUS .NE. 1) CALL IBIS_SIGNAL(NEWIBIS,STATUS,1)

	RETURN
	END


c*************************************************************************
	SUBROUTINE OPEN_IBIS_FILE(UNIT,IBIS,GRDIM,MODE)
C	 Opens existing IBIS file for update
	IMPLICIT NONE
	CHARACTER*(*) MODE
	INTEGER*4	 UNIT,IBIS,STATUS,GRDIM

	CALL IBIS_FILE_OPEN(UNIT,IBIS,MODE, GRDIM,
     +					0,' ',' ', STATUS)
	IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

	RETURN
	END

c*************************************************************************
	SUBROUTINE COPY_IBIS_DATA( IBISIN,IBISOUT,USE_COL)
C	 Copies IBISIN data to UNITOUT, then closes UNITOUT
C	 We will use a smart copy, taking organization into account.
	IMPLICIT NONE
	INCLUDE 'edibis.fin'
	INTEGER*4 COL,INNC,INNR,THISROW,IBISIN,COUNT
	INTEGER*4 CBUFSIZE,BYTEBUFSIZE,ROW,SIZE,ROWSNOW,ROWINC,NINC
	INTEGER*4	INCOLS(MAXFILECOLS),OUTCOLS(MAXFILECOLS)
	INTEGER*4 IBIS_FILE_GET,STATUS,IBISOUT,INRECORD,OUTRECORD
	LOGICAL*4 USE_COL
	PARAMETER (CBUFSIZE=1024,BYTEBUFSIZE=CBUFSIZE*8)
	CHARACTER*10 FORG
	REAL*8 DBUFFER(CBUFSIZE)

	COUNT = IBIS_FILE_GET(IBISOUT, 'NC', INNC,1,1)
	COUNT = IBIS_FILE_GET(IBISOUT, 'NR', INNR,1,1)
	COUNT = IBIS_FILE_GET(IBISOUT, 'ORG', FORG,1,1)
	IF (USE_COL) THEN
		DO COL=1,NCOL
			INCOLS(COL)=COLS(COL)
			OUTCOLS(COL)=COL
		ENDDO
	ELSE
		DO COL=1,INNC
			INCOLS(COL)=COL
			OUTCOLS(COL)=COL
		ENDDO
	ENDIF
	
	IF (FORG(1:1).EQ.'R'.OR.FORG(1:1).EQ.'r') THEN
C            Row-Oriented copy; Use Unformatted Records, 
C            as we know both files have the same host.
		CALL IBIS_RECORD_OPEN(IBISIN,INRECORD,' ',
     +              INCOLS,INNC,'NONE',STATUS)
		CALL IBIS_RECORD_OPEN(IBISOUT,OUTRECORD,' ',
     +              OUTCOLS,INNC,'NONE',STATUS)
     		DO ROW=1,INNR
		   CALL IBIS_RECORD_READ(INRECORD,DBUFFER,ROW,STATUS)
		   IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
		   CALL IBIS_RECORD_WRITE(OUTRECORD,DBUFFER,ROW,STATUS)
		   IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
		ENDDO
	ELSE
C            Column-Oriented copy. We use unformatted columns
C            as we know both files have the same host.
		DO COL=1,INNC
		   CALL IBIS_COLUMN_SET(IBISIN,'U_FORMAT',
     +                                 'NONE',INCOLS(COL),STATUS)
		   CALL IBIS_COLUMN_SET(IBISOUT,'U_FORMAT',
     +                                 'NONE',OUTCOLS(COL),STATUS)
		   CALL IBIS_COLUMN_GET(IBISIN,'U_SIZE',
     +				       SIZE,INCOLS(COL),STATUS)
		   ROWINC = BYTEBUFSIZE/SIZE
		   IF (ROWINC.GT.INNR) ROWINC=INNR
		   NINC = (INNR+ROWINC-1)/ROWINC
		   THISROW=1
		   ROWSNOW=ROWINC
		   DO ROW=1,NINC
		   	IF (ROW.EQ.NINC) ROWSNOW = INNR+1-THISROW
			
		   	CALL IBIS_COLUMN_READ(IBISIN,DBUFFER,
     +                          INCOLS(COL),THISROW,ROWSNOW,STATUS)
		   	IF (STATUS.NE.1) 
     +				CALL IBIS_SIGNAL(IBISIN,STATUS,1)
     
		   	CALL IBIS_COLUMN_WRITE(IBISOUT,DBUFFER,
     +                             OUTCOLS(COL),THISROW,ROWSNOW,STATUS)     
		   	IF (STATUS.NE.1) 
     +				CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
     
     			THISROW = THISROW + ROWINC
		   ENDDO
		ENDDO
	ENDIF

	! Copy the groups over as well
	CALL IBIS_GROUP_TRANSFER(IBISIN,IBISOUT,'ANY',
     +    INCOLS,OUTCOLS,INNC,STATUS)

	RETURN
	END

c*************************************************************************
	SUBROUTINE IBISCOPY( IBISIN,UNITOUT,USE_COL)
C	 Copies IBISIN to UNITOUT, then closes UNITOUT
	IMPLICIT NONE
	LOGICAL*4 USE_COL
	INTEGER*4 STATUS,IBISIN,IBISOUT,UNITOUT
	INCLUDE 'edibis.fin'

	CALL GET_INFO_FROM_FILE(IBISIN, USE_COL)
	CALL CREATE_IBIS_FILE( UNITOUT, IBISOUT )
	CALL COPY_IBIS_DATA( IBISIN, IBISOUT,USE_COL )
	CALL IBIS_FILE_CLOSE( IBISOUT, ' ', STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)

	RETURN
	END


c*************************************************************************
	SUBROUTINE FILESETUP
C	    Opens the interface file (if specified) and open the 
C	 output interface file (if specified) to edit.
	IMPLICIT NONE
	INCLUDE 'edibis.fin'
	INTEGER*4	STATUS, COUNT, INPDEF, OUTDEF, UNIT1
	INTEGER*4 IBIS1
	CHARACTER*200  INPNAME, OUTNAME
	INTEGER*4 GRDIM
	LOGICAL*4 XVPTST

	READONLY = XVPTST('READONLY')

C			See if there is an input file
	CALL XVPARM ('INP', INPNAME, COUNT, INPDEF,0)
	CALL XVP ('GR1DIM',    GRDIM,    COUNT)
	IF (INPDEF .EQ. 0) THEN
	    CALL XVUNIT(UNIT1,'INP',1,STATUS,' ')
	    CALL OPEN_IBIS_FILE( UNIT1, IBIS1,GRDIM,'READ' )
	ENDIF

C			See if there is an output file
	CALL XVPARM ('OUT', OUTNAME, COUNT, OUTDEF,0)
	IF (OUTDEF .EQ. 0) THEN
	    CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
	    IF (INPDEF .EQ. 1) THEN
C			If output and no input then just create a file
	    	CALL GET_INFO_FROM_PARMS
		CALL CREATE_IBIS_FILE( UNIT, IBIS )
		CALL IBIS_FILE_CLEAR( IBIS, STATUS )
		IF (STATUS.NE.1) CALL IBIS_SIGNAL( IBIS,STATUS,1)
	    ELSE
C			If output and input then copy desired columns
		CALL IBISCOPY(IBIS1, UNIT, .TRUE.)
		CALL IBIS_FILE_CLOSE( IBIS1, ' ', STATUS)
	    	CALL OPEN_IBIS_FILE( UNIT, IBIS, NCOL,'UPDATE' )
	    ENDIF
	    OUTUNIT = -1  !ie, dont use a temporary file
	    FILENAME = OUTNAME
	ELSE
	    IF (INPDEF .EQ. 1) THEN
	        CALL XVMESSAGE(
     +            ' Must specify input or output file',' ')
		CALL ABEND
	    ELSEIF (READONLY) THEN
		FILENAME = INPNAME
		UNIT = UNIT1
		IBIS = IBIS1
		OUTUNIT=-1
		CALL GET_INFO_FROM_FILE( IBIS,.FALSE.)
	    ELSE   !Create working buffer
		FILENAME = INPNAME
		OUTUNIT = UNIT1
		CALL IBISSCRATCH(UNIT)
		CALL IBISCOPY(IBIS1,UNIT,.FALSE.)
		CALL IBIS_FILE_CLOSE( IBIS1, ' ', STATUS)
	    	CALL OPEN_IBIS_FILE( UNIT, IBIS, NCOL,'UPDATE' )
	    ENDIF
	ENDIF

	
	IF (NCOL .GT. MAXFILECOLS) THEN
	   CALL XVMESSAGE (' Maximum number of columns exceeded.',' ')
	   CALL ABEND
	ENDIF
	RETURN
	END



c*************************************************************************
	SUBROUTINE GET_FMT_FROM_PARMS
C	    Initializes screen formats from parms.
	IMPLICIT NONE
	INTEGER*4	I, COUNT
	INTEGER*4	ASC, FORMCODE, SLEN, XLEN
	CHARACTER*64 STRING
	CHARACTER*1  CHR
	CHARACTER*32  INITFORMAT
	INCLUDE 'edibis.fin'

	CALL XVP ('FORMAT', INITFORMAT, COUNT)

	FORMCODE = 0
	DO I = 1,16		! Find out the data type real, int, or char
	    ASC = ICHAR(INITFORMAT(I:I))
	    IF (ASC .GE. 96)  ASC = ASC - 32
	    CHR = CHAR(ASC)
	    IF (CHR .EQ. 'F' .OR. CHR .EQ. 'E')  FORMCODE = FMT_REAL
	    IF (CHR .EQ. 'I')  FORMCODE = FMT_FULL
	    IF (CHR .EQ. 'A')  FORMCODE = FMT_A4
	ENDDO
	IF (FORMCODE .EQ. FMT_REAL) THEN	! See if legal format and find size
	    WRITE (STRING, INITFORMAT ,ERR=199) 7.0
	ELSE IF (FORMCODE .EQ. FMT_FULL) THEN
	    WRITE (STRING, INITFORMAT ,ERR=199) 7
	ELSE IF (FORMCODE .EQ. FMT_A4) THEN
	    WRITE (STRING, INITFORMAT ,ERR=199) 'XXXX'
	ELSE
	    GOTO 199
	ENDIF
	XLEN = SLEN(STRING)
	GOTO 200

199	CONTINUE		! Here if error in initial format
	INITFORMAT = '(F11.3)'
	XLEN = 11
	FORMCODE = FMT_REAL

200	CONTINUE
	DO I = 1, MAXFILECOLS
	    COLXLEN(I) = XLEN
	    COLFORMAT(I) = INITFORMAT
	    COLFORMCODE(I) = FORMCODE
	ENDDO

	RETURN
	END


c*************************************************************************
	SUBROUTINE GET_FMT_FROM_FILE
C	    Gets formats from IBIS-2 file
	IMPLICIT NONE
	INTEGER*4	I, ASIZE
	INTEGER*4	ASC, FORMCODE, XLEN
	CHARACTER*1  CHR
	CHARACTER*32  INITFORMAT
	INCLUDE 'edibis.fin'

	FORMCODE = 0
	DO I = 1, NCOL
	    ASC = ICHAR(COLFILFMT(I)(1:1))
	    IF (ASC .GE. 96)  ASC = ASC - 32
	    CHR = CHAR(ASC)
	    
	    IF (CHR .EQ. 'A') THEN
	    	READ(COLFILFMT(I)(2:),*)ASIZE
		IF (ASIZE.LE.4) THEN
		   INITFORMAT = '(2X,A4)'
		   FORMCODE=FMT_A4
		   XLEN=6
		ELSEIF (ASIZE.LE.16) THEN
		   INITFORMAT = '(X,A16)'
		   FORMCODE=FMT_A16
		   XLEN=17
		ELSE IF (ASIZE.LE.32) THEN
		   INITFORMAT = '(X,A32)'
		   FORMCODE=FMT_A32
		   XLEN=33
		ELSE
		   INITFORMAT = '(X,A64)'
		   FORMCODE=FMT_A64
		   XLEN=65
		ENDIF
	    ELSEIF (CHR .EQ. 'B') THEN
		INITFORMAT = '(I8.3)'
	    	FORMCODE = FMT_FULL
		XLEN=8
	    ELSEIF ( CHR .EQ. 'H') THEN
		INITFORMAT = '(I8.5)'
	    	FORMCODE = FMT_FULL
		XLEN=8
	    ELSEIF ( CHR .EQ. 'F')  THEN
		INITFORMAT = '(I12.10)'
	    	FORMCODE = FMT_FULL
		XLEN=12
	    ELSEIF ( CHR .EQ. 'D')  THEN
	    	FORMCODE = FMT_DOUB
		INITFORMAT = '(F11.3)'
		XLEN=11
	    ELSEIF ( CHR .EQ. 'C')  THEN
	    	FORMCODE = FMT_COMP
		INITFORMAT = '(X,''('',F8.3,'','',F8.3,'')'')'
		XLEN=20
	    ELSE
	    	FORMCODE = FMT_REAL
		INITFORMAT = '(F11.3)'
		XLEN=11
	    ENDIF
	    COLXLEN(I) = XLEN
	    COLFORMAT(I) = INITFORMAT
	    COLFORMCODE(I) = FORMCODE
	ENDDO

	RETURN
	END

c*************************************************************************
	SUBROUTINE INITIALIZE
C	    Initializes variables and sets up the screen.
	IMPLICIT NONE
	INTEGER*4 COUNT
	CHARACTER*10 XCMD
	INCLUDE 'edibis.fin'

	CALL XVPCNT ('COMMANDS', NUM_CMD)
	CALL XVPONE ('COMMANDS', XCMD,1,0)
	BATCH_MODE = (XCMD(1:1) .NE. ' ')
	IF (BATCH_MODE .AND. READONLY) THEN
		CALL XVMESSAGE('Cant batch-edit in READONLY mode!',' ')
		CALL ABEND
	ENDIF

	CALL SETUP_COLUMN_TRANS(IBIS)
	IF (FILEVERSION(6:6).EQ.'2') THEN
		CALL GET_FMT_FROM_FILE
	ELSE
		CALL GET_FMT_FROM_PARMS
	ENDIF

	CALL XVP ('SCRNSIZE', SCREENXSIZE, COUNT)
	
	IF (BATCH_MODE) THEN
		CUR_CMD = 1
		FIRSTYPOS = 1
		LASTYPOS = 1
		NUMDISPROWS = 1
		NUMDISPCOLS = 1
	
		FIRSTBUFROW = 1
		FIRSTBUFCOL = 1
	
		FORWARDMODE = .TRUE.
		HIGHLEN = 0
		
		DISPROW = 1
		DISPCOL = 1
		CURROW = 1
		CURCOL = 1
		RETURN
	ENDIF
	
	FIRSTYPOS = 5
	LASTYPOS = 22
	NUMDISPROWS = MAXROWS
	NUMDISPROWS = MIN (NUMDISPROWS, CLEN)
	NUMDISPCOLS = 8
	NUMDISPCOLS = MIN (NUMDISPCOLS, NCOL)

	FIRSTBUFROW = 1
	FIRSTBUFCOL = 1

	FORWARDMODE = .TRUE.
	HIGHLEN = 0
	
	DISPROW = 1
	DISPCOL = 1
	CURROW = 1
	CURCOL = 1

	CALL INITSCREEN
	CALL SETUPCOLDISPLAY
	CALL NEWSCREEN

	RETURN
	END

c*************************************************************************
	SUBROUTINE SETUPCOLDISPLAY
C	    Sets up the screen X positions for each display column
C	 based on the display lengths of the columns.  Also calculates
C	 the number of columns to display on the screen.
	IMPLICIT NONE
	INTEGER*4	COL, NUM
	LOGICAL*4 INBOUNDS
	INCLUDE 'edibis.fin'

	COLXPOS(DISPCOL) = 1
	COL = DISPCOL
	NUM = 1
	INBOUNDS=.TRUE.
	DO WHILE (INBOUNDS .AND. COL .LT. NCOL)
	    COLXPOS(COL+1) = COLXPOS(COL) + COLXLEN(COL)
	    COL = COL + 1
	    INBOUNDS = COLXPOS(COL)+COLXLEN(COL) .LE. SCREENXSIZE
	    IF (INBOUNDS) NUM = NUM + 1
	ENDDO
	IF (NUM .LT. NUMDISPCOLS) THEN
	    DO COL = DISPCOL+NUM, DISPCOL+NUMDISPCOLS-1
		CALL WRITEBUFCOL (COL)
	    ENDDO
	ELSE IF (NUM .GT. NUMDISPCOLS) THEN
	    DO COL = DISPCOL+NUMDISPCOLS, DISPCOL+NUM-1
		CALL READBUFCOL (COL)
	    ENDDO
	ENDIF
	NUMDISPCOLS = NUM

	RETURN
	END




C *****************************************************************
C          SCREEN DISPLAY SUBROUTINES 
C *****************************************************************


	SUBROUTINE DISPLAYSCREEN
C	    Displays all of the rows of data on the screen
	IMPLICIT NONE
	INTEGER*4	ROW, YPOS
	INCLUDE 'edibis.fin'

	YPOS = FIRSTYPOS
	DO ROW = DISPROW, DISPROW+MAXROWS-1
	    CALL DISPLAYROW (ROW, YPOS)
	    YPOS = YPOS + 1
	ENDDO
	HIGHLEN = 0
	
	CALL SHOW_DATA

	! Clear to end of Display Deleted
	
	RETURN
	END

c*************************************************************************
	SUBROUTINE FORMATELEMENT(OUTSTRING,ROW,COL,LEN)
	IMPLICIT NONE
	CHARACTER*(*) OUTSTRING
	INTEGER*4 COL,ROW,INCOL, I, ASC,LEN,LIMIT
	INCLUDE 'edibis.fin'

	INCOL = COL + 1 - FIRSTBUFCOL
	
	IF (COLFORMCODE(COL) .EQ. FMT_REAL) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_FULL) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_FULL(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_DOUB) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_DOUB(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_COMP) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_COMP(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A4) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_A4(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A16) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_A16(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A32) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_A32(ROW,INCOL)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A64) THEN
	   WRITE (OUTSTRING, COLFORMAT(COL),
     +                      ERR=199) IBISBUF_A64(ROW,INCOL)
	ENDIF
199	CONTINUE

	IF (COLFORMCODE(COL) .GE. FMT_A4) THEN
		LIMIT=33  ! Turn spaces to "."
	ELSE
		LIMIT=32  ! Leave spaces blank
	ENDIF
		
	DO I = 1, LEN
	   ASC = ICHAR(OUTSTRING(I:I))
	   IF (ASC .LT. LIMIT .OR. ASC .GE. 127)  
     +			OUTSTRING(I:I) = '.'
	ENDDO
	
	RETURN
	END
	
	
c*************************************************************************
	SUBROUTINE DISPLAYROW (ROW, YPOS)
C	    Displays a particular row of data at a specified Y position
C	 on the screen
	IMPLICIT NONE
	INTEGER*4	ROW, YPOS, COL, PTR, X, X2, LEN
	CHARACTER*200  STRING
	INCLUDE 'edibis.fin'

	X2 = 0
	CALL BLANKSTRING (STRING)
	PTR = ROW - FIRSTBUFROW + 1
	
	IF (ROW - DISPROW .LT. NUMDISPROWS) THEN
	  DO COL = DISPCOL, DISPCOL+NUMDISPCOLS-1
	    X = COLXPOS(COL)
	    LEN = COLXLEN(COL)
	    X2 = X + LEN - 1
	    CALL FORMATELEMENT(STRING(X:X2),PTR,COL,LEN)
	  ENDDO
	ENDIF

	CALL GOTOPOSITION(YPOS,1)
	CALL ERASELINE
	CALL GOTOPOSITION(YPOS,1)
	CALL SCREEN_WRITE(STRING(1:X2))

	RETURN
	END


c*************************************************************************
	SUBROUTINE DISPLAYHEADER
C	    Displays the file info line, the current row and column line,
C	 and the format line.
	IMPLICIT NONE
	INTEGER*4	SLEN
	INCLUDE 'edibis.fin'

	CALL GOTOPOSITION(1,1)
	CALL ERASELINE
	WRITE (OUTPUTSTRING,1) 'File: ',
     +                   FILENAME(1:SLEN(FILENAME)), 
     +			'     Columns: ',NCOL, '    Rows: ',CLEN, 
     +			'     Version: ',FILEVERSION
1	FORMAT (A,A,A,I5,A,I6,A,A)
	CALL SCREEN_WRITE(OUTPUTSTRING)

	CALL GOTOPOSITION(2,1)
	CALL ERASELINE
	WRITE (OUTPUTSTRING,2) 'Row: ',CURROW,'   Column: ', CURCOL
2	FORMAT(A5,I6,A11,I5)
	CALL SCREEN_WRITE(OUTPUTSTRING)

	CALL SHOW_HEADER

	CALL DISPLAYFORMATS

	RETURN
	END

c*************************************************************************
	SUBROUTINE MAKENUMSTRING(STRING,NUM)
C	    Converts Positive NUM to left-justified STRING
	IMPLICIT NONE
	INTEGER*4 I,NUM
	CHARACTER*(*) STRING
	CHARACTER*5 NUMSTR
	
	WRITE (NUMSTR,'(I5)') NUM
	I=1
	DO WHILE (NUMSTR(I:I).EQ.' '.AND.I.LT.5)
		I=I+1
	ENDDO
	READ (NUMSTR(I:),*) STRING

	RETURN
	END

c*************************************************************************
	SUBROUTINE DISPLAYFORMATS
C	    Displays the line showing the formats for all of the columns.
C	 Also displays the line showing the column numbers.
	IMPLICIT NONE
	INTEGER*4	COL, X, L, X2, XC, SLEN
	LOGICAL*4 IBIS2
	CHARACTER*150  CSTRING, FSTRING
	INCLUDE 'edibis.fin'

	X2 = 0
	IBIS2 = FILEVERSION(6:6).EQ.'2'
	CALL BLANKSTRING (CSTRING)
	CALL BLANKSTRING (FSTRING)
	DO COL = DISPCOL, DISPCOL+NUMDISPCOLS-1
	    X = COLXPOS(COL)
	    L = COLXLEN(COL)
	    XC = X + L - 1    ! Used to be X + L/2
	    X2 = X + L - 1
	    WRITE (CSTRING(XC-4:XC), '(I5)') COL
	    IF (IBIS2) THEN
	       X = X2 - SLEN(COLFILFMT(COL)) + 1
	       L = X2 - X + 1
	       FSTRING(X:X2) = COLFILFMT(COL)(1:L)
	      ! WRITE (FSTRING(X:X2), *, ERR=199) COLFILFMT(COL)(1:L)
	    ELSE
	       X = X2 - SLEN(COLFORMAT(COL)) + 1
	       L = X2 - X + 1
	       FSTRING(X:X2) = COLFORMAT(COL)(1:L)
	       ! WRITE (FSTRING(X:X2), *, ERR=199) COLFORMAT(COL)(1:L)
	    ENDIF
	    CONTINUE
	ENDDO
	
	CALL GOTOPOSITION(3,1)
	CALL ERASELINE
	CALL GOTOPOSITION(3,1)
	CALL SCREEN_WRITE(CSTRING(1:X2))

	CALL GOTOPOSITION(4,1)	
	CALL ERASELINE
	CALL GOTOPOSITION(4,1)	
	CALL SCREEN_WRITE(FSTRING(1:X2))
	
	CALL SHOW_FORMATS
	
	RETURN
	END



c*************************************************************************
	SUBROUTINE DEHIGHLIGHT
C	    Takes the last highlighted cell (if there is one)
C	 out of reverse video.  If HIGHLEN = 0 then no cell is currently
C	 highlighted.
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	IF (HIGHLEN .GT. 0) THEN
	    CALL GOTOPOSITION(HIGHY,HIGHX)
	    CALL SCREEN_WRITE(HIGHSTRING(1:HIGHLEN))
	    HIGHLEN = 0
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE HIGHLIGHT
C	    Takes the last highlighted cell out of reverse video
C	 and puts the new highlighted cell in reverse video.
	IMPLICIT NONE
	INTEGER*4	PTR
	INCLUDE 'edibis.fin'

	CALL DEHIGHLIGHT

        PTR = CURROW - FIRSTBUFROW + 1
        HIGHX = COLXPOS(CURCOL)
        HIGHY = FIRSTYPOS + CURROW - DISPROW
        HIGHLEN = COLXLEN(CURCOL)

	CALL FORMATELEMENT(HIGHSTRING(1:HIGHLEN),PTR,CURCOL,HIGHLEN)
	CALL HIGH_WRITE(HIGHY,HIGHX,HIGHSTRING(1:HIGHLEN))

	RETURN
	END

c*************************************************************************
	SUBROUTINE BLANKSTRING (STRING)
	IMPLICIT NONE
	CHARACTER*(*) STRING
	INTEGER*4	I

	DO I = 1, LEN(STRING)
	    STRING(I:I) = ' '
	ENDDO

	RETURN
	END
	
c*************************************************************************
	INTEGER*4 FUNCTION SLEN(STRING)
	IMPLICIT NONE
	INTEGER*4	I
	CHARACTER*(*) STRING

	I = LEN(STRING)
	DO WHILE (ICHAR(STRING(I:I)) .EQ. 32 .AND. I .GT. 1)
	    I = I - 1
	ENDDO
	SLEN = I
	RETURN
	END


c*************************************************************************
	SUBROUTINE UPDATEROWDISPLAY
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	WRITE (OUTPUTSTRING,'(I6)') CURROW
	CALL ROWNUM_WRITE( OUTPUTSTRING(1:6) )
	CALL SHOW_ROWNUM

	RETURN
	END

c*************************************************************************
	SUBROUTINE UPDATECOLDISPLAY
	IMPLICIT NONE
	INCLUDE 'edibis.fin'

	WRITE (OUTPUTSTRING,'(I5)') CURCOL
	CALL COLNUM_WRITE( OUTPUTSTRING(1:5) )
	CALL SHOW_COLNUM

	RETURN
	END



C *****************************************************************
C          FILE BUFFER SUBROUTINES 
C *****************************************************************

	SUBROUTINE FILLBUFFER
	IMPLICIT NONE
	INTEGER*4	COL
	INCLUDE 'edibis.fin'

	DO COL = DISPCOL, DISPCOL+NUMDISPCOLS-1
	    CALL READBUFCOL (COL)
	ENDDO

	RETURN
	END

c*************************************************************************
	SUBROUTINE OUTPUTBUFFER
	IMPLICIT NONE
	INTEGER*4	COL
	INCLUDE 'edibis.fin'

	IF (READONLY) RETURN
	DO COL = DISPCOL, DISPCOL+NUMDISPCOLS-1
	    CALL WRITEBUFCOL (COL)
	ENDDO

	RETURN
	END


c*************************************************************************
	SUBROUTINE CHECKBUFPOS (ROW)
C	    Checks to see if ROW is now in buffer.  If not then the
C	 buffer is written out, and the appropriate blocks read in.
	IMPLICIT NONE
	INTEGER*4	ROW

	INCLUDE 'edibis.fin'

	IF (ROW .LT. FIRSTBUFROW .OR. 
     +		ROW+NUMDISPROWS .GT. FIRSTBUFROW+255) THEN

     	    CALL OUTPUTBUFFER
	    FIRSTBUFROW = 128*((ROW-64)/128) + 1
	    FIRSTBUFROW = MIN (FIRSTBUFROW, 128*((CLEN-129)/128)+1)
	    FIRSTBUFROW = MAX (FIRSTBUFROW, 1)
	    CALL FILLBUFFER

	ENDIF

	RETURN
	END
c*************************************************************************
	SUBROUTINE SETDISPCOL (COL)
C	    Checks to see if COL is now in scroll buffer.  If not then
C	 the buffer is written out, and the appropriate blocks read in.
	IMPLICIT NONE
	LOGICAL*4 OUTOFBOUNDS
	INTEGER*4	 COL, INC

	INCLUDE 'edibis.fin'

	OUTOFBOUNDS = (COL .LT. FIRSTBUFCOL .OR. 
     +		  COL+NUMDISPCOLS .GT. FIRSTBUFCOL+MAXBUFCOLS)
     
        IF (OUTOFBOUNDS) CALL OUTPUTBUFFER
	DISPCOL = COL
	IF (OUTOFBOUNDS) THEN
	    INC = MAXBUFCOLS/2
	    FIRSTBUFCOL = INC*((COL-1)/INC) + 1
	    FIRSTBUFCOL = MIN (FIRSTBUFCOL, INC*((NCOL-INC)/INC)+1)
	    FIRSTBUFCOL = MAX (FIRSTBUFCOL, 1)
	    CALL FILLBUFFER
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE WRITEBUFCOL (COL)
C	    Writes one column of the buffer (two blocks) out to the file
C	    if the column has been changed.  A valid FIRSTBUFROW and
C	    FIRSTBUFCOL is required.
	IMPLICIT NONE
	INTEGER*4	STATUS,NROWS,COL,OUTCOL
	INCLUDE 'edibis.fin'

	IF (READONLY) RETURN

	NROWS = MAXBUFROWS
	IF (FIRSTBUFROW + NROWS -1 .GT. CLEN) 
     +       NROWS = CLEN + 1 - FIRSTBUFROW
   
	IF (COLCHANGED(COL)) THEN
	    OUTCOL = COL + 1 - FIRSTBUFCOL

	    IF (COLFORMCODE(COL) .EQ. FMT_REAL) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_FULL) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_FULL(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_DOUB) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_DOUB(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_COMP) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_COMP(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_A4) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_A4(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_A16) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_A16(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_A32) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_A32(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ELSE IF (COLFORMCODE(COL) .EQ. FMT_A64) THEN
	    	CALL IBIS_COLUMN_WRITE(IBIS,
     +             IBISBUF_A64(1,OUTCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	    ENDIF
	    IF (STATUS .NE. 1) THEN
		CALL NEWSCREEN
		CALL ENDSCREEN
		CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    ENDIF
	ENDIF

	RETURN
	END

c*************************************************************************
	SUBROUTINE READBUFCOL (COL)
C	    Read two blocks from the file to fill one column of the buffer.
C	 A valid FIRSTBUFROW is required.
	IMPLICIT NONE
	INTEGER*4	COL,INCOL, STATUS,NROWS
	INCLUDE 'edibis.fin'


	! Need to allow for ASCII columns
	NROWS = MAXBUFROWS
	IF (FIRSTBUFROW + NROWS -1 .GT. CLEN) 
     +       NROWS = CLEN + 1 - FIRSTBUFROW
	INCOL = COL + 1 - FIRSTBUFCOL


	IF (COLFORMCODE(COL) .EQ. FMT_REAL) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_FULL) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_FULL(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_DOUB) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_DOUB(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_COMP) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_COMP(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A4) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_A4(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A16) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_A16(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A32) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_A32(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ELSE IF (COLFORMCODE(COL) .EQ. FMT_A64) THEN
	    	CALL IBIS_COLUMN_READ(IBIS,
     +             IBISBUF_A64(1,INCOL),COL,FIRSTBUFROW,NROWS,STATUS)
	ENDIF

	IF (STATUS .NE. 1) THEN
		CALL NEWSCREEN
		CALL ENDSCREEN
		CALL IBIS_SIGNAL(IBIS,STATUS,1)
	ENDIF
	COLCHANGED(COL) = .FALSE.

	RETURN
	END



C *****************************************************************
C          KEYBOARD INPUT AND PARSING SUBROUTINES 
C *****************************************************************

	SUBROUTINE GETCOMMAND (COMMAND, ARGSTRING, ARGLEN)
C	    This subroutine returns the next command, either
C           from the keypad, command-line or parms if batch-mode.
	IMPLICIT NONE
	INTEGER*4	COMMAND, ARGLEN
	CHARACTER*(*)	ARGSTRING
	INCLUDE 'edibis.fin'

	! Get the command and handle error messages
c	print *,"MODE = BATCHMODE = ",BATCH_MODE
	IF (BATCH_MODE) THEN
		CALL GET_COMMAND_FROM_PARM(COMMAND,ARGSTRING,ARGLEN)
	ELSE
		CALL DISPLAY_PROMPT('] ')
		CALL GET_COMMAND_FROM_KEYBOARD(COMMAND,ARGSTRING,ARGLEN)
	ENDIF
	RETURN
	END
c*************************************************************************
	SUBROUTINE GET_COMMAND_FROM_PARM (COMMAND, ARGSTRING, ARGLEN)
C	    This subroutine gets the commands from the COMMANDS parm
	IMPLICIT NONE
	INTEGER*4	COMMAND, ARGLEN
	CHARACTER*(*)	ARGSTRING
	CHARACTER*200 CMDLINE
	INCLUDE 'edibis.fin'

345     CONTINUE
c	print *,"COMMAND, CUR_CMD and NUM_CMD = ",COMMAND,CUR_CMD,NUM_CMD
	IF (CUR_CMD .GT. NUM_CMD) THEN
		COMMAND = ENDEDIT		!automatically terminate when end of commands
		RETURN
	ENDIF

	CALL XVPONE ('COMMANDS', CMDLINE,CUR_CMD,0)
	CUR_CMD = CUR_CMD + 1
c	print *, "CMDLINE = ",CMDLINE
	CALL CONVERT_STRING_TO_COMMAND (CMDLINE,COMMAND, 
     +                    ARGSTRING, ARGLEN)
c	print *,"COMMAND, ARGSTRING, ARGLEN = ",COMMAND, ARGSTRING, ARGLEN 
	IF (COMMAND.EQ.BADCOMMAND) THEN
		CALL XVMESSAGE(ARGSTRING,' ')
		GOTO 345
	ENDIF

	RETURN
	END
c*************************************************************************
	SUBROUTINE CONVERT_STRING_TO_COMMAND (CMDLINE, COMMAND, 
     +                     ARGSTRING, ARGLEN)
C	    This subroutine converts a command
	IMPLICIT NONE
	INTEGER*4	COMMAND, ARGLEN, ARGPOS, CMDPOS
	INTEGER*4	SLEN,STAT,LN, THISROW,THISCOL
	CHARACTER*(*)	ARGSTRING,CMDLINE
	CHARACTER*200 XCMDSTR
	CHARACTER*3 XCOM
	INCLUDE 'edibis.fin'
c
c	8-14-2011 - RJB - had to add new call CALL FIND_ROWCOL
c	THISROW and THISCOL are found from list directed read
c	in gcc 4.4.4:
c      READ (CMDLINE(CMDPOS:),*,ERR=332) THISCOL i
c	does not detect properly the trailing ")" as a terminator
C	It gives an error, where the older compiler evidently 
c	recognized ) as a proper terminator
c
C   -- skip spaces
	CMDPOS=1
	LN = SLEN(CMDLINE)
c	print *,"LN = ",LN
        DO WHILE (CMDLINE(CMDPOS:CMDPOS).EQ.' '.AND.CMDPOS.LE.LN)
		CMDPOS = CMDPOS+1
	ENDDO
	IF (CMDPOS.GT.LN) CMDPOS=1
C   -- check to see if there is a (row,column) position prefix
	IF (CMDLINE(CMDPOS:CMDPOS).EQ.'(') THEN
c	  print *, "found ("

c get row and column
	   CALL FIND_ROWCOL(CMDLINE,CMDPOS,LN,THISROW,THISCOL)

	   IF (CMDPOS.GT.LN) GOTO 345  !Error	GOTO 345
	   IF (THISCOL.GT.0) CALL DO_GOTOCOL (THISCOL)
	   IF (THISROW.GT.0) CALL DO_GOTOROW (THISROW)
	   IF (CMDPOS.EQ.LN) RETURN
	   CMDPOS = CMDPOS+1
	   DO WHILE (CMDLINE(CMDPOS:CMDPOS).EQ.' '.AND.CMDPOS.LE.LN)
	      CMDPOS = CMDPOS+1
	   ENDDO
	ENDIF			!
	XCMDSTR = CMDLINE(CMDPOS:)

C   -- Find the beginning of the argument following command
	ARGPOS=1
	LN = SLEN(XCMDSTR)
	DO WHILE (XCMDSTR(ARGPOS:ARGPOS).NE.' ' .AND. ARGPOS.LE.LN)
	    ARGPOS = ARGPOS+1
	ENDDO
	DO WHILE (XCMDSTR(ARGPOS:ARGPOS).EQ.' '.AND. ARGPOS.LE.LN)
	    ARGPOS = ARGPOS+1
	ENDDO

	ARGLEN = LN+1-ARGPOS
	IF (ARGPOS .LE. LN) ARGSTRING = XCMDSTR(ARGPOS:)

C   -- Parse the command
	XCOM = XCMDSTR(1:3)
	CALL CCASE(XCOM,1,3) !Convert to uppercase
	IF (XCOM.EQ.'ROW') THEN
		COMMAND=GOTOROW
	ELSEIF (XCOM.EQ.'COL') THEN
		COMMAND=GOTOCOL
	ELSEIF (XCOM.EQ.'FOR') THEN
		COMMAND=CHNGFORMAT
	ELSEIF (XCOM.EQ.'DEL') THEN
		COMMAND=DELETEROW
	ELSEIF (XCOM.EQ.'INS') THEN
		COMMAND=INSERTROW
	ELSEIF (XCOM.EQ.'CUT') THEN
		COMMAND=ZEROCELL
	ELSEIF (XCOM.EQ.'FWD') THEN
		FORWARDMODE = .TRUE.
		ARGSTRING=' '
		COMMAND=TEXTSTRING
	ELSEIF (XCOM.EQ.'BAC') THEN
		FORWARDMODE = .FALSE.
		ARGSTRING=' '
		COMMAND=TEXTSTRING
	ELSEIF (XCOM.EQ.'JUM') THEN !already jumped
		ARGSTRING=' '
		COMMAND=TEXTSTRING
	ELSEIF (XCOM.EQ.'PAS') THEN
		COMMAND=RECALLCELL
	ELSEIF (XCOM.EQ.'SET') THEN
		COMMAND=SETTEXTVAL
	ELSEIF (XCOM.EQ.'EXI') THEN
		COMMAND=ENDEDIT
	ELSEIF (XCOM.EQ.'TOP') THEN
		COMMAND=FILETOP
	ELSEIF (XCOM.EQ.'BOT') THEN
		COMMAND=FILEBOTTOM
	ELSEIF (XCOM.EQ.'LEF') THEN
		COMMAND = PAGELEFT
	ELSEIF (XCOM.EQ.'RIG') THEN
		COMMAND = PAGERIGHT
	ELSEIF (XCOM.EQ.'SEA') THEN
		IF (ARGLEN .GT. 0) 
     +             CALL SET_SEARCH_STRING(ARGSTRING,ARGLEN,STAT)
		IF (.NOT.INITSET) THEN
		  ARGSTRING = 'NO SEARCH STRING SPECIFIED'
	          COMMAND=BADCOMMAND
	          RETURN
		ENDIF
		COMMAND=SEARCH
	ELSE
		GOTO 345
	ENDIF

	RETURN

 345    ARGSTRING = 'Unknown Command:'//CMDLINE

	COMMAND=BADCOMMAND
	if (BATCH_MODE) then
	     call xvmessage (ARGSTRING,' ')
	     call xvmessage ('??E - BATCH_MODE abend',' ')
	    call abend 
	endif
	RETURN
	END
c*************************************************************************
	SUBROUTINE FIND_ROWCOL(CMDLINE,CMDPOS,LN,THISROW,THISCOL)
c
c	This subroutine parses out the row and column from commands
c	such as "(1,2) SET 9.123". In the original code (1,2) was
c	extracted from the command line through 
c
c
        IMPLICIT NONE
        INTEGER*4  i,cmdlen,LN,CMDPOS,THISROW,THISCOL
        CHARACTER*(*)   CMDLINE
        CHARACTER*200 cmdshort

	   cmdshort(1:200) = ' '
           i = 0
           do WHILE (CMDLINE(CMDPOS:CMDPOS).NE.')' .AND. CMDPOS.LE.LN)
                i= i + 1
                cmdshort(i:i) = CMDLINE(CMDPOS:CMDPOS)
                CMDPOS = CMDPOS+1
           enddo
           cmdlen = i
           cmdshort(i+1:i+1) = ','
           i = 1
c          print *,"CMDshort(i+1:) = ",CMDshort(i+1:)
           READ (CMDshort(i+1:),*, ERR=330) THISROW                 !ERR=345
c           print *,"THISROW = ",THISROW
           DO WHILE (CMDshort(i:i).NE.',' .AND. i.LE.cmdlen)
                i = i + 1
           ENDDO
c           print *,"CMDshort(i+1:) = ",CMDshort(i+1:)
           READ (CMDshort(i+1:),*,ERR=332) THISCOL        ! ERR=332               !ERR=345 - 3rd error
c            print *,"THISCOL = ",THISCOL

	RETURN
 330    continue
	call xvmessage ("??E - Error reading row in (row,col)"," ")
	call abend
 332    continue
	call xvmessage ("??E - Error reading column in (row,col)"," ")
        call abend 
        return



	END
c*************************************************************************
	SUBROUTINE GET_COMMAND_FROM_KEYBOARD (COMMAND, 
     +                                   ARGSTRING, ARGLEN)
C	    This subroutine translates keys hit into editting commands without
C	 prompt. Normal keys are built up into a text string (ARGSTRING) to
C	 return.  Keypad keys are translated. If the "/ command" form is used
C        command names are translated.
	IMPLICIT NONE
	INTEGER*4	COMMAND, ARGLEN
	CHARACTER*(*)	ARGSTRING
	CHARACTER*200 CMDLINE
	INTEGER*4	POS, KEY, LN
	LOGICAL*4	PF1MODE
	INCLUDE 'edibis.fin'

	LN = LEN(ARGSTRING)
	POS = 0
	CALL BLANKSTRING (ARGSTRING)

	PF1MODE = .FALSE.
	COMMAND = NONE
	DO WHILE (COMMAND .EQ. NONE)
	    CALL KEYINPUT (KEY)

C			Build up the text string
	    IF (KEY .GE. 32) THEN
		IF (KEY .EQ. 127 .AND. POS .GT. 0) THEN !Backspace
		    CALL DELETE_CHAR
		    ARGSTRING(POS:POS) = ' '
		    POS = POS - 1
		ELSE IF (KEY.NE.127 .AND. POS .LT. LN) THEN
		    CALL SHOW_CHAR( KEY )
		    POS = POS + 1
		    ARGSTRING(POS:POS) = CHAR(KEY)
		ENDIF
	    ELSE IF (KEY .EQ. 13) THEN		! Carriage return ends string
		COMMAND = TEXTSTRING
	    ELSE IF (KEY .EQ. 26) THEN		! Control Z ends edit
		COMMAND = ENDEDIT
	    ELSE IF (KEY .EQ. 18) THEN		! Control R refeshes screen
		COMMAND = REFRESH
	    ELSE IF (KEY .EQ. 6) THEN		! Control F for changing formats

		COMMAND = CHNGFORMAT
	    ELSE IF (KEY .EQ. 4) THEN		! Control D to quit
		COMMAND = QUIT

C			Process keys that are independent of PF1 mode
	    ELSE IF (KEY .EQ. -1) THEN
		COMMAND = UPARROW
	    ELSE IF (KEY .EQ. -2) THEN
		COMMAND = DOWNARROW
	    ELSE IF (KEY .EQ. -3) THEN
		COMMAND = RIGHTARROW
	    ELSE IF (KEY .EQ. -4) THEN
		COMMAND = LEFTARROW
	    ELSE IF (KEY .EQ. -6) THEN
		COMMAND = HELP
	    ELSE IF (KEY .EQ. -5) THEN ! toggle PF1MODE
		PF1MODE = .NOT.PF1MODE

C			Process PF1 mode commands
	    ELSE IF (PF1MODE) THEN
		IF (KEY .EQ. -14) THEN
		    COMMAND = FILEBOTTOM
		ELSE IF (KEY .EQ. -15) THEN
		    COMMAND = FILETOP
	        ELSE IF (KEY .EQ. -9) THEN
		    COMMAND = GOTOCOL
		ELSE IF (KEY .EQ. -8) THEN
		    COMMAND = INSERTROW
		ELSE IF (KEY .EQ. -7) THEN
		    COMMAND = INITSEARCH
		ELSE IF (KEY .EQ. -21) THEN
		    COMMAND = RECALLCELL
		ENDIF

C			Process non PF1 mode commands
	    ELSE IF (.NOT. PF1MODE) THEN
		IF (KEY .EQ. -14) THEN
		    FORWARDMODE = .TRUE.
		ELSE IF (KEY .EQ. -15) THEN
		    FORWARDMODE = .FALSE.
		ELSE IF (KEY .EQ. -8) THEN
		    COMMAND = DELETEROW
	        ELSE IF (KEY .EQ. -9) THEN
		    COMMAND = GOTOROW
		ELSE IF (KEY .EQ. -7) THEN
		    COMMAND = SEARCH
		ELSE IF (KEY .EQ. -21) THEN
		    COMMAND = ZEROCELL

		ELSE IF (FORWARDMODE) THEN	! Process forward mode commands
		    IF (KEY .EQ. -18) THEN
			COMMAND = PAGEDOWN
		    ELSE IF (KEY .EQ. -12) THEN
			COMMAND = PAGERIGHT
		    ELSE IF (KEY .EQ. -17) THEN
			COMMAND = FILEBOTTOM
		    ENDIF
					! Process backward mode commands

		ELSE IF (.NOT. FORWARDMODE) THEN 
		    IF (KEY .EQ. -18) THEN
			COMMAND = PAGEUP
		    ELSE IF (KEY .EQ. -12) THEN
			COMMAND = PAGELEFT
		    ELSE IF (KEY .EQ. -17) THEN
			COMMAND = FILETOP
		    ENDIF
		ENDIF
	   ENDIF
	ENDDO

	CALL ERASELINE

	ARGLEN = POS

	! Trap the special '/ command' option
	IF (COMMAND.EQ.TEXTSTRING .AND. ARGSTRING(1:1).EQ.'/') THEN
	 	CMDLINE = ARGSTRING(2:)
		CALL CONVERT_STRING_TO_COMMAND (CMDLINE, COMMAND, 
     +                             ARGSTRING, ARGLEN)		
		IF (COMMAND.EQ.BADCOMMAND) THEN
	     	   CALL EMIT_BEEP
		   CALL DISPLAY_STATUS(ARGSTRING)
		ENDIF
	ENDIF

	RETURN
	END


c*************************************************************************
	SUBROUTINE KEYINPUT (KEY)
C	    Returns a integer value for a keyboard input.
C	 The ascii value is returned for ascii values 1 thru 127 
C	 (i.e. normal and control characters).
C	 VT100 keypad mode keys return the following codes:
C	 Up arrow = -1   Down arrow = -2   Right arrow = -3   Left arrow = -4
C	 PF1 = -5    PF2 = -6   PF3 = -7   PF4 = -8    Enter = -9
C	 Keypad 0 thru Keypad 9  = -10 thru -19
C	 Keypad , = -20.   Keypad - = -21.   Keypad . = -21.
C	 Zero is returned for all other keys
C	 CSI (<esc> [) sequences are eaten correctly and return 0
	IMPLICIT NONE
	INTEGER*4	 CH
	INTEGER*4 GET_CHAR
	INTEGER*4 KEY

	KEY = 0
	CH = GET_CHAR(.TRUE.)

	IF (CH .EQ. 27) THEN  !ESCAPE FOUND
	    CH = GET_CHAR(.TRUE.)
	    IF (CH .EQ. 91) THEN   ! '[' FOUND
		CH = GET_CHAR(.TRUE.)
		! between A and D
		IF (CH .GE. 65 .AND. CH .LE. 68) THEN
		    KEY = -(CH - 64)
		ELSE        ! STOP IF CH = '^' OR '~' (on WYSE TERMINALS)
		    DO WHILE ((CH .NE. 94).AND.(CH.NE.126))
			CH = GET_CHAR(.TRUE.)
		    ENDDO
		ENDIF
	    ELSE IF (CH .EQ. 79) THEN  ! 'O' FOUND
		CH = GET_CHAR(.TRUE.)
		IF (CH .GE. 65 .AND. CH .LE. 68) THEN
		    KEY = -(CH - 64)
		ELSE IF (CH .GE. 80 .AND. CH .LE. 83) THEN
		    KEY = -(CH - 75)
		ELSE IF (CH .EQ. 77) THEN
		    KEY = -9
		ELSE IF (CH .GE. 112 .AND. CH .LE. 121) THEN
		    KEY = -(CH - 102)
		ELSE IF (CH .GE. 108 .AND. CH .LE. 110) THEN
		    KEY = -(CH - 88)
		ENDIF
	    ENDIF
	ELSE IF (CH .GE. 1 .AND. CH .LE. 127) THEN
	    KEY = CH
	ENDIF

	RETURN
	END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create edibis_windows.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * Portable "Curses" window screen management for EDIBIS
 *   12-08-2012 - RJB - Converted char to const chtype
 *                  in void FTN_NAME(show_char)(const chtype *thechar);
 *                  on LINUX and MacOSX 10.6.8 
 */

#include "xvmaininc.h"
#include <curses.h>
#include <string.h>
#include "ftnbridge.h"
#define MAX_STRING 200

/*prototypes*/
void FTN_NAME(initscreen)(int batch);
void FTN_NAME(endscreen)(void);
void FTN_NAME(emit_beep)(void);
void FTN_NAME(high_write)(int *y, int *x, char *string, ZFORSTR_PARAM);
void FTN_NAME(gotoposition)(int *vert,int *horiz);
void FTN_NAME(gotoy)(int *vert);
void FTN_NAME(eraseline)(void);
void FTN_NAME(delete_char)(void);
void FTN_NAME(show_char)(const chtype *thechar);        //was int *thechar
void FTN_NAME(newscreen)(void);
void FTN_NAME(show_help)(void);
void FTN_NAME(show_nokeypad_help)(void);
void FTN_NAME(show_formats)(void);
void FTN_NAME(show_data)(void);
void FTN_NAME(clear_data)(void);
void FTN_NAME(show_header)(void);
void FTN_NAME(show_rownum)(void);
void FTN_NAME(show_colnum)(void);
void FTN_NAME(row_write)(char *string, int *row, ZFORSTR_PARAM);
void FTN_NAME(rownum_write)(char *string, ZFORSTR_PARAM);
void FTN_NAME(colnum_write)(char *string, ZFORSTR_PARAM);
void FTN_NAME(screen_write)(char *string, ZFORSTR_PARAM);
void FTN_NAME(display_prompt)(char *string, ZFORSTR_PARAM);
void FTN_NAME(display_status)(char *string, ZFORSTR_PARAM);

#if VMS_OS
#  define 	STANDOUT(win) wsetattr(win,_REVERSE)
#  define 	STANDEND(win) wclrattr(win,_REVERSE)
#else
#  ifdef A_REVERSE /* Then attributes work */
#    define 	STANDOUT(win) wattron(win,A_REVERSE)
#    define 	STANDEND(win) wattroff(win,A_REVERSE)
#  else /* try the only thing left */
#    define  STANDOUT(win) wstandout(win)
#    define  STANDEND(win) wstandend(win)
#  endif
#endif


#  define beep() printf("%c",7); 

#define PROMPTPOS 22
#define STATUSPOS 22
#define DATAPOS 4
#define DATASIZE 18
#define RCPOS 1
#define ROWPOS 5
#define COLPOS 22
#define FORMATPOS 2

static WINDOW *WEdit=(WINDOW *)0;
static WINDOW *WHeader=(WINDOW *)0;
static WINDOW *WFormats=(WINDOW *)0;
static WINDOW *WColNum=(WINDOW *)0;
static WINDOW *WRowNum=(WINDOW *)0;
static WINDOW *WData=(WINDOW *)0;
//static WINDOW *WPrompt=(WINDOW *)0;
//static WINDOW *WStatus=(WINDOW *)0;
static char keypad_mode[20];

void FTN_NAME(initscreen)(batch)
int batch;
{

#if VMS_OS
	strcpy(keypad_mode,"\033=");
#endif
#if UNIX_OS
	strcpy(keypad_mode,"\033[?1h\033=");
#endif

	printf(keypad_mode);  /* use "keypad()" for SYSV */

	initscr();
	noecho(); /* don't echo chars on terminal */
	crmode(); /* immediately transmit chars; no filter */
	nonl();   /* don't convert nl->CR/LF */

	WEdit = newwin(0,0,0,0);
		WHeader = subwin( WEdit,   2,0,0,0);
			WRowNum = subwin( WEdit, 1,6, RCPOS,  ROWPOS);
			WColNum = subwin( WEdit, 1,5, RCPOS,  COLPOS);
		WFormats = subwin( WEdit,  2,0,FORMATPOS,0);
		WData = subwin( WEdit,DATASIZE ,0,DATAPOS,0);

}

void FTN_NAME(endscreen)(void)
{
	endwin();
}

void FTN_NAME(emit_beep)(void)
{
	beep();
}

void FTN_NAME(high_write)(int *y, int *x, char *string, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
        char c_string[MAX_STRING+1];
	
        zsfor2c(c_string, MAX_STRING, string, &y, 3, 3, 1, string);
	wmove(WEdit,(*y)-1,(*x)-1);
	STANDOUT(WEdit);
	waddstr(WEdit,c_string);
	touchwin(WEdit);
	wrefresh(WEdit);
	STANDEND(WEdit);
}

void FTN_NAME(gotoposition)(vert,horiz)
int *vert;
int *horiz;
{
	wmove(WEdit,(*vert)-1,(*horiz)-1);
}

void FTN_NAME(gotoy)(vert)
int *vert;
{
	wmove(WEdit,(*vert)-1,0);
}

void FTN_NAME(eraseline)(void)
{
	wclrtoeol(WEdit);
 }


void FTN_NAME(delete_char)(void)
{
	int y,x;
	
	getyx(WEdit,y,x);
	wmove(WEdit,y,x-1);
	wclrtoeol(WEdit);
	touchwin(WEdit);
	wrefresh(WEdit);
}

void FTN_NAME(show_char)(thechar)
const chtype *thechar;                      // was int *thechar  12/8/2012
{
	waddch(WEdit,*thechar);
	touchwin(WEdit);
	wrefresh(WEdit);
}


void FTN_NAME(newscreen)(void)
{
	clear(); 
}


void FTN_NAME(show_help)(void)
{
	int line=0;

	wclear(WEdit);

#define WP(str) mvwaddstr(WEdit,line++,0,(str));
WP("          EDIBIS COMMANDS                 +-----------------------------------+")
WP("    (For NOKEYPAD help, hit return)       |        |        | FNDNXT | DELETE |")
WP("Arrow keys move current cell one place.   |  GOLD  |  HELP  |        |  (ROW) |")
WP("ADVANCE and BACKUP set direction.         |        |        |  FIND  | INSERT |")
WP("                                          |--------+--------+--------+--------|")
WP("FIND Search Strings:                      |        | PAGE   |        |  CUT   |")
WP("----------------------                    |   --   | UP/DWN |   --   | (CELL) |")
WP("  VALUE     -Exact Match                  |        |        |        | PASTE  |")
WP("  MIN:MAX   -Between values               |--------+--------+--------+--------|")
WP("  MIN:      -Value and above              | ADVANCE| BACKUP |        |        |")
WP("  :MAX      -Value and below              |        |        |   --   |   --   |")
WP("                                          | BOTTOM |  TOP   |        |        |")
WP("Other Commands:                           |--------+--------+--------+--------|")
WP("---------------                           |        | PAGE   |        | GO TO  |")
WP("CTRL/R      Refresh screen                |   --   | RT/LFT |   --   |  ROW   |")
WP("CTRL/F      Display values using format   |        |        |        |        |")
WP("QUIT        Quit and exit to VICAR        |--------+--------+--------|        |")
WP("EXIT        Save and exit to VICAR        |                 |        | GO TO  |")
WP("<RETURN>    Change cell value to input    |      --         |   --   |  COL   |")
WP("/ <command> Use NOKEYPAD command          +-----------------+--------+--------+")
WP("                                           To exit, press the spacebar.        ")

	touchwin( WEdit );
	wrefresh(WEdit);
}


void FTN_NAME(show_nokeypad_help)(void)
{
	int line=0;

	wclear(WEdit);

#define WP(str) mvwaddstr(WEdit,line++,0,(str));
WP("NOKEYPAD: The no-keypad commands may  be used interactively by placing the     ")
WP("command on the EDIBIS command-line, prefaced by a '/' character, e.g.          ")
WP("                                                                               ")             
WP("    ]  /(1,2) set 5.1                                                          ") 
WP("                                                                               ")             
WP("The (row,col) parameter is optional and defaults to current cursor position.   ")
WP("                                                                               ")             
WP("Command (may be abbreviated)                     Function                      ")
WP("-----------------------------   ---------------------------------------------  ")
WP("(row,col) SET <value>           Change the value of cell (row,col) to <value>  ")
WP("(row,col) DEL*ETE <numrows>     Delete <numrows> rows, starting at <row>       ")
WP("(row,col) INS*ERT <numrows>     Delete <numrows> rows, starting at <row>       ")
WP("(row,col) JUM*P                 Jump to position (row,col) in file.            ")
WP("(row,col) CUT                   Copy current cell value into buffer, and clear ")
WP("(row,col) PAS*TE                Paste buffer into current cell                 ")
WP("(row,col) SEA*RCH <string>      Search fwd/backwd in column for range <string>.")
WP("(row,col) FOR*MAT (formt)       (IBIS-1 only) Set the column FORTRAN FORMAT.   ")
WP("          FWD/BAC*KWARD         Set search direction ForWarD(down)/BACkwd(Up).")
WP("          TOP/BOTTOM            Go to top/bottom of file                       ")
WP("          LEF*T/RIG*HT          Go one page left/right in file                 ")
WP("          ROW <rownum>          Go to row <rownum>, same column                ")
WP("          COL*UMN <colnum>      Go to column <colnum>, same row                ")
WP("To exit, press any key.                                                        ")

	touchwin( WEdit );
	wrefresh(WEdit);
}

void FTN_NAME(show_formats)(void)
{
	touchwin( WEdit );
	wrefresh( WFormats );
}


void FTN_NAME(show_data)(void)
{
	touchwin( WEdit );
}

void FTN_NAME(clear_data)(void)
{
   wclear(WData);
   touchwin(WData);
}

void FTN_NAME(show_header)(void)
{
	touchwin( WEdit );
	wrefresh( WHeader );
}

void FTN_NAME(show_rownum)(void)
{
	touchwin( WRowNum );
}

void FTN_NAME(show_colnum)(void)
{
	touchwin( WColNum );
}


/*
 *  Append a string of characters, with no
 *  end-of-line return. This avoids the VAX-VMS prompting
 *  '+','$' carriage-control extension, which is non-portable.
 */


void FTN_NAME(row_write)(char *string, int *row, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 2, 1, 1, row);
   wmove(WEdit,DATAPOS + (*row)-1,0);
   waddstr(WEdit,c_string);
}

void FTN_NAME(rownum_write)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   wclear(WRowNum);
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,RCPOS,ROWPOS);
   waddstr(WEdit,c_string);
}

void FTN_NAME(colnum_write)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   wclear(WColNum);
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,RCPOS,COLPOS);
   waddstr(WEdit,c_string);
}

void FTN_NAME(screen_write)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   waddstr(WEdit,c_string);
}

void FTN_NAME(display_prompt)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,PROMPTPOS,0);
   wclrtoeol(WEdit);
   wmove(WEdit,PROMPTPOS,0);
   waddstr(WEdit,c_string);
   wrefresh(WEdit);
}

void FTN_NAME(display_status)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
//   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,STATUSPOS,0);
   wclrtoeol(WEdit);
   wmove(WEdit,STATUSPOS,0);
   waddstr(WEdit,c_string);
   waddstr(WEdit," (hit <return> to continue)");
   wrefresh(WEdit);
   wgetch(WEdit);
   wmove(WEdit,STATUSPOS,0);
   wclrtoeol(WEdit);
   wrefresh(WEdit);
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create edibis_terminal.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  edibis_terminal.c
 *
 *   Handles ALL terminal I/O  and does non-blocking
 *   reads direct from keyboard/pad.
 *
 *   NB: This is not fully ported to all hosts.
 *   8-13-2011 - RJB - Fixes for gcc4.4.4 on 64-bit linux 
 *   12-08-2012 - RJB - Converted to POSIX termios.h compatibility
 *                  on LINUX and MacOSX 10.6.8 

 */

#include <stdio.h>
#include <unistd.h>         /* read and close */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"        /* zvmessage */
#if VMS_OS
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#else
#ifndef SOLARIS
#include <termios.h>        /* replaced termio.h - 12/8/2012 */
#endif
#ifdef SOLARIS
#include <termio.h>
#include <curses.h>
#endif
#include <term.h>
#include <sys/types.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#define RESET		0
#define RAW		1
#define STDCHAR		2
#define CTRLCHAR	3
#endif

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

#ifndef BOOLEAN
typedef unsigned char 	BOOLEAN;
#endif

#define WAIT_INPUT	20
#define POLL_INPUT	21

#if UNIX_OS
/* prototypes */
int FTN_NAME(get_char)(int *startup);
static int settermmode(int terminal, int mode);
#endif

/*  This routine waits for the user to press a key, then processes the key    */
/*  and returns the value to the calling context.  The 'startup' parameter    */
/*  should be set to True (1) while getting input.  The routine should be     */
/*  called one last time when you are through gathering input with a para-    */
/*  meter value of False (0) to restore the terminal to normal.               */

/*  --Borrowed from GETINPUT.COM. May be eliminated if and when the           */
/*  "waitforinput" call can indicate whether the key returned was a Keypad    */
/*   escape sequence or not. This routine returns everything.                 */

int FTN_NAME(get_char)(startup)
int *startup;
{
   char inpchar, errmsg[81];
   static BOOLEAN first = TRUE;

#if VMS_OS
   int stat;
   static int  channel;
   static struct {
      unsigned short status;
      unsigned short count;
      long info;
   } iosb;
   $DESCRIPTOR(name, "sys$command");

   if (*startup) {
      if (first) {
         stat = SYS$ASSIGN(&name, &channel, 0, 0);
         if (stat != SS$_NORMAL) {
            zvmessage("Error assigning a channel to your terminal:", "");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (0);
         }
         first = FALSE;
      }

      stat = SYS$QIOW(0, channel, IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
           &iosb, 0, 0, &inpchar, 1, 0, 0, 0, 0);
      if (stat != SS$_NORMAL || iosb.status != SS$_NORMAL) {
         zvmessage("Error reading terminal input:"," ");
         sprintf(errmsg, "     status = %d, iosb status = %d", stat, iosb.status);
         zvmessage(errmsg, " ");
         return (0);
      }
   }
   else {
      if (!first) {
         SYS$CANCEL(channel);
         SYS$DASSGN(channel);
         first = TRUE;
      }
      return ((int) '\0');
   }

#else

   ssize_t stat;
   static int terminal;

   if (startup) {
      if (first) {
         first = FALSE;
         terminal = open("/dev/tty", O_RDONLY, 0);
         if (terminal == -1) {
            zvmessage("Unable to obtain terminal file descriptor--using default instead."," ");
            terminal = 0;
         }
         settermmode(terminal, STDCHAR);
      }

      stat = read(terminal, &inpchar, sizeof(inpchar));
      if (stat <= 0) {
         zvmessage("Error reading terminal input:"," ");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, " ");
         perror(NULL);
         return (0);
      }
   }
   else {
      if (!first) {
         settermmode(terminal, RESET);
         close(terminal);
         first = TRUE;
      }
      return ((int) '\0');
   }
#endif

   /* deal with VAX/UNIX differences in <return> codes */
  
   if ((int)inpchar == 10 || (int)inpchar == 13)
      return (13);
   else
      return ((int) inpchar);
}



#if UNIX_OS
static int settermmode(terminal, mode)
int terminal, mode;
{
   static struct termios tbufsave;          /* termio to termios */
   struct termios tbuf;                     /* termio to termios */
   static int termset = FALSE;
//   char errmsg[81];

   if (mode != RESET) {
/*
      if (ioctl(terminal, TCGETA, &tbuf) == -1) {
         zvmessage("Error getting tty info:"," ");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }

    change for termios.h
*/
   if (tcgetattr(terminal, &tbufsave) != 0)
           perror("edibis_terminal: tcgetattr error");      

      tbufsave = tbuf;
      termset = TRUE;

      if (mode == RAW) {
         tbuf.c_iflag &= (short unsigned int)(~(INLCR | ICRNL | ISTRIP | IXON | BRKINT));
         tbuf.c_oflag &= (short unsigned int)(~OPOST);
         tbuf.c_lflag &= (short unsigned int)(~(ICANON | ISIG | ECHO));
      }
      else if (mode == STDCHAR) {
         tbuf.c_lflag &= (short unsigned int)(~(ICANON | ECHO));
      }
      else if (mode == CTRLCHAR) {
         tbuf.c_iflag &= (short unsigned int)(~BRKINT);
         tbuf.c_lflag &= (short unsigned int)(~(ICANON | ISIG | ECHO));
      }

      tbuf.c_cc[4] = sizeof(char);
      tbuf.c_cc[5] = 2;
/*      if (ioctl(terminal, TCSETAF, &tbuf) == -1) {
         zvmessage("Error setting terminal mode:"," ");
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
*/
      if (tcsetattr(terminal, TCSAFLUSH, &tbufsave) != 0)
           perror("edibis_terminal: tcsetattr error");
   }
   else {
      if (termset) {
/*         if (ioctl(terminal, TCSETAF, &tbufsave) == -1) {
            zvmessage("Error re-setting terminal:", " ");
            sprintf(errmsg, "     errno = %d; ", errno);
            zvmessage(errmsg, "");
            perror(NULL);
         }
*/     
      if (tcsetattr(terminal,  TCSAFLUSH, &tbufsave) != 0)
           perror("edibis_terminal: tcsetattr reset error");
         termset = FALSE;
      }
   }
return(termset);
}
#endif



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create edibis.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
C    Include file for EDIBIS 
	INTEGER	MAXFILECOLS
	PARAMETER (MAXFILECOLS = 1024)
	INTEGER	MAXBUFCOLS
	PARAMETER (MAXBUFCOLS = 40)
	INTEGER	MAXBUFROWS
	PARAMETER (MAXBUFROWS = 256)
	INTEGER	MAXROWS
	PARAMETER (MAXROWS = 18)
	INTEGER	UNIT,OUTUNIT,IBIS
	INTEGER	NCOL, CLEN
C	    Rows and cols refer to IBIS file,  X and Y to screen coords.
	INTEGER	CURROW, CURCOL		! Current cell position in file
	INTEGER	DISPROW, DISPCOL    	! File pos. of upper left cell on screen

	INTEGER	NUMDISPROWS, NUMDISPCOLS ! Number of cells displayed
	INTEGER	FIRSTBUFROW		! Row number of first row in buffer
	INTEGER	FIRSTBUFCOL		! Col number of first col in buffer
	INTEGER	FIRSTYPOS, LASTYPOS, SCREENXSIZE
	INTEGER	HIGHX, HIGHY, HIGHLEN	! Cell highlight info
	CHARACTER*80 HIGHSTRING
	INTEGER	COLXPOS(MAXFILECOLS), COLXLEN(MAXFILECOLS)  ! Column screen pos.

C	-- Format (buffer translation) codes:
	INTEGER	FMT_REAL,FMT_FULL,FMT_DOUB,FMT_COMP
	INTEGER FMT_A4,FMT_A16,FMT_A32,FMT_A64
	PARAMETER (FMT_REAL = 1)
	PARAMETER (FMT_FULL = 2)
	PARAMETER (FMT_DOUB = 3)
	PARAMETER (FMT_COMP = 4)
	PARAMETER (FMT_A4   = 5)
	PARAMETER (FMT_A16 = 6)
	PARAMETER (FMT_A32 = 7)
	PARAMETER (FMT_A64 = 8)
	INTEGER	COLFORMCODE(MAXFILECOLS)
	INTEGER	COLS(MAXFILECOLS)
	CHARACTER*32 COLFORMAT(MAXFILECOLS) 

C	    Buffer contains MAXBUFROWS for each column in file.
C		Only the columns on the screen are active.
C		We use 	EQUIVALENCE here only to save space!!
	CHARACTER *64	IBISBUF_A64(MAXBUFROWS,MAXBUFCOLS)
	CHARACTER *32	IBISBUF_A32(MAXBUFROWS*2,MAXBUFCOLS)
	CHARACTER *16	IBISBUF_A16(MAXBUFROWS*4,MAXBUFCOLS)
	CHARACTER *4	IBISBUF_A4(MAXBUFROWS*16,MAXBUFCOLS)
	REAL*8		IBISBUF_DOUB(MAXBUFROWS*8,MAXBUFCOLS)  
	COMPLEX*8	IBISBUF_COMP(MAXBUFROWS*8,MAXBUFCOLS)  
	REAL*4		IBISBUF(MAXBUFROWS*16,MAXBUFCOLS)  
	INTEGER*4	IBISBUF_FULL(MAXBUFROWS*16,MAXBUFCOLS)  
	EQUIVALENCE(IBISBUF,IBISBUF_FULL,IBISBUF_COMP,IBISBUF_DOUB)
	EQUIVALENCE(IBISBUF,IBISBUF_A64,IBISBUF_A32)
	EQUIVALENCE(IBISBUF,IBISBUF_A16,IBISBUF_A4)
C
C 	  Paste buffer (multi-format)
C
	CHARACTER *64	SAVE_A64
	CHARACTER *32	SAVE_A32
	CHARACTER *16	SAVE_A16
	CHARACTER *16	SAVE_A4
	REAL*8		SAVE_DOUB  
	COMPLEX*8	SAVE_COMP  
	INTEGER*4	SAVE_FULL  
	REAL*4	SAVECELLVALUE	! Recall cell value
	EQUIVALENCE(SAVECELLVALUE,SAVE_A64,SAVE_A32,SAVE_A16,SAVE_A4)
	EQUIVALENCE(SAVECELLVALUE,SAVE_DOUB,SAVE_COMP,SAVE_FULL)
	
	LOGICAL	COLCHANGED(MAXFILECOLS)  ! True if need to write blocks out
	LOGICAL BATCH_MODE,READONLY
	INTEGER CUR_CMD,NUM_CMD
	CHARACTER*200 FILENAME
	CHARACTER*200 OUTPUTSTRING
	CHARACTER*6 COLFILFMT(MAXFILECOLS) 
	CHARACTER*6 DEFLTFMT
	CHARACTER*6 FILEORG
	CHARACTER*32 FHOST
	CHARACTER*8 FILEVERSION
	LOGICAL	FORWARDMODE

	COMMON /IBISCOM/  IBISBUF_DOUB, SAVE_A64, UNIT,IBIS,OUTUNIT, 
     +                  CLEN, NCOL,CURROW, CURCOL, DISPROW, DISPCOL, 
     +			NUMDISPROWS, NUMDISPCOLS, FIRSTBUFROW, 
     +			FIRSTYPOS, LASTYPOS, SCREENXSIZE,
     +			HIGHX, HIGHY, HIGHLEN, HIGHSTRING,
     +			COLXPOS, COLXLEN, COLFORMAT, COLFORMCODE,
     +			COLCHANGED, FORWARDMODE, FILENAME, 
     +			COLFILFMT, DEFLTFMT, FILEORG, FILEVERSION,
     +			COLS,FHOST,FIRSTBUFCOL,OUTPUTSTRING,BATCH_MODE,
     +			CUR_CMD,NUM_CMD,READONLY

C		Editing command name constants
	INTEGER	NONE, TEXTSTRING, ENDEDIT
	INTEGER	UPARROW, DOWNARROW, LEFTARROW, RIGHTARROW
	INTEGER	PAGEUP, PAGEDOWN, PAGELEFT, PAGERIGHT,SETTEXTVAL
	INTEGER	FILETOP, FILEBOTTOM, GOTOROW, GOTOCOL,BADCOMMAND
	INTEGER	DELETEROW, INSERTROW, ZEROCELL, RECALLCELL
	INTEGER	CHNGFORMAT, REFRESH, HELP, QUIT, SEARCH, INITSEARCH
	PARAMETER (NONE=0, TEXTSTRING=1, ENDEDIT=2) 
	PARAMETER (UPARROW=3, DOWNARROW=4, LEFTARROW=5, RIGHTARROW=6)
	PARAMETER (PAGEUP=7, PAGEDOWN=8, PAGELEFT=9, PAGERIGHT=10)
	PARAMETER (FILETOP=11, FILEBOTTOM=12, GOTOROW=13, GOTOCOL=14)
	PARAMETER (DELETEROW=15, INSERTROW=16, ZEROCELL=17, RECALLCELL=18)
	PARAMETER (CHNGFORMAT=19, REFRESH=20, HELP=21, QUIT=22,SEARCH=23)
	PARAMETER (INITSEARCH=24,BADCOMMAND=25,SETTEXTVAL=26)
C
C	  Search Block
	LOGICAL INITSET
	CHARACTER*64	MINVAL_A64, MAXVAL_A64
	CHARACTER*32	MINVAL_A32, MAXVAL_A32
	CHARACTER*16	MINVAL_A16, MAXVAL_A16
	CHARACTER*16	MINVAL_A4,  MAXVAL_A4
	REAL*8		MINVAL_DOUB, MAXVAL_DOUB
	COMPLEX*8	MINVAL_COMP, MAXVAL_COMP
	REAL*4		MINVAL, MAXVAL
	INTEGER*4 MINVAL_FULL,MAXVAL_FULL
	EQUIVALENCE(MINVAL,MINVAL_FULL,MINVAL_DOUB,MINVAL_COMP)
	EQUIVALENCE(MINVAL,MINVAL_A64,MINVAL_A32,MINVAL_A16,MINVAL_A4)
	EQUIVALENCE(MAXVAL,MAXVAL_FULL,MAXVAL_DOUB,MAXVAL_COMP)
	EQUIVALENCE(MAXVAL,MAXVAL_A64,MAXVAL_A32,MAXVAL_A16,MAXVAL_A4)
	COMMON/SEARCHCOM/MINVAL_A64,MAXVAL_A64,INITSET

C    End of include file 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create edibis.imake
#define PROGRAM edibis

#define R2LIB

#define MODULE_LIST edibis.f edibis_windows.c edibis_terminal.c
#define INCLUDE_LIST edibis.fin
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_ANSI_C
#define FTN_STRING

#define LIB_CURSES

#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL

$!-----------------------------------------------------------------------------
$ create edibis.afmake
#==============================================================
# List of things to build

PROG_LIST = edibis

# Include makefile to get rules
include $(AFIDS_ROOT)/AfidsMakefile.in
#==============================================================
# Add include/carto to include line. This avoids needing to
# change existing code by adding "carto/" to the include
# line.
CPPFLAGS += -I$(AFIDSTOP)/include/carto -I.
#-Wno-globals - removes reports of 
#Argument #2 of `xvp' is one type at (2) but is some other type at (1) [info -f g77 M GLOBALS]
#Argument #2 of `xvwrit' is one precision at (2) but is some other precision at (1) [info -f g77 M GLOBALS]

FFLAGS += -Wunused -Wuninitialized -Wsurprising -Wno-globals

CFLAGS += -Wmissing-prototypes -Wstrict-prototypes
CFLAGS += -Wundef -Wconversion -Wmissing-declarations -Wsign-compare
#CFLAGS += -Wunreachable-code - gives bogus error messages in boxflt2c.c

#==============================================================
# Extra files hist depends on

edibis: edibis_windows.o edibis_terminal.o


$ Return
$!#############################################################################
$PDF_File:
$ create edibis.pdf
PROCESS HELP=*
 PARM INP      TYPE=STRING DEFAULT=""
 PARM OUT      TYPE=STRING DEFAULT=""
 PARM NCOL     TYPE=INTEGER VALID=(1:1024) DEFAULT=1
 PARM LCOL     TYPE=INTEGER DEFAULT=1
 PARM COLS     TYPE=INTEGER COUNT=1:40 VALID=0:40  DEFAULT=0
 PARM FORMAT   TYPE=(STRING,32) DEFAULT="(F11.3)"
 PARM SCRNSIZE TYPE=INTEGER  VALID=(16:132) DEFAULT=80
 PARM VERSION  TYPE=KEYWORD VALID=(IBIS-1,IBIS-2) DEFAULT="IBIS-1"
 PARM ORG      TYPE=KEYWORD VALID=(ROW,COLUMN) DEFAULT=COLUMN
 PARM HOST     TYPE=(STRING,32) DEFAULT="NATIVE"
 PARM CFORMAT   TYPE=(STRING,6) COUNT=1:40 DEFAULT=" "
 PARM GR1DIM   TYPE=INTEGER VALID=(1:40) DEFAULT=2
 PARM DEFFMT   TYPE=(STRING,6) DEFAULT=" " 
 PARM COMMANDS TYPE=(STRING,200) COUNT=1:50 DEFAULT=" "
 PARM SCRATCH  TYPE=STRING DEFAULT="EDIBIS.TMP"
 PARM MODE     TYPE=KEYWORD VALID=(WRITE,READONLY) DEFAULT=WRITE
END-PROC
.TITLE
VICAR/IBIS Program EDIBIS
.HELP
PURPOSE

    EDIBIS is an interactive screen editor for editing IBIS 
tabular or graphics files on a VT100 compatible terminal.  The command 
entry is similar to keypad mode in the EDT editor. Support also
exists for performing commands in batch-mode, without a terminal.
Note: If your terminal does not generate vt100 keypad codes, you can
use the batch-mode commands on the EDIBIS command-line instead.


    
EXECUTION

   EDIBIS  INPUT.INT OUTPUT.INT  <PARAMS>

.PAGE
EXAMPLES

   EDIBIS  IBIS.INT
This example shows how to edit an existing IBIS tabular file.

   EDIBIS  IBIS.GR   GR1DIM=3
This example shows how to edit an existing 3D IBIS graphics-1 file.

   EDIBIS  INPUT.INT OUTPUT.INT  COLS=(1,3,4) LCOL=200
This copies the desired columns in the input file to the output, increases
the output column length to 200, and edits the output.

.page
EXECUTION

   EDIBIS  OUT=NEWFILE.INT  NCOL=10 LCOL=100
This creates a tabular file with 10 columns of 100 rows each.

   EDIBIS IBIS.INT  SCRNSIZE=132
To edit in 132 column mode on the terminal, first put the terminal in
132 column mode with a SET TERM/WIDTH=132, and then specify the SCRNSIZE
parameter.

   EDIBIS IBIS.INT  FORMAT="(F10.4)"
This sets the initial format for all of the columns to F10.4 .
(This will only work for IBIS-1 files. IBIS-2 files are formatted).
.page
EXAMPLES

   EDIBIS a b cols=(1,2,4) command=("(1,3) search 4.567","delete",exit)
    top,paste,exit)

This puts EDIBIS into batch mode, first doing a search through
column 3 for 4.567, and deleting the found column.

   EDIBIS  old command=("(1,2) format (a4)","search not", +
   cut,"(3,0) paste",exit)

This set of batch commands first changes the format of 
column 2 to "(A4)", and then searched for the string "not",
and if found, clears the cell and pastes the value into row 3.
.PAGE
OPERATION

Interactive Mode:

    The EDIBIS tabular file editor allows one to roam through out the
file looking at and changing any of the data values.  The format for
each column may be specified so that real numbers, integers, and character
data may be displayed and entered.  The numeric keypad is used for the
command entry, which is patterned after the EDT keypad mode commands.
The screen serves as a window into the tabular file.  Eighteen rows
and as many columns as fit are displayed.  The current cell is
highlighted and header information is display at all times.  The header
information consists of the file name and size, the current row and column,
and the format of all of the columns being displayed.  Data and command 
information is entered at the lower left corner of the screen.
.PAGE
OPERATION

Batch Mode:

Batch Mode is invoked when the "COMMANDS" parameter is used.
In this case interactive mode is turned off and the file is manipulated
using the commands specified by the parameter. See the COMMANDS
parameter for more information. A description of each command is
given below.

The batch commands may also be used interactively by placing the
command on the EDIBIS command-line, prefaced by a "/" character, e.g.

    ]  /(1,2) set 5.1


FAILURE MODE IN BATCH OPERATIONS

NOTE: In older versions no ABEND calls were made upon detecting errors
The basic assumption was that you were at an interactive terminal
and you could reenter bad commands. This created problems in Batch
mode in long scripts if a bad command was entered.

This particularly could be a problem  when _onfail was set to
STOP. The abend signal was never sent so the script continued. If
the proc continued for minutes to hours you may never notice that
the command had failed.

After 14-Aug-2011 this version will issue an abend for BATCH mode
operations, i.e., when COMMANDS=("some command") parameter is provided.
 
.PAGE
COMMANDS

SETTING CELL VALUES:

Batch command: "(row,col) SET <value>", where the (row,col) is optional.

    The value of the current cell is changed by typing the desired new value
and pressing the Return key.  If the entered value is illegal then the
cell value will not be changed.  There are a few cases where this is
the *only* way to set a value. For example, if column 2 is ASCII, 
and you want to set the value of cell (1,2) to "quit", you can't just type
"quit", because that is the command to quit EDIBIS without saving the
file. So, to set the cell value, give the command:

    ] /(1,2) set quit

To set a string value containing spaces, put it in quotes "like this".
.PAGE
COMMANDS

MOVING AROUND

Batch Commands: "TOP" "BOTTOM", "LEFT","RIGHT".

    The arrow keys move the current cell one place in the appropriate 
direction.  If the cell is at the edge of the screen, scrolling is performed.
As with the EDT editor there is a forward mode and a backward mode which 
are selected by (keypad 4) and (keypad 5), respectively. Using the GOLD KEY
(PF1 key) with these keys advances the file to the bottom row or top row,
respectively.

    Page up and page down (keypad 8) and page left and page right (keypad 2)
move the screen one window size unless the edges of the file are encountered.
The direction forward/backward mode is determined by the previously mentioned
keypad commands.
.PAGE
COMMANDS

MOVING AROUND (cont'd)

Batch command: "Row N", "Column M" and/or "(N,M) JUMP"

    The current row may be set to a particular row by typing an integer and
touching the (keypad enter) key. The current column may be set by typing
an integer and using GOLD KEY and (kepad enter). 

.PAGE
COMMANDS

DELETING/INSERTING ROWS:

 Batch Commands: "(row,col) DELETE N" 
             and "(row,col) INSERT N". The (row,col) is optional.

    Any number of rows may be deleted or inserted at the current position
by typing the number of rows and pressing (keypad PF4) or (keypad PF1,
keypad PF4) respectively.  If no number is specified then one row is 
deleted or inserted.  The inserted rows are inserted after the current 
line and are filled with zeros.

Warning: the whole file must be rewritten for these two commands so 
they can take a while, especially if the file is large.
.page
COMMANDS

CUT/PASTING Values:

Batch commands: "(row,col) Cut" and "(row,col) PASTE".

    The (keypad -) key zeros (or blanks, if character format) the current
cell.  The (keypad PF1, keypad -) combination changes the current cell by 
recalling the value of the last zeroed cell.

.PAGE
COMMANDS

COLUMN FORMATTING (IBIS-1 only)

Batch command: "(row,col) FORMAT (formt)" where the (row,col) is optional.

    The display format of the current column is automatically determined
for IBIS-2 format files, and is set to floating point for IBIS-1 and
GRAPHICS-1 files. The format may be changed by typing the
new format and then typing ^F.  The format string must be a legal
Fortran format statement beginning and ending with parentheses.  If
blank spacing is specified (e.g. 2X) it must precede the data part or
it will be ignored.  The format string itself must be less than or equal 
to 16 characters, and the length of the display it specifies must be less 
than or equal to 32 characters. 
.PAGE
COMMANDS

SCREEN-REFRESH/HELP

    The screen can be refreshed (i.e. redisplayed) with the ^R key.

    The (keypad PF2) key will display a brief command summary.  Hit 
<return> for batch-command help, and any other key (e.g. spacebar) to
go back to editing.

.PAGE
COMMANDS

SEARCHING FOR VALUES

Batch Command: "(row,col) SEARCH <string>", where (row,col) is optional.

   The PF1 PF3 combination will initialize the EDIBIS search mode.  It will
prompt you with:

	ENTER RANGE]

and will take the command string to be a range of values to search for
within the current column marked by the cursor.
.PAGE
COMMANDS

SEARCHING (ctd')

Batch Command: "FWD" and "BACKWARD"

 The direction of the search is determined by the keypad FWD/BACK mode,
and valid syntax for search strings is as follows:

	ENTER RANGE] VALUE	Find first exact match to VALUE
	ENTER RANGE] MIN:MAX	Find first value not outside interval
	ENTER RANGE] MIN:	Find first value not smaller than MIN
	ENTER RANGE] :MAX	Find first value not larger than MAX

If the column is in CHARACTER format, a string match is performed.
.PAGE
ENDING AN EDITING SESSION

Batch Command:  "EXIT" will save the file and quit.

In interactive mode "EXIT" will save and quit, and "QUIT"
will exit without saving the file. If the file was opened
read-only no saving is done at all.

VMS SHORT-CUTS:

    The editing session is ended, the file saved, and the program ended
with the ^Z key.    Hitting ^C will abort and leave a temp file called 
'EDIBIS.TMP' -- the scroll will be disabled ,though.  The session may be
gracefully aborted with the ^D key or by typing "quit"; no changes or
temp files.
.PAGE

RESTRICTIONS

The maximum number of columns allowed for IBIS-1 tabular files
and GRAPHICS-1 files is 40, and for IBIS-2 files the limit is 1024.
There is no limit on the column length.
The each cell may occupy no more than 64 characters on the display.

Original Programmer:   Frank Evans	January 1987

Cognizant Programmer:  Ray Bambery 

Revision       8		02 September 2013

       REVISION HISTORY
            
               Rev 8           RJB             02 September 2013
               --- Changed mabend call to xvmessage + abend. Had
                    some kind of buffer overrun on output with mabend
               Rev 7           RJB             30 August 2013
               --- Changed a variable that failed in AFIDS
                   autotools make environment (VERSION to VERS) 

               Rev 6           RJB             08 December 2012
               --- Fixes edibis_windows.c and dibis_terminal.c
                   For POSIX compliance with RedHat Linus 6.2 and 
                   MacOSX 10.7.4

               Rev 5           RJB             14 August 2011
               --- Fixes for gcc 4.4.4 under Linux
                   In SUBROUTINE CONVERT_STRING_TO_COMMAND
                   had to add new call CALL FIND_ROWCOL
                   THISROW and THISCOL are found from list directed read
                   in gcc 4.4.4:
                   READ (CMDLINE(CMDPOS:),*,ERR=332) THISCOL i
                   does not detect properly the trailing ")" as a terminator
                   It gives an error, where the older compilers evidently 
                   recognized ")" as a proper terminator

               REV 4           NDR             17 November 1994
               --- Changed code to use direct IBIS2 routines
                   for forward compatibility. Converted all
                   FORTRAN I/O to C for portable coding.
                   Added scripting and command-line features.
                   and portable "curses" library windowing.

               REV 3           NDR             4 February 1991
               --- FIXED BUG CAUSING LOCKOUT WHEN "NEXT SCRN"
                   KEYS HIT ON WYSE TERMINALS (FAILED IN INPUTKEY).
                   Ref:   FR #66574 by C. Avis

               REV 2           NDR             7 October 1987
               --- ADDED SEARCH AND QUIT COMMANDS


.LEVEL1
.VARIABLE INP
The input IBIS tabular file.
.VARIABLE OUT
The output IBIS tabular file.
(optional).
.VARIABLE NCOL
The number of columns.
Only if output file.
.VARIABLE LCOL
The column length.
Only if output file.
.VARIABLE COLS
The columns in the input file
to copy to output file.
Default is all columns.
.VARIABLE FORMAT
The initial column format.
Fortran format statement.
.VARIABLE SCRNSIZE
The number of columns on
the screen to use.
.VARIABLE VERSION
Create IBIS-1 or IBIS-2 file?
.VARIABLE ORG
File organized by ROW/COLUMN?
.VARIABLE HOST
Host Data format(IBIS-2 only)
.VARIABLE CFORMAT
Column Formats (IBIS-2 only)
.VARIABLE GR1DIM
GRAPHICS-1 file dimension
.VARIABLE DEFFMT
Default column data format
.VARIABLE COMMANDS
Commands for Batch-processing
.VARIABLE SCRATCH
Name of working scratch file
.VARIABLE MODE
Open file read-only?
.LEVEL2
.VARIABLE INP
The input IBIS tabular file.  If an output file is specified then the
input will be copied to the output, and the output editted.  Otherwise
the input will be editting in place (no backup!).  If the input is
not specified then the output must be.
.VARIABLE OUT
The optional output IBIS tabular file.  If an input file is specified
then the input will be copied to the output, and the output editted.  
Otherwise NCOL and LCOL should be specified and a new tabular file will
be created.
.VARIABLE NCOL
The number of columns in the output file.  If COLS is specified then
NCOL is optional.
.VARIABLE LCOL
The column length in the output file.  If there is an input file then
the column length will be the maximum of LCOL and the input length.
.VARIABLE COLS
The columns in the input file to copy to output file.  If COLS is not
specified then all of the columns in the input will be copied to the
output.
.VARIABLE FORMAT
The initial column format for all of the columns, specified as a Fortran 
format statement, e.g. "(1X,F10.4)".  The format for individual columns
may be changed during the edit session.
.VARIABLE SCRNSIZE
The number of columns on the screen to use.  Default is 80.
.VARIABLE VERSION
This keyword indicates whether to create an IBIS-1 or IBIS-2
format file.
.VARIABLE ORG
This keyword indicates whether a new file is to be organized by
contiguous ROW data or contigous COLUMN data. For IBIS-1 format
files, 'ROW is synonymous with creating a GRAPHICS-1 file, and
'COLUMN is synonymous with a TABULAR file. For IBIS-2 format,
these file organizations are somewhat arbitrary, and may be
chosen based on the needs/use of the file itself.
.VARIABLE HOST
This keyword indicates which host platform the data should be
formatted. For IBIS-1 files this keyword should not be specified,
as ALL known IBIS-1 tabular and graphics files use VAX-VMS hosts.
The default is to create a NATIVE format file, which is the
suggested format.
.VARIABLE CFORMAT
This parameter allows the specification of the data format of
each column of a new file. This parameter is only valid for
IBIS-2 files, as IBIS-1 files do not contain any formatting
information.
.VARIABLE GR1DIM
GRAPHICS-1 files do not contain their dimension information
in the file. The GR1DIM parameter allows the specification of that
dimension, when editing a pre-existing file. This parameter
is required for GRAPHICS-1, as there is no natural default
dimension. For all other file formats, this parameter is ignored.
.VARIABLE DEFFMT
This keyword allows the specification of the default column
format of a new IBIS-2 file. If the COLFMT parameter is not
specified, then all of the columns created in this file will
be of this format. This parameter is not valid for IBIS-1
files.
.VARIABLE SCRATCH
To allow this program to non-destructively quit, all editing
is done upon a temporary work file. If the current directory
is read-only, you may use this parameter to change the location
and name of the scratch file. This may also be necessary if the
current device does not have enough space.

.VARIABLE COMMANDS
Commands for Batch-processing & testing. Supported commands
(which may be abbreviated down to 3 characters) are:

    ROW N              --go to row N of current column
    COLUMN N           --go to column N of current row
    FORMAT string      --set current column FORTRAN format to <string>
                         (IBIS-1 only)
    DELETE N           --Delete N rows starting with curent row
    INSERT N           --Insert N rows above current row
    CUT                --Clear current cell and store value
    PASTE              --Paste stored value into current cell
    SET string         --Set current cell to value <string>
    EXIT               --Save and quit.
    TOP                --Go to top of current column
    BOTTOM             --Go to bottom of current column
    SEARCH string      --Search Down current column for string,
                         if specified, or previous search value if not.

each of these commands may be preceded by a (row,column) location,
such as "(3,4) set 2", which would mean "set the value of the cell
in row 3, column 4, to the value 2. Passing in a 0 for one of the
values means "use the current value"; for example, "(3,0) X" means
to go to row 3 of the current column and do X.


.VARIABLE MODE
Allows opening files read-only, in case they are in
protected directories or are not to be modified.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstedibis.pdf
procedure
refgbl $echo
! Aug 30, 2013 - RJB
! TEST SCRIPT FOR EDIBIS
! tests IBIS tabular files
!
! Vicar Programs:
!       ibis-gen ibis-list
! 
! External Programs;
!   <none>
!
! Parameters:
!   <none>
!
! Requires NO external test data: 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

body
 let _onfail="stop"
 let $echo="no"
 write "****************************************************"
 write "EDIBIS is primarily an interactive program requiring"
 write "a vt-100 type terminal interface with a keypad for"
 write "EDT style editing commands. A full test of EDIBIS"
 write "requires testing interactive behaviour and display"
 write "***************************************************"
 write " " 
 write "BATCH-ONLY tests:"
 write " "
 let $echo="yes"
 ibis-gen a nc=5 nr=6 index=1 format=(real,real,a8,real,real)

!  TEST 1 - Test general ops, and make sure that "junk" is skipped:
! --Added test for correct A8 text handline, FR#85876
!
 edibis a command=("(2,2) set 3.103","(2,4) SET 4.567","row 3",+
 "Insert 3","(1,3) set woof",EXIT)
 ibis-list a


! TEST 2 - cut 'n paste
 edibis a b cols=(1,2,4) command=("(1,3) search 4.567","delete",exit)
 ibis-list b
 edibis a b cols=(1,2,4) command=("(2,3) cut",paste,bottom,paste, +
    top,paste,exit)
 ibis-list b

! TEST 3 - Test Formatting for old IBIS and searches:
 ibis-gen old nc=3 nr=100 'ibis-1 'column format=(FULL,A4,REAL) +
   strcol=2 string=(this,does," ",have,a,blank,space,not) index=1
 ibis-list old a4col=2 intcol=1 nr=8
 edibis old command=("(1,2) format (a4)","search not", +
   cut,"(3,0) paste",exit)
 ibis-list old a4col=2 intcol=1 nr=8

let _onfail="continue"
let $echo="no"
write "********************"
write "command should ABEND"
write "********************"
let $echo="yes"
! TEST 4 - test abend - previously it crashed

edibis a command=("junk")
let $echo="no"
end-proc

$!-----------------------------------------------------------------------------
$ create tstedibis.log
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

****************************************************
EDIBIS is primarily an interactive program requiring
a vt-100 type terminal interface with a keypad for
EDT style editing commands. A full test of EDIBIS
requires testing interactive behaviour and display
***************************************************
 
BATCH-ONLY tests:
 
 ibis-gen a nc=5 nr=6 index=1 format=(real,real,a8,real,real)
Beginning VICAR task ibis
 edibis a command=("(2,2) set 3.103","(2,4) SET 4.567","row 3", +
 "Insert 3","(1,3) set woof",EXIT)
Beginning VICAR task edibis
edibis - version Jan 29, 2015 - WLB
 ibis-list a
Beginning VICAR task ibis
 
Number of Rows:9  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:9
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        0.00        woof        0.00        0.00
        2.00        3.10                    4.57        0.00
        0.00        0.00                    0.00        0.00
        0.00        0.00                    0.00        0.00
        0.00        0.00                    0.00        0.00
        3.00        0.00                    0.00        0.00
        4.00        0.00                    0.00        0.00
        5.00        0.00                    0.00        0.00
        6.00        0.00                    0.00        0.00
 edibis a b cols=(1,2,4) command=("(1,3) search 4.567","delete",exit)
Beginning VICAR task edibis
edibis - version Jan 29, 2015 - WLB
 ibis-list b
Beginning VICAR task ibis
 
Number of Rows:8  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:8
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        3.00        0.00        0.00
        4.00        0.00        0.00
        5.00        0.00        0.00
        6.00        0.00        0.00
 edibis a b cols=(1,2,4) command=("(2,3) cut",paste,bottom,paste,  +
    top,paste,exit)
Beginning VICAR task edibis
edibis - version Jan 29, 2015 - WLB
 ibis-list b
Beginning VICAR task ibis
 
Number of Rows:9  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:9
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        0.00        4.57
        2.00        3.10        4.57
        0.00        0.00        0.00
        0.00        0.00        0.00
        0.00        0.00        0.00
        3.00        0.00        0.00
        4.00        0.00        0.00
        5.00        0.00        0.00
        6.00        0.00        4.57
 ibis-gen old nc=3 nr=100 'ibis-1 'column format=(FULL,A4,REAL)  +
   strcol=2 string=(this,does," ",have,a,blank,space,not) index=1
Beginning VICAR task ibis
 ibis-list old a4col=2 intcol=1 nr=8
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 3       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:8
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
           1        this        0.00
           2        does        0.00
           3                    0.00
           4        have        0.00
           5        a           0.00
           6        blan        0.00
           7        spac        0.00
           8        not         0.00
 edibis old command=("(1,2) format (a4)","search not",  +
   cut,"(3,0) paste",exit)
Beginning VICAR task edibis
edibis - version Jan 29, 2015 - WLB
 ibis-list old a4col=2 intcol=1 nr=8
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 3       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:8
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
           1        this        0.00
   538976258        does        0.00
           3                    0.00
   538976260        have        0.00
           5        a           0.00
   538976262        blan        0.00
           7        spac        0.00
   538976264                    0.00
let _onfail="continue"
let $echo="no"
********************
command should ABEND
********************
edibis a command=("junk")
Beginning VICAR task edibis
edibis - version Jan 29, 2015 - WLB
Unknown Command:junk
??E - BATCH_MODE abend
 ** ABEND called **
continue
let $echo="no"
$ Return
$!#############################################################################
