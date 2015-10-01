$!****************************************************************************
$!
$! Build proc for MIPL module edimage
$! VPACK Version 1.9, Friday, March 26, 2010, 16:34:13
$!
$! Execute by entering:		$ @edimage
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
$ write sys$output "*** module edimage ***"
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
$ write sys$output "Invalid argument given to edimage.com file -- ", primary
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
$   if F$SEARCH("edimage.imake") .nes. ""
$   then
$      vimake edimage
$      purge edimage.bld
$   else
$      if F$SEARCH("edimage.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake edimage
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @edimage.bld "STD"
$   else
$      @edimage.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create edimage.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack edimage.com -mixed -
	-s edimage.f edimage.fin -
	-i edimage.imake -
	-p edimage.pdf -
	-t tstedimage.pdf tstedimage1.scr tstedimage.scr
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create edimage.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	VICAR Program:  "edimage"  --  Interactive Image Editing Program
C
C	Original Programmer:  Frank Evans	May 1987
C
C	Revision:	new
C
C       2-88    SP  ADDED SETLUT=1 FOR NEW VRDI.
C       2-88    SP  ADDED CHANGES FROM NILES RITTER FOR SCREEN PARAMETER
C                   AND SIZE OPTION FOR A BOX ON POLYGON COMMAND.
C       2-88    SP  ADDED TEST SCRIPT, TEST PARAMETER, AND TEST MODE.
C       2-88    SP  CORRECTED STRETCH TO WORK FOR GREEN AND BLUE.
C       3-88    SP  ADDED CORRECTIONS FROM JOHN REIMER, INCLUDING CHANGE
C                   TO A SOLID CURSOR: CURSFORM=2.
C       3-90    SP  CORRECTED ERROR HANDLING FOR VRDI ROUTINES BY CHANGING
C                   DECLARATIONS OF XD ROUTINES AND IERR TO INTEGER.
C       3-95    AS  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
c       3-10   lwk  Renamed subroutine GET_COMMAND to GET_COMD to avoid name
c		    conflict with new Linux OS.

	IMPLICIT NONE
	INCLUDE 'edimage.fin'
C	Include file for EDIMAGE

	LOGICAL	EXIT
	CHARACTER*128  CMD_STRING
	CHARACTER*8    CMD_NAME

C ------------------------------------------------------------
C	Start of Executable Code
C ------------------------------------------------------------

	CALL IFMESSAGE('EDIMAGE version 6-MAR-95')
	CALL XVMESSAGE(' ',' ')
        CALL XVEACTION('SA',' ')

	CALL FILE_SETUP
	CALL DISPLAY_SETUP
	CALL INITIALIZE

	EXIT = .FALSE.
	DO WHILE (.NOT. EXIT)
	    CALL GET_COMD (CMD_STRING)
	    CALL PARSE_COMMAND (CMD_STRING, CMD_NAME)
	    CALL COMMAND_DISPATCH (CMD_NAME, EXIT)
	ENDDO

	CALL FILE_OUTPUT
	CALL DISPLAY_OFF

	RETURN
	END



C *************************************************************
C                 SUBROUTINES
C *************************************************************


	SUBROUTINE COMMAND_DISPATCH (CMD_NAME, EXIT)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	CHARACTER*8 CMD_NAME
	LOGICAL	EXIT

        IF (CMD_NAME(1:4).EQ.'    '.OR.CMD_NAME(1:7).EQ.'COMMENT') THEN
	   RETURN
        ELSE IF (CMD_NAME(1:5) .EQ. 'PAINT') THEN
	   CALL DO_PAINT
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'DRAW') THEN
	   CALL DO_DRAW
	   RETURN
        ELSE IF (CMD_NAME(1:7) .EQ. 'TEXTURE') THEN
	   CALL DO_TEXTURE
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'TEXT') THEN
	   CALL DO_TEXT
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'DISP') THEN
	   CALL DO_DISPLAY
	   RETURN
        ELSE IF (CMD_NAME(1:7) .EQ. 'STRETCH') THEN
	   CALL DO_STRETCH
	   RETURN
        ELSE IF (CMD_NAME(1:3) .EQ. 'PAN') THEN
	   CALL DO_PAN
	   RETURN
        ELSE IF (CMD_NAME(1:5) .EQ. 'HZOOM') THEN
	   CALL DO_HZOOM
	   RETURN
        ELSE IF (CMD_NAME(1:5) .EQ. 'COLOR') THEN
	   CALL DO_COLOR
	   RETURN
        ELSE IF (CMD_NAME(1:7) .EQ. 'PALETTE') THEN
	   CALL DO_PALETTE
	   RETURN
        ELSE IF (CMD_NAME(1:5) .EQ. 'BRUSH') THEN
	   CALL DO_BRUSH
	   RETURN
        ELSE IF (CMD_NAME(1:6) .EQ. 'PSEUDO') THEN
	   CALL DO_PSEUDO
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'POLY') THEN
	   CALL DO_POLYGON
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'COPY') THEN
	   CALL DO_COPY
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'FILL') THEN
	   CALL DO_FILL
	   RETURN
        ELSE IF (CMD_NAME(1:7) .EQ. 'STATIST') THEN
	   CALL DO_STATISTICS
	   RETURN
        ELSE IF (CMD_NAME(1:5) .EQ. 'TABLE') THEN
	   CALL DO_TABLE
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'HELP') THEN
	   CALL DO_HELP
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'EXIT') THEN
	   EXIT = .TRUE.
	   RETURN
        ELSE IF (CMD_NAME(1:4) .EQ. 'QUIT') THEN
	   EXIT = .TRUE.
	   CHANGEDMEM = .FALSE.
	   RETURN
        ELSE
	   PRINT *, 'Unknown command'
        END IF
	RETURN
	END


	SUBROUTINE DO_PAINT
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER   IMP, CLINE, CSAMP, LINE, SAMP
	INTEGER   I, J, LOFF, SOFF, IOFF, JOFF, BRUSHNL, BRUSHNS
	INTEGER   FLAG, COL, DN
	REAL	DENSITY, D
	BYTE    BUFFER(2*MAXBRUSHSIZE)
C	EQUIVALENCE (DN,DNB)
	INTEGER	XDCLOCATION, XDILINEREAD, XDILINEWRITE, IERR
	LOGICAL SWITCHED, SWITCHDOWN


	CALL RESETSWITCHES
	CALL GET_KEYWORD_REAL ('PAINT', 1, PAINTDENS, FLAG, PAINTDENS)
	PAINTDENS = MIN (MAX (PAINTDENS, 0.0), 1.0)
	DENSITY = PAINTDENS/100.0

	PRINT *, 'Hold down switch 1 to paint.'
	PRINT *, 'Hit switch 2 to get new color.  Hit switch 3 to end.'
	DO WHILE (.NOT. SWITCHED(3))   ! Loop until switch 3 is depressed

C			If switch 1 is depressed then paint where the cursor is
	    IF (SWITCHDOWN(1)) THEN
C					Get cursor location
		IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
		IF ( IERR .ne. 1 )  RETURN
		LINE = (CLINE-1)/HZOOMFAC + SLMEM 
		SAMP = (CSAMP-1)/HZOOMFAC + SSMEM 
C				Get brush square location - deal with sides
		LOFF = LINE - MAXBRUSHSIZE/2 + 1
		IOFF = 1
		IF (LOFF .LT. 1)  IOFF = 2 - LOFF
		BRUSHNL = MIN (MAXBRUSHSIZE, NLMEM-LOFF+1)
		SOFF = SAMP - MAXBRUSHSIZE/2 + 1
		JOFF = 1
		IF (SOFF .LT. 1) THEN
		    JOFF = 2 - SOFF
		    SOFF = 1
		ENDIF
		BRUSHNS = MIN (MAXBRUSHSIZE, NSMEM-SOFF+1)
C				Where the brush pattern is on, average the
C				    old DN with the current color to get
C				    the new DN.
		DO I = IOFF, BRUSHNL
		  IF (BRUSHLON(I)) THEN
		    LINE = LOFF + I - 1
		    DO IMP = 1, NUMIMAGES
		      COL = CURCOLOR(IMP)
		      IERR = XDILINEREAD (DISPU, IMP, SOFF, LINE,
     +				BRUSHNS, BUFFER(JOFF))
		      IF ( IERR .ne. 1 )  RETURN
		      DO J = JOFF, BRUSHNS
			IF (BRUSHPAT(I,J) .GT. 0) THEN
			  D = DENSITY*BRUSHPAT(I,J)
C			  DNB(1) = BUFFER(J)
C			  DNB(2) = 0
			  DN = NINT(D*COL + (1.-D)*BYTE2INT(BUFFER(J)))
			  BUFFER(J) = INT2BYTE(DN)
			ENDIF
		      ENDDO
		      IERR = XDILINEWRITE (DISPU, IMP, SOFF, LINE,
     +				BRUSHNS, BUFFER(JOFF))
		      IF ( IERR .ne. 1 )  RETURN
		    ENDDO
		  ENDIF
		ENDDO
	    ENDIF

C			If switch 2 on then change current color 
C			    to color under cursor
	    IF (SWITCHED(2)) THEN
		CALL CURSORCOLOR (CURCOLOR)
	    ENDIF

	    CALL VWAIT(3)
	ENDDO

	CHANGEDMEM = .TRUE.

	RETURN
	END




	SUBROUTINE DO_DRAW
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	LINE1, SAMP1, LINE2, SAMP2
	INTEGER CSAMP, CLINE
	INTEGER	LASTSWITCH, SWITCH, FLAG, GRNUM, WIDTH, W2, W3
	INTEGER	GRAPHFLAG, GRDN
	REAL	L1, S1, L2, S2, L3, S3, L4, S4
	INTEGER	IERR, XDCLOCATION
	INTEGER	WHERECURSOR, RDGR, NEXTGR, GETGR, CLGR
        LOGICAL DRAW
	LOGICAL	EOF, EOL, keytest, NEWPOS, SWITCHED, PENDOWN, INSTRING
	INTEGER OCLINE, OCSAMP, STATUS
	COMMON /CURS/ OCLINE, OCSAMP


	CALL RESETSWITCHES
	CALL GET_KEYWORD_INTEGER ('WIDTH', 1, 1, FLAG, WIDTH)


C		Interactive line drawing
	IF (keytest('LINE')) THEN
	    PRINT *,'Press switch 1 for each vertex.'
	    PRINT *,'Press switch 2 to get color under cursor.'
	    PRINT *,
     +         'Hit switch 3 to end linestring, again to end draw mode.'
	    LASTSWITCH = 1
	    DO WHILE (LASTSWITCH .NE. 3)
		INSTRING = .FALSE.
50		CONTINUE
		    LASTSWITCH = SWITCH
		    CALL SWITCHWAIT (0, SWITCH)
		    IF (SWITCH .EQ. 2) THEN
			CALL CURSORCOLOR (CURCOLOR)
		    ELSE IF (SWITCH .EQ. 1) THEN
			IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
			IF ( IERR .ne. 1 )  RETURN
			LINE2 = (CLINE-1)/HZOOMFAC + SLMEM 
			SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM 	
			IF (INSTRING) THEN
			    CALL DRAWLINE (LINE1, SAMP1, LINE2, SAMP2, 
     +					CURCOLOR, WIDTH)
			ENDIF
			INSTRING = .TRUE.
			LINE1 = LINE2
			SAMP1 = SAMP2
		    ENDIF
		IF (SWITCH .NE. 3) GOTO 50  ! Loop until switch 3 is depressed
	    ENDDO


	ELSE IF (keytest('BOX')) THEN

	    PRINT *,'Select first corner with switch 1'
	    CALL SWITCHWAIT (1, SWITCH)
	    IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE) ! Get cursor location
	    IF ( IERR .ne. 1 )  RETURN
	    LINE1 = (CLINE-1)/HZOOMFAC + SLMEM	! Convert to image mem coords
	    SAMP1 = (CSAMP-1)/HZOOMFAC + SSMEM

	    PRINT *,'Select second corner with switch 1'
	    CALL SWITCHWAIT (1, SWITCH)
	    IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE) ! Get cursor location
	    IF ( IERR .ne. 1 )  RETURN
	    LINE2 = (CLINE-1)/HZOOMFAC + SLMEM	! Convert to image mem coords
	    SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM

	    CALL ORDER (LINE1, LINE2)
	    CALL ORDER (SAMP1, SAMP2)
	    W2 = (WIDTH-1)/2
	    W3 = WIDTH/2
	    CALL DRAWLINE (LINE1, SAMP1-W2, LINE1, SAMP2+W2, CURCOLOR,
     *                     WIDTH)
	    CALL DRAWLINE (LINE1-W2, SAMP2, LINE2+W3, SAMP2, CURCOLOR,
     *                     WIDTH)
	    CALL DRAWLINE (LINE2, SAMP2+W3, LINE2, SAMP1-W2, CURCOLOR,
     *                     WIDTH)
	    CALL DRAWLINE (LINE2+W2, SAMP1, LINE1-W2, SAMP1, CURCOLOR,
     *                     WIDTH)


	ELSE IF (keytest('FILE')) THEN
C			Read an IBIS graphics file

	    CALL GET_KEYWORD_INTEGER ('FILE', 1, NUMIMAGES+1,FLAG,GRNUM)
	    IF (GRNUM .LE. NUMIMAGES .OR. GRNUM .GT. NUMINPFILES) THEN
		PRINT *,'Incorrect input file for graphics'
		RETURN
	    ENDIF
	    CALL GET_KEYWORD_INTEGER ('GRAPHICS', 1, 1, GRAPHFLAG, GRDN)
	    GRDN = MIN (MAX (GRDN, 0), 7)

	    STATUS = RDGR (GRNUM, 1, 2)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		STATUS = NEXTGR (1, EOF, L1, S1, 0)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
		IF (EOF) GOTO 99
		L1 = L1 - SLFILE + 1
		S1 = S1 - SSFILE + 1
		EOL = .FALSE.
		DO WHILE (.NOT. EOL)
		    STATUS = GETGR (1, EOL, EOF, L2, S2, 0)
                    IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
		    IF (EOF) GOTO 99
		    IF (.NOT. EOL) THEN
			L2 = L2 - SLFILE + 1
			S2 = S2 - SSFILE + 1
			CALL CLIPPER (L1, S1, L2, S2,
     +				      L3, S3, L4, S4, DRAW)
			IF (DRAW) THEN
			    IF (GRAPHFLAG .GE. 0) THEN
				CALL DISPLAYLINE ( NINT(L3), NINT(S3), 
     +					      NINT(L4), NINT(S4), GRDN)
			    ELSE
				CALL DRAWLINE (NINT(L3), NINT(S3), 
     +					       NINT(L4), NINT(S4), 
     +					       CURCOLOR, WIDTH)
			    ENDIF
			ENDIF
			L1 = L2
			S1 = S2
		    ENDIF
		ENDDO
	    ENDDO
99	    CONTINUE
	    STATUS = CLGR(1)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)

	ELSE
C		Interactive drawing (digitizing mode)
	    PRINT *,'Press switch 1 to lift and lower pen.'
	    PRINT *,'Press switch 2 to get color under cursor.'
	    PRINT *,'Press switch 3 to end draw mode.'
	    PENDOWN = .FALSE.
	    DO WHILE (.NOT. SWITCHED(3))
		IF (SWITCHED(1)) THEN
		    PENDOWN = .NOT. PENDOWN
		    INSTRING = .FALSE.
		ENDIF
		IF (SWITCHED(2))  CALL CURSORCOLOR (CURCOLOR)

		IF (PENDOWN) THEN
		    IF (.NOT. INSTRING) THEN
			IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
			IF ( IERR .ne. 1 )  RETURN
			OCLINE = CLINE
			OCSAMP = CSAMP
			LINE2 = (CLINE-1)/HZOOMFAC + SLMEM 
			SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM 	
		    ELSE
			IERR = WHERECURSOR (CSAMP, CLINE, NEWPOS)
			IF (IERR .NE. 1) RETURN
			IF (NEWPOS) THEN
			    LINE2 = (CLINE-1)/HZOOMFAC + SLMEM 
			    SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM 	
			    CALL DRAWLINE (LINE1, SAMP1, LINE2, SAMP2, 
     +						CURCOLOR, WIDTH)
			ENDIF
		    ENDIF
		    INSTRING = .TRUE.
		    LINE1 = LINE2
		    SAMP1 = SAMP2
		ENDIF
		CALL VWAIT (5)
	    ENDDO

	ENDIF

	CHANGEDMEM = .TRUE.

	RETURN
	END




	SUBROUTINE DO_TEXT
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER	FLAG, FLAG1, FLAG2, I, TEMPFONT
	INTEGER	NCHAR, SLEN, LINE, SAMP, SWITCH
	INTEGER CLINE, CSAMP, IMP
	BYTE	TEXTARRAY(128)
	LOGICAL	keytest 
        INTEGER IERR, XDTCOLOR
	INTEGER	XDTSIZE, XDTROTATE ,XDTFONT, XDTTEXT, XDCLOCATION


C		Get various text parameters and call text routines 
C		    to set the parameters

	CALL GET_KEYWORD_INTEGER ('SIZE', 1, TEXTSIZE, FLAG1, TEXTSIZE)
	CALL GET_KEYWORD_REAL ('SCALE', 1, TEXTSCALE, FLAG2, TEXTSCALE)
	IF (FLAG1 .GT. 0 .OR. FLAG2 .GT. 0) THEN
	    IERR = XDTSIZE (TEXTSIZE, TEXTSCALE)
	ENDIF

	CALL GET_KEYWORD_INTEGER ('FONT', 1, TEXTFONT, FLAG, TEMPFONT)
	IF (FLAG .GT. 0) THEN
	    IERR = XDTFONT (TEMPFONT)
	    IF ( IERR .ne. 1) THEN
		TEXTFONT = TEMPFONT
	    ENDIF
	ENDIF

	CALL GET_KEYWORD_REAL ('ANGLE', 1, TEXTANGLE, FLAG, TEXTANGLE)
	IF (FLAG .GT. 0) THEN
	    TEXTANGLE = MOD (TEXTANGLE, 360.0)
	    IF (TEXTANGLE .GT. 180.0)  TEXTANGLE = TEXTANGLE - 360.
	    IF (TEXTANGLE .LT. -180.0)  TEXTANGLE = TEXTANGLE + 360.
	    IERR = XDTROTATE (TEXTANGLE)
	ENDIF

	IF (keytest('LEFT')) THEN
	    TEXTLOC = 1
	ELSE IF (keytest('CENTER')) THEN
	    TEXTLOC = 2
	ELSE IF (keytest('RIGHT')) THEN
	    TEXTLOC = 3
	ENDIF


	CALL GET_KEYWORD_STRING ('STRING',1,TEXTSTRING,FLAG,TEXTSTRING)
	IF (FLAG .LT. 0)  RETURN

	NCHAR = SLEN(TEXTSTRING)
	DO I = 1, NCHAR
	    TEXTARRAY(I) = ICHAR(TEXTSTRING(I:I))
	ENDDO

	IF (TEXTFONT .LT. 0) THEN
	    IERR = XDTFONT (0)
	    TEXTFONT = 0
	ENDIF

	CALL RESETSWITCHES
	SWITCH = 2		! Loop until user gets it right
	DO WHILE (SWITCH .EQ. 2)
	    PRINT *,
     +        'Select text position with switch 1.  Quit with switch 3.'
	    CALL SWITCHWAIT (0, SWITCH)
	    IF (SWITCH .EQ. 3) RETURN
	    IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE) ! Get cursor location
	    IF ( IERR .ne. 1 )  RETURN
	    LINE = (CLINE-1)/HZOOMFAC + SLMEM	! Convert to image mem coords
	    SAMP = (CSAMP-1)/HZOOMFAC + SSMEM

	    IERR = XDTCOLOR (1, 0)   ! Write text on graphics plane
	    IERR = XDTTEXT (DISPU, GRIMP, SAMP, LINE, TEXTLOC, 
     +				NCHAR, TEXTARRAY)

	    PRINT *,'Accept with switch 1, reject with switch 2'
	    CALL SWITCHWAIT (0, SWITCH)
	    IERR = XDTCOLOR (0, 0)	! Erase text in graphics plane
	    IERR = XDTTEXT (DISPU, GRIMP, SAMP, LINE, TEXTLOC, 
     +				NCHAR, TEXTARRAY)

	    IF (SWITCH .EQ. 1) THEN	!  If accepted then write text on 
		DO IMP = 1, NUMIMAGES	!  image memory planes 
		    IERR = XDTCOLOR (INT2BYTE(CURCOLOR(IMP)), 0)
		    IERR = XDTTEXT (DISPU, IMP, SAMP, LINE, TEXTLOC, 
     +					NCHAR, TEXTARRAY)
		ENDDO
	    ENDIF
	ENDDO

	CHANGEDMEM = .TRUE.

	RETURN
	END




	SUBROUTINE DO_STRETCH
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	  FLAG, DEFPAR(2), STRETPAR(2)
	INTEGER	  WIDTH, CENTER
	INTEGER   CSAMP, CLINE, LUT
	CHARACTER*5  COLSTR(3)
        INTEGER   IERR
	LOGICAL	  SWITCHED
	INTEGER	  SETCURSOR, WHERECURSOR
        LOGICAL   NEWPOS

        DATA DEFPAR/0,255/
        DATA COLSTR/'RED  ','GREEN','BLUE '/
C		Do interactive stretch if stretch with no parameters

	IF (NUMTOKENS .EQ. 1) THEN
	    CALL RESETSWITCHES
	    PRINT *,'Hit switch 1 to end trackball controlled stretch.'
C				Set cursor to middle of display
	    IERR = SETCURSOR (NSDISP/2, NLDISP/2)
	    IF (IERR .NE. 1)  RETURN
C				Set up initial stretch parameters
	    STRETPAR(1) = 0
	    STRETPAR(2) = 255
	    CALL LINEARLUT (STRETCHLUT, STRETPAR, MAXLUTVAL)
	    CALL UPDATELUTS (1, NLUTS)

C				Loop until switch 1 is depressed
	    DO WHILE (.NOT.SWITCHED(1))
		IERR = WHERECURSOR (CSAMP, CLINE, NEWPOS)
		IF (IERR .NE. 1)  RETURN
C				If the cursor has moved calculate
C				    linear stretch parameters and stretch LUTs
		IF (NEWPOS) THEN  
		    WIDTH = NINT(255*2*(1.0-FLOAT(CLINE)/NLDISP))
		    CENTER = 384 - NINT(512*FLOAT(CSAMP)/NSDISP)
		    STRETPAR(1) = CENTER - (WIDTH+1)/2
		    STRETPAR(2) = STRETPAR(1) + WIDTH
		    CALL LINEARLUT (STRETCHLUT, STRETPAR, MAXLUTVAL)
		    CALL UPDATELUTS (1, NLUTS)
		ENDIF
		CALL VWAIT(5)
	    ENDDO
	    WRITE (*, '(1X,A,I5,1X,I5)', ERR=190) 'Final stretch: ', 
     +			STRETPAR(1), STRETPAR(2)
190	    RETURN
	ENDIF

C			If plain stretch then stretch all three LUTs
	CALL GET_KEYWORD_INTEGER ('STRETCH', 2, DEFPAR, FLAG, STRETPAR)
	IF (FLAG .GT. 0) THEN
	    CALL LINEARLUT (STRETCHLUT, STRETPAR, MAXLUTVAL)
	    CALL UPDATELUTS (1, NLUTS)
	ENDIF

C			Do separate LUT stretches if desired
	DO LUT = 1, NLUTS
	    CALL GET_KEYWORD_INTEGER(COLSTR(LUT),2,DEFPAR,FLAG,STRETPAR)
	    IF (FLAG .GT. 0) THEN
		CALL LINEARLUT (STRETCHLUT, STRETPAR, MAXLUTVAL)
		CALL UPDATELUTS (LUT, LUT)
	    ENDIF
	ENDDO


	RETURN
	END



	SUBROUTINE DO_DISPLAY
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	FLAG, STATUS, I
	INTEGER	IMP, LINE,  OLDNLD, OLDNSD
	BYTE	BUFFER(MAXMEMSAMPS)
	INTEGER	IERR, XDILINEREAD, XDIFILL, XDILINEWRITE
	LOGICAL	keytest


	CALL ERASEPALETTE

	IF (keytest('NOSAVE'))  CHANGEDMEM = .FALSE.

C		If image memory has been changed then write out memory
C		    to file.
	IF (CHANGEDMEM) THEN
	    DO IMP = 1, NUMIMAGES
		DO LINE = 1, NLD
		    IERR = XDILINEREAD (DISPU, IMP, 1, LINE, NSD,BUFFER)
		    IF ( IERR .ne. 1 )  RETURN
		    CALL XVWRIT (UNIT(IMP), BUFFER, STATUS, 
     +					'LINE',LINE+SLFILE-1,
     +					'SAMP',SSFILE, 'NSAMPS',NSD,' ')
		ENDDO
	    ENDDO
	    CHANGEDMEM = .FALSE.
	ENDIF

	OLDNLD = NLD
	OLDNSD = NSD

C		Get the display parameters
	CALL GET_KEYWORD_INTEGER ('SL', 1, 1, FLAG, SLFILE)
	CALL GET_KEYWORD_INTEGER ('SS', 1, 1, FLAG, SSFILE)
	CALL GET_KEYWORD_INTEGER ('NL', 1, NLFILE, FLAG, NLD)
	CALL GET_KEYWORD_INTEGER ('NS', 1, NSFILE, FLAG, NSD)
C		Make the size legal
	NLD = MIN (NLD, NLFILE-SLFILE+1)
	NSD = MIN (NSD, NSFILE-SSFILE+1)
	NLD = MIN (NLD, NLMEM)
	NSD = MIN (NSD, NSMEM)

C		If the old display was larger then clear the image planes
	IF (OLDNLD .GT. NLD .OR. OLDNSD .GT. NSD) THEN
	    DO IMP = 1, NUMIMAGES
		IERR = XDIFILL (DISPU, IMP, 0)
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	ENDIF
	IF (NUMPOLY .GT. 0) THEN
	    IERR = XDIFILL (DISPU, GRIMP, 0)
	    IF ( IERR .ne. 1 )  RETURN
	ENDIF

C		Write the image from the file to the image memory planes
	DO IMP = 1, NUMIMAGES
	    DO LINE = 1, NLD
		CALL XVREAD (UNIT(IMP), BUFFER, STATUS, 'LINE',
     +			  LINE+SLFILE-1,'SAMP',SSFILE, 'NSAMPS',NSD,' ')
		IERR = XDILINEWRITE (DISPU, IMP, 1, LINE, NSD, BUFFER)
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	ENDDO

	DO I = 1, NUMPOLY
	    CALL DISPLAYPOLYGON (GRAPHBUF(1,GRAPHPTR(I)),GRAPHLEN(I),1)
	ENDDO


	RETURN
	END



	SUBROUTINE DO_PAN
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INCLUDE 'verrdefs'
	INTEGER CLINE, CSAMP
	INTEGER SAMP,LINE, IMP
	INTEGER	  XDIDWSET, IERR
	INTEGER	  SETCURSOR, WHERECURSOR
        LOGICAL   SWITCHED
        LOGICAL   NEWPOS


	CALL RESETSWITCHES
	PRINT *,'Hit switch 1 to end pan mode.'

C			Start out at last pan place
	CSAMP = (NSDISP*(SSMEM-1))/NSMEM + 1
	CLINE = (NLDISP*(SLMEM-1))/NLMEM + 1
	IERR = SETCURSOR (CSAMP, CLINE)
	IF (IERR .NE. 1)  RETURN

C		Loop until switch 1 is depressed
C
	DO WHILE (.NOT.SWITCHED(1))
C			Get cursor location 
	    IERR = WHERECURSOR (CSAMP, CLINE, NEWPOS)
C			Convert the cursor position to memory plane position
C			    and pan the image planes
	    IF (NEWPOS) THEN
		SAMP = (NSMEM*(CSAMP-1))/NSDISP + 1
		LINE = (NLMEM*(CLINE-1))/NLDISP + 1
		DO IMP = 1, NUMIMAGES
		    IERR = XDIDWSET (DISPU, IMP, SAMP, LINE)
		    IF ((IERR.ne.1).and.(IERR.ne.MUSTSETDW))  RETURN
		ENDDO
		IF (IGRAPH .EQ. 1) THEN
		    IERR = XDIDWSET (DISPU, GRIMP, SAMP, LINE)
		    IF ((IERR.ne.1).and.(IERR.ne.MUSTSETDW))  RETURN
		ENDIF
	    ENDIF
	    CALL VWAIT (5)
	ENDDO

	SLMEM = LINE
	SSMEM = SAMP

	RETURN
	END




	SUBROUTINE DO_HZOOM
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INCLUDE 'verrdefs'
	INTEGER	FLAG, TEMP
	INTEGER IMP
	INTEGER	XDIZOOM,  IERR

	TEMP = HZOOMFAC
	CALL GET_KEYWORD_INTEGER ('HZOOM',1, 2*HZOOMFAC, FLAG, HZOOMFAC)
	HZOOMFAC = MAX (HZOOMFAC, 1)

	DO IMP = 1, NIMPS
	    IERR = XDIZOOM (DISPU, IMP, HZOOMFAC)
	    IF ((IERR.ne.1).and.(IERR.ne.MUSTZOOM)) THEN
		HZOOMFAC = TEMP
		GOTO 199
	    ENDIF
	ENDDO
199	CONTINUE

	RETURN
	END



	SUBROUTINE DO_TABLE
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	FILENUM, TABNUM, FLAG, TABUNIT, STATUS, NSTAB, NLTAB
	INTEGER   LUTARRAY(256,4), LUT, I, J
        INTEGER*2 TABARRAY(4,256)
	CHARACTER*16  FORMAT
	INTEGER	IERR, XDLREAD, XDLWRITE


	CALL GET_KEYWORD_INTEGER ('NUMBER', 1, 1, FLAG, TABNUM)

	CALL GET_KEYWORD_INTEGER ('LOAD', 1, NUMIMAGES+1, FLAG, FILENUM)
	IF (FLAG .GE. 0) THEN
	    IF (FILENUM.LE.NUMIMAGES .OR. FILENUM.GT.NUMINPFILES) THEN
		PRINT *,'Incorrect input file for table'
		RETURN
	    ENDIF
	    CALL XVUNIT (TABUNIT, 'INP', FILENUM, STATUS,' ')
	    CALL XVOPEN (TABUNIT, STATUS, 'OP','READ',
     *                   'U_FORMAT','HALF',' ')
	    CALL XVGET (TABUNIT, STATUS, 'NS',NSTAB, 'NL',NLTAB,
     +				'FORMAT',FORMAT,' ')
	    IF (FORMAT(1:4) .NE. 'BYTE' .OR. NSTAB .NE. 1024) THEN
		PRINT *,'Incorrect table format'
		RETURN
	    ENDIF
	    IF (TABNUM .GT. NLTAB) THEN
		WRITE (*,'(A,I2,A)') ' Table number', TABNUM, 
     +					' not available in file.'
		CALL XVCLOSE (TABUNIT, STATUS,' ')
		RETURN
	    ENDIF
	    CALL XVREAD (TABUNIT, TABARRAY, STATUS, 'LINE',TABNUM,' ')
	    CALL XVCLOSE (TABUNIT, STATUS,' ')
	    DO J = 1, 4
		DO I = 1, 256
		    LUTARRAY(I,J) = TABARRAY(J,I)
		ENDDO
	    ENDDO
	    DO I = 1, 256
		STRETCHLUT(I) = I-1
	    ENDDO
	    DO LUT = 1, 3
		DO I = 1, 256
		    PSEUDOLUT(I,LUT) = LUTARRAY(I,LUT)
		ENDDO
	    ENDDO
	    DO LUT = 1, 3
		IERR = XDLWRITE (DISPU, LUT, LUTSECT, LUTARRAY(1,LUT))
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	    RETURN
	ENDIF


	CALL GET_KEYWORD_INTEGER ('SAVE',1,NUMOUTIMAGES+1,FLAG,FILENUM)
	IF (FLAG .GE. 0) THEN
	    IF (FILENUM.LE.NUMOUTIMAGES .OR.FILENUM.GT.NUMOUTFILES) THEN
		PRINT *,'Incorrect output file for table'
		RETURN
	    ENDIF
	    CALL XVUNIT (TABUNIT, 'OUT', FILENUM, STATUS,' ')
 	    CALL XVOPEN (TABUNIT, STATUS, 'IO_ACT','S', 'OPEN_ACT','S',
     +		    'OP','WRITE', 'U_NL',1, 'U_NS',1024,
     +		    'U_FORMAT','HALF', 'O_FORMAT','BYTE',' ')
	    DO LUT = 1, 3
		IERR = XDLREAD (DISPU, LUT, LUTSECT, LUTARRAY(1,LUT))
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	    DO J = 1, 3
		DO I = 1, 256
		    TABARRAY(J,I) = LUTARRAY(I,J) 
		ENDDO
	    ENDDO
	    DO I = 1, 256
		TABARRAY(4,I) = 0
	    ENDDO
	    CALL XVWRIT (TABUNIT, TABARRAY, STATUS,' ')
	    CALL XVCLOSE (TABUNIT, STATUS,' ')
	    RETURN
	ENDIF

	RETURN
	END



	SUBROUTINE DO_COLOR
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER	FLAG, SWITCH, COLORNUM
	INTEGER	RGBDEF(3), RGBVAL(3), DNVAL
	INTEGER CLINE, CSAMP
	INTEGER  IMP, LINE, SAMP
	INTEGER  I, J, LOFF, SOFF, JOFF
	INTEGER	   SUM1, SUM2
	INTEGER	NLCOMP, NSCOMP
	BYTE    BUFFER(2*MAXBRUSHSIZE)
C	EQUIVALENCE (DN,DNB)
	REAL	COLX, COLY, COLZ, MINX, MAXX, MINY, MAXY
	REAL	RED, GREEN, BLUE
	REAL	TRICENLINE, TRICENSAMP, TRISCALE
	REAL	INTENSLINE, INTENSSAMP, INTENSSCALE
	INTEGER	IERR, XDCLOCATION, XDILINEREAD
        INTEGER SETCURSOR, WHERECURSOR
	LOGICAL	keytest, SWITCHED
	LOGICAL	TRIANGLE, NEWPOS

        DATA RGBDEF/128,128,128/
C			Get the color number
	CALL GET_KEYWORD_INTEGER ('COLOR', 1, 0, FLAG, COLORNUM)

C			Use DN for setting black and white value
	CALL GET_KEYWORD_INTEGER ('DN', 1, 128, FLAG, DNVAL)
	IF (FLAG .GE. 0) THEN
	    CURCOLOR(1) = DNVAL
	    CURCOLOR(2) = DNVAL
	    CURCOLOR(3) = DNVAL
	    GOTO 199
	ENDIF

C			Use RGB for setting (red,green,blue) color
	CALL GET_KEYWORD_INTEGER ('RGB', 3, RGBDEF, FLAG, RGBVAL)
	IF (FLAG .GE. 0) THEN
	    CURCOLOR(1) = RGBVAL(1)
	    CURCOLOR(2) = RGBVAL(2)
	    CURCOLOR(3) = RGBVAL(3)
	    DO IMP = NUMIMAGES+1, 3
		CURCOLOR(IMP) = CURCOLOR(1)
	    ENDDO
	    GOTO 199
	ENDIF


C			Do cursor color procedure
	IF (keytest('CURSOR') .OR. keytest('AVERAGE')) THEN
	    CALL RESETSWITCHES
	    PRINT *,'Select point and hit switch 1 to end.'
	    CALL SWITCHWAIT (1, SWITCH)

	    IF (.NOT. keytest('AVERAGE')) THEN
C				Get the color of the pixel under the cursor
		CALL CURSORCOLOR (CURCOLOR)

	    ELSE
		IERR = XDCLOCATION (DISPU, CURS, SAMP, LINE)
		IF ( IERR .ne. 1 )  RETURN
		LINE = (LINE-1)/HZOOMFAC + SLMEM
		SAMP = (SAMP-1)/HZOOMFAC + SSMEM
C			Get the color of the area under the brush
		LOFF = LINE - MAXBRUSHSIZE/2
		SOFF = SAMP - MAXBRUSHSIZE/2 + 1
		JOFF = 1
		IF (SOFF .LT. 1) THEN
		    JOFF = 2 - SOFF
		    SOFF = 1
		ENDIF
		DO IMP = 1, NUMIMAGES
		  SUM1 = 0
		  SUM2 = 0
		  DO I = 1, MAXBRUSHSIZE
		    LINE = LOFF + I
		    IERR = XDILINEREAD (DISPU, IMP, SOFF, LINE,
     +				MAXBRUSHSIZE, BUFFER(JOFF))
		    IF ( IERR .ne. 1 )  RETURN
		    DO J = JOFF, MAXBRUSHSIZE
		      IF (BRUSHPAT(I,J) .GT. 0) THEN
C			DNB(1) = BUFFER(J)
C			DNB(2) = 0
			SUM1 = SUM1 + BRUSHPAT(I,J)
			SUM2 = SUM2 + BRUSHPAT(I,J)*BYTE2INT(BUFFER(J))
		      ENDIF
		    ENDDO
		  ENDDO
		  CURCOLOR(IMP) = NINT(FLOAT(SUM2)/SUM1)
		ENDDO
		DO IMP = NUMIMAGES+1, 3
		    CURCOLOR(IMP) = CURCOLOR(1)
		ENDDO
	    ENDIF
	    GOTO 199
	ENDIF


C			Do compose color procedure
	IF (keytest('COMPOSE')) THEN
	    CALL RESETSWITCHES
	    IF (COLORMODE .EQ. FULLCOLOR) THEN
		PRINT *,
     +'Hit switch 1 to toggle between intensity scale & color triangle.'
	    ENDIF
	    PRINT *,'Hit switch 2 to end.'
	    CALL DISPLAYPALETTE

	    NLCOMP = MIN( NLDISP, (NLMEM-SLMEM+1)*HZOOMFAC )
	    NSCOMP = MIN( NSDISP, (NSMEM-SSMEM+1)*HZOOMFAC )
	    TRICENLINE = 0.75*NLCOMP
	    TRICENSAMP = 0.65*NSCOMP
	    TRISCALE = 0.3*NSCOMP
	    INTENSLINE = 0.75*NLCOMP
	    INTENSSAMP = 0.15*NSCOMP
	    INTENSSCALE = 0.3*NSCOMP
	    CALL DRAWCOMPOSE (1, COLORMODE, TRICENLINE, TRICENSAMP, 
     +			TRISCALE, INTENSLINE, INTENSSAMP, INTENSSCALE )

	    COLORNUM = MIN (MAX (COLORNUM, 1), PALCOLORS)
	    RED = COLORTABLE(1,COLORNUM)/255.
	    GREEN = COLORTABLE(2,COLORNUM)/255.
	    BLUE = COLORTABLE(3,COLORNUM)/255.
	    COLX = 0.0
	    COLY = 0.0
	    IF (COLORMODE .EQ. FULLCOLOR) THEN
		COLZ = 0.333*(RED + GREEN + BLUE)
		IF (COLZ .NE. 0.0) THEN
		    COLX = 0.5*(GREEN - RED) /(3.*COLZ)
		    COLY = 0.577*(BLUE - 0.5*(RED + GREEN)) /(3.*COLZ)
		ENDIF
	    ELSE
		COLZ = RED
	    ENDIF

	    MINY = -0.288675
	    MAXY =  0.577350
	    TRIANGLE = .FALSE.
	    CLINE = INTENSLINE
	    CSAMP = INTENSSAMP + INTENSSCALE*COLZ
            IERR = SETCURSOR (CSAMP, CLINE)
	    IF (IERR .NE. 1)  RETURN

	    DO WHILE (.NOT. SWITCHED(2))
		IF (COLORMODE .EQ. FULLCOLOR .AND. SWITCHED(1)) THEN
		    TRIANGLE = .NOT. TRIANGLE
		    IF (TRIANGLE) THEN
			CLINE = TRICENLINE - TRISCALE*COLY
			CSAMP = TRICENSAMP + TRISCALE*COLX
		    ELSE
			CLINE = INTENSLINE
			CSAMP = INTENSSAMP + INTENSSCALE*COLZ
		    ENDIF
		    IERR = SETCURSOR (CSAMP, CLINE)
		ENDIF

		IERR = WHERECURSOR (CSAMP, CLINE, NEWPOS) ! Get cursor location
		IF (IERR .NE. 1)  RETURN
		IF (NEWPOS) THEN
		    IF (TRIANGLE) THEN	! Convert cursor pos into triangle pos
			COLY = -(CLINE - TRICENLINE) /TRISCALE
			COLX = (CSAMP - TRICENSAMP) /TRISCALE
			COLY = MIN( MAX( COLY, MINY), MAXY)
			MINX = (COLY-MINY)/1.73205 - 0.5
			MAXX = -(COLY-MINY)/1.73205 + 0.5
			COLX = MIN( MAX( COLX, MINX), MAXX)
		    ELSE		! Convert cursor pos into intensity
			COLZ = (CSAMP - INTENSSAMP) /INTENSSCALE
			COLZ = MIN (MAX( COLZ, 0.0), 1.0)
		    ENDIF		! Transform into color space
		    RED =   COLZ*3.*(-COLX - 0.57735*COLY + 0.33333)
		    GREEN = COLZ*3.*(+COLX - 0.57735*COLY + 0.33333)
		    BLUE =  COLZ*3.*(        1.15470*COLY + 0.33333)
		    COLORTABLE(1,COLORNUM) =
     *                         MIN(MAX(NINT(255.*RED), 0),255)
		    COLORTABLE(2,COLORNUM) =
     *                         MIN(MAX(NINT(255.*GREEN), 0),255)
		    COLORTABLE(3,COLORNUM) =
     *                         MIN(MAX(NINT(255.*BLUE), 0),255)
		    CALL UPDATEPALETTE (COLORNUM)
		ENDIF
		CALL VWAIT (10)
	    ENDDO
	    CALL DRAWCOMPOSE (0, COLORMODE, TRICENLINE, TRICENSAMP, 
     +			TRISCALE, INTENSLINE, INTENSSAMP, INTENSSCALE )
	ENDIF



C	    If a color number was specified but no new color was made
C		(COLOR n) then get the color from the table
	IF (COLORNUM .GT. 0 .AND. COLORNUM .LE. MAXCOLORS) THEN
	    DO I = 1, 3
		CURCOLOR(I) = COLORTABLE(I,COLORNUM)
	    ENDDO
	ENDIF
	RETURN

C		If a color number was specified and a color was created
C		    then update the color table
199	CONTINUE
	IF (COLORNUM .GT. 0 .AND. COLORNUM .LE. MAXCOLORS) THEN
	    DO I = 1, 3
		COLORTABLE(I,COLORNUM) = CURCOLOR(I)
	    ENDDO
	    CALL UPDATEPALETTE (COLORNUM)
	ENDIF
	
	RETURN
	END





	SUBROUTINE DO_PSEUDO
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER LUT
	INTEGER	FLAG, I, PSTABLE, DNPERCOL
	INTEGER	PSCOLOR(3), RGBVAL(3)
	INTEGER	DNVAL(2), DNDEF(2)/0,0/
	INTEGER CLINE, CSAMP
	INTEGER	NLCOMP, NSCOMP
	REAL	COLX, COLY, COLZ, MINX, MAXX, MINY, MAXY
	REAL	RED, GREEN, BLUE
	REAL	TRICENLINE, TRICENSAMP, TRISCALE
	REAL	INTENSLINE, INTENSSAMP, INTENSSCALE
	INTEGER	IERR, SETCURSOR, WHERECURSOR
	LOGICAL	keytest, SWITCHED
	LOGICAL	TRIANGLE, NEWPOS
	INTEGER NPSCOLOR(7)
	INTEGER PSEUDOTABLE(3,32,7)

	DATA NPSCOLOR /8,16,32,16,8,6,4/
        DATA PSEUDOTABLE /0,0,255, 0,128,255, 0,255,255, 0,255,0,   ! table 1
     +	 84,200,0, 255,255,0, 255,128,0, 255,0,0, 72*0,	            ! table 2
     +	 170,0,255, 115,0,210, 125,70,240, 0,50,255, 50,110,224,
     +   30,150,200, 0,200,200, 0,185,120, 0,225,100, 0,255,0,
     +   200,255,150, 255,255,0, 255,215,0, 255,160,0, 255,100,0,
     +   255,0,0, 48*0,				        ! table 3
     +	 0,0,0, 0,0,160, 0,0,200, 0,0,255, 0,70,230, 0,100,200,
     +   0,130,170, 0,150,150, 0,170,130, 0,190,100, 0,210,80,
     +   0,220,60, 0,200,0, 90,180,0,	130,170,0, 220,170,0, 220,200,0,
     +   255,220,0, 255,240,0, 255,255,0, 255,140,80,	255,120,80,
     +   255,80,80, 200,60,60, 220,0,0, 255,0,0, 230,0,100, 245,0,120,
     +	 255,0,150, 255,50,180, 255,0,255, 255,255,255,	            ! table 4
     +	 0,0,160, 0,0,255, 0,100,200, 0,130,170, 0,170,130, 0,210,80,
     +   0,200,0, 130,170,0, 255,220,0, 255,255,0, 255,140,80,
     +   255,80,80, 255,0,0, 230,0,100, 255,0,150, 255,0,255, 48*0, ! table 5
     +	 0,0,255, 0,130,170, 0,170,130, 0,200,0, 255,255,0, 255,80,80,
     +   255,0,0, 255,0,150, 72*0,				    ! table 6
     +	 0,0,255, 0,170,130, 0,200,0, 255,255,0, 255,0,0, 255,0,150,
     +   78*0, 0,0,255, 0,190,100, 255,255,0, 255,0,0, 84*0/	    ! table 7



	IF (COLORMODE .EQ. FULLCOLOR) THEN
	    PRINT *, 'Pseudo color not valid in color mode.'
	    RETURN
	ENDIF
	IF (NLUTS .LT. 3) THEN
	   PRINT *,
     +          'Must have three look up tables (LUTs) for pseudo color'
	   RETURN
	ENDIF

C				Turn pseudo color off - go to gray scale LUTs
	IF (keytest('OFF')) THEN
	    COLORMODE = BW
	    CALL UPDATELUTS (1,3)
	    RETURN
	ENDIF


	COLORMODE = PSEUDO

C			Load LUTs with a pseudo color table
	CALL GET_KEYWORD_INTEGER ('TABLE', 1, 3, FLAG, PSTABLE)
	IF (FLAG .GE. 0) THEN
	    PSTABLE = MIN( MAX( PSTABLE, 0), 7)
	    IF (PSTABLE .EQ. 0) THEN
		DO LUT = 1, 3
		    DO I = 1, 256
			PSEUDOLUT(I,LUT) = I - 1
		    ENDDO
		ENDDO
	    ELSE
		DNPERCOL = 256/NPSCOLOR(PSTABLE)
		DO LUT = 1, 3
		    DO I = 1, 256
			PSEUDOLUT(I,LUT) = 
     +			PSEUDOTABLE(LUT, 1+(I-1)/DNPERCOL, PSTABLE)
		    ENDDO
		ENDDO
	    ENDIF
	    CALL UPDATELUTS (1, 3)
	    RETURN
	ENDIF
	


C			Get DN values to change LUT's for
	CALL GET_KEYWORD_INTEGER ('DN', 2, DNDEF, FLAG, DNVAL)

	IF (FLAG .LE. 0)  RETURN  ! Need DN range for all the rest

	IF (FLAG .EQ. 1)  DNVAL(2) = DNVAL(1)
	DNVAL(1) = MIN( MAX( DNVAL(1), 0), 255)
	DNVAL(2) = MIN( MAX( DNVAL(2), 0), 255)

C			Get original color for first DN value
	DO LUT = 1, 3
	    PSCOLOR(LUT) = PSEUDOLUT(DNVAL(1)+1,LUT)
	ENDDO


C			Get pseudo color from RGB (red,green,blue) parameter
	CALL GET_KEYWORD_INTEGER ('RGB', 3, PSCOLOR, FLAG, RGBVAL)
	IF (FLAG .GE. 0) THEN
	    PSCOLOR(1) = RGBVAL(1)
	    PSCOLOR(2) = RGBVAL(2)
	    PSCOLOR(3) = RGBVAL(3)
	    DO LUT = 1, 3
		DO I = DNVAL(1)+1, DNVAL(2)+1
		    PSEUDOLUT(I,LUT) = PSCOLOR(LUT)
		ENDDO
	    ENDDO
	    CALL UPDATELUTS (1, 3)
	    RETURN
	ENDIF


C			Do compose color procedure
	IF (keytest('COMPOSE')) THEN
	    CALL RESETSWITCHES
	    PRINT *,
     +'Hit switch 1 to toggle between intensity scale & color triangle.'
	    PRINT *,'Hit switch 2 to end.'

	    NLCOMP = MIN( NLDISP, (NLMEM-SLMEM+1)*HZOOMFAC )
	    NSCOMP = MIN( NSDISP, (NSMEM-SSMEM+1)*HZOOMFAC )
	    TRICENLINE = 0.75*NLCOMP
	    TRICENSAMP = 0.65*NSCOMP
	    TRISCALE = 0.3*NSCOMP
	    INTENSLINE = 0.75*NLCOMP
	    INTENSSAMP = 0.15*NSCOMP
	    INTENSSCALE = 0.3*NSCOMP
	    CALL DRAWCOMPOSE (1, FULLCOLOR, TRICENLINE, TRICENSAMP, 
     +			TRISCALE, INTENSLINE, INTENSSAMP, INTENSSCALE )
	    RED = PSCOLOR(1)/255.
	    GREEN = PSCOLOR(2)/255.
	    BLUE = PSCOLOR(3)/255.
	    COLZ = 0.333*(RED + GREEN + BLUE)
	    IF (COLZ .NE. 0.0) THEN
		COLX = 0.5*(GREEN - RED) /(3.*COLZ)
		COLY = 0.577*(BLUE - 0.5*(RED + GREEN)) /(3.*COLZ)
	    ELSE
		COLX = 0.0
		COLY = 0.0
	    ENDIF

	    MINY = -0.288675
	    MAXY =  0.577350
	    TRIANGLE = .FALSE.
	    CLINE = INTENSLINE
	    CSAMP = INTENSSAMP + INTENSSCALE*COLZ
            IERR = SETCURSOR (CSAMP, CLINE)
	    IF (IERR .NE. 1)  RETURN

	    DO WHILE (.NOT. SWITCHED(2))
		IF (SWITCHED(1)) THEN
		    TRIANGLE = .NOT. TRIANGLE
		    IF (TRIANGLE) THEN
			CLINE = TRICENLINE - TRISCALE*COLY
			CSAMP = TRICENSAMP + TRISCALE*COLX
		    ELSE
			CLINE = INTENSLINE
			CSAMP = INTENSSAMP + INTENSSCALE*COLZ
		    ENDIF
		    IERR = SETCURSOR (CSAMP, CLINE)
		ENDIF

		IERR = WHERECURSOR (CSAMP, CLINE, NEWPOS) ! Get cursor location
		IF (IERR .NE. 1)  RETURN
		IF (NEWPOS) THEN
		    IF (TRIANGLE) THEN	! Convert cursor pos into triangle pos
			COLY = -(CLINE - TRICENLINE) /TRISCALE
			COLX = (CSAMP - TRICENSAMP) /TRISCALE
			COLY = MIN( MAX( COLY, MINY), MAXY)
			MINX = (COLY-MINY)/1.73205 - 0.5
			MAXX = -(COLY-MINY)/1.73205 + 0.5
			COLX = MIN( MAX( COLX, MINX), MAXX)
		    ELSE		! Convert cursor pos into intensity
			COLZ = (CSAMP - INTENSSAMP) /INTENSSCALE
			COLZ = MIN (MAX( COLZ, 0.0), 1.0)
		    ENDIF		! Transform into color space
		    RED =   COLZ*3.*(-COLX - 0.57735*COLY + 0.33333)
		    GREEN = COLZ*3.*(+COLX - 0.57735*COLY + 0.33333)
		    BLUE =  COLZ*3.*(        1.15470*COLY + 0.33333)
		    PSCOLOR(1) = MIN(MAX(NINT(255.*RED), 0),255)
		    PSCOLOR(2) = MIN(MAX(NINT(255.*GREEN), 0),255)
		    PSCOLOR(3) = MIN(MAX(NINT(255.*BLUE), 0),255)
		    DO LUT = 1, 3
			DO I = DNVAL(1)+1, DNVAL(2)+1
			    PSEUDOLUT(I,LUT) = PSCOLOR(LUT)
			ENDDO
		    ENDDO
		    CALL UPDATELUTS (1, 3)
		ENDIF
		CALL VWAIT (10)
	    ENDDO
	    CALL DRAWCOMPOSE (0, FULLCOLOR, TRICENLINE, TRICENSAMP, 
     +			TRISCALE, INTENSLINE, INTENSSAMP, INTENSSCALE )
	    RETURN
	ENDIF


	RETURN
	END





	SUBROUTINE DO_PALETTE
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	I, COL, FLAG, COLOR(3)
	LOGICAL	keytest
	CHARACTER*80  FILENAME


	CALL GET_KEYWORD_STRING('SAVE',1,'EDIMAGE.PAL',FLAG,FILENAME)  
	IF (FLAG .GE. 0) THEN
	    OPEN (UNIT=1,FILE=FILENAME,ACCESS='SEQUENTIAL',STATUS='NEW')
	    DO COL = 1, MAXCOLORS
		WRITE (1,'(1X,4(I3,2X))')  COL,(COLORTABLE(I,COL),I=1,3)
	    ENDDO
	    CLOSE (1)
	    RETURN
	ENDIF


	CALL GET_KEYWORD_STRING ('LOAD', 1, 'EDIMAGE.PAL',FLAG,FILENAME)
	IF (FLAG .GE. 0) THEN
	    OPEN (UNIT=1, FILE=FILENAME, ACCESS='SEQUENTIAL', 
     +			STATUS='OLD', ERR=89)
	    DO WHILE (.TRUE.)
		READ (1,'(1X,4(I3,2X))', END=110, ERR=99)  
     +					COL, (COLOR(I), I =1,3)
		DO I = 1, 3
		    COLORTABLE(I,COL) = COLOR(I)
		ENDDO
	    ENDDO
110	    CONTINUE
	    CLOSE (1)
	    IF (PALETTEON)  CALL DISPLAYPALETTE
	    RETURN
89	    PRINT *,'Error opening palette file'
	    RETURN
99	    PRINT *,'Error reading palette file'
	    RETURN
	ENDIF


	IF (keytest('OFF')) THEN
	    CALL ERASEPALETTE
	ELSE
	    CALL DISPLAYPALETTE
	ENDIF

	RETURN
	END




	SUBROUTINE DO_BRUSH
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	FLAG


	CALL GET_KEYWORD_INTEGER ('SIZE', 1, BRUSHSIZE, FLAG, BRUSHSIZE)
	CALL GET_KEYWORD_STRING ('TYPE', 1, BRUSHTYPE, FLAG, BRUSHTYPE)

	CALL MAKEBRUSH

	WRITE (*, '(1X,A,I3,A,16A)') 'Brush size: ', BRUSHSIZE,
     +					'      Brush type: ', BRUSHTYPE

	RETURN
	END




	SUBROUTINE DO_POLYGON
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER PTR, STPTR, N, I, SWITCH, GRNUM, FLAG
	INTEGER	LINE1, SAMP1, LINE2, SAMP2, BSIZE(4), WRGR, PUTGR
	INTEGER CLINE, CSAMP, STATUS, RDGR, NEXTGR, GETGR, CLGR, ENDGR
	REAL	STARTX, STARTY
	LOGICAL	EOF, EOL,  keytest
	INTEGER	IERR, XDIFILL, XDCLOCATION


	IF (IGRAPH .LE. 0) THEN
	    PRINT *,'Graphics plane not available.  Command inoperative.'
	    RETURN
	ENDIF


	IF (keytest('READ')) THEN
C				Read an IBIS graphics file

	    CALL GET_KEYWORD_INTEGER ('READ',1,NUMIMAGES+1, FLAG, GRNUM)
	    IF (GRNUM .LE. NUMIMAGES .OR. GRNUM .GT. NUMINPFILES) THEN
		PRINT *,'Incorrect input file for graphics'
		RETURN
	    ENDIF
	    STATUS = RDGR (GRNUM, 1, 2)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    PTR = GRAPHPTR(NUMPOLY+1)
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		STATUS = NEXTGR(1,EOF,GRAPHBUF(1,PTR),GRAPHBUF(2,PTR),0)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
		IF (EOF) GOTO 99
		PTR = PTR + 1
		EOL = .FALSE.
		DO WHILE (.NOT. EOL)
		   STATUS = GETGR (1, EOL, EOF, GRAPHBUF(1,PTR),
     +                          GRAPHBUF(2,PTR), 0)
                   IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
		   IF (EOF) GOTO 99
		   PTR = PTR + 1
		   IF (PTR .GT. MAXGRBUF-10) THEN
		      PRINT *,
     +                     'Out of space in graphics buffer - stopping.'
		      GOTO 99
		   ENDIF
		ENDDO
		PTR = PTR - 1
		NUMPOLY = NUMPOLY + 1
		N = PTR - GRAPHPTR(NUMPOLY)
		GRAPHLEN(NUMPOLY) = N
		GRAPHPTR(NUMPOLY+1) = GRAPHPTR(NUMPOLY) + N
		CALL DISPLAYPOLYGON (GRAPHBUF(1,GRAPHPTR(NUMPOLY)),N, 1)
		IF (NUMPOLY .GE. MAXNUMPOLY-4) THEN
		    PRINT *,'Out of space in graphics buffer - stopping.'
		    GOTO 99
		ENDIF
	    ENDDO
99	    CONTINUE
	    STATUS = CLGR(1)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)


	ELSE IF (keytest('WRITE')) THEN
C				Write an IBIS graphics file
	    CALL GET_KEYWORD_INTEGER ('WRITE', 1, NUMOUTIMAGES+1,
     +                                FLAG, GRNUM)
	    IF (GRNUM.LE.NUMOUTIMAGES .OR. GRNUM .GT. NUMOUTFILES) THEN
		PRINT *,'Incorrect output file for graphics'
		RETURN
	    ENDIF
	    STATUS = WRGR (GRNUM, 1, 2)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    DO I = 1, NUMPOLY
		DO PTR = GRAPHPTR(I), GRAPHPTR(I)+GRAPHLEN(I)-1
		    STATUS = PUTGR (1,GRAPHBUF(1,PTR),GRAPHBUF(2,PTR),0)
                    IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
		ENDDO
		STATUS = ENDGR (1)
                IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    ENDDO
	    STATUS = CLGR(1)
            IF (STATUS .NE. 1) CALL SIGNALGR(1,STATUS,1)
	    PRINT *, 'Graphics buffer written to file.'


	ELSE IF (keytest('CLEAR')) THEN
C			Clear graphics display plane and graphics memory
	    NUMPOLY = 0
	    GRAPHPTR(1) = 1
	    IERR = XDIFILL (DISPU, GRIMP, 0)


	ELSE IF (keytest('BOX')) THEN
C			
 	 IF (keytest('SIZE')) THEN
 	    CALL GET_KEYWORD_INTEGER('SIZE',4,BSIZE,FLAG,BSIZE)
 	    LINE1 =BSIZE(1)
 	    SAMP1 = BSIZE(2)
 	    LINE2 = BSIZE(3) + LINE1 - 1
 	    SAMP2 = BSIZE(4) + SAMP1 - 1
 
 	    CALL ORDER (LINE1, LINE2)
 	    CALL ORDER (SAMP1, SAMP2)
 
 	    PTR = GRAPHPTR(NUMPOLY+1)
 	    NUMPOLY = NUMPOLY + 1
 	    IF (PTR.GT.MAXGRBUF-10 .OR. NUMPOLY .GE. MAXNUMPOLY-4) THEN
 		PRINT *, 'Out of space in graphics buffer.'
 	    ENDIF
 	    GRAPHBUF(1,PTR) = FLOAT(LINE1)
 	    GRAPHBUF(2,PTR) = FLOAT(SAMP1)
 	    GRAPHBUF(1,PTR+1) = FLOAT(LINE1)
 	    GRAPHBUF(2,PTR+1) = FLOAT(SAMP2)
 	    GRAPHBUF(1,PTR+2) = FLOAT(LINE2)
 	    GRAPHBUF(2,PTR+2) = FLOAT(SAMP2)
 	    GRAPHBUF(1,PTR+3) = FLOAT(LINE2)
 	    GRAPHBUF(2,PTR+3) = FLOAT(SAMP1)
 	    GRAPHBUF(1,PTR+4) = FLOAT(LINE1)
 	    GRAPHBUF(2,PTR+4) = FLOAT(SAMP1)
 	    GRAPHLEN(NUMPOLY) = 5
 	    GRAPHPTR(NUMPOLY+1) = GRAPHPTR(NUMPOLY) + 5
 	    CALL DISPLAYPOLYGON (GRAPHBUF(1,GRAPHPTR(NUMPOLY)), 5, 1)
 
	 ELSE
	    CALL RESETSWITCHES
	    PRINT *,'Select first corner with switch 1'
	    CALL SWITCHWAIT (1, SWITCH)
	    IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE) ! Get cursor location
	    IF ( IERR .ne. 1 )  RETURN
	    LINE1 = (CLINE-1)/HZOOMFAC + SLMEM + SLFILE-1
	    SAMP1 = (CSAMP-1)/HZOOMFAC + SSMEM + SSFILE-1

	    PRINT *,'Select second corner with switch 1'
	    CALL SWITCHWAIT (1, SWITCH)
	    IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE) ! Get cursor location
	    IF ( IERR .ne. 1 )  RETURN
	    LINE2 = (CLINE-1)/HZOOMFAC + SLMEM + SLFILE-1
	    SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM + SSFILE-1

	    CALL ORDER (LINE1, LINE2)
	    CALL ORDER (SAMP1, SAMP2)

	    PTR = GRAPHPTR(NUMPOLY+1)
	    NUMPOLY = NUMPOLY + 1
	    IF (PTR.GT.MAXGRBUF-10 .OR. NUMPOLY .GE. MAXNUMPOLY-4) THEN
		PRINT *, 'Out of space in graphics buffer.'
	    ENDIF
	    GRAPHBUF(1,PTR) = FLOAT(LINE1)
	    GRAPHBUF(2,PTR) = FLOAT(SAMP1)
	    GRAPHBUF(1,PTR+1) = FLOAT(LINE1)
	    GRAPHBUF(2,PTR+1) = FLOAT(SAMP2)
	    GRAPHBUF(1,PTR+2) = FLOAT(LINE2)
	    GRAPHBUF(2,PTR+2) = FLOAT(SAMP2)
	    GRAPHBUF(1,PTR+3) = FLOAT(LINE2)
	    GRAPHBUF(2,PTR+3) = FLOAT(SAMP1)
	    GRAPHBUF(1,PTR+4) = FLOAT(LINE1)
	    GRAPHBUF(2,PTR+4) = FLOAT(SAMP1)
	    GRAPHLEN(NUMPOLY) = 5
	    GRAPHPTR(NUMPOLY+1) = GRAPHPTR(NUMPOLY) + 5
	    CALL DISPLAYPOLYGON (GRAPHBUF(1,GRAPHPTR(NUMPOLY)), 5, 1)
	  ENDIF

	ELSE

C		Interactive polygon creation

	    CALL RESETSWITCHES
	    PRINT *, 
     +'Press switch 1 for each vertex. Press switch 2 for next polygon.'
	    PRINT *,'Press switch 3 to end polygon mode.'
180	  CONTINUE
	    STPTR = GRAPHPTR(NUMPOLY+1)
	    PTR = STPTR
	    DO WHILE (.TRUE.)	! Loop until switch 2 or 3 is depressed
		CALL SWITCHWAIT (0, SWITCH)
		IF (SWITCH .GE. 2) GOTO 199

C			Get cursor location 
		IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
		IF ( IERR .ne. 1 )  RETURN
C			Convert the cursor position to image file coords
		GRAPHBUF(1,PTR) = (CLINE-1)/HZOOMFAC + SLMEM + SLFILE-1
		GRAPHBUF(2,PTR) = (CSAMP-1)/HZOOMFAC + SSMEM + SSFILE-1	
		IF (PTR .GT. STPTR) THEN
		    IF (GRAPHBUF(1,PTR) .EQ. GRAPHBUF(1,PTR-1) .AND.
     +		        GRAPHBUF(2,PTR) .EQ. GRAPHBUF(2,PTR-1) ) THEN
			PTR = PTR - 1
		    ELSE
			CALL DISPLAYPOLYGON (GRAPHBUF(1,PTR-1), 2, 1)
		    ENDIF
		ELSE
		    CALL DISPLAYPOLYGON (GRAPHBUF(1,PTR), 1, 1)
		ENDIF
		PTR = PTR + 1
		IF (PTR .GT. MAXGRBUF-10) THEN
		    PRINT *, 'Out of space in graphics buffer.'
		    GOTO 299
		ENDIF
	    ENDDO
199	    CONTINUE

C			Only accept polygon if at least three vertices
	    IF (PTR .GE. STPTR+3) THEN  
			! Close the polygon if not already closed
		STARTY = GRAPHBUF(1,STPTR)
		STARTX = GRAPHBUF(2,STPTR)
		IF  (GRAPHBUF(1,PTR-1) .NE. STARTY .OR.
     +	             GRAPHBUF(2,PTR-1) .NE. STARTX) THEN
		    GRAPHBUF(1,PTR) = STARTY
		    GRAPHBUF(2,PTR) = STARTX
		    CALL DISPLAYPOLYGON (GRAPHBUF(1,PTR-1), 2, 1)
		    PTR = PTR + 1
		ENDIF
		NUMPOLY = NUMPOLY + 1
		GRAPHLEN(NUMPOLY) = PTR - STPTR
		GRAPHPTR(NUMPOLY+1) = PTR
		IF (NUMPOLY .GE. MAXNUMPOLY-4) THEN
		    PRINT *, 'Out of space in graphics buffer.'
		    GOTO 299
		ENDIF
	    ENDIF
	  IF (SWITCH .EQ. 2) GOTO 180

299	  CONTINUE
	ENDIF


	RETURN
	END




	SUBROUTINE DO_COPY
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	POLYNUM, NUMSEGS, SWITCH, I, SS
	INTEGER	LINE1, SAMP1, LINE2, SAMP2, DELL, DELS
	INTEGER	LINE(MAXSEGS), SSAMP(MAXSEGS), NSAMP(MAXSEGS)
	INTEGER  CLINE, CSAMP, IMP
	BYTE	   BUFFER(MAXMEMSAMPS)
	INTEGER	   IERR, XDCLOCATION
	INTEGER	   XDILINEREAD, XDILINEWRITE


	IF (IGRAPH .LE. 0) THEN
	    PRINT *,'Graphics plane not available.  Command inoperative.'
	    RETURN
	ENDIF
	CALL RESETSWITCHES

C			Have the user select which polygon
	CALL SELECTPOLYGON (POLYNUM, LINE1, SAMP1)
	IF (POLYNUM .EQ. 0)  RETURN

	SWITCH = 1
	DO WHILE (SWITCH .EQ. 1)
	    PRINT *,
     +'Select point to copy to with switch 1.  Hit switch 2 to end.'
C			Wait until a switch is depressed
	    CALL SWITCHWAIT (0, SWITCH)
	    IF (SWITCH .EQ. 2) RETURN     ! return if switch 2

C			Get cursor location 
	    IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
	    IF ( IERR .ne. 1 )  RETURN

C			Convert the cursor position to image memory coords
	    LINE2 = (CLINE-1)/HZOOMFAC + SLMEM
	    SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM
	    DELL = LINE2 - LINE1
	    DELS = SAMP2 - SAMP1

C			Find the pieces of image lines in the polygon
	    CALL IMAGEINPOLY (POLYNUM, 1, NUMSEGS, LINE, SSAMP, NSAMP)

C			Move those image lines
	    IF (DELL .LT. 0) THEN
		DO I = 1, NUMSEGS		! If moving up start at top
		    DO IMP = 1, NUMIMAGES
			IERR = XDILINEREAD (DISPU, IMP,SSAMP(I),LINE(I), 
     +				NSAMP(I), BUFFER)
			IF ( IERR .ne. 1 )  RETURN
			SS = SSAMP(I)+DELS
			IERR = XDILINEWRITE (DISPU, IMP, MAX(SS,1), 
     +				LINE(I)+DELL, NSAMP(I)-MAX(1-SS,0), 
     +				BUFFER(MAX(2-SS,1)))
			IF ( IERR .ne. 1 )  RETURN
		    ENDDO
		ENDDO
	    ELSE
	       DO I = NUMSEGS, 1, -1	! If moving down start at bottom
		  DO IMP = 1, NUMIMAGES
		     IERR = XDILINEREAD (DISPU,IMP,SSAMP(I),LINE(I), 
     +				NSAMP(I), BUFFER)
		     IF ( IERR .ne. 1 )  RETURN
		     SS = SSAMP(I)+DELS
		     IERR=XDILINEWRITE(DISPU,IMP,MAX(SS,1),LINE(I)+DELL, 
     +		          NSAMP(I)-MAX(1-SS,0), BUFFER(MAX(2-SS,1)))
		     IF ( IERR .ne. 1 )  RETURN
		  ENDDO
	       ENDDO
	    ENDIF

	    CHANGEDMEM = .TRUE.
	ENDDO

	RETURN
	END


	SUBROUTINE DO_FILL
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	POLYNUM, LINE1, SAMP1, NUMSEGS, FLAG, TRANBUF1(12)
	INTEGER	I, L, SS, NS, ES, S, COL, TRANBUF2(12), STAT
	INTEGER	MAXRAD, NUMPTS, PTR, INOUT
	INTEGER  IMP, IS, IL
        INTEGER*2 INTBUF(3,MAXSEGS), LINEBUF(MAXMEMSAMPS)
	INTEGER	LINE(MAXSEGS), SSAMP(MAXSEGS), NSAMP(MAXSEGS)
        INTEGER*2 DN
	REAL	DENS
	BYTE	BUFFER(MAXMEMSAMPS), COLBUFFER(MAXMEMSAMPS,3), DNB(2)
C	EQUIVALENCE (DN,DNB)
	INTEGER	   IERR, XDILINEWRITE, XDIPIXELREAD, XDILINEREAD
	LOGICAL	   keytest, SOLIDFILL


        CALL XVTRANS_SET(TRANBUF1,'BYTE','HALF',STAT)
        IF(STAT.NE.1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')
        CALL XVTRANS_SET(TRANBUF2,'HALF','BYTE',STAT)
        IF(STAT.NE.1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')

	IF (IGRAPH .LE. 0) THEN
	    PRINT *,'Graphics plane not available.  Command inoperative.'
	    RETURN
	ENDIF
	CALL RESETSWITCHES

C			Have the user select which polygon
	CALL SELECTPOLYGON (POLYNUM, LINE1, SAMP1)
	IF (POLYNUM .EQ. 0)  RETURN


C			Find the pieces of image lines in the polygon
	INOUT = 1
	IF (keytest ('OUTSIDE'))  INOUT = 0
	CALL IMAGEINPOLY (POLYNUM, INOUT, NUMSEGS, LINE, SSAMP, NSAMP)


	IF (keytest('INTERPOLATE')) THEN
C			Interpolate over polygon
	    CALL GET_KEYWORD_INTEGER ('INTERPOLATE', 1,1000,FLAG,MAXRAD)
	    MAXRAD = MAXRAD**2
	    NUMPTS = GRAPHLEN(POLYNUM)
	    PTR = GRAPHPTR(POLYNUM)-1
	    DO IMP = 1, NUMIMAGES
		DO I = 1, NUMPTS
		    INTBUF(1,I) = NINT(GRAPHBUF(2,PTR+I)) - SSFILE + 1
		    INTBUF(2,I) = NINT(GRAPHBUF(1,PTR+I)) - SLFILE + 1
		    INTBUF(3,I) = 0
                    IS=INTBUF(1,I)
                    IL=INTBUF(2,I)
		    IERR = XDIPIXELREAD (DISPU, IMP, IS, IL, DNB(1) )
                    CALL XVTRANS(TRANBUF1,DNB,DN,1)
                    INTBUF(3,I) = DN
		    IF ( IERR .ne. 1 )  RETURN
		ENDDO
		DO I = 1, NUMSEGS
		    L = LINE(I)
		    SS = SSAMP(I)
		    NS = NSAMP(I)
		    ES = SS + NS - 1
		    CALL EXTRAP(NUMPTS, L,SS,ES, INTBUF, LINEBUF,MAXRAD)
		    CALL MVE (-3, NS, LINEBUF, BUFFER,1,1)
		    IERR = XDILINEWRITE (DISPU, IMP, SS, L, NS, BUFFER)
		    IF ( IERR .ne. 1 )  RETURN
		ENDDO
	    ENDDO


	ELSE


	    CALL GET_KEYWORD_REAL ('TRANSPAR', 1, 1.0, FLAG, DENS)
	    SOLIDFILL = (DENS .GE. 1.0)

	    IF (SOLIDFILL) THEN
C		Fill the area with a solid color
C			Get a line of the current color
		DO IMP = 1, NUMIMAGES
		    DN = CURCOLOR(IMP)
                    CALL XVTRANS(TRANBUF2,DN,DNB,1)
		    DO I = 1, NSMEM
			COLBUFFER(I,IMP) =  DNB(1)
		    ENDDO
		ENDDO
C			Fill the image lines
		DO I = 1, NUMSEGS
		    L = LINE(I)
		    SS = SSAMP(I)
		    NS = NSAMP(I)
		    DO IMP = 1, NUMIMAGES
			IERR = XDILINEWRITE (DISPU, IMP, SS, L, NS, 
     +						COLBUFFER(1,IMP))
			IF ( IERR .ne. 1 )  RETURN
		    ENDDO
		ENDDO

	    ELSE

		DENS = MIN (MAX (DENS, 0.0), 1.0)
		DO I = 1, NUMSEGS
		    L = LINE(I)
		    SS = SSAMP(I)
		    NS = NSAMP(I)
		    DO IMP = 1, NUMIMAGES
			IERR = XDILINEREAD (DISPU, IMP, SS, L,NS,BUFFER)
			IF ( IERR .ne. 1 )  RETURN
			COL = CURCOLOR(IMP)
			DO S = 1, NS
			    DNB(1) = BUFFER(S)
			    DNB(2) = 0
                            CALL XVTRANS(TRANBUF1,DNB,DN,1)
			    DN = NINT(DENS*COL + (1.-DENS)*DN)
                            CALL XVTRANS(TRANBUF2,DN,DNB,1)
			    BUFFER(S) = DNB(1)
			ENDDO
			IERR = XDILINEWRITE (DISPU, IMP, SS,L,NS,BUFFER)
			IF ( IERR .ne. 1 )  RETURN
		    ENDDO
		ENDDO

	    ENDIF

	ENDIF

	CHANGEDMEM = .TRUE.

	RETURN
	END




	SUBROUTINE DO_STATISTICS
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER	POLYNUM, LINE1, SAMP1, NUMSEGS
	INTEGER	I, L, S, SS, NS, NPIX, NP, IMP, DN
	INTEGER	ISUM, ISUMSQ, MINDN, MAXDN, MINVAL(3), MAXVAL(3)
	INTEGER	LINE(MAXSEGS), SSAMP(MAXSEGS), NSAMP(MAXSEGS)
	REAL	SUM(3), SUMSQ(3), MEAN(3), STDDEV(3)
	BYTE	BUFFER(MAXMEMSAMPS)
	INTEGER	IERR, XDILINEREAD
        CHARACTER*80 MSG
C	EQUIVALENCE (DN,DNB)


	IF (IGRAPH .LE. 0) THEN
	    PRINT *,'Graphics plane not available.  Command inoperative.'
	    RETURN
	ENDIF
	CALL RESETSWITCHES

C			Have the user select which polygon
	CALL SELECTPOLYGON (POLYNUM, LINE1, SAMP1)
	IF (POLYNUM .EQ. 0)  RETURN

C			Find the pieces of image lines in the polygon
	CALL IMAGEINPOLY (POLYNUM, 1, NUMSEGS, LINE, SSAMP, NSAMP)

C			Do statistics on the image lines
	NPIX = 0
	DO IMP = 1, NUMIMAGES
	    SUM(IMP) = 0.0
	    SUMSQ(IMP) = 0.0
	    MINVAL(IMP) = 100000
	    MAXVAL(IMP) = -100000
	ENDDO
	DO I = 1, NUMSEGS
	    L = LINE(I)
	    SS = SSAMP(I)
	    NS = NSAMP(I)
	    DO IMP = 1, NUMIMAGES
		IERR = XDILINEREAD (DISPU, IMP, SS, L, NS, BUFFER)
		IF ( IERR .ne. 1 )  RETURN
		ISUM = 0
		ISUMSQ = 0
		MINDN = MINVAL(IMP) 
		MAXDN = MAXVAL(IMP) 
		DO S = 1, NS
C		    DNB(1) = BUFFER(S)
C		    DNB(2) = 0
                    DN = BYTE2INT(BUFFER(S))
		    ISUM = ISUM + DN
		    ISUMSQ = ISUMSQ + DN**2
		    MINDN = MIN (MINDN, DN)
		    MAXDN = MAX (MAXDN, DN)
		ENDDO
		SUM(IMP) = SUM(IMP) + ISUM
		SUMSQ(IMP) = SUMSQ(IMP) + ISUMSQ
		MINVAL(IMP) = MINDN
		MAXVAL(IMP) = MAXDN
	    ENDDO
	    NPIX = NPIX + NS
	ENDDO
	NP = NUMIMAGES
	DO IMP = 1, NP
	    MEAN(IMP) = SUM(IMP)/FLOAT(NPIX)
	    STDDEV(IMP) = SQRT(SUMSQ(IMP)/FLOAT(NPIX) - MEAN(IMP)**2)
	ENDDO
	WRITE (MSG,'(1X,A,I7)') 'Number of pixels:', NPIX
        CALL XVMESSAGE (MSG,' ')
	MSG='Average:'
        DO I=1,NP
	   WRITE (MSG(12*NP:(12*NP)+11),'(F11.3)') MEAN(I)
        ENDDO
        CALL XVMESSAGE (MSG,' ')
        MSG='Std dev:'
        DO I=1,NP
	   WRITE (MSG(12*NP:(12*NP)+11),'(F11.3)') STDDEV(I)
        ENDDO
        CALL XVMESSAGE (MSG,' ')
        MSG='Min DN: '
        DO I=1,NP
	   WRITE (MSG(12*NP:(12*NP)+11),'(I11)') MINVAL(I)
        ENDDO
        CALL XVMESSAGE (MSG,' ')
        MSG='Max DN: '
        DO I=1,NP
	   WRITE (MSG(12*NP:(12*NP)+11),'(I11)') MAXVAL(I)
        ENDDO
        CALL XVMESSAGE (MSG,' ')
	RETURN
	END




	SUBROUTINE DO_TEXTURE
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INCLUDE 'fortport'
	INTEGER  DN(3), CLINE, CSAMP
	INTEGER	FLAG, AVGFLAG, STDFLAG
	INTEGER	POLY1, POLY2
	INTEGER	LINE1, SAMP1, LINE2, SAMP2
	INTEGER	SL, SS, NL, NS, NLFFT, NSFFT, NLPAT, NSPAT
	INTEGER	I, J, K, L, S, IMP
	INTEGER	NP, NPIX, NUMSEGS, SWITCH
	INTEGER	LINE(MAXSEGS), SSAMP(MAXSEGS), NSAMP(MAXSEGS)
	INTEGER	FILTSIZE, HIPASS, SEED, DNOUT
	REAL	POLYSIZE(4)
	REAL	SUM(3), SUMSQ(3), AVG(3), STD(3)
	REAL	NORM, HIPASSL, HIPASSS, FIL
	REAL	GAUSNOIS, TMPSECS
	REAL	SUM2, AVGTEXT, STDTEXT, SCALE, OFFSET
	BYTE	   BUFFER(MAXMEMSAMPS,3)
	INTEGER*2  TEMPBUF(MAXMEMSAMPS,3)
	REAL	   PATBUF(MAXIMAGEBUF), TEXTBUF(MAXIMAGEBUF)
	INTEGER	IERR, XDILINEREAD, XDILINEWRITE, XDCLOCATION, STATUS



	IF (IGRAPH .LE. 0) THEN
	    PRINT *,'Graphics plane not available.  Command inoperative.'
	    RETURN
	ENDIF
	CALL RESETSWITCHES

C			Have the user select which polygon
	PRINT *,'Select polygon to texture.'
	CALL SELECTPOLYGON (POLY1, LINE1, SAMP1)
	IF (POLY1 .EQ. 0)  RETURN
C			Find the bounding rectangle for the polygon
	CALL MAKEPOLYSIZE (GRAPHBUF(1,GRAPHPTR(POLY1)), 
     +					GRAPHLEN(POLY1), POLYSIZE)
	SL = MAX (NINT(POLYSIZE(1)) - SLFILE, 1) 
	SS = MAX (NINT(POLYSIZE(3)) - SSFILE, 1)
	NL = MIN (NINT(POLYSIZE(2)) - SLFILE + 2, NLMEM) - SL + 1
	NS = MIN (NINT(POLYSIZE(4)) - SSFILE + 2, NSMEM) - SS + 1
	NLFFT = 2**( INT(LOG(NL-1.)/LOG(2.)) + 1 )
	NSFFT = 2**( INT(LOG(NS-1.)/LOG(2.)) + 1 )
	IF ((NLFFT+2)*NSFFT .GT. MAXIMAGEBUF) THEN
	    PRINT *,'Too large a region to texture.'
	    RETURN
	ENDIF


C		Get color statistics

	NP = NUMIMAGES
	DO I = 1, NP
	    AVG(I) = CURCOLOR(I)
	    STD(I) = 20.0
	ENDDO
	CALL GET_KEYWORD_REAL ('AVG', 3, AVG, AVGFLAG, AVG)
	CALL GET_KEYWORD_REAL ('STD', 3, STD, STDFLAG, STD)

	IF (AVGFLAG .GE. 1 .OR. STDFLAG .GE. 1) THEN
	    DO I = 2, NP
		IF (AVGFLAG .EQ. 1)  AVG(I) = AVG(1)
		IF (STDFLAG .EQ. 1)  STD(I) = STD(1)
	    ENDDO

	ELSE
	    PRINT *,' '
	    PRINT *,'Select polygon for color statistics.'
	    CALL SELECTPOLYGON (POLY2, LINE1, SAMP1)
	    IF (POLY2 .EQ. 0)  RETURN

C			Find the pieces of image lines in the polygon
	    CALL IMAGEINPOLY (POLY2, 1, NUMSEGS, LINE, SSAMP, NSAMP)

C			Do statistics on the image lines
	    NPIX = 0
	    DO I = 1, NP
		SUM(I) = 0.0
		SUMSQ(I) = 0.0
	    ENDDO
	    DO K = 1, NUMSEGS
		DO IMP = 1, NP
		    IERR = XDILINEREAD (DISPU, IMP, SSAMP(K), LINE(K), 
     +					NSAMP(K), BUFFER(1,IMP))
		    IF ( IERR .ne. 1 )  RETURN
		ENDDO
		DO I = 1, NP
		    DO S = 1, NSAMP(K)
			DN(I) = BYTE2INT(BUFFER(S,I))
			SUM(I) = SUM(I) + DN(I)
			SUMSQ(I) = SUMSQ(I) + FLOAT(DN(I))**2
		    ENDDO
		ENDDO
		NPIX = NPIX + NSAMP(K)
	    ENDDO
	    DO I = 1, NP
		AVG(I) = SUM(I)/NPIX
		STD(I) = SQRT(SUMSQ(I)/NPIX - AVG(I)**2)
	    ENDDO
	ENDIF



C			Get user defined pattern rectangle
	PRINT *,' '
100	CONTINUE
	PRINT *,'Select texture pattern rectangle.'

	PRINT *,'Select first corner with switch 1'
	CALL SWITCHWAIT (1, SWITCH)
	IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)	! Get cursor location
	IF ( IERR .ne. 1 )  RETURN
	LINE1 = (CLINE-1)/HZOOMFAC + SLMEM	! Convert to image mem coords
	SAMP1 = (CSAMP-1)/HZOOMFAC + SSMEM

	PRINT *,'Select second corner with switch 1'
	CALL SWITCHWAIT (1, SWITCH)
	IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)	! Get cursor location
	IF ( IERR .ne. 1 )  RETURN
	LINE2 = (CLINE-1)/HZOOMFAC + SLMEM	! Convert to image mem coords
	SAMP2 = (CSAMP-1)/HZOOMFAC + SSMEM

	CALL ORDER (LINE1, LINE2)
	CALL ORDER (SAMP1, SAMP2)
	NLPAT = MIN( MAX(LINE2 - LINE1 + 1, 8), 128)
	NSPAT = MIN( MAX(SAMP2 - SAMP1 + 1, 8), 128)
	NLPAT = 2**( INT(LOG(FLOAT(NLPAT)) /LOG(2.)) )
	NSPAT = 2**( INT(LOG(FLOAT(NSPAT)) /LOG(2.)) )

					! Display the rectangle
	CALL DISPLAYBOX (LINE1, SAMP1, NLPAT, NSPAT, 2)

	PRINT *,'Hit switch 1 to accept rectangle,  switch 2 to reject'
	CALL SWITCHWAIT (0, SWITCH)

					! Erase the rectangle
	CALL DISPLAYBOX (LINE1, SAMP1, NLPAT, NSPAT, 0)
	IF (SWITCH .EQ. 2) GOTO 100	    ! if switch 2 then try again


C			Read in pattern rectangle
	K = 1
	DO L = LINE1, LINE1+NLPAT-1
	    DO IMP = 1, NUMIMAGES
		IERR = XDILINEREAD (DISPU, IMP, SAMP1, L, NSPAT, 
     +					BUFFER(1,IMP))
		IF ( IERR .ne. 1 )  RETURN
		CALL MVE (3, NSPAT, BUFFER(1,IMP), TEMPBUF(1,IMP),1,1)
	    ENDDO
	    IF (NUMIMAGES .EQ. 1) THEN
		DO S = 1, NSPAT
		    PATBUF(K) = TEMPBUF(S,1)
		    K = K + 1
		ENDDO
	    ELSE			! If color then average 3 planes
		DO S = 1, NSPAT
		   PATBUF(K)=(TEMPBUF(S,1)+TEMPBUF(S,2)+TEMPBUF(S,3))/3.
		   K = K + 1
		ENDDO
	    ENDIF
	ENDDO

C			FFT the pattern rectangle
	CALL RFT2 (PATBUF, NLPAT, NSPAT, 1, STATUS)
        IF (STATUS.NE.1) CALL MABEND('ERROR RETURNED BY RFT2')
	NORM = 1./(NSPAT*NLPAT)
	K = 1
	DO I = 1, NLPAT+1, 2	! Zero the phase of the complex numbers
	    L = K + NSPAT
	    DO J = 1, NSPAT
		PATBUF(K) = NORM*SQRT(PATBUF(K)**2 + PATBUF(L)**2)
		PATBUF(L) = 0.0
		K = K + 1
		L = L + 1
	    ENDDO
	    K = K + NSPAT
	ENDDO
	PATBUF(1) = 0.0
C			FFT back to get the filter
	CALL RFT2 (PATBUF, NLPAT, NSPAT, -1, STATUS)
        IF (STATUS.NE.1) CALL MABEND('ERROR RETURNED BY RFT2')


	FILTSIZE = MIN(NLPAT,NSPAT)
	CALL GET_KEYWORD_INTEGER ('SIZE', 1, 16, FLAG, FILTSIZE)
C			Take inner portion of filter
	K = 1
	DO I = 1, NLFFT
	    DO J = 1, NSFFT
		IF (I .LE. FILTSIZE .AND. J .LE. FILTSIZE) THEN
		    TEXTBUF(K) = PATBUF((I-1)*NSPAT+J)
		ELSE
		    TEXTBUF(K) = 0.0
		ENDIF
		K = K + 1
	    ENDDO
	ENDDO
	DO K = 1, NLFFT*NSFFT		! Bring back from temporary buffer
	    PATBUF(K) = TEXTBUF(K)
	ENDDO
C			FFT filter to Fourier domain
	CALL RFT2 (PATBUF, NLFFT, NSFFT, 1, STATUS)
        IF (STATUS.NE.1) CALL MABEND('ERROR RETURNED BY RFT2')

C			Zero out axes in Fourier domain
	DO K = 1, NSFFT
	    PATBUF(K) = 0.0
	    PATBUF(K+NSFFT) = 0.0
	ENDDO
	K = 1
	DO I = 0, NLFFT/2
	    PATBUF(K) = 0.0
	    PATBUF(K+NSFFT) = 0.0
	    K = K + 2*NSFFT
	ENDDO

C			Make filter high pass if desired
	CALL GET_KEYWORD_INTEGER ('HIPASS', 1, 8, FLAG, HIPASS)
	IF (FLAG .GE. 0) THEN
	    HIPASSL = NSFFT/FLOAT(HIPASS)
	    HIPASSS = NLFFT/FLOAT(HIPASS)
	    K = 1
	    DO I = 0, NLFFT/2
		DO J = 0, NSFFT-1
		    FIL = 1.0-EXP(-(0.5*(I/HIPASSL)**2+(J/HIPASSS)**2))
		    PATBUF(K) = FIL*PATBUF(K)
		    PATBUF(K+NSFFT) = FIL*PATBUF(K+NSFFT) 
		    K = K + 1
		ENDDO
		K = K + NSFFT
	    ENDDO
	ENDIF




C			Get polygon image line segments for write out
	CALL IMAGEINPOLY (POLY1, 1, NUMSEGS, LINE, SSAMP, NSAMP)


C			Generate the Gaussian noise in the Fourier domain
        CALL GET_SECONDS(TMPSECS)
	SEED = NINT(FLOAT(3979)*TMPSECS)
	NPIX = NLFFT*NSFFT

	DO K = 1, (NLFFT+2)*NSFFT
	    TEXTBUF(K) = GAUSNOIS(SEED)
	ENDDO
	DO J = 0, NPIX, NPIX	! Make the noise complex congjugate symmetric
	    DO I = 2, NSFFT/2	!     for RFT2
		TEXTBUF(J+NSFFT+2-I) = TEXTBUF(J+I)
		TEXTBUF(J+NSFFT+2-I+NSFFT) = -TEXTBUF(J+I+NSFFT)
	    ENDDO
	    TEXTBUF(J+1+NSFFT) = 0.0
	    TEXTBUF(J+1+NSFFT/2+NSFFT) = 0.0
	ENDDO


C				Multiply the noise by the filter 
	K = 1
	DO I = 1, NLFFT+1, 2
	    L = K + NSFFT
	    DO J = 1, NSFFT
		TEXTBUF(K) = PATBUF(K)*TEXTBUF(K) - PATBUF(L)*TEXTBUF(L)
		TEXTBUF(L) = PATBUF(K)*TEXTBUF(L) + PATBUF(L)*TEXTBUF(K)
		K = K + 1
		L = L + 1
	    ENDDO
	    K = K + NSFFT
	ENDDO
	AVGTEXT = TEXTBUF(1)/NPIX
	CALL RFT2 (TEXTBUF, NLFFT, NSFFT, -1, STATUS)! FFT back to get texture

	SUM2 = 0.0
	DO I = 1, NPIX
	    SUM2 = SUM2 + TEXTBUF(I)**2
	ENDDO
	STDTEXT = SQRT(SUM2/NPIX-AVGTEXT**2)

	DO IMP = 1, NUMIMAGES
C			Scale and offset values to get right mean and variance
	    SCALE = 0.0
	    IF (STDTEXT .GT. 1.0E-30)  SCALE = STD(IMP) / STDTEXT
	    OFFSET = AVG(IMP) - SCALE*AVGTEXT

C			Convert format and output to image memory
	    DO I = 1, NUMSEGS
		K = (LINE(I)-SL)*NSFFT + SSAMP(I)-SS+1
		DO J = 1, NSAMP(I)
		    DNOUT = NINT(SCALE*TEXTBUF(K) + OFFSET)
		    TEMPBUF(J,1) = MAX( MIN( DNOUT, 255), 0)
		    K = K + 1
		ENDDO
		CALL MVE (-3, NSAMP(I), TEMPBUF, BUFFER,1,1)
		IERR = XDILINEWRITE (DISPU, IMP, SSAMP(I), LINE(I), 
     +				NSAMP(I), BUFFER)
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	ENDDO

	CHANGEDMEM = .TRUE.

	RETURN
	END





	SUBROUTINE DO_HELP
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	CHARACTER*(MAXTOKLEN)  COMMAND


	COMMAND = TOKENSTR(2)
      IF (COMMAND(1:7) .EQ. 'GENERAL') THEN
	PRINT *,'Commands are of the form:'
	PRINT *,'  COMMAND KEYWORD=VALUE1,VALUE2   KEYWORD VALUE '
	PRINT *,
     +    'Commands and keywords may be abbreviated and may be in upper'
	PRINT *,
     +    'or lower case.  Most any character can serve as a delimiter.'
	PRINT *,
     +        'Some keywords take multiple values others take no value.'
	PRINT *,
     +       'Often the command serves as the keyword (STRETCH 50 200).'
      ELSE IF (COMMAND(1:5) .EQ. 'PAINT') THEN
	PRINT *,'  PAINT puts the program in painting mode.'
	PRINT *,'In paint mode the brush (marked by the cursor) is moved'
	PRINT *,'with the trackball.  When the switch 1 is held down the'
	PRINT *,
     +       'brush paints the image.  The painting involves a weighted'
	PRINT *,
     +       'average between the current color and the pixel values in'
	PRINT *,
     +     'the image.  The painting density (weight) may be changed by'
	PRINT *,'giving a new value after the PAINT command.  Some brush'
	PRINT *,
     +      'types have variable painting densities over their surface.'
	PRINT *,'A new current color may be selected inside paint mode'
	PRINT *,'by finding a pixel with the desired color and pressing'
	PRINT *,'switch 2.  Hit switch 3 to leave paint mode.'
	PRINT *,'See COLOR and BRUSH for related information.'
      ELSE IF (COMMAND(1:4) .EQ. 'DRAW') THEN
	PRINT *,' DRAW draws line graphics of the current color into the'
	PRINT *,'image memory planes.  There are several ways of drawing'
	PRINT *,'the graphics:'
	PRINT *,' default    draws the line as the cursor is moved'
	PRINT *,' LINE       draws segments between endpoints'
	PRINT *,' BOX        draws a rectangle from two corners'
	PRINT *,' FILE n     draws graphics from input file n'
	PRINT *,' '
	PRINT *,'  The graphics lines drawn may have variable thickness:'
	PRINT *,' WIDTH w    draws lines with a width of w pixels'
      ELSE IF (COMMAND(1:7) .EQ. 'TEXTURE') THEN
	PRINT *,'  TEXTURE textures a polygonal image area using another'
	PRINT *,'area as a texture pattern.'
	PRINT *,'First, the polygon to texture is selected.  Second, the'
	PRINT *,'color statistics polygon is selected unless the AVG or'
	PRINT *,'STD keywords are given.  The color polygon is used to'
	PRINT *,'provide the color average and variance for the textured'
	PRINT *,'area.  Finally, the texture pattern region is selected'
	PRINT *,'by selecting two corners of a rectangle.  The rectangle'
	PRINT *,
     +       'is forced to be a power of two in size.  The rectangle is'
	PRINT *,
     +      'highlighted, and the user may pick another one if desired.'
	PRINT *,
     +       '  The texture pattern rectangle is used to make a filter.'
	PRINT *,
     +        'The filter is then convolved with gaussian noise to make'
	PRINT *,'the textured region.  A high pass filter is used to cut'
	PRINT *,
     +        'out the low spatial frequencies and make a flat textured'
	PRINT *,'image.'
	PRINT *,' Keywords:'
	PRINT *,'  AVG      the average DN value for each color band'
	PRINT *,'  STD      the standard deviation for the color bands'
	PRINT *,'  SIZE     size in pixels of the filter (default=16)'
	PRINT *,
     +      '  HIPASS   cutoff size in pixels for the high pass (def=8)'
      ELSE IF (COMMAND(1:4) .EQ. 'TEXT') THEN
	PRINT *,
     +       '  TEXT sets the text parameters and draws text characters'
	PRINT *,
     +        'in the image memory planes.  The text parameters stay in'
	PRINT *,'effect until changed.  If a text string is given (with'
	PRINT *,
     +       'parameter STRING) then a cursor position is prompted for,'
	PRINT *,
     +       'and the text is drawn on the graphics plane.  If the text'
	PRINT *,
     +       'is accepted it is then burned into the image memory using'
	PRINT *,'the current color.'
	PRINT *,' Parameters:'
	PRINT *,' STRING "the text string" '
	PRINT *,
     +    ' STRING    parameter with no value uses the last text string'
	PRINT *,
     +        ' SIZE  h   height of the characters in pixels (def = 10)'
	PRINT *,
     +      ' SCALE s   horizontal scale factor for text size (def = 1)'
	PRINT *,
     +        ' ANGLE a   the angle from the horizontal axis in degrees'
	PRINT *,
     +     ' FONT  f   the font number (see help file for descriptions)'
	PRINT *,' LEFT CENTER RIGHT  keywords for text justification'
      ELSE IF (COMMAND(1:7) .EQ. 'DISPLAY') THEN
	PRINT *,'  DISPLAY saves the current image memory planes in the'
	PRINT *,'file and displays the new region of the image.'
	PRINT *,'The default region is as much of the upper left corner'
	PRINT *,'of the image as fits in the image memory planes.'
	PRINT *,' '
	PRINT *,'The following keywords select the region to display:'
	PRINT *,'  SL, SS  . . the starting line and sample in the image'
	PRINT *,'  NL, NS  . . the number of lines and samples'
	PRINT *,' '
	PRINT *,'  The NOSAVE keyword disables saving the current region'
	PRINT *,'before displaying the new region.'
      ELSE IF (COMMAND(1:7) .EQ. 'STRETCH') THEN
	PRINT *,'  STRETCH performs contrast stretches on the display.'
	PRINT *,'The stretch is performed by changing the look up tables'
	PRINT *,'without changing the actual pixel values.'
	PRINT *,'The stretch may be performed using various keywords:'
	PRINT *,'  STRET n1 n2      performs a linear gray scale stretch'
	PRINT *,'                   between DN values n1 and n2'
	PRINT *,
     .  '  STRET RED n1 n2  performs linear stretch on the red look up'
	PRINT *,'                   table.  Similarly for GREEN and BLUE'
	PRINT *,'STRETCH with nothing else will perform a trackball'
	PRINT *,'controlled linear stretch.  The vertical axis controls'
	PRINT *,
     +       'the contrast and the horizontal axis controls the offset.'
	PRINT *,'If the program is in pseudo color mode then stretch'
	PRINT *,'performs a contrast stretch on the pseudo color table.'
      ELSE IF (COMMAND(1:3) .EQ. 'PAN') THEN
	PRINT *,'  PAN puts the program in pan mode.'
	PRINT *,'In pan mode the trackball is used to select the region'
	PRINT *,
     +        'of the image memory plane which will be displayed on the'
	PRINT *,'monitor.  This is useful if the display is hardware'
	PRINT *,
     +        'zoomed or if the image memory planes are larger than the'
	PRINT *,'display.  Pan mode is exited by pressing switch 1.'
      ELSE IF (COMMAND(1:5) .EQ. 'HZOOM') THEN
	PRINT *,'  HZOOM performs a hardware zoom of the display.'
	PRINT *,'If no value follows the command then the zoom factor is'
	PRINT *,'doubled from its previous value, otherwise the zoom'
	PRINT *,'factor is set to the new value.'
      ELSE IF (COMMAND(1:5) .EQ. 'COLOR') THEN
	PRINT *,'  COLOR changes the current color.'
	PRINT *,
     +   'COLOR n   selects the nth palette color as the current color.'
	PRINT *,
     +       'The current color is changed with the following keywords:'
	PRINT *,'  DN      sets the color to given gray scale value'
	PRINT *,'  RGB     sets the color to the given red,'
        PRINT *,'          green, and blue values'
	PRINT *,'  CURSOR  sets the color to that of a selected pixel'
        PRINT *,'          on the display'
	PRINT *,'  AVERAGE sets the color to the average under the brush'
        PRINT *,'          on the display'
	PRINT *,'  COMPOSE enters the interactive color composing mode'
	PRINT *,'  '
	PRINT *,'Example:  COLOR 3  RGB 240 50 200'
	PRINT *,'Sets the current color to the particular RGB value and'
	PRINT *,'Sets palette color three to this color.'
	PRINT *,'If no color number is specified then the current color'
	PRINT *,'is changed, but the color table is not.'
      ELSE IF (COMMAND(1:7) .EQ. 'PALETTE') THEN
	PRINT *,'  PALETTE controls the display of the palette.'
	PRINT *,'The palette displays the first 16 colors of the color'
	PRINT *,'table at the bottom of the screen.'
	PRINT *,' PALETTE       displays (or redisplays) the palette'
	PRINT *,' PALETTE OFF   erases the palette'
	PRINT *,
     +        'Zooming or panning will move the palette from the bottom'
	PRINT *,'of the display;  the PALETTE command will redisplay it'
	PRINT *,'in the correct place.'
	PRINT *,'  The palette may be saved to and loaded from a file:'
	PRINT *,' PALETTE SAVE=file   saves palette colors in file'
	PRINT *,' PALETTE LOAD=file   loads palette colors from file'
	PRINT *,'                     The default file is EDIMAGE.PAL'
	PRINT *,'The palette file is an ascii table of the 32 colors in'
	PRINT *,'the palette; the color number, red value, green value,'
	PRINT *,'and blue value are stored in each row.'
      ELSE IF (COMMAND(1:5) .EQ. 'BRUSH') THEN
	PRINT *,'  BRUSH sets the size and type of the brush.'
	PRINT *,
     +        'The SIZE parameter selects the brush diameter in pixels.'
	PRINT *,'The TYPE parameter selects the brush type.  Valid brush'
	PRINT *,'types are: DISK, SQUARE, DOME, GAUSSIAN.'
	PRINT *,'If a parameter is not given then the old value will be'
	PRINT *,'used.  The brush size and type are displayed.'
      ELSE IF (COMMAND(1:6) .EQ. 'PSEUDO') THEN
	PRINT *,
     +      '  PSEUDO changes the look up tables for pseudo color mode.'
	PRINT *,'It enters pseudo color mode if in gray scale mode.'
	PRINT *,
     +     ' OFF        turns off pseudo color, returning to gray scale'
	PRINT *,
     +    ' TABLE n    selects the nth pseudo color table (same as IDX)'
	PRINT *,
     +  ' DN n1 n2   sets the DN value range to change with RGB or COMP'
	PRINT *,
     +     ' RGB r g b  sets the DN range to given red, green, and blue'
	PRINT *,' COMPOSE    enters interactive color composing mode'
	PRINT *,'Note: PSEUDO does not change the palette colors or the'
	PRINT *,'current color, only the look up tables.'
      ELSE IF (COMMAND(1:7) .EQ. 'POLYGON') THEN
	PRINT *,
     +        ' POLYGON draws graphics on the graphics plane and stores'
	PRINT *,
     +        'the graphics internally for later use.  The graphics may'
	PRINT *,
     +       'be generated interactively or from an IBIS graphics file.'
	PRINT *,'The graphics are in image file coordinates.'
	PRINT *,'  Subcommands:'
	PRINT *,'no keyword   enter interactive mode'
	PRINT *,'BOX [parm]   specify a rectangle from two corners'
	PRINT *,'             Optional keyword SIZE SL SS NL NS'
	PRINT *,
     +      '             specifies rectangle of standard SIZE in image'
	PRINT *,'READ n       reads graphics from input file n'
	PRINT *,'WRITE n      writes graphics buffer to output file n'
	PRINT *,'CLEAR        clear graphics plane and graphics buffer'
	PRINT *,' '
	PRINT *,'  See also FILL, COPY, and STAT.'
      ELSE IF (COMMAND(1:4) .EQ. 'COPY') THEN
	PRINT *,'  COPY copies the image inside of a polygon to another'
	PRINT *,'region of the image memory plane.  The desired polygon'
	PRINT *,'is chosen by placing the cursor inside the polygon and'
	PRINT *,'pressing switch 1.  The polygon is highlighted and the'
	PRINT *,'user is asked for verification.  Another polygon may be'
	PRINT *,
     +        'chosen.  If the cursor is not in any polygon the command'
	PRINT *,
     +      'will not be performed.  The place to put the image area is'
	PRINT *,'chosen with another cursor selection.  The area will be'
	PRINT *,'moved the distance between the first and second cursor'
	PRINT *,'selections.  Multiple copies may be made.'
      ELSE IF (COMMAND(1:4) .EQ. 'FILL') THEN
	PRINT *,'  FILL fills the image area inside of a polygon.'
	PRINT *,'The desired polygon is chosen by placing the cursor'
	PRINT *,'inside the polygon and pressing switch 1.  '
	PRINT *,'The polygon is highlighted and the user is asked for'
	PRINT *,'verification.  Another polygon may be chosen.  If the'
	PRINT *,'cursor is not in any polygon the command will not be'
	PRINT *,'performed.'
	PRINT *,'There are three methods of filling:'
	PRINT *,'no keyword   The area is filled with the current color.'
	PRINT *,
     +      'TRANSPAR d   The area is filled with a weighted average of'
	PRINT *,
     +       '                the original image and the current color.'
	PRINT *,
     +   '                TRANS 1.0 is equivalent to the standard fill.'
	PRINT *,'INTERP  r    Interpolates the image area using the DN' 
	PRINT *,
     +       '                values at the polygon vertices.  r is the'
	PRINT *,'                maximum radius for a vertex to be used.'
	PRINT *,
     +        'OUTSIDE      Fills outside of polygon instead of inside.'
      ELSE IF (COMMAND(1:8) .EQ. 'STATISTI') THEN
	PRINT *,'  STATISTICS calculates simple statistics for the image'
	PRINT *,
     +        'area inside of a polygon.  The desired polygon is chosen'
	PRINT *,'by placing the cursor inside the polygon and pressing'
	PRINT *,
     +       'switch 1.  The number of pixels and the average, standard'
	PRINT *,'deviation, minimum, and maximum for each image plane is'
	PRINT *,'typed out.'
      ELSE IF (COMMAND(1:5) .EQ. 'TABLE') THEN
	PRINT *,'  TABLE saves and loads display look up tables on disk.'
	PRINT *,'The tables are in the same format that IDX and LOOKUP'
	PRINT *,'use.  Parameters:'
	PRINT *,
     +     ' SAVE f    saves the current look up table in output file f'
	PRINT *,' LOAD f    loads the look up table from input file f'
	PRINT *,' NUMBER t  table number t loaded from file (def is 1)'
	PRINT *,' '
      ELSE IF (COMMAND(1:4) .EQ. 'HELP') THEN
	PRINT *,'  HELP displays the online help information.'
	PRINT *,'See also the TAE help (e.g. in tutor mode).'
      ELSE IF (COMMAND(1:4) .EQ. 'EXIT') THEN
	PRINT *,'  EXIT exits the program.'
	PRINT *,'If the image memory planes have been changed, they are'
	PRINT *,'written back to the file.'
      ELSE IF (COMMAND(1:4) .EQ. 'QUIT') THEN
	PRINT *,
     +     '  QUIT exits the program without saving the current display'
	PRINT *,'screen.  Previous display screens will have already'
	PRINT *,'been saved, so the file may be changed.'
      ELSE IF (COMMAND(1:7) .EQ. 'COMMENT') THEN
	PRINT *,'  COMMENT is used to flag a command line as a comment.'
      ELSE
	PRINT *,'The following commands are available:'
	PRINT *,' DISPLAY  . . to display a region of the image'
	PRINT *,
     +        ' STRETCH  . . to perform contrast stretches on the image'
	PRINT *,' HZOOM    . . to perform hardware zooming'
	PRINT *,' PAN      . . to pan around the display'
	PRINT *,' TABLE    . . to load and save display look up tables'
	PRINT *,' PALETTE  . . to control display of the color palette'
	PRINT *,' PSEUDO   . . to enter pseudo color mode'
	PRINT *,' COLOR    . . to set the current color'
	PRINT *,' BRUSH    . . to set the current brush'
	PRINT *,' PAINT    . . to paint the image with the brush'
	PRINT *,' DRAW     . . to draw lines in the image'
	PRINT *,' TEXT     . . to put text into the image'
	PRINT *,' POLYGON  . . to generate polygon graphics'
	PRINT *,' COPY     . . to copy polygonal image areas'
	PRINT *,' FILL     . . to fill polygonal image areas'
	PRINT *,' STATIST  . . to calculate image statistics in polygons'
	PRINT *,' TEXTURE  . . to texture polygonal image areas'
	PRINT *,' HELP     . . to get help on various commands'
        PRINT *,
     .   ' COMMENT  . . to have a command line treated as a comment'
	PRINT *,' EXIT     . . to save image planes and exit'
	PRINT *,' QUIT     . . to exit without saving image planes'
	PRINT *,' GENERAL  . . for general purpose help'
	PRINT *,'Type: HELP command    for help on a particular command.'
      END IF

      RETURN
      END




C ********************************************************************
C                   COMMAND INPUT AND PARSING ROUTINES



	SUBROUTINE GET_COMD (CMD_STRING)
	IMPLICIT NONE
	CHARACTER*(*)	CMD_STRING
        INTEGER  ICOUNT, IDEF

        COMMON /TESTCMN/   TEST
        LOGICAL            TEST

C==================================================================

        IF (TEST)  THEN                ! TEST MODE

          CALL XVINTRACT('IPARAM','Command: ')   !wait for inter. params.
          CALL XVIPARM( 'COMMAND', CMD_STRING, ICOUNT, IDEF, 1)

        ELSE
10        CONTINUE
 	  WRITE (*,11) 'Command: '
11 	  FORMAT	(1X,A,$)

	  READ (*,'(A)',ERR=10,END=10) CMD_STRING

        END IF
	RETURN
	END



	SUBROUTINE PARSE_COMMAND (CMD_STRING, CMD_NAME)
C	    Parses the command string into a list of tokens.
C	 The token info is stored in the common, and consists of
C	 the number of tokens, the string for each token, the 
C	 token type (alpha or numeric), and the real token value if numeric.
C
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	CHARACTER*(*) CMD_STRING,  CMD_NAME

	INTEGER	I, J, NEWTYPE, LASTTYPE, SLEN, LINELEN
	BYTE	CH
	LOGICAL	STRINGMODE
	CHARACTER*(MAXTOKLEN)	TOK
	INTEGER	BLANK, ALPHA, NUMERIC
	PARAMETER (BLANK = 0, ALPHA = 1, NUMERIC = 2)


	DO I = 1, MAXTOKENS		! Zero the old tokens
	    TOKENSTR(I) = '   '
	    TOKENVALUE(I) = 0.0
	    TOKENTYPE(I) = BLANK
	ENDDO

	TOK = '                '
	J = 1
	STRINGMODE = .FALSE.
	NUMTOKENS = 0
	LASTTYPE = BLANK
	LINELEN = SLEN(CMD_STRING) + 1
	DO I = 1, LINELEN			! Loop thru each character
	    CH = ICHAR(CMD_STRING(I:I))
	    IF (.NOT.STRINGMODE) THEN		! Convert to upper case
		IF (CH .GE. ICHAR('a') .AND. CH .LE. ICHAR('z')) CH = CH - 32
	    ENDIF

C			Get the type of this character
	    IF (STRINGMODE) THEN	! If stringmode, everything is alpha
		IF (CH .EQ. ICHAR('"') .OR. I .EQ. LINELEN) THEN
		    STRINGMODE = .FALSE.
		    NEWTYPE = BLANK
		ELSE
		    NEWTYPE = ALPHA
		ENDIF
	    ELSE
		IF ((CH .GE. ICHAR('0') .AND. CH .LE. ICHAR('9')) .OR. 
     +		     CH .EQ. ICHAR('.') .OR. CH .EQ. ICHAR('+') .OR.
     +               CH .EQ. ICHAR('-')) THEN
		    NEWTYPE = NUMERIC
		ELSE IF (CH .GE. ICHAR('A') .AND. CH .LE. ICHAR('Z')) THEN
		    NEWTYPE = ALPHA
		ELSE IF (CH .EQ. ICHAR('"')) THEN
		    STRINGMODE = .TRUE.
		    NEWTYPE = BLANK
		ELSE
		    NEWTYPE = BLANK
		ENDIF
	    ENDIF


	    IF (NEWTYPE .NE. LASTTYPE) THEN
		IF (NEWTYPE .EQ. BLANK) THEN	! Store away the token
		   NUMTOKENS = NUMTOKENS + 1
		   TOKENSTR(NUMTOKENS) = TOK
		   TOKENTYPE(NUMTOKENS) = LASTTYPE
		   IF (LASTTYPE .EQ. NUMERIC) THEN
			READ (TOK, *, ERR=99)  TOKENVALUE(NUMTOKENS)
		   ENDIF
		   TOK = '                '
		   J = 1
		   LASTTYPE = NEWTYPE
		ELSE IF (LASTTYPE .EQ. BLANK) THEN
		   LASTTYPE = NEWTYPE
		ENDIF
	    ENDIF

	    IF (NEWTYPE .NE. BLANK .AND. J .LE. MAXTOKLEN) THEN
		TOK(J:J) = CHAR(CH)		! Build up the token string
		J = J + 1
	    ENDIF
	ENDDO


	IF (NUMTOKENS .GT. 0 .AND. TOKENTYPE(1) .EQ. ALPHA) THEN
	    CMD_NAME = TOKENSTR(1)
	ELSE
	    CMD_NAME = '        '
	ENDIF

	RETURN


99	CONTINUE		! Print error if bad numeric
	WRITE (*, '(1X,A,A)') 'Illegal number: ', TOK
	NUMTOKENS = 0
	CMD_NAME = '        '

	RETURN
	END




	SUBROUTINE GET_KEYWORD_INTEGER (KEYWORD, MAXNUM, DEFAULT, 
     +					NUMVAL, VALUES)
C	    Searches through the token list for the KEYWORD.  Puts
C	 up to MAXNUM following numeric values into VALUES.  DEFAULT
C	 values are put in if the keyword is missing or if values aren't
C	 supplied.  NUMVAL returns the number of values supplied.
C	 NUMVAL=-1 if keyword not in token list.  
C	 NUMVAL=0 if no numeric values following keyword.
C	 
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	MAXNUM, DEFAULT(1), NUMVAL, VALUES(1)
	CHARACTER*(*) KEYWORD
	INTEGER	I, J, L, SLEN
	INTEGER	ALPHA, NUMERIC
	PARAMETER (ALPHA = 1, NUMERIC = 2)


	NUMVAL = -1
	DO I = 1, NUMTOKENS
	    IF (TOKENTYPE(I) .EQ. ALPHA) THEN	! Only match alpha tokens
		L = SLEN(TOKENSTR(I))
		IF (TOKENSTR(I)(1:L) .EQ. KEYWORD(1:L)) THEN
		    J = 1		! If token matches, get values
		    DO WHILE (J .LE. MAXNUM .AND. TOKENTYPE(I+J)
     +                          .EQ. NUMERIC)
			VALUES(J) = NINT(TOKENVALUE(I+J))
			J = J + 1
		    ENDDO
		    NUMVAL = J - 1
		    GOTO 200
		ENDIF
	    ENDIF
	ENDDO

200	CONTINUE
	DO J = MAX(NUMVAL+1,1), MAXNUM	  ! Fill in places left with defaults
	    VALUES(J) = DEFAULT(J)
	ENDDO

	RETURN
	END




	SUBROUTINE GET_KEYWORD_REAL (KEYWORD, MAXNUM, DEFAULT, 
     +					NUMVAL, VALUES)
C	    Searches through the token list for the KEYWORD.  Puts
C	 up to MAXNUM following numeric values into VALUES.  DEFAULT
C	 values are put in if the keyword is missing or if values aren't
C	 supplied.  NUMVAL returns the number of values supplied.
C	 NUMVAL=-1 if keyword not in token list.  
C	 NUMVAL=0 if no numeric values following keyword.
C	 
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	MAXNUM, NUMVAL
	REAL	DEFAULT(1), VALUES(1)
	CHARACTER*(*) KEYWORD
	INTEGER	I, J, L, SLEN
	INTEGER	ALPHA, NUMERIC
	PARAMETER (ALPHA = 1, NUMERIC = 2)


	NUMVAL = -1
	DO I = 1, NUMTOKENS
	    IF (TOKENTYPE(I) .EQ. ALPHA) THEN	! Only match alpha tokens
		L = SLEN(TOKENSTR(I))
		IF (TOKENSTR(I)(1:L) .EQ. KEYWORD(1:L)) THEN
		    J = 1		! If token matches, get values
		    DO WHILE (J.LE.MAXNUM.AND.TOKENTYPE(I+J).EQ.NUMERIC)
			VALUES(J) = TOKENVALUE(I+J)
			J = J + 1
		    ENDDO
		    NUMVAL = J - 1
		    GOTO 200
		ENDIF
	    ENDIF
	ENDDO

200	CONTINUE
	DO J = MAX(NUMVAL+1,1), MAXNUM	  ! Fill in places left with defaults
	    VALUES(J) = DEFAULT(J)
	ENDDO

	RETURN
	END




	SUBROUTINE GET_KEYWORD_STRING (KEYWORD, MAXNUM, DEFAULT, 
     +					NUMVAL, VALUES)
C	    Searches through the token list for the KEYWORD.  Puts
C	 up to MAXNUM following string values into VALUES.  DEFAULT
C	 values are put in if the keyword is missing or if values aren't
C	 supplied.  NUMVAL returns the number of values supplied.
C	 NUMVAL=-1 if keyword not in token list.  
C	 NUMVAL=0 if no numeric values following keyword.
C	 
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	MAXNUM, NUMVAL
	CHARACTER*(*) KEYWORD,  DEFAULT(1), VALUES(1)
	INTEGER	I, J, L, SLEN
	INTEGER	ALPHA, NUMERIC
	PARAMETER (ALPHA = 1, NUMERIC = 2)


	NUMVAL = -1
	DO I = 1, NUMTOKENS
	    IF (TOKENTYPE(I) .EQ. ALPHA) THEN	! Only match alpha tokens
		L = SLEN(TOKENSTR(I))
		IF (TOKENSTR(I)(1:L) .EQ. KEYWORD(1:L)) THEN
		    J = 1		! If token matches, get values
		    DO WHILE (J .LE. MAXNUM .AND. I+J .LE. NUMTOKENS)
			VALUES(J) = TOKENSTR(I+J)
			J = J + 1
		    ENDDO
		    NUMVAL = J - 1
		    GOTO 200
		ENDIF
	    ENDIF
	ENDDO

200	CONTINUE
	DO J = MAX(NUMVAL+1,1), MAXNUM	  ! Fill in places left with defaults
	    VALUES(J) = DEFAULT(J)
	ENDDO

	RETURN
	END




	LOGICAL FUNCTION keytest (KEYWORD)
C	    Searches through the token list for the KEYWORD.  
C	 Returns true if keyword is in token list.
	 
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	CHARACTER*(*) KEYWORD
	INTEGER	I, L, SLEN
	INTEGER	ALPHA, NUMERIC
	PARAMETER (ALPHA = 1, NUMERIC = 2)


	DO I = 1, NUMTOKENS
	    IF (TOKENTYPE(I) .EQ. ALPHA) THEN	! Only match alpha tokens
		L = SLEN(TOKENSTR(I))
		IF (TOKENSTR(I)(1:L) .EQ. KEYWORD(1:L)) THEN
		    keytest = .TRUE.
		    RETURN
		ENDIF
	    ENDIF
	ENDDO
	keytest = .FALSE.

	RETURN
	END




C ********************************************************************
C	             INITIALIZATION ROUTINES


	SUBROUTINE FILE_SETUP
C	    FILE_SETUP determines the file configuration: whether 3 files
C	  for color mode or 1 file for BW;  whether the input files are
C	  to be copied to the output file.  The file(s) that are to be
C	  used in the rest of the program are opened for update.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	UNIT1(3), UNIT2(3), NLINE(3), NSAMP(3)
	INTEGER	I, LINE, STATUS
	INTEGER	NUMINPIMAGES
	INTEGER	SL, SS, NL, NS, NLI, NSI
	BYTE	BUFFER(MAXFILESAMPS)
	LOGICAL	XVPTST
	CHARACTER*8  FORMAT
	CHARACTER*60 STRING
	CHARACTER*128  FILES(10)

        COMMON /TESTCMN/   TEST
        LOGICAL            TEST

C==================================================================

	IF (XVPTST('COLOR')) THEN
	    COLORMODE = FULLCOLOR
	    NUMINPIMAGES = 3
	ELSE
	    COLORMODE = BW
	    NUMINPIMAGES = 1
	ENDIF


	CALL XVP ('INP', FILES, NUMINPFILES)
	IF (COLORMODE .EQ. FULLCOLOR .AND. NUMINPFILES .LT. 3) THEN
	   CALL MABEND('Must be at least three input files for color.')
	ENDIF

	DO I = 1, NUMINPIMAGES
	    CALL XVUNIT (UNIT1(I), 'INP', I, STATUS,' ')
	    CALL XVOPEN (UNIT1(I), STATUS, 'OP','READ',
     +                   'U_FORMAT','BYTE',' ')
	    CALL XVGET (UNIT1(I), STATUS, 'NL', NLINE(I), 'NS',NSAMP(I),
     +			'FORMAT',FORMAT,' ')
	    IF (FORMAT(1:4) .NE. 'BYTE') THEN
		CALL MABEND ('Input file must be in byte format.')
	    ENDIF
	ENDDO
	DO I = 2, NUMINPIMAGES
	    IF (NLINE(I) .NE. NLINE(1) .OR. NSAMP(I) .NE. NSAMP(1)) THEN
		CALL MABEND ('Input files must be same size.')
	    ENDIF
	ENDDO
	


	CALL XVP ('OUT', FILES, NUMOUTFILES)
	IF (COLORMODE .EQ. FULLCOLOR) THEN
	    IF (NUMOUTFILES .LT. 3) THEN
		NUMOUTIMAGES = 0
	    ELSE
		NUMOUTIMAGES = 3
	    ENDIF
	ELSE
	    IF (NUMOUTFILES .LT. 1) THEN
		NUMOUTIMAGES = 0
	    ELSE
		NUMOUTIMAGES = 1
	    ENDIF
	ENDIF


	IF (NUMOUTIMAGES .EQ. 0) THEN
	    DO I = 1, NUMINPIMAGES
		CALL XVCLOSE (UNIT1(I), STATUS,' ')
		UNIT(I) = UNIT1(I)
	    ENDDO
	    NLFILE = NLINE(1)
	    NSFILE = NSAMP(1)
	    NUMIMAGES = NUMINPIMAGES
	    CALL XVMESSAGE ('Warning : Editing original image(s).',' ')

	ELSE


	    NUMIMAGES = NUMOUTIMAGES
	    CALL XVSIZE (SL, SS, NL, NS, NLI, NSI)
	    NLFILE = MIN (NL, NLI-SL+1)
	    NSFILE = MIN (NS, NSI-SS+1)
	    DO I = 1, NUMOUTIMAGES
		CALL XVUNIT (UNIT2(I), 'OUT', I, STATUS,' ')
		CALL XVOPEN (UNIT2(I), STATUS, 'OP','WRITE',
     +                       'U_FORMAT','BYTE', 'O_FORMAT','BYTE',
     +			     'U_NL',NLFILE, 'U_NS',NSFILE,' ')
		DO LINE = SL, NLFILE+SL-1
		    CALL XVREAD (UNIT1(I), BUFFER, STATUS, 'LINE', LINE,
     +			'SAMP',SS, 'NSAMPS',NSFILE,' ')
		    CALL XVWRIT (UNIT2(I), BUFFER, STATUS,' ')
		ENDDO
		CALL XVCLOSE (UNIT1(I), STATUS,' ')
		CALL XVCLOSE (UNIT2(I), STATUS,' ')
		UNIT(I) = UNIT2(I)
	    ENDDO
	ENDIF


	DO I = 1, NUMIMAGES
	   CALL XVOPEN (UNIT(I),STATUS,'OP','UPDATE',
     +                  'U_FORMAT','BYTE',' ')
	ENDDO


	WRITE (STRING, '(1X,A,I5,1X,I5)', ERR=299) 
     +		'Image lines and samples: ', NLFILE, NSFILE
299	CALL XVMESSAGE (STRING,' ')

	IF (COLORMODE .EQ. FULLCOLOR) THEN
	    CALL XVMESSAGE ('Editing in color mode.',' ')
	ELSE
	    CALL XVMESSAGE ('Editing in black and white mode.',' ')
	ENDIF
	CALL XVMESSAGE (' ',' ')

        TEST = XVPTST( 'TEST' )     ! CHECK FOR TEST MODE.

	RETURN
	END




	SUBROUTINE DISPLAY_SETUP
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	MAXL, MAXS, CONFIG, NCURS, NINTIO, NSWITCH
	INTEGER	L, I, J, CURSFORM, CURSBLINK, CSETUP(4), ASETUP(4)
	INTEGER	RED(256), GREEN(256), BLUE(256), RGB(24)
	INTEGER	IERR, XDDUNIT, XDDACTIVATE, XDDOPEN, XDDCONFIGURE
	INTEGER	XDCON, XDCAUTOTRACK, XDDINFO, XDLRAMP, XDLCONNECT
	INTEGER	XDGCONNECT, XDGLWRITE, XDIFILL, XDGON, XDEACTION
	LOGICAL	FLAG, XVPTST

	DATA	CSETUP/0,0,0,0/,  ASETUP/1,1,0,0/
	DATA	RGB/0,0,0, 255,255,255, 255,0,0, 0,255,0, 
     +		    0,0,255, 255,255,0, 0,255,255, 255,0,255/

	CURSFORM = 2
        CURSBLINK = 0
	CURS = 1
	LUTSECT = 1

C		Define VRDI error action
  	IERR = XDEACTION ( 2, 2, 2 )

C		Get display device unit number
	IERR = XDDUNIT ( DISPU )       
	IF ( IERR .ne. 1 )  THEN
	    CALL ABEND
	END IF

C		Open display unit
	IERR = XDDOPEN ( DISPU )
	IF ( IERR .ne. 1 )  THEN
	    CALL ABEND
	END IF

C		Activate the display unit so that we can write on it
	FLAG = .TRUE.
	IERR = XDDACTIVATE ( DISPU, FLAG )

C		Configure the display (csetup is all 0's - default)
	IERR = XDDCONFIGURE ( DISPU, CSETUP )

C		Get device information 
	IERR = XDDINFO ( DISPU, 3, 1,  NLUTS )
	IERR = XDDINFO ( DISPU, 4, 1,  NIMPS )
	IERR = XDDINFO ( DISPU, 5, 1,  MAXL )
	IERR = XDDINFO ( DISPU, 6, 1,  MAXS )
	IERR = XDDINFO ( DISPU, 7, 1,  CONFIG )
CCC	CALL XDDINFO ( DISPU, 21, 1, SETLUT )
        SETLUT = 1                       ! CHANGE FOR NEW VRDI BY SXP.
	IERR = XDDINFO ( DISPU, 26, 1, MAXLUTVAL )
	IERR = XDDINFO ( DISPU, 30, 1, IGRAPH )
	IERR = XDDINFO ( DISPU, 34, 1, GRIMP )
	IERR = XDDINFO ( DISPU, 48, 1, NCURS )
	IERR = XDDINFO ( DISPU, 60, 1, NINTIO )
	IERR = XDDINFO ( DISPU, 64, 1, NSWITCH )
	NLMEM = MAXL
	NSMEM = MAXS
	IF (BTEST(CONFIG,9)) THEN
	    NLDISP = 1024
	    NSDISP = 1024
	ELSE
	    NLDISP = 512
	    NSDISP = 512
	ENDIF
	NLDISP = MIN (NLDISP, NLMEM)
	NSDISP = MIN (NSDISP, NSMEM)

	IF (COLORMODE .EQ. FULLCOLOR .AND. 
     +		(NIMPS .LT. 3 .OR. NLUTS .LT. 3)) THEN
	    CALL MABEND ('This display device does not support color.')
	ENDIF
	IF (NCURS .LT. 1) THEN
	    CALL MABEND ('Must have a display cursor for this program.')
	ENDIF
	IF (NINTIO .LT. 1 .OR. NSWITCH .LT. 3) THEN
	    CALL MABEND
     +           ('Must have three switch buttons for this program.') 
	ENDIF


C		Enable autotracking of cursor
	IERR = XDCAUTOTRACK (DISPU, CURS, 0, .TRUE.)

C		Turn on the cursor
	IERR = XDCON ( DISPU, CURS, CURSFORM, CURSBLINK )



C		Clear the image planes
	IF (XVPTST('CLEAR')) THEN
	IF (COLORMODE .EQ. FULLCOLOR) THEN
	    DO L = 1, NIMPS
		IERR = XDIFILL (DISPU, L, 0)
	    ENDDO
	ELSE
	    IERR = XDIFILL (DISPU, 1, 0)
	ENDIF
        ENDIF
C		Ramp and connect the LUTs
	DO L = 1, NLUTS
	    IERR = XDLRAMP ( DISPU, L, LUTSECT )
	ENDDO
	IF (COLORMODE .EQ. FULLCOLOR) THEN
	    DO L = 1, NLUTS
		IERR = XDLCONNECT (DISPU, L, L, LUTSECT, .FALSE. )
	    ENDDO
	ELSE
	    IERR = XDLCONNECT (DISPU, 1, 1, LUTSECT, .FALSE. )
	    IF (SETLUT .EQ. 1) THEN
		DO L = 2, NLUTS
		    IERR = XDLCONNECT (DISPU, 1, L, LUTSECT, .FALSE. )
		ENDDO
	    ENDIF
	ENDIF


C		Initialize the graphics plane
	IF (IGRAPH .NE. 1) THEN
	    GRIMP = 1
	ELSE
C		Clear the graphics plane
	    IERR = XDIFILL (DISPU, GRIMP, 0)

C		Initialize graphics lut
	    J = 1
	    DO I = 1, 8
		RED(I)   = RGB(J)
		GREEN(I) = RGB(J+1)
		BLUE(I)  = RGB(J+2)
		J = J + 3
	    ENDDO
	    IERR = XDGLWRITE( DISPU, LUTSECT, RED, GREEN, BLUE )

	    IERR = XDGCONNECT ( DISPU, GRIMP, LUTSECT, .FALSE. )

C		Turn on the graphics overlay plane
	    IERR = XDGON ( DISPU )
	ENDIF

	RETURN
	END



	SUBROUTINE INITIALIZE
C	    INITIALIZE initializes the global variables
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	I, J, LUT, PALCOLTAB(3,16), OCLINE, OCSAMP
	REAL	XMIN, XMAX, YMIN, YMAX
	COMMON / CLIPCOM / XMIN, XMAX, YMIN, YMAX
	COMMON /CURS/ OCLINE, OCSAMP

	DATA PALCOLTAB/
     +	     0,0,0,	0,0,200,	0,70,230,	0,130,170,
     +	     0,170,130,	0,210,80,	0,200,0,	130,170,0,
     +	     220,200,0,	255,240,0,	255,140,80,	255,80,80,
     +	     220,0,0,	230,0,100,	255,0,150,	255,255,255/


	SLFILE = 1
	SSFILE = 1
	NLD = 0
	NSD = 0
	SLMEM = 1
	SSMEM = 1
	HZOOMFAC = 1
	CHANGEDMEM = .FALSE.

	DO I = 1, 256
	    STRETCHLUT(I) = I - 1
	    DO LUT = 1, 3
		PSEUDOLUT(I,LUT) = I - 1
	    ENDDO
	ENDDO


	PALLINES = NLDISP/32
	PALSAMPS = NSDISP
	PALCOLORS = 16
	PALETTEON = .FALSE.
	IF (COLORMODE .EQ. FULLCOLOR) THEN
	    DO I = 1, PALCOLORS
		DO J = 1, 3
		    COLORTABLE(J,I) = PALCOLTAB(J,I)
		ENDDO
	    ENDDO
	ELSE
	    DO I = 1, PALCOLORS
		COLORTABLE(1,I) = NINT( 256.0/PALCOLORS *(I-0.5) )
	    ENDDO
	ENDIF

	BRUSHSIZE = 10
	BRUSHTYPE = 'DISK'
	CALL MAKEBRUSH 
	PAINTDENS = 0.25

	TEXTFONT = -1
	TEXTSIZE = 10
	TEXTLOC = 1
	TEXTSCALE = 1.0
	TEXTANGLE = 0.0
	TEXTSTRING = ' '

	NUMPOLY = 0
	GRAPHPTR(1) = 1
C			Set up the clipping window
	XMIN = 1.0
	XMAX = FLOAT(NLMEM)
	YMIN = 1.0
	YMAX = FLOAT(NSMEM)

	OCLINE = 1
	OCSAMP = 1

	RETURN
	END




C *******************************************************************
C                 ENDING ROUTINES 



	SUBROUTINE DISPLAY_OFF
C	    Turns the image display device off
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	XDDACTIVATE, XDDCLOSE, IERR


C		Deactivate the display device 
	IERR = XDDACTIVATE ( DISPU, .FALSE.)

C		Close device
	IERR = XDDCLOSE (DISPU )

	RETURN
	END



	SUBROUTINE FILE_OUTPUT
C	    Writes out the image memory planes if they have been changed.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	STATUS, IMP, LINE
	BYTE	BUFFER(MAXMEMSAMPS)
	INTEGER	XDILINEREAD, IERR


C		If image memory has been changed then write out memory
C		    to file.
	IF (CHANGEDMEM) THEN
	    CALL ERASEPALETTE
	    DO IMP = 1, NUMIMAGES
		DO LINE = 1, NLD
		    IERR = XDILINEREAD (DISPU, IMP, 1, LINE,NSD,BUFFER)
		    CALL XVWRIT (UNIT(IMP), BUFFER, STATUS, 
     +					'LINE',LINE+SLFILE-1,
     +					'SAMP',SSFILE,'NSAMPS',NSD,' ')
		ENDDO
		CALL XVCLOSE (UNIT(IMP), STATUS,' ')
	    ENDDO
	ENDIF

	RETURN
	END



C *************************************************************
C                  GRAPHICS-POLYGON ROUTINES

	SUBROUTINE SELECTPOLYGON (NUM, LINE, SAMP)
C	    SELECTPOLYGON has the user select the desired polygon by
C	  locating the cursor inside it.  All of the polygons are 
C	  searched thru and the first one that contain cursor location 
C	  is highlighted.  The user is then asked for verification.
C	  If the user rejects the highlighted polygon (switch 2), then
C	  the user is asked to select again and the rest of the polygons 
C	  are searched.  This continues until the user accepts a polygon
C	  or no polygon is selected.   The polygon number is returned in
C	  NUM and the image memory plane cursor position is returned
C	  in LINE and SAMP.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	   NUM, LINE, SAMP
	INTEGER    PTR, N, I, FLAG, SWITCH
	INTEGER    CLINE, CSAMP
	REAL	   POINT(2)
	INTEGER	   IERR, XDCLOCATION


	NUM = 0

100	CONTINUE
	PRINT *,'Select point in polygon with switch 1'

C			Wait until switch 1 is depressed
	CALL SWITCHWAIT (1, SWITCH)

C			Get cursor location 
	IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
	IF ( IERR .ne. 1 )  RETURN
C			Convert the cursor position to image file coords
	LINE = (CLINE-1)/HZOOMFAC + SLMEM
	SAMP = (CSAMP-1)/HZOOMFAC + SSMEM
	POINT(1) = FLOAT( LINE + SLFILE-1 )   ! Convert to file coords
	POINT(2) = FLOAT( SAMP + SSFILE-1 )

C			Search thru the polygons to find the right one
90	CONTINUE
	I = NUM
	FLAG = 0
	DO WHILE (FLAG .EQ. 0)
	    I = I + 1
	    IF (I .GT. NUMPOLY) THEN
		IF (NUM .EQ. 0)  RETURN ! Return, if gone thru whole list
		NUM = 0			!  otherwise start at beginning
		GOTO 90
	    ENDIF
	    PTR = GRAPHPTR(I)
	    N = GRAPHLEN(I)
	    CALL POINTINPOLY (GRAPHBUF(1,PTR), N, POINT, FLAG)
	ENDDO
	NUM = I

C			Turn the polygon red to highlight it
	CALL DISPLAYPOLYGON (GRAPHBUF(1,PTR), N, 2)

	PRINT *,'Hit switch 1 to accept polygon,  switch 2 to reject'

C			Wait until a switch is depressed
	CALL SWITCHWAIT (0, SWITCH)

C			Dehighlight the polygon
	CALL DISPLAYPOLYGON (GRAPHBUF(1,GRAPHPTR(NUM)),GRAPHLEN(NUM),1)

	IF (SWITCH .EQ. 2) GOTO 100	    ! if switch 2 then try again

	RETURN
	END




	SUBROUTINE IMAGEINPOLY (POLYNUM, INOUT,NUMSEGS,LINE,SSAMP,NSAMP)
C	    IMAGEINPOLY returns the segments of image lines contained
C	  in the polygon with number POLYNUM.  The line, starting sample,
C	  and number of samples for all segments are returned.
C	  The coordinates returned are in image memory plane coordinates.
C	  If INOUT = 1 then the inside of the polygon is returned.
C	  If INOUT = 0 then the outside of the polygon is returned.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	POLYNUM, INOUT, NUMSEGS
	INTEGER	LINE(MAXSEGS), SSAMP(MAXSEGS), NSAMP(MAXSEGS)
	INTEGER	PTR, N, SL, EL, SS, ES, L, S, NUMSECTS, J, K
	REAL	POLYSIZE(4), INTERSECTS(200)


	PTR = GRAPHPTR(POLYNUM)
	N = GRAPHLEN(POLYNUM)
	IF (INOUT .EQ. 1) THEN
C			Find the bounding rectangle for the polygon
	    CALL MAKEPOLYSIZE (GRAPHBUF(1,PTR), N, POLYSIZE)
	    SL = MAX (NINT(POLYSIZE(1)) - 1, SLFILE)
	    EL = MIN (NINT(POLYSIZE(2)) + 1, NLMEM+SLFILE-1)
	    SS = MAX (NINT(POLYSIZE(3)) - 1, SSFILE)
	    ES = MIN (NINT(POLYSIZE(4)) + 1, NSMEM+SSFILE-1)
	ELSE
	    SL = SLFILE
	    EL = MIN(NLFILE,NLMEM)+SLFILE-1
	    SS = SSFILE
	    ES = MIN(NSFILE,NSMEM)+SSFILE-1
	ENDIF

C			For each line in the rectangle find the intersect points
	K = 1
	DO L = SL, EL
	    CALL POLYSECTLINE (GRAPHBUF(1,PTR), N, L, SS, ES, 
     +					NUMSECTS, INTERSECTS)
	    DO J = 1+INOUT, NUMSECTS-INOUT, 2
		LINE(K) = L - SLFILE + 1
		S = NINT(INTERSECTS(J)) 
		SSAMP(K) = S - SSFILE + 1
		NSAMP(K) = NINT(INTERSECTS(J+1)) - S + 1
		K = K + 1
	    ENDDO
	    IF (K .GT. MAXSEGS-20) THEN 
		NUMSEGS = K - 1
		RETURN
	    ENDIF
	ENDDO
	NUMSEGS = K - 1

	RETURN
	END


	SUBROUTINE MAKEPOLYSIZE (POLY, POLYLEN, POLYSIZE)
C	    MAKEPOLYSIZE finds the min and max for each coordinate (x,y)
C	in a polygon.
	IMPLICIT NONE
	INTEGER	POLYLEN, I
	REAL	POLY(2,1), POLYSIZE(4)
	REAL	XMIN, XMAX, YMIN, YMAX


	XMIN = POLY(1,1)
	XMAX = POLY(1,1)
	YMIN = POLY(2,1)
	YMAX = POLY(2,1)
	DO I = 2, POLYLEN
	    XMIN = MIN( XMIN, POLY(1,I) )
	    XMAX = MAX( XMAX, POLY(1,I) )
	    YMIN = MIN( YMIN, POLY(2,I) )
	    YMAX = MAX( YMAX, POLY(2,I) )
	ENDDO

	POLYSIZE(1) = XMIN
	POLYSIZE(2) = XMAX
	POLYSIZE(3) = YMIN
	POLYSIZE(4) = YMAX

	RETURN
	END


	SUBROUTINE POINTINPOLY (POLY, N, POINT, FLAG)
C	    POINTINPOLY determines whether a point is in a polygon
C	by finding how many times a line drawn to "infinity" is
C	crossed by the line segments of the polygon.  It also 
C	determines if the point is within 0.01 of the polygon.
C	FLAG=0 for point outside polygon, FLAG=1 for inside, and 
C	FLAG=2 for point on polygon.
C	  Polygons are stored as a list of coordinates with the last
C	coordinate value being the same as the first.  There are
C	three numbers per coordinate (the third is label data).

	IMPLICIT NONE
	INTEGER	N, FLAG
	REAL	POLY(2,N), POINT(2)
	INTEGER	COUNT, I, INSECT
	REAL	LINESEG(2,2),  DUMMY(2), DIST


	LINESEG(1,1) = POINT(1)
	LINESEG(2,1) = POINT(2)
	LINESEG(1,2) = 1.0E5
	LINESEG(2,2) = 1.0E5
100	CONTINUE
	COUNT = 0
	DO I = 1, N-1
	    CALL POINTLINEDIST (POLY(1,I), POINT, DIST)
	    IF (DIST .LT. 0.01) THEN
		FLAG = 2
		RETURN
	    ENDIF
	    CALL LINESEGSECT (LINESEG, POLY(1,I), INSECT, DUMMY)
	    IF (INSECT .GE. 1) THEN
		COUNT = COUNT + 1
		IF (INSECT .NE. 4) THEN
		    LINESEG(1,2) = LINESEG(1,2) - 1.0E4
		    GOTO 100
		ENDIF
	    ENDIF
	ENDDO
	FLAG = MOD(COUNT,2)

	RETURN
	END



	SUBROUTINE POINTLINEDIST (LINESEG, POINT, DIST)
C	    POINTLINEDIST calculates the distance between a point (POINT)
C	and a line segment (LINESEG).
C	    Parametric equations are used for accuracy.
	IMPLICIT NONE
	REAL	LINESEG(2,2), POINT(2), DIST
	REAL	X1, Y1, DX, DY, DET, T, DX01, DY01
	
C		Find the point on the line that is closest to the point
C		    (i.e. the point on the line such that the line segment
C		     made is perpendicular to the original line).
C		    If the point is not of the line segment then slide it
C		    down or up to the segment and then find the distance
C		    between the point on the line and the inputted point.
	X1 = LINESEG(1,1)
	Y1 = LINESEG(2,1)
	DX = LINESEG(1,2) - X1
	DY = LINESEG(2,2) - Y1
	DX01 = POINT(1) - X1
	DY01 = POINT(2) - Y1
	DET = DX**2 + DY**2
	IF (DET .NE. 0.0) THEN
	    T = ( DX*DX01 + DY*DY01 )/DET
	    T = MIN( MAX( T, 0.0), 1.0)
	    DIST = SQRT( (T*DX-DX01)**2 + (T*DY-DY01)**2 )
	ELSE
	    DIST = SQRT (DX01**2 + DY01**2)
	ENDIF

	RETURN
	END



	SUBROUTINE POLYSECTLINE (POLY, NVERTS, LINE, SS, ES, 
     +					NUMSECTS, INTERSECTS)
C	    POLYSECTLINE finds the intersection points between 
C	the polygon and the current line (LINE).  The beginning (SS) 
C	and ending (ES) of the line are included in the list of 
C	intersections.	The list (returned in INTERSECTS) is sorted and
C	gives the sample coordinates of all of the intersections.
C	
	IMPLICIT NONE
	INTEGER	NVERTS, LINE, SS, ES, NUMSECTS
	REAL	POLY(2,1), INTERSECTS(*)

	INTEGER	I, FLAG, K, J
	REAL	LINESEG(2,2), POINT(2)
	REAL	SSREAL, ESREAL, DEL0, DEL1, DEL2, LASTY
	LOGICAL	NEARPOINTS, JOINT

C		Make the intersecting line segment and put the ends of the
C		    image line in the intersect list
	LINESEG(1,1) = FLOAT(LINE)
	LINESEG(2,1) = -1.0E5
	LINESEG(1,2) = FLOAT(LINE)
	LINESEG(2,2) = 1.0E5
	SSREAL = FLOAT(SS)
	ESREAL = FLOAT(ES)
	INTERSECTS(1) = SSREAL
	INTERSECTS(2) = ESREAL
	K = 2
	LASTY = POLY(1,NVERTS-1)

C		Go through all the line segments of the polygon 
C		    and find the intersections.
	DO I = 1, NVERTS-1
	    CALL LINESEGSECT (LINESEG, POLY(1,I), FLAG, POINT)
	    IF (FLAG .EQ. -2) GOTO 200    ! Ignore zero length segments
C				Skip if no intersection
	    IF (FLAG .LE. 0)  GOTO 199 
C			   See if intersection is through a vertex
	    IF (FLAG .EQ. 3) THEN
		DEL1 = POLY(1,I) - LASTY
		DEL2 = POLY(1,I+1) - POLY(1,I)
C			If this vertex is on the end of a horizontal line
C			  then skip intersect if segment before horizontal
C			  line goes opposite direction from next segment 
		IF (DEL1 .EQ. 0.0 .AND.NEARPOINTS(POINT,POLY(1,I))) THEN
		    J = I
		    DO WHILE (POLY(1,J) .EQ. POLY(1,I))
			J = J - 1
			IF (J .LT. 1)  J = NVERTS - 1
		    ENDDO
		    DEL0 = POLY(1,I) - POLY(1,J)
		    IF (DEL0*DEL2 .GT. 0.0)  GOTO 199
		ENDIF
C			If vertex is a tip take both intersects, but if it is
C			  a joint only take intersect from one segment.
		JOINT = DEL1*DEL2 .GT. 0.0
		IF (JOINT .AND. NEARPOINTS(POINT,POLY(1,I)) ) GOTO 199
	    ENDIF

C			Clip the intersections if the polygons is partially
C			    or fully off the image.
	    POINT(2) = MAX( MIN( POINT(2), ESREAL), SSREAL)
	    K = K + 1
	    INTERSECTS(K) = POINT(2)
199	    CONTINUE
	    LASTY = POLY(1,I)
200	    CONTINUE
	ENDDO

	NUMSECTS = K
C		Sort the intersections (by sample)
	IF (NUMSECTS .GT. 2)  CALL SSORT (INTERSECTS, 1, NUMSECTS)

	RETURN
	END


	LOGICAL FUNCTION NEARPOINTS (POINT1, POINT2)
C	    Returns true if points are near each other (0.01 is near).
	IMPLICIT NONE
	REAL	POINT1(2), POINT2(2)

	IF (ABS(POINT1(1)-POINT2(1)) .LE. 0.01 .AND.
     +	    ABS(POINT1(2)-POINT2(2)) .LE. 0.01 )  THEN
	    NEARPOINTS = .TRUE.
	ELSE
	    NEARPOINTS = .FALSE.
	ENDIF

	RETURN
	END



	SUBROUTINE LINESEGSECT (LINESEG1, LINESEG2, FLAG, POINT)
C	    LINESEGSECT determines if two line segments intersect
C	(including the endpoints).  The method is to find the
C	intersection point of the two mathematical lines and
C	then see if that point is between the two points for
C	both segments.  If there is intersection then
C	the coordinates of the intersect point is returned (POINT).
C	    Parametric equations are used to avoid special cases and
C	roundoff error from large slopes.  The equations for a line are:
C		X = T*(X2-X1) + X1
C		Y = T*(Y2-Y1) + Y1
C
C	FLAG = -2   if a line segment is a point
C	FLAG = -1   for colinear line segments 
C	FLAG = 0    for no intersection
C	FLAG = 1    for intersection involving endpoints from both segments
C	FLAG = 2    for intersection involving endpoints from segment 1 only
C	FLAG = 3    for intersection involving endpoints from segment 2 only
C	FLAG = 4    for intersection excluding all endpoints 
C	
	IMPLICIT NONE
	INTEGER	FLAG
	REAL	LINESEG1(2,2), LINESEG2(2,2), POINT(2)
	REAL	MINX, MAXX, MINY, MAXY
	REAL	X1, Y1, DX1, DY1, DX2, DY2, DX, DY, DET
	REAL	T1, T2, DEL1, DEL2, R1, R2
	LOGICAL	INCLUD1, INCLUD2, EXCLUD1, EXCLUD2


	FLAG = 0
	MINX = MIN (LINESEG1(1,1), LINESEG1(1,2))
	IF (LINESEG2(1,1).LT. MINX .AND. LINESEG2(1,2) .LT. MINX) RETURN
	MAXX = MAX (LINESEG1(1,1), LINESEG1(1,2))
	IF (LINESEG2(1,1).GT. MAXX .AND. LINESEG2(1,2) .GT. MAXX) RETURN
	MINY = MIN (LINESEG1(2,1), LINESEG1(2,2))
	IF (LINESEG2(2,1).LT. MINY .AND. LINESEG2(2,2) .LT. MINY) RETURN
	MAXY = MAX (LINESEG1(2,1), LINESEG1(2,2))
	IF (LINESEG2(2,1).GT. MAXY .AND. LINESEG2(2,2) .GT. MAXY) RETURN

	X1 = LINESEG1(1,1)
	Y1 = LINESEG1(2,1)
	DX1 = LINESEG1(1,2) - X1
	DY1 = LINESEG1(2,2) - Y1
	DX2 = LINESEG2(1,2) - LINESEG2(1,1)
	DY2 = LINESEG2(2,2) - LINESEG2(2,1)
	R1 = SQRT(DX1**2 + DY1**2)
	R2 = SQRT(DX2**2 + DY2**2)
	IF (R1 .LT. 0.01 .OR. R2 .LT. 0.01) THEN
	    FLAG = -2
	    RETURN
	ENDIF
	DET = DX2*DY1 - DX1*DY2 
	IF (ABS(DET) .LE. 1.0E-5) THEN
	    FLAG = -1
	    RETURN
	ENDIF
	DX = LINESEG2(1,1) - X1
	DY = LINESEG2(2,1) - Y1
	T1 = (DX2*DY - DY2*DX)/DET
	T2 = (DX1*DY - DY1*DX)/DET
	POINT(1) = T1*DX1 + X1
	POINT(2) = T1*DY1 + Y1
	DEL1 = 0.01/R1
	DEL2 = 0.01/R2
	INCLUD1 = ( T1 .GT. -DEL1 .AND. T1 .LT. 1.0+DEL1)
	INCLUD2 = ( T2 .GT. -DEL2 .AND. T2 .LT. 1.0+DEL2)
	IF (INCLUD1 .AND. INCLUD2) THEN
	    FLAG = 1
	    EXCLUD1 = ( T1 .GT. DEL1 .AND. T1 .LT. 1.0-DEL1)
	    EXCLUD2 = ( T2 .GT. DEL2 .AND. T2 .LT. 1.0-DEL2)
	    IF (EXCLUD1) FLAG = FLAG + 2
	    IF (EXCLUD2) FLAG = FLAG + 1
	ENDIF

	RETURN
	END





	SUBROUTINE DISPLAYPOLYGON (POLY, N, DN)
C	    DISPLAYPOLYGON displays the line string POLY with N points
C	  on the graphics plane in color DN.  Full clipping at the edge
C	  of the image memory planes is performed.  The linestring may be
C	  any number of points (0 - ...)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	N, DN, I
	REAL	POLY(2,1)
	REAL	L1, S1, L2, S2	! L is line, S is sample
	LOGICAL	DRAWFLAG

	IF (N .LT. 1) RETURN
	IF (N .EQ. 1) THEN
	    CALL CLIPPER (POLY(1,1)-SLFILE+1, POLY(2,1)-SSFILE+1,
     +			  POLY(1,1)-SLFILE+1, POLY(2,1)-SSFILE+1, 
     +			  L1, S1, L2, S2, DRAWFLAG)
	    IF (DRAWFLAG) 
     +		CALL DISPLAYLINE (NINT(L1), NINT(S1), 
     +				  NINT(L2), NINT(S2), DN)
	ELSE
	    DO I = 1, N-1
		CALL CLIPPER (POLY(1,I)-SLFILE+1,  POLY(2,I)-SSFILE+1, 
     +			      POLY(1,I+1)-SLFILE+1,POLY(2,I+1)-SSFILE+1, 
     +			      L1, S1, L2, S2, DRAWFLAG)
		IF (DRAWFLAG) 
     +		    CALL DISPLAYLINE (NINT(L1), NINT(S1), 
     +				      NINT(L2), NINT(S2), DN)
	    ENDDO
	ENDIF

	RETURN
	END




	SUBROUTINE CLIPPER( A1, B1, A2, B2,  X1, Y1, X2, Y2, ACCEPT)

c	This routine is a fortran version of the Cohen-Sutherland
c	algorithm for clipping. A complete explanation of the 
c	workings of the algorithm plus the Pascal code can be
c	found in the book "Fundamentals of Interactive Computer
c	Graphics" by J. D. Foley and A. Van Dam ( 1982 ).

	IMPLICIT NONE
	LOGICAL OUTCODE1(4),  OUTCODE2(4), ACCEPT, REJECT, DONE
	LOGICAL rjcheck, accheck, SWAPPED
	REAL*4  X1, X2, Y1, Y2
	REAL*4  A1, A2, B1, B2
	REAL*4  XMIN, XMAX, YMIN, YMAX
	COMMON / CLIPCOM / XMIN, XMAX, YMIN, YMAX


	X1 = A1
	X2 = A2
	Y1 = B1
	Y2 = B2

	ACCEPT = .FALSE.
	REJECT = .FALSE.
	DONE = .FALSE.
	SWAPPED = .FALSE.

	DO WHILE( .NOT. DONE )
	  CALL OUTCODES( X1, Y1, OUTCODE1 )
	  CALL OUTCODES( X2, Y2, OUTCODE2 )
	  REJECT = rjcheck ( OUTCODE1, OUTCODE2 )
	  IF ( REJECT ) THEN
	    DONE = .TRUE.
	  ELSE
	    ACCEPT = accheck( OUTCODE1, OUTCODE2 )
	    IF ( ACCEPT ) THEN
	      DONE = .TRUE.
	    ELSE
	      IF (.NOT.( OUTCODE1(1) .OR. OUTCODE1(2) .OR. OUTCODE1(3)        
     &		    .OR. OUTCODE1(4) ) ) THEN
		CALL SWAP( X1, X2, Y1, Y2, OUTCODE1, OUTCODE2 )
		SWAPPED = .NOT. SWAPPED
	      ENDIF

	      IF ( OUTCODE1(1) ) THEN
		X1 = X1 + ( X2 - X1 ) * ( YMAX - Y1 ) / ( Y2 - Y1 )
		Y1 = YMAX
	      ELSE IF ( OUTCODE1(2) ) THEN
		X1 = X1 + ( X2 - X1 ) * ( YMIN - Y1 ) / ( Y2 - Y1 )
		Y1 = YMIN
	      ELSE IF ( OUTCODE1(3) ) THEN
		Y1 = Y1 + ( Y2 - Y1 ) * ( XMAX - X1 ) / ( X2 - X1 )
		X1 = XMAX
	      ELSE IF ( OUTCODE1(4) ) THEN
		Y1 = Y1 + ( Y2 - Y1 ) * ( XMIN - X1 ) / ( X2 - X1 )
		X1 = XMIN
	      ENDIF
	    ENDIF
	  ENDIF
	ENDDO ! WHILE

	RETURN
	END


	SUBROUTINE OUTCODES( X, Y, OUTCODE )
	IMPLICIT NONE
	REAL*4 X, Y, XMIN, XMAX, YMIN, YMAX
	LOGICAL OUTCODE(4)

	COMMON /CLIPCOM/ XMIN, XMAX, YMIN, YMAX

	IF ( X .LT. XMIN ) THEN 
	  OUTCODE(4) = .TRUE.
	ELSE
	  OUTCODE(4) = .FALSE.
	ENDIF
	IF ( X .GT. XMAX ) THEN
	  OUTCODE(3) = .TRUE.
	ELSE
	  OUTCODE(3) = .FALSE.
	ENDIF
	IF ( Y .LT. YMIN ) THEN 
	  OUTCODE(2) = .TRUE.
	ELSE
	  OUTCODE(2) = .FALSE.
	ENDIF
	IF ( Y .GT. YMAX ) THEN
	  OUTCODE(1) = .TRUE.
	ELSE
	  OUTCODE(1) = .FALSE.
	ENDIF

	RETURN
	END


	LOGICAL FUNCTION rjcheck( OUTCODE1, OUTCODE2 )
	IMPLICIT NONE
	LOGICAL OUTCODE1(4), OUTCODE2(4)

	rjcheck = .FALSE.
	IF      ( OUTCODE1(1) .AND. OUTCODE2(1) ) THEN
	  rjcheck = .TRUE.
	ELSE IF ( OUTCODE1(2) .AND. OUTCODE2(2) ) THEN
	  rjcheck = .TRUE.
	ELSE IF ( OUTCODE1(3) .AND. OUTCODE2(3) ) THEN
	  rjcheck = .TRUE.
	ELSE IF ( OUTCODE1(4) .AND. OUTCODE2(4) ) THEN
	  rjcheck = .TRUE.
	ENDIF

	RETURN
	END


	LOGICAL FUNCTION accheck( OUTCODE1, OUTCODE2 )
	IMPLICIT NONE
	LOGICAL OUTCODE1(4), OUTCODE2(4)
	INTEGER*4 I

	accheck = .TRUE.
	DO I = 1, 4
	  IF ( OUTCODE1(I) .OR. OUTCODE2(I) ) THEN
	    accheck = .FALSE.
	  ENDIF
	ENDDO

	RETURN
	END



	SUBROUTINE SWAP( X1, X2, Y1, Y2, OUTCODE1, OUTCODE2 )
	IMPLICIT NONE
	LOGICAL OUTCODE1(4), OUTCODE2(4), LTEMP
	REAL*4  X1, Y1, X2, Y2, TEMP
	INTEGER I

	TEMP = X1
	X1 = X2
	X2 = TEMP
	TEMP = Y1
	Y1 = Y2
	Y2 = TEMP
	DO I = 1, 4
	  LTEMP = OUTCODE1(I)
	  OUTCODE1(I) = OUTCODE2(I)
	  OUTCODE2(I) = LTEMP
	ENDDO
	RETURN
	END



	SUBROUTINE DISPLAYLINE ( L1, S1, L2, S2, DN)
C	    DISPLAYLINE outputs a line segment to the graphics plane.
C	  Coordinates in image memory plane space.  
C	  L is line direction, S is sample direction.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER	L1, S1, L2, S2
	INTEGER	DN
	INTEGER  X(2), Y(2)
	INTEGER	XDIPOLYLINE, IERR

	X(1) = S1
	Y(1) = L1
	X(2) = S2
	Y(2) = L2
	IERR = XDIPOLYLINE (DISPU, GRIMP, INT2BYTE(DN), 2, X, Y)
	RETURN 
	END




	SUBROUTINE DISPLAYBOX (SL, SS, NL, NS, DN)
	IMPLICIT NONE
	INTEGER	SL, SS, NL, NS, DN

	CALL DISPLAYLINE (SL, SS, SL+NL-1, SS, DN)
	CALL DISPLAYLINE (SL+NL-1, SS, SL+NL-1, SS+NS-1, DN)
	CALL DISPLAYLINE (SL+NL-1, SS+NS-1, SL, SS+NS-1, DN)
	CALL DISPLAYLINE (SL, SS+NS-1, SL, SS, DN)

	RETURN
	END



	SUBROUTINE DRAWLINE ( L1, S1, L2, S2, COLOR, WIDTH)
C	    DRAWLINE outputs a line segment to the image planes.
C	  Coordinates are in image memory plane space.  
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	L1, S1, L2, S2, COLOR(3), WIDTH
	INTEGER  X(2), Y(2), IMP
	INTEGER	XDIPOLYLINE, IERR
	INTEGER	DX, DY, I, J, DELX, DELY

	IF (WIDTH .GT. 1) THEN
	    WIDTH = MIN (WIDTH,9)
	    DELX = S2 - S1
	    DELY = L2 - L1
	    IF (ABS(DELX) .GT. ABS(DELY)) THEN
		DX = 0
		DY = 1
	    ELSE
		DX = 1
		DY = 0
	    ENDIF
	    J = - (WIDTH+1)/2 
	    X(1) = S1 + DX*J
	    X(2) = S2 + DX*J
	    Y(1) = L1 + DY*J
	    Y(2) = L2 + DY*J
	    DO I = 1, WIDTH
		X(1) = X(1) + DX
		X(2) = X(2) + DX
		Y(1) = Y(1) + DY
		Y(2) = Y(2) + DY
		DO IMP = 1, NUMIMAGES
		    IERR = XDIPOLYLINE (DISPU,IMP,COLOR(IMP),2,X,Y)
		ENDDO
	    ENDDO
	ELSE

	    X(1) = S1
	    X(2) = S2
	    Y(1) = L1
	    Y(2) = L2
	    DO IMP = 1, NUMIMAGES
		IERR = XDIPOLYLINE (DISPU, IMP, COLOR(IMP), 2, X, Y)
	    ENDDO
	ENDIF

	RETURN 
	END





C *************************************************************
C                  UTILITY ROUTINES


	SUBROUTINE CURSORCOLOR (COLOR)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER	COLOR(3)
	INTEGER  LINE, SAMP, IMP
        BYTE DNB(2)
	INTEGER	IERR, XDCLOCATION, XDIPIXELREAD


	IERR = XDCLOCATION (DISPU, CURS, SAMP, LINE)
	IF ( IERR .ne. 1 )  RETURN
	LINE = (LINE-1)/HZOOMFAC + SLMEM
	SAMP = (SAMP-1)/HZOOMFAC + SSMEM
	DO IMP = 1, NUMIMAGES
	    IERR = XDIPIXELREAD (DISPU, IMP, SAMP, LINE, DNB(1))
	    IF ( IERR .ne. 1 )  RETURN
	    COLOR(IMP) = BYTE2INT(DNB(1))
	ENDDO
	DO IMP = NUMIMAGES+1, 3
	    COLOR(IMP) = BYTE2INT(DNB(1))
	ENDDO

	RETURN
	END



	SUBROUTINE UPDATELUTS (STARTLUT, NUMLUTS)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER STARTLUT, NUMLUTS
	INTEGER LUTARRAY(256), LUT, I, J
	INTEGER	XDLWRITE, IERR

	DO LUT = STARTLUT, NUMLUTS
	    IF (COLORMODE .EQ. PSEUDO) THEN
		DO I = 1, 256
		    J = MIN( MAX( STRETCHLUT(I) + 1, 1), 256)
		    LUTARRAY(I) = PSEUDOLUT(J,LUT)
		ENDDO
	    ELSE
		DO I = 1, 256
		    LUTARRAY(I) = STRETCHLUT(I)
		ENDDO
	    ENDIF
	    IERR = XDLWRITE (DISPU, LUT, LUTSECT, LUTARRAY)
	    IF ( IERR .ne. 1 )  RETURN
	ENDDO

	RETURN
	END



	SUBROUTINE DRAWCOMPOSE (DN, MODE, TRICENLINE, TRICENSAMP, 
     +			TRISCALE, INTENSLINE, INTENSSAMP, INTENSSCALE )
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
        INCLUDE 'fortport'
	INTEGER	DN, MODE, IERR, XDTFONT, XDTCOLOR, XDTSIZE, XDTROTATE
	REAL	TRICENLINE, TRICENSAMP, TRISCALE
	REAL	INTENSLINE, INTENSSAMP, INTENSSCALE
	INTEGER	TRILINE, TRISAMP, INTLINE, INTSAMP, XDTTEXT
	REAL	X, Y
	CHARACTER	RED, GREEN, BLUE, ZERO, TWO55(3)
	
	DATA	RED/'R'/, GREEN/'G'/, BLUE/'B'/
	DATA	ZERO/'0'/, TWO55/'2','5','5'/

	TRILINE(Y) = NINT(TRICENLINE - TRISCALE*Y - 1.)/HZOOMFAC + SLMEM
	TRISAMP(X) = NINT(TRICENSAMP + TRISCALE*X - 1.)/HZOOMFAC + SSMEM
	INTLINE(Y) = NINT(INTENSLINE - INTENSSCALE*Y -1.)/HZOOMFAC+SLMEM
	INTSAMP(X) = NINT(INTENSSAMP + INTENSSCALE*X -1.)/HZOOMFAC+SSMEM


	IF (IGRAPH .LE. 0) RETURN	! No graphics plane


	IF (TEXTFONT .LT. 0) THEN
	    IERR = XDTFONT (0)
	    TEXTFONT = 0
	ENDIF
	IERR = XDTCOLOR (INT2BYTE(DN), 0)
	IERR = XDTSIZE (10, 0.8)
	IERR = XDTROTATE (0.0)

	IF (MODE .EQ. FULLCOLOR) THEN
	    CALL DISPLAYLINE (TRILINE(-0.289), TRISAMP(-0.5), 
     +			      TRILINE(-0.289), TRISAMP(0.5), DN)
	    CALL DISPLAYLINE (TRILINE(-0.289), TRISAMP(0.5), 
     +			      TRILINE(0.577), TRISAMP(0.0), DN)
	    CALL DISPLAYLINE (TRILINE(0.577), TRISAMP(0.0), 
     +			      TRILINE(-0.289), TRISAMP(-0.5), DN)
	    IERR = XDTTEXT (DISPU, GRIMP, TRISAMP(-0.5), 
     +				TRILINE(-0.289)+14, 2, 1, RED)
	    IERR = XDTTEXT (DISPU, GRIMP, TRISAMP(0.5), 
     +				TRILINE(-0.289)+14, 2, 1, GREEN)
	    IERR = XDTTEXT (DISPU, GRIMP, TRISAMP(0.0), 
     +				TRILINE(0.577)-3, 2, 1, BLUE)
	ENDIF


	CALL DISPLAYLINE (INTLINE(-0.04), INTSAMP(0.0), 
     +			      INTLINE(-0.04), INTSAMP(1.0), DN)
	CALL DISPLAYLINE (INTLINE(-0.04), INTSAMP(0.0), 
     +			      INTLINE(0.04), INTSAMP(0.0), DN)
	CALL DISPLAYLINE (INTLINE(-0.04), INTSAMP(1.0), 
     +			      INTLINE(0.04), INTSAMP(1.0), DN)
	IERR = XDTTEXT (DISPU, GRIMP, INTSAMP(0.0), 
     +				INTLINE(-0.04)+14, 2, 1, ZERO)
	IERR = XDTTEXT (DISPU, GRIMP, INTSAMP(1.0), 
     +				INTLINE(-0.04)+14, 2, 3, TWO55)


	RETURN
	END



	SUBROUTINE DISPLAYPALETTE
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	BOXSAMPS, COLSAMPS, IMP, BOXSS, COL, PTR, LINE
	BYTE	BUFFER(MAXMEMSAMPS,3)
	INTEGER	IERR, XDILINEREAD, XDILINEWRITE


	IF (PALETTEON) CALL ERASEPALETTE

	NLPAL = MAX( (PALLINES-1)/HZOOMFAC+1, 1)
	NSPAL = (PALSAMPS-1)/HZOOMFAC + 1
	SLPAL = MIN ((NLDISP-1)/HZOOMFAC + 1 + SLMEM, NLMEM+1) - NLPAL
	SSPAL = SSMEM

	BOXSAMPS = NSPAL/PALCOLORS
	COLSAMPS = NINT(0.75*BOXSAMPS)
	DO IMP = 1, NUMIMAGES
	    CALL MVE (-5, NSPAL, 0, BUFFER(1,IMP), 0, 1) ! Clear display buffer
	    DO COL = 1, PALCOLORS
		BOXSS = (COL-1)*BOXSAMPS + 1
		CALL MVE (-5, COLSAMPS, COLORTABLE(IMP,COL), 
     +					BUFFER(BOXSS,IMP), 0, 1)
	    ENDDO
	ENDDO

	PTR = 1
	DO LINE = SLPAL, NLPAL+SLPAL-1
	    DO IMP = 1, NUMIMAGES
		IERR = XDILINEREAD (DISPU, IMP, SSPAL, LINE, NSPAL, 
     +						PALBUF(PTR))
		IF ( IERR .ne. 1 )  RETURN
		PTR = PTR + NSPAL
		IERR = XDILINEWRITE (DISPU, IMP, SSPAL, LINE, NSPAL, 
     +						BUFFER(1,IMP))
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	ENDDO

	PALETTEON = .TRUE.
	RETURN
	END




	SUBROUTINE UPDATEPALETTE (COLORNUM)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	COLORNUM
	INTEGER	BOXSAMPS, COLSAMPS, IMP, BOXSS, LINE
	BYTE	BUFFER(MAXMEMSAMPS,3)
	INTEGER	IERR, XDILINEWRITE


	IF (.NOT. PALETTEON)  RETURN
	IF (COLORNUM .LT. 0 .OR. COLORNUM .GT. PALCOLORS)  RETURN

	BOXSAMPS = NSPAL/PALCOLORS
	COLSAMPS = NINT(0.75*BOXSAMPS)
	DO IMP = 1, NUMIMAGES
	    CALL MVE (-5, COLSAMPS, COLORTABLE(IMP,COLORNUM), 
     +					BUFFER(1,IMP), 0, 1)
	ENDDO

	BOXSS = (COLORNUM-1)*BOXSAMPS + SSPAL
	DO LINE = SLPAL, NLPAL+SLPAL-1
	    DO IMP = 1, NUMIMAGES
		IERR = XDILINEWRITE (DISPU, IMP, BOXSS, LINE, COLSAMPS, 
     +						BUFFER(1,IMP))
		IF ( IERR .ne. 1 )  RETURN
	    ENDDO
	ENDDO

	RETURN
	END



	SUBROUTINE ERASEPALETTE
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	IMP, PTR, LINE
	INTEGER	IERR, XDILINEWRITE


	IF (.NOT. PALETTEON)  RETURN

	PTR = 1
	DO LINE = SLPAL, NLPAL+SLPAL-1
	    DO IMP = 1, NUMIMAGES
		IERR = XDILINEWRITE (DISPU, IMP, SSPAL, LINE, NSPAL, 
     +						PALBUF(PTR))
		IF ( IERR .ne. 1 )  RETURN
		PTR = PTR + NSPAL
	    ENDDO
	ENDDO

	PALETTEON = .FALSE.
	RETURN
	END



	SUBROUTINE LINEARLUT (LUTARRAY, STRETPAR, MAXLUTVAL)
	IMPLICIT NONE
	INTEGER LUTARRAY(256), MAXLUTVAL
	INTEGER	  STRETPAR(2), UP, LOW, DIF, I, I1, I2
	
	LOW = STRETPAR(1)
	UP = STRETPAR(2)
	IF (LOW .LE. UP) THEN
	    DO I = 0, MIN(LOW,255)
		LUTARRAY(I+1) = 0
	    ENDDO
	    DIF = UP - LOW
	    I1 = MIN (MAX (LOW,0), 255)
	    I2 = MIN (MAX (UP,0), 255)
	    DO I = I1+1, I2
		LUTARRAY(I+1) = MAXLUTVAL*(I-LOW)/DIF
	    ENDDO
	    DO I = MAX(UP,0), 255
		LUTARRAY(I+1) = MAXLUTVAL
	    ENDDO
	ELSE
	    DO I = 0, MIN(UP,255)
		LUTARRAY(I+1) = MAXLUTVAL
	    ENDDO
	    DIF = LOW - UP
	    I1 = MIN (MAX (UP,0), 255)
	    I2 = MIN (MAX (LOW,0), 255)
	    DO I = I1+1, I2
		LUTARRAY(I+1) = MAXLUTVAL*(LOW-I)/DIF
	    ENDDO
	    DO I = MAX(LOW,0), 255
		LUTARRAY(I+1) = 0
	    ENDDO
	ENDIF

	RETURN
	END



	SUBROUTINE MAKEBRUSH
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	CENTER, I, J
	REAL	R, BRUSHRAD


	BRUSHSIZE = MAX (MIN (BRUSHSIZE, MAXBRUSHSIZE), 1)
	CENTER = MAXBRUSHSIZE/2
	BRUSHRAD = BRUSHSIZE/2.


C				! Square brush
	IF (BRUSHTYPE(1:2) .EQ. 'SQ') THEN
	    BRUSHTYPE = 'SQUARE'
	    DO I = 1, MAXBRUSHSIZE
		BRUSHLON(I) = .FALSE.
		DO J = 1, MAXBRUSHSIZE
		    R = SQRT(FLOAT((I-CENTER)**2 + (J-CENTER)**2))
		    IF (ABS(I-CENTER) .LE. BRUSHRAD .AND.
     +		        ABS(J-CENTER) .LE. BRUSHRAD)  THEN
			BRUSHPAT(I,J) = 100
			BRUSHLON(I) = .TRUE.
		    ELSE
			BRUSHPAT(I,J) = 0
		    ENDIF
		ENDDO
	    ENDDO

C				! Dome brush
	ELSE IF (BRUSHTYPE(1:2) .EQ. 'DO') THEN
	    BRUSHTYPE = 'DOME'
	    DO I = 1, MAXBRUSHSIZE
		BRUSHLON(I) = .FALSE.
		DO J = 1, MAXBRUSHSIZE
		    R = SQRT(FLOAT((I-CENTER)**2 + (J-CENTER)**2))
		    IF (R .LE. BRUSHRAD) THEN
			BRUSHPAT(I,J) = NINT(100.*SQRT(1.0 -
     +                                  (R/BRUSHRAD)**2))
			BRUSHLON(I) = .TRUE.
		    ELSE
			BRUSHPAT(I,J) = 0
		    ENDIF
		ENDDO
	    ENDDO

C				! Gaussian brush
	ELSE IF (BRUSHTYPE(1:2) .EQ. 'GA') THEN
	    BRUSHTYPE = 'GAUSSIAN'
	    DO I = 1, MAXBRUSHSIZE
		BRUSHLON(I) = .FALSE.
		DO J = 1, MAXBRUSHSIZE
		    R = SQRT(FLOAT((I-CENTER)**2 + (J-CENTER)**2))
		    IF (R .LE. BRUSHRAD) THEN
		      BRUSHPAT(I,J)=NINT(100.*EXP(-2.0*(R/BRUSHRAD)**2))
		      BRUSHLON(I) = .TRUE.
		    ELSE
			BRUSHPAT(I,J) = 0
		    ENDIF
		ENDDO
	    ENDDO

	ELSE
C				! Disk brush
	    BRUSHTYPE = 'DISK'
	    DO I = 1, MAXBRUSHSIZE
		BRUSHLON(I) = .FALSE.
		DO J = 1, MAXBRUSHSIZE
		    R = SQRT(FLOAT((I-CENTER)**2 + (J-CENTER)**2))
		    IF (R .LT. BRUSHRAD) THEN
			BRUSHPAT(I,J) = 100
			BRUSHLON(I) = .TRUE.
		    ELSE
			BRUSHPAT(I,J) = 0
		    ENDIF
		ENDDO
	    ENDDO

	ENDIF
	
	RETURN
	END



	LOGICAL FUNCTION SWITCHED (SWITCH)
C	    SWITCHED returns True if the switch is depressed.
C	  Does not return until switch is no longer depressed.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER  SWITCH, SWSTATE
	INTEGER	IERR, XDXSWITCH

	SWITCHED =  .FALSE.
	IERR = XDXSWITCH (DISPU, 1, SWITCH, SWSTATE)
	IF ( IERR .ne. 1 )  RETURN
	IF (SWSTATE .EQ. 1)  SWITCHED = .TRUE.
	DO WHILE (SWSTATE .EQ. 1)
	    IERR = XDXSWITCH (DISPU, 1, SWITCH, SWSTATE)
	    IF ( IERR .ne. 1 )  RETURN
	    CALL VWAIT (3)
	ENDDO

	RETURN
	END



	LOGICAL FUNCTION SWITCHDOWN (SWITCH)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER  SWITCH, SWSTATE
	INTEGER	IERR, XDXSWITCH

	SWITCHDOWN =  .FALSE.
	IERR = XDXSWITCH (DISPU, 1, SWITCH, SWSTATE)
	IF ( IERR .ne. 1 )  RETURN
	IF (SWSTATE .EQ. 1)  SWITCHDOWN = .TRUE.

	RETURN
	END



	SUBROUTINE RESETSWITCHES
	IMPLICIT NONE
	INTEGER SWITCH
	LOGICAL DUMMY, SWITCHED
C				   Wait until all switches are not depressed
	DO SWITCH = 1, 3
	    DUMMY = SWITCHED (SWITCH)
	ENDDO

	RETURN
	END



	SUBROUTINE SWITCHWAIT (SWITCHIN, SWITCHOUT)
C	    SWITCHWAIT has two modes of operation:  if SWITCHIN is zero
C	  then switches 1 thru 3 are scanned until one of them is depressed;
C	  otherwise only switch SWITCHIN is checked until it is pressed.
C	  In both cases the routine only returns when the switch is no 
C	  longer depressed.
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER	SWITCHIN, SWITCHOUT
	INTEGER  SWSTATE, SWITCH
	INTEGER	  IERR, XDXSWITCH

	IF (SWITCHIN .EQ. 0) THEN
	    SWITCH = 0
	    SWSTATE = 0
	    DO WHILE (SWSTATE .EQ. 0)   ! Loop until a switch is hit
		SWITCH = SWITCH + 1
		IF (SWITCH .GT. 3)  SWITCH = 1
		IERR = XDXSWITCH (DISPU, 1, SWITCH, SWSTATE)
		IF ( IERR .ne. 1 )  RETURN
		CALL VWAIT (2)
	    ENDDO
	    DO WHILE (SWSTATE .EQ. 1)	! Wait until switch is lifted
		IERR = XDXSWITCH (DISPU, 1, SWITCH, SWSTATE)
		IF ( IERR .ne. 1 )  RETURN
		CALL VWAIT (5)
	    ENDDO
	    SWITCHOUT = SWITCH

	ELSE
	    SWSTATE = 0
	    DO WHILE (SWSTATE .EQ. 0)   ! Wait until switch is depressed
		IERR = XDXSWITCH (DISPU, 1, SWITCHIN, SWSTATE)
		IF ( IERR .ne. 1 )  RETURN
		CALL VWAIT (2)
	    ENDDO
	    DO WHILE (SWSTATE .EQ. 1)   ! Wait until switch is not depressed
		IERR = XDXSWITCH (DISPU, 1, SWITCHIN, SWSTATE)
		IF ( IERR .ne. 1 )  RETURN
		CALL VWAIT (5)
	    ENDDO
	    SWITCHOUT = SWITCHIN
	ENDIF

	RETURN
	END



	INTEGER FUNCTION SETCURSOR (CSAMP, CLINE)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER CLINE, CSAMP
	INTEGER	  XDCSET, IERR
	INTEGER OCLINE, OCSAMP
	COMMON /CURS/ OCLINE, OCSAMP

	IERR = XDCSET (DISPU, CURS, CSAMP, CLINE)
	OCSAMP = CSAMP
	OCLINE = CLINE
	SETCURSOR = IERR

	RETURN
	END



	INTEGER FUNCTION WHERECURSOR (CSAMP, CLINE, NEWPOS)
	IMPLICIT NONE
	INCLUDE 'edimage.fin'
	INTEGER CLINE, CSAMP
	INTEGER	  XDCSET, XDCLOCATION, IERR
        LOGICAL   NEWPOS, WRAP
	INTEGER OCLINE, OCSAMP
	COMMON /CURS/ OCLINE, OCSAMP

C			Get cursor location 
	IERR = XDCLOCATION (DISPU, CURS, CSAMP, CLINE)
	IF ( IERR .ne. 1)  GOTO 199

C			Check for wrap around
	WRAP = .FALSE.
	IF (ABS(CSAMP-OCSAMP) .GT. NSDISP/2) THEN
	    IF (OCSAMP .LT. NSDISP/2) THEN
		CSAMP = 1
	    ELSE
		CSAMP = NSDISP
	    ENDIF
	    WRAP = .TRUE.
	ENDIF
	IF (ABS(CLINE-OCLINE) .GT. NLDISP/2)  THEN
	    IF (OCLINE .LT. NLDISP/2) THEN
		CLINE = 1
	    ELSE
		CLINE = NLDISP
	    ENDIF
	    WRAP = .TRUE.
	ENDIF
C			If wrapped then fix cursor
	IF (WRAP) THEN
	    IERR = XDCSET (DISPU, CURS, CSAMP, CLINE)
	    IF ( IERR .ne. 1 )  GOTO 199
	ENDIF

	IF (CLINE .NE. OCLINE .OR. CSAMP .NE. OCSAMP) THEN
	    NEWPOS = .TRUE.
	    OCLINE = CLINE
	    OCSAMP = CSAMP
	ELSE
	    NEWPOS = .FALSE.
	ENDIF

199	CONTINUE
	WHERECURSOR = IERR

	RETURN
	END


	REAL FUNCTION GAUSNOIS (SEED)
C	    GAUSNOIS returns the value of a random variable with a
C	  Gaussian (normal) distribution with mean zero and variance one.
	IMPLICIT NONE
	INTEGER	SEED
	REAL	AM, PH, TEMP

        CALL RANGEN(SEED,TEMP)
	AM = LOG(MAX (TEMP, 1.0E-20))
	PH = 6.28318531*TEMP
	GAUSNOIS = AM*COS(PH)

	RETURN
	END


	SUBROUTINE ORDER (I1, I2)
	IMPLICIT NONE
	INTEGER	I1, I2, TEMP

	IF (I1 .GT. I2) THEN
	    TEMP = I1
	    I1 = I2
	    I2 = TEMP
	ENDIF
	RETURN
	END



	INTEGER FUNCTION SLEN(STRING)
	IMPLICIT NONE
	INTEGER	I
	CHARACTER*(*) STRING

	I = LEN(STRING)
	DO WHILE (ICHAR(STRING(I:I)) .EQ. 32 .AND. I .GT. 0)
	    I = I - 1
	ENDDO
	SLEN = I
	RETURN
	END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create edimage.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
C	Include file for EDIMAGE

C		Constant definitions
	INTEGER	MAXTOKENS, MAXTOKLEN
	INTEGER	MAXFILESAMPS, MAXMEMSAMPS
	INTEGER	MAXCOLORS, MAXPALBUF
	INTEGER	MAXBRUSHSIZE
	INTEGER	MAXNUMPOLY, MAXGRBUF
	INTEGER	MAXSEGS, MAXIMAGEBUF
	PARAMETER (MAXTOKENS = 32, MAXTOKLEN = 64)
	PARAMETER (MAXFILESAMPS = 32768, MAXMEMSAMPS = 4096)
	PARAMETER (MAXCOLORS = 32, MAXPALBUF = 3*16*2048)
	PARAMETER (MAXBRUSHSIZE = 32)
	PARAMETER (MAXNUMPOLY = 1024, MAXGRBUF = 32768)
	PARAMETER (MAXSEGS = 4096, MAXIMAGEBUF=65536+512)
	INTEGER	FULLCOLOR, BW, PSEUDO
	PARAMETER (FULLCOLOR = 1, BW = 2, PSEUDO = 3)

C		Global variable declarations

	INTEGER NUMINPFILES, NUMOUTFILES, NUMOUTIMAGES
	INTEGER	NUMIMAGES, UNIT(3), COLORMODE
	INTEGER	NLFILE, NSFILE, NLMEM, NSMEM, NLDISP, NSDISP
	INTEGER	SLFILE, SSFILE, NLD, NSD, SLMEM, SSMEM, HZOOMFAC
	LOGICAL	CHANGEDMEM

	INTEGER	COLORTABLE(3,MAXCOLORS), CURCOLOR(3)
	INTEGER	PALLINES, PALSAMPS, PALCOLORS
	INTEGER	SLPAL, SSPAL, NLPAL, NSPAL
	LOGICAL	PALETTEON
	BYTE	PALBUF(MAXPALBUF)
	INTEGER	BRUSHSIZE
	CHARACTER*16  BRUSHTYPE
	BYTE	BRUSHPAT(MAXBRUSHSIZE,MAXBRUSHSIZE)
	LOGICAL	BRUSHLON(MAXBRUSHSIZE)
	REAL	PAINTDENS
	INTEGER	TEXTSIZE, TEXTFONT, TEXTLOC
	REAL	TEXTSCALE, TEXTANGLE
	CHARACTER*(MAXTOKLEN) TEXTSTRING

	INTEGER	NUMPOLY, GRAPHPTR(MAXNUMPOLY), GRAPHLEN(MAXNUMPOLY) 
	REAL	GRAPHBUF(2,MAXGRBUF)

	INTEGER	NUMTOKENS, TOKENTYPE(MAXTOKENS)
	REAL	TOKENVALUE(MAXTOKENS)
	CHARACTER*(MAXTOKLEN)  TOKENSTR(MAXTOKENS)

	INTEGER	STRETCHLUT(256), PSEUDOLUT(256,3)
	INTEGER	DISPU, CURS, NIMPS, NLUTS, MAXLUTVAL
	INTEGER	LUTSECT, SETLUT, IGRAPH, GRIMP


C			Global common block
	COMMON /EDIMAGECOM/  NUMINPFILES, NUMOUTFILES, NUMOUTIMAGES,
     +		NUMIMAGES, UNIT,  COLORMODE, NLFILE, NSFILE,
     +		NLMEM, NSMEM, NLDISP, NSDISP, SLFILE, SSFILE,
     +		NLD, NSD, SLMEM, SSMEM, HZOOMFAC, CHANGEDMEM,
     +		COLORTABLE, CURCOLOR, PALLINES, PALSAMPS, PALCOLORS, 
     +		SLPAL, SSPAL, NLPAL, NSPAL, PALETTEON, BRUSHSIZE, 
     +		BRUSHTYPE, BRUSHLON, PAINTDENS, TEXTSIZE, TEXTFONT,
     +		TEXTLOC, TEXTSCALE, TEXTANGLE, NUMPOLY, GRAPHPTR, 
     +          GRAPHBUF, NUMTOKENS, TOKENTYPE, TOKENVALUE, STRETCHLUT,
     +		PSEUDOLUT, DISPU, CURS, NIMPS, NLUTS, MAXLUTVAL,LUTSECT,
     +		SETLUT, IGRAPH, GRIMP, GRAPHLEN, TOKENSTR, TEXTSTRING,
     +          PALBUF, BRUSHPAT

C	End of EDIMAGE include file
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create edimage.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM edimage

   To Create the build file give the command:

		$ vimake edimage			(VMS)
   or
		% vimake edimage			(Unix)


************************************************************************/


#define PROGRAM	edimage
#define R2LIB

#define MODULE_LIST edimage.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define INCLUDE_LIST edimage.fin
#define FTNINC_LIST verrdefs fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_VRDI
#define LIB_MATH77

/*#define DEBUG	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create edimage.pdf
PROCESS HELP=*
SUBCMD-DEFAULT MAIN       ! TCL COMMAND LINE PARAMETERS
 PARM INP      TYPE=STRING  COUNT=1:10
 PARM OUT      TYPE=STRING  COUNT=0:10  DEFAULT=--
 PARM SIZE     TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
 PARM SL       TYPE=INTEGER DEFAULT=1
 PARM SS       TYPE=INTEGER DEFAULT=1
 PARM NL       TYPE=INTEGER DEFAULT=0
 PARM NS       TYPE=INTEGER DEFAULT=0
 PARM MODE     TYPE=KEYWORD VALID=(COLOR,BW) DEFAULT=BW
 PARM SCREEN   TYPE=KEYWORD VALID=(CLEAR,NOCLR) DEF=CLEAR
 PARM TEST     TYPE=KEYWORD COUNT=0:1   VALID=TEST DEF=--
END-SUBCMD
!
SUBCMD IPARAM     ! INTERACTIVE PARAMETERS
 PARM COMMAND  TYPE=(STRING  ,80)
END-SUBCMD
!
END-PROC
.TITLE
VICAR Program "edimage"
.HELP

PURPOSE


    "edimage" is an interactive program that performs editing of gray scale
or color byte images.  Image are displayed and edited on a monitor.
Images may be painted with a brush, parts copied and filled, lines drawn,
and text annotation added.

.PAGE

EXECUTION


    Before running "edimage" the appropriate display device must be
allocated.  "edimage" starts out by copying the input image(s) to the
output image(s), if they exist.  If color images are to be edited
then there must be three input images (in red, green, blue order)
and the 'COLOR keyword must be specified.  The default is to edit
one image in gray scale mode.  The Vicar size field is used to copy
a window from the input image to the output image and only edit the
window area.  There may be extra input and output files: these are
used for IBIS graphics files and color lookup tables.
.PAGE
    By default "edimage" will clear the image planes before the session
begins(SCREEN="CLEAR"). The 'NOCLR keywork preserves whatever was on the image
planes before "edimage" was invoked;  This can be used to piece together a lot
of unrelated images displayed previously with IDX, etc. 

    The TEST keyword allows "edimage" to be run in a test mode.  (See under the
help for the TEST parameter.)
.PAGE

Examples:

  edimage  INPUT.IMG  OUTPUT.IMG  SIZE=(1500,1,1000,2500)

  edimage  (IM.R,IM.G,IM.B,POLY.GRA)  (IM2.R,IM2.G,IM2.B)  'COLOR

  edimage  (IM.R,IM.G,IM.B,POLY.GRA,CLR.TBL)  POLY2.GRA  'COLOR

  edimage  (INPUT.IMG,PSCOLOR.TBL)  (OUTPUT.IMG,PSCOLOR.TBL)


.PAGE
    "edimage" prints out some informative messages such as the size of the 
image being edited as the program starts up.  After the image has been 
copied and the display device initialized the program prompts for user input:

      Command: 

    The user will then type in a series of commands and use the trackball
and switches to perform the desired image editing.

Commands are of the form:

      COMMAND  KEYWORD = VALUE1,VALUE2   KEYWORD VALUE 

    Commands and keywords may be abbreviated and may be in upper or lower
case. Most any non-alphanumeric character can serve as a delimiter.  Equal 
signs and commas are optional.  Some keywords take multiple values others 
take no value.  Often the command serves as the keyword (e.g. STRETCH 50 200).

.PAGE
Image editing example:

 edimage (TM.RED,TM.GRN,TM.BLU) (TMNEW.RED,TMNEW.GRN,TMNEW.BLU) 'COLOR

 Commands entered:

  DISP SL 250 SS 100 NL 400 NS 400  Display a 400 square window from input
  STRETCH			    Perform trackball controlled stretch
  BRUSH SIZE 12 TYPE GAUSS	    Select gaussian brush
  COLOR AVERAGE			    Set current color to average under brush
  PAINT .5			    Airbrush a region
  Palet				    Display color palette at bottom of screen
  COLOR 6			    Select palette color 6 as current color
  DRAW BOX WIDTH=4		    Draw box in the image with current color
  EXIT				    Save modified image and exit program

    If the displayed image window does not fill the image memory planes the 
remaining empty region may be used as a scratch pad.
.PAGE

The following is a list of the available commands:

    DISPLAY  . . to display a region of the image
    STRETCH  . . to perform contrast stretches on the image
    HZOOM    . . to perform hardware zooming
    PAN      . . to pan around the display
    TABLE    . . to load and save display look up tables
    PALETTE  . . to control display of the color palette
    PSEUDO   . . to enter pseudo color mode
    COLOR    . . to set the current color
    BRUSH    . . to set the current brush
    PAINT    . . to paint the image with the brush
    DRAW     . . to draw lines in the image or graphics plane
    TEXT     . . to put text into the image

		 (more commands ...)
.PAGE

More available commands:

    POLYGON  . . to generate polygon graphics
    COPY     . . to copy polygonal image areas
    STATIST  . . to calculate image statistics in polygons
    FILL     . . to fill polygonal image areas
    TEXTURE  . . to texture polygonal image areas
    HELP     . . to get help on various commands
    COMMENT  . . to have a command line treated as a comment (i.e. ignored)
    EXIT     . . to save image planes and exit
    QUIT     . . to exit without saving image planes

.PAGE

    DISPLAY saves the current image memory planes in the file
and displays the new region of the image.  The default region
is as much of the upper left corner of the image as fits in the
image memory planes.  The following keywords select the region
to display:

 SL, SS  . . the starting line and sample in the image
 NL, NS  . . the number of lines and samples

    The NOSAVE keyword disables saving the current region before
displaying the new region; this is a good way of erasing mistakes.

.PAGE

    STRETCH performs contrast stretches on the display.  The stretch
is performed by changing the look up tables without changing the actual 
pixel values.  The stretch may be performed using various keywords:

 STRET n1 n2      performs a linear gray scale stretch between DN 
		  values n1 and n2
 STRET RED n1 n2  performs linear stretch on the red look up table.  
  		  Similarly for GREEN and BLUE.

    STRETCH by itself will perform a trackball controlled linear stretch.  
The vertical axis controls the contrast and the horizontal axis controls 
the offset.  If the program is in pseudo color mode then stretch performs 
a contrast stretch on the pseudo color table.

.PAGE
    HZOOM [n] performs a hardware zoom of the display.  If no value follows 
the command then the zoom factor is doubled from its previous value, otherwise 
the zoom factor is set to the new value. (Valid n range = 1 through 8)

    PAN puts the program in pan mode.  In pan mode the trackball is used to 
select the region of the image memory plane which will be displayed on the
monitor.  This is useful if the display is hardware zoomed or if the image 
memory planes are larger than the display.  Pressing switch 1 exits PAN mode.


    TABLE saves and loads display look up tables on disk.
The tables are in the same format that IDX and LOOKUP use.
Parameters:

 SAVE f    saves the current look up table in output file f
 LOAD f    loads the look up table from input file f
 NUMBER t  table number t loaded from file (default is 1)

.PAGE
    PALETTE controls the display of the palette.  The palette displays
the first 16 colors of the color table at the bottom of the screen.

 PALETTE       displays (or redisplays) the palette
 PALETTE OFF   erases the palette

    Zooming or panning will move the palette from the bottom of the display;
the PALETTE command will redisplay it along the bottom of the screen.

    The palette may be saved to and loaded from a file:

 PALETTE SAVE=file   saves palette colors in file
 PALETTE LOAD=file   loads palette colors from file
    The default filename is "edimage".PAL

    The palette file is an ASCII table of the 32 colors in the palette; 
the color number, red value, green value, and blue value are stored in 
each row. (Note: These files are not be entered on the VICAR command line.)

.PAGE
    PSEUDO changes the look up tables for pseudo color mode.
It enters pseudo color mode if in gray scale mode.

 OFF        turns off pseudo color, returning to gray scale
 TABLE [n]  selects the nth pseudo color table (same as IDX; 1-7, def=3)
 DN n1 [n2] sets the DN value [range] to change with RGB or COMP
 RGB r g b  sets the DN range to given red, green, and blue
 COMPOSE    enters interactive color composing mode

Note: PSEUDO does not change the palette colors or the current color, 
only the look up tables.
.PAGE
    COLOR changes the current color.  

 COLOR [n]  selects the nth palette color as the current color. If the n
is omitted the current color is changed. COLOR uses the following keywords:

 DN      sets the color to given gray scale value
 RGB     sets the color to the given red, green, and blue values
 CURSOR  sets the color to that of a selected pixel on the display
 AVERAGE sets the color to the average under the brush on the display
 COMPOSE enters the interactive color composing mode

Example:  COLOR 3  RGB 240 50 200

    Sets the current color to the particular RGB value and sets palette
color three to this color.  If no color number is specified then the
current color is changed, but the color table is not.
.PAGE
    BRUSH sets the size and type of the brush used for airbrushing in
PAINT mode.  Valid brush types are:

 DISK, SQUARE, DOME, GAUSSIAN.

    The SIZE parameter selects the brush diameter in pixels. (max = 32)

    The TYPE parameter selects the brush type.  

    If a parameter is not given then the old value will be used.  The brush 
size and type are displayed.

Examples:

	BRUSH SIZE 8 TYPE GAUSS
	BRUSH SIZE 3
	BRUSH TYPE DISK

.PAGE
    PAINT [d] puts the program in painting mode (airbrushing or dabbing). In 
paint mode the brush (marked by the cursor) is moved with the trackball. When 
switch 1 is held down the brush paints the image.  The painting involves 
a weighted average between the current color and the pixel values in the image.
The painting density (weight) may be changed by giving a new value after the 
PAINT command (d) (0< d <=1.0).  The starting paint density is .25 and if no
paint value is given the previous value is used.  Some brush types have 
variable painting densities over their surface.  A new current color may be 
selected inside paint mode by finding a pixel with the desired color and 
pressing switch 2.  Press switch 3 to leave paint mode.  See COLOR and BRUSH 
for related information.  PAINT mode requires large CPU resources:  response
will degrade significantly if adequate CPU is not available.

.PAGE
    DRAW draws line graphics of the current color into the image memory
planes or into the graphics plane (only with the GRAPHICS option of FILE).
The graphics lines drawn may have variable thickness (only in image plane
mode).  There are several ways of drawing the graphics into the image plane:

 default    draws the line as the cursor is moved
 LINE       draws segments between endpoints
 BOX        draws a rectangle from two corner points
 FILE [n]   draws graphics from input file n (def = first non-image input)
                  graphics from a file are in image file coordinates
 WIDTH [w]  draws lines with a width of w pixels (def = 1, max = 9)

    To draw graphics into the graphics plane:

 FILE [n] GRAPHICS [v]  draws IBIS graphics file from input file n with a 
	    value of v (def = 1; valid 0 through 7)
	0 = Black	1 = White	2 = Red		3 = Green
	4 = Blue	5 = Yellow	6 = Cyan	7 = Magenta
.PAGE
    TEXT sets the text parameters and draws text characters in the image 
memory planes.  The text parameters stay in effect until changed.  If a 
text string is given (with parameter STRING) then a cursor position is 
prompted for and the text is drawn on the graphics plane.  If the text
is accepted it is then burned into the image memory using the current color.
Text mode may exited with switch 3.

Parameters:

 STRING "the text string" 
 STRING    parameter with no value uses the last text string
 SIZE  h   height of the characters in pixels (def = 10)
 SCALE s   horizontal scale factor for text size (def = 1)
 ANGLE a   the angle from the horizontal axis in degrees
 FONT  f   the font number (see below)
 LEFT CENTER RIGHT  keywords for text justification
.PAGE


		      Text Font Descriptions

      Font     Description	      Font     Description
	0	Default Font	
	1	Simplex			8	Hollow
	2	Duplex			9	Cartographic
	3	Roman			10	Greek
	4	Standard		11	English Gothic
	5	Standard 2		12	German Gothic
	6	Standard Italics	13	Italian Gothic
	7	Script			14	Cyrillic
            There are other special fonts as well.

.PAGE
    POLYGON draws graphics on the graphics plane and stores the graphics 
internally for later use.  The graphics may be generated interactively or 
from an IBIS graphics file.  The graphics are in image file coordinates.

    Parameters:

 no keyword   enter interactive mode
 BOX [parm]   specify a rectangle from two corner points
               Optional parameter  SIZE SL SS NL NS
	       specifies a rectangle with standard SIZE window in image.
 READ [n]     reads graphics from input file n (def = first non-image input)
 WRITE [n]    writes graphics buffer to output file n (def = same as READ)
 CLEAR        clear graphics plane and graphics buffer

    See also FILL, COPY, and STAT.

.PAGE

    COPY copies the image inside of a polygon to another region of the
image memory plane.  The desired polygon is chosen by placing the cursor 
inside the polygon and pressing switch 1.  The polygon is highlighted 
and the user is asked for verification.  Another polygon may be chosen.
If the cursor is not in any polygon the command will not be performed.
The place to put the image area is chosen with another cursor selection.  
The area will be moved the distance between the first and second cursor 
selections.  Multiple copies may be made. (Note: Use reference points
to assist you in copying images; i.e. from and to points.)

.PAGE
    FILL fills the image area inside of a polygon.  The desired polygon
is chosen by placing the cursor inside the polygon and pressing switch 1. 
The polygon is highlighted and the user is asked for verification.  
Another polygon may be chosen.  If the cursor is not in any polygon the 
command will not be performed.

    There are three methods of filling:

 no keyword   The area is filled with the current color.
 TRANSPAR d   The area is filled with a weighted average of the original
	      image and the current color.  TRANS 1.0 is equivalent to 
	      the standard fill. (Valid range: 0< d <= 1.0)
 INTERP [r]   Interpolates the image area using the DN
              values at the polygon vertices.  r is the
              maximum radius for a vertex to be used.
 OUTSIDE      Fills outside of polygon instead of inside.

.PAGE

    STATISTICS calculates simple statistics for the image area inside of
a polygon.  The desired polygon is chosen by placing the cursor inside the 
polygon and pressing switch 1.  The number of pixels and the average, 
standard deviation, minimum, and maximum for each image plane within the
selected polygon is typed out.

.PAGE
    TEXTURE textures a polygonal image area using another area as a texture
pattern.  First, the polygon to texture is selected.  Second, the color stat-
istics polygon is selected unless the AVG or STD keywords are given.  The
color polygon is used to provide the color average and variance for the textured
area.  Finally, the texture pattern region is selected by selecting two corners
of a rectangle.  The rectangle is forced to be a power of two in size.  The
rectangle is highlighted, and the user may pick another one if desired.  The 
texture pattern rectangle is used to make a filter.  The filter is then 
convolved with gaussian noise to make the textured region.  A high pass filter
is used to cut out the low spatial frequencies and make a flat textured image.

 Keywords:
 AVG      the average DN value for each color band
 STD      the standard deviation for the color bands
 SIZE     size in pixels of the filter (default=16)
 HIPASS   cutoff size in pixels for the high pass (def=8)

.PAGE



    EXIT exits the program and saves the image memory planes
if they have been changed.


    QUIT exits the program without saving the current display 
screen.  Previous display screens will have already been saved, 
so the file may be changed.

.PAGE
OPERATION

    There are three different image coordinate systems that need to be
taken into account.  The first is the coordinates of the image file, 
i.e. the line and sample values of the pixels in the file.  The second 
coordinate system is the image memory planes.  The image memory planes 
are a window into the image file that is offset from the top left corner 
of the image file according to the SL and SS parameters in the DISPLAY 
command.  The third coordinate system is the display window.  The display 
window is a window into the image memory planes.  There may be an offset 
(from PANing) and there may be a scaling (from HZOOMimg).

.PAGE
RESTRICTIONS

The maximum number of samples in an image line is 32768.
The maximum number of samples in the image memory planes is 4096.
The number of colors in the palette is 32.
The maximum brush size is 32 pixels.
The maximum number of polygons is 1024.
The maximum number of polygon vertices is 16384.

Display Device Requirements:
  One image plane for gray scale mode.
  Three image planes for color mode.
  Graphics plane for polygon related commands.
  Must have hardware cursor.
  Must have cursor locating device (i.e. trackball) with three switches.
.PAGE

Original Programmer:   Frank Evans	May 1987

Cognizant Programmer:  Frank Evans

Revision:  New		May 1987
           Made portable for UNIX  AS  (CRI)  March 1995

.LEVEL1
.VARIABLE INP
Input images and optional 
IBIS graphics files and lookup 
table files. (Input image(s) 
modified if no output image(s)
specified.)
.VARIABLE OUT
Optional output images, IBIS
graphics files and lookup table
files. (Input image(s) copied
to output image(s).)
.VARIABLE SIZE
The standard Vicar size field
Used for determining window
to copy to output image.
.VARIABLE SL
The starting line
.VARIABLE SS
The starting sample
.VARIABLE NL
The number of lines
.VARIABLE NS
The number of samples
.VARIABLE MODE
Keyword for color mode
'COLOR or 'BW
.VARIABLE SCREEN
Keyword to initialize
screen:'CLEAR or 'NOCLR
.VARIABLE TEST
Enables test mode
.LEVEL2
.VARIABLE INP
    Input datasets may be images, optional IBIS graphics files, or lookup
table files. If no output image is specified, then the input image is modified
by the program. If an output image is given, then the input image is copied to 
the output image upon execution of the program.  Optional IBIS graphics files 
are used by the program in 3 modes.  They may 1) be displayed on the graphics 
plane for reference (such as a coastal outline); 2) contain closed polygons 
to be used by the program for polygonal operations (such as filling and 
copying); and 3) be drawn into either the image or graphics planes of the 
video memory.  The same filename can be used for an input and output graphics 
file (even if the file does not exist upon entering the program from the Vicar
command line).  This allows the user to create polygonal graphics files from 
within the program and save them to the output graphics file.  Similarly, 
lookup table files can be specified in the input and output command fields, 
generated from within the program, and saved and restored as desired.  The
lookup tables have the same format as those used by programs IDX and LOOKUP. 
If the input images (color mode) are being modified, they must have different 
filenames.  A total of 10 input files are supported by the program.
.VARIABLE OUT
    Output datasets may be images, IBIS graphics files, or color lookup
table files.  All three file types are optional.

Some possible examples are as follows:

 edimage MONALISA.BW			No output - input modified
 edimage MONALISA.BW NEWMONA.BW		Modified output image NEWMONA.BW
 edimage (MONA.R,MONA.G,MONA.B,MONA.GRA,MONA.TBL) +       Modified image will
	 (LISA.R,LISA.G,LISA.B,MONA.GRA,MONA.TBL) 'COLOR  will be written to
						          to the LISA.* files. 
Notice that the same name is given to the input and output files MONA.GRA 
and MONA.TBL, allowing graphics to be generated from within the program, 
saved to MONA.GRA and restored from input MONA.GRA at a later time within 
the same program session.  The same is true for the MONA.TBL color lookup 
table file.  A total of 10 output files may be specified from the command line.
.VARIABLE SIZE
The standard Vicar size field used for determining the window to copy to 
the output image.  This parameter is ignored if there are no output images
and the editing is being conducted on the input images.
.VARIABLE SL
The starting line in the editing window.
.VARIABLE SS
The starting sample in the editing window.
.VARIABLE NL
The number of lines in the editing window.
.VARIABLE NS
The number of samples in the editing window.
.VARIABLE MODE
    This is the keyword used for distinguishing between color and black and 
white mode. The program has no way of determining, without the MODE keyword, 
if one image and several IBIS graphics files and/or color lookup table files 
have been specified or if there are three images and optional files specified.
.VARIABLE SCREEN
    The part of the screen unused for image display may be used as a scratch-
pad, and it sometimes occurs that one wishes to use parts of various images
displayed on the image planes from a previous use of the display processor(eg,
another session of "edimage" or perhaps IDX).  By default "edimage" will clear the
image planes before the session begins(SCREEN="CLEAR").
The 'NOCLR keywork preserves whatever was on the image planes
before "edimage" was invoked;  This can be used to piece together
a lot of unrelated images displayed previously with IDX, etc.
.VARIABLE TEST
   The TEST parameter enables test mode.  The test mode differs from the
normal mode only in that the command lines are read using TAE parameter
processing, treating each command line as a single string.  This allows
"edimage" to be run from a TAE script file.  Each command line must be
enclosed in quotes if there are any embedded blanks.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstedimage.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"

use xwc0

enable-script tstedimage1.scr

end-proc
$!-----------------------------------------------------------------------------
$ create tstedimage1.scr
! This is a scaled-down version of tstedimage.scr, removing the 
! interactive portions so that it can be run as part of a general
! I&T test procedure.  (lwk - Mar 2010)

gen EDIMA 512 512  LINC=.3 SINC=.3

! TRY THE SIMPLEST CASES FIRST: EDIMAGE FOR COPYING AN IMAGE, WITH
! AND WITHOUT SIZE FIELD.   SHOULD GET 0 DIFFERENCES IN DIFPIC.
edimage EDIMA EDIMAO 'TEST
exit
!
difpic (EDIMA EDIMAO)
!
edimage EDIMA EDIMAO SIZE=(40,50,300,400) 'TEST
exit
!
copy EDIMA EDIMAO2 SIZE=(40,50,300,400) 
difpic (EDIMAO2 EDIMAO)
!
! NOW TRY SOME STANDARD EDIMAGE COMMANDS IN B/W MODE.
!
edimage EDIMA EDIMAO 'TEST
"COMMENT   The next command should display the GENed image."
disp
"COMMENT   Should display gray-scale type of palette"
palette
"COMMENT   Should draw a square box"
"POLYGON BOX SIZE 100 100 50 50"
"COMMENT   Should erase palette"
"PALETTE OFF"
exit

! NOW TRY SOME MORE COMMANDS AND THE QUIT COMMAND TO FINISH WITHOUT 
! STORING THE RESULTS.

edimage EDIMA (EDIMAO TAB) 'TEST
"COMMENT   The next command should display the GENed image."
disp
"COMMENT   Enter pseudo-color mode.  Should get a rainbow of diagonal"
"COMMENT   stripes."
"PSEUDO TABLE DN"
"COMMENT   Write pseudo color table to disk"
"TABLE SAVE"
"COMMENT   Stretch red,green, and blue.  Stripes should become"
"COMMENT   narrower.  Then stretch each color back one at a time to"
"COMMENT   bring display back to original rainbow"
"STRETCH 100 150"
"STRETCH RED 0 255"
"STRETCH BLUE 0 255"
"STRETCH GREEN 0 255"
"COMMENT  Display palette."
palette
"COMMENT  Set first palette color to deep pink."
"COLOR 1 RGB 240 50 200"
"COMMENT   Should zoom (magnify) picture by a factor of 2."
"HZOOM 2"
"COMMENT   Exit without changing output file."
quit
!  DIFPIC SHOULD SHOW 0 DIFFERENCES
difpic (EDIMA EDIMAO)
!
!  PRINT TABLE SIZE. SHOULD BE 1 BY 1024.
!
label-list TAB
!
! NOW TRY SOME MORE STANDARD EDIMAGE COMMANDS IN B/W MODE.
!
edimage EDIMA EDIMAO 'TEST 'NOCL
"COMMENT   The next command should display the GENed image."
disp
exit
!
! NOW TRY EDIMAGE COMMANDS IN COLOR MODE.
!
edimage (EDIMA,EDIMA,EDIMA)  (EDIMAOR,EDIMAOG,EDIMAOB) 'TEST 'COLOR
"COMMENT   The next command should display the white GENed image."
disp
"COMMENT   Stretch individual colors.  Should get colors and"
"COMMENT   end up with white."
"STRETCH RED 50 150"
"STRETCH BLUE 50 150"
"STRETCH GREEN 50 150"
exit
!
!  COMPARE THE EDIMAGE OUTPUT WITH THE INPUT.
!  SHOULD GET 2500 DIFFERENCES each.

difpic (EDIMA EDIMAOR)
difpic (EDIMA EDIMAOG)
difpic (EDIMA EDIMAOB)
!
ush rm EDIMA*
$!-----------------------------------------------------------------------------
$ create tstedimage.scr
!  This is the original EDIMAGE test script, written by Frank Evans.
!  It contains a number of interactive steps, which make it unsuitable for
!  the standard I&T test procedures.  Therefore a stripped-down version of
!  it, tstedimage1.scr, was created for use in tstedimage.pdf.
!  This version is retained for completeness.  (LWK - Mar.2010)

gen EDIMA 512 512  LINC=.3 SINC=.3
!
! TRY THE SIMPLEST CASES FIRST: EDIMAGE FOR COPYING AN IMAGE, WITH
! AND WITHOUT SIZE FIELD.   SHOULD GET 0 DIFFERENCES IN DIFPIC.
edimage EDIMA EDIMAO 'TEST
exit
!
difpic (EDIMA EDIMAO)
!
edimage EDIMA EDIMAO SIZE=(40,50,300,400) 'TEST
exit
!
copy EDIMA EDIMAO2 SIZE=(40,50,300,400) 
difpic (EDIMAO2 EDIMAO)
label-list EDIMAO
!
! NOW TRY SOME STANDARD EDIMAGE COMMANDS IN B/W MODE.
!
edimage EDIMA EDIMAO 'TEST
"COMMENT   The next command should display the GENed image."
disp
"COMMENT   Should display gray-scale type of palette"
palette
"COMMENT   Should draw a square box"
"POLYGON BOX SIZE 100 100 50 50"
"COMMENT   Should compute statistics.  Move cursor anywhere inside"
"COMMENT   box and press trackball SW1 twice.  Should get 2500"
"COMMENT   points in polygon with AVE = 73.604, STDEV = 6.171,  "
"COMMENT   MAX = 88, MIN = 59."
statist
"COMMENT   Leave cursor in box, and press SW1 twice.  Then move cursor"
"COMMENT   about two inches below box and press SW1 once and then"
"COMMENT   SW2 once.  Should copy contents of first box to the cursor"
"COMMENT   location."
copy
"COMMENT   Select color."
"COLOR DN 128"
"COMMENT   Move cursor back inside the first box and press SW1 twice."
"COMMENT   Should fill box with DN 128 throughout."
fill
"COMMENT   Should erase palette"
"PALETTE OFF"
"COMMENT   Should exit EDIMAGE, writing the modified image to the"
"COMMENT   output file."
exit
!  IF THE BOX WAS COPIED AND THEN FILLED, THEN DIFPIC SHOULD SHOW 
!  5000 DIFFERENCES
difpic (EDIMA EDIMAO)
! NOW TRY SOME MORE COMMANDS AND THE QUIT COMMAND TO FINISH WITHOUT 
! STORING THE RESULTS.
!
edimage EDIMA (EDIMAO TAB) 'TEST
"COMMENT   The next command should display the GENed image."
disp
"COMMENT   Should draw a square box"
"POLYGON BOX SIZE 100 100 50 50"
"COMMENT   Move cursor anywhere inside"
"COMMENT   box, and press SW1 twice.  Then move cursor"
"COMMENT   about two inches below box and press SW1 once and then"
"COMMENT   SW2 once.  Should copy contents of first box to the cursor"
"COMMENT   location."
copy
"COMMENT   Enter pseudo-color mode.  Should get a rainbow of diagonal"
"COMMENT   stripes."
"PSEUDO TABLE DN"
"COMMENT   Write pseudo color table to disk"
"TABLE SAVE"
"COMMENT   Stretch red,green, and blue.  Stripes should become"
"COMMENT   narrower.  Then stretch each color back one at a time to"
"COMMENT   bring display back to original rainbow"
"STRETCH 100 150"
"STRETCH RED 0 255"
"STRETCH BLUE 0 255"
"STRETCH GREEN 0 255"
"COMMENT  Display palette."
palette
"COMMENT  Set first palette color to deep pink."
"COLOR 1 RGB 240 50 200"
"COMMENT   Set brush type and size."
"BRUSH SIZE 30 TYPE DISK"
"COMMENT   Paint image by moving cursor with switch 1 down."
"COMMENT   SW2 should change paint color to color at cursor location."
"COMMENT   Exit with SW3."
"PAINT 1"
"COMMENT   Should zoom (magnify) picture by a factor of 2."
"HZOOM 2"
"COMMENT   Puts program in pan mode.  Steer through image with"
"COMMENT   trackball.  Exit with SW1."
pan
"COMMENT   Should return to zoom 1."
"HZOOM 1"
"COMMENT   Reset from PAN by pressing SW1."
pan
"COMMENT   Draw box.  Use cursor to select upper-left and lower-"
"COMMENT   right corners of box."
"DRAW BOX"
"COMMENT   Should display gray-scale type of palette"
"COMMENT   Exit without changing output file."
quit
!  DIFPIC SHOULD SHOW 0 DIFFERENCES
difpic (EDIMA EDIMAO)
!
!  PRINT TABLE SIZE. SHOULD BE 1 BY 1024.
!
label-list TAB
!
! NOW TRY SOME MORE STANDARD EDIMAGE COMMANDS IN B/W MODE.
!
edimage EDIMA EDIMAO 'TEST 'NOCL
"COMMENT   The next command should display the GENed image."
disp
"COMMENT   Should draw two square boxes"
"POLY    BOX SIZE 100 30 50 50"
"POLYGON BOX SIZE 200 30 50 50"
"COMMENT   Move cursor inside lower box near lower left corner."
"COMMENT   Press switches to put text into image."
"TEXT SIZE 30 FONT 5 left string ""FONT Standard 2"" "
"COMMENT   Select the top box for texturing.  Use the bottom box"
"COMMENT   for color and for texture sample.  Should put random"
"COMMENT   texture into top box."
texture
"COMMENT   Should exit EDIMAGE, writing the modified image to the"
"COMMENT   output file."
exit
!  DIFPIC SHOULD SHOW ROUGHLY 3000 TO 4000 DIFFERENCES
difpic (EDIMA EDIMAO)
!
! NOW TRY EDIMAGE COMMANDS IN COLOR MODE.
!
edimage (EDIMA,EDIMA,EDIMA)  (EDIMAOR,EDIMAOG,EDIMAOB) 'TEST 'COLOR
"COMMENT   The next command should display the white GENed image."
disp
"COMMENT   Stretch individual colors.  Should get colors and"
"COMMENT   end up with white."
"STRETCH RED 50 150"
"STRETCH BLUE 50 150"
"STRETCH GREEN 50 150"
"COMMENT   Should draw a square box"
"POLYGON BOX SIZE 100 100 50 50"
"COMMENT   Select color - DEEP PINK"
"COLOR 1 RGB 240 50 200"
"COMMENT   Move cursor back inside the first box and press SW1 twice."
"COMMENT   Should fill box with deep pink throughout."
fill
exit
!
!  COMPARE THE EDIMAGE OUTPUT WITH THE INPUT.
!  SHOULD GET 2500 DIFFERENCES each.

difpic (EDIMA EDIMAOR)
difpic (EDIMA EDIMAOG)
difpic (EDIMA EDIMAOB)
!
ush rm EDIMA*
$ Return
$!#############################################################################
