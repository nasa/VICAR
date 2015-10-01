$!****************************************************************************
$!
$! Build proc for MIPL module grafimg
$! VPACK Version 1.9, Monday, December 07, 2009, 16:24:19
$!
$! Execute by entering:		$ @grafimg
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
$ write sys$output "*** module grafimg ***"
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
$ write sys$output "Invalid argument given to grafimg.com file -- ", primary
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
$   if F$SEARCH("grafimg.imake") .nes. ""
$   then
$      vimake grafimg
$      purge grafimg.bld
$   else
$      if F$SEARCH("grafimg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake grafimg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @grafimg.bld "STD"
$   else
$      @grafimg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create grafimg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack grafimg.com -mixed -
	-s grafimg.f -
	-i grafimg.imake -
	-p grafimg.pdf -
	-t tstgrafimg.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create grafimg.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM GRAFIMG
C  6 MAR 1995  ...CRI...  MSTP S/W CONVERSION (VICAR PORTING)
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	IMPLICIT NONE

	INTEGER	UNITIN
	INTEGER	SL, SS, NL, NS
	REAL BUFFER(1024), IMAGE(256,256), X, Y, Z
	REAL ZMINIMG, ZMAXIMG, ZMIN, ZMAX, LTICK, STICK, ZTICK
	REAL XSCALE, YSCALE, ZSCALE, ZTOP, ZBTM
	INTEGER	LINE, SAMP, STATUS, COUNT, NLI, NSI
        INTEGER WRGR, PUTGR,CLGR
        INTEGER SUBSAM, SUBLIN, GS, GL
        INTEGER GRIDL, GRIDS, TYPE, LINETK, SAMPTK, ZTK
	CHARACTER*3 ZTICKS
	CHARACTER*4 CUBE, LTICKS
	CHARACTER*5 MODE, STICKS
	LOGICAL MAKECUBE, COMB, FIT

	COMMON /OUTCOM/ XSCALE, YSCALE, ZSCALE, IMAGE

        CALL IFMESSAGE('GRAFIMG version 06-MAR-95')

C OPEN THE INPUT IMAGE FILE

	CALL XVUNIT (UNITIN, 'INP', 1, STATUS,' ')
	CALL XVOPEN (UNITIN,STATUS,'IO_ACT','SA','OPEN_ACT',
     +               'SA','U_FORMAT','REAL',' ')

C GET SOME INPUT PARAMETERS

	CALL XVSIZE (SL,SS,NL,NS,NLI,NSI)
        CALL XVP('ZMIN',ZMIN,COUNT)
        CALL XVP('ZMAX',ZMAX,COUNT)
        CALL XVP('ZTOP',ZTOP,COUNT)
        CALL XVP('ZBTM',ZBTM,COUNT)
        CALL XVP('SINC',SUBSAM,COUNT)
        CALL XVP('LINC',SUBLIN,COUNT)
        CALL XVP('LTICKINC',LTICK,COUNT)
        CALL XVP('STICKINC',STICK,COUNT)
        CALL XVP('ZTICKINC',ZTICK,COUNT)

C ARE WE GOING TO DRAW ANY TICK MARKS ?
C   LINE

	LINETK = 0
	CALL XVP ('LTICKS',LTICKS,COUNT)
	IF (LTICKS(1:3).EQ.'TOP') THEN
	    LINETK = 1
	ELSEIF (LTICKS(1:3).EQ.'BTM') THEN
	    LINETK = 2
	ELSEIF (LTICKS(1:4).EQ.'BOTH') THEN
	    LINETK = 3
	ENDIF

C   SAMPLE

	SAMPTK = 0
	CALL XVP ('STICKS',STICKS,COUNT)
	IF (STICKS(1:4).EQ.'LEFT') THEN
	    SAMPTK = 1
	ELSEIF (STICKS(1:5).EQ.'RIGHT') THEN
	    SAMPTK = 2
	ELSEIF (STICKS(1:4).EQ.'BOTH') THEN
	    SAMPTK = 3
	ENDIF

C   Z AXIS

	ZTK = 0
	CALL XVP ('ZTICKS',ZTICKS,COUNT)
	IF (ZTICKS(1:2).EQ.'TL') THEN
	    ZTK = 1
	ELSEIF (ZTICKS(1:2).EQ.'TR') THEN
	    ZTK = 2
	ELSEIF (ZTICKS(1:2).EQ.'BL') THEN
	    ZTK = 3
	ELSEIF (ZTICKS(1:2).EQ.'BR') THEN
	    ZTK = 4
	ELSEIF (ZTICKS(1:3).EQ.'ALL') THEN
	    ZTK = 5
	ENDIF

C FIND OUT IF WE ARE TO PUT A CUBE AROUND THE GRID

	FIT = .FALSE.
	MAKECUBE = .FALSE.
	CALL XVP ('CUBE',CUBE,COUNT)
	IF (CUBE(1:4).EQ.'CUBE') MAKECUBE = .TRUE.
	IF (CUBE(1:3).EQ.'FIT') THEN
	    MAKECUBE = .TRUE.
	    FIT = .TRUE.
	ENDIF

C NOW TEST TO MAKE SURE THAT NO GREATER THAN A 256 X 256 MESH IS USED

        GS = ( NS - 1 ) / SUBSAM + 1
        GL = ( NL - 1 ) / SUBLIN + 1

C RESET IF GREATER THAN A 256 X 256 MESH WAS REQUESTED

        IF( GS .GT. 256 ) THEN
	    SUBSAM = NS / 256  +  1
	    GS = ( NS - 1 ) / SUBSAM + 1
        ENDIF
        IF( GL .GT. 256 ) THEN
	    SUBLIN = NL / 256 +  1 
	    GL = ( NL - 1 ) / SUBLIN + 1
        ENDIF

C	IF (NS .GT. 256 .OR. NL .GT. 256)  CALL MABEND('REGION TOO LARGE')

	ZMINIMG=999999.
	ZMAXIMG=-999999.
	DO GRIDL = 1, GL 
            LINE = SL + (GRIDL - 1)*SUBLIN 
	    CALL XVREAD (UNITIN, BUFFER, STATUS, 
     +			'LINE',LINE, 'SAMP',SS, 'NSAMPS',NS,' ')
	    DO GRIDS = 1, GS
                SAMP = 1 + (GRIDS - 1)*SUBSAM 
		IMAGE(GRIDS,GRIDL) = BUFFER(SAMP)
		IF (IMAGE(GRIDS,GRIDL).LT.ZMINIMG) ZMINIMG=IMAGE(GRIDS,GRIDL)
		IF (IMAGE(GRIDS,GRIDL).GT.ZMAXIMG) ZMAXIMG=IMAGE(GRIDS,GRIDL)
	    ENDDO
	ENDDO

	CALL XVCLOSE (UNITIN, STATUS,' ')

	CALL XVP ('XSCALE', XSCALE, COUNT)
	CALL XVP ('YSCALE', YSCALE, COUNT)
	CALL XVP ('ZSCALE', ZSCALE, COUNT)

	TYPE = 1
	COMB = .FALSE.
	CALL XVP ('MODE',MODE,COUNT)
	IF (MODE(1:4).EQ.'BOTH') TYPE = 1
	IF (MODE(1:4).EQ.'XONL') TYPE = 2
	IF (MODE(1:4).EQ.'YONL') TYPE = 3
	IF (MODE(1:4).EQ.'PREV') TYPE = 4
	IF (MODE(1:4).EQ.'DOTS') TYPE = 5

C		Preview and dots
	IF (MODE(1:4).EQ.'PRDT') THEN
	    COMB = .TRUE.
	    TYPE = 5
	ENDIF

	STATUS = WRGR (1, 1, 3)
        IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	IF (TYPE.NE.4) THEN
	 IF ((TYPE.EQ.1).OR.(TYPE.EQ.2).OR.(TYPE.EQ.5)) THEN
	  DO LINE = 1, GL, 2
	    DO SAMP = 1, GS
	      IF((IMAGE(LINE,SAMP).LT.ZMIN).OR.
     +         (IMAGE(LINE,SAMP).GT.ZMAX))THEN
		    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                    IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	      ELSE
		    CALL OUTGRAF (LINE, SAMP)
		    IF (TYPE.EQ.5) THEN
                      STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
                      IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
                    endif
	      ENDIF
	    ENDDO
	    STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    IF (LINE .LT. GL) THEN
	        DO SAMP = GS, 1, -1
	          IF((IMAGE(LINE,SAMP).LT.ZMIN).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZMAX)) THEN
		    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                    IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		  ELSE
		    CALL OUTGRAF (LINE+1, SAMP)
		    IF (TYPE.EQ.5) THEN
                      STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                      IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
                    endif
		  ENDIF
	        ENDDO
	        STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
                IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    ENDIF
	  ENDDO
	END IF

	IF (COMB) TYPE = 4	! Preview as well as dots

	IF ((TYPE.EQ.1).OR.(TYPE.EQ.3)) THEN
	  DO SAMP = 1, GS, 2
	    DO LINE = 1, GL
	        IF((IMAGE(LINE,SAMP).LT.ZMIN).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZMAX)) THEN
		    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                    IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		ELSE
		    CALL OUTGRAF (LINE, SAMP)
		ENDIF
	    ENDDO
	    STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    IF (SAMP .LT. GS) THEN
	        DO LINE = GL, 1, -1
	          IF((IMAGE(LINE,SAMP).LT.ZMIN).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZMAX)) THEN
		    STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
                    IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		  ELSE
		    CALL OUTGRAF (LINE, SAMP+1)
		  ENDIF
	        ENDDO
	        STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
                IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    ENDIF
	  ENDDO
	 ENDIF
	ENDIF

C PREVIEW MODE (PROFILE AROUND EDGE OF GRID)

	IF (TYPE.EQ.4) THEN
C	  Left
	    SAMP = 1
	    DO LINE = 1, GL
	        IF((IMAGE(LINE,SAMP).LT.ZBTM).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZTOP)) THEN
		    STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
                   IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		ELSE
		    CALL OUTGRAF (LINE, SAMP)
		ENDIF
	    ENDDO
C	  Bottom
	    LINE = GL
	    DO SAMP = 1, GS
	        IF((IMAGE(LINE,SAMP).LT.ZBTM).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZTOP)) THEN
		  STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                  IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		ELSE
		    CALL OUTGRAF (LINE, SAMP)
		ENDIF
	    ENDDO
C	  Right
	    SAMP = GS
	    DO LINE = GL, 1, -1
	        IF((IMAGE(LINE,SAMP).LT.ZBTM).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZTOP)) THEN
		   STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                   IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		ELSE
		    CALL OUTGRAF (LINE, SAMP)
		ENDIF
	    ENDDO
C	  Top
	    LINE = 1
	    DO SAMP = GS, 1, -1
	        IF((IMAGE(LINE,SAMP).LT.ZBTM).OR.
     +		     (IMAGE(LINE,SAMP).GT.ZTOP)) THEN
		   STATUS = PUTGR (1, 0.0, 0.0, 0.0)
                   IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
		ELSE
		    CALL OUTGRAF (LINE, SAMP)
		ENDIF
	    ENDDO
	    STATUS =  PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	ENDIF

C PUT THE CUBE AROUND THE DATA

	IF (MAKECUBE) THEN

	    IF (FIT) THEN
		ZBTM = ZMINIMG
		ZTOP = ZMAXIMG
	    ENDIF
	    X = FLOAT(GS) * XSCALE
	    Y = FLOAT(GL) * YSCALE
	    ZBTM = ZBTM * ZSCALE
	    Z = ZSCALE * IMAGE(1,1)
	    STATUS = PUTGR (1, YSCALE, XSCALE, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, YSCALE, XSCALE, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
   	    STATUS = PUTGR (1, YSCALE, X, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    Z = ZSCALE * IMAGE(GL,1)
	    STATUS = PUTGR (1, YSCALE, X, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    STATUS = PUTGR (1, YSCALE, X, ZBTM)
	    IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
            STATUS = PUTGR (1, Y, X, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    Z = ZSCALE * IMAGE(GL,GS)
	    STATUS = PUTGR (1, Y, X, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    STATUS = PUTGR (1, Y, X, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, Y, XSCALE, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    Z = ZSCALE * IMAGE(1,GS)
	    STATUS = PUTGR (1, Y, XSCALE, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    STATUS =  PUTGR (1, Y, XSCALE, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, YSCALE, XSCALE, ZBTM)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	  IF (ZTOP.NE.0.0) THEN

	    Z = ZSCALE * IMAGE(1,1)
	    ZTOP = ZTOP * ZSCALE
	    STATUS = PUTGR (1, YSCALE, XSCALE, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, YSCALE, XSCALE, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, YSCALE, X, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    Z = ZSCALE * IMAGE(GL,1)
	    STATUS = PUTGR (1, YSCALE, X, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    STATUS =  PUTGR (1, YSCALE, X, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, Y, X, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    Z = ZSCALE * IMAGE(GL,GS)
	    STATUS =  PUTGR (1, Y, X, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    STATUS = PUTGR (1, Y, X, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, Y, XSCALE, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    Z = ZSCALE * IMAGE(1,GS)
	    STATUS =  PUTGR (1, Y, XSCALE, Z)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	    STATUS = PUTGR (1, Y, XSCALE, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, YSCALE, XSCALE, ZTOP)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)
	    STATUS = PUTGR (1, 0.0, 0.0, 0.0)
            IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	  ENDIF
	ENDIF

	STATUS = CLGR (1)
        IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	RETURN
	END

	SUBROUTINE OUTGRAF (LINE, SAMP)
	IMPLICIT NONE
	INTEGER	LINE, SAMP, PUTGR,status
	REAL	IMAGE(256,256)
	REAL	X, Y, Z, XSCALE, YSCALE, ZSCALE
	COMMON /OUTCOM/ XSCALE, YSCALE, ZSCALE, IMAGE

	X = XSCALE*FLOAT(SAMP)
	Y = YSCALE*FLOAT(LINE)
	Z = ZSCALE*IMAGE(SAMP,LINE)
        STATUS = PUTGR (1, Y, X, Z)
        IF (STATUS.NE.1) CALL SIGNALGR(1,STATUS,1)

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create grafimg.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM grafimg

   To Create the build file give the command:

		$ vimake grafimg		(VMS)
   or
		% vimake grafimg		(Unix)


************************************************************************/


#define PROGRAM	grafimg
#define R2LIB

#define MODULE_LIST grafimg.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create grafimg.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING 
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM LINC TYPE=INTEGER DEFAULT=1
PARM SINC TYPE=INTEGER DEFAULT=1
PARM MODE TYPE=KEYWORD VALID=(BOTH,XONLY,YONLY,DOTS,PREV,PRDT) DEFAULT="BOTH"
PARM XSCALE TYPE=REAL DEFAULT=1.0
PARM YSCALE TYPE=REAL DEFAULT=1.0
PARM ZSCALE TYPE=REAL DEFAULT=1.0
PARM ZMIN TYPE=REAL DEFAULT=0.0
PARM ZMAX TYPE=REAL DEFAULT=32000.0
PARM CUBE TYPE=(STRING,4) VALID=(----,CUBE,FIT) DEFAULT="----"
PARM ZTOP TYPE=REAL DEFAULT=0.0
PARM ZBTM TYPE=REAL DEFAULT=0.0
PARM LTICKS TYPE=(STRING,4) VALID=(TOP,BTM,BOTH) DEFAULT="BOTH"
PARM STICKS TYPE=(STRING,4) VALID=(LEFT,RIGHT,BOTH) DEFAULT="BOTH"
PARM ZTICKS TYPE=(STRING,3) VALID=(TL,TR,BL,BR,ALL) DEFAULT="ALL"
PARM LTICKINC TYPE=REAL DEFAULT=10.0
PARM STICKINC TYPE=REAL DEFAULT=10.0
PARM ZTICKINC TYPE=REAL DEFAULT=10.0
END-PROC
.TITLE
VICAR/IBIS Program grafimg
.HELP
PURPOSE

    grafimg turns a Vicar image into a 3-D IBIS graphics-1 file by drawing
grid lines with z values depending on the pixel values.  The program accepts
a sarting line and sample in the image and a number of lines and samples in 
the image to 'look at' -- they define a window.  Additionally/optionally
one can subsample this window when turning l,s,DN into graphics coordinates.
SINC picks up every SINC'th pixel in the sample direction for conversion.
LINC picks up every LINC'th pixel in the line direction for conversion.
So the following pixels are converted to graphics points:
	SS, SS + SINC, SS + 2*SINC, ...   for sample coordinate
	SL, SL + LINC, SL + 2*LINC, ...   for line coordinates

    The largest grid of sampled points is 256 x 256. If the user specifies 
a window and subsampling that results in a grid larger than 256 x 256, 
then the program automatically chooses an integer subsampling value that 
will map the grid size down to fit inside 256 x 256.  This can be very 
tricky; an initial window of 257 x 257 and subsampling of 1 x 1 will
result in a 128 x 128 grid !  You can lose resolution in both the graphics
domain and the image domain if subsamplings are not chosen consistent with
the window size.

    The output grafics file can also include a cube, in which the data is
contained. Either the top or the bottom or both the top and the bottom of
the cube can be created. The cube will fit the min/max values by default,
but the user may specify the exact min and max if desired. The user may also
clip the creation of output grafics below or above a specific Z value (useful
if the image is a rotate elevation file within an image and contains vast
regions of zero DN). The output data set will also be clipped to the range of
the specified cube.

    The user may view the 3-D data in one of several formats. For a quick
preview of the dataset, the user may specify the PREView option in MODE, which
will create a graphics representation around the 4 sides of the data set
(similar to profile lines across terrain). The user may select DOTS, which will
create a dot at the intersection of the grid cells or PRDT (preview plus dots).
The conventional graphics file is the default, this being the BOTH option which
will create the normal "fishnet" graphics file. Either the XONLY or the YONLY
axis can be specified with the specific option to MODE.

EXECUTION

  grafimg ELEV.IMG ELEV.GRA ZSCALE=100

  grafimg SURFACE.IMG ELEV.3D LINC=5 SINC=5 MODE=PRDT ZMIN=500 ZMAX=2500 +
			      CUBE=CUBE ZTOP=3500 ZBTM=0


Original Programmer:  Frank Evans		February 1986
Cognizant Programmer: Ted Barragy / Richard Fretz    January 1988
Made portable for UNIX:   CRI                   06-MAR-95


.LEVEL1
.VARIABLE INP
The input image
.VARIABLE OUT
The output 3-D IBIS
graphics file
.VARIABLE SIZE
The Vicar size field
.VARIABLE SL
Starting line in image
.VARIABLE SS
Starting sample in image
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE LINC
Subsamples only every LINC'th
pixel in the line direction.
Line increment.
.VARIABLE SINC
Subsamples only every SINC'th
pixel in the sample direction.
Sample increment.
.VARIABLE MODE
Type of output file
generated. BOTH,
XONLY, YONLY, DOTS,
PREView (profile
around the grid)
and PRDT (dots and
preview combined).
.VARIABLE XSCALE
Multiplier scale factor
for sample numbers.
.VARIABLE YSCALE
Multiplier scale factor
for line numbers.
.VARIABLE ZSCALE
Multiplier scale factor
for pixel values.
.VARIABLE ZMIN
Minimum Z value to
be included in 3-D
graphics output file
.VARIABLE ZMAX
Maximum Z value to
be included in 3-D
graphics output file
.VARIABLE CUBE
    CUBE or FIT
Use CUBE to indicate
inclusion of either
ZTOP of cube and/or
ZBTM of cube around
the output grid. Use
FIT to fit a cube to
min/max of raw data.
.VARIABLE ZTOP
Elevation (DN) of the
top of the cube.
.VARIABLE ZBTM
Elevation (DN) of the
bottom of the cube.
.VARIABLE LTICKS
LINE tick mark switch.
TOP BTM or BOTH
.VARIABLE STICKS
SAMPLE tick mark switch.
LEFT RIGHT or BOTH
.VARIABLE ZTICKS
Z axis tick mark switch.
TL:top left TR:top right
BL:btm left BR:btm right
or ALL
.VARIABLE LTICKINC
Tick mark increment
for the LINE axis.
.VARIABLE STICKINC
Tick mark increment
for the SAMPLE axis.
.VARIABLE ZTICKINC
Tick mark increment
for the Z (DN) axis.
.LEVEL2
.VARIABLE INP
The input image
.VARIABLE OUT
The output 3-D IBIS
graphics file
.VARIABLE SIZE
The Vicar size field
.VARIABLE SL
Starting line in image
.VARIABLE SS
Starting sample in image
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE LINC
Subsamples only every LINC'th
pixel in the line direction.
Line increment.
.VARIABLE SINC
Subsamples only every SINC'th
pixel in the sample direction.
Sample increment.
.VARIABLE MODE
Type of output file
generated. BOTH,
XONLY, YONLY, DOTS,
PREView (profile
around the grid)
and PRDT (dots and
preview combined).
.VARIABLE XSCALE
Multiplier scale factor
for sample numbers.
.VARIABLE YSCALE
Multiplier scale factor
for line numbers.
.VARIABLE ZSCALE
Multiplier scale factor
for pixel values.
.VARIABLE ZMIN
Minimum Z value to
be included in 3-D
graphics output file
.VARIABLE ZMAX
Maximum Z value to
be included in 3-D
graphics output file
.VARIABLE CUBE
    CUBE or FIT
Use CUBE to indicate
inclusion of either
ZTOP of cube and/or
ZBTM of cube around
the output grid. Use
FIT to fit a cube to
min/max of raw data.
.VARIABLE ZTOP
Elevation (DN) of the
top of the cube.
.VARIABLE ZBTM
Elevation (DN) of the
bottom of the cube.
.VARIABLE LTICKS
LINE tick mark switch.
TOP BTM or BOTH
.VARIABLE STICKS
SAMPLE tick mark switch.
LEFT RIGHT or BOTH
.VARIABLE ZTICKS
Z axis tick mark switch.
TL:top left TR:top right
BL:btm left BR:btm right
or ALL
.VARIABLE LTICKINC
Tick mark increment
for the LINE axis.
.VARIABLE STICKINC
Tick mark increment
for the SAMPLE axis.
.VARIABLE ZTICKINC
Tick mark increment
for the Z (DN) axis.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgrafimg.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
gen a nl=5 ns=5
grafimg a agr 'both
ibis-list agr gr1=3  
grafimg a agr 'xonly
ibis-list agr gr1=3  
grafimg a agr 'yonly
ibis-list agr gr1=3  
grafimg a agr cube=cube
ibis-list agr gr1=3  
grafimg a agr 'prev
ibis-list agr gr1=3  
grafimg a agr 'dot
ibis-list agr gr1=3  
gen a nl=200 ns=200
grafimg a agr linc=40 sinc=40
ibis-list agr gr1=3  
end-proc

$ Return
$!#############################################################################
