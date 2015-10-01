$!****************************************************************************
$!
$! Build proc for MIPL module stretvar
$! VPACK Version 1.7, Thursday, April 14, 1994, 14:57:51
$!
$! Execute by entering:		$ @stretvar
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
$ write sys$output "*** module stretvar ***"
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
$ write sys$output "Invalid argument given to stretvar.com file -- ", primary
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
$   if F$SEARCH("stretvar.imake") .nes. ""
$   then
$      vimake stretvar
$      purge stretvar.bld
$   else
$      if F$SEARCH("stretvar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stretvar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stretvar.bld "STD"
$   else
$      @stretvar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretvar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretvar.com -
	-s stretvar.f -
	-i stretvar.imake -
	-p stretvar.pdf -
	-t tststretvar.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stretvar.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C VICAR PROGRAM--STRETVAR
C     PURPOSE: Perform a linear stretch such that the low and high
C	   stretch limits vary as a function of line number.
C          The stretch limits are specified by the user for a number
C          of discrete lines via the TABLE parameter.  The stretch
C          limits for each line between those specified are computed
C          by interpolation.  The linear stretch is performed line-
C          by-line by computing a look-up table and calling LUT.
C
C     USER PARAMETERS:
C          TABLE, L(1),INDN1(1),OUTDN1(1),INDN2(1),OUTDN2(1),
C                 L(2),INDN1(2),OUTDN1(2),INDN2(2),OUTDN2(2),
C                                  . . .
C                 L(N),INDN1(N),OUTDN1(N),INDN2(N),OUTDN2(N)
C           - All parameters are integers.
C             L(K) specifies the line number and
C             INDN1(K),OUTDN1(K),INDN2(K),OUTDN2(K) define
C             the low and high stretch limits for the input and
C             output, such that INDN1 maps to OUTDN1 and INDN2
C             maps to OUTDN2.
C
      INCLUDE 'fortport'
      COMMON/C1/BUF(65536),PAR(250),STBL(256)
      COMMON/C1/LINEST(50),INDN1(50),INDN2(50),OUTDN1(50),OUTDN2(50)
      INTEGER*4 PAR,OUTDN1,OUTDN2
      BYTE BUF,STBL
      CHARACTER*5 FORMAT


      CALL IFMESSAGE('STRETVAR version 02-MAY-94')
      CALL XVUNIT(IDSRN,'INP',1,IND,' ')
      CALL XVOPEN(IDSRN,IND,' ')
      CALL XVGET(IDSRN,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'BYTE') GOTO 993

      CALL XVUNIT(ODSRN,'OUT',1,IND,' ')
      CALL XVOPEN(ODSRN,IND,'OP','WRITE',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)

      CALL XVP('TABLE',PAR,NVAL)
      NTAB = NVAL/5
      IF (5*NTAB.NE.NVAL) GOTO 998
      IF (NTAB.LT.2) GOTO 995

      DO I=1,NTAB
          J = (I-1)*5 + 1
          LINEST(I) = PAR(J)
          IF (I.GT.1.AND.LINEST(I).LE.LINEST(I-1)) GOTO 994
          INDN1(I) = PAR(J+1)
          INDN2(I) = PAR(J+3)
          OUTDN1(I) = PAR(J+2)
          OUTDN2(I) = PAR(J+4)
      ENDDO

      IF (LINEST(1).NE.1 .OR. LINEST(NTAB).NE.NL) GOTO 995
C
      NSTRIP = NTAB-1
C
C            Begin processing strip by strip.
      DO 500 ISTRIP=1,NSTRIP
          LINE1 = LINEST(ISTRIP)
          LINE2 = LINEST(ISTRIP+1)
          IF(ISTRIP.LT.NSTRIP) LINE2=LINE2-1
          IX1 = INDN1(ISTRIP)
          IX2 = INDN2(ISTRIP)
          IY1 = OUTDN1(ISTRIP)
          IY2 = OUTDN2(ISTRIP)
	  X1 = IX1
	  X2 = IX2
	  Y1 = IY1
	  Y2 = IY2
          X3 = INDN1(ISTRIP+1)
          X4 = INDN2(ISTRIP+1)
          Y3 = OUTDN1(ISTRIP+1)
          Y4 = OUTDN2(ISTRIP+1)
          GAP = LINE2 - LINE1
          IF(ISTRIP.LT.NSTRIP) GAP=GAP+1
          DELX1 = (X3-X1)/GAP
          DELX2 = (X4-X2)/GAP
          DELY1 = (Y3-Y1)/GAP
          DELY2 = (Y4-Y2)/GAP

          DO LINE=LINE1,LINE2
              IF(LINE/100*100.EQ.LINE) CALL PRNT(4,1,LINE,' LINE.')
C                   Compute stretch table for current line.
              CALL MVE(-5,IX1+1,IY1,STBL,0,1)
              CALL MVE(-5,256-IX2,IY2,STBL(IX2+1),0,1)
              J1 = IX1+2
              OUTDN = IY1 + 0.5
              DELTAB = (Y2-Y1)/(X2-X1)

              DO J=J1,IX2
                  OUTDN = OUTDN + DELTAB
                  IVAL = OUTDN
                  STBL(J) = INT2BYTE(IVAL)
              ENDDO
C
C		   Stretch the current line....
              CALL XVREAD(IDSRN,BUF,IND,' ')
              CALL LUT(NS,BUF,STBL,BUF)
              CALL XVWRIT(ODSRN,BUF,IND,' ')
              X1 = X1 + DELX1
              X2 = X2 + DELX2
              Y1 = Y1 + DELY1
              Y2 = Y2 + DELY2
              IX1 = X1 + .5
              IX2 = X2 + .5
              IY1 = Y1 + .5
              IY2 = Y2 + .5
          ENDDO
  500 CONTINUE

      CALL XVCLOSE(IDSRN,IND,' ')
      CALL XVCLOSE(ODSRN,IND,' ')
      CALL XVMESSAGE('STRETVAR task completed',' ')
      RETURN

  993 CALL XVMESSAGE('***Input image must be in byte format',' ')
      GOTO 999
  994 CALL XVMESSAGE('***TABLE line numbers must increase',' ')
      GOTO 999
  995 CALL XVMESSAGE('***TABLE must include lines 1 and NL',' ')
      GOTO 999
  998 CALL XVMESSAGE('***TABLE entries must be in multiples of 5',' ')
  999 CALL XVMESSAGE('***STRETVAR task cancelled',' ')
      CALL ABEND
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create stretvar.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM stretvar

   To Create the build file give the command:

		$ vimake stretvar			(VMS)
   or
		% vimake stretvar			(Unix)


************************************************************************/


#define PROGRAM	stretvar
#define R2LIB

#define MODULE_LIST stretvar.f

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
$ create stretvar.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM TABLE TYPE=INTEGER COUNT=5:250 DEFAULT=(1,0,0,0,0)
END-PROC
.TITLE
VICAR2 Program stretvar
.HELP
PURPOSE:

stretvar perform a linear stretch such that the low and high stretch
limits vary as a function of line number.  The stretch limits are
specified by the user for a number of discrete lines via the TABLE
parameter.  The stretch limits for each line between those specified
are computed by interpolation.

This program is useful in cases where the picture contrast varies
systematically for the top to the bottom of the image.

.page
EXECUTION:

	stretvar IN OUT +
            TABLE=(LINE1,ILOW1,OLOW1,IHIGH1,OHIGH1,+
                   LINE2,ILOW2,OLOW2,IHIGH2,OHIGH2,+
                                  . . .
                                  . . .
		   LINEn,ILOWn,OLOWn,IHIGHn,OHIGHn)

 All values in the TABLE parameter are integers.  The values are
 entered in sets of five, where LINEj specifies the line number and
 ILOWj, OLOWj, IHIGHj, OHIGHj specify the low and high stretch limits
 for the input and output images such that:

		ILOWj maps to OLOWj
            and IHIGHj maps to OHIGHj

 The lines must be specified in ascending order.  LINE1 must be the
 first line of the image (i.e. LINE1=1) and LINEn must be the last line
 of the image.  The stretch limits may be explicitly specified for a
 maximum of 50 lines.  Note that TAE also restricts command line
 length to 2048 characters.

 Note that although stretvar will accept a size field specification,
 SL and SS are ignored.

.page
OPERATION:

 For the specified lines L1,L2,...,Ln, stretvar performs a linear stretch
 as specified by the low and high stretch limits for the input and output
 images.  Let IDN and ODN represent the input and output DN values for
 a given pixel.  Then
			     (OHIGH - OLOW)
		ODN = OLOW + -------------- X (IDN - ILOW)
			     (IHIGH - ILOW)

 For every line L between Lj and Lj+1 the low and high stretch limits
 are computed by linear interpolation:

			       ILOWj+1 - ILOWj
		ILOW = ILOWj + --------------- X (L - Lj)
				  Lj+1 - Lj
 
.page
EXAMPLE:

	stretvar INP=A OUT=B TABLE=(1,0,0,255,255,+
 			          100,127,0,128,255)
 
 	The input image A is assumed to contain 100 lines.  The applied
        stretch will gradually increase in severity, with no contrast
        change for the first line of the image, and separation into two
	grey levels for the last line.

.page 
 WRITTEN BY:  B. Gokhman, 10 March 1983
 COGNIZANT PROGRAMMER:  G.M. Yagi

 PROGRAM HISTORY:
  29 MAR    1994...CRI...MSTP S/W CONVERSION (VICAR PORTING)
  28 AUG    1986...G.M.Yagi...............Fix bug in call to MVE
  26 AUG    1986...G.W.Garneau............Convert to VICAR2
   6 SEP    1983...A.S.M..................VICAR1 conversion

.LEVEL1
.VARIABLE INP
STRING - Input image file
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Standard VICAR size field
.VARIABLE SL
INTEGER - Starting line (ignored)
.VARIABLE SS
INTEGER - Starting sample (ignored)
.VARIABLE NL
INTEGER - Number of lines
.VARIABLE NS
INTEGER - Number of samples
.VARIABLE TABLE
INTEGER - Table specifications
.LEVEL2
.VARIABLE TABLE

            TABLE=(LINE1,ILOW1,OLOW1,IHIGH1,OHIGH1,+
                   LINE2,ILOW2,OLOW2,IHIGH2,OHIGH2,+
                                  . . .
                                  . . .
		   LINEn,ILOWn,OLOWn,IHIGHn,OHIGHn)

 All values in the TABLE parameter are integers.  The values are
 entered in sets of five, where LINEj specifies the line number and
 ILOWj, OLOWj, IHIGHj, OHIGHj specify the low and high stretch limits
 for the input and output images such that:

		ILOWj maps to OLOWj
            and IHIGHj maps to OHIGHj

 The lines must be specified in ascending order.  LINE1 must be the
 first line of the image (i.e. LINE1=1) and LINEn must be the last line
 of the image.  The stretch limits may be explicitly specified for a
 maximum of 50 lines.  Note that TAE also restricts command line
 length to 2048 characters.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststretvar.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen A 20 10 LINC=0 SINC=25
stretvar A B TABLE=(1,0,0,255,255,20,127,0,128,255)
list B
end-proc
$ Return
$!#############################################################################
