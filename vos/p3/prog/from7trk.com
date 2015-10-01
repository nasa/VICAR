$!****************************************************************************
$!
$! Build proc for MIPL module from7trk
$! VPACK Version 1.8, Thursday, April 16, 1998, 19:34:43
$!
$! Execute by entering:		$ @from7trk
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
$ write sys$output "*** module from7trk ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to from7trk.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("from7trk.imake") .nes. ""
$   then
$      vimake from7trk
$      purge from7trk.bld
$   else
$      if F$SEARCH("from7trk.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake from7trk
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @from7trk.bld "STD"
$   else
$      @from7trk.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create from7trk.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack from7trk.com -
	-s from7trk.f -
	-p from7trk.pdf -
	-i from7trk.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create from7trk.f
$ DECK/DOLLARS="$ VOKAGLEVE"
        INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44 
C
	BYTE ARR(60000),ARR2(24000)
	BYTE BUF(482),EBCDIC(360)
	CHARACTER*360 ASCII
	CHARACTER*80 INFILE,MSG
	LOGICAL XVPTST,QADD,QFIRST,QMORE
C
	QADD = .NOT. XVPTST('NOADD')
C								     open input
	CALL XVPARM('INP',INFILE,ICNT,IDEF,0)
	OPEN (73,FILE=INFILE)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
C							   read IBM VICAR labels
	QFIRST = .TRUE.
	QMORE = .TRUE.
	DO WHILE (QMORE)
	    READ (73,10) BUF
   10	    FORMAT(482A1)
	    CALL TRANS_FROM7TRK(BUF(3),EBCDIC,480)
C						    convert from EBCDIC to ASCII
	    CALL TO_ASCII(EBCDIC,ASCII,360)
	    IF (QFIRST) THEN
		CALL READ_FIRST_LABEL(ASCII,IOUT,NL,NS,QMORE)
		QFIRST = .FALSE.
		LABNUM = 6
	    ELSE
		CALL READ_OTHER_LABELS(ASCII,IOUT,QMORE,LABNUM)
	    END IF
	END DO
	LABNUM = LABNUM - 1
	CLOSE(73)
C				   compute label offset and input bytes per line
	NREC = ((LABNUM-1)/5) + 1
	IOFFSET = 480*NREC + 5
	X = 4.0*NS/3.0
	NSIN = X
	IF (X .NE. FLOAT(NSIN)) NSIN=NSIN+1
	NSIN = NSIN+3
C						     re-open inp as a vicar file
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_NL',NL,'U_NS',NSIN,'OPEN_ACT','SA',
     +		    'IO_ACT','S','COND','NOLABELS',' ')
	CALL XVREAD(INUNIT,ARR,ISTAT,' ')
	LINE = 0
	DO WHILE (LINE .LT. NL)
	    CALL XVREAD(INUNIT,ARR(NSIN+1),ISTAT,' ')
	    IF (ARR(IOFFSET-1).NE.12) THEN
		CALL ADJUST_OFFSET(ARR,NSIN,IOFFSET,LINE)
		IF (QADD) THEN
		    CALL XVWRIT(IOUT,ARR2,ISTAT,' ')
		    LINE = LINE + 1
		    WRITE (MSG,100) LINE
  100		    FORMAT('Line ',I5,' added')
		    CALL XVMESSAGE(MSG,' ')
		END IF
	    END IF
	    CALL TRANS_FROM7TRK(ARR(IOFFSET),ARR2,NSIN)
	    CALL XVWRIT(IOUT,ARR2,ISTAT,' ')
	    LINE = LINE + 1
	    CALL MVE(1,NSIN,ARR(NSIN+1),ARR(1),1,1)
	END DO
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C*******************************************************************************
	SUBROUTINE TRANS_FROM7TRK(BUF,OUTARR,LEN)
C
C	This subroutine "unpacks" the 7 track encoded byte array into a
C	conventional byte array.
C
	BYTE BUF(*),OUTARR(*)
C
	J = 1
	DO I=1,LEN,4
	    OUTARR(J) = 4*BUF(I) + BUF(I+1)/16
	    OUTARR(J+1) = 16*MOD(BUF(I+1),16) + BUF(I+2)/4
	    OUTARR(J+2) = 64*(MOD(BUF(I+2),4)) + BUF(I+3)
	    J = J+3
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE TO_ASCII(EBC,ASC,LEN)
C
C	This routine translates the EBCDIC byte array EBC into the ASCII
C	string ASC.  The length of the strings is LEN characters.
C
	BYTE EBC(*)
	CHARACTER*(*) ASC
	INTEGER LUT(256)/
     +	  0,  1,  2,  3, -1,  9, -1,127, -1, -1, -1, 11, 12, 13, 14, 15,
     +   16, 17, 18, -1, -1, -1,  8, -1, 26, 27, -1, -1, 28, 29, 30, 31,
     +   -1, -1, 28, -1, -1, 10, 23, 27, -1, -1, -1, -1, -1,  5,  6,  7,
     +   -1, -1, 22, -1, -1, 30, -1,  4, -1, -1, -1, -1, 20, 21, -1, 26,
     +   32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, 60, 40, 43,124,
     +   38, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, 36, 42, 41, 59, -1,
     +   45, 47, -1, -1, -1, -1, -1, -1, -1, -1,124, 44, 37, 95, 62, 63,
     +   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 58, 96, 64, 39, 34, 61,
     +   -1, 97, 98, 99,100,101,102,103,104,105, -1, -1, -1, -1, -1, -1,
     +   -1,106,107,108,109,110,111,112,113,114, -1, -1, -1, -1, -1, -1,
     +   -1,126,115,116,117,118,119,120,121,122, -1, -1, -1, -1, -1, -1,
     +   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     +  123, 65, 66, 67, 68, 69, 70, 71, 72, 73, -1, -1, -1, -1, -1, -1,
     +  125, 74, 75, 76, 77, 78, 79, 80, 81, 82, -1, -1, -1, -1, -1, -1,
     +   92, -1, 83, 84, 85, 86, 87, 88, 89, 90, -1, -1, -1, -1, -1, -1,
     +   48, 49, 50, 51, 52, 53, 54, 55, 56, 57,124, -1, -1, -1, -1, -1/
C
	DO I=1,LEN
	    IF (EBC(I) .LT. 0) THEN
		N = EBC(I) + 257
	    ELSE
		N = EBC(I) + 1
	    END IF
	    IF (N .GT. 0) THEN
		ASC(I:I) = CHAR(LUT(N))
	    ELSE
		ASC(I:I) = ' '
	    END IF
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_FIRST_LABEL(ASCII,IOUT,NL,NS,QMORE)
C
C	This subroutine reads the first 360 byte block of IBM VICAR labels,
C	interprets and reports the system values, opens the output file, and
C	copies any history labels into the output file.
C
	CHARACTER*360 ASCII
	CHARACTER*80 MSG
	CHARACTER*4 FORMAT
	CHARACTER*4 NAME(5)/'LAB1','LAB2','LAB3','LAB4','LAB5'/
	CHARACTER*2 C77
	CHARACTER*1 FMT,CONT
	LOGICAL QMORE
C							       read system label
	READ (ASCII,100) C77,NL,NS,FMT,CONT
  100	FORMAT(A2,30X,I4,I4,1X,A1,29X,A1)
	READ (ASCII,110,ERR=120) IPIXSIZE
  110	FORMAT(42X,I2)
  120	CONTINUE
C								77 format label?
	IF (C77 .EQ. '77') THEN
	    READ (ASCII,130,ERR=140) NL77,NS77
  130	    FORMAT(16X,I8,I8)
	    IF (NL77 .GT. 0) NL = NL77
	    IF (NS77 .GT. 0) NS = NS77
	END IF
  140	CONTINUE
C								get pixel format
	IF (IPIXSIZE .EQ. 0) IPIXSIZE=1
	IF (FMT .EQ. 'C') THEN
	    FORMAT = 'COMP'
	ELSE IF (FMT .EQ. 'R') THEN
	    IF (IPIXSIZE .EQ. 8) THEN
		FORMAT = 'DOUB'
	    ELSE
		FORMAT = 'REAL'
	    END IF
	ELSE IF (FMT .EQ. 'I') THEN
	    IF (IPIXSIZE .EQ. 4) THEN
		FORMAT = 'FULL'
	    ELSE IF (IPIXSIZE .EQ. 2) THEN
		FORMAT = 'HALF'
	    ELSE
		FORMAT = 'BYTE'
	    END IF
	ELSE
	    FORMAT = 'BYTE'
	END IF
C							report system parameters
	WRITE (MSG,200) NL,NS,FORMAT
  200	FORMAT('***Input image is ',I7,' lines by ',I7,' samples of ',
     +	       A4,' data***')
	CALL XVMESSAGE(MSG,' ')
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(ASCII(1:72),' ')
C								     open output
	CALL XVOPEN(IOUT,ISTAT,'U_NL',NL,'U_NS',NS/IPIXSIZE,
     +		    'OP','WRITE','U_FORMAT',FORMAT,'O_FORMAT',FORMAT,
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XLADD(IOUT,'HISTORY',NAME(1),ASCII(1:72),ISTAT,'FORMAT',
     +		   'STRING',' ')
C							     copy history labels
	QMORE = (CONT .EQ. 'C')
	DO I=2,5
	    J = 72*I
	    IF (QMORE) THEN
		CALL XVMESSAGE(ASCII(J-71:J),' ')
		CALL XLADD(IOUT,'HISTORY',NAME(I),ASCII(J-71:J),ISTAT,
     +			   'FORMAT','STRING',' ')
		QMORE = (ASCII(J:J) .EQ. 'C')
	    END IF
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_OTHER_LABELS(ASCII,IOUT,QMORE,LABNUM)
C
C	This subroutine reads any subsequent 360 byte blocks of IBM VICAR
C	labels, and copies the history labels into the output file.
C
	CHARACTER*360 ASCII
	CHARACTER*5 NAME
	LOGICAL QMORE
C
	DO I=1,5
	    J = 72*I
	    IF (QMORE) THEN
		CALL XVMESSAGE(ASCII(J-71:J),' ')
		IF (LABNUM .LT. 10) THEN
		    WRITE (NAME,100) LABNUM
  100		    FORMAT('LAB',I1)
		ELSE
		    WRITE (NAME,200) LABNUM
  200		    FORMAT('LAB',I2)
		END IF
		CALL XLADD(IOUT,'HISTORY',NAME,ASCII(J-71:J),ISTAT,
     +			   'FORMAT','STRING',' ')
		QMORE = (ASCII(J:J) .EQ. 'C')
		LABNUM = LABNUM + 1
	    END IF
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE ADJUST_OFFSET(ARR,LEN,IOFFSET,LINE)
C
	CHARACTER*80 MSG
	BYTE ARR(*)
C
	DO I=IOFFSET-1,LEN-1
	    IF (ARR(I+1) .EQ. 12) THEN
		IOFFSET = I + 2
		RETURN
	    END IF
	END DO
C
	DO I=IOFFSET-2,1,-1
	    IF (ARR(I+1) .EQ. 12) THEN
		IOFFSET = I + 2
		RETURN
	    END IF
	END DO
C
	WRITE (MSG,100) LINE
  100	FORMAT ('Adjustment needed but not made, line ',I5)
	CALL XVMESSAGE(MSG,' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create from7trk.pdf
process help=*
PARM INP     TYPE=(STRING,80)   COUNT=1
PARM OUT     TYPE=(STRING,80)   COUNT=1
PARM NOADD   TYPE=KEYWORD  VALID=NOADD DEFAULT=-- COUNT=(0:1)
END-PROC
.TITLE
FROM7TRK
.HELP
 PURPOSE:
      The Science Digital Data Preservation Task (SDDPT) has transcribed the
data from a large number of archived 1/2 inch CCT tapes onto a set of CD-ROM's.
While the datasets transcribed from 9 track tapes are, in general, readable
by way of previously existing software, the datasets transcribed from 7 track
tapes need a reformatting step to be usable by current VICAR programs. 
FROM7TRK is a VICAR program that provides this reformatting step.
      Since 7 track tape drives were used only during the IBM era, translation
of the IBM VICAR labels (in EBCDIC) into VICAR2 (ASCII) labels was also
necessary.
.PAGE
 RESTRICTIONS:
      While this program attempts to handle any possible valid IBM VICAR
label, there were no available test datasets for numerous options, and a
complete list of possible valid VICAR label formats is lost to the mists
of time.  In particular, the following options have been programmed in
but not tested: non-byte data, VICAR labels longer than 360 bytes, and the
LABEL77 label format.
      If this program encounters a problem with any of these formats, please
contact the cognizant programmer listed on the following page.
.PAGE
 WRITTEN BY:             Ron Alley                   16 April 1998

 COGNIZANT PROGRAMMER:   Ron Alley                   16 April 1998

 REVISION:               New
.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name
.VARIABLE NOADD
Use the keyword NOADD
to suppress the addition
of fill lines when missing
lines are suspected.
.LEVEL2
.VARIABLE INP
The input file should be a file from an SDDPT CD-ROM, or an exact copy of
such a file.  The file should be originally from the transcription of a
7 track, 1/2 inch CCT tape, and contain an IBM VICAR label.  Files from 9 
track tapes, and files that have had VICAR2 labels added to them will be 
handled improperly.
.VARIABLE OUT
The output file will be a file with a standard VICAR2 label.  The history
labels from the old IBM VICAR label will be preserved as comments in the
new label.
.VARIABLE NOADD
It was observed in the test datasets that there is sometimes a "loss of
sync" when reading the input dataset.  This appears to be associated with
read errors in the original transcription from the 7 track tape, and is
usually accompanied by a missing line, lines, or part of a line.
      In the default mode, whenever a loss of sync is detected, the previous
output line is repeated, and processing moves to the nearest sync mark.
Specifying the 'NOADD option suppresses the addition of these duplicate
lines.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create from7trk.imake
#define  PROGRAM   from7trk

#define MODULE_LIST from7trk.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
