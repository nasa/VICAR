$!****************************************************************************
$!
$! Build proc for MIPL module parse
$! VPACK Version 1.8, Thursday, May 25, 2000, 17:10:06
$!
$! Execute by entering:		$ @parse
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
$ write sys$output "*** module parse ***"
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
$ write sys$output "Invalid argument given to parse.com file -- ", primary
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
$   if F$SEARCH("parse.imake") .nes. ""
$   then
$      vimake parse
$      purge parse.bld
$   else
$      if F$SEARCH("parse.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake parse
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @parse.bld "STD"
$   else
$      @parse.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create parse.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack parse.com -
	-s parse.f -
	-p parse.pdf -
	-i parse.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create parse.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	This program splits an image into component fragments, as would
C	be appropriate for separating out individual sensors of a pushbroom
C	or whiskbroom system. In the REVERSE method of operation, the
C	image is re-assembled from its components.
C
C	Original release:  May 25, 2000     Ron Alley
C
	IMPLICIT NONE
C
	INTEGER BUF(40000),BUF2(20000),INP(50),IOUT(50),IBANDS(50)
	INTEGER ISENSORS(50),INST(200)
	INTEGER I,ICNT,IDEF,ISTAT,NINP,NOUT,NL,NS,NB,NBANDSOUT,NSENSORS
	INTEGER NSENSORSOUT,NBPP,NL_OUT,NS_OUT,NB_OUT,NHIST,NUM
	LOGICAL XVPTST,QVERT,QBY_SENSOR,QREVERSE
	CHARACTER*80 MSG
	CHARACTER*10 TASK(200)
C								   open input(s)
	CALL XVPCNT('INP',NINP)
	DO I=1,NINP
	    CALL XVUNIT(INP(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(INP(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	END DO
	CALL XVGET(INP(1),ISTAT,'NL',NL,'NS',NS,'NB',NB,
     +		   'PIX_SIZE',NBPP,' ')
C								  get parameters
	CALL XVPARM('COUNT',NSENSORS,ICNT,IDEF,1)
	QVERT = XVPTST('VERTICAL')
	QBY_SENSOR = XVPTST('BY_SENSOR')
	QREVERSE = XVPTST('REVERSE')
	CALL XVPARM('BANDS',IBANDS,NBANDSOUT,IDEF,50)
	IF (NBANDSOUT .EQ. 0) THEN
	    DO I=1,NB
		IBANDS(I) = I
	    END DO
	    NBANDSOUT = NB
	END IF
	CALL XVPARM('SENSORS',ISENSORS,NSENSORSOUT,IDEF,50)
	IF (NSENSORSOUT .EQ. 0) THEN
	    DO I=1,NSENSORS
		ISENSORS(I) = I
	    END DO
	    NSENSORSOUT = NSENSORS
	END IF
	CALL XVPCNT('OUT',NOUT)
C						check parameters for consistency
	IF (NBANDSOUT .GT. NB) THEN
	    CALL XVMESSAGE('Too many output bands specified',' ')
	    CALL ABEND
	ENDIF
	IF (NSENSORSOUT .GT. NSENSORS) THEN
	    CALL XVMESSAGE('Too many output sensors specified',' ')
	    CALL ABEND
	ENDIF
	IF (QREVERSE) THEN
	    IF (NOUT .GT. 1) THEN
		CALL XVMESSAGE(
     +		 'Only one output file is permitted in REVERSE method.',' ')
		CALL ABEND
	    ENDIF
	    IF (QBY_SENSOR) THEN
		IF (NINP .NE. NSENSORS) THEN
		    CALL XVMESSAGE(
     +		    'Number of inputs must equal the number of sensors',' ')
		    CALL ABEND
		ENDIF
	    ELSE
		IF (NB .NE. NSENSORS) THEN
		    CALL XVMESSAGE(
     +		'Number of input bands must equal number of sensors',' ')
		    CALL ABEND
		ENDIF
	    END IF
	ELSE
	    IF (NINP .GT. 1) THEN
		CALL XVMESSAGE(
     +		 'Only one input file is permitted in FORWARD method.',' ')
		CALL ABEND
	    ENDIF
	    IF (QBY_SENSOR) THEN
		IF (NOUT .NE. NSENSORSOUT) THEN
		    CALL XVMESSAGE(
     +		   'Number of outputs must equal the number of sensors',' ')
		    CALL ABEND
		ENDIF
	    ELSE
		IF (NOUT .NE. NBANDSOUT) THEN
		    CALL XVMESSAGE(
     +		     'Number of outputs must equal the number of bands',' ')
		    CALL ABEND
		ENDIF
	    END IF
	END IF
C								  open output(s)
	IF (QREVERSE) THEN
	    IF (QBY_SENSOR) THEN
		NB_OUT = NB
	    ELSE
		NB_OUT = NINP
	    END IF
	ELSE
	    IF (QBY_SENSOR) THEN
		NB_OUT = NBANDSOUT
	    ELSE
		NB_OUT = NSENSORSOUT
	    END IF
	END IF
C
	IF (QVERT) THEN
	    NL_OUT = NL
	    IF (QREVERSE) THEN
		NS_OUT = NS * NSENSORS
	    ELSE
		NS_OUT = NS / NSENSORS
	    END IF
	ELSE
	    NS_OUT = NS
	    IF (QREVERSE) THEN
		NL_OUT = NL * NSENSORS
	    ELSE
		NL_OUT = NL / NSENSORS
	    END IF
	END IF
	DO I=1,NOUT
	    CALL XVUNIT(IOUT(I),'OUT',I,ISTAT,' ')
	    CALL XVOPEN(IOUT(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'OP','WRITE','U_NL',NL_OUT,'U_NS',NS_OUT,
     +			'U_NB',NB_OUT,'U_ORG','BSQ',' ')
	END DO
C					    add/delete annotation in VICAR label
	IF (QREVERSE) THEN
	    NHIST = 200
	    CALL XLHINFO(IOUT(1),TASK,INST,NHIST,ISTAT,' ')
	    NUM = INST(NHIST)
	    DO I=1,NUM
		CALL XLDEL(IOUT(1),'HISTORY','PARSE_NOTE1',ISTAT,
     +			   'ERR_ACT','','INSTANCE',I,
     +			   'HIST','PARSE',' ')
		CALL XLDEL(IOUT(1),'HISTORY','PARSE_NOTE2',ISTAT,
     +			   'ERR_ACT','','INSTANCE',I,
     +			   'HIST','PARSE',' ')
	    END DO
	ELSE
	    IF (QBY_SENSOR) THEN
		DO I=1,NOUT
		    WRITE (MSG,530) ISENSORS(I)
  530		    FORMAT('Sensor',I4,' of a parsed image')
		    CALL XLADD(IOUT(I),'HISTORY','PARSE_NOTE1',MSG,
     +			       ISTAT,'FORMAT','STRING',' ')
		END DO
	    ELSE
		DO I=1,NOUT
		    WRITE (MSG,540) IBANDS(I)
  540		    FORMAT('Band',I4,' of a parsed image')
		    CALL XLADD(IOUT(I),'HISTORY','PARSE_NOTE1',MSG,
     +			       ISTAT,'FORMAT','STRING',' ')
		    MSG = 'Each band represents a different sensor'
		    CALL XLADD(IOUT(I),'HISTORY','PARSE_NOTE2',MSG,
     +			       ISTAT,'FORMAT','STRING',' ')
		END DO
	    END IF
	END IF
C						call appropriate parsing routine
	IF (QREVERSE) THEN
	    IF (QBY_SENSOR) THEN
		IF (QVERT) THEN
		    CALL REV_SEN_VERT(INP,IOUT,BUF,BUF2,NL,NS,NB,
     +				      NSENSORS,NBPP)
		ELSE
		    CALL REV_SEN_HOR(INP,IOUT,BUF,NL,NB,NSENSORS)
		END IF
	    ELSE
		IF (QVERT) THEN
		    CALL REV_BND_VERT(INP,IOUT,BUF,BUF2,NL,NS,NINP,
     +				      NSENSORS,NBPP)
		ELSE
		    CALL REV_BND_HOR(INP,IOUT,BUF,NL,NINP,NSENSORS)
		END IF
	    END IF
	ELSE
	    IF (QBY_SENSOR) THEN
		IF (QVERT) THEN
		    CALL FWD_SEN_VERT(INP,IOUT,BUF,BUF2,IBANDS,ISENSORS,
     +				    NBANDSOUT,NSENSORSOUT,NL_OUT,NS_OUT,
     +				    NSENSORS,NBPP)
		ELSE
		    CALL FWD_SEN_HOR(INP,IOUT,BUF,IBANDS,ISENSORS,
     +				  NBANDSOUT,NSENSORSOUT,NL_OUT,NSENSORS)
		END IF
	    ELSE
		IF (QVERT) THEN
		    CALL FWD_BND_VERT(INP,IOUT,BUF,BUF2,IBANDS,ISENSORS,
     +				    NBANDSOUT,NSENSORSOUT,NL_OUT,NS_OUT,
     +				    NSENSORS,NBPP)
		ELSE
		    CALL FWD_BND_HOR(INP,IOUT,BUF,IBANDS,ISENSORS,
     +				  NBANDSOUT,NSENSORSOUT,NL_OUT,NSENSORS)
		END IF
	    END IF
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FWD_BND_HOR(INP,IOUT,BUF,IBANDS,ISENSORS,NBANDSOUT,
     +			       NSENSORSOUT,NL_OUT,NSENSORS)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),BUF(*),IBANDS(*),ISENSORS(*),NBANDSOUT
	INTEGER NSENSORSOUT,NL_OUT,NSENSORS
	INTEGER IBAND,JBAND,ILINE,ISENSOR,JSENSOR,LINE_IN,ISTAT
C
	DO IBAND=1,NBANDSOUT
	    JBAND = IBANDS(IBAND)
	    DO ISENSOR=1,NSENSORSOUT
		JSENSOR = ISENSORS(ISENSOR)
		DO ILINE=1,NL_OUT
		    LINE_IN = NSENSORS*(ILINE-1) + JSENSOR
		    CALL XVREAD(INP(1),BUF,ISTAT,'BAND',JBAND,'LINE',
     +				LINE_IN,' ')
		    CALL XVWRIT(IOUT(IBAND),BUF,ISTAT,' ')
		END DO
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FWD_BND_VERT(INP,IOUT,BUF,BUF2,IBANDS,ISENSORS,
     +				NBANDSOUT,NSENSORSOUT,NL_OUT,NS_OUT,
     +				NSENSORS,NBPP)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),IBANDS(*),ISENSORS(*),NBANDSOUT
	INTEGER NSENSORSOUT,NL_OUT,NS_OUT,NSENSORS,NBPP
	INTEGER IBAND,JBAND,ILINE,ISENSOR,JSENSOR,ISTAT,LOC
	BYTE BUF(*),BUF2(*)
C
	DO IBAND=1,NBANDSOUT
	    JBAND = IBANDS(IBAND)
	    DO ISENSOR=1,NSENSORSOUT
		JSENSOR = ISENSORS(ISENSOR)
		LOC = NBPP*(JSENSOR-1) + 1
		DO ILINE=1,NL_OUT
		    CALL XVREAD(INP(1),BUF,ISTAT,'BAND',JBAND,
     +				'LINE',ILINE,' ')
		    CALL MVE(NBPP,NS_OUT,BUF(LOC),BUF2,NSENSORS,1)
		    CALL XVWRIT(IOUT(IBAND),BUF2,ISTAT,' ')
		END DO
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FWD_SEN_HOR(INP,IOUT,BUF,IBANDS,ISENSORS,NBANDSOUT,
     +			       NSENSORSOUT,NL_OUT,NSENSORS)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),BUF(*),IBANDS(*),ISENSORS(*),NBANDSOUT
	INTEGER NSENSORSOUT,NL_OUT,NSENSORS
	INTEGER IBAND,JBAND,ILINE,ISENSOR,JSENSOR,LINE_IN,ISTAT
C
	DO IBAND=1,NBANDSOUT
	    JBAND = IBANDS(IBAND)
	    DO ISENSOR=1,NSENSORSOUT
		JSENSOR = ISENSORS(ISENSOR)
		DO ILINE=1,NL_OUT
		    LINE_IN = NSENSORS*(ILINE-1) + JSENSOR
		    CALL XVREAD(INP(1),BUF,ISTAT,'BAND',JBAND,'LINE',
     +				LINE_IN,' ')
		    CALL XVWRIT(IOUT(ISENSOR),BUF,ISTAT,' ')
		END DO
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FWD_SEN_VERT(INP,IOUT,BUF,BUF2,IBANDS,ISENSORS,
     +				NBANDSOUT,NSENSORSOUT,NL_OUT,NS_OUT,
     +				NSENSORS,NBPP)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),IBANDS(*),ISENSORS(*),NBANDSOUT
	INTEGER NSENSORSOUT,NL_OUT,NS_OUT,NSENSORS,NBPP
	INTEGER LOC(50),IBAND,JBAND,ILINE,ISENSOR,JSENSOR,ISTAT
	BYTE BUF(*),BUF2(*)
C						 set up pointers for each sensor
	DO ISENSOR=1,NSENSORSOUT
	    JSENSOR = ISENSORS(ISENSOR)
	    LOC(ISENSOR) = NBPP*(JSENSOR-1) + 1
	END DO
C
	DO IBAND=1,NBANDSOUT
	    JBAND = IBANDS(IBAND)
	    DO ILINE=1,NL_OUT
		CALL XVREAD(INP(1),BUF,ISTAT,'BAND',JBAND,'LINE',ILINE,
     +			    ' ')
		DO ISENSOR=1,NSENSORSOUT
		    CALL MVE(NBPP,NS_OUT,BUF(LOC(ISENSOR)),BUF2,
     +			     NSENSORS,1)
		    CALL XVWRIT(IOUT(ISENSOR),BUF2,ISTAT,' ')
		END DO
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE REV_BND_HOR(INP,IOUT,BUF,NL,NINP,NSENSORS)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),BUF(*),NL,NINP,NSENSORS
	INTEGER INFILE,ILINE,ISENSOR,ISTAT
C
	DO INFILE=1,NINP
	    DO ILINE=1,NL
		DO ISENSOR=1,NSENSORS
		    CALL XVREAD(INP(INFILE),BUF,ISTAT,'BAND',ISENSOR,
     +				'LINE',ILINE,' ')
		    CALL XVWRIT(IOUT(1),BUF,ISTAT,' ')
		END DO
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE REV_BND_VERT(INP,IOUT,BUF,BUF2,NL,NS,NINP,NSENSORS,
     +			        NBPP)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),NL,NS,NINP,NSENSORS,NBPP
	INTEGER LOC(50),INFILE,ILINE,ISENSOR,ISTAT
	BYTE BUF(*),BUF2(*)
C						 set up pointers for each sensor
	DO ISENSOR=1,NSENSORS
	    LOC(ISENSOR) = NBPP*(ISENSOR-1) + 1
	END DO
C
	DO INFILE=1,NINP
	    DO ILINE=1,NL
		DO ISENSOR=1,NSENSORS
		    CALL XVREAD(INP(INFILE),BUF2,ISTAT,'BAND',ISENSOR,
     +				'LINE',ILINE,' ')
		    CALL MVE(NBPP,NS,BUF2,BUF(LOC(ISENSOR)),1,NSENSORS)
		END DO
		CALL XVWRIT(IOUT(1),BUF,ISTAT,' ')
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE REV_SEN_HOR(INP,IOUT,BUF,NL,NB,NSENSORS)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),BUF(*),NL,NB,NSENSORS
	INTEGER IBAND,ILINE,ISENSOR,ISTAT
C
	DO IBAND=1,NB
	    DO ILINE=1,NL
		DO ISENSOR=1,NSENSORS
		    CALL XVREAD(INP(ISENSOR),BUF,ISTAT,'BAND',IBAND,
     +				'LINE',ILINE,' ')
		    CALL XVWRIT(IOUT(1),BUF,ISTAT,' ')
		END DO
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE REV_SEN_VERT(INP,IOUT,BUF,BUF2,NL,NS,NB,NSENSORS,
     +			        NBPP)
C
	IMPLICIT NONE
C
	INTEGER INP(*),IOUT(*),NL,NS,NB,NSENSORS,NBPP
	INTEGER LOC(50),IBAND,ILINE,ISENSOR,ISTAT
	BYTE BUF(*),BUF2(*)
C						 set up pointers for each sensor
	DO ISENSOR=1,NSENSORS
	    LOC(ISENSOR) = NBPP*(ISENSOR-1) + 1
	END DO
C
	DO IBAND=1,NB
	    DO ILINE=1,NL
		DO ISENSOR=1,NSENSORS
		    CALL XVREAD(INP(ISENSOR),BUF2,ISTAT,'BAND',IBAND,
     +				'LINE',ILINE,' ')
		    CALL MVE(NBPP,NS,BUF2,BUF(LOC(ISENSOR)),1,NSENSORS)
		END DO
		CALL XVWRIT(IOUT(1),BUF,ISTAT,' ')
	    END DO
	END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create parse.pdf
process help=*
PARM INP       TYPE=(STRING,60)  COUNT=(1:50)
PARM OUT       TYPE=(STRING,60)  COUNT=(1:50)
PARM COUNT     TYPE=INTEGER
PARM MODE      TYPE=KEYWORD DEFAULT=HORIZONTAL VALID=(HORIZONTAL,VERTICAL)
PARM FILES_BY  TYPE=KEYWORD DEFAULT=BY_BAND VALID=(BY_BAND,BY_SENSOR)
PARM METHOD    TYPE=KEYWORD DEFAULT=FORWARD VALID=(FORWARD,REVERSE)
PARM BANDS     TYPE=INTEGER COUNT=0:50 DEFAULT=--
PARM SENSORS   TYPE=INTEGER COUNT=0:50 DEFAULT=--
END-PROC
.TITLE
VICAR Program PARSE
.HELP
PURPOSE:
PARSE separates images into their periodic components. That is, each output
is created from every nth line (HORIZONTAL mode) or every nth sample
(VERTICAL mode) from the input image, with each particular output starting
from a different line (or sample).  Its original intent was to separate one 
image into images of each component detector (sensor), but it may be used 
whenever an image of every nth line or nth sample is desired.

The user may choose to output only selected bands (for multichannel data) or
selected sensors (lines or samples). Each output image will contain the
selected sensors for one band in the BY_BAND mode, or the selected bands for
one sensor in the BY_SENSOR mode.

There is also a REVERSE mode, which performs the reverse operation.  That 
is, given multiple inputs from an image that has been PARSE'd apart, an
output image will be rebuilt.
.PAGE
Examples:

  PARSE ASTER_TIR (CH10,CH11,C12,C13,C14) COUNT=10

In this case, an input with 5 channels in the input file is split into 5
output images, one per channel.  Each output file will contain 10 bands,
each band is taken from every tenth input line, Band 1 starting with Line 1,
Band 2 starting with Line 2, etc.


  PARSE ASTER_SWIR (ODD,EVEN) 'VERTICAL 'BY_SENSOR COUNT=2

In this example, an input is split in two, with the first output containing
the odd numbered samples, and the second output holding the even numbered
samples.
.PAGE
  PARSE ASTER_TIR (S1,S7,S10) 'BY_SENSOR COUNT=10 SENSORS=(1,7,10)

Here, the input image is parsed by every tenth line, but only the first,
seventh, and tenth starting lines (sensors) are output.


  PARSE (CH10,CH11,CH12,C13,CH14) ASTER_TIR COUNT=10 'REVERSE

In this example, a single image is regenerated from its components.  That is,
if the outputs of the first example were used as inputs for this example, the
output would be identical to the input of the first example.
.PAGE
WRITTEN BY:  Ron Alley   25 May 2000
COGNIZANT PROGRAMMER:  Ron Alley
REVISION:  New  25 May 2000
.LEVEL1
.VARIABLE INP
Input image file(s)
.VARIABLE OUT
Output image file(s)
.VARIABLE COUNT
Number of sensors,
or, the modulus.
.VARIABLE MODE
Scan direction, or
parsing method
Valid: HORIZONTAL, VERTICAL
.VARIABLE FILES_BY
Output files will
be organized by:
Valid: BY_BAND, BY_SENSOR
.VARIABLE METHOD
Split files up (FORWARD) or put
them together (REVERSE)?
.VARIABLE BANDS
Include only the following list
of bands. (Default is to use 
all bands)
.VARIABLE SENSORS
Include only the following list
of sensors (seeds). (Default is
to use all sensors)
.LEVEL2
.VARIABLE INP
In the default method, one input is needed; the image to be split up.
In the REVERSE method, there needs to be one input file for each sensor
(if BY_SENSOR) or for each band (if BY_BAND).
.VARIABLE OUT
In the default method, there needs to be one output file for each sensor
(if BY_SENSOR) or for each band (if BY_BAND).  In the REVERSE method, 
only one output is needed; the full image to be built.
.VARIABLE COUNT
COUNT is the number of sensors, i.e., the modulus.  For example, if COUNT=6,
the input image is split into six parts, and each output file contains every
sixth line (HORIZONTAL MODE) or every sixth sample (VERTICAL MODE).
.VARIABLE MODE
In the HORIZONTAL (default) MODE, the input image is separated into its
component lines (rows).  In the VERTICAL MODE, the input image is separated
into its component samples (columns).  For the REVERSE method, this  is
used to indicate the contents of the input files
.VARIABLE FILES_BY
If the BY_BAND keyword is used, there will be one output file for each 
desired output band. Each output file will contain each of the desired
sensors as separate bands.

If the BY_SENSOR keyword is used, there will be one output file for each
desired sensor.
.VARIABLE METHOD
In the default (FORWARD) method, a single input is parsed into its components.
To rebuild the original image from those components, use the REVERSE keyword.
.VARIABLE BANDS
If this parameter is defaulted, all bands will be processed.  To process only
a subset of all bands, list the desired bands to process here.
.VARIABLE SENSORS
If this parameter is defaulted, all sensors will be processed.  To process only
a subset of all sensors, list the desired sensors to process here.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create parse.imake
#define  PROGRAM   parse

#define MODULE_LIST parse.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
