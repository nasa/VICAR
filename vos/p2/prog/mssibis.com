$!****************************************************************************
$!
$! Build proc for MIPL module mssibis
$! VPACK Version 1.8, Tuesday, February 28, 1995, 13:52:32
$!
$! Execute by entering:		$ @mssibis
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
$ write sys$output "*** module mssibis ***"
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
$ write sys$output "Invalid argument given to mssibis.com file -- ", primary
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
$   if F$SEARCH("mssibis.imake") .nes. ""
$   then
$      vimake mssibis
$      purge mssibis.bld
$   else
$      if F$SEARCH("mssibis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mssibis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mssibis.bld "STD"
$   else
$      @mssibis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mssibis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mssibis.com -
	-s mssibis.f -
	-i mssibis.imake -
	-p mssibis.pdf -
	-t tstmssibis.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mssibis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	MSSIBIS - TRANSFERS DATA BETWEEN MSS FORMAT IMAGES AND 
C					IBIS TABULAR FILES
C
C       REVISION:  D            CRI             MARCH   1995
C               MSTP S/W CONVERSION (VICAR PORTING)
C
C	REVISION:  C		NILES RITTER    JANUARY 1990
C		FORMAT STRING PARAMETER CHANGED TO KEYWORD
C
C	REVISION:  B		NILES RITTER    OCTOBER 1988
C
C	ORIGINAL PROGRAMMER:   FRANK EVANS	OCTOBER 1986
C
C	COGNIZANT PROGRAMMER:  NDR
C
C
	IMPLICIT  NONE
	INTEGER	  MAXSAMPS
	PARAMETER (MAXSAMPS=32768)
	INTEGER	  INUNIT, IN2UNIT, OUTUNIT, IBIS, STATUS, COUNT, DEF
	INTEGER	  SL,SS,NL,NS, NLI,NSI, NSO, INC, RECORD
	INTEGER	  NSB, MSS, NBAND, NDIM
	INTEGER	  LINE,SAMP, N, I
	INTEGER	  CLEN, NCOL, DCOLS, ROW
	INTEGER   BANDS(40), COLS(40), BANDPTR(40)
	BYTE	  MASKBUF(MAXSAMPS)
	REAL	  BUFFER(MAXSAMPS)
	REAL	  ROWBUF(40), VLO, VHI
	LOGICAL	  XVPTST, CNVT, MASK, UPDATE
	CHARACTER*16  FORMAT
	CHARACTER*80  OUTNAME,INNAME(2)

	EXTERNAL RANGE

        CALL IFMESSAGE('MSSIBIS version 6-MAR-95')
	IF (XVPTST('TOIBIS')) THEN

C  ***************   MSS TO IBIS SECTION *******************


C--			GET THE PARAMETERS
	CALL XVP ('MSS', MSS, COUNT)
	CALL XVPARM ('BANDS', BANDS, NDIM, DEF, 40)
	IF (DEF .EQ. 1) THEN
	    NDIM = MSS
	    DO I = 1, NDIM
		BANDS(I) = I
	    ENDDO
	ENDIF

	CALL XVPARM ('COLS', COLS, DCOLS, DEF, 40)
	IF (DCOLS .LT. NDIM) THEN
	    DCOLS = NDIM
	    DO I = 1, DCOLS
		COLS(I) = I
	    ENDDO
	ENDIF
	CALL XVPARM ('NCOL', NCOL, COUNT, DEF, 1)
	IF (DEF .EQ. 1) THEN
	    NCOL = DCOLS
	    DO I = 1, DCOLS
		NCOL = MAX(COLS(I), NCOL)
	    ENDDO
	ENDIF
	NCOL = MAX( NCOL, NDIM)

	CALL XVP ('INC', INC, COUNT)


C--			OPEN THE MSS FORMAT IMAGE FILE
	CALL XVUNIT (INUNIT, 'INP', 1, STATUS,' ')
	CALL XVOPEN (INUNIT, STATUS, 'IO_ACT','SA','OPEN_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVSIZE (SL,SS, NL,NS, NLI,NSI)
	IF (NSI .GT. MAXSAMPS) CALL MABEND ('TOO MANY SAMPLES PER LINE')
	NSB = NSI/MSS
	NS = MIN (NS, NSB-SS+1)
	NL = MIN (NL, NLI-SL+1)

	CALL XVP ('INP', INNAME, COUNT)
	MASK =  (COUNT .EQ. 2)

	IF (MASK) THEN
	    CALL XVUNIT (IN2UNIT, 'INP', 2, STATUS,' ')
	    CALL XVOPEN (IN2UNIT, STATUS, 'IO_ACT','SA','OPEN_ACT','SA',
     +			      'U_FORMAT','BYTE',' ')
	    CLEN = 0
	    DO LINE = 1, NL, INC
		CALL XVREAD (IN2UNIT, MASKBUF, STATUS, 
     +			'SAMP',SS, 'LINE',LINE+SL-1,' ')
		DO SAMP = 1, NS, INC
		    IF (MASKBUF(SAMP) .NE. 0)  CLEN = CLEN + 1
		ENDDO
	    ENDDO
	ELSE

	    CLEN = INT((NS-1)/INC+1) * INT((NL-1)/INC+1)
	ENDIF

C			OPEN THE IBIS TABULAR FILE
        CALL XVUNIT(OUTUNIT, 'OUT', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(OUTUNIT,IBIS,'WRITE',NCOL,CLEN,
     *                      ' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)


        CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',
     *                        COLS,DCOLS,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

	DO N = 1, NDIM
	    BANDPTR(N) = (BANDS(N)-1)*NSB
	ENDDO

C			TRANSFER THE DATA TO THE IBIS FILE
	ROW = 1
	DO LINE = 1, NL, INC
	    CALL XVREAD (INUNIT, BUFFER, STATUS, 
     +			'SAMP',SS, 'LINE',LINE+SL-1,' ')
	    IF (MASK) THEN
		CALL XVREAD (IN2UNIT, MASKBUF, STATUS, 
     +			'SAMP',SS, 'LINE',LINE+SL-1,' ')
		DO SAMP = 1, NS, INC
		    IF (MASKBUF(SAMP) .NE. 0) THEN
			DO N = 1, NDIM
			    ROWBUF(N) = BUFFER(SAMP+BANDPTR(N))
			ENDDO
	                CALL IBIS_RECORD_WRITE(RECORD,ROWBUF,ROW,STATUS)
                        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
			ROW = ROW + 1
		    ENDIF
		ENDDO
	    ELSE
		DO SAMP = 1, NS, INC
		    DO N = 1, NDIM
			ROWBUF(N) = BUFFER(SAMP+BANDPTR(N))
		    ENDDO
                    CALL IBIS_RECORD_WRITE(RECORD,ROWBUF,ROW,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
		    ROW = ROW + 1
		ENDDO
	    ENDIF
	ENDDO

        CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	CALL XVCLOSE (INUNIT,STATUS,' ')
	IF (MASK)  CALL XVCLOSE (IN2UNIT,STATUS,' ')
	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)


	ELSE
C  ***************   IBIS TO MSS SECTION *******************




C--			OPEN THE IBIS TABULAR FILE
        CALL XVUNIT(INUNIT, 'INP', 1, STATUS, ' ')
	CALL IBIS_FILE_OPEN(INUNIT,IBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)


C--			GET THE PARAMETERS
	CALL XVPARM ('COLS', COLS, DCOLS, DEF, 40)
	IF (DEF .EQ. 1) THEN
	    DCOLS = NCOL
	    DO I = 1, DCOLS
		COLS(I) = I
	    ENDDO
	ENDIF
	NDIM = DCOLS

	CALL XVPARM ('MSS', MSS, COUNT, DEF, 1)
	IF (DEF .EQ. 1)  MSS = NDIM
	MSS = MAX (MSS, NDIM)
	CALL XVPARM ('BANDS', BANDS, NBAND, DEF, 40)
	IF (NBAND .LT. NDIM) THEN
	    DO I = 1, NDIM
		BANDS(I) = I
	    ENDDO
	ELSE
	    DO I = 1, NDIM
		MSS = MAX(BANDS(I), MSS)
	    ENDDO
	ENDIF

	UPDATE = XVPTST('UPDATE')
	IF( UPDATE ) THEN
	 CALL XVPARM('OUT',OUTNAME,COUNT,DEF,1)
	 CALL XVUNIT(OUTUNIT,'   ',1,STATUS,'U_NAME',OUTNAME,' ')
	 CALL XVOPEN(OUTUNIT,STATUS,'OP','UPDATE','IO_ACT','SA',
     +     'OPEN_ACT','SA', 'U_FORMAT','REAL',' ')
	 CALL XVPARM ('NS', NS, COUNT, DEF, 1)
	 IF (DEF .EQ. 1)  CALL XVGET(OUTUNIT,STATUS,'NS',NS,' ') 
	 CALL XVPARM ('NL', NL, COUNT, DEF, 1)
	 IF (DEF .EQ. 1)  CALL XVGET(OUTUNIT,STATUS,'NL',NL,' ') 
	 CALL XVGET (OUTUNIT,STATUS,'FORMAT', FORMAT,' ')

	 NL = MIN (NL, INT(CLEN/NS))
	 NSB = NS
	 NSO = MSS*NS

	ELSE  !New Output file
	 CALL XVSIZE (SL,SS, NL,NS, NLI,NSI)
	 IF (NS .EQ. 0) THEN
	   CALL XVPARM ('NS', NS, COUNT, DEF, 1)
	   IF (DEF .EQ. 1)  NS = INT(SQRT(FLOAT(CLEN)))
	 ENDIF
	 IF (NL .EQ. 0) THEN
	   CALL XVPARM ('NL', NL, COUNT, DEF, 1)
	   IF (DEF .EQ. 1)  NL = INT(CLEN/NS)
	 ENDIF
	 CALL XVP ('FORMAT', FORMAT, COUNT)

	 NL = MIN (NL, INT(CLEN/NS))
	 NSB = NS
	 NSO = MSS*NS

C--			OPEN THE MSS FORMAT IMAGE FILE
	 CALL XVUNIT (OUTUNIT, 'OUT', 1, STATUS,' ')
	 CALL XVOPEN (OUTUNIT, STATUS, 'IO_ACT','SA','OPEN_ACT','SA',
     +			'U_FORMAT','REAL', 'OP','WRITE', 
     +			'U_NL',NL, 'U_NS',NSO, 'O_FORMAT',FORMAT,' ')

	ENDIF  

	IF (FORMAT(1:4) .EQ. 'BYTE') THEN
	    VLO = 0.0
	    VHI = 255.0
	ELSEIF (FORMAT(1:4) .EQ. 'HALF') THEN
	    VLO = -32768.0
	    VHI = 32767.0
	ELSEIF (FORMAT(1:4) .EQ. 'FULL') THEN
	    VLO = -2.14748E9
	    VHI = 2.14748E9
	ENDIF
	IF (FORMAT(1:4) .EQ. 'REAL') THEN
	    CNVT = .FALSE.
	ELSE
	    CNVT = .TRUE.
	ENDIF




C--			READ IN ALL OF THE DATA POINTS
	DO N = 1, NDIM
	    BANDPTR(N) = (BANDS(N)-1)*NSB
	ENDDO
        CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',
     &                        COLS,DCOLS,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)

	ROW = 1
	DO LINE = 1, NL
	    IF (UPDATE) CALL XVREAD(OUTUNIT, BUFFER,STATUS,
     &                              'LINE',LINE,' ')
	    DO SAMP = 1, NS
	        CALL IBIS_RECORD_READ(RECORD, ROWBUF, ROW, STATUS)
                IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(IBIS,STATUS,1)
		DO N = 1, NDIM
		    BUFFER(SAMP+BANDPTR(N)) = ROWBUF(N) 
		ENDDO
		ROW = ROW + 1
	    ENDDO
	    IF (CNVT)  CALL RANGE (BUFFER, NSO, VLO, VHI)
	    CALL XVWRIT (OUTUNIT, BUFFER, STATUS,' ')
	ENDDO

    	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
	CALL XVCLOSE (OUTUNIT,STATUS,' ')


	ENDIF


	RETURN
	END




	SUBROUTINE RANGE (RBUF, NS, VLO, VHI)
	INTEGER NS, SAMP
	REAL	RBUF(1), VLO, VHI

	DO SAMP = 1, NS
	    V = RBUF(SAMP)
	    IF (V .LT. VLO) V = VLO
	    IF (V .GT. VHI) V = VHI
	    RBUF(SAMP) = V + 0.5
	ENDDO

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mssibis.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mssibis

   To Create the build file give the command:

		$ vimake mssibis			(VMS)
   or
		% vimake mssibis			(Unix)


************************************************************************/


#define PROGRAM	mssibis
#define R2LIB

#define MODULE_LIST mssibis.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create mssibis.pdf
PROCESS HELP=*
PARM INP      TYPE=STRING  COUNT=1:2
PARM OUT      TYPE=STRING
PARM SIZE     TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER DEFAULT=0
PARM NS       TYPE=INTEGER DEFAULT=0
PARM MODE     TYPE=KEYWORD VALID=(TOIBIS,TOMSS) DEFAULT=TOIBIS
PARM MSS      TYPE=INTEGER DEFAULT=1
PARM BANDS    TYPE=INTEGER COUNT=1:40  DEFAULT=1
PARM COLS     TYPE=INTEGER COUNT=1:40  DEFAULT=1
PARM NCOL     TYPE=INTEGER DEFAULT=1
PARM INC      TYPE=INTEGER DEFAULT=1
PARM FORMAT   TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL) DEFAULT=BYTE
PARM UPDATE   TYPE=KEYWORD VALID=(UPDATE,NOUPDATE) DEFAULT=NOUPDATE
END-PROC
.TITLE
VICAR/IBIS Program "mssibis"
.HELP
PURPOSE

    "mssibis" transfers data between MSS format images and IBIS tabular file.
    Data may be transfered to IBIS files from Vicar images using a mask image.



EXECUTION


mssibis  INPUT.MSS  OUTPUT.INT   SIZE=(100,50,160,200) INC=4  +
		MSS=4 BANDS=(1,3,4)  COLS=(1,3,5) NCOL=7 'TOIBIS

mssibis  INPUT.MSS  OUTPUT.INT   MSS=4

mssibis  INPUT.INT  OUTPUT.MSS 'TOMSS  COLS=(2,4,6,7) NL=50 NS=60

mssibis  (INPUT.MSS,MASK.IMG)  OUTPUT.INT  MSS=3




Original Programmer:	Frank Evans	October 1986

Cognizant Programmer:   Niles Ritter    

Revision:               A Scop (CRI)    March 1995     Made portable for UNIX

.LEVEL1
.VARIABLE INP
1.  Input dataset (MSS format 
  image or IBIS tabular file)
2.  Optional mask image
  for 'TOIBIS mode.
.VARIABLE OUT
Output dataset (IBIS tabular
file or MSS format image)
.VARIABLE SIZE
Standard VICAR size field.
Refers to one band in MSS
format image.
.VARIABLE SL
Starting line
Not used in 'TOMSS.
.VARIABLE SS
Starting sample
Not used in 'TOMSS.
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples in one band
.VARIABLE MODE     
Keyword for direction
('TOIBIS or 'TOMSS)
.VARIABLE MSS
Number of bands in MSS
format dataset.
.VARIABLE BANDS
Which bands to use in
MSS format image.
.VARIABLE COLS
Columns for data in
IBIS file.
.VARIABLE NCOL     
Number of columns in 
output IBIS file.
.VARIABLE INC      
The increment (or subsampling)
factor for 'TOIBIS mode.
.VARIABLE FORMAT   
The pixel format for the 
output MSS format image.
.VARIABLE UPDATE
Indicates the OUT file is an
old file to 'UPDATE.

.LEVEL2
.VARIABLE INP
    In 'TOIBIS mode the first input file is a Vicar MSS format image.  
The MSS format is a way of storing multiple bands of images in one Vicar 
image.  The multiple images are appended in the sample direction using
the Vicar program MSS.  A normal Vicar image can also be transfered as a
single banded MSS image.    
    The second input file is an optional mask image.  Only those pixels 
that are non zero in the mask image will be transfered.  The mask image 
should be the same size as one band in the MSS image.

    In 'TOMSS mode the input file is an IBIS tabular (interface) file.

.VARIABLE OUT
    In 'TOIBIS mode the output file is an IBIS tabular file.  The pixels
are put into the IBIS file in the order in which they are encountered,
i.e. from top to bottom and left to right.  Pixel values from the
specified bands are converted to real format and put in the specified
column of the tabular file.
 
    In 'TOMSS mode the output file in a MSS format Vicar image.  The
pixels are put into the MSS format image in the line scanning direction.
The pixels are rounded and clipped if necessary, depending on output pixel
format.  Each column corresponds to a separate MSS band.  The image size
(if not set by SIZE, NL or NS) will default to the largest square image
that may be completely filled by the column data.

.VARIABLE SIZE
The standard VICAR size field.   
Refers to one band in MSS format image.  Thus a subwindow from each band
will be taken out in 'TOIBIS mode. 
In 'TOMSS mode only the NL and NS part are used.
.VARIABLE SL
Starting line.  Not used in 'TOMSS.
.VARIABLE SS
Starting sample in one MSS band.  Not used in 'TOMSS.
.VARIABLE NL
Number of lines.
.VARIABLE NS
Number of samples in one MSS band.
.VARIABLE MODE     
Keyword for transfer direction.  The default 'TOIBIS mode transfers data
from MSS format Vicar images to IBIS tabular files, while 'TOMSS mode
transfers data from tabular files to MSS format images.  The transfer
is always between columns and corresponding MSS bands.
.VARIABLE MSS
Number of bands in MSS format dataset.  Must include total number of
bands that make up the MSS format image, not only those that will be used.
.VARIABLE BANDS
Which bands to use in MSS format image.  Default is to use (1,2,...).
.VARIABLE COLS
Columns for data in IBIS file.  Default is to use (1,2,...). Each column
corresponds to a separate band in the MSS file.
.VARIABLE NCOL     
Number of columns in output IBIS file.  Only used in 'TOIBIS mode.
Only necessary if more columns are desired in the tabular file.
.VARIABLE INC      
The increment (or subsampling) factor for 'TOIBIS mode.  This is used
to decrease the amount of data transfered to the tabular file when only
a statistical sampling is desired.
.VARIABLE FORMAT   
The pixel format for the output MSS format image.  May be byte, half, full,
or real.  Only used for 'TOMSS mode.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmssibis.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
   gen A LINC=1 SINC=1 NS=8 NL=15
   gen B LINC=3 SINC=2 NS=8 NL=15
   mss (A,B) C
   list  C
   mssibis C T    MSS=2  COLS=(1,3) NCOL=4
   ibis-list T
   mssibis T D    MSS=2  COLS=(1,3) BANDS=(1,2) 'TOMSS NL=15 NS=8
   list D
   difpic (C,D)
   f2 A F FUNC=(LINE.EQ.SAMP)
   mssibis (C,F) T2 MSS=2  COLS=(1,3) NCOL=4
   ibis-list T2
end-proc
$ Return
$!#############################################################################
