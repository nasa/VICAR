$!****************************************************************************
$!
$! Build proc for MIPL module mssview
$! VPACK Version 1.7, Friday, July 29, 1994, 10:34:10
$!
$! Execute by entering:		$ @mssview
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
$ write sys$output "*** module mssview ***"
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
$ write sys$output "Invalid argument given to mssview.com file -- ", primary
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
$   if F$SEARCH("mssview.imake") .nes. ""
$   then
$      vimake mssview
$      purge mssview.bld
$   else
$      if F$SEARCH("mssview.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mssview
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mssview.bld "STD"
$   else
$      @mssview.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mssview.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mssview.com -
	-s mssview.f -
	-i mssview.imake -
	-p mssview.pdf -
	-t tstmssview.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mssview.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C	MSSVIEW - MAKES SCATTER PLOTS OF MULTIDIMENSIONAL DATA IN MSS FORMAT
C
C
C	REVISION:  A		NEW
C
C	ORIGINAL PROGRAMMER:   FRANK EVANS	DECEMBER 1985
C
C	COGNIZANT PROGRAMMER:  KFE
C
C    5-SEP-94  ...AMS...  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C   14-APR-87  ...LWK...  MODS FOR PLANETARY IMAGES (AS IN CLUSAN)
C
C
      IMPLICIT    NONE
      INCLUDE     'fortport'
      INTEGER	  MAXNPNTS,MAXRECL
      PARAMETER   (MAXRECL = 60000)
      PARAMETER   (MAXNPNTS = 50000)
      INTEGER     BANDS(3)
      INTEGER	  MSS,CNT, NDIM, SL,SS,NL,NLI,NSI, NS, NPNTS, NSB, INCR
      INTEGER	  LINE,SAMP, N, SLW,SSW,NLW,NSW, NLOUT, NSOUT, RECL
      INTEGER     LOC, INTVAL, UNIT, STATUS, DEF, I, K
      INTEGER	  LINC, SINC, THRESHDEF
      REAL	  X(3,MAXNPNTS)
      REAL	  RBUF(MAXRECL)
      BYTE	  BUFFER(1000*1000), BYTEVAL
      REAL	  SLOPE1,OFFS1, SLOPE2,OFFS2, SLOPE3,OFFS3
      REAL	  VALUE, THRESH
      REAL	  MINX(3), MAXX(3)
      INTEGER	  NCHAR, CHARSIZE, LENGTH, POS
      BYTE        DOTMATRIX(6*4*80,7*4,3) 
      CHARACTER*80 STRING
      CHARACTER*32 LABEL(3)
      CHARACTER*60 OUTSTR


      CALL IFMESSAGE('MSSVIEW version 5-SEP-94')
      CALL XVEACTION('SA',' ')
C--			GET THE MSS FORMAT INFO
      CALL XVP('MSS',MSS,CNT)
      CALL XVPARM('BANDS',BANDS,NDIM,DEF,3)
      IF (DEF .EQ. 1) THEN
	 NDIM = MSS
	 DO I=1,NDIM
	    BANDS(I) = I
	 ENDDO
      ENDIF

C--			OPEN THE MSS FORMAT IMAGE FILE
      CALL XVUNIT(UNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(UNIT,STATUS,'U_FORMAT','REAL',' ')

      CALL XVGET(UNIT,STATUS,'NS',RECL,' ')
      IF (RECL.GT.MAXRECL) CALL MABEND('RECORD LENGTH TOO LARGE')
      NSB = RECL/MSS

      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      IF (NS.EQ.RECL) NS = NS/MSS


C   LINC/SINC, THRESH PARAMS
      CALL XVPARM( 'INCR', INCR, CNT, DEF,1)
      IF (CNT.EQ.0) THEN
	 CALL XVPARM( 'LINC', LINC, CNT, DEF,1)
	 CALL XVPARM( 'SINC', SINC, CNT, DEF,1)
      ELSE
	 LINC = INCR
	 SINC = INCR
      ENDIF
      CALL XVPARM( 'THRESH', THRESH, CNT, THRESHDEF,1)

C--			READ IN ALL OF THE DATA POINTS
      NPNTS = 0
      DO LINE=1,NL,LINC
         CALL XVREAD(UNIT,RBUF,STATUS,'SAMP',SS,'LINE',LINE+SL-1,' ')
	 DO SAMP = 1,NS,SINC
	    IF (THRESHDEF .EQ. 0) THEN
	       DO N = 1,NDIM
		  IF (RBUF(SAMP+(BANDS(N)-1)*NSB).LT.THRESH)GO TO 100
	       ENDDO
	    ENDIF
	    NPNTS = NPNTS + 1
	    IF (NPNTS .GT. MAXNPNTS) THEN
	       CALL MABEND ('Maximum number of data points exceeded')
	    ENDIF
	    DO N = 1,NDIM
	       X(N,NPNTS) = RBUF(SAMP+(BANDS(N)-1)*NSB)
	    ENDDO
100	    CONTINUE
	 ENDDO
      ENDDO
      WRITE (OUTSTR, '(1X,A,I7)') 'NUMBER OF POINTS USED:', NPNTS
      CALL XVMESSAGE (OUTSTR,' ')
      CALL XVCLOSE(UNIT,STATUS,' ')


C--			GO THROUGH ALL THE DATA POINTS AND CALCULATE 
C-				THE MIN AND MAX FOR EACH DIMENSION
      DO N = 1,NDIM
	 MINX(N) = +1E20
	 MAXX(N) = -1E20
	 DO I = 1,NPNTS
	    MINX(N) = MIN(X(N,I),MINX(N))
	    MAXX(N) = MAX(X(N,I),MAXX(N))
	 ENDDO	
      ENDDO


C--			GET THE SIZE OF THE IMAGE AND CALCULATE THE
C-				BOX SIZE AND THE CHARACTER SIZE
      CALL XVP('NLOUT',NLOUT,CNT)
      CALL XVP('NSOUT',NSOUT,CNT)
      CHARSIZE =  INT( MIN(NLOUT,NSOUT) / 250)
      IF (CHARSIZE .EQ. 0)  CHARSIZE = 1
      NLW = NLOUT - 6* 7*CHARSIZE
      NSW = NSOUT - 6* 7*CHARSIZE
      SLW = 3* 7*CHARSIZE
      SSW = 3* 7*CHARSIZE

C--			COMPUTE THE SLOPES AND OFFSETS FOR FASTER OPERATION
      SLOPE1 =  NSW / (MAXX(1) - MINX(1))
      OFFS1 =  -SLOPE1 * MINX(1) + SSW
      SLOPE2 = -NLW / (MAXX(2) - MINX(2))
      OFFS2 =   -SLOPE2 * MINX(2) + NLW+SLW-1 - 1
      IF (NDIM .EQ. 3) THEN 
	 SLOPE3 =  150 / (MAXX(3) - MINX(3))
	 OFFS3 =  -SLOPE3 * MINX(3) + 100
      ENDIF


C--			PUT EACH DATA POINT INTO THE IMAGE
      DO I = 1,NPNTS
	 SAMP = SLOPE1*X(1,I) + OFFS1
	 LINE = SLOPE2*X(2,I) + OFFS2
	 LOC = NSOUT*LINE + SAMP
	 IF (NDIM .EQ. 3) THEN
	    VALUE = SLOPE3*X(3,I) + OFFS3
	    INTVAL = MAX(MIN( VALUE, 255.0), 0.0)
	 ELSE
	    INTVAL = 255
	 ENDIF
         BYTEVAL=INT2BYTE(INTVAL)
	 BUFFER(LOC) = BYTEVAL
      ENDDO


C--			DRAW THE BOX AROUND THE SCATTERGRAM
      INTVAL = 255
      BYTEVAL=INT2BYTE(INTVAL)
      DO SAMP = SSW-2, NSW+SSW
	 BUFFER(NSOUT*(SLW-3)+SAMP) = BYTEVAL
	 BUFFER(NSOUT*(NLW+SLW-1)+SAMP) = BYTEVAL
      ENDDO
      DO LINE = SLW-2, NLW+SLW
	 BUFFER(NSOUT*(LINE-1)+SSW-2) = BYTEVAL
	 BUFFER(NSOUT*(LINE-1)+NSW+SSW) = BYTEVAL
      ENDDO


      CALL XVP('LABELS',LABEL,CNT)
      NCHAR = MIN( NSW, NLW)/ (6*CHARSIZE)	! NUMBER OF CHAR ON LABEL

C--			FOR EACH OF THE 2 OR 3 LABELS  MAKE THE STRING
C-				AND TRANSLATE IT INTO DOT MATRIX FORM
      DO N = 1, NDIM
         STRING=' '
C-			CENTER THE LABEL IN THE STRING
	 LENGTH = LEN(LABEL(N))
	 DO WHILE (LABEL(N)(LENGTH:LENGTH) .EQ. ' ')
	    LENGTH = LENGTH - 1
	 ENDDO
	 POS = MAX( (NCHAR-LENGTH)/2, 9+1)
	 DO K = 1, LENGTH
	    STRING(POS+K:POS+K) = LABEL(N)(K:K)
	 ENDDO
C-			PUT THE MIN AND MAX INTO THE STRING
	 WRITE (STRING(1:9),'(F9.3)')MINX(N)
	 WRITE (STRING(NCHAR-8:NCHAR),'(F9.3)')MAXX(N)
C--			GET SEVEN ROWS OF THE DOT MATRIX FOR THE STRING
	 DO I = 0, 6
	    CALL TEXT (STRING, NCHAR, I, DOTMATRIX(1,I*CHARSIZE+1,N),
     +		       6*CHARSIZE,0)
C--			IF MULTIPLE HEIGHT THEN COPY THE ROW
	    DO K = 2, CHARSIZE
	       CALL MVE(1,6*CHARSIZE*NCHAR,DOTMATRIX(1,I*CHARSIZE+1,N),
     +			 DOTMATRIX(1,I*CHARSIZE+K,N),1,1)
	    ENDDO
	 ENDDO
      ENDDO
C--			PUT THE DOT MATRICES OF THE LABELS IN THE IMAGE
      DO I = 1,7*CHARSIZE
	 LOC = NSOUT*(NLOUT-2*7*CHARSIZE + I)
	 DO K = 1, NCHAR* 6*CHARSIZE
	    BUFFER(LOC+SSW + K) = DOTMATRIX(K,I,1)
	 ENDDO
      ENDDO
      DO K = 1,NCHAR* 6*CHARSIZE
	 LOC = NSOUT*(NLW+SLW - K)
	 DO I = 1,7*CHARSIZE
	    BUFFER(LOC+7*CHARSIZE + I) = DOTMATRIX(K,I,2)
	 ENDDO
      ENDDO
      IF (NDIM .EQ. 3) THEN
	 DO I = 1,7*CHARSIZE
	    LOC = NSOUT*(7*CHARSIZE + I)
	    DO K = 1, NCHAR* 6*CHARSIZE
	       BUFFER(LOC+SSW + K) = DOTMATRIX(K,I,3)
	    ENDDO
	 ENDDO
      ENDIF



C--			SAVE THE IMAGE
      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(UNIT,STATUS,'U_NL',NLOUT,'U_NS',NSOUT,'OP','WRITE',
     +	          'U_FORMAT','BYTE','O_FORMAT','BYTE',' ')

      DO LINE = 1,NLOUT
	 CALL XVWRIT(UNIT,BUFFER(NSOUT*(LINE-1)+1),STATUS,' ')
      ENDDO
      CALL XVCLOSE(UNIT,STATUS,' ')


      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mssview.imake
#define PROGRAM mssview


#define R2LIB

#define MODULE_LIST mssview.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$ Return
$!#############################################################################
$PDF_File:
$ create mssview.pdf
PROCESS HELP=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM MSS TYPE=INTEGER VALID=2:1000
PARM BANDS TYPE=INTEGER COUNT=2:3 VALID=1:1000
PARM INCR INTEGER COUNT=0:1 DEFAULT=--
PARM LINC INTEGER DEFAULT=1
PARM SINC INTEGER DEFAULT=1
PARM THRESH REAL DEFAULT=0.
PARM NLOUT TYPE=INTEGER DEFAULT=256 VALID=150:1000
PARM NSOUT TYPE=INTEGER DEFAULT=256 VALID=150:1000
PARM LABELS TYPE=(STRING,16) COUNT=2:3 +
            DEFAULT=("FIRST BAND", "SECOND BAND", "THIRD BAND")		
END-PROC
.TITLE
VICAR Program "mssview"
.HELP
PURPOSE:

"mssview" makes an image that contains a scatterplot graph of two or three
	bands in an MSS format dataset.  The graph contains the scatterplot
	in a box in the middle of the image.  Around the sides of the image
	are labels with the minimum and maximum values and user defined
	labels.  The scatterplot can plot three dimensions by varying the
	intensity of the point by the value of the third dimension.
	Each dimension plotted is automatically scaled to fit.

.PAGE
EXECUTION EXAMPLES:

mssview  INPUT.MSS  OUTPUT.IMG  MSS=6 BANDS=(3,4) NLOUT=300 NSOUT=400 +
				LABELS=("RED BAND","IR BAND")

    In this example the input MSS format data set is INPUT.MSS and it is made
up of 6 bands.  The bands that will be plotted are the third and the fourth
in the dataset.  The number of lines in the output image will be 300 and the
number of samples will be 400.
    The output image is in byte format and the box and the labels have DN
value 255.  The data points are read in and the minimum and maximum are 
determined for each band.  The plot is scaled so that all of the points will
just fit in.  The characters are generated with a 7 by 5 dot matrix.  Larger 
size images will have multiple height characters.

.PAGE
mssview  INPUT.MSS  OUTPUT.IMG  MSS=5 BANDS=(1,2,5) 

    In this example, bands 1, 2, and 5 in INPUT.MSS will be plotted with
band 1 in the x direction, band 2 in the y direction, and band 5 plotted
as the intensity of the points.  The default output image size of 256 by
256 will used.  The intensity of the plotted points will range from a DN
value of 50 to a value of 250.


.PAGE
RESTRICTIONS:

The minimum output image size is 150 by 150 , and the maximum is 1000 by 1000.
The maximum number of data points that can be plotted is 50,000.
The maximum label length is 32 characters.
The maximum line length on the MSS format input dataset is 60,000 samples.
.page
HISTORY:

Original Programmer:	Frank Evans	November 1985

Cognizant Programmer:	Frank Evans

Revisions:

   5-SEP-94,  AMS:  (CRI) Made portable for UNIX
  14-APR-87,  LWK:  added INCR, LINC, SINC, THRESH parameters

.LEVEL1
.VARIABLE INP
Input MSS format data
.VARIABLE OUT
Output scattergram image
.VARIABLE SIZE
VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample (per band)
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples (per band)
.VARIABLE MSS
Number of bands in MSS
format input dataset
.VARIABLE BANDS
Which bands to use in 
scatter plot ( 2 or 3 numbers)
.VARIABLE NLOUT
Number of lines in output
.VARIABLE NSOUT
Number of samples in output
.VARIABLE LABELS
The labels associated with each band
.VARIABLE INCR
LINC and SINC
.VARIABLE LINC
Line increment
.VARIABLE SINC
Sample increment
.VARIABLE THRESH
DN threshold
.LEVEL2
.VARIABLE INP
Vicar image arranged in MSS format which consists of all of the 
image bands appended in the sample dimensional.  See program MSS.
.VARIABLE OUT
Output scattergram image
.VARIABLE SIZE
Standard VICAR size field, except that SS and NS refer to each band
separately, not to the entire MSS file.
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample, in each band separately.
.VARIABLE NL
Number of lines.
.VARIABLE NS
Number of samples, in each band separately.
.VARIABLE MSS
Number of bands in MSS format input dataset. 
Used to calculate where each band is.
.VARIABLE BANDS
Which bands to use in scatter plot. 
Can plot either 2 or 3 bands.
.VARIABLE NLOUT
Number of lines in output scatterplot image
.VARIABLE NSOUT
Number of samples in output scatterplot image
.VARIABLE LABELS
The labels associated with each band.  Will be
plotted along each axis.
.VARIABLE INCR
This specifies both LINC and SINC, q.v.
.VARIABLE LINC
Line increment:  this specifies that every LINC lines will be skipped,
in order to reduce the number of data points.
.VARIABLE SINC
Sample increment:  this specifies that every SINC samples will be skipped,
in order to reduce the number of data points.
.VARIABLE THRESH
If THRESH is specified then all DN values below this value will be ignored.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmssview.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

gen input.mss nl=30 ns=180

mssview  input.mss  output.img  mss=6 bands=(3,4) nlout=300 nsout=400 +
				labels=("red band","ir band")
! Make sure RED... label appears
list output.img ns=15 nl=40 'zero size=(286,125,10,17)
! Look at overview
size output.img small zoom=-32 
list small 'zero

mssview  input.mss  output.img  mss=6 bands=(1,2,5) 
! Take a look at some text
list output.img ns=15 nl=40 size=(24,6,40,16) 'zero
! Now look the overview
size output.img small zoom=-20 
list small 'zero


end-proc
$ Return
$!#############################################################################
