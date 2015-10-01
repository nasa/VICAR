$!****************************************************************************
$!
$! Build proc for MIPL module linemtch
$! VPACK Version 1.9, Tuesday, May 12, 1998, 11:35:08
$!
$! Execute by entering:		$ @linemtch
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
$ write sys$output "*** module linemtch ***"
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
$ write sys$output "Invalid argument given to linemtch.com file -- ", primary
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
$   if F$SEARCH("linemtch.imake") .nes. ""
$   then
$      vimake linemtch
$      purge linemtch.bld
$   else
$      if F$SEARCH("linemtch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake linemtch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @linemtch.bld "STD"
$   else
$      @linemtch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create linemtch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack linemtch.com -
	-s linemtch.f -
	-i linemtch.imake -
	-p linemtch.pdf -
	-t tstlinemtch.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create linemtch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C---- VICAR "LINEMTCH".
C
C
C   REVISION HISTORY
C
C     5-96   SP  Corrected DCODE for calls to MVE and MINMAX.  Changed to
C                call the current version of FFTT instead of (a previous
C                one?) with 4 arguments.  Added print of final FFT offset
C                as a consistency check. (The value should be small in
C                absolute value.  Increased buffer size to 60000.
C                Changed to use IBIS-2 calls for output file.  Dropped
C                the "purging of buffers" which wrote numerous rows of
C                zeroes after regular data.

      COMMON/BUFFER/LBUFA(60000),LBUFB(60000)
      BYTE LBUFA,LBUFB
      LOGICAL XVPTST,PRINT
      REAL*4 GRA(1024),GRB(1024),ABSGRB(1024),RLCORR(1024)
      REAL*4 ITIEAR(2),OTIEAR(2)
      INTEGER*4 LINEAR(200)
      INTEGER*4 ITIEDF,OTIEDF,LINEDF,MINSDF,MAXSDF,SPACDF,AREADF,
     *          POWEDF
      INTEGER WUNIT, IBIS, IRECORD, NUMPTS, STATUS

      DIMENSION CORMMX(9),IMMX(9),OFFS(9)
      INTEGER*4 LINEA(100),LINEB(100)
      COMPLEX GRACOM(1024),GRBCOM(1024),GRAC(1024),GRBC(1024)
      COMPLEX CONGRB(1024),CORR(1024)
      DIMENSION ROWBUF(5),ICOLS(5)
      DATA JAREA/128/,JSTEP/12/,IPOWER/5/
      DATA PRINT/.TRUE./
      DATA ROWBUF/5*0./,NCOL/5/,ICOLS/1,2,3,4,5/
C-------------------------------------------------------
C---- READ PARAMETERS.
C
      CALL XVPARM('ITIE',ITIEAR,NITIE,ITIEDF,0)
      CALL XVPARM('OTIE',OTIEAR,NOTIE,OTIEDF,0)
      X1ITIE = ITIEAR(1)
      X2ITIE = ITIEAR(2)
      IF (X1ITIE .EQ. X2ITIE) 
     .   CALL MABEND('ERROR:ITIE values may not be the same')
      X1OTIE = OTIEAR(1)
      X2OTIE = OTIEAR(2)
      IF (X1OTIE .EQ. X2OTIE) 
     .   CALL MABEND('ERROR:OTIE values may not be the same') 
      
      CALL XVPARM('LINES',LINEAR,NLINE,LINEDF,0)
      NPAIR = NLINE/2
      IF (MOD(NLINE,2) .NE. 0) 
     . CALL MABEND('ERROR: Number of values for parameter LINES is odd')
      DO 100 I=1,NPAIR
         LINEA(I) = LINEAR(2*I-1)
         LINEB(I) = LINEAR(2*I)
  100 CONTINUE
      CALL XVPARM('MINS',MINS,NMINS,MINSDF,0)
      CALL XVPARM('MAXS',MAXS,NMAXS,MAXSDF,0)
      CALL XVPARM('SPACING',ISPAC,NSPAC,SPACDF,0)
      CALL XVPARM('AREA',JAREA,NAREA,AREADF,0)
      CALL XVPARM('POWER',IPOWER,NPOWER,POWEDF,0)
      PRINT = .NOT.XVPTST('NOPRINT')
      IF(IPOWER.GT.10) GO TO 2000
      IDIM = 2**IPOWER
      IF(AREADF.EQ.0) JSTEP =(JAREA/2-IDIM/2)/4
      IF(JSTEP.LT.0) JSTEP=0
      CALL PRNT(4,1,IDIM,'SAMPLING SIZE .')
      CALL PRNT(4,1,JAREA,'SEARCH AREA .')
C
C---- OPEN FILES.
C
      CALL XVUNIT(RUNIT1,'INP',1,STATUS,' ')
      CALL XVOPEN(RUNIT1,STATUS, 'OPEN_ACT','SA', 'IO_ACT','SA' ,' ')
      CALL XVGET(RUNIT1,STATUS,'NL',NLA,'NS',NSA,' ')
      CALL XVPCNT('INP',NINP)
      RUNIT2 = RUNIT1
      NLB = NLA
      NSB = NSA
      IF(NINP.EQ.1) GO TO 200
      CALL XVUNIT(RUNIT2,'INP',2,STATUS,' ')
      CALL XVOPEN(RUNIT2,STATUS, 'OPEN_ACT','SA', 'IO_ACT','SA' ,' ')
      CALL XVGET(RUNIT2,STATUS,'NL',NLB,'NS',NSB,' ')
  200 CONTINUE

      LCOL = (MAXS-MINS)/ISPAC+1       ! LOCATIONS PER LINE
      NUMPTS = LCOL * NPAIR            ! NUMBER 0F ROWS = NUMBER OF POINTS
      CALL XVUNIT(WUNIT,'OUT',1,STATUS,' ')
      CALL IBIS_FILE_OPEN(WUNIT,IBIS,'WRITE',NCOL, NUMPTS,
     +                          ' ','ROW',STATUS)
       IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL IBIS_RECORD_OPEN(IBIS,IRECORD,' ',    ! 1 record (row) is cols. 1-5.
     +                        ICOLS,NCOL,'NONE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

C
C---- BEGIN LINE MATCHING.
C
      IRW = 0    ! moved bam - work courtesy of Thomas Huang

      TRAN1 = (X1ITIE-X2ITIE)/(X1OTIE-X2OTIE)
      TRAN2 = (X1OTIE*X2ITIE-X2OTIE*X1ITIE)/(X1OTIE-X2OTIE)
      DO 1000 N=1,NPAIR
       CALL XVREAD(RUNIT1,LBUFA,STATUS,'LINE',LINEA(N),' ')
       CALL XVREAD(RUNIT2,LBUFB,STATUS,'LINE',LINEB(N),' ')
C
C---- MATCH PORTIONS OF B-IMAGE STARTING AT "MINS" WITH SPACING
C     "SPAC".
C
      DO 900 IS=MINS,MAXS,ISPAC
C
C---- OBTAIN RESAMPLING, FFT, CONJUGATE FFT, ABS FROM THE "B".
C
         CALL GETGR(IDIM,IS,1.,0.,IDIM,LBUFB,GRB,GRBCOM)
         CALL MVE(7,2*IDIM,GRBCOM,GRBC,1,1)
         CALL FFTT(IPOWER,-1,GRBC)
         DO 910 I=1,IDIM
            CONGRB(I) = CONJG(GRBC(I))
            ABSGRB(I) = CABS(GRBC(I))
  910    CONTINUE
C
C---- CONDUCT SEARCH IN JAREA-SAMPLE SEARCH AREA OF THE "A".
C
      JS = IS-5*JSTEP
      DO 800 J=1,9
         JS = JS+JSTEP
C
C---- OBTAIN RESAMPLING, FFT, ABS FROM THE "A".
C
         CALL GETGR(IDIM,JS,TRAN1,TRAN2,IDIM,LBUFA,GRA,GRACOM)
         CALL MVE(7,2*IDIM,GRACOM,GRAC,1,1)
         CALL FFTT(IPOWER,-1,GRAC)
C
C---- COMPUTE CORRELATION FUNCTION (INVERSE FFT).
C
         CORR(1) = CMPLX(0.,0.)
         DO 810 I=2,IDIM
            RNORM = AMAX1(1.E-6,(CABS(GRAC(I))*ABSGRB(I)))
            CORR(I) = GRAC(I)*CONGRB(I)/RNORM
  810    CONTINUE
         CALL FFTT(IPOWER,1,CORR)
         DO 820 I=1,IDIM
            RLCORR(I) = REAL(CORR(I))
  820    CONTINUE
C
C---- FIND LOCAL MAXIMA FOR CORRELATIONS IN A JAREA-SAMP SEARCH AREA
C
         CALL MINMAX(7,IDIM,RLCORR,CORMIN,CORMAX,IMIN,IMAX)
         CORMMX(J) = CORMAX
         IMMX(J) = IMAX-1
         OFFS(J) = IMMX(J)
         IF(IMMX(J).GT.IDIM/2) OFFS(J)=IMMX(J)-IDIM
         OFFS(J) = OFFS(J)+JS-IS
  800 CONTINUE
C
C---- FIND FINAL OFFSET.
C
      CALL MINMAX(7,9,CORMMX,CMIN,CMAX,JMIN,JMAX)
      INDMAX = IMMX(JMAX)
      OFFSET = OFFS(JMAX)
      IF(PRINT)CALL XVMESSAGE('------------------------',' ')
      IF(PRINT)CALL PRNT(4,1,IS,'SAMPLE IN B=.')
C
C---- PERFORM FFT AT FOUND OFFSET.
C
      JS = IS+OFFSET
      CALL GETGR(IDIM,JS,TRAN1,TRAN2,IDIM,LBUFA,GRA,GRACOM)
      CALL MVE(7,2*IDIM,GRACOM,GRAC,1,1)
      CALL FFTT(IPOWER,-1,GRAC)
      CORR(1) = CMPLX(0.,0.)
      DO 920 I=2,IDIM
         RNORM = AMAX1(1.E-6,(CABS(GRAC(I))*ABSGRB(I)))
         CORR(I) = GRAC(I)*CONGRB(I)/RNORM
  920 CONTINUE
      CALL FFTT(IPOWER,1,CORR)
      DO 930 I=1,IDIM
         RLCORR(I) = REAL(CORR(I))
  930 CONTINUE
      CALL MINMAX(7,IDIM,RLCORR,CORMIN,CORMAX,IMIN,IMAX)
      IF(PRINT)CALL PRNT(7,1,CORMAX,'CORRELATION VALUE=.')

      OFFSET = IMAX-1
      IF (OFFSET .GT. IDIM/2)    OFFSET = OFFSET-IDIM
      IF(PRINT)CALL PRNT(7,1,OFFSET,'FFT OFFSET=.')
      
C
C---- PERFORM SUBPIXEL FIT.
C
      I2 = IMAX
      I3 = MOD(IMAX,IDIM)+1
      I1 = IMAX-1
      IF(I1.EQ.0) I1=IDIM
      F1 = RLCORR(I1)
      F2 = RLCORR(I2)
      F3 = RLCORR(I3)
      X2 = IMAX-1
      IF(IMAX.GT.(IDIM/2+1)) X2 = X2-IDIM
      CALL SUBFIT(X2-1,X2,X2+1,F1,F2,F3,X)
      OFFSET = JS-IS+X
      SAMPA = IS+OFFSET
      IRW = IRW+1
      ROWBUF(1) = N
      ROWBUF(2) = SAMPA
      ROWBUF(3) = IS
      ROWBUF(4) = OFFSET
      ROWBUF(5) = CORMAX

      CALL IBIS_RECORD_WRITE(IRECORD, ROWBUF,IRW,STATUS)
          IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      IF(PRINT)CALL PRNT(7,1,OFFSET,'OFFSET=.')
      IF(PRINT)CALL PRNT(7,1,SAMPA,'SAMPLE IN A=.')
  900 CONTINUE
 1000 CONTINUE

      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      RETURN            ! NORMAL COMPLETION

 2000 CONTINUE
      CALL XVMESSAGE('PARAMETER ERROR',' ')
      CALL ABEND
      RETURN
      END
C***********************************************
C             RESAMPLE PIXEL DATA ACCORDING TO GEOMETRIC MODEL.

      SUBROUTINE GETGR(IDIM,ICENTR,TRAN1,TRAN2,NPT,LBUF,GR,GRCOM)
      INCLUDE 'fortport'    !DEFINES BYTE2INT CONVERSION.

      BYTE LBUF(*)
      COMPLEX GRCOM(*)
      REAL*4 GR(*)
C===================================================================
      GAP = TRAN1*1.
      TCENTR = AINT(TRAN1*ICENTR+TRAN2)+.5
      XRES = TCENTR-(IDIM/2+.5)*GAP
      DO 100 I=1,IDIM
         XRES = XRES+GAP
         IRES = XRES
         IGR1 = BYTE2INT(LBUF(IRES))
         IGR2 = BYTE2INT(LBUF(IRES+1))
         GR0 = (XRES-IRES)*(IGR2-IGR1)+IGR1
         GR(I) = GR0
         GRCOM(I) = CMPLX(GR(I),0.0)
  100 CONTINUE
      RETURN
      END
C***********************************************
      SUBROUTINE SUBFIT(X1,X2,X3,F1,F2,F3,X)
      DELA = (F1-F3)*X2 + (F2-F1)*X3 + (F3-F2)*X1
      DELB = (F2-F3)*X1*X1 + (F1-F2)*X3*X3 + (F3-F1)*X2*X2
      X = -.5*DELB/AMAX1(1.E-6,DELA)
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create linemtch.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM linemtch

   To Create the build file give the command:

		$ vimake linemtch			(VMS)
   or
		% vimake linemtch			(Unix)
************************************************************************/
#define PROGRAM	linemtch
#define R2LIB

#define MODULE_LIST linemtch.f
#define FTNINC_LIST fortport
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create linemtch.pdf
PROCESS        HELP=*
! LINEMTCH
! VICAR/IBIS SOFTWARE
! VICAR2/MIPL VERSION
PARM INP TYPE=(STRING) COUNT=(1:2) 
PARM OUT TYPE=(STRING) COUNT=1
PARM ITIE TYPE=REAL COUNT=2
PARM OTIE TYPE=REAL COUNT=2
PARM LINES TYPE=INTEGER COUNT=(2:200)
PARM MINS TYPE=INTEGER
PARM MAXS TYPE=INTEGER
PARM SPACING TYPE=INTEGER
PARM POWER TYPE=INTEGER DEFAULT=5
PARM AREA TYPE=INTEGER DEFAULT=128
PARM NOPRINT TYPE=KEYWORD VALID=NOPRINT COUNT=(0:1) DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program LINEMTCH
.HELP     
PURPOSE
     LINEMTCH performs one-dimensional line-to-line correlation between two
images.  It is helpful for precise comparison or registration of two images.
It allows one of the images to have a different scale than the other or
to be offset from each other, but does not allow one image to be rotated 
with respect to the other.
It is similar in purpose to program PICMATCH, but is more special purpose.
It could have application in geometric calibration of cameras.
.page
LINEMTCH requires that the geometric realtionship between the two images be 
approximately linear.  It computes the variation (OFFSET) between the images
and the linear model.  Thus it can perform precise (subpixel) measurement
of non-linear variations.

TAE COMMAND LINE FORMAT

     LINEMTCH INP=(A,B) INP=INT  PARAMS

     where
     A                   is the first image to be correlated.
     B                   is the second image to be correlated.
     INT                 is  the output IBIS interface  file 
                         that contains the pairs of matching 
                         sample   locations  together   with 
                         editing    information   such    as 
                         correlation value.
.PAGE
OPERATION

     First  the  program sets up a geometric model  relating 
     the two images using ITIE-OTIE parameters.  Since it is 
     assumed  that  the relationship between the geometry of
     the two images is approximately linear,   two 
     tiepoints should be enough to specify the  model.   The 
     model is used to estimate the a search locations and to 
     resample  the input image to match the sampling of  the 
     output  image.   Then  the  FFTs of  input  and  output 
     samplings  are computed and phase correlation  function 
     is obtained as an inverse FFT of the normalized product 
     of those two.   The maximum of the correlation function 
     should  yield the value of offset.   If the search area 
     is  larger than the sampling size,  program extracts  9 
     uniformly  spaced  samplings from the  search  area  of 
     input image and choses the offset value as maximum of 9 
     phase correlation function,  thus ruling out accidental 
     matches.   For each line pair, at the sample locations
     defined by the parameters MINS, MAXS, & SPACING, the offset
     (between the geometric model and the location of the
     cross-correlation maximum) and the correlation value
     are computed. The  final results are placed in columns  of 
     the IBIS interface file as follows:

Column:      1            2         3         4       5
Value:  line pair # input samp  output samp  offs  correlation
                                                        value
EXAMPLE

     linemtch INP=(A,B) OUT=INT ITIE=(1,400) OTIE=(20,730)+
          LINE=(1,1,5,5) MINS=100 MAXS=800 SPAC=100 POWER=5+
          AREA=128
RESTRICTIONS
     Input images must have byte format.
     Line size <= 60000 samples
     Power <= 10, area <= 1024
.PAGE
WRITTEN BY:                   B. Gokhman

COGNIZANT PROGRAMMER:         K. F. Evans

REVISION:                     New           March 4, 1983

  7-96  SP   Ported to UNIX.  Unfortunately, the only source file available
             appears as if it was not the final version.  It called a version
             of FFTT that had 4 arguments.  The current FFTT has 3 arguments.
             The phase correlation values are supposed to be normalized to
             1.0 (for an exact match) but the version I am delivering
             does not seem to give values greater than .5.  This program
             probably needs more debugging before it is used, which may
             be never.  The exact meaning of the offset column in the
             output file remains fuzzy.

.LEVEL1
.VARIABLE INP
Input images to correlate
.VARIABLE OUT
Interface file for corr info
.VARIABLE ITIE
Samp coord of tiepoints(input)
.VARIABLE OTIE
Samp coord of tiepoints(output)
.VARIABLE LINES
Line numbers to correlate
.VARIABLE MINS
First sample for matching
.VARIABLE MAXS
Last sample for matching
.VARIABLE SPACING
Step in samples
.VARIABLE AREA
Search area in samples
.VARIABLE POWER
Size of sampling (power of 2)
.VARIABLE NOPRINT
Suppress printout
.LEVEL2
.VARIABLE INP
     INP=(A,B)             A  and  B are the  images  to  be                           
                           correlated.
.VARIABLE OUT
     OUT=INT               is the output IBIS interface file 
                           that   contains   the  pairs   of 
                           matching     sample     locations 
                           together with editing information 
                           such as correlation value.
.VARIABLE ITIE
     ITIE=(X1,X2)          X1  and  X2  specify  the  sample 
                           coordinates  of the two tiepoints 
                           on the line of "input" images (in 
                           GEOM sense).
.VARIABLE OTIE
     OTIE=(Y1,Y2)          Y1  and  Y2  specify  the  sample 
                           coordinates of the two  tiepoints 
                           on the line of "output" image (in 
                           GEOM sense).

.VARIABLE LINES
     LINES=(LI1,LO1,LI2,LO2,...LIn,LOn)

                           Pairs  LIK,  LOK specify the line 
                           numbers from "input" and "output" 
                           images  to be  correlated.   n  <= 
                           100.

.VARIABLE MINS
     MINS=m                Integer  m  specifies  the  first 
                           sample to perform matching.

.VARIABLE MAXS
     MAXS=n                Integer   n  specifies  the  last 
                           sample to perform matching.

.VARIABLE SPACING
     SPACING=k             Integer  k specifies the step  in 
                           samples.

.VARIABLE AREA
     AREA=a                Integer  a specifies  the  search 
                           area  of  input image,  which  is 
                           scanned  for  matching  with  the 
                           fixed  sampling from  the  output 
                           image.   It  is recommended  that 
                           area  is chosen to be a power  of 
                           2,   i.e.;   32,  64,  128,  etc.  
                           Default:  a = 128.   a <= 1024.

.VARIABLE POWER
     POWER=j               Integer  j specifies the size  of 
                           sampling (2**j)   that   is   used   in 
                           correlations.     FFT     routine 
                           employed  requires that  sampling 
                           is chosen as a power of 2,  i.e.; 
                           32,  64, 128, etc.  Default:  j = 
                           5, i.e.; sampling = 32.  j <= 10.

.VARIABLE NOPRINT
     NOPRINT               Cancels   the   printout    mode.  
                           Default:   results of matching in 
                           every point will be printed out.
$ Return
$!#############################################################################
$Test_File:
$ create tstlinemtch.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

gen a 100 100 sinc=20.33 linc=0.47 

copy a a2

!FFTs care mainly about shapes; this should work.
stretch a b 'gauss 

size a c lzoom=2 szoom=1

write "use identical images and tell program one is offset by 0.6 pixel"
linemtch (a, a2) table1 itie=(10,90) otie=(10.6,90.6) +
  lines=(11,11,51,51,91,91) mins=20 maxs=80 spacing=30 +
  area=32 
ibis-list table1

write "compare unstretched and stretched images.  "
write "LINEMTCH should see them as same."
linemtch (a, b) table2 itie=(10,90) otie=(10,90) + 
  lines=(11,11,51,51,91,91) mins=20 maxs=80 spacing=30 +
  area=32
ibis-list table2

write "LINEMTCH should see line 10 of 'a' the same as line 20 of 'c'"  
linemtch (a, c) table3 itie=(10,90) otie=(10,90) +
  lines=(10,20,50,100,90,180) mins=20 maxs=80 +
  spacing=30 area=32
ibis-list table3

end-proc
$ Return
$!#############################################################################
