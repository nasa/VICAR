$!****************************************************************************
$!
$! Build proc for MIPL module getzval
$! VPACK Version 1.8, Wednesday, December 28, 1994, 11:58:06
$!
$! Execute by entering:		$ @getzval
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
$ write sys$output "*** module getzval ***"
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
$ write sys$output "Invalid argument given to getzval.com file -- ", primary
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
$   if F$SEARCH("getzval.imake") .nes. ""
$   then
$      vimake getzval
$      purge getzval.bld
$   else
$      if F$SEARCH("getzval.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getzval
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getzval.bld "STD"
$   else
$      @getzval.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getzval.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getzval.com -
	-s getzval.f -
	-i getzval.imake -
	-p getzval.pdf -
	-t tstgetz.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getzval.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  IBIS ROUTINE GETZVAL
C
C  PURPOSE:  LOOK UP IMAGE BRIGHTNESS VALUES AT A SET OF POINTS.  THE PO
C  ARE SPECIFIED IN TWO COLUMNS OF AN IBIS INTERFACE FILE AND THE RESULT
C  BRIGHTNESS IS PLACED IN A THIRD COLUMN OF THE SAME FILE.
C
C  2 JAN 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
      IMPLICIT INTEGER(A-Z)
      COMMON /COM/DATA(16384)
      COMMON /COMG/GETGRV(2000)
      LOGICAL XVPTST,noin
      REAL SUM
      REAL LINC,SINC,PRED(2),RMAG(2),XG(6),SHIFT(2)
      REAL*4 RECORD,ROWBUF(3),INCRAR(2)
      INTEGER INIBIS,ncol
      INTEGER*4 GETAR(3)
      byte DATA,KLOG(4)
c      EQUIVALENCE (KINT,KLOG)

      DATA REJE,ITEMP/100,0/,RMAG/1.,1./,XG/1.,0.,0.,0.,1.,0./
      DATA WIND,LINC,SINC/10,1.,1./

      CALL IFMESSAGE('GETZVAL version 02-JAN-95')
C
C---- GET PARAMETERS
C
      CALL XVPARM('REJECT',REJE,NREJE,REJEDF,1)
      CALL XVPARM('WINDOW',WIND,NWIND,WINDDF,1)
      NOIN = XVPTST('NOIN')
      IF (NOIN) WIND=2
      WSQ = WIND*WIND
      CALL XVPARM('INCREM',INCRAR,NINCR,INCRDF,2)
      LINC = INCRAR(1)
      SINC = INCRAR(2)
      XG(1) = LINC
      XG(5) = SINC
      CALL XVPARM('COLS',GETAR,NGET,GETDF,3)
C
C		OPEN IMAGE AND INTERFACE FILE
C
      CALL XVUNIT(RUNIT1,'INP',1,STATUS,' ')
      CALL XVUNIT(RUNIT2,'INP',2,STATUS,' ')
      CALL XVOPEN(RUNIT1,STATUS,' ')
c      CALL RDFIL(RUNIT2,2,CL,NCOL,NOFILE)

      CALL IBIS_FILE_OPEN(RUNIT2,INIBIS,'UPDATE',0,0,' ',' ',STATUS)
      IF (STATUS .NE. 1)  CALL IBIS_SIGNAL_U(RUNIT1,STATUS,1)
      CALL IBIS_FILE_GET(INIBIS,'NR',CL,1,1)
      CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)
      

      CALL XVGET(RUNIT1,STATUS,'NL',NL,'NS',NS,' ')
C
C
C---- MAIN PROCESSING LOOP
C
      CALL IBIS_RECORD_OPEN(INIBIS,RECORD,'FORMAT:REAL',0,
     +                         NCOL,'REAL',STATUS)
      DO 10 I=1,CL
c         CALL GETREC(RUNIT2,3,GETAR,ROWBUF,I,CL,RECORD)

         CALL IBIS_RECORD_READ(RECORD,ROWBUF,I,STATUS)
         IF (STATUS .NE. 1)  CALL IBIS_SIGNAL_U(RUNIT1,STATUS,1)

	 IF (ROWBUF(1) .EQ. 0.0 .AND. ROWBUF(2) .EQ. 0.0) GOTO 30
         PRED(1) = ROWBUF(1)
         PRED(2) = ROWBUF(2)
         IF(NOIN) PRED(1)=PRED(1)+.5
         IF(NOIN) PRED(2)=PRED(2)+.5
         CALL GETGRD(RUNIT1,DATA,WIND,GETGRV,4000,WIND,NL,NS,
     *        PRED,XG,SHIFT,RMAG,CHOP,NOIN,NOINV,*100)

         IF (CHOP.GE.1) GO TO 100
C
         SUM = 0.
         KINT = 0
         IRE = 0
         DO 40 IC=1,WSQ
         KLOG(1) = DATA(IC)
c         IF (KINT.EQ.0) IRE = IRE+1
         if (klog(1) .LT.0) then
            kint = 256 + klog(1)
         else
            kint = klog(1)
         endif
         if (kint .eq. 0) ire = ire+1
 40      SUM = SUM+KINT
         IF (IRE.GT.REJE) GO TO 100
         ROWBUF(3) = SUM/FLOAT(WSQ)
         IF (NOIN) ROWBUF(3) = FLOAT(NOINV)
         GO TO 30
 100     ROWBUF(3) = -999.0
   30    CONTINUE
c         CALL PUTREC(RUNIT2,3,GETAR,ROWBUF,I,CL,RECORD)
         CALL IBIS_RECORD_WRITE(RECORD,ROWBUF,I,STATUS)
         IF (STATUS .NE. 1)  CALL IBIS_SIGNAL_U(RUNIT2,STATUS,1)
   10 CONTINUE
C
      CALL XVCLOSE(RUNIT1,STATUS,' ')
      call ibis_file_close(inibis,' ',status)
      RETURN
      END
C**************************************************************
      SUBROUTINE GETGRD(RUNIT,GRID,NDIM,BUFIN,LBUFIN,N,LINE,
     *       SAMP,CENTR,TRANS,SHIFT,MAGNIF,CHOP,NOIN,NOINV,*)
C
C---- VICAR SUBROUTINE "GETGRD".
C     PURPOSE:    TO OBTAIN THE DN-VALUES IN THE POINTS
C   OF THE LINEARLY TRANSFORMED GRID.
C   DN-VALUES ARE OBTAINED BY THE BILINEAR
C   INTERPOLATION BETWEEN FOUR PIXELS
C   ADJACENT TO THE GRID POINT.
C     PARAMETERS: IDSRN - DATA SET REFERENCE NUMBER.
C   GRID  - OUTPUT L*1 ARRAY OF INTERPOLATED
C    DN-VALUES.
C   NDIM  - DIMENSION OF THE GRID(NDIM MAX=128), EVEN
C   BUFIN - WORK AREA, TWO-DIMENSIONAL L*1 ARRAY
C    BUFIN(LBUFIN,2), LBUFIN>NDIM*MAGN(2)+50
C   LBUFIN- DEFINES THE SIZE OF BUFIN.
C   N     - DEFINES THE PORTION OF THE GRID
C    TO BE PROCESSED (MUST BE EVEN).
C   LINE  - NUMBER OF LINES IN THE IMAGE.
C   SAMP  - NUMBER OF SAMPLES IN THE IMAGE.
C   CENTR - ARRAY, CENTR(1)=Y OF THE GRID CENTER,
C    CENTR(2)=X OF THE GRID CENTER.
C   TRANS - ARRAY OF THE GRID TRANSFORMATION COEFFICIENTS:
C    YNEW=TRANS(1)*YOLD+TRANS(2)*XOLD+TRANS(3)
C    XNEW=TRANS(4)*YOLD+TRANS(5)*XOLD+TRANS(6)
C   SHIFT - VECTOR TO ADD TO THE TRANSFORMED GRID
C    TO BRING ITS CENTER TO "CENTR".
C   MAGNIF- ARRAY CONTAINING MAGNIFICATION OF
C    X- AND Y- DISTANCES IN THE GRID.
C   CHOP  - NUMBER OF GRID POINTS OUTSIDE THE IMAGE.
C    CORRESPONDING DN-VALUES IN THE "GRID"
C    ARE SET TO ZERO.
C     PROGRAMMER:  BORIS GOKHMAN, OCTOBER 1980.
C
      include 'fortport'
      LOGICAL NOIN
      byte GRID(1)
      byte BUFIN(LBUFIN,2)
      INTEGER*4 DN1,DN2,DN3,DN4
      DIMENSION TRANS(6),CENTR(2),SHIFT(2)
      REAL*4 MAGNIF(2)
      REAL*4 Y(4),X(4)
      INTEGER*4 CHOP,LINE,SAMP
      INTEGER*4 ISTART(128)
      INTEGER*4 ISGNES(4), JSGNES(4)
      DIMENSION YINC1(128),XINC2(128),YINC4(128),XINC5(128)
      LOGICAL ENDJ(128)
      DATA ISGNES / 1, -1, -1, 1 /
      DATA JSGNES / 1, 1, -1, -1 /
      DATA DN1 /0/
      DATA DN2 /0/
      DATA DN3 /0/
      DATA DN4 /0/
C
C---- PRELIMINARY CALCULATIONS
C
      NOFFB = (NDIM-N)*(NDIM+1)/2
      IHI = 1
      ILOW = 2
      RLINE = LINE
      RSAMP = SAMP
      CHOP = 0
      IF(CENTR(1).LT.1..OR.CENTR(1).GT.RLINE) RETURN 1
      IF(CENTR(2).LT.1..OR.CENTR(2).GT.RSAMP) RETURN 1
      GAPX = MAGNIF(2)
      GAPY = MAGNIF(1)
      SHIFT(1) = CENTR(1)-(N+1)/2.*(TRANS(1)+TRANS(2))-TRANS(3)
      SHIFT(2) = CENTR(2)-(N+1)/2.*(TRANS(4)+TRANS(5))-TRANS(6)
      XMIN = (N+1)/2.-GAPX*(N-1)/2.
      YMIN = (N+1)/2.-GAPY*(N-1)/2.
      XMAX = (N+1)/2.+GAPX*(N-1)/2.
      YMAX = (N+1)/2.+GAPY*(N-1)/2.
C
C---- FIND THE UPPERMOST CORNER OF THE TRANSFORMED QUAD
C     AND THE SIGN OF THE ARRAY INDEXES CHANGE.
C
      Y(1) = TRANS(1)*YMIN+TRANS(2)*XMIN+TRANS(3)+SHIFT(1)
      Y(2) = TRANS(1)*YMAX+TRANS(2)*XMIN+TRANS(3)+SHIFT(1)
      Y(3) = TRANS(1)*YMAX+TRANS(2)*XMAX+TRANS(3)+SHIFT(1)
      Y(4) = TRANS(1)*YMIN+TRANS(2)*XMAX+TRANS(3)+SHIFT(1)
      IMIN = 1
      DO 20 I=2,4
          IF(Y(I).LT.Y(IMIN)) IMIN = I
   20 CONTINUE
      ISIG = ISGNES(IMIN)
      JSIG = JSGNES(IMIN)
C
C---- FIND THE SAMPLE COORDINATES OF LEFTMOST AND
C     RIGHTMOST CORNERS OF THE GRID.
C
      X(1) = TRANS(4)*YMIN+TRANS(5)*XMIN+TRANS(6)+SHIFT(2)
      X(2) = TRANS(4)*YMAX+TRANS(5)*XMIN+TRANS(6)+SHIFT(2)
      X(3) = TRANS(4)*YMAX+TRANS(5)*XMAX+TRANS(6)+SHIFT(2)
      X(4) = TRANS(4)*YMIN+TRANS(5)*XMAX+TRANS(6)+SHIFT(2)
      IABBR = X(1)
      IRIGHT = X(1)
      DO 21 I=2,4
          IF(X(I).LT.IABBR) IABBR = X(I)
          IF(X(I).GT.IRIGHT) IRIGHT = X(I)
   21 CONTINUE
      IABBR = MAX0(IABBR-10,0)
      ICOUNT = IRIGHT-IABBR+10
C
C---- SET UP PARAMETERS FOR THE LOOP.
C
      T1GAPY = TRANS(1)*GAPY
      T2GAPX = TRANS(2)*GAPX
      T4GAPY = TRANS(4)*GAPY
      T5GAPX = TRANS(5)*GAPX
      BASEY = TRANS(1)*(YMIN-GAPY)+TRANS(2)*(XMIN-GAPX)+
     * TRANS(3)+SHIFT(1)
      BASEX = TRANS(4)*(YMIN-GAPY)+TRANS(5)*(XMIN-GAPX)+
     * TRANS(6)+SHIFT(2)
      DO 30 I=1,N
          YINC1(I) = T1GAPY*I
          XINC2(I) = T2GAPX*I
          YINC4(I) = T4GAPY*I
          XINC5(I) = T5GAPX*I
   30 ISTART(I) = 1
      I0 = (N+1)*(1-ISIG)/2
      J0 = (N+1)*(1-JSIG)/2
      JFIN = J0+ISIGN(N,JSIG)
   60 IREC = Y(IMIN)
      IF(IREC.GT.0.AND.IREC.LE.LINE)
     *CALL XVREAD(RUNIT,BUFIN(1,1),STATUS,'LINE',IREC,'SAMP',IABBR+1,
     *            'NSAMPS',ICOUNT,' ')
      IREC = IREC+1
      IF(IREC.GT.0.AND.IREC.LE.LINE)
     *CALL XVREAD(RUNIT,BUFIN(1,2),STATUS,'LINE',IREC,'SAMP',IABBR+1,
     *            'NSAMPS',ICOUNT,' ')
C
C---- BEGIN THE LOOP
C
  100 CONTINUE
      DO 300 J1=1,N
      J = J0+ISIGN(J1,JSIG)
      IF(.NOT.ENDJ(J).AND.ISTART(J).EQ.N) GO TO 300
      IST = ISTART(J)
      ENDJ(J) = .FALSE.
      DO 200 I1=IST,N
          I = I0+ISIGN(I1,ISIG)
          Y0 = BASEY+YINC1(I)+XINC2(J)
          X0 = BASEX+YINC4(I)+XINC5(J)
          IF(Y0.GE.IREC.AND.I1.EQ.1) GO TO 400
C CHECK IF THE END OF COLUMN #J IN THE STRIP IS REACHED
          ISTART(J) = I1
          IF(Y0.LT.IREC) GO TO 240
          ENDJ(J) = .TRUE.
          GO TO 300
  240    CONTINUE
          IF(X0.GE.1.0.AND.X0.LE.RSAMP.AND.Y0.GE.1.0.AND.Y0.LE.RLINE)
     * GO TO 210
          IDN = 0
          CHOP = CHOP+1
        IF (CHOP.GT.5000) CALL MABEND(5000)
          GO TO 220
  210    CONTINUE
          IX = X0
          JX = IX-IABBR
          JXPLUS = JX+1
          IY = Y0
c          LPIX1 = BUFIN(JX,IHI)
c          LPIX2 = BUFIN(JXPLUS,IHI)
c          LPIX3 = BUFIN(JX,ILOW)
c          LPIX4 = BUFIN(JXPLUS,ILOW)
          if (bufin(jx,ihi) .lt. 0) then
             dn1 = 256 + bufin(jx,ihi)
          else
             dn1 = bufin(jx,ihi)
          endif
          if (bufin(jxplus,ihi) .lt. 0) then
             dn2 = 256 + bufin(jxplus,ihi)
          else
             dn2 = bufin(jxplus,ihi)
          endif
          if (bufin(jx,ilow) .lt. 0) then
             dn3 = 256 + bufin(jx,ilow)
          else
             dn3 = bufin(jx,ilow)
          endif
          if (bufin(jxplus,ilow) .lt. 0) then
             dn4 = 256 + bufin(jxplus,ilow)
          else
             dn4 = bufin(jxplus,ilow)
          endif
      IF (NOIN) GO TO 9000
          FACTX = (X0-IX)
          DN12 = DN1+(DN2-DN1)*FACTX
          DN34 = DN3+(DN4-DN3)*FACTX
          IDN = DN12+(DN34-DN12)*(Y0-IY)+.5
  220    GRID(NDIM*(I-1)+J+NOFFB) = int2byte(idn)
  200 CONTINUE
  300 CONTINUE
C
C---- READ THE NEW LINE AND REPEAT THE LOOP
C     OR EXIT
C
  400 CONTINUE
      IF(.NOT.ENDJ(JFIN).AND.ISTART(JFIN).EQ.N) RETURN
C
C---- READ THE NEW LINE.
C
      I = IHI
      IHI = ILOW
      ILOW = I
      IREC = IREC+1
      IF(IREC.GT.0.AND.IREC.LE.LINE)
     *CALL XVREAD(RUNIT,BUFIN(1,ILOW),STATUS,'LINE',IREC,'SAMP',IABBR+1,
     *            'NSAMPS',ICOUNT,' ')
      GO TO 100
 9000 JQ=X0-IABBR+.5
      IQ=Y0+.5
      JREC = IREC
      IF(JQ.EQ.JX .AND. IQ.EQ.(JREC-1))     noinv=dn1
      IF(JQ.EQ.JXPLUS .AND. IQ.EQ.(JREC-1)) noinv=dn2
      IF(JQ.EQ.JX .AND. IQ.EQ.JREC)         noinv=dn3
      IF(JQ.EQ.JXPLUS .AND. IQ.EQ.JREC)     noinv=dn4
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getzval.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM getzval

   To Create the build file give the command:

		$ vimake getzval		(VMS)
   or
		% vimake getzval		(Unix)


************************************************************************/


#define PROGRAM	getzval
#define R2LIB

#define MODULE_LIST getzval.f

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
$ create getzval.pdf
PROCESS    HELP=*
PARM INP      TYPE=(STRING,72) COUNT=2
PARM COLS     TYPE=INTEGER COUNT=3 DEFAULT=(1,2,3)
PARM WINDOW   TYPE=INTEGER DEFAULT=10
PARM INCREM   TYPE=REAL COUNT=(1:2) DEFAULT=0.
PARM REJECT   TYPE=INTEGER DEFAULT=100
PARM NOIN     TYPE=KEYWORD COUNT=(0:1) VALID=NOIN DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program  getzval
.HELP     
PURPOSE

     getzval looks up brightness values in an image at a set 
     of  points  specified in an IBIS interface  file.   The 
     brightness   average   of  a  surrounding   window   is 
     calculated  for each point and placed back in the  IBIS 
     interface   file.    Interpolation  is  performed   for 
     fractional point locations.
.PAGE
TAE COMMAND LINE FORMAT

     getzval INP=(PIC,INT) SIZE PARAMS

     where

     PIC                 is an image.
     INT                 is    an   IBIS   interface    file 
                         containing columns which give point 
                         locations in the image PIC.
                         No  output  file is  used  but  the 
                         second input file is modified.
     SIZE                is the standard VICAR size field.
     PARAMS              is  the  standard  VICAR  parameter 
                         field.
.PAGE
OPERATION
     The  point  locations in the IBIS file are in  floating 
     point, hence they may be fractional.  The grey value is 
     calculated for a WxW interpolated grid about the  point 
     location.  Bilinear interpolation is used.  The average 
     grey  value  is  stored in the selected column  of  the 
     interface file.   In two cases,  the flag -999.0  is 
     stored:   first,  if the window touches the boundary of 
     the  image,  or  second,  if the  REJECT  threshold  is 
     achieved in the window.   Point locations of (0,0) are
     skipped over.


WRITTEN BY:                   H. Wilczynski      01Dec77
COGNIZANT PROGRAMMER:         K. F. Evans
REVISION:                     3                  May 1986
Made Portable for UNIX        CRI                02 JAN 95

.PAGE
.LEVEL1
.VARIABLE INP
Input image and interface file
.VARIABLE COLS
Column numbers for L,S and DN
.VARIABLE REJECT
Rejection threshold
.VARIABLE WINDOW
Box size
.VARIABLE INCREM
Increment for the window
.VARIABLE NOIN
No interpolation
.LEVEL2
.VARIABLE INP
     INP=(PIC,INT)       PIC is an image.
                         INT   is  an  IBIS  interface  file 
                         containing columns which give point 
                         locations in the image PIC.
                         No  output  file is  used  but  the 
                         second input file is modified.
.VARIABLE GET
     COLS=(L,S,T)   The   integers  L,S  specify  the   file 
                    columns   which  contain  the  line  and 
                    sample  coordinates  to  be  looked  up.  
                    Column T is used to store the  resultant 
                    grey values.
.VARIABLE REJECT
     REJECT=N       The  integer  N specifies that if  N  or 
                    more  grey  values  in a  rectangle  are 
                    zero,  then a "no information" marker is 
                    stored  instead of a  grey  value.   The 
                    marker  is  the value -999.0 .
.VARIABLE WINDOW
     WINDOW=W       specifies that a window of size W points 
                    square  is  used to obtain  the  average 
                    brightness  value.   The window will  be 
                    centered on the line,  sample coordinate 
                    even  if  it  is fractional.   W  is  an 
                    integer (default value 10).
.VARIABLE INCREM
     INCREM=(H,V)   The  floating numbers H and V specify  a 
                    horizontal  and vertical  increment  for 
                    the   WxW  window.    Thus  W=10  H=V=2. 
                    specifies  that a 20 pixel by  20  pixel 
                    area will be sampled by a 10x10 grid.
.VARIABLE NOIN
     NOIN           specifies no-interpolation option.   The 
                    DN-value  at the location nearest to the 
                    one  specified by line-sample  from  the 
                    columns L, S  will be read and stored in 
                    the column T.
$ Return
$!#############################################################################
$Test_File:
$ create tstgetz.pdf
procedure
refgbl $autousage
body
let $autousage="none"

!Create a file sampling every other point at 
! the exact location. Throw in a (0,0) to make
! sure that null points are skipped
ibis-gen table nc=3 nr=36 datacol=(1,2) 'ibis-1
mf table fun=("c1=INT((index-1) / 6)*2+1", +
			"c2=((index-1) % 6)*2 + 1", +
			"c2=c2*(index<36)","c1=c1*(index<36)")
ibis-list table

gen img nl=12 ns=12 mod=10
getzval (img,table) 
ibis-list table

! Test REJECT option
getzval (img,table) window=3 REJECT=1
ibis-list table

!Test the fractional-pixel sampling
mf table fun=("c1=INT((index-1) / 6)*2+0.5", +
			"c2=((index-1) % 6)*2 + 0.25")
getzval (img,table) 
ibis-list table

! No interpolation test
getzval (img,table) 'noin
ibis-list table


end-proc
$ Return
$!#############################################################################
