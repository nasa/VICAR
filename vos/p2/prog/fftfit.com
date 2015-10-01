$!****************************************************************************
$!
$! Build proc for MIPL module fftfit
$! VPACK Version 1.7, Tuesday, September 13, 1994, 11:32:22
$!
$! Execute by entering:		$ @fftfit
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
$ write sys$output "*** module fftfit ***"
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
$ write sys$output "Invalid argument given to fftfit.com file -- ", primary
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
$   if F$SEARCH("fftfit.imake") .nes. ""
$   then
$      vimake fftfit
$      purge fftfit.bld
$   else
$      if F$SEARCH("fftfit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftfit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftfit.bld "STD"
$   else
$      @fftfit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftfit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftfit.com -
	-s fftfit.f -
	-i fftfit.imake -
	-p fftfit.pdf -
	-t tstfftfit.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 30 SEPT 1983
C     MSTP S/W CONVERSION (VICAR PORTING) BY A SCOP, (CRI) 31 OCT 1994
C**********************************************************************
      SUBROUTINE MAIN44
      INTEGER*2 IN(1024),MAX
      COMPLEX*8 C(1024),CDC
      INTEGER*4 IPARM(2),SAMPLE,STAT,OUNIT
      REAL*4 RPARM(2)
      LOGICAL*4 XVPTST
      CHARACTER*8 FMT1,FMT2
      COMMON/C1/ C,IN

      CALL IFMESSAGE('FFTFIT version 31-OCT-94')
      CALL XVEACTION('SA',' ')
C
C ----- OPEN DATA SETS
C
      CALL XVUNIT(IUNIT1,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT1,STAT,' ')

      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT2,STAT,'U_FORMAT','HALF',' ')

      CALL XVGET(IUNIT1,STAT,'NL',NL1,'NS',NS1,'FORMAT',FMT1,' ')
      CALL XVGET(IUNIT2,STAT,'NL',NL2,'NS',NS2,'FORMAT',FMT2,' ')
      IF (FMT2.NE.'BYTE' .AND. FMT2.NE.'HALF') THEN
         CALL MABEND('SECOND INPUT MUST BE BYTE OR HALF')
      END IF

      NLI=MAX0(NL1,NL2)
      NSI=MAX0(NS1,NS2)
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NL',NLI,'U_NS',NSI,' ')

C
C ----- SET DEFAULTS
C
      NL=NL1
      NPIX=NS1
      SCALE=1.0
      LINE=NL/2+1
      SAMPLE=NPIX/2+1
      MULT=0
      ISET1=2
      ISET2=3
      IFIND=0
      S=-99.
C
C ----- PARAMETER PROCESSING
C

C        'LINE'
      CALL XVPARM('LINE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) LINE = IPARM(1)

C        'SAMPLE'
      CALL XVPARM('SAMPLE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) SAMPLE = IPARM(1)

C        'SCALE'
      CALL XVPARM('SCALE',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) S = RPARM(1)

C        'MODE' - 'MULT'
      IF(XVPTST('MULT')) MULT = 1

C        'FIND'
      IF(XVPTST('FIND')) IFIND = 1

C        'SET'
      CALL XVPARM('SET',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
         ISET1 = IPARM(1)
         ISET2 = IPARM(2)
      END IF

C
C**********************************************************************
C           FIND MAX DN TO SET CENTER OF MASK PICTURE TO
C**********************************************************************
C
      IF(IFIND.NE.0) THEN
          MAX=-32760
          DO 201 L=1,NL
              CALL XVREAD(IUNIT2,IN,STAT,'LINE',L,'NSAMPS',NPIX,' ')
              DO 202 J=1,NPIX
                  IF(MAX.EQ.IN(J)) THEN
                      K=K+1
                      LINE=LINE+L
                      SAMPLE=SAMPLE+J
                  ELSE IF (MAX.LT.IN(J)) THEN
                      MAX=IN(J)
                      LINE=L
                      SAMPLE=J
                      K=1
                  END IF
202           CONTINUE
201       CONTINUE
          LINE=LINE/FLOAT(K)+0.5
          SAMPLE=SAMPLE/FLOAT(K)+0.5
          CALL PRNT(4,1,LINE,' LINE=.')
          CALL PRNT(4,1,SAMPLE,' SAMPLE=.')
      END IF
C
C**********************************************************************
C              IF SCALE WASN'T SPECIFIED, DETERMINE HERE
C**********************************************************************
C
      IF(S.EQ.-99.) THEN
C
C  OBTAIN MEAN AMPLITUDE OF TRANSFORM IN SET AREA
C
          S=1.0
          I=ISET1
          IF(MULT.NE.1) THEN
              K=0
              AMPL=0.0
              DO 300 L=1,ISET2
                  CALL XVREAD(IUNIT1,C,STAT,'LINE',L,'NSAMPS',NPIX,' ')
                  IF(L.GE.ISET1) ISET1=1
                  J=ISET1
                  DO WHILE (J.EQ.ISET1 .OR. J.LE.ISET2)
                      AMPL=CABS(C(J))+AMPL
                      K=K+1
                      J=J+1
                  END DO
300           CONTINUE
              S=AMPL/K
          END IF
C
C  OBTAIN MEAN DN OF MASK IN SET AREA
C
          ISET1=I
          K=0
          AMPL=0.0
          DO 320 L=1,ISET2
              MASK=LINE+L-1
              IF(MASK.GT.NL) MASK=MASK-NL
              CALL XVREAD(IUNIT2,IN,STAT,'LINE',MASK,'NSAMPS',NPIX,' ')
              IF(L.GE.ISET1) ISET1=1
              J=ISET1
              DO WHILE (J.EQ.ISET1 .OR. J.LE.ISET2)
                  MISK=SAMPLE+J-1
                  IF(MISK.GT.NPIX) MISK=MISK-NPIX
                  AMPL=AMPL+IN(MISK)
                  K=K+1
                  J=J+1
              END DO
320       CONTINUE
          S=S/(AMPL/K)
      END IF
      CALL PRNT(7,1,S,' SCALE=.')
C
C**********************************************************************
C                 SET UP CONSTANTS FOR FFT FORMAT
C**********************************************************************
C
      N1=NPIX-SAMPLE+1
      N2=SAMPLE-1
      N3=N1+1
C
C**********************************************************************
C                          MAIN LINE LOOP
C**********************************************************************
C
      DO 100 L=1,NL
C
C  READ FFT RECORD # L
C
          CALL XVREAD(IUNIT1,C,STAT,'LINE',L,'NSAMPS',NPIX,' ')
          CDC=C(1)
C
C  READ CORRESPONDING MASK RECORD
C
          MASK=LINE+L-1
          IF(MASK.GT.NL) MASK=MASK-NL
          CALL XVREAD(IUNIT2,IN,STAT,'LINE',MASK,'NSAMPS',NPIX,' ')
C
C  SAMPLE LOOP
C
          IF(MULT.NE.1) THEN
C
C  FITTING MODE
C
              DO 110 J=1,N1
                  A=CABS(C(J))
                  IF(A.GE.1.0E-10) C(J)=C(J)*S*IN(N2+J)/A
110           CONTINUE
              J=N3
              K=1
              A=CABS(C(J))
              DO WHILE (A.GE.1.0E-10 .AND. J.LE.NPIX)
                  C(J)=C(J)*S*IN(K)/A
                  K = K+1
                  J = J+1
                  A=CABS(C(J))
              END DO
          ELSE
C
C  MULTIPLICATIVE MODE
C
              DO 120 J=1,N1
                  C(J)=C(J)*S*IN(N2+J)
120           CONTINUE
              K=0
              DO 121 J=N3,NPIX
                  K=K+1
                  C(J)=C(J)*S*IN(K)
121           CONTINUE
          END IF

          IF(L.EQ.1) C(1)=CDC

          CALL XVWRIT(OUNIT,C,STAT,'NSAMPS',NPIX,' ')
100   CONTINUE

      CALL XVCLOSE(IUNIT1,STAT,' ')
      CALL XVCLOSE(IUNIT2,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftfit.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftfit

   To Create the build file give the command:

		$ vimake fftfit			(VMS)
   or
		% vimake fftfit			(Unix)


************************************************************************/


#define PROGRAM	fftfit
#define R2LIB

#define MODULE_LIST fftfit.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fftfit.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING
PARM MODE TYPE=KEYWORD VALID=(MULT,AMPMATCH) DEFAULT=AMPMATCH
PARM LINE TYPE=INTEGER   COUNT=(0,1) DEFAULT=--
PARM SAMPLE TYPE=INTEGER COUNT=(0,1) DEFAULT=--
PARM FIND TYPE=KEYWORD VALID=FIND COUNT=0:1 DEFAULT=--
PARM SCALE TYPE=REAL     COUNT=(0,1) DEFAULT=--
PARM SET TYPE=INTEGER    COUNT=2 DEFAULT=(2,3)
END-PROC
.TITLE
"fftfit"
.HELP
PURPOSE:
"fftfit" will modify a complex Fourier transform created by "fft2" or "fft22"
in one of two ways:

1. The complex Fourier transform will be multiplied by an input picture.
2. The amplitude of the complex Fourier transform will be made proportional
   to an input picture.  Phase will not be altered.

"fftfit" can be used to force pictures to have identical power spectra and
can provide for the execution of detailed filters.

 
EXECUTION:

Example - Basic Parameters MULT and HALF

fftfit  INP=(FT,PIX)  OUT=FTOUT  'MULT  'HALF

Complex Fourier transform FT, which must have been created by "fft2" or "fft22",
is input to "fftfit" along with the input picture PIX which is used to modify
the transform.  The modified Fourier transform will be sent to FTOUT.  This
command demonstrates only MULT and HALF; all other options default to various
values as described in the examples below.  'MULT specifies that the
transform is to be multiplied by the input picture; this is the first mode
listed above.  Otherwise, and by default, the transform is made proportional
to the input picture.  'HALF specifies that PIX is a halfword picture;
the default is to read the input image as byte data. 


Examples - Controlling FFTOUT phase by specifying the correspondence between
	   (line,sample) in PIX and (line,sample) in FT.

fftfit  INP=(FT,PIX)  OUT=FTOUT  LINE=10  SAMPLE=15

This example shows one way in which to specify the correspondence between PIX
and FT.  The LINE keyword indicates that line 10 in PIX corresponds with line
one (the DC line) in FT.  The default is for LINE to equal NL/2 + 1 where NL
is the number of lines in FT.  The SAMPLE keyword specifies the sample number
in PIX which corresponds with column one (the DC column) in FT.  The default
is NPIX/2 + 1 where NPIX is one eighth the number of samples in FT.  All
other parameters default as described in the relevant examples.

fftfit  INP=(FT,PIX)  OUT=FTOUT  'FIND

The FIND keyword indicates that LINE and SAMPLE described above are to be
determined from PIX by locating the position of the largest DN value in PIX.
If more than one pixel has the maximum DN value, the centroid of all maximum
DN values is selected.  The default is not to perform this operation.


Examples - Controlling scaling of PIX before application to transform FT.

fftfit  INP=(FT,PIX)  OUT=FTOUT  SCALE=2.0

SCALE specifies the scaling factor applied to the input image before
processing. 
    In the multiplicative mode, SCALE (by default) equals one divided by the
mean DN value measured within the sampling area specified by the SET keyword or
its defaults; in the matching mode, where FTOUT is made proportional to the
input picture, SCALE equals A-bar divided by the mean DN value, where A-bar is
a similarly-determined reference amplitude in FT. 
    The SCALE keyword allows the user to bypass all of these calculations and
explicitly specify the scaling factor to be used.  All other parameters in this
example are allowed to default. 

fftfit  INP=(FT,PIX)  OUT=FTOUT  SET=(3,5)

SET directs the program to calculate the scaling value SCALE from an area
defined by the integer values, 3 and 5 in this example, in both PIX and FT.
A-bar is obtained from the mean amplitude in the shaded area bounded by 
N3 and N4 in the upper left quadrant to FT (see figure).

		N3	N4		NPIX=NS/8
	+-------+-------+-----------------------+
	|DC	|///////|			|DC
	|	|///////|			|
      N3+-------+///////|			|
	|///////////////|			|	matrix of 
	|///////////////|			|	complex FFT values
      N4+---------------+			|
	|					|
	|					|
	|					|
	|					|
	|					|
      NL+---------------------------------------+
         DC					 DC

The mean DC value is computed from the corresponding area in PIX starting at 
(LINE,SAMPLE) rather than at (1,1) as in FT.  Default is SET=(2,3).


Restrictions:
1. PIX should have the same number of lines and samples as FT. This assures
   a one to one correspondence between PIX pixels and complex FT pixels.


OPERATION:

"fftfit" allows two modes:

1. Force the amplitude of the Fourier transform (FT) to be a scaled replica
   of the picture (PIX).

|FTOUT(i,j)| is proportional to DN(i,j).

FTOUT(i,j) = FT(i,j) * SCALE * DN(i,j) / |FT(i,j)|

                       DN(i,j)     A-bar
	   = FT(i,j) * ------- * ---------
		       mean DN   |FT(i,j)|

where SCALE equals A-bar divided by the mean DN value and |FT| represents
the amplitude SQRT( a**2 + b**2) = |a + bi|.

2.  Multiply the FT by PIX.
						   DN(i,j)
FTOUT(i,j) = FT(i,j) * SCALE * DN(i,j) = FT(i,j) * -------
						   mean-DN


In both cases the DC term of FT is set apart from the transformation and is
never altered.  Whatever the result, the mean of the inverse transform of
FTOUT will always be the same as that of the original picture whose transform
is FT.

It is important to note the significance of the LINE, SAMPLE keywords.  These
values point to the pixel in PIX which will be centered on the DC term (1,1)
in the FT picture.  In the majority of cases PIX will be a symmetrical 
function such as a gaussian, etc.  LINE and SAMPLE will usually refer to the
center of the symmetry of this function.  When this is the case "fftfit" will be
able to divide up PIX into symmetrical quadrants and match them to the
complex format of FT (with DC in the corners).  If LINE, SAMPLE do not refer to
to a symmetry point FTOUT will be asymmetrical and the inverse transformation
will produce the real part of a complex picture (if PIX is asymmetrical) or a
displaced image (if PIX is symmetrical but just off center).

FT can be created with either "fft2" or "fft22". DC remains the same for both 
complex formats but the latter has been transposed relative to the former
(rows become columns).  "fftfit" assumes the user has organized PIX such that
it is compatible with the appropriate FT format.  Symmetrical cases are 
invariant.  One can convert PIX from one format to the other with two 
executions of "flot".

It is suggested that when the inverse transform is made (particularly in the
amplitude matching mode) the output picture be halfword.  Most operations
will cause some DN's to lie outside the byte range and halfword formatting
will allow access to these pixels.


WRITTEN BY:  J. J. Lorre, 31 October 1977
COGNIZANT PROGRAMMER:  A. S. Mazer
REVISION 1:  A. Scop, 31 October 1994   Made portable for UNIX

.LEVEL1
.VARIABLE INP
STRING - Image and transform files
.VARIABLE OUT
STRING - Output transform file
.VARIABLE MODE
KEYWORD - MULTiplication mode flag (AMPMATCH, MULT)
.VARIABLE LINE
INTEGER - Image line corr to FT line 1
.VARIABLE SAMPLE
INTEGER - Image samp corr to FT sample 1
.VARIABLE FIND
KEYWORD - Computed-correspondence flag (FIND)
.VARIABLE SCALE
REAL - Scaling factor
.VARIABLE SET
INTEGER - Area for auto SCALE computation 
.LEVEL2
.VARIABLE INP
INP specifies the complex Fourier transform and the image picture used to 
modify the transform, in that order.  The Fourier transform must have been
created by "fft2" or "fft22".
.VARIABLE OUT
OUT specifies the name of the modified Fourier transform.
.VARIABLE MODE
MODE=MULT (or 'MULT) selects the multiplication mode of "fftfit".  The
default is the amplitude matching mode. 
.VARIABLE LINE
LINE specifies the line number in the input image which corresponds to 
line one (the DC line) in FT.  The default is NL/2+1 where NL is the 
number of lines in FT.
.VARIABLE SAMPLE
SAMPLE specifies the sample number in the input image which corresponds with
column one (the DC column) in FT.  The default is NPIX/2+1 where NPIX is
one eighth the number of samples in FT.
.VARIABLE FIND
'FIND will compute the LINE and SAMPLE parameter values from the input image by
locating the position of the largest DN value.  If more than one pixel has the
maximum DN value, the centroid of all maximum DN values is selected. Default is
to the LINE, SAMPLE defaults. 
.VARIABLE SCALE
SCALE specifies the scaling factor applied to the input image before
processing. 
    In the multiplicative mode, SCALE (by default) equals one divided by the
mean DN value measured within the sampling area specified by the SET keyword or
its defaults; in the matching mode, where FTOUT is made proportional to the
input picture, SCALE equals A-bar divided by the mean DN value, where A-bar is
a similarly-determined reference amplitude in FT. 
    The SCALE keyword allows the user to bypass all of these calculations and
explicitly specify the scaling factor to be used.  All other parameters in this
example are allowed to default. 
.VARIABLE SET
SET directs the program to calculate the scaling value SCALE from an area
defined by the integer values, 3 and 5 in this example, in both PIX and FT.
A-bar is obtained from the mean amplitude in the shaded area bounded by 
N3 and N4 in the upper left quadrant to FT (see figure).

		N3	N4		NPIX=NS/8
	+-------+-------+-----------------------+
	|DC	|///////|			|DC
	|	|///////|			|
      N3+-------+///////|			|
	|///////////////|			|	matrix of 
	|///////////////|			|	complex FFT values
      N4+---------------+			|
	|					|
	|					|
	|					|
	|					|
	|					|
      NL+---------------------------------------+
         DC					 DC

The mean DC value is computed from the corresponding area in PIX starting at 
(LINE,SAMPLE) rather than at (1,1) as in FT.  Default is SET=(2,3).
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftfit.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! TEST SCRIPT FOR FFTFIT
!
! THIS SCRIPT TAKES A WHILE TO EXECUTE (FOREWARNING!)
!
gen PIC 128 128 IVAL=2 LINC=0 SINC=0 'HALF
fft22 PIC FT
fftfit (FT,PIC) OUT 'MULT 'FIND
fft22 OUT FTOUT 'INV FORMAT=HALF
list FTOUT (1,1,10,10)
hist FTOUT 'NOHIST
!
gen PIC 128 128 IVAL=2 LINC=0 SINC=0 'HALF
fft22 PIC FT
fftfit (FT,PIC) OUT SET=(2,4)
fft22 OUT FTOUT 'INV FORMAT=HALF
list FTOUT (1,1,10,10)
hist FTOUT 'NOHIST
!
gen PIC 128 128 IVAL=2 LINC=.1 SINC=.1 'HALF
fft22 PIC FT
fftfit (FT,PIC) OUT 'MULT LINE=129 SAMP=129 SCALE=0.5
fft22 OUT FTOUT 'INV FORMAT=HALF
list FTOUT (1,1,10,10)
hist FTOUT 'NOHIST
!
gen PIC 128 128 IVAL=2 LINC=.1 SINC=.1 'HALF
fft22 PIC FT
fftfit (FT,PIC) OUT SCALE=0.1
fft22 OUT FTOUT 'INV FORMAT=HALF
list FTOUT (1,1,10,10)
hist FTOUT 'NOHIST
!
! END-SCRIPT
!
end-proc
$ Return
$!#############################################################################
