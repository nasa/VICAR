$!****************************************************************************
$!
$! Build proc for MIPL module fftmagic
$! VPACK Version 1.7, Thursday, July 21, 1994, 11:10:37
$!
$! Execute by entering:		$ @fftmagic
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
$ write sys$output "*** module fftmagic ***"
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
$ write sys$output "Invalid argument given to fftmagic.com file -- ", primary
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
$   if F$SEARCH("fftmagic.imake") .nes. ""
$   then
$      vimake fftmagic
$      purge fftmagic.bld
$   else
$      if F$SEARCH("fftmagic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftmagic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftmagic.bld "STD"
$   else
$      @fftmagic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftmagic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftmagic.com -
	-s fftmagic.f -
	-i fftmagic.imake -
	-p fftmagic.pdf -
	-t tstfftmgc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftmagic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ASM, OCT 1983
C**********************************************************************
C
      SUBROUTINE MAIN44
      EXTERNAL WORK
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &   BETA,NPSF,LOOP,NRESET
C        
      CALL IFMESSAGE('FFTMAGIC version 31-OCT-94')
C
      CALL PARAMP(INUNIT1,INUNIT2)
      CALL STACKA(6,WORK,2,4*NL2*NPIX,4*NL2*NPIX,INUNIT1,INUNIT2)
      RETURN
C100   CALL MABEND('NOT ENOUGH CORE FOR STACKA')
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE WORK(PIX,LPIX,REF,LREF,INUNIT1,INUNIT2)
      COMMON /MBUF/ MBUF
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      INTEGER PSF
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)
      COMMON /PSF/ PSF(7,100)
      
      CHARACTER*132 MBUF
C      CALL MVE(1,38,
C     &    ' PIXELS CHANGED   SUM NEGATIVES   BETA',
C     &    MBUF(1),1,1) 
      MBUF(1:38)=' PIXELS CHANGED   SUM NEGATIVES   BETA'
C
      IF (LPIX.LT.4*NPIX*NL2 .OR. LREF.LT.4*NL2*NPIX) THEN
          CALL MABEND('NOT ENOUGH CORE FOR STACKA')
      END IF
C        
C       READ IN IMAGE ESTIMATE DSRN2
C        
      CALL LDIMAG(INUNIT1,PIX) 
      CALL RFT2(PIX,NX,NPIX,1,STATUS)   
C        
C       READ IN REFERENCE IMAGE(BLURRED)  DSRN3  
C        
      CALL LDIMAG(INUNIT2,REF) 
      IF (NPSF.GT.0) CALL FNDSTR(REF)
      CALL RFT2(REF,NX,NPIX,1,STATUS)
      CALL XVMESSAGE(MBUF,' ') 
C      DO KK=1,100
C          MBUF(KK:KK)=' '
C      END DO
      MBUF = ' '
C        
      LOOP=0 
      DO WHILE (LOOP.EQ.0 .OR. 
     &        (NRESET.NE.0 .AND. LOOP.LT.ITER)) 
C
          LOOP=LOOP+1 
C        
          IF (MODE.EQ.1) THEN 
              CALL MAMPL(PIX,REF)
          ELSE 
              CALL MPHASE(PIX,REF)
          END IF
C        
C       TAKE INVERSE FFT OF PIX  
C        
          CALL RFT2(PIX,NX,NPIX,-1,STATUS)
          CALL PICBOU(PIX)
          IF (NRESET.NE.0 .AND. LOOP.LT.ITER) THEN
              IF (NPSF.GT.0) CALL SETSTR(PIX)
C        
C	TAKE DIRECT FFT OF PIX
C        
              CALL RFT2(PIX,NX,NPIX,1,STATUS)
          END IF
C       
      END DO
C        
      CALL OUTLST(PIX) 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE PARAMP(INUNIT1,INUNIT2)
      INTEGER IPAR(600)
      LOGICAL XVPTST
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      COMMON /PSF/ PSF(7,100)

      CALL XVUNIT(INUNIT1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT1,ISTATUS,'U_FORMAT','REAL',' ')
      CALL XVSIZE(ISL,ISS,NX,NPIX,NLI,NSI)
      NL2 = NX + 2

      CALL XVUNIT(INUNIT2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INUNIT2,ISTATUS,'U_FORMAT','REAL',' ')

      IF (XVPTST('PHASE')) THEN
          MODE=2
      ELSE 
          MODE=1
      END IF

      IF (XVPTST('PRINT')) THEN
          IPRINT=1
      ELSE 
          IPRINT=0
      END IF

      CALL XVP ('ITER',ITER,ICNT)
      CALL XVP ('BETA',BETA,ICNT)

      CALL XVP ('PSF',IPAR,ICNT)
      IF ( ICNT .NE. 0 ) THEN
          NPSF=IPAR(1)
          I=2
          DO N=1,NPSF 
              PSF(5,N) = IPAR(I)
              PSF(6,N) = IPAR(I+1)
              PSF(7,N) = IPAR(I+2)
              I=I+3
          END DO
      ELSE
          NPSF=0
      END IF
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE LDIMAG(INUNIT,BUF)
C        
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL BUF(NPIX,NL2)

      L=0
      DO LINE=ISL,ISL+NX-1 
              L=L+1 
              CALL XVREAD(INUNIT,BUF(1,L),ISTATUS,'LINE',LINE, 
     +                    'SAMP',ISS,'NSAMPS',NPIX,' ')
      END DO
      CALL XVCLOSE(INUNIT,ISTATUS,' ') 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE MAMPL(PIX,REF)
C        
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)
      DO L=1,NL2,2 
          DO J=1,NPIX 
              PA=PIX(J,L)**2+PIX(J,L+1)**2 
              RA=REF(J,L)**2+REF(J,L+1)**2 
              IF (PA.GT.0.0) THEN
                  S=SQRT(RA/PA) 
                  PIX(J,L)=PIX(J,L)*S 
                  PIX(J,L+1)=PIX(J,L+1)*S 
              END IF
          END DO
      END DO
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE MPHASE(PIX,REF)
C        
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)
      DO L=1,NL2,2 
          DO J=1,NPIX
              PA=PIX(J,L)**2+PIX(J,L+1)**2 
              RA=REF(J,L)**2+REF(J,L+1)**2 
              IF (RA.GT.0.0) THEN
                  S=SQRT(PA/RA) 
                  PIX(J,L)=S*REF(J,L) 
                  PIX(J,L+1)=S*REF(J,L+1) 
              ELSE 
                  PIX(J,L)=0.0 
                  PIX(J,L+1)=0.0 
              END IF
          END DO
      END DO
C        
      PIX(1,1)=REF(1,1) 
      PIX(1,2)=REF(1,2) 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE PICBOU(PIX)
      CHARACTER*132 MBUF
C
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      COMMON /MBUF/ MBUF
      REAL PIX(NPIX,NL2)
      NRESET=0
      SUMNEG=0.E0
C        
      IF (LOOP.EQ.(LOOP/2)*2) THEN
          SCALE_NEG = -BETA 
      ELSE 
          SCALE_NEG = 0.E0 
      END IF        
      S=NX*NPIX*2 
      DO L=1,NX 
          DO J=1,NPIX 
              PIX(J,L)=PIX(J,L)/S          
              IF (PIX(J,L).LT.0.0) THEN
                  SUMNEG=SUMNEG+PIX(J,L) 
                  PIX(J,L)=PIX(J,L)*SCALE_NEG 
                  NRESET=NRESET+1 
              END IF
          END DO
      END DO
      IF (IPRINT.GT.0) THEN
          WRITE (MBUF,9900) NRESET,SUMNEG,SCALE_NEG
9900  FORMAT ('     ',I5,'          ',F10.1,'  ',F6.2)
          CALL XVMESSAGE(MBUF(2:50),' ')
      END IF
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE OUTLST(PIX)
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      LOGICAL XVPTST
      REAL PIX(NPIX,NL2)
C        
C        
      CALL XVUNIT(IOUTUNIT,'OUT',1,ISTATUS,' ')
      IF (XVPTST('OUTREAL')) THEN
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','REAL',' ')
      ELSE IF (XVPTST('OUTINTEG')) THEN
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','FULL',' ')
      ELSE IF (XVPTST('OUTHALF')) THEN
          DO L=1,NX
              DO J=1,NPIX
                  IF (PIX(J,L).GT.32767) PIX(J,L)=32767
              END DO
          END DO
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','HALF',' ')
      ELSE
          DO L=1,NX
              DO J=1,NPIX
                  IF (PIX(J,L).GT.255) PIX(J,L)=255 
              END DO
          END DO
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','BYTE',' ')
      END IF      

      DO L=1,NX
           CALL XVWRIT(IOUTUNIT,PIX(1,L),ISTATUS,'NSAMPS',
     +                 NPIX,' ')
      END DO
C        
      CALL PRNT(4,1,LOOP,' ITERATION ENDED ON PASS .') 
      CALL XVCLOSE(IOUTUNIT,ISTATUS,' ')
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE FNDSTR(REF)
C        
      REAL REF(NPIX,NL2)
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      INTEGER PSF
      COMMON /PSF/ PSF(7,100)
      INTEGER LINE,SAMP,LEFT,RIGHT,TOP,BOTTOM
      REAL MAXDN,SUMDN
C        
      DO N=1,NPSF  
C        
          LEFT=PSF(6,N)-PSF(7,N)   
          RIGHT=PSF(6,N)+PSF(7,N) 
          TOP=PSF(5,N)-PSF(7,N) 
          BOTTOM=PSF(5,N)+PSF(7,N) 
          IF (LEFT.LT.1) LEFT=1 
          IF (RIGHT.GT.NPIX) RIGHT=NPIX 
          IF (TOP.LT.1) TOP = 1 
          IF (BOTTOM.GT.NX) BOTTOM=NX 
          PSF(1,N)=TOP
          PSF(2,N)=LEFT
          PSF(3,N)=BOTTOM
          PSF(4,N)=RIGHT
C        
          LINE=TOP  
          SAMP=LEFT
          MAXDN=REF(SAMP,LINE) 
          DO L= TOP,BOTTOM 
              DO J=LEFT,RIGHT 
                  IF (REF(J,L).GT.MAXDN) THEN
                      MAXDN=REF(J,L) 
                      LINE=L  
                      SAMP=J
                  END IF
              END DO
          END DO
          PSF(5,N)=LINE
          PSF(6,N)=SAMP
C        
          MAXDN=0     
          DO J=LEFT,RIGHT 
              MAXDN=MAXDN+REF(J,TOP)+REF(J,BOTTOM)  
          END DO
          DO L=TOP,BOTTOM 
              MAXDN=MAXDN+REF(LEFT,L)+REF(RIGHT,L)  
          END DO
          MAXDN=MAXDN/(2*(BOTTOM-TOP+1+RIGHT-LEFT+1))   
C        
          SUMDN=0.E0      
          DO L=TOP,BOTTOM 
              DO J=LEFT,RIGHT 
                  SUMDN=SUMDN+REF(J,L)-MAXDN 
              END DO
          END DO
          PSF(7,N)=SUMDN 
          CALL PRNT(4,7,PSF(1,N),' PSF BUF=.') 
C        
      END DO
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE SETSTR(PIX)
C
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2)
      INTEGER PSF
      COMMON /PSF/ PSF(7,100)
      REAL MEANDN
C        
      DO N=1,NPSF
C        
          MEANDN=0.E0
          DO L=PSF(1,N),PSF(3,N) 
              MEANDN=MEANDN+PIX(PSF(2,N),L)+PIX(PSF(4,N),L)
          END DO
          DO J=PSF(2,N),PSF(4,N) 
              MEANDN=MEANDN+PIX(J,PSF(1,N))+PIX(J,PSF(3,N))
          END DO
          MEANDN=MEANDN/(2*(PSF(3,N)-PSF(1,N)+1+PSF(4,N)-PSF(2,N)+1)) 
C        
          DO L=PSF(1,N),PSF(3,N) 
              DO J=PSF(2,N),PSF(4,N) 
                  PIX(J,L)=MEANDN 
              END DO
          END DO
          PIX(PSF(6,N),PSF(5,N))=PSF(7,N)  
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftmagic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftmagic

   To Create the build file give the command:

		$ vimake fftmagic		(VMS)
   or
		% vimake fftmagic		(Unix)


************************************************************************/


#define PROGRAM	fftmagic
#define R2LIB

#define MODULE_LIST fftmagic.f

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
$ create fftmagic.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING 
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM OUTFMT TYPE=KEYWORD VALID=(OUTBYTE,OUTINTEG,OUTHALF,OUTREAL) DEF=OUTBYTE
PARM MODE TYPE=KEYWORD VALID=(PHASE,AMPLITUD) DEF=AMPLITUD
PARM ITER TYPE=INTEGER DEFAULT=100
PARM PRINT TYPE=KEYWORD VALID=PRINT COUNT=0:1 DEFAULT=--
PARM BETA TYPE=REAL DEFAULT=0.5
PARM PSF TYPE=INTEGER COUNT=(4:600) DEFAULT=(0,0,0,0)
END-PROC
.TITLE
fftmagic
.HELP
PURPOSE:
fftmagic provides an iterative method for obtaining either the phase from the
amplitude or the amplitude from the phase of a Fourier transform.  The
technique is controversial and may not work for all images. 

 
EXECUTION:

Examples

fftmagic INP=(EST,REF) OUT=PIC 

This command will read estimate picture EST and reference picture REF as byte
data (default), and create an output picture, PIC, also in byte format, whose
Fourier transform will correspond in amplitude (default) to that of REF.
Up to 100 iterations will be performed (default), and negative pixels on odd
iterations will be multiplied by -0.5 (the negative default of BETA).

fftmagic INP=(EST,REF) OUT=PIC  'OUTINTEG 'PHASE

This command is similar, except that the output picture will be in
in integer format, as specified by the OUTINTEG command, instead
of the default, which is to match the format of the input pictures; other
output format specifications include OUTREAL, OUTBYTE, and OUTHALF.  Finally,
the output picture's Fourier transform will be matched in PHASE to that of
the reference picture (REF), instead of matching amplitude as in the example
above.  The omitted keywords are INPFMT, OUTFMT, and MODE, respectively.

fftmagic INP=(EST,REF) OUT=PIC ITER=200 'PRINT  BETA=0.75

This command will again read the input pictures in byte format, but will this
time perform up to 200 iterations.  Normally, the program will iterate until no
pixels have changed, or 100 loops have been made; in this case, the program
will use up to 200 iterations.  The PRINT option specified above causes the
number of pixels changed and the sum of the negative pixels to be printed at
each iteration.  Finally, negative pixels will be multiplied by -BETA = -0.75
instead of the default -0.5.

fftmagic INP=(EST,REF) OUT=PIC PSF=(1,100,20,40)

This command demonstrates the use of the PSF keyword.  PSF specifies the
(line, sample, radius) triplets needed to describe point spread functions in 
the REF image.  These triplets are preceded by the number of triplets to be
used; in this case, there is one point spread function, centered at line 100,
sample 20, and with a radius of 40.  Additional triplets can follow as
necessary.  When PSF is specified, each center position is re-centered at the
max DN value in each circular area and the sum of the pixels less background
(determined from the border points) is computed for the values within each
circle.  The, for each iteration, all of the points inside each circle are
set to the mean DN plus the sum of the DN's as previously computed.  PSF acts
as an additional boundary condition for stellar-type point spread functions.


Notes and Restrictions

1) Both FFT's must reside in core together.  Together they take NxNx8 bytes if
   N is the picture dimension.
2) (Timing restriction belonging here will be inserted when VAX benchmarks
   have been run.)  (Was:  For 64x64 pictures each iteration consumes about
   4 seconds.)


OPERATION:
fftmagic computes and stores in core the FFT's of the initial estimate picture
and the reference picture.  It then forces the amplitude or phase of the 
estimate to match that of the reference.  The inverse FFT is taken and all
negative DN's are multiplied by -BETA (as specified by the BETA keyword) on the
odd iterations and by 0 on the even iterations.  If PSF has been specified, 
then star images are also set to delta functions.


	       EST             REF
		|		|
		|		|
		V		V
             FFT(EST)       FFT(REF)
		 \             /
		  \           /
		   \         /
		    \       /
		     \     /
		      \   /
			V
		     MATCH <------------------------------+
		     PHASE OR				  |
		     AMPLITUDE				  |
			|				  |
			|				  ^
			V			   RESET NEGATIVES
		     FFT**-1 (EST) -----------> RESET PSF'S






REFERENCE:
Fienup, J.R., Optical Society of America, Optics Letters, Volume 3, No. 1.


WRITTEN BY:  J. J. Lorre, 25 October 1979
COGNIZANT PROGRAMMER:  A. S. Mazer
REVISION:  New

MADE PORTABLE FOR UNIX: CRI     03-OCT-94

.LEVEL1
.VARIABLE INP
STRING - Estimate and reference pictures
.VARIABLE OUT
STRING - Output picture
.VARIABLE SIZE
INTEGER - Standard VICAR size field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE OUTFMT
KEYWORD - Output data format (OUTBYTE, OUTHALF, OUTINTEG, OUTREAL)
.VARIABLE MODE
KEYWORD - Selects phase boundary condition (AMPLITUDE, PHASE)
.VARIABLE ITER
INTEGER - Number of iterations allowed
.VARIABLE PRINT
KEYWORD - Program status trace (PRINT)
.VARIABLE BETA
REAL - Negative-pixel factor
.VARIABLE PSF
INTEGER - Spec for point-spread functions
.LEVEL2
.VARIABLE INP
INP specifies the initial estimate picture (EST) and the reference picture 
(REF).  The initial estimate picture will be used to obtain the unknown part
of the Fourier transform.  EST is usually a random noise scene.  The reference
picture's Fourier transform will be used (either the amplitude or the phase)
as a "known" quantity.
.VARIABLE OUT
OUT specifies the picture whose Fourier transform corresponds either in phase
or amplitude to REF after ITER iterations.
.VARIABLE OUTFMT
OUTFMT specifies the format for output data.  The valid values are OUTBYTE,
OUTHALF, OUTINTEG, and OUTREAL.  The default is to use the format of the
input pictures.
.VARIABLE MODE
MODE=PHASE (or 'PHASE) specifies that the phase of the reference picture (REF)
will be used as a boundary condition, leaving the amplitude to be determined. 
The default is for the amplitude to be used as the boundary condition. 
.VARIABLE ITER
ITER specifies the maximum number of iterations allowed.  If no pixels are 
reset on any pass, the iteration process is terminated.  Default is ITER=100.
.VARIABLE PRINT
'PRINT causes the number of pixels changed and the sum of the
negative pixels to be printed at each iteration.  
.VARIABLE BETA
BETA specifies the real value to be multiplied by each negative pixel in order
to render it positive or zero.  The negative of BETA is used in the program,
and only every other iteration is used.  The default is BETA = 0.5 .
.VARIABLE PSF
The PSF keyword is used in the form PSF=(N,L1,S1,R1,...,Ln,Sn,Rn), where 
each L, S, and R triplet specifies the line, sample, and radius of a 
point-spread function in the reference (REF) image.  If PSF is specified,
each center position is re-centered at the max DN value in each circular area
and the sum of the pixels less background (determined from the border points)
is computed for the values within the circle.  Then, for each iteration, all
of the points inside each circle are set to the mean DN of the border points
and the central pixel is set to the mean DN plus the sum of the DN's as
previously computed.  PSF acts as an additional boundary condition for stellar-
type point-spread functions.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftmgc.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gausnois a nl=64 ns=64 seed=666
boxflt2 a b nlw=9 nsw=9
stretch b a table=(0.,0.,127.,0.,128.,255.,255.,255.)
gausnois b nl=64 ns=64 seed=1234567
fftmagic (b,a) c 'print
end-proc
$ Return
$!#############################################################################
