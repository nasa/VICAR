$!****************************************************************************
$!
$! Build proc for MIPL module wiener
$! VPACK Version 1.8, Friday, May 09, 1997, 13:49:04
$!
$! Execute by entering:		$ @wiener
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
$ write sys$output "*** module wiener ***"
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
$ write sys$output "Invalid argument given to wiener.com file -- ", primary
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
$   if F$SEARCH("wiener.imake") .nes. ""
$   then
$      vimake wiener
$      purge wiener.bld
$   else
$      if F$SEARCH("wiener.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake wiener
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @wiener.bld "STD"
$   else
$      @wiener.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create wiener.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack wiener.com -
	-s wiener.f -
	-i wiener.imake -
	-p wiener.pdf -
	-t tstwiener.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create wiener.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM WIENER  --  APPLY WIENER FILTER TO A FOURIER TRANSFORM

c  Nov 1996  JJL
C  2-JAN-95 ..CRI..   MSTP S/W Conversion (VICAR Porting)
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C  APR-85  ...LWK...  INITIIAL VERSION, FROM IBM PGM 'RESTORE'
C  MAY-85  ...LWK...  ADDED AP CODE, FROM IBM PGM 'GPFILT'

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT INTEGER (A-Z)
      parameter(maxsize=4096)
      INTEGER*4 IUN(3), OUN
      REAL*4 SN,rnorm,psfnor,outnor,mtf,factor
      complex im(maxsize),otf(maxsize),restor(maxsize)
      complex ratio
      CHARACTER*8 FMT
      logical xvptst

      CALL IFMESSAGE('WIENER version Nov 1996')

c open & check inputs
      CALL XVPCNT( 'INP', NIDS)
      DO I=1,NIDS
	CALL XVUNIT( IUN(I), 'INP', I, ISTAT,' ')
	CALL XVOPEN(IUN(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XVGET( IUN(I), STATUS, 'FORMAT', FMT, 'NL', NL1, 'NS',
     .	 NS1,' ')
	IF (FMT.NE.'COMPLEX' .AND. FMT.NE.'COMP') THEN
	  CALL XVMESSAGE('ALL INPUT FILES MUST BE COMPLEX',' ')
	  CALL ABEND
	ENDIF
	IF (I.EQ.1) THEN
	  NL = NL1
	  NS = NS1
	ELSEIF (NL1.NE.NL .OR. NS1.NE.NS) THEN
	  CALL XVMESSAGE('ALL INPUT FILES MUST BE SAME SIZE',' ')
	  CALL ABEND
	ENDIF
      ENDDO

c open outputs
      CALL XVUNIT( OUN, 'OUT', 1, ISTAT,' ')
      CALL XVOPEN( OUN, ISTAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     . 'OP', 'WRITE',' ')

c get parameters
      CALL XVPARM( 'SN', SN, I, J,1)
      SN = 1./SN**2

      if(xvptst('wiener'))then
        factor=1.0/(1.0+sn)
        factor=1.0/factor
      endif

C process image
      DO L=1,NL
	CALL XVREAD( IUN(1), IM, I,' ')
	CALL XVREAD( IUN(2), OTF, I,' ')

	IF (L.EQ.1)then
           RNORM = REAL(OTF(1))
           psfnor=real(IM(i))
        endif

c       normalize the otf
        do i=1,ns
          otf(i)=otf(i)/rnorm
        enddo

c       Apply direct otf  filter
        if(xvptst('direct'))then
  	  DO I=1,NS
	    RESTOR(I)=OTF(I)*IM(I)
	  ENDDO
        endif

c       Apply wiener restoration filter
        if(xvptst('wiener'))then
  	  DO I=1,NS
	    RESTOR(I)=IM(i)*factor*CONJG(OTF(I))/
     +        ((REAL(OTF(I)))**2+(AIMAG(OTF(I)))**2+SN)
	  ENDDO
cccccc      if(L.eq.1)restor(1)=cmplx(psfnor,0.0)
        endif

c       Obtain ratio restoration filter
        if(xvptst('ratio'))then
  	  DO I=1,NS
            if((real(OTF(i)))**2+(aimag(OTF(i)))**2.ne.0.)then
              ratio=IM(i)/(psfnor*OTF(i))
       	      RESTOR(I)=CONJG(ratio)/
     +        ((REAL(ratio))**2+(AIMAG(ratio))**2+SN)
            else
              RESTOR(i)=cmplx(0.,0.)
            endif
	  ENDDO
        endif

c       Apply amplitude restoration filter
        if(xvptst('amplitude'))then
  	  DO I=1,NS
            mtf=sqrt((real(IM(i)))**2+(aimag(IM(i)))**2)/
     +         (psfnor*sqrt((real(OTF(i)))**2+(aimag(OTF(i)))**2+
     +         .00001))
	    RESTOR(I)=IM(i)*mtf/(mtf*mtf+SN)
	  ENDDO
          if(L.eq.1)restor(1)=cmplx(psfnor,0.0)
        endif

c       Optionally APPLY MTF OF DESIRED OUTPUT
	IF (NIDS.EQ.3) THEN
	  CALL XVREAD( IUN(3), OTF, I,' ')
	  IF (L.EQ.1) outnor = REAL(OTF(1))
	  DO I=1,NS
	    RESTOR(I)=RESTOR(I)*OTF(I)/outnor
	  ENDDO
          if(L.eq.1)restor(1)=cmplx(psfnor,0.0)
	ENDIF

	CALL XVWRIT( OUN, RESTOR, I,' ')
      ENDDO

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create wiener.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM wiener

   To Create the build file give the command:

		$ vimake wiener			(VMS)
   or
		% vimake wiener			(Unix)


************************************************************************/


#define PROGRAM	wiener
#define R2LIB

#define MODULE_LIST wiener.f

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
$ create wiener.pdf
process help=*
PARM INP (STRING,80) COUNT=(2:3)
PARM OUT (STRING,80)
PARM SN REAL DEFAULT=20.
PARM OPTION TYPE=KEYWORD VALID=("direct","wiener","ratio","amplitude") +
 DEFAULT="wiener"
END-PROC
.TITLE
VICAR1 Program WIENER  --  Image restoration using Wiener filter.

.HELP
This program modifies a fourier transform using the Wiener noise additive
model for restoration.  It was developed as part of the procedure RESTORW,
but may also be used in stand-alone mode.

.page
EXECUTION:
  WIENER  INP=(in,otf,otfout)  OUT

 where:  in is the fft of the input degraded image.

         otf is the optical transfer function. This represents the image
             degradation. It is also an fft the same size as in.

         otfout is the desired output optical transfer function.
                It is optional.

         out is the fft of the output image.

Note that WIENER has no SIZE parameter.  The output must be the same
size as the primary input, and the other input files must also be of
the same size.

.page
METHOD
There are four options:

option=wiener (the default)
WIENER applies the Wiener noise additive restoration model on a point
by point basis:

        OUT(i,j) = IN(i,j) * W(i,j)

                        OTF"(i,j)
	W(i,j) = -----------------------
                 |OTF(i,j)|**2 + 1/SN**2

        OTF is normalized by dividing by the dc term.

option=direct
Wiener applies the OTF directly to the input:

        OUT(i,j)= IN(i,j) * OTF(i,j)

        OTF is normalized by dividing by the dc term.

option=ratio
Wiener constructs a transfer function from both inputs:
In this mode OTF is a focused image and IN is a blurred image.

                        X"(i,j)
        OUT(i,j) = -----------------------
                   |X(i,j)|**2 + 1/SN**2
 
        X(i,j)= IN(i,j)/OTF(i,j)

        IN and OTF are normalized by their dc terms

        Notice that no corrected image is produced. What you get is the
        fft of the restoring kernel. You may load a small piece into FILTER
        to restore the original image. If you use all of it you'll just
        reproduce OTF.

option=amplitude
Wiener constructs a transfer function from both inputs:
In this mode OTF is a focused image and IN is a blurred image.

        OUT(i,j)= IN(i,j) * W(i,j)

                        X(i,j)
        W(i,j) = -----------------------
                 |X(i,j)|**2 + 1/SN**2
 
        X(i,j)= | IN(i,j) | / | OTF(i,j) +.00001 |

        IN and OTF are normalized by their dc terms.

        Notice that W is real and symmetric so the convolving kernel will
        also be symmetric. This function cannot correct for phase.

where IN and OUT are the fourier transforms before and after restoration
respectively, W is the Wiener filter, OTF is the optical transfer function
(the fourier transform of the point spread function), OTF" is the complex
conjugate of OTF, and SN is the signal-to-noise ratio.

The optional third input is the fourier transform of the desired point
spread function of the output, OTFD.  If this is specified, then the
restored transform produced by the above processing is multiplied by
this:

  FT2(final,i,j) = FT2(i,j) * OTFD(i,j)

Normally one desires a delta function PSF, and OTFD is not specified.
.page
HISTORY

  MAY-1985  ...LWK...  Original MIPL program, from combination of
			IBM programs RESTORE and GPFILT
  sep1990  ...lwk... removed NOPACK keyword because of replacement of
			MIPL program FT2 by FFT2AP  (code is still in
			place in case it's needed)
  JAN-1995 ...WH (CRI).. Made portable for UNIX

  Nov 1996 J Lorre  Added options
  
 Current Cognizant Programmer:  J Lorre

.LEVEL1

.vari INP
2-3 input filenames

.vari OUT
Output filename

.vari SN
Signal-to-noise ratio

.vari OPTION
program mode

.LEVEL2

.vari INP
The input files to WIENER are:

1. The fourier transform of the image to be restored, in the format
  produced by programs FFT22 or FT2.

2. The optical transfer function (OTF) of the degraded image, i.e.,
  the fourier transform of the point spread function (PSF).  This may
  be obtained using program PSF.

3. Optionally, the fourier transform of the desired output point
  spread function.  Normally one desires a delta function for this,
  and this input is not specified.

All input files must be of COMPLEX format and of the same size.

.VARIABLE OUT
The output file is the restored fourier transform.  The corresponding
image may be produced using program FFT2 in inverse mode.

The output file will be of the same size as the input.

.vari SN
This parameter specified the signal-to-noise ratio to be used in the
restoration. Noisy images should have values like 5. Clean images
should have values like 30.

.vari OPTION
The program options:
 VALID=("direct","wiener","ratio","amplitude")  DEFAULT="wiener"

option=wiener (the default)
WIENER applies the Wiener noise additive restoration model on a point
by point basis:
 
        OUT(i,j) = IN(i,j) * W(i,j)
 
                        OTF"(i,j)
        W(i,j) = -----------------------
                 |OTF(i,j)|**2 + 1/SN**2
 
        OTF is normalized by dividing by the dc term.
 
option=direct
Wiener applies the OTF directly to the input:
 
        OUT(i,j)= IN(i,j) * OTF(i,j)
 
        OTF is normalized by dividing by the dc term.
 
option=ratio
Wiener constructs a transfer function from both inputs:
In this mode OTF is a focused image and IN is a blurred image.
 
                        X"(i,j)
        OUT(i,j) = -----------------------
                   |X(i,j)|**2 + 1/SN**2
 
        X(i,j)= IN(i,j)/OTF(i,j)
 
        IN and OTF are normalized by their dc terms
 
        Notice that no corrected image is produced. What you get is the
        fft of the restoring kernel. You may load a small piece into FILTER
        to restore the original image. If you use all of it you'll just
        reproduce OTF.
 
option=amplitude
Wiener constructs a transfer function from both inputs:
In this mode OTF is a focused image and IN is a blurred image.
 
        OUT(i,j)= IN(i,j) * W(i,j)
 
                        X(i,j)
        W(i,j) = -----------------------
                 |X(i,j)|**2 + 1/SN**2
 
        X(i,j)= | IN(i,j) | / | OTF(i,j) +.00001 |
 
        IN and OTF are normalized by their dc terms.
 
        Notice that W is real and symmetric so the convolving kernel will
        also be symmetric. This function cannot correct for phase.
 
where IN and OUT are the fourier transforms before and after restoration
respectively, W is the Wiener filter, OTF is the optical transfer function
(the fourier transform of the point spread function), OTF" is the complex
conjugate of OTF, and SN is the signal-to-noise ratio.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstwiener.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! generate asymmetric point spread function
gen out=x.img nl=64 ns=64 linc=0 sinc=0
qsar inp=x.img out=y.img area=(1,1,1,7,50,1,7,3,1,50)
fft22 y.img ffty.img
!
! generate an image
qsar inp=x.img out=z.img area=(32,32,5,5,200)
fft22 z.img fftz.img
!
! blur the image
wiener inp=(fftz.img,ffty.img) out=ffts.img 'direct
fft22 inp=ffts.img out=blur.img 'inverse format=half
!xvd blur.img
!
! restore the image
wiener inp=(ffts.img,ffty.img) out=fftss.img 'wiener
fft22 inp=fftss.img out=restored.img 'inverse format=half
!xvd restored.img
!
! restoration using symmetric psf approximation.
wiener inp=(ffts.img,fftz.img) out=fftss.img 'amplitude
fft22 inp=fftss.img out=symmetricrestoration.img 'inverse format=half
!xvd symmetricrestoration.img
!
! create a restoration kernel
wiener inp=(ffts.img,fftz.img) out=fftss.img 'ratio
fft22 inp=fftss.img out=kernel.img 'inverse format=real
!xvd kernel.img
!
! deconvolve with the kernel
wiener inp=(ffts.img,fftss.img) out=ffts.img 'direct
fft22 inp=ffts.img out=restwk.img 'inverse format=half
!xvd restwk.img
!
!copy /scr/jjl/color/earth.green out=a.img sl=150 ss=150 nl=512 ns=512
!fft22 inp=a.img out=ffta.img
!gen out=x.img nl=512 ns=512 linc=0 sinc=0
!qsar inp=x.img out=y.img area=(1,1,5,5,50)
!fft22 y.img ffty.img
!wiener inp=(ffta.img,ffty.img) out=fftblur.img 'direct
!fft22 inp=fftblur.img out=blur.img 'inverse
!wiener inp=(fftblur.img,ffty.img) out=fftrest.img sn=20
!fft22 inp=fftrest.img out=rest.img 'inverse
!hist a.img
!hist blur.img
!hist rest.img
end-proc
$ Return
$!#############################################################################
