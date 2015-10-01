$!****************************************************************************
$!
$! Build proc for MIPL module apodize
$! VPACK Version 1.9, Friday, October 19, 2012, 12:14:45
$!
$! Execute by entering:		$ @apodize
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
$ write sys$output "*** module apodize ***"
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
$ write sys$output "Invalid argument given to apodize.com file -- ", primary
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
$   if F$SEARCH("apodize.imake") .nes. ""
$   then
$      vimake apodize
$      purge apodize.bld
$   else
$      if F$SEARCH("apodize.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake apodize
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @apodize.bld "STD"
$   else
$      @apodize.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create apodize.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack apodize.com -mixed -
	-s apodize.f -
	-i apodize.imake -
	-p apodize.pdf -
	-t tstapodize.pdf tstapodize.log_solos tstapodize.log_linux -
	   tstapodize.log_rjb
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create apodize.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 27 SEPT 1983
C     JHR: CONVERTED TO VICAR2   1 JULY 1985
C     AS(CRI): MSTP S/W CONVERSION (VICAR PORTING) 1 JULY 1994
C     ENABLED 3D IMAGE CAPABILITY (N TOOLE), 16 SEPT 2003
C
C     PUTS A SINE WAVE AROUND PICTURE EDGES TO STOP FFT2 LEAKAGE.
C     KEYWORD FOR THE EDGE IS 'EDGE', DEFAULT VALUE IS 10.
C
	implicit none
      	real*4 r1(8192),r2(8192)
      	integer*4 ounit,stat,ss,sl,sb,nbo,band,bandout,lineout,nbi
	integer*4 iunit,npix,icode,nlo,nso,nli,nsi,icount,idef
	integer*4 ii,j,l,m,line,ln
c      	integer*2 HBUF(8192)
	real*4 hbuf(8192)
	real*4  p,a,rat,diff,diff1,rr,rl
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      	character*8 format

      	CHARACTER*3 ORGIN
C
      COMMON/C1/R1,R2,HBUF
C
C        SET DEFAULTS AND INITIALIZE
      	npix=10
C
      call ifmessage('APODIZE - 28-Jun-2012')
      CALL XVEACTION('SA',' ')
C          OPEN INPUT DATA SET
      call xvunit(iunit,'INP',1,stat,' ')
      call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C        GET DATA FORMAT AND CHECK
      call xvget(iunit,stat,'FORMAT',FORMAT,'ORG',orgin,' ')

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
	call xvclose(iunit,stat,' ')
	call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')		!FMT(INCODE),' ')

c     Check organization of image, prohibit BIP
      if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

C
C        GET SIZE INFORMATION AND CHECK
      call xvsize(sl,ss,nlo,nso,nli,nsi)
      if(sl+nlo-1 .gt. nli) then
         call xvmessage
     &    ('??E - Number of lines requested exceeds input size',' ')
         call abend
      endif
      if(ss+nso-1 .gt. nsi) then
         call xvmessage
     &    ('??E - Number of samples requested exceeds input size',' ')
         call abend
      endif
      if(nso .gt. 8192) then
         call xvmessage
     &    ('??E - Number of samples exceeds buffer size of 8192',' ')
         call abend
      endif

      call xvbands(sb,nbo,nbi)

      if (sb.gt.nbi) call mabend(
     & '??E - SB is greater than the total number of bands')
                 
      if ( sb + nbo - 1 .gt. nbi) then
         call xvmessage('***Number of bands truncated', ' ')
         nbo = nbi + 1 - sb     
      endif

C
C        OPEN OUTPUT DATA SET
      call xvunit(ounit,'OUT',1,stat,' ')
      call xvopen(ounit,stat,'OP','WRITE','O_FORMAT',fmt(icode),
     & 'U_FORMAT',fmt(4),'U_NL',nlo,'U_NS',nso,'U_NB',nbo,' ')
C
C           PROCESS PARAMETERS
C        'EDGE'
      call xvparm('EDGE',npix,icount,idef,1)
C      
C        SETUP FOR MAIN PROCESSING
      p=npix*2
      rat=3.14159/p
      ii=npix*2
C
C        PROCESS LEFT AND RIGHT BORDERS
      bandout = 0
      do 35 band = sb,nbo+sb-1
        bandout = bandout + 1
        lineout = 0
        do 40 l=1,nlo
          line=l+sl-1
          lineout = lineout + 1
          call xvread(iunit,hbuf,stat,'LINE',line,'BAND',
     &            band,'SAMP',ss,'NSAMPS',nso,' ')
          rr=hbuf(npix)
          rl=hbuf(nso-npix+1)
          diff1=rl-rr
          diff=abs(diff1)/2.0
          a=1.0
          if (diff.gt.1.0e-20) a=(-diff1)/(abs(diff1))
          do 50 m=1,ii
            ln=m-npix
            if (ln.le.0) then
              hbuf(nso+ln)=diff*sin(rat*ln*a)+amin1(rl,rr)+diff+0.5
            else
              hbuf(ln)=diff*sin(rat*ln*a)+amin1(rl,rr)+diff+0.5
            endif
50        continue
          call xvwrit(ounit,hbuf,stat,'NSAMPS',nso,'LINE',lineout,
     +           'BAND',bandout,' ')
40      continue
35    continue
C
C        RE-OPEN OUTPUT FOR UPDATE
      call xvclose(ounit,stat,' ')
      call xvopen(ounit,stat,'OP','UPDATE','O_FORMAT',fmt(icode),
     & 'U_FORMAT', fmt(4), 'U_NL',nlo,'U_NS',nso,'U_NB',nbo,' ')
C
C        PROCESS TOP AND BOTTOM BORDERS
      do 5 band=1,nbo
         line=npix 
         call xvread(ounit,hbuf,stat,'LINE',line,'BAND',band,
     &            'SAMP',ss,'NSAMPS',nso,' ')
         do 71 j=1,nso
            r1(j)=hbuf(j)
71       continue
         line=nlo-npix+1
         call xvread(ounit,hbuf,stat,'LINE',line,'BAND',band,
     &            'SAMP',ss,'NSAMPS',nso,' ')
         do 70 j=1,nso
            r2(j)=hbuf(j)
70       continue
         do 10 l=1,ii
            ln=npix-l
            do 20 m=1,nso
               diff1=r2(m)-r1(m)
               diff=abs(diff1)/2.0
               a=1.0
               if (diff.gt.1.0e-20) a=diff1/abs(diff1)
               hbuf(m)=diff*sin(rat*ln*a)+amin1(r2(m),r1(m))+diff+0.5
20          continue
            if (ln.ge.0) then
               line=nlo-ln
            else
               line=iabs(ln)
            endif
            call xvwrit(ounit,hbuf,stat,'LINE',line,'NSAMPS',nso,
     +         'BAND',band,' ')
10       continue
5     continue
C
C        CLOSE DATA SETS
      call xvclose(iunit,stat,' ')
      call xvclose(ounit,stat,' ')
C
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create apodize.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM apodize

   To Create the build file give the command:

		$ vimake apodize			(VMS)
   or
		% vimake apodize			(Unix)


************************************************************************/


#define PROGRAM	apodize
#define R2LIB

#define MODULE_LIST apodize.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create apodize.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM SB TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM NB TYPE=INTEGER DEFAULT=0
PARM EDGE TYPE=INTEGER DEFAULT=10
END-PROC
.TITLE
"APODIZE"
.HELP
PURPOSE:
"apodize" modifies picture borders so that the Fourier transform of the
picture will be free of spikes through the zero-frequency axis, allowing
filters to operate without ringing on the picture peripheries.

 
EXECUTION:

Examples

apodize IN OUT

This command will perform the operation on input image IN, sending the output
to image OUT.  The input image is assumed to be in byte format; a ten-pixel
border will be used. 

apodize IN OUT EDGE=3

This command is similar, but a three-pixel border is used.
.page
OPERATION:
The Fourier transform considers the input picture to really be an infinite
mosaic (checkerboard) of identical pictures.  If the picture borders do not
match but cause abrupt DN displacement, this information is included in the
transform as a broad response on a line through DC both vertically and
horizontally.  When restoration filters are applied, this information (which
has not partaken of the image degradation in question) is altered.  The inverse
Fourier transform no longer contains the information needed to represent a
clean picture border and ringing is the result. 

"apodize" fits a sine wave across the picture border so that abrupt transitions
in the mosaic become continuous.  The EDGE parameter controls the wavelength
such that lambda = 4 * EDGE.  All information within the border strip is lost. 

.page
RESTRICTIONS

Max image size is 8192 in samples

.page
HISTORY

WRITTEN BY:  J. J. Lorre, 20 May 1974
COGNIZANT PROGRAMMER:  R. J. Bambery

REVISIONS:  

    1 July 1994     A. Scop (CRI)   Made portable for UNIX
    16 Sept 2003    N. Toole        Enabled for 3D images
    19 Apr 2011     R. J. Bambery   Made internal format REAL
                                    Now works with BYTE, HALF, FULL, REAL    
    28 Jun 2012     R. J. Bambery   Removed <tab> in front of continuation
                                    lines to make backward compatible with
                                    32-bit Linux gfortran 4.2.1, otherwise
                                    compatible 64-bit Linux gfortran 4.6.3
    18 Oct 2012     L.W.Kamp  Trimmed some lines to make <72 characters for Solaris
.LEVEL1
.VARIABLE INP
STRING - Input image file
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Standard VICAR size field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE SB
INTEGER - Starting band
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE NB
INTEGER - Number of bands
.VARIABLE EDGE
INTEGER - Border width
.LEVEL2
.VARIABLE EDGE
EDGE is an integer specifying the pixel-width of the border within the picture
the user wishes to modify. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstapodize.pdf
procedure
local   maxval  real    count=1
local   minval  real    count=1
local   imaxval int    count=1
local   iminval int    count=1
local   afidsroot   type=string count=1

refgbl $autousage
refgbl $echo
! Jun 20, 2012 - RJB
! TEST SCRIPT FOR APODIZE
! replaces test script of 2003 which really didn't illustrate anything
!
! Vicar Programs:
!       gen f2 cform rotate (rotate2, lgeom) copy ccomp  maxmin 
! Optional: xvd
! Cartlab:  fft2005
! MIPL:     fft22 fftflip 
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
!
! Uses cartlab or mipl dependent pointers
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
!
!f2 (F8_fft.amp,F8_fft.pha) F8_fft.abs func="sqrt(in1**2+in2**2)"
!===========
! Gen a large image 700x700 sinusoid pattern but to yield 400x400 rotated
! sinusoid repeats each 8 pixels
gen mask87.real nl=700 ns=700 format=real sinc=1.0 linc=0 modulo=8
f2 mask87.real out=F87.real format=real function="sin(in1)"
cform F87.real F87H  irange=(-1.0,1.0) orange=(0,255) oform=half
rotate F87H F87N angle=25 center=(350,350) 'nointerp
rotate F87H F87I angle=25 center=(350,350)
!no-interp
copy F87N F87N.rot size=(1,1,400,400)           !size=(150,150,400,400)
! Do fft on rotated no interpolation
!NO APODIZATION 
if (afidsroot = "")
!MIPL
    fft22 F87N.rot xxA mode=forward format=comp 
    fftflip xxA F87N_fft 'flip
else
!CARTLAB
    fft2005 F87N.rot F87N_fft mode=forward format=comp 'quadswap
end-if
ccomp F87N_fft (F87N_fft.amp,F87N_fft.pha) trans=polar
maxmin F87N_fft.amp minival=minval maxival=maxval
let $echo="no"
let imaxval = $fix(maxval)
let iminval = $fix(minval)
write "NO APODIZATION on NO-INTERPOLATION"
write "F87N_fft.amp range = &iminval to &imaxval"
let $echo="yes"
cform F87N_fft.amp F87N_fft.full irange=(&maxval,&minval) orange=(&imaxval,&iminval) +
   oform=full
cform F87N_fft.full F87N_fft.norm irange=(0,35000) orange=(0,255)
!cform F87N_fft.full F87NH_fft.norm irange=(0,10000) orange=(0,255) oform=half

!interp
copy F87I F87I.rot size=(1,1,400,400)           !size=(150,150,400,400)
! F87N_fft.norm is noisier than F87I_fft.norm
! Do fft on rotated with interpolation
if (afidsroot = "")
!MIPL
    fft22 F87I.rot xxA mode=forward format=comp
    fftflip xxA F87I_fft 'flip
else
!CARTLAb
    fft2005 F87I.rot F87I_fft mode=forward format=comp 'quadswap
end-if
ccomp F87I_fft (F87I_fft.amp,F87I_fft.pha) trans=polar
maxmin F87I_fft.amp minival=minval maxival=maxval
let $echo="no"
let imaxval = $fix(maxval)
let iminval = $fix(minval)
write "NO APODIZATION on INTERPOLATION"
write "F87I_fft.amp range = &iminval to &imaxval"
let $echo="yes"
cform F87I_fft.amp F87I_fft.full irange=(&maxval,&minval) orange=(&imaxval,&iminval) +
   oform=full
cform F87I_fft.full F87I_fft.norm irange=(0,35000) orange=(0,255)
!cform F87I_fft.full F87IH_fft.norm irange=(0,10000) orange=(0,255) oform=half

! Now apodize 3x3 on non-interpolated rotated
!
apodize F87N.rot F87N33.rot edge=3
if (afidsroot = "")
!MIPL
    fft22 F87N33.rot xxA mode=forward format=comp
    fftflip xxA F87N33_fft 'flip
else
!CARTLAB
    fft2005 F87N33.rot F87N33_fft mode=forward format=comp 'quadswap
end-if
ccomp F87N33_fft (F87N33_fft.amp,F87N33_fft.pha) trans=polar
maxmin F87N33_fft.amp minival=minval maxival=maxval
let $echo="no"
let imaxval = $fix(maxval)
let iminval = $fix(minval)
write "3x3 APODIZATION on NON-INTERPOLATION"
write "F87N33_fft.amp range = &iminval to &imaxval"
let $echo="yes"

cform F87N33_fft.amp F87N33_fft.full irange=(&maxval,&minval) orange=(&imaxval,&iminval) +
   oform=full
cform F87N33_fft.full F87N33_fft.norm irange=(0,35000) orange=(0,255)

! Now apodize 5x5 on non-interpolated
apodize F87N.rot F87N55.rot edge=5
if (afidsroot = "")
!MIPL
    fft22 F87N55.rot xxA mode=forward format=comp
    fftflip xxA F87N55_fft 'flip
else
!CARTLAB
    fft2005 F87N55.rot F87N55_fft mode=forward format=comp 'quadswap
end-if
ccomp F87N55_fft (F87N55_fft.amp,F87N55_fft.pha) trans=polar
maxmin F87N55_fft.amp minival=minval maxival=maxval
let $echo="no"
let imaxval = $fix(maxval)
let iminval = $fix(minval)
write "5x5 APODIZATION on NON-INTERPOLATION"
write "F87N55_fft.amp range = &iminval to &imaxval"
let $echo="yes"

cform F87N55_fft.amp F87N55_fft.full irange=(&maxval,&minval) orange=(&imaxval,&iminval) +
   oform=full
cform F87N55_fft.full F87N55_fft.norm irange=(0,35000) orange=(0,255)
! now display F87N_fft.norm    F87N33_fft.norm   and   F87N55_fft.norm side by side
! see how power thru  DC (row 201) is reduced from non-apodized to 5x5 apodized

! Now apodize 3x3 on interpolated rotated
!
apodize F87I.rot F87I33.rot edge=3
if (afidsroot = "")
!MIPL
    fft22 F87I33.rot xxA mode=forward format=comp
    fftflip xxA F87I33_fft 'flip
else
!CARTLAB
    fft2005 F87I33.rot F87I33_fft mode=forward format=comp 'quadswap
end-if
ccomp F87I33_fft (F87I33_fft.amp,F87I33_fft.pha) trans=polar
maxmin F87I33_fft.amp minival=minval maxival=maxval
let $echo="no"
let imaxval = $fix(maxval)
let iminval = $fix(minval)
write "3x3 APODIZATION on INTERPOLATION"
write "F87I33_fft.amp range = &iminval to &imaxval"
let $echo="yes"

cform F87I33_fft.amp F87I33_fft.full irange=(&maxval,&minval) orange=(&imaxval,&iminval) +
   oform=full
cform F87I33_fft.full F87I33_fft.norm irange=(0,35000) orange=(0,255)

! Now apodize 5x5 on interpolated
apodize F87I.rot F87I55.rot edge=5
if (afidsroot = "")
!MIPL
    fft22 F87I55.rot xxA mode=forward format=comp
    fftflip xxA F87I55_fft 'flip
else
!CARTLAB
    fft2005 F87I55.rot F87I55_fft mode=forward format=comp 'quadswap
end-if
ccomp F87I55_fft (F87I55_fft.amp,F87I55_fft.pha) trans=polar
maxmin F87I55_fft.amp minival=minval maxival=maxval
let $echo="no"
let imaxval = $fix(maxval)
let iminval = $fix(minval)
write "5x5 APODIZATION on INTERPOLATION"
write "F87I55_fft.amp range = &iminval to &imaxval"
let $echo="yes"

cform F87I55_fft.amp F87I55_fft.full irange=(&maxval,&minval) orange=(&imaxval,&iminval) +
   oform=full
cform F87I55_fft.full F87I55_fft.norm irange=(0,35000) orange=(0,255)
! now display F87I_fft.norm    F87I33_fft.norm   and   F87I55_fft.norm side by side
! see how power thru  DC (row 201) is reduced from non-apodized to 5x5 apodized
! using xvd
! xvd F87I_fft.norm
! xvd F87I33_fft.norm
! xvd F87I55_fft.norm
!
let $echo="no"

! clean up
ush rm -f F87*fft*
ush rm -f F87?
ush rm -f F87.real
ush rm -f F87*.rot
ush rm -f mask87.real
ush rm -f ZZPAR
ush rm -f xxA

end-proc
$!-----------------------------------------------------------------------------
$ create tstapodize.log_solos
tstapodize
translog INP=AFIDS_ROOT TRANS=afidsroot
gen mask87.real nl=700 ns=700 format=real sinc=1.0 linc=0 modulo=8
Beginning VICAR task gen
GEN Version 6
GEN task completed
f2 mask87.real out=F87.real format=real function="sin(in1)"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 490000 TIMES
cform F87.real F87H  irange=(-1.0,1.0) orange=(0,255) oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *   127.500+   127.500
INPUT FORMAT = REAL
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
rotate F87H F87N angle=25 center=(350,350) 'nointerp
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
REGION (   1,    1,  700,  700) OF THE INPUT PICTURE IS ROTATED    25.0 DEGREES ABOUT 350.5 , 350.5
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL      350.0,   350.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=F87H OUT=F87N SIZE=@SIZE NL=@NL NS=@NS  +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
rotate F87H F87I angle=25 center=(350,350)
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
REGION (   1,    1,  700,  700) OF THE INPUT PICTURE IS ROTATED    25.0 DEGREES ABOUT 350.5 , 350.5
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL      350.0,   350.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=F87H OUT=F87I SIZE=@SIZE NL=@NL NS=@NS  +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
copy F87N F87N.rot size=(1,1,400,400)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
if (afidsroot = "")
    fft22 F87N.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87N_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N_fft (F87N_fft.amp,F87N_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     2,   398)
Max. value: 3.274100E+04   at  (    11,   252)

let $echo="no"
NO APODIZATION on NO-INTERPOLATION
F87N_fft.amp range = 0 to 32741
cform F87N_fft.amp F87N_fft.full irange=(3.274100000000e+04,0.000000000000e+00) orange+
=(32741,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N_fft.full F87N_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
copy F87I F87I.rot size=(1,1,400,400)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
if (afidsroot = "")
    fft22 F87I.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87I_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I_fft (F87I_fft.amp,F87I_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     2,    13)
Max. value: 3.274000E+04   at  (   223,   247)

let $echo="no"
NO APODIZATION on INTERPOLATION
F87I_fft.amp range = 0 to 32740
cform F87I_fft.amp F87I_fft.full irange=(3.274000000000e+04,0.000000000000e+00) orange+
=(32740,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I_fft.full F87I_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87N.rot F87N33.rot edge=3
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87N33.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87N33_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N33_fft (F87N33_fft.amp,F87N33_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N33_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,    32)
Max. value: 3.273200E+04   at  (    11,   252)

let $echo="no"
3x3 APODIZATION on NON-INTERPOLATION
F87N33_fft.amp range = 0 to 32732
cform F87N33_fft.amp F87N33_fft.full irange=(3.273200000000e+04,0.000000000000e+00) orange+
=(32732,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N33_fft.full F87N33_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87N.rot F87N55.rot edge=5
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87N55.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87N55_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N55_fft (F87N55_fft.amp,F87N55_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N55_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     9,   178)
Max. value: 3.274800E+04   at  (   179,   148)

let $echo="no"
5x5 APODIZATION on NON-INTERPOLATION
F87N55_fft.amp range = 0 to 32748
cform F87N55_fft.amp F87N55_fft.full irange=(3.274800000000e+04,0.000000000000e+00) orange+
=(32748,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N55_fft.full F87N55_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87I.rot F87I33.rot edge=3
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87I33.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87I33_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I33_fft (F87I33_fft.amp,F87I33_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I33_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,   104)
Max. value: 3.271200E+04   at  (   170,   135)

let $echo="no"
3x3 APODIZATION on INTERPOLATION
F87I33_fft.amp range = 0 to 32712
cform F87I33_fft.amp F87I33_fft.full irange=(3.271200000000e+04,0.000000000000e+00) orange+
=(32712,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I33_fft.full F87I33_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87I.rot F87I55.rot edge=5
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87I55.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87I55_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I55_fft (F87I55_fft.amp,F87I55_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I55_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,    40)
Max. value: 3.276500E+04   at  (   153,   111)

let $echo="no"
5x5 APODIZATION on INTERPOLATION
F87I55_fft.amp range = 0 to 32765
cform F87I55_fft.amp F87I55_fft.full irange=(3.276500000000e+04,0.000000000000e+00) orange+
=(32765,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I55_fft.full F87I55_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstapodize.log_linux
tstapodize
translog INP=AFIDS_ROOT TRANS=afidsroot
gen mask87.real nl=700 ns=700 format=real sinc=1.0 linc=0 modulo=8
Beginning VICAR task gen
GEN Version 6
GEN task completed
f2 mask87.real out=F87.real format=real function="sin(in1)"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 490000 TIMES
cform F87.real F87H  irange=(-1.0,1.0) orange=(0,255) oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *   127.500+   127.500
INPUT FORMAT = REAL
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
rotate F87H F87N angle=25 center=(350,350) 'nointerp
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
REGION (   1,    1,  700,  700) OF THE INPUT PICTURE IS ROTATED    25.0 DEGREES ABOUT 350.5 , 350.5
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL      350.0,   350.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=F87H OUT=F87N SIZE=@SIZE NL=@NL NS=@NS  +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
rotate F87H F87I angle=25 center=(350,350)
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
REGION (   1,    1,  700,  700) OF THE INPUT PICTURE IS ROTATED    25.0 DEGREES ABOUT 350.5 , 350.5
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL      350.0,   350.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=F87H OUT=F87I SIZE=@SIZE NL=@NL NS=@NS  +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
copy F87N F87N.rot size=(1,1,400,400)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
if (afidsroot = "")
    fft22 F87N.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87N_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N_fft (F87N_fft.amp,F87N_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     7,   167)
Max. value: 3.276800E+04   at  (     2,   236)

let $echo="no"
NO APODIZATION on NO-INTERPOLATION
F87N_fft.amp range = 0 to 32768
cform F87N_fft.amp F87N_fft.full irange=(3.276800000000e+04,0.000000000000e+00) orange+
=(32768,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N_fft.full F87N_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
copy F87I F87I.rot size=(1,1,400,400)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
if (afidsroot = "")
    fft22 F87I.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87I_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I_fft (F87I_fft.amp,F87I_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     2,   272)
Max. value: 3.276800E+04   at  (    53,   283)

let $echo="no"
NO APODIZATION on INTERPOLATION
F87I_fft.amp range = 0 to 32768
cform F87I_fft.amp F87I_fft.full irange=(3.276800000000e+04,0.000000000000e+00) orange+
=(32768,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I_fft.full F87I_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87N.rot F87N33.rot edge=3
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87N33.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87N33_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N33_fft (F87N33_fft.amp,F87N33_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N33_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     5,    26)
Max. value: 3.276800E+04   at  (     2,   236)

let $echo="no"
3x3 APODIZATION on NON-INTERPOLATION
F87N33_fft.amp range = 0 to 32768
cform F87N33_fft.amp F87N33_fft.full irange=(3.276800000000e+04,0.000000000000e+00) orange+
=(32768,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N33_fft.full F87N33_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87N.rot F87N55.rot edge=5
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87N55.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87N55_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N55_fft (F87N55_fft.amp,F87N55_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N55_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,    83)
Max. value: 3.276800E+04   at  (     2,   236)

let $echo="no"
5x5 APODIZATION on NON-INTERPOLATION
F87N55_fft.amp range = 0 to 32768
cform F87N55_fft.amp F87N55_fft.full irange=(3.276800000000e+04,0.000000000000e+00) orange+
=(32768,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N55_fft.full F87N55_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87I.rot F87I33.rot edge=3
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87I33.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87I33_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I33_fft (F87I33_fft.amp,F87I33_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I33_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,    11)
Max. value: 3.276800E+04   at  (    53,   283)

let $echo="no"
3x3 APODIZATION on INTERPOLATION
F87I33_fft.amp range = 0 to 32768
cform F87I33_fft.amp F87I33_fft.full irange=(3.276800000000e+04,0.000000000000e+00) orange+
=(32768,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I33_fft.full F87I33_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87I.rot F87I55.rot edge=5
Beginning VICAR task apodize
APODIZE - 28-Jun-2012
if (afidsroot = "")
    fft22 F87I55.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.lwk
TRANSFORM COMPLETED
    fftflip xxA F87I55_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I55_fft (F87I55_fft.amp,F87I55_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I55_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     2,   370)
Max. value: 3.276800E+04   at  (    53,   283)

let $echo="no"
5x5 APODIZATION on INTERPOLATION
F87I55_fft.amp range = 0 to 32768
cform F87I55_fft.amp F87I55_fft.full irange=(3.276800000000e+04,0.000000000000e+00) orange+
=(32768,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I55_fft.full F87I55_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstapodize.log_rjb
tstapodize
translog INP=AFIDS_ROOT TRANS=afidsroot
gen mask87.real nl=700 ns=700 format=real sinc=1.0 linc=0 modulo=8
Beginning VICAR task gen
GEN Version 6
GEN task completed
f2 mask87.real out=F87.real format=real function="sin(in1)"
Beginning VICAR task f2
F2 version 24-Jun-2011 (64-bit) - RJB
F2 calculating every pixel
FUNCTION EVALUATED 490000 TIMES
cform F87.real F87H  irange=(-1.0,1.0) orange=(0,255) oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *   127.500+   127.500
INPUT FORMAT = REAL
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
rotate F87H F87N angle=25 center=(350,350) 'nointerp
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
ROTATE2 - 20-Apr-2011 (64-bit) - RJB
REGION (   1,    1,  700,  700) OF THE INPUT PICTURE IS ROTATED    25.0 DEGREES ABOUT 350.5 , 350.5
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL      350.0,   350.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=F87H OUT=F87N SIZE=@SIZE NL=@NL NS=@NS  +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
rotate F87H F87I angle=25 center=(350,350)
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
ROTATE2 - 20-Apr-2011 (64-bit) - RJB
REGION (   1,    1,  700,  700) OF THE INPUT PICTURE IS ROTATED    25.0 DEGREES ABOUT 350.5 , 350.5
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL      350.0,   350.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=F87H OUT=F87I SIZE=@SIZE NL=@NL NS=@NS  +
        IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
copy F87N F87N.rot size=(1,1,400,400)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
if (afidsroot = "")
    fft22 F87N.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.rjb
TRANSFORM COMPLETED
    fftflip xxA F87N_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N_fft (F87N_fft.amp,F87N_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 12/04/2010 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     2,   398)
Max. value: 3.274100E+04   at  (    11,   252)

let $echo="no"
NO APODIZATION on NO-INTERPOLATION
F87N_fft.amp range = 0 to 32741
cform F87N_fft.amp F87N_fft.full irange=(3.274100000000e+04,0.000000000000e+00) orange+
=(32741,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N_fft.full F87N_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
copy F87I F87I.rot size=(1,1,400,400)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
if (afidsroot = "")
    fft22 F87I.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.rjb
TRANSFORM COMPLETED
    fftflip xxA F87I_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I_fft (F87I_fft.amp,F87I_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 12/04/2010 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     2,    13)
Max. value: 3.274000E+04   at  (   223,   247)

let $echo="no"
NO APODIZATION on INTERPOLATION
F87I_fft.amp range = 0 to 32740
cform F87I_fft.amp F87I_fft.full irange=(3.274000000000e+04,0.000000000000e+00) orange+
=(32740,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I_fft.full F87I_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87N.rot F87N33.rot edge=3
Beginning VICAR task apodize
APODIZE - 28-Jun-2012 - (64bit) - RJB
if (afidsroot = "")
    fft22 F87N33.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.rjb
TRANSFORM COMPLETED
    fftflip xxA F87N33_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N33_fft (F87N33_fft.amp,F87N33_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N33_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 12/04/2010 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,    32)
Max. value: 3.273200E+04   at  (    11,   252)

let $echo="no"
3x3 APODIZATION on NON-INTERPOLATION
F87N33_fft.amp range = 0 to 32732
cform F87N33_fft.amp F87N33_fft.full irange=(3.273200000000e+04,0.000000000000e+00) orange+
=(32732,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N33_fft.full F87N33_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87N.rot F87N55.rot edge=5
Beginning VICAR task apodize
APODIZE - 28-Jun-2012 - (64bit) - RJB
if (afidsroot = "")
    fft22 F87N55.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.rjb
TRANSFORM COMPLETED
    fftflip xxA F87N55_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87N55_fft (F87N55_fft.amp,F87N55_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87N55_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 12/04/2010 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     9,   178)
Max. value: 3.274800E+04   at  (   179,   148)

let $echo="no"
5x5 APODIZATION on NON-INTERPOLATION
F87N55_fft.amp range = 0 to 32748
cform F87N55_fft.amp F87N55_fft.full irange=(3.274800000000e+04,0.000000000000e+00) orange+
=(32748,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87N55_fft.full F87N55_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87I.rot F87I33.rot edge=3
Beginning VICAR task apodize
APODIZE - 28-Jun-2012 - (64bit) - RJB
if (afidsroot = "")
    fft22 F87I33.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.rjb
TRANSFORM COMPLETED
    fftflip xxA F87I33_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I33_fft (F87I33_fft.amp,F87I33_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I33_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 12/04/2010 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,   104)
Max. value: 3.271200E+04   at  (   170,   135)

let $echo="no"
3x3 APODIZATION on INTERPOLATION
F87I33_fft.amp range = 0 to 32712
cform F87I33_fft.amp F87I33_fft.full irange=(3.271200000000e+04,0.000000000000e+00) orange+
=(32712,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I33_fft.full F87I33_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
apodize F87I.rot F87I55.rot edge=5
Beginning VICAR task apodize
APODIZE - 28-Jun-2012 - (64bit) - RJB
if (afidsroot = "")
    fft22 F87I55.rot xxA mode=forward format=comp
Beginning VICAR task fft22
FFT22 version 1-JUL-94
FORWARD TRANSFORM
USING SCRATCH FILE: /tmp/fft2scrx.rjb
TRANSFORM COMPLETED
    fftflip xxA F87I55_fft 'flip
Beginning VICAR task fftflip
FFTFLIP version 5-SEP-94
else
end-if
ccomp F87I55_fft (F87I55_fft.amp,F87I55_fft.pha) trans=polar
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
maxmin F87I55_fft.amp minival=minval maxival=maxval
Beginning VICAR task maxmin
*** maxmin - 12/04/2010 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,    40)
Max. value: 3.276500E+04   at  (   153,   111)

let $echo="no"
5x5 APODIZATION on INTERPOLATION
F87I55_fft.amp range = 0 to 32765
cform F87I55_fft.amp F87I55_fft.full irange=(3.276500000000e+04,0.000000000000e+00) orange+
=(32765,0)     oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = REAL
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
cform F87I55_fft.full F87I55_fft.norm irange=(0,35000) orange=(0,255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     0.007+     0.000
INPUT FORMAT = FULL
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
