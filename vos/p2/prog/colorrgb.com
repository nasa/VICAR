$!****************************************************************************
$!
$! Build proc for MIPL module colorrgb
$! VPACK Version 1.9, Sunday, March 18, 2012, 13:26:08
$!
$! Execute by entering:		$ @colorrgb
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
$ write sys$output "*** module colorrgb ***"
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
$ write sys$output "Invalid argument given to colorrgb.com file -- ", primary
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
$   if F$SEARCH("colorrgb.imake") .nes. ""
$   then
$      vimake colorrgb
$      purge colorrgb.bld
$   else
$      if F$SEARCH("colorrgb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake colorrgb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @colorrgb.bld "STD"
$   else
$      @colorrgb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create colorrgb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack colorrgb.com -mixed -
	-s colorrgb.f -
	-i colorrgb.imake -
	-p colorrgb.pdf -
	-t tstcolorrgb.pdf tstcolorrgb.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create colorrgb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c colorrgb
      include 'VICMAIN_FOR'
      subroutine main44
      parameter (maxns=30000,maxpix=10,maxcolors=30)

      integer*4 inunit(maxpix),outunit(3),def,nl,ns,status
      integer*4 line(maxcolors),samp(maxcolors)
      real*4 buf(maxns,maxpix),obuf(maxns,3)
      real*4 red_target(maxcolors),green_target(maxcolors)
      real*4 rpar(maxcolors*5),blue_target(maxcolors)
      real*4 dn(maxpix,maxcolors)
      real*8 c(maxcolors,maxpix+1),cl(maxcolors),wts(maxcolors)
      real*8 red_coef(maxpix+1),green_coef(maxpix+1)
      real*8 blue_coef(maxpix+1),r,g,b

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      call xvparm('COLOR',rpar,npar,def,300)
      call xvparm('AREA',nlw,n,def,maxcolors)
      if(npar.gt.0)then
        ncolors=npar/5
        if(ncolors.lt.nin+2)then
          write(*,*)'Requires at least ',nin+2,' colors'
          call abend()
        endif
        if(ncolors*5.ne.npar)then
          write(*,*)'Incorrect number of color parameters'
          call abend()
        endif
        write(*,*)'Data from parameters:'
        write(*,*)' color line  sample    red    green      blue'
        k=1
        do i=1,ncolors
          line(i)=nint(rpar(k))
          k=k+1
          samp(i)=nint(rpar(k))
          k=k+1
          red_target(i)=rpar(k)
          k=k+1
          green_target(i)=rpar(k)
          k=k+1
          blue_target(i)=rpar(k)
          k=k+1
          write(*,10)i,line(i),samp(i),red_target(i),green_target(i),
     +      blue_target(i)
        enddo
10      format(3i6,3f10.2)
      else
        ncolors=0
      endif

c open all inputs
      do i=1,nin
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        if(i.eq.1)then
          call xvget(inunit(i),status,'NL',nl,'NS',ns,' ')
          if(ns.gt.maxns)then
            call xvmessage('Input image line too long',' ')
            call abend
          endif
        endif
      enddo

c open all outputs
      do i=1,3
        call xvunit(outunit(i),'OUT',i,status,' ')
        call xvopen(outunit(i),status,'O_FORMAT','BYTE',
     +    'U_FORMAT','REAL','OP','WRITE',' ')
      enddo

c read cal file
      if(ncolors.eq.0)then
        write(*,*)'Reading calibration file'
        nu=nin+1
        call read_calibration_file(red_coef,green_coef,blue_coef,nin+1)
        goto 50
      endif

c get the input dn's
      write(*,*)' color  Dn values from the images -->'
      do i=1,ncolors
        do j=1,nin
          sum=0.0
          isum=0
          do m=line(i)-nlw/2,line(i)+nlw/2
            call xvread(inunit(j),buf(1,j),status,'LINE',m,' ')
            do n=samp(i)-nlw/2,samp(i)+nlw/2
              isum=isum+1
              sum=sum+buf(n,j)
            enddo
          enddo
          dn(j,i)=sum/isum
        enddo
        write(*,11)i,(dn(j,i),j=1,nin)
      enddo
11    format(i6,11f9.1)
        
c solve for the least squares relation between input and target color.
c target_color=c1*dn1+c2*dn2+...+cn*dnn+c
      ne=ncolors
      nu=nin+1
      do i=1,ncolors
        do j=1,nin
          c(i,j)=dn(j,i)
          cl(i)=red_target(i)
          wts(i)=1.d0
        enddo
        c(i,nu)=1.d0
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,red_coef,maxcolors)
      if(ind.ne.0)then
        write(*,*)'Singular solution for red'
        stop
      endif
      do i=1,ncolors
        cl(i)=green_target(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,green_coef,maxcolors)
      if(ind.ne.0)then
        write(*,*)'Singular solution for green'
        stop
      endif
      do i=1,ncolors
        cl(i)=blue_target(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,blue_coef,maxcolors)
      if(ind.ne.0)then
        write(*,*)'Singular solution for blue'
        stop
      endif

c write results
      write(*,*)'Writing calibration file'
      call write_calibration_file(red_coef,green_coef,blue_coef,nin+1)

50    continue
c create output r g b.
      do j=1,nl
        do k=1,nin
          call xvread(inunit(k),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns
          r=red_coef(nu)
          g=green_coef(nu)
          b=blue_coef(nu)
          do k=1,nin
            r=r+red_coef(k)*buf(i,k)
            g=g+green_coef(k)*buf(i,k)
            b=b+blue_coef(k)*buf(i,k)
          enddo
          if(r.lt.0.0)r=0.0
          if(r.gt.255.)r=255.
          if(g.lt.0.0)g=0.0
          if(g.gt.255.)g=255.
          if(b.lt.0.0)b=0.0
          if(b.gt.255.)b=255.
          obuf(i,1)=r
          obuf(i,2)=g
          obuf(i,3)=b
        enddo
        do k=1,3
          call xvwrit(outunit(k),obuf(1,k),status,'LINE',j,' ')
        enddo
      enddo

      return
      end
     
c***********************************************************************
      subroutine read_calibration_file(
     + coefX,coefY,coefZ,ntable)
      real*8 coefX(ntable),coefY(ntable),coefZ(ntable)

      open(unit=10,file='color_calibration.txt',
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)then
        write(*,*)'cannot open calibration file:'
        write(*,*)'color_calibration.txt'
        stop
      endif

      read(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      read(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      read(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      close(unit=10)
      return
      end

c***********************************************************************
      subroutine write_calibration_file(
     +   coefX,coefY,coefZ,ntable)
      real*8 coefX(ntable),coefY(ntable),coefZ(ntable)

      open(unit=10,file='color_calibration.txt',
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='UNKNOWN')
      if(ios.gt.0)then
        write(*,*)'cannot open calibration file'
        stop
      endif

      write(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      write(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      write(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error on calibration file'
        stop
      endif

      close(unit=10)
      return
      end

c***********************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1,len)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      parameter (id=11)
      REAL*8  A(id,id),AL(id),R(id,id),RL(id)
      real*8 Q(id,id),X(id),SUM
      REAL*8 C(len,id),CL(len),X1(id),wts(len)

      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).eq.0.d0)goto 999
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).eq.0.d0)goto 999
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).eq.0.d0)goto 999
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      RETURN
999   ind=1
      return
      END                                                                       


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create colorrgb.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM colorrgb

   To Create the build file give the command:

		$ vimake colorrgb			(VMS)
   or
		% vimake colorrgb			(Unix)


************************************************************************/


#define PROGRAM	colorrgb
#define R2LIB

#define MODULE_LIST colorrgb.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create colorrgb.pdf
process help=*
PARM INP TYPE=STRING COUNT=(3:10)
PARM OUT TYPE=STRING COUNT=3
PARM COLOR TYPE=REAL COUNT=(0:300) DEFAULT=--
PARM AREA TYPE=INTEGER COUNT=(0:1) DEFAULT=6
END-PROC

.TITLE
VICAR program COLORRGB

.HELP
PURPOSE:
 To convert a set of N input multispectral images into an output set
         of either:
         A) 3 multispectral images representing r, g, b for quasi true
            color applications.
         B) 3 images representing X,Y,Z tristimulus for more accurate
            conversion to true color on a physical device.
Examples:

1. To convert 3 input images into quasi true color:
colorrgb in=color.red,color.grn,color.blu out=r.img,g.img,b.img \
color=22,304,173,222,255,322,429,255,213,190,47,187,239,255,255,369,188,174,185,
153,\
258,491,121,103,53

2. To process 3 other images the same way (using the identical transformation): 
colorrgb in=color2.red,color2.grn,color2.blu out=r.img,g.img,b.img     
    
METHOD:
  Colorrgb constructs three sets of simultaneous linear equations. Each set of
equations represents r,g,b respectively (or X,Y,Z). The user specifies a set
of known colors. These specify the target r,g,b (or X,Y,Z) values. Each color
becomes one equation. The line,sample locations provide the input dn values
for each color in each band. Thus the red equations look like:
r(color1)=k1*dn(band1,color1)+k2*dn(band2,color1)+...+kn*dn(bandn,color1)+k
r(color2)=k1*dn(band1,color2)+k2*dn(band2,color2)+...+kn*dn(bandn,color2)+k
..............
Colorrgb solves for k,k1-kn. These values are stored in the calibration file.

This is really nothing more than the integral for tristimulus. Kodak uses this
method (chemically) to color balance images using the four colors: sky, grass,
cloud, skin.

Note: Input images can be presented in any spectral order. (but see warning)

Warning: A calibration file must be used with input images in the same
         spectral order and number that they were presented when it was
         generated.

HISTORY:
1-1-2003 J Lorre. 
COGNIZANT PROGRAMMER:  Lucas Kamp

.LEVEL1
.VARI INP
N input images

.VARI OUT
3 byte Output images

.VARI COLOR
Color coordinates
and dn values.

.VARI AREA
Size of image patch

.LEVEL2
.VARI INP
Up to 10 multispectral registered input images.

.VARI OUT
Three byte output images in the order:
r,g,b

.VARI COLOR
(Only required to create a new color calibration file.)
 A table of color calibration values. The values come in groups of 5 in
       the order: line1,sample1,r1,g1,b1, line2,sample2,r2,g2,b2, ...
       Each group of 5 values represents a known color in the scene. There
       must be at least two more colors than input files.
       Line and sample refer to the image location of a known color whose
       output (desired) dn values will become r, g, b. You may, instead of
       r,g,b, enter the X,Y,Z tristimulus values. Then the output files
       will be in X,Y,Z units (and you should specify format=real).
       This keyword causes the calibration values to be written to a file.
       If "color" is not specified then the calibration file is read and
       these values will be used on the input files.
      
.VARI AREA
 Area specifies the dimension of a square box n by n pixels within
 which the mean dn will be computed. The boxes are located at the
 line,sample locations indicated in the "color" keyword.
 Default is 6. 
 

$ Return
$!#############################################################################
$Test_File:
$ create tstcolorrgb.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! first run creates file color_calibration.txt:
colorrgb inp=( +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_l_r.mca, +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_l_g.mca, +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_l_b.mca) +
 out=(lr.img,lg.img,lb.img) area=10 color=( +
 243,8288,154,109,93, 219,5771,139,98,80, 24,18921,204,167,151, +
 127,19244,65,41,31, 243,8288,154,109,93, 219,5771,139,98,80)

! second run uses calibration file instead of specifying
! 'color' parameter:
colorrgb inp=( +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_r_r.mca, +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_r_g.mca, +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_r_b.mca) +
 out=(rr.img,rg.img,rb.img)

! output some DNs so that future runs can check results:
list lr.img linc=200 sinc=2000
list rr.img linc=200 sinc=2000

! can view interactively, if desired (s/b stereo pair):
!xvd inp=(lr.img,lg.img,lb.img)
!xvd inp=(rr.img,rg.img,rb.img)

! with colored glasses, this anaglyph should appear as stero:
!xvd inp=(lr.img,rg.img,rb.img)

! clean up
ush rm lr.img
ush rm lg.img
ush rm lb.img
ush rm rr.img
ush rm rg.img
ush rm rb.img

! these images are of unknown provenance ... (lwk)
!colorrgb inp=(lrin.img,lgin.img,lbin.img) +
! out=(lr.img,lg.img,lb.img) area=8 color=( +
! 6,261,145,147,162, 97,473,154,109,93, 137,2146,139,98,80, +
! 342,843,204,167,151, 6,3740,197,174,180, 20,1556,65,41,31)
!colorrgb inp=(rrin.img,rgin.img,rbin.img) +
! out=(rr.img,rg.img,rb.img)

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstcolorrgb.log_solos
tstcolorrgb
colorrgb inp=(  +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_l_r.mca,  +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_l_g.mca,  +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_l_b.mca)  +
 out=(lr.img,lg.img,lb.img) area=10 color=(  +
 243,8288,154,109,93, 219,5771,139,98,80, 24,18921,204,167,151,  +
 127,19244,65,41,31, 243,8288,154,109,93, 219,5771,139,98,80)
Beginning VICAR task colorrgb
colorrgb inp=(  +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_r_r.mca,  +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_r_g.mca,  +
 /project/test_work/testdata/p2/colorrgb/pan_s1p1_scrubbed_r_b.mca)  +
 out=(rr.img,rg.img,rb.img)
Beginning VICAR task colorrgb
list lr.img linc=200 sinc=2000
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:MARSMCAU  User:rgd       Date_Time:Wed Jan 22 10:58:43 2003
 Task:COLORRGB  User:lwk       Date_Time:Fri Mar 16 17:16:06 2012
     Samp     1    4001    8001   12001   16001   20001
   Line
      1     174  20 137 146 137 165 152 146 135  89 113
    201     146 155 120 132 137 179 145 179  73 117 169
    401       1 129 142 151 129 170 177 148 152   0 125
    601     158 150 143 142 136 130 180 126 119 145 185
    801     162 158 156 157 128 185 189 163 135 164 198
   1001     146 176 148 146 140 158  85 140 141 156 171
   1201     162 111 154 171 142 137 146 174 125 204 159
   1401     137 159 142 133 138 143 141 160 171 113 178
   1601     140 161 153 139 136 145 172 171 154 255  92
   1801     165 159 129 175 143 130 168 123 177 195  88
   2001     104 185  71 148  92 139 176 139 145 219  91
   2201      94 149 103 161 149 136 178 153  32 215 111
   2401      79 165 164 166 141 166 176 141 135   6 140
list rr.img linc=200 sinc=2000
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:MARSMCAU  User:rgd       Date_Time:Wed Jan 22 11:00:29 2003
 Task:COLORRGB  User:lwk       Date_Time:Fri Mar 16 17:17:04 2012
     Samp     1    4001    8001   12001   16001   20001
   Line
      1     181  11 178 161 133 173 157  38 140 114 132
    201     143 164 159 147 192 184 110 142 120 126 172
    401     152 132 133 159 178 161 139 135  80 155 173
    601     151 154 183 190 157 139 185 165 185 176 187
    801     152 168 147 125 180 144 141 156 178 149 145
   1001     196 189 143 156 170 148 148 166 152 177 164
   1201     186 161 166 149 153 118 176 141 164 153 167
   1401     181 147 125 134 165 173  69 142 196 107  69
   1601     153 153 167 151 141 142 170 158 187 127 125
   1801     158 159  99 101 138 169 215 140 204 132 119
   2001     121 150 122  84 154 145  95 152  82 122  95
   2201     131 129 162 139 165 178 158 127  25   0 102
   2401     144 168 150 146 175 149  16 167  22  63  76

ush rm lr.img
ush rm lg.img
ush rm lb.img
ush rm rr.img
ush rm rg.img
ush rm rb.img

let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
