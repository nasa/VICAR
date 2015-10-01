$!****************************************************************************
$!
$! Build proc for MIPL module clean
$! VPACK Version 1.9, Wednesday, January 06, 1999, 16:47:03
$!
$! Execute by entering:		$ @clean
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
$ write sys$output "*** module clean ***"
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
$ write sys$output "Invalid argument given to clean.com file -- ", primary
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
$   if F$SEARCH("clean.imake") .nes. ""
$   then
$      vimake clean
$      purge clean.bld
$   else
$      if F$SEARCH("clean.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake clean
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @clean.bld "STD"
$   else
$      @clean.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create clean.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack clean.com -
	-s clean.f -
	-i clean.imake -
	-p clean.pdf -
	-t tstclean.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create clean.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program clean
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maximage=1000000, maxpsf=10000)
      real*4 pixbuf(maximage),psf(maxpsf),scr(maximage),noise,gain
      integer*4 inunit1,inunit2,ounit1,ounit2,def,count,status
      integer*4 ounit3
      integer*2 hist(maximage),maxh
      logical xvptst,convolve,total

c parameters
c      call xvpcnt('INP',nids)      
      convolve=xvptst('CONVOLVE')
      total=xvptst('TOTAL')
      call xvparm('GAIN',gain,count,def,1)
      call xvparm('NOISE',noise,count,def,1)
      call xvparm('ITER',maxiter,count,def,1)
      call xvparm('MAXMOD',maxmod,count,def,1)
      call xvparm('CHANGE',change,count,def,1)
      if(total)then
        call xvmessage('Mode is TOTAL',' ')
      else
        call xvmessage('Mode is POSITIVE',' ')
      endif

c read image into memory
      call xvunit(inunit1,'INP',1,status,' ')
      call xvopen(inunit1,status,'U_FORMAT','REAL',' ')
      call xvget(inunit1,status,'NL',nl,'NS',ns,' ')
      if(ns*nl.gt.maximage)then
          call xvmessage(' NS*NS of image storage buffer too small',' ')
          call abend
      endif
      k=1
      do j=1,nl                    ! line loop
          call xvread(inunit1,pixbuf(k),status,'LINE',j,' ')
          k=k+ns
      enddo

c read psf into memory
      call xvunit(inunit2,'INP',2,status,' ')
      call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
      call xvget(inunit2,status,'NL',nl2,'NS',ns2,' ')
      if(ns2*nl2.gt.maxpsf)then
          call xvmessage(' NS*NL of psf storage buffer too small',' ')
          call abend
      endif
      k=1
      do j=1,nl2                    ! line loop
          call xvread(inunit2,psf(k),status,'LINE',j,' ')
          k=k+ns2
      enddo

c normalize the psf to 1 max.
      call psfnorm(psf,nl2,ns2)

c open outputs
      call xvunit(ounit1,'OUT',1,status,' ')
      call xvopen(ounit1,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')
      call xvunit(ounit2,'OUT',2,status,' ')
      call xvopen(ounit2,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')
      call xvunit(ounit3,'OUT',3,status,' ')
      call xvopen(ounit3,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')

c convolve input
      if(convolve)then
        call xvmessage('Convolving the input image',' ')
        call convolveit(pixbuf,nl,ns,psf,nl2,ns2,1,scr)
        do j=1,nl*ns
            pixbuf(j)=scr(j)
        enddo
      endif

c clean
      do j=1,nl*ns
          scr(j)=0.0
          hist(j)=0
      enddo
      call stats(pixbuf,nl,ns,total,sdev)
      old_sdev=sdev
      write(*,*)'initial residual ',sdev
      write(*,*)'iteration residual %decrease maxhist'
      do iter=1,maxiter
        if(total)then
          call clean_abs(pixbuf,nl,ns,psf,nl2,ns2,gain,noise,
     +                 hist,maxmod,scr)
        else
          call clean_pos(pixbuf,nl,ns,psf,nl2,ns2,gain,noise,
     +                 hist,maxmod,scr)
        endif
        if(mod(iter,ns*nl/10).eq.0)then
            call stats(pixbuf,nl,ns,total,sdev)
            step=100.*(old_sdev-sdev)/old_sdev
            old_sdev=sdev
            call maxhist(hist,nl,ns,maxh)
            write(*,100)iter,sdev,step,maxh
100         format(i8,2f10.3,i6)
            if(step.lt.change)goto 10
            if(sdev.lt.noise)goto 10
        endif
      enddo
      call stats(pixbuf,nl,ns,total,sdev)
      call maxhist(hist,nl,ns,maxh)
10    write(*,*)'final residual ',sdev
      write(*,*)iter,' iterations '
      write(*,*)'maxhist ',maxh

c write outputs 1 & 2
      k=1
      do j=1,nl
          call xvwrit(ounit1,scr(k),status,' ')
          call xvwrit(ounit2,pixbuf(k),status,' ')
          k=k+ns
      enddo

c convolve delta function image
      call convolveit(scr,nl,ns,psf,nl2,ns2,0,pixbuf)

c write output 3
      k=1
      do j=1,nl
          call xvwrit(ounit3,pixbuf(k),status,' ')
          k=k+ns
      enddo


      return
      end

c***************************************************************************
      subroutine maxhist(hist,nl,ns,maxh)
      integer*2 hist(ns,nl),maxh
      maxh=0
      do j=1,nl
        do i=1,ns
          if(hist(i,j).gt.maxh) maxh=hist(i,j)
        enddo
      enddo
      return
      end

c***************************************************************************
      subroutine stats(pix,nl,ns,total,sdev)
      logical total
      real*4 pix(ns,nl)
      sum=0.0
      if(total)then
        do j=1,nl
          do i=1,ns
            sum=sum+pix(i,j)**2
          enddo
        enddo
        sdev=sqrt(sum/(ns*nl))
      else
        n=0
        do j=1,nl
          do i=1,ns
            if(pix(i,j).gt.0.0)then
              sum=sum+pix(i,j)**2
              n=n+1
            endif
          enddo
        enddo
        if(n.eq.0)n=1
        sdev=sqrt(sum/n)
      endif
      return
      end

c***************************************************************************
      subroutine clean_pos(pix,nl,ns,psf,nl2,ns2,gain0,noise,
     +                   hist,maxmod,scr)
c clean algorithm
c pix is image of ns by nl
c psf is the psf of ns2 by nl2
c gain is the clean gain
c noise is the noise estimated standard deviation.
c maxmod is the maximum permitted number of changes permitted per pixel
c scr is the output image ns by nl
      real*4 pix(ns,nl),psf(ns2,nl2),gain,noise
      real*4 scr(ns,nl)
      integer*4 s,l
      integer*2 hist(ns,nl)

      ks=ns2/2
      kl=nl2/2
      gain=gain0

c find the max dn
      big=0.
      ii=0
      do j=1,nl
        do i=1,ns
          if(hist(i,j).lt.maxmod)then
            if(pix(i,j).gt.big)then
              big=pix(i,j)
              ii=i
              jj=j
            endif
          endif
        enddo
      enddo
      if(ii.eq.0)return

c subtract the psf
      l=0
      do j=jj-kl,jj+kl
        l=l+1
        s=0 
        do i=ii-ks,ii+ks
          s=s+1
          if(i.lt.1)goto 10
          if(i.gt.ns)goto 10
          if(j.lt.1)goto 10
          if(j.gt.nl)goto 10
          pix(i,j)=pix(i,j)-psf(s,l)*gain*big
10        continue
        enddo
      enddo

c update the delta function image
      scr(ii,jj)=scr(ii,jj)+gain*big

c update the histogram (number of times a pixel has been set)
      hist(ii,jj)=hist(ii,jj)+1

      return
      end

c***************************************************************************
      subroutine clean_abs(pix,nl,ns,psf,nl2,ns2,gain,noise,
     +                   hist,maxmod,scr)
c clean algorithm
c pix is image of ns by nl
c psf is the psf of ns2 by nl2
c gain is the clean gain
c noise is the noise estimated standard deviation.
c maxmod is the maximum permitted number of changes permitted per pixel
c scr is the output image ns by nl
      real*4 pix(ns,nl),psf(ns2,nl2),gain,noise
      real*4 scr(ns,nl)
      integer*4 s,l
      integer*2 hist(ns,nl)

      ks=ns2/2
      kl=nl2/2

c find the max dn
      big=-1.0
      ii=0
      do j=1,nl
        do i=1,ns
          if(hist(i,j).lt.maxmod)then
            if(abs(pix(i,j)).gt.big)then
              signbig=pix(i,j)
              big=abs(signbig)
              ii=i
              jj=j
            endif
          endif
        enddo
      enddo
      if(ii.eq.0)return

c subtract the psf
      l=0
      do j=jj-kl,jj+kl
        l=l+1
        s=0 
        do i=ii-ks,ii+ks
          s=s+1
          if(i.lt.1)goto 10
          if(i.gt.ns)goto 10
          if(j.lt.1)goto 10
          if(j.gt.nl)goto 10
          pix(i,j)=pix(i,j)-psf(s,l)*gain*signbig
10        continue
        enddo
      enddo

c update the delta function image
      scr(ii,jj)=scr(ii,jj)+gain*signbig

c update the histogram (number of times a pixel has been set)
      hist(ii,jj)=hist(ii,jj)+1

      return
      end

c***************************************************************************
c normalize psf to unity amplitude
      subroutine psfnorm(psf,nl2,ns2)
      real*4 psf(ns2,nl2)
      if(2*(ns2/2).eq.ns2)then
        write(*,*)'psf samples must be odd number'
        call abend
      endif
      if(2*(nl2/2).eq.ns2)then
        write(*,*)'psf lines must be odd number'
        call abend
      endif
      big=-1.0e+20
      do j=1,nl2
        do i=1,ns2
          if(psf(i,j).gt.big)then
            big=psf(i,j)
            ii=i
            jj=j
          endif
        enddo
      enddo
      do j=1,nl2
        do i=1,ns2
          psf(i,j)=psf(i,j)/big
        enddo
      enddo
      if((ii.ne.ns2/2+1).or.(jj.ne.nl2/2+1))then
        write(*,*)'psf center at sample ',ii,' line ',jj
        write(*,*)'should be at sample ',ns2/2+1,' line ',nl2/2+1
        call abend
      endif
      return
      end

c***************************************************************************
c convolve an image pix with a point spread function psf.
c pix is image of ns by nl
c psf is the psf of ns2 by nl2
c scr is the output of ns by nl
      subroutine convolveit(pix,nl,ns,psf,nl2,ns2,norm,scr)
      real*4 pix(ns,nl),psf(ns2,nl2),scr(ns,nl)
      integer*4 s,l
      ks=ns2/2
      kl=nl2/2
      do j=1,nl
        do i=1,ns
          sum=0.0
          sumw=0.0
          l=0
          do jj=j-kl,j+kl
            l=l+1
            s=0 
            do ii=i-ks,i+ks
              s=s+1
              if(ii.lt.1)goto 10
              if(ii.gt.ns)goto 10
              if(jj.lt.1)goto 10
              if(jj.gt.nl)goto 10
              sum=sum+pix(ii,jj)*psf(s,l)
              sumw=sumw+psf(s,l)
10            continue
            enddo
          enddo
          if(sumw.gt.0.0)then
            if(norm.eq.1)then
              scr(i,j)=sum/sumw
            else
              scr(i,j)=sum
            endif
          else
            scr(i,j)=0.0
          endif
        enddo
      enddo
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create clean.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM clean

   To Create the build file give the command:

		$ vimake clean			(VMS)
   or
		% vimake clean			(Unix)


************************************************************************/


#define PROGRAM	clean
#define R2LIB

#define MODULE_LIST clean.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create clean.pdf
process help=*
PARM INP TYPE=STRING COUNT=(2:3)
PARM OUT TYPE=STRING COUNT=3
PARM GAIN TYPE=REAL COUNT=(0:1) DEFAULT=0.2
PARM NOISE TYPE=REAL COUNT=(0:1) DEFAULT=2.0
PARM CHANGE TYPE=REAL COUNT=(0:1) DEFAULT=4.0
PARM ITER TYPE=INTEGER COUNT=(0:1) DEFAULT=1000000
PARM MAXMOD TYPE=INTEGER COUNT=(0:1) DEFAULT=200
PARM MODE TYPE=KEYWORD COUNT=(0:1) VALID=(TOTAL,POSITIVE) DEFAULT=TOTAL
PARM CONVOLVE TYPE=KEYWORD COUNT=(0:1) VALID=(CONVOLVE,NONE) DEFAULT=NONE
END-PROC

.TITLE
VICAR program CLEAN

.HELP
PURPOSE:
To implement the "clean" algorithm, where a point spread function is
iteratively removed from an image in order to deconvolve it.

EXECUTION:
clean inp=(blurredimage,psf) out=(restored,residual,convolved) mode=...

where:
blurredimage   Is the image convolved with the point spread function.
psf            Is the point spread function.
restored       Is an image which when convolved by psf will be similar to 
               blurredimage. It will not resemble the scene before
               convolution however.
residual       Is the remaining data after many psf subtractions.
convolved      Is "restored" convolved by psf. Should closely resemble
               "blurredimage". 

NOTE: psf must be odd in size and the max pixel must be at the center.

METHOD:
see   http://www.astro.virginia.edu/~eww6n/math/CLEANAlgorithm.html

Clean computes an image which, when convolved by psf, produces the input
blurred image. It does this by iteratively subtracting the psf until
nothing is left, just noise. Clean can be used to study the structure of
small abjects, to permit psf changes, and to make maps of artifact
inconsistent with the psf (which cannot be modelled as superpositions
of psf's).

Note: The restored image will not resemble the true image before convolution
with the point spread function. It is merely a possible such image.
The rms nearest image is generated by program WIENER.
The most likely image is generated by program MEM.

HISTORY:
9-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
1. blurred image.
2. point spread function.

.VARI OUT
1. delta function image.
2. residual image
3. convolved restored image

.VARI GAIN
algorithm gain.

.VARI NOISE
Standard deviation
 of the noise.

.VARI ITER
Number of iterations.

.VARI CHANGE
Limiting change in
residual

.VARI MAXMOD
Max number of changes
per pixel.

.VARI MODE
Operation mode:
TOTAL or POSITIVE

.VARI CONVOLVE
Convolves the input image

.LEVEL2
.VARI INP
1. blurred image.
2. point spread function.

.VARI OUT
1. delta function image.
2. residual image (input after psf removals).
3. delta function image convolved with psf.

.VARI GAIN
algorithm gain. Each time a point spread function is subtracted from the
image the amplitude of the psf is set to GAIN* the DN of the pixel.
Default is 0.2
Large point spread functions should have smaller gains.

.VARI NOISE
Standard deviation of the noise.
Iterations will cease when the residual falls below NOISE.
Defaults to 2.0
This test usually stops the algorithm in the case MODE=POSITIVE.

.VARI ITER
Maximum permitted umber of iterations.
Defaults to 1,000,000

.VARI CHANGE
Iterations will cease when the percent reduction in the residual becomes 
less than CHANGE.
Defaults to 4.0
This test usually stops the algorithm in the case MODE=TOTAL

.VARI MAXMOD
The maximum number of times a pixel can be modified. This keeps clean
from cycling between a small set of pixels comprising artifact
which cannot be subracted as point spread functions.
Defaults to 200

.VARI MODE
The mode of operation.
There are two modes:

TOTAL where the point spread function can be positive or negative.
This mode will match the input image very well but will create negative
intensities in the restored image.

POSITIVE where the point spread function can only be positive.
This mode will not match the input image very well but will create
positive intensities in the restored image.

Default is TOTAL.

.VARI CONVOLVE
Causes the input image to be convolved with the point spread function
before any other operations. The default is for the first input to be
convolved already.
$ Return
$!#############################################################################
$Test_File:
$ create tstclean.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
copy /project/test_work/testdata/gll/s0349632000.u orig.img sl=75 ss=150 +
 nl=512 ns=512
copy inp=orig.img out=o.img nl=256 ns=256 
spot out=a.img shape=gaussian sigmax=10 sigmay=10 x0=128 y0=128
copy inp=a.img out=psf.img sl=95 ss=95 nl=67 ns=67
copy inp=a.img out=b.img nl=256 ns=256
swap inp=b.img out=spsf.img
fft22 inp=spsf.img out=mtf.img
fft22 inp=o.img out=ffto.img
wiener inp=(ffto.img,mtf.img) out=fftblurred.img 'direct
fft22 inp=fftblurred.img out=blurred.img 'inverse format=real
gausnois out=noise.img nl=128 ns=128 mean=0. sigma=2.0 format=real
copy inp=blurred.img out=c.img sl=64 ss=64 nl=128 ns=128
f2 inp=(c.img,noise.img) out=o.img function="in1+in2"
copy inp=orig.img out=a.img sl=64 ss=64 nl=128 ns=128
!
clean inp=(o.img,psf.img) out=(b.img,c.img,d.img) 'positive gain=.01
xvd b.img
xvd c.img
xvd d.img
xvd o.img
xvd a.img
clean inp=(o.img,psf.img) out=(b.img,c.img,d.img) 'total gain=.05
xvd b.img
xvd c.img
xvd d.img
!
end-proc
$ Return
$!#############################################################################
