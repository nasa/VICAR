$!****************************************************************************
$!
$! Build proc for MIPL module bestscale
$! VPACK Version 1.9, Wednesday, March 17, 2004, 09:15:27
$!
$! Execute by entering:		$ @bestscale
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
$ write sys$output "*** module bestscale ***"
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
$ write sys$output "Invalid argument given to bestscale.com file -- ", primary
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
$   if F$SEARCH("bestscale.imake") .nes. ""
$   then
$      vimake bestscale
$      purge bestscale.bld
$   else
$      if F$SEARCH("bestscale.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bestscale
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bestscale.bld "STD"
$   else
$      @bestscale.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bestscale.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bestscale.com -mixed -
	-s bestscale.f -
	-i bestscale.imake -
	-p bestscale.pdf -
	-t tstbestscale.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bestscale.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c program bestscale

      include 'VICMAIN_FOR'
      subroutine main44
      
      parameter (nlmax=1100,nsmax=1100)
      real*4 inbuf(nsmax,nlmax),obuf(nsmax,nlmax)
      real*4 scale(2),refbuf(nsmax,nlmax)
      real*4 cor(100),sc(100)
      integer*4 count,def,status,unit,ounit
      real*8 r,sumx,sumy,sumx2,sumy2,sumxy

      call xvpcnt('INP',nin)
      call xvpcnt('OUT',nout)

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      call xvpcnt('OUT',nout)
      if(nin.ne.nout)then
        write(*,*)'inputs must match outputs in number'
        call abend
      endif
      call xvparm('SCALE',scale,count,def,2)

c open input1
      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'U_FORMAT','REAL',' ')
      call xvget(unit,status,'NL',nl,'NS',ns,' ')
      if(ns.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
      endif
      if(nl.gt.nlmax)then
          call xvmessage('Line column too long',' ')
          call abend
      endif

c open output1
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')

c copy first input to first output
      do j=1,nl
        call xvread(unit,refbuf(1,j),status,'LINE',j,' ')
        call xvwrit(ounit,refbuf(1,j),status,' ')
      enddo
      call xvclose(unit,status,'CLOS_ACT','FREE',' ')
      call xvclose(ounit,status,'CLOS_ACT','FREE',' ')

      sinc=(scale(2)-scale(1))/50. ! can go to 100
      cx=ns/2.0
      cy=nl/2.0
      inc=5
      mode=0 ! bilinear
      write(*,*)'Image   scale   correlation'
      write(*,*)1,1.0,1.0
      inp=1
10    inp=inp+1

c read next image
      call xvselpi(inp) ! use this input label for the output.
      call xvunit(unit,'INP',inp,status,' ')
      call xvopen(unit,status,'U_FORMAT','REAL',' ')
      do j=1,nl
        call xvread(unit,inbuf(1,j),status,'LINE',j,' ')
      enddo

c compute correlations between each image and refimage
c as a function of scale.
      k=0
      do s=scale(1),scale(2),sinc
        k=k+1
        n=0
        sumx=0.d0
        sumx2=0.d0
        sumy=0.d0
        sumy2=0.d0
        sumxy=0.d0
        do j=1,nl,inc
          y=(j-cy)*s+cy
          do i=1,ns,inc
            x=(i-cx)*s+cx
            call interpolate(inbuf,nlmax,nsmax,nl,ns,mode,
     +                         x,y,dn,ind)
            if(ind.eq.0)then
              n=n+1
              sumx=sumx+refbuf(i,j)
              sumx2=sumx2+refbuf(i,j)*refbuf(i,j)
              sumy=sumy+dn
              sumy2=sumy2+dn*dn
              sumxy=sumxy+refbuf(i,j)*dn
            endif
          enddo
        enddo
        r=(sumxy-sumx*sumy/n)**2/
     +    ((sumx2-sumx**2/n)*(sumy2-sumy**2/n))
        cor(k)=dsqrt(r)
        sc(k)=s
c        write(*,*)k,sc(k),cor(k)
      enddo
      
c obtain the best estimate for the scale
      best=-1.0
      do i=1,k
        if(cor(i).gt.best)then
          best=cor(i)
          n=i
        endif
      enddo
      if(n.eq.1)then
        s=sc(1)
      else if(n.eq.k)then
        s=sc(k)
      else
        s=sc(n)-0.5*
     +    ((sc(n)-sc(n-1))**2*(cor(n)-cor(n+1))-(sc(n)-sc(n+1))**2*
     +    (cor(n)-cor(n-1)))  /
     +    ((sc(n)-sc(n-1))*(cor(n)-cor(n+1))-(sc(n)-sc(n+1))*
     +    (cor(n)-cor(n-1)))
      endif
      write(*,*)inp,s,cor(n)
      
c generate image at best scale
      do j=1,nl
        y=(j-cy)*s+cy
        do i=1,ns
          x=(i-cx)*s+cx
          call interpolate(inbuf,nlmax,nsmax,nl,ns,mode,
     +                         x,y,dn,ind)
          obuf(i,j)=dn
        enddo
      enddo

c write next output
      call xvunit(ounit,'OUT',inp,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')
      do j=1,nl
        call xvwrit(ounit,obuf(1,j),status,' ')
      enddo
      call xvclose(unit,status,'CLOS_ACT','FREE',' ')
      call xvclose(ounit,status,'CLOS_ACT','FREE',' ')
      
      if(inp.lt.nin)goto 10

      return
      end


      subroutine interpolate(inbuf,nlbuf,nsbuf,nl,ns,mode,
     +  x,y,dn,ind)
c inbuf of dimension nsbuf by nlbuf, input , real, ns by nl data.
c mode is the interpolation mode.
c mode=0 bilinear interpolation, 4 neighbors.
c mode=1 nearest neighbor.
c mode=2 2nd order, 6 neighbors.
c mode=3 sampling theorem, 15 by 15 convolution.
c mode=4 bicubic spline
c x,y are real coordinates of input point, input, real.
c dn is interpolated intensity, returned, real.
c ind=0 if dn computed ok. ind=1 if x,y out of bounds.
c    returned, integer.
c If ind=0 on return, dn is set to zero.

      parameter (kerw=9)
      real*4 inbuf(nsbuf,nlbuf),tw,bw,lw,rw
      real*4 kerx(-kerw:kerw),kery(-kerw:kerw)
      real*4 ya(4),y1(4),y2(4),y12(4),c(4,4),cl(16),xa(16)
      real*4 wt(16,16)
      DATA WT/1.,0.,-3.,2.,4*0.,-3.,0.,9.,-6.,2.,0.,-6.,
     *  4.,8*0.,3.,0.,-9.,6.,-2.,0.,6.,-4.,10*0.,9.,-6.,
     *  2*0.,-6.,4.,2*0.,3.,-2.,6*0.,-9.,6.,2*0.,6.,-4.,
     *  4*0.,1.,0.,-3.,2.,-2.,0.,6.,-4.,1.,0.,-3.,2.,8*0.,
     *  -1.,0.,3.,-2.,1.,0.,-3.,2.,10*0.,-3.,2.,2*0.,3.,
     *  -2.,6*0.,3.,-2.,2*0.,-6.,4.,2*0.,3.,-2.,0.,1.,-2.,
     *  1.,5*0.,-3.,6.,-3.,0.,2.,-4.,2.,9*0.,3.,-6.,3.,0.,
     *  -2.,4.,-2.,10*0.,-3.,3.,2*0.,2.,-2.,2*0.,-1.,1.,
     *  6*0.,3.,-3.,2*0.,-2.,2.,5*0.,1.,-2.,1.,0.,-2.,4.,
     *  -2.,0.,1.,-2.,1.,9*0.,-1.,2.,-1.,0.,1.,-2.,1.,10*0.,
     *  1.,-1.,2*0.,-1.,1.,6*0.,-1.,1.,2*0.,2.,-2.,2*0.,-1.,1./

      ind=1
      dn=0.0

      if(mode.eq.0)then ! bilinear
        i=x
        j=y
        if(i.lt.1)return
        if(j.lt.1)return
        if(i.ge.ns)return
        if(j.ge.nl)return
        ind=0
        lw=x-i
        rw=1.0-lw
        tw=y-j
        bw=1.0-tw
        top=lw*inbuf(i+1,j)+rw*inbuf(i,j)
        bot=lw*inbuf(i+1,j+1)+rw*inbuf(i,j+1)
        dn=bw*top+tw*bot

      else if(mode.eq.1)then ! nearest neighbor
        i=nint(x)
        j=nint(y)
        if(i.lt.1)return
        if(j.lt.1)return
        if(i.gt.ns)return
        if(j.gt.nl)return
        ind=0
        dn=inbuf(i,j)

      else if(mode.eq.2)then ! second order
        i=nint(x)
        j=nint(y)
        if(i.lt.2)return
        if(j.lt.2)return
        if(i.ge.ns)return
        if(j.ge.nl)return
        ind=0
        p=x-i
        q=y-j
        dn=q*(q-1.0)*0.5*inbuf(i,j-1)+
     +     p*(p-1.0)*0.5*inbuf(i-1,j)+
     +    (1.0+p*q-p*p-q*q)*inbuf(i,j)+
     +    p*(p-2.0*q+1.0)*0.5*inbuf(i+1,j)+
     +    q*(q-2.0*p+1.0)*0.5*inbuf(i,j+1)+
     +    p*q*inbuf(i+1,j+1)

      else if(mode.eq.3)then ! Sampling Theorem
        ii=nint(x)
        jj=nint(y)
        if(ii-kerw.lt.1)return
        if(jj-kerw.lt.1)return
        if(ii+kerw.gt.ns)return
        if(jj+kerw.gt.nl)return
        pi=3.14159
        ind=0
        xx=x-ii
        do i=-kerw,kerw
          d=pi*(i-xx)
          if(d.ne.0.0)then
            kerx(i)=sin(d)/d
          else
            kerx(i)=1.0
          endif
        enddo
        yy=y-jj
        do j=-kerw,kerw
          d=pi*(j-yy)
          if(d.ne.0.0)then
            kery(j)=sin(d)/d
          else
            kery(j)=1.0
          endif
        enddo
        dn=0.0
        do j=-kerw,kerw
          do i=-kerw,kerw
            dn=dn+kery(j)*kerx(i)*inbuf(ii+i,jj+j)
          enddo
        enddo

      else if(mode.eq.4)then ! bicubic spline
        i=x
        j=y
        if(i.lt.2)return
        if(j.lt.2)return
        if(i.gt.ns-2)return
        if(j.gt.nl-2)return
        ind=0
        ya(1)=inbuf(i,j)
        ya(2)=inbuf(i+1,j)
        ya(3)=inbuf(i+1,j+1)
        ya(4)=inbuf(i,j+1)
        y1(1)=(inbuf(i+1,j)-inbuf(i-1,j))/2.0
        y1(2)=(inbuf(i+2,j)-inbuf(i,j))/2.0
        y1(3)=(inbuf(i+2,j+1)-inbuf(i,j+1))/2.0
        y1(4)=(inbuf(i+1,j+1)-inbuf(i-1,j+1))/2.0
        y2(1)=(inbuf(i,j+1)-inbuf(i,j-1))/2.0
        y2(2)=(inbuf(i+1,j+1)-inbuf(i+1,j-1))/2.0
        y2(3)=(inbuf(i+1,j+2)-inbuf(i+1,j))/2.0
        y2(4)=(inbuf(i,j+2)-inbuf(i,j))/2.0
        y12(1)=(inbuf(i+1,j+1)-inbuf(i+1,j-1)-
     +          inbuf(i-1,j+1)+inbuf(i-1,j-1))/4.0
        y12(2)=(inbuf(i+2,j+1)-inbuf(i+2,j-1)-
     +          inbuf(i,j+1)+inbuf(i,j-1))/4.0
        y12(3)=(inbuf(i+2,j+2)-inbuf(i+2,j)-
     +          inbuf(i,j+2)+inbuf(i,j))/4.0
        y12(4)=(inbuf(i+1,j+2)-inbuf(i+1,j)-
     +          inbuf(i-1,j+2)+inbuf(i-1,j))/4.0
        ii=i
        jj=j
        DO 11 I=1,4
          Xa(I)=Ya(I)
          Xa(I+4)=Y1(I)
          Xa(I+8)=Y2(I)
          Xa(I+12)=Y12(I)
11      CONTINUE
        DO 13 I=1,16
          XX=0.
          DO 12 K=1,16
            XX=XX+WT(I,K)*Xa(K)
12        CONTINUE
          CL(I)=XX
13      CONTINUE
        L=0
        DO 15 I=1,4
          DO 14 J=1,4
            L=L+1
            C(I,J)=CL(L)
14        CONTINUE
15      CONTINUE
        T=x-ii
        U=y-jj
        dn=0.
        DO 16 I=4,1,-1
          dn=T*dn+((C(I,4)*U+C(I,3))*U+C(I,2))*U+C(I,1)
16      CONTINUE

      else
        write(*,*)'INTERPOLATE: invalid mode'
        stop
      endif
      return
      end



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bestscale.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM bestscale

   To Create the build file give the command:

		$ vimake bestscale			(VMS)
   or
		% vimake bestscale			(Unix)


************************************************************************/


#define PROGRAM	bestscale
#define R2LIB

#define MODULE_LIST bestscale.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create bestscale.pdf
process help=*
PARM INP TYPE=STRING COUNT=(2:10)
PARM OUT TYPE=STRING COUNT=(2:10)
PARM SCALE TYPE=REAL COUNT=(0,2) VALID=(0.:1000.,0.:1000.) DEFAULT=(.7,1.)
END-PROC

.TITLE
VICAR program bestscale
.HELP
PURPOSE:
To rescale images all the same size by geoming each image to match the 
first one. All geoms are centered at the image center and offer only 
differences in image size (magnification) about the center. The program
determines the amount of magnification.

.PAGE
example:
bestscale inp=(p1,p2,p3,p4,p5) out=(s1,s2,s3,s4,s5) scale=(.7,1.)
bestfocus inp=(s1,s2,s3,s4,s5) out=(focus,dtm) 'inverse

USAGE:
To get output images without black borders, order your input images such
that the image with the greatest magnification (smallest field of view)
is first. This way all images will be magnified and the data contents
of the output images will be about the same. Then your scale parameter
will go from a number less than 1 to 1. Example: scale=(.5,1.)

METHOD:
Bestscale finds the best scale by computing Pearson's R between the first input
and all the others. Each image is rescaled between the SCALE limits in
equal intervals until the best value of R is found. The best scale is then 
interpolated and the image geomed. Image 1 is copied to the first output.
Scale is the factor by which image structure is smaller or larger than it is
in the first input image.

HISTORY:
2-2004  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
2-10 inputs

.VARI OUT
2-10 outputs

.VARI SCALE
1. lowest scale
2. highest scale

.LEVEL2
.VARI INP
2-10 inputs. Input 1 is the reference image.

.VARI OUT
2-10 outputs. Output 1 is a copy of input 1.

.VARI SCALE
1. lowest scale
2. highest scale
The range of permitted scales over which the program may search.
Scale is the factor by which image structure is smaller or larger than it is
in the first input image.
The program searches for a scale between low scale and high scale. The program
selects the final scale by itself. A different scale is computed for each input
image. See USAGE section for hints on ordering the input images.

end-proc                                                                     
        
$ Return
$!#############################################################################
$Test_File:
$ create tstbestscale.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
bestscale inp=(v4.img,v3.img,v2.img,v1.img) +
  out=(f4.img,f3.img,f2.img,f1.img) scale=(.7,1.1)
bestfocus inp=(f4.img,f3.img,f2.img,f1.img) out=(f.img,topo.img) 'inverse
!
end-proc
$ Return
$!#############################################################################
