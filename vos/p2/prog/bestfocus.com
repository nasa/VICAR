$!****************************************************************************
$!
$! Build proc for MIPL module bestfocus
$! VPACK Version 1.9, Wednesday, March 10, 2004, 11:20:59
$!
$! Execute by entering:		$ @bestfocus
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
$ write sys$output "*** module bestfocus ***"
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
$ write sys$output "Invalid argument given to bestfocus.com file -- ", primary
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
$   if F$SEARCH("bestfocus.imake") .nes. ""
$   then
$      vimake bestfocus
$      purge bestfocus.bld
$   else
$      if F$SEARCH("bestfocus.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bestfocus
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bestfocus.bld "STD"
$   else
$      @bestfocus.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bestfocus.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bestfocus.com -mixed -
	-s bestfocus.f -
	-i bestfocus.imake -
	-p bestfocus.pdf -
	-t tstbestfocus.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bestfocus.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c program bestfocus

      include 'VICMAIN_FOR'
      subroutine main44
      
      parameter (nlmax=1100,nsmax=1100,maxpix=10)
      real*4 buf(nsmax,nlmax),obuf(nsmax,nlmax,maxpix)
      real*4 inbuf(nsmax,nlmax,maxpix)
      integer*4 sborder,count,def,status,unit(maxpix),ounit(2)
      real*8 sum(nsmax),sumc(nsmax),sumc2(nsmax),normalize
      real*8 sum2(nsmax)
      logical inverse,xvptst

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.gt.maxpix)then
        write(*,*)'Too many input images, increase maxpix'
        call abend
      endif
      call xvpcnt('OUT',nout)
      call xvparm('NLW',nlw,count,def,1)
      if(nlw.eq.(nlw/2)*2)nlw=nlw+1
      call xvparm('NSW',nsw,count,def,1)
      if(nsw.eq.(nsw/2)*2)nsw=nsw+1
      inverse=xvptst('INVERSE')

c open inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
        if(nl.gt.nlmax)then
          call xvmessage('Line column too long',' ')
          call abend
        endif
      enddo

c open outputs
        call xvunit(ounit(1),'OUT',1,status,' ')
        call xvopen(ounit(1),status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')
        call xvunit(ounit(2),'OUT',2,status,' ')
        call xvopen(ounit(2),status,'U_FORMAT','REAL',
     +              'OP','WRITE','O_FORMAT','REAL',' ')

      normalize=nlw*nsw
      sborder=(nsw-1)/2
      lborder=(nlw-1)/2
      nlo=nl+2*lborder
      nso=ns+2*sborder
      if(nlo.gt.nlmax.or.nso.gt.nsmax)then
        write(*,*)'internal image buffer too small'
        stop
      endif

      image=0
10    image=image+1
      do j=1,nl
        call xvread(unit(image),buf(1,j),status,'LINE',j,' ')
      enddo

c save inputs
      do j=1,nl
        do i=1,ns
          inbuf(i,j,image)=buf(i,j)
        enddo
      enddo

c shift image area to leave a border around it.
      do j=nl,1,-1
        do i=ns,1,-1
          buf(i+sborder,j+lborder)=buf(i,j)
        enddo
      enddo

c reflect the image into the borders
      k=lborder
      do j=lborder,1,-1
        k=k+1
        do i=1,nso
          buf(i,j)=buf(i,k)
        enddo
      enddo
      k=nlo-lborder+1
      do j=nlo-lborder+1,nlo
        k=k-1
        do i=1,nso
          buf(i,j)=buf(i,k)
        enddo
      enddo
      k=sborder
      do i=sborder,1,-1
        k=k+1
        do j=1,nlo
          buf(i,j)=buf(k,j)
        enddo
      enddo
      k=nso-sborder+1
      do i=nso-sborder+1,nso
        k=k-1
        do j=1,nlo
          buf(i,j)=buf(k,j)
        enddo
      enddo

c collect first sums of columns to get started.
      do i=1,nso
        sumc(i)=0.d0
        sumc2(i)=0.d0
        do j=1,nlw
          sumc(i)=sumc(i)+buf(i,j)
          sumc2(i)=sumc2(i)+dble(buf(i,j))**2
        enddo
      enddo

      do j=1,nl

c compute low pass
        sum(1)=0.d0
        sum2(1)=0.d0
        do i=1,nsw
          sum(1)=sum(1)+sumc(i)
          sum2(1)=sum2(1)+sumc2(i)
        enddo
        do i=2,ns
          sum(i)=sum(i-1)+sumc(i+nsw-1)-sumc(i-1)
          sum2(i)=sum2(i-1)+sumc2(i+nsw-1)-sumc2(i-1)
        enddo

c update column sums
        do i=1,nso
          sumc(i)=sumc(i)+buf(i,j+nlw)-buf(i,j)
          sumc2(i)=sumc2(i)+dble(buf(i,j+nlw))**2-dble(buf(i,j))**2
        enddo

c compute standard deviation
        do i=1,ns
          obuf(i,j,image)=sqrt(sum2(i)/normalize-(sum(i)/normalize)**2)
        enddo

      enddo

      if(image.lt.nin)goto 10

c create best focus image
      do j=1,nl
        do i=1,ns
          best=-1.0
          do k=1,nin
            if(obuf(i,j,k).gt.best)then
              best=obuf(i,j,k)
              n=k
            endif
          enddo
          buf(i,j)=inbuf(i,j,n)
        enddo
        call xvwrit(ounit(1),buf(1,j),status,' ')
      enddo

      if(nout.lt.2)return

c create relative topographic image
      nlow=0
      nhigh=0
      do j=1,nl
        do i=1,ns
          best=-1.0
          do k=1,nin
            if(obuf(i,j,k).gt.best)then
              best=obuf(i,j,k)
              n=k ! the index of the highest point
            endif
          enddo
          if(n.eq.1)then
            buf(i,j)=n
            nlow=nlow+1
          else if(n.eq.nin)then
            buf(i,j)=nin
            nhigh=nhigh+1
          else
            a1=obuf(i,j,n-1)
            b1=obuf(i,j,n)
            c1=obuf(i,j,n+1)
            d=2.0*b1-c1-a1
            if(d.ne.0.0)then
              buf(i,j)=n-0.5*(a1-c1)/d
            else
              buf(i,j)=n
            endif
          endif
          if(inverse)buf(i,j)=nin-buf(i,j)+1
        enddo
        call xvwrit(ounit(2),buf(1,j),status,' ')
      enddo
      if(nlow.gt.0)then
        write(*,*)100.*real(nlow)/real(nl*ns),
     +  ' % of points had a solution below image 1, set to 1'
      endif
      if(nhigh.gt.0)then
        write(*,*)100.*real(nhigh)/real(nl*ns),
     +  ' % of points had a solution above image ',nin,
     +  ' ,set to ',nin
      endif

      end





$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bestfocus.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM bestfocus

   To Create the build file give the command:

		$ vimake bestfocus			(VMS)
   or
		% vimake bestfocus			(Unix)


************************************************************************/


#define PROGRAM	bestfocus
#define R2LIB

#define MODULE_LIST bestfocus.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create bestfocus.pdf
process help=*
PARM INP TYPE=STRING COUNT=(3:10)
PARM OUT TYPE=STRING COUNT=(1:2)
PARM INVERSE TYPE=KEYWORD COUNT=(0:1) VALID=("DIRECT","INVERSE") +
 DEFAULT="DIRECT"
PARM NLW TYPE=INTEGER COUNT=(0:1) VALID=(3:101) DEFAULT=5
PARM NSW TYPE=INTEGER COUNT=(0:1) VALID=(3:101) DEFAULT=5
END-PROC

.TITLE
VICAR program bestfocus

.HELP
Purpose: To combine a set of images taken of a 3-d object
         at progressively different focus positions and create:
1. A single best focus image.
2. A relative depth map (topo or DTM).

Usage: bestfocus in=p1,p2,p3,...pn out=focus,topo parameters
where:

p1...pn are input images with incremental changes in focus. The object
must be a 3-d target.

'focus' is a composite image made from the best focus locations of 
all the input images. Same format as input.

'topo' is a scaled relative depth map or topo map (dtm map). See the
"scale" parameter for scaling. Without scaling the topo image values
range from 1 to nin where nin is the number of input images.
Format is REAL.
 
Restrictions:
The focus must monotonically increase (or decrease) in the order of the
input images.


.PAGE

METHOD:
Bestfocus computes the local standard deviation in a moving box nlw
lines by nsw samples across all input images. It then computes a
curve of standard deviation versus image input number at each pixel.
The image with the highest standard deviation is nearest the best focus
and this image is selected for the "focus" image at this pixel.
The peak of the curve is determined by quadratic fitting to the highest
point and it's two lower neighbors. This number from 1 to nin (the
number of input images) is then scaled by the "scale" parameter and
written to the "topo" image. output=topo*scale1+scale2 (see scale
defaults). The "inverse" parameter can complement topo before scaling.
If the highest standard deviation image is 1 or nin then no quadratic
fit is made and the result will be 1 or nin respectively. Quadratic fitting
results in non integer topo values but the limit will always be between
1.0 and real(nin).
Note a low pass median filter will improve the "topo" image.

HISTORY:
2-2004  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
3-10 inputs

.VARI OUT
1. best focus
2. topo map (REAL)

.VARI INVERSE
Complement topomap

.VARI NLW
Filter height, Odd #.

.VARI NSW
Filter width, Odd #


.LEVEL2
.VARI INP
3-10 inputs

.VARI OUT
1. best focus
2. topo map (REAL format)

.VARI INVERSE
Complement topomap
the topo map will be complemented before being scaled and written.

.VARI NLW
Filter height, Odd #.
Must be an odd integer. Default is nlw=5

.VARI NSW
Filter width, Odd #
Must be an odd integer. Default is nsw=5


end-proc                                                                     
        
$ Return
$!#############################################################################
$Test_File:
$ create tstbestfocus.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
bestfocus inp=(f1.img,f2.img,f3.img,f4.img) out=(f.img,topo.img) 'inverse
!
end-proc
$ Return
$!#############################################################################
