$!****************************************************************************
$!
$! Build proc for MIPL module topomap
$! VPACK Version 1.9, Wednesday, March 27, 2002, 10:13:40
$!
$! Execute by entering:		$ @topomap
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
$ write sys$output "*** module topomap ***"
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
$ write sys$output "Invalid argument given to topomap.com file -- ", primary
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
$   if F$SEARCH("topomap.imake") .nes. ""
$   then
$      vimake topomap
$      purge topomap.bld
$   else
$      if F$SEARCH("topomap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake topomap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @topomap.bld "STD"
$   else
$      @topomap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create topomap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack topomap.com -mixed -
	-s topomap.f -
	-i topomap.imake -
	-p topomap.pdf -
	-t tsttopomap.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create topomap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program topomap
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxnl=1000, maxns=1000)
      character*132 msg
      integer*4 def,count,status,unit1,unit2,unit3,ounit
      real*4 buf(maxns,maxnl),bufnew(maxns,maxnl)
      real*4 mark(4,1000),linedisp(4000)
      real*4 sampdisp(4000),dn(1000),r(1000)
      real*8 length(-3:362),lpf_length(-3:362)
      integer*4 hist(0:359),pass

      call xvpcnt('INP',nids)
      call xvparm('RADIUS',nw,count,def,1)
      call xvparm('MINPTS',minpts,count,def,1)
      call xvparm('MAXPASS',maxpass,count,def,1)

      call xvunit(unit1,'INP',1,status,' ')
      call xvopen(unit1,status,' ')
      call xvget(unit1,status,'NL',nl1,'NS',ns1,' ')
      call xvunit(unit2,'INP',2,status,' ')
      call xvopen(unit2,status,'U_FORMAT','REAL',' ')
      call xvget(unit2,status,'NL',nl2,'NS',ns2,' ')
      if(nids.eq.3)then
        call xvunit(unit3,'INP',3,status,' ')
        call xvopen(unit3,status,'U_FORMAT','REAL',' ')
      endif

      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +    'OP','WRITE',' ')

c     Checks
      if(nl1.gt.maxnl)then
        call xvmessage('Too many input lines',' ')
        call abend()
      endif
      if(ns1.gt.maxns)then
        call xvmessage('Too many input samples',' ')
        call abend()
      endif

c     Clear image buffer.
      do j=1,nl1
        do i=1,ns1
          buf(i,j)=0.0
          bufnew(i,j)=0.0
        enddo
      enddo

c     Determine histogram of vector direction.
      do i=0,359
        hist(i)=0
        length(i)=0.d0
      enddo
      if(nids.eq.2)then   ! MARK input
        do j=1,nl2
          call xvread(unit2,mark,status,'LINE',j,' ')
          do i=1,ns2/4
            difs=mark(4,i)-mark(2,i)
            difl=mark(3,i)-mark(1,i)
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              angle=atan2(-difl,difs)*57.29578
              if(angle.lt.0.0)angle=360.+angle
              k=angle
              if(k.gt.359)k=359
              if(k.lt.0)k=0
              hist(k)=hist(k)+1
              length(k)=length(k)+sqrt(difs*difs+difl*difl)
            endif
          enddo
        enddo
      else                 ! DISPARITY inputs
        do j=1,nl2
          call xvread(unit2,linedisp,status,'LINE',j,' ')
          call xvread(unit3,sampdisp,status,'LINE',j,' ')
          do i=1,ns2
            difs=sampdisp(i)-i
            difl=linedisp(i)-j
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              angle=atan2(-difl,difs)*57.29578
              if(angle.lt.0.0)angle=360.+angle
              k=angle
              if(k.gt.359)k=359
              if(k.lt.0)k=0
              hist(k)=hist(k)+1
              length(k)=length(k)+sqrt(difs*difs+difl*difl)
            endif
          enddo
        enddo
      endif

c     Determine major vector direction
      length(-1)=length(359)
      length(-2)=length(358)
      length(-3)=length(357)
      length(360)=length(0)
      length(361)=length(1)
      length(362)=length(2)
      call uniflt(8,366,length(-3),lpf_length(-3),7)
      max_length=0
      k=-1
      do i=0,359
c       write(*,*)i,hist(i),length(i),lpf_length(i)
        if(lpf_length(i).gt.max_length)then
          max_length=lpf_length(i)
          k=i
        endif
      enddo
      if(k.eq.-1)then
        call xvmessage('All disparities are zero',' ')
        call abend()
      endif
      write(*,*)'Mean disparity is at ',k,' degrees'
      vector_angle=k
      if(vector_angle.gt.180.)vector_angle=vector_angle-360.
      vector_angle=vector_angle/57.29578
      vector_length=lpf_length(k)/hist(k)
      write(*,*)'Mean component vector length is ',
     +           vector_length,' pixels'

c     Compute the topomap at specific input values.
      if(nids.eq.2)then   ! MARK input
        do j=1,nl2
          call xvread(unit2,mark,status,'LINE',j,' ')
          do i=1,ns2/4
            difs=mark(4,i)-mark(2,i)
            difl=mark(3,i)-mark(1,i)
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              dangle=atan2(-difl,difs)-vector_angle
              vlength=sqrt(difs*difs+difl*difl)
              ii=nint(mark(2,i))
              jj=nint(mark(1,i))
              buf(ii,jj)=vlength*cos(dangle)
            endif
          enddo
        enddo
      else                 ! DISPARITY inputs
        do j=1,nl2
          call xvread(unit2,linedisp,status,'LINE',j,' ')
          call xvread(unit3,sampdisp,status,'LINE',j,' ')
          do i=1,ns2
            difs=sampdisp(i)-i
            difl=linedisp(i)-j
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              dangle=atan2(-difl,difs)-vector_angle
              vlength=sqrt(difs*difs+difl*difl)
              buf(i,j)=vlength*cos(dangle)
            endif
          enddo
        enddo
      endif

c     Fillin the missing pixels.
      rnw=nw*nw+.1 ! radius squared of collection circle + a bit
      pass=0
100   count=0
      pass=pass+1
      do j=1,nl1
        do i=1,ns1
          if(buf(i,j).ne.0.0)goto 10
          k=0
          do jj=max(j-nw,1),min(j+nw,nl1)
            j2=(j-jj)**2
            do ii=max(i-nw,1),min(i+nw,ns1)
              if(buf(ii,jj).ne.0.0)then
c               rad=(i-ii)**2+(j-jj)**2
                rad=(i-ii)**2+j2
                if(rad.le.rnw)then
                  k=k+1
                  dn(k)=buf(ii,jj)
                  r(k)=rad
                endif
              endif
            enddo
          enddo
          if(k.lt.minpts)goto 10
          sum1=0.0
          sum2=0.0
          do n=1,k
            sum1=sum1+dn(n)/r(n)
            sum2=sum2+1.0/r(n)
          enddo
          bufnew(i,j)=sum1/sum2
          count=count+1
10        continue
        enddo
      enddo
      do j=1,nl1
        do i=1,ns1
          if(bufnew(i,j).ne.0.0)then
            buf(i,j)=bufnew(i,j)
            bufnew(i,j)=0.0
          endif
        enddo
      enddo
      write(*,*)count,' pixels interpolated'
      if((count.gt.0).and.(pass.lt.maxpass))then
        goto 100
      endif


c     Write output
      do j=1,nl1
        call xvwrit(ounit,buf(1,j),status,' ')
      enddo

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create topomap.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM topomap

   To Create the build file give the command:

		$ vimake topomap			(VMS)
   or
		% vimake topomap			(Unix)


************************************************************************/


#define PROGRAM	topomap
#define R2LIB

#define MODULE_LIST topomap.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create topomap.pdf
process help=*
PARM INP TYPE=STRING COUNT=(2:3)
PARM OUT TYPE=STRING COUNT=(1)
PARM RADIUS TYPE=INTEGER COUNT=(0:1) VALID=(1:17) DEFAULT=13
PARM MINPTS TYPE=INTEGER COUNT=(0:1) VALID=(1:1000) DEFAULT=4
PARM MAXPASS TYPE=INTEGER COUNT=(0:1) VALID=(1:100000) DEFAULT=4
END-PROC

.TITLE
VICAR program topomap

.HELP
PURPOSE:
To generate relative elevation maps from either a tracker3 MARK
file or a pair of MARSCORR line,sample DISPARITY files.
No knowledge of camera pointing is necessary. Only use this
program if you do not have spice information or camera models.
(See programs LSTOXYZ & TOTOPO if you do.)

EXECUTION:
tracker3 inp=image out=mark
tptedt2 inp=mark out=editedmark  ( optional )
topomap inp=(image,editedmark) out=topo
or
marscorr inp=(left,right) out=(linedisp,sampledisp)
topomap inp=(left,linedisp,sampledisp) out=topo

An output smoothing operation is advisable.

.PAGE

METHOD:
Processing steps are as follows:

1. Determine the epipolar direction from the greatest vector trend.

2. Compute the vector scalar component along the epipolar direction
   for each pixel in the MARK or DISPARITY file
   and place in the output image as a real dn value.

3. For each zero dn pixel (those values between input vectors) collect
   all the non zero pixels within RADIUS and, if the number is MINPTS
   or more, interpolate over the central pixel. Store these values
   until done with this iteration over the entire image.

4. Add the interpolated pixels to the output.

5. Go to step 3 MAXPASS times.

The first input image is only used to get the size of the output.

INTERPOLATION FORMULA:

Given a location (i,j), a set of N dn values DN(x,y), 
and a set of N weights R which are the squares of the distances
between (i,j) and (x,y),
we compute the DN value at (i,j) from:

         DN(x1,y1)    DN(x2,y2)           DN(xN,yN)
         ---------  + ---------  .....  + ---------
         R(x1,y1)     R(x2,y2)            R(xN,yN)
DN(i,j)=-------------------------------------------
             1            1                   1
         ---------  + ---------  .....  + ---------
         R(x1,y1)     R(x2,y2)            R(xN,yN)

HISTORY:
3-2001  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
1. image
2. mark file
or
1. image
2. line disparity
3. sample disparity

.VARI OUT
REAL topo image

.VARI RADIUS
Radius of circle
collecting points.
should be larger
than mark grid size.

.VARI MINPTS
Minimum number of points
within circle to permit
interpolation.

.VARI MAXPASS
Number of passes
through the image.

.LEVEL2
.VARI INP
Either two or three inputs.
1. image
2. mark file
or
1. image
2. line disparity
3. sample disparity

.VARI OUT
REAL topo image.

.VARI RADIUS
Radius of circle within which points are collected to be used to
interpolate over the central pixel (if it is zero).
RADIUS should be greater than the MARK grid size.

.VARI MINPTS
For interpolation to be performed a minimum of MINPTS must be
collected within the circle.

.VARI MAXPASS
Number of passes to be performed through the image. If no points
are interpolated then iteration ceases.

end-proc                                                                     
        
$ Return
$!#############################################################################
$Test_File:
$ create tsttopomap.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
gausnois out=a.img nl=200 ns=200
lgeom inp=a.img out=b.img nah=1 nav=1 tiep=( +
 1,1,1,1, 1,200,5,195, 200,1,195,5, 200,200,200,200)
tracker3 inp=(a.img,b.img) out=mark.img nlw=15 nsw=15 grid=5 +
 nlarea=27 nsarea=27
dvector inp=(a.img,mark.img) out=vector.img
xvd vector.img
topomap inp=(a.img,mark.img) out=dtm.img
xvd dtm.img
! 
!tracker3 inp=(I25-I27.stereo.red.vic,I25-I27.stereo.blu.vic) +
!  out=mark.img nlw=35 nsw=35 nlarea=41 nsarea=41 quality=0.3 +
!  'zero grid=3
!dvector inp=(I25-I27.stereo.red.vic,mark.img) out=vector.img exag=2.
!xvd vector.img
!topomap inp=(I25-I27.stereo.red.vic,mark.img) out=dtm.img
!xvd dtm.img
!
end-proc
$ Return
$!#############################################################################
