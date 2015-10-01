$!****************************************************************************
$!
$! Build proc for MIPL module remray
$! VPACK Version 1.9, Monday, December 07, 2009, 16:59:16
$!
$! Execute by entering:		$ @remray
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
$ write sys$output "*** module remray ***"
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
$ write sys$output "Invalid argument given to remray.com file -- ", primary
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
$   if F$SEARCH("remray.imake") .nes. ""
$   then
$      vimake remray
$      purge remray.bld
$   else
$      if F$SEARCH("remray.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake remray
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @remray.bld "STD"
$   else
$      @remray.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create remray.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack remray.com -mixed -
	-s remray.f -
	-i remray.imake -
	-p remray.pdf -
	-t tstremray.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create remray.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program remray
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter(npix=10000)
      integer*4 ounit,def,count,count1,count2,status
      real*4 buf1(npix),buf2(npix),buf3(npix),obuf(npix)
      real*4 med1(npix),med2(npix),med3(npix)
      integer*4 hist(-32768:32767)

c set constants
      nreset=0       ! # pixels changed
      
c parameters
      call xvparm('GAIN1',gain1,count1,def,1)
      call xvparm('GAIN2',gain2,count2,def,1)
      call xvparm('TOL',tolerance,count,def,1)
            
c open input 1
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','REAL',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      if(ns.gt.npix)then
        write(*,*)'Max number of pixels/line=',npix
        call abend
      endif

c open input 2
      call xvunit(inunit2,'INP',2,status,' ')
      call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
      call xvget(inunit2,status,'NL',nl2,'NS',ns2,' ')
      if((ns2.ne.ns).or.(nl2.ne.nl))then
        write(*,*)'Input images are of unequal size'
        call abend
      endif
      
c compute histogram of medians & GAIN1 if defaulted.
      if(count1.eq.0)then
        do i=-32768,32767
          hist(i)=0
        enddo
        do j=1,nl
          call xvread(inunit2,med1,status,'LINE',j,' ')
          do i=1,ns
            k=nint(med1(i))
            hist(k)=hist(k)+1
          enddo
        enddo
        k=0
        n=nl*ns*0.02
        do i=32767,-32768,-1
          k=k+hist(i)
          if(k.ge.n)then
            upper_dn=i
            goto 10
          endif
        enddo
10      continue
        write(*,*)'Upper DN range is about ',upper_dn
        if(upper_dn.eq.0.)upper_dn=1.
        gain1=tolerance/upper_dn
        write(*,*)'GAIN1 reset to ',gain1
      endif
      
c compute histogram of activity & GAIN2 if defaulted.
      if(count2.eq.0)then
        do i=0,32767
          hist(i)=0
        enddo
        do j=1,nl
      
c         read picture data
          call getlines(j,nl,ns,inunit,inunit2,
     +      buf1,buf2,buf3,med1,med2,med3)
       
c         pixel loop
          do i=2,ns+1
            dn_sum=med1(i-1)+med1(i)+med1(i+1)+
     +             med2(i-1)+        med2(i+1)+
     +             med3(i-1)+med3(i)+med3(i+1)
            deviation=abs(8.0*med2(i)-dn_sum)/8.0
            k=nint(deviation*1000.)
            if(k.gt.32767)k=32767
            if(k.lt.-32768)k=-32768
            hist(k)=hist(k)+1
          enddo
        enddo
        k=0
        n=nl*ns*0.02
        do i=32767,0,-1
          k=k+hist(i)
          if(k.ge.n)then
            upper_dn=i/1000.
            goto 11
          endif
        enddo
11      continue
        write(*,*)'Upper activity range is about ',upper_dn
        if(upper_dn.eq.0.)upper_dn=1.
        gain2=tolerance/upper_dn
        write(*,*)'GAIN2 reset to ',gain2
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL','OP','WRITE',' ')
      
c Process data.
      do j=1,nl
      
c       read picture data
        call getlines(j,nl,ns,inunit,inunit2,
     +    buf1,buf2,buf3,med1,med2,med3)
       
c       pixel loop
        do i=2,ns+1

          dn_sum=med1(i-1)+med1(i)+med1(i+1)+
     +           med2(i-1)+        med2(i+1)+
     +           med3(i-1)+med3(i)+med3(i+1)
          deviation=abs(8.0*med2(i)-dn_sum)/8.0
          tol=gain1*med2(i)+gain2*deviation
          
c         determine if pixel is changed.
          if(buf2(i)-med2(i).gt.tol)then
            dn_mean=(dn_sum+med2(i))/9.0
            obuf(i)=dn_mean   ! reset to mean
            nreset=nreset+1
          else
            obuf(i)=buf2(i)   ! no change
          endif
          
        enddo 
        call xvwrit(ounit,obuf(2),status,' ')
        
      enddo
      
c statistics
      write(*,*)nreset,' pixels reset'            
      return
      end
      
c********************************************************************      
      subroutine getlines(j,nl,ns,inunit,inunit2,
     +    buf1,buf2,buf3,med1,med2,med3)
      real*4 buf1(*),buf2(*),buf3(*),med1(*),med2(*),med3(*)
      integer*4 status
c j=picture line
c buf1,2,3 are 3 consecutive lines top down centered on line j
c med1,2,3 are the same but medians.
      
        if(j.eq.1)then
c         picture
          call xvread(inunit,buf2(2),status,'LINE',1,' ')
          buf2(1)=buf2(2)
          buf2(ns+2)=buf2(ns+1)
          call xvread(inunit,buf3(2),status,'LINE',2,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)
          call mve(7,ns+2,buf2,buf1,1,1)
c         median
          call xvread(inunit2,med2(2),status,'LINE',1,' ')
          med2(1)=med2(2)
          med2(ns+2)=med2(ns+1)
          call xvread(inunit2,med3(2),status,'LINE',2,' ')
          med3(1)=med3(2)
          med3(ns+2)=med3(ns+1)
          call mve(7,ns+2,med2,med1,1,1)

        else if(j.eq.nl)then
c         picture
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
c         median
          call mve(7,ns+2,med2,med1,1,1)
          call mve(7,ns+2,med3,med2,1,1)

        else
c         picture
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
          call xvread(inunit,buf3(2),status,'LINE',j+1,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)
c         median
          call mve(7,ns+2,med2,med1,1,1)
          call mve(7,ns+2,med3,med2,1,1)
          call xvread(inunit2,med3(2),status,'LINE',j+1,' ')
          med3(1)=med3(2)
          med3(ns+2)=med3(ns+1)
            
        endif
        return
        end
      
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create remray.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM remray

   To Create the build file give the command:

		$ vimake remray			(VMS)
   or
		% vimake remray			(Unix)


************************************************************************/


#define PROGRAM	remray
#define R2LIB

#define MODULE_LIST remray.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define DEBUG
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create remray.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=1
PARM TOL TYPE=REAL COUNT=(0:1) DEFAULT=20
PARM GAIN1 TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM GAIN2 TYPE=REAL COUNT=(0:1) DEFAULT=--
END-PROC

.TITLE
VICAR program REMRAY

.HELP
PURPOSE:
Remray removes small artifacts such as cosmic ray or high energy charged 
particle tracks from images. The artifacts are always assumed to be brighter 
than the true image intensities.
Remray can remove small clusters of pixels which Remnoise cannot handle.

EXECUTION:
median inp=image out=median nlw=5 nsw=5
remray inp=(image,median) out=smooth tol=20

METHOD:
An input image has dn values of DN.
A model for the background is taken from the median image.
An activity measure is computed from the medians as the absolute value of
a derivative filter whose kernel is
-1 -1 -1
-1  8 -1
-1 -1 -1 
If DN-median exceeds GAIN1*(the median) + GAIN2*(the activity)
the pixel is reset to the median average over a 3 by 3 area..

The algorithm is as follows for each pixel:
1. Select a 3 by 3 window of DN(i,j).
2. Subtract the median window MED(i,j)
   HP(i,j)=DN(i,j)-MED(i,j)
3. Compute an activity measure A from:
the absolute value of a derivative filter whose kernel is
-1 -1 -1
-1  8 -1
-1 -1 -1 
6. If(HP(2,2) > GAIN1*MED(2,2) + GAIN2*A) then 
    DN(2,2)= the 3 by 3 median average.
   ( the (2,2) refers to the central pixel in the 3 by 3 window).

If the GAIN1 and GAIN2 factors are defaulted they are computed from TOL. To do
this remray computes histograms of the median intensity and the activity and
selects the upper few percent values. Then..
GAIN1=TOL/(max dn of medians)
GAIN2=TOL/(max activity of medians).


HISTORY:
3-1-99  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
1. Input image.
2. Median image
   nlw=nsw=3 or
   nlw=nsw=5 or
   whatever works. 

.VARI OUT
output image.

.VARI TOL
Tolerance threshold.

.VARI GAIN1
Intensity gain factor.
(default is to compute
it from TOL.)

.VARI GAIN2
Activity gain factor.
(default is to compute
it from TOL.)

.LEVEL2
.VARI INP
1. Input image.
2. Median image generated with a 3 by 3 or a 5 by 5 filter.
   The filter size should exceed the artifact size by a factor of 2.

.VARI OUT
output image with artifacts removed.

.VARI TOL
Tolerance threshold. Defaults to 20.
Used to automatically set the GAIN1 and GAIN2 keywords if they are not 
specified.

.VARI GAIN1
Intensity gain factor. 
Used to compute a threshold proportional to the local median.
The default is to compute it from TOL and the maximum dn in the image:
GAIN1=TOL/(max dn of medians)
Pixels are changed if their median values exceed  a total 
threshold composed of
GAIN1*median + GAIN2*activity.

.VARI GAIN2
Activity gain factor.
Used to compute a threshold proportional to the unevenness of the local scene.
The default is to compute it from TOL and the maximum activity in the image:
GAIN2=TOL/(max activity of medians).
Activity is the absolute value of a derivative filter:
-1 -1 -1
-1  8 -1
-1 -1 -1
Pixels are changed if their median values exceed  a total 
threshold composed of
GAIN1*median + GAIN2*activity.
$ Return
$!#############################################################################
$Test_File:
$ create tstremray.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
median inp=/project/test_work/testdata/sitod1/test_data/images/moon1.img +
 out=med1.img nlw=5 nsw=5
remray inp=( +
 /project/test_work/testdata/sitod1/test_data/images/moon1.img,med1.img) +
 out=a.img tol=30
f2 inp=(/project/test_work/testdata/sitod1/test_data/images/moon1.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/europa.img +
 out=med2.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/europa.img,med2.img) out=a.img tol=10
f2 inp=(/home/gmy/rad/europa.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/amalthea.img out=med3.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/amalthea.img,med3.img) out=a.img tol=20
f2 inp=(/home/gmy/rad/amalthea.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/europa2.img out=med3.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/europa2.img,med3.img) out=a.img tol=20
f2 inp=(/home/gmy/rad/europa2.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/europa3.img out=med3.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/europa3.img,med3.img) out=a.img tol=20
f2 inp=(/home/gmy/rad/europa3.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/io.img out=med3.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/io.img,med3.img) out=a.img tol=20
f2 inp=(/home/gmy/rad/io.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/io2.img out=med3.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/io2.img,med3.img) out=a.img tol=20
f2 inp=(/home/gmy/rad/io2.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img
!
median inp=/home/gmy/rad/jring.img out=med3.img nlw=5 nsw=5
remray inp=(/home/gmy/rad/jring.img,med3.img) out=a.img tol=20
f2 inp=(/home/gmy/rad/jring.img, +
 a.img) out=b.img function="(in1-in2)*40+128"
xvd b.img
xvd a.img 
!
end-proc
$ Return
$!#############################################################################
