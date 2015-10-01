$!****************************************************************************
$!
$! Build proc for MIPL module remnoise
$! VPACK Version 1.9, Monday, December 07, 2009, 16:59:05
$!
$! Execute by entering:		$ @remnoise
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
$ write sys$output "*** module remnoise ***"
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
$ write sys$output "Invalid argument given to remnoise.com file -- ", primary
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
$   if F$SEARCH("remnoise.imake") .nes. ""
$   then
$      vimake remnoise
$      purge remnoise.bld
$   else
$      if F$SEARCH("remnoise.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake remnoise
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @remnoise.bld "STD"
$   else
$      @remnoise.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create remnoise.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack remnoise.com -mixed -
	-s remnoise.f -
	-i remnoise.imake -
	-p remnoise.pdf -
	-t tstremnoise.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create remnoise.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program remnoise
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter(npix=20000)
      integer*4 ounit,def,count_tol,status
      integer*4 count_factor
      real*4 buf1(npix),buf2(npix),buf3(npix),obuf(npix)
      real*4 sdev(npix)
      real*4 border(8)
      character*32 format
      logical roundoff

c set constants
      w=.7072        ! weight for a corner pixel.
      nreset=0       ! # pixels changed
      
c parameters
      call xvparm('TOL',tol_fixed,count_tol,def,1)
      call xvparm('FACTOR',factor,count_factor,def,1)
      call xvpcnt('INP',nids)
            
c open input 1
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','REAL',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      roundoff=.false.
      if(format.eq.'BYTE')roundoff=.true.
      if(format.eq.'HALF')roundoff=.true.
      if(format.eq.'FULL')roundoff=.true.
      if(roundoff)write(*,*)'Integer roundoff correction applied'
      if(ns.gt.npix)then
        write(*,*)'Max number of pixels/line=',npix
        call abend
      endif

c open input 2
      if(nids.gt.1)then
        call xvunit(inunit2,'INP',2,status,' ')
        call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
        bigsig=0.0
        do j=1,nl
          call xvread(inunit2,sdev,status,'LINE',j,' ')
          do i=1,ns
            if(sdev(i).gt.bigsig)bigsig=sdev(i)
          enddo
        enddo
        write(*,*)'largest standard deviation is',bigsig
        scale=ntab/bigsig
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL','OP','WRITE',' ')
      
c line loop
      do j=1,nl
      
c       read picture data
        if(j.eq.1)then
          call xvread(inunit,buf2(2),status,'LINE',1,' ')
          buf2(1)=buf2(2)
          buf2(ns+2)=buf2(ns+1)
          call xvread(inunit,buf3(2),status,'LINE',2,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)
          call mve(7,ns+2,buf2,buf1,1,1)
        else if(j.eq.nl)then
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
        else
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
          call xvread(inunit,buf3(2),status,'LINE',j+1,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)      
        endif
        
c       read standard deviation data
        if(nids.gt.1)call xvread(inunit2,sdev(2),status,'LINE',j,' ') 
                 
c       pixel loop
        do i=2,ns+1

c         interpolate medians around 3 by 3 box border.
c         border(8) border(1) border(5)
c         border(3)           border(4)
c         border(7) border(2) border(6)
          sum=0.0
          sumr=0.0
          call median(buf1(i-1),w,buf1(i),1.,buf1(i+1),w,sum,sumr
     +     ,border(1))
          call median(buf3(i-1),w,buf3(i),1.,buf3(i+1),w,sum,sumr
     +     ,border(2))
          call median(buf1(i-1),w,buf2(i-1),1.,buf3(i-1),w,sum,sumr
     +     ,border(3))
          call median(buf1(i+1),w,buf2(i+1),1.,buf3(i+1),w,sum,sumr
     +     ,border(4))
          call median(buf1(i),1.,buf1(i+1),w,buf2(i+1),1.,sum,sumr
     +     ,border(5))
          call median(buf2(i+1),1.,buf3(i+1),w,buf3(i),1.,sum,sumr
     +     ,border(6))
          call median(buf3(i),1.,buf3(i-1),w,buf2(i-1),1.,sum,sumr
     +     ,border(7))
          call median(buf2(i-1),1.,buf1(i-1),w,buf1(i),1.,sum,sumr
     +     ,border(8)) 
          surface=sum/sumr
          dif=abs(buf2(i)-surface)
                    
c         compute tol          
          if(nids.eq.1)then
            tol=factor*((abs(border(8)-border(6))+
     +                   abs(border(5)-border(7))+
     +                   abs(border(3)-border(4))+
     +                   abs(border(1)-border(2)))/4.0 )+
     +          tol_fixed
          else
            tol=factor*sdev(i) + tol_fixed
          endif
          
c         determine if pixel is changed.
          if(dif.gt.tol)then
            obuf(i)=surface
            nreset=nreset+1
          else
            obuf(i)=buf2(i)
          endif
          
c         roundoff for integer output
          if(roundoff)obuf(i)=float(nint(obuf(i)))
          
        enddo 
        call xvwrit(ounit,obuf(2),status,' ')
      enddo
      
c statistics
      write(*,*)nreset,' pixels reset'
            
      return
      end
      
c************************************************************************
      subroutine median(a,wa,b,wb,c,wc,sum,sumr,med)
c given 3 numbers a,b,c and weights wa,wb,wc 
c return sum=sum+median(a,b,c) and sumr=sumr+corresponding weight.
      real*4 med
      if((b.ge.a).and.(b.le.c))then
        med=b
        sum=sum+b*wb
        sumr=sumr+wb
      else if((b.le.a).and.(b.ge.c))then
        med=b
        sum=sum+b*wb
        sumr=sumr+wb
      else if((a.ge.b).and.(a.le.c))then
        med=a
        sum=sum+a*wa
        sumr=sumr+wa
      else if((a.le.b).and.(a.ge.c))then
        med=a
        sum=sum+a*wa
        sumr=sumr+wa
      else if((c.ge.a).and.(c.le.b))then
        med=c
        sum=sum+c*wc
        sumr=sumr+wc
      else if((c.le.a).and.(c.ge.b))then
        med=c
        sum=sum+c*wc
        sumr=sumr+wc
      else
        write(*,*)'MEDIAN: should not be here'
      endif
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create remnoise.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM remnoise

   To Create the build file give the command:

		$ vimake remnoise			(VMS)
   or
		% vimake remnoise			(Unix)


************************************************************************/


#define PROGRAM	remnoise
#define R2LIB

#define MODULE_LIST remnoise.f

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
$ create remnoise.pdf
process help=*
PARM INP TYPE=STRING COUNT=(1:2)
PARM OUT TYPE=STRING COUNT=1
PARM TOL TYPE=REAL COUNT=(0:1) DEFAULT=30.0
PARM FACTOR TYPE=REAL COUNT=(0:1) DEFAULT=0.5
END-PROC

.TITLE
VICAR program REMTOL

.HELP
PURPOSE:
Remnoise removes small artifacts, like bit errors, from images.
At low error rates it performs the same as ADESPIKE. Performance is superior
to ADESPIKE at bit error rates over one in 500.

EXECUTION:
remnoise inp=image out=cleaned tol=30.0

Note:
The TOL parameter is somewhat sensitive to bit error rate and very sensitive to
the image itself. To tune it take a typical image of interest without noise
and run the following test with RATE=the expected bit error rate:

addnoise inp=image out=a.img gain=30 rate=100
remnoise inp=a.img out=b.img tol=10 factor=.5
f2 inp=(image,b.img) out=c.img function="in1-in2+100"
hist inp=c.img 'nohist

repeatedly changing TOL each time until the standard deviation listed by hist is 
a minimum.

The FACTOR parameter is insensitive to bit error rate.

METHOD:
1. A 3 by 3 pixel box is centered on each pixel of dn J.
2. A median is computed taking each consecutive 3 pixels going around the box.
   This makes 8 median values I.
   I1 I2 I3
   I4    I5
   I6 I7 I8
3. A surface is interpolated using I1-I8
   The model is: M=sum(I/R)/sum(1/R) where R is the distance of I from the
   center (1 or 1.414).
4. An activity measure A is computed from:
   A=(|I1-I8|+|I3-I6|+|I2-I7|+|I4-I5|)/4
5. If abs(J-M) > TOL+FACTOR*A then  J=M

HISTORY:
1-1-99  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
1. Input image.
2. Standard deviation 
   image (optional).

.VARI OUT
output image.

.VARI TOL
Fixed noise threshold.

.VARI FACTOR
Scaling factor for
the activity measure.

.LEVEL2
.VARI INP
1. Input image (required).
2. Standard deviation image (optional).

.VARI OUT
output image with noise removed.

.VARI TOL
Fixed noise threshold term TOL.
Pixels are changed if their dn values differ from the interpolated model
by over TOL+FACTOR*A where A is the activity measure.

.VARI FACTOR
The scale factor used to compute a positionally dependent noise threshold. 
Pixels are changed if their dn values differ from the interpolated model
by over TOL+FACTOR*A where A is the activity measure.
$ Return
$!#############################################################################
$Test_File:
$ create tstremnoise.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
addnoise inp=/project/test_work/testdata/gll/s0349632000.u out=a.img +
 gain=30 rate=100
remnoise inp=a.img out=b.img tol=10 factor=.5
f2 inp=(/project/test_work/testdata/gll/s0349632000.u,b.img) out=c.img +
 function="in1-in2+100"
hist inp=c.img 'nohist
!
end-proc
$ Return
$!#############################################################################
