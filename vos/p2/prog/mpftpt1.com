$!****************************************************************************
$!
$! Build proc for MIPL module mpftpt1
$! VPACK Version 1.9, Tuesday, May 01, 2012, 14:32:10
$!
$! Execute by entering:		$ @mpftpt1
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
$ write sys$output "*** module mpftpt1 ***"
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
$ write sys$output "Invalid argument given to mpftpt1.com file -- ", primary
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
$   if F$SEARCH("mpftpt1.imake") .nes. ""
$   then
$      vimake mpftpt1
$      purge mpftpt1.bld
$   else
$      if F$SEARCH("mpftpt1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mpftpt1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mpftpt1.bld "STD"
$   else
$      @mpftpt1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mpftpt1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mpftpt1.com -mixed -
	-s mpftpt1.f -
	-i mpftpt1.imake -
	-p mpftpt1.pdf -
	-t tstmpftpt1.pdf tstmpftpt1.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mpftpt1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program mpftpt1
c
      include 'VICMAIN_FOR'
      subroutine main44

      integer maxns
      parameter (maxns=5000,maxnl=15)

      integer*4 ounit(3),def,count
      integer*4 unit(2),status,nl,ns,search,template,shift
      real*4 left(maxns,maxnl),right(maxns,maxnl)
      real*4 cl(maxns),cs(maxns)
      real*4 sum(maxns),qualcor(maxns)
c      real*4 qualcor2(maxns),cs2(maxns),cl2(maxns)
      real*8 sumxy,sumx,sumy,sumx2,sumy2,d1,d2

c parameters
      call xvparm('TEMPLATE',template,count,def,1)
      if((template/2)*2.eq.template)template=template+1
      call xvparm('SEARCH',search,count,def,1)
      if((search/2)*2.eq.search)search=search+1
      call xvparm('SHIFT',shift,count,def,1)
      call xvparm('QUALITY',quality,count,def,1)
      call xvparm('NLW',nlw,count,def,1)
      if((nlw/2)*2.eq.nlw)nlw=nlw+1
      nlw2=nlw/2

      call xveaction('SA',' ')
      if(search.le.template)then
        call xvmessage("search must be > template"," ")
        call abend
      endif

      template1=template
      search1=search
      template2=(2*template)/3
      search2=search
      if((template2/2)*2.eq.template2)template2=template2+1
      if((search2/2)*2.eq.search2)search2=search2+1

c open all inputs
      do i=1,2
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.maxns)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
      enddo

c open all outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE',' ')
      enddo

      do line=1,nl                         ! line loop

        loop=0
100     loop=loop+1

c process images

        i_left=(search-template)/2 + 1
        i_right=ns-(search-template)/2-template+1
        nconvolution=search-template+1
        noffset=(search-template)/2+template/2
        nof=template/2
      
c       Read blocks of nlw lines
        lineright=line+shift
        do k=1,nlw
          kline=line-nlw2+k-1
          if(kline.lt.1)kline=1
          if(kline.gt.nl)kline=nl
          call xvread(unit(1),left(1,k),status,'LINE',kline,' ')
        enddo
        do k=1,nlw
          kline=line+shift-nlw2+k-1
          if(kline.lt.1)kline=1
          if(kline.gt.nl)kline=nl
          call xvread(unit(2),right(1,k),status,'LINE',kline,' ')
        enddo
        
        m=0
        do i=i_left,i_right                     ! pixel loop
        
c         pre compute template statistics
          sumx=0.d0
          sumx2=0.d0
          do kk=1,nlw
            do k=1,template
              sumx=sumx+left(i+k-1,kk)
              sumx2=sumx2+left(i+k-1,kk)*left(i+k-1,kk)
            enddo
          enddo
          m=m+1
          
          do j=1,nconvolution
            sumxy=0.d0
            sumy=0.d0
            sumy2=0.d0
            do kk=1,nlw
              do k=1,template
                sumy=sumy+right(j+m+k-2,kk)
                sumxy=sumxy+left(i+k-1,kk)*right(j+m+k-2,kk)
                sumy2=sumy2+right(j+m+k-2,kk)*right(j+m+k-2,kk)
              enddo
            enddo
            d1=sumx2-sumx*sumx/(template*nlw)
            d2=sumy2-sumy*sumy/(template*nlw)
            if(d1*d2.ne.0.0)then
              sum(j)=(sumxy-sumx*sumy/(template*nlw))**2/(d1*d2)
            else
              sum(j)=0.0
            endif
          enddo
          
          k=1
          do j=2,nconvolution
            if(sum(j).gt.sum(k))k=j
          enddo
          if(sum(k).gt.quality)then
            cs(m+noffset)=m+k+nof-1
            cl(m+noffset)=lineright
            qualcor(m+noffset)=sum(k)
          else
            cs(m+noffset)=0.0
            cl(m+noffset)=0.0
            qualcor(m+noffset)=0.0
          endif
        enddo   ! pixel loop

        do i=1,noffset ! fill in ends of line
          cl(i)=0.0  ! empty
          cs(i)=0.0  ! empty
          cl(ns-i+1)=0.0  ! empty
          cs(ns-i+1)=0.0  ! empty
        enddo

c        if(loop.eq.1)then
c          do i=1,ns
c            cs2(i)=cs(i)
c            cl2(i)=cl(i)
c            qualcor2(i)=qualcor(i)
c          enddo
c          template=template2
c          search=search2
c          goto 100
c        else
c          do i=1,ns
c            if((qualcor(i).eq.0.).or.
c     +         (qualcor2(i).eq.0.).or.
c     +         (abs(cs(i)-cs2(i)).gt.2.1))then
c              cs(i)=0.
c              cl(i)=0.
c              qualcor(i)=0.
c            endif
c          enddo
c          template=template1
c          search=search1
c        endif

        call xvwrit(ounit(1),cl,status,' ')
        call xvwrit(ounit(2),cs,status,' ')
        call xvwrit(ounit(3),qualcor,status,' ')
      enddo     ! line loop

      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mpftpt1.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mpftpt1

   To Create the build file give the command:

		$ vimake mpftpt1			(VMS)
   or
		% vimake mpftpt1			(Unix)


************************************************************************/


#define PROGRAM	mpftpt1
#define R2LIB

#define MODULE_LIST mpftpt1.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create mpftpt1.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=3
PARM NLW TYPE=INTEGER COUNT=(0:1) VALID=(1:15) DEFAULT=9
PARM TEMPLATE TYPE=INTEGER COUNT=(0:1) DEFAULT=23
PARM SEARCH TYPE=INTEGER COUNT=(0:1) DEFAULT=95
PARM SHIFT TYPE=INTEGER COUNT=(0:1) VALID=(0:10) DEFAULT=0
PARM QUALITY TYPE=REAL COUNT=(0:1) VALID=(0.:1.) DEFAULT=.5
END-PROC

.TITLE
VICAR program MPFTPT1

.HELP
PURPOSE:
To compute, from a stereo pair, the line and sample disparity
of every pixel in the scene.
This program only computes horizontal disparity by 1-d correlations using
many lines taken together.

EXECUTION:
mpftpt1 inp=(left,right) out=(line_disparity,sample_disparity,quality) parameters
where:
left is the left eye image of a stereo pair
right is the corresponding right eye image of a stereo pair
line_disparity is the right eye line value.
sample_disparity is the right eye sample value.
quality is the correlation quality.

.PAGE

METHOD:
MPFTPT1 performs a fast 1-d correlation between the left and the right
eye images using NLW lines at a time.
Note: correlations are only computed on integral pixel boundaries.

Output file contents:
 
All output files are in the coordinates of the first (left eye) image.
For example if output file #2 has a value of 56.67 at sample 50 it means
that the sample location of a tiepoint located at sample 50 in the left
eye image corresponds to sample 56.67 in the right eye image.
 
First output: A REAL image containing the line disparity. This is the line in
the second input (right eye) which corresponds to the line in the first
input image (left eye). This program generates a line disparity which is
always zero (unless the SHIFT keyword is used).
If both left & right disparity values are 0.000 the point has no value.
 
Second output: A REAL image containing the sample disparity. This is the sample
in the second input (right eye) which corresponds to the sample in the first
input image (left eye).
If both left & right disparity values are 0.000 the point has no value.

HISTORY:
6-30-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
2 input images

.VARI OUT
3 Output images

.VARI NLW
correlation size
 height

.VARI TEMPLATE
correlation size
width
 
.VARI SEARCH
correlation search
area

.VARI SHIFT
Line shift of
right image.
- is down.

.VARI QUALITY
Minimum acceptable
correlation quality

.LEVEL2

.VARI INP
First left eye image, then right eye image.
 
.VARI OUT
First output: A REAL image containing the line disparity. This is the line in
the second input (right eye) which corresponds to the line in the first
input image (left eye).
Second output: A REAL image containing the sample disparity. This is the sample
in the second input (right eye) which corresponds to the sample in the first
input image (left eye).
Third output: A REAL image containing the normalized quality, 0 to 1.

.VARI NLW
Correlation size height. An odd number.
The number of lines correlated at one time.
Defaults to 9.

.VARI TEMPLATE
Correlation size. Odd number. Defaults to 23.
 
.VARI SEARCH
Correlation search area. Odd number. Defaults to 95.
SEARCH > TEMPLATE.

.VARI SHIFT
Line displacement of right image before correlation.
- is down, + is up. 
A shift were +1, means that the right image is shifted up one line to match
the left image before correlation begins.
Default shift is zero.

.VARI QUALITY
Minimum acceptable correlation quality. Zero to 1.0 .
Defaults to 0.5
$ Return
$!#############################################################################
$Test_File:
$ create tstmpftpt1.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! use MER Pancam images (even though the pgm was written for MPF):
mpftpt1 inp=(/project/test_work/testdata/mer/l.img +
 /project/test_work/testdata/mer/r.img) +
 out=(ld.img,sd.img,q.img) quality=0.5

! show distribution of disparities and quality:
hist ld.img nlin=21 lim=(0 1000)
hist sd.img nlin=21 lim=(0 1000)
hist q.img nlin=21 lim=(0 1)

! this was the test that JJL had:
!xvd l.img
!xvd r.img
!mpftpt1 inp=(l.img,r.img) out=(ld.img,sd.img,q.img) quality=0.5
!xvd sd.img
!xvd q.img

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstmpftpt1.log_solos
tstmpftpt1
mpftpt1 inp=(/project/test_work/testdata/mer/l.img  +
 /project/test_work/testdata/mer/r.img)  +
 out=(ld.img,sd.img,q.img) quality=0.5
Beginning VICAR task mpftpt1
hist ld.img nlin=21 lim=(0 1000)
Beginning VICAR task hist
HIST version 27-JUL-11

          0  883065    **************************************************  1
         50    6855    *************
        100   10981    **********************
        150   19819    ****************************************
        200   20133    ****************************************
        250   21430    *******************************************
        300   22827    **********************************************
        350   24680    **************************************************  2
        400   20496    *****************************************
        450    9595    *******************
        500    2150    ****
        550     138
        600     231
        650     118
        700      12
        750     398
        800     301
        850     739    *
        900    1218    **
        950    1091    **
       1000    2299    ****

AVERAGE GRAY LEVEL=46.42300
STANDARD DEVIATION=125.9016
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=1015.000

hist sd.img nlin=21 lim=(0 1000)
Beginning VICAR task hist
HIST version 27-JUL-11

          0  881829    **************************************************  1
        100*   3475    **************
        150   10409    *******************************************
        200    9566    ***************************************
        250   11208    **********************************************
        300   11452    ***********************************************
        350   12017    **************************************************  2
        400   10045    *****************************************
        450   10508    *******************************************
        500   10832    *********************************************
        550    9860    *****************************************
        600    9558    ***************************************
        650    9904    *****************************************
        700   11480    ***********************************************
        750   11765    ************************************************
        800    9915    *****************************************
        850   10113    ******************************************
        900    4640    *******************

AVERAGE GRAY LEVEL=79.61821
STANDARD DEVIATION=204.3170
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=917.0000

hist q.img nlin=21 lim=(0 1)
Beginning VICAR task hist
HIST version 27-JUL-11

       0.00  881829    **************************************************  1
       0.50*  18639    **************************
       0.55   35517    **************************************************  2
       0.60   32403    *********************************************
       0.65   26830    *************************************
       0.70   19161    **************************
       0.75   12814    ******************
       0.80    8389    ***********
       0.85    5178    *******
       0.90    3624    *****
       0.95    4130    *****
       1.00      62

AVERAGE GRAY LEVEL=0.102234
STANDARD DEVIATION=0.239064
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=0.982524

let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
