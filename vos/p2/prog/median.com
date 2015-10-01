$!****************************************************************************
$!
$! Build proc for MIPL module median
$! VPACK Version 1.9, Friday, August 19, 2005, 16:22:05
$!
$! Execute by entering:		$ @median
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
$ write sys$output "*** module median ***"
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
$ write sys$output "Invalid argument given to median.com file -- ", primary
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
$   if F$SEARCH("median.imake") .nes. ""
$   then
$      vimake median
$      purge median.bld
$   else
$      if F$SEARCH("median.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake median
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @median.bld "STD"
$   else
$      @median.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create median.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack median.com -mixed -
	-s median.f -
	-i median.imake -
	-p median.pdf -
	-t tstmedian.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create median.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c VICAR program MEDIAN
c
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit none
      common/c1/iunit,ounit,sl,ss,mindn,maxdn
      integer*4 iunit,ounit,sl,ss,mindn,maxdn

      integer*4 nl,ns,nlin,nsin,nlw,nsw,nbuf
      integer*4 n1,n2,status,cnt
      character*10 fmt
      external mainmed

      call xvmessage('MEDIAN Aug 18, 2005 GMY',' ')
      call xvunit(iunit,'inp',1,status,' ')
      call xvopen(iunit,status,'open_act','sa','io_act','sa',
     +            'u_format','half',' ')
      call xvsize(sl,ss,nl,ns,nlin,nsin)
      if (ns.lt.2 .or. nl.lt.2) 
     1	 call mabend(' median cannot handle a file with nl ' //
     +		 'or ns less than 2.')
      call xvget(iunit,status,'format',fmt,' ')
      call uprcase(fmt)
      if (fmt.eq.'BYTE') then
         mindn = 0
         maxdn = 255
      elseif (fmt.eq.'HALF' .or. fmt.eq.'WORD') then
         mindn = -32768
         maxdn = 32767
      else
         call mabend('***Illegal data format')
      endif

      call xvunit(ounit,'out',1,status,' ')
      call xvopen(ounit,status,'op','write','open_act','sa',
     +          'io_act','sa','u_format','half',' ')

      call xvp('nlw',nlw,cnt)
      call xvp('nsw',nsw,cnt)
      if (nlw.gt.nl) nlw=nl
      if (nsw.gt.ns) nsw=ns
      nlw = 2*(nlw/2) + 1		!force window to be odd
      nsw = 2*(nsw/2) + 1
      nbuf = ns + nsw - 1

      n1 = 2*nlw*nbuf		!integer*2 img(nbuf,nlw)
      n2 = 2*ns			!integer*2 out(ns)
      call stacka(9,mainmed,2,n1,n2,nl,ns,nlw,nsw,nbuf)
      call xvclose(iunit,status,' ')
      call xvclose(ounit,status,' ')
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine mainmed(img,n1,out,n2,nl,ns,nlw,nsw,nbuf)
      implicit none
      integer*4 n1,n2,nl,ns,nlw,nsw,nbuf
      integer*2 img(nbuf,nlw),out(ns)

      common/c1/iunit,ounit,sl,ss,mindn,maxdn
      integer*4 iunit,ounit,sl,ss,mindn,maxdn

      integer*4 status,cnt,i,i1,i2,j,j1,j2,line,oline
      integer*4 nlw2,nsw2,n50,dclev,minval,maxval
      real*4 perc,dctran
      logical xvptst,high

      minval = mindn
      maxval = maxdn
      call xvp('PERCENT',perc,cnt)
      n50 = nsw*nlw*perc/100
      call xvp('DCTRAN',dctran,cnt)
      high = xvptst('HIGHPASS')
      call xvp('DCLEVEL',dclev,cnt)

      nlw2 = nlw/2
      nsw2 = nsw/2
      j = nlw2 + 1		!index to first line
      i1 = nsw2 + 1		!index to first and
      i2 = i1 + ns - 1		!last pixel of line

c     ...read initial lines into memory, reflecting at left and right margins
      do 20 line=1,nlw2+1
      call xvread(iunit,img(i1,j),status,'nsamps',ns,'samp',ss,
     &                'line',sl+line-1,' ')
      if (nsw.gt.1) then
         do i=1,nsw2
            img(i1-i,j)=img(i1+i,j)
            img(i2+i,j)=img(i2-i,j)
         enddo
      endif
   20 j = j + 1

      j1 = nlw2
      j2 = nlw2 + 2
c     ...reflect lines at top margin 
      do line=1,nlw2
         call mve(2,nbuf,img(1,j2),img(1,j1),1,1)
         j1 = j1 - 1
         j2 = j2 + 1
      enddo

      j = nlw2 + 1		!index to current line (middle of window)
      j1 = nlw			!index to bottom line of window
      line = nlw2 + 1		!image line number at bottom of window

      do 30 oline=1,nl
      call med2d(img,out,ns,nbuf,nlw,nsw,n50,mindn,maxdn,minval,maxval)
      if (high) call hp(img(i1,j),out,ns,dclev,dctran)
      call xvwrit(ounit,out,status,' ')
      j = mod(j,nlw) + 1
      j1 = mod(j1,nlw) + 1
      line = line + 1
      if (line.le.nl) then
         call xvread(iunit,img(i1,j1),status,'nsamps',ns,'samp',ss,
     &               'line',sl+line-1,' ')
         if (nsw.gt.1) then
            do i=1,nsw2
               img(i1-i,j1)=img(i1+i,j1)
               img(i2+i,j1)=img(i2-i,j1)
            enddo
         endif
      else		!reflect lines at bottom of image
          if (oline.eq.nl) goto 30
          if (line.eq.nl+1) then
             j2 = j1 - 2
          else
             j2 = j2 - 1
          endif
          if (j2.lt.1) j2=j2+nlw
          call mve(2,nbuf,img(1,j2),img(1,j1),1,1)
      endif
   30 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c compute median filter for an image line.
c
      subroutine med2d(img,out,ns,nbuf,nlw,nsw,n50,mindn,maxdn,
     &		minval,maxval)
      implicit none
      integer*4 ns,nbuf,nlw,nsw,n50,mindn,maxdn,minval,maxval
      integer*2 img(nbuf,nlw),out(ns)

      integer*4 hist(-32768:32767)
      integer med,ltm,dn
      integer*4 i,il,is

      do i=minval,maxval
         hist(i)=0
      enddo

      minval = maxdn
      maxval = mindn
      do il=1,nlw
         do is=1,nsw
            dn = img(is,il)
            if (dn.lt.minval) then
               minval=dn
            elseif (dn.gt.maxval) then
               maxval=dn
            endif
            hist(dn)=hist(dn)+1
         enddo
      enddo

      med = minval
      ltm = 0		!number of samples less than median

      do 50 is=1,ns
      if (ltm.le.n50) goto 15
   10 med = med - 1
      ltm = ltm - hist(med)
      if (ltm.gt.n50) goto 10
      goto 20

   15 ltm = ltm + hist(med)
      med = med + 1
      if (ltm.le.n50) goto 15
      med = med - 1
      ltm = ltm - hist(med)

   20 out(is) = med
      if (is.eq.ns) goto 50

      do 30 il=1,nlw
      dn = img(is,il)
      if (dn.lt.med) ltm=ltm-1
      hist(dn) = hist(dn) - 1
      dn = img(is+nsw,il)
      if (dn.lt.minval) then
         minval=dn
      elseif (dn.gt.maxval) then
         maxval=dn
      endif
      if (dn.lt.med) ltm=ltm+1
      hist(dn) = hist(dn) + 1
   30 continue

   50 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c perform high pass filtering on line
c
      subroutine hp(in,out,ns,dclev,dctran)
      implicit none
      integer*4 ns,dclev
      integer*2 in(ns),out(ns)
      real*4 dctran

      integer i,a

      do i=1,ns				!halfword input & output
         a = in(i) - out(i) + dclev	
         if (dctran.ne.0.0) a=a+dctran*float(out(i))
        out(i) = a
      enddo
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create median.imake
#define  PROGRAM   median
#define R2LIB 

#define MODULE_LIST median.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create median.pdf
process help=*
PARM INP	TYPE=STRING  COUNT=1
PARM OUT	TYPE=STRING  COUNT=1				DEFAULT=MEDIAN
PARM SIZE	TYPE=INTEGER COUNT=0:4				DEFAULT=--
PARM NL		TYPE=INTEGER COUNT=0:1				DEFAULT=--
PARM NS         TYPE=INTEGER COUNT=0:1				DEFAULT=--
PARM NLW        TYPE=INTEGER VALID=(1:999)			DEFAULT=11
PARM NSW        TYPE=INTEGER VALID=(1:999)			DEFAULT=11
PARM HIGHPASS	TYPE=KEYWORD VALID=HIGHPASS COUNT=(0:1)		DEFAULT =--
PARM DCLEVEL	TYPE=INTEGER COUNT=0:1 VALID=(0:32767)		DEFAULT=128
PARM PERCENT	TYPE=REAL    VALID=(0.:100.)			DEFAULT=50.0
PARM DCTRAN     TYPE=REAL    COUNT=0:1 VALID=(0.:1.)		DEFAULT=0.0
END-PROC
.TITLE
VICAR1 program MEDIAN: Spatial median filter
.HELP
PURPOSE:

MEDIAN is a VICAR applications program which performs nonlinear spatial
filtering of an image based upon the local (rectangular window) median of
the input. Output may be in either lowpass or highpass form.

.PAGE
EXECUTION:

	median inp=ipic out=opic
where
  ipic is the input image
  opic is the output image

ipic may be in byte or halfword data format.  opic is output in the same
format.  The input image must be at least 2 lines and 2 samples in size.
There is no upper limit for the image size.

Reference:
  Huang, Yang, and Tang, "A Fast Two-Dimensional Median Filtering Algorithm,"
	IEEE Trans., Vol. ASSP-27, No. 1, February 1979.

.PAGE
OPERATION:

MEDIAN replaces each input pixel by the local median value of an NLW X NSW
window centered at that pixel (see NLW and NSW parameters).

The median is defined as the DN value for which half of the pixels in the
window have a lower value and half have a higher value.  That is, if the
pixels in the window were sorted by DN value, then the median would be the
DN of the pixel exactly half way in the sorted list.  The sorting is performed
by accumulating a histogram of the window (see referenced paper).

The output value can be more generally defined using the PERCENT parameter.
If PERCENT=P, then the output value is that DN for which P*NLW*NSW/100 pixels
in the window have a higher value.  For example, if PERCENT=50 (the default),
then the output is the median.

The effect of replacing each pixel by the local median is to remove all high
frequency scene information.  Thus the median filter functions like a low
pass filter.  See also program BOXFLT2.

If HIGHPASS is specified, the final output is given as:

		OUT = IN - MEDIAN + DCLEVEL
where
	DCLEVEL is an integer value (see DCLEVEL parameter).

The effect of the HIGHPASS keyword is to convert the program into a high pass
filter.

.PAGE
EXAMPLES:

	1) MEDIAN INP=A OUT=B NLW=5 NSW=7

		This example performs the lowpass median filter of size 5 lines
		by 7 samples.

	2) MEDIAN INP=A OUT=B 'HIGH NLW=3 NSW=3

		This example performs the highpass median filter of size
		3x3 pixels.

	3) MEDIAN INP=A OUT=B PERCENT=20 NLW=7 NSW=9

		This example outputs a value corresponding to the 20% level
 		of a histogram generated by a 7 line by 9 sample filter. The
		output is then the 12th lowest value in the local window
		rather than the 31st lowest value (50% level).


.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: W. D. Benton, 27 November 1978
REWRITTEN WITH FASTER ALGORITHM BY:  H. J. Frieden,  22 July 1980
CONVERTED TO VAX BY:  Helen De Rueda,  30 Nov. 1983
COGNIZANT PROGRAMMER:  Jean Lorre

Revisions:
 18 Aug 05  G Yagi	Fix DCLEVEL parameter on Linux.

.LEVEL1
.VARI INP
Required string
Input image
.VARI OUT
Required string
Output image
.VARI SIZE
Optional 4 integers
VICAR size field,
SIZE=(SL,SS,NL,NS)
.VARI NL
Optional integer
number of output lines
.VARI NS
Optional integer
number of output samples
output file.
.VARI NLW
Optional integer
filter kernel height (lines)
.VARI NSW
filter kernel width (samples)
.VARI HIGHPASS
Optional keyword
output highpass image
.VARI DCLEVEL
Optional integer
DN offset added to output
.VARI PERCENT
Optional REAL
percent of data greater than
output (=50 for median)
.VARI DCTRAN
Optional REAL
DCTRAN*Local Median is
added to the highpass output.

.LEVEL2
.VARI INP
  INP=ipic
where ipic is the input image.  ipic may be in byte or halfword data format.
ipic must be at least 2 lines and 2 samples in size.  ipic may be arbitrarily
large.

.VARI OUT
  OUT=opic
where opic is the output image.  opic will have the same data format as the
input image.

.VARI SIZE
4 INTEGERS - SIZE=(SL,SS,NL,NS) where SL is the starting line, SS is the
 starting sample, NL is the number of lines in the input dataset and NS
 is the number of samples in the input dataset. (SIZE is usually defined
 as SIZE=(1,1,NL,NS)). Default is taken from the VICAR label within the
 program.
.VARI NL
INTEGER - NL=N1 where is N1 is the number of lines in the input dataset.
.VARI NS
INTEGER - NS=N1 where is N1 is the number of samples in the input dataset.
.VARI NLW
INTEGER - NLW=I1 where I1 is an integer and specifies the size of the filter
 kernel in lines. Default is NLW=11.
.VARI NSW
INTEGER - NSW=I2 where I2 is an integer and specifies the size of the filter
 kernel in samples. Default is NSW=11.
.VARI HIGHPASS
KEYWORD - Valid:('HIGH) 'HIGHPASS specifies that the output is to be in
 highpass format, i.e., the input minus the local median value. Default
 is the lowpass mode.
.VARI DCLEVEL
INTEGER - DCLEVEL=I3 where I3 is an integer and
 specifies the offset to be added to the highpass output. Default is
 DCLEVEL=128.
.VARI PERCENT
REAL - PERCENT=R4 where R4 is an floating point number and specifies the
 percentage of the window size which must be satisfied for an output value
 to be found at each point. The default of PERCENT=50.0 specifies that the
 true median value of window is to be found, whereas if a lower (or higher)
 value is specified, the output value will be somewhat lower (or higher)
 than the actual median.
.VARI DCTRAN
REAL - DCTRAN=R where R is a number such that 0.0<=R<=1.0. 
DCTRAN * output(local median value) is added to the highpass output.
Default is DCTRAN=0.0
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmedian.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!This is a test file for MEDIAN
!This example performs the lowpass median filter of default size 3 lines
!by 3 samples.
gausnois out=a.img nl=10 ns=10 seed=7382382
list a.img
median inp=a.img out=b.img nlw=3 nsw=3
list b.img
!This example performs the highpass median filter of default size 3 lines
!by 3 samples.
median inp=a.img out=b.img 'high nlw=3 nsw=3 dclevel=100. dctran=0.
list b.img
!This example outputs the value corresponding to the 20% level of a 
!histogram
median inp=a.img out=b.img perc=20.0 nlw=5 nsw=5
list b.img
gausnois out=a.img nl=10 ns=10 seed=7382382 format=half sigma=10. +
  mean=-1000
list a.img
median inp=a.img out=b.img nlw=3 nsw=3
list b.img
!This example performs the highpass median filter of default size 3 lines
!by 3 samples.
median inp=a.img out=b.img 'high nlw=3 nsw=3 dclevel=100. dctran=0.
list b.img
!This example outputs the value corresponding to the 20% level of a 
!histogram
median inp=a.img out=b.img perc=20.0 nlw=5 nsw=5
list b.img
end-proc
$ Return
$!#############################################################################
