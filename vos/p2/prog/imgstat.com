$!****************************************************************************
$!
$! Build proc for MIPL module imgstat
$! VPACK Version 1.9, Wednesday, December 19, 2012, 14:07:58
$!
$! Execute by entering:		$ @imgstat
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
$ write sys$output "*** module imgstat ***"
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
$ write sys$output "Invalid argument given to imgstat.com file -- ", primary
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
$   if F$SEARCH("imgstat.imake") .nes. ""
$   then
$      vimake imgstat
$      purge imgstat.bld
$   else
$      if F$SEARCH("imgstat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake imgstat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @imgstat.bld "STD"
$   else
$      @imgstat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create imgstat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack imgstat.com -mixed -
	-s imgstat.f -
	-p imgstat.pdf -
	-i imgstat.imake -
	-t tstimgstat.pdf tstimgstat.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create imgstat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR PROGRAM IMGSTAT
C
C-------------------------------------------------------------------
C Edit History:
c
C        BAM 2/95
c        BAM 3/96
c        BAM 10/96
C        NDR 1/97  -- Added Slope deviation output options.
c	 RJB 4-13-2011 - internals to real*4
c	TBD: This may need a x and y scaling feature for slopedev.
c
C-------------------------------------------------------------------


	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	implicit none  !Dont use implicit integer
	integer maxin,maxout,maxwind
	parameter (MAXIN=14000,MAXOUT=MAXIN/2,MAXWIND=36) !ndr 3/96

c        integer*2 databuf(MAXIN*MAXWIND)
c        integer*2 windowbuf(MAXWIND*MAXWIND)
c        integer*2 out1(MAXOUT), out2(MAXOUT), out3(MAXOUT), out4(MAXOUT)
        real*4    sout(MAXOUT) ! for MSSD mode
        integer*4 nli,nsi,cur_line,ninput,dcode
	integer*4 imin,imax,ipt,mm,begin,ist
	integer*4 i,j,k,l,m,status
	integer*4 nl,ns,nlw,nodset,icount
	integer*4 sl,ss,lines,samps,windowsize
	integer*4 iunit(3),out(5)
	real*4    dxbuf(maxout),dybuf(maxout),dx,dy
	real*4    databuf(MAXIN*MAXWIND)
	real*4    out1(MAXOUT), out2(MAXOUT), out3(MAXOUT), out4(MAXOUT)
	real*4    windowbuf(MAXWIND*MAXWIND)
	real*4 	  cmin,cmax
        integer*4 nn,next,icode
        
        real*8 mean, sd, sum, sldev, slerr, center
        logical*4 XVPTST,KMIN,KMAX,KMEAN,KSD,KMSSD
        integer outs(5)

	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org

	call xvmessage ('IMGSTAT version 13-Apr-2011',' ')
! PARAMETER PROCESSING 

        call xvpcnt ( 'INP',  ninput )  ! the input data sets
! Note all inputs must be of same format
	call xvunit(iunit(1),'INP',1,status,' ')
        call xvopen(iunit(1),status,'OPEN_ACT','SA','IO_ACT','SA',' ')

        call xvget(iunit(1),status,'FORMAT',format,'ORG',org,' ')

        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF' .or. format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend
        endif
        call xvclose(iunit(1),status,' ')

	do i=1,ninput                   ! open the input
	  call xvunit(iunit(i),'INP',i,status,' ')
	  if (status.ne.1) call xvsignal(iunit(i),status,1)
	end do
	
	call xvopen(iunit(1),status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
	if (status.ne.1) call xvsignal(iunit(1),status,1)
	
	if (ninput.eq.3) then           ! check for derivative inputs
	  call xvopen(iunit(2),status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
	  if (status.ne.1) call xvsignal(iunit(2),status,1)
	  call xvopen(iunit(3),status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
	  if (status.ne.1) call xvsignal(iunit(3),status,1)
	end if
	
        call xvpcnt ( 'OUT',  nodset )  ! the output data sets

	call xvp('WINDOW',nlw,icount) ! get window size

	call xvsize( sl, ss, nl, ns, nli,nsi)

	if(ns .gt. maxin) then    ! bam 2/95 !! ndr 3/96
	  call xvmessage('??E - Image has too many samples',' ')
	  call abend
	endif

        kmin  = XVPTST('MIN')    ! SEE WHAT WE WISH TO PROCESS
        kmax  = XVPTST('MAX')
        kmean = XVPTST('MEAN')
        ksd   = XVPTST('SD')
        kmssd = XVPTST('MSSD')

        lines = nl / nlw                ! get # of lines and samples
        samps = ns / nlw
        windowsize = nlw * nlw
	center = (1.0 + nlw)/2.0

        do i = 1,5                      ! initialize output files required
            outs(i) = 1                 ! default = all on
        end do

        if ( .not. kmean ) outs(1) = 0
        if ( .not. kmin  ) outs(2) = 0
        if ( .not. kmax  ) outs(3) = 0
        if ( .not. ksd   ) outs(4) = 0
        if ( .not. kmssd ) outs(5) = 0


!       check we have enough output files specified for the work required

        sum = outs(1) + outs(2) + outs(3) + outs(4) + outs(5)
        if ( sum .ne. nodset ) then
          call xvmessage('??E - Outputs requested do not match number of output data sets.',' ')
          call abend
        endif


        do i = 1,nodset
	    call xvunit(out(i),'OUT',i,status,' ')
	    if (status.ne.1) call xvsignal(out(i),status,1)
	    call xvopen(out(i),status,'OP','WRITE','OPEN_ACT','SA',
     -       'O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),'U_NL',lines, 'U_NS',samps,' ')
	    if (status.ne.1) call xvsignal(out(i),status,1)
        end do
c	decode 1=BYTE, 2=HALF, 4=FULL, 7=REAL, 8=DOUBLE, 10=COMPLEX
        dcode = 7
	cur_line = 1

        do i = 1, lines

            do j = 1, nlw              ! read a window of lines - all samples
               ipt = 1 + ( j - 1 ) * ns
               call xvread(iunit(1),databuf(ipt),status,'NSAMPS',ns,
     +              'line',cur_line,' ')
	       cur_line = cur_line+1
	       if (status.ne.1) call xvsignal(iunit(1),status,1)
            end do

	    if (ninput.eq.3) then  ! Read in a line of DX and DY slopes
               call xvread(iunit(2),dxbuf,status, ' ')
	       if (status.ne.1) call xvsignal(iunit(2),status,1)
               call xvread(iunit(3),dybuf,status, ' ')
	       if (status.ne.1) call xvsignal(iunit(3),status,1)
	    end if
	    
            do k = 1, samps                     ! loop through the windows
                mm = 1
                begin = ( k - 1 ) * nlw 
                sum = 0.0d0
                do l = 1, nlw                   ! # of lines
                    ist = begin + ( l - 1 ) * ns
                    do m = 1, nlw               ! # of samples
                        sum = sum + databuf(ist+m)
                        windowbuf(mm) = databuf(ist+m)
                        mm = mm + 1
                    end do                      !Loop for one window-line
                end do                 !Loop for one window over a sample

                out1(k) = sum / windowsize
c  minmax is sub in P2
c	all parms integer*4 except windowbuf
                call minmax (dcode, windowsize, windowbuf, 
     +            cmin, cmax, imin, imax)
                out2(k) = cmin
                out3(k) = cmax

                mean = out1(k)
                sum = 0.0d0
                do nn = 1, windowsize
                    sum = sum + (windowbuf(nn) - mean )**2
                end do
                sd = sum / windowsize
                out4(k) = dsqrt(sd)
		
		if (ninput.ne.3) continue  !with k-samp loop
		
		!compute mean slope deviation
		dx = dxbuf(k)  !slope with increasing x (to right)
		dy = dybuf(k)  !slope with increasing y (up)
		sldev = 0.0d0
		mm = 1
                do l = 1, nlw                   ! # of lines
                    do m = 1, nlw               ! # of samples
                        slerr = windowbuf(mm) 
			slerr = slerr - 
     +				(mean + dx*(m-center) + dy*(center-l))
                        sldev = sldev + slerr*slerr
                        mm = mm + 1
                    end do  !Loop for one window-line
                end do !Loop for one window over a sample
		
		sldev = dsqrt( sldev / (windowsize -1) )   ! standard deviation
                sout(k) = sldev
		
            end do


            next = 1                       ! output files
            if ( outs(1) .eq. 1 ) then     ! mean required
                call xvwrit(out(next),out1,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(2) .eq. 1 ) then      ! min required
                call xvwrit(out(next),out2,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(3) .eq. 1 ) then ! max required
                call xvwrit(out(next),out3,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(4) .eq. 1 ) then ! sd required
                call xvwrit(out(next),out4,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(5) .eq. 1 ) then ! sd required
                call xvwrit(out(next),sout,status,' ')
    	        if (status.ne.1) call xvsignal(sout,status,1)
            end if


        end do   	! end of main loop



        do i = i, ninput   ! close input
   	        call xvclose(iunit(i),status,' ')
		if (status.ne.1) call xvsignal(out(i),status,1)
	enddo

	do i=1,nodset      ! and output
		call xvclose(out(i),status,' ')
		if (status.ne.1) call xvsignal(out(i),status,1)
	enddo


	return
 	end

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create imgstat.pdf
PROCESS help=*
  PARM INP  TYPE=STRING COUNT=0:3 DEFAULT=--
  PARM OUT  TYPE=STRING COUNT=0:5 DEFAULT=--
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM SL   TYPE=INTEGER DEFAULT=1
  PARM SS   TYPE=INTEGER DEFAULT=1
  PARM NL   TYPE=INTEGER DEFAULT=0
  PARM NS   TYPE=INTEGER DEFAULT=0
  PARM MEAN TYPE=KEYWORD  COUNT=0:1   VALID=(MEAN)  DEFAULT=-- 
  PARM MIN  TYPE=KEYWORD  COUNT=0:1   VALID=(MIN)   DEFAULT=-- 
  PARM MAX  TYPE=KEYWORD  COUNT=0:1   VALID=(MAX)   DEFAULT=-- 
  PARM SD   TYPE=KEYWORD  COUNT=0:1   VALID=(SD)    DEFAULT=-- 
  PARM MSSD TYPE=KEYWORD  COUNT=0:1   VALID=(MSSD)  DEFAULT=-- 
  PARM WINDOW TYPE=INTEGER DEF=12 
END-PROC
.TITLE 
VICAR Program IMGSTAT
.HELP
PURPOSE

	IMGSTAT is a VICAR applications program for calculating statistical
	quantities in local areas surrounding each pixel in an input image.
    It is a sizing down operation producing images zoomed down by the
    window size.  
	The local mean, minimum, maximum, standard deviations, and optionally
        the mean slope devided by the standard deviation are quantities that 
        are obtained.

.PAGE
EXECUTION FORMAT

	TAE>IMGSTAT IN,OUT, WINDOW

	where

	IN	is the input data set (VICAR labeled image).

	OUT	is the output image of the selected statistic.
                Three output files are required; optionally,
                if four output files are requested, a standard
                deviations output file is created.

	WINDOW	is the number of lines in the local area window.
.PAGE
OPERATION

	IMGSTAT performs a sliding window statistical analysis on an input
	image. An output pixel's position represents the center position of
	the window for the input image and its value represents statistics
	based on data within the window only. The window moves along one sample
	at a time and one line at a time until each pixel in the input has
	been at the center of the window. In other words statistics are 
	compiled for the local area around each and every pixel in the input
	image.

	The edge conditions are handled as follows. Any window positions that
	extend beyond the edge of the image are handled by using data from the
	nearest pixel on the image. This is not a reflection scheme like some
	sliding window algorithms have.


    Byte, half, full and real formats are supported. The output
    format is the same as whatever is input. The internal calculations
    are performed in real format. The vicar xv routines handles the
    output conversion.
	Input images must have less than 14001 samples. There is no limit 
    to the number of lines in the input.
.PAGE
EQUATIONS

	MIN
		The min is a smallest value of all DN values in the 
		window.

	MAX
		The max is a largest value of all DN values in the 
		window.

	MEAN
		The mean is a result of the sum of all DN values in the 
		window divided by the number of pixels in the window.
		(the average of the DNs)
			WINDOWSUM / NPIXWNDO

    STANDARD DEVIAION
        The standard deviation is the square root of the sum 
        of all observations minus the mean devided by the 
        number of observations.     


     MEAN SLOPE / STANDARD DEVIAION
        The mean slope is computed and is then devided by the 
        standard deviaion for all observations.
  

Original Programmer:	Barbara McGuffie    February 1995
Cognizant Programmer:	Ray Bambery     

History:
        2/95  - B. McGuffie  
        3/96  - B. McGuffie     Increased samples to 14000
        10/96 - B. McGuffie
        1/97  - N. Ritter       Added Slope deviation output options.
        13-Apr-2001 - R Bambery - Converted to real*4 internals.
                                Now allow BYTE, HALF, FULL and REAL formats

.LEVEL1
.VARI INP
Vicar labeled image file (input)
Byte, Half, Full or Real
.VARI OUT
Vicar labeled image file/s
.VARI WINDOW
Number of lines/samples
in the window
.VARI NL
Number of input lines
.VARI NS
Number of input samples (Maximum 14000)
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI SIZE
Standard VICAR size field
.VARI MEAN
If indicated, specifies that the 
first output data set will 
contain mean values
.VARI MIN
If indicated, specifies that 
the next output data set will 
contain minimum values
.VARI MAX
If indicated, specifies that 
the next output data set will 
contain maximum values
.VARI SD
If indicated, specifies that 
the next output data set will 
contain standard deviations
.VARI MSSD
If indicated, specifies that
the next output data set will 
contain mean slope devided by 
the standard deviations
.LEVEL2
.VARI	IN
File name to be used as the input
data set (VICAR labeled image).
.VARI   OUT
File names to be used as the output
data set (VICAR labeled image).
.VARI 	WINDOW
The number of lines in the local area window.
.VARI NL
Number of input lines
.VARI NS
Number of input samples
.VARI SL
Starting line
.VARI SS
Starting sample
.END
$ Return
$!#############################################################################
$Imake_File:
$ create imgstat.imake

/***********************************************************************

                     IMAKE FILE FOR PROGRAM imgstat

   To Create the build file give the command:

		$ vimake imgstat 			(VMS)
   or
		% vimake imgstat	       		(Unix)


************************************************************************/

#define PROGRAM	imgstat
#define R2LIB

#define MODULE_LIST imgstat.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstimgstat.pdf
!  Procedure to test the procedure IMGSTAT
procedure
refgbl $echo
refgbl $autousage
! Jun 22, 2012 - RJB
! TEST SCRIPT FOR IMGSTAT
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list   
! 
! parameters:
!   <none>
!
! Requires NO external test data: 

body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
!BYTE
gen a 10 10 
list a
imgstat a b 'mean window=3
list b
imgstat a b 'min window=3
list b
imgstat a b 'min window=5
list b
imgstat a b 'max window=3
list b
imgstat a b 'sd window=3
list b
imgstat a b 'mssd window=3
list b
imgstat a (b,c,d,e,f) 'mean 'min 'max 'sd 'mssd window=3
list b
list c
list d
list e
list f
imgstat a (b,c)  'min 'max window=3
list b
list c
!HALF
gen h 10 10 ival=100 format=HALF
list h
imgstat h i 'mean window=3
list i
imgstat h i 'min window=3
list i
imgstat h i 'min window=5
list i
imgstat h i 'max window=3
list i
imgstat h i 'sd window=3
list i
imgstat h i 'mssd window=3
list i
imgstat h (i,j,k,l,m) 'mean 'min 'max 'sd 'mssd window=3
list i
list j
list k
list l
list m
imgstat h (i,j)  'min 'max window=3
list i
list j

!FULL
gen f 10 10 ival=10000 format=FULL
list f
imgstat f g 'mean window=3
list g
imgstat f g 'min window=3
list g
imgstat f g 'min window=5
list g
imgstat f g 'max window=3
list g
imgstat f g 'sd window=3
list g
imgstat f g 'mssd window=3
list g
imgstat f (g,h,i,j,k) 'mean 'min 'max 'sd 'mssd window=3
list g
list h
list i
list j 
list k
imgstat f (g,h)  'min 'max window=3
list g
list h

!REAL
gen r 10 10 ival=10000.0 format=REAL
list r
imgstat r s 'mean window=3
list s
imgstat r s 'min window=3
list s
imgstat r s 'min window=5
list s
imgstat r s 'max window=3
list s
imgstat r s 'sd window=3
list s
imgstat r s 'mssd window=3
list s
imgstat r (s,t,u,v,w) 'mean 'min 'max 'sd 'mssd window=3
list s
list t
list u
list v 
list w
imgstat r (s,t)  'min 'max window=3
list s
list t

! clean up
ush rm -f ?

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstimgstat.log_solos
tstimgstat
gen a 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
imgstat a b 'mean window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:30 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
imgstat a b 'min window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:31 2012
     Samp     1       3
   Line
      1       0   3   6
      2       3   6   9
      3       6   9  12
imgstat a b 'min window=5
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:31 2012
     Samp     1
   Line
      1       0   5
      2       5  10
imgstat a b 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:32 2012
     Samp     1       3
   Line
      1       4   7  10
      2       7  10  13
      3      10  13  16
imgstat a b 'sd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:32 2012
     Samp     1       3
   Line
      1       1   1   1
      2       1   1   1
      3       1   1   1
imgstat a b 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:33 2012
     Samp     1       3
   Line
      1       1   1   1
      2       1   1   1
      3       1   1   1
imgstat a (b,c,d,e,f) 'mean 'min 'max 'sd 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:34 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:34 2012
     Samp     1       3
   Line
      1       0   3   6
      2       3   6   9
      3       6   9  12
list d
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:34 2012
     Samp     1       3
   Line
      1       4   7  10
      2       7  10  13
      3      10  13  16
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:34 2012
     Samp     1       3
   Line
      1       1   1   1
      2       1   1   1
      3       1   1   1
list f
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:34 2012
     Samp     1       3
   Line
      1       1   1   1
      2       1   1   1
      3       1   1   1
imgstat a (b,c)  'min 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
     Samp     1       3
   Line
      1       0   3   6
      2       3   6   9
      3       6   9  12
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:29 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
     Samp     1       3
   Line
      1       4   7  10
      2       7  10  13
      3      10  13  16
gen h 10 10 ival=100 format=HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list h
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       100   101   102   103   104   105   106   107   108   109
      2       101   102   103   104   105   106   107   108   109   110
      3       102   103   104   105   106   107   108   109   110   111
      4       103   104   105   106   107   108   109   110   111   112
      5       104   105   106   107   108   109   110   111   112   113
      6       105   106   107   108   109   110   111   112   113   114
      7       106   107   108   109   110   111   112   113   114   115
      8       107   108   109   110   111   112   113   114   115   116
      9       108   109   110   111   112   113   114   115   116   117
     10       109   110   111   112   113   114   115   116   117   118
imgstat h i 'mean window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:36 2012
     Samp       1     2     3
   Line
      1       102   105   108
      2       105   108   111
      3       108   111   114
imgstat h i 'min window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:36 2012
     Samp       1     2     3
   Line
      1       100   103   106
      2       103   106   109
      3       106   109   112
imgstat h i 'min window=5
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:37 2012
     Samp       1     2
   Line
      1       100   105
      2       105   110
imgstat h i 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:37 2012
     Samp       1     2     3
   Line
      1       104   107   110
      2       107   110   113
      3       110   113   116
imgstat h i 'sd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:38 2012
     Samp       1     2     3
   Line
      1         1     1     1
      2         1     1     1
      3         1     1     1
imgstat h i 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:39 2012
     Samp       1     2     3
   Line
      1         1     1     1
      2         1     1     1
      3         1     1     1
imgstat h (i,j,k,l,m) 'mean 'min 'max 'sd 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:39 2012
     Samp       1     2     3
   Line
      1       102   105   108
      2       105   108   111
      3       108   111   114
list j
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:39 2012
     Samp       1     2     3
   Line
      1       100   103   106
      2       103   106   109
      3       106   109   112
list k
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:39 2012
     Samp       1     2     3
   Line
      1       104   107   110
      2       107   110   113
      3       110   113   116
list l
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:39 2012
     Samp       1     2     3
   Line
      1         1     1     1
      2         1     1     1
      3         1     1     1
list m
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:39 2012
     Samp       1     2     3
   Line
      1         1     1     1
      2         1     1     1
      3         1     1     1
imgstat h (i,j)  'min 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list i
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:40 2012
     Samp       1     2     3
   Line
      1       100   103   106
      2       103   106   109
      3       106   109   112
list j
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:35 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:40 2012
     Samp       1     2     3
   Line
      1       104   107   110
      2       107   110   113
      3       110   113   116
gen f 10 10 ival=10000 format=FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list f
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          10000      10001      10002      10003      10004      10005      10006      10007      10008      10009
      2          10001      10002      10003      10004      10005      10006      10007      10008      10009      10010
      3          10002      10003      10004      10005      10006      10007      10008      10009      10010      10011
      4          10003      10004      10005      10006      10007      10008      10009      10010      10011      10012
      5          10004      10005      10006      10007      10008      10009      10010      10011      10012      10013
      6          10005      10006      10007      10008      10009      10010      10011      10012      10013      10014
      7          10006      10007      10008      10009      10010      10011      10012      10013      10014      10015
      8          10007      10008      10009      10010      10011      10012      10013      10014      10015      10016
      9          10008      10009      10010      10011      10012      10013      10014      10015      10016      10017
     10          10009      10010      10011      10012      10013      10014      10015      10016      10017      10018
imgstat f g 'mean window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
     Samp            1          2          3
   Line
      1          10002      10005      10008
      2          10005      10008      10011
      3          10008      10011      10014
imgstat f g 'min window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:42 2012
     Samp            1          2          3
   Line
      1          10000      10003      10006
      2          10003      10006      10009
      3          10006      10009      10012
imgstat f g 'min window=5
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:43 2012
     Samp            1          2
   Line
      1          10000      10005
      2          10005      10010
imgstat f g 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:43 2012
     Samp            1          2          3
   Line
      1          10004      10007      10010
      2          10007      10010      10013
      3          10010      10013      10016
imgstat f g 'sd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:44 2012
     Samp            1          2          3
   Line
      1              1          1          1
      2              1          1          1
      3              1          1          1
imgstat f g 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:45 2012
     Samp            1          2          3
   Line
      1              1          1          1
      2              1          1          1
      3              1          1          1
imgstat f (g,h,i,j,k) 'mean 'min 'max 'sd 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:45 2012
     Samp            1          2          3
   Line
      1          10002      10005      10008
      2          10005      10008      10011
      3          10008      10011      10014
list h
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:45 2012
     Samp            1          2          3
   Line
      1          10000      10003      10006
      2          10003      10006      10009
      3          10006      10009      10012
list i
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:45 2012
     Samp            1          2          3
   Line
      1          10004      10007      10010
      2          10007      10010      10013
      3          10010      10013      10016
list j
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:45 2012
     Samp            1          2          3
   Line
      1              1          1          1
      2              1          1          1
      3              1          1          1
list k
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:45 2012
     Samp            1          2          3
   Line
      1              1          1          1
      2              1          1          1
      3              1          1          1
imgstat f (g,h)  'min 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list g
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
     Samp            1          2          3
   Line
      1          10000      10003      10006
      2          10003      10006      10009
      3          10006      10009      10012
list h
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:41 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
     Samp            1          2          3
   Line
      1          10004      10007      10010
      2          10007      10010      10013
      3          10010      10013      10016
gen r 10 10 ival=10000.0 format=REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list r
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.000E+04   1.000E+04   1.000E+04   1.000E+04   1.000E+04   1.000E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04
      2       1.000E+04   1.000E+04   1.000E+04   1.000E+04   1.000E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04
      3       1.000E+04   1.000E+04   1.000E+04   1.000E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04
      4       1.000E+04   1.000E+04   1.000E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04
      5       1.000E+04   1.000E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04
      6       1.000E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04
      7       1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.002E+04
      8       1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.002E+04   1.002E+04
      9       1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.002E+04   1.002E+04   1.002E+04
     10       1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.001E+04   1.002E+04   1.002E+04   1.002E+04   1.002E+04
imgstat r s 'mean window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:47 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.000E+04   1.001E+04
      2       1.000E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.001E+04
imgstat r s 'min window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:48 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.000E+04   1.001E+04
      2       1.000E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.001E+04
imgstat r s 'min window=5
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:48 2012
     Samp             1           2
   Line
      1       1.000E+04   1.000E+04
      2       1.000E+04   1.001E+04
imgstat r s 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:49 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.001E+04   1.001E+04
      2       1.001E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.002E+04
imgstat r s 'sd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:49 2012
     Samp             1           2           3
   Line
      1       1.155E+00   1.155E+00   1.155E+00
      2       1.155E+00   1.155E+00   1.155E+00
      3       1.155E+00   1.155E+00   1.155E+00
imgstat r s 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:50 2012
     Samp             1           2           3
   Line
      1       1.225E+00   1.225E+00   1.225E+00
      2       1.225E+00   1.225E+00   1.225E+00
      3       1.225E+00   1.225E+00   1.225E+00
imgstat r (s,t,u,v,w) 'mean 'min 'max 'sd 'mssd window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:51 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.000E+04   1.001E+04
      2       1.000E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.001E+04
list t
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:51 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.000E+04   1.001E+04
      2       1.000E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.001E+04
list u
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:51 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.001E+04   1.001E+04
      2       1.001E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.002E+04
list v
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:51 2012
     Samp             1           2           3
   Line
      1       1.155E+00   1.155E+00   1.155E+00
      2       1.155E+00   1.155E+00   1.155E+00
      3       1.155E+00   1.155E+00   1.155E+00
list w
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:51 2012
     Samp             1           2           3
   Line
      1       1.225E+00   1.225E+00   1.225E+00
      2       1.225E+00   1.225E+00   1.225E+00
      3       1.225E+00   1.225E+00   1.225E+00
imgstat r (s,t)  'min 'max window=3
Beginning VICAR task imgstat
IMGSTAT version 13-Apr-2011
list s
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:52 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.000E+04   1.001E+04
      2       1.000E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.001E+04
list t
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Nov  8 12:57:46 2012
 Task:IMGSTAT   User:lwk       Date_Time:Thu Nov  8 12:57:52 2012
     Samp             1           2           3
   Line
      1       1.000E+04   1.001E+04   1.001E+04
      2       1.001E+04   1.001E+04   1.001E+04
      3       1.001E+04   1.001E+04   1.002E+04
ush rm -f ?
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
