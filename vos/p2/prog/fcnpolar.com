$!****************************************************************************
$!
$! Build proc for MIPL module fcnpolar
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:10
$!
$! Execute by entering:		$ @fcnpolar
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
$ write sys$output "*** module fcnpolar ***"
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
$ write sys$output "Invalid argument given to fcnpolar.com file -- ", primary
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
$   if F$SEARCH("fcnpolar.imake") .nes. ""
$   then
$      vimake fcnpolar
$      purge fcnpolar.bld
$   else
$      if F$SEARCH("fcnpolar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fcnpolar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fcnpolar.bld "STD"
$   else
$      @fcnpolar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fcnpolar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fcnpolar.com -mixed -
	-s fcnpolar.f -
	-i fcnpolar.imake -
	-p fcnpolar.pdf -
	-t tstfcnpolar.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fcnpolar.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

c EXAMPLE # 1
c Purpose: To test the Metropolis algorithm in three dimensions.
c The example below is to fit a circle to 6 data points, two
c of which do not lie on the circle.
c Notice the cost function rewards for including lots of data
c points and is allowed to reject points which fall far from the
c fitted function.
      real range(3),answer(3),array(500),array2(500),i2,i3,i4,i5,d(500)
      real sum,sres,sumres,serror,terror
      real*8 PI
      data PI/3.141592653589793D0/
      integer i1,npts,nnd
      character*80 msg
      character*255 polartbl
      external cost
      numten=1000
      limit=5000
      iprint=0
      narg=3
      norm=330


c (1)=a (2)=b (3)=theta

      call xvparm('POLARTBL', polartbl, icnt, idef, 1)
c     xvfilename generated error in VMS so took it out

      do 60 ii=1,5
      npts=0
      sum=0.
      sumres=0.
      sres=0.
      serror=0.
      nnd=0.	
      answer(1)=4095.
      answer(2)=0.
      answer(3)=90.
      data range/1000.,1000.,90./

c      open(11,name=polartbl,type='old',err=999)
      open(11,file=polartbl,status='old',err=999)
      read(11,1)
      read(11,1)
1     format(a)
      do 10 I=1,500
	read(11,2,end=50,err=999)I1,I2,I3,I4,I5
2       format(I6,1X,E18.12,1X,E18.12,1X,E18.12,1X,E18.12)
        npts=npts+1
        array(I)=I4
        array2(I)=I2
        if (I4 .lt. answer(1)) then
		answer(1)=I4
        endif
	if (I4 .gt. answer(2)) then
		answer(2)=I4
        endif
10    continue
	answer(2)=answer(2)-answer(1)
50    close(11,err=999)

      call metropolis(cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      if(ind.eq.0)then
        write(msg,*)'guess number:',(ii)
        call xvmessage(msg,' ')
         write(msg,*)'answer=',(answer(j),j=1,narg)
         call xvmessage(msg,' ')

        do j=1,npts
        
	 d(j)=abs(answer(1)+answer(2)*
     &		(cos((array2(j)-answer(3))*PI/180.)**2)  -array(j))
         sres=sres+abs(d(j))
	 sumres=sumres+d(j)*d(j)
        
	enddo

        mean=sres/npts
        sigma=sqrt(sumres/npts - mean*mean)
        write(msg,*)'sigma=',(sigma)
        call xvmessage(msg,' ')   
	do j=1,npts
	   if (d(j) .lt. sigma) then
		nnd=nnd+1
		serror=serror+d(j)/array(j)
	   endif
	enddo
	terror=serror/nnd
        write(msg,*)'error=',(terror)
        call xvmessage(msg,' ')   

      else
         call xvmessage('no solution',' ')
      endif
60    continue
      return

999   call xvmessage('Error reading input file',' ')
      call abend
      end


      subroutine  cost(x,array,array2,n,error,ind)
c Returns the cost function resulting from guessing x (answer).
c This could be any function or logical operation.

c X     is the solution vector.
c ARRAY is an array of N data points if needed.
c ARRAY2 is another array of N elements if needed.
c N      is the number of elements/data points in ARRAY & ARRAY2.
c ERROR is the returned cost.
c IND   is 0 for normal return, 1 for abnormal return.
c        If METROPOLIS senses an indicator of 1 it will generate another
c        guess somewhere else & try again.

c (1)=radius (2)=x_center (3)=y_center
      real array(n),array2(n),x(4),dr(500)

      ind=1
      if(x(1).lt.0.)return
      if(x(1).gt.4095.)return
      if(x(2).gt.4095.)return
      if(x(3).gt.180.)return
      if(x(2).lt.0.)return
      if(x(3).lt.0.)return
      sumdr=0.
      sum=0.
      range=1000
      m=0
      do j=1,n
        dr(j)=abs(x(1)+x(2)*(cos((array2(j)-x(3))*PI/180.)**2)
     &					- array(j))
        sum=sum+dr(j)
        if(dr(j).lt.range)then
           m=m+1
           sumdr=sumdr+dr(j)
        endif
      enddo
      if(m.eq.0)then
         error=sum/n + range/n
      else
         error=sumdr/m + range/m
      endif
      ind=0
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fcnpolar.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fcnpolar

   To Create the build file give the command:

		$ vimake fcnpolar			(VMS)
   or
		% vimake fcnpolar			(Unix)


************************************************************************/


#define PROGRAM	fcnpolar
#define R2LIB

#define MODULE_LIST fcnpolar.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fcnpolar.pdf
PROCESS help=*
PARM POLARTBL	STRING
END-PROC
.TITLE
VICAR PROGRAM fcnpolar
.HELP

Fcnpolar is a VICAR program that fits a function to a set of data points
obtained from a polarization test involving the program srchedge.  Fcnpolar
uses the subroutine metropolis to compute the parameters of the function and
it outputs the results to the screen.

EXECUTION

	fcnpolar filename

OPERATION

The form of the function is:

	VALUE = A + B * COS(ANGLE - THETA)**2

where A is the level,
      B is the scale factor, and
      THETA is an angle in degrees.

VALUE and ANGLE are obtained from the table of data points and the
parameters A, B, and THETA are calculated using metropolis.

Fcnpolar produces five fits.  Note that the answer= line in the output
lists A, B, and THETA in that order.  Values for sigma and error are also
displayed.

PRECISION
	Some variation in output will occur due to the use of subroutine
	metropolis, which uses random numbers.  See farenc.pdf and
	metropolis.pdf for details.

REVISIONS
	7-97 ...RRD... Made portable for UNIX

.LEVEL1
.VARIABLE POLARTBL
ASCII input file of data points.
.LEVEL2
.VARIABLE POLARTBL
STRING
The file is in the form of a table with columns SCLK, ANGLE(DEG.), AVE_DK_DN, 
AVE_BR_DN, and RATIO.  The first column is an integer and the rest are floats.
Note that the first two lines of the file are ignored.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfcnpolar.pdf
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo=("yes","no","no")	! echo only1 top level
!
createfile polar.dat
addtofile polar.dat "ISSNA	PTP_NAC_EM	-1.276000022888e+01"
addtofile polar.dat "SCLK	ANGLE(DEG.)		AVE_DK_DN		AVE_BR_DN		RATIO"
addtofile polar.dat "111016	0.000000000000e+00	1.000000000000e+00	1.000000000000e+00	1.000000000000e+00"
addtofile polar.dat "111016	4.500000000000e+01	1.000000000000e+00	1.500000000000e+00	1.500000000000e+00"
addtofile polar.dat "111016	9.000000000000e+01	1.000000000000e+00	2.000000000000e+00	2.000000000000e+00"
addtofile polar.dat "111016	1.350000000000e+02	1.000000000000e+00	1.500000000000e+00	1.500000000000e+00"
addtofile polar.dat "111016	1.800000000000e+02	1.000000000000e+00	1.000000000000e+00	1.000000000000e+00"
addtofile polar.dat "111016	2.250000000000e+02	1.000000000000e+00	1.500000000000e+00	1.500000000000e+00"
addtofile polar.dat "111016	2.700000000000e+02	1.000000000000e+00	2.000000000000e+00	2.000000000000e+00"
addtofile polar.dat "111016	3.150000000000e+02	1.000000000000e+00	1.500000000000e+00	1.500000000000e+00"
addtofile polar.dat "111016	3.600000000000e+02	1.000000000000e+00	1.000000000000e+00	1.000000000000e+00"
!
fcnpolar polar.dat
end-proc
.help

	Test pdf for the routine fcnpolar.

	Note:   The answer= line should be approximately  1.00000  1.00000  90.0000.
		Results will vary due to the use of subroutine metropolis.
.end
$ Return
$!#############################################################################
