$!****************************************************************************
$!
$! Build proc for MIPL module colorfit
$! VPACK Version 1.8, Monday, August 18, 1997, 14:24:37
$!
$! Execute by entering:		$ @colorfit
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
$ write sys$output "*** module colorfit ***"
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
$ write sys$output "Invalid argument given to colorfit.com file -- ", primary
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
$   if F$SEARCH("colorfit.imake") .nes. ""
$   then
$      vimake colorfit
$      purge colorfit.bld
$   else
$      if F$SEARCH("colorfit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake colorfit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @colorfit.bld "STD"
$   else
$      @colorfit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create colorfit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack colorfit.com -
	-s colorfit.f -
	-i colorfit.imake -
	-p colorfit.pdf -
	-t tstcolorfit.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create colorfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program colorme
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxpix=3,maxtable=10000)
c      character*200 msg
c      integer*4 def,count
      integer*4 inunit(maxpix),status,nl(maxpix),ns(maxpix)
      real*4 buf(maxpix)
      real*8 C(maxtable,4),CL(maxtable),coef(4),error

c parameters
c      call xvparm('PERCENT',percent,count,def,1)

c checks
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.eq.0)then
        call xvmessage('No inputs',' ')
        call abend()
      endif

c open all inputs to get nl & ns
      do i=1,nin
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        call xvget(inunit(i),status,'NL',nl(i),'NS',ns(i),' ')
      enddo

c make sizes equal
      i=ns(1)
      j=nl(1)
      do image=1,nin
        i=min(ns(image),i)
        j=min(nl(image),j)
      enddo
      do image=1,nin
        ns(image)=i
        nl(image)=j
      enddo

c collect data points for least squares fitting
      call get_seconds(iseed)
      do index=1,maxtable
100     call rangen(iseed,rannum) ! 0.0 < rannum < 1.0
        i=nint((ns(1)-1)*rannum+1.0)
        call rangen(iseed,rannum) ! 0.0 < rannum < 1.0
        line=nint((nl(1)-1)*rannum+1.0)
        if((i.lt.1).or.(i.gt.ns(1)))goto 100
        if((line.lt.1).or.(line.gt.nl(1)))goto 100
        do image=1,nin
          call xvread(inunit(image),buf(image),status,'LINE',line,
     +                'SAMP',i,'NSAMPS',1,' ')
          if(buf(image).eq.0.0)goto 100
          if(buf(image).eq.32767)goto 100
        enddo
        c(index,1)=buf(1)
        c(index,2)=buf(2)
        c(index,3)=1.d0
        c(index,4)=buf(1)/buf(2)
        cl(index)=buf(3)
      enddo

c solve for coefficients
c WARNING: the print statements below are necessary to get the code to compile
c on sgi machines. God help us !
      write(*,*)'   '
      call lsqp(maxtable,4,c,cl,coef,maxtable)
      write(*,*)'   '
c      write(msg,*)coef(1),'*IN1+',coef(2),'*IN2+',coef(3),'+',coef(4),
c     + '*IN1/IN2'
c      call xvmessage(msg,' ')
      write(*,*)coef(1),'*IN1+',coef(2),'*IN2+',coef(3),'+',coef(4),
     + '*IN1/IN2'

      error=0.d0
      do index=1,maxtable
        error=error+abs( coef(1)*c(index,1) +
     +        coef(2)*c(index,2) + coef(3) +
     +        coef(4)*c(index,1)/c(index,2) - cl(index) )
      enddo
      error=error/maxtable
c      write(msg,*)'Mean dn residual is ',error
c      call xvmessage(msg,' ')
      write(*,*)'Mean dn residual is ',error

      end

c *********************************************************************
      SUBROUTINE LSQP(NE,NU,C,CL,X1,maxtable)
C2    THE INFORMATION FROM THE MAIN PROGRAM IS,C%I,J<#COEFFICIENT MATRIX
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C2    THE INFORMATION FROM THE MAIN PROGRAM IS,C%I,J<#COEFFICIENT MATRIX
C     CL%I<#ARRAY OF FREE TERMS,  NE#NUMBER OF
C     EQUATIONS AND NU#NUMBER OF UNKNOWNS%NE AND NU NOT TO EXCEED THE
C     DIMENSION SPECIFICATIONS BELOW<.
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS,X1(J)=THE COMPUTED
C     VALUES OF THE UNKNOWNS, V#RESIDUALS I.E. OBSERVED MINUS COMPUTED,
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.

      REAL*8 C(maxtable,4),CL(maxtable),X1(4)
      REAL*8 A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SUM

      DO 57 J=1,NU
         DO 57 I=1,NU
            A(I,J)=0.
            R(I,J)=0.
57       Q(I,J)=0.
      DO 100 I=1,NU
         DO 100 J=1,NU
            DO 100 K=1,NE
100           A(I,J)=A(I,J)+C(K,I)*C(K,J)
      DO 102 I=1,NU
         AL(I)=0.
         DO 102 K=1,NE
102            AL(I)=AL(I)+ C(K,I)*CL(K)
 
      DO I=1,NU
         if (A(I,I) .eq. 0.0)  call mabend(
     .     'ERROR in routine LSQP: Please check input parameters')
      END DO
 
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create colorfit.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM colorfit

   To Create the build file give the command:

		$ vimake colorfit			(VMS)
   or
		% vimake colorfit			(Unix)


************************************************************************/


#define PROGRAM	colorfit
#define R2LIB

#define MODULE_LIST colorfit.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create colorfit.pdf
process help=*
PARM INP TYPE=STRING COUNT=3
END-PROC

.TITLE
VICAR program colorfit

.HELP
PURPOSE:
To fit a numerical model to a set of registered color images permitting
one of them to be predicted from the others. 
Once the coefficients relating the images are established they can be used
to predict the same spectral band from other image data.

EXECUTION:
colorfit inp=(in1,in2,in3)

METHOD:
Image three is to be predicted from images 1 and 2.
The model is: in3 = A * in1 + B * in2 + C + D * in1 / in2
Coefficients A-D will be printed out.

To then reconstruct image 3 use F2:
f2 inp=(in1,in2) out=in3 function="IN1*A+IN2*B+C+D*IN1/IN2"

HISTORY:
8-30-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
3 input images

.LEVEL2
.VARI INP
3 input images

$ Return
$!#############################################################################
$Test_File:
$ create tstcolorfit.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
!colorfit inp=(/home/jjl/junk/r.img,/home/jjl/junk/b.img, +
! /home/jjl/junk/g.img)
!f2 inp=(/home/jjl/junk/r.img,/home/jjl/junk/b.img) out=g.img +
! function="0.44034435653476*IN1+0.39425723025466*IN2+44.904632489732+0.22887502363200*IN1/IN2"
!xvd inp=(/home/jjl/junk/r.img,/home/jjl/junk/g.img,/home/jjl/junk/b.img)
!xvd inp=(/home/jjl/junk/r.img,g.img,/home/jjl/junk/b.img)
gausnois out=x1.img nl=100 ns=100 seed=123456
gausnois out=x2.img nl=100 ns=100 seed=654321
f2 inp=(x1.img,x2.img) out=x3.img function="0.5*IN1+0.3*IN2+10.+10.*IN1/IN2"
colorfit inp=(x1.img,x2.img,x3.img)
!
end-proc
$ Return
$!#############################################################################
