$!****************************************************************************
$!
$! Build proc for MIPL module alphar
$! VPACK Version 1.8, Monday, November 15, 2004, 16:52:02
$!
$! Execute by entering:		$ @alphar
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
$ write sys$output "*** module alphar ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to alphar.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("alphar.imake") .nes. ""
$   then
$      vimake alphar
$      purge alphar.bld
$   else
$      if F$SEARCH("alphar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake alphar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @alphar.bld "STD"
$   else
$      @alphar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create alphar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack alphar.com -
	-s alphar.f -
	-i alphar.imake -
	-p alphar.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create alphar.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none

      integer     nlo,nso,nli,nsi,slo,sso,iunit,ounit,stat,sb,nb,nbi,i
      integer     icount,idef,line,band      
      integer     goodvalue(10000)
      real*8      avgbuf(10000)
      real        c1,c2,pi,fact,a,b,c
      real        bufin(10000),bufout(10000),avgout(10000)
      real        wave(20),weight(20)

C     Echo the version when the program runs and abort with an 
C     Error message if a vicar routine fails      
      call ifmessage ('alphar Version 15-Nov-2004')
      call xveaction ('sa', ' ')

C     Open input data set
      call xvunit(iunit,'inp',1,stat,' ')
      call xvopen(iunit,stat,'u_format','real',' ')

C     Get the size of the ouput
      call xvsize(slo,sso,nlo,nso,nli,nsi)
      call xvbands(sb,nb,nbi)
      
C     Check the number of lines requested does not exceed the 
C     number available, if there it does - abort
      if (slo+nlo.gt.nli+1) then
        call mabend('Error: requested lines exceed those available')
      endif
      
C     Check the number of samples requested does not exceed the 
C     number available, if there it does - abort
      if (sso+nso.gt.nsi+1) then
        call mabend('Error: requested samples exceed those available')
      endif
      
C     Check there are less than 20 bands and less than 10,000
C     samples, if there are - abort
      if(nsi.gt.10000) then
      	call mabend('sorry the max # of input samples is 10,000')
      end if
      if(nbi.gt.20) then
      	call mabend('sorry the max # of input bands is 20')
      end if
      
C     Get the wavelength vals and make sure there are as many 
C     values as there are bands
      call xvparm('wave',wave,icount,idef,20)
      if(icount.ne.nbi) then
      	call mabend('sorry # of wave vals must equal # input bands')
      end if            	

C     Open output data set, make it one band larger than the input
      call xvunit(ounit,'out',1,stat,' ')
      call xvopen(ounit,stat,'u_format','real','o_format','real',
     &            'op','write','u_nl',nlo,'u_ns',nso,'u_nb',nb+1,
     &            'u_org','bil',' ')

C     Set the constants 
      c1 = 3.74151e-16    !in w/m2
      c2 = 0.0143879
      pi = 3.1416
      fact = 1.0e-6
C
      print *, ''
      print *, 'This program requires your data to be'
      print *, 'atmospherically corrected and in'
      print *, 'W/m*m/um/sr'
      print *, ''
C
C     Convert wavelength to metres and calculate the band weights  
      do i = 1,nb
        wave(i) = wave(i)*fact         
        a = a + (wave(i)*log(c1))
        b = b + (5.0*wave(i)*log(wave(i)))
        c = c + (log(pi)*wave(i))
      enddo
c
      a = a/nb
      b = b/nb
      c = c/nb
      print *,'the weights are:'
      do i = 1,nb
        weight(i) = (wave(i)*(log(c1))) 
     1              - (5.0*wave(i)*(log(wave(i))))  
     2              - (wave(i)*(log(pi))) - a + b + c
        weight(i) = - weight(i)          
      print*, weight(i)
      enddo
      print *, ''

C     For each line first calulate the wavelength weighted mean then 
C     calculate the alpha residual
      do  line = slo,slo+nlo-1
      
C         Zero the avgbuf and goodvalue arrays
          call zia(avgbuf,2*10000)
          call zia(goodvalue,10000)
          
C         Calculate the wavelength weighted log sum 
      	  do band = 1,nb
            call xvread(iunit,bufin,stat,'line',line,'band',band,
     1                   'samp',sso,'nsamps',nso,' ')
            call arsum(wave(band), bufin, avgbuf, goodvalue, nso)
          end do
C         Calculate the wavelength weighted log mean
          call armean(avgbuf, goodvalue, avgout, nso, nb) 
          
C         Calculate the alpha residual
     	  do band = 1,nb
            call xvread(iunit,bufin,stat,'line',line,'band',band,
     1                  'samp',sso,'nsamps',nso,' ')
            call ar(wave(band), weight(band), bufin, avgbuf, bufout, 
     1              nso)
            call xvwrit(ounit,bufout,stat,'nsamps',nso,' ')
          end do
C         Write out the wavelength weighted log average
          call xvwrit(ounit,avgout,stat,'nsamps',nso,' ')
                          
      end do
C
C     Close data sets
      call xvclose(iunit,stat,' ')
      call xvclose(ounit,stat,' ')
C
      return
      end
      
C*******************************************************************************
C     Subroutine to calculate the wavelength weighted log sum for all bands
      subroutine arsum(wave, bufin, avgbuf, goodvalue, nso, nb)
      implicit none
      
      real wave,bufin(nso)
      real*8 avgbuf(nso), avg
      integer goodvalue(nso), nso, sample, nb 

C     Calculate the wavelength weighted sum
      do sample = 1,nso
C       Convert the input into w/m*m*m/sr
        avg = bufin(sample) * 1.0d+6
C       Wavelength weight the radiance
        if(avg.gt.0.0)then
          avg = dlog(avg)*dble(wave)       
          avgbuf(sample) = avgbuf(sample)+avg
C          print *, avgbuf(sample), avg
          goodvalue(sample)=goodvalue(sample)+1
        endif            
      end do
      return
      end
 
C*******************************************************************************
C     Subroutine to calculate the wavelength weighted log 
C     mean (1 value per pixel)
      subroutine armean(avgbuf, goodvalue, avgout, nso, nb)
      implicit none
      
      real*8 avgbuf(nso)
      real avgout(nso)
      integer goodvalue(nso)
      integer nso, nb, sample

C     Calc the mean      
      do sample = 1,nso
        if(goodvalue(sample).eq.nb) then
C          print *, avgbuf(sample)
          avgbuf(sample)=(avgbuf(sample)/dble(goodvalue(sample)))
        else
          avgbuf(sample)=0
        endif
C       Create a single precision version for output
        avgout(sample)=avgbuf(sample) * 1.0e7
      end do
      return 
      end
      
C*******************************************************************************
C     Subroutine to calculate the alpha residual 
      subroutine ar(wave, weight, bufin, avgbuf, bufout, nso)
      implicit none
      
      real wave, weight, bufin(nso), bufout(nso)
      real*8 avgbuf(nso), rad, alpha
      integer nso, sample

C     Calculate the alpha residual, multiply by 1.0e+10 to  get
C     in a suitable range for halfword. Until now all calculations
C     have been done with wavelength in m, multiplying by 1.0e+6 is like
C     putting everything in micrometers, by 1.0e+10 is nanometersx10 but 
C     I am not too good with units!    
      do sample = 1,nso
        rad =dble(bufin(sample))*1.0d+6
C        print *,  rad, wave, avgbuf(sample), weight
        alpha=dble(wave)*dlog(rad)-avgbuf(sample)+dble(weight)
C        print *, alpha, avgbuf(sample)
        bufout(sample)=alpha*1.0e+10
C        print *, alpha
      end do
        
      return 
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create alphar.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM alphar
   To Create the build file give the command:
		$ vimake alphar			(VMS)
   or
		% vimake alphar			(Unix)
************************************************************************/
#define PROGRAM	alphar
#define R2LIB

#define MODULE_LIST alphar.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
/* #define INCLUDE_LIST alphar.fin */
/* #define FTNINC_LIST fortport */

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create alphar.pdf
process help=*
PARM INP TYPE=(STRING,60) COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER COUNT=1 DEFAULT=1
PARM SS TYPE=INTEGER COUNT=1 DEFAULT=1
PARM NL TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NS TYPE=INTEGER COUNT=1 DEFAULT=0
PARM WAVE TYPE=REAL COUNT=(0:20) DEFAULT=--
END-PROC
.TITLE
ALPHAR version 15 November 2004
.HELP
ALPHAR performs the alpha residual transformation on an input dataset.
The input dataset must be in units of Watts/meter^2/micrometer/steradian
in order for this program to work correctly. This differs from the input 
dataset requirements of this program prior to November, 2004

The output has one more band than the input. This extra band contains
the wavelength weighted log mean of the input radiance. This can be 
thought of as a pseudo temperature image. It does show emissivity 
effects but these are subdued compared to those in the original imagery.

For more information about this algorithm see Kealy and Hook (1993). 
"Separating temperature and emissivity in thermal infrared 
multispectral scanner data: Implications for recovering 
land surface temperature." IEEE Transactions of Geoscience 
and Remote Sensing, vol. 31 pp.1155-1164.

Testing
This program was tested on an artificial 2x2x6 band radiance dataset. 
It was also tested on an TIMS image acquired over Death Valley 
that was calibrated (TIMSCAL) and atmospherically corrected (TIMSCAL2)
to radiance at the surface.

RESTRICTIONS;
1. Input data must be in units of Watts/meter^2/micrometer/steradian
2. Maximum size of one band of the input is 10,000 values.
3. Maximum number of bands in the input is 20.

HISTORY
Written by Simon J. Hook in FORTRAN.

.LEVEL1
.VARIABLE INP
Input radiance dataset 
(W/m*m/um/sr)
.VARIABLE OUT
Output alpha residual dataset 
plus an extra band including the
wavelength weighted log mean. 
.VARIABLE SIZE
standard VICAR size field
.VARIABLE SL
starting line
.VARIABLE SS
starting sample
.VARIABLE NL
number of lines
.VARIABLE NS
number of samples
.VARIABLE WAVE
the central wavelength for the 
input channels, in micrometers
.LEVEL2
.VARIABLE WAVE
The centroid wavelength value for each channel in micrometers. This can be 
calculated for a TIMS image using TIMSRESP (The central wavelength values are
dumped to the screen after the program runs).
.END
$ Return
$!#############################################################################
