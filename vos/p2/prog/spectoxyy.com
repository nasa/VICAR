$!****************************************************************************
$!
$! Build proc for MIPL module spectoxyy
$! VPACK Version 1.9, Thursday, January 12, 2006, 11:37:48
$!
$! Execute by entering:		$ @spectoxyy
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
$ write sys$output "*** module spectoxyy ***"
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
$ write sys$output "Invalid argument given to spectoxyy.com file -- ", primary
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
$   if F$SEARCH("spectoxyy.imake") .nes. ""
$   then
$      vimake spectoxyy
$      purge spectoxyy.bld
$   else
$      if F$SEARCH("spectoxyy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spectoxyy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spectoxyy.bld "STD"
$   else
$      @spectoxyy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spectoxyy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spectoxyy.com -mixed -
	-s spectoxyy.f -
	-i spectoxyy.imake -
	-p spectoxyy.pdf -
	-t tstspectoxyy.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spectoxyy.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program spectoxyy
c
      include 'VICMAIN_FOR'
      subroutine main44

      integer maxin,mapxy,nlamda
      parameter (mapxy=256,maxin=10,nlamda=41)

c maxin=     the largest number of input images
c mapxy=     the nl and ns of the 4th output chromaticity map.
c nlamda=    the number of wavelengths in the color matching functions

      character*10 modes,illumin
      integer*2 xybuf(mapxy,mapxy)
      integer*4 ounit(3),xyunit,def,index(maxin)
      integer*4 unit(maxin),status,nli(maxin),nsi(maxin)
      real*4 conv(maxin),lamda(maxin),buf(40000,maxin)
      real*4 cmfx(nlamda),cmfy(nlamda),cmfz(nlamda)
      real*4 wavelength(nlamda),illuminant(nlamda)
      real*4 sun_space(nlamda),D65(nlamda),save(maxin)
      real*4 spectrum(maxin),sec_deriv(maxin)

c wavelengths for the following tables in nm
      DATA wavelength/380.,390.,400.,410.,420.,430.,440.,450.,
     1 460.,470.,480.,490.,500.,510.,520.,530.,540.,550.,560.,
     2 570.,580.,590.,600.,610.,620.,630.,640.,650.,660.,670.,
     3 680.,690.,700.,710.,720.,730.,740.,750.,760.,770.,780./

c x color matching function from 380 to 780 nm
      DATA cmfx/.0014,.0042,.0143,.0435,.1344,.2839,.3483,.3362,
     1 .2908,.1954,.0956,.032,.0049,.0093,.0633,.1655,.2904,.4334,
     2 .5945,.7621,.9163,1.0263,1.0622,1.0026,.8544,.6424,.4479,.2835,
     3 .1649,.0874,.0468,.0227,.0114,.0058,.0029,.0014,.0007,.0003,
     4 .0002,.0001,0.0/

c y color matching function from 380 to 780 nm
      DATA cmfy/0.,.0001,.0004,.0012,.004,.0116,.023,.038,
     1 .06,.091,.139,.208,.323,.503,.71,.862,.954,.995,.995,.952,
     3 .87,.757,.631,.503,.381,.265,.175,.107,.061,.032,.017,.0082,
     5 .0041,.0021,.001,.0005,.0002,.0001,.0001,0.,0./

c z color matching function from 380 to 780 nm
      DATA cmfz/.0065,.0201,.0679,.2074,.6456,1.3856,1.7471,
     1 1.7721,1.6692,1.2876,.813,.4652,.272,.1582,.0782,.0422,.0203,
     2 .0087,.0039,.0021,.0017,.0011,.0008,.0003,.0002,16*0./

c D65 illuminant (normalized to 100 at 560 nm) 
c Earth average daylight at typical phase including scattering
      DATA D65/50.,54.6,82.8,91.5,93.4,86.7,104.9,117.,117.8,114.9,
     1 115.9,108.8,109.4,107.8,104.8,107.7,104.4,104.,100.,96.3,
     2 95.8,88.7,90.,89.6,87.7,83.3,83.7,80.,80.2,82.3,78.3,69.7,
     3 71.6,74.3,61.6,69.9,75.1,63.6,46.4,66.8,63.4/

c solar spectrum above Earth's atmosphere in watts/(meter**2,micron)
c spectral irradiance units. We'll divide by 2pi later,
c and correct for range to the sun.
      DATA sun_space/1120.,1098.,1429.,1751.,1747.,1639.,1810.,
     1 2006.,2066.,2033.,2074.,1950.,1942.,1882.,1833.,1842.,
     2 1783.,1725.,1695.,1712.,1715.,1700.,1666.,1635.,1602.,
     3 1570.,1544.,1511.,1486.,1456.,1427.,1402.,1369.,1344.,
     4 1314.,1290.,1260.,1235.,1211.,1185.,1159./

c checks
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.lt.3)then
         call mabend('Require at least 3 inputs')
      endif
      call xvpcnt('OUT',nout)
      if(nout.lt.3)then
         call mabend('Require 3 outputs')
      endif

c defaults
      do i=1,nin
         conv(i)=1.0
      enddo

c adjust solar spectrum for range to target
c convert to per steradian
      call xvparm('RANGE',range,ncount,def,1)
      do i=1,nlamda
        sun_space(i)=sun_space(i)/(6.28318*range**2)
      enddo

c adjust D65 to have same amplitude as the sun at 780 nm 
      scale=sun_space(41)/D65(41)
      do i=1,nlamda
        D65(i)=D65(i)*scale
      enddo

c parameters
      call xvparm('CONVERT',conv,ncount,def,maxin)
      if(ncount.ne.nin)then
         call mabend('Require as many CONVs as input images')
      endif
      call xvparm('LAMDA',lamda,ncount,def,maxin)
      if(ncount.ne.nin)then
         call mabend('Require as many LAMDAs as input images')
      endif
      call xvparm('ILLUMIN',illumin,ncount,def,1)
      if(illumin.eq.'D65')call mve(7,nlamda,D65,illuminant,1,1)
      if(illumin.eq.'SUN')call mve(7,nlamda,sun_space,illuminant,1,1)
      call xvparm('MODE',modes,ncount,def,1)
      if(modes.eq.'RADIANCE') mode=1
      if(modes.eq.'REFLECT') mode=2
      call xvparm('SOURCE',illuminant,ncount,def,nlamda)
      if((ncount.gt.0).and.(ncount.ne.nlamda))then
         call mabend('SOURCE requires 41 spectral values')
      endif

c open all inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nli(i),'NS',nsi(i),' ')
      enddo

c determine output image size
      ns=nsi(1)
      nl=nli(1)
      do i=2,nin
         nl=min(nl,nli(i))
         ns=min(ns,nsi(i))
      enddo
      if(ns.gt.40000)call mabend('Line length > 40000')

c open outputs
c     xyY images
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

c     Histogram
      if(nout.eq.4)then
        call xvunit(xyunit,'OUT',4,status,' ')
        call xvopen(xyunit,status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',mapxy,'U_NS',mapxy,'OP','WRITE',' ')
        xy_slope=mapxy/1.2
        xy_offset=mapxy- xy_slope*1.1
        call mve(2,mapxy*mapxy,0,xybuf,0,1)  ! set histogram buffer to 0
      endif

c sort the wavelengths into ascending order for spline.
c index points to the inputs in ascending order
      call mve(7,nin,lamda,save,1,1)        
      do j=1,nin
        x=1.0e+20
        do i=1,nin
          if(lamda(i).lt.x)then
            x=lamda(i)
            k=i
          endif
        enddo
        index(j)=k
        lamda(k)=2.0e+20
      enddo
c     Sort wavelengths
      do j=1,nin
        lamda(j)=save(index(j))
      enddo
c     Sort conv values
      call mve(7,nin,conv,save,1,1)        
      do j=1,nin
        conv(j)=save(index(j))
      enddo

c compute normalizing factor
      tristim_Y=0.0
      do l=1,nlamda
         tristim_Y=tristim_Y+illuminant(l)*cmfy(l)
      enddo
      factor=100./tristim_Y


c process the image**************************************************

      do j=1,nl                         ! line loop
        do k=1,nin                      ! picture loop
          call xvread(unit(index(k)),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns                       ! pixel loop

c         Compute spline coefficients for spectrum modelling
          do l=1,nin
            spectrum(l)=buf(i,l)*conv(l)
            if(buf(i,l).eq.32767.)then
              buf(i,1)=0.3333 ! x
              buf(i,2)=0.3333 ! y
              buf(i,3)=32767. ! Y
              goto 11
            endif
          enddo
          call spline(lamda,spectrum,nin,1.0e+30,1.0e+30,sec_deriv)

c         Compute tristimulus values by integrating with color matching fcns
          tristim_X=0.0
          tristim_Y=0.0
          tristim_Z=0.0
          do l=1,nlamda
c           compute spectrum amplitude at wavelength l
            call splint(lamda,spectrum,sec_deriv,nin,wavelength(l),
     +                  spectrum_at_l)
            if(mode.eq.1)then  ! radiance input images
              tristim_X=tristim_X+spectrum_at_l*cmfx(l)
              tristim_Y=tristim_Y+spectrum_at_l*cmfy(l)
              tristim_Z=tristim_Z+spectrum_at_l*cmfz(l)
            else           ! reflectance input images
              tristim_X=tristim_X+illuminant(l)*spectrum_at_l*cmfx(l)
              tristim_Y=tristim_Y+illuminant(l)*spectrum_at_l*cmfy(l)
              tristim_Z=tristim_Z+illuminant(l)*spectrum_at_l*cmfz(l)
            endif
          enddo
          tristim_X=tristim_X * factor
          tristim_Y=tristim_Y * factor
          tristim_Z=tristim_Z * factor
          
c         Convert to x,y,Y
          sum=tristim_X + tristim_Y + tristim_Z
          if(sum.ne.0.0)then
            x=tristim_X/sum
            y=tristim_Y/sum
          else
            x=0.0
            y=0.0
          endif

c         check for negative luminance
          if(tristim_Y.lt.0.0)then
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
          else
            buf(i,1)=x
            buf(i,2)=y
            buf(i,3)=tristim_Y
          endif

c         update the 4th output image if desired
          if(nout.eq.4)then
            l=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
            m=nint(x*xy_slope+xy_offset)           ! sample
            if(l.lt.1) l=1
            if(m.lt.1) m=1
            if(l.gt.mapxy) l=mapxy
            if(m.gt.mapxy) m=mapxy
            if(xybuf(m,l).lt.32767) xybuf(m,l)=xybuf(m,l)+1
          endif
11        continue

        enddo   ! pixel loop      

        do k=1,3             ! write one line loop
          call xvwrit(ounit(k),buf(1,k),status,' ')
        enddo

      enddo     ! line loop


c stretch and write out the 4th output image
      if(nout.eq.4)then
c       stretch xybuf
        x=0.0
        y=0.0
        l_of_zero=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
        m_of_zero=nint(x*xy_slope+xy_offset)           ! sample
        maxhist=0
        do l=1,mapxy
          do m=1,mapxy
            if(l.ne.l_of_zero.and.m.ne.m_of_zero)then
              if(xybuf(m,l).gt.maxhist) maxhist=xybuf(m,l)
            endif
          enddo
        enddo
        xy_sl=255./log(maxhist+1.0)
        do l=1,mapxy
          do m=1,mapxy
              k=nint(log(xybuf(m,l)+1.0)*xy_sl)
              if(k.gt.255) k=255
              if(k.lt.0) k=0
              xybuf(m,l)=k
          enddo
        enddo
        k=nint(xy_offset)
        l=mapxy- nint(xy_offset) +1
        do i=1,mapxy
          xybuf(k,i)=128 ! draw y axis
          xybuf(i,l)=128 ! draw x axis
          xybuf(i,i)=128 ! draw diagonal
        enddo
c       write output file
        do l=1,mapxy             ! picture loop
          call xvwrit(xyunit,xybuf(1,l),status,' ')
        enddo
      endif

      end

c******************************************************************
      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

c******************************************************************
      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.)then
         call mabend('SPLINT: Wavelengths not unique')
      endif
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create spectoxyy.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM spectoxyy

   To Create the build file give the command:

		$ vimake spectoxyy			(VMS)
   or
		% vimake spectoxyy			(Unix)


************************************************************************/


#define PROGRAM	spectoxyy
#define R2LIB

#define MODULE_LIST spectoxyy.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create spectoxyy.pdf
process help=*

parm INP	count=3:10
parm OUT	count=3:4
parm CONVERT	type=real count=0:10 default=--
parm LAMDA	type=real count=0:10
parm SOURCE	type=real count=0:41 default=--
parm ILLUMIN	type=keyword valid=("D65","SUN") default="SUN"
parm MODE       type=keyword valid=("RADIANCE","REFLECT") default="RADIANCE"
parm RANGE      type=real valid=(.1:100.) default=1.0
end-proc

.title
VICAR2 program "spectoxyy"

.help
PURPOSE
Spectoxyy converts up to 10 registered multispectral images taken through
narrow band filters and either in units of spectral radiance or in
units of spectral reflectance and converts these into three images
representing x and y chromaticity coordinates and Y tristimulus.

EXECUTION EXAMPLES:
spectoxyy inp=(in1,in2,in3,in4) out=(x,y,Y) +
  convert=(.0001,.0001,.0001,.0001) lamda=(460,540,590,630) +
  illumin=SUN mode=REFLECT
or
spectoxyy inp=(in1,in2,in3,in4) out=(x,y,Y,hist) +
  convert=(1.,1.,1.,1.) lamda=(460,540,590,630) +
  illumin=D65 mode=RADIANCE

Note: "hist" is a 2 dimensional histogram of the chromaticity space.
The x and y axes pass through (0,0) and the diagonal passes through 
(0,1) and (1,0). The enclosed triangle contains the valid chromaticity
space. Each pixel or bin contains the log of the accumulated counts
falling on that location. Horizontal is x and vertical is y.

WRITTEN BY: 		J Lorre 5/23/96

THEORY:

Colorimetric units of xyY are obtainable from an input spectrum by
integrating the spectrum with color matching functions. 

If S is the radiance spectrum of the target,
   R is the reflectance spectrum of the target,
   I is the illuminating spectrum,
   Cx is the x color matching function, 
   Cy is the y color matching function, 
   Cz is the z color matching function, 

then one can compute tristimulus values Tx,Ty,Tz from:
   Tx=k*sum(S*Cx) radiance
   Ty=k*sum(S*Cy)
   Tz=k*sum(S*Cz)
or
   Tx=k*sum(R*I*Cx) reflectance
   Ty=k*sum(R*I*Cy)
   Tz=k*sum(R*I*Cz)

where k=100/sum(I*Cy)

Then x=Tx/(Tx+Ty+Tz)
     y=Ty/(Tx+Ty+Tz)
     Y=Ty

I is corrected for the distance of the target (RANGE keyword) from the sun
by dividing it by 1/(RANGE*RANGE), RANGE is in AU.
Notice that in reflectance mode the sun distance is ignored.

ILLUMINANTS:

You have a choice of two illuminants. These are:
1. D65 which represents a 
daylight spectrum on Earth with direct filtered sunlight and blue sky
scattering, and 
2. Unfiltered sunlight above the Earth's atmosphere.

With these you can reconstruct the color of a scene as it appears on Mars or
as it would appear on Earth (even if it isn't).

IMPLEMENTATION:

The program steps are as follows:

1. Reorder the input files into ascending wavelength order.
2. For each pixel:
3.	Compute the second derivative spline coefficients for interpolation.
4.	Loop on wavelength from 380 to 780 nm by 10 nm intervals.
5.		Interpolate spectrum amplitude.
6.		Integrate spectrum with color matching functions.
7.	Get x,y,Y
8.	Update 2-d chromaticity space histogram (option).
9. Write log of histogram.

Warning 1: There is no colorimetric absolute for Y tristimulus value.
Data typically ranges from 0 to 100, 100 being for a perfect Lambertian
surface. Calibrated devices expect
to see Y on the order of 10 to 90 or so. If your outputs for Y are wildly
far from this (like 20000 or .0003) then the display device cannot 
represent them. You can adjust your output using the CONVERT parameters, the
RANGE parameter, or just multiply the Y image by a constant. 

Warning 2: This program takes the highly unusual step of adjusting the D65
spectrum to match the solar spectrum above the atmosphere at the 780 nm
point. This is to help to get reasonable Y tristimulus values in the radiance
mode should you wish to use D65.

.level1
.vari INP
input image files
(3 to 10)

.vari OUT
output image files
(3 to 4)

.vari CONVERT
input image conversion factors
for radiance or reflectance.
Defaults to 1.0

.vari LAMDA
Wavelength of input
images in nanometers.
(ie: 556. for example)

.vari ILLUMIN
Illumination source
"D65" or "SUN"
Defaults to SUN

.vari MODE
Input image data.
"REFLECT" or "RADIANCE"
Defaults to RADIANCE

.vari RANGE
Range to the sun in AU.
Defaults to 1
Ignored in reflectance
mode.

.vari SOURCE
Illumination source
spectrum. Override
of ILLUMIN options.

.level2
.vari INP
INP = (input1,...,inputN), where the inputs are up to ten 
images in spectral radiance or spectral reflectance.  
The images must have been taken
with different filters and must all be the same size.

.vari OUT
Three output images in REAL format representing:
1. x chromaticity
2. y chromaticity
3. Y tristimulus
There is an optional fourth output file containing a 2-d histogram of
the chromaticity space. 

.vari CONVERT
Input image conversion factors for radiance or reflectance.
The input images are multiplied by these factors to convert them into
whatever units the software requires.
For reflectance images the software requires data from 0.0 to 1.0.
For radiance images the software requires data should be in units of 
Watts/meter**2/micron/srteradian and
should be numbers around 200 at 500 nanometers, if at an Earth distance.
CONVERT defaults to 1.0 for all inputs.

.vari LAMDA
Wavelength of the input images in nanometers.
(ie: 556. for example)

.vari ILLUMIN
Illumination source. Options are: "D65" or "SUN"
D65 represents daylight at average phase angles under the Earth's
atmosphere, including direst sunlight and blue sky scattering.
D65 is normalized to 100 at 560 nm.
SUN represents unfiltered sunlight above the Earth's atmosphere.
Defaults to SUN

.vari MODE
Input image data units. Options are: "REFLECT" or "RADIANCE"
Defaults to RADIANCE

.vari RANGE
Range to the sun in AU.
Defaults to 1
Ignored in reflectance mode.
Used to correct the illuminant ( D65 or SUN but not SOURCE) for the distance
to the sun in radiance mode only.

.vari SOURCE
Illumination source spectrum. One can override the provided  ILLUMIN options
with ones own spectrum. There must be 41 values beginning at 380 nm and 
ending at 780 nm.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstspectoxyy.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
local path type=string init="/project/test_work/testdata/gll/"
if ($SYSCHAR(1)="VAX_VMS")
  let PATH = "WMS_TEST_WORK:[TESTDATA.GLL]"
end-if 

!
spectoxyy inp=( +
  "&path"earth.red,  +
  "&path"earth.grn, +
  "&path"earth.blu) +
         out=(x.img,y.img,yy.img,hist.img) mode=radiance +
         convert=(1.,1.,1.) lamda=(660,560,430) illumin=sun
list inp=x.img linc=50 sinc=200
list inp=y.img linc=50 sinc=200
list inp=yy.img linc=50 sinc=200 
!
spectoxyy inp=( +
  "&path"earth.red,  +
  "&path"earth.grn, +
  "&path"earth.blu) +
         out=(x.img,y.img,yy.img,hist.img) mode=reflect +
         convert=(.00392,.00392,.00392) lamda=(660,560,430) illumin=sun
list inp=x.img linc=50 sinc=200 
list inp=y.img linc=50 sinc=200
list inp=yy.img linc=50 sinc=200 
!
end-proc
$ Return
$!#############################################################################
