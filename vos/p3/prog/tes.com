$!****************************************************************************
$!
$! Build proc for MIPL module tes
$! VPACK Version 1.8, Thursday, November 04, 2004, 19:12:38
$!
$! Execute by entering:		$ @tes
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
$ write sys$output "*** module tes ***"
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
$ write sys$output "Invalid argument given to tes.com file -- ", primary
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
$   if F$SEARCH("tes.imake") .nes. ""
$   then
$      vimake tes
$      purge tes.bld
$   else
$      if F$SEARCH("tes.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tes
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tes.bld "STD"
$   else
$      @tes.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tes.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tes.com -
	-s tes.f -
	-i tes.imake -
	-p tes.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tes.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C	27 Oct 2004  ...rea...	Removed limitation to halfword input data, and
C				for maximum input pixel value. Added HOT option.
C	05 Nov 2004  ...rea...  Changed the units of input pixels to Watts/
C				meter^2/steradian/micron (from milliWatts/...)
C				and change the output pixel values to unscaled
C				real*4, with emissivity ranging from 0.0 to 1.0,
C				and temperature in degrees Celsius. The HOT
C				option, now unnecessary, has been deleted.
C	Error checking:
C	If the derived emissivity from applying the regression equation is 
C	greater than 1.0 or less than 0.1 then the pixel is marked as bad and 
C	not processed further.
C	-1 : radiance before, during or after iteration falls below limit.
C	-3 : emissivity below 0.0
C	-4 : emissivity above 1.0 
C	
	include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      
      integer maxsamps, maxbands
      parameter (maxsamps=10000)
      parameter (maxbands=20)

      integer     nlo,nso,nli,nsi,slo,sso,iunit,ounit,stat,sb,nb,nbi,i
      integer     icount,idef,line,band,isamp, nTESLoops 
      integer     niter, eminidx(maxsamps)
      real*8      esum(maxsamps)
      real        c1, c2, pi, fact, acoeff, bcoeff, ccoeff 
      real        skyrad(maxbands), ntemps(maxsamps), eavg(maxsamps)
      real        wave(maxbands), aemis, ttemps(maxsamps)
      real        radiance(maxsamps,maxbands), bufin(maxsamps,maxbands)
      real        emissivity(maxsamps,maxbands)
      real        skycorrad(maxsamps,maxbands)
      real        mmd(maxsamps), emax(maxsamps), emin(maxsamps)
      character*8 format, tesequation
      real		rlimits(4)
      data		rlimits/0.0,20000000,-1,-2/
      integer		badpixel(maxsamps)
      real		elimits(4)
      data		elimits/0.0,1.0,-3,-4/

C     Echo the version when the program runs and abort with an 
C     Error message if a vicar routine fails      
      call ifmessage ('TES (last modified 2004-11-05)')
      call xveaction ('sa', ' ')

C     Open input data set
      call xvunit(iunit,'inp',1,stat,' ')
      call xvopen(iunit,stat,'u_format','real',' ')

C     Get data format and check that its in real format
      call xvget(iunit,stat,'format',format,' ')
      if(format.ne.'REAL') call mabend(
     +		'TES now requires that input pixels be in REAL format')

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
      
C     Check there are less than maxbands and less than maxsamps
C     samples, if not - abort
      if(nsi.gt.maxsamps) then
      	call mabend('sorry image larger than max # samples')
      end if
      if(nbi.gt.maxbands) then
      	call mabend('sorry image larger than max # bands')
      end if
      
C     Get the wavelength vals and make sure there are as many 
C     values as there are bands
      call xvparm('wave',wave,icount,idef,maxbands)
      if(icount.ne.nbi) then
      	call mabend('sorry # of wave vals must equal # input bands')
      end if
      
C     Get the sky radiance vals and make sure there are as many 
C     values as there are bands
      call xvparm('skyrad',skyrad,icount,idef,maxbands)
      if(icount.ne.nbi) then
      	call mabend('sorry # of skyrad vals must equal # input bands')
      end if
     
C     Get the Assumed emissivity
      call xvparm('aemis',aemis,icount,idef,1)
      if(aemis.LT.0 .OR. aemis.GT.2) then
      	call mabend('sorry assumed emissivity must be between 0 and 2')
      end if
      
C     Get the number of TES iterations
      call xvparm('niter',niter,icount,idef,1)
      if (niter.LT.1) then
        print *, 'Minimum number iterations is 1'
        print *, 'NITER reset to 1'
      end if
C
C	Get the TES equation
	call xvparm('tesequation',tesequation,icount,idef,1)
	if (tesequation.EQ.'ASTER') then
		acoeff = 0.994
		bcoeff = 0.687
		ccoeff = 0.737 
	else if (tesequation.EQ.'MASTER10') then
		acoeff = 1.001
		bcoeff = 0.761
		ccoeff = 0.812
	else
		acoeff = 0.990
		bcoeff = 0.757
		ccoeff = 0.834
	end if
C
C     Open output data set, make it one band larger than the input
      call xvunit(ounit,'out',1,stat,' ')
      call xvopen(ounit,stat,'op','write','u_org','bil',
     &            'u_format','real','o_format','real',
     &            'u_nl',nlo,'u_ns',nso,'u_nb',nb+1,' ')

C     Set the constants 
      c1 = 3.74151e-16    !in w/m2
      c2 = 0.0143879
      pi = 3.1416
      fact = 1.0e-6

C     Convert wavelength to metres  
      do i = 1,nb
        wave(i) = wave(i)*fact         
      enddo
      
C     Convert the skyrad to W/m3 and divide by pi
      do i = 1,nb
	  skyrad(i) = 1000.0*skyrad(i)/pi         
      enddo

      print *, ''
      print *, 'This program requires your data to be'
      print *, 'atmospherically corrected and in'
	print *, 'milliwatts/(m*m*sr*micrometer) and'
      print *, 'and your sky radiance values in' 
	print *, 'milliwatts/(m*m*micrometer)'
      
      print *, ''
      print *, 'The assumed emissivity is:',aemis
      
      print *, 'The wavelengths and sky radiances are:'
      do i=1,nb
        print *, wave(i)/fact, '  ', pi*skyrad(i)/1000.0
      enddo
      
      print *, ''
      print *, 'The number of TES iterations =',niter
      print *, ''

	print *, ''
	print *, 'The ',tesequation, ' empirical equation used:'
	print *, acoeff, '-', bcoeff, '*MMD^',ccoeff
      
C     For each line calculate:
C       the normalized temperature,
C       iterate from the normalized temperature niter times
C       ratio of the iterated emissivity to the mean of iterated emissivities
C       the MMD of the ratio
C       the regressed emin
C       the emin temperature
C       the emissivities in the other channels using
C         the emin temperature.
 
      do  line = slo,slo+nlo-1
      
C       Read a full line of data, and convert to double radiance W/m*m*m/sr
        do band = 1,nb
          call xvread(iunit,bufin(1,band),stat,'line',line,
     1      'band',band,'samp',sso,'nsamps',nso,' ')
     	  call scale(bufin(1,band),radiance(1,band),nso)
        end do
        
C       Copy the assumed emissivity into the iteration array
	  call mve(7,maxsamps*maxbands,aemis,emissivity,0,1)
	  
C	  Create a bad pixel array, initialized to 0
	  call zia(badpixel,nso)            
	
C       Iterate the sky correction niter times
        do nTESLoops=1,niter
           
C	  print *, nTESLoops

C         Set ntemps to low values so first temp is saved       	  
      	  call mve(7,nso,-1.0e10,ntemps,0,1)	
      	  
      	  do band = 1, nb
C           Correct for the reflected sky radiance
      	    call skycorr(radiance(1,band), emissivity(1,band),
     1 	      skycorrad(1,band), skyrad(band), nso, rlimits, badpixel)
C           Calculate the normalized temperature, save maxT
            call normt(wave(band), skycorrad(1,band), 
     1        emissivity(1,band), c1, c2, pi, ntemps, nso, badpixel)
          end do
            
C         Calculate the new emissivities using the normalized temperature
	  do band = 1, nb
	    call norme(wave(band), skycorrad(1,band), emissivity(1,band), 
     1                 c1, c2, pi, ntemps, nso, badpixel)
          end do
                    
        end do 
C       End TES niter loop

C       Radiance now corrected for reflected sky radiation so calculate
C       the tes temperatures and emissivities.
        call eminindex(emissivity, eminidx, emin, nso, nb, maxsamps, 
     1                 maxbands, badpixel)
        call normeavg(emissivity, esum, eavg, nso, nb, maxsamps, 
     1                maxbands, badpixel)
        call ratio(emissivity, eavg, nso, nb, maxsamps, maxbands, 
     1	           badpixel)
        call minmaxdiff(emissivity, emax, emin, mmd, nso, nb, 
     1                  maxsamps, maxbands, badpixel)
        call tesemin(mmd, emin, nso, acoeff, bcoeff, ccoeff, badpixel, 
     1	             elimits)
        call testem(skycorrad, emin, eminidx, ttemps, maxsamps, 
     1              maxbands, wave, nso, nb, c1,c 2, pi, badpixel)
        call tesemis(emissivity, skycorrad, ttemps, maxsamps, maxbands, 
     1               wave, nso, nb, c1, c2, pi, badpixel)
      
C       Flag bad pixels, change from Kelvin to Celsius
	do isamp = 1,nso
	  if (badpixel(isamp) .ne. 0) then
	    do band = 1,nb
	      emissivity(isamp,band) = badpixel(isamp)
	      ttemps(isamp) = badpixel(isamp)
	    end do
	  else
	    ttemps(isamp) = ttemps(isamp) - 273.15
	  end if
	end do
	
C       write out
        do band = 1,nb         
          call xvwrit(ounit,emissivity(1,band),stat,'nsamps',nso,' ')
        end do
        call xvwrit(ounit,ttemps,stat,'nsamps',nso,' ')

C	Call the lap counter	  
	call lapcounter(line)
                                             
      end do

C     Clear out the lap counter
      write(*,*) 

C     Close data sets
      call xvclose(iunit,stat,' ')
      call xvclose(ounit,stat,' ')
C
      return
      end
      
C*******************************************************************************
CC    Subroutine to scale the data to w/m*m*m/sr
      subroutine scale(bufin, r, nso)      
      implicit none
      
      integer nso, sample 
      real bufin(nso), r(nso)

      do sample = 1,nso
C       Convert the input into w/m*m*m/sr
        r(sample) = bufin(sample) * 1.0e+6
C        print *, r(sample)               
      end do
      
      return
      end

C*******************************************************************************
CC    Subroutine to correct for the reflected sky radiation
      subroutine skycorr(r, e, scr, skyrad, nso, rlimits, badpixel)      
      implicit none
      
      integer nso, sample
	real r(nso), e(nso), scr(nso), skyrad, rlimits(4)
	integer badpixel(nso)
	
C	print *, 'In skycorr' 

      do sample = 1,nso
C	  print *, rlimits(1), r(sample)
C       Correct for the reflected sky radiation, provided within range
		if (r(sample).LE.rlimits(1)) then 
			badpixel(sample)=rlimits(3)
		else
C			print *, r(sample), e(sample), skyrad
			scr(sample) = r(sample) - ((1-e(sample))*skyrad)
C			print *, r(sample), e(sample), skyrad, scr(sample)
		end if 
      end do

	do sample = 1,nso
		if (scr(sample).LE.rlimits(1)) then 
			badpixel(sample)=ifix(rlimits(3))
		end if
	end do
	
C	print *, 'Out skycorr'
      
      return
      end      
      
C*******************************************************************************
CC    Subroutine to calculate the normalized temperature for each pixel
C     and save the highest t
      subroutine normt(w, r, e, c1, c2, pi, ntemps, nso, badpixel)      
      implicit none
      
      integer nso, sample
	real w, r(nso), e(nso), c1, c2, pi,  ntemps(nso), t
	integer badpixel(nso) 

C	print *, 'In normt'

      do sample = 1,nso        
C       Calculate the temperature
C		print*, e(sample), r(sample)
		if (badpixel(sample).EQ.0) then
		  t=c2/(w*log(((e(sample)*c1)/(r(sample)*w**5.0*pi))+1))    
		  if (t.GT.ntemps(sample))ntemps(sample)=t ! Save the max
C           print *, t
		end if              
      end do

C	print *, 'Out normt'
      
      return
      end
      
C*******************************************************************************
CC    Subroutine to calculate the normalized emissivity for each pixel
      subroutine norme(w, r, e, c1, c2, pi, ntemps, nso, badpixel)      
      implicit none
      
      integer nso, sample 
	real w, r(nso), e(nso), c1, c2, pi, ntemps(nso)
	integer badpixel(nso)

C	print *, 'In norme'

      do sample = 1,nso        
C       Calculate the emissivity
C        print *, sample, ntemps(sample), r(sample), w
		if (badpixel(sample).EQ.0) then
            e(sample)=r(sample)/
     1        (c1/(w**5.0*pi*(exp(c2/(w*ntemps(sample)))-1)))
	    end if
          
C       print *, ntemps(sample), r(sample), w, e(sample), sample
      end do

C	print *, 'Out norme'
      
      return
      end
      
C*******************************************************************************
CC    Subroutine to calculate an index to the minimum emissivity      
      subroutine eminindex(e, eindex, emin, nso, nb, maxsamps, maxbands,
     +			   badpixel)
      implicit none
      
      integer nso, nb, sample, band, maxsamps, maxbands, eindex(nso)
      real e(maxsamps,maxbands), emin(nso)
	integer badpixel(nso)

C	print *, 'In eminindex'
      
C     Initialize the emin array and index array
      call mve(7,nso,+1.0e10,emin,0,1)
      call zia(eindex,nso)
      
C     Calculate the emin and eminindex
      do band = 1,nb
        do sample = 1,nso
C          print *, e(sample,band), sample, band, emin(sample)
			if (badpixel(sample).EQ.0) then  
				if (e(sample,band) .LT. emin(sample)) then
				emin(sample) = e(sample,band)
				eindex(sample) = band
C				print *, eindex(sample) 
				end if
			end if
        end do           
      end do
      
C      do sample = 1,nso
C        print *, emin(sample), eindex(sample)
C      end do

C	print *, 'Out eminindex'
        
      return
      end
            
C*******************************************************************************
CC    Subroutine to calculate the average of the normalized emissivities 
      subroutine normeavg(e, esum, eavg, nso, nb, maxsamps, maxbands, 
     +			  badpixel)
      implicit none
      
      integer nso, nb, sample, band, maxsamps, maxbands
	real e(maxsamps,maxbands), eavg(nso)
      real*8 esum(nso)
	integer badpixel(nso)
      
C	print *, 'In normeavg'


C     Initialize the sum array
      call zia(esum,2*nso) 

C     Calculate the sum
      do band = 1,nb
        do sample = 1,nso
		if (badpixel(sample).EQ.0) then   
			esum(sample) = esum(sample)+e(sample,band)
		end if
        end do           
      end do
      
C     Calculate the mean of the sum of the BB radiances
      do sample = 1,nso
		if (badpixel(sample).EQ.0) then
			eavg(sample)=real(esum(sample)/dble(nb))
		end if
      end do

C	print *, 'Out normeavg'
        
      return
      end
             
C*******************************************************************************
CC    Subroutine to calculate the ratio of the normalized emissivities
CC    to the average of the normalized emissivities 
      subroutine ratio(e, eavg, nso, nb, maxsamps, maxbands, badpixel)
      implicit none
      
      integer nso, nb, sample, band, maxsamps, maxbands
	real e(maxsamps, maxbands), eavg(nso)
	integer badpixel(nso)

C	print *, 'In ratio'
      
C     Calculate the ratio
      do sample = 1,nso
		if (badpixel(sample).EQ.0) then
			do band = 1,nb   
				e(sample,band) = e(sample,band)/eavg(sample)
C				print *, e(sample,band)
			end do
		end if           
      end do

C	print *, 'Out ratio'
        
      return 
      end
      
C*******************************************************************************
C     Subroutine to calculate the MMD
      subroutine minmaxdiff(e, emax, emin, mmd, nso, nb, maxsamps, 
     +  maxbands, badpixel)
      implicit none
      
      integer nso, nb, sample, band, maxsamps, maxbands
	real e(maxsamps,maxbands), mmd(nso)
      real emax(nso), emin(nso)
	integer badpixel(nso)

C	print *, 'In minmaxdiff'
      
C     Initialize the sum array
      call mve(7,nso,-1.0e10,emax,0,1)
      call mve(7,nso,+1.0e10,emin,0,1) 

C     Calculate the max and min
      do band = 1,nb
        do sample = 1,nso  
		if (badpixel(sample).EQ.0) then 
			if (e(sample,band) .GT. emax(sample)) then
				emax(sample) = e(sample,band)
			end if
			if (e(sample,band) .LT. emin(sample)) then
				emin(sample) = e(sample,band)
			end if
		end if
        end do           
      end do      
           
C     Calculate the MMD
      do sample = 1,nso
		if (badpixel(sample).EQ.0) then
			mmd(sample)=emax(sample)-emin(sample)
C			print *, mmd(sample)
		end if
      end do

C	print *, 'Out minmaxdiff'
        
      return
      end
      
C*******************************************************************************
CC    Subroutine to calculate the emin for each pixel
      subroutine tesemin(mmd, emin, nso, acoeff, bcoeff, ccoeff, 
     1		badpixel, elimits)
      implicit none
      
      integer nso, sample
	real mmd(nso), emin(nso), elimits(4)
	real acoeff, bcoeff, ccoeff
	integer badpixel(nso)

C	print *, 'In tesemin'
      
C	print *, acoeff, bcoeff, ccoeff
C     Calculate the mmd
      do sample =1,nso
		if (badpixel(sample).EQ.0) then
			emin(sample)=acoeff-bcoeff*mmd(sample)**ccoeff
C			Emissivity must be GT than elimits(1) and LT elimits(2)
			if (emin(sample).LE.elimits(1)) then 
				badpixel(sample)=ifix(elimits(3))
			end if 
			if (emin(sample).GE.elimits(2)) then
				badpixel(sample)=ifix(elimits(4))
			end if
C			print *, emin(sample)
		end if     
      enddo
	
C	print *, 'Out tesemin'     
        
      return
      end
      
C*******************************************************************************
CC    Subroutine to calculate the tes temperatures for each pixel
      subroutine testem(skycorr, emin, eminidx, ttemps, maxsamps, 
     +  maxbands, wave, nso, nb, c1, c2, pi, badpixel)
      implicit none
            
      integer maxsamps, maxbands, nso, nb, sample, eminidx(nso)
      real wave(nb), c1, c2, pi, r, w, e 
      real skycorr(maxsamps,maxbands), emin(nso)
      real ttemps(nso)
	integer badpixel(nso)

C	print *, 'In testem'
      
C     Create an index in the emissivity array to the min emissivity for each
C     pixel
      do sample = 1,nso
		if (badpixel(sample).EQ.0) then
C			print *,skycorr(sample,eminidx(sample)),eminidx(sample),
C     1			wave(eminidx(sample)), emin(sample)
		
			r = skycorr(sample, eminidx(sample))
			w = wave(eminidx(sample))
			e = emin(sample)
			ttemps(sample) = c2/(w*log(((e*c1)/(r*w**5.0*pi))+1))
C			print *, ttemps(sample)
		endif              
      end do
	
C	print *, 'Out testem'      
                        
      return 
      end
      
C*******************************************************************************
CC    Subroutine to calculate the tes emissivities for each pixel
      subroutine tesemis(e, sc, ttemps, maxsamps, maxbands, 
     +  wave, nso, nb, c1, c2, pi, badpixel)
      implicit none
            
      integer maxsamps, maxbands, nso, nb, sample, band
      real wave(nb), c1, c2, pi, w, r, t 
      real sc(maxsamps,maxbands), e(maxsamps, maxbands)
      real ttemps(nso)
	integer badpixel(nso)

C	print *, 'In testemis'
      
C     Use the tes temperature to determine the emissivities
      do band = 1,nb
        w = wave(band)
        do sample = 1,nso
		if (badpixel(sample).EQ.0) then
			r = sc(sample,band)
			t = ttemps(sample)
C			print *, w, r, t
			e(sample,band)=r/(c1/(w**5.0*pi*(exp(c2/(w*t))-1)))
C			print *, e(sample,band)
		end if
        end do           
      end do  
	
C	print *, 'Out testemis'    
                        
      return 
      end
      
C*******************************************************************************
C	Subroutine to echo the line being processed.
      subroutine lapcounter(line)
C
      byte erase(20)/20*8/
C
      write(*,111) erase, line
 111  format(20a1,'Processing Line',i5,$)
 
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tes.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM tes
   To Create the build file give the command:
		$ vimake tes			(VMS)
   or
		% vimake tes			(Unix)
************************************************************************/
#define PROGRAM	tes
#define R2LIB

#define MODULE_LIST tes.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
/* #define INCLUDE_LIST tes.fin */
/* #define FTNINC_LIST fortport */

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create tes.pdf
process help=*
PARM INP TYPE=(STRING,60) COUNT=1
PARM OUT TYPE=(STRING,60) COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER COUNT=1 DEFAULT=1
PARM SS TYPE=INTEGER COUNT=1 DEFAULT=1
PARM NL TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NS TYPE=INTEGER COUNT=1 DEFAULT=0
PARM AEMIS TYPE=REAL COUNT=1 DEFAULT=0.96
PARM WAVE TYPE=REAL COUNT=(0:20) DEFAULT=--
PARM SKYRAD TYPE=REAL COUNT=(0:20) DEFAULT=--
PARM NITER TYPE=INTEGER COUNT=1 DEFAULT=5
PARM TESEQUATION TYPE=KEYWORD VALID=(ASTER,MASTER10,MASTER8) DEFAULT=ASTER COUNT=1

END-PROC
.TITLE
TES (last modified 2004-11-05)
.HELP
TES uses the TES algorithm to calculate the emissivity and temperature
from an input dataset. The input dataset must be in radiance units of
Watts per square meter per steradian per micrometer.  This is different
from versions of the program in effect prior to November, 2004.  As a
consequence, input pixels should be real (floating point) values.

The output has one more band than the input. This extra band contains
the tes temperature (the last band), the other bands contain the tes 
emissivity.

Testing
This program was tested on an artificial 2x2x6 band radiance dataset. 

RESTRICTIONS;
1. Maximum size of one band of the input is 10,000 values.
2. Maximum number of bands in the input is 20.
3. If the input radiance is LE zero (mW/m*m/um/sr) the pixel is considered bad
   and not processed. Bad pixel emissivities are set to -1 and -2 respectively.
4. If the emissivity calculated from the regression is LE zero or GE 1.0, the
   pixel is considered bad and set to -3 and -4 respectively. 

HISTORY
Written by Simon J. Hook in FORTRAN.
27 Oct 2004  ...rea...     Modified to accept FULLWORD and REAL input pixels,
                           and remove the restriction of 20,000 for maximum
                           input pixel value.
                           HOT keyword added.
 5 Nov 2004  ...rea...     Modified to treat both input and output pixels as
                           unscaled values.  Output pixels are now real (rather
                           than scaled integer) values.
.LEVEL1
.VARIABLE INP
Input radiance dataset in
radiance (W/m*m/um/sr)
.VARIABLE OUT
Output tes emissivity dataset 
plus an extra band including the
tes temperature. 
Emissivity range: 0.0 to 1.0
Temperature in degrees Celsius
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
.VARIABLE EMIS
the assumed emissivity for the
calculating the normalized 
temperature. Typically 0.97
.VARIABLE WAVE
the central wavelength for the 
input channels, in micrometers.
(1 value per channel)
.VARIABLE SKYRAD
the downwelling sky radiance
values in mW/m**2/um/sr as 
output by TIMSCAL2
(1 value per channel)
.VARIABLE NITER
The number of TES iterations
.VARIABLE TESEQUATION
MASTER or ASTER TES equation

.LEVEL2
.VARIABLE WAVE
The centroid wavelength value for each channel in micrometers. This can be 
calculated for a TIMS image using TIMSRESP (The central wavelength values are
dumped to the screen after the program runs).

.VARIABLE TES
TES uses an empirical calibration curve to calculate the emissivity minimum 
from the temperature independant MMD ratio. This calibration curve was derived 
by convolving ASTER library spectra to the ASTER or MASTER channels and 
plotting the MMD against the emissivity minimum. The empirical formula has the 
form:

a-b*MMD^c

where the coefficients (a, b and c) are determined for either ASTER or MASTER. 
In the caseof MASTER they are provided for the case where you have all 10 
MASTER thermal channels and also the case where you have the inner 8 MASTER 
channels. This latter case is provided since the shortest and longest 
wavelength MASTER channels are strongly influenced by the atmosphere and often 
a better result is obtained by excluding these channels from TES since they 
typically contain residual atmospheric effects which will negatively influence 
the result.

ASTER 6 band
a = 0.994
b = 0.687
c = 0.737 

MASTER 10 band
a = 1.001
b = 0.761
c = 0.812

MASTER 8 band
a = 0.990
b = 0.757
c = 0.834

where MMD is the maximum difference of the beta ratio and ^ = raise to power.

For more information about this algorithm see 
1) Temperature/Emissivity Separation (TES) Algorithm Theoretical Basis Document.
   Version 2.0 by Gillespie, Rokugawa, Hook, Matsunaga and Kahle.
2) Gillespie, A., Rokugawa, S. Matsunaga, T., Cothern, S, Hook, S. and A.
   Kahle, 1998. A Temperature and Emissivity Separation Algorithm for Advanced 
   Spaceborne Thermal Emission and Reflectance Radiometer (ASTER) Images. IEEE 
   Transactions on Geoscience and Remote Sensing, vol. 36 pp. 1113-1126.
3) Hook, S. J., Dmochowski, J. E., Howard, K. A., and Rowan, L. C. and K. E.
   Karlstrom, 2002. Mapping Weight Percent Silica Variation from Remotely 
   Acquired Multispectral Thermal Infrared Data with Examples from the Hiller
   Mountains, Nevada, USA and Tres Virgenes-La Reforma, Baja California Sur,
   Mexico. In prep.

.END
$ Return
$!#############################################################################
