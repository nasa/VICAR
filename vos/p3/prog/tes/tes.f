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

