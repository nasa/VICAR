      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      
      integer maxsamps, maxbands
      parameter (maxsamps=10000)
      parameter (maxbands=20)

      integer     nlo,nso,nli,nsi,slo,sso,iunit,ounit,stat,sb,nb,nbi,i
      integer     icount,idef,line,band,goodvalue(maxsamps)
      integer*2   bufin(maxsamps,maxbands),bufout(maxsamps)
      integer*2   avgout(maxsamps),tout(maxsamps)
      real*8      avgbuf(maxsamps),residual(maxsamps,maxbands)
      real*8      mina(maxsamps), maxa(maxsamps), amean(maxsamps)
      real*8      emissivity(maxsamps)
      real        c1,c2,pi,fact,a,b,c
      real        wave(maxbands),weight(maxbands)
      character*8 format      

C     Echo the version when the program runs and abort with an 
C     Error message if a vicar routine fails      
      call ifmessage ('AMER Version 21-Jan-96')
      call xveaction ('sa', ' ')

C     Open input data set
      call xvunit(iunit,'inp',1,stat,' ')
      call xvopen(iunit,stat,'u_format','half',' ')

C     Get data format and check that its in halfword
      call xvget(iunit,stat,'format',format,' ')
      if(format.ne.'HALF') then
         call mabend('betae expects halfword radiance data input')
      end if

C     Get the size of the ouput
      call xvsize(slo,sso,nlo,nso,nli,nsi)
      call xvbands(sb,nb,nbi)
      
C     Check the number of samples requested does not exceed the 
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

C     Open output data set, make it one band larger than the input
      call xvunit(ounit,'out',1,stat,' ')
      call xvopen(ounit,stat,'op','write','u_format','half',
     &            'u_nl',nlo,'u_ns',nso,'u_nb',nb+1,'u_org','bil',' ')

C     Set the constants 
      c1 = 3.74151e-16    !in w/m2
      c2 = 0.0143879
      pi = 3.1416
      fact = 1.0e-6
C
      print *, ''
      print *, 'This program requires your data to be'
      print *, 'atmospherically corrected and in'
      print *, 'mw/m*m/um/sr'
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
      print *,'Wavelengths and weights are:'
      do i = 1,nb
        weight(i) = (wave(i)*(log(c1))) 
     1              - (5.0*wave(i)*(log(wave(i))))  
     2              - (wave(i)*(log(pi))) - a + b + c
        weight(i) = - weight(i)          
      print*, wave(i)/fact, '   ', weight(i)
      enddo
      print *, ''

C     For each line first calculate:
C       the wavelength weighted mean, 
C       the alpha residual,
C       the alpha mean,
C       the average-MMD emissivity
C       the average-MMD temperature
C       the emissivities from the average-MMD temperature
 
      do  line = slo,slo+nlo-1
      
C         Zero the arrays used in summations
          call zia(avgbuf,2*nso)
          call zia(mina,2*nso)
          call zia(maxa,2*nso)
          call zia(goodvalue,nso)
          
C         Calculate the wavelength weighted log sum 
      	  do band = 1,nb
            call xvread(iunit,bufin(1,band),stat,'line',line,
     1        'band',band,'samp',sso,'nsamps',nso,' ')
            call arsum(wave(band), bufin(1,band), avgbuf, 
     1        goodvalue, nso)
          end do
          
C         Calculate the wavelength weighted log mean
          call armean(avgbuf, goodvalue, avgout, nso, nb) 
          
C         Calculate the alpha residual and stats for the variance
     	  do band = 1,nb
            call ar(wave(band), weight(band), bufin(1,band), avgbuf, 
     1        residual(1,band), nso)
            call arstats(residual(1,band),nso,fact,mina,maxa,band)
          end do
          
C         Calculate the alpha mean from the variance
          call am(mina,maxa,amean,nso,nb,fact)
                    
C         Calculate the Average-MMD emissivity from the alpha mean,
C         Note only need one channel since all temperatures are same
C         because using weins approximation.
          call ae(wave(1),residual(1,1),amean,nso,nb,fact,
     1        emissivity)           
          
C         Calculate the Average-MMD temperature. Note only need 
C         one channel since all temperatures are same
C         because using weins approximation.
          call atemp(bufin(1,1),emissivity,wave(1),nso,nb,c1,c2,pi,
     1        tout) 
                    
C         Calculate the emissivity from the temperature and write out
C         the emissivity
          do band = 1,nb         
            call getemis(bufin(1,band),tout,wave(band),nso,c1,c2,pi,
     1        bufout)
            call xvwrit(ounit,bufout,stat,'nsamps',nso,' ')
          end do

C         Write out the temperature
          call xvwrit(ounit,tout,stat,'nsamps',nso,' ')
                          
      end do

C     Close data sets
      call xvclose(iunit,stat,' ')
      call xvclose(ounit,stat,' ')
C
      return
      end
      
C     Subroutine to calculate the wavelength weighted log sum for all bands
      subroutine arsum(wave, bufin, avgbuf, goodvalue, nso)
      implicit none
      
      real wave
      real*8 avgbuf(nso), avg
      integer*2 bufin(nso)
      integer goodvalue(nso), nso, sample 

C     Calculate the wavelength weighted sum
      do sample = 1,nso
C       Zero avg
        avg = 0.0d0
C       Convert the input into w/m*m*m/sr
        avg = dble(bufin(sample) * 1.0e+3)
C       Wavelength weight the radiance
        if(avg.gt.0.0)then
          avg = dlog(avg)*dble(wave)       
          avgbuf(sample) = avgbuf(sample)+avg
          goodvalue(sample)=goodvalue(sample)+1
        endif            
      end do
      return
      end
 
C     Subroutine to calculate the wavelength weighted log 
C     mean (1 value per pixel)
      subroutine armean(avgbuf, goodvalue, avgout, nso, nb)
      implicit none
      
      real*8 avgbuf(nso)
      integer goodvalue(nso)
      integer*2 avgout(nso)
      integer nso, nb, sample

C     Calc the mean      
      do sample = 1,nso
        if(goodvalue(sample).eq.nb) then
          avgbuf(sample)=(avgbuf(sample)/dble(goodvalue(sample)))
        else
          avgbuf(sample)=0
        endif
C       Create a halfword version for output
        avgout(sample)=avgbuf(sample)*1.0e+7    
      end do
      return 
      end
      
C     Subroutine to calculate the alpha residual 
      subroutine ar(wave, weight, bufin, avgbuf, residual, nso)
      implicit none
      
      real wave, weight
      real*8 avgbuf(nso), rad, residual(nso)
      integer*2 bufin(nso)
      integer nso, sample

C     Calculate the alpha residual   
      do sample = 1,nso
C       Convert the input into w/m*m*m/sr      
        rad =dble(bufin(sample))*1.0e+3
C        print *,  rad, wave, avgbuf(sample), weight
C     Calculate the alpha residual
        residual(sample)=
     1     dble(wave)*dlog(rad)-avgbuf(sample)+dble(weight)
C      print *, residual(sample)
      end do
        
      return 
      end
       
C     Subroutine to calculate the stats for getting the mmd 
      subroutine arstats(residual, nso, fact, mina, maxa, pass)
      implicit none
      
      real*8 residual(nso), mina(nso), maxa(nso)
      real fact
      integer nso, sample, pass
      
C     If its the first pass, set all the values
      if (pass .EQ. 1) then
        do sample =1,nso
          mina(sample)=residual(sample)
          maxa(sample)=residual(sample)
        enddo
      endif  
      
C     Calculate the max and min
      do sample = 1,nso
        if (mina(sample) .GT. residual(sample)) 
     1    mina(sample) = residual(sample)
        if (maxa(sample) .LT. residual(sample)) 
     1    maxa(sample) = residual(sample)               
      end do
        
      return 
      end
      
C     Subroutine to calculate the Average-MMD emissivity mean
C     The MMD is divided by fact since this form was used in 
C     calculating the empirical calibration curve
      subroutine am(mina,maxa,amean,nso,nb,fact)
      implicit none
            
      integer nso, nb, sample
      real*8 mina(nso), maxa(nso), amean(nso)
      real mmd, fact  
      
C     Calculate the variance and derived alpha mean   
      do sample = 1,nso
        mmd = maxa(sample)-mina(sample)
C     Linear approx - not used.   
C        amean(sample) = -0.50317 * (mmd/fact) - 0.23615
C        print *, amean(sample)
C     Power approx   
        amean(sample) = -0.790683 * ((mmd/fact)**0.723098)
C        print *, amean(sample)

      end do
      
      return 
      end
       
C     Subroutine to calculate the Average-MMD emissivity
C     The residual is divided by fact since this form was used in 
C     calculating the amean from the empirical calibration curve

      subroutine ae(wave,residual,amean,nso,nb,fact,emissivity)
      implicit none
            
      integer nso, nb, sample
      real*8 residual(nso), amean(nso), emissivity(nso), w      
      real fact, wave      
            
C     Calculate the emissivity
C     Convert the wavelength and residual to microns for Pete's method
      w=dble(wave/fact)
      do sample = 1, nso
        emissivity(sample) = 
     1    dexp(((residual(sample)/dble(fact)) + amean(sample)) / w)

      enddo
             
      return 
      end

C     Subroutine to calculate the sum of the Average-MMD derived temperatures
C     Note if weins approximation is used then all temps are the same.
      subroutine atemp(bufin,emissivity,wave,nso,nb,c1,c2,pi,tout)
      implicit none
            
      integer nso, nb, sample
      integer*2 bufin(nso), tout(nso)
      real*8 emissivity(nso)     
      real temp, c1, c2, pi, wave, rad      
            
C     Calculate the temperature
      do sample = 1, nso
C       Convert the input into w/m*m*m/sr
        rad = float(bufin(sample)) * 1.0e+3

C       Calculate the temperature with the Wein approximation
        temp = (c2/(wave*log(((emissivity(sample)*c1)/
     1    (rad*wave**5.0*pi))))) - 273.15 
        tout(sample)=ifix(temp*100)

      enddo
             
      return 
      end
      
C     Subroutine to calculate the emissivity from a temperature and radiance  
      subroutine getemis(bufin,tout,wave,nso,c1,c2,pi,bufout)
      implicit none
            
      integer nso, sample
      integer*2 tout(nso), bufin(nso)
      integer*2 bufout(nso)
      real wave, c1, c2, pi, r, e, t

C     Calculate the radiance for each wavelength then calculate the
C     emissivity
        do sample = 1, nso
          t = (tout(sample)/100.0) + 273.15
          r = c1 / (wave**5.0 * pi * (exp(c2/(wave*t))-1.0))
          e =  float(bufin(sample))*1.0e3 / r
C          print *, r, bufin(sample)*1.0e3
          bufout(sample) = e * 1.0e4        
            
      enddo
             
      return 
      end
