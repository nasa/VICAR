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
