      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

c EXAMPLE # 1
c Purpose: To test the Metropolis algorithm in three dimensions.
c The example below is to fit a circle to 6 data points, two
c of which do not lie on the circle.
c Notice the cost function rewards for including lots of data
c points and is allowed to reject points which fall far from the
c fitted function.
      real*4 range(3),answer(3),array(2,6),array2(1)
      character*80 msg
      external cost
      numten=1000
      limit=3000
      iprint=200
      narg=3
      norm=330
      npts=6
c sample,line pairs...
      data array/10.,50.,30.,50.,10.,70.,30.,70.,80.,30.,80.,40./
c (1)=radius (2)=x_center (3)=y_center
      data answer/30.,60.,50./
      data range/15.,20.,20./
      call xvmessage('Fortran test case 1',' ')

      call metropolis(cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      if(ind.eq.0)then
         write(msg,*)'answer=',(answer(j),j=1,narg)
         call xvmessage(msg,' ')
      else
         call xvmessage('no solution',' ')
      endif
      call xvmessage('solution should be about 14.1 20. 60.',' ')
      call xvmessage(' ',' ')
C
C     The "C" is not portable on all machines and therefore the
C     following:' CALL TZMETROPOLIS ' is commented our of the delivered
C     test procedure 
C
C      CALL TZMETROPOLIS 
C
      call example2
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
      real*4 array(2,n),array2(n),x(4),dr(10)

      ind=1
      if(x(1).lt.5.)return
      if(x(1).gt.50.)return
      if(x(2).gt.100.)return
      if(x(3).gt.100.)return
      if(x(2).lt.1.)return
      if(x(3).lt.1.)return
      sumdr=0.
      sum=0.
      range=20.
      m=0
      do j=1,n
        dr(j)=abs(x(1)-sqrt((array(1,j)-x(2))**2+(array(2,j)-x(3))**2))
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

      subroutine example2
c EXAMPLE # 2
c purpose to test the Metropolis algorithm in one dimension.
c The example below is to find the LOWEST minimum of the
c polynomial expressed by the COST function found at the
c end of this example.
      real*4 range(1),answer(1),array(1),array2(1)
      character*80 msg
      external e2cost
      range(1)=2.0
      numten=500
      answer(1)=3.
      limit=1500
      iprint=200
      narg=1
      norm=200
      npts=0
      call xvmessage('Fortran test case 2',' ')
      call metropolis(e2cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      write(msg,*)'answer=',answer(1)
      call xvmessage(msg,' ')
      call xvmessage('solution should be about -2.9',' ')
      return
      end


      subroutine e2cost(x,array,array2,n,error,ind)
      real*4 array(1),array2(1),x(1)
      ind=0
      error=x(1)**4-16*x(1)**2+5*x(1)
      return
      end
