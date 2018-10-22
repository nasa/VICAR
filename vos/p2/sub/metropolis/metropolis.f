      subroutine metropolis(cost,narg,array,array2,range,numten,answer,
     +                       limits,norm,npts,prnt,ind)

C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                       METROPOLIS
C       ----------------                                       ----------
C	Anneals the cost function by iterative guessing of ANSWER using
C       a monte carlo generator with continually decreasing bounds.
C
C	Fortran format of call:
C
C	CALL metropolis(cost,narg,array,array2,range,numten,answer,
C                       limits,norm,npts,prnt,ind)
C
C	"C" format of call:
C
C	zmetropolis(cost,narg,array,array2,range,numten,answer,
C                  limits,norm,npts,prnt,ind)
C
C	Parameters:-
C
C          COST  A function provided by the user (see test examples).
C          NARG  The number of variables. (Integer)
C          ARRAY An array of data. (Float)
C          ARRAY2 Another array of data. (Float)
C          RANGE Bounds of solution vector elements. (Integer) 
C          NUMTEN The number of iterations to reduce the error by 10. (Integer)
C          ANSWER The solution vector. (Float could be array)
C          LIMITS The total number of iterations permitted. (Integer)
C          NORM The number of iterations between normalizations. (Integer)
C          NPTS The number of points in ARRAY & ARRAY2. (Integer)
C          PRNT The number of iterations between printouts. (Integer)
C          IND  Status indicator. (Integer)
C
C   REVISION HISTORY
C
C      14-05-94   CRI  MSTP S/W Conversion (VICAR Porting)
C      19-05-95   CRI  Correction per FR 85782
C                      See help file for "C" bridge support
C      14-07-95   CRI  Correction per FR 87256 Removed hard coded seed line
C
C   REQUIRED SUBROUTINE cost (provided by the user):
C
C     Returns the cost function resulting from guessing x (answer).
C     This could be any function or logical operation.
C     The user supplies this subroutine. The purpose of COST is
C     to return a penalty measure which METROPOLIS uses as it tries
C     to feel it's way to the solution by guessing at values for
C     the solution vector X. METROPOLIS will select that solution vector
C     which results in the smallest cost.
C
C	Fortran format of subroutine:
C
C          subroutine cost(x,array,array2,n,error,ind)
C
C	"C" format of subroutine:
C
C          zcost(x,array,array2,n,error,ind)
C               The name zcost may be arbritary in "C", but must be 
C               defined as subroutine name when passed to the 
C               zmetropolis bridge. (See Test Cases).
C   
C	Parameters:-
C
C          X     is the solution vector. REAL (length known by COST).
C          ARRAY is an array of N data points (if needed). REAL
C          ARRAY2 is another array of N elements if needed. REAL
C          N      is the number of elements/data points in ARRAY & ARRAY2.
C          ERROR is the returned cost. REAL scalar.
C          IND   is 0 for normal return, 1 for abnormal return.
C          If METROPOLIS senses an indicator of 1 it will generate another
C                guess somewhere else & try again.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      integer ind,npts,prnt,narg,limits,numten,norm
      integer fail,limit,j,loop,loop1,loop2,loop3,numreset,k
      real pi,pi2,scale,costsum,c1,c2,c3,mincost,boltzman,energy
      real*4 range(narg),answer(narg),prob
      real*4 array(1),array2(1)
      real*4 temp(10),x(10),minx(10)
      character*80 msg  
      integer*4 SEED
      REAL*4 randnum

      external cost

      pi=3.14159
      pi2=pi/2.0
      limit=limits

c  Compute a random number seed based on the time of day

       call get_seconds(SEED)
c  Compute the cost at position ANSWER and assign to variable C1.

      call cost(answer,array,array2,npts,c1,ind)
      if(ind.ne.0)then
         call xvmessage('Failure in COST function at initial guess',' ')
         return
      endif

c  Save the cost in case the user had good reason to inspect this
c  solution position.
      mincost=c1
      do j=1,narg
         minx(j)=answer(j)
      enddo

c  Set initial temperatures to the range estimates.

      do j=1,narg
         temp(j)=range(j)
      enddo

      fail=0
      loop=1
      loop1=0
      loop2=0
      loop3=0
      numreset=numten/10
      scale=exp((log(0.1))/real(numten))

      if(prnt.gt.0)then
          call xvmessage(' ',' ')
          write (msg, 900) scale
900       format ('scale=', f15.7)
          call xvmessage(msg,' ')
          msg(1:49)='Solution  Temperature     Cost  #downhill #uphill'
          msg(50:72)=' #rejected #outofbounds'
          call xvmessage(msg,' ')
          call xvmessage(' ',' ')
      endif

c   MAIN LOOP: loop on number of successful changes in solution space. 
      do while(loop.lt.limit)

c       Compute the delta_cost/temperature ratio for
c       normalization of probabilities.
c       Note that this is the Boltzmann constant for this 'system'.

        if(mod(loop,norm).eq.1)then
           costsum=0.0
           k=0
           do j=1,narg
              x(j)=answer(j)
           enddo
           do j=1,narg
              x(j)=answer(j)-temp(j)
              call cost(x,array,array2,npts,c2,ind)
              if(ind.eq.0)then
                k=k+1
                costsum=costsum+abs(c1-c2)
              endif
              x(j)=answer(j)+temp(j)
              call cost(x,array,array2,npts,c2,ind)
              if(ind.eq.0)then
                k=k+1
                costsum=costsum+abs(c1-c2)
              endif
              x(j)=answer(j)
           enddo
           if(k.eq.0) then
              call xvmessage(' ',' ')
              call xvmessage('Failure in normalization procedure',' ')
              call xvmessage(' ',' ')
              call xvmessage('solution + - range outofbounds',' ')
              call xvmessage(' ',' ')
              return
           endif              
           boltzman=5.0*(costsum/k)/temp(1)
           if(prnt.gt.0) then
              write (msg,930) boltzman
930           format ('Boltzmann = ',f15.7)
              call xvmessage(msg,' ')
           endif
        endif
                     
c       Decrement the temperature according to the multiplicative
c       cooling schedule.

        do j=1,narg
           temp(j)=temp(j)*scale
        enddo
        energy=boltzman*temp(1)

c       Compute a solution space guess using a Cauchy-Lorentzian
c       random probability distribution function.

91      do j=1,narg
           call rangen(SEED,randnum)
           x(j)=temp(j)*tan(pi*randnum+pi2)+answer(j)
        enddo
        call cost(x,array,array2,npts,c2,ind)
        if(ind.ne.0)then
           loop3=loop3+1
           goto 91
        endif

        if(c2.lt.c1)then

c           Accept lower cost position.
c           We always accept a downhill cost route if offered.

            c1=c2
            do j=1,narg
               answer(j)=x(j)
            enddo
            loop1=loop1+1

        else
c           Compute probability of accepting higher cost position.
c           This comes from the Boltzmann probability of our system 
c           transitioning from energy state c1 to energy state c2.

            c3=(c2-c1)/energy
            if(c3.gt.50.)then
               fail=fail+1
               goto 91
            endif
c           prob=1.0/(1.0+exp(c3))
            prob=1.0/exp(c3)

c           Evaluate the probability by comparing it against chance.
            call rangen(SEED,randnum) 
            if(prob.gt.randnum) then
c               Accept higher cost position.
                c1=c2
                do j=1,narg
                   answer(j)=x(j)
                enddo
                loop2=loop2+1
            else
c               Reject higher cost position.
                fail=fail+1
                goto 91
            endif
        endif

c       Save the minimum cost and associated solution as we go.

        if(c1.lt.mincost)then
            mincost=c1
            do j=1,narg
               minx(j)=answer(j)
            enddo
        endif

c       Reset the solution pointer to the minimum cost
c       location every numreset successful iterations.

        if(mod(loop,numreset).eq.0)then
            c1=mincost
            do j=1,narg
               answer(j)=minx(j)
            enddo
        endif

        loop=loop+1

c       Print out a status every PRNT iterations.

        if(prnt.gt.0)then
           if(mod(loop,prnt).eq.0)then
               write(msg,100)answer(1),temp(1),c1,loop1,loop2,fail,loop3
               call xvmessage(msg,' ')
               if(narg.gt.1)then
                  do j=2,narg
                      write(msg,100) answer(j),temp(j)
                      call xvmessage(msg,' ')
                  enddo
               endif
100            format(3(1x,g11.5),4(i5,3x))
               loop1=0
               loop2=0
               loop3=0
               fail=0
           endif
        endif

      enddo
      call xvmessage(' ',' ')
c     END of MAIN LOOP

c     Put minimum solution into ANSWER & it's cost into
c     RANGE.

      do j=1,narg
         answer(j)=minx(j)
      enddo
      range(1)=mincost

      return
      end
