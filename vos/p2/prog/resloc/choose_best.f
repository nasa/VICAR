cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Choose candidate with best match quality...

      subroutine choose_best(q,choice,dbug)
      implicit none
c Input...
      real*4 q(3,202)
      logical dbug
c Output...
      integer choice(202)		!choice=1,2,or 3
c Local variables...
      integer i,k
      real*4 maxq
      integer imax
      character*80 msg
  101 format('Mark',i3,' Candidate chosen=',i1)


      do 20 k=1,202
      maxq = q(1,k)
      imax = 1
      do i=2,3
         if (q(i,k).gt.maxq) then
            maxq = q(i,k)
            imax = i
         endif
      enddo
      choice(k) = imax
      if (dbug .and. imax.ne.1) then
         write(msg,101) k,imax
        call xvmessage(msg,0)
      endif
   20 continue

      return
      end
