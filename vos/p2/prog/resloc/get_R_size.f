cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get search area dimensions.

      subroutine get_R_size(hsR,hlR)
      implicit none
c Outputs...
      integer hsR,hlR		!half widths of R

      integer*4 count
      integer nver,nhor		!search area is nver x nhor
      character*80 msg
  101 format('search area is ',i2,' x ',i2)

      call xvp('nhor',nhor,count)	!width of search window
      call xvp('nver',nver,count)	!height of search window
      write(msg,101) nver,nhor
      call xvmessage(msg,0)
      hsR = nhor/2
      hlR = nver/2
      if (nhor.eq.2*hsR) call mabend('***nhor must be odd')
      if (nver.eq.2*hlR) call mabend('***nver must be odd')
      return
      end
