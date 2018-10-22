ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read input pic into matrix D.

      subroutine read_pic(pic,D,d_valid)
      implicit none
c Input...
      integer pic
      logical d_valid
c Outputs...
      byte D(800,800)		!byte format to reduce memory.
      integer*4 ids(5)		!ids=(ifrm,icam,ifil,iyear,iday)

c Local variables...
      integer l
      integer ind		!ignored return status.

      if (d_valid) return	!return if already done.

      do l=1,800
         call xvread(pic,D(1,l),ind,0)
      enddo
      call xvclose(pic,ind,0)
      d_valid = .true.
      return
      end

