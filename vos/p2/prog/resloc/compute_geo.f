cccccccccccccccccccccccccccccccccccccccccccccccc
c create geoma parameters...

      subroutine compute_geo(res,os_res,geo)
      implicit none
c inputs...
      real*4 res(2,202)		!image space coordinates
      real*4 os_res(2,202)	!object space coordinates
c output...
      real*4 geo(4,24,23)	!quad grid in geoma format
c local variables..
      real*4 is(2,13,23)	!IS coordinates of triangular grid
      real*4 os(2,13,23)	!OS coordinates of triangular grid
      integer row
      logical xvptst

      do row=1,23
         call interg(res,os_res,row,is(1,1,row),os(1,1,row))
         call triag(row,is(1,1,row),os(1,1,row),geo(1,1,row))
      enddo

      if (xvptst('pgeo')) call print_rows(is,os)
      return
      end
