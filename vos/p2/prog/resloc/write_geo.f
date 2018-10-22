ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output geo...

      subroutine write_geo(geo)
      implicit none
c Inputs...
      real geo(4,24,23)		!geomtric transformation parameters
c Local variables...
      integer*4 icam		!camera serial number
      integer ind,gunit
      character*6 format(409)/5*'full',404*'real'/

c     ...open geo as VICAR file to write history label
      call xvunit(gunit,'out',2,ind,0)	!get VICAR unit number for geo
      call xvopen(gunit,ind,'op','write',0)
      call xladd(gunit,'history', 'title',
     .	 '**RESLOC GEOMA PARAMETERS**',ind,'format','string',0)
      call xvclose(gunit,ind,0)

c     ...open geo as an IBIS file to write record
      call xvunit(gunit,'out',2,ind,0)
      call iwrite_tiepoints(gunit,23,22,0,geo,4)
      return
      end
