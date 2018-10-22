C Return radius for a given lat,lon.
C Longitudes may be either east or west.
C
      subroutine ellipse_radius(lat,lon,ra,rb,rc,lora,gcr)
      implicit none
      real*8 lat,lon		!Input lat,lon coordinates
      real*8 ra,rb,rc		!Input target body radii
      real*8 lora		!Longitude of semi-major axis
      real*8 gcr		!geocentric radius
      real*8 rlat,rlon,clat,slat,clon,slon,dtor,PI

      PI = 3.141592653589793D0
      dtor = PI/180.d0		!degrees to radians
      rlat = lat*dtor		!convert to radians
      rlon = (lon-lora)*dtor
      clat = dcos(rlat)
      slat = dsin(rlat)
      clon = dcos(rlon)
      slon = dsin(rlon)
      gcr = 1.d0/dsqrt((clat*clon/ra)**2+(clat*slon/rb)**2+(slat/rc)**2)
      return
      end
