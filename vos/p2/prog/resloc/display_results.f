c Display and print results.

      subroutine display_results(D,res,nom)
      implicit none
c Inputs...
      byte D(800,800)
      real*4 res(2,202)		!reseau coordinates found in D
      real*4 nom(2,202)		!nominal reseau coordinates

c Local variables...
      integer cnt
      real*4 dif(2,202)		!dif = res - nom
      real*4 dif2(2,202)	!dif2 = res - nom - mean
      character*132 file
      logical xvptst

c Overlay res and nom over the image...
      call xvp('ores',file,cnt)
      if (cnt.eq.1) call overlay(D,res,file)
      call xvp('onom',file,cnt)
      if (cnt.eq.1) call overlay(D,nom,file)
      return
      end
