cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copy input image to ounit, overlaying it with reseau locations...
c Overlay resau locations on D, outputting the results to the filename
c specified by param. 

      subroutine overlay(D,res,file)
      implicit none
c Inputs...
      byte D(800,800)
      real*4 res(2,202)
      character*132 file

c Local variables...
      byte buf(825)		!output image line
      integer ounit		!VICAR logical unit number for output
      integer ires
      integer line,l,s
      integer n,dn,ind,cnt
!!!      byte black/0/,white/zff/
      byte black/0/,white/-1/
      character*3 msg
  101 format(i3)

c Given file, get ounit and open it...
      call xvunit(ounit,'scr',1,ind,'u_name',file,' ')
      call xvopen(ounit,ind,'op','write',
     &          'open_act','sa','io_act','sa',' ')

c Create the overlay one line at a time...

      do 50 line=1,800

      do s=1,800
         buf(s) = D(s,line)
      enddo

      do 40 ires=1,202
      l = res(1,ires)
      s = res(2,ires)
      if (line.lt.l .or. line.gt.l+7) goto 40
      if (s.lt.1 .or. s.gt.800) goto 40

      if (l.eq.line) then
         buf(s) = white
      else
         if (ires.lt.10) then
            n = 1
         elseif (ires.lt.100) then
            n = 2
         else
            n = 3
         endif
         write(msg,101) ires
         call text(msg(4-n:3),n,line-l-1,buf(s+2),6,255)
      endif
   40 continue

   50 call xvwrit(ounit,buf,ind,0)

      call xvclose(ounit,ind,0)
      return
      end
