ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open image D and return pic and lab.

      subroutine open_pic(pic,lab)
      implicit none
c Outputs...
      integer pic		!VICAR unit number for D
      character*7200 lab        !Voyager image label

c Local variables...
      integer*4 labsize/7200/
      integer ind		!ignored return status
      integer ns,nl		!pic image size according to VICAR
      character*5 format

      call xvunit(pic,'inp',1,ind,0) 	!inp=pic
      call xvopen(pic,ind,'open_act','sa','io_act','sa',0)

c Make sure the image is valid...
      call xvget(pic,ind,'nl',nl,'ns',ns,' ')
      if (nl.ne.800.or.ns.ne.800) call mabend('***wrong pic size')
      call xvget(pic,ind,'format',format,' ')
      if (format.ne.'BYTE') call mabend('***pic must be byte') 

c Read the Voyager label...
      call xlgetlabel(pic,lab,labsize,ind)
      call chkstat(ind,'***err reading label, ind=',1,ind,1)
      return
      end

