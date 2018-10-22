cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get size of correlation template.

      subroutine get_A_size(ia,ja)
      implicit none
c Outputs...
      integer*4 ia,ja		!template half widths

c Local variables...
      integer*4 count
      integer*4 nsw,nlw		!correlation template is nlw x nsw
      character*80 msg
  101 format('nlw='i2,' nsw=',i2)

      call xvp('nlw',nlw,count)		!height of correlation template
      call xvp('nsw',nsw,count)		!width of correlation template
      write(msg,101) nlw,nsw
      call xvmessage(msg,0)

      ja = nlw/2 			!half widths of template
      ia = nsw/2
      if (nlw.eq.2*ja) call mabend('***nlw must be odd')
      if (nsw.eq.2*ia) call mabend('***nsw must be odd')
      return
      end
