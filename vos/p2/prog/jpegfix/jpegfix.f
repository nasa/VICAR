c
c program smooth
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter(npix=2000000)
      integer*2 buf1(npix),buf2(npix),t
      integer*4 ounit,inunit,status
      integer*4 def,count,thresh
      character*32 format
      logical roundoff
      
c get parameters
      call xvparm('ITER',iter,count,def,1)
      call xvparm('THRESH',thresh,count,def,1)
      t=thresh
            
c open input
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','HALF',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      roundoff=.false.
      if(ns*nl.gt.npix)then
        write(*,*)'Max number of input pixels',npix
        call abend
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','HALF','OP','WRITE',' ')
      
c read image into memory
      do j=1,nl
        call xvread(inunit,buf1((j-1)*ns+1),status,'LINE',j,' ')
      enddo 

c process data
      do j=1,iter      
        call grow_h(buf1,nl,ns,buf2,t)
        call grow_v(buf2,nl,ns,buf1,t)
      enddo

c write image from memory
      do j=1,nl
        call xvwrit(ounit,buf1((j-1)*ns+1),status,' ')
      enddo
      
      return
      end

c**************************************************************************
      subroutine grow_v(inbuf,nl,ns,obuf,t)
      integer*2 inbuf(ns,nl),obuf(ns,nl),t

      do i=1,ns
        do j=1,nl
          obuf(i,j)=inbuf(i,j)
        enddo
        do j=2,nl-2
          if((inbuf(i,j).ge.inbuf(i,j-1)).and.
     +       (inbuf(i,j)-inbuf(i,j-1).le.t))then          
            if((inbuf(i,j).lt.inbuf(i,j+1)).and.
     +         (inbuf(i,j+1).lt.inbuf(i,j+2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j+1))/2
               obuf(i,j+1)=(inbuf(i,j+1)+inbuf(i,j+2))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i,j-1)).and.
     +       (inbuf(i,j-1)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i,j+1)).and.
     +         (inbuf(i,j+1).gt.inbuf(i,j+2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j+1))/2
               obuf(i,j+1)=(inbuf(i,j+1)+inbuf(i,j+2))/2
            endif
          endif
        enddo
        do j=1,nl
          inbuf(i,j)=obuf(i,j)
        enddo
        do j=nl-1,3,-1
          if((inbuf(i,j).ge.inbuf(i,j+1)).and.
     +       (inbuf(i,j)-inbuf(i,j+1).le.t))then          
            if((inbuf(i,j).lt.inbuf(i,j-1)).and.
     +         (inbuf(i,j-1).lt.inbuf(i,j-2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j-1))/2
               obuf(i,j-1)=(inbuf(i,j-1)+inbuf(i,j-2))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i,j+1)).and.
     +       (inbuf(i,j+1)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i,j-1)).and.
     +         (inbuf(i,j-1).gt.inbuf(i,j-2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j-1))/2
               obuf(i,j-1)=(inbuf(i,j-1)+inbuf(i,j-2))/2
            endif
          endif
        enddo
      enddo

      return
      end        
      
c**************************************************************************
      subroutine grow_h(inbuf,nl,ns,obuf,t)
      integer*2 inbuf(ns,nl),obuf(ns,nl),t

      do j=1,nl
        do i=1,ns
          obuf(i,j)=inbuf(i,j)
        enddo
        do i=2,ns-2
          if((inbuf(i,j).ge.inbuf(i-1,j)).and.
     +       (inbuf(i,j)-inbuf(i-1,j).le.t))then          
            if((inbuf(i,j).lt.inbuf(i+1,j)).and.
     +         (inbuf(i+1,j).lt.inbuf(i+2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i+1,j))/2
               obuf(i+1,j)=(inbuf(i+1,j)+inbuf(i+2,j))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i-1,j)).and.
     +       (inbuf(i-1,j)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i+1,j)).and.
     +         (inbuf(i+1,j).gt.inbuf(i+2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i+1,j))/2
               obuf(i+1,j)=(inbuf(i+1,j)+inbuf(i+2,j))/2
            endif
          endif
        enddo
        do i=1,ns
          inbuf(i,j)=obuf(i,j)
        enddo
        do i=ns-1,3,-1
          if((inbuf(i,j).ge.inbuf(i+1,j)).and.
     +       (inbuf(i,j)-inbuf(i+1,j).le.t))then          
            if((inbuf(i,j).lt.inbuf(i-1,j)).and.
     +         (inbuf(i-1,j).lt.inbuf(i-2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i-1,j))/2
               obuf(i-1,j)=(inbuf(i-1,j)+inbuf(i-2,j))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i+1,j)).and.
     +       (inbuf(i+1,j)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i-1,j)).and.
     +         (inbuf(i-1,j).gt.inbuf(i-2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i-1,j))/2
               obuf(i-1,j)=(inbuf(i-1,j)+inbuf(i-2,j))/2
            endif
          endif
        enddo
      enddo

      return
      end        
