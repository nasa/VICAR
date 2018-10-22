c
c program markibis
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (ntable=200000)
c ntable = max # tiepoints
      integer*4 status,inmark,outmark,inibis,outibis
      real*4 buf(4,ntable)
      character*80 msg
      logical flag

c determine the type of the input file
      call xvunit(i,'INP',1,status,' ')
      call ibis_file_open(i,j,'READ',0,0,' ',' ',status)
      if(status.eq.1)then
        mode=1 ! ibis input
      else
        mode=2 ! mark input
      endif
      call ibis_file_close(j,' ',status)
      if(mode.eq.1) goto 200

c*********************************************************************
c load the tiepoint table from the mark file.
      call xvunit(inmark,'INP',1,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl,'NS',ns,' ')
      npts=nl*ns/4
      if(npts.gt.ntable)then
        call xvmessage('Too many input points',' ')
        call abend
      endif
      k=1
      do i=1,nl
         call xvread(inmark,buf(1,k),status,'LINE',i,' ')
         k=k+ns/4
      enddo
      write(msg,*)nl,' mark tiepoint records read'
      call xvmessage(msg,' ')

c remove zeroes 
      j=0
10    j=j+1
11      flag=.false.
        if(buf(1,j).eq.0.0) flag=.true.  
        if(buf(2,j).eq.0.0) flag=.true.  
        if(buf(3,j).eq.0.0) flag=.true.  
        if(buf(4,j).eq.0.0) flag=.true.  
        if(flag)then
          if(j.eq.npts)then ! last point is bad
            npts=npts-1
            goto 12
          endif
          do i=1,npts-j     ! move everything up one
            buf(1,j+i-1)=buf(1,j+i)
            buf(2,j+i-1)=buf(2,j+i)
            buf(3,j+i-1)=buf(3,j+i)
            buf(4,j+i-1)=buf(4,j+i)
          enddo
          npts=npts-1
          goto 11
        endif
      if(j.lt.npts) goto 10
12    continue
      write(msg,*)npts,' good tiepoints located'
      call xvmessage(msg,' ')

c switch the tiepoint locations so IN is columns 3,4 and OUT is 1,2
c which is the ibis tiepoint file convention.
      do i=1,npts
        tl=buf(1,i)
        ts=buf(2,i)
        buf(1,i)=buf(3,i)
        buf(2,i)=buf(4,i)
        buf(3,i)=tl
        buf(4,i)=ts
      enddo

c write the ibis file
      call xvunit(outibis,'OUT',1,status,' ')
      call iwrite_tiepoints(outibis,0,0,npts,buf,4)
      call xvmessage('Ibis file written',' ')
      return

c**********************************************************************
c read the ibis file
200   continue
      call xvunit(inibis,'INP',1,status,' ')
      i=0
      call iread_tiepoints(inibis,i,npts,ntable,buf,4)
      write(msg,*)npts,' tiepoints located'
      call xvmessage(msg,' ')

c fill the end of the buffer with zeroes
      nl=npts/32
      if(nl*32.lt.npts) nl=nl+1
      if(nl*32.gt.npts)then
        do i=npts+1,nl*32
          buf(1,i)=0.0      
          buf(2,i)=0.0      
          buf(3,i)=0.0      
          buf(4,i)=0.0      
        enddo
      endif

c switch the tiepoint locations so IN is columns 1,2 and OUT is 3,4
c which is the mark convention.
      do i=1,npts
        tl=buf(1,i)
        ts=buf(2,i)
        buf(1,i)=buf(3,i)
        buf(2,i)=buf(4,i)
        buf(3,i)=tl
        buf(4,i)=ts
      enddo

c write the mark file.
      call xvunit(outmark,'OUT',1,status,' ')
      call xvopen(outmark,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'U_NL',nl,'U_NS',128,'U_NB',1,
     +            'OP','WRITE','O_FORMAT','REAL','OPEN_ACT','AS',' ')
      do i=1,nl
         call xvwrit(outmark,buf(1,(i-1)*32+1),status,'LINE',i,' ')
      enddo
      call xvclose(outmark,status,'CLOS_ACT','FREE',' ')
      call xvmessage('Mark file written',' ')

      return
      end
