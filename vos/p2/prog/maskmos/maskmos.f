C
C     PROGRAM maskmos
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      parameter (nsmax=1502,nlmax=1502) ! image storage array size
      implicit integer(a-z)
      integer*2 buf(nsmax,nlmax),code
      character*80 MSG

      call xvpcnt('INP',nids)

      do n=1,nids
C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      CALL XVSELPI(n)  ! copy corresponding label
      CALL XVUNIT(INUNIT,'INP',n,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','HALF',' ')
      CALL XVGET(INUNIT,STATUS,'NL',nl,'NS',ns,' ')
      if(nl.gt.nlmax-2)then
         write(msg,100) nlmax
100      format(' Max # lines permitted= ',i5)
         call xvmessage(msg,' ')
         call xvmessage(' Can recompile with new nlmax value',' ')
         call abend
      endif
      if(ns.gt.nsmax-2)then
         write(msg,101) nsmax
101      format(' Max # samples permitted= ',i5)
         call xvmessage(msg,' ')
         call xvmessage(' Can recompile with new nsmax value',' ')
         call abend
      endif
      CALL XVUNIT(OUTUNIT,'OUT',n,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','HALF','O_FORMAT','HALF',
     +          'U_NL',nl,'U_NS',ns,' ')

c load picture into memory leaving a zero border 1 pixel wide
      do line=1,nl
         CALL XVREAD(INUNIT,BUF(2,line+1),STATUS,'LINE',LINE,' ')
      enddo

c zero border
      do i=1,ns+2
         buf(i,1)=0
         buf(i,nl+2)=0
      enddo
      do j=1,nl+2
         buf(1,j)=0
         buf(ns+2,j)=0
      enddo

c set all non zero pixels to 32767
      do j=2,nl+1
         do i=2,ns+1
            if(buf(i,j).ne.0) buf(i,j)=32767
         enddo
      enddo

c set all border points to one.
c border points are non zero with a zero adjacent pixel.
      do j=2,nl+1
         do i=2,ns+1
            if(buf(i,j).ne.0) then
               if(buf(i-1,j-1).eq.0) goto 10
               if(buf(i  ,j-1).eq.0) goto 10
               if(buf(i+1,j-1).eq.0) goto 10
               if(buf(i-1,j  ).eq.0) goto 10
               if(buf(i  ,j  ).eq.0) goto 10
               if(buf(i+1,j  ).eq.0) goto 10
               if(buf(i-1,j+1).eq.0) goto 10
               if(buf(i  ,j+1).eq.0) goto 10
               if(buf(i+1,j+1).eq.0) goto 10
               goto 20
10             buf(i,j)=1
20             continue
            endif
         enddo
      enddo

c grow a region from the border incrementing 1 each time.
      code=0
      ltop=2
      lbot=nl+1
      sleft=2
      sright=ns+1
30    continue
      count=0
      code=code+1
      lmin=nl
      lmax=1
      smin=ns
      smax=1
      do j=ltop,lbot
         do i=sleft,sright
            if(buf(i,j).eq.code) then
               if(buf(i-1,j-1).eq.32767) then
                  buf(i-1,j-1)=code+1
                  count=count+1
                  if(i-1.lt.smin) smin=i-1
                  if(i-1.gt.smax) smax=i-1
                  if(j-1.lt.lmin) lmin=j-1
                  if(j-1.gt.lmax) lmax=j-1
               endif
               if(buf(i  ,j-1).eq.32767) then
                  buf(i  ,j-1)=code+1
                  count=count+1
                  if(i  .lt.smin) smin=i
                  if(i  .gt.smax) smax=i
                  if(j-1.lt.lmin) lmin=j-1
                  if(j-1.gt.lmax) lmax=j-1
               endif
               if(buf(i+1,j-1).eq.32767) then
                  buf(i+1,j-1)=code+1
                  count=count+1
                  if(i+1.lt.smin) smin=i+1
                  if(i+1.gt.smax) smax=i+1
                  if(j-1.lt.lmin) lmin=j-1
                  if(j-1.gt.lmax) lmax=j-1
               endif
               if(buf(i-1,j  ).eq.32767) then
                  buf(i-1,j  )=code+1
                  count=count+1
                  if(i-1.lt.smin) smin=i-1
                  if(i-1.gt.smax) smax=i-1
                  if(j  .lt.lmin) lmin=j
                  if(j  .gt.lmax) lmax=j
               endif
               if(buf(i  ,j  ).eq.32767) then
                  buf(i  ,j  )=code+1
                  count=count+1
                  if(i  .lt.smin) smin=i
                  if(i  .gt.smax) smax=i
                  if(j  .lt.lmin) lmin=j
                  if(j  .gt.lmax) lmax=j
               endif
               if(buf(i+1,j  ).eq.32767) then
                  buf(i+1,j  )=code+1
                  count=count+1
                  if(i+1.lt.smin) smin=i+1
                  if(i+1.gt.smax) smax=i+1
                  if(j  .lt.lmin) lmin=j
                  if(j  .gt.lmax) lmax=j
               endif
               if(buf(i-1,j+1).eq.32767) then
                  buf(i-1,j+1)=code+1
                  count=count+1
                  if(i-1.lt.smin) smin=i-1
                  if(i-1.gt.smax) smax=i-1
                  if(j+1.lt.lmin) lmin=j+1
                  if(j+1.gt.lmax) lmax=j+1
               endif
               if(buf(i  ,j+1).eq.32767) then
                  buf(i  ,j+1)=code+1
                  count=count+1
                  if(i  .lt.smin) smin=i
                  if(i  .gt.smax) smax=i
                  if(j+1.lt.lmin) lmin=j+1
                  if(j+1.gt.lmax) lmax=j+1
               endif
               if(buf(i+1,j+1).eq.32767) then
                  buf(i+1,j+1)=code+1
                  count=count+1
                  if(i+1.lt.smin) smin=i+1
                  if(i+1.gt.smax) smax=i+1
                  if(j+1.lt.lmin) lmin=j+1
                  if(j+1.gt.lmax) lmax=j+1
               endif
            endif
         enddo
      enddo
      ltop=lmin
      lbot=lmax
      sleft=smin
      sright=smax
      if(count.gt.0) goto 30

c load data into output.
      do line=1,nl
         CALL XVWRIT(OUTUNIT,BUF(2,line+1),STATUS,'LINE',LINE,' ')
      enddo
C
C  CLOSE DATA SETS
C
      CALL XVCLOSE(INUNIT,STATUS,'CLOS_ACT','FREE',' ')
      CALL XVCLOSE(OUTUNIT,STATUS,'CLOS_ACT','FREE',' ')

      enddo ! end picture loop
      RETURN
      END
