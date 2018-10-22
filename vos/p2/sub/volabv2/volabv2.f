C***********************************************************************
C
C     VO75 LABEL PROCESSI%NG SUBROUTINE
C
C     22 AUG 1977  ...RMR... INITIAL RELEASE
C     24-APR-1986  ...JAM... convert to Vicar2
C     10-JUL-1991  ...CCA... delete opens,closes-increase buffer to 7200
C                            add PICNO,SCET
C     29-JUL-1993  ...TLT... PORTED TO UNIX
c
C***********************************************************************
      SUBROUTINE volabv2(IND,unit,LBUF)
      INTEGER*4 LBUF(40),unit,stat,bufsize,ptr
      CHARACTER*7200 LABI
      CHARACTER*8 MISC
      data MISC/'!       '/

C-----INITIALIZE ARRAYS------------
      ind=0
      call mve(4,16,-999,lbuf,0,1)

      bufsize=0
      call xlgetlabel(unit,labi,bufsize,stat,' ')
      if(bufsize.gt.7200)then
          call prnt(4,1,bufsize,
     *    ' volab buffer not big enough for label,size=.')
          ind=20
          return
      endif
      bufsize=7200
      call xlgetlabel(unit,labi,bufsize,stat,' ')
      call chkstat(stat,' err in xlgetlabel in volabv2,stat=',1,stat,1)

C-----FLIGHT FORMAT EDR LABEL PROCESSOR------------

1000  lbuf(1)=2
      if(index(labi(1:bufsize),'VO75 1A').gt.0)then
             lbuf(2)=7
             lbuf(3)=1
             lbuf(4)=1
      else if(index(labi(1:bufsize),'VO75 1B').gt.0)then
             lbuf(2)=4
             lbuf(3)=1
             lbuf(4)=2
      else if(index(labi(1:bufsize),'VO75 2A').gt.0)then
             lbuf(2)=8
             lbuf(3)=2
             lbuf(4)=1
      else if(index(labi(1:bufsize),'VO75 2B').gt.0)then
             lbuf(2)=6
             lbuf(3)=2
             lbuf(4)=2
      else
             ind=20  ! not a valid label
             return
      endif
c
      ptr=index(labi(1:bufsize),'FILTER')
      read (labi(ptr+6:),'(BN,i2)') lbuf(8)      ! filter
c
      ptr=index(labi(1:bufsize),'EXP')
      read (labi(ptr+3:),'(BN,i5)') lbuf(9)        ! exposure
c
      ptr=index(labi(1:bufsize),' FGD ')
      if (labi(ptr+5:ptr+5) .eq. '0') lbuf(10) = 0    ! flood state
      if (labi(ptr+5:ptr+5) .eq. '1') lbuf(10) = 1
      if (labi(ptr+6:ptr+6) .eq. '0') lbuf(11) = 0    ! gain state
      if (labi(ptr+6:ptr+6) .eq. '1') lbuf(11) = 1
      if (labi(ptr+7:ptr+7) .eq. '0') lbuf(12) = 0    ! dc offset
      if (labi(ptr+7:ptr+7) .eq. '1') lbuf(12) = 1
c
      ptr=index(labi(1:bufsize),'FSC')
      read (labi(ptr+3:),'(BN,i10)') lbuf(7)            ! fsc
c
      ptr=index(labi(1:bufsize),'OET-GMT')			!oet=scet?
      read (labi(ptr+7:),'(BN,i4)') lbuf(19)      ! scet year
      read (labi(ptr+11:),'(BN,i4)') lbuf(20)      ! scet day
      read (labi(ptr+15:),'(BN,i3)') lbuf(21)      ! scet hour
      read (labi(ptr+18:),'(BN,i3)') lbuf(22)      ! scet min
      read (labi(ptr+21:),'(BN,i3)') lbuf(23)      ! scet sec
      lbuf(24)=0  				! scet millisec

c     if bad values (*) for range,fov and scale .... return -999
      ptr=index(labi(1:bufsize),'RNG=')
      if (ptr .gt. 0 .and. labi(ptr+7:ptr+7) .ne. '*') then  
             read (labi(ptr+4:),'(BN,i6)') lbuf(16)     ! range
             ptr=index(labi(1:bufsize),'HFOV=')
             read (labi(ptr+5:),'(BN,i5)') lbuf(15) ! fov width
             ptr=index(labi(1:bufsize),'VFOV=')
             read (labi(ptr+5:),'(BN,i5)') lbuf(14)     ! fov height
             ptr=index(labi(1:bufsize),'SCL=')
             read (labi(ptr+4:),'(BN,i5)') lbuf(13)     ! scale in m/pxl
      endif
c
      call mvcl(misc(1:),lbuf(17),8)
      ptr=index(labi(1:bufsize),' PICNO')
      if (ptr .gt. 0) call mvcl(labi(ptr+7:),lbuf(17),8)
c
c      call xvclose(unit,stat,' ')
c      call chkstat(stat,' ERROR IN XVCLOSE,IND=',1,stat,1)
c
      return
      end
