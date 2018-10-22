c
c program auxiliary
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=20000, maxpix=50, maxint=10000)
      real*4 obuf(maxsamp),inbuf(maxsamp)
      real*4 slop(maxpix),ofst(maxpix)
      integer*4 unit(maxpix),nl(maxpix),ns(maxpix),status
      integer ounit,hist(0:maxint)
      character*100 msg
      logical xvptst,no_stretch

c parameters
      call xvpcnt('INP',nids)      
      no_stretch=xvptst('NOSTR')
      call xvparm('NS',nso,count,def,1)
      call xvparm('PERCENT',percent,count,def,1)
      if(nids.gt.maxpix)then
        call xvmessage('Too many inputs',' ')
        call abend
      endif

c open inputs
      do i=1,nids
        call xvunit(unit(i),'INP',i,status,' ')
        call xvsignal(unit(i),status,1)
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(unit(i),status,1)
        call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(unit(i),status,1)
        if(ns(i).gt.maxsamp)then
          call xvmessage('Input picture line too long',' ')
          call abend
        endif
        if(ns(i).gt.ns(1))then
          call xvmessage('Input picture lines differ',' ')
          call abend
        endif
        if(nl(i).gt.nl(1))then
          call xvmessage('Input pictures columns differ',' ')
          call abend
        endif
      enddo

c get limits of intensity on inputs
c get percent stretch parameters.
      if(no_stretch)goto 101
      do 100 image=1,nids
        do i=0,maxint
          hist(i)=0
        enddo
        rmin=1.0e+30
        rmax=-1.0e+30
        do line=1,nl(image)
          call xvread(unit(image),inbuf,status,'LINE',line,' ')
          call xvsignal(unit(image),status,1)
          do i=1,ns(image)
            if(inbuf(i).lt.rmin)rmin=inbuf(i)
            if(inbuf(i).gt.rmax)rmax=inbuf(i)
          enddo
        enddo
        write(msg,*)'image= ',image,' min dn= ',rmin,' max dn= ',rmax
        call xvmessage(msg,' ')
        if(rmax.eq.rmin)then
          slop(image)=0.0
          ofst(image)=128.0
          goto 100
        endif
        if(rmax .le. rmin) then
          slope=maxint/1.0
          offset = 0.0
        else
          slope=maxint/(rmax-rmin)
          offset=maxint-slope*rmax
        endif
        do line=1,nl(image)
          call xvread(unit(image),inbuf,status,'LINE',line,' ')
          call xvsignal(unit(image),status,1)
          do i=1,ns(image)
            j=nint(inbuf(i)*slope+offset)
            hist(j)=hist(j)+1
          enddo
        enddo
        ncounts=nl(image)*ns(image)*percent/100.
        n=0
        do i=0,maxint
          n=n+hist(i)
          ii=i
          if(n.ge.ncounts)goto 10
        enddo
10      rmin=(ii-offset)/slope
        n=0
        do i=maxint,0,-1
          n=n+hist(i)
          ii=i
          if(n.ge.ncounts)goto 11
        enddo
11      rmax=(ii-offset)/slope
        if(rmax .le. rmin) then
          slop(image)=255.0
          ofst(image)=0.0
        else
          slop(image)=255./(rmax-rmin)
          ofst(image)=255.-slop(image)*rmax
        endif
100   continue
101   continue

c determine size of output
      if((nso/ns(1))*ns(1).ne.nso)then
        call xvmessage('NS not a multiple of input line length',' ')
        call abend
      endif
      npic_per_row=nso/ns(1)
      npic_per_col=nids/npic_per_row
      if(npic_per_col*npic_per_row.lt.nids)
     +   npic_per_col=npic_per_col+1
      nlo=npic_per_col*nl(1)

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvsignal(ounit,status,1)
      call xvopen(ounit,status,'U_FORMAT','REAL','O_FORMAT','BYTE',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
      call xvsignal(ounit,status,1)

c write data
      left_image=1-npic_per_row
      do irow=1,npic_per_col
        left_image=left_image+npic_per_row

        do line=1,nl(1)
          k=0
          image=left_image-1

          do icol=1,npic_per_row
            image=image+1

            if(image.le.nids)then
              call xvread(unit(image),inbuf,status,'LINE',line,' ')
              call xvsignal(unit(image),status,1)
              do i=1,ns(image)
                k=k+1
                if(no_stretch)then
                  dn=inbuf(i)
                else
                  dn=inbuf(i)*slop(image)+ofst(image)+0.5
                endif
                if(dn.lt.0.0)dn=0.0
                if(dn.gt.255.0)dn=255.0
                obuf(k)=dn
              enddo
            else
              do i=1,ns(1)
                k=k+1
                obuf(k)=0.0
              enddo
            endif

          enddo
          call xvwrit(ounit,obuf,status,' ')
          call xvsignal(ounit,status,1)
        enddo
      enddo

      return
      end

