c Draw dvectors from a mark file containing tiepoints.
      include 'VICMAIN_FOR'

      subroutine main44

      implicit none

      character*32 oformat
      character*4 ifmt,ofmt,ifmt3
      real*4      data(4,32),exag
      integer*2   buf(500000),zero
      integer*4   inmark(101),file
      integer*4   maxcnt, count, def, dn, nids,inunit
      integer*4   status, nl, np, i, nl3, ns3, outunit
      integer*4   sl, nlstor, pass, npass, n, top, bot, line
      integer*4   ldat, tpt

! Begin data initialization

      data exag /0.0/
      data file/0/, maxcnt/0/, count/0/, def/0/, dn/0/, zero/0/
      data nids/0/,inunit/0/, status/0/, nl/0/, np/0/, i/0/
      data nl3/0/, ns3/0/, outunit/0/,sl/0/, nlstor/0/, pass/0/
      data npass/0/, n/0/, top/0/, bot/0/, line/0/,ldat/0/, tpt/0/

      call zia (inmark,101)
      call zia (data, 4*32)
      call zia (buf, 500000/2)
      oformat = ' '
      ifmt = ' '
      ofmt = ' '
      ifmt3= ' '
     
! End data initialization

      call ifmessage ('DVECTOR version 31-OCT-94')
      maxcnt = 1
      call xvparm('EXAG',exag,count,def,maxcnt)
      call xvparm('DN',dn,count,def,maxcnt)
      call xvpcnt('INP',nids)

c open files
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','HALF','IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit,status,'NL',nl,'NS',np,'FORMAT',oformat,' ')

      do i=2,nids
        call xvunit(inmark(i),'INP',i,status,' ')
        call xvopen(inmark(i),status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      enddo

      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'U_FORMAT','HALF','O_FORMAT',
     +            oformat,'OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS',' ')
      sl=1

c compute constants
      nlstor=500000/np
      if(nlstor.gt.nl) nlstor=nl
      npass=nl/nlstor
      if(npass*nlstor.lt.nl) npass=npass+1

c main line loop for image blocks
      do 120 pass=1,npass

c       Read a block of lines into buf
        n=1
        top=(pass-1)*nlstor+1
        bot=pass*nlstor
        do 130 line=top,bot
          if(line.gt.nl) goto 130
          if(dn.ge.0)then
            call xvread(inunit,buf(n),status,'LINE',line,' ')
          else
            call mve(2,np,zero,buf(n),0,1)
          endif
          n=n+np
130     continue

c       Loop on input mark files
        do file=2,nids
          call xvget(inmark(file),status,'NL',nl3,'NS',ns3,' ')

c         Loop on tiepoint records
          do 140 ldat=1,nl3
            call xvread(inmark(file),data,status,'LINE',ldat,' ')

c           Loop on tiepoints
            do 150 tpt=1,32
              if((data(1,tpt).eq.0.0).and.(data(2,tpt).eq.0.0)) goto 150

c             Draw vectors on image segment stored in buf.
              call draw(buf,np,nlstor,top,bot,data(1,tpt),data(2,tpt),
     +                data(3,tpt),data(4,tpt),exag,dn,tpt,pass,file)
       
150         continue
140       continue
        enddo
c       Write the buffer back to the output
        n=1
        do 160 line=top,bot
          if(line.gt.nl) goto 160
          call xvwrit(outunit,buf(n),status,'LINE',line,' ')
          n=n+np
160     continue

120   continue

      return
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine draw(buf,np,nlstor,top,bot,y1,x1,y2,x2,exag,
     +                dn,tpt,pass,file)

c Draw dvectors on image segment

      integer*2 buf(np,nlstor)
      integer*4 top,bot,dn,tpt,pass,file,dnout

      rtop=top
      rbot=bot
      rleft=1.
      rright=np
      dy=(y2-y1)*exag
      dx=(x2-x1)*exag
      y3=y1+dy
      x3=x1+dx
      if((y1.lt.rtop).and.(y3.lt.rtop)) return
      if((y1.gt.rbot).and.(y3.gt.rbot)) return
      dnout=dn

      if(dn.lt.0)then
        angle=57.2958*atan2(-dy,dx)
        if(angle.lt.0.0)angle=angle+360.
        angle=angle*255./360.
        dnout=nint(angle)
        if(dnout.eq.0)dnout=1
      endif

c draw the base X
      n=0
      if((file.eq.2).and.(dn.ge.0))n=1   ! only on first mark file
      l=nint(y1)
      j=nint(x1)
      do 5 i=j-n,j+n
          if((i.ge.1).and.(i.le.np).and.(l.ge.top).
     +      and.(l.le.bot)) buf(i,l-top+1)=dnout
5     continue
      do 6 i=l-n,l+n
        if((j.ge.1).and.(j.le.np).and.(i.ge.top).
     +      and.(i.le.bot)) buf(j,i-top+1)=dnout
6     continue

c draw long vertical vectors
      if(abs(dy).ge.abs(dx))then
        if(abs(dy).lt.1.0) return
        if(y1.gt.y3)then
          k=-1
        else
          k=1
        endif
        ratio=k*dx/dy
        x=x1-ratio
        do 10 l=nint(y1),nint(y3),k
          x=x+ratio
          j=nint(x)
          if((l.lt.top).or.(l.gt.bot)) goto 10
          if((j.lt.1).or.(j.gt.np)) goto 10
          buf(j,l-top+1)=dnout
10      continue
      else

c draw long horizontal vectors
        if(abs(dx).lt.1.0) return
        if(x1.gt.x3)then
          k=-1
        else
          k=1
        endif
        ratio=k*dy/dx
        y=y1-ratio
        do 20 j=nint(x1),nint(x3),k
          y=y+ratio
          l=nint(y)
          if((j.lt.1).or.(j.gt.np)) goto 20
          if((l.lt.top).or.(l.gt.bot)) goto 20
          buf(j,l-top+1)=dnout
20      continue
      endif
      return
      end

