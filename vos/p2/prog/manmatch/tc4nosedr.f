c *******************************************************************
      subroutine tc4nosedr(winsl1,winss1,winsl2,winss2,
     +   side,sl1,ss1,nl1,ns1,sl2,ss2,nl2,ns2,
     +   vrdi_unit,winnl,winns,bottom,right,zoom1,zoom2,
     +   lbox,pair,graphicsplane)
c move correlation window & get cursor location.
c side=0 means first pass for new tiepoint
c side=1 left side
c side=2 right side

      integer zoom1,zoom2
      integer four,five,zero,one,xcurs,ycurs
      integer oldxcurs,oldycurs,value
      integer two,three,gstatus,pair
      integer xdclocation,xdipolyline,xdxswitch,xdcset
      integer winnl,winns,winsl1,winss1,winsl2,winss2,side
      integer sl1,ss1,sl2,ss2,bottom,right
      integer   xbox(5),ybox(5),vrdi_unit
      byte      i255, i0
      data one/1/,four/4/,i255/255/,five/5/,zero/0/,two/2/
      data three/3/, i0/0/
      integer graphicsplane
    
     
      pair=0
      side=0
      nr=right/6
      xcurs=right/4
      ycurs=(nl1/zoom1)/2
      oldxcurs=xcurs
      oldycurs=ycurs
      gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdcset error #.')
          call prnt(4,1,xcurs,'xcurs value=.')
          call prnt(4,1,ycurs,'ycurs value=.')
c         call abend
      endif
      winsl1=ycurs*zoom1+sl1-1
      winss1=xcurs*zoom1+ss1-1
      call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif

      winsl2=(ycurs*zoom2)+sl2-1
      winss2=(xcurs*zoom2)+ss2-1
      winsl1=(ycurs*zoom1)+sl1-1
      winss1=(xcurs*zoom1)+ss1-1

c main loop
ccc 30    call vwait(100)

30	continue
      value=0          

c     Check for QUIT condition (left button)
      gstatus=xdxswitch(vrdi_unit,one,one,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
c         call abend
      endif
      if(value.eq.1)then
         if(side.eq.0)then
            gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,
     +        				five,xbox,ybox)
            if(gstatus.ne.1)then
                call prnt(4,1,gstatus,'xdipolyline error #.')
c               call abend
            endif
         endif
         return
      endif


c     Check for LEFT side condition (left button)
      gstatus=xdxswitch(vrdi_unit,one,two,value)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdxswitch error #.')
c         call abend
      endif
      if(value.eq.1)then
         value=0
         side=1
         xcurs=(winss1-ss1+1)/zoom1
         ycurs=(winsl1-sl1+1)/zoom1
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
c            call abend
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
        goto 30
      endif

c     Check for RIGHT side condition (third button)
      gstatus=xdxswitch(vrdi_unit,one,three,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
c         call abend
      endif
      if(value.eq.1)then
         value=0
         side=2
         xcurs=(winss2-ss2+1)/zoom2 + (right/2)
         ycurs=(winsl2-sl2+1)/zoom2
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
c            call abend
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom2,winnl/zoom2)
         goto 30
      endif

c read new cursor position.
      gstatus=xdclocation(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdlocation error #.')
c         call abend
      endif

c restrict correlation area to be within the display area.
      if(side.eq.2)then
         if(xcurs.lt.right/2) xcurs=right/2
         if(xcurs+(winns/zoom2)-1.gt.right)
     +        xcurs=right-(winns/zoom2)+1
      else
         if(xcurs.lt.1) xcurs=1
         if(xcurs+(winns/zoom1)-1.gt.right/2-1)
     +        xcurs=right/2-1-winns/zoom1+1
      endif
      if(ycurs+(winnl/zoom1)-1.gt.bottom) 
     +        ycurs=bottom-winnl/zoom1+1
      if(ycurs.lt.1)ycurs=1

      if(xcurs.eq.oldxcurs.and.ycurs.eq.oldycurs) goto 30
      oldxcurs=xcurs
      oldycurs=ycurs
c erase the old box location
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif
c update picture coordinates
      if(side.eq.2)then
         winsl2=ycurs*zoom2+sl2-1
         winss2=(xcurs-right/2)*zoom2+ss2-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +        				winns/zoom2,winnl/zoom2)
      else
         winsl1=ycurs*zoom1+sl1-1
         winss1=xcurs*zoom1+ss1-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
      endif
c draw in box
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif

c Check if cursor is in one of the control areas below left picture.
      if((ycurs.le.lbox).or.(xcurs.ge.3*nr)
     +    .or.(ycurs.gt.lbox+100) ) goto 30
      if(xcurs.lt.nr)then
         pair=-1
         return
      else if(xcurs.lt.2*nr)then
         pair=1
         return
      else
         side=0
         return
      endif

      goto 30
      end


