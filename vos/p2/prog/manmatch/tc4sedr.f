c *******************************************************************
      subroutine tc4sedr(winsl1,winss1,winsl2,winss2,
     +   side,sl1,ss1,nl1,ns1,sl2,ss2,nl2,ns2,
     +   vrdi_unit,winnl,winns,bottom,right,zoom1,zoom2,
     +   lbox,pair,datal,datar,conv,graphicsplane)
c SEDR assisted tiepoint acquisition.
c move correlation window & get cursor location.
c side=0 means first pass for new tiepoint
c side=1 left side
c side=2 right side

      integer zoom1,zoom2,vrdi_unit
      integer oldxcurs,oldycurs,xcurs,ycurs
      integer gstatus,pair,value,side
      integer xdclocation,xdipolyline,xdxswitch,xdcset
      integer winnl  			! winnl = x = 64,  from CORR(x,y)
      integer winns        		! winns = y = 64,  from CORR(x,y)	
      integer winsl1,winss1		! line/samp of 1st image
      integer winsl2,winss2		! line/samp of 2nd image
      integer sl1			! sl1 from U1 or D1
      integer sl2			! sl2  
      integer ss1,ss2			! ss1 = ss2 = 1
      integer bottom
      integer right			! 512=right side of window
      integer xcursr,ycursr,xboxr(5),yboxr(5)
      integer xbox(5),ybox(5)
      integer nr			! size of each control box
      real*4    lat,lon
      real*4    winsl, winss
      real*4   datal(40),datar(40)
      real   conv(1)
      
      integer two,three,four,five,zero,one
      byte     i255, i0
      data one/1/,four/4/,i255/255/,five/5/,zero/0/,two/2/
      data three/3/, i0/0/
      
      integer graphicsplane
      
      
c     SET CURSOR     
      pair=0
      side=0
      xcurs=right/4
      ycurs=(nl1/zoom1)/2
      oldxcurs=xcurs
      oldycurs=ycurs
      xcursr=xcurs+right/2
      ycursr=ycurs
      nr=right/6
      gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdcset error #.')
         call prnt(4,1,xcurs,'xcurs value=.')
         call prnt(4,1,ycurs,'ycurs value=.')
      endif
      
c     DRAW BOX IN LEFT IMAGE (ONLY)
      winsl1=ycurs*zoom1+sl1-1
      winss1=xcurs*zoom1+ss1-1
      call get_square_coord(xbox,ybox,xcurs,ycurs,
     +      			winns/zoom1,winnl/zoom2)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
      endif


c main loop
ccc30    call vwait(50)
30    continue
      value=0          

c     CHECK FOR QUIT CONDITION (left trackball button)
      gstatus=xdxswitch(vrdi_unit,one,one,value)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdxswitch error #.')
      endif
      if(value.eq.1)then
         if(side.ne.2)then
                  
c            gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,
c     +            			five,xbox,ybox)
            if(gstatus.ne.1)then
               call prnt(4,1,gstatus,'xdipolyline error #.')
            endif
         endif
         return
      endif


c     CHECK FOR LEFT SIDE CONDITION (left button)
      gstatus=xdxswitch(vrdi_unit,one,two,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
      endif
      if(value.eq.1)then
         value=0
         side=1
         xcurs=(winss1-ss1+1)/zoom1			! new cursor location 
         ycurs=(winsl1-sl1+1)/zoom1			
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
        goto 30
      endif

c     CHECK FOR RIGHT SIDE CONDITION (right button)
      gstatus=xdxswitch(vrdi_unit,one,three,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
      endif
      if(value.eq.1)then
         value=0
         side=2
         xcurs=(winss2-ss2+1)/zoom2 + right/2
         ycurs=(winsl2-sl2+1)/zoom2
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         			winns/zoom2,winnl/zoom2)
         goto 30
      endif

c     GET NEW CURSOR LOCATION
      gstatus=xdclocation(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlocation error #.')
      endif


c     RESTRICT CURSOR TO BE WITHIN CORRELATION/DISPLAY AREA
      if(side.eq.2)then   					! right side
         if(xcurs.lt.right/2) xcurs=right/2
         if(xcurs+(winns/zoom2)-1.gt.right)
     +        xcurs=right-(winns/zoom2)+1
      else							! left side
         if(xcurs.lt.1) xcurs=1
         if(xcurs+(winns/zoom1)-1.gt.right/2-1)
     +        xcurs=(right/2)-1-(winns/zoom1)+1
      endif

      if(xcurs.eq.oldxcurs.and.ycurs.eq.oldycurs) goto 30
      oldxcurs=xcurs
      oldycurs=ycurs

c     ERASE THE OLD BOX(s)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,five,
     +         			  xbox,ybox)
      if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
      endif
      if(side.ne.2) then    ! erase right box too
         call get_square_coord(xboxr,yboxr,xcursr,ycursr,
     +         			  winns/zoom2,winnl/zoom2)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,five,
     +         			  xboxr,yboxr)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
c            call abend
         endif
      endif

c     DRAW RIGHT BOX AT LATEST CURSOR LOCATION (if cursor in right)
      if(side.eq.2)then
         winsl2=ycurs*zoom2+sl2-1
         winss2=(xcurs-right/2)*zoom2+ss2-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,winns/zoom2,
     +         			winnl/zoom2)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,
     +         			xbox,ybox)
         if(gstatus.ne.1)then
            call prnt(4,1,gstatus,'xdipolyline error #.')
         endif
      else
         winsl1=ycurs*zoom1+sl1-1
         winss1=xcurs*zoom1+ss1-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         			winns/zoom1,winnl/zoom1)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,
     +         			xbox,ybox)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
         endif
      endif


c     DRAW SEDR BOX ON RIGHT SIDE (IF CURSOR IN LEFT)         
      if(side.ne.2)then   
c        convert from line,samp to lat,lon in left image
         winsl=winsl1	!+(winnl/(2*zoom1))  
         winss=winss1	!+(winns/(2*zoom1))  

C        print *, 'RIGHT WINSL1 ', winsl1, ' WINSS1 ', winss1
         
         call convev(ind,datal,datal,winsl,winss,lat,lon,two,conv)
         if(ind.ne.0)then
            call xvmessage ('Point off planet', ' ')
            goto 31
         endif

c        convert lat,lon to line,samp in right image

         call convev(ind,datar,datar,winsl,winss,lat,lon,one,conv)
         if(ind.ne.0)then
            call xvmessage('                     Point off planet',' ')
            goto 31
         endif
         winsl2=nint(winsl)	!-(winnl/(2*zoom2))
         winss2=nint(winss)	!-(winns/(2*zoom2))
         
         xcursr=(winss2-ss2+1)/zoom2+(right/2)
         ycursr=(winsl2-sl2+1)/zoom2
         
         if(xcursr.lt.right/2) xcursr=right/2
         if(xcursr+winns/zoom2-1.gt.right)
     +        xcursr=right-winns/zoom2+1
         if(ycursr+winnl/zoom2-1.gt.bottom) 
     +        ycursr=bottom-winnl/zoom2+1
         if(ycursr.lt.1)ycursr=1
         winsl2=ycursr*zoom2+sl2-1
         winss2=(xcursr-right/2)*zoom2+ss2-1
C        print *, 'LEFT WINSL2 ', winsl2, ' WINSS2 ', winss2
         call get_square_coord(xboxr,yboxr,xcursr,ycursr,
     +         				winns/zoom2,winnl/zoom2)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xboxr,
     +         				yboxr)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
         endif
      endif


c     CHECK IF CURSOR IS IN ONE OF CONTROL AREAS BELOW IMAGES
31    continue
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



