C==============================================================================
c split screen display controlling routine...
C==============================================================================
      subroutine sdsply(pic1,nl1,ns1,pic2,nl2,ns2,*)
      IMPLICIT NONE
      integer ns1,nl1,ns2,nl2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'dev.fin'

      common/c1_buf/buf(1024)
      integer buf
      common/chist/his(256,2),hflg(2),ihis(256,2),ihflg(2),nspikes
      integer his,hflg,ihis,ihflg,nspikes
      common/ipic2/nl(2),ns(2)
      integer nl,ns
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss

      logical xst
      integer*4 par(2),iflag,mpv,il,ih,n

      logical xdifill,parmtst

      iflag = 0

      if (parmtst('H', mpv, 1)) then
         call home(0,pic1,nl1,ns1,pic2,nl2,ns2)
         iflag = 1
      endif

      if (parmtst('STRETCH',par, 2)) then
           il = par(1)
           ih = par(2) 
           call strech(il,ih,buf)
           call lutwrite(idev,buf)
           iflag = 1
      endif

      if (parmtst('STR1',par, 2)) then
         ilow(1) = par(1)
         ihigh(1) = par(2)
         call strech(ilow,ihigh,buf)
         call mve(-5,256,buf,stbl, 1, 1)
         call spic(1,pic1,nl1,ns1,buf)
         call tpdisplay(1)
         iflag = 1
      endif

      if (parmtst('STR2',par, 2)) then
         ilow(2) = par(1)
         ihigh(2) = par(2)
         call strech(ilow(2),ihigh(2),buf)
         call mve(-5,256,buf,stbl(1,2), 1, 1)
         call spic(2,pic2,nl2,ns2,buf)
         call tpdisplay(2)
         iflag = 1
      endif

      if (parmtst('GERASE', mpv, 1)) then
           xst = xdifill(idev,g,0)
           iflag = 1
      endif

      if (parmtst('SPIKES',n, 2)) then
           nspikes=n
           iflag = 1
      endif

      if (parmtst('SPTS', mpv, 1)) then
           call tpdisplay(1)
           call tpdisplay(2)
           iflag = 1
      endif

      if (parmtst('HIST',mpv,1)) then
         call hisgen(pic1,sl(1),ss(1),nl1,ns1,ihis,ihflg(1))
         call hdisplay(idev,g,ihis,101,101,100,nspikes)
         call hisgen(pic2,sl(2),ss(2),nl2,ns2,ihis(1,2),ihflg(2))
         call hdisplay(idev,g,ihis(1,2),101,nsd/2+101,100,nspikes)
         iflag = 1
      endif

      if (iflag.eq.1) return1
      return
      end

C==============================================================================
c routine to move the window(s) of the split-screen display, acting on
c commands input by the user.  if any of the commands below are detected,
c then video plane v2 is used.  alternate return if any action is taken.
C==============================================================================
      subroutine swindow(pic1,nl1,ns1,pic2,nl2,ns2,*)
      IMPLICIT NONE
      integer nl1, ns1, nl2, ns2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'dev.fin'

      common/c1_buf/buf(1024)
      integer buf
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      integer sl1,ss1,sl2,ss2,n
      logical both,left,right,parmtst

c     ....first, reset all flags
      left = .false.		!display left screen
      right = .false.		!display right screen
      both = .false.		!display both screens

      sl1 = sl(1)
      ss1 = ss(1)
      sl2 = sl(2)
      ss2 = ss(2)

      if (parmtst('CZOOM',n, 2)) then
         izoom1 = n
         zoom1 = n
         if (n .lt. 0) zoom1 = -1./n
         call czoom(zoom1,sl1,ss1,sl2,ss2,*999)
         both = .true.
         goto 100
      endif

      if (parmtst('SL',n, 2)) then
         sl1 = n
         sl2 = n
         both = .true.
      endif

      if (parmtst('SL1',n, 2)) then
         sl1 = n
         left = .true.
      endif

      if (parmtst('SL2',n, 2)) then
         sl2 = n
         right = .true.
      endif

      if (parmtst('SS',n, 2)) then
         ss1 = n
         ss2 = n
         both = .true.
      endif

      if (parmtst('SS1',n, 2)) then
         ss1 = n
         left = .true.
      endif

      if (parmtst('SS2',n, 2)) then
         ss2 = n
         right = .true.
      endif

      if (parmtst('U',n, 2))   then
         sl1 = sl1 - n
         sl2 = sl2 - n
         both = .true.
      endif

      if (parmtst('D',n, 2))   then
         sl1 = sl1 + n
         sl2 = sl2 + n
         both = .true.
      endif

      if (parmtst('U1',n, 2)) then
         sl1 = sl1 - n
         left = .true.
      endif

      if (parmtst('D1',n, 2)) then
         sl1 = sl1 + n
         left = .true.
      endif

      if (parmtst('U2',n, 2)) then
         sl2 = sl2 - n
         right = .true.
      endif

      if (parmtst('D2',n, 2)) then
         sl2 = sl2 + n
         right = .true.
      endif

      if (parmtst('L',n, 2))  then
         ss1 = ss1 - n
         ss2 = ss2 - n
         both = .true.
      endif

      if (parmtst('R',n, 2))  then
         ss1 = ss1 + n
         ss2 = ss2 + n
         both = .true.
      endif

      if (parmtst('L1',n, 2)) then
         ss1 = ss1 - n
         left = .true.
      endif

      if (parmtst('R1',n, 2)) then
         ss1 = ss1 + n
         left = .true.
      endif

      if (parmtst('L2',n, 2)) then
         ss2 = ss2 - n
         right = .true.
      endif

      if (parmtst('R2',n, 2)) then
         ss2 = ss2 + n
         right = .true.
      endif

      if (parmtst('ZOOM',n, 2)) then
         izoom1 = n
         zoom1 = n
         if (n .lt. 0) zoom1 = -1./n
         both = .true.
      endif

  100 if (both) then
          left = .true.
          right = .true.
      endif

      if (.not.left .and. .not.right) return

      if (vid.ne.v2) then
         vid = v2
         call impon(idev,vid)
      endif

      zoom = zoom1
      izoom = izoom1

      if (left) then
         call cwindow(1,sl1,ss1)
         call spic(1,pic1,nl1,ns1,buf)
         call tpdisplay(1)
      endif

      if (right) then
         call cwindow(2,sl2,ss2)
         call spic(2,pic2,nl2,ns2,buf)
         call tpdisplay(2)
      endif

  999 return1
      end

C==============================================================================
c routine to check window movement to keep display within picture boundaries.
c inputs: ipic=1 for left image, =2 for right image
c         (sli,ssi)=picture offsets for display
c outputs: sl(ipic),ss(ipic)
C==============================================================================
      subroutine cwindow(ipic,sli,ssi,ind)
      IMPLICIT NONE
      integer ipic,sli,ssi,ind

      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      character*80 msg
      integer maxsl,maxss

  110 format(' (SL',i1,',SS',i1,')=(',i4,',',i4,')  ZOOM=',i2)

      maxsl = nl(ipic) - nld/zoom + 1
      maxss = ns(ipic) - nsd/(2.*zoom) + 1
      sli = min0(sli,maxsl)
      ssi = min0(ssi,maxss)
      sl(ipic) = max0(sli,1)
      ss(ipic) = max0(ssi,1)
      write(msg,110) ipic,ipic,sl(ipic),ss(ipic),izoom
      call xvmessage(msg,' ')
      return
      end

C==============================================================================
c full-frame split-screen display.  all arguments are inputs.
C==============================================================================
      subroutine home(mode,pic1,nl1,ns1,pic2,nl2,ns2)
      IMPLICIT NONE
      integer mode,ns1,nl1,ns2,nl2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

C      common/c1_buf/buf(1024)		!temporary work area
      byte buf(1024)

      vid = v1
      zoom = zoom2
      izoom = izoom2
      call impon(idev,vid)
      sl(1) = 1
      ss(1) = 1
      sl(2) = 1
      ss(2) = 1
c     ....display if necessary
      if (v1.eq.v2.or.mode.eq.1) then
         call spic(1,pic1,nl1,ns1,buf)	!display left image
         call spic(2,pic2,nl2,ns2,buf)	!display right image
      endif
      call tpdisplay(1)			!display left tiepoints
      call tpdisplay(2)			!display right tiepoints
      return
      end

C==============================================================================
c determine line and sample offsets which center the left and right display
c about a point selected by the user via the cursor.
c outputs: (sl1,ss1) and (sl2,ss2)
C==============================================================================
      subroutine czoom(zoom_1,sl1,ss1,sl2,ss2,*)
      IMPLICIT NONE
      integer*4 sl1,ss1,sl2,ss2
      real zoom_1

      include 'dev.fin'

      integer*4 s1,s2,mpv,l1,l2,ind
      real*8 rl1,rs1,rlat,rlon,rl2,rs2
      logical parmtst,xst,xdcset

c     ....user selects point in left image
   10 call xvintract('READY',' Position LEFT Cursor')
      if (parmtst('EXIT', mpv, 1)) return1
      call cursor(1,l1,s1,rl1,rs1,*10)
c     ....predict corresponding location of point in the right image
      call latlon2(1,rl1,rs1,rlat,rlon,ind)
      if (ind.eq.1) call linsam2(2,rlat,rlon,rl2,rs2,ind)
      if (ind.eq.1) then
         l2 = (rl2-sl2)*zoom
         s2 = (rs2-ss2)*zoom + nsd/2
         l2 = max0(l2,1)
         s2 = max0(s2,nsd/2)
         l2 = min0(l2,nld)
         s2 = min0(s2,nsd)
      else
         l2 = l1
         s2 = s1 + nsd/2
      endif
      xst = xdcset(idev,tb,s2,l2)	!move cursor to point in right image
c     ....user selects point in right image
   20 call xvintract('READY',' Position RIGHT Cursor')
      if (parmtst('EXIT', mpv, 1)) return1
      call cursor(2,l2,s2,rl2,rs2,*20)
c     ....compute line and sample offsets in the display
      sl1 = rl1 - nld/(2.*zoom_1)
      ss1 = rs1 - nsd/(4.*zoom_1)
      sl2 = rl2 - nld/(2.*zoom_1)
      ss2 = rs2 - nsd/(4.*zoom_1)
      xst = xdcset(idev,tb,nsd/4,nld/2)		!move cursor to left image
      return
      end

C==============================================================================
c routine to write the image to split-screen display
C==============================================================================
      subroutine spic(ipic,pic,nli,nsi,buf)
      IMPLICIT NONE
      integer ipic,nsi,nli
      byte pic(nsi,nli),buf(1024)

      include 'dev.fin'

      common/chist/his(256,2),hflg(2),ihis(256,2),ihflg(2),nspikes
      integer his,hflg,ihis,ihflg,nspikes
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss

      integer sli,ssi,nsds,nlds,ssds,istrech,esds,slds,inc,nlo,nso,
     +        l,i,n
      logical xst

      logical xdiawset,xdifill,xdilinewrite

      hflg(ipic) = 0
      ihflg(ipic) = 0
      sli = sl(ipic)
      ssi = ss(ipic)
      nsds = nsd/2
      nlds = nld
      ssds = 1
      if (ipic.eq.2) ssds = nsds + 1
      esds = ssds + nsds - 1
      xst = xdiawset(idev,vid,ssds,1,esds,nlds)	!set access window
      xst = xdifill(idev,vid,0)			!erase image plane
c     ....determine if a stretch is to be applied before writing to display
      if (ilow(ipic).eq.0.and.ihigh(ipic).eq.255) then
         istrech = 0		!no stretch
      else
         istrech = 1		!apply stretch
      endif
c
      if (izoom.eq.1) then
          nlds = min0(nli-sli+1,nlds)
          if (ipic.eq.1) nsds = nsds - 1
          nsds = min0(nsi-ssi+1,nsds)
          xst = xdiawset(idev,vid,ssds,1,ssds+nsds-1,nlds)

          do slds=1,nlds
             if (istrech.eq.1) then
                call lut(nsds,pic(ssi,sli+slds-1),stbl(1,ipic),buf)
                xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
             else
       write(*,*) 'Not changing LUT'
                xst = xdilinewrite(idev,vid,ssds,slds,nsds,
     &			pic(ssi,sli+slds-1))
             endif
             if (.not.xst) goto 999
          enddo
          xst = xdiawset(idev,vid,1,1,nsd,nld)	!reset window to full-screen
          return
      endif

      if (izoom.lt.1) then
          inc = -izoom
          nlds = min0((nli-sli)/inc+1,nlds)
          n = (nsi-ssi)/inc + 1
          if (n.lt.nsds) then
              nsds = n
              xst = xdiawset(idev,vid,ssds,1,ssds+nsds-1,nlds)
          endif
          if (ipic.eq.1) then
               buf(nsds)=.false.	!put vertical black line
               n = nsds - 1		!between left and right frames
          else
               n = nsds
          endif
          do slds=1,nlds
             call mve(1,n,pic(ssi,sli),buf,inc, 1)    ! check the 1 for binc
             if (istrech.eq.1) call lut(n,buf,stbl(1,ipic),buf)
             xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
             if (.not.xst) goto 999
             sli = sli + inc
          enddo
          xst = xdiawset(idev,vid,1,1,nsd,nld)	!reset window to full-screen
          return
      endif


      if (izoom.gt.1) then
         nlo = min0(nli-sli+1,nlds/izoom)
         nso = min0(nsi-ssi+1,nsds/izoom)
         nsds = nso*izoom
         xst = xdiawset(idev,vid,ssds,1,ssds+nsds-1,nlds)
         slds = 0

         do l=1,nlo
            if (istrech.eq.1) then
                call lut(nsds,pic(ssi,sli+l-1),stbl(1,ipic),buf)
                call expand(buf,buf,nso,izoom)
            else
                call expand(pic(ssi,sli+l-1),buf,nso,izoom)
            endif
            if (ipic.eq.1) buf(nsds)=.false.
            do i=1,izoom
               slds = slds + 1
               xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
               if (.not.xst) goto 999
            enddo
         enddo
         xst = xdiawset(idev,vid,1,1,nsd,nld)	!reset window to full-screen
         return
      endif
c
      return
c     ....here if error writing to image display
  999 return
      end

C==============================================================================
c routine to display a histogram on the graphics plane g, starting at
c pixel coordinates (l0,s0).
C==============================================================================
      subroutine hdisplay(idev,g,his,l0,s0,ihght,nspikes)
      IMPLICIT NONE
      integer idev,g
      integer his(1),l0,s0,ihght,nspikes,maxf/268435455/,x,y,dx,tic

      include 'xdgclr.fin'

      integer xx(10),yy(10),n,maxs,max,j,ifreq,i,l,ierr
      real height,zscale
      character*3 msg

      integer xdttext,xdipolyline,xdtcolor

c          linc must be a power of two
      n = nspikes + 1
      maxs = maxf
c          scale histogram to max frequency
      do j=1,n
         max = 0
         do i=2,254
            ifreq = his(i)
            if (ifreq.gt.max .and. ifreq.lt.maxs) max=ifreq
         end do
         maxs = max
      end do
c
      height = ihght
      zscale = 1.
      if (max.gt.1) zscale = height/alog10(float(max))
      x = s0
      y = l0

      ierr=xdtcolor(RED,0)
      do 50 l=0,255
      tic = 0
      if (mod(l,10).eq.0) tic=2
      if (mod(l,50).eq.0) then
C           call outcon(l,msg(3),3)
	   write(msg, '(I3)') l
           ierr = xdttext(idev,g,x-34,y+4,1,3,msg)
           tic = 4
      endif

      ifreq = his(l+1)
      if (ifreq.gt.1) then
           dx = zscale*alog10(float(ifreq))
      else
           dx = 0.0
      endif

      if(dx.le.height) goto 30
      dx = height
      xx(1) = x + dx + 2
      yy(1) = y
      xx(2) = x + dx + 2
      yy(2) = y
      ierr = xdipolyline(idev,g,RED,2,xx,yy)
   30 xx(1) = x - tic
      yy(1) = y
      xx(2) = x + dx
      yy(2) = y
      ierr = xdipolyline(idev,g,RED,2,xx,yy)
      y = y + 1
   50 continue
c
      return
      end

C==============================================================================
c routine to display the tiepoints in graphics...
c ipic=1 to display tiepoints in left picture
c     =2 to display tiepoints in right picture
C==============================================================================
      subroutine tpdisplay(ipic)
      IMPLICIT NONE
      integer ipic

      include 'cpts.fin'
      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      character*3 gmsg
      integer s,l,sld,ssd,nsds,ssds,esds,i,nch
      logical xst
      real rl,rs

      logical xdiawset,xdifill,xdipixelwrite,xdttext

      nsds = nsd/2
      ssds = 1
      if (ipic.eq.2) ssds=nsds+1
      esds = ssds + nsds - 1
      xst = xdiawset(idev,g,ssds,1,esds,nld)	!set access window
      xst = xdifill(idev,g,0)			!erase graphics

      sld = sl(ipic)
      ssd = ss(ipic)

      do 50 i=1,npts
      if (ipic.eq.1) then
          rl = lpt(1,i)
          rs = lpt(2,i)
      else
          rl = rpt(1,i)
          rs = rpt(2,i)
      endif

      if (rl.lt.0.0.or.rs.lt.0.0) goto 50
      l = (rl-sld)*zoom + 1.5
      s = (rs-ssd)*zoom + 1.5
      if (l.lt.0.or.s.lt.0) goto 50
      if (l.gt.nld.or.s.gt.nsds) goto 50
      if (ipic.eq.2) s=s+nsds
      if (.not.xdipixelwrite(idev,g,s,l,255)) goto 999
C      nch = 1
C      if (i.ge.10) nch=2
C      if (i.ge.100) nch=3
C      call outcon(i,gmsg(nch),nch)
      if (i.lt.10) then
      	  write(gmsg, '(I1)') i
	  nch = 1
      else if (i.ge.10) then
      	  write(gmsg, '(I2)') i
	  nch = 2
      else if (i.ge.100) then
      	  write(gmsg, '(I3)') i
	  nch = 3
      endif
      xst = xdttext(idev,g,s+2,l+10,1,nch,gmsg)
   50 continue

      xst = xdiawset(idev,g,1,1,nsd,nld)  !reset window to full-screen
      return
c     ....here if error writing to graphics display
  999 return
      end

C==============================================================================
c read cursor and return display coordinates (l,s) and image
c coordinates (rl,rs) for left or right image.
C==============================================================================
      subroutine cursor(ipic,l,s,rl,rs,*)
      IMPLICIT NONE
      integer ipic,l,s
      real*8 rl,rs

      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      logical xst

      logical xdclocation

      xst = xdclocation(idev,tb,s,l)
      rl = (l-1)/zoom + sl(ipic)
      if (ipic.eq.1) then
         rs = (s-1)/zoom + ss(ipic)
         if (rl.gt.nl(1).or.rs.gt.ns(1).or.s.gt.nsd/2) goto 995
      else
         rs = (s-nsd/2-1)/zoom + ss(ipic)
         if (rl.gt.nl(2).or.rs.gt.ns(2).or.s.le.nsd/2) goto 996
      endif
      return

  995 call xvmessage(' ***Cursor lies off LEFT picture',' ')
      goto 999
  996 call xvmessage(' ***Cursor lies off RIGHT picture',' ')
  999 return1
      end

C==============================================================================
c generate histogram from area (sl,ss,nl,ns) of input image.
c output: his = 256 grey-level histogram.
c updated: upon return, hflg =1
C==============================================================================
      subroutine hisgen(pic,sl,ss,nl,ns,his,hflg)
      IMPLICIT NONE
      integer sl,ss,ns,nl,his(256),hflg
      byte pic(ns,nl)

      include 'dev.fin'

      integer nlo,nso,el,inc,l

      if (hflg.eq.1) return		!skip if histogram already computed
      nlo = nld/zoom + 0.001
      nso = nsd/(2.*zoom) + 0.001
      nlo = min0(nl-sl+1,nlo) - 1
      nso = min0(ns-ss+1,nso)
      el = sl + nlo - 1
      inc = 1
      if (izoom.lt.0) inc=-izoom
      call zia(his,256)			!zero out the histogram
c     ....accumulate the histogram
      do l=sl,el,inc
         call hsub(1,nso,pic(ss,l),his)
      enddo

      hflg = 1
      return
      end

C==============================================================================
c generate a stretch table (stbl) to linearly stretch an image.
C==============================================================================
      subroutine strech(i1,i2,stbl)
      IMPLICIT NONE
      integer i1,i2,stbl(1)

      integer d,offset,k,i

      d = i2 - i1
      if (d .eq. 0) d = 1
      offset = d/2 - 255*i1
      k = 0

   10 i = (255*k+offset)/d
      if (i .lt. 0) i=0
      if (i .gt. 255) i=255
      stbl(k+1) = i
      k = k + 1
      if (k .le. 255) goto 10
c
      return
      end

C==============================================================================
c expand an image line by and integral factor (izoom) by pixel replication...
c input array: pic    output array: buf
c note the pic and buf may be concurrent...
C==============================================================================
      subroutine expand(pic,buf,ns,izoom)
      IMPLICIT NONE
      byte pic(1),buf(1)
      integer ns,izoom

      integer k,i,j

      k = izoom*ns
      do i=ns,1,-1
           do j=1,izoom
               buf(k) = pic(i)
               k = k - 1
           enddo
      enddo
      return
      end

C==============================================================================
c open and initialize the video plane, graphics plane, and cursor.
c alternate return if unsuccessful.
C==============================================================================
      subroutine device2(nl1,ns1,nl2,ns2,*)
      IMPLICIT NONE
      integer nl1,ns1,nl2,ns2

      include 'dev.fin'
      include 'xdgclr.fin'

      integer buf(256), info(80), con(4),lut,xdsvnl,xdsvns,nl,ns,n,i

      logical xdeaction,xddunit,xddopen,xddactivate,xddinfo,
     +        xddconfigure,xdlramp,xdgconnect,xdglinit,
     +        xdgon,xdtfont,xdtsize,xdtrotate,xdcon,xdcset,
     +        xdcautotrack,xdiawset,xdsgraph
      integer xdgcolor

      if (.not.xdeaction(2,2,3)) return1	!define error action
      if (.not.xddunit(idev)) return1		!get logical unit number
      if (.not.xddopen(idev)) return1		!open device
      if (.not.xddactivate(idev,.true.)) return1 !activate device
      if (.not.xddinfo(idev,1,80,info)) return1
      v1 = 1				!variable field image plane
      v2 = 2				!full-field (home) image plane
      v3 = 3				!unused
      if (info(4).lt.2) v2=v1
      if (info(4).lt.3) v3=v2

      tb = 1				!cursor
c     ....configure the display for b/w mode
      con(1) = 0			!default color display
      con(2) = 0			!default memory plane size
      con(3) = 0			!default video size
      con(4) = 0			!default aspect ratio
      if (.not.xddconfigure(idev,con)) return1
c     ....eliminate stretches in luts
      do lut=1,3
         if (.not.xdlramp(idev,lut,1)) return1
      enddo
      g = xdsgraph(idev)   ! g = graphic plane

c     ....pick plane for g
      if (.not.xdgconnect(idev,g,1,.false.)) return1
      if (.not.xdglinit(idev,1)) return1
      RED = xdgcolor(idev, 'RED')
      CYAN = xdgcolor(idev,'CYAN')
      if (RED*CYAN .EQ. 0) return1
      TRANSPARENT = 0
      
      if (.not.xdgon(idev)) return1		!turn g on
      if (.not.xdtfont(30)) return1		!set font type
      if (.not.xdtsize(8,1.0)) return1		!set text size
      if (.not.xdtrotate(0)) return1
      if (.not.xdcon(idev,tb,1,0)) return1	!turn cursor on
      if (.not.xdcset(idev,tb,256,256)) return1	!center cursor
      if (.not.xdcautotrack(idev,tb,tb,.true.)) return1
c     ....initialize zoom factors.
      nl = max0(nl1,nl2)
      ns = max0(ns1,ns2)
      nld = xdsvnl(idev)
      nsd = xdsvns(idev)
      n = max0((nl-1)/nld,2*(ns-1)/nsd) + 1
      if (n.gt.1) then
         zoom1 = 1.0/n
         izoom1 = -n
      else
         zoom1 = 1
         izoom1 = 1
      endif

      zoom2 = zoom1
      izoom2 = izoom1
c     ....set access window to full-screen
      if (.not.xdiawset(idev,g,1,1,nsd,nld)) return1
      if (.not.xdiawset(idev,v1,1,1,nsd,nld)) return1
      if (.not.xdiawset(idev,v2,1,1,nsd,nld)) return1

c     ....initialize stretch tables
      do i=1,2
         ilow(i) = 0
         ihigh(i) = 255
         call strech(0,255,buf)
         call mve(-5,256,buf,stbl(1,i), 1, 1)
      enddo
      return
      end
c connects all luts to specified image plane (imp)
c
      subroutine impon(idev,imp)

      do lut=1,3
         call xdlconnect(idev,imp,lut,1,.false.)
      end do
      return
      end

      subroutine lutwrite(idev,stbl)
      integer stbl(256)

      do lut=1,3
         call xdlwrite(idev,lut,1,stbl)
      end do
      return
      end

C Detect end of zero-terminated string and blank-fill.
      SUBROUTINE bfstr(buf,nchar)
      IMPLICIT NONE
      integer nchar
      byte buf(nchar)

      integer izero,i

      izero = 0
      do i=1,nchar
         if (buf(i).eq.0) izero=1
         if (izero.eq.1) buf(i)=32
      enddo
      return
      end


