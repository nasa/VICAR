      include 'VICMAIN_FOR'

      subroutine main44


      parameter (nlbuf=1200,nsbuf=1000,ntable=10000,numbox=150)

c nlbuf = max # lines in two first input files
c nsbuf = max # samples in two first input files
c ntable = max # tiepoints
c numbox = max # horizontal boxes (nsbuf/parameter INC)

      integer*2 dnleft(nsbuf,nlbuf),dnright(nsbuf,nlbuf)
      integer*2 dnout(nsbuf)
      integer*4 status,outunit,count,def,located(20)
      integer   maxcnt, nods, inc, nptsfit, nstart
      real*4 left_line(ntable),left_samp(ntable)
      real*4 right_line(ntable),right_samp(ntable)
      real*4 morph_line(ntable),morph_samp(ntable)
      real*4 buf(4,32)
      real*4 left,right,top,bottom

      real*8 l_coefx(4,numbox),l_coefy(4,numbox)
      real*8 r_coefx(4,numbox),r_coefy(4,numbox)
      real*8 a(4,4),aa(4,4),rl,rk
      real*8 c(20,4),cl(20),weights(20)

      byte    flag(numbox)
      integer name_length, i, inunit1, inunit2, nl, np, nl2
      integer np2, nboxes, npts, inmark, nl3, ns3, j, n_output
      integer num_start, name_len, lbox, jcol, jbox, i_topleft
      integer i_topright, i_botleft, i_botright, kstat, n, k
      integer jj, l, m  
      real    wt_right, wt_left, gridl, grids, d_topleft, d_topright
      real    d_botleft, d_botright, d, dist, x, y, yy, xx, d2
      real    d1, dx, dy, dntop, dnbot, dnl, dnr

      character*80 filename,fname
      character*1 name1(80),name2(80)

      logical leftonly,xvptst

      equivalence (filename,name1),(fname,name2)

! Begin data initialization

      data status/0/,outunit/0/,count/0/,def/0/,located/20*0/
      data maxcnt/0/, nods/0/, inc/0/, nptsfit/0/, nstart/0/
      data left/0.0/,right/0.0/,top/0.0/,bottom/0.0/
      data rl/0.d0/, rk/0.d0/

      data name_length/0/, i/0/, inunit1/0/, inunit2/0/
      data nl/0/, np/0/, nl2/0/, np2/0/, nboxes/0/, npts/0/
      data inmark/0/, nl3/0/, ns3/0/, j/0/, n_output/0/
      data num_start/0/, name_len/0/, lbox/0/, jcol/0/, m/0/
      data jbox/0/, i_topleft/0/, i_topright/0/, i_botleft/0/
      data i_botright/0/, kstat/0/, n/0/, k/0/, jj/0/, l/0/

      call zia (dnleft, nsbuf*nlbuf/2)
      call zia (dnright, nsbuf*nlbuf/2)
      call zia (dnout, nsbuf/2)
      
      call zia (left_line, ntable)
      call zia (left_samp, ntable)
      call zia (right_line, ntable)
      call zia (right_samp, ntable)
      call zia (morph_line, ntable)
      call zia (morph_samp, ntable)
      call zia (buf, 4*32)
      call zia (l_coefx, 4*numbox*2)
      call zia (l_coefy, 4*numbox*2)
      call zia (r_coefx, 4*numbox*2)
      call zia (r_coefy, 4*numbox*2)
      call zia (r_coefy, 4*numbox*2)
      call zia (a,  4*4*2)
      call zia (aa, 4*4*2)
      call zia (c,  20*4*2)
      call zia (cl, 20*2)
      call zia (weights, 20*2)

      call zia (flag, numbox/4)
      filename = ' '
      fname    = ' '
      do 40000 maxcnt = 1, 80
         name1(maxcnt) = ' '
         name2(maxcnt) = ' '
40000 continue

! End data initialization

      call ifmessage ('MORPH version 31-OCT-95')

      maxcnt = 1
      call xvparm('FRAMES',nods,count,def,maxcnt)
      maxcnt = 80
      call xvparm('NAME',filename,count,def,maxcnt)
      maxcnt = 1
      call xvparm('INC',inc,count,def,maxcnt)
      call xvparm('NPTS',nptsfit,count,def,maxcnt)
      call xvparm('START',nstart,count,def,maxcnt)
      leftonly=xvptst('LEFTONLY')
      if(leftonly) call xvmessage('Only left input image is used',' ')

c find length of filename prefix
      do i=80,1,-1
        if(name1(i).ne.' ') then
          name_length=i
          goto 5
        endif
      enddo
5     continue

c open input images
      call xvunit(inunit1,'INP',1,status,' ')
      call xvopen(inunit1,status,'U_FORMAT','HALF','IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit1,status,'NL',nl,'NS',np,' ')

      call xvunit(inunit2,'INP',2,status,' ')
      call xvopen(inunit2,status,'U_FORMAT','HALF','IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit2,status,'NL',nl2,'NS',np2,' ')
      nl=min(nl,nl2)
      np=min(np,np2)
      nboxes=np/inc

c checks
      if((nl*np.gt.nlbuf*nsbuf).or.(nl.gt.nlbuf).or.(np.gt.nsbuf))then
         call xvmessage ('Input images too large',' ')
         call prnt (4,1,nlbuf,'NL limit=.')
         call prnt (4,1,nsbuf,'NS limit=.')
         call abend
      endif
      if(nboxes.gt.numbox)then
         call xvmessage ('ns/inc <= 100',' ')
         call abend
      endif

c load left image into memory as integer*2 
      do i=1,nl
         call xvread(inunit1,dnleft(1,i),status,'LINE',i,' ')
      enddo
      call xvclose(inunit1,status,'CLOS_ACT','FREE',' ')

c load right image into memory as integer*2 
      do i=1,nl
         call xvread(inunit2,dnright(1,i),status,'LINE',i,' ')
      enddo
      call xvclose(inunit2,status,'CLOS_ACT','FREE', ' ')
      
c load the tiepoint table from the mark file.
      npts=0
      call xvunit(inmark,'INP',3,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl3,'NS',ns3,' ')
      do i=1,nl3
         call xvread(inmark,buf,status,'LINE',i,' ')
         do 10 j=1,32
            if(buf(1,j).eq.0.0) goto 10
            if(buf(2,j).eq.0.0) goto 10
            if(buf(3,j).eq.0.0) goto 10
            if(buf(4,j).eq.0.0) goto 10
            npts=npts+1
            if(npts.gt.ntable)then
               call prnt(4,1,ntable,'Too many input points, limit= .')
               call abend
            endif
            left_line(npts)=buf(1,j)            
            left_samp(npts)=buf(2,j)            
            right_line(npts)=buf(3,j)            
            right_samp(npts)=buf(4,j)            
10       continue
      enddo
      call xvclose(inmark,status,'CLOS_ACT','FREE',' ')

c loop on the number of output files.
      n_output=nstart-1   ! the starting output file - 1
100   n_output=n_output+1

c compute the MORPH tiepoint locations by weighting the points
c linearly with the distance of the morph from left & right.
      wt_right=float(n_output)/float(nods+1)   ! weight of right image
      wt_left=1.0 - wt_right                   ! weight of left  image
      do i=1,npts
         morph_line(i)=wt_left * left_line(i) + wt_right * right_line(i)
         morph_samp(i)=wt_left * left_samp(i) + wt_right * right_samp(i)
      enddo

c determine the limits on the range of the morph points.
      left=morph_samp(1)
      right=left
      top=morph_line(1)
      bottom=top
      do i=2,npts
         if(left.gt.morph_samp(i)) left=morph_samp(i)
         if(right.lt.morph_samp(i)) right=morph_samp(i)
         if(top.gt.morph_line(i)) top=morph_line(i)
         if(bottom.lt.morph_line(i)) bottom=morph_line(i)
      enddo

c build output filename
      write(fname,20) n_output
20    format(i8)
      do i=1,8
        if(name2(i).ne.' ') then
          num_start=i
          goto 30
        endif
      enddo
30    name_len=name_length
      do i=num_start,8
        name_len=name_len + 1
        name1(name_len)=name2(i)
      enddo
      name1(name_len+1)='.'
      name1(name_len+2)='i'
      name1(name_len+3)='m'
      name1(name_len+4)='g'

c open the next output
      call xvunit(outunit,'NEW',n_output,status,
     &            'U_NAME',filename,' ')
      call xvopen(outunit,status,'U_FORMAT','HALF',
     +            'OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS',' ')

c     loop on the rows of output boxes inc apart
      do lbox=1,nl+inc,inc
         if(lbox.gt.nl) goto 1000
         gridl=lbox + inc/2.0 -0.5    ! line box center

c        loop on the columns of output boxes inc apart
         do jcol=1,nboxes
            jbox=(jcol-1)*inc+1
            grids=jbox + inc/2.0 -0.5    ! sample box center

c           determine the 4 nearest points, one in each quadrant.
            d_topleft=1.0e+10
            d_topright=1.0e+10
            d_botleft=1.0e+10
            d_botright=1.0e+10
            i_topleft=0
            i_topright=0
            i_botleft=0
            i_botright=0
            do i=1,npts
               if(morph_line(i).lt.gridl)then
                  if(morph_samp(i).lt.grids)then  ! upper left
                     d=gridl-morph_line(i) + grids-morph_samp(i)
                     if(d.lt.d_topleft)then
                        d_topleft=d
                        i_topleft=i
                     endif
                  else                            ! upper right
                     d=gridl-morph_line(i) + morph_samp(i)-grids
                     if(d.lt.d_topright)then
                        d_topright=d
                        i_topright=i
                     endif
                  endif
               else                               
                  if(morph_samp(i).lt.grids)then  ! lower left
                     d=morph_line(i)-gridl + grids-morph_samp(i)
                     if(d.lt.d_botleft)then
                        d_botleft=d
                        i_botleft=i
                     endif
                  else                            ! lower right
                     d=morph_line(i)-gridl + morph_samp(i)-grids
                     if(d.lt.d_botright)then
                        d_botright=d
                        i_botright=i
                     endif
                  endif
               endif
            enddo

c           PERFORM FITTING STEP. 

            if(nptsfit.gt.4) goto 200

c           perform exact fit using 4 points only, no weighting.

c           skip column if all 4 quadrants dont have a point
            if(i_topleft.eq.0.or.i_topright.eq.0.or.
     +         i_botleft.eq.0.or.i_botright.eq.0)then
               flag(jcol)=0
               goto 80
            endif

            a(1,1)=dble(morph_line(i_topleft))*
     +             dble(morph_samp(i_topleft))
            a(1,2)=dble(morph_line(i_topleft))
            a(1,3)=dble(morph_samp(i_topleft))
            a(1,4)=1.d0
            a(2,1)=dble(morph_line(i_topright))*
     +             dble(morph_samp(i_topright))
            a(2,2)=dble(morph_line(i_topright))
            a(2,3)=dble(morph_samp(i_topright))
            a(2,4)=1.d0
            a(3,1)=dble(morph_line(i_botleft))*
     +             dble(morph_samp(i_botleft))
            a(3,2)=dble(morph_line(i_botleft))
            a(3,3)=dble(morph_samp(i_botleft))
            a(3,4)=1.d0
            a(4,1)=dble(morph_line(i_botright))*
     +             dble(morph_samp(i_botright))
            a(4,2)=dble(morph_line(i_botright))
            a(4,3)=dble(morph_samp(i_botright))
            a(4,4)=1.d0
            call mve(8,16,a,aa,1,1) ! save a

c           to left image line
            l_coefy(1,jcol)=left_line(i_topleft)
            l_coefy(2,jcol)=left_line(i_topright)
            l_coefy(3,jcol)=left_line(i_botleft)
            l_coefy(4,jcol)=left_line(i_botright)
            call dsimq(a,l_coefy(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif

c           to left image sample
            call mve(8,16,aa,a,1,1) ! restore a
            l_coefx(1,jcol)=left_samp(i_topleft)
            l_coefx(2,jcol)=left_samp(i_topright)
            l_coefx(3,jcol)=left_samp(i_botleft)
            l_coefx(4,jcol)=left_samp(i_botright)
            call dsimq(a,l_coefx(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage ('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif
            
c           to right image line
            call mve(8,16,aa,a,1,1) ! restore a
            r_coefy(1,jcol)=right_line(i_topleft)
            r_coefy(2,jcol)=right_line(i_topright)
            r_coefy(3,jcol)=right_line(i_botleft)
            r_coefy(4,jcol)=right_line(i_botright)
            call dsimq(a,r_coefy(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage ('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif

c           to right image sample
            call mve(8,16,aa,a,1,1) ! restore a
            r_coefx(1,jcol)=right_samp(i_topleft)
            r_coefx(2,jcol)=right_samp(i_topright)
            r_coefx(3,jcol)=right_samp(i_botleft)
            r_coefx(4,jcol)=right_samp(i_botright)
            call dsimq(a,r_coefx(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage ('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif
80          continue            ! abort fit for a column
            goto 300

c           Least squares fit using from 5 to 20 nearest points.
200         continue

c           move 4 quadrant points into located() buffer
            n=0
            if(i_topleft.gt.0)then
              n=n+1
              located(n)=i_topleft
            endif
            if(i_topright.gt.0)then
              n=n+1
              located(n)=i_topright
            endif
            if(i_botleft.gt.0)then
              n=n+1
              located(n)=i_botleft
            endif
            if(i_botright.gt.0)then
              n=n+1
              located(n)=i_botright
            endif

c           locate additional points as the nearest ones remaining
            do i=n+1,nptsfit
              d=1.0e+20
              do j=1,npts
                do k=1,i-1   ! avoid points already located
                  if(located(k).eq.j) goto 210
                enddo
                dist=(gridl-morph_line(j))**2+(grids-morph_samp(j))**2
                if(dist.lt.d) then
                   d=dist
                   jj=j
                endif
210             continue
              enddo
              located(i)=jj
            enddo

c           compute weights for points inversely with distance
            do i=1,nptsfit
              dist=sqrt((gridl-morph_line(located(i)))**2 +
     +                  (grids-morph_samp(located(i)))**2 )
              weights(i)=inc/(dist+1.0)
            enddo

c           fit for left image line direction.
            do i=1,nptsfit
              c(i,1)=dble(morph_line(located(i))) *
     +               dble(morph_samp(located(i)))
              c(i,2)=dble(morph_line(located(i)))
              c(i,3)=dble(morph_samp(located(i)))
              c(i,4)=1.d0
              cl(i)=left_line(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,l_coefy(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ') 
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif

c           fit for left image sample direction.
            do i=1,nptsfit
              cl(i)=left_samp(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,l_coefx(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ')
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif

c           fit for right image line direction.
            do i=1,nptsfit
              cl(i)=right_line(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,r_coefy(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ')
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif

c           fit for right image sample direction.
            do i=1,nptsfit
              cl(i)=right_samp(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,r_coefx(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ')
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif
220         continue

300         continue

c END OF FITTING

         enddo    ! end of column loop

c        write out a block of INC lines
         do l=lbox,lbox+inc-1
            if(l.gt.nl) goto 1000
            rl=l
            k=0
            do jcol=1,nboxes
               if(flag(jcol).gt.0)then ! have good polynomial fit
                 grids=(jcol-1)*inc +inc/2.0 +0.5
                 do i=1,inc
900                 k=k+1
                    rk=k

c                   compute the x,y position in the left image.
                    y=l_coefy(1,jcol)*rl*rk+l_coefy(2,jcol)*rl+
     +                l_coefy(3,jcol)*rk+l_coefy(4,jcol)
                    x=l_coefx(1,jcol)*rl*rk+l_coefx(2,jcol)*rl+
     +                l_coefx(3,jcol)*rk+l_coefx(4,jcol)

c                   compute same transform on adjacent fit and weight
c                   the results for smoothness.
                    if(rk.lt.grids.and.jcol.gt.1)then
                      yy=l_coefy(1,jcol-1)*rl*rk+l_coefy(2,jcol-1)*rl+
     +                   l_coefy(3,jcol-1)*rk+l_coefy(4,jcol-1)
                      xx=l_coefx(1,jcol-1)*rl*rk+l_coefx(2,jcol-1)*rl+
     +                   l_coefx(3,jcol-1)*rk+l_coefx(4,jcol-1)
                      d2=(grids-rk)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif
                    if(rk.ge.grids.and.jcol.lt.nboxes)then
                      yy=l_coefy(1,jcol+1)*rl*rk+l_coefy(2,jcol+1)*rl+
     +                   l_coefy(3,jcol+1)*rk+l_coefy(4,jcol+1)
                      xx=l_coefx(1,jcol+1)*rl*rk+l_coefx(2,jcol+1)*rl+
     +                   l_coefx(3,jcol+1)*rk+l_coefx(4,jcol+1)
                      d2=(rk-grids)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif

c                   interpolate the DN value in the left image.
                    n=x
                    m=y
                    if(n.lt.1.or.n.ge.np.or.m.lt.1.or.m.ge.nl)then
                       dnout(k)=0
                       goto 90
                    endif
                    dx=x-n
                    dy=y-m
                    dntop=dnleft(n,m)*(1.-dx)+dnleft(n+1,m)*dx 
                    dnbot=dnleft(n,m+1)*(1.-dx)+dnleft(n+1,m+1)*dx 
                    dnl=dntop*(1.-dy)+dnbot*dy

c                   compute the x,y position in the right image.
                    y=r_coefy(1,jcol)*rl*rk+r_coefy(2,jcol)*rl+
     +                r_coefy(3,jcol)*rk+r_coefy(4,jcol)
                    x=r_coefx(1,jcol)*rl*rk+r_coefx(2,jcol)*rl+
     +                r_coefx(3,jcol)*rk+r_coefx(4,jcol)

c                   compute same transform on adjacent fit and weight
c                   the results for smoothness.
                    if(rk.lt.grids.and.jcol.gt.1)then
                      yy=r_coefy(1,jcol-1)*rl*rk+r_coefy(2,jcol-1)*rl+
     +                   r_coefy(3,jcol-1)*rk+r_coefy(4,jcol-1)
                      xx=r_coefx(1,jcol-1)*rl*rk+r_coefx(2,jcol-1)*rl+
     +                   r_coefx(3,jcol-1)*rk+r_coefx(4,jcol-1)
                      d2=(grids-rk)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif
                    if(rk.ge.grids.and.jcol.lt.nboxes)then
                      yy=r_coefy(1,jcol+1)*rl*rk+r_coefy(2,jcol+1)*rl+
     +                   r_coefy(3,jcol+1)*rk+r_coefy(4,jcol+1)
                      xx=r_coefx(1,jcol+1)*rl*rk+r_coefx(2,jcol+1)*rl+
     +                   r_coefx(3,jcol+1)*rk+r_coefx(4,jcol+1)
                      d2=(rk-grids)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif

c                   interpolate the DN value in the right image.
                    n=x
                    m=y
                    if(n.lt.1.or.n.ge.np.or.m.lt.1.or.m.ge.nl)then
                       dnout(k)=0
                       goto 90
                    endif
                    dx=x-n
                    dy=y-m
                    dntop=dnright(n,m)*(1.-dx)+dnright(n+1,m)*dx    
                    dnbot=dnright(n,m+1)*(1.-dx)+dnright(n+1,m+1)*dx
                    dnr=dntop*(1.-dy)+dnbot*dy

                    if(leftonly)then
                      dnout(k)=nint(dnl)
                    else
                      dnout(k)=nint(dnl*wt_left+dnr*wt_right)
                    endif

90                  continue        ! abort pixel, set dn to zero
                    if(jcol.eq.nboxes.and.k.lt.np) goto 900  ! endofline
                 enddo
              else                  ! abort column of pixels
                 do i=1,inc
                    k=k+1
                    dnout(k)=0
                 enddo
              endif
            enddo

c           pad end of line if columns stop short of right edge
            if(k.lt.np)then
               do i=k+1,np
                  dnout(i)=0
               enddo
            endif

            call xvwrit(outunit,dnout,status,'LINE',l,' ')
         enddo

      enddo       ! end of row loop

c write blank lines if last row doesen't extend to end of image
c      if(lbox+inc-1.lt.nl)then
c         call mve(2,np,0,dnout,0,1)
c         do i=lbox+inc,nl
c            call xvwrit(outunit,dnout,status,'LINE',l,' ')
c         enddo
c      endif

1000  call xvclose(outunit,status,'CLOS_ACT','FREE',' ')  ! close output

      if(n_output.lt.nods) goto 100   ! go make another picture
      return
      end

c**************************************************************

      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      implicit none

! Passed Parameters
      REAL*8  C(20,4), CL(20), wts(20), X1(4)
      integer ind, NE, NU

! Local Parameters
      REAL*8  A(4,4), AL(4), R(4,4), RL(4), Q(4,4),
     &        X(4), SUM
      integer J, I, K, NUM, NUP, l, ix, ixi

      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).eq.0.d0)goto 999
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).eq.0.d0)goto 999
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).eq.0.d0)goto 999
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      RETURN
999   ind=1
      return
      END


C*********************************************************************

      SUBROUTINE DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS

      implicit none

! Passed parameters
      real*8  A(1),B(1)
      integer N, KS

! Local Parameters
      real*8  biga,save,tol
      integer JJ, IMAX, I, J, IT, IJ, I1, I2, K, IX, JX, JY, 
     &        IXJ, IQS, IXJX, JJX, NY, IA, IB, IC

C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

