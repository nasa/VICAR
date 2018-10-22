c ********************************************************************
      subroutine photb(buf,l1,tbrite,l2,bbrite,l3,
     +          cmbuf,l4,total,nsox,ncol)
      implicit none
      integer l1,l2,l3,l4,total
      integer nsox,ncol
      real*4 buf(nsox)			!input and output image line
      real*4 tbrite(ncol),bbrite(ncol)	!correction for top and bottom rows
      integer*2 cmbuf(nsox)		!classification map line
      real gbuf(1)		!****this has to be assigned via stacka*****

      common/units/iunit,ounit,maxdn,fmt
      integer iunit,ounit,maxdn
      character*5 fmt

      common/pf0/linc,sinc,grid,term,limb,const,maxcor,nointerp
      integer linc,sinc,grid
      real term,limb,const,maxcor
      logical*1 nointerp

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      integer irow,icol,nrow
      integer jl,line,samp,nline,nsamp
      integer numcor1,numcor2,numoff,ind,j,num,iprnt
      real ul,ur,ll,lr,vall,valr
      logical xvptst

      character*75 msg1/
     &'   LINE    SAMPLE  LATITUDE LONGITUDE  INC    EMI   PHASE   BRIGHT COLUMN'/

c Photfunc computes the normalized brightnesses for an nrow x ncol grid of
c tiepoints.  Each four adjacent tiepoints form the corners of a linc x sinc
c box.  Therefore, the the image is divided into (nrow-1) x (ncol-1) boxes.
c For the pixels within a box, the brightnesses are interpolated from these
c four corners.

c If any of the corners of the box is off the target or unilluminated, the
c brightnesses are computed exactly at each pixel in the box.

      iprnt = xvptst('ALL')             !print all points
      if (xvptst('PRINT')) iprnt=2	!print illuminated pts
      if (iprnt.gt.0) call xvmessage( msg1,' ')

      if (total.ne.l1+l2+l3+l4) call mabend(' STACKA ERROR')

      nrow = (nlo-2)/linc + 2

      if (xvptst('NOCORREC') .or. ounit.eq.0) then	!if no correction,
         line = sl
         do irow=1,nrow			!print out brights for each box
            call setup(iprnt,gbuf,line,ncol,bbrite,numoff)
            line = line + linc
         enddo
         return                         !and exit.
      endif

      numcor1 = 0		!number of pixels interpolated
      numcor2 = 0		!number of pixels computed exactly
      numoff = 0		!number of grid points off limb/term

c compute normalized photometric function for top row of tiepoints.
      call setup(iprnt,gbuf,sl,ncol,bbrite,numoff)

      do 50 irow=1,nrow-1		!loop thru vertical row of boxes
      j = (irow-1)*linc			!line relative to output image
      line = sl + j			!line relative to input image
      nline = min0(nlo-j,linc)		!number of lines in box

      call mve(7,ncol,bbrite,tbrite,1,1)!copy brights from bottom to top
      call setup(iprnt,gbuf,line+nline,ncol,bbrite,numoff)

      do 45 jl=0,nline-1
      call xvread(iunit,buf,ind,'LINE',line,'SAMP',ss,'NSAMPS',nso,' ')
      if (cmap.gt.0) then		!read line from classication map
         call cm_read(cmap,cmbuf,line,class,ind)
         if (ind.eq.0) then		!if no pixels on line in class
            call mve(7,nso,0.,buf,0,1)	!zero out the output line
            goto 42
         endif
      endif

      do 40 icol=1,ncol-1		!loop thru horizontal row of boxes
      j = (icol-1)*sinc + 1		!samp relative to output image
      samp = ss + j - 1			!samp relative to input image
      nsamp = min0(nso-j+1,sinc)	!pixel length of line segment
      ul=tbrite(icol)
      ur=tbrite(icol+1)
      ll=bbrite(icol)
      lr=bbrite(icol+1)

      num=0		!count # of corners on target and within limb and term
      if (ul.gt.0.0) num=num+1
      if (ll.gt.0.0) num=num+1
      if (ur.gt.0.0) num=num+1
      if (lr.gt.0.0) num=num+1
      if (num.eq.0) then			!if all 4 corners are bad
         call mve(7,nsamp,0.,buf(j),0,1)	!zero out segment
         goto 40
      endif
      if (nointerp) num=0	!force pixel-by-pixel computation
      if (num.eq.4) then	!if all 4 pts are illuminated, interpolate
         vall = jl*(ll-ul)/nline + ul	!brightness at left end of segment
         valr = jl*(lr-ur)/nline + ur	!brightness at right end of segment
         call interp_segment(buf(j),cmbuf(j),vall,valr,nsamp,numcor1)
      else
         call compute_segment(buf(j),cmbuf(j),line,samp,nsamp,numcor2)
      endif
   40 continue		!end horizontal loop thru ncol boxes

   42 call xvwrit(ounit,buf,ind,' ')
   45 line = line + 1	!end line loop thru a row of boxes

   50 continue		!end vertical loop thru nrow boxes

      call prnt(4,1,numcor1,' # of pixels interpolated =.')
      call prnt(4,1,numcor2,' # of pixels computed exactly =.')
      call prnt(4,1,numoff,' Number of grid points flagged=.')
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read line from classification map into cmbuf.  Upon return cmbuf(i)=1
c if the pixel should be corrected, =0 otherwise.
c
      subroutine cm_read(cmap,cmbuf,line,class,ind)
      implicit none
      integer*2 cmbuf(1)
      integer cmap,line,class
      integer ind		!=0 if line is empty, =1 otherwise

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      integer i

      call xvread(cmap,cmbuf,ind,'LINE',line,'SAMP',ss,
     +		'NSAMPS',nso,' ')

      if (class.lt.0) goto 10
      ind = 0		!no class members on this line
      do i=1,nso
         if (cmbuf(i).eq.class) then
            cmbuf(i) = 1
            ind = 1	!class member found
         else
            cmbuf(i) = 0
         endif
      enddo
      return

c here for complement operation
   10 continue
      ind = 0

      do i=1,nso
         if (cmbuf(i).ne.-class .and. cmbuf(i).ne.0) then
            cmbuf(i) = 1
            ind = 1
         else
            cmbuf(i) = 0
         endif
      enddo

      return
      end
