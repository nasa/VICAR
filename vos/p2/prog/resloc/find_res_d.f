cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find the reseau marks in D.

      subroutine find_res_d(D,lab,nom,res)
      implicit none
c Inputs...
      byte D(800,800)		!Voyager image
      character*7200 lab        !label of image D
      real*4 nom(2,202)		!(lnom,snom)=nominal coordinates for camera

c Output...
      real*4 res(2,202)		!(line,samp) coordinates reseau marks

c Local variables...
      real*4 dc(202)		!estimate DN contribution of the camera
      real*4 lnom(202),snom(202)!lnom(i)=nom(1,i) and snom(i)=nom(2,i)
      integer lk,sk		!nominals truncated to integers
      real*4 e,f		!mean offsets from nominals

c Given the shape template A, we compute the correlation coefficient rho at
c every pixel inside the search area.
      integer*2 A(9,9)		!reseau shape template
      integer*2 W(9,9)		!a window in the search area
      integer ia,ja		!half-window dimensions of A
      real*4 R(25,25)		!rho for each pixel in (max) 25x25 search area.
      integer ir,jr		!half-window dimensions of R 
      integer rsw(9)		!sums of rows of W

c For convenience, we copy all the pixels needed to conduct the search
c into a window B.
      integer*2 B(33,33)	!window centered at truncated nominals (lk,sk)
      integer ib,jb		!half-window dimensions of B
      integer csw(33)		!column sums along the windows of B

c We save the following measurements for the 3 highest peaks in each R:
      real*4 rho(3,202)		!correlation coefficient
      real*4 dn(3,202)		!darkest DN in W (near center of window)
      real*4 dedge(3,202)	!mean DN of pixels along edge of window
      real*4 l(3,202),s(3,202)	!(line,sample) coordinates of the 3 peaks
      real*4 el(3,202),es(3,202)!estimated (line,sample) coordinates of peaks
      real*4 eps(3,202)		!distance from (l,s) to (el,es)
      real*4 g(3,202)		!normalized darkness measure
      real*4 h(3,202)		!normalized distance measure
      real*4 q(3,202)		!composite match quality

c We choose the peak with the highest q and copy its measurements...
      integer choice(202)	!index of chosen candidate (1,2 or 3).
      real*4 crho(202)		!correlation coefficient
      real*4 cdn(202)		!minimum DN
      real*4 cdedge(202)	!mean DN of window's perimeter
      real*4 cel(202),ces(202)	!estimated coordinates
      real*4 cl(202),cs(202)	!located coordinates
      real*4 ceps(202)		!distance from (cel,ces) to (cl,cs)
      real*4 cg(202)		!normalized darkness measure
      real*4 ch(202)		!normalized distance measure
      real*4 cq(202)		!composite match quality

c Miscellaneous variables...
      integer i,k,n,count
      integer*4 di(3),dj(3)	!integer offsets from nominals 
      real*4 dx(3),dy(3)	!fractional offsets from nominals
      real*4 maxeps		!maximum displacement
      integer*4 poe		!planet of encounter (J=5,S=6,U=7,N=8)
      integer tres		!print intermediate values for reseau mark tres
      logical gap		!true if search area contains data gaps
      logical filter		!filter hot edges
      logical noin		!do no interpolation of R
      logical dbug		!print more stuff to aid debugging
      logical xvptst

c Reseau mark data constants...
      integer nbr(8,202),rtype(202)

      call get_neighbors(nbr)	!8 neighbors of each mark
      call get_rtype(rtype)	!type of reseau mark (corner,edge,interior)
      call get_poe(lab,poe)	!data-compression line truncation if poe=6 or 7

c Reformat the nominals to make the problem easier to see....
      do k=1,202
         lnom(k) = nom(1,k)
         snom(k) = nom(2,k)		
      enddo

c Initialize all measurements as invalid...

      do k=1,202
         dc(k) = -999
         do i=1,3
            rho(i,k) = -999
            dn(i,k) = -999
            dedge(i,k) = -999
            l(i,k) = -999
            s(i,k) = -999
            eps(i,k) = -999
            g(i,k) = -999
            h(i,k) = -999
            q(i,k) = -999
         enddo
      enddo

c Set the program parameters...

      filter = .not.xvptst('nofilter')	!hot edge filter
      noin = xvptst('noin')		!no subpixel accuracy
      dbug = xvptst('dbug')		!print diagnostic messages
      call xvp('tres',tres,count)	!print arrays for this test reseau mark
      call get_A_size(ia,ja)		!get half-window dimensions of A
      call compute_A(A,ia,ja)		!compute the shape template
      call get_R_size(ir,jr)		!get half-window dimensions of R
      ib = ir + ia			!compute half-window dimensions of B
      jb = jr + ja

c Main loop...

c For each reseau mark k, k=1,2,...,202, the following is done:
c The correlation coefficient is computed at every pixel in the search area
c and stored in matrix R.  The mark is assumed to be located at one of the
c three highest peaks in R.  For each of these three peaks, we measure its
c darkness and its distance from its expected coordinates.  And from the
c darkness, distance, and our previously computed R values, we select the peak
c where the mark is most likely to be found.

      do 20 k=1,202		
      lk = lnom(k)		!truncate the nominal coordinates
      sk = snom(k)		!into integers (lk,sk).
      call get_window(D,lk,sk,B,ib,jb)	!get window centered at (lk,sk)
      call check_window(B,ib,jb,lk,sk,k,rtype(k),poe,gap) !flag gaps in window
      if (filter) call filter_B(B,ib,jb,lk,sk,rtype(k))	!filter hot edges
      if (k.eq.tres) call print_B(B,ib,jb,k)
      if (gap .or. rtype(k).ne.0) then
         call compute_R_gap(A,ia,ja,B,ib,jb,R,ir,jr)
      else
         call compute_R_detrend(A,ia,ja,B,ib,jb,R,ir,jr,
     &		W,rsw,csw)
      endif
      if (k.eq.tres) call print_R(R,ir,jr)

c     ...Find the 3 best candidates for mark k and compute l, s, dn and
c     ...dedge for these candidates.

      call find_rmax(R,ir,jr,rho(1,k),di,dj)	!return offsets from R(0,0)
      do i=1,3		!loop thru the 3 highest peaks in R
         if (rho(i,k).gt.-1) then	!if rho is a valid measure...
            call interp_max(R,ir,jr,di(i),dj(i),noin,rho(i,k),
     &		dx(i),dy(i))		!compute subpixel offsets
            l(i,k) = lk + dj(i) + dy(i)
            s(i,k) = sk + di(i) + dx(i)
            call get_W(B,ib,jb,W,ia,ja,di(i),dj(i))	!measure the dn
            call compute_dn(W,ia,ja,dn(i,k))		!dn of mark
            call compute_dedge(W,ia,ja,dedge(i,k))	!background dn
            if (k.eq.tres) then
               call ptres5(i,di,dj,dx,dy,k,rho,dn,dedge)
ccc               call detrend_window(B,ib,jb,di(i),dj(i),W,ia,ja,rsw,csw)
               call print_window(W,ia,ja)
            endif
         endif
      enddo
   20 continue

      call compute_offsets(l,s,lnom,snom,rtype,e,f)  !compute e and f
      call compute_eps(l,s,lnom,snom,e,f,tres,eps)
      call compute_dc(l,s,dn,nbr,rtype,dc)

c     ...Choose the candidate with the highest q.
ccc      maxeps = sqrt((ir+abs(e))**2+(jr+abs(f))**2)
      maxeps = ir
      call compute_q(rho,dn,dedge,dc,eps,maxeps,tres,g,h,q)
      call choose_best(q,choice,dbug)

c     ...Simplify things by copying the data for the chosen candidate
      do k=1,202
         i = choice(k)
         crho(k) = rho(i,k)
         cdn(k) = dn(i,k)
         cdedge(k) = dedge(i,k)
         cl(k) = l(i,k) 
         cs(k) = s(i,k)
         ceps(k) = eps(i,k)
         cg(k) = g(i,k)
         ch(k) = h(i,k)
         cq(k) = q(i,k)
      enddo

      if (xvptst('pstats')) call print_stats(cdn,dc,
     &		cdedge,ceps,crho,cg,ch,cq)

c     ...Identify and reject mismatches (by setting cl and cs to -999).
      call reject(cl,cs)	!reject a specific list of reseau marks 
      call check_rho(crho,cl,cs,dbug)	!reject if rho<rthresh
      call check_q(cq,cl,cs,dbug)	!reject if q<qthresh
      call check_corners(ceps,cl,cs)	!reject if wrong mark in corner
      call p_avg_stats(ceps,crho,cq,cl)

c     ...Fill in the (cl,cs) coordinates of all marks rejected or not found
      if (.not.xvptst('nofill'))
     &	 call fill_res(lnom,snom,nbr,rtype,e,f,cl,cs)

      do k=1,202		!copy coordinates to output format
         res(1,k) = cl(k)
         res(2,k) = cs(k)
      enddo

      return
      end
