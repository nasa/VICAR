ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check the window W for data gaps, and replaces these gaps with -998 DN.

c The problem is that data gaps are zero filled and zero is also a
c valid DN value.  Therefore, the meaning of 0 DN is ambiguous.

c Any string of 7 or more consecutive zeroes, either vertical or horizontal, is
c considered a data gap because such an event rarely occurs naturally, and
c is more likely to be due to (1) data dropouts (in the transmission from
c Jupiter to Earth), (2) image data-compression line truncation, or (3)
c image editing.  Therefore, any such string is replaced by -998 DN.
 
c Additionally, windows along the left and right margins are checked for
c data-compression line truncation, no matter how short the gap may be.

      subroutine check_window(W,iw,jw,lk,sk,k,rtype,poe,gap)
      implicit none
c Inputs...
      integer iw,jw
      integer lk,sk		!W(0,0) = D(sk,lk)
      integer k
      integer rtype
      integer poe		!planet-of-encounter (J=5,S=6,U=7,N=8)
c Updated...
      integer*2 W(-iw:iw,-jw:jw)	!gaps set to -998
c Output...
      logical gap			!true if data gap exist
c Local variables...
      integer i,j,ii,jj,loop,dn
      integer n			!running count of strings of 0s
      integer switch		!0=even line, 1=odd line 
      integer line,samp
      integer type		!1=left margin, 2=right margin, 0=neither

      gap = .false.		!initialize the result

c Any horizontal or vertical string of 0s that is at least 7 pixels long is 
c considered a data gap.

c Since images taken at Uranus and Neptune suffer from data-compression
c line truncation, we allow for shorter horizontal strings based on
c the following rules:

c If the mark is on the left margin and the line is even, and if the string of
c 0s starts at sample 1, we assume that this is a result of data compression
c line truncation.

c If the mark is on the right margin and the string ends at sample 800, we
c assume truncation.  (In mode IMK, both odd and even lines may be truncated on
c the right)
 
c This algorithm is not foolproof, but assumes the most probable circumstances.

c To expedite the algorithm, we classify the marks into three types:
c   type=1 if mark is on the left margin
c   type=2 if mark is on the right margin
c   type=0 if neither is true

c type is derived from the more general rtype, where
c rtype=1,2,3,4 for the upper-left, upper-right, lower-left, and lower-
c right corners, =5,6,7,8 for the top, left, bottom, and right margins,
c and 0 everywhere else (the interior).

      type = 0
      if (rtype.eq.1 .or. rtype.eq.3 .or. rtype.eq.6) type=1
      if (rtype.eq.2 .or. rtype.eq.4 .or. rtype.eq.8) type=2

c For pre data compression encounters, we turn the type off
      if (poe.lt.7) type=0


c Search for horizontal strings of 0s...

c Before scanning line-by-line, we set the odd/even switch...
      line = lk - jw		!first line in W
      if (2*(line/2).eq.line) then
         switch = 0		!first line is even
      else
         switch = 1		!first line is odd
      endif


c Scan each line segment of W for strings of 0s... `

      do 30 j=-jw,jw
      n = 0			!reset the string count

      do 20 i=-iw,iw
      if (W(i,j).eq.0) then
         n = n + 1		!add 1 to the string
         goto 20
      endif
      if (n.eq.0) goto 20	!if no string, continue
      if (n.ge.7) goto 16	!if string is long enough, fill in the gap
      if (type.eq.1 .and. switch.eq.0) then
         samp = sk + i - n
         if (samp.eq.1) goto 16		!fill in the gap
      endif
      if (type.eq.2) then 
         samp = sk + i - 1
         if (samp.eq.800) goto 16	!fill in the gap
      endif
      n = 0			!if no gap detected, reset the string
      goto 20			!and continue 
   16 gap = .true.		!here if gap has been detected
      do ii=i-n,i-1
         W(ii,j) = -998		!change the 0s to -998s
      enddo
      n = 0
   20 continue

      if (n.ge.7) then		!flush out the last gap
         gap = .true.
         ii = iw		!the gap ends at the end of W
         do loop=1,n
            W(ii,j) = -998	!change the 0s to -998s
            ii = ii - 1
         enddo
      endif

   30 switch = 1 - switch	!flip the odd/even switch


c Scan every column segment of W for strings of 0s or -998s...

      do 60 i=-iw,iw
      j = -jw
      n = 0

      do 50 j=-jw,jw
      if (W(i,j).eq.0 .or. W(i,j).eq.-998) then
         n = n + 1
         goto 50
      endif
      if (n.eq.0) goto 50
      if (n.ge.7) then
         gap = .true.
         do jj=j-n,j-1
            W(i,jj) = -998
         enddo
      endif
      n = 0
   50 continue
   60 continue

      return
      end
