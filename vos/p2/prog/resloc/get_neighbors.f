ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors surrounding each of the 202 reseau marks.
c
      subroutine get_neighbors(nbr)
      implicit none
c Output...
      integer nbr(8,202)	!8 neighbors of each reseau mark

      integer k,i
c      character*80 msg

      call get_nbrs_corners(nbr)
      call get_nbrs_frame(nbr)
      call get_nbrs_interior(nbr)
      call get_nbrs_202(nbr)
c      do k=1,202
c      write(msg,101) k,(nbr(i,k),i=1,8)
c  101 format('k=',i3,' nbrs=',8i4)
c      call xvmessage(msg,0)
c      enddo
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of the reseau marks in the corners.

      subroutine get_nbrs_corners(nbr)
      implicit none
c Filled...
      integer nbr(8,202)

c Local variables...
      integer i

      integer n1(8)/13,2,24,25,14,36,0,0/	!neighbors of res mark 1
      integer n13(8)/1,2,24,25,0,14,36,37/	!neighbors of res mark 13
      integer n25(8)/13,14,36,37,24,26,2,48/
      integer n37(8)/25,26,48,0,36,38,14,52/

      integer n12(8)/23,11,35,34,22,46,0,0/	!0 if neighbor does not exist
      integer n23(8)/11,12,34,35,22,0,46,45/
      integer n34(8)/22,23,45,46,33,35,11,49/
      integer n45(8)/33,34,0,49,44,46,22,60/

      integer n157(8)/153,168,169,156,158,142,180,0/
      integer n168(8)/156,157,179,180,167,169,153,191/
      integer n179(8)/167,168,190,191,156,180,157,0/
      integer n190(8)/179,167,191,168,0,180,156,0/

      integer n165(8)/154,176,177,164,166,150,188,0/
      integer n177(8)/165,166,188,189,176,178,154,200/
      integer n189(8)/177,178,200,201,188,0,166,165/
      integer n201(8)/189,178,200,177,166,188,0,0/

c Upper-left corner...
      do i=1,8
         nbr(i,1) = n1(i)
         nbr(i,13) = n13(i)
         nbr(i,25) = n25(i)
         nbr(i,37) = n37(i)
      enddo

c Upper-right corner...
      do i=1,8
         nbr(i,12) = n12(i)
         nbr(i,23) = n23(i)
         nbr(i,34) = n34(i)
         nbr(i,45) = n45(i)
      enddo

c Lower-left corner...
      do i=1,8
         nbr(i,157) = n157(i)
         nbr(i,168) = n168(i)
         nbr(i,179) = n179(i)
         nbr(i,190) = n190(i)
      enddo

c Lower-right corner...
      do i=1,8
         nbr(i,165) = n165(i)
         nbr(i,177) = n177(i)
         nbr(i,189) = n189(i)
         nbr(i,201) = n201(i)
      enddo 

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors for the reseau marks making up the frame.
c
      subroutine get_nbrs_frame(nbr)
      implicit none
c Ouput...
      integer nbr(8,202)

c Local variables...

c Neighbors of reseau mark 2,14,26,etc...

      integer n2(8)/13,14,1,3,25,0,0,0/		!0 if neighbor does not exist
      integer n14(8)/2,3,25,26,13,15,0,37/
      integer n26(8)/14,15,37,38,25,27,3,0/
      integer n38(8)/26,27,0,0,37,39,15,53/

      integer n158(8)/169,170,157,159,143,181,0,0/
      integer n169(8)/157,158,180,181,168,170,192,0/
      integer n180(8)/168,169,191,192,179,181,157,0/
      integer n191(8)/179,180,190,192,168,0,0,0/

      integer n24(8)/13,36,1,47,25,0,0,0/ 
      integer n36(8)/24,25,47,48,13,51,37,0/
      integer n48(8)/36,37,51,52,25,63,47,0/
      integer n52(8)/48,63,51,53,37,67,0,0/

      integer n60(8)/49,64,59,61,45,75,0,0/
      integer n49(8)/45,46,60,61,34,64,50,0/
      integer n46(8)/34,35,49,50,23,61,45,0/
      integer n35(8)/23,46,12,50,34,0,0,0/

c Get neighbors of top frame...
      call get_nbrs_row(2,11,n2,nbr)	!row starts at 2 and ends at 11
      call get_nbrs_row(14,22,n14,nbr)
      call get_nbrs_row(26,33,n26,nbr)
      call get_nbrs_row(38,44,n38,nbr)
      nbr(4,43) = 202			!202 is a near neighbor of 43 and 44
      nbr(3,44) = 202
      nbr(8,32) = 202			!202 is a distant neighbor of 32

c Get nbrs of bottom frame...
      call get_nbrs_row(158,164,n158,nbr)
      call get_nbrs_row(169,176,n169,nbr)
      call get_nbrs_row(180,188,n180,nbr)
      call get_nbrs_row(191,200,n191,nbr)

c Get neighbors of left frame...
      call get_nbrs_col(24,167,n24,nbr)
      call get_nbrs_col(36,156,n36,nbr)
      call get_nbrs_col(48,153,n48,nbr)
      call get_nbrs_col(52,142,n52,nbr)

c Get neighbors of right frame...
      call get_nbrs_col(60,150,n60,nbr)
      call get_nbrs_col(49,154,n49,nbr)
      call get_nbrs_col(46,166,n46,nbr)
      call get_nbrs_col(35,178,n35,nbr)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of the interior reseau marks.

      subroutine get_nbrs_interior(nbr)
      implicit none
c Output...
      integer nbr(8,202)	!only the nbrs of the interior marks are filled

c Local variables...
      integer i
      integer k		!reseau mark index
      integer nk(8)	!neighbors of k

c The interior consists of the 7x7 grid of reseau marks at the center of the
c image.  To fill in the neighbors of all 49 marks, we need only know the
c neighbor of mark 53:
  
      integer n53(8)/0,0,0,0,52,54,38,68/	!interior has no near neighbors

c Initialize the neighbors of k...

      do i=1,8
         nk(i) = n53(i)
      enddo

c Fill the rows starting at 53,68,83,98,113,128, and 143...

      do k=53,143,15
         call get_nbrs_row(k,k+6,nk,nbr)
         do i=5,8
            nk(i) = nk(i) + 15
         enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of 202.

      subroutine get_nbrs_202(nbr)
      implicit none
c Output...
      integer nbr(8,202)

c Local variables...
      integer i

      integer n202(8)/43,44,58,59,0,0,32,0/

      do i=1,8
         nbr(i,202) = n202(i)
      enddo

c Add 202 to list of its neighbors
      nbr(8,32) = 202                   !distant neighbor...
      nbr(4,43) = 202                   !near neighbors...
      nbr(3,44) = 202
      nbr(2,58) = 202
      nbr(1,59) = 202
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of the row starting with sk and ending with ek.

      subroutine get_nbrs_row(sk,ek,nsk,nbrs)
      implicit none
c Inputs...
      integer sk,ek	!starting k, ending k of row
      integer nsk(8)	!neighbors of sk
c Output...
      integer nbrs(8,202)

c Local variables...
      integer i,j
      integer k		!current mark
      integer nk(8)	!neighbors of k

      do i=1,8
         nk(i) = nsk(i)
      enddo

      do 20 k=sk,ek
      do i=1,8
         nbrs(i,k) = nk(i)
         if (nk(i).ne.0) nk(i)=nk(i)+1
      enddo
   20 continue
       
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get neighbors of column that begins with sk and ends with ek.

      subroutine get_nbrs_col(sk,ek,nsk,nbrs)
      implicit none
c Inputs...
      integer sk,ek	!starting k and ending k of column
      integer nsk(8)	!neighbors of sk
c Output...
      integer nbrs(8,202)

c Local variables...
      integer i
      integer k		!current mark on column
      integer nk(8)	!neighbors of k

      do i=1,8
         nk(i) = nsk(i)
      enddo

      k = sk

   10 continue
      do i=1,8
         nbrs(i,k) = nk(i)
      enddo

      if (k.ge.ek) return

      do i=1,8
         if (nk(i).ne.0) then
            if (nk(i).lt.34 .or. nk(i).gt.153) then
               nk(i) = nk(i) + 23
            else
               nk(i) = nk(i) + 15
            endif
         endif
      enddo
      if (k.lt.34 .or. k.gt.153) then
         k = k + 23
      else
         k = k + 15
      endif
      goto 10

      end
