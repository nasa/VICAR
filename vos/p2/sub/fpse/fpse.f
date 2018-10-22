C 2 January 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting)
C27 January 1995 ... SP      Changed the name VADD to VADDEM because of
C                            name conflict with SPICE.  

c dummy routines for FPS emulation package

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apinit
      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apwd
      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apwr
      return
      end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apget( buf, offs, n, type)

c FPS emulation routine.
c Move N elements from APMEM starting at specified offset OFFS to array BUF,
c reformatting as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
c	       1: in = I*4, out = I*2 ( then use VFIX)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c
	implicit integer(a-y)

        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	integer*4 buf(1)

        integer   F2HBUF(12)            ! Full to half translation
        integer   status

        call xvtrans_set (F2HBUF,'FULL','HALF',status)
        if (status .ne. 1) then
           call mabend ('APGET: Full to HALF translation error',' ')
        endif

	if (type .eq. 0) then               ! type 0: in I*4 /out = I*4
            call MVE ( 4, N, zbuf(offs+1), buf, 1, 1)
	ELSE if (type.eq.1) then          ! type 1: in I*4 /out = I*2
	    call XVTRANS (F2HBUF,zbuf(offs+1), buf, n)
	ELSE if (type.eq.2 .or. type .eq. 3) then ! type 2or3: in R*4 /out = R*4
	    call MVE ( 7, n, zbuf(offs+1), buf, 1, 1)
        ENDIF

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine apput( buf, offs, n, type)

c FPS emulation routine.
c Move N elements from array BUF to APMEM starting at specified offset OFFS,
c reformatting as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VUP8, VFLT32)
c	       1: in = I*2, out = I*4 ( then use VFLT)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c (This implies that only for TYPE=1 do we need to do any work)

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	integer*4 buf(1)

	if (type.eq.0) then
            call mve ( 4,n,buf,zbuf(offs+1),1,1)
	else if (type.eq.1) then
 	    call mve( 6, n, buf, zbuf(offs+1),1,1)
	elseif (type.eq.2 .or. type .eq. 3) then
 	    call mve( 7, n, buf, zbuf(offs+1),1,1)
	endif

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine conv( a, i, b, j, c, k, n1, n2)

c FPS emulation routine.
c Correlate or convolve arrays A and B to obtain C:
c	C(mK) = SUM (A((m+q)I) * B(qJ),	from q = 1 to N2, for m = 1,...,N1
c Note that A, B, and C are offsets in APMEM.
c If I & J have the same sign, the operation is correlation, else it
c is convolution.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m = 0,n1-1
	  zsum = 0.
	  do q = 0, n2-1
	    zsum = zsum + zbuf(a+1+(m+q)*i) * zbuf(b+1+q*j)
	  enddo
	  zbuf(c+1+m*k) = zsum
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine hist( a, i, c, n, nb, amax, amin)

c FPS emulation routine.
c Generates the histogram of an array starting at AP menory offset A, 
c increment = I, with limits AMAX, AMIN, and puts the results in the 
c AP memory array at offset C.

c 12-sep-85 ...REA... made binwidth,diff real*4

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	real binwidth,diff

c (don't inititalize C because FPS routine doesn't)

    	diff = zbuf(amax+1)-zbuf(amin+1)
	if (diff.eq.0.) then
	  call xvmessage ('** HIST: BAD LIMITS **',' ')
	  return
	endif
	binwidth = nb/diff
	do m = 0,n-1
	  if (zbuf(a+1+m*i).lt.zbuf(amin+1)) then
	    j = 1
	  elseif (zbuf(a+1+m*i).ge.zbuf(amax+1)) then
	    j = nb
	  else
	    j = binwidth*(zbuf(a+1+m*i)-zbuf(amin+1)) + 1.0
	  endif
	  zbuf(c+j) = zbuf(c+j)+1.0
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine mmul32( a, i, b, j, c, k, mc, nc, na)

c FPS emulation routine.
c Matrix multiply arrays A and B to obtain C

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do p = 0,mc-1
	  do q = 0,nc-1
	    zsum = 0.
	    do r = 0, na-1
	      zsum = zsum + zbuf(a+1+(p+r*mc)*i) * zbuf(b+1+(r+q*na)*j) 
	    enddo
	    zbuf(c+1+(p+q*mc)*k) = zsum
	  enddo
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vaddem ( a, i, b, j, c, k, n)

c FPS emulation routine.
c Add arrays A and B to obtain C:
c	C(mK) = A(mI) + B(mJ),	m = 0,...,N-1
c Note that A, B, and C are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) + zbuf(b+1+(m-1)*j)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vclip (a, i, b, c, d, k, n)

c FPS emulation routine.
c Move array A to D, clipping it to the range (B - C):
c	D(mK) = B	if A(mI) < B		m = 0,...,N-1
c		A(mI) 	if B <= A(mI) < C
c		C	if C <= A(mI)
c Note that A, B, C, and D are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	zlo = zbuf(b+1)
	zhi = zbuf(c+1)
	do m =1,n
	  z1 = zbuf(a+1+(m-1)*i) 
	  if (z1.lt.zlo) then
	    z1 = zlo
	  elseif (z1.gt.zhi) then
	    z1 = zhi
	  endif
	  zbuf(d+1+(m-1)*k) = z1
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vclr (c, k, n)

c FPS emulation routine.
c  Clears an array starting at C, increment = K.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m = 0,n-1
	  zbuf(c+1+m*k) = 0.
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vfix (a, i, c, k, n)

c FPS emulation routine.
c Convert elements from Floating-point to Integer:
c	C(mK) = FIX( A(mI)),	m = 0,...,N-1
c Note that C and A are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	integer*4 buf
        integer   R2FBUF(12)            ! Real to Full translation

	entry vfix32( a, i, c, k, n)

        call xvtrans_set (R2FBUF,'REAL','FULL',STATUS)
        if (status .ne. 1) then
           call mabend ('VFIX: Real to Full translation error',' ')
        endif

	do m =1,n
	  call XVTRANS (R2FBUF,zbuf(a+1+(m-1)*i), buf, 1)
	  call mve(1,4, buf, zbuf(c+1+(m-1)*k), 1,1)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vflt( a, i, c, k, n)

c FPS emulation routine.
c Convert elements from Integer to Floating-point:
c	C(mK) = FLOAT( A(mI)),	m = 0,...,N-1
c Note that C and A are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

        integer   F2RBUF(12)            ! Full to Real translation

	entry vflt32( a, i, c, k, n)

        call xvtrans_set (F2RBUF,'FULL','REAL',STATUS)
        if (status .ne. 1) then
           call mabend ('VFLT: Full to Real translation error',' ')
        endif

        do m = 1,n
	   call XVTRANS (F2RBUF,zbuf(a+1+(m-1)*i),zbuf(c+1+(m-1)*k),1)
        end do

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vmov( a, i, c, k, n)

c FPS emulation routine.
c Move array A to C:
c	C(mK) = A(mI) 
c Note that A and C are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) 
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vpk8( a, i, c, k, n)

c FPS emulation routine.
c  Packs lo-order byte (unsigned) from 4 words of A into each word of C.

	implicit integer(a-y)
        include 'fortport'

        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	byte      bbuf(4)
        integer   IBUF
        integer   R2BBUF(12)           
        integer   R2FBUF(12)           
        integer   F2BBUF(12)           

        call xvtrans_set (R2BBUF,'REAL','BYTE',STATUS)
        if (status .ne. 1) then
           call mabend ('VPK8: Real to Byte translation error',' ')
        endif

        call xvtrans_set (R2FBUF,'REAL','FULL',STATUS)
        if (status .ne. 1) then
           call mabend ('VPK8: Real to Full translation error',' ')
        endif

        call xvtrans_set (F2BBUF,'FULL','BYTE',STATUS)
        if (status .ne. 1) then
           call mabend ('VPK8: Full to Byte translation error',' ')
        endif

	do m = 0,n-1
	  do j=0,3

            ! Move one word of zbuf 
	    call XVTRANS (R2BBUF,zbuf(a+1+(4*m+j)*i), bbuf(j+1), 1)

	    call XVTRANS (R2FBUF,zbuf(a+1+(4*m+j)*i), IBUF, 1)

	    call XVTRANS (F2BBUF,IBUF,BBUF(J+1), 1)

	  enddo
	  call mve( 1,4,bbuf, zbuf(c+1+m*k), 1,1)
	enddo
	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vsadd( a, i, b, c, k, n)

c FPS emulation routine.
c Add array A and scalar B to obtain C:
c	C(mK) = A(mI) + B,	m = 0,...,N-1
c Note that A, B, and C are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
        common/apmem/ zbuf	        ! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) + zbuf(b+1)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vsmul( a, i, b, c, k, n)

c FPS emulation routine.
c Multiply array A and scalar B to obtain C:
c	C(mK) = A(mI) * B,	m = 0,...,N-1
c Note that A, B, and C are offsets in APMEM.
       
	implicit integer(a-y)
        real zbuf (1000000)
        common/apmem/ zbuf	        ! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) * zbuf(b+1)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vup8( a, i, c, k, n)

c FPS emulation routine.
c  Unpacks 4 bytes (unsigned) from each byte of single word A into 4 words of C.

	implicit integer(a-y)
        include 'fortport'
        real      zbuf (1000000)
        common/apmem/ zbuf	        ! mock AP memory

        ! Call C function to perform unpack; Located in C bridge package
        call zfvup8( zbuf, a, i, c, k, n)

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
