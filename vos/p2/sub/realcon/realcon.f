	subroutine realcon( r, buf, n)

c Convert REAL*4 to N-character output format in BUF.
c  Uses Fixed-point format unless not enough characters available.

	logical neg
        character*(*) buf
        CHARACTER*80  RUNTIME

c  intitialize to "**..*"
	do i=1,n
	  buf(i:i) = '*'
	enddo

	ra = r
	neg = .FALSE.
	if (r.lt. 0.) then
	  neg = .TRUE.
	  ra = -r
	endif
	if (r.eq.0.) ra=1.
	rlog = alog10( ra)

	if (rlog.ge.0) then	! |r| >= 1.0 
	  is = ifix(rlog+1.)	! # digits to left of "."
	  if (neg) is = is+1	! one for "-"
	  id = n-is-1		! # digits to right of "."
	  if (id.lt.0) then	! can't use F format
	    if (rlog.eq.0.) rlog = 1.
	    ie = alog10(rlog)+1	! "E+yy"
	    id = n-4-ie		! # digits between "x." and "E+yy"
	    if (neg) id = id-1	! "-"
	    if (id.lt.0) return
            WRITE (RUNTIME,9020) n, id, ie
            WRITE (BUF(1:n), RUNTIME) r
	  else
            WRITE (RUNTIME,9010) n, id
            WRITE (BUF(1:n), RUNTIME) r
	  endif

	else			! |r| < 1.0
	  id = n-2		! # digits to right of "."
	  if (neg) id = id-1	! one for "-"
	  idm = ifix(-rlog)+1	! minimum # required
	  if (id.lt.idm) then	! can't use F format
	    rlog = -rlog        ! if r=2.e-10, then 10>rlog>9 after this line.
	    if (rlog.eq.0.) then
                rlog = 1.
            else                ! so need to add 1 to make ie come out right.
                if (rlog .ne. aint(rlog) )  rlog = rlog + 1.0
            end if
	    ie = alog10(rlog)+1	! "E-yy"
	    id = n-4-ie		! # digits between "x." and "E-yy"
	    if (neg) id = id-1	! "-"
	    if (id.lt.0) return
            WRITE (RUNTIME,9020) n, id, ie
            WRITE (BUF(1:n), RUNTIME) r
	  else
            WRITE (RUNTIME,9010) n, id
            WRITE (BUF(1:n), RUNTIME) r
	  endif

	endif
	return

9010    FORMAT( '(F',   I3.3, '.', I3.3, ')' )
9020    FORMAT( '(1PE', I3.3, '.', I3.3,'E', I3.3, ')' )
	end
