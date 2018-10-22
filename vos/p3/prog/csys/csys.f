c   program CSYS  --  Change SYStems 

c  convert RA,Dec,Twist from B1950 to J2000 or vice versa

	implicit real*8 (a-h,o-z)
	real*8 cmat(3,3), cmat1(3,3)
	real*8 to1950(3,3)/
     1    0.9999256794956877, -0.0111814832204662, -0.0048590038153592,
     2    0.0111814832391717,  0.9999374848933135, -0.0000271625947142,
     3    0.0048590037723143, -0.0000271702937440,  0.9999881946023742/

	degrad = 180.d0/pi()	! degrees per radian

	print*,' Enter 1 to go to J2000, 2 to go to B1950:'
	read(5,*) mode
	if (mode.lt.1 .or. mode.gt.2) call exit

c  get the pointing:
1	print*,' enter RA (degrees) ... or -999 to quit:'
	read(5,*) ra
	if (ra.lt.-900.) call exit
	print*,' enter Dec (degrees):'
	read(5,*) dec
	print*,' enter Twist (degrees):'
	read(5,*) twt

	ra = ra/degrad		! convert to radians
	dec = dec/degrad
	twt = twt/degrad

	cdec = halfpi()-dec
	call eul2m( twt, cdec, ra, 3, 2, 3, cmat)	! make C-matrix

c  convert C-matrix by column vectors:
	do i=1,3
	  do j=1,3
	    cmat1(i,j) = 0.0
	    do k=1,3
	      if (mode.eq.1) then
		cmat1(i,j) = cmat1(i,j) + cmat(i,k)*to1950(k,j)
	      else
		cmat1(i,j) = cmat1(i,j) + cmat(i,k)*to1950(j,k)
	      endif
	    enddo
	  enddo
	enddo

		! convert back to angles
	call m2eul( cmat1, 3, 2, 3, twt, cdec, ra)
	dec = halfpi()-cdec
	if (ra.lt.0.) ra = ra+2.0*pi()
	if (twt.lt.0.) twt = twt+2.0*pi()

		! and list:
	write(6,1001) ra*degrad, dec*degrad, twt*degrad

	go to 1

1001	format(' RA =', f8.3,'  Dec =', f8.3,'  Twt =', f8.3)

	end
