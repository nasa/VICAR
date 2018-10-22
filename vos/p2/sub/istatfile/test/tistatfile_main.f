C Fortran Test file for ISTATFILE module
C

	include 'VICMAIN_FOR'

	
	subroutine main44
	implicit none
	call c_test
	call fortran_test
	return
	end
	

	subroutine fortran_test
	implicit none
	integer unit, status 
	
	call xvmessage('*** FORTRAN - TEST ***', ' ') 
	call xvunit(unit,'OUT',1,status,' ') 
	if (status .lt. 0) call xvsignal(unit, status, 1) 
	
	call create_stats_file(unit) 
	call read_stats_file(unit) 
	return
	end


	subroutine create_stats_file(unit)
	implicit none
	integer unit

	integer status,def 
	integer nclasses 
	integer nbands 
	character *40 inst
	logical xvptst
	
	call xvp('NCLASS',nclasses,def) 
	call xvp('NBAND',nbands,def) 
	if (xvptst('USE_INST')) then
	   inst = 'Testing' 
	else
	   inst = ' ' 
	endif
	
	call istat_file_open(unit,'write',nclasses,nbands,inst,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 
	
	call write_stats_data(unit,nclasses,nbands) 
	
	call istat_file_close(unit,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 
		
	return
	end


	
	subroutine write_stats_data(unit,nclasses,nbands)
	implicit none
	integer unit
	integer nclasses
	integer nbands

	integer status,i 
	integer row 
	character*8 classname
	real*4 covariance(1000) 
	real*4 means(1000) 
	integer npix 
	
	do i=1,1000
	  covariance(i) = 0
	  means(i) = 0
	enddo
	
	do row=1,nclasses	
		! just make some junk up 
		write(classname, '(A,I3.3)') 'Class',row
		npix = row*row 
		means(1) = float(row)/2.
		covariance(1) = float(row)*2 
		
		! and write it out 
		call istat_record_write(unit,row, classname,npix,
     +				nbands,means,covariance,status) 
		if (status .lt. 0) call istat_signal(unit,status,1) 
	enddo
	return
	end
	
	subroutine read_stats_file(unit)
	implicit none
	integer unit

	integer status 
	integer nbands,nclasses 
	real*4 means(20),covariance(400) 
	character*8 classname
	character*80 message
	integer npix,nb,row,ibis
	integer istat_record_find  !function declaration

	call istat_file_open(unit,'read',0,0,0,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 
	
	! find out how big the file is and allocate memory 
	
	call istat_file_Info(unit, nclasses,nbands,ibis) 

	! print out some records 
	write (message,'(A10,A11,2A7,2A9)')
     +     'ROW','CLASSNAME','NPIX','NB','MEANS','COVAR'
	call xvmessage(message,' ') 
		
	do row=1,nclasses
		call istat_record_read(unit,row, classname,npix,
     +			nb,means,covariance,status) 
		if (status .lt. 0) call istat_signal(unit,status,1) 
		
		write (message,'(I10,A11,2I7,2F9.3)')
     +             row,classname,npix,nb,means(1),covariance(1)
		call xvmessage(message,' ') 
	enddo
	
	call xvmessage('--- Searching for Class003 ---',' ') 	
	row = istat_record_find(unit,'Class003') 
	if (row .lt. 0) then
		message = '  Not Found. ' 
	else 
		write (message, '(A,I)') 'Found at row: ',row
	endif 
	call xvmessage(message,' ')
	
	call istat_record_read(unit,row, classname,npix,
     + 		nb,means,covariance,status) 
	if (status .lt. 0) call istat_signal(unit,status,1) 
	
	write (message,'(I10,A11,2I7,2F9.3)')
     +      row,classname,npix,nb,means(0),covariance(0)
	call xvmessage(message,' ') 
	
	call istat_file_close(unit,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 

	return
	end

