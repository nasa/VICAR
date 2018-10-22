C
C       This routine gives an example of how to use the IBIS-2
C       subroutine package using the FORTRAN bridges.
C
C	Uses: Ported VICAR runtime library,
C       IBIS-2 subroutine library.
C

	include 'VICMAIN_FOR'

	subroutine main44
	! Call the C routine from here so that
	! The file-I/O will work ok
	call main44_c
	return
	end

	subroutine fortran_test
	logical xvptst
	if (xvptst('A4TEST')) then
		call a4test
	else
		call copytest
	endif
	return
	end

c
c  a4test tests the fortran a4 conversion
c   for old IBIS files.
c

	subroutine a4test

	integer i,unit,ibis,status 
	character*4  buf(10) 


	call xvunit(unit,'inp',1,status,0) 
	if (status.ne.1) call xvsignal(unit,status,1) 

	call  ibis_file_open(unit,ibis,'read',0,0,
     +                                ' ',' ',status)
	if (status .ne. 1) call ibis_signal_u(unit,status,1) 

	call  ibis_column_set(ibis,'FORMAT','A4',1,status) 
	if (status .ne. 1) call ibis_signal(ibis,status,1) 
	call  ibis_column_set(ibis,'U_FORMAT','A4',1,status) 
	if (status .ne. 1) call ibis_signal(ibis,status,1) 

	call  ibis_column_read(ibis,buf,1,1,3,status) 
	if (status .ne. 1) call ibis_signal(ibis,status,1) 

	do i=1,3
		call xvmessage(buf(i),' ') 
	enddo

	call ibis_file_close(ibis,' ',status)
	return
	end


C	copytest: Copies one general new IBIS file to another,
C	possibly with a change in organization (row/column).
C
	subroutine copytest
	integer inunit,outunit,status
	integer count
	integer  ibis_out,ibis_in,ibufsize
	parameter (ibufsize=250)
	integer nc, nr, col,colsize
	integer row, row_inc, nrow 
	character*16  org 
	character*6 format(1000) 
	character*6 cfmt
	logical ascii
	character*64 cbuffer(ibufsize*8)
	real*8 buffer(ibufsize*64)
	! The equivalence is just to conserve memory:
	equivalence (buffer,cbuffer)

C   open the input IBIS file, abort on error. The four '0' arguments
C   in the ibis_file_open are only used for creating output files
	
	call xvunit( inunit, 'inp', 1, status, ' ') 
	call ibis_file_open( inunit, ibis_in, 'read', 0,0,' ',' ', status) 
	if (status.ne.1) call ibis_signal_u( inunit, status, 1) 

C  get the # rows & columns and the column-format
C  of the input IBIS file:

	count = ibis_file_get( ibis_in,'nc', nc, 1,1 )
	if (count.lt.0) call ibis_signal( ibis_in, count, 1)
	count = ibis_file_get( ibis_in,'nr', nr, 1,1 )
	if (count.lt.0) call ibis_signal( ibis_in, count, 1)
	count = ibis_file_get( ibis_in,'formats', format, 1, nc )
	if (count.lt.0) call ibis_signal( ibis_in, count, 1)

C open the output file with identical structure. The routine returns
C in variable 'ibis_out' the handle to the IBIS file, which should be used
C in all IBIS file-io operations
	
	call xvpone( 'org', org, 1, 0 ) 
	call xvunit( outunit, 'out', 1,status, ' ') 
	call ibis_file_open( outunit, ibis_out, 'write', nc, 
     +  	             nr, format, org, status ) 
	if (status.ne.1) call ibis_signal_u( outunit, status, 1) 
	
C   copy the column data from input to output

	do col=1,nc
	
		call ibis_column_get( ibis_in, 'format', cfmt, col, status )  
		if (status .ne.1) call ibis_signal( ibis_in, status, 1)
		ascii = (cfmt(1:1).eq.'a'.or.cfmt(1:1).eq.'A')
		if (ascii) then
			call ibis_column_set( ibis_in, 'u_format','a64',col,status)
			if (status .ne.1) call ibis_signal( ibis_in, status, 1)
			call ibis_column_set( ibis_out, 'u_format','a64',col,status)
			if (status .ne.1) call ibis_signal( ibis_out, status, 1)
		endif
		
		! Get the size of a column's element, in bytes
		call ibis_column_get( ibis_in, 'size', colsize, col, status ) 
		if (status .ne.1) call ibis_signal( ibis_in, status, 1)
		
		! Compute #elements that will fit into buffer
		row_inc = (ibufsize + colsize  - 1)/colsize  
		
		! Actually do some writing. The ibis_column_read/write calls
		! read/write <nrow> rows of column #<col> from buffer,
		! starting at row #<row>. The data is automatically converted
		! into the native host format of the given column.
		
		do row = 1,nr,row_inc
			nrow = MIN( nr+1-row , row_inc) 

			! If this is an ASCII column, need to use the A*64 buffer:
			! Note that we could have used the same buffer if we 
			! just turned the U_FORMAT to NONE.
			
			if (ascii) then
			   call ibis_column_read( ibis_in, cbuffer, 
     +				col, row, nrow,status )
			else
			   call ibis_column_read( ibis_in, buffer, 
     +				col, row, nrow,status )
			endif
     
     			! We know that ibis_in is a valid IBIS handle here
     			! so in case of error, we can call ibis_signal
     	
			if (status.ne.1) call ibis_signal( ibis_in, status, 1) 

			! If this is an ASCII column, need to use CHAR:
			if (ascii) then
			   call ibis_column_write( ibis_out, cbuffer, 
     +				col, row, nrow,status )
			else
			   call ibis_column_write( ibis_out, buffer, 
     +				col, row, nrow,status )
			endif
			if (status.ne.1) call ibis_signal( ibis_out, status, 1) 
		enddo
	enddo

C close up shop 

	call ibis_file_close( ibis_in, ' ', status ) 
	if (status.ne.1) call ibis_signal( ibis_in, status, 1) 
	call ibis_file_close( ibis_out,' ', status )
	if (status.ne.1) call ibis_signal( ibis_out, status, 1) 
	
	return
	end

