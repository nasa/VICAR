	include 'VICMAIN_FOR'
	subroutine main44
	implicit none

	integer num_values, 	! count of values specified for a parameter
c     +	    defaults_used,	! default used for param (=1) or specified (=0)
     +	    status,		! status from xvunit; dummy for xvread,xvclose
     +	    iform, oform,	! formats for input and output files
     +	    in_unit,		! unit number for reading input file
     +	    out_unit,		! output: unit # or IBIS graphics #
     +	    nl, ns,		! # of lines,samples from input file label
     +	    pix_size,		! # of bytes used to store an input pixel
     +	    new_pix_size	! # of bytes used to read an input pixel
c	character*4 i_format,	! format string for input file
c     +	    o_format		! format string for output file
	character*80 message	! place to write messages prior to qprint call
	logical xvptst		! procedure to test for presence of a keyword
	integer ncol,		! # cols for interface file; # dim for graphics 
     +	    nrow,		! number of rows in interface file
     +	    buf_ptr,		! pointer into line buffer
     +	    i, j
	integer ibis,           ! interger for calls to ibis routines
     +      record              ! record number for ibis_record_write 
	integer*2 zext		! VAX procedure to convert byte to integer*2
	real real		! VAX procedure to convert integer*2 to real

	integer max_ns, max_ncol
	parameter (
     +	    max_ns = 10000,
     +	    max_ncol = 40)

	byte i1_line(4*max_ns)		! \
	integer*2 i2_line(2*max_ns)	!  \ buffers to contain one line of
	integer i4_line(max_ns)		!  / samples & to read them in different
	real r4_line(max_ns)		! /  formats
	equivalence (i1_line, i2_line, i4_line, r4_line)

	real r4_buffer(max_ncol)	! work buffer for assembling data
	integer columns(max_ncol)	! columns to write in write rec call

	include 'fortport'

	call ifmessage('TOIBIS version 6-MAR-95')

! Set the input form. If the user uses the default (the input file's format),
! then we'll specify a U_FORMAT of REAL so that the XVREAD calls will convert
! the data from the input file format to real format for output. If the user
! specifies an input format, then we'll ignore the format that the file may
! have and we'll not specify a U_FORMAT. In the XVREAD call, we'll supply the
! equivalenced buffer that corresponds to the input file format specified by
! the user. If this specified format needs to be converted to real, we'll do
! it prior to writing it out. A value of iform > 0 signifies that we'll be
! overriding the input file format.
	if (xvptst('BYTE')) then
	    iform = 1
	else if (xvptst('HALF')) then
	    iform = 2
	else if (xvptst('FULL')) then
	    iform = 3
	else if (xvptst('REAL')) then
	    iform = 4
	else
	    iform = 0
	endif

! Get the output file format desired; the default is INTERFACE
	if (xvptst('GRAPHICS')) then
	    oform = 1
	    call xvp ('NDIM', ncol, num_values)
	else
	    oform = 2
	    call xvp ('NCOL', ncol, num_values)
	endif

! Open the input file
	call xvunit (in_unit, 'INP', 1, status,' ')

	if (iform .eq. 0) then
!     Open so that we're using XVREAD to convert the input file to real format
	    call xvopen (in_unit, status, 'OP', 'READ', 'OPEN_ACT', 'SA',
     +		'IO_ACT', 'SA', 'U_FORMAT', 'REAL',' ')

	else
!     Open so that we're ignoring the input file format and we're doing our
!     own format conversion
	    call xvopen (in_unit, status, 'OP', 'READ', 'OPEN_ACT', 'SA',
     +		'IO_ACT', 'SA',' ')
	endif

! Get the size of the file, and adjust the sample count if we're altering
! the input file format
!                          iform
!              |    1        2        3,4
!            --------------------------------
!            1 |    ok    ns=ns/2  ns=ns/4
! pix_size   2 | ns=ns*2     ok    ns=ns/2
!            4 | ns=ns*4  ns=ns*2     ok
!
	call xvget (in_unit,status,'PIX_SIZE',pix_size,'NL',nl,
     +						'NS',ns,' ')

	if (iform .ne. 0) then
	    if (pix_size.ne.1 .and. pix_size.ne.2 .and.
     +					 pix_size.ne.4) then
		write (message, 10) pix_size
 10		format (' Input file uses ',I3,
     +              ' bytes per pixel; program limited to 1, 2, 4.')
		call xvmessage (message,' ')
		call abend
	    endif

	    new_pix_size = iform
	    if (new_pix_size .eq. 3) new_pix_size = 4
	    ns = ns  *  pix_size/new_pix_size
	endif

	if (ns .gt. max_ns) then
	    write (message, 12) ns, max_ns
 12	    format (' Input file number of samples (',I7,
     +          ') exceeds program limit (',I7,')')
	    call xvmessage (message,' ')
	    call abend
	endif

! Open the output file, allocating space for the interface file
	if (oform .eq. 1) then		! graphics file
	    out_unit = 1
	    call wrgr (1, out_unit, ncol)
	    nrow = (nl * ns) / ncol
	else				! interface file
	    nrow = (nl * ns) / ncol
c	    call wrfil (out_unit, 1, nrow, ncol, error_sw)
	    call xvunit(out_unit,'OUT',1,status,' ')
	    call ibis_file_open(out_unit,ibis,'WRITE',ncol,nrow,
     +                  ' ',' ',status)
            if (status.ne.1) call ibis_signal(ibis,status,1)
            call ibis_file_set(ibis,'NR',nrow,status)
            if (status.ne.1) call ibis_signal(ibis,status,1)
            call ibis_file_set(ibis,'NC',ncol,status)
            if (status.ne.1) call ibis_signal(ibis,status,1)

!     Initialize for record write call
	    do i = 1, ncol
		columns(i) = i
                call ibis_column_set(ibis,'FORMAT','REAL',i,status)
	    enddo
      	    call ibis_record_open(ibis,record,' ',columns,
     +                    ncol,'REAL',status)
            if (status.ne.1) call ibis_signal(ibis,status,1)
	endif

! Transfer the data from the input file to the output file
	buf_ptr = max_ns + 1

	do i = 1, nrow
!     Transfer a set of data from the input line to the output data

	    go to (400, 410, 420, 430, 400), iform+1

!     Transfer real*4 data (either automatically converted by xvread or
!     specified by user)
 400	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = r4_line(buf_ptr)
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500

!     Transfer byte data (integer) (value is converted first to integer*2,
!     thence to real)
 410	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = real (BYTE2INT(i1_line(buf_ptr)))
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500

!     Transfer half data (2 bytes, integer)
 420	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = real (i2_line(buf_ptr))
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500

!     Transfer full data (4 bytes, integer)
 430	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = real (i4_line(buf_ptr))
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500


! Write out the data for this row
 500	    if (oform .eq. 1) then
		call putgr (out_unit, r4_buffer(1), r4_buffer(2),
     +		    r4_buffer(3))
	    else
c		call putrec (out_unit, ncol, columns, r4_buffer,
c     +		    i, nrow, work_buf)
	        call ibis_record_write(record,r4_buffer,i,status)
                if (status.ne.1) call ibis_signal(ibis,status,1)
	    endif

	enddo  ! looping for all rows in output file

! Close the input and output files; don't need to close the interface file
	call xvclose (in_unit, status,' ')
	if (oform .eq. 1) then
	       call clgr (out_unit)
        else
   	       call ibis_record_close(record,status)
               if (status.ne.1) call ibis_signal(ibis,status,1)
               call xvclose(out_unit, status,' ')
	endif
	return
	end  ! main44
