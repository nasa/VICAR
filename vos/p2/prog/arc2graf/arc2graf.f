c
c               *** rev. C ***
c
c
c		program:      ARC2GRAF
c		author:       Barbara McGuffie
c		date written: 27 October 1988
c
c		purpose: To take an ARC/INFO ungenerate file,
c		extract the (x,y) componants of its elememts
c		and produce IBIS graphics files.
c
c
c
c*************************************************************************
c       Revision A - BAM     10/88 
c         Initial release
c
c       Revision B - BAM     6/89
c         - MODIFIED FOR POINT FILES
c
c       Revision C - BAM     11/89
c         - corrected error in multiple point files code
c
c       Revision D - BAM     4/98
c         - corrected error in multiple point files code

c  05Feb2010 -lwk- replaced DECODEs and variables in FORMAT statements
c		  with internal reads, for new compiler
c  15Jan2013 -lwk- fixed continued CHARACTER constant for new compiler flag on Solaris
c
c*************************************************************************

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE


        CHARACTER E/'E'/                     ! ASCII character E

        BYTE LINE_FEED/Z'0A'/             ! ASCII line feed




c       BYTE      group(80)             ! ascii group ( level ) value
	character*80 group

c       BYTE      values(80)            ! ascii pair of values
	character*80 values

	CHARACTER*72 outfiles(20)       ! output file/s name/s

	INTEGER   status		! indicates proper functioning
					! of "xv" routines

	INTEGER   idef1		        ! dummy variable for subroutine calls

	INTEGER   outcount		! no. of output files

        INTEGER   nfields               ! number of fields input

        LOGICAL   xvptst                ! a logical expression 

        LOGICAL   point                 ! point files flag

        LOGICAL   deb                   ! debug flag


        INTEGER   i,j,k,l,m             ! loop indicies
     
        LOGICAL   overlap               ! data in multiple files

        INTEGER   end_count             ! counter for E's encountered

        INTEGER   prev_index            ! pointer to last linefeed

        INTEGER   bytes_to_ignore       ! useless bytes to ignore

        INTEGER   bytes_per_pair        ! bytes per pair

        INTEGER   bytes_to_move         ! actual bytes per pair for MVL

        INTEGER   number_of_pairs       ! number of pairs of values

        INTEGER   number_left           ! bytes left in previous block/s

        INTEGER   bytes_left            ! bytes left in previous block 
                                        ! + 2 bytes for length field

        INTEGER   number_of_bytes       ! number of bytes of values

        INTEGER   bytes_per_group       ! bytes per group - LEVEL

        INTEGER   length                ! length of data in points file

        INTEGER   increment             ! increment for length + 2 bytes

        INTEGER   blocks                ! no. of blocks read in OVERLAP zone

        INTEGER   start                 ! start of data for MVL

        INTEGER   n1,n2,n3,iostat,ios   ! information for DCODE
	character*8 fstrng

        INTEGER   ist                   ! start in values array

        INTEGER   max                   ! length of data in buffer

        REAL      x, y, x1, y1, val     ! graphics coordinates


	COMMON /SCALE/ scale, xoffset, yoffset

	REAL*4    scale                 ! scalar
        REAL*4    xoffset, yoffset      ! X and Y offsets


	COMMON /INPUT/ input, inbuf, index

	INTEGER   input	                ! unit number for graphics file/s
	BYTE      inbuf(512)		! 512 byte input buffer
	INTEGER   index			! current position in inbuf array


        COMMON / level / level, levels, nlevels, all_lvls

        INTEGER   level                 ! level for this element
	INTEGER   levels(20)		! a list of graphics level numbers 
                                        ! to be extracted
        INTEGER   nlevels               ! no. of levels 
        LOGICAL   all_lvls		! all levels flag
       

        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set
                                        ! ( used with FLIP parameter )


        COMMON /FORMAT/ fields
	INTEGER   fields(3)		! format of the incoming data.
                                        ! first word contains the integer
                                        ! value equal to the group value;
                                        ! the second word contains the
                                        ! digits to the left of the decimal  
                                        ! point; the third word contains 
                                        ! digits to the right of the 
                                        ! decimal point

        common/data/ buf
        byte    buf(8192)	        ! byte buffer

          



c------------------------------------------------------------------------------
c
c		              PARAMETER PROCESSING
c
c------------------------------------------------------------------------------

	call xvmessage(' ARC2GRAF version 15-Jan-2013',' ')

	call xvp( 'OUT',     outfiles,   outcount)
	call xvp( 'SCALE',   scale,      idef1)  ! input parameters
	call xvp( 'XOFFSET', xoffset,    idef1)
	call xvp( 'YOFFSET', yoffset,    idef1)


        flip = xvptst( 'FLIP' )                ! for line/sample coordinates
        call xvp ( 'NLINES', nlines, idef1)


	call xvparm( 'GROUP',  levels,  nlevels, idef1, ' ' )
        if ( idef1 .eq. 1 ) nlevels = 1
        all_lvls = xvptst( 'ALL')              ! do all levels
        if ( all_lvls )  nlevels = 1	       ! for all levels, do only one


	call xvparm( 'FIELDS',  fields,  nfields, idef1, ' ' )
        if ( nfields .ne. 3 ) then             ! ensure we have a format
	    call xvmessage 
     +	    ('FIELDS  - incomplete format information.  Please correct.',
     +	    ' ')
	    call abend
	endif

        do j = 1, nfields
            if ( fields(j) .eq. 0 ) then
                call xvmessage 
     +	        ('FIELDS  - incorrect format information. Please correct.',' ')
	        call abend
            endif
        end do


        point    = xvptst( 'POINT' )           ! point files flag
        deb      = xvptst( 'DEB' )             ! debug flag


        if ( DEB ) then
            call xvmessage ( '     ' ,' ')
            call xvmessage ( ' ARC2GRAF - Revision C ',' ')
            call xvmessage ( '     ' ,' ')
        end if


c------------------------------------------------------------------------------
c
c                            OPEN FILES
c
c------------------------------------------------------------------------------


 	call xvunit( input, 'INP', 1, status, ' ')	! open input
	if ( status .ne. 1 ) then                       ! Ung   ! Un
   	    call xvmessage(' Error in xvunit - input',' ')
	    call abend
	endif


	call xvopen(input,status,'COND','NOLABELS','U_FORMAT','BYTE',
     -              'U_NS', 512, 'IO_ACT', 'S ', ' ' )
        if ( status .ne. 1 ) then
            call xvmessage(' Data set open error. Program terminated.',
     1		    ' ')
	    call abend
        end if



	if (outcount .ne. nlevels) then     ! check number of output files
	    call xvmessage 
     +	    ('Number of output files does not match number of levels',
     +	    ' ')
	    call abend
	endif


        do j = 1, nlevels
             call wrgr(j, j, 2)             ! open output file/s
        end do




c------------------------------------------------------------------------------
c
c                            MAIN PROCESSING LOOP
c
c------------------------------------------------------------------------------


        n1 = fields(2)                 ! format for decode
        n2 = fields(3)
        n3 = fields(1)        

        bytes_per_group = fields(1)         ! integer field


        overlap = .FALSE.              ! initialize overlap zone
        number_left = 0                ! initialize number left            
        max = 512                      ! bytes per block
        blocks  = 1                    ! and blocks 

        index = 3                      ! initialize start of search
        prev_index = 1                 ! and previous index


        call read_block                ! read first block of data


        if ( point ) then              ! process points file here
                        
             length = inbuf(1)	       ! length of the data to follow     
             increment = length + 2    ! for the next data set
             bytes_to_ignore = 2       ! 2 for length counter
             bytes_per_pair  = 2 * fields(2)  ! for each pair of data values

             do j = 1, 99999           ! loop through data file

               do i = index, max, increment     ! loop through block

                  if ( .not. OVERLAP ) then     ! check for END
                     ist = i
                  else 
                     ist = i - bytes_left
                  end if
                  if ( inbuf(ist) .eq. ichar(E) ) go to 100    

                  number_left = max - i + 1     ! bytes left in block
                  if ( number_left .lt. length ) go to 1

                  start = i                           ! skip length
                  if ( .not. OVERLAP ) then           ! get level
c                      call mvl (inbuf(start),group,bytes_per_group)
                    call mve(1,bytes_per_group,inbuf(start),group,1,1)
                  else
c                      call mvl (buf(start),group,bytes_per_group)
                    call mve(1,bytes_per_group,buf(start),group,1,1)
                  end if
                     
c                 decode ( n3, 20, group, iostat=ios, err=200) level
c  see below ...
		  fstrng = ' '
		  write(fstrng,20) n3
		  read( group, fstrng, iostat=ios, err=200) level

                  start = start + bytes_per_group     ! point to coords
                  if ( OVERLAP ) then                 ! move coordinates
c                      call mvl ( buf(start),values,bytes_per_pair )
                    call mve(1,bytes_per_pair,buf(start),values,1,1)
                  else
c                      call mvl ( inbuf(start),values,bytes_per_pair )
                    call mve(1,bytes_per_pair,inbuf(start),values,1,1)
                  end if
                     
                  ist  = 1                            ! decode coordinates
                  do l = 1,2
c                      decode ( n1, 40, values(ist), 
c    -                          iostat=ios, err=200) val
c  see below ...
		       fstrng = ' '
		       write(fstrng,40) n1, n2
		       read( values(ist:ist+n1-1), fstrng, iostat=ios,
     1				err=200) val
                       if ( l .eq. 1 ) then
                           x = val
                           ist = n1 + 1
                       else
                           y = val
                       end if
                  end do

                  x1 = x / scale + xoffset     ! scale if required
                  y1 = y / scale + yoffset

                  x = x1                       ! watch out for flip
                  y = y1
                  
                  call write_element( x1, y1 ) ! write out
                  call write_element( x, y )   ! write out
                  call write_element( 0., 0. ) ! terminating zeroes 
                  if ( DEB ) call xvmessage ( 'Point line',' ')
               end do
               number_left = i - max - 1
               if ( number_left .eq. 2 ) then
                   number_left = -2
                   bytes_left = 0
                   go to 2
               end if  

 1             bytes_left = number_left + 2
               if ( OVERLAP ) then               ! get another block of data
c                  call mvl ( buf(i-2), buf, bytes_left )
                    call mve(1,bytes_left,buf(i-2),buf,1,1)
               else
c                  call mvl ( inbuf(i-2), buf, bytes_left )
                    call mve(1,bytes_left,inbuf(i-2),buf,1,1)
               end if


 2             call read_block                   ! read data 
   

               start  = number_left + 3       ! address for buf
c               call mvl ( inbuf(1), buf(start), 512 ) ! put in more data
                    call mve(1,512,inbuf(1),buf(start),1,1)


               index      = 3                    ! start search at third byte
               max        = 512 + bytes_left     ! update buffer length
               overlap    = .TRUE.               ! data between files

             end do

        else             

             bytes_per_pair  = 2 * fields(2) + 2 ! for each pair of data values
             bytes_to_ignore = 8       ! 2 for linefeed; 6 for end


             do j = 1, 99999           ! loop through data file

               do i = index, 512          ! loop through block

                 if ( inbuf(i) .eq. ichar(E) ) then     ! check for END
                    end_count = end_count + 1  ! update counter
                    if ( end_count .eq. 2 ) go to 10 ! 2 ends - finished
                                                      ! last element

                 else if ( inbuf(i) .eq. LINE_FEED ) then    

                    end_count = 0              ! reset end counter

 10                 if ( .not. OVERLAP ) then           ! get level
                       start = prev_index + 2           ! skip linefeed
                	                                ! move level 
c                       call mvl (inbuf(start),group,bytes_per_group)
                       call mve(1,bytes_per_group,inbuf(start),
     -                          group,1,1)
                       start = prev_index + bytes_per_group + 4
                    else
                       start = 3                        ! skip linefeed
                	                                ! move level 
c                       call mvl (buf(start),group,bytes_per_group)
                       call mve(1,bytes_per_group,buf(start),group,1,1)
                       start = 1 + bytes_per_group + 4
                    end if

                     
c                   decode ( n3, 20, group, iostat=ios, err=200) level
c20                 format ( I<n3> )
c  above statements are non-standard, replace with:
		    fstrng = ' '
		    write(fstrng,20) n3
20		    format('(I',i3,')')
		    read( group, fstrng, iostat=ios, err=200) level

                    if ( ALL_LVLS ) go to 30  ! ignore elements if not required
                    do m = 1, nlevels   
                        if ( level .eq. levels(m) ) go to 30
                    end do
                    if ( DEB ) call xvmessage ( ' Ignored element',' ')
                    go to 50

 30                 if ( DEB ) PRINT *, ' Level = ', level

                    number_of_bytes = i - prev_index     ! bytes in this block
     -                                + number_left      ! bytes from previous
                                                         ! block
     -                                + ( blocks - 1 ) * 512  ! intermediate
                                                              ! blocks
     -                                - bytes_to_ignore       ! ignored blocks
     -                                - bytes_per_group       ! bytes in level

                    number_of_pairs = number_of_bytes / bytes_per_pair


                    if ( DEB ) then           ! printout information
                       if ( number_of_pairs .eq. 2 ) then
                           call xvmessage ( ' Line segment',' ')
                       else 
                           call xvmessage ( ' Line string',' ')
                       end if
                   end if

                     
                   bytes_to_move = bytes_per_pair - 2

                   do k = 1, number_of_pairs       ! get coordinates
                       if ( OVERLAP ) then
c                           call mvl ( buf(start),  values, 
c     -                                bytes_to_move )
                           call mve(1,bytes_to_move,
     -                              buf(start),values,1,1)
                       else
c                           call mvl ( inbuf(start), values, 
c     -                                bytes_to_move )
                            call mve(1,bytes_to_move,
     -                               inbuf(start),values,1,1)
                       end if
                     
                       ist  = 1
                       do l = 1,2
c                           decode ( n1, 40, values(ist), 
c    -                               iostat=ios, err=200) val
c40                         format ( f<n1>.<n2> )
c  above statements are non-standard, replace with:
			    fstrng = ' '
			    write(fstrng,40) n1, n2
40			    format('(f',i2,'.',i2,')')
			    read( values(ist:ist+n1-1), fstrng, iostat=ios,
     1				     err=200) val
                            if ( l .eq. 1 ) then
                                x = val
                                ist = n1 + 1
                            else
                                y = val
                            end if
                       end do

                       x = x / scale + xoffset      ! scale if required
                       y = y / scale + yoffset

                       call write_element( x, y )   ! write out

                       start = start + bytes_per_pair
                    end do                         

                    call write_element( 0., 0. )    ! terminating zeroes

                    number_left = 0                 ! reinitialize number left
                    blocks  = 1                     ! and blocks 


 50                 if ( end_count .eq. 2 ) go to 100  ! 2 ends - finished

                    overlap = .FALSE.              ! reset overlap

                    prev_index = i                 ! save previous linfeed

                 end if

               end do



               if ( OVERLAP ) then               ! get another block of data
                  start  = number_left + blocks * 512 + 1 ! address for buf
                  blocks = blocks + 1               ! count blocks
               else
                  blocks = 1                        ! no. of blocks
                  number_left = 512 - prev_index + 1! bytes in previous block
c                  call mvl ( inbuf(prev_index), buf, number_left )
                  call mve(1,number_left,inbuf(prev_index),buf,1,1)
                  start = number_left + 1           ! address for buf
               end if


               call read_block                        ! read data 
   

c               call mvl ( inbuf(1), buf(start), 512 ) ! put in more data
                call mve(1,512,inbuf(1),buf(start),1,1)


               index      = 1                    ! start search at first byte
               prev_index = 1                    ! reset previous index

               overlap    = .TRUE.               ! data between files

             end do

        end if


c------------------------------------------------------------------------------
c
c                            FINISH
c
c------------------------------------------------------------------------------

  200   continue
        call xvmessage ( ' Error encountered',' ')


  100   do j = 1, nlevels                         ! close output file/s
             level = levels(j)
             call write_element( 0., 0. )         ! terminating zeroes 
                                                  ! for end of the file
             call clgr(j, j, 2)      
        end do


        call xvclose ( input,  status, ' ' )      ! close input

        if ( deb ) call xvmessage ( ' Task completed',' ')
	return				         
	end


c*******************************************************************************
c
c     *** revision a ***
c
c     subroutine name: read_block
c
c     author: Barbara McGuffie
c
c     date: 15 November 1988
c
c     purpose: read a block of data from disk
c
c     calling sequence: call read_block
c      
c****************************************************************************
c
c     Update history
c
c     Revision A - initial release
c
c****************************************************************************


        subroutine read_block

        implicit none


	COMMON /INPUT/ input, inbuf, index

	INTEGER   input	                ! unit number for graphics file/s
	BYTE      inbuf(512)		! 512 byte input buffer
	INTEGER   index			! current position in inbuf array

        INTEGER   status                ! returned status of xv calls

        INTEGER   block/1/              ! data block to read

        LOGICAL   first/.TRUE./          ! first time in flag

        SAVE      block, first


c---------------------------------------------------------------------------
c
c                       Start Executable Code
c
c---------------------------------------------------------------------------

       if ( FIRST ) then                            ! point to correct block
           block = 1
           first = .FALSE.
       else
           block = block + 1
       end if


       call xvread ( input, inbuf,  status,               ! fill array
     -     'LINE', block, 'SAMP', 1, 'NSAMPS', 512, ' ' )

       if ( status .ne. 1 ) then                       ! Ungen file
   	   call xvmessage(' Error in xvread. Program terminated.',' ')
	   call abend
       end if

 

      return                            
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c		This routine writes an element to a graphics-1 file
c

	subroutine write_element ( x, y )

	IMPLICIT NONE

        REAL       x, y                 ! passed coordinates


        COMMON / LEVEL / level, levels, nlevels, all_lvls

        INTEGER   level                 ! level for this element
	INTEGER   levels(20)		! a list of graphics level numbers 
                                        ! to be extracted
        INTEGER   nlevels               ! no. of levels 
        LOGICAL   all_lvls		! all levels flag



        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set



        INTEGER*4 m                     ! loop index / level no.



	if ( all_lvls ) then            ! select correct output file
   	     m = 1                      ! for all levels
  	     go to 10
	end if

        do m = 1, nlevels
          if ( level .eq. levels(m)) go to 10
        end do

10      if ( x .eq. 0.0 .and. y .eq. 0.0 .or. .not. flip ) then
          call putgr ( m, x, y )
        else 
          call xytols ( x, y )        ! flip here
          call putgr  ( m, x, y )
        end if

	return
	end




ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       xy coordinates to line sample coordinates
c
 	subroutine xytols (a,b)

	implicit none

	real a, b, c


        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set


	c = b
	b = a
        if ( nlines .eq. 0 ) then                   ! for lfj 3/86
            a = c                                   ! no nlines required
        else
  	    a = nlines - c                          ! for specified nlines
        end if
 
	return
	end
