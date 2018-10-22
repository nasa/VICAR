c
c               *** rev. D ***
c
c
c		program:      GRAF2ARC
c		author:       Barbara McGuffie
c		date written: 7 December 1988
c
c		purpose: To take a VICAR/IBIS graphics 1 file,
c		extract the (x,y) componants of its elememts
c		and produce an ARC/INFO UNGENERATE file.
c
c
c
c*************************************************************************
c       Revision A - BAM     12/88 
c         Initial release
c
c       Revision B - BAM     6/89 
c         modified for point file formats
c
c       Revision c - BAM     9/89 
c         ignored first zeroes in the file
c
c       Revision D - BAM     1/95 
c         included a 3d IBIS graphics input
c         for contours - Muller UK
c
c   05Feb2010 -lwk- replaced variables in format stmts with internal writes;
c		  program is screwy:  code allows for multiple output files
c		  (but does not assign unit #s), but pdf has only 1 output,
c		  which is consistent with pgm. ARC2GRAF ... for now, restrict
c		  input & output counts to 1.

c*************************************************************************

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE




        REAL      data(500000)          ! data buffer of values
        INTEGER   group_no              ! group number

	CHARACTER*72 infiles(20)        ! input file/s name

	CHARACTER*72 outfile(20)        ! output file name


	INTEGER   idef1		        ! dummy variable for subroutine calls

	INTEGER   incount		! no. of input files

	INTEGER   outcount		! no. of output files

        INTEGER   nfields               ! number of fields input

        LOGICAL   xvptst                ! a logical expression 

        LOGICAL   deb                   ! debug flag


        LOGICAL   EOF                   ! end of file flag

        LOGICAL   ZERO                  ! zero terminators encountered flag
 
        LOGICAL   three_d               ! 3D IBIS graphics file flag
        INTEGER   dimension             ! IBIS file dimension

        REAL      first_c               ! coordinates returned from GETGR
        REAL      second_c
        REAL      third_c



        INTEGER   len                   ! length of bytes for o/p file
     
        INTEGER   number_of_pairs       ! number of pairs of values

        INTEGER   ist                   ! start in values array

        COMMON /POINT/ point
        LOGICAL point                   ! for point files



	COMMON /OUTPUT/ output, j

	INTEGER   output(20)		! unit for output file/s
        INTEGER   j                     ! loop index / output file
       

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
          
	REAL*4    scale                 ! scalar

        REAL*4    xoffset, yoffset      ! X and Y offsets

        INTEGER   low_elev, high_elev

        INTEGER   nstrings

	call xvmessage(' GRAF2ARC version 05-Feb-2010', ' ')

c------------------------------------------------------------------------------
c
c		              PARAMETER PROCESSING
c
c------------------------------------------------------------------------------

	call xvp( 'INP',     infiles,    incount)   ! input files
	call xvp( 'OUT',     outfile,    outcount ) ! output files
	call xvp( 'LOW',     low_elev,   idef1)     ! scale for data
	call xvp( 'HIGH',    high_elev,  idef1)     ! scale for data
	call xvp( 'SCALE',   scale,      idef1)     ! scale for data
	call xvp( 'XOFFSET', xoffset,    idef1)     ! x offset
	call xvp( 'YOFFSET', yoffset,    idef1)     ! y offset

        if ( incount .ne. outcount ) then     ! be sure we have enough files
	    call qprint 
     +	    ('INP/OUT  - unequal input/output files.  Please correct.')
	    call abend
	endif

        flip = xvptst( 'FLIP' )                ! for line/sample coordinates
        call xvp ( 'NLINES', nlines, idef1)

	three_d = xvptst('THREE_D')            ! 3-D flag
        if ( THREE_D ) then
            dimension = 3            
        else
            dimension = 2                      ! 2-D graphics
        end if

	call xvparm( 'FIELDS',  fields,  nfields, idef1, 0 )   ! input format
        if ( nfields .ne. 3 ) then             ! ensure we have a format
	    call qprint 
     +	    ('FIELDS  - incomplete format information.  Please correct.')
	    call abend
	endif

        do j = 1, nfields
            if ( fields(j) .eq. 0 ) then
                call qprint 
     +	        ('FIELDS  - incorrect format information. Please correct.
     +          ')
	        call abend
            endif
        end do


        point = xvptst( 'POINT' )           ! point file flag
	call xvp( 'GROUP', group_no, idef1) ! starting group number


        len   = fields(2) + fields(2)       ! compute record length
        if ( POINT ) len = len + fields(1)


        deb   = xvptst( 'DEB' )             ! debug flag

        if ( DEB ) then
            call qprint ( '     ' )
            call qprint ( ' GRAF2ARC - Revision D ')
            call qprint ( '     ' )
        end if


        do j = 1, incount
             call rdgr(j, j, dimension)    ! open input file/s
        end do


c------------------------------------------------------------------------------
c
c                            MAIN PROCESSING LOOP
c
c------------------------------------------------------------------------------


        nstrings = 0

	output(1) = 1	! arbitrary (lwk)

        do j = 1, incount              ! loop through input files

            ist = 1                    ! start of temp data buffer

            open ( UNIT=output(j), FILE=outfile(j), STATUS='NEW', ! open output
     +       RECL=len)

            number_of_pairs = 0        ! initialize value

 10         continue                   ! get coordinate pair

            call getgr ( j, ZERO, EOF, first_c, second_c, third_c )

            if ( .not. EOF ) then      ! check for end of file
     
                if ( .not. ZERO ) then           ! check for data

                                       ! see if we need this elevation
                    if (third_c.lt.low_elev) go to 10
                    if (third_c.eq.high_elev) go to 105

                    number_of_pairs  = number_of_pairs + 1  ! count pairs
                    data ( ist )     = first_c  / scale + xoffset
                    data ( ist + 1 ) = second_c / scale + yoffset
                    if ( dimension .eq. 3 ) group_no = third_c
                    ist   = ist + 2              ! update buffer pointer

                else                             ! write data to file 
                    if ( number_of_pairs .ne. 0 ) then
                        call write_data ( group_no, data, 
     -                                    number_of_pairs )
                        nstrings = nstrings + 1
       
                        if ( DEB .and. number_of_pairs .eq. 2 ) then
                            if ( POINT ) then
                                call qprint ( ' Point line ' )
                            else
                                call qprint ( ' Line segment' )
                            end if
                        else if ( DEB .and. number_of_pairs .gt. 2 )then
                            call qprint ( ' Line string ' )
                        end if       
                    
                        number_of_pairs = 0          ! reinitialize pointers
                        ist = 1
                        if ( .not. POINT .and. dimension .eq. 2) then
                            group_no = group_no + 1  
                        end if
                    end if
                end if

                go to 10               ! get more data

            end if


  105       continue
            write ( output(j), 110 )             ! put on the terminator end
            if ( POINT ) write ( output(j), 110 )! finish point file
  110       format ( 'END' ) 
            close ( unit=output(j), status='KEEP' )! close output file

            if ( DEB ) print *, ' File', j, ' completed.'

        end do

c------------------------------------------------------------------------------
c
c                            FINISH
c
c------------------------------------------------------------------------------

  
        do j = 1, incount                       ! close input file/s
             call clgr(j, j, 2)      
        end do


        if ( deb ) call qprint ( ' Task completed')
	return				         
	end


c*******************************************************************************
c
c     *** revision a ***
c
c     subroutine name: write_data
c
c     author: Barbara McGuffie
c
c     date: 12 December 1988
c
c     purpose: write data to output file
c
c     calling sequence: call write_data ( group_no, data, 
c                                         number_of_pairs )
c      
c****************************************************************************
c
c     Update history
c
c     Revision A - initial release
c
c****************************************************************************


        subroutine write_data ( group_no, data, number_of_pairs )

        implicit none

        INTEGER   group_no              ! group numgber
        REAL      data(500000)          ! data buffer of values
        INTEGER   number_of_pairs       ! number of pairs of values


        COMMON /POINT/ point
        LOGICAL point                   ! for point files


	COMMON /OUTPUT/ output, j

	INTEGER   output(20)		! unit for output file/s
        INTEGER   j                     ! loop index / output file


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

	character*15 fstrng

        INTEGER   i                     ! loop index
        INTEGER   ist                   ! start of the data in real array



c---------------------------------------------------------------------------
c
c                       Start Executable Code
c
c---------------------------------------------------------------------------

       ist = 1
       if ( FLIP ) then
           do i = 1, number_of_pairs        ! flip coordinates if required
               call lstoxy ( data(ist), data(ist+1) )
               ist = ist + 2
           end do                  
       end if

       ist = 1                              ! start of data
       if ( POINT ) then

c          write ( output(j), 10 ) group_no, data(1), data(2)
c10        format ( I<fields(1)>,2F<fields(2)>.<fields(3)>)
c  above is non-standard, for new compiler must replace with:
	   fstrng = ' '
	   write( fstrng, 10) fields(1), fields(2), fields(3)
10	   format('(I',i4,',2F',i2,'.',i2,')')
	   write( output(j), fstrng) group_no, data(1), data(2)

       else

c          write ( output(j), 20 ) group_no 
c20        format ( I<fields(1)>)
c  above is non-standard, for new compiler must replace with:
	   fstrng = ' '
	   write( fstrng, 20) fields(1)
20	   format('(I',i4,')')
	   write( output(j), fstrng) group_no

           do i = 1, number_of_pairs
c              write ( output(j), 30 ) data(ist), data(ist + 1)
c30            format ( 2F<fields(2)>.<fields(3)>)
c  above is non-standard, for new compiler must replace with:
	       fstrng = ' '
	       write( fstrng, 30) fields(2), fields(3)
30	       format('(2F',i2,'.',i2,')')
	       write( output(j), fstrng) data(1), data(2)
               ist = ist + 2
           end do     

           write ( output(j), 40 )                ! put on an end
 40        format ( 'END' ) 

       end if

       return                            
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       line/sample coordinates to cartesian coordinates
c
 	subroutine lstoxy (a,b)

	implicit none

	real a, b, c


        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set


	c = a
	a = b

        if ( nlines .eq. 0 ) then                   ! for lfj 3/86
            b = c                                   ! no nlines required
        else
  	    b = nlines - c                          ! for specified nlines
        end if

	return
	end
