$!****************************************************************************
$!
$! Build proc for MIPL module arc2graf
$! VPACK Version 1.9, Tuesday, January 15, 2013, 16:59:43
$!
$! Execute by entering:		$ @arc2graf
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module arc2graf ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to arc2graf.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("arc2graf.imake") .nes. ""
$   then
$      vimake arc2graf
$      purge arc2graf.bld
$   else
$      if F$SEARCH("arc2graf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake arc2graf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @arc2graf.bld "STD"
$   else
$      @arc2graf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create arc2graf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack arc2graf.com -mixed -
	-s arc2graf.f -
	-p arc2graf.pdf -
	-i arc2graf.imake -
	-t tstarc2graf.pdf tstarc2graf.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create arc2graf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create arc2graf.pdf
PROCESS HELP=*
 PARM INP        TYPE=STRING   COUNT=1                      DEF=""
 PARM OUT        TYPE=STRING   COUNT=(1:20)                 DEF=""
 PARM FIELDS     TYPE=INTEGER  COUNT=(1:3)                  DEF=(10,15,6)
 PARM GROUP      TYPE=INTEGER  COUNT=(1:20)                 DEF=1
 PARM ALL        TYPE=KEYWORD  COUNT=(0:1) VALID=(ALL)      DEF=--
 PARM FLIP       TYPE=KEYWORD  COUNT=(0:1) VALID=(FLIP)     DEF=--
 PARM NLINES     TYPE=INTEGER  COUNT=(0:1)		    DEF=0
 PARM SCALE      TYPE=REAL     COUNT=1                      DEF=1.0
 PARM XOFFSET    TYPE=REAL     COUNT=1                      DEF=0.0
 PARM YOFFSET    TYPE=REAL     COUNT=1                      DEF=0.0
 PARM POINT      TYPE=KEYWORD  COUNT=(0:1) VALID=POINT      DEF=--
 PARM DEB        TYPE=KEYWORD  COUNT=(0:1) VALID=DEB        DEF=--
END-PROC
.TITLE
VICAR/IBIS Program ARC2GRAF
.HELP
PURPOSE
-------

ARC2GRAF converts 2-D ARC/INFO point or line UNGENERATE format 
files to IBIS Graphics-1 file format.  The program will produce 
up to 20 output data sets.  Each output data set is generated from
a specified line descriptor.  


EXECUTION
---------
Execution is initiated by:

ARC2GRAF INP (OUT1,OUT2...OUT20) PARAMETERS

See tutor mode for an explanation of the parameters.


RESTRICTIONS
------------

1) There can be at most twenty output files.
2) 2-D data only is accepted.

.LEVEL1
.VARIABLE INP
Input filename.
.VARIABLE OUT
Output filename(s).
.VARIABLE FLIP
Input  graphics  file 
coordinates are Y , X
(line,sample) format. 
.VARIABLE NLINES
Number of lines represented
in the input graphics file.
Used with FLIP option.
.VARIABLE GROUP
User specified lines to be
extracted: ( 1 per output
data set ).
.VARIABLE ALL
Ignore levels  specified
in "GROUP" and  produce
one output data set con-
taining all  input  file 
information.
.VARIABLE SCALE
Data divisor.
.VARIABLE XOFFSET
X offset
.VARIABLE YOFFSET
Y offset
.VARIABLE FIELDS
Input data format.
.VARIABLE POINT
Indicates that output 
should be in POINT
file format.
.VARIABLE DEB
Debug flag - Produces 
listing on monitor of 
digitized types.
.LEVEL2
.VARIABLE INP
            UNGENERATE file name.
.VARIBLE  OUT
            Output filename(s). Maximum 20.

            First  output file name is for the first  line;
            second output file name is for the second line, etc.

.VARIABLE FLIP
            Indicates that coordinates from the input graphics 
            file/s are in Y,X ( line, sample ) format.   These 
            will be flipped to an X,Y coordinate system.

            Default is X,Y.
.VARIABLE NLINES
            Number of lines  represented  in  the input 
            graphics file. This variable is only needed
            if the FLIP keyword is invoked.
.VARIABLE GROUP
             An array containing the lines from which to extract
             data. Up to 20 lines may be input. However, the num-
             ber  of output graphics files must equal the number 
             of levels or the program will abort.
.VARIABLE ALL
             Ignore groups specified in "GROUP " and produce
             one  output graphics file containing all design 
	     file graphics information.
.VARIABLE SCALE
             Data will be divided by this scalar if required.

             Default = 1.0
.VARIABLE XOFFSET
            X offset - value added to UNGENERATE X values.

            Default = 0
.VARIABLE YOFFSET
            Y offset - value added to UNGENERATE Y values.

            Default = 0
.VARIABLE FIELDS
            Input data format. Informs the program of the Fortran
            formats of the input data.


            First word contains the integer format for the line 
            descriptor. For example if the line is input in I10
            format,  the user would  code the number 10 in this 
            field. The second and third words describe the X, Y
            coordinate data pairs.   It  is  assumed that these 
            values are in F format.  The second  word  contains 
            the  digits  to the left of the decimal point;  the
            third  word  contains  digits  to  the right of the 
            decimal point.

            Defaults = 10, 15, 6
.VARIABLE POINT
            Indicates that the output file should be in POINT
            file format.
.VARIABLE DEB
                               Debug flag 

            Produces listing on monitor of digitized 
            types (e.g. LINE SEGMENT, LINE STRING, POINT LINE ).
            This is a program aid for debugging code.

 
                              Valid = "DEB"
.END
$ Return
$!#############################################################################
$Imake_File:
$ create arc2graf.imake
#define PROGRAM arc2graf

#define MODULE_LIST arc2graf.f 

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstarc2graf.pdf
PROCEDURE
BODY

IBIS-GEN    A DATAC=(1,2) NC=2 NR=15 'COL DATA=(10,10,20,15,20,5,10,10,0,0, +
            10,25,20,30,20,20,10,25,0,0, +
            25,12.5,25,22.5,35,17.5,25,12.5,0,0)
IBIS-COPY   A B SR=1 SC=1 NC=2 'ROW
IBIS-LIST   B
GRAF2ARC    B A.TMP 'FLIP
!DCL TYPE    A.TMP
ush cat A.TMP
ARC2GRAF    A.TMP B 'all 
IBIS-LIST   B gr1dim=2 nr=16 
END-PROC

$!-----------------------------------------------------------------------------
$ create tstarc2graf.log_solos
tstarc2graf
Beginning VICAR task IBIS
Beginning VICAR task IBIS
Beginning VICAR task IBIS
 
Number of Rows:15  Number of Columns: 2       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:15
+-----------+-----------
         C:1         C:2
+-----------+-----------
       10.00       10.00
       20.00       15.00
       20.00        5.00
       10.00       10.00
        0.00        0.00
       10.00       25.00
       20.00       30.00
       20.00       20.00
       10.00       25.00
        0.00        0.00
       25.00       12.50
       25.00       22.50
       35.00       17.50
       25.00       12.50
        0.00        0.00
Beginning VICAR task GRAF2ARC
 GRAF2ARC version 05-Feb-2010
Beginning VICAR task ARC2GRAF
 ARC2GRAF version 15-Jan-2013
 Error encountered
Beginning VICAR task IBIS
 
Number of Rows:64  Number of Columns: 2       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:16
+-----------+-----------
         C:1         C:2
+-----------+-----------
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
        0.00        0.00
exit
slogoff
$ Return
$!#############################################################################
