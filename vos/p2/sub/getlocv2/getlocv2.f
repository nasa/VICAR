      subroutine getlocv2(ibis,nrows,buf,fdata,lret)

c
c     Arguments: ibis = ibis file # from ibis_file_open call (input)
c                nrows = # of rows in the reseau file (input)
c                buf = nasty remnant from original code 
c                      this array has 5 variables that are in common
c		       in the main and are as follows:
c                      frame,camera,filter,year and day - sorry about that
c                      (input)
c                fdata = array containing reseau locations (returned)
c                lret = error flag (returned)
c			0 = good
c			1 = error occurred                
c
c


C ROUTINE TO GET RESEAU RECORD FROM VOYAGER RESLOC FILE

      IMPLICIT INTEGER(A-Z)

      integer buf(5)
      real*4 fdata(404)
      

c for porting     BAM
	integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer record1
        integer record2
        integer lrw
        integer status
        integer day

        integer jdata(5)    ! IBIS data arrays
   
	include 'fortport'

c
c       Note: the below array information comes in via a common block
c             within the calling programs


C    DSRN       BUF(1) = FRAME
c               BUF(2) = CAMERA
C               BUF(3) = FILTER
C               BUF(4) = YEAR
C               BUF(5) = DAY
C               BUF(6) = RES(1,1)    reseaux
C                 .  
C                 .  
C
C               BUF(409)=RES(2,202)
C
                           ! initialize useful stuff
      FRAME  = BUF(1)      ! obvious  - frame
      CAMERA = BUF(2)      ! and camera


      call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_read(record1,jdata,1,status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      found = 0
      l = 1
      do while ( found .eq. 0 .and. l .le. nrows )
          call ibis_record_read(record1,jdata,l,status)
          if ( FRAME.eq.jdata(1) ) then
              IF ( CAMERA.EQ.0.OR.CAMERA.EQ.jdata(2)) go to 6
          end if
          l = l + 1
      end do

      
      LRET = 1    ! error - not found
      go to 10

    6 continue     ! we have data
      call ibis_record_read(record2,fdata,l,status) ! read reseau

      call prnt (4, 1, jdata(1), ' Frame  = ')
      call prnt (4, 1, jdata(2), ' Camera = ')
      call prnt (4, 1, jdata(3), ' Filter = ')
      call prnt (4, 1, jdata(4), ' Year   = ')
      call prnt (4, 1, jdata(5), ' Day    = ')

      do i = 1, 5     ! transfer information for output
          buf(i) = jdata(i)
      end do

      LRET = 0    ! found

 10   continue
      call ibis_record_close(record1,status)
      call ibis_record_close(record2,status)

      RETURN
      end
 
CCCCCCCCCCCCCCCCC



      subroutine putlocv2(in2,ibis,nrows,buf,fdata)
c
c     Arguments: in2 = reseau input file for update
c                ibis = ibis file # from ibis_file_open call (input)
c                nrows = # of rows in the reseau file (input)
c                buf = nasty remnant from original code 
c                      this array has 5 variables that are in common
c		       in the main and are as follows:
c                      frame,camera,filter,year and day - sorry about that
c                      (input)
c                fdata = array containing reseau locations (returned)
c
c
C  PUT RESLOCS INTO RESLOC FILE


      IMPLICIT INTEGER(A-Z)
      INTEGER buf(5)
      real*4 fdata(404)      

c for porting     BAM
	integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer record1
        integer record2
        integer lrw
        integer status
        integer day
        integer jdata(5)

	include 'fortport'



      FRAME = BUF(1)
      CAMERA = BUF(2)

      call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

c
c     check if we are writing to the master reseau file
c     if we are not, ie. nrows = 1, then just write
c

      if ( nrows .eq. 1 ) go to 10  

      call ibis_record_read(record1,jdata,1,status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

C          SEE IF FRAME IS ALREADY IN RESLOC FILE

      found = 0
      l = 1
      do while ( found .eq. 0 .and. l .le. nrows )
          call ibis_record_read(record1,jdata,l,status)
          if ( (frame.eq.jdata(1))) then
              found = 1
              go to 10
          end if
          l = l + 1
      end do

c
c     writing to reseau file
c

 10   continue
      call ibis_file_close(ibis,' ',status)

      jdata(1) = frame          ! fill the first 5 columns
      jdata(2) = camera
      jdata(3) = buf(3)
      jdata(4) = buf(4)
      jdata(5) = buf(5)

      if ( nrows .ne. 1 ) then  ! write to correct location
          if ( found .eq. 0 ) then
              next = nrows + 1      ! master file
          else
              next = nrows
          end if

          call xvunit( in2, 'INP', 2, status, 0)  
          if ( status .ne. 1 ) then   
              call xvmessage ( ' Error getting unit for reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if
          call ibis_file_open(in2,ibis,'update',409,next,
     -                        format,0,status)
          if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
      else
          next = 1              ! single file
          call ibis_file_open(in2,ibis,'write',409,next,
     -                        format,0,status)
          if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
          call xvunit( in2, 'OUT', 1, status, 0)  
          if ( status .ne. 1 ) then   
              call xvmessage ( ' Error getting unit for reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if
      end if


c
c     set the number of rows properly - this is necessary
c     when trying to add a row of data in update mode for
c     the new ibis routines.....  
c
      call ibis_file_set(ibis,'nr',next,status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      if ( found .eq. 0 ) then  ! new record
          call ibis_record_write(record2,fdata,next,status)
          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
          call ibis_record_write(record1,jdata,next,status)
          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
      else                      ! updated record
          call ibis_record_write(record2,fdata,1,status)
          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
          call ibis_record_write(record1,jdata,1,status)
          if ( status .ne. 1 ) call ibis_signal(l,status,1)
      end if
      nrows = next     ! reset to reflect type of reseau file

      RETURN
      END
