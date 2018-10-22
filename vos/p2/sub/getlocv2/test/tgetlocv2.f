c
c
c       TGETLOCV2
c
c
C-----THIS IS A ROUTINE TO TEST SUBROUTINES GETLOCV2/PUTLOCV2
C-----PUTLOCV2.



	include 'VICMAIN_FOR'
	subroutine main44

c
c	rewritten by BAM  4/96
c
	IMPLICIT INTEGER (A-Z)
	COMMON/C/ UNIT,LAST,NL
        real*4 fdata(404)
   
c for porting     BAM
        integer in2
        integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer status

        integer jdata(5)    ! IBIS data arrays

        integer iframe(2000)

        include 'fortport'


c
c       start main code
c

	CALL XVUNIT( in2, 'INP', 2, STATUS, 0) ! get a unit
        call ibis_file_open(in2,ibis,'read',409,99999, 
     -                        format,0,status)
        if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)


        icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
        if ( nrows .lt. 0 ) call ibis_signal(ibis,count,1)
	CALL PRNT(4,1,nrows,' Number of rows in the file = ')


                                  ! get all the frames
        call ibis_column_read(ibis,iframe,1,1,nrows,istat)
        if ( istat .ne. 1 ) call ibis_signal(ibis,' ',istat)


	CALL xvmessage(' FRAMES IN FILE BEFORE UPDATE',0)


        do i = 1, nrows    ! report what's in the input reseau file
          if ( iframe(i) .ne. 0 ) then
              frm = iframe(i)  
              jdata(1) = frm
              jdata(2) = 0
              call getlocV2(ibis,nrows,jdata,fdata,iret)
      	      CALL PRNT(0,4,iframe(I),' IN HEX.')
    	      CALL PRNT(7,8,fdata,' 1ST 4 (L,S).')
          end if
        end do
C
	FRM = 2
	CAM = 4
        jdata(1) = frm
        jdata(2) = cam
	CALL PUTLOCV2(in2,ibis,nrows,jdata,fdata)   !FILL RESEAU RECORD
        icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
        if ( nrows .lt. 0 ) call ibis_signal(ibis,count,1)
	CALL PRNT(4,1,nrows,' Number of rows in the file = ')
	CALL xvmessage(' FRAMES IN FILE AFTER UPDATE',0)
C
        do i = 1, nrows    ! report what's in the input reseau file
          frm = iframe(i)  
          jdata(1) = frm
          jdata(2) = 0
          if ( frm .ne. 0 ) then
              call getlocV2(ibis,nrows,jdata,fdata,iret)
    	      CALL PRNT(0,4,frm,' IN HEX.')
 	      CALL PRNT(7,8,fdata,' 1ST 4 (L,S).')
          end if
        end do

        call ibis_file_close( ibis,' ', status )

	RETURN
	END
