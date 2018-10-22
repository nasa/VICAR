
      subroutine new_class_fortran
      integer unit, i, ibis,status
      integer lutcols(3)
      integer pixpt(3)
      integer valpt
      integer frame
      
      do i=1,3
         lutcols(i)=i
         pixpt(i)=i+3
      enddo
      valpt=7
      frame=8
      
      
      ! create IBIS File 
      call xvunit(unit,'out',1,status,' ')
      call ibis_file_open(unit,ibis,'write',
     +         10,256,' ',' ',status)
      if (status.ne.1) call ibis_signal_u(unit,status,1)
      
      ! 
      ! Set up groups. If this were a pre-existing file
      !  we would have to check to see if the groups
      !  already existed, in which case we would use
      !  IBISGroupModify( 'APPEND') to add the columns.
      
      
      !
      !  The stretch is by default only in the class C_VALUE.
      !   We need to override this so that it will be RGB.
      !   No instance needed, so we pass in a blank.
      
      
      call icl_new_rgb(ibis,lutcols(1),lutcols(2),
     +                      lutcols(3),' ',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      call icl_new_lookup_table(ibis,lutcols,3,0,0,
     +                     'PSEUDOCOLOR','MyLut',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      
      
      !
      !  Declare value and pixel subtypes for POINT
      
      call icl_new_value(ibis,valpt,1,'DN',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      call icl_new_pos_image(ibis,pixpt(1),pixpt(2),
     +                            pixpt(3),' ',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      
      ! create the pointer class 
      call icl_new_point(ibis,pixpt,3,valpt,1,
     +           'PIXEL','MyPOINT',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      call icl_new_point(ibis,frame,1,0,0,'FRAME','MyPOINT',status)
      if (status .lt. 0) call ibis_signal(ibis,status,0)
      
      call ibis_file_close(ibis,' ',status)
      
      return
      end
      
      
      
      subroutine get_class_fortran
      integer unit, ibis,status,count
      integer lutcols(3)
      integer pixpt(3)
      integer valpt
      integer frame
      integer ibis_column_find
      character*80 message
      
      ! open IBIS File 
      call xvunit(unit,'inp',1,status,' ')
      call ibis_file_open(unit,ibis,'read',0,0,' ',' ',status)
      if (status.ne.1) call ibis_signal_u(unit,status,1)  

      ! 
      ! Get groups. We go exactly backwards from the
      !  order in the 'new' routine.
      !
      
      !
      !  The stretch was by default only in the class C_VALUE.
      !   We needed to override this so that it would be RGB.
      !   No index needed, so we pass in blank. Note that the
      !   'get' routines do not have a status; we detect failure
      !   by counting the columns in the local group we define.
      !
       
      call icl_get_lookup_table(ibis,'$MyLut',
     +   ' ','PSEUDOCOLOR','MyLut')
      call icl_get_rgb(ibis,'$MyRED','$MyGRN','$MyBLU','$MyLut')

      count = ibis_column_find(ibis,'LOCAL','$MyRED',lutcols,1,3)
      write(message,'(I2,A,I2)') count,' RED LUT Cols: ',lutcols(1)
      call xvmessage(message,' ')
      count = ibis_column_find(ibis,'LOCAL','$MyGRN',lutcols,1,3)
      write(message,'(I2,A,I2)') count,' GRN LUT Cols: ',lutcols(1)
      call xvmessage(message,' ')
      count = ibis_column_find(ibis,'LOCAL','$MyBLU',lutcols,1,3)
      write(message,'(I2,A,I2)') count,' BLUE LUT Cols: ',lutcols(1)
      call xvmessage(message,' ')

      ! Get the FRAME and PIXEL members of POINT instance 'MyPOINT'
      call icl_get_point(ibis,'$MyFrame',' ','FRAME','MyPOINT')
      call icl_get_point(ibis,'$MyPOS','$MyVal','PIXEL','MyPOINT')

      count = ibis_column_find(ibis,'LOCAL','$MyFrame',frame,1,1)
      write(message,'(I2,A,I2)') count,' FRAME LUT Cols: ',frame
      call xvmessage(message,' ')
      
      !
      !  Get value and pixel subtypes from POINT
      !
      call icl_get_value(ibis,'$MyDN','$MyVal & DN')
      count = ibis_column_find(ibis,'LOCAL','$MyDN',valpt,1,1)
      write(message,'(I2,A,I2)') count,' DN LUT Cols: ',valpt
      call xvmessage(message,' ')

      call icl_get_pos_image(ibis,'$MyLINE',
     +             '$MySAMP','$MyBAND','$MyPOS')
      count = ibis_column_find(ibis,'ANY',
     +             '$MyLINE | $MySAMP | $MyBAND',pixpt,1,3)
      write(message,'(I2,A,3I2)') 
     +   count,' POSITION Cols:(L,S,B)= ',pixpt(1),pixpt(2),pixpt(3)
      call xvmessage(message,' ')

      call ibis_file_close(ibis,' ',status)
      
      return
      end
      
