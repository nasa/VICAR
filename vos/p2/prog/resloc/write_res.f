cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write res to IBIS file (out=res) 

      subroutine write_res(res,ids)
      implicit none
c Inputs...
      real res(2,202)	!reseau locations
      integer*4 ids(5)	!frame,camera,filter,year,day

      integer runit,ibis	!VICAR and IBIS	unit numbers for res
      integer nrows/1/		!res is an IBIS file of 1 row
      integer ind
      character*80 msg
      character*6 format(409) /5*'full',404*'real'/

c     ...open runit as a VICAR file to write history label
      call xvunit(runit,'out',1,ind,0)		!out=(res,geo)
      call xvopen(runit,ind,'op','write','open_act','sa',
     &	 'io_act','sa',' ')
      call xladd(runit,'history','title','**RESLOC COORDINATES**',
     .	 ind,'format','string',0)
      call xvclose(runit,ind,0 )

c     ...open res as an IBIS file to write res
      call xvunit(runit,'out',1,ind,0)
      call ibis_file_open(runit,ibis,'write',409,nrows,format,0,ind)
      if (ind.ne.1) call ibis_signal_u(runit,ind,1)
      call putlocv2(runit,ibis,nrows,ids,res)	!write the record
      call ibis_file_close(ibis,' ',ind)
      return
      end
