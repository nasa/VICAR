ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open the Reseau Database (RDB)
c All arguments are outputs

      subroutine open_rdb(rdb,ibis,nrows)
      implicit none
      integer rdb,ibis		!VICAR and IBIS unit numbers for RDB
      integer nrows		!number of records in RDB

      integer ind,ibis_file_get
      character*6 format(409)/5*'full',404*'real'/	!RDB record format

      call xvunit(rdb,'inp',2,ind,0)  
      call ibis_file_open(rdb,ibis,'read',409,99999,
     &                        format,0,ind)
      if (ind.ne.1) call ibis_signal_u(rdb,ind,1)
      ind = ibis_file_get(ibis,'nr',nrows,1,1)
      if (nrows.lt.0) call ibis_signal(ibis,ind,1)
      return
      end
