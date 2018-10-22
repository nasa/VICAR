cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write res to the RDB.
c
      subroutine put_res_rdb(ids,res)
      implicit none
c Inputs...
      integer ids(5)		!pic IDs: ifrm,icam,ifil,iyear,iday
      real res(2,202)		!output locations (line,samp)

      common/cibis/rdb,ibis,nrows
      integer rdb               !VICAR unit number for rdb
      integer ibis              !IBIS unit number for rdb
      integer nrows             !number of images in rdb table

      integer ind
      character*80 msg

      call putlocv2(rdb,ibis,nrows,ids,res)	!put res in RDB
      call ibis_file_close(ibis,' ',ind)
      write(msg,101) ids(1)		!print frame number
  101 format('Frame ',i7,' stored in reseau database.')
      call xvmessage(msg,' ')
      return
      end
