cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c The database is a table (spreadsheet) where each row contains the reseau
c locations for a Voyager image (res) together with its image identifiers (ids).
c The ids uniquely identifies each image taken during the mission.  

c Try to get res from the rdb.
      subroutine get_res_rdb(ids,res,success)
      implicit none
c Input...
      integer ids(5)		!frm,cam,fil,and scet year and day
c Outputs...
      real res(2,202)		!output locations (line,samp)
      logical success		!.true. if row for res was found in rdb

c VICAR and IBIS interfaces to the database...
      common/cibis/rdb,ibis,nrows
      integer rdb               !VICAR unit number for rdb
      integer ibis              !IBIS unit number for rdb
      integer nrows             !number of rows in the rdb

      integer ind
      logical xvptst
 

c Open the reseau database and set cibis...
      call open_rdb(rdb,ibis,nrows)

c If redo, ignore any rdb entry...
      success = .false.
      if (xvptst('redo')) return

c Retrieve res from the rdb...
      call getlocv2(ibis,nrows,ids,res,ind)
      if (ind.eq.0) then
         success = .true.
         call xvmessage('res was retrieved from RDB',0)
      endif
      return
      end
