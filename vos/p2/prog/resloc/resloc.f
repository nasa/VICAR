c Voyager image reseau location program resloc.

      include 'VICMAIN_FOR'	!resloc is entered through VICAR

c Nominal usage...
c	resloc inp=D out=res
c	resloc inp=D out=(res,geo)
c
c Used with a reseau database...
c	resloc inp=(D,rdb)
c       resloc inp=(D,rdb) out=res
c       resloc inp=(D,rdb) out=(res,geo)

      subroutine main44		!top level resloc
      implicit none
      integer ni		!number of inputs (1 or 2)
      integer no		!number of outputs (0,1,or 2)

      byte D(800,800)		!D = 800x800 Voyager byte image
      integer pic		!VICAR unit number for D
      character*7200 lab        !Voyager image label
      integer*4 labsize/7200/

      real*4 res(2,202)		!(l,s) coordinates of the marks found in D
      real*4 nom(2,202)		!nominal coordinates for camera sn
      real*4 os_res(2,202)	!object space (l,s) coordinates

      logical valid(202)	!.true. if res coordinates are valid
      logical d_valid		!.true. if image has been read into D.

      integer sn		!Voyager camera serial number (4,5,6,7)
      real geo(4,24,23)		!geometric transformation parameters for sn
      integer ids(5)		!Image IDs: frm,sn,fil,scet year & day
      integer cnt		!cnt=1 if file is specified.
      logical xvptst,success
      character*132 file

      call ifmessage('resloc version April 14, 2013')
      call open_pic(pic,lab)	!open image D and return pic and lab
      call get_lab_ids(lab,ids)	!get pic identifiers from lab
      sn = ids(2)		!Voyager camera serial number (4,5,6,7)
      call getres(nom,sn)	!get nominal coordinates for sn 
      if (xvptst('pnom')) call print_nom(nom)

      d_valid = .false.		!in the beginning, matrix D is empty

c Find the reseau locations in D or get it from the rdb...
      call xvpcnt('inp',ni)			!number of inputs
      if (ni.eq.1) then				!If inp=D, then
         call read_pic(pic,D,d_valid)		!read pic into D
         call find_res_d(D,lab,nom,res)		!and find res.
      else					!else if inp=(D,rdb), then
         call get_res_rdb(ids,res,success)      !retrieve res from rdb.
         if (.not.success) then			!If res not in rdb, then
            call read_pic(pic,D,d_valid)	! read pic into D
            call find_res_d(D,lab,nom,res)	! find res in D
            call put_res_rdb(ids,res)  		! store res in the rdb
         endif
      endif
      if (xvptst('pres')) call print_res(res)

c Output res and geo...
      call xvpcnt('out',no)			!number of outputs=0,1 or 2
      if (no.eq.1) then
         call write_res(res,ids)		!output res
      elseif (no.eq.2) then
         call write_res(res,ids)		!output res
         call vgros(sn,os_res)			!get object space coordinates
         call compute_geo(res,os_res,geo)	!compute geo
         call write_geo(geo)			!output geo
      endif

c Optionally overlay res on image D...
      call xvp('ores',file,cnt)
      if (cnt.eq.1) then
         call read_pic(pic,D,d_valid)
         call overlay(D,res,file)
      endif

c Optionally overlay nom on image D...
      call xvp('onom',file,cnt)
      if (cnt.eq.1) then
         call read_pic(pic,D,d_valid)
         call overlay(D,nom,file)
      endif

      call xvmessage('resloc task completed',0)
      return
      end
