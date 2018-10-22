ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get function and parametrs from grid labels.
c
      subroutine grid_labels(grid)
      implicit none
      integer grid

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/c9/gdlat,gdlon,gslat,gslon,numlat,numlon,nlrg,nlg,nbg,
     & numsam
      real gdlat,gdlon,gslat,gslon
      integer numlat,numlon,nlrg,nlg,nbg,numsam

      integer istat,ifu,minn1
      character*8 func

      integer ibuf(10)		!****never initialized
      real rbuf(10)
      equivalence(ibuf,rbuf)

c these are photfit keywords -- in photfunc, lambert = cosine,
c  oldhapke=hapke, hapke=newhapke, and linear & buratti are not supported.
      character*8 funcn(9)/ 'MINNAERT', 'LAMBERT', 'VEVERKA',
     & 'LINEAR', 'BURATTI', 'MOSHER', 'OLDHAPKE', 'COOK', 'HAPKE'/

      call zia(ibuf,6)

      call xlget(grid,'HISTORY','FUNCTION',func,istat,' ')
      call chkstat(istat, 'ERROR IN GRID LABEL', 1)
      do ifu=1,9
         if (funcn(ifu).eq.func) goto 10
      enddo
      call mabend(' INVALID FUNCTION IN GRID LABEL')

   10 if (ifu.eq.1) then
         minn1=1
      elseif (ifu.eq.2) then
         minn1=3
      elseif (ifu.eq.3) then
         minn1=4
      elseif (ifu.eq.6) then
         minn1=5
      elseif (ifu.ge.7) then
         minn1=2
         ihapke=1
         if (ifu.eq.8) icook=1
         if (ifu.eq.9) ihapke=2
      else
         call mabend(' FUNCTION '//func//' NOT SUPPORTED')
      endif

      if (minn.eq.0 .or. minn.eq.minn1) then
         minn = minn1
      else
         call mabend(
     &     'FUNCTION IN GRID D.S. INCOMPATIBLE WITH USER SPECIFICATION')
      endif
      call prnt(4,1,minn,' MINN=.')

c get grid parameters from label
      call xlget( grid, 'HISTORY', 'DLAT', gdlat, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'DLON', gdlon, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'SLAT', gslat, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'SLON', gslon, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'NUMLAT', numlat, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'NUMLON', numlon, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call prnt(7,4,rbuf,' GRID DLAT,DLON,SLAT,SLON=.')
      call prnt(4,2,ibuf(5),' GRID NUMLAT,NUMLON=.')
      return
      end
