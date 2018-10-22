      subroutine PBNAME(ID,PAR,*)
c
c  This is simply an interface to the SPICE routines BODN2C and BODC2N,
c  provided for compatibility with the VICAR calling sequence.
c  (The old code for PBNAME is included in the pbname.com as pbname_old.f.)
c
c  11aug10 -lwk- delivered SPICE version
c  22sep10 -lwk- XPBID was corrupting string being passed to it!  Fixed to
c               first copy this to local buffer.
c
C  ROUTINE TO RETURN PLANET NAME GIVEN ID
c
      character*12 par
      logical found

      call bodc2n( id, par, found)
      if (.not.found) return 1
      return
C
C ROUTINE TO RETURN PLANET ID GIVEN PLANET NAME (PAR)
c
      entry PBID(PAR,ID,*)
C
      call bodn2c( par, id, found)
      if (.not.found) return 1
      return

      end

c  FORTRAN bridges for above routines:

      subroutine xpbname(id,bpar,ind)
      byte bpar(13)
      character*12 par
      ind = 1
      call pbname(id,par,*100)
      go to 200
100   par = ' '
      ind = 0
200   call mvcl(par,bpar,13)
      bpar(13) = 0
      return
      end

      subroutine xpbid(bpar,id,ind)
      byte bpar(13),bpar1(13)
      character*12 par
      logical cend
      cend = .false.
      ind = 1
c  trim trailing zero and any extra stuff:
      do i = 1,13
        bpar1(i) = bpar(i)
        if (bpar1(i).eq.0) cend = .true.
        if (cend) bpar1(i) = 32		! space
      enddo
      call mvlc(bpar1,par,12)
      call pbid(par,id,*100)
      go to 200
100   ind = 0
200   return
      end

