C*****************************************************************************
C Unit test program TASTRCH.F for subroutine ASTRCH
C Ported to UNIX 11/10/1992
C*****************************************************************************
      include 'VICMAIN_FOR'
      subroutine main44
      integer*4 h(256),lowdn,highdn,nlev,i
      real*4 lper,hper

C Test the Fortan-callable subroutine
      do i=1,256
         h(i)=5
      end do
      lowdn=0
      highdn=0
      lper=25.0
      hper=25.0
      nlev=256
      call astrch(h,lowdn,highdn,lper,hper,nlev)
      call xvmessage('Testing Fortran-callable subroutine',' ')
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')
      call zia(h,256)
      h(256) = 100
      call astrch(h,lowdn,highdn,lper,hper,nlev)
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')

c Test the C-bridge....
      do i=1,256
         h(i)=5
      end do
      lowdn=0
      highdn=0
      lper=25.0
      hper=25.0
      nlev=256
      call tzastrch(h,lowdn,highdn,lper,hper,nlev)
      call xvmessage('Testing C-callable subroutine',' ')
      call xvmessage('Results should be identical',' ')   
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')
      call zia(h,256)
      h(256) = 100
      call tzastrch(h,lowdn,highdn,lper,hper,nlev)
      call prnt(4,1,lowdn,'   Low DN  =.')
      call prnt(4,1,highdn,'   High DN =.')
      return
      end
