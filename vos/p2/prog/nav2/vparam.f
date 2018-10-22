C==============================================================================
c prompt user for navvel parameters.
c common area zvp is filled.
C==============================================================================
      subroutine vparam(navmode)
      IMPLICIT NONE
      integer navmode	!0=no wind adjustment, 1=yes, 2=split errors

      include 'cpic.fin'
      include 'cpts.fin'

      common/zvp/nz,zvp(2,1000)
      integer nz
      real*4 zvp

      common/c1_coords/coords(2,500), rcoords(2, 500)
      real*4 coords, rcoords

      integer navmode0,id,mpv
      logical parmtst

      navmode0 = navmode
      id = mod(target_id,100)
      if (id.ne.99) goto 990
      if (target_id.ne.599.and.navmode.eq.0.and.npts.gt.0) goto 992
      call xvmessage(' Do you wish to split the pointing error',' ')
      call xvmessage(' between the two frames?',' ')
   10 call xvintract('QUERY',' Enter Y or N')
      if (parmtst('EXIT', mpv, 1)) return
      if (parmtst('Y', mpv, 1)) then
         navmode = 2
      else if (parmtst('N', mpv, 1)) then
         navmode = 1
      else
         goto 10
      endif

C     ....check if zonal velocity profile is to be used, 
C     ....only Voyager on Jupiter is valid
      if (project_id.ne.4 .or. target_id.ne.599) return
      if (nz.gt.0) goto 30		! already being used

      if (navmode0.eq.0.and.npts.gt.0) then
         call xvmessage(' Zonal velocity profile will be used to',' ')
         call xvmessage(' adjust for wind speed.',' ')
         goto 25
      endif

      call xvmessage(' Do you wish to use the zonal velocity profile?',
     +              ' ')
   20 call xvintract('QUERY',' Enter Y or N')
      if (parmtst('N', mpv, 1)) return
      if (.not.parmtst('Y', mpv, 1)) goto 20

   25 call jupiter_zvp(zvp,nz)

c     ....get zonal velocities for all existing tiepoints
   30 if (navmode.eq.1.and.npts.gt.0) then
         call getlatlon(2,npts,rpt_os,coords)
         call getzv(npts,coords,u,v)
      endif
      return

  990 call xvmessage('***Wind speed adjustment valid only for',' ')
      call xvmessage(' ***planets with atmospheres',' ')
      goto 999
  992 call xvmessage('***Wind speed adjustment must be selected',' ')
      call xvmessage('***upon entry to the program.',' ')
  999 return
      end

C==============================================================================
c  get user command:
c  upon return, parmtst=.true. if user has entered the command "parm",
c     =.false. otherwise.
c  if the command is of the form keyword=value, then the values are
c  stored in array nn if count is 2 or 3.
c  all args must be passed but set count to number of args desired 
C==============================================================================
      logical function parmtst(parm,nn,count)
      IMPLICIT NONE
      character*(*) parm
      integer nn(1),count

      integer n(10),cnt,def

      parmtst = .false.
      if (count .eq. 1) then
         call xviparm(parm,n,cnt,def,' ')
      else 
         call xviparm(parm,nn,cnt,def,' ')
      endif
      if (def .eq. 0) parmtst = .true.
      return
      end

C==============================================================================
c sends message to user, prompting for a string of length length.
c the string is returned in upper-case and blank-filled on the right.
c ind=1 for success, =0 if cancelled by user.
C==============================================================================
      subroutine getstring(message,length,string,ind)
      IMPLICIT NONE
      character*(*) message
      integer length
      character*(*) string
      logical parmtst

      integer ind, mpv, icnt, idef

   10 call xvintract('STRING',message)
      if (parmtst('EXIT', mpv, 1)) then
         ind = 0
         return
      endif
      call xviparm('STRNG',string,icnt,idef,' ')
      if (idef.ne.0) goto 10
      call uprcase(string)
      call bfstr(string,length)
      ind = 1
      return
      end

C==============================================================================
c get the project, frame, and camera ids
c   img = input logical unit number for frame
c   lbuf = output buffer returned by getlabcon
C==============================================================================
      subroutine frameid(img,project,lbuf,frame_id,camera_id,
     &                   project_id,ind)
      IMPLICIT NONE
      character*5 project
      integer img,lbuf(80),frame_id,camera_id,project_id,ind,status
      logical parmtst

      call getproj(img,project,camera_id,frame_id,ind)
      if (ind.eq.0) goto 12

   10 call xvmessage(' ***Unknown project ID',' ')
      call getstring('Enter project ID',5,project,ind)
      if (ind.eq.0) return

   12 if (project.eq.'MAR-9') then
         project_id = 1		!mariner 9
      else if (project.eq.'MAR10') then
         project_id = 2		!mariner 10
      else if (project.eq.'VIKOR') then
         project_id = 3		!viking orbitor
      else if (project.eq.'VGR-1' .or. project.eq.'VGR-2') then
	 project_id = 4		!voyager
      else if (project.eq.'GLL') then
         project_id = 5		!galileo
      else if (project.eq.'CASSI') then
         project_id = 6		!cassini
      else
         call xvmessage(' ***Invalid project ID',' ')
         goto 10
      endif

      call getlabcon(img,project,lbuf,ind)
      if (ind.eq.2 .or. lbuf(2).eq.-999) then
         call xvintract('IVALUE','Enter frame number')
         status=parmtst('VALUE',frame_id, 2)
      endif

      if (ind.eq.2 .or. lbuf(6).eq.-999) then
         if (project_id.eq.5) then
            camera_id = 1		!only one camera on galileo
         else
            call xvintract('IVALUE','Enter camera serial number')
            status=parmtst('VALUE',camera_id, 2)
         endif
      endif
      ind = 1
      return
      end
