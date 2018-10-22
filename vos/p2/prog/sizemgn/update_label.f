CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C If present, update map history and property labels to reflect change in
C picture scale.
C
c	Label update errors do not abort size program
      subroutine update_label(iunit,ounit,sli,ssi,slo,sso,zooml,zooms)
      implicit none
      include 'mp_for_defs' 
      integer*4 iunit,ounit,sli,ssi,slo,sso
      real*4 zooml,zooms

      real*8 offs,offl,fscale,cl,cs,pcl,pcs,scale,res,radius,mp
      character*40 maptype
      integer istat
      logical xvptst
C
C     ....get map label data, if any (only if aspect ratio is constant)
      call mp_init(mp,istat)
      if (istat.lt.MP_SUCCESS) goto 900
      call mp_label_read( mp, iunit, istat)
      if (istat.lt. MP_SUCCESS) goto 901
      if (zooml.ne.zooms) goto 902
      call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',maptype,istat)
      if(istat.le.MP_FAILURE) goto 903

! when correcting Line/Sample items, recall that integer L/S
! values refer to pixel centers, whereas in a zoom only the
! top left corner (0.5,0.5) remains constant;  so, for
! X = Line or Sample and Z = Zoom:
!		X' = (X-0.5)*Z + 0.5
! If there is also an sub-area offset SX, then (SX-1) must
! be subtracted from X.

      if (maptype.ne.'POINT_PERSPECTIVE') goto 20
      call mp_get_value(mp,'FOCAL_PLANE_SCALE',fscale,istat)
      if (istat.gt.MP_FAILURE) then
         fscale = fscale*zooml	! camera scale (pix/mm)
         call mp_set_value(mp,'FOCAL_PLANE_SCALE',fscale,istat)
         if (istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting FOCAL_PLANE_SCALE',' ')
      else
         call xvmessage('error getting FOCAL_PLANE_SCALE',' ')
      endif

      call mp_get_value(mp,'OPT_AXIS_INTERCEPT_LINE',cl,istat)
      if (istat.gt.MP_FAILURE) then
         cl = zooml*(cl-sli+0.5) + slo - 0.5	! OAL
         call mp_set_value(mp,'OPT_AXIS_INTERCEPT_LINE',cl,istat)
         if (istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting OPT_AXIS_INTERCEPT_LINE',' ')
      else
         call xvmessage('error getting OPT_AXIS_INTERCEPT_LINE',' ')
      endif

      call mp_get_value(mp,'OPT_AXIS_INTERCEPT_SAMPLE',cs,istat)
      if (istat.gt.MP_FAILURE) then
         cs = zooml*(cs-ssi+0.5) + sso - 0.5	! OAS
         call mp_set_value(mp,'OPT_AXIS_INTERCEPT_SAMPLE',cs,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('error setting OPT_AXIS_INTERCEPT_SAMPLE',' ')
      else
         call xvmessage('error getting OPT_AXIS_INTERCEPT_SAMPLE',' ')
      endif

      call mp_get_value(mp,'PLANET_CENTER_LINE',pcl,istat)
      if (istat.gt.MP_FAILURE) then
         pcl = zooml*(pcl-sli+0.5) + slo - 0.5	! special line
         call mp_set_value(mp,'PLANET_CENTER_LINE',pcl,istat)
         if(istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting PLANET_CENTER_LINE',' ')
      else
         call xvmessage('error getting PLANET_CENTER_LINE',' ')
      endif

      call mp_get_value(mp,'PLANET_CENTER_SAMPLE',pcs,istat)
      if (istat.gt. MP_FAILURE) then
         pcs = zooml*(pcs-ssi+0.5) + sso - 0.5	! special sample
         call mp_set_value(mp,'PLANET_CENTER_SAMPLE',pcs,istat)
         if (istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting PLANET_CENTER_SAMPLE',' ')
      else
         call xvmessage('error getting PLANET_CENTER_SAMPLE',' ')
      endif
      goto 50

C     ....Here if not point-perspective
   20 call mp_get_value(mp,'SAMPLE_PROJECTION_OFFSET',offs,istat)
      if (istat.gt.MP_FAILURE) then
         offs = zooml*(offs-ssi+1.5) + sso - 1.5	! special sample
         call mp_set_value(mp,'SAMPLE_PROJECTION_OFFSET',offs,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('error setting SAMPLE_PROJECTION_OFFSET',' ')
      else
         call xvmessage('error getting SAMPLE_PROJECTION_OFFSET',' ')
      endif

      call mp_get_value(mp,'LINE_PROJECTION_OFFSET',offl,istat)
      if (istat .gt. MP_FAILURE) then
         offl = zooml*(offl-sli+1.5) + slo - 1.5	! special line
         call mp_set_value(mp,'LINE_PROJECTION_OFFSET',offl,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('error setting LINE_PROJECTION_OFFSET',' ')
      else
         call xvmessage('error getting LINE_PROJECTION_OFFSET',' ')
      endif

      call mp_get_value(mp,'MAP_SCALE',scale,istat)
      if (istat.gt. MP_FAILURE) then
         scale = scale/zooml		! map scale (km/pix)
      else
         call xvmessage ('***MAP_SCALE not found',' ')
         scale = -1. 
      endif

      call mp_get_value(mp,'MAP_RESOLUTION',res,istat)
      if (istat .gt. MP_FAILURE) then
         res = res*zooml		! map resolution (pix/deg)
      else
         call xvmessage('***MAP_RESOLUTION not found',' ')
         res = -1.
      endif

      if (scale.ge.0. .and. res.ge.0.) goto 40		!Go on if both are found
      if (scale.lt.0. .and. res.lt.0.) goto 40		!Go on if both missing

C     ....If scale or res is missing, compute it from the other
      call mp_get_value(mp,'A_AXIS_RADIUS',radius,istat)
      if (istat .le. MP_FAILURE) goto 50
      if (scale.lt.0.) then
         call xvmessage('Computing scale from resolution',' ')
         scale = 0.0174533*radius/res
      else
         call xvmessage('Computing resolution from scale',' ')
         res = 0.0174533*radius/scale
      endif

   40 if (scale.ge.0.) then
         call mp_set_value(mp,'MAP_SCALE',scale,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('***Err setting MAP_SCALE',' ')
      endif
      if (res.ge.0.) then
         call mp_set_value(mp,'MAP_RESOLUTION',res,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('***Err setting MAP_RESOLUTION',' ')
      endif

   50 call mp_label_write( mp,ounit,'PROPERTY',istat)
      if (istat.gt.MP_FAILURE) then
         call xvmessage('MAP property label updated',' ')
      else
         call xvmessage('***Err updating MAP property label',' ')
      endif
      call mp_label_write(mp,ounit,'HISTORY',istat)
      if (istat.gt. MP_FAILURE) then
         call xvmessage('MAP history label updated',' ')
      else
         call xvmessage('***Err updating MAP history label',' ')
      endif
      call mp_free(mp)
      return

  900 call xvmessage('err in mp_init,map label not found',' ')
      return
  901 if (XVPTST('DEBUG')) call xvmessage
     &     ('err reading MP labels or MP labels do not exist',' ')
      return
  902 call xvmessage('ASPECT change: MAP labels NOT updated',' ')
      return
  903 call xvmessage('error getting projection type',' ')
      return
      end
