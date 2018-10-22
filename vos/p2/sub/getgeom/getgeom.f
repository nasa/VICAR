C Returns in restab the GEOMA parameters required to geometrically correct
C an image.
C
      SUBROUTINE GETGEOM(UNIT,PROJECT,CAMERA,GEOM,restab,mestab,NAH,NAV,
     + ind)
      IMPLICIT NONE
      INTEGER*4 UNIT		!Unit number of input image
      CHARACTER*5 PROJECT	!GLL VGR-1 VGR-2 VIKOR MAR10 MAR-9
      INTEGER*4 CAMERA		!Camera serial number
      INTEGER*4 GEOM		!0=return nominals, 1=read from unit number
      REAL*4 RESTAB(2720)	!Returned GEOMA parameters
      Real*4 MESTAB(2720)	!Same buffer as RESTAB
      INTEGER*4 NAH,NAV		!Number of horizontal and vertical areas
      INTEGER*4 IND		!Return status: 0=normal, 1=error

      INTEGER*4 ICOL,MAXPTS
      REAL*4 rloc(404)
c===================================================================
      ind=0

C For CCD cameras, geometric paramters are not used.  Therefore, for these
C cameras, the project ID is returned in words 1-2 of Restab:
      If (Project.eq.'GLL  ' .or. Project.eq.'CASSI') then
          Call MVCL(Project, Restab, 5) 
          CALL MVE(4,1,CAMERA,restab(3),1,1)
         Return
      Endif

      Call Zia (restab,2720)
      If (GEOM.EQ.0) Go To 100
c
C  case 1: READ DISTORTION CORRECTION FILE
c
      ICOL = 4
      MAXPTS=680
      CALL IREAD_TIEPOINTS(unit,nah,nav, MAXPTS, restab(9),ICOL)

      If ( (nah+1)*(nav+1) .gt. MAXPTS) Then
         Call Xvmessage('GETGEOM: Geoma file too large',' ')
         ind = 1
         Return
      EndIf

      call mvcl('NAH     ', restab(1), 8)
      call mvcl('NAV     ', restab(4), 8)
      call mvcl('TIEPOINT', restab(7), 8)
      Call MVE(4,1,nah,Restab(3), 1,1)
      Call MVE(4,1,nav,Restab(6), 1,1)
      Return
c
C  *** case 2: NO GEOMA FILE, USE NOMINALS ***
100   Continue
c
c   Voyager nominals
c
      If (Project .eq. 'VGR-1' .or. Project.eq.'VGR-2') then
           If (camera .ge.4 .and. camera.le.7) then
              Call getres(rloc,camera)
              Call geomav(restab,camera,rloc)
           Else
              Call Xvmessage('GETGEOM: Improper VGR camera #',' ')
              ind=1
              Return
           Endif
c
c Mariner 9 nominals
c
      Else If (Project .eq. 'MAR-9') then
           If (camera.eq.1) then
              Call  MM71A(RESTAB)
           Else If(camera.eq.2) then
              Call  MM71B(RESTAB)
           Else
              Call Xvmessage('GETGEOM: Improper MAR-9 camera #', ' ')
              ind=1
              Return
           Endif
c
c Mariner 10 nominals
c
      Else If (project.eq.'MAR10') then
           If (camera.eq.1) then
              Call MVM73A(RESTAB)
           Else If (camera.eq.2) then
              Call MVM73B(RESTAB)
           Else
              Call Xvmessage('GETGEOM: Improper MAR10 camera #',' ')
              ind=1
              Return
           Endif
c
c Viking nominals
c
      Else  If (project.eq.'VIKOR') then
           If (camera.eq.7) then
              Call VOSN7(RESTAB)
           Else If (camera.eq.4) then
              Call VOSN4(RESTAB)
           Else If (camera.eq.8) then
              Call VOSN8(RESTAB)
           Else If (camera.eq.6) then
              Call VOSN6(RESTAB)
           Else
              Call Xvmessage('GETGEOM: Improper Viking camera #',' ')
              ind=1
              Return
           Endif
c
      Else
         Call Xvmessage('GETGEOM: Unknown flight project', ' ')
         ind=1
         Return
      Endif

      Call MVE(4,1,Restab(3), nah,1,1)
      Call MVE(4,1,Restab(6), nav,1,1)

      Return  
      End 
