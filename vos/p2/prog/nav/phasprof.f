CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute and output constant phase profile....
C
      SUBROUTINE PHASPROF(PIC,HPIC,NL,NS)
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'fortport'        ! defines int2byte
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE   ! 1=outer edge, 2=inner edge, 3=thin edge
      CHARACTER*1 RINGS(15)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      common/navdv2/xdw,xdb
      integer xdw,xdb
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CLIMIT/SRR,ERR,SLONR,ELONR
      REAL*4 SRR,ERR,SLONR,ELONR

      COMMON/CONST/PI,DTR,RTD

      INTEGER*4 d1,d2,d3,d4
      BYTE L1(4),L2(4),L3(4),L4(4)
c      EQUIVALENCE (L1,d1),(L2,d2),(L3,d3),(L4,d4)

      INTEGER*4 SAMP,S0,XX(2),YY(2)
      REAL X0,Y0,DX,DY

      CHARACTER*80 CMSG
      CHARACTER*132 MSG
      CHARACTER*60 OUTFILE
      LOGICAL PARMTST,XVIPTST
      LOGICAL XDIPOLYLINE,XST,XDCLOCATION,XDIFILL
      LOGICAL REDO,no_rtd

      doubleprecision csun0(3), csun(3), cnorm(3)
      doubleprecision ctrvec(3), widvec1(3), widvec2(3), corvec(3)
      doubleprecision ctrcrs(3)
      doubleprecision ctrmag,lenmag,lenang
      doubleprecision phaze, pha1, pha2, xincid, xemiss
      doubleprecision vdot, vnorm
      doubleprecision vec1(3), vec2(3)
      doubleprecision vecrot0(3), veccrs(3), vecrot1(3)
      doubleprecision vecsun(3)

      integer IILON, IIRAD, IIDN, IIINC, IIEMI, IIPHA, IILAST
      parameter (IILON = 1)
      parameter (IIRAD = IILON + 1)
      parameter (IIDN  = IIRAD + 1)
      parameter (IIINC = IIDN + 1)
      parameter (IIEMI = IIINC + 1)
      parameter (IIPHA = IIEMI  + 1)
      parameter (IILAST = IIPHA)
      REAL*4 WORK3(IILAST,5000)
      doubleprecision nominals( IILAST)

CBTCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      bmdn = int2byte(xdw)

CBTC
CBTC target-to-sc vector in camera coordinates
CBTC target-to-sun vector in camera coordinates
CBTC ***N.B. Assumes all scan-to-sun vectors are parallel i.e. distance to
CBTC         sun is large compared to area scanned
CBTC
CBTC ***N.B. NAV & SPICE rotation matrices are transposes of each other,
CBTC         so use mtxv with CM to convert EME50/J2k to camera
CBTC
      CALL GETRING(MRING)	! VRH change to work in Master plane 5/30/89
      call mtxv( cm, psun, csun0)
      call vscl( rsun, csun0, csun0)
      cnorm(1) = om(3,1)
      cnorm(2) = om(3,2)
      cnorm(3) = om(3,3)
CBTC
CBTC get image limits of display
      rllo = sl
      rslo = ss
      rlhi = (nlds - 1) / zoom + sl
      rshi = (nsds - 1) / zoom + ss

      REDO = .true.
      igotem = 0
      iindx = 0

      CALL XVMESSAGE('Scan along constant phase',' ')
      IF (MRING.EQ.1) THEN ! VRH added to indicate plane 5/30/89
          CALL XVMESSAGE('Reference plane is planet''s equator',' ')
      ELSE
          WRITE(CMSG,116) RINGS(MRING)
116       FORMAT(' Reference plane is that of ',A1,'-Ring')
          CALL XVMESSAGE(CMSG,' ')
      ENDIF

   10 CONTINUE

      CALL XVINTRACT('PHASPROF','Constant-Phase Profiling')
      IF (XVIPTST('EXIT')) RETURN
      IF (XVIPTST('HELP')) THEN  ! VRH add RL,LS option 5/30/89
         CALL XVMESSAGE('Enter ''Plot, OUtput=<filespec>,' 
     &     // ' ''CEnterpt, ''COrnerpt,',' ')
         CALL XVMESSAGE( '''PLot,''Dnplot, ''Longplot, ''RAdiplot, '
     &                // '''EMisplot, ''Inciplot, ''PHasplot', ' ')
         CALL XVMESSAGE('''C, ''H, CZoom=<IZ>, ''REFresh' 
     &     // ' STretch=(lo,hi), ''GErase, ', ' ')
         CALL XVMESSAGE('''EDit, GColor=<color>' 
     &     // ' (or ''<color>), or ''EXit',' ')
         GOTO 10
      ENDIF

CBTC check for graphics color parm GCOLOR
      call isetcolor

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.EQ.0) CALL RDISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 10

      IF (XVIPTST('EDIT')) THEN
        CALL EDITNAV(IMG,PIC,HPIC,NL,NS)
        XST = XDIFILL(IDEV,G,xdb)
        CALL GETRING(MRING)	! VRH change to work in Master plane 5/30/89
        call mtxv( cm, psun, csun0)
        call vscl( rsun, csun0, csun0)
        cnorm(1) = om(3,1)
        cnorm(2) = om(3,2)
        cnorm(3) = om(3,3)
        rllo = sl
        rslo = ss
        rlhi = (nlds - 1) / zoom + sl
        rshi = (nsds - 1) / zoom + ss
        igotem = 0
        REDO = .true.
        iindx = 0
        GOTO 10
      ENDIF	!'edit

      IF (XVIPTST('REFRESH')) THEN
         XST = XDIFILL(IDEV,G,xdb)
      ENDIF

CBTC cursor is centerpoint of scan, calculate phase and plot points

      IF ( XVIPTST('CENTERPT') 
     &  .or. ( XVIPTST('REFRESH') .and. (and(igotem,1) .eq. 1))
     &  ) THEN

        if ( XVIPTST('CENTERPT') ) then
          call cursor( rcline, rcsamp)
          call img2obj( rcline, rcsamp, ctrvec)

CBTC start ring-specific code
CBTC find rate of change along ring axis wrt camera vector, divide it into
CBTC   s/c Z distance below ring plane to get distance along camera 
CBTC   vector to ring plane
          xdist = vdot( ctrvec, cnorm)
          if ( xdist .ne. 0d0) xdist = -psc3(3) / xdist
CBTC end ring-specific code

          if ( xdist .le. 0d0) then
            call xvmessage( 'That point does not intersect target, '
     &                     // 'try again', ' ')
            igotem = 0
            goto 10
          endif

CBTC add planet-to-s/c & s/c-to-intersection vectors,
CBTC   result is planet-to-intersection vector

          call mtxv( om, psc3, vec1)
          call vscl( xdist, ctrvec, vec2)
          call vadd( vec1, vec2, vec1)

CBTC - convert to planet coordinates, use it to get rrad, rlon, rlat

          call mxv( om, vec1, vec2)
          rrad = vnorm( vec2)
          if ( rrad .gt. 0d0) then
            rlon = dmod(datan2(vec2(2),vec2(1))+rlora+2d0*pi,2d0*pi)
            if (MODEL .ne. 4 .and. vec2(3) .ne. 0d0) then
              rlat = geocen( dasin( vec2(3) / rrad), rlon)
            else
              rlat = 0d0
            endif
          else
            rlon= 0d0
            rlat = 0d0
          endif

CBTC - reverse, add to planet-sun vector, & vhat => intersection-to-sun unit vec

          call vminus( vec1, vec1)
          call vadd( csun0, vec1, csun)
CBTCEND NEW CODE 2
          call vhat( csun, csun)


CBTC check if ctrvec is at 0 or 180 phase

          call vcrss( csun, ctrvec, ctrcrs)
          call unorm( ctrcrs, ctrcrs, ctrmag)
          if ( ctrmag .eq. 0d0) then
            call xvmessage( '0/180 phase point chosen, try again', ' ')
            igotem = 0
            goto 10
          endif

CBTC N.B. phaze 180-<actual-phase>; same for pha2 & pha1 later

          phaze = dacos( vdot( ctrvec, csun))
          xincid = vdot( csun, cnorm)
          xemiss = - vdot( ctrvec, cnorm)

CBTC ensure incidence angle <= 90

          if ( xincid .lt. 0d0 .or. 
     &        (xincid .eq. 0d0 .and. xemiss .lt. 0d0)) then
            xincid = dacos( -xincid)
            xemiss = dacos( -xemiss)
          else
            xincid = dacos( xincid)
            xemiss = dacos( xemiss)
          endif
          WRITE(CMSG,'(a)') 
     &             'Nomimal Incidence, Emission & Phase angles (deg) '
     &          // 'at center point of scan are'
          CALL XVMESSAGE(CMSG,' ')
          WRITE (MSG(2:52),'(1pg17.9,2g17.9)') 
     &                     xincid*rtd, xemiss*rtd, 180d0 - phaze*rtd
          CALL XVMESSAGE(MSG(2:51),' ')
          REDO = .true.
          igotem = 1
        endif

CBTC GRAPHICS: 
CBTC calculate delrot, approx rotation of center vector around csun per 5 pixels
CBTC   - assume zscale >> sin(phaze) => 2*sin(delrot/2) ~ delrot
CBTC   - limit delrot to 10 degrees
CBTC rotate ctrvec around csun ~5 pixels at a time, plot points

        delrot = min( 10d0*dtr, 5d0 / (abs(sin(phaze))*zscale*zoom) )
        i = 1
        nrpts = 0
        rot = delrot
        dowhile (rot .lt. (180d0*dtr) .and. nrpts .le. 19998)
          call rotvecs2img( ctrvec, csun, rot, rllo, rslo, rlhi, rshi
     &                    , nrpts, rpts(1,nrpts+1))
          i = i + 1
          rot = i * delrot
        enddo
        call DRAWCURVE(rPTS,NrPTS,0)

CBTC drop through if this is a 'REFRESH i.e. not a 'CENTERPT

        if ( XVIPTST('CENTERPT') ) goto 10
      ENDIF	!'centerpt


CBTC corner point of scan wrt center point determines length & width of scan

      IF ( XVIPTST('CORNERPT') 
     &  .or. ( XVIPTST('REFRESH') .and. (and(igotem,2) .eq. 2))
     &  ) THEN

        if ( and(igotem,1) .ne. 1) then
          call xvmessage( 'Set the ''CENTERPT before the ''CORNERPT'
     &                  , ' ')
          goto 10
        endif

        if ( XVIPTST('CORNERPT') ) then

CBTC (1) determine length of scan from corner wrt scan center (ctrvec) 
CBTC   - convert cursor position to corner vector
CBTC   - test for 0/180 phase point
CBTC   - get half-length of scan in terms of rotation about sun vector by
CBTC     finding angle between center-cross-sun & length-cross-sun vectors
CBTC     - test for zero-length scan along the way

          call cursor( rxline, rxsamp)
          call img2obj( rxline, rxsamp, corvec)
          call vcrss( csun, corvec, vec1)
          call unorm( vec1, vec1, lenmag)
          if ( lenmag .eq. 0d0) then
            call xvmessage( '0/180 phase point chosen, try again', ' ')
            goto 10
          endif
          lenang = vdot( vec1, ctrcrs)
          if ( lenang .ge. 1d0) then
            call xvmessage( 'Zero-length scan chosen, try again', ' ')
            goto 10
          endif
          lenang = dacos(lenang)

CBTC (2) convert cursor position wrt scan center to width of scan
CBTC   - find phase angles of corner vector & its "mirror" about center vector
CBTC   - test for 0/180 phase point
CBTC   - rotate center vector around sun-cross-center vector to width vectors
          pha1 = dacos( vdot( csun, corvec))
          pha2 = 2 * phaze - pha1
          if ( min(pha1,pha2).le.0d0 .or. max(pha1,pha2).ge.PI) then
            call xvmessage( '0/180 phase point inside scan, try again'
     &                    , ' ')
            goto 10
          endif
          call rotvec( pha1-phaze, ctrvec, ctrcrs, widvec1)
          call rotvec( pha2-phaze, ctrvec, ctrcrs, widvec2)

          write( cmsg, '(a,1pg14.7, a, g14.7)') 'Phase range (deg) is '
     &         , 180d0 - max(pha1,pha2)*rtd, ' to '
     &         , 180d0 - min(pha1,pha2)*rtd
          call xvmessage( cmsg, ' ') 

          REDO = .true.
          igotem = 3
        endif

CBTC GRAPHICS:  put lines at end of scan to show length

        nrpts = 0
        call rotvecs2img( widvec1, csun, lenang
     &                    , -1d30, -1d30, 1d30, 1d30
     &                    , nrpts, rpts(1,nrpts+1))
        call rotvecs2img( widvec2, csun, lenang
     &                    , -1d30, -1d30, 1d30, 1d30
     &                    , nrpts, rpts(1,nrpts+1))
        YY(1) = ZOOM*(rpts(1,1)-SL) + 1.5
        XX(1) = ZOOM*(rpts(2,1)-SS) + 1.5
        YY(2) = ZOOM*(rpts(1,3)-SL) + 1.5
        XX(2) = ZOOM*(rpts(2,3)-SS) + 1.5
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
        YY(1) = ZOOM*(rpts(1,2)-SL) + 1.5
        XX(1) = ZOOM*(rpts(2,2)-SS) + 1.5
        YY(2) = ZOOM*(rpts(1,4)-SL) + 1.5
        XX(2) = ZOOM*(rpts(2,4)-SS) + 1.5
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

CBTC GRAPHICS:  put dot every 10 DISPLAY pixels to show width of scan

        delrot = min( 10d0*dtr
     &       , 1d1/(min(abs(sin(pha1)),abs(sin(pha2)))*zscale*zoom))

        i = 1
        nrpts = 0
        rot = delrot
        dowhile (rot .lt. (180d0*dtr) .and. nrpts .le. 19996)
          call rotvecs2img( widvec1, csun, rot
     &                    , rllo, rslo, rlhi, rshi
     &                    , nrpts, rpts(1,nrpts+1))
          call rotvecs2img( widvec2, csun, rot
     &                    , rllo, rslo, rlhi, rshi
     &                    , nrpts, rpts(1,nrpts+1))
          i = i + 1
          rot = i * delrot
        enddo
        call DRAWCURVE(rPTS,NrPTS,0)

CBTC drop through if this is a 'REFRESH i.e. not a 'CENTERPT

        IF ( XVIPTST('CORNERPT') ) GOTO 10

      endif	!'cornerpt

CBTC do the scan
CBTC - no scan unless we drop through 'edit, 'centerpt & 'cornerpt above

      if ( igotem .eq. 3 .and. redo) then
        call xvmessage( 'Scanning ...', ' ')
        delrot2 = 1d0 / zscale
        delrot = delrot2 / sin(phaze)
        n3 = min( 5000, max( 3, int(1 + lenang * 2 / delrot)))
        phadel = abs(pha1-pha2)
        n4 = max( 3, int( 1 + phadel / delrot2))
        if ( mod(n4,2) .eq. 0) n4 = n4 + 1
        n3a = 0
        do j=1,n3
          rot0 = ((j-1) * 2 * lenang) / (n3-1) - lenang
          call rotvec( rot0, ctrvec, csun, vecrot0)
          call vcrss( csun, vecrot0, veccrs)
          call vhat( veccrs, veccrs)
          sumlon = 0d0
          sumrad = 0d0
          sumdn  = 0d0
          sumpha = 0d0
          suminc = 0d0
          sumemi = 0d0
          n4a = 0
          do i=1,n4
            rot1 = ((i-1) * phadel) / (n4-1) - phadel / 2
            call rotvec( rot1, vecrot0, veccrs, vecrot1)
            call obj2img( vecrot1, rline, rsamp)

CBTC start ring-specific code
            xdist = vdot( vecrot1, cnorm)
            if ( xdist .ne. 0d0) xdist = -psc3(3) / xdist
CBTC end ring-specific code

            if ( xdist .le. 0d0) goto 00058

CBTC add planet-to-s/c & s/c-to-intersection vectors,
CBTC   result is planet-to-intersection vector

            call mtxv( om, psc3, vec1)
            call vscl( xdist, vecrot1, vec2)
            call vadd( vec1, vec2, vec1)

CBTC - convert to planet coordinates, use it to get rrad, rlon, rlat

            call mxv( om, vec1, vec2)
            rrad = vnorm( vec2)
            if ( rrad .gt. 0d0) then
              rlon = dmod(datan2(vec2(2),vec2(1))+rlora+2d0*pi,2d0*pi)
              if (MODEL .ne. 4 .and. vec2(3) .ne. 0d0) then
                rlat = geocen( dasin( vec2(3) / rrad), rlon)
              else
                rlat = 0d0
              endif
            else
              rlon= 0d0
              rlat = 0d0
            endif

CBTC - reverse, add to planet-sun vector, & vhat => intersection-to-sun unit vec

            call vminus( vec1, vec1)
            call vadd( csun0, vec1, vecsun)

CBTCEND NEW CODE 3

            call vhat( vecsun, vecsun)
            vecpha = dacos( vdot( vecrot1, vecsun))
            vecinc = vdot( vecsun, cnorm)
            vecemi = - vdot( vecrot1, cnorm)
            if ( vecinc .lt. 0d0 .or. 
     &          (vecinc .eq. 0d0 .and. vecemi .lt. 0d0)) then
              vecinc = dacos( -vecinc)
              vecemi = dacos( -vecemi)
            else
              vecinc = dacos( vecinc)
              vecemi = dacos( vecemi)
            endif
C             ....Compute DN value at (rline,rsamp) via bilinear interpolation
            IL = RLINE
            IS = RSAMP
            IF (IL.LT.1.OR.IL.GE.NL) GOTO 58
            IF (IS.LT.1.OR.IS.GE.NS) GOTO 58
            x = RLINE - IL
            y = RSAMP - IS
            IF (ICODE.EQ.1) THEN
              L1(1) = PIC(IS,IL)
              L2(1) = PIC(IS+1,IL)
              L3(1) = PIC(IS,IL+1)
              L4(1) = PIC(IS+1,IL+1)
              d1 = byte2int(l1(1))
              d2 = byte2int(l2(1))
              d3 = byte2int(l3(1))
              d4 = byte2int(l4(1))
            ELSE
              d1 = HPIC(IS,IL)
              d2 = HPIC(IS+1,IL)
              d3 = HPIC(IS,IL+1)
              d4 = HPIC(IS+1,IL+1)
            ENDIF
            dn = d1 + (d2-d1)*x + (d3-d1)*y + (d1-d2-d3+d4)*x*y
            sumrad = sumrad + rrad
            sumlon = sumlon + rlon
            sumdn  = sumdn  + dn
            suminc = suminc + (vecinc-xincid)
            sumemi = sumemi + (vecemi-xemiss)
CBTC reverse phase delta because phaze & vecpha are calculated using reverse 
CBTC emission vector
            sumpha = sumpha + (phaze-vecpha)
            n4a = n4a + 1
00058         continue
          enddo	!i=1,n4
          if ( n4a .gt. 0) then
            work3(IILON,j) = rtd * sumlon / n4a
            work3(IIRAD,j) = sumrad / n4a
            work3(IIDN ,j) = sumdn  / n4a
            work3(IIINC,j) = rtd * suminc / n4a
            work3(IIEMI,j) = rtd * sumemi / n4a
            work3(IIPHA,j) = rtd * sumpha / n4a
            n3a = n3a + 1
          else
            work3(IILON,j) = -999e10
          endif
        enddo	!j=1,n3
        if ( n3a .lt. n3) then
          msg(1:80) = ' '
CBTC             12345678901234567890123456789012345678901234567890
          msg = '*** Warning:  x234 out of x234 scanpoint'
     &       // 's are outside image/geometry limits'
          write(msg(15:18),'(i4)') n3-n3a
          write(msg(27:30),'(i4)') n3
          if ( (n3-n3a) .eq. 1) msg(43:45) = ' is'
          call xvmessage( msg(1:76), ' ')
        endif
        redo = .false.

      endif	!igotem.eq.3 .and. redo

CBTC determine PLOT type, default to DNplot

      if ( .not.redo) then
        if ( xviptst('plot') .or. xviptst('dnplot')) then
          iindx = iidn
        elseif ( xviptst('longplot')) then
          iindx = iilon
        elseif ( xviptst('radiplot')) then
          iindx = iirad
        elseif ( xviptst('emisplot')) then
          iindx = iiemi
        elseif ( xviptst('inciplot')) then
          iindx = iiinc
        elseif ( xviptst('phasplot')) then
          iindx = iipha
        endif
      endif

CBTC do the plot

      if ( (iindx .gt. 0 .and. .not.redo) .or. 
     &     (iindx .lt. 0 .and. xviptst('refresh')) ) then
        iindx = -abs(iindx)
        do j=1,IILAST
          nominals(j) = 0d0
        enddo
        nominals(IIINC) = xincid * rtd
        nominals(IIEMI) = xemiss * rtd
        nominals(IIPHA) = 180d0 - phaze * rtd
        nrpts = 0
        do j=1,n3
          if ( work3(IILON,j) .ne. -999e10) then
            nrpts = nrpts + 1
            rpts(1,nrpts) = j
            rpts(2,nrpts) = work3(-iindx,j) + nominals( -iindx)
          endif
        enddo
        YY(1) = ZOOM*(RcLINE-SL) + 1.5
        XX(1) = ZOOM*(RcSAMP-SS) + 1.5
        IF (nrpts.LE.0) THEN
          CALL XVMESSAGE('*** Error, nothing to plot',' ')
          GOTO 10
        ELSE IF (nrpts.GT.1000) THEN
          S0 = 19
        ELSE
          S0 = 19
          IF (XX(1).LE.256) S0=271
        ENDIF
        L0 = 237
        IF (YY(1).LE.256) L0=493
        no_rtd = .true.
        CALL PRDISPLAY(rpts,nrpts,no_rtd,L0,S0,X0,Y0,DX,DY)
      endif

      IF ( XVIPTST('CP') .and. n3a.gt.0 
     &     .and. .not.redo .and. iindx.lt.0) THEN
   12    XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
CBTC            123456789012345678901234567890123456789012
         msg = 'PHASE EMISS INCID DN    RADIUSLONGIT'
         WRITE(CMSG,115) LINE,SAMP
     &                 , msg(37+iindx*6:42+iindx*6)
     &                 , X0+DX*FLOAT(SAMP-S0), Y0+DY*FLOAT(LINE-L0)
  115    FORMAT('(L,S)=(',I4,',',I4,') (PIXEL,',a5,')=(',
     &         1pg15.7,',',g15.7,')')
         CALL XVMESSAGE(CMSG,' ')
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 12
         GOTO 10
      ENDIF

CBTC 'OUTPUT to file

      IF ( PARMTST('OUTPUT',OUTFILE,I) .and. igotem.eq.3 .and.
     &     .not.redo) THEN

CBTC
CBTC Open file, write out file header info

        OPEN(1,FILE=OUTFILE,STATUS='NEW',ERR=994)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' 10 x234 4 - first & last lines of data,'
     &     // ' line with nominal inc, emi, phase'
        WRITE (MSG(5:8),'(I4)') n3a + 9
        WRITE(1,113) (MSG(I:I),I=1,74)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Constant-phase plot of frame '
        WRITE (MSG(31:40),'(I10)') FRAME_ID
        CALL XVMESSAGE(MSG(2:40),' ')
        WRITE(1,113) (MSG(I:I),I=1,40)
  113   FORMAT(80A1)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Nominal Incidence, Emission, & Phase an'
     &     // 'gles (degrees, at center point):'
        CALL XVMESSAGE(MSG(2:72),' ')
        WRITE(1,113) (MSG(I:I),I=1,72)
        MSG(1:80) = ' '
CBTC
        WRITE (MSG(2:52),'(1pg17.9,2g17.9)') 
     &                     xincid*rtd, xemiss*rtd, 180d0 - phaze*rtd
        CALL XVMESSAGE(MSG(2:51),' ')
        WRITE(1,113) (MSG(I:I),I=1,52)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Scan''s phase angle width is               degrees'
        WRITE (MSG(30:44),'(1pg15.7)') abs(pha1-pha2)*rtd
        CALL XVMESSAGE(MSG(2:44),' ')
        WRITE(1,113) (MSG(I:I),I=1,44)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Photometric angles in table below are d'
     &     // 'eltas from nominal'
        CALL XVMESSAGE(MSG(2:59),' ')
        WRITE(1,113) (MSG(I:I),I=1,59)
        MSG(1:80) = ' '
CBTC               123456789012345678901234567890123456789012345678901234567890
        IF (MRING.EQ.1) THEN
            MSG = ' Reference plane is planet''s equator'
        ELSE
            WRITE(CMSG,116) RINGS(MRING)
            MSG(1:34) = CMSG(1:34)
        ENDIF
CBTC        CALL XVMESSAGE(MSG(2:36),' ')
        WRITE(1,113) (MSG(I:I),I=1,36)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' INDX    LONG     RADIUS       DN       '
     &     // '   dINCID         dEMISS          dPHASE'
        WRITE(1,113) (MSG(I:I),I=1,80)
        MSG = ' ----    ----     ------       --       '
     &     // '   ------         ------          ------'
        WRITE(1,113) (MSG(I:I),I=1,80)
        MSG(1:80) = ' '
        DO J=1,N3
          if ( work3(IILON,j) .ne. -999e10) then
            WRITE (MSG(2:5),'(i4)') j
            WRITE (MSG(7:13),'(F7.2)') WORK3(IILON,J)
            WRITE (MSG(15:24),'(I10)') int(WORK3(IIRAD,J))
            WRITE (MSG(26:33),'(f8.2)') WORK3(IIDN,J)
            WRITE (MSG(35:49),'(1pg15.7)') WORK3(IIINC,J)
            WRITE (MSG(51:64),'(1pg14.7)') WORK3(IIEMI,J)
            WRITE (MSG(66:80),'(1pg15.7)') WORK3(IIPHA,J)
            WRITE(1,113) (MSG(I:I),I=1,80)
          endif
        ENDDO
        CLOSE(1,ERR=995)
        CALL XVMESSAGE('Written to disc',' ')
        GOTO 10
  994   CALL XVMESSAGE('***Error opening output file',' ')
        GOTO 10
  995   CALL XVMESSAGE('***Error closing output file',' ')
        GOTO 10
      ENDIF	!'output

      GOTO 10
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute and output radial or longitudinal profile....
C
      SUBROUTINE PROFILE(PIC,HPIC,NL,NS)
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'fortport'        ! defines int2byte
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE   ! 1=outer edge, 2=inner edge, 3=thin edge
      CHARACTER*1 RINGS(15)

      REAL*4 WORK3(2,5000),rl4,rs4,mindis,maxdis

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      common/navdv2/xdw,xdb
      integer xdw,xdb
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CLIMIT/SRR,ERR,SLONR,ELONR
      REAL*4 SRR,ERR,SLONR,ELONR,par(2),WLONR

      COMMON/CONST/PI,DTR,RTD

      INTEGER*4 d1,d2,d3,d4
      BYTE L1(4),L2(4),L3(4),L4(4)
c      EQUIVALENCE (L1,d1),(L2,d2),(L3,d3),(L4,d4)

      INTEGER*4 SAMP,S0,XX(2),YY(2),SDIST
      REAL X0,Y0,DX,DY

      CHARACTER*1 PTYPE
      CHARACTER*80 CMSG
      CHARACTER*132 MSG
      CHARACTER*60 OUTFILE
      LOGICAL PARMTST,rPARMTST,XVIPTST
      LOGICAL XDIPOLYLINE,XST,XDCLOCATION,XDIFILL
      LOGICAL RPLOT,REDO,DOPHA
      DATA RPLOT/.TRUE./
      LOGICAL DPLOT,SPLOT

      bmdn = int2byte(xdw)

      CALL GETRING(MRING)	! VRH change to work in Master plane 5/30/89
      REDO = .TRUE.

   10 IF (RPLOT) THEN
         IF(DPLOT) THEN
           CALL XVMESSAGE('Diagonal Radial scan',' ')
           CALL XVMESSAGE(
     &     '*** Pay attention to width at both ends ***',' ')
         ELSE
           CALL XVMESSAGE('Radial scan',' ')
         ENDIF
      ELSE
         IF(SPLOT) THEN
           CALL XVMESSAGE('Longitudinal smear scan',' ')
         ELSE
           CALL XVMESSAGE('Longitudinal scan',' ')
         ENDIF
      ENDIF
      IF (MRING.EQ.1) THEN ! VRH added to indicate plane 5/30/89
          CALL XVMESSAGE('Reference plane is planet''s equator',' ')
      ELSE
          WRITE(CMSG,116) RINGS(MRING)
116       FORMAT(' Reference plane is that of ',A1,'-Ring')
          CALL XVMESSAGE(CMSG,' ')
      ENDIF
      IF(.NOT.SPLOT) THEN
         IF (SR.NE.0..OR.ER.NE.0.) THEN  ! VRH bug fix 5/30/89
            WRITE(CMSG,111) SR,ER
111         FORMAT(' Radius limits: ',F9.1,',',F9.1,' (km)')
            CALL XVMESSAGE(CMSG,' ')
         ELSE
            CALL XVMESSAGE('No radius limits',' ')
         ENDIF
      ELSE
         IF(RCENT.NE.0) THEN
           WRITE(CMSG,119) SMANG*RTD
119        FORMAT('Smear direction: ',F7.2,' (degrees)')
           CALL XVMESSAGE(CMSG,' ')
           WRITE(CMSG,120) SDIST
120        FORMAT('Smear length: ',I4,' (pixels)')
           CALL XVMESSAGE(CMSG,' ')
           WRITE(CMSG,121) RCENT
121        FORMAT('Center Radius of scan: ',F9.1,' (km)')
           CALL XVMESSAGE(CMSG,' ')
         ELSE
           CALL XVMESSAGE('Smear not yet defined',' ')
         ENDIF
      ENDIF
      IF (SLON.NE.0..OR.ELON.NE.0.) THEN  ! VRH bug fix 5/30/89
         IF(DPLOT) THEN
           WRITE(CMSG,117) SLON*RTD,ELON*RTD
117        FORMAT('Center longitudes: ',F7.2,',',F7.2,' (degrees)')
         ELSE
           WRITE(CMSG,114) SLON*RTD,ELON*RTD
114        FORMAT('Longitude limits: ',F7.2,',',F7.2,' (degrees)')
         ENDIF
         CALL XVMESSAGE(CMSG,' ')
      ELSE
         CALL XVMESSAGE('No longitude limits',' ')
      ENDIF
      IF(DPLOT) THEN
         IF (WLON.NE.0.D0) THEN
           WRITE(CMSG,118) WLON*RTD
118        FORMAT('Longitude width: ',F7.2,' (degrees)')
           CALL XVMESSAGE(CMSG,' ')
         ELSE
           CALL XVMESSAGE('No longitude width',' ')
         ENDIF
      ENDIF

      CALL XVINTRACT('PROFILE','PROFILE')
      IF (XVIPTST('EXIT')) RETURN
      IF (XVIPTST('HELP')) THEN  ! VRH add RL,LS option 5/30/89
         CALL XVMESSAGE('Enter TYPE, RLIMITS, LLIMITS,' //
     .        ' ''PLOT, OUTPUT, ''PHASE, ''CP',' ')
         CALL XVMESSAGE('RL=(rad,long), LS=<line,samp>, ''C, ''H, '
     &        // 'CZOOM=<iz>, STRETCH=(l,h), ''GERASE, or ''EXIT',' ')
         GOTO 10
      ENDIF

CBTC check for graphics color parm GCOLOR
      call isetcolor

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.EQ.0) CALL RDISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 10

      IF (PARMTST('TYPE',PTYPE,I)) THEN
        REDO = .TRUE.
        RPLOT = .TRUE.
        DPLOT = .FALSE.
        SPLOT = .FALSE.
        WLON = 0.D0
        IF (PTYPE.EQ.'L'.OR.PTYPE.EQ.'l') RPLOT = .FALSE.
        IF (PTYPE.EQ.'D'.OR.PTYPE.EQ.'d') DPLOT = .TRUE.
        IF (PTYPE.EQ.'S'.OR.PTYPE.EQ.'s') THEN
            RPLOT = .FALSE.
            SPLOT = .TRUE.
   20       CALL XVMESSAGE('Specify smear direction:',' ')
            CALL XVMESSAGE('Move Cursor on one side of profile',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT to skip')
            IF (XVIPTST('EXIT')) GOTO 30
            XST = XDIFILL(IDEV,G,XDB)
            CALL CURSOR(RLINE,RSAMP)
            YY(1) = ZOOM*(RLINE-SL) + 1.5
            XX(1) = ZOOM*(RSAMP-SS) + 1.5
            XST = XDIPOLYLINE(IDEV,G,bmdn,1,XX,YY)
C
   21       CALL XVMESSAGE('Move Cursor to other side of profile',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT if done')
            IF (XVIPTST('EXIT')) GOTO 30
            CALL CURSOR(RLINE2,RSAMP2)
            IF((RLINE2-RLINE).EQ.0..AND.(RSAMP2-RSAMP).EQ.0.) THEN
              CALL XVMESSAGE(
     &           'Start and end points are same, try again',' ')
              GOTO 21
            ELSE
              SMANG = ATAN2((RLINE2 - RLINE),(RSAMP2 - RSAMP))
            END IF
            XST = XDIFILL(IDEV,G,xdb)
            YY(2) = ZOOM*(RLINE2-SL) + 1.5
            XX(2) = ZOOM*(RSAMP2-SS) + 1.5
            XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
            WRITE(CMSG,119) SMANG*RTD
            CALL XVMESSAGE(CMSG,' ')
            GOTO 21

   30       CALL XVMESSAGE('Specify smear length:',' ')
            CALL XVMESSAGE('Move Cursor on one endpoint of smear',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT to skip')
            IF (XVIPTST('EXIT')) GOTO 32
            CALL CURSOR(RLINE,RSAMP)
C
   31       CALL XVMESSAGE('Move Cursor to other endpoint of smear',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT if done')
            IF (XVIPTST('EXIT')) GOTO 32
            CALL CURSOR(RLINE2,RSAMP2)
            SDIST = NINT(DSQRT((RLINE2-RLINE)**2 + (RSAMP2-RSAMP)**2))
            CENTLINE = (RLINE2 + RLINE)/2.D0
            CENTSAMP = (RSAMP2 + RSAMP)/2.D0
            CALL LATLON(IND,CENTLINE,CENTSAMP,RCENT,RLON)
            WRITE(CMSG,120) SDIST
            CALL XVMESSAGE(CMSG,' ')
            WRITE(CMSG,121) RCENT
            CALL XVMESSAGE(CMSG,' ')
            GOTO 31
   32       CONTINUE
        ENDIF
      ENDIF

      IF (rPARMTST('LLIMITS',par,I)) THEN
	slonr = par(1)
	elonr = par(2)
         IF (SLONR.GT.ELONR) THEN
            CALL XVMESSAGE('*** Error, Longitude limits in wrong order',
     .                     ' ')
            SLON = 0.
            ELON = 0.
         ELSE
            SLON = DBLE(SLONR)*DTR
            ELON = DBLE(ELONR)*DTR
         ENDIF
         REDO = .TRUE.
      ENDIF

      IF (rPARMTST('RLIMITS',par,I)) THEN
	srr = par(1)
	err = par(2)
         IF (SRR.GT.ERR) THEN
           CALL XVMESSAGE('*** Error, Radius limits in wrong order',' ')
            SR = 0.
            ER = 0.
         ELSE
            SR = DBLE(SRR)
            ER = DBLE(ERR)
         ENDIF
         REDO = .TRUE.
      ENDIF

      IF (rPARMTST('LWIDTH',wlonr,I)) THEN
         WLON = DBLE(WLONR)*DTR
         REDO = .TRUE.
      ENDIF

      IF (REDO) THEN
         IF (.NOT.SPLOT.AND.SR.EQ.0.D0.AND.ER.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, no radius limits',' ')
            GOTO 10
         ENDIF
         IF (SLON.EQ.0.D0.AND.ELON.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, no longitude limits',' ')
            GOTO 10
         ENDIF
         IF(DPLOT.AND.WLON.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, no longitude width',' ')
            GOTO 10
         ENDIF
         IF(SPLOT.AND.RCENT.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, smear not defined',' ')
            GOTO 10
         ENDIF
         XST = XDIFILL(IDEV,G,xdb)
         AVGLON = (SLON+ELON)/2.D0
         IF(SPLOT) THEN
            AVGR = RCENT
            CALL LINSAM(IND,RCENT,SLON,SL1,SS1)
            CALL LINSAM(IND,RCENT,ELON,SL2,SS2)
         ELSE
            AVGR = (SR+ER)/2.D0
            CALL LINSAM(IND,SR,SLON,SL1,SS1)
            CALL LINSAM(IND,SR,ELON,SL2,SS2)
            CALL LINSAM(IND,ER,SLON,SL3,SS3)
            CALL LINSAM(IND,ER,ELON,SL4,SS4)
         ENDIF
      ENDIF

      IF(XVIPTST('PLOT').OR.REDO) THEN  ! Draw sector
	mindis = 0.6
	maxdis = 1.2
        IF(DPLOT) THEN

         YY(1) = ZOOM*(SL1-SL) + 1.5
         XX(1) = ZOOM*(SS1-SS) + 1.5
         YY(2) = ZOOM*(SL4-SL) + 1.5
         XX(2) = ZOOM*(SS4-SS) + 1.5
         XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

C        ....Establish longitudinal scale
         CALL RINGPT2(SR,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &        OAL,OAS,ZSCALE,1,1,NL,NS,  !VRH 8/12/89 fix start from (1,1)
     &        mindis,maxdis,SLON-0.5*WLON,SLON+0.5*WLON,RPTS,NPTS1)
         IF (NPTS1.GT.0) CALL DRAWCURVE(RPTS,NPTS1,0)

         CALL RINGPT2(ER,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &        OAL,OAS,ZSCALE,1,1,NL,NS,  !VRH 8/12/89 fix
     &        mindis,maxdis,ELON-0.5*WLON,ELON+0.5*WLON,RPTS,NPTS2)
         IF (NPTS2.GT.0) CALL DRAWCURVE(RPTS,NPTS2,0)

        ELSE
         IF(SPLOT) THEN
           YY(1) = ZOOM*(SL1-SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(1) = ZOOM*(SS1-COS(SMANG)*SDIST/2.-SS) + 1.5
           YY(2) = ZOOM*(SL1+SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(2) = ZOOM*(SS1+COS(SMANG)*SDIST/2.-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

           YY(1) = ZOOM*(SL2-SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(1) = ZOOM*(SS2-COS(SMANG)*SDIST/2.-SS) + 1.5
           YY(2) = ZOOM*(SL2+SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(2) = ZOOM*(SS2+COS(SMANG)*SDIST/2.-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

C          ....Establish longitudinal scale
           CALL RINGPT2(RCENT,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &          OAL,OAS,ZSCALE,1,1,NL,NS,  !VRH 8/12/89 fix start from (1,1)
     &          mindis,maxdis,SLON,ELON,RPTS,NPTS1)
           IF (NPTS1.GT.0) CALL DRAWCURVE(RPTS,NPTS1,0)
           NPTS2 = 0
         ELSE
           YY(1) = ZOOM*(SL1-SL) + 1.5
           XX(1) = ZOOM*(SS1-SS) + 1.5
           YY(2) = ZOOM*(SL3-SL) + 1.5
           XX(2) = ZOOM*(SS3-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

           YY(1) = ZOOM*(SL2-SL) + 1.5
           XX(1) = ZOOM*(SS2-SS) + 1.5
           YY(2) = ZOOM*(SL4-SL) + 1.5
           XX(2) = ZOOM*(SS4-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

C          ....Establish longitudinal scale
           CALL RINGPT2(SR,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &          OAL,OAS,ZSCALE,1,1,NL,NS,  
     &          mindis,maxdis,SLON,ELON,RPTS,NPTS1)
           IF (NPTS1.GT.0) CALL DRAWCURVE(RPTS,NPTS1,0)

           CALL RINGPT2(ER,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &          OAL,OAS,ZSCALE,1,1,NL,NS,  
     &          mindis,maxdis,SLON,ELON,RPTS,NPTS2)
           IF (NPTS2.GT.0) CALL DRAWCURVE(RPTS,NPTS2,0)
        ENDIF
       ENDIF
      ENDIF

      IF (REDO) THEN
         NPTS = MAX0(NPTS1,NPTS2)
         IF (NPTS.EQ.0) THEN
            CALL XVMESSAGE('***Sector lies outside image',' ')
            RETURN
         ENDIF
         DLON = (ELON-SLON)/NPTS
         SCALE = DABS(DLON)
         WRITE(CMSG,112) SCALE*RTD
  112    FORMAT('Average longitudinal resolution is ',F10.4,
     &     ' degrees/pixel')
         CALL XVMESSAGE(CMSG,' ')

        IF(.NOT.SPLOT) THEN
C         ....Establish maximum radial resolution (km/pixel) in sector
C         ....Go across scan to find maximum pixel width    
	  RLON = SLON
          IF(DPLOT) RLON = RLON - 0.5*WLON
	  D1 = 0
          NPTS1 = 0
40        CALL LINSAM(IND,SR,RLON,SL5,SS5)
          IF(DPLOT) THEN
            CALL LINSAM(IND,ER,RLON+(ELON-SLON),SL6,SS6)
          ELSE
            CALL LINSAM(IND,ER,RLON,SL6,SS6)
          ENDIF
	  NPTS1 = NPTS1 + 1 
          IF (NPTS1.GT.5000) THEN
            CALL XVMESSAGE('***Maximum number of ring points exceeded',
     &                   ' ')
            GOTO 45
          ENDIF
          RPTS(1,NPTS1) = DSQRT((SL6-SL5)**2 + (SS6-SS5)**2)
          D1 = MAX(D1,INT(RPTS(1,NPTS1)+0.99))
	  RLON = RLON + DLON
          IF(DPLOT.AND.(RLON.LT.SLON+0.5*WLON)) GOTO 40
          IF(.NOT.DPLOT.AND.(RLON.LT.ELON)) GOTO 40

45        DR = (ER-SR)/D1
          WRITE(CMSG,110) DABS(DR)
  110     FORMAT('Maximum radial resolution is ',F10.2,' km/pixel')
          CALL XVMESSAGE(CMSG,' ')
        ENDIF

        IF (DPLOT) THEN ! RPTS = R,LON along diagonal line
          DIST = DSQRT((SL4-SL1)**2 + (SS4-SS1)**2)
          RPTS(1,1) = SR
          RPTS(2,1) = SLON
          DO I=2,INT(DIST)
            RLINE = (SL4-SL1)*FLOAT(I-1)/DIST + SL1
            RSAMP = (SS4-SS1)*FLOAT(I-1)/DIST + SS1
            CALL LATLON(IND,RLINE,RSAMP,R,RLON)
            RPTS(1,I) = R
            RPTS(2,I) = RLON
          END DO
          RPTS(1,INT(DIST)+1) = ER
          RPTS(2,INT(DIST)+1) = ELON
        ENDIF

        J = 0
        I = 0
        IF (RPLOT) THEN
           R = SR
        ELSE
           RLON = SLON
        ENDIF

   50   I = I + 1
        IF (.NOT.RPLOT.AND.RPTS(1,I).LT.1.0) THEN
           RLON = RLON + DLON
           IF (RLON.LE.ELON) GOTO 50
           GOTO 59
        END IF
	IF (RPLOT) THEN
           RLON = SLON
           IF (DPLOT) THEN ! Calculate Center longitude from RPTS
              DO K=1,INT(DIST)
                IF(R.GE.RPTS(1,K).AND.R.LE.RPTS(1,K+1)) GOTO 51
              END DO
   51         CENTL = (RPTS(2,K+1)-RPTS(2,K))*(R-RPTS(1,K))
              CENTL = CENTL/(RPTS(1,K+1)-RPTS(1,K)) + RPTS(2,K)
              RLON = CENTL - 0.5*WLON
           ENDIF
        ELSE
           R = SR
           IF(SPLOT) R = RCENT
        ENDIF
        SUMDN = 0.D0	!Sum of all dn's at radius R or longitude
        d1 = 0
        d2 = 0
        d3 = 0
        d4 = 0
        NPTS = 0		!Number of pixels summed
C       ....Convert from (R,RLON) to (rline,rsamp) coordinates 
   55   CALL RINV(IND,R,RLON,RLINE,RSAMP,OM,PSC3,RLORA,OAL,OAS,ZSCALE)
        IF (ITYPE.EQ.7) then
	  CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(RLINE),sngl(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	  rline=rl4
	  rsamp=rs4
	endif

   56   IF (SPLOT) THEN
          IF (NPTS.EQ.0) THEN
            RLINE = RLINE - SIN(SMANG)*SDIST/2.
            RSAMP = RSAMP - COS(SMANG)*SDIST/2.
          ELSE
            RLINE = RLINE + SIN(SMANG)
            RSAMP = RSAMP + COS(SMANG)
          ENDIF
        ENDIF

C       ....Compute DN value at (rline,rsamp) via bilinear interpolation
        IL = RLINE
        IS = RSAMP
        IF (IL.LT.1.OR.IL.GE.NL) GOTO 58
        IF (IS.LT.1.OR.IS.GE.NS) GOTO 58
        x = RLINE - IL
        y = RSAMP - IS
        IF (ICODE.EQ.1) THEN
          L1(1) = PIC(IS,IL)
          L2(1) = PIC(IS+1,IL)
          L3(1) = PIC(IS,IL+1)
          L4(1) = PIC(IS+1,IL+1)
	  d1 = byte2int(l1(1))
	  d2 = byte2int(l2(1))
	  d3 = byte2int(l3(1))
	  d4 = byte2int(l4(1))
        ELSE
           d1 = HPIC(IS,IL)
           d2 = HPIC(IS+1,IL)
           d3 = HPIC(IS,IL+1)
           d4 = HPIC(IS+1,IL+1)
        ENDIF
        dn = d1 + (d2-d1)*x + (d3-d1)*y + (d1-d2-d3+d4)*x*y
        SUMDN = SUMDN + dn
        NPTS = NPTS + 1
   58   IF (RPLOT) THEN
           RLON = RLON + DLON
           IF (DPLOT.AND.(RLON.LT.CENTL+0.5*WLON)) GOTO 55
           IF (RLON.LT.ELON) GOTO 55
        ELSE
           IF (.NOT.SPLOT) THEN
             R = R + DR
             IF (R.LT.ER) GOTO 55
           ELSE
             IF (NPTS.LE.SDIST) GOTO 56
           END IF
        ENDIF

        IF(NPTS.GT.0) J = J + 1
        IF(NPTS.GT.0) WORK3(2,J)=SUMDN/NPTS
        IF (RPLOT) THEN
           IF (NPTS.GT.0) THEN
              WORK3(1,J)=R
              IF (DPLOT) ARPTS(1,J)=CENTL
           ENDIF
           IF (J.EQ.5000) THEN
              CALL XVMESSAGE('*** Warning, filled plot array',' ')
           ELSE
              R = R + DR
              IF (R.LE.ER) GOTO 50
           ENDIF
        ELSE
           IF (NPTS.GT.0) WORK3(1,J)=RLON
           IF (J.EQ.5000) THEN
              CALL XVMESSAGE('*** Warning, filled plot array',' ')
           ELSE
              RLON = RLON + DLON
              IF (RLON.LE.ELON) GOTO 50
           ENDIF
        ENDIF
59      N3 = J
      ENDIF

      IF (XVIPTST('PLOT').OR.REDO) THEN
         CALL LINSAM(IND,AVGR,AVGLON,RLINE,RSAMP)
         YY(1) = ZOOM*(RLINE-SL) + 1.5
         XX(1) = ZOOM*(RSAMP-SS) + 1.5
         IF (N3.LE.0) THEN
            CALL XVMESSAGE('*** Error, nothing to plot',' ')
            GOTO 10
         ELSE IF (N3.GT.1000) THEN
           S0 = 19
         ELSE
           S0 = 19
           IF (XX(1).LE.256) S0=271
         ENDIF
         L0 = 237
         IF (YY(1).LE.256) L0=493
         CALL PRDISPLAY(WORK3,N3,RPLOT,L0,S0,X0,Y0,DX,DY)
         REDO = .FALSE.
      ENDIF

      IF (XVIPTST('CP')) THEN
   12    XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
         WRITE(CMSG,115) LINE,SAMP,X0+DX*FLOAT(SAMP-S0),
     &                   Y0+DY*FLOAT(LINE-L0)
  115    FORMAT('(L,S)=(',I4,',',I4,') (XAXIS,DN)=(',
     &         F9.2,',',F9.2,')')
         CALL XVMESSAGE(CMSG,' ')
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 12
         GOTO 10
      ENDIF

      IF (PARMTST('OUTPUT',OUTFILE,I)) THEN
        DOPHA = .FALSE.
        IF (XVIPTST('PHASE')) DOPHA=.TRUE.
        OPEN(1,FILE=OUTFILE,STATUS='NEW',ERR=994)

        MSG(1:80) = ' '
        MSG(1:25) = ' Plot of frame xxxxxxx   '
        WRITE (MSG(16:25),'(I10)') FRAME_ID
        CALL XVMESSAGE(MSG(2:25),' ')
        WRITE(1,113) (MSG(I:I),I=1,30)
  113   FORMAT(37A1)
        MSG(1:80) = ' ' ! VRH added to indicate plane 5/30/89
        IF (MRING.EQ.1) THEN
            MSG(1:36) = ' Reference plane is planet''s equator'
        ELSE
            WRITE(CMSG,116) RINGS(MRING)
            MSG(1:34) = CMSG(1:34)
        ENDIF
        CALL XVMESSAGE(MSG(2:36),' ')
        WRITE(1,113) (MSG(I:I),I=1,36)
        MSG(1:80) = ' ' ! VRH bug fix - clear line 5/31/89
        IF (DOPHA) THEN
          MSG(1:37) = '     LONG    RADIUS      DN     PHASE'
          WRITE(1,113) (MSG(I:I),I=1,37)
          MSG(1:37) = '     ----    ------      --     -----'
          WRITE(1,113) (MSG(I:I),I=1,37)
        ELSE
          MSG(1:28) = '     LONG    RADIUS      DN '
          WRITE(1,113) (MSG(I:I),I=1,30)
          MSG(1:28) = '     ----    ------      -- '
          WRITE(1,113) (MSG(I:I),I=1,30)
        ENDIF
        MSG(1:80) = ' ' ! VRH bug fix - clear line 5/31/89
        DO J=1,N3
          IF (RPLOT) THEN
            IF (DPLOT) THEN
              RLON = ARPTS(1,J) !CENTL
            ELSE
              RLON = AVGLON
            ENDIF
            WRITE (MSG(4:10),'(F7.2)') RLON*RTD
            WRITE (MSG(12:20),'(F9.1)') WORK3(1,J)
            IF (DOPHA)
     &         CALL PHASE(WORK3(1,J),RLON,PHA,DUMMY,DUMMY,DUMMY)
          ELSE
            WRITE (MSG(4:10),'(F7.2)') WORK3(1,J)*RTD
            WRITE (MSG(12:20),'(F9.1)') AVGR
            IF (DOPHA)
     &         CALL PHASE(AVGR,WORK3(1,J),PHA,DUMMY,DUMMY,DUMMY)
          ENDIF
          WRITE (MSG(23:30),'(F8.2)') WORK3(2,J)
          IF (DOPHA) THEN
            WRITE (MSG(32:37),'(F6.2)') PHA*RTD ! VRH Change to 37 5/31/89
            WRITE(1,113) (MSG(I:I),I=1,37)
          ELSE
            WRITE(1,113) (MSG(I:I),I=1,31)
          ENDIF
        END DO
        CLOSE(1,ERR=995)
        CALL XVMESSAGE('Written to disc',' ')
        GOTO 10
  994   CALL XVMESSAGE('***Error opening output file',' ')
        GOTO 10
  995   CALL XVMESSAGE('***Error closing output file',' ')
        GOTO 10
      ENDIF

      GOTO 10
      END
