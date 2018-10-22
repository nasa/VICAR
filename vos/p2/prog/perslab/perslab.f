c
c program perslab
c
      include 'VICMAIN_FOR'
      subroutine main44

      character*80 msg
      character*30 utcstr
      character*12 planet     
      character*5 project
      character*80 filename
      character*40000 label
      logical file_exists
      real*4 data(40),not_set,north
      real*4 pldata(20),rd(80)
      integer*4 idata(40),def,count,input(10),nids 
      integer*4 d(80),buf(200),camera,fds,work(10000),ounit,kunit
      integer*4 status,datetime(6),bufsize
      real*8 data8(20),rbuf(100),j2000,spd,dpr,rpd
      real*8 julian,ra_v1,dec_v1,pa_v3,north_angle
      real*8 ephemeris_time,state(6),light_time,tibf(3,3),target_range
      real*8 lat,lon,target_ep_time
      equivalence(label,work)
      equivalence (buf,rbuf),(instrument,buf(2))
      equivalence (csource,buf(11)),(d,rd)
      equivalence (data8,data,idata)

      real*8 mp			! for mp routines
      INTEGER*4 GllPlanetID     ! function for PlanetID conversion

      CALL XVMESSAGE ('PERSLAB version (August 28, 2002)', ' ')

c initialize & load naif spice kernels
      call init_spice

c initialize mp routines - bam 11/95

      call mp_init(mp,status)
      if ( status .ne. 0 ) then
          call xvmessage ('MP error on init',' ')
          call xvmessage ('Program terminated',' ')
          return
      end if

c defaults
      not_set=-99999.0
      sclat=not_set
      sclon=not_set
      sunlat=not_set
      sunlon=not_set
      centline=not_set
      centsamp=not_set
      range=not_set
      focal=not_set
      scale=not_set
      north=not_set
      oaline=not_set
      oasamp=not_set
      julian=0.d0
      ra_v1=0.d0
      dec_v1=0.d0
      pa_v3=0.d0
      do i=1,40
         data(i)=0.0
      enddo
      idata(39)=0

c read the input labels.
      k=1
      call xvunit(input(k),'INP',k,status,' ')
      call xvopen(input(k),status,'op','read','open_act','sa',' ')
      call xvget(input(k),status,'NL',nl,'NS',ns,' ')

c      for porting to UNIX - BAM 11/95
c      remove calls to searcv2 and insert mp routines as follows
c
c      call searcv2(input(k),x,x,data,data)
      call mp_label_read(mp,input(k),status)
      if (status.ne. -1009) then
          call xvmessage ('MP error on read',' ')
          call xvmessage ('Program terminated',' ')
          return
      end if              
c
c     end searcv2 code
c


c get project
      project='     '
      camera=0
      call xvparm('PROJECT',project,count,def,1)
      call xvparm('CAMERA',camera,count,def,1)
      if(project.eq.'     ')then
        call getproj(input(k),project,camera,fds,ind)
        if(ind.ne.0) then
           call xvmessage('getproj: unknown project & camera',' ')
        else
           call xvmessage('Project is '//project,' ')
        endif
      endif

c get camera constants
      if(project.ne.'     ')then
         call getcamcon(project,camera,focal,oaline,oasamp,scale,ind)
         if(ind.ne.0) call xvmessage('GETCAMCON: fatal indicator',' ')
      endif
           
c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'op','write','open_act','sa',' ')

      if(project.eq.'     ') goto 200
      if(project.eq.'WFPC1'.or.project.eq.'WFPC2') goto 100

c get label info
      call getlabcon(input(k),project,d,ind)      
      if(ind.eq.1) call xvmessage('GETLABCON: warning indicator',' ')
      if(ind.gt.1) call xvmessage('GETLABCON: fatal indicator',' ')
      if(d(1).eq.0)call xvmessage('GETLABCON: invalid label type',' ')
     
c get spice info
      call getspice2(ounit,.TRUE.,buf,ind)
      IF (ind.eq.0) THEN
        call xvmessage('GETSPICE2: FAILURE',' ')
        call xvmessage('           All parameters should be'//
     &                 ' supplied',' ') 
      END IF
      range=rbuf(27)
      sunlat=rbuf(28)
      sunlon=rbuf(29)
      sclat=rbuf(30)
      sclon=rbuf(31)
      north=rbuf(68)

      IF ((project(1:3).EQ.'GLL') .OR. (project.EQ.'VGR-1') .OR.
     +    (project.EQ.'VGR-2') .OR. (project.EQ.'CASSI')) THEN
        CALL xvmessage('Correcting the NORTH_ANGLE with '//
     +                  '90-degree offset',' ')
        north = amod(north+90,360.0)
      END IF
      centline=rbuf(69)
      centsamp=rbuf(70)
      planet='            '

      go to 200
100   continue

c for wfpc read labels
      bufsize=40000
      call xlgetlabel(input(1),label(1:1),bufsize,stat)
      call chkstat(stat,'Error in xlgetlabel,status=',1,stat)
         i=index(label(1:bufsize),'DATE-OBS= "')
         if(i.gt.0)then
            read(label(i+11:),121) iday
            read(label(i+14:),121) imonth
            read(label(i+17:),121) iyear
         endif
         i=index(label(1:bufsize),'TIME-OBS= "')
         if(i.gt.0)then
            read(label(i+11:),121) ihour
            read(label(i+14:),121) iminute
            read(label(i+17:),121) isecond
         endif
         i=index(label(1:bufsize),'EXPTIME =')
         if(i.gt.0)then
            read(label(i+11:),122) exptime
         endif
         i=index(label(1:bufsize),'EXPSTART=')
         if(i.gt.0)then
            read(label(i+10:),123) julian
         endif
         i=index(label(1:bufsize),'TARGNAME= "')
         if(i.gt.0)then
            read(label(i+11:),131) planet
         endif
c         i=index(label(1:bufsize),'TARGDIST=')
c         if(i.gt.0)then
c            read(label(i+10:),123) range
c         endif
         range=0.
         i=index(label(1:bufsize),'RA_TARG = ')
         if(i.gt.0)then
            read(label(i+10:),123) ra_v1
         endif
         i=index(label(1:bufsize),'DEC_TARG= ')
         if(i.gt.0)then
            read(label(i+10:),123) dec_v1
         endif
         i=index(label(1:bufsize),'PA_V3   =')
         if(i.gt.0)then
            read(label(i+11:),122) pa_v3
         endif

c only for WFPC - read the auxiliary ascii files for parameters.
      call xvpcnt('INP',nids)      
      if(nids.eq.1) goto 201
      do 130 k=2,nids
         status=xvpone('INP',filename,k,80)
         kunit = 1
         open(kunit,file=filename,status='old',
     +        access='sequential',
     +        form='formatted')
         call xvmessage('opening file '//filename,' ')
110      read(kunit,fmt=120,err=129) label
120      format(A80)
         i=index(label(1:80),'DATE-OBS= ''')
         if(i.gt.0)then
            read(label(i+11:),121) iday
            read(label(i+14:),121) imonth
            read(label(i+17:),121) iyear
121         format(i2)
         endif
         i=index(label(1:80),'TIME-OBS= ''')
         if(i.gt.0)then
            read(label(i+11:),121) ihour
            read(label(i+14:),121) iminute
            read(label(i+17:),121) isecond
         endif
         i=index(label(1:80),'EXPTIME =')
         if(i.gt.0)then
            read(label(i+11:),122) exptime
122         format(g19.7)
        endif
         i=index(label(1:80),'EXPSTART=')
         if(i.gt.0)then
            read(label(i+10:),123) julian
123         format(g20.13)
         endif
         i=index(label(1:80),'TARKEY1 = ''')
         if(i.gt.0)then
            read(label(i+18:),131) planet
131         format(A11)
         endif
         i=index(label(1:80),'TARGDIST=')
         if(i.gt.0)then
            read(label(i+10:),123) range
         endif
         i=index(label(1:80),'RA_V1   =')
         if(i.gt.0)then
            read(label(i+10:),123) ra_v1
         endif
         i=index(label(1:80),'DEC_V1  =')
         if(i.gt.0)then
            read(label(i+10:),123) dec_v1
         endif
         i=index(label(1:80),'PA_V3   =')
         if(i.gt.0)then
            read(label(i+11:),122) pa_v3
         endif

         goto 110
129      close(kunit)
130   continue
201   continue

c parameter override
      call xvparm('JULTIME',utcstr,count,def,1)
      if(count.gt.0)then
         call xvmessage(utcstr,' ')
         call utc2et(utcstr,julian)
         julian=julian-2400000.5d0
      endif

      call xvparm('JULDATE',datetime,count,def,5)
      if(count.gt.0)then
         mty=datetime(1)
         mtd=datetime(2)
         mth=datetime(3)
         mtm=datetime(4)
         mts=datetime(5)
         mtms=0
C        USE NATIONAL RADIO ASTRONOMY OBSERVATORY ALGORITHM TO
C        COMPUTE JULIAN DATE.
         JDN=1721060-(4-MOD(MTY+0,4))/4+(100-MOD(MTY+0,100))/100
     &   -(400-MOD(MTY+0,400))/400+MTY*365+MTY/4-MTY/100+MTY/400+MTD
         julian=DFLOAT(JDN)-0.5D0+(MTH+
     &      (MTM+(MTS+MTMS/1000.)/60.)/60.)/24.D0  -2400000.5d0
      endif
      
      call xvparmd('JULIAN',julian,count,def,1)
      if(count.gt.0) julian=julian-2400000.5d0
      call xvparmd('RA_V1',ra_v1,count,def,1)
      call xvparmd('DEC_V1',dec_v1,count,def,1)
      call xvparmd('PA_V3',pa_v3,count,def,1)

      write(msg,128)iyear,imonth,iday,ihour,iminute,isecond
128   format('UT time in y:m:d:h:m:s=',6(i4,1x))
      call xvmessage(msg,' ')
      if(iyear.gt.0) call jday(imonth,iday,iyear+1900,idayofyear)
      write(msg,136)iyear,idayofyear,ihour,iminute,isecond
136   format('UT time in y:d:h:m:s=',5(i4,1x))
      call xvmessage(msg,' ')
      write(msg,127)exptime
127   format('exposure time (sec)=',f9.4)
      call xvmessage(msg,' ')
      julian=julian + 2400000.5d0
      write(msg,126)julian
126   format('Julian date start exposure= ',d20.13)
      call xvmessage(msg,' ')
      julian= julian + (exptime/2.d0)/86400.d0     
      write(msg,125)julian
125   format('Julian date center exposure= ',d20.13)
      call xvmessage(msg,' ')
c      julian= julian - (range/299792.5d0)/86400.d0
c      write(msg,135)julian
c135   format('Julian date light time corrected =',d20.13)
c      call xvmessage(msg,' ')
      write(msg,134) range
134   format('Target range (km)= ',e15.8)
      call xvmessage(msg,' ')
      write(msg,132) ra_v1,dec_v1
132   format('RA of V1 = ',d20.13,' DEC of V1 = ',d20.13)
      call xvmessage(msg,' ')
      write(msg,133) pa_v3
133   format('Position angle of V3 = ',f10.3)
      call xvmessage(msg,' ')


200   continue
c get planet constants
      IF (project(1:3).EQ.'VGR') THEN
        ! Convert idnum to GLL's convention so getplacon get execute
        idnum=GllPlanetID(buf(9))
        IF (idnum.EQ.0) THEN
          call xvmessage('WARNING: unable to obtain GLL Planet ID',' ')
        END IF
      ELSE
        idnum=buf(9)
      END IF
      call xvparm('TARGET',planet,count,def,1)
      if(count.gt.0) call ucase(planet,planet) ! upper case
      call getplacon(planet,idnum,pldata,ind)
      if(ind.eq.1) call xvmessage('Unrecognizable planet name',' ')
      if(ind.eq.2) call xvmessage('Unrecognizable planet id#',' ')
      call xvmessage('Target planet body is: '//planet,' ')
      call prnt(4,1,idnum,    'Planet id number ')
      data(26)=pldata(2)
      data(25)=pldata(3)

c get farenc planet center from FARENC.POS file.
      inquire(FILE='FARENC.POS',EXIST=file_exists)
      if(file_exists)then
        call xvunit(farunit,'oldfile',1,status,'U_NAME','FARENC.POS',
     +              ' ')
	call xvopen(farunit,status,'op','write','open_act','sa',' ')
        call xvread(farunit,data(33),status,'LINE',1,'SAMP',1,
     +               'NSAMPS',2,' ')
        call xvclose(farunit,status,' ')
      endif

c compute target body sub-earth and sub-sun points (planetocentric)
      if( ((project.eq.'WFPC1').and.(nids.gt.1)).or.
     +          (project.eq.'WFPC2')) then
         ephemeris_time=(julian-j2000())*spd()
         id_target=idnum   ! The target object
         id_observer=399   ! Earth
c        compute target state light time corrected at epoch from observer 
         call spkez(id_target,ephemeris_time,'J2000','LT',id_observer,
     +              state,light_time)

c        compute target RA & DEC
         call reclat(state,target_range,lon,lat)
         tgt_dec=lat*dpr()
         tgt_ra=lon*dpr()
         if(tgt_ra.lt.0.) tgt_ra=tgt_ra+360.
         write(msg,140) tgt_ra,tgt_dec
140      format(' Target RA=',f10.5,' Target DEC=',f10.5)
         call xvmessage(msg,' ') 

c        compute inertial to body-fixed rotation matrix tibf
         call bodmat(id_target,ephemeris_time - light_time,tibf)
c        reverse state vector to from target to observer
         call vminus(state,state)
c        rotate state into body-fixed
         call mxv(tibf,state,state)
c        compute range to target latitude & longitude of sub point
         call reclat(state,target_range,lon,lat)
         range=target_range
         sclat=lat*dpr()
         sclon=lon*dpr()
         sclon=360.-sclon               ! convert to west
         if(sclon.gt.360.) sclon=sclon-360.
         if(sclon.lt.0.) sclon=sclon+360.
         write(msg,137) sclat,sclon
137      format(' Target: Subsc lat=',f10.5,' Subsc lon=',f10.5)
         call xvmessage(msg,' ') 
         target_ep_time = ephemeris_time - light_time

c        compute north angle
         if(dec_v1.eq.0.d0) dec_v1=tgt_dec
         if(ra_v1.eq.0.d0) ra_v1=tgt_ra
         call nangle(id_target,target_ep_time,'J2000',ra_v1*rpd(),
     +               dec_v1*rpd(),pa_v3*rpd(),north_angle)
         north=north_angle*dpr()
         if(north.lt.0.0) north=north+360.  ! make positive
         if(project.eq.'WFPC1') north=north+180.   ! fix to match imagery
         if(project.eq.'WFPC2') north=north+260.   !    "
         if(north.gt.360.) north=north-360.
         write(msg,141)north
141      format(' North angle of spin axis= ',f10.3)
         call xvmessage(msg,' ')

c        compute ra & dec of sun
         call spkez(10,target_ep_time,'J2000','NONE',
     +              id_observer,state,light_time)
         call reclat(state,target_range,lon,lat)
         sun_dec=lat*dpr()
         sun_ra=lon*dpr()
         if(sun_ra.lt.0.) sun_ra=sun_ra+360.
         write(msg,138) sun_ra,sun_dec
138      format(' Sun RA=',f10.5,' Sun DEC=',f10.5)
         call xvmessage(msg,' ') 

c        Compute sub solar point on target
         id_observer=10   ! Sun
c         call spkez(id_target,ephemeris_time,'J2000','LT',id_observer,
c     +              state,light_time)
c         call bodmat(id_target,ephemeris_time - light_time,tibf)
         call spkez(id_target,target_ep_time,'J2000','NONE',
     +              id_observer,state,light_time)
         call bodmat(id_target,target_ep_time,tibf)
         call vminus(state,state)
         call mxv(tibf,state,state)
         call reclat(state,target_range,lon,lat)
         sunlat=lat*dpr()
         sunlon=lon*dpr()
         sunlon=360.-sunlon             ! convert to west
         if(sunlon.gt.360.) sunlon=sunlon-360.
         if(sunlon.lt.0.) sunlon=sunlon+360.
         write(msg,139) sunlat,sunlon
139      format(' Target: subsol lat=',f10.5,' subsol lon=',f10.5)
         call xvmessage(msg,' ') 
      endif

c override parameters
      call xvparm('SCLAT',sclat,count,def,1)
      call xvparm('SCLON',sclon,count,def,1)
      call xvparm('CENTLINE',centline,count,def,1)
      call xvparm('CENTSAMP',centsamp,count,def,1)
      call xvparm('RANGE',range,count,def,1)
      call xvparm('FOCAL',focal,count,def,1)
      call xvparm('SCALE',scale,count,def,1)
      call xvparm('NORTH',north,count,def,1)
      call xvparm('OALINE',oaline,count,def,1)
      call xvparm('OASAMP',oasamp,count,def,1)
      call xvparm('SUNLAT',sunlat,count,def,1)
      call xvparm('SUNLON',sunlon,count,def,1)
      call xvparm('DATETIME',datetime,count,def,5)
      if(count.gt.0)then
         iyear=datetime(1)
         idayofyear=datetime(2)
         ihour=datetime(3)
         iminute=datetime(4)
         isecond=datetime(5)
      endif

c add date & time to label
      if(project.eq.'WFPC1'.or.project.eq.'WFPC2')then
        call xladd(ounit,'HISTORY','PLANET',planet,status,
     +           'FORMAT','STRING','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
        call xladd(ounit,'HISTORY','UT_DATE_YEAR',iyear,status,
     +           'FORMAT','INT','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
        call xladd(ounit,'HISTORY','UT_DATE_DAY',idayofyear,status,
     +           'FORMAT','INT','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
        call xladd(ounit,'HISTORY','UT_DATE_HOUR',ihour,status,
     +           'FORMAT','INT','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
        call xladd(ounit,'HISTORY','UT_DATE_MINUTE',iminute,status,
     +           'FORMAT','INT','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
        call xladd(ounit,'HISTORY','UT_DATE_SECOND',isecond,status,
     +           'FORMAT','INT','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
      endif

c add solar lat & lon to label
      call xladd(ounit,'HISTORY','SUB_SOLAR_LATITUDE',sunlat,status,
     +           'FORMAT','REAL','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')
      call xladd(ounit,'HISTORY','SUB_SOLAR_LONGITUDE',sunlon,status,
     +           'FORMAT','REAL','MODE','REPLACE',
     +           'HIST','PERSLAB',' ')

c load data buffer
      if(sclat.ne.not_set) data(31)=sclat
      if(sclon.ne.not_set) data(32)=sclon
      if(centline.ne.not_set) data(33)=centline
      if(centsamp.ne.not_set) data(34)=centsamp
      if(range.ne.not_set) data(38)=range
      if(focal.ne.not_set) data(27)=focal
      if(scale.ne.not_set) data(30)=scale
      if(north.ne.not_set) data(35)=north 
      if(oaline.ne.not_set) data(28)=oaline
      if(oasamp.ne.not_set) data(29)=oasamp
      idata(39)=16  ! perspective type

c add map3 label
c      call maplabv2(data,data,ounit,nlr,work)
       call mp_buf2mpo( idata, mp, status )
      if ( status .ne. 0 ) then 
          call xvmessage ('MP error on buffer transfer',0)
          call xvmessage ('Program terminated',0)
          return
      end if
c
      call mp_set_value(mp, 'A_AXIS_RADIUS', dble(pldata(1)), status)
      call mp_label_write( mp, ounit, 'HISTORY', status )
      if ( status .ne. 0 ) then 
          call xvmessage ('MP label write error',0)
          call xvmessage ('Program terminated',0)
          return
      end if

      call mp_label_write( mp, ounit, 'PROPERTY', status )
      if ( status .ne. 0 ) then 
          call xvmessage ('MP label write error',0)
          call xvmessage ('Program terminated',0)
          return
      end if
C Free MP Buffer
      call mp_free(mp)

c copy input to output
      call xvget(input(1),status,'NL',nl,'NS',ns,' ')
      do i=1,nl
         call xvread(input(1),work,status,'LINE',i,' ')
         call xvwrit(ounit,work,status,'LINE',i,' ')
      enddo

      return
      end

c***********************************************************************

c**********************************************************************
      SUBROUTINE NANGLE ( BODY, ET, REF, RA, DEC, V3POS, ANGLE )

      IMPLICIT NONE

C$ Abstract
C
C     Find the North angle of a specified body in an HST image.
C
C$ Required_Reading
C
C     PCK
C     TIME
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C
C$ Declarations

      INTEGER               BODY
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      V3POS
      DOUBLE PRECISION      ANGLE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   NAIF integer code for target body.
C     ET         I   Epoch of observation, ephemeris seconds past J2000.
C     REF        I   Inertial reference frame specification.
C     RA,
C     DEC,
C     V3POS,     I   RA, Dec, and V3 position angle of HST camera.
C     ANGLE      O   North angle of VECTOR as seen in HST image.
C
C$ Detailed_Input
C
C     BODY           NAIF integer code for target body whose "North
C                    angle" is to be found.
C
C                    The correspondence between body codes and names
C                    is given in the NAIF_IDS required reading.
C                    Also, the GLLSPICE routine BODTRN_G implements
C                    the mapping between names and codes.
C
C     ET             is the epoch, specified in ephemeris seconds
C                    past J2000.
C
C
C     REF            is a NAIF name for an Earth mean equator and
C                    equinox inertial reference frame.  Possible
C                    values include 'J2000' and 'B1950'.  See the
C                    SPICELIB routine CHGIRF for the complete set
C                    of supported frame.
C
C     RA,
C     DEC,
C     V3POS          are, respectively, the right ascension and
C                    declination of the HST camera's V1 (boresight)
C                    axis, and position angle of the camera's V3
C                    axis.  These angles must be specified relative to
C                    the inertial reference frame designated by
C                    REF.
C
C                    Units are radians.
C
C$ Detailed_Output
C
C     ANGLE          is the North angle of the North pole of
C                    the body designated by BODY, measured in the
C                    HST camera's V2-V3 plane.  Units are radians.
C                    ANGLE satisfies the inequality
C
C
C                       - pi/2  <  ANGLE  <  pi/2
C                                         -
C
C                    In an HST image, the angle is measured from
C                    the projection of the BODY's North pole to
C                    up in the image, counterclockwise, as shown
C                    below:
C
C
C                        [Jean, note that this picture represents
C                         a *guess* as to rotation of the image
C                          --Nat]
C
C
C                                ^
C                 "up" in image  | +V3
C                                |
C                              > | <
C                       +---.----|----.---+
C                       |  .     |     .  |  V3 position angle V3POS
C  Output ANGLE (the angle .     |     .  |  (measured counterclockwise
C  shown is negative)   | +V1 into page   |  about +V1---the angle
C                 <---------.--- +    .   |  shown is negative)
C                 +V2   |      ./ \.      |
C                       |      /   \      |
C                       |     /     \     |
C                       +----/-------\----+
C                           /         \
C                          /           \
C    Projection of North  *             * North vector projection
C       pole of BODY
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input argument DEC is outside of the range
C
C            ( -pi/2,  pi/2  )
C
C         the error SPICE(VALUEOUTOFRANGE) will be signalled.
C
C     2)  A SPICE PCK kernel containing rotational elements for the
C         body designated by the input argument BODY must be loaded
C         at the time this routine is called.  If this data is
C         not available at the time this routine is called, the error
C         will be diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C
C     1)  Suppose we wish to find the North angle of Jupiter's
C         spin axis at a specified UTC epoch.  We'd use this routine
C         in combination with SPICELIB routines to solve the problem.
C
C         A sample solution is shown below.  Note that the file
C         names used are fictitious; in your own code, you'd
C         supply the names of kernel files available within your own
C         file system.
C
C            C
C            C     Load a leapseconds kernel to support UTC-->ET
C            C     conversion.
C            C
C                  CALL LDPOOL ( 'my_leapseconds.ker' )
C
C            C
C            C     Load a PCK kernel to provide us with rotational
C            C     elements for Jupiter.
C            C
C                  CALL LDPOOL ( 'my_pck.ker' )
C
C            C
C            C     Convert the UTC epoch of interest to ET.
C            C
C                  CALL UTC2ET ( UTC, ET )
C
C            C     We assume we have the HST camera's RA, DEC, and
C            C     V3 postion angle, all measured relative to the
C            C     J2000 frame, in *radians*.
C            C
C                  CALL NANGLE (  599,  ET,  'J2000',
C                 .               RA,   DEC,  V3POS,  ANGLE  )
C
C            C
C            C     Display the result in degrees.   We use the
C            C     SPICELIB function DPR for unit conversion.
C            C
C                  WRITE (*,*) 'North position angle of Jupiter''s' //
C                 .            'North pole vector at epoch (deg): ',
C                 .            ANGLE * DPR()
C
C$ Restrictions
C
C     Intended for review by Jean Lorre (JPL) only.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 01-OCT-1993 (NJB)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI

      LOGICAL               RETURN


C
C     Local variables
C
      DOUBLE PRECISION      CAMPOL ( 3 )
      DOUBLE PRECISION      ENORTH ( 3 )
      DOUBLE PRECISION      ENPERP ( 3 )
      DOUBLE PRECISION      NPOLE  ( 3 )
      DOUBLE PRECISION      TI2CAM ( 3, 3 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V3     ( 3 )

      INTEGER               I


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NANGLE' )
      END IF

C
C     The problem is ill-defined if
C
C         | DEC |  =   +/- (pi/2)
C
C
      IF (  DABS(DEC) .GE. HALFPI() ) THEN

         CALL SETMSG ( 'DEC is out of range; DEC = #.' )
         CALL ERRDP  ( '#',  DEC                       )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'        )
         CALL CHKOUT ( 'NANGLE'                        )
         RETURN

      END IF

C
C     To start out, we'll need the 3x3 inertial-to-HST-camera-frame
C     transformation matrix.  We'll need to construct the HST's V1, V2,
C     and V3 vectors in inertial coordinates from our input information.
C
C     The V1 axis (boresight direction) is easy.
C
      CALL RADREC ( 1.D0, RA, DEC, V1 )

C
C     The V3 position angle will allow us to find the V3 axis.  First,
C     find the component of the Earth's North direction that's
C     orthogonal to the V1 vector.  We'll call this component ENPERP.
C
      CALL VPACK (  0.D0,    0.D0,  1.D0,   ENORTH  )
      CALL VPERP (  ENORTH,  V1,    ENPERP          )

C
C     If ENPERP is the null vector, we have a singular case.  However,
C     our earlier error check on DEC should have ruled out this
C     possibility.
C
C     Next, rotate ENPERP in the positive right-handed sense by V3POS to
C     obtain the direction of V3.  Unitizing the result will yield the
C     unit vector V3.
C
      CALL VROTV ( ENPERP, V1, V3POS, V3 )
      CALL VHAT  ( V3,                V3 )

C
C     The system is right handed, so V2 is V3 x V1.
C
      CALL VCRSS ( V3, V1, V2 )

C
C     Form the desired transformation matrix TI2CAM
C     ("transformation from inertial to camera").
C
      DO I = 1, 3

         TI2CAM(1,I) = V1(I)
         TI2CAM(2,I) = V2(I)
         TI2CAM(3,I) = V3(I)

      END DO

C
C     Obtain the north pole of the body of interest, specified
C     relative to the frame of interest.  The pole is the third
C     row of the matrix TIPM.
C
      CALL TIPBOD ( REF, BODY, ET, TIPM  )

      DO I = 1, 3
         NPOLE(I)  =  TIPM(I,3)
      END DO

C
C     At long last, we're ready to convert the pole vector into HST
C     camera coordinates.  The conversion is effected by the
C     left-multiplication
C
C        CAMPOL = TI2CAM * NPOLE
C
      CALL MXV ( TI2CAM, NPOLE, CAMPOL )

C
C     The last two components of CAMPOL give us the projection of
C     CAMPOL onto the focal plane (the V2-V3 plane).  The angle of this
C     projection from the V2 axis is given by the two-argument
C     arctangent; we just subtract pi/2 from it to get the North angle.
C
      ANGLE  =  DATAN2 ( CAMPOL(3), CAMPOL(2) ) - HALFPI()


      CALL CHKOUT ( 'NANGLE' )
      RETURN
      END


C------------------------------------------------------------------------------
C Function GllPlanetID
C Purpose: Converts a given Voyager planet ID to that of the Gailileo's
C Input: VgrPlanetID => The planet ID as defined by Voyager project
C Output: None
C Return: Planet ID defined by Gailileo project
C------------------------------------------------------------------------------
      INTEGER*4 FUNCTION GllPlanetID(VgrPlanetID)
C----------------------- Parameter List, Begin --------------------------------
      INTEGER*4 VgrPlanetID                             ! in
C----------------------- Parameter List, End ----------------------------------
C------------------ Conversion Table, Begin ----------------------------------
      INTEGER*4 NUMID/63/     ! Number of elements for conversion table
      INTEGER*4 GLLID(63)/    ! Gailileo Planet ID
     + 1,
     + 2,
     + 399,301,
     + 4,401,402,
     + 599,501,502,503,504,505,506,507,508,509,510,511,512,513,
     + 514,515,516,
     + 699,601,602,603,604,605,606,607,608,609,610,611,612,613,
     + 614,615,616,617,
     + 799,701,702,703,704,705,713,714,715,710,712,709,707,708,
     + 711,706,
     + 899,801,802,
     + 999,901/

      INTEGER*4 VGRID(63)/    ! Voyager SEDR Planet ID
     & 1,
     & 2,
     & 3,13,
     & 4,14,24,
     & 5,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,
     & 6,16,26,36,46,56,66,76,86,96,106,116,126,136,146,156,166,176,
     & 7,17,27,37,47,57,67,77,87,97,107,117,127,137,147,157,
     & 8,18,28,
     & 9,19/
C---------------------- Conversion Table, End ---------------------------------
      INTEGER I

      GllPlanetID = 0
      DO I = 1,NUMID
         IF (VgrPlanetID.EQ.VGRID(I)) THEN
           GllPlanetID = GLLID(I)
           RETURN
         ENDIF
      ENDDO

      RETURN
      END

