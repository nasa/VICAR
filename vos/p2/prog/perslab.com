$!****************************************************************************
$!
$! Build proc for MIPL module perslab
$! VPACK Version 1.9, Thursday, August 29, 2002, 15:40:29
$!
$! Execute by entering:		$ @perslab
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the SYS parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module perslab ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = "SYS"
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to perslab.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("perslab.imake") .nes. ""
$   then
$      vimake perslab
$      purge perslab.bld
$   else
$      if F$SEARCH("perslab.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake perslab
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @perslab.bld "STD"
$   else
$      @perslab.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create perslab.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack perslab.com -u -mixed -
	-s perslab.f -
	-i perslab.imake -
	-p perslab.pdf -
	-t tstperslab.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create perslab.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create perslab.imake
#define PROGRAM perslab

#define MODULE_LIST perslab.f  

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define FTNINC_LIST fspc
#define LIB_P2INC
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77
#define LIB_SPICE   /* to be removed when MP routine's dependency is removed
                     * from LIB_SPICE.  Prabhu knows when the removal is done
                     */
#define LIB_NETWORK

/* #define DEBUG  /* Remove upon delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create perslab.pdf
process help=*
PARM INP TYPE=STRING COUNT=(1:10) 
PARM OUT TYPE=STRING COUNT=1 
PARM KERNELS TYPE=STRING COUNT=(0:1) +
  DEFAULT=DEV2:[GMY059.NAIF]JUP035-HST.BSP
PARM SCLAT TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM SCLON TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM SUNLAT TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM SUNLON TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM CENTLINE TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM CENTSAMP TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM RANGE TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM FOCAL TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM SCALE TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM NORTH TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM OALINE TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM OASAMP TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM DATETIME TYPE=INTEGER COUNT=(0,5) DEFAULT=--
PARM JULDATE TYPE=INTEGER COUNT=(0,5) DEFAULT=--
PARM JULTIME TYPE=(STRING,30) COUNT=(0:1) DEFAULT=--
PARM JULIAN TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM RA_V1 TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM DEC_V1 TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM PA_V3 TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM PROJECT TYPE=(STRING,5) COUNT=(0:1) DEFAULT=--
PARM CAMERA TYPE=INTEGER COUNT=(0:1) DEFAULT=--

PARM TARGET     TYPE=(STRING,12) COUNT=0:1                      DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1                        DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1                DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1                        DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
                   
!# parm inp(2-10) hints=default
END-PROC
.TITLE
VICAR program PERSLAB.

.HELP
PURPOSE:
Places a perspective map projection label onto an object space image.
In general this is not desired for flight projects because the image
navigation is contained in the SEDR/SPICE data base and is subject
to change. For Space Telescope and other imagery there is no spice 
so this provides an alternative. Many programs will honor this label
and not request spice information.

USAGE & EXECUTION:
perslab inp=(in1,in2,...inn) out=in1_modified parameters

for example:

FOR SPACE TELESCOPE AND OTHER NON FLIGHT PROJECTS:
The input files in2,in3... contain ascii header info which contains
navigation information from which spice can be requested to compute
complete navigation. If these ascii files are missing then the spice
cannot be called and the user must supply all the parameters himself.
For the following example perslab first puts into the label 
everything it knows.
Then Farenc creates a file called farenc.pos containing the planet center.
Then perslab reads the farenc file and adds this to the label.
Both perslab calls are identical.

TEST FOR WFPC1
vfits2 inp=a_fits_image  out=w.half
perslab inp=(w.half,w.hdr,w.dat) out=x.img
farenc inp=x.img area=(60,60,680,680)
perslab inp=(w.half,w.hdr,w.dat) out=x.img

TEST FOR WFPC2
vfits2 inp=a_fits_image  out=w.real
flot inp=w.real out=f.real 'vert
astrtchr inp=f.real out=f.byte percent=2.
perslab inp=(f.byte) out=x.img
farenc inp=x.img area=(60,60,680,680)
perslab inp=(f.real) out=x.img

TEST FOR WFPC2 where no labels exist but the camera and time are known.
vfits2 inp=a_fits_image  out=w.real
flot inp=w.real out=f.real 'vert
astrtchr inp=f.real out=f.byte percent=2.
perslab inp=(f.byte) out=x.img project=WFPC2 camera=2 +
 juldate=(1994,196,11,33,2) planet=jupiter north=313.3 +
 project=WFPC2 camera=2
farenc inp=x.img area=(60,60,680,680)
perslab inp=(f.real) out=x.img project=WFPC2 camera=2 +
 juldate=(1994,196,11,33,2) planet=jupiter north=313.3 +
 project=WFPC2 camera=2

OVERRIDING ALL PARAMETERS
perslab inp=something out=x.img +
  planet=jupiter sclat=45. +
  sclon=234. sunlat=.5 sunlon=99. centline=456 centsamp=66 +
  range=987654. north=2. focal=1400. scale=65. oaline=300. +
  oasamp=301. datetime=(92,350,14,14,17)

 VOYAGER TEST
perslab inp=mipl:[mipl.vgr]f1636832.fic out=x.img source=FARE

 GALILEO TEST
perslab inp=mipl:[mipl.gll]venus.img out=x.img

You now can run other programs as below:

 TEST OF PHOTFUNC
photfunc inp=x.img out=v2$scratch:c.img minn=.7 maxcor=5.

 TEST OF MAP3
Uses a zonal flow model and a reference time one day later. Note that
perslab stores the space telescope date in the label for retrieval.
map3 inp=x.img out=v2$scratch:p.img 'orth north=0 +
   scale=200 reftime=(92,351,14,14,17,0)

.PAGE
NEW FEATURES SINCE LAST DELIVERY

Version (January 22, 1997):
* The program now informs the user that it's making 90-degree adjustment to the
NORTH_ANGLE when processing data of project GLL, VGR-1, VGR-2 or CASSI.  The
actual adjustment was actually made on 7/15/96, but it seems nice to let the
user  know.

.PAGE
OPERATION:
Perslab reads the input label and any additional input ascii files for
information about image navigation.

For flight projects it reads the sedr/spice files.

For WFPC it looks for specific label entries and computes the rest
using the NAIF spice subroutines.

.PAGE
SPICE and SEDR PARAMETERS

All of the following parameters except TARGET are for SPICE and SEDR operation 
only.  These SPICE parameters will only affect the GLL and VGR processing.

TARGET
SPICEMODE
CKNAME
CKID
USERID
GROUPID
INSTITUTE
CDATE
REQNUM
PURPOSE
PROGRAM
SPKID 

.PAGE
SPICE Operations
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels:

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
When specified, the CKID parameter overrides the CKNAME parameter.  If the
camera pointing data is not found in the requested C kernel, the other C kernels
are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific instance of
camera pointing based upon this provenance information:

CDATE specifies the date and time the camera pointing was created.
REQNUM identifies the request number associated with the camera pointing.
PURPOSE identifies the purpose for creating the camera pointing.
PROGRAM identifies the program which created the camera pointing.
SPKID identifies the SP-kernel used to create the camera pointing.
USERID identifies the user who created the camera pointing.
GROUPID identifies the group which created the camera pointing.                 
INSTITUTE identifies the facility which created the camera pointing.

A complete list of CK and SPK IDs are located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

The above parameters are optional, and if defaulted (or if no data is found for
the requested version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.

           'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

.PAGE 
FOR USERS OF PREVIOUS VERSION OF PERSLAB

Parameter PLANET is replaced by TARGET; SOURCE by CKNAME in order to eliminate 
the duplication with getspice2's parameters

HISTORY:
08-28-02 VRH  Have 90 added to NORTH_ANGLE for CASSI also. AR107373
02-14-97 SMC  Terminated all necessary XV* calls & Free MP buffer        FR89945
01-22-97 SMC  * "Normalize" MP_LABEL's NORTH_ANGLE to fall within 0-359. FR89954
              * Insert correct value to MP_LABEL's A_AXIS_RADIUS         FR89954
07-16-96 SMC  Added SPICE parameters, replace parameter PLANET by TARGET;
              SOURCE by CKNAME
07-16-96 SMC  use XVPARM to obtain parameter NORTH instead of XVPARMD
07-15-96 SMC  Corrected the NORTH_ANGLE for GLL and VGRs by adding 90 to it.
07-13-96 SMC  Convert Planet ID to that of GLL's when processing VGR, FR89379
              Open INP file as READ instead of WRITE.

8-15-93  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
First input: image
The rest: ascii files

.VARI OUT
Output image

.VARI KERNELS
NAIF SP kernels filename.

.VARI SCLAT
Sub spacecraft latitude
in degrees.

.VARI SCLON
Sub spacecraft longitude
in degrees west.

.VARI SUNLAT
Sub solar latitude
in degrees.

.VARI SUNLON
Sub solar longitude
in degrees west.

.VARI CENTLINE
Line of planet center.

.VARI CENTSAMP
Sample of planet center

.VARI RANGE
Distance from spacecraft
to planet center in KM.

.VARI FOCAL
Camera focal length.
In mm.

.VARI SCALE
Camera scale.
pixels/mm.

.VARI NORTH
The north angle
in degrees clockwise
from up.

.VARI OALINE
Line of the camera
optical axis.

.VARI OASAMP
Sample of the camera
optical axis.

.VARI DATETIME
Date & time 
in the form
y d h m s
all integers

.VARI PROJECT
Override the project

.VARI CAMERA
Override camera number

.VARI JULDATE
Override the julian
date.
Five integers.

.VARI JULTIME
Override the julian
date.
A string.

.VARI JULIAN
Override the julian
date.
Large float.

.VARI RA_V1
Override the space
telescope V1 axis
RA direction.

.VARI DEC_V1
Override the space
telescope V1 axis
Declination direction.

.VARI PA_V3
Override the space
teleccope V3 axis
position angle.

.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
This parameter replaces PLANET
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
This parameter replaces SOURCE
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created


.LEVEL2
.VARI INP
First input: Vicar image of target body. This image will be copied
to the output with a perspective projection label.

For WFPC project (space telescope images only) there may be additional
input files. These are ascii files with information which can be
searched for by PERSLAB. These are the equivalent of the .SHH and .IMH
space telescope auxiliary files. To make use of the naif spice these
files must be present. If not then you must provide missing information.

.VARI OUT
The output image containing the perspective label.

.VARI KERNELS
NAIF SP kernels filename.
This is the ephemeris kernels file for all planetary bodies of interest.
Default is: dev2:[gmy059.naif]jup035-hst.bsp

.VARI SCLAT
Sub spacecraft planetocentric latitude in degrees.
Normally this is obtained from the input label(s).

.VARI SCLON
Sub spacecraft longitude in degrees west.
Normally this is obtained from the input label(s).

.VARI SUNLAT
Sub solar planetocentric latitude in degrees.
Normally this is obtained from the input label(s).

.VARI SUNLON
Sub solar longitude in degrees west.
Normally this is obtained from the input label(s).

.VARI CENTLINE
Line of planet center.
Normally this is obtained from the input label(s).

.VARI CENTSAMP
Sample of planet center
Normally this is obtained from the input label(s).

.VARI RANGE
Distance from spacecraft to planet center in KM.
Normally this is obtained from the input label(s).

.VARI FOCAL
Camera focal length in mm.
Normally this is obtained from the input label(s).
or from the PROJECT & CAMERA keywords.

.VARI SCALE
Camera scale in pixels/mm.
Normally this is obtained from the input label(s).
or from the PROJECT & CAMERA keywords.

.VARI NORTH
The angle measured in degrees clockwise from up of the projection of the
planet spin axis (north end) normally onto the image plane.
Normally this is obtained from the input label(s).

.VARI OALINE
Line of the camera optical axis.
Normally this is obtained from the input label(s).
or from the PROJECT & CAMERA keywords.

.VARI OASAMP
Sample of the camera optical axis.
Normally this is obtained from the input label(s).
or from the PROJECT & CAMERA keywords.

.VARI DATETIME
Date & time of the image in the form: year day_of_year hour minute second.
All 5 values are integers.
The only use for this field is to pass through getlabcon these 5 integers
so that a program like map3 can compute time intervals for zonal flow
correction. Consequently use the compatible format to what map3 will receive
in it's parameter REFTIME. ie: 1992 versus 92 for example.
Normally this is obtained from the input label(s).

.VARI PROJECT
Override the project
Normally this is obtained from the input label(s).
Example: project=WFPC2

.VARI CAMERA
Override camera number
Normally this is obtained from the input label(s).
For flight projects this is the GETCAMCON camera number.
For WFPC 1-Planetary camera, 2=Wide field camera.

.VARI JULDATE
Override the julian date.
Normally this is obtained from the input label(s).
Five integers. (year,dayofyear,hour,minute,second)
Note that the julian date is used to compute values from the spice
navigation files. Unless the project is WFPC1 or WFPC2 the spice
computation will be ignored .

.VARI JULTIME
Override the julian date.
Normally this is obtained from the input label(s).
A string of the form: "1994-186 // 11:59:21"
Not presently implemented

.VARI JULIAN
Override the julian date.
Normally this is obtained from the input label(s).
Large floating value.
See JULDATE restrictions for project ID.

.VARI RA_V1
Override the space telescope V1 axis RA direction.
Normally this is obtained from the input label(s).
In degrees.

.VARI DEC_V1
Override the space telescope V1 axis Declination direction.
Normally this is obtained from the input label(s).
In degrees.

.VARI PA_V3
Override the space teleccope V3 axis position angle.
Normally this is obtained from the input label(s).
In degrees.
.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME        C KERNEL
  --------      -------------
  DAVI          MIPS_DAVI.CK
  NAV           MIPS_NAV.CK
  FARE          MIPS_FARENC.CK
  NAV2          MIPS_NAV2.CK
  NEAR          MIPS_NEAR.CK
  AMOS          MIPS_AMOS.CK
  NAIF          the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID    CKNAME        C KERNEL
  ----    --------      -------------
  M906    DAVI          MIPS_DAVI.CK
  M905    NAV           MIPS_NAV.CK
  M904    FARE          MIPS_FARENC.CK
  M903    NAV2          MIPS_NAV2.CK
  M902    NEAR          MIPS_NEAR.CK
  M901    AMOS          MIPS_AMOS.CK
  varies  NAIF          there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstperslab.pdf
procedure
  refgbl $echo
  RefGbl $SysChar
body
  Local InputGllDir  String
  Local InputVgrDir  String
  Local InputCasDir  String

  let $Echo="no"
  IF ($SysChar(1)="VAX_VMS")
    LET InputGllDir = "wms_test_work:[testdata.mipl.gll]"
    LET InputVgrDir = "wms_test_work:[testdata.mipl.vgr]"
    LET InputCasDir = "wms_test_work:[testdata.cassini.cas$i$ss]"
  ELSE
    LET InputGllDir = "/project/test_work/testdata/mipl/gll/"
    LET InputVgrDir = "/project/test_work/testdata/mipl/vgr/"
    LET InputCasDir = "/project/test_work/testdata/cassini/casIss/"
  END-IF

  write "===VOYAGER TEST"
  let $echo="yes"
 PERSLAB INP=&"InputVgrDir"f1636832.geo OUT=vgr.img TARGET=io
  let $echo="no"
  write "===Please visually verify that DIFPIC result is 0"
  let $echo="yes"
 DIFPIC  (&"InputVgrDir"f1636832.geo vgr.img)
  let $echo="no"
  write "===Please check VICAR label for the existance of the map project label"
  let $echo="yes"
 LABEL-LIST vgr.img
  let $echo="no"

  write "===GALILEO TEST"
  let $echo="yes"
 PERSLAB  INP=&"InputGllDir"venus.img OUT=gll.img 'remote
  let $echo="no"
  write "===Please visually verify that DIFPIC result is 0"
  let $echo="yes"
 DIFPIC (&"InputGllDir"venus.img gll.img)
  let $echo="no"
  write "===Please check VICAR label for the existance of the map project label"
  let $echo="yes"
 LABEL-LIST gll.img 
  let $echo="no"

  write "===CASSINI TEST"
  let $echo="yes"
 PERSLAB  INP=&"InputCasDir"n1354897340.1 OUT=cas.img 'remote
  let $echo="no"
  write "===Please visually verify that DIFPIC result is 0"
  let $echo="yes"
 DIFPIC (&"InputCasDir"n1354897340.1 cas.img)
  let $echo="no"
  write "===Please check VICAR label for the existance of the map project label"
  let $echo="yes"
 LABEL-LIST cas.img 
  let $echo="no"
end-proc
$ Return
$!#############################################################################
