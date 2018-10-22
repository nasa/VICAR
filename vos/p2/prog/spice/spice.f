c Draw dvectors from a mark file containing tiepoints.
      include 'VICMAIN_FOR'
      subroutine main44
      implicit integer*4 (a-z)
      character*5 project
      character*4 destination
      character*12 planet,labplanet
      character*36 picname
      character*20 dcfile,radfile,blemfile,shfile
      character*8 intape,outtape
      character*4 CKNAME,instrument,csource
      integer*4 data(80),buf(200),count,def
      real*8 rbuf(100)
      real*4 restab(2720),pldata(20),rdata(80)
      real*4 is_line,is_samp,os_line,os_samp
      logical xvptst
      equivalence (buf,rbuf),(instrument,buf(2))
      equivalence (csource,buf(11)),(data,rdata)
      equivalence (labplanet,data(25)),(picname,data(31))
      equivalence (dcfile,data(40)),(radfile,data(45))
      equivalence (blemfile,data(50)),(shfile,data(55))
      equivalence (intape,data(60)),(outtape,data(63))

      call init_spice
      call xvunit(unit,'INP',1,status, ' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      call qprint('  ')

      call qprint('GETPROJ:')
      call getproj(unit,project,camera,fds,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call qprint(project)
      call prnt(4,1,camera,'camera serial number=.')
      call prnt(4,1,fds,   'frame number=        .')
      call qprint('  ')

      call qprint('GETCAMCON:')
      call getcamcon(project,camera,focal,oal,oas,scale,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(7,1,focal,'focal length=       .')
      call prnt(7,1,oal,  'optical axis line=  .')
      call prnt(7,1,oas,  'optical axis sample=.')
      call prnt(7,1,scale,'image scale=        .')
      call qprint('  ')
           
      call qprint('GETLABCON:')
      call getlabcon(unit,project,data,ind)      
      if(ind.eq.1) call prnt(4,1,ind,'warning indicator=.')
      if(ind.gt.1) call prnt(4,1,ind,'fatal indicator=.')
      if(data(1).eq.0) then
         call qprint('invalid label type')
      else if(data(1).eq.1) then
         call qprint('ground calibration label')
      else if(data(1).eq.2) then
         call qprint('flight label')
      else
         call prnt(4,1,data(1),'data(1)=.')
      endif         
      call prnt(4,1,data(2), 'frame number          .')
      call prnt(7,1,rdata(3), 'exposure time         .')
      call prnt(4,1,data(4), 'filter position       .')
      call prnt(4,1,data(5), 'frame or scan rate    .')
      call prnt(4,1,data(6), 'camera serial number  .')
      call prnt(4,1,data(7), 'gain state            .')
      call prnt(4,1,data(8), 'S/C event time year   .')
      call prnt(4,1,data(9), 'S/C event time day    .')
      call prnt(4,1,data(10),'S/C event time hour   .')
      call prnt(4,1,data(11),'S/C event time minute .')
      call prnt(4,1,data(12),'S/C event time second .')
      call prnt(4,1,data(13),'S/C event time milsec .')
      call prnt(4,1,data(14),'S/C ID                .')
      call prnt(4,1,data(17),'FIBE                  .')
      call prnt(4,1,data(18),'Boom flag             .')
      call prnt(4,1,data(23),'clock                 .')
      call qprint('planet in label is: '//labplanet)
      call prnt(7,1,data(28),'DN to reflectance IOVF.')
      call prnt(7,1,data(29),'DN to radiance    CONV.')
      call prnt(7,1,data(30),'Range target to sun   .')
      call qprint('input tape name '//intape)
      call prnt(4,1,data(62),'Input  file #         .')
      call qprint('output tape name '//outtape)
      call prnt(4,1,data(65),'Output file #         .')
      if (data(31).eq.-999) data(31)=0
      call qprint('picno '//picname)
      if (data(40).eq.-999)  data(40)=0
      call qprint('dark calibration file '//dcfile)
      if (data(45).eq.-999)  data(45)=0
      call qprint('radiance cal file '//radfile)
      if (data(50).eq.-999)  data(50)=0
      call qprint('blemish correction file '//blemfile)
      if (data(55).eq.-999) data(55)=0
      call qprint('shutter offset file '//shfile)
      call prnt(4,1,data(66),'Earth rcvd time year  .')
      call prnt(4,1,data(67),'Earth rcvd time day   .')
      call prnt(4,1,data(68),'Earth rcvd time hour  .')
      call prnt(4,1,data(69),'Earth rcvd time minute.')
      call prnt(4,1,data(70),'Earth rcvd time second.')
      call prnt(4,1,data(71),'Uneven bit weighting  .')
      call prnt(4,1,data(72),'Camera ID (1 or 2)    .')
      call prnt(4,1,data(73),'Partition(#RIM cycles).')
      call qprint('  ')
     
      call qprint('GETSPICE2:')
      if(project.eq.'GLL  ') then
         call qprint('coordinate system is: J2000')
      else
         call qprint('coordinate system is: B1950')
      endif
c     CKNAME='    '
      call xvparm('CKNAME',CKNAME,count,def,0)
      if(CKNAME.eq.'    ')then
         call qprint('SEDR/SPICE CKNAME is defaulted to blanks')
      else
         call qprint('SEDR/SPICE CKNAME set to '//CKNAME)
      endif
      planet='            '
      call getspice2(unit,.true.,buf,ind)
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         call qprint('Central body= '//planet)
      else
         call qprint('Target body= '//planet)
      endif
      if(ind.ne.1) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(4,1,buf(1),  'spacecraft spice ID #=            .')
      call prnt(4,1,buf(13),  'SEDR update DDDYY                .')
      call qprint('Instrument= '//instrument)
      call prnt(4,1,buf(9),  'target body code                  .')
      call prnt(4,1,buf(3),  'measurement time year             .')
      call prnt(4,1,buf(4),  'measurement time day              .')
      call prnt(4,1,buf(5), 'measurement time hour              .')
      call prnt(4,1,buf(6), 'measurement time minute            .')
      call prnt(4,1,buf(7), 'measurement time second            .')
      call prnt(4,1,buf(8), 'measurement time millisecond       .')
      call prnt(4,1,buf(12), 'FDS or SCLK counter               .')
      call prnt(8,3,rbuf(16), 'xyz of s/c rel central body      .')
      call prnt(8,3,rbuf(19), 'xyz of picture body rel s/c      .')
      call prnt(8,1,rbuf(25), 's/c range from sun               .')
      call prnt(8,1,rbuf(26), 's/c range from central body      .')
      call prnt(8,1,rbuf(27), 's/c range from picture body      .')
      call prnt(8,2,rbuf(28), 'lat & lon of sun rel picture body.')
      call prnt(8,2,rbuf(30), 'lat & lon of s/c rel picture body.')
      call prnt(8,9,rbuf(41), 'C-matrix                         .')
      call prnt(8,2,rbuf(77), 'lat & lon of P5 point            .')
      call prnt(8,1,rbuf(79), 'incidence angle at P5 point      .')
      call prnt(8,1,rbuf(80), 'emission  angle at P5 point      .')
      call prnt(8,1,rbuf(81), 'phase     angle at P5 point      .')
      call prnt(8,2,rbuf(82), 'horiz & vert pixel size at P5    .')
      call prnt(8,9,rbuf(50), 'ME-matrix                        .')
      call prnt(8,1,rbuf(84),'s/c range to P5                   .')
      call prnt(8,1,rbuf(68),'north angle                       .')
      call qprint('SEDR CKNAME= '//csource)
      call prnt(8,1,rbuf(13),'picture body equat radius, long   .')
      call prnt(8,1,rbuf(14),'picture body equat radius, short  .')
      call prnt(8,1,rbuf(15),'picture body polar radius         .')
      call prnt(8,9,rbuf(59),'OM-matrix                         .')
      call prnt(8,3,rbuf(22),'RS-vector                         .')
      call prnt(8,1,rbuf(69),'line   of sub-s/c point           .')
      call prnt(8,1,rbuf(70),'sample of sub-s/c point           .')
      call qprint('  ')

      call qprint('GETPLACON:')
      idnum=buf(9)
      planet='            '
      call getplacon(planet,idnum,pldata,ind)
      if(ind.eq.1) call qprint('unrecognizable planet name')
      if(ind.eq.2) call qprint('unrecognizable planet id#')
      call qprint('Target planet body is:'//planet)
      call prnt(4,1,idnum,    'Planet id number        .')
      call prnt(7,1,pldata(1),'equatorial radius-long  .')
      call prnt(7,1,pldata(2),'equatorial radius-short .')
      call prnt(7,1,pldata(3),'polar radius            .')
      call prnt(7,1,pldata(4),'longitude of long radius.')
      call prnt(7,1,pldata(5),'rotation period         .')
      call prnt(7,1,pldata(6),'solar range             .')
      call qprint('   ')      


      call qprint('GETGEOM:')
      call xvpcnt('INP',count)
      if(count.gt.1)then
         call xvunit(unit2,'INP',2,status, ' ')
         geomsor=1
         call qprint('obtaining geom parameters from input file')
      else
         geomsor=0
         call qprint('obtaining geom parameters from nominals')
      endif
      call getgeom(unit2,project,camera,geomsor,
     +             restab,restab,
     +             nah,nav,ind)
      if(ind.ne.0) call qprint('GETGEOM: error, ind=1')
      call qprint('    ')

      call qprint('CONVISOS:')
      call qprint('assume image space l,s=400.,400.')
      is_line=400.
      is_samp=400.
      nph=nah+1
      npv=nav+1
      mode=1
      call convisos(project,camera,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call qprint('CONVISOS: error, ind=1')
      call prnt(7,1,os_line,'object space line=.')
      call prnt(7,1,os_samp,'object space samp=.')
      mode=0
      call convisos(project,camera,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call qprint('CONVISOS: error, ind=1')
      call prnt(7,1,is_line,'image space line=.')
      call prnt(7,1,is_samp,'image space samp=.')
      call qprint('   ')

      if(xvptst('UPDATE'))then
        call qprint('PUTSPICE2:')
        call xvparm('DESTINAT',destination,count,def,0) ! loads buf(11)
        if(csource.eq.'    ') then
           call qprint('must specify SPICE destination keyword')
           call abend
        endif
        call putspice2(destination,'SPICE',buf,ind)
        if(ind.ne.1) call prnt(4,1,ind,'PUTSPICE: fatal indicator.')
        if(ind.eq.1) call prnt(4,1,ind,'PUTSPICE: successful run.')
      endif

      return
      end


      subroutine qprint (message)
      character *(*) message

      call xvmessage ( message , ' ')
      return
      end













































