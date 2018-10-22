c Draw dvectors from a mark file containing tiepoints.
      include 'VICMAIN_FOR'
      subroutine main44
      implicit integer*4 (a-z)
      character*5 project
      character*12 planet,labplanet
      character*36 picname
      character*20 dcfile,radfile,blemfile,shfile
      character*8 intape,outtape
      character*4 ckname,instrument,csource
      integer*4 data(80),buf(200),count,def
      real*8 rbuf(100)
      real*4 restab(2720),pldata(20),rdata(80)
      real*4 is_line,is_samp,os_line,os_samp
C     logical xvptst
      equivalence (buf,rbuf),(instrument,buf(2))
      equivalence (csource,buf(11)),(data,rdata)
      equivalence (labplanet,data(25)),(picname,data(31))
      equivalence (dcfile,data(40)),(radfile,data(45))
      equivalence (blemfile,data(50)),(shfile,data(55))
      equivalence (intape,data(60)),(outtape,data(63))

c****************
c      CALL INIT_SPICE
c****************
c/*****init_spice******/
c     call errprt('SET','NONE')
c      call xvmessage('after errprt ...',' ')
c      if (failed()) call reset()

c      call erract('SET','RETURN')
c      call xvmessage('after erract ...',' ')
c      if (failed()) call reset()

c      call vwait(1)
c      call clpool()
c      call rspool_1('BINPOOL')
c/*****end init_spice******/

C     call init_spice
      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      call xvmessage('  ',' ')

      call xvmessage('GETPROJ:',' ')
      call getproj(unit,project,camera,fds,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call xvmessage(project,' ')
      call prnt(4,1,camera,'camera serial number=.')
      call prnt(4,1,fds,   'frame number=        .')
      call xvmessage('  ',' ')

      call xvmessage('GETCAMCON:',' ')
      call getcamcon(project,camera,focal,oal,oas,scale,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(7,1,focal,'focal length=       .')
      call prnt(7,1,oal,  'optical axis line=  .')
      call prnt(7,1,oas,  'optical axis sample=.')
      call prnt(7,1,scale,'image scale=        .')
      call xvmessage('  ',' ')
           
      call xvmessage('GETLABCON:',' ')
      call getlabcon(unit,project,data,ind)      
      if(ind.eq.1) call prnt(4,1,ind,'warning indicator=.')
      if(ind.gt.1) call prnt(4,1,ind,'fatal indicator=.')
      if(data(1).eq.0) then
         call xvmessage('invalid label type',' ')
      else if(data(1).eq.1) then
         call xvmessage('ground calibration label',' ')
      else if(data(1).eq.2) then
         call xvmessage('flight label',' ')
      else
         call prnt(4,1,data(1),'data(1)=.')
      endif         

      call xvmessage('GETSPICE2:',' ')
      if(project.eq.'GLL  ') then
         call xvmessage('coordinate system is: J2000',' ')
      else
         call xvmessage('coordinate system is: B1950',' ')
      endif
c     CKNAME='    '
      call xvparm('CKNAME',CKNAME,count,def,1)
      if(CKNAME.eq.'    ')then
         call xvmessage('SEDR/SPICE CKNAME is defaulted to blanks',' ')
      else
         call xvmessage('SEDR/SPICE CKNAME set to '//CKNAME,' ')
      endif
      planet='            '

      call getspice2(unit,.true.,buf,ind)
      if (ind.EQ.-1000) then
	go to 100
      endif
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         call xvmessage('Central body= '//planet,' ')
      else
         call xvmessage('Target body= '//planet,' ')
      endif
      if(ind.ne.1) call prnt(4,1,ind,'fatal indicator=.')

      call xvmessage('GETPLACON:',' ')
      call xvmessage('Test from Fortran',' ')
      idnum=buf(9)
      call getplacon(labplanet,idnum,pldata,ind)
      if(ind.eq.1) call xvmessage('unrecognizable planet name',' ')
      if(ind.eq.2) call xvmessage('unrecognizable planet id#',' ')
      call xvmessage('Target planet body is:'//labplanet,' ')
      call prnt(4,1,idnum,    'Planet id number        .')
      call prnt(7,1,pldata(1),'equatorial radius-long  .')
      call prnt(7,1,pldata(2),'equatorial radius-short .')
      call prnt(7,1,pldata(3),'polar radius            .')
      call prnt(7,1,pldata(4),'longitude of long radius.')
      call prnt(7,1,pldata(5),'rotation period         .')
      call prnt(7,1,pldata(6),'solar range             .')
      call xvmessage('   ',' ')      

      call xvmessage('GETPLACON:',' ')
      call xvmessage('Test from C',' ')
      idnum=buf(9)
      call tzgetplacon(labplanet,idnum,pldata,ind)
      if(ind.eq.1) call xvmessage('unrecognizable planet name',' ')
      if(ind.eq.2) call xvmessage('unrecognizable planet id#',' ')
      call xvmessage('Target planet body is:'//labplanet,' ')
      call prnt(4,1,idnum,    'Planet id number        .')
      call prnt(7,1,pldata(1),'equatorial radius-long  .')
      call prnt(7,1,pldata(2),'equatorial radius-short .')
      call prnt(7,1,pldata(3),'polar radius            .')
      call prnt(7,1,pldata(4),'longitude of long radius.')
      call prnt(7,1,pldata(5),'rotation period         .')
      call prnt(7,1,pldata(6),'solar range             .')
      call xvmessage('   ',' ')      

      call xvmessage('GETGEOM:',' ')
      call xvpcnt('INP',count)
      if(count.gt.1)then
         call xvunit(unit2,'INP',2,status,' ')
         call xvopen(unit2,status,'OPEN_ACT','SA',' ')
         geomsor=1
         call xvmessage('obtaining geom parameters from input file',' ')
      else
         geomsor=0
         call xvmessage('obtaining geom parameters from nominals',' ')
      endif
      call getgeom(unit2,project,camera,geomsor,
     +             restab,restab,
     +             nah,nav,ind)
      if(ind.ne.0) call xvmessage('GETGEOM: error, ind=1',' ')
      call xvmessage('    ',' ')

      call xvmessage('CONVISOS:',' ')
      call xvmessage('assume image space l,s=400.,400.',' ')
      is_line=400.
      is_samp=400.
      nph=nah+1
      npv=nav+1
      mode=1
      call convisos(project,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call xvmessage('CONVISOS: error, ind=1',' ')
      call prnt(7,1,os_line,'object space line=.')
      call prnt(7,1,os_samp,'object space samp=.')
      mode=0
      call convisos(project,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call xvmessage('CONVISOS: error, ind=1',' ')
      call prnt(7,1,is_line,'image space line=.')
      call prnt(7,1,is_samp,'image space samp=.')
      call xvmessage('   ',' ')

C      if(xvptst('UPDATE'))then
C        call xvmessage('PUTSPICE2:',' ')
C        call xvparm('DESTINAT',csource,count,def,1) ! loads buf(11)
C        if(csource.eq.'    ') then
C           call xvmessage('must specify SPICE destination keyword',' ')
C           call abend
C        endif
C        call putspice2(csource,'tgetplacon',buf,ind)
C        if(ind.ne.1) call prnt(4,1,ind,'PUTSPICE: fatal indicator.')
C        if(ind.eq.1) call prnt(4,1,ind,'PUTSPICE: successful run.')
C      endif
100   if (mode.gt.0) then
      	call spcfexit(info)
      endif
      return
      end







