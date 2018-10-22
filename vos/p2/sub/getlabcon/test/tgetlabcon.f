C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                    GETLABCON
C
C********************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44

      implicit integer*4 (a-z)
      integer*4 data(100)
      character*5 project

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      call xvmessage('**********fortran callable***********',' ')
      call getproj(unit,project,cam,fds,ind)
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
      call prnt(4,1,data(2), 'frame number          .')
      call prnt(7,1,data(3), 'exposure time  msec  .')
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
      call prnt(4,1,data(15),'Camera Flood State    .')
      call prnt(4,1,data(16),'DC Offset State       .')
      call prnt(4,1,data(17),'FIBE                  .')
      call prnt(4,1,data(18),'Boom flag             .')
      call prnt(4,1,data(19),'Image Scale  m/pixel  .')
      call prnt(4,1,data(20),'FOV Height            .')
      call prnt(4,1,data(21),'FOV Width             .')
      call prnt(4,1,data(22),'Range                 .')
      call prnt(4,1,data(23),'clock                 .')
      call prnt(4,1,data(24),'Frame Start Count     .')
      call prnt(99,12,data(25),'target body in label is: ')
      call prnt(7,1,data(28),'DN to reflectance IOVF.')
      call prnt(7,1,data(29),'DN to radiance    CONV.')
      call prnt(7,1,data(30),'Range target to sun   .')
      call prnt(99,6,data(60),'input tape name ')
      call prnt(4,1,data(62),'Input  file #         .')
      call prnt(99,6,data(63),'output tape name ')
      call prnt(4,1,data(65),'Output file #         .')
      call prnt(99,10,data(31),'picno ')
      call prnt(99,10,data(40),'dark calibration file ')
      call prnt(99,10,data(45),'radiance cal file ')
      call prnt(99,10,data(50),'blemish correction file ')
      call prnt(99,14,data(55),'shutter offset file ')
      call prnt(4,1,data(66),'Earth rcvd time year  .')
      call prnt(4,1,data(67),'Earth rcvd time day   .')
      call prnt(4,1,data(68),'Earth rcvd time hour  .')
      call prnt(4,1,data(69),'Earth rcvd time minute.')
      call prnt(4,1,data(70),'Earth rcvd time second.')
      call prnt(4,1,data(71),'Uneven bit weighting  .')
      call prnt(4,1,data(72),'Camera ID (1 or 2)    .')
      call prnt(4,1,data(73),'Partition(#RIM cycles).')
      call xvmessage('  ',' ')
      call xvmessage('**********c callable***********',' ')
      call tzgetlabcon(unit)
      return
      end
