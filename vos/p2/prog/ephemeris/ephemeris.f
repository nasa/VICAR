c
c program ephemeris
c
      include 'VICMAIN_FOR'
      subroutine main44

      implicit real*8(a-h,o-z)
      character*100 msg,epoch,spicefile,object,viewer
      integer*4 def,count,object_num,viewer_num,h(2)
      integer*4 num,vnum
      logical failed
      real*8 lon,lat,light_time,state(6),tibf(3,3)

      call xvmessage('EPHEMERIS version 13 Sept 1998',' ')
      call init_spice

      call xvparm('SPICEFILE',spicefile,count,def,1)
      call spklef(spicefile,h(1))
      if (failed()) goto 900
c get parameters
      call xvparm('EPOCH',epoch,count,def,1)
      call xvparm('OBJECT',object,count,def,1)
      call xvparm('VIEWER',viewer,count,def,1)

c reads times like: asfd19920112044820a.vic
c with the time-stamp:  yyyymmddhhmmss
      i=index(epoch,'.vic')
      if(i.gt.0)then
        msg(1:2)=epoch(9:10) ! month
        msg(3:3)='/'
        msg(4:5)=epoch(11:12) ! day
        msg(6:6)='/'
        msg(7:10)=epoch(5:8) ! year
        msg(11:11)=' '
        msg(12:13)=epoch(13:14) ! hour
        msg(14:14)=':'
        msg(15:16)=epoch(15:16) ! min
        msg(17:17)=':'
        msg(18:19)=epoch(17:18) ! sec
        msg(20:100)='.'
        epoch(1:100)=msg(1:100)
      endif

c convert epoch to ephemeris time
      call utc2et(epoch,ephemeris_time)
      if (failed()) goto 901
c get the planet id #
      if(object.eq.'sun')object_num=10
      if(object.eq.'mercury')object_num=199
      if(object.eq.'venus')object_num=299
      if(object.eq.'earth')object_num=399
      if(object.eq.'moon')object_num=301
      if(object.eq.'mars')object_num=499
      if(object.eq.'jupiter')object_num=599
      if(object.eq.'saturn')object_num=699
      if(object.eq.'uranus')object_num=799
      if(object.eq.'neptune')object_num=899
      if(object.eq.'pluto')object_num=999
      if(viewer.eq.'sun')viewer_num=10
      if(viewer.eq.'mercury')viewer_num=199
      if(viewer.eq.'venus')viewer_num=299
      if(viewer.eq.'earth')viewer_num=399
      if(viewer.eq.'moon')viewer_num=301
      if(viewer.eq.'mars')viewer_num=499
      if(viewer.eq.'jupiter')viewer_num=599
      if(viewer.eq.'saturn')viewer_num=699
      if(viewer.eq.'uranus')viewer_num=799
      if(viewer.eq.'neptune')viewer_num=899
      if(viewer.eq.'pluto')viewer_num=999

c compute target state light time corrected at epoch from observer
      call spkez(object_num,ephemeris_time,'J2000','LT',viewer_num,
     +   state,light_time)
      if (failed()) then
         num = object_num
         vnum = viewer_num
         if (object_num.gt.10) num=object_num/100
         if (viewer_num.gt.10) vnum=viewer_num/100
         call xvmessage('Planet ephemerides not found',' ')
         call xvmessage('Using barycenter ephemerides',' ')
         call reset1
         call spkez(num,ephemeris_time,'J2000','LT',vnum,
     +      state,light_time)
         if (failed()) goto 902
      endif
c compute inertial to body-fixed rotation matrix tibf
      call bodmat(object_num,ephemeris_time - light_time,tibf)

c reverse state vector to from target to observer
      call vminus(state,state)

c rotate state into body-fixed
      call mxv(tibf,state,state)

c compute range to target, latitude & longitude of sub point
      call reclat(state,range,lon,lat)
      sclat=lat*dpr()
      sclon=lon*dpr()
      sclon=360.-sclon               ! convert to west
      if(sclon.gt.360.) sclon=sclon-360.
      if(sclon.lt.0.) sclon=sclon+360.

      write(msg,*)'Object center centric latitude=',sclat,' degrees'
      call xvmessage(msg,' ')
      write(msg,*)'Object center west longitude=',sclon,' degrees'
      call xvmessage(msg,' ')
      write(msg,*)'Object range= ',range,' km'
      call xvmessage(msg,' ') 
      return

  900 call xvmessage('***Err loading SP kernel',' ')
      goto 999
  901 call xvmessage('***Err converting UTC to ET',' ')
      goto 999
  902 call xvmessage('***Err getting ephemeris data',' ')
  999 call xvmessage('***Ephemeris task canceled',' ')
      call abend
      end
