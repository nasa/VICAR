ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get map projection information.
c
c Routines called:
c   mp_label_read: read map3 label
c   mp_mpo2buf: convert mp data to rdata (CONVEV format)
c   map_parms: if no map3 label, check for map projection parameters
c
c The remainder of the routine is an attempt to avoid calling SPICE by using
c parameters instead:
c   target_parms: get target radii
c   rs_parms: get spacecraft vector
c   sun_label: get solar lat-long from label or parameters
c
      subroutine map_label(iunit,itype,ind)
      implicit none
      integer iunit		!input image unit number
      integer itype		!map projection type
      integer ind		!=1 if map data is complete

      common/cmp/mp
      real*8 mp

      common/c2/sunlat,sunlon,sunrange
      real*8 sunlat,sunlon,sunrange

      common/c3/rdata
      real*4 rdata(40)
      integer idata(40)
      real*8 data(20),rs(3)
      real ra,rb,rc,lora	!target body radii, longitude of radius a
      real sclat,sclon,rmag	!spacecraft lat,long and distance
      equivalence (data,rdata,idata),(data(10),rs)
      equivalence (rdata(25),rc),(rdata(26),ra),(rdata(37),rb)
      equivalence (rdata(31),sclat),(rdata(32),sclon),(rdata(38),rmag)
      equivalence (rdata(36),lora)

      integer stat,cnt,def
      real far(2)

c Initialize all projection and lighting geometry as invalid...
      call mve(7,40,-999.,rdata,0,1)
      rdata(9) =0.              !default north angle (map projections)
      rdata(36) = 0.		!default lora

      sunlat = -999.
      sunlon = -999.
      sunrange = -999.
      call xvparm('SOLAR',far,cnt,def,' ')
      if (def.eq.0) then
         sunlat = far(1)
         sunlon = far(2)
      endif

      itype = 0                 !projection type is initially unknown

c Retrieve image geometry from map projection label (if present)
      call mp_init(mp,stat)
      if (stat.ne.0) call mabend('***MP error on init')
      call mp_label_read(mp,iunit,stat)
      if (stat.eq.0) then	!if map label found, copy to data buffer
         call sun_label(iunit,sunlat,sunlon)
         call mp_mpo2buf(mp,rdata,stat)
         if (stat.ne.0) call mabend('***Err converting MP to data buf')
         itype = idata(39)
      endif

      if (itype.eq.0) call map_parms(rdata,itype)
      idata(39) = itype

      ind = 0
      if (itype.eq.0) return		!skip if not a map projection

c For map-projected images, check if the illumination geometry has been
c specified via parameters:

      call target_parms(ra,rb,rc,lora)		!get target radii
      call rs_parms(sclat,sclon,rmag,rs)	!get spacecraft position

      if (ra.eq.-999. .or. rb.eq.-999. .or. rc.eq.-999.) return
      if (sclat.eq.-999. .or. sclon.eq.-999. .or. rmag.eq.-999.) return
      if (sunlat.eq.-999. .or. sunlon.eq.-999.) return

      ind = 1		!information complete, so we are done
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get map projection parameters.  rdata and itype may be updated.
c
      subroutine map_parms(rdata,itype)
      implicit none
      real rdata(40)
      integer itype

      integer cnt,def
      logical xvptst

c determine projection type
      if (xvptst('ORTHOGRA')) then
         itype = 2			!orthographic projection
         if(xvptst('POLE'))itype=1	!polar orthographic projection
      endif

      if (xvptst('STEREOGR')) then
         itype = 4			!stereographic projection
         if(xvptst('POLE'))itype=3	!polar stereographic projection
      endif

      if(xvptst('LAMBERT'))itype=5	!lambert projection
      if(xvptst('MERCATOR'))itype=6	!mercator projection
      if(xvptst('CYLINDRI'))itype=9	!cylindrical projection
      if(xvptst('RECTANGU'))itype=10 !rectangular,lat lon
      if(xvptst('LATLON'))itype=10

      if (itype.eq.0 .or. itype.eq.16) return	!skip if not standard projection

      call prnt(4,1,itype,'Map projection type specified=.')

c get projection parameters
      call xvparm('SAMPLE',rdata(1),cnt,def)	!special sample
      if (def.ne.0) call mabend('SAMPLE parameter not specified')

      call xvparm('LINE',rdata(2),cnt,def)	!special line
      if (def.ne.0) call mabend('LINE parameter not specified')

      call xvparm('LATITUDE',rdata(3),cnt,def)	!special latitude
      if (def.ne.0) call mabend('LATI parameter not specified')

      call xvparm('LONGITUD',rdata(6),cnt,def)	!special longitude 
      if (def.ne.0) call mabend('LONG parameter not specified')

      call xvparm('SCALE',rdata(7),cnt,def)	!scale (km/pixel)
      if (def.ne.0) call mabend('SCALE parameter not specified')

      call xvparm('NORTH',rdata(9),cnt,def)	!north angle (default=0.)

      if (itype.eq.1 .or. itype.eq.3) return	!skip rest if polar projection

      if (itype.eq.6) call mercpatch(rdata)

      if (itype.ne.5) return			!skip rest if not lambert

      call xvparm('PAR1',rdata(4),cnt,def)	!lat of parallel for lambert
      if (def.ne.0) call mabend('PAR1 parameter not specified')

      call xvparm('PAR2',rdata(5),cnt,def)	!lat of parallel for lambert
      if (def.ne.0) call mabend('PAR2 parameter not specified')
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get solar latitude and longitude from PERSLAB label.
c
      subroutine sun_label(iunit,sunlat,sunlon)
      implicit none
      integer iunit		!input image unit number
      real*8 sunlat,sunlon	!returned

      real r
      integer ind

      call xlget(iunit,'HISTORY','SUB_SOLAR_LATITUDE',r,
     +              ind,'FORMAT','REAL','HIST','PERSLAB',' ')
      if (ind.eq.1) sunlat=r

      call xlget(iunit,'HISTORY','SUB_SOLAR_LONGITUDE',r,
     +              ind,'FORMAT','REAL','HIST','PERSLAB',' ')
      if (ind.eq.1) sunlon=r
      return
      end
