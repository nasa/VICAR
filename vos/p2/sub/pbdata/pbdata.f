C Returns picture body data given pb name (12 characters)

c BUF contains:
c  1-3. Triaxial radii (RA,RB,RC) in km
c  4. Longitude of longest axis (LORA in VICAR, BODY_LONG_AXIS in SPICE), in degrees
c  5. Rotation period in days
c  6. Mean solar distance in km

c In the current implementation, 1-3 and 5 are retrieved from SPICE.
c 4 (LORA) was always zero in the old PBDATA and is zero in all current SPICE kernels
c (and is also deprecated by NAIF), so is simply set to zero.
c 6 (solar distance) is hard-coded in the same manner as was done in the old PBDATA.

c The old code for PBDATA is included in pbdata.com as pbdata_old.f

      SUBROUTINE PBDATA(name,BUF,*)

      REAL*4 BUF(*)
      REAL*4 SRANGE(11)
      character*12 name
      real*8 rad(3)
      logical found, failed
      external failed

C Mean solar ranges of the planets (AU) plus Gaspra & Ida:
      data SRANGE/ 0.387098, 0.723331, 1.0, 1.523679, 5.2027, 9.546,
     &   19.2, 30.09, 39.5, 2.2016, 2.9485/

      call bodn2c( name, id, found)
      if (.not.found) return 1

c  initialize buffer to zero
      do i=1,6
        buf(i) = 0.0
      enddo

c  get radii from SPICE:
      call bodvrd( name, 'RADII', 3, n, rad)
      if (failed()) then
        call reset()
      else
        do i=1,n
          buf(i) = rad(i)
        enddo
      endif

c  get orientation of Prime Meridian, 2nd entry is rotation rate in deg/day:
      call bodvrd( name, 'PM', 3, n, rad)
      if (failed()) then
        call reset()
      else
        buf(5) = 360.0/rad(2)
      endif

c  for ID = 1-9:  use SRANGE(ID)
c         = 100-999:  use SRANGE(ID/100)
c  for Gaspra/Ida: special code.
      buf(6) = 0.0
      k = id
      if (name.eq.'GASPRA') then
        k = 10
      elseif (name.eq.'IDA') then
        k = 11
      elseif (k.ge.10) then
        k = k/100
        if (k.lt.1 .or. k.gt.9) return
      endif
      BUF(6) = 149597871.D0*SRANGE(K)	!Solar range in km

      RETURN
      END
