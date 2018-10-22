c  program CTIM -- Convert TIMes

c  converts between various time systems used by NAIF SPICE

c  the SCLK kernel appropriate to the requested project must be
c  assigned to the logical name SCLK
c  (s/c id codes are in the file with logical name BODY_IDS)

c  ?91   -lwk-  initial version
c  28jun93 -lwk- added "ET Julian date" option
c  19apr96 -lwk- replace "GLL" with general project code
c  28nov00 -lwk- replaced ldpool calls with init_spice for unix

c  5 systems are supported:
c    1) character SCLK  (rim.mod91.mod10.mod8 or something like that)
c    2) double-precision SCLK, or "ticks" since start of mission
c    3) UTC (YY-DDD//HH:MM:SS)
c    4) ET2000 (seconds before J2000)
c    5) ET1950 (seconds after B1950)
c    6) ET Julian date (JED)

c  the following is a table of NAIF calls to convert between these:

c    OUT =     SCLKCH    SCLKDP     UTC      ET
c  IN:
c   SCLKCH        X      SCENCD      --     SCS2E
c   SCLKDP      SCDECD     X         --     SCT2E
c      UTC       --       --          X    UTC2ET
c       ET       SCE2S    SCE2T    ET2UTC     X

c  ("X" means not applicable, "--" means not available.)  So the
c  only route to/from UTC is via ET, otherwise there are calls for
c  all transitions.

c  There are no calls to convert to/from ET1950 or JED;  instead this is done 
c  by a formula based on ET.

	implicit real*8 (a-h,o-z)
	real*8 jed, j2000
	character*18 sclkc
	character*24 utc
c	character*4 project
	integer sc_id/0/		! s/c id for SCLK

c  initialize SPICE processing:
	call init_spice

c	print*,' enter project (4-char: GLL,VGR1,...):'
c	read(5,1001) project
c	call get_bdy_ids( project, sc_id)	! a MIPS routine, not NAIF!!
	print*,' enter project id (-77 = GLL, -31 = VGR1, ...):'
	read(5,*) sc_id

1	print*,' enter input time format:'
	print*,' 1 = SCLK (char),  2 = SCLK (ticks), 3 = UTC,
     1 4 = ET2000, 5 = ET1950, 6 = JED'
	read(5,*) modin
	if (modin.eq.1) then
	  print*,' enter RIM,MOD91,MOD10,MOD8 separated by "." or space:'
	  read(5,1004) sclkc
	elseif (modin.eq.2) then
	  print*,' enter double-precision SCLK:'
	  read(5,*) sclkdp
	elseif (modin.eq.3) then
	  print*,' enter UTC as YY-DDD//HH:MM:SSS.SS, or JDnnnnnn.nn:'
	  read(5,1001) utc
	elseif (modin.eq.4) then
	  print*,' enter Ephemeris Time (seconds before J2000):'
	  read(5,*) et
	elseif (modin.eq.5) then
	  print*,' enter 1950 Ephemeris Time (seconds after B1950):'
	  read(5,*) et1950
	elseif (modin.eq.6) then
	  print*,' enter ET Julian date (e.g., 2450376.482):'
	  read(5,*) jed
	else
	  print*,' invalid format'
	  go to 1
	endif

10	print*,' enter output time format:'
	print*,' 1 = SCLK (char),  2 = SCLK (ticks), 3 = UTC,
     1 4 = ET2000, 5 = ET1950, 6 = JED'
	read(5,*) modout

	if (modout.eq.1) then
	  if (modin.eq.1) then
	    print*,' no conversion requested'
	    go to 1
	  elseif (modin.eq.2) then
	    call scdecd( sc_id, sclkdp, sclkc)
	  elseif (modin.eq.3) then
	    call utc2et( utc, et)
	    call sce2s( sc_id, et, sclkc)
	  elseif (modin.eq.4) then
	    call sce2s( sc_id, et, sclkc)
	  elseif (modin.eq.5) then
	    et = et1950-1577880000.0
	    call sce2s( sc_id, et, sclkc)
	  elseif (modin.eq.6) then
	    et = (jed-j2000())*spd()
	    call sce2s( sc_id, et, sclkc)
	  endif
	  write(6,1002) sclkc

	elseif (modout.eq.2) then
	  if (modin.eq.1) then
	    call scencd( sc_id, sclkc, sclkdp)
	  elseif (modin.eq.2) then
	    print*,' no conversion requested'
	    go to 1
	  elseif (modin.eq.3) then
	    call utc2et( utc, et)
	    call sce2t( sc_id, et, sclkdp)
	  elseif (modin.eq.4) then
	    call sce2t( sc_id, et, sclkdp)
	  elseif (modin.eq.5) then
	    et = et1950-1577880000.0
	    call sce2t( sc_id, et, sclkdp)
	  elseif (modin.eq.6) then
	    et = (jed-j2000())*spd()
	    call sce2t( sc_id, et, sclkdp)
	  endif
	  print*,' SCLK ticks =', sclkdp

	elseif (modout.eq.3) then
	  if (modin.eq.1) then
	    call scs2e( sc_id, sclkc, et)
	  elseif (modin.eq.2) then
	    call sct2e( sc_id, sclkdp, et)
	  elseif (modin.eq.3) then
	    print*,' no conversion requested'
	    go to 1
	  elseif (modin.eq.5) then
	    et = et1950-1577880000.0
	  elseif (modin.eq.6) then
	    et = (jed-j2000())*spd()
	  endif
	  call et2utc( et, 'D', 2, utc)
	  write(6,1003) utc

	elseif (modout.eq.4) then
	  if (modin.eq.1) then
	    call scs2e( sc_id, sclkc, et)
	  elseif (modin.eq.2) then
	    call sct2e( sc_id, sclkdp, et)
	  elseif (modin.eq.3) then
	    call utc2et( utc, et)
	  elseif (modin.eq.4) then
	    print*,' no conversion requested'
	    go to 1
	  elseif (modin.eq.5) then
	    et = et1950-1577880000.0
	  elseif (modin.eq.6) then
	    et = (jed-j2000())*spd()
	  endif
	  print*,' ET2000 =', et

	elseif (modout.eq.5) then
	  if (modin.eq.1) then
	    call scs2e( sc_id, sclkc, et)
	  elseif (modin.eq.2) then
	    call sct2e( sc_id, sclkdp, et)
	  elseif (modin.eq.3) then
	    call utc2et( utc, et)
	  elseif (modin.eq.5) then
	    print*,' no conversion requested'
	    go to 1
	  elseif (modin.eq.6) then
	    et = (jed-j2000())*spd()
	  endif
	  et1950 = et+1577880000.0
	  print*,' ET1950 =', et1950

	elseif (modout.eq.6) then
	  if (modin.eq.1) then
	    call scs2e( sc_id, sclkc, et)
	  elseif (modin.eq.2) then
	    call sct2e( sc_id, sclkdp, et)
	  elseif (modin.eq.3) then
	    call utc2et( utc, et)
	  elseif (modin.eq.5) then
	    et = et1950-1577880000.0
	  elseif (modin.eq.6) then
	    print*,' no conversion requested'
	    go to 1
	  endif
	  jed = j2000() + et/spd()
	  print*,' JED =', jed

	else
	  print*,' invalid format'
	  go to 10
	endif

	print*,' enter 1 to quit, <CR> for another conversion:'
	read(5,1000) i
	if (i.eq.0) go to 1
	call exit

1000	format(i1)
1001	format(a24)
1002	format(' SCLK = ', a18)
1003	format(' UTC = ', a24)
1004	format(a18)
	end
