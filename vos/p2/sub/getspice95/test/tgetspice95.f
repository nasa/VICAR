C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                    GETSPICE95
C
C
C Change:  MPB  8/5/99
C       Created the string proj_name.  Previously, calls to 
C       getproj() and getlabcon() were using 
C       the integer 'project' as a parameter, 
C       which was causing them to fail in IRIX.
C
C********************************************************
C
	include 'VICMAIN_FOR'
	subroutine main44

        character*6 proj_name
	integer*4 mode
	character*80 imgfile
	character*10 source
	integer*4 project
	character*10 usr_info(10)
	double precision dbuf(100)
	integer*4 system
	integer*4 scet(6)
	integer*4 ibuf(200)
	integer*4 ind, count, def
	equivalence (dbuf, ibuf)
	character*5 camera
	character*12 target_name
	character*30 temp
	integer*4 data(80)

C	Selecting mode to tell getspice95()
C	where to get the data
	print *, 'Input mode (local = 0, remote = 1)'
	read *, mode

	call xvunit(unit, 'INP', 1, status, ' ')
	call xvopen(unit, status, 'OPEN_ACT', 'SA', ' ')
	call getproj(unit, proj_name, cam, fds, ind)
	call getlabcon(unit, proj_name, data, ind)

	call mve(4, 6, data(8), scet, 1, 1)
	call mvlc(data(25), target_name, 12)

        call xvparm('INP', imgfile, count, def, 0)
        call xvparm('SOURCE', source, count, def, 0)
	call xvparm('PROJECT', project, count, def, 0)
	call xvparm('CAMERA', camera, count, def, 0)
	call xvparm('TARGET', temp, count, def, 0)

	if (def .EQ. 0) then
	   target_name=temp
	endif

	call xvparm('INSTITUTE', usr_info(1), count, def, 0)
	call xvparm('PURPOSE', usr_info(2), count, def, 0)
	call xvparm('PROG_NAME', usr_info(3), count, def, 0)
	call xvparm('SP_REF', usr_info(4), count, def, 0)
	call xvparm('REQ_NO', usr_info(5), count, def, 0)
	call xvparm('YEAR', usr_info(6), count, def, 0)
	call xvparm('MONTH_DAY', usr_info(7), count, def, 0)
	call xvparm('HOUR_MIN', usr_info(8), count, def, 0)
	call xvparm('FILE_ID', usr_info(9), count, def, 0)
	call xvparm('USR_GRP_ID', usr_info(10), count, def, 0)

	if (project .eq. -77) then
	   system = 2
	else
	   system = 1
	endif

	call xvmessage(' ', ' ')
	call xvmessage('....................................', ' ')
        if (mode .eq. 0) then
           call xvmessage('GETSPICE95::MODE_LOCAL Fortran Test', ' ')
        else if (mode .eq. 1) then
	   call xvmessage('GETSPICE95::MODE_REMOTE Fortran Test', ' ')
	endif

	call getspice95(mode, project, camera, scet, target_name,
	1	system, source, usr_info, dbuf, ind)

	if (ind .eq. 0) then
	   call xvmessage('ZGETSPICE95::Test From FORTRAN:FAILURE', ' ')
	else
	   call xvmessage('ZGETSPICE95::Test From FORTRAN:SUCCESS', ' ')
	   temp = ' '
	   call mvlc(ibuf(2), temp, 4)
	   print*, 'Instrument                    : ', temp
	   print*, 'measurement time year         : ', ibuf(3)
	   print*, 'measurement time day          : ', ibuf(4)
	   print*, 'measurement time hour         : ', ibuf(5)
	   print*, 'measurement time minute       : ', ibuf(6)
	   print*, 'measurement time second       : ', ibuf(7)
	   print*, 'measurement time millisec     : ', ibuf(8)
	   print*, 'Target Body Code (target_id)  : ', ibuf(9)
	   
	   print*, ibuf(10)

	   if (ibuf(10) .eq. 2) then
	      print*, 'Coordinate System             : B1950'
	   else if (ibuf(10) .eq. 1) then
	      print*, 'Coordinate System             : J2000'
	   endif
	   print*, ' '
	   print*, 'XYZ of SC relative central body: '
	   print*, '       ', dbuf(16), dbuf(17), dbuf(18)
	   print*, '           '
	   print*, 'XYZ of picture body relative to SC: '
	   print*, '       ', dbuf(19), dbuf(20), dbuf(21)
	   print*, ' '
	   print*, 'SC range from sun           : ', dbuf(25)
	   print*, 'SC range from central body  : ', dbuf(26)
	   print*, 'SC range from picture body  : ', dbuf(27)
	   print*, ' '
	   print*, 'lat & lon of sun rel pic body: '
	   print*, '        ', dbuf(28), dbuf(29)
	   print*, ' '
	   print*, 'lat & lon of SC rel pic body : '
	   print*, '        ', dbuf(30), dbuf(31)
	   print*, ' '
	   print*, 'C_MATRIX: '
	   print*, dbuf(41), dbuf(42), dbuf(43)
	   print*, dbuf(44), dbuf(45), dbuf(46)
	   print*, dbuf(47), dbuf(48), dbuf(49)
	   print*, ' '
	   print*, 'lat & lon at P5 point: ', dbuf(77), dbuf(78)
	   print*, ' '
	   print*, 'incidence angle at P5 point: ', dbuf(79)
	   print*, 'emission angle at P5 point : ', dbuf(80)
	   print*, 'phase angle at P5 point    : ', dbuf(81)
	   print*, 'Hor & Vert pix size at P5  : ', dbuf(82), dbuf(83)
	   print*, '           '
	   print*, 'ME_MATRIX: '
	   print*, dbuf(50), dbuf(51), dbuf(52)
	   print*, dbuf(53), dbuf(54), dbuf(55)
	   print*, dbuf(56), dbuf(57), dbuf(58)
	   print*, '            '
	   print*, 'SC range to P5:	', dbuf(84)
	   print*, 'North Angle   :     ', dbuf(68)
	   print*, '            '
	   print*, 'Picture body equat radius, long : ', dbuf(13)
	   print*, 'Picture body equat radius, short: ', dbuf(14)
	   print*, 'Picture body equat radius       : ', dbuf(15)
	   print*, ' '
	   print*, 'OM_MATRIX: '
	   print*, dbuf(59), dbuf(60), dbuf(61)
	   print*, dbuf(62), dbuf(63), dbuf(64)
	   print*, dbuf(65), dbuf(66), dbuf(67)
	   print*, ' '
	   print*, 'RS-Vector: ', dbuf(22), dbuf(23), dbuf(24)
	   print*, 'line   sub-sc-point   : ', dbuf(69)
	   print*, 'sample sub-sc-point   : ', dbuf(70)

	   temp = '               '
	   call mvlc(ibuf(189), temp, 4)
	   print*, 'Institute          : ', temp
	   temp = '               '
	   call mvlc(ibuf(169), temp, 4)
	   print*, 'year               : ', temp
	   temp = '               '
	   call mvlc(ibuf(170), temp, 4)
	   print*, 'month-day          : ', temp
           temp = '               '
	   call mvlc(ibuf(171), temp, 4)
	   print*, 'hour-min           : ', temp
           temp = '               '
	   call mvlc(ibuf(14), temp, 4)
	   print*, 'SPK_ref            : ', temp
	   temp = '               '
	   call mvlc(ibuf(172), temp, 4)
	   print*, 'CK_ref             : ', temp
           temp = '               '
	   call mvlc(ibuf(173), temp, 4)
	   print*, 'purpose            : ', temp
           temp = '               '
	   call mvlc(ibuf(174), temp, 6)
	   print*, 'prog_name          : ', temp
           temp = '               '
	   call mvlc(ibuf(176) ,temp, 4)
	   print*, 'job_req_no,        : ', temp
           temp = '               '
	   call mvlc(ibuf(177), temp, 6)
	   print*, 'usr_grp_id         : ', temp
	endif

	call xvmessage('             ', ' ')
	call xvmessage( '..................................', ' ')
	call xvmessage('GETSPICE95::Test From C....', ' ')
        call tzgetspice95(mode, project, camera, scet, target_name,
	1	system, source, usr_info, dbuf, ind)

        if (ind .eq. 1) then
	   call xvmessage('GETSPICE95::Test SUCCESS', ' ')
	else
	   call xvmessage('Fatal Indicator::Test Failed', ' ')
	endif

	return
	end
