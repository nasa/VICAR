C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                    PUTSPICE95
C
C  Modified 8/6/99 by Michael Brady
C  
C  Added the variable "proj_name".  Previously, the 
C  integer "project" was used as a parameter for
C  getlabcon and get proj, causing them to fail.
C
C********************************************************
C
	include 'VICMAIN_FOR'
	subroutine main44

        implicit none

        character*6 proj_name
	integer*4 mode
	character*80 imgfile
	character*10 source
	integer*4 project
	integer*4 system 
	character*10 usr_info(10)
	character*5 camera
        character*12 target_name
        character*12 temp
	double precision dbuf(100)
	integer*4 ibuf(200)
	integer*4 ind, count, def
	equivalence (dbuf, ibuf)
	integer*4 scet(6)
	integer*4 data(80)
        integer*4 unit, status, cam, fds

C	Selecting mode to tell putspice95() where to
C	write the data
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

        call xvmessage
     &     ('............................................', ' ')
        call xvmessage(' ',' ')
        call xvmessage('..................', ' ')
        call xvmessage('Calling GetSpice95', ' ')
	call getspice95(mode, project, camera, scet, target_name,
     &        system, source, usr_info, dbuf, ind)
        call xvmessage(' ',' ')

	if (ind .eq. 0) then
	   print*, 'ZGETSPICE95::Test From FORTRAN:FAILURE'
	else
           call xvparm('SOURCE', source, count, def, 0)
	   call mvcl(source, ibuf(11), 4)

        call xvmessage
     &     ('...... Testing PUTSPICE95 In Fortran .......', ' ')
	   ind  = 0
	   call putspice95(project, dbuf, mode, ind)
	   if (ind .eq. 0) then
	      print*, 'PUTSPICE95_ERROR:: Fortran Test FAILED'
	   else
              print*, 'PUTSPICE95:: Fortran Test SUCCESS'
	   endif
       endif

        call xvmessage('           ', ' ')
        call xvmessage
     &     ('...... Testing PUTSPICE95 In C .......', ' ')
        ind  = 0
        call tzputspice95(project, dbuf, mode, ind)
        if (ind .eq. 0) then
           print*, 'PUTSPICE_ERROR:: C Test FAILED'
        else
           print*, 'PUTSPICE95::C Test SUCCESS'
        endif

	return
	end
