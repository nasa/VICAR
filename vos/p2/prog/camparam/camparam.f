	include 'VICMAIN_FOR'

	subroutine main44

c       Revision History:
c       02 Jan 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting)
c------ program CAMPARAM 
    
c------ Program CAMPARAM will fill the LOCAL variables;
c------ "sc", "scan", "camera", "filter", "fds" and "exprng"
c------ and return the variables to the calling proc.  It will
c------ acquire the data via an able77 call on the VGR label.

	character*2 cam(2)
	character*4 irange,ISECCHR
	integer*4   able(19),parb(500)

	character*80 MSG
	character*4  ISECSTR
	character*3  flt(2,8)
        character*1  virgule
        integer      ist, unit, ind, isec, nc
        real         exp

        DATA FLT   / 'CHJ','CLR','BLU','VIO',
     1   	     'CLR','BLU','VIO','ORG',
     2      	     'NAD','CLR','GRN','GRN',
     3    	     'CH4','GRN','ORG','UV '/
C
	data cam     /'NA','WA'/
	data irange  /'    '/
        data isecchr /'    '/
        data isecstr /'    '/
        data virgule /'/'/
        msg = ' '
        call zia (able,19)
        call zia (parb,500)

        call ifmessage ('CAMPARAM version 02-Jan-95')

	call xvunit(unit,'INP',1,ist,' ')
	call xvopen(unit,ist,' ')
	able(1) = 19
	call able77v2(ind,unit,able)
c
        call mve (4,1,able(3),exp,1,1)

c-------calculate the exposure range
	NC = 4

	if (exp .lt. 23000.) then
		irange = 'A'
	else if(exp .lt. 62000.) then
		irange = 'B'
	else
                irange = 'C'
		ISEC = IFIX(EXP/1000.)
		IF (ISEC .LT. 100) then
                   NC = 2
                   write (ISECSTR(1:2),'(I2)') ISEC
                else IF (ISEC .LT. 1000)  then
                   NC = 3
                   write (ISECSTR(1:3),'(I3)') ISEC
                else 
                   write (ISECSTR(1:4),'(I4)') ISEC
		end if
                write (ISECCHR(1:4),'(A4)') ISECSTR(1:4)
      end if
      call xvmessage(' ',' ')
      call xvmessage('From the Voyager Label:',' ')

      call xvmessage
     &('   FDS    SC   CAMERA    FILTER    EXP(SEC)/RANGE  SCAN',' ')
      MSG = ' '
      WRITE (MSG(2:8),  '(I7)') ABLE(2)
      WRITE (MSG(12:12),'(I1)') ABLE(19)
      WRITE (MSG(36:43),'(F8.3)') EXP/1000.
      WRITE (MSG(53:54),'(I2)') ABLE(5)

      write (msg(18:19),'(A2)') cam(able(7)) 
      write (msg(27:29),'(A3)') flt(3-able(7),able(4)+1)
      write (msg(44:44),'(A1)') virgule
      IF (IRANGE .NE. 'C') THEN
                write (msg(45:48),'(A4)') irange
      ELSE
                write (msg(45:48),'(A4)') ISECCHR
      END IF
      call xvmessage (msg,' ')
c
      call xqini(parb,500,xabort)
      call xqintg(parb,'SC',1,able(19),xadd,ist)
      call xqintg(parb,'SCAN',1,able(5),xadd,ist)
      call xqstr(parb,'CAMERA',1,cam(able(7)),xadd,ist)
      call xqstr(parb,'FILTER',1,flt(3-able(7),able(4)+1),xadd,ist)
      call xqintg(parb,'FDS',1,able(2),xadd,ist)
      IF (IRANGE .NE. 'C') THEN
		call xqstr(parb,'EXPRNG',1,irange,xadd,ist)
      ELSE
		call xqstr(parb,'EXPRNG',1,ISECCHR,xadd,ist)
      END IF
      call xvqout(parb,ist)
      call xvclose(unit,ist,' ')
      return
      end
