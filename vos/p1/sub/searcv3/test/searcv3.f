c
c MIPS VICAR routine SEARCV3
c
c  This routine should never be called directly by an application
c  program.  It is used by the map label readers to process IBM (old-style)
c  map projection labels.
c
c Calling Sequence
c
c  CALL SEARCV3( UNIT, DATA, DATA, STATUS)
c
c  Arguments: UNIT   - input I*4 - unit of file to be searched
c             DATA   - output *4 (40) - MAP data buffer
c             STATUS - output I*4 - status code
c History
c
c  Original Programmer: 		L.W.Kamp
c  Current Cognizant Programmer:	L.W.Kamp
c  Date written:			18 September 1993 (based on SEARCV2)
c  Source Language: 			FORTRAN
c
c  Revisions:				
c
c  19sep93 -lwk- this is the old SEARCV2 code with a simplified parameter
c		 list, to be called by MP_LABEL_READ and the new SEARCV2
c  01dec93 -lwk- replaced XLGETLABEL by multiple XLGETs
c  11jan94 -lwk- fixed bugs in Mercator case and for cases where map labels
c		 are not in the last task of the label
c  16feb94 -lwk- fixed bug getting SCALE in simple cylindrical
c  25apr94 -jfm- revised test pdf to be useful on all platforms
c  13mar95 -jfm- added retrieval of PXLS/DEG to functionality
c		 for projections: MERCATOR, TRANSVERSE MERCATOR, 
c		 CYLINDRICAL EQUAL AREA, and SIMPLE CYLINDRICAL.
c  04apr95 -lwk- fixed "Instances" in xlhinfo/xlget calls
c  26apr95 -ffm- initialize nc to 0 if CANNOT_FIND_KEY(-38)
c		 initialize bufsize to 0 

c
      SUBROUTINE SEARCV3( UNIT, DATA, IDATA, ISTAT)
c
c  NOTE:  in the original code, when multiple tasks had performed map
c  projections in the history, the Radii are taken from the *first* one
c  encountered (even though the current map projection is taken from the
c  last)!  This is considered to be a bug (although it will be very rare
c  to change the radius of the target body when reprojecting) and has
c  been revised in this version.

C   LABEL SEARCH FOR MAP3 PARAMETERS:
C     DATA(1)      XC        SPECIAL SAMPLE POINT
C     DATA(2)      ZC        SPECIAL LINE POINT
C     DATA(3)      THETA0    SPECIAL LATITUDE POINT DEG.
C     DATA(4)      THETA1    LATITUDE OF SPECIAL PARALLEL DEG.
C     DATA(5)      THETA2    LATITUDE OF SPECIAL PARALLEL DEG.
C     DATA(6)      LAMDA0    SPECIAL LONGITUDE DEG.
C     DATA(7)      F         SCALE KM/PIXEL
C     DATA(8)      CAS       +1 IF VISIBLE POLE IS N.  -1 IF SOUTH
C     DATA(9)      PSI       NORTH ANGLE DEG.
C     DATA(10)     RES	     MAP RESOLUTION IN PXLS/DEG
C     DATA(25)               polar radius
C     DATA(26)               equatorial radius
C     DATA(39)     PROJECTION TYPE OF PICTURE I*4
C                  POLAR ORTHOGRAPHIC	        = 1
C                  OBLIQUE ORTHOGRAPHIC   	= 2
C                  POLAR STEREOGRAPHIC  	= 3
C                  OBLIQUE STEREOGRAPHC	        = 4
C                  LAMBERT		        = 5
C                  MERCATOR		        = 6
C                  IMAGE SPACE		        = 7
C                  OBJECT SPACE		        = 8
C                  NORMAL CYLINDRICAL	        = 9
C                  SIMPLE CYLINDRICAL	        = 10
C                  OBLIQUE SIMPLE CYLINDRICAL	= 11
C                  SINUSOIDAL		        = 12
C                  OBLIQUE SINUSOIDAL	        = 13
C                  MOLLWEIDE		        = 14
C		   TRANSVERSE MERCATOR	        = 15
C                  PERSPECTIVE                  = 16
C
      parameter (maxtasks=100)

      CHARACTER*16 MERCAT,LAMBER
      CHARACTER*16 ORTHOG,STEREO
      CHARACTER*16 CYLIND,PSTER
      CHARACTER*16 PORTH,OBCYL
      CHARACTER*16 LATL
      CHARACTER*16 MOLL
      CHARACTER*16 SINU
      CHARACTER*16 OBSINU
      CHARACTER*16 TVERSMER
      CHARACTER*16 PERSPECTIVE
      CHARACTER*16 OBJECTSP
      CHARACTER*8 GEOMA,MGEOM,GEOM
      CHARACTER*8 GEOMAR,FARENC
      CHARACTER*8 FARENX
      CHARACTER*8 FARENY,FARENZ
      CHARACTER*8 NORTH,SOUTH
      CHARACTER*10 MAP2

      REAL*4 DATA(40),RBUF(4)

      INTEGER IDATA(40),UNIT
      INTEGER BUFSIZE
c
c  max buffer needed is 7 MAPxxx labels of 70 bytes each, plus 100 bytes pad:
c
      parameter (maxbuf=600)
      CHARACTER*600 WORK
      character*6 mlab
      character*32 tasks(maxtasks)
      integer inst(maxtasks)

      LOGICAL maptask,more

C was previously in type declaration
C changed by Roatsch
      MERCAT = '***MERCATOR PROJ'
      LAMBER = '*** LAMBERT CONF'
      ORTHOG = '*** ORTHOGRAPHIC'
      STEREO = '*** STEREOGRAPHI'
      CYLIND = '*** CYLINDRICAL'
      PSTER  = '*** POLAR STEREO'
      PORTH  = '*** POLAR ORTHOG'
      OBCYL  = '*** OBLIQUE SIMP'
      LATL   = '*** SIMPLE CYLIN'
      MOLL   = '*** MOLLWEIDE PR'
      SINU   = '*** SINUSOIDAL P'
      OBSINU = '*** OBLIQUE SINU'
      TVERSMER = '***TRANS MERCATO'
      PERSPECTIVE = '*** PERSPECTIVE '
      OBJECTSP = '*** OBJECT SPACE'
      GEOMA   = 'GEOMA   '
      MGEOM   = 'MGEOM   '
      GEOM    = 'GEOM    '
      GEOMAR  = 'GEOMAR  '
      FARENC  = 'FARENC  '
      FARENX  = 'FARENCX '
      FARENY  = 'FARENCY '
      FARENZ  = 'FARENCZ '
      NORTH   = 'NORTH   '
      SOUTH   = 'SOUTH   '
      MAP2    = 'MAP2 LABEL'
      
c
c  determine how many tasks are present and search them for MAP labels:
c
      ntasks = maxtasks		! on input, set to max. value
      call xlhinfo( unit, tasks, inst, ntasks, istat, 
     & 			'ERR_ACT', ' ', ' ')
c     replace call to chkstat, roatsch march 2000
c      call chkstat( istat,' ** too many tasks, SEARCV3 ***',1)
      if (status.ne.1) then
         call xvmessage(' ** too many tasks, SEARCV3 ***',' ')
         call abend
      endif
      
c     continues original code

      mlab = 'MAP001'
      imap = 1		! index of MAPxxx, which is never reset
      bufsize = 0
      do i=1,ntasks
	j = 1		! index in WORK buffer, reset each new task
	more = .true.
	maptask = .false.
	do while (more)
	  call xlget( unit, 'HISTORY', mlab, work(j:), istat, 'LENGTH',
     &     nc, 'FORMAT', 'STRING', 'HIST', tasks(i), 'INSTANCE', 
     &     inst(i), 'ERR_ACT', ' ', ' ')
	  if (istat.eq.-38) nc=0
	  if (j+nc-1.gt.maxbuf-100) then
	    call xvmessage('*** SEARCV3: label buffer overflow ***',' ')
	    j = maxbuf-99-nc
	  endif
	  if (istat.eq.1) then
	    maptask = .true.
	    j = j+nc
	    if (j.gt.maxbuf-150) more = .false.
	    imap = imap+1
	    write(mlab(4:),'(i3.3)') imap	! force leading zeroes
	  else
	    more = .false.
	  endif
	enddo
	if (maptask) then
	  if (j.le.maxbuf) work(j:) = ' '	! clear end of buffer
	  bufsize = j-1
	endif
      enddo
      if (bufsize.le.0) then
c	call xvmessage(' MAP3 LABELS NOT FOUND',' ')
	go to 950	! leave ISTAT as XLGET set it
      endif

      istat = 1
      DATA(8)=1.

      if (INDEX(WORK,PORTH).gt.0) then
	idata(39) = 1
      else if(INDEX(WORK,ORTHOG).gt.0) then
	idata(39) = 2
      else if(INDEX(WORK,PSTER).gt.0) then
	idata(39) = 3
      else if(INDEX(WORK,STEREO).gt.0) then
	idata(39) = 4
      else if(INDEX(WORK,LAMBER).gt.0) then
	idata(39) = 5
      else if(INDEX(WORK,MERCAT).gt.0) then
	idata(39) = 6
      else if(INDEX(WORK,CYLIND).gt.0) then
	idata(39) = 9
      else if(INDEX(WORK,LATL).gt.0) then
	idata(39) = 10
      else if(INDEX(WORK,OBCYL).gt.0) then
	idata(39) = 11
      else if(INDEX(WORK,SINU).gt.0) then
	idata(39) = 12
      else if(INDEX(WORK,OBSINU).gt.0) then
	idata(39) = 13
      else if(INDEX(WORK,MOLL).gt.0) then
	idata(39) = 14
      else if(INDEX(WORK,TVERSMER).gt.0) then
	idata(39) = 15
      else if(INDEX(WORK,PERSPECTIVE).gt.0 .or.
     &        INDEX(WORK,OBJECTSP).gt.0) then
	idata(39) = 16
      else			! could not locate a projection in label
	istat = -1
	go to 9500
      ENDIF
c
c  get radii
c
      KK=INDEX(WORK(:BUFSIZE),'RADII=(')
      read(work(kk+7:),'(3(f8.1,1x))') (rbuf(i),i=1,3)
      data(25) = rbuf(3)                  ! polar radius
      data(26) = (rbuf(1)+rbuf(1))/2.            ! equatorial radius

 6190 CONTINUE      
      L=IDATA(39)
      GO TO(6210,6220,6230,6240,7000,8000,9500,9500,9000,9100,
     1      6280,6250,6260,6270,8100,8500),L
      go to 9500	! (should be a redundant check)

 6210 CONTINUE
C
C     POLAR ORTHOGRAPHIC PROJECTION. #1
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE
      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6220 CONTINUE
C
C     OBLIQUE ORTHOGRAPHIC PROJECTION. #2
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3,16x,f7.3)') (rbuf(i),i=1,2)
      DATA(7)=RBUF(1)		! SCALE
      DATA(9)=RBUF(2)		! NORTH ANGLE
      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6230 CONTINUE
C
C     POLAR STEREOGRAPHIC PROJECTION. #3
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6240 CONTINUE
C
C     OBLIQUE STEREOGRAPHIC PROJECTION. #4
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3,16x,f7.3)') (rbuf(i),i=1,2)
      DATA(7)=RBUF(1)		! SCALE
      DATA(9)=RBUF(2)		! NORTH ANGLE

      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6250 CONTINUE
C
C     SINUSOIDAL PROJECTION. #12
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      GO TO 10000 

 6260 CONTINUE
C
C     OBLIQUE SINUSOIDAL PROJECTION. #13
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)')
     & (rbuf(i),i=1,2)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE

      II=INDEX(work(:BUFSIZE),'OBLIQUE POLE AT LAT')
      read(work(ii+20:),'(f7.3,6x,f7.3)') (rbuf(i),i=1,2)
      DATA(3)=RBUF(1)		! LATITUDE
      DATA(6)=RBUF(2)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'OBLIQUE LONG')
      read(work(ii+13:),'(f7.3)') rbuf(1)
      DATA(4)=RBUF(1)		! ROTATION

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE
      GO TO 10000 

 6270 CONTINUE
C
C     MOLLWEIDE PROJECTION. #14
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,18x,f7.3)') (rbuf(i),i=1,3)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(6)=RBUF(3)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE
      GO TO 10000 

 6280 CONTINUE
C
C     OBLIQUE SIMPLE CYLINDRICAL PROJECTION. #11
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1)') (rbuf(i),i=1,2)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE

      II=INDEX(work(:BUFSIZE),'OBLIQUE POLE AT LAT')
      read(work(ii+20:),'(f7.3,6x,f7.3)') (rbuf(i),i=1,2)
      DATA(3)=RBUF(1)		! LATITUDE
      DATA(6)=RBUF(2)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'OBLIQUE LONG')
      read(work(ii+13:),'(f7.3)') rbuf(1)
      DATA(4)=RBUF(1)		! ROTATION

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      GO TO 10000 

 7000 CONTINUE
C
C     LAMBERT PROJECTION #5
C
      II=INDEX(work(:BUFSIZE),'*** C.M. AT LONG')
      read(work(ii+17:),'(f7.3)') rbuf(1)
      DATA(6)=RBUF(1)		! LONG

      II=INDEX(work(:BUFSIZE),'STANDARD PARALLELS')
      read(work(ii+19:),'(f7.3,6x,f7.3)') (rbuf(i),i=1,2)
      DATA(4)=RBUF(1)		! PAR1
      DATA(5)=RBUF(2)		! PAR2

      II=INDEX(work(:BUFSIZE),'THE SCALE AT STD PARALL')
      read(work(ii+31:),'(f7.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      II=INDEX(work(:BUFSIZE),'THE SOUTH POLE IS AT LINE ')
      IJ=INDEX(work(:BUFSIZE),'THE NORTH POLE IS AT LINE ')
      IK=MAX0(II,IJ)
      IF(IK.EQ.II)THEN  !SOUTH
         DATA(8)=-1.
         DATA(3)=-90.
      ELSE              !NORTH
         DATA(8)=1.
         DATA(3)=90.
      ENDIF
      ii = ik
      read(work(ii+26:),'(f7.1,11x,f7.1)') (rbuf(i),i=1,2)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE

      GO TO 10000 

 8000 CONTINUE
C
C     MERCATOR PROJECTION #6
C
      DATA(1)=1.
      DATA(2)=1.
      DATA(8)=1.
      ii = index(work(:bufsize),mercat)
      read(work(ii+42:),'(f7.3,9x,f7.3)') (rbuf(i),i=1,2)
      DATA(3)=RBUF(1)		! latitude
      DATA(6)=RBUF(2)		! longitude

      II=INDEX(work(:BUFSIZE),'SCALE AT EQUATOR ')
      read(work(ii+18:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

 8100 CONTINUE
C
C     TRANSVERSE MERCATOR PROJECTION #15
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE AT MERIDIAN ')
      read(work(ii+19:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

 8500 CONTINUE
C
C     OBJECT SPACE MAP3 LABEL  PERSPECTIVE PROJECTION #16
C
      II=INDEX(work(:BUFSIZE),'S/C LAT')
      read(work(ii+8:),'(f8.4,10x,f8.4,10x,f8.4)') (rbuf(i),i=1,3)
      DATA(31)=RBUF(1)		! S/C LAT
      DATA(32)=RBUF(2)		! S/C LONG
      DATA(33)=RBUF(3)		! LINE

      II=INDEX(work(:BUFSIZE),'S/C SAMPLE')
      read(work(ii+11:),'(f8.2,11x,1pe15.9)') (rbuf(i),i=1,2)
      DATA(34)=RBUF(1)		! SAMPLE
      DATA(38)=RBUF(2)		! RANGE

      II=INDEX(work(:BUFSIZE),'FOCAL')
      read(work(ii+6:),'(f9.3,10x,f8.4,13x,f7.3)') (rbuf(i),i=1,3)
      DATA(27)=RBUF(1)		! FOCAL
      DATA(30)=RBUF(2)		! PIXEL/MM
      DATA(35)=RBUF(3)		! NORTH ANGLE

      II=INDEX(work(:BUFSIZE),'OPTICAL AXIS LINE')
      read(work(ii+18:),'(f8.3,8x,f8.3)') (rbuf(i),i=1,2)
      DATA(28)=RBUF(1)		! optical axis line
      DATA(29)=RBUF(2)		! optical axis sample

c
c     compute OM matrix & RS vector
c
c     Note: The use of elements 1 through 18 of the real*4 array
c	    to store 9 real*8 values of the OM matrix and 3 real*8
c	    RS vector values is not portable.
c 
      call momati(dble(data(28)),dble(data(29)),dble(data(33)),
     +            dble(data(34)),dble(data(30)),dble(data(27)),
     +            dble(data(32)),dble(data(31)),dble(data(35)),
     +            dble(data(38)),data(1),data(19))
      return

 9000 CONTINUE
C
C     CYLINDRICAL PROJECTION #9
C
      DATA(3)=0.
      DATA(8)=1.
      II=INDEX(work(:BUFSIZE),'AT S')
      read(work(ii+5:),'(f8.1,3x,f8.1,30x,f9.3)') (rbuf(i),i=1,3)
      DATA(1)=RBUF(1)		! SAMPLE
      DATA(2)=RBUF(2)		! LINE
      DATA(6)=RBUF(3)		! LONG
   
      II=INDEX(work(:BUFSIZE),'SCALE AT EQUATOR')
      read(work(ii+18:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

 9100 CONTINUE
C
C     SIMPLE CYLINDRICAL PROJECTION #10
C
      II=INDEX(work(:BUFSIZE),'AT LINE')
      read(work(ii+8:),'(f9.2,9x,f9.2,7x,f8.3,7x,f8.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATI
      DATA(6)=RBUF(4)		! LONG

      II=INDEX(work(:BUFSIZE),'SCALE AT EQUATOR')
      read(work(ii+18:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

c
c  here if no map labels:  check for object space tasks:
c
 9500 do i=1,ntasks
	if (index(tasks(i),'GEOM').gt.0 .or. tasks(i).eq.'FARENC') then
	  idata(39)=8   ! object space
          return
	endif
      enddo
      idata(39)=7	! image space
      return

10000 CONTINUE
      DATA(6)=AMOD(DATA(6),360.)
      RETURN
      END
