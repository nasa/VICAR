C	
C	
C	PURPOSE: Returns planetary constants given either the planet
C	name or the planet ID# as in the sedr/spice. Note:
C	If the planet name is given the id number will be returned
C	too. If the planet name is blanks then the planet name will
C	be returned too.
C
C	call getplacon(planet,id,data,ind)
C
C	Author: 		Jean Lorre
C	Cognizant Programmer:	Jean Lorre
C	Date written:		October 1989
C
C	Revisions:	
C	
C	April 24, 1992	JFM	Planet name is no longer CaSe sensitive;
C				lowercase and uppercase names acceptable;
C				(FR 73773)
C
C

      subroutine getplacon(planet,id,data,ind)

C planet=planet name                     input  character*12
C id    =planet SEDR/SPICE id number     input  integer*4
C data  =data buffer. 20 real*4 values.  returned  real
C ind   =status. 0=OK   1=unknown planet   2=unknown id
C data buffer:
C word #    contents
C  1      equatorial radius (long) km.
C  2      equatorial radius (short) km.
C  3      polar radius   km.
C  4      longitude of long axis    deg
C  5      rotation period          days
C  6      solar range              km
C  7-20   unused

      character*12 planet
      real*4 data(20)
				      ! CCASE routine setup
      mode=1			      ! set case selection 
      max=12			      ! maximum number of characters
      do 5 I=1,20
         data(i)=0.0
 5    continue
      ind=0
      if(planet.eq.'            ')then
         call pbname(id,planet,*10)   ! return planet
      else
         call pbid(planet,id,*11)     ! return id #
      endif
12    call ccase(planet,mode,-1)      ! ensure case is UPPER
      call pbdata(planet,data,*10)    ! return data
      return
10    ind=1
      return
11    ind=2
      goto 12
      end













