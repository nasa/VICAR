$!****************************************************************************
$!
$! Build proc for MIPL module ibisgcp
$! VPACK Version 1.9, Monday, February 03, 2003, 14:58:41
$!
$! Execute by entering:		$ @ibisgcp
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module ibisgcp ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ibisgcp.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ibisgcp.imake") .nes. ""
$   then
$      vimake ibisgcp
$      purge ibisgcp.bld
$   else
$      if F$SEARCH("ibisgcp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisgcp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisgcp.bld "STD"
$   else
$      @ibisgcp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisgcp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisgcp.com -mixed -
	-s ibisgcp.f -
	-p ibisgcp.pdf -
	-i ibisgcp.imake -
	-t tstibisgcp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisgcp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR program IBISGCP: Output Ground Control Points to an IBIS file.

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT INTEGER (A-Z)

        parameter (maxpix=100)
	integer*4 sn(maxpix),scet(6,maxpix)
        real*4 rsvec(maxpix,3),omang(maxpix,3),radpol(maxpix)
        real*4 radeq(maxpix),focal(maxpix),optaxl(maxpix)
        real*4 optaxs(maxpix),scale(maxpix),r_cam(maxpix)

        real*4 buf(200),conv(3600)	!spice and is-to-os buffers
        integer*4 idata(40)		!convev buffer
        real*4 data(40)
        real*8 data8(20)        
        equivalence (data,idata,data8)

c       ...Output ground-control points: frame index, line, samp, lat, lon
	real*4 f(1000),l(1000),s(1000),lt(1000),ln(1000)

	REAL*4 RL,RS,RLN,RLT,RLNX,RLTX,FID(100)
        real*8 alpha,delta,kappa

        character*12 planet
        character*5 project
        character*80 infile

        logical input_file/.false./
        integer iu
	integer ibis_in, ibis_out
        character*6 format(5)/5*'REAL'/

	logical xvptst
c------------------------------------------------------------------

        CALL XVPARM('PROJECT',project,ICNT,IDEF,' ')
        CALL ucase (project, project)
c
c       If present, read IBIS input
c
        CALL XVPARM('INP',infile,icnt,idef,' ')
        if (idef.eq.0) then
          input_file=.true.
          call xvunit(iu,'INP',1,istat,' ')
          if ( istat.ne.1) call mabend('Err on IBIS unit.',' ')
          call ibis_file_open(iu,ibis_in,'read',0,0,' ',' ',istat)
          if ( istat .ne. 1 ) call ibis_signal_u(iu,istat)

          call ibis_file_get (ibis_in,'NR', npict,1,1)
          ifds=npict

          call xvparm('id',fid,ifid,idef,' ')
          do i=1,ifid
            if (nint(fid(i)).gt.ifds)
     +         call mabend('ID # larger than # IBIS records',' ')
          enddo
          ifds=ifid

          call ibis_column_read(ibis_in,r_cam,4,1,npict,istat)
          if (istat.ne.1) call ibis_signal(ibis_in,' ',istat)
          DO I=1,npict
            SN(i) = r_cam(i)
          END DO

          call ibis_column_read(ibis_in,rsvec(1,1),5,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,rsvec(1,2),6,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,rsvec(1,3),7,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,omang(1,1),21,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,omang(1,2),22,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,omang(1,3),23,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,radpol,26,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,radeq,27,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,focal,28,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,optaxl,29,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,optaxs,30,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,scale,31,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_file_close(ibis_in,' ',istat)
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

        else

          call init_spice		!Initialize spice server
          CALL XVPARM('TARGET',planet,ICNT,IDEF,' ')
          CALL ucase(planet, planet)

          call xvparm('SCET',scet,ifds,idef,' ')
          ifds=ifds/6
          if (ifds.eq.0) call mabend('SCET parameter missing',' ')

          CALL XVPARM('CAMERA',SN,isn,IDEF,' ')
          if (isn.ne.ifds)
     +		 call mabend('CAMERA required for each frame.',' ')

          CALL XVPARM('ID',FID,ifid,IDEF,' ')
          if (ifid.ne.ifds)
     +       call mabend('frame sequence # req for each frame.',' ')

        endif

        idata(39)=7
        if (project.eq.'VGR-1' .or. project.eq.'VGR-2'
     +		.or. project.eq.'VIKOR') idata(39)=8
        if (xvptst('OBJECT')) idata(39)=8

	TOT = 0
        call mvcl( project, conv, 5)
	call xvparm('linc',linc,icnt,idef,' ')
	call xvparm('sinc',sinc,icnt,idef,' ')


C-------Main loop
	DO 200 JJ=1,IFDS
	    call xvmessage(' ***********************',' ')
            call mve(4,1,sn(i),conv(3),1,1)

            if(input_file)then
               i=nint(fid(jj))
               call prnt(4,1,i,'IBIS SPICE entry # .')
               data(25)=radpol(i)
               data(26)=radeq(i)
               data(27)=focal(i)
               data(28)=optaxl(i)
               data(29)=optaxs(i)
               data(30)=scale(i)
               data8(10)=rsvec(i,1)
               data8(11)=rsvec(i,2)
               data8(12)=rsvec(i,3)
               data(38)=dsqrt(data8(10)**2+data8(11)**2+data8(12)**2)
               alpha=omang(i,1)
               delta=omang(i,2)
               kappa=omang(i,3)
               call fromeuler(alpha,delta,kappa,data8)
          
            else 
               call prnt(4,6,scet(1,jj),'SCET:y,d,h,m,s,ms.')
               CALL MSEDR(IND,SN,BUF,buf,DATA,planet,
     +             scet(1,jj),project)

            endif

            call prnt(8,9,data(1),'OM:.')
            call prnt(8,3,data(19),'RS:.')

            RL = 0          ! Initialize
            RS = SINC

            DO 10 I=1,1000
                RL = RL + LINC
                IF (RL.GT.1000-LINC) THEN
      	            RL = LINC
	            RS = RS + SINC
	            IF (RS.GT.1000-SINC) GO TO 15  !GO TO NEXT FRAME
                END IF

                CALL CONVEV(IND,DATA,DATA,RL,RS,RLT,RLN,2,CONV)
                IF (IND .EQ. 0) THEN

C-------IS RL,RS WITHIN 125 PIXELS OF LIMB? YES=SKIP IT
                  CALL CONVEV(IND,DATA,DATA,RL-125.,RS,RLTX,RLNX,2,CONV)
                  IF (IND .NE. 0) GO TO 10
                  CALL CONVEV(IND,DATA,DATA,RL+125.,RS,RLTX,RLNX,2,CONV)
      	          IF (IND .NE. 0) GO TO 10
   	          CALL CONVEV(IND,DATA,DATA,RL,RS-125.,RLTX,RLNX,2,CONV)
	          IF (IND .NE. 0) GO TO 10
	          CALL CONVEV(IND,DATA,DATA,RL,RS+125.,RLTX,RLNX,2,CONV)
	          IF (IND .NE. 0) GO TO 10

	          TOT = TOT + 1
                  L(TOT) = RL
	          S(TOT) = RS
	          LT(TOT) = RLT
	          LN(TOT) = RLN
	          F(TOT) = FID(JJ)
	          IF (TOT .EQ. 1000) GO TO 15	!END
	        END IF
10 	    CONTINUE				!POINT LOOP


15	    TOTTMP = TOT - LASTTOT
   	    LASTTOT = TOT
	    CALL PRNT(4,1,TOTTMP,' POINTS GATHERED = .')
	    IF (TOT .EQ. 1000) GO TO 201	!END

200	CONTINUE				!FRAME LOOP


c
c       finished processing - output data
c

201	continue

	call xvunit(ou,'out',1,istat,' ')
        call ibis_file_open(ou,ibis_out,'write',5,tot,' ',' ',istat)
        if ( istat .ne. 1 ) call ibis_signal_u(ou,istat)

        CALL xvmessage(' ***********************',' ')
	CALL PRNT(4,1,TOT,'0TOTAL POINTS GATHERED = .')

	call ibis_column_write(ibis_out,F,1,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,L,2,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,S,3,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,LT,4,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,LN,5,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
        call ibis_file_close(ibis_out,' ',istat)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	return
        end

c **************************************************************
	subroutine fromeuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)
	real*8	c(3,3)      ! output - derived rotation matrix 

c  this routine performs the functional inverse of routine toeuler.  the
c  three euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

	real*8  cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8  cos_kappa, sin_kappa, dtor

        dtor = 3.141592653589793D0/180.D0
	sin_alpha = sin(alpha*dtor)
	cos_alpha = cos(alpha*dtor)
	sin_delta = sin(delta*dtor)
	cos_delta = cos(delta*dtor)
	sin_kappa = sin(kappa*dtor)
	cos_kappa = cos(kappa*dtor)
	c(1,1) = -sin_alpha * cos_kappa - cos_alpha * sin_delta *
     &							 sin_kappa
	c(1,2) =  cos_alpha * cos_kappa - sin_alpha * sin_delta *
     &							 sin_kappa
	c(1,3) =  cos_delta * sin_kappa
	c(2,1) =  sin_alpha * sin_kappa - cos_alpha * sin_delta *
     &							 cos_kappa
	c(2,2) = -cos_alpha * sin_kappa - sin_alpha * sin_delta *
     &							 cos_kappa
	c(2,3) =  cos_delta * cos_kappa
	c(3,1) =  cos_alpha * cos_delta
	c(3,2) =  sin_alpha * cos_delta
	c(3,3) =  sin_delta
	return
	end

c ***********************************************************
      SUBROUTINE MSEDR(IND,ISN,BUF,buf8,DATA,PLANET,
     +                 scet,project)
      integer*4 ind,isn

      REAL*4 DATA(40),BUF(200)
      REAL*8 PI,R2,DTOR,buf8(100)

      character*12 planet
      character*5 project
      integer*4 scet(6),fds


C
      PI = 3.141592653589793D0
      dtor = pi/180.D0

c get camera constants
      call getcamcon(project,isn,data(27),data(28),data(29),
     +               data(30),ind)
      if(ind.ne.0)then
         call xvmessage('GETCAMCON: bad indicator',' ')
         call abend
      endif

      call getspice3(project,planet,isn,fds,scet,.TRUE.,buf,ind)
      if(ind.ne.1)then
         call prnt(4,1,ind,'GETSPICE3: bad ind=.')
         call abend
      endif
      CALL cmsource(buf, ind)

C          PICTURE BODY
      DATA(36) = BUF(9)

C          PLANET RADII
      DATA(25) = BUF8(15)
	rp2=data(25)*data(25)
      DATA(26) = (BUF8(13)+buf8(14))/2.0
	re2=data(26)*data(26)
	r2=1.0D0*rp2/re2

C          RANGE
      DATA(38) = BUF8(27)

C          SUBSPACECRAFT (LAT,LON)
      DATA(31) = BUF8(30)
      r=buf8(31)
      DATA(32) = AMOD(r+360.,360.)

C          NORTH ANGLE
      r=buf8(68)
      DATA(35) = AMOD(r+90.,360.)

C-----OM MATRIX
      CALL MVE(8,9,BUF8(59),DATA,1,1)

C-----RS VECTOR
      CALL MVE(8,3,BUF8(22),DATA(19),1,1)      !UPDATED ONE

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ibisgcp.pdf
process help=*

PARM INP        STRING          COUNT=(0:1)   DEFAULT=""
PARM OUT 	STRING 		COUNT=1
PARM PROJECT    TYPE=(STRING,5) COUNT=1 +
   VALID=("VGR-1","VGR-2","MAR10","MAR-9","VIKOR","GLL  ","CASSI")
PARM ID 	REAL 		COUNT=(1:100)
PARM SCET 	INTEGER 	COUNT=(0:600) DEFAULT=--
PARM CAMERA 	INTEGER 	COUNT=(0:100) DEFAULT=--
PARM LINC 	INTEGER 	COUNT=0:1	DEFAULT=300
PARM SINC 	INTEGER 	COUNT=0:1	DEFAULT=300
PARM OBJECT     TYPE=KEYWORD    COUNT=0:1     VALID=OBJECT	DEFAULT=--
PARM TARGET     TYPE=(STRING,12) COUNT=0:1                      DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME 	TYPE=(STRING,4)	COUNT=(0:1) +
   VALID=("DAVI","NAV ","NAV2","FARE","NEAR","AMOS","NAIF")     DEFAULT="DAVI"
PARM CKID       TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1                DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1                        DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
END-PROC

.TITLE
  VICAR PROGRAM IBISGCP - output Ground-Control Points for mosaicking.

.HELP
PURPOSE:

IBISGCP outputs Ground-Control Points to an IBIS file for use in registering
the elements of a mosaic.  IBISGCP is a multimission program and can be used
process images from Cassini, Galileo, Voyager, and Viking Orbiter.

EXECUTION:
	   IBISGCP  inp=nav  out=gcp  project=GLL  id=(5,8)
or
	   IBISGCP  out=gcp  project=GLL  id=(5,8)
              target=EARTH  camera=(1,1) +
	      scet=(1990,345,14,6,22,0, +
		    1990,345,22,16,780,0)
where
	nav is an input IBIS SEDR file containing navigation data for each
	    image in a mosaic (see program IBISNAV),
	gcp is an output file containing the ground-control points from
	    one or more reference images in the mosaic.
	project is CASSI, GLL, VGR-1, VGR-2, or VIKOR,
	id is a list of frame sequence numbers corresponding to the position
	    of each reference image in the IBIS SEDR file.

If the IBIS SEDR file is not input, the navigation data will be retrieved via
the MIPS SPICE server.  This requires that the target be specified, and that
each reference image be identified by camera serial number and spacecraft-event
time (see TARGET, CAMERA and SCET parameters).

.page
OPERATION:

IBISGCP outputs ground-control points for use in registering the elements of a
mosaic.  For Voyager refer to procedure MANUAL2.COM.  For Galileo refer to
procedure MANUAL3.COM.  Note that both of these procedures are currently
unported (and therefore available only via the Harvest CM system).

Ground-control points are points on a target body whose latitude-longitude
coordinates are known.  IBISGCP creates psuedo ground-control points by
assuming that the navigation data for each of the reference images is perfect
(i.e. contains no errors).  The reference images are normally images which
have been previously navigated to improve their camera pointing knowledge.
Images with visible limbs are usually used as reference images since these may
be navigated by fitting the limb (see programs FARENC and NAV).

The reference images are specified in one of two ways:

1: by inputing an IBIS SEDR file and specifying the frame sequence number
   of each reference image within this file (see ID parameter).
2: by specifying their camera IDs and spacecraft-event times (see CAMERA and
   SCET paramters).

   navigation data via the SPICE server.

Method #1 is normally used.  In this mode, IBISNAV is first run to create the
IBIS SEDR file.  This file will contain the navigation data for all frames
comprising the mosaic.

If method #2 is used, IBISGCP retrieves the navigation data for each reference
image via the MIPS SPICE server.  This requires that the target-body name
be specified (see TARGET parameter).  Moreover, the spacecraft-event time and
camera serial number for each reference image must be specified (see SCET and
CAMERA parameters).  For unknown reasons, the ID parameter is also required,
even though the corresponding IBIS SEDR file may not even exist.

For either method, the spacecraft ID must be specified (see MISSION parameter).

Ground-control points are selected in each reference image so that they form
a grid pattern over the image (see LINC and SINC parameters).  The line-sample
coordinates of each grid point is converted to latitude-longitude coordinates.
If the point is not on the target body, it is skipped.

All points from the same reference image are equally precise.  The actual
precision is solely determined by the accuracy of the navigation source.

The ground-control points are output to an IBIS file suitable for input to
program OMC or OMCOR2.  The IBIS file has 5 "columns" and N "rows", where N is
the number of ground-control points.  The columns are:

	1	frame sequence number (from ID parameter)
	2	line coordinate
	3	sample coordinate
	4	latitude coordinate
	5	longitude coordinate

.page
GEOMETRIC DISTORTIONS:

For Voyager and Viking Orbiter, the reference images are assumed to be
geometrically corrected.  Thus, the line-sample coordinates for the ground
control points are assumed to be in object space.

For all later missions (Cassini, Galileo), the reference images are assumed
to be geometricall uncorrected.  Thus, the line-sample coordinates for the
ground control points are assumed to be in image space.

These assumptions effect the resulting latitude-longitude computations.  The
keyword 'OBJECT may be used to specify an object-space output.  Note that it
is not possible to image-space coordinates for Voyager or Viking Orbiter since
the reseau locations are unknown.

.page
PROGRAM HISTORY:

Written by:         C. AVIS, 2/89
Current Cognizant Programmer:  Gary Yagi, Jan 31, 2003
Revisions:

When      Who  What
--------- ---  ---------------------------------------------------------------
Jan 31 03 GMY  Fixed Linux compile errors.  Updated to support Cassini.
	       Fixed Voyager to reflect use of SPICE instead of SEDR.
	       Major revision of documentation.
Dec 09 96 SMC  * Modified for summation mode support                 (FR89818)
               * Parameter PLANET is changed to TARGET
               * Modified to call GETSPICE3 instead of GETSPICE          (DFR)
               * Discovered that MSEDR did not correctly assign the OM and
                 RS to DATA buffer, fixed.                               (DFR)
               * Discovered that subroutine FROMEULER is incorrect, the one
                 from MOSPLOT is copied and tested.                      (DFR)
               * Discovered that the CONVEV DATA buffer is not complete if
                 the program is given an IBISNAV file, fixed.            (DFR)
	
.LEVEL1

.VARI OUT
String
Name of IBIS ground control file

.VARI PROJECT
name of project

.VARI OBJECT
Causes output ground-control
points to be in object space.

.VARI CAMERA 
Integer 
Camera serial numbers for
each reference image. 

.VARI SCET
The spacecraft event time 
for each reference image.

.VARI ID 
Required Real
IBISNAV frame sequence 
number for each reference image.

.VARI CKNAME
Optional sring
SPICE C-kernal name
String 4 characters.

.VARI TARGET
Optional sring
String
Target name

.VARI LINC 
Optional Integer
Line spacing between ground 
control points.

.VARI SINC 
Optional Integer
Sample spacing between ground 
control points.

.LEVEL2

.VARI OUT
Output IBIS file containing the ground-control points:

record	constant				format

1       frame index number of IBIS SEDR file	r*4
2       line coordinate of point 		r*4
3       sample coordinate of point 		r*4
4	planetographic latitude of point	r*4
5	west longitude of point			r*4

.VARI PROJECT
The project name. Valid are:
VGR-1 VGR-2 MAR10 MAR9 VIKOR GLL

.VARI OBJECT
Causes ground-control points to be output in object space line-sample
coordinates.  The default is object-space for Voyager and all earlier missions,
and image-space for all later missions (Galileo, Cassini).

.VARI CAMERA 
Integer 
Camera serial number for each reference frame.

	CAMERA=(I1,I2,I3,...,IN)

The camera serial number is used to retrieve the focal length, line and sample
of the optical-axis intercept point, and the picture scale from built-in
tables.  The current values in these tables are:

		      CAMERA	 FOCAL	  LAXIS  SAXIS  PSCALE (pixels/mm)
        CASSI NAC       1       2000.00    512    512   83.333333
	CASSI WAC       2        200.736    "      "      "
        CASSI NAC 2x2  21       2000.00    256    256   41.666665
	CASSI WAC 2x2  22        200.736    "      "      "
        CASSI NAC 4x4  41       2000.00    128    128   20.833333
	CASSI WAC 4x4  42        200.736    "      "      "

        GLL             1       1501.039   400    400   65.6167979 
        GLL 2x2 sum     2	1501.039   200    200   32.8083990

	VGR-2 WA        4	 200.770   500    500   84.821428
	VGR-2 NA        5	1503.49     "      "      "
	VGR-1 WA        6	 200.465    "      "      "
	VGR-1 NA        7	1500.19     "      "      "

        VIKOR 1A        7	474.610    575    625   85.0
        VIKOR 1B        4	474.398     "      "      "
        VIKOR 2A        8	474.101     "      "      "
        VIKOR 2B        6	474.448     "      "      "

        MAR10 A         1      1495.66     400    475   74.78
        MAR10 B         1      1503.69     400    475   74.78

        MAR-9           1	 52.267    400    475   75.0     
        MAR-9		2	500.636     "      "      "

Note: These tables are obtained via a call to VICAR subroutine GETCAMCON.  For
active missions, these values may be updated as they are more accurately
determined.

.VARI SCET
The SpaceCraft-Event Time for each reference image.

	SCET=(2001,128,13,22,31,801, +
	      2001,128,13,23,16,10, +
	      2001,128,13,23,56,12, +
	      2001,128,13,25,33,510)

Input as groups of 6 integers in the order:
scet year
scet day
scet hour
scet minute
scet second
scet milisecond

.VARI ID 
Required Real
Sequence number from the IBISNAV file for each reference frame.

.VARI CKNAME
String
SPICE C-kernal name
Valid: "DAVI","NAV ","NAV2","FARE","NEAR","AMOS","NAIF"

.VARI TARGET
String 12
Target name
	e.g. TARGET=EUROPA

.VARI LINC 
Optional Integer
Line spacing between ground control points (default=300).  The ground-control
points are spaced in a grid pattern LINC lines and SINC samples apart.

.VARI SINC 
Optional Integer
Sample spacing between ground control points (default=300).  The ground-control
points are spaced in a grid pattern LINC lines and SINC samples apart.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create ibisgcp.imake
#define PROGRAM ibisgcp
#define R2LIB

#define MODULE_LIST ibisgcp.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_SPICE
#define LIB_TAE
#define LIB_MATH77
#define LIB_RTL
#define LIB_P2SUB
#define LIB_NETWORK
$ Return
$!#############################################################################
$Test_File:
$ create tstibisgcp.pdf
procedure
  PARM   TEST_PROJECT TYPE=KEYWORD VALID=(GLL,VGR,CAS,ALL) DEFAULT=ALL
  RefGbl $SysChar
  RefGbl $Echo
  LOCAL  SHELL TYPE=STRING
body
  let _onfail="continue"
  let $echo="no"
  if ($SysChar(1)="VAX_VMS")
    let SHELL = "DCL"
  else
    let SHELL = "USH"
  end-if

  if ("&TEST_PROJECT"="VGR" OR "&TEST_PROJECT"="ALL")
    write "===FIND A SET OF FRAMES WITH ORIGINAL SEDR"
    write "=== Voyager test without inputs:"
    let $echo="yes"
    IBISGCP out=gcp.ibis project=VGR-1 TARGET=jupiter ckname="NAV " +
	camera=7 id=1 scet=(1979,63,19,23,0,0) linc=150 sinc=150
    IBIS-LIST |stdout=gcp1.txt| inp=gcp.ibis
    let $echo="no"

    write "===VGR test with inputs (should be same as last IBIS file output)"
    let $echo="yes"
    IBISNAV out=nav.ibis project=VGR-1 target=JUPITER ckname="NAV " +
	camera=7 scet=(1979,63,19,23,0,0)
    IBISGCP inp=nav.ibis out=gcp.ibis project=VGR-1 id=1 linc=150 sinc=150
    IBIS-LIST |stdout=gcp2.txt| inp=gcp.ibis

    let $echo="no"
    if ($SysChar(1)="VAX_VMS")
      write "===Make sure DIFF reports 0 difference."
    else
      write "===An Abnormal Shell termination means something was different"
    end-if
    write "==============================================================="
    let $echo="yes"
    &SHELL diff gcp1.txt gcp2.txt
    let $echo="no"
    write "==============================================================="
  end-if
  
!-----------------------------Cassini Test--------------------------------
  if ("&TEST_PROJECT"="GLL" OR "&TEST_PROJECT"="ALL")
    write "===Galileo test, Full Frame:"
    let $echo="yes"

    IBISGCP out=gcp1.ibis project=GLL target=EARTH ckname=FARE +
	camera=1 id=1 scet=(1990,345,14,6,22,0) linc=250 sinc=250
    IBIS-LIST |stdout=gcp1.txt| inp=gcp1.ibis

    IBISNAV out=nav.ibis project=GLL target=EARTH ckname=FARE +
	camera=1 SCET=(1990,345,14,6,22,0)
    IBISGCP inp=nav.ibis out=gcp2.ibis project=GLL id=1 linc=250 sinc=250
    IBIS-LIST |stdout=gcp2.txt| inp=gcp2.ibis

    let $echo="no"
    if ($SysChar(1)="VAX_VMS")
      write "===Make sure DIFF reports 0 difference."
    else
      write "===An Abnormal Shell termination means something was different"
    end-if
    write "==============================================================="
    let $echo="yes"
    &SHELL diff gcp1.txt gcp2.txt
    let $echo="no"
    write "==============================================================="

    write "===Galileo test, Summation Mode"
    let $echo="yes"
    IBISGCP out=gcp1.ibis project=gll target=ganymede ckname="NAV " +
	camera=(2,2,2,2) id=(1,2,3,4) +
        scet=(1996,178,8,47,5,459,  +
              1996,178,8,46,4,893,  +
              1996,178,8,46,20,126, +
              1996,178,8,46,35,293) linc=100 sinc=100
    IBIS-LIST |stdout=gcp1.txt| inp=gcp1.ibis

    IBISNAV out=nav.ibis project=gll target=ganymede +
	ckname=("NAV ","NAV ","NAV ","NAV ") +
	camera=(2,2,2,2) +
        scet=(1996,178,8,47,5,459,  +
              1996,178,8,46,4,893,  +
              1996,178,8,46,20,126, +
              1996,178,8,46,35,293)
    IBISGCP inp=nav.ibis out=gcp2.ibis project=gll +
	id=(1,2,3,4) linc=100 sinc=100
    IBIS-LIST |stdout=gcp2.txt| inp=gcp2.ibis

    let $echo="no"
    if ($SysChar(1)="VAX_VMS")
      write "===Make sure DIFF reports 0 difference."
    else
      write "===An Abnormal Shell termination means something was different"
    end-if
    write "==============================================================="
    let $echo="yes"
    &SHELL diff gcp1.txt gcp2.txt
    let $echo="no"
    write "==============================================================="
  end-if

!-----------------------------Cassini Test--------------------------------
  if ("&TEST_PROJECT"="CAS" OR "&TEST_PROJECT"="ALL")
    write "===Cassini test, Full Frame:"
    let $echo="yes"
    IBISGCP out=gcp1.ibis linc=100 sinc=100 project=CASSI target=JUPITER +
         camera=1 ID=1 SCET=(2000,342,16,10,56,162)
    IBIS-LIST inp=gcp1.ibis
  end-if

end-proc
$ Return
$!#############################################################################
