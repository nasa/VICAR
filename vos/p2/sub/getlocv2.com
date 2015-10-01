$!****************************************************************************
$!
$! Build proc for MIPL module getlocv2
$! VPACK Version 1.8, Thursday, June 13, 1996, 14:16:13
$!
$! Execute by entering:		$ @getlocv2
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module getlocv2 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to getlocv2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("getlocv2.imake") .nes. ""
$   then
$      vimake getlocv2
$      purge getlocv2.bld
$   else
$      if F$SEARCH("getlocv2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getlocv2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getlocv2.bld "STD"
$   else
$      @getlocv2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getlocv2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getlocv2.com -
	-s getlocv2.f -
	-i getlocv2.imake -
	-t tgetlocv2.f tgetlocv2.imake tgetlocv2.pdf tstgetlocv2.pdf -
	-o getlocv2.hlp putlocv2.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getlocv2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      subroutine getlocv2(ibis,nrows,buf,fdata,lret)

c
c     Arguments: ibis = ibis file # from ibis_file_open call (input)
c                nrows = # of rows in the reseau file (input)
c                buf = nasty remnant from original code 
c                      this array has 5 variables that are in common
c		       in the main and are as follows:
c                      frame,camera,filter,year and day - sorry about that
c                      (input)
c                fdata = array containing reseau locations (returned)
c                lret = error flag (returned)
c			0 = good
c			1 = error occurred                
c
c


C ROUTINE TO GET RESEAU RECORD FROM VOYAGER RESLOC FILE

      IMPLICIT INTEGER(A-Z)

      integer buf(5)
      real*4 fdata(404)
      

c for porting     BAM
	integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer record1
        integer record2
        integer lrw
        integer status
        integer day

        integer jdata(5)    ! IBIS data arrays
   
	include 'fortport'

c
c       Note: the below array information comes in via a common block
c             within the calling programs


C    DSRN       BUF(1) = FRAME
c               BUF(2) = CAMERA
C               BUF(3) = FILTER
C               BUF(4) = YEAR
C               BUF(5) = DAY
C               BUF(6) = RES(1,1)    reseaux
C                 .  
C                 .  
C
C               BUF(409)=RES(2,202)
C
                           ! initialize useful stuff
      FRAME  = BUF(1)      ! obvious  - frame
      CAMERA = BUF(2)      ! and camera


      call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_read(record1,jdata,1,status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      found = 0
      l = 1
      do while ( found .eq. 0 .and. l .le. nrows )
          call ibis_record_read(record1,jdata,l,status)
          if ( FRAME.eq.jdata(1) ) then
              IF ( CAMERA.EQ.0.OR.CAMERA.EQ.jdata(2)) go to 6
          end if
          l = l + 1
      end do

      
      LRET = 1    ! error - not found
      go to 10

    6 continue     ! we have data
      call ibis_record_read(record2,fdata,l,status) ! read reseau

      call prnt (4, 1, jdata(1), ' Frame  = ')
      call prnt (4, 1, jdata(2), ' Camera = ')
      call prnt (4, 1, jdata(3), ' Filter = ')
      call prnt (4, 1, jdata(4), ' Year   = ')
      call prnt (4, 1, jdata(5), ' Day    = ')

      do i = 1, 5     ! transfer information for output
          buf(i) = jdata(i)
      end do

      LRET = 0    ! found

 10   continue
      call ibis_record_close(record1,status)
      call ibis_record_close(record2,status)

      RETURN
      end
 
CCCCCCCCCCCCCCCCC



      subroutine putlocv2(in2,ibis,nrows,buf,fdata)
c
c     Arguments: in2 = reseau input file for update
c                ibis = ibis file # from ibis_file_open call (input)
c                nrows = # of rows in the reseau file (input)
c                buf = nasty remnant from original code 
c                      this array has 5 variables that are in common
c		       in the main and are as follows:
c                      frame,camera,filter,year and day - sorry about that
c                      (input)
c                fdata = array containing reseau locations (returned)
c
c
C  PUT RESLOCS INTO RESLOC FILE


      IMPLICIT INTEGER(A-Z)
      INTEGER buf(5)
      real*4 fdata(404)      

c for porting     BAM
	integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer record1
        integer record2
        integer lrw
        integer status
        integer day
        integer jdata(5)

	include 'fortport'



      FRAME = BUF(1)
      CAMERA = BUF(2)

      call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

c
c     check if we are writing to the master reseau file
c     if we are not, ie. nrows = 1, then just write
c

      if ( nrows .eq. 1 ) go to 10  

      call ibis_record_read(record1,jdata,1,status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

C          SEE IF FRAME IS ALREADY IN RESLOC FILE

      found = 0
      l = 1
      do while ( found .eq. 0 .and. l .le. nrows )
          call ibis_record_read(record1,jdata,l,status)
          if ( (frame.eq.jdata(1))) then
              found = 1
              go to 10
          end if
          l = l + 1
      end do

c
c     writing to reseau file
c

 10   continue
      call ibis_file_close(ibis,' ',status)

      jdata(1) = frame          ! fill the first 5 columns
      jdata(2) = camera
      jdata(3) = buf(3)
      jdata(4) = buf(4)
      jdata(5) = buf(5)

      if ( nrows .ne. 1 ) then  ! write to correct location
          if ( found .eq. 0 ) then
              next = nrows + 1      ! master file
          else
              next = nrows
          end if

          call xvunit( in2, 'INP', 2, status, 0)  
          if ( status .ne. 1 ) then   
              call xvmessage ( ' Error getting unit for reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if
          call ibis_file_open(in2,ibis,'update',409,next,
     -                        format,0,status)
          if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
      else
          next = 1              ! single file
          call ibis_file_open(in2,ibis,'write',409,next,
     -                        format,0,status)
          if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
          call xvunit( in2, 'OUT', 1, status, 0)  
          if ( status .ne. 1 ) then   
              call xvmessage ( ' Error getting unit for reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if
      end if


c
c     set the number of rows properly - this is necessary
c     when trying to add a row of data in update mode for
c     the new ibis routines.....  
c
      call ibis_file_set(ibis,'nr',next,status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      if ( found .eq. 0 ) then  ! new record
          call ibis_record_write(record2,fdata,next,status)
          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
          call ibis_record_write(record1,jdata,next,status)
          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
      else                      ! updated record
          call ibis_record_write(record2,fdata,1,status)
          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
          call ibis_record_write(record1,jdata,1,status)
          if ( status .ne. 1 ) call ibis_signal(l,status,1)
      end if
      nrows = next     ! reset to reflect type of reseau file

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getlocv2.imake
/***********************************************************************

                     IMAKE FILE FOR VICAR SUBROUTINE GETLOCV2

   To Create the build file give the command:

		$ vimake getlocv2			(VMS)
   or
		% vimake getlocv2			(Unix)


************************************************************************/


#define SUBROUTINE getlocv2

#define MODULE_LIST getlocv2.f

#define P2_SUBLIB

#define USES_FORTRAN

#define FTNINC_LIST fortport
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tgetlocv2.f
c
c
c       TGETLOCV2
c
c
C-----THIS IS A ROUTINE TO TEST SUBROUTINES GETLOCV2/PUTLOCV2
C-----PUTLOCV2.



	include 'VICMAIN_FOR'
	subroutine main44

c
c	rewritten by BAM  4/96
c
	IMPLICIT INTEGER (A-Z)
	COMMON/C/ UNIT,LAST,NL
        real*4 fdata(404)
   
c for porting     BAM
        integer in2
        integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer status

        integer jdata(5)    ! IBIS data arrays

        integer iframe(2000)

        include 'fortport'


c
c       start main code
c

	CALL XVUNIT( in2, 'INP', 2, STATUS, 0) ! get a unit
        call ibis_file_open(in2,ibis,'read',409,99999, 
     -                        format,0,status)
        if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)


        icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
        if ( nrows .lt. 0 ) call ibis_signal(ibis,count,1)
	CALL PRNT(4,1,nrows,' Number of rows in the file = ')


                                  ! get all the frames
        call ibis_column_read(ibis,iframe,1,1,nrows,istat)
        if ( istat .ne. 1 ) call ibis_signal(ibis,' ',istat)


	CALL xvmessage(' FRAMES IN FILE BEFORE UPDATE',0)


        do i = 1, nrows    ! report what's in the input reseau file
          if ( iframe(i) .ne. 0 ) then
              frm = iframe(i)  
              jdata(1) = frm
              jdata(2) = 0
              call getlocV2(ibis,nrows,jdata,fdata,iret)
      	      CALL PRNT(0,4,iframe(I),' IN HEX.')
    	      CALL PRNT(7,8,fdata,' 1ST 4 (L,S).')
          end if
        end do
C
	FRM = 2
	CAM = 4
        jdata(1) = frm
        jdata(2) = cam
	CALL PUTLOCV2(in2,ibis,nrows,jdata,fdata)   !FILL RESEAU RECORD
        icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
        if ( nrows .lt. 0 ) call ibis_signal(ibis,count,1)
	CALL PRNT(4,1,nrows,' Number of rows in the file = ')
	CALL xvmessage(' FRAMES IN FILE AFTER UPDATE',0)
C
        do i = 1, nrows    ! report what's in the input reseau file
          frm = iframe(i)  
          jdata(1) = frm
          jdata(2) = 0
          if ( frm .ne. 0 ) then
              call getlocV2(ibis,nrows,jdata,fdata,iret)
    	      CALL PRNT(0,4,frm,' IN HEX.')
 	      CALL PRNT(7,8,fdata,' 1ST 4 (L,S).')
          end if
        end do

        call ibis_file_close( ibis,' ', status )

	RETURN
	END
$!-----------------------------------------------------------------------------
$ create tgetlocv2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tgetlocv2

   To Create the build file give the command:

		$ vimake tgetlocv2			(VMS)
   or
		% vimake tgetlocv2			(Unix)


************************************************************************/


#define PROGRAM	tgetlocv2 
#define R2LIB

#define MODULE_LIST tgetlocv2.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$!-----------------------------------------------------------------------------
$ create tgetlocv2.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
end-proc
$!-----------------------------------------------------------------------------
$ create tstgetlocv2.pdf
procedure
!   tsttgetlocv2 
refgbl $echo
refgbl $autousage
refgbl $syschar
local PATH string init="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
local PATH1 string init="dev:[bam059]"
LOCAL INPIC1 TYPE=STRING
LOCAL INPIC2 TYPE=STRING
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
  let PATH="/project/test_work/testdata/mipl/vgr/"
  let PATH1="/home/bam/"
end-if
let INPIC1 = "f1636832.raw"
let INPIC2 = "resfil.tst"
write "THIS IS A TEST OF MODULE TGETLOCV2"
write "MAKE A TEST COPY OF the Reseau File"
ibis-copy &"PATH1"reseau.test tf
label-list tf
ibis-list tf nr=26 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5)
tgetlocv2 (&"PATH"&INPIC1,tf)
write "List contents of file after add"
label-list tf
ibis-list tf nr=27 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5)
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getlocv2.hlp
1 GETLOCV2

    GETLOCV2 subroutine to extract reseau locations for VGR

2 USAGE

  Calling Sequence:  Call GETLOCV2(ibis,nrows,buf,fdata,lret)
 
c     Arguments: ibis = ibis file # from ibis_file_open call (input)
c                nrows = # of rows in the reseau file (input)
c                buf = nasty remnant from original code
c                      this array has 5 variables that are in common
c                      in the main and are as follows:
c                      frame,camera,filter,year and day - sorry about that
c                      (input)
c                fdata = array containing reseau locations (returned)
c                lret = error flag (returned)
c                       0 = good
c                       1 = error occurred


2 HISTORY

  Original Programmer: Gary Yagi, 20 April 1978
  Current Cognizant Programmer: Barbara McGuffie
  Source Language: Fortran

2 OPERATION

	This subroutine (along with PUTLOC) is designed to standardize 
        the manner in which records in the reseau location files are 
        read, stored and updated.  When reading or updating the file, 
        the following conventions should be observed:

	1. Entries are uniquely stored by frame number and camera serial
	   number (i.e. there may be two or more records with the same
	   frame number, but no more than one per camera.

	2. Any zero entry in the frame table FTBL corresponds to an unused
	   record.


	3. When a record is added or deleted from the file, this sequence 
	   should be followed:

		a. INITIALIZING THE FILE.  Before the file can be read
		   or updated, the following steps must be executed:
			i. open the file with ibis_file_open
			   with repositioning.
		       ii. get the number of rows in the file via
                           ibis_file_get
		b. READING A RESEAU RECORD.  The reseau locations for a 
		   specified frame number and camera serial number are read
		   from the file as follows:
		   	i. set BUF(1) =  frame number
		       ii. set BUF(2) =  camera serial number
		      iii. call GETLOCV2
		   GETLOCV2 will search the directory for the specified frame
		   and camera.  If BUF(2) = 0, then the first entry with the
		   matching frame number is used.  If an entry is found, then
		   GETLOCV2 will read the corresponding reseau location record.
		   Frame number, camera, filter, year and day are stored in
                   BUF(1) thru BUF(5). Reseau location information is stored in
		   fdata(1) through fdata(404). If no entry is found in the 
                   file, then GETLOCV2 returns lret as 1 - error condition.
		c. STORING A RESEAU RECORD.  The reseau locations for a spec-
		   ified frame number and camera serial number are stored into
		   the file as follows:
			i. close the file and open for update
		       ii. set BUF(1) = frame number
		      iii. set BUF(2) = camera serial number
		       iv. call PUTLOCV2
		   PUTLOCV2 will search the directory for the specified frame
		   and camera.  If an entry is found, then the information 
		   contained in BUF is written into the corresponding reseau
		   record location in the file, overwriting any old informa-
		   tion.  If no entry is found then the information is stored 
                   in the next row in the file.

3 WARNING

	   Extreme care must be exercised when using PUTLOCV2 to ensure
	   that the file is not inadvertantly destroyed through 
	   improper use of these routines.  It is requested that 
	   anyone using these routines to update the files please 
	   consult the author first.


3 BUFFERS

	BUF(1) = FRAME NUMBER
	BUF(2) = CAMERA NUMBER
	BUF(3) = FILTER
	BUF(4) = YEAR
	BUF(5) = DAY
	     .      				
	FDATA(1) = RES(1,1)
        FDATA(404) = RES(202,2)

3 EXAMPLE

	The following VICAR program reads a reseau location record from
	the reseau location file, changes the filter and date information,
	and re-writes the record into the file.

	Frame, camera, filter, year, and day information is held in JDATA 
        and reseau location record is held in RES.

		Subroutine main44
		implicit integer (a-z)
                integer jdata(5)
                real res(2,202)


		call xvunit( in, 'INP', 1, status)
		call xvopen( in, status)
                call ibis_file_open(in,ibis,'read',409,99999,
     -                        format,0,status)
                if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
                icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
                if ( nrows .lt. 0 ) call ibis_signal(ibis,icount,1)
		frame = 1875
		camera = 4
                jdata(1) = frame
                jdata(2) = camera	
   	        call getlocv2(ibis,nrows,jdata,res,iret)
                if ( iret .eq. 1 ) go to 999
		filter = 4
		year = 77
		day = 1
                jdata(3) = filter                
                jdata(4)
		call putlocv2(in,ibis,nrows,jdata,res)
                call ibis_file_close(ibis,' ',status)
		return
	990	call abend
		end
$!-----------------------------------------------------------------------------
$ create putlocv2.hlp
1 PUTLOCV2

    PUTLOCV2 subroutine to archive reseau locations for VGR

2 USAGE

  Calling Sequence:  call putlocv2(in,ibis,nrows,buf,fdata)
 
c     Arguments: in = input file unit number (input)
c                ibis = ibis file # from ibis_file_open call (input)
c                nrows = # of rows in the reseau file (input)
c                buf = nasty remnant from original code
c                      this array has 5 variables that are in common
c                      in the main and are as follows:
c                      frame,camera,filter,year and day - sorry about that
c                      (input)
c                fdata = array containing reseau locations (returned)


2 HISTORY

  Original Programmer: Gary Yagi, 20 April 1978
  Current Cognizant Programmer: Barbara McGuffie
  Source Language: Fortran

2 OPERATION

	This subroutine (along with GETLOC) is designed to standardize 
        the manner in which records in the reseau location files are 
        read, stored and updated.  When reading or updating the file, 
        the following conventions should be observed:

	1. Entries are uniquely stored by frame number and camera serial
	   number (i.e. there may be two or more records with the same
	   frame number, but no more than one per camera.

	2. Any zero entry in the frame table FTBL corresponds to an unused
	   record.


	3. When a record is added or deleted from the file, this sequence 
	   should be followed:

		a. INITIALIZING THE FILE.  Before the file can be read
		   or updated, the following steps must be executed:
			i. open the file with ibis_file_open
			   with repositioning.
		       ii. get the number of rows in the file via
                           ibis_file_get
		b. READING A RESEAU RECORD.  The reseau locations for a 
		   specified frame number and camera serial number are read
		   from the file as follows:
		   	i. set BUF(1) =  frame number
		       ii. set BUF(2) =  camera serial number
		      iii. call GETLOCV2
		   GETLOCV2 will search the directory for the specified frame
		   and camera.  If BUF(2) = 0, then the first entry with the
		   matching frame number is used.  If an entry is found, then
		   GETLOCV2 will read the corresponding reseau location record.
		   Frame number, camera, filter, year and day are stored in
                   BUF(1) thru BUF(5). Reseau location information is stored in
		   fdata(1) through fdata(404). If no entry is found in the 
                   file, then GETLOCV2 returns lret as 1 - error condition.
		c. STORING A RESEAU RECORD.  The reseau locations for a spec-
		   ified frame number and camera serial number are stored into
		   the file as follows:
			i. close the file and open for update
		       ii. set BUF(1) = frame number
		      iii. set BUF(2) = camera serial number
		       iv. call PUTLOCV2
		   PUTLOCV2 will search the directory for the specified frame
		   and camera.  If an entry is found, then the information 
		   contained in BUF is written into the corresponding reseau
		   record location in the file, overwriting any old informa-
		   tion.  If no entry is found then the information is stored 
                   in the next row in the file.

3 WARNING

	   Extreme care must be exercised when using PUTLOCV2 to ensure
	   that the file is not inadvertantly destroyed through 
	   improper use of these routines.  It is requested that 
	   anyone using these routines to update the files please 
	   consult the author first.


3 BUFFERS

	BUF(1) = FRAME NUMBER
	BUF(2) = CAMERA NUMBER
	BUF(3) = FILTER
	BUF(4) = YEAR
	BUF(5) = DAY
	     .      				
	FDATA(1) = RES(1,1)
        FDATA(404) = RES(202,2)

3 EXAMPLE

	The following VICAR program reads a reseau location record from
	the reseau location file, changes the filter and date information,
	and re-writes the record into the file.

	Frame, camera, filter, year, and day information is held in JDATA 
        and reseau location record is held in RES.

		Subroutine main44
		implicit integer (a-z)
                integer jdata(5)
                real res(2,202)


		call xvunit( in, 'INP', 1, status)
		call xvopen( in, status)
                call ibis_file_open(in,ibis,'read',409,99999,
     -                        format,0,status)
                if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
                icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
                if ( nrows .lt. 0 ) call ibis_signal(ibis,icount,1)
		frame = 1875
		camera = 4
                jdata(1) = frame
                jdata(2) = camera	
   	        call getlocv2(ibis,nrows,jdata,res,iret)
                if ( iret .eq. 1 ) go to 999
		filter = 4
		year = 77
		day = 1
                jdata(3) = filter                
                jdata(4)
		call putlocv2(in,ibis,nrows,jdata,res)
                call ibis_file_close(ibis,' ',status)
		return
	990	call abend
		end
$ Return
$!#############################################################################
