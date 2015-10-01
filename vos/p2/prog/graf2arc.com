$!****************************************************************************
$!
$! Build proc for MIPL module graf2arc
$! VPACK Version 1.9, Friday, March 05, 2010, 15:52:23
$!
$! Execute by entering:		$ @graf2arc
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
$ write sys$output "*** module graf2arc ***"
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
$ write sys$output "Invalid argument given to graf2arc.com file -- ", primary
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
$   if F$SEARCH("graf2arc.imake") .nes. ""
$   then
$      vimake graf2arc
$      purge graf2arc.bld
$   else
$      if F$SEARCH("graf2arc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake graf2arc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @graf2arc.bld "STD"
$   else
$      @graf2arc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create graf2arc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack graf2arc.com -mixed -
	-s graf2arc.f -
	-p graf2arc.pdf -
	-i graf2arc.imake -
	-t tstgraf2arc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create graf2arc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c               *** rev. D ***
c
c
c		program:      GRAF2ARC
c		author:       Barbara McGuffie
c		date written: 7 December 1988
c
c		purpose: To take a VICAR/IBIS graphics 1 file,
c		extract the (x,y) componants of its elememts
c		and produce an ARC/INFO UNGENERATE file.
c
c
c
c*************************************************************************
c       Revision A - BAM     12/88 
c         Initial release
c
c       Revision B - BAM     6/89 
c         modified for point file formats
c
c       Revision c - BAM     9/89 
c         ignored first zeroes in the file
c
c       Revision D - BAM     1/95 
c         included a 3d IBIS graphics input
c         for contours - Muller UK
c
c   05Feb2010 -lwk- replaced variables in format stmts with internal writes;
c		  program is screwy:  code allows for multiple output files
c		  (but does not assign unit #s), but pdf has only 1 output,
c		  which is consistent with pgm. ARC2GRAF ... for now, restrict
c		  input & output counts to 1.

c*************************************************************************

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE




        REAL      data(500000)          ! data buffer of values
        INTEGER   group_no              ! group number

	CHARACTER*72 infiles(20)        ! input file/s name

	CHARACTER*72 outfile(20)        ! output file name


	INTEGER   idef1		        ! dummy variable for subroutine calls

	INTEGER   incount		! no. of input files

	INTEGER   outcount		! no. of output files

        INTEGER   nfields               ! number of fields input

        LOGICAL   xvptst                ! a logical expression 

        LOGICAL   deb                   ! debug flag


        LOGICAL   EOF                   ! end of file flag

        LOGICAL   ZERO                  ! zero terminators encountered flag
 
        LOGICAL   three_d               ! 3D IBIS graphics file flag
        INTEGER   dimension             ! IBIS file dimension

        REAL      first_c               ! coordinates returned from GETGR
        REAL      second_c
        REAL      third_c



        INTEGER   len                   ! length of bytes for o/p file
     
        INTEGER   number_of_pairs       ! number of pairs of values

        INTEGER   ist                   ! start in values array

        COMMON /POINT/ point
        LOGICAL point                   ! for point files



	COMMON /OUTPUT/ output, j

	INTEGER   output(20)		! unit for output file/s
        INTEGER   j                     ! loop index / output file
       

        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set
                                        ! ( used with FLIP parameter )


        COMMON /FORMAT/ fields
	INTEGER   fields(3)		! format of the incoming data.
                                        ! first word contains the integer
                                        ! value equal to the group value;
                                        ! the second word contains the
                                        ! digits to the left of the decimal  
                                        ! point; the third word contains 
                                        ! digits to the right of the 
                                        ! decimal point
          
	REAL*4    scale                 ! scalar

        REAL*4    xoffset, yoffset      ! X and Y offsets

        INTEGER   low_elev, high_elev

        INTEGER   nstrings

	call xvmessage(' GRAF2ARC version 05-Feb-2010', ' ')

c------------------------------------------------------------------------------
c
c		              PARAMETER PROCESSING
c
c------------------------------------------------------------------------------

	call xvp( 'INP',     infiles,    incount)   ! input files
	call xvp( 'OUT',     outfile,    outcount ) ! output files
	call xvp( 'LOW',     low_elev,   idef1)     ! scale for data
	call xvp( 'HIGH',    high_elev,  idef1)     ! scale for data
	call xvp( 'SCALE',   scale,      idef1)     ! scale for data
	call xvp( 'XOFFSET', xoffset,    idef1)     ! x offset
	call xvp( 'YOFFSET', yoffset,    idef1)     ! y offset

        if ( incount .ne. outcount ) then     ! be sure we have enough files
	    call qprint 
     +	    ('INP/OUT  - unequal input/output files.  Please correct.')
	    call abend
	endif

        flip = xvptst( 'FLIP' )                ! for line/sample coordinates
        call xvp ( 'NLINES', nlines, idef1)

	three_d = xvptst('THREE_D')            ! 3-D flag
        if ( THREE_D ) then
            dimension = 3            
        else
            dimension = 2                      ! 2-D graphics
        end if

	call xvparm( 'FIELDS',  fields,  nfields, idef1, 0 )   ! input format
        if ( nfields .ne. 3 ) then             ! ensure we have a format
	    call qprint 
     +	    ('FIELDS  - incomplete format information.  Please correct.')
	    call abend
	endif

        do j = 1, nfields
            if ( fields(j) .eq. 0 ) then
                call qprint 
     +	        ('FIELDS  - incorrect format information. Please correct.
     +          ')
	        call abend
            endif
        end do


        point = xvptst( 'POINT' )           ! point file flag
	call xvp( 'GROUP', group_no, idef1) ! starting group number


        len   = fields(2) + fields(2)       ! compute record length
        if ( POINT ) len = len + fields(1)


        deb   = xvptst( 'DEB' )             ! debug flag

        if ( DEB ) then
            call qprint ( '     ' )
            call qprint ( ' GRAF2ARC - Revision D ')
            call qprint ( '     ' )
        end if


        do j = 1, incount
             call rdgr(j, j, dimension)    ! open input file/s
        end do


c------------------------------------------------------------------------------
c
c                            MAIN PROCESSING LOOP
c
c------------------------------------------------------------------------------


        nstrings = 0

	output(1) = 1	! arbitrary (lwk)

        do j = 1, incount              ! loop through input files

            ist = 1                    ! start of temp data buffer

            open ( UNIT=output(j), FILE=outfile(j), STATUS='NEW', ! open output
     +       RECL=len)

            number_of_pairs = 0        ! initialize value

 10         continue                   ! get coordinate pair

            call getgr ( j, ZERO, EOF, first_c, second_c, third_c )

            if ( .not. EOF ) then      ! check for end of file
     
                if ( .not. ZERO ) then           ! check for data

                                       ! see if we need this elevation
                    if (third_c.lt.low_elev) go to 10
                    if (third_c.eq.high_elev) go to 105

                    number_of_pairs  = number_of_pairs + 1  ! count pairs
                    data ( ist )     = first_c  / scale + xoffset
                    data ( ist + 1 ) = second_c / scale + yoffset
                    if ( dimension .eq. 3 ) group_no = third_c
                    ist   = ist + 2              ! update buffer pointer

                else                             ! write data to file 
                    if ( number_of_pairs .ne. 0 ) then
                        call write_data ( group_no, data, 
     -                                    number_of_pairs )
                        nstrings = nstrings + 1
       
                        if ( DEB .and. number_of_pairs .eq. 2 ) then
                            if ( POINT ) then
                                call qprint ( ' Point line ' )
                            else
                                call qprint ( ' Line segment' )
                            end if
                        else if ( DEB .and. number_of_pairs .gt. 2 )then
                            call qprint ( ' Line string ' )
                        end if       
                    
                        number_of_pairs = 0          ! reinitialize pointers
                        ist = 1
                        if ( .not. POINT .and. dimension .eq. 2) then
                            group_no = group_no + 1  
                        end if
                    end if
                end if

                go to 10               ! get more data

            end if


  105       continue
            write ( output(j), 110 )             ! put on the terminator end
            if ( POINT ) write ( output(j), 110 )! finish point file
  110       format ( 'END' ) 
            close ( unit=output(j), status='KEEP' )! close output file

            if ( DEB ) print *, ' File', j, ' completed.'

        end do

c------------------------------------------------------------------------------
c
c                            FINISH
c
c------------------------------------------------------------------------------

  
        do j = 1, incount                       ! close input file/s
             call clgr(j, j, 2)      
        end do


        if ( deb ) call qprint ( ' Task completed')
	return				         
	end


c*******************************************************************************
c
c     *** revision a ***
c
c     subroutine name: write_data
c
c     author: Barbara McGuffie
c
c     date: 12 December 1988
c
c     purpose: write data to output file
c
c     calling sequence: call write_data ( group_no, data, 
c                                         number_of_pairs )
c      
c****************************************************************************
c
c     Update history
c
c     Revision A - initial release
c
c****************************************************************************


        subroutine write_data ( group_no, data, number_of_pairs )

        implicit none

        INTEGER   group_no              ! group numgber
        REAL      data(500000)          ! data buffer of values
        INTEGER   number_of_pairs       ! number of pairs of values


        COMMON /POINT/ point
        LOGICAL point                   ! for point files


	COMMON /OUTPUT/ output, j

	INTEGER   output(20)		! unit for output file/s
        INTEGER   j                     ! loop index / output file


        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set
                                        ! ( used with FLIP parameter )

        COMMON /FORMAT/ fields

	INTEGER   fields(3)		! format of the incoming data.
                                        ! first word contains the integer
                                        ! value equal to the group value;
                                        ! the second word contains the
                                        ! digits to the left of the decimal  
                                        ! point; the third word contains 
                                        ! digits to the right of the 
                                        ! decimal point

	character*15 fstrng

        INTEGER   i                     ! loop index
        INTEGER   ist                   ! start of the data in real array



c---------------------------------------------------------------------------
c
c                       Start Executable Code
c
c---------------------------------------------------------------------------

       ist = 1
       if ( FLIP ) then
           do i = 1, number_of_pairs        ! flip coordinates if required
               call lstoxy ( data(ist), data(ist+1) )
               ist = ist + 2
           end do                  
       end if

       ist = 1                              ! start of data
       if ( POINT ) then

c          write ( output(j), 10 ) group_no, data(1), data(2)
c10        format ( I<fields(1)>,2F<fields(2)>.<fields(3)>)
c  above is non-standard, for new compiler must replace with:
	   fstrng = ' '
	   write( fstrng, 10) fields(1), fields(2), fields(3)
10	   format('(I',i4,',2F',i2,'.',i2,')')
	   write( output(j), fstrng) group_no, data(1), data(2)

       else

c          write ( output(j), 20 ) group_no 
c20        format ( I<fields(1)>)
c  above is non-standard, for new compiler must replace with:
	   fstrng = ' '
	   write( fstrng, 20) fields(1)
20	   format('(I',i4,')')
	   write( output(j), fstrng) group_no

           do i = 1, number_of_pairs
c              write ( output(j), 30 ) data(ist), data(ist + 1)
c30            format ( 2F<fields(2)>.<fields(3)>)
c  above is non-standard, for new compiler must replace with:
	       fstrng = ' '
	       write( fstrng, 30) fields(2), fields(3)
30	       format('(2F',i2,'.',i2,')')
	       write( output(j), fstrng) data(1), data(2)
               ist = ist + 2
           end do     

           write ( output(j), 40 )                ! put on an end
 40        format ( 'END' ) 

       end if

       return                            
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       line/sample coordinates to cartesian coordinates
c
 	subroutine lstoxy (a,b)

	implicit none

	real a, b, c


        COMMON /FLIP/  flip, nlines

        LOGICAL   flip                  ! output coordinates are line/sample
        INTEGER   nlines                ! no. of lines in the output data set


	c = a
	a = b

        if ( nlines .eq. 0 ) then                   ! for lfj 3/86
            b = c                                   ! no nlines required
        else
  	    b = nlines - c                          ! for specified nlines
        end if

	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create graf2arc.pdf
PROCESS HELP=*
# PARM INP      TYPE=STRING   COUNT=(1:20)                 DEF=""
# see comments in code (lwk - 05Feb2010)
 PARM INP      TYPE=STRING   COUNT=1                 DEF=""
 PARM OUT      TYPE=STRING   COUNT=1                      DEF=""
 PARM LOW      TYPE=INTEGER  COUNT=1                      DEF=0
 PARM HIGH     TYPE=INTEGER  COUNT=1                      DEF=10000
 PARM FIELDS   TYPE=INTEGER  COUNT=(1:3)                  DEF=(10,15,6)
 PARM FLIP     TYPE=KEYWORD  COUNT=(0:1) VALID=(FLIP)     DEF=--
 PARM THREE_D  TYPE=KEYWORD  COUNT=(0:1) VALID=(THREE_D)  DEF=--
 PARM NLINES   TYPE=INTEGER  COUNT=(0:1)		  DEF=0
 PARM SCALE    TYPE=REAL     COUNT=1                      DEF=1.0
 PARM XOFFSET  TYPE=REAL     COUNT=1                      DEF=0.0
 PARM YOFFSET  TYPE=REAL     COUNT=1                      DEF=0.0
 PARM GROUP    TYPE=INTEGER  COUNT=1                      DEF=1
 PARM POINT    TYPE=KEYWORD  COUNT=(0:1) VALID=POINT      DEF=--
 PARM DEB      TYPE=KEYWORD  COUNT=(0:1) VALID=DEB        DEF=--
END-PROC
.TITLE
VICAR/IBIS Program GRAF2ARC
.HELP
PURPOSE
-------

     GRAF2ARC converts IBIS Graphics-1 formatted files  to  either 
2-D or 3-D ARC/INFO UNGENERATE file format.  The program will 
reduce up to 20 input data sets.   
     Two dimensional output file data lines are numbered
starting from one and incremented  by one to the end  of the
file.
     Three dimensional output data begin with the elevation of the
string on the first line followed by the X,Y coordinate pairs on each
subsequent line.

EXECUTION
---------
Execution is initiated by:

GRAF2ARC INP (OUT1,OUT2...OUT20) PARAMETERS

See tutor mode for an explanation of the parameters.


RESTRICTIONS
------------

1) There can be at most twenty input files.

.LEVEL1
.VARIABLE INP
Input filename(s).
.VARIABLE OUT
Output filename.
.VARIABLE LOW
Low elevation
.VARIABLE HIGH
High elevation
.VARIABLE FLIP
Input  graphics  file 
coordinates are Y , X
(line,sample) format. 
.VARIABLE NLINES
Number of lines represented
in the input graphics file.
Used with FLIP option.
.VARIABLE SCALE
Data divisor.
.VARIABLE XOFFSET
X offset - first coordinate
.VARIABLE YOFFSET
Y offset - second coordinate
.VARIABLE FIELDS
Output data format.
.VARIABLE GROUP
Output group no
- 2 dimensional data
.VARIABLE THREE_D
IBIS 3 dimensional data
.VARIABLE DEB
Debug flag - Produces 
listing on monitor of 
graphics types.
.LEVEL2
.VARIABLE INP
             IBIS graphics input file name/s.

             Up to 20 input files may be input.

.VARIABLE OUT
             Output UNGENERATE filename.
 

.VARIABLE FLIP
             Indicates that coordinates from the input graphics 
             file/s are in Y,X ( line, sample ) format.   These 
             will be flipped to an X,Y coordinate system.

             Default is X,Y.
.VARIABLE NLINES
             Number of lines  represented  in  the input 
             graphics file. This variable is only needed
             if the FLIP keyword is invoked.
.VARIABLE SCALE
             Data will be divided by this scalar if required.

             Default = 1.0

.VARIABLE XOFFSET
             X offset - value added to IBIS graphics X values.
 
             Default = 0
.VARIABLE YOFFSET
             Y offset - value added to IBIS graphics Y values.

             Default = 0
.VARIABLE FIELDS
             Output data format. Informs the program of the Fortran
             formats required for the output data.


             First word contains  the integer format for the arc 
             descriptor.  For example if the arc is input in I10
             format,  the user would  code the number 10 in this 
             field. The second and third words describe the X, Y
             coordinate data pairs.   It  is  assumed that these 
             values are in F format.  The second  word  contains 
             the  digits  to the left of the decimal point;  the
             third  word  contains  digits  to  the right of the 
             decimal point.

             Defaults = 10, 15, 6
.VARIABLE DEB
                               Debug flag 

             Produces listing on monitor of digitized 
             types (e.g. LINE SEGMENT, LINE STRING ).
             This is a program aid for debugging code.

 
                              Valid = "DEB"
.END
$ Return
$!#############################################################################
$Imake_File:
$ create graf2arc.imake
#define PROGRAM graf2arc

#define MODULE_LIST graf2arc.f 

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstgraf2arc.pdf
PROCEDURE
BODY

IBIS-GEN    A DATAC=(1,2) NC=2 NR=15 'COL DATA=(10,10,20,15,20,5,10,10,0,0, +
            10,25,20,30,20,20,10,25,0,0, +
            25,12.5,25,22.5,35,17.5,25,12.5,0,0)
IBIS-COPY   A B SR=1 SC=1 NC=2 'ROW
IBIS-LIST   B
GRAF2ARC    B A.TMP 'FLIP
!dcl type a.tmp
ush cat A.TMP
END-PROC

$ Return
$!#############################################################################
