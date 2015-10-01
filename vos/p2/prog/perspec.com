$!****************************************************************************
$!
$! Build proc for MIPL module perspec
$! VPACK Version 1.9, Monday, June 09, 2014, 17:13:29
$!
$! Execute by entering:		$ @perspec
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
$ write sys$output "*** module perspec ***"
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
$ write sys$output "Invalid argument given to perspec.com file -- ", primary
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
$   if F$SEARCH("perspec.imake") .nes. ""
$   then
$      vimake perspec
$      purge perspec.bld
$   else
$      if F$SEARCH("perspec.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake perspec
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @perspec.bld "STD"
$   else
$      @perspec.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create perspec.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack perspec.com -mixed -
	-s perspec.f -
	-i perspec.imake -
	-p perspec.pdf -
	-t tstperspec.pdf tstperspec.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create perspec.f
$ DECK/DOLLARS="$ VOKAGLEVE"
        include 'VICMAIN_FOR'
c       
        subroutine main44

        implicit none
c
        integer*4 status, def, count
	integer*4 dataformat,ninpfile
        integer*4 rdgr,wrgr,nextgr,putgr,getgr,clgr		!setgr

        real*4    viewelev, viewazi
        real*4    theta, phi, d2r
        real*4    x, y, z,  x2, y2
	real*4    rho, origin(3), zscale
        real*4    sinth, costh, costhcosph, sinthcosph, sinph
        real*4    costhsinph, sinthsinph, cosph

	logical*4 eof, zero
	character*3  dataform
        character*80  inpfile,outfile


        common /perspeccom/ dataformat, origin, zscale, rho,
     +                  sinth, costh, costhcosph, sinthcosph, sinph,
     +                  costhsinph, sinthsinph, cosph


       call ifmessage('PERSPEC version 2-10-2013 (64-bit) - rjb')

c______________________________________________________________________________
c                       start executable code
c------------------------------------------------------------------------------
c
        d2r = acos(-1.0)/180.0     !degree to radian conversion PI/180



        call xvparm('INP',inpfile,count,def,1)
          ninpfile=index(inpfile,'   ') - 1

        call xvp ('DATAFORM', dataform, count)
        if (dataform(1:3) .eq. 'XYZ') then              !X,Y,Z
            dataformat = 1
        else if (dataform(1:3) .eq. 'YXZ') then         !Y,X,Z
            dataformat = 2
        else if (dataform(1:3) .eq. 'LSZ') then         !line,sample,Z
            dataformat = 3
        else
            dataformat = 1                              !default is X,Y,Z
        endif

        call xvp ('ZSCALE', zscale, count )

c        call xvparm ('SCALE', scale, count, def,1 )
c        autoscaling = (def .eq. 1)
c        if (scale .ne. 0.0)  scale = 1.0/scale

        call xvp ('DISTANCE', rho, count)
        call xvp ('ELEV', viewelev, count)
        call xvp ('AZIMUTH', viewazi, count)
        call xvp ('ORIGIN', origin(1), count)
        phi = 90.0 - viewelev
        theta = 90.0 - viewazi

        sinth = sin (theta * d2r)
        costh = cos (theta * d2r)
        sinph = sin (phi * d2r)
        cosph = cos (phi * d2r)
        costhcosph = costh*cosph
        sinthcosph = sinth*cosph
        costhsinph = costh*sinph
        sinthsinph = sinth*sinph

c                       Open the input graphics file
        status = rdgr (1,1,3)
        if (status.ne.1) call signalgr(1,status,1)

c                       If output exists then output 2-D perspective
        call xvparm ('OUT', outfile, count, def,1 )
        if (def .eq. 0) then
            status = wrgr ( 1, 2, 3 )
            if (status.ne.1) call signalgr(2,status,1)
            eof = .false.
            do while (.not. eof)
c                       scan for the beginning of a line string
                status = nextgr (1,eof,x,y,z)
                if (status.ne.1) call signalgr(1,status,1)
                if (eof) go to 199

                zero = .false.
                do while (.not. zero .and. .not. eof)
                    call perspective (x, y, z,  x2, y2)
                    status = putgr (2,x2,y2,0.0)
                    if (status.ne.1) call signalgr(2,status,1)
                    status = getgr ( 1,zero,eof,x,y,z)
                    if (status.ne.1) call signalgr(1,status,1)
                enddo
                status = putgr (2,0.0,0.0,0.0)
                if (status.ne.1) call signalgr(2,status,1)
            enddo
199         continue
            status = clgr (1)
            if (status.ne.1) call signalgr(1,status,1)
            status = clgr (2)

            if (status.ne.1) call signalgr(2,status,1)
            return
        endif


        return
        end

C***********************************************************
        subroutine perspective (x, y, z, x2, y2)
        implicit none
        real    x, y, z,  x2, y2
        real    xp, yp, zp, x_eye, y_eye, z_eye, tmp

        integer dataformat
        real    rho, origin(3), zscale
        real    sinth, costh, costhcosph, sinthcosph, sinph
        real    costhsinph, sinthsinph, cosph

        common /perspeccom/ dataformat, origin, zscale, rho,
     +                  sinth, costh, costhcosph, sinthcosph, sinph,
     +                  costhsinph, sinthsinph, cosph

        xp = x - origin(1)
        yp = y - origin(2)
        zp = (z - origin(3))/zscale
        if (dataformat .eq. 2) then
            tmp = xp
            xp = yp
            yp = tmp
        else if (dataformat .eq. 3) then
            tmp = xp
            xp = yp
            yp = -tmp
        endif
        x_eye = -xp*sinth + yp*costh
        y_eye = -xp*costhcosph - yp*sinthcosph  + zp*sinph
        z_eye = 1.0 - (xp*costhsinph + yp*sinth*sinph + zp*cosph)/rho
        x2 = x_eye/z_eye
        y2 = y_eye/z_eye

        return
        end


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create perspec.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM perspec

   To Create the build file give the command:

		$ vimake perspec			(VMS)
   or
		% vimake perspec			(Unix)


************************************************************************/


#define PROGRAM	perspec

#define MODULE_LIST perspec.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create perspec.pdf
PROCESS HELP=*
 PARM INP      TYPE=(STRING,80)  COUNT=1
 PARM OUT      TYPE=(STRING,80)  COUNT=0:1 DEFAULT=--
 PARM ELEV     TYPE=REAL    DEFAULT=0       valid=(0:180)      !30
 PARM AZIMUTH  TYPE=REAL    DEFAULT=180 valid=(0:360)     !180
 PARM DISTANCE TYPE=REAL    DEFAULT=0
 PARM ORIGIN   TYPE=REAL    COUNT=3   DEFAULT=(0,0,0)
 PARM ZSCALE   TYPE=REAL    DEFAULT=1.0
 PARM DATAFORM TYPE=(STRING,3)  VALID=(XYZ,YXZ,LSZ) DEFAULT=XYZ
END-PROC

.TITLE
VICAR/IBIS Program "perspec"
.HELP        
PURPOSE      
             
    "perspec" converts 3-D IBIS graphics-1 files into a true 2-D perspective
3-D IBIS graphics-1 file.

OPERATION

    Perspec allows the user to manipulate the vertices in an IBIS-1 3-D graphics
file. When a graphics file is constructed the vertices are in a specific coordinate
system. Program PLTGRAF can be used to plot the object in whatever coordinates
given in the list. Perspec then allows the IBIS-1 graphics file coordinate system
to be modified. It also allows the user to adjust the origin and distance as well
as the angular relationships.


PARAMETERS

    INP and OUT are the names of the input and output IBIS files respectively,

    ELEV and AZIMUTH are the angles. 

    ORIGIN is the location of the origin of the perspective. Not the origin
    of the axes.

    DISTANCE is the location of the observer away from the ORIGIN

    ZSCALE allows the Z-AXIS to be scaled differently from the X- and Y-AXES.

    DATAFORM is how to show the X-, Y- and Z-axes. Normally, X and Y are horizontal
and Z is vertical.   

IBIS GRAPHICS-1 FILES

    IBIS graphics-1 files are a way to store 2-dimensional or 3-dimensional
polygonal objects in a file. The file contains a list of the successive vertices
of the polygons. When the list contains a line of all zeroes then it terminates
that face of the polygon. In the view of a drawing program it would be a
"pen-up" command. 

    With the advent of the IBIS-2 format graphics-1 files are deprecated.
In the new IBIS-2 format graphics-1 can replace that format via an index
column. In the IBIS-2 context each row is associated with a single vertex,
and the row order in a column determines the sequence of plotting the
vertices. A pen-up type command would involve a change of the value in
the index column. 


 
.PAGE        
EXECUTION    
             
Examples:    
             
perspec inp.gr1 out.gr1 ELEV=30 AZIMUTH=135 DISTANCE=200

perspec inp.gr1 out.gr1 ELEV=30 AZIMUTH=135 DISTANCE=200  ZSCALE=10  ORIGIN=(100,100,10)

.Page

Code extracted from PLOT3D

Original Programmer:    Ray Bambery , 10 Feb 2013            
             
Cognizant Programmer:   Ray Bambery
             
.LEVEL1      
.VARIABLE INP
IBIS 3-D graphics file name.
.VARIABLE OUT
 2-D IBIS graphics file.
.VARIABLE ELEV
Observer elevation angle
in degrees above horizon.
.VARIABLE AZIMUTH
Observer azimuthial angle
in degrees east of north.
.VARIABLE DISTANCE
Observer distance from origin
(in same units as graphics).
.VARIABLE ORIGIN   
The view origin (observer looks
toward origin) in same format
as 3-D graphics file.
.VARIABLE ZSCALE
Divisor to convert scale of
Z values into X,Y units.
.VARIABLE DATAFORM 
3-D data format:
XYZ for (X,Y,Z)
YXZ for (Y,X,Z)
LSZ for (line,sample,Z)
.LEVEL2      
.VARIABLE INP
IBIS 3-D graphics file name.
.VARIABLE OUT
If an output file is specified then a perspective transformation will be
performed and the result output in an 2-D IBIS graphics file.  The output
is in (X,Y) format.
.VARIABLE ELEV
The elevation angle of the observer in degrees above the horizon.
.VARIABLE AZIMUTH
The azimuthial angle of the observer in degrees east of north where 
north is the y direction.  Default is to look from the south (180 degrees).
.VARIABLE DISTANCE
The distance of the observer from origin (in same units as graphics file).  
This controls the amount of perspective. 
.VARIABLE ORIGIN   
The 3-D location of the origin toward which the observer looks.  
In same format as the 3-D graphics file (e.g. XYZ, LSZ).
.VARIABLE ZSCALE
Divisor scale factor to convert scale of Z values same scale as the 
X and Y values.
.VARIABLE DATAFORM 
The 3-D graphics-1 file contains triplets of real numbers.  
The DATAFORM parameter specifies the format of the triplet:  
XYZ for (X,Y,Z),  YXZ for (Y,X,Z),  LSZ for (line,sample,Z).

.END







$ Return
$!#############################################################################
$Test_File:
$ create tstperspec.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch
! Nov 15, 2012 - RJB
! TEST SCRIPT FOR PERSPEC
!
! tests IBIS-1 graphics files
!
! Vicar Programs:
!   ibis-gen ibis-list pltgraf
!
! External programs
!       gnuplot 4.6 or greater
!           
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display 
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!   In batch mode it produces files testx.eps by calling gnuplot
!       to create the encapsulated postscript file which can be
!       later viewed with ghostscript or gimp
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!            
! External test data: 
!       <none>
!
! Output:    
!   GENed test.imga sets, .gpi and .eps files and intermediate 
!       tmp* files 
!   the *.gpi data produced by pltgraf are gnuplot scripts
!            

    refgbl $autousage
    refgbl $echo
body
    let $autousage="none"
    let _onfail="stop"
    let $echo="yes"

! Create a polygon consisting of a cube with a diamond to one side; this
! is an attempt to make a shape that will have a recognizable orientation.
    ibis-gen cube.pic NC=3 NR=26 'ibis-1 'row DATACOL=(1,2,3) +
        data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51, +
    1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0, +
    51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0, +
    36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)

! TEST 1 - plot genned data
    ibis-list cube.pic NC=3 NR=26 GR1DIM=3
    pltgraf cube.pic title="Plot of original data" dim=3 +
        plot=pgrf0 mode=axis xrange=(-10,60) yrange=(-10,60) +
        xlabel="X meters" ylabel="Y meters"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot pgrf0.gpi
end-if

! TEST 2 - plot 1st reoriented data
! Plot the cube default style to an output file, then use pltgraf to display
! the file
    perspec cube.pic cube_perspective azimuth=30 distance=100 origin=(26,26,26)
    ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
    pltgraf cube_perspective title="Perspective view of a cube - 30 degrees AZ" dim=3 +
            plot=pgrf1 mode=axis xrange=(-50,50) yrange=(-50,50) +
            xlabel="X meters" ylabel="Y meters"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot pgrf1.gpi
end-if       

! TEST 3 - plot 2nd reoriented data
    perspec cube.pic cube_perspective2 azimuth=40 distance=100 origin=(26,26,26)
    ibis-list cube_perspective2 NC=3 NR=26 GR1DIM=3
    pltgraf cube_perspective2 title="Perspective view of a cube - 40 degrees AZ" dim=3 +
            plot=pgrf2 mode=axis xrange=(-50,50) yrange=(-50,50) +
            xlabel="X meters" ylabel="Y meters"
  
if (mode = "nobatch" or mode = "inter")
    ush gnuplot pgrf2.gpi
end-if
 
let $echo="no" 
end-proc

$!-----------------------------------------------------------------------------
$ create tstperspec.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

    ibis-gen cube.pic NC=3 NR=26 'ibis-1 'row DATACOL=(1,2,3)  +
        data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51,  +
    1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0,  +
    51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0,  +
    36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)
Beginning VICAR task ibis
    ibis-list cube.pic NC=3 NR=26 GR1DIM=3
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:26
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        1.00
        1.00       51.00        1.00
       51.00       51.00        1.00
       51.00        1.00        1.00
        1.00        1.00        1.00
        1.00        1.00       51.00
        1.00       51.00       51.00
       51.00       51.00       51.00
       51.00        1.00       51.00
        1.00        1.00       51.00
        0.00        0.00        0.00
        1.00       51.00        1.00
        1.00       51.00       51.00
        0.00        0.00        0.00
       51.00       51.00        1.00
       51.00       51.00       51.00
        0.00        0.00        0.00
       51.00        1.00        1.00
       51.00        1.00       51.00
        0.00        0.00        0.00
       36.00       41.00       26.00
       41.00       41.00       36.00
       36.00       41.00       46.00
       31.00       41.00       36.00
       36.00       41.00       26.00
        0.00        0.00        0.00
    pltgraf cube.pic title="Plot of original data" dim=3  +
        plot=pgrf0 mode=axis xrange=(-10,60) yrange=(-10,60)  +
        xlabel="X meters" ylabel="Y meters"
if (mode = "nobatch" or mode = "inter")
end-if
    perspec cube.pic cube_perspective azimuth=30 distance=100 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:26
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        6.82      -18.64        0.00
       37.59      -27.52        0.00
      -13.90      -37.97        0.00
      -31.29      -22.90        0.00
        6.82      -18.64        0.00
        6.82       18.64        0.00
       37.59       27.52        0.00
      -13.90       37.97        0.00
      -31.29       22.90        0.00
        6.82       18.64        0.00
        0.00        0.00        0.00
       37.59      -27.52        0.00
       37.59       27.52        0.00
        0.00        0.00        0.00
      -13.90      -37.97        0.00
      -13.90       37.97        0.00
        0.00        0.00        0.00
      -31.29      -22.90        0.00
      -31.29       22.90        0.00
        0.00        0.00        0.00
       -1.41        0.00        0.00
       -6.91       12.58        0.00
       -1.41       24.39        0.00
        3.75       11.83        0.00
       -1.41        0.00        0.00
        0.00        0.00        0.00
    pltgraf cube_perspective title="Perspective view of a cube - 30 degrees AZ" dim=3  +
            plot=pgrf1 mode=axis xrange=(-50,50) yrange=(-50,50)  +
            xlabel="X meters" ylabel="Y meters"
if (mode = "nobatch" or mode = "inter")
end-if
    perspec cube.pic cube_perspective2 azimuth=40 distance=100 origin=(26,26,26)
Beginning VICAR task perspec
PERSPEC version 2-10-2013 (64-bit) - rjb
    ibis-list cube_perspective2 NC=3 NR=26 GR1DIM=3
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:26
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        2.28      -18.49        0.00
       36.34      -25.79        0.00
       -4.76      -38.59        0.00
      -34.17      -24.25        0.00
        2.28      -18.49        0.00
        2.28       18.49        0.00
       36.34       25.79        0.00
       -4.76       38.59        0.00
      -34.17       24.25        0.00
        2.28       18.49        0.00
        0.00        0.00        0.00
       36.34      -25.79        0.00
       36.34       25.79        0.00
        0.00        0.00        0.00
       -4.76      -38.59        0.00
       -4.76       38.59        0.00
        0.00        0.00        0.00
      -34.17      -24.25        0.00
      -34.17       24.25        0.00
        0.00        0.00        0.00
        2.41        0.00        0.00
       -2.34       12.68        0.00
        2.41       24.37        0.00
        6.81       11.72        0.00
        2.41        0.00        0.00
        0.00        0.00        0.00
    pltgraf cube_perspective2 title="Perspective view of a cube - 40 degrees AZ" dim=3  +
            plot=pgrf2 mode=axis xrange=(-50,50) yrange=(-50,50)  +
            xlabel="X meters" ylabel="Y meters"
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
$ Return
$!#############################################################################
