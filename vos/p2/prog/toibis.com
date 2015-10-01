$!****************************************************************************
$!
$! Build proc for MIPL module toibis
$! VPACK Version 1.8, Friday, August 25, 1995, 11:16:13
$!
$! Execute by entering:		$ @toibis
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
$ write sys$output "*** module toibis ***"
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
$ write sys$output "Invalid argument given to toibis.com file -- ", primary
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
$   if F$SEARCH("toibis.imake") .nes. ""
$   then
$      vimake toibis
$      purge toibis.bld
$   else
$      if F$SEARCH("toibis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake toibis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @toibis.bld "STD"
$   else
$      @toibis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create toibis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack toibis.com -
	-s toibis.f -
	-i toibis.imake -
	-p toibis.pdf -
	-t tsttoibis.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create toibis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	subroutine main44
	implicit none

	integer num_values, 	! count of values specified for a parameter
c     +	    defaults_used,	! default used for param (=1) or specified (=0)
     +	    status,		! status from xvunit; dummy for xvread,xvclose
     +	    iform, oform,	! formats for input and output files
     +	    in_unit,		! unit number for reading input file
     +	    out_unit,		! output: unit # or IBIS graphics #
     +	    nl, ns,		! # of lines,samples from input file label
     +	    pix_size,		! # of bytes used to store an input pixel
     +	    new_pix_size	! # of bytes used to read an input pixel
c	character*4 i_format,	! format string for input file
c     +	    o_format		! format string for output file
	character*80 message	! place to write messages prior to qprint call
	logical xvptst		! procedure to test for presence of a keyword
	integer ncol,		! # cols for interface file; # dim for graphics 
     +	    nrow,		! number of rows in interface file
     +	    buf_ptr,		! pointer into line buffer
     +	    i, j
	integer ibis,           ! interger for calls to ibis routines
     +      record              ! record number for ibis_record_write 
	integer*2 zext		! VAX procedure to convert byte to integer*2
	real real		! VAX procedure to convert integer*2 to real

	integer max_ns, max_ncol
	parameter (
     +	    max_ns = 10000,
     +	    max_ncol = 40)

	byte i1_line(4*max_ns)		! \
	integer*2 i2_line(2*max_ns)	!  \ buffers to contain one line of
	integer i4_line(max_ns)		!  / samples & to read them in different
	real r4_line(max_ns)		! /  formats
	equivalence (i1_line, i2_line, i4_line, r4_line)

	real r4_buffer(max_ncol)	! work buffer for assembling data
	integer columns(max_ncol)	! columns to write in write rec call

	include 'fortport'

	call ifmessage('TOIBIS version 6-MAR-95')

! Set the input form. If the user uses the default (the input file's format),
! then we'll specify a U_FORMAT of REAL so that the XVREAD calls will convert
! the data from the input file format to real format for output. If the user
! specifies an input format, then we'll ignore the format that the file may
! have and we'll not specify a U_FORMAT. In the XVREAD call, we'll supply the
! equivalenced buffer that corresponds to the input file format specified by
! the user. If this specified format needs to be converted to real, we'll do
! it prior to writing it out. A value of iform > 0 signifies that we'll be
! overriding the input file format.
	if (xvptst('BYTE')) then
	    iform = 1
	else if (xvptst('HALF')) then
	    iform = 2
	else if (xvptst('FULL')) then
	    iform = 3
	else if (xvptst('REAL')) then
	    iform = 4
	else
	    iform = 0
	endif

! Get the output file format desired; the default is INTERFACE
	if (xvptst('GRAPHICS')) then
	    oform = 1
	    call xvp ('NDIM', ncol, num_values)
	else
	    oform = 2
	    call xvp ('NCOL', ncol, num_values)
	endif

! Open the input file
	call xvunit (in_unit, 'INP', 1, status,' ')

	if (iform .eq. 0) then
!     Open so that we're using XVREAD to convert the input file to real format
	    call xvopen (in_unit, status, 'OP', 'READ', 'OPEN_ACT', 'SA',
     +		'IO_ACT', 'SA', 'U_FORMAT', 'REAL',' ')

	else
!     Open so that we're ignoring the input file format and we're doing our
!     own format conversion
	    call xvopen (in_unit, status, 'OP', 'READ', 'OPEN_ACT', 'SA',
     +		'IO_ACT', 'SA',' ')
	endif

! Get the size of the file, and adjust the sample count if we're altering
! the input file format
!                          iform
!              |    1        2        3,4
!            --------------------------------
!            1 |    ok    ns=ns/2  ns=ns/4
! pix_size   2 | ns=ns*2     ok    ns=ns/2
!            4 | ns=ns*4  ns=ns*2     ok
!
	call xvget (in_unit,status,'PIX_SIZE',pix_size,'NL',nl,
     +						'NS',ns,' ')

	if (iform .ne. 0) then
	    if (pix_size.ne.1 .and. pix_size.ne.2 .and.
     +					 pix_size.ne.4) then
		write (message, 10) pix_size
 10		format (' Input file uses ',I3,
     +              ' bytes per pixel; program limited to 1, 2, 4.')
		call xvmessage (message,' ')
		call abend
	    endif

	    new_pix_size = iform
	    if (new_pix_size .eq. 3) new_pix_size = 4
	    ns = ns  *  pix_size/new_pix_size
	endif

	if (ns .gt. max_ns) then
	    write (message, 12) ns, max_ns
 12	    format (' Input file number of samples (',I7,
     +          ') exceeds program limit (',I7,')')
	    call xvmessage (message,' ')
	    call abend
	endif

! Open the output file, allocating space for the interface file
	if (oform .eq. 1) then		! graphics file
	    out_unit = 1
	    call wrgr (1, out_unit, ncol)
	    nrow = (nl * ns) / ncol
	else				! interface file
	    nrow = (nl * ns) / ncol
c	    call wrfil (out_unit, 1, nrow, ncol, error_sw)
	    call xvunit(out_unit,'OUT',1,status,' ')
	    call ibis_file_open(out_unit,ibis,'WRITE',ncol,nrow,
     +                  ' ',' ',status)
            if (status.ne.1) call ibis_signal(ibis,status,1)
            call ibis_file_set(ibis,'NR',nrow,status)
            if (status.ne.1) call ibis_signal(ibis,status,1)
            call ibis_file_set(ibis,'NC',ncol,status)
            if (status.ne.1) call ibis_signal(ibis,status,1)

!     Initialize for record write call
	    do i = 1, ncol
		columns(i) = i
                call ibis_column_set(ibis,'FORMAT','REAL',i,status)
	    enddo
      	    call ibis_record_open(ibis,record,' ',columns,
     +                    ncol,'REAL',status)
            if (status.ne.1) call ibis_signal(ibis,status,1)
	endif

! Transfer the data from the input file to the output file
	buf_ptr = max_ns + 1

	do i = 1, nrow
!     Transfer a set of data from the input line to the output data

	    go to (400, 410, 420, 430, 400), iform+1

!     Transfer real*4 data (either automatically converted by xvread or
!     specified by user)
 400	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = r4_line(buf_ptr)
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500

!     Transfer byte data (integer) (value is converted first to integer*2,
!     thence to real)
 410	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = real (BYTE2INT(i1_line(buf_ptr)))
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500

!     Transfer half data (2 bytes, integer)
 420	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = real (i2_line(buf_ptr))
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500

!     Transfer full data (4 bytes, integer)
 430	    do j = 1, ncol
		if (buf_ptr .gt. ns) then
		    buf_ptr = 1
		    call xvread (in_unit, r4_line, status,' ')
		endif
		r4_buffer(j) = real (i4_line(buf_ptr))
		buf_ptr = buf_ptr + 1
	    enddo
	    go to 500


! Write out the data for this row
 500	    if (oform .eq. 1) then
		call putgr (out_unit, r4_buffer(1), r4_buffer(2),
     +		    r4_buffer(3))
	    else
c		call putrec (out_unit, ncol, columns, r4_buffer,
c     +		    i, nrow, work_buf)
	        call ibis_record_write(record,r4_buffer,i,status)
                if (status.ne.1) call ibis_signal(ibis,status,1)
	    endif

	enddo  ! looping for all rows in output file

! Close the input and output files; don't need to close the interface file
	call xvclose (in_unit, status,' ')
	if (oform .eq. 1) then
	       call clgr (out_unit)
        else
   	       call ibis_record_close(record,status)
               if (status.ne.1) call ibis_signal(ibis,status,1)
               call xvclose(out_unit, status,' ')
	endif
	return
	end  ! main44
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create toibis.imake
#define  PROGRAM   toibis

#define MODULE_LIST toibis.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define FTNINC_LIST fortport

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create toibis.pdf
PROCESS help=*
    parm INP	type=string
    parm OUT	type=string
    parm IFORM	type=keyword count=(0:1) valid=(byte,half,full,real) default=--
    parm OFORM	type=keyword valid=(graphics,interface) default=interface
    parm NDIM	type=integer default=2
    parm NCOL	type=integer default=2
END-PROC
.TITLE
VICAR Program TOIBIS
.HELP
PURPOSE
The TOIBIS program converts a file with an arbitrary line count, sample 
count, and format into an IBIS graphics or interface file.

TAE COMMAND LINE FORMAT
                       [ { 'BYTE } ]  [ {'GRAPHICS [NDIM=n] } ]
    toibis INP=a OUT=b [ { 'HALF } ]  [ {                   } ]
                       [ { 'FULL } ]  [ {'INTERFACE [NCOL=n]} ]
                       [ { 'REAL } ]

where
    INP		    The file with some line count, sample count, and data
		    format  (string).
    OUT		    The IBIS graphics or interface file produced  (string).
    IFORM	    A switch indicating how the INP file's data format should
		    be interpreted; default is to use data format as specified
		    in file's label. The following switches ignore the label:
	'BYTE	    Read INP file's data as if it were one-byte integers
	'HALF	    Read INP file's data as if it were two-byte integers
	'FULL	    Read INP file's data as if it were four-byte integers
	'REAL	    Read INP file's data as if it were four-byte reals
    OFORM	    A switch indicating the type of IBIS file desired; default
		    is to create an INTERFACE file.
	'GRAPHICS   IBIS graphics file
	'INTERFACE  IBIS interface file
    NDIM	    Dimension of file's coordinate sets if OFORM=GRAPHICS;
		    not used otherwise
    NCOL	    Number of columns in file if OFORM=INTERFACE; not used 
		    otherwise

    []  signifies an option (e.g., argument "NDIM=n" is optional)
    {}  signifies a mutually exclusive choice (e.g., if one of the
	format switches is used, only one of the set {BYTE, HALF, FULL,
	REAL} may be chosen)

EXECUTION
The input file is a stream of numbers in binary (as opposed to ASCII) format;
TOIBIS provides the means for breaking the stream up into a stream of rows
of data or of coordinate sets. For example, given the stream 
    a, b, c, d, e, f, g, h, i, j, k, l

in file "a", one can create a set of 2D coordinates in file "b" with TOIBIS:
   toibis a b 'GRAPHICS
    result: (a,b), (c,d), (e,f), (g,h), (i,j), (k,l)

or one can create a set of 3D coordinates:
    toibis a b 'GRAPHICS NDIM=3
    result: (a,b,c), (d,e,f), (g,h,i), (j,k,l)

or one can create rows of 3 columns:
    toibis a b 'INTERFACE NCOL=3
    result:    a    b    c
               d    e    f
               g    h    i
               j    k    l

or one can create rows of 6 columns:
    toibis a b 'INTERFACE NCOL=6
    result:    a    b    c    d    e    f
               g    h    i    j    k    l

The line and sample counts are only used to determine the total length of
the stream of values.

In some cases, the label of the input file will claim that the data format
is BYTE, but the bits forming the data should really be interpreted as
reals for them to make sense. It is for these cases that the IFORM switch
is provided. IFORM causes TOIBIS to ignore the data format label and to 
read the data as if the label matched the specified switch. If the switch
matches the label, then the switch could be left out.


RESTRICTIONS
 1. The graphics file has a maximum dimension of 40.
 2. The interface file has a maximum dimension of 40.
 3. The number of pixels used to represent data in the input file must be
    1, 2, or 4 if using the switches 'BYTE, 'HALF, 'FULL, or 'REAL.
 4. The number of samples in the original file is limited by
     a. If not using format switch: maximum number of samples is 10000.
     b. If using format switch:
	    ns * pix_size/new_pix_size  <=  10000
	where
	    ns		is the count of samples as written in the input
			file header.
	    pix_size	is the number of bytes used to represent one
			element of the input file (e.g., HALF format
			has pix_size=2, and FULL format has pix_size=4).
	    new_pix_size
			is the number of bytes used to represent one
			element of the output file.
NOTE:(FR87138)
Currently (1995) when the IBIS-2 Graphics subroutine software (IBISGR.COM)
creates a IBIS graphics file, the default format is IBIS-1 graphics.  This
allows compatability with unported IBIS programs as well as interchange of
files between various machines.  Eventually the default format of the
graphics files will become IBIS-2


HISTORY:
           6 MAR 1995 Made portable for UNIX  CRS  (CRI)
          25 AUG 1995 Added note to PDF as per FR87138

.LEVEL1
.VARIABLE INP
File with arbitrary line,sample
format.
.VARIABLE OUT
Resulting IBIS graphics or 
interface file.
.VARIABLE IFORM
Format to be used to interpret
the INP file; if this keyword
is omitted, then the format in
the file label is used. 
Specifiable types are BYTE,
HALF, FULL, & REAL.
.VARIABLE OFORM
Format to be used to create the
output file. Specifiable types
are GRAPHICS & INTERFACE; 
INTERFACE is the default.
.VARIABLE NDIM
If OFORM is GRAPHICS, NDIM is
used to specify the number of
values in each coordinate set
in the graphics file. Default
value is 2.
.VARIABLE NCOL
If OFORM is INTERFACE, NCOL is
used to specify the number of
columns in the interface file.
Default value is 2.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttoibis.pdf
procedure
refgbl $echo
refgbl $autousage
body 
let _onfail="continue"
let $echo="yes"
let $autousage="none"
!
gen out=a.img nl=1 ns=12
toibis inp=a.img out=b.ibis 'graphics ndim=3
!For Porting to UNIX
!REPLACED: grlist inp=b.ibis dim=3
!  with:
ibis-list inp=b.ibis nc=3 nr=4 gr1dim=3
toibis inp=a.img out=b.ibis 'byte 'interface ncol=3
!For Porting to UNIX
!REPLACED: qrep inp=b.ibis
!  with:
ibis-list inp=b.ibis
!
end-proc
$ Return
$!#############################################################################
