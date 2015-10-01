$!****************************************************************************
$!
$! Build proc for MIPL module conlab
$! VPACK Version 1.8, Monday, November 27, 1995, 15:14:55
$!
$! Execute by entering:		$ @conlab
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module conlab ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to conlab.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("conlab.imake") .nes. ""
$   then
$      vimake conlab
$      purge conlab.bld
$   else
$      if F$SEARCH("conlab.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake conlab
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @conlab.bld "STD"
$   else
$      @conlab.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create conlab.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack conlab.com -
	-i conlab.imake -
	-p conlab.pdf -
	-t tstconlab.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create conlab.imake
#define  PROCEDURE conlab
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create conlab.pdf
procedure help=*

parm	input		type=string	count=1
parm	output		type=string	count=1
parm	min_len		type=integer	count=(0:1)	default=100
parm	font_height	type=integer	count=(0:1)	default=5
parm	font_type	type=integer	count=(0:1)	default=1

local	cornr_nl	type=integer
local	cornr_ns	type=integer
local	end_lupi	type=integer
local	end_lupj	type=integer
local	file_nl		type=integer
local	file_ns		type=integer
local	line_ns		type=integer
local	lupi		type=integer	init=0
local	lupj		type=integer	init=0
local	lupj1		type=integer
local	lup_nl		type=integer	init=0
local	lup_ns		type=integer	init=0
local	str_line	type=integer	init=128
local	str_samp	type=integer	init=128
local	tmp_fm		type=integer
local	tmp_i		type=integer
local	tmp_j		type=integer

local	chk_nl		type=real
local	chk_ns		type=real
local	val_max		type=real
local	val_min		type=real

local	fil_form	type=string
local	fm_inp		type=string	count=(0:99)
local	fm_parm1	type=string
local	fm_parm2	type=string
local	fm_parm3	type=string
local	fm_parm4	type=string
local	in_file		type=string
local	out_file	type=string
local	tmp_str1	type=string
local	tmp_str2	type=string
local	tmp_str3	type=string
local	tmp_str4	type=string

refgbl $autousage
refgbl $echo
refgbl $aecho
refgbl $becho
refgbl $syschar

body

let $autousage = "none"
let $echo = "yes"
let $aecho = "yes"
let $becho = "yes"

let in_file = input
let out_file = output

!	find the NL and NS of the input image
!
lab2tcl +
  inp     = &in_file +
  keyword = ( format, nl, ns ) +
  v1      = fil_form +
  v2      = file_nl +
  v3      = file_ns +
            'all

!	subdivide the image into rectangles of &str_linex&str_samp
!
if ( &file_ns < 6145 )
  let str_samp = &str_samp
else-if ( &file_ns < 12289 )
  let str_samp = &str_samp * 2
else-if ( &file_ns < 18433 )
  let str_samp = &str_samp * 3
else
  let str_samp = &str_samp * 4
end-if

let end_lupi = &file_nl / &str_line
let end_lupj = &file_ns / &str_samp
let line_ns = end_lupj * &str_samp

!	if the image is not in byte, then convert it
!
if ( fil_form <> "BYTE" )
  maxmin +
    inp     = &in_file +
    minival = val_min +
    maxival = val_max

  c +
    inp = &in_file +
    out = tmp_file. +
    ira = ( &val_min, &val_max ) +
    ora = ( 0., 255. )

  let in_file = "tmp_file."
end-if

loop

  loop

!	this followed method is used to bypass the overflowed line ( >250 char )
!
    if ( lupj = 0 )
      let tmp_str1 = ""
      let tmp_str2 = ""
      let fm_parm2 = ""
      let tmp_str3 = ""
      let fm_parm3 = ""
      let tmp_str4 = ""
      let fm_parm4 = ""
    else
      if ( lupj < 12 )
        let tmp_str1 = "&fm_parm1" // " "
      else-if ( lupj < 24 )
        let tmp_str2 = "&fm_parm2" // " "
      else-if ( lupj < 36 )
        let tmp_str3 = "&fm_parm3" // " "
      else
        let tmp_str4 = "&fm_parm4" // " "
      end-if
    end-if

    let tmp_i = lupi * &str_line + 1
    let tmp_j = lupj * &str_samp + 1

!	begin the division of the input image
!
    copy +
      inp  = &in_file +
      out  = frac.cop +
      size = ( &tmp_i, &tmp_j, &str_line, &str_samp )

!	find the contour
!
    contour +
      inp     = frac.cop +
      out     = frac.con +
      zstart  = 0 +
      zend    = 255 +
      contint = 50 +
      numpts  = 5 +
      dim     = 3

!	generate the contour
!
    polyscrb +
      inp = frac.con +
      out = frac.pol +
      dim = 3 +
      nl  = &str_line +
      ns  = &str_samp

!	find the contour and generate an output file for the FONT program
!
    clabel +
      inp    = frac.con +
      minlen = &min_len +
      fontht = &font_height +
      font   = &font_type +
      trans  = ( .5, 1000. ) +
      img_nl = &str_line +
      img_ns = &str_samp

!	generate the font using the output of the clabel program
!
    font +
      inp   = frac.pol +
      out   = frac.tal&lupi&lupj +
      parms = grlabel.parm

    let tmp_fm = &lupj + 1
    let fm_inp( tmp_fm ) = "frac.tal&lupi&lupj"

!	this followed method is used to bypass the overflowed line (>250 char)
!
    let lupj1 = lupj + 1

    if ( lupj < 12 )
      let fm_parm1 = "&tmp_str1" // "OFF&lupj1=( 1, &tmp_j )"
    else-if ( lupj < 24 )
      let fm_parm2 = "&tmp_str2" // "OFF&lupj1=( 1, &tmp_j )"
    else-if ( lupj < 36 )
      let fm_parm3 = "&tmp_str3" // "OFF&lupj1=( 1, &tmp_j )"
    else
      let fm_parm4 = "&tmp_str4" // "OFF&lupj1=( 1, &tmp_j )"
    end-if

    let lupj = lupj + 1

    if ( lupj >= end_lupj )
      goto big_loop
    end-if

  end-loop

big_loop>

!	mosaic all the line-tiles into a combined image
!

  if ( &lupj < 12 )
    fastmos +
      inp  = &fm_inp +
      out  = frac0.doug +
      size = ( 1, 1, &str_line, &line_ns ) +
             &fm_parm1
  else-if ( &lupj < 24 )
    fastmos +
      inp  = &fm_inp +
      out  = frac0.doug +
      size = ( 1, 1, &str_line, &line_ns ) +
             &fm_parm1 +
             &fm_parm2
  else-if ( &lupj < 36 )
    fastmos +
      inp  = &fm_inp +
      out  = frac0.doug +
      size = ( 1, 1, &str_line, &line_ns ) +
             &fm_parm1 +
             &fm_parm2 +
             &fm_parm3
  else
    fastmos +
      inp  = &fm_inp +
      out  = frac0.doug +
      size = ( 1, 1, &str_line, &line_ns ) +
             &fm_parm1 +
             &fm_parm2 +
             &fm_parm3 +
             &fm_parm4
  end-if

  if ( lupi <> 0 )
    fastmos +
      inp  = ( frac.doug, frac0.doug ) +
      out  = fracl.doug +
      size = ( 1, 1, &file_nl, &line_ns ) +
      off1 = (      1, 1 ) +
      off2 = ( &tmp_i, 1 )

    if ($syschar(1) = "VAX_VMS")
       dcl ren fracl.doug frac.doug
       dcl pu frac.doug
       dcl del frac0.doug.
    else
       ush mv fracl.doug frac.doug
       ush rm frac0.doug
    end-if
  else
    if ($syschar(1) = "VAX_VMS")
       dcl ren frac0.doug frac.doug
    else
       ush mv frac0.doug frac.doug
    end-if
  end-if

  if ($syschar(1) = "VAX_VMS")
    dcl del frac.tal*.
  else
    ush rm frac.tal*
  end-if

  let lupj = 0

  let lupi = lupi + 1

  if ( lupi >= end_lupi )
    break
  end-if

end-loop

let cornr_nl = &file_nl - &end_lupi * &str_line

if ( cornr_nl > 0 )

  let tmp_i = &end_lupi * &str_line + 1

  loop

    let tmp_j = 1 + &lup_ns * &str_samp

    copy +
      inp  = &in_file +
      out  = frac.cop +
      size = ( &tmp_i, &tmp_j, &cornr_nl, &str_samp )

    contour +
      inp     = frac.cop +
      out     = frac.con +
      zstart  = 0 +
      zend    = 255 +
      contint = 50 +
      numpts  = 5 +
      dim     = 3

    polyscrb +
      inp = frac.con +
      out = frac.pol +
      dim = 3 +
      nl  = &cornr_nl +
      ns  = &str_samp

    clabel +
      inp    = frac.con +
      minlen = &min_len +
      fontht = &font_height +
      font   = &font_type +
      trans  = ( .5, 1000. ) +
      img_nl = &cornr_nl +
      img_ns = &str_samp

    font +
      inp   = frac.pol +
      out   = frac1.doug +
      parms = grlabel.parm

    fastmos +
      inp  = ( frac.doug, frac1.doug ) +
      out  = frac.tal +
      size = ( 1, 1, &file_nl, &file_ns ) +
      off1 = (      1,      1 ) +
      off2 = ( &tmp_i, &tmp_j )

    if ($syschar(1) = "VAX_VMS")
       dcl ren frac.tal frac.doug
       dcl pu frac.doug
    else
       ush mv frac.tal frac.doug
    end-if

    let lup_ns = &lup_ns + 1

    if ( lup_ns = &end_lupj )
      break
    end-if
  end-loop
end-if

let cornr_ns = &file_ns - &end_lupj * &str_samp

if ( cornr_ns > 0)

  let tmp_j = &end_lupj * &str_samp + 1

  loop

    let tmp_i = 1 + &lup_nl * &str_line

    copy +
      inp  = &in_file +
      out  = frac.cop +
      size = ( &tmp_i, &tmp_j, &str_line, &cornr_ns )

    contour +
      inp     = frac.cop +
      out     = frac.con +
      zstart  = 0 +
      zend    = 255 +
      contint = 50 +
      numpts  = 5 +
      dim     = 3

    polyscrb +
      inp = frac.con +
      out = frac.pol +
      dim = 3 +
      nl  = &str_line +
      ns  = &cornr_nl

    clabel +
      inp    = frac.con +
      minlen = &min_len +
      fontht = &font_height +
      font   = &font_type +
      trans  = ( .5, 1000. ) +
      img_nl = &str_line +
      img_ns = &cornr_nl

    font +
      inp   = frac.pol +
      out   = frac2.doug +
      parms = grlabel.parm

    fastmos +
      inp  = ( frac.doug, frac2.doug ) +
      out  = frac.tal +
      size = ( 1, 1, &file_nl, &file_ns ) +
      off1 = (      1,      1 ) +
      off2 = ( &tmp_i, &tmp_j )

    if ($syschar(1) = "VAX_VMS")
       dcl ren frac.tal frac.doug
       dcl pu frac.doug
    else
       ush mv frac.tal frac.doug
    end-if

    let lup_nl = &lup_nl + 1

    if ( lup_nl = &end_lupi )
      break
    end-if
  end-loop
end-if

if (( cornr_nl > 0 ) and ( cornr_ns > 0 ))

  let tmp_i = &end_lupi * &str_line + 1
  let tmp_j = &end_lupj * &str_samp + 1

  copy +
    inp  = &in_file +
    out  = frac.cop +
    size = ( &tmp_i, &tmp_j, &cornr_nl, &cornr_ns )

  contour +
    inp     = frac.cop +
    out     = frac.con +
    zstart  = 0 +
    zend    = 255 +
    contint = 50 +
    numpts  = 5 +
    dim     = 3

  polyscrb +
    inp = frac.con +
    out = frac.pol +
    dim = 3 +
    nl  = &str_line +
    ns  = &cornr_nl

  clabel +
    inp    = frac.con +
    minlen = &min_len +
    fontht = &font_height +
    font   = &font_type +
    trans  = ( .5, 1000. ) +
    img_nl = &str_line +
    img_ns = &cornr_nl

  font +
    inp   = frac.pol +
    out   = frac3.doug +
    parms = grlabel.parm

    fastmos +
      inp  = ( frac.doug, frac3.doug ) +
      out  = frac.tal +
      size = ( 1, 1, &file_nl, &file_ns ) +
      off1 = (    1,    1 ) +
      off2 = ( &tmp_i, &tmp_j )

    if ($syschar(1) = "VAX_VMS")
       dcl ren frac.tal frac.doug
       dcl pu frac.doug
    else
       ush mv frac.tal frac.doug
    end-if

end-if

if ($syschar(1) = "VAX_VMS")
   dcl ren frac.doug &out_file
else
   ush mv frac.doug &out_file
end-if

if ($syschar(1) = "VAX_VMS")
   dcl del frac.con.,frac.cop.,frac.pol.,grlabel.parm.
else
   ush rm frac.con
   ush rm frac.cop
   ush rm frac.pol
   ush rm grlabel.parm
end-if

let $echo = "no"
let $aecho = "no"
let $becho = "no"

end-proc

.help
purpose:

CONLAB is a stream used to speed-up the contour label program written
by Mike Girard.  The stream will cut up an input image into strips of
str_line by str_samp pixels.  These strips will be feeded into CLABEL
and font to generate the contour labels.

.page
COGNIZANT PROGRAMMER: nghia
REVISIONS:
                8-MAY-95  AMS (CRI) MSTP S/W Conversion  Made portable for UNIX
.level1
.vari inp
The input image where the contours will
be labeled.
.vari output
The name of the output contoured labels
of the input image.
.vari min_len
Minimum contour length to be labeled.
.vari font_height
Height in pixels of font.
.vari font_type
Integer representing font type.

.level2
.vari inp
The input image where the contours will
be labeled.
.vari output
The name of the output contoured labels
of the input image.  This output will
only have the contoured labels but not
the input image.
.vari min_len
A contour in the input contour file with length less than 'minlen'
will not be labeled.
.vari font_height
The height of the text in the label in pixels.
.vari font_type
The type of font to use in the labels. The font type is
taken from those available in the program FONT.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstconlab.pdf
procedure

refgbl $autousage
refgbl $echo
refgbl $aecho
refgbl $becho

body

let $autousage = "none"
let $echo  = "yes"
let $aecho = "yes"
let $becho = "yes"

!	generate the input image
!
gen +
   out = frac.img +
   nl  = 512 +
   ns  = 512

!	generate the contours and their annotation
!
conlab +
   inp = frac.img +
   out = doug.las

let $echo  = "no"
let $aecho = "no"
let $becho = "no"

end-proc
$ Return
$!#############################################################################
