$!****************************************************************************
$!
$! Build proc for MIPL module tp
$! VPACK Version 1.9, Friday, June 04, 2010, 14:01:57
$!
$! Execute by entering:		$ @tp
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
$ write sys$output "*** module tp ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tp.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("tp.imake") .nes. ""
$   then
$      vimake tp
$      purge tp.bld
$   else
$      if F$SEARCH("tp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tp.bld "STD"
$   else
$      @tp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp.com -mixed -
	-s TpApp.cc -
	-i tp.imake -
	-p tp.pdf Tp.xres TpDisplayOne.xpm TpDisplayThree.xpm TpDisplayTwo.xpm -
	   TpLock.xpm TpUnlock.xpm TpPointSize10.xbm TpPointSize15.xbm -
	   TpPointSize20.xbm TpPointSize25.xbm TpPointSize30.xbm -
	   TpPointSize5.xbm TpPointSymbolCross45.xbm -
	   TpPointSymbolCrossWithDot.xbm TpPointSymbolCrossWithHole45.xbm -
	   TpPointSymbolDot.xbm TpPointSymbolRect.xbm -
	   TpPointSymbolRectWithCrossesWithDot.xbm TpDecZoom.xbm -
	   TpIncZoom.xbm TpDeletePoint.xpm TpKeepPoint.xpm TpListPoints.xpm -
	   TpShiftLeft.xpm TpShiftRight.xpm TpKeepPointI.xpm
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpApp.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "TpApplication.h"
#include "TpWindow.h"

Application *app = new TpApplication("Tp");
MainWindow *window = new TpWindow("TpWindow");

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tp.imake
#define PROGRAM tp
#define MODULE_LIST TpApp.cc

#define R2LIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_P2SUB
#define LIB_P1SUB
#define LIB_GUISUB
#define LIB_PDS
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_XMU
#define LIB_XPM
#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create tp.pdf
procedure help=*
parm inp string count=(0:12) default=--
parm tpfile string default=""
parm options string default=""
refgbl $syschar
body
local f string
local files string
let files = ""
if ($count(inp) >= 1)
   let f = inp(1)
   let files = "&f"
end-if
if ($count(inp) >= 2)
   let f = inp(2)
   let files = "&files &f"
end-if
if ($count(inp) >= 3)
   let f = inp(3)
   let files = "&files &f"
end-if

local tpf string
local tpfile_arg string
let tpfile_arg = ""
if ($count(tpfile) >= 1)
   let tpf = tpfile(1)
   let tpfile_arg = "-pfile &tpf"
end-if

if ($syschar(1) = "UNIX")
   ush $R2LIB/tp &options &tpfile_arg &files &
else
   dcl spawn/nowait tp &options &tpfile_arg &files
end-if
end-proc
.title
VICAR program tp
.help
The tp program is an X Windows tiepoint editing program.  It is intended as 
the replacement for a number of VIDS/VRDI-base programs, most notably PICREG.

Complete documentation for tp is available on-line at:
<http://www-mipl.jpl.nasa.gov/tp/>
or via clicking on the various Help options in the program.

tp is an interactive, X Windows program.  Before using it, you must have
set up the X display connection.  On a Unix machine, this is typically done
via "setenv DISPLAY" while on VMS it is typically done via "set disp/create".
Consult system documentation for details on establishing an X connection, or
refer to the on-line tutorial in Mosaic at
<http://www-mipl.jpl.nasa.gov/wstutor/set_x_display.html>
Please note that the X display must be set up outside of the VICAR environment.
.page
Running the program:

The tp program may be run either from within VICAR, or directly from
the shell or DCL.  In order to run it from the shell (in Unix), make sure
"$R2LIB" is in your path, or specify it explicitly:

	% $R2LIB/tp image.red image.grn image.blu -pfile tpfile.ibis

From VMS, "tp" is automatically defined for you:

	$ tp image.red image.grn image.blu -pfile tpfile.ibis
.page
From VICAR, usage conforms to standard VICAR input file conventions:

	VICAR> tp (image.red,image.grn,image.blu)

tp can handle 0 to 12 image filenames

Command-line options may be given to the program via the "options" parameter;
see the help for that parameter for details.

.page
ADDITIONAL FEATURES

In June 2010 several new features were added by rgd.  There was not time to
find or update the online help, so the new features are documented here.

1) Up to 6 windows can be displayed instead of just 3.  This is good for
dual-monitor workstations (move the window around so the split between
monitors falls between images).  No pretty icons for 4-6, just text.
There's no reason the limit couldn't be raised beyond 6, if necessary.

2) The image list in the upper right corner is now sized based on the number
of images loaded (so you won't see all 12 all the time, you'll see however
many are loaded).  However, the number shown never decreases (so if you delete
an image, the slot will still be shown).  If you load lots of images, you may
have to make the window bigger.

3) Ability to load up to 25 images instead of just 12.  This was enabled by
#2, so screen real estate is not used unless it is needed.  25 is a fairly
arbitrary number and can easily be changed (TP_MAX_IMAGES in TpDefs.h, along
with some minor changes to the resource file).

4) A "Go-to tiepoint" feature allows you to type in a general qualifier value
and select the first tiepoint that has the same qualifier.  It works similarly
to the tiepoint number field; just key in the qual value, hit return, and the
appropriate tiepoint will be selected.  If there is no match, the field will
change to "UNK" (for Unknown).  This feature has only been tested with one
general qualifier; it should behave like the qualifier text field.  Note that
the orginal qualifier text field is still there; typing in it still allows
you to change the qualifier value.

NOTE: Feature #4 must be explicitly enabled.  To do this, turn on the enable
flag:

*enableGotoQual: True

This can be done in the resource file, in a local config file, or on the
command line (via:   -xrm "*enableGotoQual:True" ).  There is no interactive
mechanism to enable this feature.

.level1
.vari inp
Input file(s)
.vari tpfile
Tiepoint file (IBIS format)
.vari options
Command-line options
.level2
.vari inp
Zero to twelve input files.  

.vari options
Various command-line options can be given to the program.  Typical options
might be a -display to redirect the display somewhere, or a -xrm to set a
resource.  See the on-line help for details.

One trouble spot with TAE is when you need to quote strings.  For example,
a -xrm specification has to be of the form:  -xrm "resource: value"  where the
double quotes are actually present in the string submitted to DCL or the
shell.  Specifying such resources is unfortunately platform-dependent.
Unix will accept single quotes (usually), while VMS requires double quotes.
You can double the quotes for VMS, however, this doesn't work on Unix.
For example:

Unix:
	tp /usr/local/images/io.red opt="-xrm '*background: grey'"

VMS:
	tp images:io.red opt="-xrm ""*background: grey"""
.end
$!-----------------------------------------------------------------------------
$ create Tp.xres
!******************************************************************************
! Tp Resources - CONFIGURABLE BY THE USER
!******************************************************************************
*Tp.title: 			Tiepoint Display

*TpWindow.iconName:		TP

Tp*background: 			cadet blue
Tp*form1*background:		khaki3
Tp*form2*background:		turquoise3
Tp*form3*background:            khaki3
Tp*form4*background:            turquoise3
Tp*form5*background:            khaki3
Tp*form6*background:            turquoise3
Tp*form7*background:            khaki3
Tp*form8*background:            turquoise3
Tp*form9*background:            khaki3
Tp*form10*background:           turquoise3
Tp*form11*background:           khaki3
Tp*form12*background:           turquoise3
Tp*form13*background:		khaki3
Tp*form14*background:		turquoise3
Tp*form15*background:		khaki3
Tp*form16*background:		turquoise3
Tp*form17*background:		khaki3
Tp*form18*background:		turquoise3
Tp*form19*background:		khaki3
Tp*form20*background:		turquoise3
Tp*form21*background:		khaki3
Tp*form22*background:		turquoise3
Tp*form23*background:		khaki3
Tp*form24*background:		turquoise3
Tp*form25*background:		khaki3
Tp*form26*background:		turquoise3

Tp*foreground: 			black

*TpWindow*traversalOn:	False
*tearOffModel: 			TEAR_OFF_ENABLED
*enablePrintWidgetTree: 	True

*fontList: 	-*-new century schoolbook-medium-r-normal--14-*-75-*-*-*-*-*

#######################
# Main image view component
#######################
*XvicImage.enableDirectColor:	False
*XvicImage.stretchPolicy:       USE_SW
*XvicImage.colormapPolicy:      ALLOC
*XvicImage.bwColormapPolicy:    ALLOC
*XvicImage.dither:              ORDERED
*XvicImage.bwDither:            ORDERED
*XvicImage.bwVisualType:        use_24bit
*XvicImage.pseudoVisualType:	use_24bit
*XvicImage.redLevels: 		4
*XvicImage.greenLevels: 	4
*XvicImage.blueLevels: 		4

# Turn off hardware overlay by default.  There seems to be some interaction
# when multiple windows use it at the same time, where the colormaps get
# weird... and tp has a LOT of image windows!

*enableHWOverlay:		false

# Translations for main image window
*tpDisplayer*XvicImage.translations: #override \n\
	~Shift<Btn1Down>:		Input(tp, select) \n\
	<Btn1Down>,<Btn1Up>,<Btn1Down>: Input(tp, selectPoint) \n\
	<Btn2Down>:			Input(tp, new) \n\
	<Btn2Motion>:			Input(tp, drag) \n\
	<Btn3Down>:			Input(tp, scrollAll) \n\
	Shift<Btn1Down>:		MousePanStart() \n\
	Shift<Btn1Motion>:		MousePan() \n\
        ~Shift~Ctrl~Meta<Key>osfLeft:   Input(tp, left) \n\
        ~Shift~Ctrl~Meta<Key>osfRight:  Input(tp, right) \n\
        ~Shift~Ctrl~Meta<Key>osfUp:     Input(tp, up) \n\
        ~Shift~Ctrl~Meta<Key>osfDown:   Input(tp, down) \n\
        Shift~Ctrl~Meta<Key>osfLeft:    PanHalfView(left) \n\
        Shift~Ctrl~Meta<Key>osfRight:   PanHalfView(right) \n\
        Shift~Ctrl~Meta<Key>osfUp:      PanHalfView(up) \n\
        Shift~Ctrl~Meta<Key>osfDown:    PanHalfView(down) \n\
        Ctrl~Shift~Meta<Key>osfLeft:    PanEdge(left) \n\
        Ctrl~Shift~Meta<Key>osfRight:   PanEdge(right) \n\
        Ctrl~Shift~Meta<Key>osfUp:      PanEdge(up) \n\
        Ctrl~Shift~Meta<Key>osfDown:    PanEdge(down) \n\
        <Key>osfEscape:                 CursorMode(toggle) \n\
        ~Shift<Key>grave:               CursorMode(toggle) \n\
        <Key>asciitilde:                CursorMode(toggle,true) \n\
        Shift<Key>grave:                CursorMode(toggle,true) \n\
        <Key>plus:                      CursorMode(floating) \n\
        <Key>minus:                     CursorMode(planted) \n\
        Shift<Motion>:                  MoveCursorMouse() \n\
        Shift Ctrl~Meta<Key>osfLeft:    MoveCursor(left) \n\
        Shift Ctrl~Meta<Key>osfRight:   MoveCursor(right) \n\
        Shift Ctrl~Meta<Key>osfUp:      MoveCursor(up) \n\
        Shift Ctrl~Meta<Key>osfDown:    MoveCursor(down) \n\
        Meta~Shift~Ctrl<Key>osfLeft:    MoveCursorScreen(left) \n\
        Meta~Shift~Ctrl<Key>osfRight:   MoveCursorScreen(right) \n\
        Meta~Shift~Ctrl<Key>osfUp:      MoveCursorScreen(up) \n\
        Meta~Shift~Ctrl<Key>osfDown:    MoveCursorScreen(down) \n\
	<Key>i:				PanOne(up) \n\
	<Key>k:				PanOne(down) \n\
	<Key>j:				PanOne(left) \n\
	<Key>l:				PanOne(right)

*imageView.width: 		350
*imageView.height: 		350

#######################
# Browse Control
#######################
*matchBrowseControl.topOffset: 6
*matchBrowseControl*XmTextField.marginHeight: 3
*mainWindow.TpWindow.matchBrowseControl*traversalOn: True

*enableGotoQual: False
*matchBrowseControl*gotoLabel.labelString: Go:
*matchBrowseControl*gotoField.columns: 5

#######################
# Button Panel
#######################
*buttonPanel.packing: 			PACK_COLUMN
*buttonPanel.numColumns:		1
*TpWindow.mainWindow.TpWindow.buttonPanel.orientation: 		HORIZONTAL

*buttonPanel.Save Point.labelType: 	PIXMAP
*buttonPanel.Delete Point.labelType:	PIXMAP
*buttonPanel.List Points.labelType:	PIXMAP
*buttonPanel.Shift Left.labelType:	PIXMAP
*buttonPanel.Shift Right.labelType:	PIXMAP

*buttonPanel.Save Point.labelPixmap:    TpKeepPoint.xpm
*buttonPanel.Save Point.labelInsensitivePixmap: TpKeepPointI.xpm
*buttonPanel.Delete Point.labelPixmap:  TpDeletePoint.xpm
*buttonPanel.List Points.labelPixmap:   TpListPoints.xpm
*buttonPanel.Shift Left.labelPixmap:	TpShiftLeft.xpm
*buttonPanel.Shift Right.labelPixmap:	TpShiftRight.xpm

*option_pane*affine.labelString: 	Autofind
*option_pane*manual.labelString: 	Manual Find

*option_pane*AutoCorr.labelString: 	Autocorrelate
*option_pane*AffineOnly.labelString: 	No Correlation

*option_pane*affine.fontList: 		8x13
*option_pane*manual.fontList: 		8x13
 
*option_pane*AutoCorr.fontList: 	8x13
*option_pane*AffineOnly.fontList: 	8x13

!*buttonPanel.Save Point.labelString:    Keep Point
!*buttonPanel.Delete Point.labelString:  Delete Point
!*buttonPanel.List Point.labelString:    List Points
!*buttonPanel.Shift Left.labelString: 	<<
!*buttonPanel.Shift Right.labelString: 	>>

#######################
# Image Reference
#######################
*imageReference.topOffset: 6

#######################
# Point Information Component
#######################
*posView*XmTextField.columns: 		7
*posView*XmFrame.shadowThickness: 	0
*posView.linePosition*labelString:	L:
*posView.sampPosition*labelString:      S:
*posView.qualPosition*labelString:      Q:

#######################
# Image info component
#######################
*imageInfo.orientation:			Horizontal

#######################
# Main image zoom component
#######################
*mainImageZoomControl1*zoomValue:  1
*mainImageZoomControl2*zoomValue:  1
*mainImageZoomControl3*zoomValue:  1
*mainImageZoomControl4*zoomValue:  1
*mainImageZoomControl5*zoomValue:  1
*mainImageZoomControl6*zoomValue:  1
*mainImageZoomControl7*zoomValue:  1
*mainImageZoomControl8*zoomValue:  1
*mainImageZoomControl9*zoomValue:  1
*mainImageZoomControl10*zoomValue: 1
*mainImageZoomControl11*zoomValue: 1
*mainImageZoomControl12*zoomValue: 1

#######################
# Zoom component
#######################
*zoomValue:			2
*zoomView.width: 		100
*zoomView.height: 		10
!*zoomControl.leftOffset:	10
*decZoom.labelString: 		-
*incZoom.labelString: 		+
*zoomLabel.labelString: 	Zoom:
*keyinZoom.columns: 		3 
*keyinZoom.marginHeight:	4
!*incZoom.labelType: PIXMAP
!*decZoom.labelType: PIXMAP
!*incZoom.labelPixmap: inc_zoom.xbm
!*decZoom.labelPixmap: dec_zoom.xbm

#######################
# Contrast component
#######################
*minContrastValue:				0
*maxContrastValue:				255
*contrastFrameLabel.labelString: 		Contrast
*keyinLowContrast.columns: 			4
*keyinHighContrast.columns: 			4
*contrastControl1.shadowThickness:		3
*contrastControl2.shadowThickness:		3
*contrastControl3.shadowThickness:		3

#######################
# Zoom component
#######################
*zoomValue:                     2
*zoomView.width:                100
*zoomView.height:               10
*decZoom.labelString:           -
*incZoom.labelString:           +
*zoomLabel.labelString:         Zoom:
*keyinZoom.columns:             3
*keyinZoom.marginHeight:        4
*tpDisplayer*panZoomForm*XvicImage.translations: #override \n\
	~Shift<Btn1Down>:               Input(tp, select) \n\
        <Btn1Down>,<Btn1Up>,<Btn1Down>: Input(tp, selectPoint) \n\
        <Btn2Down>:                     Input(tp, new_zoom) \n\
        <Btn2Motion>:                   Input(tp, drag_zoom) \n\
	~Shift~Ctrl~Meta<Key>osfLeft:   Input(tp, left) \n\
        ~Shift~Ctrl~Meta<Key>osfRight:  Input(tp, right) \n\
        ~Shift~Ctrl~Meta<Key>osfUp:     Input(tp, up) \n\
        ~Shift~Ctrl~Meta<Key>osfDown:   Input(tp, down)

#######################
# Pan view component
#######################
*panViewSW.bottomOffset: 	40
*tpDisplayer*panZoomForm.panViewSW.panView*translations: #override \n\
	~Shift<Btn1Down>:               Input(pan_mouse, start) \n\
        ~Shift<Btn1Motion>:             Input(pan_mouse, drag) \n\
	~Shift<Btn3Down>:		Input(pan_mouse, move) \n\
        ~Shift~Ctrl~Meta<Key>osfLeft:   Input(pan_one, left) \n\
        ~Shift~Ctrl~Meta<Key>osfRight:  Input(pan_one, right) \n\
        ~Shift~Ctrl~Meta<Key>osfUp:     Input(pan_one, up) \n\
        ~Shift~Ctrl~Meta<Key>osfDown:   Input(pan_one, down) \n\
        Ctrl~Shift~Meta<Key>osfLeft:    Input(pan_edge, left) \n\
        Ctrl~Shift~Meta<Key>osfRight:   Input(pan_edge, right) \n\
        Ctrl~Shift~Meta<Key>osfUp:      Input(pan_edge, up) \n\
        Ctrl~Shift~Meta<Key>osfDown:    Input(pan_edge, down) \n\
        Shift~Ctrl~Meta<Key>osfLeft:    Input(pan_half_view, left) \n\
        Shift~Ctrl~Meta<Key>osfRight:   Input(pan_half_view, right) \n\
        Shift~Ctrl~Meta<Key>osfUp:      Input(pan_half_view, up) \n\
        Shift~Ctrl~Meta<Key>osfDown:    Input(pan_half_view, down)

#######################
# Display mode settings
#######################
*displayModeLabel.labelString:			Display Mode
*DisplayModeDialog*title: 			Display Mode Settings

*DisplayModeDialog*XmFrame.shadowThickness: 	3
*displayFrameLabel.labelString: 		Number of Images Displayed
*rotationFrameLabel.labelString: 		Image Rotation
*swapFrameLabel.labelString:			Lock Image from Shifting

*rotOptMenu0.labelString: 	Image 1
*rotOptMenu1.labelString: 	Image 2
*rotOptMenu2.labelString: 	Image 3
*rotOptMenu3.labelString: 	Image 4
*rotOptMenu4.labelString: 	Image 5
*rotOptMenu5.labelString: 	Image 6
*rotOptMenu6.labelString: 	Image 7
*rotOptMenu7.labelString: 	Image 8
*rotOptMenu8.labelString: 	Image 9
*rotOptMenu9.labelString: 	Image 10
*rotOptMenu10.labelString: 	Image 11
*rotOptMenu11.labelString: 	Image 12
*rotOptMenu12.labelString: 	Image 13
*rotOptMenu13.labelString: 	Image 14
*rotOptMenu14.labelString: 	Image 15
*rotOptMenu15.labelString: 	Image 16
*rotOptMenu16.labelString: 	Image 17
*rotOptMenu17.labelString: 	Image 18
*rotOptMenu18.labelString: 	Image 19
*rotOptMenu19.labelString: 	Image 20
*rotOptMenu20.labelString: 	Image 21
*rotOptMenu21.labelString: 	Image 22
*rotOptMenu22.labelString: 	Image 23
*rotOptMenu23.labelString: 	Image 24
*rotOptMenu24.labelString: 	Image 25
*rotationRC.numColumns: 	4
*rotationRC.packing:		PACK_COLUMN
*rotationRC.orientation: 	VERTICAL

*displayTypes.packing:		PACK_COLUMN
*displayTypes.numColumns: 	1
*displayTypes.orientation: 	HORIZONTAL

*displayTypes*num1.labelType: 	PIXMAP
*displayTypes*num2.labelType: 	PIXMAP
*displayTypes*num3.labelType: 	PIXMAP
*displayTypes*num4.labelType: 	STRING
*displayTypes*num5.labelType: 	STRING
*displayTypes*num6.labelType: 	STRING
*displayTypes*num1.labelPixmap: 		TpDisplayOne.xpm
*displayTypes*num2.labelPixmap: 		TpDisplayTwo.xpm
*displayTypes*num3.labelPixmap: 		TpDisplayThree.xpm
*displayTypes*num4.labelString: 		Four
*displayTypes*num5.labelString: 		Five
*displayTypes*num6.labelString: 		Six

*swapRC.orientation:		HORIZONTAL
*swapRC.numColumns:		1
*swapRC.packing:		PACK_COLUMN
*swapRC*labelType:		PIXMAP
*swapRC*labelPixmap:		TpUnlock.xpm
*swapRC*selectPixmap:		TpLock.xpm
*swapRC*labelString:		Lock

###################
# Autofind mode settings
###################
*AutofindDialog*traversalOn: 			True
*AutofindDialog*title: 				Autofind Mode Parameters
*AutofindDialog*autofindLabel.labelString:	Autofind Mode
*AutofindDialog*affine.labelString:		Use Affine Transformation
*AutofindDialog*none.labelString:		None (Use Manual Mode)
*autofindMode:					AFFINE

###################
# Remove Image Dialog
###################
*RmImageDialog*traversalOn: 		True
*RmImageDialog*title: 			Remove Image from TP
*RmImageDialog*label.labelString: 	Enter Image Number and Press [Return]:
*RmImageDialog*nameView*field.columns: 	2

###################
# Lock label widget
###################
*lock.labelType:		PIXMAP
*lock.labelString:		Locked
*lock.labelPixmap:		TpLock.xpm

###################
# IBIS Point file selection dialog
###################
*SavePointFileAs.pattern:	*.tp
*LoadPointFile.pattern:		*.tp
*saveAs.pattern:		*.tp

###################
# Pulldown menus strings
###################
*Status.labelString:		Special Exit Status

###################
# Help file locations
###################
*helpBrowserCommand: netscape
*helpBaseLocation: http://www-mipl.jpl.nasa.gov/tp/

! default
*helpLocation: tp.html

###################
# cursor symbols
###################
*CursorSymbolDialog*TpCursorEditorOptsLabel.labelString: Cursor Symbols:
*CursorSymbolDialog*labelColor.labelString: Color:
*CursorSymbolDialog*XmTextField.marginHeight:   4

!*crosshair.labelType:			PIXMAP
!*crosshair.labelPixmap:			cross.xbm
!*center_ptr.labelType:                         PIXMAP
!*center_ptr.labelPixmap:                       XC_center_ptr
!*cross.labelType:                       PIXMAP
!*cross.labelPixmap:                     XC_cross
!*dot.labelType:                       PIXMAP
!*dot.labelPixmap:                     XC_dot
!*dotbox.labelType:                      PIXMAP
!*dotbox.labelPixmap:                    XC_dotbox
!*tcross.labelType:                       PIXMAP
!*tcross.labelPixmap:                     XC_tcross

###################
# point symbols
###################
*pointSymbolWidth:      15
*pointSymbolHeight:     15
! Choose shape among Rectangle, CrossWithDot, CrossWithHole45
*pointSymbolShape:      CrossWithDot
*clickPrecision:        4

*TpPointEditorOptsLabel.labelString:	Point Symbols:

*CrossWithDot.labelType:  		PIXMAP
*CrossWithDot.labelPixmap:	     	TpPointSymbolCrossWithDot.xbm
*Rectangle.labelType:			PIXMAP
*Rectangle.labelPixmap:		     	TpPointSymbolRect.xbm
*Dot.labelType:				PIXMAP
*Dot.labelPixmap:		     	TpPointSymbolDot.xbm
*Cross45.labelType:			PIXMAP
*Cross45.labelPixmap:		     	TpPointSymbolCross45.xbm
*CrossWithHole45.labelType:		PIXMAP
*CrossWithHole45.labelPixmap:        	TpPointSymbolCrossWithHole45.xbm
*RectWithCrossesWithDot.labelType:	PIXMAP
*RectWithCrossesWithDot.labelPixmap: 	TpPointSymbolRectWithCrossesWithDot.xbm
*symbol.labelString:		     	Point Symbol:

*Size5.labelType:			PIXMAP
*Size5.labelPixmap:  			TpPointSize5.xbm
*Size10.labelType:			PIXMAP
*Size10.labelPixmap: 			TpPointSize10.xbm
*Size15.labelType:			PIXMAP
*Size15.labelPixmap: 			TpPointSize15.xbm
*Size20.labelType:			PIXMAP
*Size20.labelPixmap: 			TpPointSize20.xbm
*Size25.labelType:			PIXMAP
*Size25.labelPixmap: 			TpPointSize25.xbm
*Size30.labelType:			PIXMAP
*Size30.labelPixmap: 			TpPointSize30.xbm
*size.labelString: 			Point Size:

*PointSymbolsDialog*labelColor.labelString: 	Color of Unselected Point:
*PointSymbolsDialog*labelColorSel.labelString:	Color of Selected Point:
*PointSymbolsDialog*XmTextField.marginHeight:   4

###################
# Tags dialog
###################
*PointTagsDialog*traversalOn: 			True
*PointTagsDialog*title: 			Point Tags
*PointTagsDialog*TpPointTagLabel.labelString: 	Point Tag Position:
*PointTagsDialog*NorthEast.labelString: 	North-East
*PointTagsDialog*NorthWest.labelString: 	North-West
*PointTagsDialog*SouthEast.labelString: 	South-East
*PointTagsDialog*SouthWest.labelString: 	South-West
*PointTagsDialog*Center.labelString: 		Center
 
###################
# Qualifier dialog
###################
*QualFormatDialog*traversalOn: 				True
*QualFormatDialog*title: 				Qualifiers Format
*QualFormatDialog.width: 				650
*QualFormatDialog.height: 				500
*QualFormatDialog*XmFrame.shadowThickness: 		3
*QualFormatDialog*numQualsView.XmFrame.shadowThickness: 0
*QualFormatDialog*genQualFormatCmd*qualFormatFrameLabel.labelString: General Qualifiers
*QualFormatDialog*pntQualFormatCmd*qualFormatFrameLabel.labelString: Point Qualifiers
*QualFormatDialog*qualFormatLabel.labelString: 		Qualifier Format:
*QualFormatDialog*numQualsView.label.labelString: 	Number of qualifiers:
*QualFormatDialog*titleName.labelString: 		Name
*QualFormatDialog*titleType.labelString: 		Type
*QualFormatDialog*titleUnit.labelString: 		Unit
*QualFormatDialog*qualFormatRadioBox*real.labelString: 	REAL
*QualFormatDialog*qualFormatRadioBox*full.labelString: 	INT
*QualFormatDialog*qualFormatRadioBox*text.labelString: 	ASCII
*QualFormatDialog*rcGenQualExtra*genQualFormatCmd.labelString:	Set First Gen Qual to ID
*QualFormatDialog*rcGenQualExtra*offsetLabel.labelString: Start At:
*QualFormatDialog*rcGenQualExtra*nextLabel.labelString: Set Next ID to:
*QualFormatDialog*rcGenQualExtra*setMatchIdOffset.columns: 2
*QualFormatDialog*rcGenQualExtra*setMatchIdNext.columns: 2

###################
# Match Mode dialog
###################
*MatchModeDialog*traversalOn: True
*MatchModeDialog*title: Match Mode Parameters
*MatchModeDialog*matchModeLabel.labelString: Match Mode
*MatchModeDialog*AssistedCorr.labelString: Assisted Correlation
*MatchModeDialog*AutoCorr.labelString: Automatic Correlation
*MatchModeDialog*RedoCorr.labelString: Redo All Entries
*MatchModeDialog*AffineOnly.labelString: Use Affine Transformation Only

*MatchModeDialog*matchModeValuesFrameLabel.labelString: Match Mode Parameters
*MatchModeDialog*pmk.label.labelString: Patch Size PMK (1-20):
*MatchModeDialog*lsm.label.labelString: Patch Size LSM (5-31):
*MatchModeDialog*sw.label.labelString: Search Window (9-51):
*MatchModeDialog*accuracy.label.labelString: Accuracy (0.07-1.0):
*MatchModeDialog*threshold.label.labelString: Correlation Treshold (0.0-1.0):

###################
# Autofind Results dialog
###################
*AutofindResultsDialog*title: Autofind Results
*AutofindResultsDialog*XmTextField.cursorPositionVisible: 	False
*AutofindResultsDialog*XmTextField.traversalOn: 		False

*AutofindResultsDialog*autofindResultsLabel.labelString: Affine Transformation Parameters
*AutofindResultsDialog*affPar0.label.labelString: 0:
*AutofindResultsDialog*affPar1.label.labelString: 1:
*AutofindResultsDialog*affPar2.label.labelString: 2:
*AutofindResultsDialog*affPar3.label.labelString: 3:
*AutofindResultsDialog*affPar4.label.labelString: 4:
*AutofindResultsDialog*affPar5.label.labelString: 5:
*AutofindResultsDialog*autofindResults*field.editable: False
*AutofindResultsDialog*autofindResults*frame.shadowThickness: 0

###################
# Match Mode Results dialog
###################
*MatchModeResultsDialog*title: Match Mode Results
*MatchModeResultsDialog*XmTextField.cursorPositionVisible:     False
*MatchModeResultsDialog*XmTextField.traversalOn:               False
*MatchModeResultsDialog*matchModeResultsLabel.labelString: Correlation Results
*MatchModeResultsDialog*erg0.label.labelString: Line Pixel Displacement:
*MatchModeResultsDialog*erg1.label.labelString: Sample Pixel Displacement:
*MatchModeResultsDialog*erg2.label.labelString: Before LSM-fitting:
*MatchModeResultsDialog*erg3.label.labelString: After LSM-fitting:
*MatchModeResultsDialog*erg4.label.labelString: Point Accuracy:
*MatchModeResultsDialog*matchModeResults*field.editable: False
*MatchModeResultsDialog*matchModeResults*frame.shadowThickness: 0
*MatchModeResultsDialog*dump stdout*labelString: Dump results to stdout

###################
# Point Editor Options
###################
*PointEditorOptsDialog*traversalOn: True
*PointEditorOptsDialog*title: More Qualifier's Options
*PointEditorOptsDialog*GenQualUniqueCmd.labelString: Check general qualifier for uniqueness
*PointEditorOptsDialog*ShowPointLabelsCmd.labelString: Show Point Labels (Tags)

###################
# List Points Dialog
###################
*tp Info Window*errorLog.fontList: 8x13

###################
* Print Dialog
###################
*PrintDialog*traversalOn: True
*PrintDialog*title: Print Settings
*PrintDialog*XmTextField.marginHeight: 3
*PrintDialog*XmFrame.shadowThickness: 3
*PrintDialog*frame.shadowThickness: 0
*PrintDialog*PrntFile*label.labelString: File Name:
*PrintDialog*PrntName*label.labelString: Print Command:
*PrintDialog*PrintToFrameLabel.labelString: Print To
*PrintDialog*PrintToRc1*orientation: HORIZONTAL
*PrintDialog*FileView*labelString: File Name:
*PrintDialog*PrinterView*labelString: Print Command:
*PrintDialog*FileFormatFrameLabel.labelString: File Format
*PrintDialog*FileFormatFrame*orientation: HORIZONTAL
*PrintDialog*PaperSizeFrameLabel.labelString: Paper Size
*PrintDialog*PaperSizeFrame*orientation: VERTICAL
*PrintDialog*PaperSizeFrame*numColumns: 2

###################
# Text widget resources
###################
*XmTextField.marginHeight: 1
*XmTextField.fontList: 8x13

###################
# Form and Frame widget resources
###################
*XmFrame.shadowThickness: 0
*XmForm.marginHeight: 0
*XmForm.marginWidth: 0
*XmForm.borderWidth: 0

!----------------------------------------------------------------------------
! Menus
!----------------------------------------------------------------------------
*File*Open Image File.labelString: Open Image File...
*File*Remove Image.labelString: Remove Image...
*File*Load Point File.labelString: Load Point File...
*File*Close Point File.labelString: Close Point File
*File*Save Point File.labelString: Save Point File
*File*Save Point File As.labelString: Save Point File As...
*File*Load Config.labelString: Load Config...
*File*Save Config.labelString: Save Config...
*File*Save Config As.labelString: Save Config As...
*File*Pring.labelString: Print...
*File*Save and Exit.labelString: Save and Exit
*File*Exit.labelString: Exit

!----------------------------------------------------------------------------
!
! Mnemonics and accelerators...
!
! The following Ctrl- accelerators are used at the tp top level:
! ABCDEFGHIJKLMNOPQRSTUVWXYZ
! A CDE      L  O   S    X    
!----------------------------------------------------------------------
*File.mnemonic: F
 
*File.Open Image File.mnemonic: O
*File.Open Image File.accelerator: Ctrl<Key>O
*File.Open Image File.acceleratorText: Ctrl+O
 
*File.Load Point File.mnemonic: L
*File.Load Point File.accelerator: Ctrl<Key>L
*File.Load Point File.acceleratorText: Ctrl+L
 
*File.Load Config.mnemonic: C
*File.Load Config.accelerator: Ctrl<Key>C
*File.Load Config.acceleratorText: Ctrl+C

*File.Save and Exit.mnemonic: x
*File.Save and Exit.accelerator: Ctrl<Key>X
*File.Save and Exit.acceleratorText: Ctrl+X

*File.Exit.mnemonic: E
*File.Exit.accelerator: Ctrl<Key>E
*File.Exit.acceleratorText: Ctrl+E
 
*Edit.mnemonic: E
 
*Edit.Save Point.mnemonic: S
*Edit.Save Point.accelerator: Ctrl<Key>S
*Edit.Save Point.acceleratorText: Ctrl+S
 
*Edit.Delete Point.mnemonic: D
*Edit.Delete Point.accelerator: Ctrl<Key>D
*Edit.Delete Point.acceleratorText: Ctrl+D

*Edit.Auto Sync Points.mnemonic: A
*Edit.Auto Sync Points.accelerator: Ctrl<Key>A
*Edit.Auto Sync Points.acceleratorText: Ctrl+A

*Point.mnemonic: P

*View.mnemonic: V

*Help.mnemonic: H

$!-----------------------------------------------------------------------------
$ create TpDisplayOne.xpm
/* XPM */
static char * TpDisplayOne_xpm[] = {
"32 32 3 1",
" 	c white",
".	c black",
"X	c khaki3",
"                                ",
"                                ",
"                                ",
"   ..........................   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   ..........................   ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpDisplayThree.xpm
/* XPM */
static char * TpDisplayThree_xpm[] = {
"32 32 4 1",
" 	c white",
".	c black",
"X	c khaki3",
"o	c turquoise3",
"                                ",
"                                ",
"                                ",
"   ..........................   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   .XXXXXXX.oooooooo.XXXXXXX.   ",
"   ..........................   ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpDisplayTwo.xpm
/* XPM */
static char * TpDisplayTwo_xpm[] = {
"32 32 4 1",
" 	c white",
".	c black",
"X	c khaki3",
"o	c turquoise3",
"                                ",
"                                ",
"                                ",
"   ..........................   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   .XXXXXXXXXXXX.ooooooooooo.   ",
"   ..........................   ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpLock.xpm
/* XPM */
static char * TpLock_xpm[] = {
"32 32 2 1",
" 	c white",
".	c red",
"                                ",
"                                ",
"                                ",
"                                ",
"              .....             ",
"            .... ....           ",
"           ..       ..          ",
"          .           .         ",
"         ..           ..        ",
"         .             .        ",
"        ..             ..       ",
"        ..             ..       ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"                                ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpUnlock.xpm
/* XPM */
static char * TpUnlock_xpm[] = {
"32 32 2 1",
" 	s None	c None",
".	c black",
"                                ",
"                                ",
"              .....             ",
"            .... ....           ",
"           ..       ..          ",
"          .           .         ",
"         ..           ..        ",
"         .             ..       ",
"        ..             ..       ",
"                       ..       ",
"                       ..       ",
"                       ..       ",
"                       ..       ",
"                       ..       ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"      .....................     ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpPointSize10.xbm
#define TpPointSize10_width 30
#define TpPointSize10_height 30
static char TpPointSize10_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xfc, 0x0f, 0x00, 0x00, 0xfc, 0x0f, 0x00, 0x00, 0xfc, 0x0f, 0x00,
   0x00, 0xfc, 0x0f, 0x00, 0x00, 0xfc, 0x0f, 0x00, 0x00, 0xfc, 0x0f, 0x00,
   0x00, 0xfc, 0x0f, 0x00, 0x00, 0xfc, 0x0f, 0x00, 0x00, 0xfc, 0x0f, 0x00,
   0x00, 0xfc, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpPointSize15.xbm
#define TpPointSize15_width 30
#define TpPointSize15_height 30
static char TpPointSize15_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00,
   0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00,
   0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00,
   0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00,
   0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00, 0x80, 0xff, 0x3f, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpPointSize20.xbm
#define TpPointSize20_width 30
#define TpPointSize20_height 30
static char TpPointSize20_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpPointSize25.xbm
#define TpPointSize25_width 30
#define TpPointSize25_height 30
static char TpPointSize25_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07, 0xfc, 0xff, 0xff, 0x07,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpPointSize30.xbm
#define TpPointSize30_width 30
#define TpPointSize30_height 30
static char TpPointSize30_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f};
$!-----------------------------------------------------------------------------
$ create TpPointSize5.xbm
#define TpPointSize5_width 30
#define TpPointSize5_height 30
static char TpPointSize5_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xf0, 0x01, 0x00, 0x00, 0xf0, 0x01, 0x00, 0x00, 0xf0, 0x01, 0x00,
   0x00, 0xf0, 0x01, 0x00, 0x00, 0xf0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpPointSymbolCross45.xbm
#define TpPointSymbolCross45_width 16
#define TpPointSymbolCross45_height 16
static char TpPointSymbolCross45_bits[] = {
   0x03, 0xc0, 0x07, 0xe0, 0x0e, 0x70, 0x1c, 0x38, 0x38, 0x1c, 0x70, 0x0e,
   0xe0, 0x07, 0xc0, 0x03, 0xc0, 0x03, 0xe0, 0x07, 0x70, 0x0e, 0x38, 0x1c,
   0x1c, 0x38, 0x0e, 0x70, 0x07, 0xe0, 0x03, 0xc0};
$!-----------------------------------------------------------------------------
$ create TpPointSymbolCrossWithDot.xbm
#define TpPointSymbolCross_width 16
#define TpPointSymbolCross_height 16
static char TpPointSymbolCross_bits[] = {
   0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
   0x00, 0x00, 0xbf, 0xfd, 0xbf, 0xfd, 0x00, 0x00, 0x80, 0x01, 0x80, 0x01,
   0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01};
$!-----------------------------------------------------------------------------
$ create TpPointSymbolCrossWithHole45.xbm
#define TpPointSymbolCrossWithHole45_width 16
#define TpPointSymbolCrossWithHole45_height 16
static char TpPointSymbolCrossWithHole45_bits[] = {
   0x03, 0xc0, 0x07, 0xe0, 0x0e, 0x70, 0x1c, 0x38, 0x38, 0x1c, 0x70, 0x0e,
   0x60, 0x06, 0x00, 0x00, 0x00, 0x00, 0x60, 0x06, 0x70, 0x0e, 0x38, 0x1c,
   0x1c, 0x38, 0x0e, 0x70, 0x07, 0xe0, 0x03, 0xc0};
$!-----------------------------------------------------------------------------
$ create TpPointSymbolDot.xbm
#define TpPointSymbolDot_width 16
#define TpPointSymbolDot_height 16
static char TpPointSymbolDot_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x80, 0x01, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpPointSymbolRect.xbm
#define TpPointSymbolRect_width 16
#define TpPointSymbolRect_height 16
static char TpPointSymbolRect_bits[] = {
   0xff, 0xff, 0xff, 0xff, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0,
   0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0,
   0x03, 0xc0, 0x03, 0xc0, 0xff, 0xff, 0xff, 0xff};
$!-----------------------------------------------------------------------------
$ create TpPointSymbolRectWithCrossesWithDot.xbm
#define RectangleWithCrossesWithDot_width 16
#define RectangleWithCrossesWithDot_height 16
static char RectangleWithCrossesWithDot_bits[] = {
   0xff, 0xff, 0x87, 0xe1, 0x8f, 0xf1, 0x9d, 0xb9, 0xb9, 0x9d, 0x31, 0x8c,
   0x01, 0x80, 0x9f, 0xf9, 0x9f, 0xf9, 0x01, 0x80, 0x31, 0x8c, 0xb9, 0x9d,
   0x9d, 0xb9, 0x8f, 0xf1, 0x87, 0xe1, 0xff, 0xff};
$!-----------------------------------------------------------------------------
$ create TpDecZoom.xbm
#define TpDecZoom_width 16
#define TpDecZoom_height 16
static char TpDecZoom_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
$!-----------------------------------------------------------------------------
$ create TpIncZoom.xbm
#define TpIncZoom_width 16
#define TpIncZoom_height 16
static char TpIncZoom_bits[] = {
   0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
   0x80, 0x01, 0xff, 0xff, 0xff, 0xff, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
   0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01};
$!-----------------------------------------------------------------------------
$ create TpDeletePoint.xpm
/* XPM */
static char * TpDeletePoint_xpm[] = {
"32 32 2 1",
" 	c khaki",
".	c red",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"  ...  .... .   .... ..... .... ",
"  .  . .    .   .      .   .    ",
"  .  . ...  .   ...    .   ...  ",
"  .  . .    .   .      .   .    ",
"  .  . .    .   .      .   .    ",
"  ...  .... ... ....   .   .... ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"    ...   ..  ... .  . .....    ",
"    .  . .  .  .  .. .   .      ",
"    .  . .  .  .  . ..   .      ",
"    ...  .  .  .  .  .   .      ",
"    .    .  .  .  .  .   .      ",
"    .     ..  ... .  .   .      ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpKeepPoint.xpm
/* XPM */
static char * TpKeepPoint_xpm[] = {
"32 32 2 1",
" 	c khaki",
".	c red",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"     .  .  ....  ....  ...      ",
"     . .   .     .     .  .     ",
"     ..    ...   ...   .  .     ",
"     . .   .     .     ...      ",
"     .  .  .     .     .        ",
"     .  .  ....  ....  .        ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"    ...   ..  ... .  . .....    ",
"    .  . .  .  .  .. .   .      ",
"    .  . .  .  .  . ..   .      ",
"    ...  .  .  .  .  .   .      ",
"    .    .  .  .  .  .   .      ",
"    .     ..  ... .  .   .      ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpListPoints.xpm
/* XPM */
static char * TpListPoints_xpm[] = {
"32 32 2 1",
" 	c khaki",
".	c red",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"     .     ...    ..   .....    ",
"     .      .    .  .    .      ",
"     .      .     .      .      ",
"     .      .      .     .      ",
"     .      .    .  .    .      ",
"     ....  ...    ..     .      ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"  ...   ..  ... .  . .....  ..  ",
"  .  . .  .  .  .. .   .   .  . ",
"  .  . .  .  .  . ..   .    .   ",
"  ...  .  .  .  .  .   .     .  ",
"  .    .  .  .  .  .   .   .  . ",
"  .     ..  ... .  .   .    ..  ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpShiftLeft.xpm
/* XPM */
static char * TpShiftLeft_xpm[] = {
"32 32 2 1",
" 	c cadet blue",
".	c black",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"              ..    ..          ",
"             ..    ..           ",
"            ..    ..            ",
"           ..    ..             ",
"          ..    ..              ",
"           ..    ..             ",
"            ..    ..            ",
"             ..    ..           ",
"              ..    ..          ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpShiftRight.xpm
/* XPM */
static char * TpShiftRight_xpm[] = {
"32 32 2 1",
" 	c cadet blue",
".	c black",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"          ..    ..              ",
"           ..    ..             ",
"            ..    ..            ",
"             ..    ..           ",
"              ..    ..          ",
"             ..    ..           ",
"            ..    ..            ",
"           ..    ..             ",
"          ..    ..              ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                "};
$!-----------------------------------------------------------------------------
$ create TpKeepPointI.xpm
/* XPM */
static char * TpKeepPointI_xpm[] = {
"32 32 2 1",
" 	c grey",
".	c red",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"     .  .  . ..  . ..  . .      ",
"                          .     ",
"     ..    . .   . .   .  .     ",
"     .     .     .     . .      ",
"        .                       ",
"     .  .  . ..  . ..  .        ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"    . .   .   . . .  . . . .    ",
"       . .  .      .     .      ",
"    .  . .  .  .  . ..          ",
"    . .           .  .   .      ",
"         .  .  .                ",
"    .     ..  . . .  .   .      ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                ",
"                                "};
$ Return
$!#############################################################################
