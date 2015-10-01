$!****************************************************************************
$!
$! Build proc for MIPL module xvd
$! VPACK Version 1.9, Thursday, May 31, 2007, 13:05:52
$!
$! Execute by entering:		$ @xvd
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
$ write sys$output "*** module xvd ***"
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
$ write sys$output "Invalid argument given to xvd.com file -- ", primary
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
$   if F$SEARCH("xvd.imake") .nes. ""
$   then
$      vimake xvd
$      purge xvd.bld
$   else
$      if F$SEARCH("xvd.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvd
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvd.bld "STD"
$   else
$      @xvd.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvd.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvd.com -mixed -
	-s XvdApp.cc -
	-i xvd.imake -
	-p xvd.pdf XVd.xres xvd.dict
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XvdApp.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "XvdApplication.h"
#include "ImageWindow.h"

Application * xvdApp = new XvdApplication("XVd");
MainWindow * imageWindow = new ImageWindow("XVd");
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvd.imake
#define PROGRAM xvd
#define MODULE_LIST XvdApp.cc

#define GUILIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS

/* #define ENABLE_SAGE */
#ifdef ENABLE_SAGE
#define LIB_DALI
#define LIB_SAGE_BASE
#define LIB_SAGE_CLIENT
#define LIB_DD_PLUS_PLUS
#define CCC_TEMPLATES
#endif

#ifndef NO_PDS
#define LIB_PDS
#endif

#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_XMU
#define LIB_P1SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_XPM

#if 0
#define LIB_LOCAL
#define LOCAL_LIBRARY `ar t sublib.a`
#define DEBUG
#endif


$ Return
$!#############################################################################
$PDF_File:
$ create xvd.pdf
procedure help=*
parm inp string count=(0:3) default=--
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

if ($syschar(1) = "UNIX")
   ush $GUILIB/xvd &options &files &
else
   dcl spawn/nowait xvd &options &files
end-if
end-proc
.title
VICAR program xvd
.help
The xvd program is an X-windows display program.  It is intended as the main
display of the GUI, and to eventually replace VIDS.

Complete documentation for xvd is available on-line at:
<http://www-mipl.jpl.nasa.gov/Xvd_1.0/Xvd/Xvd.html>
or via clicking on the various Help options in the program.

xvd is an interactive, X-windows program.  Before using it, you must have
set up the X display connection.  On a Unix machine, this is typically done
via "setenv DISPLAY" while on VMS it is typically done via "set disp/create".
Consult system documentation for details on establishing an X connection, or
refer to the on-line tutorial in Mosaic at
<http://www-mipl.jpl.nasa.gov/wstutor/set_x_display.html>
Please note that the X display must be set up outside of the VICAR environment.
.page
Running the program:

The xvd program may be run either from within VICAR, or directly from
the shell or DCL.  In order to run it from the shell (in Unix), make sure
"$GUILIB" is in your path, or specify it explicitly:

	% $GUILIB/xvd image.red image.grn image.blu

From VMS, "xvd" is automatically defined for you:

	$ xvd image.red image.grn image.blu
.page
From VICAR, usage conforms to standard VICAR input file conventions:

	VICAR> xvd (image.red,image.grn,image.blu)

xvd can handle 0 to 3 filenames, and abbreviations and band specifications
(in parentheses after the filename) are allowed.  See the Mosaic help for
complete documentation.

Command-line options may be given to the program via the "options" parameter;
see the help for that parameter for details.

.level1
.vari inp
Input file(s)
.vari options
Command-line options
.level2
.vari inp
Zero to three input files.  A B&W image has one input file, a color image
typically has three input files in the order (red, green, blue), or a single
3-band input file.

If a filename starts with a ".", the previous name is stripped of its
extension, and the given extension is used instead.  This allows files to
be specified using abbreviations:

	xvd (/usr/local/images/io.red .grn .blu)

In addition, a band number in parentheses may follow any filename, but there
must not be any spaces.  You may have to quote the string.

Filename and especially band selection is much easier using the Open...
dialog box in the program.

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
	xvd /usr/local/images/io.red opt="-xrm '*imageZoom: -2'"

VMS:
	xvd images:io.red opt="-xrm ""*imageZoom: -2"""
.end
$!-----------------------------------------------------------------------------
$ create XVd.xres
!******************************************************************************
!  Resource File for the xvd program
! 
!  This resource file contains all the resources required for proper execution
!  of the xvd program.
!
!  The resources are identified  by function and further by resources that are
!  configurable by the user and resources that are not configurable by the 
!  user.
!
!  The configurable and nonconfigurable resources are followed by the resources
!  needed for changing label text.  Again, these are broken down by function.
!******************************************************************************
!
!  Resources for the xvd View - This is the first window to appear.  It 
!  contains the image, the side bar and menues. The Side Bar contains
!  various button for tools and cursor information.
!
!******************************************************************************
!  XVD Resources - CONFIGURABLE BY THE USER
!
! --> WARNING: not all of the resources defined in this file are modifiable.
!              Also, this file is not organized by modifiable/unmodifiable
!              resources.
!******************************************************************************

*XVd.title:				xvd

! Since Unix names the application in lower case, and VMS in upper case,
! all resource specs should start with "*" and no application name.  The
! exceptions are global foreground and background, which need the app _class_
! name (the same as the resource file name) in order to properly override
! the xrdb universal defaults on both VMS and Unix.  However, this requires
! the class name on *all* foreground/background resources.

XVd*background: cadet blue
XVd*foreground: black
XVd*menubar*background: cadet blue
XVd*action_area*background: cadet blue
XVd*imageView*background: black

*tearOffModel:				TEAR_OFF_ENABLED
*enablePrintWidgetTree:			True

*allocRedLevels:			8
*allocGreenLevels:			8
*allocBlueLevels:			6
*allocGrayLevels:			0

*showSideBar:				true
*fullScreenDisplay:			false
*displayMotifBorder:			true
*displayMenuBar:			true

*imageView.stretchPolicy:		USE_SW
*imageView.colormapPolicy:		ALLOC
*imageView.bwColormapPolicy:            ALLOC
*imageView.dither:			ORDERED
*imageView.bwDither:			ORDERED
*imageView.bwVisualType:		use_24bit
*imageView.pseudoVisualType: 		use_24bit

*imageView.shadowThickness:      	0
*imageView.highlightThickness: 		0
*imageView.translations: #override \n\
	<Btn1Down>: 			MousePanStart() \n\
	<Btn1Motion>: 			MousePan() \n\
	~Shift~Ctrl~Meta<Key>osfLeft: 	PanOne(left) \n\
	~Shift~Ctrl~Meta<Key>osfRight: 	PanOne(right) \n\
	~Shift~Ctrl~Meta<Key>osfUp: 	PanOne(up) \n\
	~Shift~Ctrl~Meta<Key>osfDown: 	PanOne(down) \n\
	Shift~Ctrl~Meta<Key>osfLeft: 	PanHalfView(left) \n\
	Shift~Ctrl~Meta<Key>osfRight: 	PanHalfView(right) \n\
	Shift~Ctrl~Meta<Key>osfUp: 	PanHalfView(up) \n\
	Shift~Ctrl~Meta<Key>osfDown: 	PanHalfView(down) \n\
	Ctrl~Shift~Meta<Key>osfLeft: 	PanEdge(left) \n\
	Ctrl~Shift~Meta<Key>osfRight: 	PanEdge(right) \n\
	Ctrl~Shift~Meta<Key>osfUp: 	PanEdge(up) \n\
	Ctrl~Shift~Meta<Key>osfDown:	PanEdge(down) \n\
	<Key>osfEscape:                 CursorMode(toggle) \n\
	~Shift<Key>grave:               CursorMode(toggle) \n\
	<Key>asciitilde:                CursorMode(toggle,true) \n\
	Shift<Key>grave:                CursorMode(toggle,true) \n\
	<Key>plus:                      CursorMode(floating) \n\
	<Key>minus:                     CursorMode(planted) \n\
	Shift<Motion>:                  MoveCursorMouse() \n\
	Shift Ctrl~Meta<Key>osfLeft:	MoveCursor(left) \n\
	Shift Ctrl~Meta<Key>osfRight:	MoveCursor(right) \n\
	Shift Ctrl~Meta<Key>osfUp:	MoveCursor(up) \n\
	Shift Ctrl~Meta<Key>osfDown:	MoveCursor(down) \n\
	Meta~Shift~Ctrl<Key>osfLeft:	MoveCursorScreen(left) \n\
	Meta~Shift~Ctrl<Key>osfRight:	MoveCursorScreen(right) \n\
	Meta~Shift~Ctrl<Key>osfUp:	MoveCursorScreen(up) \n\
	Meta~Shift~Ctrl<Key>osfDown:	MoveCursorScreen(down)

! The following can be changed (or set via -xrm) to enable a script to be
! run from xvd.  The script name must be in the path, or be a full pathname,
! and must be executable.  The single argument to the script is a temporary
! file with information about xvd's state.  See SiRunScriptCmd.cc for
! details.
! Save is also implemented via a script.  This script may be replaced to
! add additional functionality (e.g. other file types).  It should be
! implemented in-line; this was a pragmatic decision to get something
! running quickly.
! Print is in the same boat, too.  It actually uses the same script as Save,
! but that's not required.

*enableScriptCommand: False
*scriptCommand: dummy_name

*enableSaveCommand: True
*saveCommand: $GUILIB/xvd_save_script

*enablePrintCommand: True
*printCommand: $GUILIB/xvd_save_script

!******************************************************************************
!  XVD Resources - NOT CONFIGURABLE BY THE USER
!******************************************************************************

*imageView.traversalOn:		False
*imageDisplayer*traversalOn:	False

!******************************************************************************
!
!  Resources for XVicDisp SideBar View.  This is in the Box that appears to 
!  the left of the Image Window. 
!
!******************************************************************************
!  Side Bar View- CONFIGURABLE BY THE USER
!******************************************************************************

! Resources for histogram button on the SideBar
*sideBar.Raw Hist.histGraphView.width:      	64
*sideBar.Raw Hist.histGraphView.height:     	64
*sideBar.Raw Hist.shadowThickness:           	4
*sideBar*histGraphView.horizontal:		TRUE
*sideBar*histGraphView.blended: 		TRUE
*sideBar*histGraphView.ascending:    		TRUE
*sideBar*histGraphView.spike:        		3
XVd*sideBar*histGraphView.background:  		black

*sideBar.Lookup Table View.lutGraphView.width:  64
*sideBar.Lookup Table View.lutGraphView.height: 64
*sideBar.Lookup Table View.shadowThickness: 	4

!******************************************************************************
!*** Cursor Display

*textfieldBlueDn.topShadowColor:   	blue
*textfieldRedDn.topShadowColor:   	red
*textfieldGreenDn.topShadowColor:   	green
*textfieldBwDn.topShadowColor:   	grey
*textfieldCursorX.topShadowColor:   	grey
*textfieldCursorY.topShadowColor:   	grey

*cursorPositionView*columns:   		5
*cursorDnView*columns:   		3

*cursorPositionView*fontList: -*-new century schoolbook-medium-r-normal--14-*-75-*-*-*-*-*
*cursorDnView*fontList: -*-new century schoolbook-medium-r-normal--14-*-75-*-*-*-*-*
*cursorLatLonView*fontList: -*-new century schoolbook-medium-r-normal--14-*-75-*-*-*-*-*

!******************************************************************************
!*** Image Size View

*labelImageSize.labelString:     	Image Size (NSxNL):
*textfieldImageSize.topShadowColor:   	grey
*imageSizeView*columns:   		5
*imageSizeView*fontList: -*-new century schoolbook-medium-r-normal--12-*-75-*-*-*-*-*
*imageSizeView*cursorPositionVisible:	False
*imageSizeView*editable:		False
*imageSizeView*traversalOn:		False


!******************************************************************************
!*** Zoom Menu View

*zoomMenu.orientation: 			Vertical


!******************************************************************************
!  Side Bar View - NOT CONFIGURABLE BY THE USER
!******************************************************************************


!******************************************************************************
! *** Cursor Display
*cursorPositionView*cursorPositionVisible:	False
*cursorDnView*cursorPositionVisible:		False

*cursorPositionView*editable:			False
*cursorDnView*editable:				False


!******************************************************************************
!
!  Resources for Lut View.  This window appears when the Lookup Table 
!  button is selected under the Tools Menu or the Button in the SideBar that
!  has a drawing of the Lookup Table is pressed.
!
!******************************************************************************
!  LUT View - CONFIGURABLE BY THE USER
!******************************************************************************

*lutGraphView.width:  256
*lutGraphView.height: 256
XVd*lutGraphView.background: black

!******************************************************************************
!  LUT View - NOT CONFIGURABLE BY THE USER
!******************************************************************************
! None
!******************************************************************************
!
!  Resources for Hist View.  This is the window that appears when the HIST 
!  button is selected under the Tools Menu or the Button in the SideBar that
!  has a drawing of the Histogram is pressed.
!
!******************************************************************************
!  Histogram View - CONFIGURABLE BY THE USER
!******************************************************************************

! Resources for histogram window
!	General
*histBox*method:                	BLEND
*histBox*horizontal: 			TRUE
*histBox*popDirection:			ROW
*histBox*spike:				1
*histBox*showAxis:			TRUE
*histBox*showStat:			TRUE
*histBox*showHist:			TRUE
XVd*histBox*graph.background:		black
*histBox.RedHist.graph.redColor:	red
*histBox.GrnHist.graph.redColor:	green
*histBox.BluHist.graph.redColor:	blue
!	Axis
*histBox*axis*width:			40
*histBox*axis*fontList:			6x10
!	Stat
XVd*histBox*statView.frameMeanR.background: 	red
XVd*histBox*statView.frameMeanG.background:    	green
XVd*histBox*statView.frameMeanB.background:    	blue

XVd*histBox*statView.frameStDevR.background: 	red
XVd*histBox*statView.frameStDevG.background:   	green
XVd*histBox*statView.frameStDevB.background:   	blue

XVd*histBox*RedHist.statView.frameMeanR.background:	red
XVd*histBox*GrnHist.statView.frameMeanR.background: 	green
XVd*histBox*BluHist.statView.frameMeanR.background: 	blue

XVd*histBox*RedHist.statView.frameStDevR.background: 	red
XVd*histBox*GrnHist.statView.frameStDevR.background: 	green
XVd*histBox*BluHist.statView.frameStDevR.background: 	blue

*histBox*statView.fontList:                  lucidasans-12

! Graph component color preferences

*redColor:			red
*greenColor:			green
*blueColor:			blue
*yellowColor:			yellow
*cyanColor:			cyan
*magentaColor:			magenta
*whiteColor:			white

! Histogram spike dialog
*Spike*packing:		        	XmPACK_TIGHT
*Spike*workArea*labelString:		Spike Value:
*Spike*columns:				5
*Spike*Cancel*labelString:		Close

!******************************************************************************
!  Hist View - NOT CONFIGURABLE BY THE USER
!******************************************************************************
!  None
!******************************************************************************
!
!  Resources for Pan Tool View.  This is the window that appears when the PAN
!  button is selected under the Tools Menu.
!
!******************************************************************************
!  Pan Tool View - CONFIGURABLE BY THE USER
!******************************************************************************

*panBoxColor: red
*panDesiredSize: 200
*applyStretchToPan: False

!******************************************************************************
!  Pan Tool View - NOT CONFIGURABLE BY THE USER
!******************************************************************************

*panTool.traversalOn:      False
*panTool*dataSavePolicy:   XIMAGE
*panTool*workProcPolicy:   READ

*panTool*translations: #override \n\
	~Shift<Btn1Down>:		Input(pan_mouse, start) \n\
	~Shift<Btn1Motion>:		Input(pan_mouse, drag) \n\
	~Shift~Ctrl~Meta<Key>osfLeft:	Input(pan_one, left) \n\
	~Shift~Ctrl~Meta<Key>osfRight:	Input(pan_one, right) \n\
	~Shift~Ctrl~Meta<Key>osfUp:	Input(pan_one, up) \n\
	~Shift~Ctrl~Meta<Key>osfDown:	Input(pan_one, down) \n\
	Ctrl~Shift~Meta<Key>osfLeft:	Input(pan_edge, left) \n\
	Ctrl~Shift~Meta<Key>osfRight:	Input(pan_edge, right) \n\
	Ctrl~Shift~Meta<Key>osfUp:	Input(pan_edge, up) \n\
	Ctrl~Shift~Meta<Key>osfDown:	Input(pan_edge, down) \n\
	Shift~Ctrl~Meta<Key>osfLeft:	Input(pan_half_view, left) \n\
	Shift~Ctrl~Meta<Key>osfRight:	Input(pan_half_view, right) \n\
	Shift~Ctrl~Meta<Key>osfUp:	Input(pan_half_view, up) \n\
	Shift~Ctrl~Meta<Key>osfDown:	Input(pan_half_view, down)

!******************************************************************************
!  Mag Glass Tool View - NOT CONFIGURABLE BY THE USER
!******************************************************************************

*Mag.labelString: Magnifying Glass
*magTool*translations: #override \n\
	~Shift<Btn1Down>:		Input(mag_ratio, increase) \n\
	~Shift<Btn2Down>:		Input(mag_ratio, decrease) \n\
	Shift~Ctrl~Meta<Key>osfUp:	Input(mag_size, increase) \n\
	Shift~Ctrl~Meta<Key>osfDown:	Input(mag_size, decrease)

!******************************************************************************
!
!  Resources for changing the text of label for the various functions.
!
!******************************************************************************
!  XVicDisp View Label Strings - CONFIGURABLE BY THE USER
!******************************************************************************

!******************************************************************************
! *** File Selection Box Labels

*FileSelBox*Band 1.labelString:		Red/BW:
*FileSelBox*Band 2.labelString:		Green:
*FileSelBox*Band 3.labelString:		Blue:
*FileSelBox*toggleLabel.labelString:
*FileSelBox*filesLabel.labelString: 	Filename
*FileSelBox*bandsLabel.labelString: 	Band
*FileSelWindow*filename.labelString: SAGE Parameter Mode
*FileSelBox*typeLabel.labelString:	Open Dual-Label File As:
*FileSelBox*typePDS.labelString:	PDS
*FileSelBox*typeVICAR.labelString:	VICAR

!******************************************************************************
! *** Parameter Box specific parameters

*FileSelWindow.title: xvd

*DefaultView.shadowThickness: 4
*DataFlowView.shadowThickness: 4
*NamedFlowView.shadowThickness: 4

!******************************************************************************
! *** Zoom Dialog Box and Option Menu

*ZoomDialog*Zoom X In*labelString: 	Zoom X In
*ZoomDialog*Zoom X Out*labelString: 	Zoom X Out
*ZoomDialog*Zoom Y In*labelString: 	Zoom Y In
*ZoomDialog*Zoom Y Out*labelString: 	Zoom Y Out
*ZoomDialog.Zoom.leftOffset: 		20
*ZoomDialog.Zoom*field.columns: 	8

 
*zoomMenu.labelString: 			Zoom Factor

!******************************************************************************
! *** Preference Dialog Box Option Menu Labels

*Dither Mode.labelString: 		Dither Mode
*ColorMap Mode.labelString: 		ColorMap Policy
*LUT Mode.labelString: 			Stretch Policy
*Lat/Lon Type.labelString:              Lat/Lon Type

!******************************************************************************
! *** Data Range Dialog Labels

*Data Range*Set Data Range.fontList: -*-new century schoolbook-medium-r-normal--14-*-75-*-*-*-*-*
*Data Range*Auto Range.alignment: ALIGNMENT_BEGINNING
*Data Range*XmTextField.columns: 12

!******************************************************************************
! *** Side Bar Fonts for Cursor Display

*labelRedDn.labelString:                R:
*labelGreenDn.labelString:              G:
*labelBlueDn.labelString:               B:
*labelBwDn.labelString:                 Grey:
*labelCursorX.labelString:     		Samp:
*labelCursorY.labelString:     		Line:

!******************************************************************************
!  Hist View Label Strings - CONFIGURABLE BY THE USER
!******************************************************************************

*statView*labelM.labelString:		Mean:
*statView*labelSD.labelString:          Sigma:
*statView*labelMean.labelString:        R:
*statView*labelStDev.labelString:       R:
*statView*labelMean1.labelString:       G:
*statView*labelStDev1.labelString:      G:
*statView*labelMean2.labelString:       B:
*statView*labelStDev2.labelString:      B:

!*******************************************************************************
!  Stretch Dialog Label Strings - CONFIGURABLE BY THE USER
!*******************************************************************************
*File.load.labelString: 		Load IBIS LUT
*File.save.labelString:			Save as IBIS LUT
*Options.mnemonic: O
*Options.postPercValuesDialog.labelString:	View Percent Stretch Limits

*Stretch Dialog*alarm.labelString:	Alarm
*Stretch Dialog*clip.labelString:	Bit Clipping
*Stretch Dialog*comp.labelString:	Complement
*Stretch Dialog*contour.labelString:	Contour
*Stretch Dialog*ellipse.labelString:	Ellipse
*Stretch Dialog*func.labelString:	Function
*Stretch Dialog*gauss.labelString:	Gaussian
*Stretch Dialog*itable.labelString:	Integer Table
*Stretch Dialog*linear.labelString:	Linear
*Stretch Dialog*log.labelString:	Logarithmic
*Stretch Dialog*mean.labelString:	Mean
*Stretch Dialog*off.labelString:        Switch-Off
*Stretch Dialog*peak.labelString:	Peak
*Stretch Dialog*percent.labelString:	Percent
*Stretch Dialog*period.labelString:	Periodic (Sinusoidal)
*Stretch Dialog*power.labelString:	Power
*Stretch Dialog*raw.labelString:        Ramp (Linear 0-255)
*Stretch Dialog*smooth.labelString:	Smooth

*Stretch Dialog*low*labelString:	Low Pixel Value
*Stretch Dialog*high*labelString:	High Pixel Value
*Stretch Dialog*dnmin*labelString:	Min Pixel Value
*Stretch Dialog*dnmax*labelString:	Max Pixel Value
*Stretch Dialog*nVals*labelString:	Number of Values
*Stretch Dialog*dnValue*labelString:	Contour Value
*Stretch Dialog*nbits*labelString:	Clip Number of Bits
*Stretch Dialog*gmean*labelString:	Mean Value
*Stretch Dialog*gsigma*labelString:	Standard Deviation
*Stretch Dialog*curve*labelString:	Log Curvature
*Stretch Dialog*interval*labelString:	Contour Interval
*Stretch Dialog*maxnum*labelString:	Contour Max Number
*Stretch Dialog*backgnd*labelString:	Background
*Stretch Dialog*func*labelString:	Function (string)
*Stretch Dialog*mean*labelString:	Gaussian Mean
*Stretch Dialog*pmean*labelString:	Periodic PhaseMean
*Stretch Dialog*ampl*labelString:	Periodic Amplitude
*Stretch Dialog*freq*labelString:	Periodic Frequency
*Stretch Dialog*phi*labelString:	Periodic Phi
*Stretch Dialog*lPerc*labelString:	Low Percent
*Stretch Dialog*hPerc*labelString:	High Percent

*Stretch Dialog*Stretch.postFrame.postLabel.labelString: Post Stretches
*Stretch Dialog*Post List Dialog.labelString: Set Pixel Value Ranges
*Stretch Dialog*rcPostList.table.labelString: Table
*Stretch Dialog*columns:    10

XVd*Stretch Dialog*StretchDialogRC*stretchForm.stretchNameRC*foreground: gold
XVd*Stretch Dialog*rcPostList*foreground:    gold

!******************************************************************************
!  Stretch Dialog Resources - NOT CONFIGURABLE BY THE USER
!******************************************************************************

*Stretch Dialog*Stretch.stretchForm*frame.shadowThickness: 0
*Stretch Dialog*Stretch.stretchForm.stretchParms1.packing: XmPACK_COLUMN
*Stretch Dialog*Stretch.stretchForm.stretchParms2.packing: XmPACK_COLUMN
*Stretch Dialog*Stretch.stretchForm.stretchParms1*leftOffset: 20
*Stretch Dialog*Stretch.stretchForm.stretchParms2*leftOffset: 20
*Stretch Dialog*Stretch.stretchForm.stretchParms2.PostITableListDialog.topOffset: 10
*Stretch Dialog*Stretch.stretchForm.stretchParms2.PostITableListDialog.labelString: Set Integer Table Values

*Stretch Dialog*Stretch*func*field.columns:       46

*Stretch*visibleItemCount:              	4
*Stretch*field.columns:                 	4
*Stretch.curListInValue.label.labelString:      IN:
*Stretch*curListOutValue.label.labelString:     OUT:
*Stretch*curListOutValue*rightAttachment:       XmATTACH_FORM
*Stretch*listControl.add.labelString:           Add
*Stretch*listControl.delete.labelString:        Delete

*itableListDialog*OK.labelString:		Close
*tableListDialog*OK.labelString:		Close
*alarmListDialog*OK.labelString:		Close

*Stretch.listControl.add.leftOffset:            20
*Stretch.listControl.delete.rightOffset:        20
*tableListDialog*title:				Enter the TABLE Values
*tableListDialog.width:				250
*itableListDialog*title:                        Enter the ITABLE Values
*itableListDialog.width:                        250
*alarmListDialog*title:                         Enter the ALARM Values

*alarmListDialog*Stretch.curListOutValue*columns: 1
*alarmListDialog*Stretch.curListOutValue*rightAttachment: XmATTACH_NONE
*alarmListDialog*Stretch.curListInValue.label.labelString: Alarm Values:
*alarmListDialog*Stretch.curListOutValue.label.labelString:
*alarmListDialog*Stretch.listControl.add.leftOffset: 20
*alarmListDialog*Stretch.listControl.delete.rightOffset: 20

*Stretch Dialog*Stretch*PostTableListDialog.labelString: Set Table Values
*Stretch Dialog*Stretch*PostAlarmListDialog.labelString: Set Alarm Values

!*******************************************************
! percValuesDialog resources
!*******************************************************
*percValuesDialog*Percent Stretch Values.labelString: Percent Stretch Values
*percValuesDialog*High Label.labelString: High
*percValuesDialog*Low Label.labelString: Low
*percValuesDialog*field.editable: False
*percValuesDialog*frame.shadowThickness: 0
*percValuesDialog*cursorPositionVisible: False
XVd*percValuesDialog*highlightThickness: 0
XVd*percValuesDialog*lPercValueRed*field.topShadowColor: red
XVd*percValuesDialog*lPercValueGrn*field.topShadowColor: green
XVd*percValuesDialog*lPercValueBlu*field.topShadowColor: blue
XVd*percValuesDialog*hPercValueRed*field.topShadowColor: red
XVd*percValuesDialog*hPercValueGrn*field.topShadowColor: green
XVd*percValuesDialog*hPercValueBlu*field.topShadowColor: blue
*percValuesDialog*lPercValueRed*label.labelString: Red
*percValuesDialog*lPercValueGrn*label.labelString: Green
*percValuesDialog*lPercValueBlu*label.labelString: Blue
*percValuesDialog*hPercValueRed*label.labelString:
*percValuesDialog*hPercValueGrn*label.labelString:
*percValuesDialog*hPercValueBlu*label.labelString:

!******************************************************************************
! Save dialog
!******************************************************************************
*Save As.filename red.label.labelString: Red/BW:
*Save As.filename grn.label.labelString:  Green:
*Save As.filename blu.label.labelString:   Blue:
*Save As.post red.labelString: Browse...
*Save As.post grn.labelString: Browse...
*Save As.post blu.labelString: Browse...
*post red.title: Select Save File (Red/BW)
*post grn.title: Select Save File (Green)
*post blu.title: Select Save File (Blue)
*Save As*display.labelString: Displayed Area Only
*Save As*file.labelString: Entire File
*Save As*raw.labelString: Raw Data (no stretch)
*Save As*stretch.labelString: Save With Stretch
*Save As*pseudo.labelString: Pseudo and Stretch
*Save As*pseudo_only.labelString: Pseudo only (no stretch)
*Save As*preserve data type.labelString: Save Original Data Type
*Save As*convert to byte.labelString: Convert to Byte
*Save As*VICAR.labelString: Save as VICAR
*Save As*TIFF.labelString: Save as TIFF
*Save As*label1.labelString: Stretch/Pseudo only valid with Byte data
*Save As*label2.labelString: Only VICAR input files are supported
*Save As*label3.labelString: See terminal window for errors/messages

!******************************************************************************
! Print dialog
!******************************************************************************
*Print*to printer.labelString: Print to Printer
*Print*to file.labelString: Print to PostScript File
*Print*display.labelString: Displayed Area Only
*Print*file.labelString: Entire File
*Print*raw.labelString: Raw Data (no stretch)
*Print*stretch.labelString: Print With Stretch
*Print*pseudo.labelString: Pseudo and Stretch
*Print*pseudo_only.labelString: Pseudo only (no stretch)
*Print*printer command.label.labelString: Print Command:
*Print*printer command*field.columns: 16
*Print.filename.label.labelString: File:
*Print.post file.labelString: Browse...
*post file.title: Select Output PostScript File
*Print*label1.labelString: Blank height and/or width for auto-size
*Print*width.label.labelString: Image Width (inches):
*Print*width*field.columns: 6
*Print*height.label.labelString: Image Height (inches):
*Print*height*field.columns: 6
*Print*portrait.labelString: Portrait (tall)
*Print*landscape.labelString: Landscape (wide)
*Print*title filename.labelString: Use Filename for Title
*Print*title custom.labelString: Custom Title (may be blank)
*Print*title.label.labelString: Title:
*Print*label2.labelString: Only VICAR input files are supported
*Print*label3.labelString: See terminal window for errors/messages

!******************************************************************************
! Renamed labels for clarity - '...' added to menu items that bring up a dialog
!	and changed some of the names to be more appropriate
!******************************************************************************
*File.Open.labelString: Open...
*File.Save As.labelString: Save As...
*File.Print.labelString: Print...
*Stretch.labelString: Stretch...
*Zoom.labelString: Zoom...
*Special.labelString: Special...
*Tools.Pseudo.labelString: Pseudo...
*Image Popup.Pseudo.labelString: Pseudo...
*Preferences.labelString: Preferences...
*Data Range.labelString: Data Range...
*Rotate Image*0.labelString: None
*Rotate Image*90.labelString: CW 90 Degrees
*Rotate Image*180.labelString: 180 Degrees
*Rotate Image*270.labelString: CW 270 Degrees
*Rotate Image*Flip NE_SW.labelString: Diagonal Flip NE->SW
*Rotate Image*Flip NW_SE.labelString: Diagonal Flip NW->SE
*Spike.labelString: Spike...
*Raw Hist.labelString: Histogram
*Stretched Hist.labelString: Stretched Histogram
*Lookup Table View.labelString: Stretch Lookup Tables
*Pseudocolor View.labelString: Pseudocolor Tables
*menubar.Hist_View.labelString: Options
*StackNoBlend.labelString: Stack (No Blend)
*StackBlend.labelString: Stack (Blend)
*Show Stats.labelString: Statistics
*Show Axis.labelString: Axes
*Horizontal HistGraph.labelString: Horizontal Graph
*Vertical HistGraph.labelString: Vertical Graph
*ScriptCommand.labelString: Run Predefined Script
*LatLonBar.labelString: Lat/Lon Display


!----------------------------------------------------------------------------
!
! Mnemonics and accelerators...
!
! The following Ctrl- accelerators are used at the xvd top level:
!   CDEF HI  LM OPQRSTU    Z		(also Backspace)
!----------------------------------------------------------------------------

*File.mnemonic: F
*File.Open.mnemonic: O
*File.Open.accelerator: Ctrl<Key>O
*File.Open.acceleratorText: Ctrl+O
*File.Reload.mnemonic: R
*File.Reload.accelerator: Ctrl<Key>E
*File.Reload.acceleratorText: Ctrl+E
*File.Save As.mnemonic: S
*File.Save As.accelerator: Ctrl<Key>V
*File.Save As.acceleratorText: Ctrl+V
*File.Print.mnemonic: P
*File.Print.accelerator: Ctrl<Key>N
*File.Print.acceleratorText: Ctrl+N
*File.Exit.mnemonic: E
*File.Exit.accelerator: Ctrl<Key>Q
*File.Exit.acceleratorText: Ctrl+Q

*Edit.mnemonic: E
*Edit.Undo.mnemonic: U
*Edit.Undo.accelerator: Ctrl<Key>osfBackSpace
*Edit.Undo.acceleratorText: Ctrl+Backspace
*Data Range.mnemonic: D
*Data Range.accelerator: Ctrl<Key>D
*Data Range.acceleratorText: Ctrl+D
*Preferences.mnemonic: P
*Preferences.accelerator: Ctrl<Key>F
*Preferences.acceleratorText: Ctrl+F

*Tools.mnemonic: T

*Raw Hist.mnemonic: H
*Raw Hist.accelerator: Ctrl<Key>H
*Raw Hist.acceleratorText: Ctrl+H

*Stretched Hist.mnemonic: t
*Stretched Hist.accelerator: Ctrl<Key>T
*Stretched Hist.acceleratorText: Ctrl+T

*Stretch.mnemonic: S
*Stretch.accelerator: Ctrl<Key>S
*Stretch.acceleratorText: Ctrl+S

*Cursor Stretch.mnemonic: r
*Cursor Stretch.accelerator: Ctrl<Key>I
*Cursor Stretch.acceleratorText: Ctrl+I

*Label Display.mnemonic: L
*Label Display.accelerator: Ctrl<Key>L
*Label Display.acceleratorText: Ctrl+L

*Lookup Table View.mnemonic: b
*Lookup Table View.accelerator: Ctrl<Key>B
*Lookup Table View.acceleratorText: Ctrl+B

*Pseudocolor View.mnemonic: c
*Pseudocolor View.accelerator: Ctrl<Key>C
*Pseudocolor View.acceleratorText: Ctrl+C

*Pan.mnemonic: P
*Pan.accelerator: Ctrl<Key>P
*Pan.acceleratorText: Ctrl+P

*Mag.mnemonic: M
*Mag.accelerator: Ctrl<Key>M
*Mag.acceleratorText: Ctrl+M

*Pseudo.mnemonic: u
*Pseudo.accelerator: Ctrl<Key>U
*Pseudo.acceleratorText: Ctrl+U

*Zoom.mnemonic: Z
*Zoom.accelerator: Ctrl<Key>Z
*Zoom.acceleratorText: Ctrl+Z

*LatLonBar.mnemonic: a
*LatLonBar.accelerator: Ctrl<Key>A
*LatLonBar.acceleratorText: Ctrl+A

*ScriptCommand.mnemonic: d
*ScriptCommand.accelerator: Ctrl<Key>R
*ScriptCommand.acceleratorText: Ctrl+R

*menubar*Help.mnemonic: H
*Help.On Context.mnemonic: C
*Help.On Context.accelerator: <Key>osfHelp
*Help.On Context.acceleratorText: Help or F1
*Help.On Help.mnemonic: H
*Help.On Version.mnemonic: V
*Help.On Window.mnemonic: W
*Help.On Keys.mnemonic: K



!************
!************

!Help Stuff 

!************
!************



*helpBrowserCommand: netscape
*helpBaseLocation: http://www-mipl.jpl.nasa.gov/GUIHelp_1.4/
*helpLocation: Xvd/Xvd.html

!*********************************
! Help on the menu bar
!*********************************
*mainWindow*menubar.File*helpLocation: 	Xvd/Xvd.html#filemenu
*mainWindow*menubar.Edit*helpLocation: 	Xvd/Xvd.html#editmenu
*mainWindow*menubar.Tools*helpLocation: Xvd/Xvd.html#toolsmenu
*mainWindow*menubar.Help*helpLocation: 	Xvd/Xvd.html#helpmenu


!*********************************************
! Help on File->open display for Direct option 
!*********************************************
*FileSelWindow*FileSelBox*helpLocation:  Xvd/Xvd.html#open
*FileSelWindow*Help*helpLocation:        Xvd/Xvd.html#open


!*********************************************
! Help on File->open/exit tear-off menu
!*********************************************
*File.Open*helpLocation:    Xvd/Xvd.html#filemenu
*File.Exit*helpLocation:    Xvd/Xvd.html#filemenu


!********************************************
! Help on the Edit->Preferences Dialog
!********************************************
*PrefDialog*helpLocation:             Xvd/PreferencesDialog.html
*PrefDialog*menubar.Help*helpLocation:Xvd/PreferencesDialog.html 


!*********************************************
! Help on Edit->   tear-off menu
!*********************************************
*Edit.Undo*helpLocation:          Xvd/Xvd.html#editmenu
*Edit.Data Range*helpLocation:    Xvd/Xvd.html#editmenu
*Edit.Preferences*helpLocation:   Xvd/Xvd.html#editmenu


!********************************************
! Help on the Help menu
!********************************************
*menubar.Help*helpLocation:     Xvd/Xvd.html#helpmenu


!********************************************
! Help on the Help tear-off menu
!********************************************
*Help*On Help*helpLocation:     Xvd/Xvd.html#helpmenu
*Help*On Window*helpLocation:   Xvd/Xvd.html
*Help*On Keys*helpLocation:     Xvd/Keys.html 
*Help*On Version*helpLocation:  General/Version.html


!*********************************
! Help on the Tools tear-off menu
!*********************************
*Tools.Raw Hist*helpLocation:            Xvd/ToolsMenu.html#histogram
*Tools.Stretched Hist*helpLocation:      Xvd/ToolsMenu.html#histogram
*Tools.Stretch*helpLocation:             Xvd/ToolsMenu.html#stretch
*Tools.Cursor Stretch*helpLocation:      Xvd/ToolsMenu.html#cursor_stretch
*Tools.Lookup Table View*helpLocation:   Xvd/ToolsMenu.html#stretch_LUT
*Tools.Pan*helpLocation:                 Xvd/ToolsMenu.html#pan
*Tools.Mag*helpLocation:                 Xvd/ToolsMenu.html#magnify
*Tools.Zoom*helpLocation:                Xvd/ToolsMenu.html#zoom
*Tools.Pseudo*helpLocation:              Xvd/ToolsMenu.html#pseudo
*Tools.Pseudocolor View*helpLocation:    Xvd/ToolsMenu.html#pseudo_LUT
*Tools.PseudoMode*helpLocation:          Xvd/ToolsMenu.html#use_pseudo_table



!*********************************
! Help on the Tools->histogram
!*********************************
	! Raw Hist resource works for both the display and tear-off menu
*Raw Hist*helpLocation: 		Xvd/ToolsMenu.html#histogram
*Histogram*histBox*helpLocation: 	Xvd/ToolsMenu.html#histogram
*Histogram*menubar*helpLocation:	Xvd/ToolsMenu.html#histoptions


!***************************************
! Help on Tools->Histogram->Options Menu 
!***************************************
*StackNoBlend*helpLocation:		Hist/HistMenu.html#Presentation
*StackBlend*helpLocation: 		Hist/HistMenu.html#Presentation
*Row*helpLocation: 			Hist/HistMenu.html#Presentation
*Column*helpLocation: 			Hist/HistMenu.html#Presentation
*Show Stats*helpLocation: 		Hist/HistMenu.html#Annotation
*Show Axis*helpLocation: 		Hist/HistMenu.html#Annotation
*Horizontal HistGraph*helpLocation: 	Hist/HistMenu.html#HistOrientation
*Vertical HistGraph*helpLocation: 	Hist/HistMenu.html#HistOrientation
*Ascending Axis Values*helpLocation: 	Hist/HistMenu.html#AxesOrientation
*Descending Axis Values*helpLocation: 	Hist/HistMenu.html#AxesOrientation
*Spike*helpLocation: 			Hist/HistMenu.html#Spike
*Log-Scaled Axis*helpLocation:		Hist/HistMenu.html#Log-Scaled


!********************************************
! Help on the Tools->stretched histogram
!********************************************
	!Stretched Histogram only works for the display
*Stretched Histogram*helpLocation: 		Xvd/ToolsMenu.html#histogram
*Stretched Histogram*menubar*helpLocation:	Xvd/ToolsMenu.html#histoptions


!********************************************
! Help on the Tools->stretch dialog
!********************************************
*Stretch Dialog*helpLocation: 			Xvd/ToolsMenu.html#stretch
*Stretch Dialog*menubar.File*helpLocation:	Xvd/ToolsMenu.html#stretch_filemenu
*Stretch Dialog*menubar.Options*helpLocation:	Xvd/ToolsMenu.html#stretch_optionsmenu
*Stretch Dialog*menubar.Help*helpLocation:      Xvd/ToolsMenu.html#stretch
*Stretch Dialog*action_area.Help*helpLocation:  Xvd/ToolsMenu.html#stretch
*Stretch Dialog*Stretch*clip*helpLocation:	Stretch/Stretchtype.html#BitClip
*Stretch Dialog*Stretch*contour*helpLocation:	Stretch/Stretchtype.html#Contour
*Stretch Dialog*Stretch*ellipse*helpLocation:	Stretch/Stretchtype.html#Ellipse
*Stretch Dialog*Stretch*func*helpLocation:	Stretch/Stretchtype.html#Function
*Stretch Dialog*Stretch*gauss*helpLocation:	Stretch/Stretchtype.html#Gauss
*Stretch Dialog*Stretch*itable*helpLocation:	Stretch/Stretchtype.html#Itable
*Stretch Dialog*Stretch*linear*helpLocation:	Stretch/Stretchtype.html#Linear
*Stretch Dialog*Stretch*log*helpLocation:	Stretch/Stretchtype.html#Log
*Stretch Dialog*Stretch*percent*helpLocation:	Stretch/Stretchtype.html#Perc
*Stretch Dialog*Stretch*period*helpLocation:	Stretch/Stretchtype.html#Period
*Stretch Dialog*Stretch*power*helpLocation:	Stretch/Stretchtype.html#Power
*Stretch Dialog*Stretch*raw*helpLocation:	Stretch/Stretchtype.html#Raw
*Stretch Dialog*Stretch*smooth*helpLocation:	Stretch/Stretchtype.html#Smooth
*Stretch Dialog*Stretch*table*helpLocation:	Stretch/Stretchtype.html#Table
*Stretch Dialog*Stretch*alarm*helpLocation:	Stretch/Stretchtype.html#Alarm
*Stretch Dialog*Stretch*comp*helpLocation:	Stretch/Stretchtype.html#Comp
*Stretch Dialog*Stretch*off*helpLocation:	Stretch/Stretchtype.html#Off
*Stretch Dialog*Stretch*PostTableListDialog*helpLocation:	Stretch/Stretchtype.html#Table
*Stretch Dialog*Stretch*PostAlarmListDialog*helpLocation:	Stretch/Stretchtype.html#Alarm
*Stretch Dialog*Stretch.all*helpLocation:	Xvd/ToolsMenu.html#stretchtype
*Stretch Dialog*Stretch.red*helpLocation:	Xvd/ToolsMenu.html#stretchtype
*Stretch Dialog*Stretch.green*helpLocation:	Xvd/ToolsMenu.html#stretchtype
*Stretch Dialog*Stretch.blue*helpLocation:	Xvd/ToolsMenu.html#stretchtype
    ! Help for Options->View Percent Stretch Limits
*percValuesDialog*action_area.Help*helpLocation:Xvd/ToolsMenu.html#stretch_optio
nsmenu
    ! Help for Stretch->File->Load/Save menu options
*load.loadFile.Help:helpLocation:       Xvd/ToolsMenu.html#stretch_filemenu
*save.saveFile.Help:helpLocation:       Xvd/ToolsMenu.html#stretch_filemenu

 

!*****************************************************************************
! Help on the LUT window for both Tools->Stretch & Pseudocolor Tables displays
!*****************************************************************************
*lutBox*helpLocation:		Xvd/ToolsMenu.html#LUT



!********************************************
! Help on the Tools->Pan Tool display
!********************************************
*PanToolWindow*helpLocation: 	Xvd/ToolsMenu.html#pan


!********************************************
! Help on the Tools->Magnifying Glass display
!********************************************
*magInfo*helpLocation		Xvd/ToolsMenu.html#magnify


!********************************************
! Help on the Tools->Zoom display
!********************************************
*ZoomDialog*helpLocation: 	Xvd/ToolsMenu.html#zoom


!********************************************
! Help on Tools->Pseudo Dialog display
!********************************************
*Pseudocolor Dialog*helpLocation:       	Xvd/ToolsMenu.html#pseudo
*Pseudocolor Dialog*File*helpLocation:		Xvd/ToolsMenu.html#pseudo_filemenu
*Pseudocolor Dialog*Options*helpLocation: 	Xvd/ToolsMenu.html#pseudo_optionsmenu


!***********************************************
! Help on XVD display area
!***********************************************
*sideBar.cursorPositionView*helpLocation:   Xvd/Xvd.html#xydn
*sideBar.cursorDnView*helpLocation:         Xvd/Xvd.html#xydn
*sideBar.zoomMenu*helpLocation:             Xvd/ToolsMenu.html#zoom
*sideBar.Raw Hist*helpLocation:             Xvd/ToolsMenu.html#histogram
*sideBar.Lookup Table View*helpLocation:    Xvd/ToolsMenu.html#LUT





 

!******************************************************
!  Pseudo Color resources
!******************************************************
*Pseudo*rgbController*red*troughColor:   red
*Pseudo*rgbController*green*troughColor: green
*Pseudo*rgbController*blue*troughColor:  blue
*Pseudo*rgbController*scaleWidth:        256
*Pseudo*rgbController*scaleHeight:       20
*Pseudo*rgbController*showValue:         True
XVd*Pseudo*rgbController*red*foreground:    red
XVd*Pseudo*rgbController*green*foreground:  green
XVd*Pseudo*rgbController*blue*foreground:   blue

! Peudocolored wedge
*Pseudo*pseudoColor*wedgeView.colormapPolicy: HALF
*Pseudo*pseudoColor*wedgeView.grayLevels:     0
*Pseudo*pseudoColor*wedgeView.redLevels:      8
*Pseudo*pseudoColor*wedgeView.greenLevels:    8
*Pseudo*pseudoColor*wedgeView.blueLevels:     8

! Both wedges
*Pseudo*wedgeView.bwVisualType:		 use_24bit
*Pseudo*wedgeView.colorVisualType:       use_24bit
*Pseudo*wedgeView.pseudoVisualType:      use_24bit
*Pseudo*wedgeView.visualType:            use_24bit
*Pseudo*wedgeView.imageMode:             BW
*Pseudo*wedgeView.bwDither:              ORDERED
*Pseudo*wedgeView.colorDither:           ORDERED
*Pseudo*wedgeView.lutType:               RAW
*Pseudo*wedgeView.colormapPolicy:        HALF
*Pseudo*wedgeView.constrainPan:          BOTH
*Pseudo*wedgeView.imageHeight:           50
*Pseudo*wedgeView.imageWidth:            256
*Pseudo*wedgeView.viewHeight:            50
*Pseudo*wedgeView.viewWidth:             256
*Pseudo*wedgeView.grayLevels:            16
*Pseudo*wedgeView.redLevels:             0
*Pseudo*wedgeView.greenLevels:           0
*Pseudo*wedgeView.blueLevels:            0
*Pseudo*wedgeView.scrollBarDisplayPolicy:NEVER
*Pseudo*wedgeView.shadowThickness:       0
*Pseudo*wedgeView.highlightThickness:    0
*Pseudo*wedgeView*orientation:           HORIZONTAL
*Pseudo*wedgeView*nsteps:                256
*Pseudo*wedgeView*minPixelDN:            0
*Pseudo*wedgeView*maxPixelDN:            255
*Pseudo*wedgeView.traversalOn:    	 False

*Pseudo*wedgeView*translations: #override \n\
        ~Shift<Btn1Down>:               Input(mark_point, start) \n\
        ~Shift<Btn1Motion>:             Input(mark_point, drag) \n\
        ~Shift<Btn1Up>:                 Input(mark_point, release) \n\
        ~Shift<Btn2Down>:               Input(mark_interval, start) \n\
        ~Shift<Btn2Motion>:             Input(mark_interval, drag)

*Pseudo*cursor:                         center_ptr
*Pseudo*cursorForeground:               green

*Pseudo.interpolationFrame.interpolationFrameLabel.labelString: Interpolation
*Pseudo*interpolationChooser.none.labelString:        None
*Pseudo*interpolationChooser.flat.labelString:        Flat
*Pseudo*interpolationChooser.linear.labelString:      Linear
*Pseudo*interpolationChooser.cubs.labelString:        CubS

*Pseudo.dnValueView*field.traversalOn:                False
*Pseudo.dnValueView*field.columns:                    5
*Pseudo.dnValueView*field.editable:                   False
*Pseudo.dnValueView*field.cursorPositionVisible:      False
*Pseudo.dnValueView*label.labelString:                DN Value:
*Pseudo.dnValueView*shadowThickness:                  0


*PseudoMode.labelString:                              Use Pseudocolor Tables

*File*loadWin.labelString:                            Load IBIS File
*File*saveWin.labelString:                            Save as IBIS File

*Options.labelString:                                 Options
*Options.SetDeferredCmd.labelString:                  Defer Command Execution
*Options.ClearMarksCmd.labelString:                   Clear All Marks

*numPseudoFiles:                8

*file1.filename:                ps_bw.ibis-2
*file2.filename:                ps1.ibis-2
*file3.filename:                ps2.ibis-2
*file4.filename:                ps3.ibis-2
*file5.filename:                ps4.ibis-2
*file6.filename:                ps5.ibis-2
*file7.filename:                ps6.ibis-2
*file8.filename:                ps7.ibis-2

*dirUNIX:                       $V2DATA/gui/
	! It is possible to set-up different path for different files 
	! by specifying resource *fileX.dirUNIX where X=1..numPseudoFiles.
	! Make sure there is a slash at the end of the string.
*dirVMS:			v2$data:[gui]

*file1.labelString:             Load BW Ramp 
*file2.labelString:             Load Table 1
*file3.labelString:             Load Table 2
*file4.labelString:             Load Table 3
*file5.labelString:             Load Table 4
*file6.labelString:             Load Table 5
*file7.labelString:             Load Table 6
*file8.labelString:             Load Table 7

XVd*Pseudo.RGBView.field1.foreground: red
XVd*Pseudo.RGBView.field2.foreground: green
XVd*Pseudo.RGBView.field3.foreground: blue

XVd*Pseudo.RGBView.label1.foreground: red
XVd*Pseudo.RGBView.label2.foreground: green
XVd*Pseudo.RGBView.label3.foreground: blue

*Pseudo.RGBView*columns: 3
*Pseudo.RGBView*shadowThickness: 0
*Pseudo.RGBView*traversalOn: False

*Pseudo.RGBView.orientation: HORIZONTAL
*Pseudo.RGBView.numColumns: 6
*Pseudo.RGBView.spacing: 0

*Pseudo.RGBView.label1.labelString: Red:
*Pseudo.RGBView.label2.labelString: Green:
*Pseudo.RGBView.label3.labelString: Blue:

*Pseudo*markFrameLabel.labelString: Edit Current Mark

!----------------------------------------------------------------------------
!
! Mnemonics and accelerators for Pseudocolor...
!
!----------------------------------------------------------------------------
*Pseudocolor Dialog*File.mnemonic: F

*File.loadWin.mnemonic: L
*File.loadWin.accelerator: Ctrl<Key>L
*File.loadWin.acceleratorText: Ctrl+L

*File.saveWin.mnemonic: S
*File.saveWin.accelerator: Ctrl<Key>S
*File.saveWin.acceleratorText: Ctrl+S


*Pseudocolor Dialog*Options.mnemonic: O

*Options.SetDeferredCmd.mnemonic: D
*Options.SetDeferredCmd.accelerator: Ctrl<Key>D
*Options.SetDeferredCmd.acceleratorText: Ctrl+D

*Options.PseudoMode.mnemonic: U
*Options.PseudoMode.accelerator: Ctrl<Key>U
*Options.PseudoMode.acceleratorText: Ctrl+U

*Options.ClearMarksCmd.mnemonic: C
*Options.ClearMarksCmd.accelerator: Ctrl<Key>C
*Options.ClearMarksCmd.acceleratorText: Ctrl+C

*Options.file1.mnemonic: B
*Options.file1.accelerator: Ctrl<Key>B
*Options.file1.acceleratorText: Ctrl+B

*Options.file2.mnemonic: 1
*Options.file2.accelerator: Ctrl<Key>1
*Options.file2.acceleratorText: Ctrl+1

*Options.file3.mnemonic: 2
*Options.file3.accelerator: Ctrl<Key>2
*Options.file3.acceleratorText: Ctrl+2

*Options.file4.mnemonic: 3
*Options.file4.accelerator: Ctrl<Key>3
*Options.file4.acceleratorText: Ctrl+3

*Options.file5.mnemonic: 4
*Options.file5.accelerator: Ctrl<Key>4
*Options.file5.acceleratorText: Ctrl+4

*Options.file6.mnemonic: 5
*Options.file6.accelerator: Ctrl<Key>5
*Options.file6.acceleratorText: Ctrl+5

*Options.file7.mnemonic: 6
*Options.file7.accelerator: Ctrl<Key>6
*Options.file7.acceleratorText: Ctrl+6

*Options.file8.mnemonic: 7
*Options.file8.accelerator: Ctrl<Key>7
*Options.file8.acceleratorText: Ctrl+7
!----------------------------------------------------------------------------

!***********************************************
! Latitude/Longitude Resources
!***********************************************
*cursorLatLonView*columns:                    12

*cursorLatLonView*editable:                   False
*cursorLatLonView*cursorPositionVisible:      False

*cursorLatLonView*LatLabel.labelString:            Lat:
*cursorLatLonView*LonLabel.labelString:            Lon:
!----------------------------------------------------------------------------

!***********************************************
! Label Display Resources
!***********************************************
*File.Save Label.labelString: Save Label...
*File.Save Label.mnemonic: S
*File.Save Label.accelerator: Ctrl<Key>S
*File.Save Label.acceleratorText: Ctrl+S
*Options.Clear Output.mnemonic: C
*Options.Clear Output.accelerator: Ctrl<Key>C
*Options.Clear Output.acceleratorText: Ctrl+C
*Options.Clear Output On Every Run.mnemonic: E
*Options.Clear Output On Every Run.accelerator: Ctrl<Key>E
*Options.Clear Output On Every Run.acceleratorText: Ctrl+E
*Options.Find.mnemonic: F
*Options.Find.accelerator: Ctrl<Key>F
*Options.Find.acceleratorText: Ctrl+F
*Options.Find Next.mnemonic: N
*Options.Find Next.accelerator: Ctrl<Key>N
*Options.Find Next.acceleratorText: Ctrl+N

*text.editMode:                     XmMULTI_LINE_EDIT
*text.rows:                         20
*text.columns:                      60
*text.cursorPositionVisible:        False
*text.editable:                     False

$!-----------------------------------------------------------------------------
$ create xvd.dict
machine null_machine:local:null

compute_module xvd:xvd:null_machine:
	INPUT ~VICARFILE_1D Filename:
	ParamBox=xvd;icon=xvd
$ Return
$!#############################################################################
