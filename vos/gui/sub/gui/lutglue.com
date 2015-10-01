$!****************************************************************************
$!
$! Build proc for MIPL module lutglue
$! VPACK Version 1.8, Thursday, January 16, 1997, 14:49:22
$!
$! Execute by entering:		$ @lutglue
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
$ write sys$output "*** module lutglue ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to lutglue.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("lutglue.imake") .nes. ""
$   then
$      vimake lutglue
$      purge lutglue.bld
$   else
$      if F$SEARCH("lutglue.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lutglue
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lutglue.bld "STD"
$   else
$      @lutglue.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lutglue.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lutglue.com -mixed -
	-s LutToImageWidgetGlue.cc -
	-i lutglue.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create LutToImageWidgetGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////
// LutToImageWidgetGlue.cc: class that serves as a "glue" class between a
// set of LUT objects and an Image Widget instance.  Whenever the LUT
// changes, update() writes that LUT out to the image widget.
// This class creates no widget, therefore it should never be managed.
// Note:  if the image changes modes, the LUTs need to be reloaded,
// since the BW stretch goes in a different place than the color stretch.
// This is handled automatically by attaching a mode-changed callback
// to the widget.
//////////////////////////////////////////////////////////
#include "LutToImageWidgetGlue.h"
#include "Lut.h"
#include "XvicImage.h"

LutToImageWidgetGlue::LutToImageWidgetGlue(Lut *lutR, Lut *lutG, Lut *lutB,
		Widget iw, Boolean isStretch)
	: LutView ( "iwglue", lutR, lutG, lutB)
{
   _iw = iw;
   _isStretch = isStretch;

   if (_lut)
      _lut ->attachView(this);
   if (_lut1)
      _lut1->attachView(this);
   if (_lut2)
      _lut2->attachView(this);

   XtAddCallback(_iw, XvicNvisibleAreaCallback,
		&LutToImageWidgetGlue::modeChangedCallback, (XtPointer) this);
}

LutToImageWidgetGlue::LutToImageWidgetGlue(Lut *lut,
                Widget iw, Boolean isStretch)
        : LutView ( "iwglue", lut)
{
   _iw = iw;
   _isStretch = isStretch;

   if (_lut)
      _lut ->attachView(this);

   XtAddCallback(_iw, XvicNvisibleAreaCallback,
                &LutToImageWidgetGlue::modeChangedCallback, (XtPointer) this);
}

LutToImageWidgetGlue::~LutToImageWidgetGlue()
{
   XtRemoveCallback(_iw, XvicNvisibleAreaCallback,
                &LutToImageWidgetGlue::modeChangedCallback, (XtPointer) this);
}

void LutToImageWidgetGlue::update()
{
   unsigned char mode;
   int *arrayR = NULL;
   int *arrayG = NULL;
   int *arrayB = NULL;

   if (_lut)
      arrayR = _lut->getAsArray();
   if (_lut1)
      arrayG = _lut1->getAsArray();
   if (_lut2)
      arrayB = _lut2->getAsArray();

   // Get the color/BW mode so we can determine which LUTs to set

   XtVaGetValues(_iw, XvicNimageMode, &mode, NULL);

   // Now set the stretch in the image widget.

   if (mode == XvicCOLOR) {
      if (_isStretch)
         XvicImageSetColorLUT(_iw, arrayR, arrayG, arrayB);
      // Pseudocolor table in COLOR mode has no effect
   }
   else {			// BW mode
      if (_isStretch)
         XvicImageSetMonoLUT(_iw, arrayR);	// stretch table
      else
         XvicImageSetColorLUT(_iw, arrayR, arrayG, arrayB); // pseudocolor table
   }
}

void LutToImageWidgetGlue::modeChangedCallback(Widget, XtPointer clientData,
						       XtPointer callData)
{
   LutToImageWidgetGlue *obj = (LutToImageWidgetGlue *)clientData;
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;

   // We do this check here instead of in modeChanged() for efficiency,
   // because this callback is called many times (like once per pan).
   // Also, it allows us to just call the existing update() function.

   if (cb->reason != XvicCR_VISIBLE_AREA || !(cb->flags & XvicMODE_CHANGED))
      return;

   obj->update();
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lutglue.imake
#define SUBROUTINE lutglue
#define MODULE_LIST LutToImageWidgetGlue.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
