$!****************************************************************************
$!
$! Build proc for MIPL module lutglue_h
$! VPACK Version 1.8, Thursday, January 16, 1997, 14:49:22
$!
$! Execute by entering:		$ @lutglue_h
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
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$ write sys$output "*** module lutglue_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to lutglue_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("lutglue_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @lutglue_h.bld "STD"
$   else
$      @lutglue_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lutglue_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lutglue_h.com -mixed -
	-s LutToImageWidgetGlue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create LutToImageWidgetGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// LutToImageWidgetGlue: class that serves as a "glue" class between a
// set of LUT objects and an Image Widget instance.  Whenever the LUT
// changes, update() writes that LUT out to the image widget.
// This class creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef LUTTOIMAGEWIDGETGLUE_H
#define LUTTOIMAGEWIDGETGLUE_H
#include "LutView.h"

class LutToImageWidgetGlue : public LutView {

 private:

   static void modeChangedCallback(Widget, XtPointer, XtPointer);

 protected:

   Widget _iw;
   Boolean _isStretch;		// True for stretch table, False for pseudocolor

 public:

   LutToImageWidgetGlue (Lut *lutR, Lut *lutG, Lut *lutB,
		Widget iw, Boolean isStretch);

   LutToImageWidgetGlue (Lut *lut, Widget iw, Boolean isStretch);

   virtual ~LutToImageWidgetGlue();

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "LutToImageWidgetGlue"; }

};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
