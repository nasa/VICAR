$!****************************************************************************
$!
$! Build proc for MIPL module datarange_h
$! VPACK Version 1.8, Wednesday, August 14, 1996, 15:40:14
$!
$! Execute by entering:		$ @datarange_h
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
$ write sys$output "*** module datarange_h ***"
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
$ write sys$output "Invalid argument given to datarange_h.com file -- ", primary
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
$   if F$SEARCH("datarange_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @datarange_h.bld "STD"
$   else
$      @datarange_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create datarange_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack datarange_h.com -mixed -
	-s DataRangeDialog.h DataRangeAutoCmd.h DataRangeValueCmd.h -
	   ImageToRangeDialogGlue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DataRangeDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// DataRangeDialog.h:  Create the Data Range dialog.
////////////////////////////////////////////////////////////////
#ifndef DATARANGEDIALOG_H
#define DATARANGEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class StringKeyinInterface;

class DataRangeDialog: public CustomDialog {

 protected:

   Cmd *_minAutoCmd;
   Cmd *_maxAutoCmd;
   Cmd *_minValueCmd;
   Cmd *_maxValueCmd;
   StringKeyinInterface *_minInterface;
   StringKeyinInterface *_maxInterface;
   double _saveMin, _saveMax;

   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

 public:

   DataRangeDialog(const char *name, Cmd *, Cmd *, Cmd *, Cmd *);

   virtual Widget createWorkArea(Widget);

   virtual void setDataRange(double min, double max);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataRangeAutoCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// DataRangeAutoCmd.h: Set or clear automatic determination of
// data range in the image model.
/////////////////////////////////////////////////////////////
#ifndef DATARANGEAUTOCMD_H
#define DATARANGEAUTOCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>		// Only for Boolean!!

class ImageData;

class DataRangeAutoCmd : public Cmd {

 protected:

   CmdValue _lastValue;		// for undo
   ImageData *_image;
   Boolean _isMax;		// True: max range, False: min range

   virtual void doit();   
   virtual void undoit(); 

 public:

   DataRangeAutoCmd(const char *, int, ImageData *, Boolean isMax);
   void SetValue(CmdValue value) { _value = value; }
   virtual const char *const className () { return "DataRangeAutoCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataRangeValueCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// DataRangeValueCmd.h: Sets the data range to the value specified
// in the string CmdValue.  Also, turns off auto mode.
/////////////////////////////////////////////////////////////
#ifndef DATARANGEVALUECMD_H
#define DATARANGEVALUECMD_H
#include "Cmd.h"
#include <Xm/Xm.h>		// Only for Boolean!!

class ImageData;

class DataRangeValueCmd : public Cmd {

 protected:

   double _lastValue;		// for undo
   CmdValue _lastAuto;		// for undo

   ImageData *_image;
   Cmd *_autoCmd;
   Boolean _isMax;		// True: max range, False: min range

   virtual void doit();   
   virtual void undoit(); 

 public:

   DataRangeValueCmd(const char *, int, ImageData *, Cmd *, Boolean isMax);

   virtual void freeValue(CmdValue value) { if (value) XtFree((char *)value); }

   virtual const char *const className () { return "DataRangeValueCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToRangeDialogGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToRangeDialogGlue: class that serves as a "glue" class between an
// ImageData object and the Data Range dialog.  The class is a View to
// ImageData, so whenever it receives an update() from ImageData,
// it tells the RangeDialog to update its min/max value displays.
// This class, even though it's a UIComponent, creates no widget,
// therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETORANGEDIALOGGLUE_H
#define IMAGETORANGEDIALOGGLUE_H
#include "BasicImageView.h"

class ImageData;
class DataRangeDialog;

class ImageToRangeDialogGlue : public BasicImageView {

 private:

 protected:
   DataRangeDialog *_dialog;

 public:

   ImageToRangeDialogGlue (ImageData *model, DataRangeDialog *dialog);

   virtual ~ImageToRangeDialogGlue();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToRangeDialogGlue"; }

};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
