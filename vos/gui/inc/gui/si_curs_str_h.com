$!****************************************************************************
$!
$! Build proc for MIPL module si_curs_str_h
$! VPACK Version 1.8, Tuesday, May 20, 1997, 20:00:55
$!
$! Execute by entering:		$ @si_curs_str_h
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
$ write sys$output "*** module si_curs_str_h ***"
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
$ write sys$output "Invalid argument given to si_curs_str_h.com file -- ", primary
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
$   if F$SEARCH("si_curs_str_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @si_curs_str_h.bld "STD"
$   else
$      @si_curs_str_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_curs_str_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_curs_str_h.com -mixed -
	-s SiCursorStretchInterface.h SiCursorStretchEnableCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiCursorStretchInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// SiCursorStretchInterface.h: Cursor-controlled linear stretch.
// Vertical motion controls contrast (slope of line) while horizontal
// motion controls brightness (y-intercept of line)
// Although this is a CmdInterface, there is no specific screen
// control for it... merely turn it on and it uses cursor callbacks
// on the IW and turns itself off.
////////////////////////////////////////////////////////////////////////
#ifndef SICURSORSTRETCHINTERFACE
#define SICURSORSTRETCHINTERFACE
#include "CmdInterface.h"
#include "StretchValue.h"

class CallbackCompressor;

class SiCursorStretchInterface : public CmdInterface {

 private:

   static void cursorMovedCallback(Widget, XtPointer, XtPointer);

 protected: 

   static Pixmap _cursorPixmap, _cursorMask;

   StretchValue *_undoStretchValue;	// Saved for undo (must save original,
					// not each cursor increment)

   Widget _iw;				// image widget we're talking to

   Boolean _saveTrackFC;
   char *_saveCursor;
   unsigned char _saveCursorMode;
   double _saveCursX, _saveCursY;
   CallbackCompressor *_cb_compress;

   void cursorMoved(XtPointer);
   void setLocationFromStretch();

 public:

   SiCursorStretchInterface(Widget, Cmd *, Widget iw);
   virtual ~SiCursorStretchInterface();

   virtual void activate();		// overrides CmdInterface
   virtual void deactivate();		// ditto
   Boolean active() { return _active; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCursorStretchEnableCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiCursorStretchEnableCmd - Enable or disable the cursor stretch.
// This command has a most unusual Undo... undo does not turn the
// command back on, it ensures that the command is off then resets
// the stretch back to what it was when it was turned on.
////////////////////////////////////////////////////////////////

#ifndef SICURSORSTRETCHENABLECMD_H
#define SICURSORSTRETCHENABLECMD_H
#include "Cmd.h"

class SiCursorStretchInterface;
class StretchValue;

class SiCursorStretchEnableCmd : public Cmd {

 protected:

   SiCursorStretchInterface *_interface;
   StretchValue *_saveStretchForUndo;
   Cmd *_stretchCmd;			// used for undo

 public:

   SiCursorStretchEnableCmd(const char *name, int active,
			SiCursorStretchInterface *csif, Cmd *stretchCmd);
   virtual ~SiCursorStretchEnableCmd();

   virtual void doit();
   virtual void undoit();

};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
