$!****************************************************************************
$!
$! Build proc for MIPL module sg_misc_h
$! VPACK Version 1.8, Wednesday, October 01, 1997, 16:58:24
$!
$! Execute by entering:		$ @sg_misc_h
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
$ write sys$output "*** module sg_misc_h ***"
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
$ write sys$output "Invalid argument given to sg_misc_h.com file -- ", primary
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
$   if F$SEARCH("sg_misc_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @sg_misc_h.bld "STD"
$   else
$      @sg_misc_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_misc_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_misc_h.com -mixed -
	-s SgKeyinCmdInterface.h SgKeyinDialog.h SgCmdDialog.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgKeyinCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////
// SgKeyinCmdInterface:  Command interface to get string value for cmd.
// Initial values come from Cmd object.  This class is not specific
// to the sage client library and can be reused if needed.
////////////////////////////////////////////////////////
#ifndef SGKEYINCMDINTERFACE_H
#define SGKEYINCMDINTERFACE_H
#include "CmdInterface.h"

class KeyinView;

class SgKeyinCmdInterface : public CmdInterface {

 protected: 

   static String _defaults[];

   KeyinView *_keyinView;

 public:

   SgKeyinCmdInterface(Widget, Cmd *);
   virtual ~SgKeyinCmdInterface();

   virtual void executeCmd(XtPointer);

   virtual void setValue(CmdValue);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgKeyinDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SgKeyinDialog.h: A dialog class used to take single string
//      input from the user
////////////////////////////////////////////////////////////////

#ifndef SGKEYINDIALOG_H
#define SGKEYINDIALOG_H

#include "SgCmdDialog.h"
#include "SgKeyinCmdInterface.h"

class SgKeyinDialog : public SgCmdDialog {

 protected:

   Cmd *_command;
   CmdInterface *_ci;

   virtual void setCmdIfDeferredExec() { }

   virtual CmdInterface *createCmdInterface(Widget, Cmd *);

 public:
 
   SgKeyinDialog(const char *, Cmd *);

   virtual ~SgKeyinDialog() { };

   virtual CmdInterface *getCmdInterface() { return _ci; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgCmdDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SgCmdDialog.h: This is an abstract class that implements a 
// dialog that holds a command interface.  The subclasses must
// provide the *command interface*. The *command* itself is created
// outside the class and is passed in via the constructor.
////////////////////////////////////////////////////////////////

#ifndef SGCMDDIALOG_H
#define SGCMDDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class CmdInterface;

class SgCmdDialog : public CustomDialog {


 private:

   CmdInterface *_cmdInterface;
   Cmd *_cmd;

   virtual CmdInterface *createCmdInterface(Widget, Cmd *) = 0;
   virtual void setCmdIfDeferredExec();

   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

 public:

   SgCmdDialog(const char *name, Cmd *cmd, ButtonState showOk,
	       ButtonState showApply, ButtonState showReset,
	       ButtonState showCancel, ButtonState showHelp);

   virtual ~SgCmdDialog();

   virtual void resetDialog(Cmd *);

   virtual Widget createWorkArea(Widget);
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
