$!****************************************************************************
$!
$! Build proc for MIPL module spt_multifilesel_h
$! VPACK Version 1.8, Friday, May 19, 1995, 11:46:23
$!
$! Execute by entering:		$ @spt_multifilesel_h
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
$ write sys$output "*** module spt_multifilesel_h ***"
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
$ write sys$output "Invalid argument given to spt_multifilesel_h.com file -- ", primary
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
$   if F$SEARCH("spt_multifilesel_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @spt_multifilesel_h.bld "STD"
$   else
$      @spt_multifilesel_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spt_multifilesel_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spt_multifilesel_h.com -mixed -
	-s SptMultiFileSelView.h SptParamMultiFileSel.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SptMultiFileSelView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// SptMultiFileSelView - View object for Multiple File Selection dialog
////////////////////////////////////////////////////////////////////////
#ifndef SPTMULTIFILESELVIEW_H
#define SPTMULTIFILESELVIEW_H

#include "SptDirectBase.h"

class SptDirectValueCmd;
class FileSelBox;
class MainWindow;

class DropSite;

class SptMultiFileSelView : public SptDirectBase {
 private:

 protected:
   FileSelBox *_fileSelBox;
   SptDirectValueCmd *_stringValueCmd;

   DropSite *_dropSite;

   virtual void displayValue();
   virtual void displayValue(char *);

 public:
   SptMultiFileSelView(Widget, char *, SptParamBase *, MainWindow *mw = NULL);

   virtual void paramValueChanged();
   virtual void setDeferredExec(CmdList *);

   virtual void manage();
   virtual void unmanage();
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SptParamMultiFileSel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// SptParamMultiFileSel - Parameter class for a multiple file selection
// dialog (like on xvd).  Filename may come from user or from dataflow.
// When a new filename arrives, the given command is executed if it is
// non-NULL.  This command receives a dynamically-allocated filename string.
////////////////////////////////////////////////////////////////////////
#ifndef SPTPARAMMULTIFILESEL_H
#define SPTPARAMMULTIFILESEL_H

#include "SptInputParamBase.h"

class MainWindow;

class SptParamMultiFileSel : public SptInputParamBase {
 private:

 protected:

   MainWindow *_mainWindow;		// So FileSelBox can pop down window

   virtual void doLayout(Widget parent, char *name);

   virtual void createDirectView(Widget parent);
   virtual void createShowValueCmd();
   virtual void createModeMenu(Widget parent, char *name);

   SptParamMultiFileSel(char *name, Cmd *cmd=NULL, DA_ParamID param=NULL_ID,
			MainWindow *mainWindow=NULL)
	: SptInputParamBase(name, FALSE, cmd, param)
		{ _mainWindow = mainWindow; }
 public:

   static SptParamMultiFileSel *create(Widget parent, char *name,
		Cmd *cmd = NULL, DA_ParamID param = NULL_ID,
		MainWindow *mainWindow = NULL)
	{  SptParamMultiFileSel *ptr=new SptParamMultiFileSel(name, cmd, param,
								mainWindow);
	   if (ptr) ptr->initialize(parent, ParamModeDirect);
	   return ptr;
	}
};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
