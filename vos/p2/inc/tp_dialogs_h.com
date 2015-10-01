$!****************************************************************************
$!
$! Build proc for MIPL module tp_dialogs_h
$! VPACK Version 1.9, Wednesday, February 28, 2001, 15:29:25
$!
$! Execute by entering:		$ @tp_dialogs_h
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
$ write sys$output "*** module tp_dialogs_h ***"
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
$ write sys$output "Invalid argument given to tp_dialogs_h.com file -- ", primary
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
$   if F$SEARCH("tp_dialogs_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @tp_dialogs_h.bld "STD"
$   else
$      @tp_dialogs_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_dialogs_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_dialogs_h.com -mixed -
	-s TpDisplayModeDialog.h TpPointEditorOptsDialog.h -
	   TpPointSymbolsDialog.h TpPointTagsDialog.h TpCursorSymbolsDialog.h -
	   TpAutofindDialog.h TpMatchModeDialog.h TpAutofindResultsDialog.h -
	   TpMatchModeResultsDialog.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpDisplayModeDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpDisplayModeDialog.h: Dialog containing display mode values.
////////////////////////////////////////////////////////////
#ifndef TpDisplayModeDialog_H
#define TpDisplayModeDialog_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpDisplayer;

class TpDisplayModeDialog : public CustomDialog {

  protected:

    TpDisplayer *_displayer;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpDisplayModeDialog(const char *name, TpDisplayer *);

    virtual Widget createWorkArea(Widget);

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointEditorOptsDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPointEditorOptsDialog.h: Dialog containing point editor options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINTEDITOROPTSDIALOG_H
#define TPPOINTEDITOROPTSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;

class TpPointEditorOptsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpPointEditorOptsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointSymbolsDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPointSymbolsDialog.h: Dialog containing point editor options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINTSYMBOLSDIALOG_H
#define TPPOINTSYMBOLSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpSetPointSymbolCmd;
class TpSetPointSizeCmd;
class TpColorCodeCmd;

class TpPointSymbolsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager; 

    TpSetPointSymbolCmd *_symbolCmd[6];
    TpSetPointSizeCmd *_sizeCmd[6];
    TpColorCodeCmd *_colorCodeCmd[2];
    Cmd *_colorCmd;
    Cmd *_colorSelCmd;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpPointSymbolsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

    virtual void post ();
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointTagsDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPointTagsDialog.h: Dialog containing point tag options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINTTAGSDIALOG_H
#define TPPOINTTAGSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;

class TpPointTagsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpPointTagsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpCursorSymbolsDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpCursorSymbolsDialog.h: Dialog containing cursor symbol options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPCURSORSYMBOLSDIALOG_H
#define TPCURSORSYMBOLSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpDisplayer;

class TpCursorSymbolsDialog : public CustomDialog {

  protected:

    TpDisplayer *_displayer;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpCursorSymbolsDialog(const char *name, TpDisplayer *);

    virtual Widget createWorkArea(Widget);

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutofindDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAutofindDialog.h: Dialog containing autofind values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOFINDDIALOG_H
#define TPAUTOFINDDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpSetAutofindCmd;
class CmdList;

class TpAutofindDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;
    CmdList *_findRadioList;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpAutofindDialog(const char *name, TpMatchManager *, CmdList *);

    virtual Widget createWorkArea(Widget);

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchModeDialog.h: Dialog containing Match Mode values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODEDIALOG_H
#define TPMATCHMODEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpSetMatchModeCmd;
class CmdList;
class Cmd;

class TpMatchModeDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;
    CmdList *_modeRadioList;

    Cmd *_setMatchModeValuesCmd;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpMatchModeDialog(const char *name, TpMatchManager *, CmdList *);

    virtual Widget createWorkArea(Widget);

    Cmd *getSetMatchModeValuesCmd() { return _setMatchModeValuesCmd; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutofindResultsDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAutofindResultsDialog.h: Dialog containing autofind mode values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOFINDRESULTSDIALOG_H
#define TPAUTOFINDRESULTSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpAutofindResults;

class TpAutofindResultsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    TpAutofindResults *_autofindResults;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpAutofindResultsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

    void setValues(double [6]);
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeResultsDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchModeResultsDialog.h: Dialog containing matchMode mode values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODERESULTSDIALOG_H
#define TPMATCHMODERESULTSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpMatchModeResults;

class TpMatchModeResultsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    TpMatchModeResults *_matchModeResults;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpMatchModeResultsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

    void setValues(float [5]);

    Boolean isDumpToStdout();   // True if we should dump results to stdout too
                                // We should really dump in here, but for
                                // headers and such we must dump outside.

};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
