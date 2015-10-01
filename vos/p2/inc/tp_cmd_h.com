$!****************************************************************************
$!
$! Build proc for MIPL module tp_cmd_h
$! VPACK Version 1.9, Tuesday, August 17, 2010, 16:29:46
$!
$! Execute by entering:		$ @tp_cmd_h
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
$ write sys$output "*** module tp_cmd_h ***"
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
$ write sys$output "Invalid argument given to tp_cmd_h.com file -- ", primary
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
$   if F$SEARCH("tp_cmd_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @tp_cmd_h.bld "STD"
$   else
$      @tp_cmd_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_cmd_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_cmd_h.com -mixed -
	-s TpCloseCmd.h TpContrastCmd.h TpDeletePointCmd.h TpExitStatusCmd.h -
	   TpListPointsCmd.h TpLoadConfigCmd.h TpLoadImageCmd.h -
	   TpRemoveImageCmd.h TpLoadPointFileCmd.h TpClosePointFileCmd.h -
	   TpNumDisplaysCmd.h TpQuitCmd.h TpRotateImageCmd.h -
	   TpSaveAndExitCmd.h TpSaveConfigAsCmd.h TpSaveConfigCmd.h -
	   TpSavePointCmd.h TpSavePointFileAsCmd.h TpSavePointFileCmd.h -
	   TpSetAutofindCmd.h TpSetPointSizeCmd.h TpSetPointSymbolCmd.h -
	   TpShiftCmd.h TpSwapLockCmd.h TpWritePointsCmd.h -
	   TpSetPointColorCmd.h TpSetPointColorSelCmd.h TpContrastValue.h -
	   TpSetMatchModeCmd.h TpSetCursorSymbolCmd.h TpSetCursorColorCmd.h -
	   TpSetMatchModeValuesCmd.h TpPrintCmd.h TpRedoMatchIdsCmd.h -
	   TpAutoSyncPointsCmd.h TpCheckGenQualUniqueCmd.h -
	   TpSetTagPositionCmd.h TpColorCodeCmd.h TpSetMatchIdOffsetCmd.h -
	   TpSetMatchIdNextCmd.h TpShowPointLabelsCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpCloseCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// TpCloseCmd.h: Select the point file then call the quit command
// so that the application may exit.
////////////////////////////////////////////////////////////
#ifndef TPCLOSECMD_H
#define TPCLOSECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpCloseCmd : public NoUndoCmd {

  private:

    static void saveCallback(void *clientData);
    static void noSaveCallback(void *clientData);

    void save();
    void noSave();

  protected:

    TpMatchManager *_matchManager;
    Cmd *_quitCmd;
    
    virtual void doit();
    
  public:

    TpCloseCmd(const char *, int, TpMatchManager *, Cmd *);

    virtual const char *const className () { return "TpCloseCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpContrastCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpContrastCmd.h
///////////////////////////////////////////////////////////////////
#ifndef TPCONTRASTCMD_H
#define TPCONTRASTCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class ImageData;
class TpWedgeOverlayView;

class TpContrastCmd : public Cmd {

  protected:

    Widget _iw;
    Widget _ziw;
    Widget _pan;

    void doit();
    void undoit() { }

  public:

    TpContrastCmd(const char *name, int active, Widget iw, Widget ziw, Widget pan,
		      ImageData *image);
    virtual ~TpContrastCmd() { }

    virtual const char *const className () { return "TpContrastCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpDeletePointCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpDeletePointCmd.h: Deletes currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPDELETEPOINTCMD_H
#define TPDELETEPOINTCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpDeletePointCmd : public Cmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();
    virtual void undoit();

  public:

    TpDeletePointCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpDeletePointCmd() { };

    virtual const char *const className () { return "TpDeletePointCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpExitStatusCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpExitStatusCmd.cc: Set application exit status.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPEXITSTATUSCMD_H
#define TPEXITSTATUSCMD_H
#include "NoUndoCmd.h"

class TpExitStatusCmd : public NoUndoCmd {

  protected:

    virtual void doit();
    
  public:

    TpExitStatusCmd(const char *name, int active);

    virtual const char *const className () { return "TpExitStatusCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpListPointsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpListPointsCmd.h: Outputs points to stdout.
/////////////////////////////////////////////////////////////
#ifndef TPLISTPOINTSCMD_H
#define TPLISTPOINTSCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpListPointsCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();

  public:

    TpListPointsCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpListPointsCmd() { };

    virtual const char *const className () { return "TpListPointsCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpLoadConfigCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpLoadConfigCmd.h: Load config file.
////////////////////////////////////////////////////////////
#ifndef TPLOADCONFIGCMD_H
#define TPLOADCONFIGCMD_H
#include "NoUndoCmd.h"

class TpWindow;

class TpLoadConfigCmd : public NoUndoCmd {

  protected:

    TpWindow *_window;
    
    virtual void doit();
    
  public:

    TpLoadConfigCmd(const char *, int, TpWindow *);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpLoadConfigCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpLoadImageCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpLoadImageCmd.h: 
////////////////////////////////////////////////////////////
#ifndef TPLOADIMAGECMD_H
#define TPLOADIMAGECMD_H
#include "Cmd.h"

class TpDisplayer;

class TpLoadImageCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;
    
    virtual void doit();
    virtual void undoit();
    
  public:

    TpLoadImageCmd(const char *, int, TpDisplayer *);

    virtual const char *const className () { return "TpLoadImageCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpRemoveImageCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpRemoveImageCmd.h: 
////////////////////////////////////////////////////////////
#ifndef TPREMOVEIMAGECMD_H
#define TPREMOVEIMAGECMD_H
#include "WarnNoUndoCmd.h"

class TpDisplayer;

class TpRemoveImageCmd : public WarnNoUndoCmd {

  protected:

    TpDisplayer *_displayer;
    
    virtual void doit();
    
  public:

    TpRemoveImageCmd(const char *, int, TpDisplayer *);

    virtual const char *const className () { return "TpRemoveImageCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpLoadPointFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpLoadPointFileCmd.h: Load ibis-2 point file.
////////////////////////////////////////////////////////////
#ifndef TPLOADPOINTFILECMD_H
#define TPLOADPOINTFILECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpLoadPointFileCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;
    
    virtual void doit();
    
  public:

    TpLoadPointFileCmd(const char *, int, TpMatchManager *);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpLoadPointFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpClosePointFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpClosePointFileCmd.h: Close ibis-2 point file.
////////////////////////////////////////////////////////////
#ifndef TPCLOSEPOINTFILECMD_H
#define TPCLOSEPOINTFILECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpClosePointFileCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;
    
    virtual void doit();
    
  public:

    TpClosePointFileCmd(const char *, int, TpMatchManager *);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpClosePointFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpNumDisplaysCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpNumDisplaysCmd.h: Set number of displays to either one, 
// two, or three.
////////////////////////////////////////////////////////////
#ifndef TpNumDisplaysCmd_H
#define TpNumDisplaysCmd_H
#include "RadioCmd.h"
#include "TpDefs.h"

class TpDisplayer;

class TpNumDisplaysCmd : public RadioCmd {

  protected:

    TpDisplayer *_displayer;
    int _numDisplays;

    virtual void doit();
    
  public:

    TpNumDisplaysCmd(const char *, int, CmdValue, CmdList *, 
			TpDisplayer *, int);

    virtual const char *const className () { return "TpNumDisplaysCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQuitCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpQuitCmd.h: Exit the application if there is no tiepoint file to save.
// Otherwise execute TpCloseCmd and provide it with 'this' pointer so that
// TpCloseCmd object could call us again when it is done with closing project.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUITCMD_H
#define TPQUITCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpQuitCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;

    virtual void doit();
    
  public:

    TpQuitCmd(const char *name, int active, TpMatchManager *);

    virtual const char *const className () { return "TpQuitCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpRotateImageCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpRotateImageCmd.h: Rotate image.
////////////////////////////////////////////////////////////
#ifndef TpRotateImageCmd_H
#define TpRotateImageCmd_H
#include "RadioCmd.h"
#include "RotationDefs.h"

class TpSubDisplayer;

class TpRotateImageCmd : public RadioCmd {

  protected:

    TpSubDisplayer *_subDisplayer;
    RotationType _rotation;

    virtual void doit();
    
  public:

    TpRotateImageCmd(const char *, int, CmdList *, TpSubDisplayer *, RotationType);

    virtual const char *const className () { return "TpRotateImageCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSaveAndExitCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSaveAndExitCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEANDEXITCMD_H
#define TPSAVEANDEXITCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpSaveAndExitCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();

  public:

    TpSaveAndExitCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpSaveAndExitCmd() { };

    virtual const char *const className () { return "TpSaveAndExitCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSaveConfigAsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSaveConfigAsCmd.h: Saves configuration file as the name provided as a 
// command value.
//////////////////////////////////////////////////////////////////////////////
#ifndef TPSAVECONFIGASCMD_H
#define TPSAVECONFIGASCMD_H
#include "NoUndoCmd.h"

class TpWindow;

class TpSaveConfigAsCmd : public NoUndoCmd {

  private:

    TpWindow *_window;

    CmdValue _tmpValue;

    static void okCallback(void *clientData);

  protected:

    virtual void doit();

  public:

    TpSaveConfigAsCmd(const char *name, int active, TpWindow *);

    virtual void execute(CmdValue new_value = NULL);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpSaveConfigAsCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSaveConfigCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSaveConfigCmd.h: Save configuration file.
/////////////////////////////////////////////////////////////
#ifndef TPSAVECONFIGCMD_H
#define TPSAVECONFIGCMD_H
#include "NoUndoCmd.h"

class TpWindow;

class TpSaveConfigCmd : public NoUndoCmd {

  protected:

    TpWindow *_window;

    virtual void doit();

  public:

    TpSaveConfigCmd(const char *name, int active, TpWindow *);

    virtual const char *const className () { return "TpSaveConfigCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSavePointCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSavePointCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEPOINTCMD_H
#define TPSAVEPOINTCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSavePointCmd : public Cmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();
    virtual void undoit();

  public:

    TpSavePointCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpSavePointCmd() { };

    virtual const char *const className () { return "TpSavePointCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSavePointFileAsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSavePointFileAsCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEPOINTFILEASCMD_H
#define TPSAVEPOINTFILEASCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpSavePointFileAsCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;
    Cmd *_quitCmd;

    CmdValue _tmpValue;

    static void okCallback(void *clientData);

  protected:

    virtual void doit();

  public:

    TpSavePointFileAsCmd(const char *name, int active, 
			 TpMatchManager *matchManager, Cmd *quitCmd = NULL);
    virtual ~TpSavePointFileAsCmd() { };

    virtual void execute(CmdValue new_value = NULL);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpSavePointFileAsCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSavePointFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSavePointFileCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEPOINTFILECMD_H
#define TPSAVEPOINTFILECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpSavePointFileCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();

  public:

    TpSavePointFileCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpSavePointFileCmd() { };

    virtual const char *const className () { return "TpSavePointFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetAutofindCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpSetAutofindCmd.h: Set autofind mode
////////////////////////////////////////////////////////////
#ifndef TPSETAUTOFINDCMD_H
#define TPSETAUTOFINDCMD_H
#include "RadioCmd.h"
#include "TpDefs.h"

class TpMatchManager;

class TpSetAutofindCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    TpAutofindMode _autofindMode;

    virtual void doit();
    
  public:

    TpSetAutofindCmd(const char *, int, CmdValue, CmdList *, 
			TpMatchManager *, TpAutofindMode);

    virtual const char *const className () { return "TpSetAutofindCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointSizeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointSizeCmd.h: This command class allow user to set point size to 
// one of the predefined shapes.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTSIZECMD_H
#define TPSETPOINTSIZECMD_H
#include "RadioCmd.h"
#include "TpPoint.h"

class TpMatchManager;

class TpSetPointSizeCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    int _size;

    void doit();
    void undoit() { }

  public:

    TpSetPointSizeCmd(const char *name, int active, CmdList *, 
		      int size, TpMatchManager *);
    virtual ~TpSetPointSizeCmd() { }

    int getSize() { return _size; }

    virtual const char *const className () { return "TpSetPointSizeCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointSymbolCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointSymbolCmd.h: This command class allow user to set point symbol to 
// one of the predefined shapes.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTSYMBOLCMD_H
#define TPSETPOINTSYMBOLCMD_H
#include "RadioCmd.h"
#include "TpPoint.h"

class TpMatchManager;

class TpSetPointSymbolCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    TpPointSymbolShapeType _shape;

    void doit();
    void undoit() { }

  public:

    TpSetPointSymbolCmd(const char *name, int active, CmdList *, 
			TpPointSymbolShapeType, TpMatchManager *);
    virtual ~TpSetPointSymbolCmd() { }

    TpPointSymbolShapeType getShape() { return _shape; }

    virtual const char *const className () { return "TpSetPointSymbolCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpShiftCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpShiftCmd.h: Shift subdisplays either to the left or to
// the right depending on the value passed in the constructor.
////////////////////////////////////////////////////////////
#ifndef TPSHIFTCMD_H
#define TPSHIFTCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>	// for Boolean

class TpDisplayer;

class TpShiftCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;
    Boolean _leftShift;
    
    virtual void doit();
    virtual void undoit();
    
  public:

    TpShiftCmd(const char *, int, TpDisplayer *, Boolean leftShift);

    virtual const char *const className () { return "TpShiftCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSwapLockCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpSwapLockCmd.h: Set image swap lock option on or off.
////////////////////////////////////////////////////////////
#ifndef TpSwapLockCmd_H
#define TpSwapLockCmd_H
#include "Cmd.h"

class TpDisplayer;

class TpSwapLockCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;
    int _imageNo;

    virtual void doit();
    virtual void undoit();

  public:

    TpSwapLockCmd(const char *, int, TpDisplayer *, int i);

    virtual const char *const className () { return "TpSwapLockCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpWritePointsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpWritePointsCmd.h: Write collected points 
// to the ASCII file.
////////////////////////////////////////////////////////////
#ifndef TPWRITEPOINTSCMD_H
#define TPWRITEPOINTSCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpWritePointsCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;
    
    virtual void doit();
    
  public:

    TpWritePointsCmd(const char *, int, TpMatchManager *);

    virtual const char *const className () { return "TpWritePointsCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointColorCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorCmd.h: This command class allow user to set point color to 
// one of the X predefined colors.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTCOLORCMD_H
#define TPSETPOINTCOLORCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetPointColorCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetPointColorCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetPointColorCmd() { }

    virtual const char *const className () { return "TpSetPointColorCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointColorSelCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorSelCmd.h: This command class allow user to set point's 
// selected color to one of the X predefined colors.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTCOLORSELCMD_H
#define TPSETPOINTCOLORSELCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetPointColorSelCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetPointColorSelCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetPointColorSelCmd() { }

    virtual const char *const className () { return "TpSetPointColorSelCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpContrastValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
class TpContrastValue {

  protected:

    int _min, _max;

  public:

    TpContrastValue(int min, int max) { _min = min; _max = max; }
    TpContrastValue(TpContrastValue &value) 
	{ _min = value.getMin(); _max = value.getMax(); }

    int getMin() { return _min; }
    int getMax() { return _max; }

    void setMin(int min) { _min = min; }
    void setMax(int max) { _max = max; }

};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchModeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeCmd.h: This command class allow user to set the match mode.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHMODECMD_H
#define TPSETMATCHMODECMD_H
#include "RadioCmd.h"
#include "TpDefs.h"

class TpMatchManager;

class TpSetMatchModeCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    TpMatchMode _matchMode;

    void doit();
    void undoit() { }

  public:

    TpSetMatchModeCmd(const char *name, int active, CmdValue, CmdList *, 
		      TpMatchMode, TpMatchManager *);
    virtual ~TpSetMatchModeCmd() { }

    TpMatchMode getMatchMode() { return _matchMode; }

    virtual const char *const className () { return "TpSetMatchModeCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetCursorSymbolCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetCursorSymbolCmd.h: This command class allow user to set cursor symbol 
// to one of the predefined shapes.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETCURSORSYMBOLCMD_H
#define TPSETCURSORSYMBOLCMD_H
#include "RadioCmd.h"
#include <UIComponent.h>

class TpDisplayer;

class TpSetCursorSymbolCmd : public RadioCmd {

  protected:

    TpDisplayer *_displayer;
    String _cursor;

    void doit();
    void undoit() { }

  public:

    TpSetCursorSymbolCmd(const char *name, int active, CmdList *, TpDisplayer *);
    virtual ~TpSetCursorSymbolCmd() { }

    String getCursor() { return _cursor; }

    virtual const char *const className () { return "TpSetCursorSymbolCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetCursorColorCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetCursorColorCmd.h: This command class allow user to set cursor color to 
// one of the X predefined colors.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETCURSORCOLORCMD_H
#define TPSETCURSORCOLORCMD_H
#include "Cmd.h"

class TpDisplayer;

class TpSetCursorColorCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;

    void doit();
    void undoit() { }

  public:

    TpSetCursorColorCmd(const char *name, int active, CmdValue, TpDisplayer *);
    virtual ~TpSetCursorColorCmd() { }

    virtual const char *const className () { return "TpSetCursorColorCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchModeValuesCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmd.h: This command class allow user to set the match 
// mode values.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHMODEVALUESCMD_H
#define TPSETMATCHMODEVALUESCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetMatchModeValuesCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetMatchModeValuesCmd(const char *name, int active, CmdValue value, 
			    TpMatchManager *);
    virtual ~TpSetMatchModeValuesCmd() { }

    virtual const char *const className () {return "TpSetMatchModeValuesCmd";}
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPrintCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// TpPrintCmd.h
/////////////////////////////////////////////////////////
#ifndef TPPRINTCMD_H
#define TPPRINTCMD_H

#include "NoUndoCmd.h"
#include "SgPrintValue.h"

class TpDisplayer;

class TpPrintCmd : public NoUndoCmd {

  protected:

    TpDisplayer *_displayer;

    SgPrintValue *_printValue;
    
  public:

    TpPrintCmd(const char *, int, TpDisplayer *displayer,
	       SgPrintValue *printValue=NULL);
    
    virtual void doit(); 
    
    virtual void freeValue(CmdValue value) 
	{ if (value) delete (SgPrintValue *)value; }
    
    virtual const char *const className () { return "TpPrintCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpRedoMatchIdsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpRedoMatchIdsCmd.h: This command class allow user to redo all the general 
// qualifier ids.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPREDOMATCHIDSCMD_H
#define TPREDOMATCHIDSCMD_H
#include "WarnNoUndoCmd.h"

class TpMatchManager;

class TpRedoMatchIdsCmd : public WarnNoUndoCmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();

  public:

    TpRedoMatchIdsCmd(const char *name, int active, TpMatchManager *);
    virtual ~TpRedoMatchIdsCmd() { }

    virtual const char *const className () { return "TpRedoMatchIdsCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutoSyncPointsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpAutoSyncPointsCmd.h: This command class allow user to set status of 
// points collection to "redo" mode that will redo matching as soon as 
// the location of any of the points change.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOSYNCPOINTSCMD_H
#define TPAUTOSYNCPOINTSCMD_H
#include "TpMatchManager.h"
#include "Cmd.h"

class TpMatchManager;

class TpAutoSyncPointsCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit()
	{
	    if (_value)
		_matchManager->setAutoSyncPoints(True);
	    else
		_matchManager->setAutoSyncPoints(False);
	}

    void undoit()
	{
	    if (_value)
		_matchManager->setAutoSyncPoints(False);
	    else
		_matchManager->setAutoSyncPoints(True);
	    
	    _value = (CmdValue)(!_value);
	    newValue();
	}


  public:

    TpAutoSyncPointsCmd(const char *name, int active, TpMatchManager *mm)
	: Cmd(name, active)
	{ _matchManager = mm; }
    virtual ~TpAutoSyncPointsCmd() { }

    virtual const char *const className () { return "TpAutoSyncPointsCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpCheckGenQualUniqueCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpCheckGenQualUniqueCmd.h: This command class allow user to specify whether 
// the program should check for general qualifier uniqueness.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPCHECKGENQUALUNIQUECMD_H
#define TPCHECKGENQUALUNIQUECMD_H
#include "TpMatchManager.h"
#include "Cmd.h"

class TpMatchManager;

class TpCheckGenQualUniqueCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit()
	{
            if (_value)
                _matchManager->setCheckingGenQualUnique(True);
            else
                _matchManager->setCheckingGenQualUnique(False);
        }

    void undoit()
	{
            if (_value)
                _matchManager->setCheckingGenQualUnique(False);
            else
                _matchManager->setCheckingGenQualUnique(True);
 
            _value = (CmdValue)(!_value);
            newValue();
        }

  public:

    TpCheckGenQualUniqueCmd(const char *name, int active, TpMatchManager *mm) 
	: Cmd(name, active)
	{ _matchManager = mm; }
    virtual ~TpCheckGenQualUniqueCmd() { }

    virtual const char *const className () { return "TpCheckGenQualUniqueCmd";}
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetTagPositionCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetTagPositionCmd.h: This command class allow user to set tag position 
// to one of the predefined types (NE, NW, SE, SW, CTR).
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETTAGPOSITIONCMD_H
#define TPSETTAGPOSITIONCMD_H
#include "RadioCmd.h"
#include "TpPoint.h"
#include "TpMatchManager.h"

class TpSetTagPositionCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    const TpTagPosition _tagPosition;

    void doit()
	{ 
	    if (_value) {
		_matchManager->setTagPosition(_tagPosition);
	    }
	}

  public:

    TpSetTagPositionCmd(const char *name, int active, CmdValue value, 
			CmdList *radCmdList, TpTagPosition position, 
			TpMatchManager *mm)
	: RadioCmd(name, active, value, radCmdList), _tagPosition(position)
	{
	    _matchManager = mm;
	}

    virtual ~TpSetTagPositionCmd() { }

    TpTagPosition getTagPosition() const { return _tagPosition; }

    virtual const char *const className () { return "TpSetTagPositionCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpColorCodeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpColorCodeCmd.h: This command class allow user to colorcode point color 
// based on the qualifier value
///////////////////////////////////////////////////////////////////////////////
#ifndef TPCOLORCODECMD_H
#define TPCOLORCODECMD_H
#include "Cmd.h"
#include "TpPoint.h"

class TpMatchManager;

class TpColorCodeCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpColorCodeCmd(const char *name, int active, TpMatchManager *);
    virtual ~TpColorCodeCmd() { }

    virtual const char *const className () { return "TpColorCodeCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchIdOffsetCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdOffsetCmd:  
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHIDOFFSETCMD_H
#define TPSETMATCHIDOFFSETCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetMatchIdOffsetCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetMatchIdOffsetCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetMatchIdOffsetCmd() { }

    virtual const char *const className () { return "TpSetMatchIdOffsetCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchIdNextCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdNextCmd:  
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHIDNEXTCMD_H
#define TPSETMATCHIDNEXTCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetMatchIdNextCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetMatchIdNextCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetMatchIdNextCmd() { }

    virtual const char *const className () { return "TpSetMatchIdNextCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpShowPointLabelsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpShowPointLabelsCmd.h: This command class allow user to specify whether 
// the program should display point labels.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSHOWPOINTLABELSCMD_H
#define TPSHOWPOINTLABELSCMD_H
#include "TpMatchManager.h"
#include "Cmd.h"

class TpMatchManager;

class TpShowPointLabelsCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit()
	{
            if (_value)
                _matchManager->showPointLabels(True);
            else
                _matchManager->showPointLabels(False);
        }

    void undoit()
	{
            if (_value)
                _matchManager->showPointLabels(False);
            else
                _matchManager->showPointLabels(True);
 
            _value = (CmdValue)(!_value);
            newValue();
        }

  public:

    TpShowPointLabelsCmd(const char *name, int active, TpMatchManager *mm) 
	: Cmd(name, active, (CmdValue)True)
	{ _matchManager = mm; }
    virtual ~TpShowPointLabelsCmd() { }

    virtual const char *const className () { return "TpShowPointLabelsCmd";}
};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
