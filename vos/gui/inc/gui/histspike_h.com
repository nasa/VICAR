$!****************************************************************************
$!
$! Build proc for MIPL module histspike_h
$! VPACK Version 1.8, Tuesday, December 13, 1994, 10:20:24
$!
$! Execute by entering:		$ @histspike_h
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
$ write sys$output "*** module histspike_h ***"
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
$ write sys$output "Invalid argument given to histspike_h.com file -- ", primary
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
$   if F$SEARCH("histspike_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @histspike_h.bld "STD"
$   else
$      @histspike_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histspike_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histspike_h.com -mixed -
	-s SpikeDialog.h SpikeDialogCmd.h KeyInSpikeInterface.h -
	   SpikeButtonInterface.h SpikeCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SpikeDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
//SpikeDialog.h: Include file to create the Spike Dialog 
//              Box
//////////////////////////////////////////////////////////////
#ifndef SPIKEDIALOG_H
#define SPIKEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class HistBox;
class Cmd;

class SpikeDialog : public CustomDialog {

  protected:

   Cmd *_SpikeCmd;
   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w);}
   
   HistBox *_histBox;

  public:

    SpikeDialog ( HistBox *, const char * );
    virtual Widget createWorkArea(Widget);

};

#endif
















$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SpikeDialogCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// SpikeDialogCmd.h: Include file to define class for displaying
//                  Spike Dialog Box for XVICDISP widget
///////////////////////////////////////////////////////////////
#ifndef SPIKEDIALOGCMD_H
#define SPIKEDIALOGCMD_H
#include "Cmd.h"
#include "CustomDialog.h"
#include "NoUndoCmd.h"

class BasicImageView;

class SpikeDialogCmd : public NoUndoCmd {
  private:
    CustomDialog *_dialog;
  protected:
    
    virtual void doit() { _dialog->post(); };   

  public:
    
    SpikeDialogCmd ( const char *name , int active, CustomDialog *dialog ) : NoUndoCmd( name, active) { _dialog = dialog; };
    virtual const char *const className () { return "SpikeDialogCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create KeyInSpikeInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// KeyInSpikeInterface.h: An Keyin interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef KEYINSPIKEINTERFACE
#define KEYINSPIKEINTERFACE
#include "CmdInterface.h"
#include <Xm/Xm.h>

class KeyinView;

class KeyInSpikeInterface : public CmdInterface {
  protected:

    KeyinView *_spike;
    CmdValue  *_init;
    
    virtual KeyinView *addOneSubView(Widget parent, const char *name);
    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:
    
    KeyInSpikeInterface ( Widget, Cmd * );
    ~KeyInSpikeInterface();
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SpikeButtonInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SpikeButtonInterface.h: An SpikeButton interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SPIKEBUTTONINTERFACE
#define SPIKEBUTTONINTERFACE
#include "ArrowButtonInterface.h"
//#include "Histogram.h"

class Histogram;

class SpikeButtonInterface : public ArrowButtonInterface {
  protected:    

    int _step;
    Histogram *_histogram;
    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:
    
    SpikeButtonInterface ( Widget, int, Histogram *, Cmd * );
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SpikeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SpikeCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SPIKECMD_H
#define SPIKECMD_H
#include "Cmd.h"

class HistBox;

class SpikeCmd : public Cmd {

  protected:

    virtual void doit();   
    virtual void undoit(); 
    HistBox *_histBox;
    int _prevValue;

  public:

    SpikeCmd ( const char *, int, HistBox * );
    virtual const char *const className () { return "SpikeCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
