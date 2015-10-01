$!****************************************************************************
$!
$! Build proc for MIPL module histcommands_h
$! VPACK Version 1.8, Thursday, June 13, 1996, 14:33:18
$!
$! Execute by entering:		$ @histcommands_h
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
$ write sys$output "*** module histcommands_h ***"
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
$ write sys$output "Invalid argument given to histcommands_h.com file -- ", primary
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
$   if F$SEARCH("histcommands_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @histcommands_h.bld "STD"
$   else
$      @histcommands_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histcommands_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histcommands_h.com -mixed -
	-s AxisCmd.h HistCmd.h SetAscAxisCmd.h SetDescAxisCmd.h -
	   SetHorHistGraphCmd.h SetPopUpDirColCmd.h SetPopUpDirRowCmd.h -
	   SetStackBlendCmd.h SetStackNoBlendCmd.h SetVertHistGraphCmd.h -
	   StatsCmd.h HistLogScaleCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create AxisCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// AxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#ifndef AXISCMD_H
#define AXISCMD_H
#include "Cmd.h"

class HistBox;

class AxisCmd : public Cmd {

  private:

    int _oldValue;
    HistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    AxisCmd ( const char *, int, HistBox * );

    virtual const char *const className () { return "AxisCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// HistCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef HISTCMD_H
#define HISTCMD_H
#include "Cmd.h"

class HistBox;

class HistCmd : public Cmd {

  private:

    int _oldValue;
    HistBox *_menuView;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    HistCmd ( char *, int, HistBox * );
    virtual const char *const className () { return "HistCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetAscAxisCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetAscAxisCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETASCAXISCMD_H
#define SETASCAXISCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetAscAxisCmd : public RadioCmd {
  private:
    VerAxisDirType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   

  public:
    
    SetAscAxisCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetAscAxisCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetDescAxisCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetDescAxisCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETDESCAXISCMD_H
#define SETDESCAXISCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetDescAxisCmd : public RadioCmd {
  private:
    VerAxisDirType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   

  public:
    
    SetDescAxisCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetDescAxisCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetHorHistGraphCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetHorHistGraphCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETHORHISTGRAPHCMD_H
#define SETHORHISTGRAPHCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetHorHistGraphCmd : public RadioCmd {
  private:
    OrientType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   
//    virtual void undoit(); 

  public:
    
    SetHorHistGraphCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetHorHistGraphCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetPopUpDirColCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetPopUpDirColCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETPOPUPDIRCOLCMD_H
#define SETPOPUPDIRCOLCMD_H
#include "RadioCmd.h"
#include "HistDefs.h"
class HistBox;

class SetPopUpDirColCmd : public RadioCmd {

  private:
    PopupDirectionType _oldPopupDirectionValue;
    MethodType  _oldMethodValue;
    HistBox *_menuView;

  protected:
    virtual void doit();   

  public:
    SetPopUpDirColCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetPopUpDirColCmd"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetPopUpDirRowCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetPopUpDirRowCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETPOPUPDIRROWCMD_H
#define SETPOPUPDIRROWCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetPopUpDirRowCmd : public RadioCmd {

  private:
    PopupDirectionType _oldPopupDirectionValue;
    MethodType _oldMethodValue;
    HistBox *_menuView;

  protected:
    virtual void doit();   

  public:
    SetPopUpDirRowCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetPopUpDirRowCmd"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetStackBlendCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetStackBlendCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETSTACKBLENDCMD_H
#define SETSTACKBLENDCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetStackBlendCmd : public RadioCmd {

  private:
    HistBox *_menuView;

  protected:
    virtual void doit();   

  public:
    SetStackBlendCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetStackBlendCmd"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetStackNoBlendCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetStackNoBlendCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETSTACKNOBLENDCMD_H
#define SETSTACKNOBLENDCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetStackNoBlendCmd : public RadioCmd {

  private:
    HistBox *_menuView;

  protected:    
    virtual void doit();   

  public:    
    SetStackNoBlendCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetStackNoBlendCmd"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetVertHistGraphCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetVertHistGraphCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETVERTHISTGRAPHCMD_H
#define SETVERTHISTGRAPHCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetVertHistGraphCmd : public RadioCmd {
  private:
    OrientType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   
//    virtual void undoit(); 

  public:
    
    SetVertHistGraphCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetVertHistGraphCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StatsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// StatsCmd.h : This include file defines the command for
//              turning the Histogram graph on and off
/////////////////////////////////////////////////////////////
#ifndef STATSCMD_H
#define STATSCMD_H
#include "Cmd.h"

class HistBox;

class StatsCmd : public Cmd {

  private:

    int _oldValue;        // Last valid command for Undo
    HistBox *_menuView;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    StatsCmd ( const char *, int, HistBox * );
    virtual const char *const className () { return "StatsCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistLogScaleCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// HistLogScaleCmd.h: Displays histogram using logarithmic scale.
/////////////////////////////////////////////////////////////
#ifndef HISTLOGSCALECMD_H
#define HISTLOGSCALECMD_H
#include "Cmd.h"

class HistBox;

class HistLogScaleCmd : public Cmd {

  private:

    int _oldValue;
    HistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:

    HistLogScaleCmd ( const char *, int, HistBox * );

    virtual const char *const className () { return "HistLogScaleCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
